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

static void do_cmd_eat_food_aux(object_type *o_ptr)
{
	bool ident;

	/* Sound */
	sound(SOUND_EAT);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Identity not known yet */
	ident = FALSE;

	/* Eat the food */
	switch (o_ptr->sval)
	{
		case SV_FOOD_RATION:
		case SV_FOOD_BISCUIT:
		case SV_FOOD_JERKY:
		case SV_FOOD_SLIME_MOLD:
		case SV_FOOD_PINT_OF_ALE:
		case SV_FOOD_PINT_OF_WINE:
			msgf("That tastes good.");
			ident = TRUE;
			break;
		case SV_FOOD_WAYBREAD:
			msgf("That tastes good.");
			set_poisoned(0);
			hp_player(damroll(4, 8));
			ident = TRUE;
			break;
		case SV_FOOD_RESTORING:
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;
		case SV_FOOD_RESTORE_CON:
			if (do_res_stat(A_CON)) ident = TRUE;
			break;
		case SV_FOOD_RESTORE_STR:
			if (do_res_stat(A_STR)) ident = TRUE;
			break;
		case SV_FOOD_CURE_SERIOUS:
			if (hp_player(75)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
			break;
		case SV_FOOD_CURE_CONFUSION:
			if (set_confused(0)) ident = TRUE;
			break;
		case SV_FOOD_CURE_PARANOIA:
			if (set_afraid(0)) ident = TRUE;
			break;
		case SV_FOOD_CURE_BLINDNESS:
			if (set_blind(0)) ident = TRUE;
			break;
		case SV_FOOD_CURE_POISON:
			if (set_poisoned(0)) ident = TRUE;
			break;
		case SV_FOOD_DISEASE:
			take_hit(damroll(10, 10), "poisonous food");
			do_dec_stat(A_STR);
			ident = TRUE;
			break;
		case SV_FOOD_UNHEALTH:
			take_hit(damroll(10, 10), "poisonous food");
			do_dec_stat(A_CON);
			ident = TRUE;
			break;
		case SV_FOOD_NAIVETY:
			take_hit(damroll(8, 8), "poisonous food");
			do_dec_stat(A_WIS);
			ident = TRUE;
			break;
		case SV_FOOD_STUPIDITY:
			take_hit(damroll(8, 8), "poisonous food");
			do_dec_stat(A_INT);
			ident = TRUE;
			break;
		case SV_FOOD_SICKNESS:
			take_hit(damroll(6, 6), "poisonous food");
			do_dec_stat(A_CON);
			ident = TRUE;
			break;
		case SV_FOOD_WEAKNESS:
			take_hit(damroll(6, 6), "poisonous food");
			do_dec_stat(A_STR);
			ident = TRUE;
			break;
		case SV_FOOD_PARALYSIS:
			if (!p_ptr->free_act)
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_int(10) + 10))
					ident = TRUE;
			}
			break;
		case SV_FOOD_HALLUCINATION:
			if (!p_ptr->resist_chaos)
			{
				if (set_image(p_ptr->image + rand_int(250) + 250))
					ident = TRUE;
			}
			break;
		case SV_FOOD_CONFUSION:
			if (!p_ptr->resist_confu)
			{
				if (set_confused(p_ptr->confused + rand_int(10) + 10))
					ident = TRUE;
			}
			break;
		case SV_FOOD_PARANOIA:
			if (!p_ptr->resist_fear)
			{
				if (set_afraid(p_ptr->afraid + rand_int(10) + 10))
					ident = TRUE;
			}
			break;
		case SV_FOOD_POISON:
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois || p_ptr->immune_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_int(10) + 10))
					ident = TRUE;
			}
			break;
		case SV_FOOD_BLINDNESS:
			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + rand_int(200) + 200))
					ident = TRUE;
			}
			break;
		}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Food can feed the player */
	if (p_ptr->prace == RACE_VAMPIRE)
	{
		/* Reduced nutritional benefit */
		(void)set_food(p_ptr->food + (o_ptr->pval / 10));
		msgf
			("Mere victuals hold scant sustenance for a being such as yourself.");
		if (p_ptr->food < PY_FOOD_ALERT)	/* Hungry */
			msgf("Your hunger can only be satisfied with fresh blood!");
	}
	else if (p_ptr->prace == RACE_SKELETON)
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
	else if ((p_ptr->prace == RACE_GOLEM) ||
			 (p_ptr->prace == RACE_ZOMBIE) ||
			 (p_ptr->prace == RACE_SPECTRE) || (p_ptr->prace == RACE_GHOUL))
	{
		msgf("The food of mortals is poor sustenance for you.");
		(void)set_food(p_ptr->food + ((o_ptr->pval) / 20));
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
	int ee;

	/* Sound */
	sound(SOUND_QUAFF);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Quaff the potion */
	switch (o_ptr->sval)
	{
	case SV_POTION_WATER:
	case SV_POTION_APPLE_JUICE:
	case SV_POTION_SLIME_MOLD:
		msgf("You feel less thirsty.");
		ident = TRUE;
		break;
	case SV_POTION_SLOWNESS:
		if (set_slow(p_ptr->slow + rand_range(15, 40))) ident = TRUE;
		break;
	case SV_POTION_SALT_WATER:
		msgf("The potion makes you vomit!");
		if (p_ptr->food > PY_FOOD_STARVE - 1) set_food(PY_FOOD_STARVE - 1);
		set_poisoned(0);
		set_paralyzed(p_ptr->paralyzed + 4);
		ident = TRUE;
		break;
	case SV_POTION_POISON:
		if (!(p_ptr->resist_pois || p_ptr->oppose_pois || p_ptr->immune_pois))
			if (set_poisoned(p_ptr->poisoned + rand_range(10, 25)))
				ident = TRUE;
		break;
	case SV_POTION_BLINDNESS:
		if (!p_ptr->resist_blind)
			if (set_blind(p_ptr->blind + rand_range(100, 200)))
				ident = TRUE;
		break;
	// Booze
	case SV_POTION_CONFUSION:
		if (! p_ptr->resist_confu)
			if (set_confused(p_ptr->confused + rand_range(15, 35)))
				ident = TRUE;
		if (! p_ptr->resist_chaos )
			if (one_in_(2) )
				if (set_image(p_ptr->image + rand_range(150, 300)) )
					ident = TRUE;
		if (one_in_(13))
		{
			ident = TRUE;
			
			if (one_in_(3)) lose_all_info();
		
			teleport_player(100);
			wiz_dark();
			msgf("You wake up somewhere with a sore head...");
			msgf("You can't remember a thing,: how you got here!");
		}
		break;

	case SV_POTION_SLEEP:
		if (! p_ptr->free_act)
		{
			msgf("You fall asleep.");

			if (ironman_nightmare)
			{
				msgf("A horrible vision enters your mind.");

				// Pick a nightmare
				get_mon_num_prep(get_nightmare, NULL);

				// Have some nightmares
				have_nightmare(get_mon_num(MAX_DEPTH));

				// Remove the monster restriction
				get_mon_num_prep(NULL, NULL);
			}

			if (set_paralyzed(p_ptr->paralyzed + rand_range(4, 8)))
				ident = TRUE;
		}
		break;
	case SV_POTION_LOSE_MEMORIES:
		if (!(p_ptr->hold_life) && (p_ptr->exp > 0))
		{
			msgf("You feel your memories fade.");
			lose_exp(p_ptr->exp / 4);
			ident = TRUE;
		}
		break;
	case SV_POTION_RUINATION:
		msgf("Your nerves and muscles feel weak and lifeless!");
		take_hit(damroll(10, 10), "a potion of Ruination");
		dec_stat(A_DEX, 25, TRUE);
		dec_stat(A_WIS, 25, TRUE);
		dec_stat(A_CON, 25, TRUE);
		dec_stat(A_STR, 25, TRUE);
		dec_stat(A_CHR, 25, TRUE);
		dec_stat(A_INT, 25, TRUE);
		ident = TRUE;
		break;
	case SV_POTION_DEC_STR:
		if (do_dec_stat(A_STR)) ident = TRUE;
		break;
	case SV_POTION_DEC_INT:
		if (do_dec_stat(A_INT)) ident = TRUE;
		break;		
	case SV_POTION_DEC_WIS:
		if (do_dec_stat(A_WIS)) ident = TRUE;
		break;		
	case SV_POTION_DEC_DEX:
		if (do_dec_stat(A_DEX)) ident = TRUE;
		break;		
	case SV_POTION_DEC_CON:
		if (do_dec_stat(A_CON)) ident = TRUE;
		break;		
	case SV_POTION_DEC_CHR:
		if (do_dec_stat(A_CHR)) ident = TRUE;
		break;		
	case SV_POTION_DETONATIONS:
		msgf("Massive explosions rupture your body!");
		take_hit(damroll(50, 20), "a potion of Detonation");
		set_stun(p_ptr->stun + 75);
		set_cut(p_ptr->cut + 5000);
		ident = TRUE;
		break;		
	case SV_POTION_DEATH:
		msgf("A feeling of Death flows through your body.");
		take_hit(5000, "a potion of Death");
		ident = TRUE;
		break;		
	case SV_POTION_INFRAVISION:
		if (set_tim_infra(p_ptr->tim_infra + rand_range(100, 200)))
			ident = TRUE;
		break;
	case SV_POTION_DETECT_INVIS:
		if (set_tim_invis(p_ptr->tim_invis + rand_range(12, 24)))
			ident = TRUE;
		break;
	case SV_POTION_SLOW_POISON:
		if (set_poisoned(p_ptr->poisoned / 2)) ident = TRUE;
		break;		
	case SV_POTION_CURE_POISON:
		if (set_poisoned(0)) ident = TRUE;
		break;		
	case SV_POTION_BOLDNESS:
		if (set_afraid(0)) ident = TRUE;
		break;		
	case SV_POTION_SPEED:
		if (p_ptr->fast == 0) 
		{
			if (set_fast(rand_range(15, 40))) ident = TRUE;
		}
		else set_fast(p_ptr->fast + 5);
		break;		
	case SV_POTION_RESIST_HEAT:
		if (set_oppose_fire(p_ptr->oppose_fire + rand_range(10, 20)))
			ident = TRUE;
		break;
		
	case SV_POTION_RESIST_COLD:
		if (set_oppose_cold(p_ptr->oppose_cold + rand_range(10, 20)))
			ident = TRUE;
		break;
	case SV_POTION_HEROISM:
		if (set_afraid(0)) ident = TRUE;
		if (set_hero(p_ptr->hero + rand_range(25, 50))) ident = TRUE;
		if (hp_player(10)) ident = TRUE;
		break;
	case SV_POTION_BERSERK_STRENGTH:
		if (set_afraid(0)) ident = TRUE;
		if (set_shero(p_ptr->shero + rand_range(25, 50))) ident = TRUE;
		if (hp_player(30)) ident = TRUE;
		break;
	case SV_POTION_CURE_LIGHT:
		if (hp_player(40)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_cut(p_ptr->cut - 10)) ident = TRUE;
		break;
	case SV_POTION_CURE_SERIOUS:
		if (hp_player(75)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
		break;
	case SV_POTION_CURE_CRITICAL:
		if (hp_player(150)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		break;
	case SV_POTION_HEALING:
		if (hp_player(300)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		break;
	case SV_POTION_STAR_HEALING:
		if (hp_player(1200)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		break;
	case SV_POTION_LIFE:
		msgf("You feel life flow through your body!");
		restore_level();
		set_poisoned(0);
		set_blind(0);
		set_confused(0);
		set_image(0);
		set_stun(0);
		set_cut(0);
		do_res_stat(A_STR);
		do_res_stat(A_CON);
		do_res_stat(A_DEX);
		do_res_stat(A_WIS);
		do_res_stat(A_INT);
		do_res_stat(A_CHR);
		break;

		// Recalculate max. hitpoints
		update_stuff();

		hp_player(5000);
		ident = TRUE;
		break;
	case SV_POTION_RESTORE_MANA:
		if (p_ptr->csp < p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
			p_ptr->csp_frac = 0;
			msgf("Your feel your head clear.");
			p_ptr->redraw |= PR_MANA;
			p_ptr->window |= PW_PLAYER;
			p_ptr->window |= PW_SPELL;
			ident = TRUE;
		}
		break;
	case SV_POTION_RESTORE_EXP:
		if (restore_level()) ident = TRUE;
		break;		
	case SV_POTION_RES_STR:
		if (do_res_stat(A_STR)) ident = TRUE;
		break;
	case SV_POTION_RES_INT:
		if (do_res_stat(A_INT)) ident = TRUE;
		break;
	case SV_POTION_RES_WIS:
		if (do_res_stat(A_WIS)) ident = TRUE;
		break;
	case SV_POTION_RES_DEX:
		if (do_res_stat(A_DEX)) ident = TRUE;
		break;
	case SV_POTION_RES_CON:
		if (do_res_stat(A_CON)) ident = TRUE;
		break;
	case SV_POTION_RES_CHR:
		if (do_res_stat(A_CHR)) ident = TRUE;
		break;
	case SV_POTION_INC_STR:
		if (do_inc_stat(A_STR)) ident = TRUE;
		break;
	case SV_POTION_INC_INT:
		if (do_inc_stat(A_INT)) ident = TRUE;
		break;
	case SV_POTION_INC_WIS:
		if (do_inc_stat(A_WIS)) ident = TRUE;
		break;
	case SV_POTION_INC_DEX:
		if (do_inc_stat(A_DEX)) ident = TRUE;
		break;
	case SV_POTION_INC_CON:
		if (do_inc_stat(A_CON)) ident = TRUE;
		break;
	case SV_POTION_INC_CHR:
		if (do_inc_stat(A_CHR)) ident = TRUE;
		break;
	case SV_POTION_AUGMENTATION:
		if (do_inc_stat(A_STR)) ident = TRUE;
		if (do_inc_stat(A_INT)) ident = TRUE;
		if (do_inc_stat(A_WIS)) ident = TRUE;
		if (do_inc_stat(A_DEX)) ident = TRUE;
		if (do_inc_stat(A_CON)) ident = TRUE;
		if (do_inc_stat(A_CHR)) ident = TRUE;
		break;
	case SV_POTION_ENLIGHTENMENT:
		msgf("An image of your surroundings forms in your mind...");
		wiz_lite();
		ident = TRUE;
		break;
	case SV_POTION_STAR_ENLIGHTENMENT:
		msgf("You begin to feel more enlightened...");
		message_flush();
		wiz_lite();
		do_inc_stat(A_INT);
		do_inc_stat(A_WIS);
		detect_traps();
		detect_doors();
		detect_stairs();
		detect_treasure();
		detect_objects_gold();
		detect_objects_normal();
		identify_pack();
		self_knowledge();
		ident = TRUE;
		break;
	case SV_POTION_SELF_KNOWLEDGE:
		msgf("You begin to know yourself a little better...");
		message_flush();
		self_knowledge();
		ident = TRUE;
		break;
	case SV_POTION_EXPERIENCE:
		if (p_ptr->exp < PY_MAX_EXP)
		{
			ee = (p_ptr->exp / 2) + 10;
			if (ee > 100000) ee = 100000;
			msgf("You feel more experienced.");
			gain_exp(ee);
			ident = TRUE;
		}
		break;
	case SV_POTION_RESISTANCE:
		set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
		set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
		set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
		set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
		set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
		ident = TRUE;
		break;
	case SV_POTION_CURING:
		if (hp_player(150)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		if (set_image(0)) ident = TRUE;
		break;
	case SV_POTION_INVULNERABILITY:
		set_invuln(p_ptr->invuln + rand_range(7, 14));
		ident = TRUE;
		break;
	case SV_POTION_NEW_LIFE:
		do_cmd_rerate();
		if ((p_ptr->muta1 != 0) || (p_ptr->muta2 != 0) || (p_ptr->muta3 != 0))
		{
			msgf("You are cured of all mutations.");
			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;
			p_ptr->update |= p_ptr->update;
			handle_stuff();
		}
		ident = TRUE;
		break;
	}

	if (p_ptr->prace == RACE_SKELETON)
	{
		msgf("Some of the fluid falls through your jaws!");
		(void)potion_smash_effect(0, p_ptr->px, p_ptr->py, o_ptr->k_idx);
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Potions can feed the player */
	switch (p_ptr->prace)
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
	bool ident, used_up;
	int k;
	int r_max;

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	// assume the scroll is used up

	used_up = TRUE;
	/* Read the scroll */
	switch (o_ptr->sval)
	{
	case SV_SCROLL_DARKNESS:
		if (!p_ptr->resist_blind && !p_ptr->resist_dark)
			set_blind(p_ptr->blind + rand_range(3, 8));
		if (unlite_area(10, 3)) ident = TRUE;;
		break;
	case SV_SCROLL_AGGRAVATE_MONSTER:
		msgf("There is a high pitched humming noise.");
		aggravate_monsters(0);
		ident = TRUE;
		break;
	case SV_SCROLL_CURSE_ARMOR:
		if (curse_armor()) ident = TRUE;
		break;
	case SV_SCROLL_CURSE_WEAPON:
		if (curse_weapon()) ident = TRUE;
		break;
	case SV_SCROLL_SUMMON_MONSTER:
		r_max = randint1(3);
		for (k=0; k <r_max; k++)
			if (summon_specific(0, p_ptr->px, p_ptr->py, p_ptr->depth, 0, TRUE, FALSE, FALSE))
				ident = TRUE;
		break;
	case SV_SCROLL_SUMMON_UNDEAD:
		r_max = randint1(3);
		for (k=0; k<r_max; k++)
			if (summon_specific(0, p_ptr->px, p_ptr->py, p_ptr->depth, SUMMON_UNDEAD, TRUE, FALSE, FALSE))
				ident = TRUE;
		break;
	case SV_SCROLL_TRAP_CREATION:
		if (trap_creation()) ident = TRUE;
		break;
	case SV_SCROLL_PHASE_DOOR:
		teleport_player(10);
		ident = TRUE;
		break;
	case SV_SCROLL_TELEPORT:
		teleport_player(100);
		ident = TRUE;
		break;
	case SV_SCROLL_TELEPORT_LEVEL:
		teleport_player_level();
		ident = TRUE;
		break;
	case SV_SCROLL_WORD_OF_RECALL:
		word_of_recall();
		ident = TRUE;
		break;
	case SV_SCROLL_IDENTIFY:
		ident = TRUE;
		if (! ident_spell()) used_up = FALSE;
		break;
	case SV_SCROLL_STAR_IDENTIFY:
		ident = TRUE;
		if (! identify_fully()) used_up = FALSE;
		break;
	case SV_SCROLL_REMOVE_CURSE:
		if (remove_curse())
		{
			msgf("You feel as if someone is watching over you.");
			ident = TRUE;
		}
		break;			
	case SV_SCROLL_STAR_REMOVE_CURSE:
		remove_all_curse();
		ident = TRUE;
		break;
	case SV_SCROLL_ENCHANT_ARMOR:
		ident = TRUE;
		if (! enchant_spell(0, 0, 1)) used_up = FALSE;
		break;		
	case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		if (! enchant_spell(1, 0, 0)) used_up = FALSE;
		ident = TRUE;
		break;
	case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		if (! enchant_spell(0, 1, 0)) used_up = FALSE;
		ident = TRUE;
		break;
	case SV_SCROLL_STAR_ENCHANT_ARMOR:
		if (! enchant_spell(0, 0, rand_range(2, 7))) used_up = FALSE;
		ident = TRUE;
		break;
	case SV_SCROLL_STAR_ENCHANT_WEAPON:
		if (! enchant_spell(randint1(5), randint1(5), 0)) used_up = FALSE;
		ident = TRUE;
		break;
	case SV_SCROLL_RECHARGING:
		if (! recharge(130)) used_up = FALSE;
		ident = TRUE;
		break;
	case SV_SCROLL_MUNDANITY:
		ident = TRUE;
		if (! mundane_spell()) used_up = FALSE;
		break;
	case SV_SCROLL_LIGHT:
		if (lite_area(damroll(2, 8), 2)) ident = TRUE;
		break;
	case SV_SCROLL_MAPPING:
		map_area();
		ident = TRUE;
		break;
	case SV_SCROLL_DETECT_GOLD:
		if (detect_treasure()) ident = TRUE;
		if (detect_objects_gold()) ident = TRUE;
		break;
	case SV_SCROLL_DETECT_ITEM:
		if (detect_objects_normal()) ident = TRUE;
		break;
	case SV_SCROLL_DETECT_TRAP:
		if (detect_traps()) ident = TRUE;
		break;
	case SV_SCROLL_DETECT_DOOR:
		if (detect_doors()) ident = TRUE;
		if (detect_stairs()) ident = TRUE;
		break;
	case SV_SCROLL_DETECT_INVIS:
		if (detect_monsters_invis()) ident = TRUE;
		break;
	case SV_SCROLL_SATISFY_HUNGER:
		if (set_food(PY_FOOD_MAX - 1)) ident = TRUE;
		break;
	case SV_SCROLL_BLESSING:
		if (set_blessed(p_ptr->blessed + rand_range(6, 18))) ident = TRUE;
		break;
	case SV_SCROLL_HOLY_CHANT:
		if (set_blessed(p_ptr->blessed + rand_range(12, 36))) ident = TRUE;
		break;
	case SV_SCROLL_HOLY_PRAYER:
		if (set_blessed(p_ptr->blessed + rand_range(24, 72))) ident = TRUE;
		break;
	case SV_SCROLL_MONSTER_CONFUSION:
		if (p_ptr->confusing == 0)
		{
			msgf("Your hands begin to glow.");
			p_ptr->confusing = TRUE;
			ident = TRUE;
			p_ptr->redraw |= PR_STATUS;
		}
		break;
	case SV_SCROLL_PROTECTION_FROM_EVIL:
		k = 3 * p_ptr->lev;
		if (set_protevil(p_ptr->protevil + randint1(25) + k)) ident = TRUE;
		break;
	case SV_SCROLL_RUNE_OF_PROTECTION:
		if (!warding_glyph()) used_up = FALSE;
		ident = TRUE;
		break;
	case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		if (destroy_doors_touch()) ident = TRUE;
		break;
	case SV_SCROLL_STAR_DESTRUCTION:
		if (destroy_area(p_ptr->px, p_ptr->py, 15))	ident = TRUE;
		else msgf("The dungeon trembles...");
		break;
	case SV_SCROLL_DISPEL_UNDEAD:
		if (dispel_undead(60)) ident = TRUE;
		break;
	case SV_SCROLL_GENOCIDE:
		genocide(TRUE);
		ident = TRUE;
		break;
	case SV_SCROLL_MASS_GENOCIDE:
		mass_genocide(TRUE);
		ident = TRUE;
		break;
	case SV_SCROLL_ACQUIREMENT:
		acquirement(p_ptr->px, p_ptr->py, 1, TRUE, FALSE);
		ident = TRUE;
		break;
	case SV_SCROLL_STAR_ACQUIREMENT:
		acquirement(p_ptr->px, p_ptr->py, rand_range(2, 3), TRUE, FALSE);
		ident = TRUE;
		break;
	case SV_SCROLL_FIRE:
		fire_ball(GF_FIRE, 0, 300, 4);
		// Note: "Double" damage since it is centered on the player ...
		if ((p_ptr->oppose_fire == 0) && !(p_ptr->resist_fire) && !(p_ptr->immune_fire))
			take_hit(rand_range(50, 100), "a Scroll of Fire");
		ident = TRUE;
		break;
	case SV_SCROLL_ICE:
		fire_ball(GF_ICE, 0, 350, 4);
		if ((p_ptr->oppose_cold == 0) && !(p_ptr->resist_cold) && !(p_ptr->immune_cold))
			take_hit(rand_range(100, 200), "a Scroll of Ice");
		ident = TRUE;
		break;
	case SV_SCROLL_CHAOS:
		fire_ball(GF_CHAOS, 0, 400, 4);
		if (!p_ptr->resist_chaos)
			take_hit(rand_range(150, 300), "a Scroll of Chaos");
		ident = TRUE;
		break;		
	case SV_SCROLL_RUMOR:
		{
			char rumor[1024];
			msgf("There is message on the scroll. It says:");
			message_flush();
			if (!get_rnd_line("rumors.txt", 0, rumor))	msgf(rumor);
			message_flush();
			msgf("The scroll disappears in a puff of smoke!");
			ident = TRUE;
			break;
		}
	case SV_SCROLL_ARTIFACT:
		if (!artifact_scroll()) used_up = FALSE;
		ident = TRUE;
		break;
	}
	
	/* Hack - the scroll may already be destroyed by its effect */
	if (o_ptr->k_idx)
	{
		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


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
	if (p_ptr->blind)
	{
		msgf("You can't see anything.");
		return;
	}
	if (no_lite())
	{
		msgf("You have no light to read by.");
		return;
	}
	if (p_ptr->confused)
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
	int chance, lev, k, r_max;
	bool ident, use_charge;

	use_charge = TRUE;

	/* Mega-Hack -- refuse to use a pile from the ground */
	if (floor_item(o_ptr) && (o_ptr->number > 1))
	{
		msgf("You must first pick up the staffs.");
		return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

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

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

		return;
	}


	/* Sound */
	sound(SOUND_ZAP);

	/* Use the staff */
	switch (o_ptr->sval)
	{
	case SV_STAFF_DARKNESS:
		if (!p_ptr->resist_blind && !p_ptr->resist_dark)
		{
			if (set_blind(p_ptr->blind + rand_range(4, 8))) ident = TRUE;
		}
		if (unlite_area(10, 3)) ident = TRUE;
		break;
	case SV_STAFF_SLOWNESS:
		if (set_slow(p_ptr->slow + rand_range(15, 45))) ident = TRUE;
		break;
	case SV_STAFF_HASTE_MONSTERS:
		if (speed_monsters()) ident = TRUE;
		break;
	case SV_STAFF_SUMMONING:
		r_max = randint1(4);
		for (k = 0; k<r_max; k++)
			if (summon_specific(0, p_ptr->px, p_ptr->py, p_ptr->depth, 0, TRUE, FALSE, FALSE))
				ident = TRUE;
		break;
	case SV_STAFF_TELEPORTATION:
		teleport_player(100);
		ident = TRUE;
		break;		
	case SV_STAFF_IDENTIFY:
		if (!ident_spell()) use_charge = FALSE;;
		ident = TRUE;
		break;
	case SV_STAFF_REMOVE_CURSE:
		if (remove_curse())
		{
			if (p_ptr->blind == 0) msgf("The staff glows blue for a moment...");
		}
		ident = TRUE;
		break;
	case SV_STAFF_STARLITE:
		if (p_ptr->blind == 0) msgf("The; of the staff glows brightly...");
		starlite();
		ident = TRUE;
		break;
	case SV_STAFF_LITE:
		if (lite_area(damroll(2, 8), 2)) ident = TRUE;
		break;
	case SV_STAFF_MAPPING:
		map_area();
		ident = TRUE;
		break;
	case SV_STAFF_DETECT_GOLD:
		if (detect_treasure()) ident = TRUE;
		if (detect_objects_gold()) ident = TRUE;
		break;
	case SV_STAFF_DETECT_ITEM:
		if (detect_objects_normal()) ident = TRUE;
		break;
	case SV_STAFF_DETECT_TRAP:
		if (detect_traps()) ident = TRUE;
		break;
	case SV_STAFF_DETECT_DOOR:
		if (detect_doors()) ident = TRUE;
		if (detect_stairs()) ident = TRUE;
		break;
	case SV_STAFF_DETECT_INVIS:
		if (detect_monsters_invis()) ident = TRUE;
		break;
	case SV_STAFF_DETECT_EVIL:
		if (detect_monsters_evil()) ident = TRUE;
		break;
	case SV_STAFF_CURE_LIGHT:
		if (hp_player(40)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_cut(p_ptr->cut - 10)) ident = TRUE;
		break;
	case SV_STAFF_CURING:
		if (hp_player(150)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		if (set_image(0)) ident = TRUE;
		break;
	case SV_STAFF_HEALING:
		if (hp_player(300)) ident = TRUE;
		if (set_blind(0)) ident = TRUE;
		if (set_confused(0)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		break;
	case SV_STAFF_THE_MAGI:
		if (do_res_stat(A_INT)) ident = TRUE;
		if (p_ptr->csp < p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
			p_ptr->csp_frac = 0;
			msgf("Your feel your head clear.");
			p_ptr->redraw |= PR_MANA;
			p_ptr->window |= PW_PLAYER;
			p_ptr->window |= PW_SPELL;
			ident = TRUE;
		}
		break;
	case SV_STAFF_SLEEP_MONSTERS:
		if (sleep_monsters()) ident = TRUE;
		break;
	case SV_STAFF_SLOW_MONSTERS:
		if (slow_monsters()) ident = TRUE;
		break;
	case SV_STAFF_SPEED:
		if (p_ptr->fast == 0)
		{
			if (set_fast(rand_range(15, 45))) ident = TRUE;
		}
		else
		{
			set_fast(p_ptr->fast + 5);
		}
		break;
	case SV_STAFF_PROBING:
		probing();
		ident = TRUE;
		break;
	case SV_STAFF_DISPEL_EVIL:
		if (dispel_evil(60)) ident = TRUE;
		break;
	case SV_STAFF_POWER:
		if (dispel_monsters(300)) ident = TRUE;
		break;
	case SV_STAFF_HOLINESS:
		if (dispel_evil(300)) ident = TRUE;
		k = 3 * p_ptr->lev;
		if (set_protevil(p_ptr->protevil + randint1(25) + k)) ident = TRUE;
		if (set_poisoned(0)) ident = TRUE;
		if (set_afraid(0)) ident = TRUE;
		if (hp_player(50)) ident = TRUE;
		if (set_stun(0)) ident = TRUE;
		if (set_cut(0)) ident = TRUE;
		break;
	case SV_STAFF_GENOCIDE:
		genocide(TRUE);
		ident = TRUE;
		break;
	case SV_STAFF_EARTHQUAKES:
		if (earthquake(p_ptr->px, p_ptr->py, 10)) ident = TRUE;
		else msgf("The dungeon trembles.");
		break;
	case SV_STAFF_DESTRUCTION:
		if (destroy_area(p_ptr->px, p_ptr->py, 15)) ident = TRUE;
	}
	
	/* Hack - the staff may destroy itself when activated on the ground */
	if (o_ptr->k_idx)
	{
		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Tried the item */
		object_tried(o_ptr);

		/* An identification was made */
		if (ident && !object_aware_p(o_ptr))
		{
			object_aware(o_ptr);
			gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


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
	bool ident, use_charge, success;
	int dir, sval, lev, chance, choice;

	use_charge = TRUE;
	ident = FALSE;
	success = FALSE;

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

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

		return;
	}

	// Allow direction to be cancelled for free
	success = get_aim_dir(&dir);

	if (!success)	return;

	// Take a turn
	p_ptr->energy_use = min(75, 200 - 5 * p_ptr->skill_dev / 8);

	// ! identified yet
	ident = FALSE;

	// Get the object level
	lev = k_info[o_ptr->k_idx].level;

	// Base chance of success
	chance = p_ptr->skill_dev;

	// Confusion hurts skill
	if ((p_ptr->confused != 0)) chance = chance / 2;

	// High level objects are harder
	chance = chance - lev / 2;

	// Give everyone a (slight) chance
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1)) chance = USE_DEVICE;

	// Roll for usage
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msgf("You failed to use the wand properly.");
		sound(SOUND_FAIL);
		return;
	}

	// The wand is already empty!
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
		msgf("The wand has no charges left.");
		o_ptr->info   |= OB_EMPTY;
		p_ptr->notice |= PN_COMBINE | PN_REORDER;
		p_ptr->window |= PW_INVEN;
		return;
	}

	// Sound
	sound(SOUND_ZAP);

	sval = o_ptr->sval;

	// Hack -- Wand of wonder can do anything before it
	if (sval == SV_WAND_WONDER) sval = randint0(SV_WAND_WONDER);

	switch (sval)
	{
	case SV_WAND_HEAL_MONSTER:
		if (heal_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_HASTE_MONSTER:
		if (speed_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_CLONE_MONSTER:
		if (clone_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_TELEPORT_AWAY:
		if (teleport_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_DISARMING:
		if (disarm_trap(dir)) ident = TRUE;
		break;
	case SV_WAND_TRAP_DOOR_DEST:
		if (destroy_door(dir)) ident = TRUE;
		break;
	case SV_WAND_STONE_TO_MUD:
		if (wall_to_mud(dir)) ident = TRUE;
		break;
	case SV_WAND_LITE:
		msgf("A line of blue shimmering light appears.");
		lite_line(dir);
		ident = TRUE;
		break;
	case SV_WAND_SLEEP_MONSTER:
		if (sleep_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_SLOW_MONSTER:
		if (slow_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_CONFUSE_MONSTER:
		if (confuse_monster(dir, 20)) ident = TRUE;
		break;
	case SV_WAND_FEAR_MONSTER:
		if (fear_monster(dir, 20)) ident = TRUE;
		break;
	case SV_WAND_DRAIN_LIFE:
		if (drain_life(dir, 150)) ident = TRUE;
		break;
	case SV_WAND_POLYMORPH:
		if (poly_monster(dir)) ident = TRUE;
		break;
	case SV_WAND_STINKING_CLOUD:
		ident = fire_ball(GF_POIS, dir, 15, 2);
		break;
	case SV_WAND_MAGIC_MISSILE:
		ident = fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2, 6));
		break;
	case SV_WAND_ACID_BOLT:
		ident = fire_bolt_or_beam(20, GF_ACID, dir, damroll(6, 8));
		break;
	case SV_WAND_CHARM_MONSTER:
		ident = charm_monster(dir, 45);
		break;
	case SV_WAND_FIRE_BOLT:
		ident = fire_bolt_or_beam(20, GF_FIRE, dir, damroll(10, 8));
		break;
	case SV_WAND_COLD_BOLT:
		ident = fire_bolt_or_beam(20, GF_COLD, dir, damroll(6, 8));
		break;
	case SV_WAND_ACID_BALL:
		ident = fire_ball(GF_ACID, dir, 125, 2);
		break;
	case SV_WAND_ELEC_BALL:
		ident = fire_ball(GF_ELEC, dir, 75, 2);
		break;
	case SV_WAND_FIRE_BALL:
		ident = fire_ball(GF_FIRE, dir, 150, 2);
		break;
	case SV_WAND_COLD_BALL:
		ident = fire_ball(GF_COLD, dir, 100, 2);
		break;
	case SV_WAND_WONDER:
		msgf("Oops.  Wand of wonder activated.");
		break;
	case SV_WAND_DRAGON_FIRE:
		ident = fire_ball(GF_FIRE, dir, 250, 3);
		ident = TRUE;
		break;
	case SV_WAND_DRAGON_COLD:
		ident = fire_ball(GF_COLD, dir, 200, 3);
		ident = TRUE;
		break;
	case SV_WAND_DRAGON_BREATH:
		choice = randint1(5);
		switch (choice)
		{
		case 1:
			ident = fire_ball(GF_ACID, dir, 250, 3);
			break;
		case 2:
			ident = fire_ball(GF_ELEC, dir, 150, 3);
			break;
		case 3:
			ident = fire_ball(GF_FIRE, dir, 200, 3);
			break;
		case 4:
			ident = fire_ball(GF_COLD, dir, 200, 3);
			break;
		case 5:
			ident = fire_ball(GF_POIS, dir, 200, 3);
			break;
		}
		ident = TRUE;
		break;
		
	case SV_WAND_ANNIHILATION:
		ident = fire_ball(GF_DISINTEGRATE, dir, rand_range(125, 225), 2);
		break;
	case SV_WAND_ROCKETS:
		msgf("You launch a rocket!");
		fire_ball(GF_ROCKET, dir, 250, 2);
		ident = TRUE;
		break;
	}
	
	/* Hack - wands may destroy themselves if activated on the ground */
	if (o_ptr->k_idx)
	{
		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Mark it as tried */
		object_tried(o_ptr);

		/* Apply identification */
		if (ident && !object_aware_p(o_ptr))
		{
			int lev = get_object_level(o_ptr);

			object_aware(o_ptr);
			gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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
 * Hack -- rods of Identify/genocide can be "cancelled"
 * All rods can be cancelled at the "Direction?" prompt
 *
 * pvals are defined for each rod in k_info. -LM-
 */
static void do_cmd_zap_rod_aux(object_type *o_ptr)
{
	int ident, chance, dir, lev;

	/* Hack -- let Identification get aborted */
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
	p_ptr->energy_use = MIN(75, 200 - 5 * p_ptr->skill_dev / 8);

	/* Not identified yet */
	ident = FALSE;

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

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
			word_of_recall();
			ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(4, 8), 2)) ident = TRUE;
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
			ident = detect_all();
			break;
		}

		case SV_ROD_PROBING:
		{
			ident = probing();
			break;
		}

		case SV_ROD_CURING:
		{
			if (hp_player(200)) ident = TRUE;
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
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
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
				if (set_fast(rand_range(15, 45))) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			break;
		}

		case SV_ROD_PESTICIDE:
		{
			ident = fire_ball(GF_POIS, dir, 8, 3);
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
			msgf("A line of blue shimmering light appears.");
			(void)lite_line(dir);
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
			if (drain_life(dir, 150)) ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			ident = fire_bolt_or_beam(10, GF_ACID, dir, damroll(6, 8));
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			ident = fire_bolt_or_beam(10, GF_ELEC, dir, damroll(5, 8));
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			ident = fire_bolt_or_beam(10, GF_FIRE, dir, damroll(10, 8));
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			ident = fire_bolt_or_beam(10, GF_COLD, dir, damroll(6, 8));
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			ident = fire_ball(GF_ACID, dir, 125, 2);
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			ident = fire_ball(GF_ELEC, dir, 75, 2);
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			ident = fire_ball(GF_FIRE, dir, 150, 2);
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			ident = fire_ball(GF_COLD, dir, 100, 2);
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
		gain_exp((lev + p_ptr->lev / 2) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

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
	u32b f1, f2, f3;

	/* Check statues */
	if (o_ptr->tval == TV_STATUE) return (TRUE);

	/* Ignore dungeon objects */
	if (o_ptr->iy || o_ptr->ix) return (FALSE);

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
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static void do_cmd_activate_aux(object_type *o_ptr)
{
	int dir, lev, chance;

	/* Take a turn */
	p_ptr->energy_use = MIN(75, 200 - 5 * p_ptr->skill_dev / 8);

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance /= 2;

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

	if (o_ptr->activate)
	{
		(void)activate_effect(o_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		make_noise(3);

		/* Success */
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
				msgf("You breathe lightning.");
				(void)fire_ball(GF_ELEC, dir, 330, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_WHITE:
			{
				msgf("You breathe frost.");
				(void)fire_ball(GF_COLD, dir, 370, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_BLACK:
			{
				msgf("You breathe acid.");
				(void)fire_ball(GF_ACID, dir, 430, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_GREEN:
			{
				msgf("You breathe poison gas.");
				(void)fire_ball(GF_POIS, dir, 500, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_RED:
			{
				msgf("You breathe fire.");
				(void)fire_ball(GF_FIRE, dir, 670, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_MULTIHUED:
			{
				chance = randint0(5);
				msgf("You breathe %s.",
						   ((chance == 1) ? "lightning" :
							((chance == 2) ? "frost" :
							 ((chance == 3) ? "acid" :
							  ((chance == 4) ? "poison gas" : "fire")))));
				(void)fire_ball(((chance == 1) ? GF_ELEC :
								 ((chance == 2) ? GF_COLD :
								  ((chance == 3) ? GF_ACID :
								   ((chance == 4) ? GF_POIS : GF_FIRE)))),
								dir, 840, 2);
				o_ptr->timeout = (s16b)rand_range(25, 50);
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				msgf("You breathe confusion.");
				(void)fire_ball(GF_CONFUSION, dir, 400, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_GOLD:
			{
				msgf("You breathe sound.");
				(void)fire_ball(GF_SOUND, dir, 430, 2);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = randint0(2);
				msgf("You breathe %s.",
						   ((chance == 1 ? "chaos" : "disenchantment")));
				(void)fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
								dir, 740, 2);
				o_ptr->timeout = (s16b)rand_range(30, 60);
				break;
			}

			case SV_DRAGON_LAW:
			{
				chance = randint0(2);
				msgf("You breathe %s.",
						   ((chance == 1 ? "sound" : "shards")));
				(void)fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
								dir, 750, 2);
				o_ptr->timeout = (s16b)rand_range(30, 60);
				break;
			}

			case SV_DRAGON_BALANCE:
			{
				chance = randint0(4);
				msgf("You breathe %s.",
						   ((chance == 1) ? "chaos" :
							((chance == 2) ? "disenchantment" :
							 ((chance == 3) ? "sound" : "shards"))));
				(void)fire_ball(((chance == 1) ? GF_CHAOS :
								 ((chance == 2) ? GF_DISENCHANT :
								  ((chance == 3) ? GF_SOUND : GF_SHARDS))),
								dir, 840, 2);
				o_ptr->timeout = (s16b)rand_range(30, 60);
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = randint0(2);
				msgf("You breathe %s.",
						   ((chance == 0 ? "light" : "darkness")));
				(void)fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 670, 2);
				o_ptr->timeout = (s16b)rand_range(30, 60);
				break;
			}

			case SV_DRAGON_POWER:
			{
				msgf("You breathe the elements.");
				(void)fire_ball(GF_MISSILE, dir, 1000, 3);
				o_ptr->timeout = (s16b)rand_range(30, 60);
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		make_noise(4);

		/* Success */
		return;
	}

	/* Mistake */
	msgf("Oops.  That object cannot be activated.");
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


/*
 * Imbue Item
 *
 * Imbues a soul into an amulet or ring
 */
void do_cmd_imbuesoul(void)
{
	object_type *o_ptr;
	object_type *s_ptr;
	cptr q, s;

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_imbue;

	/* Get an item */
	q = "Imbue which item? ";
	s = "You have no imbueable items.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR | USE_EQUIP));

	/* No valid item */
	if (!o_ptr) return;

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_soulgem;

	/* Get an item */
	q = "Which soul will be imbued? ";
	s = "You have no soul gems.";

	s_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR ));

	/* No valid item */
	if (!s_ptr) return;

	//give it a soul type

	o_ptr->soul_source = s_ptr->soul_source;
	o_ptr->soul_type1  = s_ptr->soul_type1;
	o_ptr->soul_type2  = s_ptr->soul_type2;

	/* Set the first flags */
	o_ptr->flags1 |= s_info[o_ptr->soul_type1].flags1[0];
	o_ptr->flags2 |= s_info[o_ptr->soul_type1].flags2[0];
	o_ptr->flags3 |= s_info[o_ptr->soul_type1].flags3[0];

	o_ptr->flags1 |= s_info[o_ptr->soul_type2].flags1[0];
	o_ptr->flags2 |= s_info[o_ptr->soul_type2].flags2[0];
	o_ptr->flags3 |= s_info[o_ptr->soul_type2].flags3[0];

	o_ptr->pval = 1;

	//start the levels counting
	o_ptr->level = 1;

	/* gain any bonuses to AC / Damage / Accuracy */

	o_ptr->to_h = (o_ptr->level * s_info[o_ptr->soul_type1].max_to_h) / 6;
	o_ptr->to_d = (o_ptr->level * s_info[o_ptr->soul_type1].max_to_d) / 6;
	o_ptr->to_a = (o_ptr->level * s_info[o_ptr->soul_type1].max_to_a) / 6;


	/* Identify the item */
	identify_item(o_ptr);
	object_mental(o_ptr);

	/* Save all the known flags */
	o_ptr->kn_flags1 = o_ptr->flags1;
	o_ptr->kn_flags2 = o_ptr->flags2;
	o_ptr->kn_flags3 = o_ptr->flags3;

	/* if we are imbuing a stack of rings, put a stop to that nonsense */
	if (o_ptr->number > 1)
	{
		msgf("You cannot imbue more than one item.");
		msgf("The excess %d are destroyed.", (o_ptr->number) - 1);

		o_ptr->number = 1;

		/* Notice weight changes */
		p_ptr->update |= PU_WEIGHT;
	}

	/* Take the soul away from the player, describe the result */
	item_increase(s_ptr, -1);
}
