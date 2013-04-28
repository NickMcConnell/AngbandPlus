/* File: cmd6.c */

/*
 * Effects and extra information on individual food and mushrooms,
 * potions, scrolls, staffs, wands, and rods.  Chance to get extra object
 * information.  Use objects, use magical devices.  Activation effects
 * and extra information, activate an item.
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Food, scrolls, potions, and magical devices become "tried" once used.
 * If the character observed any obvious effects, the name (but not the
 * charges or special effects) of an unaware item also become known, and
 * he gets experience.  Further uses of that kind of object will even-
 * tually yield greater knowledge of damage, duration, etc.
 *
 * Learning by using:
 * The game tries to encourage players to learn by using.
 * It is therefore mandatory that no unaware food, potion, or scroll
 * instantly kill the character.  Reducing exp permanently, adding any
 * more items that drain stats permanently, paralyzing the character
 * for extended periods of time, or trashing non-replaceable items like
 * artifacts (unless they can be repaired), would also be Bad Ideas.
 *
 * Problem -- Object effects destroying objects in inventory:
 * New code will need to be added if objects are ever able to destroy
 * themselves or objects above themselves in inventory.  The easiest way
 * to get in trouble is to add a staff of recharging.
 */


/*
 * Remember if object will be identified and/or used up.
 */
static int obj_ident;
static int obj_used_up;

/*
 * Hack -- allow ID messages and exp cancelling.
 */
static int hack_id_notice_suppress = 0;


/*
 * Handle the effects of individual foods and mushrooms, potions, and
 * scrolls.
 */
cptr do_object(int mode, object_type *o_ptr)
{
	/* Get the object kind */
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Modes of operation */
	bool use  = (mode == OBJECT_USE);
	bool info = (mode == OBJECT_INFO);

	/* Output special message only once */
	u16b msg = !(k_ptr->special & (SPECIAL_MESSAGE));

	/* Note whether we are aware of the effects of this item */
	u16b aware = TRUE;

	int py = p_ptr->py;
	int px = p_ptr->px;

	int dur, dur1, dur2;
	int pow, pow1, pow2;
	int goat, dam, choice, i;

	cptr extra = "";
	char buf[DESC_LEN];
	char o_name[DESC_LEN];


	/* Describe the (singular) object */
	object_desc_plural = -1;
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

	/* In order to get the unaware bonus, an object must be unsensed too */
	if ((!object_aware_p(o_ptr)) && (!(o_ptr->ident & (IDENT_SENSE))))
	{
		aware = FALSE;
	}

	/* Assume no special ID suppression */
	hack_id_notice_suppress = 0;


	/* Jump to the right category of item */
	if      (o_ptr->tval == TV_FOOD)   goto do_food;
	else if (o_ptr->tval == TV_POTION) goto do_potion;
	else if (o_ptr->tval == TV_SCROLL) goto do_scroll;
	else return ("");


	/*** Handle Food and Mushrooms ***/
	do_food:

	/* Goats can eat almost anything */
	goat = (p_ptr->schange == SHAPE_GOAT);

	/* Note action in saved messages */
	if (use) msg_add(format("You eat %s.", o_name));

	/* Analyze the food */
	switch (o_ptr->sval)
	{
		case SV_FOOD_BLINDNESS:
		{
			if (info) return ("");

			if (!(p_ptr->resist_blind || goat))
			{
				if (set_blind(p_ptr->blind + rand_range(50, 150),
					"A veil of darkness surrounds you."))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARANOIA:
		{
			if (info) return ("");

			if (!(p_ptr->resist_fear || goat))
			{
				if (set_afraid(p_ptr->afraid + rand_range(15, 30)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_CONFUSION:
		{
			if (info) return ("");

			if (!(p_ptr->resist_confu || goat))
			{
				if (set_confused(p_ptr->confused + rand_range(20, 40)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_HALLUCINATION:
		{
			if (info) return ("");

			if (!(p_ptr->resist_chaos || goat))
			{
				if (set_image(p_ptr->image + rand_range(75, 100)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_PARALYSIS:
		{
			if (info) return ("");

			if (!(p_ptr->free_act || goat))
			{
				if (set_paralyzed(p_ptr->paralyzed + rand_range(7, 14)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_POISON:
		{
			if (info) return ("");

			if (!(p_ptr->resist_pois || p_ptr->oppose_pois || goat))
			{
				if (set_poisoned(p_ptr->poisoned + rand_range(20, 30)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_ENVENOMATION:
		{
			if (info) return ("");

			if (!goat)
			{
				/* Message */
				if (msg)
				{
					msg_print("Arghh!  It was a Mushroom of Envenomation!");
					hack_id_notice_suppress = 1;
				}

				/* Resist poison helps a lot */
				if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
				{
					dam = MIN(MAX(0, p_ptr->chp - 20), rand_range(80, 120));

					(void)set_poisoned(p_ptr->poisoned + rand_range(150, 200));
					if (take_hit(dam, 0, NULL, "a Mushroom of Envenomation")) break;

					msg_print("You vomit and pass out!");
					(void)set_food(p_ptr->food_fainting - 1);
					(void)set_paralyzed(p_ptr->paralyzed + rand_range(4, 6));

					(void)do_dec_stat(A_STR, rand_range(3, 6), FALSE,
						"The disease attacks your vitality!",
						"You resist some of the effects of the disease.");

					obj_ident = TRUE;
				}
				else if (!(p_ptr->resist_pois && p_ptr->oppose_pois))
				{
					dam = MIN(MAX(0, p_ptr->chp - 5), rand_range(20, 30));

					(void)set_poisoned(p_ptr->poisoned + rand_range(30, 45));
					if (take_hit(dam, 0, NULL,
						"a Mushroom of Envenomation")) break;

					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_SICKNESS:
		{
			if (info) return ("");

			if (!goat)
			{
				/* Some damage */
				dam = rand_range(25, 35);

				/* Message */
				if (msg)
				{
					msg_print("Arghh!  It was a Mushroom of Sickness!");
					hack_id_notice_suppress = 1;
				}

				/* Infect with disease */
				disease(&dam);

				/* Do not kill the character */
				dam = MIN(dam, MAX(0, p_ptr->chp - 40));
				if (dam < 0) dam = 0;

				/* Apply adjusted damage */
				(void)take_hit(dam, 0, NULL, "a Mushroom of Sickness");

				obj_ident = TRUE;
			}
			break;
		}

		case SV_FOOD_DISEASE:
		{
			if (info) return ("");

			if (!goat)
			{
				/* A lot of damage */
				dam = rand_range(120, 140);

				/* Message */
				if (msg)
				{
					msg_print("Arghh!  It was a Mushroom of Disease!");
					hack_id_notice_suppress = 1;
				}

				/* Infect with disease */
				disease(&dam);

				/* Do not kill the character */
				dam = MIN(dam, MAX(0, p_ptr->chp - 40));
				if (dam < 0) dam = 0;

				/* Apply a bit of pure damage */
				(void)take_hit(dam, 0, NULL, "a Mushroom of Disease");

				obj_ident = TRUE;
			}
			break;
		}

		case SV_FOOD_RUINATION:
		{
			if (info) return ("");

			/* We sustain everything */
			if (p_ptr->sustain_str && p_ptr->sustain_int &&
			    p_ptr->sustain_wis && p_ptr->sustain_dex &&
			    p_ptr->sustain_con && p_ptr->sustain_chr)
			{
				if (msg)
				{
					msg_print("Mushrooms of Ruination are extremely tasty!");
					hack_id_notice_suppress = 1;
				}
			}
			else
			{
				/* Do not kill the character */
				dam = MIN(MAX(0, p_ptr->chp - 1), rand_range(50, 100));
				if (dam < 0) dam = 0;
				(void)take_hit(dam, 0,
					"Your nerves and muscles feel weak and lifeless!",
					"a Mushroom of Ruination");

				if (!p_ptr->sustain_str)
					(void)dec_stat(A_STR, (goat ? rand_int(2) : 1), TRUE);
				if (!p_ptr->sustain_int)
					(void)dec_stat(A_INT, (goat ? rand_int(2) : 1), TRUE);
				if (!p_ptr->sustain_wis)
					(void)dec_stat(A_WIS, (goat ? rand_int(2) : 1), TRUE);
				if (!p_ptr->sustain_dex)
					(void)dec_stat(A_DEX, (goat ? rand_int(2) : 1), TRUE);
				if (!p_ptr->sustain_con)
					(void)dec_stat(A_CON, (goat ? rand_int(2) : 1), TRUE);
				if (!p_ptr->sustain_chr)
					(void)dec_stat(A_CHR, (goat ? rand_int(2) : 1), TRUE);
			}

			obj_ident = TRUE;
			break;
		}

		case SV_FOOD_METAMORPHOSIS:
		{
			/* Seldom change race at first, frequently change it afterwards */
			int race_odds = (!aware ? 15 : 3);

			if (info) return ("");

			/* Goats aren't innately nexus-resistant */

			/* Resist nexus allows a saving throw */
			if ((!p_ptr->resist_nexus) || (one_in_(3)))
			{
				/* Message */
				if (msg)
				{
					msg_print("Arghh!  It was a Mushroom of Metamorphosis!");
					hack_id_notice_suppress = 1;
				}

				/* Force the character back into his normal shape */
				if (p_ptr->schange != SHAPE_NORMAL)
				{
					msg_print("You are wrenched back into your normal form!");
					shapechange_perm(SHAPE_NORMAL);
				}

				/* Shuffle stats (resist nexus helps a lot) */
				shuffle_stats(p_ptr->resist_nexus ? 1 : 4);

				/* If no resist nexus, sometimes change race */
				if ((!p_ptr->resist_nexus) && (one_in_(race_odds)))
				{
					int new_race;
					int chp, mhp;

					/* Choose a second, different race */
					for (new_race = p_ptr->prace;
					     new_race == p_ptr->prace;
					     new_race = rand_int(MAX_RACES));

					/* Change race, update racial information */
					p_ptr->prace = new_race;
					rp_ptr = &race_info[p_ptr->prace];

					/* Store current and maximum hit points -JM */
					chp = p_ptr->chp;
					mhp = p_ptr->mhp;

					/* Reroll hp for new race */
					get_extra();

					/* Hack -- Reset chp to appropriate value -JM */
					calc_hitpoints();
					p_ptr->chp = div_round(p_ptr->mhp * chp, mhp);

					/* Message */
					msg_format("You polymorph into a %s!",
						race_info[p_ptr->prace].title);

					/* Update preferences for chosen race */
					(void)process_pref_file("race.prf");

					/* Update a lot of stuff */
					p_ptr->update |= (PU_BONUS | PU_SCORE | PU_HP | PU_MANA | PU_SPELLS);

					/* Fully update the visuals (see invisible, infravision) */
					p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_MONSTERS);

					/* Redraw everything */
					p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

					/* Window stuff */
					p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1 | PW_M_LIST | PW_O_LIST);

					/* Window stuff */
					p_ptr->window |= (PW_OVERHEAD | PW_MONSTER | PW_OBJECT);

					/* Hack -- update */
					handle_stuff();
				}
				else
				{
					/* Notice save (warning for next time...) */
					msg_print("Your body resists total alteration!");
				}

				obj_ident = TRUE;
			}

			break;
		}

		case SV_FOOD_MANIA:
		{
			if (info) return ("");

			if (!p_ptr->mania)
			{
				/* Message */
				if (msg)
				{
					msg_print("Arghh!  It was a Mushroom of Mania!");
					hack_id_notice_suppress = 1;
				}

				(void)set_mania(p_ptr->mania + rand_range(5000, 7500));
				obj_ident = TRUE;
			}
			break;
		}

		case SV_FOOD_REGEN_MANA:
		{
			dur = (p_ptr->regen_mana ?  50 + p_ptr->power / 2 :
			                           100 + p_ptr->power);
			if (p_ptr->regen_mana) extra = " longer";

			if (info) return (format("(duration: %d)", dur));

			/* Regain a little mana */
			(void)sp_player(4, NULL);

			/* Increase mana regen, display message */
			if (p_ptr->msp)
			{
				(void)set_regen_mana(p_ptr->regen_mana + dur);
			}

			/* It would be annoying if chars w/o mana couldn't ID this */
			obj_ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_POISON:
		{
			if (info) return ("");

			if (set_poisoned(0)) obj_ident = TRUE;
			if (set_diseased(0, "You purge the disease from your body."))
				obj_ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_BLINDNESS:
		{
			if (info) return ("");

			if (set_blind(0, "The veil of darkness lifts.")) obj_ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_CONFUSION:
		{
			if (info) return ("");

			if (set_confused(0)) obj_ident = TRUE;
			if (set_image(0)) obj_ident = TRUE;
			if (set_stun(p_ptr->stun - 5)) obj_ident = TRUE;
			break;
		}

		/* Cures some poison and disease, stuns, cuts, and heals a little */
		case SV_FOOD_MENDING:
		{
			if (info) return ("");

			if (set_poisoned(p_ptr->poisoned / 2 - 5)) obj_ident = TRUE;
			if (set_diseased(2 * p_ptr->diseased / 3 - 5, NULL)) obj_ident = TRUE;
			if (set_stun(0)) obj_ident = TRUE;
			if (set_cut(0)) obj_ident = TRUE;
			if (hp_player(damroll(5, 19))) obj_ident = TRUE;
			if (p_ptr->food < p_ptr->food_weak) set_food(p_ptr->food + 500);
			break;
		}

		/* Cures almost everything except major stuns/wounds */
		case SV_FOOD_CURING:
		{
			if (info) return ("");

			if (set_blind(0, NULL)) obj_ident = TRUE;
			if (set_poisoned(0)) obj_ident = TRUE;
			if (set_diseased(0, "You purge the disease from your body."))
				obj_ident = TRUE;
			if (set_confused(0)) obj_ident = TRUE;
			if (set_image(0)) obj_ident = TRUE;
			if (set_stun(p_ptr->stun - 10)) obj_ident = TRUE;
			if (set_cut(p_ptr->cut - 25)) obj_ident = TRUE;
			if (p_ptr->food < p_ptr->food_weak) set_food(p_ptr->food + 500);
			break;
		}

		case SV_FOOD_RESTORING:
		{
			if (info) return ("");

			if (restore_stats()) obj_ident = TRUE;
			if (p_ptr->food < p_ptr->food_weak) set_food(p_ptr->food + 250);
			break;
		}

		case SV_FOOD_BOLDNESS:
		{
			dur1 = 10;     dur2 = 20;

			if (info) return (format("(duration: %d-%d)", dur1, dur2));

			if (set_bold(rand_range(dur1, dur2))) obj_ident = TRUE;
			break;
		}

		case SV_FOOD_HEROISM:
		{
			dur1 = (p_ptr->hero ?  1 : 20);
			dur2 = (p_ptr->hero ? 20 : 40);
			if (p_ptr->hero) extra = " longer";

			if (info) return (format("(duration %d-%d%s)", dur1, dur2, extra));

			if (set_hero(p_ptr->hero + rand_range(dur1, dur2))) obj_ident = TRUE;
			break;
		}

		case SV_FOOD_BERSERKERGANG:
		{
			dur1 = 30;     dur2 = 60;

			if (info) return (format("(duration: %d-%d)", dur1, dur2));

			/* Message */
			if (msg)
			{
				msg_print("You have eaten of the Elixir of Battle!");
			}

			if (set_berserk(BERSERK_WEAKNESS_LENGTH + rand_range(dur1, dur2)))
				obj_ident = TRUE;
			break;
		}

		case SV_FOOD_VITALITY:
		{
			dur1 = (p_ptr->vitality ?  50 : 200);
			dur2 = (p_ptr->vitality ? 100 : 300);
			if (p_ptr->vitality) extra = " longer";

			if (info) return (format("(duration %d-%d%s)", dur1, dur2, extra));

			if (set_vitality(rand_range(dur1, dur2))) obj_ident = TRUE;
			if (p_ptr->food < p_ptr->food_weak) set_food(p_ptr->food + 2500);
			break;
		}

		case SV_FOOD_ARMORING:
		{
			dur1 = (p_ptr->steelskin ?  1 : 20);
			dur2 = (p_ptr->steelskin ? 20 : 40);
			if (p_ptr->steelskin) extra = " longer";

			if (info) return (format("(duration %d-%d%s)", dur1, dur2, extra));

			if (set_steelskin(p_ptr->steelskin + rand_range(dur1, dur2),
				"Your skin turns to steel!"))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_FOOD_SLIME_MOLD:
		{
			if (info) return ("");

			msg_print("That tastes ... slimy.");
			obj_ident = TRUE;
			break;
		}

		case SV_FOOD_BISCUIT:
		{
			if (info) return (format("(food value: %d)", k_ptr->pval));

			msg_print("The biscuit is hard and tasteless.");
			obj_ident = TRUE;
			break;
		}

		case SV_FOOD_RATION:
		case SV_FOOD_JERKY:
		{
			if (info) return (format("(food value: %d)", k_ptr->pval));

			msg_print("That tastes good.");
			obj_ident = TRUE;
			break;
		}

		case SV_FOOD_BEORNING:
		{
			if (info) return (format("(food value: %d)", k_ptr->pval));

			msg_print("The cakes of the Beornings are tasty.");
			(void)hp_player(damroll(5, 8));
			obj_ident = TRUE;
			break;
		}

		/* Waybread is always fully satisfying. */
		case SV_FOOD_WAYBREAD:
		{
			if (info) return ("");

			if ((p_ptr->prace != RACE_HALF_ORC) &&
			    (p_ptr->prace != RACE_HALF_TROLL))
			{
				msg_print("You happily eat the lembas.");
				(void)set_food(p_ptr->food_bloated - 50);  /* Avoid bloating */
				(void)hp_player(damroll(5, 10));
			}
			else
			{
				msg_print("You choke down the nasty elvish food.");
				(void)set_food((p_ptr->food + p_ptr->food_bloated) / 2 - 1);
				(void)hp_player(damroll(2, 5));
			}
			obj_ident = TRUE;
			break;
		}

		/* Orcish spirits is heady stuff. */
		case SV_FOOD_ORCISH_FIREWATER:
		{
			bool vomit = FALSE;
			if (info)
			{
				/* Tell about the basic effects */
				if ((p_ptr->prace != RACE_HALF_ORC) &&
					 (p_ptr->prace != RACE_HALF_TROLL))
					return ("(food, reduce pois/cut)");
				else
					return ("(food, reduce pois/stun/cut, heal)");
			}

			/* Orcs and Trolls handle Orcish spirits better */
			if ((p_ptr->prace != RACE_HALF_ORC) &&
			    (p_ptr->prace != RACE_HALF_TROLL))
			{
				msg_print("You force down the Orcish firewater...");
				if (one_in_(3))
				{
					msg_print("You vomit it up!");

					/* Less "effective" vomit */
					(void)set_food(MIN(p_ptr->food, p_ptr->food_weak - 1));
					(void)set_poisoned(p_ptr->cut - (5 + p_ptr->cut / 3));
					vomit = TRUE;
				}
				else
				{
					msg_print("and manage to keep it down.");
					(void)set_food((p_ptr->food + p_ptr->food_bloated) / 2 - 1);

					/* Reduce poison and cuts by 1/3rd, plus 5 */
					(void)set_poisoned(p_ptr->cut - (5 + p_ptr->cut / 3));
					(void)set_cut(p_ptr->cut - (5 + p_ptr->cut / 3));
					(void)hp_player(damroll(2, 5));
				}
			}
			else
			{
				msg_print("You knock back a slug of the firewater.");

				/* Reduce poison, stuns, and cuts by 1/3rd, plus 5 */
				(void)set_poisoned(p_ptr->cut - (5 + p_ptr->cut / 3));
				(void)set_stun(p_ptr->cut - (5 + p_ptr->cut / 3));
				(void)set_cut(p_ptr->cut - (5 + p_ptr->cut / 3));
				(void)hp_player(damroll(4, 5));
				obj_ident = TRUE;
			}

			/* Powerful stuff those orcs brew */
			if (!vomit)
			{
				if (one_in_(4))
				{
					(void)set_hero(p_ptr->hero + rand_range(8, 12));
				}
				else if (one_in_(3))
				{
					(void)set_fast(p_ptr->fast + rand_range(6, 9));
				}
				else if (one_in_(2))
				{
					(void)set_image(p_ptr->image + rand_range(6, 12));
				}
			}

			obj_ident = TRUE;
			break;
		}


		case SV_FOOD_PINT_OF_ALE:
		case SV_FOOD_PINT_OF_WINE:
		{
			if (info) return (format("(food value: %d)", k_ptr->pval));

			msg_print("Glugglugglug.");
			obj_ident = TRUE;
			break;
		}

		case SV_FOOD_ATHELAS:
		{
			if (info) return ("");

			msg_print("A fresh, clean essence rises, driving away wounds and poison.");
			(void)set_poisoned(0);
			(void)set_diseased(0, "You recover from disease.");
			(void)set_stun(0);
			(void)set_cut(0);
			if (p_ptr->black_breath)
			{
				msg_print("The hold of the Black Breath on you is broken!");
				p_ptr->redraw |= (PR_CONDITIONS);
			}
			p_ptr->black_breath = FALSE;
			obj_ident = TRUE;
			break;
		}
	}

	/* End of Food and Mushroom section */
	return ("");



	/*** Handle Potions ***/
	do_potion:

	/* Sound */
	sound(MSG_QUAFF);

	/* Note action in saved messages */
	if (use) msg_add(format("You quaff %s.", o_name));

	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		case SV_POTION_SLIME_MOLD:
		{
			if (info) return (format("(food value: %d)", o_ptr->pval));

			msg_print("You feel less thirsty.");
			obj_ident = TRUE;
			break;
		}

		case SV_POTION_GRENADE:
		{
			if (info) return (format("(damage: %dd%d)", o_ptr->dd, o_ptr->ds));

			msg_print("The potion explodes!");

			/* Potion explodes */
			obj_ident = potion_smash_effect(-1, p_ptr->py, p_ptr->px, o_ptr);

			break;
		}

		case SV_POTION_SLOWNESS:
		{
			if (info) return ("");

			if (set_slow(p_ptr->slow + rand_range(15, 40))) obj_ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			if (info) return (format("(but it does cure poison)"));

			msg_print("The potion makes you vomit!");
			(void)set_food(p_ptr->food_starving - 1);
			(void)set_poisoned(0);
			(void)set_paralyzed(p_ptr->paralyzed + 4);
			obj_ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (info) return ("");

			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + rand_range(10, 35)))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_BLINDNESS:
		{
			if (info) return ("");

			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + rand_range(50, 100),
					"A veil of darkness surrounds you."))
				{
					obj_ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_CONFUSION:
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

		case SV_POTION_SLEEP:
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

		case SV_POTION_LOSE_MEMORIES:
		{
			if (info) return ("");

			if (!p_ptr->hold_life)
			{
				/* Message */
				if (msg)
				{
					msg_print("Arghh!  It was a Potion of Lose Memories!");
					hack_id_notice_suppress = 2;
				}
				else
				{
					msg_print("You feel your memories fade.");
				}
				lose_exp(calc_spent_exp() / 4, FALSE);
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_INVIS:
		{
			dur1 = 12;      dur2 = 24;

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (set_detect_inv(p_ptr->detect_inv + rand_range(dur1, dur2)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_DEC_STR:
		case SV_POTION_DEC_INT:
		case SV_POTION_DEC_WIS:
		case SV_POTION_DEC_DEX:
		case SV_POTION_DEC_CON:
		case SV_POTION_DEC_CHR:
		{
			int stat = o_ptr->sval - SV_POTION_DEC_STR;

			if (info) return ("");

			/* Chance for permanent stat-loss if taken by surprise */
			if ((!aware) && (one_in_(2)))
			{
				char buf2[DESC_LEN];

				/* Get a (lowercase) description of this stat */
				strcpy(buf2, flag_creation_data[stat].desc);
				strlower(buf2);

				/* Message */
				msg_print("You feel a ruination spell acting on you...");

				/* Build a sustain string */
				(void)strnfmt(buf, sizeof(buf), "But your %s is sustained!", buf2);

				/* Attempt to lower the stat */
				if (do_dec_stat(stat, 1, TRUE, NULL, buf)) obj_ident = TRUE;
			}

			/* Temporary stat-loss */
			else
			{
				/* Roll for severity of stat loss */
				pow = 1;
				while (one_in_(2)) pow++;
				if (do_dec_stat(stat, pow, FALSE, NULL, NULL)) obj_ident = TRUE;
			}

			break;
		}

		case SV_POTION_DETONATIONS:
		{
			if (info) return (format("(damage %dd%d)", o_ptr->dd, o_ptr->ds));

			/* Hurt, but do not (immediately) kill, the character */
			(void)take_hit(MAX(0, p_ptr->chp - 10), 0,
				"Massive explosions rupture your body!",
				"a potion of Detonations");

			/* Heavy stunning, mortal wounds */
			(void)set_stun(p_ptr->stun + rand_range(HVY_STUN, KNOCKED_OUT));
			(void)set_cut(p_ptr->cut + 5000);

			obj_ident = TRUE;
			break;
		}

		case SV_POTION_DEATH:
		{
			if (info) return (format("(damage %dd%d)", o_ptr->dd, o_ptr->ds));

			/*
			 * If the potion is not identified, we don't kill the
			 * character.  If it is, he's a goner!
			 */
			if (!object_known_p(o_ptr)) pow =    0;
			else                        pow = 5000;

			/* Trash the stats (but not permanently) */
			for (i = 0; i < A_MAX; i++) dec_stat(i, 1000, FALSE);

			/* Total forget */
			lose_all_info("Your memories are wiped out!");
			wiz_dark(FALSE);

			/* Hurt, possibly even kill, the character */
			(void)take_hit(p_ptr->chp + pow, 0,
				"A feeling of Death flows through your body.",
				"committing suicide.  That's right:  SUICIDE");

			/* Character is still alive */
			if (!p_ptr->is_dead)
			{
				/* Use the standard display and center an 80 column view */
				display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
					80, 0);

				/* Hack -- Pretend to be dead */
				p_ptr->is_dead = TRUE;
				strcpy(p_ptr->died_from, "a potion of Death");

				/* Request appropriate music */
				music(MUSIC_DEATH + 100);

				/* Fake prompt */
				center_string(buf, sizeof(buf),
					"[(i)nformation, (m)essages, (f)ile dump, (v)iew scores, e(x)amine item, ESC]",
					display_width());

				/* Display the tombstone */
				print_tomb();

				/* Hide the cursor */
				inkey_cursor_hack[TERM_MAIN] = -1;

				/* Wait patiently */
				(void)inkey(ALLOW_CLICK);

				/* Hack -- Stop pretending to be dead */
				p_ptr->is_dead = FALSE;
				strcpy(p_ptr->died_from, "(alive and well)");


				/* Restore previous display */
				display_change(DSP_RESTORE | DSP_LOAD, 0, 0);

				/* Redraw everything */
				do_cmd_redraw();

				/* Saved! */
				if (msg)
				{
					/* Request appropriate music */
					danger_music_level(TRUE);

					/* Message */
					message_flush();
					message(MSG_L_BLUE, 500, "You are, apparently, reserved for a different fate.");
				}
			}

			/* Hack -- immediate return on death */
			else return ("");

			obj_ident = TRUE;
			break;
		}

		case SV_POTION_INFRAVISION:
		{
			dur1 = (aware ? 150 : 350);
			dur2 = (aware ? 200 : 500);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (set_tim_infra(p_ptr->tim_infra + rand_range(dur1, dur2)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_SLOW_POISON:
		{
			if (info) return ("");

			pow = aware ? p_ptr->poisoned / 2 : 0;

			if (set_poisoned(pow)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_MENTAL_AWARENESS:
		{
			dur1 = (aware ?  80 : 300);
			dur2 = (aware ? 120 : 400);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			set_tim_esp(p_ptr->tim_esp + rand_range(dur1, dur2));

			obj_ident = TRUE;
			break;
		}

		case SV_POTION_INVIS:
		{
			dur1 = (aware ?  40 : 200);
			dur2 = (aware ?  60 : 300);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			dur = rand_range(dur1, dur2);

			(void)set_invis(p_ptr->tim_invis + dur,
			                get_skill(S_STEALTH, 20, 40));
			(void)set_detect_inv(p_ptr->detect_inv + dur);

			obj_ident = TRUE;
			break;
		}

		case SV_POTION_SPEED:
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

		case SV_POTION_RESIST_HEAT:
		{
			dur1 = (aware ? 60 : 300);
			dur2 = (aware ? 80 : 400);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (set_oppose_fire(p_ptr->oppose_fire + rand_range(dur1, dur2)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			dur1 = (aware ? 60 : 300);
			dur2 = (aware ? 80 : 400);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (set_oppose_cold(p_ptr->oppose_cold + rand_range(dur1, dur2)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_POIS:
		{
			dur1 = (aware ? 60 : 300);
			dur2 = (aware ? 80 : 400);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (set_oppose_pois(p_ptr->oppose_pois + rand_range(dur1, dur2)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			pow1 = (aware ? 2 : 5);   pow2 = 10;

			if (info) return (format("(heal %dd%d)", pow1, pow2));

			if (hp_player(damroll(pow1, pow2))) obj_ident = TRUE;
			if (set_blind(2 * p_ptr->blind / 3, NULL)) obj_ident = TRUE;
			if (set_confused(2 * p_ptr->confused / 3)) obj_ident = TRUE;
			if (set_poisoned(p_ptr->poisoned - 3)) obj_ident = TRUE;
			if (set_diseased(p_ptr->diseased - 2, NULL)) obj_ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			pow1 = (aware ? 4 : 10);   pow2 = 15;

			if (info) return (format("(heal %dd%d)", pow1, pow2));

			if (hp_player(damroll(pow1, pow2))) obj_ident = TRUE;
			if (set_blind(2 * p_ptr->blind / 3, NULL)) obj_ident = TRUE;
			if (set_confused(2 * p_ptr->confused / 3)) obj_ident = TRUE;
			if (set_poisoned((3 * p_ptr->poisoned / 4) - 10)) obj_ident = TRUE;
			if (set_diseased(p_ptr->diseased - 5, NULL)) obj_ident = TRUE;
			if (set_cut((2 * p_ptr->cut / 3) - 25)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			pow1 = (aware ? 6 : 15);   pow2 = 25;

			if (info) return (format("(heal %dd%d)", pow1, pow2));

			if (hp_player(damroll(pow1, pow2))) obj_ident = TRUE;
			if (set_blind(p_ptr->blind / 2 - 20, NULL)) obj_ident = TRUE;
			if (set_confused(p_ptr->confused / 2 - 10)) obj_ident = TRUE;
			if (set_poisoned((2 * p_ptr->poisoned / 3) - 15)) obj_ident = TRUE;
			if (set_diseased(p_ptr->diseased - 10, NULL)) obj_ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			pow = (aware ? 300 : 600);

			if (info) return (format("(heal %d)", pow));

			if (hp_player(pow)) obj_ident = TRUE;
			if (set_blind((p_ptr->blind / 3) - 80, NULL)) obj_ident = TRUE;
			if (set_confused((p_ptr->confused / 3) - 40)) obj_ident = TRUE;
			if (set_poisoned((p_ptr->poisoned / 2) - 40)) obj_ident = TRUE;
			if (set_diseased(p_ptr->diseased / 2 - 10, NULL)) obj_ident = TRUE;
			if (set_stun(0)) obj_ident = TRUE;
			if (set_cut((p_ptr->cut / 3) - 200)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			pow = (aware ? 600 : 1200);

			if (info) return (format("(heal %d)", pow));

			if (!aware)
			{
				if (restore_level()) obj_ident = TRUE;
				if (restore_stats()) obj_ident = TRUE;
			}
			if (hp_player(pow)) obj_ident = TRUE;
			if (set_blind(0, NULL)) obj_ident = TRUE;
			if (set_confused(0)) obj_ident = TRUE;
			if (set_poisoned(0)) obj_ident = TRUE;
			if (set_diseased(0, NULL)) obj_ident = TRUE;
			if (set_stun(0)) obj_ident = TRUE;
			if (set_cut(0)) obj_ident = TRUE;
			if (!aware)
			{
				if (set_mania(0)) obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_LIFE:
		{
			if (info) return ("(heal ****)");

			msg_print("You feel life flow through your body!");

			restore_level();
			(void)restore_stats();
			hp_player(p_ptr->mhp);
			(void)set_poisoned(0);
			(void)set_diseased(0, NULL);
			(void)set_blind(0, NULL);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_mania(0);
			if (p_ptr->black_breath)
			{
				msg_print("The hold of the Black Breath on you is broken!");
			}
			p_ptr->black_breath = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (info) return ("");

			if (p_ptr->csp < p_ptr->msp)
			{
				(void)sp_player(p_ptr->msp,
					"Your magical powers are completely restored!");
				obj_ident = TRUE;
			}
			else if (one_in_(3))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			if (info) return ("");

			if (restore_level()) obj_ident = TRUE;
			break;
		}

		case SV_POTION_RES_STR:
		case SV_POTION_RES_INT:
		case SV_POTION_RES_WIS:
		case SV_POTION_RES_DEX:
		case SV_POTION_RES_CON:
		case SV_POTION_RES_CHR:
		{
			int stat = o_ptr->sval - SV_POTION_RES_STR;

			if (info) return ("");

			if (do_res_stat(stat, NULL)) obj_ident = TRUE;

			if ((!aware) && (one_in_(3)))
			{
				msg_print("The liquid coursing down your throat gains in power!");
				(void)do_inc_stat(stat, 1, NULL);
				obj_ident = TRUE;
			}
			break;
		}

		case SV_POTION_INC_STR:
		case SV_POTION_INC_INT:
		case SV_POTION_INC_WIS:
		case SV_POTION_INC_DEX:
		case SV_POTION_INC_CON:
		case SV_POTION_INC_CHR:
		{
			int stat = o_ptr->sval - SV_POTION_INC_STR;

			if (info) return ("");

			if (do_inc_stat(stat, (aware ? 1 : randint(2)), NULL))
				obj_ident = TRUE;
			break;
		}

		case SV_POTION_RESIST_ALL:
		{
			dur1 = (aware ? 30 :  80);
			dur2 = (aware ? 60 : 160);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			dur = rand_range(dur1, dur2);

			if (set_oppose_fire(p_ptr->oppose_fire + dur)) obj_ident = TRUE;
			if (set_oppose_cold(p_ptr->oppose_cold + dur)) obj_ident = TRUE;
			if (set_oppose_acid(p_ptr->oppose_acid + dur)) obj_ident = TRUE;
			if (set_oppose_elec(p_ptr->oppose_elec + dur)) obj_ident = TRUE;
			if (set_oppose_pois(p_ptr->oppose_pois + dur)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (info) return ("");

			/* Restore and increase all stats by (at least) one */
			for (i = 0; i < A_MAX; i++)
			{
				if (res_stat(i)) obj_ident = TRUE;
				if (inc_stat(i, (aware ? 1 : randint(2)))) obj_ident = TRUE;
			}

			/* Message -- if we gained anything */
			if (obj_ident) msg_print("You feel power flow through your body!");
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			if (info) return ("");

			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite(FALSE, TRUE);
			obj_ident = TRUE;
			break;
		}
		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			if (info) return ("");

			set_self_knowledge(p_ptr->self_knowledge + 10,
				"You begin to feel more enlightened...");

			wiz_lite(TRUE, TRUE);
			msg_print("You suddenly see a vision of the entire dungeon!");

			(void)do_inc_stat(A_INT, (aware ? 1 : 2), NULL);
			(void)do_inc_stat(A_WIS, (aware ? 1 : 2), NULL);

			(void)detect_all(TRUE, aware);
			identify_pack();

			obj_ident = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			if (info) return ("");

			set_self_knowledge(p_ptr->self_knowledge + (aware ? 4 : 8),
				"You begin to know yourself a little better...");
			self_knowledge(TRUE);
			if (!msg) msg_print("While the effects last, you may type '~', option 8 to examine your attributes.");
			obj_ident = TRUE;
			break;
		}

		case SV_POTION_GAIN_SKILL:
		{
			/* Determine how much to increase skills */
			long gain = (calc_spent_exp() >= 25000L ? 12500L :
				calc_spent_exp() / 2);

			if (info) return ("");

			/* Restore and raise skills, see if character noticed */
			if (restore_level()) obj_ident = TRUE;
			if (raise_skills(gain)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_STAR_GAIN_SKILL:
		{
			/* Determine how much to increase skills */
			long gain = (calc_spent_exp() >= 100000L ? 50000L :
				calc_spent_exp() / 2);

			if (info) return ("");

			/* Restore and raise skills, see if character noticed */
			if (restore_level()) obj_ident = TRUE;
			if (raise_skills(gain)) obj_ident = TRUE;
			break;
		}

		case SV_POTION_TROLLFORM:
		{
			dur1 = (aware ? 40 : 160);
			dur2 = (aware ? 60 : 240);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (p_ptr->schange == SHAPE_TROLL) dur = p_ptr->form_dur + rand_range(dur1, dur2);
			else dur = rand_range(dur1, dur2);

			(void)shapechange_temp(dur, SHAPE_TROLL);
			obj_ident = TRUE;
			break;
		}

		case SV_POTION_DRAGONFORM:
		{
			dur1 = (aware ? 30 : 120);
			dur2 = (aware ? 50 : 200);

			if (info) return (format("(duration %d-%d)", dur1, dur2));

			if (p_ptr->schange == SHAPE_DRAGON) dur = p_ptr->form_dur + rand_range(dur1, dur2);
			else dur = rand_range(dur1, dur2);

			(void)shapechange_temp(dur, SHAPE_DRAGON);

			obj_ident = TRUE;
			break;
		}
	}

	/* End of Potion section */
	return ("");




	/*** Handle Scrolls ***/
	do_scroll:

	/* Note action in saved messages */
	if (use) msg_add(format("You read %s.", o_name));

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (info) return ("");

			if ((!p_ptr->resist_blind) && (!p_ptr->resist_dark))
			{
				if (set_blind(p_ptr->blind + rand_range(4, 8),
					"A veil of darkness surrounds you.")) obj_ident = TRUE;
			}
			if (unlite_area(10, 3)) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
			if (info) return ("");

			aggravate_monsters(-1, FALSE,
				"There is a high pitched humming noise.");
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
			if (info) return ("");

			if (curse_armor()) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_WEAPON:
		{
			if (info) return ("");

			if (curse_weapon()) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			if (info) return ("");

			sound(MSG_SUM_MONSTER);
			if (summon_specific(py, px, FALSE, p_ptr->depth + 2, 0,
				rand_range(3, 5)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			if (info) return ("");

			sound(MSG_SUM_UNDEAD);
			if (summon_specific(py, px, FALSE, p_ptr->depth + 3,
				SUMMON_UNDEAD, rand_range(2, 3)))
			{
				obj_ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_SUMMON_DEMONS:
		{
			if (info) return ("");

			if (unlite_area(10, 3))
			{
				msg_print("The lights go out!");
				obj_ident = TRUE;
			}

			message(MSG_SUM_DEMON, 0, "Fiery, twisted forms emerge from the darkness!");

			if (summon_specific(py, px, FALSE,
				MAX(p_ptr->depth, p_ptr->max_depth) + 3, SUMMON_HI_DEMON, 6))
			{
				obj_ident = TRUE;
			}

			if ((p_ptr->resist_fear) || (check_save(100)))
			{
				msg_print("You refuse to be frightened.");
			}
			else
			{
				(void)set_afraid(p_ptr->afraid + rand_range(35, 70));
			}

			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (info) return ("");

			/* Identify if "tried" */
			if ((trap_creation(py, px)) || (k_ptr->special & (SPECIAL_TRIED)))
				obj_ident = TRUE;

			/* If scroll is known, traps give no exp  XXX XXX */
			if (object_known_p(o_ptr)) no_exp_traps(py, px);
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			if (info) return ("(range 10)");

			teleport_player(10, TRUE, FALSE);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			if (info) return ("(range 100)");

			teleport_player(100, TRUE, FALSE);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			if (info) return ("");

			(void)teleport_player_level();
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			if (info) return ("");

			recall_player();
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			if (info) return ("");

			obj_ident = TRUE;
			if (!ident_spell()) obj_used_up = FALSE;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			if (info) return ("");

			obj_ident = TRUE;
			if (!identify_fully()) obj_used_up = FALSE;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (info) return ("");

			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				obj_ident = TRUE;
			}
			break;
		}
		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			if (info) return ("");

			if (remove_all_curse())
			{
				msg_print("A great power breaks the curses on your gear!");
				obj_ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_ENCHANT_ARMOR:
		{
			if (info) return ("");

			pow = (aware ? 1 : 2);
			obj_ident = TRUE;
			if (!enchant_spell(0, 0, pow, FALSE)) obj_used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			if (info) return ("");

			pow = (aware ? 1 : 2);
			if (!enchant_spell(pow, 0, 0, FALSE)) obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			if (info) return ("");

			pow = (aware ? 1 : 2);
			if (!enchant_spell(0, pow, 0, FALSE)) obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
			if (info) return ("");

			pow = (aware ? rand_range(4, 6) : 8);

			if (!enchant_spell(0, 0, pow, TRUE))
				obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (info) return ("");

			pow1 = (aware ? rand_range(2, 4) : 5);
			pow2 = (aware ? rand_range(4, 6) : 5);

			if (!enchant_spell(pow1, pow2, 0, TRUE))
				obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (info) return ("");

			pow = (aware ? 130 : 200);

			if (msg) msg_print("Magic power gathers around you...");
			if (!recharge(130, FALSE)) obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_RECHARGING:
		{
			if (info) return ("");

			pow = (aware ? 200 : 300);

			if (msg) msg_print("Powerful magics gather around you...");
			if (!recharge(200, FALSE)) obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_ELEMENTAL_ATTACKS:
		{
			pow1 = (aware ? 30 :  90);
			pow2 = (aware ? 40 : 120);

			if (info) return (format("(attacks %d-%d)", pow1, pow2));

			choice = randint(8);

			if (choice == 1)
			{
				(void)set_acid_attack(rand_range(pow1, pow2));
			}
			else if (choice == 2)
			{
				(void)set_elec_attack(rand_range(pow1, pow2));
			}
			else if ((choice == 3) || (choice == 4) || (choice == 5))
			{
				(void)set_fire_attack(rand_range(pow1, pow2));
			}
			else
			{
				(void)set_cold_attack(rand_range(pow1, pow2));
			}

			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			if (info) return ("");

			/* Easter Egg */
			if (msg)
			{
				message_flush();
				message(MSG_L_BLUE, 4, "Out of darkness, light!        ");
			}
			(void)lite_area(damroll(2, 8), 2);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			if (info) return ("");

			map_area(0, 0, FALSE);

			/* Message */
			if (msg) msg_print("You see a map of your surroundings.");

			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_GOLD:
		{
			if (info) return ("");

			if (detect_treasure(FALSE)) obj_ident = TRUE;
			if (detect_objects_gold(FALSE)) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_ITEM:
		{
			if (info) return ("");

			if (detect_objects_normal(FALSE)) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
			if (info) return ("");

			if (detect_traps(FALSE, aware)) obj_ident = TRUE;
			break;
		}
		case SV_SCROLL_DETECT_DOOR:
		{
			if (info) return ("");

			if (detect_doors(FALSE)) obj_ident = TRUE;
			if (detect_stairs(FALSE)) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
			if (info) return ("");

			if (detect_monsters_invis(FALSE, aware)) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_LEARN_MAGIC:
		{
			if (info) return ("");

			obj_ident = TRUE;
			if (!sense_magic()) obj_used_up = FALSE;
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			if (info) return ("");

			if (set_food(p_ptr->food_bloated - 50)) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		case SV_SCROLL_HOLY_CHANT:
		case SV_SCROLL_HOLY_PRAYER:
		{
			/* Different scrolls have different durations */
			if (o_ptr->sval == SV_SCROLL_BLESSING)
			{
				dur1 = (aware ? 12 : 30);
				dur2 = (aware ? 24 : 60);
			}
			else if (o_ptr->sval == SV_SCROLL_HOLY_CHANT)
			{
				dur1 = (aware ? 24 :  60);
				dur2 = (aware ? 48 : 120);
			}
			else
			{
				dur1 = (aware ? 48 : 120);
				dur2 = (aware ? 96 : 240);
			}

			/* Blessing is not fully cumulative */
			if (p_ptr->blessed)
			{
				dur1 /= 2;
				dur2 /= 2;
				extra = " longer";
			}

			if (info) return (format("(duration %d-%d%s)", dur1, dur2, extra));

			if (set_blessed(p_ptr->blessed + rand_range(dur1, dur2), NULL))
				obj_ident = TRUE;

			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (info) return ("");

			if (!(p_ptr->special_attack & (ATTACK_CONFUSE)))
			{
				msg_print("Your hands begin to glow.");
				p_ptr->special_attack |= (ATTACK_CONFUSE);
				obj_ident = TRUE;

				/* Print "special attacks" */
				left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

				/* Redraw conditions status */
				p_ptr->redraw |= (PR_CONDITIONS);
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			dur1 = get_skill(S_PIETY, 0, 100);
			dur2 = dur1 + 25;

			if (p_ptr->protevil)
			{
				dur1 /= 2;
				dur2 /= 2;
				extra = " longer";
			}

			if (info) return (format("(duration %d-%d%s)", dur1, dur2, extra));

			if (set_protevil(p_ptr->protevil + rand_range(dur1, dur2)))
				obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			if (info) return (format("(glyphs: %d out of %d)",
				num_glyph_on_level, MAX_GLYPHS));

			/* Use up scroll only if warding_glyph is created. */
			if (!warding_glyph(py, px)) obj_used_up = FALSE;
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_FORCE_DOOR:
		{
			if (info) return ("");

			if (force_doors_touch()) obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_QUESTING:
		{
			if (info) return ("");

			msg_print("A shimmering gate appears.");

			if (get_check("Walk through the gate of Questing?"))
			{
				/* On a special quest */
				p_ptr->special_quest = TRUE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
				obj_used_up = FALSE;
			}
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			if (info) return ("");

			if (msg) msg_print("KABOOM!");
			destroy_area(py, px, 15, TRUE, FALSE);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_POISON_CLOUD:
		{
			pow = 10;
			dur = 15;

			if (info) return (format("(damage ~%d for ~%d turns)", pow, dur));

			/* Get a new effect index */
			i = effect_prep();

			/* Note failure XXX */
			if (i < 0) break;

			/* We want an lingering cloud, */
			x_list[i].index = EFFECT_IRREGULAR_CLOUD;

			/* Of poison */
			x_list[i].type = GF_POIS;

			/* That starts at the character location. */
			x_list[i].y0 = x_list[i].y1 = py;
			x_list[i].x0 = x_list[i].x1 = px;

			/* Practices the magic device skill */
			x_list[i].practice_skill = S_DEVICE;

			/* It attacks every 10 -> 5 game turns, */
			x_list[i].time_delay = 10 - p_ptr->power / 20;

			/* Does damage, has a large radius, */
			x_list[i].power = rand_range(pow - 2, pow + 2);
			x_list[i].power2 = 7;

			/* And lasts for about 15 attacks */
			x_list[i].lifespan = rand_range(dur - 3, dur + 3);


			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_NEXUS_STORM:
		{
			if (info) return (format("(damage 75 each beam, radius 13)"));

			if ((!aware) && (msg))
				msg_print("This was a Scroll of Invoke Nexus!");

			/* Fire a storm of nexus beams */
			fire_storm(-1, GF_NEXUS, py, px, 75, 13, 5, 1, FALSE);

			/* Character does not resist nexus */
			if (!p_ptr->resist_nexus)
			{
				/* Then he probably should not have read this scroll */
				set_nexus_field(p_ptr->nexus_field + rand_range(3, 5), 200);
			}

			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_GENOCIDE:
		{
			if (info) return ("");

			if (!aware) msg_print("This is a genocide scroll.");
			if (!genocide(0))
			{
				/* Only use object up if genocide goes off */
				obj_used_up = FALSE;
			}
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_GENOCIDE:
		{
			if (info) return ("");

			msg_print("A long, shrill note sounds...");
			(void)mass_genocide(py, px);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			if (info) return ("");

			acquirement(py, px, (aware ? 1 : 2), TRUE);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			if (info) return ("");

			acquirement(py, px, (aware ? 3 : 5), TRUE);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_ALTER_REALITY:
		{
			if (info) return ("");

			msg_print("The world changes!");

			/* Leaving */
			p_ptr->leaving = TRUE;

			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_MADNESS:
		{
			if (info) return ("");

			judgement(10 + 4 * p_ptr->depth / 5);
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_NIGHTFALL:
		{
			if (info) return ("(damage: about 150 each monster)");

			nightfall();
			obj_ident = TRUE;
			break;
		}

		case SV_SCROLL_TREES:
		{
			if (info) return ("");

			(void)fire_star(GF_MAKE_TREES, 0, 5, rand_range(6, 9));
			obj_ident = TRUE;
			break;
		}
		case SV_SCROLL_WATER:
		{
			if (info) return ("");

			(void)fire_star(GF_MAKE_WATER, 0, 5, rand_range(5, 8));
			obj_ident = TRUE;
			break;
		}
		case SV_SCROLL_LAVA:
		{
			if (info) return ("");

			(void)fire_star(GF_MAKE_LAVA, 0, 5, rand_range(5, 7));
			obj_ident = TRUE;
			break;
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
	/* Get the object kind */
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Modes of operation */
	bool use  = (mode == OBJECT_USE);
	bool info = (mode == OBJECT_INFO);

	bool aware = object_aware_p(o_ptr);

	/* Output special message only once */
	bool msg = !(k_ptr->special & (SPECIAL_MESSAGE));

	int py = p_ptr->py;
	int px = p_ptr->px;

	/* Spell variables */
	int dam, dam1, dam2;
	int      dur1, dur2;
	int pow;
	int dice, sides;
	int k;

	int sval = o_ptr->sval;

	/* Allow pre-set targets (in uncontrolled mode) */
	int dir = 0;

	/* Usually ask for a direction when appropriate */
	bool need_dir = (uncontrolled ? FALSE : TRUE);

	/* Determine device skill (0 to 100) */
	int power = get_skill(S_DEVICE, 0, 100);

	char buf[DESC_LEN];
	char o_name[DESC_LEN];


	/* Describe the (singular) object */
	object_desc_plural = -1;
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

	/* Unknown wands and rods always need to be "used in a direction" */
	if ((use) && (!uncontrolled) && (!aware) &&
	    ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_ROD)))
	{
		if (!get_aim_dir(&dir)) return ("");
		need_dir = FALSE;
	}

	/* Jump to the right category of item */
	if      (o_ptr->tval == TV_STAFF) goto do_staff;
	else if (o_ptr->tval == TV_WAND)  goto do_wand;
	else if (o_ptr->tval == TV_ROD)   goto do_rod;
	else return ("");


	/*** Handle Staffs ***/
	do_staff:

	/* Sound */
	if (use) sound(MSG_USE_STAFF);

	/* Note action in saved messages */
	if (use) msg_add(format("You use %s.", o_name));

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:
		{
			if (info) return ("");
			if (use)
			{
				if ((!p_ptr->resist_blind) && (!p_ptr->resist_dark))
				{
					if (set_blind(p_ptr->blind + rand_range(6, 12),
						"A veil of darkness surrounds you.")) *ident = TRUE;
				}
				if (unlite_area(10, 3)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (info) return ("");
			if (use)
			{
				if (set_slow(p_ptr->slow + rand_range(15, 45))) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (info) return ("");
			if (use)
			{
				if (speed_monsters()) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			if (info) return ("");
			if (use)
			{
				sound(MSG_SUM_MONSTER);
				if (summon_specific(py, px, FALSE, p_ptr->depth + 3, 0, randint(4)))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_TELEPORTATION:
		{
			if (info) return ("(range: 100)");
			if (use)
			{
				teleport_player(100, TRUE, FALSE);
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_IDENTIFY:
		{
			if (info) return ("");
			if (use)
			{
				*ident = TRUE;
				if ((uncontrolled) || (!ident_spell())) return ("");
			}
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (info) return ("");
			if (use)
			{
				if (remove_curse())
				{
					if (msg && !p_ptr->blind)
						msg_print("The staff glows blue for a moment...");

					msg_print("A curse is broken!");

					*ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_STARLIGHT:
		{
			dam = get_skill(S_DEVICE, 25, 50);
			pow = rand_range(get_skill(S_DEVICE, 10, 20), 30);

			if (info) return (format("(damage: %d * %d)", dam, pow));
			if (use)
			{
				/* Note effects if not blind */
				if (!p_ptr->blind)
				{
					if (msg) msg_print("The staff glitters with unearthly light.");

					/* Hard not to identify. */
					*ident = TRUE;
				}

				/* Starlight */
				(void)beam_burst(py, px, GF_LITE, pow, dam);
			}
			break;
		}

		case SV_STAFF_LIGHT:
		{
			if (info) return ("");
			if (use)
			{
				if (lite_area(damroll(2, 8), 2))
				{
					/* Note effects if not blind */
					if (!p_ptr->blind) *ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_MAPPING:
		{
			if (info) return ("");
			if (use)
			{
				/* Use extended mapping */
				map_area(0, 0, TRUE);

				/* Message */
				if (msg) msg_print("You see a map of your surroundings.");

				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECT_GOLD:
		{
			if (info) return ("");
			if (use)
			{
				if (detect_treasure(TRUE)) *ident = TRUE;
				if (detect_objects_gold(TRUE)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECT_ITEM:
		{
			if (info) return ("");
			if (use)
			{
				if (detect_objects_normal(TRUE)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
			if (info) return ("");
			if (use)
			{
				if (detect_traps(TRUE, aware)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
			if (info) return ("");
			if (use)
			{
				if (detect_doors(TRUE)) *ident = TRUE;
				if (detect_stairs(TRUE)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
			if (info) return ("");
			if (use)
			{
				if (detect_monsters_invis(TRUE, aware)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (info) return ("");
			if (use)
			{
				if (detect_evil(TRUE, aware)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_CURE_MEDIUM:
		{
			pow = 15;

			if (info) return (format("(heal about %d%%, reduce cuts)", pow));
			if (use)
			{
				if (heal_player(pow, pow * 2)) *ident = TRUE;
				if (set_cut(p_ptr->cut - 25)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_HEALING:
		{
			pow = 30;

			if (info) return (format("(heal about %d%%, reduce cuts)", pow));
			if (use)
			{
				if (heal_player(pow, pow * 4)) *ident = TRUE;
				if (set_cut(p_ptr->cut - 150)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_BANISHMENT:
		{
			if (info) return ("");
			if (use)
			{
				if (banishment(RF3_EVIL, 50))
				{
					*ident = TRUE;
					if (msg) msg_print("A mighty force drives away evil!");
				}
			}
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			if (info) return ("");
			if (use)
			{
				if (sleep_monsters(get_skill(S_DEVICE, 25, 95))) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			if (info) return ("");
			if (use)
			{
				if (slow_monsters(get_skill(S_DEVICE, 25, 95))) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_SPEED:
		{
			cptr extra = (p_ptr->fast ? " longer" : "");

			dur1 = (p_ptr->fast ?  1 : 20);
			dur2 = (p_ptr->fast ? 10 : 45);

			if (info) return (format("(duration: %d-%d%s)", dur1, dur2, extra));
			if (use)
			{
				if (set_fast(p_ptr->fast + rand_range(dur1, dur2)))
					*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_PROBING:
		{
			if (info) return ("");
			if (use)
			{
				if (probing()) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			if (info) return ("(damage: 60)");
			if (use)
			{
				if (dispel_evil(60)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_POWER:
		{
			if (info) return ("(damage: 100)");
			if (use)
			{
				if (dispel_monsters(100)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (info) return ("(damage: 100)");
			if (use)
			{
				if (msg) msg_print("An aura of holiness surrounds you!");

				if (dispel_evil(100)) *ident = TRUE;
				k = get_skill(S_PIETY, 0, 100);
				if (set_protevil(p_ptr->protevil + randint(25) + k))
					*ident = TRUE;
				if (set_bold(rand_range(15, 30))) *ident = TRUE;
				if (hp_player(50)) *ident = TRUE;
				if (set_stun(0)) *ident = TRUE;
				if (set_cut(0)) *ident = TRUE;
			}
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			if (info) return ("");
			if (use)
			{
				if (msg) msg_print("The earth shakes!");

				earthquake(py, px, 10);
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			if (info) return ("");
			if (use)
			{
				if (msg) msg_print("KABOOM!");

				destroy_area(py, px, 15, TRUE, FALSE);
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DETECTION:
		{
			if (info) return ("");
			if (use)
			{
				(void)detect_all(FALSE, aware);
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_DOOMSPELLS:
		{
			dam1 =   0 + get_skill(S_DEVICE, 0, 200);
			dam2 = 100 + get_skill(S_DEVICE, 0, 200);

			if (info) return (format("(damage: %d-%d)", dam1, dam2));
			if (use)
			{
				/* Cast explosions of mana. */
				doomspells(FALSE, get_skill(S_DEVICE, 0, 100));

				/* Hard to mistake... */
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_CHAOS:
		{
			dam1 = get_skill(S_DEVICE, 0, 80) + (p_ptr->chaos_power * 8);
			dam2 = dam1 * 2;

			if (info) return (format("(damage: %d-%d)", dam1, dam2));
			if (use)
			{
				if (msg) msg_print("You unleash the powers of chaos!");

				/* Call chaos */
				call_chaos(rand_range(dam1, dam2));

				/* Hard to mistake... */
				*ident = TRUE;
			}
			break;
		}
	}

	/* We used the magical device */
	*used = TRUE;

	/* End of Staffs section */
	return ("");



	/*** Handle Wands ***/
	do_wand:

	/* Note action in saved messages */
	if (use) msg_add(format("You aim %s.", o_name));

	/* XXX Hack -- Wand of wonder can do anything before it */
	if ((use) && (sval == SV_WAND_WONDER))
	{
		/* Special message when using for the first time */
		if (msg)
		{
			msg_print("Wand of Wonder, do your thing, whatever it is that you might bring!");
			*ident = TRUE;
		}
		sval = rand_int(SV_WAND_WONDER);
	}

	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (heal_monster(dir, damroll(p_ptr->depth, 5))) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (speed_monster(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (clone_monster(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (teleport_monster(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_DISARMING:
		{
			if (info) return ("(chance: 95%)");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (disarm_trap(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_FORCE_DOOR:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (force_door(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_STONE_TO_MUD:
		{
			dam1 = get_skill(S_DEVICE, 10, 110);
			dam2 = dam1 + 20;

			if (info) return (format("(damage %d-%d)", dam1, dam2));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (wall_to_mud(dir, rand_range(dam1, dam2))) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_LITE:
		{
			if (info) return ("(damage: 4d5)");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				msg_print("A line of golden light appears.");
				lite_line(dir);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (sleep_monster(dir, get_skill(S_DEVICE, 25, 80)))
					*ident = TRUE;
			}
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (slow_monster(dir, get_skill(S_DEVICE, 25, 80)))
					*ident = TRUE;
			}
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (confuse_monster(dir, get_skill(S_DEVICE, 25, 80)))
					*ident = TRUE;
			}
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (fear_monster(dir, get_skill(S_DEVICE, 30, 80)))
					*ident = TRUE;
			}
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (info) return ("");
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (poly_monster(dir, get_skill(S_DEVICE, 35, 100)))
					*ident = TRUE;
			}
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			dam = 12;

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_ball(GF_POIS, dir, dam, 2);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			dice = 2;     sides = 6;

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_bolt_or_beam(10, GF_MANA, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			dice = get_skill(S_DEVICE, 5, 10);     sides = 8;

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_bolt_or_beam(power / 2, GF_ACID, dir,
					damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			dice = get_skill(S_DEVICE, 2, 7);     sides = 8;

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_bolt_or_beam(power / 2, GF_ELEC, dir,
					damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			dice = get_skill(S_DEVICE, 6, 12);     sides = 8;

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_bolt_or_beam(power / 2, GF_FIRE, dir,
					damroll(dice, sides));

				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			dice = get_skill(S_DEVICE, 4, 8);     sides = 8;

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_bolt_or_beam(power / 2, GF_COLD, dir,
					damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			dam = get_skill(S_DEVICE, 50, 140);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_ball(GF_ACID, dir, dam, 3);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			dam = get_skill(S_DEVICE, 30, 120);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_ball(GF_ELEC, dir, dam, 3);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			dam = get_skill(S_DEVICE, 60, 150);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_ball(GF_FIRE, dir, dam, 3);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			dam = get_skill(S_DEVICE, 40, 130);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_ball(GF_COLD, dir, dam, 3);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_WONDER:
		{
			if (info) return ("(various effects)");
			if (use)
			{
				msg_print("Oops.  Wand of wonder activated.");
			}
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			dam = get_skill(S_DEVICE, 40, 150);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (drain_life(dir, dam)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
			dam = get_skill(S_DEVICE, 120, 220);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (msg) msg_print("A black orb shoots out from your wand!");
				if (drain_life(dir, dam)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			dam = get_skill(S_DEVICE, 70, 220);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (msg) msg_print("Dragonfire!");
				(void)fire_arc(GF_FIRE, dir, dam, 7, 70);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			dam = get_skill(S_DEVICE, 70, 220);

			if (info) return (format("(damage: %d)", dam));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (msg) msg_print("Dragonfrost!");
				(void)fire_arc(GF_COLD, dir, dam, 7, 20);
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			dam = get_skill(S_DEVICE, 80, 240);

			if (info) return (format("(damage: %d-%d)", dam, dam + 30));
			if (use)
			{
				int tmp = randint(5);

				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (msg) msg_print("Dragon's breath!");

				if (tmp == 1) (void)fire_arc(GF_ACID, dir, dam + 20, 9, 70);
				if (tmp == 2) (void)fire_arc(GF_ELEC, dir, dam     , 9, 70);
				if (tmp == 3) (void)fire_arc(GF_COLD, dir, dam + 10, 9, 70);
				if (tmp == 4) (void)fire_arc(GF_FIRE, dir, dam + 30, 9, 70);
				if (tmp == 5) (void)fire_arc(GF_POIS, dir, dam + 20, 7, 100);

				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_DOOM_BOLT:
		{
			dam = get_skill(S_DEVICE, 80, 180);
			dam_to_dice(dam, &dice, &sides, FALSE);

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				if (msg) msg_print("Pure mana streams out from your wand!");
				(void)fire_beam(GF_MANA, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_WAND_WIZARDRY:
		{
			if (info) return ("(damage and effects variable)");
			if (use)
			{
				int i;
				int typ;

				/* Get skill */
				int skill = get_skill(S_DEVICE, 50, 100);

				/* Wands of wizardry do better in skilled hands */
				int choice = randint(skill);

				/* Get direction */
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);

				/* Clone monster a few times */
				if (choice == 1)
				{
					for (i = 0; i < randint(4); i++)
					{
						if (clone_monster(dir)) *ident = TRUE;
					}
				}
				/* Heal monster a lot */
				else if (choice < 4)
				{
					if (heal_monster(dir, damroll(p_ptr->depth, 10)))
						*ident = TRUE;
				}
				/* Haste monster */
				else if (choice < 7)
				{
					if (speed_monster(dir)) *ident = TRUE;
				}
				/* Polymorph monster */
				else if (choice < 8)
				{
					if (poly_monster(dir, get_skill(S_DEVICE, 50, 120)))
						*ident = TRUE;
				}
				/* Sleep monster */
				else if (choice < 11)
				{
					if (sleep_monster(dir, get_skill(S_DEVICE, 30, 100)))
						*ident = TRUE;
				}
				/* Slow monster */
				else if (choice < 15)
				{
					if (slow_monster(dir, get_skill(S_DEVICE, 30, 100)))
						*ident = TRUE;
				}
				/* Confuse monster */
				else if (choice < 18)
				{
					if (confuse_monster(dir, get_skill(S_DEVICE, 30, 100)))
						*ident = TRUE;
				}
				/* Frighten monster */
				else if (choice < 21)
				{
					if (fear_monster(dir, get_skill(S_DEVICE, 30, 100)))
						*ident = TRUE;
				}

				/* Fire any of "wizard"-type projections */
				else
				{
					cptr typ_desc = "magic";

					/* Average 300 damage for a character of perfect skill */
					dam = rand_range(skill * 2, skill * 4);

					/* You pays yer money... */
					choice = rand_int(80);

					/* ... and you takes yer choice */
					if      (choice < 10)
					{
						typ_desc = "fire";
						typ = GF_FIRE;
					}
					else if (choice < 20)
					{
						typ_desc = "cold";
						typ = GF_COLD;
					}
					else if (choice < 30)
					{
						typ_desc = "acid";
						typ = GF_ACID;
					}
					else if (choice < 40)
					{
						typ_desc = "lightning";
						typ = GF_ELEC;
					}
					else if (choice < 45)
					{
						typ_desc = "poison";
						typ = GF_POIS;
					}
					else if (choice < 50)
					{
						typ_desc = "light";
						typ = GF_LITE;
					}
					else if (choice < 55)
					{
						typ_desc = "darkness";
						typ = GF_DARK;
					}
					else if (choice < 60)
					{
						typ_desc = "nexus";
						typ = GF_NEXUS;
					}
					else if (choice < 65)
					{
						typ_desc = "confusion";
						typ = GF_CONFUSION;
					}
					else if (choice < 70)
					{
						typ_desc = "sound";
						typ = GF_SOUND;
					}
					else if (choice < 75)
					{
						typ_desc = "shards";
						typ = GF_SHARD;
					}
					else
					{
						typ_desc = "chaos";
						typ = GF_CHAOS;
					}

					/* Choose and cast a projection */
					choice = rand_int(5);

					if (choice <= 1)
					{
						msg_format("The wand fires a %s bolt.", typ_desc);
						(void)fire_bolt(typ, dir, dam);
					}
					else if (choice == 2)
					{
						msg_format("The wand fires a beam of %s.", typ_desc);
						(void)fire_beam(typ, dir, dam);
					}
					else if (choice == 3)
					{
						msg_format("The wand fires a ball of %s.", typ_desc);
						(void)fire_ball(typ, dir, dam, 1 + dam / 100);
					}
					else
					{
						msg_format("The wand breathes %s.", typ_desc);
						fire_arc(typ, dir, dam, 3 + dam / 30,
							rand_range(40, 80));
					}

					/* Notice */
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_WAND_SPARK:
		{
			dice = 1;    sides = get_skill(S_DEVICE, 6, 24);

			if (info) return (format("(damage: %dd%d)", dice, sides));
			if (use)
			{
				int rad = 3;
				p_ptr->max_dist = rad;
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_AIM_WAND);
				(void)fire_arc(GF_ELEC, dir, damroll(dice, sides), rad, 0);
				*ident = TRUE;
			}
			break;
		}
	}

	/* We used the magical device */
	*used = TRUE;

	/* End of Wands section */
	return ("");



	/*** Handle Rods ***/
	do_rod:

	/* Note action in saved messages */
	if (use) msg_add(format("You zap %s.", o_name));

	/* Display time to recharge */
	(void)strnfmt(buf, sizeof(buf), "every %d turns", k_ptr->pval);

	/* Analyze the rod */
	switch (o_ptr->sval)
	{
		case SV_ROD_DETECT_TRAP:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (detect_traps(FALSE, aware)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (detect_doors(FALSE)) *ident = TRUE;
				if (detect_stairs(FALSE)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				*ident = TRUE;
				if ((uncontrolled) || (!ident_spell())) return ("");
			}
			break;
		}

		case SV_ROD_RECALL:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				recall_player();
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (lite_area(damroll(2, 8), 2)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_MAPPING:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				map_area(0, 0, FALSE);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_PROBING:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (probing()) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_CURING:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (set_blind(0, NULL)) *ident = TRUE;
				if (set_poisoned(0)) *ident = TRUE;
				if (set_diseased(p_ptr->diseased / 2 - 5, NULL)) *ident = TRUE;
				if (set_confused(0)) *ident = TRUE;
				if (set_image(p_ptr->image - 20)) *ident = TRUE;
				if (set_stun(p_ptr->stun - 10)) *ident = TRUE;
				if (set_cut(p_ptr->cut - 10)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_HEALING:
		{
			pow = 50;

			if (info) return (format("(heal about %d%%, cure cuts and stunning) %s", pow, buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (heal_player(pow, pow * 5)) *ident = TRUE;
				if (set_stun(0)) *ident = TRUE;
				if (set_cut(0)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_RESTORATION:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (restore_level()) *ident = TRUE;
				if (restore_stats()) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_SPEED:
		{
			cptr extra = (p_ptr->fast ? " longer" : "");

			dur1 = (p_ptr->fast ?  1 : 20);
			dur2 = (p_ptr->fast ? 10 : 45);

			if (info) return (format("(duration: %d-%d%s) %s", dur1, dur2, extra, buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				if (set_fast(p_ptr->fast + rand_range(dur1, dur2)))
					*ident = TRUE;
			}
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (teleport_monster(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (info) return (format("(chance: 95%%) %s", buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (disarm_trap(dir)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_LITE:
		{
			if (info) return (format("(damage: 4d5) %s", buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				msg_print("A line of golden light appears.");
				lite_line(dir);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_BLINKING:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				sound(MSG_ZAP_ROD);
				teleport_player(10, TRUE, FALSE);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_SUMMON_HITHER:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);

				/* Assume that spell does not succeed */
				p_ptr->came_hither = 0;

				/* Try to teleport a monster to you */
				(void)come_hither(dir);

				/* The spell succeeded and the character isn't blind */
				if ((p_ptr->came_hither) && (!p_ptr->blind))
				{
					char name[DESC_LEN];

					/* Get the monster */
					monster_type *m_ptr = &m_list[p_ptr->came_hither];

					/* Get the monster name ("the kobold" or "something") */
					monster_desc(name, m_ptr, 0x44);

					/* Message -- even when invisible (you sense it) */
					msg_format("You have summoned %s.", name);

					/* Identify */
					*ident = TRUE;

					/* Reset */
					p_ptr->came_hither = 0;
				}
			}
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			dam = get_skill(S_DEVICE, 60, 130);

			if (info) return (format("(damage: %d) %s", dam, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (drain_life(dir, dam)) *ident = TRUE;
			}
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (info) return (format("%s", buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (poly_monster(dir, get_skill(S_DEVICE, 35, 100)))
					*ident = TRUE;
			}
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			dice = get_skill(S_DEVICE, 6, 11);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_bolt_or_beam(10, GF_ACID, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			dice = get_skill(S_DEVICE, 4, 7);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_bolt_or_beam(10, GF_ELEC, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			dice = get_skill(S_DEVICE, 7, 13);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_bolt_or_beam(10, GF_FIRE, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			dice = get_skill(S_DEVICE, 5, 9);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_bolt_or_beam(10, GF_COLD, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			dam = get_skill(S_DEVICE, 50, 150);

			if (info) return (format("(damage: %d) %s", dam, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_ball(GF_ACID, dir, dam, 1);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			dam = get_skill(S_DEVICE, 30, 130);

			if (info) return (format("(damage: %d) %s", dam, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_ball(GF_ELEC, dir, dam, 1);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			dam = get_skill(S_DEVICE, 60, 160);

			if (info) return (format("(damage: %d) %s", dam, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_ball(GF_FIRE, dir, dam, 1);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			dam = get_skill(S_DEVICE, 40, 140);

			if (info) return (format("(damage: %d) %s", dam, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				(void)fire_ball(GF_COLD, dir, dam, 1);
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_LIGHTNINGSTRIKE:
		{
			dice = get_skill(S_DEVICE, 6, 54);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (msg) msg_print("A great spark shoots out from your rod!");

				(void)fire_bolt(GF_ELEC, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_NORTHWINDS:
		{
			dice = get_skill(S_DEVICE, 14, 56);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (msg) msg_print("A massive bolt of frost shoots out from your rod!");

				(void)fire_bolt(GF_COLD, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_DRAGONFIRE:
		{
			dice = get_skill(S_DEVICE, 22, 58);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (msg) msg_print("A blazing bolt shoots out from your rod!");

				(void)fire_bolt(GF_FIRE, dir, damroll(dice, sides));
				*ident = TRUE;
			}
			break;
		}

		case SV_ROD_GLAURUNGS:
		{
			dice = get_skill(S_DEVICE, 30, 60);     sides = 8;

			if (info) return (format("(damage: %dd%d) %s", dice, sides, buf));
			if (use)
			{
				if ((need_dir) && (!get_aim_dir(&dir))) return ("");
				sound(MSG_ZAP_ROD);
				if (msg) msg_print("Black, deadly acid shoots out from your rod!");

				(void)fire_bolt(GF_ACID, dir, damroll(dice, sides));
				*ident = TRUE;
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
 * Learn details about an object
 */
void learn_details(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	int more_info = FALSE;
	int skill;
	int odds = 0;

	char o_name[DESC_LEN];

	/* Require known object */
	if (!object_known_p(o_ptr)) return;

	/* We already know all about it */
	if (k_ptr->special & (SPECIAL_KNOWN_EFFECT)) return;

	/* The character must have his wits about him */
	if (p_ptr->confused || p_ptr->image || p_ptr->berserk) return;

	/* Get chance to learn about the object */
	if (is_magical_device(o_ptr))
	{
		skill = get_skill(S_DEVICE, 0, 15) + get_skill(S_PERCEPTION, 0, 15);

		/* Get odds against learning magical device effects */
		if      (o_ptr->tval == TV_STAFF) odds = 35 - skill;
		else if (o_ptr->tval == TV_WAND)  odds = 40 - skill;
		else if (o_ptr->tval == TV_ROD)   odds = 45 - skill;

		/* There is always something new to learn about devices */
		more_info = TRUE;
	}

	/* Food, potions, or scrolls */
	else if ((o_ptr->tval == TV_FOOD) ||
	         (o_ptr->tval == TV_POTION) ||
	         (o_ptr->tval == TV_SCROLL))
	{
		odds = 30 - get_skill(S_PERCEPTION, 0, 15);

		more_info = strlen(do_object(OBJECT_INFO, o_ptr));
	}
	else return;

	/* Roll for learning */
	if ((odds <= 0) || (one_in_(odds)))
	{
		/* We now know about the object's effects (in theory) */
		k_ptr->special |= (SPECIAL_KNOWN_EFFECT);

		/* If we actually give more information now, let the player know. */
		if (more_info)
		{
			/* No flavors, force plural */
			object_desc_flavour = -1;
			object_desc_plural = 1;

			/* Describe the object */
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

			/* Message */
			msg_format("You feel you know more about %s.", o_name);
		}
	}
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
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return (FALSE);
	}
	return (TRUE);
}

/*
 * Hook to determine if an object is readable
 */
static bool item_tester_hook_scroll(const object_type *o_ptr)
{
	if (o_ptr->tval == TV_SCROLL)
	{
		if (!no_light() || (k_info[o_ptr->k_idx].flags2 & TR2_GLOW_WORDS) )
        {
		      return (TRUE);
        }
     }
     return (FALSE);
}


/*
 * Hook to determine if an object is quaffable.
 */
static bool item_tester_hook_quaff(const object_type *o_ptr)
{
	/* Is potion */
	if (o_ptr->tval == TV_POTION) return (TRUE);

	/* Is food */
	if (o_ptr->tval == TV_FOOD)
	{
		/* Ale, Wine, and Orcish Sprits can be quaffed */
		if (o_ptr->sval == SV_FOOD_PINT_OF_ALE) return (TRUE);
		if (o_ptr->sval == SV_FOOD_PINT_OF_WINE) return (TRUE);
		if (o_ptr->sval == SV_FOOD_ORCISH_FIREWATER) return (TRUE);
	}

	/* Nope. */
	return (FALSE);
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

	/* Note whether we are aware of the effects of this item */
	u16b aware;

	cptr q, s;


	/* Output text based on the object kind */
	if (tval == TV_FOOD)
	{
		/* Restrict choices to food */
		item_tester_tval = TV_FOOD;

		q = "Eat which item?";
		s = "You have nothing to eat.";
	}
	else if (tval == TV_SCROLL)
	{
		/* Check some conditions */
		if (!can_read_scroll()) return;

		/* Restrict choices to scrolls */
		item_tester_hook = item_tester_hook_scroll;

		/* Get an item */
		q = "Read which scroll?";
		s = "You have no scrolls to read.";
	}
	else if (tval == TV_POTION)
	{
		/* Restrict choices to quaffables */
		item_tester_hook = item_tester_hook_quaff;

		/* Get an item */
		q = "Quaff which potion?";
		s = "You have no potions to quaff.";
	}
	else
	{
		msg_print("Unknown object type in \"use_object()\".");
		return;
	}


	/* Get an item */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;
	item_to_object(o_ptr, item);

	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];


	/* Handle some things */
	if (o_ptr->tval == TV_FOOD)
	{
		/* Sound */
		sound(MSG_EAT);
	}
	else if (o_ptr->tval == TV_POTION)
	{
		/* Sound */
		sound(MSG_QUAFF);
	}
	else if (o_ptr->tval == TV_SCROLL)
	{
		/* No sound effect */
	}


	/* Note whether we are aware of the effects of this item */
	aware = object_aware_p(o_ptr);

	/* Assume the item will get used up */
	obj_used_up = TRUE;

	/* Assume no identification */
	obj_ident = FALSE;


	/* Eat the food, quaff the potion, read the scroll, or use the object */
	(void)do_object(OBJECT_USE, o_ptr);

	/* We have tried it */
	object_tried(o_ptr);

	/* Always have some chance to learn about the object */
	if (!obj_ident)
	{
		int chance;

		if (o_ptr->tval == TV_FOOD)
		{
			/* Chance increases with nature lore, and decreases with level */
			chance = 25 - (k_ptr->level / 5) + get_skill(S_NATURE, 0, 100);

			/* Goats are pretty good at identifying food */
			if (p_ptr->schange == SHAPE_GOAT) chance += 25;
		}
		else if (o_ptr->tval == TV_POTION)
		{
			/* Alchemists know their potions */
			chance = get_skill(S_ALCHEMY, 10, 100);
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
		/* Become aware of the object kind */
		object_aware(o_ptr);

		/* Becoming aware of a flavoured object is worth experience */
		if (!aware && k_ptr->flavor)
		{
			int skill;
			char o_name[DESC_LEN];

			/* Describe the usage */
			cptr p = "are using";
			if (o_ptr->tval == TV_POTION) p = "have quaffed";
			if (o_ptr->tval == TV_SCROLL) p = "have read";
			if (o_ptr->tval == TV_FOOD)   p = "have eaten";


			/* Describe only one, unflavoured, object */
			object_desc_plural = -1;
			object_desc_flavour = -1;

			/* Describe the object (briefly) */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);


			/* Choose a skill to practice */
			if (is_magical_device(o_ptr)) skill = S_DEVICE;
			else                          skill = S_NOSKILL;

			/* Gain experience - unsensed (or uncertain) object */
			if (!(o_ptr->ident & (IDENT_SENSE)) ||
			    (o_ptr->inscrip == INSCRIP_UNCERTAIN))
			{
				/* Message */
				if (hack_id_notice_suppress == 0)
				{
					msg_format("You realize that you %s %s.  Your experience rises.", p, o_name);
				}

				/* Experience */
				if (hack_id_notice_suppress < 2)
				{
					gain_exp(MAX(1, k_ptr->level * k_ptr->level / 2), skill);
				}
			}

			/* Gain experience - sensed object */
			else
			{
				/* Message */
				if (hack_id_notice_suppress == 0)
				{
					msg_format("You realize that you %s %s.", p, o_name);
				}

				/* Experience */
				if (hack_id_notice_suppress < 2)
				{
					gain_exp(MAX(1, k_ptr->level / 2), skill);
				}
			}

			/* Learning something always takes time */
			p_ptr->energy_use = 100;
		}

		/* Chance to learn more about (known) items of this type */
		else if (obj_used_up)
		{
			learn_details(o_ptr);
		}
	}


	/* Hack -- allow certain items to be "preserved" */
	if (!obj_used_up) return;


	/* Special message only prints once */
	k_info[o_ptr->k_idx].special |= (SPECIAL_MESSAGE);



	/* Take a turn (unless object was neither used nor identified) */
	p_ptr->energy_use = 100;


	/* Food can feed the player */
	if (tval == TV_FOOD)
	{
		(void)set_food(p_ptr->food + o_ptr->pval);
	}

	/* Potions can usually feed the player */
	else if (tval == TV_POTION)
	{
		if (o_ptr->sval != SV_POTION_GRENADE)
		{
			(void)set_food(p_ptr->food + o_ptr->pval);
		}
	}


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

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


	/* We quaffed a potion, and want to save bottles */
	if ((tval == TV_POTION) && (!p_ptr->suppress_bottle))
	{
		/* Character has some alchemy skill or potion is very high-level */
		if ((get_skill(S_ALCHEMY, 0, 100) >= LEV_REQ_ALCHEMY) ||
		    (k_ptr->level > p_ptr->power / 2 + 30))
		{
			/* Create a bottle, give it to the character or the floor */
			make_specific_tval(TV_BOTTLE, k_ptr->level, (item >= 0));
		}
	}

	/* We read a scroll, and want to save parchments */
	else if ((tval == TV_SCROLL) && (!p_ptr->suppress_bottle))
	{
		/* Character has some alchemy skill or scroll is very high-level */
		if ((get_skill(S_ALCHEMY, 0, 100) >= LEV_REQ_ALCHEMY) ||
			 (k_ptr->level > p_ptr->power / 2 + 30))
		{
			/* Create a parchment, give it to the character or the floor */
			make_specific_tval(TV_PARCHMENT, k_ptr->level, (item >= 0));
		}
	}
}


/*
 * Hook to determine if an object is a magical device.
 */
static bool item_tester_hook_device(const object_type *o_ptr)
{
	/* Must be a staff, wand, or rod */
	if ((o_ptr->tval == TV_STAFF) ||
	    (o_ptr->tval == TV_WAND) ||
	    (o_ptr->tval == TV_ROD))
	{
		return (TRUE);
	}

	/* Disallow anything else */
	return (FALSE);
}


/*
 * Calculate success chance with magical devices and activatable items.
 */
int device_chance(const object_type *o_ptr)
{
	int lev, chance;

	int skill = p_ptr->skill_dev;

	/* Object is an artifact - use artifact level */
	if(artifact_p(o_ptr))
	{
		lev = a_info[o_ptr->artifact_index].level;
	}

	/* Non-artifact dragon scale mail does not require special skills */
	else if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* All characters eventually get perfect usage of DSM */
		skill = div_round(p_ptr->power, 4);

		lev = 0;

	}

	/* Other gear uses item level */
	else
	{
		lev = k_info[o_ptr->k_idx].level;
	}

	/* Items other than devices are relatively easy to activate, as well as some devices */
	if (!is_magical_device(o_ptr) || ((o_ptr->flags2) & TR2_EASY_ACTIVATE))
	{
		if (lev > 10) lev -= ((lev - 10) / 2);
	}

	/*
	 * Determine percentage chance of success.
	 * Is perfect if skill is >= 25 above object level.
	 */
	if (skill < lev) chance = MAX(0, 20 + skill - lev);
	else             chance = 20 + rsqrt((skill - lev) * 256);

	/* Confusion or hallucination makes things harder */
	if ((p_ptr->confused) || (p_ptr->image)) chance /= 2;

	/* Berserk or necromantic rage makes things harder */
	if ((p_ptr->berserk) || (p_ptr->necro_rage)) chance /= 2;

	/* Stunning makes things a little harder */
	if (p_ptr->stun) chance = (100 * chance) / (105 + p_ptr->stun);

	/* Fear makes things a little harder */
	if (p_ptr->afraid) chance -= chance / 5;

	/* Blindness or lack of light makes things a little harder */
	if ((p_ptr->blind) || (no_light() && (p_ptr->see_infra <= 0))) chance -= chance / 4;


	/* Set bounds */
	if (chance < 0) chance = 0;
	if (chance > 100) chance = 100;

	return (chance);
}


/*
 * Use a staff, wand, or rod.
 */
void use_device(int tval)
{
	/* Assume no identification/usage */
	bool ident = FALSE;
	bool used  = FALSE;

	/* For ident purposes, necessary to keep track of all uses  -EFG- */
	bool used_via_failure = FALSE;

	int item;
	int chance;

	object_type *o_ptr;
	object_kind *k_ptr;

	cptr q, s, p;


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
	else if (tval == TV_STAFF)
	{
		q = "Use which staff?";
		s = "You have no staff to use.";
	}
	else if (tval == TV_WAND)
	{
		if (rogue_like_commands)
		{
			q = "Zap which wand?";
			s = "You have no wand to zap.";
		}
		else
		{
			q = "Aim which wand?";
			s = "You have no wand to aim.";
		}
	}
	else if (tval == TV_ROD)
	{
		if (rogue_like_commands)
		{
			q = "Aim which rod?";
			s = "You have no rod to aim.";
		}
		else
		{
			q = "Zap which rod?";
			s = "You have no rod to zap.";
		}
	}
	else
	{
		msg_print("Unknown object type in \"use_device()\".");
		return;
	}


	/* Get an item index */
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Get the actual object */
	item_to_object(o_ptr, item);

	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Get name */
	if      (o_ptr->tval == TV_STAFF) p = "staff";
	else if (o_ptr->tval == TV_WAND)  p = "wand";
	else if (o_ptr->tval == TV_ROD)   p = "rod";
	else                              p = "device";


	/* Notice that staffs or wands are empty - require knowledge */
	if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
	{
		if ((o_ptr->ident & (IDENT_EMPTY)) ||
		    (object_known_p(o_ptr) && (!o_ptr->pval)))
		{
			if (flush_failure) flush();
			msg_format("Your %s has no charges left.", p);
			if (delay_failure) pause_for(250);
			return;
		}
	}

	/* Notice that rods are empty - currently requires no knowledge  -LM- */
	else if (o_ptr->tval == TV_ROD)
	{
		/* A single rod is still charging */
		if ((o_ptr->number == 1) && (o_ptr->timeout))
		{
			if (flush_failure) flush();
			msg_print("The rod is still charging.");
			if (delay_failure) pause_for(250);
			return;
		}

		/* A stack of rods lacks enough energy */
		else if ((o_ptr->number > 1) &&
			(o_ptr->timeout > o_ptr->pval - k_ptr->pval))
		{
			if (flush_failure) flush();
			msg_print("The rods are all still charging.");
			if (delay_failure) pause_for(250);
			return;
		}
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Calculate success chance */
	chance = device_chance(o_ptr);

	/* Roll for failure */
	if (chance <= rand_int(100))
	{
		/* Optional flush on failure */
		if (flush_failure) flush();

		/* Staffs of Doomspells have a mind of their own. */
		if ((o_ptr->tval == TV_STAFF) &&
		    (o_ptr->sval == SV_STAFF_DOOMSPELLS) && (one_in_(10)))
		{
			doomspells(one_in_(2), MAX(50, p_ptr->depth));
			used_via_failure = TRUE;
			ident = TRUE;
		}

		/* Rods of Summon Hither summon regardless */
		else if ((o_ptr->tval == TV_ROD) &&
		         (o_ptr->sval == SV_ROD_SUMMON_HITHER))
		{
			used_via_failure = TRUE;

			if (summon_specific(p_ptr->py, p_ptr->px, FALSE,
				p_ptr->depth + 3, 0, 1))
			{
				if (!p_ptr->blind)
				{
					msg_print("A new opponent appears!");
					ident = TRUE;
				}
			}
		}

		/* Other kinds of magical devices just sit there */
		else
		{
			msg_format("You failed to use the %s properly.", p);
		}

		/* Optional delay */
		if (delay_failure) pause_for(250);

		/* Return -- unless device activates */
		if (!used_via_failure)
		{
			/* Note total lack of chance.  -EFG- */
			if (chance <= 0)
			{
				msg_format("You have no idea how to get the %s to work.", p);
			}
			return;
		}
	}


	/* Notice empty staffs or wands */
	if ((!used_via_failure) &&
	    ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)))
	{
		if (o_ptr->pval <= 0)
		{
			/* Optional flush on failure */
			if (flush_failure) flush();

			/* Staffs of Doomspells have a mind of their own. */
			if ((o_ptr->tval == TV_STAFF) &&
				(o_ptr->sval == SV_STAFF_DOOMSPELLS) && (one_in_(10)))
			{
				doomspells(one_in_(2), MAX(50, p_ptr->depth));
				used_via_failure = TRUE;
				ident = TRUE;
			}

			/* Other devices note that they are empty */
			else
			{
				msg_format("The %s has no charges left.", p);
				o_ptr->ident |= (IDENT_EMPTY);
			}

			/* Optional delay */
			if (delay_failure) pause_for(250);

			/* Return -- unless device activates */
			if (!used_via_failure) return;
		}
		else
		{
			/* Player remembers that device is not empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}

	/* If item was not used through failure */
	if (!used_via_failure)
	{
		/* Practice the device skill */
		skill_being_used = S_DEVICE;

		/* Use device */
		(void)do_device(OBJECT_USE, o_ptr, &ident, &used, FALSE);

		/* Note cancel */
		if (!used)
		{
			/* Do not take a turn  XXX XXX */
			p_ptr->energy_use = 0;
			return;
		}

		/* Special message only prints once */
		k_info[o_ptr->k_idx].special |= (SPECIAL_MESSAGE);
	}


	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident)
	{
		/* Newly learnt object */
		if (!object_aware_p(o_ptr))
		{
			char o_name[DESC_LEN];

			/* Become aware of the object's effects */
			object_aware(o_ptr);

			/* User has a chance of learning charges */
			sense_object(o_ptr, 0, FALSE, FALSE);

			/* Describe only one, unflavoured, object */
			object_desc_plural = -1;
			object_desc_flavour = -1;

			/* Describe the object (briefly) */
			object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);


			/* Gain experience - unsensed (or uncertain) device */
			if (!(o_ptr->ident & (IDENT_SENSE)) ||
			    (o_ptr->inscrip == INSCRIP_UNCERTAIN))
			{
				/* Message */
				msg_format("You realize that you are using %s.  Your experience rises.", o_name);

				gain_exp(MAX(1, k_ptr->level * k_ptr->level / 4), S_NOSKILL);
			}

			/* Gain experience - sensed device */
			else
			{
				gain_exp(MAX(1, k_ptr->level / 4), S_NOSKILL);

				/* Message */
				msg_format("You realize that you are using %s.", o_name);
			}
		}

		/* Chance to learn more about (known) devices of this type */
		else
		{
			learn_details(o_ptr);
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);



	/* Use a staff or wand */
	if ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND))
	{
		/* Use charge (if available) */
		if (o_ptr->pval > 0) o_ptr->pval--;

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

	/* Use a rod */
	else if (o_ptr->tval == TV_ROD)
	{
		/* Increase the timeout by the rod kind's pval. */
		o_ptr->timeout += k_ptr->pval;
	}
}





/*
 * Hook to determine if an object is activatable.
 */
static bool item_tester_hook_activate(const object_type *o_ptr)
{
	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Cursed non-artifact items can't be activated */
	if (cursed_p(o_ptr) && !artifact_p(o_ptr)) return (FALSE);

	/* Must have an activation. */
	if (o_ptr->activate) return (TRUE);

	/* Nope. */
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
		{
			/* Message */
			msg_print("You are surrounded by a powerful aura!");

			/* Dispel monsters */
			dispel_monsters(500);

			break;
		}

		case 2:
		case 3:
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
 * Handle activations:  effects, information display.
 */
cptr do_activation_aux(int mode, object_type *o_ptr)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	cptr melee_desc = (p_ptr->barehand ? "hands" : "weapons");
	cptr extra = "";

	TARGET_DECLARE

	/* Modes of operation */
	bool info = (mode == OBJECT_INFO);
	bool act =  (mode == OBJECT_USE);

	int i, dir, chance;

	int dam, dam1, dam2;
	int dur, dur1, dur2;
	int dice, sides;
	int pow;
	int timeout = 0;
	int timeout1 = 0;
	int timeout2 = 0;


	/* No activation */
	if (!o_ptr->activate) return ("");

	/* Process activation */
	switch (o_ptr->activate)
	{
		case ACTIV_GALADRIEL:
		{
			dice = 4;     sides = p_ptr->power / 4;
			timeout1 = 10;     timeout2 = 20;

			if (info) return (format("illumination, burst of light (%dd%d damage), and some curing every %d-%d turns",
				dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("The phial wells with clear light...");

				/* Light up the room */
				lite_room(py, px);

				/* Burst of light */
				fire_star(GF_LITE_EXTRA, 0, damroll(dice, sides), 4);

				/* Cure moderate fear, give a little boldness */
				if (p_ptr->afraid <= 30)
				{
					set_afraid(0);
					set_bold(15);
				}

				/* Cure minor blindness */
				if (p_ptr->blind <= 15) set_blind(0, "The veil of darkness lifts.");
			}
			break;
		}

		case ACTIV_ELENDIL:
		{
			timeout1 = 30;     timeout2 = 60;

			if (info) return (format("magic mapping every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("The star shines brightly...");

				map_area(0, 0, FALSE);
			}
			break;
		}

		case ACTIV_THRAIN:
		{
			timeout1 = 120;     timeout2 = 240;

			if (info) return (format("clairvoyance every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("The stone glows a deep green...");

				wiz_lite(FALSE, TRUE);
				(void)detect_traps(TRUE, TRUE);
				(void)detect_doors(TRUE);
				(void)detect_stairs(TRUE);
			}
			break;
		}

		case ACTIV_PALANTIR:
		{
			if (info) return ("full clairvoyance - at grave risk - every 175-350 turns");
			if (act)
			{
				timeout = stare_into_the_palantir();
			}
			break;
		}

		case ACTIV_CARLAMMAS:
		{
			timeout1 = 225;     timeout2 = 450;
			dur1 = 50;       dur2 = 100;

			if (info) return (format("banishment and protection from evil (duration %d-%d) every %d-%d turns", dur1, dur2, timeout1, timeout2));
			if (act)
			{
				msg_print("The amulet lets out a shrill wail...");

				if (banishment(RF3_EVIL, 60))
					msg_print("You thrust your evil enemies back!");

				(void)set_protevil(p_ptr->protevil + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_INGWE:
		{
			timeout1 = 200;     timeout2 = 300;
			dam = MAX(20, p_ptr->power * 2);

			if (info) return (format("dispel evil (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				msg_print("An aura of good floods the area...");

				dispel_evil(dam);
			}
			break;
		}

		case ACTIV_TULKAS:
		{
			timeout1 = 150;     timeout2 = 300;
			dur1 = (p_ptr->fast ?  1 :  75);
			dur2 = (p_ptr->fast ? 20 : 150);
			if (p_ptr->fast) extra = " extra turns";

			if (info) return (format("haste self (duration %d-%d%s) every %d-%d turns", dur1, dur2, extra, timeout1, timeout2));
			if (act)
			{
				msg_print("The ring glows brightly...");

				(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_NARYA:
		{
			timeout1 = 125;     timeout2 = 150;
			dam = 225;

			if (info) return (format("large fire ball (%d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				msg_print("The ring glows deep red...");

				if (!get_aim_dir(&dir)) return ("");
				fire_ball(GF_FIRE, dir, dam, 3);
			}
			break;
		}

		case ACTIV_NENYA:
		{
			timeout1 = 125;     timeout2 = 150;
			dam = 250;

			if (info) return (format("large frost ball (%d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				msg_print("The ring glows bright white...");

				if (!get_aim_dir(&dir)) return ("");
				fire_ball(GF_COLD, dir, dam, 3);
			}
			break;
		}


		case ACTIV_VILYA:
		{
			timeout1 = 125;     timeout2 = 150;
			dam = 275;

			if (info) return (format("large electricity ball (%d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				msg_print("The ring glows deep blue...");

				if (!get_aim_dir(&dir)) return ("");
				fire_ball(GF_ELEC, dir, dam, 3);
			}
			break;
		}

		case ACTIV_POWER:
		{
			timeout1 = 250;     timeout2 = 500;

			if (info) return (format("bizarre things every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("The ring glows intensely black...");

				if (!get_aim_dir(&dir)) return ("");
				ring_of_power(dir);
			}
			break;
		}

		case ACTIV_RAZORBACK:
		{
			timeout = 150;
			dam = 120;

			if (info) return (format("star ball (%d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("You are surrounded by lightning!");

				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], dam, 3);
			}
			break;
		}

		case ACTIV_BLADETURNER:
		{
			timeout = 400;
			dur1 = 50;     dur2 = 100;

			if (info) return (format("heroism, bless, and resistance (duration %d-%d turns) every %d turns", dur1, dur2, timeout));
			if (act)
			{
				int tmp;

				msg_print("Your armor glows many colours...");

				(void)set_hero(p_ptr->hero + rand_range(dur1, dur2));
				(void)set_blessed(p_ptr->blessed + rand_range(dur1, dur2), NULL);

				tmp = rand_range(dur1, dur2);
				(void)set_oppose_acid(p_ptr->oppose_acid + tmp);
				(void)set_oppose_elec(p_ptr->oppose_elec + tmp);
				(void)set_oppose_fire(p_ptr->oppose_fire + tmp);
				(void)set_oppose_cold(p_ptr->oppose_cold + tmp);
				(void)set_oppose_pois(p_ptr->oppose_pois + tmp);
			}
			break;
		}

		case ACTIV_SOULKEEPER:
		{
			timeout = 400;
			pow = 1000;

			if (info) return (format("heal (%d) every %d turns", pow, timeout));
			if (act)
			{
				msg_print("Your armor glows a bright white...");

				(void)hp_player(pow);
				(void)set_cut(0);
			}
			break;
		}

		case ACTIV_BELEGENNON:
		{
			timeout = 2;

			if (info) return (format("phase door every %d turns", timeout));
			if (act)
			{
				msg_print("Your armor twists space around you...");

				teleport_player(10, TRUE, FALSE);
			}
			break;
		}

		case ACTIV_CELEBORN:
		{
			timeout = 500;

			if (info) return (format("genocide every %d turns", timeout));
			if (act)
			{
				msg_print("Your armor glows deep blue...");

				if (!genocide(0))
				{
					/* Only use object up if genocide goes off */
					obj_used_up = FALSE;
				}
			}
			break;
		}

		case ACTIV_CASPANION:
		{
			if (info) return ("door destruction every turn");
			if (act)
			{
				msg_print("Your armor glows bright red...");

				destroy_doors_touch();
			}
			break;
		}

		case ACTIV_HIMRING:
		{
			dur1  = 35;      dur2  = 55;
			timeout1 = 200;     timeout2 = 400;

			if (info) return (format("protection from evil (duration %d-%d) every %d-%d turns", dur1, dur2, timeout1, timeout2));
			if (act)
			{
				msg_print("The spirit of Himring surrounds you...");
				(void)set_protevil(p_ptr->protevil + rand_range(dur1, dur2));
			}
			break;
		}
		case ACTIV_ELEMENTS:
		{
			dur1 = 20;
			dur2 = 40;

			timeout = 160;

			if (info) return (format("opposition to the elements (duration %d-%d) every %d turns", dur1, dur2, timeout));
			if (act)
			{
				dur = rand_range(20, 40);

				msg_print("Your shield glows many colours...");
				(void)set_oppose_acid(p_ptr->oppose_acid + dur);
				(void)set_oppose_elec(p_ptr->oppose_elec + dur);
				(void)set_oppose_fire(p_ptr->oppose_fire + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			}
			break;
		}
		case ACTIV_EARENDIL:
		{
			timeout = 100;

			if (info) return (format("curing every %d turns", timeout));
			if (act)
			{
				(void)set_poisoned(0);
				(void)set_diseased(p_ptr->diseased / 2 - 10, NULL);
				(void)set_confused(0);
				(void)set_image(0);
				(void)set_blind(0, NULL);
				(void)set_stun(0);
				(void)set_cut(0);
			}
			break;
		}

		case ACTIV_GIL_GALAD:
		{
			timeout = 50;
			dam = 75;

			if (info) return (format("blinding light (%d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("Your shield gleams with blinding light...");

				fire_ball_special(GF_LITE, 0, dam, 6, 0L, 20);
				confu_monsters(80);
			}
			break;
		}

		case ACTIV_HOLHENNETH:
		{
			timeout1 = 55;     timeout2 = 110;

			if (info) return (format("detection every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("An image forms in your mind...");

				(void)detect_all(TRUE, TRUE);
			}
			break;
		}

		case ACTIV_GONDOR:
		{
			timeout = 500;
			pow = 500;

			if (info) return (format("heal (%d) every %d turns", pow, timeout));
			if (act)
			{
				msg_print("You feel a warm tingling inside...");

				(void)hp_player(pow);
				(void)set_cut(0);
			}
			break;
		}

		case ACTIV_COLLUIN:
		{
			timeout = 160;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("resistance (duration %d-%d) every %d turns", dur1, dur2, timeout));
			if (act)
			{
				dur = rand_range(dur1, dur2);

				msg_print("Your cloak glows many colours...");

				(void)set_oppose_acid(p_ptr->oppose_acid + dur);
				(void)set_oppose_elec(p_ptr->oppose_elec + dur);
				(void)set_oppose_fire(p_ptr->oppose_fire + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur);
				(void)set_oppose_pois(p_ptr->oppose_pois + dur);
			}
			break;
		}

		case ACTIV_HOLCOLLETH:
		{
			timeout = 55;

			if (info) return (format("put adjacent monsters to sleep every %d turns", timeout));
			if (act)
			{
				msg_print("Your cloak glows deep blue...");

				sleep_monsters_touch(50 + p_ptr->power);
			}
			break;
		}

		case ACTIV_THINGOL:
		{
			timeout = 70;

			if (info) return (format("recharge magical device every %d turns", timeout));
			if (act)
			{
				msg_print("Your cloak glows bright yellow...");

				if (!recharge(170, FALSE)) timeout = 0;
			}
			break;
		}

		case ACTIV_COLANNON:
		{
			timeout = 45;

			if (info) return (format("teleport (100) every %d turns", timeout));
			if (act)
			{
				msg_print("Your cloak twists space around you...");

				teleport_player(100, TRUE, FALSE);
			}
			break;
		}

		case ACTIV_LUTHIEN:
		{
			timeout = 450;

			if (info) return (format("restore skills every %d turns", timeout));
			if (act)
			{
				msg_print("Your cloak glows a deep red...");

				restore_level();
			}
			break;
		}

		case ACTIV_CAMBELEG:
		{
			if (info) return ("door destruction every turn");
			if (act)
			{
				msg_print("Your gloves glow red...");

				destroy_doors_touch();
			}
			break;
		}

		case ACTIV_CAMMITHRIM:
		{
			timeout = 4;
			dice = 5;     sides = (2 * p_ptr->power / 5);
			if (p_ptr->cur_lite == 4) dice = 8;
			if (p_ptr->cur_lite >= 5) dice = 10;

			if (info) return (format("bolt of light (%dd%d) every %d turns", dice, sides, timeout));
			if (act)
			{
				msg_print("Your gloves glow extremely brightly...");

				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_LITE, dir, damroll(dice, sides));
			}
			break;
		}

		case ACTIV_PAURLHACH:
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			dice  = a_ptr->set_bonus ? 10 : 5;     sides = 20;
			timeout1 =  5;     timeout2 = 10;

			if (info) return (format("lance of fire (damage %dd%d) every %d-%d turns.", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("Your gauntlets are covered in fire...");

				if (!get_aim_dir(&dir)) return ("");
				fire_arc(GF_FIRE, dir, damroll(dice, sides), 4, 0);
			}
			break;
		}

		case ACTIV_PAURNIMMEN:
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			dice  = a_ptr->set_bonus ? 10 : 5;     sides = 20;
			timeout1 =  5;     timeout2 = 10;

			if (info) return (format("lance of cold (damage %dd%d) every %d-%d turns.", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("Your gauntlets are covered in frost...");

				if (!get_aim_dir(&dir)) return ("");
				fire_arc(GF_COLD, dir, damroll(dice, sides), 4, 0);
			}
			break;
		}

		case ACTIV_PAURAEGEN:
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			dice  = a_ptr->set_bonus ? 10 : 5;     sides = 20;
			timeout1 =  5;     timeout2 = 10;

			if (info) return (format("spark of electricity (damage %dd%d) every %d-%d turns.", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("Your gauntlets are covered in lightning...");

				if (!get_aim_dir(&dir)) return ("");
				fire_arc(GF_ELEC, dir, damroll(dice, sides), 4, 0);
			}
			break;
		}

		case ACTIV_PAURNEN:
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			dice  = a_ptr->set_bonus ? 10 : 5;     sides = 20;
			timeout1 =  5;     timeout2 = 10;

			if (info) return (format("lance of acid (damage %dd%d) every %d-%d turns.", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("Your gauntlets are covered in acid...");

				if (!get_aim_dir(&dir)) return ("");
				fire_arc(GF_ACID, dir, damroll(dice, sides), 4, 0);
			}
			break;
		}

		case ACTIV_FINGOLFIN:
		{
			timeout1 = 100;     timeout2 = 200;

			if (info) return (format("vorpal blows every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Your cesti gleam brightly ...");
				msg_format("and your %s brighter still!", melee_desc);

				p_ptr->special_attack |= (ATTACK_VORPAL);
			}
			break;
		}

		case ACTIV_FEANOR:
		{
			timeout = 200;
			dur1 = (p_ptr->fast ?  1 : 20);
			dur2 = (p_ptr->fast ?  5 : 40);
			if (p_ptr->fast) extra = " extra turns";

			if (info) return (format("haste self (duration %d-%d%s) every %d turns", dur1, dur2, extra, timeout));
			if (act)
			{
				msg_print("Your boots glow bright green...");

				(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_DAL:
		{
			timeout = 5;

			if (info) return (format("remove fear, cast boldness, and cure poison every %d turns", timeout));
			if (act)
			{
				msg_print("You feel energy flow through your feet...");

				(void)set_bold(rand_range(10, 15));
				(void)set_poisoned(0);
			}
			break;
		}

		case ACTIV_GIMLI:
		{
			timeout1 = 35;     timeout2 = 70;

			if (info) return (format("dungeon mapping every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You understand the structure of the dungeon around you.");

				map_area(0, 0, FALSE);
			}
			break;
		}

		case ACTIV_RILIA:
		{
			dur = 30;
			timeout1 = 100;     timeout2 = 150;

			if (info) return (format("invisibility (duration %d) every %d-%d turns", dur, timeout1, timeout2));
			if (act)
			{
				msg_print("Your dagger pulses...");

				(void)set_invis(p_ptr->tim_invis + dur,
				                get_skill(S_STEALTH, 30, 40));
			}
			break;
		}

		case ACTIV_BELANGIL:
		{
			timeout1 = 5;     timeout2 = 10;
			dam = 60;

			if (info) return (format("ball of frost (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				msg_print("Your dagger is covered in frost...");

				if (!get_aim_dir(&dir)) return ("");
				fire_ball(GF_COLD, dir, dam, 2);
			}
			break;
		}

		case ACTIV_ARUNRUTH:
		{
			timeout = 20;

			if (info) return (format("panic demons every %d turns", timeout));
			if (act)
			{
				msg_print("Your sword flashes...");

				(void)fear_demons(80);
			}
			break;
		}

		case ACTIV_RINGIL:
		{
			timeout = 30;

			if (info) return (format("explosion of light every %d turns", timeout));
			if (act)
			{
				msg_print("Your sword glows an intense blue-white...");

				/* Strong starlight */
				beam_burst(py, px, GF_LITE, 25, 50);
			}
			break;
		}

		case ACTIV_ANDURIL:
		{
			timeout = 160;
			dur1 = (p_ptr->hero ?  1 : 30);
			dur2 = (p_ptr->hero ? 15 : 50);
			if (p_ptr->hero) extra = " extra turns";

			if (info) return (format("heroism (duration %d-%d%s) every %d turns", dur1, dur2, extra, timeout));
			if (act)
			{
				msg_print("Your sword glows brightly, and you feel yourself grow braver...");

				set_hero(p_ptr->hero + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_DOOMCALLER:
		{
			timeout = 250;
			dur1 = 40;
			dur2 = 60;

			if (info) return (format("go berserk (duration %d-%d) every %d turns", dur1, dur2, timeout));
			if (act)
			{
				msg_print("Your sword flashes...");

				set_berserk(BERSERK_WEAKNESS_LENGTH + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_AR_PHARAZON:
		{
			timeout = 17;
			dam = 77;

			if (info) return (format("dispel animals (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("Your blade gleams...");
				dispel_animals(dam);
			}
			break;
		}


		case ACTIV_THEODEN:
		{
			timeout = 10;
			dam = 120;

			if (info) return (format("drain life (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("The blade of your axe glows black...");

				if (!get_aim_dir(&dir)) return ("");
				drain_life(dir, dam);
			}
			break;
		}

		case ACTIV_AEGLOS:
		{
			timeout = 40;
			dam = 125;

			if (info) return (format("dispel evil (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("A wave of goodness washes over you...");

				(void)dispel_evil(100);

				if (p_ptr->realm == NECRO)
				{
					(void)take_hit(randint(100), 0, "Your black soul is hit!",
						"a self-inflicted dispel evil spell");
				}
			}
			break;
		}

		case ACTIV_OROME:
		{
			timeout = 5;
			dam1 = 50;     dam2 = 100;

			if (info) return (format("stone to mud (damage %d-%d) every %d turns", dam1, dam2, timeout));
			if (act)
			{
				msg_print("Your spear pulsates...");

				if (!get_aim_dir(&dir)) return ("");
				wall_to_mud(dir, rand_range(dam1, dam2));
			}
			break;
		}

		case ACTIV_EONWE:
		{
			timeout = 1000;

			if (info) return (format("mass genocide every %d turns", timeout));
			if (act)
			{
				msg_print("Your axe lets out a long, shrill note...");

				(void)mass_genocide(py, px);
			}
			break;
		}

		case ACTIV_LOTHARANG:
		{
			dice = 4;     sides = 4;

			if (info) return (format("heal (%dd%d), cure wounds, stunning, poison, and disease every turn", dice, sides));
			if (act)
			{
				msg_print("Your battle axe radiates deep purple...");

				hp_player(damroll(dice, sides));
				(void)set_cut((p_ptr->cut / 2) - 10);
				(void)set_stun((p_ptr->stun / 2) - 10);
				(void)set_poisoned(p_ptr->poisoned - 20);
				(void)set_diseased(p_ptr->diseased - 4, NULL);
			}
			break;
		}

		case ACTIV_ULMO:
		{
			timeout = 15;

			if (info) return (format("teleport away every %d turns", timeout));
			if (act)
			{
				msg_print("Your trident glows deep red...");

				if (!get_aim_dir(&dir)) return ("");
				teleport_monster(dir);
			}
			break;
		}

		case ACTIV_AVAVIR:
		{
			timeout = 200;

			if (info) return (format("word of recall every %d turns", timeout));
			if (act)
			{
				msg_print("Your scythe glows soft white...");

				recall_player();
			}
			break;
		}

		case ACTIV_MELKOR:
		{
			timeout = 50;
			dam = 220;

			if (info) return (format("lance of darkness (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("Your spear is covered in darkness...");
				if (!get_aim_dir(&dir)) return ("");
				fire_arc(GF_DARK, dir, dam, 7, 0);
			}
			break;
		}

		case ACTIV_SLAUGHTERFIELD:
		{
			/* Calculate damage */
			s32b damage = o_ptr->dd * (o_ptr->ds + 1) / 2;
			apply_deadliness(&damage, p_ptr->to_d + o_ptr->to_d);
			damage *= get_skill(S_THROWING, 20, 50);
			damage /= 1000;

			if (info) return (format("slaughter the weak (damage about %ld) every turn", damage));
			if (act)
			{
				object_type object_type_body;

				/* Get local object */
				object_type *i_ptr = &object_type_body;

				/* Copy the object */
				object_copy(i_ptr, o_ptr);

				/* (Copied) axe goes a-hunting */
				slaughterfield((int)Rand_normal(damage, damage / 6), i_ptr);

				/* Because the axe will be removed, we must stop here XXX XXX */
				return ("");
			}
			break;
		}

		case ACTIV_TOTILA:
		{
			timeout = 15;

			if (info) return (format("confuse monster every %d turns", timeout));
			if (act)
			{
				msg_print("Your flail glows in scintillating colours...");

				if (!get_aim_dir(&dir)) return ("");
				confuse_monster(dir, 40 + p_ptr->power / 2);
			}
			break;
		}

		case ACTIV_FIRESTAR:
		{
			timeout = 50;
			dam = 70;

			if (info) return (format("large fire ball (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("Your morning star rages in fire...");

				if (!get_aim_dir(&dir)) return ("");
				fire_ball(GF_FIRE, dir, dam, 3);
			}
			break;
		}

		case ACTIV_TARATOL:
		{
			timeout1 = 100;   timeout2 = 200;
			dur1 = (p_ptr->fast ? 1 : 20);
			dur2 = (p_ptr->fast ? 5 : 40);
			if (p_ptr->fast) extra = " extra turns";

			if (info) return (format("haste self (duration %d-%d%s) every %d-%d turns", dur1, dur2, extra, timeout1, timeout2));
			if (act)
			{
				msg_print("Your mace glows bright green...");

				(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_ERIRIL:
		{
			timeout = 10;

			if (info) return (format("identify every %d turns", timeout));
			if (act)
			{
				msg_print("Your quarterstaff glows yellow...");

				if (!ident_spell()) return ("");
			}
			break;
		}

		case ACTIV_OLORIN:
		{
			timeout = 30;

			if (info) return (format("probing every %d turns", timeout));
			if (act)
			{
				msg_print("Your quarterstaff glows brightly...");

				(void)probing();
			}
			break;
		}

		case ACTIV_TURMIL:
		{
			timeout = 40;
			dice = 10;     sides = 19;

			if (info) return (format("holy orb (damage %dd%d) every %d turns", dice, sides, timeout));
			if (act)
			{
				msg_print("Your mace glows with the holy spirit...");

				if (!get_aim_dir(&dir)) return ("");
				fire_ball(GF_HOLY_ORB, dir, damroll(dice, sides), 1);
			}
			break;
		}

		case ACTIV_SKULLCLEAVER:
		{
			timeout = 150;

			if (info) return (format("smash dungeon every %d turns", timeout));
			if (act)
			{
				msg_print("You strike your mace against the ground...");

				destroy_area(py, px, 15, TRUE, FALSE);
				(void)take_hit(damroll(5, 10), 0, "It hurts!", "activating Skullcleaver");
			}
			break;
		}

		case ACTIV_HARAD:
		{
			timeout1 = 30;     timeout2 = 60;

			if (info) return (format("a deadly shot every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("The bolt you have ready to hand gleams with deadly power.");

				p_ptr->special_attack |= (ATTACK_DEADLY);
			}
			break;
		}

		case ACTIV_BARD:
		{
			timeout1 = 200;     timeout2 = 400;

			if (info) return (format("a dragon-slaying shot every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Your longbow glitters faintly...");

				p_ptr->special_attack |= (ATTACK_BARD);
			}
			break;
		}

		case ACTIV_BUCKLAND:
		{
			timeout = 1000;

			if (info) return (format("brand shots every %d turns", timeout));
			if (act)
			{
				msg_print("Your sling glows with power...");

				(void)brand_missile(TV_SHOT, 0);
			}
			break;
		}

		case ACTIV_EREBOR:
		{
			timeout = 25;

			if (info) return (format("travel instantly from one side of a wall to the other every %d turns", timeout));
			if (act)
			{
				msg_print("Your pick twists in your hands.");

				if (!get_aim_dir(&dir)) return ("");
				(void)passwall(dir, TRUE);
			}
			break;
		}


		/* Activations for random artifacts, and available for use elsewhere. */
		case ACTIV_RANDOM_FIRE1:
		{
			timeout1 = 7;     timeout2 = 14;
			dice = 4 + p_ptr->power / 4;     sides = 8;

			if (info) return (format("fire bolt (damage %dd%d) every %d-%d turns", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("You launch a bolt of fire.");

				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_FIRE, dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_FIRE2:
		{
			timeout = 110;
			dam = 40 + p_ptr->power;

			if (info) return (format("orb of fire (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("You feel an orb of fire form between your hands.");

				if (!get_aim_dir(&dir)) return ("");
				fire_orb(GF_FIRE, dir, dam, 1);
			}
			break;
		}
		case ACTIV_RANDOM_FIRE3:
		{
			timeout = 150;
			dam = 2 * p_ptr->power + 30;

			if (info) return (format("fire storm (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("The fires of Anor rise in wrath!");

				fire_star(GF_FIRE, 0, dam, 6);
			}
			break;
		}
		case ACTIV_RANDOM_COLD1:
		{
			timeout1 = 7;     timeout2 = 14;
			dice = 4 + p_ptr->power / 4;     sides = 8;

			if (info) return (format("cold bolt (damage %dd%d) every %d-%d turns", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("You launch a bolt of frost.");

				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_COLD, dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_COLD2:
		{
			timeout = 110;
			dam = 40 + p_ptr->power;

			if (info) return (format("orb of frost (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("You hurl an orb of killing frost.");

				if (!get_aim_dir(&dir)) return ("");
				fire_orb(GF_COLD, dir, dam, 1);
			}
			break;
		}
		case ACTIV_RANDOM_COLD3:
		{
			timeout = 150;
			dam = 2 * p_ptr->power + 30;

			if (info) return (format("frost storm (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("A wild Northland frost storms uncontrollably!");

				fire_star(GF_COLD, 0, dam, 6);
			}
			break;
		}
		case ACTIV_RANDOM_ACID1:
		{
			timeout1 = 7;     timeout2 = 14;
			dice = 3 + p_ptr->power / 4;     sides = 8;

			if (info) return (format("acid bolt (damage %dd%d) every %d-%d turns", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("You launch a bolt of acid.");

				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_ACID, dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_ACID2:
		{
			timeout = 110;
			dam = 30 + p_ptr->power;

			if (info) return (format("orb of acid (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("An orb of deadly acid forms upon your hand.");

				if (!get_aim_dir(&dir)) return ("");
				fire_orb(GF_ACID, dir, dam, 1);
			}
			break;
		}
		case ACTIV_RANDOM_ACID3:
		{
			timeout = 150;
			dam = 2 * p_ptr->power;

			if (info) return (format("acid storm (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("A tornado of acid melts armor and flesh!");

				fire_ball_special(GF_ACID, 0, dam, 3, 0L, 20);
			}
			break;
		}
		case ACTIV_RANDOM_ELEC1:
		{
			timeout1 = 7;     timeout2 = 14;
			dice = 3 + p_ptr->power / 4;     sides = 8;

			if (info) return (format("lightning bolt (damage %dd%d) every %d-%d turns", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("You launch a bolt of lightning.");

				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_ELEC, dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_ELEC2:
		{
			timeout = 110;
			dam = 30 + p_ptr->power;

			if (info) return (format("orb of lightning (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("You summon ball lightning to your aid.");

				if (!get_aim_dir(&dir)) return ("");
				fire_orb(GF_ELEC, dir, dam, 1);
			}
			break;
		}
		case ACTIV_RANDOM_ELEC3:
		{
			timeout = 150;
			dam1 = 3 * p_ptr->power / 2;
			dam2 = p_ptr->power / 2;

			if (info) return (format("lightning strike (damage %d + %d) every %d turns", dam1, dam2, timeout));
			if (act)
			{
				msg_print("A massive stroke of lightning smites the ground!");
				fire_ball_special(GF_ELEC, 0, dam1, 2, 0L, 20);
				msg_print("Boom!");
				fire_ball_special(GF_SOUND, 0, dam2, 9, 0L, 20);
			}
			break;
		}
		case ACTIV_RANDOM_POIS1:
		{
			timeout1 = 8;     timeout2 = 16;
			dice = 4 + p_ptr->power / 4;     sides = 8;

			if (info) return (format("poison dart (damage %dd%d) every %d-%d turns", dice, sides, timeout1, timeout2));
			if (act)
			{
				msg_print("You launch a poison dart.");

				if (!get_aim_dir(&dir)) return ("");
				fire_bolt(GF_POIS, dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_POIS2:
		{
			timeout = 120;
			dam = 80 + 4 * p_ptr->power / 5;

			if (info) return (format("poison cloud (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("Deadly gases blanket the area.");

				fire_star(GF_POIS, 0, dam, 8);
			}
			break;
		}
		case ACTIV_RANDOM_LIGHT1:
		{
			timeout = 150;
			dam1 = 40 + p_ptr->power / 2;     dam2 = 10 + p_ptr->power / 10;

			if (info) return (format("blinding ball of light (damage %d + %d) every %d turns", dam1, dam2, timeout));
			if (act)
			{
				msg_print("You throw a radiant sphere...");
				if (!get_aim_dir(&dir)) return ("");
				TARGET_PRESERVE
				fire_ball(GF_LITE, dir, dam1, 0);
				TARGET_RESTORE
				fire_ball(GF_CONFUSION, dir, dam2, 0);
			}
			break;
		}
		case ACTIV_RANDOM_LIGHT2:
		{
			timeout = 150;
			dam = 110 + p_ptr->power;

			if (info) return (format("dispel light-hating (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("You bathe the area in radiant light!");
				(void)dispel_light_hating(dam);
			}
			break;
		}
		case ACTIV_RANDOM_DISPEL_UNDEAD:
		{
			timeout = 80;
			dam = 60 + 4 * p_ptr->power / 5;

			if (info) return (format("dispel undead (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("A tide of life surrounds you!");
				(void)dispel_undead(dam);
			}
			break;
		}
		case ACTIV_RANDOM_DISPEL_EVIL:
		{
			timeout = 80;
			dam = 60 + 3 * p_ptr->power / 5;

			if (info) return (format("dispel evil (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("A wave of holiness washes over you...");
				(void)dispel_evil(dam);

				if (p_ptr->realm == NECRO)
				{
					(void)take_hit(25, 0, "Your black soul is hit!", "struck down by Good");
				}
			}
			break;
		}
		case ACTIV_RANDOM_SMITE_UNDEAD:
		{
			timeout = 50;
			dice = MAX(1, p_ptr->power / 7);     sides = 33;

			if (info) return (format("dispel an undead (damage %dd%d) every %d turns", dice, sides, timeout));
			if (act)
			{
				msg_print("Spells to dispel an undead antagonist surround you...");
				if (!get_aim_dir(&dir)) return ("");
				dispel_an_undead(dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_SMITE_DEMON:
		{
			timeout = 50;
			dice = MAX(1, p_ptr->power / 7);     sides = 33;

			if (info) return (format("dispel a demon (damage %dd%d) every %d turns", dice, sides, timeout));
			if (act)
			{
				msg_print("Spells to dispel a demonic adversary surround you...");
				if (!get_aim_dir(&dir)) return ("");
				dispel_a_demon(dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_SMITE_DRAGON:
		{
			timeout = 50;
			dice = MAX(1, p_ptr->power / 7);     sides = 33;

			if (info) return (format("dispel a dragon (damage %dd%d) every %d turns", dice, sides, timeout));
			if (act)
			{
				msg_print("Spells to dispel a draconic foe surround you...");
				if (!get_aim_dir(&dir)) return ("");
				dispel_a_dragon(dir, damroll(dice, sides));
			}
			break;
		}
		case ACTIV_RANDOM_HOLY_ORB:
		{
			timeout = 50;
			dam = 40 + 3 * p_ptr->power / 5;

			if (info) return (format("holy orb (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("A holy orb materializes on your fingertips.");
				if (!get_aim_dir(&dir)) return ("");
				fire_orb(GF_HOLY_ORB, dir, dam, 1);
			}
			break;
		}
		case ACTIV_RANDOM_BLESS:
		{
			timeout = 120;
			dur1 = (p_ptr->blessed ?  1 : 24);
			dur2 = (p_ptr->blessed ? 24 : 48);
			if (p_ptr->blessed) extra = " extra turns";

			if (info) return (format("blessing (duration %d-%d%s) every %d turns", dur1, dur2, extra, timeout));
			if (act)
			{
				(void)set_blessed(p_ptr->blessed + rand_range(dur1, dur2),
					"You feel blessed for battle.");
			}
			break;
		}
		case ACTIV_RANDOM_FRIGHTEN_ALL:
		{
			timeout1 = 80;     timeout2 = 160;

			if (info) return (format("panic monsters every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You reveal yourself in wrath...");

				if (fear_monsters(10 + 4 * p_ptr->power / 5))
				{
					msg_print("Your enemies tremble!");
				}
			}
			break;
		}
		case ACTIV_RANDOM_HEAL1:
		{
			timeout = 45;
			pow = 10;

			if (info) return (format("healing (about %d%%) every %d turns", pow, timeout));
			if (act)
			{
				(void)heal_player(pow, pow * 2);
				(void)set_stun(p_ptr->stun - 15);
				(void)set_cut(p_ptr->cut - 5);
				(void)set_poisoned(p_ptr->poisoned - 5);
			}
			break;
		}
		case ACTIV_RANDOM_HEAL2:
		{
			timeout = 55;
			pow = 30;

			if (info) return (format("healing (about %d%%) every %d turns", pow, timeout));
			if (act)
			{
				(void)heal_player(pow, pow * 3);
				(void)set_stun((p_ptr->stun / 2) - 15);
				(void)set_cut((p_ptr->cut / 2) - 5);
				(void)set_poisoned((p_ptr->poisoned / 2) - 5);
			}
			break;
		}
		case ACTIV_RANDOM_HEAL3:
		{
			timeout = 65;
			pow = 40;

			if (info) return (format("healing (about %d%%) every %d turns", pow, timeout));
			if (act)
			{
				(void)heal_player(pow, pow * 5);
				(void)set_stun(0);
				(void)set_cut(0);
				(void)set_poisoned(0);
			}
			break;
		}
		case ACTIV_RANDOM_CURE:
		{
			timeout = 100;

			if (info) return (format("cure ailments every %d turns", timeout));
			if (act)
			{
				msg_print("Tender hands massage your hurts away.");
				(void)set_poisoned(0);
				(void)set_diseased(p_ptr->diseased / 2 - 10, NULL);
				(void)set_confused(0);
				(void)set_image(0);
				(void)set_blind(0, NULL);
				(void)set_stun(0);
				(void)set_cut(0);
				(void)do_res_stat(A_CON, NULL);
			}
			break;
		}
		case ACTIV_RANDOM_PROT_FROM_EVIL:
		{
			timeout = 175;
			dur1 = (p_ptr->protevil ?  1 : 24);
			dur2 = (p_ptr->protevil ? 24 : 48);
			if (p_ptr->protevil) extra = " extra turns";

			if (info) return (format("protection from evil (duration %d-%d%s) every %d turns", dur1, dur2, extra, timeout));
			if (act)
			{
				msg_print("A shrill wail surrounds you.");

				(void)set_protevil(p_ptr->protevil + rand_range(dur1, dur2));

				msg_print("You feel somewhat safer.");
			}
			break;
		}
		case ACTIV_RANDOM_CHAOS:
		{
			timeout = 100;
			pow = 15 + p_ptr->power / 10;
			dam = 20 + p_ptr->power / 5;

			if (info) return (format("explosion of chaos (%d x %d damage) every %d turns", pow, dam, timeout));
			if (act)
			{
				msg_print("You unleash the powers of Unmaking.");
				(void)beam_burst(py, px, GF_CHAOS, pow, dam);
			}
			break;
		}
		case ACTIV_RANDOM_SHARD_SOUND:
		{
			timeout = 75;
			dam = 90 + p_ptr->power;

			if (info) return (format("shard or sound ball (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				msg_print("You evoke the powers of Law...");

				if (!get_aim_dir(&dir)) return ("");
				if (one_in_(2))
				{
					fire_ball(GF_SHARD, dir, dam, 4);
				}
				else
				{
					fire_ball(GF_SOUND, dir, dam, 4);
				}
			}
			break;
		}
		case ACTIV_RANDOM_NETHR:
		{
			timeout = 65;
			dam = 80 + p_ptr->power;

			if (info) return (format("nether orb (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				msg_print("You cast a gleaming orb.");
				fire_orb(GF_NETHER, dir, dam, 1);
			}
			break;
		}
		case ACTIV_RANDOM_LINE_LIGHT:
		{
			timeout1 = 5;     timeout2 = 10;

			if (info) return (format("ray of light (damage 4d5) every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				msg_print("A line of shimmering yellow light appears.");
				lite_line(dir);
			}
			break;
		}
		case ACTIV_RANDOM_STARBURST:
		{
			timeout = 65;
			dam = 60 + p_ptr->power;

			if (info) return (format("starburst (damage %d) every %d turns", dam, timeout));
			if (act)
			{
				(void)fire_star(GF_LITE, 0, dam, 6);
			}
			break;
		}
		case ACTIV_RANDOM_EARTHQUAKE:
		{
			timeout1 = 10;     timeout2 = 20;

			if (info) return (format("earthquake every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("The dungeon trembles!");
				earthquake(p_ptr->py, p_ptr->px, 10);
			}
			break;
		}
		case ACTIV_RANDOM_IDENTIFY:
		{
			timeout = 20;

			if (info) return (format("identify every %d turns", timeout));
			if (act)
			{
				msg_print("You unveil hidden secrets.");
				if (!ident_spell()) return ("");
			}
			break;
		}
		case ACTIV_RANDOM_SPEED:
		{
			timeout1 = 100;     timeout2 = 200;
			dur1 = (p_ptr->fast ? 1 : 20);
			dur2 = (p_ptr->fast ? 5 : 40);
			if (p_ptr->fast) extra = " extra turns";

			if (info) return (format("haste self (duration %d-%d%s) every %d-%d turns", dur1, dur2, extra, timeout1, timeout2));
			if (act)
			{
				msg_print("A bright green glow surrounds you.");

				(void)set_fast(p_ptr->fast + rand_range(dur1, dur2));
			}
			break;
		}
		case ACTIV_RANDOM_TELEPORT_AWAY:
		{
			timeout = 90;

			if (info) return (format("teleport away every %d turns", timeout));
			if (act)
			{
				msg_print("You weave a pattern of rejection and denial.");

				if (!get_aim_dir(&dir)) return ("");
				(void)teleport_monster(dir);
			}

			break;
		}
		case ACTIV_RANDOM_HEROISM:
		{
			timeout1 = 100;     timeout2 = 200;
			dur1 = (p_ptr->hero ?  1 : 25);
			dur2 = (p_ptr->hero ? 25 : 50);
			if (p_ptr->hero) extra = " extra turns";

			if (info) return (format("heroism (duration %d-%d%s) every %d-%d turns", dur1, dur2, extra, timeout1, timeout2));
			if (act)
			{
				msg_print("A thrilling battle song awakes the warrior within you!");

				(void)set_hero(p_ptr->hero + rand_range(dur1, dur2));
			}
			break;
		}
		case ACTIV_RANDOM_STORM_DANCE:
		{
			timeout = 100;

			if (info) return (format("storm dance every %d turns", timeout));
			if (act)
			{
				msg_print("Wild music plays, and you dance up a storm...");
				fire_ball_special(GF_SOUND, 0, 24, 8, 0L, 20);
				fire_ball_special(GF_SHARD, 0, 32, 8, 0L, 20);
				fire_ball_special(GF_CONFUSION, 0, 8, 8, 0L, 20);

				if (one_in_(2))
				{
					(void)take_hit(damroll(1, 12), 0,
						"Your wild movements exhaust you!", "danced to death");
				}
			}
			break;
		}
		case ACTIV_RANDOM_RESIST_ELEMENTS:
		{
			timeout1 = 150;     timeout2 = 400;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("resistance to the elements (duration %d-%d) every %d-%d turns", dur1, dur2, timeout1, timeout2));
			if (act)
			{
				dur = rand_range(dur1, dur2);

				msg_print("Quadricoloured magics swirl around you protectingly.");
				(void)set_oppose_acid(p_ptr->oppose_acid + dur);
				(void)set_oppose_elec(p_ptr->oppose_elec + dur);
				(void)set_oppose_fire(p_ptr->oppose_fire + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur);
			}
			break;
		}
		case ACTIV_RANDOM_RESIST_ALL:
		{
			timeout1 = 150;     timeout2 = 300;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("resistance (%d-%d turns) every %d-%d turns", dur1, dur2, timeout1, timeout2));
			if (act)
			{
				dur = rand_range(dur1, dur2);

				msg_print("Penticoloured magics swirl around you protectingly.");
				(void)set_oppose_acid(p_ptr->oppose_acid + dur);
				(void)set_oppose_elec(p_ptr->oppose_elec + dur);
				(void)set_oppose_fire(p_ptr->oppose_fire + dur);
				(void)set_oppose_cold(p_ptr->oppose_cold + dur);
				(void)set_oppose_pois(p_ptr->oppose_pois + dur);
			}
			break;
		}
		case ACTIV_RANDOM_TELEPORT1:
		{
			timeout1 = 5;     timeout2 = 10;
			pow = 30;

			if (info) return (format("teleport (%d) every %d-%d turns", pow, timeout1, timeout2));
			if (act)
			{
				msg_print("You pass through a transparent gateway...");
				teleport_player(pow, TRUE, FALSE);
			}
			break;
		}
		case ACTIV_RANDOM_TELEPORT2:
		{
			timeout1 = 10;     timeout2 = 20;
			pow = 200;

			if (info) return (format("teleport (%d) every %d-%d turns", pow, timeout1, timeout2));
			if (act)
			{
				msg_print("Time and space twist about you...");
				teleport_player(pow, TRUE, FALSE);
			}
			break;
		}
		case ACTIV_RANDOM_RECALL:
		{
			timeout = 275;

			if (info) return (format("word of recall every %d turns", timeout));
			if (act)
			{
				recall_player();
			}
			break;
		}
		case ACTIV_RANDOM_REGAIN:
		{
			timeout = 650;

			if (info) return (format("restore skills every %d turns", timeout));
			if (act)
			{
				msg_print("Surrounded by darkness, you evoke light and beauty.");

				(void)restore_level();
			}
			break;
		}
		case ACTIV_RANDOM_RESTORE:
		{
			timeout = 650;

			if (info) return (format("restore stats every %d turns", timeout));
			if (act)
			{
				msg_print("A multicolored mist surrounds you, restoring body and mind.");

				(void)do_res_stat(A_STR, NULL);
				(void)do_res_stat(A_INT, NULL);
				(void)do_res_stat(A_WIS, NULL);
				(void)do_res_stat(A_DEX, NULL);
				(void)do_res_stat(A_CON, NULL);
				(void)do_res_stat(A_CHR, NULL);
			}
			break;
		}
		case ACTIV_RANDOM_SHIELD:
		{
			timeout = 150;
			dur1 = (p_ptr->shield ?  1 : 25);
			dur2 = (p_ptr->shield ? 25 : 50);
			if (p_ptr->shield) extra = " extra turns";

			if (info) return (format("magic shield (duration %d-%d%s) every %d turns", dur1, dur2, extra, timeout));
			if (act)
			{
				(void)set_shield(p_ptr->shield + rand_range(dur1, dur2),
					"Magics coalesce to form a shimmering barrier.");
			}
			break;
		}

		case ACTIV_RANDOM_BRAND_MISSILE:
		{
			timeout = 1250;

			if (info) return (format("brand missiles every %d turns", timeout));
			if (act)
			{
				msg_print("Your missile launcher glows with power...");

				(void)brand_missile(0, 0);
			}
			break;
		}
		case ACTIV_RANDOM_DEADLY_SHOOTING:
		{
			timeout1 = 20;     timeout2 = 40;

			if (info) return (format("an especially deadly shot every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				/* Get the correct name for the missile, if possible. */
				cptr missile_name = "missile";
				if (o_ptr->tval == TV_CROSSBOW) missile_name = "bolt";
				if (o_ptr->tval == TV_BOW)      missile_name = "arrow";
				if (o_ptr->tval == TV_SLING)    missile_name = "shot";

				msg_format("The %s you have ready to hand gleams with deadly power.", missile_name);
				p_ptr->special_attack |= (ATTACK_DEADLY);
			}
			break;
		}
		case ACTIV_RANDOM_DETECT_MONSTERS:
		{
			timeout1 = 6;     timeout2 = 12;

			if (info) return (format("detect monsters every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You search for monsters.");

				(void)detect_monsters_normal(FALSE, TRUE);
			}
			break;
		}
		case ACTIV_RANDOM_DETECT_EVIL:
		{
			timeout1 = 6;     timeout2 = 12;

			if (info) return (format("detect evil creatures every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You hunt for evil creatures...");

				(void)detect_evil(FALSE, TRUE);
			}
			break;
		}
		case ACTIV_RANDOM_DETECT_ALL:
		{
			timeout1 = 40;     timeout2 = 80;

			if (info) return (format("detection every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You sense the dungeon around you.");

				(void)detect_all(FALSE, TRUE);
			}
			break;
		}
		case ACTIV_RANDOM_MAGIC_MAP:
		{
			timeout1 = 35;     timeout2 = 70;

			if (info) return (format("dungeon mapping every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("A mental image of your surroundings is fixed in your mind.");

				map_area(0, 0, FALSE);
			}
			break;
		}
		case ACTIV_RANDOM_DETECT_D_S_T:
		{
			timeout1 = 30;     timeout2 = 45;

			if (info) return (format("detect traps, doors, and stairs every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				(void)detect_traps(FALSE, TRUE);
				(void)detect_doors(FALSE);
				(void)detect_stairs(FALSE);
			}
			break;
		}
		case ACTIV_RANDOM_CONFU_FOE:
		{
			timeout1 = 30;     timeout2 = 40;

			if (info) return (format("strong confuse monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You chant runes of confusing...");

				if (!get_aim_dir(&dir)) return ("");
				(void)confuse_monster(dir, 30 + 2 * p_ptr->power / 3);
			}
			break;
		}
		case ACTIV_RANDOM_SLEEP_FOE:
		{
			timeout1 = 30;     timeout2 = 40;

			if (info) return (format("strong sleep monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("A fine dust appears in your hand, and you throw it...");

				if (!get_aim_dir(&dir)) return ("");
				(void)sleep_monster(dir, 30 + 2 * p_ptr->power / 3);
			}
			break;
		}
		case ACTIV_RANDOM_TURN_FOE:
		{
			timeout1 = 30;     timeout2 = 40;

			if (info) return (format("strong panic monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You lock eyes with an enemy...");

				if (!get_aim_dir(&dir)) return ("");
				(void)fear_monster(dir, 30 + 2 * p_ptr->power / 3);
			}
			break;
		}
		case ACTIV_RANDOM_SLOW_FOE:
		{
			timeout1 = 30;     timeout2 = 40;

			if (info) return (format("strong slow monster every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You focus on the mind of an opponent...");

				if (!get_aim_dir(&dir)) return ("");
				(void)slow_monster(dir, 30 + 2 * p_ptr->power / 3);
			}
			break;
		}
		case ACTIV_RANDOM_BANISH_EVIL:
		{
			timeout1 = 80;     timeout2 = 160;

			if (info) return (format("banish evil monsters every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("A mighty hand drives your foes from you!");

				(void)banishment(RF3_EVIL, 100);
			}
			break;
		}
		case ACTIV_RANDOM_DISARM:
		{
			timeout1 = 7;     timeout2 = 14;

			if (info) return (format("disarm trap (chance 95%%) every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("You feel skilled hands guiding your disarming.");

				if (!get_aim_dir(&dir)) return ("");
				(void)disarm_trap(dir);
			}
			break;
		}
		case ACTIV_RANDOM_CONFU_FOES:
		{
			timeout = 150;

			if (info) return (format("confuse monsters every %d turns", timeout));
			if (act)
			{
				msg_print("You intone a bewildering hex...");

				(void)confu_monsters(15 + 4 * p_ptr->power / 5);
			}
			break;
		}
		case ACTIV_RANDOM_SLEEP_FOES:
		{
			timeout = 150;

			if (info) return (format("sleep monsters every %d turns", timeout));
			if (act)
			{
				msg_print("Soft, soothing music washes over you...");

				(void)sleep_monsters(15 + 4 * p_ptr->power / 5);
			}
			break;
		}
		case ACTIV_RANDOM_TURN_FOES:
		{
			timeout = 150;

			if (info) return (format("panic monsters every %d turns", timeout));
			if (act)
			{
				msg_print("You reveal yourself in wrath!");

				(void)fear_monsters(15 + 4 * p_ptr->power / 5);
			}
			break;
		}
		case ACTIV_RANDOM_SLOW_FOES:
		{
			timeout = 150;

			if (info) return (format("slow monsters every %d turns", timeout));
			if (act)
			{
				msg_print("A opaque cloud blankets the area...");

				(void)slow_monsters(15 + 4 * p_ptr->power / 5);
			}
			break;
		}
		case ACTIV_RANDOM_BERSERK:
		{
			timeout = 320;
			dur1 = 40;
			dur2 = 60;

			if (info) return (format("go berserk (duration %d-%d) every %d turns", dur1, dur2, timeout));
			if (act)
			{
				msg_print("A song of wrath sounds!");

				set_berserk(BERSERK_WEAKNESS_LENGTH + rand_range(dur1, dur2));
			}
			break;
		}
		case ACTIV_RANDOM_LIGHT_AREA:
		{
			/* This activation is designed not to be too powerful */
			timeout1 = 15;     timeout2 = 30;

			if (info) return (format("Light up a room or the local area every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				lite_area(0, 3);
			}
			break;
		}
		case ACTIV_RANDOM_SELF_KNOWLEDGE:
		{
			if (info) return ("Grants perfect self knowledge for a brief time.");
			if (act)
			{
				set_self_knowledge(p_ptr->self_knowledge + 3,
					"You begin to know yourself a little better...");
				self_knowledge(TRUE);
			}
			break;
		}
		case ACTIV_RANDOM_DARK_AREA:
		{
			timeout1 = 15;     timeout2 = 30;

			if (info) return (format("Darken a room or the local area every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				unlite_area(0, 3);
			}
			break;
		}

		case ACTIV_EGO_HERO:
		{
			timeout1 = 50;    timeout2 = 100;
			dur1 = 15;     dur2 = 30;

			if (info) return (format("heroism (%d-%d turns) every %d-%d turns", dur1, dur2, timeout1, timeout2));
			if (act)
			{
				(void)set_hero(p_ptr->hero + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_EGO_WHIP:
		{
			if (info) return ("lash out with fire every turn");
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				/* A crude way to simulate a long-range melee blow... */
				fire_arc(GF_FIRE, dir, damroll(o_ptr->dd, o_ptr->ds), 2, 0);
			}
			break;
		}

		/* Activations for missile launchers. */
		case ACTIV_SHOT_PIERCING:
		case ACTIV_SHOT_DAMAGE:
		case ACTIV_SHOT_IMPACT:
		case ACTIV_SHOT_ACCURATE:
		case ACTIV_SHOT_FIRE:
		case ACTIV_SHOT_COLD:
		{
			/* Get the timeout */
			if ((o_ptr->activate == ACTIV_SHOT_ACCURATE) ||
			    (o_ptr->activate == ACTIV_SHOT_FIRE) ||
			    (o_ptr->activate == ACTIV_SHOT_COLD))
			{
				timeout1 = 10;    timeout2 = 20;
			}
			else if ((o_ptr->activate == ACTIV_SHOT_IMPACT) ||
			         (o_ptr->activate == ACTIV_SHOT_DAMAGE))
			{
				timeout1 = 20;    timeout2 = 40;
			}
			else
			{
				timeout1 = 25;    timeout2 = 50;
			}

			if (info)
			{
				cptr str;

				/* Get the information text */
				if (o_ptr->activate == ACTIV_SHOT_PIERCING)
					str = "a shot that can pierce more than one foe";
				else if (o_ptr->activate == ACTIV_SHOT_DAMAGE)
					str = "an especially damaging shot";
				else if (o_ptr->activate == ACTIV_SHOT_IMPACT)
					str = "an shot that can hurl opponents back";
				else if (o_ptr->activate == ACTIV_SHOT_ACCURATE)
					str = "an unusually accurate shot";
				else if (o_ptr->activate == ACTIV_SHOT_FIRE)
					str = "a flaming shot";
				else if (o_ptr->activate == ACTIV_SHOT_COLD)
					str = "a shot of frost";
				else
					str = "an undefined shot";

				return (format("%s every %d-%d turns", str, timeout1, timeout2));
			}

			if (act)
			{
				/* Get the correct name for the missile, if possible. */
				cptr missile_name = "missile";
				if (o_ptr->tval == TV_CROSSBOW) missile_name = "bolt";
				if (o_ptr->tval == TV_BOW)      missile_name = "arrow";
				if (o_ptr->tval == TV_SLING)    missile_name = "shot";

				/* Apply special attack */
				if (o_ptr->activate == ACTIV_SHOT_PIERCING)
					p_ptr->special_attack |= (ATTACK_PIERCING);
				else if (o_ptr->activate == ACTIV_SHOT_DAMAGE)
					p_ptr->special_attack |= (ATTACK_DAMAGE);
				else if (o_ptr->activate == ACTIV_SHOT_IMPACT)
					p_ptr->special_attack |= (ATTACK_IMPACT);
				else if (o_ptr->activate == ACTIV_SHOT_ACCURATE)
					p_ptr->special_attack |= (ATTACK_ACCURATE);
				else if (o_ptr->activate == ACTIV_SHOT_FIRE)
					p_ptr->special_attack |= (ATTACK_SHOT_FIRE);
				else if (o_ptr->activate == ACTIV_SHOT_COLD)
					p_ptr->special_attack |= (ATTACK_SHOT_COLD);

				/* Print message */
				if (o_ptr->activate == ACTIV_SHOT_PIERCING)
					msg_format("The %s you have ready to hand gleams dangerously.", missile_name);
				else if (o_ptr->activate == ACTIV_SHOT_DAMAGE)
					msg_format("The %s you have ready to hand glitters darkly.", missile_name);
				else if (o_ptr->activate == ACTIV_SHOT_IMPACT)
					msg_format("The %s you have ready to hand glows with pure force.", missile_name);
				else if (o_ptr->activate == ACTIV_SHOT_ACCURATE)
					msg_format("The %s you have ready to hand gleams faint blue.", missile_name);
				else if (o_ptr->activate == ACTIV_SHOT_FIRE)
					msg_format("The %s you have ready to hand glows like an ember.", missile_name);
				else if (o_ptr->activate == ACTIV_SHOT_COLD)
					msg_format("The %s you have ready to hand becomes terribly cold.", missile_name);
				else
					msg_print("This activation is undefined.");

				/* Print "special attacks" */
				left_panel_display(DISPLAY_SPECIAL_ATTACK, 0);

				/* Redraw conditions status */
				p_ptr->redraw |= (PR_CONDITIONS);
			}
			break;
		}


		/* Activations for rings. */
		case ACTIV_HOLINESS:
		{
			timeout1 = 60;    timeout2 = 120;
			dur1 = (p_ptr->blessed ?  1 : 24);
			dur2 = (p_ptr->blessed ? 24 : 48);
			if (p_ptr->blessed) extra = " extra turns";

			if (info) return (format("blessing (duration %d-%d%s) every %d-%d turns", dur1, dur2, extra, timeout1, timeout2));
			if (act)
			{
				(void)set_blessed(p_ptr->blessed + rand_range(dur1, dur2), NULL);
			}
			break;
		}

		case ACTIV_RING_ACID:
		{
			timeout1 = 75;    timeout2 = 150;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("infuse %s with acid and oppose acid (duration %d-%d) every %d-%d turns", melee_desc, dur1, dur2, timeout1, timeout2));
			if (act)
			{
				(void)set_acid_attack(get_skill(S_DEVICE, 20, 40));
				(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_RING_ELEC:
		{
			timeout1 = 75;    timeout2 = 150;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("infuse %s with electricity and oppose electricity (duration %d-%d) every %d-%d turns", melee_desc, dur1, dur2, timeout1, timeout2));
			if (act)
			{
				(void)set_elec_attack(get_skill(S_DEVICE, 20, 40));
				(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_RING_FIRE:
		{
			timeout1 = 75;    timeout2 = 150;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("infuse %s with fire and oppose fire (duration %d-%d) every %d-%d turns", melee_desc, dur1, dur2, timeout1, timeout2));
			if (act)
			{
				(void)set_fire_attack(get_skill(S_DEVICE, 20, 40));
				(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_RING_COLD:
		{
			timeout1 = 75;    timeout2 = 150;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("infuse %s with cold and oppose cold (duration %d-%d) every %d-%d turns", melee_desc, dur1, dur2, timeout1, timeout2));
			if (act)
			{
				(void)set_cold_attack(get_skill(S_DEVICE, 20, 40));
				(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(dur1, dur2));
			}
			break;
		}

		case ACTIV_RING_POIS:
		{
			timeout1 = 75;    timeout2 = 150;
			dur1 = 20;     dur2 = 40;

			if (info) return (format("infuse %s with poison and oppose poison (duration %d-%d) every %d-%d turns", melee_desc, dur1, dur2, timeout1, timeout2));
			if (act)
			{
				(void)set_pois_attack(get_skill(S_DEVICE, 20, 40));
				(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(dur1, dur2));
			}
			break;
		}

		/* Activations for amulets. */
		case ACTIV_AMULET_SUSTENANCE:
		{
			timeout1 = 400;    timeout2 = 800;

			if (info) return (format("feed self every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				msg_print("Tasty food fills your belly.");
				(void)set_food(p_ptr->food_bloated - 50);
			}
			break;
		}

		case ACTIV_AMULET_ESCAPING:
		{
			timeout1 = 12;    timeout2 = 24;
			pow = 40;

			if (info) return (format("teleport (%d) every %d-%d turns",
				pow, timeout1, timeout2));
			if (act)
			{
				teleport_player(pow, TRUE, FALSE);
			}
			break;
		}

		case ACTIV_AMULET_MANA:
		{
			timeout1 = 90;    timeout2 = 180;

			if (info) return (format("recover some mana every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				/* Recover mana (but not too much mana) */
				if (p_ptr->csp < p_ptr->msp)
				{
					/* Recover mana based on device skill and max mana */
					pow = (20 + p_ptr->msp) *
						  get_skill(S_DEVICE, 5, 10) / 25;

					(void)sp_player(pow, NULL);
				}

				/* Message */
				else if (object_known_p(o_ptr))
					msg_print("Your mana is already at maximum.");
			}
			break;
		}

		case ACTIV_AMULET_RECHARGING:
		{
			timeout1 = 40;    timeout2 = 60;

			if (info) return (format("recharge magical device every %d-%d turns", timeout1, timeout2));
			if (act)
			{
				if (!recharge(get_skill(S_DEVICE, 90, 140), FALSE))
				{
					timeout1 = 0;
					timeout2 = 0;
				}
			}
			break;
		}



		case ACTIV_DSM_BLACK:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 20 + 2*p_ptr->power;

			if (info) return (format("breathe acid (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_ACID, 0, "You breathe acid.");
				fire_arc(GF_ACID, dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_BLUE:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 20 + 2*p_ptr->power;

			if (info) return (format("breathe lightning (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_ELEC, 0, "You breathe lightning.");
				fire_arc(GF_ELEC, dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_WHITE:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 20 + 2*p_ptr->power;

			if (info) return (format("breathe frost (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_FROST, 0, "You breathe frost.");
				fire_arc(GF_COLD, dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_RED:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 30 + 2*p_ptr->power;

			if (info) return (format("breathe fire (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_FIRE, 0, "You breathe fire.");
				fire_arc(GF_FIRE, dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_GREEN:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 20 + 2*p_ptr->power;

			if (info) return (format("breathe poison gas (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_GAS, 0, "You breathe poison.");
				fire_arc(GF_POIS, dir, dam, 10, 80);
			}
			break;
		}

		case ACTIV_DSM_MULTIHUED:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 40 + 2*p_ptr->power;

			if (info) return (format("breathe an element or poison (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");
				chance = rand_int(5);
				sound(   ((chance == 1) ? MSG_BR_ELEC :
				          ((chance == 2) ? MSG_BR_FROST :
				           ((chance == 3) ? MSG_BR_ACID :
				            ((chance == 4) ? MSG_BR_GAS : MSG_BR_FIRE)))));
				msg_format("You breathe %s.",
						 ((chance == 1) ? "lightning" :
						  ((chance == 2) ? "frost" :
						  ((chance == 3) ? "acid" :
						   ((chance == 4) ? "poison gas" : "fire")))));
				fire_arc(((chance == 1) ? GF_ELEC :
						  ((chance == 2) ? GF_COLD :
						   ((chance == 3) ? GF_ACID :
						    ((chance == 4) ? GF_POIS : GF_FIRE)))),
						dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_SHINING:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 2*p_ptr->power;

			if (info) return (format("breathe light or darkness (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				chance = rand_int(2);
				sound(chance == 0 ? MSG_BR_LIGHT : MSG_BR_DARK);
				msg_format("You breathe %s.",
						  ((chance == 0 ? "light" : "darkness")));
				fire_arc((chance == 0 ? GF_LITE : GF_DARK), dir,
						dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_MORPHIC:
		{
			timeout1 = 200;     timeout2 = 300;
			dur = 40;

			if (info) return (format("dragon metamorphosis (duration %d) every %d-%d turns",
				dur, timeout1, timeout2));
			if (act)
			{
				/* Turn into a dragon, but only temporarily */
				(void)shapechange_temp(dur, SHAPE_DRAGON);
			}
			break;
		}

		case ACTIV_DSM_BRONZE:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 30 + 3 * p_ptr->power / 2;

			if (info) return (format("breathe confusion (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_CONF, 0, "You breathe confusion.");
				fire_arc(GF_CONFUSION, dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_GOLD:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 2*p_ptr->power;

			if (info) return (format("breathe sound (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_SOUND, 0, "You breathe sound.");
				fire_arc(GF_SOUND, dir, dam, 10, 65);
			}
			break;
		}

		case ACTIV_DSM_CHAOS:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 50 + 2*p_ptr->power;

			if (info) return (format("breathe chaos or disenchant (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				chance = rand_int(3);
				sound(chance != 0 ? MSG_BR_CHAOS : MSG_BR_DISENCHANT);
				msg_format("You breathe %s.",
					((chance != 0 ? "chaos" : "disenchantment")));
				fire_arc((chance != 0 ? GF_CHAOS : GF_DISENCHANT),
						dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_LAW:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 50 + 2*p_ptr->power;

			if (info) return (format("breathe sound or shards (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				chance = rand_int(2);
				sound(chance == 1 ? MSG_BR_SOUND : MSG_BR_SHARDS);
				msg_format("You breathe %s.",
					((chance == 1 ? "sound" : "shards")));
				fire_arc((chance == 1 ? GF_SOUND : GF_SHARD),
						dir, dam, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_BALANCE:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 70 + 2*p_ptr->power;

			if (info) return (format("breathe sound, shards, chaos, or disenchantment (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				chance = rand_int(4);
				sound(   ((chance == 1) ? MSG_BR_CHAOS :
				          ((chance == 2) ? MSG_BR_DISENCHANT :
				           ((chance == 3) ? MSG_BR_SOUND : MSG_BR_SHARDS))));
				msg_format("You breathe %s.",
				         ((chance == 1) ? "chaos" :
				          ((chance == 2) ? "disenchantment" :
				           ((chance == 3) ? "sound" : "shards"))));
				fire_arc(((chance == 1) ? GF_CHAOS :
				          ((chance == 2) ? GF_DISENCHANT :
				           ((chance == 3) ? GF_SOUND : GF_SHARD))),
						dir, 210, 10, 50);
			}
			break;
		}

		case ACTIV_DSM_POWER:
		{
			timeout1 = 120;     timeout2 = 160;
			dam = 70 + 2*p_ptr->power;

			if (info) return (format("breathe the elements (damage %d) every %d-%d turns", dam, timeout1, timeout2));
			if (act)
			{
				if (!get_aim_dir(&dir)) return ("");

				message(MSG_BR_ELEMENTS, 0, "You breathe the elements.");
				fire_arc(GF_MANA, dir, dam, 10, 50);
			}
			break;
		}


		default:
		{
			if (info) return ("Undefined activation.");
			if (act) msg_print("Undefined activation.");
			break;
		}
	}

	/* Activating the item */
	if (act)
	{
		/* Set timeout */
		o_ptr->timeout = timeout + rand_range(timeout1, timeout2);
	}


	/* Return */
	return ("");
}


/*
 * Activate a wielded object.
 *
 * The player has a (4%) chance to learn more about any dragon armor
 * activation.  If he does, detailed information about it will appear
 * when the item is 'I'nspected.  -LM-
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" when asked for a direction.
 *
 * We do not allow items to be activated from inventory, even though
 * that would be more convenient, because it just feels weird to be
 * able to zap DSM like rods.
 */
void do_cmd_activate(void)
{
	int item;
	int chance;

	object_type *o_ptr;
	object_kind *k_ptr;


	cptr q, s;

	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
	q = "Activate which item?";
	s = "You have nothing to activate.";
	if (!get_item(&item, q, s, (USE_EQUIP))) return;
	item_to_object(o_ptr, item);


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Calculate success chance */
	chance = device_chance(o_ptr);

	/* Note total lack of chance */
	if (chance <= 0)
	{
		char o_name[DESC_LEN];

		/* Describe the object briefly (without the flavour) */
		object_desc_flavour = -1;
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		/* Message */
		msg_format("You have no idea how to get the %s to work.", o_name);
		return;
	}

	/* Roll for failure */
	if (chance <= rand_int(100))
	{
		if (flush_failure) flush();

		/* The One Ring does not like incompetence */
		if (o_ptr->activate == ACTIV_POWER)
		{
			/* Oh no. */
			msg_print("The One Ring jerks angrily!");
			msg_print("You are surrounded by a malignant aura!");

			/* Decrease some stats (permanently) */
			if (one_in_(3)) (void)dec_stat(A_STR, 1, TRUE);
			if (one_in_(3)) (void)dec_stat(A_INT, 1, TRUE);
			if (one_in_(3)) (void)dec_stat(A_WIS, 1, TRUE);
			if (one_in_(3)) (void)dec_stat(A_DEX, 1, TRUE);
			if (one_in_(3)) (void)dec_stat(A_CON, 1, TRUE);
			if (one_in_(3)) (void)dec_stat(A_CHR, 1, TRUE);

			/* Lose a fair amount of experience (permanently) */
			lose_exp(calc_spent_exp() / 50, TRUE);

			check_experience();
		}

		/* Other activatables do nothing special */
		else
		{
			msg_print("You failed to activate it properly.");
		}

		/* Optional delay */
		if (delay_failure) pause_for(250);

		return;
	}


	/* Check the recharge */
	if (o_ptr->timeout)
	{
		/* Getting greedy with the One Ring is a bad mistake. */
		if (o_ptr->activate == ACTIV_POWER)
		{
			/* Oh no. */
			msg_print("The One Ring turns on you!");
			msg_print("You are surrounded by a malignant aura!");

			/* Greatly decrease all stats (permanently) */
			(void)dec_stat(A_STR, rand_range(2, 4), TRUE);
			(void)dec_stat(A_INT, rand_range(2, 4), TRUE);
			(void)dec_stat(A_WIS, rand_range(2, 4), TRUE);
			(void)dec_stat(A_DEX, rand_range(2, 4), TRUE);
			(void)dec_stat(A_CON, rand_range(2, 4), TRUE);
			(void)dec_stat(A_CHR, rand_range(2, 4), TRUE);

			/* Lose a lot of experience (permanently) */
			lose_exp(calc_spent_exp() / 5, TRUE);

			check_experience();
		}

		/* Other items just fail (but you don't lose your turn) */
		else
		{
			msg_print("It whines, glows and fades...");
			p_ptr->energy_use = 0;
		}

		/* Optional delay */
		if (delay_failure) pause_for(250);

		return;
	}


	/* Wonder Twin Powers... Activate! */
	message(MSG_ACT_ARTIFACT, 0, "You activate it...");

	/* Practice the device skill */
	skill_being_used = S_DEVICE;

	/* Activate */
	(void)do_activation_aux(OBJECT_USE, o_ptr);


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Hack -- The Axe 'Slaughterfield' is now on the floor */
	if (o_ptr->activate == ACTIV_SLAUGHTERFIELD)
	{
		/* Reduce and describe inventory */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Reduce and describe floor item */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_optimize(0 - item);
		}
		return;
	}


	/* Get the object kind */
	k_ptr = &k_info[o_ptr->k_idx];

	/* Object (not artifact) is known, but effects are not yet fully known */
	if ((object_known_p(o_ptr)) &&
	    (!(k_ptr->special & (SPECIAL_KNOWN_EFFECT))))
	{
		/* Any weapon or armor is easy to learn */
		if (is_wargear(o_ptr))
		{
			chance = get_skill(S_PERCEPTION, 20, 70);
		}

		/* Rings, amulets, and light sources are harder */
		else
		{
			chance = get_skill(S_DEVICE, 0, 10) +
			         get_skill(S_PERCEPTION, 0, 10);
		}

		/* Roll for chance */
		if (rand_int(100) < chance)
		{
			/* We now know about the object's effects */
			k_ptr->special |= (SPECIAL_KNOWN_EFFECT);

			/* Not an artifact */
			if (!artifact_p(o_ptr))
			{
				if (o_ptr->tval == TV_DRAG_ARMOR)
				{
					msg_format("You feel you know more about %s.",
						k_name + k_ptr->name);
				}
				else if ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET))
				{
					cptr p = ((o_ptr->tval == TV_RING) ? "ring" : "amulet");

					msg_format("You feel you know more about %ss of %s.",
						p, k_name + k_ptr->name);
				}
				else
				{
					msg_print("You feel you know more about this object.");
				}
			}

			/* An artifact */
			else
			{
				char o_name[DESC_LEN];

				/* Get a description */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 3);

				msg_format("You feel you know more about the %s.", o_name);
			}
		}
	}

	/* Done */
	return;
}

