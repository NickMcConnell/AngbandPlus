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


static void do_cmd_eat_food_aux(int item)
{
	object_type *o_ptr;
	char        Rumor[1024];
	bool        egg_summoned = FALSE;

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

	if (o_ptr->tval == TV_FOOD)
	{
		/* Analyze the food */
		switch (o_ptr->sval)
		{
			case SV_FOOD_INC_STR:
			{
				(void)do_inc_stat(A_STR);
				break;
			}

			case SV_FOOD_INC_INT:
			{
				(void)do_inc_stat(A_INT);
				break;
			}

			case SV_FOOD_INC_WIS:
			{
				(void)do_inc_stat(A_WIS);
				break;
			}

			case SV_FOOD_INC_DEX:
			{
				(void)do_inc_stat(A_DEX);
				break;
			}

			case SV_FOOD_INC_CON:
			{
				(void)do_inc_stat(A_CON);
				break;
			}

			case SV_FOOD_INC_CHR:
			{
				(void)do_inc_stat(A_CHR);
				break;
			}

			case SV_FOOD_EGG:
			case SV_FOOD_BIG_EGG:
			case SV_FOOD_SPECIAL_EGG:
			{
				switch (randint1(5))
				{
					case 1:
					{
						int r_idx = MON_EGG_BASE + 5 * (o_ptr->sval - SV_FOOD_EGG) + randint0(3);
						egg_summoned = summon_named_creature(-1, py, px, r_idx, PM_FORCE_PET);
						if (egg_summoned) break;
						/* Fall through */
					}
					default:
#ifdef JP
						msg_print("美味い。");
#else
						msg_print("That tastes good.");
#endif
						break;
				}
				break;
			}

			case SV_FOOD_ROTTEN_EGG:
			{
				switch (randint1(10))
				{
					case 1:
					{
						int r_idx = MON_ROTTEN_EGG_BASE + randint0(3);
						egg_summoned = summon_named_creature(-1, py, px, r_idx, PM_FORCE_PET);
						if (egg_summoned) break;
						/* Fall through */
					}
					default:
#ifdef JP
						msg_print("腐っている!");
#else
						msg_print("It's rotten!");
#endif
						(void)set_paralyzed(p_ptr->paralyzed + 4);
						(void)set_poisoned(p_ptr->poisoned + randint0(15) + 10);

						if (!(p_ptr->muta3 & MUT3_WART_SKIN)) gain_random_mutation(165, FALSE);
						if (!(p_ptr->muta2 & MUT2_WASTING)) gain_random_mutation(105, FALSE);
						break;
				}
				break;
			}

			case SV_FOOD_AUGMENTATION:
			{
				(void)do_inc_stat(A_STR);
				(void)do_inc_stat(A_INT);
				(void)do_inc_stat(A_WIS);
				(void)do_inc_stat(A_DEX);
				(void)do_inc_stat(A_CON);
				(void)do_inc_stat(A_CHR);
				break;
			}

#ifdef JP
			/* それぞれの食べ物の感想をオリジナルより細かく表現 */
			case SV_FOOD_BISCUIT:
			{
				msg_print("甘くてサクサクしてとてもおいしい。");
				break;
			}

			case SV_FOOD_JERKY:
			{
				msg_print("歯ごたえがあっておいしい。");
				break;
			}

			case SV_FOOD_SLIME_MOLD:
			{
				msg_print("これはなんとも形容しがたい味だ。");
				break;
			}

			case SV_FOOD_RATION:
			{
				msg_print("これはおいしい。");
				break;
			}
#else
			case SV_FOOD_RATION:
			case SV_FOOD_BISCUIT:
			case SV_FOOD_JERKY:
			case SV_FOOD_SLIME_MOLD:
			{
				msg_print("That tastes good.");
				break;
			}
#endif


			case SV_FOOD_WAYBREAD:
			{
#ifdef JP
				msg_print("これはひじょうに美味だ。");
#else
				msg_print("That tastes good.");
#endif

				(void)set_poisoned(0);
				(void)hp_player(damroll(4, 8));
				break;
			}

#ifdef JP
			case SV_FOOD_PINT_OF_ALE:
			case SV_FOOD_PINT_OF_WINE:
			{
				msg_print("のどごし爽やかだ。");
				break;
			}
#else
			case SV_FOOD_PINT_OF_ALE:
			case SV_FOOD_PINT_OF_WINE:
			{
				msg_print("That tastes good.");
				break;
			}
#endif

			case SV_FOOD_FORTUNE_COOKIE:
			{
				errr err = 0;

				switch (randint1(20))
				{
					case 1:
#ifdef JP
						err = get_rnd_line("error_j.txt", 0, Rumor);
#else
						err = get_rnd_line("error.txt", 0, Rumor);
#endif
						break;

					case 2:
					case 3:
					case 4:
#ifdef JP
						err = get_rnd_line("death_j.txt", 0, Rumor);
#else
						err = get_rnd_line("death.txt", 0, Rumor);
#endif
						break;

					default:
#ifdef JP
						err = get_rnd_line_jonly("rumors_j.txt", 0, Rumor, 10);
#else
						err = get_rnd_line("rumors.txt", 0, Rumor);
#endif
						break;
				}

				/* An error occured */
#ifdef JP
				if (err) strcpy(Rumor, "入っていた石で歯が欠けてしまった。");
#else
				if (err) strcpy(Rumor, "Some rumors are wrong.");
#endif

#ifdef JP
				msg_print("これはおいしい。");
				msg_print("クッキーの中にはメッセージが入っていた:");
#else
				msg_print("That tastes good.");
				msg_print("There is message in the cookie. It says:");
#endif

				msg_print(NULL);
				msg_format("%s", Rumor);
				msg_print(NULL);

				break;
			}

			case SV_FOOD_ROTTEN_PUMPKIN:
			{
				if (prace_is_(RACE_PUMPKINHEAD))
				{
#ifdef JP
					msg_print("これはひじょうに美味だ。");
#else
					msg_print("That tastes good.");
#endif
					(void)hp_player(80);
					(void)set_blind(0);
					(void)set_poisoned(0);
					(void)set_confused(0);
					(void)set_stun(0);
					(void)set_cut(0);
					(void)set_image(0);
					(void)set_stoning(0);
				}
				else
				{
					int i = randint0(20) + 10;

					if (!p_ptr->resist_pois && !p_ptr->oppose_pois) (void)set_poisoned(p_ptr->poisoned + i);
					if (!p_ptr->resist_conf) (void)set_confused(p_ptr->confused + i);
					if (!p_ptr->resist_chaos) (void)set_image(p_ptr->image + i);
				}

				break;
			}
		}
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	if (egg_summoned)
	{
		/* Nothing */
	}
	else if (prace_is_(RACE_SKELETON))
	{
		if (!((o_ptr->sval == SV_FOOD_WAYBREAD) ||
		      (o_ptr->sval < SV_FOOD_BISCUIT)))
		{
			object_type forge;
			object_type *q_ptr = &forge;

#ifdef JP
			msg_print("食べ物がアゴを素通りして落ちた！");
#else
			msg_print("The food falls through your jaws!");
#endif


			/* Create the item */
			object_prep(q_ptr, lookup_kind(o_ptr->tval, o_ptr->sval));

			/* Drop the object from heaven */
			(void)drop_near(q_ptr, -1, py, px);
		}
		else
		{
#ifdef JP
			msg_print("食べ物がアゴを素通りして落ち、消えた！");
#else
			msg_print("The food falls through your jaws and vanishes!");
#endif

		}
	}
	else if (p_ptr->no_digest)
	{
#ifdef JP
		msg_print("食物はあなたにとって全く栄養にならない。");
#else
		msg_print("The food is non-sustenance for you.");
#endif
	}
	else if (p_ptr->pclass == CLASS_VAMPIRE)
	{
		/* Reduced nutritional benefit */
		(void)set_food(p_ptr->food + (o_ptr->pval / 10));
#ifdef JP
		msg_print("あなたのような者にとって食糧など僅かな栄養にしかならない。");
#else
		msg_print("Mere victuals hold scant sustenance for a being such as yourself.");
#endif

		if (p_ptr->food < PY_FOOD_ALERT)   /* Hungry */
#ifdef JP
			msg_print("あなたの飢えは新鮮な血によってのみ満たされる！");
#else
			msg_print("Your hunger can only be satisfied with fresh blood!");
#endif

	}
	else
	{
		if (!prace_is_(RACE_PUMPKINHEAD) && (o_ptr->sval == SV_FOOD_ROTTEN_PUMPKIN))
		{
#ifdef JP
			msg_print("うげぇ！！ ...吐いてしまった。");
#else
			msg_print("The food makes you vomit!");
#endif
			(void)set_food(PY_FOOD_STARVE - 1);
		}
		else
		{
			(void)set_food(p_ptr->food + o_ptr->pval);
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
}


/*
 * Hook to determine if an object is eatable
 */
static bool item_tester_hook_eatable(object_type *o_ptr)
{
	if (o_ptr->tval == TV_FOOD) return TRUE;

	/* Assume not */
	return FALSE;
}


/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{
	int         item;
	cptr        q, s;


	/* Restrict choices to food */
	item_tester_hook = item_tester_hook_eatable;

	/* Get an item */
#ifdef JP
	q = "どれを食べますか? ";
	s = "食べ物がない。";
#else
	q = "Eat which item? ";
	s = "You have nothing to eat.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Eat the object */
	do_cmd_eat_food_aux(item);
}


/*
 * Quaff a potion (from the pack or the floor)
 */
static void do_cmd_quaff_potion_aux(int item)
{
	int         ident, lev;
	object_type	*o_ptr;
	object_type forge;
	object_type *q_ptr;

	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];


	/* Take a turn */
	energy_use = 100;

	if (stop_the_time_player)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("瓶から水が流れ出てこない！");
#else
		msg_print("The potion doesn't flow out from a bottle.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	if (p_ptr->singing) stop_singing();

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

	/* Get local object */
	q_ptr = &forge;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Single object */
	q_ptr->number = 1;

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
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}

	/* Sound */
	sound(SOUND_QUAFF);


	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = get_object_level(q_ptr);

	/* Analyze the potion */
	if (q_ptr->tval == TV_POTION)
	{
		switch (q_ptr->sval)
		{
		case SV_POTION_SLOWNESS:
			if (set_slow(randint1(25) + 15, FALSE)) ident = TRUE;
			break;

		case SV_POTION_SALT_WATER:
			if (!p_ptr->no_digest)
			{
#ifdef JP
				msg_print("うぇ！思わず吐いてしまった。");
#else
				msg_print("The potion makes you vomit!");
#endif

				/* Only living creatures get thirsty */
				(void)set_food(PY_FOOD_STARVE - 1);
				(void)set_poisoned(0);
				(void)set_paralyzed(p_ptr->paralyzed + 4);
				ident = TRUE;
			}

			break;

		case SV_POTION_POISON:
			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + randint0(15) + 10))
				{
					ident = TRUE;
				}
			}
			break;

		case SV_POTION_BLINDNESS:
			if (!p_ptr->resist_blind)
			{
				if (set_blind(p_ptr->blind + randint0(100) + 100))
				{
					ident = TRUE;
				}
			}
			break;

		case SV_POTION_CONFUSION: /* Booze */
			if (!p_ptr->resist_conf)
			{
				if (set_confused(randint0(20) + 15))
				{
					ident = TRUE;
				}
			}

			if (!p_ptr->resist_chaos)
			{
				if (one_in_(2))
				{
					if (set_image(p_ptr->image + randint0(150) + 150))
					{
						ident = TRUE;
					}
				}
				if (one_in_(13))
				{
					ident = TRUE;
					if (one_in_(3)) lose_all_info();
					else wiz_dark(FALSE);
					teleport_player(100);
					wiz_dark(FALSE);
#ifdef JP
					msg_print("知らない場所で目が醒めた。頭痛がする。");
					msg_print("何も思い出せない。どうやってここへ来たのかも分からない！");
#else
					msg_print("You wake up somewhere with a sore head...");
					msg_print("You can't remember a thing, or how you got here!");
#endif

				}
			}
			break;

		case SV_POTION_SLEEP:
			if (!p_ptr->free_act)
			{
#ifdef JP
				msg_print("あなたは眠ってしまった。");
#else
				msg_print("You fall asleep.");
#endif


				if (set_paralyzed(p_ptr->paralyzed + randint0(4) + 4))
				{
					ident = TRUE;
				}
			}
			break;

		case SV_POTION_LOSE_MEMORIES:
			if (!p_ptr->hold_life && ((p_ptr->exp > 0) || (cexp_ptr->cexp > 0)))
			{
#ifdef JP
				msg_print("過去の記憶が薄れていく気がする。");
#else
				msg_print("You feel your memories fade.");
#endif

				lose_class_exp(cexp_ptr->cexp / 4);
				lose_racial_exp(p_ptr->exp / 4);
				ident = TRUE;
			}
			break;

		case SV_POTION_RUINATION:
#ifdef JP
			msg_print("身も心も弱ってきて、精気が抜けていくようだ。");
			take_hit(DAMAGE_LOSELIFE, damroll(10, 10), "破滅の薬");
#else
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(DAMAGE_LOSELIFE, damroll(10, 10), "a potion of Ruination");
#endif

			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
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
#ifdef JP
			msg_print("体の中で激しい爆発が起きた！");
			take_hit(DAMAGE_NOESCAPE, damroll(50, 20), "爆発の薬");
#else
			msg_print("Massive explosions rupture your body!");
			take_hit(DAMAGE_NOESCAPE, damroll(50, 20), "a potion of Detonation");
#endif

			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			ident = TRUE;
			break;

		case SV_POTION_DEATH:
#ifdef JP
			msg_print("死の予感が体中を駆けめぐった。");
			take_hit(DAMAGE_LOSELIFE, 5000, "死の薬");
#else
			msg_print("A feeling of Death flows through your body.");
			take_hit(DAMAGE_LOSELIFE, 5000, "a potion of Death");
#endif

			ident = TRUE;
			break;

		case SV_POTION_INFRAVISION:
			if (set_tim_infra(p_ptr->tim_infra + 100 + randint1(100), FALSE))
			{
				ident = TRUE;
			}
			break;

		case SV_POTION_DETECT_INVIS:
			if (set_tim_invis(p_ptr->tim_invis + 12 + randint1(12), FALSE))
			{
				ident = TRUE;
			}
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
			if (!p_ptr->fast)
			{
				if (set_fast(randint1(25) + 15, FALSE)) ident = TRUE;
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5, FALSE);
			}
			break;

		case SV_POTION_RESIST_HEAT:
			if (set_oppose_fire(p_ptr->oppose_fire + randint1(10) + 10, FALSE))
			{
				ident = TRUE;
			}
			break;

		case SV_POTION_RESIST_COLD:
			if (set_oppose_cold(p_ptr->oppose_cold + randint1(10) + 10, FALSE))
			{
				ident = TRUE;
			}
			break;

		case SV_POTION_HEROISM:
			if (set_afraid(0)) ident = TRUE;
			if (set_hero(p_ptr->hero + randint1(25) + 25, FALSE)) ident = TRUE;
			if (hp_player(10)) ident = TRUE;
			break;

		case SV_POTION_BERSERK_STRENGTH:
			if (set_afraid(0)) ident = TRUE;
			if (set_shero(p_ptr->shero + randint1(25) + 25, FALSE)) ident = TRUE;
			if (hp_player(30)) ident = TRUE;
			break;

		case SV_POTION_CURE_LIGHT:
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_cut(p_ptr->cut - 10)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
			break;

		case SV_POTION_CURE_SERIOUS:
			if (hp_player(damroll(4, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_cut((p_ptr->cut / 2) - 50)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
			break;

		case SV_POTION_CURE_CRITICAL:
			if (hp_player(damroll(6, 8))) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
			break;

		case SV_POTION_HEALING:
			if (hp_player(300)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
			break;

		case SV_POTION_STAR_HEALING:
			if (hp_player(1200)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
			break;

		case SV_POTION_LIFE:
#ifdef JP
			msg_print("体中に生命力が満ちあふれてきた！");
#else
			msg_print("You feel life flow through your body!");
#endif

			restore_level();
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_stoning(0);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);
			(void)set_shero(0,TRUE);
			update_stuff();
			hp_player(5000);
			ident = TRUE;
			break;

		case SV_POTION_RESTORE_MANA:
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
#ifdef JP
				msg_print("頭がハッキリとした。");
#else
				msg_print("Your feel your head clear.");
#endif

				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
				ident = TRUE;
			}
			if (set_shero(0,TRUE)) ident = TRUE;
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

		case SV_POTION_RESTORING:
			if (do_res_stat(A_STR)) ident = TRUE;
			if (do_res_stat(A_INT)) ident = TRUE;
			if (do_res_stat(A_WIS)) ident = TRUE;
			if (do_res_stat(A_DEX)) ident = TRUE;
			if (do_res_stat(A_CON)) ident = TRUE;
			if (do_res_stat(A_CHR)) ident = TRUE;
			break;

		case SV_POTION_ENLIGHTENMENT:
#ifdef JP
			msg_print("自分の置かれている状況が脳裏に浮かんできた...");
#else
			msg_print("An image of your surroundings forms in your mind...");
#endif

			wiz_lite(FALSE);
			ident = TRUE;
			break;

		case SV_POTION_CURE_STONING:
			if (set_stoning(0)) ident = TRUE;
			break;

		case SV_POTION_SELF_KNOWLEDGE:
#ifdef JP
			msg_print("自分自身のことが少しは分かった気がする...");
#else
			msg_print("You begin to know yourself a little better...");
#endif

			msg_print(NULL);
			self_knowledge();
			ident = TRUE;
			break;

		case SV_POTION_EXPERIENCE:
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee;

#ifdef JP
				msg_print("更に経験を積んだような気がする。");
#else
				msg_print("You feel more experienced.");
#endif
				/* Class */
				ee = (cexp_ptr->cexp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				gain_class_exp(ee);

				/* Racial */
				ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				gain_racial_exp(ee);

				ident = TRUE;
			}
			change_your_alignment_lnc(1);
			break;

		case SV_POTION_RESISTANCE:
			(void)set_oppose_acid(p_ptr->oppose_acid + randint1(20) + 20, FALSE);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint1(20) + 20, FALSE);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint1(20) + 20, FALSE);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint1(20) + 20, FALSE);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint1(20) + 20, FALSE);
			ident = TRUE;
			break;

		case SV_POTION_CURING:
			if (hp_player(50)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;

		case SV_POTION_INVULNERABILITY:
			(void)set_invuln(p_ptr->invuln + randint1(4) + 4, FALSE);
			ident = TRUE;
			break;

		case SV_POTION_CURE_MUTATIONS:
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
#ifdef JP
				msg_print("全ての突然変異が治った。");
#else
				msg_print("You are cured of all mutations.");
#endif

				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
				mutant_regenerate_mod = calc_mutant_regenerate_mod();
				ident = TRUE;
			}
			break;

		case SV_POTION_STAR_CURING:
			if (hp_player(700)) ident = TRUE;
			if (set_blind(0)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_confused(0)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_stoning(0)) ident = TRUE;
			if (set_image(0)) ident = TRUE;
			break;

		case SV_POTION_POLYMORPH:
			if ((p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3) && one_in_(23))
			{
#ifdef JP
				msg_print("全ての突然変異が治った。");
#else
				msg_print("You are cured of all mutations.");
#endif

				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
			else
			{
				do
				{
					if (one_in_(2))
					{
						if (gain_random_mutation(0, TRUE)) ident = TRUE;
					}
					else if (lose_mutation(0)) ident = TRUE;
				} while(!ident || one_in_(2));
			}
			break;
		}
	}

	if (prace_is_(RACE_SKELETON))
	{
#ifdef JP
		msg_print("液体の一部はあなたのアゴを素通りして落ちた！");
#else
		msg_print("Some of the fluid falls through your jaws!");
#endif

		(void)potion_smash_effect(0, py, px, q_ptr->k_idx);
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	if (!(object_aware_p(o_ptr))) change_your_alignment_lnc(-1);

	/* The item has been tried */
	object_tried(q_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(q_ptr))
	{
		object_aware(q_ptr);
		gain_class_exp((lev + (cexp_ptr->clev >> 1)) / cexp_ptr->clev);
		gain_racial_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

	/* Potions can feed the player */
	if (p_ptr->pclass == CLASS_VAMPIRE) set_food(p_ptr->food + o_ptr->pval / 10);
	else if (!p_ptr->no_digest) set_food(p_ptr->food + o_ptr->pval);
}


/*
 * Hook to determine if an object can be quaffed
 */
static bool item_tester_hook_quaff(object_type *o_ptr)
{
	return (o_ptr->tval == TV_POTION) ? TRUE : FALSE;
}


/*
 * Quaff some potion (from the pack or floor)
 */
void do_cmd_quaff_potion(void)
{
	int  item;
	cptr q, s;

	/* Restrict choices to potions */
	item_tester_hook = item_tester_hook_quaff;

	/* Get an item */
#ifdef JP
	q = "どの薬を飲みますか? ";
	s = "飲める薬がない。";
#else
	q = "Quaff which potion? ";
	s = "You have no potions to quaff.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Quaff the potion */
	do_cmd_quaff_potion_aux(item);
}


/*
 * Read a scroll (from the pack or floor).
 *
 * Certain scrolls can be "aborted" without losing the scroll.  These
 * include scrolls with no effects but recharge or identify, which are
 * cancelled before use.  XXX Reading them still takes a turn, though.
 */
static void do_cmd_read_scroll_aux(int item, bool known)
{
	int         k, used_up, ident, lev;
	object_type *o_ptr;


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

	if (stop_the_time_player)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("止まった時の中ではうまく働かないようだ。");
#else
		msg_print("Nothing happen.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	if (p_ptr->singing) stop_singing();

	/* Not identified yet */
	ident = FALSE;

	/* Object level */
	lev = get_object_level(o_ptr);

	/* Assume the scroll will get used up */
	used_up = TRUE;

	if (o_ptr->tval == TV_SCROLL)
	{
	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				(void)set_blind(p_ptr->blind + 3 + randint1(5));
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
#ifdef JP
			msg_print("カン高くうなる様な音が辺りを覆った。");
#else
			msg_print("There is a high pitched humming noise.");
#endif

			aggravate_monsters(0);
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
			if (curse_weapon(FALSE, INVEN_RARM)) ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			for (k = 0; k < randint1(3); k++)
			{
				if (summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID)))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			for (k = 0; k < randint1(3); k++)
			{
				if (summon_specific(0, py, px, dun_level, SUMMON_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID)))
				{
					ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_PET:
		{
			if (summon_specific(-1, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_FORCE_PET)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_SUMMON_KIN:
		{
			if (summon_kin_player(p_ptr->lev, py, px, (PM_FORCE_PET | PM_ALLOW_GROUP)))
			{
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation(py, px)) ident = TRUE;
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
			(void)teleport_level(0);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			if (!word_of_recall()) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			if (!ident_spell(FALSE)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			if (!identify_fully(FALSE)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse())
			{
#ifdef JP
				msg_print("誰かに見守られているような気がする。");
#else
				msg_print("You feel as if someone is watching over you.");
#endif

				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			if (remove_all_curse())
			{
#ifdef JP
				msg_print("誰かに見守られているような気がする。");
#else
				msg_print("You feel as if someone is watching over you.");
#endif
			}
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
			if (!enchant_spell(0, 0, randint1(3) + 2)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(randint1(3), randint1(3), 0)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (!recharge(130)) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_MUNDANITY:
		{
			ident = TRUE;
			if (!mundane_spell(FALSE)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			if (lite_area(damroll(2, 8), 2)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			map_area(DETECT_RAD_MAP);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_GOLD:
		{
			if (detect_treasure(DETECT_RAD_DEFAULT)) ident = TRUE;
			if (detect_objects_gold(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_ITEM:
		{
			if (detect_objects_normal(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
			if (detect_traps(DETECT_RAD_DEFAULT, known)) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_DOOR:
		{
			if (detect_doors(DETECT_RAD_DEFAULT)) ident = TRUE;
			if (detect_stairs(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
			if (detect_monsters_invis(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESS_WEAPON:
		{
			if (!bless_weapon()) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_UNHOLY_WEAPON:
		{
			if (!unholy_weapon()) used_up = FALSE;
			ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (set_blessed(p_ptr->blessed + randint1(12) + 6, FALSE)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (set_blessed(p_ptr->blessed + randint1(24) + 12, FALSE)) ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (set_blessed(p_ptr->blessed + randint1(48) + 24, FALSE)) ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (!(p_ptr->special_attack & ATTACK_CONFUSE))
			{
#ifdef JP
				msg_print("手が輝き始めた。");
#else
				msg_print("Your hands begin to glow.");
#endif

				p_ptr->special_attack |= ATTACK_CONFUSE;
				p_ptr->redraw |= (PR_STATUS);
				ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint1(25) + k, FALSE)) ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			warding_glyph();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_ILLUSION:
		{
			if (set_multishadow(12 + randint1(12), FALSE)) ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			if (destroy_area(py, px, 13+randint0(5)))
				ident = TRUE;
			else
#ifdef JP
				msg_print("ダンジョンが揺れた...");
#else
				msg_print("The dungeon trembles...");
#endif


			break;
		}

		case SV_SCROLL_SNAP_DRAGON:
		{
			snap_dragon();
			ident = TRUE;
			break;
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(80)) ident = TRUE;
			break;
		}

		case SV_SCROLL_GENOCIDE:
		{
			(void)symbol_genocide(300, TRUE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_GENOCIDE:
		{
			(void)mass_genocide(300, TRUE);
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
			acquirement(py, px, randint1(2) + 1, TRUE, FALSE);
			ident = TRUE;
			break;
		}

		case SV_SCROLL_FIRE:
		{
			fire_ball(GF_FIRE, 0, 666, 4, TRUE);
			/* Note: "Double" damage since it is centered on the player ... */
			if (!(p_ptr->oppose_fire || p_ptr->resist_fire || p_ptr->immune_fire))
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, 50+randint1(50), "炎の巻物");
#else
				take_hit(DAMAGE_NOESCAPE, 50 + randint1(50), "a Scroll of Fire");
#endif

			ident = TRUE;
			break;
		}


		case SV_SCROLL_ICE:
		{
			fire_ball(GF_ICE, 0, 777, 4, TRUE);
			if (!(p_ptr->oppose_cold || p_ptr->resist_cold || p_ptr->immune_cold))
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, 100+randint1(100), "氷の巻物");
#else
				take_hit(DAMAGE_NOESCAPE, 100 + randint1(100), "a Scroll of Ice");
#endif

			ident = TRUE;
			break;
		}

		case SV_SCROLL_CHAOS:
		{
			fire_ball(GF_CHAOS, 0, 1000, 4, TRUE);
			if (!p_ptr->resist_chaos)
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, 111+randint1(111), "カオスの巻物");
#else
				take_hit(DAMAGE_NOESCAPE, 111 + randint1(111), "a Scroll of Chaos");
#endif

			ident = TRUE;
			break;
		}

		case SV_SCROLL_DEADLY_BLOW_LEARNING:
		{
			int  available[MAX_SB];
			int  num = 0;

			for (k = 0; k < MAX_SB; k++)
			{
				if (!(p_ptr->special_blow & (0x00000001L << k))) available[num++] = k;
			}

			if (num)
			{
				k = randint0(num);
#ifdef JP
				msg_format("%sを習得した！", special_blow_info[available[k]].name);
#else
				msg_format("You learned %s!", special_blow_info[available[k]].name);
#endif
				p_ptr->special_blow |= (0x00000001L << available[k]);
			}
			else
			{
#ifdef JP
				msg_print("何も新しい技は覚えられなかった。");
#else
				msg_format("You learned no new special blow.");
#endif
			}

			ident = TRUE;
			break;
		}

		case SV_SCROLL_ARTIFACT:
		{
			ident = TRUE;
			if (!artifact_scroll()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_RESET_RECALL:
		{
			ident = TRUE;
			if (!reset_recall()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_EGO_CREATION:
		{
			ident = TRUE;
			if (!ego_creation_scroll()) used_up = FALSE;
			break;
		}

		case SV_SCROLL_REVERT_ELEM:
		{
			if (set_opposite_pelem(p_ptr->opposite_pelem + 300 + randint1(200))) ident = TRUE;
			break;
		}

		case SV_SCROLL_ELEM_FIRE:
		{
			ident = TRUE;
			inc_area_elem(0, ELEM_FIRE, 5, -2, TRUE);
			break;
		}

		case SV_SCROLL_ELEM_AQUA:
		{
			ident = TRUE;
			inc_area_elem(0, ELEM_AQUA, 5, -2, TRUE);
			break;
		}

		case SV_SCROLL_ELEM_EARTH:
		{
			ident = TRUE;
			inc_area_elem(0, ELEM_EARTH, 5, -2, TRUE);
			break;
		}

		case SV_SCROLL_ELEM_WIND:
		{
			ident = TRUE;
			inc_area_elem(0, ELEM_WIND, 5, -2, TRUE);
			break;
		}
	}
	}
	else if (o_ptr->name1 == ART_FIRECREST)
	{
#ifdef JP
		msg_print("不思議な呪文が記されているが、あなたはそれを読むことができない。");
#else
		msg_print("There is mysterious spell, but you cannot read.");
#endif
		used_up = FALSE;
	}
	else if (o_ptr->tval == TV_TAROT)
	{
		if (!activate_tarot_power(o_ptr->pval)) used_up = FALSE;
	}
	else if (o_ptr->tval == TV_SCRATCH_CARD)
	{
		int i;
		scratch_card_type *sci_ptr;
		byte tval, sval;
		bool allow_prep;

		msg_print("あなたはスピードくじを開けた。 … ");

		for (i = 1; i <= MAX_SCRATCH_CARD_INFO; i++)
		{
			sci_ptr = &scratch_card_info[i];

			if (!one_in_(sci_ptr->prob)) continue;

			allow_prep = TRUE;

			msg_print(sci_ptr->msg);

			tval = sci_ptr->tval;
			sval = sci_ptr->sval;
			switch (i)
			{
			case 1:
				{
					artifact_type *a_ptr = &a_info[ART_BLADETURNER];

					if (!a_ptr->cur_num)
					{
						/* Create the artifact */
						if (create_named_art(ART_BLADETURNER, py, px))
						{
							a_ptr->cur_num = 1;
							/* Hack -- Memorize location of artifact in saved floors */
							if (character_dungeon) a_ptr->floor_id = p_ptr->floor_id;
						}
						else if (!preserve_mode)
							a_ptr->cur_num = 1;
						if (a_ptr->cur_num) allow_prep = FALSE;
					}
					else
					{
						tval = TV_SCROLL;
						sval = SV_SCROLL_ACQUIREMENT;
					}
				}
				break;
			case 10:
				switch (p_ptr->pelem)
				{
				case ELEM_FIRE:
					sval = SV_AMULET_FOL;
					break;
				case ELEM_AQUA:
					sval = SV_AMULET_OHN;
					break;
				case ELEM_EARTH:
					sval = SV_AMULET_SOL;
					break;
				case ELEM_WIND:
					sval = SV_AMULET_VAN;
					break;
				}
				break;
			}

			if (allow_prep)
			{
				object_type forge;
				object_type *q_ptr;

				/* Get local object */
				q_ptr = &forge;

				/* Create the item */
				object_prep(q_ptr, lookup_kind(tval, sval));

				/* Apply magic (no messages, no artifacts) */
				apply_magic(q_ptr, dun_level, AMF_GOOD);

				(void)drop_near(q_ptr, -1, py, px);
			}
			break;
		}

		if (i > MAX_SCRATCH_CARD_INFO)
		{
			sci_ptr = &scratch_card_info[0];
			msg_print(sci_ptr->msg);
		}
	}


	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	if (!(object_aware_p(o_ptr))) change_your_alignment_lnc(-1);

	/* The item was tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
		object_aware(o_ptr);
		gain_class_exp((lev + (cexp_ptr->clev >> 1)) / cexp_ptr->clev);
		gain_racial_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);


	/* Hack -- allow certain scrolls to be "preserved" */
	if (!used_up)
	{
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
}


/*
 * Hook to determine if an object is readable
 */
static bool item_tester_hook_readable(object_type *o_ptr)
{
	if ((o_ptr->tval == TV_TAROT) || (o_ptr->tval == TV_SCRATCH_CARD) || (o_ptr->tval == TV_SCROLL) || (o_ptr->name1 == ART_FIRECREST)) return (TRUE);

	/* Assume not */
	return (FALSE);
}


void do_cmd_read_scroll(void)
{
	object_type *o_ptr;
	int  item;
	cptr q, s;

	/* Check some conditions */
	if (p_ptr->blind)
	{
#ifdef JP
		msg_print("目が見えない。");
#else
		msg_print("You can't see anything.");
#endif

		return;
	}
	if (no_lite())
	{
#ifdef JP
		msg_print("明かりがないので、暗くて読めない。");
#else
		msg_print("You have no light to read by.");
#endif

		return;
	}
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("混乱していて読めない。");
#else
		msg_print("You are too confused!");
#endif

		return;
	}


	/* Restrict choices to scrolls */
	item_tester_hook = item_tester_hook_readable;

	/* Get an item */
#ifdef JP
	q = "どの巻物を読みますか? ";
	s = "読める巻物がない。";
#else
	q = "Read which scroll? ";
	s = "You have no scrolls to read.";
#endif

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

	/* Read the scroll */
	do_cmd_read_scroll_aux(item, object_aware_p(o_ptr));
}


static int staff_effect(int sval, bool *use_charge, bool known)
{
	int k;
	int ident = FALSE;

	/* Analyze the staff */
	switch (sval)
	{
		case SV_STAFF_DARKNESS:
		{
			if (!(p_ptr->resist_blind) && !(p_ptr->resist_dark))
			{
				if (set_blind(p_ptr->blind + 3 + randint1(5))) ident = TRUE;
			}
			if (unlite_area(10, 3)) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOWNESS:
		{
			if (set_slow(p_ptr->slow + randint1(30) + 15, FALSE)) ident = TRUE;
			break;
		}

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters(p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			for (k = 0; k < randint1(4); k++)
			{
				if (summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID)))
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
			if (!ident_spell(FALSE)) *use_charge = FALSE;
			ident = TRUE;
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				if (!p_ptr->blind)
				{
#ifdef JP
					msg_print("杖は一瞬ブルーに輝いた...");
#else
					msg_print("The staff glows blue for a moment...");
#endif

				}
				ident = TRUE;
			}
			break;
		}

		case SV_STAFF_STARLITE:
		{
			int num = damroll(5, 3);
			int y, x;
			int attempts;

			if (!p_ptr->blind)
			{
#ifdef JP
				msg_print("杖の先が明るく輝いた...");
#else
				msg_print("The end of the staff glows brightly...");
#endif

			}
			for (k = 0; k < num; k++)
			{
				attempts = 1000;

				while(attempts--)
				{
					scatter(&y, &x, py, px, 4, 0);

					if (!cave_floor_bold(y, x)) continue;

					if ((y != py) || (x != px)) break;
				}

				project(0, 0, y, x, damroll(6 + p_ptr->lev / 8, 10), GF_LITE_WEAK,
						  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL), MODIFY_ELEM_MODE_MAGIC);
			}
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
			map_area(DETECT_RAD_MAP);
			ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_GOLD:
		{
			if (detect_treasure(DETECT_RAD_DEFAULT)) ident = TRUE;
			if (detect_objects_gold(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_ITEM:
		{
			if (detect_objects_normal(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
			if (detect_traps(DETECT_RAD_DEFAULT, known)) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
			if (detect_doors(DETECT_RAD_DEFAULT)) ident = TRUE;
			if (detect_stairs(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
			if (detect_monsters_invis(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
			if (detect_monsters_evil(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			if (hp_player(damroll(2, 8))) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
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
			if (set_shero(0,TRUE)) ident = TRUE;
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
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
#ifdef JP
				msg_print("頭がハッキリとした。");
#else
				msg_print("Your feel your head clear.");
#endif

				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER);
				p_ptr->window |= (PW_SPELL);
			}
			if (set_shero(0,TRUE)) ident = TRUE;
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			if (sleep_monsters(p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			if (slow_monsters(p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_STAFF_SPEED:
		{
			if (set_fast(randint1(30) + 15, FALSE)) ident = TRUE;
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
			if (dispel_evil(80)) ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(150)) ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(150)) ident = TRUE;
			k = 3 * p_ptr->lev;
			if (set_protevil(p_ptr->protevil + randint1(25) + k, FALSE)) ident = TRUE;
			if (set_poisoned(0)) ident = TRUE;
			if (set_afraid(0)) ident = TRUE;
			if (hp_player(50)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			break;
		}

		case SV_STAFF_GENOCIDE:
		{
			(void)symbol_genocide(200, TRUE);
			ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			if (earthquake(py, px, 10))
				ident = TRUE;
			else
#ifdef JP
				msg_print("ダンジョンが揺れた。");
#else
				msg_print("The dungeon trembles.");
#endif


			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			if (destroy_area(py, px, 13+randint0(5)))
				ident = TRUE;

			break;
		}

		case SV_STAFF_ANIMATE_DEAD:
		{
			if (animate_dead(0, py, px))
				ident = TRUE;

			break;
		}

		case SV_STAFF_MSTORM:
		{
#ifdef JP
			msg_print("強力な魔力が敵を引き裂いた！");
#else
			msg_print("Mighty magics rend your enemies!");
#endif
			project(0, 5, py, px,
				(randint1(200) + 300) * 2, GF_MANA, PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID, MODIFY_ELEM_MODE_MAGIC);
			if ((p_ptr->pclass != CLASS_WIZARD) && (p_ptr->pclass != CLASS_WARLOCK) && (p_ptr->pclass != CLASS_ARCHMAGE) && (p_ptr->pclass != CLASS_WITCH) && (p_ptr->pclass != CLASS_SIRENE) && (p_ptr->pclass != CLASS_LICH) && (p_ptr->pclass != CLASS_HIGHWITCH))
			{
#ifdef JP
				(void)take_hit(DAMAGE_NOESCAPE, 50, "コントロールし難い強力な魔力の解放");
#else
				(void)take_hit(DAMAGE_NOESCAPE, 50, "unleashing magics too mighty to control");
#endif
			}
			ident = TRUE;

			break;
		}

		case SV_STAFF_VOID:
		{
			call_the_();
			ident = TRUE;

			break;
		}
	}
	return ident;
}

/*
 * Use a staff.			-RAK-
 *
 * One charge of one staff disappears.
 *
 * Hack -- staffs of identify can be "cancelled".
 */
static void do_cmd_use_staff_aux(int item)
{
	int         ident, chance, lev;
	object_type *o_ptr;


	/* Hack -- let staffs of identify get aborted */
	bool use_charge = TRUE;


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
#ifdef JP
		msg_print("まずは杖を拾わなければ。");
#else
		msg_print("You must first pick up the staffs.");
#endif

		return;
	}


	/* Take a turn */
	energy_use = 100;

	/* Extract the item level */
	lev = get_object_level(o_ptr);
	if (lev > 50) lev = 50 + (lev - 50)/2;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - lev;

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	if (stop_the_time_player)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("止まった時の中ではうまく働かないようだ。");
#else
		msg_print("Nothing happen. Maybe this staff is freezing too.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE) || (p_ptr->pclass == CLASS_TERRORKNIGHT))
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("杖をうまく使えなかった。");
#else
		msg_print("You failed to use the staff properly.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	/* Notice empty staffs */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("この杖にはもう魔力が残っていない。");
#else
		msg_print("The staff has no charges left.");
#endif

		o_ptr->ident |= (IDENT_EMPTY);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->window |= (PW_INVEN);

		return;
	}


	/* Sound */
	sound(SOUND_ZAP);

	ident = staff_effect(o_ptr->sval, &use_charge, object_aware_p(o_ptr));

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	if (!(object_aware_p(o_ptr))) change_your_alignment_lnc(-1);

	/* Tried the item */
	object_tried(o_ptr);

	/* An identification was made */
	if (ident && !object_aware_p(o_ptr))
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
		object_aware(o_ptr);
		gain_class_exp((lev + (cexp_ptr->clev >> 1)) / cexp_ptr->clev);
		gain_racial_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
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
		p_ptr->total_weight -= q_ptr->weight;
		item = inven_carry(q_ptr);

		/* Message */
#ifdef JP
		msg_print("杖をまとめなおした。");
#else
		msg_print("You unstack your staff.");
#endif

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


void do_cmd_use_staff(void)
{
	int  item;
	cptr q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_STAFF;

	/* Get an item */
#ifdef JP
	q = "どの杖を使いますか? ";
	s = "使える杖がない。";
#else
	q = "Use which staff? ";
	s = "You have no staff to use.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	do_cmd_use_staff_aux(item);
}


static int wand_effect(int sval, int dir)
{
	int ident = FALSE;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER) sval = randint0(SV_WAND_WONDER);

	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir, damroll(10, 10))) ident = TRUE;
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir, p_ptr->lev)) ident = TRUE;
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
#ifdef JP
			msg_print("青く輝く光線が放たれた。");
#else
			msg_print("A line of blue shimmering light appears.");
#endif

			(void)lite_line(dir);
			ident = TRUE;
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
			if (sleep_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
			if (slow_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			if (confuse_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
			if (fear_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			if (drain_life(dir, 80 + p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			if (poly_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			fire_ball(GF_POIS, dir, 12 + p_ptr->lev / 4, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			fire_bolt_or_beam(20, GF_MISSILE, dir, damroll(2 + p_ptr->lev / 10, 6));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			fire_bolt_or_beam(20, GF_ACID, dir, damroll(6 + p_ptr->lev / 7, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_CHARM_MONSTER:
		{
			if (charm_monster(dir, MAX(20, p_ptr->lev)))
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			fire_bolt_or_beam(20, GF_FIRE, dir, damroll(7 + p_ptr->lev / 6, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			fire_bolt_or_beam(20, GF_COLD, dir, damroll(5 + p_ptr->lev / 8, 8));
			ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60 + 3 * p_ptr->lev / 4, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 40 + 3 * p_ptr->lev / 4, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 70 + 3 * p_ptr->lev / 4, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 50 + 3 * p_ptr->lev / 4, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_WONDER:
		{
#ifdef JP
			msg_print("おっと、謎の魔法棒を始動させた。");
#else
			msg_print("Oops.  Wand of wonder activated.");
#endif

			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_PURE_FIRE, dir, 200, -3, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			fire_ball(GF_PURE_AQUA, dir, 180, -3, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint1(5))
			{
				case 1:
				{
					fire_ball(GF_PURE_EARTH, dir, 240, -3, FALSE);
					break;
				}

				case 2:
				{
					fire_ball(GF_PURE_WIND, dir, 210, -3, FALSE);
					break;
				}

				case 3:
				{
					fire_ball(GF_PURE_FIRE, dir, 240, -3, FALSE);
					break;
				}

				case 4:
				{
					fire_ball(GF_PURE_AQUA, dir, 210, -3, FALSE);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 180, -3, FALSE);
					break;
				}
			}

			ident = TRUE;
			break;
		}

		case SV_WAND_PETRO_CLOUD:
		{
			fire_ball(GF_STONE, dir, 200 + randint1(p_ptr->lev * 2), 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_DISINTEGRATE:
		{
			fire_ball(GF_DISINTEGRATE, dir, 200 + randint1(p_ptr->lev * 2), 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_STRIKING:
		{
			fire_bolt(GF_METEOR, dir, damroll(15 + p_ptr->lev / 3, 13));
			ident = TRUE;
			break;
		}

		case SV_WAND_GENOCIDE:
		{
			fire_ball_hide(GF_GENOCIDE, dir, 250, 0, FALSE);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEM_FIRE:
		{
#ifdef JP
			msg_print("火のエレメントを活性化させた。");
#else
			msg_print("You enhanced the fire elememt.");
#endif
			fire_elem_ball(ELEM_FIRE, dir, 3, 6);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEM_AQUA:
		{
#ifdef JP
			msg_print("水のエレメントを活性化させた。");
#else
			msg_print("You enhanced the aqua elememt.");
#endif
			fire_elem_ball(ELEM_AQUA, dir, 3, 6);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEM_EARTH:
		{
#ifdef JP
			msg_print("地のエレメントを活性化させた。");
#else
			msg_print("You enhanced the earth elememt.");
#endif
			fire_elem_ball(ELEM_EARTH, dir, 3, 6);
			ident = TRUE;
			break;
		}

		case SV_WAND_ELEM_WIND:
		{
#ifdef JP
			msg_print("風のエレメントを活性化させた。");
#else
			msg_print("You enhanced the wind elememt.");
#endif
			fire_elem_ball(ELEM_WIND, dir, 3, 6);
			ident = TRUE;
			break;
		}
	}
	return ident;
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
static void do_cmd_aim_wand_aux(int item)
{
	int         lev, ident, chance, dir;
	object_type *o_ptr;
	bool old_target_pet = target_pet;

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
#ifdef JP
		msg_print("まずは魔法棒を拾わなければ。");
#else
		msg_print("You must first pick up the wands.");
#endif

		return;
	}


	/* Allow direction to be cancelled for free */
	if (object_aware_p(o_ptr) && (o_ptr->sval == SV_WAND_HEAL_MONSTER
				      || o_ptr->sval == SV_WAND_HASTE_MONSTER))
			target_pet = TRUE;
	if (!get_aim_dir(&dir))
	{
		target_pet = old_target_pet;
		return;
	}
	target_pet = old_target_pet;

	/* Take a turn */
	energy_use = 100;

	/* Get the level */
	lev = get_object_level(o_ptr);
	if (lev > 50) lev = 50 + (lev - 50)/2;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - lev;

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && one_in_(USE_DEVICE - chance + 1))
	{
		chance = USE_DEVICE;
	}

	if (stop_the_time_player)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("止まった時の中ではうまく働かないようだ。");
#else
		msg_print("Nothing happen. Maybe this wand is freezing too.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint1(chance) < USE_DEVICE) || (p_ptr->pclass == CLASS_TERRORKNIGHT))
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("魔法棒をうまく使えなかった。");
#else
		msg_print("You failed to use the wand properly.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	/* The wand is already empty! */
	if (o_ptr->pval <= 0)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("この魔法棒にはもう魔力が残っていない。");
#else
		msg_print("The wand has no charges left.");
#endif

		o_ptr->ident |= (IDENT_EMPTY);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->window |= (PW_INVEN);

		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	ident = wand_effect(o_ptr->sval, dir);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	if (!(object_aware_p(o_ptr))) change_your_alignment_lnc(-1);

	/* Mark it as tried */
	object_tried(o_ptr);

	/* Apply identification */
	if (ident && !object_aware_p(o_ptr))
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
		object_aware(o_ptr);
		gain_class_exp((lev + (cexp_ptr->clev >> 1)) / cexp_ptr->clev);
		gain_racial_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
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


void do_cmd_aim_wand(void)
{
	int     item;
	cptr    q, s;

	/* Restrict choices to wands */
	item_tester_tval = TV_WAND;

	/* Get an item */
#ifdef JP
	q = "どの魔法棒で狙いますか? ";
	s = "使える魔法棒がない。";
#else
	q = "Aim which wand? ";
	s = "You have no wand to aim.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Aim the wand */
	do_cmd_aim_wand_aux(item);
}


static int rod_effect(int sval, int dir, bool *use_charge)
{
	int ident = FALSE;

	/* Analyze the rod */
	switch (sval)
	{
		case SV_ROD_DETECT_TRAP:
		{
			if (detect_traps(DETECT_RAD_DEFAULT, (bool)(dir ? FALSE : TRUE))) ident = TRUE;
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
			if (detect_doors(DETECT_RAD_DEFAULT)) ident = TRUE;
			if (detect_stairs(DETECT_RAD_DEFAULT)) ident = TRUE;
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			if (!ident_spell(FALSE)) *use_charge = FALSE;
			ident = TRUE;
			break;
		}

		case SV_ROD_RECALL:
		{
			if (!word_of_recall()) *use_charge = FALSE;
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
			map_area(DETECT_RAD_MAP);
			ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
			detect_all(DETECT_RAD_DEFAULT);
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
			if (set_shero(0,TRUE)) ident = TRUE;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(500)) ident = TRUE;
			if (set_stun(0)) ident = TRUE;
			if (set_cut(0)) ident = TRUE;
			if (set_shero(0,TRUE)) ident = TRUE;
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
			if (set_fast(randint1(30) + 15, FALSE)) ident = TRUE;
			break;
		}

		case SV_ROD_PESTICIDE:
		{
			if (dispel_monsters(4)) ident = TRUE;
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
#ifdef JP
			msg_print("青く輝く光線が放たれた。");
#else
			msg_print("A line of blue shimmering light appears.");
#endif

			(void)lite_line(dir);
			ident = TRUE;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
			if (sleep_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
			if (slow_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			if (drain_life(dir, 70 + 3 * p_ptr->lev / 2)) ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir, p_ptr->lev)) ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(6 + p_ptr->lev / 7, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(4 + p_ptr->lev / 9, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(7 + p_ptr->lev / 6, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(5 + p_ptr->lev / 8, 8));
			ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 60 + p_ptr->lev, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 40 + p_ptr->lev, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			fire_ball(GF_FIRE, dir, 70 + p_ptr->lev, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 50 + p_ptr->lev, 2, FALSE);
			ident = TRUE;
			break;
		}

		case SV_ROD_HAVOC:
		{
			call_chaos(p_ptr->lev);
			ident = TRUE;
			break;
		}

		case SV_ROD_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) ident = TRUE;
			break;
		}

		case SV_ROD_AGGRAVATE:
		{
			aggravate_monsters(0);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEM_FIRE:
		{
			inc_area_elem(0, ELEM_FIRE, 6, 2, TRUE);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEM_AQUA:
		{
			inc_area_elem(0, ELEM_AQUA, 6, 2, TRUE);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEM_EARTH:
		{
			inc_area_elem(0, ELEM_EARTH, 6, 2, TRUE);
			ident = TRUE;
			break;
		}

		case SV_ROD_ELEM_WIND:
		{
			inc_area_elem(0, ELEM_WIND, 6, 2, TRUE);
			ident = TRUE;
			break;
		}
	}
	return ident;
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
static void do_cmd_zap_rod_aux(int item)
{
	int ident, chance, lev, fail;
	int dir = 0;
	object_type *o_ptr;
	bool success;

	/* Hack -- let perception get aborted */
	bool use_charge = TRUE;

	object_kind *k_ptr;

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
#ifdef JP
		msg_print("まずはロッドを拾わなければ。");
#else
		msg_print("You must first pick up the rods.");
#endif

		return;
	}


	/* Get a direction (unless KNOWN not to need it) */
	if (((o_ptr->sval >= SV_ROD_MIN_DIRECTION) &&
	     (o_ptr->sval != SV_ROD_HAVOC) &&
	     (o_ptr->sval != SV_ROD_PESTICIDE) &&
	     (o_ptr->sval != SV_ROD_AGGRAVATE) &&
	     !((o_ptr->sval >= SV_ROD_ELEM_FIRE) && (o_ptr->sval <= SV_ROD_ELEM_WIND))) ||
	     !object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return;
	}


	/* Take a turn */
	energy_use = 100;

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	fail = lev+5;
	if (chance > fail) fail -= (chance - fail)*2;
	else chance -= (fail - chance)*2;
	if (fail < USE_DEVICE) fail = USE_DEVICE;
	if (chance < USE_DEVICE) chance = USE_DEVICE;

	if (stop_the_time_player)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("止まった時の中ではうまく働かないようだ。");
#else
		msg_print("Nothing happen. Maybe this rod is freezing too.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	if (p_ptr->pclass == CLASS_TERRORKNIGHT) success = FALSE;
	else if (chance > fail)
	{
		if (randint0(chance*2) < fail) success = FALSE;
		else success = TRUE;
	}
	else
	{
		if (randint0(fail*2) < chance) success = TRUE;
		else success = FALSE;
	}

	/* Roll for usage */
	if (!success)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("うまくロッドを使えなかった。");
#else
		msg_print("You failed to use the rod properly.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	k_ptr = &k_info[o_ptr->k_idx];

	/* A single rod is still charging */
	if ((o_ptr->number == 1) && (o_ptr->timeout))
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("このロッドはまだ魔力を充填している最中だ。");
#else
		msg_print("The rod is still charging.");
#endif

		return;
	}
	/* A stack of rods lacks enough energy. */
	else if ((o_ptr->number > 1) && (o_ptr->timeout > k_ptr->pval * (o_ptr->number - 1)))
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("そのロッドはまだ充填中です。");
#else
		msg_print("The rods are all still charging.");
#endif

		return;
	}

	/* Sound */
	sound(SOUND_ZAP);

	ident = rod_effect(o_ptr->sval, dir, &use_charge);

	/* Increase the timeout by the rod kind's pval. -LM- */
	if (use_charge) o_ptr->timeout += k_ptr->pval;

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	if (!(object_aware_p(o_ptr))) change_your_alignment_lnc(-1);

	/* Tried the object */
	object_tried(o_ptr);

	/* Successfully determined the object function */
	if (ident && !object_aware_p(o_ptr))
	{
		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
		object_aware(o_ptr);
		gain_class_exp((lev + (cexp_ptr->clev >> 1)) / cexp_ptr->clev);
		gain_racial_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);
	}

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}


void do_cmd_zap_rod(void)
{
	int item;
	cptr q, s;

	/* Restrict choices to rods */
	item_tester_tval = TV_ROD;

	/* Get an item */
#ifdef JP
	q = "どのロッドを振りますか? ";
	s = "使えるロッドがない。";
#else
	q = "Zap which rod? ";
	s = "You have no rod to zap.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

	/* Zap the rod */
	do_cmd_zap_rod_aux(item);
}


/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	/* Check activation flag */
	if (have_flag(flgs, TR_ACTIVATE)) return (TRUE);

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
			cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

			/* Message */
#ifdef JP
			msg_print("あなたは悪性のオーラに包み込まれた。");
#else
			msg_print("You are surrounded by a malignant aura.");
#endif

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
			if (p_ptr->exp < 0) p_ptr->exp = 0;
			p_ptr->max_exp -= (p_ptr->exp / 4);
			if (p_ptr->max_exp < 0) p_ptr->max_exp = 0;
			cexp_ptr->cexp -= (cexp_ptr->cexp / 4);
			if (cexp_ptr->cexp < 0) cexp_ptr->cexp = 0;
			cexp_ptr->max_cexp -= (cexp_ptr->cexp / 4);
			if (cexp_ptr->max_cexp < 0) cexp_ptr->max_cexp = 0;
			check_experience();

			break;
		}

		case 3:
		{
			/* Message */
#ifdef JP
			msg_print("あなたは強力なオーラに包み込まれた。");
#else
			msg_print("You are surrounded by a powerful aura.");
#endif


			/* Dispel monsters */
			dispel_monsters(1000);

			break;
		}

		case 4:
		case 5:
		case 6:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, dir, 600, 3, FALSE);

			break;
		}

		case 7:
		case 8:
		case 9:
		case 10:
		{
			/* Mana Bolt */
			fire_bolt(GF_MANA, dir, 500);

			break;
		}
	}
}


/*
 * Hack -- activate the Bladeturner
 */
static void activate_bladeturner(int dir)
{
	/* Pick a random effect */
	switch (randint1(4))
	{
		case 1:
		{
			/* Message */
			msg_print("あなたは*火炎*のブレスを吐いた。");
			fire_ball(GF_PURE_FIRE, dir, 300, -4, FALSE);

			break;
		}

		case 2:
		{
			/* Message */
			msg_print("あなたは*水*のブレスを吐いた。");
			fire_ball(GF_PURE_AQUA, dir, 300, -4, FALSE);

			break;
		}

		case 3:
		{
			/* Message */
			msg_print("あなたは*大地*のブレスを吐いた。");
			fire_ball(GF_PURE_EARTH, dir, 300, -4, FALSE);

			break;
		}

		case 4:
		{
			/* Message */
			msg_print("あなたは*風*のブレスを吐いた。");
			fire_ball(GF_PURE_WIND, dir, 300, -4, FALSE);

			break;
		}
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
static void do_cmd_activate_aux(int item)
{
	int         k, dir, lev, chance, fail;
	int vit = p_ptr->stat_use[A_CON];
	object_type *o_ptr;
	bool success;


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

	/* Extract the item level */
	lev = get_object_level(o_ptr);

	/* Hack -- use artifact level instead */
	if (artifact_p(o_ptr)) lev = a_info[o_ptr->name1].level;
	else if (o_ptr->art_name)
	{
		switch (o_ptr->xtra2)
		{
			case ACT_SUNLIGHT:
			case ACT_BO_MISS_1:
			case ACT_BA_POIS_1:
			case ACT_CONFUSE:
			case ACT_SLEEP:
			case ACT_CURE_LW:
			case ACT_CURE_POISON:
			case ACT_BERSERK:
			case ACT_LIGHT:
			case ACT_DEST_DOOR:
			case ACT_TELEPORT:
				lev = 10;
				break;
			case ACT_BO_ELEC_1:
			case ACT_BO_ACID_1:
			case ACT_BO_COLD_1:
			case ACT_BO_FIRE_1:
			case ACT_MAP_LIGHT:
			case ACT_STONE_MUD:
			case ACT_CURE_MW:
			case ACT_QUAKE:
				lev = 20;
				break;
			case ACT_DRAIN_1:
			case ACT_TELE_AWAY:
			case ACT_ESP:
			case ACT_RESIST_ALL:
			case ACT_DETECT_ALL:
			case ACT_RECALL:
			case ACT_SATIATE:
			case ACT_RECHARGE:
				lev = 30;
				break;
			case ACT_BA_COLD_1:
			case ACT_BA_FIRE_1:
			case ACT_TERROR:
			case ACT_PROT_EVIL:
			case ACT_ID_PLAIN:
			case ACT_REST_LIFE:
			case ACT_SPEED:
			case ACT_BANISH_EVIL:
				lev = 40;
				break;
			case ACT_DRAIN_2:
			case ACT_VAMPIRE_1:
			case ACT_BO_MISS_2:
			case ACT_BA_FIRE_2:
			case ACT_WHIRLWIND:
			case ACT_CHARM_ANIMAL:
			case ACT_SUMMON_ANIMAL:
			case ACT_DISP_EVIL:
			case ACT_DISP_GOOD:
			case ACT_XTRA_SPEED:
			case ACT_DETECT_XTRA:
			case ACT_ID_FULL:
				lev = 50;
				break;
			case ACT_VAMPIRE_2:
			case ACT_BA_COLD_3:
			case ACT_BA_ELEC_3:
			case ACT_GENOCIDE:
			case ACT_CHARM_UNDEAD:
			case ACT_CHARM_OTHER:
			case ACT_SUMMON_PHANTOM:
			case ACT_SUMMON_ELEMENTAL:
			case ACT_RUNE_EXPLO:
				lev = 60;
				break;
			case ACT_MASS_GENO:
			case ACT_CHARM_ANIMALS:
			case ACT_CHARM_OTHERS:
			case ACT_CURE_700:
			case ACT_RUNE_PROT:
			case ACT_ALCHEMY:
			case ACT_REST_ALL:
				lev = 70;
				break;
			case ACT_CALL_CHAOS:
			case ACT_ROCKET:
			case ACT_BA_MISS_3:
			case ACT_CURE_1000:
			case ACT_DIM_DOOR:
			case ACT_SUMMON_UNDEAD:
			case ACT_SUMMON_DEMON:
				lev = 80;
				break;
			case ACT_WRAITH:
			case ACT_INVULN:
				lev = 100;
				break;
			default:
				lev = 0;
		}
	}
	else if (((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) && o_ptr->name2) lev = e_info[o_ptr->name2].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	fail = lev+5;
	if (chance > fail) fail -= (chance - fail)*2;
	else chance -= (fail - chance)*2;
	if (fail < USE_DEVICE) fail = USE_DEVICE;
	if (chance < USE_DEVICE) chance = USE_DEVICE;

	if (stop_the_time_player)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("止まった時の中ではうまく働かないようだ。");
#else
		msg_print("It shows no reaction.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	else if (chance > fail)
	{
		if (randint0(chance*2) < fail) success = FALSE;
		else success = TRUE;
	}
	else
	{
		if (randint0(fail*2) < chance) success = TRUE;
		else success = FALSE;
	}

	/* Roll for usage */
	if (!success)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_print("うまく始動させることができなかった。");
#else
		msg_print("You failed to activate it properly.");
#endif

		sound(SOUND_FAIL);
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
#ifdef JP
		msg_print("それは微かに音を立て、輝き、消えた...");
#else
		msg_print("It whines, glows and fades...");
#endif

		return;
	}


	/* Activate the artifact */
#ifdef JP
	msg_print("始動させた...");
#else
	msg_print("You activate it...");
#endif


	/* Sound */
	sound(SOUND_ZAP);


	if (o_ptr->art_name && o_ptr->xtra2)
	{
		(void)activate_random_artifact(o_ptr);

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
			case ART_LEGACY:
			{
#ifdef JP
				msg_print("オルゴールから澄んだ光があふれ出た...");
#else
				msg_print("The music box wells with clear light...");
#endif

				lite_area(damroll(2, 15), 3);
				o_ptr->timeout = randint0(10) + 10;
				break;
			}

			case ART_PUMPKIN:
			{
				int ty = py, tx = px, count = 0;

#ifdef JP
				msg_print("召喚場所を指定して下さい。");
#else
				msg_print("Choose the place to summon.");
#endif
				if (!tgt_pt(&tx, &ty, TRUE)) return;
				if (!player_has_los_bold(ty, tx))
				{
#ifdef JP
					msg_print("その場所に召喚することはできません。");
#else
					msg_print("You can't summon on that place.");
#endif
					return;
				}
				for (k = 0; k < 6; k++)
				{
					if (summon_named_creature(-1, ty, tx, MON_PUMPKIN_HEAD, PM_FORCE_PET))
						count++;
				}
				if (count)
				{
#ifdef JP
					msg_print("パンプキンヘッドを召喚した。");
#else
					msg_print("You summon Pumpkin-Heads.");
#endif
				}
				else
				{
#ifdef JP
					msg_print("パンプキンヘッドは現れなかった。");
#else
					msg_print("No Pumpkin-Head arrive.");
#endif
				}
				o_ptr->timeout = randint0(200) + 400;
				break;
			}

			case ART_HABORYM_EYE:
			{
#ifdef JP
				msg_print("その剥製は赤く明るく光った！");
#else
				msg_print("The Jewel flashes bright red!");
#endif

				wiz_lite(FALSE);
				msg_print("その剥製はあなたの体力を奪った...");
				take_hit(DAMAGE_LOSELIFE, damroll(3, 8), "剣聖ハボリムの失われた眼");

				(void)detect_traps(DETECT_RAD_DEFAULT, TRUE);
				(void)detect_doors(DETECT_RAD_DEFAULT);
				(void)detect_stairs(DETECT_RAD_DEFAULT);

				o_ptr->timeout = randint0(20) + 20;
				break;
			}

			case ART_RESIST:
			{
#ifdef JP
				msg_print("アミュレットから鋭い音が流れ出た...");
#else
				msg_print("The amulet lets out a shrill wail...");
#endif

				k = 3 * p_ptr->lev;
				(void)set_protevil(randint1(25) + k, FALSE);
				o_ptr->timeout = randint0(225) + 225;
				break;
			}

			case ART_RODERIC:
			{
				switch (get_your_alignment_gne())
				{
				case ALIGN_GNE_GOOD:
#ifdef JP
					msg_print("アミュレットは辺りを聖なる力で満たした...");
#else
					msg_print("The amulet floods the area with goodness...");
#endif

					dispel_evil(p_ptr->lev * 5);
					break;

				case ALIGN_GNE_NEUTRAL:
#ifdef JP
					msg_print("アミュレットは辺りを力で満たした...");
#else
					msg_print("The amulet floods the area with power...");
#endif

					dispel_monsters(p_ptr->lev * 5);
					break;

				case ALIGN_GNE_EVIL:
#ifdef JP
					msg_print("アミュレットは辺りを邪悪なオーラで満たした...");
#else
					msg_print("The amulet floods the area with evil...");
#endif

					dispel_good(p_ptr->lev * 5);
					break;
				}
				o_ptr->timeout = randint0(200) + 200;
				break;
			}

			case ART_PRESANCE:
			{
#ifdef JP
				msg_print("メイスは辺りを善のオーラで満たした...");
#else
				msg_print("The mace floods the area with goodness...");
#endif

				dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = randint0(200) + 200;
				break;
			}

			case ART_TULKAS:
			{
#ifdef JP
				msg_print("指輪は明るく輝いた...");
#else
				msg_print("The ring glows brightly...");
#endif

				(void)set_fast(randint1(75) + 75, FALSE);
				o_ptr->timeout = randint0(150) + 150;
				break;
			}

			case ART_SACRED_RING:
			{
#ifdef JP
				msg_print("指輪から聖なる光が放たれた...");
#else
				msg_print("The ring wells with holy light...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_FIRE, dir, 150, 3, FALSE);
				o_ptr->timeout = randint0(225) + 225;
				break;
			}

			case ART_EVIL_RING:
			{
#ifdef JP
				msg_print("指輪は奈落の門を開いた...");
#else
				msg_print("The ring opens abyss gate...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HELL_FIRE, dir, 250, 3, FALSE);
				o_ptr->timeout = randint0(325) + 325;
				break;
			}

			case ART_DEMON_RING:
			{
#ifdef JP
				msg_print("指輪は闇の力を凝縮している...");
#else
				msg_print("The ring condenses dark force...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HELL_FIRE, dir, 999, 0, FALSE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_FIRECREST:
			{
#ifdef JP
				msg_print("宝玉は漆黒に輝いた...");
#else
				msg_print("The jewel glows intensely black...");
#endif

				if (!get_aim_dir(&dir)) return;
				ring_of_power(dir);
				o_ptr->timeout = randint0(450) + 450;
				break;
			}

			case ART_RUNGVIE:
			{
				int num = damroll(5, 3);
				int y, x;
				int attempts;

#ifdef JP
				msg_print("鎧が稲妻で覆われた...");
#else
				msg_print("Your armor is surrounded by lightning...");
#endif


				for (k = 0; k < num; k++)
				{
					attempts = 1000;

					while(attempts--)
					{
						scatter(&y, &x, py, px, 4, 0);

						if (!cave_floor_bold(y, x)) continue;

						if ((y != py) || (x != px)) break;
					}

					project(0, 3, y, x, 150, GF_ELEC,
							  (PROJECT_THRU | PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL), MODIFY_ELEM_MODE_MAGIC);
				}

				o_ptr->timeout = 1000;
				break;
			}

			case ART_BLADETURNER:
			{
				if (!get_aim_dir(&dir)) return;
				activate_bladeturner(dir);

#ifdef JP
				msg_print("鎧が様々な色に輝いた...");
#else
				msg_print("Your armor glows many colours...");
#endif

				(void)set_afraid(0);
				(void)set_hero(randint1(50) + 50, FALSE);
				(void)hp_player(10);
				(void)set_blessed(randint1(50) + 50, FALSE);
				(void)set_oppose_acid(randint1(50) + 50, FALSE);
				(void)set_oppose_elec(randint1(50) + 50, FALSE);
				(void)set_oppose_fire(randint1(50) + 50, FALSE);
				(void)set_oppose_cold(randint1(50) + 50, FALSE);
				(void)set_oppose_pois(randint1(50) + 50, FALSE);
				o_ptr->timeout = 400;
				break;
			}

			case ART_SOULKEEPER:
			{
#ifdef JP
				msg_print("鎧が白く明るく輝いた...");
				msg_print("ひじょうに気分がよい...");
#else
				msg_print("Your armor glows a bright white...");
				msg_print("You feel much better...");
#endif

				(void)hp_player(1000);
				(void)set_cut(0);
				o_ptr->timeout = 888;
				break;
			}

			case ART_LANCELOT_H:
			{
#ifdef JP
				msg_print("天国の歌が聞こえる...");
#else
				msg_print("A heavenly choir sings...");
#endif

				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_hero(randint1(25) + 25, FALSE);
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}

			case ART_DENIM:
			{
#ifdef JP
				msg_print("鎧が深いブルーに輝いた...");
#else
				msg_print("Your armor glows deep blue...");
#endif

				(void)symbol_genocide(200, TRUE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_LEVIATHAN:
			{
#ifdef JP
				msg_print("鎧が赤く明るく輝いた...");
#else
				msg_print("Your armor glows bright red...");
#endif

				destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}

			case ART_GARINGA:
			case ART_SKULL_MASK:
			{
				turn_monsters(40 + p_ptr->lev);
				o_ptr->timeout = 3 * (p_ptr->lev + 10);
				break;
			}

			case ART_RENDAL:
			{
#ifdef JP
				msg_print("帽子が白く明るく輝いた...");
				msg_print("心にイメージが浮かんできた...");
#else
				msg_print("Your hat glows bright white...");
				msg_print("An image forms in your mind...");
#endif

				detect_all(DETECT_RAD_DEFAULT);
				o_ptr->timeout = randint0(55) + 55;
				break;
			}

			case ART_DANIKA:
			{
#ifdef JP
				msg_print("王冠が深いブルーに輝いた...");
				msg_print("体内に暖かい鼓動が感じられる...");
#else
				msg_print("Your crown glows deep blue...");
				msg_print("You feel a warm tingling inside...");
#endif

				(void)hp_player(700);
				(void)set_cut(0);
				o_ptr->timeout = 250;
				break;
			}

			case ART_BELZBUTE:
			{
#ifdef JP
				msg_print("クロークが様々な色に輝いた...");
#else
				msg_print("Your cloak glows many colours...");
#endif

				(void)set_oppose_acid(randint1(20) + 20, FALSE);
				(void)set_oppose_elec(randint1(20) + 20, FALSE);
				(void)set_oppose_fire(randint1(20) + 20, FALSE);
				(void)set_oppose_cold(randint1(20) + 20, FALSE);
				(void)set_oppose_pois(randint1(20) + 20, FALSE);
				o_ptr->timeout = 111;
				break;
			}

			case ART_CLARE:
			{
#ifdef JP
				msg_print("クロークが深いブルーに輝いた...");
#else
				msg_print("Your cloak glows deep blue...");
#endif

				sleep_monsters_touch(p_ptr->lev);
				o_ptr->timeout = 55;
				break;
			}

			case ART_AISHA:
			{
#ifdef JP
				msg_print("クロークが白く明るく輝いた...");
#else
				msg_print("Your cloak glows bright white...");
#endif

				(void)set_poisoned(0);
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);
				(void)set_stoning(0);
				hp_player(80);
				o_ptr->timeout = randint0(50) + 50;
				break;
			}

			case ART_GRINCER_COAT:
			{
#ifdef JP
				msg_print("コートが辺りの空間をゆがませた...");
#else
				msg_print("Your coat twists space around you...");
#endif

				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_ALBELEO:
			{
#ifdef JP
				msg_print("クロークが深紅に輝いた...");
#else
				msg_print("Your cloak glows a deep red...");
#endif

				restore_level();
				o_ptr->timeout = 450;
				break;
			}

			case ART_GUACHARO:
			{
#ifdef JP
				msg_print("ガントレットが炎に覆われた...");
#else
				msg_print("Your gauntlets are covered in fire...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = randint0(8) + 8;
				break;
			}

			case ART_DEBARDES:
			{
#ifdef JP
				msg_print("ガントレットが冷気に覆われた...");
#else
				msg_print("Your gauntlets are covered in frost...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = randint0(7) + 7;
				break;
			}

			case ART_ERIG:
			{
#ifdef JP
				msg_print("セスタスに魔法のトゲが現れた...");
#else
				msg_print("Your cesti grows magical spikes...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_EDGED, dir, 150);
				o_ptr->timeout = randint0(90) + 90;
				break;
			}

			case ART_ROSHFEL:
			{
#ifdef JP
				msg_print("ブーツがグリーンに明るく輝いた...");
#else
				msg_print("Your boots glow bright green...");
#endif

				(void)set_fast(randint1(20) + 20, FALSE);
				o_ptr->timeout = 200;
				break;
			}

			case ART_NORN:
			{
#ifdef JP
				msg_print("ブーツが深いブルーに輝いた...");
#else
				msg_print("Your boots glow deep blue...");
#endif

				(void)set_afraid(0);
				(void)set_poisoned(0);
				o_ptr->timeout = 5;
				break;
			}

			case ART_FREUDE_HELM:
			{
				dispel_evil(100);
				(void)set_protevil(randint1(25) + 50, FALSE);
				o_ptr->timeout = 200;
				break;
			}

			case ART_SARA:
			{
#ifdef JP
				msg_print("ダガーが火花に覆われた...");
#else
				msg_print("Your dagger is covered in sparks...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = randint0(6) + 6;
				break;
			}

			case ART_BERSALIA:
			{
#ifdef JP
				msg_print("ダガーが冷気に覆われた...");
#else
				msg_print("Your dagger is covered in frost...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 48, 2, FALSE);
				o_ptr->timeout = randint0(5) + 5;
				break;
			}

			case ART_ZENOBIA:
			{
				(void)set_afraid(0);
				(void)set_hero(randint1(25) + 25, FALSE);
				(void)hp_player(10);
				(void)set_blessed(randint1(50) + 50, FALSE);
				(void)set_fast(randint1(75) + 75, FALSE);
				o_ptr->timeout = randint0(50) + 100;
				break;
			}

			case ART_SISTEENA:
			{
				switch (randint1(13))
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
#ifdef JP
					if (get_check("この階を去りますか？"))
#else
					if (get_check("Leave this level? "))
#endif

					{
						if (autosave_l) do_cmd_save_game(TRUE);

						/* Leaving */
						p_ptr->leaving = TRUE;
					}
				}
				o_ptr->timeout = 35;
				break;
			}

			case ART_SONIC_BLADE:
			{
#ifdef JP
				msg_print("剣が青く激しく輝いた...");
#else
				msg_print("Your sword glows an intense blue...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2, FALSE);
				o_ptr->timeout = 300;
				break;
			}

			case ART_BRUNHILD:
			{
				cave_type *c_ptr = &cave[py][px];

				if (astral_mode)
				{
#ifdef JP
					msg_print("何も起こらなかった。");
#else
					msg_print("Nothing happens.");
#endif
					return;
				}

				if (dungeon_type == DUNGEON_HEAVEN)
				{
					if (quest[QUEST_FILARHH].status == QUEST_STATUS_FINISHED)
					{
						if (!get_check("天界への道へ帰還します。よろしいですか？")) return;

						dungeon_type = DUNGEON_HEAVEN_WAY;
						dun_level = d_info[dungeon_type].maxdepth;

						msg_format("…あなたは天界への道%d階に降り立った。", dun_level);

						/* Save player position */
						p_ptr->oldpx = px;
						p_ptr->oldpy = py;

						if (record_stair)
						{
							char buf[160];
							sprintf(buf, "カオスゲートを通って%sの%d階に移動した。", d_name+d_info[dungeon_type].name, dun_level);
							do_cmd_write_nikki(NIKKI_RECALL, 0, buf);
						}

						back_from_heaven = TRUE;
						p_ptr->leaving = TRUE;
					}
					else
					{
						msg_print("ブリュンヒルドは力を失っている。");
					}
				}
				else if (IN_HEAVEN_GATE() && (c_ptr->mimic == FEAT_FLOOR) &&
					((c_ptr->feat == FEAT_GRASS) || (c_ptr->feat == FEAT_DEEP_GRASS)))
				{
					int old_inside_quest = p_ptr->inside_quest;
					int i;
					quest_type *q_ptr;

					if (!get_check("カオスゲートが開いた！天界へ侵入しますか？")) return;
					msg_print("あなたは天界へ降り立った…。");

					for (i = QUEST_FELLANA; i <= QUEST_FILARHH; i++)
					{
						q_ptr = &quest[i];

						if (q_ptr->status == QUEST_STATUS_UNTAKEN)
						{
							/* Init the heaven quest */
							init_flags = INIT_ASSIGN;
							p_ptr->inside_quest = i;

							process_dungeon_file("q_info.txt", 0, 0, 0, 0);

							quest[i].status = QUEST_STATUS_TAKEN;
						}
					}

					p_ptr->inside_quest = old_inside_quest;

					dungeon_type = DUNGEON_HEAVEN;
					dun_level = d_info[dungeon_type].mindepth;

					/* Save player position */
					p_ptr->oldpx = px;
					p_ptr->oldpy = py;

					if (record_stair)
					{
						char buf[160];
						sprintf(buf, "カオスゲートを通って%sの%d階に移動した。", d_name+d_info[dungeon_type].name, dun_level);
						do_cmd_write_nikki(NIKKI_RECALL, 0, buf);
					}

					prepare_change_floor_mode(CFM_RAND_PLACE);

					p_ptr->leaving = TRUE;
				}
				else
				{
					msg_print("この剣は天界への門の正しい位置で発動しなければならない。");
				}

				break;
			}

			case ART_BOREAS:
			{
#ifdef JP
				 msg_print("斧の刃が黒く輝いた...");
#else
				msg_print("Your axe blade glows black...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 200, 2, FALSE);
				(void)set_oppose_elec(randint1(20) + 20, FALSE);
				o_ptr->timeout = 300;
				break;
			}

			case ART_OSRIC:
			{
				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_COLD, dir, 100);
				o_ptr->timeout = randint0(100) + 100;
				break;
			}

			case ART_GUNGNIR:
			{
#ifdef JP
				msg_print("あなたの槍は電気でスパークしている...");
#else
				msg_print("Your spear crackles with electricity...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 100, 3, FALSE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_EVIL_SPEAR:
			{
#ifdef JP
				msg_print("スピアが鼓動した...");
#else
				msg_print("Your spear pulsates...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_NEW_DRAIN, dir, damroll(10, 25));
				o_ptr->timeout = 200;
				break;
			}

			case ART_GYPSY_QUEEN:
			{
#ifdef JP
				msg_print("斧からひどく鋭い音が流れ出た...");
#else
				msg_print("Your axe lets out a long, shrill note...");
#endif

				(void)mass_genocide(200, TRUE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_GRAMLOCK:
			{
#ifdef JP
				msg_print("斧が深紫の光を放射した...");
#else
				msg_print("Your battle axe radiates deep purple...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 200, 3, FALSE);
				o_ptr->timeout = 200;
				break;
			}

			case ART_BENTISCA:
			{
#ifdef JP
				msg_print("トライデントが深紅に輝いた...");
#else
				msg_print("Your trident glows deep red...");
#endif

				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

			case ART_SATANS_BULLOVA:
			{
				msg_print("周囲が冥界の空気に変わる...");

				project_hack_living(GF_NETHER, 200);
				project_hack_living(GF_OLD_SLEEP, 200);
				o_ptr->timeout = randint0(200) + 500;
				break;
			}

			case ART_SAVAGE:
			{
				msg_print("天候が荒れていく...");

				set_weather(8, 8, 8);
				o_ptr->timeout = 800;
				break;
			}

			case ART_RAPTURE_ROSE:
			{
#ifdef JP
				msg_print("モーニングスターから炎が吹き出した...");
#else
				msg_print("Your morning star rages in fire...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 72, 3, FALSE);
				o_ptr->timeout = 100;
				break;
			}

			case ART_ZADOVA:
			{
#ifdef JP
				msg_print("凍るような眼で周囲を眺め回した...");
#else
				msg_print("You look around with freezing eyes...");
#endif
				(void)stone_gaze(0);
				o_ptr->timeout = randint0(100) + 200;
				break;
			}

			case ART_BAIAN:
			{
#ifdef JP
				msg_print("杖が黄色く輝いた...");
#else
				msg_print("Your quarterstaff glows yellow...");
#endif

				if (!ident_spell(FALSE)) return;
				o_ptr->timeout = 10;
				break;
			}

			case ART_WARREN:
			{
#ifdef JP
				msg_print("杖が明るく輝いた...");
#else
				msg_print("Your quarterstaff glows brightly...");
#endif

				detect_all(DETECT_RAD_DEFAULT);
				probing();
				identify_fully(FALSE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_TURMIL:
			{
#ifdef JP
				msg_print("ハンマーが白く輝いた...");
#else
				msg_print("Your hammer glows white...");
#endif

				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				o_ptr->timeout = 70;
				break;
			}

			case ART_EUROS:
			{
				(void)set_fast(randint1(50) + 50, FALSE);
				hp_player(10);
				set_afraid(0);
				set_hero(randint1(50) + 50, FALSE);
				o_ptr->timeout = randint0(200) + 100;
				break;
			}

			case ART_ZEPHYRUS:
			{
#ifdef JP
				msg_print("槍先が深い青色に鼓動している...");
#else
				msg_print("Your spear throbs deep blue...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 200, 3, FALSE);
				o_ptr->timeout = 250;
				break;
			}

			case ART_RED:
			{
#ifdef JP
				msg_print("あなたの敵を恐怖させた！");
#else
				msg_print("You wind a mighty blast; your enemies tremble!");
#endif
				(void)turn_monsters((3 * p_ptr->lev / 2) + 10);
				o_ptr->timeout = randint0(40) + 40;
				break;
			}

			case ART_BLUE:
			{
#ifdef JP
				msg_print("あなたは害虫を一掃した。");
#else
				msg_print("You exterminate small life.");
#endif
				(void)dispel_monsters(4);
				o_ptr->timeout = randint0(55) + 55;
				break;
			}

			case ART_YENDOR:
			{
#ifdef JP
				msg_print("カードが白く輝いた．．．");
#else
				msg_print("Your card gleams with blinding light...");
#endif
				if (!recharge(1000)) return;
				o_ptr->timeout = 200;
				break;
			}

			case ART_HOLY_LANCE:
			{
#ifdef JP
				msg_print("スピアが白く明るく輝いた...");
#else
				msg_print("Your spear glows a bright white...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_beam(GF_HOLY_FIRE, dir, 150);
				o_ptr->timeout = 200;
				break;
			}

			case ART_IGNIS:
			{
#ifdef JP
				msg_print("スピアが深い赤色に輝いた...");
#else
				msg_print("Your spear glows deep red...");
#endif

				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 120, 3, FALSE);
				o_ptr->timeout = 15;
				break;
			}

			case ART_ANGELIC_ARMOR:
			{
#ifdef JP
				msg_print("鈍い音が辺りを包みこんだ。");
#else
				msg_print("A shrill wailing sound surrounds you.");
#endif
				(void)set_protevil(randint1(25) + p_ptr->lev, FALSE);
				o_ptr->timeout = randint0(200) + 200;
				break;
			}

			case ART_RIPPLES_STAFF:
			{
#ifdef JP
				msg_print("杖が純粋な魔力で震えた。");
#else
				msg_print("The staff pulsates with raw mana...");
#endif
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MANA, dir, 120);
				o_ptr->timeout = randint0(20) + 20;
				break;
			}

			case ART_BLOOD_WHIP:
			{
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 90);
				o_ptr->timeout = 70;
				break;
			}

			case ART_CORAL:
			{
				msg_print("天候が穏やかになっていく...");

				set_weather(-8, -8, -8);
				o_ptr->timeout = 800;
				break;
			}

			case ART_COMET:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_HOLY_FIRE, dir, 75, 2, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			}

			case ART_MARITZA:
			{
#ifdef JP
				msg_print("杖から魔力が流れ込んでくる。");
#else
				msg_print("nanka.");
#endif
				(void)set_chargespell(randint1(25) + 50, FALSE);
				o_ptr->timeout = 200;
				break;
			}

			case ART_OGRE_BLADE:
			{
				snap_dragon();
				break;
			}

			case ART_LIFE_STAFF:
			{
				msg_print("杖が優しい光に包まれた。");
				hp_player(80);
				o_ptr->timeout = 30;
				break;
			}

			case ART_CLEAR_STAFF:
			{
				msg_print("杖が清浄な光に包まれた。");
				(void)set_poisoned(0);
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_stoning(0);
				o_ptr->timeout = 10;
				break;
			}

			case ART_FAKE:
			{
				int count = 0;
				msg_print("その剥製は赤く明るく光った！");
				msg_print("その剥製はあなたの体力を奪った...");
				take_hit(DAMAGE_LOSELIFE, damroll(3, 8), "暗黒騎士ランスロットの失われた眼");
				(void)activate_ty_curse(FALSE, &count);
				break;
			}

			case ART_NIGHT:
			{
#ifdef JP
				msg_print("アミュレットが深い闇に覆われた...");
#else
				msg_print("Your amulet is coverd in pitch-darkness...");
#endif
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_DARK, dir, 250, 4, FALSE);
				o_ptr->timeout = randint0(150) + 150;
				break;
			}

			case ART_FIREBIRD:
			{
#ifdef JP
				msg_print("魔獣を手なずける力を放つ...");
#else
				msg_print("You activate power to tame a beast...");
#endif
				if (!get_aim_dir(&dir)) return;
				charm_beast(dir, 150);
				o_ptr->timeout = 400;
				break;
			}

			case ART_CAMERA:
			{
				if (!get_aim_dir(&dir)) return;
				project_length = 1;
				fire_beam(GF_PHOTO, dir, 1);
				break;
			}

			case ART_VOLUPTUOUS:
			{
				if (p_ptr->psex != SEX_FEMALE)
				{
#ifdef JP
					msg_print("女性でないと発動できません。");
#else
					msg_print("Activation is for ladies only.");
#endif
					return;
				}
#ifdef JP
				msg_print("周囲の男性を振り向かせる...");
#else
				msg_print("You activate power to charm all males...");
#endif
				charm_males(800);
				o_ptr->timeout = 20;
				break;
			}

			case ART_LIGHTNING_BOW:
			{
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PURE_WIND, dir, 200, 0, FALSE);
				o_ptr->timeout = 200;
				break;
			}

			case ART_AMU_FIRE:
			{
#ifdef JP
				msg_print("アミュレットが赤い光に包まれた...");
#else
				msg_print("Your amulet is coverd in red light...");
#endif

				inc_area_elem(0, ELEM_FIRE, 99, 5, FALSE);

				(void)summon_god(GF_PURE_FIRE, randint1(vit * 2) + vit * 5);
				o_ptr->timeout = 300;
				break;
			}

			case ART_AMU_AQUA:
			{
#ifdef JP
				msg_print("アミュレットが青い光に包まれた...");
#else
				msg_print("Your amulet is coverd in blue light...");
#endif

				inc_area_elem(0, ELEM_AQUA, 99, 5, FALSE);

				(void)summon_god(GF_PURE_AQUA, randint1(vit * 2) + vit * 5);
				o_ptr->timeout = 300;
				break;
			}

			case ART_AMU_EARTH:
			{
#ifdef JP
				msg_print("アミュレットが黄色い光に包まれた...");
#else
				msg_print("Your amulet is coverd in yellow light...");
#endif

				inc_area_elem(0, ELEM_EARTH, 99, 5, FALSE);

				(void)summon_god(GF_PURE_EARTH, randint1(vit * 2) + vit * 5);
				o_ptr->timeout = 300;
				break;
			}

			case ART_AMU_WIND:
			{
#ifdef JP
				msg_print("アミュレットが緑の光に包まれた...");
#else
				msg_print("Your amulet is coverd in green light...");
#endif

				inc_area_elem(0, ELEM_WIND, 99, 5, FALSE);

				(void)summon_god(GF_PURE_WIND, randint1(vit * 2) + vit * 5);
				o_ptr->timeout = 300;
				break;
			}

		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}


	if (o_ptr->name2 == EGO_WARP)
	{
		if (!dimension_door(p_ptr->lev)) return;
		o_ptr->timeout = 50 + randint1(25);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}


	if (o_ptr->name2 == EGO_SHOOTING_STAR)
	{
		if ((item != INVEN_RARM) && (item != INVEN_LARM)) return;

		if (!do_cmd_throw_aux(10, (PY_THROW_CHOSEN | PY_THROW_SHOOTING_STAR), item)) return;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}


	if (o_ptr->name2 == EGO_EARTHQUAKES)
	{
		earthquake(py, px, 5);
		o_ptr->timeout = 100 + randint1(100);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}


	if (o_ptr->name2 == EGO_LITE_ILLUMINATION)
	{
		if (!o_ptr->xtra4 && ((o_ptr->sval == SV_LITE_TORCH) || (o_ptr->sval == SV_LITE_LANTERN)))
		{
#ifdef JP
			msg_print("燃料がない。");
#else
			msg_print("It has no fuel.");
#endif
			energy_use = 0;
			return;
		}
		lite_area(damroll(2, 15), 3);
		o_ptr->timeout = randint0(10) + 10;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		return;
	}


	else if (o_ptr->tval == TV_RING)
	{
		if (o_ptr->name2)
		{
			bool success = TRUE;

			switch (o_ptr->name2)
			{
			case EGO_RING_HERO:
				(void)set_afraid(0);
				(void)set_hero(randint1(25) + 25, FALSE);
				(void)hp_player(10);
				o_ptr->timeout = randint1(100)+100;
				break;
			case EGO_RING_MAGIC_MIS:
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6));
				o_ptr->timeout = 2;
				break;
			case EGO_RING_FIRE_BOLT:
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(9, 8));
				o_ptr->timeout = randint0(8) + 8;
				break;
			case EGO_RING_COLD_BOLT:
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(6, 8));
				o_ptr->timeout = randint0(7) + 7;
				break;
			case EGO_RING_ELEC_BOLT:
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(4, 8));
				o_ptr->timeout = randint0(5) + 5;
				break;
			case EGO_RING_ACID_BOLT:
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(5, 8));
				o_ptr->timeout = randint0(6) + 6;
				break;
			case EGO_RING_MANA_BOLT:
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MANA, dir, 120);
				o_ptr->timeout = randint0(120)+120;
				break;
			case EGO_RING_FIRE_BALL:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 100, 2, FALSE);
				o_ptr->timeout = randint0(80) + 80;
				break;
			case EGO_RING_COLD_BALL:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 100, 2, FALSE);
				o_ptr->timeout = randint0(80) + 80;
				break;
			case EGO_RING_ELEC_BALL:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 100, 2, FALSE);
				o_ptr->timeout = randint0(80) + 80;
				break;
			case EGO_RING_ACID_BALL:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir, 100, 2, FALSE);
				o_ptr->timeout = randint0(80) + 80;
				break;
			case EGO_RING_MANA_BALL:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_MANA, dir, 250, 2, FALSE);
				o_ptr->timeout = 300;
				break;
			case EGO_RING_DRAGON_F:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PURE_FIRE, dir, 200, -2, FALSE);
				if (o_ptr->sval == SV_RING_FLAMES)
				{
					(void)set_oppose_fire(randint1(20) + 20, FALSE);
					o_ptr->timeout = 200;
				}
				else o_ptr->timeout = 250;
				break;
			case EGO_RING_DRAGON_C:
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_PURE_AQUA, dir, 200, -2, FALSE);
				if (o_ptr->sval == SV_RING_ICE)
				{
					(void)set_oppose_cold(randint1(20) + 20, FALSE);
					o_ptr->timeout = 200;
				}
				else o_ptr->timeout = 250;
				break;
			case EGO_RING_M_DETECT:
				(void)detect_monsters_invis(255);
				(void)detect_monsters_normal(255);
				o_ptr->timeout = 150;
				break;
			case EGO_RING_D_SPEED:
				(void)set_fast(randint1(30) + 15, FALSE);
				o_ptr->timeout = 100;
				break;
			case EGO_RING_BERSERKER:
				(void)set_shero(randint1(25) + 25, FALSE);
				o_ptr->timeout = randint0(75)+75;
				break;
			case EGO_RING_TELE_AWAY:
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			default:
				success = FALSE;
				break;
			}
			if (success) return;
		}

		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

		switch (o_ptr->sval)
		{
			case SV_RING_ACID:
			{
				fire_ball(GF_ACID, dir, 100, 2, FALSE);
				(void)set_oppose_acid(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			}

			case SV_RING_ICE:
			{
				fire_ball(GF_COLD, dir, 100, 2, FALSE);
				(void)set_oppose_cold(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			}

			case SV_RING_FLAMES:
			{
				fire_ball(GF_FIRE, dir, 100, 2, FALSE);
				(void)set_oppose_fire(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			}

			case SV_RING_ELEC:
			{
				fire_ball(GF_ELEC, dir, 100, 2, FALSE);
				(void)set_oppose_elec(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	else if (o_ptr->tval == TV_AMULET)
	{
		if (o_ptr->name2)
		{
			switch (o_ptr->name2)
			{
			case EGO_AMU_IDENT:
				if (!ident_spell(FALSE)) return;
				o_ptr->timeout = 10;
				break;
			case EGO_AMU_CHARM:
				if (!get_aim_dir(&dir)) return;
				charm_monster(dir, MAX(20, p_ptr->lev));
				o_ptr->timeout = 200;
				break;
			case EGO_AMU_JUMP:
				teleport_player(10);
				o_ptr->timeout = randint0(10) + 10;
				break;
			case EGO_AMU_TELEPORT:
				teleport_player(100);
				o_ptr->timeout = randint0(50) + 50;
				break;
			case EGO_AMU_D_DOOR:
				if (!dimension_door(p_ptr->lev)) return;
				o_ptr->timeout = 200;
				break;
			case EGO_AMU_RES_FIRE_:
				(void)set_oppose_fire(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			case EGO_AMU_RES_COLD_:
				(void)set_oppose_cold(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			case EGO_AMU_RES_ELEC_:
				(void)set_oppose_elec(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			case EGO_AMU_RES_ACID_:
				(void)set_oppose_acid(randint1(20) + 20, FALSE);
				o_ptr->timeout = randint0(50) + 50;
				break;
			case EGO_AMU_DETECTION:
				detect_all(DETECT_RAD_DEFAULT);
				o_ptr->timeout = randint0(55)+55;
				break;
			}
		}
		else
		{
			switch (o_ptr->sval)
			{
				case SV_AMULET_FOL:
				{
#ifdef JP
					if (get_check("ペンダントの力を使いますか?"))
#else
					if (get_check("Do you want to use power of necklace ?"))
#endif
					{
						inc_area_elem(0, ELEM_FIRE, 99, 5, FALSE);

						inven_item_increase(item, -1);
						inven_item_optimize(item);
#ifdef JP
						msg_print("ペンダントは砕け散った。");
#else
						msg_print("The Necklace has go to pieces.");
#endif
					}
				}
				break;

				case SV_AMULET_OHN:
				{
#ifdef JP
					if (get_check("ペンダントの力を使いますか?"))
#else
					if (get_check("Do you want to use power of necklace ?"))
#endif
					{
						inc_area_elem(0, ELEM_AQUA, 99, 5, FALSE);

						inven_item_increase(item, -1);
						inven_item_optimize(item);
#ifdef JP
						msg_print("ペンダントは砕け散った。");
#else
						msg_print("The Necklace has go to pieces.");
#endif
					}
				}
				break;

				case SV_AMULET_SOL:
				{
#ifdef JP
					if (get_check("ペンダントの力を使いますか?"))
#else
					if (get_check("Do you want to use power of necklace ?"))
#endif
					{
						inc_area_elem(0, ELEM_EARTH, 99, 5, FALSE);

						inven_item_increase(item, -1);
						inven_item_optimize(item);
#ifdef JP
						msg_print("ペンダントは砕け散った。");
#else
						msg_print("The Necklace has go to pieces.");
#endif
					}
				}
				break;

				case SV_AMULET_VAN:
				{
#ifdef JP
					if (get_check("ペンダントの力を使いますか?"))
#else
					if (get_check("Do you want to use power of necklace ?"))
#endif
					{
						inc_area_elem(0, ELEM_WIND, 99, 5, FALSE);

						inven_item_increase(item, -1);
						inven_item_optimize(item);
#ifdef JP
						msg_print("ペンダントは砕け散った。");
#else
						msg_print("The Necklace has go to pieces.");
#endif
					}
				}
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	else if (o_ptr->tval == TV_STONE)
	{
		if (!dun_level)
		{
			if (one_in_(7))
			{
				int y, x;

				for (y = 0; y < max_wild_y; y++)
				{
					for (x = 0; x < max_wild_x; x++)
					{
						if (wilderness[y][x].town == 1)
						{
							p_ptr->wilderness_y = y;
							p_ptr->wilderness_x = x;
						}
					}
				}
				p_ptr->leaving = TRUE;
				p_ptr->teleport_town = TRUE;
			}
			else if (one_in_(5))
			{
				teleport_player(222);
			}
			else
			{
				(void)teleport_level(0);
			}
		}
		else
		{
			if (p_ptr->inside_arena || astral_mode || (d_info[dungeon_type].flags1 & DF1_CLOSED) ||
				(p_ptr->inside_quest && (quest[p_ptr->inside_quest].flags & QUEST_FLAG_NO_RECALL)))
			{
#ifdef JP
				msg_print("何も起こらなかった。");
#else
				msg_print("Nothing happens.");
#endif
			}
			else
				p_ptr->word_recall = 1;
		}

		inven_item_increase(item, -1);
		inven_item_optimize(item);
#ifdef JP
		msg_print("転移石は消え去った。");
#else
		msg_print("The Transferring Stone has vanished.");
#endif

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	else if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_MAGICAL_LAMP))
	{
		bool happen = FALSE;

		switch (randint1(10))
		{
		case 1: case 2: case 3: case 4: case 5: case 6:
			if (summon_named_creature(-1, py, px, MON_DJINNI, PM_FORCE_FRIENDLY | PM_IGNORE_AMGRID))
			{
				set_friendly(&m_list[hack_m_idx_ii]);
				msg_print("風のジンがあらわれた！願いを一つ叶えてくれる！");
				while (1)
				{
					if (wish_object("Whats? ")) break;
				}
				happen = TRUE;
			}
			break;

		case 7: case 8: case 9:
			if (summon_named_creature(0, py, px, MON_DJINNI, PM_NO_PET | PM_IGNORE_AMGRID))
			{
				msg_print("風のジンがあらわれた！彼は怒っているようだ！");
				happen = TRUE;
			}
			break;

		default:
			break;
		}

		if (!happen)
		{
			msg_print("あなたはランプを必死にこすったが、何も起こらなかった。");
		}

		inven_item_increase(item, -1);
		inven_item_optimize(item);
#ifdef JP
		msg_print("ランプは音を立てて崩れ去った。");
#else
		msg_print("The Magical Lamp is broken with sound.");
#endif

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	else if (o_ptr->tval == TV_TRUMP)
	{
		if(!o_ptr->pval)
		{
			bool old_target_pet = target_pet;
			target_pet = TRUE;
			if (!get_aim_dir(&dir))
			{
				target_pet = old_target_pet;
				return;
			}
			target_pet = old_target_pet;

			if(fire_ball(GF_CAPTURE, dir, 0, 0, FALSE))
			{
				o_ptr->pval = cap_mon;
				o_ptr->xtra3 = cap_mspeed;
				o_ptr->xtra4 = cap_hp;
				o_ptr->xtra5 = cap_maxhp;
				if (cap_nickname)
				{
					cptr t;
					char *s;
					char buf[80] = "";

					if (o_ptr->inscription)
						strcpy(buf, quark_str(o_ptr->inscription));
					s = buf;
					for (s = buf;*s && (*s != '#'); s++)
					{
#ifdef JP
						if (iskanji(*s)) s++;
#endif
					}
					*s = '#';
					s++;
#ifdef JP
 /*nothing*/
#else
					*s++ = '\'';
#endif
					t = quark_str(cap_nickname);
					while (*t)
					{
						*s = *t;
						s++;
						t++;
					}
#ifdef JP
 /*nothing*/
#else
					*s++ = '\'';
#endif
					*s = '\0';
					o_ptr->inscription = quark_add(buf);
				}
			}
		}
		else
		{
			bool success = FALSE;
			if (!get_rep_dir2(&dir)) return;
			if (monster_can_enter(py + ddy[dir], px + ddx[dir], &r_info[o_ptr->pval]))
			{
				if (place_monster_aux(0, py + ddy[dir], px + ddx[dir], o_ptr->pval, (PM_FORCE_PET)))
				{
					if (o_ptr->xtra3) m_list[hack_m_idx_ii].mspeed = o_ptr->xtra3;
					if (o_ptr->xtra5) m_list[hack_m_idx_ii].max_maxhp = o_ptr->xtra5;
					if (o_ptr->xtra4) m_list[hack_m_idx_ii].hp = o_ptr->xtra4;
					m_list[hack_m_idx_ii].maxhp = m_list[hack_m_idx_ii].max_maxhp;
					if (o_ptr->inscription)
					{
						char buf[80];
						cptr t;
#ifndef JP
						bool quote = FALSE;
#endif

						t = quark_str(o_ptr->inscription);
						for (t = quark_str(o_ptr->inscription);*t && (*t != '#'); t++)
						{
#ifdef JP
							if (iskanji(*t)) t++;
#endif
						}
						if (*t)
						{
							char *s = buf;
							t++;
#ifdef JP
							/* nothing */
#else
							if (*t =='\'')
							{
								t++;
								quote = TRUE;
							}
#endif
							while(*t)
							{
								*s = *t;
								t++;
								s++;
							}
#ifdef JP
							/* nothing */
#else
							if (quote && *(s-1) =='\'')
								s--;
#endif
							*s = '\0';
							m_list[hack_m_idx_ii].nickname = quark_add(buf);
							t = quark_str(o_ptr->inscription);
							s = buf;
							while(*t && (*t != '#'))
							{
								*s = *t;
								t++;
								s++;
							}
							*s = '\0';
							o_ptr->inscription = quark_add(buf);
						}
					}
					o_ptr->pval = 0;
					o_ptr->xtra3 = 0;
					o_ptr->xtra4 = 0;
					o_ptr->xtra5 = 0;
					success = TRUE;
				}
			}
			if (!success)
#ifdef JP
				msg_print("おっと、解放に失敗した。");
#else
				msg_print("Oops.  You failed to release your pet.");
#endif
		}
		return;
	}

	/* Mistake */
#ifdef JP
	msg_print("おっと、このアイテムは始動できない。");
#else
	msg_print("Oops.  That object cannot be activated.");
#endif

}


void do_cmd_activate(void)
{
	int     item;
	cptr    q, s;


	item_tester_no_ryoute = TRUE;
	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item */
#ifdef JP
	q = "どのアイテムを始動させますか? ";
	s = "始動できるアイテムを装備していない。";
#else
	q = "Activate which item? ";
	s = "You have nothing to activate.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP))) return;

	/* Activate the item */
	do_cmd_activate_aux(item);
}


/*
 * Hook to determine if an object is useable
 */
static bool item_tester_hook_use(object_type *o_ptr)
{
	u32b flgs[TR_FLAG_SIZE];

	/* Ammo */
	if (o_ptr->tval == p_ptr->tval_ammo)
		return (TRUE);

	/* Useable object */
	switch (o_ptr->tval)
	{
		case TV_SPIKE:
		case TV_STAFF:
		case TV_WAND:
		case TV_ROD:
		case TV_SCROLL:
		case TV_POTION:
		case TV_FOOD:
		{
			return (TRUE);
		}

		default:
		{
			int i;

			/* Not known */
			if (!object_known_p(o_ptr)) return (FALSE);

			/* HACK - only items from the equipment can be activated */
			for (i = INVEN_RARM; i < INVEN_TOTAL; i++)
			{
				if (&inventory[i] == o_ptr)
				{
					/* Extract the flags */
					object_flags(o_ptr, flgs);

					/* Check activation flag */
					if (have_flag(flgs, TR_ACTIVATE)) return (TRUE);
				}
			}
		}
	}

	/* Assume not */
	return (FALSE);
}


/*
 * Use an item
 * XXX - Add actions for other item types
 */
void do_cmd_use(void)
{
	int         item;
	object_type *o_ptr;
	cptr        q, s;

	item_tester_no_ryoute = TRUE;
	/* Prepare the hook */
	item_tester_hook = item_tester_hook_use;

	/* Get an item */
#ifdef JP
q = "どれを使いますか？";
s = "使えるものがありません。";
#else
	q = "Use which item? ";
	s = "You have nothing to use.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP | USE_FLOOR))) return;

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

	switch (o_ptr->tval)
	{
		/* Spike a door */
		case TV_SPIKE:
		{
			do_cmd_spike();
			break;
		}

		/* Eat some food */
		case TV_FOOD:
		{
			do_cmd_eat_food_aux(item);
			break;
		}

		/* Aim a wand */
		case TV_WAND:
		{
			do_cmd_aim_wand_aux(item);
			break;
		}

		/* Use a staff */
		case TV_STAFF:
		{
			do_cmd_use_staff_aux(item);
			break;
		}

		/* Zap a rod */
		case TV_ROD:
		{
			do_cmd_zap_rod_aux(item);
			break;
		}

		/* Quaff a potion */
		case TV_POTION:
		{
			do_cmd_quaff_potion_aux(item);
			break;
		}

		/* Read a scroll */
		case TV_SCROLL:
		case TV_TAROT:
		case TV_SCRATCH_CARD:
		{
			/* Check some conditions */
			if (p_ptr->blind)
			{
#ifdef JP
				msg_print("目が見えない。");
#else
				msg_print("You can't see anything.");
#endif

				return;
			}
			if (no_lite())
			{
#ifdef JP
				msg_print("明かりがないので、暗くて読めない。");
#else
				msg_print("You have no light to read by.");
#endif

				return;
			}
			if (p_ptr->confused)
			{
#ifdef JP
				msg_print("混乱していて読めない！");
#else
				msg_print("You are too confused!");
#endif

				return;
			}

		  do_cmd_read_scroll_aux(item, object_aware_p(o_ptr));
		  break;
		}

		/* Fire ammo */
		case TV_BULLET:
		case TV_ROUND:
		case TV_SHELL:
		case TV_ROCKET:
		case TV_ARROW:
		case TV_BOLT:
		{
			(void)do_cmd_fire_aux(item, &inventory[INVEN_BOW], DCFA_NONE, 0, 0, 0, FALSE);
			break;
		}

		/* Activate an artifact */
		default:
		{
			do_cmd_activate_aux(item);
			break;
		}
	}
}
