/* File: dungeon.c */

/*
 * Regenerate hitpoints and mana.  Handle the passage of time.  Enter
 * wizard, debug, and borg modes.  Process commands.  Handle a character's
 * turn.  Interact with the current dungeon level.  Play a game.
 *
 * Copyright (c) 1998 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"



/*
 * Regenerate hit points
 *
 * The basic rate of hitpoint recovery is 30/10000ths per hitpoint, plus
 * 250/10000.  Therefore, a character with 100 maximum hitpoints will
 * recover (30 * 100) + 250/ 10000 = 3250 / 10000, or about a third of a
 * hitpoint every 10 game turns under normal conditions.
 *
 * By comparison, most monsters with 100 maximum hitpoints regain 1 hit-
 * point every 100 game turns (some monsters regenerate at twice this).
 */
static void regenhp(int amount)
{
	s32b temp;

	/* Save the old hitpoints */
	int old_chp = p_ptr->chp;


	/* Multiply max HP by "amount", then add the base regeneration */
	temp = (long)p_ptr->mhp * amount + PY_REGEN_HPBASE;

	/* Add stored fractional regeneration */
	temp += p_ptr->chp_frac;

	/* Additional HPs are 1/10000th of the result */
	p_ptr->chp += temp / 10000;

	/* Store any fractional regeneration */
	p_ptr->chp_frac = temp % 10000;


	/* Fully healed */
	if (p_ptr->chp >= p_ptr->mhp)
	{
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;
	}

	/* Notice changes */
	if (old_chp != p_ptr->chp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}


/*
 * Regenerate mana points
 */
static void regenmana(int amount)
{
	s32b temp;

	/* Save the old hitpoints */
	int old_csp = p_ptr->csp;


	/* Multiply max mana by "amount", then add the base regeneration */
	temp = (long)p_ptr->msp * amount + PY_REGEN_MNBASE;

	/* Add stored fractional regeneration */
	temp += p_ptr->csp_frac;

	/* Additional mana points are 1/10000th of the result */
	p_ptr->csp += temp / 10000;

	/* Store any fractional regeneration */
	p_ptr->csp_frac = temp % 10000;


	/* Fully healed */
	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	/* Redraw mana */
	if (old_csp != p_ptr->csp)
	{
		/* Redraw */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}
}

/*
 * As certain events occur, the character is told parts of a story.
 *
 * At present this is very simple, but this may change...
 */
void tell_story(int part)
{
	int old_rows = screen_rows;

	/* Save the screen */
	screen_save();

	/* Clear the screen */
	clear_from(0);

	/* Set to 25 rows */
	Term_rows(FALSE);

	/* Skip some lines */
	move_cursor(4, 0);

	/* We have just slain Sauron */
	if (part == MON_SAURON)
	{
		/* Display some text */
		c_roff_centered(TERM_WHITE, "As Sauron falls, a greater evil is unmasked.  For the Creator of the One Ring, Sauron the Great, was but the servant of another:\n\nMorgoth, Lord of Darkness awaits you!", 10, 70);
	}

	/* We have just slain Morgoth */
	else if (part == MON_MORGOTH)
	{
		char buf[120];

		/* Get a racial adjective */
		cptr race_adj = race_info[p_ptr->prace].title;

		/* Get a character title (never "winner") */
		cptr title_str = get_title(80, FALSE);

		/* Skip past quotes marker */
		if (title_str[0] == '#') title_str++;

		/* Copy over the string  XXX XXX */
		strcpy(buf, format("%s", title_str));

		/* Display some text */
		c_roff_centered(TERM_WHITE, format("Morgoth, Lord of Darkness lies vanquished, and fear dies with him; God's bright creation, our Middle-Earth, is free!  Your skill and valour is the stuff of legend; always we will remember the %s %s who saved us.\n\nWhen ready to assume the crown that awaits you, press \"control-C\" to retire.",  race_adj, buf), 10, 70);
	}

	/* Wait for it */
	pause_line(23);


	/* Set to 50 rows, if we were showing 50 before */
	if (old_rows == 50)
	{
		p_ptr->redraw |= (PR_MAP | PR_BASIC | PR_EXTRA);
		Term_rows(TRUE);
	}

	/* Restore the screen */
	screen_load();
}


/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	char o_name[120];

	cptr s;

	/* No inscription */
	if (!o_ptr->note) return;

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->note), '!');

	/* Process notification request. */
	while (s)
	{
		/* Find another '!' */
		if (s[1] == '!')
		{
			/* Describe (briefly) */
			object_desc(o_name, o_ptr, FALSE, 0);

			/* Notify the player */
			if (o_ptr->number > 1)
				msg_format("Your %s are recharged.", o_name);
			else msg_format("Your %s is recharged.", o_name);

			/* Disturb */
			disturb(0, 0);

			/* Done. */
			return;
		}

		/* Keep looking for '!'s */
		s = strchr(s + 1, '!');
	}
}


/*
 * Handle the equipment and inventory as time passes.
 */
static void process_world_aux_inven(void)
{
	object_type *o_ptr;
	object_kind *k_ptr;

	int i;
	u32b f1, f2, f3;

	/* Assume nothing to notice */
	bool flag = FALSE;

	/* Process equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Get the object */
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Get object attributes */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Burn some fuel in the current light, unless it doesn't need fuel */
		if ((o_ptr->tval == TV_LITE) && (!(f3 & (TR3_NOFUEL))))
		{
			/* Hack -- Use some fuel */
			if (o_ptr->pval > 0)
			{
				/* Decrease life-span */
				o_ptr->pval--;

				/* Hack -- notice interesting fuel steps */
				if ((o_ptr->pval < 100) || (!(o_ptr->pval % 20)))
				{
					flag = TRUE;
				}

				/* The light is now out */
				if (o_ptr->pval == 0)
				{
					/* Hack -- Special treatment when blind */
					if (p_ptr->blind)
					{
						o_ptr->pval++;
					}
					else
					{
						disturb(0, 0);
						msg_print("Your light has gone out!");

						/* Calculate torch radius */
						p_ptr->update |= (PU_TORCH);
					}
				}

				/* The light is getting dim */
				else if ((o_ptr->pval < 100) && (!(o_ptr->pval % 10)))
				{
					if (disturb_minor) disturb(0, 0);
					msg_print("Your light is growing faint.");
				}

				/* The torch is getting dim */
				else if ((o_ptr->sval == SV_LITE_TORCH) &&
				         (o_ptr->pval == TORCH_LOW - 1))
				{
					if (disturb_minor) disturb(0, 0);
					msg_print("Your torch fades.");

					/* Calculate torch radius */
					p_ptr->update |= (PU_TORCH);
				}
			}
		}

		/* Feed soul eaters */
		if (p_ptr->soulsteal && (f3 & (TR3_SOULSTEAL)))
		{
			p_ptr->soul_reserve -= randint(3);
			if (p_ptr->soul_reserve < 0)
			{
				char o_desc[80];

				if (one_in_(3))
				{
					object_desc(o_desc, o_ptr, FALSE, 0);
					take_hit(damroll(6, 6), 0,
						format("Your %s is feeding on your soul!", o_desc), "a weapon feeding on your soul");
				}
				p_ptr->soul_reserve = 0;
			}
		}

		/* Learn about unaware rings and amulets */
		if ((!object_aware_p(o_ptr)) &&
		    ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)))
		{
			/* Sense carefully every so often */
			if (one_in_(200))
			{
				sense_object(o_ptr, i, TRUE, FALSE);

				/* Object is now aware */
				if (object_aware_p(o_ptr))
				{
					k_ptr = &k_info[o_ptr->k_idx];

					/* Gain a significant amount of exp */
					gain_exp(k_ptr->level * k_ptr->level / 2, S_NOSKILL);
				}

				flag = TRUE;
			}
		}

		/* Get crushed */
		if ((p_ptr->being_crushed) && (f3 & (TR3_DRAIN_HP)) &&
		    (one_in_(2)))
		{
			/* Damage is proportional to remaining health */
			int dam = randint(rsqrt(p_ptr->chp));

			/* Take a hit */
			if (dam)
			{
				char o_desc[80];

				/* Different text for different kind of objects */
				cptr str = "is crushing you";

				if (o_ptr->artifact_index == ART_LIGHTNING_LANCE)
					str = "electrocutes you";
				else if (is_any_weapon(o_ptr)) str = "attacks you";

				/* Describe the object */
				object_desc(o_desc, o_ptr, FALSE, 0);

				/* Ouch! */
				if (o_ptr->artifact_index == ART_LIGHTNING_LANCE)
				{
					elec_dam(dam, 0, format("Your %s %s!", o_desc, str),
						"a murderous item");
				}
				else
				{
					take_hit(dam, 0, format("Your %s %s!", o_desc, str),
						"a murderous item");
				}
			}
		}
	}

	/* Notice changes */
	if (flag)
	{
		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);
	}

	/* Assume nothing to notice */
	flag = FALSE;

	/*
	 * Recharge rods and activatable objects.  Rods now use timeout to
	 * control charging status, and each charging rod in a stack decreases
	 * the stack's timeout by one per turn.  -LM-
	 */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge activatable objects */
		if (o_ptr->timeout > 0)
		{
			/* Object is a rod */
			if (o_ptr->tval == TV_ROD)
			{
				int num = 0;

				/* Some rods should never get discharged at all */
				if (!k_ptr->pval) o_ptr->timeout = 0;

				/* Handle normal rods */
				else
				{
					/* Determine how many rods are actually charging */
					num = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
					if (num > o_ptr->number) num = o_ptr->number;

					/* Decrease timeout by that number. */
					o_ptr->timeout -= num;

					/* Boundary control. */
					if (o_ptr->timeout < 0) o_ptr->timeout = 0;
				}

				/* Update if any rods are recharged */
				if (num > (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval)
				{
					/* Update equipment window */
					flag = TRUE;

					/* Message if whole stack is recharged, if inscribed */
					if (!(o_ptr->timeout)) recharged_notice(o_ptr);
				}
			}

			/* Object is any other activatable object */
			else
			{
				/* Recharge */
				o_ptr->timeout--;

				/* Notice changes, notify if object is inscribed. */
				if (!(o_ptr->timeout))
				{
					recharged_notice(o_ptr);
					flag = TRUE;
				}
			}
		}
	}

	/* Notice changes */
	if (flag)
	{
		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}
}



/*
 * Handle certain things as time passes.
 */
static void process_world(void)
{
	int i, temp;

	/* Remember old hitpoint and mana recovery */
	int old_life_recovery_value = p_ptr->life_recovery_value;
	int old_mana_recovery_value = p_ptr->mana_recovery_value;

	object_type *o_ptr;
	object_kind *k_ptr;


	/* We decrease noise slightly every game turn */
	total_wakeup_chance -= 400;

	/* But the character always makes some noise */
	if (total_wakeup_chance < p_ptr->base_wakeup_chance)
	    total_wakeup_chance = p_ptr->base_wakeup_chance;


	/* Every 10 game turns */
	if (turn % 10) return;

#ifdef ALLOW_BORG
	if (count_stop)
	{
		/* The borg is always in perfect health */

		/* Remove curses */
		(void)remove_all_curse();

		/* Restore stats */
		(void)res_stat(A_STR);
		(void)res_stat(A_INT);
		(void)res_stat(A_WIS);
		(void)res_stat(A_CON);
		(void)res_stat(A_DEX);
		(void)res_stat(A_CHR);

		/* No maladies */
		p_ptr->blind = 0;
		p_ptr->confused = 0;
		p_ptr->poisoned = 0;
		p_ptr->afraid = 0;
		p_ptr->paralyzed = 0;
		p_ptr->image = 0;
		p_ptr->slow = 0;
		p_ptr->stun = 0;
		p_ptr->paralyzed = 0;
		p_ptr->cut = 0;

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;

		/* No longer hungry */
		p_ptr->food = PY_FOOD_MAX - 1;
	}
#endif

	/*** Check the Time and Load ***/

	if (!(turn % 1000))
	{
		/* Check time and load */
		if ((0 != check_time()) || (0 != check_load()))
		{
			/* Warning */
			if (closing_flag <= 2)
			{
				/* Disturb */
				disturb(0, 0);

				/* Count warnings */
				closing_flag++;

				/* Message */
				msg_print("The gates to ANGBAND are closing...");
				msg_print("Please finish up and/or save your game.");
			}

			/* Slam the gate */
			else
			{
				/* Message */
				msg_print("The gates to ANGBAND are now closed.");

				/* Stop playing */
				p_ptr->playing = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}


	/*** Attempt timed autosave.  From Zangband. ***/
	if (autosave_freq)
	{
		if (!(turn % (autosave_freq * 10L)))
		{
			do_cmd_save_game(TRUE);
		}
	}

	/*** Update quests ***/

	/* Check for quest failure */
	check_quest_failure(1);


	/*** Handle the "town" (stores and sunshine) ***/

	/* While in town */
	if (!p_ptr->depth)
	{
		/* Hack -- Daybreak/Nighfall in town */
		if (!(turn % ((10L * TOWN_DAWN) / 2)))
		{
			bool dawn;

			/* Check for dawn */
			dawn = (!(turn % (10L * TOWN_DAWN)));

			/* Day breaks */
			if (dawn)
			{
				/* Message */
				msg_print("Dawn breaks, and the day begins.");
			}

			/* Night falls */
			else
			{
				/* Message */
				msg_print("The sun sets.");
			}

			/* Illuminate */
			town_illuminate(dawn);
		}
	}


	/* While in the dungeon */
	else
	{
		/* Update the stores every so often */
		if (!(turn % (10L * STORE_TURNS)))
		{
			int n;

			/* Change one item in each shop */
			for (n = 0; n < MAX_STORES; n++) store_maint(n, FALSE);

			/* Shuffle the shop-keepers on rare occasion */
			if (one_in_(STORE_SHUFFLE))
			{
				/* Pick a random shop (except home) */
				while (TRUE)
				{
					n = rand_int(MAX_STORES);
					if (n != STORE_HOME) break;
				}

				/* Shuffle it */
				store_shuffle(n);
			}
		}
	}


	/*** Process the monsters ***/

	/*
	 * Monsters appear more often deep in the dungeon and less often
	 * around stealthy characters.
	 */
	i = MAX_M_ALLOC_CHANCE;
	i += get_skill(S_STEALTH, -40, 80);
	i -= p_ptr->depth / 2;

	/* Stay sane */
	if (i < 50) i = 50;


	/* Check for creature generation -- but not in the town */
	if ((p_ptr->depth) && (one_in_(i)))
	{
		int slp = FALSE;

		/* Sneaky characters make monsters sleepy (not in town though) */
		if ((p_ptr->depth) && (get_skill(S_STEALTH, 0, 40) > rand_int(100)))
		{
			slp = TRUE;
		}

		/* Make a new monster */
		(void)alloc_monster(MAX_SIGHT + 5, slp);
	}


	/*** Damage over Time ***/

	/* Natural vitality allows faster recovery from some conditions */
	temp = 1;
	if (p_ptr->vitality) temp = 4;


	/* Take damage from poison */
	if ((p_ptr->poisoned) && (!p_ptr->necro_rage))
	{
		/* Take damage (increases with severity of poison) */
		take_hit(randint(p_ptr->poisoned > 400 ? 20 :
			p_ptr->poisoned / 20), 0, NULL, "poison");
	}

	/* Take damage from disease */
	if (p_ptr->diseased)
	{
		/* Take damage (increases with severity of disease) */
		take_hit(randint(p_ptr->diseased > 400 ? 40 :
			p_ptr->diseased / 10), 0, NULL, "a fatal disease");
	}

	/* Take damage from cuts */
	if ((p_ptr->cut) && (!p_ptr->berserk) && (!p_ptr->necro_rage))
	{
		/* Mortal wound or Deep Gash */
		if (p_ptr->cut > 200)
		{
			i = 3;
		}

		/* Severe cut */
		else if (p_ptr->cut > 100)
		{
			i = 2;
		}

		/* Other cuts */
		else
		{
			i = 1;
		}

		/* Take damage */
		take_hit(i, 0, NULL, "a fatal wound");
	}

	/* Handle manic-depressive fits */
	if ((p_ptr->mania > 0) && (one_in_(100)))
	{
		/* Mania */
		if (one_in_(2))
		{
			/* Message */
			msg_print("You go into a manic fit!");

			/* Cancel slowness or speed up */
			if (p_ptr->slow > 0)
			{
				(void)set_slow(0);
			}
			else
			{
				(void)set_fast(p_ptr->fast + rand_range(10, 40));
			}

			/* Recover any fear */
			set_bold(p_ptr->bold + rand_range(5, 15));
		}

		/* Depression */
		else
		{
			/* Message */
			msg_print("You suddenly feel lethargic!");

			/* Cancel speed or slow down */
			if (p_ptr->fast > 0)
			{
				(void)set_fast(0);
			}
			else
			{
				(void)set_slow(p_ptr->slow + rand_range(10, 40));
			}

			/* Sometimes get frightened */
			if ((one_in_(3)) && ((!p_ptr->resist_fear ||
				p_ptr->hero || p_ptr->berserk)))
			{
				msg_print("You feel scared!");
				set_afraid(p_ptr->afraid + rand_range(10, 25));
			}
		}
	}

	/*** Check the Food, and Regenerate ***/

	/* Digest normally */
	if (p_ptr->food < p_ptr->food_bloated)
	{
		/* Every 100 game turns */
		if (!(turn % 100))
		{
			/* Basic digestion rate based on speed */
			i = extract_energy[p_ptr->pspeed] * 2;

			/* Half-trolls and Giants need to eat a lot -clefs- */
			if ((p_ptr->prace == RACE_HALF_TROLL) ||
			    (p_ptr->prace == RACE_GIANT))
			{
				i += i / 2;
			}

			/* Regeneration takes more food */
			if (p_ptr->regenerate) i += 30;

			/* Slow digestion halves food consumption */
			if (p_ptr->slow_digest) i /= 2;

			/* Minimal digestion */
			if (i < 1) i = 1;

			/* Digest some food */
			(void)set_food(p_ptr->food - i);
		}
	}

	/* Digest quickly when gorged */
	else
	{
		/* Digest a lot of food */
		(void)set_food(p_ptr->food - 100);
	}

	/* Starve to death (slowly) */
	if (p_ptr->food < p_ptr->food_starving)
	{
		/* Calculate damage */
		i = (p_ptr->food_starving - p_ptr->food) / 10;

		/* Take damage */
		take_hit(i, 0, NULL, "starvation");
	}

	/* Getting Faint */
	if (p_ptr->food < p_ptr->food_fainting)
	{
		/* Faint occasionally */
		if (!p_ptr->paralyzed && (one_in_(10)))
		{
			/* Message */
			msg_print("You faint from the lack of food.");
			disturb(1, 0);

			/* Hack -- faint (bypass free action) */
			(void)set_paralyzed(p_ptr->paralyzed + randint(5));
		}
	}


	/* Calculate hitpoint recovery */
	p_ptr->life_recovery_value = calc_hp_regen();

	/* Calculate mana recovery */
	p_ptr->mana_recovery_value = calc_mana_regen();


	/* Regenerate hitpoints */
	if (p_ptr->chp < p_ptr->mhp)
	{
		regenhp(p_ptr->life_recovery_value);
	}

	/* Regenerate mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		regenmana(p_ptr->mana_recovery_value);
	}


	/* Note if rates of recovery change */
	if ((p_ptr->life_recovery_value != old_life_recovery_value) ||
	    (p_ptr->mana_recovery_value != old_mana_recovery_value))
	{
		/* Print "regen: HP/MP" */
		left_panel_display(DISPLAY_REGEN, 0);
	}


	/*** Timeout Various Things ***/

	/* Blindness */
	if (p_ptr->blind)
	{
		(void)set_blind(p_ptr->blind - temp, NULL);
	}

	/* Confusion */
	if (p_ptr->confused)
	{
		(void)set_confused(p_ptr->confused - temp);
	}

	/* Hack -- Hallucinating */
	if (p_ptr->image)
	{
		(void)set_image(p_ptr->image - 1);
	}

	/* Afraid */
	if (p_ptr->afraid)
	{
		(void)set_afraid(p_ptr->afraid - 1);
	}

	/* Paralysis */
	if (p_ptr->paralyzed)
	{
		(void)set_paralyzed(p_ptr->paralyzed - temp);
	}

	/* Fast */
	if (p_ptr->fast)
	{
		(void)set_fast(p_ptr->fast - 1);
	}

	/* Slow */
	if (p_ptr->slow)
	{
		(void)set_slow(p_ptr->slow - 1);
	}

	/* Shield */
	if (p_ptr->shield)
	{
		(void)set_shield(p_ptr->shield - 1, NULL);
	}
	if (p_ptr->steelskin)
	{
		(void)set_steelskin(p_ptr->steelskin - 1, NULL);
	}

	/* Blessed */
	if (p_ptr->blessed)
	{
		(void)set_blessed(p_ptr->blessed - 1, NULL);
	}

	/* Holy */
	if (p_ptr->holy)
	{
		(void)set_holy(p_ptr->holy - 1);
	}

	/* Boldness */
	if (p_ptr->bold)
	{
		(void)set_bold(p_ptr->bold - 1);
	}

	/* Heroism */
	if (p_ptr->hero)
	{
		(void)set_hero(p_ptr->hero - 1);
	}

	/* Berserk */
	if (p_ptr->berserk)
	{
		(void)set_berserk(p_ptr->berserk - 1);
	}

	/* Necro rage */
	if (p_ptr->necro_rage)
	{
		(void)set_necro_rage(p_ptr->necro_rage - 1);
	}

	/* Protection from evil */
	if (p_ptr->protevil)
	{
		(void)set_protevil(p_ptr->protevil - 1);
	}

	/* Timed Wizardry Protection */
	if (p_ptr->wiz_prot)
	{
		(void)set_wiz_prot(p_ptr->wiz_prot - 1);
	}

	/* Timed see-invisible */
	if (p_ptr->detect_inv)
	{
		(void)set_detect_inv(p_ptr->detect_inv - 1);
	}

	/* Timed ESP of evil */
	if (p_ptr->esp_evil)
	{
		(void)set_esp_evil(p_ptr->esp_evil - 1);
	}

	/* Timed ESP */
	if (p_ptr->tim_esp)
	{
		(void)set_tim_esp(p_ptr->tim_esp - 1);
	}

	/* Timed infra-vision */
	if (p_ptr->tim_infra)
	{
		(void)set_tim_infra(p_ptr->tim_infra - 1);
	}

	/* Timed invisibility */
	if (p_ptr->tim_invis)
	{
		(void)set_invis(p_ptr->tim_invis - 1, p_ptr->tim_inv_pow);
	}

	/* Timed stealth */
	if (p_ptr->tim_stealth)
	{
		(void)set_tim_stealth(p_ptr->tim_stealth - 1);
	}

	/* Timed regeneration */
	if (p_ptr->regen_hp)
	{
		(void)set_regen_hp(p_ptr->regen_hp - 1);
	}
	if (p_ptr->regen_mana)
	{
		(void)set_regen_mana(p_ptr->regen_mana - 1);
	}

	/* Timed vitality */
	if (p_ptr->vitality)
	{
		(void)set_vitality(p_ptr->vitality - 1);
	}

	/* Timed mania */
	if (p_ptr->mania)
	{
		(void)set_mania(p_ptr->mania - 1);
	}

	/* Damage resistance */
	if (p_ptr->res_dam)
	{
		(void)set_res_dam(p_ptr->res_dam - 1);
	}

	/* Talent time-out */
	for (i = 0; i < NUM_TALENTS; i++)
	{
		/* Require a talent that is timed out */
		if (p_ptr->ptalents[i].count > 0)
		{
			p_ptr->ptalents[i].count--;

			/* Messages about "marked" talents */
			if ((p_ptr->ptalents[i].count == 0) && (p_ptr->ptalents[i].marked))
			{
				msg_format("You may use the talent \"%s\".", talent_info[i].name);
			}
		}
	}

	/* Oppose Acid */
	if (p_ptr->oppose_acid)
	{
		(void)set_oppose_acid(p_ptr->oppose_acid - 1);
	}

	/* Oppose Lightning */
	if (p_ptr->oppose_elec)
	{
		(void)set_oppose_elec(p_ptr->oppose_elec - 1);
	}

	/* Oppose Fire */
	if (p_ptr->oppose_fire)
	{
		(void)set_oppose_fire(p_ptr->oppose_fire - 1);
	}

	/* Oppose Cold */
	if (p_ptr->oppose_cold)
	{
		(void)set_oppose_cold(p_ptr->oppose_cold - 1);
	}

	/* Oppose Poison */
	if (p_ptr->oppose_pois)
	{
		(void)set_oppose_pois(p_ptr->oppose_pois - 1);
	}

	/* Oppose Ethereal */
	if (p_ptr->oppose_ethereal)
	{
		(void)set_oppose_ethereal(p_ptr->oppose_ethereal - 1);
	}

	/* Mental Barrier */
	if (p_ptr->mental_barrier)
	{
		(void)set_mental_barrier(p_ptr->mental_barrier - 1);
	}

	/* Forbid Summoning */
	if (p_ptr->forbid_summoning)
	{
		(void)set_forbid_summoning(p_ptr->forbid_summoning - 1);
	}

	/* Foes-a-phasing */
	if (p_ptr->phasing_foes)
	{
		(void)set_phasing_foes(p_ptr->phasing_foes - 1, NULL);
	}

	/* Evasion */
	if (p_ptr->evasion)
	{
		(void)set_evasion(p_ptr->evasion - 1);
	}

	/* Shapechanges and form-shifts */
	if (p_ptr->wraithform)
	{
		(void)set_wraithform(p_ptr->wraithform - 1);
	}
	if (p_ptr->trollform)
	{
		(void)set_trollform(p_ptr->trollform - 1);
	}
	if (p_ptr->dragonform)
	{
		(void)set_dragonform(p_ptr->dragonform - 1);
	}


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + temp);
		if (p_ptr->prace == RACE_HOBBIT) adjust++;

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Disease */
	if (p_ptr->diseased)
	{
		/* Disease is hard to shake off */
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + temp) / 4;

		/* Always recover a little */
		if (adjust == 0) adjust = rand_int(2);

		/* Hobbits are resiliant */
		if (p_ptr->prace == RACE_HOBBIT) adjust++;

		/* Apply some recovery */
		(void)set_diseased(p_ptr->diseased - adjust, NULL);
	}

	/* Stun */
	if (p_ptr->stun)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_DEX]] + temp);

		int skill;

		/* Martial arts experts recover from stunning faster */
		if (p_ptr->pskills[S_WRESTLING].cur >= p_ptr->pskills[S_KARATE].cur)
			skill = S_WRESTLING;
		else
			skill = S_KARATE;

		adjust += get_skill(skill, 0, 5);


		/* Apply some healing */
		(void)set_stun(p_ptr->stun - adjust);
	}

	/* Cut */
	if (p_ptr->cut)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + temp);
		if (p_ptr->prace == RACE_HOBBIT) adjust++;

		/* Vitality */
		if (p_ptr->vitality) adjust *= 2;

		/* Hack -- Truly "mortal" wound */
		if (p_ptr->cut >= WOUND_MORTAL) adjust = 0;

		/* Apply some healing */
		(void)set_cut(p_ptr->cut - adjust);
	}


	/* Handle experience draining -- light */
	if (p_ptr->drain_exp)
	{
		/* Hobbits are resistant */
		int chance = 90;
		if (p_ptr->prace == RACE_HOBBIT) chance /= 3;

		/* Most of the time */
		if (rand_int(100) < chance)
		{
			lose_exp(1, TRUE);
		}
	}

	/* Handle experience draining -- heavy */
	if (p_ptr->black_breath)
	{
		/* Exp drained depends on total exp */
		s32b dam = calc_spent_exp() / 100000L;

		/* Hobbits are resistant */
		if (p_ptr->prace == RACE_HOBBIT) dam /= 3;

		/* Always lose something */
		if (dam < 1L) dam = 1L;

		/* Sometimes drain skills */
		if (one_in_(1000))
		{
			lose_exp(dam * 100, TRUE);
		}

		/* Usually attack unspent exp */
		else
		{
			p_ptr->exp -= (dam - 1);

			lose_exp(1, TRUE);
		}
	}


	/*** Process Inventory ***/
	process_world_aux_inven();


	/* Process objects in the dungeon */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Object has a timeout (only one time in three, though)  -LM- */
		if ((o_ptr->timeout) && (!(turn % 30)))
		{
			/* Object is a rod */
			if (o_ptr->tval == TV_ROD)
			{
				k_ptr = &k_info[o_ptr->k_idx];

				/* Some rods should never get discharged at all */
				if (!k_ptr->pval) o_ptr->timeout = 0;

				/* Handle normal rods */
				else
				{
					int num = (o_ptr->timeout + (k_ptr->pval - 1)) /
						k_ptr->pval;
					if (num > o_ptr->number) num = o_ptr->number;

					/* Decrease timeout by that number. */
					o_ptr->timeout -= num;

					/* Boundary control. */
					if (o_ptr->timeout < 0) o_ptr->timeout = 0;
				}
			}

			/* Object is any other activatable object */
			else
			{
				/* Recharge */
				o_ptr->timeout--;
			}
		}
	}


	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->teleport) && (one_in_(100)))
	{
		/* Teleport player */
		teleport_player(40, FALSE);
	}

	/* Nexus field */
	if (p_ptr->nexus_field)
	{
		/* Nexus effects */
		apply_nexus(-1, -1, p_ptr->nexus_field_strength);

		/* Timeout field */
		set_nexus_field(p_ptr->nexus_field - 1, p_ptr->nexus_field_strength);
	}

	/* Delayed Word-of-Recall */
	if (p_ptr->word_recall)
	{
		/* Count down towards recall */
		p_ptr->word_recall--;

		/* Activate the recall */
		if (!p_ptr->word_recall)
		{
			/* Disturbing! */
			disturb(0, 0);

			/* Sound */
			sound(MSG_TPLEVEL);

			/* Determine the level */
			if (p_ptr->depth)
			{
				msg_print("You feel yourself yanked upwards!");

				/* New depth */
				p_ptr->depth = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
				msg_print("You feel yourself yanked downwards!");

				/* New depth */
				p_ptr->depth = p_ptr->max_depth;
				if (p_ptr->depth < 1) p_ptr->depth = 1;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}
	}

	/* Delayed level feelings */
	if ((p_ptr->depth) && (!p_ptr->leaving) && (no_feeling_yet))
	{
		/* Perceptive characters learn about levels faster */
		int time_needed = 1500 - get_skill(S_PERCEPTION, 0, 1000);

		/* After sufficient time, can learn about the level */
		if (turn - old_turn >= time_needed)
		{
			/* Now have a feeling */
			no_feeling_yet = FALSE;

			/* Announce feeling */
			do_cmd_feeling();

			/* Update the level indicator */
			p_ptr->redraw |= (PR_DEPTH);

			/* Disturb */
			disturb(0, 0);
		}
	}


	/*** Weather ***/
	if (TRUE)
	{
		bool change = FALSE;


		/* Count down to next random change in the weather */
		if (p_ptr->tim_weath) p_ptr->tim_weath--;

		/* Weather is magically held */
		if (p_ptr->hold_weath > 0)
		{
			p_ptr->hold_weath--;
		}

		/* Time for a change in the weather */
		else if (p_ptr->tim_weath <= 0)
		{
			/* Change the weather randomly */
			change = change_weather(0, 0, 0);

			/* Reset time until next random weather change */
			p_ptr->tim_weath = rand_range(WEATHER_LENGTH,
				WEATHER_LENGTH * 2) - (4 * p_ptr->depth / 10);
		}

		/* Notice changes (nature magic-users only) */
		if ((change) && (p_ptr->realm == DRUID))
		{
			if (p_ptr->depth)
			{
				/*
				 * Give the player a chance to sense that the
				 * weather has changed.  (100% yields ~95% at 5000')
				 */
				if (rand_int(p_ptr->depth + 5) < get_skill(S_NATURE, 0, 100))
				{
					msg_print("You feel a change in the weather.");
					if (disturb_minor) disturb(0, 0);
				}
			}
			else
			{
				/* If you're in town, you never miss a change. */
				if (disturb_minor) disturb(0, 0);
				msg_print("The weather is changing.");
				predict_weather(100);
			}
		}
	}

	/* Update time display */
	left_panel_display(DISPLAY_TIME, 0);
}


/*
 * Verify use of "wizard" mode
 */
static bool enter_wizard_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & (0x0002)))
	{
		/* Mention effects */
		msg_print("You are about to enter 'wizard' mode for the first time!");
		msg_print("This is a form of cheating, and your game will not be scored!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to enter wizard mode?"))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= (0x0002);

	/* Success */
	return (TRUE);
}



#ifdef ALLOW_DEBUG

/*
 * Verify use of "debug" mode
 */
static bool verify_debug_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & (0x0008)))
	{
		/* Mention effects */
		msg_print("You are about to use the potentially dangerous debug commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the debug commands?"))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= (0x0008);

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- Declare the Debug Routines
 */
extern void do_cmd_debug(void);

#endif



#ifdef ALLOW_BORG

/*
 * Verify use of "borg" mode
 */
static bool verify_borg_mode(void)
{
	/* Ask first time */
	if (!(p_ptr->noscore & 0x0010))
	{
		/* Mention effects */
		msg_print("You are about to use the potentially dangerous borg commands!");
		msg_print("Your machine may crash, and your savefile may become corrupted!");
		message_flush();

		/* Verify request */
		if (!get_check("Are you sure you want to use the borg commands?"))
		{
			return (FALSE);
		}
	}

	/* Mark savefile */
	p_ptr->noscore |= (0x0010);

	/* Okay */
	return (TRUE);
}


/*
 * Hack -- Declare the Borg Routines
 */
extern void do_cmd_borg(void);

#endif



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
static void process_command(void)
{
	bool flag = FALSE;

	/* Handle repeating the last command  -TNB- */
	repeat_check();

	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Ignore */
		case ESCAPE:
		case ' ':
		case '\n':
		case '\r':
		case '\a':
		{
			break;
		}

		/*** Cheating Commands ***/

		/* Toggle Wizard Mode */
		case KTRL('W'):
		{
			if (p_ptr->wizard)
			{
				p_ptr->wizard = FALSE;
				msg_print("Wizard mode off.");
			}
			else if (enter_wizard_mode())
			{
				p_ptr->wizard = TRUE;
				msg_print("Wizard mode on.");
			}

			/* Redraw everything */
			do_cmd_redraw();

			break;
		}


#ifdef ALLOW_DEBUG

		/* Special "debug" commands */
		case KTRL('A'):
		{
			if (verify_debug_mode()) do_cmd_debug();
			break;
		}

#endif


#ifdef ALLOW_BORG

		/* Special "borg" commands */
		case KTRL('Z'):
		{
			if (verify_borg_mode()) do_cmd_borg();
			break;
		}

#endif



		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

		/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Drop an item */
		case 'd':
		{
			do_cmd_drop();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Observe an object */
		case 'I':
		{
			do_cmd_observe(NULL, FALSE);
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}


		/*** Standard "Movement" Commands ***/

		/* Alter a grid */
		case '+':
		{
			/* Use deliberate or automatic alteration */
			if (p_ptr->using_keymap) do_cmd_alter(FALSE);
			else                     do_cmd_alter(TRUE);
			break;
		}

		/* Dig a tunnel */
		case 'T':
		{
			do_cmd_tunnel();
			break;
		}

		/* Walk */
		case ';':
		{
			do_cmd_walk();
			break;
		}

		/* Jump */
		case '-':
		{
			do_cmd_jump();
			break;
		}


		/*** Running, Resting, Searching, Staying */

		/* Begin Running -- Arg is Max Distance */
		case '.':
		{
			do_cmd_run();
			break;
		}

		/* Hold still for a turn.  Pick up objects if auto-pickup is true. */
		case ',':
		{
			do_cmd_hold();
			break;
		}

		/* Always pick up objects. */
		case 'g':
		{
			do_cmd_pickup();
			break;
		}

		/* Rest -- Arg is time */
		case 'R':
		{
			do_cmd_rest();
			break;
		}

		/* Search for traps/doors */
		case 's':
		{
			do_cmd_search();
			break;
		}

		/* Toggle sneaking mode */
		case 'S':
		{
			do_cmd_sneaking();
			break;
		}


		/*** Stairs and Doors and Chests and Traps ***/

		/* Enter store */
		case '_':
		{
			do_cmd_store();
			break;
		}

		/* Go up staircase */
		case '<':
		{
			do_cmd_go_up();
			break;
		}

		/* Go down staircase */
		case '>':
		{
			do_cmd_go_down();
			break;
		}

		/* Open a door or chest */
		case 'o':
		{
			do_cmd_open();
			break;
		}

		/* Close a door */
		case 'c':
		{
			do_cmd_close();
			break;
		}

		/* Jam a door with spikes */
		case 'j':
		{
			do_cmd_spike();
			break;
		}

		/* Bash a door */
		case 'B':
		{
			do_cmd_bash();
			break;
		}

		/* Disarm a trap or chest */
		case 'D':
		{
			do_cmd_disarm();
			break;
		}


		/*** Magic and Prayers ***/

		/* Gain new spells/prayers */
		case 'G':
		{
			do_cmd_study();
			break;
		}

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Cast a spell */
		case 'm':
		case 'p':
		{
			(void)do_spell(SPELL_CAST, 0);
			break;
		}

		/*** Use various objects ***/

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}

		/* Activate an artifact (or other activatable item) */
		case 'A':
		{
			do_cmd_activate();
			break;
		}

		/* Eat some food */
		case 'E':
		{
			use_object(TV_FOOD);
			break;
		}

		/* Fuel your lantern/torch */
		case 'F':
		{
			do_cmd_refill();
			break;
		}

		/* Fire an item */
		case 'f':
		{
			do_cmd_fire();
			break;
		}

		/* Throw an item */
		case 'v':
		{
			do_cmd_throw();
			break;
		}

		/* Aim a wand */
		case 'a':
		{
			use_device(TV_WAND);
			break;
		}

		/* Zap a rod */
		case 'z':
		{
			use_device(TV_ROD);
			break;
		}

		/* Quaff a potion */
		case 'q':
		{
			use_object(TV_POTION);
			break;
		}

		/* Read a scroll */
		case 'r':
		{
			use_object(TV_SCROLL);
			break;
		}

		/* Use a staff */
		case 'u':
		{
			use_device(TV_STAFF);
			break;
		}


		/*** Looking at Things (nearby or on map) ***/

		/* Full dungeon map */
		case 'M':
		{
			do_cmd_view_map();
			break;
		}

		/* Locate player on map */
		case 'L':
		{
			do_cmd_locate();
			break;
		}

		/* Look around */
		case 'l':
		{
			do_cmd_look();
			break;
		}

		/* Target monster or location */
		case '*':
		{
			do_cmd_target();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			do_cmd_redraw();
			break;
		}


		/*** Misc Commands ***/

		/* Interact with skills */
		case '$':
		{
			do_cmd_skills();
			break;
		}

		/* Use talents */
		case '[':
		{
			do_cmd_talents();
			break;
		}

		/* Stop doing a shapechange */
		case ']':
		{
			do_cmd_unchange(TRUE);
			break;
		}

		/* Barehanded fighting */
		case '|':
		{
			do_cmd_barehanded();
			break;
		}

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show quest */
		case KTRL('Q'):
		{
			do_cmd_quest();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Redraw the screen */
		case KTRL('R'):
		{
			do_cmd_redraw();
			break;
		}

#ifndef VERIFY_SAVEFILE

		/* Hack -- Save and don't quit */
		case KTRL('S'):
		{
			do_cmd_save_game(FALSE);
			break;
		}

#endif

		/* Save and quit */
		case KTRL('X'):
		{
			/* Stop playing */
			p_ptr->playing = FALSE;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		/* Quit (suicide or retire) */
		case 'Q':
		{
			do_cmd_quit();
			break;
		}

		/* Check knowledge */
		case '~':
		{
			/* Knowledge menu */
			if (p_ptr->get_help_index != HELP_CMD_REVIEW)
				do_cmd_knowledge();

			/* Review commands */
			else do_cmd_help();

			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}

		/* Hack -- Unknown command */
		default:
		{
			prt("Type '?' for help, or '~' to review commands.", 0, 0);
			flag = TRUE;
			break;
		}
	}

	/* Review commands */
	if (flag) p_ptr->get_help_index = HELP_CMD_REVIEW;

	/* Normal help */
	else p_ptr->get_help_index = HELP_GENERAL;
}



/*
 * Hack -- helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void process_player_aux(void)
{
	static int old_monster_race_idx = 0;

	static u32b	old_flags1 = 0L;
	static u32b	old_flags2 = 0L;
	static u32b	old_flags3 = 0L;
	static u32b	old_flags4 = 0L;
	static u32b	old_flags5 = 0L;
	static u32b	old_flags6 = 0L;
	static u32b	old_flags7 = 0L;

	static byte	old_blows[MONSTER_BLOW_MAX];

	static byte	old_ranged = 0;

	int i;


	/* Tracking a monster */
	if (p_ptr->monster_race_idx)
	{
		/* Get the monster lore */
		monster_lore *l_ptr = &l_list[p_ptr->monster_race_idx];

		/* Check for change of any kind */
		if ((old_monster_race_idx != p_ptr->monster_race_idx) ||
		    (old_flags1 != l_ptr->flags1) ||
		    (old_flags2 != l_ptr->flags2) ||
		    (old_flags3 != l_ptr->flags3) ||
		    (old_flags4 != l_ptr->flags4) ||
		    (old_flags5 != l_ptr->flags5) ||
		    (old_flags6 != l_ptr->flags6) ||
		    (old_flags7 != l_ptr->flags7) ||
		    (old_blows[0] != l_ptr->blows[0]) ||
		    (old_blows[1] != l_ptr->blows[1]) ||
		    (old_blows[2] != l_ptr->blows[2]) ||
		    (old_blows[3] != l_ptr->blows[3]) ||
		    (old_ranged != l_ptr->ranged))
		{
			/* Memorize old race */
			old_monster_race_idx = p_ptr->monster_race_idx;

			/* Memorize flags */
			old_flags1 = l_ptr->flags1;
			old_flags2 = l_ptr->flags2;
			old_flags3 = l_ptr->flags3;
			old_flags4 = l_ptr->flags4;
			old_flags5 = l_ptr->flags5;
			old_flags6 = l_ptr->flags6;
			old_flags7 = l_ptr->flags7;

			/* Memorize blows */
			for (i = 0; i < MONSTER_BLOW_MAX; i++)
			{
				old_blows[i] = l_ptr->blows[i];
			}

			/* Memorize castings */
			old_ranged = l_ptr->ranged;

			/* Window stuff */
			p_ptr->window |= (PW_MONSTER);

			/* Window stuff */
			window_stuff();
		}
	}
}

/*
 * Some refreshes are done before a character takes his turn.  -LM-
 *
 * We update extra monster health bars, and insure that enough quest
 * monsters exist.
 */
static void refresh_monsters_before(void)
{
	int i;

	/* Remove "show" flags from all monsters */
	if (repair_mflag_show)
	{
		for (i = 1; i < m_max; i++)
		{
			monster_type *m_ptr;

			/* Get the monster */
			m_ptr = &m_list[i];

			/* Monster is no longer required to be visible */
			m_ptr->mflag &= ~(MFLAG_SHOW);
		}

		/* Done */
		repair_mflag_show = FALSE;
	}

	/* Refresh the extra monster health bars  XXX XXX */
	p_ptr->redraw |= (PR_HEALTH_EXTRA);

	/* Make sure that enough quest monsters exist  XXX XXX */
	if ((p_ptr->cur_quest) && (p_ptr->depth == p_ptr->cur_quest))
	{
		insure_quest_monsters();
	}
}



/*
 * We refresh monsters after every character action that uses energy.  -LM-
 *
 * All monsters detected here, or during that action, will stay visible
 * until after the character's next energy-using action.
 */
static void refresh_monsters_after(void)
{
	int i;

	monster_type *m_ptr;
	monster_race *r_ptr;

	/* Note whether we need to update distances */
	bool do_dist = (p_ptr->update & (PU_DISTANCE)) ? TRUE : FALSE;


	/* We handle monster visibility and distances here */
	p_ptr->update &= ~(PU_MONSTERS | PU_DISTANCE);

	/* Update stuff (especially the character's field of vision) */
	handle_stuff();


	/* Remove "mark" flags from all monsters not magically detected */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];

		/* Monster has been magically detected */
		if (m_ptr->mflag & (MFLAG_SHOW))
		{
			/* Repair "show" flags at the beginning of next turn */
			repair_mflag_show = TRUE;
		}
		else
		{
			/* Monster is no longer required to be visible */
			m_ptr->mflag &= ~(MFLAG_FULL | MFLAG_MARK);
		}
	}


	/* Process the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Handle visibility changes due to telepathy, sensing, detection. */
		if (update_mon(i, do_dist, TRUE))
		{
			continue;
		}

		/* Handle color changes due to shimmering */
		else if ((r_ptr->flags1 & (RF1_ATTR_MULTI)) &&
		         (mon_fully_visible(m_ptr)))
		{
			/* Reraw the monster */
			lite_spot(m_ptr->fy, m_ptr->fx);
		}
	}
}


/*
 * Handle pack overflow	- clefs -
 *
 * We avoid dropping items inscribed {!d} or {!*} if possible.
 * Otherwise, drop objects starting from the bottom of the inventory list.
 * Empty bottles and parchments are therefore most likely to be dropped
 * and spellbooks are relatively safe.
 */
static void pack_overflow(void)
{
	bool allow_ban = TRUE;

	/* Keep dropping things until our pack is no longer too full */
	while (inventory[INVEN_PACK - p_ptr->pack_size_reduce].k_idx)
	{
		int i;
		object_type *o_ptr;

		cptr s;
		char o_name[120];

		/* Assume illegal drop */
		int drop = -1;

		/* Look for an item allowed to drop */
		for (i = INVEN_PACK; i >= 0; i--)
		{
			/* Assume we can drop this object */
			bool banned = FALSE;

			/* Get object */
			o_ptr = &inventory[i];

			/* Skip non-objects */
			if (!o_ptr->k_idx) continue;

			/* Have inscription, and we are still looking for them */
			if ((o_ptr->note) && (allow_ban))
			{
				/* Find a '!' */
				s = strchr(quark_str(o_ptr->note), '!');

				/* Process preventions */
				while (s)
				{
					/* Check the "restriction" */
					if ((s[1] == 'd') || (s[1] == '*'))
					{
						banned = TRUE;
						break;
					}

					/* Find another '!' */
					s = strchr(s + 1, '!');
				}

				/* Skip this object */
				if (banned) continue;
			}

			/* Drop this object */
			drop = i;
			break;
		}

		/* Paranoia -- do not allow all objects to be banned */
		if (drop == -1)
		{
			allow_ban = FALSE;
			continue;
		}

		/* Access the slot to be dropped */
		o_ptr = &inventory[drop];

		/* Disturbing */
		disturb(0, 0);

		/* Warning */
		msg_print("Your pack overflows!");

		/* Describe */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Message */
		msg_format("You drop %s (%c).", o_name, index_to_label(drop));

		/* Drop it (carefully) near the player */
		drop_near(o_ptr, 0, p_ptr->py, p_ptr->px);

		/* Modify, Describe, Optimize */
		inven_item_increase(drop, -255);
		inven_item_describe(drop);
		inven_item_optimize(drop);

		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();

		/* Window stuff (if needed) */
		if (p_ptr->window) window_stuff();
	}
}


/*
 * Process the player
 *
 * Notice the annoying code to handle "pack overflow", which
 * must come first just in case somebody manages to corrupt
 * the savefiles by clever use of menu commands or something.
 *
 * Notice the annoying code to handle "monster memory" changes,
 * which allows us to avoid having to update the window flags
 * every time we change any internal monster memory field, and
 * also reduces the number of times that the recall window must
 * be redrawn.
 *
 * Note that the code to check for user abort during repeated commands
 * and running and resting can be disabled entirely with an option, and
 * even if not disabled, it will only check during every 128th game turn
 * while resting, for efficiency.
 */
static void process_player(void)
{
	u16b temp_wakeup_chance;


	/*** Check for interrupts ***/

	/* Complete resting */
	if (p_ptr->resting < 0)
	{
		/* Basic resting */
		if (p_ptr->resting == -1)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) && (p_ptr->csp == p_ptr->msp))
			{
				disturb(0, 0);
			}
		}

		/* Complete resting */
		else if (p_ptr->resting == -2)
		{
			/* Stop resting */
			if ((p_ptr->chp == p_ptr->mhp) &&
			    (p_ptr->csp == p_ptr->msp) &&
			    !p_ptr->blind && !p_ptr->confused &&
			    !p_ptr->poisoned &&
			    !p_ptr->afraid && !p_ptr->stun && !p_ptr->cut &&
			    !p_ptr->slow && !p_ptr->paralyzed &&
			    !p_ptr->image && !p_ptr->word_recall &&
			    (p_ptr->food < p_ptr->food_bloated))
			{
				/* (mania takes too long to heal) */

				disturb(0, 0);
			}
		}
	}

	/* Check for "player abort" */
	if (p_ptr->running || p_ptr->command_rep ||
	   (p_ptr->resting && !(turn & 0x7F)))
	{
		/* Do not wait */
		inkey_scan = TRUE;

		/* Check for a key */
		if (inkey())
		{
			/* Flush input */
			flush();

			/* Disturb */
			disturb(0, 0);

			/* Flush any pending messages */
			message_flush();

			/* Hack -- Show a Message */
			msg_print("Cancelled.");
		}
	}


	/*** Handle certain refreshes right before player moves ***/

	/* Refresh monsters as needed */
	refresh_monsters_before();

	/* Print "target"  XXX */
	left_panel_display(DISPLAY_TARGET, 0);


	/*** Handle actual user input ***/

	/* Repeat until energy is reduced */
	do
	{
		/* Notice stuff (if needed) */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff (if needed) */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->redraw) redraw_stuff();

		/* Redraw stuff (if needed) */
		if (p_ptr->window) window_stuff();


		/* Place the cursor on the player */
		move_cursor_relative(p_ptr->py, p_ptr->px);

		/* Refresh */
		Term_fresh();


		/* Hack -- handle pack overflow */
		pack_overflow();


		/* Hack -- cancel "lurking browse mode" */
		if (!p_ptr->command_new) p_ptr->command_see = FALSE;


		/* Assume free turn */
		p_ptr->energy_use = 0;

#ifdef ALLOW_BORG
		/* Using the borg. */
		if (count_stop) do_cmd_borg();

		/* Otherwise, paralyzed or Knocked Out */
		else if ((p_ptr->paralyzed) || (p_ptr->stun >= KNOCKED_OUT))
		{
			/* Take a turn */
			p_ptr->energy_use = 100;
		}

#else

		/* Paralyzed or Knocked Out */
		if ((p_ptr->paralyzed) || (p_ptr->stun >= KNOCKED_OUT))
		{
			/* Take a turn */
			p_ptr->energy_use = 100;
		}
#endif

		/* Resting */
		else if (p_ptr->resting)
		{
			/* Timed rest */
			if (p_ptr->resting > 0)
			{
				/* Reduce rest count */
				p_ptr->resting--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}

			/* Take a turn */
			p_ptr->energy_use = 100;
		}

		/* Running */
		else if (p_ptr->running)
		{
			/* Take a step */
			run_step(0);
		}

		/* Repeated command */
		else if (p_ptr->command_rep)
		{
			/* Hack -- Assume messages were seen */
			msg_flag = FALSE;

			/* Clear the top line */
			prt("", 0, 0);

			/* Process the command */
			process_command();

			/* Count this execution */
			if (p_ptr->command_rep)
			{
				/* Count this execution */
				p_ptr->command_rep--;

				/* Redraw the state */
				p_ptr->redraw |= (PR_STATE);
			}
		}

		/* Normal command */
		else
		{
			/* Check monster recall */
			process_player_aux();

			/* Place the cursor on the player */
			move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Get a command (normal) */
			request_command(FALSE);

			/* Process the command */
			process_command();
		}


		/* Action has taken some time */
		if (p_ptr->energy_use)
		{
			/* Use some energy */
			p_ptr->energy -= p_ptr->energy_use;
		}
	}
	while (!p_ptr->energy_use && !p_ptr->leaving);


	/* Hack -- constant hallucination  XXX XXX (inefficient) */
	if (p_ptr->image) p_ptr->redraw |= (PR_MAP);

	/* Refresh monsters */
	refresh_monsters_after();


	/*
	 * Apply and time out certain temporary effects.  XXX XXX XXX
	 */

	/* Dancing feet - blink every player turn, use up some magic */
	if (p_ptr->dancing_feet)
	{
		teleport_player(10, p_ptr->dancing_feet_safe);
		set_dancing_feet(p_ptr->dancing_feet - 1, NULL,
			p_ptr->dancing_feet_safe);
	}

	/* Spell enhancements */
	if (p_ptr->aura_cold)
	{
		set_aura_cold(p_ptr->aura_cold - 1);
	}
	if (p_ptr->aura_fire)
	{
		set_aura_fire(p_ptr->aura_fire - 1);
	}
	if (p_ptr->pois_power)
	{
		set_pois_power(p_ptr->pois_power, p_ptr->pois_power_dur - 1);
	}
	if (p_ptr->chaos_power)
	{
		set_chaos_power(p_ptr->chaos_power, p_ptr->chaos_power_dur - 1);
	}

	/* No longer practicing a skill */
	skill_being_used = S_NOSKILL;


	/* Get ambiant noise level -- lowered when resting */
	if (p_ptr->resting)
		temp_wakeup_chance = p_ptr->base_wakeup_chance / 2;
	else
		temp_wakeup_chance = p_ptr->base_wakeup_chance;

	/* Adjust the ambiant noise level */
	if (temp_wakeup_chance > total_wakeup_chance)
	{
		total_wakeup_chance = temp_wakeup_chance;
	}

	/* Increase noise if necessary */
	if (add_wakeup_chance)
	{
		total_wakeup_chance += add_wakeup_chance;
		add_wakeup_chance = 0;
	}

	/* Limit on noise (100% effective in disturbing monsters) */
	if (total_wakeup_chance > 10000)
	{
		total_wakeup_chance = 10000;
	}

	/* Print "noise" */
	left_panel_display(DISPLAY_NOISE, 0);

	/* Update noise flow information */
	update_noise(FALSE);

	/* Update scent trail */
	update_smell();


	/*
	 * Reset character vulnerability if appropriate.  Will be calculated
	 * by the first member of an animal pack that has a use for it.
	 */
	if (p_ptr->vulnerability == 100)
	{
		/* Reset vulnerability only if the character is fully healed */
		if (p_ptr->chp >= p_ptr->mhp) p_ptr->vulnerability = 0;
	}
	else
	{
		p_ptr->vulnerability = 0;
	}
}


/*
 * Check the scores
 */
static bool any_scores(void)
{
	char buf[1024];
	char tmp[100];
	bool score = FALSE;

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* File exists */
	if (highscore_fd >= 0)
	{
		/* Try to read some data */
		if (!fd_read(highscore_fd, tmp, 80)) score = TRUE;
	}

	/* Shut the high score file */
	fd_close(highscore_fd);

	return (score);
}



/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
	int i;

	/* Hack -- enforce illegal panel */
	p_ptr->wy = dungeon_hgt;
	p_ptr->wx = dungeon_wid;


	/* Not leaving */
	p_ptr->leaving = FALSE;


	/* Reset the "command" vars */
	p_ptr->command_cmd = 0;
	p_ptr->command_new = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_arg = 0;
	p_ptr->command_dir = 0;


	/* Reset shimmer flags */
	shimmer_objects = TRUE;


	/* Disturb */
	disturb(1, 0);


	/* Track maximum dungeon level */
	if (p_ptr->max_depth < p_ptr->depth)
	{
		p_ptr->max_depth = p_ptr->depth;
	}

	/* Choose panel */
	verify_panel(0, FALSE);


	/* Flush messages */
	message_flush();


	/* Hack -- Silence certain status messages */
	character_silent = 1;


	/* Blank the screen */
	Term_clear();


	/* Update various things */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_TORCH);

	/* Fully update the visuals (and monster distances) */
	p_ptr->update |= (PU_FORGET_VIEW | PU_UPDATE_VIEW | PU_DISTANCE);

	/* Update stuff */
	update_stuff();


	/* Redraw main window */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP | PR_EQUIPPY);

	/* Redraw stuff */
	redraw_stuff();


	/* Refresh a lot of additional windows */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);
	p_ptr->window |= (PW_MONSTER | PW_OVERHEAD | PW_M_LIST);

	/* Redraw the windows */
	window_stuff();


	/* Status messages are no longer silenced */
	character_silent = 0;


	/* Update stuff */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();

	/* Update stuff */
	update_stuff();

	/* Redraw stuff */
	redraw_stuff();

	/* Window stuff */
	window_stuff();

	/* Refresh */
	Term_fresh();


	/* Handle delayed death */
	if (p_ptr->is_dead) return;

	/* Mark quest as started  (move this code at some point) */
	if (p_ptr->cur_quest == p_ptr->depth)
	{
		/* Check quests */
		for (i = 0; i < z_info->q_max; i++)
		{
			/* Check for quest */
			if (q_info[i].active_level == p_ptr->depth)
			{
				q_info[i].started = TRUE;

				/* Take note of quest */
				left_panel_display(DISPLAY_QUEST, 0);

				break;
			}
		}
	}

	/* Hack -- Announce (or repeat) the feeling */
	if (!no_feeling_yet) do_cmd_feeling();


	/* Totally new character, no high scores */
	if ((turn <= 1L) && (!any_scores()))
	{
		msg_format("If you press '?' now, you can start a %s tutorial.",
			VERSION_NAME);

		p_ptr->get_help_index = HELP_TUTORIAL;
	}


	/*** Process this dungeon level ***/

	/* Reset the monster generation level */
	monster_level = p_ptr->depth;

	/* Reset the object generation level */
	object_level = p_ptr->depth;

	/* Main loop */
	while (TRUE)
	{
		/* Hack -- Compact the monster list occasionally */
		if (m_cnt + 32 > z_info->m_max) compact_monsters(64);

		/* Hack -- Compress the monster list occasionally */
		if (m_cnt + 32 < m_max) compact_monsters(0);


		/* Hack -- Compact the object list occasionally */
		if (o_cnt + 32 > z_info->o_max) compact_objects(64);

		/* Hack -- Compress the object list occasionally */
		if (o_cnt + 32 < o_max) compact_objects(0);


		/*** Apply energy ***/

		/* Give the player some energy */
		p_ptr->energy += extract_energy[p_ptr->pspeed];

		/* Can the player move? */
		while (p_ptr->energy >= 100 && !p_ptr->leaving)
		{
			/* Process monster with even more energy first */
			process_monsters((byte)(p_ptr->energy + 1));

			/* If still alive */
			if (!p_ptr->leaving)
			{
				/* Process the player */
				process_player();

			}
		}

		/* Process other things that happen during a game turn */
		for (i = 0;; i++)
		{
			/* Notice stuff */
			if (p_ptr->notice) notice_stuff();

			/* Update stuff */
			if (p_ptr->update) update_stuff();

			/* Redraw stuff */
			if (p_ptr->redraw) redraw_stuff();

			/* Redraw stuff */
			if (p_ptr->window) window_stuff();

			/* Hack -- Highlight the player */
			move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Optional fresh */
			if (fresh_after) Term_fresh();

			/* Handle "leaving" */
			if (p_ptr->leaving) break;

			/* Monsters (any that haven't had a chance to move yet) */
			if (i == 0)
			{
				/* Process monsters */
				process_monsters(0);

				/* Reset monsters */
				reset_monsters();
			}

			/* Special effects */
			else if (i == 1)
			{
				/* Process effects */
				process_effects();
			}

			/* The world */
			else if (i == 2)
			{
				/* Process the world */
				process_world();
			}

			/* Count game turns, end this turn */
			else
			{
				/* Count game turns */
				turn++;

				/* End this game turn */
				break;
			}
		}

		/* Handle "leaving" */
		if (p_ptr->leaving) break;
	}
}



/*
 * Process the preference file "<<basename>>.prf".
 */
static void process_name_pref_file(void)
{
	char buf[1024];

	/* Get the "<<basename>>.prf" filename */
	sprintf(buf, "%s.prf", op_ptr->base_name);

	/* Process the "<<basename>>.prf" file */
	(void)process_pref_file(buf);
}



/*
 * Actually play a game.
 *
 * This function is called from a variety of entry points, since both
 * the standard "main.c" file, as well as several platform-specific
 * "main-xxx.c" files, call this function to start a new game with a
 * new savefile, start a new game with an existing savefile, or resume
 * a saved game with an existing savefile.
 *
 * If the "new_game" parameter is true, and the savefile contains a
 * living character, then that character will be killed, so that the
 * player may start a new game with that savefile.  This is only used
 * by the "-n" option in "main.c".
 *
 * If the savefile does not exist, cannot be loaded, or contains a dead
 * (non-wizard-mode) character, then a new game will be started.
 *
 * Several platforms (Windows, Macintosh, Amiga) start brand new games
 * with "savefile" and "op_ptr->base_name" both empty, and initialize
 * them later based on the player name.  To prevent weirdness, we must
 * initialize "op_ptr->base_name" to "PLAYER" if it is empty.
 *
 * Note that we load the RNG state from savefiles (2.8.0 or later) and
 * so we only initialize it if we were unable to load it.  The loading
 * code marks successful loading of the RNG state using the "Rand_quick"
 * flag, which is a hack, but which optimizes loading of savefiles.
 */
void play_game(bool new_game)
{
	int ch;

	/* Hack -- Increase "icky" depth */
	character_icky++;

	/* Verify main term */
	if (!term_screen)
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	Term_activate(term_screen);

	/* Verify minimum size */
	if ((Term->hgt < 24) || (Term->wid < 80))
	{
		quit("main window is too small");
	}

	/* Hack -- set to 25 screen rows */
	Term_rows(FALSE);

	/* Hack -- Turn off the cursor */
	(void)Term_set_cursor(FALSE);

	/* Init RNG */
	if (Rand_quick)
	{
		u32b seed;

		/* Basic seed */
		seed = (time(NULL));

#ifdef SET_UID

		/* Mutate the seed on UNIX machines */
		seed = ((seed >> 3) * (getpid() << 1));

#endif

		/* Use the complex RNG */
		Rand_quick = FALSE;

		/* Seed the "complex" RNG */
		Rand_state_init(seed);
	}

	/* Reset visuals */
	reset_visuals();

	/* Process the "/lib/pref/user.prf" and "/lib/user/user.prf" files */
	(void)process_pref_file("user.prf");


	/* We have not already loaded a savefile, and do not require a new game */
	if ((!character_loaded) && (!new_game))
	{
		/* We have a specific savefile we want to load */
		if (savefile[0])
		{
			/* Blank the screen */
			Term_clear();

			/* Load savefile, note any errors */
			if (load_player(FALSE))
			{
				/* Load character (use a menu) */
				savefile_load(TRUE);
			}
		}

		/* We haven't specified a savefile (usual case) */
		else
		{
			/* Load character (do not force menus/autoload obvious savefiles) */
			savefile_load(FALSE);
		}
	}

	/* Repeat until satisfied */
	while (TRUE)
	{
		/* Character is alive - display, allow player to start over */
		if (character_loaded)
		{
			/* Hack -- Silence certain status messages */
			character_silent = 1;

			/* Update the character  XXX XXX */
			p_ptr->update |= (PU_BONUS | PU_TORCH | PU_HP | PU_MANA | PU_SPELLS);
			update_stuff();

			/* Messages are no longer silenced */
			character_silent = 0;

			/* Display the character */
			display_player(0);

			/* Prompt for it */
			prt("['Q' to quit, 'L' to load another savefile, or any other key to continue]", 23, 3);

			/* Get response */
			ch = inkey();

			/* Quit */
			if (ch == 'Q')
			{
				quit(NULL);
			}

			/* Load another savefile */
			else if ((ch == 'L') || (ch == 'l'))
			{
				/* Continue */
			}

			/* Play a game with this character */
			else
			{
				break;
			}
		}

		/* Roll new character */
		else
		{
			/* Flash a comforting message */
			if ((sf_lives) && (character_existed))
			{
				prt("Using ancestor's monster memory...", 0, 0);

				/* Wait for it */
				(void)inkey();
			}

			/* The dungeon is not ready */
			character_dungeon = FALSE;

			/* Start in town */
			p_ptr->depth = 0;

			/* Hack -- seed for flavors */
			seed_flavor = rand_int(0x10000000);

			/* Hack -- seed for town layout */
			seed_town = rand_int(0x10000000);

			/* Roll up a new character */
			if (player_birth())
			{
				/* Prepare random artifacts. */
				initialize_random_artifacts();

				/* Hack -- enter the world */
				turn = 1;

				/* All done */
				break;
			}

			/* Cancel request for new game */
			new_game = FALSE;
		}

		/* Hack -- avoid wrong message */
		character_existed = FALSE;

		/* Show available savefiles, get a choice */
		savefile_load(TRUE);
	}


	/* Use the name "PLAYER" for savefiles if no other is specified */
	if ((character_loaded) && (strstr(op_ptr->base_name, "PLAYER")))
	{
		process_player_name(FALSE);
	}

	/* Use given name for savefile if specified */
	else
	{
		process_player_name(TRUE);
	}


	/* Flash a message */
	msg_print("Please wait...");

	/* Do not wait */
	inkey_scan = TRUE;

	/* Flush the message */
	message_flush();


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->wizard = TRUE;


	/* Flavor the objects */
	flavor_init();

	/* Load the user preference file "<<basename>>.prf" */
	process_name_pref_file();


	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

	/* Window stuff */
	p_ptr->window |= (PW_MONSTER | PW_M_LIST | PW_MESSAGE);

	/* Window stuff */
	window_stuff();


	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* React to changes */
	Term_xtra(TERM_XTRA_REACT, 0);


	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Character is now "complete" */
	character_generated = TRUE;


	/* Hack -- Decrease "icky" depth */
	character_icky--;


	/* Start playing */
	p_ptr->playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;

	/* Hack -- reset the screen rows */
	if (force_25_rows) Term_rows(FALSE);
	else               Term_rows(TRUE);


	/* Process */
	while (TRUE)
	{
		/* Process the level */
		dungeon();

		/* Notice stuff */
		if (p_ptr->notice) notice_stuff();

		/* Update stuff */
		if (p_ptr->update) update_stuff();

		/* Redraw stuff */
		if (p_ptr->redraw) redraw_stuff();

		/* Window stuff */
		if (p_ptr->window) window_stuff();


		/* Handle "quit and save" */
		if (!p_ptr->playing && !p_ptr->is_dead) break;


		/* Make sure that all messages have been read  XXX */
		message_flush();


		/* Accidental Death */
		if (p_ptr->playing && p_ptr->is_dead)
		{
			/* Mega-Hack -- Allow player to cheat death */
			if ((p_ptr->wizard || cheat_live) && !get_check("Die?"))
			{
				/* Mark social class, reset age, if needed */
				if (p_ptr->sc) p_ptr->sc = p_ptr->age = 0;

				/* Increase age */
				p_ptr->age++;

				/* Mark savefile */
				p_ptr->noscore |= (0x0001);

				/* Message */
				msg_print("You invoke wizard mode and cheat death.");
				message_flush();

				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Restore hit points */
				p_ptr->chp = p_ptr->mhp;
				p_ptr->chp_frac = 0;

				/* Restore spell points */
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;

				/* Hack -- Healing */
				(void)set_blind(0, NULL);
				(void)set_confused(0);
				(void)set_poisoned(0);
				(void)set_diseased(0, NULL);
				(void)set_afraid(0);
				(void)set_paralyzed(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);
				p_ptr->black_breath = FALSE;

				/* Hack -- Prevent starvation */
				(void)set_food(p_ptr->food_bloated - 1);

				/* Hack -- cancel recall */
				if (p_ptr->word_recall) set_recall(0);

				/* Note cause of death XXX XXX XXX */
				strcpy(p_ptr->died_from, "(Cheating death)");

				/* Jump to the town */
				p_ptr->depth = 0;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
		}

		/* Handle "death" */
		if (p_ptr->is_dead) break;

		/* Make a new level */
		generate_cave();

		/*** Update quests ***/

		/* Check for quest failure */
		check_quest_failure(2);
	}

	/* Close stuff */
	close_game();
}
