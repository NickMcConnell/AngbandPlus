/* File: dungeon.c */

/*
 * Regenerate hitpoints and mana.  Handle the passage of time.  Special events.
 * Enter wizard, debug, and borg modes.  Process commands.  Visual refreshes.
 * Handle a character's turn.  Interact with the current dungeon level.  Play
 * a game.
 *
 * Copyright (c) 2007 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"



/*
 * Regenerate hit points
 *
 * The basic rate of hitpoint recovery is 30/10000ths per hitpoint, plus
 * 400/10000.  Therefore, a character with 100 maximum hitpoints will
 * recover (30 * 100) + 400 / 10000 = 3400 / 10000, or about a third of a
 * hitpoint every 10 game turns under normal conditions.  Assuming no
 * modifications, like those for wounds or resting.
 *
 * By comparison, most monsters with 100 maximum hitpoints regain 1.5 hit-
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
	/* Use the standard display and center an 80 column view */
	display_change(DSP_REMEMBER | DSP_SAVE | DSP_CLEAR | DSP_NORM | DSP_CX,
		80, 0);

	/* Skip some lines */
	move_cursor(4, 0);


	/* We have just slain Sauron */
	if (part == MON_SAURON)
	{
		/* Display some text */
		c_roff_centered(TERM_WHITE, "     As Sauron falls, a greater evil is unmasked.  For the Creator of the One Ring, Sauron the Great, was but the servant of another:\n\nMorgoth, Lord of Darkness, awaits you!", 5, 75);
	}

	/* We have just slain Morgoth */
	else if (part == MON_MORGOTH)
	{
		char buf[DESC_LEN];

		/* Get a racial adjective */
		cptr race_adj = race_info[p_ptr->prace].title;

		/* Get a character title (never "winner") */
		strcpy(buf, format("%s", get_title(80, FALSE, FALSE)));

		/* Display some text */
		c_roff_centered(TERM_WHITE, format("     Morgoth lies vanquished, and fear dies with him; Illuvatar's bright creation, our Middle-Earth, is free!  Your skill and valour is the stuff of legend; always we will remember the %s %s who saved us.\n\nWhen ready to assume the crown that awaits you, press \'Q\' to retire.",  race_adj, buf), 5, 75);
	}

	/* Wait for it */
	pause_line(Term->rows - 1);

	/* Restore previous display */
	display_change(DSP_RESTORE | DSP_LOAD, 0, 0);
}


/*
 * If player has inscribed the object with "!!", let him know when it's
 * recharged. -LM-
 */
static void recharged_notice(object_type *o_ptr)
{
	char o_name[DESC_LEN];

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
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

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

	char o_name[DESC_LEN];

	int i;
	u32b f1, f2, f3;

	bool do_acid = FALSE;
	bool do_elec = FALSE;
	bool do_fire = FALSE;
	bool do_cold = FALSE;
	bool do_disen = FALSE;
	bool do_drain = FALSE;
	bool do_curse = FALSE;

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
		if ((o_ptr->flags3 & (TR3_IS_LIT)) &&
		    (o_ptr->tval == TV_LITE) && (!(f3 & (TR3_NOFUEL))))
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
					if (disturb_state) disturb(0, 0);
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
				if (one_in_(3))
				{
					object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
					(void)take_hit(damroll(6, 6), 0,
						format("Your %s is feeding on your soul!", o_name), "a weapon feeding on your soul");
				}
				p_ptr->soul_reserve = 0;
			}
		}

		/* Learn about unaware rings and amulets */
		if ((!object_aware_p(o_ptr)) &&
		    ((o_ptr->tval == TV_RING) || (o_ptr->tval == TV_AMULET)) &&
		    (one_in_(200)))
		{
			k_ptr = &k_info[o_ptr->k_idx];

			/* Sense it */
			sense_object(o_ptr, i, TRUE, FALSE);

			/* Object is now aware */
			if (object_aware_p(o_ptr))
			{
				/* Describe the object (briefly) */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

				/* Message */
				msg_format("Your experience rises.", o_name);

				/* Gain experience */
				gain_exp(MAX(1, k_ptr->level * k_ptr->level / 4), S_NOSKILL);
			}
			flag = TRUE;
		}

		/* Get crushed */
		if ((p_ptr->being_crushed) && (f3 & (TR3_DRAIN_HP)) &&
		    (one_in_(2)))
		{
			/* Damage is proportional to remaining hitpoints */
			int dam = randint(rsqrt(p_ptr->chp));

			/* Take a hit */
			if (dam)
			{
				/* Different text for different kind of objects */
				cptr str = "is crushing you";

				if (o_ptr->artifact_index == ART_LIGHTNING_LANCE)
					str = "electrocutes you";
				else if (is_any_weapon(o_ptr)) str = "attacks you";

				/* Describe the object */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

				/* Ouch! */
				if (o_ptr->artifact_index == ART_LIGHTNING_LANCE)
				{
					elec_dam(dam, 0, format("Your %s %s!", o_name, str),
						"a murderous item");
				}
				else
				{
					(void)take_hit(dam, 0, format("Your %s %s!", o_name, str),
						"a murderous item");
				}
			}
		}

		/* Cursed brands (acid, elec, fire, cold, poison) hurt */
		if ((f3 & (TR3_LIGHT_CURSE)) &&
		    (f1 & (TR1_BRAND_FIRE | TR1_BRAND_ACID | TR1_BRAND_ELEC |
		           TR1_BRAND_COLD | TR1_BRAND_POIS | TR1_BRAND_FLAME |
		           TR1_BRAND_VENOM)) &&
		    (one_in_(15)))
		{
			/* Damage is proportional to remaining hitpoints */
			int dam = rand_range(p_ptr->chp / 9, (p_ptr->chp + 2) / 3);

			/* Store the brands */
			byte brands_array[5] = { 0, 0, 0, 0, 0 };
			byte brands_num = 0;

			/* Describe the object kind */
			strip_name(o_name, o_ptr->k_idx);


			/* Run through the brands */
			if (f1 & (TR1_BRAND_ACID))
			{
				brands_array[0] = 1;
				brands_num++;
			}
			if (f1 & (TR1_BRAND_ELEC))
			{
				brands_array[1] = 1;
				brands_num++;
			}
			if (f1 & (TR1_BRAND_FLAME | TR1_BRAND_FIRE))
			{
				brands_array[2] = 1;
				brands_num++;
			}
			if (f1 & (TR1_BRAND_COLD))
			{
				brands_array[3] = 1;
				brands_num++;
			}
			if (f1 & (TR1_BRAND_VENOM | TR1_BRAND_POIS))
			{
				brands_array[4] = 1;
				brands_num++;
			}

			/* Randomize choice of brand */
			brands_num = randint(brands_num);

			/* Choose the brand */
			for (i = 0;brands_num > 0;i++)
			{
				if (brands_array[i]) brands_num--;
			}

			/* Fire off the brand */
			if (i == 1)
			{
				acid_dam(dam, 0, format("Acid drips from your %s!", o_name),
					"an acidic weapon");
			}
			if (i == 2)
			{
				elec_dam(dam, 0, format("Your %s shocks you!", o_name),
					"an electric weapon");
			}
			if (i == 3)
			{
				fire_dam(dam, 0, format("Your %s burns you!", o_name),
					"a burning weapon");
			}
			if (i == 4)
			{
				cold_dam(dam, 0, format("Your %s freezes you!", o_name),
					"a freezing weapon");
			}
			if (i == 5)
			{
				if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
				{
					if (!p_ptr->poisoned)
						msg_format("Your %s poisons you!", o_name);
					(void)set_poisoned(p_ptr->poisoned + 20);
				}
			}
		}

		/* Cursed slays call out to monsters */
		if ((f3 & (TR3_LIGHT_CURSE)) &&
		    (f1 & (TR1_SLAY_ANIMAL | TR1_SLAY_EVIL | TR1_SLAY_UNDEAD |
		           TR1_SLAY_DEMON | TR1_SLAY_ORC | TR1_SLAY_TROLL |
		           TR1_SLAY_GIANT | TR1_SLAY_DRAGON | TR1_KILL_DRAGON)) &&
		    (one_in_(30)))
		{
			/* Describe the object kind */
			strip_name(o_name, o_ptr->k_idx);

			/* Run through the slays */
			if (f1 & (TR1_SLAY_ANIMAL))
			{
				aggravate_monster_race(RF3_ANIMAL, o_name, "animals");
			}
			if (f1 & (TR1_SLAY_EVIL))
			{
				aggravate_monster_race(RF3_EVIL, o_name, "evil");
			}
			if (f1 & (TR1_SLAY_UNDEAD))
			{
				aggravate_monster_race(RF3_UNDEAD, o_name, "undead");
			}
			if (f1 & (TR1_SLAY_DEMON))
			{
				aggravate_monster_race(RF3_DEMON, o_name, "demons");
			}
			if (f1 & (TR1_SLAY_ORC))
			{
				aggravate_monster_race(RF3_ORC, o_name, "orcs and ogres");
			}
			if (f1 & (TR1_SLAY_TROLL))
			{
				aggravate_monster_race(RF3_TROLL, o_name, "trolls");
			}
			if (f1 & (TR1_SLAY_GIANT))
			{
				aggravate_monster_race(RF3_GIANT, o_name, "giants");
			}
			if (f1 & (TR1_SLAY_DRAGON | TR1_KILL_DRAGON))
			{
				aggravate_monster_race(RF3_DRAGON, o_name, "dragons");
			}
		}

		/* Plague-infected armor */
		if ((o_ptr->ego_item_index == EGO_PLAGUE) && (one_in_(10)))
		{
			int dam = 5 + p_ptr->power / 2;

			/* Inflict disease (but not when near death) */
			if (p_ptr->chp >= 20) disease(&dam);
		}

		/* Mind-Melting (terrible, but short-lived) */
		if ((o_ptr->ego_item_index == EGO_MIND_MELT) &&
		    (!p_ptr->paralyzed) && (one_in_(4)))
		{
			/* Describe the object kind */
			strip_name(o_name, o_ptr->k_idx);

			/* The headgear vanishes */
			if (one_in_(20))
			{
				/* Message */
				msg_format("The %s you are wearing has vanished!", o_name);

				/* Destroy the headgear */
				inven_item_increase(i, -1);
				inven_item_optimize(i);
			}

			/* It stays, and attacks you again */
			else
			{
				/* Message */
				msg_format("Your %s attacks your mind!", o_name);

				/* Hit the character with a psionic attack */
				(void)explosion(0, 0, p_ptr->py, p_ptr->px, 25 + p_ptr->power / 2, GF_PSI);
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
				/*
				 * Recharge (wearables only recharge at 1/3rd speed in the
				 * backpack, just as they do on the floor).
				 */
				if (!(is_wearable(o_ptr)) || !(turn % 30))
				o_ptr->timeout--;

				/* Notice changes, notify if object is inscribed. */
				if (!(o_ptr->timeout))
				{
					recharged_notice(o_ptr);
					flag = TRUE;
				}
			}
		}

		/* Handle cursed blankets */
		if ((o_ptr->tval == TV_JUNK) && (o_ptr->sval == SV_BLANKET) &&
		    (cursed_p(o_ptr)))
		{
			/* Learn about cursed blankets the hard way */
			object_aware(o_ptr);
			object_known(o_ptr);

			/* Something nasty happens */
			flag = TRUE;

			/* Blankets of Melting */
			if (o_ptr->flags2 & (TR2_RES_ACID)) do_acid = TRUE;

			/* Blankets of Electrocution */
			if (o_ptr->flags2 & (TR2_RES_ELEC)) do_elec = TRUE;

			/* Blankets of Burning */
			if (o_ptr->flags2 & (TR2_RES_FIRE)) do_fire = TRUE;

			/* Blankets of Shattering */
			if (o_ptr->flags2 & (TR2_RES_COLD)) do_cold = TRUE;

			/* Blankets of Disenchantment */
			if (o_ptr->flags2 & (TR2_RES_DISEN)) do_disen = TRUE;

			/* Blankets of Draining */
			if (o_ptr->flags2 & (TR2_RES_DRAIN)) do_drain = TRUE;

			/* Blankets of Cursing */
			if (o_ptr->flags3 & (TR3_BLESSED)) do_curse = TRUE;
		}

		/* Handle athelas spoilage */
		if (o_ptr->tval == TV_FOOD && o_ptr->sval == SV_FOOD_ATHELAS)
		{
			/* Should wilt in around 1 day */
			if (one_in_(10000 / o_ptr->number))
			{
				msg_print("Your athelas wilts!");
				inven_item_increase(i, -1);
				inven_item_optimize(i);
			}
		}
	}

	/* Apply effects of cursed blankets */
	if (do_acid)  (void)inven_damage(set_acid_destroy, 30, 70);
	if (do_elec)  (void)inven_damage(set_elec_destroy, 30, 100);
	if (do_fire)  (void)inven_damage(set_fire_destroy, 30, 80);
	if (do_cold)  (void)inven_damage(set_cold_destroy, 30, 90);
	if (do_disen) (void)apply_disenchant(0);
	if (do_drain) (void)apply_draining(0);
	if (do_curse) curse_equipment(p_ptr->power);

	/* Notice changes */
	if (flag)
	{
		/* Window stuff */
		p_ptr->window |= (PW_INVEN);
	}
}


/*
 * Base noise can be adjusted by various factors.
 */
static s16b base_noise(void)
{
	s32b tmp = (s32b)p_ptr->base_wakeup_chance;

	/* Characters with some stealth skill make less noise in the dark  XXX */
	if (no_light() && (!p_ptr->blind))
	{
		int skill = get_skill(S_STEALTH, 0, 100);
		int add = rsqrt(skill);
		tmp *= 5L;
		tmp /= (s32b)(5L + add);
	}

	return ((s16b)tmp);
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
#endif  /* ALLOW_BORG */

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

	/* Create new monsters */
	if (p_ptr->depth)
	{
		/* Base odds against a new monster (200 to 1) */
		int odds = MAX_M_ALLOC_CHANCE;

		int max_m_cnt = (2 * p_ptr->depth / 3) + 20;

		/* Do not overpopulate the dungeon (important) */
		if (m_cnt > max_m_cnt) odds += 4 * (m_cnt - max_m_cnt);

		/* Fewer monsters if stealthy, more if deep */
		odds += get_skill(S_STEALTH, -40, 80);
		odds -= p_ptr->depth / 2;

		/* Check for creature generation */
		if (one_in_(odds))
		{
			int slp = FALSE;

			/* Sneaky characters make monsters sleepy */
			if (get_skill(S_STEALTH, 0, 40) > rand_int(100)) slp = TRUE;

			/* Make a new monster */
			(void)alloc_monster(MAX_SIGHT + 5, slp);
		}
	}


	/*** Damage over Time ***/

	/* Natural vitality allows faster recovery from some conditions */
	temp = 1;
	if (p_ptr->vitality) temp = 4;

	/* Take damage from poison */
	if ((p_ptr->poisoned) && (p_ptr->necro_rage <= NECRO_WEAKNESS_LENGTH))
	{
		/* Take damage (increases with severity of poison) */
		if (take_hit(randint(p_ptr->poisoned > 400 ? 10 :
			(p_ptr->poisoned+39) / 40), 0, NULL, "poison")) return;
	}

	/* Take damage from disease */
	if (p_ptr->diseased)
	{
		/* Take damage (increases with severity of disease) */
		if (take_hit(randint(p_ptr->diseased > 600 ? 20 :
			(p_ptr->diseased+29) / 30), 0, NULL, "a fatal disease")) return;
	}

	/* Take damage from cuts */
	if ((p_ptr->cut) &&
	    (p_ptr->berserk <= BERSERK_WEAKNESS_LENGTH) &&
	    (p_ptr->necro_rage <= NECRO_WEAKNESS_LENGTH))
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
		if (take_hit(i, 0, NULL, "a fatal wound")) return;
	}

	/* Handle manic-depressive fits */
	if ((p_ptr->mania > 0) && (one_in_(100)))
	{
		/* Mania */
		if (one_in_(2))
		{
			temp = rand_range(15, 30);

			/* Message */
			msg_print("You go into a manic fit!");

			/* Speed up */
			(void)set_fast(p_ptr->fast + temp);

			/* Recover any fear */
			set_bold(p_ptr->bold + temp);
		}

		/* Depression */
		else
		{
			temp = rand_range(20, 50);

			/* Message */
			msg_print("You suddenly feel lethargic!");

			/* Slow down */
			(void)set_slow(p_ptr->slow + temp);

			/* Sometimes get frightened */
			if ((!p_ptr->resist_fear) && (one_in_(3)))
			{
				msg_print("You feel scared!");
				set_afraid(p_ptr->afraid + temp);
			}
		}
	}

	/* Hack -- Slowly recover lost luck when moving about  XXX XXX */
	if ((p_ptr->luck < 100) && (p_ptr->depth) && (!p_ptr->resting))
	{
		if (one_in_(50))
		{
			(void)set_luck(p_ptr->luck + 1, "You feel less unlucky...");
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
		if (take_hit(i, 0, NULL, "starvation")) return;
	}

	/* Getting Faint */
	if (p_ptr->food < p_ptr->food_fainting)
	{
		/* Faint occasionally */
		if (!p_ptr->paralyzed && (one_in_(10)))
		{
			/* Message */
			message(MSG_RED, 500, "You faint from the lack of food.");
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

	/* Unsanctified */
	if (p_ptr->unsanctified)
	{
		set_unsanctified(p_ptr->unsanctified - 1);
	}

	/* Self Knowledge */
	if (p_ptr->self_knowledge)
	{
		set_self_knowledge(p_ptr->self_knowledge - 1, NULL);
	}

	/* Shapechanges and form-shifts */
	if (p_ptr->wraithform)
	{
		(void)set_wraithform(p_ptr->wraithform - 1);
	}
	if (p_ptr->form_dur)
	{
		(void)shapechange_temp(p_ptr->form_dur - 1, p_ptr->schange);
	}


	/*** Poison and Stun and Cut ***/

	/* Poison */
	if (p_ptr->poisoned)
	{
		int adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + temp);
		if (p_ptr->prace == RACE_HOBBIT) adjust += 2;

		/* Apply some healing */
		(void)set_poisoned(p_ptr->poisoned - adjust);
	}

	/* Disease */
	if (p_ptr->diseased)
	{
		/* Disease is hard to shake off */
		int adjust = div_round(adj_con_fix[p_ptr->stat_ind[A_CON]] + temp, 4);

		/* Hobbits are resilient */
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
		if (p_ptr->prace == RACE_HOBBIT) adjust += 2;

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
			message(TERM_L_RED, 100, "The Black Breath attacks your skills!");
			lose_exp(dam * 100, TRUE);
		}

		/* Usually attack unspent exp */
		else
		{
			p_ptr->exp -= (dam - 1);
			lose_exp(1, TRUE);
		}
	}


	/* Slowly reduce recent thefts (note that you can rest, or hide in town) */
	if ((num_recent_thefts > 0) && !(turn % 3500))
	{
		num_recent_thefts--;

		if (num_recent_thefts == get_skill(S_STEALTH, 2, 4))
		{
			message(MSG_YELLOW, 400,
				"You sense that the Pits are slowly beginning to calm down about your thievery.");
		}
		else if (!num_recent_thefts)
		{
			message(MSG_L_GREEN, 400,
				"You sense that the Pits have forgotten your thievery.");
		}
	}


	/*** Process Inventory ***/
	process_world_aux_inven();

	/*** Process Home ***/
	process_world_aux_home();


	/* Process objects in the dungeon */
	for (i = 1; i < o_max; i++)
	{
		/* Get the object */
		o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Recharge objects (only one time in three, though)  -LM- */
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

		/* Handle light sources (only if lit and use fuel) */
		if ((o_ptr->flags3 & (TR3_IS_LIT)) && (o_ptr->tval == TV_LITE))
		{
			u32b f1, f2, f3;

			/* Get flags */
			object_flags(o_ptr, &f1, &f2, &f3);

			if (!(f3 & (TR3_NOFUEL)))
			{
				/* Fuel goes down */
				if (o_ptr->pval > 0) o_ptr->pval--;

				/* Light eventually goes out */
				if (o_ptr->pval <= 0) o_ptr->flags3 &= ~(TR3_IS_LIT);
			}
		}

		/* Handle athelas spoilage */
		if (o_ptr->tval == TV_FOOD && o_ptr->sval == SV_FOOD_ATHELAS)
		{
			/* Should wilt in around 1 day */
			if (one_in_(10000 / o_ptr->number))
			{
				if (player_has_los_bold(o_ptr->iy, o_ptr->ix)) msg_print("The athelas wilts.");
				floor_item_increase(i, -1);
				floor_item_optimize(i);
			}
		}
	}


	/*** Involuntary Movement ***/

	/* Mega-Hack -- Random teleportation XXX XXX XXX */
	if ((p_ptr->teleport) && (one_in_(100)))
	{
		/* Teleport player */
		teleport_player(40, FALSE, FALSE);

		/* Disturbing */
		message_format(0, 1000, "Your equipment whisks you about!");
		disturb(0, 0);
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
			do_cmd_feeling(TRUE);

			/* Update the level indicator */
			p_ptr->redraw |= (PR_DEPTH);

			/* Panic if things are really dangerous */
			if ((feeling == 2) && (!p_ptr->resist_fear) && (!p_ptr->afraid))
			{
				set_afraid(25);
			}

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
			int length = WEATHER_LENGTH;

			/*
			 * Hack -- deep in the dungeon, the character moves faster,
			 * and has better control of weather, and so weather needs to
			 * change more often to stay interesting.  XXX XXX
			 */
			if (p_ptr->depth >= 65) length -= WEATHER_LENGTH / 10;
			if (p_ptr->depth >= 75) length -= WEATHER_LENGTH / 10;
			if (p_ptr->depth >= 85) length -= WEATHER_LENGTH / 10;
			if (p_ptr->depth >= 90) length -= WEATHER_LENGTH / 10;
			if (p_ptr->depth >= 95) length -= WEATHER_LENGTH / 10;

			/* Reset time until next random weather change */
			p_ptr->tim_weath = rand_range(length, length * 2);

			/* Change the weather randomly */
			change = change_weather(0, 0, 0);
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
	if (!cheat_wizard)
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
	p_ptr->noscore |= (CHEAT_WIZARD);

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
	if (!cheat_debug)
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
	p_ptr->noscore |= (CHEAT_DEBUG);

	/* Okay */
	return (TRUE);
}

#endif



#ifdef ALLOW_BORG

/*
 * Verify use of "borg" mode
 */
static bool verify_borg_mode(void)
{
	/* Ask first time */
	if (!cheat_borg)
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
	p_ptr->noscore |= (CHEAT_BORG);

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
	int i;
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

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Cast a spell */
		case 'm':
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
			do_cmd_look(0);
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

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals('\0');
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
			do_cmd_talents(TALENT_UTILITY);
			break;
		}

		case 'p':
		{
			do_cmd_talents(TALENT_WARRIOR);
			break;
		}

		/* Handle shapechange (either talent or unchange) */
		case ']':
		{
			if (get_skill(S_SHAPECHANGE, 0, 100)) do_cmd_talents(TALENT_SHAPE);
			else do_cmd_unchange(TRUE);
			break;
		}

		/* Barehanded fighting */
		case '|':
		{
			do_cmd_weapon_switch();
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
			do_cmd_feeling(FALSE);
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

		/* Hack -- Save and don't quit */
		case KTRL('S'):
		{
			do_cmd_save_game(FALSE);
			break;
		}

		/* Save and quit */
		case KTRL('X'):
		{
			/* Stop playing */
			p_ptr->playing = FALSE;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		/* Save and quit, or retire */
		case 'Q':
		{
			if (!p_ptr->total_winner && !ironman_play)
			{
				/* Stop playing */
				p_ptr->playing = FALSE;

				/* Leaving */
				p_ptr->leaving = TRUE;
			}
			else
			{
				do_cmd_quit();
			}
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

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}

		/* Light or douse light sources */
		case '(':
		{
			do_cmd_light_and_douse();
			break;
		}

		/* Mouse press */
		case MOUSEKEY:
		{
			mouseaction_type *mouse = &cur_mouse_action;

			/* Start looking on mouse left double-click (in main or map term) */
			if ((mouse->button == MOUSE_L_DBLCLICK) &&
			    ((mouse->term == TERM_MAIN) || (mouse->term == TERM_MAP)))
			{
				/* Look, jumping to the mouse position */
				do_cmd_look(TARGET_MOUS);
			}

			/* Otherwise, ignore (for now) */
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


	/* HACK -- Clean up:  cancel any special cursor visibility */
	for (i = 0; i < TERM_MAX; i++) inkey_cursor_hack[i] = 0;
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

			/* Window stuff -- hopefully, this is not necessary */
			/* window_stuff(); */
		}
	}
}

/*
 * Glowing objects.  -LM-
 *
 * We use a pre-calculated table (that handles radii of <= 5) for efficiency.
 *
 * This function can handle glowing objects of radius 0, but at present they
 * are forbidden to glow.
 */
static void glow_object(int y0, int x0, int rad)
{
	int grid, i;
	int y, x, y1, x1;

	/* We can handle light sources up to a radius of 5 */
	int grid_limit;

	int adjust_y = y0 - 5;
	int adjust_x = x0 - 5;

	bool no_los;


	/* Ignore or correct illegal radii */
	if (rad < 0) return;
	if (rad > 5) rad = 5;

	/* Get the number of grids within the given radius */
	grid_limit = grids_in_radius[rad];

	/* Scan the los table until all legal grids are checked */
	for (i = 0, grid = 0; grid < grid_limit; grid++)
	{
		/* First two table entries locate this grid */
		y = los_nearby_table[i++] + adjust_y;
		x = los_nearby_table[i++] + adjust_x;

		/* Assume LOS */
		no_los = FALSE;

		/* Only check if in bounds */
		if (!in_bounds(y, x))
		{
			/* Scan past the next end-of-grid marker */
			while (los_nearby_table[i++] != 100); /* loop */
			continue;
		}
		else
		{
			/* Scan all potential LOS-blocking grids */
			while (los_nearby_table[i] != 100)
			{
				/* We already have a wall in the way */
				if (no_los)
				{
					/* Skip past all the tests */
					i++;
					continue;
				}

				/* Get the grid */
				y1 = los_nearby_table[i++] + adjust_y;
				x1 = los_nearby_table[i++] + adjust_x;

				/* Check bounds (should never fail) */
				if (in_bounds(y1, x1))
				{
					/* Note grids which block LOS */
					if (!cave_los_bold(y1, x1)) no_los = TRUE;
				}
			}

			/* Skip past end-of-grid marker */
			i++;

			/* Turn on the light */
			if (!no_los)
			{
				/* Light this grid */
				cave_info[y][x] |= (CAVE_LITE);

				/* Remember grid */
				lite_g[lite_n++] = GRID(y, x);

				/* Refresh the grid */
				lite_spot(y, x);
			}
		}
	}
}

/*
 * Some refreshes are done just before a character takes his turn.  -LM-
 *
 * There are (at least) two major ways of handling visual updates.
 *
 * Traditionally, Angband has refreshed things as actions happen or just
 * afterwards.  This has the advantage that the screen display properly
 * reflects current knowledge every game turn.
 *
 * Alongside this method, Sangband is also introducing display updates
 * right before the character takes a turn.  This offers several
 * advantages:
 *      Firstly, it is much easier to code correctly.  Visibility
 * changes, especially persistent, limited-duration, or random ones,
 * can be made with confidence that they will be reflected on screen
 * whenever the player gets control of the game.  This was especially
 * important in getting the new sensing code right.
 *      Secondly, this second method is more efficient.  Not only are
 * many display changes made less often, but they are made only as
 * quickly as the player plays the game.  Instead of having, say, to
 * handle shining objects whenever they are manipulated in some way, we
 * can just stick in a check in this function.
 */
static void refresh_visuals_before(void)
{
	int i;
	int y, x, iy, ix;
	int old_lite_n = lite_n;


	/* Refresh the extra monster health bars */
	p_ptr->redraw |= (PR_HEALTH_EXTRA);

	/* Update object list window */
	p_ptr->window |= (PW_O_LIST);


	/* Make sure that enough quest monsters exist  XXX XXX */
	if ((p_ptr->cur_quest) && (p_ptr->depth == p_ptr->cur_quest))
	{
		insure_quest_monsters();
	}

	/* Some temporary light is in the dungeon */
	if (lite_n)
	{
		/* Erase the light */
		for (i = 0; i < lite_n; i++)
		{
			y = GRID_Y(lite_g[i]);
			x = GRID_X(lite_g[i]);

			/* No longer in the array */
			cave_info[y][x] &= ~(CAVE_LITE);

			/* Optionally -- Forget floor grids that do not have permanent light */
			if ((!remember_seen_grids) && !(cave_info[y][x] & (CAVE_GLOW)) &&
			    (cave_floor_bold(y, x)))
			{
				cave_info[y][x] &= ~(CAVE_MARK);
			}

			/* Refresh this grid */
			lite_spot(y, x);
		}

		/* Array is erased */
		lite_n = 0;
	}


	/* Process the character's light source -- only if radius 1 or greater */
	if (p_ptr->cur_lite > 0) glow_object(p_ptr->py, p_ptr->px, p_ptr->cur_lite);


	/* Process objects in the dungeon */
	if (!p_ptr->blind)
	{
		for (i = 1; i < o_max; i++)
		{
			/* Get the object */
			object_type *o_ptr = &o_list[i];

			/* Skip dead objects */
			if (!o_ptr->k_idx) continue;

			/* Get location */
			iy = o_ptr->iy;
			ix = o_ptr->ix;

			/* Objects in field of view can shine and shimmer */
			if ((in_bounds_fully(iy, ix)) &&
				 (cave_info[iy][ix] & (CAVE_VIEW)))
			{
				u32b f1, f2, f3;

				/* Get object attributes */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Notice lit objects */
				if (f3 & (TR3_LITE))
				{
					o_ptr->marked = TRUE;
					lite_spot(iy, ix);
				}

				/* Handle multi-hued and special objects */
				if ((!p_ptr->image) && (o_ptr->marked) &&
				    ((f3 & (TR3_ATTR_MULTI)) || (o_ptr->artifact_index) ||
				     (o_ptr->ego_item_index)))
				{
					/* Get color */
					int a = shimmer_object(o_ptr, f1, f2, f3);

					/* Save and display color (if special) */
					if (a >= 1)
					{
						o_ptr->marked = COLORED_OBJ_MIN + a;
						lite_spot(iy, ix);
					}
				}
			}


			/* Handle light sources (only if lit and nearby) */
			if ((o_ptr->flags3 & (TR3_IS_LIT)) &&
			    (distance(p_ptr->py, p_ptr->px, iy, ix) <= MAX_SIGHT + 5))
			{
				/* Get light radius */
				int rad = get_object_pval(o_ptr, TR_PVAL_LIGHT);

				u32b f1, f2, f3;

				/* Get object attributes */
				object_flags(o_ptr, &f1, &f2, &f3);

				/* Objects without permanent light require fuel */
				if ((o_ptr->tval == TV_LITE) && !(f3 & (TR3_NOFUEL)))
				{
					/* Require fuel */
					if (o_ptr->pval <= 0) continue;

					/* Torches with low fuel burn more dimly */
					if (o_ptr->sval == SV_LITE_TORCH)
					{
						if ((o_ptr->pval < (TORCH_LOW + 1000)) && (rad > 1)) rad -= 1;
					}
				}

				/* Shine only if light radius is positive */
				if (rad >= 1) glow_object(iy, ix, rad);
			}
		}
	}

	/* Temporary lighting often affects the view */
	if (lite_n || old_lite_n) p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}


/*
 * We refresh monsters after every character action that uses energy.  -LM-
 *
 * See detection notes for "update_mon()".
 */
static void refresh_visuals_after(void)
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


	/* Remove "show" flags from all monsters */
	if (repair_mflag_show)
	{
		for (i = 1; i < m_max; i++)
		{
			/* Get the monster */
			m_ptr = &m_list[i];

			/* Monster is no longer required to be visible */
			m_ptr->mflag &= ~(MFLAG_SHOW);
		}

		/* Done */
		repair_mflag_show = FALSE;
	}


	/* Randomize the detection seed value (0-9999) */
	seed_detection = rand_int(10000);

	/* Process the monsters */
	for (i = 1; i < m_max; i++)
	{
		/* Get the monster */
		m_ptr = &m_list[i];
		r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;


		/* Monster has been magically detected */
		if (m_ptr->mflag & (MFLAG_SHOW))
		{
			/* Monster must stay visible through next turn */
			repair_mflag_show = TRUE;
		}
		else
		{
			/* Monster is no longer required to be visible */
			m_ptr->mflag &= ~(MFLAG_FULL | MFLAG_MARK);
		}


		/* Handle visibility changes */
		if (update_mon(i, do_dist)) continue;

		/* If monster not already redrawn, shimmer it */
		if ((r_ptr->flags1 & (RF1_ATTR_MULTI)) &&
		    (mon_fully_visible(m_ptr)))
		{
			/* Redraw the monster */
			lite_spot(m_ptr->fy, m_ptr->fx);
		}
	}
}


/*
 * Handle pack overflow.
 *
 * We avoid dropping items inscribed {!d} or {!*} if possible.
 * Otherwise, drop objects starting from the bottom of the inventory list.
 * Empty bottles and parchments are therefore most likely to be dropped
 * and spellbooks are relatively safe.
 *
 * Allow inscriptions to prevent dropping.  - clefs -
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
		char o_name[DESC_LEN];

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

		/* Get the object to be dropped */
		o_ptr = &inventory[drop];

		/* Disturbing */
		disturb(0, 0);

		/* Warning */
		msg_print("Your pack overflows!");

		/* Describe */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Message */
		msg_format("You drop %s (%c).", o_name, index_to_label(drop));

		/* Drop it (carefully) near the player */
		drop_near(o_ptr, 0, p_ptr->py, p_ptr->px,
			DROP_HERE | DROP_CHAR_DROP);

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
void process_player(void)
{
	int i;

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
		if (inkey(ALLOW_CLICK))
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

	/* Refresh monsters, objects, etc. as needed */
	refresh_visuals_before();

	/* Print "target"  XXX */
	left_panel_display(DISPLAY_TARGET, 0);

	/* Request appropriate music for the current danger level */
	danger_music_level(FALSE);


	/* Optional hitpoint warning */
	if (p_ptr->hitpoint_warning)
	{
		/* Clear the warning */
		p_ptr->hitpoint_warning = FALSE;

		/* Message with delay */
		message(MSG_HITPOINT_WARN, 100, "*** LOW HITPOINT WARNING! ***");
	}



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
		(void)Term_fresh();

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

		/* Picking up objects */
		else if (p_ptr->notice & (PN_PICKUP1))
		{
			/* Hack -- Force the cursor to be hidden in some cases  XXX XXX */
			if ((!highlight_player) && (use_special_map)) inkey_cursor_hack[TERM_MAP] = -1;

			/* Recursively call the pickup function, use energy */
			p_ptr->energy_use = py_pickup(1) * 10;
			p_ptr->notice &= ~(PN_PICKUP0 | PN_PICKUP1);

			/* Allow the cursor to be shown again */
			inkey_cursor_hack[TERM_MAP] = 0;
		}

		/* Noticing objects (allow pickup) */
		else if (p_ptr->notice & (PN_PICKUP0))
		{
			/* Hack -- Force the cursor to be hidden in some cases  XXX XXX */
			if ((!highlight_player) && (use_special_map)) inkey_cursor_hack[TERM_MAP] = -1;

			/* Recursively call the pickup function, use energy */
			p_ptr->energy_use = py_pickup(0) * 10;
			p_ptr->notice &= ~(PN_PICKUP0 | PN_PICKUP1);

			/* Allow the cursor to be shown again */
			inkey_cursor_hack[TERM_MAP] = 0;
		}

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

			/* Increment resting turns counter */
			p_ptr->resting_turns++;

			/* Note end of rest */
			if (!p_ptr->resting) left_panel_display(DISPLAY_REGEN, 0);
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

			p_ptr->total_turns++;
		}
	}
	while (!p_ptr->energy_use && !p_ptr->leaving);


	/* Hack -- constant hallucination  XXX XXX (inefficient) */
	if (p_ptr->image) p_ptr->redraw |= (PR_MAP);

	/* Refresh visuals -- unless leaving */
	if (!p_ptr->leaving) refresh_visuals_after();


	/*
	 * Apply and time out certain temporary effects.  XXX XXX XXX
	 */

	/* Dancing feet - blink every player turn, use up some magic */
	if ((p_ptr->dancing_feet) && (rand_int(100) < p_ptr->energy_use))
	{
		teleport_player(10, p_ptr->dancing_feet_safe, FALSE);
		set_dancing_feet(p_ptr->dancing_feet - 1, NULL,
			p_ptr->dancing_feet_safe);
		disturb(0, 0);
	}

	/* Spell enhancements */
	if ((p_ptr->aura_cold) && (rand_int(100) < p_ptr->energy_use))
	{
		set_aura_cold(p_ptr->aura_cold - 1);
	}
	if ((p_ptr->aura_fire) && (rand_int(100) < p_ptr->energy_use))
	{
		set_aura_fire(p_ptr->aura_fire - 1);
	}
	if ((p_ptr->pois_power) && (rand_int(100) < p_ptr->energy_use))
	{
		set_pois_power(p_ptr->pois_power, p_ptr->pois_power_dur - 1);
	}
	if ((p_ptr->chaos_power) && (rand_int(100) < p_ptr->energy_use))
	{
		set_chaos_power(p_ptr->chaos_power, p_ptr->chaos_power_dur - 1);
	}

	/* No longer practicing a skill */
	skill_being_used = S_NOSKILL;

	/* Allowed to automatically pick up things again */
	p_ptr->auto_pickup_okay = TRUE;


	/* Old noise dies down */
	if (TRUE)
	{
		/* Calculate how many game turns passed during this move */
		int game_turns_elapsed = div_round(p_ptr->energy_use,
			extract_energy[p_ptr->pspeed]);

		/* We decrease noise by 20% (at least 100) every game turn */
		for (i = 0; i < game_turns_elapsed; i++)
		{
			total_wakeup_chance -= MAX(100, total_wakeup_chance / 5);
		}

		/* But the character always makes some noise */
		if (total_wakeup_chance < base_noise())
		    total_wakeup_chance = base_noise();
	}

	/* New noise is generated */
	if (TRUE)
	{
		/* Apply base noise, depending on % of turn taken */
		if (!p_ptr->resting)
		{
			total_wakeup_chance += p_ptr->energy_use * base_noise() / 100;
		}

		/* Apply temporary noise increases */
		total_wakeup_chance += add_wakeup_chance;
		add_wakeup_chance = 0;
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
	if (p_ptr->vulnerability < 100)
	{
		p_ptr->vulnerability = 0;
	}
	else
	{
		/* Time out charges */
		p_ptr->vulnerability--;
	}
}


/*
 * Check the scores
 */
static bool any_scores(void)
{
	char buf[1024];
	char tmp[DESC_LEN];
	bool score = FALSE;

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_APEX, "scores.raw");

	/* Open the binary high score file, for reading */
	highscore_fd = fd_open(buf, O_RDONLY);

	/* File exists */
	if (highscore_fd >= 0)
	{
		/* Try to read some data */
		if (!fd_read(highscore_fd, tmp, sizeof(tmp))) score = TRUE;
	}

	/* Shut the high score file */
	(void)fd_close(highscore_fd);

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
	(void)Term_clear();


	/* Combine / Reorder the pack */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Notice stuff */
	notice_stuff();


	/* Request everything */
	p_ptr->update = 0xFFFFFFFF;

	/* Redraw everything */
	p_ptr->redraw = 0xFFFFFFFF;

	/* Refresh everything */
	p_ptr->window = 0xFFFFFFFF;

	/* Handle stuff */
	handle_stuff();


	/* Status messages are no longer silenced */
	character_silent = 0;

	/* Refresh the main term */
	(void)Term_fresh();


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
	if (!no_feeling_yet) do_cmd_feeling(FALSE);


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


		/* Process things that happen during a game turn */
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

			/* Hack -- Highlight the character */
			move_cursor_relative(p_ptr->py, p_ptr->px);

			/* Optional fresh */
			if (fresh_after) (void)Term_fresh();

			/* Handle "leaving" */
			if (p_ptr->leaving) break;

			/* Monsters and the character (and other "entities") */
			if (i == 0)
			{
				/* Process character and monsters, give both energy */
				process_entities();
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
	(void)strnfmt(buf, sizeof(buf), "%s.prf", op_ptr->base_name);

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


	/* Blank the screen and set to standard screen display */
	display_change(DSP_LOCK | DSP_NORM, 0, 0);


	/* Verify main term */
	if (!term_main)
	{
		quit("main window does not exist");
	}

	/* Make sure main term is active */
	(void)Term_activate(term_main);

	/* Verify minimum size (at present, 80x24) */
	if ((Term->cols < term_size_min[TERM_MAIN][0]) || (Term->rows < term_size_min[TERM_MAIN][1]))
	{
		quit(format("main window is too small (is %dx%d, needs to be %dx%d)",
			Term->cols, Term->rows, term_size_min[TERM_MAIN][0], term_size_min[TERM_MAIN][1]));
	}


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


	/* Character is not "complete" yet */
	character_generated = FALSE;


	/* We have not already loaded a savefile, and do not require a new game */
	if ((!character_loaded) && (!new_game))
	{
		/* We have a specific savefile we want to load */
		if (savefile[0])
		{
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

	/* We want a new game, but have also specified a savefile */
	else if ((savefile[0]) && (new_game))
	{
		/* Load savefile and note any errors, but don't go to the menu */
		(void)load_player(FALSE);
	}


	/* Repeat until satisfied */
	while (TRUE)
	{
		/* Character is alive - display, allow player to start over */
		if ((character_loaded) && (!new_game))
		{
			char buf[1024];

			/* Hack -- Silence certain status messages */
			character_silent = 1;

			/* Update the character  XXX XXX */
			p_ptr->update |= (PU_BONUS | PU_TORCH | PU_HP | PU_MANA | PU_SPELLS);
			update_stuff();

			/* Messages are no longer silenced */
			character_silent = 0;

			/* Display the character */
			display_player(0, TRUE);

			/* Center the prompt */
			center_string(buf, sizeof(buf),
				"['Q' to quit, 'C' to play another character, or any other key to continue]", display_width());

			/* Prompt for it */
			prt(buf, 23, 0);

			/* Hide the cursor */
			inkey_cursor_hack[TERM_MAIN] = -1;

			/* Get response */
			ch = inkey(ALLOW_CLICK);

			/* Allow the cursor to be shown again */
			inkey_cursor_hack[TERM_MAIN] = 0;

			/* Quit */
			if ((ch == 'Q') || (ch == KTRL('X')))
			{
				quit(NULL);
			}

			/* Play another character */
			else if ((ch == 'C') || (ch == 'c') || (ch == ESCAPE))
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
			/* Cancel request for new game */
			new_game = FALSE;

			/* Flash a comforting message */
			if ((sf_lives) && (character_existed))
			{
				prt("Using ancestor's monster memory...", 0, 0);

				/* Wait for it */
				(void)inkey(ALLOW_CLICK);
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
		}

		/* Hack -- avoid strange messages */
		character_existed = FALSE;

		/* Show available savefiles, get a choice */
		savefile_load(TRUE);
	}

	/* Character has been loaded; process name, don't change savename */
	if (character_loaded)
	{
		process_player_name(FALSE);
	}

	/* Use given name for savefile if character has not been loaded */
	else
	{
		process_player_name(TRUE);
	}

	/* A character has now been loaded */
	character_loaded = TRUE;


	/* Flash a message */
	prt("Please wait...", 23, 33);


	/* Hack -- Enter wizard mode */
	if (arg_wizard && enter_wizard_mode()) p_ptr->wizard = TRUE;


	/* Flavor the objects */
	flavor_init();

	/* Update preferences for chosen race */
	(void)process_pref_file("race.prf");

	/* Update preferences for chosen realm */
	(void)process_pref_file("realm.prf");

	/* Load the user preference file "<<basename>>.prf" */
	process_name_pref_file();



	/* Set or clear "rogue_like_commands" if requested */
	if (arg_force_original) rogue_like_commands = FALSE;
	if (arg_force_roguelike) rogue_like_commands = TRUE;

	/* React to changes */
	(void)Term_xtra(TERM_XTRA_REACT, 0);


	/* Generate a dungeon level if needed */
	if (!character_dungeon) generate_cave();

	/* Character is now "complete" */
	character_generated = TRUE;


	/* Display the main view */
	display_change(DSP_UNLOCK, 0, 0);

	/* Calculate the size of the map display, if not using a special map window */
	if (!use_special_map) calc_map_size(Term->cols - COL_MAP, Term->rows - ROW_MAP - 1);


	/* Start playing */
	p_ptr->playing = TRUE;

	/* Hack -- Enforce "delayed death" */
	if (p_ptr->chp < 0) p_ptr->is_dead = TRUE;


	/* Play a game with the user */
	while (TRUE)
	{
		/* Start playing music (or change song) */
		danger_music_level(TRUE);

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
			if ((p_ptr->wizard || (p_ptr->character_type == PCHAR_BEGINNER)) && !get_check("Die?"))
			{
				/* Increase deaths */
				p_ptr->deaths++;

				/* Mark savefile */
				p_ptr->noscore |= (CHEAT_DEATH);

				/* Message */
				if (p_ptr->wizard)
					msg_print("You invoke wizard mode and cheat death.");
				else
					msg_print("You cheat death.");
				message_flush();

				/* Cheat death */
				p_ptr->is_dead = FALSE;

				/* Pay the ferryman (unless in wizard mode) */
				if (!p_ptr->wizard) p_ptr->au = 0;

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
				(void)set_food(p_ptr->food_bloated - 50);

				/* Hack -- cancel recall */
				if (p_ptr->word_recall) set_recall(0);

				/* Hack -- cancel hitpoint warning */
				p_ptr->hitpoint_warning = FALSE;

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
