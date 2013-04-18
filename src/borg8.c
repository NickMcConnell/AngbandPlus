/* File: borg8.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG


/*
 * Mega-Hack -- extract some "hidden" variables
 *
 * XXX XXX XXX This step would not be necessary if more info
 * was available on the screen.  Perhaps we should track the
 * maximal value of "b_ptr->stat_cur" so we have "b_ptr->stat_max".
 */
static void borg_hidden(void)
{
	int i;

	int stat_add[6];


	/* Clear "stat_add[]" */
	for (i = 0; i < 6; i++) stat_add[i] = 0;

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		auto_item *item = &borg_items[i];

		/* Skip empty items */
		if (!item->iqty) continue;

		/* Affect stats */
		if (item->flags1 & TR1_STR) stat_add[A_STR] += item->pval;
		if (item->flags1 & TR1_INT) stat_add[A_INT] += item->pval;
		if (item->flags1 & TR1_WIS) stat_add[A_WIS] += item->pval;
		if (item->flags1 & TR1_DEX) stat_add[A_DEX] += item->pval;
		if (item->flags1 & TR1_CON) stat_add[A_CON] += item->pval;
		if (item->flags1 & TR1_CHR) stat_add[A_CHR] += item->pval;
	}

	/* Mega-Hack -- Guess at "b_ptr->stat_cur[]" */
	for (i = 0; i < 6; i++)
	{
		int value;

		/* Hack -- reverse the known bonus */
		value = modify_stat_value(borg_base_stat[i], -stat_add[i]);

		/* Hack -- save the maximum/current stats */
		b_ptr->stat_max[i] = b_ptr->stat_cur[i] = value;
	}
}


/*
 * Object flags -- name
 */
static cptr info_name[96] =
{
	"STR",
	"INT",
	"WIS",
	"DEX",
	"CON",
	"CHR",
	"XXX1",
	"XXX2",
	"STEALTH",
	"SEARCH",
	"INFRA",
	"TUNNEL",
	"SPEED",
	"BLOWS",
	"SHOTS",
	"MIGHT",
	"SLAY_ANIMAL",
	"SLAY_EVIL",
	"SLAY_UNDEAD",
	"SLAY_DEMON",
	"SLAY_ORC",
	"SLAY_TROLL",
	"SLAY_GIANT",
	"SLAY_DRAGON",
	"KILL_DRAGON",
	"XXX5",
	"XXX6",
	"XXX7",
	"BRAND_ACID",
	"BRAND_ELEC",
	"BRAND_FIRE",
	"BRAND_COLD",
	"SUST_STR",
	"SUST_INT",
	"SUST_WIS",
	"SUST_DEX",
	"SUST_CON",
	"SUST_CHR",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"XXX5",
	"XXX6",
	"IM_ACID",
	"IM_ELEC",
	"IM_FIRE",
	"IM_COLD",
	"RES_ACID",
	"RES_ELEC",
	"RES_FIRE",
	"RES_COLD",
	"RES_POIS",
	"RES_FEAR",
	"RES_LITE",
	"RES_DARK",
	"RES_BLIND",
	"RES_CONFU",
	"RES_SOUND",
	"RES_SHARD",
	"RES_NEXUS",
	"RES_NETHR",
	"RES_CHAOS",
	"RES_DISEN",
	"SLOW_DIGEST",
	"FEATHER",
	"LITE",
	"REGEN",
	"TELEPATHY",
	"SEE_INVIS",
	"FREE_ACT",
	"HOLD_LIFE",
	"XXX1",
	"XXX2",
	"XXX3",
	"XXX4",
	"IMPACT",
	"TELEPORT",
	"AGGRAVATE",
	"DRAIN_EXP",
	"IGNORE_ACID",
	"IGNORE_ELEC",
	"IGNORE_FIRE",
	"IGNORE_COLD",
	"XXX5",
	"XXX6",
	"BLESSED",
	"ACTIVATE",
	"INSTA_ART",
	"EASY_KNOW",
	"HIDE_TYPE",
	"SHOW_MODS",
	"XXX7",
	"LIGHT_CURSE",
	"HEAVY_CURSE",
	"PERMA_CURSE"
};


/*
 * Object flags -- text
 *
 * Only includes the "special" flags which we care about.
 */
static cptr info_text[96] =
{
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"It sustains your strength.",
	"It sustains your intelligence.",
	"It sustains your wisdom.",
	"It sustains your dexterity.",
	"It sustains your constitution.",
	"It sustains your charisma.",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	"It provides resistance to acid.",
	"It provides resistance to electricity.",
	"It provides resistance to fire.",
	"It provides resistance to cold.",
	"It provides resistance to poison.",
	"It provides resistance to fear.",
	"It provides resistance to light.",
	"It provides resistance to dark.",
	"It provides resistance to blindness.",
	"It provides resistance to confusion.",
	"It provides resistance to sound.",
	"It provides resistance to shards.",
	"It provides resistance to nexus.",
	"It provides resistance to nether.",
	"It provides resistance to chaos.",
	"It provides resistance to disenchantment.",
	"It slows your metabolism.",
	"It induces feather falling.",
	"It provides permanent light.",
	"It speeds your regenerative powers.",
	"It gives telepathic powers.",
	"It allows you to see invisible monsters.",
	"It provides immunity to paralysis.",
	"It provides resistance to life draining.",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL
};


/*
 * Parse "examination" screen
 */
static void borg_parse_examine(void)
{
	int y;

	byte t_a;
	
	char buf[80];

	/* Parse information */
	for (y = 2; y < 22; y++)
	{
		/* Get the message */
		if (0 == borg_what_text(0, y, -65, &t_a, buf))
		{
			int i;

			/* Scan flags */
			for (i = 0; i < 96; i++)
			{
				/* Check info text */
				if (info_text[i] && prefix(buf, info_text[i]))
				{
					auto_item *item = &borg_items[borg_exam_item];

					/* Message */
					borg_note(format("Examine found flag '%s'", info_name[i]));

					/* Notice missing flag */
					if (((i / 32 == 0) && !(item->flags1 & (1L << (i % 32)))) ||
					    ((i / 32 == 1) && !(item->flags2 & (1L << (i % 32)))) ||
					    ((i / 32 == 2) && !(item->flags3 & (1L << (i % 32)))))
					{
						/* Message */
						borg_note(format("Examine using flag '%s'", info_name[i]));

						/* Hack -- Memorize flag */
						sprintf(borg_exam_note, "BORG_%s", info_name[i]);
					}

					/* Done */
					break;
				}
			}
		}
	}
}


/*
 * Think about the world and perform an action
 *
 * Check inventory/equipment/spells/panel once per "turn"
 *
 * Process "store" and other modes when necessary
 *
 * Note that the non-cheating "inventory" and "equipment" parsers
 * will get confused by a "weird" situation involving an ant ("a")
 * on line one of the screen, near the left, next to a shield, of
 * the same color, and using --(-- the ")" symbol, directly to the
 * right of the ant.  This is very rare, but perhaps not completely
 * impossible.  I ignore this situation.
 *
 * The handling of stores is a complete and total hack, but seems
 * to work remarkably well, considering...  Note that while in a
 * store, time does not pass, and most actions are not available,
 * and a few new commands are available ("sell" and "purchase").
 *
 * Note the use of "cheat" functions to extract the current inventory,
 * the current equipment, the current panel, and the current spellbook
 * information.  These can be replaced by (very expensive) "parse"
 * functions, which cause an insane amount of "screen flashing".
 *
 * Technically, we should attempt to parse all the messages that indicate
 * that it is necessary to re-parse the equipment and/or the inventory, and
 * only set the appropriate flags at that point.  This would greatly reduce
 * the amount of screen scraping performed, especially when the full screen
 * sub-windows are not available.
 *
 * For paranoia, we could always select items and spells using capital letters,
 * and keep a global verification buffer, and induce failure and recheck the
 * inventory/equipment any time we get a mis-match.  We could even do some of
 * the state processing by hand, for example, charge reduction and such.  This
 * would help us to keep track of how long we have held objects, especially if
 * we attempt to do "item tracking" in the inventory extraction code.
 */
void borg_think(void)
{
	int i, j;

	byte t_a;

	char buf[128];

	int sub_inven = -1;
	int sub_equip = -1;


	/*** Check the special windows ***/

	/* Scan windows */
	for (j = 1; j < 8; j++)
	{
		/* No window */
		if (!angband_term[j]) continue;

		/* Hack -- verify minimum size */
		if (angband_term[j]->hgt < 24) continue;
		if (angband_term[j]->wid < 80) continue;

		/* Check for reliable "PW_INVEN" display */
		if ((op_ptr->window_flag[j] & (PW_INVEN)) == PW_INVEN)
		{
			sub_inven = j;
		}

		/* Check for reliable "PW_EQUIP" display */
		if ((op_ptr->window_flag[j] & (PW_EQUIP)) == PW_EQUIP)
		{
			sub_equip = j;
		}
	}


	/*** Handle "examine" screen ***/

	/* Only when requested */
	if (borg_exam_item >= 0)
	{
		/* Note */
		borg_note(format("Examining item %d...", borg_exam_item));

		/* Examine */
		borg_parse_examine();

		/* More info XXX XXX XXX */
		if ((0 == borg_what_text(15, 22, 10, &t_a, buf)) &&
		    (streq(buf, "-- more --")))
		{
			/* Send see more info key */
			borg_keypress(' ');

			/* Continue */
			return;
		}

		/* Send all done key */
		borg_keypress(ESCAPE);

		/* Inscribe (if possible) */
		if (!streq(borg_exam_note, ""))
		{
			/* Inscribe */
			borg_send_inscribe_item(borg_exam_item, borg_exam_note);
		}

		/* Forget */
		borg_exam_item = -1;
		strcpy(borg_exam_note, "");

		/* Done */
		return;
	}


	/*** Process inventory/equipment ***/

	/* Watch sub-window if possible */
	if (borg_do_equip && (sub_equip > 0))
	{
		/* Only do it once */
		borg_do_equip = FALSE;

		/* Watch the "equip" window */
		borg_watch_equip(sub_equip);
	}

	/* Watch sub-window if possible */
	if (borg_do_inven && (sub_inven > 0))
	{
		/* Only do it once */
		borg_do_inven = FALSE;

		/* Watch the "inven" screen */
		borg_watch_inven(sub_inven);
	}


	/* Parse "Equipment" mode */
	if (borg_prompt &&
	    (0 == borg_what_text(0, 0, 12, &t_a, buf)) &&
	    (streq(buf, "(Equipment) ")))
	{
		/* Parse the "equip" screen */
		borg_parse_equip();

		/* Send all done key */
		borg_keypress(ESCAPE);

		/* Done */
		return;
	}


	/* Parse "Inventory" mode */
	if (borg_prompt &&
	    (0 == borg_what_text(0, 0, 12, &t_a, buf)) &&
	    (streq(buf, "(Inventory) ")))
	{
		/* Parse the "inven" screen */
		borg_parse_inven();

		/* Send all done key */
		borg_keypress(ESCAPE);

		/* Done */
		return;
	}


	/* Check "equip" */
	if (borg_do_equip)
	{
		/* Only do it once */
		borg_do_equip = FALSE;

		/* Send action (view equipment) */
		borg_keypress('e');

		/* Done */
		return;
	}

	/* Check "inven" */
	if (borg_do_inven)
	{
		/* Only do it once */
		borg_do_inven = FALSE;

		/* Send action (view inventory) */
		borg_keypress('i');

		/* Done */
		return;
	}


	/*** Find books ***/

	/* Only if needed */
	if (borg_do_spell && (borg_do_spell_aux == 0))
	{
		/* Assume no books */
		for (i = 0; i < 9; i++) borg_base_book[i] = -1;

		/* Scan the pack */
		for (i = 0; i < INVEN_PACK; i++)
		{
			auto_item *item = &borg_items[i];

			/* Skip non-books */
			if (item->tval != mb_ptr->spell_book) continue;

			/* Note book locations */
			borg_base_book[item->sval] = i;
		}
	}


	/*** Process books ***/

	/* Hack -- Warriors never browse */
	if (b_ptr->pclass == 0) borg_do_spell = FALSE;

	/* Hack -- Blind or Confused prevents browsing */
	if (borg_base_is_blind || borg_base_is_confused) borg_do_spell = FALSE;

	/* Hack -- Stop doing spells when done */
	if (borg_do_spell_aux > 8) borg_do_spell = FALSE;

	/* Check for "Browsing" mode */
	if (borg_prompt &&
	    (0 == borg_what_text(0, 0, 11, &t_a, buf)) &&
	    (streq(buf, "(Browsing) ")))
	{
		/* Parse the "spell" screen */
		borg_parse_spell(borg_do_spell_aux);

		/* Advance to the next book */
		borg_do_spell_aux++;

		/* Send all done key */
		borg_keypress(ESCAPE);

		/* Done */
		return;
	}

	/* Check "spells" */
	while ((borg_do_spell) && (borg_do_spell_aux < 9))
	{
		/* Look for the book */
		i = borg_base_book[borg_do_spell_aux];

		/* Enter the "spell" screen */
		if (i >= 0)
		{
			/* Send action (browse book) */
			borg_keypress('b');

			/* Send item index (book) */
			borg_send_item_index(i);

			/* Done */
			return;
		}

		/* Advance to the next book */
		borg_do_spell_aux++;
	}


	/*** Handle stores ***/

	/* Mega-Hack -- Check for being in a store */
	if (!borg_prompt &&
	    (0 == borg_what_text(3, 5, 16, &t_a, buf)) &&
	    (streq(buf, "Item Description")))
	{
		/* Assume the Home */
		borg_base_shop = 7;

		/* Extract the "store" name */
		if (0 == borg_what_text(50, 3, -20, &t_a, buf))
		{
			int i;

			/* Check the store names */
			for (i = 0; i < 7; i++)
			{
				int feat = FEAT_SHOP_HEAD+i;
				cptr name = (f_name + f_info[feat].name);
				if (prefix(buf, name)) borg_base_shop = i;
			}
		}

		/* Hack -- reset page/more */
		borg_shops[borg_base_shop].page = 0;
		borg_shops[borg_base_shop].more = 0;

		/* React to new stores */
		if (borg_do_browse_what != borg_base_shop)
		{
			/* Clear all the items */
			for (i = 0; i < 24; i++)
			{
				/* XXX Wipe the ware */
				WIPE(&borg_shops[borg_base_shop].ware[i], auto_item);
			}

			/* Save the store */
			borg_do_browse_what = borg_base_shop;
		}


		/* Extract the "page", if any */
		if ((0 == borg_what_text(20, 5, 8, &t_a, buf)) &&
		    (prefix(buf, "(Page "))) /* --)-- */
		{
			/* Take note of the page */
			borg_shops[borg_base_shop].more = 1;
			borg_shops[borg_base_shop].page = D2I(buf[6]) - 1;
		}

		/* React to disappearing pages */
		if (borg_do_browse_more != borg_shops[borg_base_shop].more)
		{
			/* Clear the second page */
			for (i = 12; i < 24; i++)
			{
				/* XXX Wipe the ware */
				WIPE(&borg_shops[borg_base_shop].ware[i], auto_item);
			}

			/* Save the new one */
			borg_do_browse_more = borg_shops[borg_base_shop].more;
		}

		/* Extract the current gold (unless in home) */
		if (0 == borg_what_text(68, 19, -9, &t_a, buf))
		{
			/* Save the gold, if valid */
			if (buf[0]) b_ptr->au = atol(buf);
		}

		/* Parse the items */
		for (i = 0; i < 12; i++)
		{
			int n = borg_shops[borg_base_shop].page * 12 + i;

			char desc[80];
			char cost[10];

			/* Default to "empty" */
			desc[0] = '\0';
			cost[0] = '\0';

			/* Verify "intro" to the item --(-- */
			if ((0 == borg_what_text(0, i + 6, 3, &t_a, buf)) &&
			    (buf[0] == I2A(n)) && (buf[1] == ')') && (buf[2] == ' '))
			{
				int k;

				/* Extract the item description */
				if (0 != borg_what_text(3, i + 6, -65, &t_a, desc))
				{
					desc[0] = '\0';
				}

				/* Strip trailing spaces */
				for (k = strlen(desc); (k > 0) && (desc[k-1] == ' '); k--) /* loop */;
				desc[k] = '\0';

				/* Extract the item cost in stores */
				if (borg_base_shop != 7)
				{
					if (0 != borg_what_text(68, i + 6, -9, &t_a, cost))
					{
						cost[0] = '\0';
					}
				}
			}

			/* Ignore "unchanged" descriptions */
			if (streq(desc, borg_shops[borg_base_shop].ware[n].desc)) continue;

			/* Analyze the item */
			borg_item_analyze(&borg_shops[borg_base_shop].ware[n], desc);

			/* Hack -- Save the declared cost (if any) */
			borg_shops[borg_base_shop].ware[n].cost = atol(cost);
		}

		/* Hack -- browse as needed */
		if (borg_shops[borg_base_shop].more && borg_do_browse)
		{
			/* Send next page key */
			borg_keypress(' ');

			/* Done browsing */
			borg_do_browse = FALSE;

			/* Done */
			return;
		}

		/* Recheck inventory */
		borg_do_inven = TRUE;

		/* Recheck equipment */
		borg_do_equip = TRUE;

		/* Recheck store items */
		borg_do_browse = TRUE;

		/* Examine the inventory */
		borg_notice();

		/* Evaluate the current world */
		borg_base_power = borg_power();

		/* Hack -- Support clean cancel */
		if (borg_active > 0) borg_active--;

		/* Hack -- Support clean cancel */
		if (borg_active == 0) return;
	
		/* Think until done */
		if (borg_think_store()) return;

		/* Oops */
		borg_oops("think_store failed");

		/* Oops */
		return;
	}



	/*** Determine panel ***/

	/* Hack -- Check for "sector" mode */
	if (borg_prompt &&
	    (0 == borg_what_text(0, 0, 24, &t_a, buf)) &&
	    (prefix(buf, "Map sector ")))
	{
		/* Hack -- get the panel info */
		b_ptr->wy = D2I(buf[12]) * PANEL_HGT + atoi(&buf[14]);
		b_ptr->wx = D2I(buf[19]) * PANEL_WID + atoi(&buf[21]);

		/* Send all done key */
		borg_keypress(ESCAPE);

		/* Done */
		return;
	}

	/* Check equipment */
	if (borg_do_panel)
	{
		/* Only do it once */
		borg_do_panel = FALSE;

		/* Send action (view panel info) */
		borg_keypress('L');

		/* Done */
		return;
	}


	/*** Paranoia -- catch unexpected prompts ***/

	/* Notice prompts */
	if (borg_prompt)
	{
		/* Oops */
		borg_oops("unexpected prompt");

		/* Oops */
		return;
	}


	/*** Analyze the Frame ***/

	/* Analyze the frame */
	if (borg_do_frame)
	{
		/* Only once */
		borg_do_frame = FALSE;

		/* Analyze the "frame" */
		borg_update_frame();
	}


	/*** Re-activate Tests ***/

	/* Check equip again later */
	borg_do_equip = TRUE;

	/* Check inven again later */
	borg_do_inven = TRUE;

	/* Check panel again later */
	borg_do_panel = TRUE;

	/* Check frame again later */
	borg_do_frame = TRUE;


	/*** Analyze status ***/

	/* Track best level */
	if (b_ptr->max_lev < b_ptr->lev) b_ptr->max_lev = b_ptr->lev;
	if (b_ptr->max_depth < b_ptr->depth) b_ptr->max_depth = b_ptr->depth;


	/*** Think about it ***/

	/* Increment the clock */
	borg_time++;

	/* Extract some "hidden" variables */
	borg_hidden();

	/* Examine the equipment/inventory */
	borg_notice();

	/* Evaluate the current world */
	borg_base_power = borg_power();

	/* Examine the screen */
	borg_update();

	/* Hack -- Support clean cancel */
	if (borg_active > 0) borg_active--;

	/* Hack -- Support clean cancel */
	if (borg_active == 0) return;

	/* Do something */
	if (borg_think_dungeon()) return;

	/* Oops */
	borg_oops("think_dungeon failed");

	/* Oops */
	return;
}



/*
 * Hack -- methods of hurting a monster (order not important).
 *
 * See "message_pain()" for details.
 */
static cptr suffix_pain[] =
{
	" barely notices.",
	" flinches.",
	" squelches.",
	" quivers in pain.",
	" writhes about.",
	" writhes in agony.",
	" jerks limply.",

	" shrugs off the attack.",
	" snarls with pain.",
	" yelps in pain.",
	" howls in pain.",
	" howls in agony.",
	/* " writhes in agony.", */
	" yelps feebly.",

	" ignores the attack.",
	" grunts with pain.",
	" squeals in pain.",
	" shrieks in pain.",
	" shrieks in agony.",
	/* " writhes in agony.", */
	" cries out feebly.",

	/* " shrugs off the attack.", */
	/* " grunts with pain.", */
	" cries out in pain.",
	" screams in pain.",
	" screams in agony.",
	/* " writhes in agony.", */
	/* " cries out feebly.", */

	NULL
};


/*
 * Hack -- methods of killing a monster (order not important).
 *
 * See "mon_take_hit()" for details.
 */
static cptr prefix_kill[] =
{
	"You have killed ",
	"You have slain ",
	"You have destroyed ",
	NULL
};


/*
 * Hack -- methods of monster death (order not important).
 *
 * See "project_m()", "do_cmd_fire()", "mon_take_hit()" for details.
 */
static cptr suffix_died[] =
{
	" dies.",
	" is destroyed.",
	" dissolves!",
	" shrivels away in the light!",
	NULL
};


/*
 * Hack -- methods of hitting the player (order not important).
 *
 * The "drools", "insults", "moans", and "begs you for money" messages
 * are ignored (see below) since they are annoying.  XXX XXX XXX
 *
 * See "make_attack_normal()" for details.
 */
static cptr suffix_hit_by[] =
{
	" hits you.",				/* RBM_HIT */
	" touches you.",			/* RBM_TOUCH */
	" punches you.",			/* RBM_PUNCH */
	" kicks you.",				/* RBM_KICK */
	" claws you.",				/* RBM_CLAW */
	" bites you.",				/* RBM_BITE */
	" stings you.",				/* RBM_STING */
	/* xxx */					/* RBM_XXX1 */
	" butts you.",				/* RBM_BUTT */
	" crushes you.",			/* RBM_CRUSH */
	" engulfs you.",			/* RBM_ENGULF */
	/* xxx */					/* RBM_XXX2 */
	" crawls on you.",			/* RBM_CRAWL */
	/* xxx */					/* RBM_DROOL */
	" spits on you.",			/* RBM_SPIT */
	/* xxx */					/* RBM_XXX3 */
	" gazes at you.",			/* RBM_GAZE */
	" wails at you.",			/* RBM_WAIL */
	" releases spores at you.",	/* RBM_SPORE */
	/* xxx */					/* RBM_XXX4 */
	/* xxx */					/* RBM_BEG */
	/* xxx */					/* RBM_INSULT */
	/* xxx */					/* RBM_MOAN */
	/* xxx */					/* RBM_XXX5 */
	NULL
};


/*
 * Hack -- ignorable methods of hitting the player (order not important).
 *
 * The "drools", "insults", "moans", and "begs you for money" messages
 * must be matched so they can be ignored.  XXX XXX XXX
 *
 * See "make_attack_normal()" for details.
 */
static cptr suffix_hit_by_ignorable[] =
{
	" drools on you.",

	" begs you for money.",

	" insults you!",
	" insults your mother!",
	" gives you the finger!",
	" humiliates you!",
	" defiles you!",
	" dances around you!",
	" makes obscene gestures!",
	" moons you!!!",

	" seems sad about something.",
	" asks if you have seen his dogs.",
	" tells you to get off his land.",
	" mumbles something about mushrooms.",

	NULL
};


/*
 * Hack -- methods of casting spells at the player (order important).
 *
 * Note that the "S/He/it concentrates on her/his/its body." message is
 * actually matched elsewhere, due to the weirdness of the possessive.
 *
 * Note that the "S/He/it concentrates on her/his/its wounds." message is
 * actually matched elsewhere, due to the weirdness of the possessive.
 *
 * See "make_attack_spell()" for details.
 */
static cptr suffix_spell[] =
{
	" makes a high pitched shriek.",				/* RF4_SHRIEK */
	" does something.",								/* RF4_XXX2X4 */
	" does something.",								/* RF4_XXX3X4 */
	" does something.",								/* RF4_XXX4X4 */
	" fires an arrow.",								/* RF4_ARROW_1 */
	" fires an arrow!",								/* RF4_ARROW_2 */
	" fires a bolt.",							/* RF4_ARROW_3 */
	" fires a bolt!",							/* RF4_ARROW_4 */
	" breathes acid.",								/* RF4_BR_ACID */
	" breathes lightning.",							/* RF4_BR_ELEC */
	" breathes fire.",								/* RF4_BR_FIRE */
	" breathes frost.",								/* RF4_BR_COLD */
	" breathes gas.",								/* RF4_BR_POIS */
	" breathes nether.",							/* RF4_BR_NETH */
	" invokes light.",								/* RF4_BR_LITE */
	" invokes darkness.",							/* RF4_BR_DARK */
	" breathes chemicals.",							/* RF4_BR_CONF */
	" sneezes very loudly.",								/* RF4_BR_SOUN */
	" breathes chaos.",								/* RF4_BR_CHAO */
	" breathes disenchantment.",					/* RF4_BR_DISE */
	" breathes nexus.",								/* RF4_BR_NEXU */
	" warps time around you.",								/* RF4_BR_TIME */
	" commands you to stay.",							/* RF4_BR_INER */
	" warps space around you.",							/* RF4_BR_GRAV */
	" breathes shards.",							/* RF4_BR_SHAR */
	" breathes plasma.",							/* RF4_BR_PLAS */
	" breathes forcefully.",								/* RF4_BR_WALL */
	" blasts you with raw mana.",								/* RF4_BR_MANA */
	" does something.",								/* RF4_XXX5X4 */
	" does something.",								/* RF4_XXX6X4 */
	" does something.",								/* RF4_XXX7X4 */
	" does something.",								/* RF4_XXX8X4 */

	" casts an acid ball.",							/* RF5_BA_ACID */
	" casts a lightning ball.",						/* RF5_BA_ELEC */
	" casts a fire ball.",							/* RF5_BA_FIRE */
	" casts a frost ball.",							/* RF5_BA_COLD */
	" casts a stinking cloud.",						/* RF5_BA_POIS */
	" casts a nether ball.",						/* RF5_BA_NETH */
	" gestures fluidly.",							/* RF5_BA_WATE */
	" invokes a mana storm.",						/* RF5_BA_MANA */
	" invokes a darkness storm.",					/* RF5_BA_DARK */
	" draws psychic energy from you!",				/* RF5_DRAIN_MANA */
	" gazes deep into your eyes.",					/* RF5_MIND_BLAST */
	" looks deep into your eyes.",					/* RF5_BRAIN_SMASH */
	" points at you and curses.",					/* RF5_CAUSE_1 */
	" points at you and curses horribly.",			/* RF5_CAUSE_2 */
	" points at you, incanting terribly!",			/* RF5_CAUSE_3 */
	" points at you, screaming the word DIE!",		/* RF5_CAUSE_4 */
	" casts a acid bolt.",							/* RF5_BO_ACID */
	" casts a lightning bolt.",						/* RF5_BO_ELEC */
	" casts a fire bolt.",							/* RF5_BO_FIRE */
	" casts a frost bolt.",							/* RF5_BO_COLD */
	" does something.",								/* RF5_BO_POIS */
	" casts a nether bolt.",						/* RF5_BO_NETH */
	" casts a water bolt.",							/* RF5_BO_WATE */
	" casts a mana bolt.",							/* RF5_BO_MANA */
	" casts a plasma bolt.",						/* RF5_BO_PLAS */
	" casts an ice bolt.",							/* RF5_BO_ICEE */
	" casts a magic missile.",						/* RF5_MISSILE */
	" casts a fearful illusion.",					/* RF5_SCARE */
	" casts a spell, burning your eyes!",			/* RF5_BLIND */
	" creates a mesmerising illusion.",				/* RF5_CONF */
	" drains power from your muscles!",				/* RF5_SLOW */
	" stares deep into your eyes!",					/* RF5_HOLD */

	" concentrates on XXX body.",					/* RF6_HASTE */
	" does something.",								/* RF6_XXX1X6 */
	" concentrates on XXX wounds.",					/* RF6_HEAL */
	" does something.",								/* RF6_XXX2X6 */
	" blinks away.",								/* RF6_BLINK */
	" teleports away.",								/* RF6_TPORT */
	" does something.",								/* RF6_XXX3X6 */
	" does something.",								/* RF6_XXX4X6 */
	" commands you to return.",						/* RF6_TELE_TO */
	" teleports you away.",							/* RF6_TELE_AWAY */
	" gestures at your feet.",						/* RF6_TELE_LEVEL */
	" does something.",								/* RF6_XXX5 */
	" gestures in shadow.",							/* RF6_DARKNESS */
	" casts a spell and cackles evilly.",			/* RF6_TRAPS */
	" tries to blank your mind.",					/* RF6_FORGET */
	" does something.",								/* RF6_XXX6X6 */
	" does something.",								/* RF6_XXX7X6 */
	" does something.",								/* RF6_XXX8X6 */
	" magically summons help!",						/* RF6_S_MONSTER */
	" magically summons monsters!",					/* RF6_S_MONSTERS */
	" magically summons ants.",						/* RF6_S_ANT */
	" magically summons spiders.",					/* RF6_S_SPIDER */
	" magically summons hounds.",					/* RF6_S_HOUND */
	" magically summons hydras.",					/* RF6_S_HYDRA */
	" magically summons an angel!",					/* RF6_S_ANGEL */
	" magically summons a hellish adversary!",		/* RF6_S_DEMON */
	" magically summons an undead adversary!",		/* RF6_S_UNDEAD */
	" magically summons a dragon!",					/* RF6_S_DRAGON */
	" magically summons greater undead!",			/* RF6_S_HI_UNDEAD */
	" magically summons ancient dragons!",			/* RF6_S_HI_DRAGON */
	" magically summons mighty undead opponents!",	/* RF6_S_WRAITH */
	" magically summons special opponents!",		/* RF6_S_UNIQUE */

	NULL
};



/*
 * Hack -- Spontaneous level feelings (order important).
 *
 * See "do_cmd_feeling()" for details.
 */
static cptr prefix_feeling[] =
{
	"Looks like any other level",
	"You feel there is something special",
	"You have a superb feeling",
	"You have an excellent feeling",
	"You have a very good feeling",
	"You have a good feeling",
	"You feel strangely lucky",
	"You feel your luck is turning",
	"You like the look of this place",
	"This level can't be all bad",
	"What a boring place",
	NULL
};



/*
 * Ignorable atmospheric messages (complete)
 */
static cptr atmosphere_streq[] =
{
	/* Extra damage */
	"It was a good hit!",
	"It was a great hit!",
	"It was a superb hit!",
	"It was a *GREAT* hit!",
	"It was a *SUPERB* hit!",

	/* Discoveries */
	"You found a trap!",
	"You have found a trap.",
	"You have found a secret door.",
	"You have discovered a trap on the chest!",

	/* Trap effects */
	"You fall through a trap door!",
	"You float gently down to the next level.",
	"You fall into a pit!",
	"You float gently to the bottom of the pit.",
	"You fall into a spiked pit!",
	"You float gently to the floor of the pit.",
	"You carefully avoid touching the spikes.",
	"You are impaled!",
	"You are impaled on poisonous spikes!",
	"The poison does not affect you!",
	"You are enveloped in a cloud of smoke!",
	"You hit a teleport trap!",
	"You are enveloped in flames!",
	"You are splashed with acid!",
	"A small dart hits you!",
	"A small dart barely misses you.",
	"You are surrounded by a black gas!",
	"You are surrounded by a gas of scintillating colors!",
	"You are surrounded by a pungent green gas!",
	"You are surrounded by a strange white mist!",

	/* Artifact effects */
	"You are surrounded by a malignant aura.",
	"You are surrounded by a powerful aura.",
	"Your bolts are covered in a fiery aura!",
	"The fiery enchantment failed.",
	"You failed to activate it properly.",
	"It whines, glows and fades...",
	"You activate it...",
	"The phial wells with clear light...",
	"The star shines brightly...",
	"The stone glows a deep green...",
	"The amulet lets out a shrill wail...",
	"The amulet floods the area with goodness...",
	"The ring glows brightly...",
	"The ring glows deep red...",
	"The ring glows bright white...",
	"The ring glows deep blue...",
	"The ring glows intensely black...",
	"Your armor is surrounded by lightning...",
	"Your armor glows many colours...",
	"Your armor glows a bright white...",
	"You feel much better...",
	"Your armor twists space around you...",
	"Your armor glows deep blue...",
	"Your armor glows bright red...",
	"Your helm glows bright white...",
	"An image forms in your mind...",
	"Your crown glows deep blue...",
	"You feel a warm tingling inside...",
	"Your cloak glows many colours...",
	"Your cloak glows deep blue...",
	"Your cloak glows bright yellow...",
	"Your cloak twists space around you...",
	"Your cloak glows a deep red...",
	"Your gloves glow extremely brightly...",
	"Your gauntlets are covered in fire...",
	"Your gauntlets are covered in frost...",
	"Your gauntlets are covered in sparks...",
	"Your gauntlets are covered in acid...",
	"Your cesti grows magical spikes...",
	"Your boots glow bright green...",
	"Your boots glow deep blue...",
	"Your dagger is covered in fire...",
	"Your dagger is covered in frost...",
	"Your dagger is covered in sparks...",
	"Your dagger throbs deep green...",
	"Your dagger is covered in frost...",
	"Your sword glows a pale blue...",
	"Your sword glows an intense blue...",
	"Your sword glows an intense red...",
	"Your axe blade glows black...",
	"Your spear glows a bright white...",
	"Your spear pulsates...",
	"Your axe lets out a long, shrill note...",
	"Your battle axe radiates deep purple...",
	"Your trident glows deep red...",
	"Your scythe glows soft white...",
	"Your flail glows in scintillating colours...",
	"Your morning star rages in fire...",
	"Your mace glows bright green...",
	"Your quarterstaff glows yellow...",
	"Your quarterstaff glows brightly...",
	"Your hammer glows white...",
	"Your crossbow glows deep red...",

	/* Dragon breath */
	"You breathe lightning.",
	"You breathe frost.",
	"You breathe acid.",
	"You breathe poison gas.",
	"You breathe fire.",
	"You breathe confusion.",
	"You breathe sound.",
	"You breathe chaos.",
	"You breathe disenchantment.",
	"You breathe shards.",
	"You breathe light.",
	"You breathe darkness.",
	"You breathe the elements.",

	/* Equipment side effects */
	"Your covered hands feel unsuitable for spellcasting.",
	"Your hands feel more suitable for spellcasting.",
	"The weight of your armor encumbers your movement.",
	"You feel able to move more freely.",
	"You have trouble wielding such a heavy bow.",
	"You have no trouble wielding your bow.",
	"You feel relieved to put down your heavy bow.",
	"You have trouble wielding such a heavy weapon.",
	"You have no trouble wielding your weapon.",
	"You feel relieved to put down your heavy weapon.",
	"You do not feel comfortable with your weapon.",
	"You feel comfortable with your weapon.",
	"You feel more comfortable after removing your weapon.",

	/* Removing inscription */
	"Inscription removed.",

	/* Confused */
	"You are confused.",

	/* Targetting */
	"Target Selected.",
	"Target Aborted.",

	/* Sun rising/setting */
	"The sun has risen.",
	"The sun has fallen.",

	/* Cancelled */
	"Cancelled.",

	/* Pack overflow */
	"Your pack overflows!",

	/* Unstacking */
	"You unstack your staff.",
	"You unstack your wand.",
	"You unstack your rod.",

	/* Fainting */
	"You faint from the lack of food.",

	/* Running out of light */
	"Your light has gone out!",
	"Your light is growing faint.",

	/* Cheat death */
	"You invoke wizard mode and cheat death.",

	/* Saving game */
	"Saving game...",
	"done.",
	"failed!",

	/* Refueling */
	"You fuel your lamp.",
	"Your lamp is full.",
	"You combine the torches.",
	"Your torch is fully fueled.",
	"Your torch glows more brightly.",

	/* Object effects */
	"That tastes good.",
	"You feel less thirsty.",
	"The potion makes you vomit!",
	"You feel your memories fade.",
	"Your nerves and muscles feel weak and lifeless!",
	"Massive explosions rupture your body!",
	"A feeling of Death flows through your body.",
	"You feel life flow through your body!",
	"An image of your surroundings forms in your mind...",
	"You begin to feel more enlightened...",
	"You begin to know yourself a little better...",
	"You feel more experienced.",
	"There is a high pitched humming noise.",
	"You feel as if someone is watching over you.",
	"The staff glows blue for a moment...",
	"The end of the staff glows brightly...",
	"A line of blue shimmering light appears.",
	"A line of blue shimmering light appears.",
	"The power of your god banishes evil!",
	"The world changes!",

	/* Nexus effect */
	"Your body starts to scramble...",

	/* Spell/Prayer failures */
	"You failed to get the spell off!",
	"You failed to concentrate hard enough!",

	/* Branding failure */
	"The Branding failed.",

	/* Object failures */
	"You failed to use the staff properly.",
	"The staff has no charges left.",
	"You failed to use the wand properly.",
	"The wand has no charges left.",
	"You failed to use the rod properly.",
	"The rod is still charging.",

	/* Shop messages */
	"You instantly agree upon the price.",
	"You eventually agree upon the price.",
	"You quickly agree upon the price.",

	/* Shop messages */
	"The shopkeeper retires.",
	"The shopkeeper brings out some new stock.",

	/* Player takes damage */
	"You are hit by acid!",
	"You are hit by fire!",
	"You are hit by cold!",
	"You are hit by lightning!",
	"You are hit by poison!",
	"You are hit by something!",
	"You are hit by something sharp!",
	"You are hit by something strange!",

	/* Elemental attacks */
	"You are covered in acid!",
	"You are struck by electricity!",
	"You are enveloped in flames!",
	"You are covered with frost!",

	/* Life draining */
	"You keep hold of your life force!",
	"You feel your life slipping away!",
	"You feel your life draining away!",
	"You feel life has clocked back.",

	/* Gravity attack */
	"Gravity warps around you.",

	/* Earthquake */
	"There is a searing blast of light!",
	"The cave ceiling collapses!",
	"The cave floor twists in an unnatural way!",
	"The cave quakes!",
	"You are pummeled with debris!",
	"You are severely crushed!",
	"You nimbly dodge the blast!",
	"You are bashed by rubble!",
	"You are crushed between the floor and ceiling!",

	/* Call lite/dark */
	"You are surrounded by a white light.",
	"Darkness surrounds you.",

	/* Sensing things */
	"You sense the presence of traps!",
	"You sense the presence of doors!",
	"You sense the presence of stairs!",
	"You sense the presence of buried treasure!",
	"You sense the presence of treasure!",
	"You sense the presence of objects!",
	"You sense the presence of magic objects!",
	"You sense the presence of monsters!",
	"You sense the presence of invisible creatures!",
	"You sense the presence of evil creatures!",

	/* Not enough mana */
	"You faint from the effort!",
	"You have damaged your health!",

	/* Aggravation */
	"You feel a sudden stirring nearby!",
	"You hear a sudden stirring in the distance!",

	/* Teleport level */
	"You rise up through the ceiling.",
	"You sink through the floor.",

	/* Alter reality */
	"The world changes!",

	/* Player is unaffected */
	"You are unaffected!",
	"You resist the effects!",
	"You stand your ground!",
	"You refuse to be frightened.",
	"You disbelieve the feeble spell.",

	/* Thief blinks away */
	"There is a puff of smoke!",

	/* Spell attack effects */
	"You are engulfed in a whirlpool.",
	"You feel something focusing on your mind.",
	"Your mind is blasted by psionic energy.",
	"Your memories fade away.",

	/* Monsters summoned */
	"You hear something appear nearby.",
	"You hear many things appear nearby.",
	"You hear many creepy things appear nearby.",
	"You hear many powerful things appear nearby.",

	/* Curse broken */
	"The curse is broken!",

	/* Enchantment failed */
	"The enchantment failed.",

	/* Objects falls under player */
	"You feel something roll beneath your feet.",

	/* Overcharged rod */
	"The recharge backfires, draining the rod further!",

	/* Overcharged wand/staff */
	"There is a bright flash of light.",

	/* Terrain effects */
	"There is a bright flash of light!",
	"The wall turns into mud!",
	"The vein turns into mud!",
	"You have found something!",
	"The rubble turns into mud!",
	"There was something buried in the rubble!",
	"The door turns into mud!",
	"Click!",

	/* Object resists */
	"The object resists the spell.",

	/* Monster bashes door */
	"You hear a door burst open!",

	/* Monster breaks rune */
	"The rune of protection is broken!",

	/* Inventory management */
	"You combine some items in your pack.",
	"You reorder some items in your pack.",

	/* Draining/Theft attacks */
	"Energy drains from your pack!",
	"You quickly protect your money pouch!",
	"Nothing was stolen.",
	"Your purse feels lighter.",
	"All of your coins were stolen!",
	"You grab hold of your backpack!",
	"Your light dims.",

	/* Nothing special to examine */
	"You have no special knowledge about that item.",
	"You see nothing special.",

	/* Shops */
	"Okay.",
	"Fine.",
	"Accepted!",
	"Agreed!",
	"Done!",
	"Taken!",
	"Arrgghh!",
	"You bastard!",
	"You hear someone sobbing...",
	"The shopkeeper howls in agony!",
	"Damn!",
	"You fiend!",
	"The shopkeeper curses at you.",
	"The shopkeeper glares at you.",
	"Cool!",
	"You've made my day!",
	"The shopkeeper giggles.",
	"The shopkeeper laughs loudly.",
	"Yipee!",
	"I think I'll retire!",
	"The shopkeeper jumps for joy.",
	"The shopkeeper smiles gleefully.",

#if 0
	/* Haggling */
	"You try my patience.",
	"My patience grows thin.",
	"Ha!",
	"You knave!",
	"That's a pittance!",
	"That's an insult!",
	"As if!",
	"My arse!",
	"May the fleas of 1000 orcs molest you!",
	"May your most favourite parts go moldy!",
	"May Morgoth find you tasty!",
	"Your mother was an Ogre!",
	"Be reasonable.",
	"You try my patience.",
	"My patience grows thin.",
	"Never!",
	"That is too much!",
	"That looks war surplus!",
	"That's an insult!",
	"Enough!",
	"You have abused me once too often!",
	"Arghhh!",
	"I have had enough abuse for one day!",
	"That does it!",
	"You shall waste my time no more!",
	"This is getting nowhere!",
	"I'm going to Londis!",
	"Leave my store!",
	"Get out of my sight!",
	"Begone, you scoundrel!",
	"Out, out, out!",
	"Try again.",
	"Ridiculous!",
	"You will have to do better than that!",
	"Do you wish to do business or not?",
	"You've got to be kidding!",
	"You'd better be kidding!",
	"You try my patience.",
	"Hmmm, nice weather we're having.",
	"I must have heard you wrong.",
	"I'm sorry, I missed that.",
	"I'm sorry, what was that?",
	"Sorry, what was that again?",
#endif

	/* Dungeon maintenance */
	"Compacting monsters...",
	"Too many monsters!",
	"Compacting objects...",
	"Too many objects!",

	NULL
};


/*
 * Ignorable atmospheric messages (prefixes)
 */
static cptr atmosphere_prefix[] =
{
	/* "You have found xxx gold pieces worth of yyy." */
	"You have found ",

	/* "You see xxx." */
	"You see ",

	/* "You have no room for xxx." */
	"You have no room for ",

	/* "You have xxx (c)." */
	/* "You have xxx." */
	"You have ",

	/* "You drop xxx (c)." */
	"You drop ",

	/* "You destroy xxx." */
	"You destroy ",

	/* "You cannot destroy xxx." */
	"You cannot destroy ",

	/* "You feel the xxx you are xxx is xxx." */
	/* "You feel the xxx you are xxx are xxx." */
	/* "You feel the xxx in your pack is xxx." */
	/* "You feel the xxx in your pack are xxx." */
	"You feel the ",

	/* Equipment management */
	"You were wielding ",
	"You were holding ",
	"You were wearing ",

	/* Equipment management */
	"You are wielding",
	"You are shooting with",
	"Your light source is",
	"You are wearing",

	/* Identification */
	/* "On the ground: xxx." */
	/* "In your pack: xxx (c)." */
	/* "xxx: xxx (c)." */
	"In your pack: ",
	"On the ground: ",
	"Attacking monsters with: ",
	"Shooting missiles with: ",
	"Wearing on your left hand: ",
	"Wearing on your right hand: ",
	"Wearing around your neck: ",
	"Using to light the way: ",
	"Wearing on your body: ",
	"Wearing on your back: ",
	"Wearing on your arm: ",
	"Wearing on your head: ",
	"Wearing on your hands: ",
	"Wearing on your feet: ",
	"Carrying in your pack: ",
	"Just lifting: ",
	"Just holding: ",

	/* About to inscribe */
	"Inscribing ",

	/* About to examine */
	"Examining ",

	/* Time attack */
	/* "You're not as xxx as you used to be..." */
	"You're not as ",

	/* Terrible black aura */
	"A terrible black aura ",

	/* Shops */
	"Buying ",
	"You bought ",
	"Selling ",
	"You sold ",

#if 0
	/* Haggling */
	"I can take no less than ",
	"I will accept no less than ",
	"No less than ",
	"I want ",
	"How about ",
	"Try ",
	"Perhaps ",
	"How about ",
	"I will pay no more than ",
	"I can afford no more than ",
	"How about ",
	"I'll buy it as scrap for ",
	"Say ",
#endif

	/* Dungeon maintenance */
	"Generation restarted ",

	NULL
};


/*
 * Ignorable atmospheric messages (suffixes)
 */
static cptr atmosphere_suffix[] =
{
	/* "You have xxx charges remaining." */
	/* "You have xxx charge remaining." */
	/* "There are xxx charges remaining." */
	/* "There is xxx charge remaining." */
	" charges remaining.",
	" charge remaining.",

	/* Enchantment */
	" glows brightly!",
	" glow brightly!",

	/* "xxx coins were stolen!" */
	" coins were stolen!",

	/* "One of your xxx was stolen!" */
	/* "Your xxx was stolen!" */
	" was stolen!",

	/* "One of your xxx was eaten!" */
	/* "Your xxx was eaten!" */
	" was eaten!",

	/* Object destruction */
	" melts!",
	" melt!",
	" burns up!",
	" burn up!",
	" shatters!",
	" shatter!",
	" is destroyed!",
	" are destroyed!",

	/* Breakage */
	/* "The xxx disappears." */
	/* "The xxx disappear." */
	" disappears.",
	" disappear.",

	/* Destruction */
	/* "All of your xxx were destroyed!" */
	/* "Some of your xxx were destroyed!" */
	/* "One of your xxx was destroyed!" */
	/* "Your xxx was destroyed!" */
	/* XXX XXX XXX */
	" were destroyed!",
	" was destroyed!",

	/* "Your xxx is unaffected!" */
	/* "The xxx is unaffected!" */
	/* "The xxx are unaffected!" */
	" is unaffected!",
	" are unaffected!",

	/* "Your xxx is damaged!" */
	" is damaged!",

	/* "Your xxx (c) resists disenchantment!" */
	/* "Your xxx (c) resist disenchantment!" */
	" resists disenchantment!",
	" resist disenchantment!",

	/* "Your xxx (c) was disenchanted!" */
	/* "Your xxx (c) were disenchanted!" */
	" was disenchanted!",
	" were disenchanted!",

	/* Branding */
	" is covered in a fiery shield!",
	" glows deep, icy blue!",

#if 0
	/* Haggling */
	" is final.",
	" is more like it.",
	" gold pieces and be thankful for it!",
	" gold pieces and not a copper more!",
#endif

	NULL
};


/*
 * Unexpected messages (exact)
 */
static cptr unexpected_streq[] =
{
	"You are not wielding a light.",
	"Your light cannot be refilled.",
	"You have nothing to fire with.",
	"You have nothing to fire.",
	"You have nothing to throw.",
	"You have nothing you can wear or wield.",
	"You are not wearing anything to take off.",
	"You have nothing to drop.",
	"You have nothing to destroy.",
	"You have nothing to examine.",
	"You have nothing to un-inscribe.",
	"You have nothing to inscribe.",
	"You have no flasks of oil.",
	"You have no extra torches.",
	"You have no books that you can read.",
	"You have no spell books!",
	"You have no prayer books!",
	"You have nothing to eat.",
	"You have no potions to quaff.",
	"You have no scrolls to read.",
	"You have no staff to use.",
	"You have no wand to aim.",
	"You have no rod to zap.",
	"You have nothing to activate.",
	"You have nothing to enchant.",
	"You have nothing to identify.",
	"You have nothing to recharge.",
	"You have nothing that I want.",
	"You cannot see!",
	"You cannot read books!",
	"You cannot learn any new spells!",
	"You cannot learn any new prayers!",
	"You cannot learn any new spells in that book.",
	"You cannot learn any new prayers in that book.",
	"You cannot cast spells!",
	"You are too confused!",
	"You don't know any spells in that book.",
	"You do not have enough mana to cast this spell.",
	"Pray hard enough and your prayers may be answered.",
	"You don't know any prayers in that book.",
	"You do not have enough mana to recite this prayer.",
	"The gates to ANGBAND are closing...",
	"Please finish up and/or save your game.",
	"The gates to ANGBAND are now closed.",
	"You must first pick up the staffs.",
	"You must first pick up the wands.",
	"You must first pick up the rods.",
	"That item had no inscription to remove.",
	"You can't see anything.",
	"You have no light to read by.",
	"Oops!",
	"It feels deathly cold!",
	"Hmmm, it seems to be cursed.",
	"*** LOW HITPOINT WARNING! ***",
	"Your home is empty.",
	"I am currently out of stock.",
	"You cannot carry that many items.",
	"You do not have enough gold.",
	"Your home is full.",
	"I have not the room in my store to keep it.",
	"Entire inventory is shown.",
	"That command does not work in stores.",
	"You see no store here.",
	"The doors are locked.",
	"Your pack is so full that you flee the store...",
	"Your pack is so full that you flee your home...",
	NULL
};


/*
 * This macro evaluates to TRUE if the string "S1" (with length "N1") has a
 * suffix consisting of the string "S2" (with length "N2") plus an arbitrary
 * string "S3" (with length "N3"), and FALSE otherwise.  The "S3" parameter
 * is given only to provide a "typical" value, and is not actually used here.
 */
#define HACKEND(S1,N1,S2,N2,S3,N3) \
	(((N1) >= ((N2) + (N3))) && \
	 !memcmp((S1) + (N1) - ((N2) + (N3)), (S2), (N2)))


/*
 * Helper function for "borg_parse_aux()" (see below)
 *
 * This function will detect any monster reference at the beginning of the
 * message which is followed by a space, and after temporarily removing the
 * reference, will attempt to analyze the message.
 *
 * Several message strings take a "possessive" of the form "his" or
 * "her" or "its".  These strings are all represented by the encoded
 * form "XXX" in the various match strings.  Unfortunately, the encode
 * form is never decoded, so the Borg must then match these messages by
 * hand.
 *
 * We assume that no normal monster name contains any other monster name as
 * a prefix (except for exact duplicates, as in the "singular" and "group"
 * versions of some monsters).
 *
 * We assume that no unique monster name contains any other unique monster 
 * name as a prefix.
 *
 * We assume that the only monster names which contains other monster names
 * as a prefix are normal monster names which are identical to other normal
 * monster names (as in the "singular" and "group" versions of some monsters).
 */
static bool borg_parse_aux_monster_1(cptr msg, int len)
{
	int i;

	int len1 = 0;
	cptr prefix1 = "";
	cptr middle1 = "";
	cptr suffix1 = "";
	char who1[256] = "";

	char buf[256];

	cptr hack;


	/* Efficiency -- Player */
	if (prefix(msg, "You ")) return (FALSE);


	/* Invisible monsters */
	if (prefix(msg, "It "))
	{
		middle1 = "It";
	}
	else if (prefix(msg, "Someone "))
	{
		middle1 = "Someone";
	}
	else if (prefix(msg, "Something "))
	{
		middle1 = "Something";
	}


	/* Check for "A " or "An " or "The " */
	else if (prefix(msg, "A "))
	{
		prefix1 = "A ";
	}
	else if (prefix(msg, "An "))
	{
		prefix1 = "An ";
	}
	else if (prefix(msg, "The "))
	{
		prefix1 = "The ";
	}


	/* Normal monsters */
	if (!*middle1 && *prefix1)
	{
		int m, n;

		/* Jump past the prefix */
		hack = msg + strlen(prefix1);

		/* Start the search */
        	m = 0; n = ab_ptr->normal_num;

		/* Scan backwards */
		for (i = n-1; i >= 0; --i)
		{
			cptr text = ab_ptr->normal_text[i];

			if (prefix(hack, text))
			{
				middle1 = text;
				break;
			}
		}
	}


	/* Unique monsters */
	if (!*middle1)
	{
		int m, n;

		/* Start the search */
        	m = 0; n = ab_ptr->unique_num;

		/* Scan backwards */
		for (i = n-1; i >= 0; --i)
		{
			cptr text = ab_ptr->unique_text[i];

			if (prefix(msg, text))
			{
				middle1 = text;
				break;
			}
		}
	}


	/* Nothing found */
	if (!*middle1) return (FALSE);


	/* Jump past the monster name */
	hack = msg + strlen(prefix1) + strlen(middle1);

	/* Hack -- Handle "offscreen" suffix */
	if (!memcmp(hack, " (offscreen)", 12))
	{
		/* Save suffix */
		suffix1 = " (offscreen)";
	}


	/* Build the total string and extract the length */
	len1 = strfmt(who1, "%s%s%s", prefix1, middle1, suffix1);

	/* Paranoia */
	if (msg[len1] != ' ') return (FALSE);


	/* Remove monster name */
	hack = msg + len1;


	/* XXX XXX XXX */
	borg_note(format("# Parsing '%s' + '%s'", who1, hack));


	/* Handle "xxx is unharmed." */
	if (streq(hack, " is unharmed."))
	{
		strnfmt(buf, 256, "PAIN:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "xxx screams in pain." (etc) */
	for (i = 0; suffix_pain[i]; i++)
	{
		if (streq(hack, suffix_pain[i]))
		{
			strnfmt(buf, 256, "PAIN:%s", who1);
			borg_react(msg, buf);
			return (TRUE);
		}
	}


	/* Handle "It dies." (etc) */
	for (i = 0; suffix_died[i]; i++)
	{
		if (streq(hack, suffix_died[i]))
		{
			strnfmt(buf, 256, "DIED:%s", who1);
			borg_react(msg, buf);
			return (TRUE);
		}
	}


	/* Handle "xxx misses you." */
	if (streq(hack, " misses you."))
	{
		strnfmt(buf, 256, "MISS_BY:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "xxx is repelled." */
	if (streq(hack, " is repelled."))
	{
		strnfmt(buf, 256, "MISS_BY:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "It hits you." (etc) */
	for (i = 0; suffix_hit_by[i]; i++)
	{
		/* "It hits you." (etc) */
		if (streq(hack, suffix_hit_by[i]))
		{
			strnfmt(buf, 256, "HIT_BY:%s", who1);
			borg_react(msg, buf);
			return (TRUE);
		}
	}

	/* Ignore "It drools on you." (etc) */
	for (i = 0; suffix_hit_by_ignorable[i]; i++)
	{
		if (streq(hack, suffix_hit_by_ignorable[i]))
		{
			return (TRUE);
		}
	}


	/* Handle "It casts a spell." (etc) */
	for (i = 0; suffix_spell[i]; i++)
	{
		if (streq(hack, suffix_spell[i]))
		{
			strnfmt(buf, 256, "SPELL_%03d:%s", 96+i, who1);
			borg_react(msg, buf);
			return (TRUE);
		}
	}

	if (prefix(hack, " concentrates on ") && suffix(hack, " body."))
	{
		strnfmt(buf, 256, "SPELL_%03d:%s", RF6_HASTE, who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	if (prefix(hack, " concentrates on ") && suffix(hack, " wounds."))
	{
		strnfmt(buf, 256, "SPELL_%03d:%s", RF6_HEAL, who1);
		borg_react(msg, buf);
		return (TRUE);
	}


	/* Handle "xxx falls asleep!" */
	if (streq(hack, " falls asleep!"))
	{
		strnfmt(buf, 256, "STATE_SLEEP:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "xxx wakes up." */
	if (streq(hack, " wakes up."))
	{
		strnfmt(buf, 256, "STATE_AWAKE:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}


	/* Handle "xxx flees in terror!" */
	if (streq(hack, " flees in terror!"))
	{
		strnfmt(buf, 256, "STATE__FEAR:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "xxx recovers xxx courage." */
	if (streq(hack, " recovers his courage.") ||
	    streq(hack, " recovers her courage.") ||
	    streq(hack, " recovers its courage."))
	{
		strnfmt(buf, 256, "STATE__BOLD:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "xxx turns to fight!" */
	if (streq(hack, " turns to fight!"))
	{
		strnfmt(buf, 256, "STATE__BOLD:%s", who1);
		borg_react(msg, buf);
		return (TRUE);
	}


	/* Ignore "xxx starts moving faster." XXX XXX XXX */
	if (streq(hack, " starts moving faster.")) return (TRUE);

	/* Ignore "xxx starts moving slower." XXX XXX XXX */
	if (streq(hack, " starts moving slower.")) return (TRUE);


	/* Ignore monster state change (healthier) XXX XXX XXX */
	if (streq(hack, " appears healthier.")) return (TRUE);
	if (streq(hack, " looks REALLY healthy!")) return (TRUE);
	if (streq(hack, " sounds REALLY healthy!")) return (TRUE);
	if (streq(hack, " looks healthier.")) return (TRUE);
	if (streq(hack, " sounds healthier.")) return (TRUE);


	/* Ignore monster state change (stunned) XXX XXX XXX */
	if (streq(hack, " is dazed.")) return (TRUE);
	if (streq(hack, " is more dazed.")) return (TRUE);

	/* Ignore monster state change (not stunned) XXX XXX XXX */
	if (streq(hack, " is no longer stunned.")) return (TRUE);


	/* Ignore monster state change (confused) XXX XXX XXX */
	if (streq(hack, " appears confused.")) return (TRUE);
	if (streq(hack, " looks confused.")) return (TRUE);
	if (streq(hack, " looks more confused.")) return (TRUE);

	/* Ignore monster state change (not confused) XXX XXX XXX */
	if (streq(hack, " is no longer confused.")) return (TRUE);


	/* Ignore monster state change (various) XXX XXX XXX */
	if (streq(hack, " spawns!")) return (TRUE);
	if (streq(hack, " cringes from the light!")) return (TRUE);
	if (streq(hack, " loses some skin!")) return (TRUE);
	if (streq(hack, " shudders.")) return (TRUE);
	if (streq(hack, " changes!")) return (TRUE);
	if (streq(hack, " disappears!")) return (TRUE);


	/* Ignore monster susceptibility XXX XXX XXX */
	if (streq(hack, " is hit hard.")) return (TRUE);

	/* Ignore monster resistance XXX XXX XXX */
	if (streq(hack, " resists.")) return (TRUE);
	if (streq(hack, " resists a lot.")) return (TRUE);
	if (streq(hack, " resists somewhat.")) return (TRUE);
	if (streq(hack, " is immune.")) return (TRUE);

	/* Ignore monster state change (unaffected) XXX XXX XXX */
	if (streq(hack, " is unaffected.")) return (TRUE);
	if (streq(hack, " is unaffected!")) return (TRUE);


	/* Ignore messages about monsters affecting objects */
	if (prefix(hack, " tries to pick up ")) return (TRUE);
	if (prefix(hack, " picks up ")) return (TRUE);
	if (prefix(hack, " crushes ")) return (TRUE);

	/* Ignore messages about monsters affected by earthquakes */
	if (streq(hack, " wails out in pain!")) return (TRUE);
	if (streq(hack, " is embedded in the rock!")) return (TRUE);

	/* Ignore messages about monster attacks while blind */
	if (streq(hack, " makes a strange noise.")) return (TRUE);
	if (streq(hack, " breathes.")) return (TRUE);
	if (streq(hack, " mumbles.")) return (TRUE);
	if (streq(hack, " mumbles powerfully.")) return (TRUE);
	if (streq(hack, " mumbles loudly.")) return (TRUE);
	if (streq(hack, " mumbles strangely.")) return (TRUE);
	if (streq(hack, " mumbles, and you hear scary noises.")) return (TRUE);
	if (streq(hack, " mumbles, and you hear puzzling noises.")) return (TRUE);
	if (streq(hack, " mumbles, and then cackles evilly.")) return (TRUE);
	if (streq(hack, " screams the word 'DIE!'")) return (TRUE);


	/* Nothing found */
	return (FALSE);
}


/*
 * Helper function for "borg_parse_aux()" (see below)
 *
 * This function will detect any monster reference at the end of the message
 * which is preceded by a space, and followed by a punctuation mark, and after
 * temporarily removing the reference and the punctuation, will attempt to
 * analyze the message.
 *
 * We assume that the only monster names which contains other monster names
 * as a suffix are unique monster names which contain normal monster names
 * (as in "Lagduf, the Snaga"), and normal monster names which are identical
 * to other normal monster names (as in the "singular" and "group" versions
 * of some monsters).
 */
static bool borg_parse_aux_monster_2(cptr msg, int len)
{
	int i;

	int len2 = 0;
	cptr prefix2 = "";
	cptr middle2 = "";
	cptr suffix2 = "";
	char who2[256] = "";

	char buf[256];

	char mark_body[1024];

	char hack_body[1024];

	cptr hack;


	/* Efficiency -- Player */
	if (HACKEND(msg, len, " you", 4, ".", 1)) return (FALSE);


	/* Hack -- handle "The arrow finds a mark" (etc) */
	if (suffix(msg, " finds a mark."))
	{
		int tmp = len - strlen(" finds a mark.");
		strcpy(mark_body, msg);
		len = tmp + strfmt(mark_body + tmp, " hits it.");
		msg = mark_body;
	}


	/* Offscreen */
	if (HACKEND(msg, len, " (offscreen)", 12, ".", 1))
	{
		/* Save suffix */
		suffix2 = " (offscreen)";
	}


	/* Hack -- skip suffix */
	if (*suffix2) len -= 12;


	/* Check for invisible monsters */
	if (HACKEND(msg, len, " it", 3, ".", 1))
	{
		middle2 = "It";
	}
	else if (HACKEND(msg, len, " someone", 8, ".", 1))
	{
		middle2 = "Someone";
	}
	else if (HACKEND(msg, len, " something", 10, ".", 1))
	{
		middle2 = "Something";
	}


	/* Unique monsters */
	if (!*middle2)
	{
		int m, n;

		/* Start the search */
        	m = 0; n = ab_ptr->unique_num;

		/* Scan backwards */
		for (i = n-1; i >= 0; --i)
		{
			cptr text = ab_ptr->unique_text[i];
			int size = ab_ptr->unique_size[i];

			if (HACKEND(msg, len, text, size, ".", 1))
			{
				middle2 = text;
				break;
			}
		}
	}


	/* Normal monsters */
	if (!*middle2)
	{
		int m, n;

		/* Start the search */
        	m = 0; n = ab_ptr->normal_num;

		/* Scan backwards */
		for (i = n-1; i >= 0; --i)
		{
			cptr text = ab_ptr->normal_text[i];
			int size = ab_ptr->normal_size[i];

			if (HACKEND(msg, len, text, size, ".", 1))
			{
				middle2 = text;
				break;
			}
		}

		/* Normal monsters take a prefix */
		if (*middle2)
		{
			int mlen = strlen(middle2) + 1;

			/* Check for "A " or "An " or "The " */
			if (HACKEND(msg, len, " a ", 3, "xxx.", mlen))
			{
				prefix2 = "A ";
			}
			else if (HACKEND(msg, len, " an ", 4, "xxx.", mlen))
			{
				prefix2 = "An ";
			}
			else if (HACKEND(msg, len, " the ", 5, "xxx.", mlen))
			{
				prefix2 = "The ";
			}

			/* Paranoia */
			if (!*prefix2) return (FALSE);
		}
	}


	/* Hack -- Repair suffix */
	if (*suffix2) len += strlen(suffix2);


	/* Nothing found */
	if (!*middle2) return (FALSE);

	/* Build the total string and extract the length */
	len2 = strfmt(who2, "%s%s%s", prefix2, middle2, suffix2);

	/* Paranoia */
	if (len <= ((len2+1)+1)) return (FALSE);
	if (msg[len-((len2+1)+1)] != ' ') return (FALSE);


	/* Remove monster name and punctuation */
	memcpy(hack_body, msg, len - (len2+1));

	/* Terminate */
	hack_body[len - (len2+1)] = '\0';

	/* New string */
	hack = hack_body;


	/* XXX XXX XXX */
	borg_note(format("# Parsing '%s' + '%s'", hack, who2));


	/* Handle "You hit xxx." */
	if (streq(hack, "You hit "))
	{
		strnfmt(buf, 256, "HIT:%s", who2);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "You miss xxx." */
	if (streq(hack, "You miss "))
	{
		strnfmt(buf, 256, "MISS:%s", who2);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "You are too afraid to attack xxx!" */
	if (streq(hack, "You are too afraid to attack "))
	{
		strnfmt(buf, 256, "MISS:%s", who2);
		borg_react(msg, buf);
		return (TRUE);
	}

	/* Handle "You have killed xxx." (etc) */
	for (i = 0; prefix_kill[i]; i++)
	{
		/* "You have killed it." (etc) */
		if (streq(hack, prefix_kill[i]))
		{
			strnfmt(buf, 256, "KILL:%s", who2);
			borg_react(msg, buf);
			return (TRUE);
		}
	}


	/* Ignore messages about objects hitting monsters XXX XXX XXX */
	/* "The xxx hits xxx." */
	if (suffix(hack, " hits ")) return (TRUE);


	/* No match */
	return (FALSE);
}


/*
 * Helper function for "borg_parse_aux()" (see below)
 *
 * This function will attempt to analyze messages about terrain features.
 */
static bool borg_parse_aux_terrain(cptr msg, int len)
{
	int y = xb_ptr->gy;
	int x = xb_ptr->gx;

	char buf[256];

	
	/* Ignore "up stairs" failure XXX XXX XXX */
	if (streq(msg, "I see no up staircase here.")) return (TRUE);

	/* Ignore "down stairs" failure XXX XXX XXX */
	if (streq(msg, "I see no down staircase here.")) return (TRUE);

	/* Ignore "up stairs" success */
	if (streq(msg, "You enter a maze of up staircases.")) return (TRUE);

	/* Ignore "down stairs" success */
	if (streq(msg, "You enter a maze of down staircases.")) return (TRUE);


	/* Ignore general failure XXX XXX XXX */
	if (streq(msg, "You see nothing there.")) return (TRUE);

	/* Ignore specific failures XXX XXX XXX */
	if (streq(msg, "You see nothing there to open.")) return (TRUE);
	if (streq(msg, "You see nothing there to close.")) return (TRUE);
	if (streq(msg, "You see nothing there to tunnel.")) return (TRUE);
	if (streq(msg, "You see nothing there to disarm.")) return (TRUE);
	if (streq(msg, "You see nothing there to bash.")) return (TRUE);
	if (streq(msg, "You see nothing there to alter.")) return (TRUE);
	if (streq(msg, "You see nothing there to spike.")) return (TRUE);

	/* Ignore motion failures XXX XXX XXX */
	if (streq(msg, "You feel a pile of rubble blocking your way.")) return (TRUE);
	if (streq(msg, "You feel a door blocking your way.")) return (TRUE);
	if (streq(msg, "You feel a wall blocking your way.")) return (TRUE);

	/* Ignore motion failures XXX XXX XXX */
	if (streq(msg, "There is a pile of rubble blocking your way.")) return (TRUE);
	if (streq(msg, "There is a door blocking your way.")) return (TRUE);
	if (streq(msg, "There is a wall blocking your way.")) return (TRUE);

	/* Ignore walking failures XXX XXX XXX */
	if (streq(msg, "There is a pile of rubble in the way!")) return (TRUE);
	if (streq(msg, "There is a door in the way!")) return (TRUE);
	if (streq(msg, "There is a wall in the way!")) return (TRUE);

	/* Ignore monster induced failures XXX XXX XXX */
	if (streq(msg, "There is a monster in the way!")) return (TRUE);

	/* Hack -- Handle alter failures */
	if (streq(msg, "You spin around."))
	{
		strnfmt(buf, 256, "WEIRD:%s", "spin");
		borg_react(msg, buf);
		return (TRUE);
	}


#if 0

	/* XXX XXX XXX */
	if (borg_opening_chest)
	{
		/* Ignore "open/disarm" trap descriptions */
		if (streq(msg, "A small needle has pricked you!")) return (TRUE);
		if (streq(msg, "A puff of green gas surrounds you!")) return (TRUE);
		if (streq(msg, "A puff of yellow gas surrounds you!")) return (TRUE);
		if (streq(msg, "You are enveloped in a cloud of smoke!")) return (TRUE);
		if (streq(msg, "There is a sudden explosion!")) return (TRUE);
		if (streq(msg, "Everything inside the chest is destroyed!")) return (TRUE);

		/* Ignore "open/disarm" failure */
		if (streq(msg, "You set off a trap!")) return (TRUE);

		/* Ignore "disarm" failure */
		if (streq(msg, "You failed to disarm the chest.")) return (TRUE);

		/* Ignore "disarm" failure (semantic) */
		if (streq(msg, "I don't see any traps.")) return (TRUE);
		if (streq(msg, "The chest is not trapped.")) return (TRUE);

		/* Ignore "disarm" success */
		if (streq(msg, "You have disarmed the chest.")) return (TRUE);

		/* Ignore "open" failure */
		if (streq(msg, "You failed to pick the lock.")) return (TRUE);

		/* Ignore "open" success */
		if (streq(msg, "You have picked the lock.")) return (TRUE);
	}

#endif


	/* Handle "open" failure (jammed) */
	if (streq(msg, "The door appears to be stuck."))
	{
		/* Convert closed/locked doors to jammed */
		if ((borg_cave_feat[y][x] >= FEAT_DOOR_HEAD) &&
			 (borg_cave_feat[y][x] <= FEAT_DOOR_HEAD + 0x07))
		{
			/* Mark the door as jammed */
			borg_cave_feat[y][x] = FEAT_DOOR_HEAD + 0x08;

			/* Forget task */
			borg_task = 0;
		}

		return (TRUE);
	}

	/* Handle "open" failure (locked) */
	if (streq(msg, "You failed to pick the lock."))
	{
		/* Modify locked doors */
		if ((borg_cave_feat[y][x] > FEAT_DOOR_HEAD) &&
			 (borg_cave_feat[y][x] < FEAT_DOOR_HEAD + 0x07))
		{
			/* Increase the power of the locked door */
			borg_cave_feat[y][x] += 0x01;

			/* Forget task */
			borg_task = 0;
		}

		/* Convert closed doors to locked */
		else if (borg_cave_feat[y][x] == FEAT_DOOR_HEAD)
		{
			/* Mark the door as jammed */
			borg_cave_feat[y][x] = FEAT_DOOR_HEAD + 0x01;

			/* Forget task */
			borg_task = 0;
		}

		return (TRUE);
	}

	/* Ignore "open" success */
	if (streq(msg, "You have picked the lock.")) return (TRUE);


	/* Handle "close" failure (broken) */
	if (streq(msg, "The door appears to be broken."))
	{
		/* Convert open doors */
		if (borg_cave_feat[y][x] == FEAT_OPEN)
		{
			/* Mark as broken */
			borg_cave_feat[y][x] = FEAT_BROKEN;

			/* Forget task */
			borg_task = 0;
		}

		return (TRUE);
	}


	/* Handle "tunnel" failure (perma-wall) */
	if (streq(msg, "This seems to be permanent rock."))
	{
		/* Only process walls */
		if ((borg_cave_feat[y][x] >= FEAT_WALL_EXTRA) &&
			 (borg_cave_feat[y][x] <= FEAT_PERM_SOLID))
		{
			/* Mark the wall as permanent */
			borg_cave_feat[y][x] = FEAT_PERM_SOLID;

			/* Forget task */
			borg_task = 0;
		}

		return (TRUE);
	}

	/* Handle "attack" failure (tunneling into a wall!) */
	if ((borg_task == GOAL_KILL) && prefix(msg, "You tunnel"))
	{
		/* Forget the monster */
		borg_delete_kill(borg_cave_m_idx[y][x]);
	}

	/* Ignore "tunnel" failure (wall/secret-door) */
	if (streq(msg, "You tunnel into the granite wall.")) return (TRUE);

	/* Handle "tunnel" failure (quartz) */
	if (streq(msg, "You tunnel into the quartz vein."))
	{
		/* Process magma veins with treasure */
		if (borg_cave_feat[y][x] == FEAT_MAGMA_K)
		{
			/* Mark the vein */
			borg_cave_feat[y][x] = FEAT_QUARTZ_K;

			/* Forget task */
			borg_task = 0;
		}

		/* Process magma veins */
		else if (borg_cave_feat[y][x] == FEAT_MAGMA)
		{
			/* Mark the vein */
			borg_cave_feat[y][x] = FEAT_QUARTZ;

			/* Forget task */
			borg_task = 0;
		}

		return (TRUE);
	}

	/* Ignore "tunnel" failure (magma) */
	if (streq(msg, "You tunnel into the magma vein.")) return (TRUE);

	/* Ignore "tunnel" failure (rubble) */
	if (streq(msg, "You dig in the rubble.")) return (TRUE);

	/* Ignore "tunnel" failure (door) */
	if (streq(msg, "You tunnel into the door.")) return (TRUE);

	/* Ignore "tunnel" success (walls/veins/doors) */
	if (streq(msg, "You have finished the tunnel.")) return (TRUE);

	/* Ignore "tunnel" success (rubble) */
	if (streq(msg, "You have removed the rubble.")) return (TRUE);

	/* Ignore "tunnel" success (rubble/veins) */
	if (streq(msg, "You have found something!")) return (TRUE);


	/* Ignore "disarm" failure */
	if (prefix(msg, "You failed to disarm the ")) return (TRUE);

	/* Ignore "disarm" failure */
	if (prefix(msg, "You set off the ")) return (TRUE);

	/* Ignore "disarm" success */
	if (prefix(msg, "You have disarmed the ")) return (TRUE);


	/* Ignore "bash" attempt */
	if (streq(msg, "You smash into the door!")) return (TRUE);

	/* Handle "bash" failures */
	if (streq(msg, "The door holds firm.") ||
	    streq(msg, "You are off-balance."))
	{
		/* Modify jammed doors */
		if ((borg_cave_feat[y][x] >= FEAT_DOOR_HEAD + 0x08) &&
		    (borg_cave_feat[y][x] < FEAT_DOOR_TAIL))
		{
			/* Increase the "power" of the door */
			borg_cave_feat[y][x] += 0x01;
		}

		return (TRUE);
	}

	/* Ignore "bash" success */
	if (streq(msg, "The door crashes open!")) return (TRUE);


	/* Ignore "spike" failure */
	if (streq(msg, "You have no spikes!")) return (TRUE);

	/* Ignore "spike" success */
	if (streq(msg, "You jam the door with a spike.")) return (TRUE);


	/* No match */
	return (FALSE);
}


/*
 * Helper function for "borg_parse_aux()" (see below)
 *
 * This function will attempt to analyze messages about the player state.
 */
static bool borg_parse_aux_player(cptr msg, int len)
{
	int i;

	char buf[256];


	if (streq(msg, "You are blind!"))
	{
		b_ptr->blind = 9999;
		return (TRUE);
	}
	if (streq(msg, "You can see again."))
	{
		b_ptr->blind = 0;
		return (TRUE);
	}

	if (streq(msg, "You are confused!"))
	{
		b_ptr->confused = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less confused now."))
	{
		b_ptr->confused = 0;
		return (TRUE);
	}

	if (streq(msg, "You are poisoned!"))
	{
		b_ptr->poisoned = 9999;
		return (TRUE);
	}
	if (streq(msg, "You are no longer poisoned."))
	{
		b_ptr->poisoned = 0;
		return (TRUE);
	}

	if (streq(msg, "You are terrified!"))
	{
		b_ptr->afraid = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel bolder now."))
	{
		b_ptr->afraid = 0;
		return (TRUE);
	}

	if (streq(msg, "You are paralyzed!"))
	{
		b_ptr->paralyzed = 9999;
		return (TRUE);
	}
	if (streq(msg, "You can move again."))
	{
		b_ptr->paralyzed = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel drugged!"))
	{
		b_ptr->image = 9999;
		return (TRUE);
	}
	if (streq(msg, "You can see clearly again."))
	{
		b_ptr->image = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel yourself moving faster!"))
	{
		b_ptr->fast = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel yourself slow down."))
	{
		b_ptr->fast = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel yourself moving slower!"))
	{
		b_ptr->slow = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel yourself speed up."))
	{
		b_ptr->slow = 0;
		return (TRUE);
	}

	if (streq(msg, "A mystic shield forms around your body!"))
	{
		b_ptr->shield = 9999;
		return (TRUE);
	}
	if (streq(msg, "Your mystic shield crumbles away."))
	{
		b_ptr->shield = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel righteous!"))
	{
		b_ptr->blessed = 9999;
		return (TRUE);
	}
	if (streq(msg, "The prayer has expired."))
	{
		b_ptr->blessed = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel like a hero!"))
	{
		b_ptr->hero = 9999;
		return (TRUE);
	}
	if (streq(msg, "The heroism wears off."))
	{
		b_ptr->hero = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel like a killing machine!"))
	{
		b_ptr->shero = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less Berserk."))
	{
		b_ptr->shero = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel safe from evil!"))
	{
		b_ptr->protevil = 9999;
		return (TRUE);
	}
	if (streq(msg, "You no longer feel safe from evil."))
	{
		b_ptr->protevil = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel invulnerable!"))
	{
		b_ptr->invuln = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel vulnerable once more."))
	{
		b_ptr->invuln = 0;
		return (TRUE);
	}

	if (streq(msg, "Your eyes feel very sensitive!"))
	{
		b_ptr->tim_invis = 9999;
		return (TRUE);
	}
	if (streq(msg, "Your eyes feel less sensitive."))
	{
		b_ptr->tim_invis = 0;
		return (TRUE);
	}

	if (streq(msg, "Your eyes begin to tingle!"))
	{
		b_ptr->tim_infra = 9999;
		return (TRUE);
	}
	if (streq(msg, "Your eyes stop tingling."))
	{
		b_ptr->tim_infra = 0;
		return (TRUE);
	}


	if (streq(msg, "You feel resistant to acid!"))
	{
		b_ptr->oppose_acid = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less resistant to acid."))
	{
		b_ptr->oppose_acid = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel resistant to electricity!"))
	{
		b_ptr->oppose_elec = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less resistant to electricity."))
	{
		b_ptr->oppose_elec = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel resistant to fire!"))
	{
		b_ptr->oppose_fire = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less resistant to fire."))
	{
		b_ptr->oppose_fire = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel resistant to cold!"))
	{
		b_ptr->oppose_cold = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less resistant to cold."))
	{
		b_ptr->oppose_cold = 0;
		return (TRUE);
	}

	if (streq(msg, "You feel resistant to poison!"))
	{
		b_ptr->oppose_pois = 9999;
		return (TRUE);
	}
	if (streq(msg, "You feel less resistant to poison."))
	{
		b_ptr->oppose_pois = 0;
		return (TRUE);
	}


	if (streq(msg, "You have been stunned."))
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		b_ptr->stun = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been heavily stunned."))
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		b_ptr->stun = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been knocked out."))
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		b_ptr->stun = 9999;
		return (TRUE);
	}

	if (streq(msg, "You are no longer stunned."))
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		b_ptr->stun = 0;
		return (TRUE);
	}


	if (streq(msg, "You have been given a graze."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been given a light cut."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been given a bad cut."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been given a nasty cut."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been given a severe cut."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been given a deep gash."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}
	if (streq(msg, "You have been given a mortal wound."))
	{
		b_ptr->cut = 9999;
		return (TRUE);
	}

	if (streq(msg, "You are no longer bleeding."))
	{
		b_ptr->cut = 0;
		return (TRUE);
	}


	if (streq(msg, "You are still weak."))
	{
		b_ptr->food = PY_FOOD_FAINT;
		return (TRUE);
	}
	if (streq(msg, "You are still hungry."))
	{
		b_ptr->food = PY_FOOD_WEAK;
		return (TRUE);
	}
	if (streq(msg, "You are no longer hungry."))
	{
		b_ptr->food = PY_FOOD_ALERT;
		return (TRUE);
	}
	if (streq(msg, "You are full!"))
	{
		b_ptr->food = PY_FOOD_FULL;
		return (TRUE);
	}
	if (streq(msg, "You have gorged yourself!"))
	{
		b_ptr->food = PY_FOOD_MAX;
		return (TRUE);
	}

	if (streq(msg, "You are getting faint from hunger!"))
	{
		b_ptr->food = 0;
		return (TRUE);
	}
	if (streq(msg, "You are getting weak from hunger!"))
	{
		b_ptr->food = PY_FOOD_FAINT;
		return (TRUE);
	}
	if (streq(msg, "You are getting hungry."))
	{
		b_ptr->food = PY_FOOD_WEAK;
		return (TRUE);
	}
	if (streq(msg, "You are no longer full."))
	{
		b_ptr->food = PY_FOOD_ALERT;
		return (TRUE);
	}
	if (streq(msg, "You are no longer gorged."))
	{
		b_ptr->food = PY_FOOD_FULL;
		return (TRUE);
	}


	/* Ignore "monster confusion" messages */
	if (streq(msg, "Your hands begin to glow.") ||
	    streq(msg, "Your hands stop glowing."))
	{
		return (TRUE);
	}


	/* Restore mana */
	if (streq(msg, "Your feel your head clear."))
	{
		return (TRUE);
	}

	/* Restore hitpoints */
	if (streq(msg, "You feel a little better.") ||
	    streq(msg, "You feel better.") ||
	    streq(msg, "You feel much better.") ||
	    streq(msg, "You feel very good."))
	{
		return (TRUE);
	}

	/* Restore experience */
	if (streq(msg, "You feel your life energies returning."))
	{
		return (TRUE);
	}


	/* Gain/Lose stats */
	/* "You feel very xxx!" */
	/* "You feel very xxx." */
	/* "You feel very xxx for a moment, but the feeling passes." */
	/* "You feel less xxx." */
	if (prefix(msg, "You feel very ") ||
	    prefix(msg, "You feel less "))
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		return (TRUE);
	}

	/* Knowledge changes */
	/* "You can learn xxx more xxx." */
	/* "You have learned the xxx of xxx." */
	/* "You have forgotten the xxx of xxx." */
	/* "You have remembered the xxx of xxx." */
	if (prefix(msg, "You can learn ") ||
	    prefix(msg, "You have learned the ") ||
	    prefix(msg, "You have forgotten the ") ||
	    prefix(msg, "You have remembered the "))
	{
		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		return (TRUE);
	}

	/* Handle player level change */
	if (prefix(msg, "Welcome to level "))
	{
		/* Save game if needed */
		if (borg_flag_save_level) borg_need_save = TRUE;

		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;

		return (TRUE);
	}

	/* Handle (near) winning messages.  XXX XXX XXX */
	if (streq(msg, "A magical staircase appears...") ||
	    streq(msg, "*** CONGRATULATIONS ***") ||
	    streq(msg, "You have won the game!") ||
	    streq(msg, "You may retire (commit suicide) when you are ready."))
	{
		borg_oops("about to win");
		return (TRUE);
	}


	/* Handle word of recall (ignition) */
	if (prefix(msg, "The air about you becomes "))
	{
		/* Initiate recall */
		borg_recalling = TRUE;
		return (TRUE);
	}

	/* Handle word of recall (lift off) */
	if (streq(msg, "You feel yourself yanked upwards!") ||
	    streq(msg, "You feel yourself yanked downwards!"))
	{
		/* Recall complete */
		borg_recalling = FALSE;
		return (TRUE);
	}

	/* Handle word of recall (cancelled) */
	if (prefix(msg, "A tension leaves "))
	{
		/* Hack -- Oops */
		borg_recalling = FALSE;
		return (TRUE);
	}


	/* Feelings about the level */
	for (i = 0; prefix_feeling[i]; i++)
	{
		/* "You feel..." (etc) */
		if (prefix(msg, prefix_feeling[i]))
		{
			strnfmt(buf, 256, "FEELING:%d", i);
			borg_react(msg, buf);
			return (TRUE);
		}
	}

	/* Ignore unexpected feeling. */
	if (streq(msg, "Looks like a typical town.")) return (TRUE);


	/* No match */
	return (FALSE);
}


/*
 * Helper function for "borg_parse_aux()" (see below)
 *
 * This function will attempt to ignore irrelevant "atmosphere" messages.
 *
 * This function will attempt to notice certain "unexpected" messages.
 */
static bool borg_parse_aux_atmosphere(cptr msg, int len)
{
	int i;


	/* Ignore atmospheric messages */
	for (i = 0; atmosphere_streq[i]; ++i)
	{
		if (streq(msg, atmosphere_streq[i])) return (TRUE);
	}

	/* Ignore atmospheric messages */
	for (i = 0; atmosphere_prefix[i]; ++i)
	{
		if (prefix(msg, atmosphere_prefix[i])) return (TRUE);
	}

	/* Ignore atmospheric messages */
	for (i = 0; atmosphere_suffix[i]; ++i)
	{
		if (suffix(msg, atmosphere_suffix[i])) return (TRUE);
	}


	/* Handle unexpected failure messages. */
	for (i = 0; unexpected_streq[i]; ++i)
	{
		if (streq(msg, unexpected_streq[i]))
		{
			borg_oops("unexpected message");
			return (TRUE);
		}
	}

	/* Handle unexpected failure messages. */
	if (suffix(msg, " appears to be cursed.") ||
	    (prefix(msg, "You may not ") && suffix(msg, " that spell")) ||
	    (prefix(msg, "You may not ") && suffix(msg, " that prayer")))
	{
		borg_oops("unexpected message");
		return (TRUE);
	}


	/* No match */
	return (FALSE);
}


/*
 * Hack -- Parse a message from the world
 *
 * Note that detecting "death" is EXTREMELY important, to prevent all sorts
 * of errors arising from attempting to parse the "tomb" screen, and to let
 * the user to "observe" the "cause" of death.
 *
 * Note that detecting "failure" is EXTREMELY important, to prevent bizarre
 * situations in which part of a command has "failed", but the "rest" of the
 * command is still in the keypress queue, where it could cause unfortunate
 * side effects.  This method is necessary because the Borg cannot currently
 * parse "prompts", and must assume the success of prompt-inducing commands,
 * unless told otherwise by a failure message.  Also, by detecting failure
 * of commands such as detection spells, we can assume that if no failures
 * were detected, then the command succeeded, and the world has thus changed
 * in the desired manner.
 *
 * Note the use of "borg_react()" to "delay" full analysis of some messages
 * until they can be understood in context.
 *
 * This function attempts to match all messages from the world, so that any
 * unknown messages can be listed for analysis.
 */
static void borg_parse_aux(cptr msg, int len)
{
	/* Log (if needed) */
	if (borg_fff) borg_info(format("& Msg <%s>", msg));


	/* Hack -- Notice death */
	if (prefix(msg, "You die."))
	{
		/* Note death */
		borg_note("Player died!");

		/* Save death */
		b_ptr->is_dead = TRUE;

		/* Done */
		return;
	}


	/* Hack -- Notice "failure" */
	if (prefix(msg, "You failed "))
	{
		/* Hack -- store the keypress */
		borg_note("# Normal failure.");

		/* Set the failure flag */
		borg_failure = TRUE;

		/* Flush our key-buffer */
		borg_flush();

		/* May need to update spells */
		borg_do_spell = TRUE;
		borg_do_spell_aux = 0;
	}


	/* Paranoia */
	if (len > 1023)
	{
		borg_oops("message too long");
		return;
	}


	/* Handle monster messages ("Someone xxx.") */
	if (borg_parse_aux_monster_1(msg, len)) return;

	/* Handle monster messages ("xxx someone.") */
	if (borg_parse_aux_monster_2(msg, len)) return;


	/* Handle terrain feature messages */
	if (borg_parse_aux_terrain(msg, len)) return;


	/* Handle player state change messages */
	if (borg_parse_aux_player(msg, len)) return;


	/* Handle atmospheric messages */
	if (borg_parse_aux_atmosphere(msg, len)) return;


	/* Unknown message */
	borg_note(format("? Unknown message <%s>", msg));
}



/*
 * Parse a message, piece of a message, or set of messages.
 *
 * We must handle long messages which are "split" into multiple pieces,
 * and multiple messages which may be "combined" into a single line, and
 * the combination of a long message and some short ones in two lines.
 *
 * All "continuation" messages always start with a space, which is not
 * part of the original message, and then have another space, which is
 * part of the original message, if the split was made on a space, so
 * in either case, simply skipping the first space, and appending to
 * our collection buffer, will do exactly what we want.
 */
void borg_parse(cptr msg)
{
	static int len = 0;
	static char buf[1024];


	/* Flush messages */
	if (len && (!msg || (msg[0] != ' ')))
	{
		int i, j;

		/* Split on "punctuation" */
		for (j = i = 0; i < len-1; i++)
		{
			/* Proper punctuation */
			if (((buf[i] == '.') ||
			     (buf[i] == '!') ||
			     (buf[i] == '?')) &&
			    (buf[i+1] == ' '))
			{
				/* Terminate */
				buf[i+1] = '\0';

				/* Parse fragment */
				borg_parse_aux(buf + j, (i+1) - j);

				/* Restore */
				buf[i+1] = ' ';

				/* Advance 'j' to next non-space */
				for (j = (i+1)+1; buf[j] == ' '; j++) /* loop */;
			}
		}

		/* Parse tail */
		borg_parse_aux(buf + j, len - j);

		/* Forget */
		len = 0;
	}


	/* No message */
	if (!msg)
	{
		/* Start over */
		len = 0;
	}

	/* Continued message */
	else if (msg[0] == ' ')
	{
		/* Collect, verify, and grow */
		len += strnfmt(buf+len, 1024-len, "%s", msg+1);
	}

	/* New message */
	else
	{
		/* Collect, verify, and grow */
		len = strnfmt(buf, 1024, "%s", msg);
	}
}




#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

