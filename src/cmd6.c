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
 * Describe charges.
 */

void item_charges(object_type * o_ptr)
{
	/* Require staff/wand */
	if ((o_ptr->tval != TV_STAFF) && (o_ptr->tval != TV_WAND))
		return;

	/* No more charges. */
	if (o_ptr->pval == 0)
	{
		mprint(MSG_WARNING, "There are no more charges left!");

		o_ptr->ident |= (IDENT_EMPTY);
		return;
	}

	/* Require known item */
	if (!object_known_p(o_ptr))
		return;

	/* Multiple charges */
	if (o_ptr->pval != 1)
	{
		/* Print a message */
		msg_format("There are %d charges remaining.", o_ptr->pval);
	}

	/* Single charge */
	else
	{
		/* Print a message */
		msg_format("There is %d charge remaining.", o_ptr->pval);
	}
}


/*
 * Only organic food is edible.
 */
bool item_tester_hook_edible(object_type * o_ptr)
{
	if (o_ptr->tval == TV_FOOD && o_ptr->stuff == STUFF_FLESH)
		return TRUE;
	return FALSE;
}

/*
 * Eat some food (from the pack or floor)
 */
void do_cmd_eat_food(void)
{

	if (p_ptr->no_eating)
	{
		mprint(MSG_TEMP, "You can't eat the repellent human food.");
		return;
	}

	item_effect("food item", "eat", TRUE, TRUE, SOUND_EAT,
		item_tester_hook_edible, 0);
}




/*
 * Quaff a potion (from the pack or the floor)
 */
void do_cmd_quaff_potion(void)
{
	item_effect("potion", "drink", TRUE, TRUE, SOUND_QUAFF, NULL,
		TV_POTION);
}


/*
 * Curse the players armor
 */
bool curse_armor(void)
{
	object_type *o_ptr;

	char o_name[80];


	/* Curse the body armor */
	o_ptr = equipment[EQUIP_BODY];

	/* Nothing to curse */
	if (o_ptr)
		return (FALSE);


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
		mformat(MSG_WARNING, "A terrible black aura blasts your %s!",
			o_name);

		/* Blast the armor */
		o_ptr->name1 = 0;
		make_ego_item_named(o_ptr, EGO_BLASTED, p_ptr->depth, FALSE);

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
	o_ptr = equipment[EQUIP_WIELD];

	/* Nothing to curse */
	if (!o_ptr)
		return (FALSE);


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
		mformat(MSG_WARNING, "A terrible black aura blasts your %s!",
			o_name);

		/* Shatter the weapon */
		o_ptr->name1 = 0;
		make_ego_item_named(o_ptr, EGO_SHATTERED, p_ptr->depth, FALSE);

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


void show_book_number(int num)
{
	char buf[1024];

	path_build(buf, 1024, ANGBAND_DIR_FILE, format("book-%d.txt", num));

	screen_save();

	show_file(buf, "Arcane Knowledge", 0, 0);

	screen_load();
}


/*
 * Read a scroll (from the pack or floor).
 */
void do_cmd_read_scroll(void)
{

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

	item_effect("scroll", "read", TRUE, TRUE, 0, NULL, TV_SCROLL);
};


/*
 * Use a staff
 *
 * One charge of one staff disappears.
 *
 */
void do_cmd_use_staff(void)
{

	object_type *o_ptr;

	o_ptr =
		item_effect("staff", "use", FALSE, FALSE, SOUND_ZAP, NULL,
		TV_STAFF);

	if (!o_ptr)
		return;

	/* Use a single charge */
	o_ptr->pval--;

	/* Describe charges. */
	item_charges(o_ptr);

	/* Handle charge ``theft'' in a shop. */
	if (o_ptr->world == WORLD_STORE)
	{
		player_theft();
	}
}

/*
 * Aim a wand (from the pack or floor).
 *
 * Use a single charge from a single item.
 */
void do_cmd_aim_wand(void)
{

	object_type *o_ptr;

	o_ptr =
		item_effect("wand", "zap", FALSE, FALSE, SOUND_ZAP, NULL, TV_WAND);

	if (!o_ptr)
		return;

	/* Use a single charge */
	o_ptr->pval--;


	/* Describe charges. */
	item_charges(o_ptr);

	/* Handle charge ``theft'' in a shop. */
	if (o_ptr->world == WORLD_STORE)
	{
		player_theft();
	}
}



bool item_tester_hook_rod(object_type * o_ptr)
{
	if (o_ptr->tval == TV_ROD && o_ptr->timeout == 0)
		return TRUE;
	return FALSE;
}

/*
 * Activate (zap) a Rod
 *
 */
void do_cmd_zap_rod(void)
{

	item_effect("rod", "zap", FALSE, FALSE, SOUND_ZAP,
		item_tester_hook_rod, 0);

}




/*
 * Hook to determine if an object is activatable
 */
static bool item_tester_hook_activate(object_type * o_ptr)
{
	u32b f1, f2, f3;
	int i;

	if (o_ptr->tval == TV_RANDART)
		return TRUE;

	/* Not known */
	if (!object_known_p(o_ptr))
		return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE))
	{

		/* Check that it's worn. */
		for (i = 0; i < EQUIP_MAX; i++)
		{
			if (equipment[i] == o_ptr)
				return TRUE;
		}
	}

	/* Assume not */
	return (FALSE);
}


void do_cmd_activate(void)
{
	item_effect("item", "activate", FALSE, FALSE, SOUND_ZAP,
		item_tester_hook_activate, 0);
}



/*
 * This is a generic function to ``activate'' an item -- i.e. drink potion,
 * eat mushroom, zap wands and rods, etc.
 *
 * Arguments:
 *
 *     name -- name of item; i.e. potion, scroll, wand.
 *     act  -- verb; i.e. drink, read, zap.
 *     obvious -- don't need to make a skill check.
 *     one_time_use -- delete the item when finished.
 *     snd -- what sound to play.
 *     hook -- a hook for inventory selection.
 *     tval -- in case you want to use a tval instead of a hook.
 */

object_type *item_effect(cptr name, cptr act, bool obvious,
	bool one_time_use, int snd, bool(hook) (object_type *), s16b tval)
{

	int lev, chance;
	object_type *o_ptr;
	object_kind *k_ptr;

	char q[80];
	char s[80];

	int mode = (USE_INVEN | USE_FLOOR);

	bool success = FALSE;


	if (p_ptr->inside_special == SPECIAL_ARENA)
	{
		msg_print("The arena absorbs all magic.");
		msg_print(NULL);
		return NULL;
	}

	/* Prepare the hooks. */
	if (hook)
	{
		item_tester_hook = hook;

	}
	else if (tval)
	{
		item_tester_tval = tval;
	}

	if (one_time_use)
	{
		mode |= (USE_JUST_ONE | USE_REMOVE);
	}

	/* Get an item */
	strfmt(q, "%^s which %s", act, name);
	strfmt(s, "You don't have any %ss you can %s.", name, act);

	o_ptr = get_item(q, s, p_ptr->py, p_ptr->px, mode);

	if (!o_ptr)
		return NULL;

	/* Take a turn */
	p_ptr->energy_use = 100;

	k_ptr = &k_info[o_ptr->k_idx];

	/* Extract the item level */
	lev = k_ptr->level;

	/* Hack -- use artifact level instead */
	if (artifact_p(o_ptr))
	{
		if (o_ptr->tval == TV_RANDART)
		{
			lev = random_artifacts[o_ptr->sval].level;
		}
		else
		{
			lev = a_info[o_ptr->name1].level;
		}
	}


	if (!obvious)
	{

		/* Base chance of success */
		chance = p_ptr->skill_dev;

		/* Confusion hurts skill */
		if (p_ptr->confused)
			chance = chance / 2;

		/* Hight level objects are harder */
		chance = chance - ((lev > 50) ? 50 : lev);

		/* Give everyone a (slight) chance */
		if ((chance < USE_DEVICE) &&
			(rand_int(USE_DEVICE - chance + 1) == 0))
		{
			chance = USE_DEVICE;
		}

		/* Roll for usage */
		if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
		{

			if (flush_failure)
				flush();

			msg_format("You failed to %s it properly.", act);

			goto done; 

		}
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		mprint(MSG_TEMP, "It whines, glows and fades...");

		goto done;
	}

	/* Mondo-hack -- check wand/staff charges. */
	if ((o_ptr->tval == TV_WAND || o_ptr->tval == TV_STAFF) &&
		o_ptr->pval == 0)
	{

		item_charges(o_ptr);
		return NULL;
	}

	/* Activate the artifact */
	mformat(MSG_TEMP, "You %s it...", act);

	/* Sound */
	if (snd)
	{
		sound(snd);
	}

	if (o_ptr->tval == TV_RANDART)
	{
		random_artifact *rart = &random_artifacts[o_ptr->sval];

		success = cause_spell_effect(&activations[rart->activation]);
		o_ptr->timeout = rart->level * 10 + randnor(0, rart->level);

		/* Paranoia. */
		if (o_ptr->timeout < 1)
			o_ptr->timeout = 1;

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return o_ptr;
	}


	if (artifact_p(o_ptr))
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		mformat(MSG_BONUS, a_text + a_ptr->text);

		success = cause_spell_effect(&activations[a_ptr->activation]);
		o_ptr->timeout =
			rand_int(a_ptr->timeout_rand) + a_ptr->timeout_static;

	}
	else
	{
		success = cause_spell_effect(&activations[k_ptr->activation]);
		o_ptr->timeout =
			rand_int(k_ptr->timeout_rand) + k_ptr->timeout_static;
	}

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);


	/* The item has been tried */
	object_tried(o_ptr);

	/* An identification was made */
	if (success && !object_aware_p(o_ptr))
	{
		char o_name[80];

		object_aware(o_ptr);
		gain_exp((lev + (p_ptr->lev >> 1)) / p_ptr->lev);

		object_desc(o_name, o_ptr, FALSE, 0);

		/* Print a helpful message. */
		msg_format("It seems it %s a %s.", (one_time_use ? "was" : "is"),
			o_name);
	}

	/* Handle fate. */
	fate_effect(o_ptr->fate, FATE_USE);

	/* HACK! */
	if (o_ptr->fate == FATE_USE) {
	  o_ptr->fate = FATE_NONE;
	}

 done:
	
	/* HACK. */
	if (one_time_use) {

	  fate_effect(o_ptr->fate, FATE_KILL);

	  remove_object(o_ptr);
	  o_ptr = NULL;
	}

	return o_ptr;
}



/* Test for an ingredient. */

static bool item_tester_hook_ingr(object_type * o_ptr)
{
	if (o_ptr->tval == TV_INGRED)
		return (TRUE);
	return (FALSE);
}


/*
 * Maximum number of pages of recipes
 */
#define MAX_RECIPE_PAGES (MAX_RECIPES / 12 + 1)


/*
 * Collect the first recipe in each page.
 */
int make_recipe_pages(int pages[MAX_RECIPE_PAGES], int per_page)
{
	int i = 0, k, page_cnt;

	/* Default to no pages */
	page_cnt = 0;

	/* Check each recipe */
	for (k = 0; k < MAX_RECIPES; k++)
	{
		/* Require known recipe */
		if (!recipe_recall[k]) continue;

		/* This object is first in a page */
		if (!(i % per_page))
		{
			/* Remember the first object in this page */
			pages[page_cnt++] = k;
		}

		/* Count recipes */
		++i;
	}

	/* Terminate */
	pages[page_cnt] = -1;

	/* Return number of pages */
	return page_cnt;
}


/*
 * Display a single page of recipes.
 */
int show_recipe_page(int pages[MAX_RECIPE_PAGES], int page_cnt,
	int page_cur, int per_page, int row, u32b mask)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, k, n = 0, x;
	u32b ingrs, ingrs_all;
	object_type o_body, *o_ptr;

	char o_name[80];


	/* Default to no ingredients available */
	ingrs_all = 0;

	/* Check every pack item */
	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{
		/* Require an ingredient */
		if (o_ptr->tval != TV_INGRED) continue;

		/* Remember this ingredient */
		ingrs_all |= (1L << o_ptr->sval);
	}

	/* Check every floor item */
	for (o_ptr = cave_o_idx[py][px]; o_ptr != NULL; o_ptr = o_ptr->next)
	{
		/* Require an ingredient */
		if (o_ptr->tval != TV_INGRED) continue;

		/* Remember this ingredient */
		ingrs_all |= (1L << o_ptr->sval);
	}

	/* Check each recipe on this page */
	for (k = pages[page_cur]; k < MAX_RECIPES; k++)
	{
		byte a = TERM_WHITE;

		/* Require known recipe */
		if (!recipe_recall[k]) continue;

		/* Get the ingredients */
		ingrs = recipe_info[k].ingrs;

		if (recipe_recall[k] == 1)
		{
			/* Get resulting object description. */
			object_prep(&o_body, recipe_info[k].result_kind);
			object_desc_store(o_name, &o_body, FALSE, 0);
		}
		else
		{
			strcpy(o_name, "Something");
		}

		/* All the ingredients were mixed */
		if (ingrs == mask)
			a = TERM_L_GREEN;

		/* All the ingredients are available or mixed */
		else if ((ingrs & (ingrs_all | mask)) == ingrs)
			a = TERM_L_BLUE;

		/* Display object, clear line */
		c_prt(a, o_name, row + n, 2);

		/* Display ingredients to the right */
		x = 45;

		/* Check each possible ingredient */
		for (i = 0; i < 16; i++)
		{
			/* This ingredient is in the recipe */
			if (ingrs & (1L << i))
			{
				byte a = TERM_WHITE;
				cptr s = ingr_short_names[i];

				/* The ingredient was mixed */
				if (mask & (1L << i))
					a = TERM_L_GREEN;

				/* This ingredient is available */
				else if (ingrs_all & (1L << i))
					a = TERM_L_BLUE;

				/* Display ingredient */
				c_put_str(a, s, row + n, x);

				/* Skip */
				x += strlen(s) + 1;
			}
		}

		/* Stop after whole page displayed */
		if (++n == per_page)
			break;
	}

	/* Return number of displayed recipes */
	return n;
}


/*
 * Mix some ingredients, and report the result.
 */
static void do_cmd_brew_stuff_aux(u32b mask, int boost)
{
	int i, level;
	u32b ingrs;
	bool made_ok;


	made_ok = FALSE;

	for (i = 0; i < MAX_RECIPES; i++)
	{
		ingrs = recipe_info[i].ingrs;

		if (ingrs == 0)
			break;

		if (mask != ingrs)
			continue;

		level = (k_info[recipe_info[i].result_kind].level / 2) + boost;

		/* All items should be possible */
		if (level > 50)
			level = 50;

		if (p_ptr->lev > level + randnor(0, 7))
		{
			made_ok = TRUE;
		}

		if ((level <= p_ptr->lev) && made_ok)
		{
			mprint(MSG_BONUS, "You have made something.");
		}
		else if ((level > p_ptr->lev) && made_ok)
		{
			mprint(MSG_BONUS,
				"Somehow you managed to make something.");
		}
		else if ((level <= p_ptr->lev) && !made_ok)
		{
			mprint(MSG_WARNING, "You messed up somewhere.");
		}
		else if ((level > p_ptr->lev) && !made_ok)
		{
			mprint(MSG_WARNING, "The mix explodes in your face!");
		}

		if (made_ok)
		{
			object_type *to_drop = new_object();

			object_prep(to_drop, recipe_info[i].result_kind);
			apply_magic(to_drop, level, FALSE, TRUE, TRUE);

			object_aware(to_drop);
			object_known(to_drop);

			drop_near(to_drop, FALSE, p_ptr->py, p_ptr->px);

			recipe_recall[i] = 1;
		}
		else
		{
			nasty_side_effect();

			if (!recipe_recall[i])
				recipe_recall[i] = -1;
		}

		return;
	}

	msg_print("You mix something gloppy and evil-smelling.");
}


/*
 * Make something using alchemical formulae.
 */
void do_cmd_brew_stuff(void)
{
	bool done, redraw;
	char command;
	u32b mask = 0;
	int boost = 0;
	int per_page, page_cur, page_cnt = 0;
	int pages[MAX_RECIPE_PAGES];


	/* XXX Hack -- Increase "icky" depth */
	character_icky++;

	/* Clear the screen */
	Term_clear();

	/* Calculate number of recipes per page (minus -more- prompt) */
	per_page = screen_y - 8 - 1;

	/* Split known recipes into pages */
	page_cnt = make_recipe_pages(pages, per_page);

	/* Start on the first page of recipes */
	page_cur = 0;

	/* Redraw recipes */
	redraw = TRUE;

	/* Not done yet */
	done = FALSE;

	while (!done)
	{
		put_str("Recipes", 2, 2);
	
		put_str("ESC) Exit", screen_y - 2, 1);

		if (page_cnt > 1)
			put_str("-,+,space) Browse recipes", screen_y - 1, 1);
	
		put_str("g) Choose ingredient", screen_y - 2, 40);
		put_str("m) Mix recipe", screen_y - 1, 40);

		if (redraw)
		{
			/* Display the current page */
			int i, n;

			n = show_recipe_page(pages, page_cnt, page_cur, per_page, 4, mask);

			/* Erase the extra lines and the "more" prompt */
			for (i = n; i < per_page + 1; i++)
				prt("", i + 4, 0);

			/* Assume "no current page" */
			put_str("           ", 2, 10);

			/* Visual reminder of "more recipes" */
			if (page_cnt > 1)
			{
				char s[10] = "  more  ";
		
				if (page_cur > 0)
					s[0] = '-';
				if (page_cur < page_cnt - 1)
					s[7] = '+';
		
				/* Show "more" reminder (after the last recipe) */
				prt(s, n + 4, 2);

				/* Indicate the "current page" */
				put_str(format("(Page %d/%d)", page_cur + 1,
					page_cnt), 2, 10);
			}

			/* Forget redraw */
			redraw = FALSE;
		}

		put_str("You may: ", screen_y - 3, 0);

		/* XXX Hack -- Assume messages were seen */
		msg_flag = FALSE;

		/* Get a key */
		command = inkey();

		/* Clear top line */
		prt("", 0, 0);

		switch (command)
		{
			case ESCAPE:
			{
				if (mask)
				{
					if (!get_check("Really throw away these ingredients? "))
						break;
					mask = 0;
				}
				
				done = TRUE;
				break;
			}

			case ' ':
			{
				if (page_cnt == 1)
				{
					bell();
					break;
				}

				if (++page_cur >= page_cnt)
					page_cur = 0;

				redraw = TRUE;
				break;
			}

			case '-':
			{
				if (page_cur > 0)
				{
					--page_cur;
					redraw = TRUE;
				}
				else
				{
					bell();
				}
				
				break;
			}

			case '+':
			case '=':
			{
				if (page_cur < page_cnt - 1)
				{
					++page_cur;
					redraw = TRUE;
				}
				else
				{
					bell();
				}
				
				break;
			}

			case 'G':
			case 'g':
			{
				cptr q, s;
				object_type *o_ptr;

				item_tester_hook = item_tester_hook_ingr;
		
				q = "Combine what";
				s = "You don't have any ingredients left!";

				o_ptr =
					get_item(q, s, p_ptr->py, p_ptr->px,
					(USE_INVEN | USE_FLOOR | USE_JUST_ONE | USE_REMOVE));
		
				if (!o_ptr)
					break;
		
				/* Hack -- reward redundancy. */
				if (mask & (1L << o_ptr->sval))
				{
					boost += 3 * o_ptr->number;
				}
				else
				{
					mask |= (1L << o_ptr->sval);
		
					/* Hack -- reward redundant ingredients. */
					if (o_ptr->number > 1)
					{
						boost += 3 * (o_ptr->number - 1);
					}
				}
		

				/* Handle fate. */
				fate_effect(o_ptr->fate, FATE_KILL);

				remove_object(o_ptr);

				redraw = TRUE;
				break;
			}

			case 'M':
			case 'm':
			{
				if (!mask)
				{
					mprint(MSG_TEMP, "You must choose some ingredients.");
					msg_print(NULL);
					break;
				}

				done = TRUE;

				break;
			}
		}
	}

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Redraw */
	p_ptr->redraw |= (PR_WIPE | PR_BASIC | PR_EXTRA | PR_MAP);

	/* Handle stuff */
	handle_stuff();

	/* Something was mixed */
	if (mask)
	{
		/* Take a turn */
		p_ptr->energy_use = 100;

		/* Mix the ingredients */
		do_cmd_brew_stuff_aux(mask, boost);
	}
}


static bool item_tester_hook_sacrifice(object_type * o_ptr)
{
	int i;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i] == o_ptr && (cursed_p(o_ptr) ||
				show_inven_equip == FALSE))
			return FALSE;
	}

	if (object_value(o_ptr) * o_ptr->number > 0 ||
	    o_ptr->fate == FATE_SACRIFICE)
		return TRUE;

	return FALSE;
}

/*
 * Handle sacrifices.
 * Grace is increased by value of sacrifice.
 */

void do_cmd_sacrifice(void)
{

	object_type *o_ptr;

	int val;

	byte on_what = cave_feat[p_ptr->py][p_ptr->px];
	byte what_god;

	/* Check valididty */

	if (on_what < FEAT_ALTAR_HEAD || on_what > FEAT_ALTAR_TAIL)
	{
	  
	  /* HACK. */
	  if (p_ptr->fated) {
	    show_god_info(TRUE);

	  } else {
	    show_god_info(FALSE);
	  }
	  return;
	}

	what_god = on_what - FEAT_ALTAR_HEAD + 1;

	item_tester_hook = item_tester_hook_sacrifice;

	/* Get sacrifice */
	o_ptr =
		get_item("Sacrifice what", "You don't have anything to sacrifice.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_BY_PARTS | USE_REMOVE));

	if (!o_ptr)
		return;

	p_ptr->energy_use = 100;

	val = object_value(o_ptr) * o_ptr->number;

	/* Modify grace */

	if (p_ptr->pgod == 0)
	{
		p_ptr->pgod = what_god;
		set_grace(p_ptr->grace + val);
		p_ptr->god_favor = -60000;

	}
	else if (p_ptr->pgod != what_god)
	{
		mformat(MSG_DEADLY, "%s thunders in outrage at your blasphemy!",
			deity_info[p_ptr->pgod - 1].name);

		set_grace(p_ptr->grace - val * 10);

		if (val > 2500)
		{
			mformat(MSG_WARNING, "You feel %s abandon you.",
				deity_info[p_ptr->pgod - 1].name);

			p_ptr->pgod = what_god;
			set_grace(val);
			p_ptr->god_favor = -60000;
		}

	}
	else
	{
		set_grace(p_ptr->grace + val * 5);
	}

	/* Handle fate. */
	fate_effect(o_ptr->fate, FATE_SACRIFICE);

	remove_object(o_ptr);
}


/* 
 * Handle CLI commands 
 */

void do_cmd_cli(void)
{
	char buff[80];
	int i;

	strcpy(buff, "");

	if (!get_string_cli("Command: ", buff, 30))
		return;

	for (i = 0; i < MAX_COMMANDS && cli_info[i].comm1; i++)
	{
		if (!strcmp(buff, cli_info[i].comm1))
		{
			cli_info[i].func();
			return;
		}
		else if (cli_info[i].comm2 && !strcmp(buff, cli_info[i].comm2))
		{
			cli_info[i].func();
			return;
		}
	}

	mformat(MSG_TEMP, "No such command: %s", buff);
}

/* Front-end to spell learning commands */

void do_cmd_gain_helper(void)
{
	if (p_ptr->prace == RACE_MUNCHKIN || p_ptr->munchkin ||
		p_ptr->pclass == CLASS_VAMPIRE)
	{
		do_cmd_study();

	}
	else if (p_ptr->prace == RACE_GHOST)
	{
		msg_print("You try to recall a spell from your past...");
		do_cmd_study();

	}
	else if (!cp_ptr->magic_innate)
	{
		mprint(MSG_TEMP, "You must visit your superiors and be taught.");
		msg_print(NULL);

	}
	else
	{
		do_cmd_study();
	}
}

/* Save and quit */

void do_cmd_save_quit(void)
{
	/* Stop playing */
	p_ptr->playing = FALSE;

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/* 
 * Print CLI help 
 */

void do_cmd_command_help(void)
{
	int i;
	FILE *fff;
	char file_name[1024];

	if (path_temp(file_name, 1024))
		return;

	fff = my_fopen(file_name, "w");

	for (i = 0; i < MAX_COMMANDS && cli_info[i].comm1; i++)
	{
		if (cli_info[i].comm2)
		{
			fprintf(fff, "    %s, %s -- %s\n", cli_info[i].comm1,
				cli_info[i].comm2, cli_info[i].descrip);
		}
		else
		{
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

static bool tport_vertically(bool how)
{
	if (p_ptr->inside_special > 0)
	{ /* arena or quest -KMW- */
		mprint(MSG_TEMP, "There is no effect.");
		return FALSE;
	}

	/* Go down */

	if (how)
	{
		if (p_ptr->inside_special == SPECIAL_QUEST ||
		    p_ptr->depth >= MAX_DEPTH - 1)
		{
			mformat(MSG_TEMP, "The floor is impermeable.");
			return FALSE;
		}

		if (p_ptr->inside_special == SPECIAL_WILD) {
		  msg_print("You sink through the ground.");

		  p_ptr->wilderness_depth = p_ptr->depth;
		  p_ptr->inside_special = FALSE;

		} else {
		  msg_print("You sink through the floor.");
		}

		p_ptr->depth++;
		p_ptr->leaving = TRUE;
	}
	else
	{
		if (!p_ptr->depth)
		{
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

void do_cmd_immovable_special(void)
{
	int i;
	int foo = p_ptr->immov_cntr;
	int lose_sp = 0;
	int lose_hp = 0;
	bool did_act = FALSE;
	bool did_load = FALSE;

	if (foo > 1)
	{
		if (p_ptr->csp > foo / 2)
		{

			mformat(MSG_WARNING, "This will drain %d mana points!",
				foo / 2);
			if (!get_check("Proceed? "))
				return;

			lose_sp = foo / 2;

		}
		else if (p_ptr->chp > foo / 2)
		{

			mformat(MSG_WARNING, "Warning: This will drain %d hit points!",
				foo / 2);
			if (!get_check("Proceed? "))
				return;

			lose_hp = foo / 2;

		}
		else
		{
			msg_print("You can't use your powers yet.");
			return;
		}
	}

	/* Save the screen */
	screen_save();


	/* Interact until done */
	while (1)
	{
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
		if (i == ESCAPE)
			break;

		/* Tele-to */
		if (i == '1')
		{
			screen_load();

			did_load = TRUE;

			if (!target_set(TARGET_GRID))
				break;

			/* Teleport to the target */
			teleport_player_to(p_ptr->target_row, p_ptr->target_col);

			did_act = TRUE;
			break;
		}

		/* Fetch item */
		else if (i == '2')
		{
			screen_load();

			did_load = TRUE;

			if (!target_set(TARGET_GRID))
				return;

			if (!fetch_item(p_ptr->lev * 15, -1, -1))
				return;

			did_act = TRUE;
			break;
		}

		/* Move up */
		else if (i == '3')
		{
			screen_load();

			did_load = TRUE;

			if (!tport_vertically(FALSE))
				return;

			did_act = TRUE;
			break;
		}

		/* Move down */
		else if (i == '4')
		{
			screen_load();

			did_load = TRUE;

			if (!tport_vertically(TRUE))
				return;

			did_act = TRUE;
			break;
		}

		/* Unknown option */
		else
		{
			bell();
		}

	}

	/* Check if screen was restored before */
	if (!did_load)
	{
		/* Restore the screen */
		screen_load();
	}

	/* Apply stat losses if something was done */
	if (did_act)
	{
		p_ptr->immov_cntr += 101 - (p_ptr->lev * 2);

		if (lose_sp)
		{
			p_ptr->csp -= lose_sp;
			p_ptr->redraw |= (PR_MANA);
		}

		if (lose_hp)
		{
			p_ptr->chp -= lose_hp;
			p_ptr->redraw |= (PR_HP);
		}
	}
}


/*
 * Figure out your shape-change chance.
 */

static int get_mimic_change_chance(int chance)
{
	chance -= (p_ptr->lev * 3);
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

	if (chance < 2)
		chance = 2;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50)
		chance += 25;
	else if (p_ptr->stun)
		chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95)
		chance = 95;

	/* Return the chance */
	return (chance);
}

/* 
 * Change your shape, using mimic books. Change into abomination
 * when failed.
 */

void do_cmd_change_shape(void)
{

	int chance;
	object_type *o_ptr;

	if (cp_ptr->spell_book != TV_MIMIC_BOOK)
	{
		mprint(MSG_TEMP, "You do not have shape-shifting abilities.");
		return;
	}

	if (p_ptr->blind || no_lite())
	{
		mprint(MSG_TEMP, "You cannot see!");
		return;
	}

	if (p_ptr->confused)
	{
		mprint(MSG_TEMP, "You are too confused!");
		return;
	}

	item_tester_tval = TV_MIMIC_BOOK;

	o_ptr =
		get_item("Use which book", "You have no books of lore.", p_ptr->py,
		p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return;

	/* Hack -- Handle stuff */
	handle_stuff();

	chance = get_mimic_change_chance(o_ptr->pval);

	if (chance > 75)
	{
		mprint(MSG_WARNING, "You feel uneasy with this shape-change.");
		if (!get_check("Try it anyway? "))
		{
			return;
		}
	}

	if (randint(100) < chance)
	{
		mprint(MSG_WARNING, "Your shape-change goes horribly wrong!");

		if (randint(100) < p_ptr->skill_sav)
		{
			msg_print("You manage to wrest your body back under control.");
		}
		else
		{
			change_shape(SHAPE_ABOMINATION);
		}
	}
	else
	{
		change_shape(o_ptr->sval);
	}
}

/* Front-end to spell casting commands */

void do_cmd_cast_helper(void)
{

	/* Hack -- allow intrinsic power usage at all times. */
	if (p_ptr->shape)
	{
		bool foo = TRUE;

		if (p_ptr->pclass == CLASS_MIMIC || p_ptr->pclass == CLASS_LYCANTH
			|| p_ptr->pclass == CLASS_BEASTMASTER)
		{
			foo =
				get_check
				("Attempt to use the magical powers of your shape? ");
		}

		if (foo)
		{
			do_cmd_cast_power();
			return;
		}
	}

	if (p_ptr->pclass == CLASS_BEASTMASTER)
	{
		summon_pet_monster();

	}
	else if (p_ptr->pclass == CLASS_LYCANTH)
	{
		change_shape(SHAPE_WOLF);

	}
	else if (p_ptr->pclass == CLASS_MIMIC)
	{
		do_cmd_change_shape();

	}
	else
	{
		do_cmd_cast_power();
	}
}

/*
 * Command to ask favors from your god.
 */

void do_cmd_pray(void)
{
	int level;
	cptr name;

	if (p_ptr->pgod == 0)
	{
		mprint(MSG_TEMP,
			"Pray hard enough and your prayers might be answered.");
		return;
	}

	if (confirm_prayers &&
	    !get_check("Are you sure you want to disturb your God? "))
	  return;

	level = interpret_grace() - interpret_favor();
	name = deity_info[p_ptr->pgod - 1].name;

	if (p_ptr->pclass == CLASS_PRIEST && magik(30)) {

	  level++;
	} else if (p_ptr->pclass == CLASS_PALADIN && magik(10)) {

	  level++;
	} else if (p_ptr->pclass == CLASS_AVATAR && magik(30)) {
	  
	  level += rand_int(3);
	}

	if (level < 0)
		level = 0;
	if (level > 10)
		level = 10;

	p_ptr->energy_use = 100;

	switch (level)
	{
		case 10:
			mformat(MSG_BIG_BONUS,
				"%s thunders: ``Thou hast pleaseth me, mortal.''", name);
			great_side_effect();
			break;

		case 9:
			mformat(MSG_BIG_BONUS,
				"%s booms out: ``Thy deeds earn thou a worthy reward.''",
				name);
			good_side_effect();
			break;

		case 8:
			mformat(MSG_BONUS, "%s thunders: ``Well done, motal.''", name);
			ok_side_effect();
			break;

		case 7:
		case 6:
			mformat(MSG_STUPID,
				"%s hisses: ``Thou shouldst not have done that, mortal!''",
				name);
			neutral_side_effect();
			if (magik(30))
				set_grace(p_ptr->grace - 1000);
			break;

		case 5:
		case 4:
		case 3:
			mformat(MSG_URGENT,
				"%s quakes in rage: ``Thou art supremely insolent, mortal!''",
				name);
			nasty_side_effect();
			set_grace(p_ptr->grace - 5000);
			break;

		case 2:
		case 1:
		case 0:
			mformat(MSG_DEADLY,
				"%s whipsers: ``Prepare to die, mortal...''", name);
			deadly_side_effect(TRUE);
			set_grace(p_ptr->grace - 20000);
			break;
	}

	p_ptr->god_favor += 25000;
}
