/* File: cmd3.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/*
 * Item tester hook (blocks out wielded equipment)
 */
static bool item_tester_hook_show_inven_equip(object_type * o_ptr)
{
	int i;

	for (i = 0; i < EQUIP_MAX; i++)
		if (equipment[i] == o_ptr)
			return FALSE;

	return TRUE;
}


/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	bool flag = FALSE;
	bool redraw = TRUE;
	char prompt[80];
	int per_page, page_cur, page_cnt;
	object_type *pages[MAX_STACK_PAGES];


	/* Test items against wielded equipment, if necessary */
	if (show_inven_equip == FALSE)
		item_tester_hook = item_tester_hook_show_inven_equip;

	/* Calculate number of items per screenful */
	per_page = screen_y - 2;

	/* XXX Hack -- Only a-z possible */
	if (per_page > 26)
		per_page = 26;

	/* Split the stack into screenfuls */
	page_cnt = make_stack_pages(inventory, pages, per_page, FALSE);

	/* Display the first page */
	page_cur = 0;

	/* Build a prompt */
	(void) sprintf(prompt,
		"(Inventory) burden %d.%d lb (%d%% of capacity). Command: ",
		p_ptr->total_weight / 10, p_ptr->total_weight % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);

	while (!flag)
	{
		int ch;

		/* Display the items */
		if (redraw)
		{
			/* Save screen */
			screen_save();

			/* Display the current page */
			show_stack_page(pages, page_cnt, page_cur, per_page, FALSE, 0);

			/* Prompt */
			prt(prompt, 0, 0);

			/* Cancel request */
			redraw = FALSE;
		}

		/* Get a key */
		ch = inkey();

		switch (ch)
		{
			case ESCAPE:
			{
				flag = TRUE;
				break;
			}

			case ' ':
			{
				if (page_cnt == 1)
				{
					bell();
					break;
				}

				/* Advance a page */
				++page_cur;

				/* Wrap as needed */
				if (page_cur >= page_cnt)
					page_cur = 0;

				/* Load screen */
				screen_load();

				/* Request redisplay */
				redraw = TRUE;

				break;
			}

			case '-':
			{
				if (page_cur > 0)
				{
					/* Go back a page */
					--page_cur;

					/* Load screen */
					screen_load();

					/* Request redisplay */
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
					/* Go forward a page */
					++page_cur;

					/* Load screen */
					screen_load();

					/* Request redisplay */
					redraw = TRUE;
				}
				else
				{
					bell();
				}

				break;
			}

			default:
			{
				/* Save the command */
				p_ptr->command_new = ch;

				/* Done */
				flag = TRUE;

				break;
			}
		}
	}

	/* Load screen */
	screen_load();

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;
}


/*
 * Display equipment
 */
void do_cmd_equip(void)
{
	char prompt[80];

	if (p_ptr->no_equip)
	{
		mprint(MSG_WARNING, "You cannot use equipment.");
		return;
	}

	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip();

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;

	/* Build the prompt */
	(void) sprintf(prompt,
		"(Equipment) burden %d.%d lb (%d%% of capacity). Command: ",
		p_ptr->total_weight / 10, p_ptr->total_weight % 10,
		p_ptr->total_weight / adj_str_wgt[p_ptr->stat_ind[A_STR]]);

	/* Display the prompt */
	prt(prompt, 0, 0);

	/* Hack -- Get a new command */
	p_ptr->command_new = inkey();

	/* Load screen */
	screen_load();
}


static bool item_tester_hook_wear(object_type * o_ptr)
{
	/* Check for a usable slot */
	int i = wield_slot(o_ptr);
	int j;
	object_type *foo = equipment[i];

	for (j = 0; j < EQUIP_MAX; j++)
	{
		if (equipment[j] == o_ptr)
			return FALSE;
	}

	if (i >= EQUIP_WIELD && (foo == NULL || !cursed_p(foo)))
		return (TRUE);

	/* Assume not wearable */
	return (FALSE);
}

static bool item_tester_hook_worn(object_type * o_ptr)
{
	int i;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i] == o_ptr && !cursed_p(o_ptr))
			return TRUE;
	}

	return FALSE;
}

static bool item_tester_hook_drop(object_type * o_ptr)
{
	int i;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i] == o_ptr && (cursed_p(o_ptr) ||
				show_inven_equip == FALSE))
			return FALSE;
	}

	return TRUE;
}


static bool item_tester_hook_inscr(object_type * o_ptr)
{
	if (o_ptr->note)
		return TRUE;
	return FALSE;
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{

	object_type *o_ptr;
	int slot;

	cptr act;
	char o_name[80];

	if (p_ptr->no_equip)
	{
		mprint(MSG_WARNING, "You cannot use equipment.");
		return;
	}

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	o_ptr =
		get_item("Wear/wield which item",
		"You have nothing you can " "wear or wield.", p_ptr->py, p_ptr->px,
		(USE_INVEN));

	if (!o_ptr)
		return;

	/* Check the slot */
	slot = wield_slot(o_ptr);

	/* Can't wear armor while shape-shifted */
	if (p_ptr->shape && slot >= EQUIP_BODY)
	{
		msg_format("Your body does not fit into this.");
		return;
	}

	/* Confirm when trying to wield a known cursed item. */
	if (confirm_cursed && cursed_p(o_ptr) && (object_known_p(o_ptr) ||
			o_ptr->ident & IDENT_SENSE) &&
		!get_check("Warning: Wield a cursed item? "))
	{
		return;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	equipment[slot] = o_ptr;

	/* Where is the item now */
	if (slot == EQUIP_WIELD)
	{
		act = "You are wielding";
	}
	else if (slot == EQUIP_BOW)
	{
		act = "You are shooting with";
	}
	else if (slot == EQUIP_LITE)
	{
		act = "Your light source is";
	}
	else
	{
		act = "You are wearing";
	}

	/* Describe the result */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("%s %s.", act, o_name);

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		mprint(MSG_URGENT, "Oops! It feels deathly cold!");

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);

	handle_stuff();
}



/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
	object_type *o_ptr;
	int i;

	item_tester_hook = item_tester_hook_worn;

	/* Get an item */
	o_ptr =
		get_item("Take off which item",
		"You are not wearing or weilding " "anything.", p_ptr->py,
		p_ptr->px, (USE_INVEN));

	if (!o_ptr)
		return;

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	for (i = 0; i < EQUIP_MAX; i++)
	{
		if (equipment[i] == o_ptr)
		{
			equipment[i] = NULL;
		}
	}

	p_ptr->redraw |= (PR_SPEED);
	p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA);
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	handle_stuff();
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{

	object_type *o_ptr;

	cptr q = "Drop which item";

	item_tester_hook = item_tester_hook_drop;

	o_ptr =
		get_item(q, "You have nothing you can drop.", p_ptr->py, p_ptr->px,
		(USE_INVEN | USE_REMOVE | USE_BY_PARTS));

	if (!o_ptr)
		return;

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Handle fate here. */
	fate_effect(o_ptr->fate, FATE_DROP);

	drop_near(o_ptr, FALSE, p_ptr->py, p_ptr->px);

}



/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
	object_type *o_ptr;

	char o_name[80];
	char out_val[80];

	item_tester_hook = item_tester_hook_drop;

	o_ptr =
		get_item("Destroy which item", "You have nothing to destroy.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR | USE_BY_PARTS));

	if (!o_ptr)
		return;

	/* Verify destruction XXX XXX XXX */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Verify destruction */
	if (verify_destroy && (verify_destroy_junk ||
			(object_value(o_ptr) >= 1)))
	{
		sprintf(out_val, "Really destroy %s? ", o_name);
		if (!get_check(out_val))
			return;
	}

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Artifacts cannot be destroyed */
	if (artifact_p(o_ptr))
	{
		cptr feel = "special";

		/* Message */
		msg_format("You cannot destroy %s.", o_name);

		/* Hack -- Handle icky artifacts */
		/* Disabled -- No free ID! */
		/* if (cursed_p(o_ptr) || broken_p(o_ptr)) feel = "terrible"; */

		/* Hack -- inscribe the artifact */
		o_ptr->note = quark_add(feel);

		/* We have "felt" it (again) */
		o_ptr->ident |= (IDENT_SENSE);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}

	/* Message */
	msg_format("You destroy %s.", o_name);

	/* Try to explode it. */
	explode_object(o_ptr, p_ptr->py, p_ptr->px);

}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{

	object_type *o_ptr;

	char o_name[80];

	o_ptr =
		get_item("Examine which item", "You have nothing to examine.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return;

	if (o_ptr->tval == TV_TEXT)
	{
		show_book_number(o_ptr->sval);
		return;
	}

	/* Description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe */
	msg_format("Examining %s...", o_name);

	/* Describe it fully */
	identify_fully_aux(o_ptr);
}


/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{

	object_type *o_ptr;

	item_tester_hook = item_tester_hook_inscr;

	o_ptr =
		get_item("Un-inscribe which item",
		"You have nothing to " "un-inscribe.", p_ptr->py, p_ptr->px,
		(USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return;

	/* Message */
	msg_print("Inscription removed.");

	/* Remove the incription */
	o_ptr->note = 0;

	/* Combine the pack */
	p_ptr->notice |= (PN_COMBINE);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{

	object_type *o_ptr;

	char o_name[80];

	char out_val[80];

	item_tester_hook = item_tester_hook_show_inven_equip;

	o_ptr =
		get_item("Inscribe which item", "You have nothing to inscribe.",
		p_ptr->py, p_ptr->px, (USE_INVEN | USE_FLOOR));

	if (!o_ptr)
		return;

	/* Describe the activity */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	msg_format("Inscribing %s.", o_name);
	msg_print(NULL);

	/* Start with nothing */
	strcpy(out_val, "");

	/* Use old inscription */
	if (o_ptr->note)
	{
		/* Start with the old inscription */
		strcpy(out_val, quark_str(o_ptr->note));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string("Inscription: ", out_val, 80))
	{
		/* Save the inscription */
		o_ptr->note = quark_add(out_val);

		/* Combine the pack */
		p_ptr->notice |= (PN_COMBINE);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);
	}
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(object_type * o_ptr)
{
	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK)
		return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(object_type * o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) && (o_ptr->sval == SV_LITE_TORCH))
		return TRUE;

	/* Assume not okay */
	return (FALSE);
}



/*
 * Refill the player's light source.
 */
void do_cmd_refill(void)
{

	object_type *o_ptr;
	object_type *j_ptr = equipment[EQUIP_LITE];

	cptr name;
	cptr fuel_name;
	s16b max;
	s32b tmp = 0;

	if (!j_ptr)
	{
		mprint(MSG_TEMP, "You have no light source to refuel.");
		return;
	}

	/* Restrict the choices */
	if (j_ptr->sval == SV_LITE_LANTERN)
	{
		item_tester_hook = item_tester_refill_lantern;
		name = "lantern";
		fuel_name = "flask";
		max = FUEL_LAMP;

	}
	else if (j_ptr->sval == SV_LITE_TORCH)
	{
		item_tester_hook = item_tester_refill_torch;
		name = "torch";
		fuel_name = "torch";
		max = FUEL_TORCH;

	}
	else
	{
		mprint(MSG_TEMP, "Your light source cannot be refueled.");
		return;
	}

	if (j_ptr->pval >= max)
	{
		mformat(MSG_TEMP, "Your %s is fully fueled.", name);
		item_tester_hook = NULL;
		return;
	}

	o_ptr =
		get_item(format("Refill with which %s", fuel_name),
		"You have no fuel.", p_ptr->py, p_ptr->px,
		(USE_INVEN | USE_FLOOR | USE_REMOVE | USE_JUST_ONE));

	if (!o_ptr)
		return;

	/* Take a partial turn */
	p_ptr->energy_use = 50;

	/* Refuel */
	tmp += o_ptr->pval;

	/* Message */
	msg_format("You fuel your %s.", name);

	/* Comment */
	if (tmp >= max)
	{
		tmp = max;
		msg_format("Your %s is fully fueled.", name);
	}

	j_ptr->pval = tmp;

	remove_object(o_ptr);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}


/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set(TARGET_KILL))
	{
		mprint(MSG_TEMP, "Target Selected.");
	}

	/* Target aborted */
	else
	{
		mprint(MSG_TEMP, "Target Aborted.");
	}
}



/*
 * Look command
 */
void do_cmd_look(void)
{
	/* Look around */
	if (target_set(TARGET_LOOK))
	{
		mprint(MSG_TEMP, "Target Selected.");
	}
}



/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;

	char tmp_val[80];

	char out_val[160];


	/* Start at current panel */
	y1 = p_ptr->wy;
	x1 = p_ptr->wx;

	/* Immediately center on player */
	y2 = p_ptr->py - SCREEN_HGT / 2;
	x2 = p_ptr->px - SCREEN_WID / 2;

	/* Verify panel */
	if (y2 < 0)
		y2 = 0;
	if (x2 < 0)
		x2 = 0;
	if (y2 > DUNGEON_HGT - SCREEN_HGT)
		y2 = DUNGEON_HGT - SCREEN_HGT;
	if (x2 > DUNGEON_WID - SCREEN_WID)
		x2 = DUNGEON_WID - SCREEN_WID;

	/* Show new panel */
	p_ptr->wy = y2;
	p_ptr->wx = x2;

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();

	/* Show panels until done */
	while (1)
	{
		/* Describe the location */
		if ((y2 == y1) && (x2 == x1))
		{
			tmp_val[0] = '\0';
		}
		else
		{
			sprintf(tmp_val, "%s%s of",
				((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
				((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		sprintf(out_val,
			"Map sector [%d,%d], which is%s your sector.  Direction?",
			(y2 / PANEL_HGT), (x2 / PANEL_WID), tmp_val);

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command))
				break;

			/* Extract direction */
			dir = target_dir(command);

			/* Error */
			if (!dir)
				bell();
		}

		/* No direction */
		if (!dir)
			break;

		/* Apply the motion */
		y2 += (ddy[dir] * PANEL_HGT);
		x2 += (ddx[dir] * PANEL_WID);

		/* Verify the row */
		if (y2 < 0)
			y2 = 0;
		if (y2 > DUNGEON_HGT - SCREEN_HGT)
			y2 = DUNGEON_HGT - SCREEN_HGT;

		/* Verify the col */
		if (x2 < 0)
			x2 = 0;
		if (x2 > DUNGEON_WID - SCREEN_WID)
			x2 = DUNGEON_WID - SCREEN_WID;

		/* Handle "changes" */
		if ((p_ptr->wy != y2) || (p_ptr->wx != x2))
		{
			/* Update panel */
			p_ptr->wy = y2;
			p_ptr->wx = x2;

			/* Update stuff */
			p_ptr->update |= (PU_MONSTERS);

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Handle stuff */
			handle_stuff();
		}
	}

	/* Recenter the map around the player */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Handle stuff */
	handle_stuff();
}






/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info[] = {
	" :A dark grid",
	"!:A potion (or oil)",
	"\":An amulet (or necklace)",
	"#:A wall, secret door, or terrain",
	"$:Treasure (gold or gems)",
	"%:A vein (magma or quartz)",
	/* "&:unused", */
	"':An open door",
	"(:Soft armor",
	"):A shield",
	"*:A vein with treasure or an inorganic monster",
	"+:A closed door",
	",:Food (or mushroom patch)",
	"-:A wand (or rod)",
	".:Floor",
	"/:A polearm (Axe/Pike/etc)",
	/* "0:unused", */
	"1:Entrance to General Store",
	"2:Entrance to Armory",
	"3:Entrance to Weaponsmith",
	"4:Entrance to Temple",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Black Market",
	"8:Entrance to your home",
	/* "9:unused", */
	"::Rubble",
	";:A glyph of warding",
	"<:An up staircase",
	"=:A ring",
	">:A down staircase",
	"?:A scroll",
	"@:You",
	"A:Angel",
	"B:Bird",
	"C:Canine",
	"D:Ancient Dragon/Wyrm",
	"E:Elemental",
	"F:Dragon Fly",
	"G:Ghost",
	"H:Hybrid",
	"I:Insect",
	"J:Snake",
	"K:Killer Beetle",
	"L:Lich",
	"M:Multi-Headed Reptile",
	"N:Demigod",
	"O:Ogre",
	"P:Giant Humanoid",
	"Q:Quylthulg or Flesh Mound",
	"R:Reptile/Amphibian",
	"S:Spider/Scorpion/Tick",
	"T:Troll",
	"U:Major Demon",
	"V:Vampire",
	"W:Wight/Wraith/etc",
	"X:Xorn/Xaren/etc",
	"Y:Yeti",
	"Z:Zephyr Hound",
	"[:Hard armor",
	"\\:A hafted weapon (mace/whip/etc)",
	"]:Misc. armor",
	"^:A trap",
	"_:A staff",
	/* "`:unused", */
	"a:Ant",
	"b:Bat",
	"c:Centipede",
	"d:Dragon",
	"e:Floating Eye",
	"f:Feline",
	"g:Golem",
	"h:Hobbit/Elf/Dwarf",
	"i:Icky Thing",
	"j:Jelly",
	"k:Kobold",
	"l:Louse",
	"m:Mold",
	"n:Naga",
	"o:Orc",
	"p:Person/Human",
	"q:Quadruped",
	"r:Rodent",
	"s:Skeleton",
	"t:Townsperson",
	"u:Minor Demon",
	"v:Vortex",
	"w:Worm/Worm-Mass",
	"x:Fish or Mollusk",
	"y:Yeek",
	"z:Zombie/Mummy",
	"{:A missile (arrow/bolt/shot)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A launcher (bow/crossbow/sling)",
	"~:A tool (or miscellaneous item)",
	NULL
};



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b *) (u);

	u16b *why = (u16b *) (v);

	int w1 = who[a];
	int w2 = who[b];

	int z1, z2;


	/* Sort by player kills */
	if (*why >= 4)
	{
		/* Extract player kills */
		z1 = r_info[w1].r_pkills;
		z2 = r_info[w2].r_pkills;

		/* Compare player kills */
		if (z1 < z2)
			return (TRUE);
		if (z1 > z2)
			return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = r_info[w1].r_tkills;
		z2 = r_info[w2].r_tkills;

		/* Compare total kills */
		if (z1 < z2)
			return (TRUE);
		if (z1 > z2)
			return (FALSE);
	}


	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[w1].level;
		z2 = r_info[w2].level;

		/* Compare levels */
		if (z1 < z2)
			return (TRUE);
		if (z1 > z2)
			return (FALSE);
	}


	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[w1].mexp;
		z2 = r_info[w2].mexp;

		/* Compare experience */
		if (z1 < z2)
			return (TRUE);
		if (z1 > z2)
			return (FALSE);
	}


	/* Compare indexes */
	return (w1 <= w2);
}


/*
 * Sorting hook -- Swap function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform.
 */
static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b *) (u);

	u16b holder;

	/* XXX XXX */
	v = v ? v : 0;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
static void roff_top(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	byte a1, a2;
	char c1, c2;


	/* Access the chars */
	c1 = r_ptr->d_char;
	c2 = r_ptr->x_char;

	/* Access the attrs */
	a1 = r_ptr->d_attr;
	a2 = r_ptr->x_attr;


	/* Clear the top line */
	Term_erase(0, 0, 255);

	/* Reset the cursor */
	Term_gotoxy(0, 0);

	/* A title (use "The" for non-uniques) */
	if (!(r_ptr->flags1 & (RF1_UNIQUE)))
	{
		Term_addstr(-1, TERM_WHITE, "The ");
	}

	/* Dump the name */
	Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

	/* Append the "standard" attr/char info */
	Term_addstr(-1, TERM_WHITE, " ('");
	Term_addch(a1, c1);
	Term_addstr(-1, TERM_WHITE, "')");

	/* Append the "optional" attr/char info */
	Term_addstr(-1, TERM_WHITE, "/('");
	Term_addch(a2, c2);
	Term_addstr(-1, TERM_WHITE, "'):");
}


/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "mulitple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored. XXX XXX XXX
 */
void do_cmd_query_symbol(void)
{
	int i, n, r_idx;
	char sym, query;
	char buf[128];

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;

	bool recall = FALSE;

	u16b why = 0;
	u16b who[MAX_R_IDX];


	/* Get a character, or abort */
	if (!get_com("Enter character to be identified: ", &sym))
		return;

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0])
			break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		all = TRUE;
		strcpy(buf, "Full monster list.");
	}
	else if (sym == KTRL('U'))
	{
		all = uniq = TRUE;
		strcpy(buf, "Unique monster list.");
	}
	else if (sym == KTRL('N'))
	{
		all = norm = TRUE;
		strcpy(buf, "Non-unique monster list.");
	}
	else if (ident_info[i])
	{
		sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 0, 0);


	/* Collect matching monsters */
	for (n = 0, i = 1; i < MAX_R_IDX - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights)
			continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE)))
			continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE)))
			continue;

		/* Collect "appropriate" monsters */
		if (all || (r_ptr->d_char == sym))
			who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
		return;


	/* Prompt XXX XXX XXX */
	put_str("Recall details? (k/p/y/n): ", 0, 40);

	/* Query */
	query = inkey();

	/* Restore */
	prt(buf, 0, 0);


	/* Sort by kills (and level) */
	if (query == 'k')
	{
		why = 4;
		query = 'y';
	}

	/* Sort by level */
	if (query == 'p')
	{
		why = 2;
		query = 'y';
	}

	/* Catch "escape" */
	if (query != 'y')
		return;


	/* Sort if needed */
	if (why)
	{
		/* Select the sort method */
		ang_sort_comp = ang_sort_comp_hook;
		ang_sort_swap = ang_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, n);
	}


	/* Start at the end */
	i = n - 1;

	/* Scan the monster memory */
	while (1)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Hack -- Auto-recall */
		monster_race_track(r_idx);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_top(r_idx);

		/* Hack -- Complete the prompt */
		Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save the screen */
				screen_save();

				/* Recall on screen */
				screen_roff(who[i], 0);

				/* Hack -- Complete the prompt (again) */
				Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC]");
			}

			/* Command */
			query = inkey();

			/* Unrecall */
			if (recall)
			{
				/* Restore */
				screen_load();
			}

			/* Normal commands */
			if (query != 'r')
				break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query == ESCAPE)
			break;

		/* Move to "prev" monster */
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list)
					break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list)
					break;
			}
		}
	}


	/* Re-display the identity */
	prt(buf, 0, 0);
}


/*
*  research_mon
*  -KMW-
*/
bool research_mon()
{
	int i, n, r_idx;
	char sym, query;
	char buf[128];

	s16b oldkills;
	byte oldwake;
	bool oldcheat;

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;
	bool notpicked;

	bool recall = FALSE;

	u16b why = 0;
	u16b who[MAX_R_IDX];

	monster_race *r2_ptr;

	oldcheat = cheat_know;

	/* Get a character, or abort */
	if (!get_com("Enter character of monster: ", &sym))
		return (TRUE);

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0])
			break;
	}

	if (ident_info[i])
	{
		sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		sprintf(buf, "%c - %s.", sym, "Unknown Symbol");
	}

	/* Display the result */
	prt(buf, 16, 10);


	/* Collect matching monsters */
	for (n = 0, i = 1; i < MAX_R_IDX - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		cheat_know = TRUE;
		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights)
			continue;

		/* Require non-unique monsters if needed */
		if (norm && (r_ptr->flags1 & (RF1_UNIQUE)))
			continue;

		/* Require unique monsters if needed */
		if (uniq && !(r_ptr->flags1 & (RF1_UNIQUE)))
			continue;

		/* Collect "appropriate" monsters */
		if (all || (r_ptr->d_char == sym))
			who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
		return (TRUE);


	/* Sort by level */
	why = 2;
	query = 'y';

	/* Sort if needed */
	if (why)
	{
		/* Select the sort method */
		ang_sort_comp = ang_sort_comp_hook;
		ang_sort_swap = ang_sort_swap_hook;

		/* Sort the array */
		ang_sort(who, &why, n);
	}


	/* Start at the end */
	i = n - 1;

	notpicked = TRUE;

	/* Scan the monster memory */
	while (notpicked)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Hack -- Auto-recall */
		monster_race_track(r_idx);

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_top(r_idx);

		/* Hack -- Complete the prompt */
		Term_addstr(-1, TERM_WHITE, " [(r)ecall, ESC, space to continue]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save the screen */
				screen_save();

				/* Recall on screen */
				r2_ptr = &r_info[r_idx];

				oldkills = r2_ptr->r_tkills;
				oldwake = r2_ptr->r_wake;
				screen_roff(who[i], 1);
				r2_ptr->r_tkills = oldkills;
				r2_ptr->r_wake = oldwake;
				r2_ptr->r_sights = 1;
				cheat_know = oldcheat;
				notpicked = FALSE;
				break;

			}

			/* Command */
			query = inkey();

			/* Unrecall */
			if (recall)
			{
				/* Restore */
				screen_load();
			}

			/* Normal commands */
			if (query != 'r')
				break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query == ESCAPE)
			break;

		/* Move to "prev" monster */
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list)
					break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list)
					break;
			}
		}
	}


	/* Re-display the identity */
	/* prt(buf, 5, 5); */

	cheat_know = oldcheat;
	return (notpicked);
}
