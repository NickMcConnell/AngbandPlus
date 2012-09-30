/* File: cmd3.c */

/* Purpose: Inventory commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"



/*
 * Display inventory
 */
void do_cmd_inven(void)
{
	/* Save screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the inventory */
	show_list(p_ptr->inventory, FALSE);

	/* Hack -- hide empty slots */
	item_tester_full = FALSE;

	/* Get a command */
	prtf(0, 0, "Inventory: carrying %d.%d pounds (%d%% of capacity). Command: ",
			p_ptr->total_weight / 10, p_ptr->total_weight % 10,
			(p_ptr->total_weight * 100) /
			((adj_str_wgt[p_ptr->stat[A_STR].ind] * 100) / 2));

	/* Get a new command */
	p_ptr->cmd.new = inkey();

	/* Load screen */
	screen_load();

	/* Process "Escape" */
	if (p_ptr->cmd.new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->cmd.new = 0;
	}
}


/*
 * Display equipment
 */
void do_cmd_equip(void)
{
	/* Save the screen */
	screen_save();

	/* Hack -- show empty slots */
	item_tester_full = TRUE;

	/* Display the equipment */
	show_equip(FALSE);

	/* Hack -- undo the hack above */
	item_tester_full = FALSE;

	/* Get a command */
	prtf(0, 0, "Equipment: carrying %d.%d pounds (%d%% of capacity). Command: ",
			p_ptr->total_weight / 10, p_ptr->total_weight % 10,
			(p_ptr->total_weight * 100) /
			((adj_str_wgt[p_ptr->stat[A_STR].ind] * 100) / 2));

	/* Get a new command */
	p_ptr->cmd.new = inkey();

	/* Restore the screen */
	screen_load();

	/* Process "Escape" */
	if (p_ptr->cmd.new == ESCAPE)
	{
		/* Reset stuff */
		p_ptr->cmd.new = 0;
	}
}


/*
 * Wield or wear a single item from the pack or floor
 */
void do_cmd_wield(void)
{
	int slot;

	object_type *q_ptr;

	object_type *o_ptr;

	cptr act;

	cptr q, s;

	/* Restrict the choices */
	item_tester_hook = item_tester_hook_wear;

	/* Get an item */
	q = "Wear/Wield which item? ";
	s = "You have nothing you can wear or wield.";

	q_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!q_ptr) return;

	/* Check the slot */
	slot = wield_slot(q_ptr);

	/* Hack - rings are special */
	if (slot == EQUIP_LEFT)
	{
		/* Check to see if have two rings */
		if ((p_ptr->equipment[EQUIP_LEFT].k_idx) &&
			(p_ptr->equipment[EQUIP_RIGHT].k_idx))
		{
			if (get_check("Use right hand? ")) slot = EQUIP_RIGHT;
		}
	}

	/* Access the wield slot */
	o_ptr = &p_ptr->equipment[slot];

	/* Prevent wielding into a cursed slot */
	if (cursed_p(o_ptr))
	{
		/* Message */
		msgf("The %v you are %s appears to be cursed.",
			 OBJECT_FMT(o_ptr, FALSE, 0), describe_use(slot));

		/* Set the knowledge flag for the player */
		o_ptr->kn_flags[2] |= TR2_CURSED;

		/* Cancel the command */
		return;
	}

	if (cursed_p(q_ptr) && confirm_wear &&
		(object_known_p(q_ptr) || (q_ptr->info & OB_SENSE)))
	{
		if (!get_check("Really use the %v {cursed}? ",
			 OBJECT_FMT(q_ptr, FALSE, 0)))
			return;
	}

	/* Take a turn */
	p_ptr->state.energy_use = 100;

	/* Split object */
	q_ptr = item_split(q_ptr, 1);

	/* Take off existing item */
	if (o_ptr->k_idx)
	{
		/* Take off existing item */
		(void)inven_takeoff(o_ptr);
	}

	/* Wear the new stuff */
	swap_objects(o_ptr, q_ptr);

	/* Forget stack */
	o_ptr->next_o_idx = 0;

	/* Forget location */
	o_ptr->iy = o_ptr->ix = 0;

	/* Forget Region */
	o_ptr->region = 0;

	/* Now no longer allocated in o_list[] */
	o_ptr->allocated = FALSE;

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

	/* Message */
	msgf("%s %v (%c).", act, OBJECT_FMT(o_ptr, TRUE, 3), I2A(slot));

	/* Cursed! */
	if (cursed_p(o_ptr))
	{
		/* Warn the player */
		msgf("Oops! It feels deathly cold!");

		chg_virtue(V_HARMONY, -1);

		/* Note the curse */
		o_ptr->info |= (OB_SENSE);
	}

	/* Learn some "obvious" things about the item */
	o_ptr->kn_flags[0] |= (o_ptr->flags[0] & TR0_EASY_MASK);

	/* Recalculate bonuses and weight */
	p_ptr->update |= (PU_BONUS | PU_WEIGHT);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	p_ptr->redraw |= (PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	
	/* Notice changes */
	notice_item();

	make_noise(1);
}



/*
 * Take off an item
 */
void do_cmd_takeoff(void)
{
	object_type *o_ptr;

	cptr q, s;

	/* Get an item */
	q = "Take off which item? ";
	s = "You are not wearing anything to take off.";

	o_ptr = get_item(q, s, (USE_EQUIP));

	/* Not a valid item */
	if (!o_ptr) return;


	/* Item is cursed */
	if (cursed_p(o_ptr))
	{
		/* Oops */
		msgf("Hmmm, it seems to be cursed.");

		/* Set the knowledge flag for the player */
		o_ptr->kn_flags[2] |= TR2_CURSED;

		/* Nope */
		return;
	}

	/* Take a partial turn */
	p_ptr->state.energy_use = 50;

	/* Take off the item */
	(void)inven_takeoff(o_ptr);

	make_noise(1);
}


/*
 * Drop an item
 */
void do_cmd_drop(void)
{
	int amt = 1;

	object_type *o_ptr;

	cptr q, s;

	/* Get an item */
	q = "Drop which item? ";
	s = "You have nothing to drop.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Hack -- Cannot remove cursed items */
	if ((!o_ptr->allocated) && cursed_p(o_ptr))
	{
		/* Oops */
		msgf("Hmmm, it seems to be cursed.");

		/* Set the knowledge flag for the player */
		o_ptr->kn_flags[2] |= TR2_CURSED;

		/* Nope */
		return;
	}


	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Take a partial turn */
	p_ptr->state.energy_use = 50;

	/* Drop (some of) the item */
	inven_drop(o_ptr, amt);

	p_ptr->redraw |= (PR_EQUIPPY);

	make_noise(1);
}


static bool high_level_book(const object_type *o_ptr)
{
	if ((o_ptr->tval == TV_LIFE_BOOK) ||
		(o_ptr->tval == TV_SORCERY_BOOK) ||
		(o_ptr->tval == TV_NATURE_BOOK) ||
		(o_ptr->tval == TV_CHAOS_BOOK) ||
		(o_ptr->tval == TV_DEATH_BOOK) || (o_ptr->tval == TV_TRUMP_BOOK))
	{
		if (o_ptr->sval > 1)
			return TRUE;
		else
			return FALSE;
	}

	return FALSE;
}


bool destroy_item_aux(object_type *o_ptr, int amt)
{
	bool gain_expr = FALSE;

	/* Can the player destroy the object? */
	if (!can_player_destroy_object(o_ptr))
	{
		/* Message */
		msgf("You cannot destroy %v.", OBJECT_FMT(o_ptr, TRUE, 3));

		/* Done */
		return (FALSE);
	}

	/* Take a turn */
	p_ptr->state.energy_use += 100;

	/* Message */
	msgf("You destroy %v.", OBJECT_FMT(o_ptr, TRUE, 3));
	sound(SOUND_DESTITEM);

	if (high_level_book(o_ptr))
	{
		if (p_ptr->rp.pclass == CLASS_WARRIOR)
		{
			gain_expr = TRUE;
		}
		else if (p_ptr->rp.pclass == CLASS_PALADIN)
		{
			if (p_ptr->spell.r[0].realm == REALM_LIFE)
			{
				if (o_ptr->tval != TV_LIFE_BOOK) gain_expr = TRUE;
			}
			else
			{
				if (o_ptr->tval == TV_LIFE_BOOK) gain_expr = TRUE;
			}
		}

		if (gain_expr && (p_ptr->exp < PY_MAX_EXP))
		{
			s32b tester_exp = p_ptr->max_exp / 20;
			if (tester_exp > 10000) tester_exp = 10000;
			if (o_ptr->sval < 3) tester_exp /= 4;
			if (tester_exp < 1) tester_exp = 1;

			msgf("You feel more experienced.");
			gain_exp(tester_exp * amt);
		}

		if (high_level_book(o_ptr) && o_ptr->tval == TV_LIFE_BOOK)
		{
			chg_virtue(V_UNLIFE, 1);
			chg_virtue(V_VITALITY, -1);
		}
		else if (high_level_book(o_ptr) && o_ptr->tval == TV_DEATH_BOOK)
		{
			chg_virtue(V_UNLIFE, -1);
			chg_virtue(V_VITALITY, 1);
		}

		if (o_ptr->to_a || o_ptr->to_h || o_ptr->to_d)
			chg_virtue(V_ENCHANT, -1);

		if (object_value_real(o_ptr) > 30000)
			chg_virtue(V_SACRIFICE, 2);

		else if (object_value_real(o_ptr) > 10000)
			chg_virtue(V_SACRIFICE, 1);
	}

	if (o_ptr->to_a != 0 || o_ptr->to_d != 0 || o_ptr->to_h != 0)
		chg_virtue(V_HARMONY, 1);

	make_noise(1);

	/* We destroyed the item(s) */
	return (TRUE);
}


/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
	int amt = 1;
	int old_number;

	bool force = FALSE;

	object_type *o_ptr;

	cptr q, s;

	/* Hack -- force destruction */
	if (p_ptr->cmd.arg > 0) force = TRUE;


	/* Get an item */
	q = "Destroy which item? ";
	s = "You have nothing to destroy.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* See how many items */
	if (o_ptr->number > 1)
	{
		/* Get a quantity */
		amt = get_quantity(NULL, o_ptr->number);

		/* Allow user abort */
		if (amt <= 0) return;
	}


	/* Describe the objects to delete */
	old_number = o_ptr->number;
	o_ptr->number = amt;
	

	/* Verify unless quantity given */
	if (!force)
	{
		if (!(auto_destroy && (object_value(o_ptr) < 1)))
		{
			/* Make a verification */
			if (!get_check("Really destroy %v? ", OBJECT_FMT(o_ptr, TRUE, 3)))
			{
				o_ptr->number = old_number;
				return;
			}
		}
	}
	
	o_ptr->number = old_number;

	/* No energy used yet */
	p_ptr->state.energy_use = 0;

	/* Physically try to destroy the item(s) */
	if (!destroy_item_aux(o_ptr, amt)) return;

	/* Reduce the charges of rods/wands */
	reduce_charges(o_ptr, amt);

	/* Eliminate the item */
	item_increase(o_ptr, -amt);
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
	object_type *o_ptr;

	cptr q, s;

	/* Get an item */
	q = "Examine which item? ";
	s = "You have nothing to examine.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Describe it fully */
	identify_fully_aux(o_ptr);
}

static bool item_tester_inscribed(const object_type *o_ptr)
{
	/* Nothing to remove */
	if (!o_ptr->inscription) return (FALSE);

	return (TRUE);
}

/*
 * Remove the inscription from an object
 * XXX Mention item (when done)?
 */
void do_cmd_uninscribe(void)
{
	object_type *o_ptr;

	cptr q, s;
	
	/* Only inscribed items */
	item_tester_hook = item_tester_inscribed;

	/* Get an item */
	q = "Un-inscribe which item? ";
	s = "You have nothing to un-inscribe.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Message */
	msgf("Inscription removed.");

    /* Remove the incription */
    quark_remove(&o_ptr->inscription);
	
	/* Notice changes */
	notice_item();

	make_noise(2);
}


/*
 * Inscribe an object with a comment
 */
void do_cmd_inscribe(void)
{
	object_type *o_ptr;

	char out_val[80];

	cptr q, s;

	/* Get an item */
	q = "Inscribe which item? ";
	s = "You have nothing to inscribe.";

	o_ptr = get_item(q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Message */
	msgf("Inscribing %v.", OBJECT_FMT(o_ptr, TRUE, 3));
	message_flush();

	/* Start with nothing */
	out_val[0] = 0;

	/* Use old inscription */
	if (o_ptr->inscription)
	{
		/* Start with the old inscription */
		strcpy(out_val, quark_str(o_ptr->inscription));
	}

	/* Get a new inscription (possibly empty) */
	if (get_string(out_val, 80, "Inscription: "))
	{
        /* Save the inscription */
        quark_remove(&o_ptr->inscription);
		o_ptr->inscription = quark_add(out_val);

		/* Notice changes */
		notice_item();

		make_noise(2);
	}
}



/*
 * An "item_tester_hook" for refilling lanterns
 */
static bool item_tester_refill_lantern(const object_type *o_ptr)
{
	/* Flasks of oil are okay */
	if (o_ptr->tval == TV_FLASK) return (TRUE);

	/* Laterns are okay */
	if ((o_ptr->tval == TV_LITE) &&
		(o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->timeout > 0)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(void)
{
	object_type *o_ptr;
	object_type *j_ptr;

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_refill_lantern;

	/* Get an item */
	q = "Refill with which source of oil? ";
	s = "You have no sources of oil.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;


	/* Take a partial turn */
	p_ptr->state.energy_use = 50;

	/* Access the lantern */
	j_ptr = &p_ptr->equipment[EQUIP_LITE];

	/* Refuel */
	if (o_ptr->tval == TV_FLASK)
	{
		/* Flasks use the pval to store the fuel */
		j_ptr->timeout += o_ptr->pval;
	}
	else
	{
		/* Lanterns use the timeout to store the fuel */
		j_ptr->timeout += o_ptr->timeout;
	}

	/* Message */
	msgf("You fuel your lamp.");

	/* Comment */
	if (j_ptr->timeout >= FUEL_LAMP)
	{
		j_ptr->timeout = FUEL_LAMP;
		msgf("Your lamp is full.");
	}

	/* Decrease the item */
	if (o_ptr->tval == TV_FLASK)
	{
		item_increase(o_ptr, -1);
	}
	else
	{
		/* The lantern is empty */
		o_ptr->timeout = 0;
	}

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
	
	/* Notice changes */
	notice_item();
}


/*
 * An "item_tester_hook" for refilling torches
 */
static bool item_tester_refill_torch(const object_type *o_ptr)
{
	/* Torches are okay */
	if ((o_ptr->tval == TV_LITE) &&
		(o_ptr->sval == SV_LITE_TORCH)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(void)
{
	object_type *o_ptr;
	object_type *j_ptr;

	cptr q, s;


	/* Restrict the choices */
	item_tester_hook = item_tester_refill_torch;

	/* Get an item */
	q = "Refuel with which torch? ";
	s = "You have no extra torches.";

	o_ptr = get_item(q, s, (USE_INVEN | USE_FLOOR));

	/* Not a valid item */
	if (!o_ptr) return;

	/* Take a partial turn */
	p_ptr->state.energy_use = 50;

	/* Access the primary torch */
	j_ptr = &p_ptr->equipment[EQUIP_LITE];

	/* Refuel */
	j_ptr->timeout += o_ptr->timeout + 5;

	/* Message */
	msgf("You combine the torches.");

	/* Over-fuel message */
	if (j_ptr->timeout >= FUEL_TORCH)
	{
		j_ptr->timeout = FUEL_TORCH;
		msgf("Your torch is fully fueled.");
	}

	/* Refuel message */
	else
	{
		msgf("Your torch glows more brightly.");
	}

	/* Decrease the item */
	item_increase(o_ptr, -1);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
}


/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
	object_type *o_ptr;

	/* Get the light */
	o_ptr = &p_ptr->equipment[EQUIP_LITE];

	/* It is nothing */
	if (o_ptr->tval != TV_LITE)
	{
		msgf("You are not wielding a light.");
	}

	/* It's a lamp */
	else if (o_ptr->sval == SV_LITE_LANTERN)
	{
		do_cmd_refill_lamp();
	}

	/* It's a torch */
	else if (o_ptr->sval == SV_LITE_TORCH)
	{
		do_cmd_refill_torch();
	}

	/* No torch to refill */
	else
	{
		msgf("Your light cannot be refilled.");
	}
}


/*
 * Target command
 */
void do_cmd_target(void)
{
	/* Target set */
	if (target_set(TARGET_KILL))
	{
		msgf("Target Selected.");
	}

	/* Target aborted */
	else
	{
		msgf("Target Aborted.");
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
		msgf("Target Selected.");
	}
}



/*
 * Allow the player to examine other sectors on the map
 */
void do_cmd_locate(void)
{
	int dir, y1, x1, y2, x2;
	int wid, hgt;
	char tmp_val[80];
	char out_val[160];

	/* Get size */
	get_map_size(&wid, &hgt);

	/* Start at current panel */
	x2 = x1 = p_ptr->panel_x1;
	y2 = y1 = p_ptr->panel_y1;

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
			strnfmt(tmp_val, 80, "%s%s of",
					((y2 < y1) ? " North" : (y2 > y1) ? " South" : ""),
					((x2 < x1) ? " West" : (x2 > x1) ? " East" : ""));
		}

		/* Prepare to ask which way to look */
		strnfmt(out_val, 160,
				"Map sector [%d(%02d),%d(%02d)], which is%s your sector.  Direction?",
				y2 / (hgt / 2), y2 % (hgt / 2),
				x2 / (wid / 2), x2 % (wid / 2), tmp_val);

		/* Assume no direction */
		dir = 0;

		/* Get a direction */
		while (!dir)
		{
			char command;

			/* Get a command (or Cancel) */
			if (!get_com(out_val, &command)) break;

			/* Extract the action (if any) */
			dir = get_keymap_dir(command);

			/* Error */
			if (!dir) bell("Illegal direction for locate!");
		}

		/* No direction */
		if (!dir) break;

		/* Apply the motion */
		if (change_panel(ddx[dir], ddy[dir]))
		{
			x2 = p_ptr->panel_x1;
			y2 = p_ptr->panel_y1;
		}
	}

	/* Recenter the map around the player */
	verify_panel();
}



/*
 * The table of "symbol info" -- each entry is a string of the form
 * "X:desc" where "X" is the trigger, and "desc" is the "info".
 */
static cptr ident_info[] =
{
	" :A dark grid",
	"!:A potion (or oil)",
	"\":An amulet (or necklace)",
	"#:A wall (or secret door)",
	"$:Treasure (gold or gems)",
	"%:Trees",
	"&:A chest",
	"':An open door",
	"(:Soft armor",
	"):A shield",
	"*:A treasure or a ball monster",
	"+:A closed door",
	",:Food (or mushroom patch)",
	"-:A wand (or rod)",
	".:Floor",
	"/:A polearm (Axe/Pike/etc)",
	/* "0:unused", XXX XXX XXX out of date */
	"1:Entrance to General Store",
	"2:Entrance to Armory",
	"3:Entrance to Weaponsmith",
	"4:Entrance to Temple",
	"5:Entrance to Alchemy shop",
	"6:Entrance to Magic store",
	"7:Entrance to Black Market",
	"8:Entrance to your home",
	"9:Entrance to the bookstore",
	"::Rubble / Rock",
	";:Swamp / Rune",
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
	/* "N:unused", */
	"O:Ogre",
	"P:Giant Humanoid",
	"Q:Quylthulg (Pulsing Flesh Mound)",
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
	"`:A figurine or statue",
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
	"l:Aquatic monster",
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
	/* "x:unused", */
	"y:Yeek",
	"z:Zombie/Mummy",
	"{:A missile (arrow/bolt/shot)",
	"|:An edged weapon (sword/dagger/etc)",
	"}:A launcher (bow/crossbow/sling)",
	"~:Fluid terrain (or miscellaneous item)",
	NULL
};



/*
 * Sorting hook -- Comp function -- see below
 *
 * We use "u" to point to array of monster indexes,
 * and "v" to select the type of sorting to perform on "u".
 */
bool ang_sort_comp_hook(const vptr u, const vptr v, int a, int b)
{
	u16b *who = (u16b *)(u);

	u16b *why = (u16b *)(v);

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
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by total kills */
	if (*why >= 3)
	{
		/* Extract total kills */
		z1 = r_info[w1].r_tkills;
		z2 = r_info[w2].r_tkills;

		/* Compare total kills */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by monster level */
	if (*why >= 2)
	{
		/* Extract levels */
		z1 = r_info[w1].level;
		z2 = r_info[w2].level;

		/* Compare levels */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
	}


	/* Sort by monster experience */
	if (*why >= 1)
	{
		/* Extract experience */
		z1 = r_info[w1].mexp;
		z2 = r_info[w2].mexp;

		/* Compare experience */
		if (z1 < z2) return (TRUE);
		if (z1 > z2) return (FALSE);
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
void ang_sort_swap_hook(const vptr u, const vptr v, int a, int b)
{
	u16b *who = (u16b *)(u);

	u16b holder;

	/* Hack - ignore v */
	(void)v;

	/* Swap */
	holder = who[a];
	who[a] = who[b];
	who[b] = holder;
}


static int resize_monster;
void (*resize_old_hook) (void);

static void resize_monster_recall(void)
{
	/* Put the monster description on the newly-sized screen.*/
	screen_roff_mon(resize_monster, 0);
}

/*
 * Identify a character, allow recall of monsters
 *
 * Several "special" responses recall "multiple" monsters:
 *   ^A (all monsters)
 *   ^U (all unique monsters)
 *   ^N (all non-unique monsters)
 *   ^M (case insensitive name search)
 *
 * The responses may be sorted in several ways, see below.
 *
 * Note that the player ghosts are ignored. XXX XXX XXX
 */
void do_cmd_query_symbol(void)
{
	int i, n, r_idx;
	char sym, query;

	bool all = FALSE;
	bool uniq = FALSE;
	bool norm = FALSE;
	bool killed = FALSE;

	bool recall = FALSE;

	u16b why = 0;
	u16b *who;

	char temp1[80] = "\0";
	char temp2[80] = "\0";

	/* Get a character, or abort */
	if (!get_com
		("Enter character to be identified, or Ctrl (A, U, N, M, K):",
		 &sym)) return;

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0]) break;
	}

	/* Describe */
	if (sym == KTRL('A'))
	{
		all = TRUE;
		prtf(0, 0, "Full monster list.");
	}
	else if (sym == KTRL('U'))
	{
		all = uniq = TRUE;
		prtf(0, 0, "Unique monster list.");
	}
	else if (sym == KTRL('N'))
	{
		all = norm = TRUE;
		prtf(0, 0, "Non-unique monster list.");
	}
	else if (sym == KTRL('M'))
	{
		all = TRUE;
		if (!get_string(temp1, 70, "Name:"))
		{
			clear_msg();
		}
		else
		{
			prtf(0, 0, "Monsters with a name \"%s\"", temp1);
		}
	}
	else if (sym == KTRL('K'))
	{
		all = killed = TRUE;
		prtf(0, 0, "Killed monster list.");
	}

	else if (ident_info[i])
	{
		if (ident_info[i][0] == '$')
		{
			/*
			 * Hack - we need two dollar signs since it
			 * is an escape code.
			 */
			prtf(0, 0, "$$ - %s.", ident_info[i] + 2);
		}
		else
		{
			prtf(0, 0, "%c - %s.", sym, ident_info[i] + 2);
		}
	}
	else
	{
		prtf(0, 0, "%c - %s.", sym, "Unknown Symbol");
	}

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Nothing to recall */
		if (!cheat_know && !r_ptr->r_sights) continue;

		/* Require non-unique monsters if needed */
		if (norm && FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Require unique monsters if needed */
		if (uniq && !FLAG(r_ptr, RF_UNIQUE)) continue;

		/* Require killed monsters if needed */
		if (killed && !r_ptr->r_pkills) continue;

		/* Collect monsters with a name temp1 */
		if (temp1[0])
		{
			int xx;

			for (xx = 0; temp1[xx] && (xx < 80); xx++)
			{
				if (isupper(temp1[xx])) temp1[xx] = tolower(temp1[xx]);
			}
			strcpy(temp2, mon_race_name(r_ptr));

			for (xx = 0; temp2[xx] && (xx < 80); xx++)
			{
				if (isupper(temp2[xx])) temp2[xx] = tolower(temp2[xx]);
			}

			if (strstr(temp2, temp1)) who[n++] = i;
		}

		/* Collect "appropriate" monsters */
		else if (all || (r_ptr->d_char == sym)) who[n++] = i;
	}

	/* Nothing to recall */
	if (!n)
	{
		/* XXX XXX Free the "who" array */
		KILL(who);

		return;
	}

	/* Prompt XXX XXX XXX */
	put_fstr(40, 0, "Recall details? (k/y/n): ");

	/* Query */
	query = inkey();
	
	/* Clear top line */
	clear_msg();

	why = 2;

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array */
	ang_sort(who, &why, n);

	/* Sort by kills (and level) */
	if (query == 'k')
	{
		why = 4;
		query = 'y';
	}

	/* Catch "escape" */
	if (query != 'y')
	{
		/* XXX XXX Free the "who" array */
		KILL(who);

		return;
	}


	/* Sort if needed */
	if (why == 4)
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
		roff_mon_top(r_idx);

		/* Hack -- Complete the prompt */
		roff(" [(r)ecall, ESC]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Save the screen */
				screen_save();

				/* Recall on screen */
				screen_roff_mon(who[i], 0);

				/* Hack -- Complete the prompt (again) */
				roff(" [(r)ecall, ESC]");
			}

			/* Remember what the resize hook was */
			resize_old_hook = angband_term[0]->resize_hook;

			/* Hack - change the redraw hook so bigscreen works */
			angband_term[0]->resize_hook = resize_monster_recall;

			/* Remember the monster for resizing */
			resize_monster = who[i];

			/* Command */
			query = inkey();

			/* Hack - change the redraw hook so bigscreen works */
			angband_term[0]->resize_hook = resize_old_hook;

			/* The size may have changed during the monster recall */
			angband_term[0]->resize_hook();

			/* Hack - Flush it */
			Term_fresh();

			/* Unrecall */
			if (recall)
			{
				/* Restore */
				screen_load();
			}

			/* Normal commands */
			if (query != 'r') break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query == ESCAPE) break;

		/* Move to "prev" monster */
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list) break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list) break;
			}
		}
	}

	/* Free the "who" array */
	KILL(who);
	
	/* Clear top line */
	clear_msg();
}


/*
 *  research_mon
 *  -KMW-
 */
bool research_mon(void)
{
	int i, n, r_idx;
	char sym, query;

	s16b oldkills;
	byte oldwake;
	bool oldcheat;

	bool picked = FALSE;
	bool cost_gold = TRUE;

	bool recall = FALSE;

	u16b why = 0;

	monster_race *r2_ptr;

	u16b *who;

	oldcheat = cheat_know;


	/* Save the screen */
	screen_save();

	/* Get a character, or abort */
	if (!get_com("Enter character of monster: ", &sym))
	{
		/* Restore */
		screen_load();

		return (FALSE);
	}

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Find that character info, and describe it */
	for (i = 0; ident_info[i]; ++i)
	{
		if (sym == ident_info[i][0]) break;
	}

	if (ident_info[i])
	{
		prtf(10, 16, "%c - %s.", sym, ident_info[i] + 2);
	}
	else
	{
		prtf(10, 16, "%c - %s.", sym, "Unknown Symbol");
	}


	/* Collect matching monsters */
	for (n = 0, i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		cheat_know = TRUE;

		/* Collect "appropriate" monsters */
		if (r_ptr->d_char == sym) who[n++] = i;
	}

	/* Back to what it was */
	cheat_know = oldcheat;

	/* Nothing to recall */
	if (!n)
	{
		/* Free the "who" array */
		KILL(who);

		/* Restore */
		screen_load();

		return (FALSE);
	}

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

	/* Scan the monster memory */
	while (!picked)
	{
		/* Extract a race */
		r_idx = who[i];

		/* Save this monster ID */
		p_ptr->monster_race_idx = r_idx;

		/* Hack -- Handle stuff */
		handle_stuff();

		/* Hack -- Begin the prompt */
		roff_mon_top(r_idx);

		/* Hack -- Complete the prompt */
		roff(" [(r)ecall, ESC, space to continue]");

		/* Interact */
		while (1)
		{
			/* Recall */
			if (recall)
			{
				/* Recall on screen */
				r2_ptr = &r_info[r_idx];

				/* Have you researched this monster before? */
				if (r2_ptr->r_flags[6] & RF6_LIBRARY)
				{
					/* Looking up a monster the second time is for free */
					cost_gold = FALSE;
				}
				else
				{
					/* This monster has now been researched */
					r2_ptr->r_flags[6] |= RF6_LIBRARY;

					/* You've seen this monster now (in a book) */
					if (!r2_ptr->r_sights) r2_ptr->r_sights = 1;
				}

				oldkills = r2_ptr->r_tkills;
				oldwake = r2_ptr->r_wake;

				/* Show the monster */
				screen_roff_mon(who[i], 1);

				r2_ptr->r_tkills = oldkills;
				r2_ptr->r_wake = oldwake;

				picked = TRUE;

			}

			/* Command */
			query = inkey();

			/* Normal commands */
			if (query != 'r') break;

			/* Toggle recall */
			recall = !recall;
		}

		/* Stop scanning */
		if (query == ESCAPE) break;

		/* Move to "prev" monster */
		if (query == '-')
		{
			if (++i == n)
			{
				i = 0;
				if (!expand_list) break;
			}
		}

		/* Move to "next" monster */
		else
		{
			if (i-- == 0)
			{
				i = n - 1;
				if (!expand_list) break;
			}
		}
	}

	/* Free the "who" array */
	KILL(who);

	/* Restore */
	screen_load();

	return (picked && cost_gold);
}
