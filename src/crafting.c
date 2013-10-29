/*
 * File: cmd0.c
 * Purpose: Implement the crafting system.
 *
 * Copyright (c) 2009 Austin McDonald
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "ui-menu.h"
#include "object/object.h"
#include "object/tvalsval.h"

#define MAX_COMPONENTS 10

typedef void crafting_cmd_type(void);

typedef struct
{
	const char* desc;
	crafting_cmd_type* hook;
} crafting_command;

typedef struct
{
	int kind;
	int quantity;
} component_type;

/*
 * Right now, recipe items are specified using kinds.  We may want to generalize this later, which will be a huge hassle.
 */
typedef struct
{
	component_type target; /* The item made by this recipe */
	int num_components; /* length(components) */
	component_type* components; /* An array of the components needed */
	int* weights; /* How important each component is - used in determining what drops from a refine, and quality - multiplies the pval of the items used. */
} recipe_type;

/*
 * Would be really nice to move this to an edit file.
 *
 * Hack: weight of -1 => can't refine
 *
 * This is going to throw up a ton of warnings about compound literals.  It's okay.
 */
const static recipe_type recipes[] =
{																				/* <target>  	    <- {<component> (<weight>)}* */
	{{30,  1}, 2, (component_type[]){{565, 1}, {567, 1}}, (int[]){10, 1}},		/* 1 Dagger 		<- 1 iron, 1 leather */
	{{33,  1}, 2, (component_type[]){{565, 2}, {567, 1}}, (int[]){20, 1}},		/* 1 Short Sword 	<- 2 iron, 1 leather */
	{{37,  1}, 2, (component_type[]){{565, 3}, {567, 1}}, (int[]){30, 1}},		/* 1 Long Sword 	<- 3 iron, 1 leather */
	{{50,  1}, 2, (component_type[]){{567, 2}, {569, 1}}, (int[]){20, 1}},		/* 1 Whip		 	<- 2 leather, 1 ash   */
	{{90,  1}, 1, (component_type[]){{567, 2}		   }, (int[]){1}    },		/* 1 Sling		 	<- 2 leather          */
	{{91,  1}, 2, (component_type[]){{569, 2}, {567, 1}}, (int[]){20, 1}},		/* 1 Short Bow   	<- 2 ash, 1 leather   */
	{{100, 5}, 1, (component_type[]){{569, 1} 	       }, (int[]){-1}   },		/* 5 Arrows  	 	<- 1 ash			  */
	{{111, 5}, 1, (component_type[]){{565, 1}          }, (int[]){1}    },		/* 5 Iron Shot	 	<- 1 iron			  */
	{{131, 1}, 1, (component_type[]){{567, 2}          }, (int[]){1}    },		/* 1 P. Leath. Bts 	<- 2 leather		  */
	{{141, 1}, 2, (component_type[]){{565, 2}, {567, 1}}, (int[]){20, 1}},		/* 1 Metal Cap  	<- 2 iron, 1 leather */
};

/* Find the first recipe with this target.*/
const recipe_type * lookup_recipe_target(int kind)
{
	int i;
	for (i = 0; i < N_ELEMENTS(recipes); ++i)
	{
		if (recipes[i].target.kind == kind)
			return &recipes[i];
	}

	return NULL;
}

/*
 * Calculates a numerical quality value for the item.
 *
 * Cursed/low-quality items should have 0 or negative quality.
 *
 * Based heavily on artifact_power from randart.c
 *
 * TODO: Activates should count for *something*
 * TODO: Only calculate quality based on what the player knows.
 */
int get_item_quality(const object_type* o_ptr) {
	if (!object_known_p(o_ptr)) return 0;
	int p = 0;
	u32b f1, f2, f3;
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Evaluate certain abilities based on type of object. */
	switch (o_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			p += o_ptr->to_d / 2;
			switch (o_ptr->sval)
			{
				case SV_SLING:
				case SV_SHORT_BOW:
					mult = 2;
					break;
				case SV_LONG_BOW:
				case SV_LIGHT_XBOW:
					mult = 3;
					break;
				case SV_HEAVY_XBOW:
					mult = 4;
					break;
				default:
					mult = 0;
			}

			if (f1 & TR1_MIGHT)
			{
				mult += o_ptr->pval;
			}
			p += mult * 2; /* Changed */
			if (f1 & TR1_SHOTS)
			{
				if (o_ptr->pval > 0)
					p += (2 * o_ptr->pval); /* Changed */
			}
			p += o_ptr->to_h / 4;
			/*if (o_ptr->weight < k_ptr->weight) p++;*/
			break;
		}
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			p += (o_ptr->dd * o_ptr->ds + 1) / 2;
			if (f1 & TR1_SLAY_EVIL) p += 3;
			if (f1 & TR1_KILL_DRAGON) p += 3;
			if (f1 & TR1_KILL_DEMON) p += 3;
			if (f1 & TR1_KILL_UNDEAD) p += 3;
			if (f1 & TR1_SLAY_ANIMAL) p += 3;
			if (f1 & TR1_SLAY_UNDEAD) p += 4;
			if (f1 & TR1_SLAY_DRAGON) p += 4;
			if (f1 & TR1_SLAY_DEMON) p += 5;
			if (f1 & TR1_SLAY_TROLL) p += 5;
			if (f1 & TR1_SLAY_ORC) p += 5;
			if (f1 & TR1_SLAY_GIANT) p += 6;

			if (f1 & TR1_BRAND_ACID) p += 2;
			if (f1 & TR1_BRAND_ELEC) p += 3;
			if (f1 & TR1_BRAND_FIRE) p += 4;
			if (f1 & TR1_BRAND_COLD) p += 4;

			p += o_ptr->to_d  / 3;
			if (o_ptr->to_d > 15) p += (o_ptr->to_d - 14) / 2;

			if (f1 & TR1_BLOWS)
			{
				if (o_ptr->pval > 0)
					p += 6 * o_ptr->pval;
			}

			if ((f1 & TR1_TUNNEL) &&
				(o_ptr->tval != TV_DIGGING))
				p += o_ptr->pval * 3;

			p += o_ptr->to_h / 4;

			/* Remember, weight is in 0.1 lb. units. */
			/*if (o_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - o_ptr->weight) / 20;*/

			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		{
			p += o_ptr->ac / 5;
			p += o_ptr->to_h / 2;
			p += o_ptr->to_d / 2;
			/*if (o_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - o_ptr->weight) / 30;*/
			break;
		}
		/*case TV_LITE:
		{
			p += 10;
			break;
		}
		case TV_RING:
		case TV_AMULET:
		{
			p += 20;
			break;
		}*/
	}

	/* pval contributes as the square, and once per each pval-related flag */
	if (o_ptr->pval > 0)
	{
		if (f1 & TR1_STR) p += o_ptr->pval * o_ptr->pval;
		if (f1 & TR1_INT) p += o_ptr->pval * o_ptr->pval;
		if (f1 & TR1_WIS) p += o_ptr->pval * o_ptr->pval;
		if (f1 & TR1_DEX) p += o_ptr->pval * o_ptr->pval;
		if (f1 & TR1_CON) p += o_ptr->pval * o_ptr->pval;
		if (f1 & TR1_STEALTH) p += o_ptr->pval * o_ptr->pval;
	}

	/* charisma is worth a lot less */
	if (f1 & TR1_CHR) p += o_ptr->pval;

	/* Infravision is only worth half the pval */
	if (f1 & TR1_INFRA) p += o_ptr->pval / 2;

	/* Speed is worth 1.5 */
	if (f1 & TR1_SPEED) p += o_ptr->pval * 3 / 2;

	if (f2 & TR2_SUST_STR) p += 6;
	if (f2 & TR2_SUST_INT) p += 4;
	if (f2 & TR2_SUST_WIS) p += 4;
	if (f2 & TR2_SUST_DEX) p += 4;
	if (f2 & TR2_SUST_CON) p += 4;
	if (f2 & TR2_SUST_CHR) p += 1;

	if (f2 & TR2_IM_ACID)
	{
		p += 20;
	}
	if (f2 & TR2_IM_ELEC)
	{
		p += 24;
	}
	if (f2 & TR2_IM_FIRE)
	{
		p += 36;
	}
	if (f2 & TR2_IM_COLD)
	{
		p += 24;
	}

	/* Resistances */
	if (f2 & TR2_RES_ACID) p += 6;
	if (f2 & TR2_RES_ELEC) p += 6;
	if (f2 & TR2_RES_FIRE) p += 6;
	if (f2 & TR2_RES_COLD) p += 6;
	if (f2 & TR2_RES_POIS) p += 12;
	if (f2 & TR2_RES_LITE) p += 8;
	if (f2 & TR2_RES_DARK) p += 10;
	if (f2 & TR2_RES_BLIND) p += 10;
	if (f2 & TR2_RES_CONFU) p += 8;
	if (f2 & TR2_RES_SOUND) p += 10;
	if (f2 & TR2_RES_SHARD) p += 8;
	if (f2 & TR2_RES_NETHR) p += 12;
	if (f2 & TR2_RES_NEXUS) p += 10;
	if (f2 & TR2_RES_CHAOS) p += 12;
	if (f2 & TR2_RES_DISEN) p += 12;

	/* Misc prices */
	if (f3 & TR3_FREE_ACT) p += 8;
	if (f3 & TR3_HOLD_LIFE) p += 10;
	if (f3 & TR3_FEATHER) p += 2;
	if (f3 & TR3_LITE) p += 2;
	if (f3 & TR3_SEE_INVIS) p += 8;
	if (f3 & TR3_TELEPATHY) p += 20;
	if (f3 & TR3_SLOW_DIGEST) p += 4;
	if (f3 & TR3_REGEN) p += 8;
	if (f3 & TR3_TELEPORT) p -= 20;
	if (f3 & TR3_DRAIN_EXP) p -= 16;
	if (f3 & TR3_AGGRAVATE) p -= 8;
	if (f3 & TR3_BLESSED) p += 4;

	/* Cursed items are just flat-out negative */
	if (f3 & TR3_LIGHT_CURSE) p = -1;
	if (f3 & TR3_HEAVY_CURSE) p = -2;
	if (f3 & TR3_PERMA_CURSE) p = -3;

	return p;
}

/*
 * Debug command for looking at quality values.
 */

void do_crafting_appraise(void)
{
	int item_selected = -1;
	int q;
	char objname[80];
	object_type* o_ptr;
	get_item(&item_selected, "Select an item to appraise:", "You have nothing to appraise.", USE_INVEN);
	if (item_selected == -1)
		return;
	o_ptr = &(inventory[item_selected]);
	object_desc(objname, 80, o_ptr, TRUE, ODESC_FULL);
	q = get_item_quality(o_ptr);
	msg_format("Item %s has quality rating %d.", objname, q);
}

bool crafting_item_tester_hook_recipe_target(const object_type* o_ptr)
{
	const recipe_type* rtype = lookup_recipe_target(o_ptr->k_idx);
	return rtype != NULL && rtype->weights[0] >= 0;
}

/* Refines an item into one of its base components.
 *
 */
void do_crafting_refine(void)
{
	int item_selected = -1;
	int sum = 0;
	int i;
	int c = 0;
	int r;
	int kind;
	int q;
	object_type* o_ptr;
	char objname_source[80];
	char objname_extract[80];
	const recipe_type* the_recipe;

	/* Only display items we have a recipe for */
	item_tester_hook = crafting_item_tester_hook_recipe_target;

	/* Select the item to refine */
	get_item(&item_selected, "Select an item to refine:", "You have nothing to refine.", USE_INVEN);
	if (item_selected == -1)
		return;
	o_ptr = &(inventory[item_selected]);
	object_desc(objname_source, 80, o_ptr, TRUE, ODESC_FULL);

	/* Look up the recipe
	 * Assumes that the first recipe with this item as the target is the refining recipe
	 */
	the_recipe = lookup_recipe_target(o_ptr->k_idx);

	/* Check for quantity */
	if (o_ptr->number < the_recipe->target.quantity) {
		msg_format("Not enough to refine; you need at least %d", the_recipe->target.quantity);
		return;
	}

	/* Extract the quality */
	q = get_item_quality(o_ptr);

	/* Delete the item from the inventory */
	o_ptr->number -= the_recipe->target.quantity;
	inven_item_optimize(item_selected);

	/* Spawn an item from the list of ingredients, of appropriate quality
	 *
	 * This requires randomly selecting one of the ingredients in proportion to its weight.
	 * Right now, we spawn exactly one item.
	 */
	/* Calculate total weight */
	for (i = 0; i < the_recipe->num_components; ++i)
	{
		sum += the_recipe->weights[i];
	}

	/* We choose a number between 0 and sum.  Then we start summing items from the beginnng
	 * The first item to add enough weight to the pool that it exceeds our chosen int will be the one we choose.
	 */
	r = randint0(sum);
	for (i = 0; i < the_recipe->num_components; ++i)
	{
		c += the_recipe->weights[i];
		if (the_recipe->weights[i] == 0) continue; /* Don't allow creations with 0 weight */
		kind = the_recipe->components[i].kind;
		if (c > r)
		{
			object_type* j_ptr = &o_list[o_pop()]; /* TODO: Handle failure of this routine gracefully */
			object_prep(j_ptr, kind);
			j_ptr->origin = ORIGIN_CRAFTED;
			/* Threshold the pval of the item */
			if (q > 10) {
				j_ptr->pval = 1;
			} else if (q > 50)
				j_ptr->pval = 2;
			inven_carry(j_ptr);
			object_desc(objname_extract, 80, j_ptr, TRUE, ODESC_FULL);
			msg_format("You extract %s from %s.", objname_extract, objname_source);
			break;
		}
	}

}

/*
 * Add some magic to the item
 *
 * j_ptr points to an initialized item
 * quality is how many points we have to spend
 *
 * TODO: Rewrite this function to make it MUCH cleaner
 */
static void randomize_item(object_type* j_ptr, int quality) {
	if (wield_slot(j_ptr) == INVEN_WIELD) {
		/* Pick a *slay*, if we can */
		if (quality > 25) {
			switch(randint1(3))
			{
			case 1:
				j_ptr->flags1 |= TR1_KILL_DRAGON;
				break;
			case 2:
				j_ptr->flags1 |= TR1_KILL_DEMON;
				break;
			default:
				j_ptr->flags1 |= TR1_KILL_UNDEAD;
			}

			quality -= 25;
		} else if (quality > 10) { /* Pick one slay, if we can (and didn't pick a *slay*) */
			switch(randint1(6))
			{
			case 1:
				j_ptr->flags1 |= TR1_SLAY_ORC;
				break;
			case 2:
				j_ptr->flags1 |= TR1_SLAY_TROLL;
				break;
			case 3:
				j_ptr->flags1 |= TR1_SLAY_GIANT;
				break;
			case 4:
				j_ptr->flags1 |= TR1_SLAY_DRAGON;
				break;
			case 5:
				j_ptr->flags1 |= TR1_SLAY_DEMON;
				break;
			default:
				j_ptr->flags1 |= TR1_SLAY_UNDEAD;
			}

			quality -= 10;
		}

		/* Pick one brand if we can */
		if (quality > 10) {
			switch(randint1(5))
			{
			case 1:
				j_ptr->flags1 |= TR1_BRAND_ACID;
				break;
			case 2:
				j_ptr->flags1 |= TR1_BRAND_ELEC;
				break;
			case 3:
				j_ptr->flags1 |= TR1_BRAND_FIRE;
				break;
			case 4:
				j_ptr->flags1 |= TR1_BRAND_COLD;
				break;
			default:
				j_ptr->flags1 |= TR1_BRAND_POIS;
			}

			quality -= 10;
		}

		/* Burn the rest on to-hit and to-dam */
		while (quality >= 1) {
			switch(randint1(2))
			{
			case 1:
				j_ptr->to_d++;
				break;

			default:
				j_ptr->to_h++;
			}

			quality--;
		}
	} else { /* Must be armor */
		/* stack resists at 5 apiece, until we have about 10 left */
		while (quality >= 10) {
			switch(randint1(16))
			{
			case 1:
				if (j_ptr->flags2 & TR2_RES_ACID) continue;
				j_ptr->flags2 |= TR2_RES_ACID;
				break;
			case 2:
				if (j_ptr->flags2 & TR2_RES_ELEC) continue;
				j_ptr->flags2 |= TR2_RES_ELEC;
				break;
			case 3:
				if (j_ptr->flags2 & TR2_RES_FIRE) continue;
				j_ptr->flags2 |= TR2_RES_FIRE;
				break;
			case 4:
				if (j_ptr->flags2 & TR2_RES_COLD) continue;
				j_ptr->flags2 |= TR2_RES_COLD;
				break;
			case 5:
				if (j_ptr->flags2 & TR2_RES_POIS) continue;
				j_ptr->flags2 |= TR2_RES_POIS;
				break;
			case 6:
				if (j_ptr->flags2 & TR2_RES_FEAR) continue;
				j_ptr->flags2 |= TR2_RES_FEAR;
				break;
			case 7:
				if (j_ptr->flags2 & TR2_RES_LITE) continue;
				j_ptr->flags2 |= TR2_RES_LITE;
				break;
			case 8:
				if (j_ptr->flags2 & TR2_RES_DARK) continue;
				j_ptr->flags2 |= TR2_RES_DARK;
				break;
			case 9:
				if (j_ptr->flags2 & TR2_RES_BLIND) continue;
				j_ptr->flags2 |= TR2_RES_BLIND;
				break;
			case 10:
				if (j_ptr->flags2 & TR2_RES_CONFU) continue;
				j_ptr->flags2 |= TR2_RES_CONFU;
				break;
			case 11:
				if (j_ptr->flags2 & TR2_RES_SOUND) continue;
				j_ptr->flags2 |= TR2_RES_SOUND;
				break;
			case 12:
				if (j_ptr->flags2 & TR2_RES_SHARD) continue;
				j_ptr->flags2 |= TR2_RES_SHARD;
				break;
			case 13:
				if (j_ptr->flags2 & TR2_RES_NEXUS) continue;
				j_ptr->flags2 |= TR2_RES_NEXUS;
				break;
			case 14:
				if (j_ptr->flags2 & TR2_RES_NETHR) continue;
				j_ptr->flags2 |= TR2_RES_NETHR;
				break;
			case 15:
				if (j_ptr->flags2 & TR2_RES_CHAOS) continue;
				j_ptr->flags2 |= TR2_RES_CHAOS;
				break;
			default:
				if (j_ptr->flags2 & TR2_RES_DISEN) continue;
				j_ptr->flags2 |= TR2_RES_DISEN;
			}

			quality -= 5;
		}

		/* Put the rest in ac */
		j_ptr->to_a += quality;
		quality = 0;
	}
}

/* Global variable for the item tester hook.
 * We want to only show items that fight the kind given.
 */
int crafting_item_tester_recipe_ingredient_kind = -1;

bool crafting_item_tester_hook_recipe_ingredient(const object_type* o_ptr)
{
	return o_ptr->k_idx == crafting_item_tester_recipe_ingredient_kind;
}

/* Given a specific recipe to execute, execute it.
 *
 * This entails having the user choose the items he wants to use from his inventory, consuming them, and then spawning the created item.
 * TODO: Later, we will want to add an interface for making the item good/magical. Way down the road.
 */
static void craft_recipe(const recipe_type* the_recipe)
{
	/* Holders for the selected ingredients */
	int items_selected[MAX_COMPONENTS];
	char prompt[80], error[80];
	char o_name[80];
	char objname[80];
	int i;
	int quality=0;
	object_type* j_ptr;

	/* Choose the components -- make the user do this even if there's only one option.
	 * That way, they'll have a harder time "accidentally" crafting stuff.
	 */
	for (i = 0; i < the_recipe->num_components; ++i) /* N_ELEMENTS doesn't like the components array - I suspect it's stored oddly by c */
	{
		items_selected[i] = -1;
		crafting_item_tester_recipe_ingredient_kind = the_recipe->components[i].kind;
		item_tester_hook = crafting_item_tester_hook_recipe_ingredient;
		object_kind_name(o_name, 80, the_recipe->components[i].kind, TRUE);
		strnfmt(prompt, 80, "Select %s:", o_name);
		strnfmt(error, 80, "Could not find %s.", o_name);
		get_item(&items_selected[i], prompt, error, USE_INVEN);
		/* TODO: verify that the user has enough quantity! */
		if (items_selected[i] < 0)
			return;

		if (inventory[items_selected[i]].number < the_recipe->components[i].quantity) {
			msg_format("Not enough %s - you need %d.", o_name, the_recipe->components[i].quantity);
			return;
		}

		quality += the_recipe->weights[i] * inventory[items_selected[i]].pval;
	}

	/* Now that all the components have been chosen, consume them */
	for (i = 0; i < the_recipe->num_components; ++i) {
		inventory[items_selected[i]].number -= the_recipe->components[i].quantity;
		inven_item_optimize(items_selected[i]);
	}

	/* Now make the item. */
	j_ptr = &o_list[o_pop()];
	object_prep(j_ptr, the_recipe->target.kind);
	j_ptr->origin = ORIGIN_CRAFTED;
	j_ptr->number = the_recipe->target.quantity;
	/* Add some random magic to it */
	randomize_item(j_ptr, quality);
	/* ID it */
	object_known(j_ptr);
	object_aware(j_ptr);
	inven_carry(j_ptr);
	object_desc(objname, 80, j_ptr, TRUE, ODESC_FULL);
	msg_format("You succesfully craft %s.", objname);
}

/* Display the name of the recipe in the menu row */
static void recipe_browser_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	/* Write the description */
	char o_name[80];
	object_kind_name(o_name, 80, recipes[oid].target.kind, TRUE);
	Term_putstr(col, row, -1, attr, o_name);
}

/* Handle the user clicking a recipe. */
static bool recipe_browser_action(char cmd, void *db, int oid)
{
	/* Only handle enter */
	if (cmd == '\n' || cmd == '\r')
	{
		craft_recipe(&recipes[oid]);
		return TRUE;
	}
	else
		return FALSE;
}

/*
 * Displays the recipe browser.
 *
 * TODO: Single column format for now, potential problem
 * BUG: The underlying crafting menu is still visible with only one recipe option
 */
void open_recipe_browser(void)
{
	menu_type menu;
	menu_iter commands_menu = {NULL, NULL, recipe_browser_entry, recipe_browser_action};
	region area = { 18, 4, 26, N_ELEMENTS(recipes)};

	ui_event_data evt;
	int cursor = 0;
	crafting_command chosen_command = {"No recipe chosen."};

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\x8B\x8C\n\r";
	menu.count = N_ELEMENTS(recipes);
	menu.menu_data = &chosen_command;
	menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

	/*Set up the screen */
	screen_save();
	window_make(17,3,44,4 + N_ELEMENTS(recipes));

	/*Select an entry */
	evt = menu_select(&menu, &cursor, 0);

	screen_load();

}

/* Crafting menu command hook */
void do_crafting_create(void)
{
	open_recipe_browser();
}

/* The crafting menu */
static crafting_command crafting_commands[] =
{
		{"Extract materials from an item.", do_crafting_refine},
		{"Create an item.", do_crafting_create}
};

/* Display a menu entry in the crafting menu */
static void crafting_sub_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	/* Write the description */
	Term_putstr(col, row, -1, attr, crafting_commands[oid].desc);
}

/* Handle user input from a command menu */
static bool cmd_sub_action(char cmd, void *db, int oid)
{
	/* Only handle enter */
	if (cmd == '\n' || cmd == '\r')
	{
		crafting_commands[oid].hook();
		return TRUE;
	}
	else
		return FALSE;

}

/*
 * Open the crafting menu and allow a selection.
 */
void do_cmd_open_crafting_menu(void)
{
	menu_type menu;
	menu_iter commands_menu = {NULL, NULL, crafting_sub_entry, cmd_sub_action};
	region area = { 18, 4, 32, N_ELEMENTS(crafting_commands)};

	ui_event_data evt;
	int cursor = 0;
	crafting_command chosen_command = {"No command chosen."};

	/* Set up the menu */
	WIPE(&menu, menu);
	menu.cmd_keys = "\x8B\x8C\n\r";
	menu.count = N_ELEMENTS(crafting_commands);
	menu.menu_data = &chosen_command;
	menu_init(&menu, MN_SKIN_SCROLL, &commands_menu, &area);

	/*Set up the screen */
	screen_save();
	window_make(17,3,50, 4 + N_ELEMENTS(crafting_commands));

	/*Select an entry */
	evt = menu_select(&menu, &cursor, 0);

	screen_load();
}
