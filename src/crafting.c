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
	int* weights; /* How important each component is - used in determining what drops from a refine, and quality */
} recipe_type;

/*
 * Would be really nice to move this to an edit file.
 *
 * Hack: weight of -1 => can't refine
 */
const static recipe_type recipes[] =
{																				/* <target>  	    <- {<component> (<weight>)}* */
	{{30,  1}, 2, (component_type[]){{565, 1}, {567, 1}}, (int[]){10, 1}},		/* 1 Dagger 		<- 1 iron, 1 leather */
	{{33,  1}, 2, (component_type[]){{565, 2}, {567, 1}}, (int[]){20, 1}},		/* 1 Short Sword 	<- 2 iron, 1 leather */
	{{37,  1}, 2, (component_type[]){{565, 3}, {567, 1}}, (int[]){30, 1}},		/* 1 Long Sword 	<- 3 iron, 1 leather */
	{{50,  1}, 2, (component_type[]){{567, 2}, {569, 1}}, (int[]){20, 1}},		/* 1 Whip		 	<- 2 leather, 1 ash   */
	{{90,  1}, 1, (component_type[]){{567, 2}		   }, (int[]){1}   },		/* 1 Sling		 	<- 2 leather          */
	{{91,  1}, 2, (component_type[]){{569, 2}, {567, 1}}, (int[]){20, 1}},		/* 1 Short Bow   	<- 2 ash, 1 leather   */
	{{100, 5}, 1, (component_type[]){{569, 1} 	       }, (int[]){-1}   },		/* 5 Arrows  	 	<- 1 ash			  */
	{{111, 5}, 1, (component_type[]){{565, 1}          }, (int[]){1}   },		/* 5 Iron Shot	 	<- 1 iron			  */
	{{131, 1}, 1, (component_type[]){{567, 2}          }, (int[]){1}    },		/* 1 P. Leath. Bts 	<- 2 leather		  */
	{{141, 1}, 2, (component_type[]){{565, 2}, {567, 1}}, (int[]){20, 1}},		/* 1 Metal Cap  	<- 2 iron, 1 leather */
};

/* Find the first recipe with this target.*/
const recipe_type * lookup_recipe_target(int kind)
{
	for (int i = 0; i < N_ELEMENTS(recipes); ++i)
	{
		if (recipes[i].target.kind == kind)
			return &recipes[i];
	}

	return NULL;
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

	/* Only display items we have a recipe for */
	item_tester_hook = crafting_item_tester_hook_recipe_target;

	/* Select the item to refine */
	get_item(&item_selected, "Select an item to refine:", "You have nothing to refine.", USE_INVEN);
	if (item_selected == -1)
		return;
	object_type* o_ptr = &(inventory[item_selected]);
	char objname_source[80];
	object_desc(objname_source, 80, o_ptr, TRUE, ODESC_FULL);

	/* Look up the recipe
	 * Assumes that the first recipe with this item as the target is the refining recipe
	 */
	const recipe_type* the_recipe = lookup_recipe_target(o_ptr->k_idx);

	/* Check for quantity */
	if (o_ptr->number < the_recipe->target.quantity) {
		msg_format("Not enough to refine; you need at least %d", the_recipe->target.quantity);
		return;
	}

	/* Delete the item from the inventory */
	o_ptr->number -= the_recipe->target.quantity;
	inven_item_optimize(item_selected);

	/* Spawn an item from the list of ingredients, of appropriate quality
	 *
	 * This requires randomly selecting one of the ingredients in proportion to its weight.
	 * Right now, we spawn exactly one item.
	 */
	/* Calculate total weight */
	int sum = 0;
	for (int i = 0; i < the_recipe->num_components; ++i)
	{
		sum += the_recipe->weights[i];
	}

	/* We choose a number between 0 and sum.  Then we start summing items from the beginnng
	 * The first item to add enough weight to the pool that it exceeds our chosen int will be the one we choose.
	 */
	int r = randint0(sum);
	int c = 0;
	for (int i = 0; i < the_recipe->num_components; ++i)
	{
		c += the_recipe->weights[i];
		if (the_recipe->weights[i] == 0) continue; /* Don't allow creations with 0 weight */
		int kind = the_recipe->components[i].kind;
		if (c > r)
		{
			object_type* j_ptr = &o_list[o_pop()]; /* TODO: Handle failure of this routine gracefully */
			object_prep(j_ptr, kind);
			j_ptr->origin = ORIGIN_CRAFTED;
			inven_carry(j_ptr);
			char objname_extract[80];
			object_desc(objname_extract, 80, j_ptr, TRUE, ODESC_FULL);
			msg_format("You extract %s from %s.", objname_extract, objname_source);
			break;
		}
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

	/* Choose the components -- make the user do this even if there's only one option.
	 * That way, they'll have a harder time "accidentally" crafting stuff.
	 */
	for (int i = 0; i < the_recipe->num_components; ++i) /* N_ELEMENTS doesn't like the components array - I suspect it's stored oddly by c */
	{
		items_selected[i] = -1;
		crafting_item_tester_recipe_ingredient_kind = the_recipe->components[i].kind;
		item_tester_hook = crafting_item_tester_hook_recipe_ingredient;
		char prompt[80], error[80];
		char o_name[80];
		object_kind_name(o_name, 80, the_recipe->components[i].kind, TRUE);
		strnfmt(prompt, 80, "Select %s:", o_name);
		strnfmt(error, 80, "Could not find %s.", o_name);
		get_item(&items_selected[i], prompt, error, USE_INVEN);
		/* TODO: verify that the user has enough quantity! */
		if (items_selected[i] < 0)
			return;
	}

	/* Now that all the components have been chosen, consume them */
	for (int i = 0; i < the_recipe->num_components; ++i) {
		inventory[items_selected[i]].number -= the_recipe->components[i].quantity;
		inven_item_optimize(items_selected[i]);
	}

	/* Now make the item. */
	object_type* j_ptr = &o_list[o_pop()];
	object_prep(j_ptr, the_recipe->target.kind);
	j_ptr->origin = ORIGIN_CRAFTED;
	j_ptr->number = the_recipe->target.quantity;
	/* ID it */
	object_known(j_ptr);
	object_aware(j_ptr);
	inven_carry(j_ptr);
	char objname[80];
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
		{"Refine an item.", do_crafting_refine},
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
	region area = { 18, 4, 19, 2};

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
	window_make(17,3,37,6);

	/*Select an entry */
	evt = menu_select(&menu, &cursor, 0);

	screen_load();
}
