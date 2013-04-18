/* File: d11adds1.c */

/*
 * Copyright (c) 2001 Stefan "Dunkelelf" Jurisch
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/* Purpose: Additions for the special D11-Angband Version */

#include "angband.h"


/*
 * Static Variables
 */

/* For Extended Genocide */
static cptr genocide_char_list[] =
{
	"a - Ant",
	"b - Bat",
	"c - Centipede",
	"d - Dragon",
	"e - Floating Eye",
	"f - Feline",
	"g - Golem",
	"h - Hobbit/Elf/Dwarf",
	"i - Icky Thing",
	"j - Jelly",
	"k - Kobold",
	"l - Louse",
	"m - Mold",
	"n - Naga",
	"o - Orc",
	"p - Person/Human",
	"q - Quadruped",
	"r - Rodent",
	"s - Skeleton",
	"t - Townsperson",
	"u - Minor Demon",
	"v - Vortex",
	"w - Worm/Worm-Mass",
	/* "x - unused", */
	"y - Yeek",
	"z - Zombie/Mummy",
	"A - Angel",
	"B - Bird",
	"C - Canine",
	"D - Ancient Dragon/Wyrm",
	"E - Elemental",
	"F - Dragon Fly",
	"G - Ghost",
	"H - Hybrid",
	"I - Insect",
	"J - Snake",
	"K - Killer Beetle",
	"L - Lich",
	"M - Multi-Headed Reptile",
	/* "N - unused", */
	"O - Ogre",
	"P - Giant Humanoid",
	"Q - Quylthulg (Pulsing Flesh Mound)",
	"R - Reptile/Amphibian",
	"S - Spider/Scorpion/Tick",
	"T - Troll",
	"U - Major Demon",
	"V - Vampire",
	"W - Wight/Wraith/etc",
	"X - Xorn/Xaren/etc",
	"Y - Yeti",
	"Z - Zephyr Hound",
	NULL
};

/* For Backpack items (including some forward declarations of functions */
object_type *backpack;
int get_backpack_item(void);
int put_backpack_item(void);
int drop_backpack_item(void);
int kill_backpack_item(void);
void show_backpack(void);
void backpack_optimize(int item);
void reorder_backpack(void);


/*
 * Extended Genocide
 */
void extend_genocide(void)
{
	int i;
	int x, y;

	/* Init Vars */
	x = 0;
	y = 1;

	/* Dump Monster list */
	for (i = 0; genocide_char_list[i]; i++)
	{
		c_put_str(TERM_WHITE, genocide_char_list[i], y, x);
		if (y == 24)
		{
			x = x + 26;
			y = 1;
		}
		else
			y++;
	}
}

/*
 * Handle Backpack (an Item which can carry other items)
 * The Backpack is an idea of Edgar Leidig, who wanted to carry more items than
 * the normal inventory is able to. Thank for that idea!
 * The backpack is an special activation type for backpack items.
 * These are items, which are able to store other items from the normal
 * inventory, put them back, drop or destroy them.
 * Some routines are taken from the vanilla code and have been modified to
 * fit the backpack handling.
 * You can do the following actions:
 * - get an item from the backpack to the inventory (key 'g')
 * - put an item from the inventory to the backpack (key 'p')
 * - destroy an item in the backpack (key 'k')
 * - drop an item from the backpack (key 'd')
 * - close the backpack (key ESCAPE)
 * A backpack can only be used, when this item is worn. it will hold all items
 * if it is dropped or left at home. killing the backpack will destroy all items in it.
 * if a new backpack is found the new one will carry all items of the old one.
 * Backpack items are artifact items. items carried in a backpack, will not cause extra
 * weight to the player.
 */
void act_backpack(void)
{
	int success;
	char in;

	/* Init Vars */
	success = 0;

	/* save the screen */
	screen_save();

	/* Input loop */
	while (!success)
	{
		Term_clear();

		/* Show Backpack and give a message */
		show_backpack();
		prt("Backpack Command (g/p/d/k/ESC): ",0 ,0);

		in = inkey();

		switch (in)
		{
			case ESCAPE:
			{
				success = 99;
				break;
			}

			case 'g':
			{
				success = get_backpack_item();
				break;
			}

			case 'p':
			{
				success = put_backpack_item();
				break;
			}

			case 'd':
			{
				success = drop_backpack_item();
				break;
			}

			case 'k':
			{
				success = kill_backpack_item();
				break;
			}
		}
	}

	/* Reorder the inventory */
	reorder_pack();

	/* reload the screen */
	screen_load();

	return;
}

/* Get an item from the backpack and put it into the standard inventory */
int get_backpack_item(void)
{
	char end_index;
	char in;
	char buf[100];
	int i;
	int free_inven_index = -1;
	int get_index;
	object_type *o_ptr;
	object_type *i_ptr;

	/* When backpack empty */
	if (p_ptr->backpack_count == 0)
		return 0;

	end_index = 'a' - 1;
	for (i = 0; i < BACKPACK; i++)
	{
		o_ptr = &backpack[i];
		if (o_ptr->k_idx)
			end_index++;
	}

	for (i = 0; i < INVEN_WIELD; i++)
	{
		i_ptr = &inventory[i];
		if (!i_ptr->k_idx)
		{
			free_inven_index = i;
			break;
		}
	}

	if (free_inven_index == -1)
	{
		c_put_str(TERM_RED, "There is no space left to unpack your backpack", 0, 0);
		inkey();
		return 0;
	}

	sprintf(buf, "Which item do you want to get from the backpack? (a-%c): ", end_index);
	c_put_str(TERM_WHITE, buf, 0, 0);

	in = inkey();

	if (in == ESCAPE)
		return 0;

	get_index = in - 'a';

	if (get_index > end_index)
		return 0;

	o_ptr = &backpack[get_index];

	COPY(&inventory[free_inven_index], o_ptr, object_type);
	p_ptr->backpack_count--;

	/* Update the backpack and inventory */
	o_ptr->number = 0;
	backpack_optimize(get_index);

	return 0;
}

/* Put an item from the inventory to the backpack */
int put_backpack_item(void)
{
	int item, amt;

	object_type *o_ptr;

	cptr q, s;

	/* When backpack full */
	if (p_ptr->backpack_count == BACKPACK)
	{
		msg_print("Your backpack is full!");
		return 0;
	}

	/* Get an item */
	q = "Put which item to your backpack? ";
	s = "You have nothing to put it into the backpack.";
	if (!get_item(&item, q, s, (USE_INVEN))) return 0;

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

	COPY(&backpack[p_ptr->backpack_count], o_ptr, object_type);
	p_ptr->backpack_count++;
	/* Update the inventory */
	o_ptr = &inventory[item];
	o_ptr->number = 0;
	inven_item_optimize(item);
	return 0;
}

/* drop an item from the backpack */
int drop_backpack_item(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int i;
	int get_index;
	char o_name[80];
	char end_index;
	char in;
	char buf[100];

	object_type *o_ptr;

	/* Get real objects */
	end_index = 'a' - 1;
	for (i = 0; i < BACKPACK; i++)
	{
		o_ptr = &backpack[i];
		if (o_ptr->k_idx)
			end_index++;
	}

	sprintf(buf, "Drop wich item from the backpack? (a-%c): ", end_index);
	c_put_str(TERM_WHITE, buf, 0, 0);

	in = inkey();

	if (in == ESCAPE)
		return 0;

	get_index = in - 'a';

	if (get_index > end_index)
		return 0;

	/* Get the original object */
	o_ptr = &backpack[get_index];

	/* Obtain an item description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Describe local object */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Message */
	sprintf(buf, "You drop %s (%c).", o_name, in);
	c_put_str(TERM_WHITE, buf, 0, 0);

	/* Drop it near the player */
	drop_near(o_ptr, 0, py, px);

	/* Modify, Describe, Optimize */
	o_ptr->number = 0;
	backpack_optimize(get_index);
	reorder_backpack();

	return 0;
}

/* kill an item in the backpack */
int kill_backpack_item(void)
{
	char end_index;
	char in;
	char buf[100];
	char o_name[80];
	int i;
	int get_index;
	object_type *o_ptr;

	end_index = 'a' - 1;
	for (i = 0; i < BACKPACK; i++)
	{
		o_ptr = &backpack[i];
		if (o_ptr->k_idx)
			end_index++;
	}

	sprintf(buf, "Destroy which item in your backpack? (a-%c): ", end_index);
	c_put_str(TERM_WHITE, buf, 0, 0);

	in = inkey();

	if (in == ESCAPE)
		return 0;

	get_index = in - 'a';

	if (get_index > end_index)
		return 0;

	/* Get the chosen object */
	o_ptr = &backpack[get_index];

	/* Obtain an item description */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* security-check: really destroy??? */
	sprintf(buf, "Really destroy %s? [y/n]: ", o_name);
	c_put_str(TERM_WHITE, buf, 0, 0);
	in = inkey();

	if (in != 'y')
		return 0;

	/* Continue destroying */
	p_ptr->backpack_count--;

	/* Message */
	sprintf(buf, "You destroy %s (%c).", o_name, in);
	c_put_str(TERM_WHITE, buf, 0, 0);

	/* Update the backpack and inventory */
	o_ptr = &backpack[get_index];
	o_ptr->number = 0;
	backpack_optimize(get_index);

	return 0;
}

/* Show the backpack */
void show_backpack(void)
{
	register int i, n, z = 23;

	object_type *o_ptr;

	byte attr;

	char tmp_val[80];

	char o_name[80];

	/* Display the pack */
	for (i = 0; i < z; i++)
	{
		/* Examine the item */
		o_ptr = &backpack[i];

		/* Start with an empty "index" */
		tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

		/* Is this item "acceptable"? */
		if (item_tester_okay(o_ptr))
		{
			/* Prepare an "index" */
			tmp_val[0] = index_to_label(i);

			/* Bracket the "index" --(-- */
			tmp_val[1] = ')';
		}

		/* Display the index (or blank space) */
		Term_putstr(0, i + 1, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		if (!strcmp(o_name, "(nothing)"))
			strcpy(o_name, "         ");

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval & 0x7F];

		/* Display the entry itself */
		Term_putstr(3, i + 1, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3+n, i + 1, 255);

		/* Display the weight if needed */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			Term_putstr(71, i + 1, -1, TERM_WHITE, tmp_val);
		}
	}
}

void backpack_optimize(int item)
{
	int i;

	p_ptr->inven_cnt--;

		/* Slide everything down */
	for (i = item; i < BACKPACK; i++)
	{

		/* Hack -- slide object */
		COPY(&backpack[i], &backpack[i+1], object_type);
	}

	/* Hack -- wipe hole */
	(void)WIPE(&backpack[i], object_type);
}

/* Reorder the backpack */
void reorder_backpack(void)
{
	int i, j, k;

	s32b o_value;
	s32b j_value;

	object_type *o_ptr;
	object_type *j_ptr;

	object_type *i_ptr;
	object_type object_type_body;

	bool flag = FALSE;


	/* Re-order the backpack (forwards) */
	for (i = 0; i < BACKPACK; i++)
	{
		/* Mega-Hack -- allow "proper" over-flow */
		if ((i == BACKPACK) && (p_ptr->backpack_count == INVEN_PACK)) break;

		/* Get the item */
		o_ptr = &backpack[i];

		/* Skip empty slots */
		if (!o_ptr->k_idx) continue;

		/* Get the "value" of the item */
		o_value = object_value(o_ptr);

		/* Scan every occupied slot */
		for (j = 0; j < BACKPACK; j++)
		{
			/* Get the item already there */
			j_ptr = &backpack[j];

			/* Use empty slots */
			if (!j_ptr->k_idx) break;

			/* Hack -- readable books always come first */
			if ((o_ptr->tval == cp_ptr->spell_book) &&
			    (j_ptr->tval != cp_ptr->spell_book)) break;
			if ((j_ptr->tval == cp_ptr->spell_book) &&
			    (o_ptr->tval != cp_ptr->spell_book)) continue;

			/* Objects sort by decreasing type */
			if (o_ptr->tval > j_ptr->tval) break;
			if (o_ptr->tval < j_ptr->tval) continue;

			/* Non-aware (flavored) items always come last */
			if (!object_aware_p(o_ptr)) continue;
			if (!object_aware_p(j_ptr)) break;

			/* Objects sort by increasing sval */
			if (o_ptr->sval < j_ptr->sval) break;
			if (o_ptr->sval > j_ptr->sval) continue;

			/* Unidentified objects always come last */
			if (!object_known_p(o_ptr)) continue;
			if (!object_known_p(j_ptr)) break;

			/* Rods sort by increasing recharge time */
			if (o_ptr->tval == TV_ROD)
			{
				if (o_ptr->pval < j_ptr->pval) break;
				if (o_ptr->pval > j_ptr->pval) continue;
			}

			/* Wands/Staffs sort by decreasing charges */
			if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}

			/* Lites sort by decreasing fuel */
			if (o_ptr->tval == TV_LITE)
			{
				if (o_ptr->pval > j_ptr->pval) break;
				if (o_ptr->pval < j_ptr->pval) continue;
			}

			/* Determine the "value" of the pack item */
			j_value = object_value(j_ptr);

			/* Objects sort by decreasing value */
			if (o_value > j_value) break;
			if (o_value < j_value) continue;
		}

		/* Never move down */
		if (j >= i) continue;

		/* Take note */
		flag = TRUE;

		/* Get local object */
		i_ptr = &object_type_body;

		/* Save a copy of the moving item */
		object_copy(i_ptr, &backpack[i]);

		/* Slide the objects */
		for (k = i; k > j; k--)
		{
			/* Slide the item */
			object_copy(&backpack[k], &backpack[k-1]);
		}

		/* Insert the moving item */
		object_copy(&backpack[j], i_ptr);
	}
}
