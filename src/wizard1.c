/* File: wizard1.c */

/*
 * Generation of object, artifact, and monster spoilers.
 *
 * Copyright (c) 2007 Ben Harrison, and others
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


#ifdef ALLOW_SPOILERS


/*
 * The spoiler file being created
 */
static FILE *fff = NULL;




/*
 * Write out `n' of the character `c' to the spoiler file
 */
static void spoiler_out_n_chars(int n, char c)
{
	while (--n >= 0) fputc(c, fff);
}

/*
 * Write out `n' blank lines to the spoiler file
 */
static void spoiler_blanklines(int n)
{
	spoiler_out_n_chars(n, '\n');
}

/*
 * Write a line to the spoiler file and then "underline" it with hyphens
 */
static void spoiler_underline(cptr str)
{
	text_out(str);
	text_out("\n");
	spoiler_out_n_chars(strlen(str), '-');
	text_out("\n");
}

/*
 * Print consistent headers for spoiler files
 */
static void print_header(cptr spoiler_type)
{
	char buf[DESC_LEN];

	(void)strnfmt(buf, sizeof(buf), "%s Spoilers for %s %d.%d.%d",
		spoiler_type, VERSION_NAME,
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
	spoiler_underline(buf);
}


/*
 * Extract a textual representation of an attribute
 */
static cptr attr_to_text(byte a)
{
	switch (a)
	{
		case TERM_DARK:    return ("Black");
		case TERM_WHITE:   return ("White");
		case TERM_SLATE:   return ("Slate");
		case TERM_ORANGE:  return ("Orange");
		case TERM_RED:     return ("Red");
		case TERM_GREEN:   return ("Green");
		case TERM_BLUE:    return ("Blue");
		case TERM_UMBER:   return ("Umber");
		case TERM_L_DARK:  return ("L.Dark");
		case TERM_L_WHITE: return ("L.Slate");
		case TERM_PURPLE:  return ("Violet");
		case TERM_YELLOW:  return ("Yellow");
		case TERM_L_RED:   return ("L.Red");
		case TERM_L_GREEN: return ("L.Green");
		case TERM_L_BLUE:  return ("L.Blue");
		case TERM_L_UMBER: return ("L.Umber");
	}

	/* Oops */
	return ("Icky");
}



/*
 * A tval grouper
 */
typedef struct
{
	byte tval;
	cptr name;
} grouper;



/*
 * Item Spoilers by Ben Harrison (benh@phial.com)
 */


/*
 * The basic items categorized by type
 */
static grouper group_item[] =
{
	{ TV_SHOT,       "Ammo" },
	{ TV_ARROW,      NULL },
	{ TV_BOLT,       NULL },

	{ TV_SLING,      "Missile Launchers" },
	{ TV_BOW,        NULL },
	{ TV_CROSSBOW,   NULL },

	{ TV_SWORD,      "Melee Weapons" },
	{ TV_POLEARM,    NULL },
	{ TV_HAFTED,     NULL },
	{ TV_DIGGING,    NULL },

	{ TV_SOFT_ARMOR, "Armor (Body)" },
	{ TV_HARD_ARMOR, NULL },
	{ TV_DRAG_ARMOR, NULL },

	{ TV_CLOAK,      "Armor (Misc)" },
	{ TV_SHIELD,     NULL },
	{ TV_HELM,       NULL },
	{ TV_CROWN,      NULL },
	{ TV_GLOVES,     NULL },
	{ TV_BOOTS,      NULL },

	{ TV_AMULET,     "Amulets" },
	{ TV_RING,       "Rings" },



	{ TV_SCROLL,     "Scrolls" },
	{ TV_POTION,     "Potions" },
	{ TV_FOOD,       "Food and Mushrooms" },

	{ TV_ROD,        "Rods" },
	{ TV_WAND,       "Wands" },
	{ TV_STAFF,      "Staffs" },

	{ TV_MAGIC_BOOK, "Books (Mage)" },
	{ TV_PRAYER_BOOK,"Books (Priest)" },
	{ TV_NATURE_BOOK,"Stones (Druid)" },
	{ TV_DARK_BOOK,  "Tomes (Necro)" },

	{ TV_CHEST,      "Chests" },

	{ TV_COMPONENT,  "Item-creation" },
	{ TV_PARCHMENT,  NULL },
	{ TV_BOTTLE,     NULL },
	{ TV_ESSENCE,    NULL },

	{ TV_SPIKE,      "Various" },
	{ TV_LITE,       NULL },
	{ TV_FLASK,      NULL },
	{ TV_JUNK,       NULL },
	{ TV_SKELETON,   NULL },
	{ TV_POUCH,      NULL },
	{ TV_GOLD,       NULL },

	{ 0, "" }
};



/*
 * Describe the kind
 */
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val, int k)
{
	object_kind *k_ptr;

	object_type *i_ptr;
	object_type object_type_body;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Prepare a fake item */
	object_prep(i_ptr, k);

	/* Obtain the "kind" info */
	k_ptr = &k_info[i_ptr->k_idx];

	/* It is known */
	i_ptr->ident |= (IDENT_KNOWN);

	/* Cancel bonuses */
	i_ptr->pval = 0;
	i_ptr->to_a = 0;
	i_ptr->to_h = 0;
	i_ptr->to_d = 0;


	/* Level */
	(*lev) = k_ptr->level;

	/* Value - use only base value with no modifications */
	(*val) = k_ptr->cost;


	/* Hack */
	if (!buf || !dam || !wgt) return;


	/* Description (too brief) */
	object_desc_store(buf, sizeof(buf), i_ptr, FALSE, 0);


	/* Misc info */
	strcpy(dam, "");

	/* Damage */
	switch (i_ptr->tval)
	{
		/* Bows */
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		{
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			(void)strnfmt(dam, 32, "%dd%d", i_ptr->dd, i_ptr->ds);
			break;
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			(void)strnfmt(dam, 32, "%dd%d", i_ptr->dd, i_ptr->ds);
			break;
		}

		/* Armor */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			(void)strnfmt(dam, 32, "%d", i_ptr->ac);
			break;
		}
	}


	/* Weight */
	if (use_metric)
	{
		(void)strnfmt(wgt, 32, "%3d.%d", make_metric(i_ptr->weight) / 10,
			make_metric(i_ptr->weight) % 10);
	}
	else
	{
		(void)strnfmt(wgt, 32, "%3d.%d", i_ptr->weight / 10, i_ptr->weight % 10);
	}

}
extern void spoil_obj_desc(cptr fname);

/*
 * Create a spoiler file for items
 */
void spoil_obj_desc(cptr fname)
{
	int i, k, s, t, n = 0;

	u16b who[256];

	char buf[1024];

	char wgt[DESC_LEN];
	char dam[DESC_LEN];

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Object");

	/* More Header */
	x_fprintf(fff, encoding, "%-45s     %8s%7s%5s%9s\n",
		"Description", "Dam/AC", "Wgt", "Lev", "Cost");
	x_fprintf(fff, encoding, "%-45s     %8s%7s%5s%9s\n",
		"----------------------------------------",
		"------", "---", "---", "----");

	/* List the groups */
	for (i = 0; TRUE; i++)
	{
		/* Write out the group title */
		if (group_item[i].name)
		{
			/* Hack -- bubble-sort by cost and then level */
			for (s = 0; s < n - 1; s++)
			{
				for (t = 0; t < n - 1; t++)
				{
					int i1 = t;
					int i2 = t + 1;

					int e1;
					int e2;

					s32b t1;
					s32b t2;

					kind_info(NULL, NULL, NULL, &e1, &t1, who[i1]);
					kind_info(NULL, NULL, NULL, &e2, &t2, who[i2]);

					if ((t1 > t2) || ((t1 == t2) && (e1 > e2)))
					{
						int tmp = who[i1];
						who[i1] = who[i2];
						who[i2] = tmp;
					}
				}
			}

			/* Spoil each item */
			for (s = 0; s < n; s++)
			{
				int e;
				s32b v;

				/* Describe the kind */
				kind_info(buf, dam, wgt, &e, &v, who[s]);

				/* Dump it */
				if (!strlen(dam))
					x_fprintf(fff, encoding, "     %-53s%7s%5d%9ld\n",
						buf, wgt, e, (long)(v));
				else
					x_fprintf(fff, encoding, "     %-45s%8s%7s%5d%9ld\n",
						buf, dam, wgt, e, (long)(v));
			}

			/* Start a new set */
			n = 0;

			/* Notice the end */
			if (!group_item[i].tval) break;

			/* Start a new set */
			x_fprintf(fff, encoding, "\n\n%s\n\n", group_item[i].name);
		}

		/* Acquire legal item types */
		for (k = 1; k < z_info->k_max; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip objects not of this tval */
			if (k_ptr->tval != group_item[i].tval) continue;

			/* Hack -- Skip instant-artifacts */
			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

			/* Save the index */
			who[n++] = k;
		}
	}


	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Message */
	msg_print("Successfully created a spoiler file.");
}


/*
 * Artifact Spoilers
 */

/*
 * MAX_LINE_LEN specifies when a line should wrap.
 */
#define MAX_LINE_LEN 75

/*
 * The artifacts categorized by type
 */
static grouper group_artifact[] =
{
	{ TV_SWORD,		"Edged Weapons" },
	{ TV_POLEARM,	"Polearms" },
	{ TV_HAFTED,	"Hafted (blunt) Weapons" },
	{ TV_SLING,		"Slings" },
	{ TV_BOW,		"Bows" },
	{ TV_CROSSBOW,	"Crossbows" },
	{ TV_SHOT,		"Ammunition - Shots" },
	{ TV_ARROW,		"Ammunition - Arrows" },
	{ TV_BOLT,		"Ammunition - Bolts" },

	{ TV_SOFT_ARMOR,	"Body Armor" },
	{ TV_HARD_ARMOR,	  NULL },
	{ TV_DRAG_ARMOR,	  NULL },

	{ TV_CLOAK,		"Cloaks" },
	{ TV_SHIELD,	"Shields" },
	{ TV_HELM,		"Helms/Crowns" },
	{ TV_CROWN,		  NULL },
	{ TV_GLOVES,	"Gloves" },
	{ TV_BOOTS,		"Boots" },

	{ TV_LITE,		"Light Sources" },
	{ TV_AMULET,	"Amulets" },
	{ TV_RING,		"Rings" },

	{ 0, NULL }
};


/*
 * Show what object kinds appear on the current level
 */
static void spoil_obj_gen(cptr fname)
{
	int i, j;
	int tmp_object_level = object_level;
	int old_depth = p_ptr->depth;

	/* Storage */
	u32b artifacts = 0L;
	u32b egoitems = 0L;
	u32b object[1500];
	s16b ego_item[500];
	s16b artifact[ART_MIN_RANDOM + 1];
	u32b tval[TV_MAX];
	u32b depth[MAX_DEPTH];

	object_type *i_ptr;
	object_type object_type_body;
	char o_name[DESC_LEN];

	char buf[1024];


	if (!get_check("This will take quite a while (as in, go read a book).  Are you certain you want to print out object generation statistics (y/n)?"))
	{
		return;
	}


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Warning */
	msg_print("This will take quite a while...");
	if (!fresh_after) (void)Term_fresh();


	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Object Generation");


	/* Initialize object level */
	object_level = 0;
	p_ptr->depth = 0;

	/* Handle various possible depths */
	while (TRUE)
	{
		/* Go from level 5 to level 100 inclusive */
		if (object_level >= 100) break;
		object_level += 5;
		p_ptr->depth += 5;

		artifacts = 0L;
		egoitems = 0L;

		/* Clear storage. */
		for (i = 0; i < z_info->k_max; i++)
		{
			object[i] = 0L;
		}

		/* Clear storage. */
		for (i = 0; i < MAX_DEPTH; i++)
		{
			depth[i] = 0L;
		}

		/* Clear storage. */
		for (i = 0; i < TV_MAX; i++)
		{
			tval[i] = 0L;
		}

		/* Clear storage. */
		for (i = 0; i < ART_MIN_RANDOM + 1; i++)
		{
			artifact[i] = 0;
		}

		/* Clear storage. */
		for (i = 0; i < z_info->e_max; i++)
		{
			ego_item[i] = 0;
		}

		/* Make a lot of objects */
		for (i = 0L; i < 1000000L; i++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Create an object - no special conditions */
			make_object(i_ptr, FALSE, FALSE, FALSE);

			/* Count artifacts. */
			if (i_ptr->artifact_index) artifacts += 1L;

			/* Count ego-items. */
			if (i_ptr->ego_item_index) egoitems += 1L;

			/* Count object by index (note quantity). */
			object[i_ptr->k_idx] += i_ptr->number;

			/* Count objects of that level (only one at a time). */
			depth[k_info[i_ptr->k_idx].level] += 1L;

			/* Count object kinds. */
			tval[i_ptr->tval] += 1L;

			/* Count artifacts */
			if (i_ptr->artifact_index)
			{
				if (i_ptr->artifact_index < ART_MIN_RANDOM)
				{
					artifact[i_ptr->artifact_index]++;
				}
				else
				{
					artifact[ART_MIN_RANDOM]++;
				}
			}

			/* Count ego-items */
			else if (i_ptr->ego_item_index)
			{
				ego_item[i_ptr->ego_item_index]++;
			}

			/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
			if (artifact_p(i_ptr)) a_info[i_ptr->artifact_index].cur_num = 0;
		}

		/* Print to file. */
		fprintf(fff, "\n\n\n\n");
		fprintf(fff, "----------------------------------------\n");
		fprintf(fff, "         Generation Level:  %d\n\n", object_level);
		fprintf(fff, "Number of objects created (1,000,000 total)\n");
		fprintf(fff, "\n");

		for (i = 1; i < z_info->k_max; i++)
		{
			if (object[i])
			{
				object_kind *k_ptr = &k_info[i];
				char *t;
				cptr str = (k_name + k_ptr->name);
				cptr desc;

				if (str == "") continue;

				/* Skip past leading characters */
				while ((*str == ' ') || (*str == '&')) str++;

				/* Copy useful chars */
				for (t = o_name; *str; str++)
				{
					if (*str != '~') *t++ = *str;
				}

				/* Terminate the new name */
				*t = '\0';

				/* Try to find this tval in the list */
				for (j = 0; tvals[j].tval; j++)
				{
					if (tvals[j].tval == k_ptr->tval) break;
				}

				if (!tvals[j].tval) desc = "unknown";

				else desc = tvals[j].desc;

				fprintf(fff, "%-20s:%-40s:%6ld\n", desc, o_name, (long)object[i]);
			}
		}

		/* Header -- tval abundance */
		fprintf(fff, "\n\n");
		fprintf(fff, "Object tvals\n\n");

		/* Scan all the tvals */
		for (i = 1; i < TV_MAX; i++)
		{
			/* Objects with this tval never appeared */
			if (!tval[i]) continue;

			/* Try to find this tval in the list */
			for (j = 0; tvals[j].tval; j++)
			{
				if (tvals[j].tval == i) break;
			}

			/* Tval has no description -- skip it */
			if (!tvals[j].tval) continue;

			/* Print out a description of the tval and its abundance */
			fprintf(fff, "%-20s:%6ld\n", tvals[j].desc, (long)tval[i]);
		}

		fprintf(fff, "\n\n");
		fprintf(fff, "Object distribution by depth\n\n");

		for (i = 0; i < MAX_DEPTH; i++)
		{
			if (depth[i]) fprintf(fff, "Level %3d:%6ld\n", i, (long)depth[i]);
		}

		fprintf(fff, "\n\n");
		fprintf(fff, "artifacts:  %ld\n", (long)artifacts);
		for (i = 0; i < ART_MIN_RANDOM + 1; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			cptr str = (a_name + a_ptr->name);

			if (artifact[i]) fprintf(fff, " %-40s:%4d\n", str, artifact[i]);
		}

		fprintf(fff, "\n\n");
		fprintf(fff, "ego-items:  %ld\n\n", (long)egoitems);
		for (i = 0; i < z_info->e_max; i++)
		{
			ego_item_type *e_ptr = &e_info[i];

			cptr str = (e_name + e_ptr->name);

			if (ego_item[i]) fprintf(fff, " %-40s:%4d\n", str, ego_item[i]);
		}
	}


	/* Reset object generation level */
	object_level = tmp_object_level;
	p_ptr->depth = old_depth;

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Message */
	msg_print("Successfully created a spoiler file.");
}


/*
 * Show what monster races appear on the current level
 */
static void spoil_mon_gen(cptr fname)
{
	int i, num;

	/* Storage */
	u32b monster[1000];
	u32b depth[MAX_DEPTH];

	char buf[1024];

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Monster Generation");

	/* Clear storage. */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster[i] = 0L;
	}

	/* Clear storage. */
	for (i = 0; i < MAX_DEPTH; i++)
	{
		depth[i] = 0L;
	}

	msg_print("This may take a while...");
	if (!fresh_after) (void)Term_fresh();

	/* Make a lot of monsters, and print their names out. */
	for (i = 0L; i < 1000000L; i++)
	{
		if (i % 10000 == 0)
		{
			prt(format("%ld monsters created", (long)i), 0, 0);
			if (!fresh_after) (void)Term_fresh();
		}

		/* Get a monster index */
		num = get_mon_num(p_ptr->depth);

		/* Count monster races. */
		monster[num] += 1L;

		/* Count monsters of that level. */
		depth[r_info[num].level] += 1L;
	}

	/* Print to file. */
	fprintf(fff, "\n\n\n");
	fprintf(fff, "Number of monsters of various kinds (1,000,000 total)\n");
	fprintf(fff, "         Generation Level:  %d\n\n", p_ptr->depth);

	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		cptr name = (r_name + r_ptr->name);

		if (monster[i])
		{
			x_fprintf(fff, encoding, "%-45s:%6ld\n", name, (long)monster[i]);
		}
	}

	fprintf(fff, "\n\n\n");
	fprintf(fff, "Monster distribution by depth\n\n");

	for (i = 0; i < MAX_DEPTH; i++)
	{
		if (depth[i]) fprintf(fff, "Level %3d:%6ld\n", i, (long)depth[i]);
	}


	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Message */
	msg_print("Successfully created a spoiler file.");
}



/*
 * Create a spoiler file for artifacts
 */
static void spoil_artifact(cptr fname)
{
	int i, j;

	object_type *i_ptr;
	object_type object_type_body;

	char buf[1024];

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Dump the header */
	print_header("Artifact");

	/* List the artifacts by tval */
	for (i = 0; group_artifact[i].tval; i++)
	{
		/* Write out the group title */
		if (group_artifact[i].name)
		{
			spoiler_blanklines(2);
			spoiler_underline(group_artifact[i].name);
			spoiler_blanklines(1);
		}

		/* Now search through all of the artifacts */
		for (j = 0; j < z_info->a_max; ++j)
		{
			artifact_type *a_ptr = &a_info[j];

			/* We only want objects in the current group */
			if (a_ptr->tval != group_artifact[i].tval) continue;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Attempt to create the artifact */
			if (!make_fake_artifact(i_ptr, j)) continue;

			/* Get this artifact */
			a_ptr = &a_info[i_ptr->artifact_index];

			/* Write a description of the artifact */
			object_desc_store(buf, sizeof(buf), i_ptr, TRUE, 1);
			x_fprintf(fff, encoding, buf);
			fprintf(fff, "\n");

			/* Write pval, flag, and activation information */
			dump_obj_attrib(fff, i_ptr, 2);

			/* Write level, rarity, and weight */
			if (use_metric) fprintf(fff, "     Level %u, Rarity %u, %d.%d kgs, "
				"%ld Gold", a_ptr->level, a_ptr->rarity,
				make_metric(a_ptr->weight) / 10, make_metric(a_ptr->weight) % 10,
				a_ptr->cost);

			else fprintf(fff, "     Level %u, Rarity %u, %d.%d lbs, "
				"%ld Gold", a_ptr->level, a_ptr->rarity,
				a_ptr->weight / 10, a_ptr->weight % 10, (long)a_ptr->cost);

			/* Insert a spacer line */
			fprintf(fff, "\n\n");
		}
	}

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Message */
	msg_print("Successfully created a spoiler file.");
}





/*
 * Create a spoiler file for monsters
 */
static void spoil_mon_desc(cptr fname)
{
	int i, n = 0;

	char buf[1024];

	/* We use either ascii or system-specific encoding */
	int encoding = (xchars_to_file) ? SYSTEM_SPECIFIC : ASCII;

	char nam[DESC_LEN];
	char lev[32];
	char rar[32];
	char spd[32];
	char ac[32];
	char hp[32];
	char exp[32];

	u16b *who;
	u16b why = 2;

	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Brief Monster");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"----", "---", "---", "---", "--", "--", "-----------");


	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++] = (u16b)i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	/* Scan again */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		cptr name = (r_name + r_ptr->name);

		/* Get the "name" */
		if (r_ptr->flags1 & (RF1_QUESTOR))
		{
			(void)strnfmt(nam, sizeof(nam), "[Q] %s", name);
		}
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			(void)strnfmt(nam, sizeof(nam), "[U] %s", name);
		}
		else
		{
			(void)strnfmt(nam, sizeof(nam), "The %s", name);
		}


		/* Level */
		(void)strnfmt(lev, sizeof(lev), "%d", r_ptr->level);

		/* Rarity */
		(void)strnfmt(rar, sizeof(rar), "%d", r_ptr->rarity);

		/* Speed */
		(void)strnfmt(spd, sizeof(spd), "%+d", (r_ptr->speed - 110));


		/* Armor Class */
		(void)strnfmt(ac, sizeof(ac), "%d", r_ptr->ac);

		/* Hitpoints */
		if (r_ptr->flags1 & (RF1_FIXED_HPS))
		{
			(void)strnfmt(hp, sizeof(hp), "%d", (int)r_ptr->hitpoints);
		}
		else
		{
			(void)strnfmt(hp, sizeof(hp), "~%d", (int)r_ptr->hitpoints);
		}


		/* Experience */
		(void)strnfmt(exp, sizeof(exp), "%ld", (long)(r_ptr->mexp));

		/* Hack -- use visual instead */
		(void)strnfmt(exp, sizeof(exp), "%s '%c'", attr_to_text(r_ptr->d_attr),
		        r_ptr->d_char);

		/* Dump the info */
		x_fprintf(fff, encoding, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			nam, lev, rar, spd, hp, ac, exp);
	}

	/* End it */
	fprintf(fff, "\n");

	/* Free the "who" array */
	FREE(who);

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Worked */
	msg_print("Successfully created a spoiler file.");
}


/*
 * Create a spoiler file for monsters
 * smchorse@ringer.cs.utsa.edu (Shawn McHorse)
 */
static void spoil_mon_info(cptr fname)
{
	char buf[1024];
	int i, n;
	u16b why = 2;
	u16b *who;
	int count = 0;


	/* Build the filename */
	(void)path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Full Monster");

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[count++] = (u16b)i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, count);

	/*
	 * List all monsters in order.
	 */
	for (n = 0; n < count; n++)
	{
		int r_idx = who[n];
		monster_race *r_ptr = &r_info[r_idx];

		/* Prefix */
		if (r_ptr->flags1 & RF1_QUESTOR)
		{
			text_out("[Q] ");
		}
		else if (r_ptr->flags1 & RF1_UNIQUE)
		{
			text_out("[U] ");
		}
		else
		{
			text_out("The ");
		}

		/* Name */
		(void)strnfmt(buf, sizeof(buf), "%s  (", (r_name + r_ptr->name));	/* ---)--- */
		text_out(buf);

		/* Color */
		text_out(attr_to_text(r_ptr->d_attr));

		/* Symbol --(-- */
		(void)strnfmt(buf, sizeof(buf), " '%c')\n", r_ptr->d_char);
		text_out(buf);


		/* Indent */
		(void)strnfmt(buf, sizeof(buf), "=== ");
		text_out(buf);

		/* Number */
		(void)strnfmt(buf, sizeof(buf), "Num:%d  ", r_idx);
		text_out(buf);

		/* Level */
		(void)strnfmt(buf, sizeof(buf), "Lev:%d  ", r_ptr->level);
		text_out(buf);

		/* Rarity */
		(void)strnfmt(buf, sizeof(buf), "Rar:%d  ", r_ptr->rarity);
		text_out(buf);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			(void)strnfmt(buf, sizeof(buf), "Spd:+%d  ", (r_ptr->speed - 110));
		}
		else
		{
			(void)strnfmt(buf, sizeof(buf), "Spd:-%d  ", (110 - r_ptr->speed));
		}
		text_out(buf);

		/* Hitpoints */
		if (r_ptr->flags1 & (RF1_FIXED_HPS))
		{
			(void)strnfmt(buf, sizeof(buf), "%d", r_ptr->hitpoints);
		}
		else
		{
			(void)strnfmt(buf, sizeof(buf), "~%d", r_ptr->hitpoints);
		}
		text_out(buf);

		/* Armor Class */
		(void)strnfmt(buf, sizeof(buf), "Ac:%d  ", r_ptr->ac);
		text_out(buf);

		/* Experience */
		(void)strnfmt(buf, sizeof(buf), "Exp:%ld\n", (long)(r_ptr->mexp));
		text_out(buf);

		/* Describe */
		describe_monster(r_idx, TRUE);

		/* Terminate the entry */
		text_out("\n");
	}

	/* Free the "who" array */
	FREE(who);

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	msg_print("Successfully created a spoiler file.");
}




/*
 * Forward declare
 */
extern void do_cmd_spoilers(void);

/*
 * Create Spoiler files
 */
void do_cmd_spoilers(void)
{
	int ch;


	/* Save screen */
	screen_save(TRUE);


	/* Drop priv's */
	safe_setuid_drop();


	/* Interact */
	while (TRUE)
	{
		/* Clear screen */
		(void)Term_clear();

		/* Info */
		prt(format("Create a spoiler file (appears in the .%s%s%s%s' directory).",
			PATH_SEP, "lib", PATH_SEP, "info"), 2, 0);

		/* Prompt for a file */
		prt("(1) Brief Object Info   (obj-desc.spo)", 5, 5);
		prt("(2) Brief Artifact Info (artifact.spo)", 6, 5);
		prt("(3) Brief Monster Info  (mon-desc.spo)", 7, 5);
		prt("(4) Full Monster Info   (mon-info.spo)", 8, 5);

		prt("(5) See what objects appear on this level  (obj-gen.spo)", 10, 5);
		prt("(6) See what monsters appear on this level (mon-gen.spo)", 11, 5);

		/* Prompt */
		prt("Command:", 12, 0);

		/* Get a choice */
		ch = inkey(FALSE);

		/* Escape */
		if (ch == ESCAPE)
		{
			break;
		}

		/* Option (1) */
		else if (ch == '1')
		{
			spoil_obj_desc("obj-desc.spo");
		}

		/* Option (2) */
		else if (ch == '2')
		{
			spoil_artifact("artifact.spo");
		}

		/* Option (3) */
		else if (ch == '3')
		{
			spoil_mon_desc("mon-desc.spo");
		}

		/* Option (4) */
		else if (ch == '4')
		{
			spoil_mon_info("mon-info.spo");
		}

		/* Option (5) */
		else if (ch == '5')
		{
			spoil_obj_gen("obj-gen.spo");
		}

		/* Option (6) */
		else if (ch == '6')
		{
			spoil_mon_gen("mon-gen.spo");
		}

		/* Oops */
		else
		{
			bell("Illegal command for spoilers!");
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Grab priv's */
	safe_setuid_grab();


	/* Load screen */
	screen_load();
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


