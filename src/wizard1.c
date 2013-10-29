/* File: wizard1.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "z-file.h"
#include "cmds.h"

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
 * Write a line to the spoiler file and then "underline" it with hypens
 */
static void spoiler_underline(cptr str, char c)
{
	text_out(str);
	text_out("\n");
	spoiler_out_n_chars(strlen(str), c);
	text_out("\n");
}



/*
 * Item Spoilers by Ben Harrison (benh@phial.com)
 */


/*
 * The basic items categorized by type
 */
static const grouper group_item[] =
{
	{ TV_SHOT,		"Ammo" },
	{ TV_ARROW,		  NULL },
	{ TV_BOLT,		  NULL },

	{ TV_BOW,		"Bows" },

	{ TV_SWORD,		"Weapons" },
	{ TV_POLEARM,	  NULL },
	{ TV_HAFTED,	  NULL },
	{ TV_DIGGING,	  NULL },

	{ TV_SOFT_ARMOR,	"Armour (Body)" },
	{ TV_HARD_ARMOR,	  NULL },
	{ TV_DRAG_ARMOR,	  NULL },

	{ TV_CLOAK,		"Armour (Misc)" },
	{ TV_SHIELD,	  NULL },
	{ TV_HELM,		  NULL },
	{ TV_CROWN,		  NULL },
	{ TV_GLOVES,	  NULL },
	{ TV_BOOTS,		  NULL },

	{ TV_AMULET,	"Amulets" },
	{ TV_RING,		"Rings" },

	{ TV_SCROLL,	"Scrolls" },
	{ TV_POTION,	"Potions" },
	{ TV_FOOD,		"Food" },

	{ TV_ROD,		"Rods" },
	{ TV_WAND,		"Wands" },
	{ TV_STAFF,		"Staffs" },

	{ TV_MAGIC_BOOK,	"Books (Mage)" },
	{ TV_PRAYER_BOOK,	"Books (Priest)" },
	{ TV_NEWM_BOOK,	    "Books (Nature)" },
	{ TV_LUCK_BOOK,	    "Books (Chance)" },
	{ TV_CHEM_BOOK,	    "Books (Alchemy)" },
	{ TV_DARK_BOOK,	    "Books (Witchcraft)" },
	/*{ TV_MIND_BOOK,	    "Books (Mind Powers)" },*/

	{ TV_CHEST,		"Chests" },

	{ TV_SPIKE,		"Various" },
	{ TV_LITE,		  NULL },
	{ TV_FLASK,		  NULL },
	{ TV_JUNK,		  NULL },
	{ TV_BOTTLE,	  NULL },
	{ TV_SKELETON,	  NULL },

	{ 0, "" }
};





/*
 * Describe the kind
 */
static void kind_info(char *buf, size_t buf_len,
                      char *dam, size_t dam_len,
                      char *wgt, size_t wgt_len,
                      int *lev, s32b *val, int k)
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

	/* Cancel bonuses */
	i_ptr->pval = 0;
	i_ptr->to_a = 0;
	i_ptr->to_h = 0;
	i_ptr->to_d = 0;


	/* Level */
	(*lev) = k_ptr->level;

	/* Make known */
	i_ptr->ident |= (IDENT_KNOWN);
	
	/* Value */
	(*val) = object_value(i_ptr);



	/* Description (too brief) */
	if (buf)
		object_desc_spoil(buf, buf_len, i_ptr, FALSE, 0);

	/* Weight */
	if (wgt)
		strnfmt(wgt, wgt_len, "%3d.%d", i_ptr->weight / 10, i_ptr->weight % 10);

	/* Hack */
	if (!dam)
		return;

	/* Misc info */
	dam[0] = '\0';

	/* Damage */
	switch (i_ptr->tval)
	{
		/* Bows */
		case TV_BOW:
		{
			break;
		}

		/* Ammo */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			strnfmt(dam, dam_len, "%dd%d", i_ptr->dd, i_ptr->ds);
			break;
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			strnfmt(dam, dam_len, "%dd%d", i_ptr->dd, i_ptr->ds);
			break;
		}

		/* Armour */
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
			strnfmt(dam, dam_len, "%d", i_ptr->ac);
			break;
		}
	}
}


/*
 * Create a spoiler file for items
 */
static void spoil_obj_desc(cptr fname)
{
	int i, k, s, t, n = 0;

	u16b who[200];

	char buf[1024];

	char wgt[80];
	char dam[80];

	cptr format = "%-51s  %7s%6s%4s%9s\n";

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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


	/* Header */
	fprintf(fff, "Spoiler File -- Basic Items (%s)\n\n\n", VERSION_STRING);

	/* More Header */
	fprintf(fff, format, "Description", "Dam/AC", "Wgt", "Lev", "Cost");
	fprintf(fff, format, "----------------------------------------",
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

					kind_info(NULL, 0, NULL, 0, NULL, 0, &e1, &t1, who[i1]);
					kind_info(NULL, 0, NULL, 0, NULL, 0, &e2, &t2, who[i2]);

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
				kind_info(buf, sizeof(buf), dam, sizeof(dam), wgt, sizeof(wgt), &e, &v, who[s]);

				/* Dump it */
				fprintf(fff, "  %-51s%7s%6s%4d%9ld\n",
				        buf, dam, wgt, e, (long)(v));
			}

			/* Start a new set */
			n = 0;

			/* Notice the end */
			if (!group_item[i].tval) break;

			/* Start a new set */
			fprintf(fff, "\n\n%s\n\n", group_item[i].name);
		}

		/* Get legal item types */
		for (k = 1; k < z_info->k_max; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip wrong tval's */
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
 * Artifact Spoilers by: randy@PICARD.tamu.edu (Randy Hutson)
 *
 * (Mostly) rewritten in 2002 by Andrew Sidwell and Robert Ruehlmann.
 */


/*
 * The artifacts categorized by type
 */
static const grouper group_artifact[] =
{
	{ TV_SWORD,         "Edged Weapons" },
	{ TV_POLEARM,       "Polearms" },
	{ TV_HAFTED,        "Hafted Weapons" },
	{ TV_BOW,           "Bows" },
	{ TV_DIGGING,       "Diggers" },

	{ TV_SOFT_ARMOR,    "Body Armor" },
	{ TV_HARD_ARMOR,    NULL },
	{ TV_DRAG_ARMOR,    NULL },

	{ TV_CLOAK,         "Cloaks" },
	{ TV_SHIELD,        "Shields" },
	{ TV_HELM,          "Helms/Crowns" },
	{ TV_CROWN,         NULL },
	{ TV_GLOVES,        "Gloves" },
	{ TV_BOOTS,         "Boots" },

	{ TV_LITE,          "Light Sources" },
	{ TV_AMULET,        "Amulets" },
	{ TV_RING,          "Rings" },

	{ 0, NULL }
};


/*
 * Hack -- Create a "forged" artifact
 */
bool make_fake_artifact(object_type *o_ptr, byte name1)
{
	int i;

	artifact_type *a_ptr = &a_info[name1];


	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return FALSE;

	/* Get the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artifact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->name1 = name1;

	/* Extract the fields */
	o_ptr->pval = a_ptr->pval;
	o_ptr->ac = a_ptr->ac;
	o_ptr->dd = a_ptr->dd;
	o_ptr->ds = a_ptr->ds;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->to_d = a_ptr->to_d;
	o_ptr->weight = a_ptr->weight;

	/* Hack -- extract the "cursed" flag */
	if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) o_ptr->ident |= (IDENT_CURSED);

	/* Success */
	return (TRUE);
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


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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

	/* Set object_info_out() hook */
	object_info_out_flags = object_flags;

	/* Dump the header */
	spoiler_underline(format("Artifact Spoilers for %s %s",
	                         VERSION_NAME, VERSION_STRING), '=');

	/* List the artifacts by tval */
	for (i = 0; group_artifact[i].tval; i++)
	{
		/* Write out the group title */
		if (group_artifact[i].name)
		{
			spoiler_blanklines(2);
			spoiler_underline(group_artifact[i].name, '=');
			spoiler_blanklines(1);
		}

		/* Now search through all of the artifacts */
		for (j = 1; j < z_info->a_max; ++j)
		{
			artifact_type *a_ptr = &a_info[j];
			char buf[80];

			/* We only want objects in the current group */
			if (a_ptr->tval != group_artifact[i].tval) continue;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Wipe the object */
			object_wipe(i_ptr);

			/* Attempt to "forge" the artifact */
			if (!make_fake_artifact(i_ptr, (byte)j)) continue;

			/* Grab artifact name */
			object_desc_spoil(buf, sizeof(buf), i_ptr, TRUE, 1);

			/* Print name and underline */
			spoiler_underline(buf, '-');

			/* Write out the artifact description to the spoiler file */
			object_info_out(i_ptr);

			/*
			 * Determine the minimum depth an artifact can appear, its rarity,
			 * its weight, and its value in gold pieces.
			 */
			text_out(format("\nLevel %u, Rarity %u, %d.%d lbs, %ld AU\n",
			                a_ptr->level, a_ptr->rarity, (a_ptr->weight / 10),
			                (a_ptr->weight % 10), ((long)a_ptr->cost)));

			/* Terminate the entry */
			spoiler_blanklines(2);
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

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	char exp[80];

	u16b *who;
	u16b why = 2;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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

	/* Dump the header */
	fprintf(fff, "Monster Spoilers for %s Version %s\n",
	        VERSION_NAME, VERSION_STRING);
	fprintf(fff, "-------------------------------------------------------\n");
	fprintf(fff, "[Q] Quest monster (Unique)                            \n");
	fprintf(fff, "[U] Unique                                            \n");
	fprintf(fff, "[H] Helper monster (usually only appears when called) \n");
	fprintf(fff, "[T] Theme-only monster (only appears on themed levels)\n");
	fprintf(fff, "-------------------------------------------------------\n\n");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
	        "Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
	        "----", "---", "---", "---", "--", "--", "-----------");

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Scan the monsters (except the ghost) */
	for (i = 1; i < z_info->r_max - 1; i++)
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
			strnfmt(nam, sizeof(nam), "[Q] %s", name);
		}
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			strnfmt(nam, sizeof(nam), "[U] %s", name);
		}
		else if (r_ptr->flags3 & (RF3_HELPER))
		{
			strnfmt(nam, sizeof(nam), "[H] The %s", name);
		}
		else if (r_ptr->flags7 & (RF7_THEME_ONLY))
		{
			strnfmt(nam, sizeof(nam), "[T] The %s", name);
		}
		else
		{
			strnfmt(nam, sizeof(nam), "The %s", name);
		}

		/* Level */
		strnfmt(lev, sizeof(lev), "%d", r_ptr->level);

		/* Rarity */
		strnfmt(rar, sizeof(rar), "%d", r_ptr->rarity);

		/* Speed */
		if (r_ptr->speed >= 110)
			strnfmt(spd, sizeof(spd), "+%d", (r_ptr->speed - 110));
		else
			strnfmt(spd, sizeof(spd), "-%d", (110 - r_ptr->speed));

		/* Armor Class */
		strnfmt(ac, sizeof(ac), "%d", r_ptr->ac);

		/* Hitpoints */
		if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
			strnfmt(hp, sizeof(hp), "%d", r_ptr->hdice * r_ptr->hside);
		else
			strnfmt(hp, sizeof(hp), "%dd%d", r_ptr->hdice, r_ptr->hside);


		/* Experience */
		strnfmt(exp, sizeof(exp), "%ld", (long)(r_ptr->mexp));

		/* Hack -- use visual instead */
		strnfmt(exp, sizeof(exp), "%s '%c'", attr_to_text(r_ptr->d_attr), r_ptr->d_char);

		/* Dump the info */
		fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
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
 * Create a stats spoiler file for monsters
 */
static void spoil_mon_stat(cptr fname)
{
	int i, n = 0, prevlev = 0;

	char buf[1024];

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	/* char exp[80]; */
	char siz[80];
	char stl[80];
	char vis[80];
	char alrt[80];
	char srg[80];

	u16b *who;
	u16b why = 2;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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

	/* Dump the header */
	fprintf(fff, "Monster Spoilers for %s Version %s\n",
	        VERSION_NAME, VERSION_STRING);
	fprintf(fff, "-------------------------------------------------------\n");
	fprintf(fff, "[Q] Quest monster (Unique)                            \n");
	fprintf(fff, "[U] Unique                                            \n");
	fprintf(fff, "[H] Helper monster (usually only appears when called) \n");
	fprintf(fff, "[T] Theme-only monster (only appears on themed levels)\n");
	fprintf(fff, "-------------------------------------------------------\n\n");

	/* an abbreviation key */
	fprintf(fff, "Rar=rarity, Stl=stealth, Spd=speed, Ac=armor class, Sz=size\n");
	fprintf(fff, " Vis=vision range, Al=alertness, SRg=spell range\n\n");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%4s%6s%8s%6s%4s%4s%6s  %4.4s\n",
	        "Name", "Lev", "Rar", "Stl", "Spd", "Hp ", "Ac", "Sz", "Vis", "Al", "SRg");
	fprintf(fff, "%-40.40s%4s%4s%4s%6s%8s%6s%4s%4s%6s  %4.4s\n",
	        "----", "---", "---", "---", "---", "----", "--", "--", "---", "--", "---");

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Scan the monsters (except the ghost) */
	for (i = 1; i < z_info->r_max - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];
			
		/* don't list NONmonsters */
		if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;

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
		char rst[80];
		bool anyr = FALSE;

		if (r_ptr->level > prevlev)
		{
			/* Dump the header */
			fprintf(fff, "\n%-40.40s%4s%4s%4s%6s%8s%6s%4s%5s%5s  %4.4s\n",
		        "Name", "Lev", "Rar", "Stl", "Spd", "Hp ", "Ac", "Sz", "Vis", "Al", "SRg");
			fprintf(fff, "%-40.40s%4s%4s%4s%6s%8s%6s%4s%5s%5s  %4.4s\n",
		        "----", "---", "---", "---", "---", "----", "--", "--", "---", "--", "---");
         }

		prevlev = r_ptr->level;

		/* Get the "name" */
		if (r_ptr->flags1 & (RF1_QUESTOR))
		{
			strnfmt(nam, sizeof(nam), "[Q] %s", name);
		}
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			strnfmt(nam, sizeof(nam), "[U] %s", name);
		}
		else if (r_ptr->flags3 & (RF3_HELPER))
		{
			strnfmt(nam, sizeof(nam), "[H] The %s", name);
		}
		else if (r_ptr->flags7 & (RF7_THEME_ONLY))
		{
			strnfmt(nam, sizeof(nam), "[T] The %s", name);
		}
		else
		{
			strnfmt(nam, sizeof(nam), "The %s", name);
		}

		/* Level */
		strnfmt(lev, sizeof(lev), "%d", r_ptr->level);

		/* Rarity */
		strnfmt(rar, sizeof(rar), "%d", r_ptr->rarity);

		/* Stealth */
		strnfmt(stl, sizeof(stl), "%d", r_ptr->stealth);

		/* Speed */
		if (r_ptr->speed >= 110)
			strnfmt(spd, sizeof(spd), "+%d", (r_ptr->speed - 110));
		else
			strnfmt(spd, sizeof(spd), "-%d", (110 - r_ptr->speed));

		/* Armor Class */
		strnfmt(ac, sizeof(ac), "%d", r_ptr->ac);

		/* Hitpoints */
		if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
			strnfmt(hp, sizeof(hp), "%d", r_ptr->hdice * r_ptr->hside);
		else
			strnfmt(hp, sizeof(hp), "%dd%d", r_ptr->hdice, r_ptr->hside);

		/* Size */
		strnfmt(siz, sizeof(siz), "%d", r_ptr->mrsize);

		/* Vision */
		strnfmt(vis, sizeof(vis), "%d", r_ptr->aaf);

		/* Alertness */
		strnfmt(alrt, sizeof(alrt), "%d", r_ptr->sleep);

		/* Spell Range */
		strnfmt(srg, sizeof(srg), "%d", r_ptr->spr);

		/* Experience */
		/* strnfmt(exp, sizeof(exp), "%ld", (long)(r_ptr->mexp)); */

		/* Hack -- use visual instead */
		/* strnfmt(exp, sizeof(exp), "%s '%c'", attr_to_text(r_ptr->d_attr), r_ptr->d_char); */

		/* "Name", "Lev", "Rar", "Stl", "Spd", "Hp", "Ac", "Sz", "Vis", "Al", "SRg"); */
		/* char siz, stl, vis, alrt, srg */

		/* Dump the info */
		fprintf(fff, "%-40.40s%4s%4s%4s%6s%8s%6s%4s%5s%5s  %4.4s\n",
		        nam, lev, rar, stl, spd, hp, ac, siz, vis, alrt, srg);

		/* Resistances */
		if ((r_ptr->Rfire) || (r_ptr->Rcold) || (r_ptr->Relec) || (r_ptr->Racid) || 
			(r_ptr->Rpois) || (r_ptr->Rlite) || (r_ptr->Rdark) || (r_ptr->Rwater) ||
			(r_ptr->Rnexus) || (r_ptr->Rsilver) || (r_ptr->Rmissile)) 
				{ fprintf(fff, "- "); anyr = TRUE; }
			
		if (r_ptr->Rfire) strnfmt(rst, sizeof(rst), "Rfire:%d  ", r_ptr->Rfire);
		if (r_ptr->Rfire) fprintf(fff, rst);
		if (r_ptr->Rcold) strnfmt(rst, sizeof(rst), "Rcold:%d  ", r_ptr->Rcold);
		if (r_ptr->Rcold) fprintf(fff, rst);
		if (r_ptr->Relec) strnfmt(rst, sizeof(rst), "Relec:%d  ", r_ptr->Relec);
		if (r_ptr->Relec) fprintf(fff, rst);
		if (r_ptr->Racid) strnfmt(rst, sizeof(rst), "Racid:%d  ", r_ptr->Racid);
		if (r_ptr->Racid) fprintf(fff, rst);
		if (r_ptr->Rpois) strnfmt(rst, sizeof(rst), "Rpois:%d  ", r_ptr->Rpois);
		if (r_ptr->Rpois) fprintf(fff, rst);
		if (r_ptr->Rlite) strnfmt(rst, sizeof(rst), "Rlite:%d  ", r_ptr->Rlite);
		if (r_ptr->Rlite) fprintf(fff, rst);
		if (r_ptr->Rdark) strnfmt(rst, sizeof(rst), "Rdark:%d  ", r_ptr->Rdark);
		if (r_ptr->Rdark) fprintf(fff, rst);
		if (r_ptr->Rwater) strnfmt(rst, sizeof(rst), "Rwater:%d  ", r_ptr->Rwater);
		if (r_ptr->Rwater) fprintf(fff, rst);
		if (r_ptr->Rnexus) strnfmt(rst, sizeof(rst), "Rnexus:%d  ", r_ptr->Rnexus);
		if (r_ptr->Rnexus) fprintf(fff, rst);
		if (r_ptr->Rsilver) strnfmt(rst, sizeof(rst), "Rsilver:%d  ", r_ptr->Rsilver);
		if (r_ptr->Rsilver) fprintf(fff, rst);
		if (r_ptr->Rmissile) strnfmt(rst, sizeof(rst), "Rmissile:%d  ", r_ptr->Rmissile);
		if (r_ptr->Rmissile) fprintf(fff, rst);
		if (anyr) fprintf(fff, "\n");
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
 * Create a spoiler file for monsters (by theme)
 * (based on spoil_mon_desc()
 */
static void spoil_mon_theme(cptr fname)
{
	int i, i2;

	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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

	/* Dump the header */
	fprintf(fff, "Themed Monster Spoilers for %s Version %s\n",
	        VERSION_NAME, VERSION_STRING);
	fprintf(fff, "-------------------------------------------------------\n");
	fprintf(fff, "[U] Unique                                            \n");
	fprintf(fff, "[H] Helper monster (usually only appears when called) \n");
	fprintf(fff, "[T] Theme-only monster (only appears on themed levels)\n");
	fprintf(fff, "-------------------------------------------------------\n\n");
	
	/* do it once for each of the 15 themes */
	for (i2 = 1; i2 < 17; i2++)
	{
		u16b *who;
		u16b why = 2;
		int n = 0, tnum = 0, uniqnum = 0, postwin = 0;
		char nam[80];
		char lev[80];
		char rar[80];
		char spd[80];
		char ac[80];
		char hp[80];
		char exp[80];
		
		if (i2 == 5) i2++; /* theme 5 was removed/combined with another so skip it */

		if (i2 > 1) fprintf(fff, "\n"); /* gap between theme lists */
		switch (i2) /* theme title */
		{
			case 1:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Cold Forest Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 2:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Fairy Forest Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 3:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Icky Place Theme (acid)", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 4:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Volcano Theme (fire)", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
#if 0
			case 5:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Earth Cave Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
#endif
			case 6:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Cloud Giants vs Titans Theme (air)", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 7:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Full Moon Theme (lightning)", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 8:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Ancient Temple Ruins Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 9:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Swamp Theme (water)", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 10:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Dwarf Mine Theme (earth)", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 11:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Bug Cave / Dark Elf Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 12:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Silver Land / Grepse Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 13:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Dark Fairy City / Nightmare Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 14:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Hell Halls Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 15:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Castle Barracks Theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
			case 16:
			{
				fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
					"Monsters not in a theme", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
                break;
			}
		}
		
		/* Dump the header */
		/* fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		        "Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info"); */
		fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		        "----", "---", "---", "---", "--", "--", "-----------");

		/* Allocate the "who" array */
		C_MAKE(who, z_info->r_max, u16b);

		/* Scan the monsters (except the ghost) */
		for (i = 1; i < z_info->r_max - 1; i++)
		{
			bool noincl = FALSE;
			monster_race *r_ptr = &r_info[i];
			
			/* don't list NONmonsters */
			if (r_ptr->flags7 & (RF7_NONMONSTER)) continue;
			
			switch (i2) /* require correct theme */
			{
				case 1:
				{
					if (!(r_ptr->flags7 & (RF7_CFOREST))) noincl = TRUE;
                    break;
				}
				case 2:
				{
					if (!(r_ptr->flags7 & (RF7_FFOREST))) noincl = TRUE;
                    break;
				}
				case 3:
				{
					if (!(r_ptr->flags7 & (RF7_ICKY_PLACE))) noincl = TRUE;
                    break;
				}
				case 4:
				{
					if (!(r_ptr->flags7 & (RF7_VOLCANO))) noincl = TRUE;
                    break;
				}
				case 5:
				{
					if (!(r_ptr->flags7 & (RF7_EARTHY_CAVE))) noincl = TRUE;
                    break;
				}
				case 6:
				{
					if (!(r_ptr->flags7 & (RF7_WINDY_CAVE))) noincl = TRUE;
                    break;
				}
				case 7:
				{
					if (!(r_ptr->flags7 & (RF7_FULL_MOON))) noincl = TRUE;
                    break;
				}
				case 8:
				{
					if (!(r_ptr->flags7 & (RF7_CASTLE))) noincl = TRUE;
                    break;
				}
				case 9:
				{
					if (!(r_ptr->flags7 & (RF7_SWAMP))) noincl = TRUE;
                    break;
				}
				case 10:
				{
					if (!(r_ptr->flags7 & (RF7_DWARF_MINE))) noincl = TRUE;
                    break;
				}
				case 11:
				{
					if (!(r_ptr->flags7 & (RF7_BUG_CAVE))) noincl = TRUE;
                    break;
				}
				case 12:
				{
					if (!(r_ptr->flags7 & (RF7_GREPSE))) noincl = TRUE;
                    break;
				}
				case 13:
				{
					if (!(r_ptr->flags7 & (RF7_DARK_CITY))) noincl = TRUE;
                    break;
				}
				case 14:
				{
					if (!(r_ptr->flags7 & (RF7_HELL_HALL))) noincl = TRUE;
                    break;
				}
				case 15:
				{
					if (!(r_ptr->flags7 & (RF7_BARRACKS))) noincl = TRUE;
                    break;
				}
				case 16:
				{
					if ((r_ptr->flags7 & (RF7_BARRACKS)) || (r_ptr->flags7 & (RF7_HELL_HALL)) ||
                    (r_ptr->flags7 & (RF7_GREPSE)) || (r_ptr->flags7 & (RF7_DARK_CITY)) ||
                    (r_ptr->flags7 & (RF7_BUG_CAVE)) || (r_ptr->flags7 & (RF7_DWARF_MINE)) ||
                    (r_ptr->flags7 & (RF7_SWAMP)) || (r_ptr->flags7 & (RF7_CASTLE)) ||
                    (r_ptr->flags7 & (RF7_FULL_MOON)) || (r_ptr->flags7 & (RF7_WINDY_CAVE)) ||
                    (r_ptr->flags7 & (RF7_VOLCANO)) || (r_ptr->flags7 & (RF7_ICKY_PLACE)) ||
                    (r_ptr->flags7 & (RF7_FFOREST)) || (r_ptr->flags7 & (RF7_CFOREST)))
						noincl = TRUE;
					break;
				}
			}
			if (noincl) continue;
			
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1)) uniqnum++;
			if (r_ptr->level > 100) postwin++;

			/* Use that monster */
			if (r_ptr->name) who[n++] = (u16b)i;
			tnum++;
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

			/* Get the name */
			if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->maxpop == 1))
			{
				strnfmt(nam, sizeof(nam), "[U] %s", name);
			}
			else if (r_ptr->flags3 & (RF3_HELPER))
			{
				strnfmt(nam, sizeof(nam), "[H] The %s", name);
			}
			else if (r_ptr->flags7 & (RF7_THEME_ONLY))
			{
				strnfmt(nam, sizeof(nam), "[T] The %s", name);
			}
			else
			{
				strnfmt(nam, sizeof(nam), "The %s", name);
			}

			/* Level */
			strnfmt(lev, sizeof(lev), "%d", r_ptr->level);

			/* Rarity */
			strnfmt(rar, sizeof(rar), "%d", r_ptr->rarity);

			/* Speed */
			if (r_ptr->speed >= 110)
				strnfmt(spd, sizeof(spd), "+%d", (r_ptr->speed - 110));
			else
				strnfmt(spd, sizeof(spd), "-%d", (110 - r_ptr->speed));

			/* Armor Class */
			strnfmt(ac, sizeof(ac), "%d", r_ptr->ac);

			/* Hitpoints */
			if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
				strnfmt(hp, sizeof(hp), "%d", r_ptr->hdice * r_ptr->hside);
			else
				strnfmt(hp, sizeof(hp), "%dd%d", r_ptr->hdice, r_ptr->hside);


			/* Experience */
			strnfmt(exp, sizeof(exp), "%ld", (long)(r_ptr->mexp));

			/* Hack -- use visual instead */
			strnfmt(exp, sizeof(exp), "%s '%c'", attr_to_text(r_ptr->d_attr), r_ptr->d_char);

			/* Dump the info */
			fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			        nam, lev, rar, spd, hp, ac, exp);
		}
		
		if (i2 == 16)
		{
			fprintf(fff, "%-26.26s%1d%1s%11s%3d%13s  %2.2d\n",
				"(number not in a theme: ", tnum, ".", "Uniques:", uniqnum, "dL100+ :", postwin);
		}
		else
		{
			fprintf(fff, "%-26.26s%1d%1s%17s%3d%13s  %2.2d\n",
				"(number in this theme: ", tnum, ".", "minus uniques:", tnum-uniqnum, "dL100+ :", postwin);
		}

		/* Free the "who" array */
		FREE(who);
	}

	/* End it */
	fprintf(fff, "\n");


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
 * Monster spoilers originally by: smchorse@ringer.cs.utsa.edu (Shawn McHorse)
 */


/*
 * Create a spoiler file for monsters (-SHAWN-)
 */
static void spoil_mon_info(cptr fname)
{
	char buf[1024];
	int i, n;
	u16b why = 2;
	u16b *who;
	int count = 0;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, fname);

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
	strnfmt(buf, sizeof(buf), "Monster Spoilers for %s Version %s\n\n",
	        VERSION_NAME, VERSION_STRING);
	text_out(buf);
	text_out("-------------------------------------------------------------\n");
	text_out("[Q] Quest monster (Unique)                            \n");
	text_out("[U] Unique                                            \n");
	text_out("[H] Helper monster (usually only appears when called) \n");
	text_out("[T] Theme-only monster (only appears on themed levels)\n");
	text_out("-------------------------------------------------------------\n\n");

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
	 * List all monsters in order (except the ghost).
	 */
	for (n = 0; n < count; n++)
	{
		int r_idx = who[n];
		monster_race *r_ptr = &r_info[r_idx];
#if thiswasmoved
		bool anyr = FALSE;
#endif

		/* Prefix */
		if (r_ptr->flags1 & RF1_QUESTOR)
		{
			text_out("[Q] ");
		}
		else if (r_ptr->flags1 & RF1_UNIQUE)
		{
			text_out("[U] ");
		}
        else if (r_ptr->flags3 & (RF3_HELPER))
		{
			text_out("[H] The ");
		}
		else if (r_ptr->flags7 & (RF7_THEME_ONLY))
		{
			text_out("[T] The ");
		}
		else
		{
			text_out("The ");
		}

		/* Name */
		strnfmt(buf, sizeof(buf), "%s  (", (r_name + r_ptr->name));	/* ---)--- */
		text_out(buf);

		/* Color */
		text_out(attr_to_text(r_ptr->d_attr));

		/* Symbol --(-- */
		strnfmt(buf, sizeof(buf), " '%c')\n", r_ptr->d_char);
		text_out(buf);


		/* Indent */
		text_out("=== ");

		/* Number */
		strnfmt(buf, sizeof(buf), "Num:%d  ", r_idx);
		text_out(buf);

		/* Level */
		strnfmt(buf, sizeof(buf), "Lev:%d  ", r_ptr->level);
		text_out(buf);

		/* Rarity */
		strnfmt(buf, sizeof(buf), "Rar:%d  ", r_ptr->rarity);
		text_out(buf);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			strnfmt(buf, sizeof(buf), "Spd:+%d  ", (r_ptr->speed - 110));
		}
		else
		{
			strnfmt(buf, sizeof(buf), "Spd:-%d  ", (110 - r_ptr->speed));
		}
		text_out(buf);

		/* Hitpoints */
		if ((r_ptr->flags1 & RF1_FORCE_MAXHP) || (r_ptr->hside == 1))
		{
			strnfmt(buf, sizeof(buf), "Hp:%d  ", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			strnfmt(buf, sizeof(buf), "Hp:%dd%d  ", r_ptr->hdice, r_ptr->hside);
		}
		text_out(buf);

		/* Armor Class */
		strnfmt(buf, sizeof(buf), "Ac:%d  ", r_ptr->ac);
		text_out(buf);

		/* Experience */
		strnfmt(buf, sizeof(buf), "Exp:%ld\n", (long)(r_ptr->mexp));
		text_out(buf);

#if thiswasmoved
		/* Resistances */
		if ((r_ptr->Rfire) || (r_ptr->Rcold) || (r_ptr->Relec) || (r_ptr->Racid) || 
			(r_ptr->Rpois) || (r_ptr->Rlite) || (r_ptr->Rdark) || (r_ptr->Rwater) ||
			(r_ptr->Rnexus) || (r_ptr->Rsilver) || (r_ptr->Rmissile)) text_out("= ");
			
		if (r_ptr->Rfire) strnfmt(buf, sizeof(buf), "Rfire:%d  ", r_ptr->Rfire);
		if (r_ptr->Rfire) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rcold) strnfmt(buf, sizeof(buf), "Rcold:%d  ", r_ptr->Rcold);
		if (r_ptr->Rcold) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Relec) strnfmt(buf, sizeof(buf), "Relec:%d  ", r_ptr->Relec);
		if (r_ptr->Relec) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Racid) strnfmt(buf, sizeof(buf), "Racid:%d  ", r_ptr->Racid);
		if (r_ptr->Racid) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rpois) strnfmt(buf, sizeof(buf), "Rpois:%d  ", r_ptr->Rpois);
		if (r_ptr->Rpois) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rlite) strnfmt(buf, sizeof(buf), "Rlite:%d  ", r_ptr->Rlite);
		if (r_ptr->Rlite) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rdark) strnfmt(buf, sizeof(buf), "Rdark:%d  ", r_ptr->Rdark);
		if (r_ptr->Rdark) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rwater) strnfmt(buf, sizeof(buf), "Rwater:%d  ", r_ptr->Rwater);
		if (r_ptr->Rwater) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rnexus) strnfmt(buf, sizeof(buf), "Rnexus:%d  ", r_ptr->Rnexus);
		if (r_ptr->Rnexus) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rsilver) strnfmt(buf, sizeof(buf), "Rsilver:%d  ", r_ptr->Rsilver);
		if (r_ptr->Rsilver) { text_out(buf); anyr = TRUE; }
		if (r_ptr->Rmissile) strnfmt(buf, sizeof(buf), "Rmissile:%d  ", r_ptr->Rmissile);
		if (r_ptr->Rmissile) { text_out(buf); anyr = TRUE; }
		if (anyr) text_out("\n");
#endif

		/* Describe */
		describe_monster(r_idx, TRUE, 0);

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
 * Create Spoiler files
 */
void do_cmd_spoilers(void)
{
	char ch;

	/* Save screen */
	screen_save();

	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Info */
		prt("Create a spoiler file.", 2, 0);

		/* Prompt for a file */
		prt("(1) Brief Object Info (obj-desc.spo)", 5, 5);
		prt("(2) Brief Artifact Info (artifact.spo)", 6, 5);
		prt("(3) Brief Monster Info (mon-desc.spo)", 7, 5);
		prt("(4) Full Monster Info (mon-info.spo)", 8, 5);
		prt("(5) Brief Monster Theme Info (mon-theme.spo)", 9, 5);
		prt("(6) Monster Stat Info (mon-stat.spo)", 10, 5);

		/* Prompt */
		prt("Command: ", 12, 0);

		/* Get a choice */
		ch = inkey();

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
			spoil_mon_theme("mon-theme.spo");
		}

		/* Option (5) */
		else if (ch == '6')
		{
			spoil_mon_stat("mon-stat.spo");
		}

		/* Oops */
		else
		{
			bell("Illegal command for spoilers!");
		}

		/* Flush messages */
		message_flush();
	}


	/* Load screen */
	screen_load();
}


#endif
