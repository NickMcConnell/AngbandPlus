/* File: object1.c */

/*
 * Object flavours, learning.  Reset visual preferences, Get object pvals and
 * attributes.  Object names.  Object indexes and labels, what slot an object
 * likes.  Display the inventory, equipment, and floor.  Object display
 * subwindows.  Inscription tags.  Select objects.
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
 * Syllables for scrolls in the "grast-speech".
 */
static cptr syllables[] =
{
	"bro", "kurn", "runt", "mor", "dar", "un", "dal", "ell", "arg", "draa",
	"ash", "mur", "ud", "gav", "gat", "uth", "lok", "kal", "bit", "kul",
	"asp", "nuth", "baal", "grun", "tol", "peel", "warth", "cul", "buno", "oth",
	"gaf", "pad", "dok", "dur", "mag", "dok", "nun", "duk", "mu", "nont",
	"vat", "mud", "frak", "kok", "kal", "rast", "rast", "wurm", "fud",
	"rath", "mun", "arth",
	"dudul", "hoot", "sluss", "slat", "smat", "gop", "vok", "ha", "lud", "kak",
	"clot", "gul", "gad", "od", "rog", "werg", "werg", "warg", "worg"
};

/*
 * A-or-pre-pended sounds for scrolls (grast-speech)
 */
static cptr append[] =
{
	"wu", "g", "r", "t", "s", "k", "p", "n", "m",
	"u", "ast", "ad", "ar", "al", "at", "ro"
};

/*
 * Conjunction words for scrolls (grast-speech)
 */
static cptr conjunct[] =
{
	"ad", "nun", "oth", "ko", "u", "lun", "na", "got", "dad", "ast"
};


/*
 * Hold the titles of scrolls, roughly 8 to 20 characters each
 */

static char scroll_adj[MAX_SCROLL_IDX][20];


/*
 * Add a regular word.
 */
static void build_scroll_name_word(char *buf, size_t size)
{
	int max_syllables = N_ELEMENTS(syllables);
	int max_append    = N_ELEMENTS(append);

	/* Sometimes have prefixes and appended sounds */
	bool pre = one_in_(8);
	bool post = (!pre && one_in_(8));


	/* Sometimes, start with a pre-pended sound */
	if (pre)
	{
		/* Add the sound */
		(void)my_strcat(buf, append[rand_int(max_append)], size);
		pre = TRUE;

		/* Add either a '-' or a ''' */
		(void)my_strcat(buf, one_in_(3) ? "\'" : "-", size);
	}

	/* Add one or two chunks */
	(void)my_strcat(buf, syllables[rand_int(max_syllables)], size);
	if (!pre && !post)
		(void)my_strcat(buf, syllables[rand_int(max_syllables)], size);

	/* Sometimes, end with an appended sound */
	if (post)
	{
		/* Add a "-" */
		(void)my_strcat(buf, "-", size);

		/* Add the sound */
		(void)my_strcat(buf, append[rand_int(max_append)], size);
	}

	/* Insert a space */
	(void)my_strcat(buf, " ", size);
}


/*
 * Build a scroll name, using the "grast-speech".  -LM-
 */
static void build_scroll_name(int i)
{
	int j;

	int max_conjunct = N_ELEMENTS(conjunct);


	/* Get a new title */
	while (TRUE)
	{
		/* Scroll names may only be 19 characters, plus terminator */
		char buf[20];

		bool okay = TRUE;
		bool flag = FALSE;

		/* Start a new title */
		buf[0] = '\0';

		/* Sometimes start with a conjunction */
		if (one_in_(3))
		{
			(void)my_strcat(buf, conjunct[rand_int(max_conjunct)], sizeof(buf));
			(void)my_strcat(buf, " ", sizeof(buf));
			flag = TRUE;
		}

		/* Add a word */
		build_scroll_name_word(buf, sizeof(buf));

		/* Sometimes, add another word */
		if (one_in_(2))
		{
			/* Sometimes, use a conjunction first */
			if (!flag && one_in_(2))
			{
				(void)my_strcat(buf, conjunct[rand_int(max_conjunct)], sizeof(buf));
				(void)my_strcat(buf, " ", sizeof(buf));
			}
			build_scroll_name_word(buf, sizeof(buf));
		}


		/* Overwrite the trailing space, if any */
		if (buf[strlen(buf) - 1] == ' ') buf[strlen(buf) - 1] = '\0';

		/* Save the title */
		strcpy(scroll_adj[i], buf);

		/* Check for duplicate scroll titles */
		for (j = 0; j < i; j++)
		{
			/* Compare titles */
			if (!strstr(scroll_adj[j], scroll_adj[i])) continue;

			/* Not okay */
			okay = FALSE;

			/* Stop looking */
			break;
		}

		/* Break when done */
		if (okay) break;
	}
}


/*
 * Assign fixed flavors
 */
static void flavor_assign_fixed(void)
{
	int i, j;

	/* Scan all the flavors */
	for (i = 0; i < z_info->flavor_max; i++)
	{
		flavor_type *flavor_ptr = &flavor_info[i];

		/* Skip empty flavors */
		if (!flavor_ptr->tval) continue;

		/* Skip random flavors */
		if (flavor_ptr->sval == SV_ANY) continue;

		/* Scan the object list for the object this flavor belongs to */
		for (j = 0; j < z_info->k_max; j++)
		{
			/* Assign to only one object, unless "sval" is 255 */
			if ((k_info[j].tval == flavor_ptr->tval) &&
			    ((k_info[j].sval == flavor_ptr->sval) ||
			     (flavor_ptr->sval == SV_ALL)))
			{
				/* Store the flavor index */
				k_info[j].flavor = i;

				/* Usually, assign to only one object */
				if (flavor_ptr->sval != SV_ALL) break;
			}
		}
	}
}


/*
 * Assign random flavors for a given tval
 */
static void flavor_assign_random(byte tval)
{
	int i, j, k;
	int flavor_count = 0;
	int choice;

	/* Count the random flavors for the given tval */
	for (i = 0; i < z_info->flavor_max; i++)
	{
		/* Special case:  Re-use wand flavors for rods */
		if (tval == TV_ROD)
		{
			if (flavor_info[i].tval == TV_WAND)
			{
				flavor_info[i].tval = TV_ROD;
				flavor_info[i].sval = SV_ANY;
				flavor_count++;
			}
		}

		/* Usual case:  Consider only random flavors of this type */
		else if ((flavor_info[i].tval == tval) &&
		         (flavor_info[i].sval == SV_ANY))
		{
			flavor_count++;
		}
	}

	/* Scan the whole object list */
	for (i = 0; i < z_info->k_max; i++)
	{
		/* Get this object */
		object_kind *k_ptr = &k_info[i];

		/* Skip other object types */
		if (k_ptr->tval != tval) continue;

		/* Skip objects that already are flavored */
		if (k_ptr->flavor) continue;

		/* HACK - Ordinary food is "boring" */
		if ((tval == TV_FOOD) && (k_ptr->sval >= SV_FOOD_MIN_FOOD)) continue;

		/* Notice lack of flavors */
		if (!flavor_count) quit_fmt("Not enough flavors for tval %d.", tval);

		/* Select a random flavor */
		choice = rand_int(flavor_count);

		/* Loop until satisfied */
		for (k = 0; !k_ptr->flavor; k++)
		{
			/* Handle errors */
			if (k > 2) quit_fmt("Not enough flavors of the right level for tval %d, sval %d.",
				tval, k_ptr->sval);

			/* Find and store the flavor */
			for (j = 1; j < z_info->flavor_max; j++)
			{
				/* Get this flavor */
				flavor_type *flavor_ptr = &flavor_info[j];

				/* Skip other tvals */
				if (flavor_ptr->tval != tval) continue;

				/* Skip assigned flavors */
				if (flavor_ptr->sval != SV_ANY) continue;

				/* Count down to our choice */
				choice--;

				/* Skip flavors of too high a level */
				if (flavor_ptr->level > k_ptr->level) continue;

				/* Hack -- Encourage high-level flavors for high-level objects */
				if (flavor_ptr->level) choice -= 2;

				/* Count down to chosen flavour */
				if (choice <= 0)
				{
					/* Store the flavor index */
					k_ptr->flavor = j;

					/* This flavor is now assigned */
					flavor_ptr->sval = k_ptr->sval;

					/* One less flavor to choose from */
					flavor_count--;

					break;
				}
			}
		}
	}
}


/*
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, Staffs, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Essences).
 *
 * Hack -- make sure everything stays the same for each saved game
 * This is accomplished by the use of a saved "random seed", as in
 * "town_gen()".  Since no other functions are called while the special
 * seed is in effect, so this function is pretty "safe".
 */
void flavor_init(void)
{
	int i;

	/* The system may have fewer colors than the flavors want */
	if (max_system_colors < MAX_COLORS)
	{
		/* Scan the flavor list */
		for (i = 0; i < z_info->flavor_max; i++)
		{
			/* Correct colors (unless graphical) */
			if (flavor_info[i].x_attr < MAX_COLORS)
				flavor_info[i].x_attr = verify_color(flavor_info[i].x_attr);
		}
	}

	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant flavors */
	Rand_value = seed_flavor;

	/* Assign fixed flavors */
	flavor_assign_fixed();

	/* Assign random flavors */
	flavor_assign_random(TV_FOOD);
	flavor_assign_random(TV_RING);
	flavor_assign_random(TV_AMULET);
	flavor_assign_random(TV_POTION);
	flavor_assign_random(TV_SCROLL);
	flavor_assign_random(TV_STAFF);
	flavor_assign_random(TV_WAND);
	flavor_assign_random(TV_ROD);



	/* Build scroll names */
	for (i = 0; i < MAX_SCROLL_IDX; i++)
	{
		/* Build a title */
		build_scroll_name(i);
	}


	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Analyze every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		/* No flavor yields aware */
		if (!k_ptr->flavor) k_ptr->special |= (SPECIAL_AWARE);
	}
}



/*
 * Hack - make it easy for some classes of objects to be marked "easy know".  XXX
 */
void easy_know_init(void)
{
	int i;

	/* Analyze every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		/* Analyze the "tval" */
		switch (k_ptr->tval)
		{
			/* Spellbooks */
			case TV_MAGIC_BOOK:
			case TV_PRAYER_BOOK:
			case TV_NATURE_BOOK:
			case TV_DARK_BOOK:
			{
				k_ptr->flags3 |= (TR3_EASY_KNOW);
				break;
			}

			/* Simple items */
			case TV_FLASK:
			case TV_SKELETON:
			case TV_SPIKE:
			case TV_COMPONENT:
			case TV_PARCHMENT:
			case TV_BOTTLE:
			case TV_ESSENCE:
			{
				k_ptr->flags3 |= (TR3_EASY_KNOW);
				break;
			}

			case TV_JUNK:
			{
				if ((k_ptr->sval != SV_BLANKET) && (k_ptr->sval != SV_BOULDER))
					k_ptr->flags3 |= (TR3_EASY_KNOW);
				break;
			}

			/* All Food, Potions, Scrolls, Rods */
			case TV_FOOD:
			case TV_POTION:
			case TV_SCROLL:
			case TV_ROD:
			{
				k_ptr->flags3 |= (TR3_EASY_KNOW);
				break;
			}
		}
	}
}


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 */
void reset_visuals(void)
{
	int i;


	/* Extract default attr/char code for features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr_lit = f_ptr->x_attr_dim = f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char_lit = f_ptr->x_char_dim = f_ptr->x_char = f_ptr->d_char;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}

	/* Extract default attr/char code for flavors */
	for (i = 0; i < z_info->flavor_max; i++)
	{
		flavor_type *flavor_ptr = &flavor_info[i];

		/* Default attr/char */
		flavor_ptr->x_attr = flavor_ptr->d_attr;
		flavor_ptr->x_char = flavor_ptr->d_char;
	}

	/* Char/attrs remappings for use with various graphical modes */
	(void)process_pref_file("tiles.prf");
}



/*
 * Given a pval-dependant flag, return the value it is modified by.
 *
 * All pvals are transferred to the object itself upon creation.
 *
 * (error-handling)
 * If, for whatever reason, the object should have more than one pval
 * affecting this flag, only the first pval (in the order "pval", "pval2",
 * "pval3") is expressed.
 */
s16b get_object_pval(const object_type *o_ptr, u32b flg)
{
	/* Ignore non-objects */
	if (!o_ptr->k_idx) return (0);

	/* Test for any pval-dependant qualities */
	if (flg == 0L)
	{
		/* See if at least one pval is non-zero, and affects any flags */
		if (((o_ptr->pval  != 0) && (o_ptr->flags_pval1)) ||
		    ((o_ptr->pval2 != 0) && (o_ptr->flags_pval2)) ||
		    ((o_ptr->pval3 != 0) && (o_ptr->flags_pval3)))
		{
			return (1);
		}
	}

	/* Test a specific quality */
	else
	{
		/* Look at the primary pval */
		if (o_ptr->pval != 0)
		{
			/* Flag is affected by this pval -- return its value */
			if (o_ptr->flags_pval1 & (flg)) return (o_ptr->pval);
		}

		/* Look at the secondary pval */
		if (o_ptr->pval2 != 0)
		{
			/* Flag is affected by this pval -- return its value */
			if (o_ptr->flags_pval2 & (flg)) return (o_ptr->pval2);
		}

		/* Look at the tertiary pval */
		if (o_ptr->pval3 != 0)
		{
			/* Flag is affected by this pval -- return its value */
			if (o_ptr->flags_pval3 & (flg)) return (o_ptr->pval3);
		}
	}

	/* No pval affects this flag */
	return (0);
}


/*
 * Modes of object_flags_aux()
 */
#define OBJECT_FLAGS_FULL     1  /* Full info */
#define OBJECT_FLAGS_KNOWN    2  /* Only flags known to the player */
#define OBJECT_FLAGS_EXTRA    3  /* Only known extra/added/random flags */


/*
 * Obtain the non-pval qualities of an item.
 *
 * The flags associated with any object may come from its base object kind,
 * its artifact or ego template, or may be stored directly in the object_
 * type fields.
 *
 * OBJECT_FLAGS_FULL gets all the flags associated with the object.
 * OBJECT_FLAGS_KNOWN gets all the known flags associated with the object.
 * OBJECT_FLAGS_EXTRA gets (only) all the added flags associated with the
 *    object, including all random flags, and all flags added by the
 *    character or game events.
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_kind *k_ptr;

	bool know_ident = (object_known_p(o_ptr) ? TRUE : FALSE);
	bool know_all = ((o_ptr->ident & (IDENT_MENTAL)) ? TRUE : FALSE);

	/* Force full knowledge */
	if (mode == OBJECT_FLAGS_FULL) know_ident = know_all = TRUE;
	if (mode == OBJECT_FLAGS_EXTRA) know_ident = know_all = TRUE;


	/* Always start with empty flags */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Ignore non-objects */
	if (!o_ptr->k_idx) return;

	/* Require identification */
	if (!know_ident) return;


	/* Usually, get the base object, ego-item, and/or artifact flags */
	if (mode != OBJECT_FLAGS_EXTRA)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object -- always fully known if ID-ed */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;

		/* Ego-item -- always fully known if ID-ed */
		if (o_ptr->ego_item_index)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

			/* Add to base object kind flags */
			(*f1) |= e_ptr->flags1;
			(*f2) |= e_ptr->flags2;
			(*f3) |= e_ptr->flags3;
		}

		/* Artifact -- non-obvious flags require *ID* to know */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			/* We know everything */
			if (know_all)
			{
				/* Overwrite base object kind flags */
				(*f1) = a_ptr->flags1;
				(*f2) = a_ptr->flags2;
				(*f3) = a_ptr->flags3;
			}

			/* We don't know everything */
			else
			{
				/* Some things are known as soon as the item is worn */
				if (o_ptr->ident & (IDENT_WORN))
				{
					/* Nastiness */
					(*f3) |= (a_ptr->flags3 & (TR3_LIGHT_CURSE |
						   TR3_HEAVY_CURSE | TR3_PERMA_CURSE |
						   TR3_DRAIN_EXP   | TR3_SOULSTEAL |
						   TR3_NOMAGIC     | TR3_TELEPORT |
						   TR3_AGGRAVATE   | TR3_DRAIN_HP));

					/* Ignores */
					(*f2) |= (a_ptr->flags2 & (TR2_IGNORE_ACID |
					         TR2_IGNORE_ELEC | TR2_IGNORE_FIRE |
					         TR2_IGNORE_COLD));
				}
			}
		}
	}

	/* Require full knowledge to get object-specific flags */
	if (know_all)
	{
		/* Extra flags -- set 1 */
		(*f1) |= o_ptr->flags1;

		/* Extra flags -- set 2 */
		(*f2) |= o_ptr->flags2;

		/* Extra flags -- set 3 */
		(*f3) |= o_ptr->flags3;
	}

	/* Handle the "only extra flags" option */
	if (mode == OBJECT_FLAGS_EXTRA)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Remove base object flags */
		(*f1) &= ~(k_ptr->flags1);
		(*f2) &= ~(k_ptr->flags2);
		(*f3) &= ~(k_ptr->flags3);

		/* Ego-item */
		if (o_ptr->ego_item_index)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

			/* Remove ego-item flags */
			(*f1) &= ~(e_ptr->flags1);
			(*f2) &= ~(e_ptr->flags2);
			(*f3) &= ~(e_ptr->flags3);
		}

		/* Artifact */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			/* Remove artifact flags */
			(*f1) &= ~(a_ptr->flags1);
			(*f2) &= ~(a_ptr->flags2);
			(*f3) &= ~(a_ptr->flags3);
		}
	}
}


/*
 * Obtain all object qualities.
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_FLAGS_FULL, o_ptr, f1, f2, f3);
}



/*
 * Obtain known object qualities.
 */
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_FLAGS_KNOWN, o_ptr, f1, f2, f3);
}

/*
 * Obtain all extra object qualities.
 */
void object_flags_extra(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_FLAGS_EXTRA, o_ptr, f1, f2, f3);
}



/*
 * Abbreviated attribute name/flag structure
 */
typedef struct flag_insc_table
{
	cptr text;
	byte flag;
} flag_insc_table;

/*
 * A list of abbreviated attribute names and flag locations.
 *
 * Table is read in order.
 */
static flag_insc_table insc_table[] =
{
	{ "A", BRAND_ACID },
	{ "E", BRAND_ELEC },
	{ "F", BRAND_FIRE },
	{ "C", BRAND_COLD },
	{ "P", BRAND_POIS },
	{ "*F*", BRAND_FLAME },
	{ "*P*", BRAND_VENOM },
	{ "I", IMPACT },
	{ "V", VORPAL },
	{ "B", BLESSED },
	{ "!", PERFECT_BALANCE },
	{ "v", THROWING },
	{ ";", 0 },
	{ "e", SLAY_EVIL },
	{ "D", KILL_DRAGON },
	{ "d", SLAY_DRAGON },
	{ "o", SLAY_ORC },
	{ "t", SLAY_TROLL },
	{ "g", SLAY_GIANT },
	{ "&", SLAY_DEMON },
	{ "u", SLAY_UNDEAD },
	{ "a", SLAY_ANIMAL },
	{ ";", 0 },
	{ "*A*", IM_ACID },
	{ "*E*", IM_ELEC },
	{ "*F*", IM_FIRE },
	{ "*C*", IM_COLD },
	{ "Ac", RES_ACID },
	{ "El", RES_ELEC },
	{ "Fi", RES_FIRE },
	{ "Co", RES_COLD },
	{ "Po", RES_POIS },
	{ "Fe", RES_FEAR },
	{ "Bl", RES_BLIND },
	{ "Cf", RES_CONFU },
	{ "Li", RES_LITE },
	{ "Dk", RES_DARK },
	{ "So", RES_SOUND },
	{ "Sh", RES_SHARD },
	{ "Nx", RES_NEXUS },
	{ "Nt", RES_NETHR },
	{ "Ca", RES_CHAOS },
	{ "Di", RES_DISEN },
	{ ";", 0 },
	{ "Sd", SLOW_DIGEST },
	{ "Ff", FEATHER },
	{ "Lu", LITE },
	{ "Rg", REGEN },
	{ "Esp", TELEPATHY },
	{ "Si", SEE_INVIS },
	{ "Fa", FREE_ACT },
	{ "Hl", HOLD_LIFE },
	{ "!M", NOMAGIC },
	{ "Ag", AGGRAVATE },
	{ ";", 0 },
	{ "St", SUST_STR },
	{ "In", SUST_INT },
	{ "Wi", SUST_WIS },
	{ "De", SUST_DEX },
	{ "Co", SUST_CON },
	{ "Ch", SUST_CHR },
	{ NULL, 0 },
};



/*
 * Build up an abbreviated attribute list.  Optionally, show only random
 * attributes or all attributes.
 *
 * Idea and some code from Hengband.
 */
static void get_inscription_attr(char *str, int len,
	const object_type *o_ptr, bool all)
{

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	int i, k, flag, set;
	u32b bitflag;

	bool listed = FALSE;


	/* Clear the string */
	str[0] = '\0';

	/* Get the object flags -- all */
	if (all)
	{
		object_flags_known(o_ptr, &f1, &f2, &f3);
	}

	/* Get the object flags -- only random */
	else
	{
		object_flags_extra(o_ptr, &f1, &f2, &f3);
	}

	/* No flags to list */
	if (!f1 && !f2 && !f3) return;


	/* Scan down the table until we run out of possible descriptions */
	for (i = 0; insc_table[i].text; i++)
	{
		/* Get this flag */
		flag = insc_table[i].flag;

		/* This is a section divider */
		if (!flag)
		{
			/* If we have listed flags, add the section marker */
			if (listed) (void)my_strcat(str, insc_table[i].text, len);

			/* Assume next section to be empty */
			listed = FALSE;
		}


		/* Which set of flags? */
		set = flag / 32;

		/* Which bitflag in this set? */
		bitflag = 1L << (flag % 32);

		/* Object has this bitflag */
		if (((set == 1) && (f1 & bitflag)) ||
		    ((set == 2) && (f2 & bitflag)) ||
		    ((set == 3) && (f3 & bitflag)))
		{
			/* Add the inscription */
			(void)my_strcat(str, insc_table[i].text, len);

			/* This section is not empty */
			listed = TRUE;
		}
	}

	/* Determine length of string */
	for (k = 0; k < len; k++)
	{
		if (!str[k]) break;
	}

	/* Final character is a ';' -- remove it */
	if ((k >= 1) && (str[k - 1] == ';')) str[k - 1] = '\0';

}


/*
 *  Edit object inscription.  Allow the use of special codes.
 */
static void get_inscription(char *str, int len, const object_type *o_ptr)
{
	/* Point to the string to be edited */
	char *t = str;

	/* Get the inscription */
	cptr s = quark_str(o_ptr->note);

	/* Scan the inscription */
	for (; *s; s++)
	{
		/* Do not write past the end of the string */
		if (t - str >= len - 1) break;

		/* Break at beginning of special name */
		if (*s == '#') break;

		/* Note '%', but only if object is fully IDed */
		if ((*s == '%') && (o_ptr->ident & (IDENT_MENTAL)))
		{
			char attr_str[DESC_LEN];

			/* Assume we want to see only random attributes */
			bool all = FALSE;

			/* Allow all object attributes to be requested */
			if ((s[1] == 'a') && (s[2] == 'l') && (s[3] == 'l'))
			{
				all = TRUE;

				/* Skip past the "all" string */
				s += 3;
			}

			/* Build up an abbreviated list of attributes */
			get_inscription_attr(attr_str, 80, o_ptr, all);

			/* Copy it to the inscription */
			if (attr_str[0])
			{
				int j;

				/* Stop when our attribute string ends */
				for (j = 0; attr_str[j]; j++)
				{
					/* Copy until we run out of room */
					if (t - str >= len - 1) break;
					*t++ = attr_str[j];
				}
			}
		}

		/* Save all other characters */
		else
		{
			*t++ = *s;
		}
	}

	/* End this string */
	*t = '\0';
}



/*
 * Efficient version of '(T) += sprintf((T), "%c", (C))'
 */
#define object_desc_chr_macro(T,C) do { \
	\
	/* Copy the char */ \
	*(T)++ = (C); \
	\
} while (0)

/*
 * Efficient version of '(T) += sprintf((T), "%s", (S))'
 */
#define object_desc_str_macro(T,S) do { \
	\
	cptr y = (S); \
	\
	/* Copy the string */ \
	while (*y) *(T)++ = *y++; \
	\
} while (0)

/*
 * Efficient version of '(T) += sprintf((T), "%u", (N))'
 */
#define object_desc_num_macro(T,N) do { \
	\
	int n = (N); \
	\
	int p; \
	\
	/* Find "size" of "n" */ \
	for (p = 1; n >= p * 10; p = p * 10) /* loop */; \
	\
	/* Dump each digit */ \
	while (p >= 1) \
{ \
	/* Dump the digit */ \
	*(T)++ = I2D(n / p); \
	\
	/* Remove the digit */ \
	n = n % p; \
	\
	/* Process next digit */ \
	p = p / 10; \
} \
	\
} while (0)

/*
 * Efficient version of '(T) += sprintf((T), "%+d", (I))'
 */
#define object_desc_int_macro(T,I) do { \
	\
	int m = (I); \
	\
	/* Negative */ \
	if (m < 0) \
{ \
	/* Take the absolute value */ \
	m = 0 - m; \
	\
	/* Use a "minus" sign */ \
	*(T)++ = '-'; \
} \
	\
	/* Positive (or zero) */ \
	else \
{ \
	/* Use a "plus" sign */ \
	*(T)++ = '+'; \
} \
	\
	/* Dump the number itself */ \
	object_desc_num_macro(T, m); \
	\
} while (0)


/*
 * Most pvals are displayed in object descriptions, but not all.
 */
static bool should_display_pval(const object_type *o_ptr, byte set)
{
	u32b flag_set = 0L;
	s16b pval = 0;

	if ((set < 1) || (set > 3)) return (FALSE);

	/* Get the correct pval and pval flags set */
	if (set == 1) pval = o_ptr->pval;
	if (set == 2) pval = o_ptr->pval2;
	if (set == 3) pval = o_ptr->pval3;

	if (set == 1) flag_set = o_ptr->flags_pval1;
	if (set == 2) flag_set = o_ptr->flags_pval2;
	if (set == 3) flag_set = o_ptr->flags_pval3;

	/* No need to display a pval if it is zero */
	if (!pval) return (FALSE);

	/* Ignore some flags */
	flag_set &= ~(TR_PVAL_LIGHT);
	flag_set &= ~(TR_PVAL_MIGHT);

	/* No need to display a pval if it affects no displayable flags */
	if (!flag_set) return (FALSE);

	/* Display the pval */
	return (TRUE);
}


/*
 * Creates a description of the item "o_ptr", and stores it in "out_val".
 *
 * One can choose the "verbosity" of the description, including whether
 * or not the "number" of items should be described, and how much detail
 * should be used when describing the item.
 *
 * The given "buf" must be 80 chars long to hold the longest possible
 * description, which can get pretty long, including inscriptions, such as:
 * "no more Maces of Disruption (Defender) (+10,+10) [+5] (+3 to stealth)".
 * Note that the inscription will be clipped to keep the total description
 * under 79 chars (plus a terminator).
 *
 * This function uses a big temporary array to create the description,
 * and then copies up to 79 characters from this array into the buffer,
 * which will prevent crashes (but not ugliness) if any object name uses
 * more than 79 characters.
 *
 * Note the use of "object_desc_int_macro()" and "object_desc_num_macro()"
 * and "object_desc_str_macro()" and "object_desc_chr_macro()" as extremely
 * efficient, portable, versions of some common "sprintf()" commands (without
 * the bounds checking or termination writing), which allow a pointer to
 * efficiently move through a buffer while modifying it in various ways.
 *
 * Various improper uses and/or placements of "&" or "~" characters can
 * easily induce out-of-bounds memory accesses.  Some of these could be
 * easily checked for, if efficiency was not a concern.
 *
 * Note that all ego-items (when known) append an "Ego-Item Name", unless
 * the item is also an artifact, which should never happen.
 *
 * Note that all artifacts (when known) append an "Artifact Name", so we
 * have special processing for "Specials" (artifact Lights, Rings, Amulets).
 * The "Specials" never use "modifiers" if they are "known", since they
 * have special "descriptions", such as "The Necklace of the Dwarves".
 *
 * Special Lights use the "k_info" base-name (Phial, Star, or Arkenstone),
 * plus the artifact name, just like any other artifact, if known.
 *
 * Special Rings and Amulets, if not "aware", use the same code as normal
 * rings and amulets, and if "aware", use the "k_info" base-name (Ring or
 * Amulet or Necklace).  They will NEVER "append" the "k_info" name.  But,
 * they will append the artifact name, just like any artifact, if known.
 *
 * None of the Special Rings/Amulets are "EASY_KNOW", though they could be,
 * at least, those which have no "pluses", such as the three artifact lights.
 *
 * Hack -- Display "The One Ring" as "a Plain Gold Ring" until aware.
 *
 * The "pluralization" rules are extremely hackish, in fact, for efficiency,
 * we only handle things like "torch"/"torches" and "cutlass"/"cutlasses",
 * and we would not handle "box"/"boxes", or "knife"/"knives", correctly.
 * Of course, it would be easy to add rules for these forms.
 *
 * If "pref" is true then a "numeric" prefix will be pre-pended, else is is
 * assumed that a string such as "The" or "Your" will be pre-pended later.
 *
 * Modes ("pref" is TRUE):
 *   0 -- Chain Mail of Death
 *   1 -- A Cloak of Death [1,+3]
 *   2 -- An Amulet of Death [1,+3] (+2 to Stealth)
 *   3 -- 5 Rings of Death [1,+3] (+2 to Stealth) {nifty}
 *
 * Modes ("pref" is FALSE):
 *   0 -- Chain Mail of Death
 *   1 -- Cloak of Death [1,+3]
 *   2 -- Amulet of Death [1,+3] (+2 to Stealth)
 *   3 -- Rings of Death [1,+3] (+2 to Stealth) {nifty}
 */
void object_desc(char *buf, size_t max, const object_type *o_ptr, int pref, int mode)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	cptr basenm, modstr;

	int power;
	int quantity;
	int i;

	bool aware;
	bool known;
	bool easy_know;

	bool flavor;

	bool append_name;
	bool do_prefix;
	bool fake_art;
	bool the_One = FALSE;

	bool show_weapon;
	bool show_armor;
	bool hide_tohit_todam;

	char *b;
	char *t;

	cptr s;
	cptr u;
	cptr v;

	char p1 = '(', p2 = ')';
	char b1 = '[', b2 = ']';
	char c1 = '{', c2 = '}';

	char tmp_note[DESC_LEN];
	char discount_buf[DESC_LEN];
	char tmp_buf[DESC_LEN];

	u32b f1, f2, f3;


	/* Get object attributes */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* See if the object is "aware" */
	aware = (object_aware_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "known" */
	known = (object_known_p(o_ptr) ? TRUE : FALSE);

	/* Assume no special "easy_know" conditions */
	easy_know = FALSE;

	/* See if the object is "flavored" */
	flavor = (k_ptr->flavor ? TRUE : FALSE);

	/* Allow flavors to be hidden when aware */
	if (aware && !show_flavors) flavor = FALSE;

	/* Allow flavors to be forced or forbidden */
	if (object_desc_flavour)
	{
		if (object_desc_flavour > 0) flavor = TRUE;
		else                         flavor = FALSE;

		object_desc_flavour = 0;
	}


	/* Get default quantity */
	quantity = o_ptr->number;

	/* Allow pluralization to be forced or forbidden */
	if (object_desc_plural)
	{
		if (object_desc_plural > 0) quantity = MAX(2, quantity);
		else                        quantity = 1;

		object_desc_plural = 0;
	}


	/* Assume no name appending */
	append_name = FALSE;

	/* Assume no prefix */
	do_prefix = FALSE;

	/* Check to see if it has been inscribed as a fake artifact */
	fake_art = (o_ptr->note && strchr(quark_str(o_ptr->note), '#'));

	/* Assume no need to show "weapon" bonuses */
	show_weapon = FALSE;

	/* Assume no need to show "armor" bonuses */
	show_armor = FALSE;

	/* Assume we show to_hit and to_dam */
	hide_tohit_todam = FALSE;


	/* The character is hallucinating and is not dead */
	if ((p_ptr->image) && (!p_ptr->is_dead))
	{
		/* The object is not being carried  XXX XXX */
		if ((o_ptr->iy != 0) && (o_ptr->ix != 0))
		{
			/* See the happy sword */
			int choice = randint(20);
			if      (choice == 1) modstr = "weird ";
			else if (choice == 2) modstr = "crazy ";
			else if (choice == 3) modstr = "strange ";
			else if (choice == 4) modstr = "psychedelic ";
			else if (choice == 5) modstr = "far-out tripping ";
			else if (choice == 6) modstr = "loco ";
			else if (choice == 7) modstr = "waving ";
			else if (choice == 8) modstr = "scary ";
			else if (choice == 9) modstr = "happy ";
			else if (choice ==10) modstr = "wriggling ";
			else                  modstr = "";

			/* Sometimes the name isn't the important thing */
			if (one_in_(5))
			{
				basenm = "I don't know what";
			}

			/* Get name of base object kind */
			else
			{
				if (o_ptr->tval == TV_SKELETON)     basenm = "bones";
				else if (o_ptr->tval == TV_BOTTLE)  basenm = "flask";
				else if (o_ptr->tval == TV_JUNK)    basenm = "stuff";
				else if (o_ptr->tval == TV_SPIKE)   basenm = "spikes";
				else if (o_ptr->tval == TV_CHEST)   basenm = "chest";

				else if (is_missile(o_ptr))         basenm = "ammo";
				else if (is_missile_weapon(o_ptr))  basenm = "bow";
				else if (o_ptr->tval == TV_DIGGING) basenm = "shovel";
				else if (o_ptr->tval == TV_HAFTED)  basenm = "club";
				else if (o_ptr->tval == TV_POLEARM) basenm = "spear";
				else if (o_ptr->tval == TV_SWORD)   basenm = "sword";

				else if (o_ptr->tval == TV_BOOTS)   basenm = "boots";
				else if (o_ptr->tval == TV_GLOVES)  basenm = "gloves";
				else if (o_ptr->tval == TV_HELM)    basenm = "helmet";
				else if (o_ptr->tval == TV_CROWN)   basenm = "crown";
				else if (o_ptr->tval == TV_SHIELD)  basenm = "shield";
				else if (o_ptr->tval == TV_CLOAK)   basenm = "cloak";
				else if (o_ptr->tval == TV_SOFT_ARMOR) basenm = "armor";
				else if (o_ptr->tval == TV_HARD_ARMOR) basenm = "armor";
				else if (o_ptr->tval == TV_DRAG_ARMOR) basenm = "armor";

				else if (o_ptr->tval == TV_LITE)    basenm = "light";
				else if (o_ptr->tval == TV_AMULET)  basenm = "amulet";
				else if (o_ptr->tval == TV_RING)    basenm = "ring";
				else if (o_ptr->tval == TV_STAFF)   basenm = "wooden stick";
				else if (o_ptr->tval == TV_WAND)    basenm = "metal bar";
				else if (o_ptr->tval == TV_ROD)     basenm = "metal bar";

				else if (o_ptr->tval == TV_FLASK)   basenm = "flask";
				else if (o_ptr->tval == TV_FOOD)    basenm = "chow";
				else if (o_ptr->tval == TV_COMPONENT)
					basenm = "lump of something";
				else if (o_ptr->tval == TV_POTION) basenm = "flask";
				else if (o_ptr->tval == TV_SCROLL) basenm = "paper";
				else if ((o_ptr->tval >= TV_MAGIC_BOOK) &&
					(o_ptr->tval <= TV_DARK_BOOK)) basenm = "book";
				else if (o_ptr->tval == TV_GOLD)   basenm = "pretty thing";

				else basenm = "thing";
			}

			/* Build the feeling */
			if (o_ptr->number)
				(void)strnfmt(buf, max, "some kind of %s%s", modstr, basenm);
			else
				(void)strnfmt(buf, max, "no more %s%s", modstr, basenm);
			/* Express the feeling */
			return;
		}
	}


	/* Extract default "base" string */
	basenm = (k_name + k_ptr->name);

	/* Assume no "modifier" string */
	modstr = "";


	/* Analyze the object */
	switch (o_ptr->tval)
	{
		/* Some objects are easy to describe */
		case TV_SKELETON:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
		case TV_BOTTLE:
		case TV_PARCHMENT:
		{
			break;
		}

		case TV_JUNK:
		{
			if (o_ptr->sval == SV_BLANKET)
			{
				if ((f2 & (TR2_RES_ACID)) &&
				    (f2 & (TR2_RES_ELEC)) &&
				    (f2 & (TR2_RES_FIRE)) &&
				    (f2 & (TR2_RES_COLD)))
				{
					if (cursed_p(o_ptr) && known)
					{
						modstr = "Elemental Destruction";
					}
					else
					{
						modstr = "Elemental Protection";
					}
				}

				else if ((f2 & (TR2_RES_DISEN)) &&
				         (f2 & (TR2_RES_DRAIN)) &&
				         (f3 & (TR3_BLESSED)))
				{
					if (cursed_p(o_ptr) && known)
					{
						modstr = "Ravaging";
					}
					else
					{
						modstr = "Ethereal Sanctuary";
					}
				}

				/* Put the modifier string in the proper place */
				if (do_prefix) basenm = "& # Blanket~";
				else 				basenm = "& Blanket~ of #";
				do_prefix = FALSE;
			}

			break;
		}

		/* Missiles/Bows/Weapons */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			show_weapon = TRUE;

			/* Some weapons are unusually heavy and powerful */
			if ((k_ptr->xtra & (XTRA_CAN_BE_HEAVY)) && (!artifact_p(o_ptr)) &&
			    (o_ptr->weight > k_ptr->weight) && (o_ptr->dd > k_ptr->dd))
			{
				/* Get a special modifier to base name */
				if (o_ptr->dd >= k_ptr->dd + 2) modstr = "Great ";
				else                            modstr = "Heavy ";

				do_prefix = TRUE;
			}

			/* Some weapons can be vorpal */
			if ((k_ptr->xtra & (XTRA_VORPAL)) && (!artifact_p(o_ptr)) &&
			    (f1 & (TR1_VORPAL)) && (known))
			{
				modstr = "Vorpal ";
				do_prefix = TRUE;
			}

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
			show_armor = TRUE;
			break;
		}

		/* Lights (including a few "Specials") */
		case TV_LITE:
		{
			break;
		}

		/* Amulets (including a few "Specials") */
		case TV_AMULET:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
			}

			/* Normal amulets */
			else
			{
				/* Flavour the object */
				modstr = flavor_text + flavor_info[k_ptr->flavor].text;
				if (aware) append_name = TRUE;
				basenm = (flavor ? "& # Amulet~" : "& Amulet~");
			}

			break;
		}

		/* Rings (including a few "Specials") */
		case TV_RING:
		{
			/* Special case -- The One Ring */
			if (o_ptr->sval == SV_RING_POWER)
			{
				if (!aware)
				{
					modstr = "Plain Gold";
					basenm = "& # Ring~";
				}
				else
				{
					the_One = TRUE;
					basenm = "";
					pref = FALSE;
					easy_know = TRUE;
				}
				append_name = FALSE;
			}

			/* Hack -- Known artifacts */
			else if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
			}

			/* Normal rings */
			else
			{
				/* Flavour the object */
				modstr = flavor_text + flavor_info[k_ptr->flavor].text;
				if (aware) append_name = TRUE;
				basenm = (flavor ? "& # Ring~" : "& Ring~");
			}

			break;
		}

		/* Staffs */
		case TV_STAFF:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
				break;
			}

			/* Flavour the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Staff~" : "& Staff~");

			break;
		}

		/* Wands */
		case TV_WAND:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
				break;
			}

			/* Flavour the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Wand~" : "& Wand~");

			break;
		}

		/* Rods */
		case TV_ROD:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
				break;
			}

			/* Flavour the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Rod~" : "& Rod~");

			break;
		}

		/* Scrolls */
		case TV_SCROLL:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
				break;
			}

			/* Flavour the object */
			modstr = scroll_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& Scroll~ titled \"#\"" : "& Scroll~");

			break;
		}

		/* Potions */
		case TV_POTION:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
				break;
			}

			/* Flavour the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Potion~" : "& Potion~");

			break;
		}

		/* Food */
		case TV_FOOD:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware)
			{
				easy_know = TRUE;
				break;
			}

			/* Ordinary food is "boring" */
			if (o_ptr->sval >= SV_FOOD_MIN_FOOD) break;

			/* Flavour the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Mushroom~" : "& Mushroom~");

			break;
		}

		/* Components */
		case TV_COMPONENT:
		{
			modstr = basenm;
			basenm = "& chunk~ of #";
			break;
		}

		/* Magic Books */
		case TV_MAGIC_BOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of Magic Spells #";
			break;
		}

		/* Prayer Books */
		case TV_PRAYER_BOOK:
		{
			modstr = basenm;
			basenm = "& Holy Book~ of Prayers #";
			break;
		}

		/* Nature Books */
		case TV_NATURE_BOOK:
		{
			modstr = basenm;
			basenm = "& Stone~ of Nature #";
			break;
		}

		/* Dark Books */
		case TV_DARK_BOOK:
		{
			modstr = basenm;
			basenm = "& Tome~ of Necromancy #";
			break;
		}

		/* Hack - Gold/Gems */
		case TV_GOLD:
		{
			(void)my_strcpy(tmp_buf, basenm, max);
			break;
		}

		/* Essences */
		case TV_ESSENCE:
		{
			modstr = basenm;
			if (aware) append_name = TRUE;
			basenm = "& essence~";
			break;
		}

		/* Hack -- The pouch  XXX XXX */
		case TV_POUCH:
		{
			int count, types;

			/* Get the number of essence types and total quantity */
			for (types = 0, count = 0, i = 0; i < NUM_ESSENCE; i++)
			{
				count += p_ptr->essence[i];
				if (p_ptr->essence[i] > 0) types++;
			}

			/* Display item information */
			if (!count) basenm = format("A %s (empty)", basenm);
			else basenm =
				format("a %s (holding %d essence%s of %d type%s)",
				basenm, count, (count != 1 ? "s" : ""), types, (types != 1 ? "s" : ""));

			(void)my_strcpy(buf, basenm, max);
			return;
		}

		/* Hack -- Default -- Used in the "inventory" routine */
		default:
		{
			(void)my_strcpy(buf, "(nothing)", max);
			return;
		}
	}


	/* Start dumping the result */
	t = b = tmp_buf;

	/* Begin */
	s = basenm;

	/* Handle objects which sometimes use "a" or "an" */
	if (*s == '&')
	{
		/* Skip the ampersand and the following space */
		s += 2;

		/* No prefix */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- None left */
		else if (quantity <= 0)
		{
			object_desc_str_macro(t, "no more ");
		}

		/* Extract the number */
		else if (quantity > 1)
		{
			object_desc_num_macro(t, o_ptr->number);
			object_desc_chr_macro(t, ' ');
		}

		/* Hack -- The only one of its kind (artifacts or inscribed). */
		else if (((artifact_p(o_ptr)) &&
		         ((known) || (aware && easy_know))) ||
		        (fake_art && is_wearable(o_ptr)))
		{
			object_desc_str_macro(t, "the ");
		}

		/* A single one, and next character will be a vowel */
		else if ((*s == '#') ? my_is_vowel(modstr[0]) : my_is_vowel(*s))
		{
			object_desc_str_macro(t, "an ");
		}

		/* A single one, and next character will be a non-vowel */
		else
		{
			object_desc_str_macro(t, "a ");
		}
	}

	/* Handle objects which never use "a" or "an" */
	else
	{
		/* No pref */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- all gone */
		else if (quantity <= 0)
		{
			object_desc_str_macro(t, "no more ");
		}

		/* Prefix a number if required */
		else if (quantity > 1)
		{
			object_desc_num_macro(t, o_ptr->number);
			object_desc_chr_macro(t, ' ');
		}

		/* Hack -- The only one of its kind (artifacts or inscribed). */
		else if (((artifact_p(o_ptr)) &&
		         ((known) || (aware && easy_know))) || fake_art)
		{
			object_desc_str_macro(t, "the ");
		}

		/* Hack -- A single item, so no prefix needed */
		else
		{
			/* Nothing */
		}
	}

	/* Non-artifact perfectly balanced throwing weapons are indicated. */
	if ((f1 & (TR1_PERFECT_BALANCE)) && (f1 & (TR1_THROWING)) &&
	    (known) && (!artifact_p(o_ptr)) && (mode >= 3))
	{
		object_desc_str_macro(t, "Well-balanced ");
	}

	/* Insert any prefix */
	if (do_prefix)
	{
		object_desc_str_macro(t, modstr);
	}

	/* Copy the string */
	for (; *s; s++)
	{
		/* Pluralizer */
		if (*s == '~')
		{
			/* Add a plural if needed */
			if (quantity != 1)
			{
				char k = t[-1];

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h')) *t++ = 'e';

				/* Knife -> Knives */
				else if ((k == 'e') && (t[-2] == 'f'))
				{
					t[-2] = 'v';
				}

				/* Add an 's' */
				*t++ = 's';
			}
		}

		/* Modifier */
		else if (*s == '#')
		{
			/* Append the modifier */
			object_desc_str_macro(t, modstr);
		}

		/* Normal */
		else
		{
			/* Copy */
			*t++ = *s;
		}
	}

	/* Treasure needs no further description */
	if (o_ptr->tval == TV_GOLD)
	{
		/* Hack -- end the string */
		*t++ = '\0';

		(void)my_strcpy(buf, tmp_buf, max);
		return;
	}

	/* Append the "kind name" to the "base name" */
	if (append_name && !fake_art)
	{
		bool alternate_name = FALSE;

		object_desc_str_macro(t, " of ");

		/*
		 * Hack -- adjust object name if necessary  XXX XXX XXX
		 *
		 * This is fairly ugly code, but it makes various rings and amulets
		 * much cooler.
		 */
		if ((known) || (o_ptr->ident & (IDENT_WORN)))
		{
			/* Adjust some nasty rings and amulets */
			if (k_ptr->tval == TV_RING)
			{
				if ((k_ptr->sval == SV_RING_STR) && (o_ptr->pval < 0))
				{
					object_desc_str_macro(t, "Weakness");
					alternate_name = TRUE;
				}
				else if ((k_ptr->sval == SV_RING_DEX) && (o_ptr->pval < 0))
				{
					object_desc_str_macro(t, "Clumsiness");
					alternate_name = TRUE;
				}
				if ((k_ptr->sval == SV_RING_CON) && (o_ptr->pval < 0))
				{
					object_desc_str_macro(t, "Withering");
					alternate_name = TRUE;
				}
				if ((k_ptr->sval == SV_RING_PROTECTION) && (o_ptr->to_a < 0))
				{
					object_desc_str_macro(t, "Vulnerability");
					alternate_name = TRUE;
				}
			}
			else if (k_ptr->tval == TV_AMULET)
			{
				if ((k_ptr->sval == SV_AMULET_INTELLIGENCE) && (o_ptr->pval < 0))
				{
					object_desc_str_macro(t, "Stupidity");
					alternate_name = TRUE;
				}
				if ((k_ptr->sval == SV_AMULET_WISDOM) && (o_ptr->pval < 0))
				{
					char tmp_obj_name[DESC_LEN];
					strcpy(tmp_obj_name, "Na[\"i]vet['e]");
					xstr_trans(tmp_obj_name, LATIN1);
					object_desc_str_macro(t, tmp_obj_name);
					alternate_name = TRUE;
				}
				if ((k_ptr->sval == SV_AMULET_CHARISMA) && (o_ptr->pval < 0))
				{
					object_desc_str_macro(t, "Ugliness");
					alternate_name = TRUE;
				}
				if ((k_ptr->sval == SV_AMULET_SAVING_THROW) && (o_ptr->pval < 0))
				{
					object_desc_str_macro(t, "Magical Vulnerability");
					alternate_name = TRUE;
				}
			}
		}

		/* Usually, use the standard name */
		if (!alternate_name) object_desc_str_macro(t, (k_name + k_ptr->name));
	}


	/*
	 * Hack -- Add false-artifact names  -TM-
	 * A dagger inscribed "#of Smell" will be named "the Dagger of Smell"
	 */
	if (fake_art)
	{
		/* Find the '#'. Everything after it is the fake artifact name. */
		cptr str = strchr(quark_str(o_ptr->note), '#');

		/* Add the false name. */
		object_desc_chr_macro(t, ' ');
		object_desc_str_macro(t, (&str[1]));
	}

	/* Hack -- Append "Artifact" or "Special" names (unless inscribed) */
	else if ((known) || ((easy_know) && (aware)))
	{
		/* Grab any artifact name */
		if (artifact_p(o_ptr))
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			if (!the_One) object_desc_chr_macro(t, ' ');
			object_desc_str_macro(t, (a_name + a_ptr->name));
		}

		/* Grab any ego-item name */
		else if (o_ptr->ego_item_index)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->ego_item_index];

			object_desc_chr_macro(t, ' ');
			object_desc_str_macro(t, (e_name + e_ptr->name));
		}
	}

	/* Hack -- grenades need special descriptions */
	if ((o_ptr->tval == TV_POTION) && (o_ptr->sval == SV_POTION_GRENADE))
	{
		/* Extract default "base" string for essences in grenade */
		char *basenm_essence =
			(k_name + k_info[lookup_kind(TV_ESSENCE, o_ptr->pval)].name);

		/* Add essence name to base name */
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, p1);
		object_desc_str_macro(t, basenm_essence);
		object_desc_chr_macro(t, p2);
	}

	/* Lit light sources are indicated */
	if (f3 & (TR3_IS_LIT))
	{
		object_desc_str_macro(t, " (lit)");
	}


	/* No more details wanted */
	if (mode < 1) goto object_desc_done;


	/* Hack -- Chests must be described in detail */
	if (o_ptr->tval == TV_CHEST)
	{
		cptr tail = "";

		/* Not searched yet */
		if (!known)
		{
			/* Nothing */
		}

		/* May be "empty" */
		else if (!o_ptr->pval)
		{
			tail = " (empty)";
		}

		/* May be "disarmed" or "unlocked" */
		else if (o_ptr->pval < 0)
		{
			/* Had traps */
			if (check_chest_traps(o_ptr, TRUE))
			{
				tail = " (disarmed)";
			}

			/* Never had traps */
			else
			{
				tail = " (unlocked)";
			}
		}

		/* Describe the traps, if any */
		else
		{
			/* Describe the traps */
			switch (check_chest_traps(o_ptr, FALSE))
			{
				case 0:
				{
					tail = " (Locked)";
					break;
				}
				case CHEST_GAS:
				{
					tail = " (Gas Trap)";
					break;
				}
				case CHEST_DRAIN_STAT:
				{
					tail = " (Needle)";
					break;
				}
				case CHEST_WEIRD:
				{
					tail = " (Strange Magiks)";
					break;
				}
				case CHEST_SUMMON:
				{
					tail = " (Summoning Runes)";
					break;
				}
				case CHEST_CURSE:
				{
					tail = " (Hexes)";
					break;
				}
				default:
				{
					tail = " (Unknown Traps)";
					break;
				}
			}
		}

		/* Append the tail */
		object_desc_str_macro(t, tail);
	}

	/* Blankets may be damaged */
	else if ((o_ptr->tval == TV_JUNK) && (o_ptr->sval == SV_BLANKET))
	{
		cptr tail = "";

		/* Blanket is destroyed */
		if (o_ptr->ac <= 0)
		{
			tail = "";
		}

		/* Blanket is almost destroyed */
		else if (o_ptr->ac <= o_ptr->pval / 4)
		{
			tail = " (almost destroyed)";
		}

		/* Blanket has significant damage */
		else if (o_ptr->ac <= o_ptr->pval / 2)
		{
			tail = " (damaged)";
		}

		/* Append the tail */
		object_desc_str_macro(t, tail);
	}

	/* Display the item like a weapon */
	if (f3 & (TR3_SHOW_MODS)) show_weapon = TRUE;

	/* Display the item like a weapon */
	if (o_ptr->to_h && o_ptr->to_d) show_weapon = TRUE;

	/* Display melee weapons with an armor class like armor */
	if ((is_melee_weapon(o_ptr)) && (o_ptr->ac)) show_armor = TRUE;

	/* Hide to_hit and to_dam for some objects */
	if ((is_magic_book(o_ptr)) || (is_magical_device(o_ptr)))
	{
		hide_tohit_todam = TRUE;
	}


	/* Dump base weapon info */
	switch (o_ptr->tval)
	{
		/* Missiles */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			/* Fall through */
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Append a "damage" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_num_macro(t, o_ptr->dd);
			object_desc_chr_macro(t, 'd');
			object_desc_num_macro(t, o_ptr->ds);
			object_desc_chr_macro(t, p2);

			/* All done */
			break;
		}

		/* Bows */
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		{
			/* Get the base power of the missile weapon */
			power = k_ptr->dd;

			/* If known, display the true power */
			if (known)
				power = o_ptr->dd + get_object_pval(o_ptr, TR_PVAL_MIGHT);

			/* Append a "power" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_chr_macro(t, 'x');
			object_desc_num_macro(t, power);
			object_desc_chr_macro(t, p2);

			/* All done */
			break;
		}

		/* Light sources */
		case TV_LITE:
		{
			/* Display the light radius */
			power = get_object_pval(o_ptr, TR_PVAL_LIGHT);

			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, b1);
			if (power < 0) object_desc_int_macro(t, power);
			else           object_desc_num_macro(t, power);
			object_desc_chr_macro(t, b2);

			break;
		}
	}

	/* Add the weapon bonuses */
	if (known && !hide_tohit_todam)
	{
		/* Show the tohit/todam on request */
		if (show_weapon)
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_int_macro(t, o_ptr->to_h);
			object_desc_chr_macro(t, ',');
			object_desc_int_macro(t, o_ptr->to_d);
			object_desc_chr_macro(t, p2);
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_int_macro(t, o_ptr->to_h);
			object_desc_chr_macro(t, p2);
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_int_macro(t, o_ptr->to_d);
			object_desc_chr_macro(t, p2);
		}
	}


	/* Add the armor bonuses */
	if (known)
	{
		/* Show the armor class info */
		if (show_armor)
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, b1);
			object_desc_num_macro(t, o_ptr->ac);
			object_desc_chr_macro(t, ',');
			object_desc_int_macro(t, o_ptr->to_a);
			object_desc_chr_macro(t, b2);
		}

		/* No base armor, but does increase armor */
		else if ((o_ptr->to_a) && (!is_missile(o_ptr)))
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, b1);
			object_desc_int_macro(t, o_ptr->to_a);
			object_desc_chr_macro(t, b2);
		}
	}

	/* Hack -- always show base armor */
	else if (show_armor)
	{
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, b1);
		object_desc_num_macro(t, o_ptr->ac);
		object_desc_chr_macro(t, b2);
	}


	/* No more details wanted */
	if (mode < 2) goto object_desc_done;

	/* Hack -- Wands and Staffs have charges */
	if (known && ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, p1);

		/* Clear explanation for staffs. */
		if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
		{
			object_desc_num_macro(t, o_ptr->number);
			object_desc_str_macro(t, "x ");
		}

		object_desc_num_macro(t, o_ptr->pval);
		object_desc_str_macro(t, " charge");

		if (o_ptr->pval != 1)
		{
			object_desc_chr_macro(t, 's');
		}

		object_desc_chr_macro(t, p2);
	}

	/*
	 * Hack -- Rods have a "charging" indicator.   Now that stacks of rods may
	 * be in any state of charge or discharge, this now includes a number.
	 */
	else if (known && (o_ptr->tval == TV_ROD))
	{
		/* Hack -- Dump " (# charging)" if relevant */
		if (o_ptr->timeout)
		{
			/* Stacks of rods display an exact count of charging rods. */
			if (o_ptr->number > 1)
			{
				/* Paranoia. */
				if (k_ptr->pval == 0) k_ptr->pval = 1;

				/* Find out how many rods are charging, by dividing
				 * current timeout by each rod's maximum timeout.
				 * Ensure that any remainder is rounded up.  Display
				 * very discharged stacks as merely fully discharged.
				 */
				power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
				if (power > o_ptr->number) power = o_ptr->number;

				/* Display prettily. */
				object_desc_str_macro(t, " (");
				object_desc_num_macro(t, power);
				object_desc_str_macro(t, " charging)");
			}

			/* "one Rod of Perception (1 charging)" would look tacky. */
			else
			{
				object_desc_str_macro(t, " (charging)");
			}
		}
	}

	/* Process light sources that need fuel */
	else if ((o_ptr->tval == TV_LITE) && (!(f3 & (TR3_NOFUEL))))
	{
		/* Hack -- Turns of light for normal light sources */
		object_desc_str_macro(t, " <Fuel: ");
		object_desc_num_macro(t, o_ptr->pval);
		object_desc_str_macro(t, ">");
	}

	/* Display object pvals */
	else if (known && (get_object_pval(o_ptr, 0L)))
	{
		int displayed = 0;

		/* Dump the first pval value */
		if (should_display_pval(o_ptr, 1))
		{
			/* Start the display */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_int_macro(t, o_ptr->pval);
			displayed++;
		}

		/* Dump the second pval value */
		if (should_display_pval(o_ptr, 2))
		{
			/* Start or continue the display */
			if (displayed) object_desc_str_macro(t, ", ");
			else
			{
				object_desc_chr_macro(t, ' ');
				object_desc_chr_macro(t, p1);
			}

			object_desc_int_macro(t, o_ptr->pval2);
			displayed++;
		}

		/* Dump the third pval value */
		if (should_display_pval(o_ptr, 3))
		{
			/* Start or continue the display */
			if (displayed) object_desc_str_macro(t, ", ");
			else
			{
				object_desc_chr_macro(t, ' ');
				object_desc_chr_macro(t, p1);
			}

			object_desc_int_macro(t, o_ptr->pval3);
			displayed++;
		}

		/* We sometimes show pval descriptions */
		if ((displayed == 1) && (!(f3 & (TR3_HIDE_TYPE))))
		{
			cptr tail = "";

			/* Speed */
			if (get_object_pval(o_ptr, TR_PVAL_SPEED))
			{
				/* Dump " to speed" */
				tail = " to speed";
			}

			/* Blows */
			else if (get_object_pval(o_ptr, TR_PVAL_BLOWS))
			{
				/* Add " attack" */
				if (ABS(get_object_pval(o_ptr, TR_PVAL_BLOWS)) == 1)
				{
					tail = " attack";
				}
				else
				{
					tail = " attacks";
				}
			}

			/* Invisibility */
			else if (get_object_pval(o_ptr, TR_PVAL_INVIS))
			{
				/* Dump " to invisibility" */
				tail = " to invisibility";
			}

			/* Stealth */
			else if (get_object_pval(o_ptr, TR_PVAL_STEALTH))
			{
				/* Dump " to stealth" */
				tail = " to stealth";
			}

			/* Disarming */
			else if (get_object_pval(o_ptr, TR_PVAL_DISARM))
			{
				/* Dump " to searching" */
				tail = " to disarming";
			}

			/* Devices */
			else if (get_object_pval(o_ptr, TR_PVAL_DEVICE))
			{
				/* Dump " to searching" */
				tail = " to devices";
			}

			/* Saving throw */
			else if (get_object_pval(o_ptr, TR_PVAL_SAVE))
			{
				/* Dump " to saving throw" */
				tail = " to saving throw (x5)";
			}

			/* Mana */
			else if (get_object_pval(o_ptr, TR_PVAL_MANA))
			{
				/* Dump " to mana" */
				tail = " to mana (x15)";
			}

			/* Awareness */
			else if (get_object_pval(o_ptr, TR_PVAL_AWARE))
			{
				/* Dump " to searching" */
				tail = " to awareness";
			}

			/* Infravision */
			else if (get_object_pval(o_ptr, TR_PVAL_INFRA))
			{
				/* Dump " to infravision" */
				tail = " to infravision";
			}

			/* Add the descriptor */
			object_desc_str_macro(t, tail);
		}

		if (displayed) object_desc_chr_macro(t, p2);
	}

	/* Magical devices can be damaged  XXX XXX */
	if (((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)) &&
		  (o_ptr->ac < k_info[o_ptr->k_idx].ac))
	{
		if (o_ptr->ac < k_info[o_ptr->k_idx].ac / 2)
			object_desc_str_macro(t, " (badly damaged)");
		else
			object_desc_str_macro(t, " (damaged)");
	}

	/* Indicate charging objects other than rods. */
	if (known && o_ptr->timeout && o_ptr->tval != TV_ROD)
	{
		/* Hack -- Dump " (charging)" if relevant */
		object_desc_str_macro(t, " (charging)");
	}

	/* No more details wanted */
	if (mode < 3) goto object_desc_done;


	/* Use standard inscription */
	if (o_ptr->note)
	{
		/* Get and edit the inscription */
		get_inscription(tmp_note, 80, o_ptr);

		/* Point to the inscription, notice lack of inscription */
		if (tmp_note[0] == '\0') u = NULL;
		else u = tmp_note;
	}

	/* Use nothing */
	else
	{
		u = NULL;
	}


	/* Hack -- Use "empty" for empty wands/staffs */
	if (!known && (o_ptr->ident & (IDENT_EMPTY)))
	{
		v = "empty";
	}

	/* Use "cursed" if the item is known to be cursed */
	else if (cursed_p(o_ptr) && known)
	{
		v = "cursed";
	}

	/* Use special inscription, if any */
	else if (o_ptr->inscrip)
	{
		v = inscrip_text[o_ptr->inscrip];
	}

	/* Use "tried" if the object has been tested unsuccessfully */
	else if (!aware && object_tried_p(o_ptr))
	{
		v = "tried";
	}

	/*
	 * Use the price adjustment, if any.   No annoying inscription for homemade
	 * items, or if you can't sell the goods anyway.
	 */
	else if (!birth_stores_only_sell && (o_ptr->cost_adjust > 0) && (o_ptr->cost_adjust < 100) &&
	         (o_ptr->cost_adjust != 20))
	{
		char *q = discount_buf;
		object_desc_num_macro(q, 100 - o_ptr->cost_adjust);
		object_desc_str_macro(q, "% off");
		*q = '\0';
		v = discount_buf;
	}


	/* Nothing */
	else
	{
		v = NULL;
	}


	/* Inscription */
	if (u || v)
	{
		/* Begin the inscription */
		*t++ = ' ';
		*t++ = c1;

		/* Standard inscription */
		if (u)
		{
			/* Append the inscription */
			while (*u) *t++ = *u++;
		}

		/* Special inscription too */
		if (u && v)
		{
			/* Separator */
			*t++ = ',';
			*t++ = ' ';
		}

		/* Special inscription */
		if (v)
		{
			/* Append the inscription */
			while (*v) *t++ = *v++;
		}

		/* Terminate the inscription */
		*t++ = c2;
	}


object_desc_done:


	/* Terminate */
	*t = '\0';

	/* Copy the string over */
	(void)my_strcpy(buf, tmp_buf, max);
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, size_t max, const object_type *o_ptr, int pref, int mode)
{
	object_type *i_ptr;
	object_type object_type_body;

	/* Save the "flavor" */
	s16b hack_flavor = k_info[o_ptr->k_idx].flavor;

	/* Save the "aware" flag */
	bool hack_aware = ((k_info[o_ptr->k_idx].special & (SPECIAL_AWARE))
	                   ? TRUE : FALSE);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the object */
	object_copy(i_ptr, o_ptr);


	/* Clear the flavor */
	k_info[i_ptr->k_idx].flavor = 0;

	/* Set the "known" flag */
	i_ptr->ident |= (IDENT_KNOWN);

	/* Force "aware" for description */
	k_info[i_ptr->k_idx].special |= (SPECIAL_AWARE);


	/* Describe the object */
	object_desc(buf, max, i_ptr, pref, mode);


	/* Restore "flavor" value */
	k_info[i_ptr->k_idx].flavor = hack_flavor;

	/* Clear "aware" flag if necessary */
	if (!hack_aware) k_info[i_ptr->k_idx].special &= ~(SPECIAL_AWARE);
}



/*
 * Strip an object or artifact name into a buffer
 */
void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
		if (*str != '~') *t++ = my_tolower(*str);
	}

	/* Terminate the new name */
	*t = '\0';
}


/*
 * Convert an inventory index into a one-character label.
 *
 * Note that the label does NOT distinguish inven/equip.
 */
char index_to_label(int i)
{
	/* Inventory indexes are straightforward */
	if (i < INVEN_WIELD) return (I2A(i));

	/* Indexes for quiver slots are numeric */
	if ((i >= INVEN_Q1) && (i <= INVEN_Q0))
	{
		if (i != INVEN_Q0) return (('1') + i - INVEN_Q1);
		else               return ('0');
	}

	/* Equipment indexes are offset */
	return (I2A(i - INVEN_WIELD));
}


/*
 * Convert a label into the index of an item in inventory.
 *
 * Return "-1" if the label does not indicate a real item.
 */
s16b label_to_inven(int c)
{
	int i;

	/* Convert */
	i = (islower((unsigned char)c) ? A2I(c) : -1);

	/* Verify the index */
	if ((i < 0) || (i > INVEN_PACK)) return (-1);

	/* Empty slots can never be chosen */
	if (!inventory[i].k_idx) return (-1);

	/* Return the index */
	return (i);
}

/*
 * Convert a label into the index of an item in the equipment.
 *
 * Return "-1" if the label does not indicate a real item.
 */
s16b label_to_equip(int c)
{
	int i;

	/* Hack -- handle quiver slots. */
	if (isdigit((unsigned char)c))
	{
		if (D2I(c) == 0) i = INVEN_Q0;
		else             i = INVEN_Q1 + (D2I(c) - 1);
	}

	/* Convert */
	else i = (islower((unsigned char)c) ? A2I(c) : -1) + INVEN_WIELD;

	/* Verify the index */
	if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return (-1);


	/* Mega-hack:  Allow choosing empty slots if slot_tester_hook is active */
	if (!slot_tester_hook)
	{
		/* Empty slots can never be chosen */
		if (!inventory[i].k_idx) return (-1);
	}

	/* Return the index */
	return (i);
}


/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(const object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			/* Get object flags */
			u32b f1, f2, f3;
			object_flags(o_ptr, &f1, &f2, &f3);

			/* If item is a thrown weapon, use quiver (by default) */
			if (f1 & (TR1_THROWING)) return (INVEN_Q1);

			/* If it is not, use wield arm */
			else                     return (INVEN_WIELD);
		}

		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		{
			return (INVEN_BOW);
		}

		case TV_RING:
		{
			/* Use the right hand first */
			if (!inventory[INVEN_RIGHT].k_idx) return (INVEN_RIGHT);

			/* Use the left hand for swapping (by default) */
			return (INVEN_LEFT);
		}

		case TV_AMULET:
		{
			return (INVEN_NECK);
		}
		case TV_LITE:
		{
			return (INVEN_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (INVEN_BODY);
		}

		case TV_CLOAK:
		{
			return (INVEN_OUTER);
		}
		case TV_SHIELD:
		{
			return (INVEN_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (INVEN_HEAD);
		}

		case TV_GLOVES:
		{
			return (INVEN_HANDS);
		}

		case TV_BOOTS:
		{
			return (INVEN_FEET);
		}

		/* Ammo asks for first quiver slot. */
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (INVEN_Q1);
		}

		case TV_POUCH:
		{
		    return (INVEN_POUCH);
		}

	}

	/* No slot available */
	return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 */
cptr mention_use(int i)
{
	cptr p;

	/* Examine the location */
	switch (i)
	{
		case INVEN_WIELD: p = "Wielding";       break;
		case INVEN_BOW:   p = "Shooting";       break;
		case INVEN_LEFT:  p = "On left hand";   break;
		case INVEN_RIGHT: p = "On right hand";  break;
		case INVEN_NECK:  p = "Around neck";    break;
		case INVEN_LITE:  p = "Light source";   break;
		case INVEN_BODY:  p = "On body";        break;
		case INVEN_OUTER: p = "About body";     break;
		case INVEN_ARM:
		{
			if (is_any_weapon(&inventory[INVEN_ARM]))
			{
				p = "Second weapon";
			}
			else
			{
				if (p_ptr->shield_on_back) p = "On back";
				else p = "On arm";
			}
			break;
		}
		case INVEN_HEAD:  p = "On head";        break;
		case INVEN_HANDS: p = "On hands";       break;
		case INVEN_FEET:  p = "On feet";        break;
		case INVEN_Q0:	case INVEN_Q1:	case INVEN_Q2:	case INVEN_Q3:
		case INVEN_Q4:	case INVEN_Q5:	case INVEN_Q6:
		case INVEN_Q7:	case INVEN_Q8:	case INVEN_Q9:
		{
			p = "In quiver"; break;
		}

		default:	  p = "In pack";   break;
	}

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		object_type *o_ptr;

		o_ptr = &inventory[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == INVEN_BOW)
	{
		object_type *o_ptr;

		o_ptr = &inventory[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just holding";
		}
	}

	/* Return the result */
	return (p);
}


/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr describe_use(int i)
{
	cptr p;

	switch (i)
	{
		case INVEN_WIELD: p = "attacking monsters with"; break;
		case INVEN_BOW:   p = "shooting missiles with"; break;
		case INVEN_LEFT:  p = "wearing on your left hand"; break;
		case INVEN_RIGHT: p = "wearing on your right hand"; break;
		case INVEN_NECK:  p = "wearing around your neck"; break;
		case INVEN_LITE:  p = "using to light the way"; break;
		case INVEN_BODY:  p = "wearing on your body"; break;
		case INVEN_OUTER: p = "wearing on your back"; break;
		case INVEN_ARM:
		{
			if (inventory[INVEN_ARM].tval == TV_SHIELD)
			{
				if (p_ptr->shield_on_back) p = "carrying on your back";
				else p = "wearing on your arm";
			}
			else
			{
				p = "also attacking with";
			}
			break;
		}

		case INVEN_HEAD:  p = "wearing on your head"; break;
		case INVEN_HANDS: p = "wearing on your hands"; break;
		case INVEN_FEET:  p = "wearing on your feet"; break;
		case INVEN_Q0:	case INVEN_Q1:	case INVEN_Q2:	case INVEN_Q3:
		case INVEN_Q4:	case INVEN_Q5:	case INVEN_Q6:
		case INVEN_Q7:	case INVEN_Q8:	case INVEN_Q9:
		{
			p = "carrying in your quiver"; break;
		}

		case -1:          p = "firing"; break;
		case -2:          p = "throwing"; break;

		default:	  p = "carrying in your pack"; break;
	}
	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		object_type *o_ptr;

		o_ptr = &inventory[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "just lifting";
		}
	}
	/* Hack -- Heavy bow */
	if (i == INVEN_BOW)
	{
		object_type *o_ptr;

		o_ptr = &inventory[i];
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "just holding";
		}
	}

	/* Return the result */
	return (p);
}



/*
 * Check an item against the item tester info
 */
bool item_tester_okay(const object_type *o_ptr)
{
	/* Hack -- allow listing empty slots */
	if (item_tester_full) return (TRUE);

	/* Normally require an item */
	if (!o_ptr->k_idx) return (FALSE);

	/* Ignore "gold" */
	if (o_ptr->tval == TV_GOLD) return (FALSE);

	/* Ignore unmarked items on the floor */
	if ((!o_ptr->marked) && ((o_ptr->iy) || (o_ptr->ix))) return (FALSE);

	/* Check the tval */
	if (item_tester_tval)
	{
		if (item_tester_tval != o_ptr->tval) return (FALSE);
	}

	/* Check the hook */
	if (item_tester_hook)
	{
		if (!(*item_tester_hook) (o_ptr)) return (FALSE);
	}

	/* Test slot directly */
	if (slot_tester_hook)
	{
		/* Allow certain slots, depending on object tval */
		return ((*slot_tester_hook) (wield_slot(o_ptr)));
	}

	/* Assume okay */
	return (TRUE);
}


/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	int i, n, z = 0;
	int w, h;

	object_type *o_ptr;

	byte attr = TERM_WHITE;

	char tmp_val[DESC_LEN];
	char o_name[DESC_LEN];


	/* Get the terminal window size */
	(void)Term_get_size(&w, &h);


	/* Find the "final" slot */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the pack */
	for (i = 0; i < z; i++)
	{
		/* Examine the item */
		o_ptr = &inventory[i];

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
		(void)Term_putstr(0, i, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Display the entry itself */
		(void)Term_putstr(3, i, n, attr, o_name);

		/* Erase the rest of the line */
		(void)Term_erase(3 + n, i, 255);

		/* Display the weight if needed (and there's enough space) */
		if ((w >= 75) && (show_weights) && (o_ptr->weight))
		{
			/* Allow only the size that fits on the screen */
			int size = 9 + MIN(0, w - 80);

			/* Calculate size, make metric, and size to fit */
			int wgt = o_ptr->weight * o_ptr->number;
			if (use_metric) wgt = make_metric(wgt);
			if (wgt > ((size >= 6) ? 9999 : 999)) wgt = ((size >= 6) ? 9999 : 999);

			/* Format the string */
			(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d", wgt / 10, wgt % 10);
			if (size >= 9)
			{
				if (use_metric) strcat(tmp_val, " kg");
				else            strcat(tmp_val, " lb");
			}

			/* Display the weight */
			(void)Term_putstr(w - size, i, -1, TERM_WHITE, tmp_val);
		}
	}

	/* Erase the rest of the window */
	clear_from(z);
}



/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
	int i, n;
	int w, h;

	object_type *o_ptr;

	byte attr = TERM_WHITE;

	char tmp_val[DESC_LEN];
	char o_name[DESC_LEN];


	/* Get the terminal window size */
	(void)Term_get_size(&w, &h);


	/* Display the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Examine the item */
		o_ptr = &inventory[i];

		/* Hack -- never show empty quiver slots. */
		if ((!o_ptr->k_idx) && (i >= INVEN_Q1) && (i <= INVEN_Q0))
		{
			/* Clear the line, skip to next slot */
			clear_row(i - INVEN_WIELD);
			continue;
		}

		/* Hack -- never show empty pouch. */
		if (i == INVEN_POUCH)
		{
			int j, count;

			/* Get the number of essence types and total quantity */
			for (count = 0, j = 0; j < NUM_ESSENCE; j++)
			{
				count += p_ptr->essence[j];
			}

			/* Empty slot -- Clear the line, skip to next slot */
			if (!count)
			{
				clear_row(i - INVEN_WIELD);
				continue;
			}
		}

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
		(void)Term_putstr(0, i - INVEN_WIELD, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Display the entry itself */
		(void)Term_putstr(3, i - INVEN_WIELD, n, attr, o_name);

		/* Erase the rest of the line */
		(void)Term_erase(3 + n, i - INVEN_WIELD, 255);

		/* Display the slot description if needed (and there's enough space) */
		if ((show_labels) && (w >= 70))
		{
			(void)Term_putstr(61, i - INVEN_WIELD, -1, TERM_WHITE, "<--");
			(void)Term_putstr(65, i - INVEN_WIELD, -1, TERM_WHITE, mention_use(i));
		}

		/* Display the weight if needed (and there's enough space) */
		if ((w >= 75) && (show_weights) && (o_ptr->weight))
		{
			/* Allow only the size that fits on the screen */
			int size = 9 + MIN(0, w - 80);

			/* Calculate size, make metric, and size to fit */
			int wgt = o_ptr->weight * o_ptr->number;
			if (use_metric) wgt = make_metric(wgt);
			if (wgt > ((size >= 6) ? 9999 : 999)) wgt = ((size >= 6) ? 9999 : 999);

			/* Format the string */
			(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d", wgt / 10, wgt % 10);
			if (size >= 9)
			{
				if (use_metric) strcat(tmp_val, " kg");
				else            strcat(tmp_val, " lb");
			}

			/* Display the weight */
			(void)Term_putstr(w - size, i, -1, TERM_WHITE, tmp_val);
		}
	}

	/* Erase the rest of the window */
	clear_from(INVEN_TOTAL - INVEN_WIELD);
}



/*
 * Display the inventory.
 */
void show_inven(void)
{
	int i, j, k, l, z = 0;
	int col, len, lim, l_margin;

	object_type *o_ptr;

	char o_name[DESC_LEN];

	char tmp_val[DESC_LEN];

	int out_index[INVEN_PACK+1];
	byte out_color[INVEN_PACK+1];
	char out_desc[INVEN_PACK+1][DESC_LEN];

	/* Count number of missiles in the quiver slots. */
	int ammo_num = quiver_count();


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions (columns - 4) */
	lim = Term->cols - 4;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Find the "final" slot */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Track */
		z = i + 1;
	}

	/* Display the inventory */
	for (k = 0, i = 0; i < z; i++)
	{
		o_ptr = &inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Save the object description */
		strcpy(out_desc[k], o_name);

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 4;

		/* Be sure to account for the weight */
		if (show_weights) l += 9;

		/* Maintain the maximum length */
		if (l > len) len = l;

		/* Advance to next "line" */
		k++;
	}

	/* Find the column to start in (move towards the right of the screen) */
	col = MAX(0, (Term->cols - 80) * 2 / 3);

	/* Remember the left margin (one-space margin unless at left edge already) */
	l_margin = MAX(0, col - 1);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the text area */
		put_str(format("%*s", len+1, ""), j+1, l_margin);

		/* Prepare an index --(-- */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%c)", index_to_label(i));

		/* Clear the line with the (possibly indented) index */
		if (o_ptr->ident & (IDENT_MENTAL))
		{
			c_put_str(TERM_L_BLUE, tmp_val, j+1, col);
		}
		else if ((object_known_p(o_ptr)) || (object_aware_p(o_ptr)))
		{
			put_str(tmp_val, j+1, col);
		}
		else
		{
			c_put_str(TERM_L_WHITE, tmp_val, j+1, col);
		}

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;

			if (use_metric)
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d kg",
					make_metric(wgt) / 10, make_metric(wgt) % 10);
			}
			else
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d lb", wgt / 10, wgt % 10);
			}

			put_str(tmp_val, j + 1, col + len - 9);
		}
	}


	/*
	 * Add notes about slots used by the quiver, if we have space, want
	 * to show all slots, and have items in the quiver.
	 */
	if ((p_ptr->pack_size_reduce) && (item_tester_full) &&
		(j <= (INVEN_PACK - p_ptr->pack_size_reduce)))
	{
		/* Insert a blank dividing line, if we have the space. */
		if (j <= ((INVEN_PACK - 1) - p_ptr->pack_size_reduce))
		{
			j++;
			put_str(format("%*s", len+1, ""), j, l_margin);
		}

		for (i = p_ptr->pack_size_reduce; i >= 1; i--)
		{
			/* Go to next line. */
			j++;

			/* Clear the text area */
			put_str(format("%*s", len+1, ""), j, l_margin);

			/* Determine index, print it out. */
			(void)strnfmt(tmp_val, sizeof(tmp_val), "%c)", index_to_label(INVEN_PACK - i));
			put_str(tmp_val, j, col);

			/* Note amount of ammo */
			k = (ammo_num > 99) ? 99 : ammo_num;

			/* Hack -- use "(Ready Ammunition)" as a description. */
			c_put_str(TERM_BLUE, format("(Ready Ammunition) [%d]", k), j, col + 3);

			/* Reduce ammo count */
			ammo_num -= k;
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < Term->rows - 1)) put_str(format("%*s", len+1, ""), j + 1, l_margin);
}


/*
 * Display the equipment.
 */
void show_equip(void)
{
	int i, j, k, l;
	int col, len, lim, l_margin;
	int essence_count = 0;

	object_type *o_ptr;

	char tmp_val[DESC_LEN];

	char o_name[DESC_LEN];

	int out_index[INVEN_PACK+1];
	byte out_color[INVEN_PACK+1];
	char out_desc[INVEN_PACK+1][DESC_LEN];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions (columns - 4) */
	lim = Term->cols - 4;

	/* Require space for labels (if needed) */
	if (show_labels) lim -= 16;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Scan the equipment list */
	for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Hack -- never show empty quiver slots. */
		if ((!o_ptr->k_idx) && (i >= INVEN_Q1) && (i <= INVEN_Q0))
		{
			/* Skip to next slot */
			continue;
		}

		/* Hack -- never show empty pouch. */
		if (i == INVEN_POUCH)
		{
			/* Get the number of essence types and total quantity */
			for (j = 0; j < NUM_ESSENCE; j++)
			{
				essence_count += p_ptr->essence[j];
			}

			/* Skip empty pouch */
			if (!essence_count)
			{
				/* Save the index */
				out_index[k] = i;
				k++;
				continue;
			}
		}

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Truncate the description */
		o_name[lim] = 0;

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Save the description */
		strcpy(out_desc[k], o_name);

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (show_labels) l += 16;

		/* Increase length for weight (if needed) */
		if (show_weights) l += 9;

		/* Maintain the max-length */
		if (l > len) len = l;

		/* Advance the entry */
		k++;
	}

	/* Find the column to start in (move towards the right of the screen) */
	col = MAX(0, (Term->cols - 80) * 2 / 3);

	/* Remember the left margin (one-space margin unless at left edge already) */
	l_margin = MAX(0, col - 1);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the text area */
		put_str(format("%*s", len+1, ""), j+1, l_margin);

		/* Leave a blank line if pouch is empty */
		if ((i == INVEN_POUCH) && (!essence_count)) continue;

		/* Prepare an index --(-- */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%c)", index_to_label(i));

		/* Clear the line with the (possibly indented) index */
		if (o_ptr->ident & (IDENT_MENTAL))
		{
			c_put_str(TERM_L_BLUE, tmp_val, j+1, col);
		}
		else if ((!o_ptr) || (object_known_p(o_ptr)) || (object_aware_p(o_ptr)))
		{
			put_str(tmp_val, j+1, col);
		}
		else
		{
			c_put_str(TERM_L_WHITE, tmp_val, j+1, col);
		}

		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
			(void)strnfmt(tmp_val, sizeof(tmp_val), "%-14s: ", mention_use(i));
			put_str(tmp_val, j+1, col + 3);

			/* Display the entry itself */
			c_put_str(out_color[j], out_desc[j], j+1, col + 3 + 14 + 2);
		}

		/* No labels */
		else
		{
			/* Display the entry itself */
			c_put_str(out_color[j], out_desc[j], j+1, col + 3);
		}


		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;

			if (use_metric)
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d kg",
					make_metric(wgt) / 10, make_metric(wgt) % 10);
			}
			else
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d lb", wgt / 10, wgt % 10);
			}

			put_str(tmp_val, j+1, col + len - 9);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < Term->rows - 1)) put_str(format("%*s", len+1, ""), j + 1, l_margin);
}

/*
 * Get the indexes of objects at a given floor location. -TNB-
 *
 * Return the number of object indexes acquired.
 *
 * Valid flags are any combination of the bits:
 *   0x01 -- Verify item tester
 *   0x02 -- Marked items only
 *   0x04 -- Only the top item
 */
bool scan_floor(int *items, int *item_num, int y, int x, int mode)
{
	int this_o_idx, next_o_idx;

	int num = 0;

	(*item_num) = 0;

	/* Sanity */
	if (!in_bounds(y, x)) return (FALSE);

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Item tester */
		if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

		/* Marked */
		if ((mode & 0x02) && !o_ptr->marked) continue;

		/* Accept this item */
		items[num++] = this_o_idx;

		/* Only one */
		if (mode & 0x04) break;

		/* XXX Hack -- Enforce limit */
		if (num == MAX_FLOOR_STACK) break;
	}

	/* Number of items */
	(*item_num) = num;

	/* Result */
	return (num != 0);
}


/*
 * Display a list of the items on the floor at the given location. -TNB-
 */
void show_floor(const int *floor_list, int floor_num, bool gold, bool blind)
{
	int i, j, k, l;
	int col, len, lim, l_margin;

	object_type *o_ptr;

	char o_name[DESC_LEN];

	char tmp_val[DESC_LEN];

	int out_index[MAX_FLOOR_STACK];
	byte out_color[MAX_FLOOR_STACK];
	char out_desc[MAX_FLOOR_STACK][DESC_LEN];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions (columns - 4) */
	lim = Term->cols - 4;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Limit displayed floor items to 23 (screen limits) */
	if (floor_num > MAX_FLOOR_STACK) floor_num = MAX_FLOOR_STACK;

	/* Display the floor */
	for (k = 0, i = 0; i < floor_num; i++)
	{
		o_ptr = &o_list[floor_list[i]];

		/* Optionally, show gold */
		if ((o_ptr->tval != TV_GOLD) || (!gold))
		{
			/* Is this item acceptable?  (always rejects gold) */
			if (!item_tester_okay(o_ptr)) continue;
		}

		/* Describe the object.  Less detail if blind. */
		if (blind) object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
		else       object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		if (blind) out_color[k] = TERM_SLATE;
		else out_color[k] = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Save the object description */
		strcpy(out_desc[k], o_name);

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Be sure to account for the weight */
		if (show_weights) l += 9;

		/* Maintain the maximum length */
		if (l > len) len = l;

		/* Advance to next "line" */
		k++;
	}

	/* Find the column to start in (move towards the right of the screen) */
	col = MAX(0, (Term->cols - 80) * 2 / 3);

	/* Remember the left margin (one-space margin unless at left edge already) */
	l_margin = MAX(0, col - 1);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = floor_list[out_index[j]];

		/* Get the item */
		o_ptr = &o_list[i];

		/* Clear the text area */
		put_str(format("%*s", len+1, ""), j+1, l_margin);

		/* Prepare an index --(-- */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "%c)", index_to_label(j));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j + 1, col);

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;

			/* Allow conversion to metric units */
			if (use_metric)
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d kg",
					make_metric(wgt) / 10, make_metric(wgt) % 10);
			}
			else
			{
				(void)strnfmt(tmp_val, sizeof(tmp_val), "%3d.%1d lb", wgt / 10, wgt % 10);
			}

			put_str(tmp_val, j + 1, col + len - 9);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < MAX_FLOOR_STACK)) put_str(format("%*s", len+1, ""), j + 1, l_margin);
}


/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < TERM_MAX; j++)
	{
		/* No term, or term is unavailable */
		if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

		/* Flip inven to equip */
		if (op_ptr->window_flag[j] & (PW_INVEN))
		{
			/* Flip flags */
			op_ptr->window_flag[j] &= ~(PW_INVEN);
			op_ptr->window_flag[j] |= (PW_EQUIP);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}

		/* Flip inven to equip */
		else if (op_ptr->window_flag[j] & (PW_EQUIP))
		{
			/* Flip flags */
			op_ptr->window_flag[j] &= ~(PW_EQUIP);
			op_ptr->window_flag[j] |= (PW_INVEN);

			/* Window stuff */
			p_ptr->window |= (PW_INVEN);
		}
	}
}


/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify_item(cptr prompt, int item)
{
	char o_name[DESC_LEN];

	char out_val[DESC_LEN];

	object_type *o_ptr;

	/* Inventory */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Floor */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

	/* Prompt */
	(void)strnfmt(out_val, sizeof(out_val), "%s %s?", prompt, o_name);

	/* Query */
	return (get_check(out_val));
}



/*
 * Hack -- allow user to "prevent" certain choices
 *
 * The item can be negative to mean "item on floor".
 */
static bool get_item_allow(int item)
{
	cptr s;
	object_type *o_ptr;

	/* Inventory */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Floor */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* No inscription */
	if (!o_ptr->note) return (TRUE);

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->note), '!');

	/* Process preventions */
	while (s)
	{
		/* Check the "restriction" */
		if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
		{
			/* Verify the choice */
			if (!verify_item("Really try", item)) return (FALSE);
		}
		/* Find another '!' */
		s = strchr(s + 1, '!');
	}

	/* Allow it */
	return (TRUE);
}



/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(int item)
{
	object_type *o_ptr;

	/* Ignore illegal items */
	if (item >= INVEN_TOTAL) return (FALSE);

	/* Test slot directly */
	if (slot_tester_hook)
	{
		return ((*slot_tester_hook) (item));
	}

	/* Inventory */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Floor */
	else
	{
		o_ptr = &o_list[0 - item];

		/* If you don't know about it, you can't manipulate it */
		if (!o_ptr->marked) return (FALSE);
	}

	/* Verify the item */
	return (item_tester_okay(o_ptr));
}


/*
 * Scan for tags on a given object.
 */
static bool get_tag_aux(int *cp, char tag, int item, object_type *o_ptr)
{
	cptr s;

	/* Skip non-objects */
	if (!o_ptr->k_idx) return (FALSE);

	/* Skip items not of the required tval. */
	if ((item_tester_tval) && (o_ptr->tval != item_tester_tval)) return (FALSE);

	/* Skip empty inscriptions */
	if (!o_ptr->note) return (FALSE);


	/* Find an '@' */
	s = strchr(quark_str(o_ptr->note), '@');

	/* Process all tags */
	while (s)
	{
		/* Check the normal tags -- only when tag is a number */
		if ((s[1] == tag) && (isdigit(tag)))
		{
			/* Save the actual inventory ID */
			*cp = item;

			/* Success */
			return (TRUE);
		}

		/* Check the special tags */
		if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))
		{
			/* Save the actual inventory ID */
			*cp = item;

			/* Success */
			return (TRUE);
		}

		/* Find another '@' */
		s = strchr(s + 1, '@');
	}

	/* No tag found */
	return (FALSE);
}

/*
 * Determine how many suitable items are available
 */
int scan_inven(int left, int right)
{
	int i, count = 0;
	for(i = left; i <= right; i++)
	{
		if (get_item_okay(i)) count++;
	}
	return count;
}

/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a char "n" appearing as "@n" anywhere in the
 * inscription of an object.
 *
 * Also, the tag "@xn" will work as well, where "n" is a tag-char,
 * and "x" is the "current" p_ptr->command_cmd code.
 */
static bool get_tag(int *cp, char tag,
	bool allow_equip, bool allow_inven, bool allow_floor)
{
	int i;

	/* Check equipment */
	if ((allow_equip) || (p_ptr->command_wrk == (USE_EQUIP)))
	{
		for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
		{
			/* Check the tag; if found, report success */
			if (get_tag_aux(cp, tag, i, &inventory[i])) return (TRUE);
		}
	}

	/* Check the pack */
	if ((allow_inven) || (p_ptr->command_wrk == (USE_INVEN)))
	{
		for (i = 0; i < INVEN_PACK; i++)
		{
			/* Check the tag; if found, report success */
			if (get_tag_aux(cp, tag, i, &inventory[i])) return (TRUE);
		}
	}

	/* Look on the floor */
	if ((allow_floor) || (p_ptr->command_wrk == (USE_FLOOR)))
	{
		int this_o_idx;

		/* Scan all objects in the grid */
		for (this_o_idx = cave_o_idx[p_ptr->py][p_ptr->px]; this_o_idx;)
		{
			/* Check the tag; if found, report success */
			if (get_tag_aux(cp, tag, (0 - this_o_idx), &o_list[this_o_idx]))
				return (TRUE);

			/* Get the next object */
			this_o_idx = o_list[this_o_idx].next_o_idx;
		}
	}

	/* No such tag */
	return (FALSE);
}



/*
 * Let the user select an item, save its "index"
 *
 * Return TRUE only if an acceptable item was chosen by the user.
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * Any of these are displayed (even if no acceptable items are in that
 * location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor.  The
 * use of "capital" letters will "examine" an inventory, equipment, or floor
 * item, and prompt for its use.
 *
 * If a legal item is selected from the inventory, we save it in "cp"
 * directly (0 to 35), and return TRUE.
 *
 * If a legal item is selected from the floor, we save it in "cp" as
 * a negative (-1 to -511), and return TRUE.
 *
 * If no item is available, we do nothing to "cp", and we display a
 * warning message, using "str" if available, and return FALSE.
 *
 * If no item is selected, we do nothing to "cp", and return FALSE.
 *
 * Global "p_ptr->command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * Global "p_ptr->command_see" may be set before calling this function to start
 * out in "browse" mode.  It is cleared before this function returns.
 *
 * Global "p_ptr->command_wrk" is used to choose between equip/inven/floor
 * listings.  It is equal to USE_INVEN or USE_EQUIP or USE_FLOOR, except
 * when this function is first called, when it is equal to zero, which will
 * cause it to be set to USE_INVEN.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 *
 * This function has been revised using code from Tim Baker's Easy Patch 1.2
 *
 * Note that only "acceptable" floor objects get indexes, so between two
 * commands, the indexes of floor objects may change.  XXX XXX XXX
 */
bool get_item(int *cp, cptr pmt, cptr str, int mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char which = ' ';

	int j, k, i1, i2, e1, e2;
	int f1, f2;

	bool done, item;

	bool oops = FALSE;

	bool use_inven = ((mode & (USE_INVEN)) ? TRUE : FALSE);
	bool use_equip = ((mode & (USE_EQUIP)) ? TRUE : FALSE);
	bool use_floor = ((mode & (USE_FLOOR)) ? TRUE : FALSE);

	bool allow_equip = FALSE;
	bool allow_inven = FALSE;
	bool allow_floor = FALSE;

	bool blind = ((p_ptr->blind) || (no_light() && (p_ptr->see_infra == 0)));

	bool toggle = FALSE;

	char tmp_val[DESC_LEN];
	char out_val[DESC_LEN];

	int floor_list[MAX_FLOOR_STACK];
	int floor_num = 0;
	int equip_num = 0;
	int inven_num = 0;

	/* Get the item index */
	if (repeat_pull(cp))
	{
		/* Verify the item */
		if (get_item_okay(*cp))
		{
			/* Forget the item_tester_tval restriction */
			item_tester_tval = 0;

			/* Forget the item_tester_hook restriction */
			item_tester_hook = NULL;

			/* Forget the slot_tester_hook restriction */
			slot_tester_hook = NULL;

			/* Success */
			return (TRUE);
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	/* Paranoia XXX XXX XXX */
	message_flush();


	/* Not done */
	done = FALSE;

	/* No item selected */
	item = FALSE;


	/* Full inventory */
	i1 = 0;
	i2 = INVEN_PACK - 1;

	/* Forbid inventory */
	if (!use_inven) i2 = -1;

	/* Restrict inventory indexes */
	while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
	while ((i1 <= i2) && (!get_item_okay(i2))) i2--;

	/* Accept inventory */
	if (i1 <= i2) allow_inven = TRUE;

	/* Full equipment */
	e1 = INVEN_WIELD;
	e2 = INVEN_TOTAL - 1;

	/* Forbid equipment */
	if (!use_equip) e2 = -1;

	/* Restrict equipment indexes */
	while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
	while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

	/* Accept equipment */
	if (e1 <= e2) allow_equip = TRUE;

	/* Restrict floor usage */
	if (mode & (USE_FLOOR))
	{
		/* Scan all marked objects in the grid */
		(void)scan_floor(floor_list, &floor_num, py, px, 0x03);
	}

	/* Count acceptable options */
	if (mode & (USE_EQUIP)) equip_num = scan_inven(e1, e2);
	if (mode & (USE_INVEN)) inven_num = scan_inven(i1, i2);

	/* Full floor */
	f1 = 0;
	f2 = floor_num - 1;

	/* Forbid floor */
	if (!use_floor) f2 = -1;

	/* Restrict floor indexes */
	while ((f1 <= f2) && (!get_item_okay(0 - floor_list[f1]))) f1++;
	while ((f1 <= f2) && (!get_item_okay(0 - floor_list[f2]))) f2--;

	/* Accept floor */
	if (f1 <= f2) allow_floor = TRUE;

	/* If requested and only one option, automatically select it */
	if ((mode & USE_AUTO) && (equip_num + inven_num + floor_num == 1))
	{
		if (equip_num == 1) *cp = e1;
		if (inven_num == 1) *cp = i1;
		if (floor_num == 1) *cp = 0 - floor_list[0];

		item_tester_hook = NULL;
		return (TRUE);
	}

	/* Require at least one legal choice */
	if (!allow_inven && !allow_equip && !allow_floor)
	{
		/* Cancel p_ptr->command_see */
		p_ptr->command_see = FALSE;

		/* Oops */
		oops = TRUE;

		/* Done */
		done = TRUE;
	}

	/* Analyze choices */
	else
	{
		/* Hack -- Start on equipment if requested */
		if (p_ptr->command_see && (p_ptr->command_wrk == (USE_EQUIP))
			&& use_equip)
		{
			p_ptr->command_wrk = (USE_EQUIP);
		}

		/* Hack -- Start on equipment if for shooting */
		else if ((p_ptr->command_cmd == 'f') && allow_equip)
		{
			p_ptr->command_wrk = (USE_EQUIP);
		}

		/* Hack -- Start on floor if a repeated item destroy command */
		else if ((p_ptr->command_cmd == 'k') && allow_floor && p_ptr->command_rep)
		{
			p_ptr->command_wrk = (USE_FLOOR);
		}

		/* Use inventory if allowed. */
		else if (use_inven && allow_inven)
		{
			p_ptr->command_wrk = (USE_INVEN);
		}

		/* Use equipment if allowed */
		else if (use_equip && allow_equip)
		{
			p_ptr->command_wrk = (USE_EQUIP);
		}

		/* Use floor if allowed */
		else if (allow_floor)
		{
			p_ptr->command_wrk = (USE_FLOOR);
		}

		/* Hack -- Use (empty) inventory if no other choices available. */
		else
		{
			p_ptr->command_wrk = (USE_INVEN);
		}
	}

	/* Option to always show a list */
	if (always_show_list)
	{
		p_ptr->command_see = TRUE;
	}


	/* Look up the "universal" tag  -AD- */
	if (get_tag(&k, '*', allow_equip, allow_inven, allow_floor))
	{
		/*Hack -- Validate the item */
		if ((k < INVEN_WIELD) ? !allow_inven : !allow_equip)
		{
			bell("Illegal object choice (tag)!");
		}

		/* Validate the item */
		else if (!get_item_okay(k))
		{
			bell("Illegal object choice (tag)!");
		}

		/* Allow player to "refuse" certain actions */
		else if (!get_item_allow(k))
		{
			/* Nothing */
		}

		else
		{
			/* Accept that choice */
			(*cp) = k;
			item = TRUE;
			done = TRUE;
			p_ptr->command_see = FALSE;
		}
	}


	/* Start out in "display" mode */
	if (p_ptr->command_see)
	{
		/* Save screen */
		screen_save(FALSE);
	}

	/* Repeat until done */
	while (!done)
	{
		/* Show choices */
		int ni = 0;
		int ne = 0;

		/* Scan windows */
		for (j = 0; j < TERM_MAX; j++)
		{
			/* No term, or term is unavailable */
			if ((!angband_term[j]) || (!angband_term[j]->mapped_flag)) continue;

			/* Count windows displaying inven */
			if (op_ptr->window_flag[j] & (PW_INVEN)) ni++;

			/* Count windows displaying equip */
			if (op_ptr->window_flag[j] & (PW_EQUIP)) ne++;
		}

		/* Toggle if needed */
		if (((p_ptr->command_wrk == (USE_EQUIP)) && ni && !ne) ||
		    ((p_ptr->command_wrk == (USE_INVEN)) && !ni && ne))
		{
			/* Toggle */
			toggle_inven_equip();

			/* Track toggles */
			toggle = !toggle;
		}

		/* Update */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Redraw windows */
		window_stuff();


		/* Viewing inventory */
		if (p_ptr->command_wrk == (USE_INVEN))
		{
			/* Redraw if needed */
			if (p_ptr->command_see) show_inven();

			/* Begin the prompt */
			(void)strnfmt(out_val, sizeof(out_val), "Inven:");


			/* Indicate lack of inventory choices. */
			if (i1 > i2) (void)strnfmt(tmp_val, sizeof(tmp_val), " (none),");

			/* List choices. */
			else (void)strnfmt(tmp_val, sizeof(tmp_val), " %c-%c, '.' for 1st item,",
				index_to_label(i1), index_to_label(i2));

			/* Append choices. */
			(void)my_strcat(out_val, tmp_val, sizeof(out_val));

			/* Indicate ability to "view" */
			if (!p_ptr->command_see)
				(void)my_strcat(out_val, " * to see,", sizeof(out_val));

			/* Indicate that equipment items are available */
			if (use_equip) (void)my_strcat(out_val, " / for equip,",
				sizeof(out_val));

			/* Indicate that floor items are available */
			if (allow_floor) (void)my_strcat(out_val, " - for floor,",
				sizeof(out_val));
		}

		/* Viewing equipment */
		else if (p_ptr->command_wrk == (USE_EQUIP))
		{
			/* Redraw if needed */
			if (p_ptr->command_see) show_equip();

			/* Begin the prompt */
			(void)strnfmt(out_val, sizeof(out_val), "Equip:");


			/* Indicate lack of equipment choices. */
			if (e1 > e2) (void)strnfmt(tmp_val, sizeof(tmp_val), " (none),");

			/* List choices. */
			else (void)strnfmt(tmp_val, sizeof(tmp_val), " %c-%c, '.' for 1st item,",
				index_to_label(e1), index_to_label(e2));

			/* Append choices. */
			(void)my_strcat(out_val, tmp_val, sizeof(out_val));

			/* Indicate ability to "view" */
			if (!p_ptr->command_see)
				(void)my_strcat(out_val, " * to see,", sizeof(out_val));

			/* Append */
			if (use_inven) (void)my_strcat(out_val, " / for inven,", sizeof(out_val));

			/* Append */
			if (allow_floor) (void)my_strcat(out_val, " - for floor,", sizeof(out_val));
		}

		/* Viewing floor */
		else if (p_ptr->command_wrk == (USE_FLOOR))
		{
			/* Redraw if needed */
			if (p_ptr->command_see) show_floor(floor_list, floor_num, FALSE, blind);

			/* Begin the prompt */
			(void)strnfmt(out_val, sizeof(out_val), "Floor:");

			/* Indicate lack of floor choices. */
			if (f1 > f2) (void)strnfmt(tmp_val, sizeof(tmp_val), " (none),");

			/* List choices. */
			else (void)strnfmt(tmp_val, sizeof(tmp_val), " %c-%c, ',' for 1st item,", I2A(f1-f1), I2A(f2-f1));

			/* Append */
			(void)my_strcat(out_val, tmp_val, sizeof(out_val));

			/* Indicate ability to "view" */
			if (!p_ptr->command_see)
				(void)my_strcat(out_val, " * to see,", sizeof(out_val));

			/* Append */
			if (use_inven)
			{
				(void)my_strcat(out_val, " / for inven,", sizeof(out_val));
			}
			else if (use_equip)
			{
				(void)my_strcat(out_val, " / for equip,", sizeof(out_val));
			}
		}

		/* Indicate that help is available */
		if (p_ptr->get_help_index)
		{
			(void)my_strcat(out_val, " ?,", sizeof(out_val));
		}

		/* Finish the prompt */
		(void)my_strcat(out_val, " ESC", sizeof(out_val));

		/* Build the prompt (add a space) */
		(void)strnfmt(tmp_val, sizeof(tmp_val), "(%s) %s ", out_val, pmt);

		/* Show the prompt */
		prt(tmp_val, 0, 0);

		/* Get a key */
		which = inkey(FALSE);

		/* Parse it */
		switch (which)
		{
			case ESCAPE:
			{
				done = TRUE;
				break;
			}

			/* Allow context-specific help */
			case '?':
			{
				/* Show help text if specified */
				if (p_ptr->get_help_index)
				{
					/* Hack -- Hide the list temporarily */
					if (p_ptr->command_see) screen_load();

					/* Show contextual help */
					do_cmd_help();

					/* Hack -- Show the list again */
					if (p_ptr->command_see) screen_save(FALSE);

					/* Await next command */
					break;
				}

				/* Otherwise, treat this command as a '*' or ' ' */
			}
			case '*':
			case ' ':
			{
				/* Hide the list */
				if (p_ptr->command_see)
				{
					/* Flip flag */
					p_ptr->command_see = FALSE;

					/* Load screen */
					screen_load();
				}

				/* Show the list */
				else
				{
					/* Save screen */
					screen_save(FALSE);

					/* Flip flag */
					p_ptr->command_see = TRUE;
				}
				break;
			}

			case '/':
			{
				/* Toggle to inventory */
				if (use_inven && (p_ptr->command_wrk != (USE_INVEN)))
				{
					p_ptr->command_wrk = (USE_INVEN);
				}

				/* Toggle to equipment */
				else if (use_equip && (p_ptr->command_wrk != (USE_EQUIP)))
				{
					p_ptr->command_wrk = (USE_EQUIP);
				}

				/* No toggle allowed; ignore command */
				else
				{
					break;
				}

				/* Hack -- Fix screen */
				if (p_ptr->command_see)
				{
					/* Load screen */
					screen_load();

					/* Save screen */
					screen_save(FALSE);
				}

				break;
			}

			case '-':
			{
				/* Ignore the command if we cannot use the floor */
				if (!allow_floor) break;

				/* Hack -- Fix screen */
				if (p_ptr->command_see)
				{
					/* Load screen */
					screen_load();

					/* Save screen */
					screen_save(FALSE);
				}

				/* Use the floor */
				p_ptr->command_wrk = (USE_FLOOR);

				break;
			}

			/* Select the first legal item.  -BR- */
			case '.':
			case ',':
			{
				/* Hack -- Auto-Select */
				if (p_ptr->command_wrk == (USE_FLOOR))
				{
					/* Special index */
					k = 0 - floor_list[0];
				}

				/* Never select the inventory when typing ',' */
				else if (which != ',')
				{
					/* Special index */
					k = (p_ptr->command_wrk == (USE_INVEN)) ? i1 : e1;
				}
				else
				{
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(k))
				{
					done = TRUE;
					break;
				}

				/* Accept that choice */
				(*cp) = k;
				item = TRUE;
				done = TRUE;

				break;
			}

			case '\n':
			case '\r':
			{
				/* Assume illegal item  XXX */
				k = 9999;

				/* Choose "default" inventory item */
				if (p_ptr->command_wrk == (USE_INVEN))
				{
					if (i1 == i2) k = i1;
				}

				/* Choose "default" equipment item */
				else if (p_ptr->command_wrk == (USE_EQUIP))
				{
					if (e1 == e2) k = e1;
				}

				/* Choose "default" floor item */
				else
				{
					if (f1 == f2) k = 0 - floor_list[f1];
				}

				/* Validate the item (refuse illegal) */
				if (!get_item_okay(k))
				{
					bell("Cannot choose a default item.");
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(k))
				{
					done = TRUE;
					break;
				}

				/* Accept that choice */
				(*cp) = k;
				item = TRUE;
				done = TRUE;
				break;
			}

			default:
			{
				bool verify;

				/* Find a tag that corresponds to "which" */
				if (get_tag(&k, which, FALSE, FALSE, FALSE))
				{
					/* More checks later */
				}

				/* We did not find a tag; use "which" as an index request */
				else
				{
					/* Convert letter to inventory index */
					if (p_ptr->command_wrk == (USE_INVEN))
					{
						k = label_to_inven(which);

						/* No index found */
						if (k < 0)
						{
							/* A number has been entered; check equip if allowed */
							if ((isdigit(which)) && (allow_equip))
							{
								k = label_to_equip(which);
							}
						}

						/* We (still) don't have a legal index */
						if (k < 0)
						{
							bell("Illegal object choice (inven)!");
							break;
						}
					}

					/* Convert letter to equipment index */
					else if (p_ptr->command_wrk == (USE_EQUIP))
					{
						k = label_to_equip(which);

						if (k < 0)
						{
							bell("Illegal object choice (equip)!");
							break;
						}
					}

					/* Convert letter to floor index */
					else
					{
						k = (islower((unsigned char)which) ? A2I(which) : -1);

						if (k < 0 || k >= floor_num)
						{
							bell("Illegal object choice (floor)!");
							break;
						}

						/* Special index */
						k = 0 - floor_list[k];
					}
				}


				/* Note verify */
				verify = (my_isupper((unsigned char)which) ? TRUE : FALSE);

				/* Lowercase */
				which = my_tolower((unsigned char)which);


				/* Validate the item */
				if (!get_item_okay(k))
				{
					bell("Illegal object choice!");
					break;
				}

				/* Verify the item */
				if (verify && !verify_item("Try", k))
				{
					done = TRUE;
					break;
				}

				/* Allow player to "refuse" certain actions */
				if (!get_item_allow(k))
				{
					done = TRUE;
					break;
				}

				/* Accept that choice */
				(*cp) = k;
				item = TRUE;
				done = TRUE;
				break;
			}
		}
	}

	/* Fix the screen if necessary */
	if (p_ptr->command_see)
	{
		/* Load screen */
		screen_load();

		/* Hack -- Cancel "display" */
		p_ptr->command_see = FALSE;
	}

	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;

	/* Forget the slot_tester_hook restriction */
	slot_tester_hook = NULL;


	/* Toggle again if needed */
	if (toggle) toggle_inven_equip();

	/* Update */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Window stuff */
	window_stuff();


	/* Clear the prompt line */
	prt("", 0, 0);

	/* Warning if needed */
	if (oops && str) msg_print(str);


	/* We have chosen something */
	if (item)
	{
		/* Save item if available */
		repeat_push(*cp);
	}

	/* Note deliberate cancellation -- when showing the main screen */
	else if ((!main_screen_inactive) && (!oops))
	{
		msg_print("Cancelled.");
	}

	/* Result */
	return (item);
}


/*
 * Determine if a given set of artifacts is being used by the player.
 */
bool check_set(int s_idx)
{
	int count = 0;
	int i;
	set_type *s_ptr = &s_info[s_idx];


	/* No set */
	if (s_idx <= 0) return (FALSE);

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Look for artifacts */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			/* Count all artifacts that belong to this set */
			if (a_ptr->set_index == s_idx) count++;
		}
	}

	/* Return TRUE if we have all the artifacts that belong to this set */
	return (count == s_ptr->no_of_items);
}

/*
 * Apply bonuses for complete artifact sets.
 *
 * This code is very hackish.
 */
void apply_set(int s_idx)
{
	set_type *s_ptr = &s_info[s_idx];

	bool bonus_applied = FALSE;

	int i, j;

	/* Scan the equipment */
	for (i = INVEN_WIELD; i < INVEN_SUBTOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Is it an artifact? */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			/* Is it in the correct set? */
			if (a_ptr->set_index == s_idx)
			{
				/* Loop through set elements */
				for (j = 0; j < (s_ptr->no_of_items); j++)
				{
					set_element *se_ptr = &s_ptr->set_items[j];

					/* Correct Element? */
					if (se_ptr->a_idx == o_ptr->artifact_index)
					{
						/* Bonus already applied? */
						if (!(a_ptr->set_bonus))
						{
							o_ptr->to_a = a_ptr->to_a + se_ptr->to_a;
							o_ptr->to_d = a_ptr->to_d + se_ptr->to_d;
							o_ptr->to_h = a_ptr->to_h + se_ptr->to_h;

							o_ptr->ac = a_ptr->ac + se_ptr->ac;
							o_ptr->dd = a_ptr->dd + se_ptr->dd;
							o_ptr->ds = a_ptr->ds + se_ptr->ds;

							o_ptr->pval  = a_ptr->pval1 + se_ptr->pval1;
							o_ptr->pval2 = a_ptr->pval2 + se_ptr->pval2;
							o_ptr->pval2 = a_ptr->pval3 + se_ptr->pval3;

							o_ptr->flags_pval1 |= (se_ptr->flags_pval1);
							o_ptr->flags_pval2 |= (se_ptr->flags_pval2);
							o_ptr->flags_pval3 |= (se_ptr->flags_pval3);

							o_ptr->flags1 |= (se_ptr->flags1);
							o_ptr->flags2 |= (se_ptr->flags2);
							o_ptr->flags3 |= (se_ptr->flags3);

							a_ptr->set_bonus = TRUE;
							bonus_applied = TRUE;
						}
					}
				}
			}
		}
	}

	/* Notify */
	if (bonus_applied)
	{
		msg_format("The %s set is complete!", s_ptr->name);
	}
}

/*
 * Remove bonuses for no-longer-complete artifact sets.
 */
cptr remove_set(int s_idx)
{
	set_type *s_ptr = &s_info[s_idx];
	bool bonus_removed = FALSE;
	byte i, j;

	/* Scan the equipment */
	for (i = INVEN_WIELD - 1; i < INVEN_SUBTOTAL; i++)
	{
		/* Get the object */
		object_type *o_ptr = &inventory[i];

		/* Is it an artifact? */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			/* Is it in the correct set? */
			if (a_ptr->set_index == s_idx)
			{
				/* Loop through set elements */
				for (j = 0; j < (s_ptr->no_of_items); j++)
				{
					set_element *se_ptr = &s_ptr->set_items[j];

					/* Correct Element? */
					if (se_ptr->a_idx == o_ptr->artifact_index)
					{
						/* Always reset certain values XXX XXX */
						o_ptr->to_a = a_ptr->to_a;
						o_ptr->to_d = a_ptr->to_d;
						o_ptr->to_h = a_ptr->to_h;

						o_ptr->ac = a_ptr->ac;
						o_ptr->dd = a_ptr->dd;
						o_ptr->ds = a_ptr->ds;

						o_ptr->pval  = a_ptr->pval1;
						o_ptr->pval2 = a_ptr->pval2;
						o_ptr->pval2 = a_ptr->pval3;

						/* Is the bonus really there? */
						if (a_ptr->set_bonus)
						{
							o_ptr->flags_pval1 &= ~(se_ptr->flags_pval1);
							o_ptr->flags_pval2 &= ~(se_ptr->flags_pval2);
							o_ptr->flags_pval3 &= ~(se_ptr->flags_pval3);

							o_ptr->flags1 &= ~(se_ptr->flags1);
							o_ptr->flags2 &= ~(se_ptr->flags2);
							o_ptr->flags3 &= ~(se_ptr->flags3);

							a_ptr->set_bonus = FALSE;
							bonus_removed = TRUE;
						}
					}
				}
			}
		}
	}

	/* Notify */
	if (bonus_removed)
	{
		return (format("The %s set is no longer complete.", s_ptr->name));
	}

	return ("");
}


/*
 * Check worn items and apply sets as set data is not stored. -GS-
 *
 * This would be unnecessary if set data were saved.  XXX XXX
 */
void check_item_sets(void)
{
	int j;

	/* Check all equipped items. */
	for (j = INVEN_WIELD; j < INVEN_SUBTOTAL; j++)
	{
		/* Get this inventory item */
		object_type *o_ptr = &inventory[j];

		/* Check artifacts */
		if (o_ptr->artifact_index)
		{
			artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

			/* Check set items */
			if (a_ptr->set_index)
			{
				/* Check for complete set. */
				if (check_set(a_ptr->set_index))
				{
					/* Apply set bonuses */
					apply_set(a_ptr->set_index);
				}
			}
		}
	}
}


/*
 * List of object categories and (short) names.
 */
typedef struct
{
	byte tval;
	char o_name[DESC_LEN];
	u16b o_idx;
} nearby_object_type;


/*
 * Sorting hook -- comp function -- array of object tvals.
 */
static bool ang_sort_comp_hook_o_tval(const void *u, const void *v, int a, int b)
{
	nearby_object_type *no = (nearby_object_type*)(u);

	/* Unused parameter */
	(void)v;

	/* Sort by decreasing tval */
	return (no[a].tval >= no[b].tval);
}

/*
 * Sorting hook -- swap function -- array of object tvals.
 */
static void ang_sort_swap_hook_o_tval(void *u, void *v, int a, int b)
{
	nearby_object_type *no = (nearby_object_type*)(u);
	nearby_object_type temp_no;

	/* Unused parameter */
	(void)v;

	/* Swap records */
	COPY(&temp_no, &no[a], nearby_object_type);
	COPY(&no[a], &no[b], nearby_object_type);
	COPY(&no[b], &temp_no, nearby_object_type);
}



/*
 * Display visible objects in line of sight in a window.  -LM-
 *
 * This display is deliberately limited to objects in line of sight,
 * mostly to preserve the coolness of carefully looking around into
 * a detected vault and discovering something really cool.  I suspect
 * that making this automatic would spoil things a bit.
 *
 * Optionally, also show visible monsters if space permits.
 */
void display_nearby_objects(int y, int x, bool also_list_monsters)
{
	int w, h, col_wid;
	int i;

	u16b dummy = 0;

	char o_name[DESC_LEN];

	nearby_object_type *nearby_o_count;
	int total_count = 0;


	/* Get size of window */
	(void)Term_get_size(&w, &h);

	/* Paranoia -- refuse to accept too small a window */
	if ((h < 2) || (w < 3)) return;

	/* Erase the window */
	if (!y && !x) (void)Term_clear();

	/* Hallucination */
	if ((p_ptr->image) && (!x) && (!y))
	{
		c_prt(TERM_L_PURPLE, "You can't believe what you are seeing!  It's like a dream!", 0, 0);
		(void)Term_fresh();
		return;
	}


	/* Calculate number of columns */
	i = ((w+20) / 40);

	/* Calculate column width */
	col_wid = w / i;


	/* Allocate the array */
	C_MAKE(nearby_o_count, o_max, nearby_object_type);


	/* Scan the object list */
	for (i = 1; i < o_max; i++)
	{
		/* Get object */
		object_type *o_ptr = &o_list[i];

		/* Skip dead or off-map objects */
		if ((!o_ptr->k_idx) || (!o_ptr->iy) || (!o_ptr->ix)) continue;

		/* Skip hidden objects */
		if (!o_ptr->marked) continue;

		/* Skip objects not in line of sight */
		if (!player_has_los_bold(o_ptr->iy, o_ptr->ix)) continue;

		/* Save object tval */
		nearby_o_count[total_count].tval = o_ptr->tval;

		/* Save object index */
 		nearby_o_count[total_count].o_idx = i;

		/* Suppress flavours */
		object_desc_flavour = -1;

		/* Obtain an object description */
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);

		/* Truncate to fit column */
		o_name[col_wid - 3] = '\0';

		/* Save object description */
		(void)my_strcpy(nearby_o_count[total_count].o_name, o_name,
			sizeof(nearby_o_count[total_count].o_name));

		/* Go to next object */
		total_count++;
	}


	/* Note no visible objects */
	if (!total_count)
	{
		c_prt(TERM_SLATE, "You see no nearby objects.", y, x);
		goto end_of_function;
	}

	/* Message */
	prt(format("You can see %d nearby object%s:",
		total_count, (total_count > 1 ? "s" : "")), y, x);


	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook_o_tval;
	ang_sort_swap = ang_sort_swap_hook_o_tval;

	/* Sort the objects by increasing tvals */
	ang_sort(nearby_o_count, &dummy, total_count);


	/* Display the objects */
	for (i = 0; i < total_count; i++)
	{
		/* Get color */
		int attr = tval_to_attr[nearby_o_count[i].tval];

		/* Get object kind */
		object_type *o_ptr = &o_list[nearby_o_count[i].o_idx];

		/* Objects are listed in columns, roughly 40 characters wide */
		if (++y >= h)
		{
			x += col_wid;
			y = 1;
		}
		if (x >= w - (col_wid / 2)) break;

		/* Display the pict */
		(void)Term_putch(x, y, object_attr(o_ptr), object_char(o_ptr));

		/* Display this object */
		c_prt(attr, format("%s", nearby_o_count[i].o_name), y, x + 2);
	}

	/* End of function */
	end_of_function:


	/* Also list monsters, if requested and possible */
	if (also_list_monsters)
	{
		/* Skip some space.  Prefer to advance to next column. */
		if ((x == 0) && (w > 3 * col_wid / 2))
		{
			x = col_wid;
			y = 0;
		}
		else if (y + 6 >= h)
		{
			x += col_wid;
			y = 0;
		}
		else y += 4;

		/* If we have (a fair amount of) space, print out nearest monsters */
		if (x < w - (col_wid / 2)) display_m_list(y, x, FALSE);
	}


	/* Free the object counters */
	FREE(nearby_o_count);
}

