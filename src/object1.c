/* File: object1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"




/*
 * Max sizes of the following arrays.
 */
#define MAX_TITLES     50       /* Used with scrolls (min 48) */
#define MAX_SYLLABLES 158       /* Used with scrolls (see below) */

/*
 * Syllables for scrolls (must be 1-4 letters each).
 */

static cptr syllables[MAX_SYLLABLES] =
{
	"a", "ab", "ag", "aks", "ala", "an", "ankh", "app",
	"arg", "arze", "ash", "aus", "ban", "bar", "bat", "bek",
	"bie", "bin", "bit", "bjor", "blu", "bot", "bu",
	"byt", "comp", "con", "cos", "cre", "dalf", "dan",
	"den", "der", "doe", "dok", "eep", "el", "eng", "er", "ere", "erk",
	"esh", "evs", "fa", "fid", "flit", "for", "fri", "fu", "gan",
	"gar", "glen", "gop", "gre", "ha", "he", "hyd", "i",
	"ing", "ion", "ip", "ish", "it", "ite", "iv", "jo",
	"kho", "kli", "klis", "la", "lech", "man", "mar",
	"me", "mi", "mic", "mik", "mon", "mung", "mur", "nag", "nej",
	"nelg", "nep", "ner", "nes", "nis", "nih", "nin", "o",
	"od", "ood", "org", "orn", "ox", "oxy", "pay", "pet",
	"ple", "plu", "po", "pot", "prok", "re", "rea", "rhov",
	"ri", "ro", "rog", "rok", "rol", "sa", "san", "sat",
	"see", "sef", "seh", "shu", "ski", "sna", "sne", "snik",
	"sno", "so", "sol", "sri", "sta", "sun", "ta", "tab",
	"tem", "ther", "ti", "tox", "trol", "tue", "turs", "u",
	"ulk", "um", "un", "uni", "ur", "val", "viv", "vly",
	"vom", "wah", "wed", "werg", "wex", "whon", "wun", "x",
	"yerg", "yp", "zun", "tri", "blaa"
};


/*
 * Hold the titles of scrolls, 6 to 14 characters each.
 *
 * Also keep an array of scroll colors (always WHITE for now).
 */

static char scroll_adj[MAX_TITLES][16];


static void flavor_assign_fixed(void)
{
	int i, j;

	for (i = 0; i < z_info->flavor_max; i++)
	{
		flavor_type *flavor_ptr = &flavor_info[i];

		/* Skip random flavors */
		if (flavor_ptr->sval == SV_UNKNOWN) continue;

		for (j = 0; j < z_info->k_max; j++)
		{
			/* Skip other objects */
			if ((k_info[j].tval == flavor_ptr->tval) &&
			    (k_info[j].sval == flavor_ptr->sval))
			{
				/* Store the flavor index */
				k_info[j].flavor = i;
			}
		}
	}
}


static void flavor_assign_random(byte tval)
{
	int i, j;
	int flavor_count = 0;
	int choice;

	/* Count the random flavors for the given tval */
	for (i = 0; i < z_info->flavor_max; i++)
	{
		if ((flavor_info[i].tval == tval) &&
		    (flavor_info[i].sval == SV_UNKNOWN))
		{
			flavor_count++;
		}
	}

	for (i = 0; i < z_info->k_max; i++)
	{
		/* Skip other object types */
		if (k_info[i].tval != tval) continue;

		/* Skip objects that already are flavored */
		if (k_info[i].flavor != 0) continue;

		/* HACK - Ordinary food is "boring" */
		if ((tval == TV_FOOD) && (k_info[i].sval >= SV_FOOD_MIN_FOOD))
			continue;

		if (!flavor_count) quit_fmt("Not enough flavors for tval %d.", tval);

		/* Select a flavor */
		choice = rand_int(flavor_count);

		/* Find and store the flavor */
		for (j = 0; j < z_info->flavor_max; j++)
		{
			/* Skip other tvals */
			if (flavor_info[j].tval != tval) continue;

			/* Skip assigned svals */
			if (flavor_info[j].sval != SV_UNKNOWN) continue;

			if (choice == 0)
			{
				/* Store the flavor index */
				k_info[i].flavor = j;

				/* Mark the flavor as used */
				flavor_info[j].sval = k_info[i].sval;

				/* One less flavor to choose from */
				flavor_count--;

				break;
			}

			choice--;
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
 * Rings, Amulets, Talismans, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused Potion).
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 1 to 8 letters long (one or two syllables of 1 to 4
 * letters each), and that no scroll is finished until it attempts to
 * grow beyond 15 letters.  The first time this can happen is when the
 * current title has 6 letters and the new word has 8 letters, which
 * would result in a 6 letter scroll title.
 *
 * Duplicate titles are avoided by requiring that no two scrolls share
 * the same first four letters (not the most efficient method, and not
 * the least efficient method, but it will always work).
 *
 * Hack -- make sure everything stays the same for each saved game
 * This is accomplished by the use of a saved "random seed", as in
 * "town_gen()".  Since no other functions are called while the special
 * seed is in effect, so this function is pretty "safe".
 */
void flavor_init(void)
{
	int i, j;


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant flavors */
	Rand_value = seed_flavor;


	flavor_assign_fixed();

	flavor_assign_random(TV_RING);
	flavor_assign_random(TV_AMULET);
	flavor_assign_random(TV_TALISMAN);
	flavor_assign_random(TV_WAND);
	flavor_assign_random(TV_FOOD);
	flavor_assign_random(TV_POTION);
	flavor_assign_random(TV_SCROLL);

	/* Scrolls (random titles, always white) */
	for (i = 0; i < MAX_TITLES; i++)
	{
		/* Get a new title */
		while (TRUE)
		{
			char buf[80];

			bool okay;

			/* Start a new title */
			buf[0] = '\0';

			/* Collect words until done */
			while (1)
			{
				int q, s;

				char tmp[80];

				/* Start a new word */
				tmp[0] = '\0';

				/* Choose one or two syllables */
				s = ((rand_int(100) < 30) ? 1 : 2);

				/* Add a one or two syllable word */
				for (q = 0; q < s; q++)
				{
					/* Add the syllable */
					my_strcat(tmp, syllables[rand_int(MAX_SYLLABLES)], sizeof(tmp));
				}

				/* Stop before getting too long */
				if (strlen(buf) + 1 + strlen(tmp) > 15) break;

				/* Add a space */
				strcat(buf, " ");

				/* Add the word */
				my_strcat(buf, tmp, sizeof(buf));
			}

			/* Save the title */
			my_strcpy(scroll_adj[i], buf+1, sizeof(scroll_adj[0]));

			/* Assume okay */
			okay = TRUE;

			/* Check for "duplicate" scroll titles */
			for (j = 0; j < i; j++)
			{
				cptr hack1 = scroll_adj[j];
				cptr hack2 = scroll_adj[i];

				/* Compare first four characters */
				if (*hack1++ != *hack2++) continue;
				if (*hack1++ != *hack2++) continue;
				if (*hack1++ != *hack2++) continue;
				if (*hack1++ != *hack2++) continue;

				/* Not okay */
				okay = FALSE;

				/* Stop looking */
				break;
			}

			/* Break when done */
			if (okay) break;
		}
	}


	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Analyze every object */
	for (i = 1; i < z_info->k_max; i++)
	{

		object_kind *k_ptr = &k_info[i];

		/*Skip "empty" objects*/
		if (!k_ptr->name) continue;

		/*No flavor yields aware*/
		if (!k_ptr->flavor) k_ptr->aware = TRUE;
	}
}



#ifdef ALLOW_BORG_GRAPHICS
extern void init_translate_visuals(void);
#endif /* ALLOW_BORG_GRAPHICS */


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful.  XXX XXX XXX
 */
void reset_visuals(bool unused)
{
	int i;


	/* Unused parameter */
	(void)unused;

	/* Extract default attr/char code for features */
	for (i = 0; i < z_info->f_max; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;
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

	/* Extract attr/chars for inventory objects (by tval) */
	for (i = 0; i < 128; i++)
	{
		/* Default to white */
		tval_to_attr[i] = TERM_WHITE;
	}


	/* Graphic symbols */
	if (use_graphics)
	{
		/* Process "graf.prf" */
		process_pref_file("graf.prf");
	}

	/* Normal symbols */
	else
	{
		/* Process "font.prf" */
		process_pref_file("font.prf");
	}

#ifdef ALLOW_BORG_GRAPHICS
	/* Initialize the translation table for the borg */
	init_translate_visuals();
#endif /* ALLOW_BORG_GRAPHICS */
}

/*
 * Returns TRUE if an object has some ego-powers that should be ignored if
 * the game does not want *full* knowledge of it.
*/
bool object_has_hidden_powers(const object_type *o_ptr)
{
	/* *identified* items are never hidden*/
	if (o_ptr->ident & (IDENT_MENTAL)) return (FALSE);

  	/* Hack - Ignore chests */
  	if (o_ptr->tval == TV_CHEST) return (FALSE);

  	/* Analyze xtra1 */
  	switch (o_ptr->xtra1)
  	{
		case OBJECT_XTRA_STAT_SUSTAIN:
		case OBJECT_XTRA_TYPE_HIGH_RESIST:
		case OBJECT_XTRA_TYPE_POWER:
		case OBJECT_XTRA_TYPE_IMMUNITY:
		{
			return (TRUE);
		}
	}

  	return (FALSE);
}

/*Helper function for add extra flags. Adds the flags from xtra2*/
static u32b add_xtra2_flags(u32b xtra_flags, byte xtra_size, u32b xtra_base)
{
	byte i;

	u32b flag_check = 0x00000001L;

	u32b return_flag = 0;

	for (i = 0; i < xtra_size; i++)
	{
		/*Do we have this flag?*/
		if (xtra_flags & flag_check)
		{
			/*mark it*/
			return_flag |= xtra_base;
		}

		/*shift everything for the next check*/
		flag_check  = flag_check << 1;
		xtra_base  = xtra_base << 1;

	}

	return (return_flag);
}


/*
 * Modes of object_flags_aux()
 */
#define OBJECT_FLAGS_FULL   1 /* Full info */
#define OBJECT_FLAGS_KNOWN  2 /* Only flags known to the player */


/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native)
{
	object_kind *k_ptr;

	if (mode == OBJECT_FLAGS_KNOWN)
	{
		/* Clear */
		(*f1) = (*f2) = (*f3) = (*native) = 0L;

		/* Must be identified */
		if (!object_known_p(o_ptr)) return;
	}

	k_ptr = &k_info[o_ptr->k_idx];

	/* Base object */
	(*f1) = k_ptr->k_flags1;
	(*f2) = k_ptr->k_flags2;
	(*f3) = k_ptr->k_flags3;
	(*native) = k_ptr->k_native;

	if (mode == OBJECT_FLAGS_FULL)
	{
		/* Artifact */
		if (o_ptr->art_num)
		{
			artifact_type *a_ptr = &a_info[o_ptr->art_num];

			(*f1) = a_ptr->a_flags1;
			(*f2) = a_ptr->a_flags2;
			(*f3) = a_ptr->a_flags3;
			(*native) = a_ptr->a_native;
		}
	}

	/* Ego-item */
	if (o_ptr->ego_num)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->ego_num];

		(*f1) |= e_ptr->flags1;
		(*f2) |= e_ptr->flags2;
		(*f3) |= e_ptr->flags3;
		(*native) |= e_ptr->e_native;
	}

	if (mode == OBJECT_FLAGS_KNOWN)
	{
		/* Obvious artifact flags */
		if (o_ptr->art_num)
		{
			artifact_type *a_ptr = &a_info[o_ptr->art_num];

			/* Obvious flags (pval) */
			(*f1) = (a_ptr->a_flags1 & (TR1_PVAL_MASK));
			(*f3) = (a_ptr->a_flags3 & (TR3_IGNORE_MASK));
		}
	}

	if (mode == OBJECT_FLAGS_KNOWN)
	{
		bool spoil = FALSE;

#ifdef SPOIL_ARTIFACTS
		/* Full knowledge for some artifacts */
		if (artifact_p(o_ptr)) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

#ifdef SPOIL_EGO_ITEMS
		/* Full knowledge for some ego-items */
		if (ego_item_p(o_ptr)) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

		/* Artifact, *ID'ed or spoiled */
		if ((o_ptr->art_num) && (spoil || (o_ptr->ident & IDENT_MENTAL)))
		{
			artifact_type *a_ptr = &a_info[o_ptr->art_num];

			(*f1) = a_ptr->a_flags1;
			(*f2) = a_ptr->a_flags2;
			(*f3) = a_ptr->a_flags3;
			(*native) = a_ptr->a_native;

		}

		/* Full knowledge for *identified* objects */
		if ((!(o_ptr->ident & IDENT_MENTAL)) &&
		    (object_has_hidden_powers(o_ptr)))	return;

	}

	/*hack - chests use xtra1 to store the theme, don't give additional powers to chests*/
	if (o_ptr->tval == TV_CHEST) return;

	/* Extra powers */
	switch (o_ptr->xtra1)
	{

		case OBJECT_XTRA_STAT_SUSTAIN:
		{
			/* Flag 2 */
			(*f2) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_SUSTAIN,
										OBJECT_XTRA_BASE_SUSTAIN);
			break;
		}

		case OBJECT_XTRA_TYPE_HIGH_RESIST:
		{
			/* Flag 2 */
			(*f2) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_HIGH_RESIST,
										OBJECT_XTRA_BASE_HIGH_RESIST);
			break;
		}

		case OBJECT_XTRA_TYPE_POWER:
		{
			/* Flag 3 */
			(*f3) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_POWER,
										OBJECT_XTRA_BASE_POWER);
			break;
		}
		case OBJECT_XTRA_TYPE_IMMUNITY:
		{
			/* Flag 2 */
			(*f2) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_IMMUNITY,
										OBJECT_XTRA_BASE_IMMUNITY);
			break;
		}
		case OBJECT_XTRA_TYPE_STAT_ADD:
		{
			/* Flag 1 */
			(*f1) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_STAT_ADD,
										OBJECT_XTRA_BASE_STAT_ADD);
			/*Stat add Also sustains*/
			(*f2) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_SUSTAIN,
										OBJECT_XTRA_BASE_SUSTAIN);
			break;
		}
		case OBJECT_XTRA_TYPE_SLAY:
		{
			/* Flag 1 */
			(*f1) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_SLAY,
										OBJECT_XTRA_BASE_SLAY);
			break;
		}
		case OBJECT_XTRA_TYPE_KILL:
		{
			/* Flag 1 */
			(*f1) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_KILL,
										OBJECT_XTRA_BASE_KILL);
			break;
		}
		case OBJECT_XTRA_TYPE_BRAND:
		{
			/* Flag 1 */
			(*f1) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_BRAND,
										OBJECT_XTRA_BASE_BRAND);
			/*
			 * elemental brands also provide the appropriate resist
			 * Note that the OBJECT_XTRA_SIZE_LOW_RESIST is not used.  There
			 * are only 4 base resists, but 5 base brands (+poison).  Hence the
			 * OBJECT_XTRA_SIZE_BRAND used here is deliberate and not a bug.
			 */
			(*f2) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_BRAND,
										OBJECT_XTRA_BASE_LOW_RESIST);

			break;
		}
		case OBJECT_XTRA_TYPE_LOW_RESIST:
		{
			/* Flag 2 */
			(*f2) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_LOW_RESIST,
										OBJECT_XTRA_BASE_LOW_RESIST);
			break;
		}
		case OBJECT_XTRA_TYPE_NATIVE:
		{
			/* Flag native */
			(*native) |= add_xtra2_flags(o_ptr->xtra2, OBJECT_XTRA_SIZE_NATIVE,
										OBJECT_XTRA_BASE_NATIVE);
			break;
		}
	}

	/*Now add the ignores for any xtra above*/
	if ((*f2) & (TR2_RES_ACID))	(*f3) |= TR3_IGNORE_ACID;
	if ((*f2) & (TR2_RES_ELEC))	(*f3) |= TR3_IGNORE_ELEC;
	if ((*f2) & (TR2_RES_FIRE))	(*f3) |= TR3_IGNORE_FIRE;
	if ((*f2) & (TR2_RES_COLD))	(*f3) |= TR3_IGNORE_COLD;
	if ((*native) & (TN1_NATIVE_LAVA | TN1_NATIVE_FIRE)) (*f3) |= TR3_IGNORE_FIRE;
	if ((*native) & (TN1_NATIVE_ICE)) (*f3) |= TR3_IGNORE_COLD;
	if ((*native) & (TN1_NATIVE_ACID)) (*f3) |= TR3_IGNORE_ACID;
}




/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native)
{
	object_flags_aux(OBJECT_FLAGS_FULL, o_ptr, f1, f2, f3, native);
}



/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *native)
{
	object_flags_aux(OBJECT_FLAGS_KNOWN, o_ptr, f1, f2, f3, native);
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
	cptr s = (S); \
 \
	/* Copy the string */ \
	while (*s) *(T)++ = *s++; \
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
	int i = (I); \
 \
	/* Negative */ \
	if (i < 0) \
	{ \
		/* Take the absolute value */ \
		i = 0 - i; \
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
	object_desc_num_macro(T, i); \
 \
} while (0)

/*
 * Strip an "object name" into a buffer.
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
		if (*str != '~') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}

/*
 * Creates a description of the item "o_ptr", and stores it in "buf".
 *
 * One can choose the "verbosity" of the description, including whether
 * or not the "number" of items should be described, and how much detail
 * should be used when describing the item.
 *
 * The given "buf" should be at least 80 chars long to hold the longest
 * possible description, which can get pretty long, including inscriptions,
 * such as:
 * "no more Maces of Disruption (Defender) (+10,+10) [+5] (+3 to stealth)".

 * Note that the object description will be clipped to fit into the given
 * buffer size.
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
 * have special processing for "Specials" (artifact Lites, Rings, Amulets).
 * The "Specials" never use "modifiers" if they are "known", since they
 * have special "descriptions", such as "The Necklace of the Dwarves".
 *
 * Special Lite's use the "k_info" base-name (Phial, Star, or Arkenstone),
 * plus the artifact name, just like any other artifact, if known.
 *
 * Special Ring's and Amulet's, if not "aware", use the same code as normal
 * rings and amulets, and if "aware", use the "k_info" base-name (Ring or
 * Amulet or Necklace).  They will NEVER "append" the "k_info" name.  But,
 * they will append the artifact name, just like any artifact, if known.
 *
 * None of the Special Rings/Amulets are "EASY_KNOW", though they could be,
 * at least, those which have no "pluses", such as the three artifact lites.
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
	cptr basenm;
	cptr modstr;

	int power;

	bool aware;
	bool known;

	bool flavor;

	bool append_name;

	bool show_weapon;
	bool show_armour;

	char *b;

	char *t;

	cptr s;

	cptr u;
	cptr v;

	char p1 = '(', p2 = ')';
	char b1 = '[', b2 = ']';
	char c1 = '{', c2 = '}';

	char discount_buf[80];

	char tmp_buf[128];

	u32b f1, f2, f3, fn;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3, &fn);

	/* See if the object is "aware" */
	aware = (object_aware_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "known" */
	known = (object_known_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "flavored" */
	flavor = (k_ptr->flavor ? TRUE : FALSE);

	/* Allow flavors to be hidden when aware */
	if (aware && !show_flavors) flavor = FALSE;

	/* Object is in the inventory of a store */
	if (o_ptr->ident & IDENT_STORE)
	{
		/* Don't show flavors */
		flavor = FALSE;

		/* Pretend known and aware */
		aware = TRUE;
		known = TRUE;
	}

	/* Player has now seen the item
	 *
	 * This code must be exactly here to properly handle objects in
	 * stores (fake assignment to "aware", see above) and unaware objects
	 * in the dungeon.
	 */
	if (aware) k_ptr->everseen = TRUE;

	/* Assume no name appending */
	append_name = FALSE;

	/* Assume no need to show "weapon" bonuses */
	show_weapon = FALSE;

	/* Assume no need to show "armour" bonuses */
	show_armour = FALSE;

	/* Extract default "base" string */
	basenm = (k_name + k_ptr->name);

	/* Assume no "modifier" string */
	modstr = "";

	/* Analyze the object */
	switch (o_ptr->tval)
	{
		/* Some objects are easy to describe */
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
		{
			break;
		}

		/* Missiles/Bows/Weapons */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			show_weapon = TRUE;
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
		case TV_DRAG_SHIELD:
		{
			show_armour = TRUE;
			break;
		}

		/* Lites (including a few "Specials") */
		case TV_LITE:
		{
			break;
		}

		/* Amulets (including a few "Specials") */
		case TV_AMULET:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware) break;

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Amulet~" : "& Amulet~");

			break;
		}

		/* Rings (including a few "Specials") */
		case TV_RING:
		{
			/* Hack -- Known artifacts */
			if (artifact_p(o_ptr) && aware) break;

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Ring~" : "& Ring~");

			break;
		}

		/* Talismans */
		case TV_TALISMAN:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Talisman~" : "& Talisman~");

			break;
		}

		/* Wands */
		case TV_WAND:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Wand~" : "& Wand~");

			break;
		}

		/* Rods */
		case TV_ROD:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Rod~" : "& Rod~");

			break;
		}

		/* Scrolls */
		case TV_SCROLL:
		{
			/* Color the object */
			modstr = scroll_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& Scroll~ titled \"#\"" : "& Scroll~");

			break;
		}

		/* Potions */
		case TV_POTION:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Potion~" : "& Potion~");

			break;
		}

		/* Food */
		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (o_ptr->sval >= SV_FOOD_MIN_FOOD) break;

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Mushroom~" : "& Mushroom~");

			break;
		}

		/* Magic Books */
		case TV_MAGIC_BOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of Mage Spells #";
			break;
		}

		/* Druid Books */
		case TV_DRUID_BOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of Druid Spells #";
			break;
		}

		/* Prayer Books */
		case TV_PRAYER_BOOK:
		{
			modstr = basenm;
			basenm = "& Holy Book~ of Prayers #";
			break;
		}

		/* Hack -- Gold/Gems */
		case TV_GOLD:
		{
			my_strcpy(buf, basenm, max);
			return;
		}

		/* Hack -- Default -- Used in the "inventory" routine */
		default:
		{
			my_strcpy(buf, "(nothing)", max);
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
		/* Paranoia XXX XXX XXX */
		/* ASSERT(s[1] == ' '); */

		/* Skip the ampersand and the following space */
		s += 2;

		/* No prefix */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- None left */
		else if (o_ptr->number <= 0)
		{
			object_desc_str_macro(t, "no more ");
		}

		/* Extract the number */
		else if (o_ptr->number > 1)
		{
			object_desc_num_macro(t, o_ptr->number);
			object_desc_chr_macro(t, ' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && artifact_p(o_ptr))
		{
			object_desc_str_macro(t, "The ");
		}


		/* Hack -- A single one, and next character will be a vowel */
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
		else if (o_ptr->number <= 0)
		{
			object_desc_str_macro(t, "no more ");
		}

		/* Prefix a number if required */
		else if (o_ptr->number > 1)
		{
			object_desc_num_macro(t, o_ptr->number);
			object_desc_chr_macro(t, ' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && artifact_p(o_ptr))
		{
			object_desc_str_macro(t, "The ");
		}

		/* Hack -- A single item, so no prefix needed */
		else
		{
			/* Nothing */
		}
	}

	/* Perfectly balanced throwing weapons are indicated. */
	if ((known) && (o_ptr->ident & IDENT_PERFECT_BALANCE))
	{
		object_desc_str_macro(t, "Well-balanced ");
	}

	/* Paranoia XXX XXX XXX */
	/* ASSERT(*s != '~'); */

	/* Copy the string */
	for (; *s; s++)
	{
		/* Pluralizer */
		if (*s == '~')
		{
			/* Add a plural if needed */
			if ((o_ptr->number != 1) && !(known && artifact_p(o_ptr)))
			{
				char k = t[-1];

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h')) *t++ = 'e';

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


	/* Append the "kind name" to the "base name" */
	if (append_name)
	{
		object_desc_str_macro(t, " of ");
		object_desc_str_macro(t, (k_name + k_ptr->name));
	}


	/* Hack -- Append "Artifact" or "Special" names */
	if (known)
	{
		/* Grab any artifact name */
		if (o_ptr->art_num)
		{
			artifact_type *a_ptr = &a_info[o_ptr->art_num];

			object_desc_chr_macro(t, ' ');
			object_desc_str_macro(t, a_ptr->name);
		}

		/* Grab any ego-item name */
		else if (o_ptr->ego_num)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->ego_num];

			object_desc_chr_macro(t, ' ');
			object_desc_str_macro(t, (e_name + e_ptr->name));

			/* Hack - Now we know about the ego-item type */
			e_info[o_ptr->ego_num].everseen = TRUE;
		}
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

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
		{
			if (chest_traps[0 - o_ptr->pval])
			{
				tail = " (disarmed)";
			}
			else
			{
				tail = " (unlocked)";
			}
		}

		/* Describe the traps, if any */
		else
		{
			/* Describe the traps */
			switch (chest_traps[o_ptr->pval])
			{
				case 0:
				{
					tail = " (Locked)";
					break;
				}
				case CHEST_LOSE_STR:
				{
					tail = " (Poison Needle)";
					break;
				}
				case CHEST_LOSE_CON:
				{
					tail = " (Poison Needle)";
					break;
				}
				case CHEST_POISON:
				{
					tail = " (Gas Trap)";
					break;
				}
				case CHEST_PARALYZE:
				{
					tail = " (Gas Trap)";
					break;
				}
				case CHEST_EXPLODE:
				{
					tail = " (Explosion Device)";
					break;
				}
				case CHEST_SUMMON:
				{
					tail = " (Summoning Runes)";
					break;
				}
				default:
				{
					tail = " (Multiple Traps)";
					break;
				}
			}
		}

		/* Append the tail */
		object_desc_str_macro(t, tail);
	}


	/* Display the item like a weapon */
	if (f3 & (TR3_SHOW_MODS)) show_weapon = TRUE;

	/* Display the item like a weapon */
	if (o_ptr->to_h && o_ptr->to_d) show_weapon = TRUE;

	/* Display the item like armour */
	if (o_ptr->ac) show_armour = TRUE;


	/* Dump base weapon info */
	switch (o_ptr->tval)
	{
		/* Missiles */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{

			power = power;
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
		case TV_BOW:
		{
			/* Hack -- Extract the "base power" */
			power = (o_ptr->sval % 10);

			/* Append a "power" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_chr_macro(t, 'x');
			object_desc_num_macro(t, power);
			object_desc_chr_macro(t, p2);

			/* All done */
			break;
		}
	}


	/* Add the weapon bonuses */

	if (known || ((o_ptr->ident & (IDENT_SENSE)) && (o_ptr->ident & (IDENT_WIELDED))))
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
	if (known || ((o_ptr->ident & (IDENT_SENSE)) && (o_ptr->ident & (IDENT_WIELDED))))
	{
		/* Show the armor class info */
		if (show_armour)
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, b1);
			object_desc_num_macro(t, o_ptr->ac);
			object_desc_chr_macro(t, ',');
			object_desc_int_macro(t, o_ptr->to_a);
			object_desc_chr_macro(t, b2);
		}

		/* No base armor, but does increase armor */
		else if (o_ptr->to_a)
		{
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, b1);
			object_desc_int_macro(t, o_ptr->to_a);
			object_desc_chr_macro(t, b2);
		}
	}

	/* Hack -- always show base armor */
	else if (show_armour)
	{
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, b1);
		object_desc_num_macro(t, o_ptr->ac);
		object_desc_chr_macro(t, b2);
	}


	/* No more details wanted */
	if (mode < 2) goto object_desc_done;

	/* Hack -- Process Lanterns/Torches */
	if (fuelable_lite_p(o_ptr))
	{
		/* Hack -- Turns of light for normal lites */
		object_desc_str_macro(t, " (with ");
		object_desc_num_macro(t, o_ptr->timeout);
		object_desc_str_macro(t, " turns of light)");
	}


	/* Dump "pval" flags for wearable items */
	if (known && (f1 & (TR1_PVAL_MASK)))
	{
		cptr tail = "";
		cptr tail2 = "";

		/* Start the display */
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, p1);

		/* Dump the "pval" itself */
		object_desc_int_macro(t, o_ptr->pval);

		/* Do not display the "pval" flags */
		if (f3 & (TR3_HIDE_TYPE))
		{
			/* Nothing */
		}

		/* Searching */
		else if (f1 & (TR1_SEARCH))
		{
			/* Dump " to searching" */
			tail = " to searching";
		}

		/* Infravision */
		else if (f1 & (TR1_INFRA))
		{
			/* Dump " to infravision" */
			tail = " to infravision";
		}

#if 0

		/* Tunneling */
		else if (f1 & (TR1_TUNNEL))
		{
			/* Dump " to digging" */
			tail = " to digging";
		}

#endif

		/* Speed */
		else if (f1 & (TR1_SPEED))
		{
			/* Dump " to speed" */
			tail = " to speed";
		}

		/* Blows */
		else if (f1 & (TR1_BLOWS))
		{
			/* Add " attack" */
			tail = " attack";

			/* Add "attacks" */
			if (ABS(o_ptr->pval) != 1) tail2 = "s";
		}

#if 0

		/* Shots */
		else if (f1 & (TR1_SHOTS))
		{
			/* Nothing */
		}

		/* Might */
		else if (f1 & (TR1_MIGHT))
		{
			/* Nothing */
		}

#endif

		/* Add the descriptor */
		object_desc_str_macro(t, tail);
		object_desc_str_macro(t, tail2);

		/* Finish the display */
		object_desc_chr_macro(t, p2);
	}


	/* Indicate "charging" objects, but not rods or lites */
	if (known && o_ptr->timeout && o_ptr->tval != TV_ROD
	    && !fuelable_lite_p(o_ptr))
	{

		/* Hack -- Dump " (charging)" if relevant */
		object_desc_str_macro(t, " (charging)");
	}


	/* No more details wanted */
	if (mode < 3) goto object_desc_done;

	/* Use standard inscription */
	if (o_ptr->obj_note)
	{
		u = quark_str(o_ptr->obj_note);
	}

	/* Use nothing */
	else
	{
		u = NULL;
	}


	/* Use special inscription, if any */
	if (o_ptr->discount >= INSCRIP_NULL)
	{
		v = inscrip_text[o_ptr->discount - INSCRIP_NULL];
	}

	/* Use "cursed" if the item is known to be cursed */
	else if (cursed_p(o_ptr) && known)
	{
		v = "cursed";
	}

	/* Hack -- Use "empty" for empty wands/staffs */
	else if (!known && (o_ptr->ident & (IDENT_EMPTY)))
	{
		v = "empty";
	}

	/* Use "tried" if the object has been tested unsuccessfully */
	else if (!aware && object_tried_p(o_ptr))
	{
		v = "tried";
	}

	/* Use the discount, if any */
	else if (o_ptr->discount > 0)
	{
		char *q = discount_buf;
		object_desc_num_macro(q, o_ptr->discount);
		object_desc_str_macro(q, "% off");
		*q = '\0';
		v = discount_buf;
	}

	/* Use the "unknown" inscription */
	else if (!known && can_be_pseudo_ided(o_ptr))
	{
		v = "unknown";
		c1 = '<';
		c2 = '>';
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
			while ((t < b + 75) && *u) *t++ = *u++;
		}

		/* Special inscription too */
		if (u && v && (t < b + 75))
		{
			/* Separator */
			*t++ = ',';
			*t++ = ' ';
		}

		/* Special inscription */
		if (v)
		{
			/* Append the inscription */
			while ((t < b + 75) && *v) *t++ = *v++;
		}

		/* Terminate the inscription */
		*t++ = c2;
	}


	object_desc_done:

	/* Terminate */
	*t = '\0';

	/* Copy the string over */
	my_strcpy(buf, tmp_buf, max);
}


/*
 * An ugly hack - for mimics - take a k_idx and return the object name/flavor
 */
void mimic_desc_object(char *buf, size_t max, s16b mimic_k_idx)
{

	cptr basenm;
	cptr modstr;

	bool aware;

	bool flavor;

	bool append_name;

	char *t;

	cptr s;

	char tmp_buf[128];

	object_kind *k_ptr = &k_info[mimic_k_idx];

	/* Extract some flags */

	/* See if the object is "aware" */
	aware = (k_ptr->aware ? TRUE : FALSE);

	/* See if the object is "flavored" */
	flavor = (k_ptr->flavor ? TRUE : FALSE);

	/* Allow flavors to be hidden when aware */
	if (aware && !show_flavors) flavor = FALSE;

	/* Assume no name appending */
	append_name = FALSE;

	/* Extract default "base" string */
	basenm = (k_name + k_ptr->name);

	/* Assume no "modifier" string */
	modstr = "";


	/* Analyze the object */
	switch (k_ptr->tval)
	{
		/* No flavor for these...*/
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_DRAG_SHIELD:
		case TV_LITE:
		{
			break;
		}

		/* Amulets (including a few "Specials") */
		case TV_AMULET:
		{

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Amulet~" : "& Amulet~");

			break;
		}

		/* Rings (including a few "Specials") */
		case TV_RING:
		{

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Ring~" : "& Ring~");

			break;
		}

		/* Talismans */
		case TV_TALISMAN:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Talisman~" : "& Talisman~");

			break;
		}

		/* Wands */
		case TV_WAND:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Wand~" : "& Wand~");

			break;
		}

		/* Rods */
		case TV_ROD:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Rod~" : "& Rod~");

			break;
		}

		/* Scrolls */
		case TV_SCROLL:
		{
			/* Color the object */
			modstr = scroll_adj[k_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& Scroll~ titled \"#\"" : "& Scroll~");

			break;
		}

		/* Potions */
		case TV_POTION:
		{
			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Potion~" : "& Potion~");

			break;
		}

		/* Food */
		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (k_ptr->sval >= SV_FOOD_MIN_FOOD) break;

			/* Color the object */
			modstr = flavor_text + flavor_info[k_ptr->flavor].text;
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Mushroom~" : "& Mushroom~");

			break;
		}

		/* Magic Books */
		case TV_MAGIC_BOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of Mage Spells #";
			break;
		}

		/* Druid Books */
		case TV_DRUID_BOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of Druid Spells #";
			break;
		}

		/* Prayer Books */
		case TV_PRAYER_BOOK:
		{
			modstr = basenm;
			basenm = "& Holy Book~ of Prayers #";
			break;
		}

		/* Hack -- Gold/Gems */
		case TV_GOLD:
		{
			my_strcpy(buf, basenm, max);
			return;
		}

		/* Hack -- Default -- Used in the "inventory" routine */
		default:
		{
			my_strcpy(buf, "(nothing)", max);
			return;
		}
	}


	/* Start dumping the result */
	t = tmp_buf;

	/* Begin */
	s = basenm;

	/* Handle objects which sometimes use "a" or "an" */
	if (*s == '&')
	{

		/* Skip the ampersand and the following space */
		s += 2;

		/* Hack -- A single one, and next character will be a vowel */
		if ((*s == '#') ? my_is_vowel(modstr[0]) : my_is_vowel(*s))
		{
			object_desc_str_macro(t, "an ");
		}

		/* A single one, and next character will be a non-vowel */
		else
		{
			object_desc_str_macro(t, "a ");
		}
	}

	/* Copy the string */
	for (; *s; s++)
	{
		/* Pluralizer */
		if (*s == '~')
		{
			 /*nothing*/
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


	/* Append the "kind name" to the "base name" */
	if (append_name)
	{
		object_desc_str_macro(t, " of ");
		object_desc_str_macro(t, (k_name + k_ptr->name));
	}


	/* Terminate */
	*t = '\0';

	/* Copy the string over */
	my_strcpy(buf, tmp_buf, max);

}

/*
 * Describe an item and pretend the item is fully known and has no flavor.
 */
void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, int pref, int mode)
{
	object_type object_type_body;
	object_type *i_ptr = &object_type_body;

	/* Make a backup */
	object_copy(i_ptr, o_ptr);

	/* HACK - Pretend the object is in a store inventory */
	i_ptr->ident |= IDENT_STORE;

	/* Describe */
	object_desc(buf, max, i_ptr, pref, mode);
}


/*
 * Describe an item's random attributes for "character dumps"
 */
void identify_random_gen(const object_type *o_ptr)
{
	/* Set hooks for character dump */
	object_info_out_flags = object_flags_known;

	/* Set the indent/wrap */
	text_out_indent = 3;
	text_out_wrap = 65;

	/* Dump the info */
	if (object_info_out(o_ptr))
		text_out("\n");

	/* Dump object history if necessary */
	if (history_interesting(o_ptr))
	{
		char buf[200];

		/* Get the history and dump it */
		if (format_object_history(buf, sizeof(buf), o_ptr))
		{
			text_out(buf);

		       	text_out("\n");
		}
	}

	/* Reset indent/wrap */
	text_out_indent = 0;
	text_out_wrap = 0;
}


/*
 * Convert an inventory index into a one character label.
 *
 * Note that the label does NOT distinguish inven/equip.
 */
char index_to_label(int i)
{
	/* Indexes for "inven" are easy */
	if (i < INVEN_WIELD) return (I2A(i));

	/* Indexes for "equip" are offset */
	return (I2A(i - INVEN_WIELD));
}


/*
 * Convert a label into the index of an item in the "inven".
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
 * Convert a label into the index of a item in the "equip".
 *
 * Return "-1" if the label does not indicate a real item.
 */
s16b label_to_equip(int c)
{
	int i;

	/* Convert */
	i = (islower((unsigned char)c) ? A2I(c) : -1) + INVEN_WIELD;

	/* Verify the index */
	if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return (-1);

	/* Empty slots can never be chosen */
	if (!inventory[i].k_idx) return (-1);

	/* Return the index */
	return (i);
}



/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(const object_type *o_ptr)
{
	/*Hack - don't allow quest items to be worn*/
	if(o_ptr->ident & (IDENT_QUEST)) return (-1);

	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (INVEN_WIELD);
		}

		case TV_BOW:
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
		case TV_DRAG_SHIELD:
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

		/* Ammo asks for first quiver slot */
		case TV_BOLT:
		case TV_ARROW:
		case TV_SHOT:
		{
			return (INVEN_QUIVER);
		}
	}

	/* No slot available */
	return (-1);
}


/*
 * Get the string that represents the pseudo-tag of the given quiver slot.
 * The color of the pseudo-tag is also obtained.
 * Returns the length of the pseudo-tag (0 on error).
 */
static int get_pseudo_tag(int slot, char tag[], int max_len, byte *color)
{
	byte tag_num = 0;
	object_type *o_ptr, *i_ptr;
	bool locked;
	int i;
	byte o_group;

	/* Paranoia */
	if (!IS_QUIVER_SLOT(slot)) return 0;

	/* Get the object */
	o_ptr = &inventory[slot];

	/* Paranoia */
	if (!o_ptr->k_idx) return 0;

	/* Get the group of the object */
	o_group = quiver_get_group(o_ptr);

	/* Check if the ammo is locked */
	locked = get_tag_num(slot, quiver_group[o_group].cmd, &tag_num);

	/* We calculate the pseudo-tag if there is not a real one */
	if (!locked)
	{
		/* Search the slots of the given ammo type */
		for (i = INVEN_QUIVER; i < slot; i++)
		{
			byte i_group;

			/* Get the object */
			i_ptr = &inventory[i];

			/* Paranoia */
			if (!i_ptr->k_idx) continue;

			/* Get the group of the object */
			i_group = quiver_get_group(i_ptr);

			/* The groups must be equal */
			if (i_group != o_group) continue;

			/*
			 * A real tag overrides the current pseudo-tag when
			 * we have many locked ammo with the same tag
			 */
			(void)get_tag_num(i, quiver_group[i_group].cmd, &tag_num);

			/* But we always increment the pseudo-tag */
			++tag_num;
		}
	}

	/* Format the pseudo-tag */
	strnfmt(tag, max_len, "%s%c%d", (locked ? "@": ""), quiver_group[o_group].cmd, tag_num);

	/* Get the color of the group */
	*color = quiver_group[o_group].color;

	return strlen(tag);
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
		case INVEN_WIELD: p = "Wielding"; break;
		case INVEN_BOW:   p = "Shooting"; break;
		case INVEN_LEFT:  p = "On left hand"; break;
		case INVEN_RIGHT: p = "On right hand"; break;
		case INVEN_NECK:  p = "Around neck"; break;
		case INVEN_LITE:  p = "Light source"; break;
		case INVEN_BODY:  p = "On body"; break;
		case INVEN_OUTER: p = "About body"; break;
		case INVEN_ARM:   p = "On arm"; break;
		case INVEN_HEAD:  p = "On head"; break;
		case INVEN_HANDS: p = "On hands"; break;
		case INVEN_FEET:  p = "On feet"; break;
		default:          p = "In pack"; break;
	}

	/* Hack -- Handle quiver */
	if (IS_QUIVER_SLOT(i))
	{
		p = "In quiver";
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
		case INVEN_ARM:   p = "wearing on your arm"; break;
		case INVEN_HEAD:  p = "wearing on your head"; break;
		case INVEN_HANDS: p = "wearing on your hands"; break;
		case INVEN_FEET:  p = "wearing on your feet"; break;
		default:          p = "carrying in your pack"; break;
	}

	/* Hack -- Handle quiver */
	if (IS_QUIVER_SLOT(i)) p = "carrying in your quiver";

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
	return p;
}





/*
 * Check an item against the item tester info
 */
bool item_tester_okay(const object_type *o_ptr)
{
	/* Hack -- allow listing empty slots */
	if (item_tester_full) return (TRUE);

	/* Require an item */
	if (!o_ptr->k_idx) return (FALSE);

	/* Hack -- ignore "gold" */
	if (o_ptr->tval == TV_GOLD) return (FALSE);

	/* Check the tval */
	if (item_tester_tval)
	{
		if (!(item_tester_tval == o_ptr->tval)) return (FALSE);
	}

	/* Check the hook */
	if (item_tester_hook)
	{
		if (!(*item_tester_hook)(o_ptr)) return (FALSE);
	}

	/* Assume okay */
	return (TRUE);
}



/*
 * Get the indexes of objects at a given floor location.
 *
 * Return the number of object indexes acquired.
 *
 * Never acquire more than "size" object indexes, and never return a
 * number bigger than "size", even if more floor objects exist.
 *
 * Valid flags are any combination of the bits:
 *
 *   0x01 -- Verify item tester
 *   0x02 -- Marked items only
 */
int scan_floor(int *items, int size, int y, int x, int mode)
{
	int this_o_idx, next_o_idx;

	int num = 0;

	/* Sanity */
	if (!in_bounds(y, x)) return (0);

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
	{
		object_type *o_ptr;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Verify item tester */
		if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

		/* Marked items only */
		if ((mode & 0x02) && !o_ptr->marked) continue;

		/* Accept this item */
		items[num++] = this_o_idx;

		/* Enforce size limit */
		if (num >= size) break;
	}

	/* Result */
	return (num);
}



/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	register int i, n, z = 0;

	object_type *o_ptr;

	byte attr;

	char tmp_val[80];

	char o_name[80];


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
		Term_putstr(0, i, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Display the entry itself */
		Term_putstr(3, i, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3+n, i, 255);

		/* Display the weight if needed */
		if (o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			Term_putstr(71, i, -1, TERM_WHITE, tmp_val);
		}
	}

	/*
	 * Add notes about slots used by the quiver, if we have space, want
	 * to show all slots, and have items in the quiver.
	 */
	if ((p_ptr->pack_size_reduce) && (i <= (INVEN_PACK - p_ptr->pack_size_reduce)))
	{
		int ammo_num = 0, ammo_slot, j;

		/* Count quiver ammo */
		for (j = INVEN_QUIVER; j < END_QUIVER; j++)
		{
			/* Get the object */
			o_ptr = &inventory[j];

			/* Ignore empty objects */
			if (!o_ptr->k_idx) continue;

			/* Increment counter */
			ammo_num += o_ptr->number * quiver_space_per_unit(o_ptr);
		}

		/* Leave out space till the bottom-most w) line */
		for (i = z; i < (INVEN_PACK - p_ptr->pack_size_reduce); i++) {
			Term_erase(0, i, 255);
		}

		for (j = 0; j < p_ptr->pack_size_reduce; j++)
		{
			/* Get the number of missiles gathered in this slot */
			if (j == 0)
			{
				ammo_slot = ammo_num -
					99 * (p_ptr->pack_size_reduce - 1);
			}
			else
			{
				ammo_slot = 99;
			}

			/* Hack -- use "(QUIVER)" as a description. */
			strnfmt(o_name, sizeof(o_name),
				"   (QUIVER - %d missile%s)", ammo_slot,
				(ammo_slot == 1) ? "": "s");

			/* Obtain the length of the description */
			n = strlen(o_name);

			/* Display the entry itself */
			Term_putstr(0, i, n, TERM_BLUE, o_name);

			/* Erase the rest of the line */
			Term_erase(n, i, 255);

			/* Go to next line. */
			i++;
		}
	}

	/* Erase the rest of the window */
	for (; i < Term->hgt; i++)
	{
		/* Erase the line */
		Term_erase(0, i, 255);
	}
}



/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
	register int i, n;
	object_type *o_ptr;
	byte attr;

	char tmp_val[80];

	char o_name[80];

	char ptag_desc[MAX_QUIVER][10];
	byte ptag_len[MAX_QUIVER];
	byte ptag_color[MAX_QUIVER];
	byte max_ptag_len = 0;
	byte ptag_space;

	/* Get the pseudo-tags of the quiver slots */
	/* Calculate the maximum length of the pseudo-tags (for alignment) */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* The index in the temporary arrays */
		int q = i - INVEN_QUIVER;

		/* Paranoia */
		ptag_len[q] = 0;

		/* Get the object */
		o_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Store pseudo-tag data in the arrays */
		ptag_len[q] = get_pseudo_tag(i, ptag_desc[q],
			sizeof(ptag_desc[q]), &ptag_color[q]);

		/* Update the maximum length if necessary */
		if (ptag_len[q] > max_ptag_len)
		{
			max_ptag_len = ptag_len[q];
		}
	}

	/* Display the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Examine the item */
		o_ptr = &inventory[i];

		/* Hack -- Never show empty quiver slots */
		if (!o_ptr->k_idx && IS_QUIVER_SLOT(i))
		{
			/* Clear that line */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
		}

		/* Hack -- Never show the gap between equipment and quiver */
		if (i == INVEN_BLANK)
		{
			/* Clear that line */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
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
		Term_putstr(0, i - INVEN_WIELD, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Regular slots don't have a pseudo-tag */
		ptag_space = 0;

		/* Show quiver slot pseudo-tag if needed */
		if (IS_QUIVER_SLOT(i) &&
			(ptag_len[i - INVEN_QUIVER] > 0))
		{
			/* The index in the temporary arrays */
			int q = i - INVEN_QUIVER;

			/* Reserve space for the pseudo-tag in this case */
			ptag_space = max_ptag_len + 1;

			/* Hack -- Clear that space first */
			Term_erase(3, i - INVEN_WIELD, ptag_space);

			/* Show the pseudo-tag */
			Term_putstr(3 + max_ptag_len - ptag_len[q],
				i - INVEN_WIELD, ptag_len[q],
				ptag_color[q], ptag_desc[q]);
		}

		/* Display the entry itself */
		Term_putstr(3 + ptag_space , i - INVEN_WIELD, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3 + ptag_space + n, i - INVEN_WIELD, 255);

		/* Display the slot description (if needed) */
		Term_putstr(61, i - INVEN_WIELD, -1, TERM_WHITE, "<--");

		Term_putstr(65, i - INVEN_WIELD, -1, TERM_WHITE, mention_use(i));

		/* Display the weight (if needed) */
		if (o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = 52;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			Term_putstr(col, i - INVEN_WIELD, -1, TERM_WHITE, tmp_val);
		}
	}

	/* Erase the rest of the window */
	for (i = INVEN_TOTAL - INVEN_WIELD; i < Term->hgt; i++)
	{
		/* Clear that line */
		Term_erase(0, i, 255);
	}
}



/*
 * Display the inventory.
 *
 * Hack -- do not display "trailing" empty slots
 */
void show_inven(void)
{
	int i, j, k, l, z = 0;
	int col, len, lim;
	int wgt;

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[80];

	int out_index[24];
	byte out_color[24];
	char out_desc[24][80];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 70 - 3;

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
		my_strcpy(out_desc[k], o_name, sizeof(out_desc[0]));

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 14;

		/* Maintain the maximum length */
		if (l > len) len = l;

		/* Advance to next "line" */
		k++;
	}

	/* Find the column to start in */
	col = (len > 76) ? 0 : (79 - len);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(i));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j + 1, col);

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

		/* Display the weight */
		wgt = o_ptr->weight * o_ptr->number;
		sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
		put_str(tmp_val, j + 1, 71);
	}

	/*
	 * Add notes about slots used by the quiver, if we have space, want
	 * to show all slots, and have items in the quiver.
	 */
	if ((p_ptr->pack_size_reduce) && (item_tester_full) &&
		(j <= (INVEN_PACK - p_ptr->pack_size_reduce)))
	{
		int ammo_num = 0, ammo_slot;

		/* Count quiver ammo */
		for (i = INVEN_QUIVER; i < END_QUIVER; i++)
		{
			/* Get the object */
			o_ptr = &inventory[i];

			/* Ignore empty objects */
			if (!o_ptr->k_idx) continue;

			/* Increment counter */
			ammo_num += o_ptr->number * quiver_space_per_unit(o_ptr);
		}

		/* Insert a blank dividing line, if we have the space. */
		if (j <= ((INVEN_PACK - 1) - p_ptr->pack_size_reduce))
		{
			j++;

			prt("", j, col ? col - 2 : col);
		}

		for (i = 0; i < p_ptr->pack_size_reduce; i++)
		{
			/* Go to next line. */
			j++;

			prt("", j, col ? col - 2 : col);

			/* Determine index, print it out. */
			sprintf(tmp_val, "%c)", index_to_label(INVEN_PACK -
				p_ptr->pack_size_reduce + i));

			put_str(tmp_val, j, col);

			/* Get the number of missiles gathered in this slot */
			if (i == 0)
			{
				ammo_slot = ammo_num -
					99 * (p_ptr->pack_size_reduce - 1);
			}
			else
			{
				ammo_slot = 99;
			}

			/* Hack -- use "(QUIVER)" as a description. */
			strnfmt(o_name, sizeof(o_name),
				"(QUIVER - %d missile%s)", ammo_slot,
				(ammo_slot == 1) ? "": "s");

			c_put_str(TERM_BLUE, o_name, j, col + 3);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < TERM_LAST_ROW)) prt("", j + 1, col ? col - 2 : col);
}


/*
 * Display the equipment.
 */
void show_equip(void)
{
	int i, j, k, l;
	int col, len, lim;

	object_type *o_ptr;

	char tmp_val[80];

	char o_name[80];

	int out_index[24];
	byte out_color[24];
	char out_desc[24][80];

	char ptag_desc[MAX_QUIVER][10];
	byte ptag_len[MAX_QUIVER];
	byte ptag_color[MAX_QUIVER];
	byte max_ptag_len = 0;
	byte ptag_space;

	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 79 - 28;

	/*
	 * Get the pseudo-tags of the quiver slots
	 * Calculate the maximum length of the pseudo-tags
	 */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* The index in the temporary arrays */
		int q = i - INVEN_QUIVER;

		/* Paranoia */
		ptag_len[q] = 0;

		/* Get the object */
		o_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!o_ptr->k_idx) continue;

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Store pseudo-tag data in the arrays */
		ptag_len[q] = get_pseudo_tag(i, ptag_desc[q],
			sizeof(ptag_desc[q]), &ptag_color[q]);

		/* Update the maximum length if necessary */
		if (ptag_len[q] > max_ptag_len)
		{
			max_ptag_len = ptag_len[q];
		}
	}

	/* Scan the equipment list */
	for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Don't show empty quiver slots */
		if (!o_ptr->k_idx && IS_QUIVER_SLOT(i)) continue;

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Regular slots don't have a pseudo-tag */
		ptag_space = 0;

		/* Truncate the description */
		o_name[lim - ptag_space] = 0;

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->tval % N_ELEMENTS(tval_to_attr)];

		/* Save the description */
		my_strcpy(out_desc[k], o_name, sizeof(out_desc[0]));

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3) + ptag_space + 25 ;

		/* Maintain the max-length */
		if (l > len) len = l;

		/* Advance the entry */
		k++;
	}

	/* Hack -- Find a column to start in */
	col = (len > 76) ? 0 : (79 - len);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		int wgt;

		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Show a blank line between "real" equipment and quiver */
		if (i == INVEN_BLANK) continue;

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(i));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

		/* Mention the use */
		strnfmt(tmp_val, sizeof(tmp_val), "%-14s: ", mention_use(i));
		put_str(tmp_val, j+1, col + 3);

		/* Show the pseudo-tag if needed */
		if (IS_QUIVER_SLOT(i) &&
			(ptag_len[i - INVEN_QUIVER] > 0))
		{
			int q = i - INVEN_QUIVER;
			c_put_str(ptag_color[q], ptag_desc[q],
				j+1, col + 3 + 13 - ptag_len[q]);
		}

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j+1, col + 3 + 14 + 2);

		/* Display the weight */
		wgt = o_ptr->weight * o_ptr->number;
		sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
		put_str(tmp_val, j+1, 71);
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < TERM_LAST_ROW)) prt("", j + 1, col ? col - 2 : col);
}


/*
 * Display a list of the items on the floor at the given location.
 */
void show_floor(const int *floor_list, int floor_num)
{
	int i, j, k, l;
	int col, len, lim;

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[80];

	int out_index[MAX_FLOOR_STACK];
	byte out_color[MAX_FLOOR_STACK];
	char out_desc[MAX_FLOOR_STACK][80];

	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 79 - 12;

	/* Display the inventory */
	for (k = 0, i = 0; i < floor_num; i++)
	{
		o_ptr = &o_list[floor_list[i]];

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
		my_strcpy(out_desc[k], o_name, sizeof(out_desc[0]));

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 14;

		/* Maintain the maximum length */
		if (l > len) len = l;

		/* Advance to next "line" */
		k++;
	}

	/* Find the column to start in */
	col = (len > 76) ? 0 : (79 - len);

	/* Output each entry */
	for (j = 0; j < k; j++)
	{
		int wgt;

		/* Get the index */
		i = floor_list[out_index[j]];

		/* Get the item */
		o_ptr = &o_list[i];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(out_index[j]));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j + 1, col);

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

		/* Display the weight */
		wgt = o_ptr->weight * o_ptr->number;
		sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
		put_str(tmp_val, j + 1, 71);
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);
}


/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < ANGBAND_TERM_MAX; j++)
	{
		/* Unused */
		if (!angband_term[j]) continue;

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
	char o_name[80];

	char out_val[160];

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
	strnfmt(out_val, sizeof(out_val), "%s %s? ", prompt, o_name);

	/* Query */
	return (get_check(out_val));
}


/*
 * Hack -- allow user to "prevent" certain choices.
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
	if (!o_ptr->obj_note) return (TRUE);

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->obj_note), '!');

	/* Process preventions */
	while (s)
	{
		/* Check the "restriction" */
		if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
		{
			int y = p_ptr->py;
			int x = p_ptr->px;

			/* Special case. Drop/sell command */
			if (p_ptr->command_cmd == 'd')
			{
				/* We are selling stuff in some shop */
				if (cave_shop_bold(y, x) && (f_info[cave_feat[y][x]].f_power != STORE_HOME))
				{
					if (!verify_item("Really sell", item)) return (FALSE);
				}
				/* Just drop stuff */
				else
				{
				      	if (!verify_item("Really drop", item)) return (FALSE);
				}
			}

			/* Verify the choice */
			else if (!verify_item("Really try", item)) return (FALSE);
		}

		/* Find another '!' */
		s = strchr(s + 1, '!');
	}

	/* Allow it */
	return (TRUE);
}


/*
 * Verify the "okayness" of a given item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool get_item_okay(int item)
{
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

	/* Verify the item */
	return (item_tester_okay(o_ptr));
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
static int get_tag(int *cp, char tag)
{
	int i;
	cptr s;

	/*
	 * The 'f'ire and 't'hrow commands behave differently when we are using the
	 * equipment (quiver)
	 */
	if (((p_ptr->command_cmd == 'f') || (p_ptr->command_cmd == 'v')) && (p_ptr->command_wrk == USE_EQUIP))
	{
		/* The pseudo-tag */
		byte tag_num = 0;
		object_type *o_ptr;
		byte group;

		/* Get the proper quiver group to determine which objects can be selected */
		if (p_ptr->command_cmd == 'f')
		{
			/* Ammo groups are taken from the missile weapon */
			switch (p_ptr->ammo_tval)
			{
				case TV_BOLT:	group = QUIVER_GROUP_BOLTS;	break;
				case TV_ARROW:	group = QUIVER_GROUP_ARROWS;	break;
				default:	group = QUIVER_GROUP_SHOTS;	break;
			}
		}
		/* Hack - Everything else is a throwing weapon */
		else
		{
		 	group = QUIVER_GROUP_THROWING_WEAPONS;
		}

		/* Iterate over the quiver */
		for (i = INVEN_QUIVER; i < END_QUIVER; i++)
		{
			o_ptr = &inventory[i];

			/* (Paranoia) Ignore empty slots */
			if (!o_ptr->k_idx) continue;

			/* Groups must be equal */
			if (quiver_get_group(o_ptr) != group) continue;

			/* Allow pseudo-tag override */
			(void)get_tag_num(i, quiver_group[group].cmd, &tag_num);

			/* We have a match? */
			if (I2D(tag_num) == tag)
			{
				*cp = i;
				return TRUE;
			}

			/* Try with the next pseudo-tag */
			++tag_num;
		}
	}

	/* Check every object */
	for (i = 0; i < INVEN_TOTAL; ++i)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip empty inscriptions */
		if (!o_ptr->obj_note) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->obj_note), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Save the actual inventory ID */
				*cp = i;

				/* Success */
				return (TRUE);
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))
			{
				/* Save the actual inventory ID */
				*cp = i;

				/* Success */
				return (TRUE);
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
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
 * The equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
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
 * Note that the "easy_floor" option affects this function in several ways.
 *
 * Note that only "acceptable" floor objects get indexes, so between two
 * commands, the indexes of floor objects may change.  XXX XXX XXX
 */
bool get_item(int *cp, cptr pmt, cptr str, int mode)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	char which;

	int i, j, k;

	int i1, i2;
	int e1, e2;
	int f1, f2;

	bool done, item;

	bool oops = FALSE;

	bool use_inven = ((mode & (USE_INVEN)) ? TRUE : FALSE);
	bool use_equip = ((mode & (USE_EQUIP)) ? TRUE : FALSE);
	bool use_floor = ((mode & (USE_FLOOR)) ? TRUE : FALSE);
	bool use_quiver = ((mode & (USE_QUIVER)) ? TRUE: FALSE);

	bool allow_inven = FALSE;
	bool allow_equip = FALSE;
	bool allow_floor = FALSE;

	bool toggle = FALSE;

	char tmp_val[160];
	char out_val[160];

	int floor_list[MAX_FLOOR_STACK];
	int floor_num;

	bool allow_list;

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

	/* Hack -- The quiver is displayed in the equipment window */
	if (use_quiver) use_equip = TRUE;

	/* Full equipment */
	e1 = INVEN_WIELD;
	e2 = INVEN_TOTAL - 1;

	/* Forbid equipment */
	if (!use_equip) e2 = -1;

	/* Restrict the beginning of the equipment */
	if (use_quiver) e1 = INVEN_QUIVER;

	/* Restrict equipment indexes */
	while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
	while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

	/* Accept equipment */
	if (e1 <= e2) allow_equip = TRUE;

	/* Scan all objects in the grid */
	floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, py, px, 0x00);

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
		if (p_ptr->command_see &&
		    (p_ptr->command_wrk == (USE_EQUIP)) &&
		    use_equip)
		{
			p_ptr->command_wrk = (USE_EQUIP);
		}

		/* Hack -- Start on equipment if shooting or throwing */
		else if (((p_ptr->command_cmd == 'f') || (p_ptr->command_cmd == 'v')) && allow_equip)
		{
			p_ptr->command_wrk = (USE_EQUIP);
		}

		/* Use inventory if allowed */
		else if (use_inven)
		{
			p_ptr->command_wrk = (USE_INVEN);
		}

		/* Use equipment if allowed */
		else if (use_equip)
		{
			p_ptr->command_wrk = (USE_EQUIP);
		}

		/* Use floor if allowed */
		else if (use_floor)
		{
			p_ptr->command_wrk = (USE_FLOOR);
		}

		/* Hack -- Use (empty) inventory */
		else
		{
			p_ptr->command_wrk = (USE_INVEN);
		}
	}

	/* Option to always show a list */
	if (auto_display_lists)
	{
		p_ptr->command_see = TRUE;
	}


	/* Start out in "display" mode */
	if (p_ptr->command_see)
	{
		/* Save screen */
		screen_save();
	}


	/* Repeat until done */
	while (!done)
	{
		int ni = 0;
		int ne = 0;

		/* Scan windows */
		for (j = 0; j < ANGBAND_TERM_MAX; j++)
		{
			/* Unused */
			if (!angband_term[j]) continue;

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
			sprintf(out_val, "Inven:");

			/* List choices */
			if (i1 <= i2)
			{
				/* Build the prompt */
				sprintf(tmp_val, " %c-%c,",
				        index_to_label(i1), index_to_label(i2));

				/* Append */
				my_strcat(out_val, tmp_val, sizeof(out_val));
			}

			/* Indicate ability to "view" */
			if (!p_ptr->command_see) strcat(out_val, " * to see,");

			/* Indicate legality of "toggle" */
			if (use_equip) strcat(out_val, " / for Equip,");

			/* Indicate legality of the "floor" */
			if (allow_floor) strcat(out_val, " - for floor,");
		}

		/* Viewing equipment */
		else if (p_ptr->command_wrk == (USE_EQUIP))
		{
			/* Redraw if needed */
			if (p_ptr->command_see) show_equip();

			/* Begin the prompt */
			sprintf(out_val, "Equip:");

			/* List choices */
			if (e1 <= e2)
			{
				/* Build the prompt */
				sprintf(tmp_val, " %c-%c,",
				        index_to_label(e1), index_to_label(e2));

				/* Append */
				my_strcat(out_val, tmp_val, sizeof(out_val));
			}

			/* Indicate ability to "view" */
			if (!p_ptr->command_see) strcat(out_val, " * to see,");

			/* Indicate legality of "toggle" */
			if (use_inven) strcat(out_val, " / for Inven,");

			/* Indicate legality of the "floor" */
			if (allow_floor) strcat(out_val, " - for floor,");
		}

		/* Viewing floor */
		else
		{
			/* Redraw if needed */
			if (p_ptr->command_see) show_floor(floor_list, floor_num);

			/* Begin the prompt */
			sprintf(out_val, "Floor:");

			/* List choices */
			if (f1 <= f2)
			{
				/* Build the prompt */
				sprintf(tmp_val, " %c-%c,", I2A(f1), I2A(f2));

				/* Append */
				my_strcat(out_val, tmp_val, sizeof(out_val));
			}

			/* Indicate ability to "view" */
			if (!p_ptr->command_see) strcat(out_val, " * to see,");

			/* Append */
			if (use_inven) strcat(out_val, " / for Inven,");

			/* Append */
			else if (use_equip) strcat(out_val, " / for Equip,");
		}

		/* Finish the prompt */
		strcat(out_val, " ESC");

		/* Build the prompt */
		strnfmt(tmp_val, sizeof(tmp_val), "(%s) %s", out_val, pmt);

		/* Show the prompt */
		prt(tmp_val, 0, 0);

		/* Hack - Find the origin of the next key */
		allow_list = interactive_input(TRUE);

		/* Get a key */
		which = inkey();

		/* Parse it */
		switch (which)
		{
			case ESCAPE:
			{
				done = TRUE;
				break;
			}

			case '*':
			case '?':
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
					screen_save();

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

				/* No toggle allowed */
				else
				{
					bell("Cannot switch item selector!");
					break;
				}

				/* Hack -- Fix screen */
				if (p_ptr->command_see)
				{
					/* Load screen */
					screen_load();

					/* Save screen */
					screen_save();
				}

				/* Need to redraw */
				break;
			}

			case '-':
			{
				/* Paranoia */
				if (!allow_floor)
				{
					bell("Cannot select floor!");
					break;
				}

				if (easy_floor && allow_list)
				{
					/* There is only one item */
					if (f1 == f2)
					{
						/* Hack -- Auto-Select */
						if ((p_ptr->command_wrk == (USE_FLOOR)) ||
						    (!floor_query_flag))
						{
							/* Special index */
							k = 0 - floor_list[f1];

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

					/* Hack -- Fix screen */
					if (p_ptr->command_see)
					{
						/* Load screen */
						screen_load();

						/* Save screen */
						screen_save();
					}

					p_ptr->command_wrk = (USE_FLOOR);

					break;
				}

				/* Check each legal object */
				for (i = 0; i < floor_num; ++i)
				{
					/* Special index */
					k = 0 - floor_list[i];

					/* Skip non-okay objects */
					if (!get_item_okay(k)) continue;

					/* Verify the item (if required) */
					if (floor_query_flag && !verify_item("Try", k)) continue;

					/* Allow player to "refuse" certain actions */
					if (!get_item_allow(k)) continue;

					/* Accept that choice */
					(*cp) = k;
					item = TRUE;
					done = TRUE;
					break;
				}

				break;
			}

			case '0':
			case '1': case '2': case '3':
			case '4': case '5': case '6':
			case '7': case '8': case '9':
			{
				/* Look up the tag */
				if (!get_tag(&k, which))
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Hack -- Validate the item */
				if ((k < INVEN_WIELD) ? !allow_inven : !allow_equip)
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Forbid classic equipment if using the quiver */
				if (use_quiver && (k >= INVEN_WIELD) && !IS_QUIVER_SLOT(k))
				{
					bell("Illegal object choice (tag)!");
					break;
				}

				/* Validate the item */
				if (!get_item_okay(k))
				{
					bell("Illegal object choice (tag)!");
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
				/* Choose "default" inventory item */
				if (p_ptr->command_wrk == (USE_INVEN))
				{
					if (i1 != i2)
					{
						bell("Illegal object choice (default)!");
						break;
					}

					k = i1;
				}

				/* Choose "default" equipment item */
				else if (p_ptr->command_wrk == (USE_EQUIP))
				{
					if (e1 != e2)
					{
						bell("Illegal object choice (default)!");
						break;
					}

					k = e1;
				}

				/* Choose "default" floor item */
				else
				{
					if (f1 != f2)
					{
						bell("Illegal object choice (default)!");
						break;
					}

					k = 0 - floor_list[f1];
				}

				/* Validate the item */
				if (!get_item_okay(k))
				{
					bell("Illegal object choice (default)!");
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

				/* Note verify */
				verify = (isupper((unsigned char)which) ? TRUE : FALSE);

				/* Lowercase */
				which = tolower((unsigned char)which);

				/* Convert letter to inventory index */
				if (p_ptr->command_wrk == (USE_INVEN))
				{
					k = label_to_inven(which);

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

					/* Forbid classic equipment if using the quiver */
					if (use_quiver && !IS_QUIVER_SLOT(k))
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

				/* Validate the item */
				if (!get_item_okay(k))
				{
					bell("Illegal object choice (normal)!");
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

	/* 	Clean up */
	/*	Toggle again if needed. */
	if (toggle) toggle_inven_equip();

	/* Update */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Window stuff */
	window_stuff();

	/* Clear the prompt line */
	prt("", 0, 0);

	/* Warning if needed */
	if (oops && str) msg_print(str);

	/* Save item if available */
	if (item) repeat_push(*cp);

	/* Result */
	return (item);
}
