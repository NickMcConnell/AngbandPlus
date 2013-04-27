/* File: object1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
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
#include "z-quark.h"
#include "option.h"
#include "randname.h"
#include "tvalsval.h"
#include "wind_flg.h"

/*
 * Max sizes of the following arrays.
 */
#define MAX_TITLES     50       /* Used with scrolls (min 48) */


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
		flavor_type *flavor_ptr = &object_kind::flavor_info[i];

		/* Skip random flavors */
		if (flavor_ptr->obj_id.sval == SV_UNKNOWN) continue;

		for (j = 0; j < z_info->k_max; j++)
			/* Skip other objects */
			if (object_type::k_info[j].obj_id == flavor_ptr->obj_id)
				/* Store the flavor index */
				object_type::k_info[j].flavor = i;
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
		if ((object_kind::flavor_info[i].obj_id.tval == tval) &&
		    (object_kind::flavor_info[i].obj_id.sval == SV_UNKNOWN))
			flavor_count++;
	}

	for (i = 0; i < z_info->k_max; i++)
	{
		/* Skip other object types */
		if (object_type::k_info[i].obj_id.tval != tval) continue;

		/* Skip objects that already are flavored */
		if (object_type::k_info[i].flavor != 0) continue;

		/* HACK - Ordinary food is "boring" */
		if ((tval == TV_FOOD) && (object_type::k_info[i].obj_id.sval >= SV_FOOD_MIN_FOOD))
			continue;

		if (!flavor_count) quit_fmt("Not enough flavors for tval %d.", tval);

		/* Select a flavor */
		choice = rand_int(flavor_count);
	
		/* Find and store the flavor */
		for (j = 0; j < z_info->flavor_max; j++)
		{
			/* Skip other tvals */
			if (object_kind::flavor_info[j].obj_id.tval != tval) continue;

			/* Skip assigned svals */
			if (object_kind::flavor_info[j].obj_id.sval != SV_UNKNOWN) continue;

			if (choice == 0)
			{
				/* Store the flavor index */
				object_type::k_info[i].flavor = j;

				/* Mark the flavor as used */
				object_kind::flavor_info[j].obj_id.sval = object_type::k_info[i].obj_id.sval;

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
 * Rings, Amulets, Staffs, Wands, Rods, Food, Potions, Scrolls.
 *
 * The first 4 entries for potions are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused Potion).
 *
 * Scroll titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 2 to 8 letters long, and that no scroll is finished 
 * until it attempts to grow beyond 15 letters.  The first time this 
 * can happen is when the current title has 6 letters and the new word 
 * has 8 letters, which would result in a 6 letter scroll title. 
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

	/* Hack -- Induce consistent flavors */
	Rand_value = seed_flavor;

	flavor_assign_fixed();

	flavor_assign_random(TV_RING);
	flavor_assign_random(TV_AMULET);
	flavor_assign_random(TV_STAFF);
	flavor_assign_random(TV_WAND);
	flavor_assign_random(TV_ROD);
	flavor_assign_random(TV_FOOD);
	flavor_assign_random(TV_POTION);
	flavor_assign_random(TV_SCROLL);

	/* Scrolls (random titles, always white) */
	for (i = 0; i < MAX_TITLES; i++)
	{
		char buf[24];
		char *end = buf;
		int titlelen = 0;
		int wordlen;
		bool okay = TRUE;

		wordlen = randname_make(RANDNAME_SCROLL, 2, 8, end, 24);
		while (titlelen + wordlen < (int)(sizeof(scroll_adj[0]) - 1))
		{
			end[wordlen] = ' ';
			titlelen += wordlen + 1;
			end += wordlen + 1;
			wordlen = randname_make(RANDNAME_SCROLL, 2, 8, end, 24 - titlelen);
		}
		buf[titlelen - 1] = '\0';
          
		/* Check the scroll name hasn't already been generated */
		for (j = 0; j < i; j++)
		{
			if (streq(buf, scroll_adj[j]))
			{
				okay = FALSE;
				break;
			}
		}
          
		if (okay)
		{
			my_strcpy(scroll_adj[i], buf, sizeof(scroll_adj[0]));
		}
		else
		{
			/* Have another go at making a name */
			i--;
		}
	}


	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Analyze every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind* const k_ptr = &object_type::k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->_name) continue;

		/* No flavor yields aware */
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
		feature_type* const f_ptr = &feature_type::f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &object_type::k_info[i];

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for monsters */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &monster_type::r_info[i];

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}

	/* Extract default attr/char code for flavors */
	for (i = 0; i < z_info->flavor_max; i++)
	{
		flavor_type *flavor_ptr = &object_kind::flavor_info[i];

		/* Default attr/char */
		flavor_ptr->x_attr = flavor_ptr->d_attr;
		flavor_ptr->x_char = flavor_ptr->d_char;
	}

	/* Extract attr/chars for inventory objects (by tval) */
	for (i = 0; i < (int)N_ELEMENTS(tval_to_attr); i++)
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
 * Modes of object_flags_aux()
 */
#define OBJECT_FLAGS_FULL   1 /* Full info */
#define OBJECT_FLAGS_KNOWN  2 /* Only flags known to the player */
#define OBJECT_FLAGS_RANDOM 3 /* Only known random flags */


/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b* f)
{
	object_kind *k_ptr;

	if (mode != OBJECT_FLAGS_FULL)
	{
		C_WIPE(f,OBJECT_FLAG_STRICT_UB);	/* Clear */
		if (!o_ptr->known()) return;		/* Must be identified */
	}

	if (mode != OBJECT_FLAGS_RANDOM)
	{
		k_ptr = &object_type::k_info[o_ptr->k_idx];

		/* Base object */
		C_COPY(f,k_ptr->flags,OBJECT_FLAG_STRICT_UB);

		if (mode == OBJECT_FLAGS_FULL)
		{
			/* Artifact */
			if (o_ptr->name1)
			{
				artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];

				C_COPY(f,a_ptr->flags,OBJECT_FLAG_STRICT_UB);
			}
		}

		/* Ego-item */
		if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &object_type::e_info[o_ptr->name2];
			size_t i;
			for(i = 0; i < OBJECT_FLAG_STRICT_UB; ++i)
			{
				 f[i] |= e_ptr->flags[i];
			};
		}

		if (mode == OBJECT_FLAGS_KNOWN)
		{
			/* Obvious artifact flags */
			if (o_ptr->name1)
			{
				artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];

				/* Obvious flags (pval) */
				f[0] = (a_ptr->flags[0] & (TR1_PVAL_MASK));
				f[2] = (a_ptr->flags[2] & (TR3_IGNORE_MASK));
			}
		}
	}

	if (mode != OBJECT_FLAGS_FULL)
	{
		bool spoil = FALSE;

#ifdef SPOIL_ARTIFACTS
		/* Full knowledge for some artifacts */
		if (o_ptr->is_artifact()) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

#ifdef SPOIL_EGO_ITEMS
		/* Full knowledge for some ego-items */
		if (ego_item_p(o_ptr)) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

		/* Need full knowledge or spoilers */
		if (!spoil && !(o_ptr->ident & IDENT_MENTAL)) return;

		/* Artifact */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];

			C_COPY(f,a_ptr->flags,OBJECT_FLAG_STRICT_UB);

			if (mode == OBJECT_FLAGS_RANDOM)
			{
				/* Hack - remove 'ignore' flags */
				f[2] &= ~(TR3_IGNORE_MASK);
			}
		}

		/* Full knowledge for *identified* objects */
		if (!(o_ptr->ident & IDENT_MENTAL)) return;
	}

	/* Extra powers */
	switch (o_ptr->xtra1)
	{
		case OBJECT_XTRA_TYPE_SUSTAIN:
		{
			/* OBJECT_XTRA_WHAT_SUSTAIN == 2 */
			f[1] |= (OBJECT_XTRA_BASE_SUSTAIN << o_ptr->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_RESIST:
		{
			/* OBJECT_XTRA_WHAT_RESIST == 2 */
			f[1] |= (OBJECT_XTRA_BASE_RESIST << o_ptr->xtra2);
			break;
		}

		case OBJECT_XTRA_TYPE_POWER:
		{
			/* OBJECT_XTRA_WHAT_POWER == 3 */
			f[2] |= (OBJECT_XTRA_BASE_POWER << o_ptr->xtra2);
			break;
		}
	}
}




/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b* f)
{
	object_flags_aux(OBJECT_FLAGS_FULL, o_ptr, f);
}



/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, u32b* f)
{
	object_flags_aux(OBJECT_FLAGS_KNOWN, o_ptr, f);
}

/*
 * should we display weapon statistics?
 */
static bool obj_desc_show_weapon(const object_type *o_ptr)
{
	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Analyze the object */
	switch (o_ptr->obj_id.tval)
	{
		/* Missiles/Bows/Weapons */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:	return TRUE;
	};

	/* Display the item like a weapon */
	if (o_ptr->to_h && o_ptr->to_d) return TRUE;

	/* Extract some flags */
	object_flags(o_ptr, f);

	/* Display the item like a weapon */
	return (f[2] & (TR3_SHOW_MODS));
}

/*
 * should we display armor statistics?
 */
static bool obj_desc_show_armor(const object_type *o_ptr)
{
	/* Analyze the object */
	switch (o_ptr->obj_id.tval)
	{
		/* Armour */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CLOAK:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:	return TRUE;
	}

	/* Display the item like armour */
	return o_ptr->ac;
}

/*
 * Should we append the name of the object?
 */
static bool object_desc_append_name(const object_type *o_ptr)
{
	/* if unaware, never append the name of the object */
	if (!o_ptr->aware() && !(o_ptr->ident & IDENT_STORE)) return FALSE;

	/* Analyze the object */
	switch (o_ptr->obj_id.tval)
	{
	case TV_FOOD:	/* Food */
		/* Ordinary food is "boring" */
		if (o_ptr->obj_id.sval >= SV_FOOD_MIN_FOOD) break;

	case TV_AMULET:	/* Amulets (including a few "Specials") */
	case TV_RING:	/* Rings (including a few "Specials") */
	case TV_STAFF:	/* Staffs */
	case TV_WAND:	/* Wands */
	case TV_ROD:	/* Rods */
	case TV_SCROLL:	/* Scrolls */
	case TV_POTION:	/* Potions */
		return TRUE;
	}
	return FALSE;
}

/*
 * KRB: Since we actually need range-checks, hard-code the variables actually used
 */

/*
 * Efficient version of 't += sprintf(t, "%c", (C))'
 */
#define object_desc_chr_macro(C) do { \
 \
	assert((sizeof(tmp_buf) > t - b + 1U) && "already overflowed");	\
	*t++ = (C);	/* Copy the char */ \
	if (sizeof(tmp_buf) <= t - b + 1U) goto object_desc_done;	\
 \
} while (0)


/*
 * Efficient version of 't += sprintf(t, "%s", (S))'
 */
#define object_desc_str_macro(S) do { \
 \
	const char* s = (S); \
	assert((sizeof(tmp_buf) > t - b + 1U) && "already overflowed");	\
 \
	/* Copy the string */ \
	while (*s)	\
	{	\
		*t++ = *s++; \
		if (sizeof(tmp_buf) <= t - b + 1U) goto object_desc_done;	\
	}	\
 \
} while (0)

/*
 * Efficient version of 't += sprintf(t, "%u", (N))'
 */
#define object_desc_num_macro(N) do { \
 \
	int n = (N); \
	int p = 1;	\
	assert((sizeof(tmp_buf) > t - b + 1U) && "already overflowed");	\
 \
	while(10 <= n / p) p *= 10;	\
 \
	do	{	\
		/* Dump the digit */ \
		*t++ = I2D(n / p); \
		if (sizeof(tmp_buf) <= t - b + 1U) goto object_desc_done;	\
		n %= p;	/* Remove the digit */	\
	    p /= 10;	/* Process next digit */ \
		}	\
	while(0 < p);	\
 \
} while (0)



/*
 * Efficient version of 't += sprintf(t, "%+d", (I))'
 */
#define object_desc_int_macro(I) do { \
 \
	int i = (I); \
	assert((sizeof(tmp_buf) > t - b + 1U) && "already overflowed");	\
 \
	/* Negative */ \
	if (0 > i) \
	{ \
		/* Take the absolute value */ \
		i = -i; \
 \
		/* Use a "minus" sign */ \
		*t++ = '-'; \
		if (sizeof(tmp_buf) <= t - b + 1U) goto object_desc_done;	\
	} \
 \
	/* Positive (or zero) */ \
	else \
	{ \
		/* Use a "plus" sign */ \
		*t++ = '+'; \
		if (sizeof(tmp_buf) <= t - b + 1U) goto object_desc_done;	\
	} \
 \
	/* Dump the number itself */ \
	object_desc_num_macro(i); \
 \
} while (0)




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

enum object_desc_mode	{	ODESC_BASE = 0, 
							ODESC_COMBAT, 
							ODESC_STORE,
							ODESC_FULL
						};

 *
 * Modes ("pref" is TRUE):
 *   ODESC_BASE -- Chain Mail of Death
 *   ODESC_COMBAT -- A Cloak of Death [1,+3]
 *   ODESC_STORE -- An Amulet of Death [1,+3] (+2 to Stealth)
 *   ODESC_FULL -- 5 Rings of Death [1,+3] (+2 to Stealth) {nifty}
 *
 * Modes ("pref" is FALSE):
 *   ODESC_BASE -- Chain Mail of Death
 *   ODESC_COMBAT -- Cloak of Death [1,+3]
 *   ODESC_STORE -- Amulet of Death [1,+3] (+2 to Stealth)
 *   ODESC_FULL -- Rings of Death [1,+3] (+2 to Stealth) {nifty}
 */
void object_desc(char *buf, size_t max, const object_type *o_ptr, bool pref, object_desc_mode mode)
{
	object_kind *k_ptr = &object_type::k_info[o_ptr->k_idx];
	const char* basenm = k_ptr->name();	/* Extract default "base" string */
	const char* modstr = "";				/* Assume no "modifier" string */

	bool aware = o_ptr->aware();	/* See if the object is "aware" */
	bool known = o_ptr->known();	/* See if the object is "known" */
	bool flavor = k_ptr->flavor;	/* See if the object is "flavored" */

	const char* s;

	const char* u = NULL;
	const char* v = NULL;

	char tmp_buf[128];
	const char* const b = tmp_buf;	/* Start dumping the result */
	char* t = tmp_buf;

	u32b f[OBJECT_FLAG_STRICT_UB];

	/* Extract some flags */
	object_flags(o_ptr, f);

	/* Object is in the inventory of a store */
	if (o_ptr->ident & IDENT_STORE)
	{
		/* Pretend known and aware */
		aware = TRUE;
		known = TRUE;
	}

	/* Allow flavors to be hidden when aware */
	if (aware && !OPTION(show_flavors)) flavor = FALSE;

	/* Analyze the object */
	/* aware artifacts do not need much analysis */
	if (o_ptr->is_artifact() && aware)
	{
		switch (o_ptr->obj_id.tval)
		{		
			/* Some objects are easy to describe */
			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_SPIKE:
			case TV_FLASK:
			case TV_CHEST:

			/* Missiles/Bows/Weapons */
			case TV_SHOT:
			case TV_BOLT:
			case TV_ARROW:
			case TV_BOW:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_DIGGING:

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

			/* Lites (including a few "Specials") */
			case TV_LITE:

			/* Amulets (including a few "Specials") */
			case TV_AMULET:

			/* Rings (including a few "Specials") */
			case TV_RING:

			/* Staffs */
			case TV_STAFF:

			/* Wands */
			case TV_WAND:

			/* Rods */
			case TV_ROD:

			/* Scrolls */
			case TV_SCROLL:

			/* Potions */
			case TV_POTION:

			/* Food */
			case TV_FOOD:

			/* Magic Books */
			case TV_MAGIC_BOOK:

			/* Prayer Books */
			case TV_PRAYER_BOOK:
				break;

			/* Gold/Gems */
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
	}
	else
	{
		switch (o_ptr->obj_id.tval)
		{
			/* Some objects are easy to describe */
			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_SPIKE:
			case TV_FLASK:
			case TV_CHEST:

			/* Missiles/Bows/Weapons */
			case TV_SHOT:
			case TV_BOLT:
			case TV_ARROW:
			case TV_BOW:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			case TV_DIGGING:

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

			/* Lites (including a few "Specials") */
			case TV_LITE:
				break;

			/* Amulets (including a few "Specials") */
			case TV_AMULET:
			{
				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Amulet~" : "& Amulet~");

				break;
			}

			/* Rings (including a few "Specials") */
			case TV_RING:
			{
				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Ring~" : "& Ring~");

				break;
			}

			/* Staffs */
			case TV_STAFF:
			{
				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Staff~" : "& Staff~");

				break;
			}

			/* Wands */
			case TV_WAND:
			{
				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Wand~" : "& Wand~");

				break;
			}

			/* Rods */
			case TV_ROD:
			{
				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Rod~" : "& Rod~");

				break;
			}

			/* Scrolls */
			case TV_SCROLL:
			{
				/* Color the object */
				modstr = scroll_adj[o_ptr->obj_id.sval];
				basenm = (flavor ? "& Scroll~ titled \"#\"" : "& Scroll~");

				break;
			}

			/* Potions */
			case TV_POTION:
			{
				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Potion~" : "& Potion~");

				break;
			}

			/* Food */
			case TV_FOOD:
			{
				/* Ordinary food is "boring" */
				if (o_ptr->obj_id.sval >= SV_FOOD_MIN_FOOD) break;

				/* Color the object */
				modstr = k_ptr->flavor_text();
				basenm = (flavor ? "& # Mushroom~" : "& Mushroom~");

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

			/* Gold/Gems */
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
	}


	/* Begin */
	s = basenm;

	/* 0 == t - b */
	/* Handle objects which sometimes use "a" or "an" */
	if (*s == '&')
	{
		assert(' ' == s[1] && "data format");

		/* Skip the ampersand and the following space */
		s += 2;

		/* 2 == t - b */
		/* No prefix */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- None left */
		else if (o_ptr->number <= 0)
		{
			strcpy(t, "no more ");
			t += sizeof("no more ") - 1;
		}

		/* Extract the number */
		else if (o_ptr->number > 1)
		{
			object_desc_num_macro(o_ptr->number);
			object_desc_chr_macro(' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && o_ptr->is_artifact())
		{
			strcpy(t, "The ");
			t += sizeof("The ") - 1;
		}

		/* Hack -- A single one, and next character will be a vowel */
		else if (is_a_vowel((*s == '#') ? modstr[0] : *s))
		{
			strcpy(t, "an ");
			t += sizeof("an ") - 1;
		}

		/* A single one, and next character will be a non-vowel */
		else
		{
			strcpy(t, "a ");
			t += sizeof("a ") - 1;
		}
		/* 0 <= t - b <= 8 */
	}

	/* Handle objects which never use "a" or "an" */
	/* require a prefixed numeric indicator */
	else if (pref)
	{
		/* Hack -- all gone */
		if (0 >= o_ptr->number)
		{
			strcpy(t, "no more ");
			t += sizeof("no more ") - 1;
		}

		/* Prefix a number if required */
		else if (1 < o_ptr->number)
		{
			object_desc_num_macro(o_ptr->number);
			object_desc_chr_macro(' ');
		}

		/* The only one of its kind */
		else if (known && o_ptr->is_artifact())
		{
			strcpy(t, "The ");
			t += sizeof("The ") - 1;
		};

		/* A single item, so no prefix needed */

		/* 0 <= t - b <= 8 */
	}
	/* 0 <= t - b <= 8 */

	/* Copy the string */
	for (; *s; s++)
	{
		/* Pluralizer */
		if (*s == '~')
		{
			/* Add a plural if needed */
			if ((o_ptr->number != 1) && !(known && o_ptr->is_artifact()))
			{
				char k = t[-1];

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h')) object_desc_chr_macro('e');

				/* Add an 's' */
				object_desc_chr_macro('s');
			}
		}

		/* Modifier */
		else if (*s == '#')
		{
			/* Append the modifier */
			object_desc_str_macro(modstr);
		}

		/* Normal */
		else
		{
			/* Copy */
			object_desc_chr_macro(*s);
		}
	}


	/* Append the "kind name" to the "base name" */
	if (object_desc_append_name(o_ptr))
	{
		object_desc_str_macro(" of ");
		object_desc_str_macro(k_ptr->name());
	}


	/* Hack -- Append "Artifact" or "Special" names */
	if (known)
	{
		/* Grab any artifact name */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &object_type::a_info[o_ptr->name1];

			object_desc_chr_macro(' ');
			object_desc_str_macro(a_ptr->name());
		}

		/* Grab any ego-item name */
		else if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &object_type::e_info[o_ptr->name2];

			object_desc_chr_macro(' ');
			object_desc_str_macro(e_ptr->name());
		}
	}


	/* No more details wanted */
	if (ODESC_BASE == mode) goto object_desc_done;


	/* Hack -- Chests must be described in detail, if known (already been searched) */
	if (o_ptr->obj_id.tval == TV_CHEST && known)
	{
		const char* tail = NULL;

		/* May be "empty" */
		if (!o_ptr->pval)
			tail = " (empty)";

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
			tail = (chest_traps[0 - o_ptr->pval]) ? " (disarmed)" : " (unlocked)";

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
		assert(NULL != tail);
		object_desc_str_macro(tail);
	}


	/* Dump base weapon info */
	switch (o_ptr->obj_id.tval)
	{
		/* Missiles */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:	/* Fall through */

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Append a "damage" string */
			object_desc_str_macro(" (");
			object_desc_num_macro(o_ptr->d.dice);
			object_desc_chr_macro('d');
			object_desc_num_macro(o_ptr->d.sides);
			object_desc_chr_macro(')');

			/* All done */
			break;
		}

		/* Bows */
		case TV_BOW:
		{
			/* Hack -- Extract the "base power" */
			int power = (o_ptr->obj_id.sval % 10);

			/* Append a "power" string */
			object_desc_str_macro(" (");
			object_desc_chr_macro('x');
			object_desc_num_macro(power);
			object_desc_chr_macro(')');

			/* All done */
			break;
		}
	}


	/* Add the weapon bonuses */
	if (known)
	{
		/* Show the tohit/todam on request */
		if (obj_desc_show_weapon(o_ptr))
		{
			object_desc_str_macro(" (");
			object_desc_int_macro(o_ptr->to_h);
			object_desc_chr_macro(',');
			object_desc_int_macro(o_ptr->to_d);
			object_desc_chr_macro(')');
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			object_desc_str_macro(" (");
			object_desc_int_macro(o_ptr->to_h);
			object_desc_chr_macro(')');
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			object_desc_str_macro(" (");
			object_desc_int_macro(o_ptr->to_d);
			object_desc_chr_macro(')');
		}
	}


	/* Add the armor bonuses */
	if (obj_desc_show_armor(o_ptr))
	{
		if (known)
		{	/* Show the armor class info */
			object_desc_str_macro(" [");
			object_desc_num_macro(o_ptr->ac);
			object_desc_chr_macro(',');
			object_desc_int_macro(o_ptr->to_a);
			object_desc_chr_macro(']');
		}
		else
		{	/* always show base armor */
			object_desc_str_macro(" [");
			object_desc_num_macro(o_ptr->ac);
			object_desc_chr_macro(']');
		}
	}
	else if (known && o_ptr->to_a)
	{	/* No base armor, but does increase armor */
		object_desc_str_macro(" [");
		object_desc_int_macro(o_ptr->to_a);
		object_desc_chr_macro(']');
	}


	/* No more details wanted */
	if (ODESC_COMBAT == mode) goto object_desc_done;

	/* Hack -- Process Lanterns/Torches */
	if ((o_ptr->obj_id.tval == TV_LITE) && (!o_ptr->is_artifact()))
	{
		/* Hack -- Turns of light for normal lites */
		object_desc_str_macro(" (with ");
		object_desc_num_macro(o_ptr->pval);
		object_desc_str_macro(" turns of light)");
	}


	/* Dump "pval" flags for wearable items */
	if (known && (f[0] & (TR1_PVAL_MASK)))
	{
		const char* tail = "";
		const char* tail2 = "";

		/* Start the display */
		object_desc_str_macro(" (");

		/* Dump the "pval" itself */
		object_desc_int_macro(o_ptr->pval);

		/* Do not display the "pval" flags */
		if (f[2] & (TR3_HIDE_TYPE))
		{
			/* Nothing */
		}

		/* Stealth */
		else if (f[0] & (TR1_STEALTH))
		{
			/* Dump " to stealth" */
			tail = " to stealth";
		}

		/* Searching */
		else if (f[0] & (TR1_SEARCH))
		{
			/* Dump " to searching" */
			tail = " to searching";
		}

		/* Infravision */
		else if (f[0] & (TR1_INFRA))
		{
			/* Dump " to infravision" */
			tail = " to infravision";
		}

#if 0

		/* Tunneling */
		else if (f[0] & (TR1_TUNNEL))
		{
			/* Dump " to digging" */
			tail = " to digging";
		}

#endif

		/* Speed */
		else if (f[0] & (TR1_SPEED))
		{
			/* Dump " to speed" */
			tail = " to speed";
		}

		/* Blows */
		else if (f[0] & (TR1_BLOWS))
		{
			/* Add " attack" */
			tail = " attack";

			/* Add "attacks" */
			if (ABS(o_ptr->pval) != 1) tail2 = "s";
		}

#if 0

		/* Shots */
		else if (f[0] & (TR1_SHOTS))
		{
			/* Nothing */
		}

		/* Might */
		else if (f[0] & (TR1_MIGHT))
		{
			/* Nothing */
		}

#endif

		/* Add the descriptor */
		object_desc_str_macro(tail);
		object_desc_str_macro(tail2);

		/* Finish the display */
		object_desc_chr_macro(')');
	}

	/* Hack -- Wands and Staffs have charges */
	if (known &&
	    ((o_ptr->obj_id.tval == TV_STAFF) ||
	     (o_ptr->obj_id.tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		object_desc_str_macro(" (");
		object_desc_num_macro(o_ptr->pval);
		object_desc_str_macro(" charge");
		if (o_ptr->pval != 1)
		{
			object_desc_chr_macro('s');
		}
		object_desc_chr_macro(')');
	}

	/* Hack -- Rods have a "charging" indicator */
	else if (known && (o_ptr->obj_id.tval == TV_ROD))
	{
		/* Hack -- Dump " (# charging)" if relevant */
		if (o_ptr->timeout > 0)
		{
			/* Stacks of rods display an exact count of charging rods. */
			if (o_ptr->number > 1)
			{
				assert(0 < k_ptr->pval && "rods must have positive pval");

				/* Find out how many rods are charging, by dividing
				 * current timeout by each rod's maximum timeout.
				 * Ensure that any remainder is rounded up.  Display
				 * very discharged stacks as merely fully discharged.
				 */
				int power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

				if (power > o_ptr->number) power = o_ptr->number;

				/* Display prettily */
				object_desc_str_macro(" (");
				object_desc_num_macro(power);
				object_desc_str_macro(" charging)");
			}
			else
			{
				/* Single rod */
				object_desc_str_macro(" (charging)");
			}
		}
	}

	/* Indicate "charging" artifacts */
	else if (known && o_ptr->timeout)
	{
		/* Hack -- Dump " (charging)" if relevant */
		object_desc_str_macro(" (charging)");
	}


	/* No more details wanted */
	if (ODESC_STORE == mode) goto object_desc_done;


	/* Use standard inscription, if present */
	if (o_ptr->note) u = quark_str(o_ptr->note);

	/* Use special inscription, if any */
	if (o_ptr->pseudo >= INSCRIP_NULL)
	{
		v = inscrip_text[o_ptr->pseudo - INSCRIP_NULL];
	}

	/* Use "cursed" if the item is known to be cursed */
	else if (o_ptr->is_cursed() && known)
	{
		v = "cursed";
	}

	/* Hack -- Use "empty" for empty wands/staffs */
	else if (!known && (o_ptr->ident & (IDENT_EMPTY)))
	{
		v = "empty";
	}

	/* Use "tried" if the object has been tested unsuccessfully */
	else if (!aware && o_ptr->tried())
	{
		v = "tried";
	};

	/* Nothing */


	/* Inscription */
	if (u || v)
	{
		/* Begin the inscription */
		object_desc_str_macro(" {");

		/* Standard inscription */
		if (u) object_desc_str_macro(u);

		/* put in a separator if both inscriptions exist */
		if (u && v) object_desc_str_macro(", ");

		/* Special inscription */
		if (v) object_desc_str_macro(v);

		/* Terminate the inscription */
		object_desc_chr_macro('}');
	}


object_desc_done:

	/* Terminate */
	*t = '\0';

	/* Copy the string over */
	my_strcpy(buf, tmp_buf, max);
}

/* clear macros */
#undef object_desc_avoid_overflow
#undef object_desc_chr_macro
#undef object_desc_str_macro
#undef object_desc_num_macro
#undef object_desc_int_macro

/*
 * Describe an item and pretend the item is fully known and has no flavor.
 */
void object_desc_spoil(char *buf, size_t max, const object_type *o_ptr, bool pref, object_desc_mode mode)
{
	object_type tmp = *o_ptr;

	/* HACK - Pretend the object is in a store inventory */
	tmp.ident |= IDENT_STORE;

	/* Describe */
	object_desc(buf, max, &tmp, pref, mode);
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
	text_out_wrap = 75;

	/* Dump the info */
	if (object_info_out(o_ptr))
		text_out("\n");

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
{	/* Convert */
	const int i = (islower((unsigned char)c) ? A2I(c) : -1);

	/* Verify the index */
	if ((i < 0) || (i > INVEN_PACK)) return (-1);

	/* Empty slots can never be chosen */
	if (!p_ptr->inventory[i].k_idx) return (-1);

	/* Return the index */
	return (i);
}


/*
 * Convert a label into the index of a item in the "equip".
 *
 * Return "-1" if the label does not indicate a real item.
 */
s16b label_to_equip(int c)
{	/* Convert */
	const int i = (islower((unsigned char)c) ? A2I(c) : -1) + INVEN_WIELD;

	/* Verify the index */
	if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return (-1);

	/* Empty slots can never be chosen */
	if (!p_ptr->inventory[i].k_idx) return (-1);

	/* Return the index */
	return (i);
}



/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(const object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->obj_id.tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:		return INVEN_WIELD;

		case TV_BOW:		return INVEN_BOW;

		case TV_RING:
		{
			/* Use the right hand first */
			if (!p_ptr->inventory[INVEN_RIGHT].k_idx) return INVEN_RIGHT;

			/* Use the left hand for swapping (by default) */
			return INVEN_LEFT;
		}

		case TV_AMULET:		return INVEN_NECK;

		case TV_LITE:		return INVEN_LITE;

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:	return INVEN_BODY;

		case TV_CLOAK:		return INVEN_OUTER;

		case TV_SHIELD:		return INVEN_ARM;

		case TV_CROWN:
		case TV_HELM:		return INVEN_HEAD;

		case TV_GLOVES:		return INVEN_HANDS;

		case TV_BOOTS:		return INVEN_FEET;
	}

	/* No slot available */
	return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 */
const char* mention_use(int i)
{
	const char* p;

	assert((0 <= i) && (i < INVEN_TOTAL) && "precondition");

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

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < p_ptr->inventory[i].weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == INVEN_BOW)
	{
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < p_ptr->inventory[i].weight / 10)
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
const char* describe_use(int i)
{
	const char* p;

	assert((0 <= i) && (i < INVEN_TOTAL) && "precondition");

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

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < p_ptr->inventory[i].weight / 10)
		{
			p = "just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == INVEN_BOW)
	{
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < p_ptr->inventory[i].weight / 10)
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
	if (item_tester_full) return TRUE;

	/* Require an item */
	if (!o_ptr->k_idx) return FALSE;

	/* Hack -- ignore "gold" */
	if (o_ptr->obj_id.tval == TV_GOLD) return FALSE;

	/* Check the tval */
	if (item_tester_tval && (item_tester_tval != o_ptr->obj_id.tval))
		return FALSE;

	/* Check the hook */
	if (item_tester_hook && !(*item_tester_hook)(o_ptr)) return FALSE;

	/* Assume okay */
	return TRUE;
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
int scan_floor(int *items, int size, coord g, int mode)
{
	int this_o_idx, next_o_idx;
	int num = 0;

	/* Sanity */
	if (!in_bounds(g.y, g.x)) return (0);

	/* Scan all objects in the grid */
	for (this_o_idx = cave_o_idx[g.y][g.x]; this_o_idx; this_o_idx = next_o_idx)
	{
		const object_type* const o_ptr = &o_list[this_o_idx];	/* Get the object */

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Verify item tester */
		if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

		/* Marked items only */
		if ((mode & 0x02) && !o_ptr->marked) continue;

		items[num++] = this_o_idx;	/* Accept this item */
		if (num >= size) break;		/* Enforce size limit */
	}

	/* Result */
	return (num);
}



/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	register int i, n;
	byte attr;
	char tmp_val[80];
	char o_name[80];

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Display the pack */
	for (i = 0; i < p_ptr->inven_cnt; ++i)
	{
		/* Examine the item */
		const object_type* const o_ptr = &p_ptr->inventory[i];

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
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->obj_id.tval % N_ELEMENTS(tval_to_attr)];

		/* Display the entry itself */
		Term_putstr(3, i, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3+n, i, 255);

		/* Display the weight if needed */
		if (OPTION(show_weights) && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			Term_putstr(71, i, -1, TERM_WHITE, tmp_val);
		}
	}

	/* Erase the rest of the window */
	for (i = p_ptr->inven_cnt; i < Term->hgt; i++)
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
	byte attr;

	char tmp_val[80];

	char o_name[80];


	/* Display the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		/* Examine the item */
		const object_type* const o_ptr = &p_ptr->inventory[i];

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
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->obj_id.tval % N_ELEMENTS(tval_to_attr)];

		/* Display the entry itself */
		Term_putstr(3, i - INVEN_WIELD, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3+n, i - INVEN_WIELD, 255);

		/* Display the slot description (if needed) */
		if (OPTION(show_labels))
		{
			Term_putstr(61, i - INVEN_WIELD, -1, TERM_WHITE, "<--");
			Term_putstr(65, i - INVEN_WIELD, -1, TERM_WHITE, mention_use(i));
		}

		/* Display the weight (if needed) */
		if (OPTION(show_weights) && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = (OPTION(show_labels) ? 52 : 71);
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
	int i, j, k, l;
	int col;
	int len = 79 - 50;	/* Default length */
	int lim = 79 - 3;	/* Maximum space allowed for descriptions */

	char o_name[80];

	char tmp_val[80];

	int out_index[24];
	byte out_color[24];
	char out_desc[24][80];

	assert(0 <= p_ptr->inven_cnt && INVEN_PACK >= p_ptr->inven_cnt && "precondition");
	assert(p_ptr->inven_cnt_is_strict_UB_of_nonzero_k_idx() && "precondition");

	/* Require space for weight (if needed) */
	if (OPTION(show_weights)) lim -= 9;


	/* Display the inventory */
	for (k = 0, i = 0; i < p_ptr->inven_cnt; i++)
	{
		const object_type* const o_ptr = &p_ptr->inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->obj_id.tval % N_ELEMENTS(tval_to_attr)];

		/* Save the object description */
		my_strcpy(out_desc[k], o_name, sizeof(out_desc[0]));

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Be sure to account for the weight */
		if (OPTION(show_weights)) l += 9;

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
		/* Get the item */
		const object_type* const o_ptr = &p_ptr->inventory[out_index[j]];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(out_index[j]));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j + 1, col);

		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

		/* Display the weight if needed */
		if (OPTION(show_weights))
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j + 1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);
}


/*
 * Display the equipment.
 */
void show_equip(void)
{
	int i, j, k, l;
	int col, len, lim;

	char tmp_val[80];

	char o_name[80];

	int out_index[24];
	byte out_color[24];
	char out_desc[24][80];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for labels (if needed) */
	if (OPTION(show_labels)) lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (OPTION(show_weights)) lim -= 9;

	/* Scan the equipment list */
	for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		const object_type* const o_ptr = &p_ptr->inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Truncate the description */
		o_name[lim] = 0;

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->obj_id.tval % N_ELEMENTS(tval_to_attr)];

		/* Save the description */
		my_strcpy(out_desc[k], o_name, sizeof(out_desc[0]));

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (OPTION(show_labels)) l += (14 + 2);

		/* Increase length for weight (if needed) */
		if (OPTION(show_weights)) l += 9;

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
		/* Get the item */
		const object_type* const o_ptr = &p_ptr->inventory[out_index[j]];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(out_index[j]));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

		/* Use labels */
		if (OPTION(show_labels))
		{
			/* Mention the use */
			strnfmt(tmp_val, sizeof(tmp_val), "%-14s: ", mention_use(out_index[j]));
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
		if (OPTION(show_weights))
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j+1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);
}


/*
 * Display a list of the items on the floor at the given location.
 */
void show_floor(const int *floor_list, int floor_num)
{
	int i, j, k, l;
	int col;
	int len = 79 - 50;	/* Default length */
	int lim = 79 - 3;	/* Maximum space allowed for descriptions */

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[80];

	int out_index[MAX_FLOOR_STACK];
	byte out_color[MAX_FLOOR_STACK];
	char out_desc[MAX_FLOOR_STACK][80];

	/* Require space for weight (if needed) */
	if (OPTION(show_weights)) lim -= 9;

	/* Display the inventory */
	for (k = 0, i = 0; i < floor_num; i++)
	{
		o_ptr = &o_list[floor_list[i]];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->obj_id.tval % N_ELEMENTS(tval_to_attr)];

		/* Save the object description */
		my_strcpy(out_desc[k], o_name, sizeof(out_desc[0]));

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Be sure to account for the weight */
		if (OPTION(show_weights)) l += 9;

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

		/* Display the weight if needed */
		if (OPTION(show_weights))
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j + 1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);
}


/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify_item(const char* const prompt, int item)
{
	char o_name[80];
	char out_val[160];
	object_type *o_ptr = get_o_ptr_from_inventory_or_floor(item);	/* Get the object */

	/* Describe */
	object_desc(o_name, sizeof(o_name), o_ptr, TRUE, ODESC_FULL);

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
	char test_inscrip[3] = {'!','*','\x00'};

	object_type *o_ptr = get_o_ptr_from_inventory_or_floor(item);	/* Get the object */

	/* general check */
	if (check_for_inscrip(o_ptr,test_inscrip))
	{
		/* Verify the choice */
		if (!verify_item("Really try", item)) return (FALSE);
	};
	/* command-specific check */
	test_inscrip[1] = p_ptr->command_cmd;
	if (check_for_inscrip(o_ptr,test_inscrip))
	{
		/* Verify the choice */
		if (!verify_item("Really try", item)) return (FALSE);
	};

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
	object_type *o_ptr = get_o_ptr_from_inventory_or_floor(item);	/* Get the object */

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
	char test_inscrip[4] = {'@',tag,'\x00','\x00'};

	/* Check every object */
	for (i = 0; i < INVEN_TOTAL; ++i)
	{
		const object_type* const o_ptr = &p_ptr->inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* check for @tag */
		if (check_for_inscrip(o_ptr,test_inscrip))
		{
			/* Save the actual inventory ID */
			*cp = i;

			/* Success */
			return (TRUE);
		};

		test_inscrip[1] = p_ptr->command_cmd;
		test_inscrip[2] = tag;

		/* check for @[command]tag */
		if (check_for_inscrip(o_ptr,test_inscrip))
		{
			/* Save the actual inventory ID */
			*cp = i;

			/* Success */
			return (TRUE);
		};
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
bool get_item(int *cp, const char* pmt, const char* str, int mode)
{
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

			return TRUE;	/* Success */
		}
		else
		{
			/* Invalid repeat - reset it */
			repeat_clear();
		}
	}

	{	/* C-ish blocking brace */
	char tmp_val[160];
	char out_val[160];

	int floor_list[MAX_FLOOR_STACK];

	/* scan all objects in the grid */
	int floor_num = scan_floor(floor_list, MAX_FLOOR_STACK, p_ptr->loc, 0x00);

	int i, k;

	char which;

	bool done = FALSE;	/* done? */
	bool item = FALSE;	/* item selected? */
	bool oops = FALSE;	/* nothing to select? */

	const bool use_inven = (mode & (USE_INVEN));
	const bool use_equip = (mode & (USE_EQUIP));
	const bool use_floor = (mode & (USE_FLOOR));

	bool toggle = FALSE;

	/* default to full inventory if allowed, disabled otherwise */
	int i1 = 0;
	int i2 = (use_inven) ? INVEN_PACK - 1 : -1;

	/* default to full equipment if allowed, disabled otherwise */
	int e1 = INVEN_WIELD;
	int e2 = (use_equip) ? INVEN_TOTAL - 1 : -1;

	/* default to full floor if allowed, disabled otherwise */
	int f1 = 0;
	int f2 = (use_floor) ? floor_num - 1 : -1;

	/* Paranoia XXX XXX XXX */
	message_flush();

	/* Restrict inventory indexes */
	while ((i1 <= i2) && (!get_item_okay(i1))) ++i1;
	while ((i1 <= i2) && (!get_item_okay(i2))) --i2;

	/* Restrict equipment indexes */
	while ((e1 <= e2) && (!get_item_okay(e1))) ++e1;
	while ((e1 <= e2) && (!get_item_okay(e2))) --e2;

	/* Restrict floor indexes */
	while ((f1 <= f2) && (!get_item_okay(0 - floor_list[f1]))) ++f1;
	while ((f1 <= f2) && (!get_item_okay(0 - floor_list[f2]))) --f2;

	{	/* C-ish blocking brace : allow_ */
	const bool allow_inven = (i1 <= i2);	/* accept inventory if any items */
	const bool allow_equip = (e1 <= e2);	/* accept equipment if any items */
	const bool allow_floor = (f1 <= f2);	/* accept floor if any items */

	/* Require at least one legal choice */
	if (!allow_inven && !allow_equip && !allow_floor)
	{
		p_ptr->command_see = FALSE;	/* Cancel p_ptr->command_see */
		oops = TRUE;
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
		else if (OPTION(easy_floor))
		{
			p_ptr->command_wrk = (USE_FLOOR);
		}

		/* Hack -- Use (empty) inventory */
		else
		{
			p_ptr->command_wrk = (USE_INVEN);
		}
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
		/* Show choices */
		if (OPTION(show_choices))
		{
			int ni = op_ptr->count_flagged_windows(PW_INVEN);
			int ne = op_ptr->count_flagged_windows(PW_EQUIP);

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
			p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

			/* Redraw windows */
			redraw_stuff();
		}

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
			if (!p_ptr->command_see) my_strcat(out_val, " * to see,", sizeof(out_val));

			/* Indicate legality of "toggle" */
			if (use_equip) my_strcat(out_val, " / for Equip,", sizeof(out_val));

			/* Indicate legality of the "floor" */
			if (allow_floor) my_strcat(out_val, " - for floor,", sizeof(out_val));
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
			if (!p_ptr->command_see) my_strcat(out_val, " * to see,", sizeof(out_val));

			/* Indicate legality of "toggle" */
			if (use_inven) my_strcat(out_val, " / for Inven,", sizeof(out_val));

			/* Indicate legality of the "floor" */
			if (allow_floor) my_strcat(out_val, " - for floor,", sizeof(out_val));
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
			if (!p_ptr->command_see) my_strcat(out_val, " * to see,", sizeof(out_val));

			/* Append */
			if (use_inven) my_strcat(out_val, " / for Inven,", sizeof(out_val));

			/* Append */
			else if (use_equip) my_strcat(out_val, " / for Equip,", sizeof(out_val));
		}

		/* Finish the prompt */
		my_strcat(out_val, " ESC", sizeof(out_val));

		/* Build the prompt */
		strnfmt(tmp_val, sizeof(tmp_val), "(%s) %s", out_val, pmt);

		/* Show the prompt */
		prt(tmp_val, 0, 0);


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

				if (OPTION(easy_floor))
				{
					/* There is only one item */
					if (floor_num == 1)
					{
						/* Hack -- Auto-Select */
						if ((p_ptr->command_wrk == (USE_FLOOR)) ||
						    (!OPTION(floor_query_flag)))
						{
							/* Special index */
							k = 0 - floor_list[0];

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
					if (OPTION(floor_query_flag) && !verify_item("Try", k)) continue;

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
			{	/* Note verify */
				bool verify = isupper((unsigned char)which);

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

	}	/* end C-ish blocking brace : allow_ */

	/* Fix the screen if necessary */
	if (p_ptr->command_see)
	{
		screen_load();				/* Load screen */
		p_ptr->command_see = FALSE;	/* Hack -- Cancel "display" */
	}

	item_tester_tval = 0;		/* Forget the item_tester_tval restriction */
	item_tester_hook = NULL;	/* Forget the item_tester_hook restriction */


	/* Clean up */
	if (OPTION(show_choices))
	{
		if (toggle) toggle_inven_equip();		/* Toggle again if needed */
		p_ptr->redraw |= (PR_INVEN | PR_EQUIP);	/* Update */
		redraw_stuff();							/* Window stuff */
	}

	prt("", 0, 0);						/* Clear the prompt line */
	if (oops && str) msg_print(str);	/* Warning if needed */
	if (item) repeat_push(*cp);			/* Save item if available */
	return item;						/* Result */
	}	/* end C-ish blocking brace */
}
