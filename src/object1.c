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
#define MAX_ROCKS      45       /* Used with rings (min 38) */
#define MAX_AMULETS    25       /* Used with amulets (min 13) */
#define MAX_WOODS      51	/* Used with staffs (min 33) */
#define MAX_METALS     51       /* Used with wands/rods (min 29/28) */
#define MAX_COLORS     84       /* Used with potions (min 60) */
#define MAX_SHROOM     30       /* Used with mushrooms (min 20) */
#define MAX_TITLES     50       /* Used with scrolls (min 48) */
#define MAX_SYLLABLES 158       /* Used with scrolls (see below) */


/*
 * Rings (adjectives and colors).
 */

static cptr ring_adj[MAX_ROCKS] =
{
	"Alexandrite", "Amethyst", "Aquamarine", "Azurite", "Beryl",
	"Bloodstone", "Calcite", "Carnelian", "Corundum", "Diamond",
	"Emerald", "Fluorite", "Garnet", "Granite", "Jade",
	"Jasper", "Lapis Lazuli", "Malachite", "Marble", "Moonstone",
	"Onyx", "Opal", "Pearl", "Quartz", "Quartzite",
	"Rhodonite", "Ruby", "Sapphire", "Tiger Eye", "Topaz",
	"Turquoise", "Zircon", "Platinum", "Bronze", "Gold",
	"Obsidian", "Silver", "Tortoise Shell", "Mithril", "Jet",
	"Engagement", "Adamantite", 

	/* from Sangband */
	"Agate", "Hematite", "Steel"
};

static byte ring_col[MAX_ROCKS] =
{
	TERM_GREEN, TERM_VIOLET, TERM_L_BLUE, TERM_L_BLUE, TERM_L_GREEN,
	TERM_RED, TERM_WHITE, TERM_RED, TERM_SLATE, TERM_WHITE,
	TERM_GREEN, TERM_L_GREEN, TERM_RED, TERM_L_WHITE, TERM_L_GREEN,
	TERM_UMBER, TERM_BLUE, TERM_GREEN, TERM_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_L_WHITE, TERM_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_RED, TERM_BLUE, TERM_YELLOW, TERM_YELLOW,
	TERM_L_BLUE, TERM_L_UMBER, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_DARK, TERM_L_WHITE, TERM_UMBER, TERM_L_BLUE, TERM_L_DARK,
	TERM_YELLOW, TERM_L_GREEN, 

	TERM_VIOLET, TERM_L_DARK, TERM_WHITE
};


/*
 * Amulets (adjectives and colors).
 */

static cptr amulet_adj[MAX_AMULETS] =
{
	"Amber", "Driftwood", "Coral", "Agate", "Ivory",
	"Obsidian", "Bone", "Brass", "Bronze", "Pewter",
	"Tortoise Shell", "Golden", "Azure", "Crystal", "Silver",
	"Copper",

	/* from Sangband */
	"Bead", "Cochineal", "Enameled", "Faceted", "Horn",
	"Jade", "Jeweled", "Scarab", "Stained Glass"
};

static byte amulet_col[MAX_AMULETS] =
{
	TERM_YELLOW, TERM_L_UMBER, TERM_WHITE, TERM_L_WHITE, TERM_WHITE,
	TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_SLATE,
	TERM_UMBER, TERM_YELLOW, TERM_L_BLUE, TERM_WHITE, TERM_L_WHITE,
	TERM_L_UMBER,

	TERM_L_GREEN, TERM_RED, TERM_BLUE, TERM_L_WHITE, TERM_WHITE,
	TERM_GREEN, TERM_VIOLET, TERM_UMBER, TERM_VIOLET
};


/*
 * Staffs (adjectives and colors), copied from Sangband
 */

static cptr staff_adj[MAX_WOODS] =
{
	"Applewood", "Ash", "Aspen", "Balsa", "Bamboo",
	"Banyan", "Baobab", "Beech", "Birch", "Butternut",
	"Cedar", "Cherry", "Cottonwood", "Cypress", "Dogwood",
	"Elm", "Eucalyptus", "Fir", "Golden", "Hazel",
	"Hawthorn", "Hemlock", "Hickory", "Holly", "Ironwood",
	"Ivory", "Laurel", "Linden", "Locust", "Mahogany",
	"Maple", "Mallorn", "Mistletoe", "Mulberry", "Oak",
	"Olive", "Palmwood", "Poplar", "Pine", "Redwood",
	"Rosewood", "Rowan", "Runed", "Sequoia", "Silver",
	"Spruce", "Sycamore", "Teak", "Walnut", "Willow",
	"Yew"
};

static byte staff_col[MAX_WOODS] =
{
	TERM_L_GREEN, TERM_L_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_SLATE, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_UMBER, TERM_RED, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_YELLOW, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_UMBER, TERM_SLATE, TERM_WHITE, TERM_L_UMBER, TERM_UMBER,
	TERM_L_UMBER, TERM_YELLOW, TERM_GREEN, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_GREEN, TERM_L_UMBER, TERM_UMBER, TERM_L_UMBER, TERM_RED,
	TERM_L_RED, TERM_UMBER, TERM_UMBER, TERM_L_WHITE, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_DARK, TERM_UMBER, TERM_L_UMBER, TERM_SLATE,
	TERM_L_DARK
};


/*
 * Wands (adjectives and colors), copied from Sangband
 */

static cptr wand_adj[MAX_METALS] =
{
	"Adamantium", "Adamantium-Plated", "Aluminum", "Antimony", "Beryllium",
	"Billon", "Shining", "Brass", "Bronze", "Carbonized",
	"Cast Iron", "Chromium", "Cobalt", "Copper", "Copper-Plated",
	"Corundum", "Damascened", "Electrum", "Galvorn", "Tarnished",
	"Gold", "Iridescent", "Iron", "Ivory", "Jeweled",
	"Lead", "Glass", "Magnesium", "Mithril", "Molybdenum",
	"Nickel", "Palladium", "Platinum", "Pewter", "Rhodium",
	"Runed", "Rusty", "Sapphire", "Silver", "Silver-Plated",
	"Steel", "Tin", "Tin-Plated", "Titanium", "Stubby",
	"Tungsten", "Uridium", "Wrought Iron", "Gleaming", "Zinc",
	"Zinc-Plated"
};

static byte wand_col[MAX_METALS] =
{
	TERM_L_GREEN, TERM_L_GREEN, TERM_L_BLUE, TERM_SLATE, TERM_L_BLUE,
	TERM_UMBER, TERM_YELLOW, TERM_L_UMBER, TERM_ORANGE, TERM_L_DARK,
	TERM_L_DARK, TERM_WHITE, TERM_BLUE, TERM_UMBER, TERM_UMBER,
	TERM_RED, TERM_L_BLUE, TERM_L_WHITE, TERM_L_DARK, TERM_GREEN,
	TERM_YELLOW, TERM_VIOLET, TERM_SLATE, TERM_WHITE, TERM_VIOLET,
	TERM_SLATE, TERM_WHITE, TERM_L_WHITE, TERM_L_BLUE, TERM_L_WHITE,
	TERM_SLATE, TERM_L_WHITE, TERM_L_WHITE, TERM_SLATE, TERM_WHITE,
	TERM_L_RED, TERM_RED, TERM_BLUE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_WHITE, TERM_WHITE, TERM_WHITE, TERM_L_WHITE, TERM_YELLOW,
	TERM_WHITE, TERM_L_GREEN, TERM_L_DARK, TERM_L_WHITE, TERM_SLATE,
	TERM_SLATE
};


/*
 * Rods (adjectives and colors).
 *
 * Efficiency -- copied from wand arrays.
 */

static cptr rod_adj[MAX_METALS];

static byte rod_col[MAX_METALS];


/*
 * Mushrooms (adjectives and colors), copied from Sangband
 */

static cptr food_adj[MAX_SHROOM] =
{
	"Blue", "Black", "Black Spotted", "Brown", "Dark Blue",
	"Dark Green", "Dark Red", "Yellow", "Furry", "Fuzzy",
	"Green", "Greasy", "Grey", "Light Blue", "Light Green",
	"Pink", "Purple Blotched", "Red", "Red Spotted", "Slimy",
	"Tan", "Violet", "White", "White Spotted", "Wrinkled",
	"Ecru", "Lemon Speckled", "Orange-Capped", "Luminescent", "Smelly"
};

static byte food_col[MAX_SHROOM] =
{
	TERM_BLUE, TERM_L_DARK, TERM_L_DARK, TERM_UMBER, TERM_BLUE,
	TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_L_WHITE, TERM_WHITE,
	TERM_GREEN, TERM_L_WHITE, TERM_SLATE, TERM_L_BLUE, TERM_L_GREEN,
	TERM_L_RED, TERM_VIOLET, TERM_RED, TERM_L_RED, TERM_SLATE,
	TERM_L_UMBER, TERM_VIOLET, TERM_WHITE, TERM_WHITE, TERM_UMBER,
	TERM_L_GREEN, TERM_YELLOW, TERM_ORANGE, TERM_L_BLUE, TERM_L_DARK
};


/*
 * Color adjectives and colors, for potions, copied from Sangband
 *
 * Hack -- The first four entries (water, apple juice, slime mold juice,
 * and undefined) are hard-coded.
 */

static cptr potion_adj[MAX_COLORS] =
{
	"Clear", "Light Brown", "Icky Green", "Mysterious",
	"Amber", "Ashen", "Auburn", "Azure", "Black",
	"Blue", "Blue Speckled", "Brown", "Brown Speckled", "Bubbling",
	"Carnation", "Chartreuse", "Chocolate-Brown", "Clear Blue", "Clotted Red",
	"Cloudy", "Cobalt", "Copper Speckled", "Crimson", "Cyan",
	"Dark Blue", "Dark Green", "Dark Red", "Dirty", "Frothing",
	"Gloopy Green", "Gold", "Gold Speckled", "Golden Brown", "Green",
	"Greenish", "Grey", "Grey Speckled", "Hazy", "Indigo",
	"Ivory White", "Lavender", "Light Blue", "Light Green", "Limpid",
	"Lincoln Green", "Magenta", "Maroon", "Metallic Blue", "Metallic Red",
	"Metallic Purple", "Misty", "Moldy", "Muddy", "Myrtle Green",
	"Oily Yellow", "Orange", "Orange Speckled", "Peach", "Pink",
	"Pearl-Grey", "Puce", "Pungent", "Purple", "Purple Speckled",
	"Red", "Red Speckled", "Tyrian Purple", "Rosy", "Sea-Blue",
	"Shimmering", "Shining", "Sickly Green", "Silver Speckled", "Smoky",
	"Tangerine", "Tawny", "Turgid", "Umber", "Violet",
	"Vermilion", "Viscous Pink", "White", "Yellow", "Yellow Dappled"
};

static byte potion_col[MAX_COLORS] =
{
	TERM_WHITE, TERM_L_UMBER, TERM_GREEN, TERM_WHITE,
	TERM_ORANGE, TERM_WHITE, TERM_UMBER, TERM_L_BLUE, TERM_L_DARK,
	TERM_BLUE, TERM_BLUE, TERM_UMBER, TERM_UMBER, TERM_L_WHITE,
	TERM_L_RED, TERM_L_GREEN, TERM_UMBER, TERM_L_BLUE, TERM_RED,
	TERM_SLATE, TERM_L_BLUE, TERM_UMBER, TERM_L_RED, TERM_L_BLUE,
	TERM_BLUE, TERM_GREEN, TERM_RED, TERM_L_UMBER, TERM_SLATE,
	TERM_GREEN, TERM_YELLOW, TERM_YELLOW, TERM_L_UMBER, TERM_GREEN,
	TERM_L_GREEN, TERM_SLATE, TERM_SLATE, TERM_WHITE, TERM_BLUE,
	TERM_L_WHITE, TERM_VIOLET, TERM_L_BLUE, TERM_L_GREEN, TERM_WHITE,
	TERM_GREEN, TERM_RED, TERM_RED, TERM_BLUE, TERM_RED,
	TERM_VIOLET, TERM_L_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_GREEN,
	TERM_YELLOW, TERM_ORANGE, TERM_ORANGE, TERM_L_RED, TERM_L_RED,
	TERM_WHITE, TERM_UMBER, TERM_L_GREEN, TERM_VIOLET, TERM_VIOLET,
	TERM_RED, TERM_RED, TERM_VIOLET, TERM_L_RED, TERM_BLUE,
	TERM_YELLOW, TERM_WHITE, TERM_L_GREEN, TERM_L_WHITE, TERM_L_DARK,
	TERM_ORANGE, TERM_SLATE, TERM_L_UMBER, TERM_UMBER, TERM_VIOLET,
	TERM_L_RED, TERM_L_RED, TERM_WHITE, TERM_YELLOW, TERM_YELLOW
};


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

static byte scroll_col[MAX_TITLES];






/*
 * Certain items have a flavor.
 *
 * This function is used only by "flavor_init()"
 */
static bool object_flavor(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];

	/* Analyze the item */
	switch (k_ptr->tval)
	{
		case TV_AMULET:
		{
			return (0x80 + amulet_col[k_ptr->sval]);
		}

		case TV_RING:
		{
			return (0x90 + ring_col[k_ptr->sval]);
		}

		case TV_STAFF:
		{
			return (0xA0 + staff_col[k_ptr->sval]);
		}

		case TV_WAND:
		{
			return (0xB0 + wand_col[k_ptr->sval]);
		}

		case TV_ROD:
		{
			return (0xC0 + rod_col[k_ptr->sval]);
		}

		case TV_SCROLL:
		{
			return (0xD0 + scroll_col[k_ptr->sval]);
		}

		case TV_POTION:
		{
			return (0xE0 + potion_col[k_ptr->sval]);
		}

		case TV_FOOD:
		{
			if (k_ptr->sval < SV_FOOD_MIN_FOOD)
			{
				return (0xF0 + food_col[k_ptr->sval]);
			}

			break;
		}
	}

	/* No flavor */
	return (0);
}


/*
 * Certain items, if aware, are known instantly.
 *
 * This function is used only by "flavor_init()".
 *
 * Add "EASY_KNOW" flag to "k_info.txt" file.  XXX XXX XXX
 */
static bool object_easy_know(int i)
{
	object_kind *k_ptr = &k_info[i];

	/* Analyze the "tval" */
	switch (k_ptr->tval)
	{
		/* Simple items */
		case TV_FLASK:
		case TV_JUNK:
		case TV_BOTTLE:
		case TV_SKELETON:
		case TV_SPIKE:
		{
			return (TRUE);
		}

		/* All Food, Potions, Scrolls, Rods */
		case TV_FOOD:
		case TV_POTION:
		case TV_SCROLL:
		case TV_ROD:
		{
			return (TRUE);
		}

		/* Some Rings, Amulets, Lites */
		case TV_RING:
		case TV_AMULET:
		case TV_LITE:
		{
			if (k_ptr->flags3 & (TR3_EASY_KNOW)) return (TRUE);
			return (FALSE);
		}
	}

	/* Nope */
	return (FALSE);
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

	byte temp_col;

	cptr temp_adj;


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistant flavors */
	Rand_value = seed_flavor;


	/* Efficiency -- Rods/Wands share initial array */
	for (i = 0; i < MAX_METALS; i++)
	{
		rod_adj[i] = wand_adj[i];
		rod_col[i] = wand_col[i];
	}


	/* Rings have "ring colors" */
	for (i = 0; i < MAX_ROCKS; i++)
	{
		j = rand_int(MAX_ROCKS);
		temp_adj = ring_adj[i];
		ring_adj[i] = ring_adj[j];
		ring_adj[j] = temp_adj;
		temp_col = ring_col[i];
		ring_col[i] = ring_col[j];
		ring_col[j] = temp_col;
	}

	/* Amulets have "amulet colors" */
	for (i = 0; i < MAX_AMULETS; i++)
	{
		j = rand_int(MAX_AMULETS);
		temp_adj = amulet_adj[i];
		amulet_adj[i] = amulet_adj[j];
		amulet_adj[j] = temp_adj;
		temp_col = amulet_col[i];
		amulet_col[i] = amulet_col[j];
		amulet_col[j] = temp_col;
	}

	/* Staffs */
	for (i = 0; i < MAX_WOODS; i++)
	{
		j = rand_int(MAX_WOODS);
		temp_adj = staff_adj[i];
		staff_adj[i] = staff_adj[j];
		staff_adj[j] = temp_adj;
		temp_col = staff_col[i];
		staff_col[i] = staff_col[j];
		staff_col[j] = temp_col;
	}

	/* Wands */
	for (i = 0; i < MAX_METALS; i++)
	{
		j = rand_int(MAX_METALS);
		temp_adj = wand_adj[i];
		wand_adj[i] = wand_adj[j];
		wand_adj[j] = temp_adj;
		temp_col = wand_col[i];
		wand_col[i] = wand_col[j];
		wand_col[j] = temp_col;
	}

	/* Rods */
	for (i = 0; i < MAX_METALS; i++)
	{
		j = rand_int(MAX_METALS);
		temp_adj = rod_adj[i];
		rod_adj[i] = rod_adj[j];
		rod_adj[j] = temp_adj;
		temp_col = rod_col[i];
		rod_col[i] = rod_col[j];
		rod_col[j] = temp_col;
	}

	/* Foods (Mushrooms) */
	for (i = 0; i < MAX_SHROOM; i++)
	{
		j = rand_int(MAX_SHROOM);
		temp_adj = food_adj[i];
		food_adj[i] = food_adj[j];
		food_adj[j] = temp_adj;
		temp_col = food_col[i];
		food_col[i] = food_col[j];
		food_col[j] = temp_col;
	}

	/* Potions */
	for (i = 4; i < MAX_COLORS; i++)
	{
		j = rand_int(MAX_COLORS - 4) + 4;
		temp_adj = potion_adj[i];
		potion_adj[i] = potion_adj[j];
		potion_adj[j] = temp_adj;
		temp_col = potion_col[i];
		potion_col[i] = potion_col[j];
		potion_col[j] = temp_col;
	}

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
					strcat(tmp, syllables[rand_int(MAX_SYLLABLES)]);
				}

				/* Stop before getting too long */
				if (strlen(buf) + 1 + strlen(tmp) > 15) break;

				/* Add a space */
				strcat(buf, " ");

				/* Add the word */
				strcat(buf, tmp);
			}

			/* Save the title */
			strcpy(scroll_adj[i], buf+1);

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

		/* All scrolls are white */
		scroll_col[i] = TERM_WHITE;
	}


	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Analyze every object */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		/* Extract "flavor" (if any) */
		k_ptr->flavor = object_flavor(i);

		/* No flavor yields aware */
		if (!k_ptr->flavor) k_ptr->aware = TRUE;

		/* Check for "easily known" */
		k_ptr->easy_know = object_easy_know(i);
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
 * Modes of object_flags_aux()
 */
#define OBJECT_FLAGS_FULL   1 /* Full info */
#define OBJECT_FLAGS_KNOWN  2 /* Only flags known to the player */
#define OBJECT_FLAGS_RANDOM 3 /* Only known random flags */


/*
 * Obtain the "flags" for an item
 */
static void object_flags_aux(int mode, const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_kind *k_ptr;

	if (mode != OBJECT_FLAGS_FULL)
	{
		/* Clear */
		(*f1) = (*f2) = (*f3) = 0L;

		/* Must be identified */
		if (!object_known_p(o_ptr)) return;
	}

	if (mode != OBJECT_FLAGS_RANDOM)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;

		if (mode == OBJECT_FLAGS_FULL)
		{
			/* Artifact */
			if (o_ptr->name1)
			{
				artifact_type *a_ptr = &a_info[o_ptr->name1];

				(*f1) = a_ptr->flags1;
				(*f2) = a_ptr->flags2;
				(*f3) = a_ptr->flags3;
			}
		}

		/* Ego-item */
		if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			(*f1) |= e_ptr->flags1;
			(*f2) |= e_ptr->flags2;
			(*f3) |= e_ptr->flags3;
		}

		if (mode == OBJECT_FLAGS_KNOWN)
		{
			/* Obvious artifact flags */
			if (o_ptr->name1)
			{
				artifact_type *a_ptr = &a_info[o_ptr->name1];

				/* Obvious flags (pval) */
				(*f1) = (a_ptr->flags1 & (TR1_PVAL_MASK));

				(*f3) = (a_ptr->flags3 & (TR3_IGNORE_MASK));
			}
		}
	}

	if (mode != OBJECT_FLAGS_FULL)
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

		/* Need full knowledge or spoilers */
		if (!spoil && !(o_ptr->ident & IDENT_MENTAL)) return;

		/* Artifact */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			(*f1) = a_ptr->flags1;
			(*f2) = a_ptr->flags2;
			(*f3) = a_ptr->flags3;

			if (mode == OBJECT_FLAGS_RANDOM)
			{
				/* Hack - remove 'ignore' flags */
				(*f3) &= ~(TR3_IGNORE_MASK);
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
			(*f2) |= (OBJECT_XTRA_BASE_SUSTAIN << o_ptr->xtra2);
			break;
		}

		/* case OBJECT_XTRA_TYPE_RESIST: */

		case OBJECT_XTRA_TYPE_POWER:
		{
			/* OBJECT_XTRA_WHAT_POWER == 3 */
			(*f3) |= (OBJECT_XTRA_BASE_POWER << o_ptr->xtra2);
			break;
		}
	}
}




/*
 * Obtain the "flags" for an item
 */
void object_flags(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_FLAGS_FULL, o_ptr, f1, f2, f3);
}



/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3)
{
	object_flags_aux(OBJECT_FLAGS_KNOWN, o_ptr, f1, f2, f3);
}


/*
 * Obtain the resistances from an item 
 */
byte object_resist(const object_type *o_ptr, int res_type)
{
     return (o_ptr->res[res_type]);
}

/*
 * Obtain the resistances from an item 
 */
byte object_resist_known(const object_type *o_ptr, int res_type)
{
     if (object_known_p(o_ptr)) 
	  return (o_ptr->res[res_type]);
     
     return (0);
}


/*
 * Efficient version of '(T) += strfmt((T), "%c", (C))'
 */
#define object_desc_chr_macro(T,C) do { \
 \
	/* Copy the char */ \
	*(T)++ = (C); \
 \
} while (0)



/*
 * Efficient version of '(T) += strfmt((T), "%s", (S))'
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
 * Efficient version of '(T) += strfmt((T), "%u", (N))'
 */
#define object_desc_num_macro(T,N) do { \
 \
	uint n = (N); \
 \
	uint p; \
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
 * Efficient version of '(T) += strfmt((T), "%+d", (I))'
 */
#define object_desc_int_macro(T,I) do { \
 \
	sint i = (I); \
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
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
int value_check_aux(const object_type *o_ptr)
{
	/* Artifacts */
	if (artifact_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_TERRIBLE);

		/* Normal */
		return (INSCRIP_SPECIAL);
	}

	/* Ego-Items */
	if (ego_item_p(o_ptr))
	{
		/* Cursed/Broken */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) return (INSCRIP_WORTHLESS);

		/* Normal */
		return (INSCRIP_EXCELLENT);
	}

	/* Hack - cursed weapons with only damage affected */
	if (cursed_p(o_ptr) && 
	    o_ptr->tval >= TV_SHOT && o_ptr->tval <= TV_LAST_WPN &&
	    o_ptr->to_a == 0 && o_ptr->to_h == 0) return (INSCRIP_AVERAGE);

	/* Cursed items */
	if (cursed_p(o_ptr)) return (INSCRIP_CURSED);

	/* Broken items */
	if (broken_p(o_ptr)) return (INSCRIP_BROKEN);

	/* Good bonus */
	if (o_ptr->to_a > 0) return (INSCRIP_GOOD);
	if (o_ptr->to_h > 0) return (INSCRIP_GOOD);

	/* Bad bonus */
	if (o_ptr->to_a < 0) return (INSCRIP_CURSED);

	/* Bad weapons */
	if (o_ptr->tval >= TV_SHOT && o_ptr->tval <= TV_LAST_WPN &&
	    o_ptr->to_h < 0) return (INSCRIP_CURSED);

	/* Default to "average" */
	return (INSCRIP_AVERAGE);
}

/*
 * Creates a description of the item "o_ptr", and stores it in "out_val".
 *
 * One can choose the "verbosity" of the description, including whether
 * or not the "number" of items should be described, and how much detail
 * should be used when describing the item.
 *
 * The given "buf" must be 80 chars long to hold the longest possible
 * description, which can get pretty long, including incriptions, such as:
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
 * efficient, portable, versions of some common "strfmt()" commands (without
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
void object_desc(char *buf, const object_type *o_ptr, int pref, int mode)
{
	cptr basenm;
	cptr modstr;

	int power;

	bool aware;
	bool known;

	bool flavor;

	bool append_name;

	bool show_weapon;
	bool show_weapon_extended = FALSE;
	bool show_armour;

	char *b;

	char *t;

	cptr s;

	cptr u;
	cptr v;

	char p1 = '(', p2 = ')';
	char b1 = '[', b2 = ']';
	char c1 = '{', c2 = '}';

	char tmp_buf[128];

	u32b f1, f2, f3;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];


	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* See if the object is "aware" */
	aware = (object_aware_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "known" */
	known = (object_known_p(o_ptr) ? TRUE : FALSE);

	/* See if the object is "flavored" */
	flavor = (k_ptr->flavor ? TRUE : FALSE);

	/* Gambling den hack, mode of -1, hides flavours and otherwise
	 * acts as mode 0 */
	if (mode == -1) flavor = FALSE;

	/* Allow flavors to be hidden when aware */
	if (aware && !show_flavors) flavor = FALSE;

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
		case TV_BOW:
		{
			show_weapon = TRUE;
			break;
		}
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_HAFTED:
	        case TV_CLAW:
		case TV_POLEARM:
		case TV_SWORD:
	        case TV_AXE:
		case TV_DIGGING:
		{
			show_weapon_extended = TRUE;
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
			modstr = amulet_adj[o_ptr->sval];
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
			modstr = ring_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Ring~" : "& Ring~");

			/* Mega-Hack -- The One Ring */
			if (!aware && (o_ptr->sval == SV_RING_POWER))
			{
				modstr = "Plain Gold";
			}

			break;
		}

		/* Staffs */
		case TV_STAFF:
		{
			/* Color the object */
			modstr = staff_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Staff~" : "& Staff~");

			break;
		}

		/* Wands */
		case TV_WAND:
		{
			/* Color the object */
			modstr = wand_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Wand~" : "& Wand~");

			break;
		}

		/* Rods */
		case TV_ROD:
		{
			/* Color the object */
			modstr = rod_adj[o_ptr->sval];
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
			modstr = potion_adj[o_ptr->sval];
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
			modstr = food_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Mushroom~" : "& Mushroom~");

			break;
		}

		/* Hack -- Gold/Gems */
		case TV_GOLD:
		{
			strcpy(buf, basenm);
			return;
		}

		/* Hack -- Default -- Used in the "inventory" routine */
		default:
		{
			strcpy(buf, "(nothing)");
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
		else if ((*s == '#') ? is_a_vowel(modstr[0]) : is_a_vowel(*s))
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


	/* Paranoia XXX XXX XXX */
	/* ASSERT(*s != '~'); */

	/* Copy the string */
	for (; *s; s++)
	{
		/* Pluralizer */
		if (*s == '~')
		{
			/* Add a plural if needed */
			if (o_ptr->number != 1)
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

	if (mode < 0) goto object_desc_done;

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
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			object_desc_chr_macro(t, ' ');
			object_desc_str_macro(t, (a_name + a_ptr->name));
		}

		/* Grab any ego-item name */
		else if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			object_desc_chr_macro(t, ' ');

			/* Show name of single skill */
			if (o_ptr->xtra1 >= OBJECT_XTRA_TYPE_SKILL_FIRST &&
			    o_ptr->xtra1 < OBJECT_XTRA_TYPE_SKILL_MULTI)
			{
			     object_desc_chr_macro(t, '(');
			     object_desc_str_macro(t, skill_info[o_ptr->xtra2].name);
			     object_desc_chr_macro(t, ')');
			}
			else /* Normal name */
			{
			     object_desc_str_macro(t, (e_name + e_ptr->name));
			}
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
			/* Fall through */
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
	        case TV_AXE:
	        case TV_CLAW:
		case TV_DIGGING:
		{
		     if (known && show_weapon_extended)
		     {
			  /* Beginning '(' */
			  if (o_ptr->to_h || o_ptr->to_d ||
			      o_ptr->dd || o_ptr->ds)
			  {
			       object_desc_chr_macro(t, ' ');
			       object_desc_chr_macro(t, p1);
			  }

			  /* Modifier to-hit */
			  if (o_ptr->to_h)
			  {
			       object_desc_int_macro(t, o_ptr->to_h);
			       object_desc_chr_macro(t, ',');
			       object_desc_chr_macro(t, ' ');
			  }
		     
			  /* Base damage */
			  if (o_ptr->dd && o_ptr->ds)
			  {
			       object_desc_num_macro(t, o_ptr->dd);
			       object_desc_chr_macro(t, 'd');
			       object_desc_num_macro(t, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));
			  }
			  		     
			  /* Closing ')' */
			  if (o_ptr->to_h || o_ptr->to_d ||
			      o_ptr->dd || o_ptr->ds)
			  {
			       object_desc_chr_macro(t, p2);		    
			  }
		     }

		     /* All done */
		     break;
		}

		/* Bows */
		case TV_BOW:
		{
		     if (known)
		     {
			/* Hack -- Extract the "base power" */
			power = (o_ptr->sval % 10);

			/* Show extra might */
			if (f1 & (TR1_MIGHT)) power += o_ptr->pval;

			/* Append a "power" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_chr_macro(t, 'x');
			object_desc_num_macro(t, power);
			object_desc_chr_macro(t, p2);
		     }

		     /* All done */
		     break;
		}
	}


	/* Add the weapon bonuses unless already shown */
	if (known && !show_weapon_extended)
	{
		/* Show the tohit/todam on request */
		if (show_weapon)
		{
		        if (o_ptr->to_h || o_ptr->to_d)
			{
			     object_desc_chr_macro(t, ' ');
			     object_desc_chr_macro(t, p1);
			}
			if (o_ptr->to_h)
			{
			     object_desc_int_macro(t, o_ptr->to_h);
			     if (o_ptr->to_d)
				  object_desc_chr_macro(t, ',');
			}
			if (o_ptr->to_d)
			     object_desc_int_macro(t, o_ptr->to_d);
		        if (o_ptr->to_h || o_ptr->to_d)
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
		if (show_armour)
		{
		        if (o_ptr->ac || o_ptr->to_a)
			{
			     object_desc_chr_macro(t, ' ');
			     object_desc_chr_macro(t, b1);
			}
			if (o_ptr->ac)
			{
			     /* AC bonus from shield skill, character's or shield's */
			     if (o_ptr->tval == TV_SHIELD && (p_ptr->skill[PALA_HOLY_SHIELD] ||
							      o_ptr->xtra2 == PALA_HOLY_SHIELD))
			     {
			          int ac_bonus = 0, ac;
				  ac_bonus += p_ptr->skill[PALA_HOLY_SHIELD];
				  if (o_ptr->xtra2 == PALA_HOLY_SHIELD)
				    ac_bonus += o_ptr->pval;
				  ac = o_ptr->ac + (o_ptr->ac * ac_bonus / 5);
				  object_desc_num_macro(t, ac);
			     }
			     else
				  object_desc_num_macro(t, o_ptr->ac);
			     if (o_ptr->to_a)
				  object_desc_chr_macro(t, ',');
			}
			if (o_ptr->to_a)
			     object_desc_int_macro(t, o_ptr->to_a);
		        if (o_ptr->ac || o_ptr->to_a)
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

	/* No more details wanted */
	if (mode < 2) goto object_desc_done;


	/* Hack -- Wands and Staffs have charges */
	if (known &&
	    ((o_ptr->tval == TV_STAFF) ||
	     (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, p1);
		object_desc_num_macro(t, o_ptr->pval);
		object_desc_str_macro(t, " charge");
		if (o_ptr->pval != 1)
		{
			object_desc_chr_macro(t, 's');
		}
		object_desc_chr_macro(t, p2);
	}

	/* Hack -- Rods have a "charging" indicator */
	else if (known && (o_ptr->tval == TV_ROD))
	{
		/* Hack -- Dump " (charging)" if relevant */
		if (o_ptr->pval)
		{
		     object_desc_str_macro(t, " (");
		     object_desc_num_macro(t, o_ptr->pval);
		     object_desc_str_macro(t, " turns left)");
		}
	}

	/* Hack -- Process Lanterns/Torches */
	else if ((o_ptr->tval == TV_LITE) && (!artifact_p(o_ptr)))
	{
		/* Hack -- Turns of light for normal lites */
		object_desc_str_macro(t, " (with ");
		object_desc_num_macro(t, o_ptr->pval);
		object_desc_str_macro(t, " turns of light)");
	}


	/* Dump "pval" flags for wearable items */
	if (known && (f1 & (TR1_PVAL_MASK)))
	{
		cptr tail = "";
		cptr tail2 = "";

		/* Do not display the "pval" flags */
		if (f3 & (TR3_HIDE_TYPE))
		{
			/* Nothing */
		}

		/* Stealth */
		else if (f1 & (TR1_STEALTH))
		{
			/* Dump " to stealth" */
			tail = " to stealth";
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

		/* Hack, hide bonus to might (already shown in multiplier) */
		if (o_ptr->tval == TV_BOW && o_ptr->name2 == EGO_EXTRA_MIGHT) { }
		else {
		  /* Start the display */
		  object_desc_chr_macro(t, ' ');
		  object_desc_chr_macro(t, p1);
		
		  /* Dump the "pval" itself */
		  object_desc_int_macro(t, o_ptr->pval);
		     
		  /* Add the descriptor */
		  object_desc_str_macro(t, tail);
		  object_desc_str_macro(t, tail2);
		
		  /* Finish the display */
		  object_desc_chr_macro(t, p2);
		}
	}


	/* Indicate "charging" artifacts */
	if (known && o_ptr->timeout)
	{
		/* Hack -- Dump " (charging)" if relevant */
		object_desc_str_macro(t, " (charging)");
	}


	/* No more details wanted */
	if (mode < 3) goto object_desc_done;


	/* Use standard inscription */
	if (o_ptr->note)
	{
		u = quark_str(o_ptr->note);
	}

	/* Use nothing */
	else
	{
		u = NULL;
	}

	/* default */
	v = NULL;

	/* Use "cursed" if the item is known to be cursed */
	if (cursed_p(o_ptr) && known)
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

	/* Psuedo-id */
	/* Must be a unknown wearable item */
	else if (o_ptr->tval >= TV_SHOT && o_ptr->tval <= TV_DRAG_ARMOR && 
		 !object_known_p(o_ptr))
	{
	  v = inscrip_text[value_check_aux(o_ptr) - INSCRIP_NULL];
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

	/* Truncate the string to 80 chars */
	tmp_buf[79] = '\0';

	/* Copy the string over */
	strcpy(buf, tmp_buf);
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, object_type *o_ptr, int pref, int mode)
{
	/* Save the "flavor" */
	byte hack_flavor = k_info[o_ptr->k_idx].flavor;

	/* Save the "aware" flag */
	bool hack_aware = k_info[o_ptr->k_idx].aware;

	/* Save the "known" flag */
	bool hack_known = (o_ptr->ident & (IDENT_KNOWN)) ? TRUE : FALSE;


	/* Clear the flavor */
	k_info[o_ptr->k_idx].flavor = FALSE;

	/* Set the "known" flag */
	o_ptr->ident |= (IDENT_KNOWN);

	/* Force "aware" for description */
	k_info[o_ptr->k_idx].aware = TRUE;


	/* Describe the object */
	object_desc(buf, o_ptr, pref, mode);


	/* Restore "flavor" value */
	k_info[o_ptr->k_idx].flavor = hack_flavor;

	/* Restore "aware" flag */
	k_info[o_ptr->k_idx].aware = hack_aware;

	/* Clear the known flag */
	if (!hack_known) o_ptr->ident &= ~(IDENT_KNOWN);
}


static cptr act_description[ACT_MAX] =
{
	"illumination",
	"magic mapping",
	"clairvoyance",
	"protection from evil",
	"dispel evil (x5)",
	"heal (500)",
	"heal (1000)",
	"cure wounds (4d7)",
	"haste self (20+d20 turns)",
	"haste self (75+d75 turns)",
	"fire bolt (9d8)",
	"fire ball (72)",
	"large fire ball (120)",
	"frost bolt (6d8)",
	"frost ball (48)",
	"frost ball (100)",
	"frost bolt (12d8)",
	"large frost ball (200)",
	NULL,
	"recharge item I",
	"sleep II",
	"lightning bolt (4d8)",
	"large lightning ball (250)",
	"genocide",
	"mass genocide",
	"identify",
	"drain life (90)",
	"drain life (120)",
	"bizarre things",
	"star ball (150)",
	"berserk rage, bless, and resistance",
	"phase door",
	"door and trap destruction",
	"detection",
	"resistance (20+d20 turns)",
	"teleport",
	"restore life levels",
	"magic missile (2d6)",
	"a magical arrow (150)",
	"remove fear and cure poison",
	"stinking cloud (12)",
	"stone to mud",
	"teleport away",
	"word of recall",
	"confuse monster",
	"probing",
	"fire branding of bolts",
};



/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr item_activation(const object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Require activation ability */
	if (!(f3 & (TR3_ACTIVATE))) return (NULL);

	/* Artifact activations */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

#if 0
		/*
		 * ToDo: Put the recharge time back into the
		 * artifact description.
		 */
		char turns[32];

		/* Format the number of turns */
		if (a_ptr->time && a_ptr->randtime)
			sprintf(turns, " every %d+d%d turns",
			        a_ptr->time, a_ptr->randtime);
		else if (a_ptr->time)
			sprintf(turns, " every %d turns", a_ptr->time);
		else if (a_ptr->randtime)
			sprintf(turns, " every d%d turns", a_ptr->randtime);
#endif

		/* Paranoia */
		if (a_ptr->activation >= ACT_MAX)
			return (NULL);

		/* Some artifacts can be activated */
		return (act_description[a_ptr->activation]);
	}

	/* Require dragon scale mail */
	if (o_ptr->tval != TV_DRAG_ARMOR) return (NULL);

	/* Branch on the sub-type */
	switch (o_ptr->sval)
	{
		case SV_DRAGON_BLUE:
		{
			return "breathe lightning (100) every 450+d450 turns";
		}
		case SV_DRAGON_WHITE:
		{
			return "breathe frost (110) every 450+d450 turns";
		}
		case SV_DRAGON_GREEN:
		{
			return "breathe poison gas (150) every 450+d450 turns";
		}
		case SV_DRAGON_RED:
		{
			return "breathe fire (200) every 450+d450 turns";
		}
		case SV_DRAGON_MULTIHUED:
		{
			return "breathe multi-hued (250) every 225+d225 turns";
		}
		case SV_DRAGON_BRONZE:
		{
			return "breathe confusion (120) every 450+d450 turns";
		}
		case SV_DRAGON_GOLD:
		{
			return "breathe sound (130) every 450+d450 turns";
		}
		case SV_DRAGON_CHAOS:
		{
			return "breathe chaos/disenchant (220) every 300+d300 turns";
		}
		case SV_DRAGON_LAW:
		{
			return "breathe sound/shards (230) every 300+d300 turns";
		}
		case SV_DRAGON_BALANCE:
		{
			return "breathe balance (250) every 300+d300 turns";
		}
		case SV_DRAGON_SHINING:
		{
			return "breathe light/darkness (200) every 300+d300 turns";
		}
		case SV_DRAGON_POWER:
		{
			return "breathe the elements (300) every 300+d300 turns";
		}
	}


	/* Oops */
	return NULL;
}


/* Get chance of activating a device */
static cptr device_chance(const object_type *o_ptr, bool artifact)
{
     int lev = k_info[o_ptr->k_idx].level, num = USE_DEVICE, denom = p_ptr->skill_dev, chance;

     /* Confusion hurts skill */
     if (p_ptr->confused) denom /= 2;

     /* High level objects are harder */
     denom -= lev;
     
     /* Give everyone a (slight) chance */
     if (denom < USE_DEVICE)
     {
	  int factor = USE_DEVICE - denom + 1;
	  num *= factor;
	  denom = USE_DEVICE * factor;
     }

     /* Convert num to the desired form. */
     num = 1 + denom - num;

     /* Perncentage */
     chance = 100 * num / denom;

     /* Gah. Can't return a formatted string */
     if (artifact)
     {
       return (format("You have a %d%% chance of activating it.", chance));
     }
     else
     {
       return (format("You have a %d%% chance of using it.", chance));
     }
}


/*
 * Fill an array with a description of the item flags.
 *
 * "info" must point to a cptr array that is big enough to store all
 * descriptions.
 *
 * Returns the number of lines.
 *
 * ToDo: Check the len of the array to prevent buffer overflows
 * (yes, this is paranoid).
 *
 * ToDo: Allow dynamic generation of strings.
 */
static bool identify_fully_aux2(const object_type *o_ptr, int mode, cptr *info, int len)
{
	int i = 0;

	u32b f1, f2, f3;

	/* Extract the "known" and "random" flags */
	object_flags_aux(mode, o_ptr, &f1, &f2, &f3);


	/* Mega-Hack -- describe activation */
	if (f3 & (TR3_ACTIVATE))
	{
	        info[i++] = device_chance(o_ptr, TRUE);
		info[i++] = item_activation(o_ptr);
		info[i++] = "...if it is being worn.";
	}

	/* Show chance of activating devices */
	if ((o_ptr->tval == TV_ROD || o_ptr->tval == TV_WAND || o_ptr->tval == TV_STAFF))
	{
	     if (object_aware_p(o_ptr) || object_known_p(o_ptr))
		  info[i++] = device_chance(o_ptr, FALSE);
	}

	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE)
	{
		if (artifact_p(o_ptr))
		{
			info[i++] = "It provides light (radius 3) forever.";
		}
		else if (o_ptr->sval == SV_LITE_LANTERN)
		{
			info[i++] = "It provides light (radius 2) when fueled.";
		}
		else
		{
			info[i++] = "It provides light (radius 1) when fueled.";
		}
	}


	/* And then describe it fully */

	/* Skills */
	if (o_ptr->xtra1 >= OBJECT_XTRA_TYPE_SKILL_FIRST &&
	    o_ptr->xtra1 < OBJECT_XTRA_TYPE_SKILL_MULTI &&
	    object_known_p(o_ptr))
	{
	     /* if the player has any skill points above those granted by the item */
	     if (skill_value(o_ptr->xtra2) - o_ptr->pval > 0)
		  info[i++] = format("It increases your %s skill by %d.", skill_info[o_ptr->xtra2].name, o_ptr->pval);
	     else
		  info[i++] = format("It grants you the %s skill at level %d.", skill_info[o_ptr->xtra2].name, o_ptr->pval);
	}
	/* Multiple skills */
	if (o_ptr->xtra1 >= OBJECT_XTRA_TYPE_SKILL_MULTI &&
	    o_ptr->xtra1 <= OBJECT_XTRA_TYPE_SKILL_LAST &&
	    object_known_p(o_ptr))
	{
	     switch (o_ptr->xtra1)
	     {
	     case OBJECT_SKILL_BARB_ALL:
		  if (p_ptr->pclass == CLASS_WARRIOR)
		       info[i++] = format("It increases your class skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you barbarian skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_SORC_ALL:
		  if (p_ptr->pclass == CLASS_MAGE)
		       info[i++] = format("It increases your class skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you sorceress skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_SORC_ALL_ELEC:
		  if (p_ptr->pclass == CLASS_MAGE)
		       info[i++] = format("It increases your electricity skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you electricity skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_SORC_ALL_FIRE:
		  if (p_ptr->pclass == CLASS_MAGE)
		       info[i++] = format("It increases your fire skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you fire skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_SORC_ALL_COLD:
		  if (p_ptr->pclass == CLASS_MAGE)
		       info[i++] = format("It increases your cold skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you cold skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_ASSI_ALL:
		  if (p_ptr->pclass == CLASS_ROGUE)
		       info[i++] = format("It increases your class skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you assasin skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_ARCH_ALL:
		  if (p_ptr->pclass == CLASS_RANGER)
		       info[i++] = format("It increases your class skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you amazon skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_ARCH_ALL_POLEARM:
		  if (p_ptr->pclass == CLASS_RANGER)
		       info[i++] = format("It increases your polearm skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you polearm skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_ARCH_ALL_BOW:
		  if (p_ptr->pclass == CLASS_RANGER)
		       info[i++] = format("It increases your missile skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you missile skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_PALA_ALL:
		  if (p_ptr->pclass == CLASS_PALADIN)
		       info[i++] = format("It increases your class skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you paladin skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_NECRO_ALL:
		  if (p_ptr->pclass == CLASS_NECRO)
		       info[i++] = format("It increases your class skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you necromancer skills at level %d.", o_ptr->pval);
		  break;
	     case OBJECT_SKILL_NECRO_ALL_DMG:
		  if (p_ptr->pclass == CLASS_NECRO)
		       info[i++] = format("It increases your damaging necromancer skills by %d.", o_ptr->pval);
		  else info[i++] = format("It grants you damaging necromancer skills at level %d.", o_ptr->pval);
		  break;
	     }
	}
	

	if (f3 & (TR3_2HANDED))
	{
		info[i++] = "It must be wielded with two hands.";
	}

	if (f1 & (TR1_STR))
	{
		info[i++] = "It affects your strength.";
	}
	if (f1 & (TR1_INT))
	{
		info[i++] = "It affects your intelligence.";
	}
	if (f1 & (TR1_WIS))
	{
		info[i++] = "It affects your wisdom.";
	}
	if (f1 & (TR1_DEX))
	{
		info[i++] = "It affects your dexterity.";
	}
	if (f1 & (TR1_CON))
	{
		info[i++] = "It affects your constitution.";
	}
	if (f1 & (TR1_CHR))
	{
		info[i++] = "It affects your charisma.";
	}

	if (f1 & (TR1_STEALTH))
	{
		info[i++] = "It affects your stealth.";
	}
	if (f1 & (TR1_SEARCH))
	{
		info[i++] = "It affects your searching.";
	}
	if (f1 & (TR1_INFRA))
	{
		info[i++] = "It affects your infravision.";
	}
	if (f1 & (TR1_TUNNEL))
	{
		info[i++] = "It affects your ability to tunnel.";
	}
	if (f1 & (TR1_SPEED))
	{
		info[i++] = "It affects your speed.";
	}
	if (f1 & (TR1_BLOWS))
	{
		info[i++] = "It affects your attack speed.";
	}
	if (f1 & (TR1_SHOTS))
	{
		info[i++] = "It affects your shooting speed.";
	}
	if (f1 & (TR1_MIGHT))
	{
		info[i++] = "It affects your shooting power.";
	}

	if (f1 & (TR1_SLAY_ANIMAL))
	{
		info[i++] = "It is especially deadly against natural creatures.";
	}
	if (f1 & (TR1_SLAY_EVIL))
	{
		info[i++] = "It fights against evil with holy fury.";
	}
	if (f1 & (TR1_SLAY_UNDEAD))
	{
		info[i++] = "It strikes at undead with holy wrath.";
	}
	if (f1 & (TR1_SLAY_DEMON))
	{
		info[i++] = "It strikes at demons with holy wrath.";
	}
	if (f1 & (TR1_SLAY_ORC))
	{
		info[i++] = "It is especially deadly against orcs.";
	}
	if (f1 & (TR1_SLAY_TROLL))
	{
		info[i++] = "It is especially deadly against trolls.";
	}
	if (f1 & (TR1_SLAY_GIANT))
	{
		info[i++] = "It is especially deadly against giants.";
	}
	if (f1 & (TR1_SLAY_DRAGON))
	{
		info[i++] = "It is especially deadly against dragons.";
	}

	if (f1 & (TR1_KILL_DRAGON))
	{
		info[i++] = "It is a great bane of dragons.";
	}

	if (f1 & (TR1_BRAND_ELEC))
	{
		info[i++] = "It does extra damage from electricity.";
	}
	if (f1 & (TR1_BRAND_FIRE))
	{
		info[i++] = "It does extra damage from fire.";
	}
	if (f1 & (TR1_BRAND_COLD))
	{
		info[i++] = "It does extra damage from frost.";
	}
	if (f1 & (TR1_BRAND_POIS))
	{
		info[i++] = "It does extra damage from poison.";
	}

	if (f2 & (TR2_SUST_STR))
	{
		info[i++] = "It sustains your strength.";
	}
	if (f2 & (TR2_SUST_INT))
	{
		info[i++] = "It sustains your intelligence.";
	}
	if (f2 & (TR2_SUST_WIS))
	{
		info[i++] = "It sustains your wisdom.";
	}
	if (f2 & (TR2_SUST_DEX))
	{
		info[i++] = "It sustains your dexterity.";
	}
	if (f2 & (TR2_SUST_CON))
	{
		info[i++] = "It sustains your constitution.";
	}
	if (f2 & (TR2_SUST_CHR))
	{
		info[i++] = "It sustains your charisma.";
	}

	if (object_resist_known(o_ptr, RES_ELEC))
	{
		info[i++] = format("It provides %d%% resistance to electricity.", object_resist_known(o_ptr, RES_ELEC));
	}

	if (object_resist_known(o_ptr, RES_FIRE))
	{
		info[i++] = format("It provides %d%% resistance to fire.", object_resist_known(o_ptr, RES_FIRE));
	}

	if (object_resist_known(o_ptr, RES_COLD))
	{
		info[i++] = format("It provides %d%% resistance to cold.", object_resist_known(o_ptr, RES_COLD));
	}

	if (object_resist_known(o_ptr, RES_POIS))
	{
		info[i++] = format("It provides %d%% resistance to poison.", object_resist_known(o_ptr, RES_POIS));
	}

	if (f2 & (TR2_RES_FEAR))
	{
		info[i++] = "It provides resistance to fear.";
	}

	if (f2 & (TR2_RES_LITE))
	{
		info[i++] = "It provides resistance to light.";
	}

	if (f2 & (TR2_RES_DARK))
	{
		info[i++] = "It provides resistance to dark.";
	}

	if (f2 & (TR2_RES_BLIND))
	{
		info[i++] = "It provides resistance to blindness.";
	}

	if (f2 & (TR2_RES_CONFU))
	{
		info[i++] = "It provides resistance to confusion.";
	}

	if (f2 & (TR2_RES_SOUND))
	{
		info[i++] = "It provides resistance to sound.";
	}

	if (f2 & (TR2_RES_SHARD))
	{
		info[i++] = "It provides resistance to shards.";
	}

	if (f2 & (TR2_RES_NEXUS))
	{
		info[i++] = "It provides resistance to nexus.";
	}

	if (f2 & (TR2_RES_NETHR))
	{
		info[i++] = "It provides resistance to nether.";
	}

	if (f2 & (TR2_RES_CHAOS))
	{
		info[i++] = "It provides resistance to chaos.";
	}

	if (f2 & (TR2_RES_DISEN))
	{
		info[i++] = "It provides resistance to disenchantment.";
	}

	if (f3 & (TR3_SLOW_DIGEST))
	{
		info[i++] = "It slows your metabolism.";
	}

	if (f3 & (TR3_FEATHER))
	{
		info[i++] = "It induces feather falling.";
	}

	if (f3 & (TR3_LITE))
	{
		info[i++] = "It provides permanent light.";
	}

	if (f3 & (TR3_REGEN))
	{
		info[i++] = "It speeds your regenerative powers.";
	}

	if (f3 & (TR3_TELEPATHY))
	{
		info[i++] = "It gives telepathic powers.";
	}

	if (f3 & (TR3_SEE_INVIS))
	{
		info[i++] = "It allows you to see invisible monsters.";
	}

	if (f3 & (TR3_FREE_ACT))
	{
		info[i++] = "It provides immunity to paralysis.";
	}

	if (f3 & (TR3_HOLD_LIFE))
	{
		info[i++] = "It provides resistance to life draining.";
	}

	if (f3 & (TR3_IMPACT))
	{
		info[i++] = "It induces earthquakes.";
	}

	if (f3 & (TR3_TELEPORT))
	{
		info[i++] = "It induces random teleportation.";
	}

	if (f3 & (TR3_AGGRAVATE))
	{
		info[i++] = "It aggravates nearby creatures.";
	}

	if (f3 & (TR3_DRAIN_EXP))
	{
		info[i++] = "It drains experience.";
	}

	if (f3 & (TR3_BLESSED))
	{
		info[i++] = "It has been blessed by the gods.";
	}

	if (object_known_p(o_ptr) && cursed_p(o_ptr))
	{
		if (f3 & (TR3_PERMA_CURSE))
		{
			info[i++] = "It is permanently cursed.";
		}
		else if (f3 & (TR3_HEAVY_CURSE))
		{
			info[i++] = "It is heavily cursed.";
		}
		else
		{
			info[i++] = "It is cursed.";
		}
	}

	if (f3 & (TR3_IGNORE_ELEC))
	{
		info[i++] = "It cannot be harmed by electricity.";
	}
	if (f3 & (TR3_IGNORE_FIRE))
	{
		info[i++] = "It cannot be harmed by fire.";
	}
	if (f3 & (TR3_IGNORE_COLD))
	{
		info[i++] = "It cannot be harmed by cold.";
	}

	/* Unknown extra powers (ego-item with random extras or artifact) */
	if (object_known_p(o_ptr) && (!(o_ptr->ident & IDENT_MENTAL)) &&
	    /* Skill items are easily known */
	    o_ptr->xtra1 < OBJECT_XTRA_TYPE_SKILL_FIRST &&
	    o_ptr->xtra1 > OBJECT_XTRA_TYPE_SKILL_LAST &&
	    ((o_ptr->xtra1) || artifact_p(o_ptr)))
	{
		info[i++] = "It has hidden powers.";
	}


	/* Return the number of lines */
	return (i);
}


/*
 * Describe an item's random attributes for "character dumps"
 */
int identify_random_gen(const object_type *o_ptr, cptr *info, int len)
{
	/* Fill the list of descriptions and return the count */
	return identify_fully_aux2(o_ptr, OBJECT_FLAGS_RANDOM, info, len);
}


/*
 * Describe an item
 */
bool identify_fully_aux(const object_type *o_ptr)
{
	int i, j, k;
	cptr info[128];


	/* Fill the list of descriptions */
	i = identify_fully_aux2(o_ptr, OBJECT_FLAGS_KNOWN, info, 128);

	/* No special effects */
	if (!i) return (FALSE);


	/* Save screen */
	screen_save();


	/* Erase the screen */
	Term_clear();

	/* Label the information */
	prt("     Item Attributes:", 1, 0);

	/* Dump some info */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prt(info[j], k++, 0);

		/* Page wrap */
		if ((k == 22) && (j+1 < i))
		{
			prt("-- more --", k, 0);
			inkey();

			/* Erase the screen */
			Term_clear();

			/* Label the information */
			prt("     Item Attributes:", 1, 0);

			/* Back to the top */
			k = 2;
		}
	}

	/* Wait for it */
	prt("[Press any key to continue]", k, 0);
	(void)inkey();


	/* Load screen */
	screen_load();


	/* Gave knowledge */
	return (TRUE);
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
	i = (islower(c) ? A2I(c) : -1);

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
	i = (islower(c) ? A2I(c) : -1) + INVEN_WIELD;

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
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
                case TV_SHOT:
		{
                        if(inventory[INVEN_BOW].k_idx)
                        {
                                if(inventory[INVEN_BOW].sval < 10)
                                        return (INVEN_AMMO);
                        }
                        return -1;
        	}
                case TV_ARROW:
		{
                        if(inventory[INVEN_BOW].k_idx)
                        {
                                if((inventory[INVEN_BOW].sval >= 10)&&(inventory[INVEN_BOW].sval < 20))
                                        return (INVEN_AMMO);
                        }
                        return -1;
        	}
                case TV_BOLT:
		{                        
                        if(inventory[INVEN_BOW].k_idx)
                        {
                                if(inventory[INVEN_BOW].sval >= 20)
                                        return (INVEN_AMMO);
                        }
                        return -1;
        	}
		case TV_DIGGING:
		{
		     return (INVEN_WIELD);
		}
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
	        case TV_AXE:
		{
		        object_type *first_ptr = &inventory[INVEN_WIELD];
			u32b f1, f2, f3, f1b, f2b, f3b;
			object_flags(o_ptr, &f1b, &f2b, &f3b); /* weapon to be wielded */
			object_flags(first_ptr, &f1, &f2, &f3); /* weapon in first slot */

		        /* Cannot wield two-handed weapon with something in shield slot */
			if (inventory[INVEN_ARM].k_idx && (f3b & (TR3_2HANDED))) 
			     return (-1);

		        /* Any weapon must replace two-handed weapon first */
			if (first_ptr->k_idx && (f3 & (TR3_2HANDED))) return (INVEN_WIELD);

			/* Two handed weapon tries first slot */
			if (f3b & (TR3_2HANDED)) return (INVEN_WIELD);

			/* Non barbarians cannot */
			if (p_ptr->pclass != CLASS_WARRIOR)
			{
			     /* Assasin could be wielding claw in second slot */
			     if (dual_wielding()) return (-1);

			     return (INVEN_WIELD);
			}

			/* Barbarians with non-okay weapon in first slot cannot */
			if (inventory[INVEN_WIELD].tval != TV_HAFTED &&
			    inventory[INVEN_WIELD].tval != TV_POLEARM &&
			    inventory[INVEN_WIELD].tval != TV_SWORD &&
			    inventory[INVEN_WIELD].tval != TV_AXE) return (INVEN_WIELD);

			/* Use the weapon slot by default */
			if (!inventory[INVEN_WIELD].k_idx)
			     return (INVEN_WIELD);

			/* Use the shield slot if weapon slot is full */
			return (INVEN_ARM);
		}

	        case TV_CLAW:
		{
			/* Non assasins cannot */
			if (p_ptr->pclass != CLASS_ROGUE)
			{
			     /* Barbarian could be wielding non-claw in second slot */
			     if (dual_wielding()) return (-1);

			     return (INVEN_WIELD);
			}

			/* Assasins with non-claw in first slot cannot */
			if (inventory[INVEN_WIELD].tval != TV_CLAW) return (INVEN_WIELD);

			/* Use the weapon slot by default */
			if (!inventory[INVEN_WIELD].k_idx)
			     return (INVEN_WIELD);

			/* Use the shield slot if weapon slot is full */
			return (INVEN_ARM);
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
		{
		        /* Cannot wield with two-handed weapon */
		        object_type *first_ptr = &inventory[INVEN_WIELD];
			u32b f1, f2, f3;
			object_flags(first_ptr, &f1, &f2, &f3);
			if (first_ptr->k_idx && (f3 & (TR3_2HANDED))) return (-1);

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
		case INVEN_WIELD: p = "Wielding"; break;
		case INVEN_BOW:   p = "Shooting"; break;
		case INVEN_AMMO:  p = "Quiver"; break;
		case INVEN_LEFT:  p = "On left hand"; break;
		case INVEN_RIGHT: p = "On right hand"; break;
		case INVEN_NECK:  p = "Around neck"; break;
		case INVEN_LITE:  p = "Light source"; break;
		case INVEN_BODY:  p = "On body"; break;
		case INVEN_OUTER: p = "About body"; break;
	        case INVEN_ARM: {
		     if (dual_wielding()) p = "Wielding";
		     else p = "On arm";
		     break; }
	        case INVEN_HEAD:  p = "On head"; break;
		case INVEN_HANDS: p = "On hands"; break;
		case INVEN_FEET:  p = "On feet"; break;
		default:          p = "In pack"; break;
	}

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD || (i == INVEN_ARM && dual_wielding()))
	{
		object_type *first_ptr;
		object_type *second_ptr;
		int weight = 0;
		first_ptr = &inventory[INVEN_WIELD];
		second_ptr = &inventory[INVEN_ARM];
		if (first_ptr->k_idx) weight += first_ptr->weight;
		if (dual_wielding()) weight += second_ptr->weight;
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < weight / 10)
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
		case INVEN_AMMO:  p = "carrying"; break;
		case INVEN_LEFT:  p = "wearing on your left hand"; break;
		case INVEN_RIGHT: p = "wearing on your right hand"; break;
		case INVEN_NECK:  p = "wearing around your neck"; break;
		case INVEN_LITE:  p = "using to light the way"; break;
		case INVEN_BODY:  p = "wearing on your body"; break;
		case INVEN_OUTER: p = "wearing on your back"; break;
	        case INVEN_ARM: {
		     if (dual_wielding()) p = "attacking monsters with"; 
		     else p = "wearing on your arm";
		     break; }
		case INVEN_HEAD:  p = "wearing on your head"; break;
		case INVEN_HANDS: p = "wearing on your hands"; break;
		case INVEN_FEET:  p = "wearing on your feet"; break;
		default:          p = "carrying in your pack"; break;
	}

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD || (i == INVEN_ARM && dual_wielding()))
	{
		object_type *first_ptr;
		object_type *second_ptr;
		int weight = 0;
		first_ptr = &inventory[INVEN_WIELD];
		second_ptr = &inventory[INVEN_ARM];
		if (first_ptr->k_idx) weight += first_ptr->weight;
		if (dual_wielding()) weight += second_ptr->weight;
		if (adj_str_hold[p_ptr->stat_ind[A_STR]] < weight / 10)
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
sint scan_floor(int *items, int size, int y, int x, int mode)
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
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval & 0x7F];

		/* Display the entry itself */
		Term_putstr(3, i, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3+n, i, 255);

		/* Display the weight if needed */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			Term_putstr(71, i, -1, TERM_WHITE, tmp_val);
		}
	}

	/* Erase the rest of the window */
	for (i = z; i < Term->hgt; i++)
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


	/* Display the equipment */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
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
		Term_putstr(0, i - INVEN_WIELD, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		attr = tval_to_attr[o_ptr->tval & 0x7F];

		/* Display the entry itself */
		Term_putstr(3, i - INVEN_WIELD, n, attr, o_name);

		/* Erase the rest of the line */
		Term_erase(3+n, i - INVEN_WIELD, 255);

		/* Display the slot description (if needed) */
		if (show_labels)
		{
			Term_putstr(61, i - INVEN_WIELD, -1, TERM_WHITE, "<--");
			Term_putstr(65, i - INVEN_WIELD, -1, TERM_WHITE, mention_use(i));
		}

		/* Display the weight (if needed) */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = (show_labels ? 52 : 71);
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

	object_type *o_ptr;

	char o_name[80];

	char tmp_val[80];

	int out_index[24];
	byte out_color[24];
	char out_desc[24][80];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

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
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];

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

		/* Display the weight if needed */
		if (show_weights)
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

	object_type *o_ptr;

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
	if (show_labels) lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Scan the equipment list */
	for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Truncate the description */
		o_name[lim] = 0;

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];

		/* Save the description */
		strcpy(out_desc[k], o_name);

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (show_labels) l += (14 + 2);

		/* Increase length for weight (if needed) */
		if (show_weights) l += 9;

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
		/* Get the index */
		i = out_index[j];

		/* Get the item */
		o_ptr = &inventory[i];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
		sprintf(tmp_val, "%c)", index_to_label(i));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
			sprintf(tmp_val, "%-14s: ", mention_use(i));
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
			sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j+1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);
}


#ifdef ALLOW_EASY_FLOOR

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

	int out_index[24];
	byte out_color[24];
	char out_desc[24][80];


	/* Default length */
	len = 79 - 50;

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Display the inventory */
	for (k = 0, i = 0; i < floor_num; i++)
	{
		o_ptr = &o_list[floor_list[i]];

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];

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
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j + 1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);
}

#endif /* ALLOW_EASY_FLOOR */



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
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Prompt */
	sprintf(out_val, "%s %s? ", prompt, o_name);

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


	/* Check every object */
	for (i = 0; i < INVEN_TOTAL; ++i)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Skip empty inscriptions */
		if (!o_ptr->note) continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->note), '@');

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

	bool allow_inven = FALSE;
	bool allow_equip = FALSE;
	bool allow_floor = FALSE;

	bool toggle = FALSE;

	char tmp_val[160];
	char out_val[160];

	int floor_list[24];
	int floor_num;


#ifdef ALLOW_REPEAT

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
	}

#endif /* ALLOW_REPEAT */


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


	/* Scan all objects in the grid */
	floor_num = scan_floor(floor_list, 23, py, px, 0x00);

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

#ifdef ALLOW_EASY_FLOOR

		/* Use floor if allowed */
		else if (easy_floor)
		{
			p_ptr->command_wrk = (USE_FLOOR);
		}

#endif /* ALLOW_EASY_FLOOR */

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
		if (show_choices)
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
				strcat(out_val, tmp_val);
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
				strcat(out_val, tmp_val);
			}

			/* Indicate ability to "view" */
			if (!p_ptr->command_see) strcat(out_val, " * to see,");

			/* Indicate legality of "toggle" */
			if (use_inven) strcat(out_val, " / for Inven,");

			/* Indicate legality of the "floor" */
			if (allow_floor) strcat(out_val, " - for floor,");
		}

#ifdef ALLOW_EASY_FLOOR

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
				strcat(out_val, tmp_val);
			}

			/* Indicate ability to "view" */
			if (!p_ptr->command_see) strcat(out_val, " * to see,");

			/* Append */
			if (use_inven) strcat(out_val, " / for Inven,");

			/* Append */
			else if (use_equip) strcat(out_val, " / for Equip,");
		}

#endif /* ALLOW_EASY_FLOOR */

		/* Finish the prompt */
		strcat(out_val, " ESC");

		/* Build the prompt */
		sprintf(tmp_val, "(%s) %s", out_val, pmt);

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

#ifdef ALLOW_EASY_FLOOR

				if (easy_floor)
				{
					/* There is only one item */
					if (floor_num == 1)
					{
						/* Hack -- Auto-Select */
						if ((p_ptr->command_wrk == (USE_FLOOR)) ||
						    (!floor_query_flag))
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

#endif /* ALLOW_EASY_FLOOR */

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

#ifdef ALLOW_EASY_FLOOR

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

#endif /* ALLOW_EASY_FLOOR */

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
				verify = (isupper(which) ? TRUE : FALSE);

				/* Lowercase */
				which = tolower(which);

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

#ifdef ALLOW_EASY_FLOOR

				/* Convert letter to floor index */
				else
				{
					k = (islower(which) ? A2I(which) : -1);

					if (k < 0 || k >= floor_num)
					{
						bell("Illegal object choice (floor)!");
						break;
					}

					/* Special index */
					k = 0 - floor_list[k];
				}

#endif /* ALLOW_EASY_FLOOR */

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


	/* Clean up */
	if (show_choices)
	{
		/* Toggle again if needed */
		if (toggle) toggle_inven_equip();

		/* Update */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Window stuff */
		window_stuff();
	}


	/* Clear the prompt line */
	prt("", 0, 0);

	/* Warning if needed */
	if (oops && str) msg_print(str);

#ifdef ALLOW_REPEAT

	/* Save item if available */
	if (item) repeat_push(*cp);

#endif /* ALLOW_REPEAT */

	/* Result */
	return (item);
}
