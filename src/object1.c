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
 * XXX XXX Hack -- note that "TERM_MULTI" is now just "TERM_VIOLET"
 * We will have to find a cleaner method for "MULTI_HUED" later.
 * There were only two multi-hued "flavors" (one potion, one food).
 * Plus five multi-hued "base-objects" (3 dragon scales, one blade
 * of chaos, and one something else).  See the SHIMMER_OBJECTS code
 * in "dungeon.c" and the object color extractor in "cave.c".
 */
#define TERM_MULTI	TERM_VIOLET


/*
 * Max sizes of the following arrays
 */
#define MAX_ROCKS      42 /* Used with rings (min 38) */
#define MAX_AMULETS    16 /* Used with amulets (min 13) */
#define MAX_WOODS      32 /* Used with staffs (min 30) */
#define MAX_METALS     32 /* Used with wands/rods (min 29/28) */
#define MAX_COLORS     90 /* Used with potions (min 60) */
#define MAX_SHROOM     20 /* Used with mushrooms (min 20) */
#define MAX_TITLES     50 /* Used with scrolls (min 48) */
#define MAX_SYLLABLES  158 /* Used with scrolls (see below) */


/*
 * Rings (adjectives and colors)
 */

static cptr ring_adj[MAX_ROCKS] = {
	"Alexandrite", "Amethyst", "Aquamarine", "Azurite", "Beryl",
	"Bloodstone", "Calcite", "Carnelian", "Corundum", "Shiny",
	"Greenish", "Fluorite", "Reddish", "Granite", "Jade",
	"Jasper", "Lapis Lazuli", "Malachite", "Marble", "Moonstone",
	"Onyx", "Dull Gray", "Pearl", "Quartz", "Quartzite",
	"Rhodonite", "Shiny Red", "Shiny Blue", "Tiger Eye", "Topaz",
	"Turquoise", "Zircon", "Platinum", "Bronze", "Radiant",
	"Glowing", "Jagged", "Tortoise Shell", "Twisted", "Jet",
	"Engagement", "Grayish"
};

static byte ring_col[MAX_ROCKS] = {
	TERM_GREEN, TERM_VIOLET, TERM_L_BLUE, TERM_L_BLUE, TERM_L_GREEN,
	TERM_RED, TERM_WHITE, TERM_RED, TERM_SLATE, TERM_WHITE,
	TERM_GREEN, TERM_L_GREEN, TERM_RED, TERM_L_WHITE, TERM_L_GREEN,
	TERM_UMBER, TERM_BLUE, TERM_GREEN, TERM_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_L_WHITE, TERM_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_RED, TERM_BLUE, TERM_YELLOW, TERM_YELLOW,
	TERM_L_BLUE, TERM_L_UMBER, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_DARK, TERM_L_WHITE, TERM_UMBER, TERM_L_BLUE, TERM_L_DARK,
	TERM_YELLOW, TERM_L_GREEN
};


/*
 * Amulets (adjectives and colors)
 */

static cptr amulet_adj[MAX_AMULETS] = {
	"Translucent", "Driftwood", "Coral", "Agate", "Ivory",
	"Smoky", "Bone", "Brass", "Shadowy", "Pewter",
	"Tortoise Shell", "Spotted", "Azure", "Inlaid", "Marble",
	"Transparent"
};

static byte amulet_col[MAX_AMULETS] = {
	TERM_YELLOW, TERM_L_UMBER, TERM_WHITE, TERM_L_WHITE, TERM_WHITE,
	TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_SLATE,
	TERM_UMBER, TERM_YELLOW, TERM_L_BLUE, TERM_WHITE, TERM_L_WHITE,
	TERM_L_UMBER
};


/*
 * Staffs (adjectives and colors)
 */

static cptr staff_adj[MAX_WOODS] = {
	"Aspen", "Balsa", "Banyan", "Birch", "Cedar",
	"Cottonwood", "Cypress", "Dogwood", "Elm", "Eucalyptus",
	"Hemlock", "Hickory", "Ironwood", "Locust", "Mahogany",
	"Maple", "Mulberry", "Oak", "Pine", "Redwood",
	"Rosewood", "Spruce", "Sycamore", "Teak", "Walnut",
	"Mistletoe", "Hawthorn", "Bamboo", "Silver", "Runed",
	"Golden", "Ashen" /*,"Gnarled","Ivory","Willow" */
};

static byte staff_col[MAX_WOODS] = {
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_RED,
	TERM_RED, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_GREEN, TERM_L_UMBER, TERM_L_UMBER, TERM_L_WHITE, TERM_UMBER,
	TERM_YELLOW, TERM_SLATE, /*???,???,??? */
};


/*
 * Wands (adjectives and colors)
 */

static cptr wand_adj[MAX_METALS] = {
	"Aluminum", "Cast Iron", "Chromium", "Copper-Plated", "Gold-Plated",
	"Iron-Plated", "Magnesium", "Molybdenum", "Nickel", "Rusty",
	"Silver-Plated", "Steel-Plated", "Tin", "Titanium", "Tungsten",
	"Zirconium", "Zinc", "Aluminum-Plated", "Inlaid", "Glass-Plated",
	"Nickel-Plated", "Wood-Plated", "Bent", "Tin-Plated", "Zinc-Plated",
	"Mithril-Plated", "Twisted", "Runed", "Bronze", "Carved",
	"Platinum", "Lead" /*,"Lead-Plated","Ivory","Pewter" */
};

static byte wand_col[MAX_METALS] = {
	TERM_L_BLUE, TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_SLATE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_UMBER, TERM_RED,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_WHITE, TERM_WHITE,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_BLUE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_UMBER, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_BLUE, TERM_L_BLUE, TERM_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_WHITE, TERM_SLATE,	/*TERM_SLATE,TERM_WHITE,TERM_SLATE */
};


/*
 * Rods (adjectives and colors).
 * Efficiency -- copied from wand arrays
 */

static cptr rod_adj[MAX_METALS];

static byte rod_col[MAX_METALS];


/*
 * Mushrooms (adjectives and colors)
 */

static cptr food_adj[MAX_SHROOM] = {
	"Blue", "Black", "Black Spotted", "Brown", "Dark Blue",
	"Dark Green", "Dark Red", "Yellow", "Furry", "Green",
	"Grey", "Light Blue", "Light Green", "Violet", "Red",
	"Slimy", "Tan", "White", "White Spotted", "Wrinkled",
};

static byte food_col[MAX_SHROOM] = {
	TERM_BLUE, TERM_L_DARK, TERM_L_DARK, TERM_UMBER, TERM_BLUE,
	TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_L_WHITE, TERM_GREEN,
	TERM_SLATE, TERM_L_BLUE, TERM_L_GREEN, TERM_VIOLET, TERM_RED,
	TERM_SLATE, TERM_L_UMBER, TERM_WHITE, TERM_WHITE, TERM_UMBER
};


/*
 * Color adjectives and colors, for potions.
 * Hack -- The first four entries are hard-coded.
 * (water, apple juice, slime mold juice, something)
 */

static cptr potion_adj[MAX_COLORS] = {
	"Clear", "Light Brown", "Icky Green", "xxx",
	"Azure", "Blue", "Blue Speckled", "Black", "Brown", "Brown Speckled",
	"Bubbling", "Chartreuse", "Cloudy", "Copper Speckled", "Crimson",
	"Cyan",
	"Dark Blue", "Dark Green", "Dark Red", "Gold Speckled", "Green",
	"Green Speckled", "Grey", "Grey Speckled", "Hazy", "Indigo",
	"Light Blue", "Light Green", "Magenta", "Metallic Blue",
	"Metallic Red",
	"Metallic Green", "Metallic Purple", "Misty", "Orange",
	"Orange Speckled",
	"Pink", "Pink Speckled", "Puce", "Purple", "Purple Speckled",
	"Red", "Red Speckled", "Silver Speckled", "Smoky", "Tangerine",
	"Violet", "Vermilion", "White", "Yellow", "Violet Speckled",
	"Pungent", "Clotted Red", "Viscous Pink", "Oily Yellow",
	"Gloopy Green",
	"Shimmering", "Coagulated Crimson", "Yellow Speckled", "Gold",
	"Swirling", "Clotted Orange", "Clotted Blue", "Clotted Silver",
	"Gloppy Purple",
	"Dirty Brown", "Bright Red", "Bubbly", "Sparkling Clear",
	"Opaque Silver",
	"Chunky", "Disgusting Yellow", "Viscous White", "Smelly", "Grainy",
	"Fibrous", "Totally Black", "Totally Clear", "Fuming Green",
	"Acidic Orange",
	"Opaque Red", "Blood-Red", "Sea-Green", "Metallic", "Pungent Purple",
	"Swirling Grey", "Metallic Grey", "Gelatinous Brown", "Boiling Clear",
	"Gold Speckled"
};

static byte potion_col[MAX_COLORS] = {
	TERM_WHITE, TERM_L_UMBER, TERM_GREEN, 0,
	TERM_L_BLUE, TERM_BLUE, TERM_BLUE, TERM_L_DARK, TERM_UMBER, TERM_UMBER,
	TERM_L_WHITE, TERM_L_GREEN, TERM_WHITE, TERM_L_UMBER, TERM_RED,
	TERM_L_BLUE,
	TERM_BLUE, TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_GREEN,
	TERM_GREEN, TERM_SLATE, TERM_SLATE, TERM_L_WHITE, TERM_VIOLET,
	TERM_L_BLUE, TERM_L_GREEN, TERM_RED, TERM_BLUE, TERM_RED,
	TERM_GREEN, TERM_VIOLET, TERM_L_WHITE, TERM_ORANGE, TERM_ORANGE,
	TERM_L_RED, TERM_L_RED, TERM_VIOLET, TERM_VIOLET, TERM_VIOLET,
	TERM_RED, TERM_RED, TERM_L_WHITE, TERM_L_DARK, TERM_ORANGE,
	TERM_VIOLET, TERM_RED, TERM_WHITE, TERM_YELLOW, TERM_VIOLET,
	TERM_L_RED, TERM_RED, TERM_L_RED, TERM_YELLOW, TERM_GREEN,
	TERM_MULTI, TERM_RED, TERM_YELLOW, TERM_YELLOW, TERM_VIOLET,
	TERM_ORANGE, TERM_BLUE, TERM_SLATE, TERM_VIOLET,
	TERM_UMBER, TERM_L_RED, TERM_WHITE, TERM_WHITE, TERM_SLATE,
	TERM_L_DARK, TERM_YELLOW, TERM_WHITE, TERM_GREEN, TERM_SLATE,
	TERM_L_UMBER, TERM_L_DARK, TERM_WHITE, TERM_L_GREEN, TERM_ORANGE,
	TERM_RED, TERM_RED, TERM_L_GREEN, TERM_SLATE, TERM_VIOLET,
	TERM_SLATE, TERM_SLATE, TERM_UMBER, TERM_WHITE, TERM_YELLOW
};


/*
 * Syllables for scrolls (must be 1-4 letters each)
 */

static cptr syllables[MAX_SYLLABLES] = {
	"ana", "aba", "aga", "aksa", "ala", "anu", "any", "appe",
	"argo", "arze", "asha", "ausa", "ban", "bar", "bat", "bek",
	"bier", "bin", "bit", "bjor", "blud", "bot", "buz",
	"byt", "com", "con", "cos", "cred", "dal", "dan",
	"den", "der", "doer", "dok", "eepe", "ela", "enge", "era", "ere",
	"erka",
	"eshe", "evsa", "fad", "fid", "flig", "for", "frid", "ful", "gan",
	"gar", "gleb", "gop", "gred", "hap", "her", "hyd", "i",
	"inge", "iona", "ipe", "isha", "itu", "ite", "ivu", "jor",
	"khod", "klig", "kis", "lapp", "lech", "man", "mar",
	"med", "mil", "mic", "mik", "mon", "mun", "mur", "nag", "nej",
	"nelg", "nep", "ner", "nes", "nis", "nih", "nin", "o",
	"odo", "oode", "orgu", "orne", "oxa", "oxy", "payd", "pet",
	"plex", "plur", "pod", "pot", "prok", "red", "real", "rhov",
	"rid", "ros", "rog", "rok", "rol", "sand", "san", "sat",
	"see", "sef", "seh", "shu", "ski", "sna", "sne", "snik",
	"snol", "sod", "sol", "srix", "staz", "sun", "taz", "tab",
	"tem", "ther", "til", "tox", "trol", "tuel", "turs", "u",
	"ulku", "ume", "una", "uni", "uru", "val", "viv", "vlyg",
	"vom", "wah", "wed", "werg", "wex", "whon", "wun", "x",
	"yerge", "ype", "zun", "trid", "blag"
};


/*
 * Hold the titles of scrolls, 6 to 14 characters each
 * Also keep an array of scroll colors (always WHITE for now)
 */

static char scroll_adj[MAX_TITLES][16];

static byte scroll_col[MAX_TITLES];






/*
 * Certain items have a flavor
 * This function is used only by "flavor_init()"
 */
static bool object_has_flavor(int i)
{
	object_kind *k_ptr = &k_info[i];

	/* Check for flavor */
	switch (k_ptr->tval)
	{
			/* The standard "flavored" items */
		case TV_AMULET:
		case TV_RING:
		case TV_STAFF:
		case TV_WAND:
		case TV_SCROLL:
		case TV_POTION:
		case TV_ROD:
		{
			return (TRUE);
		}

			/* Hack -- food SOMETIMES has a flavor */
		case TV_FOOD:
		{
			if (k_ptr->sval < SV_FOOD_MIN_FOOD)
				return (TRUE);
			return (FALSE);
		}
	}

	/* Assume no flavor */
	return (FALSE);
}


/*
 * Certain items, if aware, are known instantly
 * This function is used only by "flavor_init()"
 *
 * XXX XXX XXX Add "EASY_KNOW" flag to "k_info.txt" file
 */
static bool object_easy_know(int i)
{
	object_kind *k_ptr = &k_info[i];

	/* Analyze the "tval" */
	switch (k_ptr->tval)
	{
			/* Spellbooks */
		case TV_SPELLBOOK:
		case TV_MIMIC_BOOK:
		{
			return (TRUE);
		}

			/* Simple items */
		case TV_FLASK:
		case TV_JUNK:
		case TV_BOTTLE:
		case TV_SKELETON:
		case TV_SPIKE:
		case TV_INGRED:
		case TV_TEXT:
		case TV_CORPSE:
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
			if (k_ptr->flags3 & (TR3_EASY_KNOW))
				return (TRUE);
			return (FALSE);
		}
	}

	/* Nope */
	return (FALSE);
}


/*
 * Hack -- prepare the default object attr codes by tval
 *
 * XXX XXX XXX Off-load to "pref.prf" file
 */
static byte default_tval_to_attr(int tval)
{
	switch (tval)
	{
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_CORPSE:
		case TV_JUNK:
		{
			return (TERM_WHITE);
		}

		case TV_CHEST:
		{
			return (TERM_SLATE);
		}

		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			return (TERM_L_UMBER);
		}

		case TV_LITE:
		{
			return (TERM_YELLOW);
		}

		case TV_SPIKE:
		{
			return (TERM_SLATE);
		}

		case TV_BOW:
		{
			return (TERM_UMBER);
		}

		case TV_DIGGING:
		{
			return (TERM_SLATE);
		}

		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (TERM_L_WHITE);
		}

		case TV_BOOTS:
		case TV_GLOVES:
		case TV_CROWN:
		case TV_HELM:
		case TV_SHIELD:
		case TV_CLOAK:
		{
			return (TERM_L_UMBER);
		}

		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		{
			return (TERM_SLATE);
		}

		case TV_AMULET:
		{
			return (TERM_ORANGE);
		}

		case TV_RING:
		case TV_INGRED:
		{
			return (TERM_RED);
		}

		case TV_STAFF:
		{
			return (TERM_L_UMBER);
		}

		case TV_WAND:
		{
			return (TERM_L_GREEN);
		}

		case TV_ROD:
		{
			return (TERM_L_WHITE);
		}

		case TV_SCROLL:
		{
			return (TERM_WHITE);
		}

		case TV_POTION:
		{
			return (TERM_L_BLUE);
		}

		case TV_FLASK:
		{
			return (TERM_YELLOW);
		}

		case TV_FOOD:
		{
			return (TERM_L_UMBER);
		}

		case TV_SPELLBOOK:
		{
			return (TERM_L_RED);
		}

		case TV_RANDART:
		{
			return (TERM_YELLOW);
		}

		case TV_MIMIC_BOOK:
		{
			return TERM_L_GREEN;
		}

		case TV_TEXT:
		{
			return TERM_ORANGE;
		}
	}

	return (TERM_WHITE);
}


static cptr get_syllable_vowel()
{
	cptr try;

	while (TRUE)
	{
		try = syllables[rand_int(MAX_SYLLABLES)];

		if (strchr("aeiou", try[0]))
		{
			return try;
		}
		else
		{
			continue;
		}
	}
}

static cptr get_syllable_cons()
{
	cptr try;

	while (TRUE)
	{
		try = syllables[rand_int(MAX_SYLLABLES)];

		if (strchr("aeiou", try[0]))
		{
			continue;
		}
		else
		{
			return try;
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
 *
 * Note that the "hacked seed" may provide an RNG with alternating parity!
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

				/* Choose two or three syllables */
				s = randint(2) + 1;

				/* Add a one or two syllable word */
				for (q = 0; q < s; q++)
				{
					/* Add the syllable */
					/* Even -- cons. Odd -- vowel. */

					if (q % 2)
					{
						strcat(tmp, get_syllable_vowel());
					}
					else
					{
						strcat(tmp, get_syllable_cons());
					}
				}

				/* Stop before getting too long */
				if (strlen(buf) + 1 + strlen(tmp) > 15)
					break;

				/* Add a space */
				strcat(buf, " ");

				/* Add the word */
				strcat(buf, tmp);
			}

			/* Save the title */
			strcpy(scroll_adj[i], buf + 1);

			/* Assume okay */
			okay = TRUE;

			/* Check for "duplicate" scroll titles */
			for (j = 0; j < i; j++)
			{
				cptr hack1 = scroll_adj[j];
				cptr hack2 = scroll_adj[i];

				/* Compare first four characters */
				if (*hack1++ != *hack2++)
					continue;
				if (*hack1++ != *hack2++)
					continue;
				if (*hack1++ != *hack2++)
					continue;
				if (*hack1++ != *hack2++)
					continue;

				/* Not okay */
				okay = FALSE;

				/* Stop looking */
				break;
			}

			/* Break when done */
			if (okay)
				break;
		}

		/* All scrolls are white */
		scroll_col[i] = TERM_WHITE;
	}


	/* Hack -- Use the "complex" RNG */
	Rand_quick = FALSE;

	/* Analyze every object */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name)
			continue;

		/* Check for a "flavor" */
		k_ptr->has_flavor = object_has_flavor(i);

		/* No flavor yields aware */
		if (!k_ptr->has_flavor)
			k_ptr->aware = TRUE;

		/* Check for "easily known" */
		k_ptr->easy_know = object_easy_know(i);
	}
}




/*
 * Extract the "default" attr for each object
 * This function is used only by "flavor_init()"
 */
static byte object_d_attr(int i)
{
	object_kind *k_ptr = &k_info[i];

	/* Flavored items */
	if (k_ptr->has_flavor)
	{
		/* Extract the indexx */
		int indexx = k_ptr->sval;

		/* Analyze the item */
		switch (k_ptr->tval)
		{
			case TV_FOOD:
				return (food_col[indexx % MAX_SHROOM]);
			case TV_POTION:
				return (potion_col[indexx % MAX_COLORS]);
			case TV_SCROLL:
				return (scroll_col[indexx % MAX_TITLES]);
			case TV_AMULET:
				return (amulet_col[indexx % MAX_AMULETS]);
			case TV_RING:
				return (ring_col[indexx % MAX_ROCKS]);
			case TV_STAFF:
				return (staff_col[indexx % MAX_WOODS]);
			case TV_WAND:
				return (wand_col[indexx % MAX_METALS]);
			case TV_ROD:
				return (rod_col[indexx % MAX_METALS]);
		}
	}

	/* Default attr if legal */
	if (k_ptr->k_attr)
		return (k_ptr->k_attr);

	/* Default to white */
	return (TERM_WHITE);
}


/*
 * Extract the "default" char for each object
 * This function is used only by "flavor_init()"
 */
static byte object_d_char(int i)
{
	object_kind *k_ptr = &k_info[i];

	return (k_ptr->k_char);
}


/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 */
void reset_visuals(bool prefs)
{
	int i;


	/* Extract some info about terrain features */
	for (i = 0; i < MAX_F_IDX; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->z_attr = f_ptr->f_attr;
		f_ptr->z_char = f_ptr->f_char;
	}

	/* Extract some info about objects */
	for (i = 0; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Extract the "underlying" attr */
		k_ptr->d_attr = object_d_attr(i);

		/* Extract the "underlying" char */
		k_ptr->d_char = object_d_char(i);

		/* Assume we will use the underlying values */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract some info about monsters */
	for (i = 0; i < MAX_R_IDX; i++)
	{
		/* Extract the "underlying" attr */
		r_info[i].x_attr = r_info[i].d_attr;

		/* Extract the "underlying" char */
		r_info[i].x_char = r_info[i].d_char;
	}


	/* Default to white elsewhere? XXX XXX XXX */
	/* Extract attr/chars for equippy items (by tval) */
	for (i = 0; i < 128; i++)
	{
		/* Extract a default attr */
		tval_to_attr[i] = default_tval_to_attr(i);
	}


	/* Process user pref files */
	if (prefs)
	{
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
	}
}





/*
 * Obtain the "flags" for an item
 */
void object_flags(object_type * o_ptr, u32b * f1, u32b * f2, u32b * f3)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Base object. */
	(*f1) = k_ptr->flags1;
	(*f2) = k_ptr->flags2;
	(*f3) = k_ptr->flags3;

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		(*f1) = a_ptr->flags1;
		(*f2) = a_ptr->flags2;
		(*f3) = a_ptr->flags3;
	}

	/* Ego-item */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		(*f1) |= e_ptr->flags1;
		(*f2) |= e_ptr->flags2;
		(*f3) |= e_ptr->flags3;
	}

	/* Weird flags specific to item. */
	(*f1) |= o_ptr->flags1;
	(*f2) |= o_ptr->flags2;
	(*f3) |= o_ptr->flags3;
}



/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(object_type * o_ptr, u32b * f1, u32b * f2,
	u32b * f3)
{
	bool spoil = FALSE;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Clear */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Must be identified */
	if (!object_known_p(o_ptr))
		return;

	/* Base object */
	(*f1) = k_ptr->flags1;
	(*f2) = k_ptr->flags2;
	(*f3) = k_ptr->flags3;

	/* Ego-item */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

		(*f1) |= e_ptr->flags1;
		(*f2) |= e_ptr->flags2;
		(*f3) |= e_ptr->flags3;
	}

#ifdef SPOIL_ARTIFACTS
	/* Full knowledge for some artifacts */
	if (artifact_p(o_ptr))
		spoil = TRUE;
#endif

#ifdef SPOIL_EGO_ITEMS
	/* Full knowledge for some ego-items */
	if (ego_item_p(o_ptr))
		spoil = TRUE;
#endif

	/* Need full knowledge or spoilers */
	if (!spoil && !(o_ptr->ident & IDENT_MENTAL))
		return;

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		(*f1) = a_ptr->flags1;
		(*f2) = a_ptr->flags2;
		(*f3) = a_ptr->flags3;
	}

	/* Full knowledge for *identified* objects */
	if (!(o_ptr->ident & IDENT_MENTAL))
		return;

	/* Weird flags. */
	(*f1) |= o_ptr->flags1;
	(*f2) |= o_ptr->flags2;
	(*f3) |= o_ptr->flags3;
}







/*
 * Print a char "c" into a string "t", as if by sprintf(t, "%c", c),
 * and return a pointer to the terminator (t + 1).
 */
static char *object_desc_chr(char *t, int *len, char c)
{
	/* Hack -- 80 char limit. */
	if (*len >= 79)
		return (t);

	/* Copy the char */
	*t++ = c;

	/* Terminate */
	*t = '\0';

	/* Increase length. */
	(*len)++;

	/* Result */
	return (t);
}


/*
 * Print a string "s" into a string "t", as if by strcpy(t, s),
 * and return a pointer to the terminator.
 */
static char *object_desc_str(char *t, int *len, cptr s)
{
	/* Hack -- 80 char limit. */
	if (*len >= 79)
		return (t);

	/* Copy the string */
	while (*s)
	{
		*t++ = *s++;
		(*len)++;

		if (*len >= 79)
			return (t);
	}

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}



/*
 * Print an unsigned number "n" into a string "t", as if by
 * sprintf(t, "%u", n), and return a pointer to the terminator.
 */
static char *object_desc_num(char *t, int *len, uint n)
{
	uint p;

	/* Hack -- 80 char limit. */
	if (*len >= 79)
		return (t);

	/* Find "size" of "n" */
	for (p = 1; n >= p * 10; p = p * 10) /* loop */ ;

	/* Dump each digit */
	while (p >= 1)
	{
		/* Dump the digit */
		*t++ = '0' + n / p;

		(*len)++;

		if (*len >= 79)
			return (t);

		/* Remove the digit */
		n = n % p;

		/* Process next digit */
		p = p / 10;
	}

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}




/*
 * Print an signed number "v" into a string "t", as if by
 * sprintf(t, "%+d", n), and return a pointer to the terminator.
 * Note that we always print a sign, either "+" or "-".
 */
static char *object_desc_int(char *t, int *len, sint v)
{
	uint p, n;

	/* Hack -- 80 char limit. */
	if (*len >= 79)
		return (t);

	/* Negative */
	if (v < 0)
	{
		/* Take the absolute value */
		n = 0 - v;

		/* Use a "minus" sign */
		*t++ = '-';

		(*len)++;

		if (*len >= 79)
			return (t);
	}

	/* Positive (or zero) */
	else
	{
		/* Use the actual number */
		n = v;

		/* Use a "plus" sign */
		*t++ = '+';

		(*len)++;

		if (*len >= 79)
			return (t);
	}

	/* Find "size" of "n" */
	for (p = 1; n >= p * 10; p = p * 10) /* loop */ ;

	/* Dump each digit */
	while (p >= 1)
	{
		/* Dump the digit */
		*t++ = '0' + n / p;

		(*len)++;

		if (*len >= 79)
			return (t);

		/* Remove the digit */
		n = n % p;

		/* Process next digit */
		p = p / 10;
	}

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}


/*
 * Return a short name for an equipment slot.
 */
cptr object_desc_slot_name(int i)
{
	cptr p;

	switch (i)
	{
		case EQUIP_WIELD:
			p = "Wield";
			break;
		case EQUIP_BOW:
			p = "Shoot";
			break;
		case EQUIP_LEFT:
			p = "Left";
			break;
		case EQUIP_RIGHT:
			p = "Right";
			break;
		case EQUIP_NECK:
			p = "Neck";
			break;
		case EQUIP_LITE:
			p = "Light";
			break;
		case EQUIP_BODY:
			p = "Body";
			break;
		case EQUIP_OUTER:
			p = "Cloak";
			break;
		case EQUIP_ARM:
			p = "Arm";
			break;
		case EQUIP_HEAD:
			p = "Head";
			break;
		case EQUIP_HANDS:
			p = "Hands";
			break;
		case EQUIP_FEET:
			p = "Feet";
			break;
		default:
			p = "Bug";
			break;
	}

	return p;
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
 * Also note that the 80 char limit is strictly enforced by the code, by
 * truncation if necessary.
 *
 * Note the use of "object_desc_num()" and "object_desc_int()" as hyper-efficient,
 * portable, versions of some common "sprintf()" commands.
 *
 * Note that all ego-items (when known) append an "Ego-Item Name", unless
 * the item is also an artifact, which should NEVER happen.
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
 * If "pref" then a "numeric" prefix will be pre-pended.
 *
 * Mode:
 *   0 -- The Cloak of Death
 *   1 -- The Cloak of Death [1,+3]
 *   2 -- The Cloak of Death [1,+3] (+2 to Stealth)
 *   3 -- The Cloak of Death [1,+3] (+2 to Stealth) {nifty}
 *
 */
void object_desc(char *buf, object_type * o_ptr, int pref, int mode)
{
	cptr basenm, modstr, art_name, mat_name;
	int power, indexx;

	bool aware = FALSE;
	bool known = FALSE;

	bool append_name = FALSE;

	bool show_weapon = FALSE;
	bool show_armour = FALSE;
	bool show_gold = FALSE;

	cptr s, u;
	char *t;

	char p1 = '(', p2 = ')';
	char b1 = '[', b2 = ']';
	char c1 = '{', c2 = '}';

	char tmp_val[160];

	u32b f1, f2, f3;

	object_kind *k_ptr;

	/* Save the "aware" flag */
	bool hack_aware;

	/* Save the "known" flag */
	bool hack_known;

	/* Current length of the description. */
	int len = 1;

	bool old_stuff = FALSE;

	/* Ugly hack -- catch NULL items. */
	if (!o_ptr)
	{
		strcpy(buf, "(nothing)");

		/* Clear object_desc_mode */
		object_desc_mode = 0;

		return;
	}

	hack_aware = k_info[o_ptr->k_idx].aware;
	hack_known = (o_ptr->ident & (IDENT_KNOWN)) ? TRUE : FALSE;

	/* Describe items in stores. */
	if (o_ptr->world == WORLD_STORE)
	{
		/* Set the "known" flag */
		o_ptr->ident |= (IDENT_KNOWN);

		/* Force "aware" for description */
		k_info[o_ptr->k_idx].aware = TRUE;
	}

	k_ptr = &k_info[o_ptr->k_idx];

	/* Give silly desciptions for hallucinations */
	if (p_ptr->image)
	{
		strcpy(buf, get_random_line("silly_it.txt"));
		goto end;
	}

	/* Extract some flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* See if the object is "aware" */
	if (object_aware_p(o_ptr))
		aware = TRUE;

	/* See if the object is "known" */
	if (object_known_p(o_ptr))
		known = TRUE;

	/* Hack -- Extract the sub-type "indexx" */
	indexx = o_ptr->sval;

	/* Extract default "base" string */
	basenm = (k_name + k_ptr->name);

	/* Assume no "modifier" string */
	modstr = "";

	/* No artifact/ego item name. */
	art_name = NULL;

	/* No material name. */
	mat_name = NULL;

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
		case TV_INGRED:
		{
			break;
		}


			/* Missiles/ Bows/ Weapons */
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
			/* Known artifacts */
			if (artifact_p(o_ptr) && aware)
				break;

			/* Color the object */
			modstr = amulet_adj[indexx % MAX_AMULETS];
			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Amulet~" : "& # Amulet~";

			/* Hack -- The Amulet of Yendor */
			if (!aware && (o_ptr->sval == SV_AMULET_YENDOR)) {
			  modstr = "Plain Gold";
			}

			break;
		}


			/* Rings (including a few "Specials") */
		case TV_RING:
		{
			/* Known artifacts */
			if (artifact_p(o_ptr) && aware)
				break;

			/* Color the object */
			modstr = ring_adj[indexx % MAX_ROCKS];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Ring~" : "& # Ring~";

			/* Hack -- The One Ring */
			if (!aware && (o_ptr->sval == SV_RING_POWER))
				modstr = "Plain Gold";

			break;
		}


		case TV_STAFF:
		{
			/* Color the object */
			modstr = staff_adj[indexx % MAX_WOODS];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Staff~" : "& # Staff~";
			break;
		}

		case TV_WAND:
		{
			/* Color the object */
			modstr = wand_adj[indexx % MAX_METALS];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Wand~" : "& # Wand~";
			break;
		}

		case TV_ROD:
		{
			/* Color the object */
			modstr = rod_adj[indexx % MAX_METALS];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Rod~" : "& # Rod~";

			break;
		}

		case TV_SCROLL:
		{
			/* Color the object */
			modstr = scroll_adj[indexx % MAX_TITLES];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Scroll~" : "& Scroll~ titled \"#\"";
			break;
		}

		case TV_POTION:
		{
			/* Color the object */
			modstr = potion_adj[indexx % MAX_COLORS];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Potion~" : "& # Potion~";
			break;
		}

		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (o_ptr->sval >= SV_FOOD_MIN_FOOD)
				break;

			/* Color the object */
			modstr = food_adj[indexx % MAX_SHROOM];

			if (aware)
				append_name = TRUE;
			basenm = aware ? "& Mushroom~" : "& # Mushroom~";
			break;
		}


			/* Magic Books */
		case TV_SPELLBOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of #";
			break;
		}

		case TV_MIMIC_BOOK:
		{
			modstr = basenm;
			basenm = "& Book~ of Lore #";
			break;
		}

		case TV_TEXT:
		{
			modstr = basenm;
			basenm = "& Parchment~ titled \"#\"";
			break;
		}

		case TV_CORPSE:
		{
			monster_race *r_ptr = &r_info[o_ptr->pval];

			modstr = r_name + r_ptr->name;
			break;
		}

		case TV_RANDART:
		{
			modstr = basenm;

			if (known)
			{
				basenm = random_artifacts[indexx].name_full;
			}
			else
			{
				basenm = random_artifacts[indexx].name_short;
			}
			break;
		}

			/* Hack -- Gold/Gems */
		case TV_GOLD:
		{

			show_gold = TRUE;
			break;
		}

			/* Used in the "inventory" routine */
		default:
		{
			strcpy(buf, "(nothing)");
			goto end;
		}
	}


	/* Put the artifact/ego name in the prefix. */
	if (f1 & TR1_PREFIX_NAME)
	{
		/* Grab any artifact name */
		if (o_ptr->name1)
		{
			art_name = a_name + a_info[o_ptr->name1].name;
		}

		/* Grab any ego-item name */
		else if (o_ptr->name2)
		{
			art_name = e_name + e_info[o_ptr->name2].name;
		}
	}


	if (f1 & TR1_TRANSMUTE)
	{
		if (o_ptr->name2 && o_ptr->stuff != e_info[o_ptr->name2].stuff)
		{
			old_stuff = TRUE;
		}
	}
	else
	{
		if (o_ptr->stuff != k_ptr->stuff)
		{
			old_stuff = TRUE;
		}
	}

	if (old_stuff)
	{
		mat_name = materials[o_ptr->stuff].name;
	}

	/* Start dumping the result */
	t = buf;

	/* The object "expects" a "number" */
	if (basenm[0] == '&')
	{
		cptr first_word;

		/* Skip the ampersand (and space) */
		s = basenm + 2;

		/* No prefix */
		if (!pref || k_ptr->flags3 & TR3_DESC_SIMPLE)
		{
			/* Nothing */
		}

		/* Hack -- None left */
		else if (o_ptr->number <= 0)
		{
			t = object_desc_str(t, &len, "no more ");
		}

		/* Extract the number */
		else if (o_ptr->number > 1)
		{
			t = object_desc_num(t, &len, o_ptr->number);
			t = object_desc_chr(t, &len, ' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && artifact_p(o_ptr))
		{
			t = object_desc_str(t, &len, "The ");
		}

		else
		{
			/* Figure out what the first word in the desc. is going to be. */
			if (art_name && known)
			{
				first_word = art_name;
			}
			else if (mat_name)
			{
				first_word = mat_name;
			}
			else if (*s == '#')
			{
				first_word = modstr;
			}
			else
			{
				first_word = s;
			}

			/* The first letter is a vowel. */
			if (is_a_vowel(first_word[0]))
			{
				t = object_desc_str(t, &len, "an ");
			}
			else
			{
				t = object_desc_str(t, &len, "a ");
			}
		}
	}

	/* Hack -- objects that "never" take an article */
	else
	{
		/* No ampersand */
		s = basenm;

		/* No pref */
		if (!pref || k_ptr->flags3 & TR3_DESC_SIMPLE)
		{
			/* Nothing */
		}

		/* Hack -- all gone */
		else if (o_ptr->number <= 0)
		{
			t = object_desc_str(t, &len, "no more ");
		}

		/* Prefix a number if required */
		else if (o_ptr->number > 1)
		{
			t = object_desc_num(t, &len, o_ptr->number);
			t = object_desc_chr(t, &len, ' ');
		}

		/* Hack -- The only one of its kind */
		else if (known && artifact_p(o_ptr))
		{
			t = object_desc_str(t, &len, "The ");
		}

		/* Hack -- single items get no prefix */
		else
		{
			/* Nothing */
		}
	}

	/* Paranoia -- skip illegal tildes */
	/* while (*s == '~') s++; */


	/* Copy the artifact/ego name. */
	if (known && art_name)
	{
		t = object_desc_str(t, &len, art_name);
		t = object_desc_chr(t, &len, ' ');
	}

	/* Copy the material name. */
	if (mat_name)
	{
		t = object_desc_str(t, &len, mat_name);
		t = object_desc_chr(t, &len, ' ');
	}


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

				/* XXX XXX XXX Mega-Hack */

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h'))
					*t++ = 'e';

				/* Add an 's' */
				*t++ = 's';
			}
		}

		/* Modifier */
		else if (*s == '#')
		{
			/* Insert the modifier */
			for (u = modstr; *u; u++)
				*t++ = *u;
		}

		/* Normal */
		else
		{
			/* Copy */
			*t++ = *s;
		}
	}

	/* Terminate */
	*t = '\0';


	/* Append the "kind name" to the "base name" */
	if (append_name)
	{
		t = object_desc_str(t, &len, " of ");
		t = object_desc_str(t, &len, (k_name + k_ptr->name));
	}


	/* Hack -- Append "Artifact" or "Special" names */
	if (known && !art_name)
	{
		/* Grab any artifact name */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			t = object_desc_chr(t, &len, ' ');
			t = object_desc_str(t, &len, (a_name + a_ptr->name));
		}

		/* Grab any ego-item name */
		else if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			t = object_desc_chr(t, &len, ' ');
			t = object_desc_str(t, &len, (e_name + e_ptr->name));
		}
	}

	/* Hack -- player is in a vault store. */
	if (o_ptr->world == WORLD_STORE)
	{
		show_gold = TRUE;
	}

	/* Hack -- Don't show gold at all */
	if (object_desc_mode & ODESC_GOLD)
		show_gold = FALSE;

	/* Hack -- show value. */
	if (show_gold)
	{
		t = object_desc_str(t, &len, " (");
		t = object_desc_num(t, &len, object_store_value(o_ptr));
		t = object_desc_str(t, &len, " gp)");
	}

	/* Hack -- append equipment slot. */
	if (o_ptr->stack == STACK_INVEN)
	{
		if (!(object_desc_mode & ODESC_SLOT))
		{
			int foo;

			for (foo = 0; foo < EQUIP_MAX; foo++)
			{
				if (o_ptr == equipment[foo])
				{
					t = object_desc_str(t, &len, " <");
					t =
						object_desc_str(t, &len,
						object_desc_slot_name(foo));
					t = object_desc_str(t, &len, ">");
					break;
				}
			}
		}
	}

	/* No more details wanted */
	if (mode < 1)
		goto end;


	/* Hack -- Chests must be described in detail */
	if (o_ptr->tval == TV_CHEST)
	{
		/* Not searched yet */
		if (!known)
		{
			/* Nothing */
		}

		/* May be "empty" */
		else if (!o_ptr->pval)
		{
			t = object_desc_str(t, &len, " (empty)");
		}

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
		{
			if (chest_traps[o_ptr->pval])
			{
				t = object_desc_str(t, &len, " (disarmed)");
			}
			else
			{
				t = object_desc_str(t, &len, " (unlocked)");
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
					t = object_desc_str(t, &len, " (Locked)");
					break;
				}
				case CHEST_LOSE_STR:
				{
					t = object_desc_str(t, &len, " (Poison Needle)");
					break;
				}
				case CHEST_LOSE_CON:
				{
					t = object_desc_str(t, &len, " (Poison Needle)");
					break;
				}
				case CHEST_POISON:
				{
					t = object_desc_str(t, &len, " (Gas Trap)");
					break;
				}
				case CHEST_PARALYZE:
				{
					t = object_desc_str(t, &len, " (Gas Trap)");
					break;
				}
				case CHEST_EXPLODE:
				{
					t = object_desc_str(t, &len, " (Explosion Device)");
					break;
				}
				case CHEST_SUMMON:
				{
					t = object_desc_str(t, &len, " (Summoning Runes)");
					break;
				}
				default:
				{
					t = object_desc_str(t, &len, " (Multiple Traps)");
					break;
				}
			}
		}
	}


	/* Display the item like a weapon */
	if (f3 & (TR3_SHOW_MODS))
		show_weapon = TRUE;

	/* Display the item like a weapon */
	if (o_ptr->to_h && o_ptr->to_d)
		show_weapon = TRUE;

	/* Display the item like armour */
	if (o_ptr->ac)
		show_armour = TRUE;


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
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, p1);
			t = object_desc_num(t, &len, o_ptr->dd);
			t = object_desc_chr(t, &len, 'd');
			t = object_desc_num(t, &len, o_ptr->ds);
			t = object_desc_chr(t, &len, p2);

			/* All done */
			break;
		}

			/* Bows get a special "damage string" */
		case TV_BOW:
		{
			/* Hack -- Extract the "base power" */
			power = (o_ptr->sval % 10);

			/* Append a "power" string */
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, p1);
			t = object_desc_chr(t, &len, 'x');
			t = object_desc_num(t, &len, power);
			t = object_desc_chr(t, &len, p2);

			/* All done */
			break;
		}
	}


	/* Add the weapon bonuses */
	if (known)
	{
		/* Show the tohit/todam on request */
		if (show_weapon)
		{
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, p1);
			t = object_desc_int(t, &len, o_ptr->to_h);
			t = object_desc_chr(t, &len, ',');
			t = object_desc_int(t, &len, o_ptr->to_d);
			t = object_desc_chr(t, &len, p2);
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, p1);
			t = object_desc_int(t, &len, o_ptr->to_h);
			t = object_desc_chr(t, &len, p2);
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, p1);
			t = object_desc_int(t, &len, o_ptr->to_d);
			t = object_desc_chr(t, &len, p2);
		}
	}


	/* Add the armor bonuses */
	if (known)
	{
		/* Show the armor class info */
		if (show_armour)
		{
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, b1);
			t = object_desc_num(t, &len, o_ptr->ac);
			t = object_desc_chr(t, &len, ',');
			t = object_desc_int(t, &len, o_ptr->to_a);
			t = object_desc_chr(t, &len, b2);
		}

		/* No base armor, but does increase armor */
		else if (o_ptr->to_a)
		{
			t = object_desc_chr(t, &len, ' ');
			t = object_desc_chr(t, &len, b1);
			t = object_desc_int(t, &len, o_ptr->to_a);
			t = object_desc_chr(t, &len, b2);
		}

	}

	/* Hack -- always show base armor */
	else if (show_armour)
	{
		t = object_desc_chr(t, &len, ' ');
		t = object_desc_chr(t, &len, b1);
		t = object_desc_num(t, &len, o_ptr->ac);
		t = object_desc_chr(t, &len, b2);
	}


	/* No more details wanted */
	if (mode < 2)
		goto end;


	/* Hack -- Wands and Staffs have charges */
	if (known && ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		t = object_desc_chr(t, &len, ' ');
		t = object_desc_chr(t, &len, p1);
		t = object_desc_num(t, &len, o_ptr->pval);
		t = object_desc_str(t, &len, " charge");
		if (o_ptr->pval != 1)
			t = object_desc_chr(t, &len, 's');
		t = object_desc_chr(t, &len, p2);
	}


	/* Hack -- Process Lanterns/Torches */
	else if ((o_ptr->tval == TV_LITE) && (!artifact_p(o_ptr)))
	{
		/* Hack -- Turns of light for normal lites */
		t = object_desc_str(t, &len, " (");
		t = object_desc_num(t, &len, o_ptr->pval);
		t = object_desc_str(t, &len, " turns)");
	}

	/* Dump "pval" flags for wearable items */
	if (known && (f1 & (TR1_PVAL_MASK) || f2 & (TR2_PVAL_MASK)))
	{
		/* Start the display */
		t = object_desc_chr(t, &len, ' ');
		t = object_desc_chr(t, &len, p1);

		/* Dump the "pval" itself */
		t = object_desc_int(t, &len, o_ptr->pval);

		/* Do not display the "pval" flags */
		if (f3 & (TR3_HIDE_TYPE))
		{
			/* Nothing */
		}
		else
		{

			/* Ugly hack -- don't put leading comma. */
			bool leading_comma = TRUE;

			if (f2 & (TR2_PLUS_TO_AC))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to AC");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", AC");
				}
			}

			if (f2 & (TR2_PLUS_TO_DAM))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to damage");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", damage");
				}
			}

			if (f2 & (TR2_PLUS_TO_HIT))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to accuracy");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", accuracy");
				}
			}

			/* Stealth */
			if (f1 & (TR1_STEALTH))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to stealth");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", stealth");
				}
			}

			/* Searching */
			if (f1 & (TR1_SEARCH))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to searching");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", searching");
				}
			}

			/* Infravision */
			if (f1 & (TR1_INFRA))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to infra");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", infra");
				}
			}

#if 0

			/* Tunneling */
			if (f1 & (TR1_TUNNEL))
				/* Dump " to digging" */
				t = object_desc_str(t, &len, " to digging");

#endif

			/* Speed */
			if (f1 & (TR1_SPEED))
			{
				if (leading_comma)
				{
					t = object_desc_str(t, &len, " to speed");
					leading_comma = FALSE;

				}
				else
				{
					t = object_desc_str(t, &len, ", speed");
				}
			}

			/* Blows */
			if (f1 & (TR1_BLOWS))
			{
				t = object_desc_str(t, &len, " attack");
				leading_comma = FALSE;

				/* Add "attacks" */
				if (ABS(o_ptr->pval) != 1)
					t = object_desc_chr(t, &len, 's');
			}

#if 0

			/* Shots */
			if (f1 & (TR1_SHOTS))
			{
				/* Nothing */
			}

			/* Might */
			if (f1 & (TR1_MIGHT))
			{
				/* Nothing */
			}

#endif

		}

		/* Finish the display */
		t = object_desc_chr(t, &len, p2);
	}


	/* Indicate "charging" artifacts XXX XXX XXX */
	if (known && o_ptr->timeout)
	{
		/* Hack -- Dump " (charging)" if relevant */
		t = object_desc_str(t, &len, " (charging)");
	}

	/* Fate indicator. */
	switch (o_ptr->fate) {
	case FATE_NONE:
	  break;

	case FATE_KILL:
	  t = object_desc_str(t, &len, " (fate: destroy)");
	  break;

	case FATE_SAVE:
	  t = object_desc_str(t, &len, " (fate: save)");
	  break;

	case FATE_USE:
	  t = object_desc_str(t, &len, " (fate: use)");
	  break;

	case FATE_UNUSE:
	  t = object_desc_str(t, &len, " (fate: avoid)");
	  break;

	case FATE_CARRY:
	  t = object_desc_str(t, &len, " (fate: carry)");
	  break;

	case FATE_DROP:
	  t = object_desc_str(t, &len, " (fate: drop)");
	  break;

	case FATE_SACRIFICE:
	  t = object_desc_str(t, &len, " (fate: sacrifice)");
	  break;

	default:
	  t = object_desc_str(t, &len, " (fate: DEBUG)");
	  break;
	}


	/* No more details wanted */
	if (mode < 3)
		goto end;


	/* No inscription yet */
	tmp_val[0] = '\0';

	/* Use the standard inscription if available */
	if (o_ptr->note)
	{
		strcpy(tmp_val, quark_str(o_ptr->note));
	}

	/* Note "cursed" if the item is known to be cursed */
	else if (cursed_p(o_ptr) && (known || (o_ptr->ident & (IDENT_SENSE))))
	{
		strcpy(tmp_val, "cursed");
	}

	/* Mega-Hack -- note empty wands/staffs */
	else if (!known && (o_ptr->ident & (IDENT_EMPTY)))
	{
		strcpy(tmp_val, "empty");
	}

	/* Note "tried" if the object has been tested unsuccessfully */
	else if (!aware && object_tried_p(o_ptr))
	{
		strcpy(tmp_val, "tried");
	}

	/* Note the discount, if any */
	else if (o_ptr->discount)
	{
		object_desc_num(tmp_val, &len, o_ptr->discount);
		strcat(tmp_val, "% off");
	}

	/* Append the inscription, if any */
	if (tmp_val[0])
	{
		/* Append the inscription */
		t = object_desc_chr(t, &len, ' ');
		t = object_desc_chr(t, &len, c1);
		t = object_desc_str(t, &len, tmp_val);
		t = object_desc_chr(t, &len, c2);
	}


  end:
	/* Paranoia. */
	buf[79] = '\0';

	if (o_ptr->world == WORLD_STORE)
	{
		/* Restore "aware" flag */
		k_info[o_ptr->k_idx].aware = hack_aware;

		/* Clear the known flag */
		if (!hack_known)
			o_ptr->ident &= ~(IDENT_KNOWN);
	}

	/* Clear object_desc_mode */
	object_desc_mode = 0;
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, object_type * o_ptr, int pref, int mode)
{
	/* Save the "aware" flag */
	bool hack_aware = k_info[o_ptr->k_idx].aware;

	/* Save the "known" flag */
	bool hack_known = (o_ptr->ident & (IDENT_KNOWN)) ? TRUE : FALSE;


	/* Set the "known" flag */
	o_ptr->ident |= (IDENT_KNOWN);

	/* Force "aware" for description */
	k_info[o_ptr->k_idx].aware = TRUE;


	/* Describe the object */
	object_desc(buf, o_ptr, pref, mode);


	/* Restore "aware" flag */
	k_info[o_ptr->k_idx].aware = hack_aware;

	/* Clear the known flag */
	if (!hack_known)
		o_ptr->ident &= ~(IDENT_KNOWN);
}




/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
bool item_activation(object_type * o_ptr, char *buff)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	if (!(f3 & TR3_ACTIVATE))
		return FALSE;

	/* Random artifacts */
	if (o_ptr->tval == TV_RANDART)
	{
		random_artifact *ra_ptr = &random_artifacts[o_ptr->sval];
		spell *s_ptr = &activations[ra_ptr->activation];

		sprintf(buff, "%s (%s) every %d or so turns.", s_ptr->name,
			s_ptr->desc, ra_ptr->level * 5);

		return TRUE;

	}
	else if (artifact_p(o_ptr))
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];
		spell *s_ptr = &activations[a_ptr->activation];

		sprintf(buff, "%s (%s) every d%d+%d turns.", s_ptr->name,
			s_ptr->desc, a_ptr->timeout_rand, a_ptr->timeout_static);

		return TRUE;

	}
	else
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];
		spell *s_ptr = &activations[k_ptr->activation];

		sprintf(buff, "%s (%s) every d%d+%d turns.", s_ptr->name,
			s_ptr->desc, k_ptr->timeout_rand, k_ptr->timeout_static);

		return TRUE;
	}


	return FALSE;
}


/*
 * Describe damage status of an object for humans to see.
 */
cptr damage_status(object_type * o_ptr)
{
	int foo = (100 * o_ptr->chp) / o_ptr->mhp;
	int tv = o_ptr->tval;


	if (foo >= 100)
	{
		switch (tv)
		{
			case TV_FOOD:
			case TV_CORPSE:
				return "fresh";

			default:
				return "undamaged";
		}

	}
	else if (foo >= 80)
	{
		switch (tv)
		{
			case TV_FOOD:
			case TV_CORPSE:
				return "a little stale";

			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_CHEST:
			case TV_LITE:
			case TV_AMULET:
			case TV_RING:
			case TV_STAFF:
			case TV_WAND:
			case TV_ROD:
			case TV_FLASK:
				return "a little scratched";

			case TV_SPELLBOOK:
			case TV_SCROLL:
			case TV_MIMIC_BOOK:
			case TV_TEXT:
				return "a little crumpled";

			default:
				return "slightly used";
		}

	}
	else if (foo >= 60)
	{
		switch (tv)
		{
			case TV_FOOD:
			case TV_CORPSE:
				return "stale";

			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_CHEST:
			case TV_LITE:
			case TV_AMULET:
			case TV_RING:
			case TV_STAFF:
			case TV_WAND:
			case TV_ROD:
			case TV_FLASK:
				return "scratched";

			case TV_SPELLBOOK:
			case TV_SCROLL:
			case TV_MIMIC_BOOK:
			case TV_TEXT:
				return "crumpled";

			default:
				return "a little scratched";
		}

	}
	else if (foo >= 40)
	{
		switch (tv)
		{
			case TV_FOOD:
			case TV_CORPSE:
				return "a little rotten";
				break;

			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_CHEST:
			case TV_LITE:
			case TV_AMULET:
			case TV_RING:
			case TV_STAFF:
			case TV_WAND:
			case TV_ROD:
			case TV_FLASK:
				return "greatly scratched";

			case TV_SPELLBOOK:
			case TV_SCROLL:
			case TV_MIMIC_BOOK:
			case TV_TEXT:
				return "a little ripped";

			default:
				return "dented";
		}

	}
	else if (foo >= 20)
	{
		switch (tv)
		{
			case TV_FOOD:
			case TV_CORPSE:
				return "rotten";

			case TV_SPELLBOOK:
			case TV_SCROLL:
			case TV_MIMIC_BOOK:
			case TV_TEXT:
				return "ripped";

			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_CHEST:
			case TV_LITE:
			case TV_AMULET:
			case TV_RING:
			case TV_STAFF:
			case TV_WAND:
			case TV_ROD:
			case TV_FLASK:
				return "bent out of shape";

			default:
				return "damaged";
		}

	}
	else if (foo >= 0)
	{
		switch (tv)
		{
			case TV_FOOD:
			case TV_CORPSE:
				return "to be filled with maggots";

			case TV_SPELLBOOK:
			case TV_SCROLL:
			case TV_MIMIC_BOOK:
			case TV_TEXT:
				return "ripped to shreds";

			case TV_SKELETON:
			case TV_BOTTLE:
			case TV_JUNK:
			case TV_CHEST:
			case TV_LITE:
			case TV_AMULET:
			case TV_RING:
			case TV_STAFF:
			case TV_WAND:
			case TV_ROD:
			case TV_FLASK:
				return "cracked in places";

			default:
				return "greatly damaged";
		}
	}

	return NULL;
}






/*
 * Describe a "fully identified" item
 */
void identify_fully_aux(object_type * o_ptr)
{
	int i = 0, j, k;

	u32b f1, f2, f3;

	cptr info[128];
	char buff[80];
	char buff2[80];
	char buff3[80];
	cptr tmp;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);


	/* Show material. */
	sprintf(buff3, "%s %s made of %s.",
		(o_ptr->number > 1 ? "They" : "It"),
		(o_ptr->number > 1 ? "are" : "is"),
		materials[o_ptr->stuff].made_of_name);

	info[i++] = buff3;


	/* Show damage status. */
	tmp = damage_status(o_ptr);

	if (tmp)
	{
		sprintf(buff2, "%s look%s %s.",
			(o_ptr->number > 1 ? "They" : "It"),
			(o_ptr->number > 1 ? "" : "s"), tmp);

		info[i++] = buff2;
	}

	if (!(o_ptr->ident & IDENT_MENTAL))
		goto done;


	/* Mega-Hack -- describe activation */
	if (item_activation(o_ptr, buff))
	{
		info[i++] = "It can be activated for...";
		info[i++] = buff;

		/* Mega-hack -- get rid of useless line for randarts */
		if (o_ptr->tval != TV_RANDART)
		{
			info[i++] = "...if it is being worn.";
		}
	}


	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE)
	{
		if (o_ptr->sval == SV_LITE_UNDEATH)
		{
			info[i++] = "It provides no light whatsoever.";
		}

		else if (artifact_p(o_ptr))
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

	if (f2 & (TR2_PLUS_TO_AC))
	{
		info[i++] = "It affects your armor class.";
	}

	if (f2 & (TR2_PLUS_TO_DAM))
	{
		info[i++] = "It affects your weapon deadliness.";
	}

	if (f2 & (TR2_PLUS_TO_HIT))
	{
		info[i++] = "It affects your weapon accuracy.";
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

	if (f1 & (TR1_BRAND_ACID))
	{
		info[i++] = "It does extra damage from acid.";
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

	if (f1 & (TR1_BRAND_POIS)) /* From GJW  -KMW- */
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

	if (f2 & (TR2_IM_ACID))
	{
		info[i++] = "It provides immunity to acid.";
	}
	else if (f2 & (TR2_RES_ACID))
	{
		info[i++] = "It provides resistance to acid.";
	}

	if (f2 & (TR2_IM_ELEC))
	{
		info[i++] = "It provides immunity to electricity.";
	}
	else if (f2 & (TR2_RES_ELEC))
	{
		info[i++] = "It provides resistance to electricity.";
	}

	if (f2 & (TR2_IM_FIRE))
	{
		info[i++] = "It provides immunity to fire.";
	}
	else if (f2 & (TR2_RES_FIRE))
	{
		info[i++] = "It provides resistance to fire.";
	}

	if (f2 & (TR2_IM_COLD))
	{
		info[i++] = "It provides immunity to cold.";
	}
	else if (f2 & (TR2_RES_COLD))
	{
		info[i++] = "It provides resistance to cold.";
	}

	if (f2 & (TR2_RES_POIS))
	{
		info[i++] = "It provides resistance to poison.";
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

	if (f3 & (TR3_TELEPORT))
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

	if (f3 & (TR3_MUNCHKINISH))
	{
		info[i++] = "It grants ghostly powers.";
	}

	if (f3 & (TR3_WEIRD_ATTACK))
	{
		info[i++] = "It causes strange things to happen when attacking.";
	}

	if (f3 & TR3_FLYING)
	{
		info[i++] = "It grants levitation.";
	}

	if (f3 & TR3_VAMPIRIC)
	{
		info[i++] = "It grants vampirism.";
	}

	if (f3 & (TR3_DRAIN_EXP))
	{
		info[i++] = "It drains experience.";
	}

	if (f3 & (TR3_BLESSED))
	{
		info[i++] = "It has been blessed by the gods.";
	}

	if (cursed_p(o_ptr))
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

  done:

	/* Save the screen */
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
		if ((k == 22) && (j + 1 < i))
		{
			prt("-- more --", k, 0);
			inkey();

			/* Erase the screen */
			Term_clear();

			/* Start at the top */
			k = 2;

			/* Label the information */
			prt("     Item Attributes:", 1, 0);
		}
	}

	/* Wait for it */
	prt("[Press any key to continue]", k, 0);
	inkey();

	/* Restore the screen */
	screen_load();

	/* Gave knowledge */
	return;
}






/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(object_type * o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			return (EQUIP_WIELD);
		}

		case TV_BOW:
		{
			return (EQUIP_BOW);
		}

		case TV_RING:
		{
			/* Use the right hand first */
			if (!equipment[EQUIP_RIGHT])
				return (EQUIP_RIGHT);

			/* Use the left hand for swapping (by default) */
			return (EQUIP_LEFT);
		}

		case TV_AMULET:
		{
			return (EQUIP_NECK);
		}

		case TV_LITE:
		{
			return (EQUIP_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
			return (EQUIP_BODY);
		}

		case TV_CLOAK:
		{
			return (EQUIP_OUTER);
		}

		case TV_SHIELD:
		{
			return (EQUIP_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			return (EQUIP_HEAD);
		}

		case TV_GLOVES:
		{
			return (EQUIP_HANDS);
		}

		case TV_BOOTS:
		{
			return (EQUIP_FEET);
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
		case EQUIP_WIELD:
			p = "Wielding";
			break;
		case EQUIP_BOW:
			p = "Shooting";
			break;
		case EQUIP_LEFT:
			p = "On left hand";
			break;
		case EQUIP_RIGHT:
			p = "On right hand";
			break;
		case EQUIP_NECK:
			p = "Around neck";
			break;
		case EQUIP_LITE:
			p = "Light source";
			break;
		case EQUIP_BODY:
			p = "On body";
			break;
		case EQUIP_OUTER:
			p = "About body";
			break;
		case EQUIP_ARM:
			p = "On arm";
			break;
		case EQUIP_HEAD:
			p = "On head";
			break;
		case EQUIP_HANDS:
			p = "On hands";
			break;
		case EQUIP_FEET:
			p = "On feet";
			break;
		default:
			p = "Software Bug";
			break;
	}

	/* Hack -- Heavy weapon */
	if (i == EQUIP_WIELD)
	{
		object_type *o_ptr = equipment[i];

		if (o_ptr &&
			adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == EQUIP_BOW)
	{
		object_type *o_ptr = equipment[i];

		if (o_ptr &&
			adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
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
		case EQUIP_WIELD:
			p = "attacking monsters with";
			break;
		case EQUIP_BOW:
			p = "shooting missiles with";
			break;
		case EQUIP_LEFT:
			p = "wearing on your left hand";
			break;
		case EQUIP_RIGHT:
			p = "wearing on your right hand";
			break;
		case EQUIP_NECK:
			p = "wearing around your neck";
			break;
		case EQUIP_LITE:
			p = "using to light the way";
			break;
		case EQUIP_BODY:
			p = "wearing on your body";
			break;
		case EQUIP_OUTER:
			p = "wearing on your back";
			break;
		case EQUIP_ARM:
			p = "wearing on your arm";
			break;
		case EQUIP_HEAD:
			p = "wearing on your head";
			break;
		case EQUIP_HANDS:
			p = "wearing on your hands";
			break;
		case EQUIP_FEET:
			p = "wearing on your feet";
			break;
		default:
			p = "debugging with";
			break;
	}

	/* Hack -- Heavy weapon */
	if (i == EQUIP_WIELD)
	{
		object_type *o_ptr = equipment[i];

		if (o_ptr &&
			adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
		{
			p = "just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == EQUIP_BOW)
	{
		object_type *o_ptr = equipment[i];

		if (o_ptr &&
			adj_str_hold[p_ptr->stat_ind[A_STR]] < o_ptr->weight / 10)
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
bool item_tester_okay(object_type * o_ptr)
{
	/* Hack -- allow listing empty slots */
	if (item_tester_full)
		return (TRUE);

	/* Require an item */
	if (!o_ptr->k_idx)
		return (FALSE);

	/* Check the tval */
	if (item_tester_tval)
	{
		if (!(item_tester_tval == o_ptr->tval))
			return (FALSE);
	}

	/* Check the hook */
	if (item_tester_hook)
	{
		if (!(*item_tester_hook) (o_ptr))
			return (FALSE);
	}

	/* Assume okay */
	return (TRUE);
}



/*
 * Return the size of the stack, using ``item_tester_tval'' and 
 * ``item_tester_hook''.
 *
 * If ``glob'' is true, use the global list instead of the local one.
 */
s16b size_stack(object_type * stack, bool glob)
{
	object_type *o_ptr;
	int ret = 0;

	/* Find the maximum line length. */
	for (o_ptr = stack; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (item_tester_okay(o_ptr))
			ret++;
	}

	return ret;
}

/*
 * Print a stack of items to the screen.
 *
 * ``item_tester_tval'' and ``item_tester_hook'' will be used to filter
 * the stack.
 *
 * If ``glob'' is true, then show the global list instead of the local one.
 */

void show_stack(object_type * stack, bool glob)
{

	object_type *o_ptr;

	int i, len, lim, l;

	char tmp_val[80];
	char o_name[80];
	char o_n_buff[23][80];

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for weight (if needed) */
	if (show_weights)
		lim -= 9;

	i = 0;
	len = 0;

	/* Find the maximum line length. */
	for (o_ptr = stack; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr))
			continue;

		/* Ran out of screen space. */
		if (i == 22)
			break;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		strcpy(o_n_buff[i], o_name);

		/* Find the predicted "line length" */
		if (!glob)
		{
			l = strlen(o_name) + 5;

			/* Be sure to account for the weight */
			if (show_weights)
				l += 9;

			/* Maintain the maximum length */
			if (l > len)
				len = l;
		}

		i++;
	}

	/* Find the column to start in */
	len = (len > 76) ? 0 : (79 - len);

	i = 0;

	/* Actually output the stack. */
	for (o_ptr = stack; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr))
			continue;

		/* Ran out of screen space. */
		if (i == 22)
			break;

		/* Prepare an index --(-- */
		sprintf(tmp_val, " %c) ", ((o_ptr->stack == STACK_INVEN &&
					conserve_slots) ? o_ptr->tag : I2A(i)));

		/* Clear the line with the (possibly indented) index */
		prt(tmp_val, i + 1, (glob ? 0 : len));

		/* Display the entry itself */
		c_put_str(tval_to_attr[o_ptr->tval & 0x7F], o_n_buff[i], i + 1,
			(glob ? 4 : len + 4));

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight / o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, i + 1, 71);
		}

		i++;
	}

	/* Make a "shadow" below the list. */
	prt("", i + 1, (glob ? 1 : len));
}




/*
 * Print the player's equipment to the screen.
 */

void show_equip(void)
{
	object_type *o_ptr;

	int i, len, lim, l;

	char tmp_val[80];
	char o_name[80];
	char o_n_buff[23][80];

	byte attr;

	/* Maximum space allowed for descriptions */
	lim = screen_x - 4;

	/* Require space for labels (if needed) */
	if (show_labels)
		lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (show_weights)
		lim -= 9;

	/* Hack -- Only 80 chars in description allowed */
	if (lim > 79)
		lim = 79;

	i = 0;
	len = 0;

	/* Find the maximum line length. */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = equipment[i];

		/* XXX Hack -- Do not display slot in description */
		if (show_labels)
			object_desc_mode |= ODESC_SLOT;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		strcpy(o_n_buff[i], o_name);

		/* Find the predicted "line length" */
		l = strlen(o_name) + 5;

		/* Increase length for labels (if needed) */
		if (show_labels)
			l += (14 + 2);

		/* Be sure to account for the weight */
		if (show_weights)
			l += 9;

		/* Maintain the maximum length */
		if (l > len)
			len = l;
	}

	/* Find the column to start in */
	len = (len > (screen_x - 4)) ? 0 : ((screen_x - 1) - len);

	/* Output the equipment. */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		o_ptr = equipment[i];

		if (o_ptr)
		{
			attr = tval_to_attr[o_ptr->tval & 0x7F];
		}
		else
		{
			attr = TERM_WHITE;
		}

		/* Display the entry itself */
		if (show_labels)
		{
			/* Mention the use */
			sprintf(tmp_val, " %-14s: ", mention_use(i));
			prt(tmp_val, i + 1, len);

			/* Display the entry itself */
			c_put_str(attr, o_n_buff[i], i + 1, len + 17);

		}
		else
		{
			prt("", i + 1, len);
			c_prt(attr, o_n_buff[i], i + 1, len + 1);
		}

		/* Display the weight if needed */
		if (show_weights && o_ptr)
		{
			int wgt = o_ptr->weight / o_ptr->number;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, i + 1, screen_x - 9);
		}
	}

	/* Make a "shadow" below the list. */
	prt("", i + 1, len);
}


/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify(cptr prompt, object_type * o_ptr)
{
	char o_name[80];

	char out_val[160];

	/* Describe */
	object_desc(o_name, o_ptr, TRUE, 3);

	/* Prompt */
	sprintf(out_val, "%s %s? ", prompt, o_name);

	/* Query */
	return (get_check(out_val));
}


/*
 * Hack -- allow user to "prevent" certain choices
 */
static bool get_item_allow(object_type * o_ptr)
{
	cptr s;

	/* No inscription */
	if (!o_ptr->note)
		return (TRUE);

	/* Find a '!' */
	s = strchr(quark_str(o_ptr->note), '!');

	/* Process preventions */
	while (s)
	{
		/* Check the "restriction" */
		if ((s[1] == p_ptr->command_cmd) || (s[1] == '*'))
		{
			/* Verify the choice */
			if (!verify("Really try", o_ptr))
				return (FALSE);
		}

		/* Find another '!' */
		s = strchr(s + 1, '!');
	}

	/* Allow it */
	return (TRUE);
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
static object_type *get_tag(char tag)
{
	cptr s;

	object_type *o_ptr;

	for (o_ptr = inventory; o_ptr != NULL; o_ptr = o_ptr->next)
	{

		/* Skip non-objects */
		if (!o_ptr->k_idx)
			continue;

		/* Skip empty inscriptions */
		if (!o_ptr->note)
			continue;

		/* Find a '@' */
		s = strchr(quark_str(o_ptr->note), '@');

		/* Process all tags */
		while (s)
		{
			/* Check the normal tags */
			if (s[1] == tag)
			{
				/* Success */
				return o_ptr;
			}

			/* Check the special tags */
			if ((s[1] == p_ptr->command_cmd) && (s[2] == tag))
			{
				/* Success */
				return o_ptr;
			}

			/* Find another '@' */
			s = strchr(s + 1, '@');
		}
	}

	/* No such tag */
	return NULL;
}


/*
 * Helper function that actually gets an item from the inventory
 * stack, with a few parameters.
 */
object_type *get_item_aux(object_type * o_ptr, object_type ** stack,
	int mode)
{
	object_type *ret = NULL;

	/* Paranoia. */
	if (o_ptr == NULL)
		return NULL;

	/* Allow player to "refuse" certain actions */
	if (!get_item_allow(o_ptr))
	{
		return NULL;
	}

	if (o_ptr->number > 1 && (mode & USE_REMOVE))
	{
		/* Paranoia. */
		if (!tval_can_stack(o_ptr->tval))
		{
			mode &= ~(USE_BY_PARTS);
			mode |= USE_JUST_ONE;
		}

		/* Try to unabsorb the items. */
		if (mode & USE_BY_PARTS)
		{
			int num;

			prt("", 0, 0);
			num = get_quantity("How many? ", o_ptr->number);

			if (num)
			{
				ret = object_unabsorb(o_ptr, num);
			}
			else
			{
				return NULL;
			}
		}
		else if (mode & USE_JUST_ONE)
		{
			ret = object_unabsorb(o_ptr, 1);
		}
	}

	/* Restore the screen. 
	 * This should be done here in case remove_from_stack() wants to
	 * modify the screen.
	 */
	screen_load();

	/* Either no attempt at unabsorbtion was made, or the unabsorbtion
	 * failed. (i.e. All or none of the items were selected.) */
	if (ret == NULL)
	{
		ret = o_ptr;

		if (mode & USE_REMOVE)
			remove_from_stack(ret);
	}

	return ret;
}


/*
 * Collect the first object in each page for a stack of objects.
 */
int make_stack_pages(object_type * stack,
	object_type * pages[MAX_STACK_PAGES], int per_page, bool glob)
{
	int i = 0, page_cnt;
	object_type *o_ptr;

	/* XXX Paranoia: Only a-z possible */
	if (per_page > 26)
		per_page = 26;

	/* Default to no pages */
	page_cnt = 0;

	/* Default to no pages */
	pages[0] = NULL;

	for (o_ptr = stack; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr))
			continue;

		/* This object is first in a page */
		if (!(i % per_page))
		{
			/* Remember the first object in this page */
			pages[page_cnt++] = o_ptr;
		}

		/* Count objects */
		++i;
	}

	/* Return number of pages */
	return page_cnt;
}


/*
 * Return the number of objects displayed on the given page.
 */
int count_stack_page(object_type * pages[MAX_STACK_PAGES], int page_cnt,
	int page_cur, int per_page, bool glob)
{
	object_type *o_ptr;
	int count = 0;

	for (o_ptr = pages[page_cur]; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr))
			continue;

		/* Ran out of screen space. */
		if (++count == per_page)
			break;
	}

	return count;
}


/*
 * Print a stack of items to the screen.
 *
 * ``item_tester_tval'' and ``item_tester_hook'' will be used to filter
 * the stack.
 *
 * If ``glob'' is true, then show the global list instead of the local one.
 */
void show_stack_page(object_type * pages[MAX_STACK_PAGES], int page_cnt,
	int page_cur, int per_page, bool glob, int row)
{
	object_type *o_ptr;

	int i, len, lim, l;

	char tmp_val[80];
	char o_name[80];

	/* Maximum space allowed for descriptions */
	lim = screen_x - 4;

	/* Require space for weight (if needed) */
	if (show_weights)
		lim -= 9;

	/* Hack -- Only 80 chars in description allowed */
	if (lim > 79)
		lim = 79;

	i = 0;
	len = 0;

	/* Find the maximum line length of all the items in the stack */
	for (o_ptr = pages[0]; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr))
			continue;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Find the predicted "line length" */
		if (!glob)
		{
			l = strlen(o_name) + 5;

			/* Be sure to account for the weight */
			if (show_weights)
				l += 9;

			/* Maintain the maximum length */
			if (l > len)
				len = l;
		}

		++i;
	}

	/* Find the column to start in */
	len = (len > (screen_x - 4)) ? 0 : ((screen_x - 1) - len);

	i = 0;

	/* Actually output the stack. */
	for (o_ptr = pages[page_cur]; o_ptr != NULL;
		o_ptr = (glob ? o_ptr->next_global : o_ptr->next))
	{
		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr))
			continue;

		/* Describe the object */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Hack -- enforce max length */
		o_name[lim] = '\0';

		/* Prepare an index --(-- */
		sprintf(tmp_val, " %c) ", ((o_ptr->stack == STACK_INVEN &&
					conserve_slots) ? o_ptr->tag : I2A(i)));

		/* Clear the line with the (possibly indented) index */
		prt(tmp_val, row + i + 1, (glob ? 0 : len));

		/* Display the entry itself */
		c_put_str(tval_to_attr[o_ptr->tval & 0x7F], o_name, row + i + 1,
			(glob ? 4 : len + 4));

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight /* / o_ptr->number */;
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, row + i + 1, screen_x - 9);
		}

		/* Ran out of screen space. */
		if (++i == per_page)
			break;
	}

	len = glob ? 1 : len;

	/* Make a "shadow" below the list (only if needed) */
	if (i && (row + i < screen_y - 1))
		prt("", row + i + 1, len);

	/* Visual reminder of "more items" */
	if (page_cnt > 1)
	{
		char s[10] = "  more  ";

		if (page_cur > 0)
			s[0] = '-';
		if (page_cur < page_cnt - 1)
			s[7] = '+';

		/* Show "more" reminder (after the last object) */
		prt(s, row + i + 1, len + 4);

		/* Make a "shadow" below the list. */
		if (row + i + 2 < screen_y)
			prt("", row + i + 2, len);
	}
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
 * items are in that location) if the proper flag was given.  XXX XXX
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
 * Global "p_ptr->command_wrk" is used to choose between equip/inven listings.
 * If it is TRUE then we are viewing inventory, else equipment.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 *
 * Note that "Term_save()" / "Term_load()" blocks must not overlap.
 */
object_type *get_item(cptr prompt, cptr str, int y, int x, int mode)
{
	object_type *stack = NULL;
	object_type *s_aux = NULL;
	object_type *ret = NULL;
	int size_aux, size_main;

	int who = 0;
	int which;
	int where;

	char out_val[160];
	char tmp_val[160];

	int per_page, page_cur, page_cnt = 0, page_cur_cnt;
	object_type *pages[MAX_STACK_PAGES];


	if (mode & USE_INVEN)
	{
		who = cave_m_idx[y][x];

		if (who < 0)
		{
			stack = inventory;
		}
		else if (who > 0)
		{
			stack = m_list[who].inventory;
		}
	}

	if (mode & USE_FLOOR)
	{
		s_aux = cave_o_idx[y][x];
	}

	size_main = size_stack(stack, FALSE);
	size_aux = size_stack(s_aux, FALSE);

	/* HACK. Allow interface-less usage of this function. If
	 * "item_tester_automatic" is set, pick one item at random.
	 * This will not work if any of "USE_REMOVE", "USE_BY_PARTS" or 
	 * "USE_JUST_ONE" are set. It is possible that an item on the 
	 * floor will be picked. 
	 */
	if (item_tester_automatic) {
	  int foo, i;

	  object_type* ret;

	  if (mode & USE_REMOVE || 
	      mode & USE_BY_PARTS || 
	      mode & USE_JUST_ONE) {

	    ret = NULL;

	  } else {
	    foo = rand_int((mode & USE_INVEN ? size_main : 0) +
			   (mode & USE_FLOOR ? size_aux : 0));

	    if (foo < size_main) {
	      ret = stack;

	      for (ret = stack; ret != NULL; ret = ret->next) {
		if (i == foo) {
		  break;
		}

		if (item_tester_okay(ret)) {
		  i++;
		}
	      }

	    } else {
	      foo -= size_main;

	      for (ret = s_aux; ret != NULL; ret = ret->next) {
		if (i == foo) {
		  break;
		}

		if (item_tester_okay(ret)) {
		  i++;
		}
	      }
	    }
	  }

	  item_tester_tval = 0;
	  item_tester_hook = NULL;
	  item_tester_automatic = FALSE;

	  return ret;
	}


	/* Require at least one legal choice */
	if (!size_main && !size_aux)
	{
		/* Warning if needed */
		if (str) mprint(MSG_TEMP, str);

		/* Forget the item_tester_tval restriction */
		item_tester_tval = 0;

		/* Forget the item_tester_hook restriction */
		item_tester_hook = NULL;

		return NULL;
	}

	/* Analyze choices */
	else
	{
		/* Use inventory if allowed */
		if (size_main)
		{
			where = (USE_INVEN);
		}

		/* Use floor if allowed */
		else if (size_aux)
		{
			where = (USE_FLOOR);
		}

		/* Hack -- Use (empty) inventory */
		else
		{
			where = (USE_INVEN);
		}
	}

	/* Save the screen */
	screen_save();

	/* Calculate number of items per screenful */
	per_page = screen_y - 2;

	/* XXX Hack -- Only a-z possible */
	if (per_page > 26)
		per_page = 26;

	/* Display the first page */
	page_cur = 0;

	/* Repeat until done */
	while (ret == NULL)
	{
		/* Viewing inventory */
		if (where == USE_INVEN)
		{
			/* Split the stack into screenfuls */
			page_cnt = make_stack_pages(stack, pages, per_page, FALSE);

			/* Count the number of items on the current page */
			page_cur_cnt =
				count_stack_page(pages, page_cnt, page_cur, per_page,
				FALSE);

			/* Display the current page */
			show_stack_page(pages, page_cnt, page_cur, per_page, FALSE, 0);

			if (who < 0)
			{
				/* Begin the prompt */
				sprintf(out_val, "Inven:");
			}
			else if (who > 0)
			{
				/* Begin the prompt */
				sprintf(out_val, "Monster:");
			}

			/* Build the prompt */
			sprintf(tmp_val, " %c-%c,", I2A(0), I2A(page_cur_cnt - 1));

			/* Append */
			strcat(out_val, tmp_val);

			/* Indicate legality of the "floor" */
			if (size_aux)
				strcat(out_val, " / for Floor,");
		}

		/* Viewing floor */
		else if (where == USE_FLOOR)
		{
			/* Split the stack into screenfuls */
			page_cnt = make_stack_pages(s_aux, pages, per_page, FALSE);

			/* Count the number of items on the current page */
			page_cur_cnt =
				count_stack_page(pages, page_cnt, page_cur, per_page,
				FALSE);

			/* Display the current page */
			show_stack_page(pages, page_cnt, page_cur, per_page, FALSE, 0);

			/* Begin the prompt */
			sprintf(out_val, "Floor:");

			/* Build the prompt */
			sprintf(tmp_val, " %c-%c,", I2A(0), I2A(page_cur_cnt - 1));

			/* Append */
			strcat(out_val, tmp_val);

			/* Append */
			if (size_main)
			{
				if (who < 0)
				{
					strcat(out_val, " / for Inven,");
				}
				else if (who > 0)
				{
					strcat(out_val, " / for Monster,");
				}
			}
		}

		/* Indicate "previous page" */
		if (page_cur > 0)
			strcat(out_val, " -,");

		/* Indicate "next page" */
		if (page_cur < page_cnt - 1)
			strcat(out_val, " +,");

		/* Finish the prompt */
		strcat(out_val, " ESC");

		/* Build the prompt */
		sprintf(tmp_val, "(%s) %s? ", out_val, prompt);

		/* Show the prompt */
		prt(tmp_val, 0, 0);

		/*
		 * Hack -- Auto-select if there's only one possible choice.
		 */
		if (other_query_flag && (size_main + size_aux == 1))
		{
			which = '\n';
		}
		else
		{
			/* Get a key */
			which = inkey();
		}

		/* Parse it */
		switch (which)
		{
			case ESCAPE:
			{
				/* Fix the screen if necessary */
				screen_load();

				/* Forget the item_tester_tval restriction */
				item_tester_tval = 0;

				/* Forget the item_tester_hook restriction */
				item_tester_hook = NULL;

				return NULL;
				break;
			}

			case '/':
			{
				/* Toggle to inventory */
				if (size_main && (where != (USE_INVEN)))
				{
					where = (USE_INVEN);
				}

				/* Toggle to floor */
				else if (size_aux && (where != (USE_FLOOR)))
				{
					where = (USE_FLOOR);
				}

				/* No toggle allowed */
				else
				{
					bell();
					break;
				}

				/* Display first page */
				page_cur = 0;

				/* Load screen */
				screen_load();

				/* Save screen */
				screen_save();

				/* Need to redraw */
				break;
			}

			case ' ':
			{
				if (page_cnt == 1)
				{
					bell();
					break;
				}
				
				/* Advance a page */
				++page_cur;

				/* Wrap as needed */
				if (page_cur >= page_cnt)
					page_cur = 0;

				/* Load screen */
				screen_load();

				/* Save screen */
				screen_save();

				break;
			}

			case '-':
			{
				/* Not displaying first page */
				if (page_cur > 0)
				{
					/* Go back a page */
					--page_cur;

					/* Load screen */
					screen_load();

					/* Save screen */
					screen_save();
				}
				else
				{
					bell();
				}

				break;
			}

			case '+':
			case '=':
			{
				/* Not displaying last page */
				if (page_cur < page_cnt - 1)
				{
					/* Go forward a page */
					++page_cur;

					/* Load screen */
					screen_load();

					/* Save screen */
					screen_save();
				}
				else
				{
					bell();
				}

				break;
			}

			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':
			{
				/* Look up the tag */
				object_type *foo = get_tag(which);

				if (!foo)
				{
					bell();
					break;
				}

				/* Validate the item */
				if (!item_tester_okay(foo))
				{
					bell();
					break;
				}

				ret = get_item_aux(foo, NULL /* &stack */ , mode);
				break;
			}

			case '\n':
			case '\r':
			{
				/* Get the first displayed item */
				object_type *top = pages[page_cur];

				ret = get_item_aux(top, NULL /* &stack */ , mode);

				if (ret && other_query_flag)
				{
					char o_name[80];

					object_desc(o_name, ret, TRUE, 1);
					msg_format("You automatically select %s.", o_name);
				}

				break;
			}

			default:
			{
				/* Get the first displayed item */
				object_type *top = pages[page_cur];

				object_type *foo = NULL;
				int i = 0;
				int num;

				/* Extract "query" setting */
				num = A2I(tolower(which));

				/* Fetch that item. */
				for (foo = top; foo != NULL; foo = foo->next)
				{
					if (item_tester_okay(foo))
					{
						if (conserve_slots && (foo->stack == STACK_INVEN))
						{
							if (foo->tag == which)
								break;
						}
						else
						{
							if (i == num)
								break;
						}

						i++;
					}
				}

				if (foo == NULL)
				{
					bell();
					break;
				}

				ret = get_item_aux(foo, NULL /* &stack */ , mode);
				break;
			}
		}
	}

	/* Note that Term_load() was called in get_item_aux(). */

	/* Forget the item_tester_tval restriction */
	item_tester_tval = 0;

	/* Forget the item_tester_hook restriction */
	item_tester_hook = NULL;

	return ret;
}

