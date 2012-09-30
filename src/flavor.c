/* File: flavor.c */

/* Purpose: Object flavor code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Max sizes of the following arrays
 */
#define MAX_ROCKS      68		/* Used with rings (min 55) */
#define MAX_AMULETS    30		/* Used with amulets (min 20) */
#define MAX_WOODS      40		/* Used with staffs (min 30) */
#define MAX_METALS     39		/* Used with wands/rods (min 30/29) */
#define MAX_COLORS     76		/* Used with potions (min 64) */
#define MAX_SHROOM     25		/* Used with mushrooms (min 20) */
#define MAX_TITLES     54		/* Used with scrolls (min 48) */
#define MAX_SYLLABLES 164		/* Used with scrolls (see below) */


/*
 * Rings (adjectives and colors)
 */

static cptr ring_adj[MAX_ROCKS] =
{
	/* 0-9 */
	"Alexandrite", "Amethyst", "Aquamarine", "Azurite", "Beryl",
	"Bloodstone", "Calcite", "Carnelian", "Corundum", "Diamond",
	/* 10-19 */
	"Emerald", "Fluorite", "Garnet", "Granite", "Jade",
	"Jasper", "Lapis Lazuli", "Malachite", "Marble", "Moonstone",
	/* 20-29 */
	"Onyx", "Opal", "Pearl", "Quartz", "Quartzite",
	"Rhodonite", "Ruby", "Sapphire", "Tiger Eye", "Topaz",
	/* 30-39 */
	"Turquoise", "Zircon", "Platinum", "Bronze", "Gold",
	"Obsidian", "Silver", "Tortoise Shell", "Mithril", "Jet",
	/* 40-49 */
	"Engagement", "Adamantite", "Wire", "Dilithium", "Bone",
	"Wooden", "Iron", "Serpent", "Wedding", "Double",
	/* 50-59 */
	"Plain", "Brass", "Scarab", "Shining", "Rusty",
	"Transparent", "Cat's-Eye", "Chrysoberyl", "Serpentine", "Spinel",
	/* 60-67 */
	"Topaz", "Morganite", "Heliodor", "Tourmaline", "Chalcedony",
	"Peridot", "Hematite", "Coral"
};

static byte ring_col[MAX_ROCKS] =
{
	/* 0-9 */
	TERM_GREEN, TERM_VIOLET, TERM_L_BLUE, TERM_L_BLUE, TERM_L_GREEN,
	TERM_RED, TERM_WHITE, TERM_RED, TERM_SLATE, TERM_WHITE,
	/* 10-19 */
	TERM_GREEN, TERM_L_GREEN, TERM_RED, TERM_L_DARK, TERM_L_GREEN,
	TERM_UMBER, TERM_BLUE, TERM_GREEN, TERM_WHITE, TERM_L_WHITE,
	/* 20-29 */
	TERM_L_RED, TERM_L_WHITE, TERM_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_RED, TERM_BLUE, TERM_YELLOW, TERM_YELLOW,
	/* 30-39 */
	TERM_L_BLUE, TERM_L_UMBER, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_DARK, TERM_L_WHITE, TERM_GREEN, TERM_L_BLUE, TERM_L_DARK,
	/* 40-49 */
	TERM_YELLOW, TERM_VIOLET, TERM_UMBER, TERM_L_WHITE, TERM_WHITE,
	TERM_UMBER, TERM_BLUE, TERM_GREEN, TERM_YELLOW, TERM_ORANGE,
	/* 50-59 */
	TERM_YELLOW, TERM_ORANGE, TERM_L_GREEN, TERM_YELLOW, TERM_RED,
	TERM_WHITE, TERM_YELLOW, TERM_YELLOW, TERM_L_GREEN, TERM_RED,
	/* 60-67 */
	TERM_YELLOW, TERM_L_RED, TERM_YELLOW, TERM_GREEN, TERM_L_DARK,
	TERM_L_GREEN, TERM_L_DARK, TERM_L_RED
};


/*
 * Amulets (adjectives and colors)
 */
static cptr amulet_adj[MAX_AMULETS] =
{
	/* 0-9 */
	"Amber", "Driftwood", "Coral", "Agate", "Ivory",
	"Obsidian", "Bone", "Brass", "Bronze", "Pewter",
	/* 10-19 */
	"Tortoise Shell", "Golden", "Azure", "Crystal", "Silver", 
	"Copper", "Rosetted", "Spiral", "Star", "Square", 
	/* 20-29 */
	"Hexagonal", "Steel", "Skull", "Platinum", "Electrum",
	"Glass", "Cracked", "Heartwood", "Bark", "Mirrored"
};

static byte amulet_col[MAX_AMULETS] =
{
	/* 0-9 */
	TERM_YELLOW, TERM_L_UMBER, TERM_WHITE, TERM_L_WHITE, TERM_WHITE,
	TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_SLATE,
	/* 10-19 */
	TERM_GREEN, TERM_YELLOW, TERM_L_BLUE, TERM_L_BLUE, TERM_L_WHITE,
	TERM_L_UMBER, TERM_VIOLET, TERM_VIOLET, TERM_YELLOW, TERM_L_UMBER,
	/* 20-29 */
	TERM_L_DARK, TERM_WHITE, TERM_L_DARK, TERM_L_WHITE, TERM_WHITE,
	TERM_L_RED, TERM_RED, TERM_UMBER, TERM_UMBER, TERM_WHITE
};


/*
 * Staffs (adjectives and colors)
 */
static cptr staff_adj[MAX_WOODS] =
{
	/* 0-9 */
	"Aspen", "Balsa", "Banyan", "Birch", "Cedar",
	"Cottonwood", "Cypress", "Dogwood", "Elm", "Willow",
	/* 10-19 */
	"Hemlock", "Hickory", "Ironwood", "Locust", "Mahogany",
	"Maple", "Mulberry", "Oak", "Pine", "Redwood",
	/* 20-29 */
	"Rosewood", "Spruce", "Sycamore", "Teak", "Walnut",
	"Mistletoe", "Hawthorn", "Bamboo", "Silver", "Runed",
	/* 30-39 */
	"Golden", "Ashen", "Gnarled", "Ivory", "Sandalwood",
	"Yew", "Thorned", "Charred", "Darkwood", "Flowering"
};

static byte staff_col[MAX_WOODS] =
{
	/* 0-9 */
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	/* 10-19 */
	TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_RED,
	/* 20-29 */
	TERM_RED, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_GREEN, TERM_L_UMBER, TERM_L_UMBER, TERM_L_WHITE, TERM_UMBER,
	/* 30-39 */
	TERM_YELLOW, TERM_SLATE, TERM_UMBER, TERM_L_WHITE, TERM_YELLOW,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_DARK, TERM_L_DARK, TERM_L_GREEN
};


/*
 * Wands (adjectives and colors)
 */
static cptr wand_adj[MAX_METALS] =
{
	/* 0-9 */
	"Aluminium", "Cast Iron", "Chromium", "Copper", "Gold",
	"Iron", "Magnesium", "Molybdenum", "Nickel", "Rusty",
	/* 10-19 */
	"Silver", "Steel", "Tin", "Titanium", "Tungsten",
	"Zirconium", "Zinc", "Aluminium-Plated", "Copper-Plated", "Gold-Plated",
	/* 20-29 */
	"Nickel-Plated", "Silver-Plated", "Steel-Plated", "Tin-Plated", "Zinc-Plated",
	"Mithril-Plated", "Mithril", "Runed", "Bronze", "Brass",
	/* 30-38 */
	"Platinum", "Lead", "Lead-Plated", "Ivory", "Adamantite",
	"Uridium", "Long", "Short", "Hexagonal"
};

static byte wand_col[MAX_METALS] =
{
	/* 0-9 */
	TERM_L_BLUE, TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_SLATE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_UMBER, TERM_RED,
	/* 10-19 */
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_WHITE, TERM_WHITE,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_BLUE, TERM_L_UMBER, TERM_YELLOW,
	/* 20-29 */
	TERM_L_UMBER, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_BLUE, TERM_L_BLUE, TERM_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	/* 30-38 */
	TERM_WHITE, TERM_SLATE, TERM_SLATE, TERM_WHITE, TERM_VIOLET,
	TERM_L_RED, TERM_L_BLUE, TERM_BLUE, TERM_RED
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

static cptr food_adj[MAX_SHROOM] =
{
	/* 0-9 */
	"Blue", "Black", "Black Spotted", "Brown", "Dark Blue",
	"Dark Green", "Dark Red", "Yellow", "Furry", "Green",
	/* 10-19 */
	"Grey", "Light Blue", "Light Green", "Violet", "Red",
	"Slimy", "Tan", "White", "White Spotted", "Wrinkled",
	/* 20-24 */
	"Strange", "Silver", "Glowing", "Orange", "Dried",
};

static byte food_col[MAX_SHROOM] =
{
	/* 0-9 */
	TERM_BLUE, TERM_L_DARK, TERM_L_DARK, TERM_UMBER, TERM_BLUE,
	TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_L_WHITE, TERM_GREEN,
	/* 10-19 */
	TERM_SLATE, TERM_L_BLUE, TERM_L_GREEN, TERM_VIOLET, TERM_RED,
	TERM_SLATE, TERM_L_UMBER, TERM_WHITE, TERM_WHITE, TERM_UMBER,
	/* 20-24 */
	TERM_L_BLUE, TERM_WHITE, TERM_YELLOW, TERM_ORANGE, TERM_UMBER,
};


/*
 * Color adjectives and colors, for potions.
 * Hack -- The first four entries are hard-coded.
 * (water, apple juice, slime mold juice, something)
 */

static cptr potion_adj[MAX_COLORS] =
{
	/* Fixed colors */
	"Clear", "Light Brown", "Icky Green", "xxx",
	/* 4-9 */
	"Azure",
	"Blue", "Blue Speckled", "Black", "Brown", "Brown Speckled",
	/* 10-19 */
	"Bubbling", "Chartreuse", "Cloudy", "Copper Speckled", "Crimson",
	"Cyan", "Dark Blue", "Dark Green", "Dark Red", "Gold Speckled",
	/* 20-29 */
	"Green", "Green Speckled", "Grey", "Grey Speckled", "Hazy",
	"Indigo", "Light Blue", "Light Green", "Magenta", "Metallic Blue",
	/* 30-39 */
	"Metallic Red", "Metallic Green", "Metallic Purple", "Misty", "Orange",
	"Orange Speckled", "Pink", "Pink Speckled", "Puce", "Purple",
	/* 40-49 */
	"Purple Speckled", "Red", "Red Speckled", "Silver Speckled", "Smoky",
	"Tangerine", "Violet", "Vermilion", "White", "Yellow",
	/* 50-59 */
	"Violet Speckled", "Pungent", "Clotted Red", "Viscous Pink", "Oily Yellow",
	"Gloopy Green", "Shimmering", "Coagulated Crimson", "Yellow Speckled",
	/* 60-69 */
	"Gold", "Manly", "Stinking", "Oily Black", "Ichor", "Ivory White",
	"Sky Blue", "Bloody", "Inky Black", "Silver Flecked", "Red Flecked",
	/* 70-75 */
	"Green Flecked", "Sea Green", "Umber", "Layered", "Fizzy Yellow",
	"Fizzy Green",
};

static byte potion_col[MAX_COLORS] =
{
	/* Fixed colors */
	TERM_WHITE, TERM_L_UMBER, TERM_GREEN, 0,
	/* 4-9 */
	TERM_L_BLUE,
	TERM_BLUE, TERM_BLUE, TERM_L_DARK, TERM_UMBER, TERM_UMBER,
	/* 10-19 */
	TERM_L_WHITE, TERM_L_GREEN, TERM_WHITE, TERM_L_UMBER, TERM_RED,
	TERM_L_BLUE, TERM_BLUE, TERM_GREEN, TERM_RED, TERM_YELLOW,
	/* 20-29 */
	TERM_GREEN, TERM_GREEN, TERM_SLATE, TERM_SLATE, TERM_L_WHITE,
	TERM_VIOLET, TERM_L_BLUE, TERM_L_GREEN, TERM_RED, TERM_BLUE,
	/* 30-39 */
	TERM_RED, TERM_GREEN, TERM_VIOLET, TERM_L_WHITE, TERM_ORANGE,
	TERM_ORANGE, TERM_L_RED, TERM_L_RED, TERM_VIOLET, TERM_VIOLET,
	/* 40-49 */
	TERM_VIOLET, TERM_RED, TERM_RED, TERM_L_WHITE, TERM_L_DARK,
	TERM_ORANGE, TERM_VIOLET, TERM_RED, TERM_WHITE, TERM_YELLOW,
	/* 50-59 */
	TERM_VIOLET, TERM_L_RED, TERM_RED, TERM_L_RED, TERM_YELLOW,
	TERM_GREEN, TERM_VIOLET, TERM_RED, TERM_YELLOW, TERM_YELLOW,
	/* 60-69 */
	TERM_L_UMBER, TERM_UMBER, TERM_L_DARK, TERM_RED, TERM_WHITE,
	TERM_L_BLUE, TERM_RED, TERM_L_DARK, TERM_WHITE, TERM_RED,
	/* 70-75 */
	TERM_GREEN, TERM_L_GREEN, TERM_UMBER, TERM_L_BLUE, TERM_YELLOW,
	TERM_L_GREEN,
};


/*
 * Syllables for scrolls (must be 1-4 letters each)
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
	"vom", "wah", "wed", "werg", "wex", "whon", "wun", "xi",
	"yerg", "yp", "zun", "tri", "blaa", "jah", "bul", "on",
	"foo", "ju", "xuxu"
};


/*
 * Hold the titles of scrolls, 6 to 14 characters each
 * Also keep an array of scroll colors (always WHITE for now)
 */
static char scroll_adj[MAX_TITLES][16];

static byte scroll_col[MAX_TITLES];


/*
 * Certain items, if aware, are known instantly
 * This function is used only by "flavor_init()"
 */
static bool object_easy_know(int i)
{
	object_kind *k_ptr = &k_info[i];

	if (FLAG(k_ptr, TR_EASY_KNOW)) return (TRUE);

	/* Nope */
	return (FALSE);
}


/*
 * Certain items have a flavor
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


void get_table_name(char *out_string, bool quotes)
{
	int testcounter = 2;
	
	int len = 0;
	
	/* Empty string */
	out_string[0] = 0;

	if (quotes)
	{
		strnfcat(out_string, 18, &len, "'");
	}

	if (one_in_(3))
	{
		while (testcounter--)
			strnfcat(out_string, 18, &len, syllables[randint0(MAX_SYLLABLES)]);
	}
	else
	{
		char Syllable[80];

		while (testcounter--)
		{
			(void)get_rnd_line("elvish.txt", 0, Syllable);
			strnfcat(out_string, 18, &len, "%s", Syllable);
		}
	}

	if (quotes)
	{
		out_string[1] = toupper(out_string[1]);
		strnfcat(out_string, 18, &len, "'");
	}
	else
	{
		out_string[0] = toupper(out_string[0]);
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
		j = randint0(MAX_ROCKS);
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
		j = randint0(MAX_AMULETS);
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
		j = randint0(MAX_WOODS);
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
		j = randint0(MAX_METALS);
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
		j = randint0(MAX_METALS);
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
		j = randint0(MAX_SHROOM);
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
		j = rand_range(4, MAX_COLORS - 1);
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
			
			int buf_len = 0;

			bool okay;

			/* Start a new title */
			buf[0] = '\0';

			/* Collect words until done */
			while (1)
			{
				int q, s;

				char tmp[80];
				
				int len = 0;

				/* Start a new word */
				tmp[0] = '\0';

				/* Choose one or two syllables */
				s = ((randint0(100) < 30) ? 1 : 2);

				/* Add a one or two syllable word */
				for (q = 0; q < s; q++)
				{
					/* Add the syllable */
					strnfcat(tmp, 80, &len, syllables[randint0(MAX_SYLLABLES)]);
				}

				/* Stop before getting too long */
				if (strlen(buf) + 1 + strlen(tmp) > 15) break;

				/* Add a space + word */
				strnfcat(buf, 80, &buf_len, " %s", tmp);
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
 * under size - 1 chars (plus a terminator).
 *
 * Note the use of "object_desc_num()" and "object_desc_int()" as
 * hyper-efficient, portable, versions of some common "sprintf()" commands.
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
 */
void object_desc(char *buf, const object_type *o_ptr, int pref, int mode,
                 int max)
{
	cptr basenm, modstr;
	int power;

	bool aware = FALSE;
	bool known = FALSE;

	bool append_name = FALSE;

	bool show_weapon = FALSE;
	bool show_armour = FALSE;

	cptr s;

	object_type *bow_ptr;

	/* damage dice, damage sides, damage bonus, energy */
	int dd, ds, db, energy_use;
	int tmul;
	long avgdam;
	
	int len = 0;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	monster_race *r_ptr = &r_info[o_ptr->pval];

	/* See if the object is "aware" */
	if (object_aware_p(o_ptr)) aware = TRUE;

	/* See if the object is "known" */
	if (object_known_p(o_ptr)) known = TRUE;

	/* Artifacts are not "aware' unless "known" */
	if ((FLAG(o_ptr, TR_INSTA_ART)) && !known) aware = FALSE;

	/* Extract default "base" string */
	basenm = get_object_name(o_ptr);

	/* Assume no "modifier" string */
	modstr = "";
	
	/* Empty description */
	buf[0] = '\0';

	/* Analyze the object */
	switch (o_ptr->tval)
	{
		case TV_SKELETON:
		case TV_BOTTLE:
		case TV_JUNK:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
		{
			/* Some objects are easy to describe */
			break;
		}

		case TV_FIGURINE:
		case TV_STATUE:
		{
			/* Figurines/Statues */
			cptr tmp = mon_race_name(r_ptr);
			
			char idol_name[512];

			if (!FLAG(r_ptr, RF_UNIQUE))
			{
				strnfmt(idol_name, 512, "%s%s",
						(is_a_vowel(*tmp) ? "an " : "a "), tmp);

				modstr = idol_name;
			}
			else
			{
				modstr = tmp;
			}

			break;
		}

		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Missiles/ Bows/ Weapons */
			show_weapon = TRUE;
			break;
		}

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
			/* Armour */
			show_armour = TRUE;
			break;
		}

		case TV_LITE:
		{
			/* Lites (including a few "Specials") */
			break;
		}

		case TV_AMULET:
		{
			/* Amulets (including a few "Specials") */

			/* Known artifacts */
			if ((FLAG(k_ptr, TR_INSTA_ART)) && aware) break;

			/* Color the object */
			modstr = amulet_adj[o_ptr->sval];
			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Amulet~";
			else
				basenm = "& # Amulet~";
			break;
		}

		case TV_RING:
		{
			/* Rings (including a few "Specials") */

			/* Known artifacts */
			if ((FLAG(k_ptr, TR_INSTA_ART)) && aware) break;

			/* Color the object */
			modstr = ring_adj[o_ptr->sval];
			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Ring~";
			else
				basenm = "& # Ring~";

			/* Hack -- The One Ring */
			if (!aware && (o_ptr->sval == SV_RING_POWER)) modstr = "Plain Gold";

			break;
		}

		case TV_STAFF:
		{
			/* Color the object */
			modstr = staff_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Staff~";
			else
				basenm = "& # Staff~";
			break;
		}

		case TV_WAND:
		{
			/* Color the object */
			modstr = wand_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Wand~";
			else
				basenm = "& # Wand~";
			break;
		}

		case TV_ROD:
		{
			/* Color the object */
			modstr = rod_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Rod~";
			else
				basenm = "& # Rod~";
			break;
		}

		case TV_SCROLL:
		{
			/* Color the object */
			modstr = scroll_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Scroll~";
			else
				basenm = "& Scroll~ titled \"#\"";
			break;
		}

		case TV_POTION:
		{
			/* Color the object */
			modstr = potion_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Potion~";
			else
				basenm = "& # Potion~";
			break;
		}

		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (o_ptr->sval >= SV_FOOD_MIN_FOOD) break;

			/* Color the object */
			modstr = food_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || (o_ptr->info & OB_STOREB))
				basenm = "& Mushroom~";
			else
				basenm = "& # Mushroom~";
			break;
		}

			/*** Magic Books ***/

		case TV_LIFE_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Life Magic #";
			else
				basenm = "& Life Spellbook~ #";
			break;
		}

		case TV_SORCERY_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Sorcery #";
			else
				basenm = "& Sorcery Spellbook~ #";
			break;
		}

		case TV_NATURE_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Nature Magic #";
			else
				basenm = "& Nature Spellbook~ #";
			break;
		}

		case TV_CHAOS_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Chaos Magic #";
			else
				basenm = "& Chaos Spellbook~ #";
			break;
		}

		case TV_DEATH_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Death Magic #";
			else
				basenm = "& Death Spellbook~ #";
			break;
		}

		case TV_TRUMP_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Trump Magic #";
			else
				basenm = "& Trump Spellbook~ #";
			break;
		}

		case TV_ARCANE_BOOK:
		{
			modstr = basenm;
			if (mp_ptr->spell_book == TV_LIFE_BOOK)
				basenm = "& Book~ of Arcane Magic #";
			else
				basenm = "& Arcane Spellbook~ #";
			break;
		}


		case TV_GOLD:
		{
			/* Hack -- Gold/Gems */
			strcpy(buf, basenm);
			return;
		}

		default:
		{
			/* Used in the "inventory" routine */
			strcpy(buf, "(nothing)");
			return;
		}
	}

	/* The object "expects" a "number" */
	if (basenm[0] == '&')
	{
		/* Skip the ampersand (and space) */
		s = basenm + 2;

		/* No prefix */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- None left */
		else if (o_ptr->number <= 0)
		{
			strnfcat(buf, max, &len, "no more ");
		}

		/* Extract the number */
		else if (o_ptr->number > 1)
		{
			strnfcat(buf, max, &len, "%d ", o_ptr->number);
		}

		/* Hack -- The only one of its kind */
		else if (known && (FLAG(o_ptr, TR_INSTA_ART)))
		{
			strnfcat(buf, max, &len, "The ");
		}

		/* A single one, with a vowel in the modifier */
		else if ((*s == '#') && (is_a_vowel(modstr[0])))
		{
			strnfcat(buf, max, &len, "an ");
		}

		/* A single one, with a vowel */
		else if (is_a_vowel(*s))
		{
			strnfcat(buf, max, &len, "an ");
		}

		/* A single one, without a vowel */
		else
		{
			strnfcat(buf, max, &len, "a ");
		}
	}

	/* Hack -- objects that "never" take an article */
	else
	{
		/* No ampersand */
		s = basenm;

		/* No pref */
		if (!pref)
		{
			/* Nothing */
		}

		/* Hack -- all gone */
		else if (o_ptr->number <= 0)
		{
			strnfcat(buf, max, &len, "no more ");
		}

		/* Prefix a number if required */
		else if (o_ptr->number > 1)
		{
			strnfcat(buf, max, &len, "%d ", o_ptr->number);
		}

		/* Hack -- The only one of its kind */
		else if (known && (FLAG(o_ptr, TR_INSTA_ART)))
		{
			strnfcat(buf, max, &len, "The ");
		}

		/* Hack -- single items get no prefix */
		else
		{
			/* Nothing */
		}
	}

	/* Copy the string */
	while (*s)
	{
		/* Pluralizer */
		if (*s == '~')
		{
			/* Add a plural if needed */
			if (o_ptr->number != 1)
			{
				/* Get previous character */
				char k = s[-1];

				/* XXX XXX XXX Mega-Hack */

				/* Hack -- "Cutlass-es" and "Torch-es" */
				if ((k == 's') || (k == 'h'))
				{
					strnfcat(buf, max, &len, "es");
				}
				else
				{
					/* Add an 's' */
					strnfcat(buf, max, &len, "s");
				}
			}
		}

		/* Modifier */
		else if (*s == '#')
		{
			/* Insert the modifier */
			strnfcat(buf, max, &len, "%s", modstr);
		}

		/* Normal */
		else
		{
			/* Copy character */
			strnfcat(buf, max, &len, "%c", *s);
		}
		
		s++;
	}

	/* Append the "kind name" to the "base name" */
	if (append_name)
	{
		strnfcat(buf, max, &len, " of %s", get_object_name(o_ptr));
	}


	/* Hack -- Append "Artifact" or "Special" names */
	if (known)
	{
		if (o_ptr->inscription && strchr(quark_str(o_ptr->inscription), '#'))
		{
			/* Find the '#' */
			cptr str = strchr(quark_str(o_ptr->inscription), '#');

			/* Add the false name */
			strnfcat(buf, max, &len, " %s" CLR_DEFAULT, &str[1]);
		}

		/* Is it a new artifact or ego item? */
		else if (o_ptr->xtra_name)
		{
			strnfcat(buf, max, &len, " %s", quark_str(o_ptr->xtra_name));
		}
	}

	/* No more details wanted */
	if (mode < 1) return;

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
			strnfcat(buf, max, &len, " (empty)");
		}

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
		{
			if (chest_traps[0 - o_ptr->pval])
			{
				strnfcat(buf, max, &len, " (disarmed)");
			}
			else
			{
				strnfcat(buf, max, &len, " (unlocked)");
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
					strnfcat(buf, max, &len, " (Locked)");
					break;
				}
				case CHEST_LOSE_STR:
				{
					strnfcat(buf, max, &len, " (Poison Needle)");
					break;
				}
				case CHEST_LOSE_CON:
				{
					strnfcat(buf, max, &len, " (Poison Needle)");
					break;
				}
				case CHEST_POISON:
				{
					strnfcat(buf, max, &len, " (Gas Trap)");
					break;
				}
				case CHEST_PARALYZE:
				{
					strnfcat(buf, max, &len, " (Gas Trap)");
					break;
				}
				case CHEST_EXPLODE:
				{
					strnfcat(buf, max, &len, " (Explosion Device)");
					break;
				}
				case CHEST_SUMMON:
				{
					strnfcat(buf, max, &len, " (Summoning Runes)");
					break;
				}
				default:
				{
					strnfcat(buf, max, &len, " (Multiple Traps)");
					break;
				}
			}
		}
	}


	/* Display the item like a weapon */
	if (FLAG(o_ptr, TR_SHOW_MODS)) show_weapon = TRUE;

	/* Display the item like a weapon */
	if (o_ptr->to_h && o_ptr->to_d) show_weapon = TRUE;

	/* Display the item like armour */
	if ((o_ptr->ac) && (o_ptr->tval != TV_WAND)) show_armour = TRUE;

	/* Dump base weapon info */
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Missiles and Weapons */

			/* Append a "damage" string */
			strnfcat(buf, max, &len, " (%dd%d)", o_ptr->dd, o_ptr->ds);

			/* All done */
			break;
		}

		case TV_BOW:
		{
			/* Bows get a special "damage string" */

			/* Extract the "base power" */
			switch (o_ptr->sval)
			{
				case SV_SLING:
				{
					power = 2;
					break;
				}
				case SV_SHORT_BOW:
				{
					power = 2;
					break;
				}
				case SV_LONG_BOW:
				{
					if (p_ptr->stat[A_STR].use >= 160)
					{
						power = 3;
					}
					else
					{
						/* hack- weak players cannot use a longbow well */
						power = 2;
					}
					break;
				}
				case SV_LIGHT_XBOW:
				{
					power = 4;
					break;
				}
				case SV_HEAVY_XBOW:
				{
					power = 5;
					break;
				}
				default:
				{
					msgf("Unknown firing multiplier.");
					power = 0;
				}
			}

			/* Apply the "Extra Might" flag */
			if (FLAG(o_ptr, TR_XTRA_MIGHT)) power++;

			/* Append a special "damage" string */
			strnfcat(buf, max, &len, " (x%d)", power);

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
			strnfcat(buf, max, &len, " (%+d,%+d%%)", o_ptr->to_h, 
					deadliness_calc(o_ptr->to_d) - 100);
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			strnfcat(buf, max, &len, " (%+d)", o_ptr->to_h);
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			strnfcat(buf, max, &len, " (%+d%%)", o_ptr->to_d * 5);
		}
	}

	bow_ptr = &p_ptr->equipment[EQUIP_BOW];

	/* if have a firing weapon + ammo matches bow */
	if (bow_ptr->k_idx && (p_ptr->ammo_tval == o_ptr->tval))
	{
		/* See if the bow is "known" - then set damage bonus */
		if (object_known_p(bow_ptr))
		{
			db = bow_ptr->to_d;
		}
		else
		{
			db = 0;
		}

		/* effect of player */
		db += p_ptr->dis_to_d;

		/* effect of ammo */
		if (known) db += o_ptr->to_d;

		dd = o_ptr->dd;
		ds = o_ptr->ds;

		/* effect of damage dice x2 */
		avgdam = avg_dam(db, dd, ds);

		/* Bow properties */
		energy_use = p_ptr->bow_energy;
		tmul = p_ptr->ammo_mult;

		/* Get extra "power" from "extra might" */
		if (FLAG(p_ptr, TR_XTRA_MIGHT)) tmul++;

		/* launcher multiplier */
		avgdam *= tmul;

		/* display (shot damage/ avg damage) */
		strnfcat(buf, max, &len, " (%d/", avgdam / 200);

		tmul = p_ptr->num_fire;
		if (tmul == 0)
		{
			strnfcat(buf, max, &len, "0)");
		}
		else
		{
			/* calc effects of energy  x2 */
			avgdam *= (1 + p_ptr->num_fire);

			/* rescale */
			avgdam /= 4 * energy_use;
			strnfcat(buf, max, &len, "%d)", avgdam);
		}
	}

	/* Add the armor bonuses */
	if (known)
	{
		/* Show the armor class info */
		if (show_armour)
		{
			strnfcat(buf, max, &len, " [%d,%+d]", o_ptr->ac,  o_ptr->to_a);
		}

		/* No base armor, but does increase armor */
		else if (o_ptr->to_a)
		{
			strnfcat(buf, max, &len, " [%+d]", o_ptr->to_a);
		}
	}

	/* Hack -- always show base armor */
	else if (show_armour)
	{
		strnfcat(buf, max, &len, " [%d]", o_ptr->ac);
	}


	/* No more details wanted */
	if (mode < 2) return;


	/*
	 * Hack -- Wands and Staffs have charges.  Make certain how many charges
	 * a stack of staffs really has is clear. -LM-
	 */
	if (known && ((o_ptr->tval == TV_STAFF) || (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		strnfcat(buf, max, &len, " (");

		/* Clear explaination for staffs. */
		if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
		{
			strnfcat(buf, max, &len, "%dx ", o_ptr->number);
		}
		
		if (o_ptr->pval == 1)
		{
			strnfcat(buf, max, &len, "%d charge)", o_ptr->pval);
		}
		else
		{
			strnfcat(buf, max, &len, "%d charges)", o_ptr->pval);
		}
	}
	/* Hack -- Rods have a "charging" indicator.  Now that stacks of rods may
	 * be in any state of charge or discharge, this now includes a number. -LM-
	 */
	else if (o_ptr->tval == TV_ROD)
	{
		/* Hack -- Dump " (# charging)" if relevant */
		if (o_ptr->timeout)
		{
			/* Stacks of rods display an exact count of charging rods. */
			if (o_ptr->number > 1)
			{
				/* Paranoia. */
				if (k_ptr->pval == 0) k_ptr->pval = 1;

				/*
				 * Find out how many rods are charging, by dividing
				 * current timeout by each rod's maximum timeout.
				 * Ensure that any remainder is rounded up.  Display
				 * very discharged stacks as merely fully discharged.
				 */
				power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
				if (power > o_ptr->number) power = o_ptr->number;

				/* Display prettily. */
				strnfcat(buf, max, &len, " (%d charging)", power);
			}

			/* "one Rod of Perception (1 charging)" would look tacky. */
			else
			{
				strnfcat(buf, max, &len, " (charging)");
			}
		}
	}

	/* Hack -- Process Lanterns/Torches */
	else if (o_ptr->tval == TV_LITE)
	{
		if (FLAG(o_ptr, TR_LITE))
		{
			/* Hack - tell us when lites of everburning are "empty" */
			if ((o_ptr->sval <= SV_LITE_LANTERN) && !o_ptr->timeout)
			{
				strnfcat(buf, max, &len, " (empty)");
			}
		}
		else
		{
			/* Hack -- Turns of light for normal lites */
			strnfcat(buf, max, &len, " (with %d turns of light)", o_ptr->timeout);
		}
	}


	/* Dump "pval" flags for wearable items */
	if (known && (FLAG(o_ptr, TR_PVAL_MASK)))
	{
		/* Start the display */
		strnfcat(buf, max, &len, " (%+d", o_ptr->pval);

		/* Do not display the "pval" flags */
		if (FLAG(o_ptr, TR_HIDE_TYPE))
		{
			/* Nothing */
		}

		/* Speed */
		else if (FLAG(o_ptr, TR_SPEED))
		{
			/* Dump " to speed" */
			strnfcat(buf, max, &len, " to speed");
		}

		/* Attack speed */
		else if (FLAG(o_ptr, TR_BLOWS))
		{
			if (ABS(o_ptr->pval) == 1)
			{
				/* Add " attack" */
				strnfcat(buf, max, &len, " attack");
			}
			else
			{
				/* Add "attacks" */
				strnfcat(buf, max, &len, " attacks");
			}
		}

		/* Finish the display */
		strnfcat(buf, max, &len, ")");
	}

	/* Indicate charging objects, but not rods. */
	if (known && o_ptr->timeout && (o_ptr->tval != TV_ROD)
		&& (o_ptr->tval != TV_LITE))
	{
		/* Hack -- Dump " (charging)" if relevant */
		strnfcat(buf, max, &len, " (charging)");
	}


	/* No more details wanted */
	if (mode < 3) return;

	/* Use the standard inscription if available */
	if (o_ptr->inscription)
	{
		cptr tmp = quark_str(o_ptr->inscription);
	
		/* Append the inscription */
		strnfcat(buf, max, &len, " {");

		/* Scan for the '#' character which marks a fake name. */
		while(*tmp && (*tmp != '#'))
		{
			strnfcat(buf, max, &len, "%c", *tmp);
		
			tmp++;
		}
		
		/* Finish the inscription */
		strnfcat(buf, max, &len, CLR_DEFAULT "}");
	}

	/* Use the game-generated "feeling" otherwise, if available */
	else if (o_ptr->feeling)
	{
		/* Append the inscription */
		strnfcat(buf, max, &len, " {%s" CLR_DEFAULT "}", game_inscriptions[o_ptr->feeling]);
	}

	/* Note "cursed" if the item is known to be cursed */
	else if (cursed_p(o_ptr) && (known || (o_ptr->info & (OB_SENSE))))
	{
		/* Append the inscription */
		strnfcat(buf, max, &len, " {cursed}");
	}

	/* Mega-Hack -- note empty wands/staffs */
	else if (!known && (o_ptr->info & (OB_EMPTY)))
	{
		/* Append the inscription */
		strnfcat(buf, max, &len, " {empty}");
	}

	/* Note "tried" if the object has been tested unsuccessfully */
	else if (!aware && object_tried_p(o_ptr))
	{
		/* Append the inscription */
		strnfcat(buf, max, &len, " {tried}");
	}

	/* Note the discount, if any */
	else if (o_ptr->discount)
	{
		/* Append the inscription */
		strnfcat(buf, max, &len, " {%d%% off}", o_ptr->discount);
	}
}

/*
 * Wrapper around object_desc() for the '%v'
 * format option.  This allows object_desc() to be
 * called in a format string.
 *
 * The parameters are object_type (o_ptr), pref (int), mode(int).
 */
void object_fmt(char *buf, uint max, cptr fmt, va_list *vp)
{
	const object_type *o_ptr;
	int pref;
	int mode;
	
	/* Unused parameter */
	(void)fmt;
	
	/* Get the object */
	o_ptr = va_arg(*vp, const object_type*);
	
	/* Get the pref */
	pref = va_arg(*vp, int);
	
	/* Get the mode */
	mode = va_arg(*vp, int);
	
	object_desc(buf, o_ptr, pref, mode, max);
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, const object_type *o_ptr, int pref,
                       int mode, int size)
{
	byte hack_flavor;
	bool hack_aware;
	byte info;

	/* Hack - we will reset the object to exactly like it was */
	object_type *q_ptr = (object_type *)o_ptr;

	/* Save the "flavor" */
	hack_flavor = k_info[o_ptr->k_idx].flavor;

	/* Save the "aware" flag */
	hack_aware = k_info[o_ptr->k_idx].aware;

	/* Save the "info" */
	info = o_ptr->info;

	/* Clear the flavor */
	k_info[o_ptr->k_idx].flavor = FALSE;

	/* If this is a shop item or in you are in wizard mode play with objects */
	if (q_ptr->info & OB_STOREB)
	{
		/* Make it known */
		q_ptr->info |= (OB_KNOWN);

		/* Force "aware" for description */
		k_info[o_ptr->k_idx].aware = TRUE;
	}

	/* Describe the object */
	object_desc(buf, q_ptr, pref, mode, size);

	/* Restore "flavor" value */
	k_info[o_ptr->k_idx].flavor = hack_flavor;

	/* Restore "aware" flag */
	k_info[o_ptr->k_idx].aware = hack_aware;

	/* Restore the "info" */
	q_ptr->info = info;
}

/*
 * Wrapper around object_desc_store() for the '%v'
 * format option.  This allows object_desc() to be
 * called in a format string.
 *
 * The parameters are object_type (o_ptr), pref (int), mode(int).
 */
void object_store_fmt(char *buf, uint max, cptr fmt, va_list *vp)
{
	const object_type *o_ptr;
	int pref;
	int mode;
	
	/* Unused parameter */
	(void)fmt;
	
	/* Get the object */
	o_ptr = va_arg(*vp, const object_type*);
	
	/* Get the pref */
	pref = va_arg(*vp, int);
	
	/* Get the mode */
	mode = va_arg(*vp, int);
	
	object_desc_store(buf, o_ptr, pref, mode, max);
}

