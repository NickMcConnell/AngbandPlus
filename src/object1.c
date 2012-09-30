/* File: object1.c */

/* Purpose: Object code, part 1 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Hack -- note that "TERM_MULTI" is now just "TERM_VIOLET".
 * We will have to find a cleaner method for "MULTI_HUED" later.
 * There were only two multi-hued "flavors" (one potion, one food).
 * Plus five multi-hued "base-objects" (3 dragon scales, one blade
 * of chaos, and one something else).  See the SHIMMER_OBJECTS code
 * in "dungeon.c" and the object color extractor in "cave.c".
 */
#define TERM_MULTI      TERM_VIOLET


/*
 * Max sizes of the following arrays
 */
#define MAX_ROCKS      62       /* Used with rings (min 58) */
#define MAX_AMULETS    34       /* Used with amulets (min 30) */
#define MAX_WOODS      35       /* Used with staffs (min 32) */
#define MAX_METALS     39       /* Used with wands/rods (min 32/30) */
#define MAX_COLORS     66       /* Used with potions (min 62) */
#define MAX_SHROOM     20       /* Used with mushrooms (min 20) */
#define MAX_TITLES     55       /* Used with scrolls (min 55) */
#define MAX_SYLLABLES 164       /* Used with scrolls (see below) */


/*
 * Rings (adjectives and colors)
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
	"Wire", "Dilithium", "Bone", "Wooden",
	"Spikard", "Serpent", "Wedding", "Double",
	"Plain", "Brass", "Scarab", "Shining",
	"Rusty", "Transparent", "Copper", "Black Opal", "Nickel",
        "Glass", "Fluorspar", "Agate",
};

static byte ring_col[MAX_ROCKS] =
{
	TERM_GREEN, TERM_VIOLET, TERM_L_BLUE, TERM_L_BLUE, TERM_L_GREEN,
	TERM_RED, TERM_WHITE, TERM_RED, TERM_SLATE, TERM_WHITE,
	TERM_GREEN, TERM_L_GREEN, TERM_RED, TERM_L_DARK, TERM_L_GREEN,
	TERM_UMBER, TERM_BLUE, TERM_GREEN, TERM_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_L_WHITE, TERM_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_RED, TERM_RED, TERM_BLUE, TERM_YELLOW, TERM_YELLOW,
	TERM_L_BLUE, TERM_L_UMBER, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_DARK, TERM_L_WHITE, TERM_GREEN, TERM_L_BLUE, TERM_L_DARK,
	TERM_YELLOW, TERM_VIOLET,
	TERM_UMBER, TERM_L_WHITE, TERM_WHITE, TERM_UMBER,
	TERM_BLUE, TERM_GREEN, TERM_YELLOW, TERM_ORANGE,
	TERM_YELLOW, TERM_ORANGE, TERM_L_GREEN, TERM_YELLOW,
	TERM_RED, TERM_WHITE, TERM_UMBER, TERM_L_DARK, TERM_L_WHITE,
        TERM_WHITE, TERM_BLUE, TERM_L_WHITE
};


/*
 * Amulets (adjectives and colors)
 */

static cptr amulet_adj[MAX_AMULETS] =
{
	"Amber", "Driftwood", "Coral", "Agate", "Ivory",
	"Obsidian", "Bone", "Brass", "Bronze", "Pewter",
	"Tortoise Shell", "Golden", "Azure", "Crystal", "Silver",
        "Copper", "Amethyst", "Mithril", "Sapphire", "Dragon Tooth",
        "Carved Oak", "Sea Shell", "Flint Stone", "Ruby", "Scarab",
        "Origami Paper", "Meteoric Iron", "Platinum", "Glass", "Beryl",
        "Malachite", "Adamantite", "Mother-of-pearl", "Runed"
};

static byte amulet_col[MAX_AMULETS] =
{
	TERM_YELLOW, TERM_L_UMBER, TERM_WHITE, TERM_L_WHITE, TERM_WHITE,
	TERM_L_DARK, TERM_WHITE, TERM_ORANGE, TERM_L_UMBER, TERM_SLATE,
	TERM_GREEN, TERM_YELLOW, TERM_L_BLUE, TERM_L_BLUE, TERM_L_WHITE,
        TERM_L_UMBER, TERM_VIOLET, TERM_L_BLUE, TERM_BLUE, TERM_L_WHITE,
        TERM_UMBER, TERM_L_BLUE, TERM_SLATE, TERM_RED, TERM_L_GREEN, 
        TERM_WHITE, TERM_L_DARK, TERM_L_WHITE, TERM_WHITE, TERM_L_GREEN, 
        TERM_GREEN, TERM_VIOLET, TERM_L_WHITE, TERM_UMBER
};


/*
 * Staffs (adjectives and colors)
 */

static cptr staff_adj[MAX_WOODS] =
{
	"Aspen", "Balsa", "Banyan", "Birch", "Cedar",
	"Cottonwood", "Cypress", "Dogwood", "Elm", "Eucalyptus",
	"Hemlock", "Hickory", "Ironwood", "Locust", "Mahogany",
	"Maple", "Mulberry", "Oak", "Pine", "Redwood",
	"Rosewood", "Spruce", "Sycamore", "Teak", "Walnut",
	"Mistletoe", "Hawthorn", "Bamboo", "Silver", "Runed",
	"Golden", "Ashen", "Gnarled", "Ivory", "Willow"
};

static byte staff_col[MAX_WOODS] =
{
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_RED,
	TERM_RED, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_GREEN, TERM_L_UMBER, TERM_L_UMBER, TERM_L_WHITE, TERM_UMBER,
	TERM_YELLOW, TERM_SLATE, TERM_UMBER, TERM_L_WHITE, TERM_L_UMBER
};


/*
 * Wands (adjectives and colors)
 */

static cptr wand_adj[MAX_METALS] =
{
	"Aluminium", "Cast Iron", "Chromium", "Copper", "Gold",
	"Iron", "Magnesium", "Molybdenum", "Nickel", "Rusty",
	"Silver", "Steel", "Tin", "Titanium", "Tungsten",
	"Zirconium", "Zinc", "Aluminium-Plated", "Copper-Plated", "Gold-Plated",
	"Nickel-Plated", "Silver-Plated", "Steel-Plated", "Tin-Plated", "Zinc-Plated",
	"Mithril-Plated", "Mithril", "Runed", "Bronze", "Brass",
	"Platinum", "Lead","Lead-Plated", "Ivory" , "Adamantite",
	"Uridium", "Long", "Short", "Hexagonal"
};

static byte wand_col[MAX_METALS] =
{
	TERM_L_BLUE, TERM_L_DARK, TERM_WHITE, TERM_UMBER, TERM_YELLOW,
	TERM_SLATE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_RED,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_WHITE, TERM_WHITE,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_BLUE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_UMBER, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_L_BLUE, TERM_L_BLUE, TERM_UMBER, TERM_L_UMBER, TERM_L_UMBER,
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
	"Blue", "Black", "Black Spotted", "Brown", "Dark Blue",
	"Dark Green", "Dark Red", "Yellow", "Furry", "Green",
	"Grey", "Light Blue", "Light Green", "Violet", "Red",
	"Slimy", "Tan", "White", "White Spotted", "Wrinkled",
};

static byte food_col[MAX_SHROOM] =
{
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

static cptr potion_adj[MAX_COLORS] =
{
        "Clear", "Light Brown", "Icky Green", "Strangely Phosphorescent",
	"Azure", "Blue", "Blue Speckled", "Black", "Brown", "Brown Speckled",
	"Bubbling", "Chartreuse", "Cloudy", "Copper Speckled", "Crimson", "Cyan",
	"Dark Blue", "Dark Green", "Dark Red", "Gold Speckled", "Green",
	"Green Speckled", "Grey", "Grey Speckled", "Hazy", "Indigo",
	"Light Blue", "Light Green", "Magenta", "Metallic Blue", "Metallic Red",
	"Metallic Green", "Metallic Purple", "Misty", "Orange", "Orange Speckled",
	"Pink", "Pink Speckled", "Puce", "Purple", "Purple Speckled",
	"Red", "Red Speckled", "Silver Speckled", "Smoky", "Tangerine",
	"Violet", "Vermilion", "White", "Yellow", "Violet Speckled",
	"Pungent", "Clotted Red", "Viscous Pink", "Oily Yellow", "Gloopy Green",
	"Shimmering", "Coagulated Crimson", "Yellow Speckled", "Gold",
	"Manly", "Stinking", "Oily Black", "Ichor", "Ivory White", "Sky Blue",
};

static byte potion_col[MAX_COLORS] =
{
        TERM_WHITE, TERM_L_UMBER, TERM_GREEN, TERM_MULTI,
	TERM_L_BLUE, TERM_BLUE, TERM_BLUE, TERM_L_DARK, TERM_UMBER, TERM_UMBER,
	TERM_L_WHITE, TERM_L_GREEN, TERM_WHITE, TERM_L_UMBER, TERM_RED, TERM_L_BLUE,
	TERM_BLUE, TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_GREEN,
	TERM_GREEN, TERM_SLATE, TERM_SLATE, TERM_L_WHITE, TERM_VIOLET,
	TERM_L_BLUE, TERM_L_GREEN, TERM_RED, TERM_BLUE, TERM_RED,
	TERM_GREEN, TERM_VIOLET, TERM_L_WHITE, TERM_ORANGE, TERM_ORANGE,
	TERM_L_RED, TERM_L_RED, TERM_VIOLET, TERM_VIOLET, TERM_VIOLET,
	TERM_RED, TERM_RED, TERM_L_WHITE, TERM_L_DARK, TERM_ORANGE,
	TERM_VIOLET, TERM_RED, TERM_WHITE, TERM_YELLOW, TERM_VIOLET,
	TERM_L_RED, TERM_RED, TERM_L_RED, TERM_YELLOW, TERM_GREEN,
	TERM_MULTI, TERM_RED, TERM_YELLOW, TERM_YELLOW,
	TERM_L_UMBER, TERM_UMBER, TERM_L_DARK, TERM_RED, TERM_WHITE, TERM_L_BLUE
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
	"vom", "wah", "wed", "werg", "wex", "whon", "wun", "x",
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
                case TV_POTION2:
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


void get_table_name(char * out_string)
{
	int testcounter = (randint(3)) + 1;

	strcpy(out_string, "'");

	if (randint(3)==2)
	{
		while (testcounter--)
			strcat(out_string, syllables[(randint(MAX_SYLLABLES))-1]);
	}

	else
	{
		char Syllable[80];
		testcounter = (randint(2)) + 1;
		while (testcounter--)
		{
			get_rnd_line("elvish.txt", Syllable);
			strcat(out_string, Syllable);
		}
	}

	out_string[1] = toupper(out_string[1]);

	strcat(out_string, "'");

	out_string[18] = '\0';

	return;
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
                case TV_ILLUSION_BOOK:
                case TV_DRUID_BOOK:
                case TV_TRIBAL_BOOK:
                case TV_MAGIC_BOOK:
                case TV_PRAYER_BOOK:
                case TV_VALARIN_BOOK:
                case TV_SPIRIT_BOOK:
                case TV_MAGERY_BOOK:
                case TV_SHADOW_BOOK:
		case TV_CHAOS_BOOK:
                case TV_NETHER_BOOK:
                case TV_CRUSADE_BOOK:
                case TV_SIGALDRY_BOOK:
                case TV_MUSIC_BOOK:
                case TV_SYMBIOTIC_BOOK:
		{
			return (TRUE);
		}

		/* Simple items */
		case TV_FLASK:
                case TV_EGG:
                case TV_JUNK:
                case TV_FIRESTONE:
		case TV_BOTTLE:
		case TV_SKELETON:
                case TV_CORPSE:
                case TV_HYPNOS:
		case TV_SPIKE:
		{
			return (TRUE);
		}

		/* All Food, Potions, Scrolls, Rods */
		case TV_FOOD:
		case TV_POTION:
                case TV_POTION2:
		case TV_SCROLL:
		case TV_ROD:
                case TV_ROD_MAIN:
                case TV_BATERIE:
		{
			if (k_ptr->flags3 & TR3_NORM_ART)
				return( FALSE );
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
 *
 * Note that the "hacked seed" may provide an RNG with alternating parity!
 */
void flavor_init(void)
{
	int     i, j;

	byte    temp_col;

	cptr    temp_adj;


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
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		/* Extract "flavor" (if any) */
		k_ptr->flavor = object_flavor(i);

		/* No flavor yields aware */
                if ((!k_ptr->flavor) && (k_ptr->tval != TV_ROD_MAIN)) k_ptr->aware = TRUE;

		/* Check for "easily known" */
		k_ptr->easy_know = object_easy_know(i);
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
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful.  XXX XXX XXX
 */
void reset_visuals(void)
{
	int i;

	/* Extract some info about terrain features */
	for (i = 0; i < max_f_idx; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Assume we will use the underlying values */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;
	}

	/* Extract default attr/char code for stores */
	for (i = 0; i < max_st_idx; i++)
	{
		store_info_type *st_ptr = &st_info[i];

		/* Default attr/char */
		st_ptr->x_attr = st_ptr->d_attr;
		st_ptr->x_char = st_ptr->d_char;
	}

	/* Extract default attr/char code for objects */
	for (i = 0; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Default attr/char */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;
	}

	/* Extract default attr/char code for monsters */
	for (i = 0; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Default attr/char */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;
	}


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









/*
 * Obtain the "flags" for an item
 */
void object_flags(object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4, u32b *f5, u32b *esp)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Base object */
	(*f1) = k_ptr->flags1;
	(*f2) = k_ptr->flags2;
	(*f3) = k_ptr->flags3;
        (*f4) = k_ptr->flags4;
        (*f5) = k_ptr->flags5;
        (*esp) = k_ptr->esp;

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		(*f1) = a_ptr->flags1;
		(*f2) = a_ptr->flags2;
		(*f3) = a_ptr->flags3;
                (*f4) = a_ptr->flags4;
                (*f5) = a_ptr->flags5;
                (*esp) = a_ptr->esp;
	}

	/* Random artifact ! */
        if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags3 || o_ptr->art_flags4 || o_ptr->art_flags5 || o_ptr->art_esp)
	{
		(*f1) |= o_ptr->art_flags1;
		(*f2) |= o_ptr->art_flags2;
		(*f3) |= o_ptr->art_flags3;
                (*f4) |= o_ptr->art_flags4;
                (*f5) |= o_ptr->art_flags5;
                (*esp) |= o_ptr->art_esp;
	}

	/* Extra powers */
	if (!(o_ptr->art_name))
	{
		switch (o_ptr->xtra1)
		{
			case EGO_XTRA_SUSTAIN:
			{
				/* Choose a sustain */
				switch (o_ptr->xtra2 % 6)
				{
					case 0: (*f2) |= (TR2_SUST_STR); break;
					case 1: (*f2) |= (TR2_SUST_INT); break;
					case 2: (*f2) |= (TR2_SUST_WIS); break;
					case 3: (*f2) |= (TR2_SUST_DEX); break;
					case 4: (*f2) |= (TR2_SUST_CON); break;
					case 5: (*f2) |= (TR2_SUST_CHR); break;
				}

				break;
			}

			case EGO_XTRA_POWER:
			{
				/* Choose a power */
				switch (o_ptr->xtra2 % 11)
				{
					case  0: (*f2) |= (TR2_RES_BLIND);  break;
					case  1: (*f2) |= (TR2_RES_CONF);   break;
					case  2: (*f2) |= (TR2_RES_SOUND);  break;
					case  3: (*f2) |= (TR2_RES_SHARDS); break;
					case  4: (*f2) |= (TR2_RES_NETHER); break;
					case  5: (*f2) |= (TR2_RES_NEXUS);  break;
					case  6: (*f2) |= (TR2_RES_CHAOS);  break;
					case  7: (*f2) |= (TR2_RES_DISEN);  break;
					case  8: (*f2) |= (TR2_RES_POIS);   break;
					case  9: (*f2) |= (TR2_RES_DARK);   break;
					case 10: (*f2) |= (TR2_RES_LITE);   break;
				}

				break;
			}

			case EGO_XTRA_ABILITY:
			{
				/* Choose an ability */
				switch (o_ptr->xtra2 % 8)
				{
					case 0: (*f3) |= (TR3_FEATHER);     break;
                                        case 1: (*f3) |= (TR3_LITE1);        break;
					case 2: (*f3) |= (TR3_SEE_INVIS);   break;
                                        case 3: (*esp) |= (ESP_ALL);   break;
					case 4: (*f3) |= (TR3_SLOW_DIGEST); break;
					case 5: (*f3) |= (TR3_REGEN);       break;
					case 6: (*f2) |= (TR2_FREE_ACT);    break;
					case 7: (*f2) |= (TR2_HOLD_LIFE);   break;
				}

				break;
			}
		}
	}
}

/* Return object granted power */
int object_power(object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
        int power = -1;

	/* Base object */
        power = k_ptr->power;

	/* Ego-item */
	if (o_ptr->name2)
	{
		ego_item_type *e_ptr = &e_info[o_ptr->name2];

                if (power == -1) power = e_ptr->power;
	}

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

                if (power == -1) power = a_ptr->power;
	}

        return (power);
}



/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4, u32b *f5, u32b *esp)
{
	bool spoil = FALSE;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Clear */
        (*f1) = (*f2) = (*f3) = (*f4) = (*esp) = (*f5) = 0L;

	/* Must be identified */
	if (!object_known_p(o_ptr)) return;

	/* Base object */
	(*f1) = k_ptr->flags1;
	(*f2) = k_ptr->flags2;
	(*f3) = k_ptr->flags3;
        (*f4) = k_ptr->flags4;
        (*f5) = k_ptr->flags5;
        (*f4) = k_ptr->esp;


#ifdef SPOIL_ARTIFACTS
	/* Full knowledge for some artifacts */
	if (artifact_p(o_ptr) || o_ptr->art_name) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

#ifdef SPOIL_EGO_ITEMS
	/* Full knowledge for some ego-items */
	if (ego_item_p(o_ptr)) spoil = TRUE;
#endif /* SPOIL_EGO_ITEMS */

	/* Need full knowledge or spoilers */
	if (!spoil && !(o_ptr->ident & IDENT_MENTAL)) return;

	/* Artifact */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		(*f1) = a_ptr->flags1;
		(*f2) = a_ptr->flags2;
		(*f3) = a_ptr->flags3;
                (*f4) = a_ptr->flags4;
                (*f5) = a_ptr->flags5;
                (*esp) = a_ptr->esp;
	}

	/* Random artifact ! */
        if (o_ptr->art_flags1 || o_ptr->art_flags2 || o_ptr->art_flags4 || o_ptr->art_flags5 || o_ptr->art_esp)
	{
		(*f1) |= o_ptr->art_flags1;
		(*f2) |= o_ptr->art_flags2;
		(*f3) |= o_ptr->art_flags3;
                (*f4) |= o_ptr->art_flags4;
                (*f5) |= o_ptr->art_flags5;
                (*esp) |= o_ptr->art_esp;
	}

	/* Full knowledge for *identified* objects */
	if (!(o_ptr->ident & IDENT_MENTAL)) return;


	if (!(o_ptr->art_name))
	{
		/* Extra powers */
		switch (o_ptr->xtra1)
		{
			case EGO_XTRA_SUSTAIN:
			{
				/* Choose a sustain */
				switch (o_ptr->xtra2 % 6)
				{
					case 0: (*f2) |= (TR2_SUST_STR); break;
					case 1: (*f2) |= (TR2_SUST_INT); break;
					case 2: (*f2) |= (TR2_SUST_WIS); break;
					case 3: (*f2) |= (TR2_SUST_DEX); break;
					case 4: (*f2) |= (TR2_SUST_CON); break;
					case 5: (*f2) |= (TR2_SUST_CHR); break;
				}

				break;
			}

			case EGO_XTRA_POWER:
			{
				/* Choose a power */
				switch (o_ptr->xtra2 % 11)
				{
					case  0: (*f2) |= (TR2_RES_BLIND);  break;
					case  1: (*f2) |= (TR2_RES_CONF);   break;
					case  2: (*f2) |= (TR2_RES_SOUND);  break;
					case  3: (*f2) |= (TR2_RES_SHARDS); break;
					case  4: (*f2) |= (TR2_RES_NETHER); break;
					case  5: (*f2) |= (TR2_RES_NEXUS);  break;
					case  6: (*f2) |= (TR2_RES_CHAOS);  break;
					case  7: (*f2) |= (TR2_RES_DISEN);  break;
					case  8: (*f2) |= (TR2_RES_POIS);   break;
					case  9: (*f2) |= (TR2_RES_DARK);   break;
					case 10: (*f2) |= (TR2_RES_LITE);   break;
				}

				break;
			}

			case EGO_XTRA_ABILITY:
			{
				/* Choose an ability */
				switch (o_ptr->xtra2 % 8)
				{
					case 0: (*f3) |= (TR3_FEATHER);     break;
                                        case 1: (*f3) |= (TR3_LITE1);        break;
					case 2: (*f3) |= (TR3_SEE_INVIS);   break;
                                        case 3: (*esp) |= (ESP_ALL);   break;
					case 4: (*f3) |= (TR3_SLOW_DIGEST); break;
					case 5: (*f3) |= (TR3_REGEN);       break;
					case 6: (*f2) |= (TR2_FREE_ACT);    break;
					case 7: (*f2) |= (TR2_HOLD_LIFE);   break;
				}

				break;
			}
		}
	}
}





/*
 * Print a char "c" into a string "t", as if by sprintf(t, "%c", c),
 * and return a pointer to the terminator (t + 1).
 */
static char *object_desc_chr(char *t, char c)
{
	/* Copy the char */
	*t++ = c;

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}


/*
 * Print a string "s" into a string "t", as if by strcpy(t, s),
 * and return a pointer to the terminator.
 */
static char *object_desc_str(char *t, cptr s)
{
	/* Copy the string */
	while (*s) *t++ = *s++;

	/* Terminate */
	*t = '\0';

	/* Result */
	return (t);
}



/*
 * Print a nnumber "n" into a string "t", as if by
 * sprintf(t, "%u", n), and return a pointer to the terminator.
 */
static char *object_desc_num(char *t, int n)
{
	uint p;

	if(n < 0) {*t++ = '-'; n *= -1;}
	/* Find "size" of "n" */
	for (p = 1; n >= p * 10; p = p * 10) /* loop */;

	/* Dump each digit */
	while (p >= 1)
	{
		/* Dump the digit */
		*t++ = '0' + n / p;

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
static char *object_desc_int(char *t, sint v)
{
	uint p, n;

	/* Negative */
	if (v < 0)
	{
		/* Take the absolute value */
		n = 0 - v;

		/* Use a "minus" sign */
		*t++ = '-';
	}

	/* Positive (or zero) */
	else
	{
		/* Use the actual number */
		n = v;

		/* Use a "plus" sign */
		*t++ = '+';
	}

	/* Find "size" of "n" */
	for (p = 1; n >= p * 10; p = p * 10) /* loop */;

	/* Dump each digit */
	while (p >= 1)
	{
		/* Dump the digit */
		*t++ = '0' + n / p;

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
 */
void object_desc(char *buf, object_type *o_ptr, int pref, int mode)
{
	cptr            basenm, modstr;
	int                     power, indexx;

	bool            aware = FALSE;
	bool            known = FALSE;

	bool            append_name = FALSE;

	bool            show_weapon = FALSE;
	bool            show_armour = FALSE;

	cptr            s, u;
	char            *t;

	char            p1 = '(', p2 = ')';
	char            b1 = '[', b2 = ']';
	char            c1 = '{', c2 = '}';

	char            tmp_val[160];
	char            tmp_val2[90];

        u32b            f1, f2, f3, f4, f5, esp;

	object_kind             *k_ptr = &k_info[o_ptr->k_idx];

        cptr str;

	/* Extract some flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);


	/* See if the object is "aware" */
	if (object_aware_p(o_ptr)) aware = TRUE;

	/* See if the object is "known" */
	if (object_known_p(o_ptr)) known = TRUE;

	/* Hack -- Extract the sub-type "indexx" */
	indexx = o_ptr->sval;

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
                case TV_FIRESTONE:
		case TV_SPIKE:
		case TV_FLASK:
		case TV_CHEST:
                case TV_INSTRUMENT:
                case TV_TOOL:
		{
			break;
		}


			/* Missiles/ Bows/ Weapons */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
                case TV_BOOMERANG:
		case TV_BOW:
		case TV_HAFTED:
		case TV_POLEARM:
                case TV_MSTAFF:
		case TV_SWORD:
                case TV_AXE:
		case TV_DIGGING:
		{
			show_weapon = TRUE;
			break;
		}

		/* Trapping Kits */
		case TV_TRAPKIT:
		{
			modstr = basenm;
			basenm = "& # Trap Set~";
			break;
		}

			/* Armour */
		case TV_BOOTS:
		case TV_GLOVES:
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
			/* Color the object */
			modstr = amulet_adj[indexx];
			if (aware) append_name = TRUE;

                        if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
                                basenm = "& Amulet~";
                        else
                                basenm = aware ? "& # Amulet~" : "& # Amulet~";

                        if (known && !o_ptr->art_name && artifact_p(o_ptr)) basenm = k_ptr->name + k_name;
                        
                        break;
		}

		/* Rings (including a few "Specials") */
		case TV_RING:
		{
			/* Color the object */
			modstr = ring_adj[indexx];
			if (aware) append_name = TRUE;

			if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
				basenm = "& Ring~";
			else
				basenm = aware ? "& # Ring~" : "& # Ring~";

                        /* Hack -- The One Ring */
			if (!aware && (o_ptr->sval == SV_RING_POWER)) modstr = "Plain Gold";

                        if (known && !o_ptr->art_name && artifact_p(o_ptr)) basenm = k_ptr->name + k_name;

			break;
		}

		case TV_STAFF:
		{
			/* Color the object */
			modstr = staff_adj[indexx];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
				basenm = "& Staff~";
			else
				basenm = aware ? "& # Staff~" : "& # Staff~";
			break;
		}

        case TV_WAND:
		{
			/* Color the object */
			modstr = wand_adj[indexx];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
				basenm = "& Wand~";
			else
				basenm = aware ? "& # Wand~" : "& # Wand~";
			break;
		}

		case TV_ROD:
		{
			/* Color the object */
			modstr = rod_adj[indexx];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
                                basenm = "& Rod Tip~";
			else
                                basenm = aware ? "& # Rod Tip~" : "& # Rod Tip~";
			break;
		}

                case TV_ROD_MAIN:
		{
                        object_kind *tip_ptr = &k_info[lookup_kind(TV_ROD, o_ptr->pval)];

                        modstr = k_name + tip_ptr->name;
			break;
		}

		case TV_SCROLL:
		{
			/* Color the object */
			modstr = scroll_adj[indexx];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware)) || o_ptr->ident & IDENT_STOREB)
				basenm = "& Scroll~";
			else
				basenm = aware ? "& Scroll~ titled \"#\"" : "& Scroll~ titled \"#\"";
			break;
		}

		case TV_POTION:
                case TV_POTION2:
		{
			/* Color the object */
			modstr = potion_adj[indexx];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
				basenm = "& Potion~";
			else
				basenm = aware ? "& # Potion~" : "& # Potion~";
			break;
		}

		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (o_ptr->sval >= SV_FOOD_MIN_FOOD) break;

			/* Color the object */
			modstr = food_adj[indexx];
			if (aware) append_name = TRUE;
			if (((plain_descriptions) && (aware))  || o_ptr->ident & IDENT_STOREB)
				basenm = "& Mushroom~";
			else
				basenm = aware ? "& # Mushroom~" : "& # Mushroom~";
			break;
		}


			/* Magic Books */
        case TV_CLOAK:
                show_armour = TRUE;
                if (o_ptr->sval >= 100)
                {
                        modstr = basenm;
                        basenm = "& Cloak~ of Mimicry #";
                }
                break;

        case TV_VALARIN_BOOK:
		modstr = basenm;
                if (mp_ptr->spell_book == TV_VALARIN_BOOK)
                        basenm = "& Book~ of Valarin Magic #";
		else
                        basenm = "& Valarin Spellbook~ #";
		break;

        case TV_MAGERY_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Magery #";
            else
                basenm = "& Magery Spellbook~ #";
			break;
		}

        case TV_SHADOW_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Shadow Magic #";
            else
                basenm = "& Shadow Spellbook~ #";
			break;
		}

        case TV_SPIRIT_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Spirit Magic #";
            else
                basenm = "& Spirit Spellbook~ #";
			break;
		}

	case TV_CHAOS_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Chaos Magic #";
            else
                basenm = "& Chaos Spellbook~ #";
			break;
		}
        case TV_NETHER_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Nether Magic #";
            else
                basenm = "& Nether Spellbook~ #";
			break;
		}


    case TV_CRUSADE_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Crusade Book~ #";
            else
                basenm = "& Crusade Spellbook~ #";
			break;
		}

    case TV_SIGALDRY_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Sigaldry Magic #";
            else
                basenm = "& Sigaldry Spellbook~ #";
			break;
		}

    case TV_SYMBIOTIC_BOOK:
		{
			modstr = basenm;
            if(mp_ptr->spell_book == TV_VALARIN_BOOK)
                basenm = "& Book~ of Symbiotic Magic #";
            else
                basenm = "& Symbiotic Spellbook~ #";
			break;
		}

        case TV_MUSIC_BOOK:
		{
			modstr = basenm;
                        basenm = "& Songbook~ #";
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

                /* Illusion Books */
                case TV_ILLUSION_BOOK:
		{
			modstr = basenm;
                        basenm = "& Book~ of Illusions #";
			break;
		}

                /* Tribal Books */
                case TV_TRIBAL_BOOK:
		{
			modstr = basenm;
                        basenm = "& Tribal Spellbook~ #";
			break;
		}

                /* Daemon Books */
                case TV_DAEMON_BOOK:
		{
			modstr = basenm;
                        basenm = "& Daemon Blade~ #";
                        show_weapon = TRUE;
			break;
		}
		
                /* Druid Books */
                case TV_DRUID_BOOK:
		{
			modstr = basenm;
                        basenm = "& Elemental Stone~ #";
			break;
		}

            case TV_BATERIE:
		{
			modstr = basenm;
                        basenm = "& Essence~ of #";
			break;
		}

            case TV_PARCHEMENT:
		{
			modstr = basenm;
                        basenm = "& Parchment~ - #";
			break;
		}


			/* Hack -- Gold/Gems */
		case TV_GOLD:
		{
			strcpy(buf, basenm);
			return;
		}

                case TV_CORPSE:
                {
                        monster_race* r_ptr = &r_info[o_ptr->pval2];
			modstr = basenm;
                        if(r_ptr->flags1 & RF1_UNIQUE)
                                basenm = format("& %s's #~",r_name + r_ptr->name);
                        else
                                basenm = format("& %s #~",r_name + r_ptr->name);
                        break;
                }

                case TV_EGG:
                {
                        monster_race* r_ptr = &r_info[o_ptr->pval2];
			modstr = basenm;

                        basenm = format("& %s #~",r_name + r_ptr->name);
                        break;
                }

                case TV_HYPNOS:
                {
                        monster_race* r_ptr = &r_info[o_ptr->pval];
			modstr = basenm;
                        basenm = format("& %s~ (%d hp)",r_name + r_ptr->name,o_ptr->pval2);
                        break;
                }

	case TV_RANDART:
	  {
	    modstr = basenm;

	    if (known) {
	      basenm = random_artifacts[indexx].name_full;
	    } else {
	      basenm = random_artifacts[indexx].name_short;
	    }
            pref = FALSE;
	    break;
	  }

                case TV_RUNE2:
		{
                        if (o_ptr->sval != RUNE_STONE)
                        {
                                modstr = basenm;
                                basenm = "& Rune~ [#]";
                        }
                        break;
		}
                case TV_RUNE1:
		{
			modstr = basenm;
                        basenm = "& Rune~ [#]";
			break;
		}


			/* Used in the "inventory" routine */
		default:
		{
			strcpy(buf, "(nothing)");
			return;
		}
	}


	/* Start dumping the result */
	t = tmp_val;

	/* The object "expects" a "number" */
	if (basenm[0] == '&')
	{
                monster_race* r_ptr;
                cptr ego = NULL;

                if(o_ptr->tval==TV_CORPSE) r_ptr = &r_info[o_ptr->pval2];
                else r_ptr = &r_info[o_ptr->pval];

		/* Grab any ego-item name */
                if (known && o_ptr->name2 && (o_ptr->tval != TV_ROD_MAIN))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

                        if (e_ptr->before)
                        {
                                ego = e_ptr->name + e_name;
                        }
                }

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
			t = object_desc_str(t, "no more ");
		}

		/* Extract the number */
		else if (o_ptr->number > 1)
		{
			t = object_desc_num(t, o_ptr->number);
			t = object_desc_chr(t, ' ');
		}

                else if ((o_ptr->tval==TV_CORPSE) && (r_ptr->flags1 & RF1_UNIQUE))
                {
                }

                else if ((o_ptr->tval==TV_HYPNOS) && (r_ptr->flags1 & RF1_UNIQUE))
                {
                }

		/* Hack -- The only one of its kind */
                else if (known && (artifact_p(o_ptr) || o_ptr->art_name))
		{
			t = object_desc_str(t, "The ");
		}

                else if (ego != NULL)
                {
                        if (is_a_vowel(ego[0]))
                        {
                                t = object_desc_str(t, "an ");
                        }
                        else
                        {
                                t = object_desc_str(t, "a ");
                        }
                }

		/* A single one, with a vowel in the modifier */
		else if ((*s == '#') && (is_a_vowel(modstr[0])))
		{
			t = object_desc_str(t, "an ");
		}

		/* A single one, with a vowel */
		else if (is_a_vowel(*s))
		{
			t = object_desc_str(t, "an ");
		}

		/* A single one, without a vowel */
		else
		{
			t = object_desc_str(t, "a ");
		}

		/* Grab any ego-item name */
                if (known && o_ptr->name2 && (o_ptr->tval != TV_ROD_MAIN))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

                        if (e_ptr->before)
                        {
                                t = object_desc_str(t, (e_name + e_ptr->name));
                                t = object_desc_chr(t, ' ');
                        }
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
			t = object_desc_str(t, "no more ");
		}

		/* Prefix a number if required */
		else if (o_ptr->number > 1)
		{
			t = object_desc_num(t, o_ptr->number);
			t = object_desc_chr(t, ' ');
		}

		/* Hack -- The only one of its kind */
                else if (known && (artifact_p(o_ptr) || o_ptr->art_name))
		{
			t = object_desc_str(t, "The ");
		}

		/* Hack -- single items get no prefix */
		else
		{
			/* Nothing */
		}

		/* Grab any ego-item name */
                if (known && o_ptr->name2 && (o_ptr->tval != TV_ROD_MAIN))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

                        if (e_ptr->before)
                        {
                                t = object_desc_str(t, (e_name + e_ptr->name));
                                t = object_desc_chr(t, ' ');
                        }
		}
	}

	/* Paranoia -- skip illegal tildes */
	/* while (*s == '~') s++; */

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
				if ((k == 's') || (k == 'h')) *t++ = 'e';

				/* Add an 's' */
				*t++ = 's';
			}
		}

		/* Modifier */
		else if (*s == '#')
		{
                        /* Grab any ego-item name */
                        if (o_ptr->tval == TV_ROD_MAIN)
                        {
                                t = object_desc_chr(t, ' ');

                                if (known && o_ptr->name2)
                                {
                                        ego_item_type *e_ptr = &e_info[o_ptr->name2];

                                        t = object_desc_str(t, (e_name + e_ptr->name));
                                }
                        }

			/* Insert the modifier */
			for (u = modstr; *u; u++) *t++ = *u;
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
        if ((append_name) && ( (!artifact_p(o_ptr) || (k_ptr->flags3 & TR3_NORM_ART))))
	{
		t = object_desc_str(t, " of ");
		t = object_desc_str(t, (k_name + k_ptr->name));

                if((o_ptr->tval == TV_SCROLL) && (o_ptr->sval == SV_SCROLL_SPELL) && aware)
                {
                        t = object_desc_str(t, format("[%s]", spell_names[o_ptr->pval][o_ptr->pval2][0]));
                }
	}

	/* Hack -- Append "Artifact" or "Special" names */
	if (known)
	{

                /* -TM- Hack -- Add false-artifact names */
                /* Dagger inscribed {@w0#of Smell} will be named
                 * Dagger of Smell {@w0} */
                if (o_ptr->note)
                {
                        str = strchr(quark_str(o_ptr->note), '#');

                        /* Add the false name */
                        if (str)
                        {
                                t = object_desc_chr(t, ' ');
                                t = object_desc_str(t, &str[1]);
                        }
                }

                /* Is it a new random artifact ? */
                if (o_ptr->art_name)
                {
                        t = object_desc_chr(t, ' ');

                        t = object_desc_str(t, quark_str(o_ptr->art_name));
                }
	

		/* Grab any artifact name */
                else if (o_ptr->name1)
                {
                        artifact_type *a_ptr = &a_info[o_ptr->name1];

                        t = object_desc_chr(t, ' ');
                        t = object_desc_str(t, (a_name + a_ptr->name));
                }

		/* Grab any ego-item name */
                else if (o_ptr->name2 && (o_ptr->tval != TV_ROD_MAIN))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

                        if (!e_ptr->before)
                        {
                                t = object_desc_chr(t, ' ');
                                t = object_desc_str(t, (e_name + e_ptr->name));
                        }
		}
	}


	/* No more details wanted */
	if (mode < 1) goto copyback;

        /* Hack -- Some objects can have an exp level */
        if ((f4 & TR4_LEVELS) && known)
	{
                t = object_desc_str(t, " (E:");
                t = object_desc_num(t, o_ptr->exp);
                t = object_desc_str(t, ", L:");
                t = object_desc_num(t, o_ptr->elevel);
                t = object_desc_chr(t, ')');
        }
        if ((f4 & TR4_ART_EXP) && known)
	{
                t = object_desc_str(t, " (Exp:");
                t = object_desc_num(t, o_ptr->exp);
                t = object_desc_chr(t, ')');
        }

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
			t = object_desc_str(t, " (empty)");
		}

		/* May be "disarmed" */
		else if (o_ptr->pval < 0)
		{
			t = object_desc_str(t, " (disarmed)");
		}

		/* Describe the traps, if any */
		else
		{
			/* Describe the traps */
			t = object_desc_str(t, " (");
			if (t_info[o_ptr->pval].ident)
				t = object_desc_str(t, t_name + t_info[o_ptr->pval].name);
			else
				t = object_desc_str(t, "trapped");
			t = object_desc_str(t, ")");
		}
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
		/* Missiles and Weapons */
		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
	        /* Exploding arrow? */
                if (o_ptr->pval2 != 0)
                        t = object_desc_str(t, " (exploding)");
                /* No break, we want to continue the description */

                case TV_BOOMERANG:
		case TV_HAFTED:
		case TV_POLEARM:
                case TV_MSTAFF:
                case TV_AXE:
		case TV_SWORD:
		case TV_DIGGING:
                case TV_DAEMON_BOOK:
               
		/* Append a "damage" string */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);
		t = object_desc_num(t, o_ptr->dd);
		t = object_desc_chr(t, 'd');
		t = object_desc_num(t, o_ptr->ds);
		t = object_desc_chr(t, p2);

		/* All done */
		break;


		/* Bows get a special "damage string" */
		case TV_BOW:

		/* Mega-Hack -- Extract the "base power" */
		power = (o_ptr->sval % 10);

		/* Apply the "Extra Might" flag */
                if (f3 & (TR3_XTRA_MIGHT)) power += o_ptr->pval;

		/* Append a special "damage" string */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);
		t = object_desc_chr(t, 'x');
		t = object_desc_num(t, power);
		t = object_desc_chr(t, p2);

		/* All done */
		break;
	}


	/* Add the weapon bonuses */
	if (known)
	{
		/* Show the tohit/todam on request */
		if (show_weapon)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_int(t, o_ptr->to_h);
			t = object_desc_chr(t, ',');
			t = object_desc_int(t, o_ptr->to_d);
			t = object_desc_chr(t, p2);
		}

		/* Show the tohit if needed */
		else if (o_ptr->to_h)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_int(t, o_ptr->to_h);
			t = object_desc_chr(t, p2);
		}

		/* Show the todam if needed */
		else if (o_ptr->to_d)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, p1);
			t = object_desc_int(t, o_ptr->to_d);
			t = object_desc_chr(t, p2);
		}
	}


	/* Add the armor bonuses */
	if (known)
	{
		/* Show the armor class info */
		if (show_armour)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, b1);
			t = object_desc_num(t, o_ptr->ac);
			t = object_desc_chr(t, ',');
			t = object_desc_int(t, o_ptr->to_a);
			t = object_desc_chr(t, b2);
		}

		/* No base armor, but does increase armor */
		else if (o_ptr->to_a)
		{
			t = object_desc_chr(t, ' ');
			t = object_desc_chr(t, b1);
			t = object_desc_int(t, o_ptr->to_a);
			t = object_desc_chr(t, b2);
		}
	}

	/* Hack -- always show base armor */
	else if (show_armour)
	{
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, b1);
		t = object_desc_num(t, o_ptr->ac);
		t = object_desc_chr(t, b2);
	}

        if ((f1 & TR1_MANA) && (known) && (o_ptr->pval > 0))
        {
                t = object_desc_chr(t, '(');
                if (munchkin_multipliers)
                {
                        t = object_desc_num(t, 100 * o_ptr->pval / 5);
                }
                else
                {
                        t = object_desc_num(t, 100 * o_ptr->pval / 10);
                }
                t = object_desc_str(t, "%)");
        }

        if ((known) && (f2 & TR2_LIFE) ) /* Can disp neg now -- Improv */
        {
                t = object_desc_chr(t, '(');
                if (munchkin_multipliers)
                {
                        t = object_desc_num(t, 100 * o_ptr->pval / 5);
                }
                else
                {
                        t = object_desc_num(t, 100 * o_ptr->pval / 10);
                }
                t = object_desc_str(t, "%)");
        }

	/* No more details wanted */
	if (mode < 2) goto copyback;


	/* Hack -- Wands and Staffs have charges */
	if (known &&
	    ((o_ptr->tval == TV_STAFF) ||
	     (o_ptr->tval == TV_WAND)))
	{
		/* Dump " (N charges)" */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);
		t = object_desc_num(t, o_ptr->pval);
		t = object_desc_str(t, " charge");
		if (o_ptr->pval != 1) t = object_desc_chr(t, 's');
		t = object_desc_chr(t, p2);
	}

        /*
         * Hack -- Rods have a "charging" indicator.
	 */
        else if (known && (o_ptr->tval == TV_ROD_MAIN))
	{
                /* Display prettily. */
                t = object_desc_str(t, " (");
                t = object_desc_num(t, o_ptr->timeout);
                t = object_desc_chr(t, '/');
                t = object_desc_num(t, o_ptr->pval2);
                t = object_desc_chr(t, ')');
	}

        /*
         * Hack -- Rods have a "charging" indicator.
	 */
        else if (known && (o_ptr->tval == TV_ROD))
	{
                /* Display prettily. */
                t = object_desc_str(t, " (");
                t = object_desc_num(t, o_ptr->pval);
                t = object_desc_str(t, " Mana to cast");
                t = object_desc_chr(t, ')');
	}

	/* Hack -- Process Lanterns/Torches */
        else if ((o_ptr->tval == TV_LITE) && (f4 & TR4_FUEL_LITE))
	{
		/* Hack -- Turns of light for normal lites */
		t = object_desc_str(t, " (with ");
                t = object_desc_num(t, o_ptr->timeout);
		t = object_desc_str(t, " turns of light)");
	}


	/* Dump "pval" flags for wearable items */
        if (known && ((f1 & (TR1_PVAL_MASK)) || (f5 & (TR5_PVAL_MASK))))
	{
		/* Start the display */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, p1);

		/* Dump the "pval" itself */
		t = object_desc_int(t, o_ptr->pval);

		/* Do not display the "pval" flags */
		if (f3 & (TR3_HIDE_TYPE))
		{
			/* Nothing */
		}

		/* Speed */
		else if (f1 & (TR1_SPEED))
		{
			/* Dump " to speed" */
			t = object_desc_str(t, " to speed");
		}

		/* Attack speed */
		else if (f1 & (TR1_BLOWS))
		{
			/* Add " attack" */
			t = object_desc_str(t, " attack");

			/* Add "attacks" */
			if (ABS(o_ptr->pval) != 1) t = object_desc_chr(t, 's');
		}

                /* Critical chance */
                else if (f5 & (TR5_CRIT))
		{
			/* Add " attack" */
                        t = object_desc_str(t, "% of critical hits");
		}

		/* Stealth */
		else if (f1 & (TR1_STEALTH))
		{
			/* Dump " to stealth" */
			t = object_desc_str(t, " to stealth");
		}

		/* Search */
		else if (f1 & (TR1_SEARCH))
		{
			/* Dump " to searching" */
			t = object_desc_str(t, " to searching");
		}

		/* Infravision */
		else if (f1 & (TR1_INFRA))
		{
			/* Dump " to infravision" */
			t = object_desc_str(t, " to infravision");
		}

		/* Tunneling */
		else if (f1 & (TR1_TUNNEL))
		{
			/* Nothing */
		}

		/* Finish the display */
		t = object_desc_chr(t, p2);
	}


	/* Indicate "charging" artifacts XXX XXX XXX */
        if (known && o_ptr->timeout && (o_ptr->name2 != EGO_MSTAFF_SPELL) && (o_ptr->tval!=TV_EGG) && (o_ptr->tval != TV_ROD_MAIN) && (o_ptr->tval != TV_LITE))
	{
		/* Hack -- Dump " (charging)" if relevant */
		t = object_desc_str(t, " (charging)");
	}

        /* Indicate "stopped" eggs XXX XXX XXX */
        if (known && o_ptr->timeout && (o_ptr->tval==TV_EGG))
	{
                /* Hack -- Dump " (stoped)" if relevant */
                t = object_desc_str(t, " (stopped)");
	}

        /* Indicate "charging" Mage Staffs XXX XXX XXX */
        if (known && o_ptr->timeout && (o_ptr->name2==EGO_MSTAFF_SPELL) && (o_ptr->tval!=TV_EGG))
	{
                /* Hack -- Dump " (charging spell1)" if relevant */
                t = object_desc_str(t, " (charging spell1)");
	}
        if (known && o_ptr->xtra2 && (o_ptr->name2==EGO_MSTAFF_SPELL))
	{
                /* Hack -- Dump " (charging spell2)" if relevant */
                t = object_desc_str(t, " (charging spell2)");
	}


	/* No more details wanted */
	if (mode < 3) goto copyback;


	/* No inscription yet */
	tmp_val2[0] = '\0';
	
	/* Sensed stuff */
	if (o_ptr->ident & (IDENT_SENSE))
	{
		strcpy(tmp_val2,sense_desc[o_ptr->sense]);
	}
	
	/* Hack - Note "cursed" if the item is 'known' and cursed */
	if (cursed_p(o_ptr) && (known) && (!tmp_val2[0]))
	{
		if (tmp_val2[0]) strcat(tmp_val2,", ");
		strcat(tmp_val2, "cursed");
	}
	
	/* Use the standard inscription if available */
	if (o_ptr->note)
	{
		char *u = tmp_val2;
	
		if (tmp_val2[0]) strcat(tmp_val2,", ");
	
                strcat(tmp_val2, quark_str(o_ptr->note));
	
		for (; *u && (*u != '#'); u++);
	
		*u = '\0';
	}
	
	/* Mega-Hack -- note empty wands/staffs */
	if (!known && (o_ptr->ident & (IDENT_EMPTY)))
	{
		if (tmp_val2[0]) strcat(tmp_val2,", ");
		strcat(tmp_val2, "empty");
	}
	
	/* Note "tried" if the object has been tested unsuccessfully */
	if (!aware && object_tried_p(o_ptr))
	{
		if (tmp_val2[0]) strcat(tmp_val2,", ");
		strcpy(tmp_val2, "tried");
	}
	
	/* Note the discount, if any */
	if ((o_ptr->discount) && (!tmp_val2[0]))
	{
		object_desc_num(tmp_val2, o_ptr->discount);
		strcat(tmp_val2, "% off");
	}
	
	/* Append the inscription, if any */
	if (tmp_val2[0])
	{
		int n;
	
		/* Hack -- How much so far */
		n = (t - tmp_val);
	
		/* Paranoia -- do not be stupid */
		if (n > 75) n = 75;
	
		/* Hack -- shrink the inscription */
		tmp_val2[75 - n] = '\0';
	
		/* Append the inscription */
		t = object_desc_chr(t, ' ');
		t = object_desc_chr(t, c1);
		t = object_desc_str(t, tmp_val2);
		t = object_desc_chr(t, c2);
	}
copyback:
	/* Here's where we dump the built string into buf. */
	tmp_val[79] = '\0';
	t = tmp_val;
	while((*(buf++) = *(t++))); /* copy the string over */
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, object_type *o_ptr, int pref, int mode)
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
	if (!hack_known) o_ptr->ident &= ~(IDENT_KNOWN);
}




/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr item_activation(object_type *o_ptr, byte num)
{
        u32b f1, f2, f3, f4, f5, esp;

        /* Needed hacks */
        static char rspell[2][80];

	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

	/* Require activation ability */
	if (!(f3 & (TR3_ACTIVATE))) return (NULL);


	/*
	 * We need to deduce somehow that it is a random artifact -- one
	 * problem: It could be a random artifact which has NOT YET received
	 * a name. Thus we eliminate other possibilities instead of checking
	 * for art_name
	 */

        if (o_ptr->name2 == EGO_MSTAFF_SPELL)
        {
                int gf, mod, mana;

                if (!num)
                {
                        gf = o_ptr->pval & 0xFFFF;
                        mod = o_ptr->pval3 & 0xFFFF;
                        mana = o_ptr->pval2 & 0xFF;
                }
                else
                {
                        gf = o_ptr->pval >> 16;
                        mod = o_ptr->pval3 >> 16;
                        mana = o_ptr->pval2 >> 8;
                }
                sprintf(rspell[num], "runespell(%s, %s, %d) every %d turns",
                        k_info[lookup_kind(TV_RUNE1, gf)].name + k_name,
                        k_info[lookup_kind(TV_RUNE2, mod)].name + k_name,
                        mana, mana * 10);
                return rspell[num];
        }

        if(o_ptr->tval == TV_RANDART)
        {
                return activation_info[o_ptr->pval2].desc;
        }

        if ((!(o_ptr->name1) &&
	    !(o_ptr->name2) &&
	    !(o_ptr->xtra1) &&
             (o_ptr->xtra2)))
	{
		switch (o_ptr->xtra2)
		{
			case ACT_SUNLIGHT:
			{
				return "beam of sunlight every 10 turns";
			}
			case ACT_BO_MISS_1:
			{
				return "magic missile (2d6) every 2 turns";
			}
			case ACT_BA_POIS_1:
			{
				return "stinking cloud (12), rad. 3, every 4+d4 turns";
			}
			case ACT_BO_ELEC_1:
			{
				return "lightning bolt (4d8) every 6+d6 turns";
			}
			case ACT_BO_ACID_1:
			{
				return "acid bolt (5d8) every 5+d5 turns";
			}
			case ACT_BO_COLD_1:
			{
				return "frost bolt (6d8) every 7+d7 turns";
			}
			case ACT_BO_FIRE_1:
			{
				return "fire bolt (9d8) every 8+d8 turns";
			}
			case ACT_BA_COLD_1:
			{
				return "ball of cold (48) every 400 turns";
			}
			case ACT_BA_FIRE_1:
			{
				return "ball of fire (72) every 400 turns";
			}
			case ACT_DRAIN_1:
			{
				return "drain life (100) every 100+d100 turns";
			}
			case ACT_BA_COLD_2:
			{
				return "ball of cold (100) every 300 turns";
			}
			case ACT_BA_ELEC_2:
			{
				return "ball of lightning (100) every 500 turns";
			}
			case ACT_DRAIN_2:
			{
				return "drain life (120) every 400 turns";
			}
			case ACT_VAMPIRE_1:
			{
				return "vampiric drain (3*50) every 400 turns";
			}
			case ACT_BO_MISS_2:
			{
				return "arrows (150) every 90+d90 turns";
			}
			case ACT_BA_FIRE_2:
			{
				return "fire ball (120) every 225+d225 turns";
			}
			case ACT_BA_COLD_3:
			{
				return "ball of cold (200) every 325+d325 turns";
			}
			case ACT_WHIRLWIND:
			{
				return "whirlwind attack every 250 turns";
			}
			case ACT_VAMPIRE_2:
			{
				return "vampiric drain (3*100) every 400 turns";
			}
			case ACT_CALL_CHAOS:
			{
				return "call chaos every 350 turns";
			}
			case ACT_ROCKET:
			{
				return "launch rocket (120+level) every 400 turns";
			}
			case ACT_DISP_EVIL:
			{
				return "dispel evil (level*5) every 300+d300 turns";
			}
			case ACT_DISP_GOOD:
			{
				return "dispel good (level*5) every 300+d300 turns";
			}
			case ACT_BA_MISS_3:
			{
				return "elemental breath (300) every 500 turns";
			}
			case ACT_CONFUSE:
			{
				return "confuse monster every 15 turns";
			}
			case ACT_SLEEP:
			{
				return "sleep nearby monsters every 55 turns";
			}
			case ACT_QUAKE:
			{
				return "earthquake (rad 10) every 50 turns";
			}
			case ACT_TERROR:
			{
				return "terror every 3 * (level+10) turns";
			}
			case ACT_TELE_AWAY:
			{
				return "teleport away every 200 turns";
			}
			case ACT_BANISH_EVIL:
			{
				return "banish evil every 250+d250 turns";
			}
			case ACT_GENOCIDE:
			{
				return "genocide every 500 turns";
			}
			case ACT_MASS_GENO:
			{
				return "mass genocide every 1000 turns";
			}
			case ACT_CHARM_ANIMAL:
			{
				return "charm animal every 300 turns";
			}
			case ACT_CHARM_UNDEAD:
			{
				return "enslave undead every 333 turns";
			}
			case ACT_CHARM_OTHER:
			{
				return "charm monster every 400 turns";
			}
			case ACT_CHARM_ANIMALS:
			{
				return "animal friendship every 500 turns";
			}
			case ACT_CHARM_OTHERS:
			{
				return "mass charm every 750 turns";
			}
			case ACT_SUMMON_ANIMAL:
			{
				return "summon animal every 200+d300 turns";
			}
			case ACT_SUMMON_PHANTOM:
			{
				return "summon phantasmal servant every 200+d200 turns";
			}
			case ACT_SUMMON_ELEMENTAL:
			{
				return "summon elemental every 750 turns";
			}
			case ACT_SUMMON_DEMON:
			{
				return "summon demon every 666+d333 turns";
			}
			case ACT_SUMMON_UNDEAD:
			{
				return "summon undead every 666+d333 turns";
			}
			case ACT_CURE_LW:
			{
				return "remove fear & heal 30 hp every 10 turns";
			}
			case ACT_CURE_MW:
			{
				return "heal 4d8 & wounds every 3+d3 turns";
			}
			case ACT_CURE_POISON:
			{
				return "remove fear and cure poison every 5 turns";
			}
			case ACT_REST_LIFE:
			{
				return "restore life levels every 450 turns";
			}
			case ACT_REST_ALL:
			{
				return "restore stats and life levels every 750 turns";
			}
			case ACT_CURE_700:
			{
				return "heal 700 hit points every 250 turns";
			}
			case ACT_CURE_1000:
			{
				return "heal 1000 hit points every 888 turns";
			}
			case ACT_ESP:
			{
				return "temporary ESP (dur 25+d30) every 200 turns";
			}
			case ACT_BERSERK:
			{
				return "heroism and berserk (dur 50+d50) every 100+d100 turns";
			}
			case ACT_PROT_EVIL:
			{
				return "protect evil (dur level*3 + d25) every 225+d225 turns";
			}
			case ACT_RESIST_ALL:
			{
				return "resist elements (dur 40+d40) every 200 turns";
			}
			case ACT_SPEED:
			{
				return "speed (dur 20+d20) every 250 turns";
			}
			case ACT_XTRA_SPEED:
			{
				return "speed (dur 75+d75) every 200+d200 turns";
			}
			case ACT_WRAITH:
			{
				return "wraith form (level/2 + d(level/2)) every 1000 turns";
			}
			case ACT_INVULN:
			{
				return "invulnerability (dur 8+d8) every 1000 turns";
			}
			case ACT_LIGHT:
			{
				return "light area (dam 2d15) every 10+d10 turns";
			}
			case ACT_MAP_LIGHT:
			{
				return "light (dam 2d15) & map area every 50+d50 turns";
			}
			case ACT_DETECT_ALL:
			{
				return "detection every 55+d55 turns";
			}
			case ACT_DETECT_XTRA:
			{
				return "detection, probing and identify true every 1000 turns";
			}
			case ACT_ID_FULL:
			{
				return "identify true every 750 turns";
			}
			case ACT_ID_PLAIN:
			{
				return "identify spell every 10 turns";
			}
			case ACT_RUNE_EXPLO:
			{
				return "explosive rune every 200 turns";
			}
			case ACT_RUNE_PROT:
			{
				return "rune of protection every 400 turns";
			}
			case ACT_SATIATE:
			{
				return "satisfy hunger every 200 turns";
			}
			case ACT_DEST_DOOR:
			{
				return "destroy doors every 10 turns";
			}
			case ACT_STONE_MUD:
			{
				return "stone to mud every 5 turns";
			}
			case ACT_RECHARGE:
			{
				return "recharging every 70 turns";
			}
			case ACT_ALCHEMY:
			{
				return "alchemy every 500 turns";
			}
			case ACT_DIM_DOOR:
			{
				return "dimension door every 100 turns";
			}
			case ACT_TELEPORT:
			{
				return "teleport (range 100) every 45 turns";
			}
			case ACT_RECALL:
			{
				return "word of recall every 200 turns";
			}
			default:
			{
				return "something undefined";
			}
		}
	}

	/* Some artifacts can be activated */
	switch (o_ptr->name1)
	{
                case ART_EOL:
                {
                        return "mana bolt (9d8) 7+d7 turns";
                }
                case ART_SKULLCLEAVER:
                {
                        return "destruction every 200+d200 turns";
                }
                case ART_HARADRIM:
                {
                        return "berserk strength every 50+d50 turns";
                }
                case ART_FUNDIN:
                {
                        return "dispel evil (x4) every 100+d100 turns";
                }
                case ART_NAIN:
                {
                        return "stone to mud every 7+d5 turns";
                }
                case ART_CELEBRIMBOR:
                {
                        return "temporary ESP (dur 20+d20) every 50+d20 turns";
                }
                case ART_UMBAR:
                {
                        return "magic arrow (10d10) every 20+d20 turns";
                }
                case ART_GILGALAD:
                {
                        return "starlight (75) every 75+d75 turns";
                }
                case ART_HIMRING:
                {
                        return "protect evil (dur level*3 + d25) every 225+d225 turns";
                }
                case ART_NUMENOR:
		{
                        return "analyze monster every 500+d100 turns";
		}
		case ART_NARTHANC:
		{
			return "fire bolt (9d8) every 8+d8 turns";
		}
		case ART_NIMTHANC:
		{
			return "frost bolt (6d8) every 7+d7 turns";
		}
		case ART_DETHANC:
		{
			return "lightning bolt (4d8) every 6+d6 turns";
		}
		case ART_RILIA:
		{
			return "stinking cloud (12) every 4+d4 turns";
		}
		case ART_BELANGIL:
		{
			return "frost ball (48) every 5+d5 turns";
		}
		case ART_DAL:
		{
			return "remove fear and cure poison every 5 turns";
		}
		case ART_RINGIL:
		{
			return "frost ball (100) every 300 turns";
		}
                case ART_ERU:
		{
                        return "healing(7000), curing every 500 turns";
		}
		case ART_DAWN:
		{
			return "summon the Legion of the Dawn every 500+d500 turns";
		}
		case ART_ANDURIL:
		{
			return "fire ball (72) every 400 turns";
		}
		case ART_FIRESTAR:
		{
			return "large fire ball (72) every 100 turns";
		}
		case ART_FEANOR:
		{
			return "haste self (20+d20 turns) every 200 turns";
		}
		case ART_THEODEN:
		{
			return "drain life (120) every 400 turns";
		}
		case ART_TURMIL:
		{
			return "drain life (90) every 70 turns";
		}
		case ART_CASPANION:
		{
			return "door and trap destruction every 10 turns";
		}
		case ART_AVAVIR:
		{
			return "word of recall every 200 turns";
		}
		case ART_TARATOL:
		{
			return "haste self (20+d20 turns) every 100+d100 turns";
		}
		case ART_ERIRIL:
		{
			return "identify every 10 turns";
		}
		case ART_OLORIN:
		{
			return "probing, detection and full id every 1000 turns";
		}
		case ART_EONWE:
		{
			return "mass genocide every 1000 turns";
		}
		case ART_LOTHARANG:
		{
			return "cure wounds (4d7) every 3+d3 turns";
		}
		case ART_CUBRAGOL:
		{
			return "fire branding of bolts every 999 turns";
		}
		case ART_ANGUIREL:
		{
			return "a getaway every 35 turns";
		}
		case ART_AEGLOS:
		{
			return "lightning ball (100) every 500 turns";
		}
		case ART_OROME:
		{
			return "stone to mud every 5 turns";
		}
		case ART_SOULKEEPER:
		{
			return "heal (1000) every 888 turns";
		}
		case ART_BELEGENNON:
		{
			return ("heal (777), curing and heroism every 300 turns");
		}
		case ART_CELEBORN:
		{
			return "genocide every 500 turns";
		}
		case ART_LUTHIEN:
		{
			return "restore life levels every 450 turns";
		}
		case ART_ULMO:
		{
			return "teleport away every 150 turns";
		}
		case ART_COLLUIN:
		{
			return "resistance (20+d20 turns) every 111 turns";
		}
		case ART_HOLCOLLETH:
		{
			return "Sleep II every 55 turns";
		}
		case ART_THINGOL:
		{
			return "recharge item I every 70 turns";
		}
		case ART_COLANNON:
		{
			return "teleport every 45 turns";
		}
		case ART_TOTILA:
		{
			return "confuse monster every 15 turns";
		}
		case ART_CAMMITHRIM:
		{
			return "magic missile (2d6) every 2 turns";
		}
		case ART_PAURHACH:
		{
			return "fire bolt (9d8) every 8+d8 turns";
		}
		case ART_PAURNIMMEN:
		{
			return "frost bolt (6d8) every 7+d7 turns";
		}
		case ART_PAURAEGEN:
		{
			return "lightning bolt (4d8) every 6+d6 turns";
		}
		case ART_PAURNEN:
		{
			return "acid bolt (5d8) every 5+d5 turns";
		}
		case ART_FINGOLFIN:
		{
			return "a magical arrow (150) every 90+d90 turns";
		}
		case ART_HOLHENNETH:
		{
			return "detection every 55+d55 turns";
		}
		case ART_GONDOR:
		{
			return "heal (700) every 250 turns";
		}
		case ART_RAZORBACK:
		{
			return "star ball (150) every 1000 turns";
		}
		case ART_BLADETURNER:
		{
                        return "invulnerability (4+d8) every 800 turns";
		}
                case ART_MEDIATOR:
		{
			return "breathe elements (300), berserk rage, bless, and resistance";
		}
                case ART_KNOWLEDGE:
                {
                        return "whispers from beyond 100+d200 turns";
		}
		case ART_GALADRIEL:
		{
			return "illumination every 10+d10 turns";
		}
                case ART_UNDEATH:
		{
                        return "ruination every 10+d10 turns";
		}
		case ART_ELENDIL:
		{
			return "magic mapping and light every 50+d50 turns";
		}
		case ART_THRAIN:
		{
                        return "detection every 30+d30 turns";
		}
		case ART_INGWE:
		{
			return "dispel evil (x5) every 300+d300 turns";
		}
		case ART_CARLAMMAS:
		{
			return "protection from evil every 225+d225 turns";
		}
                case ART_FLAR:
		{
                        return "dimension door every 100 turns";
		}
		case ART_BARAHIR:
		{
			return "dispel small life every 55+d55 turns";
		}
		case ART_TULKAS:
		{
			return "haste self (75+d75 turns) every 150+d150 turns";
		}
		case ART_NARYA:
		{
                        return "healing (500) every 200+d100 turns";
		}
		case ART_NENYA:
		{
                        return "healing (800) every 100+d200 turns";
		}
		case ART_VILYA:
		{
                        return "greater healing (900) every 200+d200 turns";
		}
		case ART_POWER:
		{
                        return "powerful things";
		}
                case ART_STONE_LORE:
		{
			return "perilous identify every turn";
		}
		case ART_DOR: case ART_GORLIM:
		{
			return "rays of fear in every direction";
		}
                case ART_GANDALF:
                {
                        return "restore mana every 666 turns";
                }
                case ART_EVENSTAR:
                {
                        return "restore every 150 turns";
                }
                case ART_ELESSAR:
                {
                        return "heal and cure black breath every 200 turns";
                }
                case ART_MARDA:
                {
                        return "summon a dragonrider every 1000 turns";
                }
                case ART_PALANTIR_ITHIL:
                case ART_PALANTIR:
                {
                        return "clairvoyance every 100+d100 turns";
                }
                case ART_ROBINTON:
                {
                        return music_info[3].desc;
                }
                case ART_PIEMUR:
                {
                        return music_info[9].desc;
                }
                case ART_MENOLLY:
                {
                        return music_info[10].desc;
                }
                case ART_EREBOR:
		{
			return "open a secret passage every 75 turns";
		}
                case ART_DRUEDAIN:
		{
                        return "detection every 150 turns";
		}
                case ART_ROHAN:
		{
                        return "heroism, berserker, haste every 250 turns";
		}
                case ART_HELM:
		{
                        return "sound ball (300) every 400 turns";
		}
                case ART_BOROMIR:
		{
                        return "mass human summoning every 1000 turns";
		}
                case ART_HURIN:
                {
                        return "berserker and +10 to speed (50) every 100+d200 turns";
                }
                case ART_AXE_GOTHMOG:
		{
                        return "fire ball (300) every 200+d200 turns";
		}
                case ART_MELKOR:
		{
                        return "darkness ball (150) every 100 turns";
		}
                case ART_GROND:
		{
                        return "alter reality every 100 turns";
		}
                case ART_ORCHAST:
		{
			return "detect orcs every 10 turns";
		}
		case ART_NIGHT:
		{
			return "vampiric drain (3*100) every 250 turns";
		}
		case ART_NATUREBANE:
		{
                        return "dispel monsters (300) every 200+d200 turns";
		}
	}

        if(o_ptr->tval == TV_EGG)
        {
                return "stop or resume the egg development";
        }
        if (o_ptr->name2 == EGO_INST_DRAGONKIND)
	{
                switch(o_ptr->pval2)
                {
                        case GF_ELEC:
                                return "large lightning ball (300) every 100 turns";
                                break;
                        case GF_ACID:
                                return "large acid ball (300) every 100 turns";
                                break;
                        case GF_FIRE:
                                return "large fire ball (300) every 100 turns";
                                break;
                        case GF_COLD:
                                return "large cold ball (300) every 100 turns";
                                break;
                }
	}
        if(o_ptr->tval == TV_INSTRUMENT)
        {
                if(o_ptr->sval != SV_HORN)
                {
                        return music_info[o_ptr->pval2].desc;
                }
                else
                {
                        return "aggravate monster every 100 turns";
                }
        }
        if (o_ptr->name2 == EGO_DRAGON)
	{
		return "teleport every 50+d50 turns";
	}
        if (o_ptr->name2 == EGO_JUMP)
	{
                return "phasing every 10+d10 turns";
	}
        if (o_ptr->name2 == EGO_SPINING)
	{
                return "spining around every 50+d25 turns";
	}
        if (o_ptr->name2 == EGO_NOLDOR)
	{
                return "detect treasure every 10+d20 turns";
	}
        if (o_ptr->name2 == EGO_SPECTRAL)
        {
                return "wraith-form every 50+d50 turns";
        }
	if (o_ptr->tval == TV_RING)
	{
		switch(o_ptr->sval)
		{
                        case SV_RING_ELEC:
                                return "ball of lightning and resist lightning";
			case SV_RING_FLAMES:
				return "ball of fire and resist fire";
			case SV_RING_ICE:
				return "ball of cold and resist cold";
			case SV_RING_ACID:
				return "ball of acid and resist acid";
                        case SV_RING_TELEPORTATION:
                                return "teleportation and destruction of the ring";
			default:
				return NULL;
		}
	}

        if (o_ptr->tval == TV_AMULET)
	{
		switch(o_ptr->sval)
		{
                        case SV_AMULET_SERPENT:
                                return "vemon breathing every 40+d60 turns";
			default:
				return NULL;
		}
	}

	/* Require dragon scale mail */
	if (o_ptr->tval != TV_DRAG_ARMOR) return (NULL);

	/* Branch on the sub-type */
	switch (o_ptr->sval)
	{
		case SV_DRAGON_BLUE:
		{
                        return "breathe lightning (100) every 90+d90 turns";
		}
		case SV_DRAGON_WHITE:
		{
                        return "breathe frost (110) every 90+d90 turns";
		}
		case SV_DRAGON_BLACK:
		{
                        return "breathe acid (130) every 90+d90 turns";
		}
		case SV_DRAGON_GREEN:
		{
                        return "breathe poison gas (150) every 90+d90 turns";
		}
		case SV_DRAGON_RED:
		{
                        return "breathe fire (200) every 90+d90 turns";
		}
		case SV_DRAGON_MULTIHUED:
		{
                        return "breathe multi-hued (250) every 60+d60 turns";
		}
		case SV_DRAGON_BRONZE:
		{
                        return "breathe confusion (120) every 90+d90 turns";
		}
		case SV_DRAGON_GOLD:
		{
                        return "breathe sound (130) every 90+d90 turns";
		}
		case SV_DRAGON_CHAOS:
		{
                        return "breathe chaos/disenchant (220) every 90+d60 turns";
		}
		case SV_DRAGON_LAW:
		{
                        return "breathe sound/shards (230) every 90+d60 turns";
		}
		case SV_DRAGON_BALANCE:
		{
                        return "breathe balance (250) every 90+d60 turns";
		}
		case SV_DRAGON_SHINING:
		{
                        return "breathe light/darkness (200) every 90+d60 turns";
		}
		case SV_DRAGON_POWER:
		{
                        return "breathe the elements (300) every 90+d60 turns";
		}
	}

	/* Oops */
	return NULL;
}

/* Grab the tval desc */
int grab_tval_desc(int tval, cptr *info, int i)
{
        static char buff2[400];
        char *s, *t;
        int n, tv = 0;

        while (tval_descs[tv].tval && (tval_descs[tv].tval != tval))
        {                
                tv++;
        }

        if (!tval_descs[tv].tval) return i;
			   
        strncpy (buff2, tval_descs[tv].desc,400);
	buff2[399] = '\0'; /* Paranoia */

        s = buff2;
		
        /* Collect the history */
        while (TRUE)
        {

                /* Extract remaining length */
                n = strlen(s);

                /* All done */
                if (n < 60)
                {
                        /* Save one line of history */
                        info[i++] = s;

                        /* All done */
                        break;
                }

                /* Find a reasonable break-point */
                for (n = 60; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

                /* Save next location */
                t = s + n;

                /* Wipe trailing spaces */
                while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

                /* Save one line of history */
                info[i++] = s;

                s = t;
        }

        return (i);
}

/*
 * Describe a "fully identified" item
 */
bool identify_fully_aux(object_type *o_ptr, FILE *fff)
{
	int                     i = 0, j, k;

        u32b f1, f2, f3, f4, f5, esp;

        cptr            info[400];


	/* Extract the flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* No need to dump that */
        if (fff == NULL)
        {
                i = grab_tval_desc(o_ptr->tval, info, 0);
                if ((fff == NULL) && i) info[i++] = "";
        }

        if (o_ptr->k_idx)
	{
	
                char buff2[400], *s, *t;
                int n, oi = i;
                object_kind *k_ptr = &k_info[o_ptr->k_idx];
			   
                strcpy (buff2, k_text + k_ptr->text);

		s = buff2;
		
                /* Collect the history */
                while (TRUE)
                {

                        /* Extract remaining length */
                        n = strlen(s);

                        /* All done */
                        if (n < 60)
                        {
                                /* Save one line of history */
                                info[i++] = s;

                                /* All done */
                                break;
                        }

                        /* Find a reasonable break-point */
                        for (n = 60; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

                        /* Save next location */
                        t = s + n;

                        /* Wipe trailing spaces */
                        while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

                        /* Save one line of history */
                        info[i++] = s;

                        s = t;
                }

                /* Add a blank line */
                if ((fff == NULL) && (oi < i)) info[i++] = "";
	}

	if (o_ptr->name1)
	{
	
                char buff2[400], *s, *t;
                int n, oi = i;
                artifact_type *a_ptr = &a_info[o_ptr->name1];
			   
	        a_ptr = &a_info[o_ptr->name1];
		strcpy (buff2, a_text + a_ptr->text);

		s = buff2;
		
                /* Collect the history */
                while (TRUE)
                {

                        /* Extract remaining length */
                        n = strlen(s);

                        /* All done */
                        if (n < 60)
                        {
                                /* Save one line of history */
                                info[i++] = s;

                                /* All done */
                                break;
                        }

                        /* Find a reasonable break-point */
                        for (n = 60; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

                        /* Save next location */
                        t = s + n;

                        /* Wipe trailing spaces */
                        while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

                        /* Save one line of history */
                        info[i++] = s;

                        s = t;
                }

                /* Add a blank line */
                if ((fff == NULL) && (oi < i)) info[i++] = "";
	}

        if (f4 & TR4_LEVELS)
        {
                int j = 0;

                if (count_bits(o_ptr->pval3) == 0) info[i++] = "It is sentient.";
                else if (count_bits(o_ptr->pval3) > 1) info[i++] = "It is sentient and can have access to the realms of:";
                else  info[i++] = "It is sentient and can have access to the realm of:";

                for (j = 0; j < MAX_FLAG_GROUP; j++)
                {
                        if (BIT(j) & o_ptr->pval3) info[i++] = flags_groups[j].name;
                }

                /* Add a blank line */
                info[i++] = "";
        }

        if (f4 & TR4_COULD2H) info[i++] = "It can be wielded two-handed.";
        if (f4 & TR4_MUST2H) info[i++] = "It must be wielded two-handed.";

	/* Mega-Hack -- describe activation */
	if (f3 & (TR3_ACTIVATE))
	{
		info[i++] = "It can be activated for...";
                if (o_ptr->name2 == EGO_MSTAFF_SPELL){
                        info[i++] = item_activation(o_ptr,1);
                        info[i++] = "And...";
                }
                info[i++] = item_activation(o_ptr,0);

		/* Mega-hack -- get rid of useless line for randarts */
                if (o_ptr->tval != TV_RANDART){
                        info[i++] = "...if it is being worn.";
                }
	}
        /* Granted power */
        if (object_power(o_ptr) != -1)
        {
                info[i++] = "It grants you the power of...";
                info[i++] = powers_type[object_power(o_ptr)].name;
                info[i++] = "...if it is being worn.";
        }


	/* Hack -- describe lite's */
	if (o_ptr->tval == TV_LITE)
	{
                int radius = 0;

                if (f3 & TR3_LITE1) radius++;
                if (f4 & TR4_LITE2) radius += 2;
                if (f4 & TR4_LITE3) radius += 3;

                if (f4 & TR4_FUEL_LITE)
		{
                        info[i++] = format("It provides light (radius %d) when fueled.", radius);
		}
                else
		{
                        info[i++] = format("It provides light (radius %d) forever.", radius);
		}
	}

        /* Mega Hack^3 -- describe the Anchor of Space-time */
        if (o_ptr->name1 == ART_ANCHOR)
        {
                info[i++] = "It prevents the space-time continuum from being disrupted.";
        }

        if ((f4 & (TR4_ANTIMAGIC_50)) || (f4 & (TR4_ANTIMAGIC_30)) || (f4 & (TR4_ANTIMAGIC_20)) || (f4 & (TR4_ANTIMAGIC_10)))
        {
                info[i++] = "It generates an antimagic field.";
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
		if (o_ptr->tval != TV_TRAPKIT)
			info[i++] = "It affects your stealth.";
		else
			info[i++] = "It is well-hidden.";
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
        if (f5 & (TR5_CRIT))
	{
                info[i++] = "It affects your ability to score critical hits.";
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

	if (f1 & (TR1_BRAND_POIS))
	{
		info[i++] = "It poisons your foes.";
	}

	if (f1 & (TR1_CHAOTIC))
	{
		info[i++] = "It produces chaotic effects.";
	}

	if (f1 & (TR1_VAMPIRIC))
	{
		info[i++] = "It drains life from your foes.";
	}

	if (f1 & (TR1_IMPACT))
	{
		info[i++] = "It can cause earthquakes.";
	}

	if (f1 & (TR1_VORPAL))
	{
		info[i++] = "It is very sharp and can cut your foes.";
	}

	if (f1 & (TR1_KILL_DRAGON))
	{
		info[i++] = "It is a great bane of dragons.";
	}
	else if (f1 & (TR1_SLAY_DRAGON))
	{
		info[i++] = "It is especially deadly against dragons.";
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
        if (f5 & (TR5_KILL_DEMON))
	{
                info[i++] = "It is a great bane of demons.";
	}
        else if (f1 & (TR1_SLAY_DEMON))
	{
		info[i++] = "It strikes at demons with holy wrath.";
	}
        if (f5 & (TR5_KILL_UNDEAD))
	{
                info[i++] = "It is a great bane of undead.";
	}
        else if (f1 & (TR1_SLAY_UNDEAD))
	{
		info[i++] = "It strikes at undead with holy wrath.";
	}
	if (f1 & (TR1_SLAY_EVIL))
	{
		info[i++] = "It fights against evil with holy fury.";
	}
	if (f1 & (TR1_SLAY_ANIMAL))
	{
		info[i++] = "It is especially deadly against natural creatures.";
	}
        if (f1 & (TR1_MANA))
	{
                info[i++] = "It increases your mana capacity.";
	}
        if (f1 & (TR1_SPELL))
	{
                info[i++] = "It increases your spell power.";
	}

        if (f2 & (TR2_INVIS))
	{
                info[i++] = "It makes you invisible.";
	}
        if (f2 & (TR2_LIFE))
	{
                info[i++] = "It increases your life.";
	}
        if (o_ptr->tval != TV_TRAPKIT)
        {
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
                if (f2 & (TR2_IM_ELEC))
                {
                        info[i++] = "It provides immunity to electricity.";
                }
                if (f2 & (TR2_IM_FIRE))
                {
                        info[i++] = "It provides immunity to fire.";
                }
        }
        else
        {
		if (f2 & (TRAP2_AUTOMATIC_5))
		{
			info[i++] = "It can rearm itself.";
		}
		if (f2 & (TRAP2_AUTOMATIC_99))
		{
			info[i++] = "It rearms itself.";
		}               
		if (f2 & (TRAP2_KILL_GHOST))
		{
			info[i++] = "It is effective against Ghosts.";
		}
		if (f2 & (TRAP2_TELEPORT_TO))
		{
			info[i++] = "It can teleport monsters to you.";
		}
		if (f2 & (TRAP2_ONLY_DRAGON))
		{
			info[i++] = "It can only be set off by dragons.";
		}
		if (f2 & (TRAP2_ONLY_DEMON))
		{
			info[i++] = "It can only be set off by demons.";
		}
		if (f2 & (TRAP2_ONLY_UNDEAD))
		{
			info[i++] = "It can only be set off by undead.";
		}
		if (f2 & (TRAP2_ONLY_ANIMAL))
		{
			info[i++] = "It can only be set off by animals.";
		}
		if (f2 & (TRAP2_ONLY_EVIL))
		{
			info[i++] = "It can only be set off by evil creatures.";
		}
        }

	if (f2 & (TR2_IM_COLD))
	{
		info[i++] = "It provides immunity to cold.";
	}

	if (f2 & (TR2_FREE_ACT))
	{
		info[i++] = "It provides immunity to paralysis.";
	}
	if (f2 & (TR2_HOLD_LIFE))
	{
		info[i++] = "It provides resistance to life draining.";
	}
	if (f2 & (TR2_RES_FEAR))
	{
		info[i++] = "It makes you completely fearless.";
	}
	if (f2 & (TR2_RES_ACID))
	{
		info[i++] = "It provides resistance to acid.";
	}
	if (f2 & (TR2_RES_ELEC))
	{
		info[i++] = "It provides resistance to electricity.";
	}
	if (f2 & (TR2_RES_FIRE))
	{
		info[i++] = "It provides resistance to fire.";
	}
	if (f2 & (TR2_RES_COLD))
	{
		info[i++] = "It provides resistance to cold.";
	}
	if (f2 & (TR2_RES_POIS))
	{
		info[i++] = "It provides resistance to poison.";
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
	if (f2 & (TR2_RES_CONF))
	{
		info[i++] = "It provides resistance to confusion.";
	}
	if (f2 & (TR2_RES_SOUND))
	{
		info[i++] = "It provides resistance to sound.";
	}
	if (f2 & (TR2_RES_SHARDS))
	{
		info[i++] = "It provides resistance to shards.";
	}

        if (f4 & (TR4_IM_NETHER))
	{
                info[i++] = "It provides immunity to nether.";
	}
	if (f2 & (TR2_RES_NETHER))
	{
		info[i++] = "It provides resistance to nether.";
	}
	if (f2 & (TR2_RES_NEXUS))
	{
		info[i++] = "It provides resistance to nexus.";
	}
	if (f2 & (TR2_RES_CHAOS))
	{
		info[i++] = "It provides resistance to chaos.";
	}
	if (f2 & (TR2_RES_DISEN))
	{
		info[i++] = "It provides resistance to disenchantment.";
	}

	if (f3 & (TR3_WRAITH))
	{
		info[i++] = "It renders you incorporeal.";
	}
	if (f3 & (TR3_FEATHER))
	{
		info[i++] = "It allows you to levitate.";
	}
        if (f4 & (TR4_FLY))
	{
                info[i++] = "It allows you to fly.";
	}
        if (f4 & (TR4_CLIMB))
	{
                info[i++] = "It allows you to climb high mountains.";
	}
        if ((o_ptr->tval != TV_LITE) && ((f3 & (TR3_LITE1)) || (f4 & (TR4_LITE2)) || (f4 & (TR4_LITE3))))
	{
                info[i++] = "It provides light.";
	}
	if (f3 & (TR3_SEE_INVIS))
	{
		info[i++] = "It allows you to see invisible monsters.";
	}
        if (esp)
	{
                if (esp & ESP_ALL) info[i++] = "It gives telepathic powers.";
                else
                {
                        if (esp & ESP_ORC) info[i++] = "It allows you to sense the presence of orcs.";
                        if (esp & ESP_TROLL) info[i++] = "It allows you to sense the presence of trolls.";
                        if (esp & ESP_DRAGON) info[i++] = "It allows you to sense the presence of dragons.";
                        if (esp & ESP_SPIDER) info[i++] = "It allows you to sense the presence of spiders.";
                        if (esp & ESP_GIANT) info[i++] = "It allows you to sense the presence of giants.";
                        if (esp & ESP_DEMON) info[i++] = "It allows you to sense the presence of demons.";
                        if (esp & ESP_UNDEAD) info[i++] = "It allows you to sense presence of undead.";
                        if (esp & ESP_EVIL) info[i++] = "It allows you to sense the presence of evil beings.";
                        if (esp & ESP_ANIMAL) info[i++] = "It allows you to sense the presence of animals.";
                        if (esp & ESP_DRAGONRIDER) info[i++] = "It allows you to sense the presence of dragonriders.";
                        if (esp & ESP_GOOD) info[i++] = "It allows you to sense the presence of good beings.";
                        if (esp & ESP_NONLIVING) info[i++] = "It allows you to sense the presence of non-living things.";
                        if (esp & ESP_UNIQUE) info[i++] = "It allows you to sense the presence of unique beings.";
                }
	}
	if (f3 & (TR3_SLOW_DIGEST))
	{
		info[i++] = "It slows your metabolism.";
	}
	if (f3 & (TR3_REGEN))
	{
		info[i++] = "It speeds your regenerative powers.";
	}
	if (f2 & (TR2_REFLECT))
	{
		info[i++] = "It reflects bolts and arrows.";
	}
	if (f3 & (TR3_SH_FIRE))
	{
		info[i++] = "It produces a fiery sheath.";
	}
	if (f3 & (TR3_SH_ELEC))
	{
		info[i++] = "It produces an electric sheath.";
	}
	if (f3 & (TR3_NO_MAGIC))
	{
		info[i++] = "It produces an anti-magic shell.";
	}
	if (f3 & (TR3_NO_TELE))
	{
		info[i++] = "It prevents teleportation.";
	}
	if (f3 & (TR3_XTRA_MIGHT))
	{
		info[i++] = "It fires missiles with extra might.";
	}
	if (f3 & (TR3_XTRA_SHOTS))
	{
		info[i++] = "It fires missiles excessively fast.";
	}

        if (f5 & (TR5_DRAIN_MANA))
	{
                info[i++] = "It drains mana.";
	}

        if (f5 & (TR5_DRAIN_HP))
	{
                info[i++] = "It drains life.";
	}

	if (f3 & (TR3_DRAIN_EXP))
	{
		info[i++] = "It drains experience.";
	}
	if (f3 & (TR3_TELEPORT))
	{
		info[i++] = "It induces random teleportation.";
	}
	if (f3 & (TR3_AGGRAVATE))
	{
		info[i++] = "It aggravates nearby creatures.";
	}

	if (f3 & (TR3_BLESSED))
	{
		info[i++] = "It has been blessed by the gods.";
	}

        if (f4 & (TR4_AUTO_ID))
	{
                info[i++] = "It identifies all items for you.";
	}

        if (f4 & (TR4_NEVER_BLOW))
	{
                info[i++] = "It can't attack.";
	}

        if (f4 & (TR4_BLACK_BREATH))
        {
                info[i++] = "It fills you with the Black Breath.";
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

	if (f3 & (TR3_TY_CURSE))
	{
		info[i++] = "It carries an ancient foul curse.";
	}

	if (f3 & (TR3_IGNORE_ACID))
	{
		info[i++] = "It cannot be harmed by acid.";
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
        if (f4 & (TR4_DG_CURSE))
	{
                info[i++] = "It carries an ancient morgothian curse.";
	}
        if (f4 & (TR4_CLONE))
	{
                info[i++] = "It can clone monsters.";
	}
        if (f3 & (TR3_AUTO_CURSE))
	{
                info[i++] = "It can re-curse itself.";
	}
        if (f4 & (TR4_CAPACITY))
	{
                info[i++] = "It can hold more mana.";
	}
        if (f4 & (TR4_CHEAPNESS))
	{
                info[i++] = "It can cast spells for a lesser mana cost.";
	}
        if (f4 & (TR4_FAST_CAST))
	{
                info[i++] = "It can cast spells faster.";
	}
        if (f4 & (TR4_CHARGING))
	{
                info[i++] = "It regenerates its mana faster.";
	}

	/* No special effects */
	if (!i) return (FALSE);

        if (fff != NULL)
        {
                /* We will print on top of the map (column 13) */
                for (j = 0; j < i; j++)
                {
                        /* Show the info */
                        fprintf(fff, "   %s\n", info[j]);
                }
        }
        else
        {
                /* Save the screen */
                Term_save();

                /* Erase the screen */
                for (k = 1; k < 24; k++) prt("", k, 13);

                /* Label the information */
                prt("     Item Attributes:", 1, 15);

                /* We will print on top of the map (column 13) */
                for (k = 2, j = 0; j < i; j++)
                {
                        /* Show the info */
                        prt(info[j], k++, 15);

                        /* Every 20 entries (lines 2 to 21), start over */
                        if ((k == 22) && (j+1 < i))
                        {
                                prt("-- more --", k, 15);
                                inkey();
                                for (; k > 2; k--) prt("", k, 15);
                        }
                }

                /* Wait for it */
                prt("[Press any key to continue]", k, 15);
                inkey();

                /* Restore the screen */
                Term_load();
        }

	/* Gave knowledge */
	return (TRUE);
}



/*
 * Convert an inventory index into a one character label
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
 * Convert a label into the index of an item in the "inven"
 * Return "-1" if the label does not indicate a real item
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
 * Convert a label into the index of a item in the "equip"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_equip(int c)
{
	int i;

	/* Convert */
        i = ((islower(c) || (c > 'z')) ? A2I(c) : -1) + INVEN_WIELD;

	/* Verify the index */
	if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return (-1);

	/* Empty slots can never be chosen */
	if (!inventory[i].k_idx) return (-1);

	/* Return the index */
	return (i);
}

/*
 * Returns the next free slot of the given "type", return the first
 * if all are used
 */
int get_slot(int slot)
{
        int i = 0;

        /* If there are at least one body part corretsonding, the find the free one */
        if (p_ptr->body_parts[slot - INVEN_WIELD] == slot)
        {
                /* Find a free body part */
                while ((i < 6) && (slot + i < INVEN_TOTAL) && (p_ptr->body_parts[slot - INVEN_WIELD + i] == slot))
                {
                        if (p_ptr->body_parts[slot + i - INVEN_WIELD])
                        {
                                /* Free ? return the slot */
                                if (!inventory[slot + i].k_idx) return (slot + i);
                        }
                        else break;

                        i++;
                }
                /* Found nothing ? return the first one */
                return slot;
        }
        /* No body parts ? return -1 */
        else return (-1);
}

/*
 * Determine which equipment slot (if any) an item likes
 */
s16b wield_slot(object_type *o_ptr)
{
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
                case TV_TOOL:
		{
                        return get_slot(INVEN_TOOL);
		}

		case TV_HAFTED:
		case TV_POLEARM:
                case TV_MSTAFF:
		case TV_SWORD:
                case TV_AXE:
                case TV_DAEMON_BOOK:
		{
                        return get_slot(INVEN_WIELD);
		}

                case TV_BOOMERANG:
		case TV_BOW:
                case TV_INSTRUMENT:
		{
                        return get_slot(INVEN_BOW);
		}

		case TV_RING:
		{                               
                        return get_slot(INVEN_RING);
		}

		case TV_AMULET:
		{
                        return get_slot(INVEN_NECK);
		}

		case TV_LITE:
		{
                        return get_slot(INVEN_LITE);
		}

		case TV_DRAG_ARMOR:
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		{
                        return get_slot(INVEN_BODY);
		}

		case TV_CLOAK:
		{
                        return get_slot(INVEN_OUTER);
		}

		case TV_SHIELD:
		{
                        return get_slot(INVEN_ARM);
		}

		case TV_CROWN:
		case TV_HELM:
		{
                        return get_slot(INVEN_HEAD);
		}

		case TV_GLOVES:
		{
                        return get_slot(INVEN_HANDS);
		}

		case TV_BOOTS:
		{
                        return get_slot(INVEN_FEET);
		}

                case TV_HYPNOS:
		{
                        return get_slot(INVEN_CARRY);
		}

                case TV_SHOT:
		{
                        if(inventory[INVEN_BOW].k_idx)
                        {
                                if(inventory[INVEN_BOW].sval < 10)
                                        return get_slot(INVEN_AMMO);
                        }
                        return -1;
        	}
                case TV_ARROW:
		{
                        if(inventory[INVEN_BOW].k_idx)
                        {
                                if((inventory[INVEN_BOW].sval >= 10)&&(inventory[INVEN_BOW].sval < 20))
                                        return get_slot(INVEN_AMMO);
                        }
                        return -1;
        	}
                case TV_BOLT:
		{                        
                        if(inventory[INVEN_BOW].k_idx)
                        {
                                if(inventory[INVEN_BOW].sval >= 20)
                                        return get_slot(INVEN_AMMO);
                        }
                        return -1;
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
                case INVEN_WIELD:   p = "Wielding"; break;
                case INVEN_WIELD+1: p = "Wielding"; break;
                case INVEN_WIELD+2: p = "Wielding"; break;
		case INVEN_BOW:   p = "Shooting"; break;
                case INVEN_RING:  p = "On finger"; break;
                case INVEN_RING+1:  p = "On finger"; break;
                case INVEN_RING+2:  p = "On finger"; break;
                case INVEN_RING+3:  p = "On finger"; break;
                case INVEN_RING+4:  p = "On finger"; break;
                case INVEN_RING+5:  p = "On finger"; break;
		case INVEN_NECK:  p = "Around neck"; break;
                case INVEN_NECK+1:  p = "Around neck"; break;
		case INVEN_LITE:  p = "Light source"; break;
		case INVEN_BODY:  p = "On body"; break;
		case INVEN_OUTER: p = "About body"; break;
		case INVEN_ARM:   p = "On arm"; break;
                case INVEN_ARM+1:   p = "On arm"; break;
                case INVEN_ARM+2:   p = "On arm"; break;
		case INVEN_HEAD:  p = "On head"; break;
                case INVEN_HEAD+1:  p = "On head"; break;
		case INVEN_HANDS: p = "On hands"; break;
                case INVEN_HANDS+1: p = "On hands"; break;
                case INVEN_HANDS+2: p = "On hands"; break;
		case INVEN_FEET:  p = "On feet"; break;
                case INVEN_FEET+1:  p = "On feet"; break;
                case INVEN_CARRY: p = "Carrying"; break;
                case INVEN_AMMO:  p = "Quiver"; break;
                case INVEN_TOOL:  p = "Using"; break;
		default:          p = "In pack"; break;
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
                case INVEN_WIELD+1: p = "attacking monsters with"; break;
                case INVEN_WIELD+2: p = "attacking monsters with"; break;
		case INVEN_BOW:   p = "shooting missiles with"; break;
                case INVEN_RING:  p = "wearing on your finger"; break;
                case INVEN_RING+1:  p = "wearing on your finger"; break;
                case INVEN_RING+2:  p = "wearing on your finger"; break;
                case INVEN_RING+3:  p = "wearing on your finger"; break;
                case INVEN_RING+4:  p = "wearing on your finger"; break;
                case INVEN_RING+5:  p = "wearing on your finger"; break;
		case INVEN_NECK:  p = "wearing around your neck"; break;
                case INVEN_NECK+1:  p = "wearing around your neck"; break;
		case INVEN_LITE:  p = "using to light the way"; break;
		case INVEN_BODY:  p = "wearing on your body"; break;
		case INVEN_OUTER: p = "wearing on your back"; break;
		case INVEN_ARM:   p = "wearing on your arm"; break;
                case INVEN_ARM+1:   p = "wearing on your arm"; break;
                case INVEN_ARM+2:   p = "wearing on your arm"; break;
		case INVEN_HEAD:  p = "wearing on your head"; break;
                case INVEN_HEAD+1:  p = "wearing on your head"; break;
		case INVEN_HANDS: p = "wearing on your hands"; break;
                case INVEN_HANDS+1: p = "wearing on your hands"; break;
		case INVEN_FEET:  p = "wearing on your feet"; break;
                case INVEN_FEET+1:  p = "wearing on your feet"; break;
                case INVEN_CARRY: p = "carrying on you"; break;
		default:          p = "carrying in your pack"; break;
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
	return p;
}


/* Hack: Check if a spellbook is one of the realms we can use. -- TY */

bool check_book_realm(const int book_tval)
{
        if (p_ptr->pclass != CLASS_SORCERER)
        {
                return ((p_ptr->realm1 == book_tval - TV_VALARIN_BOOK + 1) ||
                        (p_ptr->realm2 == book_tval - TV_VALARIN_BOOK + 1));
        }
        else
        {
                int b = book_tval - TV_VALARIN_BOOK;

                if (b >= 32) return FALSE;
                if (b < 0) return FALSE;

                return ((Mrealm_choices[CLASS_SORCERER] & BIT(b))?TRUE:FALSE);
        }
}


/*
 * Check an item against the item tester info
 */
bool item_tester_okay(object_type *o_ptr)
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
                /* Is it a spellbook? If so, we need a hack -- TY */
                if (item_tester_tval<=TV_SPIRIT_BOOK && item_tester_tval>=TV_VALARIN_BOOK)
                        return check_book_realm(o_ptr->tval);
                else
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
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
	register        int i, n, z = 0;
	object_type     *o_ptr;
	byte            attr = TERM_WHITE;
	char            tmp_val[80];
	char            o_name[80];


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

		/* Get a color */
		attr = tval_to_attr[o_ptr->tval % 128];

		/* Hack -- fake monochrome */
		if (!use_color) attr = TERM_WHITE;

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
        register        int i, n, line;
	object_type     *o_ptr;
	byte            attr = TERM_WHITE;
	char            tmp_val[80];
	char            o_name[80];


	/* Display the equipment */
        line = 0;
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
                /* Is there actualy a body part here ? */
                if (!p_ptr->body_parts[i - INVEN_WIELD]) continue;

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
                Term_putstr(0, line, 3, TERM_WHITE, tmp_val);

		/* Obtain an item description */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get the color */
		attr = tval_to_attr[o_ptr->tval % 128];

		/* Hack -- fake monochrome */
		if (!use_color) attr = TERM_WHITE;

		/* Display the entry itself */
                Term_putstr(3, line, n, attr, o_name);

		/* Erase the rest of the line */
                Term_erase(3+n, line, 255);

		/* Display the slot description (if needed) */
		if (show_labels)
		{
                        Term_putstr(61, line, -1, TERM_WHITE, "<--");
                        Term_putstr(65, line, -1, TERM_WHITE, mention_use(i));
		}

		/* Display the weight (if needed) */
		if (show_weights && o_ptr->weight)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			int col = (show_labels ? 52 : 71);
			sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
                        Term_putstr(col, line, -1, TERM_WHITE, tmp_val);
		}

                line++;
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
	int             i, j, k, l, z = 0;
        int             col, len, lim;
	object_type     *o_ptr;
	char            o_name[80];
	char            tmp_val[80];
        int             out_index[23];
	byte            out_color[23];
	char            out_desc[23][80];


	/* Starting column */
	col = command_gap;

	/* Default "max-length" */
	len = 79 - col;

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	/* Require space for icon */
	if (show_inven_graph) lim -= 2;

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

                /* Save the object index */
                out_index[k] = i;

                /* Save the object color, and description */
		out_color[k] = tval_to_attr[o_ptr->tval % 128];
		(void)strcpy(out_desc[k], o_name);

		/* Find the predicted "line length" */
		l = strlen(out_desc[k]) + 5;

		/* Be sure to account for the weight */
		if (show_weights) l += 9;

		/* Account for icon if displayed */
		if (show_inven_graph) l += 2;

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

		/* Display graphics for object, if desired */
		if (show_inven_graph)
		{
			byte  a = object_attr(o_ptr);
			char c = object_char(o_ptr);
			
#ifdef AMIGA
			if (a & 0x80) a |= 0x40;
#endif

			Term_draw(col + 3, j + 1, a, c);
		}


		/* Display the entry itself */
		c_put_str(out_color[j], out_desc[j], j + 1, show_inven_graph ? (col + 5) : (col + 3));

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			(void)sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j + 1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

	/* Save the new column */
	command_gap = col;
}



/*
 * Display the equipment.
 */
void show_equip(void)
{
	int             i, j, k, l;
        int             col, len, lim, idx;
	object_type     *o_ptr;
	char            tmp_val[80];
	char            o_name[80];
        int             out_index[INVEN_TOOL - INVEN_WIELD],
                        out_rindex[INVEN_TOOL - INVEN_WIELD];
        byte            out_color[INVEN_TOOL - INVEN_WIELD];
        char            out_desc[INVEN_TOOL - INVEN_WIELD][80];


	/* Starting column */
	col = command_gap;

	/* Maximal length */
	len = 79 - col;

	/* Maximum space allowed for descriptions */
	lim = 79 - 3;

	/* Require space for labels (if needed) */
	if (show_labels) lim -= (14 + 2);

	/* Require space for weight (if needed) */
	if (show_weights) lim -= 9;

	if (show_equip_graph) lim -= 2;

	/* Scan the equipment list */
        idx = 0;
	for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
                /* Is there actualy a body part here ? */
                if (!p_ptr->body_parts[i - INVEN_WIELD]) continue;

		o_ptr = &inventory[i];

                /* Inform the player that he/she can't use a shield */
                if ((p_ptr->body_parts[i - INVEN_WIELD] == INVEN_ARM) && (!o_ptr->k_idx) && (inventory[i - INVEN_ARM + INVEN_WIELD].k_idx))
                {
                        u32b f1, f2, f3, f4, f5, esp;
                        object_type *q_ptr = &inventory[i - INVEN_ARM + INVEN_WIELD];
                        char q_name[80];

                        /* Is this item acceptable? */
                        if (!item_tester_okay(q_ptr)) continue;

                        /* Description */
                        object_desc(q_name, q_ptr, TRUE, 3);

                        /* Get weapon flags */
                        object_flags(q_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

                        if (f4 & TR4_MUST2H)
                        {
                                sprintf(o_name, "(two handed) %s", q_name);

                                /* Truncate the description */
                                o_name[lim] = 0;

                                /* Save the index */
                                out_index[k] = idx;
                                out_rindex[k] = i;
                                idx++;

                                /* Save the color */
                                out_color[k] = TERM_L_RED;
                                (void)strcpy(out_desc[k], o_name);
                        }
                        else
                        {
                                continue;
                        }
                }
                else
                {
                        /* Is this item acceptable? */
                        if (!item_tester_okay(o_ptr)) continue;

                        /* Description */
                        object_desc(o_name, o_ptr, TRUE, 3);

                        /* Truncate the description */
                        o_name[lim] = 0;

                        /* Save the index */
                        out_index[k] = idx;
                        out_rindex[k] = i;
                        idx++;

                        /* Save the color */
                        out_color[k] = tval_to_attr[o_ptr->tval % 128];
                        (void)strcpy(out_desc[k], o_name);
                }

		/* Extract the maximal length (see below) */
		l = strlen(out_desc[k]) + (2 + 3);

		/* Increase length for labels (if needed) */
		if (show_labels) l += (14 + 2);

		/* Increase length for weight (if needed) */
		if (show_weights) l += 9;

		if (show_equip_graph) l += 2;

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
                if (j > 20) break;

		/* Get the index */
		i = out_index[j];

		/* Get the item */
                o_ptr = &inventory[out_rindex[j]];

		/* Clear the line */
		prt("", j + 1, col ? col - 2 : col);

		/* Prepare an index --(-- */
                sprintf(tmp_val, "%c)", index_to_label(out_rindex[j]));

		/* Clear the line with the (possibly indented) index */
		put_str(tmp_val, j+1, col);

		if (show_equip_graph)
		{
			byte a = object_attr(o_ptr);
			char c = object_char(o_ptr);
			
#ifdef AMIGA
			if (a & 0x80) a |= 0x40;
#endif

			Term_draw(col + 3, j + 1, a, c);
		}

		/* Use labels */
		if (show_labels)
		{
			/* Mention the use */
                        (void)sprintf(tmp_val, "%-14s: ", mention_use(out_rindex[j]));
			put_str(tmp_val, j+1, show_equip_graph ? col + 5 : col + 3);

			/* Display the entry itself */
			c_put_str(out_color[j], out_desc[j], j+1, show_equip_graph ? col + 21 : col + 19);
		}

		/* No labels */
		else
		{
			/* Display the entry itself */
			c_put_str(out_color[j], out_desc[j], j+1, show_equip_graph ? col + 5 : col + 3);
		}

		/* Display the weight if needed */
		if (show_weights)
		{
			int wgt = o_ptr->weight * o_ptr->number;
			(void)sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
			put_str(tmp_val, j+1, 71);
		}
	}

	/* Make a "shadow" below the list (only if needed) */
	if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

	/* Save the new column */
	command_gap = col;
}




/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		/* Unused */
		if (!angband_term[j]) continue;

		/* Flip inven to equip */
		if (window_flag[j] & (PW_INVEN))
		{
			/* Flip flags */
			window_flag[j] &= ~(PW_INVEN);
			window_flag[j] |= (PW_EQUIP);

			/* Window stuff */
			p_ptr->window |= (PW_EQUIP);
		}

		/* Flip inven to equip */
		else if (window_flag[j] & (PW_EQUIP))
		{
			/* Flip flags */
			window_flag[j] &= ~(PW_EQUIP);
			window_flag[j] |= (PW_INVEN);

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
bool verify(cptr prompt, int item)
{
	char    o_name[80];

	char    out_val[160];

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
	(void)sprintf(out_val, "%s %s? ", prompt, o_name);

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
		if ((s[1] == command_cmd) || (s[1] == '*'))
		{
			/* Verify the choice */
			if (!verify("Really try", item)) return (FALSE);
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
static bool get_item_okay(int i)
{
	/* Illegal items */
	if ((i < 0) || (i >= INVEN_TOTAL)) return (FALSE);

	/* Verify the item */
	if (!item_tester_okay(&inventory[i])) return (FALSE);

	/* Assume okay */
	return (TRUE);
}



/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a char "n" appearing as "@n" anywhere in the
 * inscription of an object.
 *
 * Also, the tag "@xn" will work as well, where "n" is a tag-char,
 * and "x" is the "current" command_cmd code.
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
			if ((s[1] == command_cmd) && (s[2] == tag))
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
   * scan_floor --
   *
   * Return a list of o_list[] indexes of items at the given cave
   * location. Valid flags are:
   *
   *            mode & 0x01 -- Item tester
   *            mode & 0x02 -- Marked items only
   *            mode & 0x04 -- Stop after first
   */
  bool scan_floor(int *items, int *item_num, int y, int x, int mode)
  {
        int this_o_idx, next_o_idx;
  
        int num = 0;
   
        (*item_num) = 0;
  
        /* Sanity */
        if (!in_bounds(y, x)) return (FALSE);
   
        /* Scan all objects in the grid */
        for (this_o_idx = cave[y][x].o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
                object_type *o_ptr;
  
                /* Acquire object */
                o_ptr = &o_list[this_o_idx];
  
                /* Acquire next object */
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
                if (num == 23) break;
        }
  
        /* Number of items */
        (*item_num) = num;
   
        /* Result */
        return (num != 0);
  }
  
  /*
   * Display a list of the items on the floor at the given location.
   */
  void show_floor(int y, int x)
  {
        int i, j, k, l;
        int col, len, lim;
  
        object_type *o_ptr;
  
        char o_name[80];
  
        char tmp_val[80];
  
        int out_index[23];
        byte out_color[23];
        char out_desc[23][80];
  
        int floor_list[23], floor_num;
   
        /* Default length */
        len = 79 - 50;
  
        /* Maximum space allowed for descriptions */
        lim = 79 - 3;
  
        /* Require space for weight (if needed) */
        if (show_weights) lim -= 9;
  
        /* Scan for objects in the grid, using item_tester_okay() */
        (void) scan_floor(floor_list, &floor_num, y, x, 0x01);
  
        /* Display the inventory */
        for (k = 0, i = 0; i < floor_num; i++)
        {
                o_ptr = &o_list[floor_list[i]];
  
                /* Describe the object */
                object_desc(o_name, o_ptr, TRUE, 3);
  
                /* Hack -- enforce max length */
                o_name[lim] = '\0';
  
                /* Save the index */
                out_index[k] = i;
  
                /* Acquire inventory color */
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
                sprintf(tmp_val, "%c)", index_to_label(j));
  
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
   * This version of get_item() is called by get_item() when
   * the easy_floor is on.
   */
bool get_item_floor(int *cp, cptr pmt, cptr str, int mode)
  {
        char n1 = 0, n2 = 0, which = ' ';
  
        int j, k, i1, i2, e1, e2;
  
        bool done, item;
  
        bool oops = FALSE;
  
        bool equip = FALSE;
        bool inven = FALSE;
        bool floor = FALSE;
  
        bool allow_equip = FALSE;
        bool allow_inven = FALSE;
        bool allow_floor = FALSE;
  
        bool toggle = FALSE;
  
        char tmp_val[160];
        char out_val[160];
  
        int floor_num, floor_list[23], floor_top = 0;
   
  #ifdef ALLOW_REPEAT
      
        /* Get the item index */
        if (repeat_pull(cp))
        {
                /* Floor item? */
                if (*cp < 0)
                {
                        object_type *o_ptr;
  
                        /* Special index */
                        k = 0 - (*cp);
  
                        /* Acquire object */
                        o_ptr = &o_list[k];
  
                        /* Validate the item */
                        if (item_tester_okay(o_ptr))
                        {
                                /* Forget the item_tester_tval restriction */
                                item_tester_tval = 0;
  
                                /* Forget the item_tester_hook restriction */
                                item_tester_hook = NULL;
  
                                /* Success */
                                return (TRUE);
                        }
                }
         
                /* Verify the item */
                else if (get_item_okay(*cp))
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
  
        /* Extract args */
        if (mode & (USE_EQUIP)) equip = TRUE;
        if (mode & (USE_INVEN)) inven = TRUE;
        if (mode & (USE_FLOOR)) floor = TRUE;
  
  
        /* Paranoia XXX XXX XXX */
        msg_print(NULL);
  
  
        /* Not done */
        done = FALSE;
  
        /* No item selected */
        item = FALSE;
  
  
        /* Full inventory */
        i1 = 0;
        i2 = INVEN_PACK - 1;
  
        /* Forbid inventory */
        if (!inven) i2 = -1;
  
        /* Restrict inventory indexes */
        while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
        while ((i1 <= i2) && (!get_item_okay(i2))) i2--;
  
  
        /* Full equipment */
        e1 = INVEN_WIELD;
        e2 = INVEN_TOTAL - 1;
  
        /* Forbid equipment */
        if (!equip) e2 = -1;
  
        /* Restrict equipment indexes */
        while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
        while ((e1 <= e2) && (!get_item_okay(e2))) e2--;
  
   
        /* Count "okay" floor items */
        floor_num = 0;
   
        /* Restrict floor usage */
        if (floor)
        {
                /* Scan all objects in the grid */
                (void) scan_floor(floor_list, &floor_num, py, px, 0x01);
        }
  
        /* Accept inventory */
        if (i1 <= i2) allow_inven = TRUE;
  
        /* Accept equipment */
        if (e1 <= e2) allow_equip = TRUE;
  
        /* Accept floor */
        if (floor_num) allow_floor = TRUE;
  
        /* Require at least one legal choice */
        if (!allow_inven && !allow_equip && !allow_floor)
        {
                /* Cancel command_see */
                command_see = FALSE;
  
                /* Oops */
                oops = TRUE;
  
                /* Done */
                done = TRUE;
        }
  
        /* Analyze choices */
        else
        {
                /* Hack -- Start on equipment if requested */
                if (command_see && (command_wrk == (USE_EQUIP))
                        && allow_equip)
                {
                        command_wrk = (USE_EQUIP);
                }
  
                /* Use inventory if allowed */
                else if (allow_inven)
                {
                        command_wrk = (USE_INVEN);
                }
  
                /* Use equipment if allowed */
                else if (allow_equip)
                {
                        command_wrk = (USE_EQUIP);
                }
  
                /* Use floor if allowed */
                else if (allow_floor)
                {
                        command_wrk = (USE_FLOOR);
                }
        }
  
        /* Hack -- start out in "display" mode */
        if (command_see)
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
                        for (j = 0; j < 8; j++)
                        {
                                /* Unused */
                                if (!angband_term[j]) continue;
  
                                /* Count windows displaying inven */
                                if (window_flag[j] & (PW_INVEN)) ni++;
  
                                /* Count windows displaying equip */
                                if (window_flag[j] & (PW_EQUIP)) ne++;
                        }
  
                        /* Toggle if needed */
                        if ((command_wrk == (USE_EQUIP) && ni && !ne) ||
                                (command_wrk == (USE_INVEN) && !ni && ne))
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
  
                /* Inventory screen */
                if (command_wrk == (USE_INVEN))
                {
                        /* Extract the legal requests */
                        n1 = I2A(i1);
                        n2 = I2A(i2);
  
                        /* Redraw if needed */
                        if (command_see) show_inven();
                }
  
                /* Equipment screen */
                else if (command_wrk == (USE_EQUIP))
                {
                        /* Extract the legal requests */
                        n1 = I2A(e1 - INVEN_WIELD);
                        n2 = I2A(e2 - INVEN_WIELD);
  
                        /* Redraw if needed */
                        if (command_see) show_equip();
                }
  
                /* Floor screen */
                else if (command_wrk == (USE_FLOOR))
                {
                        j = floor_top;
                        k = MIN(floor_top + 23, floor_num) - 1;
                 
                        /* Extract the legal requests */
                        n1 = I2A(j - floor_top);
                        n2 = I2A(k - floor_top);
  
                        /* Redraw if needed */
                        if (command_see) show_floor(py, px);
                }
  
                /* Viewing inventory */
                if (command_wrk == (USE_INVEN))
                {
                        /* Begin the prompt */
                        sprintf(out_val, "Inven:");
  
                        /* Build the prompt */
                        sprintf(tmp_val, " %c-%c,",
                                index_to_label(i1), index_to_label(i2));
  
                        /* Append */
                        strcat(out_val, tmp_val);
  
                        /* Indicate ability to "view" */
                        if (!command_see) strcat(out_val, " * to see,");
  
                        /* Append */
                        if (allow_equip) strcat(out_val, " / for Equip,");
  
                        /* Append */
                        if (allow_floor) strcat(out_val, " - for floor,");
                }
  
                /* Viewing equipment */
                else if (command_wrk == (USE_EQUIP))
                {
                        /* Begin the prompt */
                        sprintf(out_val, "Equip:");
  
                        /* Build the prompt */
                        sprintf(tmp_val, " %c-%c,",
                                index_to_label(e1), index_to_label(e2));
  
                        /* Append */
                        strcat(out_val, tmp_val);
  
                        /* Indicate ability to "view" */
                        if (!command_see) strcat(out_val, " * to see,");
  
                        /* Append */
                        if (allow_inven) strcat(out_val, " / for Inven,");
  
                        /* Append */
                        if (allow_floor) strcat(out_val, " - for floor,");
                }
  
                /* Viewing floor */
                else if (command_wrk == (USE_FLOOR))
                {
                        /* Begin the prompt */
                        sprintf(out_val, "Floor:");
  
                        /* Build the prompt */
                        sprintf(tmp_val, " %c-%c,", n1, n2);
  
                        /* Append */
                        strcat(out_val, tmp_val);
  
                        /* Indicate ability to "view" */
                        if (!command_see) strcat(out_val, " * to see,");
  
                        /* Append */
                        if (allow_inven)
                        {
                                strcat(out_val, " / for Inven,");
                        }
                        else if (allow_equip)
                        {
                                strcat(out_val, " / for Equip,");
                        }
                }
         
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
                                if (command_see)
                                {
                                        /* Flip flag */
                                        command_see = FALSE;
  
                                        /* Load screen */
                                        screen_load();
                                }
  
                                /* Show the list */
                                else
                                {
                                        /* Save screen */
                                        screen_save();
  
                                        /* Flip flag */
                                        command_see = TRUE;
                                }
                                break;
                        }
  
                        case '/':
                        {
                                if (command_wrk == (USE_INVEN))
                                {
                                        if (!allow_equip)
                                        {
                                                bell();
                                                break;
                                        }
                                        command_wrk = (USE_EQUIP);
                                }
                                else if (command_wrk == (USE_EQUIP))
                                {
                                        if (!allow_inven)
                                        {
                                                bell();
                                                break;
                                        }
                                        command_wrk = (USE_INVEN);
                                }
                                else if (command_wrk == (USE_FLOOR))
                                {
                                        if (allow_inven)
                                        {
                                                command_wrk = (USE_INVEN);
                                        }
                                        else if (allow_equip)
                                        {
                                                command_wrk = (USE_EQUIP);
                                        }
                                        else
                                        {
                                                bell();
                                                break;
                                        }
                                }
  
                                /* Need to redraw */
                                break;
                        }
  
                        case '-':
                        {
                                if (!allow_floor)
                                {
                                        bell();
                                        break;
                                }
  
                                /*
                                 * If we are already examining the floor, and there
                                 * is only one item, we will always select it.
                                 * If we aren't examining the floor and there is only
                                 * one item, we will select it if floor_query_flag
                                 * is FALSE.
                                 */
                                if (floor_num == 1)
                                {
                                        if (command_wrk == (USE_FLOOR))
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
                                if (command_see)
                                {
                                        /* Load screen */
                                        screen_load();
  
                                        /* Save screen */
                                        screen_save();
                                }
                                 
                                command_wrk = (USE_FLOOR);
                         
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
                                        bell();
                                        break;
                                }
  
                                /* Hack -- Validate the item */
                                if ((k < INVEN_WIELD) ? !inven : !equip)
                                {
                                        bell();
                                        break;
                                }
  
                                /* Validate the item */
                                if (!get_item_okay(k))
                                {
                                        bell();
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
                                if (command_wrk == (USE_INVEN))
                                {
                                        k = ((i1 == i2) ? i1 : -1);
                                }
  
                                /* Choose "default" equipment item */
                                else if (command_wrk == (USE_EQUIP))
                                {
                                        k = ((e1 == e2) ? e1 : -1);
                                }
  
                                /* Choose "default" floor item */
                                else if (command_wrk == (USE_FLOOR))
                                {
                                        if (floor_num == 1)
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
                                        }
                                        break;
                                }
                         
                                /* Validate the item */
                                if (!get_item_okay(k))
                                {
                                        bell();
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
                                int ver;
  
                                ver = isupper(which);
                                which = tolower(which);

                                /* Convert letter to inventory index */
                                if (command_wrk == (USE_INVEN))
                                {
                                        k = label_to_inven(which);
                                        if (k == -1)
                                        {
                                                bell();
                                                break;
                                        }
                                }
  
                                /* Convert letter to equipment index */
                                else if (command_wrk == (USE_EQUIP))
                                {
                                        k = label_to_equip(which);
                                        if (k == -1)
                                        {
                                                bell();
                                                break;
                                        }
                                }
  
                                /* Convert letter to floor index */
                                else if (command_wrk == (USE_FLOOR))
                                {
                                        k = islower(which) ? A2I(which) : -1;
                                        if (k < 0 || k >= floor_num)
                                        {
                                                bell();
                                                break;
                                        }
                                 
                                        /* Special index */
                                        k = 0 - floor_list[k];
                                }
                         
                                /* Validate the item */
                                if ((k >= 0) && !get_item_okay(k))
                                {
                                        bell();
                                        break;
                                }
  
                                /* Verify the item */
                                if (ver && !verify("Try", k))
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
        if (command_see)
        {
                /* Load screen */
                screen_load();
  
                /* Hack -- Cancel "display" */
                command_see = FALSE;
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
  
      if (item) repeat_push(*cp);
      
  #endif /* ALLOW_REPEAT */
  
        /* Result */
        return (item);
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
 * Global "p_ptr->command_wrk" is used to choose between equip/inven listings.
 * If it is TRUE then we are viewing inventory, else equipment.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 */
bool get_item(int *cp, cptr pmt, cptr str, int mode)
{
        return get_item_floor(cp, pmt, str, mode);
}

/*
 * Hook to determine if an object is getable
 */
static bool item_tester_hook_getable(object_type *o_ptr)
{
        if (!inven_carry_okay(o_ptr)) return (FALSE);

        if ((o_ptr->tval == TV_HYPNOS) && (p_ptr->pclass != CLASS_SYMBIANT)) return FALSE;

        /* Assume yes */
        return (TRUE);
}

/*
 * Wear a single item from o_ptr
 */
int wear_ammo(object_type *o_ptr)
{
        int slot, num = 1;

	object_type forge;
	object_type *q_ptr;

	/* Check the slot */
	slot = wield_slot(o_ptr);

	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

        num = o_ptr->number; 

	/* Modify quantity */
        q_ptr->number = num;

	/* Access the wield slot */
	o_ptr = &inventory[slot];

        q_ptr->number += o_ptr->number;

	/* Wear the new stuff */
	object_copy(o_ptr, q_ptr);

	/* Increase the weight */
	total_weight += q_ptr->weight;

	/* Increment the equip counter by hand */
	equip_cnt++;

	/* Cursed! */
        if (cursed_p(o_ptr))
	{
		/* Warn the player */
		msg_print("Oops! It feels deathly cold!");

		/* Note the curse */
		o_ptr->ident |= (IDENT_SENSE);
		o_ptr->sense = SENSE_CURSED;
	}

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

        /* Recalculate hitpoint */
        p_ptr->update |= (PU_HP);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

        /* Redraw monster hitpoint */
        p_ptr->redraw |= (PR_MH);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

        return slot;
}

/* Try to pickup arrows */
void pickup_ammo()
{
        s16b this_o_idx, next_o_idx = 0, slot;
        char o_name[80];

        /* Scan the pile of objects */
        for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
                object_type *o_ptr;
  
                /* Acquire object */
                o_ptr = &o_list[this_o_idx];

                if (object_similar(o_ptr, &inventory[INVEN_AMMO]))
                {
                        msg_print("You add the ammo to your quiver.");
                        slot = wear_ammo(o_ptr);

                        /* Get the item again */
                        o_ptr = &inventory[slot];
   
                        /* Describe the object */
                        object_desc(o_name, o_ptr, TRUE, 3);
   
                        /* Message */
                        msg_format("You have %s (%c).", o_name, index_to_label(slot));

                        /* Delete the object */
                        delete_object_idx(this_o_idx);
                }

                /* Acquire next object */
                next_o_idx = o_ptr->next_o_idx;
        }
}


/*
 * Make the player carry everything in a grid
 *
 * If "pickup" is FALSE then only gold will be picked up
 *
 * This is called by py_pickup() when easy_floor is TRUE.
 */
bool can_carry_heavy(object_type *o_ptr)
{
		/* Query if object is heavy */
                if (prompt_pickup_heavy)
		{
			int i, j;
			int old_enc = 0;
			int new_enc = 0;

			/* Extract the "weight limit" (in tenth pounds) */
                        i = weight_limit();

			/* Calculate current encumbarance */
                        j = total_weight;

			/* Apply encumbarance from weight */
			if (j > i/2) old_enc = ((j - (i/2)) / (i / 10));

			/* Increase the weight, recalculate encumbarance */
			j += (o_ptr->number * o_ptr->weight);

			/* Apply encumbarance from weight */
			if (j > i/2) new_enc = ((j - (i/2)) / (i / 10));

			/* Should we query? */
                        if (new_enc > old_enc)
			{
                                return (FALSE);
			}
		}
        return (TRUE);
}

void py_pickup_floor(int pickup)
{
        s16b this_o_idx, next_o_idx = 0;
  
        char o_name[80] = "";
        object_type *o_ptr;
  
        int floor_num = 0, floor_o_idx = 0;
  
        bool do_pickup = TRUE;
  
        bool do_ask = TRUE;

        /* Hack -- ignore monster traps */
        if (cave[py][px].feat == FEAT_MON_TRAP) return;

        /* Squeltch the floor */
        squeltch_grid();

        /* Try to grab ammo */
        pickup_ammo();
  
        /* Scan the pile of objects */
        for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
                /* Acquire object */
                o_ptr = &o_list[this_o_idx];

                if (p_ptr->auto_id)
                {
                        object_aware(o_ptr);
                        object_known(o_ptr);
                }
  
                /* Acquire next object */
                next_o_idx = o_ptr->next_o_idx;
  
                /* Hack -- disturb */
                disturb(0, 0);
 
                /* Pick up gold */
                if (o_ptr->tval == TV_GOLD)
                {
			char goldname[80];
			object_desc(goldname, o_ptr, TRUE, 3);
                        /* Message */
                        msg_format("You have found %ld gold pieces worth of %s.",
                                   (long)o_ptr->pval, goldname);
  
                        /* Collect the gold */
                        p_ptr->au += o_ptr->pval;
  
                        /* Redraw gold */
                        p_ptr->redraw |= (PR_GOLD);
  
                        /* Window stuff */
                        p_ptr->window |= (PW_PLAYER);
  
                        /* Delete the gold */
                        delete_object_idx(this_o_idx);
  
                        continue;
                }
		{
		char testdesc[80];
		object_desc(testdesc, o_ptr, TRUE, 3);
		if(0 != strncmp(testdesc, "(nothing)",80))
			{
			strncpy(o_name,testdesc,80);
			}
		}
                /* Count non-gold */
                floor_num++;
  
                /* Remember this index */
                floor_o_idx = this_o_idx;
        }
  
        /* There were no non-gold items */
        if (!floor_num) return;

        /* Mention number of items */
        if (!pickup)
        {
                /* One item */
                if (floor_num == 1)
                {
                        /* Acquire object */
                        o_ptr = &o_list[floor_o_idx];
   
                        /* Message */
                        msg_format("You see %s.", o_name);
                }
  
                /* Multiple items */
                else
                {
                        /* Message */
                        msg_format("You see a pile of %d items.", floor_num);
                }
  
                /* Done */
                return;
        }
  
        /* One item */
        if (floor_num == 1)
        {
                /* Hack -- query every item */
                if (carry_query_flag || (!can_carry_heavy(&o_list[floor_o_idx])))
                {
                        char out_val[160];
                        sprintf(out_val, "Pick up %s? ", o_name);
                        do_pickup = get_check(out_val);
                }
  
                /* Don't ask */
                do_ask = FALSE;

                if ((o_list[floor_o_idx].tval == TV_HYPNOS) && (p_ptr->pclass != CLASS_SYMBIANT))
                        do_pickup = FALSE;
                else
                        this_o_idx = floor_o_idx;
        }
  
        /* Ask */
        if (do_ask)
        {
                cptr q, s;
  
                int item;
         
                /* Get an item */

                item_tester_hook = item_tester_hook_getable;

                q = "Get which item? ";
                s = "You see nothing there.";
                if (get_item(&item, q, s, (USE_FLOOR)))
                {
                        this_o_idx = 0 - item;

                        if (!can_carry_heavy(&o_list[this_o_idx]))
                        {
                                char out_val[160];

                                /* Describe the object */
                                object_desc(o_name, &o_list[this_o_idx], TRUE, 3);

                                sprintf(out_val, "Pick up %s? ", o_name);
                                do_pickup = get_check(out_val);
                        }
                }
                else
                {
                        do_pickup = FALSE;
                }
        }
   
        /* Pick up the item */
        if (do_pickup)
        {
                /* Access the item */
                o_ptr = &o_list[this_o_idx];
   
                /* Describe the object */
                object_desc(o_name, o_ptr, TRUE, 3);
         
                /* Note that the pack is too full */
                if (!inven_carry_okay(o_ptr) && !object_similar(o_ptr, &inventory[INVEN_AMMO]))
                {
                        msg_format("You have no room for %s.", o_name);
                }
   
                /* Pick up object */
                else
                {
                        int slot = 0;
                        object_type *q_ptr;

                        q_ptr = &inventory[INVEN_AMMO];
   
                        /* Carry the item */
                        if(object_similar(o_ptr, q_ptr))
                        {
                                msg_print("You add the ammo to your quiver.");
                                slot = wear_ammo(o_ptr);
                        }
                        else
                        {
                                slot = inven_carry(o_ptr, FALSE);
                        }

                        /* Get the item again */
                        o_ptr = &inventory[slot];
   
                        /* Describe the object */
                        object_desc(o_name, o_ptr, TRUE, 3);
   
                        /* Message */
                        msg_format("You have %s (%c).", o_name, index_to_label(slot));

                        /* Delete the object */
                        delete_object_idx(this_o_idx);
                }
        }
  }

/* Add a flags group */
void gain_flag_group(object_type *o_ptr, bool silent)
{
        int grp = 0;
        int tries = 1000;

        while (tries--)
        {
                grp = rand_int(MAX_FLAG_GROUP);

                /* If we already got this group continue */
                if (o_ptr->pval3 & BIT(grp)) continue;

                /* Not enough points ? */
                if (flags_groups[grp].price > o_ptr->pval2) continue;

                /* Ok, enough points and not already got it */
                break;
        }

        /* Ack, nothing found */
        if (tries <= 1) return;

        o_ptr->pval2 -= flags_groups[grp].price;
        o_ptr->pval3 |= BIT(grp);

        if (!silent)
        {
                char o_name[80];

                object_desc(o_name, o_ptr, FALSE, 0);
                msg_format("%s gains access to the %s realm.", o_name, flags_groups[grp].name);
        }
}

u32b get_flag(object_type *o_ptr, int grp, int k)
{
        u32b f = 0, flag_set = 0;
        int tries = 1000;
        u32b f1, f2, f3, f4, f5, esp, flag_test;

	/* Extract some flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* get the corresponding flag set of the group */
        switch (k)
        {
                case 0: flag_set = flags_groups[grp].flags1; flag_test = f1; break;
                case 1: flag_set = flags_groups[grp].flags2; flag_test = f2; break;
                case 2: flag_set = flags_groups[grp].flags3; flag_test = f3; break;
                case 3: flag_set = flags_groups[grp].flags4; flag_test = f4; break;
                case 4: flag_set = flags_groups[grp].esp; flag_test = esp; break;
                default: flag_set = flags_groups[grp].flags1; flag_test = f1; break;
        }

        /* If no flags, no need to look */
        if (!count_bits(flag_set)) return 0;

        while (tries--)
        {
                /* get a random flag */
                f = BIT(rand_int(32));

                /* is it part of the group */
                if (!(f & flag_set)) continue;

                /* Already got it */
                if (f & flag_test) continue;

                /* Ok one */
                break;
        }

        if (tries <= 1) return (0);
        else return (f);
}

/* Add a flags from a flag group */
void gain_flag_group_flag(object_type *o_ptr, bool silent)
{
        int grp = 0, k = 0;
        u32b f = 0;
        int tries = 20000;

        if (!count_bits(o_ptr->pval3)) return;

        while (tries--)
        {
                /* Get a flag set */
                k = rand_int(5);

                /* get a flag group */
                grp = rand_int(MAX_FLAG_GROUP);

                if (!(BIT(grp) & o_ptr->pval3)) continue;

                /* Return a flag from the group/set */
                f = get_flag(o_ptr, grp, k);

                if (!f) continue;

                break;
        }

        if (tries <= 1) return;

        switch (k)
        {
                case 0: o_ptr->art_flags1 |= f; break;
                case 1: o_ptr->art_flags2 |= f; break;
                case 2: o_ptr->art_flags3 |= f; break;
                case 3: o_ptr->art_flags4 |= f; break;
                case 4: o_ptr->art_esp |= f; break;
        }

        if (!silent)
        {
                char o_name[80];

                object_desc(o_name, o_ptr, FALSE, 0);
                msg_format("%s gains a new power from the %s realm.", o_name, flags_groups[grp].name);
        }
}

/*
 * When an object gain a level, he can gain some attributes
 */
void object_gain_level(object_type *o_ptr)
{
        u32b f1, f2, f3, f4, f5, esp;

	/* Extract some flags */
        object_flags(o_ptr, &f1, &f2, &f3, &f4, &f5, &esp);

        /* First it can gain some tohit and todam */
        if((o_ptr->tval == TV_AXE) || (o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM) ||
           (o_ptr->tval == TV_HAFTED) || (o_ptr->tval == TV_MSTAFF))
        {
                int k = rand_int(100);

                /* gain +2,+1 */
                if (k < 33)
                {
                        o_ptr->to_h += randint(2);
                        o_ptr->to_d += 1;
                }
                /* +1 and 1 point */
                else if (k < 66)
                {
                        o_ptr->to_h += 1;
                        o_ptr->pval2++;

                        if (magik(NEW_GROUP_CHANCE)) gain_flag_group(o_ptr, FALSE);
                }
                else
                {
                        if (!o_ptr->pval3) gain_flag_group(o_ptr, FALSE);

                        gain_flag_group_flag(o_ptr, FALSE);

                        if (!o_ptr->pval) o_ptr->pval = 1;
                        else
                        {
                                while (magik(20 - (o_ptr->pval * 2))) o_ptr->pval++;

                                if (o_ptr->pval > 5) o_ptr->pval = 5;
                        }
                }
        }
}

