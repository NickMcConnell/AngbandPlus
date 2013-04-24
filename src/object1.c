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
 * General information about classes of objects.
 *
 * Index is tval. What the hell is going on with entry 90/91?
 */
cptr obj_class_info[101] =
{
	/* 0  */"",
	/* 1  */"Skeletons are the remains of adventurers less lucky than you.",
	/* 2  */"Empty bottles at one time may have held powerful tonics",
	/* 3  */"Junk is useless trash. Perhaps someday you may find a use for it.",
	/* 4  */"",
	/* 5  */"Spikes are used to jam doors shut; a well-spiked door is difficult to open.",
	/* 6  */"",
	/* 7  */"Chests may have some really good treasures hidden inside, but can be perilous to open.  Bashing a locked chest damages whatever is inside, but also any traps that guard it.",
	/* 8  */"",
	/* 9  */"",
	/* 10 */"",
	/* 11 */"",
	/* 12 */"",
	/* 13 */"",
	/* 14 */"",
	/* 15 */"",
	/* 16 */"Pistol ammo.",
	/* 17 */"Rifle ammo.",
	/* 18 */"Shotgun ammo.",
	/* 19 */"Guns are powerful firearms that allow you to inflict damage from a distance without using magic. Pistols, rifles, and shotguns each require different types of ammo. Great skill with guns will help you stay alive.",
	/* 20 */"Diggers, especially heavy ones, are invaluable for forced entry and escape and can make a lucky miner rich.",
	/* 21 */"Hafted weapons rely on blunt force to inflict damage. Although heavy, they can do a lot of damage.",
	/* 22 */"Pole-mounted weapons are often cumbersome, but some offer both a high degree of protection and powerful piercing attacks.",
	/* 23 */"The effectiveness of swords depends on keen edges.  They tend to be quite light and are easy to use, but some may not deal enough damage for your liking.",
	/* 24 */"Daggers rely on their lightweight quickness to cause piercing damage. What they lose in raw damage, they make up for in quick strikes.",
	/* 25 */"Axes are powerful weapons doing large amounts of slashing damage. Many people may find them too heavy to wield.",
	/* 26 */"Blunt weapons such as clubs and quarterstaves often provide a defensive bonus in addition to their powerful blows.",
	/* 27 */"",
	/* 28 */"",
	/* 29 */"",
	/* 30 */"Footwear protects the feet only, but some rare items of this type have magics to render them fleet, light, or steady.",
	/* 31 */"Your hands would benefit from protection too, but many magic users need to keep their fingers unencumbered or magically supple.",
	/* 32 */"Many a blow will be struck upon your head, and protection here will certainly be helpful.  Some rare items may protect and enhance your mind.",
	/* 33 */"Many a blow will be struck upon your head, and protection here will certainly be helpful.  Some rare items may protect and enhance your mind.",
	/* 34 */"Trousers are quite useful in keeping your bits warm and secure. Enchantments may often grace leg coverings.",
	/* 35 */"Experienced adventurers wrap a cloak around their body.  Some rare items of this type may allow you to slip silently around less alert enemies.",
	/* 36 */"Some kind of body protection will become a necessity as you face ever more dangerous opponents; rare items of this type may hold many and varied protective magics.",
	/* 37 */"Some kind of body protection will become a necessity as you face ever more dangerous opponents; rare items of this type may hold many and varied protective magics.",
	/* 38 */"",
	/* 39 */"An adventurer who cannot see is jackal food.  Rare items of this type may have an extended lighting radius or require no fuel.",
	/* 40 */"Amulets slip around your neck, and almost all have magics wondrous or perilous bound inside.",
	/* 41 */"",
	/* 42 */"",
	/* 43 */"",
	/* 44 */"",
	/* 45 */"You may wear a ring upon each of your two ring fingers, and benefit or suffer from the magics it contains.",
	/* 46 */"Mechanical torsos often contain powerful enhancements and upgrades for automata and steam-mecha.",
	/* 47 */"Mechanical heads often contain powerful enhancements and upgrades for automata and steam-mecha.",
	/* 48 */"Mechanical arms often contain powerful enhancements and upgrades for automata and steam-mecha.",
	/* 49 */"Mechanical feet often contain powerful enhancements and upgrades for automata and steam-mecha. Mechanical feet have leg attachments that connect to the torso.",
	/* 50 */"",
	/* 51 */"",
	/* 52 */"",
	/* 53 */"",
	/* 54 */"",
	/* 55 */"Tools are often heavy, and take up plenty of space in your backpack, but these strange-looking machines often can produce powerful effects covering large areas.  Tools are highly durable and are easy to repower when they run out of energy.",
	/* 56 */"",
	/* 57 */"",
	/* 58 */"",
	/* 59 */"",
	/* 60 */"",
	/* 61 */"",
	/* 62 */"",
	/* 63 */"",
	/* 64 */"",
	/* 65 */"Ray guns produce a range of powerful combat effects.  Once its energy is depleted, a ray gun is useless until recharged.",
	/* 66 */"The powerful apparatuses can, utilizing some advanced form of energy, produce a limitless amount of effects.  However they require time between uses to recover.",
	/* 67 */"",
	/* 68 */"",
	/* 69 */"",
	/* 70 */"One will often find small mechanisms lying about.  They are easy to use, and are a warrior's best chance at making use of strange effects.",
	/* 71 */"Sometimes one will find an ancient document lying about with strange lore and stories.",
	/* 72 */"",
	/* 73 */"Books often contain tales of the fantastic; however, in your case they may contain information that could keep you alive. If you had somewhere you could take the book and read it in peace so you could dig out the information. . .",
	/* 74 */"",
	/* 75 */"Healers, alchemists, scientists, and sages create tonics in vast quantities, and store them in small flasks of clear glass or gleaming crystal.  Once quaffed, a tonic is guaranteed to work, but sometimes the contents are nothing but harmful snake oil. . .",
	/* 76 */"",
	/* 77 */"Oil is used to fuel lanterns, can be thrown to cause damage, and is useful in keeping complex machines running.",
	/* 78 */"",
	/* 79 */"",
	/* 80 */"Deep in the murky dungeons strange mushrooms grow, and monsters rout about sealed packets of food that their owners will never need again.	"
	/* 81 */"",
	/* 82 */"",
	/* 83 */"",
	/* 84 */"",
	/* 85 */"",
	/* 86 */"",
	/* 87 */"",
	/* 88 */"",
	/* 89 */"",
	/* 90 */"",
	/* 91 */"A manual of sorcerous magics or device of powerful effects. You must be skilled to prevent dark forces from stealing your soul if foolish enough to work with sorcery. Items are safer, but are often quite difficult to figure out.  Arcane (red) books use Ego for failure rates, Divine (green) books use Ego/Sch for failure rates, and Items (brown) use Sch.",
	/* 92 */"",
	/* 93 */"",
	/* 94 */"",
	/* 95 */"",
	/* 96 */"",
	/* 97 */"",
	/* 98 */"",
	/* 99 */"",
	/* 100 */"Small valuables and coins."
};

/*
 * Max sizes of the following arrays.
 */
#define MAX_ROCKS      46       /* Used with rings (min 38) */
#define MAX_AMULETS    30       /* Used with amulets (min 28) */
#define MAX_WOODS      35       /* Used with tools (min 30) */
#define MAX_METALS     32       /* Used with rayguns/apparatuses (min 29/28) */
#define MAX_COLORS     65       /* Used with tonics (min 63) */
#define MAX_SHROOM     26       /* Used with anodyne (min 26) */
#define MAX_TITLES     50       /* Used with mechanism (min 48) */
/* old scroll syllables */
/* #define MAX_SYLLABLES 158    */   /* Used with mechanism (see below) */
#define MAX_MECH_ADJ	49			
#define MAX_MECH_NOUN1	18
#define MAX_MECH_NOUN2	12

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
	"Obsidian", "Silver", "Tortoise Shell", "Bark", "Jet",
	"Engagement", "Viridigen", "Grass", "Oak", "Copper",
	"Bone"
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
	TERM_L_DARK, TERM_L_WHITE, TERM_UMBER, TERM_L_UMBER, TERM_L_DARK,
	TERM_YELLOW, TERM_L_GREEN, TERM_GREEN, TERM_L_UMBER, TERM_L_GREEN,
	TERM_L_WHITE
};


/*
 * Amulets (adjectives and colors).
 */

static cptr amulet_adj[MAX_AMULETS] =
{
	"Amber", "Driftwood", "Coral", "Agate", "Ivory",
	"Obsidian", "Bone", "Brass", "Bronze", "Pewter",
	"Tortoise Shell", "Golden", "Azure", "Crystal", "Silver",
	"Copper", "Silver", "Platinum", "Pearl", "Oak",
	"Mithril", "Onyx", "Opal", "Sapphire", "Ruby",
	"Jade", "Viridigen", "Teak", "Lapis Lauzi", "Star Sapphire"
};

static byte amulet_col[MAX_AMULETS] =
{
	TERM_YELLOW, TERM_L_UMBER, TERM_WHITE, TERM_L_WHITE, TERM_WHITE,
	TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_L_UMBER, TERM_SLATE,
	TERM_UMBER, TERM_YELLOW, TERM_L_BLUE, TERM_WHITE, TERM_L_WHITE,
	TERM_L_UMBER, TERM_L_WHITE, TERM_L_WHITE, TERM_WHITE, TERM_UMBER,
	TERM_L_WHITE, TERM_L_DARK, TERM_L_WHITE, TERM_L_BLUE, TERM_L_RED,
	TERM_L_GREEN, TERM_GREEN, TERM_L_UMBER, TERM_L_BLUE, TERM_BLUE
};


/*
 * tools (adjectives and colors).
 */

static cptr tool_adj[MAX_WOODS] =
{
	"Rusty", "Polished", "Greasy", "Clean", "Complex",
	"Shiny", "Dull", "Glowing", "Balanced", "Small",
	"Large", "Iron", "Ironwood", "Steel", "Cavorite",
	"Strange", "Intricate", "Slim", "Weighty", "Worn",
	"Chipped", "Round", "Square", "Thick", "Ivory",
	"Marble", "Rickety", "Bamboo", "Silver", "Runed",
	"Golden", "Ashen", "Curved", "Adjustable", "Inscribed"
};

static byte tool_col[MAX_WOODS] =
{
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_RED,
	TERM_RED, TERM_L_UMBER, TERM_L_UMBER, TERM_L_UMBER, TERM_UMBER,
	TERM_GREEN, TERM_L_UMBER, TERM_L_UMBER, TERM_L_WHITE, TERM_UMBER,
	TERM_YELLOW, TERM_SLATE, TERM_SLATE, TERM_SLATE, TERM_SLATE
};


/*
 * ray guns (adjectives and colors).
 */

static cptr ray_adj[MAX_METALS] =
{
	"Aluminum", "Cast Iron", "Chromium", "Copper", "Gold",
	"Iron", "Magnesium", "Molybdenum", "Nickel", "Rusty",
	"Silver", "Steel", "Tin", "Titanium", "Tungsten",
	"Zirconium", "Zinc", "Aluminum-Plated", "Copper-Plated", "Gold-Plated",
	"Nickel-Plated", "Silver-Plated", "Steel-Plated", "Tin-Plated", "Zinc-Plated",
	"Pearl-Plated", "Ivory", "Runed", "Bronze", "Brass",
	"Platinum", "Lead"/*,"Lead-Plated","XXX","Pewter"*/
};

static byte ray_col[MAX_METALS] =
{
	TERM_L_BLUE, TERM_L_DARK, TERM_WHITE, TERM_L_UMBER, TERM_YELLOW,
	TERM_SLATE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_UMBER, TERM_RED,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_WHITE, TERM_WHITE,
	TERM_L_WHITE, TERM_L_WHITE, TERM_L_BLUE, TERM_L_UMBER, TERM_YELLOW,
	TERM_L_UMBER, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE, TERM_L_WHITE,
	TERM_WHITE, TERM_WHITE, TERM_UMBER, TERM_L_UMBER, TERM_L_UMBER,
	TERM_WHITE, TERM_SLATE, /*TERM_SLATE,TERM_XXX,TERM_SLATE*/
};


/*
 * Rods (adjectives and colors).
 *
 * Efficiency -- copied from ray gun arrays.
 */

static cptr apparatus_adj[MAX_METALS];

static byte apparatus_col[MAX_METALS];


/*
 * Mushrooms (adjectives and colors).
 */

static cptr food_adj[MAX_SHROOM] =
{
	"Blue", "Black", "Black Spotted", "Brown", "Dark Blue",
	"Dark Green", "Dark Red", "Yellow", "Furry", "Green",
	"Grey", "Light Blue", "Light Green", "Violet", "Red",
	"Slimy", "Tan", "White", "White Spotted", "Wrinkled",
	"Flowering", "Crispy", "Smooth", "Dank", "Round", "Silky"
};

static byte food_col[MAX_SHROOM] =
{
	TERM_BLUE, TERM_L_DARK, TERM_L_DARK, TERM_UMBER, TERM_BLUE,
	TERM_GREEN, TERM_RED, TERM_YELLOW, TERM_L_WHITE, TERM_GREEN,
	TERM_SLATE, TERM_L_BLUE, TERM_L_GREEN, TERM_VIOLET, TERM_RED,
	TERM_SLATE, TERM_L_UMBER, TERM_WHITE, TERM_WHITE, TERM_UMBER,
	TERM_GREEN, TERM_UMBER, TERM_WHITE, TERM_L_DARK, TERM_VIOLET, TERM_VIOLET,
};


/*
 * Color adjectives and colors, for tonics.
 *
 * Hack -- The first four entries (water, apple juice, slime mold juice,
 * and undefined) are hard-coded.
 */

static cptr tonic_adj[MAX_COLORS] =
{
	"Clear", "Light Brown", "Icky Green", "xxx",
	"Addington's Phos Ferrone Iron", "Althrop's Constitutional",
	"Ambrecht's Coca Wine", "Angelica Bitter", "Dr. A. Armistead's Fameous Ague",
	"Baldwin's Celery Pepsin & Dandelion", "Balyeat's Fig", 
	"Bear Brand Wild Cherry", "Dr. Beard's Alterative", "Betula Beer",
	"A. M. Bininger & Co. Wheat", "Bitter Apple",
	"Dr. Blendigo's Celery", "Bock's Restorative",
	"Brother Benjamin Great Herbalo", "Dr. Brooks' Antimalarial",
	"Burk's Iron", "Cardui, The Women's", "S.S. Clark's Diamond Family",
	"Cla-wood Malt", "Colden's Liquid Beef", "Corona Distemper",
	"Dalton's Sarsaparilla and Nerve", "Davis's Morning Noon & Night",
	"The Imperial King of all", "Eureka Pepsin - A Never Failing",
	"Ferro China Milano", "Fletcher's Vege", "Goging's Wild Cherry",
	"Gray's Sparkling Spray", "Happy Home Blood Purifier and Health",
	"Highland's Bitters and Scotch", "Iowna Brain & Nerve", 
	"W.M. Johnson's Pure Herb", "Ka No Blood & Nerve", 
	"Keck's Lung & Liver", "Kress Fever",
	"Dr Kurnitzki's Aromatic Wire Grass", "Leonardi's Chill Remedy & Iron", 
	"Liebig's Malt", "Magors 1000 - Phosphor", "Malto Iron",
	"Mexican Herb", "Morrison's Sure Cure", "Mull's Grape", 
	"Old Sachem Bitters & Wigwam", "Owbridge's Lung",
	"Parker's best", "C.G. Pendleton's", "Pepper's Quinine & Iron", 
	"Primley's Iron & Wahoo", "Psychine", "Quin's Chill", 
	"Ramon's Pepsin Chill", "Royal Pepsin", "Sano Rheumatic Cure & System",
	"Vin-O-Sula Cuban", "Walt's Wild Cherry", "Mickey's Magical Brew",
	"Lillybeck's Tasteless Chill", "Sherry & Iron"
};

static byte tonic_col[MAX_COLORS] =
{
	TERM_WHITE, TERM_L_UMBER, TERM_GREEN, 0,
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
	TERM_VIOLET, TERM_RED, TERM_YELLOW, TERM_YELLOW, TERM_L_BLUE, 
	TERM_L_BLUE, TERM_UMBER, TERM_L_RED, TERM_GREEN
};

static cptr mech_adj[MAX_MECH_ADJ] =
{
	"Martian ", "Venusian ", "Alien ", "Analog ", "Electric ",
	"Clockwork ", "Mechanical ", "Steam-driven ", "Aluminum ", 
	"Cast Iron ", "Chromium ", "Copper ", "Gold ", "Iron ", "Magnesium ", 
	"Molybdenum ", "Nickel ", "Rusty ", "Silver ", "Steel ", "Tin ", 
	"Titanium ", "Tungsten ", "Zirconium ", "Zinc ", "Aluminum-Plated ", 
	"Copper-Plated ", "Gold-Plated ", "Nickel-Plated ", "Silver-Plated ", 
	"Steel-Plated ", "Tin-Plated ", "Zinc-Plated ", "Mithril-Plated ", 
	"Mithril ", "Runed ", "Bronze ", "Brass ", "Platinum ", "Lead ",
	"Cavorite ", "Whirring ", "Clanking ", "Clicking ", "Bouncing ",
	"Ringing ", "Whizzing ", "Moaning ", "Humming "
};
static cptr mech_noun1[MAX_MECH_NOUN1] =
{
	"Aether", "Anemo", "Astro", "Geo", "Gyro", "Helio", "Hydro",
	"Micro", "Phono", "Photo", "Seismo", "Tele", "Macro", "Radio",
	"Stereo", "Quadra", "Synchro", "Aero"
};
static cptr mech_noun2[MAX_MECH_NOUN2] =
{
	"graph", "meter", "phone", "scope", "ton", "pan", "com", "net",
	"cid", "phonic", "dyne", "copter"
};

/*
 * Hold the titles of mechanisms, 6 to 14 characters each.
 *
 * Also keep an array of mechanism colors (always WHITE for now).
 */

static char mechanism_adj[MAX_TITLES][42];

static byte mechanism_col[MAX_TITLES];


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

		case TV_TOOL:
		{
			return (0xA0 + tool_col[k_ptr->sval]);
		}

		case TV_RAY:
		{
			return (0xB0 + ray_col[k_ptr->sval]);
		}

		case TV_APPARATUS:
		{
			return (0xC0 + apparatus_col[k_ptr->sval]);
		}

		case TV_MECHANISM:
		{
			return (0xD0 + mechanism_col[k_ptr->sval]);
		}

		case TV_TONIC:
		{
			return (0xE0 + tonic_col[k_ptr->sval]);
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
 * Prepare the "variable" part of the "k_info" array.
 *
 * The "color"/"metal"/"type" of an item is its "flavor".
 * For the most part, flavors are assigned randomly each game.
 *
 * Initialize descriptions for the "colored" objects, including:
 * Rings, Amulets, tools, ray guns, apparatuses, Food, tonics, mechanisms.
 *
 * The first 4 entries for tonics are fixed (Water, Apple Juice,
 * Slime Mold Juice, Unused tonic).
 *
 * mechanism titles are always between 6 and 14 letters long.  This is
 * ensured because every title is composed of whole words, where every
 * word is from 1 to 8 letters long (one or two syllables of 1 to 4
 * letters each), and that no mechanism is finished until it attempts to
 * grow beyond 15 letters.  The first time this can happen is when the
 * current title has 6 letters and the new word has 8 letters, which
 * would result in a 6 letter mechanism title.
 *
 * Duplicate titles are avoided by requiring that no two mechanisms share
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


	/* Efficiency -- apparatuses/ray guns share initial array */
	for (i = 0; i < MAX_METALS; i++)
	{
		apparatus_adj[i] = ray_adj[i];
		apparatus_col[i] = ray_col[i];
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

	/* tools */
	for (i = 0; i < MAX_WOODS; i++)
	{
		j = rand_int(MAX_WOODS);
		temp_adj = tool_adj[i];
		tool_adj[i] = tool_adj[j];
		tool_adj[j] = temp_adj;
		temp_col = tool_col[i];
		tool_col[i] = tool_col[j];
		tool_col[j] = temp_col;
	}

	/* ray guns */
	for (i = 0; i < MAX_METALS; i++)
	{
		j = rand_int(MAX_METALS);
		temp_adj = ray_adj[i];
		ray_adj[i] = ray_adj[j];
		ray_adj[j] = temp_adj;
		temp_col = ray_col[i];
		ray_col[i] = ray_col[j];
		ray_col[j] = temp_col;
	}

	/* apparatuses */
	for (i = 0; i < MAX_METALS; i++)
	{
		j = rand_int(MAX_METALS);
		temp_adj = apparatus_adj[i];
		apparatus_adj[i] = apparatus_adj[j];
		apparatus_adj[j] = temp_adj;
		temp_col = apparatus_col[i];
		apparatus_col[i] = apparatus_col[j];
		apparatus_col[j] = temp_col;
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

	/* tonics */
	for (i = 4; i < MAX_COLORS; i++)
	{
		j = rand_int(MAX_COLORS - 4) + 4;
		temp_adj = tonic_adj[i];
		tonic_adj[i] = tonic_adj[j];
		tonic_adj[j] = temp_adj;
		temp_col = tonic_col[i];
		tonic_col[i] = tonic_col[j];
		tonic_col[j] = temp_col;
	}

	/* Mechanisms (random titles, always white) */
	for (i = 0; i < MAX_TITLES; i++)
	{
		/* Get a new title */
		while (TRUE)
		{
			char buf[80];

			bool okay;

			/* Start a new title */
			buf[0] = '\0';			
			
			/* Add the Adjective */
			strcat(buf, mech_adj[rand_int(MAX_MECH_ADJ)]);
			
			/* Add the first noun */
			strcat(buf, mech_noun1[rand_int(MAX_MECH_NOUN1)]);
			
			/* Add the second noun */
			strcat(buf, mech_noun2[rand_int(MAX_MECH_NOUN2)]);
						
			/* Save the title */
			strcpy(mechanism_adj[i], buf);

			/* Assume okay */
			okay = TRUE;

			/* I'll have to figure out a different way to */
			/*   check for "duplicate" mechanism titles */

			/* Break when done */
			if (okay) break;
		}

		/* All mechanisms are white */
		mechanism_col[i] = TERM_WHITE;
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
		/* if (!k_ptr->flavor) k_ptr->aware = TRUE; */
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
#define OBJECT_FLAGS_FULL   1 /* Full info */
#define OBJECT_FLAGS_KNOWN  2 /* Only flags known to the player */
#define OBJECT_FLAGS_RANDOM 3 /* Only known random flags */


/*
 * Obtain the non-pval qualities of an item
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
	bool spoil = FALSE;


	/* Always start with empty flags */
	(*f1) = (*f2) = (*f3) = 0L;

	/* Ignore non-objects */
	if (!o_ptr->k_idx) return;

	/* Require identification sometimes */
	if (mode != OBJECT_FLAGS_FULL)
	{
		/* Must be identified */
		if (!object_known_p(o_ptr)) return;
	}

	/* Usually, get the standard object/ego-item/artifact flags. */
	if (mode != OBJECT_FLAGS_RANDOM)
	{
		k_ptr = &k_info[o_ptr->k_idx];

		/* Base object */
		(*f1) = k_ptr->flags1;
		(*f2) = k_ptr->flags2;
		(*f3) = k_ptr->flags3;

		/* Ego-item -- always fully known if ID'd */
		if (o_ptr->name2)
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];

			/* Add to base object kind flags */
			(*f1) |= e_ptr->flags1;
			(*f2) |= e_ptr->flags2;
			(*f3) |= e_ptr->flags3;
		}

		/* Artifact -- non-obvious flags require *ID* to know */
		if (o_ptr->name1)
		{
			artifact_type *a_ptr = &a_info[o_ptr->name1];

			/* We know everything */
			if ((mode == OBJECT_FLAGS_FULL) || (spoil) ||
			    (o_ptr->ident & (IDENT_KNOWN)))
			    /* the above was IDENT_MENTAL */
			{
				/* Overwrite base object kind flags */
				(*f1) = a_ptr->flags1;
				(*f2) = a_ptr->flags2;
				(*f3) = a_ptr->flags3;
			}

			/* We don't know everything */
			else
			{
				/* Obvious flags (easy-to-know-nastiness) */
				(*f3) |= (a_ptr->flags3 & (TR3_LIGHT_CURSE |
					   TR3_HEAVY_CURSE | TR3_PERMA_CURSE |
					   TR3_DRAIN_EXP   | TR3_DRAIN_HP |
					   TR3_DRAIN_SP    | TR3_DRAIN_ITEM |
					   TR3_TELEPORT    | TR3_NO_MAGIC |
					   TR3_AGGRAVATE   | TR3_DISRUPT_SPELL |
					   TR3_NEVER_BLOW ));
			}
		}
	}

	/* Require full knowledge */
	if ((mode == OBJECT_FLAGS_FULL) || (spoil) ||
	    (o_ptr->ident & (IDENT_MENTAL)))
	{
		/* Extra flags -- set 1 */
		(*f1) |= o_ptr->flags1;

		/* Extra flags -- set 2 */
		(*f2) |= o_ptr->flags2;

		/* Extra flags -- set 3 */
		(*f3) |= o_ptr->flags3;
	}
}


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
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	object_type *gun_ptr;

	cptr basenm, modstr;

	int power;

	bool aware;
	bool known;
	bool easy_know;

	bool flavor;

	bool append_name;
	bool do_prefix;

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
	char d1 = '<', d2 = '>';

	char discount_buf[80];
	char tmp_buf[128];

	u32b f1, f2, f3;

	int dd, ds, db;
	int tmul;
	long avgdam;


	/* Extract some flags */
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

	/* Assume no name appending */
	append_name = FALSE;

	/* Assume no prefix */
	do_prefix = FALSE;

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
		case TV_TEXT:
		{
			break;
		}

		/* Missiles/Bows/Weapons */
		case TV_AMMO:
		case TV_SHOT:
		case TV_BULLET:
		case TV_GUN:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DAGGER:
		case TV_AXES:
		case TV_BLUNT:
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
		case TV_LEG:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:
		case TV_MECHA_TORSO:
		case TV_MECHA_HEAD:
		case TV_MECHA_ARMS:
		case TV_MECHA_FEET:
		{
			show_armour = TRUE;
			break;
		}

		/* Lites (including a few "Specials") */
		case TV_LITE:
		{
			break;
		}
		
		/* Books */
		case TV_BOOK:
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
			/*if (!aware && (o_ptr->sval == SV_RING_POWER))
			 *{
			 *	modstr = "Plain Gold";
			 *}
			 */
			break;
		}

		/* tools */
		case TV_TOOL:
		{
			/* Color the object */
			modstr = tool_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Tool~" : "& Tool~");

			break;
		}

		/* ray guns */
		case TV_RAY:
		{
			/* Color the object */
			modstr = ray_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Ray Gun~" : "& Ray Gun~");

			break;
		}

		/* apparatuses */
		case TV_APPARATUS:
		{
			/* Color the object */
			modstr = apparatus_adj[o_ptr->sval];


			/* Apparatus is now just a generic device name */
			/* It could be a deck of cards, a telescope, or any  */
			/* kind of object at all */
			if (aware) basenm = (k_name + k_ptr->name);
			else basenm = "& # Apparatus~";

			/* Old code */
			/* if (aware) append_name = TRUE; */
			/* basenm = (flavor ? "& # Apparatus~" : "& Apparatus~"); */

			break;
		}

		/* Mechanism */
		case TV_MECHANISM:
		{
			/* Color the object */
			modstr = mechanism_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Mechanism~" : "& Mechanism~");

			break;
		}
		

		/* Tonics */
		case TV_TONIC:
		{
			/* Color the object */
			modstr = tonic_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& \"#\" Tonic~" : "& Tonic~");

			break;
		}

		/* Food */
		/* Anodynes are drugs, something that sooths pain */
		case TV_FOOD:
		{
			/* Ordinary food is "boring" */
			if (o_ptr->sval >= SV_FOOD_MIN_FOOD) break;

			/* Color the object */
			modstr = food_adj[o_ptr->sval];
			if (aware) append_name = TRUE;
			basenm = (flavor ? "& # Anodyne~" : "& Anodyne~");

			break;
		}

		/* Magic Books */
		case TV_MAGIC_BOOK:
		{
			modstr = basenm;
			switch (books[o_ptr->sval].flags & SBF_TYPE_MASK)
			{
				case SBF_MAGIC:
				{
					basenm = "& Book~ of Arcane Incantations #";
					break;
				}
	
				case SBF_PRAYER:
				{
					basenm = "& Holy Book~ of Prayers #";
					break;
				}

				case SBF_DEVICE:
				{
					basenm = "& Enigmatic Device~ #";
					break;
				}
				default:
				{
					/* special books are here */
					break;
				}
			}
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

	/* Non-artifact perfectly balanced throwing weapons are indicated. */
	if ((f2 & (TR2_PERFECT_BALANCE)) && (f2 & (TR2_THROW)) &&
	    (known) && (!artifact_p(o_ptr)))
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
			if ((o_ptr->number != 1) && !(known && artifact_p(o_ptr)))
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


	/* Append the "kind name" to the "base name" */
	if (append_name)
	{
		object_desc_str_macro(t, " of ");
		object_desc_str_macro(t, (k_name + k_ptr->name));
	}


	/* Hack -- Append "Artifact" or "Special" names */
	if ((known) || ((easy_know) && (aware)))
	{
		bool no_ego_name = FALSE;

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
			object_desc_str_macro(t, (e_name + e_ptr->name));
		}
	}


	/* No more details wanted */
	if (mode < 1) goto object_desc_done;


	/* Hack -- Chests must be described in detail */
	if (o_ptr->tval == TV_CHEST)
	{
		cptr tail = "";

		/*may be a quest item*/
		if(o_ptr->ident & IDENT_QUEST)
		{
			tail = " (Sealed by Guild Magic)";
		}

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
				case CHEST_LOSE_MUS:
				{
					tail = " (Poison Needle)";
					break;
				}
				case CHEST_LOSE_VIG:
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
		case TV_AMMO:
		case TV_SHOT:
		case TV_BULLET:
		{
			/* Fall through */
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DAGGER:
		case TV_AXES:
		case TV_BLUNT:
		case TV_DIGGING:
		{
			/* Append a "damage" string */
			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, p1);
			object_desc_num_macro(t, o_ptr->dd);
			object_desc_chr_macro(t, 'd');
			object_desc_num_macro(t, o_ptr->ds);
			object_desc_chr_macro(t, '|');
			object_desc_num_macro(t, o_ptr->force);
			object_desc_chr_macro(t, p2);

			/* All done */
			break;
		}

		/* Guns */
		case TV_GUN:
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

		/* Light sources */
		case TV_LITE:
		{
			/* Display the light radius */
			power = get_object_pval(o_ptr, TR_PVAL_LIGHT);

			object_desc_chr_macro(t, ' ');
			object_desc_chr_macro(t, b1);
			object_desc_num_macro(t, power);
			object_desc_chr_macro(t, b2);

			break;
		}
	}


	/* Add the weapon bonuses */
	if (known)
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

	gun_ptr = &inventory[INVEN_GUN];

	/* if have a firing weapon + ammo matches bow */
	if (gun_ptr->k_idx && (p_ptr->ammo_tval == o_ptr->tval))
	{
		/* See if the bow is "known" - then set damage bonus */
		if (object_known_p(gun_ptr))
		{
			db = gun_ptr->to_d;
		}
		else
		{
			db = 0;
		}

		/* effect of player */
		/* db += p_ptr->dis_to_d; */

		if (p_ptr->skills[SK_VICIOUS_SHOT].skill_max > 0) 
				db += p_ptr->skills[SK_VICIOUS_SHOT].skill_rank;
		

		/* effect of ammo */
		if (known) db += o_ptr->to_d;

		dd = o_ptr->dd;
		ds = o_ptr->ds;

		/* effect of damage dice . dice * (sides + 1) / 2*/
		avgdam = ((dd * (ds + 1)) / 2) + db;

		/* Bow properties */
		/* energy_use = p_ptr->bow_energy; */
		tmul = p_ptr->ammo_mult;

		/* Get extra "power" from "extra might" */
		tmul += get_object_pval(o_ptr, TR_PVAL_MIGHT);

		/* launcher multiplier */
		avgdam *= tmul;

		/* no longer using this - it's like a brand now */
		/* if (p_ptr->skills[SK_PYROKINETICS].skill_max > 0) 
				avgdam += (p_ptr->skills[SK_PYROKINETICS].skill_rank * 3); */

		/* display (shot damage/ avg damage) */
		object_desc_chr_macro(t, ' ');
		object_desc_chr_macro(t, p1);
		object_desc_int_macro(t, avgdam);
		object_desc_chr_macro(t, '/');

		tmul = p_ptr->num_fire;

		if (tmul == 0)
		{
			object_desc_chr_macro(t, '0');
		}
		else
		{
			/* calc effects of energy  x2 */
			avgdam *= (p_ptr->num_fire);

			/* rescale */
			object_desc_num_macro(t, avgdam);
		}

		object_desc_chr_macro(t, p2);
	}


	/* Add the armor bonuses */
	if (known)
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


	/* Hack -- ray guns and tools have charges */
	if (known &&
	    ((o_ptr->tval == TV_TOOL) ||
	     (o_ptr->tval == TV_RAY)))
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

	/* Hack -- apparatuses have a "charging" indicator */
	else if (known && (o_ptr->tval == TV_APPARATUS))
	{
		/* Hack -- Dump " (charging)" if relevant */
		if (o_ptr->pval)
		{
			object_desc_str_macro(t, " (recharging)");
		}
	}

	/* Process light sources that need fuel */
	else if ((o_ptr->tval == TV_LITE) && (!(f3 & (TR3_NO_FUEL))))
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
			object_desc_chr_macro(t, d1);
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
				object_desc_chr_macro(t, d1);
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
				object_desc_chr_macro(t, d1);
			}

			object_desc_int_macro(t, o_ptr->pval3);
			displayed++;
		}

#if 0
		/* We sometimes show pval descriptions */
		if ((displayed == 1) && (!(o_ptr->flags3 & (TR3_HIDE_TYPE))))
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

			/* Stealth */
			else if (get_object_pval(o_ptr, TR_PVAL_STEALTH))
			{
				/* Dump " to stealth" */
				tail = " to stealth";
			}

			/* Saving throw */
			else if (get_object_pval(o_ptr, TR_PVAL_SAVE))
			{
				/* Dump " to saving throw" */
				tail = " to saving throw";
			}

			/* Mana */
			else if (get_object_pval(o_ptr, TR_PVAL_MANA))
			{
				/* Dump " to mana" */
				tail = " to mana (x20)";
			}

			/* Awareness */
			else if (get_object_pval(o_ptr, TR_PVAL_SEARCH))
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
#endif
		if (displayed) object_desc_chr_macro(t, d2);
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

	/* Hack -- Use "empty" for empty ray guns/tools */
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

	/* Truncate the string to 80 chars */
	tmp_buf[79] = '\0';

	/* Copy the string over */
	strcpy(buf, tmp_buf);
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void object_desc_store(char *buf, const object_type *o_ptr, int pref, int mode)
{
	object_type *i_ptr;
	object_type object_type_body;

	byte hack_flavor;
	bool hack_aware;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Copy the object */
	object_copy(i_ptr, o_ptr);

	/* Save the "flavor" */
	hack_flavor = k_info[i_ptr->k_idx].flavor;

	/* Save the "aware" flag */
	hack_aware = k_info[i_ptr->k_idx].aware;

	/* Clear the flavor */
	k_info[i_ptr->k_idx].flavor = FALSE;

	/* Set the "known" flag */
	i_ptr->ident |= (IDENT_KNOWN);

	/* Force "aware" for description */
	k_info[i_ptr->k_idx].aware = TRUE;


	/* Describe the object */
	object_desc(buf, i_ptr, pref, mode);


	/* Restore "flavor" value */
	k_info[i_ptr->k_idx].flavor = hack_flavor;

	/* Restore "aware" flag */
	k_info[i_ptr->k_idx].aware = hack_aware;
}



/*
 * Obtain the resistances from an item 
 */
s16b object_resist(const object_type *o_ptr, int res_type)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->name1];
	ego_item_type *e_ptr = &e_info[o_ptr->name2];

	int i = 0;

	/* Random abilities for ego items */
	if (o_ptr->name1)
	{
		if (((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_HIGH_RESIST)) &&
			(o_ptr->xtra2 == res_type))
			i = 25;
	}

	return (k_ptr->res[res_type] + a_ptr->res[res_type] + e_ptr->res[res_type] + i);
}

/*
 * Obtain the resistances from an item 
 */
s16b object_resist_known(const object_type *o_ptr, int res_type)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];
	artifact_type *a_ptr = &a_info[o_ptr->name1];
	ego_item_type *e_ptr = &e_info[o_ptr->name2];

	int res = 0;

	if (object_known_p(o_ptr)) 
	{
		res = k_ptr->res[res_type] + e_ptr->res[res_type];
		if (artifact_p(o_ptr)) res += a_ptr->res[res_type];

		/* Known random ego-item resists */
		if (o_ptr->name1 && (o_ptr->ident & IDENT_MENTAL))
		if (((o_ptr->xtra1 == OBJECT_XTRA_TYPE_MID_RESIST) ||
			(o_ptr->xtra1 == OBJECT_XTRA_TYPE_HIGH_RESIST)) &&
			(o_ptr->xtra2 == res_type))
			res += 25;
	}

	return (res);
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

	/* Indexes for "inven" are easy */
	/* if (i == INVEN_LOADEDGUN) return (I2A(A2I('0')) + i - INVEN_LOADEDGUN); */
	
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

	/* Hack -- handle quiver slots. */
#if 0
	if (isdigit(c)) i = (D2I(c) + INVEN_LOADEDGUN);

	/* Convert */
	else 
	{
		i = (islower(c) ? A2I(c) : -1) + INVEN_WIELD;
	}
#endif
		
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
	u32b f1, f2, f3;
	player_flags(&f1, &f2, &f3);
	
	/* Slot for equipment */
	switch (o_ptr->tval)
	{
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DAGGER:
		case TV_AXES:
		case TV_BLUNT:
		case TV_MAGIC_BOOK:
		{
			return (INVEN_WIELD);
		}

		case TV_GUN:
		{
			return (INVEN_GUN);
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
			if (f3 & (TR3_AUTOMATA))
			{
				return (0);
			}
			else return (INVEN_BODY);
		}

		case TV_CLOAK:
		{
			return (INVEN_OUTER);
		}

		case TV_LEG:
		{
			return (INVEN_LEG);
		}

		case TV_CROWN:
		case TV_HELM:
		{
			if (f3 & (TR3_AUTOMATA))
			{
				return (0);
			}
			return (INVEN_HEAD);
		}

		case TV_GLOVES:
		{
			if (f3 & (TR3_AUTOMATA))
			{
				return (0);
			}
			return (INVEN_HANDS);
		}

		case TV_BOOTS:
		{
			if (f3 & (TR3_AUTOMATA))
			{
				return (0);
			}
			return (INVEN_FEET);
		}
		case TV_MECHA_TORSO:
		{
			if (!(f3 & (TR3_AUTOMATA)))
			{
				return (0);
			}
			return (INVEN_BODY);
		}
		case TV_MECHA_HEAD:
		{
			if (!(f3 & (TR3_AUTOMATA)))
			{
				return (0);
			}
			return (INVEN_HEAD);
		}
		case TV_MECHA_ARMS:
		{
			if (!(f3 & (TR3_AUTOMATA)))
			{
				return (0);
			}
			return (INVEN_HANDS);
		}
		case TV_MECHA_FEET:
		{
			if (!(f3 & (TR3_AUTOMATA)))
			{
				return (0);
			}
			return (INVEN_FEET);
		}
		case TV_AMMO:
		case TV_BULLET:
		case TV_SHOT:
		{
			return (INVEN_LOADEDGUN);
		}
	}

	/* No slot available */
	return (-1);
}


/*
 * Return a string mentioning how a given item is carried
 * Should add Automata descriptions for mecha parts
 */
cptr mention_use(int i)
{
	cptr p;

	/* Examine the location */
	switch (i)
	{
		case INVEN_WIELD: p = "Wielding"; break;
		case INVEN_GUN:   p = "Shooting"; break;
		case INVEN_LEFT:  p = "On left hand"; break;
		case INVEN_RIGHT: p = "On right hand"; break;
		case INVEN_NECK:  p = "Around neck"; break;
		case INVEN_LITE:  p = "Light source"; break;
		case INVEN_BODY:  
		{
			if ((p_ptr->prace == RACE_AUTOMATA) || 
				(p_ptr->prace == RACE_STEAM_MECHA)) 
			{
				p = "Body"; break;
			}
			else p = "On body"; break;
		}
		case INVEN_OUTER: p = "About body"; break;
		case INVEN_LEG:   p = "On legs"; break;
		case INVEN_HEAD:  
		{
			if ((p_ptr->prace == RACE_AUTOMATA) || 
				(p_ptr->prace == RACE_STEAM_MECHA)) 
			{
				p = "Head"; break;
			}
			else p = "On head"; break;
		}
		case INVEN_HANDS: 
		{
			if ((p_ptr->prace == RACE_AUTOMATA) || 
				(p_ptr->prace == RACE_STEAM_MECHA))
			{
				 p = "Arms"; break;
			}
			else p = "On hands"; break;
		}
		case INVEN_FEET:  
		{
			if ((p_ptr->prace == RACE_AUTOMATA) || 
				(p_ptr->prace == RACE_STEAM_MECHA))
			{
				 p = "Legs"; break;
			}
			else p = "On feet"; break;
		}
		case INVEN_LOADEDGUN:
		{
			p = "in gun"; break;
		}
		default:          p = "In pack"; break;
	}

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		object_type *o_ptr;
		o_ptr = &inventory[i];
		/* @STAT@ */
		if ((p_ptr->stat_use[A_MUS] / 9 + 5) < o_ptr->weight / 10)
		{
			p = "Just lifting";
		}
	}

	/* Hack -- Heavy bow */
	if (i == INVEN_GUN)
	{
		object_type *o_ptr;
		o_ptr = &inventory[i];
		/* @STAT@ */
		if ((p_ptr->stat_use[A_MUS] / 9 + 5) < o_ptr->weight / 10)
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
		case INVEN_GUN:   p = "shooting bullets with"; break;
		case INVEN_LEFT:  p = "wearing on your left hand"; break;
		case INVEN_RIGHT: p = "wearing on your right hand"; break;
		case INVEN_NECK:  p = "wearing around your neck"; break;
		case INVEN_LITE:  p = "using to light the way"; break;
		case INVEN_BODY:  p = "wearing on your body"; break;
		case INVEN_OUTER: p = "wearing on your back"; break;
		case INVEN_LEG:   p = "wearing on your legs"; break;
		case INVEN_HEAD:  p = "wearing on your head"; break;
		case INVEN_HANDS: p = "wearing on your hands"; break;
		case INVEN_FEET:  p = "wearing on your feet"; break;
		case INVEN_LOADEDGUN: p = "loaded in your gun"; break;
		default:          p = "carrying in your pack"; break;
	}

	/* Hack -- Heavy weapon */
	if (i == INVEN_WIELD)
	{
		object_type *o_ptr;
		o_ptr = &inventory[i];
		/* @STAT@ */
		if ((p_ptr->stat_use[A_MUS] / 9 + 5) < o_ptr->weight / 10)
		{
			p = "just lifting";
		}
	}

	/* Hack -- Heavy gun */
	if (i == INVEN_GUN)
	{
		object_type *o_ptr;
		o_ptr = &inventory[i];
		/* @STAT@ */
		if ((p_ptr->stat_use[A_MUS] / 9 + 5) < o_ptr->weight / 10)
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
		if (o_ptr->tval != TV_MAGIC_BOOK) attr = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) attr = k_info[o_ptr->k_idx].d_attr;
			else attr = TERM_L_DARK;
		}

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

		/* Hack -- never show empty quiver slots. */
		if ((!o_ptr->k_idx) && (i == INVEN_LOADEDGUN))
		{
			/* Clear the line, skip to next slot */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
		}

		/* Hack -- never show the "blank" slot. */
		if (i == INVEN_BLANK)
		{
			/* Clear the line, skip to next slot */
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
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Obtain the length of the description */
		n = strlen(o_name);

		/* Get inventory color */
		if (o_ptr->tval != TV_MAGIC_BOOK) attr = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) attr = k_info[o_ptr->k_idx].d_attr;
			else attr = TERM_L_DARK;
		}

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
		if (o_ptr->tval != TV_MAGIC_BOOK) out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) out_color[k] = k_info[o_ptr->k_idx].d_attr;
			else out_color[k] = TERM_L_DARK;
		}

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

		/* Hack -- never show empty quiver slots. */
		if ((!o_ptr->k_idx) && (i == INVEN_LOADEDGUN))
		{
			/* Clear the line, skip to next slot */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
		}
		/* Hack -- never show the "blank" slot. */
		if (i == INVEN_BLANK)
		{
			/* Clear the line, skip to next slot */
			Term_erase(0, i - INVEN_WIELD, 255);
			continue;
		}

		/* Is this item acceptable? */
		if (!item_tester_okay(o_ptr)) continue;

		/* Description */
		object_desc(o_name, o_ptr, TRUE, 3);

		/* Truncate the description */
		o_name[lim] = 0;

		/* Save the index */
		out_index[k] = i;

		/* Get inventory color */
		if (o_ptr->tval != TV_MAGIC_BOOK) out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) out_color[k] = k_info[o_ptr->k_idx].d_attr;
			else out_color[k] = TERM_L_DARK;
		}

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
		if (o_ptr->tval != TV_MAGIC_BOOK) out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];
		else
		{
			if (cp_ptr->spell_book[o_ptr->sval]) out_color[k] = k_info[o_ptr->k_idx].d_attr;
			else out_color[k] = TERM_L_DARK;
		}

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
		/* I'm removing the command_see check and I'm sure it will break something */
		if (/* p_ptr->command_see && */
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


/*
 * Extract and return extended information about an object, including
 * (sometimes) damage information for magical items, and (if appropriate)
 * ego and artifact lore.
 *
 * Code mostly from object_desc and roff_aux.
 */
void object_info(char *buf, object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	char *t;
	cptr s;
	cptr v;
	cptr w;
	cptr x;

	/* Assume no flavor string, no ego info, and no base info. */
	cptr modstr = "";
	cptr egoinfo = "";
	char baseinfo[1024];


	/* Artifacts have unique descriptions. */
	if ((artifact_p(o_ptr)) && (object_known_p(o_ptr)))
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* If already in memory, simple to access */
		my_strcpy(buf, a_text + a_ptr->text, 2048);

		/* Return the description, if any. */
		return;
	}

	/* All non-artifact or random artifact objects. */
	else
	{

		/* If already in memory, simple to access */
		strcpy(buf, k_text + k_ptr->text);

		/* No object description, so return failure. */
		if (!buf[0]) return;


		/* Various object types have different kinds of information. */
		switch (o_ptr->tval)
		{
			/* Dragon Scale Mails */
			case TV_DRAG_ARMOR:
			{
				/* Allow processing of activation information. */
				strcpy(baseinfo, format("%s", buf));
				break;
			}

			/* Amulets */
			case TV_AMULET:
			{
				strcpy(baseinfo, format("An amulet %s", buf));
				break;
			}

			/* Rings */
			case TV_RING:
			{
				strcpy(baseinfo, format("A ring %s", buf));
				break;
			}

			/* Staffs */
			case TV_TOOL:
			{
				strcpy(baseinfo, format("A tool %s", buf));
				break;
			}

			/* Wands */
			case TV_RAY:
			{
				strcpy(baseinfo, format("A ray gun %s", buf));
				break;
			}

			/* Rods */
			case TV_APPARATUS:
			{
				strcpy(baseinfo, format("An apparatus %s", buf));
				break;
			}

			/* Scrolls */
			case TV_MECHANISM:
			{
				strcpy(baseinfo, format("A mechanism %s", buf));
				break;
			}

			/* Potions */
			case TV_TONIC:
			{
				strcpy(baseinfo, format("A tonic %s", buf));
				break;
			}

			/* All other objects can just display the info text. */
			default:
			{
				/* Store the basic info text. */
				strcpy(baseinfo, format("%s", buf));
			}
		}


		/* Ego-object descriptions are added to any base description. */
		if ((o_ptr->name2) && (object_known_p(o_ptr)))
		{
			ego_item_type *e_ptr = &e_info[o_ptr->name2];
			char ebuf[1024];

			/* First, find the information in memory, or get it from
			 * the binary file.
			 */

			/* If already in memory, simple to access */
			strcpy(ebuf, e_text + e_ptr->text);

			/* Point to the ego-item information. */
			egoinfo = ebuf;
		}


		/* Point to "buf", and start dumping the result */
		t = buf;


		/*** Assemble the object information. ***/

		/* The object needs an article */
		if (baseinfo[0] == '&')
		{
			/* Skip ampersand and space. */
			s = baseinfo + 2;

			/* Flavor starts with a vowel */
			if (is_a_vowel(modstr[0])) w = "An ";

			/* Flavor starts with a non-vowel */
			else w = "A ";
		}
		else
		{
			w = "";

			/* Start at beginning of base info. */
			s = baseinfo;
		}

		/* Copy the base description, inserting info text. */
		for (; *s; s++)
		{
			/* Insert article */
			if (s != baseinfo)
			{
				for (; *w; w++) *t++ = *w;
			}

			/* Insert numerical info before closing period. */
			if ((*s == '.') && (*(s + 1) == '\0'))
			{
				/* Extra info if object is fully known. */
				if (o_ptr->ident & (IDENT_MENTAL)) /* || k_ptr->special & (SPECIAL_KNOWN_EFFECT)) */
				{
					cptr moddata = "";
					bool dummy;

					/* Item is a magical device */
					if ((o_ptr->tval == TV_TOOL) ||
					    (o_ptr->tval == TV_RAY) ||
					    (o_ptr->tval == TV_APPARATUS))
					{
						moddata = do_device(OBJECT_INFO, o_ptr,
							&dummy, &dummy, FALSE);
					}

					/* Item is some food or a mushroom, a potion, or a scroll */
					else if ((o_ptr->tval == TV_FOOD) ||
					         (o_ptr->tval == TV_TONIC) ||
					         (o_ptr->tval == TV_MECHANISM))
					{
						moddata = do_object(OBJECT_INFO, o_ptr);
					}

					/* If there is any numerical data,  */
					if (strlen(moddata) > 0)
					{
						/* ...insert a space, and */
						*t++ = ' ';

						/* insert the numerical data into the string. */
						for (v = moddata; *v; v++) *t++ = *v;
					}
				}
				/* Otherwise, no extra information. */
			}
			/* Copy over the string. */
			*t++ = *s;
		}

		/* Extra info for ego items. */
		if ((o_ptr->name2) && (object_known_p(o_ptr)))
		{
			cptr divider = "                                   ---";

			/* Insert a return, a divider, and another return. */
			*t++ = '\n';
			for (x = divider; *x; x++) *t++ = *x;
			*t++ = '\n';

			/* Copy the ego info to the information string. */
			for (x = egoinfo; *x; x++) *t++ = *x;
		}

		/* End the string. */
		*t = '\0';

		/* Return the string. */
		return;
	}
}

/*
 * Descriptions of pval-dependent qualities.
 */
static cptr pval_desc_text[32] =
{
	"muscle",
	"agility",
	"vigor",
	"schooling",
	"ego",
	"charm",
	"XXX6",
	"XXX7",
	"stealth",
	"searching",
	"infravision",
	"tunneling",
	"speed",
	"mana",
	"health",
	"light radius",
	"saving throw",
	"magic bonus",
	"XXX18",
	"XXX19",
	"melee blows",
	"shooting speed",
	"missile weapon power",
	"XX23",
	"XX24",
	"XX25",
	"XX26",
	"XX27",
	"XX28",
	"XX29",
	"XX30",
	"XX31"
};

/*
 * Display most of what is known about any object.  Rewritten to
 * use "roff" and to harmonize with other description code.
 *
 * Fully known objects display all information, known objects display
 * everything except the non pval-dependant and random flags, and others
 * display only basic help text.
 */
void object_details(object_type *o_ptr, bool mental, bool known)
{
	int i, j, k, tmp1, tmp2;
	int y, x;

	int pval[32][2];

	u32b f1, f2, f3;

	int attr_listed = 0;
	int attr_num = 0;

	int num_fire = 0;

	/* Object is not known -- jump straight to the basic information */
	if (!known) goto basic_info;



	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Describe activation, if present. */
	if (f3 & (TR3_ACTIVATE))
	{
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		if (mental) /*(|| (k_ptr->special |= (SPECIAL_KNOWN_EFFECT))) */
		{
			cptr buf = do_activation_aux(OBJECT_INFO, o_ptr);

			c_roff(TERM_BLUE, "Activation:  ");
			c_roff(TERM_BLUE, format("%s\n\n", buf));
		}
	}

	/* Describe light sources */
	if (get_object_pval(o_ptr, TR_PVAL_LIGHT) > 0)
	{
		int radius = get_object_pval(o_ptr, TR_PVAL_LIGHT);

		if ((o_ptr->tval != TV_LITE) || (f3 & (TR3_NO_FUEL)))
		{
			c_roff(TERM_YELLOW, format("It provides light (radius %d) forever.\n",
				radius));
		}
		else
		{
			c_roff(TERM_YELLOW, format("It provides light (radius %d) when fueled.\n",
				radius));
		}
	}

	/* display gun ranges -- hardcoded from cmd-attk.c line ~1800 */
	/* From do_cmd_fire */
	if (o_ptr->tval == TV_GUN)
	{
		num_fire = o_ptr->num_fire; 
		num_fire += get_object_pval(o_ptr, TR_PVAL_SHOTS);

		if (o_ptr->ammo_tval == TV_AMMO)
 			c_roff(TERM_UMBER, format("This pistol has an accurate range of %d squares and holds %d bullets.\n", o_ptr->range, num_fire));
		if (o_ptr->ammo_tval == TV_BULLET)
 			c_roff(TERM_UMBER, format("This rifle has an accurate range of %d squares and holds %d bullets.\n", o_ptr->range, num_fire));
		if (o_ptr->ammo_tval == TV_SHOT)
			c_roff(TERM_UMBER, format("This shotgun has an accurate range of %d squares and holds %d shells.\n", o_ptr->range, num_fire));
	}
	
	/* Magical devices can be damaged  XXX XXX */
	if (((o_ptr->tval == TV_TOOL) || (o_ptr->tval == TV_RAY)) &&
		  (o_ptr->ac < k_info[o_ptr->k_idx].ac))
	{
		roff("This object is damaged.\n");
	}


	/* Object has a pval of some kind */
	if (get_object_pval(o_ptr, 0L))
	{
		/* Get the values of a lot of pval-dependant qualities */
		pval[0][0]  = get_object_pval(o_ptr, TR_PVAL_MUS);
		pval[1][0]  = get_object_pval(o_ptr, TR_PVAL_AGI);
		pval[2][0]  = get_object_pval(o_ptr, TR_PVAL_VIG);
		pval[3][0]  = get_object_pval(o_ptr, TR_PVAL_SCH);
		pval[4][0]  = get_object_pval(o_ptr, TR_PVAL_EGO);
		pval[5][0]  = get_object_pval(o_ptr, TR_PVAL_CHR);
		pval[6][0]  = 0;
		pval[7][0]  = 0;

		pval[8][0]  = get_object_pval(o_ptr, TR_PVAL_STEALTH);
		pval[9][0]  = get_object_pval(o_ptr, TR_PVAL_SEARCH);
		pval[10][0] = get_object_pval(o_ptr, TR_PVAL_INFRA);
		pval[11][0] = get_object_pval(o_ptr, TR_PVAL_TUNNEL);
		pval[12][0] = get_object_pval(o_ptr, TR_PVAL_SPEED);
		pval[13][0] = get_object_pval(o_ptr, TR_PVAL_MANA);

		/* Show exact values for new and unfamiliar pvals  XXX XXX */
		pval[14][0] = get_object_pval(o_ptr, TR_PVAL_HEALTH);
		pval[15][0] = 0; /* Do not show light radius here */
		pval[16][0] = get_object_pval(o_ptr, TR_PVAL_SAVE);
		pval[17][0] = get_object_pval(o_ptr, TR_PVAL_MAGIC_MASTER);
		pval[18][0] = 0; 
		pval[19][0] = 0;

		pval[20][0] = get_object_pval(o_ptr, TR_PVAL_BLOWS);
		pval[21][0] = get_object_pval(o_ptr, TR_PVAL_SHOTS);
		pval[22][0] = get_object_pval(o_ptr, TR_PVAL_MIGHT);
		pval[23][0] = 0;
		pval[24][0] = 0;
		pval[25][0] = 0;
		pval[26][0] = 0;
		pval[27][0] = 0;
		pval[28][0] = 0;
		pval[29][0] = 0;
		pval[30][0] = 0;
		pval[31][0] = 0;

		/* Special case:  all stats */
		if ((pval[0][0] != 0) &&
		    (pval[1][0] == pval[0][0]) && (pval[2][0] == pval[0][0]) &&
		    (pval[3][0] == pval[0][0]) && (pval[4][0] == pval[0][0]) &&
		    (pval[5][0] == pval[0][0]))
		{
			cptr desc = "increases";
			if (pval[0][0] < 0) desc = "decreases";

			roff(format("It %s all your stats by %d.\n", desc,
				ABS(pval[0][0])));

			/* Hack -- stats have been displayed */
			pval[0][0] = pval[1][0] = pval[2][0] = pval[3][0] =
			             pval[4][0] = pval[5][0] = 0;
		}

		/* Save the original indexes */
		for (i = 0; i < 32; i++)
		{
			pval[i][1] = i;
		}

		/* Sort all the pvals by value */
		for (i = 0; i < 32 - 1; i++)
		{
			for (j = 0; j < 32 - 1; j++)
			{
				/* Bubble sort */
				if (pval[j][0] < pval[j + 1][0])
				{
					tmp1 = pval[j][0];
					tmp2 = pval[j][1];
					pval[j][0] = pval[j + 1][0];
					pval[j][1] = pval[j + 1][1];
					pval[j + 1][0] = tmp1;
					pval[j + 1][1] = tmp2;
				}
			}
		}

		/* List all the pvals by value */
		for (k = 0; k < 32;)
		{
			/* Get pval */
			tmp1 = pval[k][0];

			/* Skip pvals of 0 */
			if (!tmp1)
			{
				k++;
				continue;
			}

			/* Figure out how many items use this pval */
			for (j = k + 1, attr_num = 1; j < 32; j++, attr_num++)
			{
				if (pval[j][0] != tmp1) break;
			}


			/* Start the description */
			if (tmp1 > 0) roff("It increases your");
			else          roff("It decreases your");

			/* List all the items with this pval */
			for (j = k, attr_listed = 0; (j < 32) && (attr_listed < attr_num);
			     j++)
			{
				/* Listing another attribute */
				attr_listed++;

				/* Commas separate members of a list of more than two. */
				if ((attr_num > 2) && (attr_listed > 1)) roff(",");

				/* "and" before final member of a list of more than one. */
				if ((attr_num > 1) && (j != k))
				{
					if (attr_num == attr_listed) roff(" and");
				}

				/* List -- use original flag index */
				roff(format(" %s", pval_desc_text[pval[j][1]]));
			}

			/* End this pval's description */
			roff(format(" by %d.\n", ABS(tmp1)));

			/* Advance to next set of pvals */
			k = j;
		}
	}

	/* Require full identification for most info */
	if (mental)
	{
		/* Sustain stats. */
		if ((f1 & (TR1_SUST_MUS)) || (f1 & (TR1_SUST_AGI)) ||
			 (f1 & (TR1_SUST_VIG)) || (f1 & (TR1_SUST_SCH)) ||
			 (f1 & (TR1_SUST_EGO)) || (f1 & (TR1_SUST_CHR)))
		{
			/* Clear number of items to list, and items listed. */
			attr_num = 0;
			attr_listed = 0;

			/* How many attributes need to be listed? */
			if (f1 & (TR1_SUST_MUS)) attr_num++;
			if (f1 & (TR1_SUST_AGI)) attr_num++;
			if (f1 & (TR1_SUST_VIG)) attr_num++;
			if (f1 & (TR1_SUST_SCH)) attr_num++;
			if (f1 & (TR1_SUST_EGO)) attr_num++;
			if (f1 & (TR1_SUST_CHR)) attr_num++;

			/* Special case:  sustain all stats */
			if (attr_num == 6)
			{
				roff("It sustains all your stats");
			}
			else
			{
				roff("It sustains your");

				/* Loop for number of attributes in this group. */
				for (j = 0; j < 6; j++)
				{
					bool list_ok = FALSE;

					if ((j == 0) && (f1 & (TR1_SUST_MUS))) list_ok = TRUE;
					if ((j == 1) && (f1 & (TR1_SUST_AGI))) list_ok = TRUE;
					if ((j == 2) && (f1 & (TR1_SUST_VIG))) list_ok = TRUE;
					if ((j == 3) && (f1 & (TR1_SUST_SCH))) list_ok = TRUE;
					if ((j == 4) && (f1 & (TR1_SUST_EGO))) list_ok = TRUE;
					if ((j == 5) && (f1 & (TR1_SUST_CHR))) list_ok = TRUE;

					if (!list_ok) continue;

					/* Listing another attribute. */
					attr_listed++;

					/* Commas separate members of a list of more than two. */
					if ((attr_num > 2) && (attr_listed > 1)) roff(",");

					/* "and" before final member of a list of more than one. */
					if ((attr_num > 1) && (j != 0))
					{
						if (attr_num == attr_listed) roff(" and");
					}

					/* List the attribute description, in its proper place. */
					if (j == 0) roff(" muscle");
					if (j == 1) roff(" agility");
					if (j == 2) roff(" vigor");
					if (j == 3) roff(" schooling");
					if (j == 4) roff(" ego");
					if (j == 5) roff(" charm");
				}
			}

			/* End sentence.  Go to next line. */
			roff(". \n");
		}
	}
	if (known)
	{
		/* Slays. */
		if ((f1 & (TR1_SLAY_ANIMAL)) || (f1 & (TR1_SLAY_EVIL)) ||
			 (f1 & (TR1_SLAY_UNDEAD)) || (f1 & (TR1_SLAY_DEMON)) ||
			 (f1 & (TR1_SLAY_AUTOMATA))    || (f1 & (TR1_SLAY_DINOSAUR)) ||
			 (f1 & (TR1_SLAY_CONSTRUCT))  || (f1 & (TR1_SLAY_ELEMENTAL)) ||
			 (f1 & (TR1_SLAY_ALIEN))  || (f1 & (TR1_SLAY_BEASTMAN)) ||
			 (f1 & (TR1_SLAY_CARDS)))
		{
			/* Clear number of items to list, and items listed. */
			attr_num = 0;
			attr_listed = 0;

			/* How many normal slays need to be listed? */
			if (f1 & (TR1_SLAY_ANIMAL)) attr_num++;
			if (f1 & (TR1_SLAY_EVIL))   attr_num++;
			if (f1 & (TR1_SLAY_UNDEAD)) attr_num++;
			if (f1 & (TR1_SLAY_DEMON))  attr_num++;
			if (f1 & (TR1_SLAY_AUTOMATA))    attr_num++;
			if (f1 & (TR1_SLAY_DINOSAUR))  attr_num++;
			if (f1 & (TR1_SLAY_CONSTRUCT))  attr_num++;
			if (f1 & (TR1_SLAY_ELEMENTAL)) attr_num++;
			if (f1 & (TR1_SLAY_ALIEN)) attr_num++;
			if (f1 & (TR1_SLAY_BEASTMAN)) attr_num++;
			if (f1 & (TR1_SLAY_CARDS)) attr_num++;

			/* Start the sentence */
			if (attr_num) roff("It slays");

			/* Loop for number of attributes in this group. */
			for (j = 0; j < 11; j++)
			{
				bool list_ok = FALSE;

				if ((j == 0) && (f1 & (TR1_SLAY_ANIMAL))) list_ok = TRUE;
				if ((j == 1) && (f1 & (TR1_SLAY_EVIL))) list_ok = TRUE;
				if ((j == 2) && (f1 & (TR1_SLAY_UNDEAD))) list_ok = TRUE;
				if ((j == 3) && (f1 & (TR1_SLAY_DEMON))) list_ok = TRUE;
				if ((j == 4) && (f1 & (TR1_SLAY_AUTOMATA))) list_ok = TRUE;
				if ((j == 5) && (f1 & (TR1_SLAY_DINOSAUR))) list_ok = TRUE;
				if ((j == 6) && (f1 & (TR1_SLAY_CONSTRUCT))) list_ok = TRUE;
				if ((j == 7) && (f1 & (TR1_SLAY_ELEMENTAL))) list_ok = TRUE;
				if ((j == 8) && (f1 & (TR1_SLAY_ALIEN))) list_ok = TRUE;
				if ((j == 9) && (f1 & (TR1_SLAY_BEASTMAN))) list_ok = TRUE;
				if ((j == 10) && (f1 & (TR1_SLAY_CARDS))) list_ok = TRUE;

				if (!list_ok) continue;

				/* Listing another attribute. */
				attr_listed++;

				/* Commas separate members of a list of more than two. */
				if ((attr_num > 2) && (attr_listed > 1)) roff(",");

				/* "and" before final member of a list of more than one. */
				if ((attr_num > 1) && (j != 0))
				{
					if (attr_num == attr_listed) roff(" and");
				}

				/* List the attribute description, in its proper place. */
				if (j == 0) roff(" animals");
				if (j == 1) roff(" evil");
				if (j == 2) roff(" undead");
				if (j == 3) roff(" demons");
				if (j == 4) roff(" automata");
				if (j == 5) roff(" dinosaurs");
				if (j == 6) roff(" constructs");
				if (j == 7) roff(" elementals");
				if (j == 8) roff(" aliens");
				if (j == 9) roff(" beastmen");
				if (j == 10) roff(" cards");
			}

			/* End sentence.  Go to next line. */
			roff(". \n");
		}


		/* Elemental and poison brands. */
		if ((f1 & (TR1_BRAND_FIRE)) || (f1 & (TR1_BRAND_ELEC)) ||
			 (f1 & (TR1_BRAND_ICE)) || (f1 & (TR1_BRAND_ACID)) ||
			 (f1 & (TR1_BRAND_POISON)) || (f1 & (TR1_BRAND_FORCE)))
		{
			/* Clear number of items to list, and items listed. */
			attr_num = 0;
			attr_listed = 0;

			/* How many normal brands need to be listed? */
			if (f1 & (TR1_BRAND_FIRE)) attr_num++;
			if (f1 & (TR1_BRAND_ELEC)) attr_num++;
			if (f1 & (TR1_BRAND_ICE)) attr_num++;
			if (f1 & (TR1_BRAND_ACID)) attr_num++;
			if (f1 & (TR1_BRAND_POISON)) attr_num++;
			if (f1 & (TR1_BRAND_FORCE)) attr_num++;

			/* Start the sentence */
			if (attr_num) roff("It");

			/* Loop for number of attributes in this group. */
			for (j = 0; j < 6; j++)
			{
				bool list_ok = FALSE;

				if ((j == 0) && (f1 & (TR1_BRAND_FIRE))) list_ok = TRUE;
				if ((j == 1) && (f1 & (TR1_BRAND_ELEC))) list_ok = TRUE;
				if ((j == 2) && (f1 & (TR1_BRAND_ICE))) list_ok = TRUE;
				if ((j == 3) && (f1 & (TR1_BRAND_ACID))) list_ok = TRUE;
				if ((j == 4) && (f1 & (TR1_BRAND_POISON))) list_ok = TRUE;
				if ((j == 5) && (f1 & (TR1_BRAND_FORCE))) list_ok = TRUE;

				if (!list_ok) continue;

				/* Listing another attribute. */
				attr_listed++;

				/* Commas separate members of a list of more than two. */
				if ((attr_num > 2) && (attr_listed > 1)) roff(",");

				/* "and" before final member of a list of more than one. */
				if ((attr_num > 1) && (j != 0))
				{
					if (attr_num == attr_listed) roff(" and");
				}

				/* List the attribute description, in its proper place. */
				if (j == 0) roff(" burns");
				if (j == 1) roff(" electrocutes");
				if (j == 2) roff(" freezes");
				if (j == 3) roff(" melts");
				if (j == 4) roff(" poisons");
				if (j == 5) roff(" powerfully strikes");
			}

			/* End the sentence */
			if (attr_num) roff(" your foes");

			/* End sentence.  Go to next line. */
			roff(". \n");
		}

		/* Vorpal weapons and missile launchers. */
#if 0	/* Guns are not vorpal */
		if (f1 & (TR1_VORPAL))
		{
			if (o_ptr->tval == TV_BOW)
			{
				roff("The missiles this weapon shoots drive deeply into their targets. \n");
			}
			else if (o_ptr->tval == TV_HAFTED)
			{
				roff("It is a weapon of concussion. \n");
			}
			else if (!is_missile(o_ptr))
			{
				roff("It is a vorpal blade. \n");
			}
			else
			{
				roff("It drives deeply into your foes. \n");
			}
		}
#endif
		/* Throwing weapons. */
		if (f2 & (TR2_THROW))
		{
			if (f2 & (TR2_PERFECT_BALANCE))
			{
				roff("It can be thrown hard and fast. \n");
			}
			roff("It can be thrown effectively. \n");
		}

		/* Resistances. */
		if ((object_resist(o_ptr, RS_FIR) != 0) || (object_resist(o_ptr, RS_EAR) != 0) ||
			(object_resist(o_ptr, RS_AIR) != 0) || (object_resist(o_ptr, RS_WTR) != 0) ||
			(object_resist(o_ptr, RS_ELC) != 0) || (object_resist(o_ptr, RS_ICE) != 0) ||
			(object_resist(o_ptr, RS_ACD) != 0) || (object_resist(o_ptr, RS_PSN) != 0) ||
			(object_resist(o_ptr, RS_TIM) != 0) || (object_resist(o_ptr, RS_ETH) != 0) ||
			(object_resist(o_ptr, RS_SND) != 0) || (object_resist(o_ptr, RS_NTH) != 0) ||
			(object_resist(o_ptr, RS_LIT) != 0) || (object_resist(o_ptr, RS_DRK) != 0) ||
			(object_resist(o_ptr, RS_PSI) != 0) || (object_resist(o_ptr, RS_TLK) != 0) ||
			(object_resist(o_ptr, RS_SPI) != 0))
		{
			/* Clear number of items to list, and items listed. */
			attr_num = 0;
			attr_listed = 0;

			/* How many attributes need to be listed? */
			if (object_resist(o_ptr, RS_FIR) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_EAR) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_AIR) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_WTR) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_ELC) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_ICE) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_ACD) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_PSN) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_TIM) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_ETH) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_SND) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_NTH) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_LIT) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_DRK) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_PSI) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_TLK) != 0)  attr_num++;
			if (object_resist(o_ptr, RS_SPI) != 0)  attr_num++;

			roff("It provides"); /* resistance to"); */

			/* Loop for number of attributes in this group. */
			for (j = 0; j < RS_MAX; j++)
			{
				bool list_ok = FALSE;

				if ((j ==  0) && (object_resist(o_ptr, RS_FIR) != 0)) list_ok = TRUE;
				if ((j ==  1) && (object_resist(o_ptr, RS_EAR) != 0)) list_ok = TRUE;
				if ((j ==  2) && (object_resist(o_ptr, RS_AIR) != 0)) list_ok = TRUE;
				if ((j ==  3) && (object_resist(o_ptr, RS_WTR) != 0)) list_ok = TRUE;
				if ((j ==  4) && (object_resist(o_ptr, RS_ELC) != 0)) list_ok = TRUE;
				if ((j ==  5) && (object_resist(o_ptr, RS_ICE) != 0)) list_ok = TRUE;
				if ((j ==  6) && (object_resist(o_ptr, RS_ACD) != 0)) list_ok = TRUE;
				if ((j ==  7) && (object_resist(o_ptr, RS_PSN) != 0)) list_ok = TRUE;
				if ((j ==  8) && (object_resist(o_ptr, RS_TIM) != 0)) list_ok = TRUE;
				if ((j ==  9) && (object_resist(o_ptr, RS_ETH) != 0)) list_ok = TRUE;
				if ((j == 10) && (object_resist(o_ptr, RS_SND) != 0)) list_ok = TRUE;
				if ((j == 11) && (object_resist(o_ptr, RS_NTH) != 0)) list_ok = TRUE;
				if ((j == 12) && (object_resist(o_ptr, RS_LIT) != 0)) list_ok = TRUE;
				if ((j == 13) && (object_resist(o_ptr, RS_DRK) != 0)) list_ok = TRUE;
				if ((j == 14) && (object_resist(o_ptr, RS_PSI) != 0)) list_ok = TRUE;
				if ((j == 15) && (object_resist(o_ptr, RS_TLK) != 0)) list_ok = TRUE;
				if ((j == 16) && (object_resist(o_ptr, RS_SPI) != 0)) list_ok = TRUE;

				if (!list_ok) continue;

				/* Listing another attribute. */
				attr_listed++;

				/* Commas separate members of a list of more than two. */
				if ((attr_num > 2) && (attr_listed > 1)) roff(",");

				/* "and" before final member of a list of more than one. */
				if ((attr_num > 1) && (j != 0))
				{
					if (attr_num == attr_listed) roff(" and");
				}
								
				/* List the attribute description, in its proper place. */
				if (j ==  0) roff(format(" %d%% fire", (object_resist(o_ptr, RS_FIR))));
				if (j ==  1) roff(format(" %d%% earth/shards", (object_resist(o_ptr, RS_EAR))));
				if (j ==  2) roff(format(" %d%% air", (object_resist(o_ptr, RS_AIR))));
				if (j ==  3) roff(format(" %d%% water", (object_resist(o_ptr, RS_WTR))));
				if (j ==  4) roff(format(" %d%% electricity", (object_resist(o_ptr, RS_ELC))));
				if (j ==  5) roff(format(" %d%% ice", (object_resist(o_ptr, RS_ICE))));
				if (j ==  6) roff(format(" %d%% acid", (object_resist(o_ptr, RS_ACD))));
				if (j ==  7) roff(format(" %d%% poison", (object_resist(o_ptr, RS_PSN))));
				if (j ==  8) roff(format(" %d%% time", (object_resist(o_ptr, RS_TIM))));
				if (j ==  9) roff(format(" %d%% ether", (object_resist(o_ptr, RS_ETH))));
				if (j == 10) roff(format(" %d%% sound", (object_resist(o_ptr, RS_SND))));
				if (j == 11) roff(format(" %d%% nether", (object_resist(o_ptr, RS_NTH))));
				if (j == 12) roff(format(" %d%% light", (object_resist(o_ptr, RS_LIT))));
				if (j == 13) roff(format(" %d%% dark", (object_resist(o_ptr, RS_DRK))));
				if (j == 14) roff(format(" %d%% psionic", (object_resist(o_ptr, RS_PSI))));
				if (j == 15) roff(format(" %d%% forces", (object_resist(o_ptr, RS_TLK))));
				if (j == 16) roff(format(" %d%% spirit", (object_resist(o_ptr, RS_SPI))));
			}

			/* End sentence.  Go to next line. */
			roff(" resistance. \n");
		}

		/* Clear a listing variable. */
		attr_num = 0;

		/* Special processing for the three "survival resists" */
		if (f2 & (TR2_RES_FEAR)) attr_num++;
		if (f2 & (TR2_RES_BLIND)) attr_num++;
		if (f2 & (TR2_RES_CONFU)) attr_num++;

		if (f2 & (TR2_RES_FEAR))
		{
			roff("It renders you fearless");
			if (attr_num == 1) roff(". \n");
			else roff(", and");
		}

		if (f2 & (TR2_RES_BLIND))
		{
			if ((attr_num > 1) && (f2 & (TR2_RES_FEAR)))
				roff(" provides resistance to blindness");
			else roff("It provides resistance to blindness");

			if (f2 & (TR2_RES_CONFU)) roff(" and");
			else roff(". \n");
		}

		if (f2 & (TR2_RES_CONFU))
		{
			if ((attr_num > 1) && (!(f2 & (TR2_RES_BLIND))))
				roff(" provides resistance to confusion.\n");
			else if (attr_num > 1) roff(" confusion.\n");
			else roff("It provides resistance to confusion.\n");
		}


		/* Miscellaneous abilities. */
		if (
			 (f2 & (TR2_IMPACT))      || (f2 & (TR2_CRITICAL)) ||
			 (f2 & (TR2_VAMPIRIC))    || (f2 & (TR2_VORPAL)) ||
			 (f2 & (TR2_RETURN))      || (f2 & (TR2_EDGED)) ||
			 (f2 & (TR2_BLUNT))       || (f2 & (TR2_PIERCE)) ||
			 (f2 & (TR2_SLOW_DIGEST)) || (f2 & (TR2_FEATHER)) ||
			 (f2 & (TR2_TELEPATHY))   || (f2 & (TR2_SEE_INVIS)) ||
			 (f2 & (TR2_FREE_ACT))    || (f2 & (TR2_HOLD_LIFE)) ||
			 (f2 & (TR2_WRAITH))      || (f2 & (TR2_REFLECT)) ||
			 (f2 & (TR2_BLESSED))     || (f2 & (TR2_REGEN_25)) ||
			 (f2 & (TR2_REGEN_50))    || (f2 & (TR2_REGEN_75)) ||
			 (f3 & (TR3_SH_FIRE))     || (f3 & (TR3_SPINES)) ||
			 (f3 & (TR3_SH_ELEC))     || (f3 & (TR3_SH_ETHER)) ||
			 (f2 & (TR2_INVISIBLE)))
		{
			/* Clear number of items to list, and items listed. */
			attr_num = 0;
			attr_listed = 0;

			/* How many attributes need to be listed? */
			if (f2 & (TR2_IMPACT))      attr_num++;
			if (f2 & (TR2_CRITICAL))    attr_num++;
			if (f2 & (TR2_VAMPIRIC))    attr_num++;
			if (f2 & (TR2_VORPAL))      attr_num++;
			if (f2 & (TR2_RETURN))      attr_num++;
			if (f2 & (TR2_EDGED))       attr_num++;
			if (f2 & (TR2_BLUNT))       attr_num++;
			if (f2 & (TR2_PIERCE))      attr_num++;
			if (f2 & (TR2_SLOW_DIGEST)) attr_num++;
			if (f2 & (TR2_FEATHER))     attr_num++;
			if (f2 & (TR2_TELEPATHY))   attr_num++;
			if (f2 & (TR2_SEE_INVIS))   attr_num++;
			if (f2 & (TR2_FREE_ACT))    attr_num++;
			if (f2 & (TR2_HOLD_LIFE))   attr_num++;
			if (f2 & (TR2_WRAITH))      attr_num++;
			if (f2 & (TR2_REFLECT))     attr_num++;
			if (f2 & (TR2_INVISIBLE))     attr_num++;
			if (f2 & (TR2_BLESSED))     attr_num++;
			if (f2 & (TR2_REGEN_25))    attr_num++;
			if (f2 & (TR2_REGEN_50))    attr_num++;
			if (f2 & (TR2_REGEN_75))    attr_num++;
			if (f3 & (TR3_SH_FIRE))     attr_num++;
			if (f3 & (TR3_SH_ELEC))     attr_num++;
			if (f3 & (TR3_SPINES))      attr_num++;
			if (f3 & (TR3_SH_ETHER))    attr_num++;

			roff("It");

			/* Loop for number of attributes in this group. */
			for (j = 0; j < 25; j++)
			{
				bool list_ok = FALSE;

				if ((j == 0) && (f2 & (TR2_IMPACT)))      list_ok = TRUE;
				if ((j == 1) && (f2 & (TR2_CRITICAL)))    list_ok = TRUE;
				if ((j == 2) && (f2 & (TR2_VAMPIRIC)))    list_ok = TRUE;
				if ((j == 3) && (f2 & (TR2_VORPAL)))      list_ok = TRUE;
				if ((j == 4) && (f2 & (TR2_RETURN)))      list_ok = TRUE;
				if ((j == 5) && (f2 & (TR2_EDGED)))       list_ok = TRUE;
				if ((j == 6) && (f2 & (TR2_BLUNT)))       list_ok = TRUE;
				if ((j == 7) && (f2 & (TR2_PIERCE)))      list_ok = TRUE;
				if ((j == 8) && (f2 & (TR2_SLOW_DIGEST))) list_ok = TRUE;
				if ((j == 9) && (f2 & (TR2_FEATHER)))     list_ok = TRUE;
				if ((j == 10) && (f2 & (TR2_TELEPATHY)))  list_ok = TRUE;
				if ((j == 11) && (f2 & (TR2_SEE_INVIS)))  list_ok = TRUE;
				if ((j == 12) && (f2 & (TR2_FREE_ACT)))   list_ok = TRUE;
				if ((j == 13) && (f2 & (TR2_HOLD_LIFE)))  list_ok = TRUE;
				if ((j == 14) && (f2 & (TR2_WRAITH)))     list_ok = TRUE;
				if ((j == 15) && (f2 & (TR2_REFLECT)))    list_ok = TRUE;
				if ((j == 16) && (f2 & (TR2_INVISIBLE)))    list_ok = TRUE;
				if ((j == 17) && (f2 & (TR2_BLESSED)))    list_ok = TRUE;
				if ((j == 18) && (f2 & (TR2_REGEN_25)))   list_ok = TRUE;
				if ((j == 19) && (f2 & (TR2_REGEN_50)))   list_ok = TRUE;
				if ((j == 20) && (f2 & (TR2_REGEN_75)))   list_ok = TRUE;
				if ((j == 21) && (f3 & (TR3_SH_FIRE)))    list_ok = TRUE;
				if ((j == 22) && (f3 & (TR3_SH_ELEC)))    list_ok = TRUE;
				if ((j == 23) && (f3 & (TR3_SPINES)))     list_ok = TRUE;
				if ((j == 24) && (f3 & (TR3_SH_ETHER)))   list_ok = TRUE;

				if (!list_ok) continue;

				/* Listing another attribute. */
				attr_listed++;

				/* Commas separate members of a list of more than two. */
				if ((attr_num > 2) && (attr_listed > 1)) roff(",");

				/* "and" before final member of a list of more than one. */
				if ((attr_num > 1) && (j != 0))
				{
					if (attr_num == attr_listed) roff(" and");
				}

				/* List the attribute description, in its proper place. */
				if (j == 0) roff(" is an impact weapon");
				if (j == 1) roff(" often causes criticals");
				if (j == 2) roff(" is a vampiric weapon");
				if (j == 3) roff(" is a vorpal weapon");
				if (j == 4) roff(" will return to you when thrown");
				if (j == 5) roff(" is an edged weapon");
				if (j == 6) roff(" is a blunt weapon");
				if (j == 7) roff(" is a piercing weapon");
				if (j == 8) roff(" slows your metabolism");
				if (j == 9) roff(" grants feather falling");
				if (j == 10) roff(" gives telepathic powers");
				if (j == 11) roff(" allows you to see invisible monsters");
				if (j == 12) roff(" provides immunity to paralysis");
				if (j == 13) roff(" provides resistance to life draining");
				if (j == 14) roff(" gives wraithform");
				if (j == 15) roff(" reflects projectiles");
				if (j == 16) roff(" renders you invisible");
				if (j == 17) roff(" has been blessed by the gods");
				if (j == 18) roff(" provides a 25% increase in regeneration");
				if (j == 19) roff(" provides a 50% increase in regeneration");
				if (j == 20) roff(" provides a 75% increase in regeneration");
				if (j == 21) roff(" provides a fire aura");
				if (j == 22) roff(" provides a electric aura");
				if (j == 23) roff(" provides a thick covering of spines");
				if (j == 24) roff(" provides a etheric aura");
			}

			/* End sentence.  Go to next line. */
			roff(". \n");
		}

		/* Nastiness. */
		if ((f3 & (TR3_MUTABLE)) || (f3 & (TR3_TELEPORT)) ||
			 (f3 & (TR3_EARTHQUAKE))  || (f3 & (TR3_NEVER_BLOW)) ||
			 (f3 & (TR3_NO_TELEPORT)) || (f3 & (TR3_NO_MAGIC)) ||
			 (f3 & (TR3_DRAIN_EXP)) || (f3 & (TR3_DRAIN_HP)) ||
			 (f3 & (TR3_DRAIN_SP)) || (f3 & (TR3_DRAIN_ITEM)) ||
			 (f3 & (TR3_AGGRAVATE)) || (f3 & (TR3_DISRUPT_SPELL)) ||
			 (cursed_p(o_ptr)))
		{
			/* Clear number of items to list, and items listed. */
			attr_num = 0;
			attr_listed = 0;

			/* How many attributes need to be listed? */
			if (f3 & (TR3_MUTABLE)) attr_num++;
			if (f3 & (TR3_TELEPORT))   attr_num++;
			if (f3 & (TR3_EARTHQUAKE))  attr_num++;
			if (f3 & (TR3_NEVER_BLOW)) attr_num++;
			if (f3 & (TR3_NO_TELEPORT)) attr_num++;
			if (f3 & (TR3_NO_MAGIC)) attr_num++;
			if (f3 & (TR3_DRAIN_EXP)) attr_num++;
			if (f3 & (TR3_DRAIN_HP)) attr_num++;
			if (f3 & (TR3_DRAIN_SP)) attr_num++;
			if (f3 & (TR3_DRAIN_ITEM)) attr_num++;
			if (f3 & (TR3_AGGRAVATE)) attr_num++;
			if (f3 & (TR3_DISRUPT_SPELL)) attr_num++;

			/* This one will display one of three possible descriptions. */
			if (cursed_p(o_ptr)) attr_num++;

			roff("It");

			/* Loop for number of attributes in this group. */
			for (j = 0; j < 13; j++)
			{
				bool list_ok = FALSE;

				if ((j == 0) && (f3 & (TR3_MUTABLE)))   list_ok = TRUE;
				if ((j == 1) && (f3 & (TR3_TELEPORT)))     list_ok = TRUE;
				if ((j == 2) && (f3 & (TR3_EARTHQUAKE)))    list_ok = TRUE;
				if ((j == 3) && (f3 & (TR3_NEVER_BLOW)))   list_ok = TRUE;
				if ((j == 4) && (f3 & (TR3_NO_TELEPORT)))   list_ok = TRUE;
				if ((j == 5) && (f3 & (TR3_NO_MAGIC)))    list_ok = TRUE;
				if ((j == 6) && (f3 & (TR3_DRAIN_EXP)))    list_ok = TRUE;
				if ((j == 7) && (f3 & (TR3_DRAIN_HP)))    list_ok = TRUE;
				if ((j == 8) && (f3 & (TR3_DRAIN_SP)))    list_ok = TRUE;
				if ((j == 9) && (f3 & (TR3_DRAIN_ITEM)))    list_ok = TRUE;
				if ((j == 10) && (f3 & (TR3_AGGRAVATE)))    list_ok = TRUE;
				if ((j == 11) && (f3 & (TR3_DISRUPT_SPELL)))    list_ok = TRUE;
				if ((j == 12) && (cursed_p(o_ptr)))        list_ok = TRUE;

				if (!list_ok) continue;

				/* Listing another attribute. */
				attr_listed++;

				/* Commas separate members of a list of more than two. */
				if ((attr_num > 2) && (attr_listed > 1)) roff(",");

				/* "and" before final member of a list of more than one. */
				if ((attr_num > 1) && (j != 0))
				{
					if (attr_num == attr_listed) roff(" and");
				}

				/* List the attribute description, in its proper place. */
				if (j == 0) roff(" causes mutations");
				if (j == 1) roff(" induces random teleportation");
				if (j == 2) roff(" causes earthquakes");
				if (j == 3) roff(" refuses to attack");
				if (j == 4) roff(" prevents teleportation");
				if (j == 5) roff(" prevents you from casting spells");
				if (j == 6) roff(" drains experience");
				if (j == 7) roff(" drains hitpoints");
				if (j == 8) roff(" drains spellpoints");
				if (j == 9) roff(" drains charges from items");
				if (j == 10) roff(" aggravates nearby creatures");
				if (j == 11) roff(" disrupts spellcasting");
				if (j == 12)
				{
					if (f3 & (TR3_PERMA_CURSE))
						roff(" can never be taken off once put on");
					else if (f3 & (TR3_HEAVY_CURSE))
						roff(" is powerfully cursed");
					else
						roff(" is cursed");
				}
			}

			/* End sentence.  Go to next line. */
			roff(". \n");
		}
		/* Ignore various elements. */
		if (f2 & (TR2_IMMUNE)) 
		{
			roff("It cannot be damaged or destroyed by the elements. \n");
		}

	}

	/* All objects should display certain basic help text */
	basic_info:


	/* Get cursor location */
	(void)Term_locate(&x, &y);

	/* We do not have enough screen space left  XXX XXX */
	if (y > Term->hgt - 4) return;


	/* Note durability */
	if ((o_ptr->ac > 0) &&
		 ((o_ptr->tval == TV_TONIC) ||
	     (o_ptr->tval == TV_MECHANISM) ||
	     (o_ptr->tval == TV_TEXT) ||
	     (o_ptr->tval == TV_AMMO) ||
	     (o_ptr->tval == TV_BULLET) ||
	     (o_ptr->tval == TV_SHOT) ||
	     (o_ptr->tval == TV_TOOL) ||
	     (o_ptr->tval == TV_RAY)))
	{
		roff("This item is unusually durable.\n");
	}

#if 0
	/* Explain skills and other "gotchas" */
#endif
}

