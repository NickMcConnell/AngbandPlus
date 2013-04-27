/* File: obj-desc.c */

/* Purpose: handle object descriptions, mostly string handling code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"


/*
 * XXX XXX Hack -- note that "COLOR_MULTI" is now just "COLOR_PURPLE"
 * We will have to find a cleaner method for "MULTI_HUED" later.
 * There were only two multi-hued "flavors" (one potion, one food).
 * Plus five multi-hued "base-objects" (3 dragon scales, one blade
 * of chaos, and one something else).  See the object color extractor
 * in "cave.c".
 */
const int COLOR_MULTI = COLOR_PURPLE;


/*
 * Max sizes of the following arrays
 */
const int MAX_ROCKS     = 42;       // Used with rings (min 38)
const int MAX_AMULETS   = 16;       // Used with amulets (min 13)
const int MAX_WOODS     = 32;       // Used with staffs (min 30)
const int MAX_METALS    = 32;       // Used with wands/rods (min 29/28)
const int MAX_COLORS    = 60;       // Used with potions (min 60)
const int MAX_SHROOM    = 20;       // Used with mushrooms (min 20)
const int MAX_TITLES    = 50;       // Used with scrolls (min 48)
const int MAX_SYLLABLES = 158;      // Used with scrolls (see below)


/*
 * Rings (adjectives and colors)
 */

static char *ring_adj[MAX_ROCKS] = {
    "alexandrite", "amethyst", "aquamarine", "azurite", "beryl",
    "bloodstone", "calcite", "carnelian","corundum", "diamond",
    "emerald", "fluorite", "garnet", "granite", "jade",
    "jasper", "lapis lazuli", "malachite", "marble", "moonstone",
    "onyx", "opal", "pearl", "quartz", "quartzite",
    "rhodonite", "ruby", "sapphire", "tiger eye", "topaz",
    "turquoise", "zircon", "platinum", "bronze", "gold",
    "obsidian", "silver", "tortoise shell", "mithril", "jet",
    "engagement", "adamantite"
};

static byte ring_col[MAX_ROCKS] = {
    COLOR_GREEN,COLOR_PURPLE,COLOR_LT_BLUE,COLOR_LT_BLUE,COLOR_LT_GREEN,
    COLOR_RED,COLOR_WHITE,COLOR_RED,COLOR_GREY,COLOR_WHITE,
    COLOR_GREEN,COLOR_LT_GREEN,COLOR_RED,COLOR_LT_GREY,COLOR_LT_GREEN,
    COLOR_BROWN,COLOR_BLUE,COLOR_GREEN,COLOR_WHITE,COLOR_LT_GREY,
    COLOR_LT_RED,COLOR_LT_GREY,COLOR_WHITE,COLOR_LT_GREY,COLOR_LT_GREY,
    COLOR_LT_RED,COLOR_RED,COLOR_BLUE,COLOR_YELLOW,COLOR_YELLOW,
    COLOR_LT_BLUE,COLOR_LT_BROWN,COLOR_WHITE,COLOR_LT_BROWN,COLOR_YELLOW,
    COLOR_DK_GREY,COLOR_LT_GREY,COLOR_BROWN,COLOR_LT_BLUE,COLOR_DK_GREY,
    COLOR_YELLOW,COLOR_LT_GREEN
};


/*
 * Amulets (adjectives and colors)
 */

static char *amulet_adj[MAX_AMULETS] = {
    "amber", "driftwood", "coral", "agate", "ivory",
    "obsidian", "bone", "brass", "bronze", "pewter",
    "tortoise shell", "golden", "azure", "crystal", "silver",
    "copper"
};

static byte amulet_col[MAX_AMULETS] = {
    COLOR_YELLOW,COLOR_LT_BROWN,COLOR_WHITE,COLOR_LT_GREY,COLOR_WHITE,
    COLOR_DK_GREY,COLOR_WHITE,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_GREY,
    COLOR_BROWN,COLOR_YELLOW,COLOR_LT_BLUE,COLOR_WHITE,COLOR_LT_GREY,
    COLOR_LT_BROWN
};


/*
 * Staffs (adjectives and colors)
 */

static char *staff_adj[MAX_WOODS] = {
    "aspen", "balsa", "banyan", "birch", "cedar",
    "cottonwood", "cypress", "dogwood", "elm", "eucalyptus",
    "hemlock", "hickory", "ironwood", "locust", "mahogany",
    "maple", "mulberry", "oak", "pine", "redwood",
    "rosewood", "spruce", "sycamore", "teak", "walnut",
    "mistletoe", "hawthorn", "bamboo", "silver", "runed",
    "golden", "ashen"/*,"gnarled","ivory","willow","brazen"*/
};

static byte staff_col[MAX_WOODS] = {
    COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,
    COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,
    COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_BROWN,COLOR_LT_BROWN,COLOR_BROWN,
    COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_RED,
    COLOR_RED,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_BROWN,
    COLOR_GREEN,COLOR_LT_BROWN,COLOR_LT_BROWN,COLOR_LT_GREY,COLOR_BROWN,
    COLOR_YELLOW,COLOR_GREY,/*???,???,???,???*/
};


/*
 * Wands (adjectives and colors)
 */

static char *wand_adj[MAX_METALS] = {
    "aluminum", "cast iron", "chromium", "copper", "gold",
    "iron", "magnesium", "molybdenum", "nickel", "rusty",
    "silver", "steel", "tin", "titanium", "tungsten",
    "zirconium", "zinc", "aluminum-plated", "copper-plated", "gold-plated",
    "nickel-plated", "silver-plated", "steel-plated","tin-plated", "zinc-plated",
    "mithril-plated", "mithril", "runed", "bronze", "brass",
    "platinum", "lead"/*,"lead-plated","ivory","pewter"*/
};

static byte wand_col[MAX_METALS] = {
    COLOR_LT_BLUE,COLOR_DK_GREY,COLOR_WHITE,COLOR_LT_BROWN,COLOR_YELLOW,
    COLOR_GREY,COLOR_LT_GREY,COLOR_LT_GREY,COLOR_LT_BROWN,COLOR_RED,
    COLOR_LT_GREY,COLOR_LT_GREY,COLOR_LT_GREY,COLOR_WHITE,COLOR_WHITE,
    COLOR_LT_GREY,COLOR_LT_GREY,COLOR_LT_BLUE,COLOR_LT_BROWN,COLOR_YELLOW,
    COLOR_LT_BROWN,COLOR_LT_GREY,COLOR_LT_GREY,COLOR_LT_GREY,COLOR_LT_GREY,
    COLOR_LT_BLUE,COLOR_LT_BLUE,COLOR_BROWN,COLOR_LT_BROWN,COLOR_LT_BROWN,
    COLOR_WHITE,COLOR_GREY,/*COLOR_GREY,COLOR_WHITE,COLOR_GREY*/
};


/*
 * Rods (adjectives and colors).
 * Efficiency -- copied from wand arrays
 */

static char *rod_adj[MAX_METALS];
static byte rod_col[MAX_METALS];


/*
 * Mushrooms (adjectives and colors)
 */

static char *food_adj[MAX_SHROOM] = {
    "blue", "black", "black spotted", "brown", "dark blue",
    "dark green", "dark red", "yellow", "furry", "green",
    "grey", "light blue", "light green", "violet", "red",
    "slimy", "tan", "white", "white spotted", "wrinkled",
};

static byte food_col[MAX_SHROOM] = {
    COLOR_BLUE,COLOR_DK_GREY,COLOR_DK_GREY,COLOR_BROWN,COLOR_BLUE,
    COLOR_GREEN,COLOR_RED,COLOR_YELLOW,COLOR_LT_GREY,COLOR_GREEN,
    COLOR_GREY,COLOR_LT_BLUE,COLOR_LT_GREEN,COLOR_PURPLE,COLOR_RED,
    COLOR_GREY,COLOR_LT_BROWN,COLOR_WHITE,COLOR_WHITE,COLOR_BROWN
};


/*
 * Color adjectives and colors, for potions.
 * Hack -- The first four entries are hard-coded.
 * (water, apple juice, slime mold juice, something)
 */
static char *potion_adj[MAX_COLORS] = {
    "clear","light brown", "icky green", "xxx",
    "azure","blue", "blue speckled", "black", "brown", "brown speckled",
    "bubbling", "chartreuse", "cloudy", "copper speckled", "crimson", "cyan",
    "dark blue", "dark green", "dark red", "gold speckled", "green",
    "green speckled", "grey", "grey speckled", "hazy", "indigo",
    "light blue", "light green", "magenta", "metallic blue", "metallic red",
    "metallic green", "metallic purple", "misty", "orange", "orange speckled",
    "pink", "pink speckled", "puce", "purple", "purple speckled",
    "red", "red speckled", "silver speckled", "smoky", "tangerine",
    "violet", "vermilion", "white", "yellow", "violet speckled",
    "pungent","clotted red", "viscous pink", "oily yellow","gloopy green",
    "shimmering", "coagulated crimson", "yellow speckled", "gold"
};

static byte potion_col[MAX_COLORS] = {
    COLOR_WHITE,COLOR_LT_BROWN,COLOR_GREEN,0,
    COLOR_LT_BLUE,COLOR_BLUE,COLOR_BLUE,COLOR_DK_GREY,COLOR_BROWN,COLOR_BROWN,
    COLOR_LT_GREY,COLOR_LT_GREEN,COLOR_WHITE,COLOR_LT_BROWN,COLOR_RED,COLOR_LT_BLUE,
    COLOR_BLUE,COLOR_GREEN,COLOR_RED,COLOR_YELLOW,COLOR_GREEN,
    COLOR_GREEN,COLOR_GREY,COLOR_GREY,COLOR_LT_GREY,COLOR_PURPLE,
    COLOR_LT_BLUE,COLOR_LT_GREEN,COLOR_RED,COLOR_BLUE,COLOR_RED,
    COLOR_GREEN,COLOR_PURPLE,COLOR_LT_GREY,COLOR_ORANGE,COLOR_ORANGE,
    COLOR_LT_RED,COLOR_LT_RED,COLOR_PURPLE,COLOR_PURPLE,COLOR_PURPLE,
    COLOR_RED,COLOR_RED,COLOR_LT_GREY,COLOR_DK_GREY,COLOR_ORANGE,
    COLOR_PURPLE,COLOR_RED,COLOR_WHITE,COLOR_YELLOW,COLOR_PURPLE,
    COLOR_LT_RED,COLOR_RED,COLOR_LT_RED,COLOR_YELLOW,COLOR_GREEN,
    COLOR_MULTI,COLOR_RED,COLOR_YELLOW,COLOR_YELLOW
};


// Syllables for scrolls (must be 1-4 letters each)
static char *syllables[MAX_SYLLABLES] = {
  "a","ab","ag","aks","ala","an","ankh","app",
  "arg","arze","ash","aus","ban","bar","bat","bek",
  "bie","bin","bit","bjor","blu","bot","bu",
  "byt","comp","con","cos","cre","dalf","dan",
  "den","der","doe","dok","eep","el","eng","er","ere","erk",
  "esh","evs","fa","fid","flit","for","fri","fu","gan",
  "gar","glen","gop","gre","ha","he","hyd","i",
  "ing","ion","ip","ish","it","ite","iv","jo",
  "kho","kli","klis","la","lech","man","mar",
  "me","mi","mic","mik","mon","mung","mur","nag","nej",
  "nelg","nep","ner","nes","nis","nih","nin","o",
  "od","ood","org","orn","ox","oxy","pay","pet",
  "ple","plu","po","pot","prok","re","rea","rhov",
  "ri","ro","rog","rok","rol","sa","san","sat",
  "see","sef","seh","shu","ski","sna","sne","snik",
  "sno","so","sol","sri","sta","sun","ta","tab",
  "tem","ther","ti","tox","trol","tue","turs","u",
  "ulk","um","un","uni","ur","val","viv","vly",
  "vom","wah","wed","werg","wex","whon","wun","x",
  "yerg","yp","zun","tri","blaa"
};


// Hold the titles of scrolls, 6 to 14 characters each
static char scroll_adj[MAX_TITLES][16];




// Get an object kind's flavor description
const char *flavor_desc(CObjectKind *k_ptr)
{
    switch (k_ptr->tval) {
        case TV_AMULET: return amulet_adj[k_ptr->sval];
        case TV_RING:   return ring_adj[k_ptr->sval];
        case TV_STAFF:  return staff_adj[k_ptr->sval];
        case TV_WAND:   return wand_adj[k_ptr->sval];
        case TV_SCROLL: return scroll_adj[k_ptr->sval];
        case TV_POTION: return potion_adj[k_ptr->sval];
        case TV_ROD:    return rod_adj[k_ptr->sval];
        case TV_FOOD:   return food_adj[k_ptr->sval];
    }
    return NULL;
}



/*
 * Certain items have a flavor
 * This function is used only by "flavor_init()"
 */
static bool object_has_flavor(int i)
{
    CObjectKind *k_ptr = &k_info[i];

    // Check for flavor
    switch (k_ptr->tval) {
        // The standard "flavored" items
        case TV_AMULET:
        case TV_RING:
        case TV_STAFF:
        case TV_WAND:
        case TV_SCROLL:
        case TV_POTION:
        case TV_ROD:
            return (TRUE);

        // Hack -- food SOMETIMES has a flavor
        case TV_FOOD:
            if (k_ptr->sval < SV_FOOD_MIN_FOOD) return (TRUE);
            return FALSE;
    }

    // Assume no flavor
    return FALSE;
}


/*
 * Certain items, if aware, are known instantly
 * This function is used only by "flavor_init()"
 */
static bool object_easy_know(int i)
{
    CObjectKind *k_ptr = &k_info[i];

    // Analyze the "tval"
    switch (k_ptr->tval) {
        // Spellbooks
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
            return (TRUE);

        // Simple items
        case TV_FLASK:
        case TV_BOTTLE:
        case TV_SKELETON:
            return TRUE;

        // All Food, Potions, Scrolls, Rods
        case TV_FOOD:
        case TV_POTION:
        case TV_SCROLL:
        case TV_ROD:
            return (TRUE);

        // Some Rings, Amulets, Lites
        case TV_RING:
        case TV_AMULET:
        case TV_LITE:
            if (k_ptr->flags3 & TR3_EASY_KNOW) return (TRUE);
            return (FALSE);
    }

    /* Nope */
    return (FALSE);
}


/*
 * Get the attribute of an object
 */
byte CItem::get_attr(void)
{
    switch (GetTval()) {
        case TV_SKELETON:
        case TV_BOTTLE:
            return COLOR_WHITE;
        case TV_CHEST:
            return COLOR_GREY;
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
            return COLOR_LT_BROWN;
        case TV_LITE:
            return COLOR_YELLOW;
        case TV_BOW:
            return COLOR_BROWN;
        case TV_DIGGING:
            return COLOR_GREY;
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
            return COLOR_LT_GREY;
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_CLOAK:
            return COLOR_LT_BROWN;
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
            return COLOR_GREY;
        case TV_AMULET:
            return COLOR_ORANGE;
        case TV_RING:
            return COLOR_RED;
        case TV_STAFF:
            return COLOR_LT_BROWN;
        case TV_WAND:
            return COLOR_LT_GREEN;
        case TV_ROD:
            return COLOR_LT_GREY;
        case TV_SCROLL:
            return COLOR_WHITE;
        case TV_POTION:
            return COLOR_LT_BLUE;
        case TV_FLASK:
            return COLOR_YELLOW;
        case TV_FOOD:
            return COLOR_LT_BROWN;
        case TV_MAGIC_BOOK:
            return COLOR_LT_RED;
        case TV_PRAYER_BOOK:
            return COLOR_LT_GREEN;
    }

    return COLOR_WHITE;
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
    char *temp_adj;


    // Hack -- Use the "simple" RNG
    Rand_quick = TRUE;

    // Hack -- Induce consistant flavors
    Rand_value = seed_flavor;


    /* Efficiency -- Rods/Wands share initial array */
    for (i = 0; i < MAX_METALS; i++) {
        rod_adj[i] = wand_adj[i];
        rod_col[i] = wand_col[i];
    }


    /* Rings have "ring colors" */
    for (i = 0; i < MAX_ROCKS; i++) {
        j = rand_int(MAX_ROCKS);
        temp_adj = ring_adj[i];
        ring_adj[i] = ring_adj[j];
        ring_adj[j] = temp_adj;
        temp_col = ring_col[i];
        ring_col[i] = ring_col[j];
        ring_col[j] = temp_col;
    }

    /* Amulets have "amulet colors" */
    for (i = 0; i < MAX_AMULETS; i++) {
        j = rand_int(MAX_AMULETS);
        temp_adj = amulet_adj[i];
        amulet_adj[i] = amulet_adj[j];
        amulet_adj[j] = temp_adj;
        temp_col = amulet_col[i];
        amulet_col[i] = amulet_col[j];
        amulet_col[j] = temp_col;
    }

    // Staffs
    for (i = 0; i < MAX_WOODS; i++) {
        j = rand_int(MAX_WOODS);
        temp_adj = staff_adj[i];
        staff_adj[i] = staff_adj[j];
        staff_adj[j] = temp_adj;
        temp_col = staff_col[i];
        staff_col[i] = staff_col[j];
        staff_col[j] = temp_col;
    }

    /* Wands */
    for (i = 0; i < MAX_METALS; i++) {
        j = rand_int(MAX_METALS);
        temp_adj = wand_adj[i];
        wand_adj[i] = wand_adj[j];
        wand_adj[j] = temp_adj;
        temp_col = wand_col[i];
        wand_col[i] = wand_col[j];
        wand_col[j] = temp_col;
    }

    /* Rods */
    for (i = 0; i < MAX_METALS; i++) {
        j = rand_int(MAX_METALS);
        temp_adj = rod_adj[i];
        rod_adj[i] = rod_adj[j];
        rod_adj[j] = temp_adj;
        temp_col = rod_col[i];
        rod_col[i] = rod_col[j];
        rod_col[j] = temp_col;
    }

    /* Foods (Mushrooms) */
    for (i = 0; i < MAX_SHROOM; i++) {
        j = rand_int(MAX_SHROOM);
        temp_adj = food_adj[i];
        food_adj[i] = food_adj[j];
        food_adj[j] = temp_adj;
        temp_col = food_col[i];
        food_col[i] = food_col[j];
        food_col[j] = temp_col;
    }

    /* Potions */
    for (i = 4; i < MAX_COLORS; i++) {
        j = rand_int(MAX_COLORS - 4) + 4;
        temp_adj = potion_adj[i];
        potion_adj[i] = potion_adj[j];
        potion_adj[j] = temp_adj;
        temp_col = potion_col[i];
        potion_col[i] = potion_col[j];
        potion_col[j] = temp_col;
    }

    /* Scrolls (random titles, always white) */
    for (i = 0; i < MAX_TITLES; i++) {
        /* Get a new title */
        while (TRUE) {
            char buf[80];

            bool okay;

            /* Start a new title */
            buf[0] = '\0';

            /* Collect words until done */
            while (1) {
                int q, s;

                char tmp[80];

                /* Start a new word */
                tmp[0] = '\0';

                /* Choose one or two syllables */
                s = percent(30) ? 1 : 2;

                /* Add a one or two syllable word */
                for (q = 0; q < s; q++) {
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
            for (j = 0; j < i; j++) {
                char *hack1 = scroll_adj[j];
                char *hack2 = scroll_adj[i];

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
    for (i = 1; i < MAX_K_IDX; i++) {
        CObjectKind *k_ptr = &k_info[i];

        /* Skip "empty" objects */
        if (!k_ptr->name) continue;

        /* Check for a "flavor" */
        k_ptr->has_flavor = object_has_flavor(i);

        /* No flavor yields aware */
        if (!k_ptr->has_flavor) k_ptr->aware = TRUE;

        /* Check for "easily known" */
        k_ptr->easy_know = object_easy_know(i);
    }
}




/*
 * Obtain the "flags" for an item
 */
void CItem::GetFlags(u32b *f1, u32b *f2, u32b *f3)
{
    /* Artifact */
    if (isArtifact()) {
        artifact_type *a_ptr = get_a_ptr();

        *f1 = a_ptr->flags1;
        *f2 = a_ptr->flags2;
        *f3 = a_ptr->flags3;
    }

    /* Not artifact */
    else {
        CObjectKind *k_ptr = get_k_ptr();

        *f1 = k_ptr->flags1;
        *f2 = k_ptr->flags2;
        *f3 = k_ptr->flags3;
    }

    /* Ego-item */
    if (isEgoItem()) {
        ego_item_type *e_ptr = get_e_ptr();

        *f1 |= e_ptr->flags1;
        *f2 |= e_ptr->flags2;
        *f3 |= e_ptr->flags3;
    }

    /* Extra powers */
    switch (GetXtra1()) {
        case EGO_XTRA_SUSTAIN:

            /* Choose a sustain */
            switch (GetXtra2() % 6) {
                case 0: *f2 |= TR2_SUST_STR; break;
                case 1: *f2 |= TR2_SUST_INT; break;
                case 2: *f2 |= TR2_SUST_WIS; break;
                case 3: *f2 |= TR2_SUST_DEX; break;
                case 4: *f2 |= TR2_SUST_CON; break;
                case 5: *f2 |= TR2_SUST_CHR; break;
            }

            break;

        case EGO_XTRA_POWER:

            /* Choose a power */
            switch (GetXtra2() % 9) {
                case 0: *f2 |= TR2_RES_BLIND; break;
                case 1: *f2 |= TR2_RES_CONF; break;
                case 2: *f2 |= TR2_RES_SOUND; break;
                case 3: *f2 |= TR2_RES_SHARDS; break;
                case 4: *f2 |= TR2_RES_NETHER; break;
                case 5: *f2 |= TR2_RES_NEXUS; break;
                case 6: *f2 |= TR2_RES_CHAOS; break;
                case 7: *f2 |= TR2_RES_DISEN; break;
                case 8: *f2 |= TR2_RES_POIS; break;
            }

            break;

        case EGO_XTRA_ABILITY:

            /* Choose an ability */
            switch (GetXtra2() % 8) {
                case 0: *f3 |= TR3_FEATHER; break;
                case 1: *f3 |= TR3_LITE; break;
                case 2: *f3 |= TR3_SEE_INVIS; break;
                case 3: *f3 |= TR3_TELEPATHY; break;
                case 4: *f3 |= TR3_SLOW_DIGEST; break;
                case 5: *f3 |= TR3_REGEN; break;
                case 6: *f2 |= TR2_FREE_ACT; break;
                case 7: *f2 |= TR2_HOLD_LIFE; break;
            }

            break;

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
static char *object_desc_str(char *t, char *s)
{
    /* Copy the string */
    while (*s) *t++ = *s++;

    /* Terminate */
    *t = '\0';

    /* Result */
    return (t);
}



/*
 * Print an unsigned number "n" into a string "t", as if by
 * sprintf(t, "%u", n), and return a pointer to the terminator.
 */
static char *object_desc_num(char *t, uint n)
{
    uint p;

    /* Find "size" of "n" */
    for (p = 1; n >= p * 10; p = p * 10) ;

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
static char *object_desc_int(char *t, int v)
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
    for (p = 1; n >= p * 10; p = p * 10) ;

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
 * Creates a description of the item "i_ptr", and stores it in "out_val".
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
void CItem::object_desc(char *buf, int pref, int mode)
{
    int power, indexx;
    bool aware = FALSE, known = FALSE;
    bool append_name = FALSE;
    bool show_weapon = FALSE, show_armor = FALSE;
//  -- MV: Correcting the misuse of modstr in some assigment
//	char *s, *u, *t, tmp_val[160], basenm[500], *modstr;
	char *s, *u, *t, tmp_val[160], basenm[500], modstr[100];
    u32b f1, f2, f3;
    CObjectKind *k_ptr = get_k_ptr();


    /* Extract some flags */
    GetFlags(&f1, &f2, &f3);


    /* See if the object is "aware" */
    if (isAware()) aware = TRUE;

    /* See if the object is "known" */
    if (isKnown()) known = TRUE;

    /* Hack -- Extract the sub-type "indexx" */
    indexx = GetSval();

    /* Extract default "base" string */
    strcpy(basenm , k_ptr->name);
    /* Assume no "modifier" string */
// -- MV
    strcpy(modstr, "");

    /* Analyze the object */
    switch (GetTval()) {
        /* Some objects are easy to describe */
        case TV_SKELETON:
        case TV_BOTTLE:
        case TV_FLASK:
        case TV_CHEST:
            break;


        /* Missiles/ Bows/ Weapons */
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
        case TV_BOW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:
            show_weapon = TRUE;
            break;


        // Armor
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
            show_armor = TRUE;
            break;


        // Lites (including a few "Specials")
        case TV_LITE:

            break;


        // Amulets (including a few "Specials")
        case TV_AMULET:

            // Known artifacts
            if (isArtifact() && aware) break;

            // Color the object
     // -- MV 
            strcpy(modstr, amulet_adj[indexx]);
            if (aware) append_name = TRUE;
            strcpy(basenm ,aware ? "& amulet~" : "& # amulet~");
            break;


        // Rings (including a few "Specials")
        case TV_RING:

            // Known artifacts
            if (isArtifact() && aware) break;

            /* Color the object */
     // -- MV 
	    strcpy(modstr, ring_adj[indexx]);
            if (aware) append_name = TRUE;
	    strcpy(basenm , aware ? "& ring~" : "& # ring~");
            /* Hack -- The One Ring */
            if (!aware && (GetSval() == SV_RING_POWER)) {
     // -- MV           
	       strcpy(modstr, "plain gold");
            }

            break;


        case TV_STAFF:

            // Color the object
     // -- MV           
            strcpy(modstr, staff_adj[indexx]);
            if (aware) append_name = TRUE;
            strcpy(basenm , aware ? "& staff~" : "& # staff~");
            break;

        case TV_WAND:

            /* Color the object */
     // -- MV           
            strcpy(modstr, wand_adj[indexx]);
            if (aware) append_name = TRUE;
            strcpy(basenm , aware ? "& wand~" : "& # wand~");
            break;

        case TV_ROD:

            // Color the object
     // -- MV           
            strcpy(modstr, rod_adj[indexx]);
            if (aware) append_name = TRUE;
            strcpy(basenm , aware ? "& rod~" : "& # rod~");
            break;

        case TV_SCROLL:

            // Color the object
     // -- MV           
            strcpy(modstr, scroll_adj[indexx]);
            if (aware) append_name = TRUE;
	    strcpy(basenm , aware ? "& scroll~" : "& scroll~ titled \"#\"");
            break;

        case TV_POTION:

            /* Color the object */
     // -- MV           
            strcpy(modstr, potion_adj[indexx]);
            if (aware) append_name = TRUE;
	    strcpy(basenm , aware ? "& potion~" : "& # potion~");
            break;

        case TV_FOOD:

            /* Ordinary food is "boring" */
            if (GetSval() >= SV_FOOD_MIN_FOOD) break;

            /* Color the object */
     // -- MV           
            strcpy(modstr, food_adj[indexx]);
            if (aware) append_name = TRUE;
	    strcpy(basenm , aware ? "& mushroom~" : "& # mushroom~");
            break;


        /* Magic Books */
        case TV_MAGIC_BOOK:
            strcpy(modstr , basenm);
            strcpy(basenm , "& book~ of magic spells #");
            break;

        /* Prayer Books */
        case TV_PRAYER_BOOK:
            strcpy(modstr , basenm);
	    strcpy(basenm , "& holy book~ of prayers #");
            break;

        /* Hack -- Gold/Gems */
        case TV_GOLD:
            strcpy(buf, basenm);
            return;

        /* Used in the "inventory" routine */
        default:
            strcpy(buf, "(nothing)");
            return;
    }

    /* Start dumping the result */
    t = buf;

    /* The object "expects" a "number" */
    if (basenm[0] == '&') {
        /* Skip the ampersand (and space) */
        s = basenm + 2;

        /* No prefix */
        if (!pref)
        {
            /* Nothing */
        }

        /* Hack -- None left */
        else if (GetNumber() <= 0) {
            t = object_desc_str(t, "no more ");
        }

        /* Extract the number */
        else if (isPlural()) {
            t = object_desc_num(t, GetNumber());
            t = object_desc_chr(t, ' ');
        }

        /* Hack -- The only one of its kind */
        else if (known && isArtifact()) {
            t = object_desc_str(t, "the ");
        }

        /* A single one, with a vowel in the modifier */
        else if ((*s == '#') && (is_a_vowel(modstr[0]))) {
            t = object_desc_str(t, "an ");
        }

        /* A single one, with a vowel */
        else if (is_a_vowel(*s)) {
            t = object_desc_str(t, "an ");
        }

        // A single one, without a vowel
        else {
            t = object_desc_str(t, "a ");
        }
    }

    /* Hack -- objects that "never" take an article */
    else {
        /* No ampersand */
        s = basenm;

        /* No pref */
        if (!pref) {
            /* Nothing */
        }

        /* Hack -- all gone */
        else if (GetNumber() <= 0) {
            t = object_desc_str(t, "no more ");
        }

        /* Prefix a number if required */
        else if (isPlural()) {
            t = object_desc_num(t, GetNumber());
            t = object_desc_chr(t, ' ');
        }

        /* Hack -- The only one of its kind */
        else if (known && isArtifact()) {
            t = object_desc_str(t, "the ");
        }

        /* Hack -- single items get no prefix */
        else {
            /* Nothing */
        }
    }

    /* Paranoia -- skip illegal tildes */
    /* while (*s == '~') s++; */

    /* Copy the string */
    for ( ; *s; s++) {
        /* Pluralizer */
        if (*s == '~') {
            /* Add a plural if needed */
            if (GetNumber() != 1) {
                char k = t[-1];

                /* XXX XXX XXX Mega-Hack */

                /* Hack -- "Cutlass-es" and "Torch-es" */
                if ((k == 's') || (k == 'h')) *t++ = 'e';

                /* Add an 's' */
                *t++ = 's';
            }
        }

        /* Modifier */
        else if (*s == '#') {
            /* Insert the modifier */
            for (u = modstr; *u; u++) *t++ = *u;
        }

        /* Normal */
        else {
            /* Copy */
            *t++ = *s;
        }
    }

    /* Terminate */
    *t = '\0';


    /* Append the "kind name" to the "base name" */
    if (append_name) {
        t = object_desc_str(t, " of ");
        t = object_desc_str(t, k_ptr->name);
    }


    /* Hack -- Append "Artifact" or "Special" names */
    if (known) {
        /* Grab any artifact name */
        if (isArtifact()) {
            artifact_type *a_ptr = get_a_ptr();

            t = object_desc_chr(t, ' ');
            t = object_desc_str(t, (a_name + a_ptr->name));
        }

        /* Grab any ego-item name */
        else if (isEgoItem()) {
            ego_item_type *e_ptr = get_e_ptr();

            t = object_desc_chr(t, ' ');
            t = object_desc_str(t, (e_name + e_ptr->name));
        }
    }


    /* No more details wanted */
    if (mode < 1) return;


    /* Hack -- Chests must be described in detail */
    if (GetTval() == TV_CHEST) {
        /* Not searched yet */
        if (!known) {
            /* Nothing */
        }

        /* May be "empty" */
        else if (!GetPval()) {
            t = object_desc_str(t, " (empty)");
        }

        /* May be "disarmed" */
        else if (GetPval() < 0) {
            if (chest_traps[GetPval()]) {
                t = object_desc_str(t, " (disarmed)");
            }
            else {
                t = object_desc_str(t, " (unlocked)");
            }
        }

        /* Describe the traps, if any */
        else {
            /* Describe the traps */
            switch (chest_traps[GetPval()]) {
                case 0:
                    t = object_desc_str(t, " (locked)");
                    break;
                case CHEST_LOSE_STR:
                    t = object_desc_str(t, " (poison needle)");
                    break;
                case CHEST_LOSE_CON:
                    t = object_desc_str(t, " (poison needle)");
                    break;
                case CHEST_POISON:
                    t = object_desc_str(t, " (gas trap)");
                    break;
                case CHEST_PARALYZE:
                    t = object_desc_str(t, " (gas trap)");
                    break;
                case CHEST_EXPLODE:
                    t = object_desc_str(t, " (explosion device)");
                    break;
                case CHEST_SUMMON:
                    t = object_desc_str(t, " (summoning runes)");
                    break;
                default:
                    t = object_desc_str(t, " (multiple traps)");
                    break;
            }
        }
    }


    /* Display the item like a weapon */
    if (f3 & TR3_SHOW_MODS) show_weapon = TRUE;

    /* Display the item like a weapon */
    if (GetToH() && GetToD()) show_weapon = TRUE;

    /* Display the item like armor */
    if (GetAC()) show_armor = TRUE;


    /* Dump base weapon info */
    switch (GetTval()) {
        /* Missiles and Weapons */
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:

            /* Append a "damage" string */
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '(');
            t = object_desc_num(t, GetDD());
            t = object_desc_chr(t, 'd');
            t = object_desc_num(t, GetDS());
            t = object_desc_chr(t, ')');

            /* All done */
            break;


        /* Bows get a special "damage string" */
        case TV_BOW:

            /* Mega-Hack -- Extract the "base power" */
            power = (GetSval() % 10);

            /* Apply the "Extra Might" flag */
            if (f3 & TR3_XTRA_MIGHT) power++;

            /* Append a special "damage" string */
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '(');
            t = object_desc_chr(t, 'x');
            t = object_desc_num(t, power);
            t = object_desc_chr(t, ')');

            /* All done */
            break;
    }


    /* Add the weapon bonuses */
    if (known) {
        /* Show the tohit/todam on request */
        if (show_weapon) {
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '(');
            t = object_desc_int(t, GetToH());
            t = object_desc_chr(t, ',');
            t = object_desc_int(t, GetToD());
            t = object_desc_chr(t, ')');
        }

        /* Show the tohit if needed */
        else if (GetToH()) {
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '(');
            t = object_desc_int(t, GetToH());
            t = object_desc_chr(t, ')');
        }

        /* Show the todam if needed */
        else if (GetToD()) {
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '(');
            t = object_desc_int(t, GetToD());
            t = object_desc_chr(t, ')');
        }
    }


    /* Add the armor bonuses */
    if (known) {
        /* Show the armor class info */
        if (show_armor) {
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '[');
            t = object_desc_num(t, GetAC());
            t = object_desc_chr(t, ',');
            t = object_desc_int(t, GetToA());
            t = object_desc_chr(t, ']');
        }

        /* No base armor, but does increase armor */
        else if (GetToA()) {
            t = object_desc_chr(t, ' ');
            t = object_desc_chr(t, '[');
            t = object_desc_int(t, GetToA());
            t = object_desc_chr(t, ']');
        }
    }

    /* Hack -- always show base armor */
    else if (show_armor) {
        t = object_desc_chr(t, ' ');
        t = object_desc_chr(t, '[');
        t = object_desc_num(t, GetAC());
        t = object_desc_chr(t, ']');
    }


    /* No more details wanted */
    if (mode < 2) return;


    /* Hack -- Wands and Staffs have charges */
    if (known && ((GetTval() == TV_STAFF) || (GetTval() == TV_WAND))) {
        /* Dump " (N charges)" */
        t = object_desc_chr(t, ' ');
        t = object_desc_chr(t, '(');
        t = object_desc_num(t, GetPval());
        t = object_desc_str(t, " charge");
        if (GetPval() != 1) t = object_desc_chr(t, 's');
        t = object_desc_chr(t, ')');
    }

    /* Hack -- Rods have a "charging" indicator */
    else if (known && (GetTval() == TV_ROD)) {
        /* Hack -- Dump " (charging)" if relevant */
        if (GetPval()) t = object_desc_str(t, " (charging)");
    }

    /* Hack -- Process Lanterns/Torches */
    else if ((GetTval() == TV_LITE) && !isArtifact()) {
        /* Hack -- Turns of light for normal lites */
        t = object_desc_str(t, " (with ");
        t = object_desc_num(t, GetPval());
        t = object_desc_str(t, " turns of light)");
    }


    /* Dump "pval" flags for wearable items */
    if (known && (f1 & TR1_PVAL_MASK)) {
        /* Start the display */
        t = object_desc_chr(t, ' ');
        t = object_desc_chr(t, '(');

        /* Dump the "pval" itself */
        t = object_desc_int(t, GetPval());

        /* Do not display the "pval" flags */
        if (f3 & TR3_HIDE_TYPE) {
            /* Nothing */
        }

        /* Speed */
        else if (f1 & TR1_SPEED) {
            /* Dump " to speed" */
            t = object_desc_str(t, " to speed");
        }

        /* Attack speed */
        else if (f1 & TR1_BLOWS) {
            /* Add " attack" */
            t = object_desc_str(t, " attack");

            /* Add "attacks" */
            if (ABS(GetPval()) != 1) t = object_desc_chr(t, 's');
        }

        /* Stealth */
        else if (f1 & TR1_STEALTH) {
            /* Dump " to stealth" */
            t = object_desc_str(t, " to stealth");
        }

        /* Search */
        else if (f1 & TR1_SEARCH) {
            /* Dump " to searching" */
            t = object_desc_str(t, " to searching");
        }

        /* Infravision */
        else if (f1 & TR1_INFRA) {
            /* Dump " to infravision" */
            t = object_desc_str(t, " to infravision");
        }

        /* Tunneling */
        else if (f1 & TR1_TUNNEL) {
            /* Nothing */
        }

        /* Finish the display */
        t = object_desc_chr(t, ')');
    }


    /* Indicate "charging" artifacts XXX XXX XXX */
    if (known && GetTimeout()) {
        /* Hack -- Dump " (charging)" if relevant */
        t = object_desc_str(t, " (charging)");
    }


    /* No more details wanted */
    if (mode < 3) return;


    /* No inscription yet */
    tmp_val[0] = '\0';

    /* Use the standard inscription if available */
    if (GetNote()) {
        strcpy(tmp_val, GetNote());
    }

    /* Note "cursed" if the item is known to be cursed */
    else if (isCursed() && (known || TestIdentFlag(ID_SENSE))) {
        strcpy(tmp_val, "cursed");
    }

    /* Mega-Hack -- note empty wands/staffs */
    else if (!known && TestIdentFlag(ID_EMPTY)) {
        strcpy(tmp_val, "empty");
    }

    /* Note "tried" if the object has been tested unsuccessfully */
    else if (!aware && isTried()) {
        strcpy(tmp_val, "tried");
    }

    /* Note the discount, if any */
    else if (GetDiscount()) {
        object_desc_num(tmp_val, GetDiscount());
        strcat(tmp_val, "% off");
    }

    /* Append the inscription, if any */
    if (tmp_val[0]!='\0') {
        int n;

        /* Hack -- How much so far */
        n = (t - buf);
        /* Paranoia -- do not be stupid */
        if (n > 75) n = 75;

        /* Hack -- shrink the inscription */
        tmp_val[75 - n] = '\0';

        /* Append the inscription */
        t = object_desc_chr(t, ' ');
        t = object_desc_chr(t, '{');
        t = object_desc_str(t, tmp_val);
        t = object_desc_chr(t, '}');
    }
}


/*
 * Hack -- describe an item currently in a store's inventory
 * This allows an item to *look* like the player is "aware" of it
 */
void CItem::object_desc_store(char *buf, int pref, int mode)
{
    CObjectKind *k_ptr = get_k_ptr();

    /* Save the "aware" flag */
    bool hack_aware = k_ptr->aware;

    /* Save the "known" flag */
    bool hack_known = TestIdentFlag(ID_KNOWN);


    /* Set the "known" flag */
    SetIdentFlag(ID_KNOWN);

    /* Force "aware" for description */
    k_ptr->aware = TRUE;

    /* Describe the object */
    object_desc(buf, pref, mode);

    /* Restore "aware" flag */
    k_ptr->aware = hack_aware;

    /* Clear the known flag */
    if (!hack_known) ClearIdentFlag(ID_KNOWN);
}




/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
char *item_activation(CItem *i_ptr)
{
    u32b f1, f2, f3;

    /* Extract the flags */
    i_ptr->GetFlags(&f1, &f2, &f3);

    /* Require activation ability */
    if (!(f3 & TR3_ACTIVATE)) return NULL;

    /* Some artifacts can be activated */
    switch (i_ptr->GetName1()) {
        case ART_NARTHANC:
            return "fire bolt (9d8) every 8+d8 turns";
        case ART_NIMTHANC:
            return "frost bolt (6d8) every 7+d7 turns";
        case ART_DETHANC:
            return "lightning bolt (4d8) every 6+d6 turns";
        case ART_RILIA:
            return "stinking cloud (12) every 4+d4 turns";
        case ART_BELANGIL:
            return "frost ball (48) every 5+d5 turns";
        case ART_DAL:
            return "remove fear and cure poison every 5 turns";
        case ART_RINGIL:
            return "frost ball (100) every 300 turns";
        case ART_ANDURIL:
            return "fire ball (72) every 400 turns";
        case ART_FIRESTAR:
            return "large fire ball (72) every 100 turns";
        case ART_FEANOR:
            return "haste self (20+d20 turns) every 200 turns";
        case ART_THEODEN:
            return "drain life (120) every 400 turns";
        case ART_TURMIL:
            return "drain life (90) every 70 turns";
        case ART_CASPANION:
            return "door and trap destruction every 10 turns";
        case ART_AVAVIR:
            return "word of recall every 200 turns";
        case ART_TARATOL:
            return "haste self (20+d20 turns) every 100+d100 turns";
        case ART_ERIRIL:
            return "identify every 10 turns";
        case ART_OLORIN:
            return "probing every 20 turns";
        case ART_EONWE:
            return "mass genocide every 1000 turns";
        case ART_LOTHARANG:
            return "cure wounds (4d7) every 3+d3 turns";
        case ART_CUBRAGOL:
            return "fire branding of bolts every 999 turns";
        case ART_ARANRUTH:
            return "frost bolt (12d8) every 500 turns";
        case ART_AEGLOS:
            return "frost ball (100) every 500 turns";
        case ART_OROME:
            return "stone to mud every 5 turns";
        case ART_SOULKEEPER:
            return "heal (1000) every 888 turns";
        case ART_BELEGENNON:
            return "phase door every 2 turns";
        case ART_CELEBORN:
            return "genocide every 500 turns";
        case ART_LUTHIEN:
            return "restore life levels every 450 turns";
        case ART_ULMO:
            return "teleport away every 150 turns";
        case ART_COLLUIN:
            return "resistance (20+d20 turns) every 111 turns";
        case ART_HOLCOLLETH:
            return "Sleep II every 55 turns";
        case ART_THINGOL:
            return "recharge item I every 70 turns";
        case ART_COLANNON:
            return "teleport every 45 turns";
        case ART_TOTILA:
            return "confuse monster every 15 turns";
        case ART_CAMMITHRIM:
            return "magic missile (3d4) every 2 turns";
        case ART_PAURHACH:
            return "fire bolt (9d8) every 8+d8 turns";
        case ART_PAURNIMMEN:
            return "frost bolt (6d8) every 7+d7 turns";
        case ART_PAURAEGEN:
            return "lightning bolt (4d8) every 6+d6 turns";
        case ART_PAURNEN:
            return "acid bolt (5d8) every 5+d5 turns";
        case ART_FINGOLFIN:
            return "a magical arrow (150) every 90+d90 turns";
        case ART_HOLHENNETH:
            return "detection every 55+d55 turns";
        case ART_GONDOR:
            return "heal (500) every 500 turns";
        case ART_RAZORBACK:
            return "star ball (150) every 1000 turns";
        case ART_BLADETURNER:
            return "berserk rage, bless, and resistance every 400 turns";
        case ART_GALADRIEL:
            return "illumination every 10+d10 turns";
        case ART_ELENDIL:
            return "magic mapping every 50+d50 turns";
        case ART_THRAIN:
            return "clairvoyance every 100+d100 turns";
        case ART_INGWE:
            return "dispel evil (x5) every 300+d300 turns";
        case ART_CARLAMMAS:
            return "protection from evil every 225+d225 turns";
        case ART_TULKAS:
            return "haste self (75+d75 turns) every 150+d150 turns";
        case ART_NARYA:
            return "large fire ball (120) every 225+d225 turns";
        case ART_NENYA:
            return "large frost ball (200) every 325+d325 turns";
        case ART_VILYA:
            return "large lightning ball (250) every 425+d425 turns";
        case ART_POWER:
            return "bizarre things every 450+d450 turns";
    }


    /* Require dragon scale mail */
    if (i_ptr->GetTval() != TV_DRAG_ARMOR) return NULL;

    /* Branch on the sub-type */
    switch (i_ptr->GetSval()) {
        case SV_DRAGON_BLUE:
            return "breathe lightning (100) every 450+d450 turns";
        case SV_DRAGON_WHITE:
            return "breathe frost (110) every 450+d450 turns";
        case SV_DRAGON_BLACK:
            return "breathe acid (130) every 450+d450 turns";
        case SV_DRAGON_GREEN:
            return "breathe poison gas (150) every 450+d450 turns";
        case SV_DRAGON_RED:
            return "breathe fire (200) every 450+d450 turns";
        case SV_DRAGON_MULTIHUED:
            return "breathe multi-hued (250) every 225+d225 turns";
        case SV_DRAGON_BRONZE:
            return "breathe confusion (120) every 450+d450 turns";
        case SV_DRAGON_GOLD:
            return "breathe sound (130) every 450+d450 turns";
        case SV_DRAGON_CHAOS:
            return "breathe chaos/disenchant (220) every 300+d300 turns";
        case SV_DRAGON_LAW:
            return "breathe sound/shards (230) every 300+d300 turns";
        case SV_DRAGON_BALANCE:
            return "You breathe balance (250) every 300+d300 turns";
        case SV_DRAGON_SHINING:
            return "breathe light/darkness (200) every 300+d300 turns";
        case SV_DRAGON_POWER:
            return "breathe the elements (300) every 300+d300 turns";
    }


    /* Oops */
    return NULL;
}


/*
 * Describe a "fully identified" item
 */
bool identify_fully_aux(CItem *i_ptr)
{
    int i = 0, j, k;
    u32b f1, f2, f3;
    char *info[128];
    byte *screen;


    /* Extract the flags */
    i_ptr->GetFlags(&f1, &f2, &f3);


    /* Mega-Hack -- describe activation */
    if (f3 & TR3_ACTIVATE) {
        info[i++] = "It can be activated for...";
        info[i++] = item_activation(i_ptr);
        info[i++] = "...if it is being worn.";
    }


    /* Hack -- describe lite's */
    if (i_ptr->GetTval() == TV_LITE) {
        if (i_ptr->isArtifact()) {
            info[i++] = "It provides light (radius 3) forever.";
        }
        else if (i_ptr->GetSval() == SV_LITE_LANTERN)
        {
            info[i++] = "It provides light (radius 2) when fueled.";
        }
        else
        {
            info[i++] = "It provides light (radius 1) when fueled.";
        }
    }


    /* And then describe it fully */

    if (f1 & TR1_STR) {
        info[i++] = "It affects your strength.";
    }
    if (f1 & TR1_INT) {
        info[i++] = "It affects your intelligence.";
    }
    if (f1 & TR1_WIS) {
        info[i++] = "It affects your wisdom.";
    }
    if (f1 & TR1_DEX) {
        info[i++] = "It affects your dexterity.";
    }
    if (f1 & TR1_CON) {
        info[i++] = "It affects your constitution.";
    }
    if (f1 & TR1_CHR) {
        info[i++] = "It affects your charisma.";
    }

    if (f1 & TR1_STEALTH) {
        info[i++] = "It affects your stealth.";
    }
    if (f1 & TR1_SEARCH) {
        info[i++] = "It affects your searching.";
    }
    if (f1 & TR1_INFRA) {
        info[i++] = "It affects your infravision.";
    }
    if (f1 & TR1_TUNNEL) {
        info[i++] = "It affects your ability to tunnel.";
    }
    if (f1 & TR1_SPEED) {
        info[i++] = "It affects your speed.";
    }
    if (f1 & TR1_BLOWS) {
        info[i++] = "It affects your attack speed.";
    }

    if (f1 & TR1_BRAND_ACID) {
        info[i++] = "It does extra damage from acid.";
    }
    if (f1 & TR1_BRAND_ELEC) {
        info[i++] = "It does extra damage from electricity.";
    }
    if (f1 & TR1_BRAND_FIRE) {
        info[i++] = "It does extra damage from fire.";
    }
    if (f1 & TR1_BRAND_COLD) {
        info[i++] = "It does extra damage from frost.";
    }
    if (f1 & TR1_BRAND_POIS) {
        info[i++] = "It does extra damage from poison.";
    }

    if (f1 & TR1_IMPACT) {
        info[i++] = "It can cause earthquakes.";
    }

    if (f1 & TR1_KILL_DRAGON) {
        info[i++] = "It is a great bane of dragons.";
    }
    else if (f1 & TR1_SLAY_DRAGON) {
        info[i++] = "It is especially deadly against dragons.";
    }
    if (f1 & TR1_SLAY_ORC) {
        info[i++] = "It is especially deadly against orcs.";
    }
    if (f1 & TR1_SLAY_TROLL) {
        info[i++] = "It is especially deadly against trolls.";
    }
    if (f1 & TR1_SLAY_GIANT) {
        info[i++] = "It is especially deadly against giants.";
    }
    if (f1 & TR1_SLAY_DEMON) {
        info[i++] = "It strikes at demons with holy wrath.";
    }
    if (f1 & TR1_SLAY_UNDEAD) {
        info[i++] = "It strikes at undead with holy wrath.";
    }
    if (f1 & TR1_SLAY_EVIL) {
        info[i++] = "It fights against evil with holy fury.";
    }
    if (f1 & TR1_SLAY_ANIMAL) {
        info[i++] = "It is especially deadly against natural creatures.";
    }

    if (f2 & TR2_SUST_STR) {
        info[i++] = "It sustains your strength.";
    }
    if (f2 & TR2_SUST_INT) {
        info[i++] = "It sustains your intelligence.";
    }
    if (f2 & TR2_SUST_WIS) {
        info[i++] = "It sustains your wisdom.";
    }
    if (f2 & TR2_SUST_DEX) {
        info[i++] = "It sustains your dexterity.";
    }
    if (f2 & TR2_SUST_CON) {
        info[i++] = "It sustains your constitution.";
    }
    if (f2 & TR2_SUST_CHR) {
        info[i++] = "It sustains your charisma.";
    }

    if (f2 & TR2_IM_ACID) {
        info[i++] = "It provides immunity to acid.";
    }
    if (f2 & TR2_IM_ELEC) {
        info[i++] = "It provides immunity to electricity.";
    }
    if (f2 & TR2_IM_FIRE) {
        info[i++] = "It provides immunity to fire.";
    }
    if (f2 & TR2_IM_COLD) {
        info[i++] = "It provides immunity to cold.";
    }

    if (f2 & TR2_FREE_ACT) {
        info[i++] = "It provides immunity to paralysis.";
    }
    if (f2 & TR2_HOLD_LIFE) {
        info[i++] = "It provides resistance to life draining.";
    }

    if (f2 & TR2_RES_ACID) {
        info[i++] = "It provides resistance to acid.";
    }
    if (f2 & TR2_RES_ELEC) {
        info[i++] = "It provides resistance to electricity.";
    }
    if (f2 & TR2_RES_FIRE) {
        info[i++] = "It provides resistance to fire.";
    }
    if (f2 & TR2_RES_COLD) {
        info[i++] = "It provides resistance to cold.";
    }
    if (f2 & TR2_RES_POIS) {
        info[i++] = "It provides resistance to poison.";
    }

    if (f2 & TR2_RES_LITE) {
        info[i++] = "It provides resistance to light.";
    }
    if (f2 & TR2_RES_DARK) {
        info[i++] = "It provides resistance to dark.";
    }

    if (f2 & TR2_RES_BLIND) {
        info[i++] = "It provides resistance to blindness.";
    }
    if (f2 & TR2_RES_CONF) {
        info[i++] = "It provides resistance to confusion.";
    }
    if (f2 & TR2_RES_SOUND) {
        info[i++] = "It provides resistance to sound.";
    }
    if (f2 & TR2_RES_SHARDS) {
        info[i++] = "It provides resistance to shards.";
    }

    if (f2 & TR2_RES_NETHER) {
        info[i++] = "It provides resistance to nether.";
    }
    if (f2 & TR2_RES_NEXUS) {
        info[i++] = "It provides resistance to nexus.";
    }
    if (f2 & TR2_RES_CHAOS) {
        info[i++] = "It provides resistance to chaos.";
    }
    if (f2 & TR2_RES_DISEN) {
        info[i++] = "It provides resistance to disenchantment.";
    }

    if (f3 & TR3_FEATHER) {
        info[i++] = "It induces feather falling.";
    }
    if (f3 & TR3_LITE) {
        info[i++] = "It provides permanent light.";
    }
    if (f3 & TR3_SEE_INVIS) {
        info[i++] = "It allows you to see invisible monsters.";
    }
    if (f3 & TR3_TELEPATHY) {
        info[i++] = "It gives telepathic powers.";
    }
    if (f3 & TR3_SLOW_DIGEST) {
        info[i++] = "It slows your metabolism.";
    }
    if (f3 & TR3_REGEN) {
        info[i++] = "It speeds your regenerative powers.";
    }

    if (f3 & TR3_XTRA_MIGHT) {
        info[i++] = "It fires missiles with extra might.";
    }
    if (f3 & TR3_XTRA_SHOTS) {
        info[i++] = "It fires missiles excessively fast.";
    }

    if (f3 & TR3_DRAIN_EXP) {
        info[i++] = "It drains experience.";
    }
    if (f3 & TR3_TELEPORT) {
        info[i++] = "It induces random teleportation.";
    }
    if (f3 & TR3_AGGRAVATE) {
        info[i++] = "It aggravates nearby creatures.";
    }

    if (f3 & TR3_BLESSED) {
        info[i++] = "It has been blessed by the gods.";
    }

    if (i_ptr->isCursed()) {
        if (f3 & TR3_PERMA_CURSE) {
            info[i++] = "It is permanently cursed.";
        }
        else if (f3 & TR3_HEAVY_CURSE) {
            info[i++] = "It is heavily cursed.";
        }
        else {
            info[i++] = "It is cursed.";
        }
    }


    if (f3 & TR3_IGNORE_ACID) {
        info[i++] = "It cannot be harmed by acid.";
    }
    if (f3 & TR3_IGNORE_ELEC) {
        info[i++] = "It cannot be harmed by electricity.";
    }
    if (f3 & TR3_IGNORE_FIRE) {
        info[i++] = "It cannot be harmed by fire.";
    }
    if (f3 & TR3_IGNORE_COLD) {
        info[i++] = "It cannot be harmed by cold.";
    }


    /* No special effects */
    if (!i) return FALSE;


    /* Save the screen */
    screen = save_screen();

    /* Erase the screen */
    box(0, 0, 639, 479, COLOR_BLACK);

    /* Label the information */
    put_string(1*8, 15*16, "     Item Attributes:", COLOR_WHITE);

    /* We will print on top of the map (column 13) */
    for (k = 2, j = 0; j < i; j++) {
        /* Show the info */
        put_string((k++)*8, 15*16, (char *) info[j], COLOR_WHITE);

        /* Every 20 entries (lines 2 to 21), start over */
        if ((k == 22) && (j+1 < i)) {
            put_string(k*8, 15*16, "-- more --", COLOR_WHITE);
            screen_refresh();
            wait_for_key();
            for ( ; k > 2; k--) box(0, 32, 639, 479, COLOR_BLACK);
        }
    }

    // Wait for it
    put_string(k*8, 15*16, "[Press any key to continue]", COLOR_WHITE);
    screen_refresh();
    wait_for_key();

    /* Restore the screen */
    restore_screen(screen);
    delete[] screen;

    /* Gave knowledge */
    return (TRUE);
}



/*
 * Convert an inventory index into a one character label
 * Note that the label does NOT distinguish inven/equip.
 */
s16b index_to_label(int i)
{
    /* Indexes for "inven" are easy */
    if (i < INVEN_WIELD) return I2A(i);

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

    // Convert
    i = (islower(c) ? A2I(c) : -1);

    /* Verify the index */
    if ((i < 0) || (i > INVEN_PACK)) return (-1);

    /* Empty slots can never be chosen */
    if (!inventory[i].exists()) return (-1);

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

    // Convert
    i = (islower(c) ? A2I(c) : -1) + INVEN_WIELD;

    // Verify the index
    if ((i < INVEN_WIELD) || (i >= INVEN_TOTAL)) return (-1);

    // Empty slots can never be chosen
    if (!inventory[i].exists()) return (-1);

    // Return the index
    return (i);
}



/*
 * Determine which equipment slot (if any) an item likes
 */
s16b CItem::WieldSlot(void)
{
    // Slot for equipment
    switch (GetTval()) {
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
            return INVEN_WIELD;

        case TV_BOW:
            return INVEN_BOW;

        case TV_RING:

            // Use the right hand first
            if (!inventory[INVEN_RIGHT].exists()) return INVEN_RIGHT;

            // Use the left hand for swapping (by default)
            return INVEN_LEFT;

        case TV_AMULET:
            return INVEN_NECK;

        case TV_LITE:
            return INVEN_LITE;

        case TV_DRAG_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SOFT_ARMOR:
            return INVEN_BODY;

        case TV_CLOAK:
            return INVEN_OUTER;

        case TV_SHIELD:
            return INVEN_ARM;

        case TV_CROWN:
        case TV_HELM:
            return INVEN_HEAD;

        case TV_GLOVES:
            return INVEN_HANDS;

        case TV_BOOTS:
            return INVEN_FEET;

        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
            return INVEN_ARROW;
    }

    // No slot available
    return -1;
}


/*
 * Return a string mentioning how a given item is carried
 */
char *mention_use(int i)
{
    char *p;

    /* Examine the location */
    switch (i) {
        case INVEN_WIELD: p = "Wielding"; break;
        case INVEN_BOW:   p = "Shooting with"; break;
        case INVEN_ARROW: p = "Shooting"; break;
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
    if (i == INVEN_WIELD) {
        CItem *i_ptr;
        i_ptr = &inventory[i];
        if (adj_str_hold[p_ptr->GetStatInd(STAT_STR)] < i_ptr->GetWeight()/10) {
            p = "Just lifting";
        }
    }

    /* Hack -- Heavy bow */
    if (i == INVEN_BOW) {
        CItem *i_ptr;
        i_ptr = &inventory[i];
        if (adj_str_hold[p_ptr->GetStatInd(STAT_STR)] < i_ptr->GetWeight()/10) {
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
char *describe_use(int i)
{
    char *p;

    switch (i) {
        case INVEN_WIELD: p = "attacking monsters with"; break;
        case INVEN_BOW:   p = "shooting missiles with"; break;
        case INVEN_ARROW: p = "shooting"; break;
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
    if (i == INVEN_WIELD) {
        CItem *i_ptr;
        i_ptr = &inventory[i];
        if (adj_str_hold[p_ptr->GetStatInd(STAT_STR)] < i_ptr->GetWeight()/10) {
            p = "just lifting";
        }
    }

    /* Hack -- Heavy bow */
    if (i == INVEN_BOW) {
        CItem *i_ptr;
        i_ptr = &inventory[i];
        if (adj_str_hold[p_ptr->GetStatInd(STAT_STR)] < i_ptr->GetWeight()/10) {
            p = "just holding";
        }
    }

    /* Return the result */
    return p;
}





/*
 * Check an item against the item tester info
 */
bool item_tester_okay(CItem *i_ptr)
{
    /* Hack -- allow listing empty slots */
    if (item_tester_full) return TRUE;

    /* Require an item */
    if (!i_ptr->exists()) return FALSE;

    /* Hack -- ignore "gold" */
    if (i_ptr->GetTval() == TV_GOLD) return FALSE;

    /* Check the tval */
    if (item_tester_tval) {
        if (!(item_tester_tval == i_ptr->GetTval())) return FALSE;
    }

    /* Check the hook */
    if (item_tester_hook) {
        if (!(*item_tester_hook)(i_ptr)) return FALSE;
    }

    /* Assume okay */
    return (TRUE);
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

    CItem *i_ptr;

    char i_name[80];

    char tmp_val[80];

    int out_index[23];
    byte out_color[23];
    char out_desc[23][80];


    /* Starting column */
    col = 50;

    /* Default "max-length" */
    len = 79 - col;

    /* Maximum space allowed for descriptions */
    lim = 79 - 3;

    /* Require space for weight (if needed) */
    if (show_inven_weight) lim -= 9;


    /* Find the "final" slot */
    for (i = 0; i < INVEN_PACK; i++) {
        i_ptr = &inventory[i];

        /* Track non-empty slots */
        if (i_ptr->exists()) z = i + 1;
    }

    /* Display the inventory */
    for (k = 0, i = 0; i < z; i++) {
        i_ptr = &inventory[i];

        /* Is this item acceptable? */
        if (!item_tester_okay(i_ptr)) continue;

        /* Describe the object */
        i_ptr->object_desc(i_name, TRUE, 3);

        /* Hack -- enforce max length */
        i_name[lim] = '\0';

        /* Save the object index, color, and description */
        out_index[k] = i;
        out_color[k] = i_ptr->get_attr();
        (void)strcpy(out_desc[k], i_name);

        /* Find the predicted "line length" */
        l = strlen(out_desc[k]) + 5;

        /* Be sure to account for the weight */
        if (show_inven_weight) l += 9;

        /* Maintain the maximum length */
        if (l > len) len = l;

        /* Advance to next "line" */
        k++;
    }

    /* Find the column to start in */
    col = (len > 76) ? 0 : (79 - len);

    /* Output each entry */
    for (j = 0; j < k; j++) {
        /* Get the index */
        i = out_index[j];

        /* Get the item */
        i_ptr = &inventory[i];

        /* Clear the line */
        box((col ? (col - 2) : col)*8, (j+1)*16, 639, (j+1)*16+15, COLOR_BLACK);

        /* Prepare an index --(-- */
        sprintf(tmp_val, "%c)", index_to_label(i));

        /* Clear the line with the (possibly indented) index */
        put_string(col*8, (j+1)*16, tmp_val, COLOR_WHITE);

        /* Display the entry itself */
        put_text((col+3)*8, (j+1)*16, out_desc[j], out_color[j], FONT_BOLD);

        /* Display the weight if needed */
        if (show_inven_weight) {
            int wgt = i_ptr->GetWeight() * i_ptr->GetNumber();
            (void)sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
            put_string(71*8, (j+1)*16, tmp_val, COLOR_WHITE);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && (j < 23)) {
        box((col ? (col - 2) : col)*8, (j+1)*16, 639, (j+1)*16+15, COLOR_BLACK);
    }
}



/*
 * Display the equipment.
 */
void show_equip(void)
{
    int i, j, k, l;
    int col, len, lim;

    CItem *i_ptr;

    char tmp_val[80];

    char i_name[80];

    int out_index[23];
    byte out_color[23];
    char out_desc[23][80];


    /* Starting column */
    col = 50;

    /* Maximal length */
    len = 79 - col;

    /* Maximum space allowed for descriptions */
    lim = 79 - 3;

    /* Require space for labels (if needed) */
    if (show_equip_label) lim -= (14 + 2);

    /* Require space for weight (if needed) */
    if (show_equip_weight) lim -= 9;

    /* Scan the equipment list */
    for (k = 0, i = INVEN_WIELD; i < INVEN_TOTAL; i++) {
        i_ptr = &inventory[i];

        /* Is this item acceptable? */
        if (!item_tester_okay(i_ptr)) continue;

        /* Description */
        i_ptr->object_desc(i_name, TRUE, 3);

        /* Truncate the description */
        i_name[lim] = 0;

        /* Save the color */
        out_index[k] = i;
        out_color[k] = i_ptr->get_attr();
        (void)strcpy(out_desc[k], i_name);

        /* Extract the maximal length (see below) */
        l = strlen(out_desc[k]) + (2 + 3);

        /* Increase length for labels (if needed) */
        if (show_equip_label) l += (14 + 2);

        /* Increase length for weight (if needed) */
        if (show_equip_weight) l += 9;

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
        i_ptr = &inventory[i];

        /* Clear the line */
        box((col ? (col - 2) : col)*8, (j+1)*16, 639, (j+1)*16+15, COLOR_BLACK);

        /* Prepare an index --(-- */
        sprintf(tmp_val, "%c)", index_to_label(i));

        /* Clear the line with the (possibly indented) index */
        put_string(col*8, (j+1)*16, tmp_val, COLOR_WHITE);

        /* Use labels */
        if (show_equip_label) {
            /* Mention the use */
            (void)sprintf(tmp_val, "%-14s: ", mention_use(i));
            put_string((col+3)*8, (j+1)*16, tmp_val, COLOR_WHITE);

            /* Display the entry itself */
            put_text((col+3+14+2)*8, (j+1)*16, out_desc[j], out_color[j], FONT_BOLD);
        }

        // No labels
        else {
            // Display the entry itself
            put_text((col+3)*8, (j+1)*16, out_desc[j], out_color[16], FONT_BOLD);
        }

        /* Display the weight if needed */
        if (show_equip_weight) {
            int wgt = i_ptr->GetWeight() * i_ptr->GetNumber();
            (void)sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
            put_string(71*8, (j+1)*16, tmp_val, COLOR_WHITE);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && (j < 23)) {
        box((col ? (col - 2) : col)*8, (j+1)*16, 639, (j+1)*16+15, COLOR_BLACK);
    }
}



/*
 * Verify the choice of an item.
 */
static bool verify_ptr(char *prompt, CItem *i_ptr)
{
    char i_name[80];
    char out_val[160];

    /* Describe */
    i_ptr->object_desc(i_name, TRUE, 3);

    /* Prompt */
    sprintf(out_val, "%s %s? ", prompt, i_name);

    /* Query */
    return get_check_old(out_val);
}


/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(int i)
{
    // Illegal items
    if ((i < 0) || (i >= INVEN_TOTAL)) return FALSE;

    // Verify the item
    if (!item_tester_okay(&inventory[i])) return FALSE;

    // Assume okay
    return TRUE;
}



/*
 * Let the user select an item, return its "index"
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.  Note that
 * the equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * If a legal item is selected, we save it in "cp" and return TRUE.
 * If this "legal" item is on the floor, we use a "cp" equal to zero
 * minus the dungeon index of the item on the floor.
 *
 * Otherwise, we return FALSE, and set "cp" to:
 *   -1 for "User hit space/escape"
 *   -2 for "No legal items to choose"
 *
 * "command_wrk" is used to choose between equip/inven listings.
 * If it is TRUE then we are viewing inventory, else equipment.
 *
 * We always erase the prompt when we are done.
 */
bool get_item(int *cp, char *pmt, byte flags)
{
    char n1, n2, which = ' ';
    int k, i1, i2, e1, e2;
    bool done, item, allow_floor = FALSE;
    char tmp_val[160], out_val[160];
    byte *screen = NULL;
    bool command_wrk = FALSE;
    bool command_see = FALSE;


    // Not done
    done = FALSE;

    // No item selected
    item = FALSE;

    // Default to "no item" (see above)
    *cp = -1;
    gi_i_ptr = NULL;


    // Paranoia
    if (!(flags & GI_INVEN) && !(flags & GI_EQUIP)) return FALSE;


    // Full inventory
    i1 = 0;
    i2 = INVEN_PACK - 1;

    // Forbid inventory
    if (!(flags & GI_INVEN)) i2 = -1;

    // Restrict inventory indexes
    while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
    while ((i1 <= i2) && (!get_item_okay(i2))) i2--;


    // Full equipment
    e1 = INVEN_WIELD;
    e2 = INVEN_TOTAL - 1;

    // Forbid equipment
    if (!(flags & GI_EQUIP)) e2 = -1;

    // Restrict equipment indexes
    while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
    while ((e1 <= e2) && (!get_item_okay(e2))) e2--;


    // Hack -- Restrict floor usage
    if (flags & GI_FLOOR) {
        CItem *i_ptr = (p_ptr->get_g_ptr())->i_ptr;

        while (i_ptr) {
            if (item_tester_okay(i_ptr)) {
                allow_floor = TRUE;
                break;
            }
            i_ptr = i_ptr->next_i_ptr;
        }
    }


    /* Verify choices */
    if (!allow_floor && (i1 > i2) && (e1 > e2)) {
        /* Hack -- Nothing to choose */
        *cp = -2;

        /* Done */
        done = TRUE;
    }

    /* Use inventory if allowed */
    else if (flags & GI_INVEN) {
        command_wrk = FALSE;
    }

    /* Use equipment if allowed */
    else if (flags & GI_EQUIP) {
        command_wrk = TRUE;
    }

    /* Use inventory for floor */
    else {
        command_wrk = FALSE;
    }


    /* Repeat until done */
    while (!done) {
        /* Inventory screen */
        if (!command_wrk) {
            /* Extract the legal requests */
            n1 = I2A(i1);
            n2 = I2A(i2);

            /* Redraw if needed */
            if (command_see) show_inven();
        }

        /* Equipment screen */
        else {
            // Extract the legal requests
            n1 = I2A(e1 - INVEN_WIELD);
            n2 = I2A(e2 - INVEN_WIELD);

            // Redraw if needed
            if (command_see) show_equip();
        }

        /* Viewing inventory */
        if (!command_wrk) {
            /* Begin the prompt */
            sprintf(out_val, "Inven:");

            /* Some legal items */
            if (i1 <= i2) {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,",
                        index_to_label(i1), index_to_label(i2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see) strcat(out_val, " * to see,");

            /* Append */
            if (flags & GI_EQUIP) strcat(out_val, " / for Equip,");
        }

        /* Viewing equipment */
        else {
            /* Begin the prompt */
            sprintf(out_val, "Equip:");

            /* Some legal items */
            if (e1 <= e2) {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,",
                        index_to_label(e1), index_to_label(e2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see) strcat(out_val, " * to see,");

            /* Append */
            if (flags & GI_INVEN) strcat(out_val, " / for Inven,");
        }

        /* Indicate legality of the "floor" item */
        if (allow_floor) strcat(out_val, " - for floor,");

        /* Finish the prompt */
        strcat(out_val, " ESC");

        /* Build the prompt */
        sprintf(tmp_val, "(%s) %s", out_val, pmt);

        /* Show the prompt */
        box(0, 0, 639, 15, COLOR_BLACK);
        put_text(0, 0, tmp_val, COLOR_WHITE, FONT_BOLD);


        /* Get a key */
        screen_refresh();
        which = scan_inkey();
        which = convert(which, get_shift(), get_capslock());

        /* Parse it */
        switch (which) {
            case ESCAPE:
                done = TRUE;
                break;

            case '*':
            case '?':
            case ' ':

                // Show/hide the list
                if (!command_see) {
                    if (screen) delete[] screen;
                    screen = save_screen();
                    command_see = TRUE;
                }
                else {
                    restore_screen(screen);
                    command_see = FALSE;
                }
                break;

            case '/':

                // Verify legality
                if (!(flags & GI_INVEN) || !(flags & GI_EQUIP)) {
                    bell();
                    break;
                }

                // Fix screen
                if (command_see) {
                    restore_screen(screen);
                    if (screen) delete[] screen;
                    screen = save_screen();
                }

                // Switch inven/equip
                command_wrk = !command_wrk;

                // Need to redraw
                break;

            case '-':

                /* Use floor item */
                if (allow_floor) {
                    CItem *i_ptr = (p_ptr->get_g_ptr())->i_ptr;
                    for (; i_ptr; i_ptr = i_ptr->next_i_ptr) {
                        // Skip illegal
                        if (!item_tester_okay(i_ptr)) continue;

                        // Skip non-verified items
                        if (!verify_ptr("Try", i_ptr)) continue;

                        // Accept
                        *cp = -6;
                        item = TRUE;
                        done = TRUE;
                        gi_i_ptr = i_ptr;
                    }
                    break;
                }
                else {
                    bell();
                }
                break;

            case '\n':
            case '\r':

                // Choose "default" inventory item
                if (!command_wrk) {
                    k = ((i1 == i2) ? i1 : -1);
                }

                // Choose "default" equipment item
                else {
                    k = ((e1 == e2) ? e1 : -1);
                }

                /* Validate the item */
                if (!get_item_okay(k)) {
                    bell();
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                gi_i_ptr = &inventory[k];
                break;

            default:

                // Extract "query" setting
                which = tolower(which);

                // Convert letter to inventory index
                if (!command_wrk) {
                    k = label_to_inven(which);
                }

                // Convert letter to equipment index
                else {
                    k = label_to_equip(which);
                }

                /* Validate the item */
                if (!get_item_okay(k)) {
                    bell();
                    break;
                }

                // Accept that choice
                *cp = k;
                item = TRUE;
                done = TRUE;
                gi_i_ptr = &inventory[k];
                break;
        }
    }


    /* Fix the screen if necessary */
    if (command_see && screen) restore_screen(screen);
    if (screen) delete[] screen;


    /* Forget the item_tester_tval restriction */
    item_tester_tval = 0;

    /* Forget the item_tester_hook restriction */
    item_tester_hook = NULL;

    /* Clear the prompt line */
    box(0, 0, 639, 15, COLOR_BLACK);

    /* Return TRUE if something was picked */
    return item;
}
