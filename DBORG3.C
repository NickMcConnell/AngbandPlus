/* File: borg3.c */

/* Purpose: Object and Spell routines for the Borg -BEN- */

#include "angband.h"


#ifdef ALLOW_BORG

#include "dborg1.h"
#include "dborg3.h"



/*
 * This file helps the Borg analyze "objects" and "shops", and to
 * deal with objects and spells.
 */



/*
 * Some variables
 */

auto_item *auto_items;      /* Current "inventory" */

auto_shop *auto_shops;      /* Current "shops" */



/*
 * Safety arrays for simulating possible worlds
 */

auto_item *safe_items;      /* Safety "inventory" */
auto_item *safe_home;       /* Safety "home stuff" */

auto_shop *safe_shops;      /* Safety "shops" */


/*
 * Spell info
 */

auto_magic auto_magics[9][9];   /* Spell info, by book/what */


/* Food Names */
static char *food_syllable1[] =
{
    "BBQ ", "Boiled ", "Fresh ", "Frozen ", "Burned ", "Rotten ", "Raw ", "Toasted ", "Broiled ", "Baked ", "Fried ", "Buttered ", "Steamed ", "Gramma's ",
};

/* Food Names */
static char *food_syllable2[] =
{
    "Pizza", "Eggs", "Spam", "Oatmeal", "Chicken", "Bacon", "Peanutbutter", "Roast Beef", "Cheese", "Toast", "Hamburger", "Carrots", "Corn", "Potato", "Pork Chops", "Chinese Takeout", "Cookies",
};

/* Slime Molds */
static char *mold_syllable1[] =
{
    "Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar", "B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et", "Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha", "Ib", "Jer", "K", "Ka", "Ked", "L", "Loth"
, "Lar", "Leg", "M", "Mir", "N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T", "Tr", "Th", "V", "Y", "Z", "W", "Wic",
};

static char *mold_syllable2[] =
{
    "a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra", "ilma", "il-Ga", "ili", "o", "orfi", "u", "y",
};

static char *mold_syllable3[] =
{
    "bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar", "nus", "rin", "ran", "sin", "sil", "sur",
};


/*
 * Hack -- help analyze the magic
 *
 * The comments yield the "name" of the spell or prayer.
 *
 * Also, the leading letter in the comment indicates how we use the
 * spell or prayer, if at all, using "A" for "attack", "D" for "call
 * light" and "detection", "E" for "escape", "H" for healing, "O" for
 * "object manipulation", and "F" for "terrain feature manipulation",
 * plus "!" for entries that can soon be handled.
 */

static byte auto_magic_method[2][9][9] =
{
    /*** Spells ***/

    {
        {
            /* Magic for Beginners (sval 0) */
            BORG_MAGIC_AIM  /*   "Magic Missile" */,
            BORG_MAGIC_EXT  /*   "Detect Monsters" */,
            BORG_MAGIC_NOP  /*   "Detect Gold" */,
            BORG_MAGIC_NOP  /*   "Light Area" */,
            BORG_MAGIC_NOP  /*   "Cure Light Wounds" */,
            BORG_MAGIC_NOP  /*   "Object Detection" */,
            BORG_MAGIC_NOP  /*   "Find Hidden Traps/Doors" */,
            BORG_MAGIC_AIM  /*   "Stinking Cloud" */
        },

        {
            /* Conjurings and Tricks (sval 1) */
            BORG_MAGIC_AIM  /*   "Sleep I" */,
            BORG_MAGIC_AIM  /*   "Trap/Door Destruction" */,
            BORG_MAGIC_AIM  /*   "Light. Bolt" */,
            BORG_MAGIC_NOP  /*   "Slow Poison" */,
            BORG_MAGIC_NOP  /*   "Phase Door" */,
            BORG_MAGIC_AIM  /*   "Spear of Light" */,
            BORG_MAGIC_AIM  /*   "Cone of Cold" */,
            BORG_MAGIC_AIM  /*   "Confuse Monster" */
        },

        {
            /* Incantations and Illusions (sval 2) */
            BORG_MAGIC_AIM  /*   "Striking" */,
            BORG_MAGIC_OBJ  /*   "Identify" */,
            BORG_MAGIC_NOP  /*   "Door Creation" */,
            BORG_MAGIC_OBJ  /*   "Recharge I" */,
            BORG_MAGIC_AIM  /*   "Fire Bolt" */,
            BORG_MAGIC_NOP  /*   "Mass Sleep" */,
            BORG_MAGIC_NOP  /*   "Teleport" */,
            BORG_MAGIC_NOP  /*   "Satisfy Hunger" */,
            BORG_MAGIC_ICK  /*   "(Blank)"*/
        },

        {
            /* Sorcery and Evocations (sval 3) */
            BORG_MAGIC_NOP  /*   "Invisible" */,
            BORG_MAGIC_AIM  /*   "Cold Ball" */,
            BORG_MAGIC_AIM  /*   "Hold Foe" */,
            BORG_MAGIC_NOP  /*   "Haste Self" */,
            BORG_MAGIC_AIM  /*   "Teleport Other" */,
            BORG_MAGIC_NOP  /*   "Word of Dest." */,
            BORG_MAGIC_AIM  /*   "Plasma Storm" */,
            BORG_MAGIC_ICK  /*   "(Blank)"*/,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Resistance of Scarabtarices (sval 4) */
            BORG_MAGIC_NOP  /*   "Resist Fire" */,
            BORG_MAGIC_NOP  /*   "Resist Cold" */,
            BORG_MAGIC_NOP  /*   "Resist Acid" */,
            BORG_MAGIC_NOP  /*   "Resist Poison" */,
            BORG_MAGIC_NOP  /*   "Cure Poison" */,
            BORG_MAGIC_NOP  /*   "Resistance" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Mordenkainen's Escapes (sval 5) */
            BORG_MAGIC_NOP  /*   "Stair Creation" */,
            BORG_MAGIC_NOP  /*   "Teleport II" */,
            BORG_MAGIC_NOP  /*   "Confusion" */,
            BORG_MAGIC_NOP  /*   "Earthquake" */,
            BORG_MAGIC_NOP  /* E "Word of Recall" */,
            BORG_MAGIC_AIM  /*   "Create Wall" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Kelek's Grimoire of Power (sval 6) */
            BORG_MAGIC_NOP  /*   "Detect Enchantment" */,
            BORG_MAGIC_NOP  /*   "Magic Mapping" */,
            BORG_MAGIC_NOP  /*   "Detect Monsters" */,
            BORG_MAGIC_OBJ  /*   "Recharge II" */,
            BORG_MAGIC_AIM  /*   "Mana Bolt" */,
            BORG_MAGIC_NOP  /*   "Scatter Foe"*/,
            BORG_MAGIC_ICK  /*   "(Blank)"*/,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Tenser's transformations... (sval 7) */
            BORG_MAGIC_NOP  /* ! "Heroism" */,
            BORG_MAGIC_AIM  /*   "Slow Monster" */,
            BORG_MAGIC_NOP  /* ! "Shield" */,
            BORG_MAGIC_AIM  /*   "Stone to Mud" */,
            BORG_MAGIC_AIM  /*   "Chaotic Change" */,
            BORG_MAGIC_NOP  /*   "Globe of Resilience" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Raal's Tome of Destruction (sval 8) */
            BORG_MAGIC_AIM  /* A "Acid Bolt" */,
            BORG_MAGIC_AIM  /* A "Acid Ball" */,
            BORG_MAGIC_AIM  /* A "Ice Storm" */,
            BORG_MAGIC_AIM  /* A "Meteor Swarm" */,
            BORG_MAGIC_NOP  /* A "Storm of Dest." */,
            BORG_MAGIC_AIM  /*   "Mana Storm"*/,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        }
    },


    /*** Prayers ***/

    {
        {
            /* Beginners Handbook (sval 0) */
            BORG_MAGIC_EXT  /*   "Detect Evil" */,
            BORG_MAGIC_NOP  /*   "Cure Light Wounds" */,
            BORG_MAGIC_NOP  /*   "Bless" */,
            BORG_MAGIC_NOP  /* H "Remove Fear" */,
            BORG_MAGIC_NOP  /* D "Call Light" */,
            BORG_MAGIC_AIM  /* D "Spiritual Hammer" */,
            BORG_MAGIC_NOP  /* D "Detect Doors/Stairs" */,
            BORG_MAGIC_NOP  /*   "Slow Poison" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Words of Wisdom (sval 1) */
            BORG_MAGIC_AIM  /*   "Scare Creature" */,
            BORG_MAGIC_NOP  /* E "Portal" */,
            BORG_MAGIC_NOP  /* H "Cure Serious Wounds" */,
            BORG_MAGIC_NOP  /*   "Chant" */,
            BORG_MAGIC_NOP  /*   "Sanctuary" */,
            BORG_MAGIC_NOP  /* H "Sense Surroundings" */,
            BORG_MAGIC_NOP  /*   "Remove Curse" */,
            BORG_MAGIC_NOP  /*   "Resist Heat and Cold" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Chants and Blessings (sval 2) */
            BORG_MAGIC_NOP  /*   "Satasfy Hunger" */,
            BORG_MAGIC_AIM  /*   "Orb of Draining" */,
            BORG_MAGIC_NOP  /*   "Cure Critical Wounds" */,
            BORG_MAGIC_EXT  /*   "Sense Invisible" */,
            BORG_MAGIC_NOP  /*   "Protection from Evil" */,
            BORG_MAGIC_NOP  /*   "Earthquake" */,
            BORG_MAGIC_NOP  /*   "Cure Mortal Wounds" */,
            BORG_MAGIC_NOP  /*   "Turn Undead" */,
            BORG_MAGIC_ICK  /*   "(blank)" */

        },

        {
            /* Exorcism and Dispelling (sval 3) */
            BORG_MAGIC_NOP  /*   "Cure Poison" */,
            BORG_MAGIC_NOP  /*   "Prayer" */,
            BORG_MAGIC_NOP  /*   "Dispel Undead" */,
            BORG_MAGIC_AIM  /*   "Confuse Monster" */,
            BORG_MAGIC_NOP  /*   "Heal" */,
            BORG_MAGIC_NOP  /*   "Dispel Evil" */,
            BORG_MAGIC_NOP  /*   "Glyph of Warding" */,
            BORG_MAGIC_NOP  /*   "Holy Word" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Ethereal openings (sval 4) */
            BORG_MAGIC_NOP  /* E "Blink" */,
            BORG_MAGIC_NOP  /* E "Teleport" */,
            BORG_MAGIC_AIM  /*   "Teleport Away" */,
            BORG_MAGIC_AIM  /*   "Create Door" */,
            BORG_MAGIC_NOP  /* E "Word of Recall" */,
            BORG_MAGIC_NOP  /*   "Alter Reality" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Godly Insights... (sval 5) */
            BORG_MAGIC_EXT  /*   "Detect Monsters" */,
            BORG_MAGIC_EXT  /* D "Detection" */,
            BORG_MAGIC_OBJ  /* O "Perception" */,
            BORG_MAGIC_NOP  /*   "Probing" */,
            BORG_MAGIC_NOP  /* D "Self Knowledge" */,
            BORG_MAGIC_NOP  /*   "Clairvoyance" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Purifications and Healing (sval 6) */
            BORG_MAGIC_NOP  /* H "Cure Serious Wounds" */,
            BORG_MAGIC_NOP  /* H "Cure Mortal Wounds" */,
            BORG_MAGIC_NOP  /* H "Healing" */,
            BORG_MAGIC_NOP  /* ! "Armour of Light" */,
            BORG_MAGIC_NOP  /* ! "Restoration" */,
            BORG_MAGIC_NOP  /*   "Remembrance" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Holy Infusions (sval 7) */
            BORG_MAGIC_NOP  /* F "Unbarring Ways" */,
            BORG_MAGIC_OBJ  /* O "Recharging" */,
            BORG_MAGIC_NOP  /*   "Dispel Curse" */,
            BORG_MAGIC_OBJ  /* O "Enchant Weapon" */,
            BORG_MAGIC_OBJ  /* O "Enchant Armour" */,
            BORG_MAGIC_NOP  /*   "Elemental Brand" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        },

        {
            /* Wrath of God (sval 8) */
            BORG_MAGIC_NOP  /* ! "Dispel Undead" */,
            BORG_MAGIC_NOP  /* ! "Dispel Evil" */,
            BORG_MAGIC_NOP  /*   "Banishment" */,
            BORG_MAGIC_NOP  /*   "Word of Destruction" */,
            BORG_MAGIC_AIM  /*   "Divine Intervention" */,
            BORG_MAGIC_AIM  /*   "Annihilation" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */,
            BORG_MAGIC_ICK  /*   "(blank)" */
        }
    }
};



/*
 * Hack -- help analyze the magic
 *
 * The comments yield the "name" of the spell or prayer.
 *
 * Also, the leading letter in the comment indicates how we use the
 * spell or prayer, if at all, using "A" for "attack", "D" for "call
 * light" and "detection", "E" for "escape", "H" for healing, "O" for
 * "object manipulation", "F" for "terrain feature manipulation",
 * "X" for "never use this", and "!" for "soon to be handled".
 *
 * The value indicates how much we want to know the spell/prayer.  A
 * rating of zero indicates that the spell/prayer is useless, and should
 * never be learned or used.  A rating from 1 to 49 indicates that the
 * spell/prayer is worth some experience to use once, so we should study
 * (and use) it when we get bored in town.  A rating from 50 to 99 means
 * that the spell/prayer should be learned as soon as possible (and used
 * when bored).
 *
 * XXX XXX XXX Verify ratings.
 */

static byte auto_magic_rating[2][9][9] =
{
    /*** Spells ***/

    {
        {
            /* Magic for Beginners (sval 0) */
            85          /* A "Magic Missile" */,
            55          /*   "Detect Monsters" */,
            55          /* E "Detect Gold" */,
            75          /* D "Light Area" */,
            65          /*   "Cure Light Wounds" */,
            5           /*   "Object Detection" */,
            95          /* D "Find Hidden Traps/Doors" */,
            85          /* A "Stinking Cloud" */
        },

        {
            /* Conjurings and Tricks (sval 1) */
            55           /*   "Sleep I" */,
            55          /* A "Trap/door Dest" */,
            75          /* F "Lightning Bolt" */,
            65          /*   "Slow Poison" */,
            95          /* H "Phase Door" */,
            55          /* A "Spear of Light" */,
            85          /* A "Cone of Cold" */,
            75          /* F "Confuse Monster" */
        },

        {
            /* Incantations and Illusions (sval 2) */
            95          /* H "Striking" */,
            96          /* O "Identify" */,
            65          /*   "Door Create" */,
            55          /*   "Recharge I" */,
            75          /* O "Fire Bolt" */,
            65          /*   "Mass Sleep " */,
            85          /* A "Teleport" */,
            75          /*   "Satisfy Hunger" */,
            55          /*   "blank" */
        },

        {
            /* Sorcery and Evocations (sval 3) */
            85          /* A "Inviso" */,
            75          /* O "Cold Ball" */,
            55          /*   "Hold Foe" */,
            75          /*   "Haste Self" */,
            85          /* A "Teleport Other" */,
            55          /*   "Word of Destruction" */,
            55          /*   "Plasma Storm" */,
            0           /*   "blank" */,
            0           /*   "(blank)" */
        },

        {
            /* Resistance of Scarabtarices (sval 4) */
            70          /*   "Resist Fire" */,
            65          /*   "Resist Cold" */,
            60          /*   "Resist Acid" */,
            70          /*   "Resist Poison" */,
            70          /*   "Cure Poison" */,
            75          /*   "Resistance" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Mordenkainen's Escapes (sval 5) */
            65          /*   "Stair Creation" */,
            75          /*   "Teleport" */,
            65          /*   "Confusion" */,
            65          /*   "Earthquake" */,
            75          /* E "Word of Recall" */,
            75          /*   "Create Wall" */,
            0           /*   "Teleport Level (soon)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Kelek's Grimoire of Power (sval 6) */
            5           /*   "Detect Evnchantment" */,
            65          /*   "Magic Mapping" */,
            55          /* O "Detect Monsters" */,
            55          /*   "Recharge II" */,
            65          /*   "Mana Bolt" */,
            65          /*   "Scatter Bolt" */,
            0           /*   "blank" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Tenser's transformations... (sval 7) */
            75          /* H "Heroism" */,
            75          /*   "Slow Monster" */,
            75          /* H "Shield" */,
            75          /*   "Stone to Mud" */,
            75          /*   "Chaotic Change" */,
            75          /*   "Globe of Resilience" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Raal's Tome of Destruction (sval 8) */
            85          /* A "Acid Bolt" */,
            85          /* A "Acid Ball" */,
            85          /* A "Ice Storm" */,
            85          /* A "Meteor Swarm" */,
            85          /* A "Storm of Dest" */,
            85          /* A "Mana Storm" */,
            0           /*   "blank" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        }
    },


    /*** Prayers ***/

    {
        {
            /* Beginners Handbook (sval 0) */
            55          /*   "Detect Evil" */,
            55          /* H "Cure Light Wounds" */,
            85          /*   "Bless" */,
            35          /* H "Remove Fear" */,
            35          /* D "Call Light" */,
            75          /* D "Spiritual Hammer" */,
            75          /* D "Detect Doors/Stairs" */,
            55          /*   "Slow Poison" */,
            0           /*   "(blank)" */
        },

        {
            /* Words of Wisdom (sval 1) */
            55           /*   "Scare Creature" */,
            95          /* E "Portal" */,
            55           /* H "Cure Serious Wounds" */,
            55           /*   "Chant" */,
            55           /*   "Sanctuary" */,
            95          /* H "Sence Surroundings " */,
            55           /*   "Remove Curse" */,
            55           /*   "Resist Heat and Cold" */,
            0           /*   "(blank)" */
        },

        {
            /* Chants and Blessings (sval 2) */
            85          /* H "Satisfy Hunger" */,
            90          /* A "Orb of Draining" */,
            55          /* H "Cure Critical Wounds" */,
            65          /*   "Sense Invisible" */,
            65          /*   "Protection from Evil" */,
            55          /*   "Earthquake" */,
            65          /* D "Cure Mortal Wounds" */,
            55          /* H "Turn Undead" */,
            0           /*   "" */
        },

        {
            /* Exorcism and Dispelling (sval 3) */
            55          /*   "Cure Poison" */,
            65          /* ! "Prayer" */,
            55          /* H "Dispel Undead" */,
            55          /* ! "Confuse Monster" */,
            55          /*   "Heal" */,
            55          /* ! "Dispel Evil" */,
            55          /*   "Glyph of warding" */,
            55          /*   "Holy Word" */,
            0           /*   "(blank)" */
        },

        {
            /* Ethereal openings (sval 4) */
            65          /* E "Blink" */,
            65          /* E "Teleport" */,
            55          /*   "Teleport Away" */,
            55          /*   "Create Door" */,
            75          /* E "Word of Recall" */,
            65          /*   "Alter Reality" */,
            0           /*   "Teleport Level(soon)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Godly Insights... (sval 5) */
            55          /*   "Detect Monsters" */,
            65          /* D "Detection" */,
            75          /* O "Perception" */,
            5           /*   "Probing" */,
            5           /* D "Self Knowlege" */,
            65           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Purifications and Healing (sval 6) */
            55          /* H "Cure Serious Wounds" */,
            55          /* H "Cure Mortal Wounds" */,
            55          /* H "Healing" */,
            55          /* ! "Armour of Light" */,
            55          /* ! "Restoration" */,
            55          /* ! "Remembrance" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Holy Infusions (sval 7) */
            50          /* F "Unbarring Ways" */,
            50          /* O "Recharging" */,
            55          /*   "Dispel Curse" */,
            50          /* O "Enchant Weapon" */,
            50          /* O "Enchant Armour" */,
            50          /*   "Elemental Brand" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        },

        {
            /* Wrath of God (sval 8) */
            65           /* ! "Dispel Undead" */,
            65           /* ! "Dispel Evil" */,
            55          /*   "Banishment" */,
            55          /*  "Word of Destruction" */,
            55          /*   "Divine Intervention" */,
            55          /*   "Annihilation" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */,
            0           /*   "(blank)" */
        }
    }
};


/*
 * Constant "item description parsers" (singles)
 */
static int auto_single_size;        /* Number of "singles" */
static s16b *auto_single_what;      /* Kind indexes for "singles" */
static cptr *auto_single_text;      /* Textual prefixes for "singles" */

/*
 * Constant "item description parsers" (plurals)
 */
static int auto_plural_size;        /* Number of "plurals" */
static s16b *auto_plural_what;      /* Kind index for "plurals" */
static cptr *auto_plural_text;      /* Textual prefixes for "plurals" */
static cptr *auto_sv_plural_text;   /* Save Textual prefixes for "plurals" (in kind order) */

/*
 * Constant "item description parsers" (suffixes)
 */
static int auto_artego_size;        /* Number of "artegos" */
static s16b *auto_artego_what;      /* Indexes for "artegos" */
static cptr *auto_artego_text;      /* Textual prefixes for "artegos" */
static cptr *auto_sv_art_text;      /* Save textual prefixes for "artifacts" (in kind order) */


/*
 * Return the slot that items of the given type are wielded into
 *
 * Note that "rings" are now automatically wielded into the left hand
 *
 * Returns "-1" if the item cannot (or should not) be wielded
 */
int borg_wield_slot(auto_item *item)
{
    int i;


    /* Slot for equipment */
    if (auto_race < RACE_MIN_DRAGON)
    {
        switch (item->tval)
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
#if 0
               if (!inventory[INVEN_RIGHT].k_idx) return (INVEN_RIGHT);
#endif
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
        };
    }

    /* The character is a DRAGON! */
    else
    {
        switch (item->tval)
        {
            case TV_SHOT: case TV_BOLT: case TV_ARROW:
            {
                return (-1);
            }

            case TV_BOW: case TV_DIGGING: case TV_HAFTED:
            case TV_POLEARM: case TV_SWORD: case TV_HELM:
            case TV_BOOTS: case TV_GLOVES: case TV_SHIELD:
            case TV_DRAG_ARMOR: case TV_HARD_ARMOR:
            case TV_SOFT_ARMOR:
            {
                    return (-1);
            }

            case TV_LITE:
            {
                return (INVEN_D_LITE);
            }

            case TV_CLOAK:
            {
                return (INVEN_D_OUTER);
            }

            case TV_CROWN:
            {
                return (INVEN_D_HEAD);
            }

            case TV_AMULET:
            {
                return (INVEN_D_NECK);
            }

            case TV_RING:
            {
                /* Go in order */
                for (i=INVEN_D_RING1; i <= INVEN_D_RING6; i++)
                {
                    if (!auto_items[i].tval) return (i);
                }

                /* Hack -- Special case to wear rings.  See borg7.c */
                return (99);

            }
        };
    }

    /* No slot available */
    return (-1);

}

/*
 * Get the *ID information
 *
 * This function pulls the information from the screen if it is not passed
 * a *real* item.  It is only passed in *real* items if the borg is allowed
 * to 'cheat' for inventory.
 * This function returns TRUE if space needs to be pressed
 */
bool borg_object_star_id_aux(auto_item *borg_item, object_type *real_item)
{
    u32b f1, f2, f3;

    /* If a real item pointer is passed in then we are cheating to get */
    /* the data directly from the real item    */
    if (real_item)
    {
        object_flags(real_item, &f1, &f2, &f3);
    }

#ifndef BORG_TK
    else
    {
        int i;

        byte t_a;

        char buf[71];

        for (i = 2; i < 22; i++)
        {
            if (!(0 == borg_what_text(15, i, 65, &t_a, buf)))
            {
                /* bummer, problem reading from screen */
                borg_oops("cannot read from screen");
                return FALSE;
            }

            /* Gives Stat Bonus */
            if (prefix(buf, "It affects your strength."))
            {
                f1 &= TR1_STR;
                continue;
            }
            if (prefix(buf, "It affects your intelligence."))
            {
                f1 &= TR1_INT;
                continue;
            }
            if (prefix(buf, "It affects your wisdom."))
            {
                f1 &= TR1_WIS;
                continue;
            }
            if (prefix(buf, "It affects your dexterity."))
            {
                f1 &= TR1_DEX;
                continue;
            }
            if (prefix(buf, "It affects your constitution."))
            {
                f1 &= TR1_CON;
                continue;
            }
            if (prefix(buf, "It affects your charisma."))
            {
                f1 &= TR1_CHR;
                continue;
            }
            /* Gives stealth Bonus */
            if (prefix(buf, "It affects your stealth."))
            {
                f1 &= TR1_STEALTH;
                continue;
            }


            /* Gives searching Bonus */
            if (prefix(buf, "It affects your searching."))
            {
                f1 &= TR1_SEARCH;
                continue;
            }
            /* Gives Infravision Bonus */
            if (prefix(buf, "It affects your infravision."))
            {
                f1 &= TR1_INFRA;
                continue;
            }
            /* Gives digging Bonus */
            if (prefix(buf, "It affects your ability to tunnel."))
            {
                f1 &= TR1_TUNNEL;
                continue;
            }
            /* Gives Speed Bonus  (Wee!) */
            if (prefix(buf, "It affects your speed."))
            {
                f1 &= TR1_SPEED;
                continue;
            }
            /* Gives Extra Blows */
            if (prefix(buf, "It affects your attack speed."))
            {
#if 0
                f1 &= TR1_BLOWS;
#endif
                continue;
            }
            /* Various Brands */
            if (prefix(buf, "It does extra damage from acid."))
            {
                f1 &= TR1_BRAND_ACID;
                continue;
            }
            if (prefix(buf, "It does extra damage from electricity."))
            {
                f1 &= TR1_BRAND_ELEC;
                continue;
            }
            if (prefix(buf, "It does extra damage from fire."))
            {
                f1 &= TR1_BRAND_FIRE;
                continue;
            }
            if (prefix(buf, "It does extra damage from frost."))
            {
                f1 &= TR1_BRAND_COLD;
                continue;
            }
            /* This is Grond. */
            if (prefix(buf, "It can cause earthquakes."))
            {
                f3 &= TR3_IMPACT;
                continue;
            }
            /* *SLAY* Dragon */
            if (prefix(buf, "It is a great bane of dragons."))
            {
                f1 &= TR1_KILL_DRAGON;
                continue;
            }
            /* Various Slays  */
            if (prefix(buf, "It is especially deadly against dragons."))
            {
                f1 &= TR1_SLAY_DRAGON;
                continue;
            }
            if (prefix(buf, "It is especially deadly against orcs."))
            {
                f1 &= TR1_SLAY_ORC;
                continue;
            }
            if (prefix(buf, "It is especially deadly against trolls."))
            {
                f1 &= TR1_SLAY_TROLL;
                continue;
            }
            if (prefix(buf, "It is especially deadly against giants."))
            {
                f1 &= TR1_SLAY_GIANT;
                continue;
            }
            if (prefix(buf, "It strikes at demons with holy wrath."))
            {
                f1 &= TR1_SLAY_DEMON;
                continue;
            }
            if (prefix(buf, "It strikes at undead with holy wrath."))
            {
                f1 &= TR1_SLAY_UNDEAD;
                continue;
            }
            if (prefix(buf, "It fights against evil with holy fury."))
            {
                f1 &= TR1_SLAY_EVIL;
                continue;
            }
            if (prefix(buf, "It is especially deadly against natural creatures."))
            {
                f1 &= TR1_SLAY_ANIMAL;
                continue;
            }

            /* Various Sustains */
            if (prefix(buf, "It sustains your strength."))
            {
                f2 &= TR2_SUST_STR;
                continue;
            }
            if (prefix(buf, "It sustains your intelligence."))
            {
                f2 &= TR2_SUST_INT;
                continue;
            }
            if (prefix(buf, "It sustains your wisdom."))
            {
                f2 &= TR2_SUST_WIS;
                continue;
            }
            if (prefix(buf, "It sustains your dexterity."))
            {
                f2 &= TR2_SUST_DEX;
                continue;
            }
            if (prefix(buf, "It sustains your constitution."))
            {
                f2 &= TR2_SUST_CON;
                continue;
            }
            if (prefix(buf, "It sustains your charisma."))
            {
                f2 &= TR2_SUST_CHR;
                continue;
            }
            /* Various immunities */
            if (prefix(buf, "It provides immunity to acid."))
            {
                f2 &= TR2_IM_ACID;
                continue;
            }
            if (prefix(buf, "It provides immunity to electricity."))
            {
                f2 &= TR2_IM_ELEC;
                continue;
            }
            if (prefix(buf, "It provides immunity to fire."))
            {
                f2 &= TR2_IM_FIRE;
                continue;
            }
            if (prefix(buf, "It provides immunity to cold."))
            {
                f2 &= TR2_IM_COLD;
                continue;
            }
            /* Free Action */
            if (prefix(buf, "It provides immunity to paralysis."))
            {
                f3 &= TR3_FREE_ACT;
                continue;
            }
            /* Hold Life */
            if (prefix(buf, "It provides resistance to life draining."))
            {
                f3 &= TR3_HOLD_LIFE;
                continue;
            }
            /* Resists */
            if (prefix(buf, "It provides resistance to acid."))
            {
                f2 &= TR2_RES_ACID;
                continue;
            }
            if (prefix(buf, "It provides resistance to electricity."))
            {
                f2 &= TR2_RES_ELEC;
                continue;
            }
            if (prefix(buf, "It provides resistance to fire."))
            {
                f2 &= TR2_RES_FIRE;
                continue;
            }
            if (prefix(buf, "It provides resistance to cold."))
            {
                f2 &= TR2_RES_COLD;
                continue;
            }
            if (prefix(buf, "It provides resistance to poison."))
            {
                f2 &= TR2_RES_POIS;
                continue;
            }

            if (prefix(buf, "It provides resistance to light."))
            {
                f2 &= TR2_RES_LITE;
                continue;
            }
            if (prefix(buf, "It provides resistance to dark."))
            {
                f2 &= TR2_RES_DARK;
                continue;
            }
            if (prefix(buf, "It provides resistance to blindness."))
            {
                f2 &= TR2_RES_BLIND;
                continue;
            }
            if (prefix(buf, "It provides resistance to confusion."))
            {
                f2 &= TR2_RES_CONFU;
                continue;
            }
            if (prefix(buf, "It provides resistance to sound."))
            {
                f2 &= TR2_RES_SOUND;
                continue;
            }
            if (prefix(buf, "It provides resistance to shards."))
            {
                f2 &= TR2_RES_SHARD;
                continue;
            }
            if (prefix(buf, "It provides resistance to nether."))
            {
                f2 &= TR2_RES_NETHR;
                continue;
            }
            if (prefix(buf, "It provides resistance to nexus."))
            {
                f2 &= TR2_RES_NEXUS;
                continue;
            }
            if (prefix(buf, "It provides resistance to chaos."))
            {
                f2 &= TR2_RES_CHAOS;
                continue;
            }
            if (prefix(buf, "It provides resistance to disenchantment."))
            {
                f2 &= TR2_RES_DISEN;
                continue;
            }
            /* Feather Fall */
            if (prefix(buf, "It induces feather falling."))
            {
                f3 &= TR3_FEATHER;
                continue;
            }
            /* It Glows! */
            if (prefix(buf, "It provides permanent light."))
            {
                f3 &= TR3_LITE;
                continue;
            }
            /* See Invisible */
            if (prefix(buf, "It allows you to see invisible monsters."))
            {
                f3 &= TR3_SEE_INVIS;
                continue;
            }
            /* ESP */
            if (prefix(buf, "It gives telepathic powers."))
            {
                f3 &= TR3_TELEPATHY;
                continue;
            }
            /* Slow Digestion */
            if (prefix(buf, "It slows your metabolism."))
            {
                f3 &= TR3_SLOW_DIGEST;
                continue;
            }
            /* Regenerate */
            if (prefix(buf, "It speeds your regenerative powers."))
            {
                f3 &= TR3_REGEN;
                continue;
            }
            /* Extra Mult for Missle Weapons */
            if (prefix(buf, "It fires missiles with extra might."))
            {
                f1 &= TR1_MIGHT;
                continue;
            }
            /* Extra Shots */
            if (prefix(buf, "It fires missiles excessively fast."))
            {
                f1 &= TR1_SHOTS;
                continue;
            }
            /* The One Ring! */
            if (prefix(buf, "It drains experience."))
            {
                f3 &= TR3_DRAIN_EXP;
                continue;
            }
            /* Teleports (cursed) */
            if (prefix(buf, "It induces random teleportation."))
            {
                f3 &= TR3_TELEPORT;
                continue;
            }
            /* Aggravate */
            if (prefix(buf, "It aggravates nearby creatures."))
            {
                f3 &= TR3_AGGRAVATE;
                continue;
            }
            /* Can be used by priests */
            if (prefix(buf, "It has been blessed by the gods."))
            {
                f3 &= TR3_BLESSED;
                continue;
            }
            /* Perma-curse */
            if (prefix(buf, "It is permanently cursed."))
            {
                f3 &= TR3_PERMA_CURSE;
                continue;
            }
            /* Regualar old curse */
            if (prefix(buf, "It is cursed."))
            {
                f3 &= TR3_LIGHT_CURSE;
                continue;
            }
            /* Item imunity */
            if (prefix(buf, "It cannot be harmed by acid."))
            {
                f3 &= TR3_IGNORE_ACID;
                continue;
            }
            if (prefix(buf, "It cannot be harmed by electricity."))
            {
                f3 &= TR3_IGNORE_ELEC;
                continue;
            }
            if (prefix(buf, "It cannot be harmed by fire."))
            {
                f3 &= TR3_IGNORE_FIRE;
                continue;
            }
            if (prefix(buf, "It cannot be harmed by cold."))
            {
                f3 &= TR3_IGNORE_COLD;
                continue;
            }
            /* press space to go to next screen. */
            if (prefix(buf, "-- more --"))
            {
                return (TRUE);
            }

        }
    }
#endif /* not BORG_TK */

    borg_item->flags1 = f1;
    borg_item->flags2 = f2;
    borg_item->flags3 = f3;

    borg_item->needs_I = FALSE;




    return (FALSE);
}

/*
 * Look for an item that needs to be analysed because it has been *ID*d
 *
 * This will go through inventory and look for items that were just*ID*'d
 * and examine them for their bonuses.
 */
bool borg_object_star_id( void )
{
    int i;

    /* look in inventory and equiptment for something to *id* */
    for (i = 0; i < INVEN_TOTAL; i++)
    {

        auto_item *item = &auto_items[i];

        if (auto_items[i].needs_I)
        {
            if ((auto_cheat_equip && i >= INVEN_WIELD) ||
                (auto_cheat_inven && i < INVEN_WIELD) )
            {
                /* cheat to get the information. */
                borg_object_star_id_aux( &auto_items[i], &inventory[i]);
            }
#ifndef BORG_TK
            else
            {
                byte t_a;

                char buf[10];

                /* Check to see if we are looking at the 'I' screen. */
                if (!(0 == borg_what_text(0, 0, 9, &t_a, buf) &&
                    (buf[0] == 'E') &
                    (buf[1] == 'x') &
                    (buf[2] == 'a') &
                    (buf[3] == 'm') &
                    (buf[4] == 'i') &
                    (buf[5] == 'n') &
                    (buf[6] == 'i') &
                    (buf[7] == 'n') &
                    (buf[8] == 'g')) )
                {
                    borg_keypress('I');
                    if (i < INVEN_WIELD)
                    {
                        borg_keypress(I2A(i));
                    }
                    else
                    {
                        borg_keypress('/');
                        borg_keypress(I2A(i - INVEN_WIELD));
                    }
                    return (TRUE);
                }

                /* Look at the screen to get the information */
                if (borg_object_star_id_aux( &auto_items[i], 0 ))
                {
                    borg_keypress(' ');
                    return (TRUE);
                }

                /* Get rid of the *ID* screen */
                borg_keypress(' ');
                borg_keypress(' ');
                borg_keypress(' ');
                return (TRUE);
            }
#endif /* BORG_TK */

            /* inscribe certain objects */

            if (!auto_depth && (item->name1 || item->name2 == EGO_ELVENKIND || item->name2 == EGO_PERMANENCE ||
                item->name2 == EGO_AMAN  || item->name2 == EGO_MAGI || (item->flags3 & TR3_BLESSED)) &&
                (streq(item->note, "{ }")  || streq(item->note, "")  || streq(item->note, "{uncursed}")))
            {

                /* make the inscription */
                borg_keypress('{');

                if (i >= INVEN_WIELD)
                {
                borg_keypress('/');
                borg_keypress(I2A(i - INVEN_WIELD));
                }
                else
                {
                borg_keypress(I2A(i));
                }

                if (item->flags1 & TR1_SPEED)
                {
                    borg_keypresses("Spd");
                }
                /* slays and immunities */
                if (item->flags2 & TR2_RES_POIS)
                {
                borg_keypresses("Poisn");
                }
                if (item->flags2 & TR2_IM_FIRE)
                {
                borg_keypresses("IFir");
                }
                if (item->flags2 & TR2_IM_COLD)
                {
                borg_keypresses("ICld");
                }
                if (item->flags2 & TR2_IM_ACID)
                {
                borg_keypresses("IAcd");
                }
                if (item->flags2 & TR2_IM_ELEC)
                {
                borg_keypresses("IElc");
                }
                if (item->flags2 & TR2_RES_LITE)
                {
                    borg_keypresses("Lite");
                }
                if (item->flags2 & TR2_RES_DARK)
                {
                    borg_keypresses("Dark");
                }
                if (item->flags2 & TR2_RES_BLIND)
                {
                    borg_keypresses("Blnd");
                }
                if (item->flags2 & TR2_RES_CONFU)
                {
                    borg_keypresses("Conf");
                }
                if (item->flags2 & TR2_RES_SOUND)
                {
                    borg_keypresses("Sound");
                }
                if (item->flags2 & TR2_RES_SHARD)
                {
                    borg_keypresses("Shrd");
                }
                if (item->flags2 & TR2_RES_NETHR)
                {
                    borg_keypresses("Nthr");
                }
                if (item->flags2 & TR2_RES_NEXUS)
                {
                    borg_keypresses("Nxs");
                }
                if (item->flags2 & TR2_RES_CHAOS)
                {
                    borg_keypresses("Chaos");
                }
                if (item->flags2 & TR2_RES_DISEN)
                {
                    borg_keypresses("Disn");
                }
                if (item->flags3 & TR3_ACTIVATE)
                {
                    borg_keypresses("Actv");
                }
                if (item->flags3 & TR3_TELEPATHY)
                {
                    borg_keypresses("ESP");
                }
                if (item->flags3 & TR3_HOLD_LIFE)
                {
                    borg_keypresses("HL");
                }
                if (item->flags3 & TR3_FREE_ACT)
                {
                    borg_keypresses("FA");
                }
                if (item->flags3 & TR3_SEE_INVIS)
                {
                    borg_keypresses("SInv");
                }

                /* end the inscription */
                borg_keypress('\n');

            }

        }

    }
    return (FALSE);
}



/*
 * Determine the "base price" of a known item (see below)
 *
 * This function is adapted from "object_value_known()".
 *
 * This routine is called only by "borg_item_analyze()", which
 * uses this function to guess at the "value" of an item, if it
 * was to be sold to a store, with perfect "charisma" modifiers.
 */
static s32b borg_object_value_known(auto_item *item)
{
    s32b value;


    object_kind *k_ptr = &k_info[item->kind];

    /* Worthless items */
    if (!k_ptr->cost) return (0L);

    /* Extract the base value */
    value = k_ptr->cost;


    /* Hack -- use artifact base costs */
    if (item->name1)
    {
        artifact_type *a_ptr;

        /* Randarts */
        if (item->name1 == ART_RANDART)
        {
            /* Hack -- Value is plugged in a bit later */
        }
        else
        {
            a_ptr = &a_info[item->name1];

            /* Hack -- "worthless" artifacts */
            if (!a_ptr->cost) return (0L);

            /* Hack -- Use the artifact cost instead */
            value = a_ptr->cost;
        }

    }



    /* Hack -- add in ego-item bonus cost */
    if (item->name2)
    {
        ego_item_type *e_ptr = &e_info[item->name2];

        /* Worthless ego-items */
        if (!e_ptr->cost) return (0L);

        /* Hack -- reward the ego-item cost */
        value += e_ptr->cost;
    }


    /* Analyze pval bonus */
    switch (item->tval)
    {
        /* Wands/Staffs */
        case TV_WAND:
        case TV_STAFF:
        {
            /* Pay extra for charges */
            value += ((value / 20) * item->pval);

            break;
        }

        /* Wearable items */
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_LITE:
        case TV_AMULET:
        case TV_RING:
        {
            /* Hack -- Negative "pval" is always bad */
            if (item->pval < 0) return (0L);

            /* No pval */
            if (!item->pval) break;

            /* Give credit for stat bonuses */
            if (item->flags1 & TR1_STR) value += (item->pval * 200L);
            if (item->flags1 & TR1_INT) value += (item->pval * 200L);
            if (item->flags1 & TR1_WIS) value += (item->pval * 200L);
            if (item->flags1 & TR1_DEX) value += (item->pval * 200L);
            if (item->flags1 & TR1_CON) value += (item->pval * 200L);
            if (item->flags1 & TR1_CHR) value += (item->pval * 200L);

            /* Give credit for stealth and searching */
            if (item->flags1 & TR1_STEALTH) value += (item->pval * 100L);
            if (item->flags1 & TR1_SEARCH) value += (item->pval * 100L);

            /* Give credit for infra-vision and tunneling */
            if (item->flags1 & TR1_INFRA) value += (item->pval * 50L);
            if (item->flags1 & TR1_TUNNEL) value += (item->pval * 50L);

            /* Give credit for extra attacks */
#if 0
            if (item->flags1 & TR1_BLOWS) value += (item->pval * 2000L);
#endif
            /* Give credit for speed bonus */
            if (item->flags1 & TR1_SPEED) value += (item->pval * 30000L);

            break;
        }
    }


    /* Analyze the item */
    switch (item->tval)
    {
        /* Rings/Amulets */
        case TV_RING:
        case TV_AMULET:
        {
            /* Hack -- negative bonuses are bad */
            if (item->to_a < 0) return (0L);
            if (item->to_h < 0) return (0L);
            if (item->to_d < 0) return (0L);

            /* Give credit for bonuses */
            value += ((item->to_h + item->to_d + item->to_a) * 100L);

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
            /* Hack -- negative armor bonus */
            if (item->to_a < 0) return (0L);

            /* Give credit for bonuses */
            value += ((item->to_h + item->to_d + item->to_a) * 100L);

            break;
        }

        /* Bows/Weapons */
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_SWORD:
        case TV_POLEARM:
        {
            /* Hack -- negative hit/damage bonuses */
            if (item->to_h + item->to_d < 0) return (0L);

            /* Factor in the bonuses */
            value += ((item->to_h + item->to_d + item->to_a) * 100L);

            /* Hack -- Factor in extra damage dice */
            if ((item->dd > k_ptr->dd) && (item->ds == k_ptr->ds))
            {
                value += (item->dd - k_ptr->dd) * item->ds * 200L;
            }

            break;
        }

        /* Ammo */
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            /* Hack -- negative hit/damage bonuses */
            if (item->to_h + item->to_d < 0) return (0L);

            /* Factor in the bonuses */
            value += ((item->to_h + item->to_d) * 5L);

            /* Hack -- Factor in extra damage dice */
            if ((item->dd > k_ptr->dd) && (item->ds == k_ptr->ds))
            {
                value += (item->dd - k_ptr->dd) * item->ds * 5L;
            }

            break;
        }
    }


    /* Return the value */
    return (value);
}


/*
 * Analyze an item given a description and (optional) cost
 *
 * From the description, extract the item identity, and the various
 * bonuses, plus the "aware" and "known" flags (in an encoded state).
 *
 * Note the use of a "prefix binary search" on the arrays of object
 * base names, and on the arrays of artifact/ego-item special names.
 *
 * A "prefix binary search" starts out just like a normal binary search,
 * in that it searches a sorted array of entries for a specific entry,
 * using a simple "less than or equal to" comparison.  When it finds
 * an entry, however, instead of simply checking for "equality" of the
 * entry to the key, it must check whether the key is a "prefix" of the
 * entry.  And if any entry can be a prefix of another entry, then it
 * must check whether the key is a "prefix" of any of the entries which
 * precede the "found" entry.  Technically, it only has to check the
 * preceding N entries, where N is the maximum distance between any two
 * entries sharing a "prefix" relation, but note that only in the case
 * of "failure" will the search need to check more than a few entries,
 * even if it scans all the way to the start of the list.
 *
 * We use the object kind to guess at the object weight and flags, and
 * then we use the artifact/ego-item information to update our guesses.
 *
 * We also guess at the value of the item, as given by "object_value()".
 *
 * Note that we will fail if the "description" was "partial", that is,
 * if it was "broken" by the display functions for any reason.  This
 * should only be an issue in "stores", which "chop" the description
 * to a length of about 60 characters, which may be "messy".  Luckily,
 * objects in stores never have important inscriptions, and we should
 * correctly handle objects with "bizarre" inscriptions, or even with
 * "broken" inscriptions, so we should be okay.
 */
void borg_item_analyze(auto_item *item, object_type *real_item, cptr desc)
{
    int i, m, n;

    int d1 = 0;
    int d2 = 0;
    int ac = 0;
    int th = 0;
    int td = 0;
    int ta = 0;

    bool done = FALSE;

    char *scan;
    char *tail;

    char temp[128];


    /* Wipe the item */
    WIPE(item, auto_item);


    /* Save the item description */
    strcpy(item->desc, desc);


    /* Advance to the "inscription" or end of string */
    for (scan = item->desc; *scan && (*scan != c1); scan++) /* loop */;

    /* Save a pointer to the inscription */
    item->note = scan;

    /* Empty item */
    if (!desc[0]) return;


    /* Assume singular */
    item->iqty = 1;

    /* Notice prefix "a " */
    if ((desc[0] == 'a') && (desc[1] == ' '))
    {
        /* Skip "a " */
        desc += 2;
    }

    /* Notice prefix "a " */
    else if ((desc[0] == 'a') && (desc[1] == 'n') && (desc[2] == ' '))
    {
        /* Skip "an " */
        desc += 3;
    }

    /* Notice prefix "The " */
    else if ((desc[0] == 'T') && (desc[1] == 'h') &&
             (desc[2] == 'e') && (desc[3] == ' '))
    {
        /* Skip "The " */
        desc += 4;

        /* hack-- he loops on randart phials */
        /* if 'The' is known, then it is ID'd */
        item->able = TRUE;
    }

    /* Notice "numerical" prefixes */
    else if (isdigit(desc[0]))
    {
        cptr s;

        /* Find the first space */
        for (s = desc; *s && (*s != ' '); s++) /* loop */;

        /* Paranoia -- Catch sillyness */
        if (*s != ' ') return;

        /* Extract a quantity */
        item->iqty = atoi(desc);

        /* Skip the quantity and space */
        desc = s + 1;
    }


    /* Paranoia */
    if (!desc[0]) return;


    /* Obtain a copy of the description */
    strcpy(temp, desc);

    /* Advance to the "inscription" or end of string */
    for (scan = temp; *scan && (*scan != c1); scan++) /* loop */;

    /* Nuke the space before the inscription */
    if ((scan[0] == c1) && (scan[-1] == ' ')) *--scan = '\0';

    /* Note that "scan" points at the "tail" of "temp" */

    /* Hack -- non-aware, singular, flavored items */
    if (item->iqty == 1)
    {
        if (prefix(temp, "Scroll titled ")) item->tval = TV_SCROLL;
        else if (streq(scan-7, " Potion")) item->tval = TV_POTION;
        else if (streq(scan-6, " Staff")) item->tval = TV_STAFF;
        else if (streq(scan-5, " Wand")) item->tval = TV_WAND;
        else if (streq(scan-4, " Rod")) item->tval = TV_ROD;
        else if (streq(scan-5, " Ring")) item->tval = TV_RING;
        else if (streq(scan-7, " Amulet")) item->tval = TV_AMULET;
        else if (streq(scan-9, " Mushroom")) item->tval = TV_FOOD;
    }

    /* Hack -- non-aware, plural, flavored items */
    else
    {
        if (prefix(temp, "Scrolls titled ")) item->tval = TV_SCROLL;
        else if (streq(scan-8, " Potions")) item->tval = TV_POTION;
        else if (streq(scan-7, " Staffs")) item->tval = TV_STAFF;
        else if (streq(scan-6, " Wands")) item->tval = TV_WAND;
        else if (streq(scan-5, " Rods")) item->tval = TV_ROD;
        else if (streq(scan-6, " Rings")) item->tval = TV_RING;
        else if (streq(scan-8, " Amulets")) item->tval = TV_AMULET;
        else if (streq(scan-10, " Mushrooms")) item->tval = TV_FOOD;
    }

    /* Accept non-aware flavored objects */
    if (item->tval)
    {
        /* Guess at weight and cost */
        switch (item->tval)
        {
            case TV_FOOD:
            {
                item->weight = 1;
                item->value = 5L;
                break;
            }

            case TV_POTION:
            {
                item->weight = 4;
                item->value = 20L;
                break;
            }

            case TV_SCROLL:
            {
                item->weight = 5;
                item->value = 20L;
                break;
            }

            case TV_STAFF:
            {
                item->weight = 50;
                item->value = 70L;
                break;
            }

            case TV_WAND:
            {
                item->weight = 10;
                item->value = 50L;
                break;
            }

            case TV_ROD:
            {
                item->weight = 15;
                item->value = 90L;
                break;
            }

            case TV_RING:
            {
                item->weight = 2;
                item->value = 45L;
                break;
            }

            case TV_AMULET:
            {
                item->weight = 3;
                item->value = 45L;
                break;
            }
        }

        /* Done */
        return;
    }


    /* Start at the beginning */
    tail = temp;

    /* Check singular items */
    if (item->iqty == 1)
    {
        /* Start the search */
        m = 0; n = auto_single_size;

        /* Simple binary search */
        while (m < n - 1)
        {
            /* Pick a "middle" entry */
            i = (m + n) / 2;

            /* Search to the right (or here) */
            if (strcmp(auto_single_text[i], tail) <= 0)
            {
                m = i;
            }

            /* Search to the left */
            else
            {
                n = i;
            }
        }

        /* Check prefixes XXX */
        for (i = m; i >= 0; i--)
        {
            /* Verify prefix */
            if (prefix(tail, auto_single_text[i]))
            {
                /* Save the item kind */
                item->kind = auto_single_what[i];

                /* Skip past the base name */
                tail += strlen(auto_single_text[i]);

                /* Done */
                break;
            }
        }
    }

    /* Check plural items */
    else
    {
        /* Start the search */
        m = 0; n = auto_plural_size;

        /* Simple binary search */
        while (m < n - 1)
        {
            /* Pick a "middle" entry */
            i = (m + n) / 2;

            /* Search to the right (or here) */
            if (strcmp(auto_plural_text[i], tail) <= 0)
            {
                m = i;
            }

            /* Search to the left */
            else
            {
                n = i;
            }
        }

        /* Check prefixes XXX */
        for (i = m; i >= 0; i--)
        {
            /* Verify prefix */
            if (prefix(tail, auto_plural_text[i]))
            {
                /* Save the item kind */
                item->kind = auto_plural_what[i];

                /* Skip past the base name */
                tail += strlen(auto_plural_text[i]);

                /* Done */
                break;
            }
        }
    }

    /* Oops */
    if (!item->kind)
    {
        borg_oops("bizarre object");
        return;
    }


    /* Extract some info */
    item->tval = k_info[item->kind].tval;
    item->sval = k_info[item->kind].sval;

    /* Guess at the weight */
    item->weight = k_info[item->kind].weight;

    /* ID the level */
    item->level = k_info[item->kind].level;

    /* Extract the base flags */
    item->flags1 = k_info[item->kind].flags1;
    item->flags2 = k_info[item->kind].flags2;
    item->flags3 = k_info[item->kind].flags3;


    /* Analyze "bonuses" */
    switch (item->tval)
    {
        /* Basic items */
        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_FLASK:
        case TV_FOOD:
        case TV_POTION:
        case TV_SCROLL:
        case TV_SPIKE:
        case TV_SKELETON:
        case TV_BOTTLE:
        case TV_JUNK:
        {
            /* Always "able" */
            item->able = TRUE;

            break;
        }

        /* Chests */
        case TV_CHEST:
        {
            /* XXX XXX XXX */

            /* Require the prefix and suffix */
            if (!prefix(tail, " (")) break;
            if (!suffix(tail, ")")) break;

            /* Assume "able" */
            item->able = TRUE;

            /* Hack -- assume "trapped" */
            item->pval = 63;

            /* Hack -- extract "empty" */
            if (streq(tail, " (empty)")) item->pval = 0;

            break;
        }

        /* Wands/Staffs -- charges */
        case TV_WAND:
        case TV_STAFF:
        {
            /* assume 1 charge unless empty.  This will allow use of  */
            /* staffs of teleport to get away when hit with a forget spell. */
            item->pval = 1;

            if (streq(item->note, "{empty}")) item->pval = 0;
            if (streq(tail, "(empty)")) item->pval = 0;

            /* Require the prefix and suffix */
            if (!prefix(tail, " (")) break; /* --(-- */
            if (!suffix(tail, " charge)") && !suffix(tail, " charges)")) break;

            /* Extract the "charges" */
            item->pval = atoi(tail+2);

            /* Assume "able" */
            item->able = TRUE;

            break;
        }

        /* Rods -- charging */
        case TV_ROD:
        {
            /* Always "able" */
            item->able = TRUE;

            /* Mega-Hack -- fake "charges" */
            item->pval = item->iqty;

            /* This is opposite from the game. (Pval is charge time) */
            /* Mega-Hack -- "charging" means no "charges" */
            if (streq(tail, " (charging)")) item->pval = 0;

            /* Hack-- we need to know if all are charging or not */
            if (suffix(tail, " charging)")) item->pval = item->iqty -1;
            if (streq(tail, format(" (%d charging)", item->iqty) )) item->pval -= item->iqty;

            break;
        }

        /* Wearable items */
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_LITE:
        case TV_AMULET:
        case TV_RING:
        {
            /* Hack -- handle "easy know" */
            if (k_info[item->kind].flags3 & TR3_EASY_KNOW)
            {
                /* Always known */
                item->able = TRUE;
            }

            /* No suffix */
            if (tail[0] != ' ') break;

            /* Start the search */
            m = 0; n = auto_artego_size;

            /* Binary search */
            while (m < n - 1)
            {
                /* Pick a "middle" entry */
                i = (m + n) / 2;

                /* Search to the right (or here) */
                if (strcmp(auto_artego_text[i], tail) <= 0)
                {
                    m = i;
                }

                /* Search to the left */
                else
                {
                    n = i;
                }
            }

            /* Check prefixes XXX */
            for (i = m; i >= 0; i--)
            {
                /* Verify prefix */
                if (prefix(tail, auto_artego_text[i]))
                {
                    /* Paranoia */
                    item->able = TRUE;

                    /* Save the artifact name */
                    if (auto_artego_what[i] < 256)
                    {
                        item->name1 = auto_artego_what[i];
                    }

                    /* Save the ego-item name */
                    else
                    {
                        item->name2 = auto_artego_what[i] - 256;
                    }

                    /* Skip the artifact/ego-item name */
                    tail += strlen(auto_artego_text[i]);

                    /* Done */
                    break;
                }
            }

            /* name3 items really screw up the borg */
            if (real_item->name3 && (real_item->ident & (IDENT_KNOWN)))
            {
                artifact_type *a_ptr;

                item->name1 = real_item->name1;
                item->name3 = real_item->name3;
                a_ptr = randart_make(real_item);
                item->value = a_ptr->cost;
                item->able = TRUE;

            }

            /* Hack -- grab "charging" suffix */
            if (suffix(tail, " (charging)"))
            {
                /* Remove the suffix */
                tail[strlen(tail)-11] = '\0';

                /* Remember it */
                item->timeout = 999;
            }

            /* Hack -- handle Lite's */
            if (item->tval == TV_LITE)
            {
                /* Hack -- Artifact Lite's */
                if (item->name1)
                {
                    /* Assume "able" */
                    item->able = TRUE;

                    /* Hack -- fuel */
                    item->pval = 2;

                    break;
                }

                /* Require the prefix and suffix */
                if (!prefix(tail, " (with ")) break;
                if (!suffix(tail, " of light)")) break;

                /* Extract "turns of lite" */
                item->pval = atoi(tail+7);

                /* Assume "able" */
                item->able = TRUE;

                break;
            }

            /* Hack -- Skip spaces */
            while (tail[0] == ' ') tail++;

            /* No suffix */
            if (!tail[0]) break;

            /* Parse "weapon-style" damage strings */
            if ((tail[0] == p1) &&
                ((item->tval == TV_HAFTED) ||
                 (item->tval == TV_POLEARM) ||
                 (item->tval == TV_SWORD) ||
                 (item->tval == TV_DIGGING) ||
                 (item->tval == TV_BOLT) ||
                 (item->tval == TV_ARROW) ||
                 (item->tval == TV_SHOT)))
            {
                /* First extract the damage string */
                for (scan = tail; *scan != p2; scan++) /* loop */;
                scan++;

                /* Hack -- Notice "end of string" */
                if (scan[0] != ' ') done = TRUE;

                /* Terminate the string and advance */
                *scan++ = '\0';

                /* Parse the damage string, or stop XXX */
                if (sscanf(tail, "(%dd%d)", &d1, &d2) != 2) break;

                /* Save the values */
                item->dd = d1;
                item->ds = d2;

                /* No extra information means not identified */
                if (done) break;

                /* Skip the "damage" info */
                tail = scan;
            }

            /* Parse the "damage" string for bows */
            else if ((tail[0] == p1) &&
                     (item->tval == TV_BOW))
            {
                /* First extract the damage string */
                for (scan = tail; *scan != p2; scan++) /* loop */;
                scan++;

                /* Hack -- Notice "end of string" */
                if (scan[0] != ' ') done = TRUE;

                /* Terminate the string and advance */
                *scan++ = '\0';

                /* Parse the multiplier string, or stop */
                if (sscanf(tail, "(x%d)", &d1) != 1) break;

                /* No extra information means not identified */
                if (done) break;

                /* Skip the "damage" info */
                tail = scan;
            }


            /* Parse the "bonus" string */
            if (tail[0] == p1)
            {
                /* Extract the extra info */
                for (scan = tail; *scan != p2; scan++) /* loop */;
                scan++;

                /* Hack -- Notice "end of string" */
                if (scan[0] != ' ') done = TRUE;

                /* Terminate the damage, advance */
                *scan++ = '\0';

                /* Parse standard "bonuses" */
                if (sscanf(tail, "(%d,%d)", &th, &td) == 2)
                {
                    item->to_h = th;
                    item->to_d = td;
                    item->able = TRUE;
                }

                /* XXX XXX Hack -- assume non-final bonuses are "to_hit" */
                else if (!done && sscanf(tail, "(%d)", &th) == 1)
                {
                    item->to_h = th;
                    item->able = TRUE;
                }

                /* XXX XXX Hack -- assume final bonuses are "pval" codes */
                else if (done)
                {
                    item->pval = atoi(tail + 1);
                    item->able = TRUE;
                }

                /* Oops */
                else
                {
                    break;
                }

                /* Nothing left */
                if (done) break;

                /* Skip the "damage bonus" info */
                tail = scan;
            }


            /* Parse the "bonus" string */
            if (tail[0] == b1)
            {
                /* Extract the extra info */
                for (scan = tail; *scan != b2; scan++) /* loop */;
                scan++;

                /* Hack -- Notice "end of string" */
                if (scan[0] != ' ') done = TRUE;

                /* Terminate the armor string, advance */
                *scan++ = '\0';

                /* Parse the armor, and bonus */
                if (sscanf(tail, "[%d,%d]", &ac, &ta) == 2)
                {
                    item->ac = ac;
                    item->to_a = ta;
                    item->able = TRUE;
                }

                /* Negative armor bonus */
                else if (sscanf(tail, "[-%d]", &ta) == 1)
                {
                    item->to_a = -ta;
                    item->able = TRUE;
                }

                /* Positive armor bonus */
                else if (sscanf(tail, "[+%d]", &ta) == 1)
                {
                    item->to_a = ta;
                    item->able = TRUE;
                }

                /* Just base armor */
                else if (sscanf(tail, "[%d]", &ac) == 1)
                {
                    item->ac = ac;
                }

                /* Oops */
                else
                {
                    break;
                }

                /* Nothing left */
                if (done) break;

                /* Skip the "armor" data */
                tail = scan;
            }


            /* Parse the final "pval" string, if any */
            if (tail[0] == p1)
            {
                /* Assume identified */
                item->able = TRUE;

                /* Hack -- Grab it */
                item->pval = atoi(tail + 1);
            }

            break;
        }
    }


    /* Hack -- repair rings of damage */
    if ((item->tval == TV_RING) && (item->sval == SV_RING_DAMAGE))
    {
        /* Bonus to dam, not pval */
        item->to_d = item->pval;
        item->pval = 0;
    }

    /* Hack -- repair rings of accuracy */
    if ((item->tval == TV_RING) && (item->sval == SV_RING_ACCURACY))
    {
        /* Bonus to hit, not pval */
        item->to_h = item->pval;
        item->pval = 0;
    }


    /* XXX XXX XXX Repair various "ego-items" */


    /* Hack -- examine artifacts */
    if (item->name1)
    {
        /* XXX XXX Hack -- fix "weird" artifacts */
        if ((item->tval != a_info[item->name1].tval) ||
            (item->sval != a_info[item->name1].sval))
        {
            /* Save the kind */
            item->kind = lookup_kind(item->tval, item->sval);

            /* Save the tval/sval */
            item->tval = k_info[item->kind].tval;
            item->sval = k_info[item->kind].sval;
        }

        /* Extract the weight */
        item->weight = a_info[item->name1].weight;

        /* Extract the artifact flags */
        item->flags1 = a_info[item->name1].flags1;
        item->flags2 = a_info[item->name1].flags2;
        item->flags3 = a_info[item->name1].flags3;
    }


    /* Hack -- examine ego-items */
    if (item->name2)
    {
        /* XXX Extract the weight */

        /* Extract the ego-item flags */
        item->flags1 |= e_info[item->name2].flags1;
        item->flags2 |= e_info[item->name2].flags2;
        item->flags3 |= e_info[item->name2].flags3;
    }


    /* Known items */
    if (item->able)
    {
        /* Process various fields */
        item->value = borg_object_value_known(item);
    }

    /* Aware items */
    else
    {
        /* Aware items can assume template cost */
        item->value = k_info[item->kind].cost;
    }


    /* Parse various "inscriptions" */
    if (item->note[0])
    {
        /* Special "discount" */
        if (streq(item->note, "{on sale}")) item->discount = 50;

        /* Standard "discounts" */
        else if (streq(item->note, "{25% off}")) item->discount = 25;
        else if (streq(item->note, "{50% off}")) item->discount = 50;
        else if (streq(item->note, "{75% off}")) item->discount = 75;
        else if (streq(item->note, "{90% off}")) item->discount = 90;

        /* Cursed indicators */
        else if (streq(item->note, "{cursed}"))
        {
            item->value = 0L;
            item->cursed = TRUE;
        }

        else if (streq(item->note, "{broken}")) item->value = 0L;
        else if (streq(item->note, "{terrible}")) item->value = 0L;
        else if (streq(item->note, "{worthless}")) item->value = 0L;
#if 0
        else if (suffix(item->note, "uncursed}"))
                item->discount = INSCRIP_UNCURSED;
#endif
        /* Ignore certain feelings */
        /* "{average}" */
        /* "{blessed}" */
        /* "{good}" */
        /* "{excellent}" */
        /* "{special}" */

        /* Ignore special inscriptions */
        /* "{empty}", "{tried}" */
    }


    /* Apply "discount" if any */
    if (item->discount) item->value -= item->value * item->discount / 100;

    /* Assume not fully Identified. */
    item->needs_I = item->fully_identified = FALSE;
}




/*
 * Send a command to inscribe item number "i" with the inscription "str".
 */
void borg_send_inscribe(int i, cptr str)
{
    cptr s;

    /* Label it */
    borg_keypress(c1);

    /* Choose from inventory */
    if (i < INVEN_WIELD)
    {
        /* Choose the item */
        borg_keypress(I2A(i));
    }

    /* Choose from equipment */
    else
    {
        /* Go to equipment (if necessary) */
        if (auto_items[0].iqty) borg_keypress('/');

        /* Choose the item */
        borg_keypress(I2A(i - INVEN_WIELD));
    }

    /* Send the label */
    for (s = str; *s; s++) borg_keypress(*s);

    /* End the inscription */
    borg_keypress('\n');

}

/*
 * Send a command to de-inscribe item number "i" .
 */
void borg_send_deinscribe(int i)
{

    /* Ok to inscribe Slime Molds */
    if (auto_items[i].tval == TV_FOOD &&
        auto_items[i].sval == SV_FOOD_SLIME_MOLD) return;

    /* Label it */
    borg_keypress('}');

    /* Choose from inventory */
    if (i < INVEN_WIELD)
    {
        /* Choose the item */
        borg_keypress(I2A(i));
    }

    /* Choose from equipment */
    else
    {
        /* Go to equipment (if necessary) */
        if (auto_items[0].iqty) borg_keypress('/');

        /* Choose the item */
        borg_keypress(I2A(i - INVEN_WIELD));
    }

}




/*
 * Find the slot of an item with the given tval/sval, if available.
 * Given multiple choices, choose the item with the largest "pval".
 * Given multiple choices, choose the smallest available pile.
 */
int borg_slot(int tval, int sval)
{
    int i, n = -1;

    /* Scan the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Skip un-aware items */
        if (!item->kind) continue;

        /* Require correct tval */
        if (item->tval != tval) continue;

        /* Require correct sval */
        if (item->sval != sval) continue;

        /* Prefer largest "pval" */
        if ((n >= 0) && (item->pval < auto_items[n].pval)) continue;

        /* Prefer smallest pile */
        if ((n >= 0) && (item->iqty > auto_items[n].iqty)) continue;

        /* Save this item */
        n = i;
    }

    /* Done */
    return (n);
}



/*
 * Hack -- refuel a torch
 */
bool borg_refuel_torch(void)
{
    int i;

    /* Look for a torch */
    i = borg_slot(TV_LITE, SV_LITE_TORCH);

    /* None available */
    if (i < 0) return (FALSE);

    /* must first wield before one can refuel */
    if (auto_items[INVEN_LITE].sval != SV_LITE_TORCH)
        {
            return (FALSE);
        }

    /* Dont bother with empty */
    if (auto_items[i].pval == 0)
    {
        return (FALSE);
    }

    /* Cant refuel nothing */
    if (auto_items[INVEN_LITE].iqty == 0)
    {
        return (FALSE);
    }

    /* Log the message */
    borg_note(format("# Refueling with %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('F');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}


/*
 * Hack -- refuel a lantern
 */
bool borg_refuel_lantern(void)
{
    int i;

    /* Look for a torch */
    i = borg_slot(TV_FLASK, 0);

    /* None available */
    if (i < 0) return (FALSE);

    /* Cant refuel a torch with oil */
    if (auto_items[INVEN_LITE].sval != SV_LITE_LANTERN)
    {
        return (FALSE);
    }

    /* Log the message */
    borg_note(format("# Refueling with %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('F');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}




/*
 * Hack -- attempt to eat the given food (by sval)
 */
bool borg_eat_food(int sval)
{
    int i;

    /* Look for that food */
    i = borg_slot(TV_FOOD, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* Log the message */
    borg_note(format("# Eating %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('E');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}

/*
 * Quaff a potion of cure critical wounds.  This is a special case
 *   for several reasons.
 *   1) it is usually the only healing potion we have on us
 *   2) we should try to conserve a couple for when we really need them
 *   3) if we are burning through them fast we should probably teleport out of
 *      the fight.
 *   4) When it is the only/best way out of danger, drink away
  */
bool borg_quaff_crit( bool no_check )
{
    static s16b when_last_quaff = 0;

    if (no_check)
    {
        if (borg_quaff_potion(SV_POTION_CURE_CRITICAL))
        {
            when_last_quaff = c_t;
            return (TRUE);
        }
        return (FALSE);
    }

    /* Save the last two for when we really need them */
    if (borg_skill[BI_ACCW] < 2)
        return FALSE;

    /* Avoid drinking CCW twice in a row */
    if (when_last_quaff > (c_t-4) &&
        when_last_quaff <= c_t  &&
        (rand_int(100) < 75))
        return FALSE;

    if (borg_quaff_potion(SV_POTION_CURE_CRITICAL))
    {
        when_last_quaff = c_t;
        return (TRUE);
    }
    return (FALSE);
}


/*
 * Hack -- attempt to quaff the given potion (by sval)
 */
bool borg_quaff_potion(int sval)
{
    int i;

    /* Look for that potion */
    i = borg_slot(TV_POTION, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* Log the message */
    borg_note(format("# Quaffing %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('q');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}
/*
 * Hack -- attempt to quaff an unknown potion
 */
bool borg_quaff_unknown(void)
{
    int i, n = -1;

    /* Scan the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Require correct tval */
        if (item->tval != TV_POTION) continue;

        /* Skip aware items */
        if (item->kind) continue;

        /* Save this item */
        n = i;
    }


    /* None available */
    if (n < 0) return (FALSE);

    /* Log the message */
    borg_note(format("# Quaffing unknown potion %s.", auto_items[n].desc));

    /* Perform the action */
    borg_keypress('q');
    borg_keypress(I2A(n));

    /* Success */
    return (TRUE);
}

/*
 * Hack -- attempt to read an unknown scroll
 */
bool borg_read_unknown(void)
{
    int i, n = -1;
    auto_grid *ag = &auto_grids[c_y][c_x];

    /* Scan the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Require correct tval */
        if (item->tval != TV_SCROLL) continue;

        /* Skip aware items */
        if (item->kind) continue;

        /* Save this item */
        n = i;
    }


    /* None available */
    if (n < 0) return (FALSE);

    /* Not when dark */
    if (!(ag->info & BORG_GLOW) && !borg_skill[BI_CUR_LITE]) return (FALSE);

    /* Blind or Confused */
    if (do_blind || do_confused) return (FALSE);

    /* Log the message */
    borg_note(format("# Reading unknown scroll %s.", auto_items[n].desc));

    /* Perform the action */
    borg_keypress('r');
    borg_keypress(I2A(n));

    /* Success */
    return (TRUE);
}


/*
 * Hack -- attempt to eat an unknown potion.  This is done in emergencies.
 */
bool borg_eat_unknown(void)
{
    int i, n = -1;

    /* Scan the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Require correct tval */
        if (item->tval != TV_FOOD) continue;

        /* Skip aware items */
        if (item->kind) continue;

        /* Save this item */
        n = i;
    }


    /* None available */
    if (n < 0) return (FALSE);

    /* Log the message */
    borg_note(format("# Eating unknown mushroom %s.", auto_items[n].desc));

    /* Perform the action */
    borg_keypress('E');
    borg_keypress(I2A(n));

    /* Success */
    return (TRUE);
}

/*
 * Hack -- attempt to use an unknown staff.  This is done in emergencies.
 */
bool borg_use_unknown(void)
{
    int i, n = -1;

    /* Scan the pack */
    for (i = 0; i < INVEN_PACK; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Require correct tval */
        if (item->tval != TV_STAFF) continue;

        /* Skip aware items */
        if (item->kind) continue;

        /* Save this item */
        n = i;
    }


    /* None available */
    if (n < 0) return (FALSE);

    /* Log the message */
    borg_note(format("# Using unknown Staff %s.", auto_items[n].desc));

    /* Perform the action */
    borg_keypress('u');
    borg_keypress(I2A(n));

    /* Success */
    return (TRUE);
}


/*
 * Hack -- attempt to read the given scroll (by sval)
 */
bool borg_read_scroll(int sval)
{
    int i;
    auto_grid *ag = &auto_grids[c_y][c_x];

    /* Dark */
    if (!(ag->info & BORG_GLOW) && !borg_skill[BI_CUR_LITE]) return (FALSE);

    /* Blind or Confused */
    if (do_blind || do_confused) return (FALSE);

    /* Look for that scroll */
    i = borg_slot(TV_SCROLL, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* Log the message */
    borg_note(format("# Reading %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress(ESCAPE);
    borg_keypress(ESCAPE);
    borg_keypress('r');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}

/*
 * Hack -- checks rod (by sval) and
 * make a fail check on it.
 */
bool borg_equips_rod(int sval)
{
    int i, chance, lev;

    /* Look for that staff */
    i = borg_slot(TV_ROD, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* No charges */
    if (!auto_items[i].pval) return (FALSE);

    /* Extract the item level */
    lev = (auto_items[i].level);

    /* Base chance of success */
    chance = borg_skill[BI_DEV];

    /* Confusion hurts skill */
    if (do_confused) chance = chance / 2;

    /* High level objects are harder */
    chance = chance - ((lev > 50) ? 50 : lev);

    /* Roll for usage */
    if (chance < USE_DEVICE*2) return (FALSE);

    /* Yep we got one */
    return (TRUE);
}



/*
 * Hack -- attempt to zap the given (charged) rod (by sval)
 */
bool borg_zap_rod(int sval)
{
    int i, lev, chance;

    /* Look for that rod */
    i = borg_slot(TV_ROD, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* Hack -- Still charging */
    if (!auto_items[i].pval) return (FALSE);

    /* Extract the item level */
    lev = (auto_items[i].level);

    /* Base chance of success */
    chance = borg_skill[BI_DEV];

    /* Confusion hurts skill */
    if (do_confused) chance = chance / 2;

    /* High level objects are harder */
    chance = chance - ((lev > 50) ? 50 : lev);

    /* Roll for usage */
    if (chance < USE_DEVICE+2) return (FALSE);

    /* Log the message */
    borg_note(format("# Zapping %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('z');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}


/*
 * Hack -- attempt to aim the given (charged) wand (by sval)
 */
bool borg_aim_wand(int sval)
{
    int i;

    /* Look for that wand */
    i = borg_slot(TV_WAND, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* No charges */
    if (!auto_items[i].pval) return (FALSE);

    /* record the address to avoid certain bugs with inscriptions&amnesia */
    zap_slot = i;

    /* Log the message */
    borg_note(format("# Aiming %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('a');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}


/*
 * Hack -- attempt to use the given (charged) staff (by sval)
 */
bool borg_use_staff(int sval)
{
    int i;

    /* Look for that staff */
    i = borg_slot(TV_STAFF, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* No charges */
    if (!auto_items[i].pval) return (FALSE);

    /* record the address to avoid certain bugs with inscriptions&amnesia */
    zap_slot = i;

    /* Log the message */
    borg_note(format("# Using %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('u');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}

/*
 * Hack -- attempt to use the given (charged) staff (by sval) and
 * make a fail check on it.
 */
bool borg_use_staff_fail(int sval)
{
    int i, chance, lev;

    /* Look for that staff */
    i = borg_slot(TV_STAFF, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* No charges */
    if (!auto_items[i].pval) return (FALSE);

    /* Extract the item level */
    lev = (auto_items[i].level);

    /* Base chance of success */
    chance = borg_skill[BI_DEV];

    /* Confusion hurts skill */
    if (do_confused) chance = chance / 2;

    /* High level objects are harder */
    chance = chance - ((lev > 50) ? 50 : lev);

    /* Roll for usage, but if its a Teleport be generous. */
    if (chance < USE_DEVICE*2)
    {
        if (sval != SV_STAFF_TELEPORTATION)
        {
            return (FALSE);
        }

        /* We need to give some "desparation attempt to teleport staff" */
        if (!do_confused && !do_blind) /* Dark? */
        {
            /* We really have no chance, return false, attempt the scroll */
            if (chance < USE_DEVICE) return (FALSE);
        }
        /* We might have a slight chance, or we cannot not read */
    }


    /* record the address to avoid certain bugs with inscriptions&amnesia */
    zap_slot = i;

    /* Log the message */
    borg_note(format("# Using %s.", auto_items[i].desc));

    /* Perform the action */
    borg_keypress('u');
    borg_keypress(I2A(i));

    /* Success */
    return (TRUE);
}
/*
 * Hack -- checks staff (by sval) and
 * make a fail check on it.
 */
bool borg_equips_staff_fail(int sval)
{
    int i, chance, lev;

    /* Look for that staff */
    i = borg_slot(TV_STAFF, sval);

    /* None available */
    if (i < 0) return (FALSE);

    /* No charges */
    if (!auto_items[i].pval) return (FALSE);

    /* Extract the item level */
    lev = (auto_items[i].level);

    /* Base chance of success */
    chance = borg_skill[BI_DEV];

    /* Confusion hurts skill */
    if (do_confused) chance = chance / 2;

    /* High level objects are harder */
    chance = chance - ((lev > 50) ? 50 : lev);

    /* Roll for usage, but if its a Teleport be generous. */
    if (chance < USE_DEVICE*2)
    {
        if (sval != SV_STAFF_TELEPORTATION && sval !=SV_STAFF_DESTRUCTION)
        {
            return (FALSE);
        }

        /* We need to give some "desparation attempt to teleport staff" */
        if (!do_confused)
        {
            /* We really have no chance, return false, attempt the scroll */
            if (chance < USE_DEVICE) return (FALSE);
        }

        /* We might have a slight chance, continue on */
    }

    /* Yep we got one */
    return (TRUE);
}



/*
 * Hack -- attempt to use the given artifact (by index)
 */
bool borg_activate_artifact(int name1, int location)
{
    int i;

    /* Check the equipment */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip incorrect artifacts */
        if (item->name1 != name1) continue;

        /* Check charge */
        if (item->timeout) return (FALSE);

        /*
         * Random Artifact must be *ID* to know the activation power.
         * The borg will cheat with random artifacts to know if the
         * artifact number is activatable, but artifact names and
         * types will be scrambled.  So he must first *ID* the artifact
         * he must play with the artifact to learn its power, just as
         * he plays with magic to gain experience.  But I am not about
         * to undertake that coding.  He needs to *ID* it anyway to learn
         * of the resists that go with the artifact.
         * Lights dont need *id* just regular id.
         */
        if  (item->name1 != ART_GALADRIEL &&
              item->name1 != ART_ELENDIL &&
              item->name1 != ART_THRAIN &&
             (!item->fully_identified))
        {
            borg_note(format("# %s must be *ID*'d before activation.", item->desc));
            return (FALSE);
        }


        /* Log the message */
        borg_note(format("# Activating artifact %s.", item->desc));

        /* Perform the action */
        borg_keypress('A');
        borg_keypress(I2A(i - INVEN_WIELD));

        /* Success */
        return (TRUE);
    }

    /* Oops */
    return (FALSE);
}


/*
 * apw Hack -- check and see if borg is wielding an artifact
 */
bool borg_equips_artifact(int name1, int location)
{
    int i;
    int lev, chance;

    /* Check the equipment-- */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip incorrect artifacts */
        if (item->name1 != name1) continue;

        /* Check charge.  But not on certain ones  Wor, ID, phase, TELEPORT.*/
        /* this is to ensure that his borg_prep code is working ok */
        if ((name1 != ART_AVAVIR &&
             name1 != ART_ERIRIL &&
             name1 != ART_BELEGENNON &&
             name1 != ART_COLANNON) &&
           (item->timeout) ) continue;
        /*
         * Random Artifact must be *ID* to know the activation power.
         * The borg will cheat with random artifacts to know if the
         * artifact number is activatable, but artifact names and
         * types will be scrambled.  So he must first *ID* the artifact
         * he must play with the artifact to learn its power, just as
         * he plays with magic to gain experience.  But I am not about
         * to undertake that coding.  He needs to *ID* it anyway to learn
         * of the resists that go with the artifact.
         * Lights dont need *id* just regular id.
         */
        if  (item->name1 != ART_GALADRIEL &&
              item->name1 != ART_ELENDIL &&
              item->name1 != ART_THRAIN &&
             (!item->fully_identified))
        {
            borg_note(format("# %s must be *ID*'d before activation.", item->desc));
            continue;
        }

       /* Extract the item level for fail rate check*/
       lev = item->level;

        /* Base chance of success */
        chance = borg_skill[BI_DEV];

        /* Confusion hurts skill */
        if (do_confused) chance = chance / 2;

        /* High level objects are harder */
        chance = chance - ((lev > 50) ? 50 : lev);

        /* Roll for usage.  Return Fail if greater than 50% fail */
        if (chance < (USE_DEVICE*2)) continue;

        /* Success */
        return (TRUE);

    }

    /* I guess I dont have it, or it is not ready */
    return (FALSE);
}
/*
 * Hack -- check and see if borg is wielding a dragon armor and if
 * he will pass a fail check.
 */
bool borg_equips_dragon(int drag_sval)
{
    int lev, chance;

       /* Check the equipment */
       auto_item *item = &auto_items[INVEN_BODY];

        /* Skip incorrect armours */
        if (item->tval !=TV_DRAG_ARMOR) return (FALSE);
        if (item->sval != drag_sval) return (FALSE);

        /* Check charge */
        if (item->timeout) return (FALSE);

        /* apw Make Sure Mail is IDed */
        if (!item->able) return (FALSE);

       /* check on fail rate
        * The fail check is automatic for dragon armor.  It is an attack
        * item.  He should not sit around failing 5 or 6 times in a row.
        * he should attempt to activate it, and if he is likely to fail, then
        * eh should look at a different attack option.  We are assuming
        * that the fail rate is about 50%.  So He may still try to activate it
        * and fail.  But he will not even try if he has negative chance or
        * less than twice the USE_DEVICE variable
        */
       /* Extract the item level */
       lev = auto_items[INVEN_BODY].level;

        /* Base chance of success */
        chance = borg_skill[BI_DEV];

        /* Confusion hurts skill */
        if (do_confused) chance = chance / 2;

        /* High level objects are harder */
        chance = chance - ((lev > 50) ? 50 : lev);

        /* Roll for usage */
        if (chance < (USE_DEVICE*2)) return (FALSE);

        /* Success */
        return (TRUE);

}

/*
 * apw Hack -- attempt to use the given dragon armour
 */
bool borg_activate_dragon(int drag_sval)
{

    /* Check the equipment */

      auto_item *item = &auto_items[INVEN_BODY];

        /* Skip incorrect mails */
        if (item->tval != TV_DRAG_ARMOR) return (FALSE);
        if (item->sval != drag_sval) return (FALSE);

        /* Check charge */
        if (item->timeout) return (FALSE);

        /* apw Make Sure Mail is IDed */
        if (!item->able) return (FALSE);

        /* Log the message */
        borg_note(format("# Activating dragon scale %s.", item->desc));

        /* Perform the action */
        borg_keypress('A');
        borg_keypress(I2A(INVEN_BODY - INVEN_WIELD));

        /* Success */
        return (TRUE);
}


/*
 * Determine if borg can cast a given spell (when fully rested)
 */
bool borg_spell_legal(int book, int what)
{
    auto_magic *as = &auto_magics[book][what];

    /* The borg must be able to "cast" spells */
    if (mb_ptr->spell_book != TV_MAGIC_BOOK) return (FALSE);

    /* The book must be possessed */
    if (amt_book[book] <= 0) return (FALSE);

    /* The spell must be "known" */
    if (as->status < BORG_MAGIC_TEST) return (FALSE);

    /* The spell must be affordable (when rested) */
    if (as->power > auto_msp) return (FALSE);

    /* Success */
    return (TRUE);
}

/*
 * Determine if borg can cast a given spell (right now)
 */
bool borg_spell_okay(int book, int what)
{
    int reserve_mana = 0;

    auto_magic *as = &auto_magics[book][what];

    auto_grid *ag = &auto_grids[c_y][c_x];

    /* Dark */
    if (!(ag->info & BORG_GLOW) && !borg_skill[BI_CUR_LITE]) return (FALSE);

    /* Define reserve_mana for each class */
    if (auto_class == CLASS_MAGE) reserve_mana = 8;
    if (auto_class == CLASS_RANGER) reserve_mana = 22;
    if (auto_class == CLASS_ROGUE) reserve_mana = 20;

    /* Only use reserve mana if high level */
    if (auto_max_level <= 35) reserve_mana = 0;

    /* Require ability (when rested) */
    if (!borg_spell_legal(book, what)) return (FALSE);

    /* The spell must be affordable (now) */
    if (as->power > auto_csp) return (FALSE);

    /* Spell cannot cut into our reserve mana, unless its an escape spell */
    if (auto_csp - as->power < reserve_mana)
    {
        /* phase spells are ok */
        if (book == 1 && what == 4) return (TRUE);

        /* teleport spells are ok */
        if (book == 2 && what == 6) return (TRUE);
        if (book == 5 && what == 1) return (TRUE);

        /* other spells are rejected */
        return (FALSE);
    }

    /* Success */
    return (TRUE);
}

/*
 * fail rate on a spell
 */
static int borg_spell_fail_rate(int book, int what)
{
    int     chance, minfail;
    auto_magic *as = &auto_magics[book][what];

    /* Access the spell  */
    chance = as->sfail;

    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (borg_skill[BI_CLEVEL] - as->level);

    /* Reduce failure rate by INT/WIS adjustment */
    chance -= 3 * (adj_mag_stat[my_stat_ind[A_INT]] - 1);

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[my_stat_ind[A_INT]];

    /* Non mage characters never get too good */
    if (auto_class != CLASS_MAGE)
    {
        if (minfail < 5) minfail = 5;
    }

    /* Minimum failure rate */
    if (chance < minfail) chance = minfail;

    /* Stunning makes spells harder */
    if (do_heavy_stun) chance += 25;
    if (do_stun) chance += 15;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Return the chance */
    return (chance);


}

/*
 * same as borg_spell_okay with a fail % check
 */
bool borg_spell_okay_fail(int book, int what, int allow_fail )
{
    if (borg_spell_fail_rate(book, what) > allow_fail)
        return FALSE;
    return borg_spell_okay( book, what );
}

/*
 * Same as borg_spell with a fail % check
 */
bool borg_spell_fail(int book, int what, int allow_fail)
{
    if (borg_spell_fail_rate(book, what) > allow_fail)
        return FALSE;
    return borg_spell( book, what );
}

/*
 * Same as borg_spell_legal with a fail % check
 */
bool borg_spell_legal_fail(int book, int what, int allow_fail)
{
    if (borg_spell_fail_rate(book, what) > allow_fail)
        return FALSE;
    return borg_spell_legal( book, what );
}

/*
 * Attempt to cast a spell
 */
bool borg_spell(int book, int what)
{
    int i;

    auto_magic *as = &auto_magics[book][what];

    /* Require ability (right now) */
    if (!borg_spell_okay(book, what)) return (FALSE);

    /* Look for the book */
    i = auto_book[book];

    /* Paranoia */
    if (i < 0) return (FALSE);

    /* Debugging Info */
    borg_note(format("# Casting %s (%d,%d).", as->name, book, what));

    /* Cast a spell */
    borg_keypress('m');
    borg_keypress(I2A(i));
    borg_keypress(I2A(what));

    /* increment the spell counter */
    as->times ++;

    /* Success */
    return (TRUE);
}


/*
 * Determine if borg can pray a given prayer (when fully rested)
 */
bool borg_prayer_legal(int book, int what)
{
    auto_magic *as = &auto_magics[book][what];

    /* The borg must be able to "pray" prayers */
    if (mb_ptr->spell_book != TV_PRAYER_BOOK) return (FALSE);

    /* Look for the book */
    if (amt_book[book] <= 0) return (FALSE);

    /* The prayer must be "known" */
    if (as->status < BORG_MAGIC_TEST) return (FALSE);

    /* The prayer must be affordable (when fully rested) */
    if (as->power > auto_msp) return (FALSE);

    /* Success */
    return (TRUE);
}

/*
 * Determine if borg can pray a given prayer (right now)
 */
bool borg_prayer_okay(int book, int what)
{
    int reserve_mana =0;

    auto_magic *as = &auto_magics[book][what];

    auto_grid *ag = &auto_grids[c_y][c_x];

    /* Dark */
    if (!(ag->info & BORG_GLOW) && !borg_skill[BI_CUR_LITE]) return (FALSE);


    /* define reserve_mana */
    if (auto_class == CLASS_PRIEST) reserve_mana = 8;
    if (auto_class == CLASS_PALADIN) reserve_mana = 20;

    /* Low level spell casters should not worry about this */
    if (borg_skill[BI_CLEVEL] < 35) reserve_mana = 0;

    /* Require ability (when rested) */
    if (!borg_prayer_legal(book, what)) return (FALSE);

    /* Hack -- blind/confused */
    if (do_blind || do_confused) return (FALSE);

    /* The prayer must be affordable (right now) */
    if (as->power > auto_csp) return (FALSE);

    /* Do not cut into reserve mana (for final teleport) */
    if (auto_csp - as->power < reserve_mana)
    {
        /* Phase spells ok */
        if (book == 1 && what == 1) return (TRUE);
        if (book == 4 && what == 0) return (TRUE);

        /* Teleport spells ok */
        if (book == 4 && what == 1) return (TRUE);

        /* others are rejected */
        return (FALSE);
    }

    /* Success */
    return (TRUE);
}

static int borg_prayer_fail_rate(int book, int what)
{
    int     chance, minfail;
    auto_magic *as = &auto_magics[book][what];

    /* Access the spell  */
    chance = as->sfail;

    /* Reduce failure rate by "effective" level adjustment */
    chance -= 3 * (borg_skill[BI_CLEVEL] - as->level);

    /* Reduce failure rate by INT/WIS adjustment */
    chance -= 3 * (adj_mag_stat[my_stat_ind[A_WIS]] - 1);

    /* Extract the minimum failure rate */
    minfail = adj_mag_fail[my_stat_ind[A_WIS]];

    /* Non priest characters never get too good */
    if (auto_class != CLASS_PRIEST)
    {
        if (minfail < 5) minfail = 5;
    }

    /* APW Hack -- Priest prayer penalty for "edged" weapons  -DGK */
    if (auto_class == CLASS_PRIEST)
    {
        auto_item       *item;

        item = &auto_items[INVEN_WIELD];

        /* Penalize non-blessed edged weapons */
        if (((item->tval == TV_SWORD) || (item->tval == TV_POLEARM)) &&
            (!(item->flags3 & TR3_BLESSED)))
        {
            chance += 25;
        }
    }
    /* Minimum failure rate */
    if (chance < minfail) chance = minfail;

    /* Stunning makes spells harder */
    if (do_heavy_stun) chance += 25;
    if (do_stun) chance += 15;

    /* Always a 5 percent chance of working */
    if (chance > 95) chance = 95;

    /* Return the chance */
    return (chance);


}


/*
 * same as borg_prayer_okay with a fail % check
 */
bool borg_prayer_okay_fail(int book, int what, int allow_fail )
{
    if (borg_prayer_fail_rate(book, what) > allow_fail)
        return FALSE;
    return borg_prayer_okay( book, what );
}

/*
 * Same as borg_prayer with a fail % check
 */
bool borg_prayer_fail(int book, int what, int allow_fail)
{
    if (borg_prayer_fail_rate(book, what) > allow_fail)
        return FALSE;
    return borg_prayer( book, what );
}

/*
 * Same as borg_prayer_legal with a fail % check
 */
bool borg_prayer_legal_fail(int book, int what, int allow_fail)
{
    if (borg_prayer_fail_rate(book, what) > allow_fail)
        return FALSE;
    return borg_prayer_legal( book, what );
}

/*
 * Attempt to pray a prayer
 */
bool borg_prayer(int book, int what)
{
    int i;

    auto_magic *as = &auto_magics[book][what];

    /* Require ability (right now) */
    if (!borg_prayer_okay(book, what)) return (FALSE);

    /* Look for the book */
    i = auto_book[book];

    /* Paranoia */
    if (i < 0) return (FALSE);

    /* Debugging Info */
    borg_note(format("# Praying %s (%d,%d).", as->name, book, what));

    /* Pray a prayer */
    borg_keypress('p');
    borg_keypress(I2A(i));
    borg_keypress(I2A(what));

    /* Because we have no launch message to indicate failure */
    if (book ==3 && what ==4)
    {
        borg_casted_glyph = TRUE;
    }
    else
    {
        borg_casted_glyph = FALSE;
    }

    /* increment the spell counter */
    as->times ++;

    /* Success */
    return (TRUE);
}
/* Inscribe food and Slime Molds
 */
extern bool borg_inscribe_food(void)
{
    int ii;
    char name[80];

    for (ii=0; ii< INVEN_TOTAL; ii++)
    {
        auto_item *item = &auto_items[ii];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* Require correct tval */
        if (item->tval != TV_FOOD) continue;

        /* skip things already inscribed */
        if (!(streq(item->note, "")) &&
            !(streq(item->note, "{ }"))) continue;

        /* inscribe foods and molds */
        if (item->sval == SV_FOOD_SLIME_MOLD || item->sval == SV_FOOD_RATION)
        {

            if (item->sval == SV_FOOD_RATION)
            {
                /* get a name */
                strcpy(name, food_syllable1[rand_int(sizeof(food_syllable1) / sizeof(char*))]);
                strcat(name, food_syllable2[rand_int(sizeof(food_syllable2) / sizeof(char*))]);

                borg_send_inscribe(ii, name);
                return (TRUE);
            }

            if (item->sval == SV_FOOD_SLIME_MOLD)
            {
                /* get a name */
                strcpy(name, mold_syllable1[rand_int(sizeof(mold_syllable1) / sizeof(char*))]);
                strcat(name, mold_syllable2[rand_int(sizeof(mold_syllable2) / sizeof(char*))]);
                strcat(name, mold_syllable3[rand_int(sizeof(mold_syllable3) / sizeof(char*))]);

                borg_send_inscribe(ii, name);
                return (TRUE);
            }

        }
    }

    /* all done */
    return (FALSE);
}

/*
 * Cheat the "equip" screen
 */
void borg_cheat_equip(void)
{
    int i;

    char buf[256];

    /* Extract the equipment */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        /* Default to "nothing" */
        buf[0] = '\0';

        /* Describe a real item */
        if (inventory[i].k_idx)
        {
            /* Describe it */
            object_desc(buf, &inventory[i], TRUE, 3);
        }

#if 0
        /* Ignore "unchanged" items */
        /* !FIX do I still need this?  I forget...  AJG */
        if (auto_items[i].needs_I)
        {
           if (streq(buf, auto_items[i].desc)) continue;
        }
#endif
        /* Analyze the item (no price) */
        borg_item_analyze(&auto_items[i], &inventory[i], buf);

        /* get the fully id stuff */
        if (inventory[i].ident & IDENT_MENTAL)
        {
            auto_items[i].fully_identified = TRUE;
            auto_items[i].needs_I = TRUE;
            auto_do_star_id = TRUE;
        }

    }
}


/*
 * Cheat the "inven" screen
 */
void borg_cheat_inven(void)
{
    int i;

    char buf[256];

    /* Extract the current weight */
    auto_cur_wgt = p_ptr->total_weight;

    /* Extract the inventory */
    for (i = 0; i < INVEN_PACK; i++)
    {
        /* Default to "nothing" */
        buf[0] = '\0';

        /* Describe a real item */
        if (inventory[i].k_idx)
        {
            /* Describe it */
            object_desc(buf, &inventory[i], TRUE, 3);
        }

        /* Ignore "unchanged" items */
        if (streq(buf, auto_items[i].desc)) continue;

        /* inventory changed so goals must change. */
        goal_shop = goal_ware = goal_item = -1;

        /* Analyze the item (no price) */
        borg_item_analyze(&auto_items[i], &inventory[i], buf);

        /* get the fully id stuff */
        if (inventory[i].ident & IDENT_MENTAL)
        {
            auto_items[i].fully_identified = TRUE;
            auto_items[i].needs_I = TRUE;
            auto_do_star_id = TRUE;
        }

        /* Note changed inventory */
        auto_do_crush_junk = TRUE;
        auto_do_crush_hole = TRUE;
        auto_do_crush_slow = TRUE;
    }
}

#ifndef BORG_TK
/*
 * Parse the "equip" screen
 */
void borg_parse_equip(void)
{
    int i;

    int row, col;

    bool done = FALSE;

    byte t_a;

    char buf[160];


    /* Find the column */
    for (col = 0; col < 55; col++)
    {
        /* Look for first prefix */
        if ((0 == borg_what_text(col, 1, 3, &t_a, buf)) &&
            (buf[0] == I2A(0)) && (buf[1] == p2) && (buf[2] == ' '))
        {
            break;
        }
    }

    /* Extract the inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        /* Access the row */
        row = i - INVEN_WIELD;

        /* Attempt to get some text */
        if (!done &&
            (0 == borg_what_text(col, row+1, 3, &t_a, buf)) &&
            (buf[0] == I2A(row)) && (buf[1] == p2) && (buf[2] == ' ') &&
            (0 == borg_what_text(col+3, row+1, -80, &t_a, buf)) &&
            (buf[0] && (buf[0] != ' ')))
        {
            int k;

            /* Strip trailing spaces */
            for (k = strlen(buf); (k > 0) && (buf[k-1] == ' '); k--) /* loop */;
            buf[k] = '\0';
        }

        /* Default to "nothing" */
        else
        {
            buf[0] = '\0';
            done = TRUE;
        }

        /* Notice empty slots */
        if (streq(buf, "(nothing)")) strcpy(buf, "");

        /* Ignore "unchanged" items */
        if (streq(buf, auto_items[i].desc) && auto_depth) continue;

        /* Analyze the item (no price) */
        borg_item_analyze(&auto_items[i], &inventory[i], buf);

        /* get the fully *id* stuff */
        if (inventory[i].ident & IDENT_MENTAL)
        {
            auto_items[i].fully_identified = TRUE;
            auto_items[i].needs_I = TRUE;
            auto_do_star_id = TRUE;
        }

    }
}


/*
 * Parse the "inven" screen
 */
void borg_parse_inven(void)
{
    int i;

    int row, col;

    int w1a, w1b;

    bool done = FALSE;

    byte t_a;

    char buf[160];


    /* XXX XXX XXX */
    /* Hack -- Parse the current total weight */
    if ((0 == borg_what_text(0, 0, -80, &t_a, buf)) &&
        (sscanf(buf, "Inventory (carrying %d.%d pounds)",
                &w1a, &w1b) == 2))
    {
        /* Save the current weight */
        auto_cur_wgt = w1a * 10 + w1b;
    }


    /* Find the column */
    for (col = 0; col < 55; col++)
    {
        /* Look for first prefix */
        if ((0 == borg_what_text(col, 1, 3, &t_a, buf)) &&
            (buf[0] == I2A(0)) && (buf[1] == p2) && (buf[2] == ' '))
        {
            break;
        }
    }

    /* Extract the inventory */
    for (i = 0; i < INVEN_PACK; i++)
    {
        /* Access the row */
        row = i;

        /* Attempt to get some text */
        if (!done &&
            (0 == borg_what_text(col, row+1, 3, &t_a, buf)) &&
            (buf[0] == I2A(row)) && (buf[1] == p2) && (buf[2] == ' ') &&
            (0 == borg_what_text(col+3, row+1, -80, &t_a, buf)) &&
            (buf[0] && (buf[0] != ' ')))
        {
            int k;

            /* Strip trailing spaces */
            for (k = strlen(buf); (k > 0) && (buf[k-1] == ' '); k--) /* loop */;
            buf[k] = '\0';
        }

        /* Default to "nothing" */
        else
        {
            buf[0] = '\0';
            done = TRUE;
        }

        /* Notice empty slots */
        if (streq(buf, "(nothing)")) strcpy(buf, "");

        /* Ignore "unchanged" items */
        if (streq(buf, auto_items[i].desc)) continue;

        /* Analyze the item (no price) */
        borg_item_analyze(&auto_items[i], &inventory[i], buf);

        /* get the fully id stuff */
        if (inventory[i].ident & IDENT_MENTAL)
        {
            auto_items[i].fully_identified = TRUE;
            auto_items[i].needs_I = TRUE;
            auto_do_star_id = TRUE;
        }

        /* Note changed inventory */
        auto_do_crush_junk = TRUE;
        auto_do_crush_hole = TRUE;
        auto_do_crush_slow = TRUE;
    }
}


#endif /* not BORG_TK */

/*
 * Hack -- Cheat the "spell" info (given the book)
 *
 * Hack -- note the use of the "cheat" field for efficiency
 */
void borg_cheat_spell(int book)
{
    int j, what;


    /* Can we use spells/prayers? */
    if (!mb_ptr->spell_book) return;


    /* Process the spells */
    for (what = 0; what < 9; what++)
    {
        /* Access the spell */
        auto_magic *as = &auto_magics[book][what];

        /* Skip illegible spells */
        if (as->status == BORG_MAGIC_ICKY) continue;

        /* Access the index */
        j = as->cheat;

        /* Note "forgotten" spells */
        if ((j < 32) ?
            ((p_ptr->spell_forgotten1 & (1L << j))) :
            ((p_ptr->spell_forgotten2 & (1L << (j - 32)))))
        {
            /* Forgotten */
            as->status = BORG_MAGIC_LOST;
        }

        /* Note "difficult" spells */
        else if (borg_skill[BI_CLEVEL] < as->level)
        {
            /* Unknown */
            as->status = BORG_MAGIC_HIGH;
        }

        /* Note "unknown" spells */
        else if (!((j < 32) ?
                   (p_ptr->spell_learned1 & (1L << j)) :
                   (p_ptr->spell_learned2 & (1L << (j - 32)))))
        {
            /* Unknown */
            as->status = BORG_MAGIC_OKAY;
        }

        /* Note "untried" spells */
        else if (!((j < 32) ?
                   (p_ptr->spell_worked1 & (1L << j)) :
                   (p_ptr->spell_worked2 & (1L << (j - 32)))))
        {
            /* Untried */
            as->status = BORG_MAGIC_TEST;
        }

        /* Note "known" spells */
        else
        {
            /* Known */
            as->status = BORG_MAGIC_KNOW;
        }
    }
}


#ifndef BORG_TK
/*
 * Hack -- Parse the "spell" info (given the book)
 */
void borg_parse_spell(int book)
{
    int what;

    byte t_a;

    char buf[160];


    /* Can we use spells/prayers? */
    if (!mb_ptr->spell_book) return;


    /* Process the spells */
    for (what = 0; what < 9; what++)
    {
        int row = ROW_SPELL + 1 + what;
        int col = COL_SPELL;

        /* Access the spell */
        auto_magic *as = &auto_magics[book][what];

        /* Skip illegible spells */
        if (as->status == BORG_MAGIC_ICKY) continue;

#if 0
        /* Format: "spell-name...................." at col 20+5 */
        if (0 != borg_what_text(col-30, row, -30, &t_a, buf)) continue;
#endif

        /* Format: "Lv Mana Freq Comment" at col 20+35 */
        if (0 != borg_what_text(col, row, -20, &t_a, buf)) continue;

        /* Note "forgotten" spells */
        if (prefix(buf + 13, "forgott"))
        {
            /* Forgotten */
            as->status = BORG_MAGIC_LOST;
        }

        /* Note "difficult" spells */
        else if (borg_skill[BI_CLEVEL] < as->level)
        {
            /* Unknown */
            as->status = BORG_MAGIC_HIGH;
        }

        /* Note "unknown" spells */
        else if (prefix(buf + 13, "unknown"))
        {
            /* Unknown */
            as->status = BORG_MAGIC_OKAY;
        }

        /* Note "untried" spells */
        else if (prefix(buf + 13, "untried"))
        {
            /* Untried */
            as->status = BORG_MAGIC_TEST;
        }

        /* Note "known" spells */
        else
        {
            /* Known */
            as->status = BORG_MAGIC_KNOW;
        }
    }
}

#endif /* not BORG_TK */



/*
 * Prepare a book
 */
static void prepare_book_info(int book)
{
    int i, what;

    int spell[64], num = 0;


    /* Reset each spell entry */
    for (what = 0; what < 9; what++)
    {
        auto_magic *as = &auto_magics[book][what];

        /* Assume no name */
        as->name = NULL;

        /* Assume illegible */
        as->status = BORG_MAGIC_ICKY;

        /* Assume illegible */
        as->method = BORG_MAGIC_ICK;

        /* Impossible values */
        as->level = 99;
        as->power = 99;

        /* Impossible value */
        as->cheat = 99;
    }


    /* Can we use spells/prayers? */
    if (!mb_ptr->spell_book) return;


    /* Extract spells */
    for (i = 0; i < 64; i++)
    {
        /* Check for this spell */
        if ((i < 32) ?
            (spell_flags[mb_ptr->spell_type][book][0] & (1L << i)) :
            (spell_flags[mb_ptr->spell_type][book][1] & (1L << (i - 32))))
        {
            /* Collect this spell */
            spell[num++] = i;
        }
    }


    /* Process each existing spell */
    for (what = 0; what < num; what++)
    {
        auto_magic *as = &auto_magics[book][what];

        magic_type *s_ptr = &mb_ptr->info[spell[what]];

        /* Skip "illegible" spells */
        if (s_ptr->slevel == 99) continue;

        /* Save the spell name */
        as->name = spell_names[mb_ptr->spell_type][spell[what]];

        /* Save the spell index */
        as->cheat = spell[what];

        /* Hack -- assume excessive level */
        as->status = BORG_MAGIC_HIGH;

        /* Access the correct "method" */
        as->method = auto_magic_method[mb_ptr->spell_type][book][what];

        /* Access the correct "rating" */
        as->rating = auto_magic_rating[mb_ptr->spell_type][book][what];

        /* Extract the level and power */
        as->level = s_ptr->slevel;
        as->power = s_ptr->smana;

        /* extract fail rate. */
        as->sfail = s_ptr->sfail;
    }
}



/*
 * Hack -- prepare some stuff based on the player race and class
 */
void prepare_race_class_info(void)
{
    int book;

    /* Initialize the various spell arrays by book */
    for (book = 0; book < 9; book++) prepare_book_info(book);
}



/*
 * Sorting hook -- comp function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
    cptr *text = (cptr*)(u);
    s16b *what = (s16b*)(v);

    int cmp;

    /* Compare the two strings */
    cmp = (strcmp(text[a], text[b]));

    /* Strictly less */
    if (cmp < 0) return (TRUE);

    /* Strictly more */
    if (cmp > 0) return (FALSE);

    /* Enforce "stable" sort */
    return (what[a] <= what[b]);
}


/*
 * Sorting hook -- swap function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
    cptr *text = (cptr*)(u);
    s16b *what = (s16b*)(v);

    cptr texttmp;
    s16b whattmp;

    /* Swap "text" */
    texttmp = text[a];
    text[a] = text[b];
    text[b] = texttmp;

    /* Swap "what" */
    whattmp = what[a];
    what[a] = what[b];
    what[b] = whattmp;
}


void borg_clear_3(void)
{
    C_KILL(auto_items, INVEN_TOTAL, auto_item);
    C_KILL(auto_shops, 9, auto_shop);
    C_KILL(safe_items, INVEN_TOTAL, auto_item);
    C_KILL(safe_home,  STORE_INVEN_MAX, auto_item);
    C_KILL(safe_shops, 8, auto_shop);
    C_KILL(auto_plural_text, auto_plural_size, cptr);
    C_KILL(auto_sv_plural_text, auto_plural_size, cptr);
    C_KILL(auto_plural_what, auto_plural_size, s16b);
    C_KILL(auto_single_text, auto_single_size, cptr);
    C_KILL(auto_single_what, auto_single_size, s16b);
    C_KILL(auto_artego_text, auto_artego_size, cptr);
    C_KILL(auto_sv_art_text, auto_artego_size, cptr);
    C_KILL(auto_artego_what, auto_artego_size, s16b);
}

/*
 * Initialize this file
 *
 * Note that the Borg will never find Grond/Morgoth, but we
 * prepare the item parsers for them anyway.  Actually, the
 * Borg might get lucky and find some of the special artifacts,
 * so it is always best to prepare for a situation if it does
 * not cost any effort.
 *
 * Note that all six artifact "Rings" will parse as "kind 506"
 * (the first artifact ring) and both artifact "Amulets" will
 * parse as "kind 503" (the first of the two artifact amulets),
 * but as long as we use the "name1" field (and not the "kind"
 * or "sval" fields) we should be okay.
 *
 * We sort the two arrays of items names in reverse order, so that
 * we will catch "mace of disruption" before "mace", "Scythe of
 * Slicing" before "Scythe", and for "Ring of XXX" before "Ring".
 *
 * Note that we do not have to parse "plural artifacts" (!)
 *
 * Hack -- This entire routine is a giant hack, but it works
 */
void borg_init_3(void)
{
    int i, k, n;

    int size;

    s16b what[512];
    cptr text[512];

    char buf[256];


    /*** Item/Ware arrays ***/

    /* Make the inventory array */
    C_MAKE(auto_items, INVEN_TOTAL, auto_item);

    /* Make the stores in the town */
    C_MAKE(auto_shops, 9, auto_shop);


    /*** Item/Ware arrays (simulation) ***/

    /* Make the "safe" inventory array */
    C_MAKE(safe_items, INVEN_TOTAL, auto_item);
    C_MAKE(safe_home,  STORE_INVEN_MAX, auto_item);

    /* Make the "safe" stores in the town */
    C_MAKE(safe_shops, 8, auto_shop);

    /*** Plural Object Templates ***/

    /* Start with no objects */
    size = 0;

    /* Analyze some "item kinds" */
    for (k = 1; k < MAX_K_IDX; k++)
    {
        object_type hack;

        /* Get the kind */
        object_kind *k_ptr = &k_info[k];

        /* Skip "empty" items */
        if (!k_ptr->name) continue;

        /* Skip "gold" objects */
        if (k_ptr->tval == TV_GOLD) continue;

        /* Skip "artifacts" */
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        /* Hack -- make an item */
        object_prep(&hack, k);

        /* Describe a "plural" object */
        hack.number = 2;
        object_desc_store(buf, &hack, FALSE, 0);

        /* Save an entry */
        text[size] = string_make(buf);
        what[size] = k;
        size++;
    }

    /* Set the sort hooks */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    C_MAKE(auto_sv_plural_text, MAX_K_IDX, cptr);
    for (i = 0; i < size; i++) auto_sv_plural_text[what[i]] = text[i];

    /* Sort */
    ang_sort(text, what, size);

    /* Save the size */
    auto_plural_size = size;

    /* Allocate the "item parsing arrays" (plurals) */
    C_MAKE(auto_plural_text, auto_plural_size, cptr);
    C_MAKE(auto_plural_what, auto_plural_size, s16b);

    /* Save the entries */
    for (i = 0; i < size; i++) auto_plural_text[i] = text[i];
    for (i = 0; i < size; i++) auto_plural_what[i] = what[i];


    /*** Singular Object Templates ***/

    /* Start with no objects */
    size = 0;

    /* Analyze some "item kinds" */
    for (k = 1; k < MAX_K_IDX; k++)
    {
        object_type hack;

        /* Get the kind */
        object_kind *k_ptr = &k_info[k];

        /* Skip "empty" items */
        if (!k_ptr->name) continue;

        /* Skip "dungeon terrain" objects */
        if (k_ptr->tval == TV_GOLD) continue;

        /* Skip "artifacts" */
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        /* Hack -- make an item */
        object_prep(&hack, k);

        /* Describe a "singular" object */
        hack.number = 1;
        object_desc_store(buf, &hack, FALSE, 0);

        /* Save an entry */
        text[size] = string_make(buf);
        what[size] = k;
        size++;
    }

    /* Analyze the "INSTA_ART" items */
    for (i = 1; i < MAX_A_IDX; i++)
    {
        object_type hack;

        artifact_type *a_ptr = &a_info[i];

        cptr name = (a_name + a_ptr->name);

        /* Skip "empty" items */
        if (!a_ptr->name) continue;

        /* Skip non INSTA_ART things */
        if (!(a_ptr->flags3 & TR3_INSTA_ART)) continue;

        /* Extract the "kind" */
        k = lookup_kind(a_ptr->tval, a_ptr->sval);

        /* Hack -- make an item */
        object_prep(&hack, k);

        /* Save the index */
        hack.name1 = i;

        /* Describe a "singular" object */
        hack.number = 1;
        object_desc_store(buf, &hack, FALSE, 0);

        /* Extract the "suffix" length */
        n = strlen(name) + 1;

        /* Remove the "suffix" */
        buf[strlen(buf) - n] = '\0';

        /* Save an entry */
        text[size] = string_make(buf);
        what[size] = k;
        size++;
    }

    /* Set the sort hooks */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;

    /* Sort */
    ang_sort(text, what, size);

    /* Save the size */
    auto_single_size = size;

    /* Allocate the "item parsing arrays" (plurals) */
    C_MAKE(auto_single_text, auto_single_size, cptr);
    C_MAKE(auto_single_what, auto_single_size, s16b);

    /* Save the entries */
    for (i = 0; i < size; i++) auto_single_text[i] = text[i];
    for (i = 0; i < size; i++) auto_single_what[i] = what[i];


    /*** Artifact and Ego-Item Parsers ***/

    /* No entries yet */
    size = 0;

    /* Collect the "artifact names" */
    for (k = 1; k < MAX_A_IDX; k++)
    {
        artifact_type *a_ptr = &a_info[k];

        /* Skip non-items */
        if (!a_ptr->name) continue;

        /* Extract a string */
        sprintf(buf, " %s", (a_name + a_ptr->name));

        /* Save an entry */
        text[size] = string_make(buf);
        what[size] = k;
        size++;
    }

    C_MAKE(auto_sv_art_text, MAX_A_IDX, cptr);
    for (i = 0; i < size; i++) auto_sv_art_text[what[i]] = text[i];

    /* Collect the "ego-item names" */
    for (k = 1; k < MAX_E_IDX; k++)
    {
        ego_item_type *e_ptr = &e_info[k];

        /* Skip non-items */
        if (!e_ptr->name) continue;

        /* Extract a string */
        sprintf(buf, " %s", (e_name + e_ptr->name));

        /* Save an entry */
        text[size] = string_make(buf);
        what[size] = k + 256;
        size++;
    }

    /* Set the sort hooks */
    ang_sort_comp = ang_sort_comp_hook;
    ang_sort_swap = ang_sort_swap_hook;


    /* Sort */
    ang_sort(text, what, size);

    /* Save the size */
    auto_artego_size = size;

    /* Allocate the "item parsing arrays" (plurals) */
    C_MAKE(auto_artego_text, auto_artego_size, cptr);
    C_MAKE(auto_artego_what, auto_artego_size, s16b);

    /* Save the entries */
    for (i = 0; i < size; i++) auto_artego_text[i] = text[i];
    for (i = 0; i < size; i++) auto_artego_what[i] = what[i];
}

cptr borg_prt_item(int item)
{
            if (item < MAX_K_IDX)
            {
                return auto_sv_plural_text[item];
            }
            if (item < MAX_K_IDX + MAX_K_IDX)
                return auto_sv_plural_text[item - MAX_K_IDX];
            if (item < MAX_K_IDX + MAX_K_IDX + MAX_A_IDX)
                return auto_sv_art_text[item - MAX_K_IDX - MAX_K_IDX];
            return (prefix_pref[item -
                                MAX_K_IDX -
                                MAX_K_IDX -
                                MAX_A_IDX]);

}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
