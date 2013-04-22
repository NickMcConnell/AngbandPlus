/* File: birth.c */

/* Purpose: create a player character */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * How often the autoroller will update the display and pause
 * to check for user interuptions.
 * Bigger values will make the autoroller faster, but slower
 * system may have problems because the user can't stop the
 * autoroller for this number of rolls.
 */
#define AUTOROLLER_STEP 250L

/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100

/* Max quests */
static byte max_quests = 0;

/*
 * Forward declare
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
	cptr info;			    /* Textual History */

	byte roll;			    /* Frequency of this entry */
	byte chart;			    /* Chart index */
	byte next;			    /* Next chart index */
	byte bonus;			    /* Social Class Bonus + 50 */
};


/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human         -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Elf      -->  4 -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Elf/High-Elf  -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
 *
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
static hist_type bg[] =
{
    {"You are the illegitimate and unacknowledged child ",   10, 1, 2, 25},
    {"You are the illegitimate but acknowledged child ",     20, 1, 2, 35},
    {"You are one of several children ",                     95, 1, 2, 45},
    {"You are the first child ",                            100, 1, 2, 50},

    {"of a Serf.  ",                                         40, 2, 3, 65},
    {"of a Yeoman.  ",                                       65, 2, 3, 80},
    {"of a Townsman.  ",                                     80, 2, 3, 90},
    {"of a Guildsman.  ",                                    90, 2, 3, 105},
    {"of a Landed Knight.  ",                                96, 2, 3, 120},
    {"of a Noble Lord.  ",                                   99, 2, 3, 130},
    {"of the Royal Blood Line.  ",                          100, 2, 3, 140},

    {"You are the black sheep of the family.  ",             20, 3, 50, 20},
    {"You are a credit to the family.  ",                    80, 3, 50, 55},
    {"You are a well liked child.  ",                       100, 3, 50, 60},

    {"Your mother was of the Avari.  ",                      25, 4, 1, 40},
    {"Your father was of the Avari.  ",                      40, 4, 1, 50},
    {"Your mother was of the Nandor.  ",                     65, 4, 1, 60},
    {"Your father was of the Nandor.  ",                     80, 4, 1, 60},
    {"Your mother was of the Sindar.  ",                     96, 4, 1, 70},
    {"Your father was of the Sindar.  ",                     99, 4, 1, 70},
    {"Your ancestry traces to Elrond.  ",                   100, 4, 1, 100},

    {"You are one of several children ",                     60, 7, 8, 50},
    {"You are the only child ",                             100, 7, 8, 55},

    {"of a Telerin ",                                        75, 8, 9, 50},
    {"of a Noldorin ",                                       95, 8, 9, 55},
    {"of a Vanyarin ",                                      100, 8, 9, 60},

    {"Ranger.  ",                                            40, 9, 54, 80},
    {"Archer.  ",                                            70, 9, 54, 90},
    {"Warrior.  ",                                          87, 9, 54, 110},
    {"Mage.  ",                                             95, 9, 54, 125},
    {"Prince.  ",                                           99, 9, 54, 140},
    {"King.  ",                                            100, 9, 54, 145},

	{"You are one of several children of a Hobbit ",		 85, 10, 11, 45},
	{"You are the only child of a Hobbit ",		        100, 10, 11, 55},

	{"Bum.  ",							 20, 11, 3, 55},
	{"Tavern Owner.  ",						 30, 11, 3, 80},
	{"Miller.  ",						 40, 11, 3, 90},
	{"Home Owner.  ",						 50, 11, 3, 100},
	{"Burglar.  ",						 80, 11, 3, 110},
	{"Warrior.  ",						 95, 11, 3, 115},
	{"Mage.  ",							 99, 11, 3, 125},
	{"Clan Elder.  ",						100, 11, 3, 140},

	{"You are one of several children of a Gnome ",		 85, 13, 14, 45},
	{"You are the only child of a Gnome ",			100, 13, 14, 55},

	{"Beggar.  ",						 20, 14, 3, 55},
	{"Braggart.  ",						 50, 14, 3, 70},
	{"Prankster.  ",						 75, 14, 3, 85},
	{"Warrior.  ",						 95, 14, 3, 100},
	{"Mage.  ",							100, 14, 3, 125},

	{"You are one of two children of a Dwarven ",		 25, 16, 17, 40},
	{"You are the only child of a Dwarven ",			100, 16, 17, 50},

	{"Thief.  ",						 10, 17, 18, 60},
	{"Prison Guard.  ",						 25, 17, 18, 75},
	{"Miner.  ",						 75, 17, 18, 90},
	{"Warrior.  ",						 90, 17, 18, 110},
	{"Priest.  ",						 99, 17, 18, 130},
	{"King.  ",							100, 17, 18, 150},

	{"You are the black sheep of the family.  ",		 15, 18, 57, 10},
	{"You are a credit to the family.  ",			 85, 18, 57, 50},
	{"You are a well liked child.  ",				100, 18, 57, 55},

	{"Your mother was an Orc, but it is unacknowledged.  ",	 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ",	100, 19, 20, 25},

	{"You are the adopted child ",				100, 20, 2, 50},

	{"Your mother was a Cave-Troll ",				 30, 22, 23, 20},
	{"Your father was a Cave-Troll ",				 60, 22, 23, 25},
	{"Your mother was a Hill-Troll ",				 75, 22, 23, 30},
	{"Your father was a Hill-Troll ",				 90, 22, 23, 35},
	{"Your mother was a Water-Troll ",				 95, 22, 23, 40},
	{"Your father was a Water-Troll ",				100, 22, 23, 45},

	{"Cook.  ",							  5, 23, 62, 60},
	{"Warrior.  ",						 95, 23, 62, 55},
	{"Shaman.  ",						 99, 23, 62, 65},
	{"Clan Chief.  ",						100, 23, 62, 80},

	{"You have dark brown eyes, ",				 20, 50, 51, 50},
	{"You have brown eyes, ",					 60, 50, 51, 50},
	{"You have hazel eyes, ",					 70, 50, 51, 50},
	{"You have green eyes, ",					 80, 50, 51, 50},
	{"You have blue eyes, ",					 90, 50, 51, 50},
	{"You have blue-gray eyes, ",				100, 50, 51, 50},

	{"straight ",						 70, 51, 52, 50},
	{"wavy ",							 90, 51, 52, 50},
	{"curly ",							100, 51, 52, 50},

	{"black hair, ",						 30, 52, 53, 50},
	{"brown hair, ",						 70, 52, 53, 50},
	{"auburn hair, ",						 80, 52, 53, 50},
	{"red hair, ",						 90, 52, 53, 50},
	{"blond hair, ",						100, 52, 53, 50},

	{"and a very dark complexion.",				 10, 53, 0, 50},
	{"and a dark complexion.",					 30, 53, 0, 50},
	{"and an average complexion.",				 80, 53, 0, 50},
	{"and a fair complexion.",					 90, 53, 0, 50},
	{"and a very fair complexion.",				100, 53, 0, 50},

	{"You have light grey eyes, ",				 85, 54, 55, 50},
	{"You have light blue eyes, ",				 95, 54, 55, 50},
	{"You have light green eyes, ",				100, 54, 55, 50},

	{"straight ",						 75, 55, 56, 50},
	{"wavy ",							100, 55, 56, 50},

	{"black hair, and a fair complexion.",			 75, 56, 0, 50},
	{"brown hair, and a fair complexion.",			 85, 56, 0, 50},
	{"blond hair, and a fair complexion.",			 95, 56, 0, 50},
	{"silver hair, and a fair complexion.",			100, 56, 0, 50},

	{"You have dark brown eyes, ",				 99, 57, 58, 50},
	{"You have glowing red eyes, ",				100, 57, 58, 60},

	{"straight ",						 90, 58, 59, 50},
	{"wavy ",							100, 58, 59, 50},

	{"black hair, ",						 75, 59, 60, 50},
	{"brown hair, ",						100, 59, 60, 50},

	{"a one foot beard, ",					 25, 60, 61, 50},
	{"a two foot beard, ",					 60, 60, 61, 51},
	{"a three foot beard, ",					 90, 60, 61, 53},
	{"a four foot beard, ",					100, 60, 61, 55},

	{"and a dark complexion.",					100, 61, 0, 50},

	{"You have slime green eyes, ",				 60, 62, 63, 50},
	{"You have puke yellow eyes, ",				 85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ",				 99, 62, 63, 50},
	{"You have glowing red eyes, ",				100, 62, 63, 55},

	{"dirty ",							 33, 63, 64, 50},
	{"mangy ",							 66, 63, 64, 50},
	{"oily ",							100, 63, 64, 50},

	{"sea-weed green hair, ",					 33, 64, 65, 50},
	{"bright red hair, ",					 66, 64, 65, 50},
	{"dark purple hair, ",					100, 64, 65, 50},

	{"and green ",						 25, 65, 66, 50},
	{"and blue ",						 50, 65, 66, 50},
	{"and white ",						 75, 65, 66, 50},
	{"and black ",						100, 65, 66, 50},

	{"ulcerous skin.",						 33, 66, 0, 50},
	{"scabby skin.",						 66, 66, 0, 50},
	{"leprous skin.",                       100, 66, 0, 50},

	{"You are one of several children of a Dark Elven ",      85, 69, 70, 45},
	{"You are the only child of a Dark Elven ",          100, 69, 70, 55},

	{"Warrior.  ", 50, 70, 71, 60 },
	{"Warlock.  ", 80, 70, 71, 75 },
	{"Noble.  ", 100, 70, 71, 95 },

	{"You have black eyes, ", 100, 71, 72, 50},

	{"straight ",                        70, 72, 73, 50},
	{"wavy ",                            90, 72, 73, 50},
	{"curly ",                          100, 72, 73, 50},

	{"black hair and a very dark complexion.", 100, 73, 0, 50 },

	{"Your mother was an Ogre, but it is unacknowledged.  ", 25, 74, 20, 25},
	{"Your father was an Ogre, but it is unacknowledged.  ", 100, 74, 20, 25},

	{"Your mother was a Hill Giant.  ", 10, 75, 20, 50},
	{"Your mother was a Fire Giant.  ", 12, 75, 20, 55},
	{"Your mother was a Frost Giant.  ", 20, 75, 20, 60},
	{"Your mother was a Cloud Giant.  ", 23, 75, 20, 65},
	{"Your mother was a Storm Giant.  ", 25, 75, 20, 70},
	{"Your father was a Hill Giant.  ",  60, 75, 20, 50},
	{"Your father was a Fire Giant.  ",  70, 75, 20, 55},
	{"Your father was a Frost Giant.  ",  80, 75, 20, 60},
	{"Your father was a Cloud Giant.  ",  90, 75, 20, 65},
	{"Your father was a Storm Giant.  ", 100, 75, 20, 70},

	{"Your father was an unknown Titan.  ", 75, 76, 20, 50 },
	{"Your mother was Themis.  ",        80, 76, 20, 100 },
	{"Your mother was Mnemosyne.  ",     85, 76, 20, 100 },
	{"Your father was Okeanoas.  ",      90, 76, 20, 100 },
	{"Your father was Crius.  ",         95, 76, 20, 100 },
	{"Your father was Hyperion.  ",      98, 76, 20, 125 },
	{"Your father was Kronos.  ",       100, 76, 20, 150 },

	{"You are the offspring of an unknown Cyclops.  ", 90, 77, 109, 50 },
	{"You are Polyphemos's child.  ", 98, 77, 109, 80 },
	{"You are Uranos's child.  ", 100, 77, 109, 135 },

	{"You are one of several children of ", 100, 78, 79, 50 },

	{"a Brown Yeek. ", 50, 79, 80, 50 },
	{"a Blue Yeek.  ", 75, 79, 80, 50 },
	{"a Master Yeek.  ", 95, 79, 80, 85 },
	{"Boldor, the King of the Yeeks.  ", 100, 79, 80, 120 },

	{"You have pale eyes, ",    25, 80, 81, 50 },
	{"You have glowing eyes, ",    50, 80, 81, 50 },
	{"You have tiny black eyes, ",    75, 80, 81, 50 },
	{"You have shining black eyes, ",    100, 80, 81, 50 },

	{"no hair at all, ",        20, 81, 65, 50 },
	{"short black hair, ",        40, 81, 65, 50 },
	{"long black hair, ",        60, 81, 65, 50 },
	{"bright red hair, ",        80, 81, 65, 50 },
	{"colourless albino hair, ",        100, 81, 65, 50 },

	{"You are one of several children of ", 100, 82, 83, 50 },

	{"a Small Kobold.  ",   40, 83, 80, 50 },
	{"a Kobold.  ",         75, 83, 80, 55 },
	{"a Large Kobold.  ",   95, 83, 80, 65 },
	{"Mughash, the Kobold Lord.  ",     100, 83, 80, 100 },

    {"You are one of several children ",                     85, 84, 85, 45},
    {"You are the first child ",                            100, 84, 85, 50},

    {"of a Serf.  ",                                         60, 85, 50, 40},
    {"of a Devoted Mercenary.  ",                            85, 85, 50, 55},
    {"of a Landed Knight  ",                                 96, 85, 50, 60},
    {"of a Marshal of the Riddermark.  ",                    99, 85, 50, 100},
    {"of a King of the Rohirrim.  ",                        100, 85, 50, 120},

	{"You are one of several children of ", 100, 87, 88, 89 },

	{"a Nibelung Slave.  ", 30, 88, 18, 20 },
	{"a Nibelung Thief.  ", 50, 88, 18, 40 },
	{"a Nibelung Smith.  ", 70, 88, 18, 60 },
	{"a Nibelung Miner.  ", 90, 88, 18, 75 },
	{"a Nibelung Shaman.  ", 95, 88, 18, 100 },
	{"Mime, the Nibelung.  ", 100, 88, 18, 100 },

        {"You are one of several children of a DragonRider. ", 85, 89, 91, 50  },
        {"You are the only child of a DragonRider. ", 100, 89, 91, 60 },

        {"You have a Green Dragon.", 30, 91, 0, 40 },
        {"You have a Blue Dragon.", 55, 91, 0, 60 },
        {"You have a Brown Dragon.", 80, 91, 0, 80 },
        {"You have a Bronze Dragon.", 90, 91, 0, 100 },
        {"You have a Gold Dragon.", 100, 91, 0, 120},

	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80 },
	{"three tentacles around your mouth.", 20, 93, 0, 45 },
	{"four tentacles around your mouth.", 80, 93, 0, 50 },
	{"five tentacles around your mouth.", 100, 93, 0, 55 },

    {"You are of an unknown generation of the Ents.",        30, 94, 95, 30},
    {"You are of the third generation of the Ents.",         40, 94, 95, 50},
    {"You are of the second generation of the Ents.",        60, 94, 95, 60},
    {"You are of the first beings who awoke on Arda.",      100, 94, 95, 80},

    {"You have green skin and unflexible members.",          50, 95, 0, 50},
    {"You have brown skin and unflexible members.",         100, 95, 0, 50},

	{"You were created by ", 100, 107, 108, 50 },

	{"a Necromancer.  ", 30, 108, 62, 50 },
	{"a Wizard.  ", 50, 108, 62, 50 },
	{"a restless spirit.  ",60, 108, 62, 50 },
	{"an Evil Priest.  ", 70, 108, 62, 50 },
	{"a pact with the demons.  ", 80, 108, 62, 50 },
	{"a curse.  ", 95, 108, 62, 30 },
	{"an oath.  ", 100, 108, 62, 50 },

	{"You have a dark brown eye, ",               20, 109, 110, 50},
	{"You have a brown eye, ",                    60, 109, 110, 50},
	{"You have a hazel eye, ",                    70, 109, 110, 50},
	{"You have a green eye, ",                    80, 109, 110, 50},
	{"You have a blue eye, ",                     90, 109, 110, 50},
	{"You have a blue-gray eye, ",               100, 109, 110, 50},

	{"straight ",                        70, 110, 111, 50},
	{"wavy ",                            90, 110, 111, 50},
	{"curly ",                          100, 110, 111, 50},

	{"black hair, ",                         30, 111, 112, 50},
	{"brown hair, ",                         70, 111, 112, 50},
	{"auburn hair, ",                        80, 111, 112, 50},
	{"red hair, ",                       90, 111, 112, 50},
	{"blond hair, ",                        100, 111, 112, 50},

	{"and a very dark complexion.",              10, 112, 0, 50},
	{"and a dark complexion.",                   30, 112, 0, 50},
	{"and an average complexion.",               80, 112, 0, 50},
	{"and a fair complexion.",                   90, 112, 0, 50},
	{"and a very fair complexion.",             100, 112, 0, 50},

	{"You arose from an unmarked grave.  ", 20, 113, 114, 50 },
	{"In life you were a simple peasant, the victim of a powerful Vampire Lord.  ", 40, 109, 110, 50 },
	{"In life you were a Vampire Hunter, but they got you.  ", 60, 113, 114, 50 },
	{"In life you were a Necromancer.  ", 80, 113, 114, 50 },
	{"In life you were a powerful noble.  ", 95, 113, 114, 50 },
	{"In life you were a powerful and cruel tyrant.  ", 100, 113, 114, 50 },

	{"You have ", 100, 114, 115, 50 },

	{"jet-black hair, ", 25, 115, 116, 50 },
	{"matted brown hair, ", 50, 115, 116, 50 },
	{"white hair, ", 75, 115, 116, 50 },
	{"a hairless head, ", 100, 115, 116, 50 },

	{"eyes like red coals, ", 25, 116, 117, 50 },
	{"blank white eyes, ", 50, 116, 117, 50 },
	{"feral yellow eyes, ", 75, 116, 117, 50 },
	{"bloodshot red eyes, ", 100, 116, 117, 50 },

	{"and a deathly pale complexion.", 100, 117, 0, 50 },

	{"You were created by ", 100, 118, 119, 50 },

	{"a Necromancer.  ", 30, 119, 134, 50 },
	{"a magical experiment.  ", 50, 119, 134, 50 },
	{"an Evil Priest.  ", 70, 119, 134, 50 },
	{"a pact with the demons.  ", 75, 119, 134, 50 },
	{"a restless spirit.  ", 85, 119, 134, 50 },
	{"a curse.  ", 95, 119, 134, 30 },
	{"an oath.  ", 100, 119, 134, 50 },

	{"jet-black hair, ", 25, 120, 121, 50 },
	{"matted brown hair, ", 50, 120, 121, 50 },
	{"white hair, ", 75, 120, 121, 50 },
	{"a hairless head, ", 100, 120, 121, 50 },

	{"eyes like red coals, ", 25, 121, 122, 50 },
	{"blank white eyes, ", 50, 121, 122, 50 },
	{"feral yellow eyes, ", 75, 121, 122, 50 },
	{"bloodshot red eyes, ", 100, 121, 122, 50 },

	{" and a deathly gray complexion. ", 100, 122, 123, 50 },
	{"An eerie green aura surrounds you.", 100, 123, 0, 50 },

	{"Your parents were ", 100, 124, 125, 50 },

	{"pixies.  ", 20, 125, 126, 35 },
	{"nixies.  ", 30, 125, 126, 25 },
	{"wood sprites.  ", 75, 125, 126, 50 },
	{"wood spirits.  ", 90, 125, 126, 75 },
	{"noble faerie folk.  ", 100, 125, 126, 85 },

	{"You have light blue wings attached to your back, ", 100, 126, 127, 50 },

	{"straight blond hair, ",                        80, 127, 128, 50},
	{"wavy blond hair, ",                            100, 127, 128, 50},

	{"blue eyes, and a very fair complexion.", 100, 128, 0, 50},

	{"You were produced by a magical experiment.  ", 30, 129, 130, 40},
        {"In your childhood, you were stupid enough to stick your head in raw chaos.  ",
	50, 129, 130, 50 },
	{"A Demon Lord of Chaos decided to have some fun, and so he created you.  ",
	60, 129, 130, 60 },
	{"You are the magical crossbreed of an animal and a man.  ", 75, 129, 130, 50},
	{"You are the blasphemous crossbreed of unspeakable creatures of chaos.  ", 100, 129, 130, 30},


	{"You have green reptilian eyes, ",              60, 130, 131, 50},
	{"You have the black eyes of a bird, ",              85, 130, 131, 50},
	{"You have the orange eyes of a cat, ",               99, 130, 131, 50},
	{"You have the fiery eyes of a demon, ",             100, 130, 131, 55},

	{"no hair at all, ",                 10, 131, 133, 50 },
	{"dirty ",                           33, 131, 132, 50},
	{"mangy ",                           66, 131, 132, 50},
	{"oily ",                           100, 131, 132, 50},

	{"brown fur, ",                    33, 132, 133, 50},
	{"gray fur, ",                    66, 132, 133, 50},
	{"albino fur, ",                  100, 132, 133, 50},

	{"and the hooves of a goat.",      50, 133, 0, 50 },
	{"and human feet.",        75, 133, 0, 50 },
	{"and bird's feet.",       85, 133, 0, 50 },
	{"and reptilian feet.",    90, 133, 0, 50 },
	{"and bovine feet.",       95, 133, 0, 50 },
	{"and feline feet.",       97, 133, 0, 50 },
	{"and canine feet.",       100, 133, 0, 50 },

	{"You have ", 100, 134, 120, 50 },

	/* Death mold description */
	
        {"You were born in dirty bilge-water, ", 10, 200, 201, 30},
        {"You were born in dirty straw, ", 20, 200, 201, 35},
        {"You were born in wet mud, ", 30, 200, 201, 40},
        {"You were born in a pile of dust, ", 40, 200, 201, 45},
        {"You were born in sand, ", 50, 200, 201, 50},
        {"You were born in pebbles, ", 60, 200, 201, 50},
        {"You were born in a kobold corpse, ", 70, 200, 201, 55},
        {"You were born in dragon droppings, ", 80, 200, 201, 60},
        {"You were born in a pile of bones, ", 90, 200, 201, 65},
        {"You were born in a corpse of a mighty hero, ", 100, 200, 201, 70},

        {"created by rotting flesh. ",              10, 201, 202, 30},
        {"created by a kobold magician. ",          20, 201, 202, 35},
        {"created by a corrupted apprentice. ",     30, 201, 202, 40},
        {"created by a curious mage apprentice. ",  40, 201, 202, 45},
        {"created by an evil Beastmaster. ",        50, 201, 202, 50},
        {"created by a practicing Necromancer. ",   60, 201, 202, 50},
        {"created by the Mutant Breeders. ",        70, 201, 202, 55},
        {"created by a curious adventurer. ",       80, 201, 202, 60},
        {"called to life by the Witch-King of Angmar. ",  90, 201, 202, 65},
        {"called to life by Sauron himself. ", 100, 201, 202, 70},

        {"Since then you have given life to ",      100, 202, 203, 50},

        {"no ",                                      10, 203, 204, 30},
        {"one weak-willed ",          20, 203, 204, 35},
        {"two ",     30, 203, 204, 40},
        {"three ",  40, 203, 204, 45},
        {"four ",        50, 203, 204, 50},
        {"five ",   60, 203, 204, 50},
        {"about twenty ",        70, 203, 204, 55},
        {"dozens of ",       80, 203, 204, 60},
        {"hundreds of ",  90, 203, 204, 65},
        {"uncounted multitudes of ", 100, 203, 204, 70},

        {"foul offspring. ", 100, 204, 0, 50},

        {"You are one of five children of a blue Yeek.  ", 25,205,3,  50},
        {"You are one of five children of a brown Yeek.  ", 75,205,3,  75},
        {"You are one of five children on a master yeek.  ", 100,205,3, 100},
};



/*
 * Current stats
 */
static s16b stat_use[6];

/*
 * Autoroll limit
 */
static s16b stat_limit[6];

/*
 * Autoroll matches
 */
static s32b stat_match[6];

/*
 * Autoroll round
 */
static s32b auto_round;

/*
 * Last round
 */
static s32b last_round;

/* Dwarves */
static char *dwarf_syllable1[] =
{
	"B", "D", "F", "G", "Gl", "H", "K", "L", "M", "N", "R", "S", "T", "Th", "V",
};

static char *dwarf_syllable2[] =
{
	"a", "e", "i", "o", "oi", "u",
};

static char *dwarf_syllable3[] =
{
	"bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar", "nus", "rin", "ran", "sin", "sil", "sur",
};

/* Elves */
static char *elf_syllable1[] =
{
	"Al", "An", "Bal", "Bel", "Cal", "Cel", "El", "Elr", "Elv", "Eow", "Ear", "F", "Fal", "Fel", "Fin", "G", "Gal", "Gel", "Gl", "Is", "Lan", "Leg", "Lom", "N", "Nal", "Nel",  "S", "Sal", "Sel", "T", "Tal", "Tel", "Thr", "Tin",
};

static char *elf_syllable2[] =
{
	"a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra", "ilma", "il-Ga", "ili", "o", "orfi", "u", "y",
};

static char *elf_syllable3[] =
{
	"l", "las", "lad", "ldor", "ldur", "linde", "lith", "mir", "n", "nd", "ndel", "ndil", "ndir", "nduil", "ng", "mbor", "r", "rith", "ril", "riand", "rion", "s", "thien", "viel", "wen", "wyn",
};

/* Gnomes */
static char *gnome_syllable1[] =
{
	"Aar", "An", "Ar", "As", "C", "H", "Han", "Har", "Hel", "Iir", "J", "Jan", "Jar", "K", "L", "M", "Mar", "N", "Nik", "Os", "Ol", "P", "R", "S", "Sam", "San", "T", "Ter", "Tom", "Ul", "V", "W", "Y",
};

static char *gnome_syllable2[] =
{
	"a", "aa",  "ai", "e", "ei", "i", "o", "uo", "u", "uu",
};

static char *gnome_syllable3[] =
{
	"ron", "re", "la", "ki", "kseli", "ksi", "ku", "ja", "ta", "na", "namari", "neli", "nika", "nikki", "nu", "nukka", "ka", "ko", "li", "kki", "rik", "po", "to", "pekka", "rjaana", "rjatta", "rjukka", "la", "lla", "lli", "mo", "nni",
};

/* Hobbit */
static char *hobbit_syllable1[] =
{
	"B", "Ber", "Br", "D", "Der", "Dr", "F", "Fr", "G", "H", "L", "Ler", "M", "Mer", "N", "P", "Pr", "Per", "R", "S", "T", "W",
};

static char *hobbit_syllable2[] =
{
	"a", "e", "i", "ia", "o", "oi", "u",
};

static char *hobbit_syllable3[] =
{
	"bo", "ck", "decan", "degar", "do", "doc", "go", "grin", "lba", "lbo", "lda", "ldo", "lla", "ll", "lo", "m", "mwise", "nac", "noc", "nwise", "p", "ppin", "pper", "tho", "to",
};

/* Human */
static char *human_syllable1[] =
{
	"Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar", "B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et", "Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha", "Ib", "Jer", "K", "Ka", "Ked", "L", "Loth", "Lar", "Leg", "M", "Mir", "N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T", "Tr", "Th", "V", "Y", "Z", "W", "Wic",
};

static char *human_syllable2[] =
{
	"a", "ae", "au", "ao", "are", "ale", "ali", "ay", "ardo", "e", "ei", "ea", "eri", "era", "ela", "eli", "enda", "erra", "i", "ia", "ie", "ire", "ira", "ila", "ili", "ira", "igo", "o", "oa", "oi", "oe", "ore", "u", "y",
};

static char *human_syllable3[] =
{
	"a", "and", "b", "bwyn", "baen", "bard", "c", "ctred", "cred", "ch", "can", "d", "dan", "don", "der", "dric", "dfrid", "dus", "f", "g", "gord", "gan", "l", "li", "lgrin", "lin", "lith", "lath", "loth", "ld", "ldric", "ldan", "m", "mas", "mos", "mar", "mond", "n", "nydd", "nidd", "nnon", "nwan", "nyth", "nad", "nn", "nnor", "nd", "p", "r", "ron", "rd", "s", "sh", "seth", "sean", "t", "th", "tha", "tlan", "trem", "tram", "v", "vudd", "w", "wan", "win", "wyn", "wyr", "wyr", "wyth",
};

/* Orc */
static char *orc_syllable1[] =
{
	"B", "Er", "G", "Gr", "H", "P", "Pr", "R", "V", "Vr", "T", "Tr", "M", "Dr",
};

static char *orc_syllable2[] =
{
	"a", "i", "o", "oo", "u", "ui",
};

static char *orc_syllable3[] =
{
	"dash", "dish", "dush", "gar", "gor", "gdush", "lo", "gdish", "k", "lg", "nak", "rag", "rbag", "rg", "rk", "ng", "nk", "rt", "ol", "urk", "shnak", "mog", "mak", "rak",
};

/* Ent */
static char *entish_syllable1[] =
{
	"Tree","Root","Bark","Beam","Leaf",
};

static char *entish_syllable2[] =
{
	"-",
};

static char *entish_syllable3[] =
{
	"tender","planter","shepherd","watcher","grower","warden",
};

/* Mold */
static char *cthuloid_syllable1[] =
{
	"Cth","Az","Fth","Ts","Xo","Q'N","R'L","Ghata","L","Zz","Fl","Cl","S","Y",
};

static char *cthuloid_syllable2[] =
{
	"nar","loi","ul","lu","noth","thon","ath","'N","rhy","oth","aza","agn","oa","og",
};

static char *cthuloid_syllable3[] =
{
	"l","a","u","oa","oggua","oth","ath","aggua","lu","lo","loth","lotha","agn","axl",
};

/*
 * Random Name Generator
 * based on a Javascript by Michael Hensley
 * "http://geocities.com/timessquare/castle/6274/"
 */
void create_random_name(int race, char *name)
{
	/* Paranoia */
	if (!name) return;

	/* Select the monster type */
	switch (race)
	{
		/* Create the monster name */
	case RACE_DWARF:
	case RACE_HALF_GIANT:
	case RACE_NIBELUNG:
		strcpy(name, dwarf_syllable1[rand_int(sizeof(dwarf_syllable1) / sizeof(char*))]);
		strcat(name, dwarf_syllable2[rand_int(sizeof(dwarf_syllable2) / sizeof(char*))]);
		strcat(name, dwarf_syllable3[rand_int(sizeof(dwarf_syllable3) / sizeof(char*))]);
		break;
	case RACE_DARK_ELF:
	case RACE_ELF:
        case RACE_WOOD_ELF:
	case RACE_HALF_ELF:
	case RACE_HIGH_ELF:
		strcpy(name, elf_syllable1[rand_int(sizeof(elf_syllable1) / sizeof(char*))]);
		strcat(name, elf_syllable2[rand_int(sizeof(elf_syllable2) / sizeof(char*))]);
		strcat(name, elf_syllable3[rand_int(sizeof(elf_syllable3) / sizeof(char*))]);
		break;
	case RACE_GNOME:
		strcpy(name, gnome_syllable1[rand_int(sizeof(gnome_syllable1) / sizeof(char*))]);
		strcat(name, gnome_syllable2[rand_int(sizeof(gnome_syllable2) / sizeof(char*))]);
		strcat(name, gnome_syllable3[rand_int(sizeof(gnome_syllable3) / sizeof(char*))]);
		break;
	case RACE_HOBBIT:
	case RACE_YEEK:
		strcpy(name, hobbit_syllable1[rand_int(sizeof(hobbit_syllable1) / sizeof(char*))]);
		strcat(name, hobbit_syllable2[rand_int(sizeof(hobbit_syllable2) / sizeof(char*))]);
		strcat(name, hobbit_syllable3[rand_int(sizeof(hobbit_syllable3) / sizeof(char*))]);
		break;
	case RACE_DUNADAN:
	case RACE_HUMAN:
        case RACE_RKNIGHT:
        case RACE_DRAGONRIDER:
		strcpy(name, human_syllable1[rand_int(sizeof(human_syllable1) / sizeof(char*))]);
		strcat(name, human_syllable2[rand_int(sizeof(human_syllable2) / sizeof(char*))]);
		strcat(name, human_syllable3[rand_int(sizeof(human_syllable3) / sizeof(char*))]);
		break;
	case RACE_HALF_OGRE:
	case RACE_HALF_ORC:
	case RACE_HALF_TROLL:
	case RACE_KOBOLD:
		strcpy(name, orc_syllable1[rand_int(sizeof(orc_syllable1) / sizeof(char*))]);
		strcat(name, orc_syllable2[rand_int(sizeof(orc_syllable2) / sizeof(char*))]);
		strcat(name, orc_syllable3[rand_int(sizeof(orc_syllable3) / sizeof(char*))]);
		break;
	case RACE_ENT:
		strcpy(name, entish_syllable1[rand_int(sizeof(entish_syllable1) / sizeof(char*))]);
		strcat(name, entish_syllable2[rand_int(sizeof(entish_syllable2) / sizeof(char*))]);
		strcat(name, entish_syllable3[rand_int(sizeof(entish_syllable3) / sizeof(char*))]);
		break;
        case RACE_MOLD:
		strcpy(name, cthuloid_syllable1[rand_int(sizeof(cthuloid_syllable1) / sizeof(char*))]);
		strcat(name, cthuloid_syllable2[rand_int(sizeof(cthuloid_syllable2) / sizeof(char*))]);
		strcat(name, cthuloid_syllable3[rand_int(sizeof(cthuloid_syllable3) / sizeof(char*))]);
		break;
		/* Create an empty name */
	default:
		name[0] = '\0';
		break;
	}
}


byte choose_realm(s32b choices)
{

	int picks[MAX_REALM] = {0};
	int k, n;

	char c;

	char p2 = ')';

	char buf[80];

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"The realm of magic will determine which spells you can learn.");

	n = 0;

        if ((choices & CH_VALARIN) && (p_ptr->realm1 != REALM_VALARIN))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Valarin");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=1;
		n++;
	}

        if ((choices & CH_MAGERY) && (p_ptr->realm1 != REALM_MAGERY))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Magery");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=2;
		n++;
	}

        if ((choices & CH_SIGALDRY) && (p_ptr->realm1 != REALM_SIGALDRY))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Sigaldry");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=7;
		n++;
	}


        if ((choices & CH_CRUSADE) && (p_ptr->realm1 != REALM_CRUSADE))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Crusade");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=6;
		n++;
	}

        if ((choices & CH_SHADOW) && (p_ptr->realm1 != REALM_SHADOW))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Shadow");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=3;
		n++;
	}

	if ((choices & CH_CHAOS) && (p_ptr->realm1 != REALM_CHAOS))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Chaos");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=4;
		n++;
	}

        if ((choices & CH_NETHER) && (p_ptr->realm1 != REALM_NETHER))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Nether");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=5;
		n++;
	}

        if ((choices & CH_TRIBAL) && (p_ptr->realm1 != REALM_TRIBAL))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Tribal");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
                picks[n]=13;
		n++;
	}

        if ((choices & CH_DRUID) && (p_ptr->realm1 != REALM_DRUID))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Druidic");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
                picks[n]=14;
		n++;
	}

        if ((choices & CH_ILLUSION) && (p_ptr->realm1 != REALM_ILLUSION))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Illusion");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
                picks[n]=12;
		n++;
	}

        if ((choices & CH_MAGIC) && (p_ptr->realm1 != REALM_MAGIC))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Vanilla Magic");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
                picks[n]=REALM_MAGIC;
		n++;
	}

        if ((choices & CH_PRAYER) && (p_ptr->realm1 != REALM_PRAYER))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Vanilla Prayer");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
                picks[n]=REALM_PRAYER;
		n++;
	}

        if ((choices & CH_SPIRIT) && (p_ptr->realm1 != REALM_SPIRIT))
	{
                sprintf(buf, "%c%c %s", I2A(n), p2, "Spirit");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
                picks[n] = REALM_SPIRIT;
		n++;
	}

        /* Get a realm */
	while (1)
	{
                sprintf(buf, "Choose a realm (%c-%c), * for random, = for options: ", I2A(0), I2A(n-1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == '*')
		{
			k = randint(n) - 1;
			break;
		}
                k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
		if (c == '?') do_cmd_help();
		else if (c == '=')
		{
			screen_save();
                        do_cmd_options_aux(6, "Startup Options", FALSE);
			screen_load();
		}
		else bell();
	}


	/* Clean up */
	clear_from(15);

	return (picks[k]);
}

static void get_realms()
{
	int pclas = p_ptr->pclass;
        s32b choices = 0;

	/* First we have null realms */
	p_ptr->realm1 = p_ptr->realm2 = REALM_NONE;

        /* hack Sorcerors */
        if (p_ptr->pclass == CLASS_SORCERER)
        {
                p_ptr->realm1 = REALM_MAGERY;
                p_ptr->realm2 = REALM_SIGALDRY;
                return;
        }

	/* Warriors and certain others get no realms */

        if ((Mrealm_choices[pclas] | mrealm_choices[pclas]) == (CH_NONE)) return;

	/* Other characters get at least one realm */

	switch (pclas)
	{
	default:
                if(Mrealm_choices[pclas])
                {
                        if(count_bits(Mrealm_choices[pclas]) > 1)
                                p_ptr->realm1 = choose_realm(Mrealm_choices[pclas]);
                        else
                        {
                                int i;

                                for(i = 1; i < MAX_REALM; i++)
                                        if(Mrealm_choices[pclas] & (1 << (i - 1)))
                                                p_ptr->realm1 = i;
                        }
                }
                else
                {
                        if(count_bits(mrealm_choices[pclas]) > 1)
                                p_ptr->realm1 = choose_realm(mrealm_choices[pclas]);
                        else
                        {
                                int i;

                                for(i = 1; i < MAX_REALM; i++)
                                        if(mrealm_choices[pclas] & (1 << (i - 1)))
                                                p_ptr->realm1 = i;
                        }
                }
	}

        choices = mrealm_choices[pclas];

        if((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_PRIEST))
        {
                switch(p_ptr->realm1)
                {
                        case REALM_MAGIC:
                        case REALM_PRAYER:
                                p_ptr->realm2 = 0;
                                return;
                                break;
                        case REALM_VALARIN:
                                choices &= ~CH_SIGALDRY;
                                choices &= ~CH_ILLUSION;
                                break;
                        case REALM_NETHER:
                                choices &= ~CH_SPIRIT;
                                choices &= ~CH_CRUSADE;
                                break;
                        case REALM_MAGERY:
                                choices &= ~CH_SPIRIT;
                                choices &= ~CH_ILLUSION;
                                break;
                        case REALM_SHADOW:
                                choices &= ~CH_CRUSADE;
                                choices &= ~CH_SIGALDRY;
                                break;
                }
        }

        if(p_ptr->pclass == CLASS_HIGH_MAGE)
        {
                switch(p_ptr->realm1)
                {
                        case REALM_VALARIN:
                        case REALM_NETHER:
                        case REALM_MAGERY:
                        case REALM_SHADOW:
                                p_ptr->realm2 = 0;
                                return;
                                break;
                }
        }

        if (count_bits(choices) > 1)
                p_ptr->realm2 = choose_realm(choices);
        else
        {
                int i;

                for(i = 1; i < MAX_REALM; i++)
                        if(choices & (1 << (i - 1)))
                                p_ptr->realm2 = i;
        }
}

/*
 * Save the current data for later
 */
static void save_prev_data(void)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
        previous_char.sex = p_ptr->psex;
        previous_char.race = p_ptr->prace;
        previous_char.rmod = p_ptr->pracem;
        previous_char.class = p_ptr->pclass;

        previous_char.realm1 = p_ptr->realm1;
        previous_char.realm2 = p_ptr->realm2;

        previous_char.quests = max_quests;

        previous_char.god = p_ptr->pgod;
        previous_char.grace = p_ptr->grace;
        previous_char.god_favor = p_ptr->god_favor;

        previous_char.age = p_ptr->age;
        previous_char.wt = p_ptr->wt;
        previous_char.ht = p_ptr->ht;
        previous_char.sc = p_ptr->sc;
        previous_char.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
                previous_char.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the chaos patron */
        previous_char.chaos_patron = p_ptr->chaos_patron;

	/* Save the weapon specialty */
        previous_char.weapon = p_ptr->class_extra1;

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
                strcpy(previous_char.history[i], history[i]);
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(bool save)
{
	int i;

	birther	temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->age;
	temp.wt = p_ptr->wt;
	temp.ht = p_ptr->ht;
	temp.sc = p_ptr->sc;
	temp.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
		temp.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the chaos patron */
	temp.chaos_patron = p_ptr->chaos_patron;

	/* Save the weapon specialty */
	temp.weapon = p_ptr->class_extra1;

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(temp.history[i], history[i]);
	}


	/*** Load the previous data ***/

	/* Load the data */
        p_ptr->age = previous_char.age;
        p_ptr->wt = previous_char.wt;
        p_ptr->ht = previous_char.ht;
        p_ptr->sc = previous_char.sc;
        p_ptr->au = previous_char.au;

	/* Load the stats */
	for (i = 0; i < 6; i++)
	{
                p_ptr->stat_max[i] = previous_char.stat[i];
                p_ptr->stat_cur[i] = previous_char.stat[i];
	}

	/* Load the chaos patron */
        p_ptr->chaos_patron = previous_char.chaos_patron;

	/* Load the weapon specialty */
        p_ptr->class_extra1 = previous_char.weapon;

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
                strcpy(history[i], previous_char.history[i]);
	}


	/*** Save the current data ***/
        if (!save) return;

	/* Save the data */
        previous_char.age = temp.age;
        previous_char.wt = temp.wt;
        previous_char.ht = temp.ht;
        previous_char.sc = temp.sc;
        previous_char.au = temp.au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
                previous_char.stat[i] = temp.stat[i];
	}

	/* Save the chaos patron */
        previous_char.chaos_patron = temp.chaos_patron;

	/* Save the weapon specialty */
        previous_char.weapon = temp.weapon;

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
                strcpy(previous_char.history[i], temp.history[i]);
	}
}




/*
 * Returns adjusted stat -JK-  Algorithm by -JWT-
 *
 * auto_roll is boolean and states maximum changes should be used rather
 * than random ones to allow specification of higher values to wait for
 *
 * The "p_ptr->maximize" code is important	-BEN-
 */
static int adjust_stat(int value, int amount, int auto_roll)
{
	int i;

	/* Negative amounts */
	if (amount < 0)
	{
		/* Apply penalty */
		for (i = 0; i < (0 - amount); i++)
		{
			if (value >= 18+10)
			{
				value -= 10;
			}
			else if (value > 18)
			{
				value = 18;
			}
			else if (value > 3)
			{
				value--;
			}
		}
	}

	/* Positive amounts */
	else if (amount > 0)
	{
		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else if (p_ptr->maximize)
			{
				value += 10;
			}
			else if (value < 18+70)
			{
				value += ((auto_roll ? 15 : randint(15)) + 5);
			}
			else if (value < 18+90)
			{
				value += ((auto_roll ? 6 : randint(6)) + 2);
			}
			else if (value < 18+100)
			{
				value++;
			}
		}
	}

	/* Return the result */
	return (value);
}




/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
	int		i, j;

	int		bonus;

	int		dice[18];


	/* Roll and verify some stats */
	while (TRUE)
	{
		/* Roll some dice */
		for (j = i = 0; i < 18; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals */
		if ((j > 42) && (j < 57)) break;
		/* 57 was 54... I hate 'magic numbers' :< TY */
	}

	/* Acquire the stats */
	for (i = 0; i < 6; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "class" */
                bonus = rp_ptr->r_adj[i] + rmp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Variable stat maxes */
		if (p_ptr->maximize)
		{
			/* Start fully healed */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/class bonuses */
			stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
			stat_use[i] = adjust_stat(p_ptr->stat_max[i], bonus, FALSE);

			/* Save the resulting stat maximum */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];
		}
		
		/* No temporary drain (yet...) */
		p_ptr->stat_cnt[i] = 0;
		p_ptr->stat_los[i] = 0;
	}
}


/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int		i, j, min_value, max_value;

#ifdef SHOW_LIFE_RATE
	int percent;
#endif

	/* Level one */
	p_ptr->max_plv = p_ptr->lev = 1;

	/* Experience factor */
        p_ptr->expfact = rp_ptr->r_exp + rmp_ptr->r_exp + cp_ptr->c_exp;

	/* Initialize arena and rewards information -KMW- */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = 0;
	p_ptr->inside_quest = 0;
	p_ptr->exit_bldg = TRUE; /* only used for arena now -KMW- */

	/* Reset rewards */
	for (i = 0; i < MAX_BACT; i++)
	{
		p_ptr->rewards[i] = 0;
	}

	/* Hitdice */
        p_ptr->hitdie = rp_ptr->r_mhp + rmp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Minimum hitpoints at highest level */
	min_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 3) / 8;
	min_value += PY_MAX_LEVEL;

	/* Maximum hitpoints at highest level */
	max_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 5) / 8;
	max_value += PY_MAX_LEVEL;

	/* Pre-calculate level 1 hitdice */
	player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */
	while (TRUE)
	{
		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			j = randint(p_ptr->hitdie);
			player_hp[i] = player_hp[i-1] + j;
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (player_hp[PY_MAX_LEVEL-1] < min_value) continue;
		if (player_hp[PY_MAX_LEVEL-1] > max_value) continue;

		/* Acceptable */
		break;
	}

        p_ptr->tactic = 4;
        p_ptr->movement = 4;

#ifdef SHOW_LIFE_RATE
	percent = (int)(((long)player_hp[PY_MAX_LEVEL - 1] * 200L) /
	(p_ptr->hitdie + ((PY_MAX_LEVEL - 1) * p_ptr->hitdie)));

	msg_format("Current Life Rating is %d/100.", percent);
	msg_print(NULL);
#endif /* SHOW_LIFE_RATE */
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, n, chart, roll, social_class;

	char *s, *t;

	char buf[240];

	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
                case RACE_DUNADAN:
		{
                        chart = 1;
			break;
		}
		case RACE_HUMAN:
		{
			chart = 1;
			break;
		}

		case RACE_HALF_ELF:
		{
			chart = 4;
			break;
		}

		case RACE_ELF:
                case RACE_WOOD_ELF:
		case RACE_HIGH_ELF:
		{
			chart = 7;
			break;
		}

		case RACE_HOBBIT:
		{
			chart = 10;
			break;
		}

		case RACE_GNOME:
		{
			chart = 13;
			break;
		}

		case RACE_DWARF:
		{
			chart = 16;
			break;
		}

		case RACE_HALF_ORC:
		{
			chart = 19;
			break;
		}

		case RACE_HALF_TROLL:
		{
			chart = 22;
			break;
		}

		case RACE_DARK_ELF:
		{
			chart = 69;
			break;
		}
		case RACE_HALF_OGRE:
		{
			chart = 74;
			break;
		}
		case RACE_HALF_GIANT:
		{
			chart = 75;
			break;
		}
		case RACE_KOBOLD:
		{
			chart = 82;
			break;
		}
                case RACE_RKNIGHT:
		{
			chart = 84;
			break;
		}
		case RACE_NIBELUNG:
		{
			chart = 87;
			break;
		}
                case RACE_DRAGONRIDER:
		{
			chart = 89;
			break;
		}
                case RACE_ENT:
		{
                        chart = 94;
			break;
		}
                case RACE_MOLD:
		{
                        chart = 200;
			break;
		}
                case RACE_YEEK:
                {
                        chart = 205;
                        break;
                }
		default:
		{
			chart = 0;
			break;
		}
	}


	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);


		/* Access the proper entry in the table */
		while ((chart != bg[i].chart) || (roll > bg[i].roll)) i++;

		/* Acquire the textual history */
		(void)strcat(buf, bg[i].info);

		/* Add in the social class */
		social_class += (int)(bg[i].bonus) - 50;

		/* Enter the next chart */
		chart = bg[i].next;
	}



	/* Verify social class */
	if (social_class > 100) social_class = 100;
	else if (social_class < 1) social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';


	/* Start at first line */
	i = 0;

	/* Collect the history */
	while (TRUE)
	{
		/* Extract remaining length */
		n = strlen(s);

		/* All done */
		if (n < 60)
		{
			/* Save one line of history */
			strcpy(history[i++], s);

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
		strcpy(history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */;
	}
}

/*
 * Fill the random_artifacts array with relevant info.
 */
errr init_randart(void) {
  int i;
  long foo;
  random_artifact* ra_ptr;
  char buf[80];

  for (i = 0; i < MAX_RANDARTS; i++) {
    ra_ptr = &random_artifacts[i];

    sprintf(buf, "%s", ANGBAND_DIR_FILE);
    strcpy(ra_ptr->name_short, get_line("rart_s.txt", buf, i));
    strcpy(ra_ptr->name_full, get_line("rart_f.txt", buf, i));

    ra_ptr->attr = randint(15);
    ra_ptr->activation = rand_int(MAX_T_ACT);
    ra_ptr->generated = FALSE;

    foo = randnor(0, 250);

    if (foo < 0) foo = 0;

    ra_ptr->cost = foo;
  }

  return 0;
}

/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	/* Calculate the age */
        p_ptr->age = rp_ptr->b_age + rmp_ptr->b_age + randint(rp_ptr->m_age + rmp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
                p_ptr->ht = randnor(rp_ptr->m_b_ht + rmp_ptr->m_b_ht, rp_ptr->m_m_ht + rmp_ptr->m_m_ht);
                p_ptr->wt = randnor(rp_ptr->m_b_wt + rmp_ptr->m_b_wt, rp_ptr->m_m_wt + rmp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
                p_ptr->ht = randnor(rp_ptr->f_b_ht + rmp_ptr->f_b_ht, rp_ptr->f_m_ht + rmp_ptr->f_m_ht);
                p_ptr->wt = randnor(rp_ptr->f_b_wt + rmp_ptr->f_b_wt, rp_ptr->f_m_wt + rmp_ptr->f_m_wt);
	}

        /* Calculate the height/weight for neuters */
        else if (p_ptr->psex == SEX_NEUTER)
	{
                p_ptr->ht = randnor((rp_ptr->m_b_ht + rmp_ptr->m_b_ht + rp_ptr->f_b_ht + rmp_ptr->f_b_ht) / 2, (rp_ptr->m_m_ht + rmp_ptr->m_m_ht + rp_ptr->f_m_ht + rmp_ptr->f_m_ht) / 2);
                p_ptr->wt = randnor((rp_ptr->m_b_wt + rmp_ptr->m_b_wt + rp_ptr->f_b_wt + rmp_ptr->f_b_wt) / 2, (rp_ptr->m_m_wt + rmp_ptr->m_m_wt + rp_ptr->f_m_wt + rmp_ptr->f_m_wt) / 2);
	}
if(p_ptr->ht < 1) p_ptr->ht = 1; /* Fixing out-of-bound weight/height */
if(p_ptr->wt < 1) p_ptr->wt = 1;
}




/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i, gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Process the stats */
	for (i = 0; i < 6; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 18+50) gold -= 300;
		else if (stat_use[i] >= 18+20) gold -= 200;
		else if (stat_use[i] > 18) gold -= 150;
		else gold -= (stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}



/*
 * Display stat values, subset of "put_stats()"
 *
 * See 'display_player()' for basic method.
 */
static void birth_put_stats(void)
{
	int		i, p;
	byte	attr;

	char	buf[80];


	/* Put the stats (and percents) */
	for (i = 0; i < 6; i++)
	{
		/* Put the stat */
		cnv_stat(stat_use[i], buf);
		c_put_str(TERM_L_GREEN, buf, 2 + i, 66);

		/* Put the percent */
		if (stat_match[i])
		{
			p = 1000L * stat_match[i] / auto_round;
			attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
			sprintf(buf, "%3d.%d%%", p/10, p%10);
			c_put_str(attr, buf, 2 + i, 73);
		}

		/* Never happened */
		else
		{
			c_put_str(TERM_RED, "(NONE)", 2 + i, 73);
		}
	}
}


/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
        int i, j;

        /* Wipe special levels */
        wipe_saved();
        
        /* No mana multiplier */
        p_ptr->to_m = 0;

	/* Hack -- zero the struct */
        p_ptr = WIPE(p_ptr, player_type);

	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
                for (j = 0; j < 60; j++)
                {
                        if (j < 59) history[i][j] = ' ';
                        else history[i][j] = '\0';
                }
	}

        /* No corruptions yet */
        p_ptr->muta1 = 0;
        p_ptr->muta2 = 0;
        p_ptr->muta3 = 0;

        /* Wipe the towns */
        for (i = 0; i < max_d_idx; i++)
        for (j = 0; j < MAX_DUNGEON_DEPTH; j++)
	{
                special_lvl[j][i] = 0;
	}

        /* Wipe the towns */
        for (i = max_real_towns + 1; i < max_towns; i++)
	{
                town[i].real = FALSE;
	}

	/* Wipe the quests */
        for (i = 0; i < MAX_Q_IDX; i++)
	{
		quest[i].status = QUEST_STATUS_UNTAKEN;
                for (j = 0; j < 2; j++)
                {
                        quest[i].data[j] = 0;
                }
	}

        /* Wipe the rune spells */
        rune_num = 0;
        for (i = 0; i < MAX_RUNES; i++)
	{
                strcpy(rune_spells[i].name, "");
                rune_spells[i].type = 0;
                rune_spells[i].rune2 = 0;
                rune_spells[i].mana = 0;
	}

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}

	/* Generate random artifacts */
	init_randart();

	/* Start with no artifacts made yet */
	for (i = 0; i < max_a_idx; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Reset the "objects" */
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;

                /* Reset "know" */
                k_ptr->know = FALSE;

                /* Reset "squeltch" */
                k_ptr->squeltch = 0;

                /* Reset "artifact" */
                k_ptr->artifact = 0;
	}


	/* Reset the "monsters" */
	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
                if (r_ptr->flags3 & RF3_UNIQUE_4) r_ptr->max_num = 4;

		/* Clear player kills */
		r_ptr->r_pkills = 0;

                /* Clear saved flag */
                r_ptr->on_saved = FALSE;
	}


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

        /* No current music */
        p_ptr->music = 255;

	/* Wipe the spells */
        for (i = 0; i < MAX_REALM; i++)
        {
                spell_learned[i][0] = spell_learned[i][1] = 0L;
                spell_worked[i][0] = spell_worked[i][1] = 0L;
                spell_forgotten[i][0] = spell_forgotten[i][1] = 0L;
        }
        for (i = 0; i < 64; i++)
        {
                realm_order[i] = 99;
                spell_order[i] = 99;
        }

	/* Clear "cheat" options */
	cheat_peek = FALSE;
	cheat_hear = FALSE;
	cheat_room = FALSE;
	cheat_xtra = FALSE;
	cheat_know = FALSE;
	cheat_live = FALSE;

	/* Assume no winning game */
	total_winner = FALSE;

	/* Assume no panic save */
	panic_save = 0;

	/* Assume no cheating */
	noscore = 0;

        /* Assume no innate spells */
        spell_num = 0;

        /* Clear the fate */
        for (i = 0; i < MAX_FATES; i++)
	{
                fates[i].fate = 0;
	}
        p_ptr->no_mortal = FALSE;

        /* Player don't have the black breath from the beginning !*/
        p_ptr->black_breath = FALSE;

	/* Default pet command settings */
	p_ptr->pet_follow_distance = 6;
	p_ptr->pet_open_doors = FALSE;
	p_ptr->pet_pickup_items = FALSE;

        /* Body changing initialisation */
        p_ptr->body_monster = 0;
        p_ptr->disembodied = FALSE;

        /* Wipe the bounties */
        total_bounties = 0;

        /* Wipe spells */
        p_ptr->xtra_spells = 0;

        /* Wipe xtra hp */
        p_ptr->hp_mod = 0;

        /* Wipe the monsters */
        wipe_m_list();

        /* Wipe the doppleganger */
        doppleganger = 0;

        /* Wipe the recall depths */
        for (i = 0; i < max_d_idx; i++)
        {
                max_dlv[i] = 0;
        }
        max_dlv[DUNGEON_GALGALS] = 1;

        /* Wipe the known inscription list */
        for (i = 0; i < MAX_INSCRIPTIONS; i++)
        {
                inscription_info[i].know = FALSE;
        }

        /* Wipe the known traps list */
        for (i = 0; i < max_t_idx; i++)
        {
                t_info[i].known = 0;
                t_info[i].ident = FALSE;
        }

        /* Reset wild_mode to FALSE */
        p_ptr->wild_mode = FALSE;
        p_ptr->old_wild_mode = FALSE;

	/* Initialize allow_one_death */
        p_ptr->allow_one_death = 0;

        p_ptr->loan = p_ptr->loan_time = 0;

        /* Wipe the power list */
        for (i = 0; i < POWER_SLOT; i++)
        {
                p_ptr->powers_mod[i] = 0;
        }

        /* No companions killed */
        p_ptr->companion_killed = 0;

        /* Wipe the spell levels */
        for (i = 0; i < MAX_REALM; i++)
	{
                for (j = 0; j < 64; j++)
                {
                        spell_level[i][j] = 0;
                }
	}
}




/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */

static byte player_init[MAX_CLASS][3][2] =
{
	{
		/* Warrior */
		{ TV_RING, SV_RING_RES_FEAR }, /* Warriors need it! */
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL }
	},

	{
		/* Mage */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER },
                { TV_NETHER_BOOK, 0 } /* Hack: for realm2 book */
	},

	{
		/* Priest */
                { TV_MAGERY_BOOK, 0 },
		{ TV_HAFTED, SV_MACE },
                { TV_NETHER_BOOK, 0 } /* Hack: for realm2 book */
	},

	{
		/* Rogue */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER },
                { TV_TRAPKIT, SV_TRAPKIT_SLING }
	},

	{
		/* Ranger */
                { TV_MAGERY_BOOK, 0 },           /* Hack: for realm1 book */
		{ TV_SWORD, SV_BROAD_SWORD },
                { TV_NETHER_BOOK, 0 }            /* Hack: for realm2 book */
	},

	{
		/* Paladin */
                { TV_MAGERY_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL }
	},

	{
		/* Warrior-Mage */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_SHORT_SWORD },
                { TV_NETHER_BOOK, 0 } /* Hack: for realm2 book */
	},

	{
		/* Chaos Warrior */
                { TV_MAGERY_BOOK, 0 }, /* Hack: For realm1 book */
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL }
	},

	{
		/* Monk */
                { TV_MAGERY_BOOK, 0 },
		{ TV_POTION, SV_POTION_HEALING },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
	},

	{
		/* Mindcrafter */
		{ TV_SWORD, SV_SMALL_SWORD },
		{ TV_POTION, SV_POTION_RESTORE_MANA },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
	},

	{
		/* High Mage */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER },
		{ TV_RING, SV_RING_SUSTAIN_INT}
	},

	{
                /* Mimic */
                { TV_CLOAK, 102 },
		{ TV_POTION, SV_POTION_HEALING },
		{ TV_SWORD, SV_DAGGER },
	},

	{
                /* BeastMaster */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
                { TV_RING, SV_RING_SUSTAIN_CHR },
                { TV_HAFTED, SV_WHIP },
	},

	{
                /* Alchemist */
                { TV_SWORD, SV_SMALL_SWORD },
                { TV_BATERIE, SV_BATERIE_EXPLOSION },
                { TV_BOTTLE, 1 },
	},

	{
                /* Symbiant */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
                { TV_SWORD, SV_DAGGER },
                { TV_SCROLL, SV_SCROLL_SUMMON_MINE }
	},

	{
                /* Harper */
                { TV_MAGERY_BOOK, 0 },
		{ TV_SWORD, SV_DAGGER },
                { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
	},

	{
                /* Power Mage */
                { TV_POTION, SV_POTION_DETONATIONS },
                { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
                { TV_POTION, SV_POTION_HEALING }
	},

	{
                /* Runecrafter */
                { TV_RUNE2, 1}, /* Rune [ARROW] */
                { TV_RUNE1, 5}, /* Rune [FIRE] */
                { TV_SWORD, SV_DAGGER }
	},

	{
                /* Possessor */
                { TV_POTION, SV_POTION_HEALING },
                { TV_SWORD, SV_SHORT_SWORD },
                { TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR }
	},

	{
                /* Sorcerer */
                { TV_MAGERY_BOOK, 0 }, /* Hack: for realm1 book */
                { TV_NETHER_BOOK, 0 }, /* Hack: for realm2 book */
                { TV_POTION, SV_POTION_RESTORE_MANA },
        },

	{
                /* Archer */
                { TV_BOW, SV_SHORT_BOW },
                { TV_SWORD, SV_DAGGER },
                { TV_BOW, SV_SLING }            /* Hack: for realm2 book */
	},

	{
		/* Illusionist  -KMW-  */
		{ TV_ILLUSION_BOOK, 0 },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL }
	},

	{
                /* Druid */
                { TV_MAGERY_BOOK, 0 },
                { TV_HAFTED, SV_WHIP },
                { TV_AMULET, SV_AMULET_SLOW_DIGEST }
	},

	{
                /* Necromancer */
                { TV_MAGERY_BOOK, 0 },
                { TV_POLEARM, SV_SICKLE },
                { TV_SCROLL, SV_SCROLL_DISPEL_UNDEAD },
	},

	{
                /* Unbeliever */
                { TV_RING, SV_RING_RES_FEAR },
                { TV_SWORD, SV_DARK_SWORD },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL }
	},

	{
                /* Daemonologist -SC- */
                { TV_MAGERY_BOOK, 0 }, /* Hack: For realm1 book */
                { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
                { TV_NETHER_BOOK, 0 }  /* Hack: for realm2 book */
	},

	{
		/*
		 * Weaponmaster - they also get a weapon appropriate to
		 * their specialty.  See below. -- Gumby
		 */
		{ TV_RING, SV_RING_RES_FEAR },
		{ TV_POTION, SV_POTION_HEROISM },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL }
	},

	{
		/* Merchant */
		{ TV_TOOL, SV_PORTABLE_HOLE },
		/* They get a Small Steel Chest which contains ~300 gold and
		 * 1-2 "useful" items */
                { TV_CHEST, 3 },
                { TV_SWORD, SV_LONG_SWORD },
	},
};

byte random_present[MAX_CLASS] =
        {
                /* Warrior */
                BIRTH_NONE,
                /* Mage */
                BIRTH_RING,
                /* Priest */
                BIRTH_AMULET,
                /* Rogue */
                BIRTH_NONE,
                /* Ranger */
                BIRTH_NONE,
                /* Paladin */
                BIRTH_NONE,
                /* WArrior Mage */
                BIRTH_NONE,
                /* Chaos warrior */
                BIRTH_NONE,
                /* Monk */
                BIRTH_NONE,
                /* Mindcrafter */
                BIRTH_NONE,
                /* High Mage */
                BIRTH_NONE,
                /* Mimic */
                BIRTH_NONE,
                /* Beastmaster */
                BIRTH_NONE,
                /* Alchemist */
                BIRTH_NONE,
                /* Symbiant */
                BIRTH_NONE,
                /* Harper */
                BIRTH_NONE,
                /* Powermage */
                BIRTH_NONE,
                /* Runecrafter */
                BIRTH_NONE,
                /* Possessor */
                BIRTH_NONE,
                /* Sorceror */
                BIRTH_NONE,
                /* Archer */
                BIRTH_NONE,
                /* Illusionnist */
                BIRTH_NONE,
                /* Druid */
                BIRTH_NONE,
                /* Necromancer */
                BIRTH_NONE,
                /* Unbeliever */
                BIRTH_NONE,
        };

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i, tv, sv;

	object_type	forge;
	object_type	*q_ptr;

        /*
         * Get an adventurer guide describing a bit of the
         * wilderness(useless for vanilla town)
         */
        if (!vanilla_town)
        {
                /* Get local object */
                q_ptr = &forge;

                /* Hack -- Give the player an adventurer guide */
                object_prep(q_ptr, lookup_kind(TV_PARCHEMENT, 20));
                q_ptr->number = 1;
                object_aware(q_ptr);
                object_known(q_ptr);
                (void)inven_carry(q_ptr, FALSE);
        }

	/* Get local object */
	q_ptr = &forge;

	/* Gotta give Weaponmasters a weapon they can use! -- Gumby */
	if (p_ptr->pclass == CLASS_WEAPONMASTER)
	{
                switch (p_ptr->class_extra1)
		{
			case TV_HAFTED:
				object_prep(q_ptr, lookup_kind(TV_HAFTED, SV_MACE));
				break;
			case TV_POLEARM:
				object_prep(q_ptr, lookup_kind(TV_POLEARM, SV_PIKE));
				break;
			case TV_AXE:
				object_prep(q_ptr, lookup_kind(TV_AXE, SV_LIGHT_WAR_AXE));
				break;
			case TV_SWORD:
				object_prep(q_ptr, lookup_kind(TV_SWORD, SV_BROAD_SWORD));
				break;
		}
		q_ptr->number = 1;

		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}

	/* Get local object */
	q_ptr = &forge;

        if (p_ptr->pracem == RMOD_VAMPIRE || p_ptr->pracem == RMOD_SPECTRE || p_ptr->pracem == RMOD_SKELETON || p_ptr->pracem == RMOD_ZOMBIE)
	{
		/* Hack -- Give the player scrolls of satisfy hunger */
		object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_SATISFY_HUNGER));
		q_ptr->number = (byte)rand_range(2,5);
		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}
	else
	{
		/* Hack -- Give the player some food */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
		q_ptr->number = (byte)rand_range(3, 7);
		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr, FALSE);
	}

	/* Get local object */
	q_ptr = &forge;

        if (p_ptr->pracem == RMOD_VAMPIRE)
	{
		/* Hack -- Give the player scrolls of light */
		object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_LIGHT));
		q_ptr->number = (byte)rand_range(3,7);
		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player scrolls of DARKNESS! */
		object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_DARKNESS));

		/*
		 * HACK: Give vampires the chance to reach the dungeon
		 * or the tavern without a SERIOUS sunburn (rr9)
		 */
		q_ptr->number = (byte)rand_range(10,15); /* was (2,5) */

		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}
	else
	{
		/* Hack -- Give the player some torches */
		object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
		q_ptr->number = (byte)rand_range(3, 7);
                q_ptr->timeout = rand_range(3, 7) * 500;
		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr, FALSE);
	}

	/* Get local object */
	q_ptr = &forge;

        if (p_ptr->prace == RACE_DRAGONRIDER)
	{
                /* Hack -- Give the player some small firestones */
                object_prep(q_ptr, lookup_kind(TV_FIRESTONE, SV_FIRE_SMALL));
                q_ptr->number = (byte)rand_range(8,15);
		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);

                p_ptr->tp_aux1=4;
                p_ptr->ctp=4;
	}

	/* Get local object */
	q_ptr = &forge;

        if (p_ptr->pclass == CLASS_HARPER)
	{
                /* Hack -- Give the player a Firelizard egg */
                object_prep(q_ptr, lookup_kind(TV_EGG, 1));
                q_ptr->number = 1;
                apply_magic(q_ptr, 1, TRUE, FALSE, FALSE);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}

	/* Get local object */
	q_ptr = &forge;

        if (p_ptr->pclass == CLASS_ALCHEMIST)
	{
                /* Hack -- Give the player a pair of gloves */
                object_prep(q_ptr, lookup_kind(TV_GLOVES, SV_SET_OF_LEATHER_GLOVES));
                q_ptr->number = 1;
                apply_magic(q_ptr, 1, TRUE, TRUE, FALSE);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}

	/* Get local object */
	q_ptr = &forge;

        if ((p_ptr->pclass == CLASS_MAGE) || (p_ptr->pclass == CLASS_SORCERER))
	{
                /* Hack -- Give the player a Wand of Fireball */
                object_prep(q_ptr, lookup_kind(TV_WAND, SV_WAND_FIRE_BOLT));
                q_ptr->number = 1;
                apply_magic(q_ptr, 1, TRUE, FALSE, FALSE);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}

	/* Get local object */
	q_ptr = &forge;

        if (p_ptr->pclass == CLASS_ARCHER)
	{
                /* Hack -- Give the player a some ammo */
                object_prep(q_ptr, lookup_kind(TV_SHOT, SV_AMMO_NORMAL));
                q_ptr->number = (byte)rand_range(15,20);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);

                /* Get local object */
                q_ptr = &forge;

                /* Hack -- Give the player a some ammo */
                object_prep(q_ptr, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
                q_ptr->number = (byte)rand_range(15,20);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}

        /* Rogues have a better knowledge of traps */
        if(p_ptr->pclass == CLASS_ROGUE)
        {
                t_info[TRAP_OF_DAGGER_I].known = randint(50) + 50;
                t_info[TRAP_OF_POISON_NEEDLE].known = randint(50) + 50;
                t_info[TRAP_OF_FIRE_BOLT].known = randint(50) + 50;
                t_info[TRAP_OF_DAGGER_I].ident = TRUE;
                t_info[TRAP_OF_POISON_NEEDLE].ident = TRUE;
                t_info[TRAP_OF_FIRE_BOLT].ident = TRUE;

                /* Hack -- Give the player a some ammo for the traps */
                object_prep(q_ptr, lookup_kind(TV_SHOT, SV_AMMO_NORMAL));
                q_ptr->number = (byte)rand_range(5,15);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
        }

        if (p_ptr->pclass == CLASS_MERCHANT)
	{
                /* Hack -- Give the player a Wand of Tame Monsters */
                object_prep(q_ptr, lookup_kind(TV_WAND, SV_WAND_CHARM_MONSTER));
                q_ptr->number = 1;
                apply_magic(q_ptr, 1, TRUE, FALSE, FALSE);
                object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void)inven_carry(q_ptr, FALSE);
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Hack to initialize spellbooks */
                if (tv  == TV_MAGERY_BOOK) tv = TV_VALARIN_BOOK + p_ptr->realm1 - 1;
                else if (tv == TV_NETHER_BOOK) tv = TV_VALARIN_BOOK + p_ptr->realm2 - 1;

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
                    p_ptr->pracem == RMOD_BARBARIAN)
			/* Barbarians do not need a ring of resist fear */
			sv = SV_RING_SUSTAIN_STR;

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player an object */
		object_prep(q_ptr, lookup_kind(tv, sv));

                /* Assassins begin the game with a poisoned dagger */
                if (tv == TV_SWORD && p_ptr->pclass == CLASS_ROGUE &&
                        p_ptr->realm1 == REALM_SHADOW) /* Only assassins get a poisoned weapon */
                {
                        q_ptr->name2 = EGO_BRAND_POIS;
                        apply_magic(q_ptr, 1, FALSE, FALSE, FALSE);
                }

		/* Merchants get a chest which is currently empty */
		if ((tv == TV_CHEST) && (p_ptr->pclass == CLASS_MERCHANT))
		{
			/* Put items into the chest */
			q_ptr->pval = -5;
			
			/* Set the number of items in the chest */
			q_ptr->pval2 = 6;
		}

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr, FALSE);
	}

        if(p_ptr->pclass == CLASS_POWERMAGE)
        {
                generate_spell(1);
        }

#if 0 /* hummm -- DG */
        /* Hack -- Give the player a random something */
        if (rand_birth)
	{
                tv = -1;

                switch (random_present[p_ptr->pclass])
                {
                        case BIRTH_RING:
                                tv = TV_RING;
                                do
                                {
                                        sv = rand_int(56);
                                }
                                while (!lookup_kind(tv, sv) || (sv == SV_RING_SPECIAL));
                                break;
                        case BIRTH_AMULET:
                                tv = TV_AMULET;
                                do
                                {
                                        sv = rand_int(17);
                                }
                                while (!lookup_kind(tv, sv));
                                break;
                }

                if (tv != -1)
                {
                        /* Get local object */
                        q_ptr = &forge;
                
                        /* Hack -- Give the player an object */
                        object_prep(q_ptr, lookup_kind(tv, sv));
                        apply_magic(q_ptr, 5, TRUE, FALSE, FALSE);

                        /* These objects are "storebought" */
                        q_ptr->ident |= IDENT_STOREB;

                        object_aware(q_ptr);
                        object_known(q_ptr);
                        (void)inven_carry(q_ptr, FALSE);
                }
	}
#endif
}


/* Possible number(and layout) or random quests */
int random_quests_types[MAX_RANDOM_QUESTS_TYPES] =
{
        5, 10, 1, 12, 11, 6, 14
};

static void gen_random_quests(int n)
{
        int step, lvl, i, j, k;

        step = 98000 / n;
        lvl = step / 2;

        quest[QUEST_RANDOM].status = QUEST_STATUS_TAKEN;
        for (i = 0; i < n; i++)
	{
                monster_race *r_ptr = &r_info[2];
                int rl = (lvl / 1000) + 1;
                random_quest *q_ptr = &random_quests[rl];

                q_ptr->type = random_quests_types[rand_int(MAX_RANDOM_QUESTS_TYPES)];

		for (j = 0; j < MAX_TRIES; j++)
		{
                        bool ok = TRUE;

                        /* Random monster 5 - 10 levels out of depth */
                        q_ptr->r_idx = get_mon_num(rl + 4 + randint(6));

                        if (!q_ptr->r_idx) continue;

			r_ptr = &r_info[q_ptr->r_idx];

                        /* Accept only monsters that can be generated */
                        if (r_ptr->flags9 & RF9_SPECIAL_GENE) continue;
                        if (r_ptr->flags9 & RF9_NEVER_GENE) continue;

                        /* Accept only monsters that are not breeders */
                        if (r_ptr->flags4 & RF4_MULTIPLY) continue;

                        /* Accept only monsters that are not friends */
                        if (r_ptr->flags7 & RF7_PET) continue;

                        /* Accept only monsters that are not good */
                        if (r_ptr->flags3 & RF3_GOOD) continue;

                        /* No exploding ones */
                        for (k = 0; k < 4; k++)
                        {
                                if (r_ptr->blow[k].method == RBM_EXPLODE) ok = FALSE;
                        }
                        if (!ok) continue;

                        /* No mutliple uniques */
                        if ((r_ptr->flags1 & RF1_UNIQUE) && ((q_ptr->type != 1) || (r_ptr->max_num == -1))) continue;

                        /* No single non uniques */
                        if ((!(r_ptr->flags1 & RF1_UNIQUE)) && (q_ptr->type == 1)) continue;

			/* Accept only monsters that are out of depth */
                        if (r_ptr->level > rl) break;
		}

                if (r_ptr->flags1 & RF1_UNIQUE)
                {
                        r_ptr->max_num = -1;
                }

                q_ptr->done = FALSE;

                if (wizard) message_add(format("Quest for %d on lvl %d", q_ptr->r_idx, rl), TERM_RED);

                lvl += step;
	}
}

/* Classment of classes */
s16b classes_warrior[] =
{
        CLASS_WARRIOR,
        CLASS_WEAPONMASTER,
        CLASS_UNBELIEVER,
        CLASS_MONK,
        CLASS_CHAOS_WARRIOR,
        CLASS_WARLOCK,
        CLASS_RANGER,
        CLASS_ARCHER,
        -1,
};
s16b classes_mage[] =
{
        CLASS_MAGE,
        CLASS_HIGH_MAGE,
        CLASS_POWERMAGE,
        CLASS_RUNECRAFTER,
        CLASS_SORCERER,
        CLASS_ILLUSIONIST,
        CLASS_NECRO,
        CLASS_DAEMONOLOGIST,
        -1,
};
s16b classes_priest[] =
{
        CLASS_PRIEST,
        CLASS_PALADIN,
        CLASS_MINDCRAFTER,
        CLASS_DRUID,
        -1,
};
s16b classes_monster[] =
{
        CLASS_BEASTMASTER,
        CLASS_SYMBIANT,
        CLASS_MIMIC,
        CLASS_POSSESSOR,
        -1,
};
s16b classes_other[] =
{
        CLASS_ROGUE,
        CLASS_ALCHEMIST,
        CLASS_HARPER,
        CLASS_MERCHANT,
        -1,
};

void print_desc(cptr txt)
{
        int i = -1, y = 12, x = 1;

        while (txt[++i] != 0)
        {
                if (txt[i] == '\n')
                {
                        x = 1;
                        y++;
                }
                else
                {
                        Term_putch(x++, y, TERM_YELLOW, txt[i]);
                }
        }
}

int dump_classes(s16b *classes, int sel, s32b restrict)
{
        int n = 0;
	char buf[80];
	char p2 = ')';
        cptr str;

	/* Clean up */
        clear_from(12);

        while (classes[n] != -1)
        {
                cptr mod = "";

                /* Analyze */
                p_ptr->pclass = classes[n];
                cp_ptr = &class_info[p_ptr->pclass];
                mp_ptr = &magic_info[p_ptr->pclass];
                str = cp_ptr->title;

#if 0
                /* Verify legality */
                if (!(restrict & (1L << classes[n]))) mod = " (*)";
#endif

                if (!(restrict & (1L << classes[n] )))
                        sprintf(buf, "%c%c (%s)%s", (n <= 25)?I2A(n):I2D(n-26), p2, str, mod);
                else
                        /* Display */
                        sprintf(buf, "%c%c %s%s", (n <= 25)?I2A(n):I2D(n-26), p2, str, mod);

                /* Print some more info */
                if (sel == n)
                {
                        print_desc(cp_ptr->desc);

                        c_put_str(TERM_L_BLUE, buf, 16 + (n/1), 2 + 16 * (n%1));
                }
                else
                {
                        put_str(buf, 16 + (n/1), 2 + 16 * (n%1));
                }
                n++;
	}

        return (n);
}

int dump_races(int sel)
{
        int n = 0;
	char buf[80];
	char p2 = ')';
        cptr str;

	/* Clean up */
        clear_from(12);

	for (n = 0; n < MAX_RACES; n++)
	{
		/* Analyze */
		p_ptr->prace = n;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;
		
		/* Display */
                sprintf(buf, "%c%c %s", I2A(n), p2, str);

                /* Print some more info */
                if (sel == n)
                {
                        print_desc(rp_ptr->desc);

                        c_put_str(TERM_L_BLUE, buf, 18 + (n/5), 2 + 15 * (n%5));
                }
                else
                {
                        put_str(buf, 18 + (n/5), 2 + 15 * (n%5));
                }
	}

        return (n);
}


int dump_rmods(int sel, int *racem, int max)
{
        int n = 0;
	char buf[80];
	char p2 = ')';
        cptr str;

	/* Clean up */
        clear_from(12);

                /* Dump races */
                for (n = 0; n < max; n++)
                {
                        /* Analyze */
                        p_ptr->pracem = racem[n];
                        rmp_ptr = &race_mod_info[p_ptr->pracem];
                        str = rmp_ptr->title;

                        /* Display */
                        if (n)
                                sprintf(buf, "%c%c %s", I2A(n), p2, str);
                        else
                                sprintf(buf, "%c%c Classical", I2A(n), p2);

                        /* Print some more info */
                        if (sel == n)
                        {
                                print_desc(rmp_ptr->desc);

                                c_put_str(TERM_L_BLUE, buf, 18 + ((n)/5), 2 + 15 * ((n) % 5));
                        }
                        else
                        {
                                put_str(buf, 18 + ((n)/5), 2 + 15 * ((n) % 5));
                        }
                }

        return (n);
}

int dump_gods(int sel)
{
        int n = 0;
	char buf[80];
	char p2 = ')';
        cptr str;

	/* Clean up */
        clear_from(12);

                Term_putstr(5, 17, -1, TERM_WHITE,
                        "You can choose to worship a god, some class must start with a god.");
                Term_putstr(5, 18, -1, TERM_WHITE,
                        "If a god name is in green it means your race is favored by her/him.");

        for (n = 0; n < MAX_GODS; n++)
	{
                deity *g_ptr = &deity_info[0];

                if (!n) str = "No God";
                else
                {
                        g_ptr = &deity_info[n - 1];
                        str = g_ptr->name;
                }

		/* Display */
                sprintf(buf, "%c%c %s", I2A(n), p2, str);

                /* Print some more info */
                if (sel == n)
                {
                        if (n) print_desc(g_ptr->desc);
                        else print_desc("You can begin as an atheist and still convert to a god later.");

                        c_put_str(TERM_L_BLUE, buf, 20 + (n/5), 2 + 15 * (n%5));
                }
                else
                {
                        if (n && ((g_ptr->race1 == p_ptr->prace) || (g_ptr->race2 == p_ptr->prace))) c_put_str(TERM_L_GREEN, buf, 20 + (n/5), 2 + 15 * (n%5));
                        else put_str(buf, 20 + (n/5), 2 + 15 * (n%5));
                }
	}

        return (n);
}

/* Ask questions */
static bool do_quick_start = FALSE;
static bool player_birth_aux_ask()
{
        int i, k, n, v, sel;

        int racem[100], max_racem = 0;

        u32b restrict;

	cptr str;

	char c;

	char p2 = ')';

        char buf[200];
        char inp[200];

        s16b *class_types;

	/*** Intro ***/
	if (1)
	{
		u32b seed;

		/* Basic seed */
		seed = time(NULL);
#ifdef SET_UID

		/* Mutate the seed on Unix machines */
		seed = ((seed >> 3) * (getpid() << 1));

#endif

		/* Use the complex RNG */
		Rand_quick = FALSE;

		/* Seed the "complex" RNG */
		Rand_state_init(seed);
	}
	/* So there */
	
	/* Clear screen */
	Term_clear();

	/* Title everything */
        put_str("Name  :", 2, 1);
        put_str("Sex   :", 3, 1);
        put_str("Race  :", 4, 1);
        put_str("Class :", 5, 1);

	/* Dump the default name */
        c_put_str(TERM_L_BLUE, player_name, 2, 9);


	/*** Instructions ***/

	/* Display some helpful information */
        Term_putstr(5, 8, -1, TERM_WHITE,
		"Please answer the following questions.  Most of the questions");
        Term_putstr(5, 9, -1, TERM_WHITE,
		"display a set of standard answers, and many will also accept");
        Term_putstr(5, 10, -1, TERM_WHITE,
		"some special responses, including 'Q' to quit, 'S' to restart,");
        Term_putstr(5, 11, -1, TERM_WHITE,
		"and '?' for help.  Note that 'Q' and 'S' must be capitalized.");

	/*** Random Options ***/
	
	while (1)
        {
                sprintf(buf, "Random options (y/n)?");
                put_str(buf, 20, 2);
                c = inkey();
                if (c == 'Q') quit(NULL);
                else if (c == 'S') return (FALSE);
                else if ((c == 'y') || (c == 'Y'))
                {
                        rand_opts = TRUE;
                        do_cmd_options();
			break;
                }
                else
                {
                        rand_opts = FALSE;
                        break;
                }
        }

	/*** Random Birth ***/
	
        while (1)
        {
                sprintf(buf, "Random birth (y/n)?");
                put_str(buf, 20, 2);
                c = inkey();
                if (c == 'Q') quit(NULL);
                else if (c == 'S') return (FALSE);
                else if ((c == 'y') || (c == 'Y'))
                {
                        rand_birth = TRUE;
                        break;
                }
                else
                {
                        rand_birth = FALSE;
                        break;
                }
        }

	
	/*** Quick Start ***/

        if (previous_char.quick_ok && !rand_birth)
        {
                /* Extra info */
                Term_putstr(1, 15, -1, TERM_WHITE,
                        "Do you want to use the quick start function(same character as your last one).");

                /* Choose */
                while (1)
                {
                        sprintf(buf, "Use quick start (y/n)?");
                        put_str(buf, 20, 2);
                        c = inkey();
                        if (c == 'Q') quit(NULL);
                        else if (c == 'S') return (FALSE);
                        else if ((c == 'y') || (c == 'Y'))
                        {
                                do_quick_start = TRUE;
                                break;
                        }
                        else
                        {
                                do_quick_start = FALSE;
                                break;
                        }
                }
        }

	/* Clean up */
	clear_from(15);

	/*** Player sex ***/

        if (do_quick_start)
        {
                k = previous_char.sex;
        }
        else
        {
	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Your 'sex' does not have any significant gameplay effects.");

	/* Prompt for "Sex" */
	for (n = 0; n < MAX_SEXES; n++)
	{
		/* Analyze */
		p_ptr->psex = n;
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->title;

		/* Display */
		sprintf(buf, "%c%c %s", I2A(n), p2, str);
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while (1)
	{
                sprintf(buf, "Choose a sex (%c-%c), * for random, = for options: ", I2A(0), I2A(n-1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == '*')
		{
                        k = rand_int(MAX_SEXES);
			break;
		}
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
			if (c == '?') do_cmd_help();
			else if (c == '=')
			{
				screen_save();
                                do_cmd_options_aux(6, "Startup Options", FALSE);
				screen_load();
			}
		else bell();
	}
        }

	/* Set sex */
	p_ptr->psex = k;
	sp_ptr = &sex_info[p_ptr->psex];
	str = sp_ptr->title;

	/* Display */
        c_put_str(TERM_L_BLUE, str, 3, 9);

	/* Clean up */
	clear_from(15);


	/*** Player race ***/

        if (do_quick_start)
        {
                k = previous_char.race;
        }
        else
        {
	/* Extra info */
        Term_putstr(5, 16, -1, TERM_WHITE,
		"Your 'race' determines various intrinsic factors and bonuses.");
        hack_corruption = FALSE;

	/* Dump races */
        sel = 0;
        n = dump_races(sel);

	/* Choose */
	while (1)
	{
                sprintf(buf, "Choose a race (%c-%c), * for a random choice, = for options, 8/2/4/6 for movment: ", I2A(0), I2A(MAX_RACES - 1));
                put_str(buf, 17, 2);

		if(rand_birth)
		  c = '*';
		else
		  c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == '*')
		{
                        k = rand_int(MAX_RACES);
			break;
		}
                k = (islower(c) ? A2I(c) : -1);
                if ((k >= 0) && (k < n)) break;
                if (c == '?') do_cmd_help();
                else if (c == '=')
                {
                        screen_save();
                        do_cmd_options_aux(6, "Startup Options", FALSE);
                        screen_load();
                }
                else if (c == '2')
                {
                        sel += 5;
			if(sel >=n) sel %= 5;
                        dump_races(sel);
                }
                else if (c == '8')
                {
                        sel -= 5;
			if(sel < 0) sel = n - 1 -( (-sel) % 5);
				/* C's modulus operator does not have defined
				results for negative first values. Damn. */
                        dump_races(sel);
                }
                else if (c == '6')
                {
                        sel++;
                        if (sel >= n) sel = 0;
                        dump_races(sel);
                }
                else if (c == '4')
                {
                        sel--;
                        if (sel < 0) sel = n - 1;
                        dump_races(sel);
                }
                else if (c == '\r')
                {
                        k = sel;
                        break;
                }
                else bell();
	}
        }
	/* Set race */
	p_ptr->prace = k;
	rp_ptr = &race_info[p_ptr->prace];
	str = rp_ptr->title;

	/* Display */
        c_put_str(TERM_L_BLUE, str, 4, 9);

	/* Get a random name */
        if (!do_quick_start) create_random_name(p_ptr->prace, player_name);

	/* Display */
        c_put_str(TERM_L_BLUE, player_name, 2, 9);

	/* Clean up */
        clear_from(12);


        /*** Player race mod ***/
        if (do_quick_start)
        {
                k = previous_char.rmod;
                p_ptr->pracem = k;
                rmp_ptr = &race_mod_info[p_ptr->pracem];
        }
        else
        {
        for (n = 0; n < 100; n++) racem[n] = 0;

        max_racem = 0;
        for (n = 0; n < MAX_RACE_MODS; n++)
	{
		/* Analyze */
                p_ptr->pracem = n;
                rmp_ptr = &race_mod_info[p_ptr->pracem];

                /* Must be an ok choice */
                if (!(BIT(p_ptr->prace) & rmp_ptr->choice)) continue;

                /* Ok thats a possibility */
                racem[max_racem++] = n;
	}

        /* Ah ! nothing found, lets use the default */
        if (!max_racem) p_ptr->pracem = 0;
        /* Only one ? use it */
        else if (max_racem == 1) p_ptr->pracem = racem[0];
        /* We got to ask the player */
        else
        {
                /* Extra info */
                Term_putstr(5, 15, -1, TERM_WHITE,
                        "Your 'race modifier' determines various intrinsic factors and bonuses.");

                /* Dump races */
                sel = 0;
                n = dump_rmods(sel, racem, max_racem);

                /* Choose */
                while (1)
                {
                        sprintf(buf, "Choose a race modifier (%c-%c), * for a random choice, = for options: ", I2A(0), I2A(max_racem - 1));
                        put_str(buf, 17, 2);
                        if(rand_birth)
			  c = '*';
			else
			  c = inkey();
                        if (c == 'Q') quit(NULL);
                        if (c == 'S') return (FALSE);
                        if (c == '*')
                        {
                                do
                                {
                                        k = rand_int(max_racem);
                                } while (!(BIT(racem[k]) & rmp_ptr->choice));
                                break;
                        }

                        k = (islower(c) ? A2I(c) : -1);
                        if ((k >= 0) && (k < max_racem) && (BIT(p_ptr->prace) & race_mod_info[racem[k]].choice)) break;

                        if (c == '?') do_cmd_help();
                        else if (c == '=')
                        {
                                screen_save();
                                do_cmd_options_aux(6, "Startup Options", FALSE);
                                screen_load();
                        }
                        else if (c == '2')
                        {
                                sel += 5;
                                if (sel >= n) sel = sel - n + 1;
                                dump_rmods(sel, racem, max_racem);
                        }
                        else if (c == '8')
                        {
                                sel -= 5;
                                if (sel < 0) sel = n - 1 + sel;
                                dump_rmods(sel, racem, max_racem);
                        }
                        else if (c == '6')
                        {
                                sel++;
                                if (sel >= n) sel = 0;
                                dump_rmods(sel, racem, max_racem);
                        }
                        else if (c == '4')
                        {
                                sel--;
                                if (sel < 0) sel = n - 1;
                                dump_rmods(sel, racem, max_racem);
                        }
                        else if (c == '\r')
                        {
                                k = sel;
                                break;
                        }
                        else bell();
                }

                /* Set race */
                p_ptr->pracem = racem[k];
        }
        rmp_ptr = &race_mod_info[p_ptr->pracem];
        sprintf(buf, "%s %s", rp_ptr->title, rmp_ptr->title);

	/* Display */
        c_put_str(TERM_L_BLUE, buf, 4, 9);
        }

        /* I know, gotos are EVIL, sorry */
repeat_player_class:

	/* Clean up */
        clear_from(12);


	/*** Player class ***/
        if (do_quick_start)
        {
                k = previous_char.class;
                p_ptr->pclass = k;
                cp_ptr = &class_info[p_ptr->pclass];
                mp_ptr = &magic_info[p_ptr->pclass];
        }
        else
        {
        restrict = rp_ptr->choice + rmp_ptr->pclass - rmp_ptr->mclass;

	/* Extra info */
        Term_putstr(5, 13, -1, TERM_WHITE,
		"Your 'class' determines various intrinsic abilities and bonuses.");

        /* Get a class type */
        c_put_str(TERM_L_UMBER, "a) Fighters     -- Hack and slash your way in Middle Earth", 16, 2);
        c_put_str(TERM_L_BLUE,  "b) Spellcasters -- Magic is the true way", 17, 2);
        c_put_str(TERM_YELLOW,  "c) Priests      -- Hail the powers of the Ainurs", 18, 2);
        c_put_str(TERM_L_GREEN, "d) Beastfriends -- Monsters are fun", 19, 2);
        c_put_str(TERM_VIOLET,  "e) Others       -- The way to your independance", 20, 2);
	while (1)
	{
                sprintf(buf, "Choose a class type (a-e), * for random, = for options: ");
                put_str(buf, 15, 2);
		if(rand_birth)
		  c = '*';
		else
		  c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == '*')
		{
                        k = rand_int(5);
			break;
		}
                k = (islower(c) ? A2I(c) : (D2I(c) + 26));
                if ((k >= 0) && (k < 5)) break;
                if (c == '?') do_cmd_help();
                else if (c == '=')
                {
                        screen_save();
                        do_cmd_options_aux(6, "Startup Options", FALSE);
                        screen_load();
                }
		else bell();
	}
        switch (k)
        {
                case 0: class_types = classes_warrior; break;
                case 1: class_types = classes_mage; break;
                case 2: class_types = classes_priest; break;
                case 3: class_types = classes_monster; break;
                case 4: class_types = classes_other; break;
                default: class_types = classes_warrior; break;
        }
	clear_from(15);

	/* Dump classes */
        sel = 0;
        n = dump_classes(class_types, sel, restrict);

	/* Get a class */
	while (1)
	{
                sprintf(buf, "Choose a class (%c-%c), * for random, = for options, 8/2/4 for up/down/back: ", I2A(0), (n <= 25)?I2A(n-1):I2D(n-26-1));
                put_str(buf, 15, 2);
		if(rand_birth)
		  c = '*';
		else
		  c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == '*')
		{
                        k = randint(n) - 1;
			break;
		}
                k = (islower(c) ? A2I(c) : (D2I(c) + 26));
		if ((k >= 0) && (k < n)) break;
                if (c == '?') do_cmd_help();
                else if (c == '=')
                {
                        screen_save();
                        do_cmd_options_aux(6, "Startup Options", FALSE);
                        screen_load();
                }
                else if (c == '2')
                {
                        sel++;
                        if (sel >= n) sel = 0;
                        dump_classes(class_types, sel, restrict);
                }
                else if (c == '8')
                {
                        sel--;
                        if (sel < 0) sel = n - 1;
                        dump_classes(class_types, sel, restrict);
                }
                else if (c == '4')
                {
                        /* UGLY I know ... */
                        goto repeat_player_class;
                }
                else if (c == '\r')
                {
                        k = sel;
                        break;
                }
		else bell();
	}

	/* Set class */
#ifdef FORBID_BAD_COMBINAISON
        if (!(restrict & (1L << k )))
        {
                noscore |= 0x0020;
                message_add(" ", TERM_VIOLET);
                message_add(" ", TERM_VIOLET);
                message_add(" ", TERM_VIOLET);
                message_add("***************************", TERM_VIOLET);
                message_add("***************************", TERM_VIOLET);
                message_add("********* Cheater *********", TERM_VIOLET);
                message_add("***************************", TERM_VIOLET);
                message_add("***************************", TERM_VIOLET);
        }
#endif
        p_ptr->pclass = class_types[k];
        }
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &magic_info[p_ptr->pclass];
	str = cp_ptr->title;

	/* Display */
        c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 9);

	/* Clean up */
	clear_from(15);

        /*** Player god ***/
        if (do_quick_start)
        {
                k = previous_char.god;
                p_ptr->pgod = k;
                set_grace(previous_char.grace);
                p_ptr->god_favor = previous_char.god_favor;
        }
        else
        {
        if (p_ptr->pclass == CLASS_DRUID)
        {
                p_ptr->pgod = GOD_YAVANNA;
        }
        else
        {
                sel = 0;
                n = dump_gods(sel);

                /* Choose */
                while (1)
                {
            	        sprintf(buf, "Choose a god (%c-%c), * for a random choice, = for options, 8/2/4/6 for movment: ", I2A(0), I2A(MAX_GODS));
                        put_str(buf, 19, 2);

			if(rand_birth)
			  c = '*';
			else
                          c = inkey();
                        if (c == 'Q') quit(NULL);
                        if (c == 'S') return (FALSE);
                        if (c == '*')
                        {
                                k = randint(MAX_GODS);
                                break;
                        }
                        k = (islower(c) ? A2I(c) : -1);
                        if ((k >= 0) && (k < n)) break;
                        if (c == '?') do_cmd_help();
                        else if (c == '=')
                        {
                                screen_save();
                                do_cmd_options_aux(6, "Startup Options", FALSE);
                                screen_load();
                        }
                        else if (c == '2')
                        {
                                sel += 5;
                                if(sel >=n) sel %= 5;
                                dump_gods(sel);
                        }
                        else if (c == '8')
                        {
                                sel -= 5;
                                if(sel < 0) sel = n - 1 -( (-sel) % 5);
                                        /* C's modulus operator does not have defined
                                        results for negative first values. Damn. */
                                dump_gods(sel);
                        }
                        else if (c == '6')
                        {
                                sel++;
                                if (sel >= n) sel = 0;
                                dump_gods(sel);
                        }
                        else if (c == '4')
                        {
                                sel--;
                                if (sel < 0) sel = n - 1;
                                dump_gods(sel);
                        }
                        else if (c == '\r')
                        {
                                k = sel;
                                break;
                        }
                        else bell();
                }

                /* Set god */
                p_ptr->pgod = k;
                p_ptr->grace = 0;
                p_ptr->god_favor = 0;
        }
	
	if ((!p_ptr->pgod) && ((p_ptr->pclass == CLASS_PRIEST) || (p_ptr->pclass == CLASS_PALADIN)))
        {
                p_ptr->pgod = randint(MAX_GODS - 1);
        }

        /* a god that like us ? more grace ! */
        if ((deity_info[p_ptr->pgod - 1].race1 == p_ptr->prace) || (deity_info[p_ptr->pgod - 1].race2 == p_ptr->prace))
        {
                set_grace(50000);
                p_ptr->god_favor = -60000;
        }
        else
        {
                if ((p_ptr->pclass == CLASS_PRIEST) || (p_ptr->pclass == CLASS_PALADIN) || (p_ptr->pclass == CLASS_DRUID))
                {
                        set_grace(40000);
                        p_ptr->god_favor = -40000;
                }
                else
                {
                        set_grace(8000);
                        p_ptr->god_favor = -20000;
                }
        }
        }

	/* Clean up */
        clear_from(12);

        if (do_quick_start)
        {
                p_ptr->realm1 = previous_char.realm1;
                p_ptr->realm2 = previous_char.realm2;
        }
        else
        {
                /* get the realms */
                get_realms();

                if (p_ptr->realm1 || p_ptr->realm2)
                        put_str("Magic :", 6, 1);
                if (p_ptr->realm1)
                        c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm1], 6, 9);
                if (p_ptr->realm2)
                        c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm2], 7, 9);


                /* Clear */
                clear_from(15);

                /*  */
                if (get_check("Do you want to modify the options"))
                {
                        screen_save();
                        do_cmd_options_aux(6, "Startup Options", FALSE);
                        screen_load();
                }
        }

        /* Set birth options: maximize, preserve, sepcial levels and astral */
	p_ptr->maximize = maximize;
	p_ptr->preserve = preserve;
	p_ptr->special = special_lvls;
	p_ptr->astral = astral_option;
	 /*
	  * A note by pelpel. (remove this please)
	  * Be it the new Vanilla way (adult vs. birth options) or
	  * the old one (player_type members), it would be less confusing
	  * to handle birth-only options in a uniform fashion,the above and
	  * the following:
	  * permanent_levels,
	  * ironman_rooms,
	  * cth_monsters,
	  * zang_monsters,
	  * joke_monsters,
	  * always_small_level, and
	  * fate_option
	  */


        /* Set dungeon seed */
        if (permanent_levels)
        {
                seed_dungeon = randint(0x10000000);
        }else{
                seed_dungeon = 0;
        }

        /* Set the recall dungeon accordingly */
        if (p_ptr->astral)
        {
                dungeon_type = DUNGEON_MANDOS;
                p_ptr->recall_dungeon = DUNGEON_GALGALS;

                /* Somewhere in the misty mountains */
                p_ptr->wilderness_x = 45;
                p_ptr->wilderness_y = 19;
        }
        else
        {
                dungeon_type = DUNGEON_GALGALS;
                p_ptr->recall_dungeon = DUNGEON_GALGALS;
        }

	/* Clean up */
	clear_from(10);

	/*** User enters number of quests ***/
	/* Heino Vander Sanden and Jimmy De Laet */

        if (!ironman_rooms)
        {
        if (do_quick_start)
        {
                v = previous_char.quests;
        }
        else
        {
        /* Extra info */
        Term_putstr(5, 15, -1, TERM_WHITE, "Please input the number of optional quests you'd like to perform");
        Term_putstr(5, 16, -1, TERM_WHITE, "If you do not want any optional quests, enter 0");

	/* Ask the number of additional quests */
	while (TRUE)
	{
                put_str(format("Number of additional quest? (<%u) ", MAX_RANDOM_QUEST), 20, 2);

		/* Get a the number of additional quest */
		while (TRUE)
		{
			/* Move the cursor */
			put_str("", 20, 37);

			/* Default */
			strcpy(inp, "20");

			/* Get a response (or escape) */
			if(rand_birth)
		          v = randint(98);
			else
			  {
			  if (!askfor_aux(inp, 2)) inp[0] = '\0';
                          if (inp[0] == '*') v = rand_int(MAX_RANDOM_QUEST);
                          else v = atoi(inp);
			  }
			/* Break on valid input */
                        if ((v < MAX_RANDOM_QUEST) && ( v >= 0 )) break;
		}
		break;
	}

	/* Clear */
	clear_from(15);
        }

	/* Set the quest monster hook */
	get_mon_num_hook = monster_quest;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Generate quests */
        for (i = 0; i < MAX_RANDOM_QUEST; i++) random_quests[i].type = 0;
        if (v) gen_random_quests(v);
        max_quests = v;
        }

	p_ptr->inside_quest = 0;

        /* Init the plots */
        plots[PLOT_MAIN] = QUEST_NECRO;
        quest[plots[PLOT_MAIN]].status = QUEST_STATUS_TAKEN;

        plots[PLOT_BREE] = QUEST_THIEVES;
        quest[plots[PLOT_BREE]].status = QUEST_STATUS_UNTAKEN;

        plots[PLOT_LORIEN] = QUEST_SPIDER;
        quest[plots[PLOT_LORIEN]].status = QUEST_STATUS_UNTAKEN;

        plots[PLOT_GONDOLIN] = QUEST_EOL;
        quest[plots[PLOT_GONDOLIN]].status = QUEST_STATUS_UNTAKEN;

        plots[PLOT_MINAS] = QUEST_BETWEEN;
        quest[plots[PLOT_MINAS]].status = QUEST_STATUS_UNTAKEN;

        plots[PLOT_OTHER] = QUEST_NULL;

        quest_random_init_hook(QUEST_RANDOM);

        /* Ok */
        return (TRUE);
}

/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[(18-10)+1] = { 0, 1, 2, 4, 7, 11, 16, 22, 30 };


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 10 to 18 (inclusive),
 * each costing a certain amount of points (as above), from a pool of 48
 * available points, to which race/class modifiers are then applied.
 *
 * Each unused point is converted into 100 gold pieces, with a maximum of
 * 600 gp at birth.
 *
 * Taken from V 2.9.0
 */
static bool player_birth_aux_point(void)
{
	int i;

	int row = 3;
	int col = 42;

	int stat = 0;

        int stats[6];

	int cost;

	char ch;

	char buf[80];

        int mode = 0;


	/* Initialize stats */
        for (i = 0; i < 6; i++)
	{
		/* Initial stats */
		stats[i] = 10;
	}


	/* Roll for base hitpoints */
	get_extra();

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();
	
	/* Hack -- get a chaos patron even if you are not a chaos warrior */
	p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;

        /* Choose specialty for Weaponmasters -- Gumby
         * Considered putting probabilities in here, but I think
         * I'll keep it simple and trust people not to reroll the
         * character just because they didn't get Swords.
         */
        if (p_ptr->pclass == CLASS_WEAPONMASTER)
        {
                p_ptr->class_extra1 = rand_range(TV_HAFTED, TV_AXE);
        }

	p_ptr->muta1 = 0;
	p_ptr->muta2 = 0;
	p_ptr->muta3 = 0;

	/* Interact */
	while (1)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
                for (i = 0; i < 6; i++)
		{
			/* Variable stat maxes */
                        if (p_ptr->maximize)
			{
				/* Reset stats */
				p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stats[i];

			}

			/* Fixed stat maxes */
			else
			{
				/* Obtain a "bonus" for "race" and "class" */
				int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

				/* Apply the racial/class bonuses */
				p_ptr->stat_cur[i] = p_ptr->stat_max[i] =
					modify_stat_value(stats[i], bonus);
			}

			/* Total cost */
			cost += birth_stat_costs[stats[i] - 10];
		}

		/* Restrict cost */
		if (cost > 48)
		{
			/* Warning */
			bell();

			/* Reduce stat */
			stats[stat]--;

			/* Recompute costs */
			continue;
		}

		/* Gold is inversely proportional to cost */
		p_ptr->au = (100 * (48 - cost)) + 100;

		/* Maximum of 600 gold */
		if (p_ptr->au > 600) p_ptr->au = 600;

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP);

		/* Update stuff */
		update_stuff();

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp;

		/* Display the player */
		display_player(mode);

		/* Display the costs header */
		put_str("Cost", row - 2, col + 32);

		/* Display the costs */
                for (i = 0; i < 6; i++)
		{
			/* Display cost */
			sprintf(buf, "%4d", birth_stat_costs[stats[i] - 10]);
			put_str(buf, row + (i-1), col + 32);
		}


		/* Prompt XXX XXX XXX */
		sprintf(buf, "Total Cost %2d/48.  Use 2/8 to move, 4/6 to modify, ESC to accept.", cost);
		prt(buf, 0, 0);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(col + 36, row + stat - 1);

		/* Get key */
		ch = inkey();

		/* Quit */
		if (ch == 'Q') quit(NULL);

		/* Start over */
		if (ch == 'S') return (FALSE);

		/* Done */
		if (ch == ESCAPE) break;

		/* Prev stat */
		if (ch == '8')
		{
                        stat = (stat + 6 - 1) % 6;
		}

		/* Next stat */
		if (ch == '2')
		{
                        stat = (stat + 1) % 6;
		}

		/* Decrease stat */
		if ((ch == '4') && (stats[stat] > 10))
		{
			stats[stat]--;
		}

		/* Increase stat */
		if ((ch == '6') && (stats[stat] < 18))
		{
			stats[stat]++;
		}
	}


	/* Done */
	return (TRUE);
}

/*
 * Use the autoroller or not to generate a char
 */
static bool player_birth_aux_auto()
{
        int i, j, m, v;

	int mode = 0;

	bool flag = FALSE;
	bool prev = FALSE;

	char c;

	char b1 = '[';
	char b2 = ']';

	char buf[80];
	char inp[80];

#ifdef ALLOW_AUTOROLLER

	/* Initialize */
	if (autoroll)
	{
		int mval[6];


		/* Clear fields */
		auto_round = 0L;
		last_round = 0L;

		/* Clean up */
		clear_from(10);

		/* Prompt for the minimum stats */
		put_str("Enter minimum attribute for: ", 15, 2);

		/* Output the maximum stats */
		for (i = 0; i < 6; i++)
		{
			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Race/Class bonus */
                        j = rp_ptr->r_adj[i] + rmp_ptr->r_adj[i] + cp_ptr->c_adj[i];

			/* Obtain the "maximal" stat */
			m = adjust_stat(17, j, TRUE);

			/* Save the maximum */
			mval[i] = m;

			/* Extract a textual format */
			/* cnv_stat(m, inp); */

			/* Above 18 */
			if (m > 18)
			{
				sprintf(inp, "(Max of 18/%02d):", (m - 18));
			}

			/* From 3 to 18 */
			else
			{
				sprintf(inp, "(Max of %2d):", m);
			}

			/* Prepare a prompt */
                        sprintf(buf, "%-5s: %-20s", stat_names[i], inp);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		/* Input the minimum stats */
		for (i = 0; i < 6; i++)
		{
			/* Get a minimum stat */
			while (TRUE)
			{
				char *s;

				/* Move the cursor */
				put_str("", 16 + i, 30);

				/* Default */
				strcpy(inp, "");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 8)) inp[0] = '\0';

				/* Hack -- add a fake slash */
				strcat(inp, "/");

				/* Hack -- look for the "slash" */
				s = strchr(inp, '/');

				/* Hack -- Nuke the slash */
				*s++ = '\0';

				/* Hack -- Extract an input */
				v = atoi(inp) + atoi(s);

				/* Break on valid input */
				if (v <= mval[i]) break;
			}

			/* Save the minimum stat */
			stat_limit[i] = (v > 0) ? v : 0;
		}
	}

#endif /* ALLOW_AUTOROLLER */

	/* Roll */
	while (TRUE)
	{
		/* Feedback */
		if (autoroll)
		{
			Term_clear();

                        put_str("Name :", 2, 1);
                        put_str("Sex  :", 3, 1);
                        put_str("Race :", 4, 1);
                        put_str("Class:", 5, 1);

                        c_put_str(TERM_L_BLUE, player_name, 2, 9);
                        c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 9);
                        if (p_ptr->pracem)
                                sprintf(buf, "%s %s", rp_ptr->title, rmp_ptr->title);
                        else
                                sprintf(buf, "%s", rp_ptr->title);
                        c_put_str(TERM_L_BLUE, buf, 4, 9);
                        c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 9);

			/* Label stats */
			put_str("STR:", 2 + A_STR, 61);
			put_str("INT:", 2 + A_INT, 61);
			put_str("WIS:", 2 + A_WIS, 61);
			put_str("DEX:", 2 + A_DEX, 61);
			put_str("CON:", 2 + A_CON, 61);
			put_str("CHR:", 2 + A_CHR, 61);

			/* Note when we started */
			last_round = auto_round;

			/* Indicate the state */
			put_str("(Hit ESC to abort)", 11, 61);

			/* Label count */
			put_str("Round:", 9, 61);
		}

		/* Otherwise just get a character */
		else
		{
			/* Get a new character */
			get_stats();
		}

		/* Auto-roll */
		while (autoroll)
		{
			bool accept = TRUE;

			/* Get a new character */
			get_stats();

			/* Advance the round */
			auto_round++;

			/* Hack -- Prevent overflow */
			if (auto_round >= 1000000L) break;

			/* Check and count acceptable stats */
			for (i = 0; i < 6; i++)
			{
				/* This stat is okay */
				if (stat_use[i] >= stat_limit[i])
				{
					stat_match[i]++;
				}

				/* This stat is not okay */
				else
				{
					accept = FALSE;
				}
			}

			/* Break if "happy" */
			if (accept) break;

			/* Take note every few rolls */
			flag = (!(auto_round % AUTOROLLER_STEP));

			/* Update display occasionally */
			if (flag || (auto_round < last_round + 100))
			{
				/* Dump data */
				birth_put_stats();

				/* Dump round */
				put_str(format("%6ld", auto_round), 9, 73);

				/* Make sure they see everything */
				Term_fresh();
#ifndef USE_FAST_AUTOROLLER
				/* Delay 1/10 second */
                                if ((fast_autoroller) && (flag)) Term_xtra(TERM_XTRA_DELAY, 100);
#endif
				/* Do not wait for a key */
				inkey_scan = TRUE;

				/* Check for a keypress */
				if (inkey()) break;
			}
		}

		/* Flush input */
		flush();


		/*** Display ***/

		/* Mode */
		mode = 0;

		/* Roll for base hitpoints */
		get_extra();

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		get_money();

		/* Choose specialty for Weaponmasters -- Gumby
		 * Considered putting probabilities in here, but I think
		 * I'll keep it simple and trust people not to reroll the
		 * character just because they didn't get Swords.
		 */
		if (p_ptr->pclass == CLASS_WEAPONMASTER)
		{
                        p_ptr->class_extra1 = rand_range(TV_HAFTED, TV_AXE);
		}

                /* Hack -- get a chaos patron even if you are not a chaos warrior */
                p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;

                p_ptr->muta1 = 0;
                p_ptr->muta2 = 0;
                p_ptr->muta3 = 0;

		/* Input loop */
		while (TRUE)
		{
			/* Calculate the bonuses and hitpoints */
                        p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_BODY);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Display the player */
			display_player(mode);

			/* Prepare a prompt (must squeeze everything in) */
			Term_gotoxy(2, 23);
			Term_addch(TERM_WHITE, b1);
			Term_addstr(-1, TERM_WHITE, "'r' to reroll");
			if (prev) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
			if (mode) Term_addstr(-1, TERM_WHITE, ", 'h' for Misc.");
			else Term_addstr(-1, TERM_WHITE, ", 'h' for History");
			Term_addstr(-1, TERM_WHITE, ", or ESC to accept");
			Term_addch(TERM_WHITE, b2);

			/* Prompt and get a command */
			c = inkey();

			/* Quit */
			if (c == 'Q') quit(NULL);

			/* Start over */
                        if (c == 'S') return (FALSE);

			/* Escape accepts the roll */
			if (c == ESCAPE) break;

			/* Reroll this character */
			if ((c == ' ') || (c == 'r')) break;

			/* Previous character */
			if (prev && (c == 'p'))
			{
                                load_prev_data(TRUE);
				continue;
			}

			/* Toggle the display */
			if ((c == 'H') || (c == 'h'))
			{
				mode = ((mode != 0) ? 0 : 1);
				continue;
			}

			/* Help */
			if (c == '?')
			{
				do_cmd_help();
				continue;
			}

			/* Warning */
			bell();
		}

		/* Are we done? */
		if (c == ESCAPE) break;

		/* Save this for the "previous" character */
		save_prev_data();

		/* Note that a previous roll exists */
		prev = TRUE;
	}

	/* Clear prompt */
	clear_from(23);

        return (TRUE);
}

/*
 * Helper function for 'player_birth()'
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.  And it cuts down on player stupidity.
 */
static bool player_birth_aux()
{
	char c;
        int y = 0, x = 0;
        char old_history[4][60];
        int i, j;

        /* Ask */
        if (!player_birth_aux_ask()) return (FALSE);

	/*** Generate ***/

        if (do_quick_start)
        {
                load_prev_data(FALSE);

                /* Roll for base hitpoints */
                get_extra();

                /* Calculate the bonuses and hitpoints */
                p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_BODY);

                /* Update stuff */
                update_stuff();

                /* Fully healed */
                p_ptr->chp = p_ptr->mhp;

                /* Fully rested */
                p_ptr->csp = p_ptr->msp;
        }
        else
        {
	/* Point based */
	if (point_based)
	{
                if (!player_birth_aux_point()) return FALSE;
	}
	/* Auto-roll */
	else
	{
                if (!player_birth_aux_auto()) return FALSE;
	}

        /* Edit character background */
        for (i = 0; i < 4; i++)
        {
                sprintf(old_history[i], "%s", history[i]);
        }
        /* Turn 0 to space */
	for (i = 0; i < 4; i++)
	{
                for (j = 0; j < 59; j++)
                {
                        if (history[i][j] == '\0') history[i][j] = ' ';
                }
	}
        display_player(1);
        c_put_str(TERM_L_GREEN, "(Character Background - Edit Mode)", 15, 20);
        while (TRUE)
        {
                for (i = 0; i < 4; i++)
                {
                        put_str(history[i], i + 16, 10);
                }
                c_put_str(TERM_L_BLUE, format("%c", history[y][x]), y + 16, x + 10);

		/* Place cursor just after cost of current stat */
                Term_gotoxy(x + 10, y + 16);

                c = inkey();

                if (c == '8')
                {
                        y--;
                        if (y < 0) y = 3;
                }
                else if (c == '2')
                {
                        y++;
                        if (y > 3) y = 0;
                }
                else if (c == '6')
                {
                        x++;
                        if (x > 59) x = 0;
                }
                else if (c == '4')
                {
                        x--;
                        if (x < 0) x = 59;
                }
                else if (c == '\r')
                {
                        break;
                }
                else if (c == ESCAPE)
                {
                        for (i = 0; i < 4; i++)
                        {
                                sprintf(history[i], "%s", old_history[i]);
                                put_str(history[i], i + 16, 10);
                        }
                        break;
                }
                else
                {
                        history[y][x++] = c;
                        if (x > 58)
                        {
                                x = 0;
                                y++;
                                if (y > 3) y = 0;
                        }
                }
        }


	/*** Finish up ***/

	/* Get a name, recolor it, prepare savefile */

	get_name();


	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

	/* Get a key */
	c = inkey();

	/* Quit */
	if (c == 'Q') quit(NULL);

	/* Start over */
	if (c == 'S') return (FALSE);
        }

        /* Save this for the next character */
        previous_char.quick_ok = TRUE;
        save_prev_data();

	/* Accept */
	return (TRUE);
}

/*
 * Helper function for validate_bg().
 */
static void validate_bg_aux(int chart, bool chart_checked[], char *buf)
{
        char *s;

        int i;

        /* Assume the chart does not exist */
        bool chart_exists = FALSE;

        /* Assume the chart is not complete */
        bool chart_complete = FALSE;

        int bg_max = sizeof(bg) / sizeof(hist_type);

        /* No chart */
        if (!chart) return;

        /* Already saw this chart */
        if (chart_checked[chart]) return;

        /* Build a debug message */
        s = buf + strlen(buf);
        (void) sprintf(s, "%d --> ", chart);

        /* Check each chart */
        for (i = 0; i < bg_max; i++)
        {
                /* Require same chart */
                if (bg[i].chart != chart) continue;

                /* The chart exists */
                chart_exists = TRUE;
                
                /* Validate the "next" chart recursively */
                validate_bg_aux(bg[i].next, chart_checked, buf);
                
                /* Require a terminator */
                if (bg[i].roll != 100) continue;
                
                /* The chart is complete */
                chart_complete = TRUE;
        }

        /* Failed: The chart does not exist */
        if (!chart_exists)
        {
                quit_fmt("birth.c: bg[] chart %d does not exist\n%s", chart, buf);
        }

        /* Failed: The chart is not complete */
        if (!chart_complete)
        {
                quit_fmt("birth.c: bg[] chart %d is not complete", chart);
        }

        /* Remember we saw this chart */
        chart_checked[chart] = TRUE;

        /* Build a debug message */
        *s = 0;
}

/*
 * Verify that the bg[] table is valid.
 */
static void validate_bg(void)
{
        int i, race;

        int race_chart[MAX_RACES];

        bool chart_checked[512];

        char buf[1024];
        
        for (i = 0; i < 512; i++) chart_checked[i] = FALSE;

        race_chart[RACE_DUNADAN] = 1;
        race_chart[RACE_HUMAN] = 1;
        race_chart[RACE_HALF_ELF] = 4;
        race_chart[RACE_ELF] = 7;
        race_chart[RACE_HIGH_ELF] = 7;
        race_chart[RACE_HOBBIT] = 10;
        race_chart[RACE_GNOME] = 13;
        race_chart[RACE_DWARF] = 16;
        race_chart[RACE_HALF_ORC] = 19;
        race_chart[RACE_HALF_TROLL] = 22;
        race_chart[RACE_DARK_ELF] = 69;
        race_chart[RACE_HALF_OGRE] = 74;
        race_chart[RACE_HALF_GIANT] = 75;
        race_chart[RACE_KOBOLD] = 82;
        race_chart[RACE_RKNIGHT] = 84;
        race_chart[RACE_NIBELUNG] = 87;
        race_chart[RACE_DRAGONRIDER] = 89;
        race_chart[RACE_ENT] = 94;
        race_chart[RACE_MOLD] = 200;
        race_chart[RACE_YEEK] = 205;
        race_chart[RACE_WOOD_ELF] = 7;

        /* Check each race */
        for (race = 0; race < MAX_RACES; race++)
        {
                /* Get the first chart for this race */
                int chart = race_chart[race];

                (void) strcpy(buf, "");
                
                /* Validate the chart recursively */
                validate_bg_aux(chart, chart_checked, buf);
        }
}

/*
 * Initialize a random town
 */
void init_town(int t_idx, int level)
{
        town_type *t_ptr = &town[t_idx];

        t_ptr->real = TRUE;
        t_ptr->seed = randint(0x10000000);
        t_ptr->numstores = 8; /* Total hack and not even used */
}

/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
        int i, j, n, rtown = TOWN_RANDOM;

        /* Validate the bg[] table */
        validate_bg();

	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
	}

        /* Make a note file if that option is set */
        if (take_notes)
        {
		add_note_type(NOTE_BIRTH);
        }

	/* Note player birth in the message recall */
        message_add(" ", TERM_L_BLUE);
        message_add("  ", TERM_L_BLUE);
        message_add("====================", TERM_L_BLUE);
        message_add("  ", TERM_L_BLUE);
        message_add(" ", TERM_L_BLUE);


	/* Hack -- outfit the player */
	player_outfit();

        /* Initialize random towns in the dungeons */
        for (i = 0; i < max_d_idx; i++)
        {
                dungeon_info_type *d_ptr = &d_info[i];
                int num = 0, z;

                d_ptr->t_num = 0;
                for (z = 0; z < 4; z++)
                {
                        d_ptr->t_idx[z] = 0;
                        d_ptr->t_level[z] = 0;
                }
                if (!(d_ptr->flags1 & DF1_RANDOM_TOWNS)) continue;

                /* Can we add a town ? */
                while (magik(TOWN_CHANCE - (num * 10)))
                {
                        int lev;

                        d_ptr->t_idx[num] = rtown;
                        rtown++;

                        while (TRUE)
                        {
                                int j;
                                bool ok = TRUE;

                                lev = rand_range(d_ptr->mindepth, d_ptr->maxdepth - 1);

                                /* Be sure it wasnt already used */
                                for (j = 0; j < num; j++)
                                {
                                        if (d_ptr->t_level[j] == lev) ok = FALSE;
                                }

                                /* Ok found one */
                                if (ok) break;
                        }
                        d_ptr->t_level[num] = lev;

                        if (wizard) message_add(format("Random dungeon town: d_idx:%d, lev:%d", i, lev), TERM_WHITE);

                        /* Create the town */
                        init_town(d_ptr->t_idx[num], d_ptr->t_level[num]);

                        num++;
                }

                d_ptr->t_num = num;
        }

        /* Init the towns */
	for (i = 1; i < max_towns; i++)
	{
                if (!town[i].real) continue;

                create_stores_stock(i);

                /* Init the stores */
                for (j = 0; j < max_st_idx; j++)
		{
			/* Initialize */
			store_init(i, j);
		}
	}

	/* Init wilderness seeds */
        for (i = 0; i < max_wild_x; i++)
	{
                for (j = 0; j < max_wild_y; j++)
		{
                        wild_map[j][i].seed = rand_int(0x10000000);
                        wild_map[j][i].entrance = 0;
                        wild_map[j][i].known = FALSE;
		}
	}

	/* Select bounty monsters. */
	select_bounties();
}

char savefile_names[46][30];
char savefile_desc[46][80];
bool savefile_alive[46];

/* Grab all the names from an index */
int load_savefile_names()
{
	FILE *fff;
        char buf[1024];
        char tmp[50];
        int max = 0, fd;

	/* Build the filename */
#ifdef SAVEFILE_USE_UID
       sprintf(tmp,"user.%d.svg", player_uid);
#else
       sprintf(tmp, "global.svg");
#endif /* SAVEFILE_USE_UID */
        path_build(buf, 1024, ANGBAND_DIR_SAVE, tmp);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	safe_setuid_grab();
        /* Read the file */
        fff = my_fopen(buf, "r");
	safe_setuid_drop();

	/* Failure */
        if (!fff) return (0);

        /* Parse, use '@' intead of ':' as a separator because it cannot exists in savefiles */
        while (0 == my_fgets(fff, buf, 1024))
	{
                int i = 1;

                if (buf[0] == '0') savefile_alive[max] = FALSE;
                else if (buf[0] == '1') savefile_alive[max] = TRUE;

                while (buf[i] != '@')
                {
                        savefile_names[max][i - 1] = buf[i];
                        i++;
                }
                savefile_names[max][i - 1] = '\0';
                i++;
                sprintf(savefile_desc[max], buf + i);
#ifdef SAVEFILE_USE_UID
		sprintf(tmp,"%d.%s", player_uid, savefile_names[max]);
#else
		sprintf(tmp, "%s", savefile_names[max]);
#endif /* SAVEFILE_USE_UID */
                /* Still existing ? */
                path_build(buf, 1024, ANGBAND_DIR_SAVE, tmp);
		safe_setuid_grab();
                FILE_TYPE(FILE_TYPE_SAVE);
                fd = fd_open(buf, O_RDONLY);
		safe_setuid_drop();
                if (fd >= 0)
                {
                        fd_close(fd);

                        max++;
                }
        }

        my_fclose(fff);

        return (max);
}

/* Save all the names from an index */
void save_savefile_names()
{
	FILE *fff;
        char buf[1024];
        char tmp[50];
        int max = load_savefile_names(), i;

	/* Build the filename */
#ifdef SAVEFILE_USE_UID
       sprintf(tmp,"user.%d.svg", player_uid);
#else
       sprintf(tmp, "global.svg");
#endif /* SAVEFILE_USE_UID */
        path_build(buf, 1024, ANGBAND_DIR_SAVE, tmp);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

        /* Read the file */
	safe_setuid_grab();
        fff = my_fopen(buf, "w");
	safe_setuid_drop();

	/* Failure */
        if (!fff) return;

        /* Save, use '@' intead of ':' as a separator because it cannot exists in savefiles */
        fprintf(fff, "%c%s@%s, the %s%s%s %s is %s\n", (death)?'0':'1', player_base, player_name, rp_ptr->title, (p_ptr->pracem)?" ":"", rmp_ptr->title, cp_ptr->title, (!death)?"alive":"dead");
        for (i = 0; i < max; i++)
        {
                if (!strcmp(savefile_names[i], player_base)) continue;
                fprintf(fff, "%c%s@%s\n", (savefile_alive[i])?'1':'0', savefile_names[i], savefile_desc[i]);
        }

        my_fclose(fff);
}

static void dump_savefiles(int sel, int max)
{
        int i;
        char buf[40], pre = ' ', post = ')';
        char ind;

        for (i = 0; i < max; i++)
        {
                ind = I2A(i % 26);
                if (i >= 26) ind = toupper(ind);

                if (sel == i)
                {
                        pre = '[';
                        post = ']';
                }
                else
                {
                        pre = ' ';
                        post = ')';
                }

                if (i == 0) sprintf(buf, "%c%c%c New Character", pre, ind, post);
                else if (i == 1) sprintf(buf, "%c%c%c Load Savefile", pre, ind, post);
                else sprintf(buf, "%c%c%c %s", pre, ind, post, savefile_names[i - 2]);
                
                if (sel == i)
                {
                        if (i >= 2)
                        {
                                if (savefile_alive[i - 2]) c_put_str(TERM_L_GREEN, savefile_desc[i - 2], 5, 0);
                                else c_put_str(TERM_L_RED, savefile_desc[i - 2], 5, 0);
                        }
                        else if (i == 1) c_put_str(TERM_YELLOW, "Load an existing savefile that is not in the list", 5, 0);
                        else c_put_str(TERM_YELLOW, "Create a new character", 5, 0);
                        c_put_str(TERM_L_BLUE, buf, 6 + (i / 4), 20 * (i % 4));
                }
                else
                        put_str(buf, 6 + (i / 4), 20 * (i % 4));
        }
}

/* Asks for new game or load game */
bool no_begin_screen = FALSE;
bool begin_screen()
{
        int k, sel, max;

        sel = 0;

        /* Hack */
        use_color = TRUE;

        /* Grab the savefiles */
        max = load_savefile_names() + 2;
        if (max > 2) sel = 2;

        while (TRUE)
        {
                /* Clear screen */
                Term_clear();

                /* Let the user choose */
                c_put_str(TERM_YELLOW, "Welcome to PernAngband, to play you will need a character.", 1, 10);
                put_str("Press 8/2/4/6 to move, Return to select, Backspace to delete a savefile.", 3, 3);
                put_str("and Esc to quit.", 4, 32);

                dump_savefiles(sel, max);

                k = inkey();

                if (k == '\e')
                {
                        quit(NULL);
                }
                if (k == '6')
                {
                        sel++;
                        if (sel >= max) sel = 0;
                        continue;
                }
                else if (k == '4')
                {
                        sel--;
                        if (sel < 0) sel = max - 1;
                        continue;
                }
                else if (k == '2')
                {
                        sel += 4;
                        if (sel >= max) sel = sel % max;
                        continue;
                }
                else if (k == '8')
                {
                        sel -= 4;
                        if (sel < 0) sel = (sel + max - 1) % max;
                        continue;
                }
                else if (k == '\r')
                {
                        if (sel < 26) k = I2A(sel);
                        else k = toupper(I2A(sel));
                }
                else if (((k == 0x7F) || (k == '\010')) && (sel >= 2))
                {
                        char buf[1024];

                        if (!get_check(format("Really delete '%s'?", savefile_names[sel - 2]))) continue;

                        path_build(buf, 1024, ANGBAND_DIR_SAVE, savefile_names[sel - 2]);
                        fd_kill(buf);

                        /* Grab the savefiles */
                        max = load_savefile_names() + 2;
                        if (max > 2) sel = 2;
                        else sel = 0;

                        continue;
                }

                if (k == 'a')
                {
                        /* Display prompt */
                        prt("Enter the name of the savefile that will hold this character: ", 23, 0);
	
                        /* Ask the user for a string */
                        if (!askfor_aux(player_base, 15)) continue;

                        /* Process the player name */
                        process_player_name(TRUE);

                        return (TRUE);
                }
                if (k == 'b')
                {
                        /* Display prompt */
                        prt("Enter the name of a savefile: ", 23, 0);
	
                        /* Ask the user for a string */
                        if (!askfor_aux(player_base, 15)) continue;

                        /* Process the player name */
                        process_player_name(TRUE);

                        return (FALSE);
                }
                else
                {
                        int x;

                        if (islower(k)) x = A2I(k);
                        else x = A2I(tolower(k)) + 26;

                        if ((x < 2) || (x >= max)) continue;

                        sprintf(player_base, "%s", savefile_names[x - 2]);

                        /* Process the player name */
                        process_player_name(TRUE);

                        return (FALSE);
                }
        }

        /* Shouldnt happen */
        return (FALSE);
}
