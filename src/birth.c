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
#define AUTOROLLER_STEP 25L

/*
 * Forward declare
 */
typedef struct birther birther;

/*
 * A structure to hold "rolled" information
 */
struct birther
{
	s16b age;
	s16b wt;
	s16b ht;
	s16b sc;

	s32b au;

	s16b stat[6];

	char history[4][60];
};



/*
 * The last character displayed
 */
static birther prev;



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
	{"of a Yeoman.  ",					 65, 2, 3, 80},
	{"of a Townsman.  ",					 80, 2, 3, 90},
	{"of a Guildsman.  ",					 90, 2, 3, 105},
	{"of a Landed Knight.  ",				 96, 2, 3, 120},
	{"of a Noble Family in the Courts of Chaos.  ",          99, 2, 3, 130},
	{"of the Royal Blood Line of Amber.  ",                 100, 2, 3, 140},

	{"You are the black sheep of the family.  ",             20, 3, 50, 20},
	{"You are a credit to the family.  ",                    80, 3, 50, 55},
	{"You are a well liked child.  ",                       100, 3, 50, 60},

	{"Your mother was of the Teleri.  ",			 40, 4, 1, 50},
	{"Your father was of the Teleri.  ",			 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ",		 	 90, 4, 1, 55},
	{"Your father was of the Noldor.  ",		 	 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ",			 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ",			100, 4, 1, 70},

	{"You are one of several children ",			 60, 7, 8, 50},
	{"You are the only child ",					100, 7, 8, 55},

	{"of a Telerin ",						 75, 8, 9, 50},
	{"of a Noldo ",						 95, 8, 9, 55},
	{"of a Vanya ",						100, 8, 9, 60},

	{"Ranger.  ",						 40, 9, 54, 80},
	{"Archer.  ",						 70, 9, 54, 90},
	{"Warrior.  ",						 87, 9, 54, 110},
	{"Mage.  ",							 95, 9, 54, 125},
	{"Prince.  ",						 99, 9, 54, 140},
	{"King.  ",							100, 9, 54, 145},

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

	{"You are an unacknowledged child of ", 50, 67, 68, 45},
	{"You are a rebel child of ",         80, 67, 68, 65},
	{"You are a long lost child of ",     100, 67, 68, 55},

	{"an unknown Amberite.  ",               50, 68, 50, 80 },
	{"an unknown third generation Amberite.  ", 65, 68, 50, 90 },
	{"an unknown second generation Amberite.  ", 79, 68, 50, 100 },
	{"Oberon.  ",       80, 68, 50, 130 },
	{"Osric.  ",        83, 68, 50, 105 },
	{"Finndo.  ",       84, 68, 50, 105 },
	{"Brand.  ",        85, 68, 50, 90 },
	{"Flora.  ",        87, 68, 50, 100 },
	{"Gerard.  ",       88, 68, 50, 125 },
	{"Deirdre.  ",      89, 68, 50, 120 },
	{"Random.  ",       90, 68, 50, 140 },
	{"Benedict.  ",     91, 68, 50, 115 },
	{"Corwin.  ",       92, 68, 50, 110 },
	{"Julian.  ",       93, 68, 50, 105 },
	{"Caine.  ",        94, 68, 50, 95 },
	{"Bleys.  ",        95, 68, 50, 115 },
	{"Fiona.  ",        96, 68, 50, 110 },
	{"Eric.  ",         97, 68, 50, 135 },
	{"Rinaldo.  ",      98, 68, 50, 90 },
	{"Merlin.  ",       99, 68, 50, 105 },
	{"Martin.  ",       100, 68, 50, 80 },


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

	{"a Kamikaze Yeek", 20, 79, 135, 40},
	{"a Brown Yeek. ", 50, 79, 80, 50 },
	{"a Blue Yeek.  ", 75, 79, 80, 50 },
	{"a Master Yeek.  ", 95, 79, 80, 85 },
	{"Boldor, the King of the Yeeks.  ", 100, 79, 80, 120 },

	{", whom you never knew.  ", 70, 135, 80, 40},
	{".  ", 100, 135, 80, 50},

	{"You have pale eyes, ",    25, 80, 81, 50 },
	{"You have glowing eyes, ",    50, 80, 81, 50 },
	{"You have tiny black eyes, ",    75, 80, 81, 50 },
	{"You have beady black eyes, ", 90, 80, 81, 50},
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

	{"You are one of several children of a Klackon hive queen.  "
	, 100, 84, 85, 50 },

	{"You have red skin ", 40, 85, 86, 50 },
	{"You have black skin ", 90, 85, 86, 50 },
	{"You have yellow skin ", 100, 85, 86, 50 },

	{"and black eyes.", 100, 86, 0, 50 },

	{"You are one of several children of ", 100, 87, 88, 89 },

	{"a Nibelung Slave.  ", 30, 88, 18, 20 },
	{"a Nibelung Thief.  ", 50, 88, 18, 40 },
	{"a Nibelung Smith.  ", 70, 88, 18, 60 },
	{"a Nibelung Miner.  ", 90, 88, 18, 75 },
	{"a Nibelung Shaman.  ", 95, 88, 18, 100 },
	{"Mime, the Nibelung.  ", 100, 88, 18, 100 },

	{"You are one of several children of a Draconian ", 85, 89, 90, 50  },
	{"You are the only child of a Draconian ", 100, 89, 90, 55 },

	{"Warrior.  ", 50, 90, 91, 50 },
	{"Priest.  ", 65, 90, 91, 65 },
	{"Mage.  ", 85, 90, 91, 70 },
	{"Noble.  ", 100, 90, 91, 100 },

	{"You have green wings, green skin and yellow belly.", 30, 91, 0, 50 },
	{"You have green wings, and green skin.", 55, 91, 0, 50 },
	{"You have red wings, and red skin.", 80, 91, 0, 50 },
	{"You have black wings, and black skin.", 90, 91, 0, 50 },
	{"You have metallic skin, and shining wings.", 100, 91, 0, 50},

	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80 },
	{"three tentacles around your mouth.", 20, 93, 0, 45 },
	{"five tentacles around your mouth.", 80, 93, 0, 50 },
	{"eight tentacles around your mouth.", 100, 93, 0, 55 },

	{"You ancestor was ", 100, 94, 95, 50 },

	{"a mindless demonic spawn.  ", 30, 95, 96, 20 },
	{"a minor demon.  ", 60, 95, 96, 50 },
	{"a major demon.  ", 90, 95, 96, 75 },
	{"a demon lord.  ", 100, 95, 96, 99 },

	{"You have brown skin, ", 100, 96, 97, 50},

	{"claws, fangs, spikes, and glowing red eyes.", 40, 97, 0, 50 },
	{"claws, fangs, and glowing red eyes.", 70, 97, 0, 50 },
	{"claws, and glowing red eyes.", 100, 97, 0, 50 },

	{"You were shaped from ", 100, 98, 99, 50 },

	{"a corpse ", 40, 99, 100, 40 },
	{"clay ", 80, 99, 100, 50 },
	{"stone ", 85, 99, 100, 60 },
	{"iron ", 99, 99, 100, 70 },
	{"mithril ", 100, 99, 100, 100},

	{"by a Kabbalist", 40, 100, 101, 50 },
	{"by a wizard", 65, 100, 101, 50 },
	{"by an alchemist", 90, 100, 101, 50},
	{"by a priest", 100, 100, 101, 60},

	{" to fight evil.", 10, 101, 0, 65 },
	{".", 100, 101, 0, 50 },

	{"You were created by ", 100, 102, 103, 50 },

	{"a legion of druj.  ", 10, 103, 104, 30 },
	{"a necromancer.  ", 30, 103, 104, 50 },
	{"a magical experiment.  ", 50, 103, 104, 50 },
	{"an evil priest.  ", 70, 103, 104, 50 },
	{"a pact with the demons.  ", 75, 103, 104, 50 },
	{"a restless spirit.  ", 85, 103, 104, 50 },
	{"a curse.  ", 95, 103, 104, 30 },
	{"an oath.  ", 99, 103, 104, 50 },
	{"Morgoth.  ", 100, 103, 104, 20 },

	{"You have ", 100, 104, 105, 50 },
	{"dirty, dry bones, ", 40, 105, 106, 50 },
	{"rotten, black bones, ", 60, 105, 106, 50 },
	{"filthy, brown bones, ", 80, 105, 106, 50 },
	{"shining white bones, ", 100, 105, 106, 50 },

	{"and glowing eyes.", 30, 106, 0, 50 },
	{"and eyes that burn with hellfire.", 50, 106, 0, 60 },
	{"and empty eyesockets.", 100, 106, 0, 50 },

	{"You arose from an unmarked grave.  ", 20, 107, 137, 20 },
	{"You arose from the grave of a ", 30, 107, 108, 50 },
	{"You emerged from a desecrated tomb.  ", 50, 107, 137, 40 },
	{"You are the result of a failed magical experiment.  ", 75, 107, 137, 50 },
	{"A mindflayer ate your brain.  ", 95, 107, 137, 50 },
	{"You came from the tomb of a ", 98, 107, 136, 50 },
	{"You were animated by druj.  ", 100, 107, 137, 30 },

	{"Peasant.  ", 50, 108, 137, 30 },
	{"Yeoman.  ", 65, 108, 137, 40 },
	{"Townsman.  ", 90, 108, 137, 45 },
	{"Guildsman.  ", 100, 108, 137, 55 },

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

	{"You arose from an unmarked grave.  ", 20, 113, 114, 30 },
	{"In life you were a simple peasant.  ", 40, 109, 110, 40 },
	{"In life you were a vampire hunter, but they got you.  ", 60, 113, 114, 50 },
	{"In life you were a necromancer.  ", 80, 113, 114, 60 },
	{"In life you were a powerful noble.  ", 95, 113, 114, 75 },
	{"In life you were a powerful and cruel tyrant.  ", 100, 113, 114, 90 },

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

	{"a legion of druj.  ", 10, 119, 134, 30 },
	{"a necromancer.  ", 30, 119, 134, 50 },
	{"a magical experiment.  ", 50, 119, 134, 50 },
	{"an evil priest.  ", 70, 119, 134, 50 },
	{"a pact with the demons.  ", 75, 119, 134, 50 },
	{"a restless spirit.  ", 85, 119, 134, 50 },
	{"a curse.  ", 95, 119, 134, 30 },
	{"an oath.  ", 99, 119, 134, 50 },
	{"Morgoth.  ", 100, 119, 134, 20 },

	{"jet-black hair, ", 25, 120, 121, 50 },
	{"matted brown hair, ", 50, 120, 121, 50 },
	{"white hair, ", 75, 120, 121, 50 },
	{"a hairless head, ", 100, 120, 121, 50 },

	{"eyes like red coals, ", 25, 121, 122, 50 },
	{"blank white eyes, ", 50, 121, 122, 50 },
	{"feral yellow eyes, ", 75, 121, 122, 50 },
	{"bloodshot red eyes, ", 100, 121, 122, 50 },

	{" and a deathly gray complexion.  ", 100, 122, 123, 50 },

	{"An eerie green aura surrounds you.", 40, 123, 0, 55 },
	{"", 100, 123, 0, 50 },

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
	{"In your childhood, you were stupid enough to stick your head in raw Logrus.  ",
	50, 129, 130, 30 },
	{"A Demon Lord of Chaos decided to have some fun, and so created you.  ",
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

	{"and the hooves of a goat.",      50, 133, 0, 40 },
	{"and human feet.",        75, 133, 0, 50 },
	{"and bird's feet.",       85, 133, 0, 45 },
	{"and reptilian feet.",    90, 133, 0, 30 },
	{"and bovine feet.",       95, 133, 0, 45 },
	{"and feline feet.",       97, 133, 0, 55 },
	{"and canine feet.",       100, 133, 0, 55 },

	{"You have ", 100, 134, 120, 50 },

	{"Knight.  ", 60, 136, 137, 40 },
	{"Noble.  ", 90, 136, 137, 50 },
	{"Royal Family.  ", 100, 136, 137, 60 },

	{"You have ", 100, 137, 138, 50 },

	{"no hair at all, ", 70, 138, 140, 40 },
	{"dirty ", 80, 138, 139, 50 },
	{"matted ", 95, 138, 139, 50 },
	{"oily ", 100, 138, 139, 50 },

	{"black hair, ", 25, 139, 140, 50 },
	{"brown hair, ", 50, 139, 140, 50 },
	{"red hair, ", 75, 139, 140, 50 },
	{"blond hair, ", 95, 139, 140, 50 },
	{"white hair, ", 100, 139, 140, 50 },

	{"milky blind eyes, ", 40, 140, 141, 50 },
	{"red bloodshot eyes, ", 60, 140, 141, 45 },
	{"eyes which ooze pus, ", 75, 140, 141, 20 },
	{"eyes black with decay, ", 90, 140, 141, 30 },
	{"only one eye, ", 97, 140, 141, 35 },
	{"no eyes at all, ", 100, 140, 141, 40 },

	{"and a vacant expression.  ", 60, 141, 142, 60 },
	{"and a gaping mouth.  ", 90, 141, 142, 45 },
	{"and a hint of intelligence.  ", 100, 141, 142, 70 },

	{"Your flesh hangs from your bones.  ", 30, 142, 143, 40 },
	{"Bits of flesh fall from your body as you walk.  ", 50, 142, 143, 30 },
	{"Your body oozes pus.  ", 65, 142, 143, 20 },
	{"You reek with decay.  ", 70, 142, 143, 20 },
	{"Your flesh crawls with maggots.  ", 80, 142, 143, 30 },
	{"Many of your bones are broken.  ", 95, 142, 143, 35 },
	{"Your skin has the pallor of death.  ", 100, 142, 143, 50 },

	{"You are mute.", 40, 143, 0, 50 },
	{"You can grunt.", 60, 143, 0, 60 },
	{"You understand a few words.", 90, 143, 0, 65 },
	{"You are able to grunt broken phrases.", 100, 143, 0, 70 },

	{"You were spawned by a major demon.  ", 70, 144, 145, 10 },
	{"A demonologist created you.  ", 85, 144, 147, 20 },
	{"You arose from a pool of primal chaos.  ", 95, 144, 147, 5 },
	{"A curse called you into being.  ", 100, 144, 147, 30 },

	{"None of your siblings survived their birth.  ", 40, 145, 147, 40 },
	{"You were the strongest of your litter.  ", 70, 145, 147, 60 },
	{"You opportunistically destroyed your siblings.  ", 77, 145, 147, 50 },
	{"You aided the strongest of your litter at your birth.  ", 92, 145, 147, 55 },
	{"You aided your litter's runt, but survived.  ", 95, 145, 147, 70 },
	{"You were the runt of the litter, ", 100, 145, 146, 60 },

	{"but managed to escape.  ", 99, 146, 147, 50 },
	{"but emerged victorious!  ", 100, 146, 147, 75 },

	{"You have brown skin, ", 70, 147, 148, 50 },
	{"You have pink skin, ", 90, 147, 148, 50 },
	{"You have jet black skin, ", 100, 147, 148, 50 },

	{"a light build, ", 10, 148, 149, 40 },
	{"a medium build, ", 40, 148, 149, 45 },
	{"a heavy build, ", 100, 148, 149, 50 },

	{"two ", 10, 149, 152, 50 },
	{"three ", 50, 149, 152, 50 },
	{"four ", 70, 149, 152, 50 },
	{"five ", 80, 149, 152, 50 },
	{"six ", 95, 149, 152, 50 },
	{"seven ", 100, 149, 152, 50 },

	{"claws on each limb, ", 100, 152, 153, 50 },

	{"glowing red eyes, ", 60, 153, 154, 45 },
	{"beady black eyes, ", 70, 153, 154, 50 },
	{"milky white eyes, ", 90, 153, 154, 47 },
	{"a chilling gaze, ", 93, 153, 154, 35 },
	{"no eyes at all, ", 100, 153, 154, 40 },

	{"and a foul, toothy grin.", 30, 154, 0, 40 },
	{"and an unusually large array of teeth.", 50, 154, 0, 45 },
	{"and sharp, ragged fangs.", 75, 154, 0, 35 },
	{"and a terrifying smile.", 82, 154, 0, 30 },
	{"and a deathly visage.", 90, 154, 0, 20 },
	{"and a sulphurous breath.", 98, 154, 0, 30 },
	{"and breath that reeks of decay.", 100, 154, 0, 15 },
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
 * Social class limit
 */
static s16b sc_limit;

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

byte choose_realm(byte choices)
{
	int picks[MAX_REALM];
	int k, n;

	char c;

	char p2 = ')';

	char buf[80];

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"The realm of magic will determine which spells you can learn.");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"Life and Sorcery are protective, Chaos and Death are destructive.");
	Term_putstr(5, 17, -1, TERM_WHITE,
		"Nature has both defensive and offensive spells.");

	n = 0;

	if ((choices & CH_LIFE) && (p_ptr->realm1 != REALM_LIFE))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Life");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_LIFE;
	}

	if ((choices & CH_SORCERY) && (p_ptr->realm1 != REALM_SORCERY))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Sorcery");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_SORCERY;
	}

	if ((choices & CH_ARCANE) && (p_ptr->realm1 != REALM_ARCANE))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Arcane");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_ARCANE;
	}

	if ((choices & CH_TRUMP) && (p_ptr->realm1 != REALM_TRUMP))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Trump");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_TRUMP;
	}

	if ((choices & CH_NATURE) && (p_ptr->realm1 != REALM_NATURE))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Nature");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_NATURE;
	}

	if ((choices & CH_CHAOS) && (p_ptr->realm1 != REALM_CHAOS))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Chaos");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_CHAOS;
	}

	if ((choices & CH_DEATH) && (p_ptr->realm1 != REALM_DEATH))
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Death");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n++] = REALM_DEATH;
	}

	/* Get a class */
	while (1)
	{
		sprintf(buf, "Choose a realm (%c-%c): ", I2A(0), I2A(n-1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}


	/* Clean up */
	clear_from(15);

	return (picks[k]);
}

static void get_realms(void)
{
	/* Start with no realms */
	p_ptr->realm1 = p_ptr->realm2 = 0;

	/* Warriors and certain others get no realms */
	if (realm_choices[p_ptr->pclass] == (CH_NONE)) return;

	/* Other characters get at least one realm */
	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR_MAGE:
		{
			p_ptr->realm1 = REALM_ARCANE;
			break;
		}
		case CLASS_CHAOS_WARRIOR:
		{
			p_ptr->realm1 = REALM_CHAOS;
			break;
		}
		case CLASS_PRIEST:
		{
			p_ptr->realm1 = choose_realm(CH_LIFE | CH_DEATH);
			break;
		}
		case CLASS_RANGER:
		{
			p_ptr->realm1 = REALM_NATURE;
			break;
		}
		default:
		{
			p_ptr->realm1 = choose_realm(realm_choices[p_ptr->pclass]);
		}
	}

	/* Some classes get two realms */
	if (realm_choices[p_ptr->pclass] & CH_TWO)
	{
		p_ptr->realm2 = choose_realm(realm_choices[p_ptr->pclass]);
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
	prev.age = p_ptr->age;
	prev.wt = p_ptr->wt;
	prev.ht = p_ptr->ht;
	prev.sc = p_ptr->sc;
	prev.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
		prev.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev.history[i], history[i]);
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(void)
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

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(temp.history[i], history[i]);
	}


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = prev.age;
	p_ptr->wt = prev.wt;
	p_ptr->ht = prev.ht;
	p_ptr->sc = prev.sc;
	p_ptr->au = prev.au;

	/* Load the stats */
	for (i = 0; i < 6; i++)
	{
		p_ptr->stat_max[i] = prev.stat[i];
		p_ptr->stat_cur[i] = prev.stat[i];
	}

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], prev.history[i]);
	}


	/*** Save the current data ***/

	/* Save the data */
	prev.age = temp.age;
	prev.wt = temp.wt;
	prev.ht = temp.ht;
	prev.sc = temp.sc;
	prev.au = temp.au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
		prev.stat[i] = temp.stat[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev.history[i], temp.history[i]);
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
static int adjust_stat(int value, s16b amount, int auto_roll)
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
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

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
	p_ptr->max_level = p_ptr->level = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

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
		case RACE_AMBERITE:
		{
			chart = 67;
			break;
		}
		case RACE_HUMAN:
		case RACE_BARBARIAN:
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
		case RACE_HALF_TITAN:
		{
			chart = 76;
			break;
		}
		case RACE_CYCLOPS:
		{
			chart = 77;
			break;
		}
		case RACE_YEEK:
		{
			chart = 78;
			break;
		}
		case RACE_KOBOLD:
		{
			chart = 82;
			break;
		}
		case RACE_KLACKON:
		{
			chart = 84;
			break;
		}
		case RACE_NIBELUNG:
		{
			chart = 87;
			break;
		}
		case RACE_DRACONIAN:
		{
			chart = 89;
			break;
		}
		case RACE_MIND_FLAYER:
		{
			chart = 92;
			break;
		}
		case RACE_IMP:
		{
			chart = 94;
			break;
		}
		case RACE_GOLEM:
		{
			chart = 98;
			break;
		}
		case RACE_SKELETON:
		{
			chart = 102;
			break;
		}
		case RACE_ZOMBIE:
		{
			chart = 107;
			break;
		}
		case RACE_VAMPIRE:
		{
			chart = 113;
			break;
		}
		case RACE_SPECTRE:
		{
			chart = 118;
			break;
		}
		case RACE_SPRITE:
		{
			chart = 124;
			break;
		}
		case RACE_BEASTMAN:
		{
			chart = 129;
			break;
		}
		case RACE_FIEND:
		{
			chart = 144;
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
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
	}
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

	/* She charmed the banker into it! -CJS- */
	/* She slept with the banker.. :) -GDH-  */
	/* if (p_ptr->psex == SEX_FEMALE) gold += 50; */

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
	int i;


	/* Hack -- zero the struct */
	WIPE(p_ptr, player_type);

	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], "");
	}


	/* No weight */
	total_weight = 0;

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artifacts made yet */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Initialize quests (Heino Vander Sanden and Jimmy De Laet) */
	initialise_quests();

	/* Reset the "objects" */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}


	/* Reset the "monsters" */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- no ghosts */
	r_info[MAX_R_IDX-1].max_num = 0;


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* Wipe the spells */
	spell_learned1 = spell_learned2 = 0L;
	spell_worked1 = spell_worked2 = 0L;
	spell_forgotten1 = spell_forgotten2 = 0L;
	for (i = 0; i < 64; i++) spell_order[i] = 99;


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
	p_ptr->noscore = 0;

	/* Use a default pet follow distance */
	p_ptr->pet_follow_distance = 6;

	/* Use some default pet options */
	p_ptr->pet_pickup_items = TRUE;
}




/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
typedef struct init_equip_type init_equip_type;
typedef struct conv_equip_type conv_equip_type;

struct init_equip_type
{
	byte tval;
	byte sval;
	byte dice;
	byte side;

	byte tag;
};

struct conv_equip_type
{
	byte tval1;
	byte sval1;
	byte tval2;
	byte sval2;

	byte tag;
};

#define TV_BOOK_REALM1		254
#define TV_BOOK_REALM2		255

#define SV_ANY					255

static init_equip_type equip_class_init[] =
{
	/* Warrior */
	{ TV_RING, 				SV_RING_RES_FEAR,						1, 1, CLASS_WARRIOR },
	{ TV_HARD_ARMOR,		SV_CHAIN_MAIL,							1, 1, CLASS_WARRIOR },
	{ TV_GLOVES,			SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_WARRIOR },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_WARRIOR },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_WARRIOR },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_WARRIOR },
	{ TV_SWORD, 			SV_BROAD_SWORD,						1, 1, CLASS_WARRIOR },

	/* Mage */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_MAGE },
	{ TV_BOOK_REALM2,		0,											1, 1, CLASS_MAGE },
	{ TV_SOFT_ARMOR,		SV_ROBE,									1, 1, CLASS_MAGE },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_MAGE },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_MAGE },
	{ TV_SWORD, 			SV_DAGGER,								1, 1, CLASS_MAGE },

	/* Priest */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_PRIEST },
	{ TV_BOOK_REALM2,		0,											1, 1, CLASS_PRIEST },
	{ TV_SOFT_ARMOR,		SV_ROBE,									1, 1, CLASS_PRIEST },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_PRIEST },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_PRIEST },
	{ TV_HAFTED,			SV_MACE,									1, 1, CLASS_PRIEST },

	/* Rogue */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_ROGUE },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_ROGUE },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_ROGUE },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_ROGUE },
	{ TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_ROGUE },
	{ TV_SWORD,				SV_DAGGER,								1, 1, CLASS_ROGUE },

	/* Ranger */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_RANGER },
	{ TV_BOOK_REALM2,		0,											1, 1, CLASS_RANGER },
	{ TV_CLOAK,				SV_ELVEN_CLOAK,						1, 1, CLASS_RANGER },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_RANGER },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_RANGER },
	{ TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_RANGER },
	{ TV_SWORD, 			SV_LONG_SWORD,							1, 1, CLASS_RANGER },
	{ TV_BOW,				SV_SHORT_BOW,							1, 1, CLASS_RANGER },
	{ TV_ARROW,				SV_AMMO_NORMAL,						6, 7, CLASS_RANGER },

	/* Paladin */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_PALADIN },
	{ TV_SCROLL, 			SV_SCROLL_PROTECTION_FROM_EVIL,	1, 1, CLASS_PALADIN },
	{ TV_HARD_ARMOR,		SV_CHAIN_MAIL,							1, 1, CLASS_PALADIN },
	{ TV_GLOVES,			SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_PALADIN },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_PALADIN },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_PALADIN },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_PALADIN },
	{ TV_HAFTED, 			SV_WAR_HAMMER,							1, 1, CLASS_PALADIN },

	/* Warrior-Mage */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_WARRIOR_MAGE },
	{ TV_BOOK_REALM2,		0,											1, 1, CLASS_WARRIOR_MAGE },
	{ TV_SOFT_ARMOR,		SV_LEATHER_SCALE_MAIL,				1, 1, CLASS_WARRIOR_MAGE },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_WARRIOR_MAGE },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_WARRIOR_MAGE },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_WARRIOR_MAGE },
	{ TV_SWORD, 			SV_SHORT_SWORD, 						1, 1, CLASS_WARRIOR_MAGE },

	/* Chaos Warrior */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_CHAOS_WARRIOR },
	{ TV_HARD_ARMOR,		SV_METAL_SCALE_MAIL,					1, 1, CLASS_CHAOS_WARRIOR },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_CHAOS_WARRIOR },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_CHAOS_WARRIOR },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_CHAOS_WARRIOR },
	{ TV_SWORD, 			SV_BROAD_SWORD,						1, 1, CLASS_CHAOS_WARRIOR },

	/* Monk */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_MONK },
	{ TV_POTION,			SV_POTION_HEALING, 					1, 1, CLASS_MONK },
	{ TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_MONK },
	{ TV_GLOVES,			SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_MONK },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_MONK },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_MONK },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_MONK },
	{ TV_HAFTED,			SV_QUARTERSTAFF,						1, 1, CLASS_MONK },

	/* Mindcrafter */
	{ TV_POTION, 			SV_POTION_RESTORE_MANA,				1, 1, CLASS_MINDCRAFTER },
	{ TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_MINDCRAFTER },
	{ TV_GLOVES,			SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_MINDCRAFTER },
	{ TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_MINDCRAFTER },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_MINDCRAFTER },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_MINDCRAFTER },
	{ TV_SWORD, 			SV_SMALL_SWORD,						1, 1, CLASS_MINDCRAFTER },

	/* High Mage */
	{ TV_BOOK_REALM1,		0,											1, 1, CLASS_HIGH_MAGE },
	{ TV_WAND,				SV_WAND_STINKING_CLOUD,				1, 1, CLASS_HIGH_MAGE },
	{ TV_RING,				SV_RING_SUSTAIN_INT,					1, 1, CLASS_HIGH_MAGE },
	{ TV_SOFT_ARMOR,		SV_ROBE,									1, 1, CLASS_HIGH_MAGE },
	{ TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_HIGH_MAGE },
	{ TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_HIGH_MAGE },
	{ TV_SWORD,				SV_DAGGER,								1, 1, CLASS_HIGH_MAGE },

	/* Terminate */
	{ 0, 						0, 										0, 0, 0 }
};

static init_equip_type equip_race_init[] =
{
	/* RACE_HUMAN */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HUMAN },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HUMAN },

	/* RACE_HALF_ELF */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_ELF },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_ELF },

	/* RACE_ELF */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_ELF },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_ELF },

	/* RACE_HOBBIT */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HOBBIT },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HOBBIT },

	/* RACE_GNOME */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_GNOME },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_GNOME },

	/* RACE_DWARF */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_DWARF },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_DWARF },

	/* RACE_HALF_ORC */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_ORC },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_ORC },

	/* RACE_HALF_TROLL */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_TROLL },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_TROLL },

	/* RACE_AMBERITE */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_AMBERITE },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_AMBERITE },

	/* RACE_HIGH_ELF */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HIGH_ELF },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HIGH_ELF },

	/* RACE_BARBARIAN */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_BARBARIAN },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_BARBARIAN },

	/* RACE_HALF_OGRE */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_OGRE },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_OGRE },

	/* RACE_HALF_GIANT */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_GIANT },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_GIANT },

	/* RACE_HALF_TITAN */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_TITAN },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_TITAN },

	/* RACE_CYCLOPS */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_CYCLOPS },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_CYCLOPS },

	/* RACE_YEEK */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_YEEK },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_YEEK },

	/* RACE_KLACKON */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_KLACKON },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_KLACKON },

	/* RACE_KOBOLD */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_KOBOLD },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_KOBOLD },

	/* RACE_NIBELUNG */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_NIBELUNG },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_NIBELUNG },

	/* RACE_DARK_ELF */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_DARK_ELF },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_DARK_ELF },

	/* RACE_DRACONIAN */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_DRACONIAN },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_DRACONIAN },

	/* RACE_MIND_FLAYER */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_MIND_FLAYER },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_MIND_FLAYER },

	/* RACE_IMP */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_IMP },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_IMP },

	/* RACE_GOLEM */
	{ TV_SCROLL,			SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_GOLEM },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_GOLEM },

	/* RACE_SKELETON */
	{ TV_SCROLL,			SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_SKELETON },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_SKELETON },

	/* RACE_ZOMBIE */
	{ TV_SCROLL,			SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_ZOMBIE },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_ZOMBIE },

	/* RACE_VAMPIRE */
	{ TV_SCROLL,			SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_VAMPIRE },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_VAMPIRE },

	/* RACE_SPECTRE */
	{ TV_SCROLL,			SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_SPECTRE },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_SPECTRE },

	/* RACE_SPRITE */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_SPRITE },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_SPRITE },

	/* RACE_BEASTMAN */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_BEASTMAN },
	{ TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_BEASTMAN },

	/* RACE_FIEND */
	{ TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_FIEND },

	/* Terminate */
	{ 0, 						0, 										0, 0, 0 }
};

static conv_equip_type equip_race_conv[] =
{
	/* RACE_HUMAN */

	/* RACE_HALF_ELF */

	/* RACE_ELF */

	/* RACE_HOBBIT */

	/* RACE_GNOME */

	/* RACE_DWARF */

	/* RACE_HALF_ORC */

	/* RACE_HALF_TROLL */

	/* RACE_AMBERITE */

	/* RACE_HIGH_ELF */

	/* RACE_BARBARIAN */
	{
		TV_RING, 			SV_RING_RES_FEAR,
		TV_RING,				SV_RING_SUSTAIN_STR,
		RACE_BARBARIAN
	},

	/* RACE_HALF_OGRE */

	/* RACE_HALF_GIANT */

	/* RACE_HALF_TITAN */

	/* RACE_CYCLOPS */

	/* RACE_YEEK */

	/* RACE_KLACKON */

	/* RACE_KOBOLD */

	/* RACE_NIBELUNG */

	/* RACE_DARK_ELF */

	/* RACE_DRACONIAN */

	/* RACE_MIND_FLAYER */

	/* RACE_IMP */

	/* RACE_GOLEM */

	/* RACE_SKELETON */

	/* RACE_ZOMBIE */

	/* RACE_VAMPIRE */

	/* RACE_SPECTRE */

	/* RACE_SPRITE */

	/* RACE_BEASTMAN */

	/* RACE_FIEND */
	{
		TV_RING, 			SV_RING_RES_FEAR,
		TV_RING,				SV_RING_SUSTAIN_STR,
		RACE_FIEND
	},
	{
		TV_BOW,	 			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_SWORD,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_POLEARM,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_HAFTED,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_CLOAK,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_GLOVES,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_BOOTS,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_LITE,				SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_CLOAK,			SV_ANY,
		0,						0,
		RACE_FIEND
	},

	/* Terminate */
	{
		0, 					0,
		0,						0,
		0
	}
};

static byte realm_to_tval[MAX_REALM] =
{
	0,
	TV_LIFE_BOOK,
	TV_SORCERY_BOOK,
	TV_NATURE_BOOK,
	TV_CHAOS_BOOK,
	TV_DEATH_BOOK,
	TV_TRUMP_BOOK,
	TV_ARCANE_BOOK,
};

static void race_conv_item(int *tval, int *sval)
{
	int i = 0;

	conv_equip_type *c_ptr;

	while(1)
	{
		/* Acquire this conversion */
		c_ptr = &equip_race_conv[i++];

		/* Stop if no more entries found */
		if (!c_ptr->tval1) break;

		/* Ignore incorrect race */
		if (c_ptr->tag != p_ptr->prace) continue;

		/* Ignore different conversions */
		if ((c_ptr->tval1 != *tval) ||
			 ((c_ptr->sval1 != SV_ANY) && (c_ptr->sval1 != *sval)))
		{
			continue;
		}

		/* Convert it */
		*tval = c_ptr->tval2;
		*sval = c_ptr->sval2;

		/* Done */
		break;
	}
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i, tv, sv;

	init_equip_type *i_ptr;

	object_type	object_type_body;
	object_type	*o_ptr = &object_type_body;

	i = 0;

	/* Give the player some useful objects based on their class */
	while (1)
	{
		/* Acquire this object */
		i_ptr = &equip_class_init[i++];

		/* Stop if no more entries found */
		if (!i_ptr->tval) break;

		/* Ignore incorrect class */
		if (i_ptr->tag != p_ptr->pclass) continue;

		tv = i_ptr->tval;
		sv = i_ptr->sval;

		switch(tv)
		{
			/* Hack -- initialize realm 1 */
			case TV_BOOK_REALM1:
			{
				tv = realm_to_tval[p_ptr->realm1];
				break;
			}
			/* Hack -- initialize realm 2 */
			case TV_BOOK_REALM2:
			{
				tv = realm_to_tval[p_ptr->realm2];
				break;
			}
		}

		/* Convert some useless objects */
		race_conv_item(&tv, &sv);

		/* Ignore useless objects */
		if (!tv) continue;

		/* Prepare the object */
		object_prep(o_ptr, lookup_kind(tv, sv));

		/* Roll the quantity */
		o_ptr->number = damroll(i_ptr->dice, i_ptr->side);

		/* Assassins begin the game with a poisoned dagger */
		if ((tv == TV_SWORD) &&
			 (p_ptr->pclass == CLASS_ROGUE) &&
			 (p_ptr->realm1 == REALM_DEATH))
		{
			o_ptr->name2 = EGO_BRAND_POIS;
		}

		/* Apply 'normal' magic */
		apply_magic(o_ptr, 0, FALSE, FALSE, FALSE);

		/* These objects are "storebought" and known */
		o_ptr->ident |= IDENT_STOREB | IDENT_MENTAL;

		object_aware(o_ptr);
		object_known(o_ptr);
		(void)inven_carry(o_ptr, FALSE);
	}

	i = 0;

	/* Give the player some useful objects based on their race */
	while (1)
	{
		/* Acquire this object */
		i_ptr = &equip_race_init[i++];

		/* Stop if no more entries found */
		if (!i_ptr->tval) break;

		/* Ignore incorrect race */
		if (i_ptr->tag != p_ptr->prace) continue;

		tv = i_ptr->tval;
		sv = i_ptr->sval;

		/* Prepare the object */
		object_prep(o_ptr, lookup_kind(tv, sv));

		/* Roll the quantity */
		o_ptr->number = damroll(i_ptr->dice, i_ptr->side);

		/* Apply 'normal' magic */
		apply_magic(o_ptr, 0, FALSE, FALSE, FALSE);

		/* These objects are "storebought" and known */
		o_ptr->ident |= IDENT_STOREB | IDENT_MENTAL;

		object_aware(o_ptr);
		object_known(o_ptr);
		(void)inven_carry(o_ptr, FALSE);
	}
}


/*
 * Helper function for 'player_birth()'
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.  And it cuts down on player stupidity.
 */
static bool player_birth_aux(void)
{
	int i, j, k, m, n, v;

	int mode;

	bool flag;
	bool prev = FALSE;

	cptr str;

	char c;

#if 0
	char p1 = '(';
#endif

	char p2 = ')';
	char b1 = '[';
	char b2 = ']';

	char buf[80];
	char inp[80];

	bool autoroll;



	/*** Intro ***/

	/* Clear screen */
	Term_clear();

	/* Title everything */
	put_str("Name        :", 2, 1);
	put_str("Sex         :", 3, 1);
	put_str("Race        :", 4, 1);
	put_str("Class       :", 5, 1);

	/* Dump the default name */
	c_put_str(TERM_L_BLUE, player_name, 2, 15);


	/*** Instructions ***/

	/* Display some helpful information */
	Term_putstr(5, 10, -1, TERM_WHITE,
		"Please answer the following questions.  Most of the questions");
	Term_putstr(5, 11, -1, TERM_WHITE,
		"display a set of standard answers, and many will also accept");
	Term_putstr(5, 12, -1, TERM_WHITE,
		"some special responses, including 'Q' to quit, 'S' to restart,");
	Term_putstr(5, 13, -1, TERM_WHITE,
		"and '?' for help.  Note that 'Q' and 'S' must be capitalized.");


	/*** Player sex ***/

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
		sprintf(buf, "Choose a sex (%c-%c): ", I2A(0), I2A(n-1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}

	/* Set sex */
	p_ptr->psex = k;
	sp_ptr = &sex_info[p_ptr->psex];
	str = sp_ptr->title;

	/* Display */
	c_put_str(TERM_L_BLUE, str, 3, 15);

	/* Clean up */
	clear_from(15);


	/*** Player race ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Your 'race' determines various intrinsic factors and bonuses.");
	hack_mutation = FALSE;

	/* Dump races */
	for (n = 0; n < MAX_RACES; n++)
	{
		/* Analyze */
		p_ptr->prace = n;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;
		
		/* Display */

		if (n<RACE_VAMPIRE)
			sprintf(buf, "%c%c %s", I2A(n), p2, str);
		else
			sprintf(buf, "%d%c %s", (n - RACE_ZOMBIE), p2, str); /* HACK */
		put_str(buf, 17 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while (1)
	{
		  sprintf(buf, "Choose a race (%c-4): ", I2A(0));
		  put_str(buf, 16, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		  if (c == '1')
		  {
				k = RACE_VAMPIRE;
				break;
		  }
		  else if (c == '2')
		  {
				k = RACE_SPECTRE;
				break;
		  }
		  else if (c == '3')
		  {
				k = RACE_SPRITE;
				break;
		  }
		  else if (c == '4')
		  {
				k = RACE_BEASTMAN;
				break;
		  }
		  else if (c == '5')
		  {
				k = RACE_FIEND;
				break;
		  }
		  else
		  {
				k = (islower(c) ? A2I(c) : -1);
            if ((k >= 0) && (k < n)) break;
            if (c == '?') do_cmd_help("help.hlp");
            else bell();
        }
	}

	/* Set race */
	p_ptr->prace = k;
	rp_ptr = &race_info[p_ptr->prace];
	str = rp_ptr->title;

	/* Display */
	c_put_str(TERM_L_BLUE, str, 4, 15);

	/* Clean up */
	clear_from(15);


	/*** Player class ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Your 'class' determines various intrinsic abilities and bonuses.");
	Term_putstr(5, 16, -1, TERM_WHITE,
		  "Any entries in parentheses should only be used by advanced players.");

	/* Dump classes */
	for (n = 0; n < MAX_CLASS; n++)
	{
		cptr mod = "";

		/* Analyze */
		p_ptr->pclass = n;
		cp_ptr = &class_info[p_ptr->pclass];
		mp_ptr = &magic_info[p_ptr->pclass];
		str = cp_ptr->title;

	 if (!(rp_ptr->choice & (1L << n )))
		  sprintf(buf, "%c%c (%s)%s", I2A(n), p2, str, mod);
    else
		/* Display */
		sprintf(buf, "%c%c %s%s", I2A(n), p2, str, mod);

		  put_str(buf, 19 + (n/3), 2 + 20 * (n%3));
	}

	/* Get a class */
	while (1)
	{
		sprintf(buf, "Choose a class (%c-%c): ", I2A(0), I2A(n-1));
		  put_str(buf, 18, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n)) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}

	/* Set class */
	p_ptr->pclass = k;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &magic_info[p_ptr->pclass];

	/* Display */
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

	/* Clean up */

	clear_from(15);

	 get_realms();

		  if (p_ptr->realm1 || p_ptr->realm2)
			 put_str("Magic       :", 6, 1);
		  if (p_ptr->realm1)
			 c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm1],6,15);
		  if (p_ptr->realm2)
			 c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm2],7,15);


	/*** Chaos patron ***/
	if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
	{
		put_str("Patron      :", 7, 1);

		/* Extra info */
		Term_putstr(5, 15, -1, TERM_WHITE,
			"Your 'patron' grants you certain abilities and may offer rewards.");

		/* Dump classes */
		for (n = 0; n < MAX_PATRON; n++)
		{
			/* Analyze */
			str = chaos_patrons[n];

			/* Display */
			sprintf(buf, "%c%c %s", I2A(n), p2, str);

			put_str(buf, 18 + (n/5), 2 + 15 * (n%5));
		}

		/* Get a class */
		while (1)
		{
			sprintf(buf, "Choose a patron (%c-%c): ", I2A(0), I2A(n-1));
			put_str(buf, 17, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			k = (islower(c) ? A2I(c) : -1);
			if (c == '*') k = rand_int(MAX_PATRON);
			if ((k >= 0) && (k < n)) break;
			if (c == '?') do_cmd_help("help.hlp");
			else bell();
		}

		/* Set class */
		p_ptr->chaos_patron = k;
		str = chaos_patrons[k];

		/* Display */
		c_put_str(TERM_L_BLUE, str, 7, 15);

		/* Clean up */

		clear_from(15);
	}
	else
	{
		p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;
	}



	/*** Maximize mode ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Using 'maximize' mode makes the game harder at the start,");
	Term_putstr(5, 16, -1, TERM_WHITE,
		  "but often makes it easier to win. In Zangband, 'maximize'");
	 Term_putstr(5, 17, -1, TERM_WHITE,
		  "mode is recommended for spellcasters.");

	/* Ask about "maximize" mode */
	while (1)
	{
		put_str("Use 'maximize' mode? (y/n) ", 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == ESCAPE) break;
		if ((c == 'y') || (c == 'n')) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}

	/* Set "maximize" mode */
	p_ptr->maximize = (c == 'y');

	/* Clear */
	clear_from(15);


	/*** Preserve mode ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Using 'preserve' mode makes it difficult to 'lose' artifacts,");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"but eliminates the 'special' feelings about some levels.");

	/* Ask about "preserve" mode */
	while (1)
	{
		put_str("Use 'preserve' mode? (y/n) ", 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == ESCAPE) break;
		if ((c == 'y') || (c == 'n')) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}

	/* Set "preserve" mode */
	p_ptr->preserve = (c == 'y');

	/* Clear */
	clear_from(15);


#ifdef ALLOW_AUTOROLLER

	/*** Autoroll ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"The 'autoroller' allows you to specify certain 'minimal' stats,");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"but be warned that your various stats may not be independant!");

	/* Ask about "auto-roller" mode */
	while (1)
	{
		put_str("Use the Auto-Roller? (y/n) ", 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == ESCAPE) break;
		if ((c == 'y') || (c == 'n')) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}

	/* Set "autoroll" */
	autoroll = (c == 'y');

	/* Clear */
	clear_from(15);


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
			j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

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
			sprintf(buf, "%-5s%-20s", stat_names[i], inp);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		put_str("Social class:", 22, 5);

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

		/* Move the cursor */
		put_str("", 22, 30);

		/* Clear the input */
		strcpy(inp, "");

		/* Get a response (or escape) */
		if (!askfor_aux(inp, 8)) inp[0] = '\0';

		/* Extract the number */
		v = atoi(inp);

		/* Save the minimum social class */
		sc_limit = (v > 0) ? ((v < 100) ? v : 100) : 0;
	}

#endif /* ALLOW_AUTOROLLER */

	/* Clean up */
	clear_from(10);

	/*** User enters number of quests ***/
	/* Heino Vander Sanden and Jimmy De Laet */

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"You can input yourself the number of quest you'd like to");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"perform next to two obligatory ones ( Oberon and the Serpent of Chaos )");
	Term_putstr(5, 17, -1, TERM_WHITE,
		"In case you do not want any additional quest, just enter 0");

	/* Ask the number of additional quests */
	while (1)
	{
		put_str(format("Number of additional quests? (<%u) ", MAX_Q_IDX - DEFAULT_QUESTS + 1), 20, 2);

		/* Get a the number of additional quest */
		while (TRUE)
		{
			/* Move the cursor */
			put_str("", 20, 38);

			/* Default */
			strcpy(inp, "20");

			/* Get a response (or escape) */
			if (!askfor_aux(inp, 3)) inp[0] = '\0';
			v = atoi(inp);

			/* Break on valid input */
			if ((v < MAX_Q_IDX - DEFAULT_QUESTS + 1) && (v >= 0)) break;
		}
		break;
	}

	total_quests = v + DEFAULT_QUESTS;

	/* Clear */
	clear_from(15);

	/* Generate quests */
	player_birth_quests();


	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		/* Feedback */
		if (autoroll)
		{
			Term_clear();

			put_str("Name        :", 2, 1);
			put_str("Sex         :", 3, 1);
			put_str("Race        :", 4, 1);
			put_str("Class       :", 5, 1);

			c_put_str(TERM_L_BLUE, player_name, 2, 15);
			c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);
			c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 15);
			c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

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
			if (auto_round >= 1000000L)
			{
				auto_round = 1;

				for (i = 0; i < 6; i++)
				{
					stat_match[i] = 0;
				}
			}

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

			/* Take note sometimes */
			flag = (!(auto_round % AUTOROLLER_STEP));

			/* Update display occasionally */
			if (flag || (auto_round < last_round + 100))
			{
				/* Dump data */
				birth_put_stats();

				/* Dump round */
				put_str(format("%10ld", auto_round), 9, 69);

				/* Make sure they see everything */
				Term_fresh();

				/* Delay 1/10 second */
/*				if (flag) Term_xtra(TERM_XTRA_DELAY, 100);*/

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

		while (1)
		{
			/* Roll for social class */
			get_history();

			/* Not autorolling */
			if (!autoroll) break;

			/* Accept good social class */
			if (p_ptr->sc >= sc_limit) break;
		}

		/* Roll for gold */
		get_money();

		p_ptr->muta1 = 0;
		p_ptr->muta2 = 0;
		p_ptr->muta3 = 0;

		/* Input loop */
		while (TRUE)
		{
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
				load_prev_data();
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
				do_cmd_help("help.hlp");
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

	/* Mutate beastmen */
	if (p_ptr->prace == RACE_BEASTMAN)
	{
		hack_mutation = TRUE;
	}

	/* Imps have claws */
	if (p_ptr->prace == RACE_IMP)
	{
		p_ptr->muta2 |= MUT2_CLAWS;
	}

	/* Check for patron-granted powers */
	if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
	{
		switch(p_ptr->chaos_patron)
		{
			/* Resist magic */
			case PATRON_SLORTAR:
			{
				p_ptr->muta3 |= MUT3_MAGIC_RES;
				break;
			}
			/* Masked appearance */
			case PATRON_MABELODE:
			{
				p_ptr->muta3 |= MUT3_ILL_NORM;
				break;
			}
			/* Random telepathy and warning */
			case PATRON_PYARAY:
			{
				p_ptr->muta2 |= MUT2_WEIRD_MIND | MUT2_WARNING;
				break;
			}
			/* Detect curse */
			case PATRON_BALAAN:
			{
				p_ptr->muta1 |= MUT1_DET_CURSE;
				break;
			}
			/* Random dispel all */
			case PATRON_EEQUOR:
			{
				p_ptr->muta2 |= MUT2_DISPEL_ALL;
				break;
			}
			/* Random mutation */
			case PATRON_BALO:
			{
				gain_random_mutation(0);
				break;
			}
			/* Random berserk */
			case PATRON_KHORNE:
			{
				p_ptr->muta2 |= MUT2_BERS_RAGE;
				break;
			}
			/* Random polymorph wounds */
			case PATRON_NURGLE:
			{
				p_ptr->muta2 |= MUT2_POLY_WOUND;
				break;
			}
			/* Augmented intelligence */
			case PATRON_TZEENTCH:
			{
				p_ptr->muta3 |= MUT3_HYPER_INT;
				break;
			}
			/* Random chaos */
			case PATRON_KHAINE:
			{
				p_ptr->muta2 |= MUT2_RAW_CHAOS;
				break;
			}
			/* Summon monsters */
			case PATRON_QLZQQLZUUP:
			{
				p_ptr->muta1 |= MUT1_SUMMON_M;
				break;
			}
			/* Mind blast */
			case PATRON_VECNA:
			{
				p_ptr->muta1 |= MUT1_MIND_BLST;
				break;
			}
			/*
			 * Pistol, shotgun, double shotgun, chaingun,
			 * rocket launcher, plasma cannon, BFG
			 * And...
			 * Random attract demon, berserk, dispel all (BFG!),
			 * or invulnerability
			 */
			case PATRON_ID:
			{
				p_ptr->muta1 |= MUT1_SHARD_BOLT | MUT1_SHARD_BLAST | MUT1_DSHARD_BLAST |
									 MUT1_CHAIN_SHARDS | MUT1_ROCKET | MUT1_PLAS_BOLT | MUT1_BFG;
				
				switch(rand_int(13))
				{
					case 0:	case 1:	case 2:	case 3:	case 4:
					{
						p_ptr->muta2 |= MUT2_ATT_DEMON;
						break;
					}
					case 5:	case 6:	case 7:
					{
						p_ptr->muta2 |= MUT2_BERS_RAGE;
						break;
					}
					case 8:
					{
						p_ptr->muta2 |= MUT2_DISPEL_ALL;
						break;
					}
					case 9:
					{
						p_ptr->muta2 |= MUT2_INVULN;
						break;
					}
				}
			}
		}
	}

	/* Accept */
	return (TRUE);
}


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	int i, n;

	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
	}


	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add("  ");
	message_add(" ");


	/* Hack -- outfit the player */
	player_outfit();

	/* Shops */
	for (n = 0; n < MAX_STORES; n++)
	{
		/* Initialize */
		store_init(n);

		/* Ignore home */
		if (n == 7) continue;

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(n);
	}
}



