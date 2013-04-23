#define BIRTH_C
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
	s16b birthday;

	s32b au;

	s16b stat[A_MAX];

	char history[4][60];
};





/*
 * Forward declare
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
	cptr info;     /* Textual History */

	byte roll;     /* Frequency of this entry */
	byte chart;     /* Chart index */
	byte next;     /* Next chart index */
	byte bonus;     /* Social Class Bonus + 50 */
};


/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human/         -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
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
	{"You are the illegitimate and unacknowledged child ",  10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ",  20, 1, 2, 35},
	{"You are one of several children ",             95, 1, 2, 45},
	{"You are the first child ", 100, 1, 2, 50},

	{"of a Serf.  ",  40, 2, 3, 65},
	{"of a Yeoman.  ",  65, 2, 3, 80},
	{"of a Townsman.  ",  80, 2, 3, 90},
	{"of a Guildsman.  ",  90, 2, 3, 105},
	{"of a Landed Knight.  ",  96, 2, 3, 120},
	{"of a Noble Family.  ",    99, 2, 3, 130},
	{"of the Royal Blood Line.  ",                100, 2, 3, 140},

	{"You are the black sheep of the family.  ",  20, 3, 50, 20},
	{"You are a credit to the family.  ",  80, 3, 50, 55},
	{"You are a well liked child.  ", 100, 3, 50, 60},

	{"Your mother was of the Teleri.  ",  40, 4, 1, 50},
	{"Your father was of the Teleri.  ",  75, 4, 1, 55},
	{"Your mother was of the Noldor.  ",    90, 4, 1, 55},
	{"Your father was of the Noldor.  ",    95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ",  98, 4, 1, 65},
	{"Your father was of the Vanyar.  ", 100, 4, 1, 70},

	{"You are one of several children ",  60, 7, 8, 50},
	{"You are the only child ", 100, 7, 8, 55},

	{"of a Teleri ",  75, 8, 9, 50},
	{"of a Noldor ",  95, 8, 9, 55},
	{"of a Vanyar ", 100, 8, 9, 60},

	{"Ranger.  ",  40, 9, 54, 80},
	{"Archer.  ",  70, 9, 54, 90},
	{"Warrior.  ",  87, 9, 54, 110},
	{"Wizard.  ",  95, 9, 54, 125},
	{"Prince.  ",  99, 9, 54, 140},
	{"King.  ", 100, 9, 54, 145},

	{"You are one of several children of a Hobbit ",  85, 10, 11, 45},
	{"You are the only child of a Hobbit ",         100, 10, 11, 55},

	{"Bum.  ",  20, 11, 3, 55},
	{"Tavern Owner.  ",  30, 11, 3, 80},
	{"Miller.  ",  40, 11, 3, 90},
	{"Home Owner.  ",  50, 11, 3, 100},
	{"Burglar.  ",  80, 11, 3, 110},
	{"Warrior.  ",  95, 11, 3, 115},
	{"Wizard.  ",  99, 11, 3, 125},
	{"Clan Elder.  ", 100, 11, 3, 140},

	{"You are one of several children of a Gnome ",  85, 13, 14, 45},
	{"You are the only child of a Gnome ", 100, 13, 14, 55},

	{"Beggar.  ",  20, 14, 3, 55},
	{"Braggart.  ",  50, 14, 3, 70},
	{"Prankster.  ",  75, 14, 3, 85},
	{"Warrior.  ",  95, 14, 3, 100},
	{"Wizard.  ", 100, 14, 3, 125},

	{"You are one of two children of a Dwarven ",  25, 16, 17, 40},
	{"You are the only child of a Dwarven ", 100, 16, 17, 50},

	{"Thief.  ",  10, 17, 18, 60},
	{"Prison Guard.  ",  25, 17, 18, 75},
	{"Miner.  ",  75, 17, 18, 90},
	{"Warrior.  ",  90, 17, 18, 110},
	{"Priest.  ",  99, 17, 18, 130},
	{"King.  ", 100, 17, 18, 150},

	{"You are the black sheep of the family.  ",  15, 18, 57, 10},
	{"You are a credit to the family.  ",  85, 18, 57, 50},
	{"You are a well liked child.  ", 100, 18, 57, 55},

	{"Your mother was an Orc, but it is unacknowledged.  ",  25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ", 100, 19, 20, 25},

	{"You are the adopted child ", 100, 20, 2, 50},

	{"Your mother was a Cave-Troll ",  30, 22, 23, 20},
	{"Your father was a Cave-Troll ",  60, 22, 23, 25},
	{"Your mother was a Hill-Troll ",  75, 22, 23, 30},
	{"Your father was a Hill-Troll ",  90, 22, 23, 35},
	{"Your mother was a Water-Troll ",  95, 22, 23, 40},
	{"Your father was a Water-Troll ", 100, 22, 23, 45},

	{"Cook.  ",   5, 23, 62, 60},
	{"Warrior.  ",  95, 23, 62, 55},
	{"Shaman.  ",  99, 23, 62, 65},
	{"Clan Chief.  ", 100, 23, 62, 80},

	{"You have dark brown eyes, ",  20, 50, 51, 50},
	{"You have brown eyes, ",  60, 50, 51, 50},
	{"You have hazel eyes, ",  70, 50, 51, 50},
	{"You have green eyes, ",  80, 50, 51, 50},
	{"You have blue eyes, ",  90, 50, 51, 50},
	{"You have blue-grey eyes, ", 100, 50, 51, 50},

	{"straight ",  70, 51, 52, 50},
	{"wavy ",  90, 51, 52, 50},
	{"curly ", 100, 51, 52, 50},

	{"black hair, ",  30, 52, 53, 50},
	{"brown hair, ",  70, 52, 53, 50},
	{"auburn hair, ",  80, 52, 53, 50},
	{"red hair, ",  90, 52, 53, 50},
	{"blond hair, ", 100, 52, 53, 50},

	{"and a very dark complexion.",  10, 53, 0, 50},
	{"and a dark complexion.",  30, 53, 0, 50},
	{"and an average complexion.",  80, 53, 0, 50},
	{"and a fair complexion.",  90, 53, 0, 50},
	{"and a very fair complexion.", 100, 53, 0, 50},

	{"You have light grey eyes, ",  85, 54, 55, 50},
	{"You have light blue eyes, ",  95, 54, 55, 50},
	{"You have light green eyes, ", 100, 54, 55, 50},

	{"straight ",  75, 55, 56, 50},
	{"wavy ", 100, 55, 56, 50},

	{"black hair, and a fair complexion.",  75, 56, 0, 50},
	{"brown hair, and a fair complexion.",  85, 56, 0, 50},
	{"blond hair, and a fair complexion.",  95, 56, 0, 50},
	{"silver hair, and a fair complexion.", 100, 56, 0, 50},

	{"You have dark brown eyes, ",  99, 57, 58, 50},
	{"You have glowing red eyes, ", 100, 57, 58, 60},

	{"straight ",  90, 58, 59, 50},
	{"wavy ", 100, 58, 59, 50},

	{"black hair, ",  75, 59, 60, 50},
	{"brown hair, ", 100, 59, 60, 50},

	{"a one foot beard, ",  25, 60, 61, 50},
	{"a two foot beard, ",  60, 60, 61, 51},
	{"a three foot beard, ",  90, 60, 61, 53},
	{"a four foot beard, ", 100, 60, 61, 55},

	{"and a dark complexion.", 100, 61, 0, 50},

	{"You have slime green eyes, ",  60, 62, 63, 50},
	{"You have puke yellow eyes, ",  85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ",  99, 62, 63, 50},
	{"You have glowing red eyes, ", 100, 62, 63, 55},

	{"dirty ",  33, 63, 64, 50},
	{"mangy ",  66, 63, 64, 50},
	{"oily ", 100, 63, 64, 50},

	{"sea-weed green hair, ",  33, 64, 65, 50},
	{"bright red hair, ",  66, 64, 65, 50},
	{"dark purple hair, ", 100, 64, 65, 50},

	{"and green ",  25, 65, 66, 50},
	{"and blue ",  50, 65, 66, 50},
	{"and white ",  75, 65, 66, 50},
	{"and black ", 100, 65, 66, 50},

	{"ulcerous skin.",  33, 66, 0, 50},
	{"scabby skin.",  66, 66, 0, 50},
	{"leprous skin.",                       100, 66, 0, 50},

	{"You are an unacknowledged child of ", 50, 67, 68, 45},
	{"You are a rebel child of ",         80, 67, 68, 65},
	{"You are a long lost child of ",     100, 67, 68, 55},

	{"someone with Great One blood.  ",               50, 68, 50, 80 },
	{"an unknown child of a Great One.  ", 65, 68, 50, 90 },
	{"an unknown Great One.  ", 79, 68, 50, 100 },
	{"Karakal.  ",       80, 68, 50, 130 },
	{"Hagarg Ryonis.  ",        83, 68, 50, 105 },
	{"Lobon.  ",       84, 68, 50, 105 },
	{"Nath-Horthath.  ",        85, 68, 50, 90 },
	{"Tamash.  ",        87, 68, 50, 100 },
	{"Zo-Kalar.  ",       88, 68, 50, 125 },
	{"Karakal.  ",      89, 68, 50, 120 },
	{"Hagarg Ryonis.  ",       90, 68, 50, 140 },
	{"Lobon.  ",     91, 68, 50, 115 },
	{"Nath-Horthath.  ",       92, 68, 50, 110 },
	{"Tamash.  ",       93, 68, 50, 105 },
	{"Zo-Kalar.  ",        94, 68, 50, 95 },
	{"Karakal.  ",        95, 68, 50, 115 },
	{"Hagarg Ryonis.  ",        96, 68, 50, 110 },
	{"Lobon.  ",         97, 68, 50, 135 },
	{"Nath-Horthath.  ",      98, 68, 50, 90 },
	{"Tamash.  ",       99, 68, 50, 105 },
	{"Zo-Kalar.  ",       100, 68, 50, 80 },


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
	{"Vort, the Kobold Queen.  ",     100, 83, 80, 100 },

	{"You are one of several children of a Klackon hive queen.  "
			, 100, 84, 85, 50 },

	{"You have red skin, ", 40, 85, 86, 50 },
	{"You have black skin, ", 90, 85, 86, 50 },
	{"You have yellow skin, ", 100, 85, 86, 50 },

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
	{"Wizard.  ", 85, 90, 91, 70 },
	{"Noble.  ", 100, 90, 91, 100 },

	{"You have green wings, green skin and yellow belly.", 30, 91, 0, 50 },
	{"You have green wings, and green skin.", 55, 91, 0, 50 },
	{"You have red wings, and red skin.", 80, 91, 0, 50 },
	{"You have black wings, and black skin.", 90, 91, 0, 50 },
	{"You have metallic skin, and shining wings.", 100, 91, 0, 50},

	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80 },
	{"three tentacles around your mouth.", 20, 93, 0, 45 },
	{"four tentacles around your mouth.", 80, 93, 0, 50 },
	{"five tentacles around your mouth.", 100, 93, 0, 55 },

	{"You ancestor was ", 100, 94, 95, 50 },

	{"a mindless demonic spawn.  ", 30, 95, 96, 20 },
	{"a minor demon.  ", 60, 95, 96, 50 },
	{"a major demon.  ", 90, 95, 96, 75 },
	{"a demon lord.  ", 100, 95, 96, 99 },

	{"You have red skin, ", 50, 96, 97, 50 },
	{"You have brown skin, ", 100, 96, 97, 50},

	{"claws, fangs, spikes, and glowing red eyes.", 40, 97, 0, 50 },
	{"claws, fangs, and glowing red eyes.", 70, 97, 0, 50 },
	{"claws, and glowing red eyes.", 100, 97, 0, 50 },

	{"You were shaped from ", 100, 98, 99, 50 },

	{"clay ", 40, 99, 100, 50 },
	{"stone ", 80, 99, 100, 50 },
	{"wood ", 85, 99, 100, 40 },
	{"iron ", 99, 99, 100, 50 },
	{"pure gold ", 100, 99, 100, 100},

	{"by a Kabbalist", 40, 100, 101, 50 },
	{"by a Wizard", 65, 100, 101, 50 },
	{"by an Alchemist", 90, 100, 101, 50},
	{"by a Priest", 100, 100, 101, 60},

	{" to fight evil.", 10, 101, 0, 65 },
	{".", 100, 101, 0, 50 },

	{"You were created by ", 100, 102, 103, 50 },

	{"a Necromancer.  ", 30, 103, 104, 50 },
	{"a magical experiment.  ", 50, 103, 104, 50 },
	{"an Evil Priest.  ", 70, 103, 104, 50 },
	{"a pact with the demons.  ", 75, 103, 104, 50 },
	{"a restless spirit.  ", 85, 103, 104, 50 },
	{"a curse.  ", 95, 103, 104, 30 },
	{"an oath.  ", 100, 103, 104, 50 },

	{"You have ", 100, 104, 105, 50 },
	{"dirty, dry bones, ", 40, 105, 106, 50 },
	{"rotten black bones, ", 60, 105, 106, 50 },
	{"filthy, brown bones, ", 80, 105, 106, 50 },
	{"shining white bones, ", 100, 105, 106, 50 },

	{"and glowing eyes.", 30, 106, 0, 50 },
	{"and eyes which burn with hellfire.", 50, 106, 0, 50 },
	{"and empty eyesockets.", 100, 106, 0, 50 },

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
	{"You have a blue-grey eye, ",               100, 109, 110, 50},

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

	{" and a deathly grey complexion. ", 100, 122, 123, 50 },
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
	{"grey fur, ",                    66, 132, 133, 50},
	{"albino fur, ",                  100, 132, 133, 50},

	{"and the hooves of a goat.",      50, 133, 0, 50 },
	{"and human feet.",        75, 133, 0, 50 },
	{"and bird's feet.",       85, 133, 0, 50 },
	{"and reptilian feet.",    90, 133, 0, 50 },
	{"and bovine feet.",       95, 133, 0, 50 },
	{"and feline feet.",       97, 133, 0, 50 },
	{"and canine feet.",       100, 133, 0, 50 },

	{"You have ", 100, 134, 120, 50 },

	{"", 0, 0, 0, 0 }
};



/*
 * Name segments for random player names
 */

typedef byte bc_type;

static void get_starting_skills(void);
static void get_hermetic_skills_randomly(void);
static bc_type get_hermetic_skills(void);
static void get_ahw_average(void);
static void get_money(bool randomly);
static s16b get_social_average(byte);
static void get_final(void);
static bc_type load_stat_set(bool);
static void roll_stats_auto(bool point_mod);
static void get_stats(void);

/* A macro to determine whether use_autoroller can be set and is set. */
#ifdef ALLOW_AUTOROLLER
#define USE_AUTOROLLER use_autoroller
#else
#define USE_AUTOROLLER FALSE
#endif

/*
 * Copy birth options to somewhere where they are read.
 */
static void get_birth_options(void)
{
	option_type *op_ptr, *o2_ptr;

	for (op_ptr = option_info; op_ptr->o_desc; op_ptr++)
	{
		/* Not a birth option. */
		if (op_ptr->o_page != OPTS_BIRTH) continue;

		o2_ptr = op_ptr+1;

		/* Not an option with a BIRTHR equivalent. */
		if (o2_ptr->o_page != OPTS_BIRTHR) continue;

		/* Copy the option across. */
		*(o2_ptr->o_var) = *(op_ptr->o_var);
	}
}

/* Return codes for birth_choice() */
#define BC_OKAY 0
#define BC_ABORT 1
#define BC_RESTART 2

/*
 * Display the birth option menu, notice when certain options change.
 */
static bc_type birth_option(void)
{
	bool old_allow_quickstart = allow_quickstart;
	bool old_allow_pickstats = allow_pickstats;
	bool old_maximise_mode = maximise_mode;
	Term_save();

	/* Modify the birth options as desired. */
	do_cmd_options_aux(7, "Startup Options", NULL);

	/* Copy the birth options to active locations. */
	get_birth_options();

	Term_load();
	/* Start again if the set of questions being asked changes.
	 * This does not include show_credits because this takes
	 * effect before we reach this point. */
	if (allow_quickstart && !old_allow_quickstart) return BC_RESTART;
	if (allow_pickstats && !old_allow_pickstats) return BC_RESTART;
	/* We need not restart for maximise mode, but we should
	 * try to keep the stats the same. We always abort because
	 * we may have passed point_mod_player(). This will be ignored
	 * if we haven't. This must be the last check.
	 */
	if (old_maximise_mode != maximise_mode)
	{
		byte x;
		for (x = 0; x < A_MAX; x++)
		{
			s16b mod = rp_ptr->r_adj[x]+cp_ptr->c_adj[x];
			if (maximise_mode) mod *= -1;
			mod = modify_stat_value(p_ptr->stat_cur[x], mod);
			p_ptr->stat_cur[x] = p_ptr->stat_max[x] = mod;
		}
		return BC_ABORT;
	}
	return BC_OKAY;
}

/*
 * Ask for a range of options. Accept a few other specific responses.
 * The list of options should already have been displayed.
 *
 * allow_abort should be set if aborting is distinct from starting afresh.
 */
static bc_type birth_choice(int row, s16b max, cptr prompt, int *option,
	bool allow_abort)
{
	char c, pmt[256];
	cptr s = (allow_abort) ? ", * for random or ESCAPE to abort" :
		" or * for random";
	strnfmt(pmt, sizeof(pmt), "%s (%c-%c%s): ", prompt, I2A(0), rtoa(max-1), s);
	while (1)
	{
		put_str(pmt, row, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		else if (c == 'S') return BC_RESTART;
		else if (c == '?') do_cmd_help(NULL);
		else if (c == ESCAPE && allow_abort) return BC_ABORT;
		else if (c == '=')
		{
			bc_type b = birth_option();
			if (allow_abort && b == BC_ABORT) return b;
			if (b == BC_RESTART) return b;
		}
		else if (c == '*')
		{
			(*option) = rand_int(max);
			return BC_OKAY;
		}
		else
		{
			(*option) = ator(c);
			if (((*option) >= 0) && ((*option) < max)) return BC_OKAY;
			else bell(0);
		}
	}
}

/*
 * For characters starting with shaman skill, start them with a spirit
 */
static bc_type get_init_spirit(bool choice)
{
	int k;
	bc_type b;

	/* Only those with some shamanism skill start with a spirit. */
	if (!skill_set[SKILL_SHAMAN].value) return BC_OKAY;

	/* Acquire a random spirit if only temporary. */
	if (!choice)
	{
		spirits[rand_int(MAX_SPHERE)].pact = TRUE;
		return BC_OKAY;
	}

	/* Ask the player. */
	clear_from(15);

	b = birth_choice(17, MAX_SPHERE, "Choose a life spirit(a) or wild spirit(b)", &k, TRUE);

	/* Nothing chosen. */
	if (b != BC_OKAY) return b;

	/* Name the spirit. */
	generate_spirit_names();

	/* Form a pact with it. */
	spirits[k].pact = TRUE;

	/* Finished. */
	return BC_OKAY;
}
/*
 * Add the random element to each skill, set the maximum and reset the base where
 * appropriate. This must be run after the other skill increases.
 */

static void get_random_skills(bool random)
{
	int i;
	for (i=0;i<MAX_SKILLS;i++)
	{
		if(skill_set[i].value>10)
		{
			skill_set[i].value+= (random ? (byte)rand_int(10) : 4);
			}
		else if(skill_set[i].value>5)
			{
		skill_set[i].value+= (random ? (byte)rand_int(5) : 2);
			}
		else if(skill_set[i].value>0)
		{
			skill_set[i].value+= (random ? (byte)rand_int(3) : 1);
		}
		skill_set[i].max_value=skill_set[i].value;
	}
	/* Set bases for arcane skills to be the same as the current values since
	 * they start low - may want to do this for all skills later, but lowering
	 * the values first (and do this for martial arts skill also).
	 */
	skill_set[SKILL_MANA].base = skill_set[SKILL_MANA].value;
	skill_set[SKILL_MA].base = skill_set[SKILL_MA].value;
	skill_set[SKILL_CORPORIS].base = skill_set[SKILL_CORPORIS].value;
	skill_set[SKILL_ANIMAE].base = skill_set[SKILL_ANIMAE].value;
	skill_set[SKILL_VIS].base = skill_set[SKILL_VIS].value;
	skill_set[SKILL_NATURAE].base = skill_set[SKILL_NATURAE].value;
	skill_set[SKILL_NECROMANCY].base = skill_set[SKILL_NECROMANCY].value;
	skill_set[SKILL_SORCERY].base = skill_set[SKILL_SORCERY].value;
	skill_set[SKILL_CONJURATION].base = skill_set[SKILL_CONJURATION].value;
	skill_set[SKILL_THAUMATURGY].base = skill_set[SKILL_THAUMATURGY].value;
	skill_set[SKILL_MINDCRAFTING].base = skill_set[SKILL_MINDCRAFTING].value;
	skill_set[SKILL_CHI].base = skill_set[SKILL_CHI].value;
	skill_set[SKILL_SHAMAN].base = skill_set[SKILL_SHAMAN].value;
	skill_set[SKILL_HEDGE].base = skill_set[SKILL_HEDGE].value;

	/* Set hack_chaos_feature now all relevant information is known. */
	hack_chaos_feature = ((skill_set[SKILL_THAUMATURGY].value > 0) ||
		player_has_flag(TR0, TR0_CHAOS));
}

/*
 * Imagine you were wielding a whip...
 */
static void wield_weapons(bool wield)
{
	if (wield)
	{
		int k;

		/* Hack - there are two reasonable choices for example weapon. */
		if (cp_ptr->skill[SKILL_STAB] > cp_ptr->skill[SKILL_CLOSE])
			k = OBJ_RAPIER;
		else
			k = OBJ_WHIP;

		object_prep(&inventory[INVEN_WIELD], k);
		object_prep(&inventory[INVEN_BOW], OBJ_LONG_BOW);
	}
	else
	{
		object_wipe(&inventory[INVEN_WIELD]);
		object_wipe(&inventory[INVEN_BOW]);
	}
	p_ptr->update |= PU_BONUS;
	update_stuff();
}

/*
 * Change the number of points available as a result of a change in a stat.
 */
static int change_points_by_stat(s16b from, s16b to)
{
	s16b points = 0;
	/* Remove the last digit to simplify matters */
	if (from > 18) from = 18 + (from-18)/10;
	if (to > 18) to = 18 + (to-18)/10;

	if (from < to) /* Increase stat case */
	{
		while (from < to)
		{
			from++;

			/* Higher stats linearly cost more */
			if(from > 25) points--;
			if(from > 22) points--;
			if(from > 18) points--;
			if(from > 14) points--;
			if(from > 3) points--;
			continue;
		}
	}
	else /* Reduce stat case */
	{
		while (from > to)
		{
			from--;

			/* Higher stats yield more mod points */
			if(from > 24) points++;
			if(from > 21) points++;
			if(from > 17) points++;
			if(from > 13) points++;
			if(from > 2) points++;
			continue;
		}
	}
	return points;
}

/* Save the current stats */
static errr save_stats(void)
{
	FILE *fff;
	int i,l;

	/* Paranoia - check that there's something to save (the default entry does not count). */
	if (stat_default_total <= 1) return SUCCESS;

	/* Drop priv's */
	safe_setuid_drop();

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen_path(ANGBAND_DIR_USER, "user-loc.prf", "a");

	/* Failure */
	if (!fff) return FILE_ERROR_CANNOT_OPEN_FILE;

	/* Start dumping */
	fprintf(fff, "\n\n# Initial stat dump\n\n");

	for (i = 1; i < stat_default_total; i++)
	{
		stat_default_type *sd_ptr = &stat_default[i];
		fprintf(fff, "D:%s:%s:%s:%d",
			sex_info[sd_ptr->sex].title, race_info[sd_ptr->race].title,
			tp_name+template_info[sd_ptr->template].name, sd_ptr->maximise);

		for (l = 0; l< A_MAX; l++) fprintf(fff, ":%d", sd_ptr->stat[l]);

		fprintf(fff, ":%s\n", quark_str(sd_ptr->name));
	}

	/* Close */
	my_fclose(fff);

	/* Grab priv's */
	safe_setuid_grab();

	mc_put_fmt(0, 2, "$gStats saved.");


	/* Success */
	return (0);
}

/*
 * Determine the maximum value for a stat given a specific race and a specific template.
 */
int maxstat(int race, int temp, int stat)
{
	int x;
	x = (race_info[race].r_adj[stat] + template_info[temp].c_adj[stat]);
	if(x > 8) x = 8;
	if(x > 0) x *= 13;
	if(x >= 0) x = (x/10)*10+28;
	else x+=19;
	return x;
}

/* Display player information for point_mod_player(). */
static void display_player_birth(int points, bool details, bool rolled)
{
	/* Mention finishing if it is possible at the moment. */
	cptr finstr = (rolled || (spend_points && points >= 0)) ?
		"'ESC' to finish, " : "";

	/* Only mention the autoroller if allowed. */
	cptr arstr = (USE_AUTOROLLER) ? " with minima" : "";

	/* Display details if required. */
	if (details)
	{
		display_player(DPLAY_BIRTH);
	}
	else
	{
		display_player(DPLAY_PLAYER);
	}
	/* Display the information required during creation. */
	clear_from(23);

	mc_put_fmt(2, 73, "<-S/s+>");
	mc_put_fmt(3, 73, "<-I/i+>");
	mc_put_fmt(4, 73, "<-W/w+>");
	mc_put_fmt(5, 73, "<-D/d+>");
	mc_put_fmt(6, 73, "<-C/c+>");
	mc_put_fmt(7, 73, "<-H/h+>");

	/* These should be the same as in display_player_misc_info() */
	mc_put_fmt(2, 1, "<N>Name");
	mc_put_fmt(3, 1, "<G>Sex"); /* Sigh... */
	mc_put_fmt(4, 1, "<R>Race");
	mc_put_fmt(5, 1, "<T>Template");

	/* Start the first string. */
	mc_put_fmt(21, 2, "[");

	/* Calculate point string, if used. */
	if (spend_points)
	{
		/* Pick a colour appropriate to the circumstances. */
		char attr = (!points) ? 'g' : (points > 0) ? 'y' : (rolled) ? 'B' : 'r';

		/* Print it out. */
		mc_add_fmt("$%c%d$w points left. ", attr, points);
	}

	/* Write the rest of the first string. */
	mc_add_fmt("Press %sX to restart,]", finstr);

	/* Write the second string. */
	mc_put_fmt(22, 2,
		"['f' to save, 'l' to load, '/' to change display, '=' for options,]");

	/* Write the third string. */
	mc_put_fmt(23, 2, "['a' to roll%s, or '?' for help.]", arstr);
}

/* A macro for A = (A+B) mod M */
#define MOD_ADD(A, B, M) \
	(A) += (B)+(M); \
	(A) %= (M);

/* Indexes for point_mod_player (0-6 hard-coded as stats and nothing) */
#define IDX_STATS ((A_STR+1) | (A_INT+1) | (A_WIS+1) | (A_DEX+1) | (A_CON+1) | (A_CHR+1))
#define IDX_RACE 0x0008
#define IDX_TEMPLATE 0x0010
#define IDX_FILE 0x0020
#define IDX_FINISH 0x0040
#define IDX_RAND_ONE 0x0080
#define IDX_LOAD 0x0100
#define IDX_START 0x0200
#define IDX_DETAILS 0x0400
#define IDX_NAME 0x0800
#define IDX_SEX 0x1000
#define IDX_OPTION 0x2000
#define IDX_HELP 0x4000
#define IDX_ROLL 0x8000
#define IDX_ALL (IDX_RACE | IDX_TEMPLATE | IDX_LOAD)


/*
 * Allow player to modify the character by spending points
 *
 * own_name keeps track of whether the player has changed the name explicitly.
 * This is because the game tries to generate names appropriate to the race,
 * and so generates a new one whenever the race is changed.
 * As the player is assumed to want any name he has set himself, setting a
 * name disables the process.
 *
 * rolled is set whenever the stat set was generated directly by rolling. This
 * is because such characters are accepted regardless of their stats.
 */
static bool point_mod_player(void)
{
	bool details, own_name, rolled;
	char UNREAD(stat); /* Never used when i = IDX_ALL, and initialised below otherwise. */
	s16b UNREAD(points); /* Initialised when i = IDX_ALL */
	u16b i;

	/* Synchronise the birth options initially. */
	get_birth_options();

	/* Set cheat_item to ensure that the weapons are described fully. Note
	 * that this has no long-term effects. */
	cheat_item = TRUE;

	/* The game inserts a name at the beginning, but the player can
	 * change it. We will change the name whenever a new character is
	 * generated, but not if the user has selected a name himself.
	 */

	for (i = IDX_START, details = own_name = rolled = FALSE; i != IDX_FINISH;)
	{
		/* Hack - use IDX_START to initialise various things.
		 * i should not be set to IDX_ALL anywhere else.
		 * i == IDX_START should not be checked anywhere else.
		 */
		if (i == IDX_START)
		{
			i = IDX_ALL;
		}
		else
		{
			/* Get an entry */
			stat = inkey();

			/* Assign values to entries, stats 0 to 5 */
			switch(stat)
			{

			/* The index to a specific stat is retrieved */
			case 's': case 'S':
				i = A_STR+1;
				break;
			case 'i': case 'I':
				i = A_INT+1;
				break;
			case 'w': case 'W':
				i = A_WIS+1;
				break;
			case 'd': case 'D':
				i = A_DEX+1;
				break;
			case 'c': case 'C':
				i = A_CON+1;
				break;
			case 'h': case 'H':
				i = A_CHR+1;
				break;
			case 'r': case 'R':
				i = IDX_RACE;
				break;
			case 't': case 'T':
				i = IDX_TEMPLATE;
				break;
			case 'f': case 'F':
				i = IDX_FILE;
				break;
			case 'l': case 'L':
				i = IDX_LOAD;
				break;
			case 'z': case 'Z':
				i = IDX_RAND_ONE;
				break;
			case 'g': case 'G':
				i = IDX_SEX;
				break;
			case 'n': case 'N':
				i = IDX_NAME;
				break;
			case '/':
				i = IDX_DETAILS;
				break;
			case '=':
				i = IDX_OPTION;
				break;
			case '?':
				i = IDX_HELP;
				break;
			case 'X': /* Restart */
				return FALSE;
			case 'Q':
				quit(NULL);
				break;
			case ESCAPE:
				i = IDX_FINISH;
				break;
			case 'a': case 'A':
				i = IDX_ROLL;
				break;
			default:
				i = 0;
			}
		}

		/* Roll for stats, with minima if desired. */
		if (i == IDX_ROLL)
		{
			if (USE_AUTOROLLER)
				roll_stats_auto(TRUE);
			else
				get_stats();
			rolled = TRUE;
		}
		/* The "rolled" status is reset by most changes. */
		else if (i & (IDX_ALL | IDX_LOAD | IDX_STATS | IDX_SEX))
		{
			rolled = FALSE;
		}

		/* Don't finish on negative points */
		if (i == IDX_FINISH)
		{
			/* Rolled sets are always allowed. */
			if (rolled);

			/* Without spend_points, only rolled sets are allowed. */
			else if (!spend_points) i = 0;

			/* Unrolled totals with negative points are forbidden. */
			else if (points < 0) i = 0;
		}

		if (i == IDX_OPTION)
		{
			bc_type b = birth_option();
			if (b == BC_ABORT) i = IDX_START;
			else if (b == BC_RESTART) return FALSE;
		}
		if (i == IDX_HELP)
		{
			do_cmd_help(NULL);
		}
		/* Toggle details if required. */
		if (i == IDX_DETAILS)
		{
			details = !details;
		}
		/* Allow the player to choose a name, remember if he does. */
		if (i == IDX_NAME)
		{
			if (get_name()) own_name = TRUE;
		}
		/* Save current stats to a pref file */
		if (i == IDX_FILE)
		{
			char name[32];
			strcpy(name, player_name);
			if (get_string("Please choose a name: ", name, 32))
			{
				(void)add_stats(p_ptr->psex, p_ptr->prace, p_ptr->ptemplate,
					maximise_mode, p_ptr->stat_max, name);
				save_stats();
			}
		}

		/* Load saved stats at startup and on demand */
		if (i & IDX_LOAD)
		{
			if (load_stat_set(i == IDX_LOAD) == BC_RESTART) return FALSE;
		}

		/* Modify the player's race. */
		if (i == IDX_RACE)
		{
			MOD_ADD(p_ptr->prace, (ISLOWER(stat) ? 1 : -1), MAX_RACES);
			rp_ptr = &race_info[p_ptr->prace];
		}

		/* Modify the player's template. */
		if (i == IDX_TEMPLATE)
		{
			MOD_ADD(p_ptr->ptemplate, (ISLOWER(stat) ? 1 : -1), z_info->templates);
			cp_ptr = &template_info[p_ptr->ptemplate];
		}

		/* Modify the player's sex. */
		if (i == IDX_SEX)
		{
			MOD_ADD(p_ptr->psex, (ISLOWER(stat) ? 1 : -1), MAX_SEXES);
			sp_ptr = &sex_info[p_ptr->psex];
		}

		/* Modify a random stat. */
		if (i == IDX_RAND_ONE)
		{
			i = rand_int(A_MAX);
		}

		if (i & (IDX_ALL | IDX_LOAD))
		{
			/* Correct the skill set. These will be finalised later. */
			get_starting_skills();
			get_hermetic_skills_randomly();
			get_init_spirit(FALSE);
			get_random_skills(FALSE);
		}

		if (i & IDX_RACE)
		{
			/* Get an average age, height and weight. */
			get_ahw_average();
				/* Set the experience factor */
				p_ptr->expfact = rp_ptr->r_exp;
			/* Get an average social class. */
			p_ptr->sc = get_social_average(p_ptr->prace);
			/* Get a name if computer-generated. */
			if (!own_name) create_random_name(rp_ptr->name_syls, player_name);
		}
		if (i & IDX_TEMPLATE)
		{
			/* Get a default weapon */
			wield_weapons(TRUE);
		}

		/* Test for lower case (add to stat) or
		upper case (subtract stat) */
		if (i & IDX_STATS)
				{
			s16b dif = (ISLOWER(stat)) ? 1 : -1;
			s16b newstat = modify_stat_value(p_ptr->stat_cur[i-1], dif);
			p_ptr->stat_cur[i-1]=p_ptr->stat_max[i-1]=newstat;
				}


		/* Avoid inappropriate stats.
		 * In maximise mode, p_ptr->stat_cur[j] can always be as low as 8 and
		 * modify_stat_value(p_ptr->stat_cur[j],rp_ptr->r_adj[j]+cp_ptr->c_adj[j])
		 * can always be as low as 3. p_ptr->stat_cur[j] can be as high as 17.
		 *
		 * In non-maximise mode, p_ptr->stat_cur[j] can be as low as 3 and as
		 * high as maxstat(p_ptr->prace, p_ptr->ptemplate, j).
		 *
		 * This also sets the points total (setting them from scratch every
		 * time is simpler than adjusting them as necessary).
		 */
		if (i & (IDX_ALL | IDX_LOAD | IDX_STATS | IDX_SEX | IDX_ROLL))
		{
			byte j;

			/* A character with stats of 0 has 64 points. */
			points = 64;

			for (j = 0; j < A_MAX; j++)
			{
				if (maximise_mode)
				{
					byte min = MIN(8, modify_stat_value(3, -rp_ptr->r_adj[j]-cp_ptr->c_adj[j]));
					if (p_ptr->stat_cur[j] < min)
					{
						p_ptr->stat_cur[j] = p_ptr->stat_max[j] = min;
					}
					else if (p_ptr->stat_cur[j] > 17)
					{
						p_ptr->stat_cur[j] = p_ptr->stat_max[j] = 17;
					}
				}
				else
				{
					byte max = maxstat(p_ptr->prace, p_ptr->ptemplate, j);
					if (p_ptr->stat_cur[j] < 3)
					{
						p_ptr->stat_cur[j] = p_ptr->stat_max[j] = 3;
					}
					else if (p_ptr->stat_cur[j] > max)
					{
						p_ptr->stat_cur[j] = p_ptr->stat_max[j] = max;
					}
				}
				points += change_points_by_stat(0, p_ptr->stat_cur[j]);
			}
		}

		/* Update various things and display everything if something has changed. */
		if (i & (IDX_ALL | IDX_STATS | IDX_DETAILS | IDX_NAME | IDX_SEX |
			IDX_ROLL | IDX_OPTION))
		{
			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;
			p_ptr->cchi = p_ptr->mchi;

			/* Find the initial money. */
			get_money(FALSE);

			/* Display everything */
			display_player_birth(points, details, rolled);
		}

		/* Give a special mention to the help text at the start. */
		if (i == IDX_ALL)
		{
			/* Display the help text prominently.
			 * Hack - must match help.hlp. */
			put_str("Press ? and then b for detailed instructions.", 0, 0);
		}


		/* Randomise the skill bonuses and set magic up as the player
		 * wants. If the player aborts the latter, we return here. */
		if (i == IDX_FINISH)
		{
			bc_type b;
			get_starting_skills();
			if (((b = get_hermetic_skills())) == BC_ABORT) i = IDX_START;
			else if (b == BC_RESTART) return FALSE;
			else if (((b = get_init_spirit(TRUE))) == BC_ABORT) i = IDX_START;
			else if (b == BC_RESTART) return FALSE;
			/* else break; */ /* Redundant */
		}
	}

	/* Prepare the player for the dungeon. */
	wield_weapons(FALSE);
	get_random_skills(TRUE);
	get_final();

	/* Ensure that the player has a filename. */
	if (!own_name) process_player_name();

	/* Reset cheat_item. */
	cheat_item = FALSE;

	return TRUE;
}

/* Hack - special race for default stat set. */
#define RACE_NONE 255

/*
 * Load a set of stats.
 *
 * If menu is true, the player wants a choice.
 */
static bc_type load_stat_set_aux(bool menu, s16b *temp_stat_default)
{
	bc_type rc;
	int x;
	s16b y;

	/* Not allowed to do this. */
	if (!allow_pickstats) return BC_ABORT;

	/* Paranoia - there should be a default entry */
	if (!stat_default_total) return BC_ABORT;

	/* Find a set of stats which match the current race and template
	 * Templates with maximise set to DEFAULT_STATS are acceptable for
	 * all race/template combinations.
	 *
	 */
	for (x = 0, y = 0; x < stat_default_total; x++)
	{
		stat_default_type *sd_ptr = &stat_default[x];

		/* Don't load default stats without a race/template combination chosen. */
		if (x == DEFAULT_STATS && p_ptr->prace == RACE_NONE) continue;

		/* Hack - make the default stats take the current race, class and maximise values */
		if (x == DEFAULT_STATS)
		{
			sd_ptr->race = p_ptr->prace;
			sd_ptr->template = p_ptr->ptemplate;
			sd_ptr->maximise = maximise_mode;
		}

		/* Don't load stats with an incorrect maximise setting. */
		if (sd_ptr->maximise != maximise_mode) continue;

		/* Accept if the race, template are correct,
		 * or if no race has been specified. */
		if (p_ptr->prace == RACE_NONE ||
			(sd_ptr->race == p_ptr->prace &&
			sd_ptr->template == p_ptr->ptemplate))
		{
			temp_stat_default[y++] = x;
		}
	}

	/* Don't do anything without a choice. */
	if (!y) return BC_ABORT;

	/* Give the player the choices. */
	if (menu)
	{
		/* Start half-way down the screen if possible. */
		int start = 14, blank = 0;

		/* Allow more room if we need to display a race and template. */
		int width = (p_ptr->prace != RACE_NONE) ? 40 : 80;

		int minx = (Term->wid >= 82) ? 2 : 0;

		/* If not, start at the top. This should be enough... */
		if (y > (Term->hgt-17)*80/width) start = 2;

		/* If there's lots of space, leave a blank line. */
		if (y < (Term->hgt-17)*80/width) blank = 1;

		clear_from(start);
		for (x = 0; x < y; x++)
		{
			stat_default_type *sd_ptr = &stat_default[temp_stat_default[x]];
			char buf[120]="";
			int z;
			sprintf(buf, "%c) %s (", rtoa(x), quark_str(sd_ptr->name));

			/* If we're just starting, we need to know the race & template. */
			if (p_ptr->prace == RACE_NONE)
			{
				sprintf(buf+strlen(buf), "%s %s %s) (",
					sex_info[sd_ptr->sex].title, race_info[sd_ptr->race].title,
					tp_name+template_info[sd_ptr->template].name);
			}
			for (z = 0; z < A_MAX; z++)
			{
				int w = sd_ptr->stat[z];
				char stat[32];
				if (sd_ptr->maximise)
				{
					w=modify_stat_value(w, race_info[sd_ptr->race].r_adj[z]+template_info[sd_ptr->template].c_adj[z]);
				}
				strnfmt(stat, sizeof(stat), "%v", cnv_stat_f1, w);
				w = 0;
				while (stat[w] == ' ') w++;
				sprintf(buf+strlen(buf), "%s%s%s", (z) ? "," : "", stat+w, (z+1 < A_MAX) ? "" : ")");
			}

			/* Don't allow overly long entries. */
			buf[width] = '\0';

			if (width > 40)
			{
				put_str(buf, start+blank+2+x, minx);
			}
			else
			{
				put_str(buf, start+blank+2+x/2, minx + 40 * (x%2));
			}
		}
		prt("Stat templates provide a quick method of obtaining a specific character.", start-2, 5);
		prt("Selecting a character will set the sex, race, template and stats as shown.",  start-1, 5);

		/* Ask for a choice */
		rc = birth_choice(start+1, y, "Choose a stat template", &x, TRUE);

		/* Finally clean up. */
		clear_from(start);
	}
	/* We're starting for the first time, so give the player the last set saved. */
	else if (!p_ptr->stat_cur[0])
	{
		x = y-1;
		rc = BC_OKAY;
	}
	/* The player has already chosen stats, and hasn't asked to load new ones,
	 * so do nothing. */
	else
	{
		return BC_ABORT;
	}
	/* Something has been chosen, so copy everything across. */
	if (x != -1)
	{
		stat_default_type *sd_ptr = &stat_default[temp_stat_default[x]];
		if (p_ptr->prace == RACE_NONE)
		{
			p_ptr->psex = sd_ptr->sex;
			sp_ptr = &sex_info[sd_ptr->sex];

			p_ptr->prace = sd_ptr->race;
			rp_ptr = &race_info[sd_ptr->race];

			p_ptr->ptemplate = sd_ptr->template;
			cp_ptr = &template_info[sd_ptr->template];

			strncpy(player_name, quark_str(sd_ptr->name), 31);
		}
		for (y = 0; y < A_MAX; y++)
		{
			s16b tmp = sd_ptr->stat[y];
			p_ptr->stat_cur[y] = p_ptr->stat_max[y] = tmp;
		}
	}
	return rc;
}

/*
 * A wrapper around the above to handle dynamic allocation.
 */
static bc_type load_stat_set(bool menu)
{
	C_TNEW(temp_stat_default, stat_default_total+1, s16b);
	bc_type returncode = load_stat_set_aux(menu, temp_stat_default);
	TFREE(temp_stat_default);
	return returncode;
}

/*
 * Random Name Generator
 * based on a Javascript by Michael Hensley
 * "http://geocities.com/timessquare/castle/6274/"
 */
void create_random_name(cptr **syl, char *name)
{
	int i,j;

	/* Paranoia */
	if (!name) return;

	/* Start with no name. */
	*name = '\0';

	/* Each name is three syllables long. */
	for (i = 0; i < 3; i++)
	{
		/* Count the possibilities. */
		for (j = 0; syl[i][j]; j++);

		/* Choose one. */
		strcat(name, syl[i][rand_int(j)]);
	}
}

/*
 * Initialise the skills for a new character
 */
static void get_starting_skills(void)
{
	int i;
	/* Set default skills */
	for(i=0;i<MAX_SKILLS;i++)
	{
		/* Set value and base from template. */
		skill_set[i].base = skill_set[i].value = cp_ptr->skill[i];

		/* Use default EXP and ceiling. */
		skill_set[i].experience = 0;
		skill_set[i].ceiling = 100;
	}

	/* Wipe spirit associations */
	for (i=0;i<MAX_SPIRITS;i++)
	{
		spirits[i].pact = 0;
	}

	/*
	 * add basic everyman values (mainly from race)
	 */
	skill_set[SKILL_TOUGH].value+=4;
	skill_set[SKILL_DEVICE].value+=rp_ptr->device_bonus;
	skill_set[SKILL_DISARM].value+=rp_ptr->disarm_bonus;
	skill_set[SKILL_CLOSE].value+=rp_ptr->melee_bonus;
	skill_set[SKILL_STAB].value+=rp_ptr->melee_bonus;
	skill_set[SKILL_SLASH].value+=rp_ptr->melee_bonus;
	skill_set[SKILL_CRUSH].value+=rp_ptr->melee_bonus;
	skill_set[SKILL_MISSILE].value+=rp_ptr->missile_bonus;
	skill_set[SKILL_PERCEPTION].value+=rp_ptr->perception_bonus;
	skill_set[SKILL_SAVE].value+=rp_ptr->save_bonus;
	skill_set[SKILL_SEARCH].value+=rp_ptr->search_bonus;
	skill_set[SKILL_STEALTH].value+=rp_ptr->stealth_bonus;


}

static name_entry magic_skills[] =
{
	{SKILL_THAUMATURGY, NULL},
	{SKILL_NECROMANCY, NULL},
	{SKILL_SORCERY, NULL},
	{SKILL_CONJURATION, NULL},
	{SKILL_ANIMAE, NULL},
	{SKILL_CORPORIS, NULL},
	{SKILL_VIS, NULL},
	{SKILL_NATURAE, NULL},
};

/*
 * Get hermetic skills randomly
 */
static void get_hermetic_skills_randomly()
{
	int i,choices;
	int choice,old_choice;

	choices = cp_ptr->choices;

	if (!choices) return;

	for(old_choice = -1, i=0; i<choices; i++)
	{
		if(i && !one_in(3)) choice = old_choice;
		else choice = rand_int(N_ELEMENTS(magic_skills));

		skill_set[magic_skills[choice].idx].value += 5;
		old_choice = choice;
	}
}

/*
 * Get hermetic skills
 */
static bc_type get_hermetic_skills()
{
	char buf[40];
	bc_type b;
	int i, k;
	name_entry *this;

	if (!cp_ptr->choices) return BC_OKAY;

	/* Extra info */
	clear_from(15);
	mc_roff_xy(5, 15,
		"Please select a school or type of hermetic magic to specialise\n"
		"in. Thaumaturgy school is the most offensive, Sorcery school is\n"
		"the least offensive. Naturae spells deal with matter, Corporis\n"
		"with flesh, Animae with spirits and Vis with energy. Each school\n"
		"has spells of all four types.");

	FOR_ALL_IN(magic_skills, this)
	{
		if (!this->str) this->str = skill_set[this->idx].name;
	}

	/* Display the options. */
	display_entry_list_bounded(magic_skills, N_ELEMENTS(magic_skills), TRUE,
		0, 22, 80, 23);

	for(i= cp_ptr->choices; i > 0; i--)
	{
		/* Get a choice */
		sprintf(buf, "%d choice%s left. Choose a school or type",
			i, (i > 1 ? "s" : ""));

		b = birth_choice(21, MAX_SCHOOL*2, buf, &k, TRUE);
		if (b) return b;

		skill_set[magic_skills[k].idx].value += 5;
	}
	return BC_OKAY;
}

/*
 * Save the current data for later
 */
static void save_prev_data(birther *prev_stat)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	prev_stat->age = p_ptr->age;
	prev_stat->wt = p_ptr->wt;
	prev_stat->ht = p_ptr->ht;
	prev_stat->sc = p_ptr->sc;
	prev_stat->au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		prev_stat->stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev_stat->history[i], history[i]);
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(birther *prev_stat)
{
	int        i;

	birther temp;


	/*** Save the current data ***/

	save_prev_data(&temp);


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = prev_stat->age;
	p_ptr->wt = prev_stat->wt;
	p_ptr->ht = prev_stat->ht;
	p_ptr->sc = prev_stat->sc;
	p_ptr->au = prev_stat->au;

	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_max[i] = prev_stat->stat[i];
		p_ptr->stat_cur[i] = prev_stat->stat[i];
	}

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], prev_stat->history[i]);
	}


	/*** Save the current data ***/

	/* Save the data */
	COPY(prev_stat, &temp, birther);
}




/*
 * Returns adjusted stat -JK-  Algorithm by -JWT-
 *
 * auto_roll is boolean and states maximum changes should be used rather
 * than random ones to allow specification of higher values to wait for
 *
 * The "maximise_mode" code is important -BEN-
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
			else if (maximise_mode)
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
	int i, j;

	int bonus;

	int dice[18];

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
		if ((j > 42) && (j < 57)) break; /* 57 was 54...
					I hate 'magic numbers' :< TY */
	}

	/* Acquire the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "template" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Variable stat maxes */
		if (maximise_mode)
		{
			/* Start fully healed */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/template bonuses */
			p_ptr->stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
			p_ptr->stat_use[i] = adjust_stat(p_ptr->stat_max[i], (s16b)bonus, FALSE);

			/* Save the resulting stat maximum */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = p_ptr->stat_use[i];
		}
	}
}


/*
 * Helper function for get_social_average()
 * This finds the the average social of a race for which rp_ptr->chart = chart.
 * Total is the size of the array of charts.
 * Oldseen is an array which indicates how often each chart has been used in the
 * path leading to the current one, generally 0 or 1.
 */
static s16b get_social_average_aux(byte *oldseen, int chart, int total)
{
	/* We stop at nothing. */
	if (chart == 0) return 0;

	/* If we've already used this chart, we must avoid being drawn
	 * into an infinite loop. We should probably find out the length
	 * of the loop and sum it to infinity, but the first 10 iterations
	 * should give a very good approximation.
	 */
	else if (oldseen[chart] > 10) return 0;
	else
	{
		s16b i;
		C_TNEW(seen, total, byte);
		s16b social_class = 0;
		byte roll = 0, droll = 0, dnext = 0;

		/* Copy the chart across. */
		for (i = 0; i < total; i++)
		{
			seen[i]=oldseen[i];
		}

		/* We have now seen this chart (again). */
		seen[chart]++;

		/* Get the next charts. */
		for (i=0; bg[i].chart; i++)
		{
			/* Only relevant charts count. */
			if (chart != bg[i].chart) continue;

			/* Only increasing charts count. */
			if (bg[i].roll < roll) continue;

			/* Notice the bonus from this chart. */
			social_class += (bg[i].bonus - 50) * (bg[i].roll - roll);

			/* If this is the first appropriate chart, notice it. */
			if (!dnext)
			{
				dnext = bg[i].next;
			}
			/* If we're changing to a different chart, get the next one. */
			else if (dnext != bg[i].next)
			{
				/* Get the next chart. */
				social_class += get_social_average_aux(seen, dnext, total)*(roll-droll)/100;
				droll = roll;
			}

			/* Move the roll counter along. */
			roll = bg[i].roll;
		}

		/* Add the last set of charts. */
		social_class += get_social_average_aux(seen, dnext, total)*(roll-droll)/100;

		TFREE(seen);

		/* And return the result (multiplied by 100 to minimise rounding errors). */
		return social_class;
	}
}

/*
 * Finds the average social class for a given race.
 */
static s16b get_social_average(byte race)
{
	byte chart = race_info[race].chart;
	byte total = chart;

	/* We're dealing in hundredths here, so this is the average of
	 * randint(4).
	 */
	s16b i,social_class = 250;

	/* Count charts */
	for (i = 0; bg[i].chart; i++)
	{
		if (bg[i].chart > total) total = bg[i].chart;
	}

	if (total++)
	{
		/* Create an array of charts which have formed part of the
		 * current branch. This is needed in order to recognise when
		 * a recursion has occurred.
		 */
		C_TNEW(seen, total, byte);

		for (i = 0; i < total; i++) seen[i] = 0;

		social_class += get_social_average_aux(seen, chart, total);

		TFREE(seen);
	}

	/* Get a social class of the expected order of magnitude. */
	social_class /= 100;

	/* Social classes must be in the range [1,100] */
	social_class = MIN(MAX(1,social_class),100);

	/* Return an average social class for the race. */
	return social_class;
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
	chart = race_info[p_ptr->prace].chart;

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
 * Computes the average age, height and weight of the current race
 * Must set everything needed on the point buying screen.
 */
static void get_ahw_average(void)
{
	p_ptr->age = rp_ptr->b_age + (rp_ptr->m_age + 1) / 2;
	/* i.e. 1st January */
	p_ptr->birthday = 0; p_ptr->startdate = 0;
	/* Mean figures for weight and height. */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = rp_ptr->m_b_ht;
		p_ptr->wt = rp_ptr->m_b_wt;
	}
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = rp_ptr->f_b_ht;
		p_ptr->wt = rp_ptr->f_b_wt;
	}
}

/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);
	p_ptr->birthday=rand_int(1481);
	/* This isn't stored in the birther struct as it isn't important yet...*/
	p_ptr->startdate=rand_int(1481);

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
static void get_money(bool random)
{
	int        i, gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + (random ? randint(100) : 51) + 300;

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (p_ptr->stat_use[i] >= 18+50) gold -= 300;
		else if (p_ptr->stat_use[i] >= 18+20) gold -= 200;
		else if (p_ptr->stat_use[i] > 18) gold -= 150;
		else gold -= (p_ptr->stat_use[i] - 8) * 10;
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
static void birth_put_stats(s32b *stat_match, s32b auto_round)
{
	int i;

	/* Put the stats (and percents) */
	for (i = 0; i < A_MAX; i++)
	{
		/* Put the stat */
		mc_put_fmt(2+i, 66, "$G%-7v", cnv_stat_f1, p_ptr->stat_use[i]);

		/* Put the percent */
		if (stat_match[i])
		{
			int p = 1000L * stat_match[i] / auto_round;
			char attr = (p < 100) ? 'y' : 'G';
			mc_put_fmt(2+i, 73, "$%c%3d.%d%% ", attr, p/10, p%10);
		}

		/* Never happened */
		else
		{
			mc_put_fmt(2+i, 73, "$r(NONE) ");
		}
	}
}

/*
 * Get number of quest monsters for quest i
 * Heino Vander Sanden
 */
static int get_number_monster(int i)
{
	int num;

	if (r_info[q_list[i].r_idx].flags1 & (RF1_UNIQUE))
		return (1);
	else
	{
		if (r_info[q_list[i].r_idx].flags1 & (RF1_FRIENDS))
			num = 10;
		else
			num = 5;

		num += rand_range(1, (q_list[i].level / 3) + 5);
		return (num);
	}
}

/*
 * Get random monster
 * Heino Vander Sanden
 */
static int get_rnd_q_monster(int q_idx)
{
	int r_idx,j;
	s16b q_ranges[] =
	{
		MON_HELLBAT,
		MON_QUASIT,
		MON_COLD_VORTEX,
		MON_HELLBLADE,
		MON_NEXUS_QUYLTHULG,
		MON_ACIDIC_CYTOPLASM,
		MON_TIME_VORTEX,
		MON_MASTER_LICH,
		MON_DEMONIC_QUYLTHULG,
		MON_BAST_GODDESS_OF_CATS,
		MON_MEPHISTOPHELES_LORD_OF_HELL /* Not eligible for quests. */
	};

	/* Pick a random monster in one of the above ranges. */
	r_idx = rand_int(10);
	r_idx = rand_range(q_ranges[r_idx], q_ranges[r_idx+1]-1);

	/* Don't multipliers to be random guardians */
	if (r_info[r_idx].flags2 & (RF2_MULTIPLY)) return (0);

	/* Paranoia? - Don't allow uncreatable guardians. */
	if (!r_info[r_idx].rarity) return (0);

	/* Don't allow duplicate guardians */
	for (j = 2; j < q_idx; j++)
	{
		if (q_list[j].r_idx == r_idx) return (0);
	}

	return (r_idx);
}

/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;
	option_type *op_ptr;


	/* Hack -- zero the struct */
	WIPE(p_ptr, player_type);

	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], "");
	}


	/* No weight */
	total_weight = 0;

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

	/* Reset the player's race. */
	p_ptr->prace = RACE_NONE;

	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

	/* Hack - ready to move. */
	p_ptr->energy = 1050;

	/* Player has no recal ritual yet */
	p_ptr->ritual = TOWN_NONE;

	/* Player has no house yet */
	for(i=0;i<MAX_STORES_TOTAL;i++)
	{
		store[i].bought = FALSE;
	}

	/* Wipe the spells */
	for (i=0;i<MAX_SCHOOL*MAX_SPELLS_PER_BOOK;i++)
	{
		magic_type *s_ptr = num_to_spell(i);
		if (s_ptr) s_ptr->flags = 0;
	}
	for (i = 0; i < 128; i++)
	{
		spell_order[i] = 255;
	}

	/* Clear "cheat" options */
	for (op_ptr = option_info; op_ptr->o_desc; op_ptr++)
	{
		if (op_ptr->o_page == OPTS_CHEAT)
		{
			*(op_ptr->o_var) = FALSE;
		}
	}

	/* Assume no winning game */
	total_winner = FALSE;

	/* Assume no cheating */
	noscore = 0;
}

/*
 * Create an object from a template for the player's initial inventory.
 */
static void make_birth_item(make_item_type *i_ptr, cptr names)
{
	object_type o_ptr[1];

	/* No such item. */
	if (!i_ptr->k_idx) return;

	/* Actually create the item. */
	make_item(o_ptr, i_ptr, names, FOUND_BIRTH, 0);

	/* Hack - torches have a special pval. */
	if (o_ptr->k_idx == OBJ_WOODEN_TORCH) o_ptr->pval = rand_range(3, 7) * 500;

	/* These objects are "storebought" */
	o_ptr->ident |= IDENT_STOREB;

	/* They are known. */
	object_aware(o_ptr);
	object_known(o_ptr);

	/* They have been touched. */
	object_touch(o_ptr);

	/* And they are in the player's inventory. */
	inven_carry(o_ptr);
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i;
	u32b f[3];

	/* For characters starting with magical skill, give them a spellbook */
	if (skill_set[SKILL_MANA].value > 0)
	{
		object_type q_ptr[1];

		int gbook[4], book[4] =
		{
			OBJ_SORCERY_BEGINNERS_HANDBOOK,
			OBJ_THAUMATURGY_SIGN_OF_CHAOS,
			OBJ_CONJURATION_MINOR_CONJURINGS,
			OBJ_NECROMANCY_BLACK_PRAYERS
		};

		i = 0;
		/* Allow books the player has some skill with. */
		if (skill_set[SKILL_SORCERY].value > 0) gbook[i++] = book[0];
		if (skill_set[SKILL_THAUMATURGY].value > 0) gbook[i++] = book[1];
		if (skill_set[SKILL_CONJURATION].value > 0) gbook[i++] = book[2];
		if (skill_set[SKILL_NECROMANCY].value > 0) gbook[i++] = book[3];

		/* No good magic schools?! Allow all books. */
		if (!i)
		{
			for (i = 0; i < 4; i++) gbook[i] = book[i];
		}

		/* Create a random allowed book and give it to the player. */
		object_prep(q_ptr, gbook[rand_int(i)]);
		q_ptr->number = 1;
		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr);
	}

	/* Give the player some basic things. */
	for (i = 0; i < MAX_RACE_ITEMS; i++)
	{
		/* Hack - don't support named racial equipment... */
		make_birth_item(&rp_ptr->items[i], NULL);
	}

	/* Find out about the player. */
	player_flags(f, f+1, f+2);

	/* Give the player three useful objects */
	for (i = 0; i < MAX_TPL_ITEMS; i++)
	{
		/* Look up standard equipment */
		make_item_type *i_ptr = &cp_ptr->items[i];
		object_kind *k_ptr = &k_info[i_ptr->k_idx];

		/* Hack - avoid rings which duplicate the player's powers. */
		if (k_ptr->flags1 & f[0] || k_ptr->flags2 & f[1] ||
			k_ptr->flags3 & f[2])
		{
			/* Give the player an alternative item, if available. */
			if (i_ptr[MAX_TPL_ITEMS].k_idx) i_ptr += MAX_TPL_ITEMS;
		}

		/* Give the player the object. */
		make_birth_item(i_ptr, tp_name);
	}
}

/*
 * Generate the additional quests
 * Heino Vander Sanden, Jimmy De Laet, and Robert Ruehlmann
 */
static void player_birth_quests(void)
{
	int i,j,k;
	int q_max = z_info->quests;
	quest_type *q_list_tmp;

	/* Add an extra 11-30 random quests. */
	q_max = z_info->quests+rand_range(11,30);
	C_MAKE(q_list_tmp, q_max, quest_type);
	C_COPY(q_list_tmp, q_list, z_info->quests, quest_type);
	KILL(q_list);
	q_list = q_list_tmp;

	/* Generate to MAX_Q_IDX with random quests */
	for (i = z_info->quests; i<q_max; i++)
	{
		/* Paranoia - quests should not be hard to find. */
		for (k = 0; k < 10000; k++)
		{
			/* Get a random monster */
			do
			{
				q_list[i].r_idx = get_rnd_q_monster(i);
			}
			while (!q_list[i].r_idx);

			/* Set the quest level to the level of the monster */
			q_list[i].level = r_info[q_list[i].r_idx].level;

			/* Quest monster at least 2 levels out of depth */
			q_list[i].level -= rand_range(2, 3+(q_list[i].level / 6));

			/* No 2 quests on the same level (?) */
			for (j = 0; j<i; j++)
			{
				if (q_list[i].level == q_list[j].level) break;
			}
			/* Unique quest level. */
			if (j == i) break;
		}

		/* Make sure uniques aren't created outside their level */
		if (r_info[q_list[i].r_idx].flags1 & RF1_UNIQUE) r_info[q_list[i].r_idx].flags1 |= RF1_GUARDIAN;

		/* Now place quest in a random dungeon
		 * but not on its lowest two levels as they *may*
		 * contain 'hard-coded' quests.
		 */
		do
		{
			j=rand_range(1,MAX_CAVES)-1;
		}
		while((q_list[i].level <= dun_defs[j].offset) ||
			(q_list[i].level >= dun_defs[j].max_level + dun_defs[j].offset));

		q_list[i].dungeon = j;
		q_list[i].max_num = get_number_monster(i);
	}

	/* Set the new quest depths correctly now, as some of the above relies
	 * on q_list[i].level including the offset.
	 */
	for (i = z_info->quests; i < q_max; i++)
	{
		q_list[i].level -= dun_defs[q_list[i].dungeon].offset;
	}

	/* Remember the new number of quests. */
	z_info->quests = q_max;
}


/*
 * Finish off generation by adding a few random things.
 */
static void get_final(void)
{
	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp;

	/* Calculate hit points. */
	do_cmd_rerate();

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

	/* Roll for gold */
	get_money(TRUE);

	/* Hack -- get a chaos patron even if you are not chaotic (yet...) */
	p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;

	/* Calculate the bonuses. */
	p_ptr->update |= (PU_BONUS);

	/* Update stuff */
	update_stuff();

	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;
	p_ptr->cchi = p_ptr->mchi;
}

static void roll_stats_auto(bool point_mod)
{
	/* Access via point mod mode needs a more careful layout. */
	const int x = (point_mod) ? 73 : 73;
	const int y = (point_mod) ? 1 : 9;
	s16b stat_limit[A_MAX];
	int i;
	s32b auto_round = 0L, last_round = 0L;
	s32b stat_match[A_MAX];

	WIPE(stat_match, stat_match);
	for (i = 0; i < A_MAX; i++)
	{
		if (maximise_mode)
		{
			int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
			stat_limit[i] = adjust_stat(p_ptr->stat_max[i], bonus, FALSE);
		}
		else
		{
			stat_limit[i] = p_ptr->stat_max[i];
		}
	}

	/* Auto-roll */
	while (1)
	{
		bool accept = TRUE;
		bool flag = FALSE;

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
			if (p_ptr->stat_use[i] >= stat_limit[i])
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

		/* Take note every 25 rolls */
		flag = (!(auto_round % 25L));

		/* Update display occasionally */
		if (flag || (auto_round < last_round + 100))
		{
			/* Dump data */
			birth_put_stats(stat_match, auto_round);
			/* Dump round if allowed. */
			mc_put_fmt(y, x, "%6ld", auto_round);
			/* Make sure they see everything */
			Term_fresh();
			/* Delay 1/10 second */
			if (flag) Term_xtra(TERM_XTRA_DELAY, 100);
			/* Do not wait for a key */
			inkey_scan = TRUE;
			/* Check for a keypress */
			if (inkey()) break;
		}
	}
}


static bool quick_start_character(void)
{
	int mode = 0;
	bool prev = FALSE;

	char UNREAD(c);

	birther prev_stat[1];



	/*** Player sex ***/
	/* Set sex */
	p_ptr->psex =(char)rand_range(0,1);
	sp_ptr = &sex_info[p_ptr->psex];

	process_pref_file("qstart.prf");

	set_gnext("*");
	if (load_stat_set(TRUE)) return FALSE;

	/* Get a random name */
	create_random_name(rp_ptr->name_syls, player_name);

	/* Get skill values */
	get_starting_skills();
	get_hermetic_skills_randomly();
	get_init_spirit(FALSE);
	get_random_skills(TRUE);

	/* Some stuff which is normally handled in point_mod_player(). */
	p_ptr->expfact = rp_ptr->r_exp;

	/* Clean up */
	clear_from(0);

	/* Display */
	mc_put_fmt(2, 1, "Name        : $B$!%s", player_name);
	mc_put_fmt(3, 1, "Sex         : $B$!%s", sp_ptr->title);
	mc_put_fmt(4, 1, "Race        : $B$!%s", rp_ptr->title);
	mc_put_fmt(5, 1, "Template    : $B$!%s", tp_name+cp_ptr->name);
	mc_put_fmt(6, 1, "Exp. factor : $B%ld", p_ptr->expfact);

	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		if (USE_AUTOROLLER)
		{
			roll_stats_auto(FALSE);
		}
		else
		{
			/* Get a new character. */
			get_stats();
		}

		/* Flush input */
		flush();


		/*** Display ***/

		/* Mode */
		mode = DPLAY_PLAYER;

		/* Roll for various things. */
		get_final();

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
			p_ptr->cchi = p_ptr->mchi;

			/* Display the player */
			display_player(mode);

			/* Prepare a prompt (must squeeze everything in) */
			mc_put_fmt(23, 2,
				"['r' to reroll, %s'/' to change display, or ESC to accept]",
				prev ? "'p' for prev, " : "");

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
				load_prev_data(prev_stat);
				continue;
			}

			/* Toggle the display */
			if (c == '/')
			{
				mode = ((mode != DPLAY_PLAYER) ? DPLAY_PLAYER : DPLAY_BIRTH);
				continue;
			}

			/* Help */
			if (c == '?')
			{
				do_cmd_help(NULL);
				continue;
			}

			/* Warning */
			bell(0);
		}

		/* Are we done? */
		if (c == ESCAPE) break;

		/* Save this for the "previous" character */
		save_prev_data(prev_stat);

		/* Note that a previous roll exists */
		prev = TRUE;
	}

	/* Clear prompt */
	clear_from(23);


	/*** Finish up ***/

	/* Allow name to be edited, recolor it, prepare savefile */
	get_name();

	/* Roll for various things (again). */
	get_final();

	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

	/* Get keys until successful. */
	while (1) switch (inkey())
	{
		/* Quit */
		case 'Q': quit(NULL);

		/* Start over */
		case 'S': return FALSE;

		/* Accept */
		case ESCAPE: return TRUE;
	}
}

/*
 * Ask if the player wishes to use quick start, if allowed.
 */
static bc_type ask_quick_start(void)
{
	if (!allow_quickstart) return BC_ABORT;

	mc_roff_xy(5, 12,
		"Quick-Start gives you a completely random character without\n"
		"further prompting.");

	while (1)
	{
		put_str("Quick-Start? (y/n): ", 15, 2);
		switch (inkey())
		{
			case 'Q': quit(NULL);
			case 'S': return BC_RESTART;
			case 'y': case 'Y': return BC_OKAY;
			case 'n': case 'N': return BC_ABORT;
			case '?': do_cmd_help(NULL); break;
			case '=':
				Term_save();
				do_cmd_options_aux(7, "Startup Options", NULL);
				if (!allow_quickstart) return BC_ABORT;
				Term_load();
				break;
			default:
				bell(0);
		}
	}
}

/*
 * A nasty macro to choose and print the choice for sex, race and template.
 * It needs to be a macro because they use different types for ARRAY and
 * XP_PTR which only happen to be equivalent for these purposes.
 */
#define CHOOSE_SRT(ARRAY, MAX, NAME, P_VAR, XP_PTR, Y, MSG) \
{ \
	int k; \
	name_entry list[MAX], *this; \
\
	/* Make some space. */ \
	clear_from(12); \
\
	/* Extra info */ \
	mc_roff_xy(5, 12, MSG); \
\
	FOR_ALL_IN(list, this) \
	{ \
		this->idx = this - list; \
		this->str = ARRAY[this->idx].title; \
	} \
\
	/* Show the options. */ \
	display_entry_list_bounded(list, MAX, TRUE, 0, 17, 80, 22); \
\
	/* Choose. */ \
	if (birth_choice(15, MAX, "Choose a " NAME, &k, FALSE) == BC_RESTART) \
		return FALSE; \
\
	/* Set it. */ \
	p_ptr->P_VAR = k; \
	XP_PTR = &ARRAY[k]; \
\
	/* Display */ \
	mc_put_fmt(Y, 15, "$B%s", XP_PTR->title); \
	return TRUE; \
}

/*
 * Let the player choose a sex, and display it if chosen.
 */
static bool choose_sex(void)
{
	CHOOSE_SRT(sex_info, MAX_SEXES, "sex", psex, sp_ptr, 3,
		"Your 'sex' does not have any significant gameplay effects.");
}

/*
 * Let the player choose a race, and display it if chosen.
 */
static bool choose_race(void)
{
	CHOOSE_SRT(race_info, MAX_RACES, "race", prace, rp_ptr, 4,
		"Your 'race' determines various intrinsic factors and bonuses.");
}

/*
 * Let the player choose a template, and display it if chosen.
 */
static bool choose_template(void)
{
	int k = 0;
	name_entry *this;

	C_TNEW(list, z_info->templates, name_entry);

	/* Make some space. */
	clear_from(12);

	/* Extra info */
	mc_roff_xy(5, 12, "Your 'template' determines various starting abilities and bonuses.");

	for (k = 0, this = list; k < z_info->templates; k++)
	{
		/* Not a real template. */
		if (!template_info[k].name) continue;

		/* Accept and advance. */
		this->idx = k;
		this->str = tp_name+template_info[k].name;
		this++;
	}

	/* Show the options. */
	display_entry_list_bounded(list, this - list, TRUE, 0, 17, 80, 22);

	/* Choose. */
	if (birth_choice(15, this - list, "Choose a template", &k, FALSE)
		== BC_RESTART) return FALSE;

	/* Set it. */
	p_ptr->ptemplate = list[k].idx;
	cp_ptr = &template_info[p_ptr->ptemplate];

	/* Display */
	mc_put_fmt(5, 15, "$B%s", tp_name+cp_ptr->name);
	return TRUE;
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
	bc_type b;

	/*** Intro ***/

	/* Clear any pending messages. */
	msg_print(NULL);

	/* Clear screen */
	Term_clear();

	/* Title everything */
	put_str("Name        :", 2, 1);
	put_str("Sex         :", 3, 1);
	put_str("Race        :", 4, 1);
	put_str("Template    :", 5, 1);

	/*** Instructions ***/

	/* Display some helpful information */
	mc_roff_xy(5, 7,
	"Please answer the following questions.  Most of the questions display\n"
	"a set of standard answers, and many will also accept special responses,\n"
	"including 'Q' to quit, '=' to change options, 'S' to restart character\n"
	"creation and '?' for help.  Note that 'Q' and 'S' must be capitalized.");


	/* Ask about quick_start, if allowed. */
	b = ask_quick_start();

	/* Remove the prompt. */
	clear_from(12);

	/* React to the prompt. */
	if (b == BC_RESTART)
		return FALSE;
	else if (b == BC_OKAY)
		return quick_start_character();

	/* Interactive character */

	/* Choose sex, race and template (and possibly some stats). */

	b = load_stat_set(TRUE);

	if (b == BC_RESTART)
	{
		return FALSE;
	}
	else if (b == BC_ABORT)
	{
		if (!choose_sex()) return FALSE;
		if (!choose_race()) return FALSE;
		if (!choose_template()) return FALSE;
	}

	/* Generate the character. */
	return point_mod_player();
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

	/* Generate quests */
	player_birth_quests();

	/* Shops */
	for (n = 0; n < MAX_STORES_TOTAL; n++)
	{
		/* Initialize */
		store_init(n);

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(n);
	}
}



