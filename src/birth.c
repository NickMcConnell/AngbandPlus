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
	{"You are the illegitimate and unacknowledged child ",	 10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ",	 20, 1, 2, 35},
    {"You are one of several children ",             95, 1, 2, 45},
	{"You are the first child ",				100, 1, 2, 50},

	{"of a Serf.  ",						 40, 2, 3, 65},
	{"of a Yeoman.  ",						 65, 2, 3, 80},
	{"of a Townsman.  ",					 80, 2, 3, 90},
	{"of a Guildsman.  ",					 90, 2, 3, 105},
	{"of a Landed Knight.  ",					 96, 2, 3, 120},
    {"of a Noble Family.  ",    99, 2, 3, 130},
    {"of the Royal Blood Line.  ",                100, 2, 3, 140},

	{"You are the black sheep of the family.  ",		 20, 3, 50, 20},
	{"You are a credit to the family.  ",			 80, 3, 50, 55},
	{"You are a well liked child.  ",				100, 3, 50, 60},

	{"Your mother was of the Teleri.  ",			 40, 4, 1, 50},
	{"Your father was of the Teleri.  ",			 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ",		 	 90, 4, 1, 55},
	{"Your father was of the Noldor.  ",		 	 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ",			 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ",			100, 4, 1, 70},

	{"You are one of several children ",			 60, 7, 8, 50},
	{"You are the only child ",					100, 7, 8, 55},

	{"of a Teleri ",						 75, 8, 9, 50},
	{"of a Noldor ",						 95, 8, 9, 55},
	{"of a Vanyar ",						100, 8, 9, 60},

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
    {"Mage.  ", 85, 90, 91, 70 },
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

/*
 * Name segments for random player names
 */

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

/* Klackon */
static char *klackon_syllable1[] =
{
	"K'", "K", "Kri", "Kir", "Kiri", "Iriki", "Irik", "Karik", "Iri","Akri",
};

static char *klackon_syllable2[] =
{
	"arak", "i", "iri", "ikki", "ki", "kiri","ikir","irak","arik","k'","r",
};

static char *klackon_syllable3[] =
{
	"akkak", "ak", "ik", "ikkik", "irik", "arik", "kidik", "kii", "k","ki","riki","irk",
};

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

/* Allow player to modify the character by spending points */
static bool point_mod_player(void)
{
	char b1 = '[';
	char b2 = ']';
	char stat;
	char modpts[4] = "none";
	char *buf = "zero";
	int tmp = 0;
	int hp = 0;
	int addhp = 0; 
	int x = 0;
	int i, points;
 
 
	points = 34;
	sprintf(modpts,"%d",points);
	clear_from(23);
	while(1)
	{ 
		/* reset variable */
		i = 0; 

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
		display_player(0);

		/* Display Stat Menu */
		clear_from(23);
		sprintf(modpts,"%d",points);
		
		Term_putstr(73,2,-1,TERM_WHITE,"<-S/s->");
		Term_putstr(73,3,-1,TERM_WHITE,"<-I/i->");
		Term_putstr(73,4,-1,TERM_WHITE,"<-W/w->");
		Term_putstr(73,5,-1,TERM_WHITE,"<-D/d->");
		Term_putstr(73,6,-1,TERM_WHITE,"<-C/c->");
		Term_putstr(73,7,-1,TERM_WHITE,"<-H/h->");

		Term_gotoxy(2, 23);
		Term_addch(TERM_WHITE, b1);
		if(points == 0)
		{
			Term_addstr(-1, TERM_GREEN, modpts);
		}
		else if(points > 0)
		{
			Term_addstr(-1, TERM_YELLOW, modpts);
		}
		else
		{
			Term_addstr(-1, TERM_RED, modpts);
		}
		Term_addstr(-1, TERM_WHITE, " points left. Press 'ESC' whilst on 0 points to finish.");
		Term_addch(TERM_WHITE, b2);
		/* Get an entry */
		stat = inkey();

		/* ESC goes back to previous menu */
		if((stat == ESCAPE) && (points == 0)) break;

		/* Assign values to entries, stats 0 to 5 */
		switch(stat)
		{

			/* The index to a specific stat is retrieved */
		case 's':
			i = 1;
			break;
		case 'S':
			i = 1;
			break;
		case 'i':
			i = 2;
			break;
		case 'I':
			i = 2;
			break;
		case 'w':
			i = 3;
			break;
		case 'W':
			i = 3;
			break;
		case 'd':
			i = 4;
			break;
		case 'D':
			i = 4;
			break;
		case 'c':
			i = 5;
			break;
		case 'C':
			i = 5;
			break;
		case 'h':
			i = 6;
			break;
		case 'H':
			i = 6;
			break;
		default:
			i = 0;
		}  

		/* Test for invalid key */
		if(!i) continue;
		i--;

		/* Test for lower case (add to stat) or 
		upper case (subtract stat) */
		if(islower(stat)) /* ('a' < stat) */
		{
			/* different conditions for maximize on */
			if(maximise_mode) 
			{
				/* Max stat increase */
				if(p_ptr->stat_max[i] < 17)
				{
					p_ptr->stat_cur[i] = ++p_ptr->stat_max[i];
				}
				else
				{
					continue;
				}
			}
			else
			{
				/* Max stat increase, maximize off */
				x = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
				if(x > 8) x = 8;
				if(x > 0) x *= 13;
				if(p_ptr->stat_max[i] < 18 + x)    
				{
					if(p_ptr->stat_max[i]> 17)
					{
						p_ptr->stat_max[i] += 10;
						p_ptr->stat_cur[i] += 10;
					}
					else
					{
						p_ptr->stat_cur[i] = ++p_ptr->stat_max[i];
					}
				}
				else
				{
					continue;
				}
			}

			/* Higher stats linearly cost more */
			if(p_ptr->stat_max[i] > 97) points--; 
			if(p_ptr->stat_max[i] > 67) points--;
			if(p_ptr->stat_max[i] > 18) points--;
			if(p_ptr->stat_max[i] > 14) points--;
			if(p_ptr->stat_max[i] > 3) points--;
			continue;
		}
		else    /* Reduce stat case */
		{ 
			if(p_ptr->stat_use[i] > 3)
			{
				if(p_ptr->stat_max[i] > 27)
				{ 
					p_ptr->stat_max[i] -= 10;
					p_ptr->stat_cur[i] -= 10;
				}
				else
				{
					p_ptr->stat_cur[i] = --p_ptr->stat_max[i];
				}
			}
			else
			{
				continue;
			}
			/* Higher stats yield more mod points */
			if(p_ptr->stat_max[i] > 87) points++; 
			if(p_ptr->stat_max[i] > 57) points++;
			if(p_ptr->stat_max[i] > 17) points++;
			if(p_ptr->stat_max[i] > 13) points++;
			if(p_ptr->stat_max[i] > 2) points++;
			continue;
		}
	}
	return TRUE;
}

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
	case RACE_CYCLOPS:
	case RACE_DWARF:
	case RACE_HALF_GIANT:
	case RACE_GOLEM:
	case RACE_NIBELUNG:
		strcpy(name, dwarf_syllable1[rand_int(sizeof(dwarf_syllable1) / sizeof(char*))]);
		strcat(name, dwarf_syllable2[rand_int(sizeof(dwarf_syllable2) / sizeof(char*))]);
		strcat(name, dwarf_syllable3[rand_int(sizeof(dwarf_syllable3) / sizeof(char*))]);
		break;
	case RACE_DARK_ELF:
	case RACE_ELF:
	case RACE_HALF_ELF:
	case RACE_HIGH_ELF:
	case RACE_SPRITE:
		strcpy(name, elf_syllable1[rand_int(sizeof(elf_syllable1) / sizeof(char*))]);
		strcat(name, elf_syllable2[rand_int(sizeof(elf_syllable2) / sizeof(char*))]);
		strcat(name, elf_syllable3[rand_int(sizeof(elf_syllable3) / sizeof(char*))]);
		break;
	case RACE_DRACONIAN:
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
	case RACE_BARBARIAN:
	case RACE_GREAT:
	case RACE_HALF_TITAN:
	case RACE_HUMAN:
	case RACE_SKELETON:
	case RACE_SPECTRE:
	case RACE_VAMPIRE:
	case RACE_ZOMBIE:
		strcpy(name, human_syllable1[rand_int(sizeof(human_syllable1) / sizeof(char*))]);
		strcat(name, human_syllable2[rand_int(sizeof(human_syllable2) / sizeof(char*))]);
		strcat(name, human_syllable3[rand_int(sizeof(human_syllable3) / sizeof(char*))]);
		break;
	case RACE_BROO:
	case RACE_HALF_OGRE:
	case RACE_HALF_ORC:
	case RACE_HALF_TROLL:
	case RACE_KOBOLD:
		strcpy(name, orc_syllable1[rand_int(sizeof(orc_syllable1) / sizeof(char*))]);
		strcat(name, orc_syllable2[rand_int(sizeof(orc_syllable2) / sizeof(char*))]);
		strcat(name, orc_syllable3[rand_int(sizeof(orc_syllable3) / sizeof(char*))]);
		break;
	case RACE_KLACKON:
		strcpy(name, klackon_syllable1[rand_int(sizeof(klackon_syllable1) / sizeof(char*))]);
		strcat(name, klackon_syllable2[rand_int(sizeof(klackon_syllable2) / sizeof(char*))]);
		strcat(name, klackon_syllable3[rand_int(sizeof(klackon_syllable3) / sizeof(char*))]);
		break;
	case RACE_IMP:
	case RACE_MIND_FLAYER:
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

/*
 * Initialise the skills for a new character
 */
void get_starting_skills(int race)
{
	int i;
	player_race *r_ptr = &(race_info[race]);
	/* Wipe skills */
	for(i=0;i<MAX_SKILLS;i++)
	{
		skill_set[i].value=0;
		skill_set[i].experience=0;
		skill_set[i].base=0;
		skill_set[i].ceiling=100;
	}

	/* Now add some from template */
	switch (p_ptr->ptemplate)
	{
	case TPL_ADVENTURER:
		/* Add to the everyman skills */
		skill_set[SKILL_TOUGH].value+=1;
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=20;
		skill_set[SKILL_STAB].value+=20;
		skill_set[SKILL_SLASH].value+=20;
		skill_set[SKILL_CRUSH].value+=20;
		skill_set[SKILL_MISSILE].value+=20;
		skill_set[SKILL_PERCEPTION].value+=10;
		skill_set[SKILL_SAVE].value+=20;
		skill_set[SKILL_SEARCH].value+=5;
		skill_set[SKILL_STEALTH].value+=5;
		skill_set[SKILL_HEDGE].value+=5;
		break;
	case TPL_SWASHBUCKLER:
		/* Add to the everyman skills */
		skill_set[SKILL_TOUGH].value+=1;
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=15;
		skill_set[SKILL_STAB].value+=35;
		skill_set[SKILL_SLASH].value+=15;
		skill_set[SKILL_CRUSH].value+=15;
		skill_set[SKILL_MISSILE].value+=15;
		skill_set[SKILL_PERCEPTION].value+=15;
		skill_set[SKILL_SAVE].value+=15;
		skill_set[SKILL_SEARCH].value+=5;
		skill_set[SKILL_STEALTH].value+=5;
		break;
	case TPL_GLADIATOR:
		/* Add to the everyman skills */
		skill_set[SKILL_TOUGH].value+=2;
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=30;
		skill_set[SKILL_STAB].value+=30;
		skill_set[SKILL_SLASH].value+=30;
		skill_set[SKILL_CRUSH].value+=30;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=10;
		skill_set[SKILL_SAVE].value+=10;
		skill_set[SKILL_SEARCH].value+=5;
		skill_set[SKILL_STEALTH].value+=5;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MA].value+=1;
		break;
	case TPL_WARRIOR_MONK:
		/* Add to the everyman skills */
		skill_set[SKILL_TOUGH].value+=1;
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=35;
		skill_set[SKILL_STAB].value+=10;
		skill_set[SKILL_SLASH].value+=10;
		skill_set[SKILL_CRUSH].value+=10;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=15;
		skill_set[SKILL_SAVE].value+=10;
		skill_set[SKILL_SEARCH].value+=5;
		skill_set[SKILL_STEALTH].value+=15;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MA].value+=4;
		break;
	case TPL_ZEN_MONK:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=25;
		skill_set[SKILL_STAB].value+=10;
		skill_set[SKILL_SLASH].value+=10;
		skill_set[SKILL_CRUSH].value+=10;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=15;
		skill_set[SKILL_SAVE].value+=10;
		skill_set[SKILL_SEARCH].value+=10;
		skill_set[SKILL_STEALTH].value+=10;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MA].value+=2;
		skill_set[SKILL_MINDCRAFTING].value+=1;
		skill_set[SKILL_CHI].value+=1;
		break;
	case TPL_ASSASSIN:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=20;
		skill_set[SKILL_CLOSE].value+=30;
		skill_set[SKILL_STAB].value+=20;
		skill_set[SKILL_SLASH].value+=20;
		skill_set[SKILL_CRUSH].value+=20;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=20;
		skill_set[SKILL_SAVE].value+=10;
		skill_set[SKILL_SEARCH].value+=20;
		skill_set[SKILL_STEALTH].value+=30;
		break;
	case TPL_RANGER:
		/* Add to the everyman skills */
		skill_set[SKILL_TOUGH].value+=1;
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=10;
		skill_set[SKILL_CLOSE].value+=15;
		skill_set[SKILL_STAB].value+=15;
		skill_set[SKILL_SLASH].value+=15;
		skill_set[SKILL_CRUSH].value+=15;
		skill_set[SKILL_MISSILE].value+=35;
		skill_set[SKILL_PERCEPTION].value+=30;
		skill_set[SKILL_SAVE].value+=10;
		skill_set[SKILL_SEARCH].value+=20;
		skill_set[SKILL_STEALTH].value+=20;
		/* Give some specialist skill(s) */
		skill_set[SKILL_SHAMAN].value+=2;
		break;
	case TPL_SHAMAN:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=10;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=15;
		skill_set[SKILL_STAB].value+=15;
		skill_set[SKILL_SLASH].value+=15;
		skill_set[SKILL_CRUSH].value+=15;
		skill_set[SKILL_MISSILE].value+=15;
		skill_set[SKILL_PERCEPTION].value+=15;
		skill_set[SKILL_SAVE].value+=20;
		skill_set[SKILL_SEARCH].value+=10;
		skill_set[SKILL_STEALTH].value+=5;
		/* Give some specialist skill(s) */
		skill_set[SKILL_SHAMAN].value+=5;
		break;
	case TPL_MINDCRAFTER:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=10;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=15;
		skill_set[SKILL_STAB].value+=15;
		skill_set[SKILL_SLASH].value+=15;
		skill_set[SKILL_CRUSH].value+=15;
		skill_set[SKILL_MISSILE].value+=15;
		skill_set[SKILL_PERCEPTION].value+=15;
		skill_set[SKILL_SAVE].value+=15;
		skill_set[SKILL_SEARCH].value+=10;
		skill_set[SKILL_STEALTH].value+=5;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MINDCRAFTING].value+=4;
		skill_set[SKILL_CHI].value+=2;
		break;
	case TPL_MAGE:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=25;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=10;
		skill_set[SKILL_STAB].value+=10;
		skill_set[SKILL_SLASH].value+=10;
		skill_set[SKILL_CRUSH].value+=10;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=15;
		skill_set[SKILL_SAVE].value+=20;
		skill_set[SKILL_SEARCH].value+=10;
		skill_set[SKILL_STEALTH].value+=5;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MANA].value+=4;
		/* hermetic skill(s) are added later */
		break;
	case TPL_WARLOCK:
		/* Add to the everyman skills */
		skill_set[SKILL_TOUGH].value+=1;
		skill_set[SKILL_DEVICE].value+=10;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=15;
		skill_set[SKILL_STAB].value+=15;
		skill_set[SKILL_SLASH].value+=15;
		skill_set[SKILL_CRUSH].value+=20;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=10;
		skill_set[SKILL_SAVE].value+=15;
		skill_set[SKILL_SEARCH].value+=10;
		skill_set[SKILL_STEALTH].value+=5;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MANA].value+=2;
		/* hermetic skill(s) are added later */
		break;
	case TPL_POWERWEAVER:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=15;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=5;
		skill_set[SKILL_STAB].value+=5;
		skill_set[SKILL_SLASH].value+=5;
		skill_set[SKILL_CRUSH].value+=5;
		skill_set[SKILL_MISSILE].value+=5;
		skill_set[SKILL_PERCEPTION].value+=10;
		skill_set[SKILL_SAVE].value+=15;
		skill_set[SKILL_SEARCH].value+=10;
		skill_set[SKILL_STEALTH].value+=5;
		/* Give some specialist skill(s) */
		skill_set[SKILL_MANA].value+=2;
		skill_set[SKILL_CHI].value+=2;
		skill_set[SKILL_MINDCRAFTING].value+=3;
		skill_set[SKILL_SHAMAN].value+=3;
		/* hermetic skill(s) are added later */
		break;
	case TPL_TOURIST:
		/* Add to the everyman skills */
		skill_set[SKILL_DEVICE].value+=5;
		skill_set[SKILL_DISARM].value+=5;
		skill_set[SKILL_CLOSE].value+=10;
		skill_set[SKILL_STAB].value+=10;
		skill_set[SKILL_SLASH].value+=10;
		skill_set[SKILL_CRUSH].value+=10;
		skill_set[SKILL_MISSILE].value+=10;
		skill_set[SKILL_PERCEPTION].value+=5;
		skill_set[SKILL_SAVE].value+=5;
		skill_set[SKILL_SEARCH].value+=5;
		skill_set[SKILL_STEALTH].value+=5;
		/* Tourist gets no specialist skills */
		break;
	default:
		break;
	}
	
	/* Template skills will become base skills - you can start advancing 
	 * them immediately */

	for(i=0;i<MAX_SKILLS;i++)
	{
		skill_set[i].base=skill_set[i].value;
	}
	skill_set[SKILL_TOUGH].base+= 1;

	/* 
	 * add basic everyman values (mainly from race)
	 */
	skill_set[SKILL_TOUGH].value+=2;
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

/*
 * Get hermetic skills randomly
 */
void get_hermetic_skills_randomly()
{
	int i,choices;
	int choice,old_choice;

	switch (p_ptr->ptemplate)
	{
	case TPL_MAGE:
		choices = 3;
		break;
	case TPL_WARLOCK:
		choices = 1;
		break;
	case TPL_POWERWEAVER:
		choices = 2;
		break;
	default:
		return; /* No skills */
	}

	old_choice = rand_range(1,8);
	for(i=0;i<choices;i++)
	{
		choice = rand_range(1,8);
		if(rand_range(1,3) != 1) choice = old_choice;
		switch(choice)
		{
		case 1:
			skill_set[SKILL_THAUMATURGY].value+=5;
			break;
		case 2:
			skill_set[SKILL_NECROMANCY].value+=5;
			break;
		case 3:
			skill_set[SKILL_SORCERY].value+=5;
			break;
		case 4:
			skill_set[SKILL_CONJURATION].value+=5;
			break;
		case 5:
			skill_set[SKILL_ANIMAE].value+=5;
			break;
		case 6:
			skill_set[SKILL_CORPORIS].value+=5;
			break;
		case 7:
			skill_set[SKILL_VIS].value+=5;
			break;
		case 8:
			skill_set[SKILL_NATURAE].value+=5;
			break;
		}
		old_choice = choice;
	}
}

/*
 * Get hermetic skills
 */
void get_hermetic_skills()
{
	int k,i,choices;
    char c;
    char buf[80];

	switch (p_ptr->ptemplate)
	{
	case TPL_MAGE:
		choices = 3;
		break;
	case TPL_WARLOCK:
		choices = 1;
		break;
	case TPL_POWERWEAVER:
		choices = 2;
		break;
	default:
		return; /* No skills */
	}

    /* Extra info */
	clear_from(15);
	Term_putstr(5, 15, -1, TERM_WHITE,
    "Please select a school or type of hermetic magic to specialise");
    Term_putstr(5, 16, -1, TERM_WHITE,
    "in. Thaumaturgy school is the most offensive, Sorcery school is");
	Term_putstr(5,17,-1,TERM_WHITE,
	"the least offensive. Naturae spells deal with matter, Corporis");
    Term_putstr(5,18,-1,TERM_WHITE,
	"with flesh, Animae with spirits and Vis with energy. Each school");
    Term_putstr(5,19,-1,TERM_WHITE,
	"has spells of all four types.");
	
	put_str("a) Thaumaturgy",22,2);
	put_str("b) Necromancy",22,17);
	put_str("c) Sorcery",22,32);
	put_str("d) Conjuration",22,47);
	put_str("e) Animae",23,2);
	put_str("f) Corporis",23,17);
	put_str("g) Vis",23,32);
	put_str("h) Naturae",23,47);

	for(i=choices;i>0;i--)
	{

		/* Get a choice */
		while (1)
		{
			sprintf(buf, "%d choic%s left. Choose a school or type (a-h): ",i,(i>1 ? "es":"e"));
			put_str(buf, 21, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			k = (islower(c) ? A2I(c) : -1);
			if ((k >= 0) && (k <= 7)) break;
			if (c == '?') do_cmd_help(syshelpfile);

			else bell();
		}

		switch(k)
		{
		case 0:
			skill_set[SKILL_THAUMATURGY].value+=5;
			break;
		case 1:
			skill_set[SKILL_NECROMANCY].value+=5;
			break;
		case 2:
			skill_set[SKILL_SORCERY].value+=5;
			break;
		case 3:
			skill_set[SKILL_CONJURATION].value+=5;
			break;
		case 4:
			skill_set[SKILL_ANIMAE].value+=5;
			break;
		case 5:
			skill_set[SKILL_CORPORIS].value+=5;
			break;
		case 6:
			skill_set[SKILL_VIS].value+=5;
			break;
		case 7:
			skill_set[SKILL_NATURAE].value+=5;
			break;
		}
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
	int        i;

	birther	temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->age;
	temp.wt = p_ptr->wt;
	temp.ht = p_ptr->ht;
	temp.sc = p_ptr->sc;
	temp.au = p_ptr->au;
	temp.birthday = p_ptr->birthday;

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
 * The "maximise_mode" code is important	-BEN-
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
        if ((j > 42) && (j < 57)) break; /* 57 was 54...
                    I hate 'magic numbers' :< TY */
	}

	/* Acquire the stats */
	for (i = 0; i < 6; i++)
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
			stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
			stat_use[i] = adjust_stat(p_ptr->stat_max[i], (s16b)bonus, FALSE);

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
	int		i, j;
	int		lastroll;
#ifdef SHOW_LIFE_RATE
    int         percent;
#endif
    


	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp; /* + cp_ptr->c_exp; */

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp; /* + cp_ptr->c_mhp; */

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie/2;

	/* Pre-calculate level 1 hitdice */
	player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */

	/* 'Roll' the hitpoint values */
	lastroll = p_ptr->hitdie;
	for (i = 1; i < 100; i++)
	{
		player_hp[i]=lastroll;
		lastroll--;
		if(lastroll<1) lastroll = p_ptr->hitdie;
	}
	/* Now shuffle them */
	for(i=1;i<100;i++)
	{
		j=randint(99);
		lastroll=player_hp[i];
		player_hp[i]=player_hp[j];
		player_hp[j]=lastroll;
	}
	/* Make each a cumulative score */
	for(i=1;i<100;i++)
	{
	player_hp[i] = player_hp[i-1] +player_hp[i];
	}
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int		i, n, chart, roll, social_class;

	char	*s, *t;

	char	buf[240];



	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) history[i][0] = '\0';


	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
        case RACE_GREAT:

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
        case RACE_BROO:
        {
            chart = 129;
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
	p_ptr->birthday=randint(365);
	/* This isn't stored in the birther struct as it isn't important yet...*/
	p_ptr->startdate=randint(365);

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
	int        i, gold;

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
 *  Initialise the matrix of quests
 */
void initialise_quests()
{
	int i,j;

	/* Start with no quests */
	for (i = 0; i < MAX_QUESTS; i++)
	{
		q_list[i].level = 0;
		q_list[i].r_idx = 0;
		q_list[i].cur_num = 0;
		q_list[i].max_num = 0;
	}

	/* Add end of dungeon quests */
	for(i=0; i< MAX_CAVES; i++)
	{
		/* End Creature */
		j=i*2;
		if(dun_defs[i].first_guardian > 0)
		{
			q_list[j].level = dun_defs[i].first_level;
			q_list[j].r_idx = dun_defs[i].first_guardian;
			q_list[j].dungeon = i;
			q_list[j].max_num = 1;
		}
		j++;
		/* Second Guardian? */
		if(dun_defs[i].second_guardian > 0)
		{
			q_list[j].level = dun_defs[i].second_level;
			q_list[j].r_idx = dun_defs[i].second_guardian;
			q_list[j].dungeon = i;
			q_list[j].max_num = 1;
		}
	}
}


/*
 * Get number of quest monsters for quest i
 * Heino Vander Sanden
 */
int get_number_monster(int i)
{
	int num;

	if ((r_info[q_list[i].r_idx].flags1 & (RF1_UNIQUE)) ||
		(r_info[q_list[i].r_idx].flags2 & (RF2_MULTIPLY)))
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
int get_rnd_q_monster(int q_idx)
{
	int r_idx,j,tmp;

	tmp = rand_range(1,10);
	/* first level 6 monster (87), last monster (573) */
	switch (tmp)
	{
		case 1 : r_idx = rand_range(181,220); break;
		case 2 : r_idx = rand_range(221,260); break;
		case 3 : r_idx = rand_range(261,300); break;
		case 4 : r_idx = rand_range(301,340); break;
		case 5 : r_idx = rand_range(341,380); break;
		case 6 : r_idx = rand_range(381,420); break;
		case 7 : r_idx = rand_range(421,460); break;
		case 8 : r_idx = rand_range(461,500); break;
		case 9 : r_idx = rand_range(501,530); break;
		case 10 : r_idx = rand_range(531,560); break;
		default : r_idx = rand_range (87,573);
	}
	/* Don't multipliers to be random guardians */
	if (r_info[r_idx].flags2 & (RF2_MULTIPLY)) return (0);
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
	

	/* Start with no quests */
	initialise_quests(); /* DEAN */

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


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* Wipe the spells */
	for (i=0;i<MAX_SCHOOL;i++)
	{
		spell_learned[i] = 0L;
		spell_worked[i] = 0L;
		spell_forgotten[i] = 0L;
	}
	for (i = 0; i < 128; i++)
	{
		spell_order[i] = 255;
	}

	/* Clear "cheat" options */
	cheat_peek = FALSE;
	cheat_hear = FALSE;
	cheat_room = FALSE;
	cheat_xtra = FALSE;
	cheat_know = FALSE;
	cheat_live = FALSE;
	cheat_skll = FALSE;
	cheat_wzrd = FALSE;

	/* Assume no winning game */
	total_winner = FALSE;

	/* Assume no panic save */
	panic_save = 0;

	/* Assume no cheating */
	noscore = 0;
}




/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */

static byte player_init[MAX_TEMPLATE][3][2] =
{
	{
		/* Adventurer */
        { TV_RING, SV_RING_RES_FEAR }, /* Warriors need it! */
		{ TV_SWORD, SV_CUTLASS },
		{ TV_CHARM, 0 }
	},

	{
		/* Swashbuckler */
        { TV_POTION, SV_POTION_SPEED },
		{ TV_SWORD, SV_RAPIER },
        { TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR }
	},

	{
		/* Gladiator */
        { TV_RING, SV_RING_FREE_ACTION },
		{ TV_SWORD, SV_BROAD_SWORD },
        { TV_SHIELD, SV_SMALL_METAL_SHIELD }
	},

	{
		/* Warrior Monk */
        { TV_RING, SV_RING_SUSTAIN_DEX },
        { TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
	},

	{
		/* Zen Monk */
        { TV_RING, SV_RING_SUSTAIN_WIS },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION }
	},

	{
		/* Assassin */
        { TV_RING, SV_RING_RESIST_POIS },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
    },

	{
        /* Ranger */
        { TV_BOW, SV_LONG_BOW },
        { TV_ARROW, SV_AMMO_NORMAL },
        { TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR }
    },

    {
        /* Shaman */
        { TV_HAFTED, SV_QUARTERSTAFF },
		{ TV_POTION, SV_POTION_HEALING },
        { TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL }
	},

    {
        /* Mindcrafter */
        { TV_RING,SV_RING_SUSTAIN_WIS },
        { TV_SWORD, SV_SHORT_SWORD },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
    },

    {
        /* Mage */
        { TV_RING, SV_RING_SUSTAIN_INT },
        { TV_POTION, SV_POTION_RESTORE_MANA },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
    },

	{
        /* Warlock */
        { TV_RING, SV_RING_SUSTAIN_INT },
		{ TV_SWORD, SV_SMALL_SWORD },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
	},

	{
		/* Powerweaver */
		{TV_RING,SV_RING_SUSTAIN_INT},
		{ TV_POTION,SV_POTION_RESTORE_MANA },
		{TV_RING,SV_RING_SUSTAIN_WIS},
	},

	{
		/* Tourist */
        { TV_SWORD,SV_DAGGER },
		{ TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
        { TV_CLOAK,SV_CLOAK }
	},

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
	

	/* Get local object */
	q_ptr = &forge;

    if (p_ptr->prace == RACE_GOLEM || p_ptr->prace == RACE_SKELETON ||
        p_ptr->prace == RACE_ZOMBIE || p_ptr->prace == RACE_VAMPIRE ||
        p_ptr->prace == RACE_SPECTRE)
    {
        /* Hack -- Give the player scrolls of satisfy hunger */
        object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_SATISFY_HUNGER));
        q_ptr->number = (char)rand_range(2,5);
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
        q_ptr->number = (char)rand_range(3, 7);
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);
    }


        /* Get local object */
        q_ptr = &forge;


    if (p_ptr->prace == RACE_VAMPIRE)
    {

        /* Hack -- Give the player scrolls of light */
        object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_LIGHT));
        q_ptr->number = (char)rand_range(3,7);
        object_aware(q_ptr);
        object_known(q_ptr);

        /* These objects are "storebought" */
        q_ptr->ident |= IDENT_STOREB;

        (void)inven_carry(q_ptr, FALSE);

        /* Get local object */
        q_ptr = &forge;

        /* Hack -- Give the player scrolls of DARKNESS! */
        object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_DARKNESS));
        q_ptr->number = (char)rand_range(2,5);
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
        q_ptr->number = (char)rand_range(3, 7);
        q_ptr->pval = (char)rand_range(3, 7) * 500;
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);
    }
    /* For characters starting with magical skill, give them a spellbook */
    if (skill_set[SKILL_MANA].value > 0) {
        int spellbook = 0;
        int spellcount = 0;
        if (skill_set[SKILL_SORCERY].value == 0 && 
            skill_set[SKILL_NECROMANCY].value == 0 &&
            skill_set[SKILL_THAUMATURGY].value == 0 && 
            skill_set[SKILL_CONJURATION].value == 0) {
            /* All spell skills are in corporis/animae/vis/naturae */
                spellbook = rand_int(4); /* Give player a random spellbook */
        } else {
            /* HACK - select spellbook randomly */
            spellcount = rand_int(20) + 1;
            while (spellcount) {
                if (skill_set[SKILL_SORCERY].value > 0) {
        	    spellcount--;
        	    if (spellcount == 0) {
        	        spellbook = 0;
        	        break;
        	    }
        	}
        	if (skill_set[SKILL_THAUMATURGY].value > 0) {
        	    spellcount--;
                     if (spellcount == 0) {
                         spellbook = 1;
                         break;
                     }
                 }
                 if (skill_set[SKILL_CONJURATION].value > 0) {
                     spellcount--;
                     if (spellcount == 0) {
                         spellbook = 2;
                         break;
                     }
                 }
                 if (skill_set[SKILL_NECROMANCY].value > 0) {
                     spellcount--;
                     if (spellcount == 0) {
                         spellbook = 3;
                         break;
                     }
                 }
             }
         } /* spellbook is now 0-3 */
             q_ptr = &forge;
             object_prep(q_ptr, lookup_kind(90 + spellbook, 0));
             q_ptr->number = 1;
             object_aware(q_ptr);
             object_known(q_ptr);
             (void)inven_carry(q_ptr, FALSE);
    }
        	 		

    /* Hack -- Give the player three useful objects */
    for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
        tv = player_init[p_ptr->ptemplate][i][0];
		sv = player_init[p_ptr->ptemplate][i][1];


        if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
                 p_ptr->prace == RACE_BARBARIAN)
        /* Barbarians do not need a ring of resist fear */
                 sv = SV_RING_SUSTAIN_STR;

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player an object */
		object_prep(q_ptr, lookup_kind(tv, sv));


		if (tv == TV_ARROW)
		{
			/* If we have an arrow, we need more than one */
			q_ptr->number = (char)rand_range(15,45);
		}

		if (tv == TV_CHARM)
		{
			q_ptr->pval = 0; /* Sulphur - it is the easiest to use */
		}

        /* Assassins begin the game with a poisoned dagger */
        if (tv == TV_SWORD && p_ptr->ptemplate == TPL_ASSASSIN)
        {
            q_ptr->name2 = EGO_BRAND_POIS;
        }

        /* These objects are "storebought" */
        q_ptr->ident |= IDENT_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr, FALSE);
	}
}

/*
 * Generate the additional quests
 * Heino Vander Sanden, Jimmy De Laet, and Robert Ruehlmann
 */
void player_birth_quests(void)
{
	int i,j;
	bool same_level;

	/* Generate to MAX_Q_IDX with random quests */
	for (i = (MAX_CAVES*2); i<MAX_Q_IDX; i++)
	{
		do
		{
			same_level = FALSE;

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

			/* No 2 quests on the same level */
			for (j = 2; j<i; j++)
			{
				if (q_list[i].level == q_list[j].level)
				{
					same_level = TRUE;
					break;
				}
			}
		}
		while (same_level);
		
		/* Make sure uniques aren't created outside their level */
		if (r_info[q_list[i].r_idx].flags1 & RF1_UNIQUE) r_info[q_list[i].r_idx].flags1 |= RF1_ALWAYS_GUARD;

		/* Now place quest in a random dungeon
		 * but not on its lowest two levels as they *may*
		 * contain 'hard-coded' quests.
		 */
		j=rand_range(1,MAX_CAVES)-1;
		while((q_list[i].level <= dun_defs[j].offset) ||
			      (q_list[i].level >= dun_defs[j].max_level + dun_defs[j].offset) ||
				  (q_list[i].level == dun_defs[j].first_level + dun_defs[j].offset) ||
				  (q_list[i].level == dun_defs[j].second_level + dun_defs[j].offset))
		{
			j=rand_range(1,MAX_CAVES)-1;
		}
		/* j now holds a valid dungeon, so set the quest and
		 * modify its level
		 */
		q_list[i].dungeon = j;
		q_list[i].level -= dun_defs[j].offset;

		q_list[i].max_num = get_number_monster(i);
	}
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
	int i, j, k, m, n, v;

	int mode = 0;

	bool flag = FALSE;
	bool prev = FALSE;

	cptr str;

	char c;

	char p2 = ')';
	char b1 = '[';
	char b2 = ']';

	char buf[80];

	bool autoroll = FALSE;
	bool point_mod = TRUE;

	char inp[80];

	/*** Intro ***/

	/* Clear screen */
	Term_clear();

	/* Title everything */
	put_str("Name        :", 2, 1);
	put_str("Sex         :", 3, 1);
	put_str("Race        :", 4, 1);
	put_str("Template    :", 5, 1);

	/*** Instructions ***/

	/* Display some helpful information */
	Term_putstr(5, 10, -1, TERM_WHITE,
		"Please answer the following questions.  Most of the questions");
	Term_putstr(5, 11, -1, TERM_WHITE,
		"display a set of standard answers, and many will also accept");
	Term_putstr(5, 12, -1, TERM_WHITE,
		"some special responses, including 'Q' to quit, 'S' to restart");
	Term_putstr(5, 13, -1, TERM_WHITE,
		"or '?' for help.  Note that 'Q' and 'S' must be capitalized.");


	/*** Quick-Start ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Quick-Start gives you a completely random character without");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"further prompting.  At this point, you may press the '=' key");
	Term_putstr(5,17,-1,TERM_WHITE,
		"to change the startup options. You will not be able to change");
	Term_putstr(5,18,-1,TERM_WHITE,
		"these once you have started character generation.");

	/* Choose */
	while (1)
	{
		sprintf(buf, "Quick-Start? (y/n/Q/S/?/=): ");
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if ((c == 'y') || (c == 'n') || (c == 'Y') || (c == 'N')) break;
		if (c == '?') do_cmd_help(syshelpfile);
		if (c == '=')
		{
			Term_save();
			do_cmd_options_aux(7,"Startup Options");
			Term_load();
		}
		else bell();
	}

	/* Clean up */
	clear_from(15);
	
	if ((c == 'Y') || (c == 'y'))
	{
	
		/*** Player sex ***/
		/* Set sex */
		p_ptr->psex =(char)rand_range(0,1);
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->title;
		/* Display */
		c_put_str(TERM_L_BLUE, str, 3, 15);
		
		/*** Player race ***/
		/* Set race */
		k=rand_range(0,MAX_RACES-1);
		hack_chaos_feature = FALSE;
		if (k==RACE_BROO) hack_chaos_feature = TRUE;
		p_ptr->prace = k;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;
		/* Display */
		c_put_str(TERM_L_BLUE, str, 4, 15);
		/*** Player template ***/
		while(1)
		{
			p_ptr->ptemplate = (char)rand_range(0,MAX_TEMPLATE-1);
			/* Analyze */
			cp_ptr = &template_info[p_ptr->ptemplate];
			mp_ptr = &magic_info;
			str = cp_ptr->title;

			if (rp_ptr->choice & (1L << p_ptr->ptemplate )) break;
		}
		/* Display */
		c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);
		
		/* Get skill values */
		get_starting_skills(p_ptr->prace);
		get_hermetic_skills_randomly();

		/* Set maxima */
		for (i=0;i<MAX_SKILLS;i++)
		{
			if(skill_set[i].value>10)
			{
				skill_set[i].value+= (byte)rand_int(10);
			}
			else if(skill_set[i].value>5)
			{
				skill_set[i].value+= (byte)rand_int(5);
			}
			else if(skill_set[i].value>0)
			{
				skill_set[i].value+= (byte)rand_int(3);
			}

			skill_set[i].max_value=skill_set[i].value;
		}
		
		if (skill_set[SKILL_THAUMATURGY].value > 0) hack_chaos_feature = TRUE;
	
		/* Get a random name */
		create_random_name(p_ptr->prace,player_name);
		/* Display */
		c_put_str(TERM_L_BLUE, player_name, 2, 13);

		/* Generate quests */
		/* Set max number of quest */
		MAX_Q_IDX =randint(20)+10+(2*MAX_CAVES);
		player_birth_quests();

#ifdef ALLOW_AUTOROLLER
		/* Set "autoroll" */
		autoroll = TRUE;
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
				/* Race/Template bonus */
				j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
				/* Obtain the "maximal" stat */
				m = adjust_stat(17, (s16b)j, TRUE);
				/* Save the maximum */
				mval[i] = m;
			}
			/* Hack in the minimum stats */
			for (i = 0; i < 6; i++)
			{
				stat_limit[i] = 0;
			}
			/* Save the minimum stat depending on template */
			switch(p_ptr->ptemplate)
			{
			case TPL_ADVENTURER:
				stat_limit[A_STR]=mval[A_STR]-1;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case TPL_SWASHBUCKLER:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_STR]=mval[A_STR]-2;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_CHR]=mval[A_CHR]-4;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				break;
			case TPL_GLADIATOR:
				stat_limit[A_CON]=mval[A_CON]-1;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_STR]=mval[A_STR]-2;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case TPL_WARRIOR_MONK:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_STR]=mval[A_STR]-4;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				break;
			case TPL_ZEN_MONK:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_WIS]=mval[A_WIS]-2;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case TPL_ASSASSIN:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_INT]=mval[A_INT]-4;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				break;
			case TPL_RANGER:
				stat_limit[A_CON]=mval[A_CON]-1;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_CHR]=mval[A_CHR]-2;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case TPL_SHAMAN:
				stat_limit[A_CHR]=mval[A_CHR]-1;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				stat_limit[A_WIS]=mval[A_WIS]-2;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case TPL_MINDCRAFTER:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case TPL_MAGE:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-2;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case TPL_WARLOCK:
				stat_limit[A_CON]=mval[A_CON]-1;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_STR]=mval[A_STR]-4;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				break;
			case TPL_POWERWEAVER:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_WIS]=mval[A_WIS]-2;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CHR]=mval[A_CHR]-4;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				if (stat_limit[A_CHR] > 18) stat_limit[A_CHR] -= 9;
				break;
			case TPL_TOURIST:
				/* No stat limits */
				break;
			}
		}

#endif /* ALLOW_AUTOROLLER */

		/* Clean up */
		clear_from(10);


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
				put_str("Template    :", 5, 1);

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

				/* Take note every 25 rolls */
				flag = (!(auto_round % 25L));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Dump data */
					birth_put_stats();

					/* Dump round */
					put_str(format("%6ld", auto_round), 9, 73);

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

			/* Hack -- get a chaos patron even if you are not chaotic (yet...) */
			p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;

			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;

			/* Player has no recal ritual yet */
			p_ptr->ritual = MAX_TOWNS + 1;

			/* Player has no house yet */
			for(i=0;i<MAX_TOWNS;i++)
			{
				p_ptr->house[i] = 0;
			}
			
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
				p_ptr->energy = 1050;

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
					do_cmd_help(syshelpfile);
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

		/* Allow name to be edited, recolor it, prepare savefile */

		get_name();


		/* Prompt for it */
		prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

		/* Get a key */
		c = inkey();

		/* Quit */
		if (c == 'Q') quit(NULL);

		/* Start over */
		if (c == 'S') return (FALSE);

		/* Accept */
	}
	else /* Interactive character */
	{
	
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
			if (c == '?') do_cmd_help(syshelpfile);
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

		/* Dump races */
		for (n = 0; n < MAX_RACES; n++)
		{
			/* Analyze */
			p_ptr->prace = n;
			rp_ptr = &race_info[p_ptr->prace];
			str = rp_ptr->title;
		
			/* Display */

			if (n<26)
			{
				sprintf(buf, "%c%c %s", I2A(n), p2, str);
			}
			else
			{
				sprintf(buf, "%d%c %s", (n - 25), p2, str); /* HACK */
			}
			put_str(buf, 18 + (n/5), 2 + 15 * (n%5));
		}

		/* Choose */
		while (1)
		{
			sprintf(buf, "Choose a race (%c-4): ", I2A(0));
			put_str(buf, 17, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			if (c == '1')
			{
				k = 26; /* RACE_VAMPIRE; */
				break;
			}
			else if (c == '2')
			{
				k = 27; /* RACE_SPECTRE; */
				break;
			}
			else if (c == '3')
			{
				k = 28;  /* RACE_SPRITE; */
				break;
			}
			else if (c == '4')
			{
				k = 29; /* RACE_BROO; */
				break;
			}
			else
			{
				k = (islower(c) ? A2I(c) : -1);
				if ((k >= 0) && (k < n)) break;
				if (c == '?') do_cmd_help(syshelpfile);
				else bell();
			}
		}

		/* Set race */
		hack_chaos_feature = FALSE;
		if (k==RACE_BROO) hack_chaos_feature= TRUE;
		p_ptr->prace = k;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;

		/* Display */
		c_put_str(TERM_L_BLUE, str, 4, 15);
		
		/* Get a random name now we have a race*/
		create_random_name(p_ptr->prace,player_name);
		/* Display */
		c_put_str(TERM_L_BLUE, player_name, 2, 15);

		/* Clean up */
		clear_from(15);


		/*** Player template ***/

		/* Extra info */
		Term_putstr(5, 15, -1, TERM_WHITE,
		"Your 'template' determines various starting abilities and bonuses.");
		Term_putstr(5, 16, -1, TERM_WHITE,
		"Any entries in parentheses should only be used by advanced players.");

		/* Dump templates */
		for (n = 0; n < MAX_TEMPLATE; n++)
		{
			cptr mod = "";

			/* Analyze */
			p_ptr->ptemplate = n;
			cp_ptr = &template_info[p_ptr->ptemplate];
			mp_ptr = &magic_info;
			str = cp_ptr->title;

			if (!(rp_ptr->choice & (1L << n )))
			{
				sprintf(buf, "%c%c (%s)%s", I2A(n), p2, str, mod);
			}
			else
			{
				sprintf(buf, "%c%c %s%s", I2A(n), p2, str, mod);
			}
			/* Display */
			put_str(buf, 19 + (n/3), 2 + 20 * (n%3));
		}

		/* Get a template */
		while (1)
		{
			sprintf(buf, "Choose a template (%c-%c): ", I2A(0), I2A(n-1));
			put_str(buf, 18, 2);
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);
			k = (islower(c) ? A2I(c) : -1);
			if ((k >= 0) && (k < n)) break;
			if (c == '?') do_cmd_help(syshelpfile);
			else bell();
		}

		/* Set template */
		p_ptr->ptemplate = k;
		cp_ptr = &template_info[p_ptr->ptemplate];
		mp_ptr = &magic_info;
		str = cp_ptr->title;

		/* Display */
		c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

		/* Get initial skill values */
		get_starting_skills(p_ptr->prace);
		get_hermetic_skills();

		/* Nudge skills up and set maxima */
		for (i=0;i<MAX_SKILLS;i++)
		{
			if(skill_set[i].value>10)
			{
				skill_set[i].value+= (byte)rand_int(10);
			}
			else if(skill_set[i].value>5)
			{
				skill_set[i].value+= (byte)rand_int(5);
			}
			else if(skill_set[i].value>0)
			{
				skill_set[i].value+= (byte)rand_int(3);
			}

			skill_set[i].max_value=skill_set[i].value;
		}

		if (skill_set[SKILL_THAUMATURGY].value > 0) hack_chaos_feature = TRUE;
		
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
		
		/* For characters starting with shaman skill, start them with a spirit */
		if (skill_set[SKILL_SHAMAN].value > 0) {
		    c = '\0';
		    while (1) {
		        sprintf(buf, "Choose a life spirit(a) or wild spirit(b): ");
		        put_str(buf, 17, 2);
		        c = inkey();
		        c = tolower(c);
		    	if (c == 'a') break;
		    	if (c == 'b') break;
		    }
		    if (c == 'a') spirits[0].pact = TRUE;
		    if (c == 'b') spirits[1].pact = TRUE;
		}

		/* Clean up */
		clear_from(15);

		/* Generate quests */
		/* Set max number of quest */
		MAX_Q_IDX =randint(20)+10+(2*MAX_CAVES);
		player_birth_quests();

#ifdef ALLOW_AUTOROLLER

		/*** Autoroll ***/


		/* Set "autoroll" and "point_mod" */
		autoroll = use_autoroller;
		point_mod = (spend_points & !(use_autoroller));

		/* Initialize Autoroller if necessary */
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

				/* Race/Template bonus */
				j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

				/* Obtain the "maximal" stat */
				m = adjust_stat(17,(s16b) j, TRUE);

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

		/* Clean up */
		clear_from(10);


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
				put_str("Template    :", 5, 1);

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
				if (point_mod)
				{
					for(i=0;i<6;i++)
					{
						p_ptr->stat_cur[i] = p_ptr->stat_max[i] = 8;
					}
					point_mod_player();  
				}
				else
				{
					get_stats();
				}
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

				/* Take note every 25 rolls */
				flag = (!(auto_round % 25L));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Dump data */
					birth_put_stats();

					/* Dump round */
					put_str(format("%6ld", auto_round), 9, 73);

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

			/* Hack -- get a chaos patron even if you are not chaotic (yet...) */
			p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;

			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;

			/* Player is ready to move... */
			p_ptr->energy=1000;

			/* Player has no recal ritual yet */
			p_ptr->ritual = MAX_TOWNS + 1;

			/* Player has no house yet */
			for(i=0;i<MAX_TOWNS;i++)
			{
				p_ptr->house[i] = 0;
			}
	
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
				p_ptr->energy = 1050;

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
					do_cmd_help(syshelpfile);
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

		/* Accept */
		return (TRUE);
	}
	/* Just in case */
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
	for (n = 0; n < MAX_STORES_TOTAL; n++)
	{
		/* Initialize */
		store_init(n);

		/* Ignore home, hall  and pawnbrokers */
		if ((store[n].type != 99) &&
			(store[n].type != STORE_HOME) &&
			(store[n].type != STORE_HALL) &&
			(store[n].type != STORE_PAWN))
		{
			/* Maintain the shop (ten times) */
			for (i = 0; i < 10; i++) store_maint(n);
		}
	}
}



