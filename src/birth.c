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
	{"Wizard.  ",						 95, 9, 54, 125},
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
	{"Wizard.  ",							 99, 11, 3, 125},
	{"Clan Elder.  ",						100, 11, 3, 140},

	{"You are one of several children of a Gnome ",		 85, 13, 14, 45},
	{"You are the only child of a Gnome ",			100, 13, 14, 55},

	{"Beggar.  ",						 20, 14, 3, 55},
	{"Braggart.  ",						 50, 14, 3, 70},
	{"Prankster.  ",						 75, 14, 3, 85},
	{"Warrior.  ",						 95, 14, 3, 100},
	{"Wizard.  ",							100, 14, 3, 125},

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
	{"You have blue-grey eyes, ",				100, 50, 51, 50},

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
 * Autoroll limit
 */
static s16b stat_limit[A_MAX];

/*
 * Autoroll matches
 */
static s32b stat_match[A_MAX];

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
static cptr dwarf_syllable1[] =
{
	"B", "D", "F", "G", "Gl", "H", "K", "L", "M", "N", "R", "S", "T", "Th", "V",
};

static cptr dwarf_syllable2[] =
{
	"a", "e", "i", "o", "oi", "u",
};

static cptr dwarf_syllable3[] =
{
	"bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar", "nus", "rin", "ran", "sin", "sil", "sur",
};

/* Elves */
static cptr elf_syllable1[] =
{
	"Al", "An", "Bal", "Bel", "Cal", "Cel", "El", "Elr", "Elv", "Eow", "Ear", "F", "Fal", "Fel", "Fin", "G", "Gal", "Gel", "Gl", "Is", "Lan", "Leg", "Lom", "N", "Nal", "Nel",  "S", "Sal", "Sel", "T", "Tal", "Tel", "Thr", "Tin",
};

static cptr elf_syllable2[] =
{
	"a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra", "ilma", "il-Ga", "ili", "o", "orfi", "u", "y",
};

static cptr elf_syllable3[] =
{
	"l", "las", "lad", "ldor", "ldur", "linde", "lith", "mir", "n", "nd", "ndel", "ndil", "ndir", "nduil", "ng", "mbor", "r", "rith", "ril", "riand", "rion", "s", "thien", "viel", "wen", "wyn",
};

/* Gnomes */
static cptr gnome_syllable1[] =
{
	"Aar", "An", "Ar", "As", "C", "H", "Han", "Har", "Hel", "Iir", "J", "Jan", "Jar", "K", "L", "M", "Mar", "N", "Nik", "Os", "Ol", "P", "R", "S", "Sam", "San", "T", "Ter", "Tom", "Ul", "V", "W", "Y",
};

static cptr gnome_syllable2[] =
{
	"a", "aa",  "ai", "e", "ei", "i", "o", "uo", "u", "uu",
};

static cptr gnome_syllable3[] =
{
	"ron", "re", "la", "ki", "kseli", "ksi", "ku", "ja", "ta", "na", "namari", "neli", "nika", "nikki", "nu", "nukka", "ka", "ko", "li", "kki", "rik", "po", "to", "pekka", "rjaana", "rjatta", "rjukka", "la", "lla", "lli", "mo", "nni",
};

/* Hobbit */
static cptr hobbit_syllable1[] =
{
	"B", "Ber", "Br", "D", "Der", "Dr", "F", "Fr", "G", "H", "L", "Ler", "M", "Mer", "N", "P", "Pr", "Per", "R", "S", "T", "W",
};

static cptr hobbit_syllable2[] =
{
	"a", "e", "i", "ia", "o", "oi", "u",
};

static cptr hobbit_syllable3[] =
{
	"bo", "ck", "decan", "degar", "do", "doc", "go", "grin", "lba", "lbo", "lda", "ldo", "lla", "ll", "lo", "m", "mwise", "nac", "noc", "nwise", "p", "ppin", "pper", "tho", "to",
};

/* Human */
static cptr human_syllable1[] =
{
	"Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar", "B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et", "Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha", "Ib", "Jer", "K", "Ka", "Ked", "L", "Loth", "Lar", "Leg", "M", "Mir", "N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T", "Tr", "Th", "V", "Y", "Z", "W", "Wic",
};

static cptr human_syllable2[] =
{
	"a", "ae", "au", "ao", "are", "ale", "ali", "ay", "ardo", "e", "ei", "ea", "eri", "era", "ela", "eli", "enda", "erra", "i", "ia", "ie", "ire", "ira", "ila", "ili", "ira", "igo", "o", "oa", "oi", "oe", "ore", "u", "y",
};

static cptr human_syllable3[] =
{
	"a", "and", "b", "bwyn", "baen", "bard", "c", "ctred", "cred", "ch", "can", "d", "dan", "don", "der", "dric", "dfrid", "dus", "f", "g", "gord", "gan", "l", "li", "lgrin", "lin", "lith", "lath", "loth", "ld", "ldric", "ldan", "m", "mas", "mos", "mar", "mond", "n", "nydd", "nidd", "nnon", "nwan", "nyth", "nad", "nn", "nnor", "nd", "p", "r", "ron", "rd", "s", "sh", "seth", "sean", "t", "th", "tha", "tlan", "trem", "tram", "v", "vudd", "w", "wan", "win", "wyn", "wyr", "wyr", "wyth",
};

/* Orc */
static cptr orc_syllable1[] =
{
	"B", "Er", "G", "Gr", "H", "P", "Pr", "R", "V", "Vr", "T", "Tr", "M", "Dr",
};

static cptr orc_syllable2[] =
{
	"a", "i", "o", "oo", "u", "ui",
};

static cptr orc_syllable3[] =
{
	"dash", "dish", "dush", "gar", "gor", "gdush", "lo", "gdish", "k", "lg", "nak", "rag", "rbag", "rg", "rk", "ng", "nk", "rt", "ol", "urk", "shnak", "mog", "mak", "rak",
};

/* Klackon */
static cptr klackon_syllable1[] =
{
	"K'", "K", "Kri", "Kir", "Kiri", "Iriki", "Irik", "Karik", "Iri","Akri",
};

static cptr klackon_syllable2[] =
{
	"arak", "i", "iri", "ikki", "ki", "kiri","ikir","irak","arik","k'","r",
};

static cptr klackon_syllable3[] =
{
	"akkak", "ak", "ik", "ikkik", "irik", "arik", "kidik", "kii", "k","ki","riki","irk",
};

static cptr cthuloid_syllable1[] =
{
	"Cth","Az","Fth","Ts","Xo","Q'N","R'L","Ghata","L","Zz","Fl","Cl","S","Y",
};

static cptr cthuloid_syllable2[] =
{
	"nar","loi","ul","lu","noth","thon","ath","'N","rhy","oth","aza","agn","oa","og",
};

static cptr cthuloid_syllable3[] =
{
	"l","a","u","oa","oggua","oth","ath","aggua","lu","lo","loth","lotha","agn","axl",
};

typedef byte bc_type;

static void get_starting_skills(void);
static void get_hermetic_skills_randomly(void);
static bc_type get_hermetic_skills(void);
static void get_ahw_average(void);
static void get_money(bool randomly);
static s16b get_social_average(byte);
static void display_player_birth_details(void);
static void get_final(void);
static bool load_stat_set(bool);
static void roll_stats_auto(bool point_mod);
static void get_stats(void);

/* A macro to determine whether use_autoroller can be set and is set. */
#ifdef ALLOW_AUTOROLLER
#define USE_AUTOROLLER	use_autoroller
#else
#define USE_AUTOROLLER FALSE
#endif

/* Return codes for birth_choice() */
#define BC_OKAY	0
#define BC_ABORT	1
#define BC_RESTART	2

/*
 * Display the birth option menu, notice when certain options change.
 */
static bc_type birth_option(void)
{
	bool old_allow_quickstart = allow_quickstart;
	bool old_allow_pickstats = allow_pickstats;
	bool old_maximise_mode = maximise_mode;
	Term_save();
	do_cmd_options_aux(7, "Startup Options", NULL);
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
static bc_type birth_choice(int row, s16b max, cptr prompt, int *option, bool allow_abort)
{
	char c;
	while (1)
	{
		put_str(format("%s (%c-%c%s): ", prompt, I2A(0), rtoa(max-1), (allow_abort) ? " or ESCAPE to abort" : ""), row, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		else if (c == 'S') return BC_RESTART;
		else if (c == '?') do_cmd_help(syshelpfile);
		else if (c == ESCAPE && allow_abort) return BC_ABORT;
		else if (c == '=')
		{
			bc_type b = birth_option();
			if (allow_abort && b == BC_ABORT) return b;
			if (b == BC_RESTART) return b;
		}
		else
		{
			(*option) = ator(c);
			if (((*option) >= 0) && ((*option) < max)) return BC_OKAY;
			else bell();
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
	hack_chaos_feature = ((skill_set[SKILL_THAUMATURGY].value > 0) || (p_ptr->prace==RACE_BROO));

}

/*
 * Imagine you were wielding a whip...
 */
static void wield_weapons(bool wield)
{
	if (wield)
	{
		int k;

		if (p_ptr->ptemplate == TPL_SWASHBUCKLER)
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
	else	/* Reduce stat case */
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

/* Just in case */
#ifndef SUCCESS
#define SUCCESS 0
#define ERR_PARSE 1
#endif

/* Save the current stats */
static errr save_stats(void)
{
	FILE *fff;
	char buf[1024];
	byte i,l;

	/* Paranoia - check that there's something to save (the default entry does not count). */
	if (stat_default_total <= 1) return SUCCESS;

	/* Drop priv's */
	safe_setuid_drop();

	/* Find user-loc.prf. */
	path_build(buf, 1024, ANGBAND_DIR_USER, "user-loc.prf");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Append to the file */
	fff = my_fopen(buf, "a");

	/* Failure */
	if (!fff) return ERR_PARSE;

	/* Start dumping */
	fprintf(fff, "\n\n# Initial stat dump\n\n");

	for (i = 1; i < stat_default_total; i++)
				{
		stat_default_type *sd_ptr = &stat_default[i];
		fprintf(fff, "D:%c:%c:%c:%d", rtoa(sd_ptr->sex), rtoa(sd_ptr->race), rtoa(sd_ptr->template), maximise_mode);
					for (l = 0; l< A_MAX; l++)
			fprintf(fff, ":%d", sd_ptr->stat[l]);
		fprintf(fff, ":%s\n", quark_str(sd_ptr->name));
				}

	/* Close */
	my_fclose(fff);

	/* Grab priv's */
	safe_setuid_grab();

	Term_putstr(2,0,-1,TERM_GREEN,"Stats saved.");


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
	char b1 = '[';
	char b2 = ']';
	char modpts[4];
	cptr finstr = "'ESC' to finish, ";
	cptr arstr = " with minima";
	char attr;

	/* Display details if required. */
	if (details)
	{
		display_player(-1);
		display_player_birth_details();
	}
	else
	{
		display_player(0);
	}
	/* Display the information required during creation. */
	clear_from(23);
	sprintf(modpts,"%d",points);
		
	Term_putstr(73,2,-1,TERM_WHITE,"<-S/s->");
	Term_putstr(73,3,-1,TERM_WHITE,"<-I/i->");
	Term_putstr(73,4,-1,TERM_WHITE,"<-W/w->");
	Term_putstr(73,5,-1,TERM_WHITE,"<-D/d->");
	Term_putstr(73,6,-1,TERM_WHITE,"<-C/c->");
	Term_putstr(73,7,-1,TERM_WHITE,"<-H/h->");

	/* These should be the same as in display_player_misc_info() */
	Term_putstr(1,2,-1,TERM_WHITE,"<N>Name");
	Term_putstr(1,3,-1,TERM_WHITE,"<G>Sex"); /* Sigh... */
	Term_putstr(1,4,-1,TERM_WHITE,"<R>Race");
	Term_putstr(1,5,-1,TERM_WHITE,"<T>Template");


	/* Start the first string. */
	Term_putch(2, 21, TERM_WHITE, b1);

	/* Calculate point string, if used. */
	if (!spend_points)
	{
		attr = 0;
		if (!rolled) finstr = "";
	}
	else if (points == 0) attr = 'g';
	else if (points > 0) attr = 'y';
	else if (rolled) attr = 'B';
	else
	{
		/* Can't finish, so clear finstr. */
		attr = 'r';
		finstr = "";
	}

	/* Only mention the autoroller if allowed. */
	if (!USE_AUTOROLLER) arstr = "";

	/* Write the point string, if any. */
	if (attr) mc_roff(format(CC_PREFIX "%c%d" CC_PREFIX "w points left. ",
		attr, points));

	/* Write the rest of the first string. */
	mc_roff(format("Press %sX to restart,%c", finstr, b2));

	/* Write the second string. */
	prt("['f' to save, 'l' to load, '/' to change display, '=' for options,]",
		22, 2);

	/* Write the third string. */
	prt(format("['a' to roll%s, or '?' for help.]", arstr), 23, 2);
}

/* Just in case */
#ifndef ind_stat
#define ind_stat(X) \
	((X < 4) ? 0 : (X < 18) ? X-3 : (X < 18+220) ? 15+(X-18)/10 : 37)
#endif
/*
 * Display various things there isn't space for normally during character creation.
 * It could be argued that this would be better placed in files.c, but it's easier here.
 */
static void display_player_birth_details(void)
{
	byte i;

		/* Clear some space */
	for (i = 15; i < 20; i++)
	{
		Term_erase(0, i, 80);
	}

	put_str("(Miscellaneous Abilities)", 15, 25);

	for (i = 0; i < 12; i++)
	{
		byte r = i%4;
		byte c = i/4;
		cptr string, temp;

		/* Find the number. These must correspond with the strings above. */
		switch (i)
		{
			case 0:
			string = "Spells at 100%";
			temp = format("%d", adj_mag_study[ind_stat(p_ptr->stat_top[A_INT])]*25+1);
			break;
			case 1:
			string = "SP at 100%";
			temp = format("%d", adj_mag_mana[ind_stat(p_ptr->stat_top[A_INT])]*25+1);
			break;
			case 2:
			string = "Min spell fail";
			temp = format("%d%%", adj_mag_fail[ind_stat(p_ptr->stat_top[A_INT])]);
			break;
			case 3:
			string = "Min favour fail";
			temp = format("%d%%", adj_mag_fail[ind_stat(p_ptr->stat_top[A_CHR])]);
			break;
			case 4:
			string = "Chi at 100%";
			temp = format("%d", adj_mag_mana[ind_stat(p_ptr->stat_top[A_WIS])]*25+1);
			break;
			case 5:
			string = "Min mindcraft fail";
			temp = format("%d%%", adj_mag_fail[ind_stat(p_ptr->stat_top[A_WIS])]);
			break;
			case 6:
			string = "Saving throw bonus";
			temp = format("%d%%", adj_wis_sav[ind_stat(p_ptr->stat_top[A_WIS])]);
			break;
			case 7:
			string = "Disarming bonus";
			temp = format("%d", adj_dex_dis[ind_stat(p_ptr->stat_top[A_DEX])]+adj_int_dis[ind_stat(p_ptr->stat_top[A_INT])]);
			break;
			case 8:
			string = "Weight limit";
			temp = format("%d", adj_str_wgt[ind_stat(p_ptr->stat_top[A_STR])]*10);
			break;
			case 9:
			string = "Weapon weight limit";
			temp = format("%d", adj_str_hold[ind_stat(p_ptr->stat_top[A_STR])]);
			break;
			case 10:
			string = "Regeneration rate";
			temp = format("%d", adj_con_fix[ind_stat(p_ptr->stat_top[A_CON])]);
			break;
			case 11:
			string = "Theft avoidance";
			temp = format("%d%%", adj_dex_safe[ind_stat(p_ptr->stat_top[A_DEX])]);
			break;
			default:
			string = "Error!";
			temp = "Error!";
		}
		/* Insert the string. */
		put_str(string, 16+r, 1+27*c);

		/* Add a colon. */
		put_str(":", 16+r, 19+27*c);

		/* Insert the number. */
		c_put_str(TERM_L_GREEN, temp, 16+r, 21+27*c);
	}
}


/* Indexes for point_mod_player (0-6 hard-coded as stats and nothing) */
#define IDX_STATS	((A_STR+1) | (A_INT+1) | (A_WIS+1) | (A_DEX+1) | (A_CON+1) | (A_CHR+1))
#define IDX_RACE	0x0008
#define IDX_TEMPLATE	0x0010
#define IDX_FILE	0x0020
#define IDX_FINISH	0x0040
#define IDX_RAND_ONE	0x0080
#define IDX_LOAD	0x0100
#define IDX_START	0x0200
#define IDX_DETAILS	0x0400
#define IDX_NAME	0x0800
#define IDX_SEX	0x1000
#define IDX_OPTION	0x2000
#define IDX_HELP	0x4000
#define IDX_ROLL	0x8000
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
	char stat = UNREAD_VALUE; /* Never used when i = IDX_ALL, and initialised below otherwise. */
	s16b points = UNREAD_VALUE; /* Initialised when i = IDX_ALL */
	u16b i;

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
			do_cmd_help(syshelpfile);
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
				(void)add_stats(p_ptr->psex, p_ptr->prace, p_ptr->ptemplate, maximise_mode,
				p_ptr->stat_max[A_STR], p_ptr->stat_max[A_INT], p_ptr->stat_max[A_WIS],
				p_ptr->stat_max[A_DEX], p_ptr->stat_max[A_CON], p_ptr->stat_max[A_CHR],
				name);
				save_stats();
			}
		}
				
		/* Load saved stats at startup and on demand */
		if (i & IDX_LOAD)
		{
			if (!load_stat_set(i == IDX_LOAD)) return FALSE;
		}

		/* Modify the player's race. */
		if (i == IDX_RACE)
			{
				p_ptr->prace += (islower(stat) ? 1 : -1);
				p_ptr->prace %= MAX_RACES;
				rp_ptr = &race_info[p_ptr->prace];
			}

		/* Modify the player's template. */
		if (i == IDX_TEMPLATE)
			{
				p_ptr->ptemplate += (islower(stat) ? 1 : -1);
				p_ptr->ptemplate %= MAX_TEMPLATE;
				cp_ptr = &template_info[p_ptr->ptemplate];
			}

		/* Modify the player's sex. */
		if (i == IDX_SEX)
		{
			p_ptr->psex += (islower(stat) ? 1 : -1);
			p_ptr->psex += MAX_SEXES;
			p_ptr->psex %= MAX_SEXES;
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
			if (!own_name) create_random_name(p_ptr->prace,player_name);
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
			s16b dif = (islower(stat)) ? 1 : -1;
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

/*
 * Load a set of stats.
 *
 * If menu is true, the player wants a choice.
 */
static bool load_stat_set_aux(bool menu, s16b *temp_stat_default)
{
	int x;
	s16b y;

	/* Paranoia - there should be a default entry */
	if (!stat_default_total) return TRUE;

	/* Find a set of stats which match the current race and template
	 * Templates with maximise set to DEFAULT_STATS are acceptable for
	 * all race/template combinations.
	 * 
	 */
	for (x = 0, y = 0; x < stat_default_total; x++)
	{
		stat_default_type *sd_ptr = &stat_default[x];

		/* Don't load default stats without a race/template combination chosen. */
		if (x == DEFAULT_STATS && !p_ptr->prace) continue;
		
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
		if (!p_ptr->prace || 
			(sd_ptr->race == p_ptr->prace &&
			sd_ptr->template == p_ptr->ptemplate))
		{
			temp_stat_default[y++] = x;
		}
	}

	/* Don't do anything without a choice. */
	if (!y) return TRUE;

	/* Give the player the choices. */
	if (menu)
	{
		bc_type b;

		/* Start half-way down the screen if possible. */
		byte start = 14;

		/* Allow more room if we need to display a race and template. */
		byte width = (p_ptr->prace) ? 40 : 80;
		
		/* If not, start at the top. This should be enough... */
		if (y > 560/width) start = 0;

		clear_from(start);
		for (x = 0; x < y; x++)
		{
			stat_default_type *sd_ptr = &stat_default[temp_stat_default[x]];
			char buf[120];
			byte z;
			sprintf(buf, "%c) %s (", rtoa(x), quark_str(sd_ptr->name));

			/* If we're just starting, we need to know the race & template. */
			if (!p_ptr->prace)
			{
				sprintf(buf+strlen(buf), "%s %s %s) (", sex_info[sd_ptr->sex].title, race_info[sd_ptr->race].title, template_info[sd_ptr->template].title);
			}
			for (z = 0; z < A_MAX; z++)
			{
				byte w = sd_ptr->stat[z];
				char stat[32];
				if (sd_ptr->maximise)
				{
					w=modify_stat_value(w, race_info[sd_ptr->race].r_adj[z]+template_info[sd_ptr->template].c_adj[z]);
				}
				cnv_stat(w, stat);
				w = 0;
				while (stat[w] == ' ') w++;
				sprintf(buf+strlen(buf), "%s%s%s", (z) ? "," : "", stat+w, (z+1 < A_MAX) ? "" : ")");
			}

			/* Don't allow overly long entries. */
			buf[width] = '\0';

			if (width > 40)
			{
				put_str(buf, start+2+x, 0);
			}
			else if (x%2)
			{
				put_str(buf, start+2+x/2, 40);
			}
			else
			{
				put_str(buf, start+2+x/2, 0);
			}
		}
		/* Ask for a choice */
		b = birth_choice(start+1, y, "Choose a template", &x, TRUE);
		if (b == BC_ABORT)
		{
			x = -1;
		}
		else if (b == BC_RESTART)
		{
			return FALSE;
		}

		/* Finally clean up. */
		clear_from(start);
	}
	/* We're starting for the first time, so give the player the last set saved. */
	else if (!p_ptr->stat_cur[0])
	{
		x = y-1;
	}
	/* The player has already chosen stats, and hasn't asked to load new ones, so do nothing. */
	else
	{
		x = -1;
	}
	/* Something has been chosen, so copy everything across. */
	if (x != -1)
	{
		stat_default_type *sd_ptr = &stat_default[temp_stat_default[x]];
		if (!p_ptr->prace)
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
	return TRUE;
}

/*
 * A wrapper around the above to handle dynamic allocation.
 */
static bool load_stat_set(bool menu)
{
	C_TNEW(temp_stat_default, stat_default_total+1, s16b);
	bool returncode = load_stat_set_aux(menu, temp_stat_default);
	TFREE(temp_stat_default);
	return returncode;
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
static void get_starting_skills(void)
{
	int i;
	/* Wipe skills */
	for(i=0;i<MAX_SKILLS;i++)
	{
		skill_set[i].value=0;
		skill_set[i].experience=0;
		skill_set[i].base=0;
		skill_set[i].ceiling=100;
	}

	/* Wipe spirit associations */
	for (i=0;i<MAX_SPIRITS;i++)
	{
		spirits[i].pact = 0;
	}

	/* Now add some from template */
	skill_set[SKILL_CLOSE].value+=cp_ptr->skill[0];
	skill_set[SKILL_STAB].value+=cp_ptr->skill[1];
	skill_set[SKILL_SLASH].value+=cp_ptr->skill[2];
	skill_set[SKILL_CRUSH].value+=cp_ptr->skill[3];
	skill_set[SKILL_MISSILE].value+=cp_ptr->skill[4];
	skill_set[SKILL_TOUGH].value+=cp_ptr->skill[5];
	skill_set[SKILL_DEVICE].value+=cp_ptr->skill[6];
	skill_set[SKILL_DISARM].value+=cp_ptr->skill[7];
	skill_set[SKILL_PERCEPTION].value+=cp_ptr->skill[8];
	skill_set[SKILL_SAVE].value+=cp_ptr->skill[9];
	skill_set[SKILL_SEARCH].value+=cp_ptr->skill[10];
	skill_set[SKILL_STEALTH].value+=cp_ptr->skill[11];
	skill_set[SKILL_MA].value+=cp_ptr->skill[12];
	skill_set[SKILL_MINDCRAFTING].value+=cp_ptr->skill[13];
	skill_set[SKILL_CHI].value+=cp_ptr->skill[14];
	skill_set[SKILL_SHAMAN].value+=cp_ptr->skill[15];
	skill_set[SKILL_HEDGE].value+=cp_ptr->skill[16];
	skill_set[SKILL_MANA].value+=cp_ptr->skill[17];
	skill_set[SKILL_PSEUDOID].value+=cp_ptr->skill[18];

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
static void get_hermetic_skills_randomly()
{
	int i,choices;
	int choice,old_choice;

	choices = cp_ptr->choices;

	if (!choices) return;
	
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
static bc_type get_hermetic_skills()
{
	int k,i,choices;

	choices = cp_ptr->choices;

	if (!choices) return BC_OKAY;
	
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
		cptr buf = string_make(format("%d choic%s left. Choose a school or type", i,(i>1 ? "es":"e")));
		bc_type b = birth_choice(21, MAX_SCHOOL*2, buf, &k, TRUE);
		(void)string_free(buf);
		if (b) return b;

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
	return BC_OKAY;
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
	for (i = 0; i < A_MAX; i++)
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
	for (i = 0; i < A_MAX; i++)
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
	for (i = 0; i < A_MAX; i++)
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
	for (i = 0; i < A_MAX; i++)
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
 * Helper function for get_social_average()
 * This finds the the average social of a race for which rp_ptr->chart = chart.
 * Total is the size of the array of charts.
 * Oldseen is an array which indicates how often each chart has been used in the
 * path leading to the current one, generally 0 or 1.
 */
static s16b get_social_average_aux(byte *oldseen, byte chart, byte total)
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

		/* We have now seen this chart. */
		seen[chart] = TRUE;

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
static void birth_put_stats(void)
{
	int		i, p;
	byte	attr;

	char	buf[80];


	/* Put the stats (and percents) */
	for (i = 0; i < A_MAX; i++)
	{
		/* Put the stat */
		cnv_stat(p_ptr->stat_use[i], buf);
		c_put_str(TERM_L_GREEN, buf, 2 + i, 66);

		/* Put the percent */
		if (stat_match[i])
		{
			p = 1000L * stat_match[i] / auto_round;
			attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
			sprintf(buf, "%3d.%d%% ", p/10, p%10);
			c_put_str(attr, buf, 2 + i, 73);
		}

		/* Never happened */
		else
		{
			c_put_str(TERM_RED, "(NONE) ", 2 + i, 73);
		}
	}
}

/*
 *  Initialise the matrix of quests
 */
static void initialise_quests(void)
{
	int i,j;

	/* Start with no quests */
	for (i = 0; i < MAX_QUESTS; i++)
	{
		q_list[i].level = 0;
		q_list[i].r_idx = 0;
		q_list[i].cur_num = 0;
		q_list[i].cur_num_known = 0;
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
	cheat_item = FALSE;
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
 * Each player starts out with a few items, given as k_idx.
 * In addition, he always has some food and a few torches.
 */

static s16b player_init[MAX_TEMPLATE][3] =
{
	{
		/* Adventurer */
		OBJ_RING_RES_FEAR, /* Warriors need it! */
		OBJ_CUTLASS,
		OBJ_LUMP_OF_SULPHUR
	},

	{
		/* Swashbuckler */
		OBJ_POTION_SPEED,
		OBJ_RAPIER,
		OBJ_HARD_LEATHER_ARMOUR 
	},

	{
		/* Gladiator */
		OBJ_RING_FREE_ACTION,
		OBJ_BROAD_SWORD,
		OBJ_SMALL_METAL_SHIELD 
	},

	{
		/* Warrior Monk */
		OBJ_RING_SUSTAIN_DEX,
		OBJ_SCROLL_MONSTER_CONFUSION,
		OBJ_SOFT_LEATHER_ARMOUR 
	},

	{
		/* Zen Monk */
		OBJ_RING_SUSTAIN_WIS,
		OBJ_SOFT_LEATHER_ARMOUR,
		OBJ_SCROLL_MONSTER_CONFUSION 
	},

	{
		/* Assassin */
		OBJ_RING_RES_POISON,
		OBJ_DAGGER,
		OBJ_SOFT_LEATHER_ARMOUR 
    },

	{
        /* Ranger */
		OBJ_LONG_BOW,
		OBJ_ARROW,
		OBJ_HARD_LEATHER_ARMOUR 
    },

    {
        /* Shaman */
		OBJ_QUARTERSTAFF,
		OBJ_POTION_HEALING,
		OBJ_SCROLL_PROTECTION_FROM_EVIL 
	},

    {
        /* Mindcrafter */
		OBJ_RING_SUSTAIN_WIS,
		OBJ_SHORT_SWORD,
		OBJ_SOFT_LEATHER_ARMOUR,
    },

    {
        /* Wizard */
		OBJ_RING_SUSTAIN_INT,
		OBJ_POTION_RES_MANA,
		OBJ_SOFT_LEATHER_ARMOUR,
    },

	{
        /* Warlock */
		OBJ_RING_SUSTAIN_INT,
		OBJ_SMALL_SWORD,
		OBJ_SOFT_LEATHER_ARMOUR,

	},

	{
		/* Powerweaver */
		OBJ_RING_SUSTAIN_INT,
		OBJ_POTION_RES_MANA,
		OBJ_RING_SUSTAIN_WIS,
	},

	{
		/* Tourist */
		OBJ_DAGGER,
		OBJ_HARD_LEATHER_BOOTS,
		OBJ_CLOAK 
	},

};



/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i;

	object_type	forge;
	object_type	*q_ptr;
	

	/* Get local object */
	q_ptr = &forge;

    if (p_ptr->prace == RACE_GOLEM || p_ptr->prace == RACE_SKELETON ||
        p_ptr->prace == RACE_ZOMBIE || p_ptr->prace == RACE_VAMPIRE ||
        p_ptr->prace == RACE_SPECTRE)
    {
        /* Hack -- Give the player scrolls of satisfy hunger */
        object_prep(q_ptr, OBJ_SCROLL_SATISFY_HUNGER);
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
        object_prep(q_ptr, OBJ_RATION_OF_FOOD);
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
        object_prep(q_ptr, OBJ_SCROLL_LIGHT);
        q_ptr->number = (char)rand_range(3,7);
        object_aware(q_ptr);
        object_known(q_ptr);

        /* These objects are "storebought" */
        q_ptr->ident |= IDENT_STOREB;

        (void)inven_carry(q_ptr, FALSE);

        /* Get local object */
        q_ptr = &forge;

        /* Hack -- Give the player scrolls of DARKNESS! */
        object_prep(q_ptr, OBJ_SCROLL_DARKNESS);
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
        object_prep(q_ptr, OBJ_WOODEN_TORCH);
        q_ptr->number = (char)rand_range(3, 7);
        q_ptr->pval = (char)rand_range(3, 7) * 500;
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);
    }
    /* For characters starting with magical skill, give them a spellbook */
    if (skill_set[SKILL_MANA].value > 0) {
		int gbook[4], book[4] =
		{
			OBJ_SORCERY_BEGINNERS_HANDBOOK,
			OBJ_THAUMATURGY_SIGN_OF_CHAOS,
			OBJ_CONJURATION_MINOR_CONJURINGS,
			OBJ_NECROMANCY_BLACK_PRAYERS
		};

		q_ptr = &forge;
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
		(void)inven_carry(q_ptr, FALSE);
    }
        	 		

    /* Hack -- Give the player three useful objects */
    for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		s16b k = player_init[p_ptr->ptemplate][i];

        if (k == OBJ_RING_RES_FEAR &&
                 p_ptr->prace == RACE_BARBARIAN)
        /* Barbarians do not need a ring of resist fear */
                 k = OBJ_RING_SUSTAIN_STR;

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player an object */
		object_prep(q_ptr, k);


		if (k == OBJ_ARROW)
		{
			/* If we have an arrow, we need more than one */
			q_ptr->number = (char)rand_range(15,45);
		}

        /* Assassins begin the game with a poisoned dagger */
		if (k == OBJ_DAGGER && p_ptr->ptemplate == TPL_ASSASSIN)
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
static void player_birth_quests(void)
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
 * Finish off generation by adding a few random things.
 */
static void get_final(void)
{
	/* Roll for base hitpoints */
	get_extra();

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

	/* Roll for gold */
	get_money(TRUE);

	/* Hack -- get a chaos patron even if you are not chaotic (yet...) */
	p_ptr->chaos_patron = (randint(MAX_PATRON)) - 1;

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

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

	if (point_mod)
	{
		int i;
		for (i = 0; i < A_MAX; i++)
		{
			int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
			stat_limit[i] = adjust_stat(p_ptr->stat_max[i], bonus, FALSE);
		}
	}

	/* Auto-roll */
	while (1)
	{
		int i;
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
			birth_put_stats();
			/* Dump round if allowed. */
			put_str(format("%6ld", auto_round), y, x);
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
	int i, j, k, m;

	int mode = 0;

	bool flag = FALSE;
	bool prev = FALSE;

	cptr str;

	char c = UNREAD_VALUE;

	char b1 = '[';
	char b2 = ']';


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
		str = cp_ptr->title;

		if (rp_ptr->choice & (1L << p_ptr->ptemplate )) break;
	}
	/* Display */
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);
	
	/* Get skill values */
	get_starting_skills();
	get_hermetic_skills_randomly();
	get_init_spirit(FALSE);
	get_random_skills(TRUE);

	/* Get a random name */
	create_random_name(p_ptr->prace,player_name);
	/* Display */
	c_put_str(TERM_L_BLUE, player_name, 2, 13);

	/* Generate quests */
	/* Set max number of quest */
	MAX_Q_IDX =randint(20)+10+(2*MAX_CAVES);
	player_birth_quests();

#ifdef ALLOW_AUTOROLLER
	/* Initialize */
	if (USE_AUTOROLLER && !spend_points)
	{
		int mval[A_MAX];
		/* Clear fields */
		auto_round = 0L;
		last_round = 0L;
		/* Clean up */
		clear_from(10);
		/* Prompt for the minimum stats */
		put_str("Enter minimum attribute for: ", 15, 2);
		/* Output the maximum stats */
		for (i = 0; i < A_MAX; i++)
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
		for (i = 0; i < A_MAX; i++)
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
		if (USE_AUTOROLLER && !spend_points)
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
		while (USE_AUTOROLLER && !spend_points)
		{
			bool accept = TRUE;

			/* Get a new character */
			get_stats();

			/* Advance the round */
			auto_round++;

			/* Hack -- Prevent overflow */
			if (auto_round >= 1000000L) break;

			/* Check and count acceptable stats */
			for (i = 0; i < A_MAX; i++)
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
				birth_put_stats();

				/* Dump round */
				put_str(format("%6ld", auto_round), 9, 73);

				/* Make sure they see everything */
				Term_fresh();

				/* Delay 1/10 second */
				if (flag) Term_xtra(TERM_XTRA_DELAY, z_info->ar_delay);

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
		get_money(TRUE);

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
		if (!spend_points)
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
			p_ptr->energy = 1050; /* Should this be based on TURN_ENERGY? */

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
	int i, k, n;

	cptr str;

	char c;

	char p2 = ')';

	char buf[80];

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
		"special responses, including 'Q' to quit, '=' to change options");
	Term_putstr(5, 13, -1, TERM_WHITE,
		"or '?' for help.  Note that 'Q' and 'S' must be capitalized.");


	/*** Quick-Start ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Quick-Start gives you a completely random character without");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"further prompting.");

	/* Choose */
	while (1)
	{
		/* Unsetting allow_quickstart causes this prompt to be ignored. */
		if (!allow_quickstart)
		{
			c = 'n';
			break;
		}
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
			do_cmd_options_aux(7,"Startup Options", NULL);
			Term_load();
		}
		else bell();
	}

	/* Clean up */
	clear_from(15);
	
	if ((c == 'Y') || (c == 'y'))
	{
		return quick_start_character();
	}
	else /* Interactive character */
	{

		/*** Choose pre-set stat set. ***/
		if (allow_pickstats)
		{
			if (!load_stat_set(TRUE)) return FALSE;
		}


		/* We may have picked a race and template, so we shouldn't
		 * ask again. The stats selected do not currently carry forward
		 * into autoroller selections, but this should be easy to change.
		 */
		if (!p_ptr->prace)
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
			if (birth_choice(20, MAX_SEXES, "Choose a sex", &k, FALSE) == BC_RESTART) return FALSE;
	
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
	
				sprintf(buf, "%c%c %s", rtoa(n), p2, str);
				put_str(buf, 18 + (n/5), 2 + 15 * (n%5));
			}
	
			/* Choose */
			if (birth_choice(17, MAX_RACES, "Choose a race", &k, FALSE) == BC_RESTART) return FALSE;
	
			/* Set race */
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
	
			if (birth_choice(18, MAX_TEMPLATE, "Choose a template", &k, FALSE) == BC_RESTART) return FALSE;
	
			/* Set template */
			p_ptr->ptemplate = k;
			cp_ptr = &template_info[p_ptr->ptemplate];
			str = cp_ptr->title;
	
			/* Display */
			c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);
		}
		
		/* Clean up */
		clear_from(15);

		/* Generate quests */
		/* Set max number of quest */
		MAX_Q_IDX =randint(20)+10+(2*MAX_CAVES);
		player_birth_quests();

		/* Clean up */
		clear_from(10);


		p_ptr->muta1 = 0;
		p_ptr->muta2 = 0;
		p_ptr->muta3 = 0;

		/* Player is ready to move... */
		p_ptr->energy=1050; /* Should this be based on TURN_ENERGY? */

		/* Player has no recal ritual yet */
		p_ptr->ritual = MAX_TOWNS + 1;

		/* Player has no house yet */
		for(i=0;i<MAX_TOWNS;i++)
		{
			p_ptr->house[i] = 0;
		}

		/* Generate the character. */
		return point_mod_player();
	}
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



