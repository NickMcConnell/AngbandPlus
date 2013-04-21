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

*   Barbarian			  1 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Cyclops				 77 -> 109 -> 110 -> 111 -> 112
*   Dark Elf			 69 ->  70 ->  71 ->  72 ->  73
*	Devilspawn			129 -> 130 -> 131 -> 132 -> 133
*	Draconian			 89 ->  90 ->  91
*   Dwarf				 16 ->  17 ->  18 ->  57 ->  58 ->  59 ->  60 ->  61
*   Elf					  7 ->   8 ->   9 ->  54 ->  55 ->  56
*   Gnome				 13 ->  14 ->   3 ->  50 ->  51 ->  52 ->  53
*	Golem				 98 ->  99 -> 100 -> 101
*   Half-Elf			  4 ->   1 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Half-Giant			 75 ->  20 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Half-Ogre			 74 ->  20 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Half-Orc			 19 ->  20 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Half-Titan			 76 ->  20 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Half-Troll			 22 ->  23 ->  62 ->  63 ->  64 ->  65 ->  66
*   High Elf			  7 ->   8 ->   9 ->  54 ->  55 ->  56
*   Hobbit				 10 ->  11 ->   3 ->  50 ->  51 ->  52 ->  53
*   Human				  1 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*	Imp					 94 ->  95 ->  96 ->  97
*	Klackon				 84 ->  85 ->  86
*	Kobold				 82 ->  83 ->  24 ->  25 ->  26
*	Mind Flayer			 92 ->  93
*   Nephilim			 67 ->  68 ->  50 ->  51 ->  52 ->  53
*	Nibelung			 87 ->  88 ->  18 ->  57 ->  58 ->  59 ->  60 ->  61
*	Skeleton			102 -> 103 -> 104 -> 105 -> 106
*	Spectre				118 -> 119 -> 134 -> 121 -> 122 -> 123
*	Sprite				124 -> 125 -> 126 -> 127 -> 128
*	Vampire				113 -> 114 -> 115 -> 116 -> 117
*	Yeek				 78 ->  79 ->  80 ->  81
*	Zombie				107 -> 108 ->  62 ->  63 ->  64 ->  65 ->  66
*
* XXX XXX XXX This table *must* be correct or drastic errors may occur!
*/
static hist_type bg[] =
{
	/* 1 = Barbarian/Half-Elf/Human */
	{"You are the illegitimate and unacknowledged child ",	 10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ",	 20, 1, 2, 35},
	{"You are one of several children ",             95, 1, 2, 45},
	{"You are the first child ",				100, 1, 2, 50},

	/* 2 = Barbarian/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Human */
	{"of a Serf.  ",						 40, 2, 3, 65},
	{"of a Yeoman.  ",						 65, 2, 3, 80},
	{"of a Townsman.  ",					 80, 2, 3, 90},
	{"of a Guildsman.  ",					 90, 2, 3, 105},
	{"of a Landed Knight.  ",					 96, 2, 3, 120},
	{"of a Noble Family.  ",    99, 2, 3, 130},
	{"of the Royal Blood Line.  ",                100, 2, 3, 140},

	/* 3 = Barbarian/Gnome/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Hobbit/Human */
	{"You are the black sheep of the family.  ",		 20, 3, 50, 20},
	{"You are a credit to the family.  ",			 80, 3, 50, 55},
	{"You are a well liked child.  ",				100, 3, 50, 60},

	/* 4 = Half-Elf */
	{"Your mother was of the Teleri.  ",			 40, 4, 1, 50},
	{"Your father was of the Teleri.  ",			 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ",		 	 90, 4, 1, 55},
	{"Your father was of the Noldor.  ",		 	 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ",			 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ",			100, 4, 1, 70},

	/* 7 = Elf/High Elf */
	{"You are one of several children ",			 60, 7, 8, 50},
	{"You are the only child ",					100, 7, 8, 55},

	/* 8 = Elf/High Elf */
	{"of a Teleri ",						 75, 8, 9, 50},
	{"of a Noldor ",						 95, 8, 9, 55},
	{"of a Vanyar ",						100, 8, 9, 60},

	/* 9 = Elf/High Elf */
	{"Ranger.  ",						 40, 9, 54, 80},
	{"Archer.  ",						 70, 9, 54, 90},
	{"Warrior.  ",						 87, 9, 54, 110},
	{"Mage.  ",							 95, 9, 54, 125},
	{"Prince.  ",						 99, 9, 54, 140},
	{"King.  ",							100, 9, 54, 145},

	/* 10 = Hobbit */
	{"You are one of several children of a Hobbit ",		 85, 10, 11, 45},
	{"You are the only child of a Hobbit ",		        100, 10, 11, 55},

	/* 11 = Hobbit */
	{"Bum.  ",							 20, 11, 3, 55},
	{"Tavern Owner.  ",						 30, 11, 3, 80},
	{"Miller.  ",						 40, 11, 3, 90},
	{"Home Owner.  ",						 50, 11, 3, 100},
	{"Burglar.  ",						 80, 11, 3, 110},
	{"Warrior.  ",						 95, 11, 3, 115},
	{"Mage.  ",							 99, 11, 3, 125},
	{"Clan Elder.  ",						100, 11, 3, 140},

	/* 13 = Gnome */
	{"You are one of several children of a Gnome ",		 85, 13, 14, 45},
	{"You are the only child of a Gnome ",			100, 13, 14, 55},

	/* 14 = Gnome */
	{"Beggar.  ",						 20, 14, 3, 55},
	{"Braggart.  ",						 50, 14, 3, 70},
	{"Prankster.  ",						 75, 14, 3, 85},
	{"Warrior.  ",						 95, 14, 3, 100},
	{"Mage.  ",							100, 14, 3, 125},

	/* 16 = Dwarf */
	{"You are one of two children of a Dwarven ",		 25, 16, 17, 40},
	{"You are the only child of a Dwarven ",			100, 16, 17, 50},

	/* 17 = Dwarf */
	{"Thief.  ",						 10, 17, 18, 60},
	{"Prison Guard.  ",						 25, 17, 18, 75},
	{"Miner.  ",						 75, 17, 18, 90},
	{"Warrior.  ",						 90, 17, 18, 110},
	{"Priest.  ",						 99, 17, 18, 130},
	{"King.  ",							100, 17, 18, 150},

	/* 18 = Dwarf/Nibelung */
	{"You are the black sheep of the family.  ",		 15, 18, 57, 10},
	{"You are a credit to the family.  ",			 85, 18, 57, 50},
	{"You are a well liked child.  ",				100, 18, 57, 55},

	/* 19 = Half-Orc */
	{"Your mother was an Orc, but it is unacknowledged.  ",	 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ",	100, 19, 20, 25},

	/* 20 = Half-Giant/Half-Ogre/Half-Orc/Half-Titan */
	{"You are the adopted child ",				100, 20, 2, 50},

	/* 22 = Half-Troll */
	{"Your mother was a Cave-Troll ",				 30, 22, 23, 20},
	{"Your father was a Cave-Troll ",				 60, 22, 23, 25},
	{"Your mother was a Hill-Troll ",				 75, 22, 23, 30},
	{"Your father was a Hill-Troll ",				 90, 22, 23, 35},
	{"Your mother was a Water-Troll ",				 95, 22, 23, 40},
	{"Your father was a Water-Troll ",				100, 22, 23, 45},

	/* 23 = Half-Troll */
	{"Cook.  ",							  5, 23, 62, 60},
	{"Warrior.  ",						 95, 23, 62, 55},
	{"Priest.  ",						 99, 23, 62, 65},
	{"Clan Chief.  ",						100, 23, 62, 80},

	/* 24 = Kobold */
	{"You have green scales, ",    25, 24, 25, 50 },
	{"You have dark green scales, ",    50, 24, 25, 50 },
	{"You have yellow scales, ",    75, 24, 25, 50 },
	{"You have green scales, a yellow belly, ",    100, 24, 25, 50 },

	/* 25 = Kobold */
	{"bright eyes, ",    25, 25, 26, 50 },
	{"yellow eyes, ",    50, 25, 26, 50 },
	{"red eyes, ",    75, 25, 26, 50 },
	{"snake-like eyes, ",    100, 25, 26, 50 },

	/* 26 = Kobold */
	{"and a long sinuous tail.",        20, 26, 0, 50 },
	{"and a short tail.",        40, 26, 0, 50 },
	{"and a muscular tail.",        60, 26, 0, 50 },
	{"and a long tail.",        80, 26, 0, 50 },
	{"and a sinuous tail.",        100, 26, 0, 50 },


	/* 50 = Barbarian/Gnome/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Hobbit/Human/Nephilim */
	{"You have dark brown eyes, ",				 20, 50, 51, 50},
	{"You have brown eyes, ",					 60, 50, 51, 50},
	{"You have hazel eyes, ",					 70, 50, 51, 50},
	{"You have green eyes, ",					 80, 50, 51, 50},
	{"You have blue eyes, ",					 90, 50, 51, 50},
	{"You have blue-gray eyes, ",				100, 50, 51, 50},

	/* 51 = Barbarian/Gnome/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Hobbit/Human/Nephilim */
	{"straight ",						 70, 51, 52, 50},
	{"wavy ",							 90, 51, 52, 50},
	{"curly ",							100, 51, 52, 50},

	/* 52 = Barbarian/Gnome/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Hobbit/Human/Nephilim */
	{"black hair, ",						 30, 52, 53, 50},
	{"brown hair, ",						 70, 52, 53, 50},
	{"auburn hair, ",						 80, 52, 53, 50},
	{"red hair, ",						 90, 52, 53, 50},
	{"blond hair, ",						100, 52, 53, 50},

	/* 53 = Barbarian/Gnome/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Hobbit/Human/Nephilim */
	{"and a very dark complexion.",				 10, 53, 0, 50},
	{"and a dark complexion.",					 30, 53, 0, 50},
	{"and an average complexion.",				 80, 53, 0, 50},
	{"and a fair complexion.",					 90, 53, 0, 50},
	{"and a very fair complexion.",				100, 53, 0, 50},

	/* 54 = Elf/High Elf */
	{"You have light grey eyes, ",				 85, 54, 55, 50},
	{"You have light blue eyes, ",				 95, 54, 55, 50},
	{"You have light green eyes, ",				100, 54, 55, 50},

	/* 55 = Elf/High Elf */
	{"straight ",						 75, 55, 56, 50},
	{"wavy ",							100, 55, 56, 50},

	/* 56 = Elf/High Elf */
	{"black hair, and a fair complexion.",			 75, 56, 0, 50},
	{"brown hair, and a fair complexion.",			 85, 56, 0, 50},
	{"blond hair, and a fair complexion.",			 95, 56, 0, 50},
	{"silver hair, and a fair complexion.",			100, 56, 0, 50},

	/* 57 = Dwarf/Nibelung */
	{"You have dark brown eyes, ",				 99, 57, 58, 50},
	{"You have glowing red eyes, ",				100, 57, 58, 60},

	/* 58 = Dwarf/Nibelung */
	{"straight ",						 90, 58, 59, 50},
	{"wavy ",							100, 58, 59, 50},

	/* 59 = Dwarf/Nibelung */
	{"black hair, ",						 75, 59, 60, 50},
	{"brown hair, ",						100, 59, 60, 50},

	/* 60 = Dwarf/Nibelung */
	{"a one foot beard, ",					 25, 60, 61, 50},
	{"a two foot beard, ",					 60, 60, 61, 51},
	{"a three foot beard, ",					 90, 60, 61, 53},
	{"a four foot beard, ",					100, 60, 61, 55},

	/* 61 = Dwarf/Nibelung */
	{"and a dark complexion.",					100, 61, 0, 50},

	/* 62 = Half-Troll/Zombie */
	{"You have slime green eyes, ",				 60, 62, 63, 50},
	{"You have puke yellow eyes, ",				 85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ",				 99, 62, 63, 50},
	{"You have glowing red eyes, ",				100, 62, 63, 55},

	/* 63 = Half-Troll/Zombie */
	{"dirty ",							 33, 63, 64, 50},
	{"mangy ",							 66, 63, 64, 50},
	{"oily ",							100, 63, 64, 50},

	/* 64 = Half-Troll/Zombie */
	{"sea-weed green hair, ",					 33, 64, 65, 50},
	{"bright red hair, ",					 66, 64, 65, 50},
	{"dark purple hair, ",					100, 64, 65, 50},

	/* 65 = Half-Troll/Zombie/Yeek */
	{"and green ",						 25, 65, 66, 50},
	{"and blue ",						 50, 65, 66, 50},
	{"and white ",						 75, 65, 66, 50},
	{"and black ",						100, 65, 66, 50},

	/* 66 = Half-Troll/Zombie/Yeek */
	{"ulcerous skin.",						 33, 66, 0, 50},
	{"scabby skin.",						 66, 66, 0, 50},
	{"leprous skin.",                       100, 66, 0, 50},

	/* 67 = Nephilim */
	{"You are an unacknowledged child of ", 50, 67, 68, 45},
	{"You are a rebel child of ",         80, 67, 68, 65},
	{"You are a long lost child of ",     100, 67, 68, 55},

	/* 68 = Nephilim */
	{"someone with angel blood.  ",               50, 68, 50, 80 },
	{"an unknown child of an angel.  ", 65, 68, 50, 90 },
	{"an unknown angel.  ", 79, 68, 50, 100 },
	{"Gabriel.  ",       80, 68, 50, 130 },
	{"Michael.  ",        83, 68, 50, 105 },
	{"Uriel.  ",       84, 68, 50, 105 },
	{"Azrael.  ",        85, 68, 50, 90 },
	{"Samuel.  ",        87, 68, 50, 100 },
	{"Shamael.  ",       88, 68, 50, 125 },
	{"Abariel.  ",      89, 68, 50, 120 },
	{"Raphael.  ",       90, 68, 50, 140 },
	{"Remiel.  ",     91, 68, 50, 115 },
	{"Phanuel.  ",       92, 68, 50, 110 },
	{"Jehoel.  ",       93, 68, 50, 105 },
	{"Orfiel.  ",        94, 68, 50, 95 },
	{"Zamael.  ",        95, 68, 50, 115 },
	{"Hanael.  ",        96, 68, 50, 110 },
	{"Barchiel.  ",         97, 68, 50, 135 },
	{"Ambriel.  ",      98, 68, 50, 90 },
	{"Anael.  ",       99, 68, 50, 105 },
	{"Danael.  ",       100, 68, 50, 80 },

	/* 69 = Dark Elf */
	{"You are one of several children of a Dark Elven ",      85, 69, 70, 45},
	{"You are the only child of a Dark Elven ",          100, 69, 70, 55},

	/* 70 = Dark Elf */
	{"Warrior.  ", 50, 70, 71, 60 },
	{"Warlock.  ", 80, 70, 71, 75 },
	{"Noble.  ", 100, 70, 71, 95 },

	/* 71 = Dark Elf */
	{"You have black eyes, ", 100, 71, 72, 50},

	/* 72 = Dark Elf */
	{"straight ",                        70, 72, 73, 50},
	{"wavy ",                            90, 72, 73, 50},
	{"curly ",                          100, 72, 73, 50},

	/* 73 = Dark Elf */
	{"black hair and a very dark complexion.", 100, 73, 0, 50 },

	/* 74 = Half-Ogre */
	{"Your mother was an Ogre, but it is unacknowledged.  ", 25, 74, 20, 25},
	{"Your father was an Ogre, but it is unacknowledged.  ", 100, 74, 20, 25},

	/* 75 = Half-Hiant */
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

	/* 76 = Half-Titan */
	{"Your father was an unknown Titan.  ", 75, 76, 20, 50 },
	{"Your mother was Themis.  ",        80, 76, 20, 100 },
	{"Your mother was Mnemosyne.  ",     85, 76, 20, 100 },
	{"Your father was Okeanoas.  ",      90, 76, 20, 100 },
	{"Your father was Crius.  ",         95, 76, 20, 100 },
	{"Your father was Hyperion.  ",      98, 76, 20, 125 },
	{"Your father was Kronos.  ",       100, 76, 20, 150 },

	/* 77 = Cyclops */
	{"You are the offspring of an unknown Cyclops.  ", 90, 77, 109, 50 },
	{"You are Polyphemos's child.  ", 98, 77, 109, 80 },
	{"You are Uranos's child.  ", 100, 77, 109, 135 },

	/* 78 = Yeek */
	{"You are one of several children of ", 100, 78, 79, 50 },

	/* 79 = Yeek */
	{"a Brown Yeek. ", 50, 79, 80, 50 },
	{"a Blue Yeek.  ", 75, 79, 80, 50 },
	{"a Master Yeek.  ", 95, 79, 80, 85 },
	{"Boldor, the King of the Yeeks.  ", 100, 79, 80, 120 },

	/* 80 = Yeek */
	{"You have pale eyes, ",    25, 80, 81, 50 },
	{"You have glowing eyes, ",    50, 80, 81, 50 },
	{"You have tiny black eyes, ",    75, 80, 81, 50 },
	{"You have shining black eyes, ",    100, 80, 81, 50 },

	/* 81 = Yeek */
	{"and short blue fur.",        20, 81, 0, 50 },
	{"and short brown fur.",        40, 81, 0, 50 },
	{"and short black fur.",        60, 81, 0, 50 },
	{"and long blue fur.",        80, 81, 0, 50 },
	{"and long brown fur.",        100, 81, 0, 50 },

	/* 82 = Kobold */
	{"You are one of several children of ", 100, 82, 83, 50 },

	/* 83 = Kobold */
	{"a Small Kobold.  ",   40, 83, 24, 50 },
	{"a Kobold.  ",         75, 83, 24, 55 },
	{"a Large Kobold.  ",   95, 83, 24, 65 },
	{"Vort, the Kobold Queen.  ",     100, 83, 24, 100 },

	/* 84 = Klackon */
	{"You are one of several children of a Klackon hive queen.  "
	, 100, 84, 85, 50 },

	/* 85 = Klackon */
	{"You have red skin, ", 40, 85, 86, 50 },
	{"You have black skin, ", 90, 85, 86, 50 },
	{"You have yellow skin, ", 100, 85, 86, 50 },

	/* 86 = Klackon */
	{"and black eyes.", 100, 86, 0, 50 },

	/* 87 = Nibelung */
	{"You are one of several children of ", 100, 87, 88, 89 },

	/* 88 = Nibelung */
	{"a Nibelung Slave.  ", 30, 88, 18, 20 },
	{"a Nibelung Thief.  ", 50, 88, 18, 40 },
	{"a Nibelung Smith.  ", 70, 88, 18, 60 },
	{"a Nibelung Miner.  ", 90, 88, 18, 75 },
	{"a Nibelung Priest.  ", 95, 88, 18, 100 },
	{"Mime, the Nibelung.  ", 100, 88, 18, 100 },

	/* 89 = Draconian */
	{"You are one of several children of a Draconian ", 85, 89, 90, 50  },
	{"You are the only child of a Draconian ", 100, 89, 90, 55 },

	/* 90 = Draconian */
	{"Warrior.  ", 50, 90, 91, 50 },
	{"Priest.  ", 65, 90, 91, 65 },
	{"Mage.  ", 85, 90, 91, 70 },
	{"Noble.  ", 100, 90, 91, 100 },

	/* 91 = Draconian */
	{"You have green wings, green skin and yellow belly.", 30, 91, 0, 50 },
	{"You have green wings, and green skin.", 55, 91, 0, 50 },
	{"You have red wings, and red skin.", 80, 91, 0, 50 },
	{"You have black wings, and black skin.", 90, 91, 0, 50 },
	{"You have metallic skin, and shining wings.", 100, 91, 0, 50},

	/* 92 = Mind Flayer */
	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80 },

	/* 93 = Mind Flayer */
	{"three tentacles around your mouth.", 20, 93, 0, 45 },
	{"four tentacles around your mouth.", 80, 93, 0, 50 },
	{"five tentacles around your mouth.", 100, 93, 0, 55 },

	/* 94 = Imp */
	{"You ancestor was ", 100, 94, 95, 50 },

	/* 95 = Imp */
	{"a mindless demonic spawn.  ", 30, 95, 96, 20 },
	{"a minor demon.  ", 60, 95, 96, 50 },
	{"a major demon.  ", 90, 95, 96, 75 },
	{"a demon lord.  ", 100, 95, 96, 99 },

	/* 96 = Imp */
	{"You have red skin, ", 50, 96, 97, 50 },
	{"You have brown skin, ", 100, 96, 97, 50},

	/* 97 = Imp */
	{"claws, fangs, spikes, and glowing red eyes.", 40, 97, 0, 50 },
	{"claws, fangs, and glowing red eyes.", 70, 97, 0, 50 },
	{"claws, and glowing red eyes.", 100, 97, 0, 50 },

	/* 98 = Golem */
	{"You were shaped from ", 100, 98, 99, 50 },

	/* 99 = Golem */
	{"clay ", 40, 99, 100, 50 },
	{"stone ", 80, 99, 100, 50 },
	{"wood ", 85, 99, 100, 40 },
	{"iron ", 99, 99, 100, 50 },
	{"pure gold ", 100, 99, 100, 100},

	/* 100 = Golem */
	{"by a Kabbalist", 40, 100, 101, 50 },
	{"by a Wizard", 65, 100, 101, 50 },
	{"by an Alchemist", 90, 100, 101, 50},
	{"by a Priest", 100, 100, 101, 60},

	/* 101 = Golem */
	{" to fight evil.", 10, 101, 0, 65 },
	{".", 100, 101, 0, 50 },

	/* 102 = Skeleton */
	{"You were created by ", 100, 102, 103, 50 },

	/* 103 = Skeleton */
	{"a Necromancer.  ", 30, 103, 104, 50 },
	{"a magical experiment.  ", 50, 103, 104, 50 },
	{"an Evil Priest.  ", 70, 103, 104, 50 },
	{"a pact with the demons.  ", 75, 103, 104, 50 },
	{"a restless spirit.  ", 85, 103, 104, 50 },
	{"a curse.  ", 95, 103, 104, 30 },
	{"an oath.  ", 100, 103, 104, 50 },

	/* 104 = Skeleton */
	{"You have ", 100, 104, 105, 50 },

	/* 105 = Skeleton */
	{"dirty, dry bones, ", 40, 105, 106, 50 },
	{"rotten black bones, ", 60, 105, 106, 50 },
	{"filthy, brown bones, ", 80, 105, 106, 50 },
	{"shining white bones, ", 100, 105, 106, 50 },

	/* 106 = Skeleton */
	{"and glowing eyes.", 30, 106, 0, 50 },
	{"and eyes which burn with hellfire.", 50, 106, 0, 50 },
	{"and empty eyesockets.", 100, 106, 0, 50 },

	/* 107 = Zombie */
	{"You were created by ", 100, 107, 108, 50 },

	/* 107 = Zombie */
	{"a Necromancer.  ", 30, 108, 62, 50 },
	{"a Wizard.  ", 50, 108, 62, 50 },
	{"a restless spirit.  ",60, 108, 62, 50 },
	{"an Evil Priest.  ", 70, 108, 62, 50 },
	{"a pact with the demons.  ", 80, 108, 62, 50 },
	{"a curse.  ", 95, 108, 62, 30 },
	{"an oath.  ", 100, 108, 62, 50 },

	/* 109 = Cyclops */
	{"You have a dark brown eye, ",               20, 109, 110, 50},
	{"You have a brown eye, ",                    60, 109, 110, 50},
	{"You have a hazel eye, ",                    70, 109, 110, 50},
	{"You have a green eye, ",                    80, 109, 110, 50},
	{"You have a blue eye, ",                     90, 109, 110, 50},
	{"You have a blue-gray eye, ",               100, 109, 110, 50},

	/* 110 = Cyclops */
	{"straight ",                        70, 110, 111, 50},
	{"wavy ",                            90, 110, 111, 50},
	{"curly ",                          100, 110, 111, 50},

	/* 111 = Cyclops */
	{"black hair, ",                         30, 111, 112, 50},
	{"brown hair, ",                         70, 111, 112, 50},
	{"auburn hair, ",                        80, 111, 112, 50},
	{"red hair, ",                       90, 111, 112, 50},
	{"blond hair, ",                        100, 111, 112, 50},

	/* 112 = Cyclops */
	{"and a very dark complexion.",              10, 112, 0, 50},
	{"and a dark complexion.",                   30, 112, 0, 50},
	{"and an average complexion.",               80, 112, 0, 50},
	{"and a fair complexion.",                   90, 112, 0, 50},
	{"and a very fair complexion.",             100, 112, 0, 50},

	/* 113 = Vampire */
	{"You arose from an unmarked grave.  ", 20, 113, 114, 50 },
	{"In life you were a simple peasant, the victim of a powerful Vampire Lord.  ", 40, 109, 110, 50 },
	{"In life you were a Vampire Hunter, but they got you.  ", 60, 113, 114, 50 },
	{"In life you were a Necromancer.  ", 80, 113, 114, 50 },
	{"In life you were a powerful noble.  ", 95, 113, 114, 50 },
	{"In life you were a powerful and cruel tyrant.  ", 100, 113, 114, 50 },

	/* 114 = Vampire */
	{"You have ", 100, 114, 115, 50 },

	/* 115 = Vampire */
	{"jet-black hair, ", 25, 115, 116, 50 },
	{"matted brown hair, ", 50, 115, 116, 50 },
	{"white hair, ", 75, 115, 116, 50 },
	{"a hairless head, ", 100, 115, 116, 50 },

	/* 116 = Vampire */
	{"eyes like red coals, ", 25, 116, 117, 50 },
	{"blank white eyes, ", 50, 116, 117, 50 },
	{"feral yellow eyes, ", 75, 116, 117, 50 },
	{"bloodshot red eyes, ", 100, 116, 117, 50 },

	/* 117 = Vampire */
	{"and a deathly pale complexion.", 100, 117, 0, 50 },

	/* 118 = Spectre */
	{"You were created by ", 100, 118, 119, 50 },

	/* 119 = Spectre */
	{"a Necromancer.  ", 30, 119, 134, 50 },
	{"a magical experiment.  ", 50, 119, 134, 50 },
	{"an Evil Priest.  ", 70, 119, 134, 50 },
	{"a pact with the demons.  ", 75, 119, 134, 50 },
	{"a restless spirit.  ", 85, 119, 134, 50 },
	{"a curse.  ", 95, 119, 134, 30 },
	{"an oath.  ", 100, 119, 134, 50 },

	/* 121 = Spectre */
	{"eyes like red coals, ", 25, 121, 122, 50 },
	{"blank white eyes, ", 50, 121, 122, 50 },
	{"feral yellow eyes, ", 75, 121, 122, 50 },
	{"bloodshot red eyes, ", 100, 121, 122, 50 },

	/* 122 = Spectre */
	{" and a deathly gray complexion. ", 100, 122, 123, 50 },

	/* 123 = Spectre */
	{"An eerie green aura surrounds you.", 100, 123, 0, 50 },

	/* 124 = Sprite */
	{"Your parents were ", 100, 124, 125, 50 },

	/* 125 = Sprite */
	{"pixies.  ", 20, 125, 126, 35 },
	{"nixies.  ", 30, 125, 126, 25 },
	{"wood sprites.  ", 75, 125, 126, 50 },
	{"wood spirits.  ", 90, 125, 126, 75 },
	{"noble faerie folk.  ", 100, 125, 126, 85 },

	/* 126 = Sprite */
	{"You have light blue wings attached to your back, ", 100, 126, 127, 50 },

	/* 127 = Sprite */
	{"straight blond hair, ",                        80, 127, 128, 50},
	{"wavy blond hair, ",                            100, 127, 128, 50},

	/* 128 = Sprite */
	{"blue eyes, and a very fair complexion.", 100, 128, 0, 50},

	/* 129 = Devilspawn */
	{"Your mother was a succubus.  ", 30, 129, 130, 40},
	{"Your father was an incubus.  ",	50, 129, 130, 50 },
	{"Your mother was Glaryssa, the Succubus Queen.  ",	60, 129, 130, 60 },
	{"You are one of the Lilim, a descendant of Lilith.  ", 75, 129, 130, 50},
	{"You are a descendant of a Devil Prince.  ", 100, 129, 130, 30},

	/* 130 = Devilspawn */
	{"You have coal black eyes, ",              60, 130, 131, 50},
	{"You have pale pink eyes, ",              85, 130, 131, 50},
	{"You have eyes like red embers, ",               99, 130, 131, 50},
	{"You have beautiful blue eyes, ",             100, 130, 131, 55},

	/* 131 = Devilspawn */
	{"no hair at all, ",                 10, 131, 133, 50 },
	{"dirty ",                           33, 131, 132, 50},
	{"mangy ",                           66, 131, 132, 50},
	{"fine ",                           100, 131, 132, 50},

	/* 132 = Devilspawn */
	{"brown hair, ",                    33, 132, 133, 50},
	{"gray hair, ",                    66, 132, 133, 50},
	{"albino hair, ",                  100, 132, 133, 50},

	/* 133 = Devilspawn */
	{"and the hooves of a goat.",      50, 133, 0, 50 },
	{"and vestigial wings.",        75, 133, 0, 50 },
	{"and slightly clawed fingers.",       85, 133, 0, 50 },
	{"and an unnaturally wide smile.",    90, 133, 0, 50 },
	{"and a slight sulphurous smell about you.",       95, 133, 0, 50 },
	{"and bright red skin.",       97, 133, 0, 50 },
	{"and a forked tongue.",       100, 133, 0, 50 },

	/* 134 = Spectre */
	{"You have ", 100, 134, 121, 50 },

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

static char *angel_syllable1[] =
{
	"Sa","A","U","Mi","Ra","Ana","Pa","Lu","She","Ga","Da","O","Pe","Lau",
};

static char *angel_syllable2[] =
{
	"br","m","l","z","zr","mm","mr","r","ral","ch","zaz","tr","n","lar",
};

static char *angel_syllable3[] =
{
	"iel","ial","ael","ubim","aphon","iel","ael",
};

static char *illithid_syllable1[] =
{
	"Cth","Az","Fth","Ts","Xo","Q'N","R'L","Ghata","L","Zz","Fl","Cl","S","Y",
};

static char *illithid_syllable2[] =
{
	"nar","loi","ul","lu","noth","thon","ath","'N","rhy","oth","aza","agn","oa","og",
};

static char *illithid_syllable3[] =
{
	"l","a","u","oa","oggua","oth","ath","aggua","lu","lo","loth","lotha","agn","axl",
};

static char *yeek_syllable1[] =
{
	"Y","Ye","Yee","Y",
};

static char *yeek_syllable2[] =
{
	"ee","eee","ee","ee-ee","ee'ee","'ee","eee","ee","ee",
};

static char *yeek_syllable3[] =
{
	"k","k","k","ek","eek","ek",
};


/* Allow player to modify the character by spending points */
static bool point_mod_player(void)
{
	char b1 = '[';
	char b2 = ']';
	char stat;
	char modpts[4] = "none";
	/*char *buf = "zero";*/
	/*int tmp = 0;*/
	/*int hp = 0;*/
	/*int addhp = 0; */
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
		strcpy(name, hobbit_syllable1[rand_int(sizeof(hobbit_syllable1) / sizeof(char*))]);
		strcat(name, hobbit_syllable2[rand_int(sizeof(hobbit_syllable2) / sizeof(char*))]);
		strcat(name, hobbit_syllable3[rand_int(sizeof(hobbit_syllable3) / sizeof(char*))]);
		break;
	case RACE_YEEK:
		strcpy(name, yeek_syllable1[rand_int(sizeof(yeek_syllable1) / sizeof(char*))]);
		strcat(name, yeek_syllable2[rand_int(sizeof(yeek_syllable2) / sizeof(char*))]);
		strcat(name, yeek_syllable3[rand_int(sizeof(yeek_syllable3) / sizeof(char*))]);
		break;
	case RACE_BARBARIAN:
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
	case RACE_MIND_FLAYER:
		strcpy(name, illithid_syllable1[rand_int(sizeof(illithid_syllable1) / sizeof(char*))]);
		strcat(name, illithid_syllable2[rand_int(sizeof(illithid_syllable2) / sizeof(char*))]);
		strcat(name, illithid_syllable3[rand_int(sizeof(illithid_syllable3) / sizeof(char*))]);
		break;
	case RACE_NEPHILIM:
	case RACE_IMP:
	case RACE_DEVILSPAWN:
		strcpy(name, angel_syllable1[rand_int(sizeof(angel_syllable1) / sizeof(char*))]);
		strcat(name, angel_syllable2[rand_int(sizeof(angel_syllable2) / sizeof(char*))]);
		strcat(name, angel_syllable3[rand_int(sizeof(angel_syllable3) / sizeof(char*))]);
		break;
		/* Create an empty name */
	default:
		name[0] = '\0';
		break;
	}
}

byte choose_realm(byte choices)
{
	int picks[MAX_REALM] = {0};
	int k, n;
	char c;
	char p2 = ')';
	char buf[80];
	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Life, Corporeal and Sorcery are protective. Chaos and Death");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"are destructive. Nature has defensive and offensive spells.");
	Term_putstr(5,17,-1,TERM_WHITE,
		"Planar is based on movement and summoning. Folk magic can do ");
	Term_putstr(5,18,-1,TERM_WHITE,
		"anything but lacks the more powerful spells of other realms.");
	n = 0;
	/* Hack: Allow priests to specialize in Life or Death magic */
	if ((choices & CH_CHAOS) && p_ptr->realm1 != 4)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Chaos");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=4;
		n++;
	}
	if ((choices & CH_CORPOREAL) && p_ptr->realm1 != 8)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Corporeal");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=8;
		n++;
	}
	if ((choices & CH_DEATH) && p_ptr->realm1 != 5)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Death");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=5;
		n++;
	}
	if ((choices & CH_FOLK) && p_ptr->realm1 != 7)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Folk");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=7;
		n++;
	}
	if ((choices & CH_LIFE) && p_ptr->realm1 != 1)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Life");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=1;
		n++;
	}
	if ((choices & CH_NATURE) && p_ptr->realm1 != 3)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Nature");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=3;
		n++;
	}
	if ((choices & CH_PLANAR) && p_ptr->realm1 != 6)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Planar");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=6;
		n++;
	}
	if ((choices & CH_SORCERY) && p_ptr->realm1 != 2)
	{
		sprintf(buf, "%c%c %s", I2A(n), p2, "Sorcery");
		put_str(buf, 21 + (n/5), 2 + 15 * (n%5));
		picks[n]=2;
		n++;
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
		if (c == '?') do_cmd_help(syshelpfile);
		else bell();
	}
	/* Clean up */
	clear_from(15);
	return (picks[k]);
}

byte choose_realm_randomly(byte choices)
{
	int picks[MAX_REALM] = {0};
	int k, n;
	n = 0;
	/* Hack: Allow priests to specialize in Life or Death magic */
	if ((choices & CH_CHAOS) && p_ptr->realm1 != 4)
	{
		picks[n]=4;
		n++;
	}
	if ((choices & CH_CORPOREAL) && p_ptr->realm1 != 8)
	{
		picks[n]=8;
		n++;
	}
	if ((choices & CH_DEATH) && p_ptr->realm1 != 5)
	{
		picks[n]=5;
		n++;
	}
	if ((choices & CH_FOLK) && p_ptr->realm1 != 7)
	{
		picks[n]=7;
		n++;
	}
	if ((choices & CH_LIFE) && p_ptr->realm1 != 1)
	{
		picks[n]=1;
		n++;
	}
	if ((choices & CH_NATURE) && p_ptr->realm1 != 3)
	{
		picks[n]=3;
		n++;
	}
	if ((choices & CH_PLANAR) && p_ptr->realm1 != 6)
	{
		picks[n]=6;
		n++;
	}
	if ((choices & CH_SORCERY) && p_ptr->realm1 != 2)
	{
		picks[n]=2;
		n++;
	}
	/* Get a realm */
	k=rand_range(0,n-1);
	return (picks[k]);
}

void get_realms()
{

	int pclas=p_ptr->pclass;

	/* First we have null realms */
	p_ptr->realm1=p_ptr->realm2=0;

	/* Warriors and certain others get no realms */

	if (realm_choices[pclas] == (CH_NONE)) return;

	/* Other characters get at least one realm */

	switch (pclas)
	{
	case CLASS_WARRIOR_MAGE:
		p_ptr->realm1 = 7;
		break;
	case CLASS_DIABOLIST:
		p_ptr->realm1 = 4;
		break;
	case CLASS_PRIEST:
		p_ptr->realm1 = choose_realm( CH_LIFE | CH_DEATH);
		/* Hack... priests can be 'dark' priests and choose death instead
		of life, but not both */
		break;
	case CLASS_RANGER:
		p_ptr->realm1 = 3;
		break;
	case CLASS_MYSTIC:
		p_ptr->realm1 = 8;
		break;
	case CLASS_DRUID:
		p_ptr->realm1 = 3;
		break;
	case CLASS_DEMONOLOGIST:
		p_ptr->realm1 = 4;
		break;
	default:
		p_ptr->realm1 = choose_realm(realm_choices[pclas]);
	}

	/* Paladins, Chaos warrriors and rogues get no second realm */
	if (pclas == CLASS_PALADIN || pclas == CLASS_ROGUE || pclas == CLASS_DIABOLIST
		|| pclas == CLASS_MYSTIC || pclas == CLASS_HIGH_MAGE || pclas == CLASS_DRUID) return;
	else
		p_ptr->realm2 = choose_realm(realm_choices[pclas]);


}

/* Get realms randomly without asking player */
void get_realms_randomly()
{
	int pclas=p_ptr->pclass;
	/* First we have null realms */
	p_ptr->realm1=p_ptr->realm2=0;
	/* Warriors and certain others get no realms */
	if (realm_choices[pclas] == (CH_NONE)) return;
	/* Other characters get at least one realm */
	switch (pclas)
	{
	case CLASS_WARRIOR_MAGE:
		p_ptr->realm1 = 7;
		break;
	case CLASS_DIABOLIST:
		p_ptr->realm1 = 4;
		break;
	case CLASS_PRIEST:
		p_ptr->realm1 = choose_realm_randomly( CH_LIFE | CH_DEATH);
		/* Hack... priests can be 'dark' priests and choose death instead
		of life, but not both */
		break;
	case CLASS_RANGER:
		p_ptr->realm1 = 3;
		break;
	case CLASS_DRUID:
		p_ptr->realm1 = 3;
		break;
	case CLASS_DEMONOLOGIST:
		p_ptr->realm1 = 4;
		break;
	default:
		p_ptr->realm1 = choose_realm_randomly(realm_choices[pclas]);
	}
	/* Paladins, Chaos warrriors and rogues get no second realm */
	if (pclas == CLASS_PALADIN || pclas == CLASS_ROGUE || pclas == CLASS_DIABOLIST
		|| pclas == CLASS_MYSTIC || pclas == CLASS_HIGH_MAGE || pclas == CLASS_DRUID) return;
	else
		p_ptr->realm2 = choose_realm_randomly(realm_choices[pclas]);
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

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Variable stat maxes */
		if (maximise_mode)
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



	/* Level one */
	p_ptr->max_plv = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Pre-calculate level 1 hitdice */
	player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */

	/* 'Roll' the hitpoint values */
	lastroll = p_ptr->hitdie;
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{
		player_hp[i]=lastroll;
		lastroll--;
		if(lastroll<1) lastroll = p_ptr->hitdie;
	}
	/* Now shuffle them */
	for(i=1;i<PY_MAX_LEVEL;i++)
	{
		j=randint(PY_MAX_LEVEL-1);
		lastroll=player_hp[i];
		player_hp[i]=player_hp[j];
		player_hp[j]=lastroll;
	}
	/* Make each a cumulative score */
	for(i=1;i<PY_MAX_LEVEL;i++)
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
	case RACE_NEPHILIM:

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
	case RACE_DEVILSPAWN:
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

	/* Hack */
	j=0;
	for(i=0;i<MAX_R_IDX;i++)
	{
		if (r_info[i].flags1 & (RF1_ALWAYS_GUARD)) /* It's a quest monster */
		{
			q_list[j].level=r_info[i].level;
			q_list[j].r_idx=i;
			q_list[j].cur_num=1;
			q_list[j].max_num=1;
			j++;
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
	/* Don't allow multipliers to be random guardians */
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


	/* Start with no artefacts made yet */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artefact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}


	/* Start with no quests */
	/*initialise_quests();  DEAN */ 

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
	spell_learned1 = spell_learned2 = 0L;
	spell_worked1 = spell_worked2 = 0L;
	spell_forgotten1 = spell_forgotten2 = 0L;
	for (i = 0; i < 64; i++) spell_order[i] = 99;


	/* Clear "Debug" options */
	debug_peek = FALSE;
	debug_hear = FALSE;
	debug_room = FALSE;
	debug_xtra = FALSE;
	debug_know = FALSE;
	debug_live = FALSE;
	debug_wild = FALSE;
	debug_mode = FALSE;
	
	/* Assume no winning game */
	total_winner = FALSE;

	/* Assume no panic save */
	panic_save = 0;

	/* Assume no Debugging */
	noscore = 0;
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
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER },
		{ TV_DEATH_BOOK, 0 } /* Hack: for realm2 book */
	},

	{
		/* Priest */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for Life / Death book */
		{ TV_HAFTED, SV_MACE },
		{ TV_DEATH_BOOK, 0 } /* Hack: for realm2 book */
	},

	{
		/* Rogue */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
	},

	{
		/* Ranger */
		{ TV_NATURE_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_DEATH_BOOK, 0 }          /* Hack: for realm2 book */
	},

	{
		/* Paladin */
		{ TV_SORCERY_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL }
	},

	{
		/* Warrior-Mage */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_SHORT_SWORD },
		{ TV_DEATH_BOOK, 0 } /* Hack: for realm2 book */
	},

	{
		/* Diabolist */
		{ TV_SORCERY_BOOK, 0 }, /* Hack: For realm1 book */
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL }
	},

	{
		/* Mystic */
		{ TV_SORCERY_BOOK, 0 },
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
			{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
			{ TV_SORCERY_BOOK, 1 }, /* Hack^2 : for realm1 book, 2nd edition */
			{ TV_SWORD, SV_DAGGER }, 		
		},

		{
			/* Druid */
			{TV_SORCERY_BOOK,0}, /* Hack: for realm1 book */
			{ TV_HAFTED, SV_QUARTERSTAFF },
			{TV_AMULET,SV_AMULET_BRILLIANCE},
		},

		{
			/* Demonologist */
			{ TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
			{ TV_RING, SV_RING_SUSTAIN_MIND },
			{ TV_DEATH_BOOK, 0 } /* Hack: for realm2 book */
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

	/* A scroll of Recall for all */
	/* Get local object */
	q_ptr = &forge;
	/* Hack -- Give the player scrolls of light */
	object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL));
	q_ptr->number = (char)rand_range(1,1);
	object_aware(q_ptr);
	object_known(q_ptr);
	/* These objects are "storebought" */
	q_ptr->ident |= IDENT_STOREB;
	(void)inven_carry(q_ptr, FALSE);

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

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Hack to initialize spellbooks 
           Note that the two TV's point to first and second realm
		   Except for high mages for which first and second book are given
		   This hack is actually not even needed, but is small and might
		   be useful later	
		*/
		if (tv  == TV_SORCERY_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm1 - 1;
		else if (tv == TV_DEATH_BOOK) tv = TV_LIFE_BOOK + ( p_ptr->pclass==CLASS_HIGH_MAGE?p_ptr->realm1:p_ptr->realm2 ) - 1;

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
			p_ptr->prace == RACE_BARBARIAN)
			/* Barbarians do not need a ring of resist fear */
			sv = SV_RING_SUSTAIN_BODY;

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player an object */
		object_prep(q_ptr, lookup_kind(tv, sv));

		/* Assassins begin the game with a poisoned dagger */
		if (tv == TV_SWORD && p_ptr->pclass == CLASS_ROGUE &&
			p_ptr->realm1 == 5) /* Only assassins get a poisoned weapon */
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
	j=0;
	do
	{
		if(q_list[j].r_idx == 0) break;
		j++;
	} while (j < MAX_QUESTS);
	if(j >= MAX_QUESTS) return; /* No room for random quests */
	for (i=j; i<MAX_Q_IDX; i++)
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
			for (j = 0; j<i; j++)
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
	put_str("Class       :", 5, 1);

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
		hack_corruption = FALSE;
		if (k==RACE_DEVILSPAWN) hack_corruption = TRUE;
		p_ptr->prace = k;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;
		/* Display */
		c_put_str(TERM_L_BLUE, str, 4, 15);
		/*** Player class ***/
		while(1)
		{
			p_ptr->pclass = (char)rand_range(0,MAX_CLASS-1);
			/* Analyze */
			cp_ptr = &class_info[p_ptr->pclass];
			mp_ptr = &magic_info[p_ptr->pclass];
			str = class_sub_name[p_ptr->pclass][p_ptr->realm1];

			if (rp_ptr->choice & (1L << p_ptr->pclass )) break;
		}
		/* Display */
		c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);


		/* Get a random name */
		create_random_name(p_ptr->prace,player_name);
		/* Display */
		c_put_str(TERM_L_BLUE, player_name, 2, 13);

		get_realms_randomly();

		if(p_ptr->realm1)
		{
			if(p_ptr->realm2)
			{
				sprintf(buf,"%s/%s",realm_names[p_ptr->realm1],realm_names[p_ptr->realm2]);
			}
			else
			{
				sprintf(buf,"%s",realm_names[p_ptr->realm1]);
			}
		}

		if (p_ptr->realm1 || p_ptr->realm2) put_str("Magic       :", 6, 1);
		if (p_ptr->realm1) c_put_str(TERM_L_BLUE, buf,6,15);

		/* Generate quests */
		/* Set max number of quest */
		/*MAX_Q_IDX =randint(20)+10; 
		if ( MAX_Q_IDX > MAX_QUESTS ){
			
		}
		*/
		MAX_Q_IDX = MAX_QUESTS;
		initialise_quests();
		/*player_birth_quests();*/

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
				/* Race/Class bonus */
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
			/* Save the minimum stat depending on class */
			switch(p_ptr->pclass)
			{
			case CLASS_WARRIOR:
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
			case CLASS_MAGE:
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
			case CLASS_PRIEST:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CHA]=mval[A_CHA]-2;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_ROGUE:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_STR]=mval[A_STR]-4;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				break;
			case CLASS_RANGER:
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
			case CLASS_PALADIN:
				stat_limit[A_STR]=mval[A_STR]-1;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_WIS]=mval[A_WIS]-2;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_WARRIOR_MAGE:
				stat_limit[A_CON]=mval[A_CON]-1;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_DIABOLIST:
				stat_limit[A_STR]=mval[A_STR]-1;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_MYSTIC:
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
			case CLASS_MINDCRAFTER:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_HIGH_MAGE:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_DRUID:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CHA]=mval[A_CHA]-2;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_DEMONOLOGIST:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
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
				put_str("Class       :", 5, 1);

				c_put_str(TERM_L_BLUE, player_name, 2, 15);
				c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);
				c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 15);
				c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);

				/* Label stats */
				put_str("STR:", 2 + A_STR, 61);
				put_str("INT:", 2 + A_INT, 61);
				put_str("WIS:", 2 + A_WIS, 61);
				put_str("DEX:", 2 + A_DEX, 61);
				put_str("CON:", 2 + A_CON, 61);
				put_str("CHA:", 2 + A_CHA, 61);

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

			/* Hack -- get a chaos patron even if you are not a Diabolist */
			p_ptr->evil_patron = (randint(MAX_PATRON)) - 1;

			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;

			/* Player has no recal ritual yet */
			p_ptr->ritual = 0;

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

		/* Allow name to be edited, recolour it, prepare savefile */

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
		hack_corruption = FALSE;

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
				k = 26; 
				break;
			}
			else if (c == '2')
			{
				k = 27; 
				break;
			}
			else if (c == '3')
			{
				k = 28;  
				break;
			}
			else if (c == '4')
			{
				k = 29;
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
		hack_corruption = FALSE;
		if (k==RACE_DEVILSPAWN) hack_corruption= TRUE;
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
			if (c == '?') do_cmd_help(syshelpfile);
			else bell();
		}

		/* Set class */
		p_ptr->pclass = k;
		cp_ptr = &class_info[p_ptr->pclass];
		mp_ptr = &magic_info[p_ptr->pclass];
		str = class_sub_name[p_ptr->pclass][p_ptr->realm1];

		/* Display */
		c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);

		/* Clean up */

		clear_from(15);

		get_realms();

		if(p_ptr->realm1)
		{
			if(p_ptr->realm2)
			{
				sprintf(buf,"%s/%s",realm_names[p_ptr->realm1],realm_names[p_ptr->realm2]);
			}
			else
			{
				sprintf(buf,"%s",realm_names[p_ptr->realm1]);
			}
		}

		if (p_ptr->realm1 || p_ptr->realm2) put_str("Magic       :", 6, 1);
		if (p_ptr->realm1) c_put_str(TERM_L_BLUE, buf,6,15);

		/* Clear */
		clear_from(15);


		/* Generate quests */
		/* Set max number of quest */
		/*MAX_Q_IDX =randint(20)+10;*/
		MAX_Q_IDX = MAX_QUESTS;
		/*player_birth_quests();*/
        initialise_quests();

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

				/* Race/Class bonus */
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
					sprintf(inp, "(Max of %2d):", (m - 19)/10 + 19);
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
					/* char *s; */

					/* Move the cursor */
					put_str("", 16 + i, 30);

					/* Default */
					strcpy(inp, "");

					/* Get a response (or escape) */
					if (!askfor_aux(inp, 8)) inp[0] = '\0';

					v = atoi(inp);
					if(v>18) v=18+(v-18)*10;


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
				put_str("CHA:", 2 + A_CHA, 61);

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

			/* Hack -- get a chaos patron even if you are not a Diabolist */
			p_ptr->evil_patron = (randint(MAX_PATRON)) - 1;

			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;

			/* Player is ready to move... */
			p_ptr->energy=1000;

			/* Player has no recal ritual yet */
			p_ptr->ritual = 0;

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

		/* Get a name, recolour it, prepare savefile */

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
	for (n = 0; n < MAX_STORES; n++)
	{
		/* Initialize */
		store_init(n);

		/* Ignore home, hall  and pawnbrokers */
		if ((n != STORE_HOME) &&
			(n != STORE_HALL) &&
			(n != STORE_PAWN))
		{
			/* Maintain the shop (ten times) */
			for (i = 0; i < 10; i++) store_maint(n);
		}
	}
}



