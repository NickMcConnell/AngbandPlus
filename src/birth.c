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

	char history[4][70];
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
* Chart progression by race, this is of course outdated :)

*   Barbarian			  1 ->   2 ->   3 ->  50 ->  51 ->  52 ->  53
*   Cyclops				 77 -> 109 -> 110 -> 111 -> 112
*   Atlantian			 69 ->  70 ->  71 ->  72 ->  73
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
	{"of a Serf. ",						 40, 2, 3, 65},
	{"of a Yeoman. ",						 65, 2, 3, 80},
	{"of a Townsman. ",					 80, 2, 3, 90},
	{"of a Guildsman. ",					 90, 2, 3, 105},
	{"of a Landed Knight. ",					 96, 2, 3, 120},
	{"of a Noble Family. ",    99, 2, 3, 130},
	{"of the Royal Blood Line. ",                100, 2, 3, 140},

	/* 3 = Barbarian/Gnome/Half-Elf/Half-Giant/Half-Ogre/Half-Orc/Half-Titan/Hobbit/Human */
	{"You are the black sheep of the family. ",		 20, 3, 50, 20},
	{"You are a credit to the family. ",			 80, 3, 50, 55},
	{"You are a well liked child. ",				100, 3, 50, 60},

	/* 4 = Half-Elf */
	{"Your mother was of the Teleri. ",			 40, 4, 1, 50},
	{"Your father was of the Teleri. ",			 75, 4, 1, 55},
	{"Your mother was of the Noldor. ",		 	 90, 4, 1, 55},
	{"Your father was of the Noldor. ",		 	 95, 4, 1, 60},
	{"Your mother was of the Vanyar. ",			 98, 4, 1, 65},
	{"Your father was of the Vanyar. ",			100, 4, 1, 70},

	/* 7 = Elf/High Elf */
	{"You are one of several children ",			 60, 7, 8, 50},
	{"You are the only child ",					100, 7, 8, 55},

	/* 8 = Elf/High Elf */
	{"of a Teleri ",						 75, 8, 9, 50},
	{"of a Noldor ",						 95, 8, 9, 55},
	{"of a Vanyar ",						100, 8, 9, 60},

	/* 9 = Elf/High Elf */
	{"Ranger. ",						 40, 9, 54, 80},
	{"Archer. ",						 70, 9, 54, 90},
	{"Warrior. ",						 87, 9, 54, 110},
	{"Mage. ",							 95, 9, 54, 125},
	{"Prince. ",						 99, 9, 54, 140},
	{"King. ",							100, 9, 54, 145},

	/* 10 = Hobbit */
	{"You are one of several children of a Hobbit ",		 85, 10, 11, 45},
	{"You are the only child of a Hobbit ",		        100, 10, 11, 55},

	/* 11 = Hobbit */
	{"Bum. ",							 20, 11, 3, 55},
	{"Tavern Owner. ",						 30, 11, 3, 80},
	{"Miller. ",						 40, 11, 3, 90},
	{"Home Owner. ",						 50, 11, 3, 100},
	{"Burglar. ",						 80, 11, 3, 110},
	{"Warrior. ",						 95, 11, 3, 115},
	{"Mage. ",							 99, 11, 3, 125},
	{"Clan Elder. ",						100, 11, 3, 140},

	/* 13 = Gnome */
	{"You are one of several children of a Gnome ",		 85, 13, 14, 45},
	{"You are the only child of a Gnome ",			100, 13, 14, 55},

	/* 14 = Gnome */
	{"Beggar. ",						 20, 14, 3, 55},
	{"Braggart. ",						 50, 14, 3, 70},
	{"Prankster. ",						 75, 14, 3, 85},
	{"Warrior. ",						 95, 14, 3, 100},
	{"Mage. ",							100, 14, 3, 125},

	/* 16 = Dwarf */
	{"You are the descendant of a Dwarven ",		 25, 16, 17, 40},
	{"You are the last descendant of a Dwarven ",			100, 16, 17, 50},

	/* 17 = Dwarf */
	{"Thief. ",						 10, 17, 18, 60},
	{"Prison Guard. ",						 25, 17, 18, 75},
	{"Miner. ",						 75, 17, 18, 90},
	{"Warrior. ",						 90, 17, 18, 110},
	{"Priest. ",						 99, 17, 18, 130},
	{"King. ",							100, 17, 18, 150},

	/* 18 = Dwarf/Nibelung */
	{"You are the black sheep of the family. ",		 15, 18, 57, 10},
	{"You are a credit to the family. ",			 85, 18, 57, 50},
	{"You are a well liked child. ",				100, 18, 57, 55},

	/* 19 = Half-Orc */
	{"Your mother was an Orc, but it is unacknowledged. ",	 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged. ",	100, 19, 20, 25},

	/* 20 = Half-Giant/Half-Ogre/Half-Orc/Half-Titan */
	{"You are the adopted child ",				100, 20, 2, 50},

	/* 22 = Half-Troll */
	{"Your father is a descendant of a cave troll ",				 30, 22, 23, 20},
	{"Your father is a descendant of a cave troll ",				 60, 22, 23, 25},
	{"Your mother is a descendant of a hill troll ",				 75, 22, 23, 30},
	{"Your mother is a descendant of a hill troll ",				 90, 22, 23, 35},
	{"Your mother is a descendant of a troll ",				 95, 22, 23, 40},
	{"Your father is a descendant of a troll ",				100, 22, 23, 45},

	/* 23 = Half-Troll */
	{"Grunt. ",							  5, 23, 62, 60},
	{"Warrior. ",						 95, 23, 62, 55},
	{"Priest. ",						 99, 23, 62, 65},
	{"Clan Chief. ",						100, 23, 62, 80},

	/* 24 = Kobold */
	{"You have a green complexion, ",    25, 24, 25, 50 },
	{"You have a dark green complexion, ",    50, 24, 25, 50 },
	{"You have a yellow complexion, ",    75, 24, 25, 50 },
	{"You have a green complexion, a yellow belly, ",    100, 24, 25, 50 },

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
	{"You have eyes that glow in the dark, ",				100, 57, 58, 60},

	/* 58 = Dwarf/Nibelung */
	{"straight ",						 90, 58, 59, 50},
	{"wavy ",							100, 58, 59, 50},

	/* 59 = Dwarf/Nibelung */
	{"black hair, ",						 75, 59, 60, 50},
	{"brown hair, ",						100, 59, 60, 50},

	/* 60 = Dwarf/Nibelung */
	{"a two foot beard, ",					 25, 60, 61, 50},
	{"a two foot beard, ",					 60, 60, 61, 51},
	{"a one foot beard, ",					 90, 60, 61, 53},
	{"a one foot beard, ",					100, 60, 61, 55},

	/* 61 = Dwarf/Nibelung */
	{"and a dark complexion.",					100, 61, 0, 50},

	/* 62 = Half-Troll/Mummy */
	{"You have slime green eyes, ",				 60, 62, 63, 50},
	{"You have puke yellow eyes, ",				 85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ",				 99, 62, 63, 50},
	{"You have glowing red eyes, ",				100, 62, 63, 55},

	/* 63 = Half-Troll/Mummy */
	{"dirty ",							 33, 63, 64, 50},
	{"mangy ",							 66, 63, 64, 50},
	{"oily ",							100, 63, 64, 50},

	/* 64 = Half-Troll/Mummy */
	{"sea-weed green hair, ",					 33, 64, 65, 50},
	{"bright red hair, ",					 66, 64, 65, 50},
	{"dark purple hair, ",					100, 64, 65, 50},

	/* 65 = Half-Troll/Mummy/Yeek */
	{"and green ",						 25, 65, 66, 50},
	{"and blue ",						 50, 65, 66, 50},
	{"and white ",						 75, 65, 66, 50},
	{"and black ",						100, 65, 66, 50},

	/* 66 = Half-Troll/Mummy/Yeek */
	{"ulcerous skin.",						 33, 66, 0, 50},
	{"scabby skin.",						 66, 66, 0, 50},
	{"leprous skin.",                       100, 66, 0, 50},

	/* 67 = Nephilim */
	{"You are an unacknowledged child of ", 50, 67, 68, 45},
	{"You are a rebel child of ",         80, 67, 68, 65},
	{"You are a long lost child of ",     100, 67, 68, 55},

	/* 68 = Nephilim */
	{"someone with angel blood. ",               50, 68, 50, 80 },
	{"an unknown child of an angel. ", 65, 68, 50, 90 },
	{"an unknown angel. ", 79, 68, 50, 100 },
	{"Araqiel. ",       80, 68, 50, 130 },
	{"Kokabiel. ",        83, 68, 50, 105 },
	{"Samyaza. ",       84, 68, 50, 105 },
	{"Ramiel. ",        85, 68, 50, 90 },
	{"Daniel. ",        87, 68, 50, 100 },
	{"Chazaqiel. ",       88, 68, 50, 125 },
	{"Baraqiel. ",      89, 68, 50, 120 },
	{"Samyaza. ",       90, 68, 50, 140 },
	{"Sariel. ",     91, 68, 50, 115 },
	{"one of the Grigori leaders. ",       92, 68, 50, 110 },
	{"one of the original 200. ",       93, 68, 50, 105 },
	{"Araqiel. ",        94, 68, 50, 95 },
	{"Kokabiel. ",        95, 68, 50, 115 },
	{"Samyaza. ",        96, 68, 50, 110 },
	{"Samyaza. ",         97, 68, 50, 135 },
	{"Baraqiel. ",      98, 68, 50, 90 },
	{"Sariel. ",       99, 68, 50, 105 },
	{"Azazel. ",       100, 68, 50, 80 },

	/* 69 = Atlantian */
	{"You are one of several children of an Atlantian ",      85, 69, 70, 45},
	{"You are the only child of an Atlantian ",          100, 69, 70, 55},

	/* 70 = Atlantian */
	{"scholar. ", 50, 70, 71, 60 },
	{"researcher. ", 80, 70, 71, 75 },
	{"guardian. ", 100, 70, 71, 95 },

	/* 71 =Atlantian*/
	{"You have black eyes, ", 100, 71, 72, 50},

	/* 72 = Atlantian */
	{"straight ",                        70, 72, 73, 50},
	{"wavy ",                            90, 72, 73, 50},
	{"curly ",                          100, 72, 73, 50},

	/* 73 = Atlantian */
	{"black hair and a very dark complexion.", 100, 73, 0, 50 },

	/* 74 = Half-Ogre */
	{"You have Ogre blood from your mothers' side, your father is unaware. ", 25, 74, 20, 25},
	{"You have Ogre blood from your father' side, your mother is unaware. ", 100, 74, 20, 25},

	/* 75 = Half-Hiant */
	{"One of your forefathers on your mother's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your mother's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your mother's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your mother's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your mother's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your father's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your father's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your father's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your father's side mother was a Giant. ", 10, 75, 20, 50},
	{"One of your forefathers on your father's side mother was a Giant. ", 10, 75, 20, 50},

	/* 76 = Half-Titan */
	{"You are the the distant offspring of an unknown Titan. ", 75, 76, 20, 50 },
	{"You are the the distant offspring of Themis. ",        80, 76, 20, 100 },
	{"You are the the distant offspring of Mnemosyne. ",     85, 76, 20, 100 },
	{"You are the the distant offspring of Okeanoas. ",      90, 76, 20, 100 },
	{"You are the the distant offspring of Crius. ",         95, 76, 20, 100 },
	{"You are the the distant offspring of Hyperion. ",      98, 76, 20, 125 },
	{"You are the the distant offspring of Kronos. ",       100, 76, 20, 150 },

	/* 77 = Cyclops */
	{"You are the offspring of an unknown Cyclops. ", 90, 77, 109, 50 },
	{"You are Polyphemos's child. ", 98, 77, 109, 80 },
	{"You are Uranos's child. ", 100, 77, 109, 135 },

	/* 78 = Yeek */
	{"You are one of several children of ", 100, 78, 79, 50 },

	/* 79 = Yeek */
	{"a Brown Yeek. ", 50, 79, 80, 50 },
	{"a Blue Yeek. ", 75, 79, 80, 50 },
	{"a Master Yeek. ", 95, 79, 80, 85 },
	{"Boldor, the King of the Yeeks. ", 100, 79, 80, 120 },

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
	{"a Small Kobold. ",   40, 83, 24, 50 },
	{"a Kobold. ",         75, 83, 24, 55 },
	{"a Large Kobold. ",   95, 83, 24, 65 },
	{"Vort, the Kobold Queen. ",     100, 83, 24, 100 },

	/* 84 = Klackon */
	{"You are one of several children of a Klackon hive queen. "
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
	{"a Nibelung Slave. ", 30, 88, 18, 20 },
	{"a Nibelung Thief. ", 50, 88, 18, 40 },
	{"a Nibelung Smith. ", 70, 88, 18, 60 },
	{"a Nibelung Miner. ", 90, 88, 18, 75 },
	{"a Nibelung Priest. ", 95, 88, 18, 100 },
	{"Mime, the Nibelung. ", 100, 88, 18, 100 },

	/* 89 = Draconian */
	{"You are one of several children of a Draconian ", 85, 89, 90, 50  },
	{"You are the only child of a Draconian ", 100, 89, 90, 55 },

	/* 90 = Draconian */
	{"Warrior. ", 50, 90, 91, 50 },
	{"Priest. ", 65, 90, 91, 65 },
	{"Mage. ", 85, 90, 91, 70 },
	{"Noble. ", 100, 90, 91, 100 },

	/* 91 = Draconian */
	{"You have green wings, green skin and yellow belly.", 30, 91, 0, 50 },
	{"You have green wings, and green skin.", 55, 91, 0, 50 },
	{"You have red wings, and red skin.", 80, 91, 0, 50 },
	{"You have black wings, and black skin.", 90, 91, 0, 50 },
	{"You have metallic skin, and shining wings.", 100, 91, 0, 50},

	/* 92 = Elder Horror */
	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80 },

	/* 93 = Elder Horror */
	{"three tentacles around your mouth.", 20, 93, 0, 45 },
	{"four tentacles around your mouth.", 80, 93, 0, 50 },
	{"five tentacles around your mouth.", 100, 93, 0, 55 },

	/* 94 = Imp */
	{"You ancestor was ", 100, 94, 95, 50 },

	/* 95 = Imp */
	{"a mindless demonic spawn. ", 30, 95, 96, 20 },
	{"a minor demon. ", 60, 95, 96, 50 },
	{"a major demon. ", 90, 95, 96, 75 },
	{"a demon lord. ", 100, 95, 96, 99 },

	/* 96 = Imp */
	{"You have red skin, ", 50, 96, 97, 50 },
	{"You have brown skin, ", 100, 96, 97, 50},

	/* 97 = Imp */
	{"claws, fangs, spikes, and glowing red eyes.", 40, 97, 0, 50 },
	{"claws, fangs, and glowing red eyes.", 70, 97, 0, 50 },
	{"claws, and glowing red eyes.", 100, 97, 0, 50 },

	/* 98 = Guardian */
	{"You were created to ", 100, 98, 99, 50 },

	/* 99 = Guardian */
	{"guard ", 40, 99, 100, 50 },
	{"protect ", 80, 99, 100, 50 },
	{"preserve ", 85, 99, 100, 40 },
	{"watch ", 99, 99, 100, 50 },
	{"oversee", 100, 99, 100, 100},

	/* 100 = Guardian */
	{"a lost Elder", 40, 100, 101, 50 },
	{"the sarcophagus of an Elder", 65, 100, 101, 50 },
	{"the stasis chamber of an Elder", 90, 100, 101, 50},
	{"the first Elder", 100, 100, 101, 60},

	/* 101 = Guardian */
	{" Horror.", 10, 101, 0, 65 },
	{".", 100, 101, 0, 50 },

	/* 102 = Skeleton */
	{"You were cursed because ", 100, 102, 103, 50 },

	/* 103 = Skeleton */
	{"you slept with a medicine man's daughter. ", 30, 103, 104, 50 },
	{"sold fake magical amulets to a medicine man. ", 50, 103, 104, 50 },
	{"pretended to be more powerful than the local medicin man. ", 70, 103, 104, 50 },
	{"mistook a medicine man for the village fool. ", 75, 103, 104, 50 },
	{"you asked a medicine man for immortality. ", 85, 103, 104, 50 },
	{"were in the wrong place at the wrong time. ", 95, 103, 104, 30 },
	{"you thought it would get you a role with Johny Depp. ", 100, 103, 104, 50 },

	/* 104 = Skeleton */
	{"You have ", 100, 104, 105, 50 },

	/* 105 = Skeleton */
	{"dirty, dry bones, ", 40, 105, 106, 50 },
	{"rotten black bones, ", 60, 105, 106, 50 },
	{"filthy, brown bones, ", 80, 105, 106, 50 },
	{"shining white bones, ", 100, 105, 106, 50 },

	/* 106 = Skeleton */
	{"and a blackened skull.", 30, 106, 0, 50 },
	{"and a fractured skull.", 50, 106, 0, 50 },
	{"and empty eyesockets.", 100, 106, 0, 50 },

	/* 107 = Mummy */
	{"You were created by ", 100, 107, 108, 50 },

	/* 108 = Mummy */
	{"the local dabbler in the dark arts. ", 30, 108, 62, 50 },
	{"a priest of Mazghuna. ", 50, 108, 62, 50 },
	{"a dark priest in Saqqara. ",60, 108, 62, 50 },
	{"an evil priest of Dahshur. ", 70, 108, 62, 50 },
	{"a pact with Egyptian Sand Demons. ", 80, 108, 62, 50 },
	{"the high priests of Gizeh. ", 95, 108, 62, 30 },
	{"the Pharaoh. ", 100, 108, 62, 50 },

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
	{"You arose from an unmarked grave. ", 20, 113, 114, 50 },
	{"In life you were a simple peasant, the victim of a powerful Vampire Lord. ", 40, 109, 110, 50 },
	{"In life you were a Vampire Hunter, but they got you. ", 60, 113, 114, 50 },
	{"In life you were a Necromancer. ", 80, 113, 114, 50 },
	{"In life you were a powerful noble. ", 95, 113, 114, 50 },
	{"In life you were a powerful and cruel tyrant. ", 100, 113, 114, 50 },

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
	{"a curse. ", 30, 119, 134, 50 },
	{"an oath. ", 50, 119, 134, 50 },
	{"an curse. ", 70, 119, 134, 50 },
	{"an oath. ", 75, 119, 134, 50 },
	{"a curse. ", 85, 119, 134, 50 },
	{"an oath. ", 95, 119, 134, 30 },
	{"a pact with demons. ", 100, 119, 134, 50 },

	/* 121 = Spectre */
	{"eyes like red coals, ", 25, 121, 122, 50 },
	{"blank white eyes, ", 50, 121, 122, 50 },
	{"feral yellow eyes, ", 75, 121, 122, 50 },
	{"bloodshot red eyes, ", 100, 121, 122, 50 },

	/* 122 = Spectre */
	{" and a deathly gray complexion. ", 100, 122, 123, 50 },

	/* 123 = Spectre */
	{"An eerie green aura surrounds you.", 100, 123, 0, 50 },

	/* 124 = Faeries */
	{"You were born ", 100, 124, 125, 50 },

	/* 125 = Faeries */
	{"in Ireland. ", 20, 125, 126, 35 },
	{"in Scotland. ", 30, 125, 126, 25 },
	{"in Whales. ", 75, 125, 126, 50 },
	{"on the island of Man. ", 90, 125, 126, 75 },
	{"under a full moon. ", 100, 125, 126, 85 },

	/* 126 = Faeries */
	{"You have a great sense of humour, ", 100, 126, 127, 50 },

	/* 127 = Faeries */
	{"curly red hair, ",                        80, 127, 128, 50},
	{"spiked red hair, ",                            100, 127, 128, 50},

	/* 128 = Faeries */
	{"blue eyes, and a very fair complexion.", 100, 128, 0, 50},

	/* 129 = Devilspawn */
	{"Your mother was a succubus. ", 30, 129, 130, 40},
	{"Your father was an incubus. ",	50, 129, 130, 50 },
	{"Your mother was Glaryssa, the Succubus Queen. ",	60, 129, 130, 60 },
	{"You are created from raw Chaos itself. ", 75, 129, 130, 50},
	{"You are a descendant of a Devil Prince. ", 100, 129, 130, 30},

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

/* Hobbits are no more, one day maybe this colelction of syllables will be usefull again */
/*static char *hobbit_syllable1[] =
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
};*/

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

/* Hellband new/mad creation screen tables */



static cptr sexes_strings[] =
{
	"Lady",
	"Gentleman",
};

static cptr sexes_descriptions[COUNT_SEXES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Lady*/
	{"The League sports a few women, all worthy members.     ",
	 "Since the world in the year 1500 is ruled by men, you  ",
	 "have learned your skills outside of the public view.   ",
	 "",
	 "","","","","","","","","",""},
	/*Gentleman*/
	{"You have been accepted quite soon in the League because",
	 "of your potential. You have never considered that being",
	 "male has made your progress in the League much easier. ",
	 "","","","","","","","","","",""},	
};


static cptr races_strings[] =
{
	"Human",
	"Faerie",
	"Spawn",
	"Elder",	
};

static cptr races_descriptions[COUNT_RACES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Human*/
	{"Humans rule the world in the year 1500, however not all",
	 "of them are of pure blood. Others have been bitten by  ",
	 "vampires or werewolves. Humans born under the right    ",
	 "constellation have gained extra-ordinary powers.       ",
	 "                                                       ",
	 "                                                       ",
	 "                                                       ",
	 "                                                       ",
	 "","","","","",""},
	/*Faerie*/
	{"These little creatures are almost lost to the world,   ",
	 "but some have adapted to the ways of the humans. Mostly",
	 "found on the British Islands, some of them support the ",
	 "activities of the League. Compared to humans, faeries  ",
	 "are more dextrous, intelligent, charming, stealthier   ",
	 "and superior in magic. They are much weaker though.    ",
	 "Their magic keeps them from falling into traps.        ",
	 "                                                       ",
	 "","","","","",""},	
	/*Spawn*/
	{"Creatures born in the pits of Hell, they all fight for ",
	 "their spot. Some win, some loose and some get thrown in",
	 "to the world of Man. The League being a source of much ",
	 "power and knowledge it attracts the occasional outcast ",
	 "Spawn. The league employs some of them, using a magical",
	 "bond that lasts a hundred years. Spawns are stronger,  ",
	 "faster, tougher and more intelligent. They are heinous ",
	 "though, providing little charisma.",
	 "","","","","",""},	
	/*Elder*/
	{"Little is known about the Elder, even the Elder have   ",
	 "forgotten where they come from, what their purpose is. ",
	 "It is a generally accepted idea that the Elder existed ",
	 "when the Earth was created, and they will be there when",
	 "the Earth will be undone. The Elder employ Guardians, a",
	 "subspecies of the Elder born to protect them. Elder are",
	 "charismatic, intelligent and superior in magic.        ",
	 "                                                       ",
	 "","","","","",""},		
	
};

static int subraces[COUNT_RACES][2] = 
{
	{0 ,11},
	{12,15},
	{16,19},
	{20,22},	
};

static cptr subraces_strings[] =
{
	"Florentian",			/*0*/
	"Gipsy",				/*1*/
	"Nordic",				/*2*/
	"Atlantian",			/*3*/
	"Dwarf descendant",		/*4*/
	"Elf descendant",		/*5*/
	"Ogre descendant",		/*6*/
	"Troll descendant",		/*7*/
	"Giant descendant",		/*8*/
	"Titan descendant",		/*9*/	
	"Nephilim",				/*10*/
	"Afflicted",			/*11*/
	"Seelie Fae",			/*12*/
	"Gnome",				/*13*/
	"Leprechaun",			/*14*/
	"Kobold",				/*15*/	
	"Devilspawn",			/*16*/
	"Imp",					/*17*/
	"Succubus",				/*18*/
	"Lili",					/*19*/	
	"Elder",				/*20*/
	"Elder Guardian",		/*21*/
	"Horror",				/*22*/
};

static cptr subraces_descriptions[COUNT_SUBRACES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Florentian*/
	{   "Florentians are citizens are Italians from the city of ",
		"Florence. They are your basic human, with maybe a bit  ",
		"more interest in Inferno than the average person given ",
		"that Dante was a Florentian as well. Being an Italian  ",
		"they get discount in Italian shops.                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Hit Dice : 10                    Experience Factor: 100"},
	/*Gipsy*/
	{   "Gipsies are not very well liked, even though they are  ",
		"great entertainers and sport some of the most beautiful",
		"women. Gipsies are charismatic, have a knack for being ",
		"stealthy and gain the Second Sight when they become    ",
		"more experienced. They are slightly better in magic.   ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Dex:+1 Cha:+1                                          ",
		"Hit Dice : 10                    Experience Factor: 110"},
	/*Nordic*/
	{   "Nordics are hardy men from the North. They are still   ",
		"very much in touch with Nature and its' spirits. This  ",
		"makes them slightly better at magical abilities.       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Int:+1 Wis:+1                                          ",
		"Hit Dice 10                      Experience Factor: 120"},
	/*Atlantian*/
	{   "Living in a dome on the bottom of the ocean, they have ",
		"no natural enemies and grown weak. They do however have",
		"a knack for magic and are resistant to darkness. Their ",
		"innate magical abilities allow to fire magical missiles",
		"at will.                                               ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +3, Wis: +2, Dex: +2, Con: -1, Cha: +1   ",
		"Hit dice: 9                      Experience Factor: 150"},
	/*Dwarf*/
	{   "True dwarfs have not dwelled on the planet surface     ",
		"since ages, but they had mingled with humans and some  ",
		"of their descendants are almost as stocky, loudmouthed ",
		"and foul-tempered as they once were. They are hard to  ",
		"be blinded and find their ways easily under the ground.",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: +1, Wis: -1, Dex: -1, Con +1, Cha -1     ",
		"Hit dice: 11                     Experience Factor: 125"},
	/*Elf*/
	{   "True elfs have not dwelled in the Scandinavian lands   ",
		"since ages, but they had mingled with humans and some  ",
		"of their descendants show a startling gracefulness.    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +1, Wis: +1, Dex: +1, Con: -1, Cha: +1   ",
		"Hit dice: 8                      Experience Factor: 120"},
	/*Ogre*/
	{   "Ogres were a race of large humanoid beings, fierce and ",
		"cruel monsters that ate human flesh. The most viscious ",
		"have been hunted down and subdued. The other ones have ",
		"taken the custom to shapeshift into a human form and   ",
		"lead a normal life among humans. Some can even trick a ",
		"human and procreate. The descendants of these ogres can",
		"still sport bulging muscles, and some still know how to",
		"shapeshift. Ogres are resistant to darkness, their     ",
		"strength cannot be drained and they can place magical  ",
		"traps that explode when touched. They tend be rich.    ",
		"                                                       ",
		"                                                       ",
		"Str: +3, Int: -1, Wis: -1, Dex: -1, Con: +3, Cha: -1   ",
		"Hit dice: 12                     Experience Factor: 130"},
	/*Troll*/
	{   "Trolls are the Scandinavian version of the German and  ",
		"French ogres. Their descendants even though civilized  ",
		"are uglier, stronger, stupider and regenerate faster. ",
		"Experienced troll can enter into a berserker fury.     ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +4, Int: -4, Wis: -2, Dex: -4, Con: +3, Cha: -2   ",
		"Hit dice: 12                     Experience Factor: 137"},
	/*Giant*/
	{   "Like the descendants of the Titans they have concealed ",
		"themselves on an island in the Mediterranean sea. The  ",
		"League has found out about their existance and requires",
		"their assistance every now and then. Even though not   ",
		"very smart they make great adventurers with their solid",
		"toughness and strength. They resist strength draining  ",
		"attacks and shards. Experienced, they can smash stone  ",
		"into dust.                                             ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +4, Int: -2, Wis: -2, Dex: -2, Con: +3, Cha: -3   ",
		"Hit dice: 13                     Experience Factor: 150"},
	/*Titan*/
	{   "The largest of all, and superior in almost every aspect",
		"these descedants have been found on a remote island in ",
		"the Mediterranean sea, protected by ancient sorceries. ",
		"The League has managed to penetrate these sorceries and",
		"some of the inhabitants have decided to join them. They",
		"resist chaos, resistance and can spot the weaknesses of",
		"others.                                                ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str:  +5, Int: +1, Wis: +1, Dex: -2, Con: +3, Cha: +1  ",
		"Hit dice: 14                     Experience Factor: 255"},
	/*Nephilim*/
	{   "Children of men and angels, they usually become giant  ",
		"man-eating creatures. It seems that at some point in   ",
		"their life Nephilim must give up their Angelic or their",
		"human heritage. Nephilim starting this adventure have  ",
		"not yet made this choice, allowing them to go either   ",
		"way.                                                   ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: +2, Wis: +2, Dex: +2, Con: +3, Cha: +2   ",
		"Hit dice: 10                     Experience Factor: 225"},
	/*Afflicted*/
	{   "Either bitten by vampire or werewolf or undead, the    ",
		"afflicted have lost their humanity. This usually means ",
		"also a higher resistance than usual to nether, cold and",
		"darkness. It also means that they have lost the effects",
		"of any constellation they were born under.             ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       "},	
	/*Seelie Fae*/
	{   "Seelie Fae, or properly called Seelie Court, are good  ",
		"faeries of the British Isles. They are a beautifull to ",
		"behold, but frail and not very strong. They are very   ",
		"dextrous and have superior magic skills. Their magic   ",
		"prevents them from falling intro traps, from light and ",
		"it allows them to toss around magical sleeping dust.   ",
		"As they get more experienced, they become faster.      ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -4, Int: +3, Wis: +3, Dex: +3, Con: -2, Cha: +2   ",
		"Hit dice: 7                      Experience Factor: 175"},
	/*Gnome*/
	{   "Gnomes are a small, playful folk. Whilst being very    ",
		"intelligent, they suffer from an almost chronic failure",
		" to take anything seriously. Gnomes are constantly on  ",
		"the move, and are impossible to paralyse or slow. In   ",
		"fact, they can even teleport themself at higher levels.",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +2, Wis: +0, Dex: +2, Con: +1, Cha: -2   ",
		"Hit dice: 8                      Experience Factor: 135"},
	/*Leprechaun*/
	{   "Leprechauns are male faeries inhabiting Ireland.  They ", 
		"are into shoemaking, mischief and gold collections.    ",
		"There are no famous leprechauns yet, even though they  ",
		"are superior in magic, dexterity, charm and speed.     ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -4, Int: +3, Wis: +3, Dex: +4, Con: -4, Cha: +7   ",
		"Hit dice: 7                      Experience Factor: 100"},
	/*Kobold*/
	{   "Kobolds are malicious faeries inhabiting the Black     ", 
		"Forest. Some of their talents are very useful and for  ",
		"the right price they sometimes work with the League.   ",
		"They are masters in stealth and poison, an experienced ",
		"kobold even grows glands that allow it to spit poison  ",
		"darts. They are not an intelligent type of faerie, and ",
		"arent great lookers either.                            ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: -1, Wis: +0, Dex: +1, Con: +0, Cha: -4   ",
		"Hit dice: 9                      Experience Factor: 125"},	
	/*Devilspawn*/
	{   "Devilspawn are the progeny of mortals and demons. As   ",
		"such, they inherit some of the raw strength of their   ",
		"demonic parentage, but their mixed race tends to leave ",
		"their thoughts confused and their forms misshapen.     ",
		"Devilspawn are remembered by their demonic anscestors, ",
		"and as such they always get a demonic patron. Their    ",
		"association with the pandemonium of hell allows them to",
		"resist both confusion and sound attacks.               ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: -1, Wis: -1, Dex, -1, Con: +2, Cha: -4	",
		"Hit dice: 11                     Experience Factor: 140"},
	/*Imp*/
	{   "Imps are small red-skinned fire demons. Although not   ",
		"terribly strong or smart, they are tough and fast. As  ",
		"they are beings of fire, they have innate resistance to",
		"it, growing into immunity as they toughen up. They can ",
		"learn how to toss flame bolts, fireballs and can even  ",
		"gain Second Sight .                                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: -1, Wis: -1, Dex: +1, Con: +2, Cha: -3   ",
		"Hit dice: 10                     Experience Factor: 110"},
	/*Succubus*/
	{   "Born in the pits of Hell, they have been selected as   ",
		"much for their beauty as their visciousness. They are  ",
		"demons that can take the form of a beautiful woman and ",
		"have a special draining attacks against men. They are  ",
		"intelligent, dextrous, fast and stealthy with a knack  ",
		"for magic. They resists chaos and confusion naturally. ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Int: +2, Dex, +2, Cha: +4                              ",
		"Hit dice: 11                     Experience Factor: 160"},
	/*Lili*/
	{   "Born from Lilith and Asmodeus they know that Lillith   ",
		"will come one day after them. They join the League for ",
		"power, power they will use when the Day comes. Lili are",
		"beautiful and rebellious like their mother and sensual ",
		"like their father. Lilim resists chaos and confusion   ",
		"and are very tough.                                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Con:4, Cha: +4                                         ",
		"Hit dice: 14                     Experience Factor: 160"},	
	/*Elder*/     
	{	"The true Elder is a very tough creature, regenerating  ",
		"wounds even when almost completely destroyed. They are ",
		"a beautiful sight to behold and radiate light in the   ",
		"dark. Their senses are magically attuned and they have ",
		"the second sight. They are protected from light-based  ",
		"attacks.                                               ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: +3, Wis: +2, Dex: +3, Con: +1, Cha: +5   ",
		"Hit dice: 10                     Experience Factor: 200"},
	/*Elder Guardian*/
	{   "Elder Guardians have been completely designed to defend",
		"their assigned Elder. A few Elder Guardians have lost  ",
		"the Elder they should guard and have joined the League,",
		"as a means to find back their protegee. They are slow, ",
		"not very bright but incredibly tough. They cannot use  ",
		"mortal food, only Ambrosia or magical means can sustain",
		"them. They have awesome defences, they cannot be bled  ",
		"or stunned. They are naturally resistant to poison and ",
		"have Second Sight.                                     ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +4, Int: -5, Wis: -5, Dex: +0, Con: +4, Cha: -4   ",
		"Hit dice: 12                     Experience Factor: 200",},
	/*Horror*/     
	{	"Some of the Elder have become Horrors, after recovering",
		"from grievous wounds their body has changed into a     ",
		"nightmarish creature. Slimy, their faces covered with  ",
		"tentacles they have gained even more mental powers at  ",
		"the cost of frailty. They can gain the Second Sight,   ",
		"sense minds from a distance and project mental energies",
		"in a direct attack.                                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -3, Int: +4, Wis: +4, Dex: +0, Con: -2, Cha: -5   ",
		"Hit dice: 9                      Experience Factor: 140"},	
			
};

static cptr afflictions_strings[] =
{
	"Vampire",
	"Werewolf",
	"Skeleton",
	"Mummy",
	"Spectre",	
};

static cptr afflictions_descriptions[COUNT_AFFLICTIONS][COUNT_LINES] =
{
/*Vampire*/
{   "Vampires are mostly originating from the Karpates. They",
	"need to sustain themselves with blood and cannot bear  ",
	"light. They can see in the dark, so they do not need   ",
	"torches or lanterns. They resist poison, nether, cold, ",
	"bleeding and draining attacks. They are superior beings",
	"safe for their wisdom.                                 ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +3, Int: +3, Wis: -1, Dex: -1, Con: +1, Cha: +2   ",
	"Hit dice: 11                     Experience Factor: 200"},
/*Werewolf*/
{   "Werewolves are mostly originating from the Black Forest",
	"in Germany. They tend to kill their beloved ones under ",
	"the full moon, so a lot of them leave their homes and  ",
	"wander. Experienced werewolves can trigger the change  ",
	"to Wolf at will. Canines are neutral to Werewolves and ",
	"will not attack them.                                  ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +3, Int: +3, Wis: -1, Dex: -1, Con: +1, Cha: +2   ",
	"Hit dice: 11                     Experience Factor: 200"},
/*Skeleton*/
{   "Skeletons in Hellband are the remains of the poor souls",
	"that annoyed an African medicin man. They have can bind",
	"the soul to the bones of a person, rotting away all the",
	"skin in the progress. Not even ambrosia can feed them, ",
	"they rely soulely on magical means to feed themselves. ",
	"They are protected from bleeding, shards, poison, cold ",
	"and life draining. They all have the Second Sigh.      ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +0, Int: -2, Wis: -2, Dex: +0, Con: +1, Cha: -4   ",
	"Hit dice: 10                     Experience Factor: 145"},
/*Mummy*/
{   "Mummies are the new zombies, and I am all out of witty ",
	"description. So here's the deal. If you play a Mummy,  ",
	"you really should take the effort to write me a nice   ",
	"overview and I'll include it in Hellband.              ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +2, Int: -6, Wis: -6, Dex: +1, Con: +4, Cha: -5   ",
	"Hit dice: 13                     Experience Factor: 135"},
/*Spectre*/
{   "Spectres are incorporeal undead spirits. As such, they ",
	"can walk through walls. This hurts them slightly, and  ",
	"it can leave them vulnerable. Their ectoplasmic form   ",
	"lacks strength and stamina, although it does not bleed.",
	"Having an unnatural lifeforce, spectres resist nether, ",  
	"poison, cold and life draining. In fact nether attacks ",
	"heal them up. They have the Second Sight, can scare and",
	"detect surrounding minds. Like most undead Spectres    ",
	"can only feed on Ambrosio or with magical means. Their ",
	"glowing ectoplasmic form emits light much like a torch.",
	"                                                       ",
	"                                                       ",
	"Str: -5, Int: +4, Wis: +4, Dex: +2, Con: -3, Cha: -6   ",
	"Hit dice: 7                      Experience Factor: 180"},
};

static cptr signs_strings[] =
{
	"Free",
	"Born under Draco",
	"Born under Serpens",
	"Born under Plutus",
	"Born under Morui", /*  Morui , Orui , Orion , */  
};

static cptr signs_descriptions[COUNT_AFFLICTIONS][COUNT_LINES] =
{
/*Free*/
{   "You have been born under no particular constellation,  ",
	"causing concern among the Elder Gods. You have no      ",
	"special powers or weaknesses.                          ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Hit dice: +0                     Experience Factor: 0  "},
/*Draco*/
{   "The constellation Draco or 'Dragon' confers under rare ",
	"circumstances dragon powers to newborn children. Later ",
	"in their life they will discover resistance to many    ",
	"elements, they will also find that they can shapeshift ",
	"into a Dragonling; scaled, winged and capable to breath",
	"fire and other elements.                               ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +2, Int: +1, Wis: +1, Dex: +1, Con: +2, Cha: -3   ",
	"Hit dice: +1                     Experience Factor: 250"},
/*Serpens*/
{   "The constellation Serpens or 'Serpent' confers under   ",
	"rare conditions powers of and over snakes. People born ",
	"under this constellation can resist poison and will not",
	"be attacked by snakes and serpents. They also can be   ",
	"very stealthy.                                         ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Hit dice: +0                     Experience Factor:  30"},
/*Plutus*/
{   "Even though Plutus' star is not classified under modern",
	"astronomy, it's effects on newborns can be profound.The",
	"need to amass large fortunes and to tell whether things",
	"are valuable or not. They also have the Second Sight,  ",
	"their belongings are protected from disenchantment and ",
	"they tend to discover things that were meant to stay   ",
	"hidden. The only drawback they have is that they cannot",
	"easily part with their money, they can only spend 10%  ",
	"of it at one time.                                     ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +1, Int: -1, Wis: +2, Dex: +0, Con: +2, Cha: +4   ",
	"Hit dice: +1                     Experience Factor: 135"},
/*Morui*/
{   "Stories are told of the people from the star Morui, now",
	"more commonly called Orion. It is said that they have  ",
	"mingled with humans and that their genes are stronger  ",
	"with children born under Orion. People born under Morui",
	"are better in every way save for an odd mind. The grow ",  
	"a tough chitin that resists acid and their thoughts are",
	"impossible to confuse. The can grow wings that help    ",
	"avoid pits and falls. As Klackons get more experienced,",
	"they also get faster and gain the ability to spit acid.",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +2, Int: -1, Wis: -1, Dex: +1, Con: +2, Cha: -2   ",
	"Hit dice: +2                     Experience Factor: 135"},
};



static cptr classes_descriptions[MAX_CLASS][COUNT_LINES] =
{
	/*Warrior*/
	{   "Warriors are the simplest class. They get no magic or  ",
		"special abilities, other than learning to resist fear  ",
		"(min lev = 30). They simply fight. However, they are   ",
		"are tougher and better at fighting than any other.     ",
		"                                                       ",  
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +5, Int: -2, Wis: -2, Dex: +2, Con: +2, Cha: -1   ",
		"Hit dice: 9                      Experience Factor:   0"},
	/*Mage*/
	{   "Mages study arcane magic, but do not specialize as     ",
		"strongly as high-mages do. Mages receive two realms of ",
		"magic of their choice. Mages struggle with combat when ",
		"not using spells.                                      ",
		"                                                       ",  
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -5, Int: +3, Wis: +0, Dex: +1, Con: -2, Cha: +1   ",
		"Hit dice: 0                      Experience Factor:  30"},
	/*Priest*/
	{   "Priests are divine magic specialists. Whilst not as    ",
		"good at combat as paladins, they are better at magic. ",
		"Priests get divine magic from either miracles or the   ",
		"death realm plus one other realm (although they can't  ",
		"take miracles and death magic. Priests who learn death ",
		"magic are called Cultists. Priests take religious vows ",
		"which prevent them from using edged weapons unless     ",
		"those weapons are blessed.",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: -3, Wis: +3, Dex: -1, Con: +0, Cha: +2   ",
		"Hit dice: 2                      Experience Factor:  20"},	
	/*Rogue*/
	{   "Rogues are masters of stealth. Although they are not as",
		"good as warriors in a straight fight, they can backstab",
		"sleeping or fleeing opponents doing large amounts of   ",
		"damage. Rogues also learn a very small amount of arcane",
		"magic from either the death, planar or folk realm.     ",
		"Rogues who learn death magic are called Assassins.     ",
		"Rogues who learn planar magic are called Card Sharps. ",
		"Rogues who learn folk magic are called Thieves.        ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +1, Wis: -2, Dex: +3, Con: +1, Cha: -1   ",
		"Hit dice: 6                      Experience Factor:  25"},	
	/*Ranger*/
	{   "Rangers are decent fighters, although they specialize  ",
		"in missile weapons. Like druids, they use divine magic ",
		"from the Nature realm. They are not as good as druids  ",
		"at nature magic, but make up for it by also learning a ",
		"second realm.                                          ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +2, Wis: +0, Dex: +1, Con: +1, Cha: +1   ",
		"Hit dice: 4                      Experience Factor:  30"},	
	/*Paladin*/
	{   "Paladins are holy warriors. There are two types - true ",
		"Paladins and Death Knights. True paladins get divine   ",
		"magic from the Miracles realm, whereas death knights   ",
		"get divine magic from the Death realm. In either case, ",
		"their magic is not as strong as that of a priest, but  ",
		"they make up for this by fighting almost as well as a  ",
		"warrior does. Paladins can learn to resist the effects ",
		"of fear at a higher level.                             ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +3, Int: -3, Wis: +1, Dex: +0, Con: +2, Cha: +2   ",
		"Hit dice: 6                      Experience Factor:  35"},	
	/*Warrior-Mage*/
	{   "Warrior mages combine reasonable combat skills with two",
		"realms of arcane magic. One of their realms of magic   ",
		"must be Charms, but the other can be any. They are not ",
		"quite as good at fighting as warriors, and not quite as",
		"good at magic as true mages, but they make up for their",
		"lack of strength by combining it with a lack of        ",
		"weakness.                                              ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +2, Wis: +0, Dex: +1, Con: +0, Cha: +1   ",
		"Hit dice: 4                      Experience Factor:  50"},	
	/*Hell Knight*/
	{   "Hell Knights have made pacts with an infernal patron in",
		"exchange for physical prowess. As such, they are good  ",
		"warriors. Their patrons give them a small amount of    ",
		"divine magic from the chaos realm, and occasionally    ",
		"give them other rewards too. Hell Knights can learn to ",
		"resist the effects of chaos and fear.                  ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +1, Wis: +0, Dex: +1, Con: +2, Cha: -2   ",
		"Hit dice: 6                      Experience Factor:  35"},	
	/*Mystic*/
	{   "Mystics are martial artists. As such they are masters  ",
		"of unarmed combat and increase their speed as they gain",
		"experience. However, they are severely hampered by     ",
		"wearing heavy armour. With experience, they can shrug  ",
		"off slowing and paralyzing attacks. As part of their   ",
		"meditations, mystics learn divine magic from the       ",
		"Somatic realm.                                         ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: -1, Wis: +1, Dex: +3, Con: +2, Cha: +1   ",
		"Hit dice: 6                      Experience Factor:  40"},	
	/*Mindcrafter*/
	{   "Mindcrafters rely on the supernatural powers that their",
		"mind is capable of producing. Many of their powers are ",
		"similar to spells and are used in the same way. Some   ",
		"powers, however, are simply passive, not requiring     ",
		"active use. Mindcrafters can resists fear and confusion",
		". They can sustain their wisdom, and even sense other  ",
		"minds once they are very experienced. They can handle  ",
		"themselves in combat.                                  ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +0, Wis: +3, Dex: -1, Con: -1, Cha: +2   ",
		"Hit dice: 2                      Experience Factor:  25"},		
	/*High-Mage**/
	{   "High mages study arcane magic from a single realm to   ",
		"the exclusion of any other magic. As such, their       ",
		"magical abilities are purer than most other classes,   ",
		"and they get more spell points than other classes do. ",
		"However, their intense study leaves them weak in combat",
		"when not using spells. High mages of different realms  ",
		"have different names: Vivimancer, Sorceror, Naturist,  ",
		"Hell Knight, Necromancer, Summoner, Hedge Wizard or Zen",
		"Master.                                                ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -5, Int: +4, Wis: +0, Dex: +0, Con: -2, Cha: +1   ",
		"Hit dice: 0                      Experience Factor:  30"},			
	/*Druid**/
	{   "Druids are nature worshippers. As such, they use divine",
		"magic from the realm of Nature. They are better at     ",
		"nature magic than any other class. Like priests, druids",
		"are not allowed to use edged weapons unless those      ",
		"weapons are blessed.                                   ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: -3, Wis: +4, Dex: -2, Con: +0, Cha: +3   ",
		"Hit dice: 2                      Experience Factor:  20"},		
	/*Warlock**/
	{   "Warlocks are people who have studied the magical       ",
		"arts of demon magic with the aid of an infernal patron.",
		"They are an arcane spell user, getting demonic spell   ",
		"and the choice of any other realm. They are better at  ",
		"demonic magic than any other class. Warlocks have      ",
		"an infernal patron who may bestow gifts upon them, and ",
		"they can learn how to resist the effects of chaos.     ",
		"Warlocks have great difficulty wielding any weapon     ",
		"that is not a weapon of chaos, since their pact with   ",
		"their patron involves only using the power of chaos.   ",
		"                                                       ",
		"                                                       ",
		"Str: -5, Int: +4, Wis: +0, Dex: +0, Con: -2, Cha: +1   ",
		"Hit dice: 0                      Experience Factor:  30"},		
	
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

		Term_putstr(73,1,-1,TERM_WHITE,"<-S/s->");
		Term_putstr(73,2,-1,TERM_WHITE,"<-I/i->");
		Term_putstr(73,3,-1,TERM_WHITE,"<-W/w->");
		Term_putstr(73,4,-1,TERM_WHITE,"<-D/d->");
		Term_putstr(73,5,-1,TERM_WHITE,"<-C/c->");
		Term_putstr(73,6,-1,TERM_WHITE,"<-H/h->");

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
		if(islower(stat) ) /* ('a' < stat) */
		{
			if(points <= 0)
				continue;
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
	case DWARF:
	case GIANT:
	case GUARDIAN:
		strcpy(name, dwarf_syllable1[rand_int(sizeof(dwarf_syllable1) / sizeof(char*))]);
		strcat(name, dwarf_syllable2[rand_int(sizeof(dwarf_syllable2) / sizeof(char*))]);
		strcat(name, dwarf_syllable3[rand_int(sizeof(dwarf_syllable3) / sizeof(char*))]);
		break;
	case ELF:
	case FAE:
	case LEPRECHAUN:
	case ATLANTIAN:	
		strcpy(name, elf_syllable1[rand_int(sizeof(elf_syllable1) / sizeof(char*))]);
		strcat(name, elf_syllable2[rand_int(sizeof(elf_syllable2) / sizeof(char*))]);
		strcat(name, elf_syllable3[rand_int(sizeof(elf_syllable3) / sizeof(char*))]);
		break;
	case GNOME:
		strcpy(name, gnome_syllable1[rand_int(sizeof(gnome_syllable1) / sizeof(char*))]);
		strcat(name, gnome_syllable2[rand_int(sizeof(gnome_syllable2) / sizeof(char*))]);
		strcat(name, gnome_syllable3[rand_int(sizeof(gnome_syllable3) / sizeof(char*))]);
		break;
	case FLORENTIAN:
	case TITAN:
	case SKELETON:
	case SPECTRE:
	case VAMPIRE:
	case MUMMY:
	case GIPSY:
	case NORDIC:	
		strcpy(name, human_syllable1[rand_int(sizeof(human_syllable1) / sizeof(char*))]);
		strcat(name, human_syllable2[rand_int(sizeof(human_syllable2) / sizeof(char*))]);
		strcat(name, human_syllable3[rand_int(sizeof(human_syllable3) / sizeof(char*))]);
		break;
	case OGRE:
	case TROLL:
	case KOBOLD:
		strcpy(name, orc_syllable1[rand_int(sizeof(orc_syllable1) / sizeof(char*))]);
		strcat(name, orc_syllable2[rand_int(sizeof(orc_syllable2) / sizeof(char*))]);
		strcat(name, orc_syllable3[rand_int(sizeof(orc_syllable3) / sizeof(char*))]);
		break;
	case HORROR:
	case ELDER:	
		strcpy(name, illithid_syllable1[rand_int(sizeof(illithid_syllable1) / sizeof(char*))]);
		strcat(name, illithid_syllable2[rand_int(sizeof(illithid_syllable2) / sizeof(char*))]);
		strcat(name, illithid_syllable3[rand_int(sizeof(illithid_syllable3) / sizeof(char*))]);
		break;
	case NEPHILIM:
	case IMP:
	case DEVILSPAWN:
	case SUCCUBUS:
	case LILI:
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
	int byteflag = 1;
	int n, i, choice, dir;
	char c;
	/* These vars were used for a) , one day they  might come back
	int k;	char p2 = ')';
	*/
	char buf[80];
	
	cptr str;
	
	/* Yah, do me own centering logic, bad konijn! */
	int screen_width = 80;
	
	/*Collect all chooseable realms, store them in array*/
	/*Note that n contains how many realms can be chose*/
	n = 0;
	for( i = 1 ; i < MAX_REALM ; i ++ )
	{
		if( (choices & byteflag) && p_ptr->realm1 != i)
		{
			picks[n]=i;
			n++;
		}
		byteflag=byteflag*2; 
	}
	
	/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
	do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );
	
	/* Get Vocation for da question*/
	cp_ptr = &class_info[p_ptr->pclass];
	str = cp_ptr->title;
	
	/* Choose a realm ?*/
	choice = 0;
	while (1)
	{
		
		sprintf(buf,"What realm will you master, %s?", str);
		c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
		
		for(i=0;i<n;i++)
			c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );			
				
		for(i=0;i<n;i++)
			c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, realm_names[picks[i]], 8+i, 3);
		
/*		for(i=0;i<COUNT_LINES;i++)
			put_str( races_descriptions[choice][i] , 8+i , 23 );*/
		
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (0);		
		if (c == '*')
		{
			choice = randint(COUNT_RACES);
		}
		if (c == '?') do_cmd_help(syshelpfile_birth);
		if (c == '=')
		{
			Term_save();
			do_cmd_options_aux(7,"Startup Options");
			Term_load();
		}
		if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
		/* Look up the direction */
		dir = get_keymap_dir(c);
		if(dir==2 || c=='2')
			choice=choice+1==n?0:choice+1;
		if(dir==8 || c=='8')
			choice=choice==0?n-1:choice-1;
		else bell();
	}		
	return picks[choice];
	
}

byte choose_realm_randomly(byte choices)
{
	int picks[MAX_REALM] = {0};
	int k, n;
	n = 0;
	/* Hack: Allow priests to specialize in Miracles or Death magic */
	if ((choices & CH_DAMNATION) && p_ptr->realm1 != 4)
	{
		picks[n]=4;
		n++;
	}
	if ((choices & CH_SOMATIC) && p_ptr->realm1 != 8)
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
	if ((choices & CH_MIRACLES) && p_ptr->realm1 != 1)
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

int get_realms()
{

	int pclas=p_ptr->pclass;

	/* First we have null realms */
	p_ptr->realm1=p_ptr->realm2=0;

	/* Warriors and certain others get no realms */

	if (realm_choices[pclas] == (CH_NONE)) return TRUE;

	/* Other characters get at least one realm */

	switch (pclas)
	{
	case CLASS_WARRIOR_MAGE:
		p_ptr->realm1 = 7;
		break;
	case CLASS_HELL_KNIGHT:
		p_ptr->realm1 = 4;
		break;
	case CLASS_PRIEST:
		/* Hack... priests can be 'dark' priests and choose death instead of Miracles, but not both */		
		p_ptr->realm1 = choose_realm( CH_MIRACLES | CH_DEATH);
		if(!p_ptr->realm1)return FALSE;
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
	case CLASS_WARLOCK:
		p_ptr->realm1 = 4;
		break;
	default:
		p_ptr->realm1 = choose_realm(realm_choices[pclas]);
		if(!p_ptr->realm1)return FALSE;
	}

	/* Paladins, Oathbreakers and Rogues get no second realm */
	if (pclas == CLASS_PALADIN || pclas == CLASS_ROGUE || pclas == CLASS_HELL_KNIGHT
		|| pclas == CLASS_MYSTIC || pclas == CLASS_HIGH_MAGE || pclas == CLASS_DRUID) return TRUE;
	else
		p_ptr->realm2 = choose_realm(realm_choices[pclas]);
		if(!p_ptr->realm2)return FALSE;
		return TRUE;
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
	case CLASS_HELL_KNIGHT:
		p_ptr->realm1 = 4;
		break;
	case CLASS_PRIEST:
		p_ptr->realm1 = choose_realm_randomly( CH_MIRACLES | CH_DEATH);
		/* Hack... priests can be 'dark' priests and choose death instead
		of life, but not both */
		break;
	case CLASS_RANGER:
		p_ptr->realm1 = 3;
		break;
	case CLASS_DRUID:
		p_ptr->realm1 = 3;
		break;
	case CLASS_WARLOCK:
		p_ptr->realm1 = 4;
		break;
	default:
		p_ptr->realm1 = choose_realm_randomly(realm_choices[pclas]);
	}
	/* Paladins, Hell Knights and rogues get no second realm */
	if (pclas == CLASS_PALADIN || pclas == CLASS_ROGUE || pclas == CLASS_HELL_KNIGHT
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
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp + bsp_ptr->r_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp + bsp_ptr->r_mhp;

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
	case NEPHILIM:
		{
			chart = 67;
			break;
		}
	case ATLANTIAN:
	{
		chart = 69;
		break;
	}
	case NORDIC:
	case GIPSY:
	case FLORENTIAN:	
		{
			chart = 1;
			break;
		}
	case ELF:
		{
			chart = 4;
			break;
		}
	case GNOME:
		{
			chart = 13;
			break;
		}
	case DWARF:
		{
			chart = 16;
			break;
		}
	case TROLL:
		{
			chart = 22;
			break;
		}
	case OGRE:
		{
			chart = 74;
			break;
		}
	case GIANT:
		{
			chart = 75;
			break;
		}
	case TITAN:
		{
			chart = 76;
			break;
		}
	case KOBOLD:
		{
			chart = 82;
			break;
		}
/*
	case :
		{
			chart = 84;
			break;
		}
*/
/*
	case :
		{
			chart = 89;
			break;
		}
*/
	case HORROR:
		{
			chart = 92;
			break;
		}
	case IMP:
		{
			chart = 94;
			break;
		}
	case GUARDIAN:
		{
			chart = 98;
			break;
		}
	case SKELETON:
		{
			chart = 102;
			break;
		}
	case MUMMY:
		{
			chart = 107;
			break;
		}
	case VAMPIRE:
		{
			chart = 113;
			break;
		}
	case SPECTRE:
		{
			chart = 118;
			break;
		}
	case FAE:
	case LEPRECHAUN:
		{
			chart = 124;
			break;
		}
	case DEVILSPAWN:
	case SUCCUBUS:
	case LILI:
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
		for (n = 70; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

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

	/* Reset the "alchemy" knowledge */
	for (i = 0; i < SV_POTION_MAX ; i++)
	{
		potion_alch[i].known1 = potion_alch[i].known2 =FALSE;
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

static byte player_init[MAX_CLASS][3][3] =
{
	{
		/* Warrior */
		{ TV_RING, SV_RING_RES_FEAR , WORN }, /* Warriors need it! */
		{ TV_SWORD, SV_BROAD_SWORD , WORN},
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL , WORN }
	},

	{
		/* Mage */
		{ TV_SORCERY_BOOK, 0 , CARRIED}, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER , WORN },
		{ TV_DEATH_BOOK, 0 , CARRIED} /* Hack: for realm2 book */
	},

	{
		/* Priest */
		{ TV_SORCERY_BOOK, 0 , CARRIED}, /* Hack: for Miracles / Death book */
		{ TV_HAFTED, SV_MACE , WORN},
		{ TV_DEATH_BOOK, 0 , CARRIED} /* Hack: for realm2 book */
	},

	{
		/* Rogue */
		{ TV_SORCERY_BOOK, 0 , CARRIED}, /* Hack: for realm1 book */
		{ TV_SWORD, SV_DAGGER , WORN },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR , WORN }
	},

	{
		/* Ranger */
		{ TV_NATURE_BOOK, 0 , CARRIED},
		{ TV_SWORD, SV_BROAD_SWORD , WORN},
		{ TV_DEATH_BOOK, 0 , CARRIED}  /* Hack: for realm2 book */
	},

	{
		/* Paladin */
		{ TV_SORCERY_BOOK, 0 , CARRIED },
		{ TV_SWORD, SV_BROAD_SWORD , WORN },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL , CARRIED }
	},

	{
		/* Warrior-Mage */
		{ TV_SORCERY_BOOK, 0 , CARRIED }, /* Hack: for realm1 book */
		{ TV_SWORD, SV_SHORT_SWORD , WORN},
		{ TV_DEATH_BOOK, 0 , WORN} /* Hack: for realm2 book */
	},

	{
		/* Hell Knight */
		{ TV_SORCERY_BOOK, 0 , CARRIED}, /* Hack: For realm1 book */
		{ TV_SWORD, SV_BROAD_SWORD , WORN},
		{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL , WORN }
	},

	{
		/* Mystic */
		{ TV_SORCERY_BOOK, 0 , CARRIED },
		{ TV_POTION, SV_POTION_HEALING , CARRIED},
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR , WORN},
	},


	{
		/* Mindcrafter */
		{ TV_SWORD, SV_SMALL_SWORD, WORN },
		{ TV_POTION, SV_POTION_RESTORE_MANA , CARRIED},
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR , WORN},
	},

		{
			/* High Mage */
			{ TV_SORCERY_BOOK, 0 , CARRIED}, /* Hack: for realm1 book */
			{ TV_SORCERY_BOOK, 1 , CARRIED}	, /* Hack^2 : for realm1 book, 2nd edition */
			{ TV_SWORD, SV_DAGGER , WORN}, 		
		},

		{
			/* Druid */
			{TV_SORCERY_BOOK,0 , CARRIED}, /* Hack: for realm1 book */
			{ TV_HAFTED, SV_QUARTERSTAFF , WORN},
			{TV_AMULET,SV_AMULET_BRILLIANCE , WORN},
		},

		{
			/* Warlock */
			{ TV_SORCERY_BOOK, 0 , CARRIED}, /* Hack: for realm1 book */
			{ TV_RING, SV_RING_SUSTAIN_MIND , WORN },
			{ TV_DEATH_BOOK, 0 , CARRIED} /* Hack: for realm2 book */
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
	object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL));
	q_ptr->number = (char)rand_range(1,1);
	object_aware(q_ptr);
	object_known(q_ptr);
	/* These objects are "storebought" */
	q_ptr->ident |= IDENT_STOREB;
	(void)inven_carry(q_ptr, FALSE);
	
	/* Hack -- Give the player 2 or 3 scrolls of teleport */
	q_ptr = &forge;
	object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT));
	q_ptr->number = (char)rand_range(2,3);
	object_aware(q_ptr);
	object_known(q_ptr);
	q_ptr->ident |= IDENT_STOREB;
	(void)inven_carry(q_ptr, FALSE);

	/* Get local object */
	q_ptr = &forge;

	if (p_ptr->prace == GUARDIAN || p_ptr->prace == SKELETON ||
		p_ptr->prace == MUMMY || p_ptr->prace == VAMPIRE ||
		p_ptr->prace == SPECTRE)
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


	if (p_ptr->prace == VAMPIRE)
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
		/* Hack -- Give the player a lantern  */
		q_ptr = &forge;
		object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
		q_ptr->number = (char)rand_range(1,1);
		q_ptr->pval = 15000;
		object_aware(q_ptr);
		object_known(q_ptr);
		q_ptr->ident |= IDENT_STOREB;
		/*(void)inven_carry(q_ptr, FALSE);	*/
		outfit( q_ptr );
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
		if (tv  == TV_SORCERY_BOOK) tv = TV_MIRACLES_BOOK + p_ptr->realm1 - 1;
		else if (tv == TV_DEATH_BOOK) tv = TV_MIRACLES_BOOK + ( p_ptr->pclass==CLASS_HIGH_MAGE?p_ptr->realm1:p_ptr->realm2 ) - 1;

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
			p_ptr->prace == NORDIC)
			/* Nordics do not need a ring of resist fear */
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
		/*Carry or wear the item*/
		if(player_init[p_ptr->pclass][i][2]==CARRIED)
			(void)inven_carry(q_ptr, FALSE);
		else 
			(void)outfit(q_ptr);
	}
}

/*
 * Wear an item out of nowhere ;)
 * Hack^2
 */
void outfit(object_type *q_ptr)

{
	int slot;
	object_type forge;
	object_type *o_ptr;

	o_ptr =q_ptr;
	
	/* Check the slot */
	slot = wield_slot(o_ptr);
	
	/* Get local object */
	q_ptr = &forge;
	
	/* Obtain local object */
	object_copy(q_ptr, o_ptr);
	
	/* Modify quantity */
	q_ptr->number = 1;
	
	/* Access the wield slot */
	o_ptr = &inventory[slot];
	
	/* Wear the new stuff */
	object_copy(o_ptr, q_ptr);
	
	/* Increase the weight */
	total_weight += q_ptr->weight;
	
	/* Increment the equip counter by hand */
	equip_cnt++;
	
	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);
	
	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);
	
	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);
	
	p_ptr->redraw |= (PR_EQUIPPY);
	
	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
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
	int i, j, k, m, v, choice;
	int dir;
	int mode = 0;
	int a_offset, a_end;

	bool flag = FALSE;
	bool prev = FALSE;
	bool quickstart = FALSE;

	cptr str;
	
	char c;

	
	/* These vars were used for a) , one day they  might come back
		int n;	char p2 = ')';
	*/

	char b1 = '[';
	char b2 = ']';

	char buf[80];

	bool autoroll = FALSE;
	bool point_mod = TRUE;

	char inp[80];
	
	/* Yah, do me own centering logic, bad konijn! */
	int screen_width = 80;


	/*** Intro ***/
	
	/* Clear screen */
	Term_clear();
	
	choice = 0;
	
	do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth.txt" );
	
	/* Choose a sex*/
	while (1)
	{
		
		c_put_str(TERM_YELLOW, "Will you be a Lady or a Gentleman ?", 5, 22);

		for(i=0;i<COUNT_SEXES;i++)
			c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );
		
		for(i=0;i<COUNT_SEXES;i++)
			c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, sexes_strings[i], 8+i, 3);
		
		for(i=0;i<COUNT_LINES;i++)
			put_str( sexes_descriptions[choice][i] , 8+i , 23 );
		
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S')
		{
			quickstart = TRUE;
			break;
		}
		if (c == '?') do_cmd_help(syshelpfile_birth);
		if (c == '=')
		{
			Term_save();
			do_cmd_options_aux(7,"Startup Options");
			Term_load();
		}
		if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
		/* Look up the direction */
		dir = get_keymap_dir(c);
		if(dir==2 || c=='2')
			choice=choice+1==COUNT_SEXES?0:choice+1;
		if(dir==8 || c=='8')
			choice=choice==0?COUNT_SEXES-1:choice-1;
		else bell();
	}		

	if (quickstart)
	{
		msg_print("Autoroller disabled for now.");
		msg_print("NULL");		
		quit(NULL);
		
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
		if (k==DEVILSPAWN) hack_corruption = TRUE;
		p_ptr->prace = k;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;
		/* Display */
		/*c_put_str(TERM_L_BLUE, str, 4, 15);*/
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
		/*c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);*/


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

		/*if (p_ptr->realm1 || p_ptr->realm2) put_str("Magic       :", 6, 1); */
		/*if (p_ptr->realm1) c_put_str(TERM_L_BLUE, buf,6,15); */

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
			put_str("Enter minimums for: ", 8, 3);
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
			case CLASS_HELL_KNIGHT:
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
			case CLASS_WARLOCK:
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

			/* Hack -- get a chaos patron even if you are not a Hell Knight */
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

		/* Set sex */
		p_ptr->psex = choice;
		/* Get sex into a string */
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->address;
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );
		
		/* Choose a genus ?*/
		choice = 0;
		while (1)
		{
			
			sprintf(buf,"What is your genus, %s?", str);
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
			
			for(i=0;i<COUNT_RACES;i++)
				c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );			
			
			for(i=0;i<COUNT_RACES;i++)
				c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, races_strings[i], 8+i, 3);
			
			for(i=0;i<COUNT_LINES;i++)
				put_str( races_descriptions[choice][i] , 8+i , 23 );
			
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);		
			if (c == '*')
			{
				choice = randint(COUNT_RACES);
			}
			if (c == '?') do_cmd_help(syshelpfile_birth);
			if (c == '=')
			{
				Term_save();
				do_cmd_options_aux(7,"Startup Options");
				Term_load();
			}
			if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
			/* Look up the direction */
			dir = get_keymap_dir(c);
			if(dir==2 || c=='2')
				choice=choice+1==COUNT_RACES?0:choice+1;
			if(dir==8 || c=='8')
				choice=choice==0?COUNT_RACES-1:choice-1;
			else bell();
		}		
		
		/* Set temporary race until player has drilled down */
		p_ptr->prace = choice;
		
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
		
		/*Set some helper variables*/
		a_offset = subraces[p_ptr->prace][0];
		a_end = subraces[p_ptr->prace][1] - subraces[p_ptr->prace][0];
		
		/* Choose specific genus ?*/
		choice = 0;
		while (1)
		{
			
			sprintf(buf,"What type of %s are you, %s?",  races_strings[p_ptr->prace] ,  str);
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
			
			for(i=0;i<=a_end;i++)
				c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );					
			
			for(i=0;i<=a_end;i++)
				c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, subraces_strings[i+a_offset], 8+i, 3);
			
			for(i=0;i<COUNT_LINES;i++)
				put_str( subraces_descriptions[choice+a_offset][i] , 8+i , 23 );
			
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);	
			if (c == '*')
			{
				choice = randint(COUNT_SUBRACES);
			}
			if (c == '?') do_cmd_help(syshelpfile_birth);
			if (c == '=')
			{
				Term_save();
				do_cmd_options_aux(7,"Startup Options");
				Term_load();
			}
			if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
			/* Look up the direction */
			dir = get_keymap_dir(c);
			/*get_key_map does not alway work, so we hardcode some stuff*/
			if(dir==2 || c=='2')
				choice=choice ==a_end?0:choice+1;
			if(dir==8 || c=='8')
				choice=choice==0?a_end:choice-1;
			else bell();
		}		
		
		/* Choice was just for UI, choice+a_offset has what we really want, which is now the new role of choice */
		choice = choice+a_offset;
		
		if( ( choice==SUCCUBUS || choice == LILI ) && p_ptr->psex == GENTLEMAN )
		{
		    /*No cross dressing in my game ;)*/
			msg_print("Hellband does not support demon crossdressers.");	
		    /* Set sex */
		    p_ptr->psex = LADY;
		    /* Get sex into a string */
		    sp_ptr = &sex_info[p_ptr->psex];
		    str = sp_ptr->address;
		}
		
		/*Unless we are afflicated we get the right to be born under a constellation*/
		if( p_ptr->prace == R_HUMAN && choice!= AFFLICTED )
		{
			p_ptr->prace = choice;
			/* Choose specific birth sign ?*/
			choice = 0;
			/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			
			while (1)
			{
				
				sprintf(buf,"Which constellation were you born under, %s?",  str);
				c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
				
				for(i=0;i<COUNT_SIGNS;i++)
					c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );				
				
				for(i=0;i<COUNT_SIGNS;i++)
					c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, signs_strings[i], 8+i, 3);
				
				for(i=0;i<COUNT_LINES;i++)
					put_str( signs_descriptions[choice][i] , 8+i , 23 );
				
				c = inkey();
				if (c == 'Q') quit(NULL);
				if (c == 'S') return (FALSE);	
				if (c == '*')
				{
					choice = randint(COUNT_SIGNS);
				}
				if (c == '?') do_cmd_help(syshelpfile_birth);
				if (c == '=')
				{
					Term_save();
					do_cmd_options_aux(7,"Startup Options");
					Term_load();
				}
				if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
				/* Look up the direction */
				dir = get_keymap_dir(c);
				/*get_key_map does not alway work, so we hardcode some stuff*/
				if(dir==2 || c=='2')
					choice=choice+1==COUNT_SIGNS?0:choice+1;
				if(dir==8 || c=='8')
					choice=choice==0?COUNT_SIGNS-1:choice-1;
				else bell();
			}		
			p_ptr->psign  = choice;
	}
		/* If we are afflicted, we get to choose which afflication*/
		else if( choice == AFFLICTED)
		{
			/* Choose specific affliction ?*/
			choice = 0;
			/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			
			while (1)
			{
				
				sprintf(buf,"What is your affliction, %s?",  str);
				c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
				
				for(i=0;i<COUNT_AFFLICTIONS;i++)
					c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );					
				
				for(i=0;i<COUNT_AFFLICTIONS;i++)
					c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, afflictions_strings[i], 8+i, 3);
				
				for(i=0;i<COUNT_LINES;i++)
					put_str( afflictions_descriptions[choice][i] , 8+i , 23 );
				
				c = inkey();
				if (c == 'Q') quit(NULL);
				if (c == 'S') return (FALSE);	
				if (c == '*')
				{
					choice = randint(COUNT_AFFLICTIONS);
				}
				if (c == '?') do_cmd_help(syshelpfile_birth);
				if (c == '=')
				{
					Term_save();
					do_cmd_options_aux(7,"Startup Options");
					Term_load();
				}
				if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
				/* Look up the direction */
				dir = get_keymap_dir(c);
				/*get_key_map does not alway work, so we hardcode some stuff*/
				if(dir==2 || c=='2')
					choice=choice+1==COUNT_AFFLICTIONS?0:choice+1;
				if(dir==8 || c=='8')
					choice=choice==0?COUNT_AFFLICTIONS-1:choice-1;
				else bell();
			}			
			/* Serious hack, human afflictions must be the last afflictions!! or else !! damnation & all !!  */
			p_ptr->prace = choice+subraces[COUNT_RACES-1][1]+1;
			p_ptr->psign  = SIGN_FREE;
		}
		/*We just store the proper genus*/
		else
		{
			p_ptr->prace = choice;
			p_ptr->psign  = SIGN_FREE;	
		}
		/* Set sign */
		bsp_ptr = &sign_info[p_ptr->psign];
		
		/* Assume no corruptions */
		hack_corruption = FALSE;

		/* Set race */
		hack_corruption = FALSE;
		rp_ptr = &race_info[p_ptr->prace];
		if (p_ptr->prace==DEVILSPAWN) hack_corruption= TRUE;	
		
		/* Get a random name now we have a race*/
		create_random_name(p_ptr->prace,player_name);

		/* Choose specific class ?*/
		choice = 0;
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
		
		while (1)
		{
			
			sprintf(buf,"What is your vocation, %s?",  str);
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
			
			for(i=0;i<MAX_CLASS;i++)
				c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );		
			
			for(i=0;i<MAX_CLASS;i++)
			{
				cp_ptr = &class_info[i];
				if (!(rp_ptr->choice & (1L << i )))
				{
					sprintf(buf, "(%s)", cp_ptr->title);
				}
				else
				{
					sprintf(buf, "%s", cp_ptr->title);
				}
				c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, buf, 8+i, 3);
			}
			
			for(i=0;i<COUNT_LINES;i++)
				put_str( classes_descriptions[choice][i] , 8+i , 23 );
			
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);	
			if (c == '*')
			{
				choice = randint(MAX_CLASS);
			}
			if (c == '?') do_cmd_help(syshelpfile_birth);
			if (c == '=')
			{
				Term_save();
				do_cmd_options_aux(7,"Startup Options");
				Term_load();
			}
			if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
			/* Look up the direction */
			dir = get_keymap_dir(c);
			/*get_key_map does not alway work, so we hardcode some stuff*/
			if(dir==2 || c=='2')
				choice=choice+1==MAX_CLASS?0:choice+1;
			if(dir==8 || c=='8')
				choice=choice==0?MAX_CLASS-1:choice-1;
			else bell();
		}			
		
		
		p_ptr->pclass = choice;
		cp_ptr = &class_info[p_ptr->pclass];
		mp_ptr = &magic_info[p_ptr->pclass];
		str = class_sub_name[p_ptr->pclass][p_ptr->realm1];

		/* Display */
		c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);

		/* Clean up */

		clear_from(15);

		if(!get_realms())
			return FALSE;

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

		/*if (p_ptr->realm1 || p_ptr->realm2) put_str("Magic       :", 6, 1);*/
		/*if (p_ptr->realm1) c_put_str(TERM_L_BLUE, buf,6,15);*/

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
			
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			sprintf(buf,"Enter minimum values for these stats");
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );			

			/* Clear fields */
			auto_round = 0L;
			last_round = 0L;

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
					sprintf(inp, "(Max of %2d):  |", (m - 19)/10 + 19);
				}

				/* From 3 to 18 */
				else
				{
					sprintf(inp, "(Max of %2d):  |", m);
				}

				/* Prepare a prompt */
				sprintf(buf, "%-5s%-20s", stat_names[i], inp);

				/* Dump the prompt */
				put_str(buf, 9 + i, 3);
			}

			/* Input the minimum stats */
			for (i = 0; i < 6; i++)
			{
				/* Get a minimum stat */
				while (TRUE)
				{
					/* char *s; */

					/* Move the cursor */
					put_str("", 9 + i, 30);

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

			/* Hack -- get a chaos patron even if you are not a Hell Knight */
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



