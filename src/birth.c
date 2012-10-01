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
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100

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
	/* Human */
	{"You are the adopted child ", 10, 1, 2, 15},
	{"You are one of several children ", 30, 1, 2, 20},
	{"You are the only child ", 50, 1, 2, 30},
	{"You are the second child ", 80, 1, 2, 40},	
	{"You are the first child ", 100, 1, 2, 50},

	{"of a poor beggar. ", 10, 2, 3, 5},
	{"of a poor merchant. ", 20, 2, 3, 10},
	{"of a commoner. ", 30, 2, 3, 15},
	{"of a mercenary. ", 40, 2, 3, 20},
	{"of a soldier. ", 50, 2, 3, 25},
	{"of a rich commoner. ", 60, 2, 3, 30},
	{"of a rich merchant. ", 70, 2, 3, 35},
	{"of a noble. ", 85, 2, 3, 40},
	{"of a great noble. ", 100, 2, 3, 50},

	{"You were born in Ivhala. ", 33, 3, 4, 10},
	{"You were born in Karingen. ", 66, 3, 4, 30},
	{"You were born in Jindar. ", 100, 3, 4, 50},

	{"You have brown eyes, ", 33, 4, 5, 10},
	{"You have blue eyes, ", 66, 4, 5, 30},
	{"You have green eyes, ", 100, 4, 5, 50},

	{"curly ", 20, 5, 6, 10},
	{"short ", 40, 5, 6, 20},
	{"wavy ", 60, 5, 6, 30},
	{"straight ", 80, 5, 6, 40},
	{"long ", 100, 5, 6, 50},

	{"brown hairs ", 25, 6, 7, 10},
	{"blond hairs ", 50, 6, 7, 25},
	{"black hairs ", 75, 6, 7, 35},
	{"red hairs ", 100, 6, 7, 50},

	{"and a fair complexion. ", 20, 7, 0, 10},
	{"and a dark complexion. ", 40, 7, 0, 20},
	{"and an average complexion. ", 60, 7, 0, 30},
	{"and a very fair complexion. ", 80, 7, 0, 40},
	{"and a very dark complexion. ", 100, 7, 0, 50},

	/* Half-Elf */
	{"You are one of several children ", 50, 8, 9, 25},
	{"You are the first child ", 100, 8, 9, 50},

	{"of a commoner. ", 25, 9, 10, 10},
	{"of a soldier. ", 50, 9, 10, 25},
	{"of a a merchant. ", 75, 9, 10, 35},
	{"of a noble. ", 100, 9, 10, 50},

	{"Your father is an elf, ", 50, 10, 11, 25},
	{"Your mother is an elf, ", 100, 10, 11, 50},

	{"and you were born in Nimbraya. ", 50, 11, 12, 25},
	{"and you were born in Jindar. ", 100, 11, 12, 50},

	{"You have brown eyes, ", 33, 12, 13, 10},
	{"You have blue eyes, ", 66, 12, 13, 30},
	{"You have green eyes, ", 100, 12, 13, 50},

	{"curly ", 20, 13, 14, 10},
	{"short ", 40, 13, 14, 20},
	{"wavy ", 60, 13, 14, 30},
	{"straight ", 80, 13, 14, 40},
	{"long ", 100, 13, 14, 50},

	{"brown hairs ", 33, 14, 15, 10},
	{"blond hairs ", 66, 14, 15, 30},
	{"black hairs ", 100, 14, 15, 50},

	{"and a dark complexion. ", 25, 15, 0, 10},
	{"and a fair complexion. ", 50, 15, 0, 25},
	{"and an average complexion. ", 75, 15, 0, 35},
	{"and a very fair complexion. ", 100, 15, 0, 50},

	/* Elf */
	{"You are one of several children ", 50, 16, 17, 25},
	{"You are the first child ", 100, 16, 17, 50},

	{"of a ranger, ", 25, 17, 18, 10},
	{"of a commoner, ", 50, 17, 18, 25},
	{"of an elvish noble, ", 75, 17, 18, 35},
	{"of a Nimbrayan priestess, ", 100, 17, 18, 50},

	{"and you were born in Nimbraya. ", 100, 18, 19, 50},

	{"You have blue eyes, ", 50, 19, 20, 25},
	{"You have green eyes, ", 100, 19, 20, 50},

	{"short ", 25, 20, 21, 10},
	{"wavy ", 50, 20, 21, 25},
	{"straight ", 75, 20, 21, 35},
	{"long ", 100, 20, 21, 50},

	{"blond hairs ", 50, 21, 22, 25},
	{"black hairs ", 100, 21, 22, 50},

	{"and a fair complexion. ", 33, 22, 0, 15},
	{"and an average complexion. ", 66, 22, 0, 35},
	{"and a very fair complexion. ", 100, 22, 0, 50},

	/* Dwarf */
	{"You are the adopted child ", 25, 23, 24, 10},
	{"You are one of several children ", 50, 23, 24, 25},
	{"You are the only child ", 75, 23, 24, 35},
	{"You are the first child ", 100, 23, 24, 50},

	{"of a dwarven smith, ", 25, 24, 25, 10},
	{"of a warrior, ", 50, 24, 25, 25},
	{"of a miner, ", 75, 24, 25, 35},
	{"of a dwarven noble, ", 100, 24, 25, 50},

	{"and you were born in the eastern mountains. ", 50, 25, 26, 25},
	{"and you were born in Ivhala. ", 100, 25, 26, 50},

	{"You have brown eyes, ", 50, 26, 27, 25},
	{"You have black eyes, ", 100, 26, 27, 50},

	{"short ", 50, 27, 28, 25},
	{"straight ", 100, 27, 28, 50},

	{"brown hairs ", 50, 28, 29, 25},
	{"black hairs ", 100, 28, 29, 50},

	{"and a dark complexion. ", 33, 29, 0, 15},
	{"and an average complexion. ", 66, 29, 0, 35},
	{"and a very dark complexion. ", 100, 29, 0, 50},

	/* Gnome */
	{"You are one of several children ", 33, 30, 31, 15},
	{"You are the only child ", 66, 30, 31, 30},
	{"You are the first child ", 100, 30, 31, 50},

	{"of a commoner, ", 33, 31, 32, 10},
	{"of an alchemist, ", 66, 31, 32, 30},
	{"of a wizard, ", 100, 31, 32, 50},

	{"and you were born in Karingen. ", 100, 32, 33, 50},

	{"You have brown eyes, ", 50, 33, 34, 25},
	{"You have black eyes, ", 100, 33, 34, 50},

	{"short ", 33, 34, 35, 15},
	{"curly ", 66, 34, 35, 35},
	{"straight ", 100, 34, 35, 50},

	{"brown hairs ", 50, 35, 36, 25},
	{"black hairs ", 100, 35, 36, 50},

	{"and a dark complexion. ", 33, 36, 0, 15},
	{"and an average complexion. ", 66, 36, 0, 35},
	{"and a fair complexion. ", 100, 36, 0, 50},

	/* Kobold */
	{"You are one of several children ", 50, 37, 38, 25},
	{"You are one of a few children ", 100, 37, 38, 50},

	{"of a wild kobold. ", 33, 38, 39, 10},
	{"of a kobold warrior. ", 66, 38, 39, 30},
	{"of a kobold shaman. ", 100, 38, 39, 50},

	{"You were born in a cave. ", 50, 39, 40, 25},
	{"You were born in sewers. ", 100, 39, 40, 50},

	{"You have black eyes ", 100, 40, 41, 50},

	{"and pale green scales. ", 33, 41, 0, 15},
	{"and green scales. ", 66, 41, 0, 35},
	{"and dark green scales. ", 100, 41, 0, 50},

	/* Devlings */
	{"You are one of several children ", 100, 42, 43, 50},

	{"of a devling commoner. ", 33, 43, 44, 10},
	{"of a devling warrior. ", 66, 43, 44, 30},
	{"of a devling merchant. ", 100, 43, 44, 50},

	{"You were born in Jindar. ", 33, 44, 45, 15},
	{"You were born in Karingen. ", 66, 44, 45, 35},
	{"You were born in Ivhala. ", 100, 44, 45, 50},

	{"You have black eyes ", 100, 45, 46, 50},

	{"and you are a red devling. ", 50, 46, 0, 25},
	{"and you are a green devling. ", 60, 46, 0, 35},
	{"and you are a black devling. ", 70, 46, 0, 40},
	{"and you are a purple devling. ", 85, 46, 0, 45},
	{"and you are an orange devling. ", 100, 46, 0, 50},

	/* Celestial */
	{"You are a messenger of heaven sent in Portralis ", 100, 47, 48, 50},

	{"to protect innocents from evil. ", 100, 48, 49, 50},

	{"You have blue eyes, ", 33, 49, 50, 15},
	{"You have silver eyes, ", 66, 49, 50, 35},
	{"You have golden eyes, ", 100, 49, 50, 50},

	{"large shining silver wings ", 50, 50, 51, 35},
	{"large shining white wings ", 100, 50, 51, 50},

	{"and a shining, fair skin. ", 100, 51, 0, 50},

	/* Demon */
	{"You are one of several children ", 25, 52, 53, 15},
	{"You are the only child ", 50, 52, 53, 30},
	{"You are the second child ", 75, 52, 53, 40},	
	{"You are the first child ", 100, 52, 53, 50},

	{"of a commoner ", 33, 53, 54, 15},
	{"of a warrior ", 66, 53, 54, 30},
	{"of a noble ", 100, 53, 54, 50},

	{"and you were born in Rhyzendal. ", 100, 54, 55, 50},

	{"You have dark red eyes, ", 15, 55, 56, 10},
	{"You have shining red eyes, ", 30, 55, 56, 15},
	{"You have dark blue eyes, ", 45, 55, 56, 20},
	{"You have shining blue eyes, ", 60, 55, 56, 30},
	{"You have dark green eyes, ", 75, 55, 56, 40},
	{"You have shining green eyes, ", 100, 55, 56, 50},

	{"short horns, large black wings ", 33, 56, 57, 15},
	{"short horns, large dark brown wings ", 66, 56, 57, 30},
	{"short horns, large dark red wings ", 100, 56, 57, 50},

	{"and dark grey skin. ", 50, 57, 0, 25},
	{"and grey skin. ", 100, 57, 0, 50},

	/* Zulgor */
	{"You were born from unknown parents, and grew in orphanage. ", 33, 58, 59, 15},
	{"You are the child of a mysterious wizard. ", 66, 58, 59, 30},
	{"You were raised by adoptive parents in Jindar. ", 100, 58, 59, 50},

	{"Your origins are a mystery, and so is the chaos inside you. ", 100, 59, 60, 50},

	{"You have brown eyes, ", 20, 60, 61, 10},
	{"You have blue eyes, ", 40, 60, 61, 20},
	{"You have green eyes, ", 60, 60, 61, 30},
	{"You have red eyes, ", 80, 60, 61, 40},
	{"You have purple eyes, ", 100, 60, 61, 50},

	{"curly ", 20, 61, 62, 10},
	{"short ", 40, 61, 62, 20},
	{"wavy ", 60, 61, 62, 30},
	{"straight ", 80, 61, 62, 40},
	{"long ", 100, 61, 62, 50},

	{"black and ", 20, 62, 63, 10},
	{"blond and ", 40, 62, 63, 20},
	{"brown and ", 60, 62, 63, 30},
	{"red and ", 80, 62, 63, 40},
	{"blue and ", 100, 62, 63, 50},

	{"purple hairs, ", 20, 63, 64, 10},
	{"green hairs, ", 40, 63, 64, 20},
	{"white hairs, ", 60, 63, 64, 30},
	{"grey hairs, ", 80, 63, 64, 40},
	{"orange hairs, ", 100, 63, 64, 50},

	{"and a fair complexion. ", 20, 64, 0, 10},
	{"and a dark complexion. ", 40, 64, 0, 20},
	{"and an average complexion. ", 60, 64, 0, 30},
	{"and a very fair complexion. ", 80, 64, 0, 40},
	{"and a very dark complexion. ", 100, 64, 0, 50},

	/* Monster */
	{"You are one of several monsters that lives in caves and dungeons. ", 100, 65, 66, 50},
	
	{"You try to visit the surface, and look for a better life. ", 100, 66, 0, 50},
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

static char *demon_syllable1[] =
{
        "Eld", "Ult", "Uk", "Der", "Yot", "Vor", "Gar", "Leth",
};

static char *demon_syllable2[] =
{
        "eor", "er", "tlor", "le", "na", "no", "lyt", "far",
};

static char *demon_syllable3[] =
{
        "rion", "rah", "rix", "ter", "noz", "riht", "cal",
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
	case RACE_BARBARIAN:
	case RACE_DUNADAN:
	case RACE_HUMAN:
	case RACE_VAMPIRE:
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
	  case RACE_DEMON:
                strcpy(name, demon_syllable1[rand_int(sizeof(demon_syllable1) / sizeof(char*))]);
                strcat(name, demon_syllable2[rand_int(sizeof(demon_syllable2) / sizeof(char*))]);
                strcat(name, demon_syllable3[rand_int(sizeof(demon_syllable3) / sizeof(char*))]);
		break;
		/* Create an empty name */
	default:
		name[0] = '\0';
		break;
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
 * Get character's stats. In NewAngband 1.6.0, the stat system was
 * entirely rewritten. All the base stats are now 5.
 * 
 */
static void get_stats(void)
{
        call_lua("starting_stats", "", "");
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
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Initialize arena and rewards information -KMW- */
	p_ptr->inside_quest = 0;
	p_ptr->inside_secret = 0;

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

        /*msg_format("Current Life Rating is %d/100.", percent);*/
	msg_print(NULL);
#endif /* SHOW_LIFE_RATE */

}


/*
 * Get the racial history, and social class, using the "history charts".
 */
void get_history(void)
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
		case RACE_HUMAN:
		{
			chart = 1;
			break;
		}

		case RACE_HALF_ELF:
		{
			chart = 8;
			break;
		}

		case RACE_ELF:
		{
			chart = 16;
			break;
		}

		case RACE_DWARF:
		{
			chart = 23;
			break;
		}

		case RACE_GNOME:
		{
			chart = 30;
			break;
		}

		case RACE_KOBOLD:
		{
			chart = 37;
			break;
		}

		case RACE_DEVLING:
                {
                        chart = 42;
                        break;
                }

		case RACE_CELESTIAL:
                {
                        chart = 47;
                        break;
                }

		case RACE_DEMON:
                {
                        chart = 52;
                        break;
                }

		case RACE_ZULGOR:
                {
                        chart = 58;
                        break;
                }
                
                case RACE_MONSTER:
                {
                        chart = 65;
                        break;
                }

		default:
		{
			chart = 1;
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
    ra_ptr->activation = randint(MAX_T_ACT);
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
        
        /* No mana multiplier */
        p_ptr->to_m = 0;

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

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


        /* Hack -- ghosts */
	r_info[max_r_idx-1].max_num = 0;
        for(i = 0; i < MAX_GHOSTS; i++)
        {
                for(j = 0; j < 120; j++)
                        ghost_file[i][j] = 0;
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

	/* Default pet command settings */
	p_ptr->pet_follow_distance = 6;
	p_ptr->pet_open_doors = FALSE;
	p_ptr->pet_pickup_items = FALSE;

        /* Body changing initialisation */
        p_ptr->body_monster = 0;
        p_ptr->disembodied = FALSE;

        /* Wipe the bounties */
        total_bounties = 0;

        /* Wipe the monsters */
        wipe_m_list();

        /* Wipe the recall depths */
        for (i = 0; i < max_d_idx; i++)
        {
                max_dlv[i] = 0;
        }
        max_dlv[DUNGEON_GALGALS] = 1;

        /* Wipe the known traps list */
        for (i = 0; i < max_t_idx; i++)
        {
                t_info[i].known = 0;
                t_info[i].ident = FALSE;
        }

        /* Reset wild_mode to FALSE */
        p_ptr->wild_mode = FALSE;
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit()
{
	int i, tv, sv;

	object_type	forge;
	object_type	*q_ptr;

	/* Get local object */
        q_ptr = &forge;

        object_prep(q_ptr, lookup_kind(TV_WEAPON, 52));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);

	/* Get local object */
        q_ptr = &forge;

        object_prep(q_ptr, lookup_kind(TV_SOFT_ARMOR, 2));
        q_ptr->number = 1;
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);

        /* Get local object */
        q_ptr = &forge;

        /* Hack -- Give the player some scrolls of Identify */
        object_prep(q_ptr, lookup_kind(TV_SCROLL, 4));
        q_ptr->number = (byte)rand_range(10,15);
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);

        /* Get local object */
        q_ptr = &forge;

        /* Hack -- Give the player some scrolls of Recall */
        object_prep(q_ptr, lookup_kind(TV_SCROLL, 11));
        q_ptr->number = (byte)rand_range(4,8);
        object_aware(q_ptr);
        object_known(q_ptr);
        (void)inven_carry(q_ptr, FALSE);

	/* Get local object */
	q_ptr = &forge;

	/* Hack -- Give the player some torches */
	object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	q_ptr->number = (byte)rand_range(3, 7);
	q_ptr->pval = rand_range(3, 7) * 500;
	object_aware(q_ptr);
	object_known(q_ptr);
	(void)inven_carry(q_ptr, FALSE);

	/* Get local object */
	q_ptr = &forge;

	object_prep(q_ptr, lookup_kind(TV_LICIALHYD, 1));
	q_ptr->number = 1;
	q_ptr->pval2 = 1;
	q_ptr->pval3 = 100;
	object_aware(q_ptr);
	object_known(q_ptr);
	(void)inven_carry(q_ptr, FALSE);

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
        int i, j, k, m, n, v, x, w;

	int mode = 0;

	bool flag = FALSE;
	bool prev = FALSE;

	cptr str;

        char c, b;

#if 0
	char p1 = '(';
#endif

	char p2 = ')';
	char b1 = '[';
	char b2 = ']';

	char buf[80];
	char inp[80];

	int xstart = 0;
	int ystart = 0;

	/*** Intro ***/

	/* Display the game's story! :) */
	display_help_file("story.txt");

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
                "Your sex do not have any signifiant gameplay effects.");

	/* Prompt for "Sex" */
        for (n = 0; n < MAX_SEXES - 1; n++)
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
                sprintf(buf, "Choose a sex (%c-%c), * for random: ", I2A(0), I2A(n-1));
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
		sprintf(buf, "%c%c %s", I2A(n), p2, str);
		put_str(buf, 20 + (n/5), 2 + 15 * (n%5));
	}

	/* Choose */
	while (1)
	{
                sprintf(buf, "Choose a race (%c-%c), * for random: ", I2A(0), I2A(n-1));
		put_str(buf, 19, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == '*')
		{
                        k = rand_int(MAX_RACES);
			break;
		}
		
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n))
		{
			sprintf(buf,"%c.txt",c);
            		display_help_file(buf);

			msg_print("Keep this race? [y/n]");
			b = inkey();
			if (b != 'y' && b != 'Y')
			{
				c = 'z';
				msg_print(NULL);
			}
			else break;
		}
			if (c == '?') do_cmd_help();
		else bell();
	}

	/* Set race */
	p_ptr->prace = k;
	rp_ptr = &race_info[p_ptr->prace];
	str = rp_ptr->title;

	/* Display */
	c_put_str(TERM_L_BLUE, str, 4, 15);

	/* Get a random name */
	create_random_name(p_ptr->prace, player_name);

        /* Just get a body for monsters... */
        if (p_ptr->prace == RACE_MONSTER) get_a_monster_body();

	/* Clean up */
	clear_from(15);

        if (p_ptr->prace == RACE_MONSTER)
	{
		p_ptr->pclass = CLASS_MONSTER;
		p_ptr->abilities_powers[0] = ((CLASS_APPRENTICE * 10) + 1);
		p_ptr->abilities_powers[1] = ((CLASS_APPRENTICE * 10) + 2);
	}
	else p_ptr->pclass = CLASS_APPRENTICE;
	cp_ptr = &class_info[p_ptr->pclass];
	str = classes_def[p_ptr->pclass].name;

	/* Display */
	c_put_str(TERM_L_BLUE, classes_def[p_ptr->pclass].name, 5, 15);

	/* Clear */
	clear_from(15);

        /* Maximize, preserve and sepcial levels init */
        p_ptr->maximize = FALSE;
        p_ptr->preserve = FALSE;
        p_ptr->special = FALSE;

        /* Set the recall dungeon accordingly */
        dungeon_type = DUNGEON_GALGALS;
        p_ptr->recall_dungeon = DUNGEON_GALGALS;

        /* Set dungeon seed */
        if (permanent_levels)
        {
                seed_dungeon = rand_int(0x10000000);
        }else{
                seed_dungeon = 0;
        }

        /* Clear */
        clear_from(15);

	/* Initialize allow_one_death */
        p_ptr->allow_one_death = 0;

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

	/*** User enters number of quests ***/
	/* Heino Vander Sanden and Jimmy De Laet */

	/* Extra info */
        /*Term_putstr(5, 15, -1, TERM_WHITE,
		"You can input yourself the number of quest you'd like to");
	Term_putstr(5, 16, -1, TERM_WHITE,
                "perform next to two obligatory ones ( Sauron and Morgoth )");
	Term_putstr(5, 17, -1, TERM_WHITE,
		"In case you do not want any additional quest, just enter 0");

	while (TRUE)
	{
		put_str(format("Number of additional quest? (<%u) ", MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 2), 20, 2);

		while (TRUE)
		{
			put_str("", 20, 37);

			strcpy(inp, "20");

			if (!askfor_aux(inp, 2)) inp[0] = '\0';
			v = atoi(inp);

			if ((v <= MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1) && ( v >= 0 )) break;
		}
		break;
	}

        clear_from(15);*/
        v = 0;
	/* Init the random quests */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = 0;
	p_ptr->inside_secret = 0;

	/* Set the quest monster hook */
	get_mon_num_hook = monster_quest;

	/* Prepare allocation table */
	get_mon_num_prep();

        p_ptr->inside_quest = 0;
	p_ptr->inside_secret = 0;


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
			c_put_str(TERM_L_BLUE, classes_def[p_ptr->pclass].name, 5, 15);

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

		/* Start with 1000 golds. */
		p_ptr->au = 1000;

                /* Reset death count. */
                p_ptr->deathcount = 0;

		/* Reset alignment. */
                p_ptr->alignment = 0;

                /* Reset Great Guard counter */
                p_ptr->guardconfuse = 0;

		/* Reset events, towns. */
		for (w = 0; w < 30000; w++)
		{
			p_ptr->events[w] = 0;
			p_ptr->towns[w] = 0;
		}

		if (p_ptr->prace == RACE_MONSTER)
		{
			p_ptr->events[29015] = 1;
			p_ptr->events[29027] = 1;
		}

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

			/* No dual wield at birth. */
			p_ptr->dualwield = 0;

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
				do_cmd_help();
				continue;
			}

                        /* Know body monster */
                        if (c == '‚')
                        {
                                know_body_monster();
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
        /* NewAngband: You can change your age! */
        /*change_age();*/

        p_ptr->town_num = 1;
	p_ptr->startx = 0;
	p_ptr->starty = 0;
	p_ptr->wild_x = birth_wild_x;
	p_ptr->wild_y = birth_wild_y;

        /* Background Editor! */
        change_background();
        /* Don't forget to delete all spells! */
        delete_all_spells();

        /* Oh, and set all abilities to 0 */
        for (x = 0; x < MAX_ABILITIES; x++)
        {
                p_ptr->abilities[x] = 0;
        }
        /* Class levels and kills too! (level is at 1) */
        for (x = 0; x < MAX_CLASS; x++)
        {
                p_ptr->class_level[x] = 1;
                p_ptr->class_kills[x] = 0;
        }
        /* Set the lord's element to 0 */
        p_ptr->elemlord = 0;
        /* Set the magic mode to 0(spells) */
	if (p_ptr->prace == RACE_MONSTER) p_ptr->magic_mode = 1;
        else p_ptr->magic_mode = 0;
        /* Set the aura to off */
        p_ptr->auraon = FALSE;
	/* Set current weapon... */
	current_weapon = &inventory[INVEN_WIELD];

	/* Prompt for it */
	prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

	/* Get a key */
	c = inkey();

	/* Quit */
	if (c == 'Q') quit(NULL);

	/* Start over */
	if (c == 'S') return (FALSE);

	/* Accept */
	display_help_file("prologue.txt");
	if (p_ptr->prace == RACE_CELESTIAL) p_ptr->elemental_effects |= ELEM_LITE;
	if (p_ptr->prace == RACE_DEMON) p_ptr->elemental_effects |= ELEM_DARK;
	if (p_ptr->prace == RACE_ZULGOR) p_ptr->elemental_effects |= ELEM_CHAOS;

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

        race_chart[RACE_HUMAN] = 1;
        race_chart[RACE_HALF_ELF] = 8;
	race_chart[RACE_ELF] = 16;
	race_chart[RACE_DWARF] = 23;
	race_chart[RACE_GNOME] = 30;
	race_chart[RACE_KOBOLD] = 37;
	race_chart[RACE_DEVLING] = 42;
	race_chart[RACE_CELESTIAL] = 47;
	race_chart[RACE_DEMON] = 52;
	race_chart[RACE_ZULGOR] = 58;
	race_chart[RACE_MONSTER] = 65;

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
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
        int i,j,n;

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

	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add("  ");
	message_add(" ");


	/* Hack -- outfit the player */
	player_outfit();

        /* Load the user options */
        load_options();

	/* Init the shops */
	for (i = 0; i < MAX_STORES; i++)
	{
		/* Initialize */
		store_init(i);
	}

	/* special levels */
        for (i = 0; i < max_d_idx; i++)
                for (n = 0; n < MAX_DUNGEON_DEPTH; n++)
                        spec_history[n][i] = 0;

	/* initialize variable according to text file v_info.txt */

	if (p_ptr->special)
	{
                /* Initialize the vaults */
                init_v_info();

                for (n=1;n<max_v_idx;n++)
                {
                        vault_type *v_ptr = &v_info[n];

                        if (v_ptr->typ == 99)
                        {
                                spec_history[v_ptr->lvl - d_info[v_ptr->dun_type].mindepth][v_ptr->dun_type] = 1;

                                for (i = 0; i < 10; i++)
                                {
                                        if (v_ptr->mon[i] != 0) /* a monster */
                                        {       
                                                monster_race    *r_ptr = &r_info[v_ptr->mon[i]];
                                                if (r_ptr->flags1 & RF1_UNIQUE)
                                                        /* Hack -- set to -1 will not generate them nor show them in the unique killed list */
                                                        r_ptr->max_num = -1;
                                                else
                                                        r_ptr->max_num--;
                                        }
                                }

#if 0 /* No longer needed, there is a flag for that */
                                for (i = 0; i < 3; i++)
                                {
                                        if (v_ptr->item[i] != 0) /* artifact */
                                        {
                                                artifact_type   *a_ptr = &a_info[v_ptr->item[i]];
                                                a_ptr->cur_num = 1;     /* mark artifact found */
                                        }
                                }
#endif
                        }
                }
	}
}

/* NEW TO THIS VERSION(AND TO THE WHOLE WORLD OF ANGBAND) */
/* Allow player to change class! */
void do_cmd_change_class(void)
{
     int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        /* First, do a couple of checks! */

	if (dun_level > 0)
	{
		msg_print("You must be in town to perform class changes!");
		return;
	}
        if (p_ptr->class_level[p_ptr->pclass] < 3 && p_ptr->pclass != CLASS_APPRENTICE)
        {
                msg_print("You must be at least level 3 in your current class before you can perform a class change!");
                return;
        }
	if (p_ptr->str_boost_dur > 0 || p_ptr->int_boost_dur > 0 || p_ptr->wis_boost_dur > 0 || p_ptr->dex_boost_dur > 0 || p_ptr->con_boost_dur > 0 || p_ptr->chr_boost_dur > 0)
	{
		msg_print("You cannot change class while under the effect of a stats boosting spell or ability.");
		return;
	}

        /* List the powers */
        strcpy(power_desc[num],"Basic Classes");powers[num++]=1;
        strcpy(power_desc[num],"Advanced Classes");powers[num++]=2;

        if(!num) {msg_print("You cannot change class!");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Set %c-%c, *=List, ESC=exit) Choose a class set. ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Set %c-%c, *=List, ESC=exit) Choose a class set. ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

        switch(Power)
        {
                case 1:
                        change_class_basic();
                        break;
                case 2:
                        change_class_advanced();
                        break;
                
        }

}

void change_back_to_apprentice(void)
{
     cptr str;
     msg_print("You become an apprentice! Now you can change class again!");
     msg_print("However, you lose all you ability points. That's the cost of changing class!");
     /* p_ptr->exp -= p_ptr->exp / 4; */
     /* p_ptr->max_exp -= p_ptr->max_exp / 4; */
     /* Your ability points are gone!!! */
     p_ptr->ability_points = 0;
     check_experience();
     p_ptr->pclass = CLASS_APPRENTICE;
     cp_ptr = &class_info[p_ptr->pclass];
     str = classes_def[p_ptr->pclass].name;
     c_put_str(TERM_L_BLUE, classes_def[p_ptr->pclass].name, 2, 0);
     p_ptr->update |= (PU_BONUS);
     p_ptr->update |= (PU_TORCH);
     p_ptr->update |= (PU_HP);
     p_ptr->update |= (PU_MANA);
}        

/* Remove *ALL* items of player(except those in the home) */
void no_more_items(void)
{
        int i;
        u32b f1, f2, f3, f4;
        for (i = 0; i < INVEN_TOTAL; i++)
	{
                object_type *o_ptr;
                o_ptr = &inventory[i];
                object_flags(o_ptr, &f1, &f2, &f3, &f4);
                /* Skip Eternal items */
		/* As of Portralis 0.4, also skip items worht 1000 golds or less. */
                if (!(f4 & (TR4_ETERNAL)) && object_value_real(o_ptr) > 1000)
                {
                        /* object_wipe(&inventory[i]); */
                        inven_item_increase(i, -(o_ptr->number));
                        inven_item_optimize(i);
                }
	}
        p_ptr->au = 0;
}        

void no_more_kills(void)
{
        int i;
	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}
}

/* Save your gold from death! */
void make_gold_pile(void)
{
        int goldamount;
	char goldstr[80];

	object_type	forge;
	object_type	*q_ptr;

	/* Get local object */
	q_ptr = &forge;

        /* How much gold ? */
        if (p_ptr->au >= 100000)
        {
		sprintf(goldstr, "How much gold in your pile(max 100000)? ");
                goldamount = get_quantity_s32b(goldstr, 100000);
        }
        else
	{
		sprintf(goldstr, "How much gold in your pile(max %ld)? ", p_ptr->au);
		goldamount = get_quantity_s32b(goldstr, p_ptr->au);
	}
        if (goldamount <= 0) return;
        
        object_prep(q_ptr, lookup_kind(TV_GOLD, 9));
        q_ptr->pval = goldamount;
        object_aware(q_ptr);
        object_known(q_ptr);

        /* These objects are "storebought" */
        q_ptr->ident |= IDENT_STOREB;

        p_ptr->au -= goldamount;
        update_and_handle();

        (void)inven_carry(q_ptr, FALSE);
        msg_print("You make a gold pile.");
}

/* Allow players of the 'Monster' race to evolve into stronger monsters. */
void do_cmd_evolve()
{
        int x = 0;
        int y = 0;
        int oldbody = 0;
        int monsters_choice[1200];
        int             Power = -1;
        int             num = 0, i;
	int             powers[36];
	char            power_desc[36][80];
	bool            flag, redraw;
        int             ask;
        char            choice;
        char            out_val[160], monstername[50];
	int		reqlev;
        monster_race *r_ptr;
        monster_race *b_ptr;

        b_ptr = &r_info[p_ptr->body_monster];
        /* Note here that max_r_idx is not used. That's because you */
        /* cannot choose monsters from the generator! */
        for (x = 1; x < max_r_idx; x++)
        {
                r_ptr = &r_info[x];
		reqlev = r_ptr->level;

                if ((r_ptr->level - essence_evolution_reduction(x)) <= p_ptr->lev && r_ptr->level > b_ptr->level && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags9 & RF9_SPECIAL_GENE) && !(r_ptr->flags7 & RF7_UNPLAYABLE) && (r_ptr->cursed == 0))
                {
                        /* Special code for lesser dragons... */
                        if (b_ptr->d_char == 'd')
                        {
                                if (r_ptr->d_char == 'd' || r_ptr->d_char == 'D')
                                {
                                        monsters_choice[y] = x;
                                        if (y < 35) y++;
                                }
                        }
                        /* Special code for lesser demons... */
                        else if (b_ptr->d_char == 'u')
                        {
                                if (r_ptr->d_char == 'u' || r_ptr->d_char == 'U')
                                {
                                        monsters_choice[y] = x;
                                        if (y < 35) y++;
                                }
                        }
			/* Special code for vampires... */
                        else if (b_ptr->d_char == 'v')
                        {
                                if (r_ptr->d_char == 'v' || r_ptr->d_char == 'V')
                                {
                                        monsters_choice[y] = x;
                                        if (y < 35) y++;
                                }
                        }
			/* Special code for skeletons... */
                        else if (b_ptr->d_char == 's')
                        {
                                if (r_ptr->d_char == 's' || r_ptr->d_char == 'L')
                                {
                                        monsters_choice[y] = x;
                                        if (y < 35) y++;
                                }
                        }
			else if (b_ptr->d_char == 'C')
                        {
                                if (r_ptr->d_char == 'C' || r_ptr->d_char == 'Z')
                                {
                                        monsters_choice[y] = x;
                                        if (y < 35) y++;
                                }
                        }
			else if (b_ptr->d_char == 'p')
                        {
                                if (r_ptr->d_char == 'p' || r_ptr->d_char == 'P')
                                {
                                        monsters_choice[y] = x;
                                        if (y < 35) y++;
                                }
                        }
                        else if (r_ptr->d_char == b_ptr->d_char)
                        {
                                monsters_choice[y] = x;
                                if (y < 35) y++;
                        }
                }
        }	  

        /* List the powers */
        for (x = 0; x < y; x++)
        {
                monster_race_desc(monstername, monsters_choice[x]); 
                strcpy(power_desc[num],monstername);powers[num++] = monsters_choice[x];
        }

        if(!num) {msg_print("No evolutions available.");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Monsters %c-%c, *=List, ESC=exit) Evolve into what? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Monsters %c-%c, *=List, ESC=exit) Evolve into what? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}
        oldbody = p_ptr->body_monster;
	evolution_compare(Power, TRUE, FALSE);
        if (!get_com("Evolve into this monster? [y/n]", &choice)) return;
        if (choice == 'y' || choice == 'Y')
        {
		int i;
		int apregain = 0;
		p_ptr->body_monster = Power;
                msg_print("You evolved into a new monster!");
                r_ptr = &r_info[p_ptr->body_monster];
		for (i = 0; i < 20; i ++)
		{
			if (p_ptr->abilities_monster_attacks[i] > 0)
			{
				apregain += p_ptr->abilities_monster_attacks[i];
				p_ptr->abilities_monster_attacks[i] = 0;
			}
			if (p_ptr->abilities_monster_spells[i] > 0)
			{
				apregain += p_ptr->abilities_monster_spells[i];
				p_ptr->abilities_monster_spells[i] = 0;
			}
		}
		if (apregain > 0) p_ptr->ability_points += apregain;
                
                update_and_handle();
        }
        else
        {
                p_ptr->body_monster = oldbody;
                update_and_handle();
        }
}

/* %$ Destroy ****ALL**** items, even Eternal ones!!! $% */
void no_more_items_variaz(void)
{
        int i;
	for (i = 0; i < INVEN_TOTAL; i++)
	{
                object_wipe(&inventory[i]);                
	}
        p_ptr->au = 0;
}        

/* Class changing! :) */
void change_class_basic()
{
	int                     Power = -1;
        int                     num = 0, i;
	int			x = 0;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
        int             ask;

        char            choice;

	char            out_val[160];

        /* List the classes */
	/* We skip class 0, which SHOULD be the Apprentice, */
	for (x = 1; x < MAX_CLASS; x++)
	{
		if (classes_def[x].created && classes_def[x].advanced == 0)
		{
        		strcpy(power_desc[num],classes_def[x].name);powers[num++]=x;
		}
	}

        if(!num) {msg_print("There are no available classes in this category!");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Classes %c-%c, *=List, ESC=exit) Become which class? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Classes %c-%c, *=List, ESC=exit) Become which class? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

        /* Become the chosen class! */
	  msg_print("You changed to a new class!");
	  c_put_str(TERM_L_BLUE, "             ", 2, 0);
	  c_put_str(TERM_L_BLUE, "             ", 3, 0);
        p_ptr->pclass = Power;
        cp_ptr = &class_info[p_ptr->pclass];
        c_put_str(TERM_L_BLUE, classes_def[p_ptr->pclass].name, 2, 0);
        p_ptr->update |= (PU_BONUS);
        p_ptr->update |= (PU_TORCH);
        p_ptr->update |= (PU_HP);
        p_ptr->update |= (PU_MANA);
        energy_use = 100;

}

/* Advanced class changing! :) */
/* Generally requires some levels in multiple classes */
/* or a high level of a specific class. */
void change_class_advanced()
{
	int                     Power = -1;
        int                     num = 0, i;
	int			x, y, z;

	int             powers[36];
	char            power_desc[36][80];

	bool            flag, redraw;
	bool		canuseclass = TRUE;
        int             ask;

        char            choice;

	char            out_val[160];

        /* List the classes */
	/* We skip class 0, which SHOULD be the Apprentice, */
	for (x = 1; x < MAX_CLASS; x++)
	{
		canuseclass = TRUE;
		if (classes_def[x].created && classes_def[x].advanced == 1)
		{
			/* Check the requirements. */
			if (p_ptr->stat_ind[A_STR] >= classes_def[x].req_str && p_ptr->stat_ind[A_INT] >= classes_def[x].req_int &&
			p_ptr->stat_ind[A_WIS] >= classes_def[x].req_wis && p_ptr->stat_ind[A_DEX] >= classes_def[x].req_dex &&
			p_ptr->stat_ind[A_CON] >= classes_def[x].req_con && p_ptr->stat_ind[A_CHR] >= classes_def[x].req_chr)
			{
				for (y = 0; y < SKILL_MAX; y++)
				{
					if (p_ptr->skill[y] < classes_def[x].req_skills[y])
					{
						canuseclass = FALSE;
					}
				}
				for (z = 0; z < MAX_CLASS; z++)
				{
					if (p_ptr->class_level[z] < classes_def[x].req_classes[z])
					{
        					canuseclass = FALSE;
					}
				}
				if (canuseclass) { strcpy(power_desc[num],classes_def[x].name);powers[num++]=x;}
			}
		}
	}

        if(!num) {msg_print("There are no available classes in this category!");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Classes %c-%c, *=List, ESC=exit) Become which class? ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Classes %c-%c, *=List, ESC=exit) Become which class? ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

        /* Become the chosen class! */
	  msg_print("You changed to a new class!");
	  c_put_str(TERM_L_BLUE, "             ", 2, 0);
	  c_put_str(TERM_L_BLUE, "             ", 3, 0);
        if (Power == CLASS_ELEM_LORD && p_ptr->elemlord == 0) pick_lord_element();
        if (Power == CLASS_MONSTER_MAGE) 
	{
		p_ptr->magic_mode = 1;
		p_ptr->learning = TRUE;
	}
        if (Power != CLASS_MONSTER_MAGE && p_ptr->magic_mode == 1 && p_ptr->abilities[(CLASS_MONSTER_MAGE * 10)] == 0) p_ptr->magic_mode = 0;
	if (Power != CLASS_MONSTER_MAGE) p_ptr->learning = FALSE;
        if (Power == CLASS_SOUL_GUARDIAN)
	{
		int k;
		bool foundability = FALSE;
		bool givenability = FALSE;
		for (k = 0; k < 36; k++)
		{
			if (p_ptr->abilities_powers[k] == ((CLASS_APPRENTICE * 10) + 9)) foundability = TRUE;
		}
		if (!(foundability))
		{
			for (k = 0; k < 36; k++)
			{
				if (p_ptr->abilities_powers[k] == 0 && !(givenability))
				{
					p_ptr->abilities_powers[k] = ((CLASS_APPRENTICE * 10) + 9);
					givenability = TRUE;
				}
			}
		}
	}
        p_ptr->pclass = Power;
        cp_ptr = &class_info[p_ptr->pclass];
        c_put_str(TERM_L_BLUE, classes_def[p_ptr->pclass].name, 2, 0);
        p_ptr->update |= (PU_BONUS);
        p_ptr->update |= (PU_TORCH);
        p_ptr->update |= (PU_HP);
        p_ptr->update |= (PU_MANA);

}

/* Pick the Elemental Lord's element */
void pick_lord_element()
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

        bool            flag, redraw;
        bool            endloop = FALSE;

        int             ask;

        char            choice;

	char            out_val[160];

	/* If were's a Zulgor, pick chaos. No point in taking anything else! ;) */
	if (p_ptr->prace == RACE_ZULGOR)
	{
		p_ptr->elemlord = GF_CHAOS;
		return;
	}
        
        /* List the powers */
        strcpy(power_desc[num],"Fire");powers[num++]=GF_FIRE;
        strcpy(power_desc[num],"Cold");powers[num++]=GF_COLD;
        strcpy(power_desc[num],"Electricity");powers[num++]=GF_ELEC;
        strcpy(power_desc[num],"Acid");powers[num++]=GF_ACID;
        strcpy(power_desc[num],"Poison");powers[num++]=GF_POIS;
        strcpy(power_desc[num],"Light");powers[num++]=GF_LITE;
        strcpy(power_desc[num],"Darkness");powers[num++]=GF_DARK;
	strcpy(power_desc[num],"Warp");powers[num++]=GF_DARK;
        strcpy(power_desc[num],"Water");powers[num++]=GF_WATER;
        strcpy(power_desc[num],"Wind");powers[num++]=GF_WIND;
        strcpy(power_desc[num],"Earth");powers[num++]=GF_EARTH;
        strcpy(power_desc[num],"Sound");powers[num++]=GF_SOUND;


        if(!num) {msg_print("There are no available elements!");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

        while (!endloop)
        {

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Elements %c-%c, *=List, ESC=exit) Choose your element... ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Elements %c-%c, *=List, ESC=exit) Choose your element... ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
                        strnfmt(tmp_val, 78, "Pick %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

        if (Power != 0) endloop = TRUE;
        }

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

        /* Pick the selected element */
        p_ptr->elemlord = Power;
}

void get_a_monster_body()
{
        int x = 0;
        int y = 0;
        int oldbody = 0;
        int monsters_choice[3000];
        int             Power = -1;
        int             num = 0, i;
	int             powers[36];
	char            power_desc[36][80];
	bool            flag, redraw;
        int             ask;
        char            choice, mchar;
        char            out_val[160], monstername[50];
        monster_race *r_ptr;

        /* Yay, another evil goto! :) Well, it saves time! */
        gotovar:

        num = 0;
        x = 0;
        y = 0;

        for (x = 0; x < max_r_idx; x++) monsters_choice[x] = 0;
        Term_erase(0, 0, 255);
        put_str("Choose a monster kind(enter a character): ", 0, 0);
        mchar = inkey();
        Term_erase(0, 0, 255);

        for (x = 1; x < max_r_idx; x++)
        {
                r_ptr = &r_info[x];
                if (r_ptr->level <= 15 && !(r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flags9 & RF9_SPECIAL_GENE) && !(r_ptr->flags7 & RF7_UNPLAYABLE) && (r_ptr->cursed == 0) && (r_ptr->level > 0))
                {
                        if (r_ptr->d_char == mchar)
                        {
                                monsters_choice[y] = x;
                                if (y < 35) y++;
                        }
                }
        }	  

        /* List the powers */
        for (x = 0; x < y; x++)
        {
                monster_race_desc(monstername, monsters_choice[x]); 
                strcpy(power_desc[num],monstername);powers[num++] = monsters_choice[x];
        }

        if(!num) {msg_print("There are no monsters of this kind!");goto gotovar;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Monsters %c-%c, *=List, ESC=exit) Choose a monster... ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Monsters %c-%c, *=List, ESC=exit) Choose a monster... ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			strnfmt(tmp_val, 78, "Use %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) Term_load();

	if (Power == -1) goto gotovar;
        oldbody = p_ptr->body_monster;
        p_ptr->body_monster = Power;
	evolution_compare(0, FALSE, FALSE);

        /* know_body_monster(); */
        if (!get_com("Are you sure? [y/n]", &choice))
	{
		p_ptr->body_monster = oldbody;
                goto gotovar;
	}
        if (choice != 'y' && choice != 'Y')
        {
                p_ptr->body_monster = oldbody;
                goto gotovar;
        }
}

/* Pick an element for Monsters Inner Elemental Mastery. */
void pick_monster_element()
{
	int                     Power = -1;
        int                     num = 0, i;

	int             powers[36];
	char            power_desc[36][80];

        bool            flag, redraw;
        bool            endloop = FALSE;

        int             ask;

        char            choice;

	char            out_val[160];
	monster_race	*r_ptr = &r_info[p_ptr->body_monster];
        
        /* List the powers */
	for (i = 1; i < MAX_RESIST; i++)
	{
		if ((i <= 12 || i == GF_PHYSICAL) || (r_ptr->resistances[i] > 0 && i != GF_DIVINATION && i != GF_LIFE_BLAST))
		{
        		strcpy(power_desc[num],get_element_name(i));powers[num++]=i;
		}
	}


        if(!num) {msg_print("There are no available elements!");return;}

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

        while (!endloop)
        {

	/* Build a prompt (accept all spells) */
	if (num <= 26)
	{
		/* Build a prompt (accept all spells) */
                strnfmt(out_val, 78, "(Elements %c-%c, *=List, ESC=exit) Choose your element... ",
			I2A(0), I2A(num - 1));
	}
	else
	{
                strnfmt(out_val, 78, "(Elements %c-%c, *=List, ESC=exit) Choose your element... ",
			I2A(0), '0' + num - 27);
	}

	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				byte y = 1, x = 0;
				int ctr = 0;
				char dummy[80];

				strcpy(dummy, "");

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				Term_save();

				prt ("", y++, x);

                                while (ctr < num && ctr < 19)
				{
					sprintf(dummy, "%c) %s", I2A(ctr), power_desc[ctr]);
					prt(dummy, y + ctr, x);
					ctr++;
				}
				while (ctr < num)
				{
					if (ctr < 26)
					{
						sprintf(dummy, " %c) %s", I2A(ctr), power_desc[ctr]);
					}
					else
					{
						sprintf(dummy, " %c) %s", '0' + ctr - 26, power_desc[ctr]);
					}
                                        prt(dummy, y + ctr - 19, x + 40);
					ctr++;
				}
                                if (ctr < 19)
				{
					prt ("", y + ctr, x);
				}
				else
				{
                                        prt ("", y + 19, x);
				}
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				Term_load();
			}

			/* Redo asking */
			continue;
		}

		if (choice == '\r' && num == 1)
		{
			choice = 'a';
		}

		if (isalpha(choice))
		{
			/* Note verify */
			ask = (isupper(choice));

			/* Lowercase */
			if (ask) choice = tolower(choice);

			/* Extract request */
			i = (islower(choice) ? A2I(choice) : -1);
		}
		else
		{
			ask = FALSE; /* Can't uppercase digits */

			i = choice - '0' + 26;
		}

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		Power = powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
                        strnfmt(tmp_val, 78, "Pick %s? ", power_desc[i]);

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

        if (Power != 0) endloop = TRUE;
        }

	/* Restore the screen */
	if (redraw) Term_load();

	/* Abort if needed */
	if (!flag) 
	{
		energy_use = 0;
                return;
	}

        /* Pick the selected element */
        p_ptr->elemlord = Power;
}

int essence_evolution_reduction(int r_idx)
{
	int i;
	int reduction = 0;
	object_type *o_ptr;

	p_ptr->cursed = 0;

	/* Scan the inventory */
	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		if (o_ptr->tval == TV_ESSENCE && o_ptr->pval == r_idx) reduction += 1;
	}

	if (reduction > 20) reduction = 20;

	return (reduction);
}
