/* File: birth-tnb.c */

/* Purpose: character generation and script commands */

/*
 * Copyright (c) 1997-2009 Tim Baker
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include <tcl.h>
#include "angband.h"
#include "tnb.h"
#include "interp.h"
#include "cmdinfo-dll.h"
#include "util-dll.h"

#if defined(ANGBANDTK) || defined(OANGBANDTK)

#define BIRTH_GENDER    1
#define BIRTH_RACE      2
#define BIRTH_CLASS     3
#define BIRTH_GENERATE  4

static char *keyword_birth[] = {
	"gender", "race", "class", "generate", NULL
};

#define STAT_LIMIT 53

#endif /* ANGBANDTK OANGBANDTK */

#if defined(KANGBANDTK)

#define BIRTH_GENDER    1
#define BIRTH_RACE      2
#define BIRTH_CLASS     3
#define BIRTH_PLOT      4
#define BIRTH_GENERATE  5

static char *keyword_birth[] = {
	"gender", "race", "class", "plot", "generate", NULL
};

#define STAT_LIMIT 53

#endif /* KANGBANDTK */

#if defined(ZANGBANDTK)

#undef SHOW_LIFE_RATE

#define BIRTH_GENDER    1
#define BIRTH_RACE      2
#define BIRTH_CLASS     3
#define BIRTH_REALM_1   4
#define BIRTH_REALM_2   5
#define BIRTH_GENERATE  6

static char *keyword_birth[] = {
	"gender", "race", "class", "realm1", "realm2", "quest", "generate", NULL
};

/*
 * Maximum number of tries for selection of a proper quest monster
 */
#define MAX_TRIES 100

#define STAT_LIMIT 56

#endif /* ZANGBANDTK */

/*
 * This is a debugging macro to make sure my switch statements match
 * the list of strings passed to Tcl_GetIndexFromObj().
 */
#define ASSERT_OPTION(table,index,string) \
	if (strcmp(table[index], string)) return TCL_ERROR;

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

	s16b stat[A_MAX];

	char history[4][60];

	s16b player_hp[PY_MAX_LEVEL];

#if defined(ZANGBANDTK)
	s16b chaos_patron;
#endif /* */
};

/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[(18 - 10) + 1] =
	{ 0, 1, 2, 4, 7, 11, 16, 22, 30 };

struct birth_info
{
	int stage;
	s16b stat_use[A_MAX];
	struct birther prev;
	bool has_prev;
	bool valid;
	int stats[A_MAX];
#if defined(ANGBANDTK)
#endif /* ANGBANDTK */
#if defined(ZANGBANDTK)
	int max_quest;
	char *realm_name[MAX_REALM + 1 + 1];
#endif /* ZANGBANDTK */
};

static struct birth_info *birth_ptr = NULL;

#if defined(ZANGBANDTK)

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
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
static hist_type h_info[] =
{
	{"You are the illegitimate and unacknowledged child ", 10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ", 20, 1, 2, 35},
	{"You are one of several children ", 95, 1, 2, 45},
	{"You are the first child ", 100, 1, 2, 50},

	{"of a Serf.  ", 40, 2, 3, 65},
	{"of a Yeoman.  ", 65, 2, 3, 80},
	{"of a Townsman.  ", 80, 2, 3, 90},
	{"of a Guildsman.  ", 90, 2, 3, 105},
	{"of a Landed Knight.  ", 96, 2, 3, 120},
	{"of a Noble Family in the Courts of Chaos.  ", 99, 2, 3, 130},
	{"of the Royal Blood Line of Amber.  ", 100, 2, 3, 140},

	{"You are the black sheep of the family.  ", 20, 3, 50, 20},
	{"You are a credit to the family.  ", 80, 3, 50, 55},
	{"You are a well liked child.  ", 100, 3, 50, 60},

	{"Your mother was of the Teleri.  ", 40, 4, 1, 50},
	{"Your father was of the Teleri.  ", 75, 4, 1, 55},
	{"Your mother was of the Noldor.  ", 90, 4, 1, 55},
	{"Your father was of the Noldor.  ", 95, 4, 1, 60},
	{"Your mother was of the Vanyar.  ", 98, 4, 1, 65},
	{"Your father was of the Vanyar.  ", 100, 4, 1, 70},

	{"You are one of several children ", 60, 7, 8, 50},
	{"You are the only child ", 100, 7, 8, 55},

	{"of a Teleri ", 75, 8, 9, 50},
	{"of a Noldor ", 95, 8, 9, 55},
	{"of a Vanyar ", 100, 8, 9, 60},

	{"Ranger.  ", 40, 9, 54, 80},
	{"Archer.  ", 70, 9, 54, 90},
	{"Warrior.  ", 87, 9, 54, 110},
	{"Mage.  ", 95, 9, 54, 125},
	{"Prince.  ", 99, 9, 54, 140},
	{"King.  ", 100, 9, 54, 145},

	{"You are one of several children of a Hobbit ", 85, 10, 11, 45},
	{"You are the only child of a Hobbit ", 100, 10, 11, 55},

	{"Bum.  ", 20, 11, 3, 55},
	{"Tavern Owner.  ", 30, 11, 3, 80},
	{"Miller.  ", 40, 11, 3, 90},
	{"Home Owner.  ", 50, 11, 3, 100},
	{"Burglar.  ", 80, 11, 3, 110},
	{"Warrior.  ", 95, 11, 3, 115},
	{"Mage.  ", 99, 11, 3, 125},
	{"Clan Elder.  ", 100, 11, 3, 140},

	{"You are one of several children of a Gnome ", 85, 13, 14, 45},
	{"You are the only child of a Gnome ", 100, 13, 14, 55},

	{"Beggar.  ", 20, 14, 3, 55},
	{"Braggart.  ", 50, 14, 3, 70},
	{"Prankster.  ", 75, 14, 3, 85},
	{"Warrior.  ", 95, 14, 3, 100},
	{"Mage.  ", 100, 14, 3, 125},

	{"You are one of two children of a Dwarven ", 25, 16, 17, 40},
	{"You are the only child of a Dwarven ", 100, 16, 17, 50},

	{"Thief.  ", 10, 17, 18, 60},
	{"Prison Guard.  ", 25, 17, 18, 75},
	{"Miner.  ", 75, 17, 18, 90},
	{"Warrior.  ", 90, 17, 18, 110},
	{"Priest.  ", 99, 17, 18, 130},
	{"King.  ", 100, 17, 18, 150},

	{"You are the black sheep of the family.  ", 15, 18, 57, 10},
	{"You are a credit to the family.  ", 85, 18, 57, 50},
	{"You are a well liked child.  ", 100, 18, 57, 55},

	{"Your mother was an Orc, but it is unacknowledged.  ", 25, 19, 20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ",	100, 19, 20, 25},

	{"You are the adopted child ", 100, 20, 2, 50},

	{"Your mother was a Cave-Troll ", 30, 22, 23, 20},
	{"Your father was a Cave-Troll ", 60, 22, 23, 25},
	{"Your mother was a Hill-Troll ", 75, 22, 23, 30},
	{"Your father was a Hill-Troll ", 90, 22, 23, 35},
	{"Your mother was a Water-Troll ", 95, 22, 23, 40},
	{"Your father was a Water-Troll ", 100, 22, 23, 45},

	{"Cook.  ", 5, 23, 62, 60},
	{"Warrior.  ", 95, 23, 62, 55},
	{"Shaman.  ", 99, 23, 62, 65},
	{"Clan Chief.  ", 100, 23, 62, 80},

	{"You have dark brown eyes, ", 20, 50, 51, 50},
	{"You have brown eyes, ", 60, 50, 51, 50},
	{"You have hazel eyes, ", 70, 50, 51, 50},
	{"You have green eyes, ", 80, 50, 51, 50},
	{"You have blue eyes, ", 90, 50, 51, 50},
	{"You have blue-gray eyes, ", 100, 50, 51, 50},

	{"straight ", 70, 51, 52, 50},
	{"wavy ", 90, 51, 52, 50},
	{"curly ", 100, 51, 52, 50},

	{"black hair, ", 30, 52, 53, 50},
	{"brown hair, ", 70, 52, 53, 50},
	{"auburn hair, ", 80, 52, 53, 50},
	{"red hair, ", 90, 52, 53, 50},
	{"blond hair, ", 100, 52, 53, 50},

	{"and a very dark complexion.", 10, 53, 0, 50},
	{"and a dark complexion.", 30, 53, 0, 50},
	{"and an average complexion.", 80, 53, 0, 50},
	{"and a fair complexion.", 90, 53, 0, 50},
	{"and a very fair complexion.", 100, 53, 0, 50},

	{"You have light grey eyes, ", 85, 54, 55, 50},
	{"You have light blue eyes, ", 95, 54, 55, 50},
	{"You have light green eyes, ", 100, 54, 55, 50},

	{"straight ", 75, 55, 56, 50},
	{"wavy ", 100, 55, 56, 50},

	{"black hair, and a fair complexion.", 75, 56, 0, 50},
	{"brown hair, and a fair complexion.", 85, 56, 0, 50},
	{"blond hair, and a fair complexion.", 95, 56, 0, 50},
	{"silver hair, and a fair complexion.", 100, 56, 0, 50},

	{"You have dark brown eyes, ", 99, 57, 58, 50},
	{"You have glowing red eyes, ", 100, 57, 58, 60},

	{"straight ", 90, 58, 59, 50},
	{"wavy ", 100, 58, 59, 50},

	{"black hair, ", 75, 59, 60, 50},
	{"brown hair, ", 100, 59, 60, 50},

	{"a one foot beard, ", 25, 60, 61, 50},
	{"a two foot beard, ", 60, 60, 61, 51},
	{"a three foot beard, ", 90, 60, 61, 53},
	{"a four foot beard, ", 100, 60, 61, 55},

	{"and a dark complexion.", 100, 61, 0, 50},

	{"You have slime green eyes, ", 60, 62, 63, 50},
	{"You have puke yellow eyes, ", 85, 62, 63, 50},
	{"You have blue-bloodshot eyes, ", 99, 62, 63, 50},
	{"You have glowing red eyes, ", 100, 62, 63, 55},

	{"dirty ", 33, 63, 64, 50},
	{"mangy ", 66, 63, 64, 50},
	{"oily ", 100, 63, 64, 50},

	{"sea-weed green hair, ", 33, 64, 65, 50},
	{"bright red hair, ", 66, 64, 65, 50},
	{"dark purple hair, ", 100, 64, 65, 50},

	{"and green ", 25, 65, 66, 50},
	{"and blue ", 50, 65, 66, 50},
	{"and white ", 75, 65, 66, 50},
	{"and black ", 100, 65, 66, 50},

	{"ulcerous skin.", 33, 66, 0, 50},
	{"scabby skin.", 66, 66, 0, 50},
	{"leprous skin.", 100, 66, 0, 50},

	{"You are an unacknowledged child of ", 50, 67, 68, 45},
	{"You are a rebel child of ", 80, 67, 68, 65},
	{"You are a long lost child of ", 100, 67, 68, 55},

	{"an unknown Amberite.  ", 50, 68, 50, 80},
	{"an unknown third generation Amberite.  ", 65, 68, 50, 90},
	{"an unknown second generation Amberite.  ", 79, 68, 50, 100},
	{"Oberon.  ", 80, 68, 50, 130},
	{"Osric.  ", 83, 68, 50, 105},
	{"Finndo.  ", 84, 68, 50, 105},
	{"Brand.  ", 85, 68, 50, 90},
	{"Flora.  ", 87, 68, 50, 100},
	{"Gerard.  ", 88, 68, 50, 125},
	{"Deirdre.  ", 89, 68, 50, 120},
	{"Random.  ", 90, 68, 50, 140},
	{"Benedict.  ", 91, 68, 50, 115},
	{"Corwin.  ", 92, 68, 50, 110},
	{"Julian.  ", 93, 68, 50, 105},
	{"Caine.  ", 94, 68, 50, 95},
	{"Bleys.  ", 95, 68, 50, 115},
	{"Fiona.  ", 96, 68, 50, 110},
	{"Eric.  ", 97, 68, 50, 135},
	{"Rinaldo.  ", 98, 68, 50, 90},
	{"Merlin.  ", 99, 68, 50, 105},
	{"Martin.  ", 100, 68, 50, 80},


	{"You are one of several children of a Dark Elven ", 85, 69, 70, 45},
	{"You are the only child of a Dark Elven ", 100, 69, 70, 55},

	{"Warrior.  ", 50, 70, 71, 60},
	{"Warlock.  ", 80, 70, 71, 75},
	{"Noble.  ", 100, 70, 71, 95},

	{"You have black eyes, ", 100, 71, 72, 50},

	{"straight ", 70, 72, 73, 50},
	{"wavy ", 90, 72, 73, 50},
	{"curly ", 100, 72, 73, 50},

	{"black hair and a very dark complexion.", 100, 73, 0, 50},

	{"Your mother was an Ogre, but it is unacknowledged.  ", 25, 74, 20, 25},
	{"Your father was an Ogre, but it is unacknowledged.  ", 100, 74, 20, 25},

	{"Your mother was a Hill Giant.  ", 10, 75, 20, 50},
	{"Your mother was a Fire Giant.  ", 12, 75, 20, 55},
	{"Your mother was a Frost Giant.  ", 20, 75, 20, 60},
	{"Your mother was a Cloud Giant.  ", 23, 75, 20, 65},
	{"Your mother was a Storm Giant.  ", 25, 75, 20, 70},
	{"Your father was a Hill Giant.  ", 60, 75, 20, 50},
	{"Your father was a Fire Giant.  ", 70, 75, 20, 55},
	{"Your father was a Frost Giant.  ", 80, 75, 20, 60},
	{"Your father was a Cloud Giant.  ", 90, 75, 20, 65},
	{"Your father was a Storm Giant.  ", 100, 75, 20, 70},

	{"Your father was an unknown Titan.  ", 75, 76, 20, 50},
	{"Your mother was Themis.  ", 80, 76, 20, 100},
	{"Your mother was Mnemosyne.  ", 85, 76, 20, 100},
	{"Your father was Okeanoas.  ", 90, 76, 20, 100},
	{"Your father was Crius.  ", 95, 76, 20, 100},
	{"Your father was Hyperion.  ", 98, 76, 20, 125},
	{"Your father was Kronos.  ", 100, 76, 20, 150},

	{"You are the offspring of an unknown Cyclops.  ", 90, 77, 109, 50},
	{"You are Polyphemos's child.  ", 98, 77, 109, 80},
	{"You are Uranos's child.  ", 100, 77, 109, 135},

	{"You are one of several children of ", 100, 78, 79, 50},

	{"a Brown Yeek. ", 50, 79, 80, 50},
	{"a Blue Yeek.  ", 75, 79, 80, 50},
	{"a Master Yeek.  ", 95, 79, 80, 85},
	{"Boldor, the King of the Yeeks.  ", 100, 79, 80, 120},

	{"You have pale eyes, ", 25, 80, 81, 50},
	{"You have glowing eyes, ", 50, 80, 81, 50},
	{"You have tiny black eyes, ", 75, 80, 81, 50},
	{"You have shining black eyes, ", 100, 80, 81, 50},

	{"no hair at all, ", 20, 81, 65, 50},
	{"short black hair, ", 40, 81, 65, 50},
	{"long black hair, ", 60, 81, 65, 50},
	{"bright red hair, ", 80, 81, 65, 50},
	{"colourless albino hair, ", 100, 81, 65, 50},

	{"You are one of several children of ", 100, 82, 83, 50},

	{"a Small Kobold.  ", 40, 83, 80, 50},
	{"a Kobold.  ", 75, 83, 80, 55},
	{"a Large Kobold.  ", 95, 83, 80, 65},
	{"Mughash, the Kobold Lord.  ", 100, 83, 80, 100},

	{"You are one of several children of a Klackon hive queen.  "
		,100, 84, 85, 50},

	{"You have red skin, ", 40, 85, 86, 50},
	{"You have black skin, ", 90, 85, 86, 50},
	{"You have yellow skin, ", 100, 85, 86, 50},

	{"and black eyes.", 100, 86, 0, 50},

	{"You are one of several children of ", 100, 87, 88, 89},

	{"a Nibelung Slave.  ", 30, 88, 18, 20},
	{"a Nibelung Thief.  ", 50, 88, 18, 40},
	{"a Nibelung Smith.  ", 70, 88, 18, 60},
	{"a Nibelung Miner.  ", 90, 88, 18, 75},
	{"a Nibelung Shaman.  ", 95, 88, 18, 100},
	{"Mime, the Nibelung.  ", 100, 88, 18, 100},

	{"You are ", 100, 89, 135, 50},

	{"the oldest child of a Draconian ", 30, 135, 90, 55},
	{"the youngest child of a Draconian ", 50, 135, 90, 50},
	{"the adopted child of a Draconian ", 55, 135, 90, 50},
	{"an orphaned child of a Draconian ", 60, 135, 90, 45},
	{"one of several children of a Draconian ", 85, 135, 90, 50},
	{"the only child of a Draconian ", 100, 135, 90, 55},

	{"Beggar.  ", 10, 90, 91, 20},
	{"Thief.  ", 21, 90, 91, 30},
	{"Sailor.  ", 26, 90, 91, 45},
	{"Mercenary.  ", 42, 90, 91, 45},
	{"Warrior.  ", 73, 90, 91, 50},
	{"Merchant.  ", 78, 90, 91, 50},
	{"Artisan.  ", 85, 90, 91, 55},
	{"Healer.  ", 89, 90, 91, 60},
	{"Priest.  ", 94, 90, 91, 65},
	{"Mage.  ", 97, 90, 91, 70},
	{"Scholar.  ", 99, 90, 91, 80},
	{"Noble.  ", 100, 90, 91, 100},

	{"You have ", 100, 91, 136, 50},

	{"charcoal wings, charcoal skin and a smoke-gray belly.", 11, 136, 0,
			50},
	{"bronze wings, bronze skin, and a copper belly.", 16, 136, 0, 50},
	{"golden wings, and golden skin.", 24, 136, 0, 50},
	{"white wings, and white skin.", 26, 136, 0, 60},
	{"blue wings, blue skin, and a cyan belly.", 32, 136, 0, 50},
	{"multi-hued wings, and multi-hued skin.", 33, 136, 0, 70},
	{"brown wings, and brown skin.", 37, 136, 0, 45},
	{"black wings, black skin, and a white belly.", 41, 136, 0, 50},
	{"lavender wings, lavender skin, and a white belly.", 48, 136, 0, 50},
	{"green wings, green skin and yellow belly.", 65, 136, 0, 50},
	{"green wings, and green skin.", 75, 136, 0, 50},
	{"red wings, and red skin.", 88, 136, 0, 50},
	{"black wings, and black skin.", 94, 136, 0, 50},
	{"metallic skin, and shining wings.", 100, 136, 0, 55},

	{"You have slimy skin, empty glowing eyes, and ", 100, 92, 93, 80},
	{"three tentacles around your mouth.", 20, 93, 0, 45},
	{"four tentacles around your mouth.", 80, 93, 0, 50},
	{"five tentacles around your mouth.", 100, 93, 0, 55},

	{"You ancestor was ", 100, 94, 95, 50},

	{"a mindless demonic spawn.  ", 30, 95, 96, 20},
	{"a minor demon.  ", 60, 95, 96, 50},
	{"a major demon.  ", 90, 95, 96, 75},
	{"a demon lord.  ", 100, 95, 96, 99},

	{"You have red skin, ", 50, 96, 97, 50},
	{"You have brown skin, ", 100, 96, 97, 50},

	{"claws, fangs, spikes, and glowing red eyes.", 40, 97, 0, 50},
	{"claws, fangs, and glowing red eyes.", 70, 97, 0, 50},
	{"claws, and glowing red eyes.", 100, 97, 0, 50},

	{"You were shaped from ", 100, 98, 99, 50},

	{"clay ", 40, 99, 100, 50},
	{"stone ", 80, 99, 100, 50},
	{"wood ", 85, 99, 100, 40},
	{"iron ", 99, 99, 100, 50},
	{"pure gold ", 100, 99, 100, 100},

	{"by a Kabbalist", 40, 100, 101, 50},
	{"by a Wizard", 65, 100, 101, 50},
	{"by an Alchemist", 90, 100, 101, 50},
	{"by a Priest", 100, 100, 101, 60},

	{" to fight evil.", 10, 101, 0, 65},
	{".", 100, 101, 0, 50},

	{"You were created by ", 100, 102, 103, 50},

	{"a Necromancer.  ", 30, 103, 104, 50},
	{"a magical experiment.  ", 50, 103, 104, 50},
	{"an Evil Priest.  ", 70, 103, 104, 50},
	{"a pact with the demons.  ", 75, 103, 104, 50},
	{"a restless spirit.  ", 85, 103, 104, 50},
	{"a curse.  ", 95, 103, 104, 30},
	{"an oath.  ", 100, 103, 104, 50},

	{"You have ", 100, 104, 105, 50},
	{"dirty, dry bones, ", 40, 105, 106, 50},
	{"rotten black bones, ", 60, 105, 106, 50},
	{"filthy, brown bones, ", 80, 105, 106, 50},
	{"shining white bones, ", 100, 105, 106, 50},

	{"and glowing eyes.", 30, 106, 0, 50},
	{"and eyes which burn with hellfire.", 50, 106, 0, 50},
	{"and empty eyesockets.", 100, 106, 0, 50},

	{"You were created by ", 100, 107, 108, 50},

	{"a Necromancer.  ", 30, 108, 62, 50},
	{"a Wizard.  ", 50, 108, 62, 50},
	{"a restless spirit.  ", 60, 108, 62, 50},
	{"an Evil Priest.  ", 70, 108, 62, 50},
	{"a pact with the demons.  ", 80, 108, 62, 50},
	{"a curse.  ", 95, 108, 62, 30},
	{"an oath.  ", 100, 108, 62, 50},

	{"You have a dark brown eye, ", 20, 109, 110, 50},
	{"You have a brown eye, ", 60, 109, 110, 50},
	{"You have a hazel eye, ", 70, 109, 110, 50},
	{"You have a green eye, ", 80, 109, 110, 50},
	{"You have a blue eye, ", 90, 109, 110, 50},
	{"You have a blue-gray eye, ", 100, 109, 110, 50},

	{"straight ", 70, 110, 111, 50},
	{"wavy ", 90, 110, 111, 50},
	{"curly ", 100, 110, 111, 50},

	{"black hair, ", 30, 111, 112, 50},
	{"brown hair, ", 70, 111, 112, 50},
	{"auburn hair, ", 80, 111, 112, 50},
	{"red hair, ", 90, 111, 112, 50},
	{"blond hair, ", 100, 111, 112, 50},

	{"and a very dark complexion.", 10, 112, 0, 50},
	{"and a dark complexion.", 30, 112, 0, 50},
	{"and an average complexion.", 80, 112, 0, 50},
	{"and a fair complexion.", 90, 112, 0, 50},
	{"and a very fair complexion.", 100, 112, 0, 50},

	{"You arose from an unmarked grave.  ", 20, 113, 114, 50},
	{"In life you were a simple peasant, the victim of a powerful Vampire Lord.  ",
		40, 113, 114, 50},
	{"In life you were a Vampire Hunter, but they got you.  ", 60, 113,
		114, 50},
	{"In life you were a Necromancer.  ", 80, 113, 114, 50},
	{"In life you were a powerful noble.  ", 95, 113, 114, 50},
	{"In life you were a powerful and cruel tyrant.  ", 100, 113, 114, 50},

	{"You have ", 100, 114, 115, 50},

	{"jet-black hair, ", 25, 115, 116, 50},
	{"matted brown hair, ", 50, 115, 116, 50},
	{"white hair, ", 75, 115, 116, 50},
	{"a hairless head, ", 100, 115, 116, 50},

	{"eyes like red coals, ", 25, 116, 117, 50},
	{"blank white eyes, ", 50, 116, 117, 50},
	{"feral yellow eyes, ", 75, 116, 117, 50},
	{"bloodshot red eyes, ", 100, 116, 117, 50},

	{"and a deathly pale complexion.", 100, 117, 0, 50},

	{"You were created by ", 100, 118, 119, 50},

	{"a Necromancer.  ", 30, 119, 134, 50},
	{"a magical experiment.  ", 50, 119, 134, 50},
	{"an Evil Priest.  ", 70, 119, 134, 50},
	{"a pact with the demons.  ", 75, 119, 134, 50},
	{"a restless spirit.  ", 85, 119, 134, 50},
	{"a curse.  ", 95, 119, 134, 30},
	{"an oath.  ", 100, 119, 134, 50},

	{"jet-black hair, ", 25, 120, 121, 50},
	{"matted brown hair, ", 50, 120, 121, 50},
	{"white hair, ", 75, 120, 121, 50},
	{"a hairless head, ", 100, 120, 121, 50},

	{"eyes like red coals, ", 25, 121, 122, 50},
	{"blank white eyes, ", 50, 121, 122, 50},
	{"feral yellow eyes, ", 75, 121, 122, 50},
	{"bloodshot red eyes, ", 100, 121, 122, 50},

	{" and a deathly gray complexion. ", 100, 122, 123, 50},
	{"An eerie green aura surrounds you.", 100, 123, 0, 50},

	{"Your parents were ", 100, 124, 125, 50},

	{"pixies.  ", 20, 125, 126, 35},
	{"nixies.  ", 30, 125, 126, 25},
	{"wood sprites.  ", 75, 125, 126, 50},
	{"wood spirits.  ", 90, 125, 126, 75},
	{"noble faerie folk.  ", 100, 125, 126, 85},

	{"You have light blue wings attached to your back, ", 100, 126, 127, 50},

	{"straight blond hair, ", 80, 127, 128, 50},
	{"wavy blond hair, ", 100, 127, 128, 50},

	{"blue eyes, and a very fair complexion.", 100, 128, 0, 50},

	{"You were produced by a magical experiment.  ", 30, 129, 130, 40},
	{"In your childhood, you were stupid enough to stick your head in raw Logrus.  ",
		50, 129, 130, 50},
	{"A Demon Lord of Chaos decided to have some fun, and so he created you.  ",
		60, 129, 130, 60},
	{"You are the magical crossbreed of an animal and a man.  ", 75, 129,
		130, 50},
	{"You are the blasphemous crossbreed of unspeakable creatures of chaos.  ",
		100, 129, 130, 30},


	{"You have green reptilian eyes, ", 60, 130, 131, 50},
	{"You have the black eyes of a bird, ", 85, 130, 131, 50},
	{"You have the orange eyes of a cat, ", 99, 130, 131, 50},
	{"You have the fiery eyes of a demon, ", 100, 130, 131, 55},

	{"no hair at all, ", 10, 131, 133, 50},
	{"dirty ", 33, 131, 132, 50},
	{"mangy ", 66, 131, 132, 50},
	{"oily ", 100, 131, 132, 50},

	{"brown fur, ", 33, 132, 133, 50},
	{"gray fur, ", 66, 132, 133, 50},
	{"albino fur, ", 100, 132, 133, 50},

	{"and the hooves of a goat.", 50, 133, 0, 50},
	{"and human feet.", 75, 133, 0, 50},
	{"and bird's feet.", 85, 133, 0, 50},
	{"and reptilian feet.", 90, 133, 0, 50},
	{"and bovine feet.", 95, 133, 0, 50},
	{"and feline feet.", 97, 133, 0, 50},
	{"and canine feet.", 100, 133, 0, 50},

	{"You have ", 100, 134, 120, 50},
};

static int get_realms(int realms[MAX_REALM + 1])
{
    int i, pclas = p_ptr->pclass;

	byte choices = 0;
	
	/* Return number of allowable realms */
	int count = 0;

    /* Always allow the "null" realm */
	realms[count++] = REALM_NONE;

    /* Some classes get no realms */
    if (realm_choices1[pclas] == (CH_NONE)) return count;

	/* Asking for realm1 */
	if (birth_ptr->stage == BIRTH_REALM_1)
	{
		/* Get choices for realm1 */
		choices = realm_choices1[pclas];
	}

	/* Asking for realm2 */
	if (birth_ptr->stage == BIRTH_REALM_2)
	{
		/* Get choices for realm2 */
		choices = realm_choices2[pclas];

		/* Can't have same realm1 and realm2 */
		choices &= ~(1 << (p_ptr->realm1 - 1));
	}
	
	/* Check each realm */
	for (i = 0; i < MAX_REALM; i++)
	{
		/* This realm is allowed */
		if (choices & (1 << i))
		{
			/* Collect this realm */
			realms[count++] = i + 1;
		}
	}

	/* Return number of allowable realms */
	return count;
}

#endif /* ZANGBANDTK */

/*
 * Save the current data for later
 */
static void save_prev_data(void)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	birth_ptr->prev.age = p_ptr->age;
	birth_ptr->prev.wt = p_ptr->wt;
	birth_ptr->prev.ht = p_ptr->ht;
	birth_ptr->prev.sc = p_ptr->sc;
	birth_ptr->prev.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
		birth_ptr->prev.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(birth_ptr->prev.history[i], p_ptr_history[i]);
	}

	/* Save player_hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
		birth_ptr->prev.player_hp[i] = p_ptr_player_hp[i];

#if defined(ZANGBANDTK)
	birth_ptr->prev.chaos_patron = p_ptr->chaos_patron;
#endif /* */
}


/*
 * Load the previous data
 */
static void load_prev_data(void)
{
	int i;

	struct birther temp;


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
		strcpy(temp.history[i], p_ptr_history[i]);
	}

	/* Save player_hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
		temp.player_hp[i] = p_ptr_player_hp[i];

#if defined(ZANGBANDTK)
	temp.chaos_patron = p_ptr->chaos_patron;
#endif /* */

	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = birth_ptr->prev.age;
	p_ptr->wt = birth_ptr->prev.wt;
	p_ptr->ht = birth_ptr->prev.ht;
	p_ptr->sc = birth_ptr->prev.sc;
	p_ptr->au = birth_ptr->prev.au;

	/* Load the stats */
	for (i = 0; i < 6; i++)
	{
		p_ptr->stat_max[i] = birth_ptr->prev.stat[i];
		p_ptr->stat_cur[i] = birth_ptr->prev.stat[i];
	}

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(p_ptr_history[i], birth_ptr->prev.history[i]);
	}

	/* Load player_hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
		p_ptr_player_hp[i] = birth_ptr->prev.player_hp[i];

#if defined(ZANGBANDTK)
	p_ptr->chaos_patron = birth_ptr->prev.chaos_patron;
#endif /* */

	/*** Save the current data ***/

	/* Save the data */
	birth_ptr->prev.age = temp.age;
	birth_ptr->prev.wt = temp.wt;
	birth_ptr->prev.ht = temp.ht;
	birth_ptr->prev.sc = temp.sc;
	birth_ptr->prev.au = temp.au;

	/* Save the stats */
	for (i = 0; i < 6; i++)
	{
		birth_ptr->prev.stat[i] = temp.stat[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(birth_ptr->prev.history[i], temp.history[i]);
	}

	/* Save player_hp */
	for (i = 0; i < PY_MAX_LEVEL; i++)
		birth_ptr->prev.player_hp[i] = temp.player_hp[i];

#if defined(ZANGBANDTK)
	birth_ptr->prev.chaos_patron = temp.chaos_patron;
#endif /* */
}

/*
 * Adjust a stat by an amount
 *
 * This just uses "modify_stat_value()" unless "maximize" mode is false,
 * and a positive bonus is being applied, in which case, a special hack
 * is used, with the "auto_roll" flag affecting the result.
 *
 * The "auto_roll" flag selects "maximal" changes for use with the
 * auto-roller initialization code.  Otherwise, if "maximize" mode
 * is being used, the changes are fixed.  Otherwise, semi-random
 * changes will occur, with larger changes at lower values.
 */
static int adjust_stat(int value, int amount, int auto_roll)
{
	/* Negative amounts or maximize mode */
	if ((amount < 0) || p_ptr_maximize)
	{
		return (modify_stat_value(value, amount));
	}

	/* Special hack */
	else
	{
		int i;

		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else if (value < 18 + 70)
			{
				value += ((auto_roll ? 15 : randint(15)) + 5);
			}
			else if (value < 18 + 90)
			{
				value += ((auto_roll ? 6 : randint(6)) + 2);
			}
			else if (value < 18 + 100)
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

	/* Using point-based generation */
	if (p_ptr_point_based)
	{
		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Variable stat maxes */
			if (p_ptr_maximize)
			{
				/* Reset stats */
				p_ptr->stat_cur[i] = p_ptr->stat_max[i] = birth_ptr->stats[i];
			}

			/* Fixed stat maxes */
			else
			{
				/* Obtain a "bonus" for "race" and "class" */
				int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

				/* Apply the racial/class bonuses */
				p_ptr->stat_cur[i] = p_ptr->stat_max[i] =
					modify_stat_value(birth_ptr->stats[i], bonus);
			}
		}

		/* Done */
		return;
	}

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
		if ((j > 42) && (j <= STAT_LIMIT)) break;
	}

	/* Acquire the stats */
	for (i = 0; i < 6; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3 * i] + dice[3 * i + 1] + dice[3 * i + 2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Variable stat maxes */
		if (p_ptr_maximize)
		{
			/* Start fully healed */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/class bonuses */
			birth_ptr->stat_use[i] = modify_stat_value(p_ptr->stat_max[i],
				bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
			birth_ptr->stat_use[i] = adjust_stat(p_ptr->stat_max[i], bonus,
				FALSE);

			/* Save the resulting stat maximum */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = birth_ptr->stat_use[i];
		}
	}
}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)

/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int i, j, min_value, max_value;

#if defined(ZANGBANDTK)
#ifdef SHOW_LIFE_RATE
	int percent;
#endif
#endif /* ZANGBANDTK */

	/* Level one */
	p_ptr_max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

#if defined(ZANGBANDTK)

	/* Initialize arena and rewards information -KMW- */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = 0;
	p_ptr->inside_quest = 0;
	p_ptr->leftbldg = FALSE;
	p_ptr->exit_bldg = TRUE; /* only used for arena now -KMW- */

	/* Reset rewards */
	for (i = 0; i < MAX_BACT; i++)
	{
		p_ptr->rewards[i] = 0;
	}

#endif /* ZANGBANDTK */

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
	p_ptr_player_hp[0] = p_ptr->hitdie;

#if defined(OANGBANDTK)

	/* Hack - Get the hitpoints for non-random hp characters.  
	 * Each level provides exactly average hitpoints.
	 * If the average is a fraction, alternate.
	 */
	if (!adult_random_hitpoints)
	{
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			p_ptr->player_hp[i] =
				p_ptr->player_hp[i - 1] + p_ptr->hitdie / 2;
			if (((p_ptr->hitdie % 2) == 1) & ((i % 2) == 1))
				p_ptr->player_hp[i]++;
		}

	}
	/* Hack, if the player wants, use old-style rolled hitpoints */
	else

#endif

	/* Roll out the hitpoints */
	while (TRUE)
	{
		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
			j = randint(p_ptr->hitdie);
			p_ptr_player_hp[i] = p_ptr_player_hp[i - 1] + j;
#endif
#if defined(ZANGBANDTK)
			/* Add in racial hit dice */
			j = randint(rp_ptr->r_mhp);
			p_ptr_player_hp[i] = p_ptr_player_hp[i - 1] + j;

			/* If class hit dice is non zero - add it on */
			if (cp_ptr->c_mhp)
			{
				player_hp[i] += randint(cp_ptr->c_mhp);
			}
#endif
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (p_ptr_player_hp[PY_MAX_LEVEL-1] < min_value) continue;
		if (p_ptr_player_hp[PY_MAX_LEVEL-1] > max_value) continue;

		/* Acceptable */
		break;
	}

#ifdef SHOW_LIFE_RATE

	percent = (int) (((long) player_hp[PY_MAX_LEVEL - 1] * 200L) /
		(2 * p_ptr->hitdie +
			((PY_MAX_LEVEL - 1) * (p_ptr->hitdie + 1))));

	msg_format("Current Life Rating is %d/100.", percent);
	message_flush();

#endif /* SHOW_LIFE_RATE */
}

#endif /* A K Z */

#if defined(OANGBANDTK)

/*
 * Roll for some info that the auto-roller ignores
 *
 * No characters will have HPs at level 50 that differ from the average.
 * HPs will never differ from the average by more than 2 * character hitdice
 * on any level.
 */
static void get_extra(void)
{
	int i, j;
	int final_hps;


	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;


	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Final hitpoints = hitdice + (49 * (average roll)) */
	final_hps =
		p_ptr->hitdie + (((PY_MAX_LEVEL + 1) * (p_ptr->hitdie + 1)) / 2);

	/* Pre-calculate level 1 hitdice */
	p_ptr->player_hp[0] = p_ptr->hitdie;


	/* Roll out the hitpoints */
	for (i = 1; i < PY_MAX_LEVEL - 1; i++)
	{
		/* Expected previous level's HPs. */
		int average =
			p_ptr->hitdie + (((i - 1) * (p_ptr->hitdie + 1)) / 2);

		/* Difference between previous level's HPs and the average */
		int diff = average - p_ptr->player_hp[i - 1];

		/* Make adjustments near the end or where necessary */
		if (i >= (PY_MAX_LEVEL - 6) ||
			rand_int(p_ptr->hitdie * 2) < ABS(diff))
		{
			/* If previous level's HPs < average, bias for a large gain. */
			if (average > p_ptr->player_hp[i - 1])
			{
				/* Strong bias if needed. */
				if (ABS(diff) >= p_ptr->hitdie)
					j = p_ptr->hitdie;

				/* Relatively small one otherwise. */
				else
					j =
						p_ptr->hitdie - rand_int(p_ptr->hitdie -
						ABS(diff));
			}

			/* If previous level's HPs > average, bias for a small gain. */
			else if (average < p_ptr->player_hp[i - 1])
			{
				/* Strong bias if needed. */
				if (ABS(diff) >= p_ptr->hitdie)
					j = 1;

				/* Relatively small one otherwise. */
				else
					j = 1 + rand_int(p_ptr->hitdie - 1 - ABS(diff));
			}

			/* No bias necessary. */
			else
				j = randint(p_ptr->hitdie - 1);
		}

		/* Usually no bias -- average gain of half the hitdice. */
		else
			j = randint(p_ptr->hitdie - 1);

		/* Add this level's HP gain to the previous level's HPs. */
		p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + j;
	}

	/* 
	 * Final HPs are always constant. 
	 */
	p_ptr->player_hp[PY_MAX_LEVEL - 1] = final_hps;
}

#endif /* OANGBANDTK */

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

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	int bg_max = z_info->h_max;
#endif
#if defined(OANGBANDTK)
	int bg_max = MAX_H_IDX;
#endif
#if defined(ZANGBANDTK)
	int bg_max = sizeof(h_info) / sizeof(hist_type);
#endif

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
		if (h_info[i].chart != chart) continue;

		/* The chart exists */
		chart_exists = TRUE;
		
		/* Validate the "next" chart recursively */
		validate_bg_aux(h_info[i].next, chart_checked, buf);
		
		/* Require a terminator */
		if (h_info[i].roll != 100) continue;
		
		/* The chart is complete */
		chart_complete = TRUE;
	}

	/* Failed: The chart does not exist */
	if (!chart_exists)
	{
		quit_fmt("birth.c: h_info[] chart %d does not exist\n%s", chart, buf);
	}

	/* Failed: The chart is not complete */
	if (!chart_complete)
	{
		quit_fmt("birth.c: h_info[] chart %d missing 100% terminator", chart);
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

	int race_chart[100 /* MAX_P_IDX */];

	bool chart_checked[512];

	char buf[1024];
	
	for (i = 0; i < 512; i++) chart_checked[i] = FALSE;

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	for (i = 0; i < MAX_P_IDX; i++)
	{
		race_chart[i] = p_info[i].hist;
	}
#endif

#if defined(ZANGBANDTK)
	race_chart[RACE_AMBERITE] = 67;
	race_chart[RACE_HUMAN] = 1;
	race_chart[RACE_BARBARIAN] = 1;
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
	race_chart[RACE_HALF_TITAN] = 76;
	race_chart[RACE_CYCLOPS] = 77;
	race_chart[RACE_YEEK] = 78;
	race_chart[RACE_KOBOLD] = 82;
	race_chart[RACE_KLACKON] = 84;
	race_chart[RACE_NIBELUNG] = 87;
	race_chart[RACE_DRACONIAN] = 89;
	race_chart[RACE_MIND_FLAYER] = 92;
	race_chart[RACE_IMP] = 94;
	race_chart[RACE_GOLEM] = 98;
	race_chart[RACE_SKELETON] = 102;
	race_chart[RACE_ZOMBIE] = 107;
	race_chart[RACE_VAMPIRE] = 113;
	race_chart[RACE_SPECTRE] = 118;
	race_chart[RACE_SPRITE] = 124;
	race_chart[RACE_BEASTMAN] = 129;
#endif /* ZANGBANDTK */

	/* Check each race */
	for (race = 0; race < MAX_P_IDX; race++)
	{
		/* Get the first chart for this race */
		int chart = race_chart[race];

		(void) strcpy(buf, "");
		
		/* Validate the chart recursively */
		validate_bg_aux(chart, chart_checked, buf);
	}
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
	for (i = 0; i < 4; i++) p_ptr_history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

	/* Starting place */
	chart = rp_ptr->hist;

	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);

		/* Access the proper entry in the table */
		while ((chart != h_info[i].chart) || (roll > h_info[i].roll))
			i++;

		/* Acquire the textual history */
		strcat(buf, (h_text + h_info[i].text));

		/* Add in the social class */
		social_class += (int) (h_info[i].bonus) - 50;

		/* Enter the next chart */
		chart = h_info[i].next;
	}

#endif /* A, K, O */

#if defined(ZANGBANDTK)

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
		while ((chart != h_info[i].chart) || (roll > h_info[i].roll))
			i++;

		/* Acquire the textual history */
		(void) strcat(buf, h_info[i].info);

		/* Add in the social class */
		social_class += (int) (h_info[i].bonus) - 50;

		/* Enter the next chart */
		chart = h_info[i].next;
	}

#endif /* Z */


	/* Verify social class */
	if (social_class > 100)
		social_class = 100;
	else if (social_class < 1)
		social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */ ;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n - 1] == ' '))
		s[--n] = '\0';


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
			strcpy(p_ptr_history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 60; ((n > 0) && (s[n - 1] != ' ')); n--) /* loop */ ;

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n - 1] == ' '))
			s[--n] = '\0';

		/* Save one line of history */
		strcpy(p_ptr_history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */ ;
	}
}


#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

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
		p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = Rand_normal(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = Rand_normal(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
	}
}

#endif /* A, K, O */

#if defined(ZANGBANDTK)

/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	int h_percent;

	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		h_percent = (int) (p_ptr->ht) * 100 / (int) (rp_ptr->m_b_ht);
		p_ptr->wt =
			randnor((int) (rp_ptr->m_b_wt) * h_percent / 100,
			(int) (rp_ptr->m_m_wt) * h_percent / 300);
	}
	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);

		h_percent = (int) (p_ptr->ht) * 100 / (int) (rp_ptr->f_b_ht);
		p_ptr->wt =
			randnor((int) (rp_ptr->f_b_wt) * h_percent / 100,
			(int) (rp_ptr->f_m_wt) * h_percent / 300);
	}
}

#endif /* ZANGBANDTK */

/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i, gold;

	if (p_ptr_point_based)
	{
		int cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Total cost */
			cost += birth_stat_costs[birth_ptr->stats[i] - 10];
		}

		/* Gold is inversely proportional to cost */
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(ZANGBANDTK)
		p_ptr->au = (100 * (48 - cost)) + 100;
#endif
#if defined(OANGBANDTK)
		p_ptr->au = (50 * (48 - cost)) + 100;
#endif

#if defined(OANGBANDTK)
		/* She charmed the banker into it! */
		/* Mum and Dad figure she won't blow it on beer! -LM- */
		if (p_ptr->psex == SEX_FEMALE)
			p_ptr->au += 50; /* restored in OAngband */
#endif /* */

		return;
	}

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Process the stats */
	for (i = 0; i < 6; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (birth_ptr->stat_use[i] >= 18 + 50)
			gold -= 300;
		else if (birth_ptr->stat_use[i] >= 18 + 20)
			gold -= 200;
		else if (birth_ptr->stat_use[i] > 18)
			gold -= 150;
		else
			gold -= (birth_ptr->stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100)
		gold = 100;

#if defined(OANGBANDTK)
	/* She charmed the banker into it! -CJS- */
	/* Mum and Dad figure she won't blow it on beer! -LM- */
	if (p_ptr->psex == SEX_FEMALE)
		gold += 50; /* restored in OAngband */
#endif /* */

	/* Save the gold */
	p_ptr->au = gold;
}

/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;


	/* Wipe the player */
	(void) WIPE(p_ptr, player_type);

#if defined(ZANGBANDTK)

	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], "");
	}

	/* Wipe the quests */
	for (i = 0; i < max_quests; i++)
	{
		quest[i].status = QUEST_STATUS_UNTAKEN;

		quest[i].cur_num = 0;
		quest[i].max_num = 0;
		quest[i].type = 0;
		quest[i].level = 0;
		quest[i].r_idx = 0;
	}

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

#endif /* ZANGBANDTK */

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artifacts made yet */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		ART_CURNUM(a_ptr) = 0;
	}

#if defined(ANGBANDTK) || defined(OANGBANDTK)

	/* Start with no quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		q_list[i].level = 0;
	}

	/* Add a special quest */
	q_list[0].level = 99;

	/* Add a second quest */
	q_list[1].level = 100;

#endif /* ANGBANDTK, OANGBANDTK */

#if defined(KANGBANDTK)

	/* Start with no quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		quest[i].status = QUEST_STATUS_UNTAKEN;
		quest[i].cur_num = 0;
		quest[i].max_num = 0;
		quest[i].type = 0;
		quest[i].level = 0;
		quest[i].r_idx = 0;
	}

	/* Add a special quest */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = QUEST_SAURON;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);
	quest[QUEST_SAURON].status = QUEST_STATUS_TAKEN;
	p_ptr->inside_quest = 0;

	/* Add a second quest */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = QUEST_MORGOTH;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);
	quest[QUEST_MORGOTH].status = QUEST_STATUS_TAKEN;
	p_ptr->inside_quest = 0;


	/* Wipe the arena and special buildings -KMW- */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = 0;
	p_ptr->leftbldg = FALSE;
	p_ptr->exit_bldg = TRUE;

	/* Reset building rewards */
	for (i = 0; i < MAX_REWARDS; i++)
	{
		p_ptr->rewards[i] = 0;
	}

#endif /* KANGBANDTK */

#if defined(ANGBANDTK) || defined(OANGBANDTK)

	/* Reset the "objects" */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}

#endif /* */

#if defined(KANGBANDTK)

	/* Reset the "objects" */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;

		/* Reset "auto_destroy" */
		k_ptr->auto_dest = FALSE;

		/* Reset "auto_pickup" */
		k_ptr->auto_pick = FALSE;
	}

#endif /* */

#if defined(ZANGBANDTK)

	/* Reset the objects */
	k_info_reset();

#endif /* */

	/* Reset the "monsters" */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & RF1_UNIQUE)
			r_ptr->max_num = 1;
#if defined(ZANGBANDTK)
		if (r_ptr->flags3 & RF3_UNIQUE_7)
			r_ptr->max_num = 7;
#endif /* ZANGBANDTK */

		LORE_NTH(i).LF(pkills) = 0;
	}


#if defined(ANGBANDTK) || defined(KANGBANDTK)
	/* Hack -- no ghosts */
	r_info[MAX_R_IDX-1].max_num = 0;
#endif /* ANGBANDTK, KANGBANDTK */

	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	/* None of the spells have been learned yet */
	for (i = 0; i < PY_MAX_SPELLS; i++)
		p_ptr->spell_order[i] = 99;
#endif /* ANGBANDTK, KANGBANDTK */

#if defined(KANGBANDTK)
	/* Default pet command settings */
	p_ptr->pet_follow_dist = PET_FOLLOW_DIST;
	p_ptr->pet_open_doors = FALSE;
	p_ptr->pet_pickup_items = FALSE;
#endif /* */

#if defined(ZANGBANDTK)
	/* Wipe the spells */
	spell_learned1 = spell_learned2 = 0L;
	spell_worked1 = spell_worked2 = 0L;
	spell_forgotten1 = spell_forgotten2 = 0L;
	for (i = 0; i < 64; i++)
		spell_order[i] = 99;

	/* Clean the mutation count */
	mutant_regenerate_mod = 100;

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

	/* Default pet command settings */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
	p_ptr->pet_open_doors = FALSE;
	p_ptr->pet_pickup_items = FALSE;

#endif /* ZANGBANDTK */
}

/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static const byte player_init[MAX_CLASS][3][2] =
{
#if defined(ANGBANDTK) || defined(KANGBANDTK)

	{
		/* Warrior */
		{ TV_POTION, SV_POTION_BESERK_STRENGTH },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL }
	},

	{
		/* Mage */
		{ TV_MAGIC_BOOK, 0 },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL }
	},

	{
		/* Priest */
		{ TV_PRAYER_BOOK, 0 },
		{ TV_HAFTED, SV_MACE },
		{ TV_POTION, SV_POTION_HEALING }
	},

	{
#if defined(ANGBANDTK)
		/* Rogue */
		{ TV_MAGIC_BOOK, 0 },
#endif
#if defined(KANGBANDTK)
		/* Rogue (now uses illusions -KMW-) */
		{ TV_ILLUSION_BOOK, 0 },
#endif /* KANGBANDTK */
		{ TV_SWORD, SV_SMALL_SWORD },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
	},

	{
		/* Ranger */
		{ TV_MAGIC_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_BOW, SV_LONG_BOW }
	},

	{
		/* Paladin */
		{ TV_PRAYER_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL }
#if defined(KANGBANDTK)
	},

	{
		/* Illusionist  -KMW-  */
		{ TV_ILLUSION_BOOK, 0 },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL }
	},

	{
		/* Druid -KMW- */
		{ TV_NATURE_BOOK, 0 },
		{ TV_HAFTED, SV_MACE },
		{ TV_POTION, SV_POTION_HEALING }
#endif /* KANGBANDTK */
	}
#endif /* ANGBANDTK, KANGBANDTK */

#if defined(OANGBANDTK)
	{
		/* Warrior */
		{TV_POTION, SV_POTION_BERSERK_STR},
		{TV_SWORD, SV_LONG_SWORD},
		{TV_HARD_ARMOR, SV_CHAIN_MAIL}
	},

	{
		/* Mage */
		{TV_MAGIC_BOOK, 0},
		{TV_SCROLL, SV_SCROLL_TELEPORT},
		{TV_SCROLL, SV_SCROLL_WORD_OF_RECALL}
	},

	{
		/* Priest */
		{TV_PRAYER_BOOK, 0},
		{TV_HAFTED, SV_MACE},
		{TV_POTION, SV_POTION_HEALING}
	},

	{
		/* Rogue */
		{TV_MAGIC_BOOK, 0},
		{TV_SWORD, SV_DAGGER},
		{TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
	},

	{
		/* Ranger */
		{TV_DRUID_BOOK, 0},
		{TV_SWORD, SV_SMALL_SWORD},
		{TV_BOW, SV_SHORT_BOW}
	},

	{
		/* Paladin */
		{TV_PRAYER_BOOK, 0},
		{TV_HAFTED, SV_MACE},
		{TV_POTION, SV_POTION_HEROISM}
	},

	{
		/* Druid */
		{TV_DRUID_BOOK, 0},
		{TV_POTION, SV_POTION_CURE_POISON},
		{TV_POTION, SV_POTION_HEALING}
	},

	{
		/* Necromancer */
		{TV_NECRO_BOOK, 0},
		{TV_HAFTED, SV_WHIP},
		{TV_CLOAK, SV_CLOAK}
	},

	{
		/* Assassin */
		{TV_NECRO_BOOK, 0},
		{TV_SWORD, SV_MAIN_GAUCHE},
		{TV_BOW, SV_SLING}
	}
#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)

	{
		/* Warrior */
		{TV_RING, SV_RING_RES_FEAR}, /* Warriors need it! */
		{TV_SWORD, SV_BROAD_SWORD},
		{TV_HARD_ARMOR, SV_CHAIN_MAIL}
	},

	{
		/* Mage */
		{TV_SORCERY_BOOK, 0}, /* Hack: for realm1 book */
		{TV_SWORD, SV_DAGGER},
		{TV_DEATH_BOOK, 0} /* Hack: for realm2 book */
	},

	{
		/* Priest */
		{TV_SORCERY_BOOK, 0}, /* Hack: for Life / Death book */
		{TV_HAFTED, SV_MACE},
		{TV_DEATH_BOOK, 0} /* Hack: for realm2 book */
	},

	{
		/* Rogue */
		{TV_SORCERY_BOOK, 0}, /* Hack: for realm1 book */
		{TV_SWORD, SV_DAGGER},
		{TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
	},

	{
		/* Ranger */
		{TV_NATURE_BOOK, 0},
		{TV_SWORD, SV_DAGGER},
		{TV_DEATH_BOOK, 0} /* Hack: for realm2 book */
	},

	{
		/* Paladin */
		{TV_SORCERY_BOOK, 0},
		{TV_SWORD, SV_BROAD_SWORD},
		{TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL}
	},

	{
		/* Warrior-Mage */
		{TV_SORCERY_BOOK, 0}, /* Hack: for realm1 book */
		{TV_SWORD, SV_SHORT_SWORD},
		{TV_DEATH_BOOK, 0} /* Hack: for realm2 book */
	},

	{
		/* Chaos Warrior */
		{TV_SORCERY_BOOK, 0}, /* Hack: For realm1 book */
		{TV_SWORD, SV_BROAD_SWORD},
		{TV_HARD_ARMOR, SV_METAL_SCALE_MAIL}
	},

	{
		/* Monk */
		{TV_SORCERY_BOOK, 0},
		{TV_POTION, SV_POTION_HEALING},
		{TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
	},

	{
		/* Mindcrafter */
		{TV_SWORD, SV_SMALL_SWORD},
		{TV_POTION, SV_POTION_RESTORE_MANA},
		{TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
	},

	{
		/* High Mage */
		{TV_SORCERY_BOOK, 0}, /* Hack: for realm1 book */
		{TV_SWORD, SV_DAGGER},
		{TV_RING, SV_RING_SUSTAIN_INT}
	}

#endif /* ZANGBANDTK */
};

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

	int i, tv, sv;

	object_type *i_ptr;
	object_type object_type_body;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some food */
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
	i_ptr->number = rand_range(3, 7);
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some torches */
	object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	i_ptr->number = rand_range(3, 7);
	i_ptr->pval = rand_range(3, 7) * 500;
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack -- Give the player an object */
		object_prep(i_ptr, lookup_kind(tv, sv));
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);
	}

#if defined(OANGBANDTK)
	/* Hack - Rangers start off with some arrows. -LM- */
	if (p_ptr->pclass == CLASS_RANGER)
	{
		object_prep(i_ptr, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
		i_ptr->number = 25;
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);
	}
#endif

#endif /* ANGBANDTK, KANGBANDTK */

#if defined(ZANGBANDTK)

	int i, tv, sv;

	object_type forge;
	object_type *q_ptr;


	/* Get local object */
	q_ptr = &forge;

	/* Give the player some food */
	switch (p_ptr->prace)
	{
		case RACE_GOLEM:
		case RACE_SKELETON:
		case RACE_ZOMBIE:
		case RACE_VAMPIRE:
		case RACE_SPECTRE:
			{
				/* Scrolls of satisfy hunger */
				object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_SATISFY_HUNGER));
				q_ptr->number = (byte) rand_range(2, 5);
				object_aware(q_ptr);
				object_known(q_ptr);

				/* These objects are "storebought" */
				q_ptr->ident |= IDENT_STOREB;

				(void) inven_carry(q_ptr);

				break;
			}
		default:
			{
				/* Food rations */
				object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
				q_ptr->number = (byte) rand_range(3, 7);
				object_aware(q_ptr);
				object_known(q_ptr);
				(void) inven_carry(q_ptr);
			}
	}

	/* Get local object */
	q_ptr = &forge;

	if (p_ptr->prace == RACE_VAMPIRE)
	{
		/* Hack -- Give the player scrolls of DARKNESS! */
		object_prep(q_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_DARKNESS));

		q_ptr->number = (byte) rand_range(2, 5);

		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		(void) inven_carry(q_ptr);
	}
	else
	{
		/* Hack -- Give the player some torches */
		object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
		q_ptr->number = (byte) rand_range(3, 7);
		q_ptr->pval = rand_range(3, 7) * 500;
		object_aware(q_ptr);
		object_known(q_ptr);
		(void) inven_carry(q_ptr);
	}

	/* Get local object */
	q_ptr = &forge;

	if (p_ptr->pclass == CLASS_RANGER)
	{
		/* Hack -- Give the player some arrows */
		object_prep(q_ptr, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
		q_ptr->number = (byte) rand_range(15, 20);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);
		(void) inven_carry(q_ptr);

		/* Hack -- Give the player some arrows */
		object_prep(q_ptr, lookup_kind(TV_BOW, SV_SHORT_BOW));

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);
		(void) inven_carry(q_ptr);
	}
	else if (p_ptr->pclass == CLASS_HIGH_MAGE)
	{
		/* Hack -- Give the player some arrows */
		object_prep(q_ptr, lookup_kind(TV_WAND, SV_WAND_MAGIC_MISSILE));
		q_ptr->number = 1;
		q_ptr->pval = (byte) rand_range(25, 30);

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);
		(void) inven_carry(q_ptr);
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Hack to initialize spellbooks */
		if (tv == TV_SORCERY_BOOK)
			tv = TV_LIFE_BOOK + p_ptr->realm1 - 1;
		else if (tv == TV_DEATH_BOOK)
			tv = TV_LIFE_BOOK + p_ptr->realm2 - 1;

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
			p_ptr->prace == RACE_BARBARIAN)
			/* Barbarians do not need a ring of resist fear */
			sv = SV_RING_SUSTAIN_STR;

		/* Get local object */
		q_ptr = &forge;

		/* Hack -- Give the player an object */
		object_prep(q_ptr, lookup_kind(tv, sv));

		/* Assassins begin the game with a poisoned dagger */
		if (tv == TV_SWORD && p_ptr->pclass == CLASS_ROGUE &&
			p_ptr->realm1 == REALM_DEATH) /* Only assassins get a poisoned weapon */
		{
			q_ptr->name2 = EGO_BRAND_POIS;
		}

		/* These objects are "storebought" */
		q_ptr->ident |= IDENT_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);
		(void) inven_carry(q_ptr);
	}

#endif /* ZANGBANDTK */
}

/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	int i, j;

	validate_bg();

	/* Set up the character creation display */
	angtk_eval("angband_birth", "setup", NULL);

	/* Wait until the Term is fed a keypress, signalling completion. */
	(void) inkey();

#if defined(OANGBANDTK)
	/* Now that the player information is available, we are able to generate 
	 * random artifacts.
	 */
	initialize_random_artifacts();
#endif /* */

#if defined(ZANGBANDTK)
	/* Create a note file if that option is set */
	if (take_notes)
	{
		add_note_type(NOTE_BIRTH);
	}
#endif /* */

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	message_add(" ", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add("====================", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);
#endif /* A */
#if defined(ZANGBANDTK)
	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add("  ");
	message_add(" ");
#endif /* K, O, Z */

	/* Hack -- outfit the player */
	player_outfit();

#if defined(KANGBANDTK)
	/* setup oldpx and oldpy so generate routines know about birth -KMW- */
	p_ptr->oldpx = 0;
	p_ptr->oldpy = 0;
#endif /* KANGBANDTK */

#if defined(ANGBANDTK) || defined(KANGBANDTK)
	/* Shops */
	for (j = 0; j < MAX_STORES; j++)
	{
		/* Initialize */
		store_init(j);

		/* Ignore home */
		if (j == STORE_HOME) continue;

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(j);
	}
#endif /* ANGBANDTK, KANGBANDTK */

#if defined(OANGBANDTK)
	/* Shops */
	for (j = 0; j < MAX_STORES; j++)
	{
		/* Initialize */
		store_init(j);

		/* Ignore home */
		if (j == STORE_HOME) continue;

		/* Save for last */
		if (j == STORE_BLACKM) continue;

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(j);
	}

	/* Maintain the black market (ten times) */
	for (i = 0; i < 10; i++)
		store_maint(STORE_BLACKM);
#endif /* OANGBANDTK */

#if defined(ZANGBANDTK)
	/* Init the shops */
	for (i = 1; i < max_towns; i++)
	{
		for (j = 0; j < MAX_STORES; j++)
		{
			/* Initialize */
			store_init(i, j);
		}
	}
#endif /* ZANGBANDTK */

#if defined(OANGBANDTK) || defined(TNB_SQUELCH)
	/* Unsquelch all items */
	for (i = 0; i < MAX_K_IDX; i++)
		k_info[i].squelch = FALSE;
#endif /* OANGBANDTK, TNB_SQUELCH */
}

#if defined(ZANGBANDTK)

static void init_quests(int num_random)
{
	int i, j;
	
	/* Init the random quests */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = MIN_RANDOM_QUEST;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);
	p_ptr->inside_quest = 0;

	/* Prepare allocation table */
	get_mon_num_prep(monster_quest, NULL);

	/* Generate quests */
	for (i = MIN_RANDOM_QUEST + num_random - 1; i >= MIN_RANDOM_QUEST; i--)
	{
		quest_type *q_ptr = &quest[i];
		monster_race *r_ptr;
		monster_race *quest_r_ptr;
		int r_idx;

		q_ptr->status = QUEST_STATUS_TAKEN;

		for (j = 0; j < MAX_TRIES; j++)
		{
			/*
			 * Random monster 5 - 10 levels out of depth
			 * (depending on level)
			 */
			r_idx =
				get_mon_num(q_ptr->level + 4 + randint(q_ptr->level / 10));			r_ptr = &r_info[r_idx];

			/* Save the index if the monster is deeper than out current monster */
			if (!q_ptr->r_idx || (r_info[r_idx].level >
					r_info[q_ptr->r_idx].level))
			{
				q_ptr->r_idx = r_idx;
			}

			/*
			 * Accept monsters that are 2 - 6 levels
			 * out of depth depending on the quest level
			 */
			if (r_ptr->level > (q_ptr->level + (q_ptr->level / 20) + 1))
				break;
		}

		quest_r_ptr = &r_info[q_ptr->r_idx];

		/* Get the number of monsters */
		if (quest_r_ptr->flags1 & RF1_UNIQUE)
		{
			/* Mark uniques */
			quest_r_ptr->flags1 |= RF1_QUESTOR;

			q_ptr->max_num = 1;
		}
		else if (quest_r_ptr->flags3 & RF3_UNIQUE_7)
		{
			/* Mark uniques */
			quest_r_ptr->flags1 |= RF1_QUESTOR;

			q_ptr->max_num = randint(quest_r_ptr->max_num);
		}
		else
		{
			q_ptr->max_num =
				5 + (s16b) rand_int(q_ptr->level / 3 +
				5) / quest_r_ptr->rarity;
		}
	}

	/* Init the two main quests (Oberon + Serpent) */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = QUEST_OBERON;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);
	quest[QUEST_OBERON].status = QUEST_STATUS_TAKEN;

	p_ptr->inside_quest = QUEST_SERPENT;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);
	quest[QUEST_SERPENT].status = QUEST_STATUS_TAKEN;
	p_ptr->inside_quest = 0;
}

#endif /* ZANGBANDTK */

static void birth_init(void)
{
	int i;

	if (birth_ptr == NULL)
	{
		/* Wipe the player */
		player_wipe();

		/* Allocate structure to be freed later */
		birth_ptr = (struct birth_info *) Tcl_Alloc(sizeof(struct birth_info));

		birth_ptr->stage = BIRTH_GENDER;
		birth_ptr->has_prev = FALSE;
		birth_ptr->valid = FALSE;
#if defined(ZANGBANDTK)
		birth_ptr->max_quest = 0;
#endif /* ZANGBANDTK */

		/* Point-based generation */
		for (i = 0; i < A_MAX; i++)
		{
			birth_ptr->stats[i] = 10;
		}

#if defined(KANGBANDTK)

		/* Initialize plot names */
		init_flags = INIT_SHOW_TEXT | INIT_ASSIGN;
		process_dungeon_file("pl_info.txt", 0, 0, 0, 0);

#endif /* KANGBANDTK */			

#if defined(ZANGBANDTK)

		/* Hack -- A table of realm names for Tcl_GetIndexFromObj() */
		birth_ptr->realm_name[0] = "None";
		for (i = 1; i < MAX_REALM + 1; i++)
		{
			birth_ptr->realm_name[i] = (char *) realm_names[i];
		}
		birth_ptr->realm_name[MAX_REALM + 1] = NULL;			

#endif /* ZANGBANDTK */
	}
}

static int birth_stage(int stage)
{
	/* Initialize if needed */
	birth_init();

	if (birth_ptr->stage != stage)
	{
		Tcl_SetResult(g_interp, format("birth stage is \"%s\"",
			keyword_birth[birth_ptr->stage - 1]), TCL_VOLATILE);
	
		return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * (birth) stat bonus $stat
 * (birth) stat current $stat
 * (birth) stat max $stat
 */
int
objcmd_birth_stat(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *cmdOptions[] = {
		"bonus", "classbonus", "current", "max", "racebonus", NULL
	};
	enum {IDX_BONUS, IDX_CBONUS, IDX_CURRENT, IDX_MAX, IDX_RBONUS} option;

	int stat;

	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], cmdOptions, "option", 0, 
		(int *) &option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[2], keyword_stat,
		"stat", 0, &stat) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case IDX_BONUS:
		{
			/* Race/Class bonus */
			int j = rp_ptr->r_adj[stat] + cp_ptr->c_adj[stat];

			/* Return bonus */
			IntResult(interp, j);
			
			/* Done */
			break;
		}

		case IDX_CBONUS:
		{
			IntResult(interp, cp_ptr->c_adj[stat]);
			break;
		}

		case IDX_CURRENT:
		{
			/* Return the current value */
			IntResult(interp, birth_ptr->stat_use[stat]);

			/* Done */
			break;
		}

		case IDX_MAX:
		{
			/* Race/Class bonus */
			int j = rp_ptr->r_adj[stat] + cp_ptr->c_adj[stat];

			/* Obtain the "maximal" stat */
			int m = adjust_stat(17, j, TRUE);

			/* Return max allowable */
			IntResult(interp, m);

			/* Done */
			break;
		}

		case IDX_RBONUS:
		{
			IntResult(interp, rp_ptr->r_adj[stat]);
			break;
		}
	}

	return TCL_OK;
}

/*
 * (birth) class $class
 */
int
objcmd_birth_class(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int class;

	/* Check the stage */
	if (birth_stage(BIRTH_CLASS) != TCL_OK)
	{
		return TCL_ERROR;
	}

    if (Tcl_GetIndexFromObj(interp, objV[1], keyword_class,
    	"class", 0, &class) != TCL_OK)
    {
    	return TCL_ERROR;
    }
    
	/* Set class */
	p_ptr->pclass = class;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &magic_info[p_ptr->pclass];

#if defined(ANGBANDTK) || defined(OANGBANDTK)
	birth_ptr->stage = BIRTH_GENERATE;
#endif /* ANGBANDTK, OANGBANDTK */
#if defined(KANGBANDTK)
	birth_ptr->stage = BIRTH_PLOT;
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
	birth_ptr->stage = BIRTH_REALM_1;
#endif /* ZANGBANDTK */

	return TCL_OK;
}

/*
 * (birth) done
 */
int
objcmd_birth_done(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (!birth_ptr->valid)
	{
		/* Set the error */
		Tcl_SetResult(interp,
			"character has not been generated yet", TCL_STATIC);

		/* Failure */
		return TCL_ERROR;
	}

#if defined(ZANGBANDTK)
	/* Initialize the quests */
	init_quests(birth_ptr->max_quest);
#endif /* ZANGBANDTK */

	/* Verify name? */

	Tcl_Free((char *) birth_ptr);
	birth_ptr = NULL;

	return TCL_OK;
}

/*
 * (birth) done_options
 */
int
objcmd_birth_done_options(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)
	int i;
#endif /* A, K, O */

	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

#if defined(ANGBANDTK) || defined(KANGBANDTK) || defined(OANGBANDTK)

	/* Set adult options from birth options */
	for (i = OPT_BIRTH; i < OPT_CHEAT; i++)
	{
		op_ptr->opt[OPT_ADULT + (i - OPT_BIRTH)] = op_ptr->opt[i];
	}

	/* Reset score options from cheat options */
	for (i = OPT_CHEAT; i < OPT_ADULT; i++)
	{
		op_ptr->opt[OPT_SCORE + (i - OPT_CHEAT)] = op_ptr->opt[i];
	}

#endif /* ANGBANDTK, KANGBANDTK, OANGBANDTK */

	return TCL_OK;
}

/*
 * (birth) gender $gender
 */
int
objcmd_birth_gender(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int gender;

	/* Check the stage */
	if (birth_stage(BIRTH_GENDER) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_gender,
		"gender", 0, &gender) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Set sex */
	p_ptr->psex = gender;
	sp_ptr = &sex_info[p_ptr->psex];

	birth_ptr->stage = BIRTH_RACE;

	return TCL_OK;
}

/*
 * (birth) get_player
 */
int
objcmd_birth_get_player(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Roll for base hitpoints */
	get_extra();

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

	/* Roll for gold */
	get_money();

#if defined(ZANGBANDTK)

	/* Hack -- get a chaos patron even if you are not a chaos warrior */
	p_ptr->chaos_patron = (s16b) rand_int(MAX_PATRON);

	/* The character has no mutations */
	p_ptr->muta1 = 0;
	p_ptr->muta2 = 0;
	p_ptr->muta3 = 0;

	/* Initialize the virtues */
	get_virtues();

#endif /* ZANGBANDTK */

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Update stuff */
	update_stuff();

	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;

	/* Note that a valid character exists */
	birth_ptr->valid = TRUE;

	return TCL_OK;
}

/*
 * (birth) get_stats
 */
int
objcmd_birth_get_stats(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Get a new character */
	get_stats();

	return TCL_OK;
}

/*
 * (birth) info $option
 */
int
objcmd_birth_info(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *infoName[] = {"has_prev", "stat_limit",
#if defined(KANGBANDTK)
		"plot_name",
#endif /* KANGBANDTK */
#if defined(ZANGBANDTK)
		"realm_name", "max_quest",
#endif /* ZANGBANDTK */
		NULL};
	int option;

	/* Initialize if needed */
	birth_init();

	if (Tcl_GetIndexFromObj(interp, objV[1], infoName, "option", 0,
		&option) != TCL_OK)
	{
		return TCL_ERROR;
	}

	switch (option)
	{
		case 0: /* has_prev */
			BooleanResult(interp, birth_ptr->has_prev);
			break;

		case 1: /* stat_limit */
			IntResult(interp, STAT_LIMIT);
			break;
	
#if defined(KANGBANDTK)

		case 2: /* plot_name */
		{
			Tcl_Obj *listObjPtr = Tcl_NewListObj(0, NULL);
			int i;
			for (i = 0; i < z_info->q_max; i++)
			{
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(plots[i].name, -1));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}

#endif /* KANGBANDTK */
			
#if defined(ZANGBANDTK)

		case 2: /* realm_name */
		{
			Tcl_Obj *listObjPtr = Tcl_NewListObj(0, NULL);
			int realms[MAX_REALM + 1];
			int i, n = get_realms(realms);
			for (i = 0; i < n; i++)
			{
				Tcl_ListObjAppendElement(interp, listObjPtr,
					Tcl_NewStringObj(birth_ptr->realm_name[realms[i]], -1));
			}
			Tcl_SetObjResult(interp, listObjPtr);
			break;
		}
			
		case 3: /* max_quest */
			IntResult(interp, MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1);
			break;

#endif /* ZANGBANDTK */
	}

	return TCL_OK;
}

/*
 * (birth) name $name
 */
int
objcmd_birth_name(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	const char *p, *s;
	int len, base_len = 0;
	Tcl_DString extDString;
	char base[64];

	/* Initialize if needed */
	birth_init();

	/* set name */
	/* Here or in "player" command? */
	s = Tcl_GetStringFromObj(objV[1], &len);
	for (p = s; *p; p = Tcl_UtfNext(p))
	{
		Tcl_UniChar uniCh;
		int utfCharLen;

		utfCharLen = Tcl_UtfToUniChar(p, &uniCh);

		/* Control characters are not allowed */
		if (Tcl_UniCharIsControl(uniCh))
		{
			/* Set the error */
			Tcl_SetResult(interp,
				format("illegal char '%d' in character name",
				uniCh), TCL_VOLATILE);

			/* Failure */
			return TCL_ERROR;
		}

		/* This should restrict to valid OS filename characters */
		if (Tcl_UniCharIsAlnum(uniCh))
		{
			memcpy(base + base_len, p, utfCharLen);
			base_len += utfCharLen;
		}
		else
		{
			base[base_len++] = '_';
		}
	}

	/* Convert UTF8 to native encoding */
	Tcl_UtfToExternalDString(NULL, s, len, &extDString);

	/* Max 15 characters, 32-1 bytes */
	if ((Tcl_DStringLength(&extDString) > 31) || (Tcl_NumUtfChars(s, len) > 15))
	{
		/* Set the error */
		Tcl_SetResult(interp,
			format("character name \"%s\" too long", s), TCL_VOLATILE);

		Tcl_DStringFree(&extDString);

		/* Failure */
		return TCL_ERROR;
	}

	(void) strcpy(op_ptr_full_name, Tcl_DStringValue(&extDString));
	Tcl_DStringFree(&extDString);

/*	process_player_name(FALSE);*/
	Tcl_UtfToExternalDString(NULL, base, base_len, &extDString);
	len = Tcl_DStringLength(&extDString);
	(void) strncpy(op_ptr_base_name, Tcl_DStringValue(&extDString), MIN(len, 31));
	Tcl_DStringFree(&extDString);

	return TCL_OK;
}

/*
 * (birth) option $option ?$value?
 */
int
objcmd_birth_option(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
#if defined(OANGBANDTK) || defined(ZANGBANDTK)
	int objC = objc - infoCmd->depth;
#endif /* */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	static CONST char *optionName[] = {
#if defined(OANGBANDTK)
		"maximize", "preserve",
#endif /* */
#if defined(ZANGBANDTK)
		"max_quest",
#endif /* ZANGBANDTK */
		NULL};
	int option;

	/* Initialize if needed */
	birth_init();

    if (Tcl_GetIndexFromObj(interp, objV[1], optionName, "option",
    	0, &option) != TCL_OK)
    {
    	return TCL_ERROR;
    }

	switch (option)
	{
#if defined(OANGBANDTK)

		case 0: /* maximize */
		{
			int i;
			ASSERT_OPTION(optionName, option, "maximize");
			if (objC == 2)
			{
				BooleanResult(interp, p_ptr_maximize);
				break;
			}
			if (Tcl_GetBooleanFromObj(interp, objV[2], &i) != TCL_OK)
				return TCL_ERROR;
			p_ptr_maximize = i;
			break;
		}

		case 1: /* preserve */
		{
			int i;
			ASSERT_OPTION(optionName, option, "preserve");
			if (objC == 2)
			{
				BooleanResult(interp, p_ptr_preserve);
				break;
			}
			if (Tcl_GetBooleanFromObj(interp, objV[2], &i) != TCL_OK)
				return TCL_ERROR;
			p_ptr_preserve = i;
			break;
		}

#endif /* O */

#if defined(ZANGBANDTK)

		case 0: /* max_quest */
		{
			int i;
			ASSERT_OPTION(optionName, option, "max_quest");
			if (objC == 2)
			{
				BooleanResult(interp, birth_ptr->max_quest);
				break;
			}
		    if (Tcl_GetIntFromObj(interp, objv[3], &i) != TCL_OK)
		    	return TCL_ERROR;
			if ((i < 0) || (i > MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1))
			{
				/* Set the error */
				FormatResult(interp, "max_quest must be between 0 and %d",
					MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1);

				/* Failure */
				return TCL_ERROR;
			}
			birth_ptr->max_quest = i;
			break;
		}

#endif /* ZANGBANDTK */

		default: /* catch errors */
			FormatResult(interp, "unhandled option index \"%d\"", option);
			return TCL_ERROR;
	}

	return TCL_OK;
}

/*
 * (birth) load_prev
 */
int
objcmd_birth_load_prev(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Require a previous roll */
	if (!birth_ptr->has_prev)
	{
		/* Set the error */
		Tcl_SetResult(interp, "no previous roll exists", TCL_STATIC);

		/* Failure */
		return TCL_ERROR;
	}

	/* Save current, restore previous roll */
	load_prev_data();

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Update stuff */
	update_stuff();

	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;

	return TCL_OK;
}

/*
 * (birth points) cost $stat
 */
int
objcmd_birth_points_cost(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int stat;

	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (!p_ptr_point_based)
	{
		Tcl_SetResult(interp, "not using point-based generation", TCL_STATIC);
		return TCL_ERROR;
	}

	/* Get the stat */
	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_stat,
		"stat", 0, &stat) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Return the cost for this stat */
	IntResult(interp, birth_stat_costs[birth_ptr->stats[stat] - 10]);

	return TCL_OK;
}

/*
 * (birth points) stat $stat ?$value?
 */
int
objcmd_birth_points_stat(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	int objC = objc - infoCmd->depth;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int stat;

	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (!p_ptr_point_based)
	{
		Tcl_SetResult(interp, "not using point-based generation", TCL_STATIC);
		return TCL_ERROR;
	}

	/* Get the stat */
	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_stat,
		"stat", 0, &stat) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (objC == 3)
	{
		int i, value, cost = 0;

		/* Get the desired stat value */
		if (Tcl_GetIntFromObj(interp, objV[2], &value) != TCL_OK)
		{
			return TCL_ERROR;
		}

		/* Restrict stat value */
		if ((value < 10) || (value > 18))
		{
			Tcl_SetResult(interp,
				format("illegal stat \"%d\": must be from 10 to 18", value),
				TCL_VOLATILE);

			return TCL_ERROR;
		}

		/* Check each stat */
		for (i = 0; i < A_MAX; i++)
		{
			int n = (i == stat) ? value : birth_ptr->stats[i];
			
			/* Total cost */
			cost += birth_stat_costs[n - 10];
		}

		/* Restrict cost */
		if (cost > 48)
		{
			Tcl_SetResult(interp, "stat cost is too high", TCL_STATIC);
			return TCL_ERROR;
		}

		/* Save the new value */
		birth_ptr->stats[stat] = value;

		/* Variable stat maxes */
		if (p_ptr_maximize)
		{
			/* Reset stats */
			p_ptr->stat_cur[stat] = p_ptr->stat_max[stat] =
				birth_ptr->stats[stat];

/* "birth stat current" wants it */
birth_ptr->stat_use[stat] = modify_stat_value(birth_ptr->stats[stat],
	rp_ptr->r_adj[stat] + cp_ptr->c_adj[stat]);
		}

		/* Fixed stat maxes */
		else
		{
			/* Obtain a "bonus" for "race" and "class" */
			int bonus = rp_ptr->r_adj[stat] + cp_ptr->c_adj[stat];

			/* Apply the racial/class bonuses */
			p_ptr->stat_cur[stat] = p_ptr->stat_max[stat] =
				modify_stat_value(birth_ptr->stats[stat], bonus);

/* "birth stat current" wants it */
birth_ptr->stat_use[stat] = p_ptr->stat_cur[stat];
		}

		return TCL_OK;
	}

	/* Return the current stat value */
	IntResult(interp, birth_ptr->stats[stat]);

	return TCL_OK;
}

/*
 * (birth) race $race
 */
int
objcmd_birth_race(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int race;

	/* Check the stage */
	if (birth_stage(BIRTH_RACE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], keyword_race,
		"race", 0, &race) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* Set race */
	p_ptr->prace = race;
	rp_ptr = &race_info[p_ptr->prace];

#if defined(ZANGBANDTK)

	/* Give beastman a mutation at character birth */
	if (p_ptr->prace == RACE_BEASTMAN)
	{
		hack_mutation = TRUE;
	}

#endif /* ZANGBANDTK */

	birth_ptr->stage = BIRTH_CLASS;

	return TCL_OK;
}

/*
 * (birth) reset
 */
int
objcmd_birth_reset(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	/* Initialize if needed */
	birth_init();

	birth_ptr->stage = BIRTH_GENDER;
	birth_ptr->has_prev = FALSE;
	birth_ptr->valid = FALSE;
#if defined(ZANGBANDTK)
	birth_ptr->max_quest = 0;
#endif /* ZANGBANDTK */

	/* Wipe the player */
	player_wipe();

#if defined(ZANGBANDTK)
	hack_mutation = FALSE;
#endif /* ZANGBANDTK */

	return TCL_OK;
}

/*
 * (birth) save_prev
 */
int
objcmd_birth_save_prev(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	/* Check the stage */
	if (birth_stage(BIRTH_GENERATE) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/* We already rolled a valid character */
	if (!birth_ptr->valid)
	{
		/* Set the error */
		Tcl_SetResult(interp,
			"character has not been generated yet", TCL_STATIC);

		/* Failure */
		return TCL_ERROR;
	}

	/* Remember the current character */
	save_prev_data();

	/* Note that a previous roll exists */
	birth_ptr->has_prev = TRUE;

	return TCL_OK;
}

#if defined(KANGBANDTK)

/*
 * (birth) plot $plot
 */
int
objcmd_birth_plot(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int plot;

	/* Check the stage */
	if (birth_stage(BIRTH_PLOT) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIntFromObj(interp, objV[1], &plot) != TCL_OK)
	{
		return TCL_ERROR;
	}
	if ((plot < 1) || (plot > 10))
	{
		Tcl_AppendResult(interp, "illegal plot number", NULL);
		return TCL_ERROR;
	}

	/* Set plot number */
	p_ptr->plot_num = plot;

	birth_ptr->stage = BIRTH_GENERATE;

	return TCL_OK;
}

#endif /* KANGBANDTK */

#if defined(ZANGBANDTK)

/*
 * (birth) realm1 $realm
 */
int
objcmd_birth_realm1(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int j, n, realm, realms[MAX_REALM + 1];

	/* Check the stage */
	if (birth_stage(BIRTH_REALM_1) != TCL_OK)
	{
		return TCL_ERROR;
	}

	/*
	 * Note: Sometimes only one realm is allowed for a given class,
	 * including "no magic". In these cases it is required that the
	 * single allowed realm is passed back via the "realm1" and
	 * "realm2" options. The user would simply not be presented with
	 * this single so-called choice.
	 */

	if (Tcl_GetIndexFromObj(interp, objV[1], (CONST char **) birth_ptr->realm_name,
		"realm", 0, &realm) != TCL_OK)
    {
    	return TCL_ERROR;
    }

	n = get_realms(realms);
	for (j = 0; j < n; j++)
	{
		if (realms[j] == realm) break;
	}
	if (j == n)
	{
		/* Set the error */
		Tcl_SetResult(interp,
			format("invalid first realm \"%s\"",
			Tcl_GetStringFromObj(objV[1], NULL)), TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	p_ptr->realm1 = realm;

	birth_ptr->stage = BIRTH_REALM_2;

	return TCL_OK;
}

/*
 * (birth) realm2 $realm
 */
int
objcmd_birth_realm2(ClientData clientData, Tcl_Interp *interp, int objc, Tcl_Obj *CONST objv[])
{
	CommandInfo *infoCmd = (CommandInfo *) clientData;
/*	int objC = objc - infoCmd->depth; */
	Tcl_Obj *CONST *objV = objv + infoCmd->depth;

	int j, n, realm, realms[MAX_REALM + 1];

	/* Check the stage */
	if (birth_stage(BIRTH_REALM_2) != TCL_OK)
	{
		return TCL_ERROR;
	}

	if (Tcl_GetIndexFromObj(interp, objV[1], (CONST char **) birth_ptr->realm_name,
		"realm", 0, &realm) != TCL_OK)
    {
    	return TCL_ERROR;
    }

	n = get_realms(realms);
	for (j = 0; j < n; j++)
	{
		if (realms[j] == realm) break;
	}
	if (j == n)
	{
		/* Set the error */
		Tcl_SetResult(interp,
			format("invalid second realm \"%s\"",
			Tcl_GetStringFromObj(objV[1], NULL)), TCL_VOLATILE);

		/* Failure */
		return TCL_ERROR;
	}

	p_ptr->realm2 = realm;

	birth_ptr->stage = BIRTH_GENERATE;

	return TCL_OK;
}

#endif /* ZANGBANDTK */

static CommandInit commandInit[] = {
	{0, "birth", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
		{1, "class", 2, 2, "className", objcmd_birth_class, (ClientData) 0},
		{1, "done", 0, 0, (char *) NULL, objcmd_birth_done, (ClientData) 0},
		{1, "done_options", 0, 0, (char *) NULL, objcmd_birth_done_options, (ClientData) 0},
		{1, "gender", 2, 2, "genderName", objcmd_birth_gender, (ClientData) 0},
		{1, "get_player", 0, 0, (char *) NULL, objcmd_birth_get_player, (ClientData) 0},
		{1, "get_stats", 0, 0, (char *) NULL, objcmd_birth_get_stats, (ClientData) 0},
		{1, "info", 2, 2, "option", objcmd_birth_info, (ClientData) 0},
		{1, "load_prev", 0, 0, (char *) NULL, objcmd_birth_load_prev, (ClientData) 0},
		{1, "name", 2, 2, "playerName", objcmd_birth_name, (ClientData) 0},
		{1, "option", 2, 3, "optionName ?value?", objcmd_birth_option, (ClientData) 0},
#if defined(KANGBANDTK)
		{1, "plot", 2, 2, "plotNumber", objcmd_birth_plot, (ClientData) 0},
#endif /* */
		{1, "points", 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0},
			{2, "cost", 2, 2, "stat", objcmd_birth_points_cost, (ClientData) 0},
			{2, "stat", 2, 3, "stat ?value?", objcmd_birth_points_stat, (ClientData) 0},
		{1, "race", 2, 2, "raceName", objcmd_birth_race, (ClientData) 0},
#if defined(ZANGBANDTK)
		{1, "realm1", 2, 2, "realmName", objcmd_birth_realm1, (ClientData) 0},
		{1, "realm2", 2, 2, "realmName", objcmd_birth_realm2, (ClientData) 0},
#endif /* */
		{1, "reset", 0, 0, (char *) NULL, objcmd_birth_reset, (ClientData) 0},
		{1, "save_prev", 0, 0, (char *) NULL, objcmd_birth_save_prev, (ClientData) 0},
		{1, "stat", 3, 3, "option statName", objcmd_birth_stat, (ClientData) 0},
	{0, (char *) NULL, 0, 0, (char *) NULL, (Tcl_ObjCmdProc *) NULL, (ClientData) 0}
};

void init_birth(void)
{
	/* Tcl commands */
	(void) CommandInfo_Init(g_interp, commandInit, NULL);
}

