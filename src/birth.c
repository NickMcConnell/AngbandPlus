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
#define AUTOROLLER_STEP		25L

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

	s16b stat[A_MAX];

	char history[4][60];

	s16b patron;

	s16b hp[PY_MAX_LEVEL];
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
	cptr info;	/* Textual History */

	byte roll;	/* Frequency of this entry */
	byte chart;	/* Chart index */
	byte next;	/* Next chart index */
	byte bonus;	/* Social Class Bonus + 50 */
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
static const hist_type bg[] =
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
	{"Your father was an Orc, but it is unacknowledged.  ", 100, 19, 20, 25},

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

	{"You are one of several children of a Klackon hive queen.  ", 100, 84, 85,
	 50},

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

	{"charcoal wings, charcoal skin and a smoke-gray belly.", 11, 136, 0, 50},
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
	{"In life you were a simple peasant, the victim of a powerful Vampire Lord.  ", 40, 113, 114, 50},
	{"In life you were a Vampire Hunter, but they got you.  ", 60, 113, 114,
	 50},
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
	{"You are the magical crossbreed of an animal and a man.  ", 75, 129, 130,
	 50},
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



/*
 * Current stats
 */
static s16b stat_use[A_MAX];


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
		strcpy(prev.history[i], p_ptr->history[i]);
	}

	/* Save the patron */
	prev.patron = p_ptr->chaos_patron;

	/* Save the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		prev.hp[i] = p_ptr->player_hp[i];
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(void)
{
	int i;

	birther temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->age;
	temp.wt = p_ptr->wt;
	temp.ht = p_ptr->ht;
	temp.sc = p_ptr->sc;
	temp.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		temp.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(temp.history[i], p_ptr->history[i]);
	}

	/* Save the patron */
	temp.patron = p_ptr->chaos_patron;

	/* Save the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		temp.hp[i] = p_ptr->player_hp[i];
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
		strcpy(p_ptr->history[i], prev.history[i]);
	}

	/* Load the patron */
	p_ptr->chaos_patron = prev.patron;

	/* Load the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		p_ptr->player_hp[i] = prev.hp[i];
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

	/* Save the patron */
	prev.patron = temp.patron;

	/* Save the hitpoints */
	for (i = 0; i < PY_MAX_LEVEL; i++)
	{
		prev.hp[i] = temp.hp[i];
	}
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
			dice[i] = randint1(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals */
		if ((j > 42) && (j < 57)) break;
		/* 57 was 54... I hate 'magic numbers' :< TY */
	}

	/* Acquire the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3 * i] + dice[3 * i + 1] + dice[3 * i + 2];

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Apply the bonus to the stat (somewhat randomly) */
		stat_use[i] = adjust_stat(i, j * 10, bonus);

		/* Start fully healed */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];
	}
}


/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int i, j, min_value, max_value;

#ifdef SHOW_LIFE_RATE
	int percent;
#endif /* SHOW_LIFE_RATE */

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

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
	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */
	while (TRUE)
	{
		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			/* Add in racial hit dice */
			j = randint1(rp_ptr->r_mhp);
			p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + j;

			/* If class hit dice is non zero - add it on */
			if (cp_ptr->c_mhp)
			{
				p_ptr->player_hp[i] += randint1(cp_ptr->c_mhp);
			}
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (p_ptr->player_hp[PY_MAX_LEVEL - 1] < min_value) continue;
		if (p_ptr->player_hp[PY_MAX_LEVEL - 1] > max_value) continue;

		/* Acceptable */
		break;
	}

#ifdef SHOW_LIFE_RATE

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
					(2 * p_ptr->hitdie +
					 ((PY_MAX_LEVEL - 1) * (p_ptr->hitdie + 1))));

	msgf("Current Life Rating is %d/100.", percent);
	message_flush();

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
	
	int len = 0;

	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) p_ptr->history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint1(4);

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
		case RACE_GHOUL:
		{
			/* The same as Zombie, for now */
			chart = 107;
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
		roll = randint1(100);

		/* Access the proper entry in the table */
		while ((chart != bg[i].chart) || (roll > bg[i].roll)) i++;

		/* Acquire the textual history */
		strnfcat(buf, 1024, &len, "%s", bg[i].info);

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
	for (s = buf; *s == ' '; s++) /* loop */ ;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n - 1] == ' ')) s[--n] = '\0';


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
			strcpy(p_ptr->history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 60; ((n > 0) && (s[n - 1] != ' ')); n--) /* loop */ ;

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n - 1] == ' ')) s[--n] = '\0';

		/* Save one line of history */
		strcpy(p_ptr->history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */ ;
	}
}


/*
 * Computes character's age, height, and weight
 */
static void get_ahw(void)
{
	int h_percent;

	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint1(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->m_b_ht);
		p_ptr->wt = Rand_normal((int)(rp_ptr->m_b_wt) * h_percent / 100,
								(int)(rp_ptr->m_m_wt) * h_percent / 300);
	}
	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);

		h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->f_b_ht);
		p_ptr->wt = Rand_normal((int)(rp_ptr->f_b_wt) * h_percent / 100,
								(int)(rp_ptr->f_m_wt) * h_percent / 300);
	}
}


/*
 * Get the player's starting money
 */
static void get_money(void)
{
	int i, gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + rand_range(300, 400);

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 18 + 50) gold -= 300;
		else if (stat_use[i] >= 18 + 20) gold -= 200;
		else if (stat_use[i] > 18) gold -= 150;
		else
			gold -= (stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}


/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;

	bool options[OPT_PLAYER];
	bool birth[OPT_BIRTH];
	pcave_type *pcave[MAX_HGT];
	pblk_ptr **pwild;

	/* Hack -- save these allocated arrays */
	C_COPY(options, p_ptr->options, OPT_PLAYER, bool);
	C_COPY(birth, p_ptr->birth, OPT_BIRTH, bool);

	/* Hack -- save the cave and wilderness arrays */
	C_COPY(pcave, p_ptr->pcave, MAX_HGT, pcave_type *);
	pwild = p_ptr->pwild;

	/*
	 * Delete the carried objects
	 */
	if (p_ptr->inventory) delete_object_list(&p_ptr->inventory);

	/* Hack -- zero the struct */
	(void)WIPE(p_ptr, player_type);

	/* Hack -- Restore the cave and wilderness arrays */
	C_COPY(p_ptr->pcave, pcave, MAX_HGT, pcave_type *);
	p_ptr->pwild = pwild;;

	/* Hack -- Restore the option arrays */
	C_COPY(p_ptr->options, options, OPT_PLAYER, bool);
	C_COPY(p_ptr->birth, birth, OPT_BIRTH, bool);

	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
		p_ptr->history[i][0] = 0;
	}

	/* Start with no artifacts made yet */
	for (i = 0; i < z_info->a_max; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Reset the objects */
	k_info_reset();

	/* Reset the "monsters" */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
		if (r_ptr->flags3 & RF3_UNIQUE_7) r_ptr->max_num = 7;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* None of the spells have been learned yet */
	for (i = 0; i < PY_MAX_SPELLS; i++) p_ptr->spell_order[i] = 99;

	/* Clean the mutation count */
	mutant_regenerate_mod = 100;

	/* Clear "cheat" options */
	cheat_peek = FALSE;
	cheat_hear = FALSE;
	cheat_room = FALSE;
	cheat_xtra = FALSE;
	cheat_know = FALSE;
	cheat_live = FALSE;

	/* Default pet command settings */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
}


/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static const byte player_init[MAX_CLASS][3][2] =
{
	{
	 /* Warrior */
	 {TV_RING, SV_RING_RES_FEAR},	/* Warriors need it! */
	 {TV_SWORD, SV_BROAD_SWORD},
	 {TV_HARD_ARMOR, SV_CHAIN_MAIL}
	 },

	{
	 /* Mage */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for realm1 book */
	 {TV_SWORD, SV_DAGGER},
	 {TV_DEATH_BOOK, 0}			/* Hack: for realm2 book */
	 },

	{
	 /* Priest */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for Life / Death book */
	 {TV_HAFTED, SV_MACE},
	 {TV_DEATH_BOOK, 0}			/* Hack: for realm2 book */
	 },

	{
	 /* Rogue */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for realm1 book */
	 {TV_SWORD, SV_DAGGER},
	 {TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
	 },

	{
	 /* Ranger */
	 {TV_NATURE_BOOK, 0},
	 {TV_SWORD, SV_DAGGER},
	 {TV_DEATH_BOOK, 0}			/* Hack: for realm2 book */
	 },

	{
	 /* Paladin */
	 {TV_SORCERY_BOOK, 0},
	 {TV_SWORD, SV_BROAD_SWORD},
	 {TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL}
	 },

	{
	 /* Warrior-Mage */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for realm1 book */
	 {TV_SWORD, SV_SHORT_SWORD},
	 {TV_DEATH_BOOK, 0}			/* Hack: for realm2 book */
	 },

	{
	 /* Chaos Warrior */
	 {TV_SORCERY_BOOK, 0},		/* Hack: For realm1 book */
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
	 {TV_SWORD, SV_DAGGER},
	 {TV_POTION, SV_POTION_RES_WIS},
	 {TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
	 },

	{
	 /* High Mage */
	 {TV_SORCERY_BOOK, 0},		/* Hack: for realm1 book */
	 {TV_SWORD, SV_DAGGER},
	 {TV_RING, SV_RING_SUSTAIN_INT}
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

	object_type *q_ptr;

	/* Give the player some food */
	switch (p_ptr->prace)
	{
		case RACE_GOLEM:
		case RACE_SKELETON:
		case RACE_ZOMBIE:
		case RACE_VAMPIRE:
		case RACE_SPECTRE:
		case RACE_GHOUL:
		{
			/* Scrolls of satisfy hunger */
			q_ptr =
				object_prep(lookup_kind(TV_SCROLL, SV_SCROLL_SATISFY_HUNGER));
			q_ptr->number = (byte)rand_range(2, 5);
			object_aware(q_ptr);
			object_known(q_ptr);

			/* These objects are "storebought" */
			q_ptr->info |= OB_STOREB;

			(void)inven_carry(q_ptr);

			break;
		}
		default:
		{
			/* Food rations */
			q_ptr = object_prep(lookup_kind(TV_FOOD, SV_FOOD_RATION));
			q_ptr->number = (byte)rand_range(3, 7);
			object_aware(q_ptr);
			object_known(q_ptr);

			(void)inven_carry(q_ptr);
		}
	}

	if (p_ptr->prace == RACE_VAMPIRE)
	{
		/* Hack -- Give the player scrolls of DARKNESS! */
		q_ptr = object_prep(lookup_kind(TV_SCROLL, SV_SCROLL_DARKNESS));

		q_ptr->number = (byte)rand_range(2, 5);

		object_aware(q_ptr);
		object_known(q_ptr);

		/* These objects are "storebought" */
		q_ptr->info |= OB_STOREB;

		(void)inven_carry(q_ptr);
	}
	else
	{
		/* Hack -- Give the player some torches */
		q_ptr = object_prep(lookup_kind(TV_LITE, SV_LITE_TORCH));
		q_ptr->number = (byte)rand_range(3, 7);
		q_ptr->timeout = rand_range(3, 7) * 500;
		q_ptr->pval = 0;
		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}

	if (p_ptr->pclass == CLASS_RANGER)
	{
		/* Hack -- Give the player some arrows */
		q_ptr = object_prep(lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
		q_ptr->number = (byte)rand_range(15, 20);

		/* These objects are "storebought" */
		q_ptr->info |= OB_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);

		/* Hack -- Give the player a bow */
		q_ptr = object_prep(lookup_kind(TV_BOW, SV_SHORT_BOW));

		/* This object is "storebought" */
		q_ptr->info |= OB_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}
	else if (p_ptr->pclass == CLASS_HIGH_MAGE)
	{
		/* Hack -- Give the player a wand of magic missile */
		q_ptr = object_prep(lookup_kind(TV_WAND, SV_WAND_MAGIC_MISSILE));
		q_ptr->number = 1;
		q_ptr->pval = (byte)rand_range(25, 30);

		/* This object is "storebought" */
		q_ptr->info |= OB_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Hack to initialize spellbooks */
		if (tv == TV_SORCERY_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm1 - 1;
		else if (tv == TV_DEATH_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm2 - 1;

		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&
				 p_ptr->prace == RACE_BARBARIAN)
		{
			/* Barbarians do not need a ring of resist fear */
			sv = SV_RING_SUSTAIN_STR;
		}

		/* Hack -- Give the player an object */
		q_ptr = object_prep(lookup_kind(tv, sv));

		/* Assassins begin the game with a poisoned dagger */
		if (tv == TV_SWORD && p_ptr->pclass == CLASS_ROGUE &&
			p_ptr->realm1 == REALM_DEATH)
		{
			add_ego_flags(q_ptr, EGO_BRAND_POIS);
		}

		/* These objects are "storebought" */
		q_ptr->info |= OB_STOREB;

		object_aware(q_ptr);
		object_known(q_ptr);

		(void)inven_carry(q_ptr);
	}
}

/* Locations of the tables on the screen */
#define QUESTION_COL	3
#define SEX_COL			0
#define RACE_COL		12
#define RACE_AUX_COL    27
#define CLASS_COL		27
#define CLASS_AUX_COL   48
#define REALM1_COL		48
#define REALM2_COL		60


/*
 * Player sex
 */
static bool get_player_sex(void)
{
	int i;
	cptr genders[MAX_SEXES];

	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Your 'sex' does not have any significant gameplay effects.");

	/* Tabulate genders */
	for (i = 0; i < MAX_SEXES; i++)
	{
		genders[i] = sex_info[i].title;
	}

	p_ptr->psex = get_player_choice(genders, MAX_SEXES, SEX_COL, 15,
									"charattr.txt#TheSexes", NULL);

	/* No selection? */
	if (p_ptr->psex == INVALID_CHOICE)
	{
		p_ptr->psex = 0;
		return (FALSE);
	}

	/* Save the sex pointer */
	sp_ptr = &sex_info[p_ptr->psex];

	return (TRUE);
}


/*
 * Display additional information about each race during the selection.
 */
static void race_aux_hook(cptr r_str)
{
	int race;

	/* Extract the proper race index from the string. */
	for (race = 0; race < MAX_RACES; race++)
	{
		if (streq(r_str, race_info[race].title)) break;
	}

	if (race == MAX_RACES) return;

	/* Display relevant details. */
	put_fstr(RACE_AUX_COL, TABLE_ROW,
    			"%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
				"Hit die: %d \n"
				"Experience: %2d%% \n"
				"Infravision: %d ft",
                stat_names_reduced[0], race_info[race].r_adj[0],
                stat_names_reduced[1], race_info[race].r_adj[1],
                stat_names_reduced[2], race_info[race].r_adj[2],
                stat_names_reduced[3], race_info[race].r_adj[3],
                stat_names_reduced[4], race_info[race].r_adj[4],
                stat_names_reduced[5], race_info[race].r_adj[5],
				race_info[race].r_mhp,
				race_info[race].r_exp,
				race_info[race].infra * 10);
}


/*
 * Player race
 */
static bool get_player_race(void)
{
	int i;
	cptr races[MAX_RACES];

	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Your 'race' determines various intrinsic factors and bonuses.");

	/* Tabulate races */
	for (i = 0; i < MAX_RACES; i++)
	{
		races[i] = race_info[i].title;
	}

	p_ptr->prace = get_player_sort_choice(races, MAX_RACES, RACE_COL, 15,
										  "charattr.txt#TheRaces",
										  race_aux_hook);

	/* No selection? */
	if (p_ptr->prace == INVALID_CHOICE)
	{
		p_ptr->prace = 0;
		return (FALSE);
	}

	/* Give beastman a mutation at character birth */
	if (p_ptr->prace == RACE_BEASTMAN)
	{
		hack_mutation = TRUE;
	}
	else
	{
		hack_mutation = FALSE;
	}

	/* Save the race pointer */
	rp_ptr = &race_info[p_ptr->prace];

	/* Success */
	return (TRUE);
}


/*
 * Display additional information about each class during the selection.
 */
static void class_aux_hook(cptr c_str)
{
	int class_idx;
	char s[128];

	/* Extract the proper class index from the string. */
	for (class_idx = 0; class_idx < MAX_CLASS; class_idx++)
	{
		if (streq(c_str, class_info[class_idx].title)) break;

		/* Also test for titles in parentheses */
		strnfmt(s, 128, "(%s)", class_info[class_idx].title);
		if (streq(c_str, s)) break;
	}

	if (class_idx == MAX_CLASS) return;

	/* Display relevant details. */
	put_fstr(CLASS_AUX_COL, TABLE_ROW,
    			"%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
                "%s%+d\n"
				"Hit die: %d \n"
				"Experience: %2d%%",
                stat_names_reduced[0], class_info[class_idx].c_adj[0],
                stat_names_reduced[1], class_info[class_idx].c_adj[1],
                stat_names_reduced[2], class_info[class_idx].c_adj[2],
                stat_names_reduced[3], class_info[class_idx].c_adj[3],
                stat_names_reduced[4], class_info[class_idx].c_adj[4],
                stat_names_reduced[5], class_info[class_idx].c_adj[5],
			 	class_info[class_idx].c_mhp,
				class_info[class_idx].c_exp);
}


/*
 * Player class
 */
static bool get_player_class(void)
{
	int i;
	char buf[80];
	cptr classes[MAX_CLASS];


	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Your 'class' determines various intrinsic abilities and bonuses.\n"
				"Any entries in parentheses should only be used by advanced players.");

	/* Tabulate classes */
	for (i = 0; i < MAX_CLASS; i++)
	{
		/* Analyze */
		if (!(rp_ptr->choice & (1L << i)))
		{
			strnfmt(buf, 80, "(%s)", class_info[i].title);
		}
		else
		{
			strnfmt(buf, 80, "%s", class_info[i].title);
		}

		/* Save the string */
		classes[i] = string_make(buf);
	}

	p_ptr->pclass = get_player_choice(classes, MAX_CLASS, CLASS_COL, 20,
									  "charattr.txt#TheClasses",
									  class_aux_hook);

	/* No selection? */
	if (p_ptr->pclass == INVALID_CHOICE)
	{
		p_ptr->pclass = 0;

		for (i = 0; i < MAX_CLASS; i++)
		{
			/* Free the strings */
			string_free(classes[i]);
		}

		return (FALSE);
	}

	/* Set class */
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &magic_info[p_ptr->pclass];

	for (i = 0; i < MAX_CLASS; i++)
	{
		/* Free the strings */
		string_free(classes[i]);
	}

	return (TRUE);
}


/*
 * Choose the magical realms
 */
static bool get_player_realms(void)
{
	int i;
	int count = 0;
	cptr realms[MAX_REALM];
	int select[MAX_REALM];
	int choose;

	/* No realms at all? */
	select[0] = REALM_NONE;

	/* Get choices */
	for (i = 1; i < MAX_REALM; i++)
	{
		/* Can we use this realm? */
		if (realm_choices1[p_ptr->pclass] & (1 << (i - 1)))
		{
			/* Save the information */
			select[count] = i;
			realms[count] = realm_names[i];

			/* Count them */
			count++;
		}
	}

	/* No magic? */
	if (!count) return (TRUE);

	/* Extra info */
	put_fstr(QUESTION_COL, QUESTION_ROW,
				"Life and Sorcery are protective, Chaos and Death are destructive.\n"
				"Nature has both defensive and offensive spells.");

	choose = get_player_choice(realms, count, REALM1_COL, 10,
							   "magic.txt#MagicRealms", NULL);

	/* No selection? */
	if (choose == INVALID_CHOICE) return (FALSE);

	/* Save the choice */
	p_ptr->realm1 = select[choose];

	/* Paranoia - No realms at all? */
	select[0] = REALM_NONE;

	/* Reset counter */
	count = 0;

	/* Get choices */
	for (i = 1; i < MAX_REALM; i++)
	{
		/* Can we use this realm? */
		if ((realm_choices2[p_ptr->pclass] & (1 << (i - 1)))
			&& (i != p_ptr->realm1))
		{
			/* Save the information */
			select[count] = i;
			realms[count] = realm_names[i];

			/* Increment counter */
			count++;
		}
	}

	/* No second realm? */
	if (!count) return (TRUE);

	choose = get_player_choice(realms, count, REALM2_COL, 10,
							   "magic.txt#MagicRealms", NULL);

	/* No selection? */
	if (choose == INVALID_CHOICE) return (FALSE);

	/* Save the choice */
	p_ptr->realm2 = select[choose];

	/* Done */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 *
 * Taken from V 2.9.0
 */
static bool player_birth_aux_1(void)
{
	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
	put_fstr(QUESTION_COL, HEADER_ROW,
			"Please select your character from the menu below.\n"
			"Use the movement keys to scroll the menu, 'enter' to select the current\n"
			"menu item, '*' for a random menu item, 'ESC' to restart the character\n"
			"selection, '=' for the birth options, '?' for help, or 'Ctrl-X' to quit.");

	if (!get_player_sex()) return (FALSE);

	/* Clean up */
    clear_region(0, QUESTION_ROW, TABLE_ROW - 1);

	/* Choose the players race */
	if (!get_player_race()) return (FALSE);

	/* Clean up */
	clear_region(0, QUESTION_ROW, TABLE_ROW - 1);

	/* Choose the players class */
	if (!get_player_class()) return (FALSE);

	/* Clean up */
	clear_region(0, QUESTION_ROW, TABLE_ROW - 1);

	/* Choose the magic realms */
	if (!get_player_realms()) return (FALSE);

	/* Clear */
	Term_clear();

	/* Display the information so far. */
	/* Name, Sex, Race, Class */
	put_fstr(0, 2,
				"Name     : " CLR_L_BLUE "%s\n" CLR_WHITE
				"Sex      : " CLR_L_BLUE "%s\n" CLR_WHITE
				"Race     : " CLR_L_BLUE "%s\n" CLR_WHITE
				"Class    : " CLR_L_BLUE "%s\n",
				player_name, sp_ptr->title, rp_ptr->title, cp_ptr->title);

	if (p_ptr->realm1 || p_ptr->realm2)
	{
		put_fstr(0, 6, "Magic    : " CLR_L_BLUE "%s", realm_names[p_ptr->realm1]);
	}

	if (p_ptr->realm2)
	{
		put_fstr(11, 7, CLR_L_BLUE "%s", realm_names[p_ptr->realm2]);
	}

	/* And finally, get the number of random quests */
	get_player_quests(-1);

	/* Clear */
	clear_from(15);

	/* Done */
	return (TRUE);
}


/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
static const int birth_stat_costs[(18 - 10) + 1] =
{ 0, 1, 2, 4, 7, 11, 16, 22, 30 };


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
static bool player_birth_aux_2(void)
{
	int i;

	int row = 3;
	int col = 42;

	int stat = 0;

	int stats[A_MAX];

	int cost;

	char ch;

	int mode = DISPLAY_PLAYER_STANDARD;


	/* Initialize stats */
	for (i = 0; i < A_MAX; i++)
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
	p_ptr->chaos_patron = (s16b)randint0(MAX_PATRON);

	p_ptr->muta1 = 0;
	p_ptr->muta2 = 0;
	p_ptr->muta3 = 0;

	/* Interact */
	while (1)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

			/* Reset stats */
			p_ptr->stat_cur[i] = adjust_stat(i, stats[i] * 10, bonus);
			p_ptr->stat_max[i] = p_ptr->stat_cur[i];

			/* Total cost */
			cost += birth_stat_costs[stats[i] - 10];
		}

		/* Restrict cost */
		if (cost > 48)
		{
			/* Warning */
			bell("Excessive stats!");

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
		put_fstr(col + 32, row - 2, "Cost");

		/* Display the costs */
		for (i = 0; i < A_MAX; i++)
		{
			/* Display cost */
			put_fstr(col + 32, row + (i - 1), "%4d", birth_stat_costs[stats[i] - 10]);
		}


		/* Prompt XXX XXX XXX */
		prtf(0, 0, "Total Cost %2d/48.  Use 2/8 to move, 4/6 to modify, Enter to accept.",
				cost);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(col + 36, row + stat - 1);

		/* Get key */
		ch = inkey();

		/* Quit */
		if (ch == KTRL('X')) quit(NULL);

		/* Start over */
		if (ch == ESCAPE) return (FALSE);

		/* Done */
		if ((ch == '\r') || (ch == '\n')) break;

		/* Prev stat */
		if (ch == '8')
		{
			stat = (stat + A_MAX - 1) % A_MAX;
		}

		/* Next stat */
		if (ch == '2')
		{
			stat = (stat + 1) % A_MAX;
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

	/* Process stats */
	for (i = 0; i < A_MAX; i++)
	{
		int bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Apply some randomness */
        p_ptr->stat_cur[i] = adjust_stat(i, stats[i] * 10, bonus);
		p_ptr->stat_max[i] = p_ptr->stat_cur[i];
	}

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Update stuff */
	update_stuff();

	/* Display the player */
	display_player(mode);

	/* Done */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "auto-rolling" and "random-rolling".
 *
 * The delay may be reduced, but is recommended to keep players
 * from continuously rolling up characters, which can be VERY
 * expensive CPU wise.  And it cuts down on player stupidity.
 */
static bool player_birth_aux_3(void)
{
	int i, v;

	bool flag;
	bool previous = FALSE;

	char ch;

	int mode = DISPLAY_PLAYER_STANDARD;

#ifdef ALLOW_AUTOROLLER

	s16b stat_weight[A_MAX];
	s16b stat_save[A_MAX];

	s32b stat_match[A_MAX];

	s32b auto_round = 0L;

	s32b last_round;


	/*** Autoroll ***/

	/* Initialize */
	if (autoroller && !ironman_moria)
	{
		char inp[80];

		/* Clean up */
		clear_from(10);

		/* Extra info */
		put_fstr(5, 10,
					"The auto-roller will generate 500 characters and try to pick\n"
					"the one with the best stats, according to the weightings you\n"
					"choose below. Enter a value from 1-100 for each stat.");

		/* Prompt for the stat weights */
		put_fstr(2, 15, "Enter weight for: ");

		/* Output the prompts */
		for (i = 0; i < A_MAX; i++)
		{
			/* Reset the "success" counter */
			stat_match[i] = 0;

			/* Dump the prompt */
			put_fstr(5, 16 + i, "%-5s", stat_names[i]);
		}

		/* Input the minimum stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* In the Antiband version this is dependent on class & stat */
			int def_weight = 50;

			/* Get a minimum stat */
			while (TRUE)
			{
				/* Move the cursor */
				Term_gotoxy(10, 16 + i);

				/* Default */
				strnfmt(inp, 80, "%i", def_weight);

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 9)) inp[0] = '\0';

				/* Extract an input */
				v = atoi(inp);

				/* Break on valid input */
				if (v <= 100) break;
			}

			/* Save the weight */
			stat_weight[i] = (v > 0) ? v : def_weight;
		}
	}

#endif /* ALLOW_AUTOROLLER */

	/* Clean up */
	clear_from(10);


	/*** Generate ***/

	/* Roll */
	while (TRUE)
	{
		int col = 42;

		/* Feedback */
		if (autoroller && !ironman_moria)
		{
			s32b best_score;
			s32b cur_score;

			Term_clear();

			/* Label */
			put_fstr(col + 5, 2, "Weight");

			/* Label */
			put_fstr(col + 13, 2, "  Roll");

			/* Put the stat weights */
			for (i = 0; i < A_MAX; i++)
			{
				/* Label stats */
				put_fstr(col, i + 3, stat_names[i]);

				/* Put the weight */
				put_fstr(col + 5, i + 3, CLR_L_BLUE "%6i", stat_weight[i]);
			}

			/* Note when we started */
			last_round = auto_round;

			/* Label count */
			put_fstr(col + 13, 10, "Round:");

			/* Indicate the state */
			put_fstr(col + 13, 12, "(Hit ESC to stop)");

			best_score = -1;
			for (i = 0; i < A_MAX; i++)
			{
				stat_save[i] = 3;
			}

			/* Auto-roll */
			while (TRUE)
			{
				/* Get a new character */
				get_stats();

				/* Advance the round */
				auto_round++;

				/* Hack -- Prevent overflow */
				if (auto_round >= 1000000L) break;

				/* Calculate a score for the rolled stats */
				cur_score = 0;
				for (i = 0; i < A_MAX; i++)
				{
		   			cur_score += (p_ptr->stat_cur[i]) * stat_weight[i];
				}

				/* Compare current score against saved stats */
				if (cur_score > best_score)
				{
					best_score = cur_score;
					for (i = 0; i < A_MAX; i++)
					{
						stat_save[i] = p_ptr->stat_cur[i];
					}
				}

				/* Break after 500 rolls */
				if (auto_round >= last_round + 500) break;

				/* Take note every x rolls */
				flag = (!(auto_round % AUTOROLLER_STEP));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Put the stats (and percents) */
					for (i = 0; i < A_MAX; i++)
					{
						/* Put the stat */
						put_fstr(col + 13, 3 + i, CLR_L_GREEN "%v",
								 stat_format, stat_use[i]);
					}

					/* Dump round */
					put_fstr(col + 20, 10, "%10ld", auto_round);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (flag) Term_xtra(TERM_XTRA_DELAY, 100);

					/* Do not wait for a key */
					p_ptr->inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
				}
			}

			/* Load best stat set rolled */
			for (i = 0; i < A_MAX; i++)
			{
				p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_save[i];
			}
		}

		/* Otherwise just get a character */
		else
		{
			/* Get a new character */
			get_stats();
		}

		/* Flush input */
		flush();


		/*** Display ***/

		/* Roll for base hitpoints */
		get_extra();

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for social class */
		get_history();

		/* Roll for gold */
		get_money();

		/* Hack -- get a chaos patron even if you are not a chaos warrior */
		p_ptr->chaos_patron = (s16b)randint0(MAX_PATRON);

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
			prtf(2, 23, "['r' to reroll%s, 'h' for history, or Enter to accept]",
				previous ? ", 'p' for prev": "");

			/* Prompt and get a command */
			ch = inkey();

			/* Quit */
			if (ch == KTRL('X')) quit(NULL);

			/* Start over */
			if (ch == ESCAPE) return (FALSE);

			/* Enter accepts the roll */
			if ((ch == '\r') || (ch == '\n')) break;

			/* Reroll this character */
			if ((ch == ' ') || (ch == 'r')) break;

			/* Previous character */
			if (previous && (ch == 'p'))
			{
				load_prev_data();
				continue;
			}

			/* Increase mode */
			if (ch == 'h')
			{
				mode = (mode + 1) % DISPLAY_PLAYER_MAX;
			}

			/* Help */
			if (ch == '?')
			{
				(void)show_file("birth.txt#CharDisplay", NULL, 0, 0);
				continue;
			}
			else if (ch == '=')
			{
				do_cmd_options(OPT_FLAG_BIRTH | OPT_FLAG_SERVER |
							   OPT_FLAG_PLAYER);
				continue;
			}

			/* Warning */
			bell("Illegal auto-roller command!");
		}

		/* Are we done? */
		if ((ch == '\r') || (ch == '\n')) break;

		/* Save this for the "previous" character */
		save_prev_data();

		/* Note that a previous roll exists */
		previous = TRUE;
	}

	/* Clear prompt */
	clear_from(23);

	/* Done */
	return (TRUE);
}


static bool player_birth_aux(void)
{
    char ch;
    int i;

	/* Ask questions */
	if (!player_birth_aux_1()) return FALSE;

	/* Point based */
	if (point_based && !ironman_moria)
	{
		if (!player_birth_aux_2()) return FALSE;
	}

	/* Auto-roll */
	else
	{
		if (!player_birth_aux_3()) return FALSE;
    }

    /* Apply some randomness */
    for (i = 0; i < A_MAX; i++)
    {
        p_ptr->stat_cur[i] += randint0(10);
        p_ptr->stat_max[i] = p_ptr->stat_cur[i];
    }

    /* Calculate the bonuses and hitpoints */
    p_ptr->update |= (PU_BONUS | PU_HP);

    /* Update stuff */
    update_stuff();

	/* Get a name, prepare savefile */
	get_character_name();

	/* Initialize the virtues */
	get_virtues();

	/* Display the player */
	display_player(DISPLAY_PLAYER_STANDARD);

	/* Prompt for it */
	prtf(10, 23,
		"['Ctrl-X' to suicide, 'Del' to start over, or Enter to continue]");

	/* Get a key */
	ch = inkey();

	/* Quit */
	if (ch == KTRL('X')) quit(NULL);

	/* Start over */
	if ((ch == 0x7F) || (ch == '.') || (ch == KTRL('H'))) return (FALSE);

	/* Accepted */

	/* Done */
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
	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
	}

	/* Create a note file if that option is set */
	if (take_notes)
	{
		add_note_type(NOTE_BIRTH);
	}

	/* Note player birth in the message recall */
	message_add(" ", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add("====================", MSG_GENERIC);
	message_add("  ", MSG_GENERIC);
	message_add(" ", MSG_GENERIC);

	/* Hack -- outfit the player */
	player_outfit();

	/* Set the message window flag as default */
	if (!window_flag[1])
		window_flag[1] |= PW_MESSAGE;

	/* Set the inv/equip window flag as default */
	if (!window_flag[2])
		window_flag[2] |= PW_INVEN;
}

