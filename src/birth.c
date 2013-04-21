/* File: birth.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
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
	cptr info; /* Textual History */

	byte roll; /* Frequency of this entry */
	byte chart;	/* Chart index */
	byte next; /* Next chart index */
	byte bonus;	/* Social Class Bonus + 50 */
};


/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human/Dunadan -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Elf      -->  4 -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Elf/High-Elf  -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
 *   Kobolds       --> 24 --> 25 --> 26 --> 27 --> 28 --> 29 --> 30
 *   Mutant        --> 31 --> 32 --> 33 --> 34 --> 35 --> 36 --> 37 --> 38
 *   Ghost         --> 39 --> 40 --> 41 --> 42
 *   Munchkin      --> 43
 *   Golem         --> 44 --> 45 --> 46 --> 47 --> 48
 *   Leprechaun    --> 67 --> 68 --> 69
 *   Death Mold    --> 70 --> 71 --> 72 --> 73 --> 74
 *   Vortex        --> 75 --> 76 

 * Note that the kobold race was added by GJW 	-KMW-
 *
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
static hist_type bg[] = {
	{"You are the illegitimate and unacknowledged child ", 10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ", 20, 1, 2, 35},
	{"You are one of several children ", 95, 1, 2, 45},
	{"You are the first child ", 100, 1, 2, 50},

	{"of a Serf.  ", 40, 2, 3, 65},
	{"of a Yeoman.  ", 65, 2, 3, 80},
	{"of a Townsman.  ", 80, 2, 3, 90},
	{"of a Guildsman.  ", 90, 2, 3, 105},
	{"of a Landed Knight.  ", 96, 2, 3, 120},
	{"of a Titled Noble.  ", 99, 2, 3, 130},
	{"of a Royal Blood Line.  ", 100, 2, 3, 140},

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

	{"Your mother was an Orc, but it is unacknowledged.  ", 25, 19, 20,
		25},
	{"Your father was an Orc, but it is unacknowledged.  ", 100, 19, 20,
		25},

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

	/* The following group of descriptions were added by GJW for the kobolds    -KMW- */
	{"You are the runt of ", 20, 24, 25, 40},
	{"You come from ", 80, 24, 25, 50},
	{"You are the largest of ", 100, 24, 25, 55},

	{"a litter of 3 pups.  ", 15, 25, 26, 45},
	{"a litter of 4 pups.  ", 40, 25, 26, 45},
	{"a litter of 5 pups.  ", 70, 25, 26, 50},
	{"a litter of 6 pups.  ", 85, 25, 26, 50},
	{"a litter of 7 pups.  ", 95, 25, 26, 55},
	{"a litter of 8 pups.  ", 100, 25, 26, 55},

	{"Your father was a fungus farmer, ", 10, 26, 27, 40},
	{"Your father was a hunter, ", 45, 26, 27, 45},
	{"Your father was a warrior, ", 70, 26, 27, 50},
	{"Your father was a shaman, ", 90, 26, 27, 55},
	{"Your father was the Chief of his tribe, ", 100, 26, 27, 60},

	{"and your mother was a prisoner of war.  ", 20, 27, 28, 45},
	{"and your mother was a prostitute.  ", 30, 27, 28, 45},
	{"and your mother was a cook.  ", 95, 27, 28, 50},
	{"and your mother was a member of the Chief's harem.  ", 100, 27, 28,
		55},

	{"You have black eyes, ", 10, 28, 29, 50},
	{"You have dark brown eyes, ", 40, 28, 29, 50},
	{"You have brown eyes, ", 80, 28, 29, 50},
	{"You have light brown eyes, ", 99, 28, 29, 50},
	{"You have glowing red eyes, ", 100, 28, 29, 55},

	{"dark brown skin, ", 40, 29, 30, 50},
	{"dark grey skin, ", 60, 29, 30, 50},
	{"olive green skin, ", 95, 29, 30, 50},
	{"deep blue skin, ", 100, 29, 30, 55},

	{"and large, flat teeth.", 10, 30, 0, 45},
	{"and small, sharp teeth.", 90, 30, 0, 50},
	{"and large, sharp teeth.", 100, 30, 0, 55},

	/* end of additional descriptions */

	{"You are the orphan of a family of ", 20, 31, 32, 35},
	{"You are the runt of a family of ", 50, 31, 32, 40},
	{"You come from a family of ", 70, 31, 32, 50},
	{"You are the largest of a family of ", 100, 31, 32, 55},

	{"3 spawns.  ", 15, 32, 33, 45},
	{"4 spawns.  ", 40, 32, 33, 45},
	{"5 spawns.  ", 70, 32, 33, 50},
	{"6 spawns.  ", 85, 32, 33, 50},
	{"7 spawns.  ", 95, 32, 33, 55},
	{"8 spawns.  ", 100, 32, 33, 55},

	{"Your father was a sickly shepherd, ", 10, 33, 34, 40},
	{"Your father was a hunter, ", 45, 33, 34, 45},
	{"Your father was a warrior, ", 70, 33, 34, 50},
	{"Your father was a High Priest, ", 90, 33, 34, 55},
	{"Your father was the Lord Warlock, ", 100, 33, 34, 60},

	{"and your mother was a sickly servant.  ", 20, 34, 35, 45},
	{"and your mother was a farmer.  ", 40, 34, 35, 45},
	{"and your mother was the village healer.  ", 80, 34, 35, 55},
	{"and your mother was the Lady Witch.  ", 100, 34, 35, 60},

	{"You have eyes on small stalks, ", 10, 35, 36, 50},
	{"You have no eyes, ", 20, 35, 36, 50},
	{"You have absolutely black eyes, ", 30, 35, 36, 50},
	{"You have clear eyes, ", 40, 35, 36, 50},
	{"You have glowing red eyes, ", 50, 35, 36, 50},
	{"You have photoreceptive skin, ", 60, 35, 36, 50},
	{"You have three blue eyes, ", 70, 35, 36, 50},
	{"You have a multitude of moving eyestalks, ", 80, 35, 36, 50},
	{"You have a eight watery yellow eyes, ", 90, 35, 36, 50},
	{"You have one large eye, ", 100, 35, 36, 50},

	{"flabby, pale skin, ", 10, 36, 37, 45},
	{"spotted skin, ", 20, 36, 37, 50},
	{"an extra set of arms, ", 30, 36, 37, 50},
	{"bumpy, scabrous skin, ", 40, 36, 37, 50},
	{"an extra set of legs, ", 50, 36, 37, 50},
	{"rubbery white skin, ", 60, 36, 37, 50},
	{"a set of wings, ", 70, 36, 37, 50},
	{"clear, oily skin, ", 80, 36, 37, 50},
	{"a segmented exoskeleton, ", 90, 36, 37, 50},
	{"a hard, bony carapace, ", 100, 36, 37, 55},

	{"a mane of orange hair, ", 10, 37, 38, 50},
	{"white, frizzly hair, ", 20, 37, 38, 50},
	{"sharp quills all over your body, ", 30, 37, 38, 50},
	{"a bony ridge along your skull, ", 40, 37, 38, 50},
	{"absolutely no hair, ", 50, 37, 38, 50},
	{"quills on top of your head, ", 60, 37, 38, 50},
	{"several thick hairs on your head, ", 70, 37, 38, 50},
	{"a set of tentacles on your head, ", 80, 37, 38, 50},
	{"an impenetrable mass of hair on your head, ", 90, 37, 38, 50},
	{"a coat of tangled body hair, ", 100, 37, 38, 50},

	{"and no mouth.", 10, 38, 0, 50},
	{"and small, flat teeth.", 20, 38, 0, 50},
	{"and a suction cup instead of a mouth.", 30, 38, 0, 50},
	{"and no teeth.", 40, 38, 0, 50},
	{"and one very sharp, very large tooth.", 50, 38, 0, 50},
	{"and long fangs.", 60, 38, 0, 50},
	{"and several soft, small teeth.", 70, 38, 0, 50},
	{"and a long tube instead of a mouth.", 80, 38, 0, 50},
	{"and three small mouths.", 90, 38, 0, 50},
	{"and rows upon rows of sharp teeth.", 100, 38, 0, 50},

	/* End of Mutant descriptions. XXX */

	/* Ghost descriptions */

	{"You are the wisp of ", 10, 39, 40, 25},
	{"You are the shade of ", 20, 39, 40, 35},
	{"You are the ghost of ", 40, 39, 40, 50},
	{"You are the ghoul of ", 60, 39, 40, 55},
	{"You are the wraith of ", 80, 39, 40, 65},
	{"You are the lich of ", 100, 39, 40, 75},

	{"a pathetic kobold. ", 10, 40, 41, 0},
	{"a lost shopkeeper. ", 20, 40, 41, 20},
	{"a mediocre adventurer. ", 30, 40, 41, 40},
	{"a valiant hero. ", 40, 40, 41, 50},
	{"a devout priest. ", 50, 40, 41, 60},
	{"an arcane wizard. ", 60, 40, 41, 70},
	{"a legendary explorer. ", 70, 40, 41, 80},
	{"an ancient wyrm. ", 80, 40, 41, 100},
	{"Sauron. ", 90, 40, 41, 150},
	{"Morgoth. ", 95, 40, 41, 200},
	{"an ancient God of antiquity. ", 100, 40, 41, 250},

	{"You died by weilding cursed equipment, ", 4, 41, 42, 5},
	{"You died by accidentally setting off a trap, ", 7, 41, 42, 5},
	{"You died by drinking the wrong potion, ", 10, 41, 42, 5},
	{"You died from lowered stats, ", 20, 41, 42, 10},
	{"You were killed by a mold, ", 30, 41, 42, 20},
	{"You were killed by a jelly, ", 40, 41, 42, 30},
	{"You were killed by Mughash, the Kobold Lord, ", 50, 41, 42, 40},
	{"You were killed by Golfimbul, the Hill Orc Chief, ", 60, 41, 42, 50},
	{"You were killed while fighting a horde of enemies, ", 70, 41, 42,
		60},
	{"You were killed while defending your lair, ", 80, 41, 42, 70},
	{"You were killed while challenging the Gods, ", 90, 41, 42, 150},
	{"You died of old age, ", 100, 41, 42, 200},

	{"and you are completely forgotten. ", 5, 42, 0, 5},
	{"and you have an old gravestone. ", 10, 42, 0, 10},
	{"and you are remembered by your immediate family. ", 20, 42, 0, 20},
	{"and you are remebered by your clan. ", 30, 42, 0, 30},
	{"and you are remembered in old manuscripts. ", 40, 42, 0, 40},
	{"and you are remembered in ancient legends. ", 50, 42, 0, 50},
	{"and you are remembered in popular tales. ", 60, 42, 0, 65},
	{"and you are remembered in religious canon. ", 70, 42, 0, 60},
	{"and you are glorified in a holy book. ", 80, 42, 0, 70},
	{"and you have a constellation named after you. ", 90, 42, 0, 90},
	{"and you are still feared among mortal men. ", 100, 42, 0, 100},

	/* End of ghost descriptions. */

	/* Munchkin description */

	{"You were born in the depths of time as a "
			"result of an evil experiment. Since then "
			"you walked through the expanses of time and "
			"space wrecking chaos and confusion on the "
			"hapless inhabitants therein. ", 100, 43, 0, 250},

	/* End */

	/* Golem description */

	{"You were made by an inquisitive child ", 10, 44, 45, 40},
	{"You were made as a school exercise ", 20, 44, 45, 40},
	{"You were made by a hapless apprentice ", 30, 44, 45, 40},
	{"You were made in a slave sweatshop ", 40, 44, 45, 45},
	{"You were made by a mediocre artisan ", 50, 44, 45, 50},
	{"You were made by a skillfull craftsman ", 60, 44, 45, 55},
	{"You were made as a guild order ", 70, 44, 45, 55},
	{"You were made on the king's order ", 80, 44, 45, 60},
	{"You were made by a gnomish tribe ", 90, 44, 45, 60},
	{"You were made as a gift by the Gods ", 100, 44, 45, 70},

	{"from wet clay. ", 10, 45, 46, 30},
	{"from wood shavings. ", 20, 45, 46, 35},
	{"from wood. ", 30, 45, 46, 40},
	{"from pebbles. ", 40, 45, 46, 45},
	{"from sand. ", 50, 45, 46, 50},
	{"from stone. ", 60, 45, 46, 50},
	{"from quartz. ", 70, 45, 46, 55},
	{"from platinum. ", 80, 45, 46, 60},
	{"from adamantite. ", 90, 45, 46, 65},
	{"from ethereal wisps. ", 100, 45, 46, 70},

	{"Your mission was to amuse the villagers. ", 10, 46, 47, 30},
	{"Your mission was to run errands. ", 20, 46, 47, 35},
	{"Your mission was to watch farm animals. ", 30, 46, 47, 40},
	{"Your mission was to guard a highway. ", 40, 46, 47, 45},
	{"Your mission was to sail a warship. ", 50, 46, 47, 50},
	{"Your mission was to fight in battle. ", 60, 46, 47, 50},
	{"Your mission was to serve in a mighty lord's mansion. ", 70, 46, 47,
		55},
	{"Your mission was to fight crime and corruption. ", 80, 46, 47, 60},
	{"Your mission was to be a mighty war machine. ", 90, 46, 47, 65},
	{"Your mission was to rule over mere mortals. ", 100, 46, 47, 70},

	{"You are pulled by draft animals, ", 10, 47, 48, 30},
	{"You have two wheels, ", 20, 47, 48, 35},
	{"You have four wheels, ", 30, 47, 48, 40},
	{"You have eight wheels, ", 40, 47, 48, 45},
	{"You have short, stubby legs, ", 50, 47, 48, 50},
	{"You have long, slender legs, ", 60, 47, 48, 50},
	{"You have eight spidery legs, ", 70, 47, 48, 55},
	{"You have many flexible tentacles for legs, ", 80, 47, 48, 60},
	{"You float on a giant magnet, ", 90, 47, 48, 65},
	{"You float on a crackling cloud of magic, ", 100, 47, 48, 70},

	{"and have no arms. ", 10, 48, 0, 30},
	{"and have one inflexible arm. ", 20, 48, 0, 35},
	{"and have one arm. ", 30, 48, 0, 40},
	{"and have two inflexible arms. ", 40, 48, 0, 45},
	{"and have two arms. ", 50, 48, 0, 50},
	{"and have two arms with clawed hands. ", 60, 48, 0, 50},
	{"and have four arms with clawed hands. ", 70, 48, 0, 55},
	{"and have eight tentacles for arms. ", 80, 48, 0, 60},
	{"and have a multitude of invisible tentacles for arms. ", 90, 48, 0,
		65},
	{"and have a multitude of invisible forcefields for arms. ", 100, 48,
		0, 70},

	/* These have been cut to make history fit in four lines. */

	{"Your brain is made of clay. ", 10, 49, 0, 30},
	{"Your brain is made of straw. ", 20, 49, 0, 35},
	{"Your brain is made of pebbles. ", 30, 49, 0, 40},
	{"Your brain is made of quartz. ", 40, 49, 0, 45},
	{"Your brain is made of steel. ", 50, 49, 0, 50},
	{"Your brain is a brain of a dead human. ", 60, 49, 0, 50},
	{"Your brain is a brain of a dead elf. ", 70, 49, 0, 55},
	{"Your brain is a brain of a dead gnome. ", 80, 49, 0, 60},
	{"Your brain is made of enchanted adamantite. ", 90, 49, 0, 65},
	{"Your brain is made of ethereal particles. ", 100, 49, 0, 70},

	/* End Golem descriptions. */

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

	/* Leprechaun descriptions. */

	{"You are a child of a farmer. ", 10, 67, 68, 30},
	{"You are a child of an artisan. ", 20, 67, 68, 35},
	{"You are a child of a mercenary. ", 30, 67, 68, 40},
	{"You are a child of a sailor. ", 40, 67, 68, 45},
	{"You are a child of a peddler. ", 50, 67, 68, 50},
	{"You are a child of a shopkeeper. ", 60, 67, 68, 50},
	{"You are a child of a petty thief. ", 70, 67, 68, 55},
	{"You are a child of a catburglar. ", 80, 67, 68, 60},
	{"You are a child of a pirate. ", 90, 67, 68, 65},
	{"You are a child of a leprechaun king. ", 100, 67, 68, 70},

	{"You lost your pot of gold gambling. ", 10, 68, 69, 30},
	{"You lost your pot of gold on a bet. ", 20, 68, 69, 35},
	{"You have a few copper coins in your pot of gold. ", 30, 68, 69, 40},
	{"You have a leather pouch of gold. ", 40, 68, 69, 45},
	{"You have a modest pot of gold. ", 50, 68, 69, 50},
	{"You have a large pot of gold. ", 60, 68, 69, 50},
	{"You have several pots of gold. ", 70, 68, 69, 55},
	{"You have a chest of gold coins. ", 80, 68, 69, 60},
	{"You have many, many pots of gold. ", 90, 68, 69, 65},
	{"You have uncounted hordes of gold. ", 100, 68, 69, 70},

	{"You are the village idiot. ", 10, 69, 0, 30},
	{"You are the laughing-stock of the town. ", 20, 69, 0, 35},
	{"You are a convicted criminal. ", 30, 69, 0, 40},
	{"You are a failed pickpocket. ", 40, 69, 0, 45},
	{"You are an apprentice burglar. ", 50, 69, 0, 50},
	{"You are a decent thief. ", 60, 69, 0, 50},
	{"You are a master theif. ", 70, 69, 0, 55},
	{"You are revered as the master of all thieves. ", 80, 69, 0, 60},
	{"You are the curse of all shopkeepers. ", 90, 69, 0, 65},
	{"Men hide their gold when they hear your name. ", 100, 69, 0, 70},

	/* End leprechaun description */

	/* Death mold description */

	{"You were born in dirty bilge-water, ", 10, 70, 71, 30},
	{"You were born in dirty straw, ", 20, 70, 71, 35},
	{"You were born in wet mud, ", 30, 70, 71, 40},
	{"You were born in a pile of dust, ", 40, 70, 71, 45},
	{"You were born in sand, ", 50, 70, 71, 50},
	{"You were born in pebbles, ", 60, 70, 71, 50},
	{"You were born in a kobold corpse, ", 70, 70, 71, 55},
	{"You were born in dragon droppings, ", 80, 70, 71, 60},
	{"You were born in a pile of bones, ", 90, 70, 71, 65},
	{"You were born in a corpse of a mighty hero, ", 100, 70, 71, 70},

	{"created by rotting flesh. ", 10, 71, 72, 30},
	{"created by a kobold magician. ", 20, 71, 72, 35},
	{"created by a corrupted apprentice. ", 30, 71, 72, 40},
	{"created by a curious mage apprentice. ", 40, 71, 72, 45},
	{"created by an evil Beastmaster. ", 50, 71, 72, 50},
	{"created by a practicing Necromancer. ", 60, 71, 72, 50},
	{"created by the Mutant Breeders. ", 70, 71, 72, 55},
	{"created by a curious adventurer. ", 80, 71, 72, 60},
	{"called to life by the Witch-King of Angmar. ", 90, 71, 72, 65},
	{"called to life by Sauron himself. ", 100, 71, 72, 70},

	{"Since then you have given life to ", 100, 72, 73, 50},

	{"no ", 10, 73, 74, 30},
	{"one weak-willed ", 20, 73, 74, 35},
	{"two ", 30, 73, 74, 40},
	{"three ", 40, 73, 74, 45},
	{"four ", 50, 73, 74, 50},
	{"five ", 60, 73, 74, 50},
	{"about twenty ", 70, 73, 74, 55},
	{"dozens of ", 80, 73, 74, 60},
	{"hundreds of ", 90, 73, 74, 65},
	{"uncounted multitudes of ", 100, 73, 74, 70},

	{"foul offspring. ", 100, 74, 0, 50},

	{"You are a towering burst of flame, ", 30, 75, 76, 20},
	{"You are a frigid cloud of ice, ", 40, 75, 76, 30},
	{"You are a bundle of raw electricity, ", 50, 75, 76, 30},
	{"You are a spinning vortex of nether, ", 60, 75, 76, 40},
	{"You are a puddle of sticky slime, ", 70, 75, 76, 20},
	{"You are a dazzling ball of light, ", 80, 75, 76, 30},
	{"You are a cloud of damp vapor, ", 100, 75, 76, 20},

	{"created by a powerful wizard. ", 30, 76, 0, 10},
	{"made from vaporous wisps by an Elven king.", 40, 76, 0, 20},
	{"shaped by the mental energy of an evil demon. ", 50, 76, 0, 30},
	{"the essence of the mental energy of an infamous lich. ", 60, 76, 0,
		40},
	{"teleported to the material plane by a hapless adventurer. ",
		80, 76, 0, 10},
	{"created by an Elemental to serve as a slave. ", 80, 76, 0,
		20},
	{"created by a magic spell gone wrong. ", 100, 76, 0, 10},


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
		strcpy(prev.history[i], p_ptr->history[i]);
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
	for (i = 0; i < 6; i++)
	{
		temp.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(temp.history[i], p_ptr->history[i]);
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
		strcpy(p_ptr->history[i], prev.history[i]);
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
 * Adjust a stat by an amount
 *
 * The "auto_roll" flag selects "maximal" changes for use with the
 * auto-roller initialization code.  Otherwise, if "maximize" mode
 * is being used, the changes are fixed.  Otherwise, semi-random
 * changes will occur, with larger changes at lower values.
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
			if (value >= 18 + 10)
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
		if ((j > 42) && (j < 54))
			break;
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
	int i, j, min_value, max_value;

	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	spell_num = 0;

	/* Initialize spells. */
	if (cp_ptr->uses_magic && cp_ptr->spell_book != SV_SPELLBOOK_NONE)
	{
		spell_num =
			init_s_info_txt(cp_ptr->spell_book, spells, MAX_SPELLS);
	}

	/* Hack -- Start with a Corrupted spell. */
	if (p_ptr->pclass == CLASS_CORRUPTED)
	{
		spell_generate_new(1);
	}

	/* Hack -- Start with some mutations. */
	if (p_ptr->prace == RACE_MUTANT)
	{
		int i;
		int j = rand_range(3, 6);

		p_ptr->mutations1 = 0L;
		p_ptr->mutations2 = 0L;
		p_ptr->mutations3 = 0L;

		for (i = 0; i < j; i++)
		{
			generate_mutation();
		}
	}

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Initialize arena and rewards information -KMW- */
	p_ptr->which_arena = 0;

	for (i = 0; i < MAX_ARENAS; i++)
	{
		p_ptr->arena_number[i] = 0;
	}

	for (i = 0; i < MAX_REWARDS; i++)
	{
		rewards[i] = FALSE;
	}

	p_ptr->inside_special = 0;
	p_ptr->wilderness_py = 0;
	p_ptr->wilderness_px = 0;
	p_ptr->wilderness_depth = 0;
	p_ptr->exit_bldg = TRUE;

	/* Initialize gods info. */
	p_ptr->grace = 0;
	p_ptr->pgod = 0;

	p_ptr->luck = 0;

	p_ptr->pets_notice = 0;
	p_ptr->number_pets = 0;

	/* Hack -- Start with a god. */
	if (p_ptr->pclass == CLASS_AVATAR) {

	  int pgod;

	  while (1) {
	    pgod = rand_int(MAX_GODS);

	    if (magik(100 / ((deity_info[pgod].rarity % 4) + 1))) break;
	  }

	  mformat(MSG_BONUS, 
		  "You are forevermore fated to be the Avatar of %s, "
		  "the God%s of %s.", 
		  deity_info[pgod].name, 
		  (deity_info[pgod].female ? "dess" : ""),
		  deity_info[pgod].god_of);

	  msg_print(NULL);

	  p_ptr->pgod = pgod + 1;
	  p_ptr->grace = 200;
	  p_ptr->god_favor = -60000;
	}

	    
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
			j = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + j;
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (p_ptr->player_hp[PY_MAX_LEVEL - 1] < min_value)
			continue;
		if (p_ptr->player_hp[PY_MAX_LEVEL - 1] > max_value)
			continue;

		/* Acceptable */
		break;
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
	for (i = 0; i < 4; i++)
		p_ptr->history[i][0] = '\0';


	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
		case RACE_DUNADAN:
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

			/* Added by GJW -KMW- */
		case RACE_KOBOLD:
		{
			chart = 24;
			break;
		}

			/* Mutant XXX */

		case RACE_MUTANT:
		{
			chart = 33;	/* XXX !HACK! Cut out 31-32 to keep history short. */
			break;
		}

		case RACE_GHOST:
		{
			chart = 39;
			break;
		}

		case RACE_MUNCHKIN:
		{
			chart = 43;
			break;
		}

		case RACE_GOLEM:
		{
			chart = 44;
			break;
		}

		case RACE_LEPRECHAUN:
		{
			chart = 67;
			break;
		}

		case RACE_MOLD:
		{
			chart = 70;
			break;
		}

		case RACE_VORTEX:
		{
			chart = 75;
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
		while ((chart != bg[i].chart) || (roll > bg[i].roll))
			i++;

		/* Acquire the textual history */
		strcat(buf, bg[i].info);

		/* Add in the social class */
		social_class += (int) (bg[i].bonus) - 50;

		/* Enter the next chart */
		chart = bg[i].next;
	}



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
		if (n < 60 || i == 4)
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
		while ((n > 0) && (s[n - 1] == ' '))
			s[--n] = '\0';

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
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females or neuters */
	else
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
		if (stat_use[i] >= 18 + 50)
			gold -= 300;
		else if (stat_use[i] >= 18 + 20)
			gold -= 200;
		else if (stat_use[i] > 18)
			gold -= 150;
		else
			gold -= (stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100)
		gold = 100;

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
	int i, p;
	byte attr;

	char buf[80];


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
			sprintf(buf, "%3d.%d%%", p / 10, p % 10);
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
 * Fill the random_artifacts array with relevant info.
 */

static errr init_randart(void)
{
	int i, j;
	long foo;
	random_artifact *ra_ptr;
	spell *activ;

	for (i = 0; i < MAX_RANDARTS; i++)
	{
		ra_ptr = &random_artifacts[i];

		strcpy(ra_ptr->name_short, get_line("rart_s.txt", i));
		strcpy(ra_ptr->name_full, get_line("rart_f.txt", i));

		ra_ptr->attr = randint(15);

		ra_ptr->level = rand_int(101);

		while (TRUE)
		{
			j = rand_int(activation_num);
			activ = &activations[j];

			if (randnor(activ->level, 2) > ra_ptr->level)
			{
				continue;
			}

			ra_ptr->activation = j;
			ra_ptr->generated = FALSE;

			foo = (randnor(ra_ptr->level, 2)) * 250;

			if (foo < 0)
				foo = 0;

			ra_ptr->cost = foo;

			break;
		}
	}

	return 0;
}

/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;

	object_type *o_ptr;
	object_type *o_nxt;

	/* Wipe the inventory. */
	o_ptr = inventory;

	while (TRUE)
	{
		if (!o_ptr)
			break;

		o_nxt = o_ptr->next;

		remove_object(o_ptr);
		o_ptr = o_nxt;
	}

	/* Wipe the equipment. */
	for (i = 0; i < EQUIP_MAX; i++)
	{
		equipment[i] = NULL;
	}


	/* Generate random artifacts */
	init_randart();

	/* Start with no artifacts made yet */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}

	/* Wipe recipe recall */
	for (i = 0; i < MAX_RECIPES; i++)
	{
		if (randint(50) == 1 && recipe_info[i].ingrs)
		{
			recipe_recall[i] = 1;
		}
		else
		{
			recipe_recall[i] = 0;
		}
	}

	/* Wipe quest status */
	for (i = 0; i < max_quests; i++)
	{
		quest_status[i] = 0;
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
		if (r_ptr->flags1 & (RF1_UNIQUE))
			r_ptr->max_num = 1;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- no ghosts */
	r_info[MAX_R_IDX - 1].max_num = 0;

}




/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */

static byte player_init[MAX_CLASS][3][2] = {
	{
			/* Warrior */
			{TV_POTION, SV_POTION_BESERK_STRENGTH},
			{TV_SWORD, SV_BROAD_SWORD},
			{TV_HARD_ARMOR, SV_CHAIN_MAIL}
		},

	{
			/* Mage */
			{TV_SPELLBOOK, SV_SPELLBOOK_MAGE},
			{TV_SWORD, SV_DAGGER},
			{TV_SCROLL, SV_SCROLL_WORD_OF_RECALL}
		},

	{
			/* Priest */
			{TV_SPELLBOOK, SV_SPELLBOOK_PRIEST},
			{TV_HAFTED, SV_MACE},
			{TV_POTION, SV_POTION_HEALING}
		},

	{
			/* Rogue */
			{TV_SPELLBOOK, SV_SPELLBOOK_ROGUE},
			{TV_SWORD, SV_SMALL_SWORD},
			{TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
		},

	{
			/* Ranger */
			{TV_SPELLBOOK, SV_SPELLBOOK_RANGER},
			{TV_SWORD, SV_BROAD_SWORD},
			{TV_BOW, SV_LONG_BOW}
		},

	{
			/* Paladin */
			{TV_SPELLBOOK, SV_SPELLBOOK_PRIEST},
			{TV_SWORD, SV_BROAD_SWORD},
			{TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL}
		},

	{
			/* Illusionist  -KMW-  */
			{TV_SPELLBOOK, SV_SPELLBOOK_ILLUSIONIST},
			{TV_SWORD, SV_DAGGER},
			{TV_SCROLL, SV_SCROLL_WORD_OF_RECALL}
		},

	{
			/* Currupted XXX */
			{TV_POTION, SV_POTION_DETONATIONS},
			{TV_SCROLL, SV_SCROLL_WORD_OF_RECALL},
			{TV_POTION, SV_POTION_HEALING}
		},

	{
			/* Beastmaster */
			{TV_POTION, SV_POTION_BESERK_STRENGTH},
			{TV_SWORD, SV_BROAD_SWORD},
			{TV_HARD_ARMOR, SV_CHAIN_MAIL}
		},

	{ /* Lycanthrope */
			{TV_SWORD, SV_BROAD_SWORD},
			{TV_POTION, SV_POTION_HEALING},
			{TV_HARD_ARMOR, SV_CHAIN_MAIL}
		},

	{ /* Mimic */
			{TV_MIMIC_BOOK, 9},
			{TV_SWORD, SV_DAGGER},
			{TV_POTION, SV_POTION_HEALING}
		},

	{ /* Vampire */
			{TV_SPELLBOOK, SV_SPELLBOOK_EVIL},
			{TV_SWORD, SV_SMALL_SWORD},
			{TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR}
		},

	{ /* Bard */
			{TV_SPELLBOOK, SV_SPELLBOOK_BARD},
			{TV_SWORD, SV_DAGGER},
			{TV_POTION, SV_POTION_HEALING}
		},

	{ /* Necromancer */
			{TV_SPELLBOOK, SV_SPELLBOOK_NECRO},
			{TV_SWORD, SV_SCALPEL},
			{TV_POTION, SV_POTION_HEALING}
		},

	{ /* Elemental */
			{TV_WAND, SV_WAND_FIRE_BOLT},
			{TV_WAND, SV_WAND_COLD_BOLT},
			{TV_WAND, SV_WAND_ELEC_BOLT}
		},
	{ /* Avatar */
	  { TV_SWORD, SV_DAGGER },
	  { TV_POTION, SV_POTION_HEALING },
	  { TV_STAFF, SV_STAFF_POWER }
	}
};

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i, tv, sv;

	object_type *i_ptr;

	/* Hack -- Give the player some food */
	/* Useless for vampires. */
	if (p_ptr->pclass != CLASS_VAMPIRE)
	{
		i_ptr = new_object();

		object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));

		i_ptr->number = rand_range(3, 7);
		i_ptr->weight *= i_ptr->number;

		object_aware(i_ptr);
		object_known(i_ptr);
		(void) inven_carry(i_ptr);
	}

	/* Hack -- Give the player some torches */
	/* Useless for ghosts & munchkins */

	if (p_ptr->prace != RACE_GHOST && p_ptr->prace != RACE_MUNCHKIN &&
	    p_ptr->prace != RACE_VORTEX)
	{
		int foo = rand_range(3, 7);
		int i;

		for (i = 0; i < foo; i++)
		{
			i_ptr = new_object();
			object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));

			apply_magic(i_ptr, 1, FALSE, FALSE, FALSE);
			object_aware(i_ptr);
			object_known(i_ptr);
			(void) inven_carry(i_ptr);
		}
	}

	/* Give ghosts id scrolls */
	if (p_ptr->prace == RACE_GHOST)
	{
		i_ptr = new_object();

		object_prep(i_ptr, lookup_kind(TV_SCROLL, SV_SCROLL_IDENTIFY));

		i_ptr->number = rand_range(60, 100);
		i_ptr->weight *= i_ptr->number;

		object_aware(i_ptr);
		object_known(i_ptr);
		inven_carry(i_ptr);
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		i_ptr = new_object();

		/* Hack -- Give the player an object */
		object_prep(i_ptr, lookup_kind(tv, sv));
		object_aware(i_ptr);
		object_known(i_ptr);
		(void) inven_carry(i_ptr);
	}
}


/*
 * Prompt the player for a town layout and arena.
 */

s16b select_town_layout(bool arenap)
{
	int i = 0;
	int c;
	vault_type *v_ptr;

	clear_from(20);

	while (TRUE)
	{
		v_ptr = &v_info[i];

		if ((arenap && v_ptr->typ == 11) || (!arenap && v_ptr->typ == 10))
		{

			prt(format("%s: Use this %s layout? (y/n) ",
					v_name + v_ptr->name, arenap ? "arena" : "town"), 20,
				2);

			c = inkey();

			if (c == 'Q')
				quit(NULL);
			if (c == 'S')
				return -1;

			if (c == 'y')
			{
				return i;
			}
		}

		i++;

		if (i == MAX_V_IDX)
			i = 0;
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

#if 0
	char p1 = '(';
#endif
	char p2 = ')';
	char b1 = '[';
	char b2 = ']';

	char buf[80];

	bool autoroll = FALSE;


	/*** Intro ***/

	/* Clear screen */
	Term_clear();

	/* Title everything */
	put_str("Name        :", 2, 1);
	put_str("Sex         :", 3, 1);
	put_str("Race        :", 4, 1);
	put_str("Class       :", 5, 1);

	/* Dump the default name */
	c_put_str(TERM_L_BLUE, op_ptr->full_name, 2, 15);


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
		put_str(buf, 21 + (n / 5), 2 + 15 * (n % 5));
	}

	/* Choose */
	while (1)
	{
		sprintf(buf, "Choose a sex (%c-%c): ", I2A(0), I2A(n - 1));
		put_str(buf, 20, 2);
		c = inkey();
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
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
	Term_putstr(5, 16, -1, TERM_YELLOW,
		"Warning: Only choose to play a munchkin if you're a cheater!");

	/* Dump races */
	for (n = 0; n < MAX_RACES; n++)
	{
		/* Analyze */
		p_ptr->prace = n;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;

		/* Display */
		sprintf(buf, "%c%c %s", I2A(n), p2, str);
		put_str(buf, 20 + (n / 5), 2 + 15 * (n % 5));
	}

	/* Choose */
	while (1)
	{
		sprintf(buf, "Choose a race (%c-%c): ", I2A(0), I2A(n - 1));
		put_str(buf, 19, 2);
		c = inkey();
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
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

	/* Dump classes */
	for (n = 0; n < MAX_CLASS; n++)
	{
		cptr mod = "";

		/* Analyze */
		p_ptr->pclass = n;
		cp_ptr = &class_info[p_ptr->pclass];
		str = cp_ptr->title;

		/* Display */
		sprintf(buf, "%c%c %s%s", I2A(n), p2, str, mod);
		put_str(buf, 18 + (n / 3), 2 + 20 * (n % 3));
	}

	/* Get a class */
	while (1)
	{
		sprintf(buf, "Choose a class (%c-%c): ", I2A(0), I2A(n - 1));
		put_str(buf, 17, 2);
		c = inkey();
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
	}

	/* Set class */
	p_ptr->pclass = k;
	cp_ptr = &class_info[p_ptr->pclass];
	str = cp_ptr->title;

	/* Display */
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

	/* Clean up */
	clear_from(15);


	/*** Maximize mode ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
		"Using 'maximize' mode makes the game harder at the start,");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"but often makes it easier to win.");

	/* Ask about "maximize" mode */
	while (1)
	{
		put_str("Use 'maximize' mode? (y/n) ", 20, 2);
		c = inkey();
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		if (c == ESCAPE)
			break;
		if ((c == 'y') || (c == 'n'))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
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
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		if (c == ESCAPE)
			break;
		if ((c == 'y') || (c == 'n'))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
	}

	/* Set "preserve" mode */
	p_ptr->preserve = (c == 'y');

	/* Clear */
	clear_from(15);


	/**** Dungeon seed ****/

	Term_putstr(5, 15, -1, TERM_WHITE,
		"If you select 'yes', all dungeon generation will use "
		"the same seed. ");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"This means that the structure of each level will be "
		"the same, though");
	Term_putstr(5, 17, -1, TERM_WHITE, "monsters and items will change.");

	/* Ask about dungeon seed */
	while (1)
	{
		put_str("Generate persistent dungeons? (y/n) ", 20, 2);
		c = inkey();
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		if (c == ESCAPE)
			break;
		if ((c == 'y') || (c == 'n'))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
	}

	/* Set dungeon seed */

	if (c == 'y')
	{
		seed_dungeon = rand_int(0x10000000);
	}
	else
	{
		seed_dungeon = 0;
	}

	/* Clear */
	clear_from(15);

	/**** Select a town and an arena layout ****/

	while (TRUE)
	{
		put_str("Select a random town layout? (y/n) ", 20, 2);
		c = inkey();

		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		if (c == ESCAPE)
			break;
		if ((c == 'y') || (c == 'n'))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
	}


	if (c == 'n')
	{
		int i;

		i = select_town_layout(FALSE);

		if (i < 0)
			return FALSE;
		p_ptr->which_town = i;

		i = select_town_layout(TRUE);

		if (i < 0)
			return FALSE;
		p_ptr->which_arena_layout = i;

	}
	else
	{
		int vindex;
		vault_type *v_ptr;
		bool foo = FALSE;
		bool bar = FALSE;

		while (TRUE)
		{
			vindex = rand_int(MAX_V_IDX);
			v_ptr = &v_info[vindex];

			if (v_ptr->typ == 10)
			{
				p_ptr->which_town = vindex;
				foo = TRUE;
			}
			else if (v_ptr->typ == 11)
			{
				p_ptr->which_arena_layout = vindex;
				bar = TRUE;
			}

			if (foo && bar)
				break;
		}
	}


	/**** Alignement ****/

/* -----------GREP-------------- 

	Term_putstr(5, 15, -1, TERM_WHITE,
		"Playing as an evil character allows you to fight lawful monsters,");
	Term_putstr(5, 16, -1, TERM_WHITE,
		"ones like Bilbo Baggins, Gandalf, etc.                  ");

	while (1)
	{
		put_str("Play as an evil character? (y/n) ", 20, 2);
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (FALSE);
		if (c == ESCAPE) break;
		if ((c == 'y') || (c == 'n')) break;
		if (c == '?') do_cmd_help();
		else bell();
	}


	if (c == 'y') {
	  p_ptr->is_evil = TRUE;
	} else {
	  p_ptr->is_evil = FALSE;
	}

  ------------------------GREP--------------------------*/

	p_ptr->is_evil = FALSE;

	/* Clear */
	clear_from(20);


	/* Mutants get random bonuses. */
	if (p_ptr->prace == RACE_MUTANT)
	{
		p_ptr->prace_info = randint(5);
	}
	else
	{
		p_ptr->prace_info = 0;
	}

	/* Random stat gain/loss for mutants. */
	/* Mutants get +/- 1 to each stat. */

	if (p_ptr->prace == RACE_MUTANT)
	{
		for (i = 0; i < 6; i++)
		{
			rp_ptr->r_adj[i] += randint(3) - 2;
		}
	}

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
		if (c == 'Q')
			quit(NULL);
		if (c == 'S')
			return (FALSE);
		if (c == ESCAPE)
			break;
		if ((c == 'y') || (c == 'n'))
			break;
		if (c == '?')
			do_cmd_help();
		else
			bell();
	}

	/* Set "autoroll" */
	autoroll = (c == 'y');

	/* Clear */
	clear_from(15);


	/* Initialize */
	if (autoroll)
	{
		int mval[6];

		char inp[80];


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
				if (!askfor_aux(inp, 8, FALSE))
					inp[0] = '\0';

				/* Hack -- add a fake slash */
				strcat(inp, "/");

				/* Hack -- look for the "slash" */
				s = strchr(inp, '/');

				/* Hack -- Nuke the slash */
				*s++ = '\0';

				/* Hack -- Extract an input */
				v = atoi(inp) + atoi(s);

				/* Break on valid input */
				if (v <= mval[i])
					break;
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

			c_put_str(TERM_L_BLUE, op_ptr->full_name, 2, 15);
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
				break;

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
			if (accept)
				break;

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
				if (flag)
					Term_xtra(TERM_XTRA_DELAY, 100);

				/* Do not wait for a key */
				inkey_scan = TRUE;

				/* Check for a keypress */
				if (inkey())
					break;
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

		/* Input loop */
		while (TRUE)
		{
			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_MANA | PU_HP | PU_SANITY);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Fully sane */
			p_ptr->csane = p_ptr->msane;

			/* Display the player */
			display_player(mode);

			/* Prepare a prompt (must squeeze everything in) */
			Term_gotoxy(2, 23);
			Term_addch(TERM_WHITE, b1);
			Term_addstr(-1, TERM_WHITE, "'r' to reroll");
			if (prev)
				Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
			if (mode)
				Term_addstr(-1, TERM_WHITE, ", 'h' for Misc.");
			else
				Term_addstr(-1, TERM_WHITE, ", 'h' for History");
			Term_addstr(-1, TERM_WHITE, ", or ESC to accept");
			Term_addch(TERM_WHITE, b2);

			/* Prompt and get a command */
			c = inkey();

			/* Quit */
			if (c == 'Q')
				quit(NULL);

			/* Start over */
			if (c == 'S')
				return (FALSE);

			/* Escape accepts the roll */
			if (c == ESCAPE)
				break;

			/* Reroll this character */
			if ((c == ' ') || (c == 'r'))
				break;

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

			/* Warning */
			bell();
		}

		/* Are we done? */
		if (c == ESCAPE)
			break;

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
	if (c == 'Q')
		quit(NULL);

	/* Start over */
	if (c == 'S')
		return (FALSE);

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


	/* Reset all global tables. */
	player_wipe();


	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		WIPE(p_ptr, player_type);

		/* Hack -- Well fed player */
		p_ptr->food = PY_FOOD_FULL - 1;

		/* Roll up a new character */
		if (player_birth_aux())
			break;
	}

	/* Note player birth in the message recall */
	message_add(" ", MSG_NORMAL);
	message_add("  ", MSG_NORMAL);
	message_add("====================", MSG_NORMAL);
	message_add("  ", MSG_NORMAL);
	message_add(" ", MSG_NORMAL);


	/* Hack -- outfit the player */
	player_outfit();

	if (p_ptr->pclass == CLASS_VAMPIRE)
	{
		p_ptr->depth = 1;
	}

	/* Setup starting depth */
	if (p_ptr->prace == RACE_GHOST)
	{
		p_ptr->depth = 90 + randnor(0, 2);
	}

	if (p_ptr->prace == RACE_MUNCHKIN)
	{
		p_ptr->depth = 30 + randnor(0, 4);
		p_ptr->noscore |= 0x0001;
	}

	/* Shops */
	for (n = 0; n < MAX_STORES; n++)
	{
		/* Initialize */
		store_init(n);

		/* Ignore home */
		if (n == MAX_STORES - 1)
			continue;

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++)
			store_maint(n);
	}

	/* Select bounty monsters. */
	select_bounties();
}
