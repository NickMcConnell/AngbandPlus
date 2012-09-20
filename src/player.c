/* player.c: player specific variable definitions

   Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

   This software may be copied and distributed for educational, research, and
   not for profit purposes provided that this copyright and statement are
   included in all such copies. */

#include "constant.h"
#include "config.h"
#include "types.h"

/* Player record for most player related info */
player_type py;
/* player location in dungeon */
int16 char_row;
int16 char_col;
/* calculated base hp values for player at each level, store them so that
   drain life + restore life does not affect hit points */
int16u player_hp[MAX_PLAYER_LEVEL];

/* Class titles for different levels				*/
#ifdef MACGAME
char *(*player_title)[MAX_PLAYER_LEVEL];
#else
char *player_title[MAX_CLASS][MAX_PLAYER_LEVEL] = {
	/* Warrior	 */
{"Rookie","Private","Soldier","Mercenary","Veteran(1st)","Veteran(2nd)",
"Veteran(3rd)","Warrior(1st)","Warrior(2nd)","Warrior(3rd)","Warrior(4th)",
"Swordsman-1","Swordsman-2","Swordsman-3","Hero","Swashbuckler","Myrmidon",
"Champion-1","Champion-2","Champion-3","Superhero","Knight","Superior Knt",
"Gallant Knt","Knt Errant","Guardian Knt","Baron","Duke","Lord (1st)",
"Lord (2nd)","Lord (3rd)","Lord (4th)","Lord (5th)","Lord (6th)","Lord (7th)",
"Lord (8th)","Lord (9th)","Lord (10th)","Lord (11th)","Lord (12th)",
"Lord (13th)","Lord (14th)","Lord (15th)","Lord (16th)","Lord (17th)",
"Lord (18th)","Lord (19th)","Lord Gallant","Lord Keeper","Lord Noble"},
	/* Mage		 */
{"Novice","Apprentice","Trickster-1","Trickster-2","Trickster-3","Cabalist-1",
"Cabalist-2","Cabalist-3","Visionist","Phantasmist","Shadowist","Spellbinder",
"Illusionist","Evoker (1st)","Evoker (2nd)","Evoker (3rd)","Evoker (4th)",
"Conjurer","Theurgist","Thaumaturge","Magician","Enchanter","Warlock",
"Sorcerer","Necromancer","Mage (1st)","Mage (2nd)","Mage (3rd)","Mage (4th)",
"Mage (5th)","Wizard (1st)","Wizard (2nd)","Wizard (3rd)","Wizard (4th)",
"Wizard (5th)","Wizard (6th)","Wizard (7th)","Wizard (8th)","Wizard (9th)",
"Wizard (10th)","Wizard (11th)","Wizard (12th)","Wizard (13th)",
"Wizard (14th)","Wizard (15th)","Wizard (16th)","Wizard (17th)",
"Wizard (18th)","Wizard (19th)","Wizard Lord"},
	/* Priests	 */
{"Believer","Acolyte(1st)","Acolyte(2nd)","Acolyte(3rd)","Adept (1st)",
"Adept (2nd)","Adept (3rd)","Priest (1st)","Priest (2nd)","Priest (3rd)",
"Priest (4th)","Priest (5th)","Priest (6th)","Priest (7th)","Priest (8th)",
"Priest (9th)","Curate (1st)","Curate (2nd)","Curate (3rd)","Curate (4th)",
"Curate (5th)","Curate (6th)","Curate (7th)","Curate (8th)","Curate (9th)",
"Canon (1st)","Canon (2nd)","Canon (3rd)","Canon (4th)","Canon (5th)",
"Canon (6th)","Canon (7th)","Canon (8th)","Canon (9th)",
"Low Lama","Lama-1","Lama-2","Lama-3","Lama-4","Lama-5","Lama-6","Lama-7",
"Lama-8","Lama-9","High Lama","Great Lama","Patriarch",
"High Priest","Great Priest","Noble Priest"},
	/* Rogues	 */
{"Vagabond","Footpad","Cutpurse","Robber","Burglar","Filcher","Sharper",
"Magsman","Common Rogue","Rogue (1st)","Rogue (2nd)","Rogue (3rd)",
"Rogue (4th)","Rogue (5th)","Rogue (6th)","Rogue (7th)","Rogue (8th)",
"Rogue (9th)","Master Rogue","Expert Rogue","Senior Rogue","Chief Rogue",
"Prime Rogue","Low Thief","Thief (1st)","Thief (2nd)","Thief (3rd)",
"Thief (4th)","Thief (5th)","Thief (6th)","Thief (7th)","Thief (8th)",
"Thief (9th)","Thief (10th)","Thief (11th)","Thief (12th)","Thief (13th)",
"Thief (14th)","Thief (15th)","Thief (16th)","Thief (17th)","Thief (18th)",
"Thief (19th)","High Thief","Master Thief","Executioner","Low Assassin",
"Assassin","High Assassin","Guildsmaster"},
	/* Rangers	 */
{"Runner (1st)","Runner (2nd)","Runner (3rd)","Strider (1st)","Strider (2nd)",
"Strider (3rd)","Scout (1st)","Scout (2nd)","Scout (3rd)","Scout (4th)",
"Scout (5th)","Courser (1st)","Courser (2nd)","Courser (3rd)","Courser (4th)",
"Courser (5th)","Tracker (1st)","Tracker (2nd)","Tracker (3rd)",
"Tracker (4th)","Tracker (5th)","Tracker (6th)","Tracker (7th)",
"Tracker (8th)","Tracker (9th)","Guide (1st)","Guide (2nd)","Guide (3rd)",
"Guide (4th)","Guide (5th)","Guide (6th)","Guide (7th)","Guide (8th)",
"Guide (9th)","Guide (10th)","Guide (11th)","Guide (12th)","Guide (13th)",
"Guide (14th)","Guide (15th)",
"Pathfinder-1","Pathfinder-2","Pathfinder-3","Pathfinder-4","Pathfinder-5",
"Pathfinder-6","Pathfinder-7","Ranger","High Ranger","Ranger Lord"},
	/* Paladins	 */
{"Gallant","Keeper (1st)","Keeper (2nd)","Keeper (3rd)","Keeper (4th)",
"Keeper (5th)","Keeper (6th)","Keeper (7th)","Keeper (8th)","Keeper (9th)",
"Protector-1","Protector-2","Protector-3","Protector-4","Protector-5",
"Protector-6","Protector-7","Protector-8","Defender-1","Defender-2",
"Defender-3","Defender-4","Defender-5","Defender-6","Defender-7","Defender-8",
"Warder (1st)","Warder (2nd)","Warder (3rd)","Warder (4th)","Warder (5th)",
"Warder (6th)","Warder (7th)","Warder (8th)","Warder (9th)","Warder (10th)",
"Warder (11th)","Warder (12th)","Warder (13th)","Warder (14th)",
"Warder (15th)","Warder (16th)","Warder (17th)","Warder (18th)",
"Warder (19th)","Guardian","Chevalier","Justiciar","Paladin","High Lord"},
       /* Monks        */
{"Humble","Student-1","Student-2","Student-3","Student-4","Student-5",
 "Student-6","Student-7","Student-8","Student-9","Honored-1","Honored-2",
 "Honored-3","Honored-4","Honored-5","Honored-6","Honored-7","Honored-8",
 "Honored-9","Holy One","Adept-1","Adept-2","Adept-3","Adept-4","Adept-5",
 "Adept-6","Adept-7","Adept-8","Adept-9","Monk (1st)","Monk (2nd)","Monk (3rd)",
 "Monk (4th)","Monk (5th)","Monk (6th)","Monk (7th)","Monk (8th)","Monk (9th)",
 "Monk (10th)","Monk (11th)","Monk (12th)","Monk (13th)","Monk (14th)",
 "Monk (15th)","Monk (16th)", /* 45th */
 "High Monk","Sensai","High Sensai","Enlightened","Nirvanic"},
     /* Dragons */
{"Egg","Hatchling-1","Hatchling-2","Hatchling-3","Hatchling-4","Hatchling-5",
 "Hatchling-6","Hatchling-7","Hatchling-8","Hatchling-9","Young-1","Young-2",
 "Young-3","Young-4","Young-5","Young-6","Young-7","Young-8","Young-9",
 "Middle-1","Middle-2","Middle-3","Middle-4","Middle-5","Middle-6","Middle-7",
 "Middle-8","Middle-9","Old-1","Old-2","Old-3","Old-4","Old-5","Old-6","Old-7",
 "Old-8","Old-9","Venerable-1","Venerable-2","Venerable-3","Venerable-4",
 "Venerable-5","Venerable-6","Venerable-7","Venerable-8","Venerable-9",
 "Minor Wyrm","Wyrm","Major Wyrm","Great Wyrm"}
};
#endif

/* Base experience levels, may be adjusted up for race and/or class*/
int32u player_exp[MAX_PLAYER_LEVEL] = {
      10,      25,	45,	 70,	  100,	    140,      200,	280,
     380,     500,     650,	850,	 1100,	   1400,     1800,     2300,
    2900,    3600,    4400,    5400,	 6800,	   8400,    10200,    12500,
   17500,   25000,  35000L,  50000L,   75000L,	100000L,  150000L,  200000L,
 275000L, 350000L, 450000L, 550000L, 700000L, 850000L, 1000000L, 1250000L,
1500000L, 1800000L, 2100000L, 2400000L, 2700000L, 3000000L, 3500000L, 4000000L,
4500000L, 5000000L
};

/*Race	STR,INT,WIS,DEX,CON,CHR,LUC
	Ages, heights, and weights (male then female)
	Racial Bases for: dis,srh,stl,fos,bth,bthb,bsav,hitdie,
	infra, exp base, choice-classes */
#ifdef MACGAME
race_type *race;
#else
race_type race[MAX_RACES] = {
   {"Human",	 0,  0,	 0,  0,	 0,  0,   5,
      14,  6, 72,  6,180, 25, 66,  4,150, 20,
      0,  0,  0,  0,  0,  0,  0, 10,  0, 100, 0xFF,
      0, 100, 5,  10 },
   {"Half-Elf", -1,  1,	 0,  1, -1,  1,   1,
      24, 16, 66,  6,130, 15, 62,  6,100, 10,
      2,  6,  1, -1, -1,  5,  3,  9,  2, 110, 0xFF,
      2, 105, 7, 15 },
   {"Elf",	-1,  2,	 1,  1, -2,  1,   2,
      75, 75, 60,  4,100,  6, 54,  4, 80,  6,
      5,  8,  1, -2, -5, 15,  6,  8,  3, 120, 0xDF,
      5, 110, 9, 20 },
   {"Hobbit", -2,    2,	-2,  3,	 1,  1,   2,
      21, 12, 36,  3, 60,  3, 33,  3, 50,  3,
      15, 12,  4, -5,-10, 20, 18,  7,  4, 110, 0xCB,
     10, 105, 15,30 },
   {"Gnome",	-1,  2,	 0,  2,	 1, -2,   3,
      50, 40, 42,  3, 90,  6, 39,  3, 75,  3,
      10,  6,  3, -3, -8,  8, 10,  9,  4, 120, 0x8F,
      7, 115, 8, 25 },
   {"Dwarf",	 2, -3,	 3, -2,	 2, -3,   0,
      35, 15, 48,  3,150, 10, 46,  3,120, 10,
      2,  7,  -1,  0, 15,  0,  9,  11,  5, 110, 0x85,
      0,  95,  2, 5 },
   {"Half-Orc",	 2, -1,	 1,  0,	 1, -4,  -1,
      11,  4, 66,  1,150,  5, 62,  1,120,  5,
      -3,  0, -1,  3, 12, -5, -3, 10,  3, 110, 0x8D,
      0,  90,  1, 3 },
   {"Half-Troll",4, -4,  0, -4,	 3, -6,  -2,
      20, 10, 96, 10,255, 50, 84,  8,225, 40,
      -5, -1, -2,  5, 20,-10, -8, 12,  3, 110, 0x85,
      0,  80,  0, 0 },
   {"Dunedain",  1,  2,  1,  2,  3,  2,   1,
      50, 20, 82, 5, 190, 20, 78,  6, 180, 15,
      4,   3,  2, -3, 15, 10,  5, 10,  0, 180, 0xFF,
      1, 105, 5, 15 },
   {"High-Elf",  1,  3, -1,  3,  1,  5,   3,
     100, 30, 90,10, 190, 20, 82, 10, 180, 15,
      4,   3,  3, -4, 15, 25, 20, 10,  4, 180, 0xDF,
      2, 110, 15, 25 }
 };
#endif

/* 5 char race for printing scores. */
char *dsp_race[MAX_RACES] = {
  "Human",
  "H-Elf",
  "Elf  ",
  "Hobbt",
  "Gnome",
  "Dwarf",
  "H-Orc",
  "H-Tro",
  "Duned",
  "HiElf"
};

/* Background information					*/
#ifdef MACGAME
background_type *background;
#else
background_type background[MAX_BACKGROUND] = {
{"You are the illegitimate and unacknowledged child ",		 10, 1, 2, 25},
{"You are the illegitimate but acknowledged child ",		 20, 1, 2, 35},
{"You are one of several children ",				 95, 1, 2, 45},
{"You are the first child ",					100, 1, 2, 50},
{"of a Serf.  ",						 40, 2, 3, 65},
{"of a Yeoman.  ",						 65, 2, 3, 80},
{"of a Townsman.  ",						 80, 2, 3, 90},
{"of a Guildsman.  ",						 90, 2, 3,105},
{"of a Landed Knight.  ",					 96, 2, 3,120},
{"of a Titled Noble.  ",					 99, 2, 3,130},
{"of a Royal Blood Line.  ",					100, 2, 3,140},
{"You are the black sheep of the family.  ",			 20, 3,50, 20},
{"You are a credit to the family.  ",				 80, 3,50, 55},
{"You are a well liked child.  ",				100, 3,50, 60},
{"Your mother was a Green-Elf.  ",				 40, 4, 1, 50},
{"Your father was a Green-Elf.  ",				 75, 4, 1, 55},
{"Your mother was a Grey-Elf.  ",				 90, 4, 1, 55},
{"Your father was a Grey-Elf.  ",				 95, 4, 1, 60},
{"Your mother was a High-Elf.  ",				 98, 4, 1, 65},
{"Your father was a High-Elf.  ",				100, 4, 1, 70},
{"You are one of several children ",				 60, 7, 8, 50},
{"You are the only child ",					100, 7, 8, 55},
{"of a Green-Elf ",						 75, 8, 9, 50},
{"of a Grey-Elf ",						 95, 8, 9, 55},
{"of a High-Elf ",						100, 8, 9, 60},
{"Ranger.  ",							 40, 9,54, 80},
{"Archer.  ",							 70, 9,54, 90},
{"Warrior.  ",							 87, 9,54,110},
{"Mage.  ",							 95, 9,54,125},
{"Prince.  ",							 99, 9,54,140},
{"King.  ",							100, 9,54,145},
{"You are one of several children of a Hobbit ",		 85,10,11, 45},
{"You are the only child of a Hobbit ",			        100,10,11, 55},
{"Bum.  ",							 20,11, 3, 55},
{"Tavern Owner.  ",						 30,11, 3, 80},
{"Miller.  ",							 40,11, 3, 90},
{"Home Owner.  ",						 50,11, 3,100},
{"Burglar.  ",							 80,11, 3,110},
{"Warrior.  ",							 95,11, 3,115},
{"Mage.  ",							 99,11, 3,125},
{"Clan Elder.  ",						100,11, 3,140},
{"You are one of several children of a Gnome ",			 85,13,14, 45},
{"You are the only child of a Gnome ",				100,13,14, 55},
{"Beggar.  ",							 20,14, 3, 55},
{"Braggart.  ",							 50,14, 3, 70},
{"Prankster.  ",						 75,14, 3, 85},
{"Warrior.  ",							 95,14, 3,100},
{"Mage.  ",							100,14, 3,125},
{"You are one of two children of a Dwarven ",			 25,16,17, 40},
{"You are the only child of a Dwarven ",			100,16,17, 50},
{"Thief.  ",							 10,17,18, 60},
{"Prison Guard.  ",						 25,17,18, 75},
{"Miner.  ",							 75,17,18, 90},
{"Warrior.  ",							 90,17,18,110},
{"Priest.  ",							 99,17,18,130},
{"King.  ",							100,17,18,150},
{"You are the black sheep of the family.  ",			 15,18,57, 10},
{"You are a credit to the family.  ",				 85,18,57, 50},
{"You are a well liked child.  ",				100,18,57, 55},
{"Your mother was an Orc, but it is unacknowledged.  ",		 25,19,20, 25},
{"Your father was an Orc, but it is unacknowledged.  ",		100,19,20, 25},
{"You are the adopted child ",					100,20, 2, 50},
{"Your mother was a Cave-Troll ",				 30,22,23, 20},
{"Your father was a Cave-Troll ",				 60,22,23, 25},
{"Your mother was a Hill-Troll ",				 75,22,23, 30},
{"Your father was a Hill-Troll ",				 90,22,23, 35},
{"Your mother was a Water-Troll ",				 95,22,23, 40},
{"Your father was a Water-Troll ",				100,22,23, 45},
{"Cook.  ",							  5,23,62, 60},
{"Warrior.  ",							 95,23,62, 55},
{"Shaman.  ",							 99,23,62, 65},
{"Clan Chief.  ",						100,23,62, 80},
{"You have dark brown eyes, ",					 20,50,51, 50},
{"You have brown eyes, ",					 60,50,51, 50},
{"You have hazel eyes, ",					 70,50,51, 50},
{"You have green eyes, ",					 80,50,51, 50},
{"You have blue eyes, ",					 90,50,51, 50},
{"You have blue-gray eyes, ",					100,50,51, 50},
{"straight ",							 70,51,52, 50},
{"wavy ",							 90,51,52, 50},
{"curly ",							100,51,52, 50},
{"black hair, ",						 30,52,53, 50},
{"brown hair, ",						 70,52,53, 50},
{"auburn hair, ",						 80,52,53, 50},
{"red hair, ",							 90,52,53, 50},
{"blond hair, ",						100,52,53, 50},
{"and a very dark complexion.",					 10,53, 0, 50},
{"and a dark complexion.",					 30,53, 0, 50},
{"and an average complexion.",					 80,53, 0, 50},
{"and a fair complexion.",					 90,53, 0, 50},
{"and a very fair complexion.",					100,53, 0, 50},
{"You have light grey eyes, ",					 85,54,55, 50},
{"You have light blue eyes, ",					 95,54,55, 50},
{"You have light green eyes, ",					100,54,55, 50},
{"straight ",							 75,55,56, 50},
{"wavy ",							100,55,56, 50},
{"black hair, and a fair complexion.",				 75,56, 0, 50},
{"brown hair, and a fair complexion.",				 85,56, 0, 50},
{"blond hair, and a fair complexion.",				 95,56, 0, 50},
{"silver hair, and a fair complexion.",				100,56, 0, 50},
{"You have dark brown eyes, ",					 99,57,58, 50},
{"You have glowing red eyes, ",					100,57,58, 60},
{"straight ",							 90,58,59, 50},
{"wavy ",							100,58,59, 50},
{"black hair, ",						 75,59,60, 50},
{"brown hair, ",						100,59,60, 50},
{"a one foot beard, ",						 25,60,61, 50},
{"a two foot beard, ",						 60,60,61, 51},
{"a three foot beard, ",					 90,60,61, 53},
{"a four foot beard, ",						100,60,61, 55},
{"and a dark complexion.",					100,61, 0, 50},
{"You have slime green eyes, ",					 60,62,63, 50},
{"You have puke yellow eyes, ",					 85,62,63, 50},
{"You have blue-bloodshot eyes, ",				 99,62,63, 50},
{"You have glowing red eyes, ",					100,62,63, 55},
{"dirty ",							 33,63,64, 50},
{"mangy ",							 66,63,64, 50},
{"oily ",							100,63,64, 50},
{"sea-weed green hair, ",					 33,64,65, 50},
{"bright red hair, ",						 66,64,65, 50},
{"dark purple hair, ",						100,64,65, 50},
{"and green ",							 25,65,66, 50},
{"and blue ",							 50,65,66, 50},
{"and white ",							 75,65,66, 50},
{"and black ",							100,65,66, 50},
{"ulcerous skin.",						 33,66, 0, 50},
{"scabby skin.",						 66,66, 0, 50},
{"leprous skin.",						100,66, 0, 50}
};
#endif

/* Classes.							*/
class_type class[MAX_CLASS] = {
/*	   HP Dis Src Stl Fos bth btb sve S  I  W  D Co Ch  L Spell Exp  spl
   Mag 2hit Dodge*/
{"Warrior",10, 25, 14, 1, 38, 80, 55, 18, 5,-2,-3, 2, 3,-1, 0, NONE,    0, 0,
  30,  60,  20},
{"Wizard",  0, 20, 10, 1, 15, 10,  0, 38,-3, 7,-2,-1,-2,-1, 1, MAGE,   20, 1,
 130,   0,   0},
{"Priest",  2, 25, 16, 2, 32, 48, 35, 34,-1,-4, 5,-1, 0, 2, 2, PRIEST, 20, 1,
 120,   5,   5},
{"Rogue",   6, 45, 50, 7, 10, 40, 66, 30, 2, 2,-2, 5, 1,-1, 1, MAGE,   15, 5,
  80,  80,  70},
{"Ranger",  4, 30, 24, 3, 24, 56, 72, 30, 2, 2, 0, 1, 1, 1, 0, MAGE,   30, 3,
  75,  30,  40},
{"Paladin", 7, 20, 12, 1, 38, 68, 40, 24, 3,-3, 2, 0, 2, 2, 1, PRIEST, 30, 1,
  80,  20,  10},
{"Monk",    2, 35, 40, 5, 16, 48, 20, 32, 2, 0, 0, 4, 0, 0, 0, MONK,   20, 1,
 100,   0,  90},
{"Dragon", 20, 25,  0, 0, 16, 70,  0, 38, 2, 3, 0,-6, 1,-7,-1, NONE,   80, 0,
   0,   0,   0}
};

/* making it 16 bits wastes a little space, but saves much signed/unsigned
   headaches in its use */
/* CLA_MISC_HIT is identical to CLA_SAVE, which takes advantage of
   the fact that the save values are independent of the class */
int16 class_level_adj[MAX_CLASS][MAX_LEV_ADJ] = {
/*	       bth    bthb   device  disarm   save   2hit  Dodging */
/* Warrior */ {	5,	4,	2,	2,	3,    3 ,   3},
/* Wizard  */ { 1,	1,	4,	2,	5,    1,    1},
/* Priest  */ { 2,	2,	4,	3,	4,    1,    2},
/* Rogue   */ { 3,	4,	3,	4,	3,    4,    3},
/* Ranger  */ { 3,	4,	3,	3,	3,    3,    3},
/* Paladin */ { 3,	2,	3,	2,	3,    2,    2},
/* Monk    */ { 3,      0,      3,      2,      3,    1,    4},
/* Dragon  */ { 3,      0,      1,      1,      3,    0,    1}
};

int32u spell_learned = 0;	/* bit mask of spells learned */
int32u spell_learned2 = 0;	/* bit mask of spells learned */
int32u spell_worked = 0;	/* bit mask of spells tried and worked */
int32u spell_worked2 = 0;	/* bit mask of spells tried and worked */
int32u spell_forgotten = 0;	/* bit mask of spells learned but forgotten */
int32u spell_forgotten2 = 0;	/* bit mask of spells learned but forgotten */
int8u spell_order[64];		/* order spells learned/remembered/forgotten */

/* Warriors don't have spells, so there is no entry for them.  Note that
   this means you must always subtract one from the py.misc.pclass before
   indexing into magic_spell[]. */
#ifdef MACGAME
spell_type (*magic_spell)[63];
#else
spell_type magic_spell[MAX_CLASS-1][63] = {
  {		  /* Wizard	   */
/* Beginners Magic */
     {	1,  1, 22,   1},
     {	1,  1, 23,   1},
     {	1,  2, 24,   2},
     {	1,  2, 26,   2},
     {  1,  2, 26,   2},
     {	1,  2, 25,   2},
     {  3,  2, 25,   2},
     {	3,  2, 25,   3},
     {	3,  2, 27,   3},
/* Conjuring and Tricks */
     {	3,  3, 30,   4},
     {	3,  3, 30,   6},
     {	3,  4, 30,   8},
     {	5,  4, 30,  10},
     {	5,  4, 35,  12},
     {	5,  5, 35,  14},
     {	5,  5, 30,  16},
     {	7,  5, 40,  18},
     {	7,  6, 44,  20},
/* Incantations and Illusions */
     {	7,  6, 45,  22},
     {	7,  6, 75,  24},
     {	9,  7, 45,  26},
     {  9,  7, 45,  28},
     {  9,  7, 75,  30},
     { 11,  8, 50,  32},
     { 11,  8, 50,  34},
     { 13,  9, 50,  36},
/* Sorcery and Evocations */
     { 15, 10, 55,  38},
     { 17, 11, 90,  44},
     { 19, 12, 60,  50},
     { 21, 14, 65,  63},
     { 23, 16, 65,  88},
     { 25, 18, 70, 125},
     { 27, 20, 75, 200},

     { 3,   7, 20,  50},
     { 5,  12, 40, 100},
     { 10, 17, 60, 110},
     { 15, 18, 70, 120},
     { 25, 25, 85, 120},

     {  7, 6,  50,  30},
     { 10, 9,  60,  50},
     { 15, 15, 70, 100},
     { 20, 25, 75, 200},
     { 25, 35, 85, 300},
     { 30, 45, 95,2000},

     { 2,  5,  50,  10},
     { 7, 10, 70,  100},
     { 12, 30, 95, 1000},
     { 17, 50, 70,  300},
     { 25, 50, 80, 1000},

     { 2, 5,  50,   100},
     { 2, 5,  50,   100},
     { 2, 5,  50,   100},
     { 4, 10, 75,   200},
     { 10, 20,  85, 1000},

     { 3, 5,  50,   100},
     { 7, 12, 75,  300},
     { 12, 20, 80,  800},
     { 18, 30, 50, 2000},
     { 35, 70, 75, 5000},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		  /* Priest	   */
     {	1,  1, 10,   1},
     {	1,  2, 15,   1},
     {	1,  2, 20,   1},
     {	1,  2, 25,   1},
     {	3,  2, 25,   1},
     {	3,  3, 27,   2},
     {	3,  3, 27,   2},
     {	3,  3, 28,   3},
     {	5,  4, 29,   4},
     {	5,  4, 30,   5},
     {	5,  4, 32,   5},
     {	5,  5, 34,   5},
     {	7,  5, 36,   6},
     {	7,  5, 38,   7},
     {	7,  6, 38,   9},
     {	7,  7, 38,   9},
     {	9,  6, 38,  10},
     {	9,  7, 38,  10},
     {	9,  7, 40,  10},
     { 11,  8, 42,  10},
     { 11,  8, 42,  12},
     { 11,  9, 55,  15},
     { 13, 10, 45,  15},
     { 13, 11, 45,  16},
     { 15, 12, 50,  20},
     { 15, 14, 50,  22},
     { 17, 14, 55,  32},
     { 19, 16, 60,  38},
     { 21, 20, 70,  75},
     { 25, 55, 90, 125},
     { 29, 32, 99, 200},

     { 3,  3,  50,   2},
     { 10, 10, 80,   50},
     { 15, 20, 80,   100},
     { 20, 10, 80,  1000},
     { 25, 50, 80,  2000},

     { 10,  5, 50,   100},
     { 13,  7, 60,   200},
     { 17, 50, 80,  1000},
     { 21, 70, 90,  2000},
     { 25, 70, 90,  3000},

     { 10, 7,  70,  100},
     { 15, 10, 75,  300},
     { 20, 25, 80,  1500},
     { 25, 35, 80,  1000},
     { 35, 60, 75,  4000},

     { 5, 6,  50,   50},
     { 10, 10, 80,  100},
     { 17, 20, 80,  1000},
     { 25, 30, 80,  2000},
     { 30, 40, 85,  3000},
     { 35, 50, 85,  6000},

     { 3, 3,  50,   5},
     { 10, 10,  50,  20},
     { 20, 20,  80,  80},
     { 30, 40,  75, 1000},
     { 35, 50,  75, 100},
     { 40, 60,  75, 3000},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		  /* Rogue	   */
     { 99, 99,	0,   0},
     {	5,  1, 30,   1},
     {	6,  2, 55,   1},
     {	7,  3, 40,   2},
     {  8,  3, 40,   2},
     {  9,  4, 45,   2},
     { 10,  4, 45,   3},
     { 11,  5, 50,   3},
     { 99, 99,	0,   0},

     { 12,  6, 55,   3},
     { 99, 99,	0,   0},
     { 13,  7, 60,   4},
     { 14,  8, 65,   5},
     { 15,  9, 70,   6},
     { 16,  9, 60,   7},
     { 17, 10, 75,   7},
     { 99, 99,	0,   0},
     { 18, 11, 80,  10},

     { 19, 12, 95,  11},
     { 20, 15, 99,  12},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 21, 18, 50,  19},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},

     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 32, 25, 70,  50},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},

     { 7,   7, 20,  50},
     { 9,  12, 40, 100},
     { 15, 17, 60, 110},
     { 30, 20, 70,   0},
     { 99, 35, 75, 120},

     { 13, 16,  50,  30},
     { 18, 20,  60,  50},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},

     { 5,  5,  50,  10},
     { 10, 10, 70,  100},
     { 35, 40, 95, 1000},
     { 99, 99,	0,   0},
     { 99, 99,	0,   0},

     { 10, 12,  50,   100},
     { 10, 12,  50,   100},
     { 10, 12,  50,   100},
     { 15, 20,  75,   200},
     { 25, 30,  85,  1000},

     { 10, 11,  50,   100},
     { 15, 20, 75,  300},
     { 20, 25, 80,  800},
     { 26, 30, 50, 2000},
     { 99, 99,  0,   0},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		   /* Ranger	    */
     {	3,  1, 30,   1},
     {	3,  2, 35,   2},
     {	3,  2, 35,   2},
     {	5,  3, 35,   2},
     { 99, 99,  0,   0},
     {	5,  3, 40,   2},
     { 99, 99,  0,   0},
     {	5,  4, 45,   3},
     {	7,  5, 40,   6},
     {	7,  6, 40,   5},
     {	9,  7, 40,   7},
     {	9,  8, 45,   8},
     { 11,  8, 40,  10},
     { 11,  9, 45,  10},
     { 13, 10, 45,  12},
     { 13, 11, 55,  13},
     { 15, 12, 50,  15},
     { 15, 13, 50,  15},
     { 17, 17, 55,  15},
     { 17, 17, 90,  17},
     { 21, 17, 55,  17},
     { 21, 19, 60,  18},
     { 23, 25, 90,  20},
     { 23, 20, 60,  20},
     { 25, 20, 60,  20},
     { 25, 21, 65,  20},
     { 27, 21, 65,  22},
     { 29, 23, 95,  23},
     { 31, 25, 70,  25},
     { 33, 25, 75,  38},
     { 35, 25, 80,  50},
     { 37, 30, 95, 100},
     { 99, 99,	0,   0},

     { 8,  17, 20,  50},
     { 19,  22, 40, 100},
     { 25, 27, 60, 110},
     { 30, 28, 60, 120},
     { 35, 35, 75, 120},

     { 20, 16,  50,  30},
     { 22, 19,  60,  50},
     { 30, 25, 70, 100},
     { 37, 35, 75, 200},
     { 35, 45, 85, 300},
     { 99, 99, 0,    0},

     { 10,  15,  50,  10},
     { 15, 20, 70,  100},
     { 35, 60, 95, 1000},
     { 99, 99, 0,    0},
     { 99, 99, 0,    0},

     { 8, 15,  50,   100},
     { 8, 15,  50,   100},
     { 8, 15,  50,   100},
     { 16, 25, 75,   200},
     { 25, 40,  85, 1000},

     { 10, 15,  50,   100},
     { 15, 20, 75,  300},
     { 25, 30, 80,  800},
     { 32, 50, 50, 2000},
     { 99, 99,  0,   0},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		  /* Paladin	   */
     {	1,  1, 30,   1},
     {	2,  2, 35,   2},
     {	3,  3, 35,   3},
     {	5,  3, 35,   5},
     {	5,  4, 35,   5},
     {	7,  5, 40,   6},
     {	7,  5, 40,   6},
     {	9,  7, 40,   7},
     {	9,  7, 40,   8},
     {	9,  8, 40,   8},
     { 11,  9, 40,  10},
     { 11, 10, 45,  10},
     { 11, 10, 45,  10},
     { 13, 10, 45,  12},
     { 13, 11, 45,  13},
     { 15, 13, 45,  15},
     { 15, 15, 50,  15},
     { 17, 15, 50,  17},
     { 17, 15, 50,  18},
     { 19, 15, 50,  19},
     { 19, 15, 50,  19},
     { 21, 17, 50,  20},
     { 23, 17, 50,  20},
     { 25, 20, 50,  20},
     { 27, 21, 50,  22},
     { 29, 22, 50,  24},
     { 31, 24, 60,  25},
     { 33, 28, 60,  31},
     { 35, 32, 70,  38},
     { 37, 70, 90,  50},
     { 39, 38, 95, 100},

     { 99,  5,  50,   2},
     { 99, 15, 80,   50},
     { 99, 25, 80,   100},
     { 99, 15, 80,  1000},
     { 99, 55, 80,  2000},

     { 99,  15, 50,   100},
     { 99,  25, 60,   200},
     { 99, 60, 80,  1000},
     { 99, 80, 90,  2000},
     { 99, 80, 90,  3000},

     { 99, 13,  70,  100},
     { 99, 20, 75,  300},
     { 99, 35, 80,  1500},
     { 99, 40, 80,  1000},
     { 99, 70, 75,  4000},

     { 99, 16,  50,   50},
     { 99, 30, 80,  100},
     { 99, 50, 80,  1000},
     { 40, 70, 80,  2000},
     { 42, 80, 85,  3000},
     { 47, 95, 85,  6000},

     { 7, 7,  50,   5},
     { 20, 20,  50,  20},
     { 25, 25,  80,  80},
     { 35, 50,  75, 1000},
     { 40, 60,  75, 100},
     { 45, 70,  75, 3000},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		  /* Monk	   */
     {  1,  1, 25,   1},
     {	1,  1, 27,   1},
     {  1,  1, 14,   1},
     {	1,  1, 31,   1},
     {	3,  1, 19,   1},
     {	3,  2, 35,   2},
     {	3,  1, 32,   2},
     {  3,  3, 29,   3},
     {  5,  3, 41,   3},
     {  5,  2, 38,   4},
     {	5,  4, 35,   5},
     {  7,  3, 32,   5},
     {  7,  3, 34,   6},
     {  7,  5, 51,   7},
     {  9,  6, 53,   8},
     {  9,  6, 55,   9},
     { 11,  7, 57,  10},
     { 11,  7, 59,  10},
     { 13,  7, 61,  10},
     { 13,  5, 48,  10},
     { 15,  5, 50,  12},
     { 17, 10, 67,  14},
     { 19, 11, 69,  16},
     { 21, 12, 71,  18},
     { 23, 13, 73,  20},
     { 25, 14, 75,  22},
     { 27, 10, 62,  26},
     { 29, 11, 65,  30},
     { 31, 17, 85,  34},
     { 33, 18, 90,  40},
     { 35, 19, 95,  60},
     { 37, 21, 95, 400},

     { 13,  3, 20,     2},
     { 20,  5, 30,    50},
     { 30,  3, 25,   100},
     { 35, 13, 50,  1000},
     { 45, 30, 60,  2000},

     { 20,  3, 30,   100},
     { 25,  5, 40,   200},
     { 30,  9, 50,  1000},
     { 35, 30, 60,  2000},
     { 40, 50, 70,  3000},

     { 25, 17, 40,   300},
     { 27, 19, 50,   500},
     { 29, 21, 60,   700},
     { 31, 23, 70,  1000},
     { 33, 25, 80,  1300},

     { 30, 15, 30,   100},
     { 32, 18, 40,   300},
     { 34, 25, 50,   800},
     { 36, 29, 60,  1200},
     { 38, 35, 70,  1800},

     { 33, 25, 40, 500},
     { 36, 28, 50, 700},
     { 39, 31, 60, 900},
     { 42, 35, 70,1300},
     { 45, 40, 80,1700}
   }

 };
#endif

char *spell_names[127+63] = {
  /* Mage Spells */
  "Magic Missile",  "Detect Monsters",	"Phase Door",  "Light Area",
  "Treasure Detection",
  "Cure Light Wounds",	  "Object Detection",
  "Find Hidden Traps/Doors",  "Stinking Cloud",
  "Confusion",	"Lightning Bolt",  "Trap/Door Destruction", "Sleep I",
  "Cure Poison",  "Teleport Self",  "Mana Bolt",  "Frost Bolt",
  "Turn Stone to Mud",	"Create Food",	"Recharge Item I",  "Sleep II",
  "Polymorph Other",  "Identify",  "Lightning Ball","Fire Bolt",
  "Slow Monster","Frost Ball",	 "Recharge Item II", "Teleport Other",
  "Haste Self","Fire Ball", "Word of Destruction", "Disrupt Other",

/* Mordenkainen's Escapes */
  "Door Creation",
  "Stair Creation",
  "Word of Recall",
  "Invisibility",
  "Mass Sleep",

/* Raal's Tome of Destruction */
  "Acid Bolt",
  "Cloud Kill",
  "Acid Ball",
  "Ice Storm",
  "Meteor Swarm",
  "Hellfire",

/*Kelek's Grimoire of Power*/
  "Detect Evil",
  "Detect Enchantment",
  "Recharge Item III",
  "Genocide",
  "Mass Genocide",

/* Resistance of Scarabtarices */
  "Resist Fire",
  "Resist Cold",
  "Resist Acid",
  "Resist Poison",
  "Resistance",

/* tenser's transformations...*/
  "Heroism",
  "Shield",
  "Berserker",
  "Essence of Speed",
  "Globe of Invulnerability",

  "blank",
  "blank",
  "blank",
  "blank",

  /* Priest Spells, start at index 31 now 63 ~Ludwig */

  "Detect Evil",  "Cure Light Wounds",	"Bless",  "Remove Fear", "Call Light",
  "Find Traps",	 "Detect Doors/Stairs",	 "Dispel Poison","Blind Creature",
  "Portal",  "Cure Medium Wounds",  "Chant",  "Repel Animals",  "Create Food",
  "Remove Curse",  "Resist Heat and Cold",  "Return Home",
  "Orb of Draining",  "Cure Serious Wounds",  "Master Invisiblity",
  "Protection from Evil",  "Earthquake",  "Sense Surroundings",
  "Cure Critical Wounds",  "Turn Undead",  "Demolish Dragons",  
   "Dispel Undead","Heal",  "Dispel Evil",  "Glyph of Warding",	"Holy Word",

/* Godly Insights... */
  "Detect Monsters",
  "Detection",
  "Perception",
  "Probing",
  "Clairvoyance",

/* Purifications and Healing */
  "Resistance",
  "Holy Shield",
  "Healing",
  "Restoration",
  "Remembrance",

/* Wrath of God */
  "Dispel Undead",
  "Dispel Evil",
  "Banishment",
  "Word of Destruction",
  "Annihilation",

/* Holy Infusions */
  "Unbarring Ways",
  "Recharging",
  "Dispel Curse",
  "Enchant Weapon",
  "Enchant Armour",
  "Elemental Brand",

/* Ethereal openings */
  "Blink",
  "Teleport",
  "Teleport Away",
  "Teleport Level",
  "Survive Death",
  "Alter Reality",

  "blank",
  "blank",
  "blank",
  "blank",
  "blank",

 /* Monk techniques, start at 126 */

 "Sense Creatures","Dispel Fear","Judo","Summon Light",
 "Boxing","Sense Doors/Traps","Wrestling","Confuse Opponent",
 "Escape","Karate","Heal Cuts","Savate",
 "Capeoira","Induce Sleep","Cure Wounds","Transport",
 "Return","Create Food","True Sight","Choi Li Fut",
 "Aikido","Flaming Touch","Wintry Touch","Glowing Touch",
 "Whirlwind","Regeneration","Thai Kick Boxing","Tae Kwon Do",
 "Invisibility","Protection","Detect Level","Mass Regeneration",

/* Ways of the Ninja */
 "Phase Wall", /* Does Stone-to-Mud 1 direction, and create walls in other */
 "Elemental Shield",
 "Ninjitsu",
 "Absolute Shield",
 "Death Touch",

/* Mental Unity */
 "Rememberance",
 "Internal Insight",
 "Restoration",
 "Mass Restoration",
 "Total Regeneration",

 /* Combat Mastery */
 "Distance Attack",
 "Hurricane",
 "Spiritual Blaze",
 "Mass Drain",
 "Disrupt Other",

 /* External Knowledge */
 "Sense Worldly Goods",
 "Sense Evil", 
 "Item Insight",
 "Sense Artifacts",
 "Probe Opponent",
   
 /* Opening Pathways */
 "Banish Other",
 "Create Stairs",
 "Create Doors",
 "Create Walls",
 "Escape Level"

};

#define MDO MAX_DUNGEON_OBJ
/* Each type of character starts out with a few provisions.	*/
/* Note that the entries refer to elements of the object_list[] array*/
/* 356 = Food Ration, 365 = Wooden Torch, 123 = Cloak, 30 = Stiletto,
   103 = Soft Leather Armor, 318 = Beginners-Magic, 322 = Beginners Handbook */
int16u player_init[MAX_CLASS][5] = {
		{ MDO, MDO+21,  34, 109, 258},	/* Warrior	 */
		{ MDO, MDO+21, 330, 331, 220},	/* Mage		 */
		{ MDO, MDO+21,  53, 334, 242},	/* Priest	 */
		{ MDO, MDO+21,  46, 103, 330},	/* Rogue	 */
		{ MDO, MDO+21,  34, 330,  74},	/* Ranger	 */
		{ MDO, MDO+21,  34, 334, 209},	/* Paladin	 */
                { MDO, MDO+21, 102, 123, 349},  /* Monk          */ 
                { MDO, MDO+21, MDO, 303, 375},  /* Dragon        */ 
        /* Last array object added for one extra useful object per class */
};
