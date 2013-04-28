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
      0,  0,  0,  0,  0,  0,  0, 10,  0, 100, 0,
      50, 100, 0,
      /* Advancement Speed, 1-9 */
     {7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7},
     /* Starting values (these are all max starting values) */
     {19,19,19,19,19,19,19,19,19,19,19,19,10,19,19,
         19,19,19,19,19,19,19,19,19,
        19,19,0,0,0,0}
 },
   {"Half-Elf", -1,  1,	 0,  1, -1,  1,   1,
      24, 16, 66,  6,130, 15, 62,  6,100, 10,
      2,  6,  1, -1, -1,  5,  3,  9,  2, 110, 0,
      45, 105, 0,
     {6,5,5,5,7,7,8,7,8,7,8,8,5,7,6,8,8,7,8,8,8,6,4,7,6,8,1,1,1,1},
      {70,30,50,30,70,70,80,60,60,40,70,70,20,30,60,
        70,80,50,20,20,20,40,20,70,
	 60,70,0,0,0,0}
 },
   {"Elf",	-1,  2,	 1,  2, -2,  2,   2,
      75, 75, 60,  4,100,  6, 54,  4, 80,  6,
      5,  8,  1, -2, -5, 30,  6,  8,  3, 120, 0,
      40, 110, 0,
      {4,3,5,3,8,8,8,8,8,7,9,8,4,7,4,7,9,7,7,8,8,7,2,7,4,9,1,1,1,1},
      {110,20,70,20,70,80,100,70,50,30,90,90,20,80,80,
	 80,120,70,20,20,20,60,20,90,
	 40,90,0,0,0,0}
  },
   {"Hobbit", -2,    2,	-3,  4,	 1,  1,   2,
      21, 12, 36,  3, 60,  3, 33,  3, 50,  3,
      15, 12,  4, -5,-10, 15, 30,  7,  4, 110,  0,
     35, 105, 0,
      {3,3,8,8,10,4,6,10,10,6,10,7,6,10,10,6,6,10,7,7,7,8,4,10,8,5,1,1,1,1},
      {10,10,60,50,120,30,30,60,60,50,140,30,20,80,120,
	 30,30,100,20,50,30,50,20,120,
	 80,50,0,0,0,0}
    },
   {"Gnome",	-1,  2,	 0,  2,	 1, -2,   4,
      50, 40, 42,  3, 90,  6, 39,  3, 75,  3,
      10,  6,  3, -3, -8,  0, 10,  9,  4, 120,  0,
     40, 115, 0,
      {3,3,7,3,7,7,7,8,7,7,7,7,3,8,7,3,3,7,8,8,8,7,3,7,9,7,1,1,1,1},
      {50,50,80,50,110,90,80,70,80,80,120,80,40,90,90,
	 50,50,90,30,40,30,60,30,100,
	 90,60,0,0,0,0}
    },
   {"Dwarf",	 2, -3,	 4, -2,	 2, -3,   0,
      35, 15, 48,  3,150, 10, 46,  3,120, 10,
      2,  7,  -1,  0, 15,  0,  9,  11,  5, 110, 0,
      35,  95, 0,
      {8,8,6,8,6,6,7,7,4,4,4,7,4,4,4,10,8,4,9,9,9,8,9,4,3,6,1,1,1,1},
      {80,80,40,80,40,50,40,70,30,40,20,50,10,20,20,
	 130,90,30,50,50,60,30,80,30,
	 20,30,0,0,0,0}
    },
   {"Half-Orc",	 2, -1,	 1,  0,	 1, -4,  -1,
      11,  4, 66,  1,150,  5, 62,  1,120,  5,
      -3,  0, -1,  3, 12, -5, -3, 12,  3, 110,  0,
     35,  90, 0,
      {10,8,8,9,4,4,4,4,3,3,3,4,3,3,4,8,8,3,4,7,7,7,6,7,4,4,4,1,1,1},
      {150,120,20,110,20,50,30,40,20,40,20,30,10,20,20,
	 70,70,20,50,50,50,20,120,20,
	 30,20,0,0,0,0}
    },
   {"Half-Troll",4, -2,  0, -1,	 3, -6,  -2,
      20, 10, 96, 10,255, 50, 84,  8,225, 40,
      -5, -1, -2,  5, 20,-10, -8, 13,  3, 110,  0,
     30,  80, 0,
      {9,10,4,10,4,4,4,6,4,4,4,6,4,4,4,6,6,4,4,9,8,8,8,8,7,4,3,1,1,1},
      {130,170,20,160,20,40,30,20,20,60,10,20,10,20,10,
	 50,50,10,50,40,40,10,160,10,
	 20,10,0,0,0,0}
    },
   {"Dunedain",  1,  2,  1,  2,  3,  2,   1,
      50, 20, 82, 5, 190, 20, 78,  6, 180, 15,
      4,   3,  2, -3, 15, 10,  5, 10,  0, 180,  0,
     70, 105, 0,
      {4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4},
      {120,120,120,120,120,120,120,120,120,120,120,120,50,120,120,
	 120,120,120,120,120,120,120,120,120,
	 120,120,0,0,0,0}
  },
   {"High-Elf",  1,  3, -1,  3,  1,  4,   3,
     100, 30, 90,10, 190, 20, 82, 10, 180, 15,
      4,   3,  3, -4, 20, 25, 20, 9,  4, 180,  0,
     70, 110, 0,
      {5,4,4,4,5,5,5,5,4,4,4,4,5,5,4,5,5,4,4,7,7,6,3,3,5,5,5,1,1,1},
      {130,120,130,110,140,130,130,130,130,120,120,130,60,140,140,
	 170,170,120,120,120,120,120,100,140,
	 120,170,0,0,0,0}
  },
   {"Dark Elf", -1,  4,  0,  2, -3, -2,  -1,
      80,150, 40,10,  90, 20, 35, 15,  70, 10,
      10, 10,  3, -3, 60, 10, 12,  8,  6, 110,  3,
      45,180, 0,
      {8,2,6,3,8,9,10,10,6,8,8,10,10,6,3,2,2,9,2,8,8,8,2,4,7,10,1,1,1,1},
      {110,20,40,10,90,150,130,120,70,80,70,140,150,60,90,
	 30,30,80,10,40,20,30,10,80,
	 30,130,0,0,0,0}
    },
   {"Giant",     2, -1,  2, -3,	 4, -2,   1,
      20, 10,120, 70,255, 50,114, 12,255, 40,
      -5, -1, -2,  5, 20,-10, -8, 15,  2, 110,  0,
     30,  40, 0,
      {7,10,3,10,3,6,8,6,3,3,3,8,9,3,3,6,6,3,3,10,8,8,10,4,3,5,1,1,1,1},
      {70,180,30,170,10,90,70,20,10,10,50,70,100,10,10,
	 50,50,20,50,50,50,20,200,20,
	 10,50,0,0,0,0}
    }
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
  "HiElf",
  "DkElf",
  "Giant"
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

int32u spell_learned = 0;	/* bit mask of spells learned */
int32u spell_learned2 = 0;	/* bit mask of spells learned */
int32u spell_worked = 0;	/* bit mask of spells tried and worked */
int32u spell_worked2 = 0;	/* bit mask of spells tried and worked */
int32u spell_forgotten = 0;	/* bit mask of spells learned but forgotten */
int32u spell_forgotten2 = 0;	/* bit mask of spells learned but forgotten */
int8u spell_order[64];		/* order spells learned/remembered/forgotten */

#ifdef MACGAME
spell_type (*magic_spell)[63];
#else
spell_type magic_spell[NECROS+1][63] = {
  {		  /* Wizard	   */
/* Beginners Magic */
     {	1,  1, 22,   1},
     {	1,  1, 23,   1},
     {	1,  2, 24,   1},
     {	3,  2, 26,   1},
     {  3,  2, 27,   1},
     {  3,  2, 25,   2},
     {  3,  2, 30,   2},
     {	3,  2, 25,   2},
     {	3,  1, 27,   2},
/* Conjuring and Tricks */
     {	5,  3, 30,   3},
     {	5,  3, 30,   3},
     {	9,  5, 70,   4},
     {	9,  4, 30,   5},
     { 11,  4, 35,   6},
     { 11,  5, 35,   7},
     { 13, 10, 70,   8},
     { 11,  4, 35,   9},
     { 15,  6, 44,  12},
/* Incantations and Illusions */
     { 17,  6, 45,  14},
     { 19,  6, 75,  16},
     { 21,  7, 45,  18},
     { 23,  7, 45,  20},
     { 25,  7, 75,  22},
     { 21,  6, 40,  20},
     { 23,  7, 45,  22},
     { 31,  9, 50,  28},
/* Sorcery and Evocations */
     { 33, 10, 55,  30},
     { 35, 11, 90,  32},
     { 37, 12, 60,  34},
     { 39, 14, 65,  36},
     { 41, 16, 65,  38},
     { 43, 18, 70,  50},
     { 45, 20, 85,  55},

     { 20,  7, 20,  50},
     { 25, 12, 40, 100},
     { 30, 17, 60, 110},

     { 35, 25, 50, 120},
     { 37, 27, 55, 120},
     { 39, 29, 60,  30},
     { 41, 31, 65,  50},
     { 43, 33, 70, 100},
     { 45, 35, 75, 200},
     { 47, 37, 80, 300},
     { 49, 50, 85,2000},

     { 15, 5,  50,  10},
     { 20, 10, 70,  100},
     { 25, 30, 95, 1000},
     { 30, 50, 70,  300},
     { 35, 50, 80, 1000},

     { 15,5,  50,   100},
     { 15,5,  50,   100},
     { 15,5,  50,   100},
     { 15,10, 75,   200},
     { 25, 20,  85, 1000},

     {25, 5,  50,   100},
     {30, 12, 75,  300},
     {35, 20, 80,  800},
     {40, 30, 50, 2000},
     {45, 70, 75, 5000},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		  /* Priest	   */
     {	1,  1, 10,   1},
     {	1,  2, 15,   1},
     {	1,  2, 20,   1},
     {	3,  2, 25,   1},
     {	3,  2, 25,   1},
     {	3,  3, 27,   2},
     {	3,  3, 27,   2},
     {	5,  3, 28,   3},
     {	5,  4, 29,   4},
     {	5,  4, 30,   5},
     {	7,  4, 32,   5},
     {	7,  5, 34,   5},
     {	7,  5, 36,   6},
     {	9,  5, 38,   7},
     {	9,  6, 38,   9},
     { 11,  7, 38,   9},
     { 11,  6, 38,  10},
     { 13,  9, 48,  10},
     { 13,  7, 40,  10},
     { 15,  8, 42,  10},
     { 17,  8, 42,  12},
     { 19,  9, 55,  15},
     { 21, 10, 45,  15},
     { 23, 11, 45,  16},
     { 25, 12, 50,  20},
     { 27, 14, 50,  22},
     { 29, 14, 55,  32},
     { 31, 16, 60,  38},
     { 33, 20, 70,  75},
     { 39, 55, 90, 125},
     { 45, 32, 85, 200},

     { 30, 10, 50,   2},
     { 33, 20, 60,   50},
     { 36, 40, 70,   100},
     { 43, 50, 80,  1000},
     { 45, 70, 90,  2000},

     { 35,  5, 50,   100},
     { 38,  7, 60,   200},
     { 41, 50, 70,  1000},
     { 43, 60, 80,  2000},
     { 45, 70, 90,  3000},

     { 35, 25, 60,  100},
     { 38, 28, 65,  300},
     { 41, 31, 70,  1500},
     { 43, 33, 75,  1000},
     { 45, 45, 80,  4000},

     { 31, 6,  50,   50},
     { 33, 10, 55,  100},
     { 35, 20, 60,  1000},
     { 37, 30, 65,  2000},
     { 40, 40, 70,  3000},
     { 43, 50, 75,  6000},

     { 30,  3, 50,   5},
     { 33, 10, 50,  20},
     { 36, 20, 80,  80},
     { 39, 40, 75, 1000},
     { 42, 50, 75, 100},
     { 45, 60, 75, 3000},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   },
   {		  /* Druid	   */
     {  1,  1, 15,   1},
     {	1,  1, 17,   1},
     {  1,  1, 19,   1},
     {	1,  2, 21,   1},
     {	3,  2, 23,   1},
     {	3,  2, 25,   2},
     {	3,  3, 27,   2},
     {  3,  3, 29,   3},
     {  5,  3, 31,   3},
     {  5,  4, 33,   4},
     {	7,  4, 35,   5},
     {  7,  5, 37,   5},
     {  9,  5, 39,   6},
     {  9,  5, 41,   7},
     { 11,  6, 43,   8},
     { 11,  6, 45,   9},
     { 13,  7, 47,  10},
     { 13,  7, 49,  10},
     { 15,  7, 51,  10},
     { 15,  8, 53,  10},
     { 17,  9, 55,  12},
     { 19, 10, 57,  14},
     { 21, 11, 59,  16},
     { 23, 12, 61,  18},
     { 25, 13, 63,  20},
     { 27, 14, 65,  22},
     { 29, 15, 67,  26},
     { 31, 16, 69,  30},
     { 35, 17, 71,  34},
     { 37, 18, 73,  40},
     { 39, 19, 75,  60},
     { 33, 20, 77, 400},

     { 35,  3, 20,     2},
     { 38,  5, 30,    50},
     { 41,  3, 25,   100},
     { 44, 13, 50,  1000},
     { 47, 30, 60,  2000},

     { 30,  3, 30,   100},
     { 33,  5, 40,   200},
     { 36,  9, 50,  1000},
     { 39, 30, 60,  2000},
     { 42, 50, 70,  3000},

     { 35, 17, 40,   300},
     { 38, 19, 50,   500},
     { 41, 21, 60,   700},
     { 44, 23, 70,  1000},
     { 47, 25, 80,  1300},

     { 30, 15, 30,   100},
     { 32, 18, 40,   300},
     { 34, 25, 50,   800},
     { 36, 29, 60,  1200},
     { 38, 35, 70,  1800},

     { 36, 25, 40, 500},
     { 38, 28, 50, 700},
     { 40, 31, 60, 900},
     { 42, 35, 70,1300},
     { 44, 40, 80,1700}
   },
   {	     /* Necros */
     {	1,  1, 25,   1},
     {	1,  1, 27,   1},
     {	1,  1, 29,   1},
     {	1,  2, 31,   1},
     {  3,  2, 33,   2},
     {	3,  2, 35,   2},
     {  3,  3, 37,   2},
     {	3,  3, 39,   2},
     {	5,  4, 41,   2},
     {	5,  4, 43,   3},
     {	7,  5, 45,   3},
     {	7,  6, 47,   4},
     {	9,  6, 49,   5},
     {	9,  7, 51,   6},
     { 11,  8, 53,   7},
     { 11,  9, 55,   2},
     { 13, 10, 57,  10},
     { 13, 11, 59,  12},
     { 15, 12, 61,  14},
     { 17, 13, 63,  16},
     { 19, 14, 65,  18},
     { 21, 15, 67,  20},
     { 23, 16, 69,  22},
     { 25, 17, 71,  24},
     { 27, 19, 73,  26},
     { 29, 21, 75,  28},
     { 31, 23, 77,  30},
     { 33, 25, 79,  32},
     { 35, 27, 81,  34},
     { 37, 29, 83,  36},
     { 45, 31, 85,  38},

 /* Mastery of the Undead */
     { 38, 20, 60, 350},
     { 41, 25, 65, 455},
     { 44, 30, 70, 650},
     { 47, 35, 75,1000},

 /* Protection from Undead */
     { 40, 40, 70, 410},
     { 42, 45, 75, 620},
     { 44, 50, 80,1120},
     { 46, 55, 85,1530},

 /* Mastery of Life Force */
     { 38, 20, 60, 300},
     { 40, 25, 70, 500},
     { 42, 30, 75, 700},
     { 44, 35, 85,1200},
     { 46, 40, 95,1800},

 /* Bodily Infusions */
     { 30, 30, 60,  600},
     { 35, 35, 65,  900},
     { 40, 40, 70, 1500},
     { 45, 45, 75, 2500},

 /* Unholy Perceptions */
     { 25,30,  60,   100},
     { 35,35,  70,   300},
     { 40,40,  80,   900},
     { 45,45,  90,  1500},


     { 35, 10, 75,   200},
     { 40, 20,  85, 1000},

     { 15, 5,  50,  100},
     { 20, 12, 75,  300},
     { 25, 20, 80,  800},
     { 30, 30, 50, 2000},
     { 40, 40, 75, 5000},

     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0},
     { 99, 99,  0,   0}
   }
 };
#endif

char *spell_names[186+63] = {
  /* Mage Spells */
  "Magic Missile",  "Detect Monsters",	"Phase Door",  "Light Area",
  "Treasure/Object Detection",
  "Find Hidden Traps/Doors","Fetch Object",
  "Reinforce Door",  "Stinking Cloud",
  "Induce Terror",	"Lightning Bolt",  "Magic Storm", "Sleep I",
  "Cure Poison",  "Teleport Self",  "Mana Bolt",  "Frost Bolt",
  "Turn Stone to Mud",	"Sustinence",	"Recharge Item I",  "Sleep II",
  "Polymorph Other",  "Identify",  "Lightning Ball","Fire Bolt",
  "Slow Monster","Frost Ball",	 "Recharge Item II", "Teleport Other",
  "Haste Self","Fire Ball", "Word of Destruction", "Regain Mana",

/* Mordenkainen's Escapes */
  "Door Creation",
  "Stair Creation",
  "Word of Recall",

/* Raal's Tome of Destruction */
  "Sonic Storm",
  "Acid Ball",
  "Plasma Vortex",
  "Mana Storm",
  "Chaos Vortex",
  "Charge Floor",
  "Creeping Walls",
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

  "Detect Evil",  "Cure Light Wounds",	"Bless",  "Remove Fear", "Radiance",
  "Find Doors/Traps","Spiritual Hammer","Dispel Poison","Induce Peace",
  "Portal",  "Cure Medium Wounds",  "Chant",  "Banish Animals",  "Sustinence",
  "Remove Curse",  "Resist Heat and Cold",  "Return Home",
  "Orb of Draining",  "Cure Serious Wounds",  "True Invisibility",
  "Protection from Evil",  "Earthquake",  "Sense Surroundings",
  "Cure Critical Wounds",  "Turn Undead",  "Banish Evil",  
   "Dispel Undead","Heal",  "Dispel Evil",  "Glyph of Warding",	"Holy Word",

/* Godly Insights... */
  "Detection",
  "Perception",
  "Probing",
  "Clairvoyance",
  "Self-Knowledge",

/* Purifications and Healing */
  "Resistance",
  "Holy Shield",
  "Restoration",
  "Remembrance",
  "Damage Immunity",

/* Wrath of God */
  "Stun Bolt",
  "Dispel Evil",
  "Annihilate Evil",
  "Word of Destruction",
  "Annihilation",

/* Holy Infusions */
  "Recharging",
  "Dispel Curse",
  "Battle Speed",
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

 /* Druid techniques, start at 126 */

 "Sense Life","Sense Weather","Blessing","Remove Fear","Call Sunlight",
 "Find Traps/Doors","Cure Light Wounds","Dispel Poison","Return Home",
 "Induce Peace","Cure Medium Wounds","Banish Animals","Phase Rock",
 "Resist Elements","Cure Serious Wounds","Sustain Self","Purtid Vapors",
 "Chain Lightning","Sense Area","Icy Spear","Major Cure","Flaming Stone",
 "Gust of Wind","Blizzard","Heat Wave","Thunderstorm","Sickening Mists",
 "Form of the Sheep","Form of the Goat","Total Healing","Super Speed",
 "Regain Harmony", /* 32 spells there */

/* Shapeshifting */
  "Form of the Bear",
  "Form of the Lion",
  "Form of the Gazelle",
  "Form of the Cheetah",
  "Form of the Dragon",

/* Dungeon Insights */
  "Understand Item",
  "Find Material Goods",
  "Probing",
  "Magical Staircase",
  "View Dungeon",

 /* Primal Forces */
  "Battle Blessing",
  "Iron Will",
  "Fighting Fury",
  "Mystic Shield",
  "Mass Sleep",

 /* Elemental Mastery */
  "Essence of Flame",
  "Absolute Zero",
  "Lethal Plague",
  "Hurricane",
  "Star Core",
   
 /* Weather Control */
  "Arid Wastes",
  "Torrential Downpour",
  "Wind Songs",
  "Calm Winds",
  "Call Frost",
  "Scorch Earth",

  /* Necros chants, start at 184 */
 "Sense Undead","Blink","Undo Curse","Brighten Room","Confuse",
 "Find Doors/Traps","Slow Poison","Sleep Other","Destroy Undead","Find Food",
 "Block Heat/Cold","Slow Undead","Iron Will","Shift Position","Return",
 "Detect Life","True Sight","Disrupt Life","Remove Wounds","Block Undead",
 "Insight","Drain Life","Weaken Others","Repel Other","Curse Weapon",
 "Battle Power","Poison Shield","Mystic Barrier","Slay Living","Infuse Weapon",
 "Destroy Cavern",

 /* Mastery of the Undead */
 "Banish Non-Living",
 "Annihilate Undead",
 "Destroy Life",
 "Summon Holy Spirits",

 /* Protection from Undead */
 "Partial Restoration",
 "Rememberance",
 "Restoration",
 "Retake life",

 /* Mastery of Life Force */
 "Regain Mana",
 "Annihilate Animals",
 "Vampiric Drain",
 "Genocide",
 "Total Genocide",

 /* Bodily Infusions */
 "Regeneration",
 "Fighting Rage",
 "Lich Form",
 "Barrier",

 /* Unholy Perceptions */
 "Detection",
 "Probing",
 "Sense Room",
 "Detect Dungeon"
};
