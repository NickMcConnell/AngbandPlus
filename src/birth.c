/* File: birth.c */

/* Purpose: create a player character */

/*
 * Copyright (c) 2000 James E. Wilson, Robert A. Koeneke, Eric Bock
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


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

	byte stat[6];

	char history[4][60];
};


/*
 * The last character displayed
 */
static birther prev;

/*
 * Current stats
 */
static byte stat_use[6];

/*
 * Autoroll score
 */
static s16b stat_score[6];

/*
 * Social class score
 */
static s16b sc_score;


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
	s16b bonus;			    /* Social Class Bonus */
};


/*
 * Background information (see below)
 */
static hist_type hist_human[] =
{
	{"You are the illegitimate and unacknowledged child",		10, 1, 2, -25},
	{"You are the illegitimate but acknowledged child",     	20, 1, 2, -15},
	{"You are one of several children",                     	95, 1, 2, -5},
	{"You are the first child",                             100, 1, 2, 0},

	{"of a Serf.",                                         	40, 2, 3, 15},
	{"of a Yeoman.",					 									65, 2, 3, 30},
	{"of a Townsman.",					 								80, 2, 3, 40},
	{"of a Guildsman.",					 								90, 2, 3, 55},
	{"of a Landed Knight.",				 								96, 2, 3, 70},
	{"of a Royal Family.",          									98, 2, 3, 80},
	{"of a Noble Family in the Courts of Chaos.",          	99, 2, 3, 90},
	{"of the Royal Blood Line of Amber.",                   100, 2, 3, 100},

	{"You are the black sheep of the family.",             	20, 3, 4, -30},
	{"You are a credit to the family.",                    	80, 3, 4, 5},
	{"You are a well liked child.",                         100, 3, 4, 10},

	{"You have dark brown eyes,",				 						20, 4, 5, 0},
	{"You have brown eyes,",					 						60, 4, 5, 0},
	{"You have hazel eyes,",					 						70, 4, 5, 0},
	{"You have green eyes,",					 						80, 4, 5, 0},
	{"You have blue eyes,",					 							90, 4, 5, 0},
	{"You have blue-gray eyes,",									  100, 4, 5, 0},

	{"straight",						 									70, 5, 6, 0},
	{"wavy",							 										90, 5, 6, 0},
	{"curly",															  100, 5, 6, 0},

	{"black hair,",						 								30, 6, 7, 0},
	{"brown hair,",						 								70, 6, 7, 0},
	{"auburn hair,",						 								80, 6, 7, 0},
	{"red hair,",						 									90, 6, 7, 0},
	{"blond hair,",													  100, 6, 7, 0},

	{"and a very dark complexion.",									10, 7, 0, 0},
	{"and a dark complexion.",					 						30, 7, 0, 0},
	{"and an average complexion.",				 					80, 7, 0, 0},
	{"and a fair complexion.",					 						90, 7, 0, 0},
	{"and a very fair complexion.",								  100, 7, 0, 0},
};

static hist_type hist_half_elf[] =
{
	{"Your mother",														50, 1, 2, 0},
	{"Your father",													  100, 1, 2, 10},

	{"was of the Nandor.",			 									20, 2, 3, 5},
	{"was of the Noldor.",		 	 									70, 2, 3, 10},
	{"was of the Sindar.",											  100, 2, 3, 20},

	{"You have",		                                      100, 4, 5, 0},

	{"dark brown eyes,",							 						 7, 4, 5, 0},
	{"brown eyes,",								 						20, 4, 5, 0},
	{"hazel eyes,",					 									23, 4, 5, 0},
	{"green eyes,",					 									27, 4, 5, 0},
	{"blue eyes,",					 										30, 4, 5, 0},
	{"blue-gray eyes,",												   33, 4, 5, 0},
	{"light grey eyes,",							 						90, 5, 6, 10},
	{"light blue eyes,",		 											96, 5, 6, 10},
	{"light green eyes,",											  100, 5, 6, 10},

	{"curly",															   33, 5, 6, -5},
	{"straight",						 									83, 6, 7, 0},
	{"wavy",																  100, 6, 7, 0},

	{"red hair,",						 									11, 6, 7, -20},
	{"auburn hair,",						 								33, 6, 7, -10},
	{"black hair,",										 				83, 7, 8, 0},
	{"brown hair,",			 											90, 7, 8, 0},
	{"blond hair,",										 				97, 7, 8, 0},
	{"silver hair,",													  100, 7, 8, 0},

	{"and a very dark complexion.",									 7, 7, 0, 0},
	{"and a dark complexion.",					 						20, 7, 0, 0},
	{"and an average complexion.",				 					53, 7, 0, 0},
	{"and a fair complexion.",					 						93, 7, 0, 15},
	{"and a very fair complexion.",								  100, 7, 0, 20},
};

static hist_type hist_elf[] =
{
	{"You are one of several children",			 					40, 1, 2, 0},
	{"You are the only child",										  100, 1, 2, 5},

	{"of a Nando",						 									20, 2, 3, -5},
	{"of a Noldo",														   70, 2, 3, 0},
	{"of a Sindar",													  100, 2, 3, 10},

	{"Ranger.",						 										40, 3, 4, 30},
	{"Archer.",						 										70, 3, 4, 40},
	{"Warrior.",						 									87, 3, 4, 60},
	{"Prince.",						 										99, 3, 4, 90},
	{"King.",															  100, 3, 4, 95},

	{"You have light",                                      100, 4, 5, 0},

	{"grey eyes,",									 						85, 5, 6, 0},
	{"blue eyes,",				 											95, 5, 6, 0},
	{"green eyes,",													  100, 5, 6, 0},

	{"straight",						 									75, 6, 7, 0},
	{"wavy",																  100, 6, 7, 0},

	{"black hair,",										 				75, 7, 8, 0},
	{"grey hair,",				 											85, 7, 8, 5},
	{"blond hair,",										 				95, 7, 8, 0},
	{"silver hair,",													  100, 7, 8, 10},

	{"and a fair complexion.",                              100, 8, 0, 10},
};

static hist_type hist_high_elf[] =
{
	{"You are one of several children",			 					40, 1, 2, 0},
	{"You are the only child",										   95, 1, 2, 5},
	{"You are a child of",                                  100, 1, 3, 30},

	{"of a Noldo.",                                          20, 2, 4, 30},
	{"of a Teleri.",					 									70, 2, 5, 70},
	{"of a Vanya.",													  100, 2, 7, 100},

	{"Finarfin.",                                            25, 3, 7, 60},
	{"Elwe.",                                                50, 3, 5, 80},
	{"Olwe.",                                                75, 3, 6, 110},
	{"Ingwe.",                                              100, 3, 7, 150},

	{"You have black hair,",                                100, 4, 8, 0},

	{"You have silver hair,",                               100, 5, 8, 0},

	{"You have white hair,",                                100, 6, 8, 0},

	{"You have golden hair,",                               100, 7, 8, 0},

	{"grey eyes,",									 						85, 8, 9, 0},
	{"blue eyes,",				 											95, 8, 9, 0},
	{"green eyes,",													  100, 8, 9, 0},

	{"and a glowing complexion.",                           100, 9, 0, 20},
};

static hist_type hist_hobbit[] =
{
	{"You are one of several children of a Hobbit",		 		85, 1, 2, -5},
	{"You are the only child of a Hobbit",		        		  100, 1, 2, 5},

	{"Bum.",							 										20, 2, 3, 5},
	{"Tavern Owner.",						 								30, 2, 3, 30},
	{"Miller.",						 										40, 2, 3, 40},
	{"Home Owner.",						 								50, 2, 3, 50},
	{"Burglar.",						 									80, 2, 3, 60},
	{"Warrior.",						 									95, 2, 3, 65},
	{"Clan Elder.",													  100, 2, 3, 90},

	{"You are the black sheep of the family.",             	20, 3, 4, -30},
	{"You are a credit to the family.",                    	80, 3, 4, 5},
	{"You are a well liked child.",                         100, 3, 4, 10},

	{"You have dark brown eyes,",				 						20, 4, 5, 0},
	{"You have brown eyes,",					 						60, 4, 5, 0},
	{"You have hazel eyes,",					 						70, 4, 5, 0},
	{"You have green eyes,",					 						80, 4, 5, 0},
	{"You have blue eyes,",					 							90, 4, 5, 0},
	{"You have blue-gray eyes,",									  100, 4, 5, 0},

	{"straight",						 									70, 5, 6, 0},
	{"wavy",							 										90, 5, 6, 0},
	{"curly",															  100, 5, 6, 0},

	{"black hair,",						 								30, 6, 7, 0},
	{"brown hair,",						 								70, 6, 7, 0},
	{"auburn hair,",						 								80, 6, 7, 0},
	{"red hair,",						 									90, 6, 7, 0},
	{"blond hair,",													  100, 6, 7, 0},

	{"and a very dark complexion.",									10, 7, 0, 0},
	{"and a dark complexion.",					 						30, 7, 0, 0},
	{"and an average complexion.",				 					80, 7, 0, 0},
	{"and a fair complexion.",					 						90, 7, 0, 0},
	{"and a very fair complexion.",								  100, 7, 0, 0},
};

static hist_type hist_gnome[] =
{
	{"You are one of several children of a Gnome",		 		85, 1, 2, -5},
	{"You are the only child of a Gnome",						  100, 1, 2, 5},

	{"Beggar.",						 										20, 2, 3, 5},
	{"Braggart.",						 									50, 2, 3, 20},
	{"Prankster.",						 									75, 2, 3, 35},
	{"Warrior.",						 									95, 2, 3, 50},
	{"Mage.",															  100, 2, 3, 75},

	{"You are the black sheep of the family.",             	20, 3, 4, -30},
	{"You are a credit to the family.",                    	80, 3, 4, 5},
	{"You are a well liked child.",                         100, 3, 4, 10},

	{"You have dark brown eyes,",				 						20, 4, 5, 0},
	{"You have brown eyes,",					 						60, 4, 5, 0},
	{"You have hazel eyes,",					 						70, 4, 5, 0},
	{"You have green eyes,",					 						80, 4, 5, 0},
	{"You have blue eyes,",					 							90, 4, 5, 0},
	{"You have blue-gray eyes,",									  100, 4, 5, 0},

	{"straight",						 									70, 5, 6, 0},
	{"wavy",							 										90, 5, 6, 0},
	{"curly",															  100, 5, 6, 0},

	{"black hair,",						 								30, 6, 7, 0},
	{"brown hair,",						 								70, 6, 7, 0},
	{"auburn hair,",						 								80, 6, 7, 0},
	{"red hair,",						 									90, 6, 7, 0},
	{"blond hair,",													  100, 6, 7, 0},

	{"and a very dark complexion.",									10, 7, 0, 0},
	{"and a dark complexion.",					 						30, 7, 0, 0},
	{"and an average complexion.",				 					80, 7, 0, 0},
	{"and a fair complexion.",					 						90, 7, 0, 0},
	{"and a very fair complexion.",								  100, 7, 0, 0},
};

static hist_type hist_dwarf[] =
{
	{"You are one of two children of a Dwarven",		 			25, 1, 2, -10},
	{"You are the only child of a Dwarven",					  100, 1, 2, 0},

	{"Thief.",						 										10, 2, 3, -40},
	{"Prison Guard.",						 								25, 2, 3, 25},
	{"Miner.",						 										75, 2, 3, 40},
	{"Warrior.",						 									90, 2, 3, 60},
	{"Priest.",						 										99, 2, 3, 80},
	{"King.",															  100, 2, 3, 100},

	{"You are the black sheep of the family.",		 			15, 3, 4, -40},
	{"You are a credit to the family.",			 					85, 3, 4, 0},
	{"You are a well liked child.",								  100, 3, 4, -5},

	{"You have dark brown eyes,",				 						99, 4, 5, 0},
	{"You have black eyes,",								  		  100, 4, 5, 10},

	{"straight",						 									90, 5, 6, 0},
	{"wavy",																  100, 5, 6, 0},

	{"black hair,",						 								75, 6, 7, 0},
	{"brown hair,",													  100, 6, 7, 0},

	{"a one foot beard,",					 							25, 7, 8, 0},
	{"a two foot beard,",					 							60, 7, 8, 1},
	{"a three foot beard,",					 							90, 7, 8, 3},
	{"a four foot beard,",											  100, 7, 8, 5},

	{"and a dark complexion.",										  100, 8, 0, 0},
};

static hist_type hist_orc[] =
{
	{"Your mother was an Orc, but it is unacknowledged.",	 	25, 1, 2, -25},
	{"Your father was an Orc, but it is unacknowledged.",	  100, 1, 2, -25},

	{"You are the adopted child",									  100, 2, 3, 0},

	{"of a Serf.",                                         	44, 3, 4, 15},
	{"of a Yeoman.",					 									72, 3, 4, 30},
	{"of a Townsman.",					 								89, 3, 4, 40},
	{"of a Guildsman.",					 							  100, 3, 4, 55},

	{"You are the black sheep of the family.",             	20, 4, 5, -30},
	{"You are a credit to the family.",                    	80, 4, 5, 5},
	{"You are a well liked child.",                         100, 4, 5, 10},

	{"You have dark brown eyes,",				 						25, 5, 6, 0},
	{"You have brown eyes,",					 						75, 5, 6, 0},
	{"You have hazel eyes,",					 						88, 5, 6, 0},
	{"You have green eyes,",					 					  100, 5, 6, 0},

	{"straight",						 									70, 6, 7, 0},
	{"wavy",							 										90, 6, 7, 0},
	{"curly",															  100, 6, 7, 0},

	{"black hair,",						 								30, 7, 8, 0},
	{"brown hair,",						 								70, 7, 8, 0},
	{"auburn hair,",						 								80, 7, 8, 0},
	{"red hair,",						 									90, 7, 8, 0},
	{"blond hair,",													  100, 7, 8, 0},

	{"and a very dark complexion.",									13, 8, 0, 0},
	{"and a dark complexion.",					 						38, 8, 0, 0},
	{"and an average complexion.",				 				  100, 8, 0, 0},
};

static hist_type hist_troll[] =
{
	{"Your mother was a",												50, 1, 2, 0},
	{"Your father was a",											  100, 1, 2, 0},

	{"Cave-Troll",				 											30, 2, 3, -30},
	{"Hill-Troll",				 											90, 2, 3, -10},
	{"Water-Troll",													  100, 2, 3, -40},

	{"Cook.",																 5, 3, 4, 0},
	{"Warrior.",						 									95, 3, 4, -10},
	{"Shaman.",						 										99, 3, 4, -15},
	{"Clan Chief.",													  100, 3, 4, 20},

	{"You have slime green eyes,",					 				60, 4, 5, -10},
	{"You have puke yellow eyes,",					 				85, 4, 5, -15},
	{"You have blue-bloodshot eyes,",					 			99, 4, 5, -5},
	{"You have glowing red eyes,",								  100, 4, 5, -20},

	{"dirty",							 									33, 5, 6, -5},
	{"mangy",							 									66, 5, 6, -5},
	{"oily",																  100, 5, 6, -5},

	{"seaweed green hair,",					 							33, 6, 7, 0},
	{"bright red hair,",					 								66, 6, 7, 0},
	{"dark purple hair,",											  100, 6, 7, 0},

	{"and green",						 									25, 7, 8, 0},
	{"and blue",						 									50, 7, 8, 0},
	{"and white",						 									75, 7, 8, 0},
	{"and black",														  100, 7, 8, 0},

	{"ulcerous skin.",						 							33, 8, 0, -20},
	{"scabby skin.",						 								66, 8, 0, -30},
	{"leprous skin.",                  						     100, 8, 0, -50},
};

static hist_type hist_amber[] =
{
	{"You are an unacknowledged child of", 						50, 1, 2, -5},
	{"You are a rebel child of",         							80, 1, 2, 15},
	{"You are a long lost child of",     						  100, 1, 2, 5},

	{"an unknown Amberite.",               						50, 2, 0, 30},
	{"an unknown third generation Amberite.", 					65, 2, 0, 40},
	{"an unknown second generation Amberite.", 					79, 2, 0, 50},
	{"Oberon.",  													   	80, 2, 0, 80},
	{"Osric.",   													   	83, 2, 0, 55},
	{"Finndo.",  													   	84, 2, 0, 55},
	{"Brand.",   													   	85, 2, 0, 40},
	{"Flora.",   													   	87, 2, 0, 50},
	{"Gerard.",  													   	88, 2, 0, 75},
	{"Deirdre.", 													   	89, 2, 0, 70},
	{"Random.",  													   	90, 2, 0, 90},
	{"Benedict.",													   	91, 2, 0, 65},
	{"Corwin.",  													   	92, 2, 0, 60},
	{"Julian.",  													   	93, 2, 0, 55},
	{"Caine.",   													   	94, 2, 0, 45},
	{"Bleys.",   													   	95, 2, 0, 65},
	{"Fiona.",   													   	96, 2, 0, 60},
	{"Eric.",    													   	97, 2, 0, 85},
	{"Rinaldo.", 													   	98, 2, 0, 40},
	{"Merlin.",  													   	99, 2, 0, 55},
	{"Martin.",  													     100, 2, 0, 30},
};

static hist_type hist_ogre[] =
{
	{"Your mother was an Ogre, but it is unacknowledged.",	25, 1, 2, 25},
	{"Your father was an Ogre, but it is unacknowledged.",  100, 1, 2, 25},

	{"You are the adopted child",									  100, 2, 3, 0},

	{"of a Yeoman.",					 									42, 3, 4, 30},
	{"of a Townsman.",					 								67, 3, 4, 40},
	{"of a Guildsman.",					 								83, 3, 4, 55},
	{"of a Landed Knight.",				 								93, 3, 4, 70},
	{"of a Royal Family.",          									97, 3, 4, 80},
	{"of a Noble Family in the Courts of Chaos.",          	99, 3, 4, 90},
	{"of the Royal Blood Line of Amber.",                   100, 3, 4, 100},

	{"You are the black sheep of the family.",             	20, 4, 5, -30},
	{"You are a credit to the family.",                    	80, 4, 5, 5},
	{"You are a well liked child.",                         100, 4, 5, 10},

	{"You have dark brown eyes,",				 						20, 5, 6, 0},
	{"You have brown eyes,",					 						60, 5, 6, 0},
	{"You have hazel eyes,",					 						70, 5, 6, 0},
	{"You have green eyes,",					 						80, 5, 6, 0},
	{"You have blue eyes,",					 							90, 5, 6, 0},
	{"You have blue-gray eyes,",									  100, 5, 6, 0},

	{"straight",						 									70, 6, 7, 0},
	{"wavy",							 										90, 6, 7, 0},
	{"curly",															  100, 6, 7, 0},

	{"black hair,",						 								30, 7, 8, 0},
	{"brown hair,",						 								70, 7, 8, 0},
	{"auburn hair,",						 								80, 7, 8, 0},
	{"red hair,",						 									90, 7, 8, 0},
	{"blond hair,",													  100, 7, 8, 0},

	{"and a very dark complexion.",									10, 8, 0, 0},
	{"and a dark complexion.",					 						30, 8, 0, 0},
	{"and an average complexion.",				 					80, 8, 0, 0},
	{"and a fair complexion.",					 						90, 8, 0, 0},
	{"and a very fair complexion.",								  100, 8, 0, 0},
};

static hist_type hist_giant[] =
{
	{"Your mother was a",												50, 1, 2, 0},
	{"Your father was a",											  100, 1, 2, 0},

	{"Hill Giant.",  														60, 2, 3, 0},
	{"Fire Giant.",  														70, 2, 3, 5},
	{"Frost Giant.",  													80, 2, 3, 10},
	{"Cloud Giant.",  													90, 2, 3, 15},
	{"Storm Giant.", 													  100, 2, 3, 20},

	{"You are the adopted child",									  100, 3, 4, 0},

	{"of a Yeoman.",					 									42, 4, 5, 30},
	{"of a Townsman.",					 								67, 4, 5, 40},
	{"of a Guildsman.",					 								83, 4, 5, 55},
	{"of a Landed Knight.",				 								93, 4, 5, 70},
	{"of a Royal Family.",          									97, 4, 5, 80},
	{"of a Noble Family in the Courts of Chaos.",          	99, 4, 5, 90},
	{"of the Royal Blood Line of Amber.",                   100, 4, 5, 100},

	{"You are the black sheep of the family.",             	20, 5, 6, -30},
	{"You are a credit to the family.",                    	80, 5, 6, 5},
	{"You are a well liked child.",                         100, 5, 6, 10},

	{"You have dark brown eyes,",				 						20, 6, 7, 0},
	{"You have brown eyes,",					 						60, 6, 7, 0},
	{"You have hazel eyes,",					 						70, 6, 7, 0},
	{"You have green eyes,",					 						80, 6, 7, 0},
	{"You have blue eyes,",					 							90, 6, 7, 0},
	{"You have blue-gray eyes,",									  100, 6, 7, 0},

	{"straight",						 									70, 7, 8, 0},
	{"wavy",							 										90, 7, 8, 0},
	{"curly",															  100, 7, 8, 0},

	{"black hair,",						 								30, 8, 9, 0},
	{"brown hair,",						 								70, 8, 9, 0},
	{"auburn hair,",						 								80, 8, 9, 0},
	{"red hair,",						 									90, 8, 9, 0},
	{"blond hair,",													  100, 8, 9, 0},

	{"and a very dark complexion.",									10, 9, 0, 0},
	{"and a dark complexion.",					 						30, 9, 0, 0},
	{"and an average complexion.",				 					80, 9, 0, 0},
	{"and a fair complexion.",					 						90, 9, 0, 0},
	{"and a very fair complexion.",								  100, 9, 0, 0},
};

static hist_type hist_titan[] =
{
	{"Your father was an unknown Titan.", 							75, 1, 2, 0},
	{"Your mother was Themis.",        								80, 1, 2, 50},
	{"Your mother was Mnemosyne.",     								85, 1, 2, 50},
	{"Your father was Okeanoas.",      								90, 1, 2, 50},
	{"Your father was Crius.",         								95, 1, 2, 50},
	{"Your father was Hyperion.",      								98, 1, 2, 75},
	{"Your father was Kronos.",       							  100, 1, 2, 100},

	{"You are the adopted child",									  100, 2, 3, 0},

	{"of a Guildsman.",					 								50, 3, 4, 55},
	{"of a Landed Knight.",				 								80, 3, 4, 70},
	{"of a Royal Family.",          									90, 3, 4, 80},
	{"of a Noble Family in the Courts of Chaos.",          	95, 3, 4, 90},
	{"of the Royal Blood Line of Amber.",                   100, 3, 4, 100},

	{"You are the black sheep of the family.",             	20, 4, 5, -30},
	{"You are a credit to the family.",                    	80, 4, 5, 5},
	{"You are a well liked child.",                         100, 4, 5, 10},

	{"You have dark brown eyes,",				 						20, 5, 6, 0},
	{"You have brown eyes,",					 						60, 5, 6, 0},
	{"You have hazel eyes,",					 						70, 5, 6, 0},
	{"You have green eyes,",					 						80, 5, 6, 0},
	{"You have blue eyes,",					 							90, 5, 6, 0},
	{"You have blue-gray eyes,",									  100, 5, 6, 0},

	{"straight",						 									70, 6, 7, 0},
	{"wavy",							 										90, 6, 7, 0},
	{"curly",															  100, 6, 7, 0},

	{"black hair,",										 				75, 7, 8, 0},
	{"grey hair,",				 											85, 7, 8, 5},
	{"blond hair,",										 				95, 7, 8, 0},
	{"silver hair,",													  100, 7, 8, 10},

	{"and a very dark complexion.",									10, 8, 0, 0},
	{"and a dark complexion.",					 						30, 8, 0, 0},
	{"and an average complexion.",				 					80, 8, 0, 0},
	{"and a fair complexion.",					 						90, 8, 0, 0},
	{"and a very fair complexion.",								  100, 8, 0, 0},
};

static hist_type hist_cyclops[] =
{
	{"You are the offspring of an unknown Cyclops.",	 		90, 1, 2, 0},
	{"You are Polyphemos's child.", 									98, 1, 2, 30},
	{"You are Uranos's child.", 									  100, 1, 2, 85},

	{"You have a dark brown eye,",               				20, 2, 0, 0},
	{"You have a brown eye,",                    				60, 2, 0, 0},
	{"You have a hazel eye,",                    				70, 2, 0, 0},
	{"You have a green eye,",                    				80, 2, 0, 0},
	{"You have a blue eye,",                     				90, 2, 0, 0},
	{"You have a blue-gray eye,",               				  100, 2, 0, 0},
};

static hist_type hist_yeek[] =
{
	{"You are one of several children of", 					  100, 1, 2, 0},

	{"a Kamikaze",		 													20, 2, 3, -30},
	{"a Brown Yeek.", 													50, 2, 4, -20},
	{"a Blue Yeek.", 														75, 2, 4, -10},
	{"a Master Yeek.", 													90, 2, 4, 35},
	{"Orfax.",								 							   94, 2, 4, 50},
	{"Boldor.",								 							  100, 2, 4, 70},

	{"Yeek whom you never knew.", 									70, 3, 4, -10},
	{"Yeek.", 															  100, 3, 4, 0},

	{"You have pale eyes,",    										25, 4, 5, 0},
	{"You have glowing eyes,",    									50, 4, 5, 0},
	{"You have tiny black eyes,",    								75, 4, 5, 0},
	{"You have beady black eyes,", 									90, 4, 5, 0},
	{"You have shining black eyes,",    						  100, 4, 5, 0},

	{"no hair at all,",        										20, 5, 6, 0},
	{"short black hair,",        										40, 5, 6, 0},
	{"long black hair,",        										60, 5, 6, 0},
	{"bright red hair,",        										80, 5, 6, 0},
	{"colourless albino hair,",        							  100, 5, 6, 0},

	{"and green",						 									33, 6, 7, 0},
	{"and blue",						 									66, 6, 7, 0},
	{"and brown",						 								  100, 6, 7, 0},

	{"lumpy skin.",							 							33, 7, 0, 0},
	{"wrinkled skin.",					 								66, 7, 0, 0},
	{"smooth skin.",                  						     100, 7, 0, 0},
};

static hist_type hist_kobold[] =
{
	{"You are one of several children of", 					  100, 1, 2, 0},

	{"a Small Kobold.",   												40, 2, 3, 0},
	{"a Kobold.",         												75, 2, 3, 5},
	{"a Large Kobold.",   												95, 2, 3, 15},
	{"Mughash.",     													  100, 2, 3, 50},

	{"You have pale eyes,",    										25, 3, 4, 0},
	{"You have glowing eyes,",    									50, 3, 4, 0},
	{"You have tiny black eyes,",    								75, 3, 4, 0},
	{"You have beady black eyes,", 									90, 3, 4, 0},
	{"You have shining black eyes,",    						  100, 3, 4, 0},

	{"no hair at all,",        										20, 4, 5, 0},
	{"short black hair,",        										40, 4, 5, 0},
	{"long black hair,",        										60, 4, 5, 0},
	{"bright red hair,",        										80, 4, 5, 0},
	{"colourless albino hair,",        							  100, 4, 5, 0},

	{"and a green",					 									33, 5, 6, 0},
	{"and a blue",						 									66, 5, 6, 0},
	{"and a brown",					 								  100, 5, 6, 0},

	{"fuzzy hide.",							 							50, 6, 7, 0},
	{"bristly hide.",						 							  100, 6, 7, 0},

	{"You have the head of a",                              100, 7, 8, 0},

	{"Jackal.",                                              38, 8, 0, -10},
	{"Foxhound.",                                            62, 8, 0, 0},
	{"Wolfhound.",                                           81, 8, 0, 0},
	{"Collie.",                                              88, 8, 0, 0},
	{"Terrier.",                                             90, 8, 0, 0},
	{"Chihuahua.",                                           91, 8, 0, -5},
	{"Wolf.",                                               100, 8, 0, 10},
};

static hist_type hist_klackon[] =
{
	{"You are one of a hive queen's many children.",		  100, 1, 2, 0},

	{"You have red skin", 												40, 2, 3, 0},
	{"You have black skin", 											90, 2, 3, 0 },
	{"You have yellow skin", 										  100, 2, 3, 0 },

	{"and black eyes.", 											     100, 3, 0, 0 },
};

static hist_type hist_nibelung[] =
{
	{"You are one of several children of", 					  100, 1, 2, 0},

	{"a Nibelung Slave.", 												30, 2, 3, -30},
	{"a Nibelung Thief.", 												50, 2, 3, -10},
	{"a Nibelung Smith.", 												70, 2, 3, 10},
	{"a Nibelung Miner.", 												90, 2, 3, 25},
	{"a Nibelung Shaman.", 												95, 2, 3, 50},
	{"Mime.",					 										  100, 2, 3, 50},

	{"You are the black sheep of the family.",		 			15, 3, 4, -40},
	{"You are a credit to the family.",			 					85, 3, 4, 0},
	{"You are a well liked child.",								  100, 3, 4, -5},

	{"You have dark brown eyes,",				 						99, 4, 5, 0},
	{"You have black eyes,",								  		  100, 4, 5, 10},

	{"straight",						 									90, 5, 6, 0},
	{"wavy",																  100, 5, 6, 0},

	{"black hair,",						 								75, 6, 7, 0},
	{"brown hair,",													  100, 6, 7, 0},

	{"a one foot beard,",					 							25, 7, 8, 0},
	{"a two foot beard,",					 							60, 7, 8, 1},
	{"a three foot beard,",					 							90, 7, 8, 3},
	{"a four foot beard,",											  100, 7, 8, 5},

	{"and a dark complexion.",										  100, 8, 0, 0},
};

static hist_type hist_draconian[] =
{
	{"You are one of several children of a Draconian", 		85, 1, 2, 0},
	{"You are the only child of a Draconian", 				  100, 1, 2, 5},

	{"Warrior.", 															50, 2, 3, 0},
	{"Priest.", 															65, 2, 3, 15},
	{"Mage.", 																85, 2, 3, 20},
	{"Noble.", 															  100, 2, 3, 50},

	{"You have green wings, green skin and yellow belly.", 	30, 3, 0, 0},
	{"You have green wings, and green skin.", 					55, 3, 0, 0},
	{"You have red wings, and red skin.", 							80, 3, 0, 0},
	{"You have black wings, and black skin.", 					90, 3, 0, 0},
	{"You have metallic skin, and shining wings.", 			  100, 3, 0, 0},
};

static hist_type hist_mindflayer[] =
{
	{"You have slimy skin, empty glowing eyes, and", 		  100, 1, 2, 30},

	{"three tentacles around your mouth.", 						20, 2, 0, 15},
	{"five tentacles around your mouth.", 							80, 2, 0, 0 },
	{"eight tentacles around your mouth.", 					  100, 2, 0, 5 },
};

static hist_type hist_imp[] =
{
	{"Your ancestor was", 											  100, 1, 2, 0},

	{"a mindless demonic spawn.", 									30, 2, 3, -30},
	{"a minor demon.", 													60, 2, 3, 0},
	{"a major demon.", 													90, 2, 3, 25},
	{"a demon lord.", 												  100, 2, 3, 50},

	{"You have brown skin,", 										  100, 3, 4, 0},

	{"claws, fangs, spikes, and glowing red eyes.", 			40, 4, 0, 0},
	{"claws, fangs, and glowing red eyes.", 						70, 4, 0, 0},
	{"claws, and glowing red eyes.", 							  100, 4, 0, 0},
};

static hist_type hist_golem[] =
{
	{"You were shaped from", 										  100, 1, 2, 0},

	{"a corpse", 															40, 2, 3, -10},
	{"clay", 																80, 2, 3, 0},
	{"stone", 																85, 2, 3, 10},
	{"iron", 																99, 2, 3, 20},
	{"mithril", 														  100, 2, 3, 50},

	{"by a Kabbalist", 													40, 3, 4, 0},
	{"by a wizard", 														65, 3, 4, 0},
	{"by an alchemist", 													90, 3, 4, 0},
	{"by a priest", 													  100, 3, 4, 10},

	{" to fight evil.", 													10, 4, 0, 15},
	{".", 																  100, 4, 0, 0},
};

static hist_type hist_skeleton[] =
{
	{"You were created by", 										  100, 1, 2, 0},

	{"a legion of druj.", 												10, 2, 3, -20},
	{"a necromancer.", 													30, 2, 3, 0},
	{"a magical experiment.", 											50, 2, 3, 0},
	{"an evil priest.", 													70, 2, 3, 0},
	{"a pact with the demons.", 										75, 2, 3, 0},
	{"a restless spirit.", 												85, 2, 3, 0},
	{"a curse.", 															95, 2, 3, -20},
	{"an oath.", 															99, 2, 3, 0},
	{"Morgoth.", 														  100, 2, 3, -50},

	{"You have", 														  100, 3, 4, 0},

	{"dirty, dry bones,", 												40, 4, 5, 0},
	{"rotten, black bones,", 											60, 4, 5, 0},
	{"filthy, brown bones,", 											80, 4, 5, 0},
	{"shining white bones,", 										  100, 4, 5, 0},

	{"and glowing eyes.", 												30, 5, 0, 0},
	{"and eyes that burn with hellfire.", 							50, 5, 0, 10},
	{"and empty eyesockets.", 										  100, 5, 0, 0},
};

static hist_type hist_zombie[] =
{
	{"You arose from an unmarked grave.", 							20, 1, 4, -30},
	{"You arose from the grave of a", 								30, 1, 2, 0},
	{"You emerged from a desecrated tomb.", 						50, 1, 4, -10},
	{"You are the result of a failed magical experiment.", 	75, 1, 4, 0},
	{"A mindflayer ate your brain.", 								95, 1, 4, 0},
	{"You came from the tomb of a", 									98, 1, 3, 0},
	{"You were animated by druj.", 								  100, 1, 4, -20},

	{"Peasant.", 															50, 2, 4, -20},
	{"Yeoman.", 															65, 2, 4, -10},
	{"Townsman.", 															90, 2, 4, -5},
	{"Guildsman.", 													  100, 2, 4, 5},

	{"Knight.", 															60, 3, 4, -10},
	{"Noble.", 																90, 3, 4, 0},
	{"Royal Family.", 												  100, 3, 4, 10},

	{"You have", 														  100, 4, 5, 0},

	{"no hair at all,", 													70, 5, 7, -10},
	{"dirty", 																80, 5, 6, 0},
	{"matted", 																95, 5, 6, 0},
	{"oily", 														     100, 5, 6, 0},

	{"black hair,", 														25, 6, 7, 0},
	{"brown hair,", 														50, 6, 7, 0},
	{"red hair,", 															75, 6, 7, 0},
	{"blond hair,", 														95, 6, 7, 0},
	{"white hair,", 													  100, 6, 7, 0},

	{"milky blind eyes,", 												40, 7, 8, 0},
	{"red bloodshot eyes,", 											60, 7, 8, -5},
	{"eyes which ooze pus,", 											75, 7, 8, -30},
	{"eyes black with decay,", 										90, 7, 8, -20},
	{"only one eye,", 													97, 7, 8, -15},
	{"no eyes at all,", 												  100, 7, 8, -10},

	{"and a vacant expression.", 										60, 8, 9, 10},
	{"and a gaping mouth.", 											90, 8, 9, -5},
	{"and a hint of intelligence.", 								  100, 8, 9, 20},

	{"Your flesh hangs from your bones.", 							30, 9, 10, -10},
	{"Bits of flesh fall from your body as you walk.", 		50, 9, 10, -20},
	{"Your body oozes pus.", 											65, 9, 10, -30},
	{"You reek with decay.", 											70, 9, 10, -30},
	{"Your flesh crawls with maggots.", 							80, 9, 10, -20},
	{"Many of your bones are broken.", 								95, 9, 10, -15},
	{"Your skin has the pallor of death.", 					  100, 9, 10, 0},

	{"You are mute.", 													40, 10, 0, 0},
	{"You can grunt.", 													60, 10, 0, 10},
	{"You understand a few words.", 									90, 10, 0, 15},
	{"You are able to grunt broken phrases.", 				  100, 10, 0, 20},
};

static hist_type hist_vampire[] =
{
	{"You arose from an unmarked grave.", 							20, 1, 2, -20},
	{"In life you were a simple peasant.", 						40, 1, 2, -10},
	{"In life you were a vampire hunter, but you failed.",	60, 1, 2, 0},
	{"In life you were a necromancer.", 							80, 1, 2, 10},
	{"In life you were a powerful noble.", 						95, 1, 2, 25},
	{"In life you were a powerful and cruel tyrant.", 		  100, 1, 2, 40},

	{"You have", 														  100, 2, 3, 0},

	{"jet-black hair,", 													25, 3, 4, 0},
	{"matted brown hair,", 												50, 3, 4, 0},
	{"white hair,", 														75, 3, 4, 0},
	{"a hairless head,", 											  100, 3, 4, 0},

	{"eyes like red coals,", 											25, 4, 5, 0},
	{"blank white eyes,", 												50, 4, 5, 0},
	{"feral yellow eyes,", 												75, 4, 5, 0},
	{"bloodshot red eyes,", 										  100, 4, 5, 0},

	{"and a deathly pale complexion.", 							  100, 5, 0, 0},
};

static hist_type hist_spectre[] =
{
	{"You were created by", 										  100, 1, 2, 0},

	{"a legion of druj.", 												10, 2, 3, -20},
	{"a necromancer.", 													30, 2, 3, 0},
	{"a magical experiment.", 											50, 2, 3, 0},
	{"an evil priest.", 													70, 2, 3, 0},
	{"a pact with the demons.", 										75, 2, 3, 0},
	{"a restless spirit.", 												85, 2, 3, 0},
	{"a curse.", 															95, 2, 3, -20},
	{"an oath.", 															99, 2, 3, 0},
	{"Morgoth.", 														  100, 2, 3, -50},

	{"You have", 														  100, 3, 4, 0},

	{"jet-black hair,", 													25, 4, 5, 0},
	{"matted brown hair,", 												50, 4, 5, 0},
	{"white hair,", 														75, 4, 5, 0},
	{"a hairless head,", 											  100, 4, 5, 0},

	{"eyes like red coals,", 											25, 5, 6, 0},
	{"blank white eyes,", 												50, 5, 6, 0},
	{"feral yellow eyes,", 												75, 5, 6, 0},
	{"bloodshot red eyes,", 										  100, 5, 6, 0},

	{" and a deathly gray complexion.", 						  100, 6, 7, 0},

	{"An eerie green aura surrounds you.", 						40, 7, 0, 5},
	{"", 																	  100, 7, 0, 0},
};

static hist_type hist_sprite[] =
{
	{"Your parents were", 										 	  100, 1, 2, 0},

	{"pixies.", 															20, 2, 3, -15},
	{"nixies.", 															30, 2, 3, -25},
	{"wood sprites.", 													75, 2, 3, 0},
	{"wood spirits.", 													90, 2, 3, 25},
	{"noble faerie folk.", 											  100, 2, 3, 35},

	{"You have light blue wings attached to your back,", 	  100, 3, 4, 0},

	{"straight blond hair,",                        			80, 4, 5, 0},
	{"wavy blond hair,",                            		  100, 4, 5, 0},

	{"blue eyes, and a very fair complexion.", 				  100, 5, 0, 0},
};

static hist_type hist_beastman[] =
{
	{"You were produced by a magical experiment.", 												30, 1, 2, -10},
	{"In your childhood, you were stupid enough to stick your head in raw Logrus.",	50, 1, 2, -20},
	{"A Demon Lord of Chaos decided to have some fun, and so created you.",				60, 1, 2, 10},
	{"You are the magical crossbreed of an animal and a man.", 								75, 1, 2, 0},
	{"You are the blasphemous crossbreed of unspeakable creatures of chaos.", 		  100, 1, 2, -20},

	{"You have green reptilian eyes,",     								         			60, 2, 3, 0},
	{"You have the black eyes of a bird,", 								             		85, 2, 3, 0},
	{"You have the orange eyes of a cat,", 								              		99, 2, 3, 0},
	{"You have the fiery eyes of a demon,",								             	  100, 2, 3, 5},

	{"no hair at all,",                 															10, 3, 4, 0},
	{"dirty",                           															33, 3, 4, 0},
	{"mangy",                           															66, 3, 4, 0},
	{"oily",                           															  100, 3, 4, 0},

	{"brown fur,",                    																33, 4, 5, 0},
	{"gray fur,",                    																66, 4, 5, 0},
	{"albino fur,",                  															  100, 4, 5, 0},

	{"and the hooves of a goat.",      																50, 5, 0, -10},
	{"and human feet.",        																		75, 5, 0, 0},
	{"and bird's feet.",       																		85, 5, 0, -5},
	{"and reptilian feet.",    																		90, 5, 0, -20},
	{"and bovine feet.",       																		95, 5, 0, -5},
	{"and feline feet.",       																		97, 5, 0, 5},
	{"and canine feet.",       																	  100, 5, 0, 5},
};

static hist_type hist_fiend[] =
{
	{"You were spawned by a major demon.", 							70, 1, 2, -40},
	{"A demonologist created you.", 										85, 1, 2, -30},
	{"You arose from a pool of primal chaos.",	 					95, 1, 2, -45},
	{"A curse called you into being.", 								  100, 1, 2, -20},

	{"None of your siblings survived their birth.", 				40, 2, 4, -10},
	{"You were the strongest of your litter.", 						70, 2, 4, 10},
	{"You opportunistically destroyed your siblings.", 			77, 2, 4, 0},
	{"You aided the strongest of your litter at your birth.",	92, 2, 4, 5},
	{"You aided your litter's runt, but survived.", 				95, 2, 4, 20},
	{"You were the runt of the litter,", 							  100, 2, 3, 10},

	{"but managed to escape.", 											99, 3, 4, 0},
	{"but emerged victorious!", 										  100, 3, 4, 25},

	{"You have brown skin,", 												70, 4, 5, 0},
	{"You have pink skin,", 												90, 4, 5, 0},
	{"You have jet black skin,", 										  100, 4, 5, 0},

	{"a light build,", 														10, 5, 6, -10},
	{"a medium build,", 														40, 5, 6, -5},
	{"a heavy build,", 													  100, 5, 6, 0},

	{"two", 																		10, 6, 7, 0},
	{"three", 																	50, 6, 7, 0},
	{"four", 																	70, 6, 7, 0},
	{"five", 																	80, 6, 7, 0},
	{"six", 																		95, 6, 7, 0},
	{"seven", 																  100, 6, 7, 0},

	{"claws on each limb,", 											  100, 7, 8, 0},

	{"glowing red eyes,", 													60, 8, 9, -5},
	{"beady black eyes,", 													70, 8, 9, 0},
	{"milky white eyes,", 													90, 8, 9, -3},
	{"a chilling gaze,", 													93, 8, 9, -15},
	{"no eyes at all,", 													  100, 8, 9, -10},

	{"and a foul, toothy grin.", 											30, 9, 0, -10},
	{"and an unusually large array of teeth.",	 					50, 9, 0, -5},
	{"and sharp, ragged fangs.", 											75, 9, 0, -15},
	{"and a terrifying smile.", 											82, 9, 0, -20},
	{"and a deathly visage.", 												90, 9, 0, -30},
	{"and a sulphurous breath.", 											98, 9, 0, -20},
	{"and breath that reeks of decay.", 							  100, 9, 0, -35},
};


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
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
	int i;

	int bonus;

	while (1)
	{
		int skew = 0;

		/* Acquire the stats */
		for (i = 0; i < 6; i++)
		{
			/* Save that value */
			p_ptr->stat_max[i] = rand_norm(15);

			p_ptr->stat_max[i] = MAX(3, p_ptr->stat_max[i]);

			skew += p_ptr->stat_max[i] - 15;

			/* Obtain a "bonus" for "race" and "class" */
			bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

			/* Start fully healed */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/class bonuses */
			stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

      if (ABS(skew) < 7) break;
	}
}


/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
	int i, j, min_value, max_value;

	/* Level one */
	p_ptr->max_level = p_ptr->level = 1;

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

		/* Require "valid" hitpoints at highest level */
		if (player_hp[PY_MAX_LEVEL-1] < min_value) continue;
		if (player_hp[PY_MAX_LEVEL-1] > max_value) continue;

		/* Acceptable */
		break;
	}
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, n, chart = 1;
	int roll, social_class;

	char buf[240];

	char *s = buf, *t;

	hist_type *hist = hist_human;

	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = rand_nor(50, 5);

	/* Starting place */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
		case RACE_BARBARIAN:
		{
			hist = hist_human;
			break;
		}
		case RACE_HALF_ELF:
		{
			hist = hist_half_elf;
			break;
		}
		case RACE_ELF:
		{
			hist = hist_elf;
			break;
		}
		case RACE_HOBBIT:
		{
			hist = hist_hobbit;
			break;
		}
		case RACE_GNOME:
		{
			hist = hist_gnome;
			break;
		}
		case RACE_DWARF:
		{
			hist = hist_dwarf;
			break;
		}
		case RACE_HALF_ORC:
		{
			hist = hist_orc;
			break;
		}
		case RACE_HALF_TROLL:
		{
			hist = hist_troll;
			break;
		}
		case RACE_AMBERITE:
		{
			hist = hist_amber;
			break;
		}
		case RACE_HIGH_ELF:
		{
			hist = hist_high_elf;
			break;
		}
		case RACE_HALF_OGRE:
		{
			hist = hist_ogre;
			break;
		}
		case RACE_HALF_GIANT:
		{
			hist = hist_giant;
			break;
		}
		case RACE_HALF_TITAN:
		{
			hist = hist_titan;
			break;
		}
		case RACE_CYCLOPS:
		{
			hist = hist_cyclops;
			break;
		}
		case RACE_YEEK:
		{
			hist = hist_yeek;
			break;
		}
		case RACE_KOBOLD:
		{
			hist = hist_kobold;
			break;
		}
		case RACE_KLACKON:
		{
			hist = hist_klackon;
			break;
		}
		case RACE_NIBELUNG:
		{
			hist = hist_nibelung;
			break;
		}
		case RACE_DRACONIAN:
		{
	      hist = hist_draconian;
			break;
		}
		case RACE_MIND_FLAYER:
		{
	      hist = hist_mindflayer;
			break;
		}
		case RACE_IMP:
		{
	      hist = hist_imp;
			break;
		}
		case RACE_GOLEM:
		{
	      hist = hist_golem;
			break;
		}
		case RACE_SKELETON:
		{
	      hist = hist_skeleton;
			break;
		}
		case RACE_ZOMBIE:
		{
	      hist = hist_zombie;
			break;
		}
		case RACE_VAMPIRE:
		{
	      hist = hist_vampire;
			break;
		}
		case RACE_SPECTRE:
		{
	      hist = hist_spectre;
			break;
		}
		case RACE_SPRITE:
		{
	      hist = hist_sprite;
			break;
		}
		case RACE_BEASTMAN:
		{
			hist = hist_beastman;
			break;
		}
		case RACE_FIEND:
		{
	      hist = hist_fiend;
			break;
		}
	}


	/* Process the history */
	while (TRUE)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);

		/* Access the proper entry in the table */
		while ((chart != hist[i].chart) || (roll > hist[i].roll)) i++;

		/* Acquire the textual history */
		strcat(buf, hist[i].info);

		/* Add in the social class */
		social_class += hist[i].bonus;

		/* Enter the next chart */
		chart = hist[i].next;

		if (!chart) break;

		/* Add spaces as needed */
		switch (buf[strlen(buf) - 1])
		{
			case '.': case '!': case '?':
			{
				strcat(buf, "  ");
	         break;
			}
			default:
			{
	         strcat(buf, " ");
			}
		}
	}

	/* Verify social class */
	if (social_class < 0) social_class = 0;

	/* Save the social class */
	p_ptr->sc = social_class;

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
		for (n = 60; (n > 0) && (s[n-1] != ' '); n--);

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

		/* Save one line of history */
		strcpy(history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++);
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
		p_ptr->ht = rand_nor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = rand_nor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = rand_nor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = rand_nor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
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

	/* Trade gold for high stats */
	for (i = 0; i < 6; i++)
	{
	   gold -= stat_score[i] * (stat_use[i] - 15) / 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = rand_nor(gold, gold / 10);
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
	for (i = 0; i < 4; i++) history[i][0] = '\0';

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

	/* Initialize quests (Heino Vander Sanden and Jimmy De Laet) */
	initialise_quests();

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

	/* Hack -- no ghosts */
	r_info[MAX_R_IDX-1].max_num = 0;

	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

	/* Wipe the spells */
	spell_learned1 = spell_learned2 = 0L;
	spell_worked1 = spell_worked2 = 0L;
	spell_forgotten1 = spell_forgotten2 = 0L;
	for (i = 0; i < 64; i++) spell_order[i] = 99;

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
	p_ptr->noscore = 0;

	/* Use a default pet follow distance */
	p_ptr->pet_follow_distance = 6;

	/* Use some default pet options */
	p_ptr->pet_pickup_items = TRUE;
}


/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
typedef struct init_equip_type init_equip_type;
typedef struct conv_equip_type conv_equip_type;

struct init_equip_type
{
	byte tval;
	byte sval;
	byte dice;
	byte side;

	byte tag;
};

struct conv_equip_type
{
	byte tval1;
	byte sval1;
	byte tval2;
	byte sval2;

	byte tag;
};

#define TV_BOOK_REALM1		254
#define TV_BOOK_REALM2		255

#define SV_ANY					255

static init_equip_type equip_class_init[] =
{
	/* Warrior */
	{TV_RING, 				SV_RING_RES_FEAR,						1, 1, CLASS_WARRIOR},
	{TV_HARD_ARMOR,		SV_CHAIN_MAIL,							1, 1, CLASS_WARRIOR},
	{TV_GLOVES,				SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_WARRIOR},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_WARRIOR},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_WARRIOR},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_WARRIOR},
	{TV_SWORD, 				SV_BROAD_SWORD,						1, 1, CLASS_WARRIOR},

	/* Mage */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_MAGE},
	{TV_BOOK_REALM2,		0,											1, 1, CLASS_MAGE},
	{TV_SOFT_ARMOR,		SV_ROBE,									1, 1, CLASS_MAGE},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_MAGE},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_MAGE},
	{TV_SWORD, 				SV_DAGGER,								1, 1, CLASS_MAGE},

	/* Priest */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_PRIEST},
	{TV_BOOK_REALM2,		0,											1, 1, CLASS_PRIEST},
	{TV_SOFT_ARMOR,		SV_ROBE,									1, 1, CLASS_PRIEST},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_PRIEST},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_PRIEST},
	{TV_HAFTED,				SV_MACE,									1, 1, CLASS_PRIEST},

	/* Rogue */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_ROGUE},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_ROGUE},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_ROGUE},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_ROGUE},
	{TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_ROGUE},
	{TV_SWORD,				SV_DAGGER,								1, 1, CLASS_ROGUE},

	/* Ranger */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_RANGER},
	{TV_BOOK_REALM2,		0,											1, 1, CLASS_RANGER},
	{TV_CLOAK,				SV_ELVEN_CLOAK,						1, 1, CLASS_RANGER},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_RANGER},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_RANGER},
	{TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_RANGER},
	{TV_SWORD, 				SV_LONG_SWORD,							1, 1, CLASS_RANGER},
	{TV_BOW,					SV_SHORT_BOW,							1, 1, CLASS_RANGER},
	{TV_ARROW,				SV_AMMO_NORMAL,						6, 7, CLASS_RANGER},

	/* Paladin */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_PALADIN},
	{TV_SCROLL, 			SV_SCROLL_PROTECTION_FROM_EVIL,	1, 1, CLASS_PALADIN},
	{TV_HARD_ARMOR,		SV_CHAIN_MAIL,							1, 1, CLASS_PALADIN},
	{TV_GLOVES,				SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_PALADIN},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_PALADIN},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_PALADIN},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_PALADIN},
	{TV_HAFTED, 			SV_WAR_HAMMER,							1, 1, CLASS_PALADIN},

	/* Warrior-Mage */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_WARRIOR_MAGE},
	{TV_BOOK_REALM2,		0,											1, 1, CLASS_WARRIOR_MAGE},
	{TV_SOFT_ARMOR,		SV_LEATHER_SCALE_MAIL,				1, 1, CLASS_WARRIOR_MAGE},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_WARRIOR_MAGE},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_WARRIOR_MAGE},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_WARRIOR_MAGE},
	{TV_SWORD, 				SV_SHORT_SWORD, 						1, 1, CLASS_WARRIOR_MAGE},

	/* Chaos Warrior */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_CHAOS_WARRIOR},
	{TV_HARD_ARMOR,		SV_METAL_SCALE_MAIL,					1, 1, CLASS_CHAOS_WARRIOR},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_CHAOS_WARRIOR},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_CHAOS_WARRIOR},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_CHAOS_WARRIOR},
	{TV_SWORD, 				SV_BROAD_SWORD,						1, 1, CLASS_CHAOS_WARRIOR},

	/* Monk */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_MONK},
	{TV_POTION,				SV_POTION_HEALING, 					1, 1, CLASS_MONK},
	{TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_MONK},
	{TV_GLOVES,				SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_MONK},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_MONK},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_MONK},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_MONK},
	{TV_HAFTED,				SV_QUARTERSTAFF,						1, 1, CLASS_MONK},

	/* Mindcrafter */
	{TV_POTION, 			SV_POTION_RESTORE_MANA,				1, 1, CLASS_MINDCRAFTER},
	{TV_SOFT_ARMOR,		SV_SOFT_LEATHER_ARMOR,				1, 1, CLASS_MINDCRAFTER},
	{TV_GLOVES,				SV_SET_OF_LEATHER_GLOVES,			1, 1, CLASS_MINDCRAFTER},
	{TV_CLOAK,				SV_CLOAK,								1, 1, CLASS_MINDCRAFTER},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_MINDCRAFTER},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_MINDCRAFTER},
	{TV_SWORD, 				SV_SMALL_SWORD,						1, 1, CLASS_MINDCRAFTER},

	/* High Mage */
	{TV_BOOK_REALM1,		0,											1, 1, CLASS_HIGH_MAGE},
	{TV_WAND,				SV_WAND_STINKING_CLOUD,				1, 1, CLASS_HIGH_MAGE},
	{TV_RING,				SV_RING_SUSTAIN_INT,					1, 1, CLASS_HIGH_MAGE},
	{TV_SOFT_ARMOR,		SV_ROBE,									1, 1, CLASS_HIGH_MAGE},
	{TV_BOOTS,				SV_PAIR_OF_SOFT_LEATHER_BOOTS,	1, 1, CLASS_HIGH_MAGE},
	{TV_HELM,				SV_HARD_LEATHER_CAP,					1, 1, CLASS_HIGH_MAGE},
	{TV_SWORD,				SV_DAGGER,								1, 1, CLASS_HIGH_MAGE},

	/* Terminate */
	{0, 						0, 										0, 0, 0}
};

static init_equip_type equip_race_init[] =
{
	/* RACE_HUMAN */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HUMAN},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HUMAN},

	/* RACE_HALF_ELF */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_ELF},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_ELF},

	/* RACE_ELF */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_ELF},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_ELF},

	/* RACE_HOBBIT */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HOBBIT},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HOBBIT},

	/* RACE_GNOME */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_GNOME},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_GNOME},

	/* RACE_DWARF */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_DWARF},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_DWARF},

	/* RACE_HALF_ORC */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_ORC},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_ORC},

	/* RACE_HALF_TROLL */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_TROLL},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_TROLL},

	/* RACE_AMBERITE */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_AMBERITE},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_AMBERITE},

	/* RACE_HIGH_ELF */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HIGH_ELF},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HIGH_ELF},

	/* RACE_BARBARIAN */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_BARBARIAN},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_BARBARIAN},

	/* RACE_HALF_OGRE */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_OGRE},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_OGRE},

	/* RACE_HALF_GIANT */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_GIANT},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_GIANT},

	/* RACE_HALF_TITAN */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_HALF_TITAN},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_HALF_TITAN},

	/* RACE_CYCLOPS */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_CYCLOPS},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_CYCLOPS},

	/* RACE_YEEK */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_YEEK},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_YEEK},

	/* RACE_KLACKON */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_KLACKON},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_KLACKON},

	/* RACE_KOBOLD */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_KOBOLD},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_KOBOLD},

	/* RACE_NIBELUNG */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_NIBELUNG},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_NIBELUNG},

	/* RACE_DRACONIAN */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_DRACONIAN},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_DRACONIAN},

	/* RACE_MIND_FLAYER */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_MIND_FLAYER},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_MIND_FLAYER},

	/* RACE_IMP */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_IMP},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_IMP},

	/* RACE_GOLEM */
	{TV_SCROLL,				SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_GOLEM},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_GOLEM},

	/* RACE_SKELETON */
	{TV_SCROLL,				SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_SKELETON},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_SKELETON},

	/* RACE_ZOMBIE */
	{TV_SCROLL,				SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_ZOMBIE},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_ZOMBIE},

	/* RACE_VAMPIRE */
	{TV_SCROLL,				SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_VAMPIRE},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_VAMPIRE},

	/* RACE_SPECTRE */
	{TV_SCROLL,				SV_SCROLL_SATISFY_HUNGER,			2, 3, RACE_SPECTRE},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_SPECTRE},

	/* RACE_SPRITE */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_SPRITE},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_SPRITE},

	/* RACE_BEASTMAN */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_BEASTMAN},
	{TV_LITE,				SV_LITE_TORCH,							3, 3, RACE_BEASTMAN},

	/* RACE_FIEND */
	{TV_FOOD,				SV_FOOD_RATION,						3, 3, RACE_FIEND},

	/* Terminate */
	{0, 						0, 										0, 0, 0}
};

static conv_equip_type equip_race_conv[] =
{
	/* RACE_HUMAN */

	/* RACE_HALF_ELF */

	/* RACE_ELF */

	/* RACE_HOBBIT */

	/* RACE_GNOME */

	/* RACE_DWARF */

	/* RACE_HALF_ORC */

	/* RACE_HALF_TROLL */

	/* RACE_AMBERITE */

	/* RACE_HIGH_ELF */

	/* RACE_BARBARIAN */
	{
		TV_RING, 			SV_RING_RES_FEAR,
		TV_RING,				SV_RING_SUSTAIN_STR,
		RACE_BARBARIAN
	},

	/* RACE_HALF_OGRE */

	/* RACE_HALF_GIANT */

	/* RACE_HALF_TITAN */

	/* RACE_CYCLOPS */

	/* RACE_YEEK */

	/* RACE_KLACKON */

	/* RACE_KOBOLD */

	/* RACE_NIBELUNG */

	/* RACE_DRACONIAN */

	/* RACE_MIND_FLAYER */

	/* RACE_IMP */

	/* RACE_GOLEM */

	/* RACE_SKELETON */

	/* RACE_ZOMBIE */

	/* RACE_VAMPIRE */

	/* RACE_SPECTRE */

	/* RACE_SPRITE */

	/* RACE_BEASTMAN */

	/* RACE_FIEND */
	{
		TV_RING, 			SV_RING_RES_FEAR,
		TV_RING,				SV_RING_SUSTAIN_STR,
		RACE_FIEND
	},
	{
		TV_BOW,	 			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_SWORD,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_POLEARM,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_HAFTED,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_CLOAK,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_GLOVES,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_BOOTS,			SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_LITE,				SV_ANY,
		0,						0,
		RACE_FIEND
	},
	{
		TV_CLOAK,			SV_ANY,
		0,						0,
		RACE_FIEND
	},

	/* Terminate */
	{
		0, 					0,
		0,						0,
		0
	}
};

static byte realm_to_tval[MAX_REALM] =
{
	0,
	TV_LIFE_BOOK,
	TV_SORCERY_BOOK,
	TV_NATURE_BOOK,
	TV_CHAOS_BOOK,
	TV_DEATH_BOOK,
	TV_TRUMP_BOOK,
	TV_ARCANE_BOOK,
};

static void race_conv_item(int *tval, int *sval)
{
	int i = 0;

	conv_equip_type *c_ptr;

	while(1)
	{
		/* Acquire this conversion */
		c_ptr = &equip_race_conv[i++];

		/* Stop if no more entries found */
		if (!c_ptr->tval1) break;

		/* Ignore incorrect race */
		if (c_ptr->tag != p_ptr->prace) continue;

		/* Ignore different conversions */
		if ((c_ptr->tval1 != *tval) ||
			 ((c_ptr->sval1 != SV_ANY) && (c_ptr->sval1 != *sval)))
		{
			continue;
		}

		/* Convert it */
		*tval = c_ptr->tval2;
		*sval = c_ptr->sval2;

		/* Done */
		break;
	}
}

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i, tv, sv;

	init_equip_type *i_ptr;

	object_type	object_type_body;
	object_type	*o_ptr = &object_type_body;

	i = 0;

	/* Give the player some useful objects based on their class */
	while (1)
	{
		/* Acquire this object */
		i_ptr = &equip_class_init[i++];

		/* Stop if no more entries found */
		if (!i_ptr->tval) break;

		/* Ignore incorrect class */
		if (i_ptr->tag != p_ptr->pclass) continue;

		tv = i_ptr->tval;
		sv = i_ptr->sval;

		switch(tv)
		{
			/* Hack -- initialize realm 1 */
			case TV_BOOK_REALM1:
			{
				tv = realm_to_tval[p_ptr->realm1];
				break;
			}
			/* Hack -- initialize realm 2 */
			case TV_BOOK_REALM2:
			{
				tv = realm_to_tval[p_ptr->realm2];
				break;
			}
		}

		/* Convert some useless objects */
		race_conv_item(&tv, &sv);

		/* Ignore useless objects */
		if (!tv) continue;

		/* Prepare the object */
		object_prep(o_ptr, lookup_kind(tv, sv));

		/* Roll the quantity */
		o_ptr->number = damroll(i_ptr->dice, i_ptr->side);

		/* Assassins begin the game with a poisoned dagger */
		if ((tv == TV_SWORD) &&
			 (p_ptr->pclass == CLASS_ROGUE) &&
			 (p_ptr->realm1 == REALM_DEATH))
		{
			o_ptr->name2 = EGO_BRAND_POIS;
		}

		/* Apply 'normal' magic */
		apply_magic(o_ptr, 0, FALSE, FALSE, FALSE);

		/* These objects are "storebought" and known */
		o_ptr->ident |= IDENT_STOREB | IDENT_MENTAL;

		object_aware(o_ptr);
		object_known(o_ptr);
		(void)inven_carry(o_ptr, FALSE);
	}

	i = 0;

	/* Give the player some useful objects based on their race */
	while (1)
	{
		/* Acquire this object */
		i_ptr = &equip_race_init[i++];

		/* Stop if no more entries found */
		if (!i_ptr->tval) break;

		/* Ignore incorrect race */
		if (i_ptr->tag != p_ptr->prace) continue;

		tv = i_ptr->tval;
		sv = i_ptr->sval;

		/* Prepare the object */
		object_prep(o_ptr, lookup_kind(tv, sv));

		/* Roll the quantity */
		o_ptr->number = damroll(i_ptr->dice, i_ptr->side);

		/* Apply 'normal' magic */
		apply_magic(o_ptr, 0, FALSE, FALSE, FALSE);

		/* These objects are "storebought" and known */
		o_ptr->ident |= IDENT_STOREB | IDENT_MENTAL;

		object_aware(o_ptr);
		object_known(o_ptr);
		(void)inven_carry(o_ptr, FALSE);
	}
}


static cptr sex_title(vptr what, s32b i)
{
	return ((player_sex *)what)[i].title;
}


static cptr race_title(vptr what, s32b i)
{
	return ((player_race *)what)[i].title;
}


static cptr class_title(vptr what, s32b i)
{
	return ((player_class *)what)[i].title;
}


static cptr realm_title(vptr what, s32b i)
{
	return realm_names[((int *)what)[i]];
}


static cptr patron_title(vptr what, s32b i)
{
	return ((cptr *)what)[i];
}


static errr player_choice(vptr what, s32b size, cptr name, cptr (*title_func)(vptr what, s32b i))
{
	s32b i, k;

	int y = 22 - size / 4;

	char c;

	char buf[80];

	for (i = 0; i < size; i++)
	{
		/* Display */
		sprintf(buf, "%c) %s", listsym[i], title_func(what, i));
		put_str(buf, y + (i / 4), 2 + 19 * (i % 4));
	}

	/* Choose */
	while (TRUE)
	{
		sprintf(buf, "Choose a %s (%c-%c) ('*' for random): ", name, listsym[0], listsym[i - 1]);
		put_str(buf, y - 1, 2);

		c = inkey();

		if (c == 'Q') quit(NULL);
		if (c == 'S') return (-1);
		if (c == '*')
		{
			k = rand_int(size);
			break;
		}

		k = strchr(listsym, c) - listsym;

		if ((k >= 0) && (k < size)) break;
		if (c == '?') do_cmd_help("help.hlp");
		else bell();
	}

	return (k);
}


static bool get_realms(void)
{
	int which[MAX_REALM];

	int i = 0;
	int k, choice;

	/* Start with no realms */
	p_ptr->realm1 = p_ptr->realm2 = 0;

	/* Warriors and certain others get no realms */
	if (realm_choices[p_ptr->pclass] == CH_NONE) return (TRUE);

	/* Extra info */
	Term_putstr(5, 10, -1, TERM_WHITE,
					"Your 'realm' determines which spells you may cast.");

	/* Other characters get at least one realm */
	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR_MAGE:
		{
			p_ptr->realm1 = REALM_ARCANE;
			return (TRUE);
		}
		case CLASS_CHAOS_WARRIOR:
		{
			p_ptr->realm1 = REALM_CHAOS;
			return (TRUE);
		}
		case CLASS_PRIEST:
		{
			which[i++] = REALM_LIFE;
			which[i++] = REALM_DEATH;
			break;
		}
		case CLASS_RANGER:
		{
			p_ptr->realm1 = REALM_NATURE;
			return (TRUE);
		}
		default:
		{
			choice = realm_choices[p_ptr->pclass];

	      if (choice & CH_LIFE) which[i++] = REALM_LIFE;
	      if (choice & CH_SORCERY) which[i++] = REALM_SORCERY;
	      if (choice & CH_NATURE) which[i++] = REALM_NATURE;
	      if (choice & CH_CHAOS) which[i++] = REALM_CHAOS;
	      if (choice & CH_DEATH) which[i++] = REALM_DEATH;
	      if (choice & CH_TRUMP) which[i++] = REALM_TRUMP;
	      if (choice & CH_ARCANE) which[i++] = REALM_ARCANE;
		}
	}

	/* Set realm */
	k = player_choice(which, i, "realm", realm_title);

	/* Restart */
	if (k < 0) return (FALSE);

	/* Set realm */
	p_ptr->realm1 = which[k];

	put_str("Magic       :", 6, 1);

	c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm1], 6, 15);

	clear_from(11);

	/* Some classes get two realms */
	if (!(realm_choices[p_ptr->pclass] & CH_TWO)) return (TRUE);

	i = 0;

	choice = realm_choices[p_ptr->pclass];

	switch (p_ptr->realm1)
	{
		case REALM_LIFE:
		{
			choice &= ~(CH_LIFE);
			break;
		}
		case REALM_SORCERY:
		{
			choice &= ~(CH_SORCERY);
			break;
		}
		case REALM_NATURE:
		{
			choice &= ~(CH_NATURE);
			break;
		}
		case REALM_CHAOS:
		{
			choice &= ~(CH_CHAOS);
			break;
		}
		case REALM_DEATH:
		{
			choice &= ~(CH_DEATH);
			break;
		}
		case REALM_TRUMP:
		{
			choice &= ~(CH_TRUMP);
			break;
		}
		case REALM_ARCANE:
		{
			choice &= ~(CH_ARCANE);
			break;
		}
	}

	if (choice & CH_LIFE) which[i++] = REALM_LIFE;
	if (choice & CH_SORCERY) which[i++] = REALM_SORCERY;
	if (choice & CH_NATURE) which[i++] = REALM_NATURE;
	if (choice & CH_CHAOS) which[i++] = REALM_CHAOS;
	if (choice & CH_DEATH) which[i++] = REALM_DEATH;
	if (choice & CH_TRUMP) which[i++] = REALM_TRUMP;
	if (choice & CH_ARCANE) which[i++] = REALM_ARCANE;

	/* Set realm */
	k = player_choice(which, i, "realm", realm_title);

	/* Restart */
	if (k < 0) return (FALSE);

	/* Set realm */
	p_ptr->realm2 = which[k];

	c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm2], 7, 15);

	return (TRUE);
}


/*
 * Helper function for 'player_birth()'
 */
static bool player_birth_aux(void)
{
	int i, k, v;

	int mode;

	bool save = FALSE;

	char buf[80];
	char inp[80];

	char c;

	/*** Intro ***/

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

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
					"Your 'sex' does not have any significant gameplay effects.");

	/* Set sex */
	k = player_choice(sex_info, MAX_SEXES, "sex", sex_title);

	/* Restart */
	if (k < 0) return (FALSE);

	p_ptr->psex = k;
	sp_ptr = &sex_info[p_ptr->psex];

	/* Display */
	c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);

	/* Clean up */
	clear_from(10);


	/*** Player race ***/

	/* Extra info */
	Term_putstr(5, 10, -1, TERM_WHITE,
					"Your 'race' determines various intrinsic factors and bonuses.");

	hack_mutation = FALSE;

	/* Set race */
	k = player_choice(race_info, MAX_RACES, "race", race_title);

	/* Restart */
	if (k < 0) return (FALSE);

	/* Set race */
	p_ptr->prace = k;
	rp_ptr = &race_info[p_ptr->prace];

	/* Display */
	c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 15);

	/* Clean up */
	clear_from(10);


	/*** Player class ***/

	/* Extra info */
	Term_putstr(5, 10, -1, TERM_WHITE,
					"Your 'class' determines various intrinsic abilities and bonuses.");

	/* Set race */
	k = player_choice(class_info, MAX_CLASS, "class", class_title);

	/* Restart */
	if (k < 0) return (FALSE);

	/* Set race */
	p_ptr->pclass = k;
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &magic_info[p_ptr->pclass];

	/* Display */
	c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

	/* Clean up */
	clear_from(10);

	/* Oops */
	if (!get_realms()) return (FALSE);

	clear_from(10);


	/*** Chaos patron ***/
	if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
	{
		put_str("Patron      :", 7, 1);

		/* Extra info */
		Term_putstr(5, 10, -1, TERM_WHITE,
						"Your 'patron' grants you certain abilities and may offer rewards.");

		/* Set race */
		k = player_choice(chaos_patrons, MAX_PATRON, "patron", patron_title);

		/* Restart */
		if (k < 0) return (FALSE);

		/* Set patron */
		p_ptr->chaos_patron = k;

		/* Display */
		c_put_str(TERM_L_BLUE, chaos_patrons[k], 7, 15);

		/* Clean up */
		clear_from(10);
	}
	else
	{
		p_ptr->chaos_patron = rand_int(MAX_PATRON);
	}


	/*** Preserve mode ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
					"Using 'preserve' mode makes it difficult to 'lose' artifacts,");
	Term_putstr(5, 16, -1, TERM_WHITE,
					"but eliminates the 'special' feelings about some levels.");

	p_ptr->preserve = get_check("Use 'preserve' mode? ");

	/* Clear */
	clear_from(15);


	/*** Autoroll ***/

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
					"The 'autoroller' allows you to specify stats based on 'weighting'.");

	for (i = 0; i < 6; i++)
	{
		stat_score[i] = 0;
	}

	/* Initialize */
	if (get_check("Use the autoroller? "))
	{
	   int max_score = 1;

		/* Clean up */
		clear_from(15);

		/* Prompt for the minimum stats */
		put_str("Enter score for: ", 15, 2);

		/* Prepare to ask for the stat scores */
		for (i = 0; i < 6; i++)
		{
			/* Prepare a prompt */
			sprintf(buf, "%-5s", stat_names[i]);

			/* Dump the prompt */
			put_str(buf, 16 + i, 5);
		}

		put_str("Social class:", 22, 5);

		/* Input the stat scores */
		for (i = 0; i < 6; i++)
		{
			/* Get a score */
			while (TRUE)
			{
				/* Move the cursor */
				put_str("", 16 + i, 30);

				/* Default */
				strcpy(inp, "");

				/* Get a response (or escape) */
				if (!askfor_aux(inp, 2)) inp[0] = '\0';

				/* Hack -- Extract an input */
				v = atoi(inp);

				/* Break on valid input */
				if ((v >= 0) && (v <= 10)) break;
			}

			/* Save the score */
			stat_score[i] = v;

	      /* Track maximum score */
	      max_score += v;
		}

		/* Move the cursor */
		put_str("", 22, 30);

		/* Clear the input */
		strcpy(inp, "");

		/* Get a response (or escape) */
		if (!askfor_aux(inp, 2)) inp[0] = '\0';

		/* Extract the number */
		v = atoi(inp);

		/* Save the social class score */
		sc_score = RNG(0, v, 10);

		/* Track maximum score */
		max_score += sc_score;

	   /* Normalize stats */
		for (i = 0; i < 6; i++)
		{
			stat_score[i] = stat_score[i] * 100 / max_score;
		}

	   /* Normalize social class */
	   sc_score = sc_score * 100 / max_score;
	}

	/* Clean up */
	clear_from(10);


	/*** User enters number of quests ***/
	/* Heino Vander Sanden and Jimmy De Laet */

	/* Extra info */
	Term_putstr(5, 15, -1, TERM_WHITE,
					"You can perform additional quests along with the two obligatory quests:");
	Term_putstr(5, 16, -1, TERM_WHITE,
					"Oberon, on level 99; and the Serpent of Chaos, on level 100.");

	/* Ask the number of additional quests */
	while (TRUE)
	{
		put_str(format("Number of additional quests? (<%u) ", MAX_Q_IDX - DEFAULT_QUESTS + 1), 20, 2);

		/* Get a the number of additional quest */
		while (TRUE)
		{
			/* Move the cursor */
			put_str("", 20, 38);

			/* Default */
			strcpy(inp, "20");

			/* Get a response (or escape) */
			if (!askfor_aux(inp, 3)) inp[0] = '\0';
			v = atoi(inp);

			/* Break on valid input */
			if ((v < MAX_Q_IDX - DEFAULT_QUESTS + 1) && (v >= 0)) break;
		}
		break;
	}

	total_quests = v + DEFAULT_QUESTS;

	/* Clear */
	clear_from(15);

	/* Generate quests */
	player_birth_quests();


	/*** Generate ***/

	while (TRUE)
	{
		int total_best = 0;
	   int count_best = 0;

		/* Autoroll */
		for (i = 0; i < 1000; i++)
		{
			int total = 0;
			int j;

			/* Get a new character */
			get_stats();

			/* Check and count acceptable stats */
			for (j = 0; j < 6; j++)
			{
				/* This stat is okay */
				total += stat_use[j] * stat_score[j];
			}

			/* Roll for social class */
			get_history();

			/* Accept good social class */
			total += p_ptr->sc * sc_score;

			/* Done if we don't care what the roll is */
			if (!total) break;

	      /* Ignore low rolls */
			if (total < total_best) continue;

	      /* Choose a random duplicate */
			if ((total == total_best) && rand_int(++count_best)) continue;

			total_best = total;

			/* Save the data */
	      save_prev_data();
		}

	   /* Load the data if it exists */
		if (total_best)
		{
	      save = FALSE;
			load_prev_data();
		}

		/* Roll for base hitpoints */
		get_extra();

		/* Roll for age/height/weight */
		get_ahw();

		/* Roll for gold */
		get_money();

	   /* Clear mutations */
		p_ptr->muta1 = 0;
		p_ptr->muta2 = 0;
		p_ptr->muta3 = 0;


		/*** Display ***/

		/* Mode */
		mode = 1;

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
			Term_gotoxy(2, 23);
			Term_addstr(-1, TERM_WHITE, "['r' to reroll");
			if (save) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
			if (mode) Term_addstr(-1, TERM_WHITE, ", 'h' for Misc.");
			else Term_addstr(-1, TERM_WHITE, ", 'h' for History");
			Term_addstr(-1, TERM_WHITE, ", or ESC to accept]");

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
			if (save && (c == 'p'))
			{
				load_prev_data();
				continue;
			}

			/* Toggle the display */
			if ((c == 'H') || (c == 'h'))
			{
				mode = !mode;
				continue;
			}

			/* Help */
			if (c == '?')
			{
				do_cmd_help("help.hlp");
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
		save = TRUE;
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

	/* Mutate beastmen */
	if (p_ptr->prace == RACE_BEASTMAN)
	{
		hack_mutation = TRUE;
	}

	/* Imps have claws */
	if (p_ptr->prace == RACE_IMP)
	{
		p_ptr->muta2 |= MUT2_CLAWS;
	}

	/* Check for patron-granted powers */
	if (p_ptr->pclass == CLASS_CHAOS_WARRIOR)
	{
		switch(p_ptr->chaos_patron)
		{
			/* Resist magic */
			case PATRON_SLORTAR:
			{
				p_ptr->muta3 |= MUT3_MAGIC_RES;
				break;
			}
			/* Masked appearance */
			case PATRON_MABELODE:
			{
				p_ptr->muta3 |= MUT3_ILL_NORM;
				break;
			}
			/* Random telepathy and warning */
			case PATRON_PYARAY:
			{
				p_ptr->muta2 |= MUT2_WEIRD_MIND | MUT2_WARNING;
				break;
			}
			/* Detect curse */
			case PATRON_BALAAN:
			{
				p_ptr->muta1 |= MUT1_DET_CURSE;
				break;
			}
			/* Random dispel all */
			case PATRON_EEQUOR:
			{
				p_ptr->muta2 |= MUT2_DISPEL_ALL;
				break;
			}
			/* Random mutation */
			case PATRON_BALO:
			{
				gain_random_mutation(0);
				break;
			}
			/* Random berserk */
			case PATRON_KHORNE:
			{
				p_ptr->muta2 |= MUT2_BERS_RAGE;
				break;
			}
			/* Random polymorph wounds */
			case PATRON_NURGLE:
			{
				p_ptr->muta2 |= MUT2_POLY_WOUND;
				break;
			}
			/* Augmented intelligence */
			case PATRON_TZEENTCH:
			{
				p_ptr->muta3 |= MUT3_HYPER_INT;
				break;
			}
			/* Random chaos */
			case PATRON_KHAINE:
			{
				p_ptr->muta2 |= MUT2_RAW_CHAOS;
				break;
			}
			/* Summon monsters */
			case PATRON_QLZQQLZUUP:
			{
				p_ptr->muta1 |= MUT1_SUMMON_M;
				break;
			}
			/* Mind blast */
			case PATRON_VECNA:
			{
				p_ptr->muta1 |= MUT1_MIND_BLST;
				break;
			}
			/*
			 * Pistol, shotgun, double shotgun, chaingun,
			 * rocket launcher, plasma cannon, BFG
			 * And...
			 * Random attract demon, berserk, dispel all (BFG!),
			 * or invulnerability
			 */
			case PATRON_ID:
			{
				p_ptr->muta1 |= MUT1_SHARD_BOLT | MUT1_SHARD_BLAST | MUT1_DSHARD_BLAST |
									 MUT1_CHAIN_SHARDS | MUT1_ROCKET | MUT1_PLAS_BOLT | MUT1_BFG;

				switch(rand_int(13))
				{
					case 0:	case 1:	case 2:	case 3:	case 4:
					{
						p_ptr->muta2 |= MUT2_ATT_DEMON;
						break;
					}
					case 5:	case 6:	case 7:
					{
						p_ptr->muta2 |= MUT2_BERS_RAGE;
						break;
					}
					case 8:
					{
						p_ptr->muta2 |= MUT2_DISPEL_ALL;
						break;
					}
					case 9:
					{
						p_ptr->muta2 |= MUT2_INVULN;
						break;
					}
				}
			}
		}
	}

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

	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* No inventory */
		total_weight = 0;

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

		/* Ignore home */
		if (n == 7) continue;

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(n);
	}
}

