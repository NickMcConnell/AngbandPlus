/* File: generate.c */

/*
 * Dungeon generation
 *
 * Code for making, stocking, and populating levels when generated.  Includes
 * rooms of every kind, pits, vaults (inc. interpretation of vault.txt), seams
 * tunneling, etc.  Level feelings, essence generation.  Creation of the town.
 *
 * Copyright (c) 2007
 * Leon Marrick, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * Level generation is not an important bottleneck, though it can be
 * annoyingly slow on older machines...  Thus we emphasize simplicity
 * and correctness over speed.  See individual functions for notes.
 *
 * This entire file is only needed for generating levels.
 * This may allow smart compilers to only load it when needed.
 *
 * The "vault.txt" file is used to store vault generation info.
 */



/*
 * Option to allow unlimited pits, vaults, and huge rooms (debug mode)
 */
#define UNLIMITED_SPECIAL_ROOMS   FALSE

/*
 * Dungeon generation values
 */
#define DUN_ROOMS            30  /* Number of rooms to attempt */
#define DEST_LEVEL_CHANCE    30  /* 1/chance of being a destroyed level */

/*
 * Dungeon tunnel generation values
 */
#define DUN_TUN_RND          30  /* 1 in # chance of random direction */
#define DUN_TUN_ADJ          10  /* 1 in # chance of adjusting direction */
#define DUN_TUN_PEN          50  /* Chance of doors at room entrances */
#define DUN_TUN_JCT          30  /* Chance of doors at tunnel junctions */

/*
 * Dungeon seam generation values
 */
#define DUN_STR_MAG           8  /* Number of magma seams */
#define DUN_STR_MC          160  /* 1/chance of treasure per magma */
#define DUN_STR_QUA           4  /* Number of quartz seams */
#define DUN_STR_QC           80  /* 1/chance of treasure per quartz */

/*
 * Dungeon treasure allocation values
 */
#define DUN_AMT_ROOM          9  /* Amount of objects for rooms */
#define DUN_AMT_ITEM          2  /* Amount of objects for rooms/corridors */
#define DUN_AMT_GOLD          3  /* Amount of treasure for rooms/corridors */

/*
 * Hack -- Dungeon allocation "places"
 */
#define ALLOC_SET_CORR        1  /* Hallway */
#define ALLOC_SET_ROOM        2  /* Room */
#define ALLOC_SET_BOTH        3  /* Anywhere */

/*
 * Hack -- Dungeon allocation "types"
 */
#define ALLOC_TYP_RUBBLE      1  /* Rubble */
#define ALLOC_TYP_LOOSE_ROCK  2  /* Loose rocks */
#define ALLOC_TYP_TRAP        3  /* Trap */
#define ALLOC_TYP_GOLD        4  /* Gold */
#define ALLOC_TYP_OBJECT      5  /* Object */
#define ALLOC_TYP_BOULDER     6  /* Boulder */
#define ALLOC_TYP_FOOD        7  /* Food */

/*
 * Maximal number of room types
 */
#define ROOM_MAX	11

/*
 * Bounds on some arrays used in the "dun_data" structure.
 * These bounds are checked, though usually this is a formality.
 */
#define CENT_MAX      DUN_ROOMS
#define DOOR_MAX            100
#define WALL_MAX             40
#define TUNN_MAX            300
#define STAIR_MAX            30

/*
 * Simple structure to hold a map location
 */

typedef struct coord coord;

struct coord
{
	byte y;
	byte x;
};

/*
 * Structure to hold all dungeon generation data
 */

typedef struct dun_data dun_data;

struct dun_data
{
	/* Array of centers of rooms */
	s16b cent_n;
	coord cent[CENT_MAX];

	/* Array to store whether rooms are connected or not. */
	bool connected[CENT_MAX];

	/* Array of possible door locations */
	s16b door_n;
	coord door[DOOR_MAX];

	/* Array of wall piercing locations */
	s16b wall_n;
	coord wall[WALL_MAX];

	/* Array of tunnel grids */
	s16b tunn_n;
	coord tunn[TUNN_MAX];

	/* Array of good potential stair grids */
	s16b stair_n;
	coord stair[STAIR_MAX];

	/* Number of blocks along each axis */
	s16b row_rooms;
	s16b col_rooms;

	/* Array to store block usage */
	s16b room_map[DUNGEON_HGT_MAX/BLOCK_HGT][DUNGEON_WID_MAX/BLOCK_WID];

	/* Number of each type of room on this level */
	s16b room_num[ROOM_MAX];
};

/*
 * Dungeon generation data -- see "cave_gen()"
 */
static dun_data *dun;


/*
 * Room type information
 */
typedef struct room_data room_data;

struct room_data
{
	/* Allocation information. */
	s16b room_gen_num[ROOM_MAX];

	/* Minimum level on which room can appear. */
	byte min_level;
};

/*
 * Table of values that control how many times each type of room will,
 * on average, appear on 100 levels at various depths.  Each type of room
 * has its own row, and each column corresponds to dungeon levels 0, 10,
 * 20, and so on.  The final value is the minimum depth the room can appear
 * at.  -LM-
 *
 * Level 101 and below use the values for level 100.
 *
 * Rooms with lots of monsters or loot may not be generated if the object or
 * monster lists are already nearly full.  Rooms will not appear above their
 * minimum depth.  No type of room (other than type 1) can appear more than
 * DUN_ROOMS/2 times in any level.  Tiny levels will not have space for all
 * the rooms you ask for.
 *
 * The entries for room type 1 are blank because these rooms are built once
 * all other rooms are finished -- until the level fills up, or the room
 * count reaches the limit (DUN_ROOMS).
 */
static room_data room[ROOM_MAX] =
{
   /* Depth:         0   10   20   30   40   50   60   70   80   90  100  min */

   /* Nothing */  {{ 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0},  0},
   /* Simple */   {{ 0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0},  0},
   /* Overlap */  {{70, 120, 150, 190, 200, 200, 200, 200, 200, 200, 200},  1},
   /* Large */    {{ 0,  30,  60,  80,  90,  95, 100, 100, 100, 100, 100},  3},
   /* Large */    {{ 0,  30,  60,  80,  90,  95, 100, 100, 100, 100, 100},  3},
   /* Pit */      {{ 0,   6,  20,  30,  35,  40,  40,  40,  40,  40,  40},  7},
   /* Chambers */ {{ 0,   2,   6,  12,  15,  18,  19,  20,  20,  20,  20},  7},
   /* I. Room */  {{30,  60,  70,  80,  80,  75,  70,  67,  65,  62,  60},  0},
   /* L. Vault */ {{ 0,   1,   4,   9,  16,  27,  40,  55,  70,  80,  90},  7},
   /* G. Vault */ {{ 0,   0,   1,   2,   3,   4,   6,   7,   8,  10,  12}, 20},
   /* Huge     */ {{ 0,   0,   0,   0,   4,   4,   4,   4,   4,   4,   4}, 41}
};


/* Build rooms in descending order of difficulty. */
static byte room_build_order[ROOM_MAX] = {10, 9, 6, 8, 5, 7, 4, 3, 2, 1, 0};


/*
 * This table takes a depth, and returns a suitable monster symbol.  Depth
 * input is assumed to be player depth.  It is also assumed that monsters
 * can be generated slightly out of depth.  -LM-
 *
 * Depths greater than 60 should map to the row for level 60.
 *
 * Monster pits use the first seven columns, and rooms of chambers use all
 * thirteen.  Because rooms of chambers are about half as common as monster
 * pits, and have fewer monsters, creatures in the first seven columns will
 * be much more common than those in the last six.  On the other hand,
 * monsters in rooms of chambers have a lot more maneuver room...
 *
 * - Symbol '*' is any creature of a randomly-chosen racial char.
 * - Symbol '0' is any animal.
 *
 * - Symbol '1' is any insect ('a', 'c', 'l', 'F', 'I', 'K').
 * - Symbol '2' is any naga, snake, or other reptile.
 * - Symbol '3' is any jelly, mold, icky thing ('i', 'j', or 'm').
 *
 * - Symbol '%' is any orc, ogre, troll, or giant.
 * - Symbol 'U' is an underground creature.
 * - Symbol 'N' is any undead.  Upon occasion, deep in the dungeon, the
 *   racial type may be forced to 'G', 'L', 'V', or 'W'.
 * - Symbols 'p' and 'h' may sometimes be found in combination.  If they
 *   are, they are usually all of a given class (magical, pious, natural,
 *   assassination/thievery, or warrior)
 * - Symbols 'E' and 'v' may be found in combination.  If they are, they
 *   will always be of a given elemental type.
 * - Symbols 'd' and 'D' both mean dragons of either char.  Dragons are
 *   often of a particular type (blue, red, gold, shadow/ethereal etc.).
 * - Symbols 'I' and '&' may mean lesser demons, greater demons, or both,
 *   depending on depth.
 *
 * - Other symbols usually represent specific racial characters.
 */
static char mon_symbol_at_depth[12][13] =
{
	/* Levels 5, 10, 15, and 20 */
	{'0', '0', '3', '3', '3', 'k', 'y',   '*', '*', '1', 'a', '2', 'S' },
	{'0', '3', '3', 'o', 'o', 'N', '1',   '*', 'C', 'f', 'a', '2', 'S' },
	{'0', '3', 'o', 'o', 'o', '&', '*',   '%', '%', 'S', 'E', '2', 'Z' },
	{'0', '3', '3', 'o', 'T', 'T', 'P',   'p', 'h', 'f', '%', '*', 'Z' },

	/* Levels 25, 30, 35, and 40 */
	{'0', '3', 'T', 'T', '&', 'P', 'P',   'p', 'v', 'd', '1', 'S', '2' },
	{'0', '0', 'T', 'P', 'P', 'N', 'd',   'p', 'h', 'f', 'v', 'g', 'Z' },
	{'0', '3', 'T', 'P', 'N', '&', 'd',   'p', 'H', 'E', '1', '*', '2' },
	{'0', '0', 'T', 'P', 'N', '&', 'd',   'p', 'h', 'g', 'E', '*', 'Z' },

	/* Levels 45, 50, 55, and 60+ */
	{'0', 'P', 'N', '&', '&', 'd', '*',   'p', 'h', 'v', 'E', '*', 'Z' },
	{'0', 'N', 'N', '&', '&', 'D', '*',   'p', 'h', '*', 'T', 'B', 'Z' },
	{'0', 'N', 'N', '&', '&', 'D', '*',   'p', 'h', 'W', 'G', '*', 'Z' },
	{'0', 'N', 'N', '&', '&', 'D', '*',   'p', 'h', 'v', '*', 'D', 'Z' }
};

/*
 * Restrictions on monsters, used in pits, vaults, and chambers.
 */
static bool allow_unique;
static char d_char_req[10];
static byte d_attr_req[4];
static u32b racial_flag_mask;
static u32b breath_flag_mask;



/*
 * Table of monster descriptions.  Used to make descriptions for kinds
 * of pits and rooms of chambers that have no special names.
 */
cptr d_char_req_desc[] =
{
	"A:angel",
	"B:bird",
	"C:canine",
	"D:dragon",
	"E:elemental",
	"F:fly or dragon fly",
	"G:ghost",
	"H:hybrid",
	"I:demon",
	"J:snake",
	"K:killer beetle",
	"L:lich",
	"M:mummy",
	"O:ogre",
	"P:giant",
	"Q:quylthulg",
	"R:reptile",
	"S:spider",
	"T:troll",
	"V:vampire",
	"W:wraith",
	"Y:yeti",
	"Z:zephyr hound",
	"a:ant",
	"b:bat",
	"c:centipede",
	"d:dragon",
	"e:floating eye",
	"f:feline",
	"g:golem",
	"h:humanoid",
	"i:icky thing",
	"j:jelly",
	"k:kobold",
	"l:louse",
	"m:mold",
	"n:naga",
	"o:orc",
	"p:human",
	"q:quadruped",
	"r:rodent",
	"s:skeleton",
	"t:townsperson",
	"v:vortex",
	"w:worm",
	"y:yeek",
	"z:zombie",
	",:mushroom patch",
	"&:demon",
	NULL
};




/**************************************************************/
/*                                                            */
/*                 The monster-selection code                 */
/*                                                            */
/**************************************************************/


/*
 * Use various selection criteria (set elsewhere) to restrict monster
 * generation.
 *
 * This function is capable of selecting monsters by:
 *   - racial symbol (may be any of the characters allowed)
 *   - symbol color (may be any of up to four colors).
 *   - racial flag(s) (monster may have any allowed flag)
 *   - breath flag(s) (monster must have exactly the flags specified)
 *
 * Uniques may be forbidden, or allowed on rare occasions.
 *
 * Some situations may require special processing; this should be
 * done in helper functions called from this one.
 */
static bool mon_select(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	bool ok = FALSE;

	/* Require that the monster symbol be correct. */
	if (d_char_req[0] != '\0')
	{
		if (strchr(d_char_req, r_ptr->d_char) == 0) return (FALSE);
	}

	/* Require correct racial type. */
	if (racial_flag_mask)
	{
		if (!(r_ptr->flags3 & (racial_flag_mask))) return (FALSE);

		/* Hack -- no invisible undead until deep. */
		if ((p_ptr->depth < 40) && (r_ptr->flags3 & (RF3_UNDEAD)) &&
			(r_ptr->flags2 & (RF2_INVISIBLE))) return (FALSE);
	}

	/* Require that monster breaths be exactly those specified. */
	if (breath_flag_mask)
	{
		if (r_ptr->flags4 != breath_flag_mask) return (FALSE);
	}

	/* Require that the monster color be correct. */
	if (d_attr_req[0])
	{
		/* Check all allowed colors, if given. */
		if ((d_attr_req[0]) && (r_ptr->d_attr == d_attr_req[0])) ok = TRUE;
		if ((d_attr_req[1]) && (r_ptr->d_attr == d_attr_req[1])) ok = TRUE;
		if ((d_attr_req[2]) && (r_ptr->d_attr == d_attr_req[2])) ok = TRUE;
		if ((d_attr_req[3]) && (r_ptr->d_attr == d_attr_req[3])) ok = TRUE;

		/* Hack -- No multihued dragons allowed in the arcane dragon pit. */
		if ((strchr(d_char_req, 'd') || strchr(d_char_req, 'D')) &&
			(d_attr_req[0] == TERM_L_PURPLE) &&
			(r_ptr->flags4 == (RF4_BRTH_ACID |
				RF4_BRTH_ELEC | RF4_BRTH_FIRE |
				RF4_BRTH_COLD | RF4_BRTH_POIS)))
		{
			return (FALSE);
		}

		/* Doesn't match any of the given colors?  Not good. */
		if (!ok) return (FALSE);
	}

	/* Usually decline unique monsters. */
	if (r_ptr->flags1 & (RF1_UNIQUE))
	{
		if (!allow_unique) return (FALSE);
		else if (!one_in_(5)) return (FALSE);
	}

	/* Okay */
	return (TRUE);
}

/*
 * Accept characters representing a race or group of monsters and
 * an (adjusted) depth, and use these to set values for required racial
 * type, monster symbol, monster symbol color, and breath type.  -LM-
 *
 * This function is called to set restrictions, point the monster
 * allocation function to "mon_select()", and remake monster allocation.
 * It undoes all of this things when called with the symbol '\0'.
 *
 * Describe the monsters (used by cheat_room) and determine if they
 * should be neatly ordered or randomly placed (used in monster pits).
 */
static char *mon_restrict(char symbol, byte depth, bool *ordered,
	bool unique_ok)
{
	int i, j;

	/* Assume no definite name */
	char name[DESC_LEN];

	/* Clear global monster restriction variables. */
	allow_unique = unique_ok;
	for (i = 0; i < 10; i++) d_char_req[i] = '\0';
	for (i = 0; i < 4; i++) d_attr_req[i] = 0;
	racial_flag_mask = 0; breath_flag_mask = 0;


	/* Initialize description */
	strcpy(name, "misc");

	/* No symbol, no restrictions. */
	if (symbol == '\0')
	{
		get_mon_num_hook = NULL;
		get_mon_num_prep();
		return (format("%s", name));
	}

	/* Handle the "wild card" symbol '*'  */
	if (symbol == '*')
	{
		for (i = 0; i < 2500; i++)
		{
			/* Get a random monster. */
			j = randint(z_info->r_max - 1);

			/* Must be a real monster */
			if (!r_info[j].rarity) continue;

			/* Try for close to depth, accept in-depth if necessary */
			if (i < 200)
			{
				if ((!(r_info[j].flags1 & RF1_UNIQUE)) &&
				      (r_info[j].level != 0) &&
				      (r_info[j].level <= depth) &&
				      (ABS(r_info[j].level - p_ptr->depth) <
				      1 + (p_ptr->depth / 4))) break;
			}
			else
			{
				if ((!(r_info[j].flags1 & RF1_UNIQUE)) &&
				      (r_info[j].level != 0) &&
				      (r_info[j].level <= depth)) break;
			}
		}

		/* We've found a monster. */
		if (i < 2499)
		{
			/* ...use that monster's symbol for all monsters. */
			symbol = r_info[j].d_char;
		}
		else
		{
			/* Paranoia - pit stays empty if no monster is found */
			return (NULL);
		}
	}

	/* Apply monster restrictions according to symbol. */
	switch (symbol)
	{
		/* All animals */
		case '0':
		{
			strcpy(name, "animal");
			racial_flag_mask = RF3_ANIMAL;
			*ordered = FALSE;
			break;
		}

		/* Insects */
		case '1':
		{
			strcpy(name, "insect");
			strcpy(d_char_req, "aclFIK");
			*ordered = FALSE;
			break;
		}

		/* Reptiles */
		case '2':
		{
			strcpy(name, "reptile");
			strcpy(d_char_req, "nJR");
			*ordered = FALSE;
			break;
		}

		/* Jellies, etc. */
		case '3':
		{
			strcpy(name, "jelly");
			strcpy(d_char_req, "ijm,");
			*ordered = FALSE;
			break;
		}

		/* Humans and humanoids */
		case 'p':
		case 'h':
		{
			/* 'p's and 'h's can coexist. */
			if (one_in_(3))
			{
				strcpy(d_char_req, "ph");

				/* If so, they will usually all be of similar classes. */
				if (!one_in_(4))
				{
					/* Randomizer. */
					i = rand_int(5);

					/* Magicians and necromancers */
					if (i == 0)
					{
						d_attr_req[0] = TERM_RED;
						d_attr_req[1] = TERM_L_RED;
						d_attr_req[2] = TERM_L_PURPLE;
						strcpy(name, "school of sorcery");
					}
					/* Priests and paladins */
					else if (i == 1)
					{
						d_attr_req[0] = TERM_GREEN;
						d_attr_req[1] = TERM_L_GREEN;
						d_attr_req[2] = TERM_WHITE;
						d_attr_req[3] = TERM_L_WHITE;
						strcpy(name, "temple of piety");
					}
					/* Druids and ninjas */
					else if (i == 2)
					{
						d_attr_req[0] = TERM_ORANGE;
						d_attr_req[1] = TERM_YELLOW;
						strcpy(name, "gathering of nature");
					}
					/* Thieves and assassins */
					else if (i == 3)
					{
						d_attr_req[0] = TERM_BLUE;
						d_attr_req[1] = TERM_L_BLUE;
						d_attr_req[2] = TERM_SLATE;
						d_attr_req[3] = TERM_L_DARK;
						strcpy(name, "den of thieves");
					}
					/* Warriors and rangers */
					else
					{
						d_attr_req[0] = TERM_UMBER;
						d_attr_req[1] = TERM_L_UMBER;
						strcpy(name, "fighter's hall");
					}
				}
				else
				{
					strcpy(name, "humans and humanoids");
				}
			}

			/* Usually, just accept the symbol. */
			else
			{
				d_char_req[0] = symbol;

				if (symbol == 'p') strcpy(name, "human");
				else if (symbol == 'h') strcpy(name, "humanoid");
			}

			*ordered = FALSE;
			break;
		}

		/* Orcs */
		case 'o':
		{
			strcpy(name, "orc");
			strcpy(d_char_req, "o");
			*ordered = TRUE;
			break;
		}

		/* Trolls */
		case 'T':
		{
			strcpy(name, "troll");
			strcpy(d_char_req, "T");
			*ordered = TRUE;
			break;
		}

		/* Giants (sometimes ogres at low levels) */
		case 'P':
		{
			strcpy(name, "giant");
			if ((p_ptr->depth < 30) && (one_in_(3)))
			     strcpy(d_char_req, "O");
			else strcpy(d_char_req, "P");
			*ordered = TRUE;
			break;
		}

		/* Orcs, ogres, trolls, or giants */
		case '%':
		{
			strcpy(name, "moria");
			strcpy(d_char_req, "oOPT");
			*ordered = FALSE;
			break;
		}

		/* Monsters found in caves */
		case 'U':
		{
			strcpy(name, "dungeon monsters");
			strcpy(d_char_req, "ykoOT");
			*ordered = FALSE;
			break;
		}

		/* Undead */
		case 'N':
		{
			/* Sometimes, restrict by symbol. */
			if ((depth > 40) && (one_in_(3)))
			{
				for (i = 0; i < 500; i++)
				{
					/* Find a suitable monster near depth. */
					j = randint(z_info->r_max - 1);

					/* Require a non-unique undead. */
					if ((r_info[j].flags3 & RF3_UNDEAD) &&
					    (!(r_info[j].flags1 & RF1_UNIQUE)) &&
					    (strchr("GLWV", r_info[j].d_char)) &&
					    (ABS(r_info[j].level - p_ptr->depth) <
					    1 + (p_ptr->depth / 4)))
					{
						break;
					}
				}

				/* If we find a monster, */
				if (i < 499)
				{
					/* Use that monster's symbol for all monsters */
					d_char_req[0] = r_info[j].d_char;

					/* No pit name (yet) */

					/* In this case, we do order the monsters */
					*ordered = TRUE;
				}
				else
				{
					/* Accept any undead. */
					strcpy(name, "undead");
					racial_flag_mask = RF3_UNDEAD;
					*ordered = FALSE;
				}
			}
			else
			{
				/* No restrictions on symbol. */
				strcpy(name, "undead");
				racial_flag_mask = RF3_UNDEAD;
				*ordered = FALSE;
			}
			break;
		}

		/* Demons */
		case 'I':
		case '&':
		{
			strcpy(name, "demon");

			if (depth <= 40) strcpy(d_char_req, "I");
			else if (depth >= 50) strcpy(d_char_req, "&");
			else racial_flag_mask = RF3_DEMON;

			*ordered = TRUE;
			break;
		}

		/* Dragons */
		case 'd':
		case 'D':
		{
			strcpy(d_char_req, "dD");

			/* Dragons usually associate with others of their kind. */
			if (!one_in_(6))
			{
				/* Dragons of a single kind are ordered. */
				*ordered = TRUE;

				/* Some dragon types are not found everywhere */
				if (depth > 70) i = rand_int(35);
				else if (depth > 45) i = rand_int(32);
				else if (depth > 32) i = rand_int(30);
				else if (depth > 23) i = rand_int(28);
				else i = rand_int(24);

				if (i < 4)
				{
					breath_flag_mask = (RF4_BRTH_ACID);
					strcpy(name, "dragon - acid");
				}
				else if (i < 8)
				{
					breath_flag_mask = (RF4_BRTH_ELEC);
					strcpy(name, "dragon - electricity");
				}
				else if (i < 12)
				{
					breath_flag_mask = (RF4_BRTH_FIRE);
					strcpy(name, "dragon - fire");
				}
				else if (i < 16)
				{
					breath_flag_mask = (RF4_BRTH_COLD);
					strcpy(name, "dragon - cold");
				}
				else if (i < 20)
				{
					breath_flag_mask = (RF4_BRTH_POIS);
					strcpy(name, "dragon - poison");
				}
				else if (i < 24)
				{
					breath_flag_mask = (RF4_BRTH_ACID |
					    RF4_BRTH_ELEC | RF4_BRTH_FIRE |
					    RF4_BRTH_COLD | RF4_BRTH_POIS);
					strcpy(name, "dragon - multihued");
				}
				else if (i < 26)
				{
					breath_flag_mask = (RF4_BRTH_CONFU);
					strcpy(name, "dragon - confusion");
				}
				else if (i < 28)
				{
					breath_flag_mask = (RF4_BRTH_SOUND);
					strcpy(name, "dragon - sound");
				}
				else if (i < 30)
				{
					breath_flag_mask = (RF4_BRTH_LITE |
					                    RF4_BRTH_DARK);
					strcpy(name, "dragon - ethereal");
				}

				/* Chaos, Law, Balance, Power, etc.) */
				else
				{
					d_attr_req[0] = TERM_L_PURPLE;
					d_attr_req[1] = TERM_L_BLUE;
					d_attr_req[2] = TERM_L_GREEN;
					strcpy(name, "dragon - arcane");
				}
			}
			else
			{
				strcpy(name, "dragon - mixed");

				/* Dragons of all kinds are not ordered. */
				*ordered = FALSE;
			}
			break;
		}

		/* Vortexes and elementals */
		case 'v':
		case 'E':
		{
			/* Usually, just have any kind of 'v' or 'E' */
			if (!one_in_(3))
			{
				d_char_req[0] = symbol;

				if (symbol == 'v') strcpy(name, "vortex");
				if (symbol == 'E') strcpy(name, "elemental");
			}

			/* Sometimes, choose both 'v' and 'E's of one element */
			else
			{
				strcpy(d_char_req, "vE");

				i = rand_int(4);

				/* Fire */
				if (i == 0)
				{
					d_attr_req[0] = TERM_RED;
					strcpy(name, "fire");
				}
				/* Frost */
				if (i == 1)
				{
					d_attr_req[0] = TERM_L_WHITE;
					d_attr_req[1] = TERM_WHITE;
					strcpy(name, "frost");
				}
				/* Air/electricity */
				if (i == 2)
				{
					d_attr_req[0] = TERM_L_BLUE;
					d_attr_req[1] = TERM_BLUE;
					strcpy(name, "air");
				}
				/* Acid/water/earth */
				if (i == 3)
				{
					d_attr_req[0] = TERM_GREEN;
					d_attr_req[1] = TERM_L_UMBER;
					d_attr_req[2] = TERM_UMBER;
					d_attr_req[3] = TERM_SLATE;
					strcpy(name, "earth & water");
				}
			}

			*ordered = FALSE;
			break;
		}

		/* Special case:  mimics and treasure */
		case '!':
		case '?':
		case '=':
		case '~':
		case '|':
		case '.':
		case '$':
		{
			/* Note that we never have chest monsters */
			if (symbol == '$')
			{
				strcpy(name, "treasure");

				/* Nothing but loot! */
				if (one_in_(3)) strcpy(d_char_req, "$");

				/* Guard the money well. */
				else strcpy(d_char_req, "$!?=|.");
			}
			else
			{
				/* No treasure. */
				strcpy(d_char_req, "!?=|.");
				strcpy(name, "mimic");
			}

			*ordered = FALSE;
			break;
		}

		/* Special case:  creatures of earth. */
		case 'X':
		case '#':
		{
			strcpy(d_char_req, "X#");
			strcpy(name, "creatures of earth");
			*ordered = FALSE;
			break;
		}

		/* Water creatures (also acid and electricity). */
		case '6':
		{
			allow_unique = TRUE;
			strcpy(d_char_req, "vEZn");
			d_attr_req[0] = TERM_SLATE;
			d_attr_req[1] = TERM_BLUE;
			break;
		}

		/* Beings of fire or ice. */
		case '7':
		{
			allow_unique = TRUE;
			strcpy(d_char_req, "vE");
			if (one_in_(2)) d_attr_req[0] = TERM_RED;
			else
			{
				d_attr_req[0] = TERM_L_WHITE;
				d_attr_req[1] = TERM_WHITE;
			}

			break;
		}



		/* Space for more monster types here. */


		/* Any symbol not handled elsewhere. */
		default:
		{
			/* Accept the character. */
			d_char_req[0] = symbol;

			/* Some monsters should logically be ordered. */
			if (strchr("knosuyzGLMOPTUVW", symbol)) *ordered = TRUE;

			/* Most should not */
			else *ordered = FALSE;

			break;
		}
	}

	/* If monster pit hasn't been named already, get a name. */
	if (streq(name, "misc"))
	{
		/* Search a table for a description of the symbol */
		for (i = 0; d_char_req_desc[i]; ++i)
		{
			if (symbol == d_char_req_desc[i][0])
			{
				/* Get all but the 1st 2 characters of the text. */
				(void)strnfmt(name, sizeof(name), "%s", d_char_req_desc[i] + 2);
				break;
			}
		}
	}

	/* Apply our restrictions */
	get_mon_num_hook = mon_select;

	/* Prepare allocation table */
	get_mon_num_prep();

	/* Return the name. */
	return (format("%s", name));
}





/**************************************************************/
/*                                                            */
/*            General dungeon-generation functions            */
/*                                                            */
/**************************************************************/


/*
 * Find a nearby floor-type grid, and return it.
 */
byte get_nearby_floor(int y, int x)
{
	int i;
	int start = rand_int(8);

	/* Look for adjacent floor */
	for (i = start; i < 8 + start; i++)
	{
		int yy = y + ddy_ddd[i % 8];
		int xx = x + ddx_ddd[i % 8];

		/* Skip non-floors */
		if (!cave_floor_bold(yy, xx)) continue;

		/* Use this terrain */
		return (cave_feat[yy][xx]);
	}

	/* No nearby floor found -- use ordinary floor */
	return (FEAT_FLOOR);
}


/*
 * Count the number of walls adjacent to the given grid.
 *
 * Note -- Assumes "in_bounds_fully(y, x)"
 */
static int next_to_walls(int y, int x)
{
	int k = 0;

	if (cave_wall_bold(y+1, x)) k++;
	if (cave_wall_bold(y-1, x)) k++;
	if (cave_wall_bold(y, x+1)) k++;
	if (cave_wall_bold(y, x-1)) k++;

	return (k);
}


/*
 * Returns co-ordinates for the player.  Player prefers to be near
 * walls, because large open spaces are dangerous.
 *
 * If "p_ptr->create_stairs" is non-zero, the character will have stairs
 * placed under him.  If positive, he will usually also be placed in a
 * nice safe alcove.
 */
static void new_player_spot(void)
{
	int i = 0;
	int y = 0;
	int x = 0;


	/* Assume no stairs */
	int feat = 0;

	/* If character starts on stairs, ... */
	if ((!birth_no_return_stair) && (p_ptr->create_stair) &&
	    (p_ptr->character_type != PCHAR_IRONMAN))
	{
		/* Get staircase request */
		feat = p_ptr->create_stair;

		/* Only place down shafts where appropriate */
		if (feat == FEAT_MORE2)
		{
			if ((quest_check(p_ptr->depth + 1)) ||
				 (p_ptr->depth >= MAX_DEPTH - 2))
			{
				feat = FEAT_MORE;
			}
		}

		/* Only place up shafts where appropriate */
		if (feat == FEAT_LESS2)
		{
			if ((quest_check(p_ptr->depth - 1)) ||
				 (p_ptr->depth <= 1))
			{
				feat = FEAT_LESS;
			}
		}

		/* Require a logical feature */
		if ((!feat) || ((feat != FEAT_MORE2) && (feat != FEAT_MORE) &&
		                (feat != FEAT_LESS2) && (feat != FEAT_LESS)))
		{
			if (!p_ptr->depth) feat = FEAT_MORE;
			else               feat = FEAT_LESS;
		}
	}


	/*
	 * Check stored stair locations, then search at random.
	 */
	while (TRUE)
	{
		i++;

		/* Scan stored locations first. */
		if ((i < dun->stair_n) && (p_ptr->create_stair > 0))
		{
			/* Get location */
			y = dun->stair[i].y;
			x = dun->stair[i].x;

			/* Require exactly three adjacent walls */
			if (next_to_walls(y, x) != 3) continue;

			/* Require a "naked" floor grid */
			if (!cave_naked_bold(y, x)) continue;

			/* Success */
			break;
		}

		/* Then, search at random */
		else
		{
			/* Pick a random grid */
			y = rand_int(dungeon_hgt);
			x = rand_int(dungeon_wid);

			/* Refuse to start on anti-teleport (vault) grids */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Must be a floor grid clear of monsters and objects  XXX */
			if (!cave_naked_bold(y, x)) continue;

			/* Try not to start in rooms */
			if ((i < 450) && (cave_info[y][x] & (CAVE_ROOM))) continue;

			/* Player prefers to be near walls. */
			if      ((i < 300) && (next_to_walls(y, x) < 2)) continue;
			else if ((i < 600) && (next_to_walls(y, x) < 1)) continue;

			/* Success */
			break;
		}
	}

	/* Clear stairs request */
	p_ptr->create_stair = 0;

	/* Remove any traps or glyphs  XXX XXX */
	remove_trap(y, x, -1);

	/* Place the stairs (if any) */
	if (feat) cave_set_feat(y, x, feat);

	/* Place the player */
	(void)player_place(y, x);
}


/*
 * Place a secret door at the given location
 */
static void place_secret_door(int y, int x)
{
	/* Create secret door */
	cave_set_feat(y, x, FEAT_SECRET);
}


/*
 * Choose a locked door.  Vary strength with level.
 */
static int pick_locked_door(int depth)
{
	int lock_max = 2 + div_round(depth, 12);
	if (lock_max > 7) lock_max = 7;

	/* Create a locked door */
	return (FEAT_DOOR_HEAD + randint(lock_max));
}


/*
 * Place an unlocked door at the given location
 */
void place_unlocked_door(int y, int x)
{
	/* Create secret door */
	cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
}

/*
 * Place a random type of closed door at the given location.
 */
void place_closed_door(int y, int x)
{
	int tmp;

	/* Choose a feature */
	tmp = rand_int(400);

	/* Closed doors (300/400) */
	if (tmp < 300)
	{
		/* Create closed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x00);
	}

	/* Locked doors (98/400) - based on level */
	else if (tmp < 398)
	{
		/* Create locked door */
		cave_set_feat(y, x, pick_locked_door(p_ptr->depth));
	}

	/* Stuck doors (2/400) - based on level */
	else
	{
		int jam_max = 2 + div_round(p_ptr->depth, 12);
		if (jam_max > 8) jam_max = 8;

		/* Create jammed door */
		cave_set_feat(y, x, FEAT_DOOR_HEAD + 0x08 + rand_int(jam_max));
	}
}


/*
 * Place a random type of door at the given location.
 */
void place_random_door(int y, int x)
{
	int tmp;

	/* Choose an object */
	tmp = rand_int(1000);

	/* Open doors (300/1000) */
	if (tmp < 300)
	{
		/* Create open door */
		cave_set_feat(y, x, FEAT_OPEN);
	}

	/* Broken doors (100/1000) */
	else if (tmp < 400)
	{
		/* Create broken door */
		cave_set_feat(y, x, FEAT_BROKEN);
	}

	/* Secret doors (200/1000) */
	else if (tmp < 600)
	{
		/* Create secret door */
		cave_set_feat(y, x, FEAT_SECRET);
	}

	/* Closed, locked, or stuck doors (400/1000) */
	else
	{
		/* Create closed door */
		place_closed_door(y, x);
	}
}


/*
 * Pick either an ordinary up staircase or an up shaft.
 */
static int pick_up_stairs(void)
{
	if ((p_ptr->depth >= 5) && (!quest_check(p_ptr->depth - 1)))
	{
		if (one_in_(2)) return (FEAT_LESS2);
	}

	return (FEAT_LESS);
}


/*
 * Pick either an ordinary down staircase or an down shaft.
 */
static int pick_down_stairs(void)
{
	if ((p_ptr->depth >= 5) && (p_ptr->depth < MAX_DEPTH - 2) &&
	    (!quest_check(p_ptr->depth + 1)))
	{
		if (one_in_(2)) return (FEAT_MORE2);
	}

	return (FEAT_MORE);
}

/*
 * Place an up/down staircase at given location
 */
static void place_random_stairs(int y, int x)
{
	/* Require a floor grid clear of objects (monsters are OK) */
	if (!cave_clean_bold(y, x)) return;

	/* Choose a staircase */
	if (!p_ptr->depth)
	{
		cave_set_feat(y, x, FEAT_MORE);
	}
	else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
	         (p_ptr->depth >= MAX_DEPTH - 1))
	{
		cave_set_feat(y, x, FEAT_LESS);
	}
	else if ((p_ptr->character_type == PCHAR_IRONMAN) || (one_in_(2)))
	{
		cave_set_feat(y, x, FEAT_MORE);
	}
	else
	{
		cave_set_feat(y, x, FEAT_LESS);
	}
}


/*
 * Places some staircases near walls
 */
static void alloc_stairs(int feat, int num, int walls)
{
	int y, x, i, j;

	/* Place "num" stairs */
	for (i = 0; i < num; i++)
	{
		/* Try hard to place the stair */
		for (j = 0; j < 3000; j++)
		{
			/* Cut some slack if necessary. */
			if ((j > dun->stair_n) && (walls > 2)) walls = 2;
			if ((j > 1000) && (walls > 1)) walls = 1;
			if (j > 2000) walls = 0;

			/* Use the stored stair locations first. */
			if (j < dun->stair_n)
			{
				y = dun->stair[j].y;
				x = dun->stair[j].x;
			}

			/* Then, search at random. */
			else
			{
				/* Pick a random grid */
				y = rand_int(dungeon_hgt);
				x = rand_int(dungeon_wid);
			}

			/* Require a floor grid clear of objects and monsters */
			if (!cave_naked_bold(y, x)) continue;

			/* Require a certain number of adjacent walls */
			if (next_to_walls(y, x) < walls) continue;

			/* Town -- must go down */
			if (!p_ptr->depth)
			{
				/* Clear previous contents, add down stairs */
				cave_set_feat(y, x, FEAT_MORE);
			}

			/* Quest -- must go up */
			else if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
			   (p_ptr->depth >= MAX_DEPTH - 1))
			{
				/* Clear previous contents, add up stairs */
				cave_set_feat(y, x, pick_up_stairs());
			}

			/* Requested type */
			else
			{
				/* Allow shafts */
				if (i != 0)
				{
					if      (feat == FEAT_LESS) feat = pick_up_stairs();
					else if (feat == FEAT_MORE) feat = pick_down_stairs();
				}

				/* Clear previous contents, add stairs */
				cave_set_feat(y, x, feat);
			}

			/* Finished with this staircase. */
			break;
		}
	}
}


/*
 * Allocates some objects (using "place" and "type")
 */
static void alloc_object(int set, int typ, int num)
{
	int y, x, k;

	/* Place some objects */
	for (k = 0; k < num; k++)
	{
		/* Pick a "legal" spot */
		while (TRUE)
		{
			/* Location */
			y = rand_int(dungeon_hgt);
			x = rand_int(dungeon_wid);

			/* Grid must be able to hold traps or objects */
			if (typ == ALLOC_TYP_TRAP)
			{
				if (!cave_trap_allowed(y, x)) continue;
			}
			else
			{
				if (!cave_allow_object_bold(y, x)) continue;
			}

			/* Some things go in rooms, some in corridors */
			if (cave_info[y][x] & (CAVE_ROOM))
			{
				if (set == ALLOC_SET_CORR) continue;
			}
			else
			{
				if (set == ALLOC_SET_ROOM) continue;
			}

			/* Accept it */
			break;
		}

		/* Place something */
		switch (typ)
		{
			case ALLOC_TYP_RUBBLE:
			{
				/* Place rubble.  One time in 25, also place a hidden treasure. */
				cave_set_feat(y, x, FEAT_RUBBLE);
				if (one_in_(25)) place_trap(y, x, TRAP_LOOSE_ROCK, p_ptr->depth);
				break;
			}

			case ALLOC_TYP_LOOSE_ROCK:
			{
				place_trap(y, x, TRAP_LOOSE_ROCK, p_ptr->depth);
				break;
			}

			case ALLOC_TYP_TRAP:
			{
				place_trap(y, x, -1, p_ptr->depth);
				break;
			}

			case ALLOC_TYP_GOLD:
			{
				place_gold(y, x);
				break;
			}

			case ALLOC_TYP_OBJECT:
			{
				place_object(y, x, FALSE, FALSE, FALSE);
				break;
			}

			case ALLOC_TYP_BOULDER:
			{
				make_boulder(y, x, p_ptr->depth);
				break;
			}
			case ALLOC_TYP_FOOD:
			{
				make_food(y, x);
				break;
			}

			default:
			{
				break;
			}
		}
	}
}


/*
 * Value "1" means the grid will be changed, value "0" means it won't.
 */
static bool seam_can_change[47] =
{
	1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1,
	1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 0, 1, 1, 1, 0
};


/*
 * Global variables for dungeon seams
 */
static bool do_seam = FALSE;
static int time_to_treas = -1;
static int can_change_pos = 0;
static int treas_chance = 0;


/*
 * Change a feature in a dungeon seam.
 */
static void do_seam_grid(int y, int x, int feat)
{
	/* Only convert "granite" walls (not secret doors) */
	if ((!cave_granite_bold(y, x)) ||
		(cave_any_door(y, x))) return;

	/* Skip vaults and whatnot */
	if (cave_info[y][x] & (CAVE_ICKY)) return;


	/* Cycle through the can_change array */
	can_change_pos++;
	if (can_change_pos >= 47) can_change_pos = 0;

	/* Skip this grid if we can't change it */
	if (!seam_can_change[can_change_pos]) return;

	/* Count down to next treasure */
	if (time_to_treas > 0) time_to_treas--;


	/* Add a treasure if we can */
	if (time_to_treas == 0)
	{
		if      (feat == FEAT_MAGMA)  cave_set_feat(y, x, FEAT_MAGMA_K);
		else if (feat == FEAT_QUARTZ) cave_set_feat(y, x, FEAT_QUARTZ_K);
		else                          cave_set_feat(y, x, feat);

		/* Reset the treasure countdown */
		if (treas_chance > 0) time_to_treas = randint(treas_chance * 2);
	}

	/* Add an ordinary feature otherwise */
	else
	{
		cave_set_feat(y, x, feat);
	}
}



/*
 * Build a destroyed level
 */
void destroy_level(bool new_level)
{
	int y1, x1, y, x, k, t, n, epicenter_max;


	/* Note destroyed levels. */
	if (cheat_room && new_level) msg_print("Destroyed Level");

	/* Determine the maximum number of epicenters. */
	if (new_level) epicenter_max = rand_range(2, 6);
	else epicenter_max = rand_range(5, 10);

	/* Drop a few epicenters */
	for (n = 0; n < epicenter_max; n++)
	{
		/* Pick an epicenter */
		x1 = rand_range(5, dungeon_wid-1 - 5);
		y1 = rand_range(5, dungeon_hgt-1 - 5);

		/* Big area of affect */
		for (y = (y1 - 15); y <= (y1 + 15); y++)
		{
			for (x = (x1 - 15); x <= (x1 + 15); x++)
			{
				/* Skip illegal grids */
				if (!in_bounds_fully(y, x)) continue;

				/* Extract the distance */
				k = distance(y1, x1, y, x);

				/* Stay in the circle of death */
				if (k >= 16) continue;

				/* Skip any monsters */
				if (cave_m_idx[y][x]) continue;

				/* Destroy grids without permanent features or artifacts */
				if (cave_valid_bold(y, x))
				{
					/* Delete objects */
					delete_object(y, x);

					/* Wall (or floor) type */
					t = rand_int(200);

					/* Granite */
					if (t < 20)
					{
						/* Create granite wall */
						cave_set_feat(y, x, FEAT_WALL_EXTRA);
					}

					/* Quartz */
					else if (t < 60)
					{
						/* Create quartz vein */
						cave_set_feat(y, x, FEAT_QUARTZ);
					}

					/* Magma */
					else if (t < 80)
					{
						/* Create magma vein */
						cave_set_feat(y, x, FEAT_MAGMA);
					}

					/* Rubble. */
					else if (t < 130)
					{
						/* Create rubble */
						cave_set_feat(y, x, FEAT_RUBBLE);
					}

					/* Floor */
					else
					{
						/* Create floor */
						cave_set_feat(y, x, get_nearby_floor(y, x));
					}

					/* No longer part of a room or vault */
					cave_info[y][x] &= ~(CAVE_ROOM | CAVE_ICKY);

					/* No longer illuminated */
					cave_info[y][x] &= ~(CAVE_GLOW);
				}
			}
		}
	}
}




/**************************************************************/
/*                                                            */
/*                   The room-building code                   */
/*                                                            */
/**************************************************************/


/*
 * Place objects, up to the number asked for, in a rectangle centered on
 * y0, x0.  Accept values for maximum vertical and horizontal displacement.
 *
 * Return prematurely if the code starts looping too much (this may happen
 * if y0 or x0 are out of bounds, or the area is filled with walls).
 */
static void spread_objects(int depth, int num, int y0, int x0, int dy, int dx)
{
	int i, j;	/* Limits on loops */
	int count;
	int y = y0, x = x0;
	int tmp_object_level = object_level;


	/* Set generation level */
	object_level = depth;

	/* Try to place objects within our rectangle of effect. */
	for (count = 0, i = 0; ((count < num) && (i < 50)); i++)
	{
		/* Get a location */
		for (j = 0; j < 10; j++)
		{
			y = rand_spread(y0, dy);
			x = rand_spread(x0, dx);

			/* Do not waste much time if called out of bounds */
			if (!in_bounds(y, x))
			{
				if (j < 9) continue;
				else return;
			}
			break;
		}

		/* Grid must be able to hold objects */
		if (!cave_allow_object_bold(y, x)) continue;

		/* Place an item */
		if (!one_in_(3))
		{
			place_object(y, x, FALSE, FALSE, FALSE);
		}

		/* Place gold */
		else
		{
			place_gold(y, x);
		}

		/* Count the object, reset the loop count */
		count++;
		i = 0;
	}

	/* Reset monster generation level. */
	object_level = tmp_object_level;
}


/*
 * Place dungeon traps, up to the number asked for, in a rectangle cen-
 * tered on y0, x0.  Accept values for maximum vertical and horizontal
 * displacement.  Accept value for trap kind (0 or -1 is random).
 *
 * Return prematurely if the code starts looping too much (this may happen
 * if y0 or x0 are out of bounds, or the area is filled with walls).
 */
static void spread_traps(int num, int y0, int x0, int dy, int dx, int feat)
{
	int i, j;	/* Limits on loops */
	int count;
	int y = y0, x = x0;


	/* Ignore calls made out of bounds */
	if (!in_bounds(y0, x0)) return;

	/* Try to create traps within our rectangle of effect. */
	for (count = 0, i = 0; ((count < num) && (i < 100)); i++)
	{
		/* Get a location */
		for (j = 0; j < 10; j++)
		{
			y = rand_spread(y0, dy);
			x = rand_spread(x0, dx);

			/* Do not waste much time if called out of bounds */
			if (!in_bounds(y, x))
			{
				if (j < 9) continue;
				else return;
			}
			break;
		}

		/* Place the trap (allow specific kinds) */
		if (place_trap(y, x, feat, p_ptr->depth))
		{
			/* Count the trap, reset the loop count */
			count++;
			i = 0;
		}
	}
}



/*
 * Place monsters, up to the number asked for, in a rectangle centered on
 * y0, x0.  Accept values for monster depth, symbol, and maximum vertical
 * and horizontal displacement.  Call monster restriction functions if
 * needed.
 *
 * Return prematurely if the code starts looping too much (this may happen
 * if y0 or x0 are out of bounds, or the area is already occupied).
 */
static void spread_monsters(char symbol, int depth, int num,
	int y0, int x0, int dy, int dx)
{
	int i, j;	/* Limits on loops */
	int count;
	int y = y0, x = x0;
	int start_mon_num = m_max;
	bool dummy;
	int tmp_monster_level = monster_level;

	/* Restrict monsters.  Allow uniques. */
	(void)mon_restrict(symbol, (byte)depth, &dummy, TRUE);

	/* Set generation level */
	monster_level = depth;

	/* Try to summon monsters within our rectangle of effect. */
	for (count = 0, i = 0; ((count < num) && (i < 50)); i++)
	{
		/* Get a location */
		for (j = 0; j < 10; j++)
		{
			y = rand_spread(y0, dy);
			x = rand_spread(x0, dx);

			/* Do not waste much time if called out of bounds */
			if (!in_bounds(y, x))
			{
				if (j < 9) continue;
				else return;
			}
			break;
		}

		/* Require passable grids */
		if (!cave_passable_bold(y, x)) continue;

		/* Place the monster (sleeping, allow groups) */
		(void)place_monster(y, x, TRUE, TRUE);

		/* Rein in monster groups and escorts a little. */
		if (m_max - start_mon_num > num * 2) break;

		/* Count the monster(s), reset the loop count */
		count++;
		i = 0;
	}

	/* Remove monster restrictions. */
	(void)mon_restrict('\0', (byte)depth, &dummy, TRUE);

	/* Reset monster generation level. */
	monster_level = tmp_monster_level;
}



/*
 * Generate helper -- create a new room with optional light
 *
 * Return FALSE if the room is not fully within the dungeon.
 */
static bool generate_room(int y1, int x1, int y2, int x2, int light)
{
	int y, x;

	/* Confirm that room is in bounds. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_info[y][x] |= (CAVE_ROOM);
			if (light) cave_info[y][x] |= (CAVE_GLOW);
		}
	}

	/* Success. */
	return (TRUE);
}


/*
 * Generate helper -- fill a rectangle with a feature
 */
static void generate_fill(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_set_feat(y, x, feat);
		}
	}
}


/*
 * Generate helper -- mark a rectangle with a set of cave_info flags
 */
static void generate_mark(int y1, int x1, int y2, int x2, u16b flg)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			cave_info[y][x] |= (flg);
		}
	}
}


/*
 * Generate helper -- draw a rectangle with a feature
 */
static void generate_draw(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;

	for (y = y1; y <= y2; y++)
	{
		cave_set_feat(y, x1, feat);
		cave_set_feat(y, x2, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		cave_set_feat(y1, x, feat);
		cave_set_feat(y2, x, feat);
	}
}


/*
 * Generate helper -- split a rectangle with a feature
 */
static void generate_plus(int y1, int x1, int y2, int x2, int feat)
{
	int y, x;
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	for (y = y1; y <= y2; y++)
	{
		cave_set_feat(y, x0, feat);
	}

	for (x = x1; x <= x2; x++)
	{
		cave_set_feat(y0, x, feat);
	}
}


/*
 * Generate helper -- open all sides of a rectangle with a feature
 */
static void generate_open(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open all sides */
	cave_set_feat(y1, x0, feat);
	cave_set_feat(y0, x1, feat);
	cave_set_feat(y2, x0, feat);
	cave_set_feat(y0, x2, feat);
}


/*
 * Generate helper -- open one side of a rectangle with a feature
 */
static void generate_hole(int y1, int x1, int y2, int x2, int feat)
{
	int y0, x0;

	/* Center */
	y0 = (y1 + y2) / 2;
	x0 = (x1 + x2) / 2;

	/* Open random side */
	switch (rand_int(4))
	{
		case 0:
		{
			cave_set_feat(y1, x0, feat);
			break;
		}
		case 1:
		{
			cave_set_feat(y0, x1, feat);
			break;
		}
		case 2:
		{
			cave_set_feat(y2, x0, feat);
			break;
		}
		case 3:
		{
			cave_set_feat(y0, x2, feat);
			break;
		}
	}
}


/*
 * Find a good spot for the next room.  -LM-
 *
 * Find and allocate a free space in the dungeon large enough to hold
 * the room calling this function.  We allocate space in 11x11 blocks.
 *
 * Be careful to include the edges of the room in height and width!
 *
 * Return TRUE and values for the center of the room if all went well.
 * Otherwise, return FALSE.
 */
static bool find_space(int *y, int *x, int height, int width)
{
	int i;
	int by, bx, by1, bx1, by2, bx2;
	int block_y, block_x;

	bool filled;

	/* Find out how many blocks we need. */
	int blocks_high = 1 + ((height - 1) / BLOCK_HGT);
	int blocks_wide = 1 + ((width - 1) / BLOCK_WID);


	/* We'll allow twenty-five guesses. */
	for (i = 0; i < 25; i++)
	{
		filled = FALSE;

		/* Pick a top left block at random */
		block_y = rand_int(dun->row_rooms - blocks_high);
		block_x = rand_int(dun->col_rooms - blocks_wide);


		/* Rooms that straddle a border must shift. */
		if ((blocks_wide == 2) && ((block_x % 3) == 2))
		{
			if (one_in_(2)) block_x--;
			else            block_x++;
		}

		/* Rooms with width divisible by 3 get fitted to a rectangle. */
		else if ((blocks_wide % 3) == 0)
		{
			/* Align to the left edge of a 11x33 rectangle. */
			if ((block_x % 3) == 2) block_x++;
			if ((block_x % 3) == 1) block_x--;
		}

		/*
		 * Big rooms that do not have a width divisible by 3 get
		 * aligned towards the edge of the dungeon closest to them.
		 */
		if (blocks_wide > 3)
		{
			/* Shift towards left edge of dungeon. */
			if (block_x + (blocks_wide / 2) <= dun->col_rooms / 2)
			{
				if (((block_x % 3) == 2) && ((blocks_wide % 3) == 2))
					block_x--;
				if ((block_x % 3) == 1) block_x--;
			}

			/* Shift toward right edge of dungeon. */
			else
			{
				if (((block_x % 3) == 2) && ((blocks_wide % 3) == 2))
					block_x++;
				if ((block_x % 3) == 1) block_x++;
			}
		}

		/* Extract blocks */
		by1 = block_y + 0;
		bx1 = block_x + 0;
		by2 = block_y + blocks_high;
		bx2 = block_x + blocks_wide;

		/* Never run off the screen */
		if ((by1 < 0) || (by2 > dun->row_rooms)) continue;
		if ((bx1 < 0) || (bx2 > dun->col_rooms)) continue;

		/* Verify available space */
		for (by = by1; by < by2; by++)
		{
			for (bx = bx1; bx < bx2; bx++)
			{
				if (dun->room_map[by][bx])
				{
					filled = TRUE;
				}
			}
		}

		/* If space filled, try again. */
		if (filled) continue;


		/* It is *extremely* important that the following calculation */
		/* be *exactly* correct to prevent memory errors  XXX XXX XXX */

		/* Acquire the location of the room */
		(*y) = ((by1 + by2) * BLOCK_HGT) / 2;
		(*x) = ((bx1 + bx2) * BLOCK_WID) / 2;


		/* Save the room location */
		if (dun->cent_n < CENT_MAX)
		{
			dun->cent[dun->cent_n].y = *y;
			dun->cent[dun->cent_n].x = *x;
			dun->cent_n++;
		}

		/* Reserve some blocks.  Mark each with the room index. */
		for (by = by1; by < by2; by++)
		{
			for (bx = bx1; bx < bx2; bx++)
			{
				dun->room_map[by][bx] = dun->cent_n;
			}
		}

		/* Success. */
		return (TRUE);
	}

	/* Failure. */
	return (FALSE);
}


/*
 * Make a starburst room. -LM-
 *
 * Starburst rooms are made in three steps:
 * 1: Choose a room size-dependant number of arcs.  Large rooms need to
 *    look less granular and alter their shape more often, so they need
 *    more arcs.
 * 2: For each of the arcs, calculate the portion of the full circle it
 *    includes, and its maximum effect range (how far in that direction
 *    we can change features in).  This depends on room size, shape, and
 *    the maximum effect range of the previous arc.
 * 3: Use the table "get_angle_to_grid" to supply angles to each grid in
 *    the room.  If the distance to that grid is not greater than the
 *    maximum effect range that applies at that angle, change the feature
 *    if appropriate (this depends on feature type).
 *
 * Usage notes:
 * - This function uses a table that cannot handle distances larger than
 *   20, so it calculates a distance conversion factor for larger rooms.
 * - This function is not good at handling rooms much longer along one axis
 *   than the other.
 * - It is safe to call this function on areas that might contain vaults or
 *   pits, because "icky" and occupied grids are left untouched.
 *
 * - Mixing these rooms (using normal floor) with rectangular ones on a
 *   regular basis produces a somewhat chaotic looking dungeon.  However,
 *   this code does works well for large and medium-sized lakes.  Small
 *   lakes end up looking too rectangular.
 */
static bool generate_starburst_room(int y1, int x1, int y2, int x2,
	bool light, int feat, bool special_ok)
{
	int y0, x0, y, x, ny, nx;
	int i, d;
	int size;

	int dist, max_dist, dist_conv, dist_check;
	int height, width, arc_dist;
	int degree_first, center_of_arc, degree;

	bool temp_array_active = FALSE;

	/* Special variant room. */
	bool make_cloverleaf = FALSE;

	/* Holds first degree of arc, maximum effect distance in arc. */
	int arc[45][2];

	/* Number (max 45) of arcs. */
	int arc_num;

	/* Notice floor */
	bool floor = ((f_info[feat].flags & (TF_FLOOR)) != 0);


	/* Make certain the room does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);

	/* Robustness -- test sanity of input coordinates. */
	if ((y2 - y1 <= 1) || (x2 - x1 <= 1)) return (FALSE);


	/* Get room height and width. */
	height = 1 + y2 - y1;
	width  = 1 + x2 - x1;

	/* Note the "size" */
	size = 2 + div_round(width + height, 22);


	/* Get a shrinkage ratio for large rooms, as table is limited. */
	if ((width > 40) || (height > 40))
	{
		if (width > height) dist_conv = 1 + (10 * width  / 40);
		else                dist_conv = 1 + (10 * height / 40);
	}
	else dist_conv = 10;


	/* Make a cloverleaf room sometimes.  (discovered by accident) */
	if ((special_ok) && (width >= 12) && (height >= 12) && (one_in_(15)))
	{
		arc_num = 12;
		make_cloverleaf = TRUE;
	}

	/* Usually, we make a normal starburst. */
	else
	{
		/* Ask for a reasonable number of arcs. */
		if (height * width < 15) arc_num = 4;
		else if (height * width < 30) arc_num = 6;
		else
		{
			arc_num = 8 + (height * width / 80);
			arc_num = rand_spread(arc_num, 3);
			if (arc_num < 8) arc_num = 8;
			if (arc_num > 45) arc_num = 45;
		}
	}


	/* Get the center of the starburst. */
	y0 = y1 + height / 2;
	x0 = x1 + width  / 2;

	/* Start out at zero degrees. */
	degree_first = 0;


	/* Determine the start degrees and expansion distance for each arc. */
	for (i = 0; i < arc_num; i++)
	{
		/* Get the first degree for this arc (using 240-degree circles). */
		arc[i][0] = degree_first;

		/* Get a slightly randomized start degree for the next arc. */
		degree_first += div_round(240, arc_num);

		/* Do not entirely leave the usual range */
		if (degree_first < 240 * (i+1) / arc_num)
		    degree_first = 240 * (i+1) / arc_num;
		if (degree_first > (240 + arc_num) * (i+1) / arc_num)
		    degree_first = (240 + arc_num) * (i+1) / arc_num;

		/* Get the center of the arc */
		center_of_arc = (degree_first + arc[i][0]) / 2;

		/* Get arc distance from the horizontal (0 and 120 degrees) */
		if      (center_of_arc <  60) arc_dist = center_of_arc;
		else if (center_of_arc > 180) arc_dist = ABS(center_of_arc - 240);
		else                          arc_dist = ABS(center_of_arc - 120);

		/*
		 * Calculate distance to expand outwards.  Pay more attention
		 * to width near the horizontal, more attention to height near
		 * the vertical.
		 */
		dist = ((height * arc_dist) + (width * (60 - arc_dist))) / 60;

		/* Usual case -- Randomize distance (never greater than radius) */
		if (!make_cloverleaf)
		{
			arc[i][1] = rand_range((dist-1) / 4, dist / 2);

			/* Keep variability under control (except for first arc). */
			if ((dist != 0) && (i != 0))
			{
				int diff = arc[i][1] - arc[i-1][1];

				if (ABS(diff) > size)
				{
					if (diff > 0)
						arc[i][1] = arc[i-1][1] + size;
					else
						arc[i][1] = arc[i-1][1] - size;
				}
				else if ((!ABS(diff)))
				{
					if (one_in_(2)) arc[i][1] = arc[i-1][1] + 1;
					else            arc[i][1] = arc[i-1][1] - 1;
				}
			}
		}

		/* Special case -- Handle cloverleaves */
		else
		{
			if (arc_dist == 60) arc[i][1] = 0;
			else                arc[i][1] = dist / 2 - 1;
		}
	}

	/* Neaten up final arc of circle by comparing it to the first. */
	if (!make_cloverleaf)
	{
		int diff = arc[arc_num - 1][1] - arc[0][1];

		if (ABS(diff) > size)
		{
			if (diff > 0) arc[arc_num - 1][1] = arc[0][1] + size;
			else          arc[arc_num - 1][1] = arc[0][1] - size;
		}
	}


	/* If we are placing water or lava, we need to ensure continuity. */
	if ((feat == FEAT_WATER) || (feat == FEAT_LAVA))
	{
		/* Spread the region of effect through any non-wall terrain */
		spread_cave_temp(y0, x0, MAX(height, width), FALSE);

		/* We are successfully using the "temp" array */
		if (temp_n >= 5) temp_array_active = TRUE;
	}

	/* Cloverleaves ignore walls near entranceways  XXX */
	if ((make_cloverleaf) && (floor))
	{
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+1][x0+1] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+1][x0+2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+2][x0+2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+2][x0+1] |= (CAVE_ICKY);

		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-1][x0+1] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-1][x0+2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-2][x0+2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-2][x0+1] |= (CAVE_ICKY);

		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+1][x0-1] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+1][x0-2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+2][x0-2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0+2][x0-1] |= (CAVE_ICKY);

		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-1][x0-1] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-1][x0-2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-2][x0-2] |= (CAVE_ICKY);
		if (cave_wall_bold(y0+1, x0+1)) cave_info[y0-2][x0-1] |= (CAVE_ICKY);
	}


	/* Pre-calculate check distance. */
	dist_check = 21 * dist_conv / 10;

	/* Change grids between (and not including) the edges. */
	for (y = y1 + 1; y < y2; y++)
	{
		for (x = x1 + 1; x < x2; x++)
		{
			/* Do not touch "icky" grids. */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Do not touch occupied grids. */
			if (cave_m_idx[y][x] != 0) continue;
			if (cave_o_idx[y][x] != 0) continue;


			/* Get distance to grid. */
			dist = distance(y0, x0, y, x);

			/* Look at the grid if within check distance. */
			if (dist < dist_check)
			{
				/* Convert and reorient grid for table access. */
				ny = 20 + 10 * (y - y0) / dist_conv;
				nx = 20 + 10 * (x - x0) / dist_conv;

				/* Illegal table access is bad. */
				if ((ny < 0) || (ny > 40) || (nx < 0) || (nx > 40))
					continue;

				/* Get angle to current grid. */
				degree = get_angle_to_grid[ny][nx];

				/* Scan arcs to find the one that applies here. */
				for (i = arc_num - 1; i >= 0; i--)
				{
					if (arc[i][0] <= degree)
					{
						max_dist = arc[i][1];

						/* Must be within effect range. */
						if (max_dist >= dist)
						{
							/* Special-case handling for seams */
							if (do_seam)
							{
								do_seam_grid(y, x, feat);
							}

							/* If new feature is not passable, or floor, always place it. */
							else if ((floor) || !(f_info[feat].flags & (TF_PASSABLE)))
							{
								cave_set_feat(y, x, feat);

								if (floor) cave_info[y][x] |= (CAVE_ROOM);
								else cave_info[y][x] &= ~(CAVE_ROOM);

								if (light) cave_info[y][x] |= (CAVE_GLOW);
								else cave_info[y][x] &= ~(CAVE_GLOW);
							}

							/* If new feature is non-floor passable terrain, place it only over floor. */
							else
							{
								/* Replace old feature in some cases. */
								if ((feat == FEAT_TREE) || (feat == FEAT_RUBBLE))
								{
									/* Make denser in the middle. */
									if ((cave_floor_bold(y, x)) &&
										(randint(max_dist + 5) >= dist + 5))
										cave_set_feat(y, x, feat);
								}

								/* Water and lava try to be contiguous */
								if ((feat == FEAT_WATER) || (feat == FEAT_LAVA))
								{
									if ((temp_array_active) &&
									    !(cave_info[y][x] & (CAVE_TEMP)))
									{
										/* Do nothing */
									}

									else if (cave_floor_bold(y, x))
									{
										cave_set_feat(y, x, feat);
									}
								}

								/* Light grid. */
								if (light) cave_info[y][x] |= (CAVE_GLOW);
							}
						}

						/* Arc found.  End search */
						break;
					}
				}
			}
		}
	}


	/* Cloverleaf entranceways sometimes have doors */
	if ((make_cloverleaf) && (floor) && (one_in_(4)))
	{
		place_random_door(y0+1, x0);
		place_random_door(y0-1, x0);
		place_random_door(y0, x0+1);
		place_random_door(y0, x0-1);
	}


	/*
	 * If we placed floors or dungeon granite, all dungeon granite next
	 * to floors needs to become outer wall.
	 */
	if ((floor) || (feat == FEAT_WALL_EXTRA))
	{
		for (y = y1 + 1; y < y2; y++)
		{
			for (x = x1 + 1; x < x2; x++)
			{
				/* Floor grids only */
				if (cave_floor_bold(y, x))
				{
					/* Look in all directions. */
					for (d = 0; d < 8; d++)
					{
						/* Extract adjacent location */
						int yy = y + ddy_ddd[d];
						int xx = x + ddx_ddd[d];

						/* Join to room */
						cave_info[yy][xx] |= (CAVE_ROOM);

						/* Illuminate if requested. */
						if (light) cave_info[yy][xx] |= (CAVE_GLOW);

						/* Look for dungeon granite. */
						if (cave_feat[yy][xx] == FEAT_WALL_EXTRA)
						{
							/* Turn into outer wall. */
							cave_set_feat(yy, xx, FEAT_WALL_OUTER);
						}
					}
				}
			}
		}
	}

	/* Clear the "temp" array */
	if (temp_n) clear_temp_array();

	/* Success */
	return (TRUE);
}



/*
 * Places seams of rock in the dungeon using the starburst code.
 *
 * Note that there are actually six different terrain features used
 * to represent seams.  Three each of magma and quartz, one for
 * basic vein, one with hidden gold, and one with known gold.  The
 * hidden gold types are currently unused.
 */
static void build_seam(int feat, int chance)
{
	/* Determine scale */
	int scale = MIN(dungeon_hgt, dungeon_wid);

	/* Determine actual size */
	int height = rand_range(scale / 6, scale / 2);
	int width  = rand_range(scale / 6, scale / 2);

	/* Calculate top-left position */
	int top  = rand_int(dungeon_hgt - height);
	int left = rand_int(dungeon_wid - width);


	/* Initialize start point on the seam change array */
	can_change_pos = rand_int(47);

	/* Store the treasure chance */
	treas_chance = chance;

	/* Initialize time to first treasure (can happen never or always) */
	if (chance > 0) time_to_treas = randint(chance * 2);
	else            time_to_treas = chance;


	/* Request seam */
	do_seam = TRUE;

	/* Build the seam */
	(void)generate_starburst_room(top, left, top + height - 1, left + width - 1,
		FALSE, feat, FALSE);

	/* No longer making a seam */
	do_seam = FALSE;
}



/*
 * Make a pool (useful for water and lava).
 */
static bool generate_pool(int y1, int x1, int y2, int x2, int feat)
{
	int i, y0, x0;
	int height, width;

	/* Size allowed is too small -- return */
	if (y2 - y1 < 2) return (FALSE);
	if (x2 - x1 < 2) return (FALSE);


	/* Look hard for a floor (or water) grid */
	for (i = 0; i < 50; i++)
	{
		/* Find a random center within the rectangle of effect */
		y0 = rand_range(y1+1, y2-1) + rand_int(2);
		x0 = rand_range(x1+1, x2-1) + rand_int(2);

		/* If it is a floor or water grid, accept it */
		if (cave_floor_bold(y0, x0) || (cave_feat[y0][x0] == FEAT_WATER)) break;
	}

	/* Calculate pool size (must not be too narrow or long */
	height  = y2 - y1;
	width   = x2 - x1;
	if      (height > width  * 2) height = width  * 2;
	else if (width  > height * 2) width  = height * 2;

	/* Make the pool */
	return (generate_starburst_room(y0 - (height+1) / 2, x0 - (width+1) / 2,
		y0 + height / 2, x0 + width / 2, FALSE, feat, FALSE));
}





/*
 * Room building routines.
 *
 * Nine basic room types:
 *   1 -- normal
 *   2 -- overlapping
 *   3 -- cross shaped
 *   4 -- large room with features
 *   5 -- monster pits
 *   6 -- chambered rooms
 *   7 -- interesting rooms
 *   8 -- simple vaults
 *   9 -- greater vaults
 *
 */



/* Convert a maze coordinate into a dungeon coordinate */
#define YPOS(y, y1)		((y1) + (y) * 2 + 1)
#define XPOS(x, x1)		((x1) + (x) * 2 + 1)


/*
 * Build an acyclic maze inside a given rectangle.  - Eric Bock -
 * Construct the maze from a given pair of features.
 *
 * Note that the edge lengths should be odd.
 *
 * THIS FUNCTION IS AVAILABLE ONLY UNDER THE MORIA LICENSE.
 */
static void draw_maze(int y1, int x1, int y2, int x2, byte feat_wall,
    byte feat_path)
{
	int i, j;
	int ydim, xdim;
	int grids;

	int y, x;
	int ty, tx;
	int dy, dx;

	byte dir[4];
	byte dirs;

	/* Start with a solid rectangle of the "wall" feat */
	generate_fill(y1, x1, y2, x2, feat_wall);

	/* Calculate dimensions */
	ydim = (y2 - y1) / 2;
	xdim = (x2 - x1) / 2;

	/* Number of unexamined grids */
	grids = ydim * xdim - 1;

	/* Set the initial position */
	y = rand_int(ydim);
	x = rand_int(xdim);

	/* Place a floor here */
	cave_set_feat(YPOS(y, y1), XPOS(x, x1), feat_path);

	/* Now build the maze */
	while (grids)
	{
		/* Only use maze grids */
		if (cave_feat[YPOS(y, y1)][XPOS(x, x1)] == feat_path)
		{
			/* Pick a target */
			ty = rand_int(ydim);
			tx = rand_int(xdim);

			while (TRUE)
			{
				dirs = 0;
				dy = 0;
				dx = 0;

				/* Calculate the dungeon position */
				j = YPOS(y, y1);
				i = XPOS(x, x1);

				/** Enumerate possible directions **/

				/* Up */
				if (y && (cave_feat[j - 2][i] == feat_wall)) dir[dirs++] = 1;

				/* Down */
				if ((y < ydim - 1) && (cave_feat[j + 2][i] == feat_wall)) dir[dirs++] = 2;

				/* Left */
				if (x && (cave_feat[j][i - 2] == feat_wall)) dir[dirs++] = 3;

				/* Right */
				if ((x < xdim - 1) && (cave_feat[j][i + 2] == feat_wall)) dir[dirs++] = 4;

				/* Dead end; go to the next valid grid */
				if (!dirs) break;

				/* Pick a random direction */
				switch (dir[rand_int(dirs)])
				{
					/* Move up */
					case 1:  dy = -1;  break;

					/* Move down */
					case 2:  dy =  1;  break;

					/* Move left */
					case 3:  dx = -1;  break;

					/* Move right */
					case 4:  dx =  1;  break;
				}

				/* Place floors */
				cave_set_feat(j + dy, i + dx, feat_path);
				cave_set_feat(j + dy * 2, i + dx * 2, feat_path);

				/* Advance */
				y += dy;
				x += dx;

				/* One less grid to examine */
				grids--;

				/* Check for completion */
				if ((y == ty) && (x == tx)) break;
			}
		}

		/* Find a new position */
		y = rand_int(ydim);
		x = rand_int(xdim);
	}
}


#undef YPOS
#undef XPOS


/*
 * Special kind of room type 1.  Uses
 * the "starburst room" code.
 */
static bool build_type1_starburst(bool light, int feat)
{
	int y0, x0, y1, x1, y2, x2;
	int i;
	int height, width;

	int choice;


	/* Try twice to find space for a room. */
	for (i = 0; i < 2; i++)
	{
		/* Fairly large room - only on first try. */
		if ((i == 0) && (one_in_(5)))
		{
			height = (rand_range(2, 3)) * BLOCK_HGT;
			width =  (rand_range(3, 5)) * BLOCK_WID;
		}

		/* Normal-sized room.  Rarely tall and thin. */
		else
		{
			if (one_in_(7))
			{
				height = (rand_range(2, 4)) * BLOCK_HGT;
				width = BLOCK_WID;
			}
			else
			{
				height = ((!one_in_(3)) ? 1 : 2) * BLOCK_HGT;
				width  = (rand_range(2, 3))      * BLOCK_WID;
			}
		}

		/* Find and reserve some space in the dungeon.  Get center of room. */
		if (!find_space(&y0, &x0, height, width))
		{
			if (i == 0) continue;
			if (i == 1) return (FALSE);
		}
		else break;
	}

	/* Locate the room */
	y1 = y0 - height / 2;
	x1 = x0 - width  / 2;
	y2 = y1 + height - 1;
	x2 = x1 + width  - 1;


	/* Generate starburst room.  Return immediately if out of bounds. */
	if (!generate_starburst_room(y1, x1, y2, x2, light, feat, TRUE))
	{
		return (FALSE);
	}

	/* Sometimes, the room may have rubble, water, or trees in it. */
	choice = randint(50);

	if      (choice == 50) feat = FEAT_TREE;
	else if (choice >= 42) feat = FEAT_WATER;
	else if (choice >= 40) feat = FEAT_RUBBLE;
	else                   feat = 0;

	/* Generate special terrain if necessary. */
	if (feat)
	{
		(void)generate_starburst_room(y1 + rand_int(height / 4),
			x1 + rand_int(width / 4), y2 - rand_int(height / 4),
			x2 - rand_int(width / 4), FALSE, feat, FALSE);
	}

	/* On rare occasion, the room may have some loose rocks */
	if (one_in_(50))
	{
		spread_traps(randint(6), y0, x0, height / 2, width / 2,
			TRAP_LOOSE_ROCK);
	}

	/* Place some "Moria" monsters */
	spread_monsters('%', monster_level, 2, y0, x0, height, width);


	/* Success */
	return (TRUE);
}



/*
 * Change a feature only if "in_bounds_fully()".  Do not change outer walls.
 */
static void set_feat_room(int y, int x, int feat)
{
	if (in_bounds_fully(y, x))
	{
		if (cave_feat[y][x] != FEAT_WALL_OUTER) cave_set_feat(y, x, feat);
	}
}


/*
 * Type 1 -- Special vaulted and ragged-edge rooms
 *
 * -EB-, -LM-
 *
 * PARTS OF THIS FUNCTION ARE AVAILABLE ONLY UNDER THE MORIA LICENSE.
 */
static bool build_type1_pillars(bool light)
{
	int y, x, dy, dx, y0, x0, y1, x1, y2, x2;
	int y_num, x_num;

	int choice;
	int pillar_count = 0;

	int spacing;
	int offset = 0;
	int stagger_y = 0;
	int stagger_x = 0;

	/* Vaulted rooms are two times as common as ragged-edge rooms */
	bool vaulted = (!one_in_(3));


	/* Spacing of pillars or wall alcoves is usually two */
	spacing = 2;

	/* Allow wider spacing sometimes */
	if (one_in_(4))
	{
		spacing++;
		if ((!vaulted) && (one_in_(4))) spacing++;
	}


	/* Determine size of room */
	while (TRUE)
	{
		/* Pick number of pillars -- prefer horizontal rooms */
		if ((map_rows < 3 * BLOCK_HGT) || (!one_in_(6)))
		{
			y_num = rand_range(MAX(2, 6 / spacing), 15 / spacing);
			x_num = rand_range(MAX(2, 6 / spacing), y_num + 15 / spacing);
		}
		else
		{
			x_num = rand_range(MAX(2, 6 / spacing), 15 / spacing);
			y_num = rand_range(MAX(2, 6 / spacing), x_num + 15 / spacing);
		}


		/* Assume an offset from the border walls of one */
		offset = 1;

		/* The offset of pillars in vaulted rooms varies */
		if ((vaulted) && (spacing >= 2) && (one_in_(4)))
		{
			/* Pillars can (rarely) be right at the walls */
			if ((spacing >= 3) && (y_num > 3) && (x_num > 3) &&
			    (one_in_(3)))
			{
				offset--;
			}

			/* Pillars can be two grids away from the walls */
			else offset++;
		}

		/* Calculate room dimensions (y) */
		dy = offset + offset + 1 + (y_num - 1) * spacing;

		/* Calculate room dimensions (x) */
		dx = offset + offset + 1 + (x_num - 1) * spacing;

		/* Room must not be higher than 33.  XXX XXX */
		if (dy > BLOCK_HGT * 3) continue;

		/* Room must not be wider than 44.  XXX XXX */
		if (dx > BLOCK_WID * 4) continue;

		/* Accept dimensions */
		break;
	}

	/* Pillars two grids apart may be staggered in some cases */
	if ((spacing % 2 == 0) && (offset == 1))
	{
		if      ((x_num % 2 == 1) && (one_in_(4)))
			stagger_y = spacing / 2;

		else if ((y_num % 2 == 1) && (one_in_(3)))
			stagger_x = spacing / 2;
	}


	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, dy, dx)) return (FALSE);

	/* Locate the room */
	y1 = y0 - dy / 2;
	x1 = x0 - dx / 2;
	y2 = y1 + dy - 1;
	x2 = x1 + dx - 1;

	/* Generate new room.  Quit immediately if out of bounds. */
	if (!generate_room(y1, x1, y2, x2, light)) return (FALSE);

	/* Generate outer walls */
	generate_draw(y1, x1, y2, x2, FEAT_WALL_OUTER);

	/* Make a standard room */
	generate_fill(y1+1, x1+1, y2-1, x2-1, FEAT_FLOOR);


	/*** Fill room ***/

	/* Randomize start of staggering */
	choice = rand_int(2);

	/* For every row of pillars, ... */
	for (y = y1 + offset; y <= y2; y += spacing)
	{
		/* Toggle stagger at start of every line */
		if ((stagger_x) || (stagger_y)) choice++;

		/* ... and every column: */
		for (x = x1 + offset; x <= x2; x += spacing)
		{
			/* Toggle vertical stagger in every grid */
			if (stagger_y) choice++;

			/* Place a pillar shifted vertically */
			if ((stagger_y) && (choice % 2 == 0))
			{
				if ((y + stagger_y > y1+1) && (y + stagger_y < y2-1) && (x > x1+1) && (x < x2-1))
				{
					set_feat_room(y + stagger_y, x, FEAT_PILLAR);
					pillar_count++;
				}
				else
					set_feat_room(y + stagger_y, x, FEAT_WALL_INNER);
			}

			/* Place a pillar shifted horizontally */
			else if ((stagger_x) && (choice % 2 == 0))
			{
				if ((y > y1+1) && (y < y2-1) && (x + stagger_x > x1+1) && (x + stagger_x < x2-1))
				{
					set_feat_room(y, x + stagger_x, FEAT_PILLAR);
					pillar_count++;
				}
				else
					set_feat_room(y, x + stagger_x, FEAT_WALL_INNER);
			}

			/* Place a pillar with no shift */
			else
			{
				if ((y > y1+1) && (y < y2-1) && (x > x1+1) && (x < x2-1))
				{
					set_feat_room(y, x, FEAT_PILLAR);
					pillar_count++;
				}
				else
					set_feat_room(y, x, FEAT_WALL_INNER);
			}
		}
	}

	/* On rare occasion, every pillar is solid gold! */
	if ((vaulted) && (p_ptr->depth >= pillar_count * 2) && (one_in_(80)))
	{
		/* Check interior of room (not adjacent to the walls) */
		for (y = y1+2; y <= y2-2; y++)
		{
			for (x = x1+2; x <= x2-2; x++)
			{
				/* This is a pillar */
				if (cave_feat[y][x] == FEAT_PILLAR)
				{
					/* Of gold */
					set_feat_room(y, x, FEAT_PILLAR_GOLD);
				}
			}
		}
	}


	/* Ragged-edged rooms have no pillars in the interior of the room */
	if (!vaulted)
	{
		/* Fill with floor  XXX XXX */
		generate_fill(y1+2, x1+2, y2-2, x2-2, FEAT_FLOOR);
	}

	/* Success */
	return (TRUE);
}


/*
 * Type 1 -- normal rectangular rooms
 *
 * These rooms have the lowest build priority (this means that they
 * should not be very large), and are by far the most common type.
 */
static bool build_type1(void)
{
	int height, width, choice;

	int y, x, y0, x0, y1, x1, y2, x2;

	bool light = FALSE;

	/* Occasional light */
	if (p_ptr->depth <= randint(35)) light = TRUE;


	/* Make a special pillared room sometimes */
	if (one_in_(30)) return (build_type1_pillars(light));

	/* At orc/troll depth, sometimes build starburst rooms */
	else if ((p_ptr->depth >= 6) && (p_ptr->depth <= 40) && (one_in_(25)))
	{
		return (build_type1_starburst(light, FEAT_FLOOR));
	}

	/* Pick a room size (less border walls).  Usually longer than tall. */
	if ((map_rows < 3 * BLOCK_HGT) || (!one_in_(5)))
	{
		width  = 1 + damroll(2, 11);
		height = 1 + damroll(2, 4);
	}
	else
	{
		width  = 1 + damroll(2, 4);
		height = 1 + damroll(2, 11);
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height+2, width+2)) return (FALSE);

	/* Locate the room */
	y1 = y0 - height / 2;
	x1 = x0 - width / 2;
	y2 =  y1 + height - 1;
	x2 =  x1 + width - 1;


	/* Generate new room.  Quit immediately if out of bounds. */
	if (!generate_room(y1-1, x1-1, y2+1, x2+1, light)) return (FALSE);


	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Make a standard room. */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Sometimes, we get creative. */
	if (one_in_(50))
	{
		/* Choose a room type */
		choice = rand_int(100);

		/* The ceiling has collapsed. */
		if (choice < 33)
		{
			for (y = y1; y <= y2; y++)
			{
				for (x = x1; x <= x2; x++)
				{
					/* Wall (or floor) type */
					int t = rand_int(100);

					/* Granite */
					if (t < 5)
					{
						/* Create granite wall */
						cave_set_feat(y, x, FEAT_WALL_EXTRA);
					}

					/* Quartz */
					else if (t < 15)
					{
						/* Create quartz vein */
						cave_set_feat(y, x, FEAT_QUARTZ);
					}

					/* Magma */
					else if (t < 25)
					{
						/* Create magma vein */
						cave_set_feat(y, x, FEAT_MAGMA);
					}

					/* Rubble. */
					else if (t < 70)
					{
						/* Create rubble (sometimes with stuff hidden beneath it) */
						cave_set_feat(y, x, FEAT_RUBBLE);
						if (one_in_(40)) place_trap(y, x, TRAP_LOOSE_ROCK, p_ptr->depth);
					}

					/* Floor */
					else
					{
						/* Create floor */
						cave_set_feat(y, x, FEAT_FLOOR);
					}
				}
			}

			/* Here, creatures of Earth dwell. */
			if ((p_ptr->depth > 25) && (p_ptr->depth < 60) && (one_in_(3)))
			{
				spread_monsters('X', monster_level, rand_range(2, 4),
					y0, x0, height, width);

				/* No normal monsters. */
				generate_mark(y1, x1, y2, x2, CAVE_TEMP);
			}
		}

		/* A lake and/or a forest now fills the room. */
		else
		{
			bool water_room = FALSE;

			/* Where there is water, ... */
			if (generate_pool(y1, x1, y2, x2, FEAT_WATER))
			{
				/* ... there may be water creatures, ... */
				if ((p_ptr->depth >= 15) && (p_ptr->depth <= 45) &&
				    (one_in_(4)))
				{
					spread_monsters('6', monster_level, rand_range(2, 4),
						y0, x0, 3, 7);

					/* No normal monsters. */
					generate_mark(y1, x1, y2, x2, CAVE_TEMP);

					water_room = TRUE;
				}
			}

			/* ... or a little nature preserve. */
			if ((!water_room) && (one_in_(2)))
			{
				/* From light and earth... */
				if (!light)
				{
					for (y = y1-1; y <= y2+1; y++)
					{
						for (x = x1-1; x <= x2+1; x++)
						{
							cave_info[y][x] |= (CAVE_GLOW);
						}
					}
				}

				/* ... spring trees. */
				(void)generate_starburst_room(y1, x1, y2, x2, FALSE,
					FEAT_TREE, FALSE);

				/* Animals love trees. */
				if (one_in_(6))
				{
					spread_monsters('3', monster_level, rand_range(2, 4),
						y0, x0, 4, 11);

					/* No normal monsters. */
					generate_mark(y1, x1, y2, x2, CAVE_TEMP);
				}
				else if (one_in_(4))
				{
					spread_monsters('0', monster_level, rand_range(2, 4),
						y0, x0, 4, 11);

					/* No normal monsters. */
					generate_mark(y1, x1, y2, x2, CAVE_TEMP);
				}
			}
		}
	}

	/* On rare occasion, the room may have some loose rocks */
	if (one_in_(80))
	{
		spread_traps(randint(5), y0, x0, y0 - y1, x0 - x1,
			TRAP_LOOSE_ROCK);
	}

	/* Success */
	return (TRUE);
}


/*
 * Type 2 -- Overlapping rectangular rooms
 */
static bool build_type2(void)
{
	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;
	int y0, x0;
	int height, width;
	int choice;

	int light = FALSE;

	/* Occasional light */
	if (p_ptr->depth <= randint(35)) light = TRUE;


	/* Determine extents of room (a) */
	y1a = randint(4);
	x1a = randint(13);
	y2a = randint(3);
	x2a = randint(9);

	/* Determine extents of room (b) */
	y1b = randint(3);
	x1b = randint(9);
	y2b = randint(4);
	x2b = randint(13);


	/* Calculate height */
	height = 11;

	/* Calculate width */
	if ((x1a < 8) && (x2a < 9) && (x1b < 8) && (x2b < 9)) width = 22;
	else width = 33;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* locate room (a) */
	y1a = y0 - y1a;
	x1a = x0 - x1a;
	y2a = y0 + y2a;
	x2a = x0 + x2a;

	/* locate room (b) */
	y1b = y0 - y1b;
	x1b = x0 - x1b;
	y2b = y0 + y2b;
	x2b = x0 + x2b;


	/* Generate new room (a).  Quit immediately if out of bounds. */
	if (!generate_room(y1a-1, x1a-1, y2a+1, x2a+1, light)) return (FALSE);

	/* Generate new room (b).  Quit immediately if out of bounds. */
	if (!generate_room(y1b-1, x1b-1, y2b+1, x2b+1, light)) return (FALSE);


	/* Generate outer walls (a) */
	generate_draw(y1a-1, x1a-1, y2a+1, x2a+1, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_draw(y1b-1, x1b-1, y2b+1, x2b+1, FEAT_WALL_OUTER);

	/* Generate inner floors (a) */
	generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);

	/* Allow special features */
	choice = randint(100);

	/* 7% chance of special features */
	if (choice >= 93)
	{
		/* A lake */
		if ((choice <= 97) || (p_ptr->depth < 12))
		{
			(void)generate_pool(MIN(y1a, y1b), MIN(x1a, x1b), MAX(y2a, y2b),
			                  MAX(x2a, x2b), FEAT_WATER);
		}

		/* Beings of frost or fire */
		else if ((choice == 98) || (choice == 99))
		{
			/* Get some monsters. */
			spread_monsters('7', monster_level, rand_range(2, 4),
				y0, x0, height / 2, width / 2);

			/* No random monsters in the maze */
			generate_mark(MIN(y1a, y1b), MIN(x1a, x1b), MAX(y2a, y2b),
			                  MAX(x2a, x2b), CAVE_TEMP);
		}

		/* Pool of lava with demons */
		else if (choice == 100)
		{
			if (generate_pool(MIN(y1a, y1b), MIN(x1a, x1b), MAX(y2a, y2b),
			                  MAX(x2a, x2b), FEAT_LAVA))
			{
				if (p_ptr->depth > 45) spread_monsters('&', monster_level,
					rand_range(2, 4), y0, x0, height / 2, width / 2);
				else spread_monsters('I', monster_level, rand_range(2, 4),
					y0, x0, height / 2, width / 2);

				/* No random monsters in the maze */
				generate_mark(MIN(y1a, y1b), MIN(x1a, x1b), MAX(y2a, y2b),
			                  MAX(x2a, x2b), CAVE_TEMP);
			}
		}
	}

	/* On rare occasion, the room may have some loose rocks */
	if (one_in_(80))
	{
		spread_traps(randint(5), y0, x0, height / 2, width / 2,
			TRAP_LOOSE_ROCK);
	}

	/* Success */
	return (TRUE);
}



/*
 * Type 3 -- Cross shaped rooms
 *
 * Room "a" runs north/south, and Room "b" runs east/east
 * So a "central pillar" would run from x1a,y1b to x2a,y2b.
 *
 * Note that currently, the "center" is always 3x3, but I think that
 * the code below will work for 5x5 (and perhaps even for asymmetric
 * values like 4x3 or 5x3 or 3x4 or 3x5).
 */
static bool build_type3(void)
{
	int y, x;
	int y0, x0;
	int height, width;

	int y1a, x1a, y2a, x2a;
	int y1b, x1b, y2b, x2b;

	int dy, dx, wy, wx;

	int light = FALSE;

	/* Occasional light */
	if (p_ptr->depth <= randint(35)) light = TRUE;


	/* Pick inner dimension */
	wy = 1;
	wx = 1;

	/* Pick outer dimension */
	if ((map_rows < 3 * BLOCK_HGT) || (!one_in_(5)))
	{
		dy = rand_range(3, 4);
		dx = rand_range(3, 11);
	}
	else
	{
		dy = rand_range(3, 11);
		dx = rand_range(3, 4);
	}

	/* Determine extents of room (a) */
	y1a = dy;
	x1a = wx;
	y2a = dy;
	x2a = wx;

	/* Determine extents of room (b) */
	y1b = wy;
	x1b = dx;
	y2b = wy;
	x2b = dx;

	/* Calculate height */
	if ((y1a + y2a + 1) > (y1b + y2b + 1)) height = y1a + y2a + 1;
	else height = y1b + y2b + 1;

	/* Calculate width */
	if ((x1a + x2a + 1) > (x1b + x2b + 1)) width = x1a + x2a + 1;
	else width = x1b + x2b + 1;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* locate room (b) */
	y1a = y0 - dy;
	x1a = x0 - wx;
	y2a = y0 + dy;
	x2a = x0 + wx;

	/* locate room (b) */
	y1b = y0 - wy;
	x1b = x0 - dx;
	y2b = y0 + wy;
	x2b = x0 + dx;


	/* Generate new room (a).  Quit immediately if out of bounds. */
	if (!generate_room(y1a-1, x1a-1, y2a+1, x2a+1, light)) return (FALSE);

	/* Generate new room (b).  Quit immediately if out of bounds. */
	if (!generate_room(y1b-1, x1b-1, y2b+1, x2b+1, light)) return (FALSE);


	/* Generate outer walls (a) */
	generate_draw(y1a-1, x1a-1, y2a+1, x2a+1, FEAT_WALL_OUTER);

	/* Generate outer walls (b) */
	generate_draw(y1b-1, x1b-1, y2b+1, x2b+1, FEAT_WALL_OUTER);

	/* Generate inner floors (a) */
	generate_fill(y1a, x1a, y2a, x2a, FEAT_FLOOR);

	/* Generate inner floors (b) */
	generate_fill(y1b, x1b, y2b, x2b, FEAT_FLOOR);


	/* Special features */
	switch (randint(4))
	{
		/* Nothing */
		case 1:
		{
			break;
		}

		/* Large solid middle pillar */
		case 2:
		{
			/* Generate a small inner solid pillar */
			generate_fill(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);

			break;
		}

		/* Inner treasure vault */
		case 3:
		{
			/* Generate a small inner vault */
			generate_draw(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);

			/* Open the inner vault with a secret door */
			generate_hole(y1b, x1a, y2b, x2a, FEAT_SECRET);

			/* Place a treasure in the vault */
			object_level = p_ptr->depth + 2;
			place_object(y0, x0, FALSE, FALSE, FALSE);
			object_level = p_ptr->depth;

			/* Let's guard the treasure well */
			monster_level = p_ptr->depth + 4;
			(void)place_monster(y0, x0, TRUE, TRUE);
			monster_level = p_ptr->depth;

			/* Traps, naturally. */
			spread_traps(randint(3), y0, x0, 4, 4, -1);

			break;
		}

		/* Something else */
		case 4:
		{
			/* Occasionally pinch the center shut */
			if (one_in_(3))
			{
				/* Pinch the east/west sides */
				for (y = y1b; y <= y2b; y++)
				{
					if (y == y0) continue;
					cave_set_feat(y, x1a - 1, FEAT_WALL_INNER);
					cave_set_feat(y, x2a + 1, FEAT_WALL_INNER);
				}

				/* Pinch the north/south sides */
				for (x = x1a; x <= x2a; x++)
				{
					if (x == x0) continue;
					cave_set_feat(y1b - 1, x, FEAT_WALL_INNER);
					cave_set_feat(y2b + 1, x, FEAT_WALL_INNER);
				}

				/* Open sides with secret doors */
				if (one_in_(3))
				{
					generate_open(y1b-1, x1a-1, y2b+1, x2a+1, FEAT_SECRET);
				}
			}

			/* Occasionally put a "plus" in the center */
			else if (one_in_(3))
			{
				generate_plus(y1b, x1a, y2b, x2a, FEAT_WALL_INNER);
			}

			/* Occasionally put a "pillar" in the center */
			else if (one_in_(3))
			{
				cave_set_feat(y0, x0, FEAT_WALL_INNER);
			}

			break;
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Type 4 -- Large room, usually with an inner room
 *
 * Possible sub-types:
 *  1 - Small inner room
 *  2 - Pillar or pillars
 *  3 - Checkerboard
 *  4 - Maze
 *  5 - Four compartments
 */
static bool build_type4(void)
{
	int y, x, y1, x1, y2, x2, e_y, e_x;
	int y0, x0;

	bool light = FALSE;
	bool inner = FALSE;

	/* Occasional light */
	if (p_ptr->depth <= randint(35)) light = TRUE;


	/* Pick a room size (less border walls).  Must have odd dimensions. */
	if ((map_rows < 3 * BLOCK_HGT) || (!one_in_(5)))
	{
		y = (one_in_(5) ? (9 + 2 * rand_range(1, 4)) : 9);
		x = 1 + (2 * rand_range(8, 14));
	}
	else
	{
		y = 1 + (2 * rand_range(8, 14));
		x = (one_in_(5) ? (9 + 2 * rand_range(1, 4)) : 9);
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, y+2, x+2)) return (FALSE);

	/* Locate the room */
	y1 = y0 - y / 2;
	x1 = x0 - x / 2;
	y2 = y1 + y - 1;
	x2 = x1 + x - 1;

	/* Generate new room.  Quit immediately if out of bounds. */
	if (!generate_room(y1-1, x1-1, y2+1, x2+1, light)) return (FALSE);

	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);


	/* Usually, but not always, have an inner room */
	if (!one_in_(5))
	{
		/* Note presence of inner room */
		inner = TRUE;

		/* The inner room */
		y1 += 2;
		y2 -= 2;
		x1 += 2;
		x2 -= 2;

		/* Generate inner walls */
		generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);
	}

	/* Note extent of room */
	e_y = (y2 - y1) / 2;
	e_x = (x2 - x1) / 2;


	/* Room variations */
	switch (randint(5))
	{
		/* Small inner room */
		case 1:
		{
			/* Open the inner room with a secret door */
			if (inner) generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Place another inner room */
			generate_draw(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_INNER);

			/* Open the inner room with a secret door */
			generate_hole(y0-1, x0-1, y0+1, x0+1, FEAT_SECRET);

			/* Monsters on guard */
			spread_monsters('\0', monster_level + 2, 4, y0, x0, e_y, e_x);

			/* Object (80%) */
			if (!one_in_(5))
			{
				object_level = p_ptr->depth + 2;
				place_object(y0, x0, FALSE, FALSE, FALSE);
				object_level = p_ptr->depth;
			}

			/* Stairs (20%) */
			else
			{
				place_random_stairs(y0, x0);
			}

			/* Traps */
			spread_traps(randint(3), y0, x0, e_y, e_x, -1);

			break;
		}

		/* An inner room with an inner pillar or pillars */
		case 2:
		{
			/* Open the inner room with a secret door */
			if (inner) generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

			/* Inner pillar */
			generate_fill(y0-1, x0-1, y0+1, x0+1, FEAT_WALL_INNER);

			/* Occasionally, two more large inner pillars */
			if (((x2 - x1) >= 15) && one_in_(2))
			{
				/* Three spaces */
				if (((x2 - x1) >= 17) && one_in_(2))
				{
					/* Inner pillar */
					generate_fill(y0-1, x0-7, y0+1, x0-5, FEAT_WALL_INNER);

					/* Inner pillar */
					generate_fill(y0-1, x0+5, y0+1, x0+7, FEAT_WALL_INNER);
				}

				/* Two spaces */
				else
				{
					/* Inner pillar */
					generate_fill(y0-1, x0-6, y0+1, x0-4, FEAT_WALL_INNER);

					/* Inner pillar */
					generate_fill(y0-1, x0+4, y0+1, x0+6, FEAT_WALL_INNER);
				}
			}

			/* Occasionally, some inner rooms */
			else
			{
				/* Inner rectangle */
				generate_draw(y0-1, x0-5, y0+1, x0+5, FEAT_WALL_INNER);

				/* Secret doors (random top/bottom) */
				place_secret_door(y0 - 3 + (randint(2) * 2), x0 - 3);
				place_secret_door(y0 - 3 + (randint(2) * 2), x0 + 3);

				/* Monsters */
				spread_monsters('\0', monster_level + 1,
					rand_range(2, 4), y0, x0, e_y, e_x);

				/* Traps */
				spread_traps(randint(4), y0, x0, e_y, e_x, -1);

				/* Objects */
				if (one_in_(2)) place_object(y0, x0 - 2, FALSE, FALSE, FALSE);
				if (one_in_(2)) place_object(y0, x0 + 2, FALSE, FALSE, FALSE);
			}

			break;
		}

		/* An inner room with a checkerboard or a maze */
		case 3: case 4:
		{
			/* Checkerboards half of the time */
			if (one_in_(2))
			{
				/* Open the inner room with a secret door */
				if (inner) generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);

				/* Checkerboard */
				for (y = y1; y <= y2; y++)
				{
					for (x = x1; x <= x2; x++)
					{
						/* Build walls every other grid */
						if ((x + y) & (1))
						{
							cave_set_feat(y, x, FEAT_WALL_INNER);
						}
					}
				}
			}

			/* Build a maze */
			else
			{
				/* Maze is in an inner room */
				if (inner)
				{
					/* Draw maze */
					draw_maze(y1 - 1, x1 - 1, y2 + 1, x2 + 1,
						FEAT_WALL_INNER, FEAT_FLOOR);

					/* Place a secret door (vertical) */
					if (one_in_(2))
					{
						int t = rand_int(e_x / 2 - 1);
						place_secret_door(y1 - 1, x1 + t * 2);
					}

					/* Place a secret door (horizontal) */
					else
					{
						int t = rand_int(e_y / 2 - 1);
						place_secret_door(y1 + t * 2, x1 - 1);
					}
				}

				/* Maze occupies the entire room */
				else
				{
					/* Draw maze (use outer walls) */
					draw_maze(y1 - 1, x1 - 1, y2 + 1, x2 + 1,
						FEAT_WALL_OUTER, FEAT_FLOOR);

					/* Change most of the maze walls to inner walls */
					for (y = y1; y <= y2; y++)
					{
						for (x = x1; x <= x2; x++)
						{
							if (cave_feat[y][x] == FEAT_WALL_OUTER)
								cave_set_feat(y, x, FEAT_WALL_INNER);
						}
					}
				}
			}

			/* Monsters (especially undead) just love mazes. */
			if (one_in_(3))
			{
				spread_monsters('N', monster_level, rand_range(2, 4),
					y0, x0, e_y, e_x);
			}
			else if (one_in_(3))
			{
				spread_monsters('*', monster_level, rand_range(2, 4),
					y0, x0, e_y, e_x);
			}
			else
			{
				spread_monsters('\0', monster_level, rand_range(2, 4),
					y0, x0, e_y, e_x);
			}

			/* No random monsters in the maze */
			generate_mark(y1, x1, y2, x2, CAVE_TEMP);

			/* Traps make them entertaining */
			spread_traps(rand_range(2, 4), y0, x0, e_y, e_x, -1);

			/* Mazes should have some pretty good treasure too. */
			spread_objects(p_ptr->depth, rand_range(3, 4), y0, x0, e_y, e_x);

			break;
		}

		/* Four small rooms */
		case 5:
		{
			/* Inner "cross" */
			generate_plus(y1, x1, y2, x2, FEAT_WALL_INNER);

			/* Only inner room is subdivided */
			if (inner)
			{
				if (one_in_(2))
				{
					int i = randint((x2 - x1) / 2);
					place_secret_door(y1 - 1, x0 - i);
					place_secret_door(y1 - 1, x0 + i);
					place_secret_door(y2 + 1, x0 - i);
					place_secret_door(y2 + 1, x0 + i);
				}
				else
				{
					int i = randint((y2 - y1) / 2);
					place_secret_door(y0 + i, x1 - 1);
					place_secret_door(y0 - i, x1 - 1);
					place_secret_door(y0 + i, x2 + 1);
					place_secret_door(y0 - i, x2 + 1);
				}

				/* Treasure, centered at the center of the cross */
				spread_objects(p_ptr->depth, rand_range(3, 4), y0, x0, 1, 1);
			}

			/* Entire room is subdivided */
			else
			{
				/* Door in the center of the cross */
				place_secret_door(y0, x0);

				/* Treasure more spread out */
				spread_objects(p_ptr->depth, rand_range(3, 4), y0, x0, e_y, e_x);

			}

			/* Gotta have some monsters */
			spread_monsters('\0', monster_level, rand_range(5, 8), y0, x0,
				e_y, e_x);

			break;
		}
	}

	/* Success */
	return (TRUE);
}


/*
 * Type 5 -- Monster pits
 *
 * A monster pit is a large room, with an inner room filled with monsters.
 *
 * The type of monsters is determined by inputting the current dungeon
 * level into "mon_symbol_at_depth", and accepting the character returned.
 * After translating this into a set of selection criteria, monsters are
 * chosen and arranged in the inner room.
 *
 * Monster pits will never contain unique monsters.
 *
 * Note:
 * Monster pits are fun, but they are among the chief monster (and thus
 * object) inflators going.  We have therefore reduced their size in many
 * cases.
 */
static bool build_type5(void)
{
	int y, x, y0, x0, y1, x1, y2, x2;
	int i, j;
	int depth;

	int width;

	char name[DESC_LEN];
	char symbol;

	bool ordered = FALSE;
	bool dummy;
	int light = FALSE;

	/* Save level rating */
	int old_level_rating = level_rating;


	/* Pick a room size (less border walls) */
	y = 9;
	x = (one_in_(3) ? 23 : 13);

	/* Save the width */
	width = (x-5) / 2;

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, y+2, x+2)) return (FALSE);

	/* Locate the room */
	y1 = y0 - y / 2;
	x1 = x0 - x / 2;
	y2 =  y1 + y - 1;
	x2 =  x1 + x - 1;


	/* Generate new room.  Quit immediately if out of bounds. */
	if (!generate_room(y1-1, x1-1, y2+1, x2+1, light)) return (FALSE);


	/* Generate outer walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_OUTER);

	/* Generate inner floors */
	generate_fill(y1, x1, y2, x2, FEAT_FLOOR);

	/* Advance to the center room */
	y1 = y1 + 2;
	y2 = y2 - 2;
	x1 = x1 + 2;
	x2 = x2 - 2;

	/* Generate inner walls */
	generate_draw(y1-1, x1-1, y2+1, x2+1, FEAT_WALL_INNER);

	/* Open the inner room with a secret door */
	generate_hole(y1-1, x1-1, y2+1, x2+1, FEAT_SECRET);


	/* Lev 7- uses lev 5's set of monsters, lev 8+ uses lev 10's */
	i = ((p_ptr->depth + 2) / 5) - 1;

	/* Stay legal */
	if (i <  0) i =  0;
	if (i > 11) i = 11;

	/* Choose a monster type, using the chosen set. */
	symbol = mon_symbol_at_depth[i][rand_int(7)];


	/* Allow tougher monsters */
	depth = p_ptr->depth + 3 + div_round(p_ptr->depth, 12);

	/*
	 * Set monster generation restrictions.  Decide how to order
	 * monsters.  Get a description of the monsters.
	 */
	(void)strnfmt(name, sizeof(name), "%s", mon_restrict(symbol, (byte)depth, &ordered, FALSE));

	/* A default description probably means trouble, so stop. */
	if (streq(name, "misc") || !name[0]) return (TRUE);

	/* Verify probability table.  Leave the room empty on failure. */
	if (!get_mon_num(depth)) return (TRUE);


	/* Arrange the monsters in the room randomly. */
	if (!ordered)
	{
		int r_idx = 0;

		/* Hack -- tone down the monster level some */
		depth -= 3;

		/* Place some monsters */
		for (y = y0 - 2; y <= y0 + 2; y++)
		{
			for (x = x0 - width; x <= x0 + width; x++)
			{
				/* Get a monster index */
				r_idx = get_mon_num(depth);

				/* Place a single monster */
				(void)place_monster_aux(y, x, r_idx, FALSE, FALSE);
			}
		}
	}

	/* Arrange the monsters in the room in an orderly fashion. */
	else
	{
		s16b what[16];

		/* Pick some monster types */
		for (i = 0; i < 16; i++)
		{
			/* Get a monster index */
			what[i] = get_mon_num(depth);
		}

		/* Sort the monsters */
		for (i = 0; i < 16 - 1; i++)
		{
			for (j = 0; j < 16 - 1; j++)
			{
				int i1 = j;
				int i2 = j + 1;

				int p1 = r_info[what[i1]].level;
				int p2 = r_info[what[i2]].level;

				/* Bubble sort */
				if (p1 > p2)
				{
					int tmp = what[i1];
					what[i1] = what[i2];
					what[i2] = tmp;
				}
			}
		}

		/* Handle both widths */
		if (width == 9)
		{
			/* Top and bottom rows (outer) */
			for (x = x0 - 9; x <= x0 - 4; x++)
			{
				place_monster_aux(y0 - 2, x, what[2], FALSE, FALSE);
				place_monster_aux(y0 + 2, x, what[2], FALSE, FALSE);
			}
			for (x = x0 + 4; x <= x0 + 9; x++)
			{
				place_monster_aux(y0 - 2, x, what[2], FALSE, FALSE);
				place_monster_aux(y0 + 2, x, what[2], FALSE, FALSE);
			}

			/* Top and bottom rows (inner) */
			for (x = x0 - 3; x <= x0 + 3; x++)
			{
				place_monster_aux(y0 - 2, x, what[3], FALSE, FALSE);
				place_monster_aux(y0 + 2, x, what[3], FALSE, FALSE);
			}

			/* Outer and middle columns */
			for (y = y0 - 1; y <= y0 + 1; y++)
			{
				place_monster_aux(y, x0 - 9, what[2], FALSE, FALSE);
				place_monster_aux(y, x0 + 9, what[2], FALSE, FALSE);

				place_monster_aux(y, x0 - 8, what[4], FALSE, FALSE);
				place_monster_aux(y, x0 + 8, what[4], FALSE, FALSE);

				place_monster_aux(y, x0 - 7, what[5], FALSE, FALSE);
				place_monster_aux(y, x0 + 7, what[5], FALSE, FALSE);

				place_monster_aux(y, x0 - 6, what[6], FALSE, FALSE);
				place_monster_aux(y, x0 + 6, what[6], FALSE, FALSE);

				place_monster_aux(y, x0 - 5, what[7], FALSE, FALSE);
				place_monster_aux(y, x0 + 5, what[7], FALSE, FALSE);

				place_monster_aux(y, x0 - 4, what[8], FALSE, FALSE);
				place_monster_aux(y, x0 + 4, what[8], FALSE, FALSE);

				place_monster_aux(y, x0 - 3, what[9], FALSE, FALSE);
				place_monster_aux(y, x0 + 3, what[9], FALSE, FALSE);

				place_monster_aux(y, x0 - 2, what[11], FALSE, FALSE);
				place_monster_aux(y, x0 + 2, what[11], FALSE, FALSE);
			}
		}
		else if (width == 4)
		{
			/* Top and bottom rows (outer) */
			for (x = x0 - 4; x <= x0 - 3; x++)
			{
				place_monster_aux(y0 - 2, x, what[2], FALSE, FALSE);
				place_monster_aux(y0 + 2, x, what[2], FALSE, FALSE);
			}
			for (x = x0 + 3; x <= x0 + 4; x++)
			{
				place_monster_aux(y0 - 2, x, what[2], FALSE, FALSE);
				place_monster_aux(y0 + 2, x, what[2], FALSE, FALSE);
			}

			/* Top and bottom rows (inner) */
			for (x = x0 - 2; x <= x0 + 2; x++)
			{
				place_monster_aux(y0 - 2, x, what[3], FALSE, FALSE);
				place_monster_aux(y0 + 2, x, what[3], FALSE, FALSE);
			}

			/* Outer and middle columns */
			for (y = y0 - 1; y <= y0 + 1; y++)
			{
				place_monster_aux(y, x0 - 4, what[2], FALSE, FALSE);
				place_monster_aux(y, x0 + 4, what[2], FALSE, FALSE);

				place_monster_aux(y, x0 - 3, what[6], FALSE, FALSE);
				place_monster_aux(y, x0 + 3, what[6], FALSE, FALSE);

				place_monster_aux(y, x0 - 2, what[9], FALSE, FALSE);
				place_monster_aux(y, x0 + 2, what[9], FALSE, FALSE);
			}
		}

		/* Above/Below the center monster */
		for (x = x0 - 1; x <= x0 + 1; x++)
		{
			place_monster_aux(y0 + 1, x, what[12], FALSE, FALSE);
			place_monster_aux(y0 - 1, x, what[12], FALSE, FALSE);
		}

		/* Next to the center monster */
		place_monster_aux(y0, x0 + 1, what[14], FALSE, FALSE);
		place_monster_aux(y0, x0 - 1, what[14], FALSE, FALSE);

		/* Center monster */
		place_monster_aux(y0, x0, what[15], FALSE, FALSE);
	}

	/* Remove restrictions */
	(void)mon_restrict('\0', (byte)depth, &dummy, FALSE);


	/* Describe */
	if (cheat_room) msg_format("Monster pit (%s)", name);

	/* Precognition message */
	else if (can_precog(100, LEV_REQ_PRECOG))
		precog_msg(PRECOG_GEN_HORDE);


	/*
	 * Because OOD monsters in monster pits are generally less dangerous than
	 * they are when wandering around, apply only one-half the difference in
	 * level rating, and then add something for the sheer quantity of monsters.
	 */
	i = level_rating - old_level_rating;
	level_rating = old_level_rating + (i / 2) + (width <= 4 ? 5 : 10);

	/* Success */
	return (TRUE);
}



/*
 * Helper function to "build_type6".  Fill a room matching
 * the rectangle input with magma, and surround it with inner wall.
 * Create a door in a random inner wall grid along the border of the
 * rectangle.
 */
static void make_chamber(int c_y1, int c_x1, int c_y2, int c_x2)
{
	int i, d, y, x;
	int count;

	/* Fill with soft granite (will later be replaced with floor). */
	generate_fill(c_y1+1, c_x1+1, c_y2-1, c_x2-1, FEAT_MAGMA);

	/* Generate inner walls over dungeon granite and magma. */
	for (y = c_y1; y <= c_y2; y++)
	{
		/* left wall */
		x = c_x1;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	for (y = c_y1; y <= c_y2; y++)
	{
		/* right wall */
		x = c_x2;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	for (x = c_x1; x <= c_x2; x++)
	{
		/* top wall */
		y = c_y1;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	for (x = c_x1; x <= c_x2; x++)
	{
		/* bottom wall */
		y = c_y2;

		if ((cave_feat[y][x] == FEAT_WALL_EXTRA) ||
			(cave_feat[y][x] == FEAT_MAGMA))
			cave_set_feat(y, x, FEAT_WALL_INNER);
	}

	/* Try a few times to place a door. */
	for (i = 0; i < 20; i++)
	{
		/* Pick a square along the edge, not a corner. */
		if (one_in_(2))
		{
			/* Somewhere along the (interior) side walls. */
			if (one_in_(2)) x = c_x1;
			else x = c_x2;
			y = c_y1 + rand_int(1 + ABS(c_y2 - c_y1));
		}
		else
		{
			/* Somewhere along the (interior) top and bottom walls. */
			if (one_in_(2)) y = c_y1;
			else y = c_y2;
			x = c_x1 + rand_int(1 + ABS(c_x2 - c_x1));
		}

		/* If not an inner wall square, try again. */
		if (cave_feat[y][x] != FEAT_WALL_INNER) continue;

		/* Paranoia */
		if (!in_bounds_fully(y, x)) continue;

		/* Reset wall count */
		count = 0;

		/* If square has not more than two adjacent walls, and no adjacent doors, place door. */
		for (d = 0; d < 9; d++)
		{
			/* Extract adjacent (legal) location */
			int yy = y + ddy_ddd[d];
			int xx = x + ddx_ddd[d];

			/* No doors beside doors. */
			if (cave_feat[yy][xx] == FEAT_OPEN) break;

			/* Count the inner walls. */
			if (cave_feat[yy][xx] == FEAT_WALL_INNER) count++;

			/* No more than two walls adjacent (plus the one we're on). */
			if (count > 3) break;

			/* Checked every direction? */
			if (d == 8)
			{
				/* Place an open door. */
				cave_set_feat(y, x, FEAT_OPEN);

				/* Success. */
				return;
			}
		}
	}
}



/*
 * Expand in every direction from a start point, turning magma into rooms.
 * Stop only when the magma and the open doors totally run out.
 */
static void hollow_out_room(int y, int x)
{
	int d, yy, xx;

	for (d = 0; d < 9; d++)
	{
		/* Extract adjacent location */
		yy = y + ddy_ddd[d];
		xx = x + ddx_ddd[d];

		/* Change magma to floor. */
		if (cave_feat[yy][xx] == FEAT_MAGMA)
		{
			cave_set_feat(yy, xx, FEAT_FLOOR);

			/* Hollow out the room. */
			hollow_out_room(yy, xx);
		}
		/* Change open door to broken door. */
		else if (cave_feat[yy][xx] == FEAT_OPEN)
		{
			cave_set_feat(yy, xx, FEAT_BROKEN);

			/* Hollow out the (new) room. */
			hollow_out_room(yy, xx);
		}
	}
}



/*
 * Type 6 -- Rooms of chambers
 *
 * Build a room, varying in size between 22x22 and 44x66, consisting of
 * many smaller, irregularly placed, chambers all connected by doors or
 * short tunnels. -LM-
 *
 * Plop down an area-dependent number of magma-filled chambers, and remove
 * blind doors and tiny rooms.
 *
 * Hollow out a chamber near the center, connect it to new chambers, and
 * hollow them out in turn.  Continue in this fashion until there are no
 * remaining chambers within two squares of any cleared chamber.
 *
 * Clean up doors.  Neaten up the wall types.  Turn floor grids into rooms,
 * illuminate if requested.
 *
 * Fill the room with up to 35 (sometimes up to 50) monsters of a creature
 * race or type that offers a challenge at the character's depth.  This is
 * similar to monster pits, except that we choose among a wider range of
 * monsters.
 *
 * Special quest levels modify some of these steps.
 */
static bool build_type6(void)
{
	int i;
	int size_mod = 0;
	int d;
	int area, num_chambers;
	int y, x, yy, xx;
	int yy1, xx1, yy2, xx2, yy3, xx3;

	int height, width, count;

	int y0, x0, y1, x1, y2, x2;

	bool dummy;

	/* Monster generation variables. */
	s16b monsters_left, depth;
	char symbol;

	/* Description of monsters in room */
	char name[DESC_LEN];

	/* Deeper in the dungeon, chambers are less likely to be lit. */
	bool light = (rand_range(25, 60) > p_ptr->depth) ? TRUE : FALSE;

	/* Hack -- special quest level */
	if (p_ptr->special_quest)
	{
		height = dungeon_hgt;
		width  = dungeon_wid;
		light  = FALSE;
	}

	/* Standard chambered rooms */
	else
	{
		/* Rooms get (slightly) larger with depth */
		if (p_ptr->depth > rand_range(40, 140)) size_mod = 4;
		else size_mod = 3;

		/* Calculate the room size. */
		height = BLOCK_HGT * size_mod;
		width = BLOCK_WID * (size_mod + rand_int(3));
	}

	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Calculate the borders of the room. */
	y1 = y0 - (height / 2);
	x1 = x0 - (width / 2);
	y2 = y0 + (height - 1) / 2;
	x2 = x0 + (width - 1) / 2;

	/* Make certain the room does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);


	/* Determine how much space we have. */
	area = ABS(y2 - y1) * ABS(x2 - x1);

	/* Calculate the number of smaller chambers to make. */
	num_chambers = 10 + area / 80;

	/* Build the chambers. */
	for (i = 0; i < num_chambers; i++)
	{
		int c_y1, c_x1, c_y2, c_x2;
		int size, w, h;

		/* Determine size of chamber. */
		size = (p_ptr->special_quest ? 7 : 3) + rand_int(4);
		w = size + rand_int(10);
		h = size + rand_int(4);

		/* Pick an upper-left corner at random. */
		c_y1 = rand_range(y1, y2 - h);
		c_x1 = rand_range(x1, x2 - w);

		/* Determine lower-right corner of chamber. */
		c_y2 = c_y1 + h;
		c_x2 = c_x1 + w;

		/* Make me a (magma filled) chamber. */
		make_chamber(c_y1, c_x1, c_y2, c_x2);
	}

	/* Remove useless doors, fill in tiny, narrow rooms. */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			count = 0;

			/* Stay legal. */
			if (!in_bounds_fully(y, x)) continue;

			/* Check all adjacent grids. */
			for (d = 0; d < 8; d++)
			{
				/* Extract adjacent location */
				yy = y + ddy_ddd[d];
				xx = x + ddx_ddd[d];

				/* Count the walls and dungeon granite. */
				if ((cave_feat[yy][xx] == FEAT_WALL_INNER) ||
					(cave_feat[yy][xx] == FEAT_WALL_EXTRA)) count++;
			}

			/* Five adjacent walls:  Change non-chamber to wall. */
			if ((count == 5) && (cave_feat[y][x] != FEAT_MAGMA))
				cave_set_feat(y, x, FEAT_WALL_INNER);

			/* More than five adjacent walls:  Change anything to wall. */
			else if (count > 5) cave_set_feat(y, x, FEAT_WALL_INNER);
		}
	}

	/* Pick a random magma spot near the center of the room. */
	for (i = 0; i < 50; i++)
	{
		y = y1 + rand_spread(height / 2, height / 4);
		x = x1 + rand_spread(width / 2, width / 4);
		if (cave_feat[y][x] == FEAT_MAGMA) break;
	}

	/* Hollow out the first room. */
	cave_set_feat(y, x, FEAT_FLOOR);
	hollow_out_room(y, x);


	/* Attempt to change every in-room magma grid to open floor. */
	for (i = 0; i < 100; i++)
	{
		/* Assume this run will do no useful work. */
		bool joy = FALSE;

		/* Make new doors and tunnels between magma and open floor. */
		for (y = y1; y < y2; y++)
		{
			for (x = x1; x < x2; x++)
			{
				/* Current grid must be magma. */
				if (cave_feat[y][x] != FEAT_MAGMA) continue;

				/* Stay legal. */
				if (!in_bounds_fully(y, x)) continue;

				/* Check only horizontal and vertical directions. */
				for (d = 0; d < 4; d++)
				{
					/* Extract adjacent location */
					yy1 = y + ddy_ddd[d];
					xx1 = x + ddx_ddd[d];

					/* Find inner wall. */
					if (cave_feat[yy1][xx1] == FEAT_WALL_INNER)
					{
						/* Keep going in the same direction. */
						yy2 = yy1 + ddy_ddd[d];
						xx2 = xx1 + ddx_ddd[d];

						/* If we find open floor, place a door. */
						if ((in_bounds(yy2, xx2)) && (cave_feat[yy2][xx2] == FEAT_FLOOR))
						{
							joy = TRUE;

							/* Make a broken door in the wall grid. */
							cave_set_feat(yy1, xx1, FEAT_BROKEN);

							/* Hollow out the new room. */
							cave_set_feat(y, x, FEAT_FLOOR);
							hollow_out_room(y, x);

							break;
						}

						/* If we find more inner wall... */
						if ((in_bounds(yy2, xx2)) && (cave_feat[yy2][xx2] == FEAT_WALL_INNER))
						{
							/* ...Keep going in the same direction. */
							yy3 = yy2 + ddy_ddd[d];
							xx3 = xx2 + ddx_ddd[d];

							/* If we /now/ find floor, make a tunnel. */
							if ((in_bounds(yy3, xx3)) && (cave_feat[yy3][xx3] == FEAT_FLOOR))
							{
								joy = TRUE;

								/* Turn both wall grids into floor. */
								cave_set_feat(yy1, xx1, FEAT_FLOOR);
								cave_set_feat(yy2, xx2, FEAT_FLOOR);

								/* Hollow out the new room. */
								cave_set_feat(y, x, FEAT_FLOOR);
								hollow_out_room(y, x);

								break;
							}
						}
					}
				}
			}
		}

		/* If we could find no work to do, stop. */
		if (!joy) break;
	}


	/* Turn broken doors into a random kind of door, remove open doors. */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			if (cave_feat[y][x] == FEAT_OPEN)
				cave_set_feat(y, x, FEAT_WALL_INNER);
			else if (cave_feat[y][x] == FEAT_BROKEN)
				place_random_door(y, x);
		}
	}


	/* Turn all walls and magma not adjacent to floor into dungeon granite. */
	/* Turn all floors and adjacent grids into rooms, sometimes lighting them. */
	for (y = (y1-1 > 0 ? y1-1 : 0) ;
		y < (y2+2 < dungeon_hgt ? y2+2 : dungeon_hgt) ; y++)
	{
		for (x = (x1-1 > 0 ? x1-1 : 0) ;
			x < (x2+2 < dungeon_wid ? x2+2 : dungeon_wid) ; x++)
		{
			if ((cave_feat[y][x] == FEAT_WALL_INNER) ||
				(cave_feat[y][x] == FEAT_MAGMA))
			{
				for (d = 0; d < 9; d++)
				{
					/* Extract adjacent location */
					yy = y + ddy_ddd[d];
					xx = x + ddx_ddd[d];

					/* Stay legal */
					if (!in_bounds(yy, xx)) continue;

					/* No floors allowed */
					if (cave_feat[yy][xx] == FEAT_FLOOR) break;

					/* Turn me into dungeon granite. */
					if (d == 8)
					{
						cave_set_feat(y, x, FEAT_WALL_EXTRA);
					}
				}
			}
			if (cave_feat[y][x] == FEAT_FLOOR)
			{
				for (d = 0; d < 9; d++)
				{
					/* Extract adjacent location */
					yy = y + ddy_ddd[d];
					xx = x + ddx_ddd[d];

					/* Stay legal */
					if (!in_bounds(yy, xx)) continue;

					/* Turn into room. */
					cave_info[yy][xx] |= (CAVE_ROOM);

					/* Illuminate if requested. */
					if (light) cave_info[yy][xx] |= (CAVE_GLOW);
				}
			}
		}
	}


	/* Turn all inner wall grids adjacent to dungeon granite into outer walls. */
	for (y = (y1-1 > 0 ? y1-1 : 0) ; y < (y2+2 < dungeon_hgt ? y2+2 : dungeon_hgt) ; y++)
	{
		for (x = (x1-1 > 0 ? x1-1 : 0) ; x < (x2+2 < dungeon_wid ? x2+2 : dungeon_wid) ; x++)
		{
			/* Stay legal. */
			if (!in_bounds_fully(y, x)) continue;

			if (cave_feat[y][x] == FEAT_WALL_INNER)
			{
				for (d = 0; d < 9; d++)
				{
					/* Extract adjacent location */
					yy = y + ddy_ddd[d];
					xx = x + ddx_ddd[d];

					/* Look for dungeon granite */
					if (cave_feat[yy][xx] == FEAT_WALL_EXTRA)
					{
						/* Turn me into outer wall. */
						cave_set_feat(y, x, FEAT_WALL_OUTER);

						/* Done */
						break;
					}
				}
			}
		}
	}

	/* Special quest levels use another method of adding monsters */
	if (p_ptr->special_quest) return (TRUE);


	/*** Now we get to place the monsters. ***/

	/* Lev 7- uses lev 5's set of monsters, lev 8+ uses lev 10's */
	i = ((p_ptr->depth + 2) / 5) - 1;

	/* Stay legal */
	if (i <  0) i =  0;
	if (i > 11) i = 11;

	/* Choose a monster type, using the chosen set. */
	symbol = mon_symbol_at_depth[i][rand_int(13)];


	/* Allow (slightly) tougher monsters (up to 5 OoD). */
	depth = p_ptr->depth + MIN(div_round(p_ptr->depth, 15), 5);

	/* Set monster generation restrictions.  Describe the monsters. */
	(void)strnfmt(name, sizeof(name), "%s", mon_restrict(symbol, (byte)depth, &dummy, TRUE));

	/* A default description probably means trouble, so stop. */
	if (streq(name, "misc") || !name[0]) return (TRUE);


	/* No normal monsters. */
	generate_mark(y1, x1, y2, x2, CAVE_TEMP);


	/* Usually, we want about 20 to 40 monsters. */
	monsters_left = (size_mod * 5) + rand_range(10, 20);

	/* Fewer monsters near the surface. */
	if (p_ptr->depth < 45) monsters_left = 5 + 2 * p_ptr->depth / 3;

	/* More monsters of kinds that tend to be weak. */
	if (strchr("abciBF", symbol)) monsters_left += 10;

	/* Place the monsters. */
	for (i = 0; i < 300; i++)
	{
		/* Check for early completion. */
		if (!monsters_left) break;

		/* Pick a random in-room square. */
		y = rand_range(y1, y2);
		x = rand_range(x1, x2);

		/* Require a floor square with no monsters or objects in it. */
		if (!cave_naked_bold(y, x)) continue;

		/* Place a single monster.  Sleeping 2/3rds of the time. */
		place_monster_aux(y, x, get_mon_num(depth),
			(!one_in_(3)), FALSE);

		/* One less monster to place. */
		monsters_left--;
	}

	/* Remove our restrictions. */
	(void)mon_restrict('\0', (byte)depth, &dummy, FALSE);


	/* Message for testers */
	if (cheat_room)
	{
		/* Room type */
		msg_format("Room of chambers (%s)", name);
	}

	/* Precognition message */
	else if (can_precog(100, LEV_REQ_PRECOG))
		precog_msg(PRECOG_GEN_COMMUNITY);


	/* Success */
	return (TRUE);
}



/*
 * Apply any general restrictions on monsters in vaults.
 */
static void general_monster_restrictions(void)
{
	int i;

	/* Clear global monster restriction variables. */
	allow_unique = TRUE;
	for (i = 0; i < 10; i++) d_char_req[i] = '\0';
	for (i = 0; i < 4; i++) d_attr_req[i] = 0;
	racial_flag_mask = 0, breath_flag_mask = 0;

	/* No monster restrictions */
	get_mon_num_hook = NULL;

	/* Un-apply monster restrictions */
	get_mon_num_prep();
}


/*
 * Hack -- fill in "vault" rooms
 */
static bool build_vault(int y0, int x0, int ymax, int xmax, cptr data,
	bool light, bool icky, byte type)
{
	int i, j, k;
	int y, x, y1, x1, y2, x2;
	int c, len;
	int temp;
	u16b cave_flag = 0;

	cptr t;
	char racial_symbol[128];
	int racial_symbol_num = 0;

	/* Allow vertical and horizontal flipping */
	bool flip_vert = (bool)rand_int(2);
	bool flip_hori = (bool)rand_int(2);

	/* Calculate the borders of the vault. */
	y1 = y0 - (ymax / 2);
	x1 = x0 - (xmax / 2);
	y2 = y1 + ymax - 1;
	x2 = x1 + xmax - 1;


	/* Make certain that the vault does not cross the dungeon edge. */
	if ((!in_bounds(y1, x1)) || (!in_bounds(y2, x2))) return (FALSE);


	/* Always a room */
	cave_flag |= (CAVE_ROOM);

	/* Vault cannot usually be teleported around in */
	if (icky) cave_flag |= (CAVE_ICKY);

	/* Optional light */
	if (light) cave_flag |= (CAVE_GLOW);

	/* No random monsters in vaults and interesting rooms. */
	cave_flag |= (CAVE_TEMP);


	/*
	 * It is possible for vaults to have monster restrictions that take
	 * effect if no other restrictions are currently in force.
	 */
	general_monster_restrictions();


	/* Place dungeon features and objects */
	for (t = data, i = 0; i < ymax; i++)
	{
		for (j = 0; j < xmax; t++)
		{
			len = (byte)*t;

			/* Hack -- high bit indicates a run */
			if (len & 0x80)
			{
				len ^= 0x80;
				t++;
			}
			else
			{
				len = 1;
			}

			/* Extract encoded run */
			for (c = 0; c < len; j++, c++)
			{
				/* Hack -- skip "non-grids" */
				if (*t == ' ')
				{
					continue;
				}

				/* Allow vertical flips */
				if (flip_vert) y = y2 - i;
				else           y = y1 + i;

				/* Allow horizontal flips */
				if (flip_hori) x = x2 - j;
				else           x = x1 + j;

				/* Lay down a floor */
				cave_set_feat(y, x, FEAT_FLOOR);

				/* Add cave_info flags */
				cave_info[y][x] |= cave_flag;

				/* Analyze the grid */
				switch (*t)
				{
					/* Granite wall (outer) */
					case '%':
					{
						if (p_ptr->special_quest) cave_set_feat(y, x, FEAT_FLOOR);
						else cave_set_feat(y, x, FEAT_WALL_OUTER);
						break;
					}
					/* Granite wall (inner) */
					case '#':
					{
						cave_set_feat(y, x, FEAT_WALL_INNER);
						break;
					}
					/* Permanent wall (inner) */
					case 'X':
					{
						cave_set_feat(y, x, FEAT_PERM_INNER);
						break;
					}
					/* Treasure seam, in either magma or quartz. */
					case '*':
					{
						if (one_in_(2))
							cave_set_feat(y, x, FEAT_MAGMA_K);
						else cave_set_feat(y, x, FEAT_QUARTZ_K);
						break;
					}
					/* Lava. */
					case '@':
					{
						cave_set_feat(y, x, FEAT_LAVA);
						break;
					}
					/* Water. */
					case 'x':
					{
						cave_set_feat(y, x, FEAT_WATER);
						break;
					}
					/* Tree. */
					case ';':
					{
						cave_set_feat(y, x, FEAT_TREE);
						break;
					}
					/* Rubble (sometimes with hidden treasure). */
					case ':':
					{
						cave_set_feat(y, x, FEAT_RUBBLE);
						if (one_in_(25)) place_trap(y, x, TRAP_LOOSE_ROCK, p_ptr->depth);
						break;
					}
					/* Treasure/trap */
					case '\'':
					{
						if (one_in_(2))
						{
							place_object(y, x, FALSE, FALSE, FALSE);
						}
						else
						{
							place_trap(y, x, -1, p_ptr->depth);
						}
						break;
					}
					/* Secret doors */
					case '+':
					{
						place_secret_door(y, x);
						break;
					}
					/* Trap */
					case '^':
					{
						place_trap(y, x, -1, p_ptr->depth);
						break;
					}
					/* Up stairs  */
					case '<':
					{
						cave_set_feat(y, x, FEAT_LESS);
						break;
					}
					/* Down stairs. */
					case '>':
					{
						/* No down stairs at bottom or on quests */
						if ((quest_check(p_ptr->depth) == QUEST_FIXED) ||
							(p_ptr->depth >= MAX_DEPTH - 1)) break;

						cave_set_feat(y, x, FEAT_MORE);
						break;
					}
				}
			}
		}
	}

	/* Place dungeon monsters and objects */
	for (t = data, i = 0; i < ymax; i++)
	{
		for (j = 0; j < xmax; t++)
		{
			len = (byte)*t;

			/* Hack -- high bit indicates a run */
			if (len & 0x80)
			{
				len ^= 0x80;
				t++;
			}
			else
			{
				len = 1;
			}

			/* Extract encoded run */
			for (c = 0; c < len; j++, c++)
			{
				/* Hack -- skip "non-grids" */
				if (*t == ' ') continue;

				/* Allow vertical flips */
				if (flip_vert) y = y2 - i;
				else           y = y1 + i;

				/* Allow horizontal flips */
				if (flip_hori) x = x2 - j;
				else           x = x1 + j;


				/* Most alphabetic characters signify monster races. */
				if (((isalpha(*t)) || (*t == '&')) && (*t != 'x') &&
				    (*t != 'X'))
				{
					/* If the symbol is not yet stored, ... */
					if ((!strchr(racial_symbol, *t)) && (racial_symbol_num < 128))
					{
						/* ... store it for later processing. */
						racial_symbol[racial_symbol_num++] = *t;
					}
				}

				/* Otherwise, analyze the symbol */
				else switch (*t)
				{
					/* An ordinary monster, object (sometimes good), or trap. */
					case '1':
					{
						int choice = rand_int(4);

						if (choice < 2)
						{
							place_monster(y, x, TRUE, TRUE);
						}

						/* I had not intended this function to create
						 * guaranteed "good" quality objects, but perhaps
						 * it's better that it does at least sometimes.
						 */
						else if (choice == 2)
						{
							if (one_in_(10)) place_object(y, x, TRUE,
								FALSE, FALSE);
							else place_object(y, x, FALSE, FALSE, FALSE);
						}
						else
						{
							place_trap(y, x, -1, p_ptr->depth);
						}
						break;
					}
					/* Slightly out of depth monster. */
					case '2':
					{
						monster_level = p_ptr->depth + 3;
						place_monster(y, x, TRUE, TRUE);
						monster_level = p_ptr->depth;
						break;
					}
					/* Slightly out of depth object. */
					case '3':
					{
						object_level = p_ptr->depth + 3;
						place_object(y, x, FALSE, FALSE, FALSE);
						object_level = p_ptr->depth;
						break;
					}
					/* Monster and/or object */
					case '4':
					{
						if (one_in_(2))
						{
							monster_level = p_ptr->depth + 4;
							place_monster(y, x, TRUE, TRUE);
							monster_level = p_ptr->depth;
						}
						if (one_in_(2))
						{
							object_level = p_ptr->depth + 4;
							place_object(y, x, FALSE, FALSE, FALSE);
							object_level = p_ptr->depth;
						}
						break;
					}
					/* Out of depth object. */
					case '5':
					{
						object_level = p_ptr->depth + 6;
						place_object(y, x, FALSE, FALSE, FALSE);
						object_level = p_ptr->depth;
						break;
					}
					/* Out of depth monster. */
					case '6':
					{
						monster_level = p_ptr->depth + 6;
						place_monster(y, x, TRUE, TRUE);
						monster_level = p_ptr->depth;
						break;
					}

					/* Very out of depth object. */
					case '7':
					{
						object_level = p_ptr->depth + 15;
						place_object(y, x, FALSE, FALSE, FALSE);
						object_level = p_ptr->depth;
						break;
					}
					/* Very out of depth monster. */
					case '8':
					{
						monster_level = p_ptr->depth + 12;
						place_monster(y, x, TRUE, TRUE);
						monster_level = p_ptr->depth;
						break;
					}
					/* Meaner monster, plus "good" (or better) object */
					case '9':
					{
						monster_level = p_ptr->depth + 10;
						place_monster(y, x, TRUE, TRUE);
						monster_level = p_ptr->depth;
						object_level = p_ptr->depth + 5; /* +10 for good */
						place_object(y, x, TRUE, FALSE, FALSE);
						object_level = p_ptr->depth;
						break;
					}

					/* Nasty monster and "great" (or better) object */
					case '0':
					{
						monster_level = p_ptr->depth + 15;
						place_monster(y, x, TRUE, TRUE);
						monster_level = p_ptr->depth;
						object_level = p_ptr->depth + 10; /* +10 for good */
						place_object(y, x, TRUE, TRUE, FALSE);
						object_level = p_ptr->depth;
						break;
					}

					/* A chest. */
					case '~':
					{
						required_tval = TV_CHEST;

						object_level = p_ptr->depth + 5;
						place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Treasure. */
					case '$':
					{
						place_gold(y, x);
						break;
					}
					/* Armor. */
					case ']':
					{
						object_level = p_ptr->depth + 3;

						if (one_in_(3)) temp = randint(9);
						else temp = randint(8);

						if      (temp == 1) required_tval = TV_BOOTS;
						else if (temp == 2) required_tval = TV_GLOVES;
						else if (temp == 3) required_tval = TV_HELM;
						else if (temp == 4) required_tval = TV_CROWN;
						else if (temp == 5) required_tval = TV_SHIELD;
						else if (temp == 6) required_tval = TV_CLOAK;
						else if (temp == 7) required_tval = TV_SOFT_ARMOR;
						else if (temp == 8) required_tval = TV_HARD_ARMOR;
						else required_tval = TV_DRAG_ARMOR;

						place_object(y, x, TRUE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Weapon. */
					case '|':
					{
						object_level = p_ptr->depth + 3;

						temp = randint(3);

						if      (temp == 1) required_tval = TV_SWORD;
						else if (temp == 2) required_tval = TV_POLEARM;
						else if (temp == 3) required_tval = TV_HAFTED;

						place_object(y, x, TRUE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Ring. */
					case '=':
					{
						required_tval = TV_RING;

						object_level = p_ptr->depth + 3;
						if (one_in_(4))
							place_object(y, x, TRUE, FALSE, TRUE);
						else place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Amulet. */
					case '"':
					{
						required_tval = TV_AMULET;

						object_level = p_ptr->depth + 3;
						if (one_in_(4))
							place_object(y, x, TRUE, FALSE, TRUE);
						else place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Potion. */
					case '!':
					{
						required_tval = TV_POTION;

						object_level = p_ptr->depth + 3;
						if (one_in_(4))
							place_object(y, x, TRUE, FALSE, TRUE);
						else place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Scroll. */
					case '?':
					{
						required_tval = TV_SCROLL;

						object_level = p_ptr->depth + 3;
						if (one_in_(4))
							place_object(y, x, TRUE, FALSE, TRUE);
						else place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Staff. */
					case '_':
					{
						required_tval = TV_STAFF;

						object_level = p_ptr->depth + 3;
						if (one_in_(4))
							place_object(y, x, TRUE, FALSE, TRUE);
						else place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Wand or rod. */
					case '-':
					{
						if (one_in_(2)) required_tval = TV_WAND;
						else required_tval = TV_ROD;

						object_level = p_ptr->depth + 3;
						if (one_in_(4))
							place_object(y, x, TRUE, FALSE, TRUE);
						else place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
					/* Food or mushroom. */
					case ',':
					{
						required_tval = TV_FOOD;

						object_level = p_ptr->depth + 3;
						place_object(y, x, FALSE, FALSE, TRUE);
						object_level = p_ptr->depth;

						required_tval = 0;

						break;
					}
				}
			}
		}
	}

	/*
	 * To avoid rebuilding the monster list too often (which can quickly
	 * get expensive), we handle monsters of a specified race separately.
	 */
	for (k = 0; k < racial_symbol_num; k++)
	{
		/* Require correct race, allow uniques. */
		allow_unique = TRUE;
		d_char_req[0] = racial_symbol[k];
		d_char_req[1] = '\0';

		/* Hack -- handle dragons */
		if ((d_char_req[0] == 'D') || (d_char_req[0] == 'd'))
		{
			d_char_req[0] = '\0';
			racial_flag_mask = RF3_DRAGON;
		}

		/* Hack -- handle demons */
		else if ((d_char_req[0] == '&') || (d_char_req[0] == 'I'))
		{
			d_char_req[0] = '\0';
			racial_flag_mask = RF3_DEMON;
		}

		/* Determine level of monster */
		if      (type == 0) temp = p_ptr->depth + 3;
		else if (type == 7) temp = p_ptr->depth;
		else if (type == 8) temp = p_ptr->depth + 3;
		else if (type == 9) temp = p_ptr->depth + 6;
		else                temp = p_ptr->depth;

		/* Apply our restrictions */
		get_mon_num_hook = mon_select;

		/* Prepare allocation table */
		get_mon_num_prep();

		/* Place the monsters */
		for (t = data, i = 0; i < ymax; i++)
		{
			for (j = 0; j < xmax; t++)
			{
				len = (byte)*t;

				/* Hack -- high bit indicates a run */
				if (len & 0x80)
				{
					len ^= 0x80;
					t++;
				}
				else
				{
					len = 1;
				}

				/* Extract encoded run */
				for (c = 0; c < len; j++, c++)
				{
					/* Allow vertical flips */
					if (flip_vert) y = y2 - i;
					else           y = y1 + i;

					/* Allow horizontal flips */
					if (flip_hori) x = x2 - j;
					else           x = x1 + j;

					/* Place this type of monster */
					if (*t == racial_symbol[k])
					{
						/* Place a monster */
						place_monster_aux(y, x, get_mon_num(temp), FALSE, FALSE);
					}
				}
			}
		}
	}

	/* Clear any current monster restrictions. */
	if (get_mon_num_hook)
	{
		get_mon_num_hook = NULL;
		get_mon_num_prep();
	}

	/* Success. */
	return (TRUE);
}


/*
 * Type 7 -- interesting rooms. -LM-
 */
static bool build_type7(void)
{
	vault_type *v_ptr;
	int i, y, x;
	s16b *v_idx;
	int v_cnt = 0;

	/* Allocate the "v_idx" array */
	C_MAKE(v_idx, z_info->v_max, s16b);


	/* Examine each vault */
	for (i = 0; i < z_info->v_max; i++)
	{
		/* Access the vault */
		v_ptr = &v_info[i];

		/* Accept each interesting room that is acceptable for this depth. */
		if ((v_ptr->typ == 7) && (v_ptr->min_lev <= p_ptr->depth) &&
		    (v_ptr->max_lev >= p_ptr->depth))
		{
			v_idx[v_cnt++] = i;
		}
	}

	/* Note if there are no vaults available */
	if (!v_cnt)
	{
		/* Free the array */
		FREE(v_idx);

		/* Leave */
		return (FALSE);
	}

	/* Access a random vault record */
	v_ptr = &v_info[v_idx[rand_int(v_cnt)]];

	if (!find_space(&y, &x, v_ptr->hgt, v_ptr->wid)) return (FALSE);

	/* Boost the rating */
	level_rating += v_ptr->rat;


	/* Build the vault (sometimes lit, not icky, type 7) */
	if (!build_vault(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text,
		(p_ptr->depth < rand_int(37)), FALSE, 7)) return (FALSE);

	/* Free the array */
	FREE(v_idx);

	return (TRUE);
}


/*
 * Type 8 -- lesser vaults.
 */
static bool build_type8(void)
{
	vault_type *v_ptr;
	int i, y, x;
	s16b *v_idx;
	int v_cnt = 0;

	/* Allocate the "v_idx" array */
	C_MAKE(v_idx, z_info->v_max, s16b);


	/* Examine each vault */
	for (i = 0; i < z_info->v_max; i++)
	{
		/* Access the vault */
		v_ptr = &v_info[i];

		/* Accept each lesser vault that is acceptable for this depth. */
		if ((v_ptr->typ == 8) && (v_ptr->min_lev <= p_ptr->depth) &&
		    (v_ptr->max_lev >= p_ptr->depth))
		{
			v_idx[v_cnt++] = i;
		}
	}

	/* Note if there are no vaults available */
	if (!v_cnt)
	{
		/* Free the array */
		FREE(v_idx);

		/* Leave */
		return (FALSE);
	}

	/* Access a random vault record */
	v_ptr = &v_info[v_idx[rand_int(v_cnt)]];


	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y, &x, v_ptr->hgt, v_ptr->wid)) return (FALSE);

	/* Build the vault (never lit, icky, type 8) */
	if (!build_vault(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text,
		FALSE, TRUE, 8)) return (FALSE);


	/* Boost the rating */
	level_rating += v_ptr->rat;


	/* Message for testers */
	if (cheat_room) msg_format("Lesser vault (%s)", v_name + v_ptr->name);

	/* Precognition message */
	else if (can_precog(100, LEV_REQ_PRECOG))
		precog_msg(PRECOG_GEN_DANGEROUS);

	/* Free the array */
	FREE(v_idx);

	return (TRUE);
}


/*
 * Type 9 -- greater vaults.
 */
static bool build_type9(void)
{
	vault_type *v_ptr;
	int i, y, x;
	s16b *v_idx;
	int v_cnt = 0;

	/* Allocate the "v_idx" array */
	C_MAKE(v_idx, z_info->v_max, s16b);


	/* Examine each vault */
	for (i = 0; i < z_info->v_max; i++)
	{
		/* Access the vault */
		v_ptr = &v_info[i];

		/* Accept each greater vault that is acceptable for this depth. */
		if ((v_ptr->typ == 9) && (v_ptr->min_lev <= p_ptr->depth) &&
		    (v_ptr->max_lev >= p_ptr->depth))
		{
			v_idx[v_cnt++] = i;
		}
	}


	/* Note if there are no vaults available */
	if (!v_cnt)
	{
		/* Free the array */
		FREE(v_idx);

		/* Leave */
		return (FALSE);
	}

	/* Access a random vault record */
	v_ptr = &v_info[v_idx[rand_int(v_cnt)]];


	/* Find and reserve some space in the dungeon.  Get center of room. */
	if (!find_space(&y, &x, v_ptr->hgt, v_ptr->wid)) return (FALSE);

	/* Build the vault (never lit, icky, type 9) */
	if (!build_vault(y, x, v_ptr->hgt, v_ptr->wid, v_text + v_ptr->text,
		FALSE, TRUE, 9)) return (FALSE);


	/* Boost the rating */
	level_rating += v_ptr->rat;

	/* Message for testers */
	if (cheat_room) msg_format("Greater vault (%s)", v_name + v_ptr->name);

	/* Precognition message */
	else if (can_precog(100, LEV_REQ_PRECOG + 15))
		precog_msg(PRECOG_GEN_VERY_DANGEROUS);
	else if (can_precog(100, LEV_REQ_PRECOG))
		precog_msg(PRECOG_GEN_DANGEROUS);

	/* Free the array */
	FREE(v_idx);

	return (TRUE);
}


/*
 * Type 10 -- Extremely large rooms.
 *
 * These are the largest, most difficult to position, and thus highest-
 * priority rooms in the dungeon.  They should be rare, so as not to
 * interfere with greater vaults.
 *
 *                     (huge chamber)
 * - A single starburst-shaped room of extreme size, usually dotted or
 * even divided with irregularly-shaped fields of rubble. No special
 * monsters.
 *
 */
static bool build_type10(void)
{
	bool light;

	int i, count;

	int y0, x0, y1, x1, y2, x2;
	int y1_tmp, x1_tmp, y2_tmp, x2_tmp;
	int width, height;
	int width_tmp, height_tmp;


	/* This room is usually lit. */
	if (!one_in_(3)) light = TRUE;
	else light = FALSE;

	/* Get a size */
	height = (rand_range(3, 5)) * BLOCK_HGT;
	width  = (rand_range(4, 7)) * BLOCK_WID;

	/* Find and reserve some space.  Get center of room. */
	if (!find_space(&y0, &x0, height, width)) return (FALSE);

	/* Locate the room */
	y1 = y0 - height / 2;
	x1 = x0 - width / 2;
	y2 = y1 + height - 1;
	x2 = x1 + width - 1;

	/* Make a huge starburst room with optional light. */
	if (!generate_starburst_room(y1, x1, y2, x2, light,
		FEAT_FLOOR, FALSE)) return (FALSE);


	/* Often, add rubble to break things up a bit. */
	if (!one_in_(3))
	{
		/* Determine how many rubble fields to add (between 1 and 6). */
		count = height * width * randint(2) / 1100;

		/* Make the rubble fields. */
		for (i = 0; i < count; i++)
		{
			height_tmp = rand_range( 8, 16);
			width_tmp  = rand_range(12, 24);

			/* Semi-random location. */
			y1_tmp = y1 + rand_int(height - height_tmp);
			x1_tmp = x1 + rand_int(width - width_tmp);
			y2_tmp = y1_tmp + height_tmp;
			x2_tmp = x1_tmp + width_tmp;

			/* Make the rubble field. */
			generate_starburst_room(y1_tmp, x1_tmp, y2_tmp,
				x2_tmp, FALSE, FEAT_RUBBLE, FALSE);
		}
	}

	/* Success. */
	return (TRUE);
}



/*
 * Helper function that reads the room_data table and returns the number
 * of rooms, of all types, we should build on this level.
 *
 * In order to avoid excessive number of monsters or objects, neither of
 * which the game design handles all that well, we usually forbid more
 * than a few really special rooms on any level.
 */
static void num_rooms_allowed(void)
{
	int allowed;
	int base_num, num_tries, i;
	int room_type;


	/* Run through all the room types */
	for (room_type = 0; room_type < ROOM_MAX; room_type++)
	{
		/* Point to the room information. */
		room_data *rm_ptr = &room[room_type];

		/* Assume no rooms of this type */
		dun->room_num[room_type] = allowed = 0;

		/* No rooms allowed above their minimum depth. */
		if (p_ptr->depth < rm_ptr->min_level) continue;

		/* No special limit on ordinary rooms. */
		if (room_type == 1)
		{
			dun->room_num[room_type] = DUN_ROOMS;
			continue;
		}

		/* Choose a set of possible monsters, using (randomized) depth */
		base_num = rm_ptr->room_gen_num[MIN(10, div_round(p_ptr->depth, 10))];

		/* Find out how many times we'll try to boost the room count. */
		num_tries = 3 * base_num / 100;
		if (num_tries < 2) num_tries = (base_num <= 12 ? 1 : 2);

		/* Try several times to increase the number of rooms to build. */
		for (i = 0; i < num_tries; i++)
		{
			if (rand_int(1000) < 10 * base_num / num_tries)
			{
				allowed++;
			}
		}

		/* Store the number of rooms of that type we should build. */
		dun->room_num[room_type] = allowed;
	}

	/* By default, limit special rooms */
	if (!UNLIMITED_SPECIAL_ROOMS)
	{
		/* Limit number of certain kinds of rooms on any level */
		if (dun->room_num[10] > 1) dun->room_num[10] = 1;
		if (dun->room_num[9]  > 1) dun->room_num[9]  = 1;
		if (dun->room_num[8]  > 2) dun->room_num[8]  = 2;
		if (dun->room_num[6]  > 1) dun->room_num[6]  = 1;
		if (dun->room_num[5]  > 1) dun->room_num[5]  = 1;

		/* We have a greater vault */
		if (dun->room_num[9])
		{
			/* No pits or chambers, maximum 1 lesser vault */
			dun->room_num[5] = 0;
			dun->room_num[6] = 0;
			if (dun->room_num[8] > 1) dun->room_num[8] = 1;
		}

		/* We have a room of chambers or a pit */
		else if ((dun->room_num[6]) || (dun->room_num[5]))
		{
			/* No pits and chambers together, maximum 1 lesser vault */
			if (dun->room_num[6])     dun->room_num[5] = 0;
			if (dun->room_num[8] > 1) dun->room_num[8] = 1;
		}
	}
}


/*
 * Build a room of the given type.
 *
 * Check to see if there will probably be enough space in the monster
 * and object arrays.
 */
static bool room_build(int room_type)
{
	/* If trying to build a special room, check some limits first. */
	if (room_type > 4)
	{
		/* Help prevent object over-flow */
		if (o_max > 3 * z_info->o_max / 4)
		{
			return (FALSE);
		}

		/* Help prevent monster over-flow */
		if (m_max > 3 * z_info->m_max / 4)
		{
			return (FALSE);
		}
	}

	/* Build a room */
	switch (room_type)
	{
		/* Find space for, position, and build the room asked for */
		case 10: if (!build_type10()) return (FALSE); break;
		case  9: if (!build_type9())  return (FALSE); break;
		case  8: if (!build_type8())  return (FALSE); break;
		case  7: if (!build_type7())  return (FALSE); break;
		case  6: if (!build_type6())  return (FALSE); break;
		case  5: if (!build_type5())  return (FALSE); break;
		case  4: if (!build_type4())  return (FALSE); break;
		case  3: if (!build_type3())  return (FALSE); break;
		case  2: if (!build_type2())  return (FALSE); break;
		case  1: if (!build_type1())  return (FALSE); break;

		/* Paranoia */
		default: return (FALSE);
	}

	/* Success */
	return (TRUE);
}



/**************************************************************/
/*                                                            */
/*                     The tunneling code                    */
/*                                                            */
/**************************************************************/


/*
 * Given a current position (y1, x1), move towards the target grid
 * (y2, x2) either vertically or horizontally.
 *
 * If both vertical and horizontal directions seem equally good,
 * prefer to move horizontally.
 */
static void correct_dir(int *row_dir, int *col_dir, int y1, int x1, int y2, int x2)
{
	/* Move vertically if vertical distance to target is greater. */
	if (ABS(y1 - y2) > ABS(x1 - x2))
	{
		*row_dir = ((y1 < y2) ? 1 : -1);
		*col_dir = 0;
	}

	/* Prefer to move horizontally. */
	else
	{
		*row_dir = 0;
		*col_dir = ((x1 < x2) ? 1 : -1);
	}
}


/*
 * Go in a semi-random direction from current location to target location.
 * Do not actually head away from the target grid.  Always make a turn.
 */
static void adjust_dir(int *row_dir, int *col_dir, int y1, int x1, int y2, int x2)
{
	/* Always turn 90 degrees. */
	if (*row_dir == 0)
	{
		*col_dir = 0;

		/* On the y-axis of target - freely choose a side to turn to. */
		if (y1 == y2) *row_dir = ((one_in_(2)) ? - 1 : 1);

		/* Never turn away from target. */
		else *row_dir = ((y1 < y2) ? 1 : -1);
	}
	else
	{
		*row_dir = 0;

		/* On the x-axis of target - freely choose a side to turn to. */
		if (x1 == x2) *col_dir = ((one_in_(2)) ? - 1 : 1);

		/* Never turn away from target. */
		else *col_dir = ((x1 < x2) ? 1 : -1);
	}
}


/*
 * Go in a completely random orthogonal direction.  If we turn around
 * 180 degrees, save the grid; it may be a good place to place stairs
 * and/or the player.
 */
static void rand_dir(int *row_dir, int *col_dir, int y, int x)
{
	/* Pick a random direction */
	int i = rand_int(4);

	/* Extract the dy/dx components */
	int row_dir_tmp = ddy_ddd[i];
	int col_dir_tmp = ddx_ddd[i];

	/* Save useful grids. */
	if ((-(*row_dir) == row_dir_tmp) && (-(*col_dir) == col_dir_tmp))
	{
		/* Save the current tunnel location if surrounded by walls. */
		if ((in_bounds_fully(y, x)) && (dun->stair_n < STAIR_MAX) &&
			(next_to_walls(y, x) == 4))
		{
			dun->stair[dun->stair_n].y = y;
			dun->stair[dun->stair_n].x = x;
			dun->stair_n++;
		}
	}

	/* Save the new direction. */
	*row_dir = row_dir_tmp;
	*col_dir = col_dir_tmp;
}


/*
 * Given a set of coordinates, return the index number of the room occupying
 * the dungeon block this location is in.
 */
static int get_room_index(int y, int x)
{
	/* Which block are we in? */
	int by = y / BLOCK_HGT;
	int bx = x / BLOCK_WID;

	/* Paranoia -- confirm that block is in the dungeon. */
	if ((by > dun->row_rooms) || (by > dun->col_rooms))
		return (-1);

	/* Get the room index. */
	return (dun->room_map[by][bx] - 1);
}



/*
 * Search for a vault entrance.
 *
 * Notes:
 * - This function looks in both directions, and chooses the nearest
 *   entrance (if it has a choice).
 * - We assume rooms will have outer walls surrounding them.
 * - We assume the vault designer hasn't designed false entrances, or
 *   done something else really sneaky.
 */
static bool find_entrance(int row_dir, int col_dir, int *row1, int *col1)
{
	int i, j;
	int y;
	int x;
	int dy, dx;

	/*
	 * Initialize entrances found while looking in both directions, and
	 * the distances to them.
	 */
	int target_y[2] = {0, 0};
	int target_x[2] = {0, 0};
	int grids[2]    = {0, 0};


	/* Search in both directions. */
	for (i = 0; i < 2; i++)
	{
		bool stop_loop = FALSE;

		y = *row1;
		x = *col1;

		dy = row_dir;
		dx = col_dir;

		/* Keep running through the steps. */
		while (TRUE)
		{
			int dy_tmp = dy;

			/* Search grids on both sides for more impassable walls. */
			for (j = i; j < 2 + i; j++)
			{
				if (dy_tmp == 0)
				{
					dy = ((j == 1) ? - 1 : 1);
					dx = 0;
				}
				else
				{
					dy = 0;
					dx = ((j == 1) ? - 1 : 1);
				}

				/* Look in chosen direction. */
				if ((!cave_permwall(y + dy, x + dx)) &&
					(cave_feat[y + dy][x + dx] != FEAT_WALL_OUTER))
				{
					/*
					 * Check the grid after this one.  If it belongs
					 * to the same room, we've found an entrance.
					 */
					if (get_room_index(y + dy, x + dx) ==
						get_room_index(y + dy + dy, x + dx + dx))
					{
						target_y[i] = y + dy;
						target_x[i] = x + dx;
						break;
					}
				}

				/* Look again. */
				else if (cave_permwall(y + dy, x + dx))
				{
					break;
				}

				/* We're out on some kind of weird spur. */
				else if (j == (1 + i))
				{
					/* Stop travelling in this direction. */
					stop_loop = TRUE;
					break;
				}
			}

			/* Success or (known) failure. */
			if (target_y[i] && target_x[i]) break;
			if (stop_loop) break;

			/* Keep heading in the same direction. */
			while (TRUE)
			{
				/* Advance to new grid in our direction of travel. */
				y += dy;
				x += dx;

				/* Count the number of grids we've travelled */
				grids[i]++;

				/*
				 * We're back where we started.  Room either has no
				 * entrances, or we can't find them.
				 */
				if ((y == *row1) && (x == *col1))
				{
					stop_loop = TRUE;
					break;
				}

				/* We have hit the dungeon edge. */
				if (!in_bounds_fully(y + dy, x + dx))
				{
					stop_loop = TRUE;
					break;
				}

				/* Next grid is outer wall. */
				if (cave_feat[y + dy][x + dx] == FEAT_WALL_OUTER)
				{
					/* We need to make another turn. */
					break;
				}

				/* Next grid is alterable, and not outer wall. */
				else if (!cave_permwall(y + dy, x + dx))
				{
					/*
					 * Check the grid after this one.  If it belongs
					 * to the same room, we've found an entrance.
					 */
					if (get_room_index(y + dy, x + dx) ==
						get_room_index(y + dy + dy, x + dx + dx))
					{
						target_y[i] = y + dy;
						target_x[i] = x + dx;
						break;
					}

					/*
					 * If we're in the same room, our likely best move
					 * is to keep moving along the permanent walls.
					 */
					else
					{
						break;
					}
				}
			}

			/* Success. */
			if (target_y[i] && target_x[i]) break;

			/* Failure. */
			if (stop_loop) break;
		}
	}

	/*
	 * Compare reports.  Pick the only target available, or choose
	 * the target that took less travelling to get to.
	 */
	if ((target_y[0] && target_x[0]) && (target_y[1] && target_x[1]))
	{
		if (grids[0] < grids[1])
		{
			*row1 = target_y[0];
			*col1 = target_x[0];
		}
		else
		{
			*row1 = target_y[1];
			*col1 = target_x[1];
		}

		return (TRUE);
	}

	else if (target_y[0] && target_x[0])
	{
		*row1 = target_y[0];
		*col1 = target_x[0];
		return (TRUE);
	}
	else if (target_y[1] && target_x[1])
	{
		*row1 = target_y[1];
		*col1 = target_x[1];
		return (TRUE);
	}

	/* No entrances found. */
	else return (FALSE);
}

/*
 * Tests suitability of potential entranceways, and places doors if appropriate.
 */
static void try_entrance(int y0, int x0)
{
	int i, k;

	/* Require walls on at least two sides. */
	for (k = 0, i = 0; i < 4; i++)
	{
		/* Extract the location */
		int y = y0 + ddy_ddd[i];
		int x = x0 + ddx_ddd[i];

		/* Ignore non-walls. */
		if (cave_feat[y][x] < FEAT_MAGMA) continue;

		/* We require at least two walls. */
		if ((++k) == 2)
		{
			place_random_door(y0, x0);
		}
	}
}

/*
 * Places door at y, x position if at least 2 walls and two corridor spaces found
 */
static void try_door(int y0, int x0)
{
	int i, y, x;
	int k = 0;


	/* Ignore walls */
	if (cave_wall_bold(y0, x0)) return;

	/* Ignore room grids */
	if (cave_info[y0][x0] & (CAVE_ROOM)) return;

	/* Occasional door (if allowed) */
	if (rand_int(100) < DUN_TUN_JCT)
	{
		/* Count the orthogonally adjacent non-wall grids */
		for (i = 0; i < 4; i++)
		{
			/* Extract the location */
			y = y0 + ddy_ddd[i];
			x = x0 + ddx_ddd[i];

			/* Skip walls */
			if (cave_wall_bold(y, x)) continue;

			/* Skip grids inside rooms */
			if (cave_info[y][x] & (CAVE_ROOM)) continue;

			/* We require at least two non-walls outside of rooms. */
			if ((k++) == 2) break;
		}

		if (k >= 2)
		{
			/* Check Vertical */
			if ((cave_feat[y0-1][x0] >= FEAT_MAGMA) &&
			    (cave_feat[y0+1][x0] >= FEAT_MAGMA))
			{
				place_random_door(y0, x0);
			}

			/* Check Horizontal */
			else if ((cave_feat[y0][x0-1] >= FEAT_MAGMA) &&
			    (cave_feat[y0][x0+1] >= FEAT_MAGMA))
			{
				place_random_door(y0, x0);
			}
		}
	}
}




/*
 * Constructs a tunnel between two points.
 *
 * The tunneling code connects room centers together.  It is the respon-
 * sibility of rooms to ensure all grids in them are accessible from the
 * center, or from a passable grid nearby if the center is a wall.
 *
 * (warnings)
 * This code is still beta-quality.  Use with care.  Known areas of
 * weakness include:
 * - A group of rooms may be connected to each other, and not to the rest
 *   of the dungeon.  This problem is rare.
 * - While the entrance-finding code is very useful, sometimes the tunnel
 *   gets lost on the way.
 * - On occasion, a tunnel will travel far too long.  It can even (rarely)
 *   happen that it would lock up the game if not artificially stopped.
 * - There are number of minor but annoying problems, both old and new,
 *   like excessive usage of tunnel grids, tunnels turning areas of the
 *   dungeon into Swiss cheese, and so on.
 * - This code is awfully, awfully long.  And complicated.
 *
 * (Handling the outer walls of rooms)
 * In order to place doors correctly, know when a room is connected, and
 * keep entrances and exits to rooms neat, we set and use several different
 * kinds of granite.  Because of this, we must call this function before
 * making seams.
 * - "Outer" walls must surround rooms.  The code can handle outer walls
 * up to two grids thick (which is common in non-rectangular rooms).
 * - When outer wall is pierced, "solid" walls are created along the axis
 * perpendicular to the direction of movement for three grids in each
 * direction.  This makes entrances tidy.
 *
 * (Handling difficult terrain)
 * When an unalterable (permanent) wall is encountered, this code is
 * capable of finding entrances and of using waypoints.  It is anticipated
 * that this will make vaults behave better than they did.
 *
 * Useful terrain values:
 *   FEAT_WALL_EXTRA -- granite walls
 *   FEAT_WALL_INNER -- inner room walls
 *   FEAT_WALL_OUTER -- outer room walls
 *   FEAT_WALL_SOLID -- solid room walls
 *   FEAT_PERM_EXTRA -- permanent walls (town)
 *   FEAT_PERM_INNER -- inner room walls (perma)
 *   FEAT_PERM_OUTER -- outer room walls (perma)
 *   FEAT_PERM_SOLID -- dungeon border (perma)
 */
static void build_tunnel(int start_room, int end_room)
{
	int i = 0, j = 0, tmp, y, x;
	int y0, x0, y1, x1;
	int dy, dx;

	int row_dir, col_dir;


	/* Get start and target grids. */
	int row1 = dun->cent[start_room].y;
	int col1 = dun->cent[start_room].x;
	int row2 = dun->cent[end_room].y;
	int col2 = dun->cent[end_room].x;
	int tmp_row = row1, tmp_col = col1;


	/* Store initial target, because we may have to use waypoints. */
	int initial_row2 = row2;
	int initial_col2 = col2;

	/* Not yet worried about our progress */
	int desperation = 0;

	/* Start out not allowing the placement of doors */
	bool door_flag = FALSE;

	/* Don't leave just yet */
	bool leave = FALSE;

	/* Not heading for a known entrance. */
	bool head_for_entrance = FALSE;

	/* Initialize some movement counters */
	int adjust_dir_timer = randint(DUN_TUN_ADJ * 2);
	int rand_dir_timer   = randint(DUN_TUN_RND * 2);
	int correct_dir_timer = 0;


	/* Set number of tunnel grids and room entrances to zero. */
	dun->tunn_n = 0;
	dun->wall_n = 0;

	/* Start out heading in the correct direction */
	correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

	/* Keep going until done (or look like we're getting nowhere). */
	while ((row1 != initial_row2) || (col1 != initial_col2))
	{
		/* Stop when tunnel is too long, or we want to stop. */
		if ((leave) || (dun->tunn_n == TUNN_MAX) || (j++ == 400)) break;

		/*
		 * If we've reached a waypoint, the source and destination rooms
		 * should be connected to each other now, but they may not be to
		 * the rest of the network.  Get another room center at random,
		 * and move towards it.
		 */
		if ((row1 == row2) && (col1 == col2))
		{
			while (TRUE)
			{
				i = rand_int(dun->cent_n);
				if ((i != start_room) && (i != end_room)) break;
			}

			row2 = initial_row2 = dun->cent[i].y;
			col2 = initial_col2 = dun->cent[i].x;

			head_for_entrance = FALSE;
		}

		/* Try moving randomly if we seem stuck. */
		else if ((row1 != tmp_row) && (col1 != tmp_col))
		{
			desperation++;

			/* Try a 90 degree turn. */
			if (desperation == 1)
			{
				adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);
				adjust_dir_timer = 3;
			}

			/* Try turning randomly. */
			else if (desperation < 4)
			{
				rand_dir(&row_dir, &col_dir, row1, col1);
				correct_dir_timer = 2;
			}
			else
			{
				/* We've run out of ideas.  Stop wasting time. */
				break;
			}
		}

		/* We're making progress. */
		else
		{
			/* No worries. */
			desperation = 0;

			/* Check room. */
			tmp = get_room_index(row1, col1);

			/* We're in our destination room - head straight for target. */
			if ((tmp == end_room) && (cave_info[row1][col1] & (CAVE_ROOM)))
			{
				correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);
			}

			else
			{
				/* Count down times until next movement changes. */
				if (adjust_dir_timer > 0) adjust_dir_timer--;
				if (rand_dir_timer > 0) rand_dir_timer--;
				if (correct_dir_timer > 0) correct_dir_timer--;

				/* Make a random turn, set timer. */
				if (rand_dir_timer == 0)
				{
					rand_dir(&row_dir, &col_dir, row1, col1);

					rand_dir_timer = randint(DUN_TUN_RND * 2);
					correct_dir_timer = randint(4);
				}

				/* Adjust direction, set timer. */
				else if (adjust_dir_timer == 0)
				{
					adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

					adjust_dir_timer = randint(DUN_TUN_ADJ * 2);
				}


				/* Go in correct direction. */
				else if (correct_dir_timer == 0)
				{
					correct_dir(&row_dir, &col_dir, row1, col1, row2, col2);

					/* Don't use again unless needed. */
					correct_dir_timer = -1;
				}
			}
		}


		/* Get the next location */
		tmp_row = row1 + row_dir;
		tmp_col = col1 + col_dir;

		/* Do not leave the dungeon */
		if (!in_bounds_fully(tmp_row, tmp_col))
		{
			/* Adjust direction */
			adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

			/* Get the next location */
			tmp_row = row1 + row_dir;
			tmp_col = col1 + col_dir;

			/* Our destination is illegal - stop. */
			if (!in_bounds_fully(tmp_row, tmp_col)) break;
		}

		/* Tunnel through dungeon granite. */
		if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_EXTRA)
		{
			/* Accept and save the current tunnel location */
			if (dun->tunn_n < TUNN_MAX)
			{
				dun->tunn[dun->tunn_n].y = row1 = tmp_row;
				dun->tunn[dun->tunn_n].x = col1 = tmp_col;
				dun->tunn_n++;
			}

			/* Allow door in next grid */
			door_flag = TRUE;

			continue;
		}


		/* Pierce outer walls of rooms. */
		else if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_OUTER)
		{
			/* Look ahead */
			y0 = tmp_row + row_dir;
			x0 = tmp_col + col_dir;

			/* Hack -- delay turns */
			adjust_dir_timer++;  rand_dir_timer++;

			/* Navigate around various kinds of walls */
			if ((cave_feat[y0][x0] == FEAT_WALL_SOLID) ||
			    (cave_feat[y0][x0] == FEAT_WALL_INNER) ||
			    (cave_permwall(y0, x0)))
			{
				/* Three rounds -- increasing unwillingness to be stopped */
				for (i = 0; i < 6; i++)
				{
					if ((i == 0) || (i == 3))
					{
						/* Check the more direct route first. */
						adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

						/* Verify that we haven't just been here. */
						if ((dun->tunn_n == 0) ||
						    (dun->tunn[dun->tunn_n - 1].y != row1 + row_dir) ||
						    (dun->tunn[dun->tunn_n - 1].x != col1 + col_dir))
						{
							tmp_row = row1 + row_dir;
							tmp_col = col1 + col_dir;
						}

						else continue;
					}

					else
					{
						/* If that didn't work, try the other side. */
						tmp_row = row1 - row_dir;
						tmp_col = col1 - col_dir;
					}

					/* Refuse to drill through solid walls */
					if (cave_permwall(tmp_row, tmp_col)) continue;
					if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_SOLID) continue;

					/* Try not to drill through outer or inner walls */
					if ((cave_feat[tmp_row][tmp_col] == FEAT_WALL_OUTER) &&
					    (i < 2)) continue;
					if ((cave_feat[tmp_row][tmp_col] == FEAT_WALL_INNER) &&
					    (i < 4)) continue;

					/* Accept the location */
					row1 = tmp_row;
					col1 = tmp_col;

					/* Save the current tunnel location */
					if (dun->tunn_n < TUNN_MAX)
					{
						dun->tunn[dun->tunn_n].y = row1;
						dun->tunn[dun->tunn_n].x = col1;
						dun->tunn_n++;
					}

					/* Move on */
					break;
				}
			}

			/* Handle a double line of outer walls robustly. */
			else if (cave_feat[y0][x0] == FEAT_WALL_OUTER)
			{
				/* Look ahead (again). */
				y1 = y0 + row_dir;
				x1 = x0 + col_dir;

				/* We've found something passable. */
				if (cave_passable_bold(y1, x1) || cave_any_door(y1, x1))
				{
					/* Turn both outer wall grids into floor. */
					cave_set_feat(tmp_row, tmp_col, FEAT_FLOOR);
					cave_set_feat(y0, x0, FEAT_FLOOR);

					/* Save the wall location */
					if (dun->wall_n < WALL_MAX)
					{
						dun->wall[dun->wall_n].y = tmp_row;
						dun->wall[dun->wall_n].x = tmp_col;
						dun->wall_n++;
					}

					/* Accept this location */
					row1 = tmp_row = y0;
					col1 = tmp_col = x0;
				}

				/* No luck - look at the sides. */
				else
				{
					for (i = 0; i < 2; i++)
					{
						if (i == 0)
						{
							/* Check the more direct route first. */
							adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

							tmp_row = row1 + row_dir;
							tmp_col = col1 + col_dir;
						}
						else
						{
							/* If that didn't work, try the other side. */
							tmp_row = row1 - row_dir;
							tmp_col = col1 - col_dir;
						}

						if ((!cave_permwall(tmp_row, tmp_col)) &&
						    (cave_feat[tmp_row][tmp_col] != FEAT_WALL_SOLID) &&
						    (cave_feat[tmp_row][tmp_col] != FEAT_WALL_OUTER) &&
						    (cave_feat[tmp_row][tmp_col] != FEAT_WALL_INNER))
						{
							/* Accept the location */
							row1 = tmp_row;
							col1 = tmp_col;

							/* Save the current tunnel location */
							if (dun->tunn_n < TUNN_MAX)
							{
								dun->tunn[dun->tunn_n].y = row1;
								dun->tunn[dun->tunn_n].x = col1;
								dun->tunn_n++;
							}

							/* Continue */
							break;
						}
					}
				}
			}

			/* Second grid contains any other kind of terrain. */
			else
			{
				/* Accept this location */
				row1 = tmp_row;
				col1 = tmp_col;

				/* If impassable, replace with floor */
				if (!cave_passable_bold(row1, col1) && !cave_any_door(row1, col1))
					cave_set_feat(row1, col1, FEAT_FLOOR);

				/* Save the wall location */
				if (dun->wall_n < WALL_MAX)
				{
					dun->wall[dun->wall_n].y = row1;
					dun->wall[dun->wall_n].x = col1;
					dun->wall_n++;
				}
			}

			/* Forbid re-entry near this piercing. */
			if ((!cave_permwall(row1 + row_dir, col1 + col_dir)) &&
				(cave_info[row1][col1] & (CAVE_ROOM)))
			{
				if (row_dir)
				{
					for (x = col1 - 3; x <= col1 + 3; x++)
					{
						/* Convert adjacent "outer" walls */
						if ((in_bounds(row1, x)) &&
						    (cave_feat[row1][x] == FEAT_WALL_OUTER))
						{
							/* Change the wall to a "solid" wall */
							cave_set_feat(row1, x, FEAT_WALL_SOLID);
						}
					}
				}
				else
				{
					for (y = row1 - 3; y <= row1 + 3; y++)
					{
						/* Convert adjacent "outer" walls */
						if ((in_bounds(y, col1)) &&
						    (cave_feat[y][col1] == FEAT_WALL_OUTER))
						{
							/* Change the wall to a "solid" wall */
							cave_set_feat(y, col1, FEAT_WALL_SOLID);
						}
					}
				}

				/* Get current room (use next grid). */
				tmp = get_room_index(row1, col1);

				/* Record our success (but not if exiting into dungeon granite). */
				if ((tmp != start_room) && (tmp != -1) &&
					(cave_feat[row1 + row_dir][col1 + col_dir] !=
					FEAT_WALL_EXTRA))
				{
					/* If this room is connected, now our start room is too. */
					if (dun->connected[tmp])
					{
						dun->connected[start_room] = TRUE;

						/* If our destination room is connected, we're done. */
						if (dun->connected[end_room]) leave = TRUE;
					}

					/* If our start room was connected, this one is too. */
					else if (dun->connected[start_room])
						dun->connected[tmp] = TRUE;
				}

				continue;
			}
		}


		/*
		 * We've hit a feature that can't be altered.
		 */
		else if (cave_permwall(tmp_row, tmp_col))
		{
			/* We don't know what to do. */
			if (!head_for_entrance)
			{
				/* Get the room that occupies this block. */
				tmp = get_room_index(tmp_row, tmp_col);

				/* We're in our starting room. */
				if (tmp == start_room)
				{
					/* Look at next grid. */
					y = tmp_row + row_dir;
					x = tmp_col + col_dir;

					/* If the next grid is outer wall, we know we need
					 * to find an entrance.  Otherwise, travel through
					 * the wall.
					 */
					if (cave_feat[y][x] != FEAT_WALL_OUTER)
					{
						row1 = tmp_row;
						col1 = tmp_col;
						continue;
					}
				}

				y = tmp_row;
				x = tmp_col;

				/* We need to find an entrance to this room. */
				if (!find_entrance(row_dir, col_dir, &y, &x))
				{
					/* No entrance means insoluble trouble. */
					leave = TRUE;
					continue;
				}

				/* We're in our starting room. */
				if (tmp == start_room)
				{
					/* Jump immediately to entrance. */
					row1 = tmp_row = y;
					col1 = tmp_col = x;

					/* Look for outer wall to head for. */
					for (i = 0; i < 4; i++)
					{
						y = row1 + ddy_ddd[i];
						x = col1 + ddx_ddd[i];

						if (cave_feat[y][x] == FEAT_WALL_OUTER)
						{
							/* Aim for outer wall. */
							row_dir = ddy_ddd[i];
							col_dir = ddx_ddd[i];

							adjust_dir_timer = 2;
						}
					}
				}

				/* We're anywhere else. */
				else
				{
					/* Aim for given waypoint. */
					row2 = y;
					col2 = x;

					/* Reset the final target. */
					initial_row2 = y;
					initial_col2 = x;

					/* Enter "head for entrance" mode. */
					head_for_entrance = TRUE;
				}
			}

			/* We're heading for an entrance to a vault. */
			if (head_for_entrance)
			{
				/* Check both sides. */
				for (i = 0; i < 2; i++)
				{
					/*
					 * Try going in the direction that best approaches
					 * the target first.  On the 2nd try, check the
					 * opposite side.
					 */
					if (col_dir == 0)
					{
						dy = 0;
						if (i == 0) dx = ((col1 < col2) ?  1 : -1);
						else        dx = ((col1 < col2) ? -1 :  1);
					}
					else
					{
						dx = 0;
						if (i == 0) dy = ((row1 < row2) ?  1 : -1);
						else        dy = ((row1 < row2) ? -1 :  1);
					}

					/* Check to see if grid to this side is alterable. */
					if (!cave_permwall(row1 + dy, col1 + dx))
					{
						/* Change direction. */
						row_dir = dy;
						col_dir = dx;

						/* Accept this location */
						row1 += row_dir;
						col1 += col_dir;

						/* If impassable, replace with floor */
						if (!cave_passable_bold(row1, col1) && !cave_any_door(row1, col1))
							cave_set_feat(row1, col1, FEAT_FLOOR);

						/* Return to main loop. */
						break;
					}

					/* We seem to be in trouble. */
					else if (i == 1)
					{
						/* If we previously found floor, accept the floor. */
						if (cave_feat[row1 -(dy)][col1 -(dx)] == FEAT_FLOOR)
						{
							/* Change direction. */
							row_dir = -(dy);
							col_dir = -(dx);

							/* Accept this location */
							row1 += row_dir;
							col1 += col_dir;

							break;
						}

						/* Otherwise, go backwards. */
						{
							/* Change direction. */
							row_dir = -(row_dir);
							col_dir = -(col_dir);

							/* Accept this location */
							row1 += row_dir;
							col1 += col_dir;

							break;
						}
					}
				}
			}
		}

		/* We've hit a solid wall. */
		else if (cave_feat[tmp_row][tmp_col] == FEAT_WALL_SOLID)
		{
			/* Check both sides, most direct route first. */
			for (i = 0; i < 2; i++)
			{
				if (i == 0)
				{
					/* Check the more direct route first. */
					adjust_dir(&row_dir, &col_dir, row1, col1, row2, col2);

					tmp_row = row1 + row_dir;
					tmp_col = col1 + col_dir;
				}
				else
				{
					/* If that didn't work, try the other side. */
					tmp_row = row1 - row_dir;
					tmp_col = col1 - col_dir;
				}

				if ((!cave_permwall(tmp_row, tmp_col)) &&
					(cave_feat[tmp_row][tmp_col] != FEAT_WALL_SOLID) &&
					(cave_feat[tmp_row][tmp_col] != FEAT_WALL_INNER))
				{
					/* Accept the location */
					row1 = tmp_row;
					col1 = tmp_col;

					/* Save the current tunnel location */
					if (dun->tunn_n < TUNN_MAX)
					{
						dun->tunn[dun->tunn_n].y = row1;
						dun->tunn[dun->tunn_n].x = col1;
						dun->tunn_n++;
					}

					/* Move on. */
					i = 2;
				}
			}

			continue;
		}

		/* Travel quickly through rooms. */
		else if (cave_info[tmp_row][tmp_col] & (CAVE_ROOM))
		{
			/* Accept the location */
			row1 = tmp_row;
			col1 = tmp_col;

			continue;
		}

		/*
		 * Handle all passable terrain outside of rooms (this is
		 * usually another corridor).
		 */
		else if (cave_passable_bold(tmp_row, tmp_col))
		{
			/* We've hit another tunnel. */
			if (cave_feat[tmp_row][tmp_col] == FEAT_FLOOR)
			{
				/* Collect legal door locations */
				if (door_flag)
				{
					/* Save the door location */
					if (dun->door_n < DOOR_MAX)
					{
						dun->door[dun->door_n].y = tmp_row;
						dun->door[dun->door_n].x = tmp_col;
						dun->door_n++;
					}

					/* No door in next grid */
					door_flag = FALSE;
				}

				/* Mark start room connected. */
				dun->connected[start_room] = TRUE;

				/*
				 * If our destination room isn't connected, jump to
				 * its center, and head towards the start room.
				 */
				if (dun->connected[end_room] == FALSE)
				{
					/* Swap rooms. */
					tmp = end_room;
					end_room = start_room;
					start_room = tmp;

					/* Re-initialize */
					row1 = dun->cent[start_room].y;
					col1 = dun->cent[start_room].x;
					row2 = dun->cent[end_room].y;
					col2 = dun->cent[end_room].x;
					initial_row2 = row2;
					initial_col2 = col2;
					tmp_row = row1, tmp_col = col1;
				}
				else
				{
					/* All done. */
					leave = TRUE;
				}

				continue;
			}

			/* Grid is not another tunnel.  Advance, make no changes. */
			row1 = tmp_row;
			col1 = tmp_col;

			continue;
		}
	}

	/* Turn the tunnel into corridor */
	for (i = 0; i < dun->tunn_n; i++)
	{
		/* Access the grid */
		y = dun->tunn[i].y;
		x = dun->tunn[i].x;

		/* If impassable, replace with floor */
		if (!cave_passable_bold(y, x) && !cave_any_door(y, x)) cave_set_feat(y, x, FEAT_FLOOR);
	}

	/* Make doors in entranceways. */
	for (i = 0; i < dun->wall_n; i++)
	{
		/* Access the grid */
		y = dun->wall[i].y;
		x = dun->wall[i].x;

		/* Sometimes, make a door in the entranceway */
		if (rand_int(100) < DUN_TUN_PEN) try_entrance(y, x);
	}


	/* We've reached the target.  If one room was connected, now both are. */
	if ((row1 == initial_row2) && (col1 == initial_col2))
	{
		if (dun->connected[start_room])
			dun->connected[end_room] = TRUE;
		else if (dun->connected[end_room])
			dun->connected[start_room] = TRUE;
	}
}


/*
 * Build a special quest level.
 *
 * This function is a hack.  I admit it.
 */
static void quest_cave_gen(void)
{
	int by, bx, i, num;

	/* Build a room of chambers filling the entire level! */
	(void)build_type6();

	/* Hack -- clear some room blocks in the center of the dungeon */
	for (by = dun->row_rooms/2 - 2; by < dun->row_rooms/2 + 2; by++)
	{
		for (bx = dun->col_rooms/2 - 3; bx < dun->col_rooms/2 + 3; bx++)
		{
			dun->room_map[by][bx] = 0;
		}
	}

	/* Build a greater vault, or at least a lesser one */
	if (p_ptr->depth >= room[9].min_level)
	{
		if (!build_type9()) (void)build_type8();
	}
	else
	{
		(void)build_type8();
	}

	/* Build a few lakes */
	num = rand_int(4);

	for (i = 0; i < num; i++)
	{
		int size_y = rand_range(5, 10);
		int size_x = rand_range(7, 15);
		int y1 = rand_range(size_y, dungeon_hgt - size_y);
		int y2 = y1 + (2 * size_y);
		int x1 = rand_range(size_x, dungeon_wid - size_x);
		int x2 = x1 + (2 * size_x);

		(void)generate_starburst_room(y1, x1, y2, x2, FALSE, FEAT_WATER, FALSE);
	}

	/* Quest levels are "special" */
	good_item_flag = TRUE;
}


/*
 * The cool thing about destroyed levels is that a player ghost is
 * always present on one.
 */
static bool place_ghost(void)
{
	int i;
	int r_idx = 0;
	int cur_lev = 0;
	monster_race *r_ptr;


	/* Cannot have more than one player ghost on a level */
	if (bones_selector) return (FALSE);

	/* Scan the monster list */
	for (i = 1; i < z_info->r_max; i++)
	{
		r_ptr = &r_info[i];

		/* Require player ghosts */
		if (!(r_ptr->flags2 & (RF2_PLAYER_GHOST))) continue;

		/* Require that ghost be placeable */
		if (r_ptr->cur_num >= r_ptr->max_num) continue;

		/* Require that ghost template level not be too high */
		if (r_ptr->level > Rand_normal(p_ptr->depth, 1)) continue;

		/* Save the highest-level ghost */
		if (r_ptr->level > cur_lev)
		{
			r_idx = i;
			cur_lev = r_ptr->level;
		}
	}

	/* Place the player ghost -- always awake */
	if (r_idx)
	{
		int y, x;

		/* Get the ghost */
		r_ptr = &r_info[r_idx];

		/* Find a legal, unoccupied, space */
		while (TRUE)
		{
			/* Pick a location */
			y = rand_int(dungeon_hgt);
			x = rand_int(dungeon_wid);

			/* Do not place player ghosts in vaults */
			if (cave_info[y][x] & (CAVE_ICKY)) continue;

			/* Do not place player ghosts in marked rooms */
			if (cave_info[y][x] & (CAVE_TEMP)) continue;

			/* Require a grid that the ghost can exist in. */
			if (!cave_exist_mon(r_ptr, y, x, FALSE, FALSE)) continue;

			break;
		}

		/* Place the ghost */
		return (place_monster_aux(y, x, r_idx, FALSE, FALSE));
	}

	return (FALSE);
}


/*
 * Generate a new dungeon level.  Build up to DUN_ROOMS rooms, type by type, in
 * descending order of size and difficulty.
 *
 * Build the dungeon borders, scramble and connect the rooms.  Place stairs,
 * doors, and random monsters, objects, and traps.  Place any quest monsters.
 *
 * We mark grids "icky" to indicate the presence of a vault.
 * We mark grids "temp" to prevent random monsters being placed there.
 */
static void cave_gen(void)
{
	int i, j, y, x, y1, x1;
	int by, bx;

	int num_to_build;
	int room_type;
	int rooms_built = 0;
	int bonus;

	int old_level_rating;

	bool destroyed = FALSE;
	bool dummy;

	int start_mon_num;

	dun_data dun_body;

	/* Global data */
	dun = &dun_body;


	/* Not a special quest level */
	if (!p_ptr->special_quest)
	{
		/* It is possible for non-quest levels to be destroyed */
		if ((p_ptr->depth > 5) && (!quest_check(p_ptr->depth)) &&
		    (one_in_(DEST_LEVEL_CHANCE)))
		{
			destroyed = TRUE;
		}
	}


	/* Hack -- Start with basic granite (or floor, if empty) */
	for (y = 0; y < dungeon_hgt; y++)
	{
		for (x = 0; x < dungeon_wid; x++)
		{
			/* Create granite wall */
			cave_info[y][x] &= ~(CAVE_LOS);
			cave_feat[y][x] = FEAT_WALL_EXTRA;
		}
	}

	/* Actual maximum number of rooms on this level */
	dun->row_rooms = dungeon_hgt / BLOCK_HGT;
	dun->col_rooms = dungeon_wid / BLOCK_WID;

	/* No stair locations yet */
	dun->stair_n = 0;

	/* Initialize the room table */
	for (by = 0; by < dun->row_rooms; by++)
	{
		for (bx = 0; bx < dun->col_rooms; bx++)
		{
			dun->room_map[by][bx] = 0;
		}
	}

	/* No rooms are connected yet */
	for (i = 0; i < CENT_MAX; i++)
	{
		dun->connected[i] = FALSE;
	}

	/* No rooms yet */
	dun->cent_n = 0;


	/* Calculate how many rooms of each type we can build */
	num_rooms_allowed();

	/* Destroyed levels do not have certain kinds of rooms. */
	if (destroyed)
	{
		/* No pits, chambers, or vaults */
		dun->room_num[5] = 0;
		dun->room_num[6] = 0;
		dun->room_num[8] = 0;
		dun->room_num[9] = 0;
	}


	/* Not a special quest level */
	if (!p_ptr->special_quest)
	{
		/*
		 * Build each type of room in turn until we cannot build any more.
		 */
		for (i = 0; i < ROOM_MAX; i++)
		{
			/* What type of room are we building now? */
			room_type = room_build_order[i];

			/* Find out how many rooms of this type we can build. */
			num_to_build = dun->room_num[room_type];

			/* Try to build all we are allowed. */
			for (j = 0; j < num_to_build; j++)
			{
				/* Stop building rooms when we hit the maximum. */
				if (rooms_built >= DUN_ROOMS) break;

				/* Build the room. */
				if (room_build(room_type))
				{
					/* Increase the room built count. */
					if (room_type == 10) rooms_built += 5;
					else if ((room_type == 6) || (room_type == 9))
						rooms_built += 3;
					else if (room_type == 8) rooms_built += 2;
					else rooms_built++;
				}

				/* Go to next type of room on failure. */
				else break;
			}
		}
	}

	/* Hack -- make the special quest level */
	else
	{
		quest_cave_gen();
	}


	/* Special boundary walls -- Top */
	for (x = 0; x < dungeon_wid; x++)
	{
		y = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Bottom */
	for (x = 0; x < dungeon_wid; x++)
	{
		y = dungeon_hgt - 1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Left */
	for (y = 0; y < dungeon_hgt; y++)
	{
		x = 0;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Special boundary walls -- Right */
	for (y = 0; y < dungeon_hgt; y++)
	{
		x = dungeon_wid - 1;

		/* Clear previous contents, add "solid" perma-wall */
		cave_set_feat(y, x, FEAT_PERM_SOLID);
	}

	/* Not a special quest level */
	if (!p_ptr->special_quest)
	{
		/* Hack -- Scramble the room order */
		for (i = 0; i < dun->cent_n; i++)
		{
			int pick1 = i;
			int pick2 = rand_int(dun->cent_n);
			y1 = dun->cent[pick1].y;
			x1 = dun->cent[pick1].x;
			dun->cent[pick1].y = dun->cent[pick2].y;
			dun->cent[pick1].x = dun->cent[pick2].x;
			dun->cent[pick2].y = y1;
			dun->cent[pick2].x = x1;

			/* XXX XXX - swap around room index numbers. */
			for (by = 0; by < 6; by++)
			{
				for (bx = 0; bx < 18; bx++)
				{
					if      (dun->room_map[by][bx] == pick2 + 1)
								dun->room_map[by][bx] =  pick1 + 1;
					else if (dun->room_map[by][bx] == pick1 + 1)
								dun->room_map[by][bx] =  pick2 + 1;
				}
			}
		}

		/* Start with no tunnel doors */
		dun->door_n = 0;

		/* Mark the first room as being connected. */
		dun->connected[0] = TRUE;

		/* Connect all the rooms together (and locate grids for tunnel doors) */
		for (i = 0; i < dun->cent_n; i++)
		{
			/* Connect the room to the next room. */
			if (i == dun->cent_n - 1) build_tunnel(dun->cent_n - 1, 0);
			else build_tunnel(i, i + 1);
		}

		/* Place tunnel doors */
		for (i = 0; i < dun->door_n; i++)
		{
			/* Extract junction location */
			y = dun->door[i].y;
			x = dun->door[i].x;

			/* Try placing doors */
			try_door(y, x - 1);
			try_door(y, x + 1);
			try_door(y - 1, x);
			try_door(y + 1, x);
		}
	}


	/* Add some magma seams */
	for (i = 0; i < DUN_STR_MAG; i++)
	{
		build_seam(FEAT_MAGMA, DUN_STR_MC);
	}

	/* Add some quartz seams */
	for (i = 0; i < DUN_STR_QUA; i++)
	{
		build_seam(FEAT_QUARTZ, DUN_STR_QC);
	}


	/* Handle destroyed levels */
	if (destroyed)
	{
		/* If we successfully place a player ghost, destroy the level */
		if (place_ghost()) destroy_level(TRUE);
	}


	/* Place some traps in the dungeon. */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_TRAP,
		randint(2 + div_round(p_ptr->depth, 15)));

	if (!p_ptr->special_quest)
	{
		/* Put some rubble in corridors. */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_RUBBLE, randint(6));

		/* Possibly have one loose rock in a corridor. */
		alloc_object(ALLOC_SET_CORR, ALLOC_TYP_LOOSE_ROCK, rand_int(2));
	}

	/* Get bonus to object numbers (needs to be pretty small) */
	bonus = m_bonus(2, p_ptr->depth, MAX_DEPTH);

	/* Special quest levels are lucrative */
	if (p_ptr->special_quest)
	{
		bonus += 6;
		object_level = p_ptr->depth + 3;
	}

	/* Put some objects in rooms */
	alloc_object(ALLOC_SET_ROOM, ALLOC_TYP_OBJECT,
		Rand_normal(DUN_AMT_ROOM + bonus, 3));

	/* Put some objects/gold in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_OBJECT,
		Rand_normal(DUN_AMT_ITEM + bonus, 2));
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_GOLD,
		Rand_normal(DUN_AMT_GOLD + bonus, 2));

	/* Put some boulders in the dungeon */
	alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_BOULDER, rand_range(2, 3));

	/* Special quest */
	if (p_ptr->special_quest) object_level = p_ptr->depth;

	/* Make it practicable to play ironman/no town games */
	if ((p_ptr->character_type == PCHAR_IRONMAN) || (birth_no_stores))
	{
		alloc_object(ALLOC_SET_BOTH, ALLOC_TYP_FOOD, rand_range(2, 3));
	}


	/* Place 3 or 4 down stairs near some walls */
	alloc_stairs(FEAT_MORE, rand_range(3, 4), 3);

	/* Place 1 or 2 up stairs near some walls */
	if (p_ptr->character_type != PCHAR_IRONMAN) alloc_stairs(FEAT_LESS, rand_range(2, 3), 3);


	/* Determine the character location */
	new_player_spot();


	/* Pick a base number of monsters (MIN_M_ALLOC_LEVEL = 30) */
	i = MIN_M_ALLOC_LEVEL * (p_ptr->depth + 200);

	/* Randomize somewhat (14-20 on level 0, 20-30 on level 100) */
	i /= rand_range(300, 450);


	/* Special quest levels have a lot more monsters */
	if (p_ptr->special_quest)
	{
		i *= 2;
		monster_level = p_ptr->depth + 3;
	}

	/* Remove all monster restrictions. */
	(void)mon_restrict('\0', (byte)p_ptr->depth, &dummy, TRUE);

	/* Set monster generation depth. */
	monster_level = p_ptr->depth;

	/* Note how many monsters we currently have */
	start_mon_num = m_max;

	/* Put some monsters in the dungeon */
	for (j = i; j > 0; j--)
	{
		/* Place a random monster, but not in grids marked "CAVE_TEMP". */
		(void)alloc_monster(2, TRUE);

		/* Rein in monster groups and escorts a little. */
		if (m_max - start_mon_num > i * 2) break;
	}

	/* Special quest */
	if (p_ptr->special_quest) monster_level = p_ptr->depth;

	/* Save level rating (quest monsters do not affect level rating) */
	old_level_rating = level_rating;

	/* Place any quest monsters */
	for (i = 0; i < z_info->q_max; i++)
	{
		quest_type *q_ptr = &q_info[i];

		/* Quest levels */
		if (q_ptr->active_level == p_ptr->depth)
		{
			monster_race *r_ptr = &r_info[q_ptr->r_idx];
			s16b num_questors;

			/* A certain number of questors */
			num_questors = q_ptr->max_num - q_ptr->cur_num;

			/* Ensure quest monsters */
			while (r_ptr->cur_num < num_questors)
			{
				/* Allow a lot of tries */
				int tries = 5000;

				/* Pick a location */
				while (tries--)
				{
					y = rand_int(dungeon_hgt);
					x = rand_int(dungeon_wid);

					/* We are not yet desperate */
					if (tries > 2000)
					{
						/* Try not to place quest monsters in vaults */
						if (cave_info[y][x] & (CAVE_ICKY)) continue;

						/* Try not to place quest monsters in marked rooms */
						if (cave_info[y][x] & (CAVE_TEMP)) continue;

						/* Do not place quest monsters near the character */
						if (distance(y, x, p_ptr->py, p_ptr->px) <= 7) continue;
					}

					/* If monster can exist here, place it */
					if (cave_exist_mon(r_ptr, y, x, FALSE, FALSE)) break;
				}

				/* Handle complete failure  XXX XXX */
				if (!tries) break;

				/* Place the monster (never sleeping, allow groups) */
				place_monster_aux(y, x, q_ptr->r_idx, FALSE, TRUE);
			}
		}
	}

	/* Restore level rating */
	level_rating = old_level_rating;

	/* Clear "temp" flags. */
	for (y = 0; y < dungeon_hgt; y++)
	{
		for (x = 0; x < dungeon_wid; x++)
		{
			cave_info[y][x] &= ~(CAVE_TEMP);
		}
	}
}

/*
 * Builds a store at a given pseudo-location
 *
 * As of 2.8.1 (?) the town is actually centered in the middle of a
 * complete level, and thus the top left corner of the town itself
 * is no longer at (0,0), but rather, at (qy,qx), so the constants
 * in the comments below should be mentally modified accordingly.
 *
 * As of 2.7.4 (?) the stores are placed in a more "user friendly"
 * configuration, such that the four "center" buildings always
 * have at least four grids between them, to allow easy running,
 * and the store doors tend to face the middle of town.
 *
 * The stores now lie inside boxes from 3-9 and 12-18 vertically,
 * and from 7-17, 21-31, 35-45, 49-59.  Note that there are thus
 * always at least 2 open grids between any disconnected walls.
 *
 * Note the use of "town_illuminate()" to handle all "illumination"
 * and "memorization" issues.
 */
static void build_store(int n, int yy, int xx)
{
	int y, x, y0, x0, y1, x1, y2, x2, tmp;

	int qy = 0;
	int qx = 0;


	/* Find the "center" of the store */
	y0 = qy + yy * 9 + 6;
	x0 = qx + xx * 14 + 12;

	/* Determine the store boundaries */
	y1 = y0 - (yy == 0 ? 1 : 0) - randint(2);
	y2 = y0 + (yy == 1 ? 1 : 0) + randint(2);
	x1 = x0 - rand_range(2, 5);
	x2 = x0 + rand_range(2, 5);

	/* Build an invulnerable rectangular building */
	for (y = y1; y <= y2; y++)
	{
		for (x = x1; x <= x2; x++)
		{
			/* Create the building */
			cave_set_feat(y, x, FEAT_PERM_EXTRA);
		}
	}

	/* Pick a door direction (S,N,E,W) */
	tmp = rand_int(4);

	/* Re-roll "annoying" doors */
	while (((tmp == 0) && (yy == 1)) ||
	       ((tmp == 1) && (yy == 0)) ||
	       ((tmp == 2) && (xx == 3)) ||
	       ((tmp == 3) && (xx == 0)))
	{
		/* Pick a new direction */
		tmp = rand_int(4);
	}

	/* Extract a "door location" */
	switch (tmp)
	{
		/* Bottom side */
		case 0:
		{
			y = y2;
			x = rand_range(x1, x2);
			break;
		}

		/* Top side */
		case 1:
		{
			y = y1;
			x = rand_range(x1, x2);
			break;
		}

		/* Right side */
		case 2:
		{
			y = rand_range(y1, y2);
			x = x2;
			break;
		}

		/* Left side */
		default:
		{
			y = rand_range(y1, y2);
			x = x1;
			break;
		}
	}

	/* Clear previous contents, add a store door */
	cave_set_feat(y, x, FEAT_SHOP_HEAD + n);


	/* The Inn goes right beside the General Store */
	if (n == STORE_GENERAL)
	{
		int dist1 = 0;
		int dist2 = 0;

		/* Scan along a horizontal wall */
		if ((tmp == 0) || (tmp == 1))
		{
			/* Hunt along both walls */
			if (cave_feat[y][x+1] == FEAT_PERM_EXTRA)
			{
				dist1++;
				if (cave_feat[y][x+2] == FEAT_PERM_EXTRA)
				{
					dist1++;
					if (cave_feat[y][x+3] == FEAT_PERM_EXTRA)
					{
						dist1++;
					}
				}
			}
			if (cave_feat[y][x-1] == FEAT_PERM_EXTRA)
			{
				dist2++;
				if (cave_feat[y][x-2] == FEAT_PERM_EXTRA)
				{
					dist2++;
					if (cave_feat[y][x-3] == FEAT_PERM_EXTRA)
					{
						dist2++;
					}
				}
			}

			/* Place Inn as far away as possible */
			x = ((dist1 > dist2) ? x + dist1 : x - dist2);

			/* Place the Inn */
			cave_set_feat(y, x, FEAT_SHOP_INN);
		}

		/* Scan along a vertical wall */
		else
		{
			/* Hunt along both walls */
			if (cave_feat[y+1][x] == FEAT_PERM_EXTRA)
			{
				dist1++;
				if (cave_feat[y+2][x] == FEAT_PERM_EXTRA)
				{
					dist1++;
					if (cave_feat[y+3][x] == FEAT_PERM_EXTRA)
					{
						dist1++;
					}
				}
			}
			if (cave_feat[y-1][x] == FEAT_PERM_EXTRA)
			{
				dist2++;
				if (cave_feat[y-2][x] == FEAT_PERM_EXTRA)
				{
					dist2++;
					if (cave_feat[y-3][x] == FEAT_PERM_EXTRA)
					{
						dist2++;
					}
				}
			}

			/* Place Inn as far away as possible */
			y = ((dist1 > dist2) ? y + dist1 : y - dist2);

			/* Place the Inn */
			cave_set_feat(y, x, FEAT_SHOP_INN);
		}
	}
}




/*
 * Generate the "consistent" town features, and place the player
 *
 * Hack -- play with the R.N.G. to always yield the same town
 * layout, including the size and shape of the buildings, the
 * locations of the doorways, and the location of the stairs.
 */
static void town_gen_hack(void)
{
	int y, x, k, n;

	int qy = 0;
	int qx = 0;

	int rooms[MAX_STORES];


	/* Hack -- Use the "simple" RNG */
	Rand_quick = TRUE;

	/* Hack -- Induce consistent town layout */
	Rand_value = seed_town;


	/* Prepare an array of remaining stores, and count them */
	for (n = 0; n < MAX_STORES - 1; n++) rooms[n] = n;

	/* Place two rows of stores */
	for (y = 0; y < 2; y++)
	{
		/* Place four stores per row */
		for (x = 0; x < 4; x++)
		{
			/* Pick a random unplaced store */
			k = ((n <= 1) ? 0 : rand_int(n));

			/* Build that store at the proper location */
			build_store(rooms[k], y, x);

			/* Shift the stores down, remove one store */
			rooms[k] = rooms[--n];
		}
	}


	/* Place the stairs */
	while (TRUE)
	{
		/* Pick a location at least "three" from the outer walls */
		y = qy + rand_range(3, dungeon_hgt - 4);
		x = qx + rand_range(3, dungeon_wid - 4);

		/* Require a floor grid with no objects or monsters */
		if (cave_naked_bold(y, x)) break;
	}

	/* Clear previous contents, add down stairs */
	cave_set_feat(y, x, FEAT_MORE);


	/* Place the player */
	(void)player_place(y, x);


	/* Hack -- use the "complex" RNG */
	Rand_quick = FALSE;
}


/*
 * Town logic flow for generation of new town
 *
 * We start with a fully wiped cave of normal floors.
 *
 * Note that town_gen_hack() plays games with the RNG.
 *
 * This function does NOT do anything about the owners of the stores,
 * nor the contents thereof.  It only handles the physical layout.
 *
 * We place the player on the stairs at the same time we make them.
 *
 * Hack -- since the player always leaves the dungeon by the stairs,
 * he is always placed on the stairs, even if he left the dungeon via
 * word of recall or teleport level.
 */
static void town_gen(void)
{
	int i, y, x;

	int residents;

	bool daytime;


	/* Day time */
	if ((turn % (10L * TOWN_DAWN)) < ((10L * TOWN_DAWN) / 2))
	{
		/* Day time */
		daytime = TRUE;

		/* Number of residents */
		residents = MIN_M_ALLOC_TD;
	}

	/* Night time */
	else
	{
		/* Night time */
		daytime = FALSE;

		/* Number of residents */
		residents = MIN_M_ALLOC_TN;
	}

	/* Start with solid walls */
	for (y = 0; y < dungeon_hgt; y++)
	{
		for (x = 0; x < dungeon_wid; x++)
		{
			/* Create "solid" perma-wall */
			cave_set_feat(y, x, FEAT_PERM_SOLID);
		}
	}

	/* Boundary walls */
	for (x = 0; x < dungeon_wid; x++)
	{
		cave_set_feat(0,               x, FEAT_PERM_EXTRA);
		cave_set_feat(dungeon_hgt - 1, x, FEAT_PERM_EXTRA);
	}

	/* Boundary walls */
	for (y = 0; y < dungeon_hgt; y++)
	{
		cave_set_feat(y, 0,               FEAT_PERM_EXTRA);
		cave_set_feat(y, dungeon_wid - 1, FEAT_PERM_EXTRA);
	}

	/* Then place some floors */
	for (y = 1; y < dungeon_hgt - 1; y++)
	{
		for (x = 1; x < dungeon_wid - 1; x++)
		{
			/* Create empty floor */
			cave_set_feat(y, x, FEAT_FLOOR);
		}
	}

	/* Build stuff */
	town_gen_hack();

	/* Apply illumination */
	town_illuminate(daytime);

	/* Make some residents */
	for (i = 0; i < residents; i++)
	{
		/* Make a resident.  Nobody sleeps in the town. */
		(void)alloc_monster(10, FALSE);
	}

	/* If nighttime, make a thief or two */
	if (!daytime)
	{
		int thieves = randint(2);

		for (i = 0; i < thieves; i++)
		{
			/* Find a spot */
			while (TRUE)
			{
				y = randint(dungeon_hgt - 1);
				x = randint(dungeon_wid - 1);

				/* Stay legal */
				if (!in_bounds(y, x)) continue;

				/* Require a floor grid with no objects or monsters */
				if (cave_naked_bold(y, x)) break;
			}

			/* Make a thief */
			(void)place_monster_aux(y, x, MON_TOWN_THIEF, FALSE, FALSE);
		}
	}
}



/*
 * Make an essence.
 */
static void make_essence(int y, int x, s32b chance, int sval)
{
	object_type *i_ptr;
	object_type object_type_body;

	int num = 1;

	/* Paranoia -- stay legal */
	if ((sval < 0) || (sval > ESSENCE_MAX_SVAL)) return;


	/* Convert high chance into multiple essences (keep things highly random) */
	if (chance > 333)
	{
		num = div_round(chance, 333);
		chance = 333;
	}

	/* Location must be capable of holding objects */
	if (cave_allow_object_bold(y, x))
	{
		/* Must pass the rarity check */
		if (chance > rand_int(1000))
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Hack -- Make an essence */
			object_prep(i_ptr, lookup_kind(TV_ESSENCE, sval));

			/* Real object */
			if (i_ptr->k_idx)
			{
				/* Adjust quantity */
				i_ptr->number = num;

				/* Give the object to the floor */
				(void)floor_carry(y, x, i_ptr);
			}
		}
	}
}


/*
 * Generate essences near a monster.
 */
void mon_essence(int y, int x, int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	int k;
	u32b flag;

	int depth = p_ptr->depth;

	/* Base probability of creating an essence (out of 1000) */
	s32b chance = ESSENCE_PROB - depth / 2;

	/* Essences are rare above 150' */
	if (depth < 3) depth = 3;

	/* Unique monsters create essences more often */
	if (r_ptr->flags1 & (RF1_UNIQUE)) chance *= 4;

	/* Higher-level monsters attract more magic */
	chance = chance * (r_ptr->level * r_ptr->level) / (depth * depth);

	/* Group monsters generate essences less often */
	if      (r_ptr->flags1 & (RF1_FRIENDS)) chance /= 4;
	else if (r_ptr->flags1 & (RF1_FRIEND))  chance /= 2;


	/* Monster is surrounded by a cloud */
	if (r_ptr->flags2 & (RF2_CLOUD_SURROUND))
	{
		int typ, dam, rad;

		/* Get cloud type */
		cloud_surround(r_idx, &typ, &dam, &rad);

		/* Generate essences for some kinds of cloud */
		if      (typ == GF_SPORE) make_essence(y, x, chance, ESSENCE_POIS);
		else if (typ == GF_POIS) make_essence(y, x, chance, ESSENCE_POIS);
		else if (typ == GF_ACID) make_essence(y, x, chance, ESSENCE_ACID);
		else if (typ == GF_ELEC) make_essence(y, x, chance, ESSENCE_ELEC);
		else if (typ == GF_COLD) make_essence(y, x, chance, ESSENCE_COLD);
		else if (typ == GF_FIRE) make_essence(y, x, chance, ESSENCE_FIRE);
		else if (typ == GF_LITE) make_essence(y, x, chance, ESSENCE_LITE);
		else if (typ == GF_LITE_WEAK) make_essence(y, x, chance/2, ESSENCE_LITE);
		else if (typ == GF_DARK) make_essence(y, x, chance, ESSENCE_DARK);
		else if (typ == GF_DARK_WEAK) make_essence(y, x, chance/2, ESSENCE_DARK);
		else if (typ == GF_SOUND) make_essence(y, x, chance, ESSENCE_FORCE);
		else if (typ == GF_CONFUSION) make_essence(y, x, chance, ESSENCE_CONFU);
	}


	/* Scan flags1 */


	/* Note flags2 */
	flag = r_ptr->flags2;

	/* Check flags2 */
	if (flag & (RF2_IS_LIT))      make_essence(y, x, chance/2, ESSENCE_LITE);


	/* Note flags3 */
	flag = r_ptr->flags3;

	/* Check flags3 */
	if (flag & (RF3_ANIMAL))      make_essence(y, x, chance/4, ESSENCE_LIFE);
	if (flag & (RF3_UNDEAD))      make_essence(y, x, chance/4, ESSENCE_DEATH);
	if (flag & (RF3_RES_PLAS))
	{
		make_essence(y, x, chance/6, ESSENCE_ELEC);
		make_essence(y, x, chance/6, ESSENCE_FIRE);
	}
	if (flag & (RF3_RES_WATER))
	{
		make_essence(y, x, chance/6, ESSENCE_ACID);
		make_essence(y, x, chance/6, ESSENCE_COLD);
	}
	if (flag & (RF3_RES_NEXUS))   make_essence(y, x, chance/5, ESSENCE_NEXUS);
	if (flag & (RF3_RES_NETHR))   make_essence(y, x, chance/5, ESSENCE_NETHR);
	if (flag & (RF3_RES_CHAOS))   make_essence(y, x, chance/5, ESSENCE_CHAOS);
	if (flag & (RF3_RES_DISEN))   make_essence(y, x, chance/5, ESSENCE_MAGIC);
	if (flag & (RF3_RES_TPORT))   make_essence(y, x, chance/4, ESSENCE_NEXUS);


	/* Note flags4 */
	flag = r_ptr->flags4;

	/* Check flags4 */
	if (flag & (RF4_PMISSL))      make_essence(y, x, chance/2, ESSENCE_POIS);
	if (flag & (RF4_BRTH_ACID))   make_essence(y, x, chance*2, ESSENCE_ACID);
	if (flag & (RF4_BRTH_ELEC))   make_essence(y, x, chance*2, ESSENCE_ELEC);
	if (flag & (RF4_BRTH_FIRE))   make_essence(y, x, chance*2, ESSENCE_FIRE);
	if (flag & (RF4_BRTH_COLD))   make_essence(y, x, chance*2, ESSENCE_COLD);
	if (flag & (RF4_BRTH_POIS))   make_essence(y, x, chance*2, ESSENCE_POIS);
	if (flag & (RF4_BRTH_PLAS))
	{
		make_essence(y, x, chance, ESSENCE_ELEC);
		make_essence(y, x, chance, ESSENCE_FIRE);
	}
	if (flag & (RF4_BRTH_LITE))   make_essence(y, x, chance*2, ESSENCE_LITE);
	if (flag & (RF4_BRTH_DARK))   make_essence(y, x, 3*chance/2, ESSENCE_DARK);
	if (flag & (RF4_BRTH_CONFU))  make_essence(y, x, chance*2, ESSENCE_CONFU);
	if (flag & (RF4_BRTH_SOUND))  make_essence(y, x, chance*2, ESSENCE_FORCE);
	if (flag & (RF4_BRTH_SHARD))  make_essence(y, x, chance*2, ESSENCE_FORCE);
	if (flag & (RF4_BRTH_INER))   make_essence(y, x, chance/2, ESSENCE_TIME);
	if (flag & (RF4_BRTH_GRAV))
	{
		make_essence(y, x, chance, ESSENCE_NEXUS);
		make_essence(y, x, chance/2, ESSENCE_FORCE);
	}
	if (flag & (RF4_BRTH_WIND))   make_essence(y, x, chance/2, ESSENCE_FORCE);
	if (flag & (RF4_BRTH_FORCE))  make_essence(y, x, 3*chance/2, ESSENCE_FORCE);
	if (flag & (RF4_BRTH_NEXUS))  make_essence(y, x, chance*2, ESSENCE_NEXUS);
	if (flag & (RF4_BRTH_NETHR))  make_essence(y, x, chance*2, ESSENCE_NETHR);
	if (flag & (RF4_BRTH_CHAOS))  make_essence(y, x, chance*2, ESSENCE_CHAOS);
	if (flag & (RF4_BRTH_DISEN))  make_essence(y, x, chance*2, ESSENCE_MAGIC);
	if (flag & (RF4_BRTH_TIME))   make_essence(y, x, chance*2, ESSENCE_TIME);
	if (flag & (RF4_BRTH_MANA))   make_essence(y, x, chance*2, ESSENCE_MAGIC);


	/* Note flags5 */
	flag = r_ptr->flags5;

	/* Check flags5 */

	if (flag & (RF5_BALL_ACID))   make_essence(y, x, chance/2, ESSENCE_ACID);
	if (flag & (RF5_BALL_ELEC))   make_essence(y, x, chance/3, ESSENCE_ELEC);
	if (flag & (RF5_BALL_FIRE))   make_essence(y, x, chance/2, ESSENCE_FIRE);
	if (flag & (RF5_BALL_COLD))   make_essence(y, x, chance/2, ESSENCE_COLD);
	if (flag & (RF5_BALL_POIS))   make_essence(y, x, chance/2, ESSENCE_POIS);
	if (flag & (RF5_BALL_LITE))   make_essence(y, x, chance/2, ESSENCE_LITE);
	if (flag & (RF5_BALL_DARK))   make_essence(y, x, chance/2, ESSENCE_DARK);
	if (flag & (RF5_BALL_CONFU))  make_essence(y, x, chance/2, ESSENCE_CONFU);
	if (flag & (RF5_BALL_SOUND))  make_essence(y, x, chance/4, ESSENCE_FORCE);
	if (flag & (RF5_BALL_SHARD))  make_essence(y, x, chance/4, ESSENCE_FORCE);
	if (flag & (RF5_BALL_WIND))   make_essence(y, x, chance/4, ESSENCE_FORCE);
	if (flag & (RF5_BALL_STORM))
	{
		make_essence(y, x, chance/6, ESSENCE_ACID);
		make_essence(y, x, chance/6, ESSENCE_ELEC);
		make_essence(y, x, chance/6, ESSENCE_COLD);
	}
	if (flag & (RF5_BALL_NETHR))  make_essence(y, x, chance/2, ESSENCE_NETHR);
	if (flag & (RF5_BALL_CHAOS))  make_essence(y, x, chance/2, ESSENCE_CHAOS);
	if (flag & (RF5_BALL_MANA))   make_essence(y, x, chance/2, ESSENCE_MAGIC);

	if (flag & (RF5_BOLT_ACID))   make_essence(y, x, chance/3, ESSENCE_ACID);
	if (flag & (RF5_BOLT_ELEC))   make_essence(y, x, chance/4, ESSENCE_ELEC);
	if (flag & (RF5_BOLT_FIRE))   make_essence(y, x, chance/3, ESSENCE_FIRE);
	if (flag & (RF5_BOLT_COLD))   make_essence(y, x, chance/3, ESSENCE_COLD);
	if (flag & (RF5_BOLT_POIS))   make_essence(y, x, chance/3, ESSENCE_POIS);
	if (flag & (RF5_BOLT_PLAS))
	{
		make_essence(y, x, chance/6, ESSENCE_ELEC);
		make_essence(y, x, chance/6, ESSENCE_FIRE);
	}
	if (flag & (RF5_BOLT_ICE))    make_essence(y, x, chance/3, ESSENCE_COLD);
	if (flag & (RF5_BOLT_WATER))
	{
		make_essence(y, x, chance/4, ESSENCE_ACID);
		make_essence(y, x, chance/8, ESSENCE_COLD);
	}
	if (flag & (RF5_BOLT_NETHR))  make_essence(y, x, chance/4, ESSENCE_NETHR);
	if (flag & (RF5_BOLT_MANA))   make_essence(y, x, chance/3, ESSENCE_MAGIC);
	if (flag & (RF5_BEAM_ELEC))   make_essence(y, x, chance/3, ESSENCE_ELEC);
	if (flag & (RF5_BEAM_ICE))    make_essence(y, x, chance/2, ESSENCE_COLD);
	if (flag & (RF5_BEAM_NETHR))  make_essence(y, x, chance/3, ESSENCE_NETHR);
	if (flag & (RF5_ARC__HFIRE))
	{
		make_essence(y, x, chance/8, ESSENCE_FIRE);
		make_essence(y, x, chance/4, ESSENCE_DEATH);
	}
	if (flag & (RF5_ARC__FORCE))  make_essence(y, x, chance/3, ESSENCE_FORCE);


	/* Note flags6 */
	flag = r_ptr->flags6;

	/* Check flags6 */
	if (flag & (RF6_HASTE))       make_essence(y, x, chance/3, ESSENCE_TIME);
	if (flag & (RF6_ADD_MANA))    make_essence(y, x, chance/3, ESSENCE_LIFE);
	if (flag & (RF6_HEAL))        make_essence(y, x, chance/3, ESSENCE_LIFE);
	if (flag & (RF6_CURE))        make_essence(y, x, chance/3, ESSENCE_LIFE);
	if (flag & (RF6_BLINK))       make_essence(y, x, chance/3, ESSENCE_NEXUS);
	if (flag & (RF6_TPORT))       make_essence(y, x, chance/3, ESSENCE_NEXUS);
	if (flag & (RF6_TELE_SELF_TO))make_essence(y, x, chance/3, ESSENCE_NEXUS);
	if (flag & (RF6_TELE_TO))     make_essence(y, x, chance/3, ESSENCE_NEXUS);
	if (flag & (RF6_TELE_AWAY))   make_essence(y, x, chance/3, ESSENCE_NEXUS);
	if (flag & (RF6_TELE_LEVEL))  make_essence(y, x, chance/3, ESSENCE_NEXUS);
	if (flag & (RF6_DARKNESS))    make_essence(y, x, chance/6, ESSENCE_DARK);
	if (flag & (RF6_FORGET))      make_essence(y, x, chance/4, ESSENCE_DEATH);
	if (flag & (RF6_DRAIN_MANA))  make_essence(y, x, chance/4, ESSENCE_MAGIC);
	if (flag & (RF6_CURSE))       make_essence(y, x, chance/4, ESSENCE_DEATH);
	if (flag & (RF6_WOUND))       make_essence(y, x, chance/4, ESSENCE_DEATH);
	if (flag & (RF6_HELLDARK))
	{
		make_essence(y, x, chance/3, ESSENCE_DARK);
		make_essence(y, x, chance/3, ESSENCE_DEATH);
	}
	if (flag & (RF6_HOLY_SMITE))
	{
		make_essence(y, x, chance/3, ESSENCE_LITE);
		make_essence(y, x, chance/3, ESSENCE_LIFE);
	}
	if (flag & (RF6_BLIND))       make_essence(y, x, chance/6, ESSENCE_DARK);
	if (flag & (RF6_CONF))        make_essence(y, x, chance/6, ESSENCE_CONFU);
	if (flag & (RF6_SLOW))        make_essence(y, x, chance/6, ESSENCE_TIME);
	if (flag & (RF6_HOLD))        make_essence(y, x, chance/4, ESSENCE_TIME);

	/* Check flags7 */

	/* Check blows */
	for (k = 0; k < MONSTER_BLOW_MAX; k++)
	{
		int effect = r_ptr->blow[k].effect;

		if (effect == RBE_SHATTER) make_essence(y, x, chance/4, ESSENCE_FORCE);

		if (effect ==RBE_UN_BONUS) make_essence(y, x, chance/6, ESSENCE_MAGIC);
		if (effect ==RBE_UN_POWER) make_essence(y, x, chance/6, ESSENCE_MAGIC);
		if (effect ==RBE_LOSE_MANA)make_essence(y, x, chance/6, ESSENCE_MAGIC);
		if (effect ==RBE_EAT_LITE) make_essence(y, x, chance/4, ESSENCE_DARK);

		if (effect == RBE_POISON)  make_essence(y, x, chance/4, ESSENCE_POIS);
		if (effect == RBE_ACID)    make_essence(y, x, chance/3, ESSENCE_ACID);
		if (effect == RBE_ELEC)    make_essence(y, x, chance/4, ESSENCE_ELEC);
		if (effect == RBE_FIRE)    make_essence(y, x, chance/3, ESSENCE_FIRE);
		if (effect == RBE_COLD)    make_essence(y, x, chance/3, ESSENCE_COLD);

		if (effect == RBE_BLIND)   make_essence(y, x, chance/12, ESSENCE_DARK);
		if (effect == RBE_CONFUSE) make_essence(y, x, chance/4, ESSENCE_CONFU);
		if (effect == RBE_HALLU)   make_essence(y, x, chance/4, ESSENCE_CHAOS);
		if (effect == RBE_DISEASE) make_essence(y, x, chance/4, ESSENCE_POIS);

		if (effect == RBE_EXP_10)  make_essence(y, x, chance/13, ESSENCE_DEATH);
		if (effect == RBE_EXP_20)  make_essence(y, x, chance/9, ESSENCE_DEATH);
		if (effect == RBE_EXP_40)  make_essence(y, x, chance/6, ESSENCE_DEATH);
		if (effect == RBE_EXP_80)  make_essence(y, x, chance/3, ESSENCE_DEATH);
	}
}


/*
 * Generate essences near an object
 */
void obj_essence(int y, int x, const object_type *o_ptr)
{
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	int i;
	u32b flags = 0L;

	int depth = p_ptr->depth;

	/* Base probability of creating an essence (out of 1000) */
	s32b base_chance = ESSENCE_PROB - depth / 2;
	s32b chance;

	bool use_cost = FALSE;


	/* Artifacts attract many more essences */
	if (artifact_p(o_ptr)) base_chance *= 5;

	/* Ego-items are also good */
	else if (ego_item_p(o_ptr)) base_chance *= 2;

	/* Essences are rare above 150' */
	if (depth < 3)
	{
		depth = 3;
		base_chance = base_chance * depth / 3;
	}


	/* Non-cursed objects with an essence cost attract those essences */
	if (!cursed_p(o_ptr))
	{
		/* Higher-level objects attract more magic */
		chance = base_chance * (k_ptr->level * k_ptr->level) /
			(depth * depth);

		/* Scan the possible essences */
		for (i = 0; i < 4; i++)
		{
			/* Note sval and number of essences */
			int num =  k_ptr->e_num[i];
			int type = k_ptr->e_type[i];

			/* Object has some essence cost */
			if (num)
			{
				/* Note that this object uses essence cost, not flags */
				use_cost = TRUE;

				/* Make this essence type */
				make_essence(y, x, chance * rsqrt(num), type);
			}
		}
	}

	/* Object uses flags, not essence costs */
	if (!use_cost)
	{
		/* Scan any pvals or special flags */
		for (i = 0; i < 128; i++)
		{
			/* Skip past empty flag sets */
			if ((i == 0) && (!get_object_pval(o_ptr, 0L))) i = 32;
			if ((i == 32) && (!o_ptr->flags1)) i = 64;
			if ((i == 64) && (!o_ptr->flags2)) i = 96;
			if ((i == 96) && (!o_ptr->flags3)) break;


			/* Scan the pval-dependant object flags */
			if (i < 32)
			{
				/* Object has a positive pval for this attribute */
				if (get_object_pval(o_ptr, (1L << (i % 32))) > 0)
				{
					/* Get essence generation probability */
					chance = base_chance * flag_creation_data[i].prob / 100L;

					/* Make the corresponding essence */
					make_essence(y, x, chance,
						flag_creation_data[i].essence_sval);
				}
			}

			/* Scan other flag sets */
			else
			{
				/* Choose the right flag set */
				if      (i == 32) flags = o_ptr->flags1;
				else if (i == 64) flags = o_ptr->flags2;
				else if (i == 96) flags = o_ptr->flags3;

				/* Scan flags */
				if (flags & (1L << (i % 32)))
				{
					/* Get essence generation probability */
					chance = base_chance * flag_creation_data[i].prob / 12L;

					/* Make the corresponding essence */
					make_essence(y, x, chance,
						flag_creation_data[i].essence_sval);
				}
			}
		}

		/* Check magic */
		chance = base_chance * (o_ptr->to_h + o_ptr->to_d) / 15L;

		/* Note magic */
		make_essence(y, x, chance, ESSENCE_MAGIC);

		/* Check (additional) protection */
		chance = base_chance * (o_ptr->to_a + o_ptr->ac - k_ptr->ac) / 15L;

		/* Note protection */
		make_essence(y, x, chance, ESSENCE_SHIELD);
	}
}

/*
 * Generate essences.
 */
static void gen_essences(void)
{
	int y, x;

	monster_type *m_ptr;
	object_type *o_ptr;

	/* Not in the town */
	if (!p_ptr->depth) return;

	/* Scan the dungeon (less the outer walls) */
	for (y = 1; y < dungeon_hgt; y++)
	{
		for (x = 1; x < dungeon_wid; x++)
		{
			/* A monster exists in this square */
			if (cave_m_idx[y][x] > 0)
			{
				m_ptr = &m_list[cave_m_idx[y][x]];

				/* Create essences for this monster */
				mon_essence(y, x, m_ptr->r_idx);
			}

			/* Scan any objects present */
			for (o_ptr = get_first_object(y, x); o_ptr;
			     o_ptr = get_next_object(o_ptr))
			{
				/* Paranoia -- skip essences */
				if (o_ptr->tval == TV_ESSENCE) continue;

				/* Create essences for this object */
				obj_essence(y, x, o_ptr);
			}
		}
	}
}




/*
 * Generate spurious dungeon generation precognition messages.
 *
 * When precognition messages first start to appear, many are spurious.
 * Further increases yield increasingly accurate messages until the
 * spurious ones entirely disappear.
 */
static void spurious_gen_precog_msg(void)
{
	int precog = get_skill(S_PERCEPTION, 0, 100);
	int num;
	int base = LEV_REQ_PRECOG;
	int d = div_round(p_ptr->depth, 10);

	/* A very high perception yields perfectly accurate messages */
	if (precog >= 95) return;

	/* A low perception yields no messages at all */
	if (precog < base) return;

	/* Fake monster pit */
	num = room[5].room_gen_num[MIN(10, d)];
	if ((num > rand_int(100)) && (one_in_(3 + precog - base)))
		precog_msg(PRECOG_GEN_HORDE);

	/* Fake room of chambers */
	num = room[6].room_gen_num[MIN(10, d)];
	if ((num > rand_int(100)) && (one_in_(3 + precog - base)))
		precog_msg(PRECOG_GEN_COMMUNITY);

	/* Fake lesser vault */
	num = room[8].room_gen_num[MIN(10, d)];
	if ((num > rand_int(100)) && (one_in_(3 + precog - base)))
		precog_msg(PRECOG_GEN_DANGEROUS);

	/* Fake greater vault */
	num = room[9].room_gen_num[MIN(10, d)];
	if ((num > rand_int(100)) && (one_in_(3 + precog - base)))
		precog_msg(PRECOG_GEN_VERY_DANGEROUS);
}


/*
 * Generate a random dungeon level
 *
 * Hack -- regenerate any "overflow" levels
 *
 * Note that this function resets flow data and grid flags directly.
 * Note that this function does not reset features, monsters, or objects.
 * Features are left to the town and dungeon generation functions, and
 * "wipe_m_list()" and "wipe_o_list()" handle monsters and objects.
 *
 * Note:  The teleportation code (and a lot of other code) assumes that
 * the top-left corner of towns and dungeons is grid (0, 0).
 */
void generate_cave(void)
{
	int y, x;
	cptr quest_feel;


	/* The dungeon is not ready */
	character_dungeon = FALSE;

	/* Don't know feeling yet */
	no_feeling_yet = TRUE;

	/* Cancel the target */
	target_set_monster(0);

	/* Cancel the health bar */
	health_track(0);


	/*** Dungeon conditions:  Defaults ***/

	/* Allow special lighting */
	p_ptr->dungeon_flags &= ~(DUNGEON_NO_SPECIAL_LIGHTING);


	/* Hack -- jump to a special quest level */
	if (p_ptr->special_quest)
	{
		p_ptr->depth = p_ptr->max_depth;

		/* Avoid levels with quest monsters  XXX */
		while (quest_num(p_ptr->depth) != 0) p_ptr->depth++;
	}

	/* Generate */
	while (TRUE)
	{
		bool okay = TRUE;

		/* Wipe the objects */
		wipe_o_list();

		/* Wipe the monsters */
		wipe_m_list();

		/* Wipe the traps */
		wipe_t_list();

		/* Wipe the precognition messages */
		precog_msg(PRECOG_WIPE);

		/* Clear flags and flow information. */
		for (y = 0; y < DUNGEON_HGT_MAX; y++)
		{
			for (x = 0; x < DUNGEON_WID_MAX; x++)
			{
				/* No flags */
				cave_info[y][x] = 0;

				/* No flow */
				cave_cost[y][x] = 0;
				cave_when[y][x] = 0;
			}
		}

		/* Mega-Hack -- no player in dungeon yet */
		cave_m_idx[p_ptr->py][p_ptr->px] = 0;
		p_ptr->px = p_ptr->py = 0;

		/* Hack -- illegal panel  XXX XXX XXX */
		p_ptr->wy = DUNGEON_HGT_MAX;
		p_ptr->wx = DUNGEON_WID_MAX;

		/* Reset the monster generation level */
		monster_level = p_ptr->depth;

		/* Reset the object generation level */
		object_level = p_ptr->depth;

		/* Nothing special here yet */
		good_item_flag = FALSE;

		/* Nothing good here yet */
		level_rating = 0;


		/* Build the town */
		if (!p_ptr->depth)
		{
			/* Town is always the same size */
			dungeon_hgt = 22;
			dungeon_wid = 66;

			/* Make a town */
			town_gen();

			/* Hack -- Clear stairs request */
			p_ptr->create_stair = 0;
		}

		/* Build a real level */
		else
		{
			/* Assume maximum size */
			dungeon_hgt = DUNGEON_HGT_MAX;
			dungeon_wid = DUNGEON_WID_MAX;

			/* Normal levels can be smaller */
			if (!p_ptr->special_quest)
			{
				/*
				 * Note:  If levels are too small, the game will freeze
				 * in "alloc_monster()".
				 */

				int blocks_y = dungeon_hgt / BLOCK_HGT;
				int blocks_x = dungeon_wid / BLOCK_WID;

				/* Allow dungeons to be less tall */
				if (one_in_(5)) blocks_y -= div_round(blocks_y, 6);
				if (one_in_(5)) blocks_y -= div_round(blocks_y, 6);

				/* Allow dungeons to be less wide */
				if (one_in_(5)) blocks_x -= div_round(blocks_x, 6);
				if (one_in_(5)) blocks_x -= div_round(blocks_x, 6);

				/* Resize dungeon, but always stay sane */
				dungeon_hgt = MAX(BLOCK_HGT * 3, blocks_y * BLOCK_HGT);
				dungeon_wid = MAX(BLOCK_WID * 3, blocks_x * BLOCK_WID);
			}

			/* Make a dungeon */
			cave_gen();
		}

		/* Determine the level feeling */
		if      (good_item_flag)                       feeling =  1;
		else if (level_rating > 85 + p_ptr->depth / 3) feeling =  2;
		else if (level_rating > 55 + p_ptr->depth / 4) feeling =  3;
		else if (level_rating > 35 + p_ptr->depth / 4) feeling =  4;
		else if (level_rating > 20 + p_ptr->depth / 5) feeling =  5;
		else if (level_rating > 15 + p_ptr->depth / 7) feeling =  6;
		else if (level_rating > 10 + p_ptr->depth /10) feeling =  7;
		else if (level_rating >  5 + p_ptr->depth /20) feeling =  8;
		else if (level_rating >  0)                    feeling =  9;
		else                                           feeling = 10;

		/* Hack -- no feeling in the town */
		if (!p_ptr->depth) feeling = 0;


		/* Prevent object over-flow */
		if (o_max >= z_info->o_max)
		{
			if (cheat_room)
				msg_print("Generation restarted (too many objects)");
			okay = FALSE;
		}

		/* Prevent monster over-flow */
		else if (m_max >= z_info->m_max)
		{
			if (cheat_room)
				msg_print("Generation restarted (too many monsters)");
			okay = FALSE;
		}

		/* Accept */
		if (okay) break;
	}

	/* Generate essences */
	gen_essences();

	/* The dungeon is ready */
	character_dungeon = TRUE;


	/* Generate spurious precognition messages */
	if (p_ptr->depth) spurious_gen_precog_msg();

	/* Display the precognition messages */
	precog_msg(PRECOG_DISPLAY);


	/* Cancel special quest changes */
	if (p_ptr->special_quest)
	{
		p_ptr->depth = p_ptr->max_depth;
		p_ptr->special_quest = FALSE;
	}

	/* Track maximum dungeon level */
	if (p_ptr->max_depth < p_ptr->depth)
	{
		p_ptr->max_depth = p_ptr->depth;
	}

	/* Display the quest description for the current level */
	quest_feel = describe_quest(p_ptr->depth, QMODE_SHORT);
	if (quest_feel != NULL) msg_print(quest_feel);

	/* Verify the panel */
	verify_panel(0, FALSE);


	/* Remember when this level was "created" */
	old_turn = turn;
}
