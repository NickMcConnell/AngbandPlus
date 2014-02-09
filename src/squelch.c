/*
 * File: squelch.c
 * Purpose: Item destruction
 *
 * Copyright (c) 2007 David T. Blackston, Iain McFall, DarkGod, Jeff Greene,
 * Diego Gonzalez, David Vestal, Pete Mack, Andrew Sidwell.
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */
#include "angband.h"
#include "cmds.h"
#include "ui-menu.h"
#include "game-event.h"

byte squelch_level[SQUELCH_BYTES];
const size_t squelch_size = SQUELCH_BYTES;

/*
 * These are the base types for quelching on identification.
 * Some of the tvls are combined by ini_tv_to_type to make this
 * list a little more reasonable.
 */

#define TYPE_AMMO    1
#define TYPE_BOW     2
#define TYPE_WEAPON1 3
#define TYPE_WEAPON2 4
#define TYPE_BODY    5
#define TYPE_CLOAK   6
#define TYPE_SHIELD  7
#define TYPE_HELM    8
#define TYPE_GLOVES  9
#define TYPE_BOOTS   10
#define TYPE_RING    11
#define TYPE_STAFF   12
#define TYPE_WAND    13
#define TYPE_ROD     14
#define TYPE_SCROLL  15
#define TYPE_POTION  16
#define TYPE_AMULET  17
#define TYPE_BOOK    18
#define TYPE_FOOD    19
#define TYPE_MISC    20
#define TYPE_MAX	 TYPE_MISC

#define MAXTV_TO_TYPE  100
/*
 * This (admittedly hacky) stores the mapping from tval to typeval
 * and is reinitialized every time do_cmd_squelch is called.  This
 * can certainly be done more cleanly.
 */
static int tv_to_type[MAXTV_TO_TYPE];
static bool seen_type[TYPE_MAX];

/*
 * List of kinds of item, for pseudo-id squelch.
 */
typedef enum
{
	PS_TYPE_WEAPON_SHARP,
	PS_TYPE_WEAPON_BLUNT,
	PS_TYPE_EQUIP_RARE,
	PS_TYPE_BOW,
	PS_TYPE_MISSILE_SLING,
	PS_TYPE_MISSILE_BOW,
	PS_TYPE_MISSILE_XBOW,
	PS_TYPE_ARMOR_ROBE,
	PS_TYPE_ARMOR_BODY,
	PS_TYPE_ARMOR_DRAGON,
	PS_TYPE_CLOAK,
	PS_TYPE_SHIELD,
	PS_TYPE_HELMS,
	PS_TYPE_CROWNS,
	PS_TYPE_GLOVES,
	PS_TYPE_BOOTS,
	PS_TYPE_DIGGER,
	PS_TYPE_RING,
	PS_TYPE_AMULET,
	PS_TYPE_LIGHT,

	PS_TYPE_MAX
} squelch_type_t;

typedef struct
{
	squelch_type_t squelch_type;
	int tval;
	int min_sval;
	int max_sval;
} quality_squelch_struct;

/*
 * SVAL Ranges for the quality squelch.
 * Note "Rare" items now have their own category.
 * As long as they come first in the list, before the
 * "0 to SV_UNKNOWN" category, they will be grouped
 * with the rare items.
 */
static quality_squelch_struct quality_mapping[] =
{
	{ PS_TYPE_EQUIP_RARE,		TV_SWORD,	SV_BLADE_OF_CHAOS,	SV_BLADE_OF_CHAOS },
	{ PS_TYPE_WEAPON_SHARP,	TV_SWORD,	0,		SV_UNKNOWN },
	{ PS_TYPE_EQUIP_RARE,		TV_POLEARM,	SV_SCYTHE_OF_SLICING,	SV_SCYTHE_OF_SLICING },
	{ PS_TYPE_WEAPON_SHARP,	TV_POLEARM,	0,		SV_UNKNOWN },
	{ PS_TYPE_EQUIP_RARE,		TV_HAFTED,	SV_MACE_OF_DISRUPTION,	SV_GROND },
	{ PS_TYPE_WEAPON_BLUNT,	TV_HAFTED,	0,		SV_UNKNOWN },
	{ PS_TYPE_BOW,				TV_BOW,		0,		SV_UNKNOWN },
	{ PS_TYPE_MISSILE_SLING,	TV_SHOT,	0,		SV_UNKNOWN },
	{ PS_TYPE_MISSILE_BOW,		TV_ARROW,	0,		SV_UNKNOWN },
	{ PS_TYPE_MISSILE_XBOW,	TV_BOLT,	0,		SV_UNKNOWN },
	{ PS_TYPE_ARMOR_ROBE,		TV_SOFT_ARMOR,	SV_ROBE,	SV_ROBE },
	{ PS_TYPE_ARMOR_BODY,		TV_SOFT_ARMOR,	0,		SV_UNKNOWN },
	{ PS_TYPE_EQUIP_RARE,		TV_HARD_ARMOR,	SV_MITHRIL_CHAIN_MAIL,	SV_ADAMANTITE_PLATE_MAIL },
	{ PS_TYPE_ARMOR_BODY,		TV_HARD_ARMOR,	0,		SV_UNKNOWN },
	{ PS_TYPE_ARMOR_DRAGON,	TV_DRAG_ARMOR,	0, 	SV_UNKNOWN},
	{ PS_TYPE_ARMOR_DRAGON,	TV_DRAG_SHIELD,	0, 	SV_UNKNOWN},
	{ PS_TYPE_EQUIP_RARE,		TV_CLOAK,	SV_SHADOW_CLOAK, 	SV_SHADOW_CLOAK },
	{ PS_TYPE_CLOAK,			TV_CLOAK,	0, 		SV_UNKNOWN },
	{ PS_TYPE_EQUIP_RARE,		TV_SHIELD,	SV_SHIELD_OF_DEFLECTION, 	SV_SHIELD_OF_DEFLECTION },
	{ PS_TYPE_SHIELD,			TV_SHIELD,	0,		SV_UNKNOWN },
	{ PS_TYPE_HELMS,			TV_HELM,	0,		SV_UNKNOWN },
	{ PS_TYPE_CROWNS,			TV_CROWN,	0,		SV_UNKNOWN },
	{ PS_TYPE_GLOVES,			TV_GLOVES,	0,		SV_UNKNOWN },
	{ PS_TYPE_BOOTS,			TV_BOOTS,	0,		SV_UNKNOWN },
	{ PS_TYPE_DIGGER,			TV_DIGGING,	0,		SV_UNKNOWN },
	{ PS_TYPE_RING,			TV_RING,	0,		SV_UNKNOWN },
	{ PS_TYPE_AMULET,			TV_AMULET,	0,		SV_UNKNOWN },
	{ PS_TYPE_LIGHT, 			TV_LIGHT, 	0,		SV_UNKNOWN },
};

typedef struct
{
	int enum_val;
	const char *name;
} quality_name_struct;

static quality_name_struct quality_choices[PS_TYPE_MAX] =
{
	{ PS_TYPE_WEAPON_SHARP,	"Sharp Melee Weapons" },
	{ PS_TYPE_WEAPON_BLUNT,	"Blunt Melee Weapons" },
	{ PS_TYPE_EQUIP_RARE,		"Rare Equipment" },
	{ PS_TYPE_BOW,				"Missile launchers" },
	{ PS_TYPE_MISSILE_SLING,	"Shots and Pebbles" },
	{ PS_TYPE_MISSILE_BOW,		"Arrows" },
	{ PS_TYPE_MISSILE_XBOW,	"Bolts" },
	{ PS_TYPE_ARMOR_ROBE,		"Robes" },
	{ PS_TYPE_ARMOR_BODY,		"Body Armor" },
	{ PS_TYPE_ARMOR_DRAGON,	"Dragon Armor/Shields" },
	{ PS_TYPE_CLOAK,			"Cloaks" },
	{ PS_TYPE_SHIELD,			"Shields" },
	{ PS_TYPE_HELMS,			"Helms" },
	{ PS_TYPE_CROWNS,			"Crowns" },
	{ PS_TYPE_GLOVES,			"Gloves" },
	{ PS_TYPE_BOOTS,			"Boots" },
	{ PS_TYPE_DIGGER,			"Diggers" },
	{ PS_TYPE_RING,			"Rings" },
	{ PS_TYPE_AMULET,			"Amulets" },
	{ PS_TYPE_LIGHT, 			"Lights" },
};


/* Categories for sval-dependent squelch. */
static tval_desc tvals[] =
{
	{TYPE_AMMO, 	"Missiles"},
	{TYPE_BOW, 		"Missile Launchers"},
	{TYPE_WEAPON1, 	"Weapons (Swords)"},
	{TYPE_WEAPON2, 	"Weapons (Non Swords)"},
	{TYPE_BODY, 	"Body Armor"},
	{TYPE_CLOAK, 	"Cloaks"},
	{TYPE_SHIELD, 	"Shields"},
	{TYPE_HELM, 	"Helmets"},
	{TYPE_GLOVES, 	"Gloves"},
	{TYPE_BOOTS, 	"Boots"},
	{TYPE_AMULET, 	"Amulets"},
	{TYPE_RING, 	"Rings"},
	{TYPE_STAFF, 	"Staves"},
	{TYPE_WAND, 	"Wands"},
	{TYPE_ROD, 		"Rods"},
	{TYPE_SCROLL, 	"Scrolls"},
	{TYPE_POTION, 	"Potions"},
	{TYPE_BOOK, 	"Magic Books"},
	{TYPE_FOOD, 	"Food Items"},
	{TYPE_MISC, 	"Miscellaneous"},
};

/*
 * The names for the various kinds of quality
 */
static quality_name_struct quality_values[SQUELCH_MAX] =
{
	{ SQUELCH_NONE,		"none" },
	{ SQUELCH_CURSED,	"squelch cursed" },
	{ SQUELCH_AVERAGE,	"squelch cursed and average" },
	{ SQUELCH_GOOD_STRONG,	"squelch good, average, and cursed" },
	{ SQUELCH_GOOD_WEAK,	"squelch good pseudo-id, average, and cursed" },
	{ SQUELCH_ALL,	"squelch all but artifacts" },
};

/*
 * menu struct for differentiating aware from unaware squelch
 */
typedef struct
{
	s16b idx;
	bool aware;
} squelch_choice;




/*** Autoinscription stuff ***/

int get_autoinscription_index(s16b k_idx)
{
	int i;

	for(i = 0; i < inscriptionsCount; i++)
	{
		if(k_idx == inscriptions[i].kindIdx)
		{
			return i;
		}
	}

	return -1;
}

/*
 * Returns the current autoinscription.
 */
cptr get_autoinscription(s16b kindIdx)
{
	int i;

	for(i = 0; i < inscriptionsCount; i++)
	{
		if(kindIdx == inscriptions[i].kindIdx)
		{
			return quark_str(inscriptions[i].inscriptionIdx);
		}
	}

	return 0;
}

/*Put the autoinscription on an object*/
int apply_autoinscription(object_type *o_ptr)
{
	char o_name[80];
	cptr note = get_autoinscription(o_ptr->k_idx);
	char buf[200];

	/* Don't inscribe unaware objects */
	if (!object_aware_p(o_ptr))
	{
		return 0;
	}

	/* Bugfix - Don't replace existing inscriptions */
	if (o_ptr->obj_note)
	{
		return 0;
	}

	/* We have an autoinscription */
	if (note)
	{
		/* Process special text patterns */
		expand_inscription(o_ptr, note, buf, sizeof(buf));

		/* Assign the note */
		note = buf;
	}
	/* Check the addition of smart autoinscriptions for ego-items */
	else if (ego_item_p(o_ptr) && object_known_p(o_ptr) &&
		(k_info[o_ptr->k_idx].squelch != SQUELCH_ALWAYS) &&
		(squelch_itemp(o_ptr, 0, TRUE) != SQUELCH_YES))
	{
		/* Describe the random powers */
		format_object_flags(o_ptr, buf, sizeof(buf), TRUE);

		/* Assign the note only if we found some powers */
		if (buf[0]) note = buf;
	}

	/* No autoinscription. Done */
	if (!note) return 0;

	/* Get the object name before adding the inscription */
	object_desc(o_name, sizeof(o_name), o_ptr, ODESC_PREFIX | ODESC_FULL);

	/* Assign the new inscription */
	o_ptr->obj_note = ((note[0] == 0) ? 0 : quark_add(note));

	/* Show a message */
	msg_format("You autoinscribe %s.", o_name);

	return 1;
}


int remove_autoinscription(s16b kind)
{
	int i = get_autoinscription_index(kind);

	/* It's not here, */
	if (i == -1) return 0;

	while(i < inscriptionsCount - 1)
	{
		inscriptions[i] = inscriptions[i+1];
		i++;
	}

	inscriptionsCount--;

	return 1;
}



int add_autoinscription(s16b kind, cptr inscription)
{
	int index;

	if(kind == 0)
	{
		/* paranoia */
		return 0;
	}

	if(!inscription || inscription[0] == 0)
	{
		return remove_autoinscription(kind);
	}

	index = get_autoinscription_index(kind);

	if(index == -1)
	{
		index = inscriptionsCount;
	}

	if(index >= AUTOINSCRIPTIONS_MAX)
	{
		msg_format("This inscription (%s) cannot be added, "
			"because the inscription array is full!", inscription);
		return 0;
	}

	inscriptions[index].kindIdx = kind;
	inscriptions[index].inscriptionIdx = quark_add(inscription);

	if(index == inscriptionsCount)
	{
		/* Only increment count if inscription added to end of array */
		inscriptionsCount++;
	}

	return 1;
}


void autoinscribe_ground(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	s16b this_o_idx, next_o_idx = 0;

	/* Scan the pile of objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the next object */
		next_o_idx = o_list[this_o_idx].next_o_idx;

		/* Apply an autoinscription */
		apply_autoinscription(&o_list[this_o_idx]);
	}
}

void autoinscribe_pack(void)
{
	int i;

	for (i = INVEN_PACK; i > 0; i--)
	{
		/* Skip empty items */
		if(!inventory[i].k_idx) continue;

		apply_autoinscription(&inventory[i]);
	}
}

/* Convert the values returned by squelch_itemp to string */
const char *squelch_to_label(int squelch)
{
  	if (squelch == SQUELCH_YES) return ("(Squelched)");

	if (squelch == SQUELCH_FAILED) return ("(Squelch Failed)");

	return ("");
}




/*** Squelch code ***/

/*
 * Determines whether a tval is eligible for tval-squelch.
 */
bool squelch_tval(int tval)
{
	size_t i;

	/* Only squelch if the tval's allowed */
	for (i = 0; i < N_ELEMENTS(tvals); i++)
	{
		if (tval == tvals[i].tval)
			return TRUE;
	}

	return FALSE;
}

/* Simple function that returns the squelch status */
byte get_squelch_status(int k_idx)
{
	object_kind *k_ptr = &k_info[k_idx];
	return (k_ptr->squelch);
}

/*
 * Find the squelch type of the object, or TYPE_MAX if none
 */
static squelch_type_t squelch_type_of(const object_type *o_ptr)
{
	size_t i;

	/* Find the appropriate squelch group */
	for (i = 0; i < N_ELEMENTS(quality_mapping); i++)
	{
		if ((quality_mapping[i].tval == o_ptr->tval) &&
			(quality_mapping[i].min_sval <= o_ptr->sval) &&
			(quality_mapping[i].max_sval >= o_ptr->sval))
			return quality_mapping[i].squelch_type;
	}

	return PS_TYPE_MAX;
}

/*
 * Determines if an object is going to be squelched on identification.
 * Input:
 *  o_ptr   : This is a pointer to the object type being identified.
 *  feeling : This is the feeling of the object if it is being
 *            pseudoidentified or 0 if the object is being identified.
 *  fullid  : Is the object is being identified?
 *
 * Output: One of the three above values.
 */

int squelch_itemp(const object_type *o_ptr, byte feelings, bool fullid)
{
	int num, result;
	byte feel;

	/* Default */
	result = SQUELCH_NO;

	/* Never squelch quest items */
	if (o_ptr->ident & IDENT_QUEST) return result;

	/* Squelch some ego items if known */
	if (fullid && (ego_item_p(o_ptr)) && (e_info[o_ptr->ego_num].squelch))
	{
		/* Squelch fails on inscribed objects */
		return ((o_ptr->obj_note) ? SQUELCH_FAILED: SQUELCH_YES);
	}

	/* Check to see if the object is eligible for squelching on id. */
	num = squelch_type_of(o_ptr);

	/* Never squelched */
	if (num == PS_TYPE_MAX) return result;

	/*
	 * Get the "feeling" of the object.  If the object is being identified
	 * get the feeling returned by a heavy pseudoid.
	 */
	feel = feelings;

	/* Handle fully identified objects */
	if (fullid)  feel = value_check_aux1(o_ptr);

	/* Get result based on the feeling and the squelch_level */
	switch (squelch_level[num])
	{
		case SQUELCH_NONE:
		{
			return result;
		}

		case SQUELCH_CURSED:
		{
			result = (((feel==INSCRIP_BROKEN) ||
				(feel==INSCRIP_TERRIBLE) ||
				(feel==INSCRIP_WORTHLESS) ||
				(feel==INSCRIP_CURSED)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_AVERAGE:
		{
			result = (((feel==INSCRIP_BROKEN) ||
				(feel==INSCRIP_TERRIBLE) ||
				(feel==INSCRIP_WORTHLESS) ||
				(feel==INSCRIP_CURSED) ||
				(feel==INSCRIP_AVERAGE)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_GOOD_STRONG:
		{
			result = (((feel==INSCRIP_BROKEN) ||
				(feel==INSCRIP_TERRIBLE) ||
				(feel==INSCRIP_WORTHLESS) ||
				(feel==INSCRIP_CURSED) ||
				(feel==INSCRIP_AVERAGE) ||
				(feel==INSCRIP_GOOD_STRONG)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_GOOD_WEAK:
		{
			result = (((feel==INSCRIP_BROKEN) ||
				(feel==INSCRIP_TERRIBLE) ||
				(feel==INSCRIP_WORTHLESS) ||
				(feel==INSCRIP_CURSED) ||
				(feel==INSCRIP_AVERAGE) ||
				(feel==INSCRIP_GOOD_STRONG) ||
				(feel==INSCRIP_GOOD_WEAK)) ? SQUELCH_YES : SQUELCH_NO);
			break;
		}

		case SQUELCH_ALL:
		{
			result = SQUELCH_YES;
			break;
		}
	}

	/* Squelching will fail on an artifact or inscribed object */
	if ((result == SQUELCH_YES) && (artifact_p(o_ptr) || o_ptr->obj_note)) result = SQUELCH_FAILED;

	return result;
}

/*
 * This performs the squelch, actually removing the item from the
 * game.  It returns 1 if the item was squelched, and 0 otherwise.
 * This return value is never actually used.
 */
int do_squelch_item(int squelch, int item, object_type *o_ptr)
{
	if (squelch != SQUELCH_YES) return 0;

	/*hack - never squelch quest items*/
	if (o_ptr->ident & IDENT_QUEST) return 0;

	if (item >= 0)
	{
		inven_item_increase(item, -o_ptr->number);
		inven_item_optimize(item);
	}

	else
	{
		floor_item_increase(0 - item, -o_ptr->number);
		floor_item_optimize(0 - item);
	}

	return 1;
}

void rearrange_stack(int y, int x)
{
	s16b o_idx, next_o_idx;
	s16b first_bad_idx, first_good_idx, cur_bad_idx, cur_good_idx;

	object_type *o_ptr;

	bool sq_flag = FALSE;

	/* Initialize */
	first_bad_idx = 0;
	first_good_idx = 0;
	cur_bad_idx = 0;
	cur_good_idx = 0;

	/*go through all the objects*/
	for(o_idx = cave_o_idx[y][x]; o_idx; o_idx = next_o_idx)
	{
		o_ptr = &(o_list[o_idx]);
		next_o_idx = o_ptr->next_o_idx;

		/*is it marked for squelching*/
		sq_flag = ((k_info[o_ptr->k_idx].squelch == SQUELCH_ALWAYS) &&
			(k_info[o_ptr->k_idx].aware));

		if (sq_flag)
		{
			if (first_bad_idx == 0)
			{
				first_bad_idx = o_idx;
				cur_bad_idx = o_idx;
			}

			else
			{
				o_list[cur_bad_idx].next_o_idx = o_idx;
				cur_bad_idx = o_idx;
			}
		}

		else

		{
			if (first_good_idx==0)
			{
				first_good_idx = o_idx;
				cur_good_idx = o_idx;
			}

			else
			{
				o_list[cur_good_idx].next_o_idx = o_idx;
				cur_good_idx = o_idx;
			}
		}
	}

	if (first_good_idx != 0)
	{
		cave_o_idx[y][x] = first_good_idx;
		o_list[cur_good_idx].next_o_idx = first_bad_idx;
		o_list[cur_bad_idx].next_o_idx = 0;
	}

	else
	{
		cave_o_idx[y][x] = first_bad_idx;
	}
}

bool squelch_item_ok(const object_type *o_ptr)
{
	object_kind *k_ptr = k_ptr = &k_info[o_ptr->k_idx];

	/* Always delete "nothings" */
	if (!o_ptr->k_idx) return (TRUE);

	/* Ignore inscribed objects, artifacts , mimics or quest objects */
	if ((o_ptr->obj_note) || (artifact_p(o_ptr)) || (o_ptr->ident & (IDENT_QUEST)) ||
		(o_ptr->mimic_r_idx))
	{
		return (FALSE);
	}

	/* Object kind is set to be always squelched */
	if ((k_ptr->squelch == SQUELCH_ALWAYS) && k_ptr->aware) return (TRUE);

	/* Apply quality squelch if possible */
	if (object_known_p(o_ptr) && (squelch_itemp(o_ptr, 0, TRUE) == SQUELCH_YES)) return TRUE;

	/* Don't squelch */
	return (FALSE);

}


/* Attempt to squelch every object in a pile. */
void do_squelch_pile(int y, int x)
{
	s16b o_idx, next_o_idx;
	object_type *o_ptr;

	for(o_idx = cave_o_idx[y][x]; o_idx; o_idx = next_o_idx)
	{
		/* Get the object */
		o_ptr = &(o_list[o_idx]);

		/* Point to the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Destroy the object? */
		if (squelch_item_ok(o_ptr))
		{
			/* Delete */
			delete_object_idx(o_idx);
		}
	}
}

/*
 * Increase or decrease the squelch setting.
 * The incremental change is intended to be either +1 or -1
 * but the function assures the setting is within
 * the required range to prevent game crashes.
 */
void change_squelch_setting(s16b k_idx, int change)
{
	object_kind *k_ptr = &k_info[k_idx];

	int new_value = k_ptr->squelch + change;

	/* Boundry Control */
	if (new_value < SQUELCH_NEVER)
	{
		k_ptr->squelch = SQUELCH_OPT_MAX - 1;
		return;
	}
	if (new_value >= SQUELCH_OPT_MAX)
	{
		k_ptr->squelch = SQUELCH_NEVER;
		return;
	}

	/* Change is within range */
	k_ptr->squelch += change;

	return;
}

/*** Ego-Item Squelch menu ***/


static tval_desc raw_tvals[] =
{
	{TV_SKELETON, "Skeletons"},
	{TV_BOTTLE, "Bottles"},
	{TV_JUNK, "Junk"},
	{TV_SPIKE, "Spikes"},
	{TV_CHEST, "Chests"},
	{TV_SHOT, "Shots"},
	{TV_ARROW, "Arrows"},
	{TV_BOLT, "Bolts"},
	{TV_BOW, "Launchers"},
	{TV_DIGGING, "Diggers"},
	{TV_HAFTED, "Maces"},
	{TV_POLEARM, "Polearms"},
	{TV_SWORD, "Swords"},
	{TV_BOOTS, "Boots"},
	{TV_GLOVES, "Gloves"},
	{TV_HELM, "Helmets"},
	{TV_CROWN, "Crowns"},
	{TV_SHIELD, "Shields"},
	{TV_CLOAK, "Cloaks"},
	{TV_SOFT_ARMOR, "Soft Armor"},
	{TV_HARD_ARMOR, "Hard Armor"},
	{TV_DRAG_ARMOR, "DSMails"},
	{TV_LIGHT, "Lights"},
	{TV_AMULET, "Amulets"},
	{TV_DRAG_SHIELD, "DSShields"},
	{TV_RING, "Rings"},
	{TV_STAFF, "Staves"},
	{TV_WAND, "Wands"},
	{TV_ROD, "Rods"},
	{TV_SCROLL, "Scrolls"},
	{TV_POTION, "Potions"},
	{TV_FLASK, "Flaskes"},
	{TV_FOOD, "Food"},
	{TV_MAGIC_BOOK, "Magic Books"},
	{TV_PRAYER_BOOK, "Prayer Books"},
	{TV_DRUID_BOOK, "Druid Books"}
};

#define NUM_RAW_TVALS (sizeof(raw_tvals) / sizeof(raw_tvals[0]))

/*
 * Utility function used to find/sort tval names.
 */
static int tval_comp_func(const void *a_ptr, const void *b_ptr)
{
	int a = ((tval_desc *)a_ptr)->tval;
	int b = ((tval_desc *)b_ptr)->tval;
	return a - b;
}


typedef struct ego_desc
{
	s16b e_idx;
	const char *short_name;
} ego_desc;

/*
 * Skip common prefixes in ego-item names.
 */
static const char *strip_ego_name(const char *name)
{
 	if (prefix(name, "of the "))	return name + 7;
 	if (prefix(name, "of "))	return name + 3;
 	return name;
}

/*
 * Utility function used for sorting an array of ego-item indices by
 * ego-item name.
 */
static int ego_comp_func(const void *a_ptr, const void *b_ptr)
{
	const ego_desc *a = a_ptr;
	const ego_desc *b = b_ptr;

	/* Note the removal of common prefixes */
	return (strcmp(a->short_name, b->short_name));
}

/*** Quality-squelch menu ***/

static void ego_squelch_hook(int oid, void *db, const region *loc)
{
	clear_from(0);

	/* Help text */

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = 1;
	text_out_wrap = 79;
	Term_gotoxy(1, 0);

	/* Help text */
	prt("Ego-Item squelch menu", 0, 0);

	Term_gotoxy(1, 1);

	/* Display some helpful information */
	text_out("Use the ");
	text_out_c(TERM_L_GREEN, "space bar");
	text_out(" and ");
	text_out_c(TERM_L_GREEN, "return");
	text_out(" to toggle the squelch setting.  Use ");
	text_out_c(TERM_L_GREEN, "ESC" );
	text_out(" to return to the main squelch menu.  ");
	text_out_c(TERM_L_RED, "[*] Squelch" );
	text_out(" [ ]:   ");
	text_out_c(TERM_L_GREEN, " No Squelch" );
	text_out(".");
}

/*
 * Display an entry in the menu.
 * The procedure for displaying the name is complicated, as it
 * first goes through all of the TVALs and collects a list
 * of all of the TVALS that can have this ego item.  This
 * must be customized for each ego item name.
 */
static void ego_item_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{

	byte tval_table[EGO_TVALS_MAX];
	int i;
	int n = 0;
	const ego_desc *ego_choice = menu->menu_data;
	int idx = ego_choice[oid].e_idx;
	ego_item_type *e_ptr = &e_info[idx];
	char buf[100] = "", *end;
	size_t prefix_size;
	size_t buf_size = sizeof(buf);

	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	const char *long_name;

	/* Paranoia - for some reason this menu has one more entry than there should be. */
	if (!e_ptr->name || !e_ptr->everseen) return;

	/* Copy the valid tvals of this ego-item type */
	for (i = 0; i < EGO_TVALS_MAX; i++)
	{
		/* Ignore "empty" entries */
		if (e_ptr->tval[i] < 1) continue;

		/* Append valid tvals */
		tval_table[n++] = e_ptr->tval[i];
	}

	/* Sort the tvals using bubble sort  */
	for (i = 0; i < n; i++)
	{
		int j;

		for (j = i + 1; j < n; j++)
		{
			if (tval_table[i] > tval_table[j])
			{
				byte temp = tval_table[i];
				tval_table[i] = tval_table[j];
				tval_table[j] = temp;
			}
		}
	}

	/* Initialize the buffer */
	end = my_fast_strcat(buf, NULL, " ", buf_size);

	/* Concatenate the tval' names */
	for (i = 0; i < n; i++)
	{
		/* Fast searching */
		tval_desc key, *result;
		const char *tval_name;

		/* Find the tval's name using binary search */
		key.tval = tval_table[i];
		key.desc = NULL;

		result = bsearch(&key, raw_tvals, NUM_RAW_TVALS, sizeof(raw_tvals[0]), tval_comp_func);

		if (result) tval_name = result->desc;
		/* Paranoia */
		else	tval_name = "????";

		/* Append the proper separator first, if any */
		if (i > 0)
		{
			end = my_fast_strcat(buf, end, (i < n - 1) ? ", ": " and ", buf_size);
		}

		/* Append the name */
		end = my_fast_strcat(buf, end, tval_name, buf_size);
	}

	/* Append an extra space */
	end = my_fast_strcat(buf, end, " ", buf_size);
	/* Get the full ego-item name */
	long_name = e_name + e_ptr->name;

	/* Get the length of the common prefix, if any */
	prefix_size = (ego_choice[oid].short_name - long_name);

	/* Found a prefix? */
	if (prefix_size > 0)
	{
		char prefix[100];

		/* Get a copy of the prefix */
		my_strcpy(prefix, long_name, prefix_size + 1);

		/* Append the prefix */
		end = my_fast_strcat(buf, end, prefix, buf_size);
	}

	/* Print it */
	if (e_ptr->squelch) c_put_str(TERM_L_RED,"[*]", row, col);
	else c_put_str(TERM_L_GREEN, "[ ]", row, col);

	col += 3;

	/* Show the buffer */
	c_put_str(attr, buf, row, col);

	/* Show the stripped ego-item name using another colour */
	if (e_ptr->squelch) attr = TERM_L_RED;
	else attr = TERM_L_GREEN;
	c_put_str(attr, ego_choice[oid].short_name, row, col + (end - buf));


}

/*
 * Handle keypresses.
 */
static bool ego_item_action(char cmd, void *db, int oid)
{
	const ego_desc *ego_choice = db;

	/* Get the selected ego-item type */
	ego_item_type *e_ptr = &e_info[ego_choice[oid].e_idx];

	/* Paranoia - for some reason this menu has one more entry than there should be. */
	if (!e_ptr->name || !e_ptr->everseen) return (FALSE);

	/* Process the command */
	switch (cmd)
	{
		case '\r':
		case '\n':
		{
			/* Toggle the "squelch" flag */
			e_ptr->squelch = !e_ptr->squelch;

			return TRUE;
		}
	}

	return FALSE;

}


/*
 * Display quality squelch menu.
 */
static void ego_item_menu(void *unused, const char *also_unused)
{
	menu_type menu;
	menu_iter menu_f = { NULL, NULL, ego_item_display, ego_item_action };
	region area = { 0, 4, -1, -1 };
	ui_event_data evt = { EVT_NONE, 0, 0, 0, 0 };
	int cursor = 0;
	int num = 0;
	int i, idx;
	ego_item_type *e_ptr;

	ego_desc *ego_choice;

	/* Alloc the array of ego indices */
	ego_choice = C_ZNEW(alloc_ego_size, ego_desc);

	/* Get the valid ego-items */
	for (i = 0; i < alloc_ego_size; i++)
	{
		idx = alloc_ego_table[i].index;

		e_ptr = &e_info[idx];

		/* Only valid known ego-items allowed */
		if (!e_ptr->name || !e_ptr->everseen) continue;

		/* Append the index */
		ego_choice[num].e_idx = idx;
		ego_choice[num].short_name = strip_ego_name(e_name + e_ptr->name);

		++num;
	}

	/* Return here if there are no objects */
	if (!num)
	{
		FREE(ego_choice);

		msg_print("No known ego items!");
		return;
	}

	/* Quickly sort the array by ego-item name */
	qsort(ego_choice, num, sizeof(ego_choice[0]), ego_comp_func);

	/* Save the screen and clear it */
	screen_save();
	clear_from(0);

	/* Set up the menu */
	WIPE(&menu, menu);

	menu.cmd_keys = " \n\r";
	menu.menu_data = ego_choice;
	menu.count = num;
	menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &area);
	menu.browse_hook = ego_squelch_hook;

	/* Select an entry */
	while (evt.key != ESCAPE)
	{
		int old_cursor = cursor;

		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');
		button_add("[TOGGLE]", '\r');

		event_signal(EVENT_MOUSEBUTTONS);

		evt = menu_select(&menu, &cursor, 0);

		if ((evt.type == EVT_SELECT) && (old_cursor == cursor))
		{
			ego_item_action('\r', ego_choice, cursor);
		}

		else if (evt.type == EVT_BUTTON)
		{
			switch (evt.key)
			{
				case '\r':
				{
					ego_item_action(evt.key, ego_choice, cursor);
					break;
				}
				case '?':	{show_file("options.txt#ego_squelch", NULL, 0, 0); break;}
				default:  	break;
			}
		}


	}

	FREE(ego_choice);

	/* Load screen */
	screen_load();
	return;
}




/*** Quality-squelch menu ***/

static void quality_squelch_hook(int oid, void *db, const region *loc)
{
	/* Help text */

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = 1;
	text_out_wrap = 79;
	Term_gotoxy(1, 0);

	/* Help text */
	prt("Quality squelch menu", 0, 0);

	Term_gotoxy(1, 1);

	/* Display some helpful information */
	text_out("Use the ");
	text_out_c(TERM_L_GREEN, "movement keys or mouse");
	text_out(" to navigate.  Use  ");
	if (game_mode == GAME_NPPMORIA) text_out_c(TERM_L_GREEN, "ncvgw");
	else text_out_c(TERM_L_GREEN, "ncvgwa");
	text_out(" to change the highlighted setting.  ");
	if (game_mode == GAME_NPPMORIA) text_out_c(TERM_L_GREEN, "NCVGW" );
	else text_out_c(TERM_L_GREEN, "NCVGWA" );
	text_out(" changes settings for all items.  For Rings and Amulets, only ");
	if (game_mode == GAME_NPPMORIA) text_out_c(TERM_L_GREEN, "nc");
	else text_out_c(TERM_L_GREEN, "nca");
	text_out(" are applicable settings.");
}

/*
 * Display an entry in the menu.
 */
static void quality_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	const char *name = quality_choices[oid].name;

	byte level = squelch_level[oid];
	const char *level_name = quality_values[level].name;

	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	c_put_str(attr, format("%-20s : %s", name, level_name), row, col);


}


/* Increase or decrease the squelch quality level by one for moria objects (no artifacts) */
static void change_squelch_level_moria(int index, int change)
{
	/* only allowable options to be toggled through*/

	/*first do the rings and amulets*/
	if ((index == PS_TYPE_AMULET) || (index == PS_TYPE_RING))
	{
		/*
		 * Move up to the next squelch setting, or go back down to the lowest setting if
		 * we are at the top.
		 */
		if (squelch_level[index] == SQUELCH_NONE) squelch_level[index] = SQUELCH_CURSED;
		else squelch_level[index] = SQUELCH_NONE;
	}


	/*
	 * Move up to the next squelch setting, or go back down to the lowest setting if
	 * we are at the top.
	 */
	else if (change > 0)
	{
		/* Weak Pseudo ID */

		if (!(cp_ptr->flags & (CF_PSEUDO_ID_HEAVY)))
		{
			if (squelch_level[index] == SQUELCH_GOOD_STRONG) squelch_level[index] = SQUELCH_NONE;
			else if (squelch_level[index] == SQUELCH_AVERAGE) squelch_level[index] = SQUELCH_GOOD_WEAK;
			else if (squelch_level[index] == SQUELCH_GOOD_WEAK) squelch_level[index] = SQUELCH_GOOD_STRONG;
			else squelch_level[index]++;

			return;
		}
		/* Strong Pseudo ID */
		else
		{
			if (squelch_level[index] == SQUELCH_GOOD_STRONG) squelch_level[index] = SQUELCH_NONE;
			else squelch_level[index]++;
		}
	}
	else /* (change <=0) */
	{

		/* Weak Pseudo ID */
		if (!(cp_ptr->flags & (CF_PSEUDO_ID_HEAVY)))
		{
			if (squelch_level[index] == SQUELCH_GOOD_STRONG) squelch_level[index] = SQUELCH_GOOD_WEAK;
			else if (squelch_level[index] == SQUELCH_GOOD_WEAK) squelch_level[index] = SQUELCH_AVERAGE;
			else if (squelch_level[index] == SQUELCH_NONE) squelch_level[index] = SQUELCH_GOOD_STRONG;
			else squelch_level[index]--;
		}
		else
		{
			/* Strong Pseudo ID */
			if (squelch_level[index] == SQUELCH_NONE) squelch_level[index] = SQUELCH_GOOD_STRONG;
			else squelch_level[index]--;
		}
	}

	return;
}


/* Increase or decrease the squelch quality level by one */
static void change_squelch_level(int index, int change)
{
	/* Slightly different handling for Moria */
	if (game_mode == GAME_NPPMORIA)
	{
		change_squelch_level_moria(index, change);
		return;
	}

	/* only allowable  options to be toggled through*/

	/*first do the rings and amulets*/
	if ((index == PS_TYPE_AMULET) || (index == PS_TYPE_RING))
	{
		/*
		 * Move up to the next squelch setting, or go back down to the lowest setting if
		 * we are at the top.
		 */
		if (change > 0)
		{
			if (squelch_level[index] == SQUELCH_NONE) squelch_level[index] = SQUELCH_CURSED;
			else if (squelch_level[index] == SQUELCH_CURSED) squelch_level[index] = SQUELCH_ALL;
			else squelch_level[index] = SQUELCH_NONE;
		}
		else /* (change <=0) */
		{
			if (squelch_level[index] == SQUELCH_CURSED) squelch_level[index] = SQUELCH_NONE;
			else if (squelch_level[index] == SQUELCH_ALL) squelch_level[index] = SQUELCH_CURSED;
			else squelch_level[index] = SQUELCH_ALL;
		}

	}

	/* Everything else*/
	else
	{

		/*
		 * Move up to the next squelch setting, or go back down to the lowest setting if
		 * we are at the top.
		 */
		if (change > 0)
		{
			if (squelch_level[index] == SQUELCH_ALL) squelch_level[index] = SQUELCH_NONE;
			else if (squelch_level[index] == SQUELCH_GOOD_STRONG) squelch_level[index] = SQUELCH_ALL;
			else squelch_level[index]++;

		}
		else /* (change <=0) */
		{

			if (squelch_level[index] == SQUELCH_NONE) squelch_level[index] = SQUELCH_ALL;
			else if (squelch_level[index] == SQUELCH_ALL) squelch_level[index] = SQUELCH_GOOD_STRONG;
			else squelch_level[index]--;
		}
	}

	return;
}

/*
 * Handle keypresses.
 */
static bool quality_action(char cmd, void *db, int oid)
{
	int index = oid;

	int i;

	(void)db;

	if (game_mode == GAME_NPPMORIA)
	{
		if (cmd == 'a') cmd = 'w';
		else if (cmd == 'A') cmd = 'W';
	}

	/* Analyze */
	switch (cmd)
	{
		/*Never squelch */
		case 'n':
		{
			squelch_level[index] = SQUELCH_NONE;
			break;
		}
		case 'N':
		{
			for (i=0; i < SQUELCH_BYTES; i++)
			{
				squelch_level[i] = SQUELCH_NONE;
			}
			break;
		}
		/* Set to squelch cursed */
		case 'c':
		{
			squelch_level[index] = SQUELCH_CURSED;
			break;
		}
		case 'C':
		{
			for (i = 0; i < SQUELCH_BYTES; i++)
			{
					squelch_level[i] = SQUELCH_CURSED;
			}
			break;
		}
			/* Set to squelch average and below */
		case 'v':
		{
			if ((index != PS_TYPE_AMULET)	&& (index != PS_TYPE_RING))
			{
				squelch_level[index] = SQUELCH_AVERAGE;
			}
			else return (FALSE);
			break;
		}
		case 'V':
		{
			for (i = 0; i < SQUELCH_BYTES ; i++)
			{
				/* Aumulets and rings only have cursed and good settings */
				if ((i == PS_TYPE_AMULET) || (i == PS_TYPE_RING)) continue;
				/* The rest can be set to average */
				squelch_level[i] = SQUELCH_AVERAGE;
			}
			break;
		}

		/* Set to squelch good (identified and strong pseudo-id) and below */
		case 'g':
		{
			if ((index != PS_TYPE_AMULET)	&& (index != PS_TYPE_RING))
			{
				squelch_level[index] = SQUELCH_GOOD_STRONG;
			}
			else return (FALSE);
			break;
		}
		case 'G':
		{
			for (i = 0; i < SQUELCH_BYTES; i++)
			{
				if ((i == PS_TYPE_AMULET) || (i == PS_TYPE_RING)) continue;
					squelch_level[i] = SQUELCH_GOOD_STRONG;
			}
				break;
		}
		/* Squelch to good (weak pseudo-id and below ) */
		case 'w':
		{
			if ((index != PS_TYPE_AMULET)	&& (index != PS_TYPE_RING))
			{
				squelch_level[index] = SQUELCH_GOOD_WEAK;
			}
			else return (FALSE);

			break;
		}
		case 'W':
		{
			for (i = 0; i < SQUELCH_BYTES; i++)
			{
				if ((i == PS_TYPE_AMULET) || (i == PS_TYPE_RING)) continue;
				squelch_level[i] = SQUELCH_GOOD_WEAK;
			}
			break;
		}
		/* Squelch all but artifact */
		case 'a':
		{
			squelch_level[index] = SQUELCH_ALL;
			break;
		}

		case 'A':
		{
			for (i = 0; i < SQUELCH_BYTES; i++)
			{
				squelch_level[i] = SQUELCH_ALL;
			}
			break;
		}

		case '-':
		case '4':
		{
			change_squelch_level(index, -1);
			break;
		}

		case '+':
		case '6':
		case DEFINED_XFF:
		{
			change_squelch_level(index, 1);
			break;
		}
		default:  return (FALSE);
	}

	return TRUE;

}

/*
 * Display quality squelch menu.
 */
static void quality_menu(void *unused, const char *also_unused)
{
	menu_type menu;
	menu_iter menu_f = { NULL, NULL, quality_display, quality_action };
	ui_event_data evt = { EVT_NONE, 0, 0, 0, 0 };
	region area = { 0, 5, -1, -1 };
	int cursor = 0;

	/* Save the screen and clear it */
	screen_save();
	clear_from(0);

	/* Set up the menu */
	WIPE(&menu, menu);

	if (game_mode == GAME_NPPMORIA) menu.cmd_keys = "nNcCvVgGwW+-\n\r";
	else menu.cmd_keys = "nNcCvVgGwWaA+-\n\r";

	menu.selections = "bdefhijklmopqrstuwxyz";
	menu.browse_hook = quality_squelch_hook;
	menu.count = PS_TYPE_MAX;
	menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &area);
	menu.flags = MN_DBL_TAP;

	/* Select an entry */
	while (evt.key != ESCAPE)
	{
		bool strong_squelch = (cursor != PS_TYPE_AMULET) && (cursor != PS_TYPE_RING);
		button_kill_all();
		/* Kill the buttons */
		button_add("ESC|", ESCAPE);
		button_add("HELP|", '?');
		button_add("NEVER_SQ|", 'n');
		button_add("SQ_CURSED|", 'c');
		if(strong_squelch)
		{
			button_add("SQ_AVE|", 'v');
			if (!(cp_ptr->flags & (CF_PSEUDO_ID_HEAVY)))	button_add("SQ_WEAK|", 'w');
			button_add("SQ_GOOD|", 'g');
		}
		if (game_mode != GAME_NPPMORIA) button_add("SQ_GREAT", 'a');
		button_add("[+]", '+');
		button_add("[-]", '-');
		event_signal(EVENT_MOUSEBUTTONS);

		evt = menu_select(&menu, &cursor, EVT_MOVE);

		if (evt.type == EVT_BUTTON)
		{
			switch (evt.key)
			{
				case 'n':
				case 'c':
				case 'v':
				case 'w':
				case 'g':
				case 'a':
				case '+':
				case '-':
				{
					quality_action(evt.key, NULL, cursor);
					break;
				}
				case '?':	{show_file("options.txt#qual_squelch", NULL, 0, 0); break;}
				default:  	break;
			}
		}
	}

	/* Load screen */
	screen_load();
	return;
}



/*** Object Squelch Menu ***/

static void object_squelch_hook(int oid, void *db, const region *loc)
{
	/* Help text */

	/* Output to the screen */
	text_out_hook = text_out_to_screen;

	/* Indent output */
	text_out_indent = 1;
	text_out_wrap = 79;
	Term_gotoxy(1, 0);

	/* Display some helpful information */
	text_out("Use the ");
	text_out_c(TERM_L_GREEN, "ESC");
	text_out(" to return to the previous menu.  ");
	text_out_c(TERM_L_GREEN, "{");
	text_out(" to create an autoinscription.  ");
	text_out_c(TERM_L_GREEN, "Enter, '+', or '-'" );
	text_out(" changes the current setting.  'N' changes to setting ");
	text_out_c(TERM_YELLOW, "Never Squelch");
	text_out(" and defer to the game pickup options.  'L' changes to  ");
	text_out_c(TERM_L_GREEN, "Never Pickup");
	text_out(".  'A' changes to  ");
	text_out_c(TERM_L_UMBER, "Always Pickup");
	text_out(".  'S' changes to ");
	text_out_c(TERM_L_RED, "Always Squelch");
	text_out(".");

	text_out_indent = 0;
}

/*
 * Display an entry on the sval menu
 */
static void object_squelch_display(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	char buf[80];
	const squelch_choice *choice = (const squelch_choice *) menu->menu_data;
	int idx = choice[oid].idx;
	const char *inscrip = get_autoinscription(idx);

	byte attr = (cursor ? TERM_L_BLUE : TERM_WHITE);

	/* Acquire the "name" of object "i" */
	strip_name(buf, idx);

	/* Print it */
	c_put_str(attr, format("%s", buf), row, (col + 4));
	if (choice[oid].aware)
	{
		object_kind *k_ptr = &k_info[idx];
		byte color;
		char sq_mode = 'E';  /*Error, should never happen*/
		byte squelch = get_squelch_status(idx);

		color = squelch_status_color[k_ptr->squelch];

		if 		(squelch == SQUELCH_NEVER) sq_mode = 'N';
		else if (squelch == SQUELCH_ALWAYS) sq_mode = 'S';
		else if (squelch == NO_SQUELCH_NEVER_PICKUP) sq_mode = 'L';
		else if (squelch == NO_SQUELCH_ALWAYS_PICKUP) sq_mode = 'A';

		c_put_str(color, format("[%c]", sq_mode), row, col);
		if (inscrip) c_put_str(TERM_YELLOW, inscrip, row, (col + 40));

	}
}

/*
 * Deal with events on the object squelch menu
 */
static bool object_squelch_action(char cmd, void *db, int oid)
{
	const squelch_choice *choice = (const squelch_choice *) db;

	int idx = choice[oid].idx;

	switch (cmd)
	{
		case '{':
		{
			char note_text[80] = "";

			/* Avoid the prompt getting in the way */
			screen_save();

			/* Prompt */
			prt("Inscribe with: ", 0, 0);

			/* Default note */
			if (idx != -1)
			{
				strnfmt(note_text, sizeof(note_text), "%s", get_autoinscription(idx));
			}

			/* Get an inscription */
			if (askfor_aux(note_text, sizeof(note_text), NULL))
			{
				/* Remove old inscription if existent */
				if (idx != -1)
				remove_autoinscription(idx);

				/* Add the autoinscription */
				add_autoinscription(idx, note_text);

				/* Notice stuff (later) */
				p_ptr->notice |= (PN_AUTOINSCRIBE);
				p_ptr->redraw |= (PR_INVEN | PR_EQUIP);

				/* Reload the screen */
				screen_load();

				return (TRUE);
			}

			/* Reload the screen */
			screen_load();
			return (FALSE);
		}
		case 'S':
		{
			k_info[idx].squelch = SQUELCH_ALWAYS;
			return (TRUE);
		}
		case 'L':
		{
			k_info[idx].squelch = NO_SQUELCH_NEVER_PICKUP;
			return (TRUE);
		}
		case 'A':
		{
			k_info[idx].squelch = NO_SQUELCH_ALWAYS_PICKUP;
			return (TRUE);
		}
		case 'N':
		{
			k_info[idx].squelch = SQUELCH_NEVER;
			return (TRUE);
		}
		case '+':
		case DEFINED_XFF:
		{
			change_squelch_setting(idx, 1);
			return TRUE;
		}
		case '-':
		{
			change_squelch_setting(idx, -1);
			return TRUE;
		}
		default: break;
	}

	return FALSE;
}


/*
 * Display list of objects to be squelched.
 */
static bool object_sqelch_menu(int tval, const char *desc)
{
	menu_type menu;
	menu_iter menu_f = { NULL, NULL, object_squelch_display, object_squelch_action };
	ui_event_data evt = { EVT_NONE, 0, 0, 0, 0 };
	int cursor = 0;
	region area = { 0, 4, -1, -1 };
	int num = 0;
	size_t i;
	int x, y;

	squelch_choice *choice;

	/* Create the array, with entries both for aware and unaware squelch */
	choice = C_ZNEW(2 * z_info->k_max, squelch_choice);

	/* Iterate over all possible object kinds, finding ones which can be squelched */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects, unseen objects, and incorrect tvals */
		if (!k_ptr->name) continue;
		if (tv_to_type[k_ptr->tval] != tval) continue;
		if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;
		if (k_ptr->tval == TV_GOLD) continue;
		if (!k_ptr->everseen) continue;

		/* aware squelch requires everseen */
		choice[num].idx = i;
		choice[num].aware = TRUE;
		num++;
	}

	/* Return here if there are no objects */
	if (!num)
	{
		FREE(choice);
		return FALSE;
	}

	/* Simple bubble sort */
	for (x = 0; x < num; x++)
	{
		for (y = x; y < num; y++)
		{
			if ((k_info[choice[x].idx].tval > k_info[choice[y].idx].tval) ||
				((k_info[choice[x].idx].tval == k_info[choice[y].idx].tval) &&
				(k_info[choice[x].idx].cost > k_info[choice[y].idx].cost)))
			{
				squelch_choice temp = choice[x];
				choice[x] = choice[y];
				choice[x] = temp;
			}
		}
	}

	/* Save the screen and clear it */
	screen_save();
	clear_from(0);

	/* Set up the menu */
	WIPE(&menu, menu);

	menu.cmd_keys = "ANLS{+-";
	/*menu.selections = "abcdefghijklmnopqrstuvwxyzBCDEFGHIJKMOPQRTUVWXYZ1234567890[]!@#";*/
	menu.count = num;
	menu.menu_data = choice;
	menu_init(&menu, MN_SKIN_SCROLL, &menu_f, &area);
	menu.flags = MN_DBL_TAP;
	menu.browse_hook = object_squelch_hook;

	/* Select an entry */
	while (evt.key != ESCAPE)
	{
		button_kill_all();
		button_add("[ESC]", ESCAPE);
		button_add("[HELP]", '?');
		button_add("[NEVER_SQ]", 'N');
		button_add("[NEVER_PICKUP]", 'L');
		button_add("[AUTO_PICKUP]", 'A');
		button_add("[SQUELCH]", 'S');
		button_add("[INSCRIBE]", '{');
		button_add("[-]", '-');
		button_add("[+]", '+');

		event_signal(EVENT_MOUSEBUTTONS);

		evt = menu_select(&menu, &cursor, 0);

		if (evt.type == EVT_BUTTON)
		{
			switch (evt.key)
			{
				case 'N':
				case 'L':
				case 'A':
				case 'S':
				case '{':
				case '+':
				case '-':
				{
					object_squelch_action(evt.key, choice, cursor);
					break;
				}
				case '?':	{show_file("options.txt#squelch", NULL, 0, 0); break;}
				default:  	break;
			}
		}
	}

	/* Free memory */
	FREE(choice);

	/* Load screen */
	screen_load();
	return TRUE;
}


/*
 * Hack -- initialize the mapping from tvals to typevals.
 * This is currently called every time the squelch menus are
 * accessed.  This can certainly be improved.
 */

static void init_tv_to_type(void)
{
	int i;

	/* Sort the tval table  */
	qsort(raw_tvals, NUM_RAW_TVALS, sizeof(raw_tvals[0]), tval_comp_func);

	/* Clear all of the types that have been seen */
	for (i = 0; i < TYPE_MAX; i++)
	{
		seen_type[i] = FALSE;
	}

	/* Make sure all the TV_TY_TYPES are within bounds */
	for (i = 0; i < MAXTV_TO_TYPE; i++)
	{
		tv_to_type[i] = TYPE_MAX;
	}

	tv_to_type[TV_SKELETON]=TYPE_MISC;
	tv_to_type[TV_BOTTLE]=TYPE_MISC;
	tv_to_type[TV_JUNK]=TYPE_MISC;
	tv_to_type[TV_SPIKE]=TYPE_MISC;
	tv_to_type[TV_CHEST]=TYPE_MISC;
	tv_to_type[TV_SHOT]=TYPE_AMMO;
	tv_to_type[TV_ARROW]=TYPE_AMMO;
	tv_to_type[TV_BOLT]=TYPE_AMMO;
	tv_to_type[TV_BOW]=TYPE_BOW;
	tv_to_type[TV_DIGGING]=TYPE_WEAPON2;
	tv_to_type[TV_HAFTED]=TYPE_WEAPON2;
	tv_to_type[TV_POLEARM]=TYPE_WEAPON2;
	tv_to_type[TV_SWORD]=TYPE_WEAPON1;
	tv_to_type[TV_BOOTS]=TYPE_BOOTS;
	tv_to_type[TV_GLOVES]=TYPE_GLOVES;
	tv_to_type[TV_HELM]=TYPE_HELM;
	tv_to_type[TV_CROWN]=TYPE_HELM;
	tv_to_type[TV_SHIELD]=TYPE_SHIELD;
	tv_to_type[TV_CLOAK]=TYPE_CLOAK;
	tv_to_type[TV_SOFT_ARMOR]=TYPE_BODY;
	tv_to_type[TV_HARD_ARMOR]=TYPE_BODY;
	tv_to_type[TV_DRAG_ARMOR]=TYPE_BODY;
	tv_to_type[TV_DRAG_SHIELD]=TYPE_SHIELD;
	tv_to_type[TV_LIGHT]=TYPE_MISC;
	tv_to_type[TV_AMULET]=TYPE_AMULET;
	tv_to_type[TV_RING]=TYPE_RING;
	tv_to_type[TV_STAFF]=TYPE_STAFF;
	tv_to_type[TV_WAND]=TYPE_WAND;
	tv_to_type[TV_ROD]=TYPE_ROD;
	tv_to_type[TV_SCROLL]=TYPE_SCROLL;
	tv_to_type[TV_POTION]=TYPE_POTION;
	tv_to_type[TV_FLASK]=TYPE_MISC;
	tv_to_type[TV_FOOD]=TYPE_FOOD;
	tv_to_type[TV_MAGIC_BOOK]=TYPE_BOOK;
	tv_to_type[TV_PRAYER_BOOK]=TYPE_BOOK;
	tv_to_type[TV_DRUID_BOOK]=TYPE_BOOK;

	/* Note the types of objects that have been seen */
	for (i = 1; i < z_info->k_max; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip empty objects, artifacts, gold and unseen objects */
		if (!k_ptr->name) continue;
		if (!k_ptr->everseen) continue;
		if (k_ptr->k_flags3 & (TR3_INSTA_ART)) continue;
		if (k_ptr->tval == TV_GOLD) continue;

		/* Note that we have seen this type */
		seen_type[tv_to_type[k_ptr->tval]] = TRUE;
	}

}

static void squelch_prefs_save(void *unused, const char *also_unused)
{
	int col = 26;
	int row = 17;
	char ftmp[80];
	char buf[80];
	int i, tval, sval, squelch;

	ang_file *fff;

	/* Prompt */
	prt("Command: Save Squelch Info", row, col);

	/* Prompt */
	prt("File: ", row+1, col);

	/* Default filename */
	sprintf(ftmp, "%s.squ", op_ptr->base_name);

	/* Get a filename */
	if (askfor_aux(ftmp, 80, NULL))
	{
		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

		/* Drop priv's */
		safe_setuid_drop();

		/* Open the file */
		fff = file_open(buf, MODE_WRITE, FTYPE_TEXT);

		/* Grab priv's */
		safe_setuid_grab();

		/* Test for success */
		if (fff)
		{
			/* Skip some lines */
			file_putf(fff, "\n\n");

			/* Start dumping */
			file_putf(fff, "# Squelch bits\n\n");

			/* Dump squelch bits */
			for (i = 1; i < z_info->k_max; i++)
			{
				tval = k_info[i].tval;
				sval = k_info[i].sval;
				squelch = (k_info[i].squelch);

				/* Dump the squelch info */
				if (tval || sval)
				{
					file_putf(fff, "Q:%d:%d:%d:%d\n", i, tval, sval, squelch);
				}
			}

			file_putf(fff, "\n\n# squelch_level array\n\n");

			for(i = 0 ; i < SQUELCH_BYTES; i++)
			{
				file_putf(fff, "Q:%d:%d\n", i, squelch_level[i]);
			}

			/* All done */
			file_putf(fff, "\n\n\n\n");

			/* Close */
			file_close(fff);

			/* Ending message */
			prt("Squelch file saved successfully.  (Hit a key.)", row, col);
			(void)inkey_ex();
		}

	}

}

static void squelch_prefs_load(void *unused, const char *also_unused)
{
	int col = 26;
	int row = 17;
	char ftmp[80];

	/* Prompt */
	prt("Command: Load squelch info from file", row, col);

	/* Prompt */
	prt("File: ", row+1, col);

	/* Default filename */
	sprintf(ftmp, "%s.squ", op_ptr->base_name);

	/* Ask for a file (or cancel) */
	if (askfor_aux(ftmp, 80, NULL))
	{
		/* Process the given filename */
		if (process_pref_file(ftmp))
		{
			/* Mention failure */
			prt("Failed to load squelch file!  (Hit a key.)", row+1, col);
		}
		else
		{
			/* Mention success */
			prt("Squelch data loaded!  (Hit a key.)",  row+1, col);
		}
		(void)inkey_ex();
	}
}

static void autoinscribe_prefs_save(void *unused, const char *also_unused)
{
	int col = 26;
	int row = 17;
	char ftmp[80];
	char buf[80];
	int i;

	ang_file *fff;

	/* Prompt */
	prt("Command: Save Autoinscribe Info", row, col);

	/* Prompt */
	prt("File: ", row+1, col);

	/* Default filename */
	sprintf(ftmp, "%s.ins", op_ptr->base_name);

	/* Get a filename */
	if (askfor_aux(ftmp, 80, NULL))
	{
		/* Build the filename */
		path_build(buf, 1024, ANGBAND_DIR_USER, ftmp);

		/* Drop priv's */
		safe_setuid_drop();

		/* Overwrite the file */
		fff = file_open(buf, MODE_WRITE, FTYPE_TEXT);

		/* Grab priv's */
		safe_setuid_grab();

		/* Test for success */
		if (fff && inscriptions)
		{
			/* Start dumping */
			file_putf(fff, "# Format: B:[Item Kind]:[Inscription]\n\n");

			for (i = 0; i < inscriptionsCount; i++)
			{
				object_kind *k_ptr = &k_info[inscriptions[i].kindIdx];

				/* Write a comment for the autoinscription*/
				file_putf(fff, "# Autoinscription for %s\n", k_name + k_ptr->name);

				/* Dump the autoinscribe info */
				file_putf(fff, "B:%d:%s\n\n", inscriptions[i].kindIdx,
								quark_str(inscriptions[i].inscriptionIdx));
			}

			/* Close */
			file_close(fff);

			/* Ending message */
			prt("Autoinscribe file saved successfully.  (Hit a key.)", row+1, col);
			(void)inkey_ex();
		}
	}
}

static void autoinscribe_prefs_load(void *unused, const char *also_unused)
{
	int col = 26;
	int row = 17;
	char ftmp[80];

	/* Prompt */
	prt("Command: Load Autoinscribe info from file", row, col);

	/* Prompt */
	prt("File: ", row+1, col);

	/* Default filename */
	sprintf(ftmp, "%s.ins", op_ptr->base_name);

	/* Ask for a file (or cancel) */
	if (askfor_aux(ftmp, 80, NULL))
	{
		/* Process the given filename */
		if (process_pref_file(ftmp))
		{
			/* Mention failure */
			prt("Failed to load autoinscribe file!  (Hit a key.)", row+1, col);
		}

		else
		{
			/* Mention success */
			prt("Autoinscribe data loaded!  (Hit a key.)", row+1, col);
		}
		(void)inkey_ex();

	}
}


/* Extra options on the "item options" menu */
struct
{
	char tag;
	const char *name;
	void (*action)(void *unused, const char *also_unused);
} extra_item_options[] =
{
	{ 'Q', "Quality squelching options", quality_menu },
	{ 'E', "Ego squelching options", ego_item_menu },
	{ 'S', "Save squelch values to pref file.", squelch_prefs_save},
	{ 'L', "Load squelch values from pref file.", squelch_prefs_load},
	{ 'B', "Save autoinscriptions to pref file.", autoinscribe_prefs_save},
	{ 'G', "Load autoinscriptions from pref file.", autoinscribe_prefs_load},
};

static char tag_options_item(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(tvals))
		return I2A(oid);

	/* Separator - blank line. */
	if (line == N_ELEMENTS(tvals))
		return 0;

	line = line - N_ELEMENTS(tvals) - 1;

	if (line < N_ELEMENTS(extra_item_options))
		return extra_item_options[line].tag;

	return 0;
}

static int valid_options_item(menu_type *menu, int oid)
{
	size_t line = (size_t) oid;

	if (line < N_ELEMENTS(tvals))
		return 1;

	/* Separator - blank line. */
	if (line == N_ELEMENTS(tvals))
		return 0;

	line = line - N_ELEMENTS(tvals) - 1;

	if (line < N_ELEMENTS(extra_item_options))
		return 1;

	return 0;
}

static void display_options_item(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
	size_t line = (size_t) oid;

	/* First section of menu - the svals */
	if (line < N_ELEMENTS(tvals))
	{
		bool known = seen_type[tvals[line].tval];
		byte attr = curs_attrs[known ? CURS_KNOWN: CURS_UNKNOWN][(int)cursor];

		c_prt(attr, tvals[line].desc, row, col);
	}
	/* Second section - the "extra options" */
	else
	{
		byte attr = curs_attrs[CURS_KNOWN][(int)cursor];

		line = line - N_ELEMENTS(tvals) - 1;

		if (line < N_ELEMENTS(extra_item_options))
			c_prt(attr, extra_item_options[line].name, row, col);
	}
}


static const menu_iter options_item_iter =
{
	tag_options_item,
	valid_options_item,
	display_options_item,
	NULL
};


/*
 * Display and handle the main squelching menu.
 */
void do_cmd_squelch_autoinsc(void *unused, cptr title)
{
	int cursor = 0;
	ui_event_data c = EVENT_EMPTY;
	const char cmd_keys[] = { ARROW_LEFT, ARROW_RIGHT, '\0' };
	menu_type menu;
	region area = (mouse_buttons ? SCREEN_REGION_BUTTONS : SCREEN_REGION);

	init_tv_to_type();

	WIPE(&menu, menu_type);
	menu.title = title;
	menu.cmd_keys = cmd_keys;
 	menu.count = N_ELEMENTS(tvals) + N_ELEMENTS(extra_item_options) + 1;
	menu_init(&menu, MN_SKIN_SCROLL, &options_item_iter, &area);

	/* Save and clear screen */
	screen_save();

	/* Kill the buttons */
	button_kill_all();

	while (c.key != ESCAPE)
	{
		clear_from(0);
		/* Kill the buttons */
		button_kill_all();
		button_add("ESC|", ESCAPE);
		button_add("HELP|", '?');
		button_add("QUAL_SQ|", 'Q');
		button_add("EGO_SQ|", 'E');
		button_add("SAVE_SQ|", 'S');
		button_add("LOAD_SQ|", 'L');
		button_add("SAVE_INSCRIP|", 'B');
		button_add("LOAD_INSCRIP", 'G');
		event_signal(EVENT_MOUSEBUTTONS);

		c = menu_select(&menu, &cursor, 0);

		if (c.type == EVT_SELECT)
		{
			if ((size_t) cursor < N_ELEMENTS(tvals))
			{
				object_sqelch_menu(tvals[cursor].tval, tvals[cursor].desc);
			}
			else
			{
				cursor = cursor - N_ELEMENTS(tvals) - 1;
				if ((size_t) cursor < N_ELEMENTS(extra_item_options))
					extra_item_options[cursor].action(NULL, NULL);
			}
		}
		else if (c.type == EVT_BUTTON)
		{
			switch (c.key)
			{
				case 'Q':	{quality_menu(NULL, NULL); break;}
				case 'E':	{ego_item_menu(NULL, NULL); break;}
				case 'S':	{squelch_prefs_save(NULL, NULL); break;}
				case 'L':	{squelch_prefs_load(NULL, NULL); break;}
				case 'B':	{autoinscribe_prefs_save(NULL, NULL); break;}
				case 'G':	{autoinscribe_prefs_load(NULL, NULL); break;}
				case '?':	{show_file("options.txt#squelch", NULL, 0, 0); break;}
				default:  	break;
			}
		}
	}

	/* Load screen and finish */
	screen_load();



	return;
}


