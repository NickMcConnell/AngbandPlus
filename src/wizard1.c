/* File: wizard1.c */

/* Purpose: Spoiler generation -BEN- */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Robert A. Koeneke and Ben Harrison have released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */

#include "angband.h"

#ifdef ALLOW_SPOILERS

/*
* The spoiler file being created
*/
/*FILE *fff = NULL; */



/*
* Extract a textual representation of an attribute
*/
static cptr attr_to_text(byte a)
{
	switch (a)
	{
	case TERM_DARK:    return ("xxx");
	case TERM_WHITE:   return ("White");
	case TERM_SLATE:   return ("Slate");
	case TERM_ORANGE:  return ("Orange");
	case TERM_RED:     return ("Red");
	case TERM_GREEN:   return ("Green");
	case TERM_BLUE:    return ("Blue");
	case TERM_UMBER:   return ("Umber");
	case TERM_L_DARK:  return ("L.Dark");
	case TERM_L_WHITE: return ("L.Slate");
	case TERM_VIOLET:  return ("Violet");
	case TERM_YELLOW:  return ("Yellow");
	case TERM_L_RED:   return ("L.Red");
	case TERM_L_GREEN: return ("L.Green");
	case TERM_L_BLUE:  return ("L.Blue");
	case TERM_L_UMBER: return ("L.Umber");
	}

	/* Oops */
	return ("Icky");
}



/*
* A tval grouper
*/
typedef struct
{
	byte tval;
	cptr name;
} grouper;



/*
* Item Spoilers by: benh@phial.com (Ben Harrison)
*/


/*
* The basic items categorized by type
*/
static grouper group_item[] =
{
	{ TV_SHOT,              "Ammo" },
	{ TV_ARROW,               NULL },
	{ TV_BOLT,                NULL },

	{ TV_BOW,               "Bows" },

	{ TV_SWORD,             "Weapons" },
	{ TV_POLEARM,     NULL },
	{ TV_HAFTED,      NULL },
	{ TV_DIGGING,     NULL },

	{ TV_SOFT_ARMOR,        "Armour (Body)" },
	{ TV_HARD_ARMOR,          NULL },
	{ TV_DRAG_ARMOR,          NULL },

	{ TV_CLOAK,             "Armour (Misc)" },
	{ TV_SHIELD,      NULL },
	{ TV_HELM,                NULL },
	{ TV_CROWN,               NULL },
	{ TV_GLOVES,      NULL },
	{ TV_BOOTS,               NULL },

	{ TV_AMULET,    "Amulets" },
	{ TV_RING,              "Rings" },

	{ TV_SCROLL,    "Scrolls" },
	{ TV_POTION,    "Potions" },
	{ TV_FOOD,              "Food" },

	{ TV_ROD,               "Rods" },
	{ TV_WAND,              "Wands" },
	{ TV_STAFF,             "Staffs" },

	{ TV_MIRACLES_BOOK,       "Books (Miracles)" },
	{ TV_SORCERY_BOOK,    "Books (Sorcery)" },
	{ TV_NATURE_BOOK,     "Books (Nature)" },
	{ TV_CHAOS_BOOK,      "Books (Demonic)" },
	{ TV_DEATH_BOOK,      "Books (Death)" },
	{ TV_TAROT_BOOK,      "Books (Tarot)" },
	{ TV_CHARMS_BOOK,     "Books (Charms)" },
	{ TV_SOMATIC_BOOK,     "Books (Somatic)"},
   	{ TV_DEMONIC_BOOK,     "Books (Demon)"}, 

	{ TV_CHEST,             "Chests" },

	{ TV_SPIKE,             "Various" },
	{ TV_LITE,                NULL },
	{ TV_FLASK,               NULL },
	{ TV_JUNK,                NULL },
	{ TV_BOTTLE,      NULL },
	{ TV_SKELETON,    NULL },

	{ 0, "" }
};





/*
* Describe the kind
*/
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val, int k)
{
	object_type forge;
	object_type *q_ptr;

	object_kind *k_ptr;


	/* Get local object */
	q_ptr = &forge;

	/* Prepare a fake item */
	object_prep(q_ptr, k);

	/* Obtain the "kind" info */
	k_ptr = &k_info[q_ptr->k_idx];

	/* It is known */
	q_ptr->ident |= (IDENT_KNOWN);

	/* Cancel bonuses */
	q_ptr->pval = 0;
	q_ptr->to_a = 0;
	q_ptr->to_h = 0;
	q_ptr->to_d = 0;


	/* Level */
	(*lev) = k_ptr->level;

	/* Value */
	(*val) = object_value(q_ptr);


	/* Hack */
	if (!buf || !dam || !wgt) return;


	/* Description (too brief) */
	object_desc_store(buf, q_ptr, FALSE, 0);


	/* Misc info */
	strcpy(dam, "");

	/* Damage */
	switch (q_ptr->tval)
	{
		/* Bows */
	case TV_BOW:
		{
			break;
		}

		/* Ammo */
	case TV_SHOT:
	case TV_BOLT:
	case TV_ARROW:
		{
			sprintf(dam, "%dd%d", q_ptr->dd, q_ptr->ds);
			break;
		}

		/* Weapons */
	case TV_HAFTED:
	case TV_POLEARM:
	case TV_SWORD:
	case TV_DIGGING:
		{
			sprintf(dam, "%dd%d", q_ptr->dd, q_ptr->ds);
			break;
		}

		/* Armour */
	case TV_BOOTS:
	case TV_GLOVES:
	case TV_CLOAK:
	case TV_CROWN:
	case TV_HELM:
	case TV_SHIELD:
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_DRAG_ARMOR:
		{
			sprintf(dam, "%d", q_ptr->ac);
			break;
		}
	}


	/* Weight */
	sprintf(wgt, "%3d.%d", q_ptr->weight / 10, q_ptr->weight % 10);
}


/*
* Create a spoiler file for items
*/
static void spoil_obj_desc(cptr fname)
{
	FILE *fff = NULL;
	
	int i, k, s, t, n = 0;

	u16b who[200];

	char buf[1024];

	char wgt[80];
	char dam[80];


	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}


	/* Header */
	fprintf(fff, "Spoiler File -- Basic Items (2.?.?)\n\n\n");

	/* More Header */
	fprintf(fff, "%-45s     %8s%7s%5s%9s\n",
		"Description", "Dam/AC", "Wgt", "Lev", "Cost");
	fprintf(fff, "%-45s     %8s%7s%5s%9s\n",
		"----------------------------------------",
		"------", "---", "---", "----");

	/* List the groups */
	for (i = 0; TRUE; i++)
	{
		/* Write out the group title */
		if (group_item[i].name)
		{
			/* Hack -- bubble-sort by cost and then level */
			for (s = 0; s < n - 1; s++)
			{
				for (t = 0; t < n - 1; t++)
				{
					int i1 = t;
					int i2 = t + 1;

					int e1;
					int e2;

					s32b t1;
					s32b t2;

					kind_info(NULL, NULL, NULL, &e1, &t1, who[i1]);
					kind_info(NULL, NULL, NULL, &e2, &t2, who[i2]);

					if ((t1 > t2) || ((t1 == t2) && (e1 > e2)))
					{
						int tmp = who[i1];
						who[i1] = who[i2];
						who[i2] = tmp;
					}
				}
			}

			/* Spoil each item */
			for (s = 0; s < n; s++)
			{
				int e;
				s32b v;

				/* Describe the kind */
				kind_info(buf, dam, wgt, &e, &v, who[s]);

				/* Dump it */
				fprintf(fff, "     %-45s%8s%7s%5d%9ld\n",
					buf, dam, wgt, e, (long)(v));
			}

			/* Start a new set */
			n = 0;

			/* Notice the end */
			if (!group_item[i].tval) break;

			/* Start a new set */
			fprintf(fff, "\n\n%s\n\n", group_item[i].name);
		}

		/* Acquire legal item types */
		for (k = 1; k < MAX_K_IDX; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip wrong tval's */
			if (k_ptr->tval != group_item[i].tval) continue;

			/* Hack -- Skip instant-artefacts */
			if (k_ptr->flags3 & (TR3_INSTA_ART)) continue;

			/* Save the index */
			who[n++] = k;
		}
	}


	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Message */
	msg_print("Successfully created a spoiler file.");
}



/*
* Artifact Spoilers by: randy@PICARD.tamu.edu (Randy Hutson)
*/


/*
* Returns a "+" string if a number is non-negative and an empty
* string if negative
*/
#define POSITIZE(v) (((v) >= 0) ? "+" : "")

/*
* These are used to format the artefact spoiler file. INDENT1 is used
* to indent all but the first line of an artefact spoiler. INDENT2 is
* used when a line "wraps". (Bladeturner's resistances cause this.)
*/
#define INDENT1 "    "
#define INDENT2 "      "

/*
* MAX_LINE_LEN specifies when a line should wrap.
*/
#define MAX_LINE_LEN 75

/*
* The artefacts categorized by type
*/
static grouper group_artefact[] =
{
	{ TV_SWORD,      "Edged Weapons" },
	{ TV_POLEARM,    "Polearms" },
	{ TV_HAFTED,     "Hafted Weapons" },
	{ TV_BOW,        "Bows" },
	{ TV_SOFT_ARMOR, "Body Armor - Soft" },
	{ TV_HARD_ARMOR, "Body Armor - Hard" },
	{ TV_DRAG_ARMOR, "Dragon Armor" },
	{ TV_CLOAK,      "Cloaks" },
	{ TV_SHIELD,     "Shields" },
	{ TV_HELM,       "Helms" },
	{ TV_CROWN,      "Crowns" },
	{ TV_GLOVES,     "Gloves" },
	{ TV_BOOTS,      "Boots" },
	{ TV_LITE,       "Light Sources" },
	{ TV_AMULET,     "Amulets" },
	{ TV_RING,       "Rings" },
	{ 0, NULL }
};



/*
* Pair together a constant flag with a textual description.
*
* Used by both "init.c" and "wiz-spo.c".
*
* Note that it sometimes more efficient to actually make an array
* of textual names, where entry 'N' is assumed to be paired with
* the flag whose value is "1L << N", but that requires hard-coding.
*/

typedef struct flag_desc flag_desc;

struct flag_desc
{
	const u32b flag;
	const char *const desc;
};



/*
* These are used for "+3 to STR, DEX", etc. These are separate from
* the other pval affected traits to simplify the case where an object
* affects all stats.  In this case, "All stats" is used instead of
* listing each stat individually.
*/

static flag_desc stat_flags_desc[] =
{
	{ TR1_STR,        "STR" },
	{ TR1_INT,        "INT" },
	{ TR1_WIS,        "WIS" },
	{ TR1_DEX,        "DEX" },
	{ TR1_CON,        "CON" },
	{ TR1_CHA,        "CHA" }
};

/*
* Besides stats, these are the other player traits
* which may be affected by an object's pval
*/

static flag_desc pval_flags1_desc[] =
{
	{ TR1_STEALTH,    "Stealth" },
	{ TR1_SEARCH,     "Searching" },
	{ TR1_INFRA,      "Infravision" },
	{ TR1_TUNNEL,     "Tunneling" },
	{ TR1_BLOWS,      "Attacks" },
	{ TR1_SPEED,      "Speed" }
};

/*
* Slaying preferences for weapons
*/

static flag_desc slay_flags_desc[] =
{
	{ TR1_SLAY_ANIMAL,        "Animal" },
	{ TR1_SLAY_EVIL,          "Evil" },
	{ TR1_SLAY_UNDEAD,        "Undead" },
	{ TR1_SLAY_DEMON,         "Demon" },
	{ TR1_SLAY_ANGEL,         "Angel" },
	{ TR1_KILL_ANGEL,         "XAngel" },
	{ TR1_SLAY_GIANT,         "Giant" },
	{ TR1_SLAY_DRAGON,        "Dragon" },
	{ TR1_KILL_DRAGON,        "Xdragon" }
};

/*
* Elemental brands for weapons
*
* Clearly, TR1_IMPACT is a bit out of place here. To simplify
* coding, it has been included here along with the elemental
* brands. It does seem to fit in with the brands and slaying
* more than the miscellaneous section.
*/
static flag_desc brand_flags_desc[] =
{
	{ TR1_BRAND_ACID,         "Acid Brand" },
	{ TR1_BRAND_ELEC,         "Lightning Brand" },
	{ TR1_BRAND_FIRE,         "Flame Tongue" },
	{ TR1_BRAND_COLD,         "Frost Brand" },
	{ TR1_BRAND_POIS,         "Poisoned" },

	{ TR1_CHAOTIC,            "Mark of Chaos" },
	{ TR1_VAMPIRIC,           "Vampiric" },
	{ TR1_IMPACT,             "Earthquake impact on hit" },
	{ TR1_VORPAL,             "Very sharp" },
};

/*
* The 15 resistables
*/

static const flag_desc resist_flags_desc[] =
{
	{ TR2_RES_ACID,   "Acid" },
	{ TR2_RES_ELEC,   "Lightning" },
	{ TR2_RES_FIRE,   "Fire" },
	{ TR2_RES_COLD,   "Cold" },
	{ TR2_RES_POIS,   "Poison" },
	{ TR2_RES_FEAR,   "Fear"},
	{ TR2_RES_LITE,   "Light" },
	{ TR2_RES_DARK,   "Dark" },
	{ TR2_RES_BLIND,  "Blindness" },
	{ TR2_RES_CONF,   "Confusion" },
	{ TR2_RES_SOUND,  "Sound" },
	{ TR2_RES_SHARDS, "Shards" },
	{ TR2_RES_NETHER, "Nether" },
	{ TR2_RES_NEXUS,  "Nexus" },
	{ TR2_RES_CHAOS,  "Chaos" },
	{ TR2_RES_DISEN,  "Disenchantment" },
};

/*
* Elemental immunities (along with poison)
*/

static const flag_desc immune_flags_desc[] =
{
	{ TR2_IM_ACID,    "Acid" },
	{ TR2_IM_ELEC,    "Lightning" },
	{ TR2_IM_FIRE,    "Fire" },
	{ TR2_IM_COLD,    "Cold" },
};

/*
* Sustain stats -  these are given their "own" line in the
* spoiler file, mainly for simplicity
*/
static const flag_desc sustain_flags_desc[] =
{
	{ TR2_SUST_STR,   "STR" },
	{ TR2_SUST_INT,   "INT" },
	{ TR2_SUST_WIS,   "WIS" },
	{ TR2_SUST_DEX,   "DEX" },
	{ TR2_SUST_CON,   "CON" },
	{ TR2_SUST_CHA,   "CHA" },
};

/*
* Miscellaneous magic given by an object's "flags2" field
*/

static const flag_desc misc_flags2_desc[] =
{
	{ TR2_REFLECT,    "Reflection" },
	{ TR2_FREE_ACT,   "Free Action" },
	{ TR2_HOLD_LIFE,  "Hold Life" },
};

/*
* Miscellaneous magic given by an object's "flags3" field
*
* Note that cursed artefacts and objects with permanent light
* are handled "directly" -- see analyze_misc_magic()
*/

static const flag_desc misc_flags3_desc[] =
{
	{ TR3_SH_FIRE,            "Fiery Aura" },
	{ TR3_SH_ELEC,            "Electric Aura" },
	{ TR3_NO_TELE,            "Prevent Teleportation" },
	{ TR3_NO_MAGIC,           "Anti-Magic" },
	{ TR3_WRAITH,             "Wraith Form" },
	{ TR3_FEATHER,            "Levitation" },
	{ TR3_SEE_INVIS,          "See Invisible" },
	{ TR3_TELEPATHY,          "ESP" },
	{ TR3_SLOW_DIGEST,        "Slow Digestion" },
	{ TR3_REGEN,              "Regeneration" },
	{ TR3_XTRA_SHOTS,         "+1 Extra Shot" },        /* always +1? */
	{ TR3_DRAIN_EXP,          "Drains Experience" },
	{ TR3_AGGRAVATE,          "Aggravates" },
	{ TR3_BLESSED,            "Blessed Blade" },
};

/*
* A special type used just for deailing with pvals
*/

typedef struct
{
	/*
	* This will contain a string such as "+2", "-10", etc.
	*/
	char pval_desc[12];

	/*
	* A list of various player traits affected by an object's pval such
	* as stats, speed, stealth, etc.  "Extra attacks" is NOT included in
	* this list since it will probably be desirable to format its
	* description differently.
	*
	* Note that room need only be reserved for the number of stats - 1
	* since the description "All stats" is used if an object affects all
	* all stats. Also, room must be reserved for a sentinel NULL pointer.
	*
	* This will be a list such as ["STR", "DEX", "Stealth", NULL] etc.
	*
	* This list includes extra attacks, for simplicity.
	*/
	cptr pval_affects[N_ELEMENTS(stat_flags_desc) - 1 +
		N_ELEMENTS(pval_flags1_desc) + 1];

} pval_info_type;


/*
* An "object analysis structure"
*
* It will be filled with descriptive strings detailing an object's
* various magical powers. The "ignore X" traits are not noted since
* all artefacts ignore "normal" destruction.
*/

typedef struct
{
	/* "The Longsword Dragonsmiter (6d4) (+20, +25)" */
	char description[160];

	/* Description of what is affected by an object's pval */
	pval_info_type pval_info;

	/* A list of an object's slaying preferences */
	cptr slays[N_ELEMENTS(slay_flags_desc) + 1];

	/* A list if an object's elemental brands */
	cptr brands[N_ELEMENTS(brand_flags_desc) + 1];

	/* A list of immunities granted by an object */
	cptr immunities[N_ELEMENTS(immune_flags_desc) + 1];

	/* A list of resistances granted by an object */
	cptr resistances[N_ELEMENTS(resist_flags_desc) + 1];

	/* A list of stats sustained by an object */
	cptr sustains[N_ELEMENTS(sustain_flags_desc)  - 1 + 1];

	/* A list of various magical qualities an object may have */
	cptr misc_magic[N_ELEMENTS(misc_flags2_desc) + N_ELEMENTS(misc_flags3_desc)
		+ 1       /* Permanent Light */
		+ 1       /* type of curse */
		+ 1];     /* sentinel NULL */

	/* A string describing an artefact's activation */
	cptr activation;

	/* "Level 20, Rarity 30, 3.0 lbs, 20000 Gold" */
	char misc_desc[80];

} obj_desc_list;



/*
* Write out `n' of the character `c' to the spoiler file
*/
static void spoiler_out_n_chars(FILE *fff, int n, char c)
{
	while (--n >= 0) fputc(c, fff);
}

/*
* Write out `n' blank lines to the spoiler file
*/
static void spoiler_blanklines(FILE *fff, int n)
{
	spoiler_out_n_chars(fff, n, '\n');
}

/*
* Write a line to the spoiler file and then "underline" it with hypens
*/
static void spoiler_underline(FILE *fff, cptr str)
{
	fprintf(fff, "%s\n", str);
	spoiler_out_n_chars(fff, strlen(str), '-');
	fprintf(fff, "\n");
}



/*
* This function does most of the actual "analysis". Given a set of bit flags
* (which will be from one of the flags fields from the object in question),
* a "flag description structure", a "description list", and the number of
* elements in the "flag description structure", this function sets the
* "description list" members to the appropriate descriptions contained in
* the "flag description structure".
*
* The possibly updated description pointer is returned.
*/

static cptr *spoiler_flag_aux(const u32b art_flags, const flag_desc *flag_ptr,
							  cptr *desc_ptr, const int n_elmnts)
{
	int i;

	for (i = 0; i < n_elmnts; ++i)
	{
		if (art_flags & flag_ptr[i].flag)
		{
			*desc_ptr++ = flag_ptr[i].desc;
		}
	}

	return desc_ptr;
}


/*
* Acquire a "basic" description "The Cloak of Death [1,+10]"
*/
static void analyze_general (object_type *o_ptr, char *desc_ptr)
{
	/* Get a "useful" description of the object */
	object_desc_store(desc_ptr, o_ptr, TRUE, 1);
}

/*
* List "player traits" altered by an artefact's pval. These include stats,
* speed, infravision, tunneling, stealth, searching, and extra attacks.
*/

static void analyze_pval (object_type *o_ptr, pval_info_type *p_ptr)
{
	const u32b all_stats = (TR1_STR | TR1_INT | TR1_WIS |
		TR1_DEX | TR1_CON | TR1_CHA);

	u32b f1, f2, f3;

	cptr *affects_list;

	/* If pval == 0, there is nothing to do. */
	if (!o_ptr->pval)
	{
		/* An "empty" pval description indicates that pval == 0 */
		p_ptr->pval_desc[0] = '\0';
		return;
	}

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	affects_list = p_ptr->pval_affects;

	/* Create the "+N" string */
	sprintf(p_ptr->pval_desc, "%s%d", POSITIZE(o_ptr->pval), o_ptr->pval);

	/* First, check to see if the pval affects all stats */
	if ((f1 & all_stats) == all_stats)
	{
		*affects_list++ = "All stats";
	}

	/* Are any stats affected? */
	else if (f1 & all_stats)
	{
		affects_list = spoiler_flag_aux(f1, stat_flags_desc,
			affects_list,
			N_ELEMENTS(stat_flags_desc));
	}

	/* And now the "rest" */
	affects_list = spoiler_flag_aux(f1, pval_flags1_desc,
		affects_list,
		N_ELEMENTS(pval_flags1_desc));

	/* Terminate the description list */
	*affects_list = NULL;
}

/* Note the slaying specialties of a weapon */

static void analyze_slay (object_type *o_ptr, cptr *slay_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	slay_list = spoiler_flag_aux(f1, slay_flags_desc, slay_list,
		N_ELEMENTS(slay_flags_desc));

	/* Terminate the description list */
	*slay_list = NULL;
}

/* Note an object's elemental brands */

static void analyze_brand (object_type *o_ptr, cptr *brand_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	brand_list = spoiler_flag_aux(f1, brand_flags_desc, brand_list,
		N_ELEMENTS(brand_flags_desc));

	/* Terminate the description list */
	*brand_list = NULL;
}


/* Note the resistances granted by an object */

static void analyze_resist (object_type *o_ptr, cptr *resist_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	resist_list = spoiler_flag_aux(f2, resist_flags_desc,
		resist_list, N_ELEMENTS(resist_flags_desc));

	/* Terminate the description list */
	*resist_list = NULL;
}

/* Note the immunities granted by an object */

static void analyze_immune (object_type *o_ptr, cptr *immune_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	immune_list = spoiler_flag_aux(f2, immune_flags_desc,
		immune_list, N_ELEMENTS(immune_flags_desc));

	/* Terminate the description list */
	*immune_list = NULL;

}

/* Note which stats an object sustains */

static void analyze_sustains (object_type *o_ptr, cptr *sustain_list)
{
	const u32b all_sustains = (TR2_SUST_STR | TR2_SUST_INT | TR2_SUST_WIS |
		TR2_SUST_DEX | TR2_SUST_CON | TR2_SUST_CHA);

	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	/* Simplify things if an item sustains all stats */
	if ((f2 & all_sustains) == all_sustains)
	{
		*sustain_list++ = "All stats";
	}

	/* Should we bother? */
	else if ((f2 & all_sustains))
	{
		sustain_list = spoiler_flag_aux(f2, sustain_flags_desc,
			sustain_list,
			N_ELEMENTS(sustain_flags_desc));
	}

	/* Terminate the description list */
	*sustain_list = NULL;
}


/*
* Note miscellaneous powers bestowed by an artefact such as see invisible,
* free action, permanent light, etc.
*/

static void analyze_misc_magic (object_type *o_ptr, cptr *misc_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	misc_list = spoiler_flag_aux(f2, misc_flags2_desc, misc_list,
		N_ELEMENTS(misc_flags2_desc));

	misc_list = spoiler_flag_aux(f3, misc_flags3_desc, misc_list,
		N_ELEMENTS(misc_flags3_desc));

	/*
	* Artifact lights -- large radius light.
	*/
	if ((o_ptr->tval == TV_LITE) && artefact_p(o_ptr))
	{
		*misc_list++ = "Permanent Light(3)";
	}

	/*
	* Glowing artefacts -- small radius light.
	*/
	if (f3 & (TR3_LITE))
	{
		*misc_list++ = "Permanent Light(1)";
	}

	/*
	* Handle cursed objects here to avoid redundancies such as noting
	* that a permanently cursed object is heavily cursed as well as
	* being "lightly cursed".
	*/

	if (cursed_p(o_ptr))
	{
		if (f3 & (TR3_TY_CURSE))
		{
			*misc_list++ = "Ancient Curse";
		}
		if (f3 & (TR3_PERMA_CURSE))
		{
			*misc_list++ = "Permanently Cursed";
		}
		else if (f3 & (TR3_HEAVY_CURSE))
		{
			*misc_list++ = "Heavily Cursed";
		}
		else
		{
			*misc_list++ = "Cursed";
		}
	}

	/* Terminate the description list */
	*misc_list = NULL;
}




/*
* Determine the minimum depth an artefact can appear, its rarity, its weight,
* and its value in gold pieces
*/

static void analyze_misc (object_type *o_ptr, char *misc_desc)
{
	artefact_type *a_ptr = &a_info[o_ptr->name1];
	u32b f1, f2, f3;

	/* Skip this for randarts and sentient weapons */
	if( !o_ptr->name1 )
	{
		object_flags(o_ptr, &f1, &f2, &f3);
		sprintf(misc_desc, ( f3 & TR3_XP )?"Sentient Weapon":"Random Artefact" );
	}
	else
	{
		sprintf(misc_desc, "Level %u, Rarity %u, %d.%d lbs, %ld Gold",
		                   a_ptr->level, 
						   a_ptr->rarity,
			               a_ptr->weight / 10, 
						   a_ptr->weight % 10, 
						   a_ptr->cost);
	}
}

/*
* Fill in an object description structure for a given object
*/

static void object_analyze(object_type *o_ptr, obj_desc_list *desc_ptr)
{
	analyze_general(o_ptr, desc_ptr->description);

	analyze_pval(o_ptr, &desc_ptr->pval_info);

	analyze_brand(o_ptr, desc_ptr->brands);

	analyze_slay(o_ptr, desc_ptr->slays);

	analyze_immune(o_ptr, desc_ptr->immunities);

	analyze_resist(o_ptr, desc_ptr->resistances);

	analyze_sustains(o_ptr, desc_ptr->sustains);

	analyze_misc_magic(o_ptr, desc_ptr->misc_magic);

	analyze_misc(o_ptr, desc_ptr->misc_desc);

	desc_ptr->activation = item_activation(o_ptr);
}


static void print_header(FILE *fff)
{
	char buf[80];

	sprintf(buf, "Artifact Spoilers for Hellband Version %d.%d.%d",
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
	spoiler_underline(fff, buf);

}

/*
* This is somewhat ugly.
*
* Given a header ("Resist", e.g.), a list ("Fire", "Cold", Acid", e.g.),
* and a separator character (',', e.g.), write the list to the spoiler file
* in a "nice" format, such as:
*
*      Resist Fire, Cold, Acid
*
* That was a simple example, but when the list is long, a line wrap
* should occur, and this should induce a new level of indention if
* a list is being spread across lines. So for example, Bladeturner's
* list of resistances should look something like this
*
*     Resist Acid, Lightning, Fire, Cold, Poison, Light, Dark, Blindness,
*       Confusion, Sound, Shards, Nether, Nexus, Chaos, Disenchantment
*
* However, the code distinguishes between a single list of many items vs.
* many lists. (The separator is used to make this determination.) A single
* list of many items will not cause line wrapping (since there is no
* apparent reason to do so). So the lists of Ulmo's miscellaneous traits
* might look like this:
*
*     Free Action; Hold Life; See Invisible; Slow Digestion; Regeneration
*     Blessed Blade
*
* So comparing the two, "Regeneration" has no trailing separator and
* "Blessed Blade" was not indented. (Also, Ulmo's lists have no headers,
* but that's not relevant to line wrapping and indention.)
*/

/* ITEM_SEP separates items within a list */
#define ITEM_SEP ','


/* LIST_SEP separates lists */
#define LIST_SEP ';'


static void spoiler_outlist(FILE *fff, cptr header, cptr *list, char separator)
{
	int line_len, buf_len;
	char line[MAX_LINE_LEN+1], buf[80];

	/* Ignore an empty list */
	if (*list == NULL) return;

	/* This function always indents */
	strcpy(line, INDENT1);

	/* Create header (if one was given) */
	if (header && (header[0]))
	{
		strcat(line, header);
		strcat(line, " ");
	}

	line_len = strlen(line);

	/* Now begin the tedious task */
	while (1)
	{
		/* Copy the current item to a buffer */
		strcpy(buf, *list);

		/* Note the buffer's length */
		buf_len = strlen(buf);

		/*
		* If there is an item following this one, pad with separator and
		* a space and adjust the buffer length
		*/

		if (list[1])
		{
			sprintf(buf + buf_len, "%c ", separator);
			buf_len += 2;
		}

		/*
		* If the buffer will fit on the current line, just append the
		* buffer to the line and adjust the line length accordingly.
		*/

		if (line_len + buf_len <= MAX_LINE_LEN)
		{
			strcat(line, buf);
			line_len += buf_len;
		}

		/* Apply line wrapping and indention semantics described above */
		else
		{
			/*
			* Don't print a trailing list separator but do print a trailing
			* item separator.
			*/
			if (line_len > 1 && line[line_len - 1] == ' '
				&& line[line_len - 2] == LIST_SEP)
			{
				/* Ignore space and separator */
				line[line_len - 2] = '\0';

				/* Write to spoiler file */
				fprintf(fff, "%s\n", line);

				/* Begin new line at primary indention level */
				sprintf(line, "%s%s", INDENT1, buf);
			}

			else
			{
				/* Write to spoiler file */
				fprintf(fff, "%s\n", line);

				/* Begin new line at secondary indention level */
				sprintf(line, "%s%s", INDENT2, buf);
			}

			line_len = strlen(line);
		}

		/* Advance, with break */
		if (!*++list) break;
	}

	/* Write what's left to the spoiler file */
	fprintf(fff, "%s\n", line);
}


/* Create a spoiler file entry for an artefact */

static void spoiler_print_art(FILE *fff, obj_desc_list *art_ptr)
{
	pval_info_type *pval_ptr = &art_ptr->pval_info;

	char buf[80];

	/* Don't indent the first line */
	fprintf(fff, "%s\n", art_ptr->description);

	/* An "empty" pval description indicates that the pval affects nothing */
	if (pval_ptr->pval_desc[0])
	{
		/* Mention the effects of pval */
		sprintf(buf, "%s to", pval_ptr->pval_desc);
		spoiler_outlist(fff, buf, pval_ptr->pval_affects, ITEM_SEP);
	}

	/* Now deal with the description lists */

	spoiler_outlist(fff, "Slay", art_ptr->slays, ITEM_SEP);

	spoiler_outlist(fff, "", art_ptr->brands, LIST_SEP);

	spoiler_outlist(fff, "Immunity to", art_ptr->immunities, ITEM_SEP);

	spoiler_outlist(fff, "Resist", art_ptr->resistances, ITEM_SEP);

	spoiler_outlist(fff, "Sustain", art_ptr->sustains, ITEM_SEP);

	spoiler_outlist(fff, "", art_ptr->misc_magic, LIST_SEP);


	/* Write out the possible activation at the primary indention level */
	if (art_ptr->activation)
	{
		fprintf(fff, "%sActivates for %s\n", INDENT1, art_ptr->activation);
	}

	/* End with the miscellaneous facts */
	fprintf(fff, "%s%s\n\n", INDENT1, art_ptr->misc_desc);
}


/*
* Hack -- Create a "forged" artefact
*/
static bool make_fake_artefact(object_type *o_ptr, int name1)
{
	int i;

	artefact_type *a_ptr = &a_info[name1];


	/* Ignore "empty" artefacts */
	if (!a_ptr->name) return FALSE;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (FALSE);

	/* Create the artefact */
	object_prep(o_ptr, i);

	/* Save the name */
	o_ptr->name1 = name1;

	/* Extract the fields */
	o_ptr->pval = a_ptr->pval;
	o_ptr->ac = a_ptr->ac;
	o_ptr->dd = a_ptr->dd;
	o_ptr->ds = a_ptr->ds;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->to_d = a_ptr->to_d;
	o_ptr->weight = a_ptr->weight;

	/* Success */
	return (TRUE);
}

/* Write down artefact spoiler info to a given file */
void spoiler_print_analyze_art(FILE *fff, object_type *q_ptr)
{	
	obj_desc_list artefact;
	
	/* Analyze the artefact */
	object_analyze(q_ptr, &artefact);

	/* Write out the artefact description to the spoiler file */
	spoiler_print_art(fff, &artefact);
}

/*
* Create a spoiler file for artefacts
*/
static void spoil_artefact(cptr fname)
{
	int i, j;

	object_type forge;
	object_type *q_ptr;

	char buf[1024];
	FILE *fff = NULL;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */
	print_header(fff);

	/* List the artefacts by tval */
	for (i = 0; group_artefact[i].tval; i++)
	{
		/* Write out the group title */
		if (group_artefact[i].name)
		{
			spoiler_blanklines(fff, 2);
			spoiler_underline(fff, group_artefact[i].name);
			spoiler_blanklines(fff, 1);
		}

		/* Now search through all of the artefacts */
		for (j = 1; j < MAX_A_IDX; ++j)
		{
			artefact_type *a_ptr = &a_info[j];

			/* We only want objects in the current group */
			if (a_ptr->tval != group_artefact[i].tval) continue;

			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Attempt to "forge" the artefact */
			if (!make_fake_artefact(q_ptr, j)) continue;

			/* Analyze object and write it to the file */
			spoiler_print_analyze_art(fff, q_ptr);
		}
	}

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Message */
	msg_print("Successfully created a spoiler file.");
}

/*
* Create a spoiler file for monster flag usage
*/

static void spoil_mon_stats(cptr fname)
{
	int i,j, n = 0;
	
	s16b who[MAX_R_IDX];

	char buf[1024];
	FILE *fff = NULL;
	
	int per_level_count,total_count,cur_level;

	monster_race *r_ptr;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */

	fprintf(fff, "Monster Spoilers for Hellband Version %d.%d.%d\n",
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
	fprintf(fff, "------------------------------------------\n\n");

	/* Scan the monsters (except the ghost) */
	for (i = 1; i < MAX_R_IDX - 1; i++)
	{
		r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++] = i;
	}

	/* Count 'flags1' monsters of each level */
	for (j=0;j<32;j++)
	{
		total_count=0;
		per_level_count=0;
		cur_level=0;
		fprintf(fff,"%s\n\nLevel 0: ",r_info_flags1[j]);
		for (i = 0; i < n; i++)
		{
			r_ptr = &r_info[who[i]];
			if (r_ptr->level > cur_level)
			{
				fprintf(fff,"%d\nLevel %d: ",per_level_count,r_ptr->level);
				cur_level=r_ptr->level;
				per_level_count=0;
			}
			if(r_ptr->flags1 & (1L << j))
			{
				per_level_count++;
				total_count++;
			}
		}
		fprintf(fff,"%d\n\nTotal: %d\n\n\n",per_level_count,total_count);
	}
	/* Count 'flags2' monsters of each level */
	for (j=0;j<32;j++)
	{
		total_count=0;
		per_level_count=0;
		cur_level=0;
		fprintf(fff,"%s\n\nLevel 0: ",r_info_flags2[j]);
		for (i = 0; i < n; i++)
		{
			r_ptr = &r_info[who[i]];
			if (r_ptr->level > cur_level)
			{
				fprintf(fff,"%d\nLevel %d: ",per_level_count,r_ptr->level);
				cur_level=r_ptr->level;
				per_level_count=0;
			}
			if(r_ptr->flags2 & (1L << j))
			{
				per_level_count++;
				total_count++;
			}
		}
		fprintf(fff,"%d\n\nTotal: %d\n\n\n",per_level_count,total_count);
	}
	/* Count 'flags3' monsters of each level */
	for (j=0;j<32;j++)
	{
		total_count=0;
		per_level_count=0;
		cur_level=0;
		fprintf(fff,"%s\n\nLevel 0: ",r_info_flags3[j]);
		for (i = 0; i < n; i++)
		{
			r_ptr = &r_info[who[i]];
			if (r_ptr->level > cur_level)
			{
				fprintf(fff,"%d\nLevel %d: ",per_level_count,r_ptr->level);
				cur_level=r_ptr->level;
				per_level_count=0;
			}
			if(r_ptr->flags3 & (1L << j))
			{
				per_level_count++;
				total_count++;
			}
		}
		fprintf(fff,"%d\n\nTotal: %d\n\n\n",per_level_count,total_count);
	}
	/* Count 'flags4' monsters of each level */
	for (j=0;j<32;j++)
	{
		total_count=0;
		per_level_count=0;
		cur_level=0;
		fprintf(fff,"%s\n\nLevel 0: ",r_info_flags4[j]);
		for (i = 0; i < n; i++)
		{
			r_ptr = &r_info[who[i]];
			if (r_ptr->level > cur_level)
			{
				fprintf(fff,"%d\nLevel %d: ",per_level_count,r_ptr->level);
				cur_level=r_ptr->level;
				per_level_count=0;
			}
			if(r_ptr->flags4 & (1L << j))
			{
				per_level_count++;
				total_count++;
			}
		}
		fprintf(fff,"%d\n\nTotal: %d\n\n\n",per_level_count,total_count);
	}
	/* Count 'flags5' monsters of each level */
	for (j=0;j<32;j++)
	{
		total_count=0;
		per_level_count=0;
		cur_level=0;
		fprintf(fff,"%s\n\nLevel 0: ",r_info_flags5[j]);
		for (i = 0; i < n; i++)
		{
			r_ptr = &r_info[who[i]];
			if (r_ptr->level > cur_level)
			{
				fprintf(fff,"%d\nLevel %d: ",per_level_count,r_ptr->level);
				cur_level=r_ptr->level;
				per_level_count=0;
			}
			if(r_ptr->flags5 & (1L << j))
			{
				per_level_count++;
				total_count++;
			}
		}
		fprintf(fff,"%d\n\nTotal: %d\n\n\n",per_level_count,total_count);
	}
	/* Count 'flags6' monsters of each level */
	for (j=0;j<32;j++)
	{
		total_count=0;
		per_level_count=0;
		cur_level=0;
		fprintf(fff,"%s\n\nLevel 0: ",r_info_flags6[j]);
		for (i = 0; i < n; i++)
		{
			r_ptr = &r_info[who[i]];
			if (r_ptr->level > cur_level)
			{
				fprintf(fff,"%d\nLevel %d: ",per_level_count,r_ptr->level);
				cur_level=r_ptr->level;
				per_level_count=0;
			}
			if(r_ptr->flags6 & (1L << j))
			{
				per_level_count++;
				total_count++;
			}
		}
		fprintf(fff,"%d\n\nTotal: %d\n\n\n",per_level_count,total_count);
	}

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Worked */
	msg_print("Successfully created a spoiler file.");
}



/*
* Create a spoiler file for monsters   -BEN-
*/
static void spoil_mon_desc(cptr fname)
{
	int i, n = 0;

	s16b who[MAX_R_IDX];

	char buf[1024];

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	char exp[80];
	FILE *fff = NULL;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */

	fprintf(fff, "Monster Spoilers for Hellband Version %d.%d.%d\n",
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
	fprintf(fff, "------------------------------------------\n\n");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"----", "---", "---", "---", "--", "--", "-----------");


	/* Scan the monsters (except the ghost) */
	for (i = 1; i < MAX_R_IDX - 1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++] = i;
	}


	/* Scan again */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		cptr name = (r_name + r_ptr->name);

		/* Get the "name" */
		if (r_ptr->flags1 & (RF1_ALWAYS_GUARD))
		{
			sprintf(nam, "[G] %s", name);
		}
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			sprintf(nam, "[U] %s", name);
		}
		else
		{
			sprintf(nam, "The %s", name);
		}


		/* Level */
		sprintf(lev, "%d", r_ptr->level);

		/* Rarity */
		sprintf(rar, "%d", r_ptr->rarity);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			sprintf(spd, "+%d", (r_ptr->speed - 110));
		}
		else
		{
			sprintf(spd, "-%d", (110 - r_ptr->speed));
		}

		/* Armor Class */
		sprintf(ac, "%d", r_ptr->ac);

		/* Hitpoints */
		if ((r_ptr->flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			sprintf(hp, "%d", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			sprintf(hp, "%dd%d", r_ptr->hdice, r_ptr->hside);
		}


		/* Experience */
		sprintf(exp, "%ld", (long)(r_ptr->mexp));

		/* Hack -- use visual instead */
		sprintf(exp, "%s '%c'", attr_to_text(r_ptr->d_attr), r_ptr->d_char);

		/* Dump the info */
		fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			nam, lev, rar, spd, hp, ac, exp);
	}

	/* End it */
	fprintf(fff, "\n");


	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	/* Worked */
	msg_print("Successfully created a spoiler file.");
}




/*
* Monster spoilers by: smchorse@ringer.cs.utsa.edu (Shawn McHorse)
*
* Primarily based on code already in mon-desc.c, mostly by -BEN-
*/

/*
* Pronoun arrays
*/
static cptr wd_che[3] =
{ "It", "He", "She" };
static cptr wd_lhe[3] =
{ "it", "he", "she" };

/*
* Buffer text to the given file. (-SHAWN-)
* This is basically c_roff() from mon-desc.c with a few changes.
*/
static void spoil_out(FILE *fff, cptr str)
{
	cptr r;

	/* Line buffer */
	static char roff_buf[256];

	/* Current pointer into line roff_buf */
	static char *roff_p = roff_buf;

	/* Last space saved into roff_buf */
	static char *roff_s = NULL;

	/* Special handling for "new sequence" */
	if (!str)
	{
		if (roff_p != roff_buf) roff_p--;
		while (*roff_p == ' ' && roff_p != roff_buf) roff_p--;
		if (roff_p == roff_buf) fprintf(fff, "\n");
		else
		{
			*(roff_p + 1) = '\0';
			fprintf(fff, "%s\n\n", roff_buf);
		}
		roff_p = roff_buf;
		roff_s = NULL;
		roff_buf[0] = '\0';
		return;
	}

	/* Scan the given string, character at a time */
	for (; *str; str++)
	{
		char ch = *str;
		int wrap = (ch == '\n');

		if (!isprint(ch)) ch = ' ';
		if (roff_p >= roff_buf + 75) wrap = 1;
		if ((ch == ' ') && (roff_p + 2 >= roff_buf + 75)) wrap = 1;

		/* Handle line-wrap */
		if (wrap)
		{
			*roff_p = '\0';
			r = roff_p;
			if (roff_s && (ch != ' '))
			{
				*roff_s = '\0';
				r = roff_s + 1;
			}
			fprintf(fff, "%s\n", roff_buf);
			roff_s = NULL;
			roff_p = roff_buf;
			while (*r) *roff_p++ = *r++;
		}

		/* Save the char */
		if ((roff_p > roff_buf) || (ch != ' '))
		{
			if (ch == ' ') roff_s = roff_p;
			*roff_p++ = ch;
		}
	}
}


/*
* Create a spoiler file for monsters (-SHAWN-)
*/
static void spoil_mon_info(cptr fname)
{
	char buf[1024];
	int msex, vn, i, j, k, n;
	bool breath, magic, sin;
	cptr p, q;
	cptr vp[64];
	u32b flags1, flags2, flags3, flags4, flags5, flags6, flags7;
	FILE *fff = NULL;

	/* Build the filename */
	path_build(buf, 1024, ANGBAND_DIR_INFO, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msg_print("Cannot create spoiler file.");
		return;
	}


	/* Dump the header */
	sprintf(buf, "Monster Spoilers for Hellband Version %d.%d.%d\n",
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);

	spoil_out(fff, buf);
	spoil_out(fff, "------------------------------------------\n\n");

	/*
	* List all monsters in order (except the ghost).
	*/
	for (n = 1; n < MAX_R_IDX - 1; n++)
	{
		monster_race *r_ptr = &r_info[n];

		/* Extract the flags */
		flags1 = r_ptr->flags1;
		flags2 = r_ptr->flags2;
		flags3 = r_ptr->flags3;
		flags4 = r_ptr->flags4;
		flags5 = r_ptr->flags5;
		flags6 = r_ptr->flags6;
		flags7 = r_ptr->flags7;
		breath = FALSE;
		magic = FALSE;

		/* Extract a gender (if applicable) */
		if (flags1 & (RF1_FEMALE)) msex = 2;
		else if (flags1 & (RF1_MALE)) msex = 1;
		else msex = 0;


		/* Prefix */
		if (flags1 & (RF1_ALWAYS_GUARD))
		{
			spoil_out(fff, "[G] ");
		}
		else if (flags1 & (RF1_UNIQUE))
		{
			spoil_out(fff, "[U] ");
		}
		else
		{
			spoil_out(fff, "The ");
		}

		/* Name */
		sprintf(buf, "%s  (", (r_name + r_ptr->name));  /* ---)--- */
		spoil_out(fff, buf);

		/* Color */
		spoil_out(fff, attr_to_text(r_ptr->d_attr));

		/* Symbol --(-- */
		sprintf(buf, " '%c')\n", r_ptr->d_char);
		spoil_out(fff, buf);


		/* Indent */
		sprintf(buf, "=== ");
		spoil_out(fff, buf);

		/* Number */
		sprintf(buf, "Num:%d  ", n);
		spoil_out(fff, buf);

		/* Level */
		sprintf(buf, "Lev:%d  ", r_ptr->level);
		spoil_out(fff, buf);

		/* Rarity */
		sprintf(buf, "Rar:%d  ", r_ptr->rarity);
		spoil_out(fff, buf);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			sprintf(buf, "Spd:+%d  ", (r_ptr->speed - 110));
		}
		else
		{
			sprintf(buf, "Spd:-%d  ", (110 - r_ptr->speed));
		}
		spoil_out(fff, buf);

		/* Hitpoints */
		if ((flags1 & (RF1_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			sprintf(buf, "Hp:%d  ", r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			sprintf(buf, "Hp:%dd%d  ", r_ptr->hdice, r_ptr->hside);
		}
		spoil_out(fff, buf);

		/* Armor Class */
		sprintf(buf, "Ac:%d  ", r_ptr->ac);
		spoil_out(fff, buf);

		/* Experience */
		sprintf(buf, "Exp:%ld\n", (long)(r_ptr->mexp));
		spoil_out(fff, buf);


		/* Describe */
		spoil_out(fff, r_text + r_ptr->text);
		spoil_out(fff, "  ");


		spoil_out(fff, "This");

		if (flags3 & (RF3_ANIMAL)) spoil_out(fff, " natural");
		if (flags3 & (RF3_EVIL)) spoil_out(fff, " evil");
		if (flags3 & (RF3_GOOD)) spoil_out(fff, " good");
		if (flags3 & (RF3_UNDEAD)) spoil_out(fff, " undead");
		if (flags3 & (RF3_DEVIL)) spoil_out(fff, " infernal");

		if (flags3 & (RF3_DRAGON)) spoil_out(fff, " dragon");
		else if (flags3 & (RF3_DEMON)) spoil_out(fff, " demon");
		else if (flags3 & (RF3_GIANT)) spoil_out(fff, " giant");
		else if (flags3 & (RF3_TROLL)) spoil_out(fff, " troll");
		else if (flags3 & (RF3_ORC)) spoil_out(fff, " orc");
		else if (flags3 & (RF3_FALLEN_ANGEL)) spoil_out(fff, " fallen angel");
		else spoil_out(fff, " creature");

		spoil_out(fff, " moves");

		if ((flags1 & (RF1_RAND_50)) && (flags1 & (RF1_RAND_25)))
		{
			spoil_out(fff, " extremely erratically");
		}
		else if (flags1 & (RF1_RAND_50))
		{
			spoil_out(fff, " somewhat erratically");
		}
		else if (flags1 & (RF1_RAND_25))
		{
			spoil_out(fff, " a bit erratically");
		}
		else
		{
			spoil_out(fff, " normally");
		}

		if (flags1 & (RF1_NEVER_MOVE))
		{
			spoil_out(fff, ", but does not deign to chase intruders");
		}

		spoil_out(fff, ".  ");




		if (flags1 & (RF1_FORCE_SLEEP))
		{
			sprintf(buf, "%s is always created sluggish.  ", wd_che[msex]);
			spoil_out(fff, buf);
		}

		if (flags2 & (RF2_AURA_FIRE))
		{
			sprintf(buf, "%s is surrounded in flames.  ", wd_che[msex]);
			spoil_out(fff, buf);
		}

		if (flags2 & (RF2_AURA_ELEC))
		{
			sprintf(buf, "%s is surrounded in electricity.  ", wd_che[msex]);
			spoil_out(fff, buf);
		}

		if (flags2 & (RF2_REFLECTING))
		{
			sprintf(buf, "%s reflects bolt spells.  ", wd_che[msex]);
			spoil_out(fff, buf);
		}

		if (flags1 & (RF1_ESCORT))
		{
			sprintf(buf, "%s usually appears with ", wd_che[msex]);
			spoil_out(fff, buf);
			if (flags1 & (RF1_ESCORTS)) spoil_out(fff, "escorts.  ");
			else spoil_out(fff, "an escort.  ");
		}

		if ((flags1 & (RF1_FRIEND)) || (flags1 & (RF1_FRIENDS)))
		{
			sprintf(buf, "%s usually appears in groups.  ", wd_che[msex]);
			spoil_out(fff, buf);
		}


		/* Collect inate attacks */
		vn = 0;
		if (flags4 & (RF4_SHRIEK)) vp[vn++]  = "shriek for help";
		if (flags4 & (RF4_CUSTOM)) vp[vn++]  = "be the only of its kind";
		if (flags4 & (RF4_XXX3)) vp[vn++]    = "do something";
		if (flags4 & (RF4_SHARD)) vp[vn++]   = "produce shard balls";
		if (flags4 & (RF4_ARROW_1)) vp[vn++] = "fire arrows";
		if (flags4 & (RF4_ARROW_2)) vp[vn++] = "fire arrows";
		if (flags4 & (RF4_ARROW_3)) vp[vn++] = "fire missiles";
		if (flags4 & (RF4_ARROW_4)) vp[vn++] = "fire missiles";

		if (vn)
		{
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " may ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " or ");
				spoil_out(fff, vp[i]);
			}
			spoil_out(fff, ".  ");
		}

		/* Collect breaths */
		vn = 0;
		if (flags4 & (RF4_BR_ACID)) vp[vn++] = "acid";
		if (flags4 & (RF4_BR_ELEC)) vp[vn++] = "lightning";
		if (flags4 & (RF4_BR_FIRE)) vp[vn++] = "fire";
		if (flags4 & (RF4_BR_COLD)) vp[vn++] = "frost";
		if (flags4 & (RF4_BR_POIS)) vp[vn++] = "poison";
		if (flags4 & (RF4_BR_NETH)) vp[vn++] = "nether";
		if (flags4 & (RF4_BR_LITE)) vp[vn++] = "light";
		if (flags4 & (RF4_BR_DARK)) vp[vn++] = "darkness";
		if (flags4 & (RF4_BR_CONF)) vp[vn++] = "confusion";
		if (flags4 & (RF4_BR_SOUN)) vp[vn++] = "sound";
		if (flags4 & (RF4_BR_CHAO)) vp[vn++] = "chaos";
		if (flags4 & (RF4_BR_DISE)) vp[vn++] = "disenchantment";
		if (flags4 & (RF4_BR_NEXU)) vp[vn++] = "nexus";
		if (flags4 & (RF4_BR_TIME)) vp[vn++] = "time";
		if (flags4 & (RF4_BR_INER)) vp[vn++] = "inertia";
		if (flags4 & (RF4_BR_GRAV)) vp[vn++] = "gravity";
		if (flags4 & (RF4_BR_SHAR)) vp[vn++] = "shards";
		if (flags4 & (RF4_BR_PLAS)) vp[vn++] = "plasma";
		if (flags4 & (RF4_BR_WALL)) vp[vn++] = "force";
		if (flags4 & (RF4_BR_MANA)) vp[vn++] = "mana";
		if (flags4 & (RF4_BR_SLIM)) vp[vn++] = "hellslime";
		if (flags4 & (RF4_BR_DISI)) vp[vn++] = "disintegration";

		if (vn)
		{
			breath = TRUE;
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " may breathe ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " or ");
				spoil_out(fff, vp[i]);
			}
			if (flags2 & (RF2_POWERFUL)) spoil_out(fff, " powerfully");
		}

		/* Collect spells */
		vn = 0;
		if (flags5 & (RF5_BA_ACID))           vp[vn++] = "produce acid balls";
		if (flags5 & (RF5_BA_ELEC))           vp[vn++] = "produce lightning balls";
		if (flags5 & (RF5_BA_FIRE))           vp[vn++] = "produce fire balls";
		if (flags5 & (RF5_BA_COLD))           vp[vn++] = "produce frost balls";
		if (flags5 & (RF5_BA_POIS))           vp[vn++] = "produce poison balls";
		if (flags5 & (RF5_BA_NETH))           vp[vn++] = "produce nether balls";
		if (flags5 & (RF5_BA_WATE))           vp[vn++] = "produce water balls";
		if (flags4 & (RF4_BA_SLIM))           vp[vn++] = "produce balls of hellslime";
		if (flags5 & (RF5_BA_MANA))           vp[vn++] = "produce mana storms";
		if (flags5 & (RF5_BA_DARK))           vp[vn++] = "produce darkness storms";
		if (flags4 & (RF4_BA_CHAO))           vp[vn++] = "invoke raw chaos";
		if (flags6 & (RF6_MARK_CAIN))         vp[vn++] = "invoke the mark of Cain";
		if (flags5 & (RF5_DRAIN_MANA))        vp[vn++] = "drain mana";
		if (flags5 & (RF5_MIND_BLAST))        vp[vn++] = "cause mind blasting";
		if (flags5 & (RF5_BRAIN_SMASH))       vp[vn++] = "cause brain smashing";
		if (flags5 & (RF5_CAUSE_1))           vp[vn++] = "cause light wounds and cursing";
		if (flags5 & (RF5_CAUSE_2))           vp[vn++] = "cause serious wounds and cursing";
		if (flags5 & (RF5_CAUSE_3))           vp[vn++] = "cause critical wounds and cursing";
		if (flags5 & (RF5_CAUSE_4))           vp[vn++] = "cause mortal wounds";
		if (flags5 & (RF5_BO_ACID))           vp[vn++] = "produce acid bolts";
		if (flags5 & (RF5_BO_ELEC))           vp[vn++] = "produce lightning bolts";
		if (flags5 & (RF5_BO_FIRE))           vp[vn++] = "produce fire bolts";
		if (flags5 & (RF5_BO_COLD))           vp[vn++] = "produce frost bolts";
		if (flags5 & (RF5_BO_POIS))           vp[vn++] = "produce poison bolts";
		if (flags5 & (RF5_BO_NETH))           vp[vn++] = "produce nether bolts";
		if (flags5 & (RF5_BO_WATE))           vp[vn++] = "produce water bolts";
		if (flags5 & (RF5_BO_MANA))           vp[vn++] = "produce mana bolts";
		if (flags5 & (RF5_BO_PLAS))           vp[vn++] = "produce plasma bolts";
		if (flags5 & (RF5_BO_ICEE))           vp[vn++] = "produce ice bolts";
		if (flags5 & (RF5_MISSILE))           vp[vn++] = "produce magic missiles";
		if (flags5 & (RF5_SCARE))             vp[vn++] = "terrify";
		if (flags5 & (RF5_BLIND))             vp[vn++] = "blind";
		if (flags5 & (RF5_CONF))              vp[vn++] = "confuse";
		if (flags5 & (RF5_SLOW))              vp[vn++] = "slow";
		if (flags5 & (RF5_HOLD))              vp[vn++] = "paralyze";
		if (flags6 & (RF6_HASTE))             vp[vn++] = "haste-self";
		if (flags6 & (RF6_HEAL))              vp[vn++] = "heal-self";
		if (flags6 & (RF6_XXX2))            vp[vn++] = "do something";
		if (flags6 & (RF6_BLINK))             vp[vn++] = "blink-self";
		if (flags6 & (RF6_TPORT))             vp[vn++] = "teleport-self";
		if (flags6 & (RF6_XXX3))            vp[vn++] = "do something";
		if (flags6 & (RF6_XXX4))            vp[vn++] = "do something";
		if (flags6 & (RF6_TELE_TO))           vp[vn++] = "teleport to";
		if (flags6 & (RF6_TELE_AWAY))         vp[vn++] = "teleport away";
		if (flags6 & (RF6_TELE_LEVEL))        vp[vn++] = "teleport level";
		if (flags6 & (RF6_XXX5))              vp[vn++] = "do something";
		if (flags6 & (RF6_DARKNESS))          vp[vn++] = "create darkness";
		if (flags6 & (RF6_TRAPS))             vp[vn++] = "create traps";
		if (flags6 & (RF6_FORGET))            vp[vn++] = "cause amnesia";
		if (flags6 & (RF6_XXX6))            vp[vn++] = "do something";
		if (flags6 & (RF6_S_MONSTER))         vp[vn++] = "summon a monster";
		if (flags6 & (RF6_S_MONSTERS))        vp[vn++] = "summon monsters";
		if (flags6 & (RF6_S_KIN))             vp[vn++] = "summon aid";
		if (flags6 & (RF6_S_ANT))             vp[vn++] = "summon ants";
		if (flags6 & (RF6_S_SPIDER))          vp[vn++] = "summon spiders";
		if (flags6 & (RF6_S_HOUND))           vp[vn++] = "summon hounds";
		if (flags6 & (RF6_S_HYDRA))           vp[vn++] = "summon hydras";
		if (flags6 & (RF6_S_DEVIL))           vp[vn++] = "summon a devil";
		if (flags6 & (RF6_S_DEMON))           vp[vn++] = "summon a demon";
		if (flags6 & (RF6_S_UNDEAD))          vp[vn++] = "summon an undead";
		if (flags6 & (RF6_S_DRAGON))          vp[vn++] = "summon a dragon";
		if (flags6 & (RF6_S_HI_UNDEAD))       vp[vn++] = "summon greater undead";
		if (flags6 & (RF6_S_HI_DRAGON))       vp[vn++] = "summon ancient dragons";
		if (flags6 & (RF6_S_REAVER))           vp[vn++] = "summon Black Reavers";
		if (flags6 & (RF6_S_FALLEN))          vp[vn++] = "summon fallen angels";
		if (flags6 & (RF6_S_UNIQUE))          vp[vn++] = "summon unique monsters";

		if (vn)
		{
			magic = TRUE;
			if (breath)
			{
				spoil_out(fff, ", and is also");
			}
			else
			{
				spoil_out(fff, wd_che[msex]);
				spoil_out(fff, " is");
			}
			spoil_out(fff, " magical, casting spells");
			if (flags2 & (RF2_SMART)) spoil_out(fff, " intelligently");
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " which ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " or ");
				spoil_out(fff, vp[i]);
			}
		}

		if (breath || magic)
		{
			sprintf(buf, "; 1 time in %d.  ",
				200 / (r_ptr->freq_inate + r_ptr->freq_spell));
			spoil_out(fff, buf);
		}

		/* Collect special abilities. */
		vn = 0;
		if (flags2 & (RF2_OPEN_DOOR)) vp[vn++] = "open doors";
		if (flags2 & (RF2_BASH_DOOR)) vp[vn++] = "bash down doors";
		if (flags2 & (RF2_PASS_WALL)) vp[vn++] = "pass through walls";
		if (flags2 & (RF2_KILL_WALL)) vp[vn++] = "bore through walls";
		if (flags2 & (RF2_MOVE_BODY)) vp[vn++] = "push past weaker monsters";
		if (flags2 & (RF2_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
		if (flags2 & (RF2_TAKE_ITEM)) vp[vn++] = "pick up objects";
		if (flags2 & (RF2_KILL_ITEM)) vp[vn++] = "destroy objects";
		if (flags7 & (RF7_AQUATIC)) vp[vn++] = "swim";
		if (flags7 & (RF7_FLIGHT)) vp[vn++] = "fly";					

		if (vn)
		{
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " can ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " and ");
				spoil_out(fff, vp[i]);
			}
			spoil_out(fff, ".  ");
		}

		if (flags2 & (RF2_INVISIBLE))
		{
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " is invisible.  ");
		}
		if (flags2 & (RF2_COLD_BLOOD))
		{
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " is cold blooded.  ");
		}
		if (flags2 & (RF2_EMPTY_MIND))
		{
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " is not detected by telepathy.  ");
		}
		if (flags2 & (RF2_WEIRD_MIND))
		{
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " is rarely detected by telepathy.  ");
		}
		if (flags2 & (RF2_MULTIPLY))
		{
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " breeds explosively.  ");
		}
		if (flags2 & (RF2_REGENERATE))
		{
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " regenerates quickly.  ");
		}

		/* Collect susceptibilities */
		vn = 0;
		if (flags3 & (RF3_HURT_ROCK)) vp[vn++] = "rock remover";
		if (flags3 & (RF3_HURT_LITE)) vp[vn++] = "bright light";
		if (flags3 & (RF3_HURT_FIRE)) vp[vn++] = "fire";
		if (flags3 & (RF3_HURT_COLD)) vp[vn++] = "cold";

		if (vn)
		{
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " is hurt by ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " and ");
				spoil_out(fff, vp[i]);
			}
			spoil_out(fff, ".  ");
		}

		/* Collect immunities */
		vn = 0;
				if (flags3 & (RF3_IM_ACID)) vp[vn++] = "acid";
				if (flags3 & (RF3_IM_ELEC)) vp[vn++] = "lightning";
				if (flags3 & (RF3_IM_FIRE)) vp[vn++] = "fire";
				if (flags3 & (RF3_IM_COLD)) vp[vn++] = "cold";
				if (flags3 & (RF3_IM_POIS)) vp[vn++] = "poison";
				if (flags7 & (RF7_IM_LITE)) vp[vn++] = "light";	
				if (flags7 & (RF7_IM_DARK)) vp[vn++] = "darkness";

		if (vn)
		{
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " resists ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " and ");
				spoil_out(fff, vp[i]);
			}
			spoil_out(fff, ".  ");
		}

		/* Collect resistances */
		vn = 0;
				if (flags7 & (RF7_RES_FIRE)) vp[vn++] = "fire";
				if (flags7 & (RF7_RES_COLD)) vp[vn++] = "cold";	
				if (flags7 & (RF7_RES_ELEC)) vp[vn++] = "electricity";
				if (flags7 & (RF7_RES_ACID)) vp[vn++] = "acid";
				if (flags7 & (RF7_RES_POIS)) vp[vn++] = "poison";	
				if (flags7 & (RF7_RES_LITE)) vp[vn++] = "light";
				if (flags7 & (RF7_RES_DARK)) vp[vn++] = "darkness";	
				if (flags3 & (RF3_RES_NETH)) vp[vn++] = "nether";
				if (flags3 & (RF3_RES_WATE)) vp[vn++] = "water";
				if (flags3 & (RF3_RES_PLAS)) vp[vn++] = "plasma";
				if (flags3 & (RF3_RES_NEXU)) vp[vn++] = "nexus";
				if (flags3 & (RF3_RES_DISE)) vp[vn++] = "disenchantment";
				if (flags3 & (RF3_RES_TELE)) vp[vn++] = "teleportation";
		if (vn)
		{
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " resists ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " and ");
				spoil_out(fff, vp[i]);
			}
			spoil_out(fff, ".  ");
		}
				
				
		/* Collect healing elements */
		vn = 0;
		if (flags7 & (RF7_HEAL_FIRE)) vp[vn++] = "fire";
		if (flags7 & (RF7_HEAL_COLD)) vp[vn++] = "cold";	
		if (flags7 & (RF7_HEAL_ELEC)) vp[vn++] = "electricity";
		if (flags7 & (RF7_HEAL_LITE)) vp[vn++] = "light";
		if (flags7 & (RF7_HEAL_DARK)) vp[vn++] = "darkness";	
		if (flags7 & (RF7_HEAL_NETH)) vp[vn++] = "nether";
		
		/* Describe resistances */
		if (vn)
		{

			/* Scan */
			for (n = 0; n < vn; n++)
			{
				/* Intro */
				if (n == 0) roff(" is healed by ");
				else if (n < vn-1) roff(", ");
				else roff(" and ");
				
				/* Dump */
				roff(vp[n]);
			}
			
			/* End */
			roff(".  ");
		}

		/* Collect non-effects */
		vn = 0;
		if (flags3 & (RF3_NO_STUN)) vp[vn++] = "stunned";
		if (flags3 & (RF3_NO_FEAR)) vp[vn++] = "frightened";
		if (flags3 & (RF3_NO_CONF)) vp[vn++] = "confused";
		if (flags3 & (RF3_NO_SLEEP)) vp[vn++] = "slept";

		if (vn)
		{
			spoil_out(fff, wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(fff, " cannot be ");
				else if (i < vn-1) spoil_out(fff, ", ");
				else spoil_out(fff, " or ");
				spoil_out(fff, vp[i]);
			}
			spoil_out(fff, ".  ");
		}

		spoil_out(fff, wd_che[msex]);
		if (r_ptr->sleep > 200)     spoil_out(fff, " prefers to ignore");
		else if (r_ptr->sleep > 95) spoil_out(fff, " pays very little attention to");
		else if (r_ptr->sleep > 75) spoil_out(fff, " pays little attention to");
		else if (r_ptr->sleep > 45) spoil_out(fff, " tends to overlook");
		else if (r_ptr->sleep > 25) spoil_out(fff, " takes quite a while to see");
		else if (r_ptr->sleep > 10) spoil_out(fff, " takes a while to see");
		else if (r_ptr->sleep > 5)  spoil_out(fff, " is fairly observant of");
		else if (r_ptr->sleep > 3)  spoil_out(fff, " is observant of");
		else if (r_ptr->sleep > 1)  spoil_out(fff, " is very observant of");
		else if (r_ptr->sleep > 0)  spoil_out(fff, " is vigilant for");
		else spoil_out(fff, " is ever vigilant for");

		sprintf(buf, " intruders, which %s may notice from %d feet.  ",
			wd_lhe[msex], 10 * r_ptr->aaf);
		spoil_out(fff, buf);

		i = 0;
		if (flags1 & (RF1_DROP_60)) i += 1;
		if (flags1 & (RF1_DROP_90)) i += 2;
		if (flags1 & (RF1_DROP_1D2)) i += 2;
		if (flags1 & (RF1_DROP_2D2)) i += 4;
		if (flags1 & (RF1_DROP_3D2)) i += 6;
		if (flags1 & (RF1_DROP_4D2)) i += 8;

		/* Drops gold and/or items */
		if (i)
		{
			sin = FALSE;
			spoil_out(fff, wd_che[msex]);
			spoil_out(fff, " will carry");

			if (i == 1)
			{
				spoil_out(fff, " a"); sin = TRUE;
			}
			else if (i == 2)
			{
				spoil_out(fff, " one or two");
				sin = TRUE;
			}
			else
			{
				sprintf(buf, " up to %u", i);
				spoil_out(fff, buf);
			}

			if (flags1 & (RF1_DROP_GREAT))
			{
				if (sin) spoil_out(fff, "n");
				spoil_out(fff, " exceptional object");
			}
			else if (flags1 & (RF1_DROP_GOOD))
			{
				spoil_out(fff, " good object");
			}
			else if (flags1 & (RF1_DROP_USEFUL))
			{
				spoil_out(fff, " useful object");
			}
			else if (flags1 & (RF1_ONLY_ITEM))
			{
				spoil_out(fff, " object");
			}
			else if (flags1 & (RF1_ONLY_GOLD))
			{
				spoil_out(fff, " treasure");
			}
			else
			{
				if (sin) spoil_out(fff, "n");
				spoil_out(fff, " object");
				if (i > 1) spoil_out(fff, "s");
				spoil_out(fff, " or treasure");
			}
			if (i > 1) spoil_out(fff, "s");

			if (flags1 & (RF1_DROP_CHOSEN))
			{
				spoil_out(fff, ", in addition to chosen objects");
			}

			spoil_out(fff, ".  ");
		}

		/* Count the actual attacks */
		for (i = 0, j = 0; j < 4; j++)
		{
			if (r_ptr->blow[j].method) i++;
		}

		/* Examine the actual attacks */
		for (k = 0, j = 0; j < 4; j++)
		{
			if (!r_ptr->blow[j].method) continue;

			/* No method yet */
			p = "???";

			/* Acquire the method */
			switch (r_ptr->blow[j].method)
			{
			case RBM_HIT:   p = "hit"; break;
			case RBM_TOUCH: p = "touch"; break;
			case RBM_PUNCH: p = "punch"; break;
			case RBM_KICK:  p = "kick"; break;
			case RBM_CLAW:  p = "claw"; break;
			case RBM_BITE:  p = "bite"; break;
			case RBM_STING: p = "sting"; break;
			case RBM_SLASH:	p = "slash"; break;
			case RBM_BUTT:  p = "butt"; break;
			case RBM_CRUSH: p = "crush"; break;
			case RBM_ENGULF:        p = "engulf"; break;
			case RBM_CHARGE: p = "charge";  break;
			case RBM_CRAWL: p = "crawl on you"; break;
			case RBM_DROOL: p = "drool on you"; break;
			case RBM_SPIT:  p = "spit"; break;
			case RBM_XXX3:  break;
			case RBM_GAZE:  p = "gaze"; break;
			case RBM_WAIL:  p = "wail"; break;
			case RBM_SPORE: p = "release spores"; break;
			case RBM_WORSHIP:  p = "hero worship";break;
			case RBM_BEG:   p = "beg"; break;
			case RBM_INSULT:        p = "insult"; break;
			case RBM_MOAN:  p = "moan"; break;
			case RBM_SHOW:  p = "sing"; break;
			case RBM_SWIPE: p = "swipe"; break;
			}


			/* Default effect */
			q = "???";

			/* Acquire the effect */
			switch (r_ptr->blow[j].effect)
			{
			case RBE_HURT:  q = "attack"; break;
			case RBE_POISON:        q = "poison"; break;
			case RBE_UN_BONUS:      q = "disenchant"; break;
			case RBE_UN_POWER:      q = "drain charges"; break;
			case RBE_EAT_GOLD:      q = "steal gold"; break;
			case RBE_EAT_ITEM:      q = "steal items"; break;
			case RBE_EAT_FOOD:      q = "eat your food"; break;
			case RBE_EAT_LITE:      q = "absorb light"; break;
			case RBE_ACID:  q = "shoot acid"; break;
			case RBE_ELEC:  q = "electrocute"; break;
			case RBE_FIRE:  q = "burn"; break;
			case RBE_COLD:  q = "freeze"; break;
			case RBE_BLIND: q = "blind"; break;
			case RBE_CONFUSE:       q = "confuse"; break;
			case RBE_TERRIFY:       q = "terrify"; break;
			case RBE_PARALYZE:      q = "paralyze"; break;
			case RBE_LOSE_STR:      q = "reduce strength"; break;
			case RBE_LOSE_INT:      q = "reduce intelligence"; break;
			case RBE_LOSE_WIS:      q = "reduce wisdom"; break;
			case RBE_LOSE_DEX:      q = "reduce dexterity"; break;
			case RBE_LOSE_CON:      q = "reduce constitution"; break;
			case RBE_LOSE_CHA:      q = "reduce charisma"; break;
			case RBE_LOSE_ALL:      q = "reduce all stats"; break;
			case RBE_SHATTER:       q = "shatter"; break;
			case RBE_EXP_10:        q = "lower experience (by 10d6+)"; break;
			case RBE_EXP_20:        q = "lower experience (by 20d6+)"; break;
			case RBE_EXP_40:        q = "lower experience (by 40d6+)"; break;
			case RBE_EXP_80:        q = "lower experience (by 80d6+)"; break;
			}


			if (!k)
			{
				spoil_out(fff, wd_che[msex]);
				spoil_out(fff, " can ");
			}
			else if (k < i-1)
			{
				spoil_out(fff, ", ");
			}
			else
			{
				spoil_out(fff, ", and ");
			}

			/* Describe the method */
			spoil_out(fff, p);

			/* Describe the effect, if any */
			if (r_ptr->blow[j].effect)
			{
				spoil_out(fff, " to ");
				spoil_out(fff, q);
				if (r_ptr->blow[j].d_dice && r_ptr->blow[j].d_side)
				{
					spoil_out(fff, " with damage");
					if (r_ptr->blow[j].d_side == 1)
						sprintf(buf, " %d", r_ptr->blow[j].d_dice);
					else
						sprintf(buf, " %dd%d",
						r_ptr->blow[j].d_dice, r_ptr->blow[j].d_side);
					spoil_out(fff, buf);
				}
			}

			k++;
		}

		if (k)
		{
			/* Now mention attack speed */
			switch (k) /* Use correct grammar */
			{
			case 1:
				{
					spoil_out(fff, format(", which %s does",wd_lhe[msex]));
					break;
				}
			case 2:
				{
					spoil_out(fff, format(" - both of which %s does",wd_lhe[msex]));
					break;
				}
			default:
				{
					spoil_out(fff, format(" - all of which %s does",wd_lhe[msex]));
					break;
				}
			}

			if(r_ptr->num_blows > 8)
			{
				spoil_out(fff, " with incredible speed");
			}
			else if(r_ptr->num_blows > 5)
			{
				spoil_out(fff, " extremely quickly");
			}
			else if(r_ptr->num_blows > 3)
			{
				spoil_out(fff, " very quickly");
			}
			else if(r_ptr->num_blows > 2)
			{
				spoil_out(fff, " quickly");
			}
			else if(r_ptr->num_blows > 1)
			{
				spoil_out(fff, " at a moderate speed");
			}
			else
			{
				spoil_out(fff, " slowly");
			}
			spoil_out(fff, ".  ");
		}
		else if (flags1 & (RF1_NEVER_BLOW))
		{
			sprintf(buf, "%s has no physical attacks.  ", wd_che[msex]);
			spoil_out(fff, buf);
		}
				
		/* Notice monsters that _have_ to be avoided */
		if (flags7 & (RF7_ANNOYED))
		{
			sprintf(buf , "%s is quickly annoyed.", wd_che[msex]);
		}
		/* Notice monsters that are in the grip of the Lover's Storm */
		if (flags7 & (RF7_STORM))
		{
			sprintf(buf, "%s is blown about to and fro by a violent storm.", wd_che[msex]);
		}	 				
        /* Notice monsters that never die permanently */
        if (flags7 & (RF7_REBORN))
        {
            roff( format("%^s recovers from fatal wounds.", wd_che[msex]));
        }	                

		spoil_out(fff, NULL);
	}

	/* Check for errors */
	if (ferror(fff) || my_fclose(fff))
	{
		msg_print("Cannot close spoiler file.");
		return;
	}

	msg_print("Successfully created a spoiler file.");
}






/*
* Forward declare
*/
extern void do_cmd_spoilers(void);

/*
* Create Spoiler files         -BEN-
*/
void do_cmd_spoilers(void)
{
	int i;


	/* Enter "icky" mode */
	character_icky = TRUE;

	/* Save the screen */
	Term_save();


	/* Drop priv's */
	safe_setuid_drop();


	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Info */
		prt("Create a spoiler file.", 2, 0);

		/* Prompt for a file */
		prt("(1) Brief Object Info (obj-desc.txt)", 5, 5);
		prt("(2) Brief Artifact Info (artefact.txt)", 6, 5);
		prt("(3) Brief Monster Info (mon-desc.txt)", 7, 5);
		prt("(4) Full Monster Info (mon-info.txt)", 8, 5);
		prt("(5) Monster statistics (mon-stat.txt)",9,5);

		/* Prompt */
		prt("Command: ", 13, 0);

		/* Get a choice */
		i = inkey();

		/* Escape */
		if (i == ESCAPE)
		{
			break;
		}

		/* Option (1) */
		else if (i == '1')
		{
			spoil_obj_desc("obj-desc.txt");
		}

		/* Option (2) */
		else if (i == '2')
		{
			spoil_artefact("artefact.txt");
		}

		/* Option (3) */
		else if (i == '3')
		{
			spoil_mon_desc("mon-desc.txt");
		}

		/* Option (4) */
		else if (i == '4')
		{
			spoil_mon_info("mon-info.txt");
		}

		/* Option (5) */
		else if (i == '5')
		{
			spoil_mon_stats("mon-stats.txt");
		}

		/* Oops */
		else
		{
			bell();
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Grab priv's */
	safe_setuid_grab();


	/* Restore the screen */
	Term_load();

	/* Leave "icky" mode */
	character_icky = FALSE;
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


