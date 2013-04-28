/* File: wizard1.c */

/*
 * Generation of object, artifact, and monster spoilers.
 *
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


#ifdef ALLOW_SPOILERS


/*
 * The spoiler file being created
 */
static FILE *fff = NULL;




/*
 * Write out `n' of the character `c' to the spoiler file
 */
static void spoiler_out_n_chars(int n, char c)
{
	while (--n >= 0) fputc(c, fff);
}

/*
 * Write out `n' blank lines to the spoiler file
 */
static void spoiler_blanklines(int n)
{
	spoiler_out_n_chars(n, '\n');
}

/*
 * Write a line to the spoiler file and then "underline" it with hyphens
 */
static void spoiler_underline(cptr str)
{
	text_out(str);
	text_out("\n");
	spoiler_out_n_chars(strlen(str), '-');
	text_out("\n");
}

/*
 * Print consistent headers for spoiler files
 */
static void print_header(cptr spoiler_type)
{
	char buf[80];

	sprintf(buf, "%s Spoilers for %s %d.%d.%d",
		spoiler_type, VERSION_NAME,
		VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH);
	spoiler_underline(buf);
}


/*
 * Extract a textual representation of an attribute
 */
static cptr attr_to_text(byte a)
{
	switch (a)
	{
		case TERM_DARK:    return ("Black");
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
 * Item Spoilers by Ben Harrison (benh@phial.com)
 */


/*
 * The basic items categorized by type
 */
static grouper group_item[] =
{
	{ TV_SHOT,       "Ammo" },
	{ TV_ARROW,      NULL },
	{ TV_BOLT,       NULL },

	{ TV_BOW,        "Missile Launchers" },

	{ TV_SWORD,      "Melee Weapons" },
	{ TV_POLEARM,    NULL },
	{ TV_HAFTED,     NULL },
	{ TV_DIGGING,    NULL },

	{ TV_SOFT_ARMOR, "Armour (Body)" },
	{ TV_HARD_ARMOR, NULL },
	{ TV_DRAG_ARMOR, NULL },

	{ TV_CLOAK,      "Armour (Misc)" },
	{ TV_SHIELD,     NULL },
	{ TV_HELM,       NULL },
	{ TV_CROWN,      NULL },
	{ TV_GLOVES,     NULL },
	{ TV_BOOTS,      NULL },

	{ TV_AMULET,     "Amulets" },
	{ TV_RING,       "Rings" },



	{ TV_SCROLL,     "Scrolls" },
	{ TV_POTION,     "Potions" },
	{ TV_FOOD,       "Food and Mushrooms" },

	{ TV_ROD,        "Rods" },
	{ TV_WAND,       "Wands" },
	{ TV_STAFF,      "Staffs" },

	{ TV_MAGIC_BOOK, "Books (Mage)" },
	{ TV_PRAYER_BOOK,"Books (Priest)" },
	{ TV_NATURE_BOOK,"Stones (Druid)" },
	{ TV_DARK_BOOK,  "Books (Necro)" },

	{ TV_CHEST,      "Chests" },

	{ TV_COMPONENT,  "Item-creation" },
	{ TV_PARCHMENT,  NULL },
	{ TV_BOTTLE,     NULL },
	{ TV_ESSENCE,    NULL },

	{ TV_SPIKE,      "Various" },
	{ TV_LITE,       NULL },
	{ TV_FLASK,      NULL },
	{ TV_JUNK,       NULL },
	{ TV_SKELETON,   NULL },
	{ TV_POUCH,      NULL },
	{ TV_GOLD,       NULL },

	{ 0, "" }
};



/*
 * Describe the kind
 */
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val, int k)
{
	object_kind *k_ptr;

	object_type *i_ptr;
	object_type object_type_body;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Prepare a fake item */
	object_prep(i_ptr, k);

	/* Obtain the "kind" info */
	k_ptr = &k_info[i_ptr->k_idx];

	/* It is known */
	i_ptr->ident |= (IDENT_KNOWN);

	/* Cancel bonuses */
	i_ptr->pval = 0;
	i_ptr->to_a = 0;
	i_ptr->to_h = 0;
	i_ptr->to_d = 0;


	/* Level */
	(*lev) = k_ptr->level;

	/* Value */
	(*val) = object_value(i_ptr);


	/* Hack */
	if (!buf || !dam || !wgt) return;


	/* Description (too brief) */
	object_desc_store(buf, i_ptr, FALSE, 0);


	/* Misc info */
	strcpy(dam, "");

	/* Damage */
	switch (i_ptr->tval)
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
			sprintf(dam, "%dd%d", i_ptr->dd, i_ptr->ds);
			break;
		}

		/* Weapons */
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			sprintf(dam, "%dd%d", i_ptr->dd, i_ptr->ds);
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
			sprintf(dam, "%d", i_ptr->ac);
			break;
		}
	}


	/* Weight */
	if (use_metric) sprintf(wgt, "%3d.%d", make_metric(i_ptr->weight) / 10,
		make_metric(i_ptr->weight) % 10);
	else sprintf(wgt, "%3d.%d", i_ptr->weight / 10, i_ptr->weight % 10);

}


/*
 * Create a spoiler file for items
 */
static void spoil_obj_desc(cptr fname)
{
	int i, k, s, t, n = 0;

	u16b who[200];

	char buf[1024];

	char wgt[80];
	char dam[80];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

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

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Object");

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
		for (k = 1; k < z_info->k_max; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip wrong tval's */
			if (k_ptr->tval != group_item[i].tval) continue;

			/* Hack -- Skip instant-artifacts */
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
 * These are used to format the artifact spoiler file. INDENT1 is used
 * to indent all but the first line of an artifact spoiler. INDENT2 is
 * used when a line "wraps". (Bladeturner's resistances cause this.)
 */
#define INDENT1 "    "
#define INDENT2 "      "

/*
 * MAX_LINE_LEN specifies when a line should wrap.
 */
#define MAX_LINE_LEN 75

/*
 * The artifacts categorized by type
 */
static grouper group_artifact[] =
{
	{ TV_SWORD,		"Edged Weapons" },
	{ TV_POLEARM,	"Polearms" },
	{ TV_HAFTED,	"Hafted (blunt) Weapons" },
	{ TV_BOW,		"Missile Launchers" },
	{ TV_SHOT,		"Ammunition - Shots" },
	{ TV_ARROW,		"Ammunition - Arrows" },
	{ TV_BOLT,		"Ammunition - Bolts" },

	{ TV_SOFT_ARMOR,	"Body Armor" },
	{ TV_HARD_ARMOR,	  NULL },
	{ TV_DRAG_ARMOR,	  NULL },

	{ TV_CLOAK,		"Cloaks" },
	{ TV_SHIELD,	"Shields" },
	{ TV_HELM,		"Helms/Crowns" },
	{ TV_CROWN,		  NULL },
	{ TV_GLOVES,	"Gloves" },
	{ TV_BOOTS,		"Boots" },

	{ TV_LITE,		"Light Sources" },
	{ TV_AMULET,	"Amulets" },
	{ TV_RING,		"Rings" },

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
	{ TR_PVAL_STR,	        "STR" },
	{ TR_PVAL_INT,	        "INT" },
	{ TR_PVAL_WIS,	        "WIS" },
	{ TR_PVAL_DEX,	        "DEX" },
	{ TR_PVAL_CON,	        "CON" },
	{ TR_PVAL_CHR,	        "CHR" }
};

/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */

static flag_desc pval_flags_desc[] =
{
	{ TR_PVAL_STEALTH,     "Stealth" },
	{ TR_PVAL_AWARE,       "Awareness" },
	{ TR_PVAL_INFRA,       "Infravision" },
	{ TR_PVAL_TUNNEL,      "Tunneling" },
	{ TR_PVAL_SPEED,       "Speed" },
	{ TR_PVAL_INVIS,       "Invisibility" },
	{ TR_PVAL_DISARM,      "Disarming" },
	{ TR_PVAL_DEVICE,      "Magical Device Skill" },
	{ TR_PVAL_SAVE,        "Saving Throw" },
	{ TR_PVAL_MANA,        "Mana" },

	{ TR_PVAL_BLOWS,       "Blows" },
	{ TR_PVAL_SHOTS,       "Shots" },
	{ TR_PVAL_MIGHT,       "Missile Multiplier" }
};


/*
 * Slaying preferences for weapons
 */

static flag_desc slay_flags_desc[] =
{
	{ TR1_SLAY_ANIMAL,     "Animal" },
	{ TR1_SLAY_EVIL,       "Evil" },
	{ TR1_SLAY_UNDEAD,     "Undead" },
	{ TR1_SLAY_DEMON,      "Demon" },
	{ TR1_SLAY_ORC,        "Orc" },
	{ TR1_SLAY_TROLL,      "Troll" },
	{ TR1_SLAY_GIANT,      "Giant" },
	{ TR1_SLAY_DRAGON,     "Dragon" },
	{ TR1_KILL_DRAGON,     "XDragon" }
};

/*
 * Elemental brands for weapons
 */
static flag_desc brand_flags_desc[] =
{
	{ TR1_BRAND_ACID,      "Acid Brand" },
	{ TR1_BRAND_ELEC,      "Lightning Brand" },
	{ TR1_BRAND_FIRE,      "Flame Tongue" },
	{ TR1_BRAND_COLD,      "Frost Brand" },
	{ TR1_BRAND_POIS,      "Poison Brand" },
	{ TR1_BRAND_FLAME,     "XFire Brand" },
	{ TR1_BRAND_VENOM,     "XPoison Brand" },
	{ TR1_VORPAL,          "Vorpal" },
	{ TR1_PERFECT_BALANCE, "Well-Balanced" }
};

/*
 * The basic resistances
 */

static const flag_desc resist_flags_desc[] =
{
	{ TR2_RES_ACID,        "Acid" },
	{ TR2_RES_ELEC,        "Lightning" },
	{ TR2_RES_FIRE,        "Fire" },
	{ TR2_RES_COLD,        "Cold" },
	{ TR2_RES_POIS,        "Poison" },
	{ TR2_RES_FEAR,        "Fear" },
	{ TR2_RES_LITE,        "Light" },
	{ TR2_RES_DARK,        "Dark" },
	{ TR2_RES_BLIND,       "Blindness" },
	{ TR2_RES_CONFU,       "Confusion" },
	{ TR2_RES_SOUND,       "Sound" },
	{ TR2_RES_SHARD,       "Shards" },
	{ TR2_RES_NEXUS,       "Nexus" },
	{ TR2_RES_NETHR,       "Nether" },
	{ TR2_RES_CHAOS,       "Chaos" },
	{ TR2_RES_DISEN,       "Disenchantment" },
};

/*
 * Elemental immunities (along with poison)
 */

static const flag_desc immune_flags_desc[] =
{
	{ TR2_IM_ACID,         "Acid" },
	{ TR2_IM_ELEC,         "Lightning" },
	{ TR2_IM_FIRE,         "Fire" },
	{ TR2_IM_COLD,         "Cold" },
};

/*
 * Sustain stats -  these are given their "own" line in the
 * spoiler file, mainly for simplicity
 */
static const flag_desc sustain_flags_desc[] =
{
	{ TR1_SUST_STR,        "STR" },
	{ TR1_SUST_INT,        "INT" },
	{ TR1_SUST_WIS,        "WIS" },
	{ TR1_SUST_DEX,        "DEX" },
	{ TR1_SUST_CON,        "CON" },
	{ TR1_SUST_CHR,        "CHR" },
};

/*
 * Miscellaneous magic given by an object's "flags3" field
 *
 * Note that cursed artifacts and objects with permanent light
 * are handled "directly" -- see analyze_misc_magic()
 */

static const flag_desc misc_flags3_desc[] =
{
	{ TR3_SLOW_DIGEST,     "Slow Digestion" },
	{ TR3_FEATHER,         "Feather Falling" },
	{ TR3_LITE,            "Permanent Light" },
	{ TR3_REGEN,           "Regeneration" },
	{ TR3_TELEPATHY,       "ESP" },
	{ TR3_SEE_INVIS,       "See Invisible" },
	{ TR3_FREE_ACT,        "Free Action" },
	{ TR3_HOLD_LIFE,       "Hold Life" },
	{ TR3_BLESSED,         "Blessed Blade" },
	{ TR3_IMPACT,          "Thrusts back opponents on hit" },
	{ TR3_NOFUEL,          "Requires no fuel" },
	{ TR3_TELEPORT,        "Induces random teleportation" },
	{ TR3_AGGRAVATE,       "Aggravates" },
	{ TR3_DRAIN_EXP,       "Drains Experience" },
	{ TR3_DRAIN_HP,        "Drains Hitpoints" },
};



/*
 * A special type used just for dealing with pvals
 */

typedef struct
{
	/*
	 * This will contain a string such as "+2", "-10", etc.
	 */
	char pval_desc[12];

	/*
	 * A list of various player traits affected by an object's pval such
	 * as stats, speed, stealth, etc.
	 *
	 * Note that room need only be reserved for the number of stats - 1
	 * since the description "All stats" is used if an object affects all
	 * stats. Also, room must be reserved for a sentinel NULL pointer.
	 *
	 * This will be a list such as ["STR", "DEX", "Stealth", NULL] etc.
	 */
	cptr pval_affects[N_ELEMENTS(stat_flags_desc) - 1 +
			  N_ELEMENTS(pval_flags_desc) + 1];


} pval_info_type;


/*
 * An "object analysis structure"
 *
 * It will be filled with descriptive strings detailing an object's
 * various magical powers. The "ignore X" traits are not noted since
 * all artifacts ignore "normal" destruction.
 */
typedef struct
{
	/* "The Longsword Dragonsmiter (6d4) (+20, +25)" */
	char description[160];

	/* Description of what is affected by an object's pvals */
	pval_info_type pval_info1;
	pval_info_type pval_info2;
	pval_info_type pval_info3;

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
	cptr misc_magic[N_ELEMENTS(misc_flags3_desc)
			+ 1	  /* Permanent Light */
			+ 1	  /* type of curse */
			+ 1];	  /* sentinel NULL */

	/* A string describing an artifact's activation */
	char activation[80];

	/* "Level 20, Rarity 30, 3.0 lbs, 20000 Gold" */
	char misc_desc[80];

} obj_desc_list;




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
static cptr *spoiler_flag_aux(const u32b art_flags, const flag_desc *flag_x_ptr,
			      cptr *desc_x_ptr, const int n_elmnts)
{
	int i;

	for (i = 0; i < n_elmnts; ++i)
	{
		if (art_flags & flag_x_ptr[i].flag)
		{
			*desc_x_ptr++ = flag_x_ptr[i].desc;
		}
	}

	return (desc_x_ptr);
}


/*
 * Acquire a "basic" description "The Cloak of Death [1,+10]"
 */
static void analyze_general(object_type *o_ptr, char *desc_x_ptr)
{
	/* Get a "useful" description of the object */
	object_desc_store(desc_x_ptr, o_ptr, TRUE, 1);
}

/*
 * List attributes altered by an artifact's pvals.
 */
static void analyze_pval(int num, object_type *o_ptr,
	pval_info_type *pval_x_ptr)
{
	const u32b all_stats = (TR_PVAL_STR | TR_PVAL_INT | TR_PVAL_WIS |
				TR_PVAL_DEX | TR_PVAL_CON | TR_PVAL_CHR);

	int pval;
	u32b flags_pval;

	cptr *affects_list = pval_x_ptr->pval_affects;

	/* Get the right pval and pval-dependant flags */
	if (num == 1)
	{
		pval = o_ptr->pval;
		flags_pval = o_ptr->flags_pval1;
	}
	else if (num == 2)
	{
		pval = o_ptr->pval2;
		flags_pval = o_ptr->flags_pval2;
	}
	else if (num == 3)
	{
		pval = o_ptr->pval3;
		flags_pval = o_ptr->flags_pval3;
	}
	else return;

	/* Create the "+N" string */
	strnfmt(pval_x_ptr->pval_desc, sizeof(pval_x_ptr->pval_desc),
	        "%+d", pval);

	/* First, check to see if the pval affects all stats */
	if ((flags_pval & all_stats) == all_stats)
	{
		*affects_list++ = "All stats";
	}

	/* Are any stats affected? */
	else if (flags_pval & all_stats)
	{
		affects_list = spoiler_flag_aux(flags_pval, stat_flags_desc,
						affects_list,
						N_ELEMENTS(stat_flags_desc));
	}

	/* And now the "rest" */
	affects_list = spoiler_flag_aux(flags_pval, pval_flags_desc,
					affects_list,
					N_ELEMENTS(pval_flags_desc));

	/* Terminate the description list */
	*affects_list = NULL;
}

/* Note the slaying specialties of a weapon */
static void analyze_slay(object_type *o_ptr, cptr *slay_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	slay_list = spoiler_flag_aux(f1, slay_flags_desc, slay_list,
				     N_ELEMENTS(slay_flags_desc));

	/* Terminate the description list */
	*slay_list = NULL;
}

/* Note an object's elemental brands */
static void analyze_brand(object_type *o_ptr, cptr *brand_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	brand_list = spoiler_flag_aux(f1, brand_flags_desc, brand_list,
				      N_ELEMENTS(brand_flags_desc));

	/* Terminate the description list */
	*brand_list = NULL;
}


/* Note the resistances granted by an object */

static void analyze_resist(object_type *o_ptr, cptr *resist_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	resist_list = spoiler_flag_aux(f2, resist_flags_desc,
				       resist_list, N_ELEMENTS(resist_flags_desc));

	/* Terminate the description list */
	*resist_list = NULL;
}

/* Note the immunities granted by an object */

static void analyze_immune(object_type *o_ptr, cptr *immune_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	immune_list = spoiler_flag_aux(f2, immune_flags_desc,
				       immune_list, N_ELEMENTS(immune_flags_desc));

	/* Terminate the description list */
	*immune_list = NULL;

}

/* Note which stats an object sustains */

static void analyze_sustains(object_type *o_ptr, cptr *sustain_list)
{
	const u32b all_sustains = (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
				   TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR);

	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	/* Simplify things if an item sustains all stats */
	if ((f1 & all_sustains) == all_sustains)
	{
		*sustain_list++ = "All stats";
	}

	/* Should we bother? */
	else if ((f1 & all_sustains))
	{
		sustain_list = spoiler_flag_aux(f1, sustain_flags_desc,
						sustain_list,
						N_ELEMENTS(sustain_flags_desc));
	}

	/* Terminate the description list */
	*sustain_list = NULL;
}


/*
 * Note miscellaneous powers bestowed by an artifact such as see invisible,
 * free action, permanent light, etc.
 */
static void analyze_misc_magic(object_type *o_ptr, cptr *misc_list)
{
	u32b f1, f2, f3;

	object_flags(o_ptr, &f1, &f2, &f3);

	/*
	 * Special flags
	 */
	misc_list = spoiler_flag_aux(f3, misc_flags3_desc, misc_list,
				     N_ELEMENTS(misc_flags3_desc));

	/*
	 * Artifact lights -- large radius light.
	 */
	if ((o_ptr->tval == TV_LITE) && artifact_p(o_ptr))
	{
		*misc_list++ = "Permanent Light(3)";
	}

	/*
	 * Handle cursed objects here to avoid redundancies such as noting
	 * that a permanently cursed object is heavily cursed as well as
	 * being "lightly cursed".
	 */

	if (cursed_p(o_ptr))
	{
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
 * Determine the minimum depth an artifact can appear, its rarity, its weight,
 * and its value in gold pieces
 */
static void analyze_misc(object_type *o_ptr, char *misc_desc)
{
	artifact_type *a_ptr = &a_info[o_ptr->artifact_index];

	if (use_metric) sprintf(misc_desc, "Level %u, Rarity %u, %d.%d kgs, "
		"%ld Gold", a_ptr->level, a_ptr->rarity,
		make_metric(a_ptr->weight) / 10, make_metric(a_ptr->weight) % 10,
		a_ptr->cost);

	else sprintf(misc_desc, "Level %u, Rarity %u, %d.%d lbs, "
		"%ld Gold", a_ptr->level, a_ptr->rarity,
		a_ptr->weight / 10, a_ptr->weight % 10, (long)a_ptr->cost);
}

/*
 * Fill in an object description structure for a given object
 */
static void object_analyze(object_type *o_ptr, obj_desc_list *desc_x_ptr)
{
	analyze_general(o_ptr, desc_x_ptr->description);

	analyze_pval(1, o_ptr, &desc_x_ptr->pval_info1);
	analyze_pval(2, o_ptr, &desc_x_ptr->pval_info2);
	analyze_pval(3, o_ptr, &desc_x_ptr->pval_info3);

	analyze_brand(o_ptr, desc_x_ptr->brands);

	analyze_slay(o_ptr, desc_x_ptr->slays);

	analyze_immune(o_ptr, desc_x_ptr->immunities);

	analyze_resist(o_ptr, desc_x_ptr->resistances);

	analyze_sustains(o_ptr, desc_x_ptr->sustains);

	analyze_misc_magic(o_ptr, desc_x_ptr->misc_magic);

	analyze_misc(o_ptr, desc_x_ptr->misc_desc);

	strcpy(desc_x_ptr->activation, do_activation_aux(OBJECT_INFO, o_ptr));
}


/*
 * This is somewhat ugly.
 *
 * Given a header ("Resist", e.g.), a list ("Fire", "Cold", Acid", e.g.),
 * and a separator character (',', e.g.), write the list to the spoiler file
 * in a "nice" format, such as:
 *
 *	Resist Fire, Cold, Acid
 *
 * That was a simple example, but when the list is long, a line wrap
 * should occur, and this should induce a new level of indention if
 * a list is being spread across lines. So for example, Bladeturner's
 * list of resistances should look something like this
 *
 *     Resist Acid, Lightning, Fire, Cold, Poison, Light, Dark, Blindness,
 *	 Confusion, Sound, Shards, Nether, Nexus, Chaos, Disenchantment
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


static void spoiler_outlist(cptr header, cptr *list, char separator)
{
	int line_len, buf_len;
	char line[MAX_LINE_LEN+1], buf[80];

	/* Ignore an empty list */
	if (*list == NULL) return;

	/* This function always indents */
	my_strcpy(line, INDENT1, sizeof(line));

	/* Create header (if one was given) */
	if (header && (header[0]))
	{
		my_strcat(line, header, sizeof(line));
		strcat(line, " ");
	}

	line_len = strlen(line);

	/* Now begin the tedious task */
	while (TRUE)
	{
		/* Copy the current item to a buffer */
		my_strcpy(buf, *list, sizeof(buf));

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
			my_strcat(line, buf, sizeof(line));
			line_len += buf_len;
		}

		/* Apply line wrapping and indention effects described above */
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
				text_out(line);
				text_out("\n");

				/* Begin new line at primary indention level */
				strnfmt(line, sizeof(line), "%s%s", INDENT1, buf);
			}
			else
			{
				/* Write to spoiler file */
				text_out(line);
				text_out("\n");

				/* Begin new line at secondary indention level */
				strnfmt(line, sizeof(line), "%s%s", INDENT2, buf);
			}

			line_len = strlen(line);
		}

		/* Advance, with break */
		if (!*++list) break;
	}

	/* Write what's left to the spoiler file */
	text_out(line);
	text_out("\n");
}


/* Create a spoiler file entry for an artifact */

static void spoiler_print_art(obj_desc_list *art_ptr)
{
	pval_info_type *pval_ptr = &art_ptr->pval_info1;

	char buf[80];

	/* Don't indent the first line */
	text_out(art_ptr->description);
	text_out("\n");

	/* An "empty" pval description indicates that the pval affects nothing */
	if (pval_ptr->pval_desc[0])
	{
		/* Mention the effects of pval */
		strnfmt(buf, sizeof(buf), "%s to", pval_ptr->pval_desc);
		spoiler_outlist(buf, pval_ptr->pval_affects, ITEM_SEP);
	}

	pval_ptr = &art_ptr->pval_info2;
	if (pval_ptr->pval_desc[0])
	{
		/* Mention the effects of pval */
		strnfmt(buf, sizeof(buf), "%s to", pval_ptr->pval_desc);
		spoiler_outlist(buf, pval_ptr->pval_affects, ITEM_SEP);
	}

	pval_ptr = &art_ptr->pval_info3;
	if (pval_ptr->pval_desc[0])
	{
		/* Mention the effects of pval */
		strnfmt(buf, sizeof(buf), "%s to", pval_ptr->pval_desc);
		spoiler_outlist(buf, pval_ptr->pval_affects, ITEM_SEP);
	}


	/* Now deal with the description lists */

	spoiler_outlist("Slay", art_ptr->slays, ITEM_SEP);

	spoiler_outlist("", art_ptr->brands, LIST_SEP);

	spoiler_outlist("Immunity to", art_ptr->immunities, ITEM_SEP);

	spoiler_outlist("Resist", art_ptr->resistances, ITEM_SEP);

	spoiler_outlist("Sustain", art_ptr->sustains, ITEM_SEP);

	spoiler_outlist("", art_ptr->misc_magic, LIST_SEP);


	/* Write out the possible activation at the primary indention level */
	if (strlen(art_ptr->activation) > 1)
	{
		fprintf(fff, "%sActivates for %s\n", INDENT1, art_ptr->activation);
	}

	/* End with the miscellaneous facts */
	fprintf(fff, "%s%s\n\n", INDENT1, art_ptr->misc_desc);
}




/*
 * Show what object kinds appear on the current level
 */
static void spoil_obj_gen(cptr fname)
{
	int i, j;
	int tmp_object_level = object_level;
	int old_depth = p_ptr->depth;

	/* Storage */
	u32b artifacts = 0L;
	u32b egoitems = 0L;
	u32b object[1500];
	s16b ego_item[500];
	s16b artifact[ART_MIN_RANDOM + 1];
	u32b tval[TV_MAX];
	u32b depth[MAX_DEPTH];

	object_type *i_ptr;
	object_type object_type_body;
	char o_name[120];

	char buf[1024];


	if (!get_check("This will take quite a while (as in, go read a book).  Are you certain you want to print out object generation statistics (y/n)?"))
	{
		return;
	}


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

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

	/* Warning */
	msg_print("This will take quite a while...");
	if (!fresh_after) Term_fresh();


	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Object Generation");


	/* Initialize object level */
	object_level = 0;
	p_ptr->depth = 0;

	/* Handle various possible depths */
	while (TRUE)
	{
		/* Go from level 5 to level 100 inclusive */
		if (object_level >= 100) break;
		object_level += 5;
		p_ptr->depth += 5;

		artifacts = 0L;
		egoitems = 0L;

		/* Clear storage. */
		for (i = 0; i < z_info->k_max; i++)
		{
			object[i] = 0L;
		}

		/* Clear storage. */
		for (i = 0; i < MAX_DEPTH; i++)
		{
			depth[i] = 0L;
		}

		/* Clear storage. */
		for (i = 0; i < TV_MAX; i++)
		{
			tval[i] = 0L;
		}

		/* Clear storage. */
		for (i = 0; i < ART_MIN_RANDOM + 1; i++)
		{
			artifact[i] = 0;
		}

		/* Clear storage. */
		for (i = 0; i < z_info->e_max; i++)
		{
			ego_item[i] = 0;
		}

		/* Make a lot of objects */
		for (i = 0L; i < 100000L; i++)
		{
			/* Get local object */
			i_ptr = &object_type_body;

			/* Create an object - no special conditions */
			make_object(i_ptr, FALSE, FALSE, FALSE);

			/* Count artifacts. */
			if (i_ptr->artifact_index) artifacts += 1L;

			/* Count ego-items. */
			if (i_ptr->ego_item_index) egoitems += 1L;

			/* Count object by index (note quantity). */
			object[i_ptr->k_idx] += i_ptr->number;

			/* Count objects of that level (only one at a time). */
			depth[k_info[i_ptr->k_idx].level] += 1L;

			/* Count object kinds. */
			tval[i_ptr->tval] += 1L;

			/* Count artifacts */
			if (i_ptr->artifact_index)
			{
				if (i_ptr->artifact_index < ART_MIN_RANDOM)
				{
					artifact[i_ptr->artifact_index]++;
				}
				else
				{
					artifact[ART_MIN_RANDOM]++;
				}
			}

			/* Count ego-items */
			else if (i_ptr->ego_item_index)
			{
				ego_item[i_ptr->ego_item_index]++;
			}

			/* Mega-Hack -- allow multiple artifacts XXX XXX XXX */
			if (artifact_p(i_ptr)) a_info[i_ptr->artifact_index].cur_num = 0;
		}

		/* Print to file. */
		fprintf(fff, "\n\n\n\n");
		fprintf(fff, "----------------------------------------\n");
		fprintf(fff, "         Generation Level:  %d\n\n", object_level);
		fprintf(fff, "Number of objects created (1,000,000 total)\n");
		fprintf(fff, "\n");

		for (i = 1; i < z_info->k_max; i++)
		{
			if (object[i])
			{
				object_kind *k_ptr = &k_info[i];
				char *t;
				cptr str = (k_name + k_ptr->name);
				cptr desc;

				if (str == "") continue;

				/* Skip past leading characters */
				while ((*str == ' ') || (*str == '&')) str++;

				/* Copy useful chars */
				for (t = o_name; *str; str++)
				{
					if (*str != '~') *t++ = *str;
				}

				/* Terminate the new name */
				*t = '\0';

				/* Try to find this tval in the list */
				for (j = 0; tvals[j].tval; j++)
				{
					if (tvals[j].tval == k_ptr->tval) break;
				}

				if (!tvals[j].tval) desc = "unknown";

				else desc = tvals[j].desc;

				fprintf(fff, "%-20s:%-40s:%6ld\n", desc, o_name, (long)object[i]);
			}
		}

		/* Header -- tval abundance */
		fprintf(fff, "\n\n");
		fprintf(fff, "Object tvals\n\n");

		/* Scan all the tvals */
		for (i = 1; i <= TV_MAX; i++)
		{
			/* Objects with this tval never appeared */
			if (!tval[i]) continue;

			/* Try to find this tval in the list */
			for (j = 0; tvals[j].tval; j++)
			{
				if (tvals[j].tval == i) break;
			}

			/* Tval has no description -- skip it */
			if (!tvals[j].tval) continue;

			/* Print out a description of the tval and its abundance */
			fprintf(fff, "%-20s:%6ld\n", tvals[j].desc, (long)tval[i]);
		}

		fprintf(fff, "\n\n");
		fprintf(fff, "Object distribution by depth\n\n");

		for (i = 0; i < MAX_DEPTH; i++)
		{
			if (depth[i]) fprintf(fff, "Level %3d:%6ld\n", i, (long)depth[i]);
		}

		fprintf(fff, "\n\n");
		fprintf(fff, "artifacts:  %ld\n", (long)artifacts);
		for (i = 0; i < ART_MIN_RANDOM + 1; i++)
		{
			artifact_type *a_ptr = &a_info[i];

			cptr str = (a_name + a_ptr->name);

			if (artifact[i]) fprintf(fff, " %-40s:%4d\n", str, artifact[i]);
		}

		fprintf(fff, "\n\n");
		fprintf(fff, "ego-items:  %ld\n\n", (long)egoitems);
		for (i = 0; i < z_info->e_max; i++)
		{
			ego_item_type *e_ptr = &e_info[i];

			cptr str = (e_name + e_ptr->name);

			if (ego_item[i]) fprintf(fff, " %-40s:%4d\n", str, ego_item[i]);
		}
	}


	/* Reset object generation level */
	object_level = tmp_object_level;
	p_ptr->depth = old_depth;

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
 * Show what monster races appear on the current level
 */
static void spoil_mon_gen(cptr fname)
{
	int i, num;

	/* Storage */
	u32b monster[1000];
	u32b depth[MAX_DEPTH];

	char buf[1024];

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

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

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Monster Generation");

	/* Clear storage. */
	for (i = 0; i < z_info->r_max; i++)
	{
		monster[i] = 0L;
	}

	/* Clear storage. */
	for (i = 0; i < MAX_DEPTH; i++)
	{
		depth[i] = 0L;
	}

	msg_print("This may take a while...");
	if (!fresh_after) Term_fresh();

	/* Make a lot of monsters, and print their names out. */
	for (i = 0L; i < 1000000L; i++)
	{
		if (i % 10000 == 0)
		{
			prt(format("%ld monsters created", (long)i), 0, 0);
			if (!fresh_after) Term_fresh();
		}

		/* Get a monster index */
		num = get_mon_num(p_ptr->depth);

		/* Count monster races. */
		monster[num] += 1L;

		/* Count monsters of that level. */
		depth[r_info[num].level] += 1L;
	}

	/* Print to file. */
	fprintf(fff, "\n\n\n");
	fprintf(fff, "Number of monsters of various kinds (1,000,000 total)\n");
	fprintf(fff, "         Generation Level:  %d\n\n", p_ptr->depth);

	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		cptr name = (r_name + r_ptr->name);

		if (monster[i])
		{
			fprintf(fff, "%-45s:%6ld\n", name, (long)monster[i]);
		}
	}

	fprintf(fff, "\n\n\n");
	fprintf(fff, "Monster distribution by depth\n\n");

	for (i = 0; i < MAX_DEPTH; i++)
	{
		if (depth[i]) fprintf(fff, "Level %3d:%6ld\n", i, (long)depth[i]);
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
 * Create a spoiler file for artifacts
 */
static void spoil_artifact(cptr fname)
{
	int i, j;

	object_type *i_ptr;
	object_type object_type_body;

	obj_desc_list artifact;

	char buf[1024];


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

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

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Dump the header */
	print_header("Artifact");

	/* List the artifacts by tval */
	for (i = 0; group_artifact[i].tval; i++)
	{
		/* Write out the group title */
		if (group_artifact[i].name)
		{
			spoiler_blanklines(2);
			spoiler_underline(group_artifact[i].name);
			spoiler_blanklines(1);
		}

		/* Now search through all of the artifacts */
		for (j = 0; j < z_info->a_max; ++j)
		{
			artifact_type *a_ptr = &a_info[j];

			/* We only want objects in the current group */
			if (a_ptr->tval != group_artifact[i].tval) continue;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Attempt to create the artifact */
			if (!make_fake_artifact(i_ptr, j)) continue;

			/* Analyze the artifact */
			object_analyze(i_ptr, &artifact);

			/* Write out the artifact description to the spoiler file */
			spoiler_print_art(&artifact);
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
 * Create a spoiler file for monsters
 */
static void spoil_mon_desc(cptr fname)
{
	int i, n = 0;

	char buf[1024];

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	char exp[80];

	u16b *who;
	u16b why = 2;

	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

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

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Brief Monster");

	/* Dump the header */
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
		"----", "---", "---", "---", "--", "--", "-----------");


	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++] = (u16b)i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);

	/* Scan again */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];

		cptr name = (r_name + r_ptr->name);

		/* Get the "name" */
		if (r_ptr->flags1 & (RF1_QUESTOR))
		{
			strnfmt(nam, sizeof(nam), "[Q] %s", name);
		}
		else if (r_ptr->flags1 & (RF1_UNIQUE))
		{
			strnfmt(nam, sizeof(nam), "[U] %s", name);
		}
		else
		{
			strnfmt(nam, sizeof(nam), "The %s", name);
		}


		/* Level */
		sprintf(lev, "%d", r_ptr->level);

		/* Rarity */
		sprintf(rar, "%d", r_ptr->rarity);

		/* Speed */
		sprintf(spd, "%+d", (r_ptr->speed - 110));


		/* Armor Class */
		sprintf(ac, "%d", r_ptr->ac);

		/* Hitpoints */
		if (r_ptr->flags1 & (RF1_FIXED_HPS))
		{
			sprintf(hp, "%d", r_ptr->hitpoints);
		}
		else
		{
			sprintf(hp, "~%d", r_ptr->hitpoints);
		}


		/* Experience */
		sprintf(exp, "%ld", (long)(r_ptr->mexp));

		/* Hack -- use visual instead */
		strnfmt(exp, sizeof(exp), "%s '%c'", attr_to_text(r_ptr->d_attr),
		        r_ptr->d_char);

		/* Dump the info */
		fprintf(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			nam, lev, rar, spd, hp, ac, exp);
	}

	/* End it */
	fprintf(fff, "\n");

	/* Free the "who" array */
	FREE(who);

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
 * Create a spoiler file for monsters
 * smchorse@ringer.cs.utsa.edu (Shawn McHorse)
 */
static void spoil_mon_info(cptr fname)
{
	char buf[1024];
	int i, n;
	u16b why = 2;
	u16b *who;
	int count = 0;


	/* Build the filename */
	path_build(buf, sizeof(buf), ANGBAND_DIR_INFO, fname);

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

	/* Dump to the spoiler file */
	text_out_hook = text_out_to_file;
	text_out_file = fff;

	/* Print header */
	print_header("Full Monster");

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, u16b);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[count++] = (u16b)i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, count);

	/*
	 * List all monsters in order.
	 */
	for (n = 0; n < count; n++)
	{
		int r_idx = who[n];
		monster_race *r_ptr = &r_info[r_idx];

		/* Prefix */
		if (r_ptr->flags1 & RF1_QUESTOR)
		{
			text_out("[Q] ");
		}
		else if (r_ptr->flags1 & RF1_UNIQUE)
		{
			text_out("[U] ");
		}
		else
		{
			text_out("The ");
		}

		/* Name */
		strnfmt(buf, sizeof(buf), "%s  (", (r_name + r_ptr->name));	/* ---)--- */
		text_out(buf);

		/* Color */
		text_out(attr_to_text(r_ptr->d_attr));

		/* Symbol --(-- */
		sprintf(buf, " '%c')\n", r_ptr->d_char);
		text_out(buf);


		/* Indent */
		sprintf(buf, "=== ");
		text_out(buf);

		/* Number */
		sprintf(buf, "Num:%d  ", r_idx);
		text_out(buf);

		/* Level */
		sprintf(buf, "Lev:%d  ", r_ptr->level);
		text_out(buf);

		/* Rarity */
		sprintf(buf, "Rar:%d  ", r_ptr->rarity);
		text_out(buf);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			sprintf(buf, "Spd:+%d  ", (r_ptr->speed - 110));
		}
		else
		{
			sprintf(buf, "Spd:-%d  ", (110 - r_ptr->speed));
		}
		text_out(buf);

		/* Hitpoints */
		if (r_ptr->flags1 & (RF1_FIXED_HPS))
		{
			sprintf(buf, "%d", r_ptr->hitpoints);
		}
		else
		{
			sprintf(buf, "~%d", r_ptr->hitpoints);
		}
		text_out(buf);

		/* Armor Class */
		sprintf(buf, "Ac:%d  ", r_ptr->ac);
		text_out(buf);

		/* Experience */
		sprintf(buf, "Exp:%ld\n", (long)(r_ptr->mexp));
		text_out(buf);

		/* Describe */
		describe_monster(r_idx, TRUE);

		/* Terminate the entry */
		text_out("\n");
	}

	/* Free the "who" array */
	FREE(who);

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
 * Create Spoiler files
 */
void do_cmd_spoilers(void)
{
	int ch;


	/* Save screen */
	screen_save();


	/* Drop priv's */
	safe_setuid_drop();


	/* Interact */
	while (TRUE)
	{
		/* Clear screen */
		Term_clear();

		/* Info */
		prt(format("Create a spoiler file (appears in the .%s%s%s%s' directory).",
			PATH_SEP, "lib", PATH_SEP, "info"), 2, 0);

		/* Prompt for a file */
		prt("(1) Brief Object Info   (obj-desc.spo)", 5, 5);
		prt("(2) Brief Artifact Info (artifact.spo)", 6, 5);
		prt("(3) Brief Monster Info  (mon-desc.spo)", 7, 5);
		prt("(4) Full Monster Info   (mon-info.spo)", 8, 5);

		prt("(5) See what objects appear on this level  (obj-gen.spo)", 10, 5);
		prt("(6) See what monsters appear on this level (mon-gen.spo)", 11, 5);

		/* Prompt */
		prt("Command:", 12, 0);

		/* Get a choice */
		ch = inkey();

		/* Escape */
		if (ch == ESCAPE)
		{
			break;
		}

		/* Option (1) */
		else if (ch == '1')
		{
			spoil_obj_desc("obj-desc.spo");
		}

		/* Option (2) */
		else if (ch == '2')
		{
			spoil_artifact("artifact.spo");
		}

		/* Option (3) */
		else if (ch == '3')
		{
			spoil_mon_desc("mon-desc.spo");
		}

		/* Option (4) */
		else if (ch == '4')
		{
			spoil_mon_info("mon-info.spo");
		}

		/* Option (5) */
		else if (ch == '5')
		{
			spoil_obj_gen("obj-gen.spo");
		}

		/* Option (6) */
		else if (ch == '6')
		{
			spoil_mon_gen("mon-gen.spo");
		}

		/* Oops */
		else
		{
			bell("Illegal command for spoilers!");
		}

		/* Flush messages */
		msg_print(NULL);
	}


	/* Grab priv's */
	safe_setuid_grab();


	/* Load screen */
	screen_load();
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif


