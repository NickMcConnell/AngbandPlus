/* File: wizard1.c */

/* Purpose: Spoiler generation -BEN- */

#include "angband.h"
#include "script.h"


#ifdef ALLOW_SPOILERS

/* Uncomment to show estimated "correct" artifact costs in spoilers */
/* #define ESTIMATED_COST */


/*
 * The spoiler file being created
 */
static FILE *fff = NULL;

/*
 * Extract a textual representation of an attribute
 */
static cptr attr_to_text(byte a)
{
	switch (a)
	{
		case TERM_DARK: return ("xxx");
		case TERM_WHITE: return ("White");
		case TERM_SLATE: return ("Slate");
		case TERM_ORANGE: return ("Orange");
		case TERM_RED: return ("Red");
		case TERM_GREEN: return ("Green");
		case TERM_BLUE: return ("Blue");
		case TERM_UMBER: return ("Umber");
		case TERM_L_DARK: return ("L.Dark");
		case TERM_L_WHITE: return ("L.Slate");
		case TERM_VIOLET: return ("Violet");
		case TERM_YELLOW: return ("Yellow");
		case TERM_L_RED: return ("L.Red");
		case TERM_L_GREEN: return ("L.Green");
		case TERM_L_BLUE: return ("L.Blue");
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
}
grouper;



/*
 * Item Spoilers by: benh@phial.com (Ben Harrison)
 */


/*
 * The basic items categorized by type
 */
static const grouper group_item[] =
{
	{TV_SHOT, "Ammo"},
	{TV_ARROW, NULL},
	{TV_BOLT, NULL},

	{TV_BOW, "Bows"},

	{TV_SWORD, "Weapons"},
	{TV_POLEARM, NULL},
	{TV_HAFTED, NULL},
	{TV_DIGGING, NULL},

	{TV_SOFT_ARMOR, "Armour (Body)"},
	{TV_HARD_ARMOR, NULL},
	{TV_DRAG_ARMOR, NULL},

	{TV_CLOAK, "Armour (Misc)"},
	{TV_SHIELD, NULL},
	{TV_HELM, NULL},
	{TV_CROWN, NULL},
	{TV_GLOVES, NULL},
	{TV_BOOTS, NULL},

	{TV_AMULET, "Amulets"},
	{TV_RING, "Rings"},

	{TV_SCROLL, "Scrolls"},
	{TV_POTION, "Potions"},
	{TV_FOOD, "Food"},

	{TV_ROD, "Rods"},
	{TV_WAND, "Wands"},
	{TV_STAFF, "Staffs"},

	{TV_LIFE_BOOK, "Books (Life)"},
	{TV_SORCERY_BOOK, "Books (Sorcery)"},
	{TV_NATURE_BOOK, "Books (Nature)"},
	{TV_CHAOS_BOOK, "Books (Chaos)"},
	{TV_DEATH_BOOK, "Books (Death)"},
	{TV_TRUMP_BOOK, "Books (Trump)"},
	{TV_ARCANE_BOOK, "Books (Arcane)"},

	{TV_CHEST, "Chests"},

	{TV_FIGURINE, "Magical Figurines"},
	{TV_STATUE, "Statues"},

	{TV_SPIKE, "Various"},
	{TV_LITE, NULL},
	{TV_FLASK, NULL},
	{TV_JUNK, NULL},
	{TV_BOTTLE, NULL},
	{TV_SKELETON, NULL},

	{0, ""}
};


/*
 * Describe the kind
 */
static void kind_info(char *buf, char *dam, char *wgt, int *lev, s32b *val,
                      int k)
{
	object_type *q_ptr;

	/* Prepare a fake item */
	q_ptr = object_prep(k);

	/* It is known */
	q_ptr->info |= (OB_STOREB);

	/* Cancel bonuses */
	q_ptr->pval = 0;
	q_ptr->to_a = 0;
	q_ptr->to_h = 0;
	q_ptr->to_d = 0;


	/* Level */
	(*lev) = get_object_level(q_ptr);

	/* Value */
	(*val) = object_value(q_ptr);


	/* Hack */
	if (!buf || !dam || !wgt) return;


	/* Description (too brief) */
	object_desc_store(buf, q_ptr, FALSE, 0, 256);


	/* Misc info */
	dam[0] = 0;

	/* Damage */
	switch (q_ptr->tval)
	{
		case TV_BOW:
		{
			/* Bows */
			break;
		}

		case TV_SHOT:
		case TV_BOLT:
		case TV_ARROW:
		{
			/* Ammo */
			strnfmt(dam, 80, "%dd%d", (int)q_ptr->dd, (int)q_ptr->ds);
			break;
		}

		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		case TV_DIGGING:
		{
			/* Weapons */
			strnfmt(dam, 80, "%dd%d", (int)q_ptr->dd, (int)q_ptr->ds);
			break;
		}

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
			/* Armour */
			strnfmt(dam, 80, "%d", q_ptr->ac);
			break;
		}
	}


	/* Weight */
	strnfmt(wgt, 80, "%3d.%d", q_ptr->weight / 10, q_ptr->weight % 10);
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
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msgf("Cannot create spoiler file.");
		return;
	}


	/* Header */
	froff(fff, "Spoiler File -- Basic Items (%s %s)\n\n\n",
			VERSION_NAME, VERSION_STRING);

	/* More Header */
	froff(fff, "%-45s     %8s%7s%5s%9s\n",
			"Description", "Dam/AC", "Wgt", "Lev", "Cost");
	froff(fff, "%-45s     %8s%7s%5s%9s\n",
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
				froff(fff, "     %-45s%8s%7s%5d%9ld\n",
						buf, dam, wgt, e, (long)(v));
			}

			/* Start a new set */
			n = 0;

			/* Notice the end */
			if (!group_item[i].tval) break;

			/* Start a new set */
			froff(fff, "\n\n%s\n\n", group_item[i].name);
		}

		/* Acquire legal item types */
		for (k = 1; k < z_info->k_max; k++)
		{
			object_kind *k_ptr = &k_info[k];

			/* Skip wrong tval's */
			if (k_ptr->tval != group_item[i].tval) continue;

			/* Hack -- Skip instant-artifacts */
			if (FLAG(k_ptr, TR_INSTA_ART)) continue;

			/* Save the index */
			who[n++] = k;
		}
	}


	/* Check for errors */
	if (ferror(fff))
	{
		msgf("Cannot close spoiler file.");
		return;
	}

	my_fclose(fff);

	/* Message */
	msgf("Successfully created a spoiler file.");
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
 * Given an array, determine how many elements are in the array
 */
#define N_ELEMENTS(a) (sizeof (a) / sizeof ((a)[0]))

/*
 * The artifacts categorized by type
 */
static const grouper group_artifact[] =
{
	{TV_SWORD, "Edged Weapons"},
	{TV_POLEARM, "Polearms"},
	{TV_HAFTED, "Hafted Weapons"},
	{TV_BOW, "Bows"},

	{TV_SOFT_ARMOR, "Body Armor"},
	{TV_HARD_ARMOR, NULL},
	{TV_DRAG_ARMOR, NULL},

	{TV_CLOAK, "Cloaks"},
	{TV_SHIELD, "Shields"},
	{TV_HELM, "Helms/Crowns"},
	{TV_CROWN, NULL},
	{TV_GLOVES, "Gloves"},
	{TV_BOOTS, "Boots"},

	{TV_LITE, "Light Sources"},
	{TV_AMULET, "Amulets"},
	{TV_RING, "Rings"},

	{0, NULL}
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
	int set;
	const u32b flag;
	const char *const desc;
};



/*
 * These are used for "+3 to STR, DEX", etc. These are separate from
 * the other pval affected traits to simplify the case where an object
 * affects all stats.  In this case, "All stats" is used instead of
 * listing each stat individually.
 */

static const flag_desc stat_flags_desc[] =
{
	{TR_STR, "STR"},
	{TR_INT, "INT"},
	{TR_WIS, "WIS"},
	{TR_DEX, "DEX"},
	{TR_CON, "CON"},
	{TR_CHR, "CHR"}
};

/*
 * Besides stats, these are the other player traits
 * which may be affected by an object's pval
 */

static const flag_desc pval_flags1_desc[] =
{
	{TR_SP, "Mana"},
	{TR_STEALTH, "Stealth"},
	{TR_SEARCH, "Searching"},
	{TR_INFRA, "Infravision"},
	{TR_TUNNEL, "Tunneling"},
	{TR_BLOWS, "Attacks"},
	{TR_SPEED, "Speed"},
	{TR_SP, "SP"}
};

/*
 * Slaying preferences for weapons
 */

static const flag_desc slay_flags_desc[] =
{
	{TR_SLAY_ANIMAL, "Animal"},
	{TR_SLAY_EVIL, "Evil"},
	{TR_SLAY_UNDEAD, "Undead"},
	{TR_SLAY_DEMON, "Demon"},
	{TR_SLAY_ORC, "Orc"},
	{TR_SLAY_TROLL, "Troll"},
	{TR_SLAY_GIANT, "Giant"},
	{TR_SLAY_DRAGON, "Dragon"},
	{TR_KILL_DRAGON, "Xdragon"}
};

/*
 * Elemental brands for weapons
 *
 * Clearly, TR0_IMPACT is a bit out of place here. To simplify
 * coding, it has been included here along with the elemental
 * brands. It does seem to fit in with the brands and slaying
 * more than the miscellaneous section.
 */
static const flag_desc brand_flags_desc[] =
{
	{TR_BRAND_ACID, "Acid Brand"},
	{TR_BRAND_ELEC, "Lightning Brand"},
	{TR_BRAND_FIRE, "Flame Tongue"},
	{TR_BRAND_COLD, "Frost Brand"},
	{TR_BRAND_POIS, "Poisoned"},

	{TR_CHAOTIC, "Mark of Chaos"},
	{TR_VAMPIRIC, "Vampiric"},
	{TR_IMPACT, "Earthquake impact on hit"},
	{TR_VORPAL, "Very sharp"},
};


/*
 * The 15 resistables
 */
static const flag_desc resist_flags_desc[] =
{
	{TR_RES_ACID, "Acid"},
	{TR_RES_ELEC, "Lightning"},
	{TR_RES_FIRE, "Fire"},
	{TR_RES_COLD, "Cold"},
	{TR_RES_POIS, "Poison"},
	{TR_RES_FEAR, "Fear"},
	{TR_RES_LITE, "Light"},
	{TR_RES_DARK, "Dark"},
	{TR_RES_BLIND, "Blindness"},
	{TR_RES_CONF, "Confusion"},
	{TR_RES_SOUND, "Sound"},
	{TR_RES_SHARDS, "Shards"},
	{TR_RES_NETHER, "Nether"},
	{TR_RES_NEXUS, "Nexus"},
	{TR_RES_CHAOS, "Chaos"},
	{TR_RES_DISEN, "Disenchantment"},
};

/*
 * Elemental immunities (along with poison)
 */

static const flag_desc immune_flags_desc[] =
{
	{TR_IM_POIS, "Poison"},
	{TR_IM_ACID, "Acid"},
	{TR_IM_ELEC, "Lightning"},
	{TR_IM_FIRE, "Fire"},
	{TR_IM_COLD, "Cold"},
	{TR_IM_LITE, "Light"},
	{TR_IM_DARK, "Darkness"}
};

/*
 * Sustain stats -  these are given their "own" line in the
 * spoiler file, mainly for simplicity
 */
static const flag_desc sustain_flags_desc[] =
{
	{TR_SUST_STR, "STR"},
	{TR_SUST_INT, "INT"},
	{TR_SUST_WIS, "WIS"},
	{TR_SUST_DEX, "DEX"},
	{TR_SUST_CON, "CON"},
	{TR_SUST_CHR, "CHR"},
};

/*
 * Miscellaneous magic
 *
 * Note that cursed artifacts and objects with permanent light
 * are handled "directly" -- see analyze_misc_magic()
 */

static const flag_desc misc_flags2_desc[] =
{
	{TR_THROW, "Throwing"},
	{TR_REFLECT, "Reflection"},
	{TR_FREE_ACT, "Free Action"},
	{TR_HOLD_LIFE, "Hold Life"},
	{TR_SH_FIRE, "Fiery Aura"},
	{TR_SH_ELEC, "Electric Aura"},
	{TR_SH_COLD, "Frost Aura"},
	{TR_SH_ACID, "Acid Aura"},
	{TR_NO_TELE, "Prevent Teleportation"},
	{TR_NO_MAGIC, "Anti-Magic"},
	{TR_FEATHER, "Levitation"},
	{TR_SEE_INVIS, "See Invisible"},
	{TR_TELEPATHY, "ESP"},
	{TR_SLOW_DIGEST, "Slow Digestion"},
	{TR_REGEN, "Regeneration"},
	{TR_XTRA_SHOTS, "+1 Extra Shot"},	/* always +1? */
	{TR_DRAIN_EXP, "Drains Experience"},
	{TR_AGGRAVATE, "Aggravates"},
	{TR_HURT_FIRE, "Fire Vulnerability"},
	{TR_HURT_COLD, "Cold Vulnerability"},
	{TR_HURT_ELEC, "Lightning Vulnerability"},
	{TR_HURT_ACID, "Acid Vulnerability"},
	{TR_HURT_LITE, "Light Vulnerability"},
	{TR_HURT_DARK, "Darkness Vulnerability"},
	{TR_AUTO_CURSE, "Spontaneous Curse"},
	{TR_CANT_EAT, "Can't Eat"},
	{TR_SLOW_HEAL, "Slow Healing"},
	{TR_DRAIN_STATS, "Drains Stats"},
	{TR_BLESSED, "Blessed Blade"},
	{TR_LUCK_10, "+10 Save"},
	{TR_MUTATE, "Mutatagen"},
	{TR_PATRON, "Chaos Patron"},
	{TR_STRANGE_LUCK, "Warp Fate"},
	{TR_PASS_WALL, "Pass Walls"},
	{TR_GHOUL_TOUCH, "Ghoul Touch"},
	{TR_PSI_CRIT, "Magic-Powered Criticals"},
	{TR_RETURN, "Returning"},
	{TR_EXPLODE, "Explosive"},
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

}
pval_info_type;


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
	char description[256];

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
	cptr sustains[N_ELEMENTS(sustain_flags_desc) - 1 + 1];

	/* A list of various magical qualities an object may have */
	cptr misc_magic[N_ELEMENTS(misc_flags2_desc) + 1 +	/* Permanent Light */
					1 +	/* type of curse */
					1];	/* sentinel NULL */

	/* A string describing an artifact's activation */
	cptr activation;

	/* A string describing miscellaneous powers */
	cptr special;

	/* "Level 20, Rarity 30, 3.0 lbs, 20000 Gold" */
	char misc_desc[80];
}
obj_desc_list;


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
 * Write a line to the spoiler file and then "underline" it with hypens
 */
static void spoiler_underline(cptr str)
{
	froff(fff, "%s\n", str);
	spoiler_out_n_chars(strlen(str), '-');
	froff(fff, "\n");
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
static cptr *spoiler_flag_aux(const u32b *flags, const flag_desc *flag_ptr,
                              cptr *desc_ptr, const int n_elmnts)
{
	int i;

	for (i = 0; i < n_elmnts; ++i)
	{
		if (flags[flag_ptr[i].set] & flag_ptr[i].flag)
		{
			*desc_ptr++ = flag_ptr[i].desc;
		}
	}

	return desc_ptr;
}


/*
 * Acquire a "basic" description "The Cloak of Death [1,+10]"
 */
static void analyze_general(const object_type *o_ptr, char *desc_ptr)
{
	/* Get a "useful" description of the object */
	object_desc_store(desc_ptr, o_ptr, TRUE, 1, 256);
}


/*
 * List "player traits" altered by an artifact's pval. These include stats,
 * speed, infravision, tunneling, stealth, searching, and extra attacks.
 */
static void analyze_pval(const object_type *o_ptr, pval_info_type *p_ptr)
{
	const u32b all_stats = (TR0_STR | TR0_INT | TR0_WIS |
							TR0_DEX | TR0_CON | TR0_CHR);

	cptr *affects_list;

	/* If pval == 0, there is nothing to do. */
	if (!o_ptr->pval)
	{
		/* An "empty" pval description indicates that pval == 0 */
		p_ptr->pval_desc[0] = '\0';
		return;
	}

	affects_list = p_ptr->pval_affects;

	/* Create the "+N" string */
	strnfmt(p_ptr->pval_desc, 12, "%s%d", POSITIZE(o_ptr->pval), o_ptr->pval);

	/* First, check to see if the pval affects all stats */
	if ((o_ptr->flags[0] & all_stats) == all_stats)
	{
		*affects_list++ = "All stats";
	}

	/* Are any stats affected? */
	else if (o_ptr->flags[0] & all_stats)
	{
		affects_list = spoiler_flag_aux(o_ptr->flags, stat_flags_desc,
										affects_list,
										N_ELEMENTS(stat_flags_desc));
	}

	/* And now the "rest" */
	affects_list = spoiler_flag_aux(o_ptr->flags, pval_flags1_desc,
									affects_list, N_ELEMENTS(pval_flags1_desc));

	/* Terminate the description list */
	*affects_list = NULL;
}


/* Note the slaying specialties of a weapon */
static void analyze_slay(const object_type *o_ptr, cptr *slay_list)
{
	slay_list = spoiler_flag_aux(o_ptr->flags, slay_flags_desc, slay_list,
								 N_ELEMENTS(slay_flags_desc));

	/* Terminate the description list */
	*slay_list = NULL;
}

/* Note an object's elemental brands */
static void analyze_brand(const object_type *o_ptr, cptr *brand_list)
{
	brand_list = spoiler_flag_aux(o_ptr->flags, brand_flags_desc, brand_list,
								  N_ELEMENTS(brand_flags_desc));

	/* Terminate the description list */
	*brand_list = NULL;
}


/* Note the resistances granted by an object */
static void analyze_resist(const object_type *o_ptr, cptr *resist_list)
{
	resist_list = spoiler_flag_aux(o_ptr->flags, resist_flags_desc,
								   resist_list, N_ELEMENTS(resist_flags_desc));

	/* Terminate the description list */
	*resist_list = NULL;
}


/* Note the immunities granted by an object */
static void analyze_immune(const object_type *o_ptr, cptr *immune_list)
{
	immune_list = spoiler_flag_aux(o_ptr->flags, immune_flags_desc,
								   immune_list, N_ELEMENTS(immune_flags_desc));

	/* Terminate the description list */
	*immune_list = NULL;
}

/* Note which stats an object sustains */

static void analyze_sustains(const object_type *o_ptr, cptr *sustain_list)
{
	const u32b all_sustains = (TR1_SUST_STR | TR1_SUST_INT | TR1_SUST_WIS |
							   TR1_SUST_DEX | TR1_SUST_CON | TR1_SUST_CHR);

	/* Simplify things if an item sustains all stats */
	if ((o_ptr->flags[1] & all_sustains) == all_sustains)
	{
		*sustain_list++ = "All stats";
	}

	/* Should we bother? */
	else if ((o_ptr->flags[1] & all_sustains))
	{
		sustain_list = spoiler_flag_aux(o_ptr->flags, sustain_flags_desc,
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
static void analyze_misc_magic(const object_type *o_ptr, cptr *misc_list)
{
	misc_list = spoiler_flag_aux(o_ptr->flags, misc_flags2_desc, misc_list,
								 N_ELEMENTS(misc_flags2_desc));

	/*
	 * Artifact lights -- large radius light.
	 */
	if ((o_ptr->tval == TV_LITE) && (FLAG(o_ptr, TR_LITE)))
	{
		*misc_list++ = "Permanent Light(3)";
	}

	/*
	 * Glowing artifacts -- small radius light.
	 */
	else if (FLAG(o_ptr, TR_LITE))
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
		if (FLAG(o_ptr, TR_TY_CURSE))
		{
			*misc_list++ = "Ancient Curse";
		}
		if (FLAG(o_ptr, TR_PERMA_CURSE))
		{
			*misc_list++ = "Permanently Cursed";
		}
		else if (FLAG(o_ptr, TR_HEAVY_CURSE))
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
static void analyze_misc(const object_type *o_ptr, char *misc_desc)
{
	artifact_type *a_ptr;

	/* Only look at artifacts */
	if (!o_ptr->a_idx) return;

	a_ptr = &a_info[o_ptr->a_idx];

#ifndef ESTIMATED_COST
	strnfmt(misc_desc, 80, "Level %u, Rarity %u, %d.%d lbs, %ld Gold",
			(uint)a_ptr->level, (uint)a_ptr->rarity,
			a_ptr->weight / 10, a_ptr->weight % 10, a_ptr->cost);
#else
	{
	int est_cost = flag_cost(o_ptr, o_ptr->pval);

	if (wield_slot(o_ptr) == EQUIP_WIELD)
		est_cost += o_ptr->dd * o_ptr->ds * (20 + o_ptr->to_h + o_ptr->to_d) * 5;
	else
		est_cost += 200 * (o_ptr->to_h + o_ptr->to_d);
	est_cost += 100 * (o_ptr->ac + o_ptr->to_a);
	strnfmt(misc_desc, 80, "Level %u, Rarity %u, %d.%d lbs, %ld Gold, %ld Est",
			(uint)a_ptr->level, (uint)a_ptr->rarity,
			a_ptr->weight / 10, a_ptr->weight % 10, a_ptr->cost, est_cost);
	}
#endif		
}


/*
 * Fill in an object description structure for a given object
 */
static void object_analyze(const object_type *o_ptr, obj_desc_list *desc_ptr)
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

	if (streq("nothing", desc_ptr->activation))
	{
		/* Display nothing, if there is no activation */
		desc_ptr->activation = NULL;
	}

	desc_ptr->special = "";
	apply_object_trigger(TRIGGER_SPOIL, (object_type *) o_ptr, ":s", LUA_RETURN_NAMED(desc_ptr->special, "desc"));
	if (streq("", desc_ptr->special))
		desc_ptr->special = NULL;
}


static void print_header(void)
{
	spoiler_underline("Artifact Spoilers for " VERSION_NAME " Version " VERSION_STRING);
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


static void spoiler_outlist(cptr header, cptr *list, char separator)
{
	int line_len, buf_len;
	char line[MAX_LINE_LEN + 1], buf[80];

	/* Ignore an empty list */
	if (*list == NULL) return;

	/* This function always indents */
	line_len = strnfmt(line, MAX_LINE_LEN + 1, INDENT1);

	/* Create header (if one was given) */
	if (header &&(header[0]))
	{
		strnfcat(line, MAX_LINE_LEN + 1, &line_len, "%s ", header);
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
			strnfmt(buf + buf_len, 80 - buf_len, "%c ", separator);
			buf_len += 2;
		}

		/*
		 * If the buffer will fit on the current line, just append the
		 * buffer to the line and adjust the line length accordingly.
		 */

		if (line_len + buf_len <= MAX_LINE_LEN)
		{
			strnfcat(line, MAX_LINE_LEN + 1, &line_len, buf);
		}

		/* Apply line wrapping and indention semantics described above */
		else
		{
			/*
			 * Don't print a trailing list separator but do print a trailing
			 * item separator.
			 */
			if ((line_len > 1) && (line[line_len - 1] == ' ') &&
				(line[line_len - 2] == LIST_SEP))
			{
				/* Ignore space and separator */
				line[line_len - 2] = '\0';

				/* Write to spoiler file */
				froff(fff, "%s\n", line);

				/* Begin new line at primary indention level */
				strnfmt(line, MAX_LINE_LEN + 1, "%s%s", INDENT1, buf);
			}

			else
			{
				/* Write to spoiler file */
				froff(fff, "%s\n", line);

				/* Begin new line at secondary indention level */
				strnfmt(line, MAX_LINE_LEN + 1, "%s%s", INDENT2, buf);
			}

			line_len = strlen(line);
		}

		/* Advance, with break */
		if (!*++list) break;
	}

	/* Write what's left to the spoiler file */
	froff(fff, "%s\n", line);
}


/*
 * Create a spoiler file entry for an artifact
 */
static void spoiler_print_art(obj_desc_list *art_ptr)
{
	pval_info_type *pval_ptr = &art_ptr->pval_info;

	char buf[80];

	/* Don't indent the first line */
	froff(fff, "%s\n", art_ptr->description);

	/* An "empty" pval description indicates that the pval affects nothing */
	if (pval_ptr->pval_desc[0])
	{
		/* Mention the effects of pval */
		strnfmt(buf, 80, "%s to", pval_ptr->pval_desc);
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
	if (art_ptr->activation)
	{
		froff(fff, "%sActivates for %s\n", INDENT1, art_ptr->activation);
	}

	if (art_ptr->special)
	{
		froff(fff, "%s%s\n", INDENT1, art_ptr->special);
	}

	/* End with the miscellaneous facts */
	froff(fff, "%s%s\n\n", INDENT1, art_ptr->misc_desc);
}


/*
 * Hack -- Create a "forged" artifact
 */
static object_type *make_fake_artifact(int a_idx)
{
	int i;

	object_type *o_ptr;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return (NULL);

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return (NULL);

	/* Create the artifact */
	o_ptr = object_prep(i);

	/* Save the artifact flags */
	o_ptr->flags[0] |= a_ptr->flags[0];
	o_ptr->flags[1] |= a_ptr->flags[1];
	o_ptr->flags[2] |= a_ptr->flags[2];
	o_ptr->flags[3] |= a_ptr->flags[3];

	/* Set the fields */
	o_ptr->pval = a_ptr->pval;
	o_ptr->ac = a_ptr->ac;
	o_ptr->dd = a_ptr->dd;
	o_ptr->ds = a_ptr->ds;
	o_ptr->to_a = a_ptr->to_a;
	o_ptr->to_h = a_ptr->to_h;
	o_ptr->to_d = a_ptr->to_d;
	o_ptr->weight = a_ptr->weight;

	/* Mega-Hack -- set activation */
	o_ptr->a_idx = a_idx;

	/* Add any special scripts */
	for (i = 0; i < MAX_TRIGGER; i++)
	{
		if (a_ptr->trigger[i])
			o_ptr->trigger[i] = quark_add(a_text + a_ptr->trigger[i]);
	}
		
	/* Do not make another one */
	a_ptr->cur_num = 1;

	/* Save the inscription */
	o_ptr->xtra_name = quark_add(a_name + a_ptr->name);

	/* Apply special scripts */
	/* apply_object_trigger(TRIGGER_MAKE, o_ptr, "i", "lev", a_ptr->level); */

	/* Success */
	return (o_ptr);
}


/*
 * Create a spoiler file for artifacts
 */
static void spoil_artifact(cptr fname)
{
	int i, j;

	object_type *q_ptr;

	obj_desc_list artifact;

	char buf[1024];


	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msgf("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */
	print_header();

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
		for (j = 1; j < z_info->a_max; ++j)
		{
			artifact_type *a_ptr = &a_info[j];

			/* We only want objects in the current group */
			if (a_ptr->tval != group_artifact[i].tval) continue;

			/* Attempt to "forge" the artifact */
			q_ptr = make_fake_artifact(j);

			if (!q_ptr) continue;

			/* Analyze the artifact */
			object_analyze(q_ptr, &artifact);

			/* Write out the artifact description to the spoiler file */
			spoiler_print_art(&artifact);
		}
	}

	/* Check for errors */
	if (ferror(fff))
	{
		msgf("Cannot close spoiler file.");
		return;
	}

	my_fclose(fff);

	/* Message */
	msgf("Successfully created a spoiler file.");
}





/*
 * Create a spoiler file for monsters   -BEN-
 */
static void spoil_mon_desc(cptr fname)
{
	int i, n = 0;

	u16b why = 2;
	s16b *who;

	char buf[1024];

	char nam[80];
	char lev[80];
	char rar[80];
	char spd[80];
	char ac[80];
	char hp[80];
	char exp[80];
	char vis[80];


	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msgf("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */
	froff(fff, "Monster Spoilers for %s Version %s\n",
			VERSION_NAME, VERSION_STRING);
	froff(fff, "-------------------------------------------\n\n");
	froff(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			"Name", "Lev", "Rar", "Spd", "Hp", "Ac", "Visual Info");
	froff(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s\n",
			"----", "---", "---", "---", "--", "--", "-----------");


	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, s16b);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[n++] = i;
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

		cptr name = mon_race_name(r_ptr);

		/* Get the "name" */
		if (FLAG(r_ptr, RF_QUESTOR))
		{
			strnfmt(nam, 80, "[Q] %s", name);
		}
		else if (FLAG(r_ptr, RF_UNIQUE))
		{
			strnfmt(nam, 80, "[U] %s", name);
		}
		else
		{
			strnfmt(nam, 80, "The %s", name);
		}


		/* Level */
		strnfmt(lev, 80, "%d", (int)r_ptr->level);

		/* Rarity */
		strnfmt(rar, 80, "%d", (int)r_ptr->rarity);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			strnfmt(spd, 80, "+%d", (r_ptr->speed - 110));
		}
		else
		{
			strnfmt(spd, 80, "-%d", (110 - r_ptr->speed));
		}

		/* Armor Class */
		strnfmt(ac, 80, "%d", r_ptr->ac);

		/* Hitpoints */
		if (FLAG(r_ptr, RF_FORCE_MAXHP) || (r_ptr->hside == 1))
		{
			strnfmt(hp, 80, "%d", (int)r_ptr->hdice * r_ptr->hside);
		}
		else
		{
			strnfmt(hp, 80, "%dd%d", (int)r_ptr->hdice, (int)r_ptr->hside);
		}


		/* Experience */
		strnfmt(exp, 80, "%ld", (long)(r_ptr->mexp));

		/* Hack -- use visual instead */
		strnfmt(vis, 80, "%s '%c'", attr_to_text(r_ptr->d_attr), r_ptr->d_char);

		/* Dump the info */
		froff(fff, "%-40.40s%4s%4s%6s%8s%4s  %11.11s %s\n",
				nam, lev, rar, spd, hp, ac, exp, vis);
	}

	/* Free the "who" array */
	KILL(who);

	/* End it */
	froff(fff, "\n");

	/* Check for errors */
	if (ferror(fff))
	{
		msgf("Cannot close spoiler file.");
		return;
	}

	my_fclose(fff);

	/* Worked */
	msgf("Successfully created a spoiler file.");
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
static void spoil_out(cptr fmt, ...)
{
	cptr r;
	
	va_list vp;

	char buf[1024];
	
	char *str;

	/* Line buffer */
	static char roff_buf[256];

	/* Current pointer into line roff_buf */
	static char *roff_p = roff_buf;

	/* Last space saved into roff_buf */
	static char *roff_s = NULL;
	
	/* Special handling for "new sequence" */
	if (!fmt)
	{
		if (roff_p != roff_buf) roff_p--;
		while (*roff_p == ' ' && roff_p != roff_buf) roff_p--;
		if (roff_p == roff_buf) froff(fff, "\n");
		else
		{
			*(roff_p + 1) = '\0';
			froff(fff, "%s\n\n", roff_buf);
		}
		roff_p = roff_buf;
		roff_s = NULL;
		roff_buf[0] = '\0';
		return;
	}
	
	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, 1024, fmt, &vp);

	/* End the Varargs Stuff */
	va_end(vp);
	
	/* Start at the head of the buffer */
	str = buf;

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
			froff(fff, "%s\n", roff_buf);
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
	int msex, vn, i, j, k, n, count = 0;
	bool breath, magic, sin;
	cptr p, q;
	cptr vp[64];
	u32b flags1, flags2, flags3, flags4, flags5, flags6, flags7;

	u16b why = 2;
	s16b *who;


	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msgf("Cannot create spoiler file.");
		return;
	}


	/* Dump the header */
	spoiler_underline("Monster Spoilers for " VERSION_NAME "  Version " VERSION_STRING);
	spoiler_blanklines(1);

	/* Allocate the "who" array */
	C_MAKE(who, z_info->r_max, s16b);

	/* Scan the monsters */
	for (i = 1; i < z_info->r_max; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Use that monster */
		if (r_ptr->name) who[count++] = i;
	}

	/* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, count);


	/* Scan again */
	for (n = 0; n < count; n++)
	{
		monster_race *r_ptr = &r_info[who[n]];

		/* Extract the flags */
		flags1 = r_ptr->flags[0];
		flags2 = r_ptr->flags[1];
		flags3 = r_ptr->flags[2];
		flags4 = r_ptr->flags[3];
		flags5 = r_ptr->flags[4];
		flags6 = r_ptr->flags[5];
		flags7 = r_ptr->flags[6];
		breath = FALSE;
		magic = FALSE;

		/* Extract a gender (if applicable) */
		if (flags1 & (RF0_FEMALE)) msex = 2;
		else if (flags1 & (RF0_MALE)) msex = 1;
		else
			msex = 0;


		/* Prefix */
		if (flags1 & (RF0_QUESTOR))
		{
			spoil_out("[Q] ");
		}
		else if (flags1 & (RF0_UNIQUE))
		{
			spoil_out("[U] ");
		}
		else
		{
			spoil_out("The ");
		}

		/* Name */
		spoil_out("%s  (", mon_race_name(r_ptr));	/* ---)--- */

		/* Color */
		spoil_out(attr_to_text(r_ptr->d_attr));

		/* Symbol --(-- */
		spoil_out(" '%c')\n", r_ptr->d_char);


		/* Indent */
		spoil_out("=== ");

		/* Number */
		spoil_out("Num:%d  ", who[n]);

		/* Level */
		spoil_out("Lev:%d  ", (int)r_ptr->level);

		/* Rarity */
		spoil_out("Rar:%d  ", (int)r_ptr->rarity);

		/* Speed */
		if (r_ptr->speed >= 110)
		{
			spoil_out("Spd:+%d  ", (r_ptr->speed - 110));
		}
		else
		{
			spoil_out("Spd:-%d  ", (110 - r_ptr->speed));
		}

		/* Hitpoints */
		if ((flags1 & (RF0_FORCE_MAXHP)) || (r_ptr->hside == 1))
		{
			spoil_out("Hp:%d  ", ((int)r_ptr->hdice) * r_ptr->hside);
		}
		else
		{
			spoil_out("Hp:%dd%d  ", (int)r_ptr->hdice, (int)r_ptr->hside);
		}

		/* Armor Class */
		spoil_out("Ac:%d  ", r_ptr->ac);

		/* Experience */
		spoil_out("Exp:%ld\n", (long)(r_ptr->mexp));

		/* Describe */
		spoil_out(r_text + r_ptr->text);
		spoil_out("  ");


		spoil_out("This");

		if (flags2 & (RF1_XXX_1)) spoil_out(" something");
		if (flags3 & (RF2_ANIMAL)) spoil_out(" natural");
		if (flags3 & (RF2_EVIL)) spoil_out(" evil");
		if (flags3 & (RF2_GOOD)) spoil_out(" good");
		if (flags3 & (RF2_UNDEAD)) spoil_out(" undead");

		if (flags3 & (RF2_DRAGON)) spoil_out(" dragon");
		else if (flags3 & (RF2_DEMON)) spoil_out(" demon");
		else if (flags3 & (RF2_GIANT)) spoil_out(" giant");
		else if (flags3 & (RF2_TROLL)) spoil_out(" troll");
		else if (flags3 & (RF2_ORC)) spoil_out(" orc");
		else if (flags3 & (RF2_AMBERITE)) spoil_out(" Amberite");
		else
			spoil_out(" creature");

		if (flags7 & RF6_CAN_FLY)
		{
			spoil_out("flies");
		}
		else
			spoil_out("moves");

		if ((flags1 & (RF0_RAND_50)) && (flags1 & (RF0_RAND_25)))
		{
			spoil_out(" extremely erratically");
		}
		else if (flags1 & (RF0_RAND_50))
		{
			spoil_out(" somewhat erratically");
		}
		else if (flags1 & (RF0_RAND_25))
		{
			spoil_out(" a bit erratically");
		}
		else
		{
			spoil_out(" normally");
		}

		if (flags1 & (RF0_NEVER_MOVE))
		{
			spoil_out(", but does not deign to chase intruders");
		}

		spoil_out(".  ");

		if (!r_ptr->level || (flags1 & (RF0_FORCE_DEPTH)))
		{
			spoil_out("%s is never found out of depth.  ", wd_che[msex]);
		}

		if (flags1 & (RF0_FORCE_SLEEP))
		{
			spoil_out("%s is always created sluggish.  ", wd_che[msex]);
		}

		if (flags2 & (RF1_AURA_FIRE))
		{
			spoil_out("%s is surrounded by flames.  ", wd_che[msex]);
		}

		if (flags3 & (RF2_AURA_COLD))
		{
			spoil_out("%s is surrounded by ice.  ", wd_che[msex]);
		}

		if (flags2 & (RF1_AURA_ELEC))
		{
			spoil_out("%s is surrounded by electricity.  ", wd_che[msex]);
		}

		if (flags2 & (RF1_REFLECTING))
		{
			spoil_out("%s reflects bolt spells.  ", wd_che[msex]);
		}

		if (flags1 & (RF0_ESCORT))
		{
			spoil_out("%s usually appears with ", wd_che[msex]);

			if (flags1 & (RF0_ESCORTS)) spoil_out("escorts.  ");
			else
				spoil_out("an escort.  ");
		}

		if (flags1 & RF0_FRIENDS)
		{
			spoil_out("%^s usually appears in groups.  ", wd_che[msex]);
		}

		if (flags1 & (RF0_CHAR_MIMIC))
		{
			spoil_out("%s is a mimic.  ", wd_che[msex]);
		}

		/* Collect inate attacks */
		vn = 0;
		if (flags4 & RF3_SHRIEK) vp[vn++] = "shriek for help";
		if (flags4 & RF3_ELDRITCH_HORROR) vp[vn++] = "blast your sanity";
		if (flags4 & RF3_ROCKET) vp[vn++] = "shoot a rocket";
		if (flags4 & RF3_ARROW) vp[vn++] = "fire arrows";

		if (vn)
		{
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" may ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" or ");
				spoil_out(vp[i]);
			}
			spoil_out(".  ");
		}

		/* Collect breaths */
		vn = 0;
		if (flags4 & (RF3_BR_ACID)) vp[vn++] = "acid";
		if (flags4 & (RF3_BR_ELEC)) vp[vn++] = "lightning";
		if (flags4 & (RF3_BR_FIRE)) vp[vn++] = "fire";
		if (flags4 & (RF3_BR_COLD)) vp[vn++] = "frost";
		if (flags4 & (RF3_BR_POIS)) vp[vn++] = "poison";
		if (flags4 & (RF3_BR_NETH)) vp[vn++] = "nether";
		if (flags4 & (RF3_BR_LITE)) vp[vn++] = "light";
		if (flags4 & (RF3_BR_DARK)) vp[vn++] = "darkness";
		if (flags4 & (RF3_BR_CONF)) vp[vn++] = "confusion";
		if (flags4 & (RF3_BR_SOUN)) vp[vn++] = "sound";
		if (flags4 & (RF3_BR_CHAO)) vp[vn++] = "chaos";
		if (flags4 & (RF3_BR_DISE)) vp[vn++] = "disenchantment";
		if (flags4 & (RF3_BR_NEXU)) vp[vn++] = "nexus";
		if (flags4 & (RF3_BR_TIME)) vp[vn++] = "time";
		if (flags4 & (RF3_BR_INER)) vp[vn++] = "inertia";
		if (flags4 & (RF3_BR_GRAV)) vp[vn++] = "gravity";
		if (flags4 & (RF3_BR_SHAR)) vp[vn++] = "shards";
		if (flags4 & (RF3_BR_PLAS)) vp[vn++] = "plasma";
		if (flags4 & (RF3_BR_WALL)) vp[vn++] = "force";
		if (flags4 & (RF3_BR_MANA)) vp[vn++] = "mana";
		if (flags4 & (RF3_BR_NUKE)) vp[vn++] = "toxic waste";
		if (flags4 & (RF3_BR_DISI)) vp[vn++] = "disintegration";

		if (vn)
		{
			breath = TRUE;
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" may breathe ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" or ");
				spoil_out(vp[i]);
			}
			if (flags2 & (RF1_POWERFUL)) spoil_out(" powerfully");
		}

		/* Collect spells */
		vn = 0;
		if (flags5 & (RF4_BA_ACID)) vp[vn++] = "produce acid balls";
		if (flags5 & (RF4_BA_ELEC)) vp[vn++] = "produce lightning balls";
		if (flags5 & (RF4_BA_FIRE)) vp[vn++] = "produce fire balls";
		if (flags5 & (RF4_BA_COLD)) vp[vn++] = "produce frost balls";
		if (flags5 & (RF4_BA_POIS)) vp[vn++] = "produce poison balls";
		if (flags5 & (RF4_BA_NETH)) vp[vn++] = "produce nether balls";
		if (flags5 & (RF4_BA_WATE)) vp[vn++] = "produce water balls";
		if (flags4 & (RF3_BA_NUKE)) vp[vn++] = "produce balls of radiation";
		if (flags5 & (RF4_BA_MANA)) vp[vn++] = "produce mana storms";
		if (flags5 & (RF4_BA_DARK)) vp[vn++] = "produce darkness storms";
		if (flags4 & (RF3_BA_CHAO)) vp[vn++] = "invoke raw Logrus";
		if (flags6 & (RF5_HAND_DOOM)) vp[vn++] = "invoke the Hand of Doom";
		if (flags5 & (RF4_DRAIN_MANA)) vp[vn++] = "drain mana";
		if (flags5 & (RF4_MIND_BLAST)) vp[vn++] = "cause mind blasting";
		if (flags5 & (RF4_BRAIN_SMASH)) vp[vn++] = "cause brain smashing";
		if (flags5 & (RF4_CAUSE_1)) vp[vn++] = "cause light wounds and cursing";
		if (flags5 & (RF4_CAUSE_2)) vp[vn++] =
				"cause serious wounds and cursing";
		if (flags5 & (RF4_CAUSE_3)) vp[vn++] =
				"cause critical wounds and cursing";
		if (flags5 & (RF4_CAUSE_4)) vp[vn++] = "cause mortal wounds";
		if (flags5 & (RF4_BO_ACID)) vp[vn++] = "produce acid bolts";
		if (flags5 & (RF4_BO_ELEC)) vp[vn++] = "produce lightning bolts";
		if (flags5 & (RF4_BO_FIRE)) vp[vn++] = "produce fire bolts";
		if (flags5 & (RF4_BO_COLD)) vp[vn++] = "produce frost bolts";
		if (flags5 & (RF4_BO_POIS)) vp[vn++] = "produce poison bolts";
		if (flags5 & (RF4_BO_NETH)) vp[vn++] = "produce nether bolts";
		if (flags5 & (RF4_BO_WATE)) vp[vn++] = "produce water bolts";
		if (flags5 & (RF4_BO_MANA)) vp[vn++] = "produce mana bolts";
		if (flags5 & (RF4_BO_PLAS)) vp[vn++] = "produce plasma bolts";
		if (flags5 & (RF4_BO_ICEE)) vp[vn++] = "produce ice bolts";
		if (flags5 & (RF4_MISSILE)) vp[vn++] = "produce magic missiles";
		if (flags5 & (RF4_SCARE)) vp[vn++] = "terrify";
		if (flags5 & (RF4_BLIND)) vp[vn++] = "blind";
		if (flags5 & (RF4_CONF)) vp[vn++] = "confuse";
		if (flags5 & (RF4_SLOW)) vp[vn++] = "slow";
		if (flags5 & (RF4_HOLD)) vp[vn++] = "paralyze";
		if (flags6 & (RF5_HASTE)) vp[vn++] = "haste-self";
		if (flags6 & (RF5_HEAL)) vp[vn++] = "heal-self";
		if (flags6 & (RF5_INVULNER)) vp[vn++] = "make invulnerable";
		if (flags6 & (RF5_BLINK)) vp[vn++] = "blink-self";
		if (flags6 & (RF5_TPORT)) vp[vn++] = "teleport-self";
		if (flags6 & (RF5_XXX3)) vp[vn++] = "do something";
		if (flags6 & (RF5_XXX4)) vp[vn++] = "do something";
		if (flags6 & (RF5_TELE_TO)) vp[vn++] = "teleport to";
		if (flags6 & (RF5_TELE_AWAY)) vp[vn++] = "teleport away";
		if (flags6 & (RF5_TELE_LEVEL)) vp[vn++] = "teleport level";
		if (flags6 & (RF5_XXX5)) vp[vn++] = "do something";
		if (flags6 & (RF5_DARKNESS)) vp[vn++] = "create darkness";
		if (flags6 & (RF5_TRAPS)) vp[vn++] = "create traps";
		if (flags6 & (RF5_FORGET)) vp[vn++] = "cause amnesia";
		if (flags6 & (RF5_RAISE_DEAD)) vp[vn++] = "raise dead";
		if (flags6 & (RF5_S_MONSTER)) vp[vn++] = "summon a monster";
		if (flags6 & (RF5_S_MONSTERS)) vp[vn++] = "summon monsters";
		if (flags6 & (RF5_S_KIN)) vp[vn++] = "summon aid";
		if (flags6 & (RF5_S_ANT)) vp[vn++] = "summon ants";
		if (flags6 & (RF5_S_SPIDER)) vp[vn++] = "summon spiders";
		if (flags6 & (RF5_S_HOUND)) vp[vn++] = "summon hounds";
		if (flags6 & (RF5_S_HYDRA)) vp[vn++] = "summon hydras";
		if (flags6 & (RF5_S_ANGEL)) vp[vn++] = "summon an angel";
		if (flags6 & (RF5_S_DEMON)) vp[vn++] = "summon a demon";
		if (flags6 & (RF5_S_UNDEAD)) vp[vn++] = "summon an undead";
		if (flags6 & (RF5_S_DRAGON)) vp[vn++] = "summon a dragon";
		if (flags6 & (RF5_S_HI_UNDEAD)) vp[vn++] = "summon greater undead";
		if (flags6 & (RF5_S_HI_DRAGON)) vp[vn++] = "summon ancient dragons";
		if (flags6 & (RF5_S_CYBER)) vp[vn++] = "summon Cyberdemons";
		if (flags6 & (RF5_S_AMBERITES)) vp[vn++] = "summon Lords of Amber";
		if (flags6 & (RF5_S_UNIQUE)) vp[vn++] = "summon unique monsters";

		if (vn)
		{
			magic = TRUE;
			if (breath)
			{
				spoil_out(", and is also");
			}
			else
			{
				spoil_out(wd_che[msex]);
				spoil_out(" is");
			}

			spoil_out(" magical, casting spells");
			if (flags2 & (RF1_SMART)) spoil_out(" intelligently");

			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" which ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" or ");
				spoil_out(vp[i]);
			}
		}

		if (breath || magic)
		{
			spoil_out("; 1 time in %d.  ",
					200 / (r_ptr->freq_inate + r_ptr->freq_spell));
		}

		/* Collect special abilities. */
		vn = 0;
		if (flags7 & (RF6_CAN_SWIM))  vp[vn++] = "swim";
		if (flags2 & (RF1_OPEN_DOOR)) vp[vn++] = "open doors";
		if (flags2 & (RF1_BASH_DOOR)) vp[vn++] = "bash down doors";
		if (flags2 & (RF1_PASS_WALL)) vp[vn++] = "pass through walls";
		if (flags2 & (RF1_KILL_WALL)) vp[vn++] = "bore through walls";
		if (flags2 & (RF1_MOVE_BODY)) vp[vn++] = "push past weaker monsters";
		if (flags2 & (RF1_KILL_BODY)) vp[vn++] = "destroy weaker monsters";
		if (flags2 & (RF1_TAKE_ITEM)) vp[vn++] = "pick up objects";
		if (flags2 & (RF1_KILL_ITEM)) vp[vn++] = "destroy objects";
		if (flags7 & (RF6_LITE_1 | RF6_LITE_2)) vp[vn++] = "light the dungeon";

		if (vn)
		{
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" can ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" and ");
				spoil_out(vp[i]);
			}
			spoil_out(".  ");
		}

		if (flags2 & (RF1_INVISIBLE))
		{
			spoil_out(wd_che[msex]);
			spoil_out(" is invisible.  ");
		}
		if (flags2 & (RF1_COLD_BLOOD))
		{
			spoil_out(wd_che[msex]);
			spoil_out(" is cold blooded.  ");
		}
		if (flags2 & (RF1_EMPTY_MIND))
		{
			spoil_out(wd_che[msex]);
			spoil_out(" is not detected by telepathy.  ");
		}
		if (flags2 & (RF1_WEIRD_MIND))
		{
			spoil_out(wd_che[msex]);
			spoil_out(" is rarely detected by telepathy.  ");
		}
		if (flags2 & (RF1_MULTIPLY))
		{
			spoil_out(wd_che[msex]);
			spoil_out(" breeds explosively.  ");
		}
		if (flags2 & (RF1_REGENERATE))
		{
			spoil_out(wd_che[msex]);
			spoil_out(" regenerates quickly.  ");
		}

		/* Collect susceptibilities */
		vn = 0;
		if (flags3 & (RF2_HURT_ROCK)) vp[vn++] = "rock remover";
		if (flags3 & (RF2_HURT_LITE)) vp[vn++] = "bright light";
		if (flags3 & (RF2_HURT_FIRE)) vp[vn++] = "fire";
		if (flags3 & (RF2_HURT_COLD)) vp[vn++] = "cold";

		if (vn)
		{
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" is hurt by ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" and ");
				spoil_out(vp[i]);
			}
			spoil_out(".  ");
		}

		/* Collect immunities */
		vn = 0;
		if (flags3 & (RF2_IM_ACID)) vp[vn++] = "acid";
		if (flags3 & (RF2_IM_ELEC)) vp[vn++] = "lightning";
		if (flags3 & (RF2_IM_FIRE)) vp[vn++] = "fire";
		if (flags3 & (RF2_IM_COLD)) vp[vn++] = "cold";
		if (flags3 & (RF2_IM_POIS)) vp[vn++] = "poison";

		if (vn)
		{
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" resists ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" and ");
				spoil_out(vp[i]);
			}
			spoil_out(".  ");
		}

		/* Collect resistances */
		vn = 0;
		if (flags3 & (RF2_RES_NETH)) vp[vn++] = "nether";
		if (flags3 & (RF2_RES_WATE)) vp[vn++] = "water";
		if (flags3 & (RF2_RES_PLAS)) vp[vn++] = "plasma";
		if (flags3 & (RF2_RES_NEXU)) vp[vn++] = "nexus";
		if (flags3 & (RF2_RES_DISE)) vp[vn++] = "disenchantment";
		if (flags3 & (RF2_RES_TELE)) vp[vn++] = "teleportation";
		if ((flags3 & RF2_RES_TELE)
			&& !(FLAG(r_ptr, RF_UNIQUE))) vp[vn++] = "teleportation";

		if (vn)
		{
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" resists ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" and ");
				spoil_out(vp[i]);
			}
			spoil_out(".  ");
		}

		/* Collect non-effects */
		vn = 0;
		if (flags3 & (RF2_NO_STUN)) vp[vn++] = "stunned";
		if (flags3 & (RF2_NO_FEAR)) vp[vn++] = "frightened";
		if (flags3 & (RF2_NO_CONF)) vp[vn++] = "confused";
		if (flags3 & (RF2_NO_SLEEP)) vp[vn++] = "slept";
		if ((flags3 & RF2_RES_TELE)
			&& (FLAG(r_ptr, RF_UNIQUE))) vp[vn++] = "teleported";

		if (vn)
		{
			spoil_out(wd_che[msex]);
			for (i = 0; i < vn; i++)
			{
				if (!i) spoil_out(" cannot be ");
				else if (i < vn - 1) spoil_out(", ");
				else
					spoil_out(" or ");
				spoil_out(vp[i]);
			}
			spoil_out(".  ");
		}

		spoil_out(wd_che[msex]);
		if (r_ptr->sleep > 200) spoil_out(" prefers to ignore");
		else if (r_ptr->sleep > 95) spoil_out(" pays very little attention to");
		else if (r_ptr->sleep > 75) spoil_out(" pays little attention to");
		else if (r_ptr->sleep > 45) spoil_out(" tends to overlook");
		else if (r_ptr->sleep > 25) spoil_out(" takes quite a while to see");
		else if (r_ptr->sleep > 10) spoil_out(" takes a while to see");
		else if (r_ptr->sleep > 5) spoil_out(" is fairly observant of");
		else if (r_ptr->sleep > 3) spoil_out(" is observant of");
		else if (r_ptr->sleep > 1) spoil_out(" is very observant of");
		else if (r_ptr->sleep > 0) spoil_out(" is vigilant for");
		else
			spoil_out(" is ever vigilant for");

		spoil_out(" intruders, which %s may notice from %d feet.  ",
				wd_lhe[msex], 10 * r_ptr->aaf);

		i = 0;
		if (flags1 & (RF0_DROP_60)) i += 1;
		if (flags1 & (RF0_DROP_90)) i += 2;
		if (flags1 & (RF0_DROP_1D2)) i += 2;
		if (flags1 & (RF0_DROP_2D2)) i += 4;
		if (flags1 & (RF0_DROP_3D2)) i += 6;
		if (flags1 & (RF0_DROP_4D2)) i += 8;

		/* Drops gold and/or items */
		if (i)
		{
			sin = FALSE;
			spoil_out(wd_che[msex]);
			spoil_out(" will carry");

			if (i == 1)
			{
				spoil_out(" a");
				sin = TRUE;
			}
			else if (i == 2)
			{
				spoil_out(" one or two");
				sin = TRUE;
			}
			else
			{
				spoil_out(" up to %u", (uint)i);
			}

			if (flags1 & (RF0_DROP_GREAT))
			{
				if (sin) spoil_out("n");
				spoil_out(" exceptional object");
			}
			else if (flags1 & (RF0_DROP_GOOD))
			{
				spoil_out(" good object");
			}
			else if (flags1 & (RF0_DROP_USEFUL))
			{
				spoil_out(" useful object");
			}
			else if (flags1 & (RF0_ONLY_ITEM))
			{
				spoil_out(" object");
			}
			else if (flags1 & (RF0_ONLY_GOLD))
			{
				spoil_out(" treasure");
			}
			else
			{
				spoil_out(" object");
				if (i > 1) spoil_out("s");
				spoil_out(" or treasure");
			}
			if (i > 1) spoil_out("s");

			if (flags1 & (RF0_DROP_CHOSEN))
			{
				spoil_out(", in addition to chosen objects");
			}

			spoil_out(".  ");
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

			/* Acquire the method */
			p = rbm_info[r_ptr->blow[j].method].name;

			/* Default effect */
			q = "???";

			/* Acquire the effect */
			switch (r_ptr->blow[j].effect)
			{
				case RBE_HURT:
				{
					q = "attack";
					break;
				}
				case RBE_POISON:
				{
					q = "poison";
					break;
				}
				case RBE_UN_BONUS:
				{
					q = "disenchant";
					break;
				}
				case RBE_UN_POWER:
				{
					q = "drain charges";
					break;
				}
				case RBE_EAT_GOLD:
				{
					q = "steal gold";
					break;
				}
				case RBE_EAT_ITEM:
				{
					q = "steal items";
					break;
				}
				case RBE_EAT_FOOD:
				{
					q = "eat your food";
					break;
				}
				case RBE_EAT_LITE:
				{
					q = "absorb light";
					break;
				}
				case RBE_ACID:
				{
					q = "shoot acid";
					break;
				}
				case RBE_ELEC:
				{
					q = "electrocute";
					break;
				}
				case RBE_FIRE:
				{
					q = "burn";
					break;
				}
				case RBE_COLD:
				{
					q = "freeze";
					break;
				}
				case RBE_BLIND:
				{
					q = "blind";
					break;
				}
				case RBE_CONFUSE:
				{
					q = "confuse";
					break;
				}
				case RBE_TERRIFY:
				{
					q = "terrify";
					break;
				}
				case RBE_PARALYZE:
				{
					q = "paralyze";
					break;
				}
				case RBE_LOSE_STR:
				{
					q = "reduce strength";
					break;
				}
				case RBE_LOSE_INT:
				{
					q = "reduce intelligence";
					break;
				}
				case RBE_LOSE_WIS:
				{
					q = "reduce wisdom";
					break;
				}
				case RBE_LOSE_DEX:
				{
					q = "reduce dexterity";
					break;
				}
				case RBE_LOSE_CON:
				{
					q = "reduce constitution";
					break;
				}
				case RBE_LOSE_CHR:
				{
					q = "reduce charisma";
					break;
				}
				case RBE_LOSE_ALL:
				{
					q = "reduce all stats";
					break;
				}
				case RBE_SHATTER:
				{
					q = "shatter";
					break;
				}
				case RBE_EXP_10:
				{
					q = "lower experience (by 10d6+)";
					break;
				}
				case RBE_EXP_20:
				{
					q = "lower experience (by 20d6+)";
					break;
				}
				case RBE_EXP_40:
				{
					q = "lower experience (by 40d6+)";
					break;
				}
				case RBE_EXP_80:
				{
					q = "lower experience (by 80d6+)";
					break;
				}
				case RBE_DISEASE:
				{
					q = "disease";
					break;
				}
				case RBE_TIME:
				{
					q = "distrupt the time continuum";
					break;
				}
				case RBE_EXP_VAMP:
				{
					q = "drain life force";
					break;
				}
			}


			if (!k)
			{
				spoil_out(wd_che[msex]);
				spoil_out(" can ");
			}
			else if (k < i - 1)
			{
				spoil_out(", ");
			}
			else
			{
				spoil_out(", and ");
			}

			/* Describe the method */
			spoil_out(p);

			/* Describe the effect, if any */
			if (r_ptr->blow[j].effect)
			{
				spoil_out(" to ");
				spoil_out(q);
				if (r_ptr->blow[j].d_dice && r_ptr->blow[j].d_side)
				{
					spoil_out(" with damage");
					if (r_ptr->blow[j].d_side == 1)
						spoil_out(" %d", (int)r_ptr->blow[j].d_dice);
					else
						spoil_out(" %dd%d",
								(int)r_ptr->blow[j].d_dice,
								(int)r_ptr->blow[j].d_side);
				}
			}

			k++;
		}

		if (k)
		{
			spoil_out(".  ");
		}
		else if (flags1 & (RF0_NEVER_BLOW))
		{
			spoil_out("%s has no physical attacks.  ", wd_che[msex]);
		}

		spoil_out(NULL);
	}

	/* Free the "who" array */
	KILL(who);

	/* Check for errors */
	if (ferror(fff))
	{
		msgf("Cannot close spoiler file.");
		return;
	}

	my_fclose(fff);

	msgf("Successfully created a spoiler file.");
}


/*
 * Abbreviations of damaged stats
 */
static cptr long_stat_names[A_MAX] =
{
	"Strength",
	"Intelligence",
	"Wisdom",
	"Dexterity",
	"Constitution",
	"Charisma"
};


/*
 * Create a spoiler file for nutations
 */
static void spoil_mutation(cptr fname)
{
	int i;
	char buf[1024];

	const mutation_type *mut_ptr;

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msgf("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */
	spoiler_underline("Mutation Spoilers for " VERSION_NAME " Version " VERSION_STRING);
	spoiler_blanklines(1);

	for (i = 0; i < MUT_PER_SET * 3; i++)
	{
		mut_ptr = &mutations[i];

		/* Headers */
		if (i == 0)
		{
			/* Activatable mutations */
			spoiler_underline("The activatable mutations");
			spoiler_blanklines(1);
		}
		else if (i == MUT_PER_SET)
		{
			/* Random mutations */
			spoiler_underline("Randomly activating mutations");
			spoil_out(NULL);
		}
		else if (i == MUT_PER_SET * 2)
		{
			/* Other mutations */
			spoiler_underline("Other mutations");
			spoil_out(NULL);
		}

		/* Describe mutation */
		spoil_out("%s \n", mut_ptr->desc_text);

		/* Type 1? */
		if (i < MUT_PER_SET)
		{
			spoil_out("- Activation: %s \n", mut_ptr->name);

			spoil_out("- Min. level: %d \n", (int)mut_ptr->level);

			spoil_out("- HP/SP Cost: %d \n", mut_ptr->cost);

			spoil_out("- Statistic : %s \n", long_stat_names[mut_ptr->stat]);

			spoil_out("- Difficulty: %d \n", mut_ptr->diff);
		}

		/* Type 2? */
		else if (i < MUT_PER_SET * 2)
		{
			if (mut_ptr->chance > 0)
			{
				spoil_out("- Chance/turn: 1-in-%d\n", mut_ptr->chance * 100);
			}
		}

		spoiler_blanklines(1);
	}

	/* Check for errors */
	if (ferror(fff))
	{
		msgf("Cannot close spoiler file.");
		return;
	}

	my_fclose(fff);

	/* Message */
	msgf("Successfully created a spoiler file.");
}


/*
 * Create a spoiler file for artifacts
 */
static void spoil_rac_pow(cptr fname)
{
	int i;
	char buf[1024];

	const mutation_type *mut_ptr;

	/* Build the filename */
	path_make(buf, ANGBAND_DIR_USER, fname);

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "w");

	/* Oops */
	if (!fff)
	{
		msgf("Cannot create spoiler file.");
		return;
	}

	/* Dump the header */
	spoiler_underline("Racial Powers Spoilers for " VERSION_NAME " Version " VERSION_STRING);
	spoiler_blanklines(1);

	/* The Racial Powers */
	spoiler_underline("The Racial Powers");
	spoiler_blanklines(1);

	for (i = 0; i < MAX_RACE_POWERS; i++)
	{
		mut_ptr = &race_powers[i];

		/* Describe power */
		rp_ptr = &race_info[mut_ptr->which];
		spoiler_underline(rp_ptr->title);

		spoil_out("%s \n", mut_ptr->desc_text);

		spoil_out("- Activation: %s \n", mut_ptr->name);

		spoil_out("- Min. level: %d \n", (int)mut_ptr->level);

		spoil_out("- HP/SP Cost: %d \n", mut_ptr->cost);

		spoil_out("- Statistic : %3s \n", long_stat_names[mut_ptr->stat]);

		spoil_out("- Difficulty: %d \n", mut_ptr->diff);

		spoiler_blanklines(1);
	}

	/* Check for errors */
	if (ferror(fff))
	{
		msgf("Cannot close spoiler file.");
		return;
	}

	my_fclose(fff);

	/* Message */
	msgf("Successfully created a spoiler file.");
}


/*
 * Forward declare
 */
extern void do_cmd_spoilers(void);

/*
 * Create Spoiler files -BEN-
 */
void do_cmd_spoilers(void)
{
	int i;


	/* Save the screen */
	screen_save();


	/* Interact */
	while (1)
	{
		/* Clear screen */
		Term_clear();

		/* Info */
		prtf(0, 2, "Create a spoiler file.");

		/* Prompt for a file */
		prtf(5, 5, "(1) Brief Object Info (obj-desc.spo)");
		prtf(5, 6, "(2) Brief Artifact Info (artifact.spo)");
		prtf(5, 7, "(3) Brief Monster Info (mon-desc.spo)");
		prtf(5, 8, "(4) Full Monster Info (mon-info.spo)");
		prtf(5, 9, "(5) Brief Mutation Info (mutation.spo)");
		prtf(5, 10, "(6) Brief Racial Powers Info (rac-pow.spo)");

		/* Prompt */
		prtf(0, 12, "Command: ");

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
			spoil_obj_desc("obj-desc.spo");
		}

		/* Option (2) */
		else if (i == '2')
		{
			spoil_artifact("artifact.spo");
		}

		/* Option (3) */
		else if (i == '3')
		{
			spoil_mon_desc("mon-desc.spo");
		}

		/* Option (4) */
		else if (i == '4')
		{
			spoil_mon_info("mon-info.spo");
		}

		/* Option (5) */
		else if (i == '5')
		{
			spoil_mutation("mutation.spo");
		}

		/* Option (6) */
		else if (i == '6')
		{
			spoil_rac_pow("rac-pow.spo");
		}

		/* Oops */
		else
		{
			bell("Illegal command for spoilers!");
		}

		/* Flush messages */
		message_flush();
	}


	/* Restore the screen */
	screen_load();
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif /* MACINTOSH */

#endif
