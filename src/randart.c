/* File: randart.c */


/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#include "init.h"

/*
 * Random artifact generator (randart) by Greg Wooledge.
 *
 * Taken from the version used in angband a2.9.1 and modified to include the
 * new EyAngband item types and abilities by Eytan Zweig, and further modified
 * to reflect some of the changes in angband 2.9.2.
 *
 * At some point some stylistic modifications will be desirable, in particular,
 * involving the order of functions, the use of spaces before function parens,
 * the use of "do/while".
 */

#ifdef GJW_RANDART

#define NAMES_FILE "names.txt"

#define MAX_TRIES 200
#define BUFLEN 1024

#define MIN_NAME_LEN 5
#define MAX_NAME_LEN 9
#define S_WORD 26
#define E_WORD S_WORD

typedef struct rand_sv_type rand_sv_type;
typedef struct rand_tv_type rand_tv_type;
typedef struct acts_list_type acts_list_type;

struct rand_sv_type
{
	int prob2;				/* Probability of choice */
	int sv;					/* Object sval */
};

struct rand_tv_type
{
	int prob1;				/* Probability of choice */
	int tv;					/* Object tval */
	int limit;				/* Need this many of these types */
	int num;				/* Number of objects (placeholder) */
	rand_sv_type *typ;		/* Pointer to array of possible svals */
};

struct acts_list_type
{
	int act;
	int chance;
	int time;
};

/*
 * Artifact types - missile weapons
 */
static rand_sv_type rand_bows[] =
{
	{ 3, SV_SLING },
	{ 10, SV_SHORT_BOW },
	{ 30, SV_LONG_BOW },
	{ 45, SV_LIGHT_XBOW },
	{ -1, SV_HEAVY_XBOW }
};

/*
 * Artifact types - digging tools
 */
static rand_sv_type rand_diggers[] =
{
	{ 10, SV_SHOVEL },
	{ 25, SV_PICK },
	{ 50, SV_GNOMISH_SHOVEL },
	{ 75, SV_ORCISH_PICK },
	{ 110, SV_DWARVEN_PICK },
	{ -1, SV_MATTOCK},
};

/*
 * Artifact types - "blunt" weapons
 */
static rand_sv_type rand_blunts[] =
{
	{ 6, SV_WOODEN_CLUB},
	{ 12, SV_WHIP },
	{ 19, SV_MACE },
	{ 28, SV_WAR_HAMMER },
	{ 32, SV_QUARTERSTAFF },
	{ 36, SV_LUCERN_HAMMER },
	{ 42, SV_MORNING_STAR },
	{ 48, SV_FLAIL },
	{ 59, SV_LEAD_FILLED_MACE },
	{ 80, SV_BALL_AND_CHAIN },
	{ 120, SV_TWO_HANDED_FLAIL },
	{ -1, SV_MACE_OF_DISRUPTION }
};

/*
 * Artifact types - long, sharp-edged weapons
 */
static rand_sv_type rand_swords[] =
{
	{ 5, SV_DAGGER },
	{ 9, SV_MAIN_GAUCHE },
	{ 10, SV_RAPIER }, /* or at least pointy ;-) */
	{ 13, SV_SHORT_SWORD },
	{ 15, SV_SABRE },
	{ 17, SV_CUTLASS },
	{ 19, SV_TULWAR },
	{ 22, SV_BROAD_SWORD },
	{ 26, SV_LONG_SWORD },
	{ 30, SV_SCIMITAR },
	{ 45, SV_BASTARD_SWORD },
	{ 60, SV_KATANA },
	{ 90, SV_TWO_HANDED_SWORD },
	{ 110, SV_EXECUTIONERS_SWORD },
	{ 113, SV_BLADE_OF_STABILITY },
	{ 117, SV_BLACKROCK_SWORD },
	{ -1, SV_BLADE_OF_CHAOS }
};


/*
 * Artifact types - weapons that are not blunt or sword-shaped
 */
static rand_sv_type rand_polearms[] =
{
	{ 12, SV_SPEAR },
	{ 20, SV_TRIDENT },
	{ 27, SV_LANCE },
	{ 35, SV_AWL_PIKE },
	{ 45, SV_PIKE },
	{ 50, SV_BEAKED_AXE },
	{ 55, SV_BROAD_AXE },
	{ 60, SV_BATTLE_AXE },
	{ 65, SV_GLAIVE },
	{ 80, SV_HALBERD },
	{ 115, SV_GREAT_AXE },
	{ 123, SV_SCYTHE },
	{ 130, SV_LOCHABER_AXE },
	{ 135, SV_BLACKROCK_SPEAR },
	{ -1, SV_SCYTHE_OF_SLICING }
};


/*
 * Artifact types - light or hard body armor
 */
static rand_sv_type rand_body_armor[] =
{
	{ 0, SV_FILTHY_RAG },
	{ 5, SV_ROBE },
	{ 10, SV_SOFT_LEATHER_ARMOR },
	{ 15, SV_SOFT_STUDDED_LEATHER },
	{ 20, SV_HARD_LEATHER_ARMOR },
	{ 30, SV_HARD_STUDDED_LEATHER },
	{ 45, SV_LEATHER_SCALE_MAIL },
	/* { 55, SV_RUSTY_CHAIN_MAIL },  <-  no rusty chain mail artifacts */
	{ 60, SV_METAL_SCALE_MAIL },
	{ 70, SV_CHAIN_MAIL },
	{ 80, SV_AUGMENTED_CHAIN_MAIL },
	{ 87, SV_DOUBLE_CHAIN_MAIL },
	{ 95, SV_BAR_CHAIN_MAIL },
	{ 100, SV_METAL_BRIGANDINE_ARMOUR },
	{ 110, SV_PARTIAL_PLATE_ARMOUR },
	{ 120, SV_METAL_LAMELLAR_ARMOUR },
	{ 130, SV_FULL_PLATE_ARMOUR },
	{ 140, SV_RIBBED_PLATE_ARMOUR },
	{ 150, SV_MITHRIL_CHAIN_MAIL },
	{ 165, SV_WIZARD_ROBE },
	{ 170, SV_MITHRIL_PLATE_MAIL },
	{ -1, SV_ADAMANTITE_PLATE_MAIL }
};


/*
 * Artifact types - shoes
 */
static rand_sv_type rand_boots[] =
{
	{ 3, SV_PAIR_OF_LEATHER_SANDALS},
	{ 9, SV_PAIR_OF_SOFT_LEATHER_BOOTS },
	{ 15, SV_PAIR_OF_HARD_LEATHER_BOOTS },
	{ 25, SV_PAIR_OF_SPIKED_LEATHER_BOOTS },
	{ -1, SV_PAIR_OF_METAL_SHOD_BOOTS }
};


/*
 * Artifact types - gloves
 */
static rand_sv_type rand_gloves[] =
{
	{ 10, SV_SET_OF_LEATHER_GLOVES },
	{ 15, SV_SET_OF_SILK_GLOVES },
	{ 35, SV_SET_OF_GAUNTLETS },
	{ -1, SV_SET_OF_CESTI }
};


/*
 * Artifact types - headgear
 */
static rand_sv_type rand_hats[] =
{
	{ 9, SV_HARD_LEATHER_CAP },
	{ 20, SV_METAL_CAP },
	{ 40, SV_IRON_HELM },
	{ 50, SV_STEEL_HELM },
	{ 60, SV_KNIGHTS_HELM },

	{ 75, SV_IRON_CROWN },
	{ 95, SV_GOLDEN_CROWN },
	{ -1, SV_JEWELED_CROWN }
};


/*
 * Artifact types - shields
 */
static rand_sv_type rand_shields[] =
{
	{ 3, SV_WOODEN_SHIELD },
	{ 12, SV_SMALL_LEATHER_SHIELD },
	{ 22, SV_SMALL_METAL_SHIELD },
	{ 43, SV_LARGE_LEATHER_SHIELD },
	{ 61, SV_LARGE_METAL_SHIELD },
	{ -1, SV_SHIELD_OF_DEFLECTION },
};


/*
 * Artifact types - cloaks
 */
static rand_sv_type rand_cloaks[] =
{
	{ 75, SV_CLOAK },
	{ 90, SV_FUR_CLOAK },
	{100, SV_ELVEN_CLOAK },
	{ -1, SV_SHADOW_CLOAK }
};


/*
 * The array of normal artifact types
 * See 'choose_item' and 'scramble_restart' for more details.
 */
static rand_tv_type rand_type[] =
{
	{ 5,	TV_BOW,			3, 0, &rand_bows[0]},
	{ 9,	TV_DIGGING,		1, 0, &rand_diggers[0]},
	{ 19,	TV_HAFTED,		5, 0, &rand_blunts[0]},
	{ 33,	TV_SWORD,		5, 0, &rand_swords[0]},
	{ 42,	TV_POLEARM,		5, 0, &rand_polearms[0]},
	{ 64,	TV_BODY_ARMOR,	5, 0, &rand_body_armor[0]},
	{ 71,	TV_BOOTS,		4, 0, &rand_boots[0]},
	{ 78,	TV_GLOVES,		4, 0, &rand_gloves[0]},
	{ 87,	TV_HEADGEAR,	4, 0, &rand_hats[0]},
	{ 94,	TV_SHIELD,		4, 0, &rand_shields[0]},
	{ 100,	TV_CLOAK,		4, 0, &rand_cloaks[0]},
	{ 0,	0,				0, 0, NULL }
};


static long lprobs[S_WORD+1][S_WORD+1][S_WORD+1];	/* global, hence init to 0 */
static long ltotal[S_WORD+1][S_WORD+1];		/* global, hence init to 0 */

/* Temporary space for names, while reading and randomizing them. */
static cptr *names;

/*
 * Cache the results of lookup_kind(), which is expensive and would
 * otherwise be called much too often.
 */
static s16b *kinds;

/*
 * Calculate the multiplier we'll get with a given bow type. 
 */
static int bow_multiplier(int sval)
{
	switch (sval)
	{
		case SV_SLING:
		case SV_SHORT_BOW:
			return (2);
		case SV_LONG_BOW:
		case SV_LIGHT_XBOW:
			return (3);
		case SV_HEAVY_XBOW:
			return (4);
		default:
			message_format(MSG_FAIL, 0, "Illegal bow sval %s\n", sval);
	}

	return 0;
}

/*
 * Use W. Sheldon Simms' random name generator.  This function builds
 * probability tables which are used later on for letter selection.  It
 * relies on the ASCII character set.
 */
static void build_prob(FILE *learn)
{
	int c_prev, c_cur, c_next;

	/* Build raw frequencies */
	while (TRUE)
	{
		c_prev = c_cur = S_WORD;

		do
		{
			c_next = getc(learn);
		} while (!isalpha(c_next) && (c_next != EOF));
		if (c_next == EOF) break;

		do
		{
			c_next = A2I(tolower(c_next));
			lprobs[c_prev][c_cur][c_next]++;
			ltotal[c_prev][c_cur]++;
			c_prev = c_cur;
			c_cur = c_next;
			c_next = getc(learn);
		} while (isalpha(c_next));

		lprobs[c_prev][c_cur][E_WORD]++;
		ltotal[c_prev][c_cur]++;
	}
}

/*
 * Use W. Sheldon Simms' random name generator.  Generate a random word using
 * the probability tables we built earlier.  Relies on the ASCII character
 * set.  Relies on European vowels (a, e, i, o, u).  The generated name should
 * be copied/used before calling this function again.
 */
static char *make_word(void)
{
	static char word_buf[90];
	int r, totalfreq;
	int tries, lnum, vow;
	int c_prev, c_cur, c_next;
	char *cp;

startover:
	vow = 0;
	lnum = 0;
	tries = 0;
	cp = word_buf;
	c_prev = c_cur = S_WORD;

	while (TRUE)
	{
	    getletter:
		c_next = 0;
		r = rand_int(ltotal[c_prev][c_cur]);
		totalfreq = lprobs[c_prev][c_cur][c_next];
		while (totalfreq <= r)
		{
			c_next++;
			totalfreq += lprobs[c_prev][c_cur][c_next];
		}

		if (c_next == E_WORD)
		{
			if ((lnum < MIN_NAME_LEN) || vow == 0)
			{
				tries++;
				if (tries < 10) goto getletter;
				goto startover;
			}
			*cp = '\0';
			break;
		}
		if (lnum >= MAX_NAME_LEN) goto startover;

		*cp = I2A(c_next);
		if (is_a_vowel(*cp)) vow++;

		cp++;
		lnum++;
		c_prev = c_cur;
		c_cur = c_next;
	}

	word_buf[0] = toupper(word_buf[0]);

	return (word_buf);
}


/*
 * Use W. Sheldon Simms' random name generator.
 */
static errr init_names(void)
{
	char buf[BUFLEN];
	size_t name_size;
	char *a_base;
	char *a_next;
	int i;
	FILE *f;

	path_build(buf, BUFLEN, ANGBAND_DIR_FILE, NAMES_FILE);
	if ((f = my_fopen(buf, "r")) == NULL)
	{
		message_format(MSG_FAIL, 0, "could not open '" NAMES_FILE "'\n");
		return 1;
	}
	build_prob(f);
	my_fclose(f);

	for (i = 0; i < z_info->a_max; i++)
	{
		char *word = make_word();

		if (rand_int(3) == 0)
			sprintf(buf, "'%s'", word);
		else
			sprintf(buf, "of %s", word);
		names[i] = string_make(buf);
	}

	/* Return the "proper" names for the non-randomizable artifacts */
	for (i = 1; i < z_info->a_max; i++)
	{
		if (a_info[i].flags3 & TR3_DONT_RANDOMIZE) 
		{
			string_free(names[i-1]);
			if ((names[i - 1] = string_make(a_name + a_info[i].name)) == NULL)
			{	
				message_format(MSG_FAIL, 0, "Memory allocation error");
				return 1;
			}
		}
	}

	/* Convert our names array into an a_name structure for later use. */
	name_size = 0;

	for (i = 1; i < z_info->a_max; i++)
	{
		name_size += strlen(names[i-1]) + 2;	/* skip first char */
	}

	if ((a_base = malloc(name_size)) == NULL)
	{
		message_format(MSG_FAIL, 0, "Memory allocation error");
		return 1;
	}

	a_next = a_base + 1;	/* skip first char */

	for (i = 1; i < z_info->a_max; i++)
	{
		strcpy(a_next, names[i-1]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen(names[i-1]) + 1;
	}

	/* Free some of our now unneeded memory. */
	C_KILL(a_name, a_head.name_size, char);

	for (i = 0; i < z_info->a_max; i++)
	{
		string_free(names[i]);
	}

	/* Store the names */
	a_name = a_base;
	a_head.name_ptr = a_base;
	a_head.name_size = name_size;

	/* Success */
	return (0);
}

/*
 * Randomly select a base item type (tval,sval).  Assign the various fields
 * corresponding to that choice.
 */
static void choose_item(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int tval, sval;
	object_kind *k_ptr;
	int r;
	s16b k_idx, r2;
	byte target_level;
	int i, j;

	/* 
	 * Look up the original artifact's base object kind to get level and
	 * rarity information to supplement the artifact level/rarity.  As a
	 * degenerate case consider Bladeturner, which has artifact lvl/rar
	 * of only 95/3, but which is based on an object with 110/64! 
	 */
	k_idx = kinds[a_idx];
 	k_ptr = &k_info[k_idx];
	target_level = k_ptr->level;

	/* 
	 * Add base object kind's rarity to artifact rarity.  Later we will
	 * subtract the new object kind's rarity. 
	 */
	a_ptr->rarity += k_ptr->chance[0];

	/*
	 * Pick a category (tval) of weapon randomly.  Within each tval, roll
	 * an sval (specific item) based on the target level.  The number we
	 * roll should be a bell curve.  The mean and standard variation of the
	 * bell curve are based on the target level; the distribution of
	 * kinds versus the bell curve is hand-tweaked. :-( 
	 */

	/* Start over */
	i = 0;

	/* Roll for tval */
	r = rand_int(100);

	/* Get the table entry */
	while (r >= rand_type[i].prob1) i++;

	/* Start the sval index */
	j = 0;

	/* Roll for sval */
	r2 = Rand_normal(target_level * 2, target_level);

	/* Extract the tval */
	tval = rand_type[i].tv;

	/* Get the table entry (treat zero specially) */
	while ((r2 >= rand_type[i].typ[j].prob2) &&
	       (rand_type[i].typ[j].prob2 >= 0)) j++;

	/* Extract the sval */
	sval = rand_type[i].typ[j].sv;

	k_idx = lookup_kind(tval, sval);
	k_ptr = &k_info[k_idx];
	kinds[a_idx] = k_idx;

	/* Subtract the new object kind's rarity (see above).  We can't
	   blindly subtract, because a_ptr->rarity is a byte. */
	if (a_ptr->rarity <= k_ptr->chance[0])
		a_ptr->rarity = 1;
	else
		a_ptr->rarity -= k_ptr->chance[0];

	a_ptr->prefix_idx = 0;
	a_ptr->tval = k_ptr->tval;
	a_ptr->sval = k_ptr->sval;
	a_ptr->pval = k_ptr->pval;
	a_ptr->to_h = k_ptr->to_h;
	a_ptr->to_d = k_ptr->to_d;
	a_ptr->to_a = k_ptr->to_a;
	a_ptr->ac = k_ptr->ac;
	a_ptr->dd = k_ptr->dd;
	a_ptr->ds = k_ptr->ds;
	a_ptr->weight = k_ptr->weight;
	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;
	a_ptr->flags4 = k_ptr->flags4;
	a_ptr->flags3 |= TR3_IGNORE_ELEM;
	a_ptr->flags3 |= TR3_IGNORE_NON_ELEM;

	/* Assign basic stats to the artifact based on its artifact level. */
	switch (a_ptr->tval)
	{
		case TV_HAFTED:
		case TV_SWORD:
		case TV_POLEARM:
		{
			/* Try to assign a prefix (one try) */
			int pfx;
			item_prefix_type *px_ptr;
			bool okay = FALSE;

			/* No "bad" prefixes */
			do 
			{
				pfx = randint(z_info->px_max - 1);
				px_ptr = &px_info[pfx];
				
				/* Check for legality */
				if (px_ptr->flags & PXF_ARTIFACT) okay = TRUE;
				else okay = FALSE;

				/* Check for material constraints */
				if (px_ptr->material)
				{
					if (px_ptr->material != k_info[lookup_kind(a_ptr->tval, a_ptr->sval)].material)
						okay = FALSE;
				}
			} while (!okay);

			/* Relatively good chance for a prefix */
			if (rand_int((px_ptr->rarity / 5) + 2) == 0) a_ptr->prefix_idx = pfx;
			
			/* Fall through */
		}
		case TV_BOW: 
		case TV_DIGGING: 
		{
			a_ptr->to_h += (s16b)(a_ptr->level / 10 + rand_int(4) + rand_int(4));
			a_ptr->to_d += (s16b)(a_ptr->level / 10 + rand_int(4));
			a_ptr->to_d += (s16b)(rand_int((a_ptr->dd * a_ptr->ds) / 2 + 1));

			break;
		}
		case TV_BODY_ARMOR:
		{
			/*
			 * Make sure armor gets some resists!  Hard body armor
			 * is generally high-level stuff, with good ac and
			 * to_a.  That sucks up all the points....
			 */
			if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_ACID;
			if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_ELEC;
			if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_COLD;
			if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_FIRE;

			/* Fall through */
		}
		case TV_BOOTS:
		case TV_GLOVES: 
		case TV_HEADGEAR: 
		case TV_SHIELD: 
		case TV_CLOAK: 
		{
			a_ptr->to_a += (s16b)(a_ptr->level / 10 + a_ptr->ac / 3 + rand_int(8));
			if (a_ptr->to_a < 10) a_ptr->to_a += (s16b)(2 + rand_int(4) + rand_int(4));

			break;
		}
	}
}

/*
 * We've just added an ability which uses the pval bonus.  Make sure it's
 * not zero.  If it's currently negative, leave it negative (heh heh).
 */
static void do_pval(artifact_type *a_ptr)
{
	if (a_ptr->pval == 0) a_ptr->pval = (s16b)(1 + rand_int(3));
	else if (a_ptr->pval < 0)
	{
		if (rand_int(2) == 0) a_ptr->pval--;
	}
	else if (rand_int(3) > 0) a_ptr->pval++;
}

/*
 * Randomly select an extra ability to be added to the artifact in question.
 * XXX - This function is way too large.
 */
static void add_ability(artifact_type *a_ptr)
{
	int r;

	r = rand_int(2);
	if (r < 1)		/* Pick something dependent on item type. */
	{
		r = rand_int(100);
		switch (a_ptr->tval)
		{
			case TV_BOW:
			{
				if (r < 15)
				{
					a_ptr->flags1 |= TR1_SHOTS;
					do_pval(a_ptr);
				}
				else if (r < 35)
				{
					a_ptr->flags1 |= TR1_MIGHT;
					do_pval(a_ptr);
				}
				else if (r < 65) a_ptr->to_h += (s16b)(2 + rand_int(2));
				else a_ptr->to_d += (s16b)(2 + rand_int(3));

				break;
			}
			case TV_DIGGING:
			case TV_HAFTED:
			case TV_POLEARM:
			case TV_SWORD:
			{
				if (r < 3)
				{
					a_ptr->flags1 |= TR1_WIS;
					do_pval(a_ptr);
					if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_WIS;
					if (a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM)
						a_ptr->flags4 |= TR4_BLESSED;
				}
				else if (r < 6)
				{
					a_ptr->flags4 |= TR4_BRAND_ACID;
					if (rand_int(4) > 0) a_ptr->flags2 |= TR2_RES_ACID;
				}
				else if (r < 9)
				{
					a_ptr->flags4 |= TR4_BRAND_ELEC;
					if (rand_int(4) > 0) a_ptr->flags2 |= TR2_RES_ELEC;
				}
				else if (r < 13)
				{
					a_ptr->flags4 |= TR4_BRAND_FIRE;
					if (rand_int(4) > 0) a_ptr->flags2 |= TR2_RES_FIRE;
				}
				else if (r < 17)
				{
					a_ptr->flags4 |= TR4_BRAND_COLD;
					if (rand_int(4) > 0) a_ptr->flags2 |= TR2_RES_COLD;
				}
				else if (r < 18)
				{
					a_ptr->flags4 |= TR4_BRAND_POIS;
					if (rand_int(3) > 0) a_ptr->flags2 |= TR2_RES_POIS;
				}
				else if (r < 20)
				{
					a_ptr->flags4 |= TR4_BRAND_LITE;
					if (rand_int(4) > 0) a_ptr->flags2 |= TR2_RES_DARK;
				}
				else if (r < 20)
				{
					a_ptr->flags4 |= TR4_BRAND_DARK;
					if (rand_int(4) > 0) a_ptr->flags2 |= TR2_RES_LITE;
				}
				else if (r < 28)
				{
					a_ptr->dd += (byte)(1 + rand_int(2) + rand_int(2));
					if (a_ptr->dd > 9) a_ptr->dd = 9;
				}
				else if (r < 30) a_ptr->flags4 |= TR4_KILL_DRAGON;
				else if (r < 33) a_ptr->flags4 |= TR4_SLAY_DRAGON;
				else if (r < 37) 
				{
					a_ptr->flags4 |= TR4_SLAY_EVIL;
					if (rand_int(4) == 0) a_ptr->flags4 |= TR4_SLAY_CHAOS;
				}
				else if (r < 40) 
				{
					a_ptr->flags4 |= TR4_SLAY_CHAOS;
					if (rand_int(4) == 0) a_ptr->flags4 |= TR4_SLAY_CHAOS;
				}
				else if (r < 44)
				{
					a_ptr->flags4 |= TR4_SLAY_ANIMAL;
					if (rand_int(3) == 0) a_ptr->flags4 |= TR4_SLAY_PLANT;
				}
				else if (r < 48) 
				{
					a_ptr->flags4 |= TR4_SLAY_PLANT;
					if (rand_int(3) == 0) a_ptr->flags4 |= TR4_SLAY_ANIMAL;
				}
				else if (r < 52)
				{
					a_ptr->flags4 |= TR4_SLAY_UNDEAD;
					if (rand_int(3) == 0) a_ptr->flags4 |= TR4_SLAY_DEMON;
				}
				else if (r < 55)
				{
					a_ptr->flags4 |= TR4_SLAY_DEMON;
					if (rand_int(3) == 0) a_ptr->flags4 |= TR4_SLAY_UNDEAD;
				}
				else if (r < 60)
				{
					a_ptr->flags4 |= TR4_SLAY_PERSON;
					if (rand_int(4) == 0) a_ptr->flags4 |= TR4_SLAY_HUMANOID;
				}
				else if (r < 67)
				{
					a_ptr->flags4 |= TR4_SLAY_HUMANOID;
					if (rand_int(4) == 0) a_ptr->flags4 |= TR4_SLAY_PERSON;
				}
				else if (r < 72) a_ptr->flags3 |= TR3_SEE_INVIS;
				else if (r < 76)
				{
					if (a_ptr->pval < 0) break;
					a_ptr->flags1 |= TR1_BLOWS;
					do_pval(a_ptr);
				}
				else if (r < 89)
				{
					a_ptr->to_d += (s16b)(3 + rand_int(4));
					a_ptr->to_h += (s16b)(3 + rand_int(4));
				}
				else if (r < 92) a_ptr->to_a += (s16b)(3 + rand_int(3));
				else if (r < 94) a_ptr->weight = (a_ptr->weight * 9) / 10;
				else if (r < 97) a_ptr->flags4 |= TR4_TERROR;
				else if (r < 98) a_ptr->flags4 |= TR4_WOUNDING;
				else 
					if (a_ptr->tval != TV_DIGGING)
					{
						a_ptr->flags1 |= TR1_TUNNEL;
						do_pval(a_ptr);
					}

				break;
			}

			case TV_BOOTS:
			{
				if (r < 10) a_ptr->flags3 |= TR3_FEATHER;
				else if (r < 50) a_ptr->to_a += (s16b)(2 + rand_int(4));
				else if (r < 80)
				{
					a_ptr->flags1 |= TR1_STEALTH;
					do_pval(a_ptr);
				}
				else if (r < 90)
				{
					a_ptr->flags1 |= TR1_SPEED;
					if (a_ptr->pval < 0) break;
					if (a_ptr->pval == 0) a_ptr->pval = (s16b)(3 + rand_int(8));
					else if (rand_int(2) == 0) a_ptr->pval++;
				}
				else a_ptr->weight = (a_ptr->weight * 9) / 10;
				break;
			}
			case TV_GLOVES:
			{
				if (r < 25) a_ptr->flags2 |= TR2_FREE_ACT;
				else if (r < 50)
				{
					a_ptr->flags1 |= TR1_DEX;
					do_pval(a_ptr);
				}
				else if (r < 75) a_ptr->to_a += (s16b)(3 + rand_int(3));
				else
				{
					a_ptr->to_h += (s16b)(2 + rand_int(3));
					a_ptr->to_d += (s16b)(2 + rand_int(3));
					a_ptr->flags3 |= TR3_SHOW_MODS;
				}
				break;
			}
			case TV_HEADGEAR:
			{
				if (r < 13) a_ptr->flags2 |= TR2_NO_BLIND;
				else if (r < 17) a_ptr->flags2 |= TR2_RES_LITE;
				else if (r < 21) a_ptr->flags2 |= TR2_RES_DARK;
				else if (r < 37) a_ptr->flags3 |= TR3_TELEPATHY;
				else if (r < 60) a_ptr->flags3 |= TR3_SEE_INVIS;
				else if (r < 70)
				{
					a_ptr->flags1 |= TR1_WIS;
					do_pval(a_ptr);
				}
				else if (r < 80)
				{
					a_ptr->flags1 |= TR1_INT;
					do_pval(a_ptr);
				}
				else a_ptr->to_a += (s16b)(3 + rand_int(3));
				break;
			}
			case TV_SHIELD:
			{
				if (r < 18) a_ptr->flags2 |= TR2_RES_ACID;
				else if (r < 36) a_ptr->flags2 |= TR2_RES_ELEC;
				else if (r < 54) a_ptr->flags2 |= TR2_RES_FIRE;
				else if (r < 72) a_ptr->flags2 |= TR2_RES_COLD;
				else if (r < 80) a_ptr->flags2 |= TR2_RES_WATER;
				else a_ptr->to_a += (s16b)(3 + rand_int(3));
				break;
			}
			case TV_CLOAK:
			{
				if (r < 1)
				{
					a_ptr->flags3 |= TR3_INVIS;
				}
				if (r < 50)
				{
					a_ptr->flags1 |= TR1_STEALTH;
					do_pval(a_ptr);
				}
				else a_ptr->to_a += (s16b)(3 + rand_int(3));
				break;
			}
			case TV_BODY_ARMOR:
			{
				if (r < 8)
				{
					a_ptr->flags1 |= TR1_STEALTH;
					do_pval(a_ptr);
				}
				else if (r < 16) a_ptr->flags2 |= TR2_HOLD_LIFE;
				else if (r < 22)
				{
					a_ptr->flags1 |= TR1_CON;
					do_pval(a_ptr);
					if (rand_int(2) == 0)
						a_ptr->flags1 |= TR1_SUST_CON;
				}
				else if (r < 34) a_ptr->flags2 |= TR2_RES_ACID;
				else if (r < 46) a_ptr->flags2 |= TR2_RES_ELEC;
				else if (r < 58) a_ptr->flags2 |= TR2_RES_FIRE;
				else if (r < 70) a_ptr->flags2 |= TR2_RES_COLD;
				else if (r < 80)
					a_ptr->weight = (a_ptr->weight * 9) / 10;
				else a_ptr->to_a += (s16b)(3 + rand_int(3));
				break;
			}
		}
	}
	else			/* Pick something universally useful. */
	{
		r = rand_int(53);
		switch (r)
		{
			case 0:
				a_ptr->flags1 |= TR1_STR;
				do_pval(a_ptr);
				if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_STR;
				break;
			case 1:
				a_ptr->flags1 |= TR1_INT;
				do_pval(a_ptr);
				if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_INT;
				break;
			case 2:
				a_ptr->flags1 |= TR1_WIS;
				do_pval(a_ptr);
				if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_WIS;
				if (a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM)
					a_ptr->flags4 |= TR4_BLESSED;
				break;
			case 3:
				a_ptr->flags1 |= TR1_DEX;
				do_pval(a_ptr);
				if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_DEX;
				break;
			case 4:
				a_ptr->flags1 |= TR1_CON;
				do_pval(a_ptr);
				if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_CON;
				break;
			case 5:
				a_ptr->flags1 |= TR1_CHR;
				do_pval(a_ptr);
				if (rand_int(2) == 0) a_ptr->flags1 |= TR1_SUST_CHR;
				break;
			case 6:
				a_ptr->flags1 |= TR1_STEALTH;
				do_pval(a_ptr);
				break;
			case 7:
				a_ptr->flags1 |= TR1_SEARCH;
				do_pval(a_ptr);
				break;
			case 8:
				a_ptr->flags1 |= TR1_INFRA;
				do_pval(a_ptr);
				break;
			case 9:
				a_ptr->flags1 |= TR1_MANA;
				do_pval(a_ptr);
				break;
			case 10:
				a_ptr->flags1 |= TR1_HEALTH;
				do_pval(a_ptr);
				break;
			case 11:
				a_ptr->flags1 |= TR1_SPEED;
				if (a_ptr->pval == 0) a_ptr->pval = (s16b)(3 + rand_int(3));
				else do_pval(a_ptr);
				break;
			case 12:
				if (rand_int(3) == 0)
				{
					a_ptr->flags1 |= TR1_BLOWS;
					do_pval(a_ptr);
				}
				break;
			case 13:
				a_ptr->flags1 |= TR1_SUST_STR;
				if (rand_int(2) == 0)
				{
					a_ptr->flags1 |= TR1_STR;
					do_pval(a_ptr);
				}
				break;
			case 14:
				a_ptr->flags1 |= TR1_SUST_INT;
				if (rand_int(2) == 0)
				{
					a_ptr->flags1 |= TR1_INT;
					do_pval(a_ptr);
				}
				break;
			case 15:
				a_ptr->flags1 |= TR1_SUST_WIS;
				if (rand_int(2) == 0)
				{
					a_ptr->flags1 |= TR1_WIS;
					do_pval(a_ptr);
					if (a_ptr->tval == TV_SWORD || a_ptr->tval == TV_POLEARM)
						a_ptr->flags4 |= TR4_BLESSED;
				}
				break;
			case 16:
				a_ptr->flags1 |= TR1_SUST_DEX;
				if (rand_int(2) == 0)
				{
					a_ptr->flags1 |= TR1_DEX;
					do_pval(a_ptr);
				}
				break;
			case 17:
				a_ptr->flags1 |= TR1_SUST_CON;
				if (rand_int(2) == 0)
				{
					a_ptr->flags1 |= TR1_CON;
					do_pval(a_ptr);
				}
				break;
			case 18:
				a_ptr->flags1 |= TR1_SUST_CHR;
				if (rand_int(2) == 0)
				{
					a_ptr->flags1 |= TR1_CHR;
					do_pval(a_ptr);
				}
				break;

			case 19:
				if (rand_int(3) == 0) a_ptr->flags2 |= TR2_IM_ACID;
				break;
			case 20:
				if (rand_int(3) == 0) a_ptr->flags2 |= TR2_IM_ELEC;
				break;
			case 21:
				if (rand_int(4) == 0) a_ptr->flags2 |= TR2_IM_FIRE;
				break;
			case 22:
				if (rand_int(3) == 0) a_ptr->flags2 |= TR2_IM_COLD;
				break;
			case 23: a_ptr->flags2 |= TR2_FREE_ACT; break;
			case 24: a_ptr->flags2 |= TR2_HOLD_LIFE; break;
			case 25: a_ptr->flags2 |= TR2_BRAVERY; break;
			case 26: a_ptr->flags2 |= TR2_NO_BLIND; break;

			case 27: a_ptr->flags2 |= TR2_RES_ACID; break;
			case 28: a_ptr->flags2 |= TR2_RES_ELEC; break;
			case 29: a_ptr->flags2 |= TR2_RES_FIRE; break;
			case 30: a_ptr->flags2 |= TR2_RES_COLD; break;

			case 31: a_ptr->flags2 |= TR2_RES_POIS; break;
			case 32: a_ptr->flags2 |= TR2_RES_DISEASE; break;
			case 33: a_ptr->flags2 |= TR2_RES_LITE; break;
			case 34: a_ptr->flags2 |= TR2_RES_DARK; break;
			case 35: a_ptr->flags2 |= TR2_RES_WATER; break;
			case 36: a_ptr->flags2 |= TR2_RES_CONFU; break;
			case 37: a_ptr->flags2 |= TR2_RES_SOUND; break;
			case 38: a_ptr->flags2 |= TR2_RES_SHARD; break;
			case 39:
				if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_NETHR;
				break;
			case 40: a_ptr->flags2 |= TR2_RES_NEXUS; break;
			case 41: a_ptr->flags2 |= TR2_RES_CHAOS; break;
			case 42:
				if (rand_int(2) == 0) a_ptr->flags2 |= TR2_RES_DISEN;
				else a_ptr->flags3 |= TR3_IGNORE_DISEN;
				break;
			case 43:
				if (rand_int(3) == 0) a_ptr->flags2 |= TR2_RES_TIME;
				break;
			case 44:
				if (rand_int(5) == 0) a_ptr->flags2 |= TR2_RES_MANA;
				break;
			case 45: a_ptr->flags3 |= TR3_FEATHER; break;
			case 46: a_ptr->flags3 |= TR3_GLOW; break;
			case 47: a_ptr->flags3 |= TR3_LUCK; break;
			case 48: a_ptr->flags3 |= TR3_SEE_INVIS; break;
			case 49:
				if (rand_int(3) == 0) a_ptr->flags3 |= TR3_TELEPATHY;
				break;
			case 50: a_ptr->flags3 |= TR3_SLOW_DIGEST; break;
			case 51: a_ptr->flags3 |= TR3_REGEN; break;
			case 52:
				if (rand_int(4) == 0) a_ptr->flags3 |= TR3_INVIS;
				break;
		}
	}
}

/*
 * Evaluate the artifact's overall power level.
 */
static s32b artifact_power(int a_idx)
{
	const artifact_type *a_ptr = &a_info[a_idx];
	s32b p = 0;
	s16b k_idx;
	object_kind *k_ptr;
	int immunities = 0;

	/* Try to use the cache */
	k_idx = kinds[a_idx];

	/* Lookup the item if not yet cached */
	if (!k_idx)
	{
		k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Cache the object index */
		kinds[a_idx] = k_idx;
 
 		/* Paranoia */
 		if (!k_idx)
 		{
 			quit_fmt("Illegal tval/sval value for artifact %d!", a_idx);
 		}
	}

	k_ptr = &k_info[k_idx];

	if (a_idx >= z_info->a_min_normal)
	{
		/* Start with a "power" rating derived from the base item's level. */
		p = (k_ptr->level + 7) / 8;
	}

	/* Evaluate certain abilities based on type of object. */
	switch (a_ptr->tval)
	{
		case TV_BOW:
		{
			int mult;

			p += (a_ptr->to_d + SGN(a_ptr->to_d)) / 2;
			mult = bow_multiplier(a_ptr->sval);
			if (a_ptr->flags1 & TR1_MIGHT)
			{
				if (a_ptr->pval > 3)
				{
					p += 20000;	/* inhibit */
					mult = 1;	/* don't overflow */
				}
				else
					mult += a_ptr->pval;
			}
			p *= mult;
			if (a_ptr->flags1 & TR1_SHOTS)
			{
				if (a_ptr->pval > 3)
					p += 20000;	/* inhibit */
				else if (a_ptr->pval > 0)
					p *= (2 * a_ptr->pval);
			}
			p += (a_ptr->to_h + 3 * SGN(a_ptr->to_h)) / 4;
			if (a_ptr->weight < k_ptr->weight) p++;
			break;
		}
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:
		{
			p += (a_ptr->dd * a_ptr->ds + 1) / 2;
			if (a_ptr->flags4 & TR4_SLAY_ANIMAL)	p = (p * 4) / 3;
			if (a_ptr->flags4 & TR4_SLAY_PLANT)		p = (p * 6) / 5;
			if (a_ptr->flags4 & TR4_SLAY_EVIL)		p = (p * 3) / 2;
			if (a_ptr->flags4 & TR4_SLAY_CHAOS)		p = (p * 3) / 2;
			if (a_ptr->flags4 & TR4_KILL_DRAGON)	p = (p * 3) / 2;
			if (a_ptr->flags4 & TR4_SLAY_UNDEAD)	p = (p * 4) / 3;
			if (a_ptr->flags4 & TR4_SLAY_DRAGON)	p = (p * 4) / 3;
			if (a_ptr->flags4 & TR4_SLAY_HUMANOID)  p = (p * 4) / 3;
			if (a_ptr->flags4 & TR4_SLAY_DEMON)		p = (p * 5) / 4;
			if (a_ptr->flags4 & TR4_SLAY_PERSON)	p = (p * 5) / 4;

			if (a_ptr->flags4 & TR4_WOUNDING)    p = (p * 2);
			if (a_ptr->flags4 & TR4_TERROR)      p = (p * 4) / 3;

			if (a_ptr->flags4 & TR4_BRAND_ACID)  p = p * 2;
			if (a_ptr->flags4 & TR4_BRAND_ELEC)  p = (p * 3) / 2;
			if (a_ptr->flags4 & TR4_BRAND_FIRE)  p = (p * 4) / 3;
			if (a_ptr->flags4 & TR4_BRAND_COLD)  p = (p * 4) / 3;
			if (a_ptr->flags4 & TR4_BRAND_POIS)  p = p * 2;
			if (a_ptr->flags4 & TR4_BRAND_LITE)  p = (p * 3) / 2;
			if (a_ptr->flags4 & TR4_BRAND_DARK)  p = (p * 3) / 2;
			if (a_ptr->flags4 & TR4_BLESSED)	 p += 5;

			p += (a_ptr->to_d + 2 * SGN(a_ptr->to_d)) / 3;
			if (a_ptr->to_d > 15) p += (a_ptr->to_d - 14) / 2;

			if ((a_ptr->flags1 & TR1_TUNNEL) && (a_ptr->tval != TV_DIGGING)) p += a_ptr->pval * 3;

			p += (a_ptr->to_h + 3 * SGN(a_ptr->to_h)) / 4;

			/* Remember, weight is in 0.1 lb. units. */
			if (a_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - a_ptr->weight) / 20;

			break;
		}
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HEADGEAR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BODY_ARMOR:
		{
			p += (a_ptr->ac + 4 * SGN(a_ptr->ac)) / 5;
			p += (a_ptr->to_h + SGN(a_ptr->to_h)) / 2;
			p += (a_ptr->to_d + SGN(a_ptr->to_d)) / 2;
			if (a_ptr->weight != k_ptr->weight)
				p += (k_ptr->weight - a_ptr->weight) / 30;
			break;
		}
		case TV_LITE:
		case TV_LITE_SPECIAL:
		{
			if (a_ptr->flags3 & TR3_LITE1) p -= 1;
			if (a_ptr->flags3 & TR3_LITE2) p += 5;
			if (a_ptr->flags3 & TR3_LITE3) p += 10; 
			if (a_ptr->flags3 & TR3_LITE4) p += 15;
			break;
		}
		case TV_RING:
		case TV_AMULET:
		{
			p += 20;
			break;
		}
	}

	/* Other abilities are evaluated independent of the object type. */
	p += (a_ptr->to_a + 3 * SGN(a_ptr->to_a)) / 4;
	if (a_ptr->to_a > 20) p += (a_ptr->to_a - 19) / 2;
	if (a_ptr->to_a > 30) p += (a_ptr->to_a - 29) / 2;
	if (a_ptr->to_a > 40) p += 20000;	/* inhibit */

	if (a_ptr->pval > 0)
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON)p += a_ptr->pval * a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval * a_ptr->pval;
	}
	else if (a_ptr->pval < 0)	/* hack: don't give large negatives */
	{
		if (a_ptr->flags1 & TR1_STR) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_INT) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_WIS) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_DEX) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_CON) p += a_ptr->pval;
		if (a_ptr->flags1 & TR1_STEALTH) p += a_ptr->pval;
	}
	if (a_ptr->flags1 & TR1_CHR) p += a_ptr->pval;
	if (a_ptr->flags1 & TR1_INFRA) p += (a_ptr->pval + SGN(a_ptr->pval)) / 2;
	if (a_ptr->flags1 & TR1_SPEED) p += (a_ptr->pval * 3) / 2;

	if (a_ptr->flags1 & TR1_BLOWS)
	{
		if (a_ptr->pval > 3) p += 20000;	/* inhibit */
		else if (a_ptr->pval > 0) p = (p * 6) / (4 - a_ptr->pval);
	}
			
	if (a_ptr->flags1 & TR1_MANA)   p += (a_ptr->pval * 4);
	if (a_ptr->flags1 & TR1_HEALTH) p += (a_ptr->pval * 5);

	if (a_ptr->flags1 & TR1_SUST_STR) p += 6;
	if (a_ptr->flags1 & TR1_SUST_INT) p += 4;
	if (a_ptr->flags1 & TR1_SUST_WIS) p += 4;
	if (a_ptr->flags1 & TR1_SUST_DEX) p += 4;
	if (a_ptr->flags1 & TR1_SUST_CON) p += 4;
	if (a_ptr->flags1 & TR1_SUST_CHR) p += 1;
	if (a_ptr->flags2 & TR2_IM_ACID)
	{
		p += 20;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_ELEC)
	{
		p += 24;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_FIRE)
	{
		p += 36;
		immunities++;
	}
	if (a_ptr->flags2 & TR2_IM_COLD)
	{
		p += 24;
		immunities++;
	}
	if (immunities > 1) p += 16;
	if (immunities > 2) p += 16;
	if (immunities > 3) p += 20000;		/* inhibit */
	if (a_ptr->flags2 & TR2_RES_ACID)    p += 6;
	if (a_ptr->flags2 & TR2_RES_ELEC)    p += 6;
	if (a_ptr->flags2 & TR2_RES_FIRE)    p += 6;
	if (a_ptr->flags2 & TR2_RES_COLD)    p += 6;
	if (a_ptr->flags2 & TR2_RES_LITE)    p += 8;
	if (a_ptr->flags2 & TR2_RES_CONFU)   p += 8;
	if (a_ptr->flags2 & TR2_RES_SHARD)   p += 8;
	if (a_ptr->flags2 & TR2_FREE_ACT)    p += 8;
	if (a_ptr->flags2 & TR2_BRAVERY)     p += 8;
	if (a_ptr->flags2 & TR2_HOLD_LIFE)   p += 10;
	if (a_ptr->flags2 & TR2_RES_DARK)    p += 10;
	if (a_ptr->flags2 & TR2_NO_BLIND)    p += 10;
	if (a_ptr->flags2 & TR2_RES_SOUND)   p += 10;
	if (a_ptr->flags2 & TR2_RES_NEXUS)   p += 10;
	if (a_ptr->flags2 & TR2_RES_WATER)   p += 10;
	if (a_ptr->flags2 & TR2_RES_POIS)    p += 12;
	if (a_ptr->flags2 & TR2_RES_DISEASE) p += 12;
	if (a_ptr->flags2 & TR2_RES_NETHR)	 p += 12;
	if (a_ptr->flags2 & TR2_RES_CHAOS)   p += 12;
	if (a_ptr->flags2 & TR2_RES_DISEN)   p += 14;
	if (a_ptr->flags2 & TR2_RES_TIME)    p += 18;
	if (a_ptr->flags2 & TR2_RES_MANA)    p += 22;

	if (a_ptr->flags3 & TR3_FEATHER)	 p += 2;
	if (a_ptr->flags3 & TR3_SLOW_DIGEST) p += 4;
	if (a_ptr->flags3 & TR3_SEE_INVIS)	 p += 8;
	if (a_ptr->flags3 & TR3_REGEN)		 p += 8;
	if (a_ptr->flags3 & TR3_GLOW)		 p += 8;
	if (a_ptr->flags3 & TR3_LUCK)		 p += 12;
	if (a_ptr->flags3 & TR3_TELEPATHY)	 p += 20;
	if (a_ptr->flags3 & TR3_INVIS)		 p += 20;

	if (a_ptr->flags3 & TR3_LIGHT_CURSE) p -= 4;
	if (a_ptr->flags3 & TR3_AGGRAVATE)   p -= 8;
	if (a_ptr->flags3 & TR3_DRAIN_EXP)	 p -= 14;
	if (a_ptr->flags3 & TR3_DISRUPT)	 p -= 16;
	if (a_ptr->flags3 & TR3_TELEPORT)	 p -= 20;
	if (a_ptr->flags3 & TR3_DRAIN_ITEM)  p -= 20;
	if (a_ptr->flags3 & TR3_HEAVY_CURSE) p -= 20;
/*	if (a_ptr->flags3 & TR3_PERMA_CURSE) p -= 40; */

	if (a_ptr->flags3 & TR3_IGNORE_DISEN) p += 2 ;

	return (p);
}

static void remove_contradictory(artifact_type *a_ptr)
{
	/* Contradictory flags */
	if ((a_ptr->tval == TV_LITE) || (a_ptr->tval == TV_LITE_SPECIAL)) 
		a_ptr->flags3 &= ~(TR3_GLOW);                                                                
	if (a_ptr->flags3 & TR3_AGGRAVATE)   a_ptr->flags1 &= ~(TR1_STEALTH);
	if (a_ptr->flags3 & TR3_LIGHT_CURSE) a_ptr->flags4 &= ~(TR4_BLESSED);
	if (a_ptr->flags3 & TR3_DRAIN_EXP)   a_ptr->flags2 &= ~(TR2_HOLD_LIFE);
	if (a_ptr->flags3 & TR3_DRAIN_ITEM)  a_ptr->flags2 &= ~(TR2_RES_DISEN);

	/* Redundant flags */
	if (a_ptr->flags2 & TR2_RES_DISEN)   a_ptr->flags3 &= ~(TR3_IGNORE_DISEN);
	if (a_ptr->flags2 & TR2_IM_ACID)	 a_ptr->flags2 &= ~(TR2_RES_ACID);
	if (a_ptr->flags2 & TR2_IM_ELEC)	 a_ptr->flags2 &= ~(TR2_RES_ELEC);
	if (a_ptr->flags2 & TR2_IM_FIRE)	 a_ptr->flags2 &= ~(TR2_RES_FIRE);
	if (a_ptr->flags2 & TR2_IM_COLD)	 a_ptr->flags2 &= ~(TR2_RES_COLD);
	if (a_ptr->flags1 & TR4_KILL_DRAGON) a_ptr->flags4 &= ~(TR4_SLAY_DRAGON);

	/* Never on a bad item flags */
	if (a_ptr->pval < 0)
	{
		if (a_ptr->flags1 & TR1_STR) a_ptr->flags1 &= ~(TR1_SUST_STR);
		if (a_ptr->flags1 & TR1_INT) a_ptr->flags1 &= ~(TR1_SUST_INT);
		if (a_ptr->flags1 & TR1_WIS) a_ptr->flags1 &= ~(TR1_SUST_WIS);
		if (a_ptr->flags1 & TR1_DEX) a_ptr->flags1 &= ~(TR1_SUST_DEX);
		if (a_ptr->flags1 & TR1_CON) a_ptr->flags1 &= ~(TR1_SUST_CON);
		if (a_ptr->flags1 & TR1_CHR) a_ptr->flags1 &= ~(TR1_SUST_CHR);
		a_ptr->flags1 &= ~(TR1_BLOWS);
	}
}

/*
 * Make it bad, or if it's already bad, make it worse!
 */
static void do_curse(artifact_type *a_ptr)
{
	if (rand_int(3) == 0) a_ptr->flags3 |= TR3_AGGRAVATE;
	if (rand_int(5) == 0) a_ptr->flags3 |= TR3_DRAIN_EXP;
	if (rand_int(7) == 0) a_ptr->flags3 |= TR3_TELEPORT;
	if (rand_int(7) == 0) a_ptr->flags3 |= TR3_DISRUPT;
	if (rand_int(8) == 0) a_ptr->flags3 |= TR3_DRAIN_ITEM;

	if ((a_ptr->pval > 0) && (rand_int(2) == 0)) a_ptr->pval = -a_ptr->pval;
	if ((a_ptr->to_a > 0) && (rand_int(2) == 0)) a_ptr->to_a = -a_ptr->to_a;
	if ((a_ptr->to_h > 0) && (rand_int(2) == 0)) a_ptr->to_h = -a_ptr->to_h;
	if ((a_ptr->to_d > 0) && (rand_int(4) == 0)) a_ptr->to_d = -a_ptr->to_d;

	if (a_ptr->flags3 & TR3_LIGHT_CURSE)
	{
		if (rand_int(2) == 0) a_ptr->flags3 |= TR3_HEAVY_CURSE;
		return;
	}

	a_ptr->flags3 |= TR3_LIGHT_CURSE;
	if (rand_int(4) == 0) a_ptr->flags3 |= TR3_HEAVY_CURSE;
}

#define POSSIBLE_ACTS	168

/* List of possible activations */
acts_list_type acts_list[POSSIBLE_ACTS] =
{
	{ POW_HEAL_2D10,		1,	 3 },
	{ POW_HEAL_4D10,		1,	 5 },
	{ POW_HEAL_6D10,		1,	10 },
	{ POW_HEAL_8D10,		1,	25 },
	{ POW_HEAL_300,			2, 100 },
	{ POW_HEAL_500,			2, 150 },
	{ POW_HEAL_1000,		3, 200 },
	{ POW_HEAL_2000,		4, 300 },
	{ POW_HEAL_CURE_6D8,	2, 100 },
	{ POW_HEAL_CURE_300,	3, 200 },
	{ POW_HEAL_CURE_1200,	5, 350 },
	{ POW_LIFE,				8, 400 },
	{ POW_RESTORE_MANA,		3, 200 },
	{ POW_RESTORE_STATS,	6, 250 },
	{ POW_RESTORE_LEVEL,	4, 200 },
	{ POW_RESTORE_ALL,		8, 400 },
	{ POW_CURE_FEAR,		1,	 5 },
	{ POW_CURE_CONFUSION,	1,	 5 },
	{ POW_CURE_DISEASE,		1,	10 },
	{ POW_CURE_POISON_2,	1,	10 },
	{ POW_CURE_POIS_DISE,	2,	50 },
	{ POW_CURE_FEAR_POIS,	1,	15 },
	{ POW_CURE_ALL,			4, 120 },
	{ POW_CURE_BODY,		2, 100 },
	{ POW_CLEAR_MIND,		2,  80 },
	{ POW_TELE_10,			1,	 3 },
	{ POW_TELE_MINOR,		2,	50 },
	{ POW_TELE_MAJOR,		2,	50 },
	{ POW_TELE_OTHER,		3, 100 },
	{ POW_TELE_LEVEL,		2,	50 },
	{ POW_TELE_CONTROL,		3,	50 },
	{ POW_WORD_RECALL,		3, 250 },
	{ POW_ALTER_REALITY,	3, 250 },
	{ POW_ARROW,			2,	20 },
	{ POW_BOLT_MISSILE_1,	1,	10 },
	{ POW_BOLT_ELEC,		1,	10 },
	{ POW_BOLT_FROST_1,		1,	12 },
	{ POW_BOLT_FROST_2,		1,	15 },
	{ POW_BOLT_ACID_1,		1,	15 },
	{ POW_BOLT_ACID_2,		2,	25 },
	{ POW_BOLT_FIRE_1,		1,	20 },
	{ POW_BOLT_FIRE_2,		2,	30 },
	{ POW_BOLT_SOUND,		2,	30 },
	{ POW_BOLT_FORCE_1,		2,	33 },
	{ POW_BOLT_FORCE_2,		3,	50 },
	{ POW_BOLT_LITE,		1,	15 },
	{ POW_BOLT_DARK,		1,	15 },
	{ POW_BOLT_WATER,		1,	25 },
	{ POW_BOLT_MANA,		3,	50 },
	{ POW_BEAM_WEAK_LITE,	2,	25 },
	{ POW_BEAM_NETHER,		8, 500 },
	{ POW_BALL_POISON_1,	1,	 6 },
	{ POW_BALL_POISON_2,	3, 150 },
	{ POW_BALL_ACID,		3, 150 },
	{ POW_BALL_ELEC_1,		2, 150 },
	{ POW_BALL_ELEC_2,		5, 250 },
	{ POW_BALL_FIRE_1,		3, 200 },
	{ POW_BALL_FIRE_2,		5, 250 },
	{ POW_BALL_FROST_1,		2, 100 },
	{ POW_BALL_FROST_2,		3, 150 },
	{ POW_BALL_FROST_3,		5, 250 },
	{ POW_BALL_SOUND,		3, 150 },
	{ POW_BALL_MANA,		5, 350 },
	{ POW_STAR_BEAM_W_LITE,	5, 250 },
	{ POW_STAR_BALL_ELEC,	5, 300 },
	{ POW_BANISH,			5, 250 },
	{ POW_BLIGHT,			5, 250 },
	{ POW_BURST_ASTRAL,		7, 250 },
	{ POW_DRAIN_LIFE_1,		2,  50 },
	{ POW_DRAIN_LIFE_2,		3,  75 },
	{ POW_DRAIN_LIFE_3,		5, 150 },
	{ POW_DISPEL_ALL,		8, 600 },
	{ POW_DISPEL_UNDEAD_1,	2, 150 },
	{ POW_DISPEL_UNDEAD_2,	3, 250 },
	{ POW_DISPEL_NON_EVIL,	4, 200 },
	{ POW_DISPEL_EVIL_3,	3, 100 },
	{ POW_DISPEL_EVIL_4,	4, 200 },
	{ POW_DISPEL_EVIL_5,	5, 300 },
	{ POW_GENOCIDE,			6, 400 },
	{ POW_MASS_GENOCIDE,    5, 400 },
	{ POW_EARTHQUAKE,		3, 200 },
	{ POW_DESTRUCTION,		5, 200 },
	{ POW_LIGHT_AREA,		1,  10 },
	{ POW_DARK_AREA,		1,   5 },
	{ POW_DETECT_MONSTERS,	1,	50 },
	{ POW_DETECT_EVIL,		1,	50 },
	{ POW_DETECT_INVIS,		1,	50 },
	{ POW_DETECT_TRAP,		1,	50 },
	{ POW_DETECT_TREASURE,	1,	30 },
	{ POW_DETECT_DOOR_STAIR,1,	50 },
	{ POW_DETECT_TRAP_DOOR,	1,	50 },
	{ POW_DETECT_ITEM,		1,	50 },
	{ POW_DETECT_ENCHANT,	3, 150 },
	{ POW_DETECT_ALL,		5, 150 },
	{ POW_ABSORB_HIT,		5, 250 },
	{ POW_BLESS_1,			1,	10 },
	{ POW_BLESS_2,			1,	50 },
	{ POW_BLESS_3,			1, 100 },
	{ POW_HEROISM,			1,	50 },
	{ POW_RAGE_1,			2, 100 },
	{ POW_RAGE_2,			4, 200 },
	{ POW_RAGE_BLESS_RESIST,6, 400 },
	{ POW_SHIELD,			1,  50 },
	{ POW_INVIS_1,			5, 300 },
	{ POW_INVIS_2,			7, 400 },
	{ POW_RESILIENCE,		8, 750 },
	{ POW_INFRAVISION,		1, 100 },
	{ POW_SEE_INVIS,		2, 100 },
	{ POW_PROT_EVIL,		5, 250 },
	{ POW_HASTE_SELF_1,		3, 100 },
	{ POW_HASTE_SELF_2,		4, 200 },
	{ POW_HASTE_SELF_3,		5, 300 },
	{ POW_DISARM,			2,  50 },
	{ POW_DEST_TRAP_DOOR_1, 2,  50 },
	{ POW_DEST_TRAP_DOOR_2, 3, 100 },
	{ POW_STONE_TO_MUD,		1,  20 },
	{ POW_CREATE_DOOR,      2,  20 },
	{ POW_CREATE_STAIR,		2,  50 },
	{ POW_AGGRAVATE_SAFE,   1,   5 },
	{ POW_CONFUSE_MONSTER,	1,  10 },
	{ POW_CONFUSE_ALL,		3,  50 },
	{ POW_SLEEP_MONSTER,	1,  10 },
	{ POW_SLEEP_ADJACENT,	2,	30 },
	{ POW_SLEEP_ALL,		3,  60 },
	{ POW_SLOW_MONSTER,		2,  30 },
	{ POW_SLOW_ALL,			3,  60 },
	{ POW_CALM_MONSTER,		1,	30 },
	{ POW_CALM_ANIMALS,		2,  50 },
	{ POW_CALM_NON_EVIL,	2, 100 },
	{ POW_CALM_NON_CHAOS,	3, 250 },
	{ POW_CALM_ALL,			6, 500 },
	{ POW_BLIND_MONSTER,	1,  50 },
	{ POW_SCARE_MONSTER,	1,  10 },
	{ POW_SCARE_UNDEAD,		2,	50 },
	{ POW_SCARE_ALL,		3, 150 },
	{ POW_CALL_MONSTER,		2,  30 },
	{ POW_POLY_MONSTER,		2, 150 },
	{ POW_SATISFY_HUNGER,	3, 750 },
	{ POW_RECHARGE_1,		2,  50 },
	{ POW_RECHARGE_2,		3,  50 },
	{ POW_RECHARGE_3,		4, 100 },
	{ POW_RECHARGE_4,		6, 200 },
	{ POW_IDENTIFY,			2,  50 },
	{ POW_IDENTIFY_PACK,	5, 500 },
	{ POW_RES_FIRE,			1,  60 },
	{ POW_RES_COLD,			1,  60 },
	{ POW_RES_FIRE_COLD,	1, 100 },
	{ POW_RES_ACID_ELEC,	1, 100 },
	{ POW_RES_POISON,		1, 100 },
	{ POW_RES_DISEASE,		1, 100 },
	{ POW_RES_SOUND,		1, 100 },
	{ POW_RES_ELEMENTS,		4, 100 },
	{ POW_RES_GREATER,		8, 500 },
	{ POW_RESISTANCE,		6, 150 },
	{ POW_REMOVE_CURSE_1,	4, 999 },
	{ POW_REMOVE_CURSE_2,	5, 999 },
	{ POW_MAP_1,			3,  50 },
	{ POW_MAP_2,			4,  75 },
	{ POW_MAP_3,			5, 100 },	
	{ POW_PROBE,			3,  20 },
	{ POW_SELF_KNOW,		6, 500 },
	{ POW_BRAND_ARROW_ANML,	4, 999 },
	{ POW_BRAND_ARROW_WOUND,4, 999 },
	{ POW_BRAND_ARROW_ELMNT,4, 999 },
	{ POW_BRAND_BOLT_FIRE,	4, 999 },
	{ POW_BRAND_BOLT_LITE,	4, 999 },
	{ POW_BRAND_SHOT_POIS,	4, 999 },
	{ POW_BRAND_SHOT_HOLY,	4, 999 }
};

/*
 * Do activation 
 */
static void do_activation(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int i;

	while (TRUE)
	{
		i = rand_int(POSSIBLE_ACTS);
		if (rand_int(acts_list[i].chance) == 0) break;
	}

	/* Assign activation times */
	a_ptr->activation = acts_list[i].act;
	a_ptr->time = (acts_list[i].time / 2) + randint(acts_list[i].time);
	if (rand_int(2) == 0)
	{ 
		a_ptr->randtime = a_ptr->time;
	}
	else
	{
		a_ptr->randtime = 0;
		a_ptr->time = (a_ptr->time * 3) / 2;
	}

	/* Never too long */
	if ((a_ptr->time + a_ptr->randtime) > 1000)
	{
		a_ptr->time = 999;
		a_ptr->randtime = 0;
	}
}

/*
 * Scramble the artifacts. 
 */
static void scramble_artifact(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	s32b power;
	int tries;
	s32b ap;
	bool curse_me = FALSE;
	bool aggravate_me = FALSE;

	/* Special cases -- don't randomize these! */
	if (a_ptr->flags3 & TR3_DONT_RANDOMIZE)	return;

	/* Skip unused artifacts, too! */
	if (a_ptr->tval == 0) return;

	/* Evaluate the original artifact to determine the power level. */
	power = artifact_power(a_idx);
	if (power < 0) curse_me = TRUE;

	if (cheat_wizard)
		message_format(MSG_CHEAT, 0, "Artifact %d: power = %ld", a_idx, (long)power);

	/* Really powerful items should aggravate. */
	if (power > 100)
	{
		if (rand_int(100) < (power - 100) * 3)
		{
			aggravate_me = TRUE;
		}
	}

#if 0
	/* XXX XXX XXX Try to give an activation */
	if (rand_int(100) < ((a_idx >= z_info->a_min_normal) ? 45 : 80))
	{
		do_activation(a_idx);
	}
	else
	{
		a_ptr->activation = 0;
		a_ptr->time = 0;
		a_ptr->randtime = 0;
	}
#endif

	if (a_idx >= z_info->a_min_normal)
	{
		/* Normal artifact - choose a random base item type.  Not too
		   powerful, so we'll have to add something to it.  Not too
		   weak, for the opposite reason. */
		int count = 0;
		s32b ap2;
		do
		{
			choose_item(a_idx);
			ap2 = artifact_power(a_idx);
			count++;
		} while ((count < MAX_TRIES) &&
			   ((ap2 > (power * 8) / 10 + 1) ||
			    (ap2 < (power / 10))));
	}
	else
	{
		/* 
		 * Special artifact (light source, ring, or amulet). 
		 * Clear the following fields; leave the rest alone. 
		 */
		a_ptr->pval = 0;
		a_ptr->to_h = a_ptr->to_d = a_ptr->to_a = 0;
		a_ptr->flags1 = a_ptr->flags2 = 0;
		a_ptr->flags3 = (TR3_IGNORE_ELEM | TR3_IGNORE_NON_ELEM);

		/* Give lites a light radius */
		if ((a_ptr->tval == TV_LITE) || (a_ptr->tval == TV_LITE_SPECIAL))
		{
			int n = rand_int(10);
			if (a_ptr->tval == TV_LITE_SPECIAL) n += 1; 

			if (n < 1) a_ptr->flags3 |= TR3_LITE1;
			else if (n < 3) a_ptr->flags3 |= TR3_LITE2;
			else if (n < 8) a_ptr->flags3 |= TR3_LITE3;
			else a_ptr->flags3 |= TR3_LITE4;
		}

	}

	/* First draft: add two abilities, then curse it three times. */
	if (curse_me)
	{
		add_ability(a_ptr);
		add_ability(a_ptr);
		do_curse(a_ptr);
		do_curse(a_ptr);
		do_curse(a_ptr);
		remove_contradictory(a_ptr);
		ap = artifact_power(a_idx);
	}
	else
	{
		/* Select a random set of abilities which roughly matches the
		   original's in terms of overall power/usefulness. */
		for (tries = 0; tries < MAX_TRIES; tries++)
		{
			artifact_type a_old;

			/* Copy artifact info temporarily. */
			a_old = *a_ptr;
			add_ability(a_ptr);
			remove_contradictory(a_ptr);
			ap = artifact_power(a_idx);
			if (ap > (power * 11) / 10 + 1)
			{	/* too powerful -- put it back */
				*a_ptr = a_old;
				continue;
			}

			else if (ap >= (power * 9) / 10)	/* just right */
			{
				break;
			}

			/* Stop if we're going negative, so we don't overload
			   the artifact with great powers to compensate. */
			else if ((ap < 0) && (ap < (-(power * 1)) / 10))
			{
				break;
			}
		}		/* end of power selection */

		if (aggravate_me)
		{
			a_ptr->flags3 |= TR3_AGGRAVATE;
			remove_contradictory(a_ptr);
			ap = artifact_power(a_idx);
		}
	}

	a_ptr->cost = ap * 1000L;

	if (a_ptr->cost < 0) a_ptr->cost = 0;
}

/*
 * Return FALSE if the whole set of random artifacts meets certain
 * criteria.  Return TRUE if we fail to meet those criteria (which will
 * restart the whole process).
 */
static bool scramble_restart(void)
{
	int i, j;
	bool restart_flag = FALSE;

	/* Reset limits */
	for (j = 0; rand_type[j].prob1 > 0; j++)
	{
		rand_type[j].num = rand_type[j].limit;
	}

	/* Scan the artifacts */
	for (i = z_info->a_min_normal; i < z_info->a_max; i++)
	{
		/* Ignore zero tval artifacts (important) */
		if (!a_info[i].tval) continue;

		for (j = 0; rand_type[j].prob1 > 0; j++)
		{
			/* Found? */
			if (a_info[i].tval == rand_type[j].tv)
			{
				/* Decrement counter */
				rand_type[j].num--;

				/* Next artifact */
				break;
			}
		}
	}

	/* Any remaining? */
	for (j = 0; rand_type[j].prob1 > 0; j++)
	{
		if (rand_type[j].num > 0) 
		{
			restart_flag = TRUE;
			break;
		}
	}

	if ((cheat_wizard) && (restart_flag))
	{
		message_format(MSG_CHEAT, 0, 
			"Restarting generation process, not enough type %d artifacts", rand_type[j].tv);
	}

	/* Return failure */
	return (restart_flag);
}

static errr scramble(void)
{
	/* If our artifact set fails to meet certain criteria, we start over. */
	while (TRUE)
	{
		int a_idx;

		/* Generate all the artifacts. */
		for (a_idx = 1; a_idx < z_info->a_max; a_idx++)
		{
			scramble_artifact(a_idx);
		}

		/* Check to see if all is ok */
		if (!scramble_restart()) break;
	} 

	/* Success */
	return (0);
}

static errr do_randart_aux(bool full)
{
	errr result;

	/* Generate random names */
	if ((result = init_names()) != 0) return (result);

	if (full)
	{
		/* Randomize the artifacts */
		if ((result = scramble()) != 0) return (result);
	}

	/* Success */
	return (0);
}

/*
 * Randomize the artifacts
 *
 * The full flag toggles between just randomizing the names and
 * complete randomization of the artifacts.
 */
errr do_randart(u32b randart_seed, bool full)
{
	errr err;

	/* Prepare to use the Angband "simple" RNG.  We want this to follow
	   the precedents set by the town layout and object flavor code. */
	Rand_value = randart_seed;
	Rand_quick = TRUE;

	C_MAKE(names, z_info->a_max, cptr);
	C_MAKE(kinds, z_info->a_max, s16b);

	/* Generate the random artifact (names) */
	err = do_randart_aux(full);

	/* When done, resume use of the Angband "complex" RNG.  Right
	   now this does nothing, but later it will be important! */
	Rand_quick = FALSE;

	C_FREE((void*)names, z_info->a_max, cptr);
	C_KILL(kinds, z_info->a_max, s16b);

	return (err);
}

#else /* GJW_RANDART */

#ifdef MACINTOSH
static int i = 0;
#endif /* MACINTOSH */

#endif /* GJW_RANDART */
