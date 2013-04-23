/* File: random.c */

/*
 * Copyright (c) 1998
 * Eric Bock
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#define MAX_ELF_V		9		/* Elvish vowels */
#define MAX_DWA_V		5		/* Dwarven vowels */
#define MAX_MAN_V		7		/* Mannish vowels */
#define MAX_BLK_V		4		/* Black Speech vowels */

#define MAX_ELF_C		20		/* Elvish consonants */
#define MAX_DWA_C		16		/* Dwarven consonants */
#define MAX_MAN_C		20		/* Mannish consonants */
#define MAX_BLK_C		21		/* Black Speech consonants */

#define MAX_ELF_R		12		/* Elvish rules */
#define MAX_DWA_R		11		/* Dwarven rules */
#define MAX_MAN_R		17		/* Mannish rules */
#define MAX_BLK_R		6		/* Black Speech rules */

/*
 * The data for the gibberish routine.
 * It is extremely easy to create a new template.
 * It is very difficult to make it look good ;)
 */
static gibber_type elf_vowel[MAX_ELF_V] =
{
	{"a", 32}, {"e", 21}, {"i", 23}, {"o", 9},
	{"u", 9}, {"ai", 3}, {"au", 1}, {"oi", 1},
	{"ui", 1}
};

static gibber_type dwarf_vowel[MAX_DWA_V] =
{
	{"a", 48}, {"e", 6}, {"i", 17}, {"o", 2},
	{"u", 28}
};

static gibber_type man_vowel[MAX_MAN_V] =
{
	{"a", 51}, {"e", 4}, {"i", 22}, {"o", 6},
	{"u", 15}, {"ai", 2}
};

static gibber_type black_vowel[MAX_BLK_V] =
{
	{"a", 41}, {"i", 12}, {"o", 7}, {"u", 39}
};

static gibber_type elf_cons[MAX_ELF_C] =
{
	{"b", 1}, {"c", 1}, {"d", 4}, {"f", 1},
	{"h", 1}, {"l", 16}, {"m", 10}, {"n", 19},
	{"p", 1}, {"qu", 1}, {"r", 18}, {"s", 6},
	{"t", 8}, {"v", 7}, {"w", 1}, {"y", 5}
};

static gibber_type dwarf_cons[MAX_DWA_C] =
{
	{"b", 11}, {"d", 10}, {"f", 2}, {"g", 9},
	{"h", 2}, {"k", 6}, {"l", 11}, {"m", 4},
	{"n", 11}, {"r", 11}, {"s", 1}, {"z", 11},
	{"gh", 1}, {"kh", 4}, {"sh", 3}, {"th", 3}
};

static gibber_type man_cons[MAX_MAN_C] =
{
	{"b", 7}, {"d", 8}, {"h", 2}, {"g", 4},
	{"k", 6}, {"l", 12}, {"m", 8}, {"n", 12},
	{"p", 1}, {"r", 13}, {"s", 3}, {"t", 4},
	{"v", 1}, {"w", 1}, {"y", 2}, {"z", 7},
	{"nd", 1}, {"kh", 2}, {"ph", 2}, {"th", 4}
};

static gibber_type black_cons[MAX_BLK_C] =
{
	{"b", 13}, {"d", 4}, {"g", 4}, {"h", 2},
	{"k", 7}, {"l", 7}, {"m", 7}, {"n", 7},
	{"p", 4}, {"r", 4}, {"t", 7}, {"gh", 2},
	{"gl", 4}, {"gr", 2}, {"kr", 2}, {"nk", 2},
	{"sh", 13}, {"thr", 2}, {"rz", 2}, {"sk", 2},
	{"zg", 5}
};

static gibber_type elf_rule[MAX_ELF_R] =
{
	{"CVC-", 49}, {"CV-", 18}, {"VC-", 25}, {"V-", 7},
	{"-CVC-", 19}, {"-CV-", 5}, {"-VC-", 9}, {"-V-", 17},
	{"-CVC.", 8}, {"-CV.", 20}, {"-VC.", 6}, {"-V.", 16}
};

static gibber_type dwarf_rule[MAX_DWA_R] =
{
	{"CVCVC-", 65}, {"VCCVC-", 8}, {"VCVCVC-", 6}, {"CVCC-", 15},
	{"CCVC-", 6}, {"-CVCVC-", 27}, {"-CCVC-", 5}, {"-CVCC-", 16},
	{"-V-", 5}, {"-CVC.", 19}, {"-VC.", 27}
};

static gibber_type man_rule[MAX_MAN_R] =
{
	{"VCVC-", 11}, {"VCCV-", 6}, {"VCVCVC-", 7}, {"VCVCC-", 4},
	{"VCCVC-", 7}, {"CVC-", 26}, {"CVCVC-", 29}, {"CVCC-", 10},
	{"-CVCVC-", 28}, {"-CVCC-", 10}, {"-CVC-", 7}, {"-VC-", 2},
	{"-V-", 4}, {"-CVC.", 8}, {"-VC.", 6}, {"-V.", 16},
	{"-.", 19}
};

static gibber_type black_rule[MAX_BLK_R] =
{
	{"VC-", 18}, {"CVC-", 82}, {"-CVC-", 17}, {"-VC-", 23}, {"-VC.", 43}, {"-CV.", 7}
};


/*
 * Generate some gibberish for names.
 * Create gibberish that imitates a Tolkien language, using one
 * of the following styles:
 * 0:	Elven
 * 1: Dwarvish
 * 2: Mannish
 * 3: Black Speech
 */
void gibber(char *buf, int mode)
{
	int r, i, base, cons, vowel, rule;

	int max_c, max_v, max_r;

	gibber_type *c_ptr, *v_ptr, *r_ptr;

	cptr t, u, symbol;

	bool middle = FALSE;
	bool end = FALSE;
	bool visible = TRUE;

	switch (mode)
	{
		case NAME_ELVISH:
		{
			/* Get maxima */
			max_c = MAX_ELF_C;
			max_v = MAX_ELF_V;
			max_r = MAX_ELF_R;

			/* Locate the arrays */
			c_ptr = elf_cons;
			v_ptr = elf_vowel;
			r_ptr = elf_rule;

			break;
		}
		case NAME_DWARVEN:
		{
			/* Get maxima */
			max_c = MAX_DWA_C;
			max_v = MAX_DWA_V;
			max_r = MAX_DWA_R;

			/* Locate the arrays */
			c_ptr = dwarf_cons;
			v_ptr = dwarf_vowel;
			r_ptr = dwarf_rule;

			break;
		}
		case NAME_MANNISH:
		{
			/* Get maxima */
			max_c = MAX_MAN_C;
			max_v = MAX_MAN_V;
			max_r = MAX_MAN_R;

			/* Locate the arrays */
			c_ptr = man_cons;
			v_ptr = man_vowel;
			r_ptr = man_rule;

			break;
		}
		case NAME_BLACK:
		{
			/* Get maxima */
			max_c = MAX_BLK_C;
			max_v = MAX_BLK_V;
			max_r = MAX_BLK_R;

			/* Locate the arrays */
			c_ptr = black_cons;
			v_ptr = black_vowel;
			r_ptr = black_rule;

			break;
		}
	}

	while (!end)
	{
		r = rand_int(100);
		base = 100;
		rule = 0;

		/* Choose a rule */
		for (i = 0; i < max_r; i++)
		{
			/* Some rules belong in the middle of words */
			if (middle)
			{
				if (*r_ptr[i].symbol != '-') continue;
			}
			else
			{
				if (*r_ptr[i].symbol == '-') continue;
			}

			/* Check probabilities */
			if (base > r)
			{
				base -= r_ptr[i].rarity;
				rule = i;
			}
		}

		/* Stop before getting too long */
		if (strlen(buf) + 1 + strlen(r_ptr[rule].symbol) > 15) end = TRUE;

		/* Process the rule */
		for (t = r_ptr[rule].symbol; *t; t++)
		{
			r = rand_int(100);

			switch (*t)
			{
				/* Consonant */
				case 'C':
				{
					base = 100;
					cons = 0;

					/* Choose a rule */
					for (i = 0; i < max_c; i++)
					{
						/* Check probabilities */
						if (base > r)
						{
							base -= c_ptr[i].rarity;
							cons = i;
						}
					}

					/* Use this consonant */
					symbol = c_ptr[cons].symbol;

					break;
				}
				/* Vowel */
				case 'V':
				{
					base = 100;
					vowel = 0;

					/* Choose a rule */
					for (i = 0; i < max_v; i++)
					{
						/* Check probabilities */
						if (base > r)
						{
							base -= v_ptr[i].rarity;
							vowel = i;
						}
					}

					/* Use this vowel */
					symbol = v_ptr[vowel].symbol;

					break;
				}
				/* Continuation */
				case '-':
				{
					middle = TRUE;
					visible = FALSE;
					break;
				}
				/* Space */
				case ' ':
				{
					middle = FALSE;
					visible = TRUE;
					break;
				}
				/* Ending */
				case '.':
				{
					end = TRUE;
					visible = FALSE;
					break;
				}
			}

			if (visible) strcat(buf, symbol);
			else visible = TRUE;
		}
	}
}


/*
 * Helper function for make_artifact_random() and make_monster_random()
 *
 * If the current score is low enough, add a flag.
 */
static bool add_flags(u16b *old_score, u16b new_score, u32b *old_flags, u32b new_flag)
{
	/* Reject uncommon flags sometimes */
	if (new_score && (new_score < rand_int(10000)))
	{
		*old_flags |= new_flag;

		/* Increase the value */
		*old_score += new_score;

		/* Success */
		return (TRUE);
	}

	/* Failure */
	return (FALSE);
}



/* Array of enchantment bonuses for make_artifact_random() */
static u16b randart_bonus[12] =
{
	10000, 11769, 13851, 16308, 19185, 22579,
	26573, 31274, 36807, 43318, 50981, 60000
};

/* Array of probabilities to receive enchantment bonuses */
static int randart_rarity[12] =
{
	1000, 707, 500, 353, 250, 163,
	106, 70, 45, 28, 18, 11
};

/*
 * Attempt to create a random artifact
 * This is a very long function; I suppose I should do something about it
 * eventually.
 */
bool make_artifact_random(object_type *o_ptr)
{
	u32b f1, f2, f3, af1, af2, af3;
	u16b bonus, art_score, min_score, mid_score, max_score, tmp_score;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	char buf[80];

	/* Get a new artifact */
	artifact_type *a_ptr;

	int rarity, level, a_idx, b_idx, i, pval;
	int to_ac, to_hit, to_dam, to_ds, to_dd;
	int old_ac, old_hit, old_dam, old_ds, old_dd;
	int rolls = 2;

	int name_style;

	/* Assume nothing */
	bool armor = FALSE;
	bool weapon = FALSE;
	bool bow = FALSE;
	bool digger = FALSE;
	bool cursed = FALSE;
	bool pval_effect = FALSE;
	bool is_pval;

	/* Determine item type */
	switch (o_ptr->tval)
	{
		case TV_HARD_ARMOR:
		case TV_SOFT_ARMOR:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		{
			armor = TRUE;
			break;
		}

		case TV_BOW:
		{
			bow = TRUE;
		}
		case TV_DIGGING:
		{
			digger = TRUE;
		}
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		{
			weapon = TRUE;
			break;
		}

		/* Not an allowed type */
		default:
		{
			return (FALSE);
		}
	}

	/* Get a new artifact */
	a_idx = a_pop();

	if (!a_idx)
	{
		return (FALSE);
	}

	a_ptr = &a_info[a_idx];

	/* Prepare the artifact */
	artifact_prep(a_ptr, o_ptr->k_idx);

	art_score = 0;

	/* Get a pval */
	for (pval = 1; !rand_int(pval); pval++);

	/* Extract the magic */
	old_ac = to_ac = a_ptr->to_a;
	old_hit = to_hit = a_ptr->to_h;
	old_dam = to_dam = a_ptr->to_d;

	old_ds = a_ptr->ds;
	old_dd = a_ptr->dd;

	to_ds = 0;
	to_dd = 0;

	af1 = a_ptr->flags1;
	af2 = a_ptr->flags2;
	af3 = (a_ptr->flags3 | TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
			 TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	/* Occasional cursed artifact */
	if (!rand_int(100) || (a_ptr->flags3 & TR3_LIGHT_CURSE))
	{
		cursed = TRUE;
		af3 |= TR3_LIGHT_CURSE;
	}

	/* Choose a name style */
	i = rand_int(100);

	/* Black Speech */
	name_style = NAME_BLACK;

	/* Mannish */
	if (i < 85) name_style = NAME_MANNISH;

	/* Dwarvish */
	if (i < 65) name_style = NAME_DWARVEN;

	/* Elvish */
	if (i < 40) name_style = NAME_ELVISH;

	/* Shovels are special */
	if (digger)
	{
		/* Use the correct name */
		switch (o_ptr->sval)
		{
			/* Dwarven */
			case SV_DWARVEN_SHOVEL:
			case SV_DWARVEN_PICK:
			{
				name_style = NAME_DWARVEN;
				break;
			}
			/* Orcish */
			case SV_ORCISH_PICK:
			{
				name_style = NAME_BLACK;
				break;
			}
		}
	}

	/* Cursed artifacts are most likely Sauron's work */
	if (cursed && (rand_int(100) < 70)) name_style = NAME_BLACK;

	rarity = rand_int(1000);

	/* Choose a rarity */
	for (i = 0; i < 12; i++)
	{
		if (randart_rarity[i] > rarity) b_idx = i;
	}

	/* Augment the extreme */
	if (b_idx >= 9) rolls++;

	/* Get the bonus */
	bonus = randart_bonus[b_idx];

	/* Find the quality of the artifact */
	max_score = (bonus * (p_ptr->depth + 1L)) / MAX_DEPTH;
	min_score = max_score / 2;

	mid_score = rand_range(min_score, max_score);

	/* Get the 'creation depth' */
	level = (bonus * (((0L + k_ptr->level) + p_ptr->depth) / 2 + 1)) / 10000;

	/* Collect the flags */
	while (art_score < mid_score)
	{
		/* Generate some new flags */
		f1 = 1L << rand_int(32);
		f2 = 1L << rand_int(32);
		f3 = 1L << rand_int(32);

		if (af1 & f1) f1 = 0;
		if (af2 & f2) f2 = 0;
		if (af3 & f3) f3 = 0;

		/* Don't waste time */
		if (!(f1 | f2 | f3)) continue;

		tmp_score = 0;

		is_pval = FALSE;

		/* Check these flags */
		switch (f1)
		{
			case TR1_STR:
			{
				tmp_score = 150 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_INT:
			{
				tmp_score = 100 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_WIS:
			{
				tmp_score = 100 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_DEX:
			{
				tmp_score = 100 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_CON:
			{
				tmp_score = 150 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_CHR:
			{
				tmp_score = 50 * pval;

				/* Orcish artifacts should be ugly */
				if ((name_style == NAME_BLACK) && !cursed) tmp_score = 0;
				else is_pval = TRUE;

				break;
			}
			case TR1_STEALTH:
			{
				tmp_score = 80 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_SEARCH:
			{
				tmp_score = 60 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_INFRA:
			{
				tmp_score = 700 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_TUNNEL:
			{
				/* Good shovels need good pvals */
				if (digger && !is_pval) pval += 5;

				tmp_score = 40 * pval;

				is_pval = TRUE;

				break;
			}
			case TR1_SPEED:
			{
				tmp_score = 1000 * pval;
				is_pval = TRUE;
				break;
			}
			case TR1_BLOWS:
			{
				/* No ridiculous pvals */
				if (pval > 4) pval = 4;

				tmp_score = 2000 * pval;
				is_pval = TRUE;

				if (!weapon || bow)
				{
					tmp_score = 0;
					is_pval = FALSE;
				}

				break;
			}
			case TR1_SHOTS:
			{
				/* No ridiculous pvals */
				if (pval > 4) pval = 4;

				tmp_score = 2000 * pval;

				if (!bow) tmp_score = 0;
				else is_pval = TRUE;

				break;
			}
			case TR1_MIGHT:
			{
				/* No ridiculous pvals */
				if (pval > 2) pval = 2;

				tmp_score = 4500 * pval;

				if (!bow) tmp_score = 0;
				else is_pval = TRUE;

				break;
			}
			case TR1_SLAY_ANIMAL:
			{
				tmp_score = 70;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_EVIL:
			{
				tmp_score = 350;

				/* No silly slays */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_UNDEAD:
			{
				tmp_score = 700;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_DEMON:
			{
				tmp_score = 520;

				/* No silly slays */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_ORC:
			{
				tmp_score = 210;

				/* No silly slays */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_TROLL:
			{
				tmp_score = 240;

				/* No silly slays */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_GIANT:
			{
				tmp_score = 170;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_SLAY_DRAGON:
			{
				tmp_score = 700;

				/* No silly slays */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_KILL_DRAGON:
			{
				tmp_score = 1400;

				/* No silly slays */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_BRAND_ACID:
			{
				tmp_score = 240;

				/* Acid conducts electricity :) */
				if (af1 & TR1_BRAND_ACID) tmp_score = 100;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_BRAND_ELEC:
			{
				tmp_score = 200;

				/* Acid conducts electricity */
				if (af1 & TR1_BRAND_ACID) tmp_score = 120;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_BRAND_FIRE:
			{
				tmp_score = 170;

				/* Fire conflicts with cold */
				if (af1 & TR1_BRAND_COLD) tmp_score = 0;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			case TR1_BRAND_COLD:
			{
				tmp_score = 170;

				/* Fire conflicts with cold */
				if (af1 & TR1_BRAND_FIRE) tmp_score = 0;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow artifacts to become too powerful */
		if ((art_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		if (add_flags(&art_score, tmp_score, &af1, f1))
		{
			/* Note the need for a pval */
			if (is_pval) pval_effect = TRUE;
		}

		tmp_score = 0;
		is_pval = FALSE;

		switch (f2)
		{
			case TR2_SUST_STR:
			{
				tmp_score = 1500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_SUST_INT:
			{
				tmp_score = 1000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_SUST_WIS:
			{
				tmp_score = 1000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_SUST_DEX:
			{
				tmp_score = 1000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_SUST_CON:
			{
				tmp_score = 1500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_SUST_CHR:
			{
				tmp_score = 500;

				/* Orcish artifacts should be ugly */
				if (name_style == NAME_BLACK) tmp_score = 0;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_IM_ACID:
			{
				tmp_score = 6000;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR2_IM_ELEC:
			{
				tmp_score = 5000;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR2_IM_FIRE:
			{
				tmp_score = 6000;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR2_IM_COLD:
			{
				tmp_score = 5000;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR2_RES_ACID:
			{
				tmp_score = 2000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_ELEC:
			{
				tmp_score = 1500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_FIRE:
			{
				tmp_score = 2000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_COLD:
			{
				tmp_score = 1500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_POIS:
			{
				tmp_score = 3000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_FEAR:
			{
				tmp_score = 700;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_LITE:
			{
				tmp_score = 500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_DARK:
			{
				tmp_score = 500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_BLIND:
			{
				tmp_score = 1500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_CONFU:
			{
				tmp_score = 1300;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_SOUND:
			{
				tmp_score = 2000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_SHARD:
			{
				tmp_score = 500;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_NEXUS:
			{
				tmp_score = 3400;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_NETHR:
			{
				tmp_score = 4000;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR2_RES_CHAOS:
			{
				tmp_score = 5300;

				if (cursed) tmp_score = 8500;

				break;
			}
			case TR2_RES_DISEN:
			{
				tmp_score = 6000;

				if (cursed) tmp_score = 9000;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow artifacts to become too powerful */
		if ((art_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		if (add_flags(&art_score, tmp_score, &af2, f2))
		{
			/* Note the need for a pval */
			if (is_pval) pval_effect = TRUE;
		}

		tmp_score = 0;
		is_pval = FALSE;

		switch (f3)
		{
			case TR3_SLOW_DIGEST:
			{
				tmp_score = 300;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_FEATHER:
			{
				tmp_score = 500;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_LITE:
			{
				tmp_score = 100;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_REGEN:
			{
				tmp_score = 800;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_TELEPATHY:
			{
				tmp_score = 2000;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_SEE_INVIS:
			{
				tmp_score = 1300;

				if (cursed) tmp_score *= 2;

				break;
			}
			case TR3_FREE_ACT:
			{
				tmp_score = 3500;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_HOLD_LIFE:
			{
				tmp_score = 3000;

				if (cursed) tmp_score = 0;

				break;
			}
			case TR3_IMPACT:
			{
				tmp_score = 7000;

				if (!weapon || bow) tmp_score = 0;

				break;
			}
			case TR3_TELEPORT:
			{
				tmp_score = 100;

				if (!cursed) tmp_score = 0;

				break;
			}
			case TR3_AGGRAVATE:
			{
				tmp_score = 50;

				if (!cursed) tmp_score = 0;

				break;
			}
			case TR3_DRAIN_EXP:
			{
				tmp_score = 2000;

				if (!cursed) tmp_score = 0;

				break;
			}
			case TR3_BLESSED:
			{
				tmp_score = 100;

				if (cursed || !weapon || bow) tmp_score = 0;

				break;
			}
			case TR3_HEAVY_CURSE:
			{
				tmp_score = 2500;

				if (!cursed) tmp_score = 0;

				break;
			}
			case TR3_PERMA_CURSE:
			{
				tmp_score = 4000;

				if (!cursed) tmp_score = 0;
				else
				{
					/* Get an evil name */
					name_style = NAME_BLACK;

					/* Permanently cursed artifacts are bad */
					rolls++;
				}

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow artifacts to become too powerful */
		if ((art_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		if (add_flags(&art_score, tmp_score, &af3, f3))
		{
			/* Note the need for a pval */
			if (is_pval) pval_effect = TRUE;
		}
	}

	/* Make a rarity check */
	if (rand_int(MAX(max_score, art_score)) < MIN(art_score, max_score))
	{
		/* Deallocate artifact */
		if (a_max < MAX_A_IDX) a_max--;

		a_cnt--;

		/* Unsuccessful */
		return (FALSE);
	}

	/* Reject weak artifacts sometimes */
	if (art_score < randint(1000))
	{
		/* Deallocate artifact */
		if (a_max < MAX_A_IDX) a_max--;

		a_cnt--;

		/* Unsuccessful */
		return (FALSE);
	}

	/* Allow cursed artifacts to become much worse */
	rolls = ((cursed && rand_int(3)) ? rolls + 1 : rolls);

	if (armor)
	{
		/* Increase to-ac */
		for (i = 0; (i < rolls) && (art_score < max_score); i++)
		{
			to_ac += m_bonus(10, level);

			/* Adjust the score */
			art_score += (to_ac - old_ac) * ((to_ac < 10) ? 20 : 68);
		}
	}
	else
	{
		/* Increase to-hit */
		for (i = 0; (i < rolls) && (art_score < max_score); i++)
		{
			to_hit += m_bonus(10, level);

			/* Adjust the score */
			art_score += (to_hit - old_hit) * ((to_hit < 10) ? 8 : 28);
		}

		/* Increase to-dam */
		for (i = 0; (i < rolls) && (art_score < max_score); i++)
		{
			to_dam += m_bonus(10, level);

			/* Adjust the score */
			art_score += (to_dam - old_dam) * ((to_dam < 10) ? 16 : 56);
		}

		/* Hack -- Super-charge the dice size */
		while ((rand_int(5L * old_dd * (old_ds + to_ds)) == 0) && (art_score < max_score))
		{
			to_ds++;
			art_score += to_ds * ((to_ds < to_dd) ? 500 : 200);
		}

		/* Hack -- Super-charge the damage dice */
		while ((rand_int(5L * (old_dd + to_dd) * old_ds) == 0) && (art_score < max_score))
		{
			to_dd++;
			art_score += to_dd * ((to_dd < to_ds) ? 500 : 200);
		}
	}

	/* Get a basic cost */
	a_ptr->cost = art_score * p_ptr->depth;

	/* No silly pvals */
	if (pval_effect && !pval) pval = 1;
	if (!pval_effect) pval = 0;

	/* Curses negate values */
	if (cursed)
	{
		if (pval > 0) pval = 0 - pval;
		if (to_ac > 0) to_ac = 0 - to_ac;
		if (to_hit > 0) to_hit = 0 - to_hit;
		if (to_dam > 0) to_dam = 0 - to_dam;

		/* Keep dice reasonable */
		to_ds = (to_ds < old_ds) ? 0 - to_ds : 0;
		to_dd = (to_dd < old_dd) ? 0 - to_dd : 0;

		a_ptr->cost = 0;
	}

	/* Apply the changes */
	a_ptr->flags1 = af1;
	a_ptr->flags2 = af2;
	a_ptr->flags3 = af3;

	a_ptr->pval = pval;

	a_ptr->to_a = to_ac;
	a_ptr->to_h = to_hit;
	a_ptr->to_d = to_dam;

	a_ptr->ds += to_ds;
	a_ptr->dd += to_dd;

	a_ptr->level = (byte)rand_spread(level, 10);
	a_ptr->level = (a_ptr->level == 0) ? 1 : a_ptr->level;

	/* Start the name */
	strcpy(buf, "\'");

	/* Get a random name */
	gibber(buf, name_style);

	/* End the name */
	strcat(buf, "\'");

	/* Capitalize this name */
	buf[1] = toupper(buf[1]);

	/* Store the name */
	strcpy(a_name + a_ptr->name, buf);

	/* Mega-Hack -- mark the item as an artifact */
	o_ptr->name1 = a_idx;

	/* Success */
	return (TRUE);
}


/* Choose an attack for a player ghost */
static void set_blows(monster_blow *b_ptr, int power, byte mode)
{
	int i = rand_int(10);

	/* Assume no damage */
	int dd = 0;
	int ds = 0;

	switch(mode)
	{
		/* Simple physical attacks */
		case UND_SKELETON:
		case UND_ZOMBIE:
		case UND_MUMMY:
		{
			switch(i)
			{
				case 0: case 1: case 2: case 3:
				{
					b_ptr->method = RBM_HIT;
					b_ptr->effect = RBE_HURT;

					power = power * 2;

					break;
				}
				case 4: case 5: case 6: case 7:
				{
					b_ptr->method = RBM_PUNCH;
					b_ptr->effect = RBE_HURT;

					power = (power * 8) / 10;

					break;
				}
				case 8:
				{
					b_ptr->method = RBM_KICK;
					b_ptr->effect = RBE_HURT;

					break;
				}
				case 9:
				{
					b_ptr->method = RBM_CRUSH;
					b_ptr->effect = RBE_HURT;

					power = (power * 13) / 10;

					break;
				}
			}

			break;
		}

		/* Wailing, clawing, stealing */
		case UND_POLTER:
		case UND_SPIRIT:
		case UND_GHOST:
		{
			switch(i)
			{
				case 0: case 1:
				{
					b_ptr->method = RBM_WAIL;
					b_ptr->effect = rand_int(2) + RBE_CONFUSE;

					power = 0;

					break;
				}
				case 2:
				{
					b_ptr->method = RBM_WAIL;
					b_ptr->effect = rand_int(2) + RBE_EXP_10;

					power = power / 10;

					break;
				}
				case 3: case 4: case 5: case 6:
				{
					b_ptr->method = RBM_CLAW;
					b_ptr->effect = rand_int(6) + RBE_LOSE_STR;

					break;
				}
				case 7: case 8: case 9:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = rand_int(4) + RBE_EAT_GOLD;

					power = 0;

					break;
				}
			}

			break;
		}

		/* Nastier wailing, clawing, and stealing */
		case UND_WRAITH:
		case UND_WIGHT:
		case UND_SHADOW:
		case UND_PHANTOM:
		{
			switch(i)
			{
				case 0: case 1: case 2:
				{
					b_ptr->method = RBM_WAIL;
					b_ptr->effect = rand_int(3) + RBE_CONFUSE;

					power = 0;

					break;
				}
				case 3: case 4:
				{
					b_ptr->method = RBM_WAIL;
					b_ptr->effect = rand_int(2) + RBE_EXP_40;

					power = power / 10;

					break;
				}
				case 5: case 6: case 7: case 8:
				{
					b_ptr->method = RBM_CLAW;
					b_ptr->effect = rand_int(6) + RBE_LOSE_STR;

					break;
				}
				case 9:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = rand_int(4) + RBE_EAT_GOLD;

					power = 0;

					break;
				}
			}

			break;
		}

		/* Biting, touching, and simple attacks */
		case UND_VAMP:
		case UND_HI_VAMP:
		{
			switch(i)
			{
				case 0: case 1:
				{
					b_ptr->method = RBM_BITE;
					b_ptr->effect = rand_int(4) + RBE_EXP_10;

					power = (power * 12) / 10;

					break;
				}
				case 2:
				{
					b_ptr->method = RBM_BITE;
					b_ptr->effect = RBE_LOSE_CON;

					break;
				}
				case 3:
				{
					b_ptr->method = RBM_BITE;
					b_ptr->effect = RBE_LOSE_STR;

					power = (power * 7) / 10;

					break;
				}
				case 4:
				{
					b_ptr->method = RBM_BITE;
					b_ptr->effect = RBE_HURT;

					power = power / 2;

					break;
				}
				case 5:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = RBE_PARALYZE;

					power = 0;

					break;
				}
				case 6: case 7: case 8:
				{
					b_ptr->method = RBM_HIT;
					b_ptr->effect = RBE_HURT;

					power = power * 2;

					break;
				}
				case 9:
				{
					b_ptr->method = RBM_PUNCH;
					b_ptr->effect = RBE_HURT;

					power = (power * 8) / 10;

					break;
				}
			}

			break;
		}

		/* Hitting */
		case UND_DREAD:
		{
			switch(i)
			{
				case 0: case 1: case 2: case 3: case 4: case 5:
				{
					b_ptr->method = RBM_HIT;
					b_ptr->effect = RBE_HURT;

					break;
				}
				case 6: case 7: case 8:
				{
					b_ptr->method = RBM_HIT;
					b_ptr->effect = RBE_LOSE_STR;

					break;
				}
				case 9:
				{
					b_ptr->method = RBM_HIT;
					b_ptr->effect = rand_int(6) + RBE_LOSE_STR;

					break;
				}
			}

			break;
		}


		/* Gazing, touching, and simple attacks */
		case UND_LICH:
		{
			switch(i)
			{
				case 0: case 1: case 2:
				{
					b_ptr->method = RBM_GAZE;
					b_ptr->effect = RBE_PARALYZE;

					power = 0;

					break;
				}
				case 3:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = RBE_PARALYZE;

					power = 0;

					break;
				}
				case 4: case 5:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = RBE_UN_POWER;

					power = 0;

					break;
				}
				case 6: case 7:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = rand_int(6) + RBE_LOSE_STR;

					break;
				}
				case 8:
				{
					b_ptr->method = RBM_TOUCH;
					b_ptr->effect = RBE_HURT;

					power = (power * 15) / 10;

					break;
				}
			}

			break;
		}
	}

	/* Increase the damage done */
	while (dd * ds < power)
	{
		ds += rand_int(2);

		if (dd * ds > power) break;

		dd += rand_int(2);
	}

	b_ptr->d_dice = dd;
	b_ptr->d_side = ds;
}


/*
 * Array of enchantment bonuses for make_ghost_aux()
 */
static u16b randmon_bonus[12] =
{
	10000, 11769, 13851, 16308, 19185, 22579,
	26573, 31274, 36807, 43318, 50981, 60000
};

/* Array of probabilities to receive enchantment bonuses */
static int randmon_rarity[12] =
{
	1000, 707, 500, 353, 250, 163,
	106, 70, 45, 28, 18, 11
};


/*
 * Set race flags for player ghosts.
 */
static bool make_ghost_aux(monster_race *r_ptr, bool magical, byte mode)
{
	u32b f1, f2, f3, f4, f5, f6, rf1, rf2, rf3, rf4, rf5, rf6;
	u16b bonus, race_score, min_score, mid_score, max_score, tmp_score;

	char buf[80];
	char tmp[80];

	int rarity, level, depth, b_idx, i;

	race_score = 0;

	rf1 = r_ptr->flags1;
	rf2 = r_ptr->flags2;
	rf3 = r_ptr->flags3;
	rf4 = r_ptr->flags4;
	rf5 = r_ptr->flags5;
	rf6 = r_ptr->flags6;

	/* The ghosts are undead */
	rf3 |= (RF3_UNDEAD);

	/* Ghosts are Unique */
	rf1 |= (RF1_UNIQUE);

	/* Force depth, maximum hp */
	rf1 |= (RF1_FORCE_DEPTH | RF1_FORCE_MAXHP);

	/* It's dead */
	rf2 |= (RF2_COLD_BLOOD);

	/* Immunities */
	rf3 |= (RF3_IM_COLD | RF3_IM_POIS);

	/* Behavior */
	rf3 |= (RF3_EVIL | RF3_NO_FEAR | RF3_NO_CONF | RF3_NO_SLEEP);

	strcpy(buf, r_name + r_ptr->name);

	/* Set some race defaults */
	switch (mode)
	{
		case UND_SKELETON:
		{
			strcpy(tmp, "skeleton");

			r_ptr->x_char = r_ptr->d_char = 's';
			r_ptr->x_attr = r_ptr->d_attr = TERM_WHITE;

			rf2 |= RF2_BASH_DOOR;
			rf3 |= (RF3_HAS_BONES | RF3_HAS_SKULL);

			magical = FALSE;

			r_ptr->speed = 110;

			break;
		}
		case UND_ZOMBIE:
		{
			strcpy(tmp, "zombie");

			r_ptr->x_char = r_ptr->d_char = 'z';
			r_ptr->x_attr = r_ptr->d_attr = TERM_GREEN;

			rf1 |= RF1_RAND_50;
			rf2 |= RF2_STUPID;
			rf2 |= RF2_BASH_DOOR;
			rf3 |= (RF3_HAS_BODY | RF3_HAS_BONES | RF3_HAS_HEAD | RF3_HAS_SKULL);

			magical = FALSE;

			r_ptr->speed = 110;
			if (!rand_int(3)) r_ptr->speed -= 10;

			break;
		}
		case UND_MUMMY:
		{
			strcpy(tmp, "mummy");

			r_ptr->x_char = r_ptr->d_char = 'z';
			r_ptr->x_attr = r_ptr->d_attr = TERM_WHITE;

			rf2 |= RF2_STUPID;
			rf2 |= RF2_BASH_DOOR;
			rf3 |= (RF3_HAS_BONES | RF3_HAS_SKULL);

			magical = FALSE;

			r_ptr->speed = 110;

			break;
		}
		/* Confusion ;) */
		case UND_POLTER:
		{
			if (rand_int(2))
			{
				/* Get a special color */
				r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_PLASMA);
				strcpy(tmp, "spirit");
			}
			else
			{
				/* Get a special color */
				r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_POIS);
				strcpy(tmp, "ghost");
			}

			r_ptr->x_char = r_ptr->d_char = 'G';

			rf1 |= RF1_RAND_25;
			rf1 |= RF1_RAND_50;
			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_STUPID;
			rf2 |= RF2_PASS_WALL;
			rf6 |= RF6_BLINK;

			magical = FALSE;

			r_ptr->speed = 130;

			break;
		}
		case UND_SPIRIT:
		{
			if (rand_int(3))
			{
				/* Get a special color */
				r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_PLASMA);
				strcpy(tmp, "spirit");
			}
			else
			{
				/* Get a special color */
				r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_POIS);
				strcpy(tmp, "ghost");
			}

			r_ptr->x_char = r_ptr->d_char = 'G';

			rf1 |= RF1_RAND_25;
			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_STUPID;
			rf2 |= RF2_PASS_WALL;

			r_ptr->speed = 120;

			break;
		}
		case UND_GHOST:
		{
			if (!rand_int(4))
			{
				/* Get a special color */
				r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_PLASMA);
				strcpy(tmp, "spirit");
			}
			else
			{
				/* Get a special color */
				r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_POIS);
				strcpy(tmp, "ghost");
			}

			r_ptr->x_char = r_ptr->d_char = 'G';

			/* Get a special color */
			r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_PLASMA);

			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_STUPID;
			rf2 |= RF2_PASS_WALL;

			r_ptr->speed = 120;

			break;
		}
		case UND_VAMP:
		case UND_HI_VAMP:
		{
			strcpy(tmp, "vampire");

			r_ptr->x_char = r_ptr->d_char = 'V';

			/* Get a special color */
			r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_HOLY_ORB);

			rf2 |= RF2_SMART;
			rf2 |= RF2_OPEN_DOOR;
			rf2 |= RF2_BASH_DOOR;
			rf3 |= (RF3_HAS_BODY | RF3_HAS_BONES | RF3_HAS_HEAD | RF3_HAS_SKULL);
			rf5 |= RF5_SCARE;
			rf5 |= RF5_HOLD;
			rf6 |= RF6_DARKNESS;

			r_ptr->speed = 110;
			if (mode == UND_HI_VAMP) r_ptr->speed = 120;

			break;
		}
		case UND_WRAITH:
		{
			strcpy(tmp, "wraith");

			r_ptr->x_char = r_ptr->d_char = 'W';

			/* Get a special color */
			r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_HOLY_ORB);

			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_PASS_WALL;
			rf2 |= RF2_SMART;

			r_ptr->speed = 110;

			break;
		}
		case UND_WIGHT:
		{
			strcpy(tmp, "wight");

			r_ptr->x_char = r_ptr->d_char = 'W';

			/* Get a special color */
			r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_HOLY_ORB);

			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_OPEN_DOOR;
			rf2 |= RF2_BASH_DOOR;
			rf5 |= RF5_SCARE;

			r_ptr->speed = 110;

			break;
		}
		case UND_SHADOW:
		{
			strcpy(tmp, "shadow");

			r_ptr->x_char = r_ptr->d_char = 'G';
			r_ptr->x_attr = r_ptr->d_attr = TERM_L_DARK;

			rf1 |= RF1_RAND_25;
			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_STUPID;
			rf2 |= RF2_PASS_WALL;

			r_ptr->speed = 120;

			break;
		}
		case UND_PHANTOM:
		{
			strcpy(tmp, "phantom");

			r_ptr->x_char = r_ptr->d_char = 'G';
			r_ptr->x_attr = r_ptr->d_attr = TERM_VIOLET;

			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_PASS_WALL;

			r_ptr->speed = 120;

			break;
		}
		case UND_DREAD:
		{
			strcpy(tmp, "dread");

			r_ptr->x_char = r_ptr->d_char = 'G';
			r_ptr->x_attr = r_ptr->d_attr = TERM_ORANGE;

			rf1 |= RF1_ESCORT;
			rf2 |= RF2_INVISIBLE;
			rf2 |= RF2_PASS_WALL;
			rf3 |= RF3_HAS_SKULL;

			r_ptr->speed = 120;

			break;
		}
		case UND_LICH:
		{
			strcpy(tmp, "lich");

			r_ptr->x_char = r_ptr->d_char = 'L';

			/* Get a special color */
			r_ptr->x_attr = r_ptr->d_attr = spell_color(GF_METEOR);

			rf2 |= RF2_SMART;
			rf2 |= RF2_OPEN_DOOR;
			rf2 |= RF2_BASH_DOOR;
			rf3 |= (RF3_HAS_BONES | RF3_HAS_SKULL);

			r_ptr->speed = 110;
			if (!rand_int(3)) r_ptr->speed += 10;
			if (!rand_int(4)) r_ptr->speed += 10;

			break;
		}
	}

	strcpy(r_name + r_ptr->name, format("%^s, the %^s", buf, tmp));

	rarity = rand_int(1000);

	/* Choose a rarity */
	for (i = 0; i < 12; i++)
	{
		if (randmon_rarity[i] > rarity) b_idx = i;
	}

	/* Get the bonus */
	bonus = randmon_bonus[b_idx];

	/* Find the nastiness of the monster */
	depth = ((r_ptr->level + mode) / 2);
	max_score = (bonus * (depth + 1L)) / MAX_DEPTH;
	min_score = max_score / 2;

	mid_score = rand_range(min_score, max_score);

	/* Get the 'creation depth' */
	level = (bonus * (depth + 1L)) / 10000;

	/* Collect the flags */
	while (race_score < mid_score)
	{
		/* Generate some new flags */
		f1 = 1L << rand_int(32);
		f2 = 1L << rand_int(32);
		f3 = 1L << rand_int(32);
		f4 = 1L << rand_int(32);
		f5 = 1L << rand_int(32);
		f6 = 1L << rand_int(32);

		if (rf1 & f1) f1 = 0;
		if (rf2 & f2) f2 = 0;
		if (rf3 & f3) f3 = 0;
		if (rf4 & f4) f4 = 0;
		if (rf5 & f5) f5 = 0;
		if (rf6 & f6) f6 = 0;

		/* Don't waste time (not likely) */
		if (!(f1 | f2 | f3 | f4 | f5 | f6)) continue;

		tmp_score = 0;

		/* Check these flags */
		switch (f1)
		{
			case RF1_RAND_25:
			{
				tmp_score = 1700;

				if (rf2 & RF2_STUPID) tmp_score = 800;

				/* Smart monsters know where they're going */
				if (rf2 & RF2_SMART) tmp_score = 0;

				break;
			}
			case RF1_RAND_50:
			{
				tmp_score = 1900;

				if (rf2 & RF2_STUPID) tmp_score = 900;

				/* Smart monsters know where they're going */
				if (rf2 & RF2_SMART) tmp_score = 0;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow monsters to become too powerful */
		if ((race_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		add_flags(&race_score, tmp_score, &rf1, f1);

		tmp_score = 0;

		switch (f2)
		{
			case RF2_STUPID:
			{
				tmp_score = 1300;

				/* Can't be smart and stupid */
				if (rf2 & RF2_SMART) tmp_score = 0;

				break;
			}
			case RF2_SMART:
			{
				tmp_score = 4300;

				/* Can't be smart and stupid */
				if (rf2 & RF2_STUPID) tmp_score = 0;

				break;
			}
			case RF2_EMPTY_MIND:
			{
				tmp_score = 500;

				/* Can't be smart without a mind */
				if (rf2 & RF2_SMART) tmp_score = 0;

				break;
			}
			case RF2_WEIRD_MIND:
			{
				tmp_score = 800;

				/* Can't be smart without a mind */
				if (rf2 & RF2_SMART) tmp_score = 0;

				break;
			}
			case RF2_REGENERATE:
			{
				tmp_score = 2000;

				break;
			}
			case RF2_POWERFUL:
			{
				tmp_score = 4000;

				break;
			}
			case RF2_PASS_WALL:
			{
				tmp_score = 2000;

				break;
			}
			case RF2_KILL_WALL:
			{
				tmp_score = 3100;

				break;
			}
			case RF2_MOVE_BODY:
			{
				tmp_score = 300;

				break;
			}
			case RF2_KILL_BODY:
			{
				tmp_score = 800;

				break;
			}
			case RF2_KILL_ITEM:
			{
				tmp_score = 1200;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow monsters to become too powerful */
		if ((race_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		add_flags(&race_score, tmp_score, &rf2, f2);

		tmp_score = 0;

		switch (f3)
		{
			case RF3_RES_NETH:
			{
				tmp_score = 1100;

				break;
			}
			case RF3_RES_WATE:
			{
				tmp_score = 2800;

				break;
			}
			case RF3_RES_PLAS:
			{
				tmp_score = 4500;

				break;
			}
			case RF3_RES_NEXU:
			{
				tmp_score = 3400;

				break;
			}
			case RF3_RES_DISE:
			{
				tmp_score = 6000;

				break;
			}
			case RF3_NO_STUN:
			{
				tmp_score = 2300;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow monsters to become too powerful */
		if ((race_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		add_flags(&race_score, tmp_score, &rf3, f3);

		tmp_score = 0;

		switch (f4)
		{
			case RF4_BR_POIS:
			{
				tmp_score = 4000;

				break;
			}
			case RF4_BR_NETH:
			{
				tmp_score = 4500;

				break;
			}
			case RF4_BR_DARK:
			{
				tmp_score = 1100;

				break;
			}
			case RF4_BR_DISE:
			{
				tmp_score = 8000;

				break;
			}
			case RF4_BR_NEXU:
			{
				tmp_score = 3500;

				break;
			}
			case RF4_BR_MANA:
			{
				tmp_score = 9000;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow monsters to become too powerful */
		if ((race_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		add_flags(&race_score, tmp_score, &rf4, f4);

		/* Non-magical monsters are done */
		if (!magical) continue;

		tmp_score = 0;

		switch (f5)
		{
			case RF5_BA_ACID:
			{
				tmp_score = 2000;

				break;
			}
			case RF5_BA_ELEC:
			{
				tmp_score = 2300;

				break;
			}
			case RF5_BA_FIRE:
			{
				tmp_score = 2500;

				break;
			}
			case RF5_BA_COLD:
			{
				tmp_score = 2400;

				break;
			}
			case RF5_BA_POIS:
			{
				tmp_score = 3800;

				break;
			}
			case RF5_BA_NETH:
			{
				tmp_score = 4300;

				break;
			}
			case RF5_BA_WATE:
			{
				tmp_score = 3300;

				break;
			}
			case RF5_BA_MANA:
			{
				tmp_score = 9000;

				break;
			}
			case RF5_BA_DARK:
			{
				tmp_score = 1000;

				break;
			}
			case RF5_DRAIN_MANA:
			{
				tmp_score = 500;

				break;
			}
			case RF5_MIND_BLAST:
			{
				tmp_score = 4500;

				break;
			}
			case RF5_BRAIN_SMASH:
			{
				tmp_score = 6000;

				break;
			}
			case RF5_CAUSE_1:
			{
				tmp_score = 400;

				break;
			}
			case RF5_CAUSE_2:
			{
				tmp_score = 500;

				break;
			}
			case RF5_CAUSE_3:
			{
				tmp_score = 1400;

				break;
			}
			case RF5_CAUSE_4:
			{
				tmp_score = 3200;

				break;
			}
			case RF5_BO_ACID:
			{
				tmp_score = 1000;

				break;
			}
			case RF5_BO_ELEC:
			{
				tmp_score = 1200;

				break;
			}
			case RF5_BO_FIRE:
			{
				tmp_score = 1300;

				break;
			}
			case RF5_BO_COLD:
			{
				tmp_score = 1200;

				break;
			}
			case RF5_BO_POIS:
			{
				tmp_score = 1900;

				break;
			}
			case RF5_BO_NETH:
			{
				tmp_score = 2200;

				break;
			}
			case RF5_BO_WATE:
			{
				tmp_score = 1700;

				break;
			}
			case RF5_BO_MANA:
			{
				tmp_score = 6000;

				break;
			}
			case RF5_BO_PLAS:
			{
				tmp_score = 2000;

				break;
			}
			case RF5_BO_ICEE:
			{
				tmp_score = 1800;

				break;
			}
			case RF5_MISSILE:
			{
				tmp_score = 100;

				break;
			}
			case RF5_SCARE:
			{
				tmp_score = 200;

				break;
			}
			case RF5_BLIND:
			{
				tmp_score = 1300;

				break;
			}
			case RF5_CONF:
			{
				tmp_score = 400;

				break;
			}
			case RF5_SLOW:
			{
				tmp_score = 500;

				break;
			}
			case RF5_HOLD:
			{
				tmp_score = 2300;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow monsters to become too powerful */
		if ((race_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		add_flags(&race_score, tmp_score, &rf5, f5);

		tmp_score = 0;

		switch (f6)
		{
			case RF6_HASTE:
			{
				tmp_score = 300;

				break;
			}
			case RF6_HEAL:
			{
				tmp_score = 1100;

				break;
			}
			case RF6_BLINK:
			{
				tmp_score = 1500;

				break;
			}
			case RF6_TPORT:
			{
				tmp_score = 2500;

				break;
			}
			case RF6_TELE_TO:
			{
				tmp_score = 2400;

				break;
			}
			case RF6_TELE_AWAY:
			{
				tmp_score = 2600;

				break;
			}
			case RF6_TELE_LEVEL:
			{
				tmp_score = 5000;

				break;
			}
			case RF6_DARKNESS:
			{
				tmp_score = 1300;

				break;
			}
			case RF6_TRAPS:
			{
				tmp_score = 1200;

				break;
			}
			case RF6_FORGET:
			{
				tmp_score = 3000;

				break;
			}
			case RF6_S_MONSTER:
			{
				tmp_score = 1300;

				break;
			}
			case RF6_S_MONSTERS:
			{
				tmp_score = 1600;

				break;
			}
			case RF6_S_SPIDER:
			{
				tmp_score = 1700;

				break;
			}
			case RF6_S_HOUND:
			{
				tmp_score = 2700;

				break;
			}
			case RF6_S_HYDRA:
			{
				tmp_score = 4500;

				break;
			}
			case RF6_S_ORC:
			{
				tmp_score = 1200;

				break;
			}
			case RF6_S_ANGBAND:
			{
				tmp_score = 5600;

				break;
			}
			case RF6_S_DEMON:
			{
				tmp_score = 4300;

				break;
			}
			case RF6_S_UNDEAD:
			{
				tmp_score = 2300;

				break;
			}
			case RF6_S_DRAGON:
			{
				tmp_score = 4700;

				break;
			}
			case RF6_S_HI_UNDEAD:
			{
				tmp_score = 4300;

				break;
			}
			case RF6_S_HI_DRAGON:
			{
				tmp_score = 6100;

				break;
			}
			case RF6_S_WRAITH:
			{
				tmp_score = 7000;

				break;
			}
			case RF6_S_UNIQUE:
			{
				tmp_score = 8000;

				break;
			}
			default:
			{
				tmp_score = 0;
				break;
			}
		}

		/* Don't allow monsters to become too powerful */
		if ((race_score + tmp_score > max_score) && (tmp_score < rand_int(10000))) break;

		/* Try to add another flag */
		add_flags(&race_score, tmp_score, &rf6, f6);
	}

	/* Apply the changes */
	r_ptr->flags1 = rf1;
	r_ptr->flags2 = rf2;
	r_ptr->flags3 = rf3;
	r_ptr->flags4 = rf4;
	r_ptr->flags5 = rf5;
	r_ptr->flags6 = rf6;

	/* Success */
	return (TRUE);
}


/*
 * Create a base race for a player ghost.
 */
bool make_ghost(int g_idx)
{
	ghost_type *g_ptr = &g_info[g_idx];
	monster_race *r_ptr = &r_info[RACE_MAX_NORMAL + g_idx];
	monster_xtra *x_ptr = &x_info[RACE_MAX_NORMAL + g_idx];
	monster_lore *l_ptr = &l_info[RACE_MAX_NORMAL + g_idx];

	int hd = 0;
	int hs = 0;
	int attacks, fail, power, mul, div, p, d, i;

	byte und_type = undead_type[0];

	bool magical;

	/* Initialize some stuff */
	strcpy(r_name + r_ptr->name, g_name + g_ptr->name);

	r_ptr->level = (g_ptr->max_depth + g_ptr->depth + g_ptr->lev) / 3;

	r_ptr->mexp = g_ptr->exp;

	while (hd * hs < g_ptr->mhp)
	{
		hs += rand_int(2);

		if (hd * hs > power) break;

		hd += rand_int(2);
	}

	r_ptr->hdice = hd;
	r_ptr->hside = hs;

	r_ptr->ac = adj_dex_ta[g_ptr->stat_max[A_DEX]] - 128 + 5;

	r_ptr->flags1 = 0L;
	r_ptr->flags2 = 0L;
	r_ptr->flags3 = 0L;
	r_ptr->flags4 = 0L;
	r_ptr->flags5 = 0L;
	r_ptr->flags6 = 0L;

	/* Just in case */
	switch (g_ptr->psex)
	{
		case SEX_FEMALE:
		{
			r_ptr->flags1 |= RF1_FEMALE;
			break;
		}
		case SEX_MALE:
		{
			r_ptr->flags1 |= RF1_MALE;
			break;
		}
	}

	/* Physical and mental power */
	switch (g_ptr->pclass)
	{
		case CLASS_WARRIOR:
		{
			mul = 5;
			div = 30;

			magical = FALSE;

			break;
		}
		case CLASS_MAGE:
		{
			mul = 2;
			div = 40;

			magical = TRUE;
			fail = adj_mag_fail[g_ptr->stat_max[A_INT]];

			break;
		}
		case CLASS_PRIEST:
		{
			mul = 3;
			div = 35;

			magical = TRUE;
			fail = adj_mag_fail[g_ptr->stat_max[A_WIS]];

			break;
		}
		case CLASS_ROGUE:
		{
			mul = 3;
			div = 30;

			magical = TRUE;
			fail = adj_mag_fail[g_ptr->stat_max[A_INT]];

			break;
		}
		case CLASS_RANGER:
		{
			mul = 4;
			div = 35;

			magical = TRUE;
			fail = adj_mag_fail[g_ptr->stat_max[A_INT]];

			break;
		}
		case CLASS_PALADIN:
		{
			mul = 4;
			div = 30;

			magical = TRUE;
			fail = adj_mag_fail[g_ptr->stat_max[A_WIS]];

			break;
		}
	}

	power = mul * (adj_str_td[g_ptr->stat_max[A_STR]] - 128 + 5);

	/* Check the blows modifiers */
	p = (adj_str_blow[g_ptr->stat_max[A_STR]] * mul) / div;
	d = adj_dex_blow[g_ptr->stat_max[A_DEX]];

	if (p > 11) p = 11;
	if (d > 11) d = 11;

	/* Set the number of attacks */
	attacks = blows_table[p][d];
	if (attacks > 4) attacks = 4;

	/* Not magical */
	if (!g_ptr->msp) magical = FALSE;
	if (!magical) fail = 255;

	/* Spell failure rate */
	if (fail < 2) fail = 2;
	r_ptr->freq_inate = r_ptr->freq_spell = 100 / fail;

	/* Choose a type of undead */
	for (i = 0; i < MAX_UNDEAD; i++)
	{
		if ((undead_type[i] < r_ptr->level) &&
			 (rand_int(r_ptr->level + undead_type[i]) > und_type))
		{
			und_type = undead_type[i];
		}
	}

	/* Remember this type */
	g_ptr->und_type = und_type;

	/* Wipe the blows */
	for (i = 0; i < 4; i++)
	{
		WIPE(&x_ptr->blow[i], monster_blow);
		l_ptr->blows[i] = 0;
	}

	/* Set the blows */
	for (i = 0; i < attacks; i++)
	{
		set_blows(&(x_ptr->blow[i]), power, und_type);
	}

	r_ptr->sleep = 0;

	r_ptr->aaf = 20;

	/* Very rare */
	r_ptr->rarity = 5;

	/* Weight */
	r_ptr->wt = g_ptr->wt * 10;

	r_ptr->max_num = 1;
	r_ptr->cur_num = 0;

	/* Wipe the inventory */
	for (i = 0; i < 10; i++)
	{
		WIPE(&x_ptr->inv[i], monster_inv);
	}

	/* Wipe the lore */
	WIPE(l_ptr, monster_lore);

	if (make_ghost_aux(r_ptr, magical, und_type)) return (TRUE);
	else return (FALSE);
}
