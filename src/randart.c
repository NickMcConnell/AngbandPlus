/* File: randart.c */

/*
 * Random artifacts (also forged items).
 *
 * Selling and providing qualities.  Choosing an object type and kind,
 * determining the potential, depth and rarity of the artifact.  Artifact
 * themes.  Adding semi-random qualities.  Cursing an artifact, removing
 * contradictory flags, naming, initializing.  Adding new names to the
 * a_name array.  Initializing all random artifacts.
 *
 * Copyright (c) 2007 Leon Marrick
 *
 * I owe thanks to Greg Wooledge for his support and string-handling code
 * and to W. Sheldon Simms for his Tolkienesque random name generator.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"
#include "init.h"


/* A global variable whose contents will be bartered to acquire powers. */
static int potential = 0;

/* The initial potential of the artifact. */
static int initial_potential = 0;

/*
 * The maximum potential of any artifact.  The higher this is, the better
 * artifacts can be.  The lower it is, the easier it is to get high pvals.
 *
 * Artifact cost is generally potential * 10.
 */
#define POTENTIAL_MAX   8000

/* The maximum potential that this artifact could have had */
static int max_potential = 0;


/* Saved object values */
int save_ds;
int save_dd;
int power_of_base_object;


/* Global variable to hold pval-dependant flags. */
static u32b add_pval_later = 0L;


/* Global variable indicating that the random naming routine is unavailable. */
static bool find_all_names = FALSE;


/* Percentage chance for an artifact to be terrible. */
#define TERRIBLE_CHANCE		10


/* Globals used by the artifact naming code. */
#define BUFLEN 1024

#define NAMES_FILE "names.txt"
#define MIN_NAME_LEN 5
#define MAX_NAME_LEN 9
#define S_WORD 26
#define E_WORD S_WORD
long lprobs[S_WORD+1][S_WORD+1][S_WORD+1];	/* global, hence init to 0 */
long ltotal[S_WORD+1][S_WORD+1];		/* global, hence init to 0 */

/* Temporary space for names, while reading and randomizing them. */
static char *names[ART_LIST_SIZE];


/*
 * Range of values for pvals controlling pval-dependant qualities.
 */
byte pval_range[32][2] =
{
	{ 1,  6 },   /* TR_PVAL_STR */
	{ 1,  6 },   /* TR_PVAL_INT */
	{ 1,  6 },   /* TR_PVAL_WIS */
	{ 1,  6 },   /* TR_PVAL_DEX */
	{ 1,  6 },   /* TR_PVAL_CON */
	{ 1,  6 },   /* TR_PVAL_CHR */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 1,  5 },   /* TR_PVAL_STEALTH */
	{ 1,  4 },   /* TR_PVAL_AWARE */
	{ 3,  8 },   /* TR_PVAL_INFRA */
	{ 2,  6 },   /* TR_PVAL_TUNNEL */
	{ 3, 10 },   /* TR_PVAL_SPEED */
	{ 2,  5 },   /* TR_PVAL_INVIS */
	{ 2,  6 },   /* TR_PVAL_DISARM */
	{ 1,  4 },   /* TR_PVAL_DEVICE */
	{ 2,  6 },   /* TR_PVAL_SAVE */
	{ 2, 10 },   /* TR_PVAL_MANA */
	{ 1,  3 },   /* TR_PVAL_LIGHT */
	{ 0,  0 },   /*  */
	{ 1,  3 },   /* TR_PVAL_BLOWS */
	{ 1,  4 },   /* TR_PVAL_SHOTS (assumes relatively low bow potentials) */
	{ 1,  3 },   /* TR_PVAL_MIGHT (assumes relatively low bow potentials) */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 },   /*  */
	{ 0,  0 }    /*  */
};


/*
 * Debit an artifact's account.
 */
static bool take_money(bool on_credit, int cost)
{
	/* Take the money. */
	if (potential >= cost)
	{
		potential -= cost;
		return (TRUE);
	}

	/* <<mutter>> OK, I'll charge it to your account... */
	else if (on_credit)
	{
		potential = 0;
		return (TRUE);
	}

	/* Otherwise, kick da bum out. */
	else return (FALSE);
}


/*
 * Get the cost of a flag (in tens of gold pieces)
 *
 * For most pval-dependant qualities, this is the normal price bonus
 * divided by 7 (speed being the exception).  Other flags have specific
 * costs.
 */
s16b get_cost_of_flag(int flag, int pval)
{
	s32b cost = -1L;

	switch (flag)
	{
		/* Flags_pval */
		case PVAL_STR:      cost = pval_value(TR_PVAL_STR, pval) / 7; break;
		case PVAL_INT:      cost = pval_value(TR_PVAL_INT, pval) / 7; break;
		case PVAL_WIS:      cost = pval_value(TR_PVAL_WIS, pval) / 7; break;
		case PVAL_DEX:      cost = pval_value(TR_PVAL_DEX, pval) / 7; break;
		case PVAL_CON:      cost = pval_value(TR_PVAL_CON, pval) / 7; break;
		case PVAL_CHR:      cost = pval_value(TR_PVAL_CHR, pval) / 7; break;

		case PVAL_STEALTH:  cost = pval_value(TR_PVAL_STEALTH, pval) / 7; break;
		case PVAL_AWARE:    cost = pval_value(TR_PVAL_AWARE, pval) / 7; break;
		case PVAL_INFRA:    cost = pval_value(TR_PVAL_INFRA, pval)  / 7; break;
		case PVAL_TUNNEL:   cost = pval_value(TR_PVAL_TUNNEL, pval) / 7; break;
		case PVAL_SPEED:    cost = pval_value(TR_PVAL_SPEED, pval)  / 40; break;
		case PVAL_INVIS:    cost = pval_value(TR_PVAL_INVIS, pval)  / 7; break;
		case PVAL_DISARM:   cost = pval_value(TR_PVAL_DISARM, pval) / 7; break;
		case PVAL_DEVICE:   cost = pval_value(TR_PVAL_DEVICE, pval) / 7; break;
		case PVAL_SAVE:     cost = pval_value(TR_PVAL_SAVE, pval)   / 7; break;
		case PVAL_MANA:     cost = pval_value(TR_PVAL_MANA, pval)   / 7; break;
		case PVAL_LIGHT:    cost = pval_value(TR_PVAL_LIGHT, pval)  / 7; break;

		case PVAL_BLOWS:    cost = pval_value(TR_PVAL_BLOWS, pval) / 7; break;
		case PVAL_SHOTS:    cost = pval_value(TR_PVAL_SHOTS, pval) / 10; break;
		case PVAL_MIGHT:    cost = pval_value(TR_PVAL_MIGHT, pval) / 10; break;

		/* Flags1 */
		case SUST_STR:        return (250);
		case SUST_WIS:        return (200);
		case SUST_INT:        return (200);
		case SUST_DEX:        return (200);
		case SUST_CON:        return (200);
		case SUST_CHR:        return (50);

		case SLAY_ANIMAL:     return (400);
		case SLAY_EVIL:       return (500);
		case SLAY_UNDEAD:     return (500);
		case SLAY_DEMON:      return (500);
		case SLAY_ORC:        return (250);
		case SLAY_TROLL:      return (400);
		case SLAY_GIANT:      return (400);
		case SLAY_DRAGON:     return (500);
		case KILL_DRAGON:     return (800);

		case BRAND_ACID:      return (500);
		case BRAND_ELEC:      return (600);
		case BRAND_FIRE:      return (500);
		case BRAND_COLD:      return (500);
		case BRAND_POIS:      return (500);
		case BRAND_FLAME:     return (1200);
		case BRAND_VENOM:     return (1200);

		case RETURNING:       return (0);  /* Special case */
		case VORPAL:          return (400);
		case THROWING:        return (350);
		case PERFECT_BALANCE: return (600);
		case TWO_HANDED_REQ:  return (0);
		case TWO_HANDED_DES:  return (0);

		/* Flags2 */
		case IM_ACID:         return (2750);
		case IM_ELEC:         return (2500);
		case IM_FIRE:         return (3000);
		case IM_COLD:         return (2500);
		case RES_ACID:        return (400);
		case RES_ELEC:        return (450);
		case RES_FIRE:        return (400);
		case RES_COLD:        return (400);
		case RES_POIS:        return (900);
		case RES_LITE:        return (500);
		case RES_DARK:        return (500);
		case RES_FEAR:        return (450);
		case RES_BLIND:       return (550);
		case RES_CONFU:       return (600);
		case RES_SOUND:       return (500);
		case RES_SHARD:       return (500);
		case RES_NEXUS:       return (500);
		case RES_NETHR:       return (650);
		case RES_CHAOS:       return (650);
		case RES_DISEN:       return (750);

		case IGNORE_ACID:     return (150);
		case IGNORE_ELEC:     return (50);
		case IGNORE_FIRE:     return (120);
		case IGNORE_COLD:     return (50);


		/* Flags3 */
		case SLOW_DIGEST:     return (350);
		case FEATHER:         return (250);
		case LITE:            return (450);
		case REGEN:           return (450);
		case TELEPATHY:       return (2600);
		case SEE_INVIS:       return (600);
		case FREE_ACT:        return (600);
		case HOLD_LIFE:       return (1100);
		case BLESSED:         return (200);
		case IMPACT:          return (600);

		case INSTA_ART:       return (0);
		case EASY_KNOW:       return (0);
		case HIDE_TYPE:       return (0);
		case SHOW_MODS:       return (0);

		case NOFUEL:          return (500);
		case SOULSTEAL:       return (0);
		case NOMAGIC:         return (0);

		case TELEPORT:        return (0);
		case AGGRAVATE:       return (0);
		case DRAIN_EXP:       return (0);
		case DRAIN_HP:        return (0);

		case LIGHT_CURSE:     return (0);
		case HEAVY_CURSE:     return (0);
		case PERMA_CURSE:     return (0);


		/* Flags not yet added */
		default:              return (-1);
	}

	/* Convert value and return it */
	if (cost > 30000L) return (30000);
	else               return ((s16b)cost);
}



/*
 * Grant the quality asked for, if the artifact can afford it.
 *
 * Adding or changing pval-dependant qualities can be a little tricky.  We
 * need to check if the quality already exists, allow exact or incremental
 * adjustments to each of three pvals, control how high pvals can go, and
 * assign reasonable prices to everything.
 *
 * We try not to use pval slots without need.
 */
static void get_quality(bool credit, int purchase, int val, int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int temp, i;

	s16b *pval_ptr = NULL;
	u32b *flags_pval_ptr = NULL;


	/* Illegal purchase */
	if (purchase < 0) return;

	/*
	 * Missiles and throwing weapons aren't meant to be wielded.  Many
	 * attributes would be strange for them to have.
	 */
	if ((is_missile(a_ptr)) || (a_ptr->flags1 & (TR1_THROWING)))
	{
		/* Pval-dependant qualities require that the object be wielded */
		if (purchase < 32) return;

		/* Many flags require that the object be wielded */
		if ((purchase >= SUST_STR) && (purchase <= SUST_CHR)) return;
		if ((purchase >= IM_ACID) && (purchase <= RES_DISEN)) return;
		if (purchase == SLOW_DIGEST) return;
		if (purchase == FEATHER) return;
		if (purchase == REGEN) return;
		if (purchase == TELEPATHY) return;
		if (purchase == SEE_INVIS) return;
		if (purchase == FREE_ACT) return;
		if (purchase == HOLD_LIFE) return;
		if (purchase == SOULSTEAL) return;
		if (purchase == NOMAGIC) return;
		if (purchase == HOLD_LIFE) return;
		if (purchase == TELEPORT) return;
		if (purchase == AGGRAVATE) return;
		if (purchase == DRAIN_EXP) return;
		if (purchase == DRAIN_HP) return;
		if (purchase == THROWING) return;

	}

	/* Bows should not have certain attributes */
	if (is_missile_weapon(a_ptr))
	{
		/* Cancel some pval-dependant qualities */
		if (purchase == PVAL_TUNNEL) return;
		if (purchase == PVAL_DISARM) return;
		if (purchase == PVAL_DEVICE) return;
		if (purchase == PVAL_MANA) return;

		/* Triple Xbows with extra might are bad */
		if ((purchase == PVAL_MIGHT) && (a_ptr->sval == SV_TRIPLE_XBOW))
			return;

		if (purchase == THROWING) return;
	}




	/* Adding pval-dependant qualities can be a little tricky */
	if (purchase < 32)
	{
		bool already_exists = TRUE;

		/* See if this quality has already been added */
		if ((a_ptr->pval1 != 0) && (a_ptr->flags_pval1 & (1L << purchase)))
		{
			pval_ptr = &a_ptr->pval1;
			flags_pval_ptr = &a_ptr->flags_pval1;
		}
		else if ((a_ptr->pval2 != 0) && (a_ptr->flags_pval2 & (1L << purchase)))
		{
			pval_ptr = &a_ptr->pval2;
			flags_pval_ptr = &a_ptr->flags_pval2;
		}
		else if ((a_ptr->pval3 != 0) && (a_ptr->flags_pval3 & (1L << purchase)))
		{
			pval_ptr = &a_ptr->pval3;
			flags_pval_ptr = &a_ptr->flags_pval3;
		}
		else
		{
			already_exists = FALSE;
		}

		/* The quality already exists */
		if (already_exists)
		{
			s32b orig_cost = 0L;
			s32b new_cost = 0L;
			s32b debit;
			int new_pval;

			/* We want to increment the pval */
			if (val <= 0)
			{
				/* Stay below maximum */
				if (*pval_ptr >= pval_range[purchase][1]) return;

				/* (try to) Increment the pval */
				new_pval = *pval_ptr + 1;
			}

			/* We want to boost the pval */
			else if (val > *pval_ptr)
			{
				/* Stay at or below maximum */
				if (val > pval_range[purchase][1]) return;

				/* (try to) Increase the pval */
				new_pval = val;
			}

			/* We do not allow pvals to be reduced here.  XXX */
			else return;

			/* For every quality currently assigned to this pval... */
			for (i = 0; i < 32; i++)
			{
				if ((*flags_pval_ptr) & (1L << i))
				{
					/* Sum the original cost of adding the quality */
					orig_cost += (s32b)get_cost_of_flag(i, *pval_ptr);

					/* Sum the new cost if the pval were boosted */
					new_cost += (s32b)get_cost_of_flag(i, new_pval);
				}
			}

			/* Can we afford to boost the pval? */
			if ((credit) || (orig_cost + potential >= new_cost))
			{
				/* Boost it */
				*pval_ptr = new_pval;

				/* Charge the cost of the change to the account */
				debit = new_cost - orig_cost;

				potential -= MIN(potential, (int)debit);

				/* A job well done */
				return;
			}

			/* Nope, can't afford it.  Sorry. */
			else
			{
				/* Wimp out.  XXX */
				return;
			}
		}


		/* We have not explicitly set a pval */
		else if (val <= 0)
		{
			/* Get upper and lower bounds */
			int low  = pval_range[purchase][0];
			int high = pval_range[purchase][1];

			/* Get a pval within the range, using remaining potential */
			val = low + m_bonus((high - low), potential, POTENTIAL_MAX);

			/* If for some reason the pval is zero, do nothing */
			if (val <= 0) return;
		}

		/* Scan existing pvals, find and point to an exact match */
		if ((a_ptr->pval1) && (a_ptr->pval1 == val))
		{
			pval_ptr = &a_ptr->pval1;
			flags_pval_ptr = &a_ptr->flags_pval1;
		}
		else if ((a_ptr->pval2) && (a_ptr->pval2 == val))
		{
			pval_ptr = &a_ptr->pval2;
			flags_pval_ptr = &a_ptr->flags_pval2;
		}
		else if ((a_ptr->pval3) && (a_ptr->pval3 == val))
		{
			pval_ptr = &a_ptr->pval3;
			flags_pval_ptr = &a_ptr->flags_pval3;
		}

		/* No exact match -- allow a little leeway */
		else if ((a_ptr->pval1) && (ABS(a_ptr->pval1 - val) <= 1))
		{
			pval_ptr = &a_ptr->pval1;
			flags_pval_ptr = &a_ptr->flags_pval1;
			val = a_ptr->pval1;
		}
		else if ((a_ptr->pval2) && (ABS(a_ptr->pval2 - val) <= 1))
		{
			pval_ptr = &a_ptr->pval2;
			flags_pval_ptr = &a_ptr->flags_pval2;
			val = a_ptr->pval2;
		}
		else if ((a_ptr->pval3) && (ABS(a_ptr->pval3 - val) <= 1))
		{
			pval_ptr = &a_ptr->pval3;
			flags_pval_ptr = &a_ptr->flags_pval3;
			val = a_ptr->pval3;
		}

		/* If no close-enough match, point to a new pval */
		else if (!a_ptr->pval1)
		{
			pval_ptr = &a_ptr->pval1;
			flags_pval_ptr = &a_ptr->flags_pval1;
		}
		else if (!a_ptr->pval2)
		{
			pval_ptr = &a_ptr->pval2;
			flags_pval_ptr = &a_ptr->flags_pval2;
		}
		else if (!a_ptr->pval3)
		{
			pval_ptr = &a_ptr->pval3;
			flags_pval_ptr = &a_ptr->flags_pval3;
		}

		/* No close-enough match, no available pval -- cancel the request */
		else return;

		/* Try to purchase the quality */
		if (take_money(credit, get_cost_of_flag(purchase, val)))
		{
			/* Adjust the pval */
			*pval_ptr = val;

			/* Add the quality */
			*flags_pval_ptr |= (1L << (purchase % 32));


			/* Handle special cases */
			switch (purchase)
			{
				case PVAL_STR:
					if (one_in_(3)) a_ptr->flags1 |= (TR1_SUST_STR);
					break;
				case PVAL_INT:
					if (one_in_(3)) a_ptr->flags1 |= (TR1_SUST_INT);
					break;
				case PVAL_WIS:
					if (one_in_(3)) a_ptr->flags1 |= (TR1_SUST_WIS);
					break;
				case PVAL_DEX:
					if (one_in_(3)) a_ptr->flags1 |= (TR1_SUST_DEX);
					break;
				case PVAL_CON:
					if (one_in_(3)) a_ptr->flags1 |= (TR1_SUST_CON);
					break;
				case PVAL_CHR:
					if (one_in_(3)) a_ptr->flags1 |= (TR1_SUST_CHR);
					break;

				default:
					break;
			}
		}
	}

	/* Adding a non pval-dependant quality */
	else
	{
		/* Handle attributes that belong to flag sets */
		if ((purchase >= 32) && (purchase < 128))
		{
			u32b *flags_ptr;

			/* Point to the right flag set */
			if ((purchase >= 32) && (purchase < 64))
				flags_ptr = &a_ptr->flags1;
			else if ((purchase >= 64) && (purchase < 96))
				flags_ptr = &a_ptr->flags2;
			else
				flags_ptr = &a_ptr->flags3;

			/* I don't already have this flag */
			if (!(*flags_ptr & (1L << (purchase % 32))))
			{
				/* I can afford to buy it */
				if (take_money(credit, get_cost_of_flag(purchase, val)))
				{
					*flags_ptr |= (1L << (purchase % 32));
				}
			}

			/* The heavy slays and brands cancel the light ones */
			if (purchase == KILL_DRAGON)
			{
				if (a_ptr->flags1 & (TR1_SLAY_DRAGON))
				{
					a_ptr->flags1 &= ~(TR1_SLAY_DRAGON);
					potential += get_cost_of_flag(SLAY_DRAGON, 0);
				}
			}
			else if (purchase == BRAND_FLAME)
			{
				if (a_ptr->flags1 & (TR1_BRAND_FIRE))
				{
					a_ptr->flags1 &= ~(TR1_BRAND_FIRE);
					potential += get_cost_of_flag(BRAND_FIRE, 0);
				}
			}
			else if (purchase == BRAND_VENOM)
			{
				if (a_ptr->flags1 & (TR1_BRAND_POIS))
				{
					a_ptr->flags1 &= ~(TR1_BRAND_POIS);
					potential += get_cost_of_flag(BRAND_POIS, 0);
				}
			}

			/* Done */
			return;
		}

		/* Handle special cases */
		switch (purchase)
		{
			case ADD_AC:
			{
				if (take_money(credit, val * 30))
				{
					a_ptr->to_a += val;
				}
				break;
			}
			case IMPROVE_BASE_AC:
			{
				if (take_money(credit, val * 30))
				{
					a_ptr->ac += val;
				}
				break;
			}
			case ENHANCE_DICE:
			{
				/* Allow a portion of the budget for improvements. */
				temp = potential / MAX(1, val);
				if (temp > 2500) temp = 2500;

				/* If credit is extended, guarantee something to work with. */
				if ((credit) && (temp < 500)) temp = 500;

				/* Subtract whatever portion is allotted from the budget. */
				if (potential > temp) potential -= temp;
				else potential = 0;


				/* Enhance the damage dice, depending on potential. */
				for (i = 0; i < 5; i++)
				{
					if ((one_in_(2)) && (temp >= 100 * (a_ptr->ds + 1)))
					{
						/* Do not go above base dice, plus 2 */
						if (a_ptr->dd < save_dd + 2)
						{
							a_ptr->dd++;

							/* Size of added die determines cost */
							temp -= 100 * (a_ptr->ds + 1);
						}
					}
					else if (temp >= 100 * a_ptr->dd)
					{
						/* Do not enlarge dice past twice normal */
						if (a_ptr->ds < save_ds * 2)
						{
							a_ptr->ds++;

							/* Number of dice enlarged determines cost */
							temp -= 100 * a_ptr->dd;
						}
					}
				}

				/* Pour any remainder back into the main budget. */
				potential += temp;

				break;
			}
			case ADD_SKILL:
			{
				if (take_money(credit, val * 30))
				{
					a_ptr->to_h += val;
				}
				break;
			}
			case ADD_DEADLINESS:
			{
				if (take_money(credit, val * 30))
				{
					a_ptr->to_d += val;
				}
				break;
			}
			default:
			{
				break;
			}
		}
	}

	/* Return */
	return;
}


/*
 * Get maximal potential of an artifact (this varies according to type).
 */
int get_max_potential(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int tval = a_ptr->tval;


	/* Some equipment types include no truly powerful artifacts. */
	if (tval == TV_CLOAK)
	{
		max_potential = 4500;
	}
	else if (tval == TV_GLOVES)
	{
		max_potential = 5000;
	}
	else if (is_missile_weapon(a_ptr))
	{
		max_potential = 3500;
	}
	else if (tval == TV_DIGGING)
	{
		max_potential = 3500;
	}
	else if (tval == TV_BOOTS)
	{
		/* Note that boots sometimes get a speed bonus on credit */
		max_potential = 4000;
	}
	else if ((tval == TV_SHOT) || (tval == TV_ARROW) || (tval == TV_BOLT))
	{
		max_potential = 2000;
	}
	else if (tval == TV_SHIELD)
	{
		max_potential = 6000;
	}
	else if ((is_any_weapon(a_ptr)) && (a_ptr->flags1 & (TR1_THROWING)))
	{
		max_potential = 3 * POTENTIAL_MAX / 5;
	}
	else
	{
		max_potential = POTENTIAL_MAX;
	}

	return (max_potential);
}



/*
 * Assign a tval and sval.  Currently, we make weapons, missiles, and
 * armor.
 *
 * Return maximum artifact potential.
 */
static void initialize_artifact(int a_idx, int tval0, int sval0)
{
	int i;
	int k_idx, freq;

	object_kind *k_ptr = NULL;
	artifact_type *a_ptr = &a_info[a_idx];


	/* Wipe the artifact */
	WIPE(a_ptr, artifact_type);


	/* We have not been assigned a tval */
	if (!tval0)
	{
		/* Assign a tval and sval. */
		while (TRUE)
		{
			/* Acquire an object at random */
			k_idx = rand_int(z_info->k_max);
			k_ptr = &k_info[k_idx];

			/* Skip "empty" objects */
			if (!k_ptr->name) continue;

			/* Skip objects that are not weapons, missiles, or armor */
			if (!is_wargear(k_ptr)) continue;

			/* Scan all three allocation chance values */
			for (freq = 0, i = 0; i < 3; i++)
			{
				/* Get the highest chance of object being made.  XXX */
				if (freq < k_ptr->chance[i]) freq = k_ptr->chance[i];
			}

			/* Hack -- we don't want many diggers or missiles */
			if ((k_ptr->tval == TV_DIGGING) || (is_missile(k_ptr)))
				freq /= 2;

			/* Accept object if it passes the rarity roll. */
			if ((freq >= 250) || (rand_int(250) < freq)) break;
		}
	}

	/* We've been assigned a tval */
	else
	{
		/* Find the object index */
		k_idx = lookup_kind(tval0, sval0);

		/* Paranoia -- require a valid index */
		if (!k_idx) return;

		/* Get the base object kind */
		k_ptr = &k_info[k_idx];
	}


	/* Save some values */
	save_ds = k_ptr->ds;
	save_dd = k_ptr->dd;

	/* Assume that the base object has no unusual power */
	power_of_base_object = 0;


	/* Determine "power" and get activation of base object. */
	switch (k_ptr->tval)
	{
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		{
			break;
		}

		case TV_HAFTED:
		{
			if (k_ptr->sval == SV_MACE_OF_DISRUPTION)
				power_of_base_object = 500;
			break;
		}

		case TV_POLEARM:
		{

			if (k_ptr->sval == SV_SCYTHE_OF_SLICING)
				power_of_base_object = 500;
			break;
		}

		case TV_SWORD:
		{
			if (k_ptr->sval == SV_FELLBLADE)
				power_of_base_object = 500;
			if (k_ptr->sval == SV_BLUESTEEL_BLADE)
				power_of_base_object = 500;
			if (k_ptr->sval == SV_BLADE_OF_CHAOS)
				power_of_base_object = 1000;
			break;
		}

		case TV_SHIELD:
		{
			if (k_ptr->sval == SV_SHIELD_OF_DEFLECTION)
				power_of_base_object = 500;
			break;
		}

		case TV_HARD_ARMOR:
		{
			if (k_ptr->sval == SV_MITHRIL_CHAIN_MAIL)
				power_of_base_object = 500;
			if (k_ptr->sval == SV_MITHRIL_PLATE_MAIL)
				power_of_base_object = 500;
			if (k_ptr->sval == SV_ADAMANTITE_PLATE_MAIL)
				power_of_base_object = 500;
			break;
		}

		case TV_DRAG_ARMOR:
		{
			if (k_ptr->sval == SV_DRAGON_BLACK)
			{
				power_of_base_object = 1000;
			}
			if (k_ptr->sval == SV_DRAGON_BLUE)
			{
				power_of_base_object = 1000;
			}
			if (k_ptr->sval == SV_DRAGON_WHITE)
			{
				power_of_base_object = 1000;
			}
			if (k_ptr->sval == SV_DRAGON_RED)
			{
				power_of_base_object = 1000;
			}
			if (k_ptr->sval == SV_DRAGON_GREEN)
			{
				power_of_base_object = 1500;
			}
			if (k_ptr->sval == SV_DRAGON_MULTIHUED)
			{
				power_of_base_object = 2000;
			}
			if (k_ptr->sval == SV_DRAGON_SHINING)
			{
				power_of_base_object = 1500;
			}
			if (k_ptr->sval == SV_DRAGON_BRONZE)
			{
				power_of_base_object = 1000;
			}
			if (k_ptr->sval == SV_DRAGON_GOLD)
			{
				power_of_base_object = 1000;
			}
			if (k_ptr->sval == SV_DRAGON_CHAOS)
			{
				power_of_base_object = 1500;
			}
			if (k_ptr->sval == SV_DRAGON_LAW)
			{
				power_of_base_object = 1500;
			}
			if (k_ptr->sval == SV_DRAGON_BALANCE)
			{
				power_of_base_object = 2500;
			}
			if (k_ptr->sval == SV_DRAGON_POWER)
			{
				power_of_base_object = 3000;
			}
			break;
		}

		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Seeker ammo needs to be controlled */
			if (k_ptr->sval == SV_AMMO_HEAVY)
			{
				power_of_base_object = 500;
			}
			break;
		}
	}


	/*
	 * Store the base values of a bunch of data.  To avoid unbalancing the
	 * game, bonuses to Skill, Deadliness, and Armor Class are cut in half.
	 * Dragon scale mail activations are preserved.  Base object cost is
	 * preserved if sufficiently high.
	 */
	a_ptr->tval = k_ptr->tval;
	a_ptr->sval = k_ptr->sval;

	a_ptr->to_h = k_ptr->to_h / 2;
	a_ptr->to_d = k_ptr->to_d / 2;
	a_ptr->to_a = k_ptr->to_a / 2;
	a_ptr->ac = k_ptr->ac;
	a_ptr->dd = k_ptr->dd;
	a_ptr->ds = k_ptr->ds;

	a_ptr->weight = k_ptr->weight;
	a_ptr->max_num = 1;

	a_ptr->pval1 = k_ptr->pval;
	a_ptr->flags_pval1 = k_ptr->flags_pval;

	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;

	a_ptr->activate = k_ptr->activate;


	/* Hack -- add IGNORE flags only for permanent artifacts */
	if (!tval0)
	{
		a_ptr->flags2 |= (TR2_IGNORE_ACID | TR2_IGNORE_ELEC |
		                  TR2_IGNORE_FIRE | TR2_IGNORE_COLD);
	}

	/* Maximum potential depends on type of artifact */
	(void)get_max_potential(a_idx);

	/* Powerful base objects don't get as many extra powers */
	max_potential -= power_of_base_object;

	/* Return */
	return;
}


/*
 * Grant a certain amount of potential to be used
 * for acquiring powers, and determine rarity and native depth.
 */
static void get_potential(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];

	object_kind *k_ptr = &k_info[lookup_kind(a_ptr->tval, a_ptr->sval)];

	long tmp, ratio;
	int artifact_depth;
	long artifact_rarity;
	int i;
	int freq;


	/* The total potential of an artifact ranges from 1500 to max_potential */
	initial_potential = potential = rand_range(1500, max_potential);

	/*
	 * The cost of the artifact depends on its potential (allowing
	 * for valuable base object kinds).
	 */
	a_ptr->cost = (long)potential * 10L;
	if (k_ptr->cost >= 5000) a_ptr->cost += k_ptr->cost;

	/* Handle the special case of missile and thrown weapons */
	if ((is_missile_weapon(a_ptr)) ||
		((is_any_weapon(a_ptr)) && (a_ptr->flags1 & (TR1_THROWING))))
	{
		a_ptr->cost += a_ptr->cost / 2;
	}


	/* Potential has more effect on depth for low-potential object kinds */
	ratio = 10L + (10L * POTENTIAL_MAX / max_potential);

	/* Calculate artifact depth */
	tmp = ((long)k_ptr->level / 4) + ((long)potential * ratio / 2000L);
	artifact_depth = (int)tmp;

	/* Boundary control. */
	if (artifact_depth <  5) artifact_depth =  5;
	if (artifact_depth > 99) artifact_depth = 99;


	/* Scan all three allocation chance values */
	for (freq = 0, i = 0; i < 3; i++)
	{
		/* Skip empty values. */
		if (k_ptr->chance[i] == 0) continue;

		/* Get the highest chance of object being made. */
		if (freq < k_ptr->chance[i]) freq = k_ptr->chance[i];
	}

	/* Calc artifact rarity (rarer base object - more common artifact) */
	artifact_rarity = ((potential + power_of_base_object) * freq / 12500L);

	/* Modify artifact rarity to handle some special cases. */
	if (a_ptr->tval == TV_CLOAK) artifact_rarity *= 2;
	if (a_ptr->tval == TV_BOOTS) artifact_rarity += 5;
	if (is_missile_weapon(a_ptr)) artifact_rarity += 5;

	/* Boundary control. */
	if (artifact_rarity <  4) artifact_rarity =  4;
	if (artifact_rarity > 65) artifact_rarity = 65;

	/* Assign the values just calculated to the artifact. */
	a_ptr->level = artifact_depth;
	a_ptr->rarity = (byte)artifact_rarity;
}



/*
 * Pick an initial set of qualities, based on a theme.  Also add a bonus to
 * armor class, Skill, and Deadliness.
 */
static void choose_basic_theme(int a_idx)
{
	int i, selection, temp, old_weight;
	artifact_type *a_ptr = &a_info[a_idx];
	object_kind *k_ptr = &k_info[lookup_kind(a_ptr->tval, a_ptr->sval)];


	/* Sometimes (not too often) make very powerful artifacts aggravate. */
	if ((potential > 4000) && (potential > rand_int(POTENTIAL_MAX * 3)))
	{
		a_ptr->flags3 |= (TR3_AGGRAVATE);
	}


	/* Frequently (but not always) assign a basic theme to the artifact. */
	selection = rand_int(100);

	switch (k_ptr->tval)
	{
		/* I'm a melee weapon... */
		case TV_SWORD: case TV_POLEARM: case TV_HAFTED:
		{
			/* ...with bonuses to Deadliness and Skill, and... */
			a_ptr->to_d += (rand_range(4, 10) + potential / 600);
			a_ptr->to_h += (rand_range(4, 10) + potential / 600);


			/* ... that is meant to be thrown */
			if (a_ptr->flags1 & (TR1_THROWING))
			{
				/* Must either be returning or multiple */
				if (one_in_(3))
				{
					get_quality(TRUE, RETURNING, 0, a_idx);
				}
				else
				{
					a_ptr->max_num = rand_range(4, 6);
				}

				/* Usually try to enhance the damage dice. */
				if (!one_in_(3)) get_quality(FALSE, ENHANCE_DICE, 3, a_idx);
			}

			/* ...of fire. */
			else if (selection < 7)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_FIRE3;
					else if (potential >= 2500)
						a_ptr->activate = ACTIV_RANDOM_FIRE2;
					else if (potential >= 1750)
						a_ptr->activate = ACTIV_RANDOM_FIRE1;
				}
				/* Brand the weapon with fire. */
				get_quality(FALSE, BRAND_FIRE, 0, a_idx);

				/* Grant either a resist or immunity to fire. */
				if (potential >= rand_range(4000, 10000))
				{
					get_quality(FALSE, IM_FIRE, 0, a_idx);
				}
				else get_quality(FALSE, RES_FIRE, 0, a_idx);
			}
			/* ...of frost. */
			else if (selection < 14)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_COLD3;
					else if (potential >= 2500)
						a_ptr->activate = ACTIV_RANDOM_COLD2;
					else if (potential >= 1750)
						a_ptr->activate = ACTIV_RANDOM_COLD1;
				}
				/* Brand the weapon with frost. */
				get_quality(FALSE, BRAND_COLD, 0, a_idx);

				/* Grant either a resist or immunity to frost. */
				if (potential >= rand_range(4000, 10000))
				{
					get_quality(FALSE, IM_COLD, 0, a_idx);
				}
				else get_quality(FALSE, RES_COLD, 0, a_idx);
			}
			/* ...of acid. */
			else if (selection < 19)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_ACID3;
					else if (potential >= 2500)
						a_ptr->activate = ACTIV_RANDOM_ACID2;
					else if (potential >= 1750)
						a_ptr->activate = ACTIV_RANDOM_ACID1;
				}
				/* Brand the weapon with acid. */
				get_quality(FALSE, BRAND_ACID, 0, a_idx);

				/* Grant either a resist or immunity to acid. */
				if (potential >= rand_range(4000, 10000))
				{
					get_quality(FALSE, IM_ACID, 0, a_idx);
				}
				else get_quality(FALSE, RES_ACID, 0, a_idx);
			}
			/* ...of electricity. */
			else if (selection < 24)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_ELEC3;
					else if (potential >= 2500)
						a_ptr->activate = ACTIV_RANDOM_ELEC2;
					else if (potential >= 1750)
						a_ptr->activate = ACTIV_RANDOM_ELEC1;
				}
				/* Brand the weapon with electricity. */
				get_quality(FALSE, BRAND_ELEC, 0, a_idx);

				/* Grant either a resist or immunity to electricity.*/
				if (potential >= rand_range(4000, 10000))
				{
					get_quality(FALSE, IM_ELEC, 0, a_idx);
				}
				else get_quality(FALSE, RES_ELEC, 0, a_idx);
			}
			/* ...of poison. */
			else if (selection < 28)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 4500)
						a_ptr->activate = ACTIV_RANDOM_POIS2;
					else if (potential >= 2000)
						a_ptr->activate = ACTIV_RANDOM_POIS1;
				}
				/* Brand the weapon with poison. */
				get_quality(FALSE, BRAND_POIS, 0, a_idx);

				/* Grant resistance to poison. */
				if (potential >= 2500) get_quality(FALSE, RES_POIS, 0, a_idx);
			}
			/* ...of life-sustaining. */
			else if (selection < 30)
			{
				/* This is an exclusive club. */
				if (potential < 3500) break;

				/* Possibly assign an activation for free. */
				if (one_in_(3))
				{
					a_ptr->activate = ACTIV_RANDOM_REGAIN;
				}

				/* Grant hold life. */
				get_quality(FALSE, HOLD_LIFE, 0, a_idx);

				/* Probably slay evil. */
				if (!one_in_(3))
					get_quality(FALSE, SLAY_EVIL, 0, a_idx);

				/* Possibly resist nether. */
				if (one_in_(3))
					get_quality(FALSE, RES_NETHR, 0, a_idx);

				/* Possibly see invisible. */
				if (one_in_(2))
					get_quality(FALSE, SEE_INVIS, 0, a_idx);

				/* Possibly slay undead. */
				if (one_in_(3))
					get_quality(FALSE, SLAY_UNDEAD, 0, a_idx);
			}
			/* ...of retain stats. */
			else if (selection < 32)
			{
				/* This is an exclusive club. */
				if (potential < 3500) break;

				/* Possibly assign an activation for free. */
				if (one_in_(3))
				{
					a_ptr->activate = ACTIV_RANDOM_RESTORE;
				}

				/* Grant resist nexus. */
				get_quality(FALSE, RES_NEXUS, 0, a_idx);

				/* Possibly resist disenchantment. */
				if (one_in_(3))
					get_quality(FALSE, RES_DISEN, 0, a_idx);

				/* And some sustains. */
				if (!one_in_(3))
					get_quality(TRUE, SUST_STR, 0, a_idx);
				if (!one_in_(3))
					get_quality(TRUE, SUST_WIS, 0, a_idx);
				if (!one_in_(3))
					get_quality(TRUE, SUST_INT, 0, a_idx);
				if (!one_in_(3))
					get_quality(TRUE, SUST_DEX, 0, a_idx);
				if (!one_in_(3))
					get_quality(TRUE, SUST_CON, 0, a_idx);
				if (!one_in_(3))
					get_quality(TRUE, SUST_CHR, 0, a_idx);
			}
			/* ...that shines with holy light. */
			else if (selection < 36)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 4500)
						a_ptr->activate = ACTIV_RANDOM_LIGHT2;
					else if (potential >= 2500)
						a_ptr->activate = ACTIV_RANDOM_LIGHT1;
				}

				/* Grant resist light and dark. */
				get_quality(TRUE, RES_LITE, 0, a_idx);
				get_quality(TRUE, RES_DARK, 0, a_idx);

				/* Possibly resist blindness. */
				if (one_in_(2))
					get_quality(FALSE, RES_BLIND, 0, a_idx);
			}
			/* ...with plenty of slays. */
			else if (selection < 41)
			{
				/* Load up on the slays. */
				if ((one_in_(6)) && (potential >= 2000))
				{
					get_quality(TRUE, SLAY_EVIL, 0, a_idx);
					get_quality(TRUE, SLAY_ORC, 0, a_idx);
					get_quality(TRUE, SLAY_TROLL, 0, a_idx);
					get_quality(TRUE, SLAY_GIANT, 0, a_idx);
				}
				else if ((one_in_(5)) && (potential >= 2000))
				{
					get_quality(TRUE, SLAY_EVIL, 0, a_idx);
					get_quality(TRUE, SLAY_UNDEAD, 0, a_idx);
					get_quality(TRUE, SLAY_DEMON, 0, a_idx);
				}
				else
				{
					if (one_in_(2))
						get_quality(FALSE, SLAY_ANIMAL, 0, a_idx);
					if (one_in_(2))
						get_quality(FALSE, SLAY_DRAGON, 0, a_idx);
					if (one_in_(4))
						get_quality(FALSE, SLAY_EVIL, 0, a_idx);
					if (one_in_(3))
						get_quality(FALSE, SLAY_ORC, 0, a_idx);
					if (one_in_(3))
						get_quality(FALSE, SLAY_TROLL, 0, a_idx);
					if (one_in_(3))
						get_quality(FALSE, SLAY_DEMON, 0, a_idx);
					if (one_in_(3))
						get_quality(FALSE, SLAY_GIANT, 0, a_idx);
				}
			}
			/* ...of wrath and fury. */
			else if (selection < 46)
			{
				/* Usually give the aggravate flag */
				if (!one_in_(3))
				{
					a_ptr->flags3 |= (TR3_AGGRAVATE);

					/* Reward */
					potential += 750;
				}

				/* Assign an activation to powerful weapons for free. */
				if (potential >= 3500)
				{
					a_ptr->activate = ACTIV_RANDOM_BERSERK;
				}

				/* Enhance the damage dice. */
				get_quality(FALSE, ENHANCE_DICE, 3, a_idx);
			}
			/* ...that a druid wants by his side. */
			else if (selection < 50)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential >= 2500))
				{
					a_ptr->activate = ACTIV_RANDOM_SLOW_FOE;
				}

				/* Provide regenerative powers. */
				get_quality(FALSE, REGEN, 0, a_idx);

				/* Mark the weapon for a later bonus to stealth. */
				add_pval_later |= (TR_PVAL_STEALTH);
			}
			/* ...that is just the thing a mage is looking for. */
			else if (selection < 54)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential >= 2500))
				{
					a_ptr->activate = ACTIV_RANDOM_SLEEP_FOE;
				}

				/* Provide resistance to blindness. */
				get_quality(FALSE, RES_BLIND, 0, a_idx);

				/* Possibly mark the weapon for a later bonus to magic mastery. */
				if (one_in_(3)) add_pval_later |= (TR_PVAL_DEVICE);
			}
			/* ...that a priest prays for. */
			else if (selection < 58)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential >= 2500))
				{
					a_ptr->activate = ACTIV_RANDOM_TURN_FOE;
				}

				/* Provide permanent light. */
				get_quality(FALSE, LITE, 0, a_idx);

				/* Bless the weapon. */
				get_quality(TRUE, BLESSED, 0, a_idx);

				/* Mark the weapon for a later bonus to wisdom. */
				add_pval_later |= (TR_PVAL_WIS);
			}
			/* ...that a necromancer would kill for. */
			else if (selection < 62)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential >= 2500))
				{
					a_ptr->activate = ACTIV_RANDOM_CONFU_FOE;
				}

				/* Provide resistance to confusion. */
				get_quality(FALSE, RES_CONFU, 0, a_idx);

				/* Mark the weapon for a later bonus to intelligence. */
				add_pval_later |= (TR_PVAL_INT);
			}
			/* ...twisted with chaos. */
			else if (selection < 65)
			{
				/* This is an exclusive club. */
				if (potential < 4000) break;

				/* Assign an activation for free. */
				a_ptr->activate = ACTIV_RANDOM_CHAOS;

				/* Resist chaos and disenchantment. */
				get_quality(TRUE, RES_CHAOS, 0, a_idx);
				get_quality(TRUE, RES_DISEN, 0, a_idx);
			}
			/* ...that is a strong champion of order. */
			else if (selection < 68)
			{
				/* This is an exclusive club. */
				if (potential < 4000) break;

				/* Assign an activation for free. */
				a_ptr->activate = ACTIV_RANDOM_SHARD_SOUND;

				/* Resist shards, sound, and confusion. */
				get_quality(TRUE, RES_SHARD, 0, a_idx);
				get_quality(TRUE, RES_SOUND, 0, a_idx);
				get_quality(TRUE, RES_CONFU, 0, a_idx);
			}
			/* ...that smashes foes and dungeon alike. */
			else if (selection < 72)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					a_ptr->activate = ACTIV_RANDOM_EARTHQUAKE;
				}

				/* 75% of the time, an impact brand. */
				if (!one_in_(4)) get_quality(TRUE, IMPACT, 0, a_idx);

				/* Enhance the damage dice. */
				get_quality(TRUE, ENHANCE_DICE, 3, a_idx);

				/* Grant either resistance or immunity to acid. */
				if ((potential >= 3500) && (one_in_(3)))
					get_quality(FALSE, IM_ACID, 0, a_idx);
				else get_quality(TRUE, RES_ACID, 0, a_idx);

				/* Mark the weapon for a later bonus to tunneling. */
				add_pval_later |= (TR_PVAL_TUNNEL);

				/* Sometimes mark the weapon for a later bonus to strength. */
				if ((potential >= 750) && (one_in_(2)))
				add_pval_later |= (TR_PVAL_STR);
			}
			/* ...that hunts down all the children of nature. */
			else if (selection < 76)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					a_ptr->activate = ACTIV_RANDOM_DETECT_MONSTERS;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_ANIMAL, 0, a_idx);

				/* A pair of survival skills. */
				get_quality(FALSE, REGEN, 0, a_idx);
				get_quality(FALSE, FEATHER, 0, a_idx);

				/* Mark the weapon for a later bonus to charisma. */
				add_pval_later |= (TR_PVAL_CHR);

				/* Sometimes mark the weapon for a later bonus to infravision. */
				if (one_in_(2))
					add_pval_later |= (TR_PVAL_INFRA);
			}
			/* ...that Orcs and Trolls must fear. */
			else if (selection < 79)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 3000)
						a_ptr->activate = ACTIV_RANDOM_STARBURST;
					else
						a_ptr->activate = ACTIV_RANDOM_LINE_LIGHT;
				}
				/* Naturally, the appropriate slays. */
				get_quality(TRUE, SLAY_ORC, 0, a_idx);
				get_quality(TRUE, SLAY_TROLL, 0, a_idx);

				/* Often, grant a bonus to ac. */
				if (one_in_(2))
					get_quality(FALSE, ADD_AC,
						randint(4) + potential / 1000, a_idx);

				/* Sometimes, slay giant. */
				if (one_in_(3))
					get_quality(FALSE, SLAY_GIANT, 0, a_idx);

				/* Mark the weapon for a later bonus to strength. */
				add_pval_later |= (TR_PVAL_STR);
			}
			/* ...that the undead cannot withstand. */
			else if (selection < 82)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential > 2500))
				{
					if (potential < 5000)
						a_ptr->activate = ACTIV_RANDOM_SMITE_UNDEAD;
					else
						a_ptr->activate = ACTIV_RANDOM_DISPEL_UNDEAD;
				}
				/* Grant slay undead and see invisible. */
				get_quality(TRUE, SLAY_UNDEAD, 0, a_idx);
				get_quality(TRUE, SEE_INVIS, 0, a_idx);

				/* Sometimes, hold life. */
				if (one_in_(3))
					get_quality(FALSE, HOLD_LIFE, 0, a_idx);

				/* Mark the weapon for a later bonus to wisdom. */
				add_pval_later |= (TR_PVAL_WIS);
			}
			/* ...that evil creatures everywhere flee from. */
			else if (selection < 90)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if ((potential >= 5000) && (one_in_(2)))
						a_ptr->activate = ACTIV_RANDOM_DISPEL_EVIL;
					else if (potential >= 5000)
						a_ptr->activate = ACTIV_RANDOM_BANISH_EVIL;
					else if ((potential >= 3500) && (!one_in_(3)))
						a_ptr->activate = ACTIV_RANDOM_HOLY_ORB;
					else if (potential >= 2000)
						a_ptr->activate = ACTIV_RANDOM_PROT_FROM_EVIL;
					else
						a_ptr->activate = ACTIV_RANDOM_DETECT_EVIL;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_EVIL, 0, a_idx);

				/* Bless the weapon if necessary. */
				if ((a_ptr->tval == TV_POLEARM) || (a_ptr->tval == TV_SWORD))
					get_quality(FALSE, BLESSED, 0, a_idx);

				/* Sometimes, resist nether. */
				if (one_in_(6))
					get_quality(FALSE, RES_NETHR, 0, a_idx);

				/* Sometimes, resist dark. */
				if (one_in_(6))
					get_quality(FALSE, RES_DARK, 0, a_idx);

				/* Possibly mark the weapon for a later bonus to intelligence. */
				if (one_in_(3))
					add_pval_later |= (TR_PVAL_INT);
			}
			/* ...that demons intensely hate. */
			else if (selection < 93)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential > 2500))
				{
					a_ptr->activate = ACTIV_RANDOM_SMITE_DEMON;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_DEMON, 0, a_idx);

				/* Sometimes, nip the spawn of hell with cold as well. */
				if (one_in_(2))
					get_quality(FALSE, BRAND_COLD, 0, a_idx);

				/* Grant resistance to fire. */
				get_quality(FALSE, RES_FIRE, 0, a_idx);

				/* Mark the weapon for a later bonus to dexterity. */
				add_pval_later |= (TR_PVAL_DEX);
			}
			/* ...that dragons long to destroy. */
			else if (selection < 96)
			{
				/* Possibly assign an activation for free. */
				if ((!one_in_(3)) && (potential > 2500))
				{
					a_ptr->activate = ACTIV_RANDOM_SMITE_DRAGON;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_DRAGON, 0, a_idx);

				/* And one of the five elemental brands. */
				temp = randint(7);
				if ((temp == 1) || (temp == 2))
					get_quality(FALSE, BRAND_FIRE, 0, a_idx);
				if ((temp == 3) || (temp == 4))
					get_quality(FALSE, BRAND_COLD, 0, a_idx);
				if (temp == 5)
					get_quality(FALSE, BRAND_ACID, 0, a_idx);
				if (temp == 6)
					get_quality(FALSE, BRAND_ELEC, 0, a_idx);
				if (temp == 7)
					get_quality(FALSE, BRAND_POIS, 0, a_idx);

				/* Mark the weapon for a later bonus to constitution. */
				add_pval_later |= (TR_PVAL_CON);
			}
			/* ...that protects the wielder. */
			else if (selection < 1000)
			{
				/* This is an exclusive club. */
				if (potential < 2500) break;

				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 5000)
						a_ptr->activate = ACTIV_RANDOM_SHIELD;
					else
						a_ptr->activate = ACTIV_RANDOM_BLESS;
				}

				/* Grant a bonus to armor class. */
				get_quality(TRUE, ADD_AC,
					randint(6) + potential / 800, a_idx);

				/* And the four basic resists. */
				get_quality(TRUE, RES_ACID, 0, a_idx);
				get_quality(TRUE, RES_ELEC, 0, a_idx);
				get_quality(TRUE, RES_FIRE, 0, a_idx);
				get_quality(TRUE, RES_COLD, 0, a_idx);

				/* And some of the survival abilities. */
				if (!one_in_(4))
					get_quality(TRUE, SEE_INVIS, 0, a_idx);
				if (!one_in_(4))
					get_quality(TRUE, FEATHER, 0, a_idx);
				if (!one_in_(4))
					get_quality(TRUE, FREE_ACT, 0, a_idx);
				if (!one_in_(4))
					get_quality(TRUE, REGEN, 0, a_idx);
			}

			break;
		}

		/* I'm a digger */
		case TV_DIGGING:
		{
			/* ...with bonuses to Deadliness and Skill, and... */
			a_ptr->to_d += (rand_range(3, 7) + potential / 650);
			a_ptr->to_h += (rand_range(3, 7) + potential / 650);

			/* Artifact diggers /always/ get a bonus to tunneling */
			add_pval_later |= (TR_PVAL_TUNNEL);

			break;
		}

		/* I'm a missile weapon... */
		case TV_SLING:
		case TV_BOW:
		case TV_CROSSBOW:
		{
			/* ...with bonuses to Deadliness and Skill, and... */
			a_ptr->to_d += (rand_range(4, 10) + potential / 350);
			a_ptr->to_h += (rand_range(4, 10) + potential / 350);

			/* Half of bows try for extra might or shots immediately */
			if      (selection <= 25)
				get_quality(FALSE, PVAL_MIGHT, -1, a_idx);

			else if (selection <= 50)
				get_quality(FALSE, PVAL_SHOTS, -1, a_idx);

			break;
		}

		/* I'm a piece of body armor... */
		case TV_SOFT_ARMOR: case TV_HARD_ARMOR: case TV_DRAG_ARMOR:
		{
			/* ...with a bonus to armor class, and... */
			a_ptr->to_a += (rand_range(5, 12) + potential / 500);


			/* ...that resists most or all of the elements. */
			if (selection < 30)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(2)) && (potential >= 3000))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_RESIST_ALL;
					else
						a_ptr->activate = ACTIV_RANDOM_RESIST_ELEMENTS;
				}

				if (!one_in_(5))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (!one_in_(5))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (!one_in_(5))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (!one_in_(5))
					get_quality(TRUE, RES_COLD, 0, a_idx);

				/* Sometimes, also poison. */
				if ((one_in_(2)) && (potential >= 2000))
					get_quality(FALSE, RES_POIS, 0, a_idx);
			}
			/* ...that protects the very soul. */
			else if (selection < 40)
			{
				/* This is an exclusive club. */
				if (potential < 5500) break;

				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (one_in_(2))
						a_ptr->activate = ACTIV_RANDOM_HEAL3;
					else
						a_ptr->activate = ACTIV_RANDOM_REGAIN;
				}

				/* Resist nether and hold life. */
				get_quality(FALSE, RES_NETHR, 0, a_idx);
				get_quality(FALSE, HOLD_LIFE, 0, a_idx);

				/* Sometimes also resist chaos. */
				if (one_in_(2))
					get_quality(FALSE, RES_CHAOS, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (one_in_(3))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...resistant to sound and confusion. */
			else if (selection < 50)
			{
				/* Resist sound and confusion. */
				get_quality(FALSE, RES_SOUND, 0, a_idx);
				get_quality(FALSE, RES_CONFU, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (one_in_(3))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...with an amazing armor class. */
			else if (selection < 62)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(3)) && (potential >= 4000))
				{
					a_ptr->activate = ACTIV_RANDOM_SHIELD;
				}

				/* Increase both base and magical ac. */
				get_quality(TRUE, ADD_AC,
					randint(3) + potential / 1600, a_idx);
				get_quality(TRUE, IMPROVE_BASE_AC,
					randint(3) + potential / 1600, a_idx);

				/* Collect a suite of basic resists. */
				if (one_in_(3))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that grants power over the realms of shadow. */
			else if (selection < 74)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (one_in_(2))
						a_ptr->activate = ACTIV_RANDOM_TELEPORT1;
					else
						a_ptr->activate = ACTIV_RANDOM_BLESS;
				}

				/* Resist dark. */
				get_quality(FALSE, RES_DARK, 0, a_idx);

				/* Mark the armor for a later bonus to stealth. */
				add_pval_later |= (TR_PVAL_STEALTH);

				/* Grant see invisible. */
				get_quality(FALSE, SEE_INVIS, 0, a_idx);

				/* Possibly resist nether. */
				if (one_in_(3))
					get_quality(FALSE, RES_NETHR, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (one_in_(3))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that protects against poison. */
			else if (selection < 82)
			{
				/* This is an exclusive club. */
				if (potential < 2500) break;

				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					a_ptr->activate = ACTIV_RANDOM_CURE;
				}

				/* Resist poison. */
				get_quality(FALSE, RES_POIS, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (one_in_(3))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that shines very brightly. */
			else if (selection < 95)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential < 4500)
						a_ptr->activate = ACTIV_RANDOM_LIGHT1;
					else
						a_ptr->activate = ACTIV_RANDOM_LIGHT2;
				}

				/* Grant permanent light. */
				get_quality(TRUE, LITE, 0, a_idx);

				/* And resistance to light. */
				get_quality(TRUE, RES_LITE, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (one_in_(3))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(3))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that contains the very essence of... */
			else if (selection < 100)
			{
				/* This is an exclusive club. */
				if (potential < 5500) break;

				temp = randint(4);

				/* ...fire. */
				if (temp == 1)
				{
					/* Assign an activation for free. */
					if (!one_in_(3))
					{
						a_ptr->activate = ACTIV_RANDOM_FIRE3;
					}

					/* Immunity. */
					get_quality(FALSE, IM_FIRE, 0, a_idx);
				}
				/* ...cold. */
				if (temp == 2)
				{
					/* Assign an activation for free. */
					if (!one_in_(3))
					{
						a_ptr->activate = ACTIV_RANDOM_COLD3;
					}

					/* Immunity. */
					get_quality(FALSE, IM_COLD, 0, a_idx);
				}
				/* ...acid. */
				if (temp == 3)
				{
					/* Assign an activation for free. */
					if (!one_in_(3))
					{
						a_ptr->activate = ACTIV_RANDOM_ACID3;
					}

					/* Immunity. */
					get_quality(FALSE, IM_ACID, 0, a_idx);
				}
				/* ...electricity. */
				if (temp == 4)
				{
					/* Assign an activation for free. */
					if (!one_in_(3))
					{
						a_ptr->activate = ACTIV_RANDOM_ELEC3;
					}

					/* Immunity. */
					get_quality(FALSE, IM_ELEC, 0, a_idx);
				}

				/* Collect a suite of basic resists. */
				if (one_in_(2))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (one_in_(2))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (one_in_(2))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (one_in_(2))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}

			break;
		}

		/* I'm a shield... */
		case TV_SHIELD:
		{
			/* ...with a bonus to armor class, and... */
			a_ptr->to_a += (rand_range(5, 12) + potential / 500);

			/* ...that resists most or all of the elements. */
			if (selection < 18)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(3)) && (potential >= 3000))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_RESIST_ALL;
					else
						a_ptr->activate = ACTIV_RANDOM_RESIST_ELEMENTS;
				}

				if (!one_in_(5))
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (!one_in_(5))
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (!one_in_(5))
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (!one_in_(5))
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that increases strength and constitution. */
			else if (selection < 30)
			{
				/* Mark the shield for a later bonus to constitution. */
				add_pval_later |= (TR_PVAL_CON);

				/* Mark the shield for a later bonus to strength. */
				add_pval_later |= (TR_PVAL_STR);
			}
			/* ...that resists shards (+ to ac). */
			else if (selection < 45)
			{
				/* Resist shards and increase base ac. */
				get_quality(TRUE, RES_SHARD, 0, a_idx);
				get_quality(TRUE, IMPROVE_BASE_AC,
					randint(3) + potential / 1200, a_idx);
			}
			/* ...that resists light and dark. */
			else if (selection < 55)
			{
				/* Grant resistance to light and dark. */
				get_quality(TRUE, RES_LITE, 0, a_idx);
				get_quality(TRUE, RES_DARK, 0, a_idx);
			}
			/* ...with runes of protection. */
			else if (selection < 85)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(3)) && (potential >= 4000))
				{
					a_ptr->activate = ACTIV_RANDOM_SHIELD;
				}

				/* Increase magical ac. */
				get_quality(TRUE, ADD_AC,
					randint(6) + potential / 2000, a_idx);

				/* Randomly add some survival-enhancing magics. */
				if (one_in_(4))
					get_quality(FALSE, REGEN, 0, a_idx);
				if (one_in_(4))
					get_quality(FALSE, FEATHER, 0, a_idx);
				if (one_in_(4))
					get_quality(FALSE, REGEN, 0, a_idx);
				if (one_in_(4))
					get_quality(FALSE, SEE_INVIS, 0, a_idx);
				if (one_in_(4))
					get_quality(FALSE, FREE_ACT, 0, a_idx);
				if (one_in_(4))
					get_quality(FALSE, RES_BLIND, 0, a_idx);
				if (one_in_(4))
					get_quality(FALSE, RES_CONFU, 0, a_idx);

			}

			break;
		}

		/* I'm a pair of boots... */
		case TV_BOOTS:
		{
			/* ...with a bonus to armor class, and... */
			a_ptr->to_a += (rand_range(5, 10) + potential / 700);

			/* ...that makes he who wears me run like the wind. */
			if (selection < 35)
			{
				/* This is an exclusive club. */
				if (potential < 3000) break;

				/* Calculate pval. */
				temp = ((potential >= 3500) ? 10 : 5);

				/* Hack -- Add a speed bonus immediately, on credit. */
				get_quality(TRUE, PVAL_SPEED, temp, a_idx);

				/* Hack -- allow some other powers */
				if (potential < 1000) potential = 1000;
			}
			/* ...that keeps a guy's feet on the ground. */
			else if (selection < 45)
			{
				/* Resist nexus and feather fall. */
				get_quality(TRUE, RES_NEXUS, 0, a_idx);
				get_quality(TRUE, FEATHER, 0, a_idx);
			}
			/* ...with unrivaled magical movement. */
			else if (selection < 55)
			{
				/* Assign an activation,... */
				if (potential >= 4000)
					a_ptr->activate = ACTIV_RANDOM_RECALL;
				else if (potential >= 2000)
					a_ptr->activate = ACTIV_RANDOM_TELEPORT2;
				else
					a_ptr->activate = ACTIV_RANDOM_TELEPORT1;

				/* ...but not for free. */
				potential = 2 * potential / 3;

				/* Resist nexus. */
				get_quality(FALSE, RES_NEXUS, 0, a_idx);

				/* Possibly mark the shield for a later bonus to dexterity. */
				if (potential >= 1000) add_pval_later |= (TR_PVAL_DEX);
			}
			/* ...that makes long marches easy. */
			else if (selection < 65)
			{
				/* Possibly assign an activation for free. */
				if (one_in_(2))
				{
					if (potential >= 5500)
						a_ptr->activate = ACTIV_RANDOM_HEAL3;
					else if (potential >= 3000)
						a_ptr->activate = ACTIV_RANDOM_HEAL2;
					else
						a_ptr->activate = ACTIV_RANDOM_HEAL1;
				}

				/* Grant regenerative powers. */
				get_quality(TRUE, REGEN, 0, a_idx);

				/* Mark the shield for a later bonus to constitution. */
				add_pval_later |= (TR_PVAL_CON);
			}
			/* ...that dance up a storm. */
			else if (selection < 75)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(2)) && (potential >= 2500))
					a_ptr->activate = ACTIV_RANDOM_STORM_DANCE;

				/* Grant feather fall. */
				get_quality(TRUE, FEATHER, 0, a_idx);

				/* Mark the boots for a later bonus to dexterity. */
				add_pval_later |= (TR_PVAL_DEX);
			}
			/* ...worn by a famous ranger of old. */
			else if (selection < 85)
			{
				/* Possibly assign an activation for free. */
				if (one_in_(2))
					a_ptr->activate = ACTIV_RANDOM_DETECT_MONSTERS;

				/* Grant regeneration and slow digest. */
				get_quality(TRUE, REGEN, 0, a_idx);
				get_quality(TRUE, SLOW_DIGEST, 0, a_idx);

				/* Possibly telepathy. */
				if (one_in_(6))
					get_quality(FALSE, TELEPATHY, 0, a_idx);
			}

			/* Possibly assign a speed activation for free. */
			if ((one_in_(4)) && (a_ptr->activate == 0) &&
				(potential >= 2000))
			{
				a_ptr->activate = ACTIV_RANDOM_SPEED;
			}

			break;
		}

		/* I'm a cloak... */
		case TV_CLOAK:
		{
			/* ...with a bonus to armor class, and... */
			a_ptr->to_a += (rand_range(5, 10) + potential / 600);


			/* ...that hides the wearer from hostile eyes. */
			if (selection < 20)
			{
				/* Possibly assign an activation for free. */
				if (one_in_(2))
					a_ptr->activate = ACTIV_RANDOM_SLEEP_FOES;

				/* Mark the cloak for a later bonus to stealth. */
				add_pval_later |= (TR_PVAL_STEALTH);
			}
			/* ...that confuses and dismays foes. */
			else if (selection < 30)
			{
				/* Possibly assign an activation for free. */
				if (one_in_(2))
				{
					if (one_in_(2))
						a_ptr->activate = ACTIV_RANDOM_CONFU_FOES;
					else
						a_ptr->activate = ACTIV_RANDOM_TURN_FOES;
				}

			}
			/* ...with amazing protective powers. */
			else if (selection < 40)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(4)) && (potential >= 3000))
				{
					a_ptr->activate = ACTIV_RANDOM_SHIELD;
				}

				/* Increase both base and magical ac. */
				get_quality(TRUE, ADD_AC,
					randint(3) + potential / 1600, a_idx);
				get_quality(TRUE, IMPROVE_BASE_AC,
					randint(3) + potential / 1600, a_idx);
			}
			/* ...that unmasks the locations of foes. */
			else if (selection < 50)
			{
				/* Assign an activation for free. */
				if (one_in_(2))
					a_ptr->activate = ACTIV_RANDOM_DETECT_MONSTERS;
				else
					a_ptr->activate = ACTIV_RANDOM_DETECT_EVIL;
			}
			/* ...that is aware of the world around. */
			else if (selection < 60)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if ((potential >= 4000) && (!one_in_(3)))
						a_ptr->activate = ACTIV_RANDOM_DETECT_ALL;
					else if (potential >= 3000)
						a_ptr->activate = ACTIV_RANDOM_MAGIC_MAP;
					else
						a_ptr->activate = ACTIV_RANDOM_DETECT_D_S_T;
				}

				/* Mark the boots for a later bonus to awareness. */
				add_pval_later |= (TR_PVAL_AWARE);
			}
			/* ...of necromantic powers. */
			else if (selection < 65)
			{
				/* Possibly assign an activation for free. */
				if (one_in_(2))
					a_ptr->activate = ACTIV_RANDOM_SLOW_FOES;

				/* Resist chaos, dark, or nether */
				temp = randint(3);
				if (temp == 1) get_quality(FALSE, RES_CHAOS, 0, a_idx);
				else if (temp == 2) get_quality(FALSE, RES_NETHR, 0, a_idx);
				else if (temp == 3) get_quality(FALSE, RES_DARK, 0, a_idx);

				/* Mark the boots for a later bonus to infravision. */
				add_pval_later |= (TR_PVAL_INFRA);
			}
			break;
		}

		/* I'm a helm or crown... */
		case TV_HELM: case TV_CROWN:
		{
			/* ...with a bonus to armor class, and... */
			a_ptr->to_a += (rand_range(5, 10) + potential / 800);


			/* ...of telepathy. */
			if (selection < 24)
			{
				/* This is an exclusive club. */
				if (potential < 3000) break;

				/* Grant telepathy. */
				get_quality(FALSE, TELEPATHY, 0, a_idx);
			}
			/* ...that maintains serenity amid uncertainly. */
			else if (selection < 35)
			{
				/* Possibly assign an activation for free. */
				if ((one_in_(2)) && (potential >= 4500))
					a_ptr->activate = ACTIV_RANDOM_CURE;

				/* Grant resistance to confusion and sound. */
				get_quality(FALSE, RES_CONFU, 0, a_idx);
				get_quality(FALSE, RES_SOUND, 0, a_idx);
			}
			/* ...preservative of sight and awareness. */
			else if (selection < 46)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 3750)
						a_ptr->activate = ACTIV_RANDOM_DETECT_ALL;
					else
						a_ptr->activate = ACTIV_RANDOM_DETECT_D_S_T;
				}

				/* Grant resistance to blindness and see invisible. */
				get_quality(FALSE, RES_BLIND, 0, a_idx);
				get_quality(FALSE, SEE_INVIS, 0, a_idx);

				/* Mark the headpiece for a later bonus to awareness. */
				add_pval_later |= (TR_PVAL_AWARE);
			}
			/* ...whose wearer stands taller in the eyes of men. */
			else if (selection < 57)
			{
				/* Mark the headpiece for a later bonus to wisdom. */
				add_pval_later |= (TR_PVAL_WIS);

				/* Mark the headpiece for a later bonus to charisma. */
				add_pval_later |= (TR_PVAL_CHR);

				/* Possibly add a bonus to base ac. */
				if (one_in_(3))
					get_quality(FALSE, IMPROVE_BASE_AC, rand_range(3, 4), a_idx);
			}
			/* ...strong in battle. */
			else if (selection < 68)
			{
				/* Possibly assign an activation for free. */
				if ((potential >= 2500) && (!one_in_(3)))
					a_ptr->activate = ACTIV_RANDOM_FRIGHTEN_ALL;
				else
					a_ptr->activate = ACTIV_RANDOM_HEROISM;

				/* No fear. */
				get_quality(TRUE, RES_FEAR, 0, a_idx);

				/* Mark the headpiece for a later bonus to strength. */
				add_pval_later |= (TR_PVAL_STR);

				/* Mark the headpiece for a later bonus to dexterity. */
				add_pval_later |= (TR_PVAL_DEX);

				/* Sustain dexterity and strength. */
				get_quality(TRUE, SUST_DEX, 0, a_idx);
				get_quality(TRUE, SUST_STR, 0, a_idx);
			}
			/* ...to whose wearer is revealed many secrets. */
			else if (selection < 79)
			{
				/* This is an exclusive club. */
				if (potential < 2500) break;

				/* Assign an activation for free. */
				if (potential < 4000) a_ptr->activate = ACTIV_RANDOM_IDENTIFY;
				else a_ptr->activate = ACTIV_RANDOM_SELF_KNOWLEDGE;
			}
			/* ...which can focus healing magics. */
			else if (selection < 90)
			{
				/* Possibly assign an activation for free. */
				if (!one_in_(3))
				{
					if (potential >= 4000)
						a_ptr->activate = ACTIV_RANDOM_HEAL3;
					else if (potential >= 2500)
						a_ptr->activate = ACTIV_RANDOM_HEAL2;
					else
						a_ptr->activate = ACTIV_RANDOM_HEAL1;
				}

				/* Grant regeneration. */
				get_quality(TRUE, REGEN, 0, a_idx);
			}

			break;
		}

		/* I'm a pair of gloves... */
		case TV_GLOVES:
		{
			/* ...with a bonus to armor class, and... */
			a_ptr->to_a += (rand_range(5, 10) + potential / 800);


			/* ...that grant increased combat prowess. */
			if (selection < 30)
			{
				/* Grant equal bonuses to Skill and Deadliness. */
				temp = 4 + (randint(4) * 2);
				get_quality(TRUE, ADD_DEADLINESS, temp, a_idx);
				get_quality(TRUE, ADD_SKILL, temp, a_idx);

				/* Often, acquire free action. */
				if (!one_in_(3))
					get_quality(TRUE, FREE_ACT, 0, a_idx);
			}
			/* ...with the dauntless spirit of a mighty warrior. */
			else if (selection < 45)
			{
				/* No fear. */
				get_quality(TRUE, RES_FEAR, 0, a_idx);

				/* Often, grant regeneration. */
				if (!one_in_(3))
					get_quality(FALSE, REGEN, 0, a_idx);

				/* Sometimes, acquire free action. */
				if (one_in_(2))
					get_quality(FALSE, FREE_ACT, 0, a_idx);


				/* Mark one of the combat stats for later acquisition. */
				if (one_in_(3)) add_pval_later |= (TR_PVAL_STR);
				else if (one_in_(2)) add_pval_later |= (TR_PVAL_DEX);
				else add_pval_later |= (TR_PVAL_CON);

				/* Possibly grant equal bonuses to Skill and Deadliness. */
				if ((potential >= 1500) && (!one_in_(3)))
				{
					temp = 4 + (randint(3) * 2);
					get_quality(TRUE, ADD_DEADLINESS, temp, a_idx);
					get_quality(TRUE, ADD_SKILL, temp, a_idx);
				}
			}
			/* ...able to protect the wearer. */
			else if (selection < 60)
			{
				/* Increase bonus to AC. */
				get_quality(FALSE, ADD_AC, rand_range(4, 8), a_idx);

				/* Grant (usually) two basic resists. */
				for (i = 0; i < 2; i++)
				{
					temp = randint(4);
					if (temp == 1) get_quality(FALSE, RES_FIRE, 0, a_idx);
					if (temp == 2) get_quality(FALSE, RES_COLD, 0, a_idx);
					if (temp == 3) get_quality(FALSE, RES_ACID, 0, a_idx);
					if (temp == 4) get_quality(FALSE, RES_ELEC, 0, a_idx);
				}

				/* Sometimes add a bolt activation for free. */
				if ((potential >= 500) && (!one_in_(3)))
				{
					if (temp == 1)
						a_ptr->activate = ACTIV_RANDOM_FIRE1;
					if (temp == 2)
						a_ptr->activate = ACTIV_RANDOM_COLD1;
					if (temp == 3)
						a_ptr->activate = ACTIV_RANDOM_ACID1;
					if (temp == 4)
						a_ptr->activate = ACTIV_RANDOM_ELEC1;
				}
			}
			/* ...with the cunning of a rogue of legend. */
			else if (selection < 75)
			{
				/* Assign an activation for free. */
				a_ptr->activate = ACTIV_RANDOM_DISARM;

				/* Mark the gloves for a later bonus to stealth. */
				add_pval_later |= (TR_PVAL_STEALTH);

				/* Mark the gloves for a later bonus to awareness. */
				add_pval_later |= (TR_PVAL_AWARE);

				/* Often, acquire free action. */
				if (!one_in_(3))
					get_quality(FALSE, FREE_ACT, 0, a_idx);
			}
			/* ...that untangles magical conundrums. */
			else if (selection < 80)
			{
				/* Mark the gloves for a later bonus to magic item mastery. */
				add_pval_later |= (TR_PVAL_DEVICE);
			}
			/* ...with a deadly mastery of archery. */
			else if (selection < 100)
			{
				/* This is an exclusive club. */
				if (potential < 3500) break;

				/* Always grant an activation, but not for free. */
				a_ptr->activate = ACTIV_RANDOM_DEADLY_SHOOTING;
				potential -= 900;

				/* Add equal bonuses to Skill and to Deadliness. */
				temp = 4 + (randint(3) * 2);
				get_quality(TRUE, ADD_DEADLINESS, temp, a_idx);
				get_quality(TRUE, ADD_SKILL, temp, a_idx);

				/* Often, acquire free action. */
				if (!one_in_(3))
					get_quality(FALSE, FREE_ACT, 0, a_idx);
			}
			break;
		}

		/* Ammunition */
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			/* Ammunition always comes in (small) groups */
			a_ptr->max_num = rand_range(8, 12);

			/* Usually try to enhance the damage dice. */
			if (!one_in_(4)) get_quality(FALSE, ENHANCE_DICE, 3, a_idx);

			/* Bonuses to Deadliness and Skill */
			a_ptr->to_d += (rand_range(4, 10) + potential / 600);
			a_ptr->to_h += (rand_range(4, 10) + potential / 600);

			break;
		}

		default:
		{
			msg_print("Error in random artifact generation.  Object kind not recognized.");
			break;
		}
	}

	/* It is possible for melee weapons to be unusually heavy or light. */
	if ((is_melee_weapon(a_ptr)) && (a_ptr->weight >= 40) && (one_in_(6)))
	{
		old_weight = a_ptr->weight;

		/* Sometimes, they are unusually heavy. */
		if (one_in_(2))
		{
			a_ptr->weight += (a_ptr->weight / 40) * 10;
		}

		/* Sometimes, they are unusually light. */
		else
		{
			a_ptr->weight -= (a_ptr->weight / 40) * 10;
		}

		potential += (a_ptr->weight - old_weight) * 10;
	}
}


/*
 * Grant activations.
 */
static void grant_activation(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int choice;


	/* We already have an activation */
	if (a_ptr->activate) return;


	/* We're a missile launcher with a reasonable potential */
	if ((is_missile_weapon(a_ptr)) && (potential >= rand_range(750, 1250)))
	{
		/* Choose an activation */
		if (potential > rand_range(2000, 2500)) choice = randint(12);
		else                                    choice = randint(6);

		if      (choice == 1) a_ptr->activate = ACTIV_SHOT_PIERCING;
		else if (choice == 2) a_ptr->activate = ACTIV_SHOT_DAMAGE;
		else if (choice == 3) a_ptr->activate = ACTIV_SHOT_IMPACT;
		else if (choice == 4) a_ptr->activate = ACTIV_SHOT_ACCURATE;
		else if (choice == 5) a_ptr->activate = ACTIV_SHOT_FIRE;
		else if (choice == 6) a_ptr->activate = ACTIV_SHOT_COLD;
		else if (choice <= 9) a_ptr->activate = ACTIV_RANDOM_DEADLY_SHOOTING;
		else                  a_ptr->activate = ACTIV_RANDOM_BRAND_MISSILE;

		/* Reduce potential */
		if (choice > 6) potential -= 750;
		else            potential -= 500;

		/* Try for some themed qualities */
		if (!one_in_(4))
		{
			if (a_ptr->activate == ACTIV_RANDOM_DEADLY_SHOOTING)
			{
				get_quality(FALSE, PVAL_MIGHT, -1, a_idx);
			}
			else if (a_ptr->activate == ACTIV_SHOT_ACCURATE)
			{
				get_quality(FALSE, PVAL_SHOTS, -1, a_idx);
			}
			else if (a_ptr->activate == ACTIV_SHOT_FIRE)
			{
				get_quality(FALSE, RES_FIRE, -1, a_idx);
			}
			else if (a_ptr->activate == ACTIV_SHOT_COLD)
			{
				get_quality(FALSE, RES_COLD, -1, a_idx);
			}
		}
	}
}

/*
 * Grant or enhance pval-dependant attributes.
 */
static void grant_pval(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int quality = -1;

	/* Missile weapons concentrate on power-enhancement */
	if ((is_missile_weapon(a_ptr)) && (one_in_(3)))
	{
		if (one_in_(2)) quality = PVAL_MIGHT;
		else            quality = PVAL_SHOTS;
	}

	/* Try to add another pval-dependant quality */
	else
	{
		/* Choose */
		int choice;

		/* Some types of artifacts more often grant "mental" abilities */
		bool mindgear = FALSE;
		if ((a_ptr->tval == TV_GLOVES) || (a_ptr->tval == TV_HELM) ||
		    (a_ptr->tval == TV_CROWN)) mindgear = TRUE;

		/* Only melee weapons can get a bonus to certain things. */
		if (is_melee_weapon(a_ptr)) choice = randint(37);
		else                        choice = randint(32);

		/* Choose */
		if      (choice <=  3) quality = PVAL_STR;
		else if (choice <=  6) quality = PVAL_INT;
		else if (choice <=  9) quality = PVAL_WIS;
		else if (choice <= 12) quality = PVAL_DEX;
		else if (choice <= 15) quality = PVAL_CON;
		else if (choice <= 17) quality = PVAL_CHR;
		else if (choice <= 19) quality = PVAL_STEALTH;
		else if (choice <= 20) quality = PVAL_AWARE;
		else if (choice <= 21) quality = PVAL_INFRA;
		else if (choice <= 22)
		{
			if ((a_ptr->tval == TV_CLOAK) || (one_in_(3)))
				                 quality = PVAL_INVIS;
		}
		else if (choice <= 25)
		{
			if (mindgear)       quality = PVAL_DISARM;
		}
		else if (choice <= 27)
		{
			if (mindgear)       quality = PVAL_DEVICE;
		}
		else if (choice <= 28) quality = PVAL_SPEED;
		else if (choice <= 29) quality = PVAL_SAVE;
		else if (choice <= 30) quality = PVAL_LIGHT;
		else if (choice <= 31)
		{
			if ((mindgear) || (one_in_(2))) quality = PVAL_MANA;
		}
		else if (choice <= 33) quality = PVAL_TUNNEL;
		else if (choice <= 36) quality = PVAL_BLOWS;
	}

	/* Grant (or enhance) this attribute */
	get_quality(FALSE, quality, -1, a_idx);
}


/*
 * Grant resistances
 */
static void grant_resist(int a_idx)
{
	int choice;

	/* Range of choices depends on wealth */
	int range = 33;
	if (potential >= rand_range(2500, 3500)) range = 44;
	if (potential >= rand_range(3500, 5500)) range = 50;

	/* Make a choice */
	choice = randint(range);

	if      (choice <=  4) get_quality(FALSE, RES_ACID,  0, a_idx);
	else if (choice <=  8) get_quality(FALSE, RES_ELEC,  0, a_idx);
	else if (choice <= 13) get_quality(FALSE, RES_FIRE,  0, a_idx);
	else if (choice <= 18) get_quality(FALSE, RES_COLD,  0, a_idx);

	else if (choice <= 20) get_quality(FALSE, RES_LITE,  0, a_idx);
	else if (choice <= 22) get_quality(FALSE, RES_DARK,  0, a_idx);
	else if (choice <= 23) get_quality(FALSE, RES_FEAR,  0, a_idx);
	else if (choice <= 25) get_quality(FALSE, RES_BLIND, 0, a_idx);
	else if (choice <= 27) get_quality(FALSE, RES_CONFU, 0, a_idx);
	else if (choice <= 29) get_quality(FALSE, RES_SOUND, 0, a_idx);
	else if (choice <= 31) get_quality(FALSE, RES_SHARD, 0, a_idx);
	else if (choice <= 33) get_quality(FALSE, RES_NEXUS, 0, a_idx);

	/* Higher-level resists */
	else if (choice <= 35) get_quality(FALSE, RES_POIS,  0, a_idx);
	else if (choice <= 37) get_quality(FALSE, RES_NETHR, 0, a_idx);
	else if (choice <= 39) get_quality(FALSE, RES_CHAOS, 0, a_idx);
	else if (choice <= 40) get_quality(FALSE, RES_DISEN, 0, a_idx);

	/* Combined resists */
	else if (choice <= 44)
	{
		get_quality(TRUE, RES_ACID, 0, a_idx);
		get_quality(TRUE, RES_ELEC, 0, a_idx);
		get_quality(TRUE, RES_FIRE, 0, a_idx);
		get_quality(TRUE, RES_COLD, 0, a_idx);
	}

	/* The really cool stuff */
	else if (choice <= 45)
	{
		get_quality(TRUE, RES_NETHR, 0, a_idx);
		get_quality(TRUE, RES_DARK,  0, a_idx);
		get_quality(TRUE, HOLD_LIFE, 0, a_idx);
		if (one_in_(4))
			get_quality(TRUE, RES_DISEN, 0, a_idx);
	}
	else if (choice <= 46)
	{
		get_quality(TRUE, RES_CONFU, 0, a_idx);
		get_quality(TRUE, RES_CHAOS, 0, a_idx);
		if (one_in_(2))
			get_quality(TRUE, RES_NEXUS, 0, a_idx);
		if (one_in_(4))
			get_quality(TRUE, RES_DISEN, 0, a_idx);
	}

	/* Immunities */
	else if (choice <= 47) get_quality(FALSE, IM_ACID, 0, a_idx);
	else if (choice <= 48) get_quality(FALSE, IM_ELEC, 0, a_idx);
	else if (choice <= 49) get_quality(FALSE, IM_FIRE, 0, a_idx);
	else if (choice <= 50) get_quality(FALSE, IM_COLD, 0, a_idx);
}


/*
 * Grant attributes
 */
static void grant_attribute(int a_idx)
{
	int choice;

	/* Range of choices depends on wealth */
	int range = 18;
	if (potential >= rand_range(3000, 4000)) range = 21;

	/* Make a choice */
	choice = randint(range);

	if      (choice <=  2) get_quality(FALSE, SLOW_DIGEST,  0, a_idx);
	else if (choice <=  4) get_quality(FALSE, FEATHER,      0, a_idx);
	else if (choice <=  6) get_quality(FALSE, LITE,         0, a_idx);
	else if (choice <=  7) get_quality(FALSE, REGEN,        0, a_idx);
	else if (choice <= 10) get_quality(FALSE, SEE_INVIS,    0, a_idx);
	else if (choice <= 12) get_quality(FALSE, FREE_ACT,     0, a_idx);

	else if (choice <= 13) get_quality(FALSE, SUST_STR,     0, a_idx);
	else if (choice <= 14) get_quality(FALSE, SUST_INT,     0, a_idx);
	else if (choice <= 15) get_quality(FALSE, SUST_WIS,     0, a_idx);
	else if (choice <= 16) get_quality(FALSE, SUST_DEX,     0, a_idx);
	else if (choice <= 17) get_quality(FALSE, SUST_CON,     0, a_idx);
	else if (choice <= 18) get_quality(FALSE, SUST_CHR,     0, a_idx);

	/* More powerful attributes */
	else if (choice == 19) get_quality(FALSE, HOLD_LIFE,    0, a_idx);
	else if (choice == 20) get_quality(FALSE, TELEPATHY,    0, a_idx);
	else if (choice == 21)
	{
		get_quality(TRUE, SUST_STR, 0, a_idx);
		get_quality(TRUE, SUST_WIS, 0, a_idx);
		get_quality(TRUE, SUST_INT, 0, a_idx);
		get_quality(TRUE, SUST_DEX, 0, a_idx);
		get_quality(TRUE, SUST_CON, 0, a_idx);
		get_quality(TRUE, SUST_CHR, 0, a_idx);
	}
}


/*
 * Grant extra abilities, until object's units of exchange are all used up.
 * This function can be quite random - indeed needs to be - because of all
 * the possible themed random artifacts.
 */
static void haggle_till_done(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int pval;
	int choice;


	/* If qualities that depend on pval have been promised, add them. */
	if (add_pval_later)
	{
		int i;

		/* Scan all the possible flags */
		for (i = 0; i < 32; i++)
		{
			/* If we have this flag marked... */
			if (add_pval_later & (1L << i))
			{
				/* Try to add it (on credit) */
				get_quality(TRUE, i, -1, a_idx);
			}
		}

		/* Clear values */
		add_pval_later = 0L;
	}


	/*** Handle melee weapons - not diggers ***/
	if ((a_ptr->tval == TV_SWORD) || (a_ptr->tval == TV_POLEARM) ||
	    (a_ptr->tval == TV_HAFTED))
	{
		/* Handle throwing weapons. */
		if (a_ptr->flags1 & (TR1_THROWING))
		{
			/* Perfect balance and permanent light are favoured */
			if (one_in_(2)) get_quality(FALSE, PERFECT_BALANCE, 0, a_idx);
			if (one_in_(3)) get_quality(FALSE, LITE, 0, a_idx);
		}

		/* Sometimes weapons specialize in pure combat power. */
		if ((potential >= 1000) && (one_in_(4)))
		{
			/* Such weapons may be unusually heavy. */
			if (one_in_(3))
			{
				a_ptr->weight += 10 + (a_ptr->weight / 30) * 10;
				potential += 500;
			}

			/* Such weapons may be usually light. */
			else if (one_in_(2))
			{
				a_ptr->weight -= (a_ptr->weight / 20) * 10;
				potential -= 500;
			}

			/* Often spend a lot to enhance the damage dice. */
			if (!one_in_(3))
			{
				/* Probably sacrifice the Skill bonus. */
				if (!one_in_(3))
				{
					a_ptr->to_h = 0;
					potential += 600;

					/* Possibly also sacrifice the Deadliness bonus. */
					if (one_in_(3))
					{
						a_ptr->to_d = 0;
						potential += 600;
					}
				}
				get_quality(FALSE, ENHANCE_DICE, rand_range(2, 3), a_idx);
			}
		}

		/* Weapon has enough power */
		if (initial_potential > 2500)
		{
			/* Try to add the soulsteal flag */
			if (one_in_(20))
			{
				get_quality(FALSE, SOULSTEAL, 0, a_idx);
				potential += 1000;
			}

			/* Try to add the nomagic flag */
			if (one_in_(20))
			{
				get_quality(FALSE, NOMAGIC, 0, a_idx);
				potential += 1500;
			}
		}
	}


	/* On rare occasion, add all the stats */
	if ((potential >= 4500) && (!is_missile(a_ptr)) && (one_in_(15)))
	{
		get_quality(TRUE, PVAL_STR, 1, a_idx);
		get_quality(TRUE, PVAL_WIS, 1, a_idx);
		get_quality(TRUE, PVAL_INT, 1, a_idx);
		get_quality(TRUE, PVAL_DEX, 1, a_idx);
		get_quality(TRUE, PVAL_CON, 1, a_idx);
		get_quality(TRUE, PVAL_CHR, 1, a_idx);
		return;
	}

	/* On rare occasion, go for a major bonus to speed */
	if ((potential >= 4000) && (!is_missile(a_ptr))  && (one_in_(12)))
	{
		if (((is_melee_weapon(a_ptr)) && (one_in_(3))) ||
			 (a_ptr->tval == TV_BOOTS))
		{
			if (potential >= (a_ptr->tval == TV_BOOTS) ? 4000 : 8000) pval = 10;
			else if (potential >= (a_ptr->tval == TV_BOOTS) ? 3500 : 7000) pval = 8;
			else pval = 5;

			get_quality(TRUE, PVAL_SPEED, pval, a_idx);
			if (potential < 750) potential = 750;
		}
	}

	/* Any weapon can be vorpal/concussive/piercing */
	if (is_any_weapon(a_ptr))
	{
		/* Try to add the vorpal flag */
		if (one_in_(6)) get_quality(FALSE, VORPAL, 0, a_idx);
	}

	/*** Now, we enter Filene's Basement, and shop 'til we drop! ***/
	while (potential >= 300)
	{
		/* I'm any melee (or throwing) weapon */
		if (is_melee_weapon(a_ptr))
		{
			/* Load up on the slays and brands */
			if (!one_in_(3))
			{
				/* Collect a slay or brand, if it is affordable. */
				if (potential >= 3500) choice = randint(23);
				else                   choice = randint(22);

				if      (choice <=  2) get_quality(FALSE, SLAY_ANIMAL, 0, a_idx);
				else if (choice <=  4) get_quality(FALSE, SLAY_EVIL,   0, a_idx);
				else if (choice <=  6) get_quality(FALSE, SLAY_UNDEAD, 0, a_idx);
				else if (choice <=  8) get_quality(FALSE, SLAY_DEMON,  0, a_idx);
				else if (choice <=  9) get_quality(FALSE, SLAY_ORC,    0, a_idx);
				else if (choice <= 11) get_quality(FALSE, SLAY_TROLL,  0, a_idx);
				else if (choice <= 13) get_quality(FALSE, SLAY_GIANT,  0, a_idx);
				else if (choice <= 15)
				{
					if (!(a_ptr->flags1 & (TR1_KILL_DRAGON)))
						get_quality(FALSE, SLAY_DRAGON, 0, a_idx);
				}
				else if (choice <= 17)
				{
					if (!(a_ptr->flags1 & (TR1_BRAND_FLAME)))
						get_quality(FALSE, BRAND_FIRE,  0, a_idx);
				}
				else if (choice <= 19) get_quality(FALSE, BRAND_COLD,  0, a_idx);
				else if (choice <= 21)
				{
					if (one_in_(3))
					{
						/* 2/3rds of normal abundance */
						get_quality(FALSE, BRAND_ACID, 0, a_idx);
					}

					else if (one_in_(2))
					{
						/* 2/3rds of normal abundance */
						get_quality(FALSE, BRAND_ELEC, 0, a_idx);
					}
					else
					{
						/* 2/3rds of normal abundance */
						if (!(a_ptr->flags1 & (TR1_BRAND_VENOM)))
							get_quality(FALSE, BRAND_POIS,  0, a_idx);
					}
				}

				else if (choice == 22) get_quality(FALSE, IMPACT, 0, a_idx);

				/* The heavy brands and slays are rare */
				else if (choice == 23)
				{
					if (one_in_(3))
					{
						get_quality(FALSE, KILL_DRAGON, 0, a_idx);
					}
					else if (one_in_(2))
					{
						get_quality(FALSE, BRAND_FLAME, 0, a_idx);
					}
					else
					{
						get_quality(FALSE, BRAND_VENOM, 0, a_idx);
					}
				}
			}

			/* Often, collect a miscellaneous quality, if it is affordable. */
			if (one_in_(2))
			{
				grant_attribute(a_idx);
			}

			/* Often, collect a pval-dependant quality, if it is affordable. */
			if (one_in_(2))
			{
				grant_pval(a_idx);
			}

			/* Sometimes, collect a resistance, if it is affordable. */
			if (one_in_(4))
			{
				grant_resist(a_idx);
			}

			/* Clean out the wallet. */
			if ((potential < 500) && (one_in_(3)))
			{
				if (a_ptr->to_d > 0) a_ptr->to_d += potential / 150;
				if (a_ptr->to_h > 0) a_ptr->to_h += potential / 150;
				potential = 0;
			}
		}

		/* I'm a bow */
		else if (is_missile_weapon(a_ptr))
		{
			/* Sometimes, collect a miscellaneous quality, if it is affordable. */
			if (one_in_(3))
			{
				grant_attribute(a_idx);
			}

			/* Collect a pval-dependant quality, if it is affordable. */
			grant_pval(a_idx);

			/* Often, collect a special missile launcher activation */
			if (one_in_(2))
			{
				grant_activation(a_idx);
			}

			/* Rarely, collect a resistance, if it is affordable. */
			if (one_in_(6))
			{
				grant_resist(a_idx);
			}

			/* Clean out the wallet. */
			if ((potential < 500) && (one_in_(3)))
			{
				if (a_ptr->to_d > 0) a_ptr->to_d += potential / 150;
				if (a_ptr->to_h > 0) a_ptr->to_h += potential / 150;
				potential = 0;
			}
		}


		/* I'm ammunition */
		else if (is_missile(a_ptr))
		{
			/* Collect a slay or brand, if it is affordable. */
			choice = randint(30);

			if      (choice <=  2) get_quality(FALSE, SLAY_ANIMAL, 0, a_idx);
			else if (choice <=  4) get_quality(FALSE, SLAY_EVIL,   0, a_idx);
			else if (choice <=  6) get_quality(FALSE, SLAY_UNDEAD, 0, a_idx);
			else if (choice <=  8) get_quality(FALSE, SLAY_DEMON,  0, a_idx);
			else if (choice <=  9) get_quality(FALSE, SLAY_ORC,    0, a_idx);
			else if (choice <= 10) get_quality(FALSE, SLAY_TROLL,  0, a_idx);
			else if (choice <= 11) get_quality(FALSE, SLAY_GIANT,  0, a_idx);
			else if (choice <= 13) get_quality(FALSE, SLAY_DRAGON, 0, a_idx);
			else if (choice <= 15) get_quality(FALSE, BRAND_FIRE,  0, a_idx);
			else if (choice <= 17) get_quality(FALSE, BRAND_COLD,  0, a_idx);
			else if (choice == 18) get_quality(FALSE, BRAND_ACID,  0, a_idx);
			else if (choice == 19) get_quality(FALSE, BRAND_ELEC,  0, a_idx);
			else if (choice <= 21) get_quality(FALSE, BRAND_POIS,  0, a_idx);
			else if (choice <= 23) get_quality(FALSE, IMPACT,      0, a_idx);
			else if (choice <= 27) get_quality(FALSE, VORPAL,      0, a_idx);
			else if (choice <= 30) get_quality(FALSE, LITE,        0, a_idx);

			/* Clean out the wallet. */
			if (potential < randint(500))
			{
				if (a_ptr->to_d > 0) a_ptr->to_d += potential / 150;
				if (a_ptr->to_h > 0) a_ptr->to_h += potential / 150;
				potential = 0;
			}
		}

		/* I'm any piece of armor */
		else if (is_any_armor(a_ptr))
		{
			/* Collect a resistance */
			grant_resist(a_idx);

			/* Often, collect a pval-dependant quality, if it is affordable. */
			if (one_in_(2))
			{
				grant_pval(a_idx);
			}

			/* Often, collect a miscellaneous quality, if it is affordable. */
			if (one_in_(2))
			{
				grant_attribute(a_idx);
			}

			/* Clean out the wallet. */
			if ((potential < 500) && (one_in_(3)))
			{
				if (a_ptr->ac > 0) a_ptr->to_a += potential / 100;
				potential = 0;
			}
		}

		/* Unrecognized object type */
		else
		{
			msg_print("Error in random artifact generation.  Object kind not recognized.");
			potential = 0;
		}
	}

	/* Frequently neaten bonuses to Armor Class, Skill, and Deadliness. */
	if (TRUE)
	{
		if      ((a_ptr->to_a % 5 == 4) && (one_in_(2)))  a_ptr->to_a++;
		else if ((a_ptr->to_a % 5 == 1) && (one_in_(2)))  a_ptr->to_a--;
		else if ((a_ptr->to_a % 2 == 1) && (!one_in_(4))) a_ptr->to_a++;

		if      ((a_ptr->to_h % 5 == 4) && (one_in_(2)))  a_ptr->to_h++;
		else if ((a_ptr->to_h % 5 == 1) && (one_in_(2)))  a_ptr->to_h--;
		else if ((a_ptr->to_h % 2 == 1) && (!one_in_(4))) a_ptr->to_h++;

		if      ((a_ptr->to_d % 5 == 4) && (one_in_(2)))  a_ptr->to_d++;
		else if ((a_ptr->to_d % 5 == 1) && (one_in_(2)))  a_ptr->to_d--;
		else if ((a_ptr->to_d % 2 == 1) && (!one_in_(4))) a_ptr->to_d++;
	}
}



/*
 * Invoke perilous magics, and curse the artifact beyond redemption!
 *
 * This code could use some more imagination...
 */
static void make_terrible(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];

	int i, gauntlet_runs, wheel_of_doom, penalty;

	bool heavy_curse = FALSE;
	bool aggravation = FALSE;


	/*
	 * Determine whether the artifact's magics are perilous enough to warrant
	 * a heavy curse or an aggravation.
	 */
	if (potential >= rand_range(2500, 3500)) heavy_curse = TRUE;
	if ((potential >= 3500) && (!one_in_(3))) aggravation = TRUE;


	/* Greatly decrease the chance for an activation. */
	if ((a_ptr->activate) && (!one_in_(3))) a_ptr->activate = 0;


	/* Force the artifact though the gauntlet two or three times. */
	gauntlet_runs = rand_range(2, 3);

	for (i = 0; i < gauntlet_runs; i++)
	{
		/* Choose a curse, biased towards penalties to_a, to_d, and to_h */
		if ((is_any_weapon(a_ptr)) && (a_ptr->to_h > 0) && (one_in_(3)))
		     wheel_of_doom = 1;
		if ((is_any_armor(a_ptr)) && (a_ptr->to_a > 0) && (one_in_(3)))
			wheel_of_doom = 2;
		else wheel_of_doom = randint(4);

		penalty = 0;

		/* Make either the Skill or Deadliness bonus negative, or both. */
		if (wheel_of_doom == 1)
		{
			/* Weapons. */
			if (is_any_weapon(a_ptr))
			{
				/* Blast Skill and (usually) Deadliness */
				a_ptr->to_h = -(rand_range(6, 12) * 2);
				if (!one_in_(3)) a_ptr->to_d = -(rand_range(6, 12) * 2);
			}

			/* All armors. */
			else
			{
				/* Reverse any magics. */
				if (a_ptr->to_h > 0) a_ptr->to_h = -(a_ptr->to_h);
				if (a_ptr->to_d > 0) a_ptr->to_d = -(a_ptr->to_d);

				/* Sometimes, blast even items without bonuses. */
				else if ((a_ptr->to_d == 0) && (a_ptr->to_h == 0) &&
				         (one_in_(5)))
				{
					penalty = rand_range(2, 5);
					penalty *= 5;
					a_ptr->to_h -= penalty;
					a_ptr->to_d -= penalty;
				}
			}

			/* Artifact might well have some remaining value. */
			if ((a_ptr->to_d < 0) && (a_ptr->to_h < 0))
			{
				a_ptr->cost -= 5000L;
			}
			else if ((a_ptr->to_d < 0) || (a_ptr->to_h < 0))
			{
				a_ptr->cost -= 2500L;
			}
		}

		/* Blast base armor class or inflict a penalty to armor class. */
		if (wheel_of_doom == 2)
		{
			/* Blast armor and twist magics. */
			if ((a_ptr->ac > 0) && (one_in_(6)))
			{
				a_ptr->cost -= a_ptr->ac * 500L;
				a_ptr->ac = 0;
			}
			if ((is_any_armor(a_ptr)) && (a_ptr->to_a >= 0))
			{
				a_ptr->to_a = -(rand_range(6, 12) * 2);
			}

			/* Chance of a truly nasty effect for weapons. */
			else if ((one_in_(3)) && (is_any_weapon(a_ptr)))
			{
				penalty = 5 * rand_range(3, 5);
				a_ptr->to_a = -penalty;
			}

			/* Artifact might very well still have some value. */
			if (a_ptr->to_a < 0) a_ptr->cost -= ABS(a_ptr->to_a) * 200L;
		}

		/* Blast pvals. */
		if (wheel_of_doom == 3)
		{
			int tmp = 1;
			if (one_in_(5)) tmp = 5;

			/* Reverse or wipe pvals */
			if (a_ptr->pval1 > 0)
			{
				if      (one_in_(4)) a_ptr->pval1 = 0;
				else if (one_in_(2)) a_ptr->pval1 = -(a_ptr->pval1 * tmp);
			}
			if (a_ptr->pval2 > 0)
			{
				if      (one_in_(4)) a_ptr->pval2 = 0;
				else if (one_in_(2)) a_ptr->pval2 = -(a_ptr->pval2 * tmp);
			}
			if (a_ptr->pval3 > 0)
			{
				if      (one_in_(4)) a_ptr->pval3 = 0;
				else if (one_in_(2)) a_ptr->pval3 = -(a_ptr->pval3 * tmp);
			}

			/* Artifact is unlikely to have any value. */
			if ((a_ptr->pval1 < 0) || (a_ptr->pval2 < 0) || (a_ptr->pval3 < 0))
				a_ptr->cost -= 20000L;

			/* Add some new (negative) attributes */
			if (a_ptr->pval1 < 0)
			{
				if (one_in_(5))
				{
					a_ptr->flags_pval1 |= (TR_PVAL_STR);
					a_ptr->flags_pval1 |= (TR_PVAL_DEX);
					a_ptr->flags_pval1 |= (TR_PVAL_CON);
				}
				if (one_in_(4))
				{
					a_ptr->flags_pval1 |= (TR_PVAL_WIS);
					a_ptr->flags_pval1 |= (TR_PVAL_INT);
				}
				else if (one_in_(6))
				{
					a_ptr->flags_pval1 |= (TR_PVAL_SPEED);
				}
				else
				{
					if (one_in_(4)) a_ptr->flags_pval1 |= (TR_PVAL_STR);
					if (one_in_(4)) a_ptr->flags_pval1 |= (TR_PVAL_DEX);
					if (one_in_(4)) a_ptr->flags_pval1 |= (TR_PVAL_CON);
					if (one_in_(4)) a_ptr->flags_pval1 |= (TR_PVAL_WIS);
					if (one_in_(4)) a_ptr->flags_pval1 |= (TR_PVAL_INT);
					if (one_in_(4)) a_ptr->flags_pval1 |= (TR_PVAL_CHR);
				}

				/* Artifact is highly unlikely to have any value. */
				a_ptr->cost -= 10000L;
			}
		}
	}


	/* Boundary control. */
	if (a_ptr->cost < 0) a_ptr->cost = 0;

	/* Apply curses, aggravation, and various nasty stuff. */
	a_ptr->flags3 |= (TR3_LIGHT_CURSE);
	if (heavy_curse)       a_ptr->flags3 |= (TR3_HEAVY_CURSE);


	/* Handle equipment item curses */
	if (is_wearable(a_ptr) && !is_missile(a_ptr))
	{
		if (aggravation)       a_ptr->flags3 |= (TR3_AGGRAVATE);
		if (one_in_(6))        a_ptr->flags3 |= (TR3_TELEPORT);

		if (is_melee_weapon(a_ptr))
		{
			if (one_in_(8))  a_ptr->flags3 |= (TR3_SOULSTEAL);
		}
		if (one_in_(12)) a_ptr->flags3 |= (TR3_NOMAGIC);
		if (one_in_(12)) a_ptr->flags3 |= (TR3_DRAIN_EXP);
		if (one_in_(30)) a_ptr->flags3 |= (TR3_DRAIN_HP);
	}
}


/*
 * Clean up the artifact by removing illogical combinations of powers.
 * Adopted from Greg Wooledge's random artifact creation code.
 */
static void remove_contradictory(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];

	if (a_ptr->flags3 & (TR3_AGGRAVATE))
	{
		a_ptr->flags_pval1 &= ~(TR_PVAL_STEALTH);
		a_ptr->flags_pval2 &= ~(TR_PVAL_STEALTH);
		a_ptr->flags_pval3 &= ~(TR_PVAL_STEALTH);
	}

	if (a_ptr->flags2 & (TR2_IM_ACID))   a_ptr->flags2 &= ~(TR2_RES_ACID);
	if (a_ptr->flags2 & (TR2_IM_ELEC))   a_ptr->flags2 &= ~(TR2_RES_ELEC);
	if (a_ptr->flags2 & (TR2_IM_FIRE))   a_ptr->flags2 &= ~(TR2_RES_FIRE);
	if (a_ptr->flags2 & (TR2_IM_COLD))   a_ptr->flags2 &= ~(TR2_RES_COLD);

	if (a_ptr->pval1 < 0)
	{
		if (a_ptr->flags_pval1 & (TR_PVAL_STR)) a_ptr->flags1 &= ~(TR1_SUST_STR);
		if (a_ptr->flags_pval1 & (TR_PVAL_INT)) a_ptr->flags1 &= ~(TR1_SUST_INT);
		if (a_ptr->flags_pval1 & (TR_PVAL_WIS)) a_ptr->flags1 &= ~(TR1_SUST_WIS);
		if (a_ptr->flags_pval1 & (TR_PVAL_DEX)) a_ptr->flags1 &= ~(TR1_SUST_DEX);
		if (a_ptr->flags_pval1 & (TR_PVAL_CON)) a_ptr->flags1 &= ~(TR1_SUST_CON);
		if (a_ptr->flags_pval1 & (TR_PVAL_CHR)) a_ptr->flags1 &= ~(TR1_SUST_CHR);
	}
	if (a_ptr->pval2 < 0)
	{
		if (a_ptr->flags_pval2 & (TR_PVAL_STR)) a_ptr->flags1 &= ~(TR1_SUST_STR);
		if (a_ptr->flags_pval2 & (TR_PVAL_INT)) a_ptr->flags1 &= ~(TR1_SUST_INT);
		if (a_ptr->flags_pval2 & (TR_PVAL_WIS)) a_ptr->flags1 &= ~(TR1_SUST_WIS);
		if (a_ptr->flags_pval2 & (TR_PVAL_DEX)) a_ptr->flags1 &= ~(TR1_SUST_DEX);
		if (a_ptr->flags_pval2 & (TR_PVAL_CON)) a_ptr->flags1 &= ~(TR1_SUST_CON);
		if (a_ptr->flags_pval2 & (TR_PVAL_CHR)) a_ptr->flags1 &= ~(TR1_SUST_CHR);
	}
	if (a_ptr->pval3 < 0)
	{
		if (a_ptr->flags_pval3 & (TR_PVAL_STR)) a_ptr->flags1 &= ~(TR1_SUST_STR);
		if (a_ptr->flags_pval3 & (TR_PVAL_INT)) a_ptr->flags1 &= ~(TR1_SUST_INT);
		if (a_ptr->flags_pval3 & (TR_PVAL_WIS)) a_ptr->flags1 &= ~(TR1_SUST_WIS);
		if (a_ptr->flags_pval3 & (TR_PVAL_DEX)) a_ptr->flags1 &= ~(TR1_SUST_DEX);
		if (a_ptr->flags_pval3 & (TR_PVAL_CON)) a_ptr->flags1 &= ~(TR1_SUST_CON);
		if (a_ptr->flags_pval3 & (TR_PVAL_CHR)) a_ptr->flags1 &= ~(TR1_SUST_CHR);
	}

	if (a_ptr->flags3 & (TR3_LIGHT_CURSE)) a_ptr->flags3 &= ~(TR3_BLESSED);
	if (a_ptr->flags3 & (TR3_DRAIN_EXP)) a_ptr->flags3 &= ~(TR3_HOLD_LIFE);
}



/*
 * String-handling function from Greg Wooledge's random artifact generator.
 */
static char *my_strdup(const char *s)
{
	char *t = (char*) malloc (strlen(s) + 1);
	if (t) strcpy(t, s);
	return (t);
}


/*
 * Use W. Sheldon Simms' random name generator.  This function builds
 * probability tables which are used later on for letter selection.  It
 * relies on the ASCII character set.  -GW-
 */
static void build_prob(void)
{
	int c_prev, c_cur, c_next;
	FILE *f;
	char buf [BUFLEN];

	/*
	 * Open the file containing our lexicon, and read from it.  Warn the
	 * rest of the code that random names are unavailable on failure.
	 */
	(void)path_build(buf, BUFLEN, ANGBAND_DIR_FILE, NAMES_FILE);
	if ((f = my_fopen(buf, "r")) == NULL)
	{
		find_all_names = TRUE;
		return;
	}

	/* Build raw frequencies */
	while (TRUE)
	{
		c_prev = c_cur = S_WORD;

		do
		{
			c_next = fgetc(f);
		} while (!isalpha(c_next) && (c_next != EOF));
		if (c_next == EOF) break;

		do
		{
			c_next = tolower(c_next) - 'a';	/* ASCII */
			lprobs[c_prev][c_cur][c_next]++;
			ltotal[c_prev][c_cur]++;
			c_prev = c_cur;
			c_cur = c_next;
			c_next = fgetc(f);
		} while (isalpha(c_next));

		lprobs [c_prev][c_cur][E_WORD]++;
		ltotal [c_prev][c_cur]++;
	}


	/* Close the file. */
	fclose(f);
}



/*
 * Use W. Sheldon Simms' random name generator.  Generate a random word
 * using the probability tables we built earlier.  Relies on the ASCII
 * character set.  Relies on European vowels (a, e, i, o, u).  The generated
 * name should be copied/used before calling this function again.  -GW-
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

		*cp = c_next + 'a';	/* ASCII */
		switch (*cp)
		{
			case 'a': case 'e': case 'i': case 'o': case 'u':
				vow++;
		}

		cp++;
		lnum++;
		c_prev = c_cur;
		c_cur = c_next;
	}

	word_buf[0] = my_toupper(word_buf[0]);
	return (word_buf);
}


/*
 * Find a name from any of various text files.
 */
static char *find_word(int a_idx)
{
	static char art_name[DESC_LEN];
	artifact_type *a_ptr = &a_info[a_idx];
	art_name[0] = '\0';

	/*
	 * Select a file, depending on whether the artifact is a weapon or
	 * armor, and whether or not it is cursed.  Get a random line from
	 * that file.
	 */
	if (a_ptr->flags3 & (TR3_LIGHT_CURSE))
	{
		if (is_any_weapon(a_ptr))
		{
			(void)get_rnd_line("w_cursed.txt", art_name);
		}
		else
		{
			(void)get_rnd_line("a_cursed.txt", art_name);
		}
	}
	else
	{
		if (is_any_weapon(a_ptr))
		{
			(void)get_rnd_line("w_normal.txt", art_name);
		}
		else
		{
			(void)get_rnd_line("a_normal.txt", art_name);
		}
	}


	/*
	 * Secretly call the make_word function in case of failure.  If it is
	 * unavailable, complain loudly.
	 */
	if (!art_name[0])
	{
		if (find_all_names)
		{
			msg_print("Cannot find any files with names or naming resources!");
			msg_print("Your 'files' folder lacks needed documents!");
		}

		/* Format the output of make_word. */
		else if (one_in_(3))
			strcpy(art_name, format("'%s'", make_word()));
		else
			strcpy(art_name, format("of %s", make_word()));

	}

	/* Return the name. */
	return (art_name);
}


/*
 * Name an artifact, using one of two methods.
 */
static void name_artifact(int a_idx)
{
	char *word;
	char buf[BUFLEN];


	/*
	 * Use W. Sheldon Simms' random name generator most of the time,
	 * generally for the less powerful artifacts, if not out of commission.
	 * Otherwise, find a name from a text file.
	 */
	if (((randint(max_potential) > initial_potential) || (one_in_(2))) &&
		(find_all_names == FALSE))
	{
		word = make_word();

		if (one_in_(3))
			(void)strnfmt(buf, sizeof(buf), "'%s'", word);
		else
			(void)strnfmt(buf, sizeof(buf), "of %s", word);
	}
	else
	{
		word = find_word(a_idx);

		(void)strnfmt(buf, sizeof(buf), "%s", word);
	}


	/* Insert whatever name is created or found into the temporary array. */
	names[a_idx] = my_strdup(buf);

}


/*
 * Design a random artifact.
 */
static void design_random_artifact(int a_idx)
{
	/* Initialize the artifact, and determine maximum potential. */
	initialize_artifact(a_idx, 0, 0);

	/* Get potential, rarity, and native depth. */
	get_potential(a_idx);

	/* Assign a theme to the artifact. */
	choose_basic_theme(a_idx);

	/* Add extra qualities until done. */
	haggle_till_done(a_idx);

	/* Decide if the artifact is to be terrible. */
	if (TERRIBLE_CHANCE > rand_int(100))
	{
		/* If it is, add a few benefits and more drawbacks. */
		make_terrible(a_idx);
	}

	/* Remove contradictory powers. */
	remove_contradictory(a_idx);

	/* Find or make a name for the artifact, and place into a temporary array. */
	name_artifact(a_idx);
}


/*
 * Initialize a temporary artifact (used by the forging code).
 */
int init_temporary_artifact(int a_idx, int tval, int sval)
{
	/* Initialize the artifact, and determine maximum potential. */
	initialize_artifact(a_idx, tval, sval);

	/* Return */
	return (max_potential);
}

/*
 * Design a temporary artifact (used by the forging code).
 */
void design_temporary_artifact(int a_idx, int v, bool corrupted)
{
	artifact_type *a_ptr = &a_info[a_idx];

	/* Save the current potential */
	potential = v;

	/* Save initial dice and dice sides */
	save_dd = a_ptr->dd;
	save_ds = a_ptr->ds;

	/* Assign a theme to the artifact, if it has some power. */
	if ((!is_missile(a_ptr)) && (potential >= rand_range(1300, 1500)))
	{
		choose_basic_theme(a_idx);
	}

	/* Wimpy little forged thing, or any missile */
	else
	{
		/* Add some plusses */
		if (is_any_armor(a_ptr))
		{
			a_ptr->to_a = 2 + rand_range(potential / 300, potential / 150);
		}
		else
		{
			a_ptr->to_h = 1 + rand_range(potential / 400, potential / 200);
			a_ptr->to_d = 1 + rand_range(potential / 400, potential / 200);
		}

		/* Use up half the potential */
		potential /= 2;
	}

	/* Add extra qualities until done. */
	haggle_till_done(a_idx);

	/* Corrupt the artifact */
	if (corrupted) make_terrible(a_idx);

	/* Remove contradictory powers. */
	remove_contradictory(a_idx);
}





/*
 * Fill in the temporary array of artifact names, and then convert it into
 * an a_name structure.  From Greg Wooledge's random artifacts.
 */
static int convert_names(void)
{
	size_t name_size;
	char *a_base;
	char *a_next;

	int i;

	/* Add the permanent artifact names to the temporary array. */
	for (i = 0; i < ART_MIN_RANDOM; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		names[i] = a_name + a_ptr->name;
	}

	/* Convert our names array into an a_name structure for later use. */
	name_size = 0;
	for (i = 0; i < z_info->a_max; i++)
	{
		name_size += strlen (names[i]) + 2;	/* skip first char */
	}

	C_MAKE(a_base, name_size, char);

	a_next = a_base + 1;	/* skip first char */

	for (i = 0; i < z_info->a_max; i++)
	{
		strcpy(a_next, names[i]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen(names[i]) + 1;
	}


	/* Free the old names */
	FREE(a_name);

	for (i = ART_MIN_RANDOM; i < z_info->a_max; i++)
	{
		(void)string_free(names[i]);
	}

	a_name = a_base;
	a_head.name_ptr = a_base;
	a_head.name_size = name_size;

	return (0);
}


/*
 * Initialize all the random artifacts in the artifact array.  This function
 * is only called when a player is born.  Because various sub-functions use
 * player information, it must be called after the player has been generated
 * and player information has been loaded.
 */
void initialize_random_artifacts(void)
{
	int err;

	/* Index of the artifact currently being initialized. */
	int a_idx;


	/* Initialize the W. Seldon Simms random name generator. */
	build_prob();


	/*
	 * Go from beginning to end of the random section of the
	 * artifact array, initializing and naming as we go.
	 */
	for (a_idx = ART_MIN_RANDOM; a_idx < z_info->a_max; a_idx++)
	{
		/* Design the artifact, storing information as we go along. */
		design_random_artifact(a_idx);
	}


	/*
	 * The new names we created while artifacts were being rolled up need to
	 * be added to the a_name structure.
	 */
	err = convert_names();

	/* Complain if naming fails. */
	if (err) msg_print("Warning - random artifact naming failed!");
}
