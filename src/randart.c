/* File: randart.c */

/* Random artifacts.  Selling and providing qualities.  Choosing a object
 * type and kind, determining the potential, depth and rarity of the
 * artifact.  Selecting a theme, and the properties of all possible themes.
 * Adding semi-random qualities until potential is all used up.  Cursing
 * an artifact.  Removing contradictory flags.  Naming an artifact, using
 * either of two methods.  Initializing one random artifact.  Adding new
 * names to the a_name array.  Initializing all random artifacts.
 *
 * Copyright (c) 1998 Leon Marrick
 *
 * I owe thanks to Greg Wooledge for his support and string-handling code
 * and to W. Sheldon Simms for his Tolkienesque random name generator.
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


/* A global variable whose contents will be bartered to acquire powers. */
static int potential = 0;

/* The initial potential of the artifact. */
static int initial_potential = 0;

/* The maximum potential that that artifact could have possessed. */
static int max_potential = 0;

/* Two global variables to hold pval-dependant qualities. */
static byte add_pval_later1 = 0;
static byte add_pval_later2 = 0;

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
/* Hack - can handle up to 500 artifacts.  If z-info can not push
 * higher.
 */
static char *names[500];

/* Definitions of most artifact flags. */

#define ADD_STR			1
#define ADD_INT			2
#define ADD_WIS			3
#define ADD_DEX			4
#define ADD_CON			5
#define ADD_CHR			6

#define MAGIC_MASTERY		9
#define STEALTH			10
#define SEARCH			11
#define INFRA			12
#define TUNNEL			13
#define SPEED			14
#define MIGHT2			15
#define SHOTS			16
#define MIGHT1			17
#define SLAY_ANIMAL		20
#define SLAY_EVIL			21
#define SLAY_UNDEAD		22
#define SLAY_DEMON		23
#define SLAY_ORC			24
#define SLAY_TROLL		25
#define SLAY_GIANT		26
#define SLAY_DRAGON		27
#define PERFECT_BALANCE		28
#define BRAND_POIS		29
#define BRAND_ACID		30
#define BRAND_ELEC		31
#define BRAND_FIRE		32
#define BRAND_COLD		33

#define SUST_STR			40
#define SUST_INT			41
#define SUST_WIS			42
#define SUST_DEX			43
#define SUST_CON			44
#define SUST_CHR			45
#define IM_ACID			48
#define IM_ELEC			49
#define IM_FIRE			50
#define IM_COLD			51
#define RES_ACID			52
#define RES_ELEC			53
#define RES_FIRE			54
#define RES_COLD			55
#define RES_POIS			56
#define RES_FEAR			57
#define RES_LITE			58
#define RES_DARK			59
#define RES_BLIND			60
#define RES_CONFU			61
#define RES_SOUND			62
#define RES_SHARD			63
#define RES_NEXUS			64
#define RES_NETHR			65
#define RES_CHAOS			66
#define RES_DISEN			67

#define SLOW_DIGEST		70
#define FEATHER	 		71
#define LITE			72
#define REGEN			73
#define TELEPATHY			74
#define SEE_INVIS			75
#define FREE_ACT			76
#define HOLD_LIFE			77
#define IMPACT			78
#define BLESSED			80

#define ADD_AC			94
#define IMPROVE_BASE_AC		95
#define ENHANCE_DICE		96
#define ADD_SKILL			97
#define ADD_DEADLINESS		98

/* Random artifact activations are defined in defines.h. */




/*
 * Debit an artifact's account.
 */
static bool take_money(bool on_credit, int cost)
{
	/* Take the money. */
	if (potential > cost - 1)
	{
		potential -= cost;
		return(TRUE);
	}

	/* <<mutter>> OK, I'll charge it to your account... */
	else if (on_credit)
	{
		potential = 0;
		return(TRUE);
	}

	/* Otherwise, kick da bum out. */
	else return(FALSE);
}



/*
 * Grant the quality asked for, if the artifact can afford it.
 */
static bool get_quality(bool on_credit, int purchase, int pval, int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];
	int temp, i;


	switch(purchase)
	{
		case ADD_STR:
		{
			if (take_money(on_credit, (150 * pval) + (pval * pval * 8)))
			{
				a_ptr->flags1 |= TR1_STR;
				return(TRUE);
			}
			break;
		}
		case ADD_WIS:
		{
			if (mp_ptr->spell_stat == A_WIS)
			{
				if (take_money(on_credit, (150 * pval) + (pval * pval * 8)))
				{
					a_ptr->flags1 |= TR1_WIS;
					return(TRUE);
				}
			}
			else
			{
				if (take_money(on_credit, (100 * pval) + (pval * pval * 5)))
				{
					a_ptr->flags1 |= TR1_WIS;
					return(TRUE);
				}
			}
			break;
		}
		case ADD_INT:
		{
			if (mp_ptr->spell_stat == A_INT)
			{
				if (take_money(on_credit, (150 * pval) + (pval * pval * 8)))
				{
					a_ptr->flags1 |= TR1_INT;
					return(TRUE);
				}
			}
			else
			{
				if (take_money(on_credit, (100 * pval) + (pval * pval * 5)))
				{
					a_ptr->flags1 |= TR1_INT;
					return(TRUE);
				}
			}
			break;
		}
		case ADD_DEX:
		{
			if (take_money(on_credit, (130 * pval) + (pval * pval * 7)))
			{
				a_ptr->flags1 |= TR1_DEX;
				return(TRUE);
			}
			break;
		}
		case ADD_CON:
		{
			if (take_money(on_credit, (130 * pval) + (pval * pval * 7)))
			{
				a_ptr->flags1 |= TR1_CON;
				return(TRUE);
			}
			break;
		}
		case ADD_CHR:
		{
			if (take_money(on_credit, (40 * pval) + (pval * pval * 2)))
			{
				a_ptr->flags1 |= TR1_CHR;
				return(TRUE);
			}
			break;
		}
		case MAGIC_MASTERY:
		{
			if (take_money(on_credit, (20 * pval) + (pval * pval * 1)))
			{
				a_ptr->flags1 |= TR1_MAGIC_MASTERY;
				return(TRUE);
			}
			break;
		}
		case STEALTH:
		{
			if (take_money(on_credit, (100 * pval) + (pval * pval * 5)))
			{
				a_ptr->flags1 |= TR1_STEALTH;
				return(TRUE);
			}
			break;
		}
		case SEARCH:
		{
			if (take_money(on_credit, (20 * pval) + (pval * pval * 1)))
			{
				a_ptr->flags1 |= TR1_SEARCH;
				return(TRUE);
			}
			break;
		}
		case INFRA:
		{
			if (take_money(on_credit, (15 * pval) + (pval * pval * 1)))
			{
				a_ptr->flags1 |= TR1_INFRA;
				return(TRUE);
			}
			break;
		}
		case TUNNEL:
		{
			if (take_money(on_credit, (25 * pval) + (pval * pval * 1)))
			{
				a_ptr->flags1 |= TR1_TUNNEL;
				return(TRUE);
			}
			break;
		}
		case SPEED:
		{
			if (take_money(on_credit, (360 * pval) + (pval * pval * 18)))
			{
				a_ptr->flags1 |= TR1_SPEED;
				return(TRUE);
			}
			break;
		}
		case MIGHT2:
		{
			if (take_money(on_credit, 2500))
			{
				a_ptr->flags1 |= TR1_MIGHT2;
				return(TRUE);
			}
			break;
		}
		case SHOTS:
		{
			if (take_money(on_credit, 1500))
			{
				a_ptr->flags1 |= TR1_SHOTS;
				return(TRUE);
			}
			break;
		}
		case MIGHT1:
		{
			if (take_money(on_credit, 1250))
			{
				a_ptr->flags1 |= TR1_MIGHT1;
				return(TRUE);
			}
			break;
		}
		case SLAY_ANIMAL:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_SLAY_ANIMAL;
				return(TRUE);
			}
			break;
		}
		case SLAY_EVIL:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_SLAY_EVIL;
				return(TRUE);
			}
			break;
		}
		case SLAY_UNDEAD:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_SLAY_UNDEAD;
				return(TRUE);
			}
			break;
		}
		case SLAY_DEMON:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags1 |= TR1_SLAY_DEMON;
				return(TRUE);
			}
			break;
		}
		case SLAY_ORC:
		{
			if (take_money(on_credit, 400))
			{
				a_ptr->flags1 |= TR1_SLAY_ORC;
				return(TRUE);
			}
			break;
		}
		case SLAY_TROLL:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags1 |= TR1_SLAY_TROLL;
				return(TRUE);
			}
			break;
		}
		case SLAY_GIANT:
		{
			if (take_money(on_credit, 400))
			{
				a_ptr->flags1 |= TR1_SLAY_GIANT;
				return(TRUE);
			}
			break;
		}
		case SLAY_DRAGON:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_SLAY_DRAGON;
				return(TRUE);
			}
			break;
		}
		case PERFECT_BALANCE:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags1 |= TR1_PERFECT_BALANCE;
				return(TRUE);
			}
			break;
		}
		case BRAND_POIS:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_BRAND_POIS;
				return(TRUE);
			}
			break;
		}
		case BRAND_ACID:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_BRAND_ACID;
				return(TRUE);
			}
			break;
		}
		case BRAND_ELEC:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_BRAND_ELEC;
				return(TRUE);
			}
			break;
		}
		case BRAND_FIRE:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_BRAND_FIRE;
				return(TRUE);
			}
			break;
		}
		case BRAND_COLD:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags1 |= TR1_BRAND_COLD;
				return(TRUE);
			}
			break;
		}
		case SUST_STR:
		{
			if (take_money(on_credit, 550))
			{
				a_ptr->flags2 |= TR2_SUST_STR;
				return(TRUE);
			}
			break;
		}
		case SUST_WIS:
		{
			if (take_money(on_credit, 350))
			{
				a_ptr->flags2 |= TR2_SUST_WIS;
				return(TRUE);
			}
			break;
		}
		case SUST_INT:
		{
			if (take_money(on_credit, 350))
			{
				a_ptr->flags2 |= TR2_SUST_INT;
				return(TRUE);
			}
			break;
		}
		case SUST_DEX:
		{
			if (take_money(on_credit, 350))
			{
				a_ptr->flags2 |= TR2_SUST_DEX;
				return(TRUE);
			}
			break;
		}
		case SUST_CON:
		{
			if (take_money(on_credit, 350))
			{
				a_ptr->flags2 |= TR2_SUST_CON;
				return(TRUE);
			}
			break;
		}
		case SUST_CHR:
		{
			if (take_money(on_credit, 50))
			{
				a_ptr->flags2 |= TR2_SUST_CHR;
				return(TRUE);
			}
			break;
		}
		case IM_ACID:
		{
			if (take_money(on_credit, 3500))
			{
				a_ptr->flags2 |= TR2_IM_ACID;
				return(TRUE);
			}
			break;
		}
		case IM_ELEC:
		{
			if (take_money(on_credit, 2750))
			{
				a_ptr->flags2 |= TR2_IM_ELEC;
				return(TRUE);
			}
			break;
		}
		case IM_FIRE:
		{
			if (take_money(on_credit, 3250))
			{
				a_ptr->flags2 |= TR2_IM_FIRE;
				return(TRUE);
			}
			break;
		}
		case IM_COLD:
		{
			if (take_money(on_credit, 2750))
			{
				a_ptr->flags2 |= TR2_IM_COLD;
				return(TRUE);
			}
			break;
		}
		case RES_ACID:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_ACID;
				return(TRUE);
			}
			break;
		}
		case RES_ELEC:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_ELEC;
				return(TRUE);
			}
			break;
		}
		case RES_FIRE:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_FIRE;
				return(TRUE);
			}
			break;
		}
		case RES_COLD:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_COLD;
				return(TRUE);
			}
			break;
		}
		case RES_POIS:
		{
			if (take_money(on_credit, 1000))
			{
				a_ptr->flags2 |= TR2_RES_POIS;
				return(TRUE);
			}
			break;
		}
		case RES_FEAR:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_FEAR;
				return(TRUE);
			}
			break;
		}
		case RES_LITE:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags2 |= TR2_RES_LITE;
				return(TRUE);
			}
			break;
		}
		case RES_DARK:
		{
			if (take_money(on_credit, 600))
			{
				a_ptr->flags2 |= TR2_RES_DARK;
				return(TRUE);
			}
			break;
		}
		case RES_BLIND:
		{
			if (take_money(on_credit, 650))
			{
				a_ptr->flags2 |= TR2_RES_BLIND;
				return(TRUE);
			}
			break;
		}
		case RES_CONFU:
		{
			if (take_money(on_credit, 750))
			{
				a_ptr->flags2 |= TR2_RES_CONFU;
				return(TRUE);
			}
			break;
		}
		case RES_SOUND:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_SOUND;
				return(TRUE);
			}
			break;
		}
		case RES_SHARD:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_SHARD;
				return(TRUE);
			}
			break;
		}
		case RES_NEXUS:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags2 |= TR2_RES_NEXUS;
				return(TRUE);
			}
			break;
		}
		case RES_NETHR:
		{
			if (take_money(on_credit, 800))
			{
				a_ptr->flags2 |= TR2_RES_NETHR;
				return(TRUE);
			}
			break;
		}
		case RES_CHAOS:
		{
			if (take_money(on_credit, 800))
			{
				a_ptr->flags2 |= TR2_RES_CHAOS;
				return(TRUE);
			}
			break;
		}
		case RES_DISEN:
		{
			if (take_money(on_credit, 1000))
			{
				a_ptr->flags2 |= TR2_RES_DISEN;
				return(TRUE);
			}
			break;
		}
		case SLOW_DIGEST:
		{
			if (take_money(on_credit, 300))
			{
				a_ptr->flags3 |= TR3_SLOW_DIGEST;
				return(TRUE);
			}
			break;
		}
		case FEATHER:
		{
			if (take_money(on_credit, 300))
			{
				a_ptr->flags3 |= TR3_FEATHER;
				return(TRUE);
			}
			break;
		}
		case LITE:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags3 |= TR3_LITE;
				return(TRUE);
			}
			break;
		}
		case REGEN:
		{
			if (take_money(on_credit, 500))
			{
				a_ptr->flags3 |= TR3_REGEN;
				return(TRUE);
			}
			break;
		}
		case TELEPATHY:
		{
			if (take_money(on_credit, 2500))
			{
				a_ptr->flags3 |= TR3_TELEPATHY;
				return(TRUE);
			}
			break;
		}
		case SEE_INVIS:
		{
			if (take_money(on_credit, 700))
			{
				a_ptr->flags3 |= TR3_SEE_INVIS;
				return(TRUE);
			}
			break;
		}
		case FREE_ACT:
		{
			if (take_money(on_credit, 700))
			{
				a_ptr->flags3 |= TR3_FREE_ACT;
				return(TRUE);
			}
			break;
		}
		case HOLD_LIFE:
		{
			if (take_money(on_credit, 1400))
			{
				a_ptr->flags3 |= TR3_HOLD_LIFE;
				return(TRUE);
			}
			break;
		}
		case IMPACT:
		{
			if (take_money(on_credit, 0))
			{
				a_ptr->flags3 |= TR3_IMPACT;
				return(TRUE);
			}
			break;
		}
		case BLESSED:
		{
			if ((check_ability(SP_BLESS_WEAPON))
			    & ((a_ptr->tval == TV_POLEARM) || (a_ptr->tval == TV_SWORD)))
			{
				if (take_money(on_credit, 400))
				{
					a_ptr->flags3 |= TR3_BLESSED;
					return(TRUE);
				}
			}
			else
			{
				if (take_money(on_credit, 0))
				{
					a_ptr->flags3 |= TR3_BLESSED;
					return(TRUE);
				}
			}
			break;
		}
		case ADD_AC:
		{
			if (take_money(on_credit, pval * 40))
			{
				a_ptr->to_a += pval;
				return(TRUE);
			}
			break;
		}
		case IMPROVE_BASE_AC:
		{
			if (take_money(on_credit, pval * 40))
			{
				a_ptr->ac += pval;
				return(TRUE);
			}
			break;
		}
		case ENHANCE_DICE:
		{
			/* Allow a portion of the budget for improvements.
			 * Use "pval" to control fraction.
			 */
			temp = potential / pval;
			if (temp > 1000) temp = 1000;

			/* If credit is extended, guarantee something to work with. */
			if (on_credit)
			{
				if (temp < 500) temp = 500;

				if (potential > temp) potential -= temp;
				else potential = 0;
			}
			/* Otherwise, subtract whatever portion is alloted from the
			 * main budget. */
			else potential -= temp;

			/* Enhance the damage dice, depending on potential. */
			for (i = 0; i < 5; i++)
			{
				if ((randint(2) == 1) && (temp >= 100 * a_ptr->ds))
				{
					a_ptr->dd++;
					temp -= 100 * a_ptr->ds;
				}
				else if (temp >= 200 * a_ptr->dd)
				{
					a_ptr->ds = a_ptr->ds + 2;
					temp -= 200 * a_ptr->dd;
				}

				/* Either stop the loop or increment the dice sides. */
				else
				{
					if (temp < 200) break;

					a_ptr->ds++;
					temp -= 200;
				}
			}

			/* Pour any remainder back into the main budget. */
			potential += temp;

			break;
		}
		case ADD_SKILL:
		{
			if (take_money(on_credit, pval * 40))
			{
				a_ptr->to_h += pval;
				return(TRUE);
			}
			break;
		}
		case ADD_DEADLINESS:
		{
			if (take_money(on_credit, pval * 40))
			{
				a_ptr->to_d += pval;
				return(TRUE);
			}
			break;
		}
	}

	/* No stock with that description on the shelf, or price too high. */
	return(FALSE);
}



/*
 * Assign a tval and sval, grant a certain amount of potential to be used
 * for acquiring powers, and determine rarity and native depth.
 */
static void initialize_artifact(int a_idx)
{
	int i;
	int index, freq, rarity;
	int base_object_depth, base_object_rarity;
	int artifact_depth, artifact_rarity;

	int power_of_base_object = 0;
	int base_object_activation = 0;


	object_kind *k_ptr;
	artifact_type *a_ptr = &a_info[a_idx];


	/* Wipe the artifact completely. */
	a_ptr->tval = 0;
	a_ptr->sval = 0;
	a_ptr->pval = 0;

	a_ptr->to_h = 0;
	a_ptr->to_d = 0;
	a_ptr->to_a = 0;

	a_ptr->ac = 0;
	a_ptr->dd = 0;
	a_ptr->ds = 0;

	a_ptr->weight = 0;
	a_ptr->cost = 0;

	a_ptr->flags1 = 0;
	a_ptr->flags2 = 0;
	a_ptr->flags3 = 0;

	a_ptr->level = 0;
	a_ptr->rarity = 0;
	a_ptr->activation = 0;


	/* Assign a tval and sval.  If the base object is powerful, an amount to
	 * reduce artifact power by will be calculated.  If the base object has
	 * an activation, it will be preserved (at least initially).
	 */
	while(TRUE)
	{
		/* Acquire an object at random */
		index = rand_int(z_info->k_max);
		k_ptr = &k_info[index];

		/* Skip "empty" objects */
		if (!k_ptr->name) continue;

		/* Skip objects that are not weapons or armour. */
		if ((k_ptr->tval != TV_BOW) && (k_ptr->tval != TV_HAFTED) &&
			(k_ptr->tval != TV_POLEARM) && (k_ptr->tval != TV_SWORD) &&
			(k_ptr->tval != TV_BOOTS) && (k_ptr->tval != TV_GLOVES) &&
			(k_ptr->tval != TV_HELM) && (k_ptr->tval != TV_CROWN)&&
			(k_ptr->tval != TV_SHIELD) && (k_ptr->tval != TV_CLOAK) &&
			(k_ptr->tval != TV_SOFT_ARMOR) && (k_ptr->tval != TV_HARD_ARMOR)
			&& (k_ptr->tval != TV_DRAG_ARMOR))
		{
			continue;
		}


		/*** Determine rarity.  Method:  adding fractions ***/
		freq = 0;

		/* Scan all four allocation chance values */
		for (i = 0; i < 4; i++)
		{
			/* Skip empty values. */
			if (k_ptr->chance[i] == 0) continue;

			/* Sum relative chances of object being made. */
			freq += (1000 / k_ptr->chance[i]);
		}
		if (!freq) continue;
		rarity = 1000 / freq;

		/* Accept object if it passes the rarity roll. */
		if (rand_int(rarity) == 0) break;
	}


	/* Determine "power" and get activation of base object. */
	switch (k_ptr->tval)
	{
		case TV_BOW:
		{
			if (k_ptr->sval == SV_HEAVY_XBOW) power_of_base_object = 750;
			break;
		}

		case TV_HAFTED:
		{
			if (k_ptr->sval == SV_THROWING_HAMMER)
				power_of_base_object = 1000;
			if (k_ptr->sval == SV_MACE_OF_DISRUPTION)
				power_of_base_object = 500;
			break;
		}

		case TV_POLEARM:
		{
			if (k_ptr->sval == SV_THROWING_AXE)
				power_of_base_object = 1000;
			if (k_ptr->sval == SV_SCYTHE_OF_SLICING)
				power_of_base_object = 750;
			break;
		}

		case TV_SWORD:
		{
			if (k_ptr->sval == SV_EXECUTIONERS_SWORD)
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
				power_of_base_object = 700;
			if (k_ptr->sval == SV_ADAMANTITE_PLATE_MAIL)
				power_of_base_object = 500;
			break;
		}

		case TV_DRAG_ARMOR:
		{
			if (k_ptr->sval == SV_DRAGON_BLACK)
			{
				power_of_base_object = 1250;
				base_object_activation = ACT_DRAGON_BLACK;
			}
			if (k_ptr->sval == SV_DRAGON_BLUE)
			{
				power_of_base_object = 1250;
				base_object_activation = ACT_DRAGON_BLUE;
			}
			if (k_ptr->sval == SV_DRAGON_WHITE)
			{
				power_of_base_object = 1250;
				base_object_activation = ACT_DRAGON_WHITE;
			}
			if (k_ptr->sval == SV_DRAGON_RED)
			{
				power_of_base_object = 1350;
				base_object_activation = ACT_DRAGON_RED;
			}
			if (k_ptr->sval == SV_DRAGON_GREEN)
			{
				power_of_base_object = 1500;
				base_object_activation = ACT_DRAGON_GREEN;
			}
			if (k_ptr->sval == SV_DRAGON_MULTIHUED)
			{
				power_of_base_object = 2000;
				base_object_activation = ACT_DRAGON_MULTIHUED;
			}
			if (k_ptr->sval == SV_DRAGON_SHINING)
			{
				power_of_base_object = 1500;
				base_object_activation = ACT_DRAGON_SHINING;
			}
			if (k_ptr->sval == SV_DRAGON_LAW)
			{
				power_of_base_object = 1750;
				base_object_activation = ACT_DRAGON_LAW;
			}
			if (k_ptr->sval == SV_DRAGON_BRONZE)
			{
				power_of_base_object = 1250;
				base_object_activation = ACT_DRAGON_BRONZE;
			}
			if (k_ptr->sval == SV_DRAGON_GOLD)
			{
				power_of_base_object = 1250;
				base_object_activation = ACT_DRAGON_GOLD;
			}
			if (k_ptr->sval == SV_DRAGON_CHAOS)
			{
				power_of_base_object = 1750;
				base_object_activation = ACT_DRAGON_CHAOS;
			}
			if (k_ptr->sval == SV_DRAGON_BALANCE)
			{
				power_of_base_object = 2250;
				base_object_activation = ACT_DRAGON_BALANCE;
			}
			if (k_ptr->sval == SV_DRAGON_POWER)
			{
				power_of_base_object = 3000;
				base_object_activation = ACT_DRAGON_POWER;
			}
			break;
		}
	}


	/* Store the base values of a bunch of data.  To avoid unbalancing the
	 * game, bonuses to Skill, Deadliness, and Armour Class are cut in half.
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
	if (k_ptr->cost > 4999) a_ptr->cost = k_ptr->cost;
	a_ptr->weight = k_ptr->weight;
	a_ptr->flags1 = k_ptr->flags1;
	a_ptr->flags2 = k_ptr->flags2;
	a_ptr->flags3 = k_ptr->flags3;
	a_ptr->flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
			   TR3_IGNORE_FIRE | TR3_IGNORE_COLD);
	a_ptr->activation = base_object_activation;

	/* The total potential of an artifact ranges from 1750 to 7750,
	 * biased towards the lesser figure. */
	potential = 1750;
	for (i = 0; i < 12; i++)
	{
		if (randint(5) != 1) potential += 500;
		else break;
	}

	/* 7750 is normally the maximum potential for any artifact. */
	max_potential = 7750;


	/* Preserve balance by not allowing powerful base objects to get very
	 * many extra powers.
	 */
	potential -= power_of_base_object;

	/* Many equipment types include no truly powerful artifacts.  Maximum
	 * and actual potential is limited.
	 */
	if (a_ptr->tval == TV_CLOAK)
	{
		if (potential > 4000) potential = 4000;
		max_potential = 4000;
	}
	if (a_ptr->tval == TV_GLOVES)
	{
		if (potential > 4000) potential = 4000;
		max_potential = 4000;
	}
	if (a_ptr->tval == TV_BOOTS)
	{
		if (potential > 6500) potential = 6500;
		max_potential = 6500;
	}
	if (a_ptr->tval == TV_CROWN)
	{
		if (potential > 5000) potential = 5000;
		max_potential = 5000;
	}
	if (a_ptr->tval == TV_HELM)
	{
		if (potential > 4500) potential = 4500;
		max_potential = 4500;
	}
	if (a_ptr->tval == TV_SHIELD)
	{
		if (potential > 5000) potential = 5000;
		max_potential = 5000;
	}
	if (a_ptr->tval == TV_BOW)
	{
		if (potential > 5000) potential = 5000;
		max_potential = 5000;
	}

	/* Regardless of other considerations, grant at least some potential. */
	if (potential < 1500) potential = 1500;

	/* Remember how much potential was allowed. */
	initial_potential = potential;

	/* The cost of the artifact depends solely on its potential (allowing
	 * for valuable base object kinds).
	 */
	a_ptr->cost += (potential * 10L);


	/* Determine the base object depth and rarity (use all indexes). */
	base_object_depth = k_ptr->locale[0];
	base_object_rarity = k_ptr->chance[0];

	/* Paranoia. */
	if (base_object_rarity == 0) base_object_rarity = 1;


	/* Calculate artifact depth.  This is fairly simple. */
	artifact_depth = (potential / 100) - 5;
	if (artifact_depth < base_object_depth - 10)
		artifact_depth = base_object_depth - 10;
	if (artifact_depth < 5) artifact_depth = 5;


	/* Calculate artifact rarity.  This requires handling some special cases. */
	artifact_rarity = (((potential + power_of_base_object) / 200)
		 / base_object_rarity);

	/* Modify artifact rarity to handle some special cases. */
	if (a_ptr->tval == TV_SHIELD) artifact_rarity /= 2;
	if ((a_ptr->tval == TV_SOFT_ARMOR) || (a_ptr->tval == TV_HARD_ARMOR))
		artifact_rarity /= 2;
	if (a_ptr->tval == TV_CLOAK) artifact_rarity *= 2;
	if ((a_ptr->tval == TV_HELM) || (a_ptr->tval == TV_CROWN))
		artifact_rarity = 2 * artifact_rarity / 3;
	if (a_ptr->tval == TV_BOOTS) artifact_rarity += 5;

	/* Boundary control. */
	if (artifact_rarity < 3) artifact_rarity = 3;

	/* Assign the values just calculated to the artifact. */
	a_ptr->level = artifact_depth;
	a_ptr->rarity = artifact_rarity;
}

/*
 * Pick an initial set of qualities, based on a theme.  Also add a bonus to
 * armour class, Skill, and Deadliness.
 */
static void choose_basic_theme(int a_idx)
{
	int i, selection, temp, old_weight;
	artifact_type *a_ptr = &a_info[a_idx];
	object_kind *k_ptr = &k_info[lookup_kind(a_ptr->tval, a_ptr->sval)];


	/* Possibly make very powerful artifacts aggravate. */
	if ((potential > 5000) && (potential > randint(15000)))
		a_ptr->flags3 |= TR3_AGGRAVATE;


	/* Frequently (but not always) assign a basic theme to the artifact. */
	selection = rand_int(100);

	switch(k_ptr->tval)
	{
		/* I'm a melee weapon... */
		case TV_SWORD: case TV_POLEARM: case TV_HAFTED:
		{
			/* ...with bonuses to Deadliness and Skill, and... */
			a_ptr->to_d += (3 + randint(7) + potential / 650);
			a_ptr->to_h += (3 + randint(7) + potential / 650);


			/* ...of fire. */
			if (selection < 6)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 6000)
						a_ptr->activation = ACT_RANDOM_FIRE3;
					else if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_FIRE2;
					else
						a_ptr->activation = ACT_RANDOM_FIRE1;
				}
				/* Brand the weapon with fire. */
				get_quality(TRUE, BRAND_FIRE, 0, a_idx);

				/* Grant either a resist or immunity to fire. */
				if ((potential >= 4500) && (randint(3) == 1))
					get_quality(FALSE, IM_FIRE, 0, a_idx);
				else get_quality(TRUE, RES_FIRE, 0, a_idx);
			}
			/* ...of frost. */
			else if (selection < 12)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 6000)
						a_ptr->activation = ACT_RANDOM_COLD3;
					else if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_COLD2;
					else
						a_ptr->activation = ACT_RANDOM_COLD1;
				}
				/* Brand the weapon with frost. */
				get_quality(TRUE, BRAND_COLD, 0, a_idx);

				/* Grant either a resist or immunity to frost. */
				if ((potential >= 4500) && (randint(3) == 1))
					get_quality(FALSE, IM_COLD, 0, a_idx);
				else get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...of acid. */
			else if (selection < 18)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 6000)
						a_ptr->activation = ACT_RANDOM_ACID3;
					else if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_ACID2;
					else
						a_ptr->activation = ACT_RANDOM_ACID1;
				}
				/* Brand the weapon with acid. */
				get_quality(TRUE, BRAND_ACID, 0, a_idx);

				/* Grant either a resist or immunity to acid. */
				if ((potential >= 4500) && (randint(3) == 1))
					get_quality(FALSE, IM_ACID, 0, a_idx);
				else get_quality(TRUE, RES_ACID, 0, a_idx);
			}
			/* ...of electricity. */
			else if (selection < 24)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 6000)
						a_ptr->activation = ACT_RANDOM_ELEC3;
					else if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_ELEC2;
					else
						a_ptr->activation = ACT_RANDOM_ELEC1;
				}
				/* Brand the weapon with electricity. */
				get_quality(TRUE, BRAND_ELEC, 0, a_idx);

				/* Grant either a resist or immunity to electricity.*/
				if ((potential >= 4500) && (randint(3) == 1))
					get_quality(FALSE, IM_ELEC, 0, a_idx);
				else get_quality(TRUE, RES_ELEC, 0, a_idx);
			}
			/* ...of poison. */
			else if (selection < 28)
			{
				/* This is an exclusive club. */
				if (potential < 2000) break;

				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 4500)
						a_ptr->activation = ACT_RANDOM_POIS2;
					else
						a_ptr->activation = ACT_RANDOM_POIS1;
				}
				/* Brand the weapon with poison. */
				get_quality(TRUE, BRAND_POIS, 0, a_idx);

				/* Grant resistance to poison. */
				get_quality(TRUE, RES_POIS, 0, a_idx);
			}
			/* ...of life-sustaining. */
			else if (selection < 30)
			{
				/* This is an exclusive club. */
				if (potential < 4000) break;

				/* Possibly assign an activation for free. */
				if (randint(3) == 1)
				{
					a_ptr->activation = ACT_RANDOM_REGAIN;
				}

				/* Grant hold life. */
				get_quality(FALSE, HOLD_LIFE, 0, a_idx);

				/* Probably slay evil. */
				if (randint(3) != 1)
					get_quality(FALSE, SLAY_EVIL, 0, a_idx);

				/* Possibly resist nether. */
				if (randint(3) == 1)
					get_quality(FALSE, RES_NETHR, 0, a_idx);

				/* Possibly see invisible. */
				if (randint(2) == 1)
					get_quality(FALSE, SEE_INVIS, 0, a_idx);

				/* Possibly slay undead. */
				if (randint(3) == 1)
					get_quality(FALSE, SLAY_UNDEAD, 0, a_idx);
			}
			/* ...of retain stats. */
			else if (selection < 32)
			{
				/* This is an exclusive club. */
				if (potential < 3500) break;

				/* Possibly assign an activation for free. */
				if (randint(3) == 1)
				{
					a_ptr->activation = ACT_RANDOM_RESTORE;
				}

				/* Grant resist nexus. */
				get_quality(FALSE, RES_NEXUS, 0, a_idx);

				/* Possibly resist disenchantment. */
				if (randint(3) == 1)
					get_quality(FALSE, RES_DISEN, 0, a_idx);

				/* And some sustains. */
				if ((mp_ptr->spell_stat == A_STR) || (randint(2) == 1))
					get_quality(FALSE, SUST_STR, 0, a_idx);
				if ((mp_ptr->spell_stat == A_WIS) || (randint(2) == 1))
					get_quality(FALSE, SUST_WIS, 0, a_idx);
				if ((mp_ptr->spell_stat == A_INT) || (randint(2) == 1))
					get_quality(FALSE, SUST_INT, 0, a_idx);
				if ((mp_ptr->spell_stat == A_DEX) || (randint(2) == 1))
					get_quality(FALSE, SUST_DEX, 0, a_idx);
				if ((mp_ptr->spell_stat == A_CON) || (randint(2) == 1))
					get_quality(FALSE, SUST_CON, 0, a_idx);
				if ((mp_ptr->spell_stat == A_CHR) || (randint(2) == 1))
					get_quality(FALSE, SUST_CHR, 0, a_idx);
			}
			/* ...that shines with holy light. */
			else if (selection < 36)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential < 4500)
						a_ptr->activation = ACT_RANDOM_LIGHT1;
					else
						a_ptr->activation = ACT_RANDOM_LIGHT2;
				}

				/* Grant resist light and dark. */
				get_quality(TRUE, RES_LITE, 0, a_idx);
				get_quality(TRUE, RES_DARK, 0, a_idx);

				/* Possibly resist blindness. */
				if (randint(2) == 1)
					get_quality(FALSE, RES_BLIND, 0, a_idx);
			}
			/* ...with plenty of slays. */
			else if (selection < 41)
			{
				/* Load up on the slays. */
				if ((randint(6) == 1) && (potential >= 2000))
				{
					get_quality(TRUE, SLAY_EVIL, 0, a_idx);
					get_quality(TRUE, SLAY_ORC, 0, a_idx);
					get_quality(TRUE, SLAY_TROLL, 0, a_idx);
					get_quality(TRUE, SLAY_GIANT, 0, a_idx);
				}
				else if ((randint(5) == 1) && (potential >= 2000))
				{
					get_quality(TRUE, SLAY_EVIL, 0, a_idx);
					get_quality(TRUE, SLAY_UNDEAD, 0, a_idx);
					get_quality(TRUE, SLAY_DEMON, 0, a_idx);
				}
				else
				{
					if (randint(2) == 1)
						get_quality(FALSE, SLAY_ANIMAL, 0, a_idx);
					if (randint(2) == 1)
						get_quality(FALSE, SLAY_DRAGON, 0, a_idx);
					if (randint(4) == 1)
						get_quality(FALSE, SLAY_EVIL, 0, a_idx);
					if (randint(3) == 1)
						get_quality(FALSE, SLAY_ORC, 0, a_idx);
					if (randint(3) == 1)
						get_quality(FALSE, SLAY_TROLL, 0, a_idx);
					if (randint(3) == 1)
						get_quality(FALSE, SLAY_DEMON, 0, a_idx);
					if (randint(3) == 1)
						get_quality(FALSE, SLAY_GIANT, 0, a_idx);
				}
			}
			/* ...with enhanced damage dice. */
			else if (selection < 46)
			{
				/* This is an exclusive club. */
				if (potential < 2000) break;

				/* Use up to a third of the budget for enhancing the
				 * damage dice.
				 */
				get_quality(FALSE, ENHANCE_DICE, 3, a_idx);
			}
			/* ...that a druid wants by his side. */
			else if (selection < 50)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential >= 2500))
				{
					a_ptr->activation = ACT_RANDOM_SLOW_FOE;
				}

				/* Provide regenerative powers. */
				get_quality(FALSE, REGEN, 0, a_idx);

				/* Mark the weapon for a later bonus to stealth. */
				add_pval_later1 = STEALTH;
			}
			/* ...that is just the thing a mage is looking for. */
			else if (selection < 54)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential >= 2500))
				{
					a_ptr->activation = ACT_RANDOM_SLEEP_FOE;
				}

				/* Provide resistance to blindness. */
				get_quality(FALSE, RES_BLIND, 0, a_idx);

				/* Possibly mark the weapon for a later bonus to magic mastery. */
				if (randint(2) == 1) add_pval_later1 = MAGIC_MASTERY;
			}
			/* ...that a priest prays for. */
			else if (selection < 58)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential >= 2500))
				{
					a_ptr->activation = ACT_RANDOM_TURN_FOE;
				}

				/* Provide permanant light. */
				get_quality(FALSE, LITE, 0, a_idx);

				/* Bless the weapon. */
				get_quality(TRUE, BLESSED, 0, a_idx);

				/* Mark the weapon for a later bonus to wisdom. */
				add_pval_later1 = ADD_WIS;
			}
			/* ...that a necromancer would kill for. */
			else if (selection < 62)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential >= 2500))
				{
					a_ptr->activation = ACT_RANDOM_CONFU_FOE;
				}

				/* Provide resistance to confusion. */
				get_quality(FALSE, RES_CONFU, 0, a_idx);

				/* Mark the weapon for a later bonus to intelligence. */
				add_pval_later1 = ADD_INT;
			}
			/* ...twisted with chaos. */
			else if (selection < 65)
			{
				/* This is an exclusive club. */
				if (potential < 4000) break;

				/* Assign an activation for free. */
				a_ptr->activation = ACT_RANDOM_CHAOS;

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
				a_ptr->activation = ACT_RANDOM_SHARD_SOUND;

				/* Resist shards, sound, and confusion. */
				get_quality(TRUE, RES_SHARD, 0, a_idx);
				get_quality(TRUE, RES_SOUND, 0, a_idx);
				get_quality(TRUE, RES_CONFU, 0, a_idx);
			}
			/* ...that smashes foes and dungeon alike. */
			else if (selection < 72)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					a_ptr->activation = ACT_RANDOM_EARTHQUAKE;
				}
				/* 75% of the time, an earthquake brand. */
				if (randint(4) != 1) get_quality(TRUE, IMPACT, 0, a_idx);

				/* Enhance the damage dice. */
				get_quality(TRUE, ENHANCE_DICE, 3, a_idx);

				/* Grant either resistance or immunity to acid. */
				if ((potential >= 3500) && (randint(3) == 1))
					get_quality(FALSE, IM_ACID, 0, a_idx);
				else get_quality(TRUE, RES_ACID, 0, a_idx);

				/* Mark the weapon for a later bonus to tunneling. */
				add_pval_later1 = TUNNEL;

				/* Sometimes mark the weapon for a later bonus to strength. */
				if ((potential >= 750) && (randint(2) == 1))
				add_pval_later2 = ADD_STR;
			}
			/* ...that hunts down all the children of nature. */
			else if (selection < 76)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					a_ptr->activation = ACT_RANDOM_DETECT_MONSTERS;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_ANIMAL, 0, a_idx);

				/* A pair of survival skills. */
				get_quality(FALSE, REGEN, 0, a_idx);
				get_quality(FALSE, FEATHER, 0, a_idx);

				/* Mark the weapon for a later bonus to charisma. */
				add_pval_later1 = ADD_CHR;

				/* Sometimes mark the weapon for a later bonus to infravision. */
				if (randint(2) == 1)
					add_pval_later2 = INFRA;
			}
			/* ...that Orcs and Trolls must fear. */
			else if (selection < 79)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_STARLIGHT;
					else
						a_ptr->activation = ACT_RANDOM_LINE_LIGHT;
				}
				/* Naturally, the appropriate slays. */
				get_quality(TRUE, SLAY_ORC, 0, a_idx);
				get_quality(TRUE, SLAY_TROLL, 0, a_idx);

				/* Often, grant a bonus to ac. */
				if (randint(2) == 1)
					get_quality(FALSE, ADD_AC,
						randint(4) + potential / 1000, a_idx);

				/* Sometimes, slay giant. */
				if (randint(3) == 1)
					get_quality(FALSE, SLAY_GIANT, 0, a_idx);

				/* Mark the weapon for a later bonus to strength. */
				add_pval_later1 = ADD_STR;
			}
			/* ...that the undead cannot withstand. */
			else if (selection < 82)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential > 2500))
				{
					if (potential < 5000)
						a_ptr->activation = ACT_RANDOM_SMITE_UNDEAD;
					else
						a_ptr->activation = ACT_RANDOM_DISPEL_UNDEAD;
				}
				/* Grant slay undead and see invisible. */
				get_quality(TRUE, SLAY_UNDEAD, 0, a_idx);
				get_quality(TRUE, SEE_INVIS, 0, a_idx);

				/* Sometimes, hold life. */
				if (randint(3) == 1)
					get_quality(FALSE, HOLD_LIFE, 0, a_idx);

				/* Mark the weapon for a later bonus to wisdom. */
				add_pval_later1 = ADD_WIS;
			}
			/* ...that evil creatures everywhere flee from. */
			else if (selection < 90)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if ((potential >= 5000) && (randint(2) == 1))
						a_ptr->activation = ACT_RANDOM_DISPEL_EVIL;
					else if (potential >= 5000)
						a_ptr->activation = ACT_RANDOM_BANISH_EVIL;
					else if ((potential >= 3500) && (randint(3) != 1))
						a_ptr->activation = ACT_RANDOM_HOLY_ORB;
					else if (potential >= 2000)
						a_ptr->activation = ACT_RANDOM_PROT_FROM_EVIL;
					else
						a_ptr->activation = ACT_RANDOM_DETECT_EVIL;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_EVIL, 0, a_idx);

				/* Bless the weapon if necessary. */
				if ((a_ptr->tval == TV_POLEARM) || (a_ptr->tval == TV_SWORD))
					get_quality(FALSE, BLESSED, 0, a_idx);

				/* Sometimes, resist nether. */
				if (randint(6) == 1)
					get_quality(FALSE, RES_NETHR, 0, a_idx);

				/* Sometimes, resist dark. */
				if (randint(6) == 1)
					get_quality(FALSE, RES_DARK, 0, a_idx);

				/* Possibly mark the weapon for a later bonus to intelligence. */
				if (randint(3) == 1)
					add_pval_later1 = ADD_INT;
			}
			/* ...that demons intensely hate. */
			else if (selection < 93)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential > 2500))
				{
					a_ptr->activation = ACT_RANDOM_SMITE_DEMON;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_DEMON, 0, a_idx);

				/* Sometimes, nip the spawn of hell with cold as well. */
				if (randint(2) == 1)
					get_quality(FALSE, BRAND_COLD, 0, a_idx);

				/* Grant resistance to fire. */
				get_quality(FALSE, RES_FIRE, 0, a_idx);

				/* Mark the weapon for a later bonus to dexerity. */
				add_pval_later1 = ADD_DEX;
			}
			/* ...that dragons long to destroy. */
			else if (selection < 96)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) != 1) && (potential > 2500))
				{
					a_ptr->activation = ACT_RANDOM_SMITE_DRAGON;
				}
				/* Naturally, the appropriate slay. */
				get_quality(TRUE, SLAY_DRAGON, 0, a_idx);

				/* And one of the five elemental brands. */
				temp = randint(5);
				if (temp == 1)
					get_quality(FALSE, BRAND_FIRE, 0, a_idx);
				if (temp == 2)
					get_quality(FALSE, BRAND_COLD, 0, a_idx);
				if (temp == 3)
					get_quality(FALSE, BRAND_ACID, 0, a_idx);
				if (temp == 4)
					get_quality(FALSE, BRAND_ELEC, 0, a_idx);
				if (temp == 5)
					get_quality(FALSE, BRAND_POIS, 0, a_idx);

				/* Mark the weapon for a later bonus to constitution. */
				add_pval_later1 = ADD_CON;
			}
			/* ...that protects the wielder. */
			else if (selection < 1000)
			{
				/* This is an exclusive club. */
				if (potential < 2500) break;

				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 5000)
						a_ptr->activation = ACT_RANDOM_SHIELD;
					else
						a_ptr->activation = ACT_RANDOM_BLESS;
				}

				/* Grant a bonus to armour class. */
				get_quality(TRUE, ADD_AC,
					randint(6) + potential / 800, a_idx);

				/* And the four basic resists. */
				get_quality(TRUE, RES_ACID, 0, a_idx);
				get_quality(TRUE, RES_ELEC, 0, a_idx);
				get_quality(TRUE, RES_FIRE, 0, a_idx);
				get_quality(TRUE, RES_COLD, 0, a_idx);

				/* And some of the survival abilities. */
				if (randint(4) != 1)
					get_quality(TRUE, SEE_INVIS, 0, a_idx);
				if (randint(4) != 1)
					get_quality(TRUE, FEATHER, 0, a_idx);
				if (randint(4) != 1)
					get_quality(TRUE, FREE_ACT, 0, a_idx);
				if (randint(4) != 1)
					get_quality(TRUE, REGEN, 0, a_idx);
			}

			break;
		}

		/* I'm a missile weapon... */
		case TV_BOW:
		{
			/* ...with bonuses to Deadliness and Skill, and... */
			a_ptr->to_d += (3 + randint(9) + potential / 650);
			a_ptr->to_h += (3 + randint(9) + potential / 650);


			/* ...of extra shots. */
			if (selection < 50)
			{
				get_quality(TRUE, SHOTS, 0, a_idx);

				/* Sometimes also grant extra might. */
				if ((potential >= 2500) && (randint(3) == 1))
					get_quality(FALSE, MIGHT1, 0, a_idx);
			}
			/* ...of extra might. */
			else
			{
				if ((potential > 4000) && (randint(2) == 1))
					get_quality(TRUE, MIGHT2, 0, a_idx);
				else get_quality(TRUE, MIGHT1, 0, a_idx);

				/* Sometimes also grant extra shots. */
				if ((potential >= 2500) && (randint(3) == 1))
					get_quality(FALSE, SHOTS, 0, a_idx);
			}

			/* Sometimes, assign an activation. */
			if (potential > randint(5000))
			{
				if (randint(2) == 1)
					a_ptr->activation = ACT_RANDOM_SUPER_SHOOTING;
				else
					a_ptr->activation = ACT_RANDOM_BRAND_MISSILE;
			}

			/* Hack - avoid boring bows. */
			if (potential < 750) potential = 750;

			break;
		}

		/* I'm a piece of body armour... */
		case TV_SOFT_ARMOR: case TV_HARD_ARMOR: case TV_DRAG_ARMOR:
		{
			/* ...with a bonus to armour class, and... */
			a_ptr->to_a += (10 + randint(8) + potential / 1500);


			/* ...that resists most or all of the elements. */
			if (selection < 30)
			{
				/* Possibly assign an activation for free. */
				if ((randint(2) == 1) && (potential >= 3000))
				{
					if (potential >= 5500)
						a_ptr->activation = ACT_RANDOM_RESIST_ALL;
					else
						a_ptr->activation = ACT_RANDOM_RESIST_ELEMENTS;
				}

				if (randint(5) != 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(5) != 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(5) != 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(5) != 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);

				/* Sometimes, also poison. */
				if ((randint(2) == 1) && (potential >= 2000))
					get_quality(FALSE, RES_POIS, 0, a_idx);
			}
			/* ...that protects the very soul. */
			else if (selection < 40)
			{
				/* This is an exclusive club. */
				if (potential < 5500) break;

				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (randint(2) == 1)
						a_ptr->activation = ACT_RANDOM_HEAL3;
					else
						a_ptr->activation = ACT_RANDOM_REGAIN;
				}

				/* Resist nether and hold life. */
				get_quality(FALSE, RES_NETHR, 0, a_idx);
				get_quality(FALSE, HOLD_LIFE, 0, a_idx);

				/* Sometimes also resist chaos. */
				if (randint(2) == 1)
					get_quality(FALSE, RES_CHAOS, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (randint(3) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...resistant to sound and confusion. */
			else if (selection < 50)
			{
				/* Resist sound and confusion. */
				get_quality(FALSE, RES_SOUND, 0, a_idx);
				get_quality(FALSE, RES_CONFU, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (randint(3) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...with an amazing armour class. */
			else if (selection < 62)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) == 1) && (potential >= 4000))
				{
					a_ptr->activation = ACT_RANDOM_SHIELD;
				}

				/* Increase both base and magical ac. */
				get_quality(TRUE, ADD_AC,
					randint(3) + potential / 1600, a_idx);
				get_quality(TRUE, IMPROVE_BASE_AC,
					randint(3) + potential / 1600, a_idx);

				/* Collect a suite of basic resists. */
				if (randint(3) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that grants power over the realms of shadow. */
			else if (selection < 74)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (randint(2) == 1)
						a_ptr->activation = ACT_RANDOM_TELEPORT1;
					else
						a_ptr->activation = ACT_RANDOM_BLESS;
				}

				/* Resist dark. */
				get_quality(FALSE, RES_DARK, 0, a_idx);

				/* Mark the armour for a later bonus to stealth. */
				add_pval_later1 = STEALTH;

				/* Grant see invisible. */
				get_quality(FALSE, SEE_INVIS, 0, a_idx);

				/* Possibly resist nether. */
				if (randint(3) == 1)
					get_quality(FALSE, RES_NETHR, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (randint(3) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that protects against poison. */
			else if (selection < 82)
			{
				/* This is an exclusive club. */
				if (potential < 2500) break;

				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					a_ptr->activation = ACT_RANDOM_CURE;
				}

				/* Resist poison. */
				get_quality(FALSE, RES_POIS, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (randint(3) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that shines very brightly. */
			else if (selection < 95)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential < 4500)
						a_ptr->activation = ACT_RANDOM_LIGHT1;
					else
						a_ptr->activation = ACT_RANDOM_LIGHT2;
				}

				/* Grant permanant light. */
				get_quality(TRUE, LITE, 0, a_idx);

				/* And resistance to light. */
				get_quality(TRUE, RES_LITE, 0, a_idx);

				/* Collect a suite of basic resists. */
				if (randint(3) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(3) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(3) == 1)
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
					if (randint(3) != 1)
					{
						a_ptr->activation = ACT_RANDOM_FIRE3;
					}

					/* Immunity. */
					get_quality(TRUE, IM_FIRE, 0, a_idx);
				}
				/* ...cold. */
				if (temp == 2)
				{
					/* Assign an activation for free. */
					if (randint(3) != 1)
					{
						a_ptr->activation = ACT_RANDOM_COLD3;
					}

					/* Immunity. */
					get_quality(TRUE, IM_COLD, 0, a_idx);
				}
				/* ...acid. */
				if (temp == 3)
				{
					/* Assign an activation for free. */
					if (randint(3) != 1)
					{
						a_ptr->activation = ACT_RANDOM_ACID3;
					}

					/* Immunity. */
					get_quality(TRUE, IM_ACID, 0, a_idx);
				}
				/* ...electricity. */
				if (temp == 4)
				{
					/* Assign an activation for free. */
					if (randint(3) != 1)
					{
						a_ptr->activation = ACT_RANDOM_ELEC3;
					}

					/* Immunity. */
					get_quality(TRUE, IM_ELEC, 0, a_idx);
				}

				/* Collect a suite of basic resists. */
				if (randint(2) == 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(2) == 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(2) == 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(2) == 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}

			break;
		}

		/* I'm a shield... */
		case TV_SHIELD:
		{
			/* ...with a bonus to armour class, and... */
			a_ptr->to_a += (8 + randint(8) + potential / 1500);

			/* ...that resists most or all of the elements. */
			if (selection < 18)
			{
				/* Possibly assign an activation for free. */
				if ((randint(3) == 1) && (potential >= 3000))
				{
					if (potential >= 5500)
						a_ptr->activation = ACT_RANDOM_RESIST_ALL;
					else
						a_ptr->activation = ACT_RANDOM_RESIST_ELEMENTS;
				}

				if (randint(5) != 1)
					get_quality(TRUE, RES_ACID, 0, a_idx);
				if (randint(5) != 1)
					get_quality(TRUE, RES_ELEC, 0, a_idx);
				if (randint(5) != 1)
					get_quality(TRUE, RES_FIRE, 0, a_idx);
				if (randint(5) != 1)
					get_quality(TRUE, RES_COLD, 0, a_idx);
			}
			/* ...that increases strength and constitution. */
			else if (selection < 30)
			{
				/* Mark the shield for a later bonus to constitution. */
				add_pval_later1 = ADD_CON;

				/* Mark the shield for a later bonus to strength. */
				add_pval_later2 = ADD_STR;
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
				if ((randint(3) == 1) && (potential >= 4000))
				{
					a_ptr->activation = ACT_RANDOM_SHIELD;
				}

				/* Increase magical ac. */
				get_quality(TRUE, ADD_AC,
					randint(6) + potential / 2000, a_idx);

				/* Randomly add some survival-enhancing magics. */
				if (randint(4) == 1)
					get_quality(FALSE, REGEN, 0, a_idx);
				if (randint(4) == 1)
					get_quality(FALSE, FEATHER, 0, a_idx);
				if (randint(4) == 1)
					get_quality(FALSE, REGEN, 0, a_idx);
				if (randint(4) == 1)
					get_quality(FALSE, SEE_INVIS, 0, a_idx);
				if (randint(4) == 1)
					get_quality(FALSE, FREE_ACT, 0, a_idx);
				if (randint(4) == 1)
					get_quality(FALSE, RES_BLIND, 0, a_idx);
				if (randint(4) == 1)
					get_quality(FALSE, RES_CONFU, 0, a_idx);

			}

			break;
		}

		/* I'm a pair of boots... */
		case TV_BOOTS:
		{
			/* ...with a bonus to armour class, and... */
			a_ptr->to_a += (6 + randint(5) + potential / 1500);


			/* ...that makes he who wears me run like the wind. */
			if (selection < 20)
			{
				/* This is an exclusive club. */
				if (potential < 5000) break;

				/* Calculate pval. */
				temp = 5 + damroll(3,2);

				/* Add a speed bonus immediately. */
				a_ptr->pval = temp;
				get_quality(TRUE, SPEED, temp, a_idx);
			}
			/* ...that keeps a guy's feet on the ground. */
			else if (selection < 30)
			{
				/* Resist nexus and feather fall. */
				get_quality(TRUE, RES_NEXUS, 0, a_idx);
				get_quality(TRUE, FEATHER, 0, a_idx);
			}
			/* ...with unrivaled magical movement. */
			else if (selection < 45)
			{
				/* Assign an activation,... */
				if (potential >= 4000)
					a_ptr->activation = ACT_RANDOM_RECALL;
				else if (potential >= 2000)
					a_ptr->activation = ACT_RANDOM_TELEPORT2;
				else
					a_ptr->activation = ACT_RANDOM_TELEPORT1;

				/* ...but not for free. */
				potential = 2 * potential / 3;

				/* Resist nexus. */
				get_quality(FALSE, RES_NEXUS, 0, a_idx);

				/* Possibly mark the shield for a later bonus to dexerity. */
				if (potential >= 1000)
					add_pval_later1 = ADD_DEX;
			}
			/* ...that makes long marches easy. */
			else if (selection < 55)
			{
				/* Possibly assign an activation for free. */
				if (randint(2) == 1)
				{
					if (potential >= 5500)
						a_ptr->activation = ACT_RANDOM_HEAL3;
					else if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_HEAL2;
					else
						a_ptr->activation = ACT_RANDOM_HEAL1;
				}

				/* Grant regenerative powers. */
				get_quality(TRUE, REGEN, 0, a_idx);

				/* Mark the shield for a later bonus to constitution. */
				add_pval_later1 = ADD_CON;
			}
			/* ...that dance up a storm. */
			else if (selection < 65)
			{
				/* Possibly assign an activation for free. */
				if ((randint(2) == 1) && (potential >= 2500))
					a_ptr->activation = ACT_RANDOM_STORM_DANCE;

				/* Grant feather fall. */
				get_quality(TRUE, FEATHER, 0, a_idx);

				/* Mark the boots for a later bonus to dexterity. */
				add_pval_later1 = ADD_DEX;
			}
			/* ...worn by a famous ranger of old. */
			else if (selection < 75)
			{
				/* Possibly assign an activation for free. */
				if (randint(2) == 1)
					a_ptr->activation = ACT_RANDOM_DETECT_MONSTERS;

				/* Grant regeneration and slow digest. */
				get_quality(TRUE, REGEN, 0, a_idx);
				get_quality(TRUE, SLOW_DIGEST, 0, a_idx);

				/* Possibly telepathy. */
				if (randint(6) == 1)
					get_quality(FALSE, TELEPATHY, 0, a_idx);
			}

			/* Possibly assign a speed activation for free. */
			if ((randint(4) == 1) && (a_ptr->activation == 0) &&
				(potential >= 2000))
			{
				a_ptr->activation = ACT_RANDOM_SPEED;
			}

			break;
		}

		/* I'm a cloak... */
		case TV_CLOAK:
		{
			/* ...with a bonus to armour class, and... */
			a_ptr->to_a += (8 + randint(6) + potential / 1500);


			/* ...that hides the wearer from hostile eyes. */
			if (selection < 20)
			{
				/* Possibly assign an activation for free. */
				if (randint(2) == 1)
					a_ptr->activation = ACT_RANDOM_SLEEP_FOES;

				/* Mark the cloak for a later bonus to stealth. */
				add_pval_later1 = STEALTH;
			}
			/* ...that confuses and dismays foes. */
			else if (selection < 30)
			{
				/* Possibly assign an activation for free. */
				if (randint(2) == 1)
				{
					if (randint(2) == 1)
						a_ptr->activation = ACT_RANDOM_CONFU_FOES;
					else
						a_ptr->activation = ACT_RANDOM_TURN_FOES;
				}

			}
			/* ...with amazing protective powers. */
			else if (selection < 40)
			{
				/* Possibly assign an activation for free. */
				if ((randint(4) == 1) && (potential >= 3000))
				{
					a_ptr->activation = ACT_RANDOM_SHIELD;
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
				if (randint(2) == 1)
					a_ptr->activation = ACT_RANDOM_DETECT_MONSTERS;
				else
					a_ptr->activation = ACT_RANDOM_DETECT_EVIL;
			}
			/* ...that is aware of the world around. */
			else if (selection < 60)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if ((potential >= 4000) && (randint(3) != 1))
						a_ptr->activation = ACT_RANDOM_DETECT_ALL;
					else if (potential >= 3000)
						a_ptr->activation = ACT_RANDOM_MAGIC_MAP;
					else
						a_ptr->activation = ACT_RANDOM_DETECT_D_S_T;
				}

				/* Mark the boots for a later bonus to searching. */
				add_pval_later1 = SEARCH;
			}
			/* ...of necromantic powers. */
			else if (selection < 65)
			{
				/* Possibly assign an activation for free. */
				if (randint(2) == 1)
					a_ptr->activation = ACT_RANDOM_SLOW_FOES;

				/* Resist chaos, dark, or nether */
				temp = randint(3);
				if (temp == 1) get_quality(FALSE, RES_CHAOS, 0, a_idx);
				else if (temp == 2) get_quality(FALSE, RES_NETHR, 0, a_idx);
				else if (temp == 3) get_quality(FALSE, RES_DARK, 0, a_idx);

				/* Mark the boots for a later bonus to infravision. */
				add_pval_later1 = INFRA;
			}

			/* Hack -- Elven Cloaks have STEALTH, so force a pval */
			if (!add_pval_later1 && (k_ptr->sval == SV_ELVEN_CLOAK))
			{
				/* Should this be for free? */
				a_ptr->pval = 2 + randint(potential / 2500);
			}

			break;
		}

		/* I'm a helm or crown... */
		case TV_HELM: case TV_CROWN:
		{
			/* ...with a bonus to armour class, and... */
			a_ptr->to_a += (8 + randint(6) + potential / 1500);


			/* ...of telepathy. */
			if (selection < 24)
			{
				/* This is an exclusive club. */
				if (potential < 3500) break;

				/* Grant telepathy. */
				get_quality(FALSE, TELEPATHY, 0, a_idx);
			}
			/* ...that maintains serenity amid uncertainly. */
			else if (selection < 35)
			{
				/* Possibly assign an activation for free. */
				if ((randint(2) == 1) && (potential >= 4500))
					a_ptr->activation = ACT_RANDOM_CURE;

				/* Grant resistance to confusion and sound. */
				get_quality(FALSE, RES_CONFU, 0, a_idx);
				get_quality(FALSE, RES_SOUND, 0, a_idx);
			}
			/* ...preservative of sight and awareness. */
			else if (selection < 46)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 3750)
						a_ptr->activation = ACT_RANDOM_DETECT_ALL;
					else
						a_ptr->activation = ACT_RANDOM_DETECT_D_S_T;
				}

				/* Grant resistance to blindness and see invisible. */
				get_quality(FALSE, RES_BLIND, 0, a_idx);
				get_quality(FALSE, SEE_INVIS, 0, a_idx);

				/* Mark the headpiece for a later bonus to searching. */
				add_pval_later1 = SEARCH;
			}
			/* ...whose wearer stands taller in the eyes of men. */
			else if (selection < 57)
			{
				/* Mark the headpiece for a later bonus to wisdom. */
				add_pval_later1 = ADD_WIS;

				/* Mark the headpiece for a later bonus to charisma. */
				add_pval_later2 = ADD_CHR;

				/* Possibly add a bonus to base ac. */
				if (randint(3) == 1)
					get_quality(FALSE, IMPROVE_BASE_AC, randint(2) + 2, a_idx);
			}
			/* ...strong in battle. */
			else if (selection < 68)
			{
				/* Possibly assign an activation for free. */
				if ((potential >= 2500) && (randint(3) != 1))
					a_ptr->activation = ACT_RANDOM_FRIGHTEN_ALL;
				else
					a_ptr->activation = ACT_RANDOM_HEROISM;

				/* No fear. */
				get_quality(TRUE, RES_FEAR, 0, a_idx);

				/* Mark the headpiece for a later bonus to strength. */
				add_pval_later1 = ADD_STR;

				/* Mark the headpiece for a later bonus to dexterity. */
				add_pval_later2 = ADD_DEX;

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
				a_ptr->activation = ACT_RANDOM_IDENTIFY;
			}
			/* ...which can focus healing magics. */
			else if (selection < 90)
			{
				/* Possibly assign an activation for free. */
				if (randint(3) != 1)
				{
					if (potential >= 4000)
						a_ptr->activation = ACT_RANDOM_HEAL3;
					else if (potential >= 2500)
						a_ptr->activation = ACT_RANDOM_HEAL2;
					else
						a_ptr->activation = ACT_RANDOM_HEAL1;
				}

				/* Grant regeneration. */
				get_quality(TRUE, REGEN, 0, a_idx);
			}

			break;
		}

		/* I'm a pair of gloves... */
		case TV_GLOVES:
		{
			/* ...with a bonus to armour class, and... */
			a_ptr->to_a += (7 + randint(5) + potential / 1500);


			/* ...that grant increased combat prowess. */
			if (selection < 30)
			{
				/* Grant equal bonuses to Skill and Deadliness. */
				temp = 6 + randint(4);
				get_quality(TRUE, ADD_DEADLINESS, temp, a_idx);
				get_quality(TRUE, ADD_SKILL, temp, a_idx);

				/* Often, acquire free action. */
				if (randint(3) != 1)
					get_quality(TRUE, FREE_ACT, 0, a_idx);
			}
			/* ...with the dauntless spirit of a mighty warrior. */
			else if (selection < 45)
			{
				/* No fear. */
				get_quality(TRUE, RES_FEAR, 0, a_idx);

				/* Often, grant regeneration. */
				if (randint(3) != 1)
					get_quality(FALSE, REGEN, 0, a_idx);

				/* Sometimes, acquire free action. */
				if (randint(2) == 1)
					get_quality(FALSE, FREE_ACT, 0, a_idx);


				/* Mark one of the combat stats for later acquisition. */
				if (randint(3) == 1) add_pval_later1 = ADD_STR;
				else if (randint(2) == 1) add_pval_later1 = ADD_DEX;
				else add_pval_later1 = ADD_CON;

				/* Possibly grant equal bonuses to Skill and Deadliness. */
				if ((potential >= 1500) && (randint(4) != 1))
				{
					temp = 4 + randint(4);
					get_quality(FALSE, ADD_DEADLINESS, temp, a_idx);
					get_quality(FALSE, ADD_SKILL, temp, a_idx);
				}
			}
			/* ...able to protect the wearer. */
			else if (selection < 60)
			{
				/* Increase bonus to AC. */
				get_quality(FALSE, ADD_AC, 4 + randint(4), a_idx);

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
				if ((potential >= 500) && (randint(3) != 1))
				{
					if (temp == 1)
						a_ptr->activation = ACT_RANDOM_FIRE1;
					if (temp == 2)
						a_ptr->activation = ACT_RANDOM_COLD1;
					if (temp == 3)
						a_ptr->activation = ACT_RANDOM_ACID1;
					if (temp == 4)
						a_ptr->activation = ACT_RANDOM_ELEC1;
				}
			}
			/* ...with the cunning of a rogue of legend. */
			else if (selection < 75)
			{
				/* Assign an activation for free. */
				a_ptr->activation = ACT_RANDOM_DISARM;

				/* Mark the gloves for a later bonus to stealth. */
				add_pval_later1 = STEALTH;

				/* Mark the gloves for a later bonus to searching. */
				add_pval_later2 = SEARCH;

				/* Often, acquire free action. */
				if (randint(3) != 1)
					get_quality(FALSE, FREE_ACT, 0, a_idx);
			}
			/* ...that untangles magical conundrums. */
			else if (selection < 90)
			{
				/* Mark the gloves for a later bonus to magic item mastery. */
				add_pval_later2 = MAGIC_MASTERY;
			}
			/* ...with a deadly mastery of archery. */
			else if (selection < 100)
			{
				/* This is an exclusive club. */
				if (potential < 3500) break;

				/* Always grant an activation, but not for free. */
				a_ptr->activation = ACT_RANDOM_SUPER_SHOOTING;
				potential -= 750;

				/* Add equal bonuses to Skill and to Deadliness. */
				temp = 5 + randint(5);
				get_quality(FALSE, ADD_DEADLINESS, temp, a_idx);
				get_quality(FALSE, ADD_SKILL, temp, a_idx);

				/* Often, acquire free action. */
				if (randint(3) != 1)
					get_quality(FALSE, FREE_ACT, 0, a_idx);
			}
			break;
		}
		default:
		{
			msg_print("error -- object kind not recognized.");
			break;
		}
	}

	/* It is possible for artifacts to be unusually heavy or light. */
	if (randint(6) == 1)
	{
		old_weight = a_ptr->weight;

		/* Sometimes, they are unusually heavy. */
		if ((randint(2) == 1) && (a_ptr->weight >= 30))
		{
			a_ptr->weight = 5 * (a_ptr->weight + 50) / 4;
			potential += (a_ptr->weight - old_weight) * 10;
		}

		/* Sometimes, they are unusually light. */
		else if (a_ptr->weight >= 50)
		{
			a_ptr->weight = 2 * (a_ptr->weight - 20) / 3;
			potential -= (old_weight - a_ptr->weight) * 10;
			if (potential < 0) potential = 0;
		}
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
	object_kind *k_ptr = &k_info[lookup_kind(a_ptr->tval, a_ptr->sval)];
	int i, rounds;
	int pval = 0;
	int choice = 0;


	/* Firstly, determine pval, add some pval-dependant qualities, and
	 * charge the account.  We assume that no pval-dependant qualities
	 * have been added yet.
	 */

	/* If qualities that depend on pval have been promised, add them. */
	if ((add_pval_later1) || (add_pval_later2))
	{
		a_ptr->pval = 2 + randint(potential / 2500);
		if (add_pval_later1)
		{
			get_quality(TRUE, add_pval_later1, a_ptr->pval, a_idx);
			add_pval_later1 = 0;
		}
		if (add_pval_later2)
		{
			get_quality(TRUE, add_pval_later2, a_ptr->pval, a_idx);
			add_pval_later2 = 0;
		}
	}
	else
	{
		/* Rarely, and only if the artifact has a lot of potential,
		 * add all the stats. */
		if ((randint(6) == 1) && (potential >= 4500))
		{
			get_quality(TRUE, ADD_STR, 1, a_idx);
			get_quality(TRUE, ADD_WIS, 1, a_idx);
			get_quality(TRUE, ADD_INT, 1, a_idx);
			get_quality(TRUE, ADD_DEX, 1, a_idx);
			get_quality(TRUE, ADD_CON, 1, a_idx);
			get_quality(TRUE, ADD_CHR, 1, a_idx);

			a_ptr->pval = 1;
		}

		/* Otherwise, if at least some potential remains, add a
		 * pval-dependant quality or two with 67% probability. */
		else if ((randint(3) != 1) && (potential >= 750))
		{
			/* Add the pval. */
			pval = potential / 2000 + randint(2);
			a_ptr->pval = pval;

			/* Determine number of loops. */
			rounds = randint(2);

			for (i = 0; i < rounds; i++)
			{

				/* Only melee weapons can get a bonus to tunnelling. */
				if ((a_ptr->tval == TV_SWORD) ||
					(a_ptr->tval == TV_POLEARM) ||
						(a_ptr->tval == TV_HAFTED))
					choice = randint(11);
				else
	 				choice = randint(10);

				if (choice == 1)
					get_quality(FALSE, ADD_STR, pval, a_idx);
				if (choice == 2)
					get_quality(FALSE, ADD_WIS, pval, a_idx);
				if (choice == 3)
					get_quality(FALSE, ADD_INT, pval, a_idx);
				if (choice == 4)
					get_quality(FALSE, ADD_DEX, pval, a_idx);
				if (choice == 5)
					get_quality(FALSE, ADD_CON, pval, a_idx);
				if (choice == 6)
					get_quality(FALSE, ADD_CHR, pval, a_idx);
				if (choice == 7)
					get_quality(FALSE, INFRA, pval, a_idx);
				if (choice == 8)
					get_quality(FALSE, STEALTH, pval, a_idx);
				if (choice == 9)
					get_quality(FALSE, SEARCH, pval, a_idx);
				if (choice == 10)
					get_quality(FALSE, SPEED, pval, a_idx);
				if (choice == 11)
					get_quality(FALSE, TUNNEL, pval, a_idx);

				choice = 0;
			}
		}
	}

	/* Artifacts that still have lots of money to spend deserve the best. */
	if ((potential > 2000) && (randint(3) != 1))
	{
		/* Make a choice... */
		choice = randint(5);

		/* ...among some tasty options. */
		if ((choice == 1) && (!(a_ptr->flags3 & TR3_TELEPATHY)))
		{
			get_quality(FALSE, TELEPATHY, 0, a_idx);
		}
		if ((choice == 2) && (!(a_ptr->flags3 & TR3_HOLD_LIFE)))
		{
			get_quality(FALSE, HOLD_LIFE, 0, a_idx);
		}
		if (choice == 3)
		{
			if (!(a_ptr->flags2 & TR2_RES_CONFU))
				get_quality(FALSE, RES_CONFU, 0, a_idx);
			if (!(a_ptr->flags2 & TR2_RES_BLIND))
				get_quality(FALSE, RES_BLIND, 0, a_idx);
		}
		if (choice == 4)
		{
			if ((randint(4) == 1) && (!(a_ptr->flags2 & TR2_RES_DISEN)))
				get_quality(FALSE, RES_DISEN, 0, a_idx);
			else if ((randint(3) == 1) && (!(a_ptr->flags2 & TR2_RES_NETHR)))
				get_quality(FALSE, RES_NETHR, 0, a_idx);
			else if ((randint(2) == 1) && (!(a_ptr->flags2 & TR2_RES_CHAOS)))
				get_quality(FALSE, RES_CHAOS, 0, a_idx);
			else if (!(a_ptr->flags2 & TR2_RES_POIS))
				get_quality(FALSE, RES_POIS, 0, a_idx);
		}
		if ((choice == 5) && (potential > 4500))
		{
			if (randint(2) == 1)
			{
				if (a_ptr->pval > 0)
					get_quality(FALSE, SPEED, a_ptr->pval, a_idx);

				/* Go for a +10 bonus to speed. */
				else if (get_quality(FALSE, SPEED, 10, a_idx))
					a_ptr->pval = 10;
			}
			else
			{
				get_quality(FALSE, SUST_STR, pval, a_idx);
				get_quality(FALSE, SUST_WIS, pval, a_idx);
				get_quality(FALSE, SUST_INT, pval, a_idx);
				get_quality(FALSE, SUST_DEX, pval, a_idx);
				get_quality(FALSE, SUST_CON, pval, a_idx);
				get_quality(FALSE, SUST_CHR, pval, a_idx);
			}
		}

		/* Sometimes, also add a new pval-dependant quality.  Infravision,
		 * speed, and magical item mastery are not on offer.  Only melee
		 * weapons can get a bonus to tunnelling.
		 */
		if ((randint(2) == 1) && (a_ptr->pval))
		{
			pval = a_ptr->pval;

			if ((a_ptr->tval == TV_SWORD) ||
				(a_ptr->tval == TV_POLEARM) ||
					(a_ptr->tval == TV_HAFTED))
				choice = randint(9);
			else
	 			choice = randint(8);

			if ((choice == 1) && (!(a_ptr->flags1 & TR1_STR)))
				get_quality(FALSE, ADD_STR, pval, a_idx);
			if ((choice == 2) && (!(a_ptr->flags1 & TR1_WIS)))
				get_quality(FALSE, ADD_WIS, pval, a_idx);
			if ((choice == 3) && (!(a_ptr->flags1 & TR1_INT)))
				get_quality(FALSE, ADD_INT, pval, a_idx);
			if ((choice == 4) && (!(a_ptr->flags1 & TR1_DEX)))
				get_quality(FALSE, ADD_DEX, pval, a_idx);
			if ((choice == 5) && (!(a_ptr->flags1 & TR1_CON)))
				get_quality(FALSE, ADD_CON, pval, a_idx);
			if ((choice == 6) && (!(a_ptr->flags1 & TR1_CHR)))
				get_quality(FALSE, ADD_CHR, pval, a_idx);
			if ((choice == 7) && (!(a_ptr->flags1 & TR1_STEALTH)))
				get_quality(FALSE, STEALTH, pval, a_idx);
			if ((choice == 8) && (!(a_ptr->flags1 & TR1_SEARCH)))
				get_quality(FALSE, SEARCH, pval, a_idx);

			if ((choice == 9) && (!(a_ptr->flags1 & TR1_TUNNEL)))
				get_quality(FALSE, TUNNEL, pval, a_idx);

				choice = 0;
		}
	}


	/* Now, we enter Filene's Basement, and shop 'til we drop! */
	while (potential >= 300)
	{
		/* I'm a melee weapon. */
		if ((a_ptr->tval == TV_SWORD) || (a_ptr->tval == TV_POLEARM) ||
			(a_ptr->tval == TV_HAFTED))
		{
			/* Some throwing weapons can be thrown hard and fast. */
			if ((a_ptr->flags1 & TR1_THROWING) && (randint(2) == 1))
				get_quality(FALSE, PERFECT_BALANCE, 0, a_idx);

			/* Some weapons already will have superb base damage. */
			if ((a_ptr->dd > k_ptr->dd) || (a_ptr->ds > k_ptr->ds))
			{
				/* Sometimes, such weapons are unusual. */
				if ((randint(2) == 1) && (potential >= 1500))
				{
					/* Sometimes, such weapons are unusually heavy. */
					if (randint(3) == 1)
					{
						a_ptr->weight = 3 * a_ptr->weight / 2 + 5;
						potential += 500;
					}

					/* Sometimes, such weapons are usually light. */
					else if ((randint(2) == 1) && (a_ptr->weight >= 150))
					{
						a_ptr->weight = a_ptr->weight / 2;
						potential -= 500;
					}

					/* Sometimes spend everything to enhance
					 * the damage dice.
					 */
					if (randint(3) == 1)
					{
						/* Probably sacrifice the Skill bonus. */
						if (randint(3) != 1)
						{
							a_ptr->to_h = 0;
							potential += 600;

							/* Possibly also sacrifice the Deadliness bonus. */
							if (randint(3) == 1)
							{
								a_ptr->to_h = 0;
								potential += 600;
							}
						}
						get_quality(FALSE, ENHANCE_DICE, 1, a_idx);

						/* We're done. */
						break;
					}
				}
			}

			/* Other weapons have enhanced damage dice in addition
			 * to other qualities. */
			else if (randint(a_ptr->dd * a_ptr->ds) == 1)
			{
				get_quality(FALSE, ENHANCE_DICE, 3, a_idx);
			}

			/* Collect a slay or brand, if it is affordable. */
			choice = randint(13);

			if ((choice == 1) && (!(a_ptr->flags1 & TR1_SLAY_ANIMAL)))
				get_quality(FALSE, SLAY_ANIMAL, 0, a_idx);
			if ((choice == 2) && (!(a_ptr->flags1 & TR1_SLAY_EVIL)))
				get_quality(FALSE, SLAY_EVIL, 0, a_idx);
			if ((choice == 3) && (!(a_ptr->flags1 & TR1_SLAY_UNDEAD)))
				get_quality(FALSE, SLAY_UNDEAD, 0, a_idx);
			if ((choice == 4) && (!(a_ptr->flags1 & TR1_SLAY_DEMON)))
				get_quality(FALSE, SLAY_DEMON, 0, a_idx);
			if ((choice == 5) && (!(a_ptr->flags1 & TR1_SLAY_ORC)))
				get_quality(FALSE, SLAY_ORC, 0, a_idx);
			if ((choice == 6) && (!(a_ptr->flags1 & TR1_SLAY_TROLL)))
				get_quality(FALSE, SLAY_TROLL, 0, a_idx);
			if ((choice == 7) && (!(a_ptr->flags1 & TR1_SLAY_GIANT)))
				get_quality(FALSE, SLAY_GIANT, 0, a_idx);
			if ((choice == 8) && (!(a_ptr->flags1 & TR1_SLAY_DRAGON)))
				get_quality(FALSE, SLAY_DRAGON, 0, a_idx);
			if ((choice == 9) && (!(a_ptr->flags1 & TR1_BRAND_FIRE)))
				get_quality(FALSE, BRAND_FIRE, 0, a_idx);
			if ((choice == 10) && (!(a_ptr->flags1 & TR1_BRAND_COLD)))
				get_quality(FALSE, BRAND_COLD, 0, a_idx);
			if ((choice == 11) && (!(a_ptr->flags1 & TR1_BRAND_ACID)))
				get_quality(FALSE, BRAND_ACID, 0, a_idx);
			if ((choice == 12) && (!(a_ptr->flags1 & TR1_BRAND_ELEC)))
				get_quality(FALSE, BRAND_ELEC, 0, a_idx);
			if ((choice == 13) && (!(a_ptr->flags1 & TR1_BRAND_POIS)))
				get_quality(FALSE, BRAND_POIS, 0, a_idx);

			/* Often, collect a miscellanious quality, if it is affordable. */
			if (randint(2) == 1)
			{
				choice = randint(6);

				if ((choice == 1) && (!(a_ptr->flags3 & TR3_SLOW_DIGEST)))
					get_quality(FALSE, SLOW_DIGEST, 0, a_idx);
				if ((choice == 2) && (!(a_ptr->flags3 & TR3_FEATHER)))
					get_quality(FALSE, FEATHER, 0, a_idx);
				if ((choice == 3) && (!(a_ptr->flags3 & TR3_LITE)))
					get_quality(FALSE, LITE, 0, a_idx);
				if ((choice == 4) && (!(a_ptr->flags3 & TR3_REGEN)))
					get_quality(FALSE, REGEN, 0, a_idx);
				if ((choice == 5) && (!(a_ptr->flags3 & TR3_SEE_INVIS)))
					get_quality(FALSE, SEE_INVIS, 0, a_idx);
				if ((choice == 6) && (!(a_ptr->flags3 & TR3_FREE_ACT)))
					get_quality(FALSE, FREE_ACT, 0, a_idx);
			}

			/* Sometimes, collect a resistance, if it is affordable. */
			if (randint(3) == 1)
			{
				choice = randint(12);

				if ((choice == 1) && (!(a_ptr->flags2 & TR2_RES_FIRE)))
					get_quality(FALSE, RES_FIRE, 0, a_idx);
				if ((choice == 2) && (!(a_ptr->flags2 & TR2_RES_COLD)))
					get_quality(FALSE, RES_COLD, 0, a_idx);
				if ((choice == 3) && (!(a_ptr->flags2 & TR2_RES_ACID)))
					get_quality(FALSE, RES_ACID, 0, a_idx);
				if ((choice == 4) && (!(a_ptr->flags2 & TR2_RES_ELEC)))
					get_quality(FALSE, RES_ELEC, 0, a_idx);
				if ((choice == 5) && (!(a_ptr->flags2 & TR2_RES_FEAR)))
					get_quality(FALSE, RES_FEAR, 0, a_idx);
				if ((choice == 6) && (!(a_ptr->flags2 & TR2_RES_LITE)))
					get_quality(FALSE, RES_LITE, 0, a_idx);
				if ((choice == 7) && (!(a_ptr->flags2 & TR2_RES_DARK)))
					get_quality(FALSE, RES_DARK, 0, a_idx);
				if ((choice == 8) && (!(a_ptr->flags2 & TR2_RES_BLIND)))
					get_quality(FALSE, RES_BLIND, 0, a_idx);
				if ((choice == 9) && (!(a_ptr->flags2 & TR2_RES_CONFU)))
					get_quality(FALSE, RES_CONFU, 0, a_idx);
				if ((choice == 10) && (!(a_ptr->flags2 & TR2_RES_SOUND)))
					get_quality(FALSE, RES_SOUND, 0, a_idx);
				if ((choice == 11) && (!(a_ptr->flags2 & TR2_RES_SHARD)))
					get_quality(FALSE, RES_SHARD, 0, a_idx);
				if ((choice == 12) && (!(a_ptr->flags2 & TR2_RES_NEXUS)))
					get_quality(FALSE, RES_NEXUS, 0, a_idx);
			}

			/* Clean out the wallet. */
			if ((potential < 500) && (randint(5) == 1))
			{
				if (a_ptr->to_d > 0) a_ptr->to_d += potential / 150;
				if (a_ptr->to_h > 0) a_ptr->to_h += potential / 150;
				potential = 0;
			}
		}


		/* I'm a missile weapon. */
		if (a_ptr->tval == TV_BOW)
		{
			/* Collect a miscellanious quality, if it is affordable. */
			choice = randint(6);

			if ((choice == 1) && (!(a_ptr->flags3 & TR3_SLOW_DIGEST)))
				get_quality(FALSE, SLOW_DIGEST, 0, a_idx);
			if ((choice == 2) && (!(a_ptr->flags3 & TR3_FEATHER)))
				get_quality(FALSE, FEATHER, 0, a_idx);
			if ((choice == 3) && (!(a_ptr->flags3 & TR3_LITE)))
				get_quality(FALSE, LITE, 0, a_idx);
			if ((choice == 4) && (!(a_ptr->flags3 & TR3_REGEN)))
				get_quality(FALSE, REGEN, 0, a_idx);
			if ((choice == 5) && (!(a_ptr->flags3 & TR3_SEE_INVIS)))
				get_quality(FALSE, SEE_INVIS, 0, a_idx);
			if ((choice == 6) && (!(a_ptr->flags3 & TR3_FREE_ACT)))
				get_quality(FALSE, FREE_ACT, 0, a_idx);

			/* Sometimes, collect a resistance, if it is affordable. */
			if (randint(2) == 1)
			{
				choice = randint(12);

				if ((choice == 1) && (!(a_ptr->flags2 & TR2_RES_FIRE)))
					get_quality(FALSE, RES_FIRE, 0, a_idx);
				if ((choice == 2) && (!(a_ptr->flags2 & TR2_RES_COLD)))
					get_quality(FALSE, RES_COLD, 0, a_idx);
				if ((choice == 3) && (!(a_ptr->flags2 & TR2_RES_ACID)))
					get_quality(FALSE, RES_ACID, 0, a_idx);
				if ((choice == 4) && (!(a_ptr->flags2 & TR2_RES_ELEC)))
					get_quality(FALSE, RES_ELEC, 0, a_idx);
				if ((choice == 5) && (!(a_ptr->flags2 & TR2_RES_FEAR)))
					get_quality(FALSE, RES_FEAR, 0, a_idx);
				if ((choice == 6) && (!(a_ptr->flags2 & TR2_RES_LITE)))
					get_quality(FALSE, RES_LITE, 0, a_idx);
				if ((choice == 7) && (!(a_ptr->flags2 & TR2_RES_DARK)))
					get_quality(FALSE, RES_DARK, 0, a_idx);
				if ((choice == 8) && (!(a_ptr->flags2 & TR2_RES_BLIND)))
					get_quality(FALSE, RES_BLIND, 0, a_idx);
				if ((choice == 9) && (!(a_ptr->flags2 & TR2_RES_CONFU)))
					get_quality(FALSE, RES_CONFU, 0, a_idx);
				if ((choice == 10) && (!(a_ptr->flags2 & TR2_RES_SOUND)))
					get_quality(FALSE, RES_SOUND, 0, a_idx);
				if ((choice == 11) && (!(a_ptr->flags2 & TR2_RES_SHARD)))
					get_quality(FALSE, RES_SHARD, 0, a_idx);
				if ((choice == 12) && (!(a_ptr->flags2 & TR2_RES_NEXUS)))
					get_quality(FALSE, RES_NEXUS, 0, a_idx);
			}

			/* Clean out the wallet. */
			if ((potential < 500) && (randint(5) == 1))
			{
				if (a_ptr->to_d > 0) a_ptr->to_d += potential / 150;
				if (a_ptr->to_h > 0) a_ptr->to_h += potential / 150;
				potential = 0;
			}
		}


		/* I'm any piece of armour. */
		else
		{
			/* Collect a resistance, if it is affordable. */
			choice = randint(12);

			if ((choice == 1) && (!(a_ptr->flags2 & TR2_RES_FIRE)))
				get_quality(FALSE, RES_FIRE, 0, a_idx);
			if ((choice == 2) && (!(a_ptr->flags2 & TR2_RES_COLD)))
				get_quality(FALSE, RES_COLD, 0, a_idx);
			if ((choice == 3) && (!(a_ptr->flags2 & TR2_RES_ACID)))
				get_quality(FALSE, RES_ACID, 0, a_idx);
			if ((choice == 4) && (!(a_ptr->flags2 & TR2_RES_ELEC)))
				get_quality(FALSE, RES_ELEC, 0, a_idx);
			if ((choice == 5) && (!(a_ptr->flags2 & TR2_RES_FEAR)))
				get_quality(FALSE, RES_FEAR, 0, a_idx);
			if ((choice == 6) && (!(a_ptr->flags2 & TR2_RES_LITE)))
				get_quality(FALSE, RES_LITE, 0, a_idx);
			if ((choice == 7) && (!(a_ptr->flags2 & TR2_RES_DARK)))
				get_quality(FALSE, RES_DARK, 0, a_idx);
			if ((choice == 8) && (!(a_ptr->flags2 & TR2_RES_BLIND)))
				get_quality(FALSE, RES_BLIND, 0, a_idx);
			if ((choice == 9) && (!(a_ptr->flags2 & TR2_RES_CONFU)))
				get_quality(FALSE, RES_CONFU, 0, a_idx);
			if ((choice == 10) && (!(a_ptr->flags2 & TR2_RES_SOUND)))
				get_quality(FALSE, RES_SOUND, 0, a_idx);
			if ((choice == 11) && (!(a_ptr->flags2 & TR2_RES_SHARD)))
				get_quality(FALSE, RES_SHARD, 0, a_idx);
			if ((choice == 12) && (!(a_ptr->flags2 & TR2_RES_NEXUS)))
				get_quality(FALSE, RES_NEXUS, 0, a_idx);

			/* Often, collect a miscellanious quality, if it is affordable. */
			if (randint(2) == 1)
			{
				choice = randint(6);

				if ((choice == 1) && (!(a_ptr->flags3 & TR3_SLOW_DIGEST)))
					get_quality(FALSE, SLOW_DIGEST, 0, a_idx);
				if ((choice == 2) && (!(a_ptr->flags3 & TR3_FEATHER)))
					get_quality(FALSE, FEATHER, 0, a_idx);
				if ((choice == 3) && (!(a_ptr->flags3 & TR3_LITE)))
					get_quality(FALSE, LITE, 0, a_idx);
				if ((choice == 4) && (!(a_ptr->flags3 & TR3_REGEN)))
					get_quality(FALSE, REGEN, 0, a_idx);
				if ((choice == 5) && (!(a_ptr->flags3 & TR3_SEE_INVIS)))
					get_quality(FALSE, SEE_INVIS, 0, a_idx);
				if ((choice == 6) && (!(a_ptr->flags3 & TR3_FREE_ACT)))
					get_quality(FALSE, FREE_ACT, 0, a_idx);
			}

			/* Clean out the wallet. */
			if ((potential < 500) && (randint(5) == 1))
			{
				if (a_ptr->to_a > 0) a_ptr->to_a += potential / 100;
				potential = 0;
			}
		}
	}


	/* On sale free!  Last chance! */

	/* If an artifact effects a stat, it may also possibly sustain it. */
	choice = randint(6);

	if ((choice == 1) && (a_ptr->flags1 & TR1_STR))
		a_ptr->flags2 |= TR2_SUST_STR;
	else if ((choice == 2) && (a_ptr->flags1 & TR1_WIS))
		a_ptr->flags2 |= TR2_SUST_WIS;
	else if ((choice == 3) && (a_ptr->flags1 & TR1_INT))
		a_ptr->flags2 |= TR2_SUST_INT;
	else if ((choice == 4) && (a_ptr->flags1 & TR1_DEX))
		a_ptr->flags2 |= TR2_SUST_DEX;
	else if ((choice == 5) && (a_ptr->flags1 & TR1_CON))
		a_ptr->flags2 |= TR2_SUST_CON;
	else if ((choice == 6) && (a_ptr->flags1 & TR1_CHR))
		a_ptr->flags2 |= TR2_SUST_CHR;

	/* Armour gets a basic resist or low-level ability with 50%
	 * probability, and weapons with 25% probability.
	 */
	if (((a_ptr->tval >= TV_BOOTS) && (randint(2) == 1)) ||
		(randint(4) == 1))
	{
		choice = randint(6);
		if (choice == 1)
			a_ptr->flags2 |= TR2_RES_FIRE;
		else if (choice == 2)
			a_ptr->flags2 |= TR2_RES_COLD;
		else if (choice == 3)
			a_ptr->flags2 |= TR2_RES_ACID;
		else if (choice == 4)
			a_ptr->flags2 |= TR2_RES_ELEC;
		else if (choice == 5)
			a_ptr->flags3 |= TR3_SLOW_DIGEST;
		else if (choice == 6)
			a_ptr->flags3 |= TR3_FEATHER;
	}


	/* Frequently neaten bonuses to Armour Class, Skill, and Deadliness. */
	{
		if ((a_ptr->to_a % 5 == 4) && (randint(2) == 1)) a_ptr->to_a++;
		else if ((a_ptr->to_a % 5 == 1) && (randint(2) == 1)) a_ptr->to_a--;
		else if ((a_ptr->to_a % 2 == 1) && (randint(4) != 1)) a_ptr->to_a++;

		if ((a_ptr->to_h % 5 == 4) && (randint(2) == 1)) a_ptr->to_h++;
		else if ((a_ptr->to_h % 5 == 1) && (randint(2) == 1)) a_ptr->to_h--;
		else if ((a_ptr->to_h % 2 == 1) && (randint(4) != 1)) a_ptr->to_h++;

		if ((a_ptr->to_d % 5 == 4) && (randint(2) == 1)) a_ptr->to_d++;
		else if ((a_ptr->to_d % 5 == 1) && (randint(2) == 1)) a_ptr->to_d--;
		else if ((a_ptr->to_d % 2 == 1) && (randint(4) != 1)) a_ptr->to_d++;
	}
}



/*
 * Envoke perilous magics, and curse the artifact beyound redemption!  I
 * had such fun coding this...
 */
static void make_terrible(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];

	int i, gauntlet_runs, wheel_of_doom, penalty;

	bool heavy_curse = FALSE;
	bool aggravation = FALSE;


	/* Determine whether the artifact's magics are perilous enough to warrant
	 * a heavy curse or an aggravation.
	 */
	if (potential >= 3000) heavy_curse = TRUE;
	if ((potential >= 3500) && (randint(3) != 1)) aggravation = TRUE;


	/* Greatly decrease the chance for an activation. */
	if ((a_ptr->activation) && (randint(3) != 1)) a_ptr->activation = 0;


	/* Force the artifact though the gauntlet two or three times. */
	gauntlet_runs = 1 + randint(2);

	for (i = 0; i < gauntlet_runs; i++)
	{
		/* Choose a curse, biased towards penalties to_a, to_d, and to_h. */
		if ((a_ptr->tval < TV_BOOTS) && (a_ptr->to_h > 0) &&
			(randint(2) == 1)) wheel_of_doom = 2;
		else if ((a_ptr->to_a > 0) && (randint(2) == 1))  wheel_of_doom = 1;
		else wheel_of_doom = 2 + randint(2);

		penalty = 0;

		/* Blast base armour class or inflict a penalty to armour class. */
		if (wheel_of_doom == 1)
		{
			/* Blast armour and twist magics. */
			if ((a_ptr->ac) && (randint(6) == 1))
			{
				a_ptr->cost -= 500L * a_ptr->ac;
				a_ptr->ac = 0;
			}
			if ((a_ptr->tval >= TV_BOOTS) && (a_ptr->to_a >= 0))
			{
				a_ptr->to_a = -(5 + randint(7)) * 2;
			}

			/* Chance of a truly nasty effect for weapons. */
			else if ((randint(4) == 1) && (a_ptr->tval < TV_BOOTS))
			{
				penalty = randint(3) + 2;
				penalty *= 5;
				a_ptr->to_a = -penalty;

				a_ptr->cost -= penalty * 200L;
			}

			/* Artifact might very well still have some value. */
			if (a_ptr->to_a < 0)
				a_ptr->cost += a_ptr->to_a * 400L;
		}

		/* Make either the Skill or Deadliness bonus negative, or both. */
		if (wheel_of_doom == 2)
		{
			/* Weapons. */
			if (a_ptr->tval < TV_BOOTS)
			{
				/* Blast items with bonuses, Deadliness more rarely. */
				a_ptr->to_h = -(5 + randint(7)) * 2;
				if (randint(3) != 1) a_ptr->to_d = -(5 + randint(7)) * 2;
			}

			/* All armours. */
			else
			{
				/* Reverse any magics. */
				if (a_ptr->to_h > 0)
					a_ptr->to_h =  -a_ptr->to_h;
				if (a_ptr->to_d > 0)
					a_ptr->to_d = -a_ptr->to_d;

				/* Sometimes, blast even items without bonuses. */
				else if ((a_ptr->to_d == 0) && (a_ptr->to_h == 0) &&
					(randint (5) == 1))
				{
					penalty = randint(4) + 1;
					penalty *= 5;
					a_ptr->to_h -= penalty;
					a_ptr->to_d -= penalty;
				}
			}

			/* Artifact might very well still have some value. */
			if ((a_ptr->to_d < 0) && (a_ptr->to_h < 0))
			{
				a_ptr->cost -= 10000L;
				if (a_ptr->cost < 0) a_ptr->cost = 0;
			}
			else if ((a_ptr->to_d < 0) || (a_ptr->to_h < 0))
			{
				a_ptr->cost -= 5000L;
				if (a_ptr->cost < 0) a_ptr->cost = 0;
			}

		}

		/* Make any positive pval negative, or strip all pval_dependant
		 * values, add a random stat or three, and infict a large negative
		 * pval. */
		if (wheel_of_doom == 3)
		{
			if ((randint(3) != 1) && (a_ptr->pval > 0))
			{
				penalty = -(a_ptr->pval);
				a_ptr->pval = penalty;

				/* Artifact is highly unlikely to have any value. */
				a_ptr->cost -= 30000L;
				if (a_ptr->cost < 0) a_ptr->cost = 0;
			}

			else if (a_ptr->pval > 0)
			{
				a_ptr->flags1 &= ~(TR1_STR);
				a_ptr->flags1 &= ~(TR1_WIS);
				a_ptr->flags1 &= ~(TR1_INT);
				a_ptr->flags1 &= ~(TR1_DEX);
				a_ptr->flags1 &= ~(TR1_CON);
				a_ptr->flags1 &= ~(TR1_CHR);
				a_ptr->flags1 &= ~(TR1_INFRA);
				a_ptr->flags1 &= ~(TR1_STEALTH);
				a_ptr->flags1 &= ~(TR1_SPEED);
				a_ptr->flags1 &= ~(TR1_MAGIC_MASTERY);
				a_ptr->flags1 &= ~(TR1_SEARCH);
				a_ptr->flags1 &= ~(TR1_TUNNEL);

				if (randint(5) == 1)
				{
					a_ptr->flags1 |= (TR1_STR);
					a_ptr->flags1 |= (TR1_DEX);
					a_ptr->flags1 |= (TR1_CON);
				}
				else if (randint(4) == 1)
				{
					a_ptr->flags1 |= (TR1_WIS);
					a_ptr->flags1 |= (TR1_INT);
				}
				else if (randint(6) == 1)
				{
					a_ptr->flags1 |= (TR1_SPEED);
				}
				else
				{
					if (randint(4) == 1) a_ptr->flags1 |= (TR1_STR);
					if (randint(4) == 1) a_ptr->flags1 |= (TR1_DEX);
					if (randint(4) == 1) a_ptr->flags1 |= (TR1_CON);
					if (randint(4) == 1) a_ptr->flags1 |= (TR1_WIS);
					if (randint(4) == 1) a_ptr->flags1 |= (TR1_INT);
					if (randint(4) == 1) a_ptr->flags1 |= (TR1_CHR);
				}

				/* Iron Crown of Beruthiel, here we come... */
				penalty = -(randint(5)) - 3;
				if (randint(3) == 1) penalty *= 5;
				a_ptr->pval = penalty;


				/* Artifact is highly unlikely to have any value. */
				if (a_ptr->pval < 0) a_ptr->cost -= 60000L;
				if (a_ptr->cost < 0) a_ptr->cost = 0;
			}
		}

		/* Strip lots of non pval-dependant qualities. */
		if (wheel_of_doom == 4)
		{
			if (randint(4) == 1) a_ptr->flags1 &= ~(TR1_SHOTS);
			if (randint(4) == 1) a_ptr->flags1 &= ~(TR1_MIGHT2);
			if (randint(4) == 1) a_ptr->flags1 &= ~(TR1_MIGHT1);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_ANIMAL);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_EVIL);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_UNDEAD);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_DEMON);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_ORC);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_TROLL);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_GIANT);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_SLAY_DRAGON);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_PERFECT_BALANCE);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_BRAND_POIS);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_BRAND_ACID);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_BRAND_ELEC);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_BRAND_FIRE);
			if (randint(3) == 1) a_ptr->flags1 &= ~(TR1_BRAND_COLD);

			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_ACID);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_ELEC);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_FIRE);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_COLD);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_POIS);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_FEAR);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_LITE);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_DARK);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_BLIND);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_CONFU);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_SOUND);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_SHARD);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_NEXUS);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_NETHR);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_CHAOS);
			if (randint(3) == 1) a_ptr->flags2 &= ~(TR2_RES_DISEN);

			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_SLOW_DIGEST);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_FEATHER);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_LITE);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_REGEN);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_TELEPATHY);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_SEE_INVIS);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_FREE_ACT);
			if (randint(3) == 1) a_ptr->flags3 &= ~(TR3_HOLD_LIFE);

			/* Artifact will still have some value. */
			if (a_ptr->cost > 0) a_ptr->cost /= 3L;
		}
	}

	/* Boundary control. */
	if (a_ptr->cost < 0) a_ptr->cost = 0;

	/* Apply curses, agggravation, and teleportation. */
	a_ptr->flags3 |= TR3_LIGHT_CURSE;

	if (heavy_curse) a_ptr->flags3 |= TR3_HEAVY_CURSE;
	if (aggravation) a_ptr->flags3 |= TR3_AGGRAVATE;
	if (randint(6) == 6) a_ptr->flags3 |= TR3_TELEPORT;
}



/*
 * Clean up the artifact by removing illogical combinations of powers.
 * Adopted from Greg Wooledge's random artifact creation code.
 */
static void remove_contradictory(int a_idx)
{
	artifact_type *a_ptr = &a_info[a_idx];

	if (a_ptr->flags3 & TR3_AGGRAVATE) a_ptr->flags1 &= ~(TR1_STEALTH);
	if (a_ptr->flags2 & TR2_IM_ACID) a_ptr->flags2 &= ~(TR2_RES_ACID);
	if (a_ptr->flags2 & TR2_IM_ELEC) a_ptr->flags2 &= ~(TR2_RES_ELEC);
	if (a_ptr->flags2 & TR2_IM_FIRE) a_ptr->flags2 &= ~(TR2_RES_FIRE);
	if (a_ptr->flags2 & TR2_IM_COLD) a_ptr->flags2 &= ~(TR2_RES_COLD);
	if (a_ptr->pval < 0)
	{
		if (a_ptr->flags1 & TR1_STR) a_ptr->flags2 &= ~(TR2_SUST_STR);
		if (a_ptr->flags1 & TR1_INT) a_ptr->flags2 &= ~(TR2_SUST_INT);
		if (a_ptr->flags1 & TR1_WIS) a_ptr->flags2 &= ~(TR2_SUST_WIS);
		if (a_ptr->flags1 & TR1_DEX) a_ptr->flags2 &= ~(TR2_SUST_DEX);
		if (a_ptr->flags1 & TR1_CON) a_ptr->flags2 &= ~(TR2_SUST_CON);
		if (a_ptr->flags1 & TR1_CHR) a_ptr->flags2 &= ~(TR2_SUST_CHR);
	}
	if (a_ptr->flags3 & TR3_LIGHT_CURSE) a_ptr->flags3 &= ~(TR3_BLESSED);

	/* Oangband drain_exp is too nasty to lightly distribute. */
	/*if (a_ptr->flags3 & TR3_DRAIN_EXP) a_ptr->flags3 &= ~(TR3_HOLD_LIFE);*/
}



/*
 * String-handling function from Greg Wooledge's random artifact generator.
 */
static char *my_strdup (const char *s)
{
	char *t = malloc (strlen (s) + 1);
	if (t) strcpy (t, s);
	return t;
}


/*
 * Use W. Sheldon Simms' random name generator.  This function builds
 * probability tables which are used later on for letter selection.  It
 * relies on the ASCII character set.
 */
static void build_prob(void)
{
	int c_prev, c_cur, c_next;
	FILE *f;
	char buf [BUFLEN];

	/* Open the file containing our lexicon, and read from it.  Warn the
	 * rest of the code that random names are unavailable on failure.
	 */
	path_build (buf, BUFLEN, ANGBAND_DIR_FILE, NAMES_FILE);
	if ((f = my_fopen(buf, "r")) == NULL)
	{
		find_all_names = TRUE;
		return;
	}

	/* Build raw frequencies */
	while (1)
	{
		c_prev = c_cur = S_WORD;

		do
		{
			c_next = fgetc (f);
		} while (!isalpha (c_next) && (c_next != EOF));
		if (c_next == EOF) break;

		do
		{
			c_next = tolower (c_next) - 'a';	/* ASCII */
			lprobs[c_prev][c_cur][c_next]++;
			ltotal[c_prev][c_cur]++;
			c_prev = c_cur;
			c_cur = c_next;
			c_next = fgetc (f);
		} while (isalpha (c_next));

		lprobs [c_prev][c_cur][E_WORD]++;
		ltotal [c_prev][c_cur]++;
	}


	/* Close the file. */
	fclose(f);
}



/* Use W. Sheldon Simms' random name generator.  Generate a random word
 * using the probability tables we built earlier.  Relies on the ASCII
 * character set.  Relies on European vowels (a, e, i, o, u).  The generated
 * name should be copied/used before calling this function again.
 */
static char *make_word(void)
{
	static char word_buf [90];
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

	while (1)
	{
	    getletter:
		c_next = 0;
		r = rand_int (ltotal [c_prev][c_cur]);
		totalfreq = lprobs [c_prev][c_cur][c_next];
		while (totalfreq <= r)
		{
			c_next++;
			totalfreq += lprobs [c_prev][c_cur][c_next];
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

	word_buf[0] = toupper (word_buf[0]);
	return word_buf;
}


/*
 * Find a name from any of various text files.
 */
static char *find_word(int a_idx)
{
	static char art_name[81];
	artifact_type *a_ptr = &a_info[a_idx];
	art_name[0] = '\0';

	/* Select a file, depending on whether the artifact is a weapon or
	 * armour, and whether or not it is cursed.  Get a random line from
	 * that file.
	 */
	if (a_ptr->flags3 & TR3_LIGHT_CURSE)
	{
		if ((a_ptr->tval == TV_BOW) || (a_ptr->tval == TV_SWORD) ||
			(a_ptr->tval == TV_HAFTED) || (a_ptr->tval == TV_POLEARM))
			get_rnd_line("w_cursed.txt", art_name);
		else get_rnd_line("a_cursed.txt", art_name);
	}
	else
	{
		if ((a_ptr->tval == TV_BOW) || (a_ptr->tval == TV_SWORD) ||
			(a_ptr->tval == TV_HAFTED) || (a_ptr->tval == TV_POLEARM))
			get_rnd_line("w_normal.txt", art_name);

		else get_rnd_line("a_normal.txt", art_name);
	}


	/* Secretly call the make_word function in case of failure.  If it is
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
		else if (randint(3) == 1)
			strcpy(art_name, format("'%s'", make_word()));
		else
			strcpy(art_name, format("of %s", make_word()));

	}

	/* Return the name. */
	return art_name;
}


/*
 * Name an artifact, using one of two methods.
 */
static void name_artifact(int a_idx)
{
	char *word;
	char buf [BUFLEN];


	/* Use W. Sheldon Simms' random name generator most of the time,
	 * generally for the less powerful artifacts, if not out of commision.
	 * Otherwise, find a name from a text file.
	 */
	if (((randint(max_potential) > initial_potential) || (randint(2) == 1)) &&
		(find_all_names == FALSE))
	{
		word = make_word();

		if (rand_int(3) == 0)
			sprintf(buf, "'%s'", word);
		else
			sprintf(buf, "of %s", word);
	}
	else
	{
		word = find_word(a_idx);

		sprintf(buf, "%s", word);
	}


	/* Insert whatever name is created or found into the temporary array. */
	names[a_idx] = my_strdup(buf);
}


/*
 * Design a random artifact.
 */
static void design_random_artifact(int a_idx)
{
	/* Initialize the artifact, and assign it a potential. */
	initialize_artifact(a_idx);

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
 * Fill in the temporary array of artifact names, and then convert it into
 * an a_name structure.  Adapted from Greg Wooledge's random artifacts.
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
	name_size = 1;
	for (i = 0; i < z_info->a_max; i++)
	{
		name_size += strlen (names[i]) + 1;	/* skip first char */
	}
	if ((a_base = ralloc(name_size)) == NULL)
	{
		msg_format("Memory allocation error");
		return 1;
	}

	a_next = a_base + 1;	/* skip first char */
	for (i = 0; i < z_info->a_max; i++)
	{
		strcpy(a_next, names[i]);
		if (a_info[i].tval > 0)		/* skip unused! */
			a_info[i].name = a_next - a_base;
		a_next += strlen(names[i]) + 1;
	}


	/* Free some of our now unneeded memory. */
	KILL (a_name);
	for (i = ART_MIN_RANDOM; i < z_info->a_max; i++)
	{
		free(names[i]);
	}
	a_name = a_base;

	return 0;
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


	/* Go from beginning to end of the random section of the
	 * artifact array, initializing and naming as we go.
	 */
	for (a_idx = ART_MIN_RANDOM; a_idx < z_info->a_max; a_idx++)
	{
		/* Design the artifact, storing information as we go along. */
		design_random_artifact(a_idx);
	}


	/* The new names we created while artifacts were being rolled up need to
	 * be added to the a_name structure.
	 */
	err = convert_names();

	/* Complain if naming fails. */
	if (err) msg_print("Warning - random artifact naming failed!");
}
