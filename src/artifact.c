/* File: artifact.c */

/* Purpose: Artifact code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/* Chance of using syllables to form the name instead of the "template" files */
#define TABLE_NAME      45
#define A_CURSED        13
#define WEIRD_LUCK      12
#define BIAS_LUCK       20

/*
 * Bias luck needs to be higher than weird luck,
 * since it is usually tested several times...
 */
#define ACTIVATION_CHANCE 3


static void curse_artifact(object_type *o_ptr)
{
	if (o_ptr->pval) o_ptr->pval = 0 - (o_ptr->pval + randint1(4));
	if (o_ptr->to_a) o_ptr->to_a = 0 - (o_ptr->to_a + randint1(4));
	if (o_ptr->to_h) o_ptr->to_h = 0 - (o_ptr->to_h + randint1(4));
	if (o_ptr->to_d) o_ptr->to_d = 0 - (o_ptr->to_d + randint1(4));

	o_ptr->flags3 |= (TR3_HEAVY_CURSE | TR3_CURSED);

	/* if (one_in_(4)) o_ptr->flags3 |= TR3_PERMA_CURSE; */
	if (one_in_(3)) o_ptr->flags3 |= TR3_TY_CURSE;
	if (one_in_(2)) o_ptr->flags3 |= TR3_AGGRAVATE;
	if (one_in_(3)) o_ptr->flags3 |= TR3_DRAIN_EXP;
	if (one_in_(2)) o_ptr->flags3 |= TR3_TELEPORT;
	else if (one_in_(3)) o_ptr->flags3 |= TR3_NO_TELE;

	if ((p_ptr->pclass != CLASS_WARRIOR) && one_in_(3))
		o_ptr->flags3 |= TR3_NO_MAGIC;
}


static int random_plus(object_type *o_ptr, int artifact_bias)
{
	switch (artifact_bias)
	{
		case BIAS_WARRIOR:
			if (!(o_ptr->flags1 & TR1_STR))
			{
				o_ptr->flags1 |= TR1_STR;
				if (one_in_(2)) return (artifact_bias);
			}

			if (!(o_ptr->flags1 & TR1_CON))
			{
				o_ptr->flags1 |= TR1_CON;
				if (one_in_(2)) return (artifact_bias);
			}

			if (!(o_ptr->flags1 & TR1_DEX))
			{
				o_ptr->flags1 |= TR1_DEX;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_MAGE:
			if (!(o_ptr->flags1 & TR1_INT))
			{
				o_ptr->flags1 |= TR1_INT;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_PRIESTLY:
			if (!(o_ptr->flags1 & TR1_WIS))
			{
				o_ptr->flags1 |= TR1_WIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_RANGER:
			if (!(o_ptr->flags1 & TR1_CON))
			{
				o_ptr->flags1 |= TR1_CON;
				if (one_in_(2)) return (artifact_bias);
			}

			if (!(o_ptr->flags1 & TR1_DEX))
			{
				o_ptr->flags1 |= TR1_DEX;
				if (one_in_(2)) return (artifact_bias);
			}

			if (!(o_ptr->flags1 & TR1_STR))
			{
				o_ptr->flags1 |= TR1_STR;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_ROGUE:
			if (!(o_ptr->flags1 & TR1_STEALTH))
			{
				o_ptr->flags1 |= TR1_STEALTH;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags1 & TR1_SEARCH))
			{
				o_ptr->flags1 |= TR1_SEARCH;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_STR:
			if (!(o_ptr->flags1 & TR1_STR))
			{
				o_ptr->flags1 |= TR1_STR;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_WIS:
			if (!(o_ptr->flags1 & TR1_WIS))
			{
				o_ptr->flags1 |= TR1_WIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_INT:
			if (!(o_ptr->flags1 & TR1_INT))
			{
				o_ptr->flags1 |= TR1_INT;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_DEX:
			if (!(o_ptr->flags1 & TR1_DEX))
			{
				o_ptr->flags1 |= TR1_DEX;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_CON:
			if (!(o_ptr->flags1 & TR1_CON))
			{
				o_ptr->flags1 |= TR1_CON;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_CHR:
			if (!(o_ptr->flags1 & TR1_CHR))
			{
				o_ptr->flags1 |= TR1_CHR;
				if (one_in_(2)) return (artifact_bias);
			}
			break;
	}


	switch (randint1(o_ptr->tval < TV_BOOTS ? 23 : 19))
	{
		case 1:  case 2:
			o_ptr->flags1 |= TR1_STR;

			if (!artifact_bias && !one_in_(13))
				artifact_bias = BIAS_STR;
			else if (!artifact_bias && one_in_(7))
				artifact_bias = BIAS_WARRIOR;
			break;
		case 3:  case 4:
			o_ptr->flags1 |= TR1_INT;

			if (!artifact_bias && !one_in_(13))
				artifact_bias = BIAS_INT;
			else if (!artifact_bias && one_in_(7))
				artifact_bias = BIAS_MAGE;
			break;
		case 5:  case 6:
			o_ptr->flags1 |= TR1_WIS;

			if (!artifact_bias && !one_in_(13))
				artifact_bias = BIAS_WIS;
			else if (!artifact_bias && one_in_(7))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 7:  case 8:
			o_ptr->flags1 |= TR1_DEX;

			if (!artifact_bias && !one_in_(13))
				artifact_bias = BIAS_DEX;
			else if (!artifact_bias && one_in_(7))
				artifact_bias = BIAS_ROGUE;
			break;
		case 9:  case 10:
			o_ptr->flags1 |= TR1_CON;

			if (!artifact_bias && !one_in_(13))
				artifact_bias = BIAS_CON;
			else if (!artifact_bias && one_in_(9))
				artifact_bias = BIAS_RANGER;
			break;
		case 11:  case 12:
			o_ptr->flags1 |= TR1_CHR;

			if (!artifact_bias && !one_in_(13))
				artifact_bias = BIAS_CHR;
			break;
		case 13:  case 14:
			o_ptr->flags1 |= TR1_STEALTH;

			if (!artifact_bias && one_in_(3))
				artifact_bias = BIAS_ROGUE;
			break;
		case 15:  case 16:
			o_ptr->flags1 |= TR1_SEARCH;

			if (!artifact_bias && one_in_(9))
				artifact_bias = BIAS_RANGER;
			break;
		case 17:  case 18:
			o_ptr->flags1 |= TR1_INFRA;

			break;
		case 19:
			o_ptr->flags1 |= TR1_SPEED;

			if (!artifact_bias && one_in_(11))
				artifact_bias = BIAS_ROGUE;
			break;
		case 20:  case 21:
			o_ptr->flags1 |= TR1_TUNNEL;

			break;
		case 22:  case 23:
			if (o_ptr->tval == TV_BOW)
			{
				o_ptr->flags1 |= TR1_DEX;
			}
			else
			{
				o_ptr->flags1 |= TR1_BLOWS;

				if (!artifact_bias && one_in_(11))
					artifact_bias = BIAS_WARRIOR;
			}
			break;
	}

	return (artifact_bias);
}


int random_resistance(object_type *o_ptr, int specific, int artifact_bias)
{
	/* Paranoia */
	if (specific && artifact_bias)
	{
		msg_format("Invalid call to 'random_resistance', %d, %d",
				   specific, artifact_bias);

		/* Bail out */
		return (0);
	}

	switch (artifact_bias)
	{
		case BIAS_ACID:
			if (!(o_ptr->flags2 & TR2_RES_ACID))
			{
				o_ptr->flags2 |= TR2_RES_ACID;
				if (one_in_(2)) return (artifact_bias);
			}
			if (one_in_(BIAS_LUCK) && !(o_ptr->flags2 & TR2_IM_ACID))
			{
				o_ptr->flags2 |= TR2_IM_ACID;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_ELEC:
			if (!(o_ptr->flags2 & TR2_RES_ELEC))
			{
				o_ptr->flags2 |= TR2_RES_ELEC;
				if (one_in_(2)) return (artifact_bias);
			}
			if ((o_ptr->tval >= TV_CLOAK) && (o_ptr->tval <= TV_HARD_ARMOR) &&
				!(o_ptr->flags3 & TR3_SH_ELEC))
			{
				o_ptr->flags3 |= TR3_SH_ELEC;
				if (one_in_(2)) return (artifact_bias);
			}
			if (one_in_(BIAS_LUCK) && !(o_ptr->flags2 & TR2_IM_ELEC))
			{
				o_ptr->flags2 |= TR2_IM_ELEC;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_FIRE:
			if (!(o_ptr->flags2 & TR2_RES_FIRE))
			{
				o_ptr->flags2 |= TR2_RES_FIRE;
				if (one_in_(2)) return (artifact_bias);
			}
			if ((o_ptr->tval >= TV_CLOAK) &&
				(o_ptr->tval <= TV_HARD_ARMOR) &&
				!(o_ptr->flags3 & TR3_SH_FIRE))
			{
				o_ptr->flags3 |= TR3_SH_FIRE;
				if (one_in_(2)) return (artifact_bias);
			}
			if (one_in_(BIAS_LUCK) && !(o_ptr->flags2 & TR2_IM_FIRE))
			{
				o_ptr->flags2 |= TR2_IM_FIRE;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_COLD:
			if (!(o_ptr->flags2 & TR2_RES_COLD))
			{
				o_ptr->flags2 |= TR2_RES_COLD;
				if (one_in_(2)) return (artifact_bias);
			}
			if (one_in_(BIAS_LUCK) && !(o_ptr->flags2 & TR2_IM_COLD))
			{
				o_ptr->flags2 |= TR2_IM_COLD;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_POIS:
			if (!(o_ptr->flags2 & TR2_RES_POIS))
			{
				o_ptr->flags2 |= TR2_RES_POIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_WARRIOR:
			if (!one_in_(3) && (!(o_ptr->flags2 & TR2_RES_FEAR)))
			{
				o_ptr->flags2 |= TR2_RES_FEAR;
				if (one_in_(2)) return (artifact_bias);
			}
			if (one_in_(3) && (!(o_ptr->flags3 & TR3_NO_MAGIC)))
			{
				o_ptr->flags3 |= TR3_NO_MAGIC;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_NECROMANTIC:
			if (!(o_ptr->flags2 & TR2_RES_NETHER))
			{
				o_ptr->flags2 |= TR2_RES_NETHER;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags2 & TR2_RES_POIS))
			{
				o_ptr->flags2 |= TR2_RES_POIS;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags2 & TR2_RES_DARK))
			{
				o_ptr->flags2 |= TR2_RES_DARK;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_CHAOS:
			if (!(o_ptr->flags2 & TR2_RES_CHAOS))
			{
				o_ptr->flags2 |= TR2_RES_CHAOS;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags2 & TR2_RES_CONF))
			{
				o_ptr->flags2 |= TR2_RES_CONF;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags2 & TR2_RES_DISEN))
			{
				o_ptr->flags2 |= TR2_RES_DISEN;
				if (one_in_(2)) return (artifact_bias);
			}
			break;
	}

	switch (specific ? specific : randint1(41))
	{
		case 1:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_ACID;
			else
			{
				o_ptr->flags2 |= TR2_IM_ACID;

				if (!artifact_bias)
					artifact_bias = BIAS_ACID;
			}
			break;
		case 2:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_ELEC;
			else
			{
				o_ptr->flags2 |= TR2_IM_ELEC;

				if (!artifact_bias)
					artifact_bias = BIAS_ELEC;
			}
			break;
		case 3:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_COLD;
			else
			{
				o_ptr->flags2 |= TR2_IM_COLD;

				if (!artifact_bias)
					artifact_bias = BIAS_COLD;
			}
			break;
		case 4:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_FIRE;
			else
			{
				o_ptr->flags2 |= TR2_IM_FIRE;

				if (!artifact_bias)
					artifact_bias = BIAS_FIRE;
			}
			break;
		case 5:
		case 6:
		case 13:
			o_ptr->flags2 |= TR2_RES_ACID;

			if (!artifact_bias)
				artifact_bias = BIAS_ACID;
			break;
		case 7:
		case 8:
		case 14:
			o_ptr->flags2 |= TR2_RES_ELEC;

			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 9:
		case 10:
		case 15:
			o_ptr->flags2 |= TR2_RES_FIRE;

			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 11:
		case 12:
		case 16:
			o_ptr->flags2 |= TR2_RES_COLD;

			if (!artifact_bias)
				artifact_bias = BIAS_COLD;
			break;
		case 17:
		case 18:
			o_ptr->flags2 |= TR2_RES_POIS;

			if (!artifact_bias && !one_in_(4))
				artifact_bias = BIAS_POIS;
			else if (!artifact_bias && one_in_(2))
				artifact_bias = BIAS_NECROMANTIC;
			else if (!artifact_bias && one_in_(2))
				artifact_bias = BIAS_ROGUE;
			break;
		case 19:
		case 20:
			o_ptr->flags2 |= TR2_RES_FEAR;

			if (!artifact_bias && one_in_(3))
				artifact_bias = BIAS_WARRIOR;
			break;
		case 21:
			o_ptr->flags2 |= TR2_RES_LITE;

			break;
		case 22:
			o_ptr->flags2 |= TR2_RES_DARK;

			break;
		case 23:
		case 24:
			o_ptr->flags2 |= TR2_RES_BLIND;

			break;
		case 25:
		case 26:
			o_ptr->flags2 |= TR2_RES_CONF;

			if (!artifact_bias && one_in_(6))
				artifact_bias = BIAS_CHAOS;
			break;
		case 27:
		case 28:
			o_ptr->flags2 |= TR2_RES_SOUND;

			break;
		case 29:
		case 30:
			o_ptr->flags2 |= TR2_RES_SHARDS;

			break;
		case 31:
		case 32:
			o_ptr->flags2 |= TR2_RES_NETHER;

			if (!artifact_bias && one_in_(3))
				artifact_bias = BIAS_NECROMANTIC;
			break;
		case 33:
		case 34:
			o_ptr->flags2 |= TR2_RES_NEXUS;

			break;
		case 35:
		case 36:
			o_ptr->flags2 |= TR2_RES_CHAOS;

			if (!artifact_bias && one_in_(2))
				artifact_bias = BIAS_CHAOS;
			break;
		case 37:
		case 38:
			o_ptr->flags2 |= TR2_RES_DISEN;

			break;
		case 39:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->flags3 |= TR3_SH_ELEC;
			else
				o_ptr->flags2 |= TR2_RES_ELEC;
			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 40:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->flags3 |= TR3_SH_FIRE;
			else
				o_ptr->flags2 |= TR2_RES_FIRE;
			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 41:
			if (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK ||
				o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR)
				o_ptr->flags2 |= TR2_REFLECT;
			else
				o_ptr->flags2 |= TR2_RES_FEAR;
			break;
	}

	return (artifact_bias);
}



static int random_misc(object_type *o_ptr, int artifact_bias)
{
	switch (artifact_bias)
	{
		case BIAS_RANGER:
			if (!(o_ptr->flags2 & TR2_SUST_CON))
			{
				o_ptr->flags2 |= TR2_SUST_CON;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_STR:
			if (!(o_ptr->flags2 & TR2_SUST_STR))
			{
				o_ptr->flags2 |= TR2_SUST_STR;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_WIS:
			if (!(o_ptr->flags2 & TR2_SUST_WIS))
			{
				o_ptr->flags2 |= TR2_SUST_WIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_INT:
			if (!(o_ptr->flags2 & TR2_SUST_INT))
			{
				o_ptr->flags2 |= TR2_SUST_INT;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_DEX:
			if (!(o_ptr->flags2 & TR2_SUST_DEX))
			{
				o_ptr->flags2 |= TR2_SUST_DEX;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_CON:
			if (!(o_ptr->flags2 & TR2_SUST_CON))
			{
				o_ptr->flags2 |= TR2_SUST_CON;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_CHR:
			if (!(o_ptr->flags2 & TR2_SUST_CHR))
			{
				o_ptr->flags2 |= TR2_SUST_CHR;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_CHAOS:
			if (!(o_ptr->flags3 & TR3_TELEPORT))
			{
				o_ptr->flags3 |= TR3_TELEPORT;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_FIRE:
			if (!(o_ptr->flags3 & TR3_LITE))
			{
				o_ptr->flags3 |= TR3_LITE;	/* Freebie */
			}
			break;
	}

	switch (randint1(31))
	{
		case 1:
			o_ptr->flags2 |= TR2_SUST_STR;

			if (!artifact_bias)
				artifact_bias = BIAS_STR;
			break;
		case 2:
			o_ptr->flags2 |= TR2_SUST_INT;

			if (!artifact_bias)
				artifact_bias = BIAS_INT;
			break;
		case 3:
			o_ptr->flags2 |= TR2_SUST_WIS;

			if (!artifact_bias)
				artifact_bias = BIAS_WIS;
			break;
		case 4:
			o_ptr->flags2 |= TR2_SUST_DEX;

			if (!artifact_bias)
				artifact_bias = BIAS_DEX;
			break;
		case 5:
			o_ptr->flags2 |= TR2_SUST_CON;

			if (!artifact_bias)
				artifact_bias = BIAS_CON;
			break;
		case 6:
			o_ptr->flags2 |= TR2_SUST_CHR;

			if (!artifact_bias)
				artifact_bias = BIAS_CHR;
			break;
		case 7:
		case 8:
		case 14:
			o_ptr->flags2 |= TR2_FREE_ACT;

			break;
		case 9:
			o_ptr->flags2 |= TR2_HOLD_LIFE;

			if (!artifact_bias && one_in_(5))
				artifact_bias = BIAS_PRIESTLY;
			else if (!artifact_bias && one_in_(6))
				artifact_bias = BIAS_NECROMANTIC;
			break;
		case 10:
		case 11:
			o_ptr->flags3 |= TR3_LITE;

			break;
		case 12:
		case 13:
			o_ptr->flags3 |= TR3_FEATHER;

			break;
		case 15:
		case 16:
		case 17:
			o_ptr->flags3 |= TR3_SEE_INVIS;

			break;
		case 18:
			o_ptr->flags3 |= TR3_TELEPATHY;

			if (!artifact_bias && one_in_(9))
				artifact_bias = BIAS_MAGE;
			break;
		case 19:
		case 20:
			o_ptr->flags3 |= TR3_SLOW_DIGEST;

			break;
		case 21:
		case 22:
			o_ptr->flags3 |= TR3_REGEN;

			break;
		case 23:
			o_ptr->flags3 |= TR3_TELEPORT;

			break;
		case 24:
		case 25:
		case 26:
			if (o_ptr->tval >= TV_BOOTS)
			{
				o_ptr->flags1 |= TR1_STEALTH;
			}
			else
			{
				o_ptr->flags3 |= TR3_SHOW_MODS;
				o_ptr->to_a = (s16b)rand_range(5, 15);
			}
			break;
		case 27:
		case 28:
		case 29:
			o_ptr->flags3 |= TR3_SHOW_MODS;
			o_ptr->to_h += (s16b)rand_range(5, 15);
			o_ptr->to_d += (s16b)rand_range(5, 15);
			break;
		case 30:
			o_ptr->flags3 |= TR3_NO_MAGIC;
			break;
		case 31:
			o_ptr->flags3 |= TR3_NO_TELE;
			break;
	}

	return (artifact_bias);
}


static int random_slay(object_type *o_ptr, int artifact_bias)
{
	/* Bows get special treatment */
	if (o_ptr->tval == TV_BOW)
	{
		switch (randint1(6))
		{
			case 1:
			case 2:
			case 3:
				o_ptr->flags3 |= TR3_XTRA_MIGHT;

				if (!artifact_bias && one_in_(9))
					artifact_bias = BIAS_RANGER;
				break;
			default:
				o_ptr->flags3 |= TR3_XTRA_SHOTS;

				if (!artifact_bias && one_in_(9))
					artifact_bias = BIAS_RANGER;
				break;
		}

		return (artifact_bias);
	}


	switch (artifact_bias)
	{
		case BIAS_CHAOS:
			if (!(o_ptr->flags1 & TR1_CHAOTIC))
			{
				o_ptr->flags1 |= TR1_CHAOTIC;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_PRIESTLY:
			if ((o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM) &&
				!(o_ptr->flags3 & TR3_BLESSED))
			{
				/* A free power for "priestly" random artifacts */
				o_ptr->flags3 |= TR3_BLESSED;
			}
			break;

		case BIAS_NECROMANTIC:
			if (!(o_ptr->flags1 & TR1_VAMPIRIC))
			{
				o_ptr->flags1 |= TR1_VAMPIRIC;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags1 & TR1_BRAND_POIS) && one_in_(2))
			{
				o_ptr->flags1 |= TR1_BRAND_POIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_RANGER:
			if (!(o_ptr->flags1 & TR1_SLAY_ANIMAL))
			{
				o_ptr->flags1 |= TR1_SLAY_ANIMAL;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_ROGUE:
			if ((((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DAGGER)) ||
				 ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_SPEAR)) ||
				 ((o_ptr->tval == TV_POLEARM) &&
				  (o_ptr->sval == SV_HATCHET))) && !(o_ptr->flags2 & TR2_THROW))
			{
				/* Free power for rogues... */
				o_ptr->flags2 |= TR2_THROW;
			}
			if ((!(o_ptr->flags1 & TR1_BRAND_POIS)) && one_in_(2))
			{
				o_ptr->flags1 |= TR1_BRAND_POIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_POIS:
			if (!(o_ptr->flags1 & TR1_BRAND_POIS))
			{
				o_ptr->flags1 |= TR1_BRAND_POIS;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_FIRE:
			if (!(o_ptr->flags1 & TR1_BRAND_FIRE))
			{
				o_ptr->flags1 |= TR1_BRAND_FIRE;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_COLD:
			if (!(o_ptr->flags1 & TR1_BRAND_COLD))
			{
				o_ptr->flags1 |= TR1_BRAND_COLD;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_ELEC:
			if (!(o_ptr->flags1 & TR1_BRAND_ELEC))
			{
				o_ptr->flags1 |= TR1_BRAND_ELEC;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_ACID:
			if (!(o_ptr->flags1 & TR1_BRAND_ACID))
			{
				o_ptr->flags1 |= TR1_BRAND_ACID;
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_LAW:

			if (!(o_ptr->flags1 & TR1_SLAY_EVIL))
			{
				o_ptr->flags1 |= TR1_SLAY_EVIL;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags1 & TR1_SLAY_UNDEAD))
			{
				o_ptr->flags1 |= TR1_SLAY_UNDEAD;
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags1 & TR1_SLAY_DEMON))
			{
				o_ptr->flags1 |= TR1_SLAY_DEMON;
				if (one_in_(2)) return (artifact_bias);
			}
			break;
	}

	switch (randint1(34))
	{
		case 1:
		case 2:
			o_ptr->flags1 |= TR1_SLAY_ANIMAL;

			break;
		case 3:
		case 4:
			o_ptr->flags1 |= TR1_SLAY_EVIL;

			if (!artifact_bias && one_in_(2))
				artifact_bias = BIAS_LAW;
			else if (!artifact_bias && one_in_(9))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 5:
		case 6:
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;

			if (!artifact_bias && one_in_(9))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 7:
		case 8:
			o_ptr->flags1 |= TR1_SLAY_DEMON;

			if (!artifact_bias && one_in_(9))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 9:
		case 10:
			o_ptr->flags1 |= TR1_SLAY_ORC;

			break;
		case 11:
		case 12:
			o_ptr->flags1 |= TR1_SLAY_TROLL;

			break;
		case 13:
		case 14:
			o_ptr->flags1 |= TR1_SLAY_GIANT;

			break;
		case 15:
		case 16:
			o_ptr->flags1 |= TR1_SLAY_DRAGON;

			break;
		case 17:
			o_ptr->flags1 |= TR1_KILL_DRAGON;

			break;
		case 18:
		case 19:
			if (o_ptr->tval == TV_SWORD)
			{
				o_ptr->flags1 |= TR1_VORPAL;

				if (!artifact_bias && one_in_(9))
					artifact_bias = BIAS_WARRIOR;
			}
			else
			{
				o_ptr->flags1 |= TR1_IMPACT;
			}
			break;
		case 20:
		case 21:
		case 22:
			o_ptr->flags1 |= TR1_BRAND_FIRE;

			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 23:
		case 24:
			o_ptr->flags1 |= TR1_BRAND_COLD;

			if (!artifact_bias)
				artifact_bias = BIAS_COLD;
			break;
		case 25:
		case 26:
			o_ptr->flags1 |= TR1_BRAND_ELEC;

			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 27:
		case 28:
			o_ptr->flags1 |= TR1_BRAND_ACID;

			if (!artifact_bias)
				artifact_bias = BIAS_ACID;
			break;
		case 29:
		case 30:
			o_ptr->flags1 |= TR1_BRAND_POIS;

			if (!artifact_bias && !one_in_(3))
				artifact_bias = BIAS_POIS;
			else if (!artifact_bias && one_in_(6))
				artifact_bias = BIAS_NECROMANTIC;
			else if (!artifact_bias)
				artifact_bias = BIAS_ROGUE;
			break;
		case 31:
		case 32:
			o_ptr->flags1 |= TR1_VAMPIRIC;

			if (!artifact_bias)
				artifact_bias = BIAS_NECROMANTIC;
			break;
		default:
			o_ptr->flags1 |= TR1_CHAOTIC;

			if (!artifact_bias)
				artifact_bias = BIAS_CHAOS;
			break;
	}

	return (artifact_bias);
}


static void give_activation_power(object_type *o_ptr, int artifact_bias)
{
	int type = 0, chance = 0;

	switch (artifact_bias)
	{
		case BIAS_ELEC:
			chance = 101;
			if (!one_in_(3))
				type = ACT_BO_ELEC_1;
			else if (!one_in_(5))
				type = ACT_BA_ELEC_2;
			else
				type = ACT_BA_ELEC_3;
			break;

		case BIAS_POIS:
			chance = 101;
			type = ACT_BA_POIS_1;
			break;

		case BIAS_FIRE:
			chance = 101;
			if (!one_in_(3))
				type = ACT_BO_FIRE_1;
			else if (!one_in_(5))
				type = ACT_BA_FIRE_1;
			else
				type = ACT_BA_FIRE_2;
			break;

		case BIAS_COLD:
			chance = 101;
			if (!one_in_(3))
				type = ACT_BO_COLD_1;
			else if (!one_in_(3))
				type = ACT_BA_COLD_1;
			else if (!one_in_(3))
				type = ACT_BA_COLD_2;
			else
				type = ACT_BA_COLD_3;
			break;

		case BIAS_CHAOS:
			chance = 50;
			if (one_in_(6))
				type = ACT_SUMMON_DEMON;
			else
				type = ACT_CALL_CHAOS;
			break;

		case BIAS_PRIESTLY:
			chance = 101;

			switch (randint1(13))
			{
				case 13:
					type = ACT_CHARM_UNDEAD;
					break;
				case 12:
					type = ACT_BANISH_EVIL;
					break;
				case 11:
					type = ACT_DISP_EVIL;
					break;
				case 10:
					type = ACT_PROT_EVIL;
					break;
				case 9:
					type = ACT_CURE_1000;
					break;
				case 8:
					type = ACT_CURE_700;
					break;
				case 7:
					type = ACT_REST_ALL;
					break;
				case 6:
					type = ACT_REST_LIFE;
					break;
				default:
					type = ACT_CURE_MW;
					break;
			}
			break;

		case BIAS_NECROMANTIC:
			chance = 101;
			if (randint1(66))
				type = ACT_WRAITH;
			else if (one_in_(13))
				type = ACT_DISP_GOOD;
			else if (one_in_(9))
				type = ACT_MASS_GENO;
			else if (one_in_(8))
				type = ACT_GENOCIDE;
			else if (one_in_(13))
				type = ACT_SUMMON_UNDEAD;
			else if (one_in_(9))
				type = ACT_VAMPIRE_2;
			else if (one_in_(6))
				type = ACT_CHARM_UNDEAD;
			else
				type = ACT_VAMPIRE_1;
			break;

		case BIAS_LAW:
			chance = 101;
			if (one_in_(8))
				type = ACT_BANISH_EVIL;
			else if (one_in_(4))
				type = ACT_DISP_EVIL;
			else
				type = ACT_PROT_EVIL;
			break;

		case BIAS_ROGUE:
			chance = 101;
			if (one_in_(50))
				type = ACT_SPEED;
			else if (one_in_(4))
				type = ACT_SLEEP;
			else if (one_in_(3))
				type = ACT_DETECT_ALL;
			else if (one_in_(8))
				type = ACT_ID_FULL;
			else
				type = ACT_ID_PLAIN;
			break;

		case BIAS_MAGE:
			chance = 66;
			if (one_in_(20))
				type = SUMMON_ELEMENTAL;
			else if (one_in_(10))
				type = SUMMON_PHANTOM;
			else if (one_in_(5))
				type = ACT_RUNE_EXPLO;
			else
				type = ACT_ESP;
			break;

		case BIAS_WARRIOR:
			chance = 80;
			if (one_in_(100))
				type = ACT_INVULN;
			else
				type = ACT_BERSERK;
			break;

		case BIAS_RANGER:
			chance = 101;
			if (one_in_(20))
				type = ACT_CHARM_ANIMALS;
			else if (one_in_(7))
				type = ACT_SUMMON_ANIMAL;
			else if (one_in_(6))
				type = ACT_CHARM_ANIMAL;
			else if (one_in_(4))
				type = ACT_RESIST_ALL;
			else if (one_in_(3))
				type = ACT_SATIATE;
			else
				type = ACT_CURE_POISON;
			break;
	}

	while (!type || (randint1(100) >= chance))
	{
		type = randint1(255);
		switch (type)
		{
			case ACT_SUNLIGHT:
			case ACT_BO_MISS_1:
			case ACT_BA_POIS_1:
			case ACT_BO_ELEC_1:
			case ACT_BO_ACID_1:
			case ACT_BO_COLD_1:
			case ACT_BO_FIRE_1:
			case ACT_CONFUSE:
			case ACT_SLEEP:
			case ACT_QUAKE:
			case ACT_CURE_LW:
			case ACT_CURE_MW:
			case ACT_CURE_POISON:
			case ACT_BERSERK:
			case ACT_LIGHT:
			case ACT_MAP_LIGHT:
			case ACT_DEST_DOOR:
			case ACT_STONE_MUD:
			case ACT_TELEPORT_1:
			case ACT_TELEPORT_2:
				chance = 101;
				break;
			case ACT_BA_COLD_1:
			case ACT_BA_FIRE_1:
			case ACT_DRAIN_1:
			case ACT_TELE_AWAY:
			case ACT_ESP:
			case ACT_RESIST_ALL:
			case ACT_DETECT_ALL:
			case ACT_RECALL:
			case ACT_SATIATE:
			case ACT_RECHARGE:
				chance = 85;
				break;
			case ACT_TERROR:
			case ACT_PROT_EVIL:
			case ACT_ID_PLAIN:
				chance = 75;
				break;
			case ACT_DRAIN_2:
			case ACT_VAMPIRE_1:
			case ACT_BO_MISS_2:
			case ACT_BA_FIRE_2:
			case ACT_REST_LIFE:
				chance = 66;
				break;
			case ACT_BA_COLD_3:
			case ACT_BA_ELEC_3:
			case ACT_WHIRLWIND:
			case ACT_VAMPIRE_2:
			case ACT_CHARM_ANIMAL:
				chance = 50;
				break;
			case ACT_SUMMON_ANIMAL:
				chance = 40;
				break;
			case ACT_DISP_EVIL:
			case ACT_BA_MISS_3:
			case ACT_DISP_GOOD:
			case ACT_BANISH_EVIL:
			case ACT_GENOCIDE:
			case ACT_MASS_GENO:
			case ACT_CHARM_UNDEAD:
			case ACT_CHARM_OTHER:
			case ACT_SUMMON_PHANTOM:
			case ACT_REST_ALL:
			case ACT_RUNE_EXPLO:
				chance = 33;
				break;
			case ACT_CALL_CHAOS:
			case ACT_ROCKET:
			case ACT_CHARM_ANIMALS:
			case ACT_CHARM_OTHERS:
			case ACT_SUMMON_ELEMENTAL:
			case ACT_CURE_700:
			case ACT_SPEED:
			case ACT_ID_FULL:
			case ACT_RUNE_PROT:
				chance = 25;
				break;
			case ACT_CURE_1000:
			case ACT_XTRA_SPEED:
			case ACT_DETECT_XTRA:
			case ACT_DIM_DOOR:
				chance = 10;
				break;
			case ACT_SUMMON_UNDEAD:
			case ACT_SUMMON_DEMON:
			case ACT_WRAITH:
			case ACT_INVULN:
			case ACT_ALCHEMY:
				chance = 5;
				break;
			default:
				chance = 0;
		}
	}

	/* A type was chosen... */
	o_ptr->activate = type;
	o_ptr->flags3 |= TR3_ACTIVATE;
	o_ptr->timeout = 0;
}


static void get_random_name(char *return_name, byte tval, int power)
{
	if ((randint1(100) <= TABLE_NAME) ||
		(tval == TV_AMULET) || (tval == TV_RING))
	{
		get_table_name(return_name, TRUE);
	}
	else
	{
		cptr filename;

		/* Armour or a Weapon? */
		if (tval >= TV_BOOTS)
		{
			switch (power)
			{
				case 0:
					filename = "a_cursed.txt";
					break;
				case 1:
					filename = "a_low.txt";
					break;
				case 2:
					filename = "a_med.txt";
					break;
				default:
					filename = "a_high.txt";
			}
		}
		else
		{
			switch (power)
			{
				case 0:
					filename = "w_cursed.txt";
					break;
				case 1:
					filename = "w_low.txt";
					break;
				case 2:
					filename = "w_med.txt";
					break;
				default:
					filename = "w_high.txt";
			}
		}

		(void)get_rnd_line(filename, 0, return_name);
	}
}


bool create_artifact(object_type *o_ptr, bool a_scroll)
{
	char new_name[1024];
	int has_pval = 0;
	int powers = rand_range(2, 6);
	int power_level;
	s32b total_flags;
	bool a_cursed = FALSE;
	int warrior_artifact_bias = 0;
	int artifact_bias = 0;

	/* Moria had no artifacts */
	if (ironman_moria) return (FALSE);

	/* No activation yet */
	o_ptr->activate = 0;

	if (a_scroll && one_in_(4))
	{
		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR:
				artifact_bias = BIAS_WARRIOR;
				break;
			case CLASS_MAGE:
			case CLASS_HIGH_MAGE:
				artifact_bias = BIAS_MAGE;
				break;
			case CLASS_PRIEST:
				artifact_bias = BIAS_PRIESTLY;
				break;
			case CLASS_ROGUE:
				artifact_bias = BIAS_ROGUE;
				warrior_artifact_bias = 25;
				break;
			case CLASS_RANGER:
				artifact_bias = BIAS_RANGER;
				warrior_artifact_bias = 30;
				break;
			case CLASS_PALADIN:
				artifact_bias = BIAS_PRIESTLY;
				warrior_artifact_bias = 40;
				break;
			case CLASS_WARRIOR_MAGE:
				artifact_bias = BIAS_MAGE;
				warrior_artifact_bias = 40;
				break;
			case CLASS_CHAOS_WARRIOR:
				artifact_bias = BIAS_CHAOS;
				warrior_artifact_bias = 40;
				break;
			case CLASS_MONK:
				artifact_bias = BIAS_PRIESTLY;
				break;
			case CLASS_MINDCRAFTER:
				if (one_in_(2)) artifact_bias = BIAS_PRIESTLY;
				break;
		}
	}

	if (a_scroll && (randint1(100) <= warrior_artifact_bias))
		artifact_bias = BIAS_WARRIOR;

	strcpy(new_name, "");

	if (!a_scroll && one_in_(A_CURSED))
		a_cursed = TRUE;

	while (one_in_(powers) || one_in_(7) || one_in_(10))
		powers++;

	if (!a_cursed && one_in_(WEIRD_LUCK))
		powers *= 2;

	if (a_cursed) powers /= 2;

	/* Main loop */
	while (powers--)
	{
		switch (randint1(o_ptr->tval < TV_BOOTS ? 7 : 5))
		{
			case 1:  case 2:
				artifact_bias = random_plus(o_ptr, artifact_bias);
				has_pval = TRUE;
				break;
			case 3:  case 4:
				artifact_bias = random_resistance(o_ptr, 0, artifact_bias);
				break;
			case 5:
				artifact_bias = random_misc(o_ptr, artifact_bias);
				break;
			case 6:  case 7:
				artifact_bias = random_slay(o_ptr, artifact_bias);
				break;
		}
	};

	if (has_pval)
	{
		if (o_ptr->flags1 & TR1_BLOWS)
		{
			if (one_in_(100))
			{
				o_ptr->pval = 2;
			}
			else
			{
				o_ptr->pval = 1;
			}
		}
		else
		{
			do
			{
				o_ptr->pval++;
			}
			while (o_ptr->pval < randint1(5) || one_in_(o_ptr->pval));
		}

		if ((o_ptr->pval > 4) && !one_in_(WEIRD_LUCK))
			o_ptr->pval = 4;
	}

	/* give it some plusses... */
	if (o_ptr->tval >= TV_BOOTS)
		o_ptr->to_a += randint1(o_ptr->to_a > 19 ? 1 : 20 - o_ptr->to_a);
	else
	{
		o_ptr->to_h += randint1(o_ptr->to_h > 19 ? 1 : 20 - o_ptr->to_h);
		o_ptr->to_d += randint1(o_ptr->to_d > 19 ? 1 : 20 - o_ptr->to_d);
	}

	/* Just to be sure */
	o_ptr->flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
					  TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	total_flags = flag_cost(o_ptr, o_ptr->pval);
	if (cheat_peek) msg_format("%ld", total_flags);

	if (a_cursed) curse_artifact(o_ptr);

	if (!a_cursed && one_in_((o_ptr->tval >= TV_BOOTS)
							 ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE))
	{
		o_ptr->activate = 0;
		give_activation_power(o_ptr, artifact_bias);
	}

	if (o_ptr->dd && o_ptr->ds)
	{
		if (one_in_(10L * o_ptr->dd * o_ptr->ds))
		{
			o_ptr->ds += (o_ptr->ds * randint1(5)) / 5;
		}
	}

	if (o_ptr->tval >= TV_BOOTS)
	{
		if (a_cursed) power_level = 0;
		else if (total_flags < 10000) power_level = 1;
		else if (total_flags < 20000) power_level = 2;
		else
			power_level = 3;
	}

	else
	{
		if (a_cursed) power_level = 0;
		else if (total_flags < 15000) power_level = 1;
		else if (total_flags < 30000) power_level = 2;
		else
			power_level = 3;
	}

	if (a_scroll)
	{
		char dummy_name[80];
		strcpy(dummy_name, "");
		(void)identify_fully_aux(o_ptr);
		o_ptr->info |= OB_STOREB;

		if (!(get_string
			  ("What do you want to call the artifact? ", dummy_name, 80)))
		{
			get_random_name(new_name, o_ptr->tval, power_level);
		}
		else
		{
			strcpy(new_name, "'");
			strcat(new_name, dummy_name);
			strcat(new_name, "'");
		}
		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags1 = o_ptr->flags1;
		o_ptr->kn_flags2 = o_ptr->flags2;
		o_ptr->kn_flags3 = o_ptr->flags3;
	}
	else
	{
		get_random_name(new_name, o_ptr->tval, power_level);
	}

	if (cheat_xtra)
	{
		if (artifact_bias)
			msg_format("Biased artifact: %d.", artifact_bias);
		else
			msg_print("No bias in artifact.");
	}

	chg_virtue(V_INDIVIDUALISM, 2);
	chg_virtue(V_ENCHANT, 5);

	/* Save the inscription */
	o_ptr->xtra_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Make the object an artifact */
	o_ptr->flags3 |= TR3_INSTA_ART;

	/* Set the cost */
	o_ptr->cost = k_info[o_ptr->k_idx].cost + flag_cost(o_ptr, o_ptr->pval);

	return TRUE;
}

/*
 * Activate an artifact / random artifact or ego item
 */
bool activate_effect(object_type *o_ptr)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	int plev = p_ptr->lev;
	int k, dir, dummy;
	byte activate;

	char o_name[256];

	/* Get the basic name of the object */
	object_desc(o_name, o_ptr, FALSE, 0, 256);

	/* Get activation */
	activate = o_ptr->activate;

	/* Normal artifacts */
	if (activate > 127)
	{
		switch (activate - 128)
		{
			case ART_GALADRIEL:
			{
				msg_print("The phial wells with clear light...");
				(void)lite_area(damroll(2, 15), 3);
				o_ptr->timeout = (s16b)rand_range(10, 20);
				break;
			}

			case ART_ELENDIL:
			{
				msg_print("The star shines brightly...");
				map_area();
				(void)lite_area(damroll(2, 15), 3);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case ART_THRAIN:
			{
				msg_print("The Jewel flashes bright red!");
				wiz_lite();
				msg_print("The Jewel drains your vitality...");
				take_hit(damroll(3, 8), "the Jewel of Judgement");
				(void)detect_traps();
				(void)detect_doors();
				(void)detect_stairs();

				if (get_check("Activate recall? "))
				{
					word_of_recall();
				}

				o_ptr->timeout = (s16b)rand_range(20, 40);
				break;
			}

			case ART_CARLAMMAS:
			{
				msg_print("The amulet lets out a shrill wail...");
				k = 3 * p_ptr->lev;
				(void)set_protevil(p_ptr->protevil + randint1(25) + k);
				o_ptr->timeout = (s16b)rand_range(225, 450);
				break;
			}

			case ART_INGWE:
			{
				msg_print("The amulet floods the area with goodness...");
				(void)dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = (s16b)rand_range(300, 600);
				break;
			}

			case ART_BARAHIR:
			{
				msg_print("You order Frakir to strangle your opponent.");
				if (!get_aim_dir(&dir)) return FALSE;
				if (drain_life(dir, 200))
					o_ptr->timeout = (s16b)rand_range(100, 200);
				break;
			}

			case ART_TULKAS:
			{
				msg_print("The ring glows brightly...");
				if (!p_ptr->fast)
				{
					(void)set_fast(rand_range(75, 150));
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = (s16b)rand_range(150, 300);
				break;
			}

			case ART_NARYA:
			{
				msg_print("The ring glows deep red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 250, 3);
				o_ptr->timeout = (s16b)rand_range(225, 450);
				break;
			}

			case ART_NENYA:
			{
				msg_print("The ring glows bright white...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 400, 3);
				o_ptr->timeout = (s16b)rand_range(325, 650);
				break;
			}

			case ART_VILYA:
			{
				msg_print("The ring glows deep blue...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_ELEC, dir, 500, 3);
				o_ptr->timeout = (s16b)rand_range(425, 850);
				break;
			}

			case ART_POWER:
			{
				msg_print("The ring glows intensely black...");
				if (!get_aim_dir(&dir)) return FALSE;
				ring_of_power(dir);
				o_ptr->timeout = (s16b)rand_range(450, 900);
				break;
			}

			case ART_ELEMENTS:
			{
				msg_print("The ring glows in multiple colours...");
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_MISSILE, dir, 800, 3);
				o_ptr->timeout = (s16b)rand_range(250, 500);
				break;
			}

			case ART_RAZORBACK:
			{
				int num = damroll(5, 3);
				int y, x;
				int attempts;
				cave_type *c_ptr;

				msg_print("Your armor is surrounded by lightning...");

				for (k = 0; k < num; k++)
				{
					attempts = 1000;

					while (attempts--)
					{
						scatter(&x, &y, px, py, 4);

						/* paranoia */
						if (!in_bounds2(x, y)) continue;

						c_ptr = area(x, y);
						if (cave_wall_grid(c_ptr)) continue;

						if ((y != py) || (x != px)) break;
					}

					(void)project(0, 3, x, y, 1000, GF_ELEC,
								  (PROJECT_THRU | PROJECT_STOP | PROJECT_GRID |
								   PROJECT_ITEM | PROJECT_KILL));
				}

				o_ptr->timeout = 100;
				break;
			}

			case ART_BLADETURNER:
			{
				if (!get_aim_dir(&dir)) return FALSE;
				msg_print("You breathe the elements.");
				(void)fire_ball(GF_MISSILE, dir, 1000, 4);
				msg_print("Your armor glows many colours...");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + rand_range(50, 100));
				(void)hp_player(30);
				(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
				(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(50, 100));
				(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(50, 100));
				(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(50, 100));
				(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(50, 100));
				(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(50, 100));
				o_ptr->timeout = 100;
				break;
			}

			case ART_SOULKEEPER:
			{
				msg_print("Your armor glows a bright white...");
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)set_cut(0);
				o_ptr->timeout = 888;
				break;
			}

			case ART_BELEGENNON:
			{
				msg_print("A heavenly choir sings...");
				(void)set_poisoned(0);
				(void)set_cut(0);
				(void)set_stun(0);
				(void)set_confused(0);
				(void)set_blind(0);
				(void)set_hero(p_ptr->hero + rand_range(25, 50));
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}

			case ART_CELEBORN:
			{
				msg_print("Your armor glows deep blue...");
				(void)genocide(TRUE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_CASPANION:
			{
				msg_print("Your armor glows bright red...");
				(void)destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}

			case ART_DOR:
			case ART_TERROR:
			{
				(void)turn_monsters(40 + p_ptr->lev);
				o_ptr->timeout = 3 * (p_ptr->lev + 10);
				break;
			}

			case ART_HOLHENNETH:
			{
				msg_print("Your helm glows bright white...");
				msg_print("An image forms in your mind...");
				(void)detect_all();
				o_ptr->timeout = (s16b)rand_range(55, 110);
				break;
			}

			case ART_GONDOR:
			{
				msg_print("Your crown glows deep blue...");
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)set_cut(0);
				o_ptr->timeout = 250;
				break;
			}

			case ART_KERI:
			{
				object_type *q_ptr;

				msg_print("Your rag feels warm for a moment...");

				/* Hack - Create the food ration */
				q_ptr = object_prep(lookup_kind(TV_FOOD, SV_FOOD_RATION));

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, p_ptr->px, p_ptr->py);

				o_ptr->timeout = 100;

				break;
			}

			case ART_COLLUIN:
			{
				msg_print("Your cloak glows many colours...");
				(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(20, 40));
				(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(20, 40));
				(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(20, 40));
				(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(20, 40));
				(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(20, 40));
				o_ptr->timeout = 111;
				break;
			}

			case ART_HOLCOLLETH:
			{
				msg_print("Your cloak glows deep blue...");
				(void)sleep_monsters_touch();
				o_ptr->timeout = 55;
				break;
			}

			case ART_THINGOL:
			{
				msg_print("Your cloak glows bright yellow...");
				(void)recharge(130);
				o_ptr->timeout = 70;
				break;
			}

			case ART_COLANNON:
			{
				msg_print("Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_LUTHIEN:
			{
				msg_print("Your cloak glows a deep red...");
				(void)restore_level();
				o_ptr->timeout = 450;
				break;
			}

			case ART_CAMMITHRIM:
			{
				msg_print("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
				o_ptr->timeout = 2;
				break;
			}

			case ART_PAURHACH:
			{
				msg_print("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
				o_ptr->timeout = (s16b)rand_range(8, 16);
				break;
			}

			case ART_CORWIN:
			{
				msg_print("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_COLD, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(7, 14);
				break;
			}

			case ART_PAURAEGEN:
			{
				msg_print("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = (s16b)rand_range(6, 12);
				break;
			}

			case ART_PAURNEN:
			{
				msg_print("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(5, 10);
				break;
			}

			case ART_FINGOLFIN:
			{
				msg_print("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ARROW, dir, 250);
				o_ptr->timeout = (s16b)rand_range(90, 180);
				break;
			}

			case ART_FEANOR:
			{
				msg_print("Your boots glow bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(rand_range(20, 40));
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = 200;
				break;
			}

			case ART_DAL:
			{
				msg_print("Your boots glow deep blue...");
				(void)set_afraid(0);
				(void)set_poisoned(0);
				o_ptr->timeout = 5;
				break;
			}

			case ART_NARTHANC:
			{
				msg_print("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
				o_ptr->timeout = (s16b)rand_range(8, 16);
				break;
			}

			case ART_NIMTHANC:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_COLD, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(7, 14);
				break;
			}

			case ART_DETHANC:
			{
				msg_print("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = (s16b)rand_range(6, 12);
				break;
			}

			case ART_RILIA:
			{
				msg_print("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_POIS, dir, 25, 3);
				o_ptr->timeout = (s16b)rand_range(4, 8);
				break;
			}

			case ART_BELANGIL:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 100, 2);
				o_ptr->timeout = (s16b)rand_range(5, 10);
				break;
			}

			case ART_ANGUIREL:
			{
				switch (randint1(13))
				{
					case 1:  case 2:  case 3:  case 4:  case 5:
						teleport_player(10);
						break;
					case 6:  case 7:  case 8:  case 9:  case 10:
						teleport_player(222);
						break;
					case 11:  case 12:
						(void)stair_creation();
						break;
					default:
						if (get_check("Leave this level? "))
						{
							if (autosave_l) do_cmd_save_game(TRUE);

							/* Leaving */
							p_ptr->leaving = TRUE;
						}
				}
				o_ptr->timeout = 35;
				break;
			}

			case ART_RINGIL:
			{
				msg_print("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 200, 2);
				o_ptr->timeout = 300;
				break;
			}

			case ART_DAWN:
			{
				msg_print("You summon the Legion of the Dawn.");
				(void)summon_specific(-1, px, py, p_ptr->depth, SUMMON_DAWN,
									  TRUE, TRUE, TRUE);
				o_ptr->timeout = (s16b)rand_range(500, 1000);
				break;
			}

			case ART_ANDURIL:
			{
				msg_print("Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 150, 2);
				o_ptr->timeout = 400;
				break;
			}

			case ART_THEODEN:
			{
				msg_print("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)drain_life(dir, 200);
				o_ptr->timeout = 400;
				break;
			}

			case ART_AEGLOS:
			{
				msg_print("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_ELEC, dir, 200, 3);
				o_ptr->timeout = 500;
				break;
			}

			case ART_OROME:
			{
				msg_print("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

			case ART_EONWE:
			{
				msg_print("Your axe lets out a long, shrill note...");
				(void)mass_genocide(TRUE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_LOTHARANG:
			{
				msg_print("Your battle axe radiates deep purple...");
				(void)hp_player(100);
				(void)set_cut((p_ptr->cut / 2) - 50);
				o_ptr->timeout = (s16b)rand_range(3, 6);
				break;
			}

			case ART_ULMO:
			{
				msg_print("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

			case ART_AVAVIR:
			{
				msg_print("Your scythe glows soft white...");

				word_of_recall();

				o_ptr->timeout = 200;
				break;
			}

			case ART_TOTILA:
			{
				msg_print("Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)confuse_monster(dir, 50);
				o_ptr->timeout = 15;
				break;
			}

			case ART_WHIRLWIND:
			{
				int y, x;
				cave_type *c_ptr;
				monster_type *m_ptr;

				msg_print("Your ball and chain swings through the air...");

				for (dir = 0; dir <= 9; dir++)
				{
					y = py + ddy[dir];
					x = px + ddx[dir];

					/* paranoia */
					if (!in_bounds2(x, y)) continue;

					c_ptr = area(x, y);

					/* Get the monster */
					m_ptr = &m_list[c_ptr->m_idx];

					/* Hack -- attack monsters */
					if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
						py_attack(x, y);
				}
				o_ptr->timeout = rand_range(50, 100);
				break;
			}

			case ART_FIRESTAR:
			{
				msg_print("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 200, 3);
				o_ptr->timeout = 100;
				break;
			}

			case ART_ENERGY:
			{
				msg_print("Your scythe glows bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(rand_range(20, 40));
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = (s16b)rand_range(100, 200);
				break;
			}

			case ART_ERIRIL:
			{
				msg_print("Your quarterstaff glows yellow...");
				if (!ident_spell()) return FALSE;
				o_ptr->timeout = 10;
				break;
			}

			case ART_OLORIN:
			{
				msg_print("Your quarterstaff glows brightly...");
				(void)detect_all();
				(void)probing();
				(void)identify_fully();
				o_ptr->timeout = 1000;
				break;
			}

			case ART_TURMIL:
			{
				msg_print("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)drain_life(dir, 200);
				o_ptr->timeout = 70;
				break;
			}

			case ART_CATAPULT:
			{
				msg_print("Your sling hums...");
				(void)set_afraid(0);
				(void)hp_player(45);
				o_ptr->timeout = 10;
				break;
			}

			case ART_BRAND:
			{
				msg_print("Your crossbow glows deep red...");
				(void)brand_bolts();
				o_ptr->timeout = 999;
				break;
			}
		}

		/* Done */
		return TRUE;
	}

	/* Activate random artifacts and ego items */
	switch (o_ptr->activate)
	{
		case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("A line of sunlight appears.");
			(void)lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
			msg_format("The %s glows extremely brightly...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
			msg_format("The %s throbs deep green...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_POIS, dir, 25, 3);
			o_ptr->timeout = (s16b)rand_range(4, 8);
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msg_format("The %s is covered in sparks...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
			o_ptr->timeout = (s16b)rand_range(6, 12);
			break;
		}

		case ACT_BO_ACID_1:
		{
			msg_format("The %s is covered in acid...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
			o_ptr->timeout = (s16b)rand_range(5, 10);
			break;
		}

		case ACT_BO_COLD_1:
		{
			msg_format("The %s is covered in frost...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_COLD, dir, damroll(9, 8));
			o_ptr->timeout = (s16b)rand_range(7, 14);
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msg_format("The %s is covered in fire...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
			o_ptr->timeout = (s16b)rand_range(8, 16);
			break;
		}

		case ACT_BA_COLD_1:
		{
			msg_format("The %s is covered in frost...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
			msg_format("The %s glows an intense red...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 150, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
			msg_format("The %s glows black...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 200))
				o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_BA_COLD_2:
		{
			msg_format("The %s glows an intense blue...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 200, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
			msg_format("The crackles with electricity...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 200, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
			msg_format("The %s glows black...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)drain_life(dir, 250);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;

			msg_format("The %s throbs red...", o_name);
			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 100);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
			msg_format("The %s grows magical spikes...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ARROW, dir, 250);
			o_ptr->timeout = (s16b)rand_range(90, 180);
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msg_format("The %s glows deep red...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 250, 3);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_BA_COLD_3:
		{
			msg_format("The %s glows bright white...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 400, 3);
			o_ptr->timeout = (s16b)rand_range(325, 650);
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msg_format("The %s glows deep blue...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 500, 3);
			o_ptr->timeout = (s16b)rand_range(425, 850);
			break;
		}

		case ACT_WHIRLWIND:
		{
			int y, x;
			cave_type *c_ptr;
			monster_type *m_ptr;

			msg_format("The %s emmits a blast of air...", o_name);

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];

				/* paranoia */
				if (!in_bounds2(x, y)) continue;

				c_ptr = area(x, y);

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
					py_attack(x, y);
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_VAMPIRE_2:
		{
			if (!get_aim_dir(&dir)) return FALSE;

			msg_format("The %s throbs red...", o_name);

			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 200);
			}

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
			msg_format("The %s glows in scintillating colours...", o_name);
			call_chaos();
			o_ptr->timeout = 350;
			break;
		}

		case ACT_ROCKET:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You launch a rocket!");
			(void)fire_ball(GF_ROCKET, dir, 300 + plev, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
			msg_format("The %s floods the area with goodness...", o_name);
			(void)dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_DISP_GOOD:
		{
			msg_format("The %s floods the area with evil...", o_name);
			(void)dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You breathe the elements.");
			(void)fire_ball(GF_MISSILE, dir, 600, 4);
			o_ptr->timeout = 500;
			break;
		}

			/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
			msg_format("The %s glows in scintillating colours...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)confuse_monster(dir, 50);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
			msg_format("The %s glows deep blue...", o_name);
			(void)sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
			msg_format("The %s vibrates...", o_name);

			(void)earthquake(px, py, 10);
			o_ptr->timeout = 50;
			break;
		}

		case ACT_TERROR:
		{
			msg_format("The %s emmits a loud blast...", o_name);

			(void)turn_monsters(40 + p_ptr->lev);
			o_ptr->timeout = 3 * (p_ptr->lev + 10);
			break;
		}

		case ACT_TELE_AWAY:
		{
			msg_format("The %s glows violet...", o_name);

			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BANISH_EVIL:
		{
			if (banish_evil(200))
			{
				msg_print("The power of the artifact banishes evil!");
			}
			o_ptr->timeout = (s16b)rand_range(250, 500);
			break;
		}

		case ACT_GENOCIDE:
		{
			msg_format("The %s glows deep blue...", o_name);
			(void)genocide(TRUE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_MASS_GENO:
		{
			msg_format("The %s lets out a long, shrill note...", o_name);
			(void)mass_genocide(TRUE);
			o_ptr->timeout = 1000;
			break;
		}

			/* Activate for summoning / charming */

		case ACT_CHARM_ANIMAL:
		{
			msg_format("The %s twists in your hands...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_animal(dir, plev);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_CHARM_UNDEAD:
		{
			msg_format("The %s shudders...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)control_one_undead(dir, plev);
			o_ptr->timeout = 333;
			break;
		}

		case ACT_CHARM_OTHER:
		{
			msg_format("The %s fades in and out...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_CHARM_ANIMALS:
		{
			msg_format("The %s hums softly...", o_name);
			(void)charm_animals(plev * 2);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_CHARM_OTHERS:
		{
			msg_format("The %s blinks in and out...", o_name);
			(void)charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_ANIMAL:
		{
			msg_print("You summon a beast.");
			(void)summon_specific(-1, px, py, plev, SUMMON_ANIMAL_RANGER, TRUE,
								  TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 500);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msg_print("You summon a phantasmal servant.");
			(void)summon_specific(-1, px, py, p_ptr->depth, SUMMON_PHANTOM,
								  TRUE, TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, ((plev * 3) / 2), SUMMON_ELEMENTAL,
				 group, FALSE, pet))
			{
				msg_print("An elemental materializes...");

				if (pet)
					msg_print("It seems obedient to you.");
				else
					msg_print("You fail to control it!");
			}

			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			bool pet = one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, ((plev * 3) / 2), SUMMON_DEMON, group,
				 FALSE, pet))
			{
				msg_print
					("The area fills with a stench of sulphur and brimstone.");
				if (pet)
					msg_print("'What is thy bidding... Master?'");
				else
					msg_print
						("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			bool pet = one_in_(3);
			bool group;
			int type;

			if (pet)
			{
				type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);
				group = (((plev > 24) && one_in_(3)) ? TRUE : FALSE);
			}
			else
			{
				type =
					(plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD);
				group = TRUE;
			}

			if (summon_specific((pet ? -1 : 0), px, py, ((plev * 3) / 2), type,
								group, FALSE, pet))
			{
				msg_print
					("Cold winds begin to blow around you, carrying with them the stench of decay...");
				if (pet)
					msg_print
						("Ancient, long-dead forms arise from the ground to serve you!");
				else
					msg_print
						("'The dead arise... to punish you for disturbing them!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}

			/* Activate for healing */

		case ACT_CURE_LW:
		{
			msg_format("The %s radiates light blue...", o_name);
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msg_format("The %s radiates deep purple...", o_name);
			(void)hp_player(75);
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = (s16b)rand_range(3, 6);
			break;
		}

		case ACT_CURE_POISON:
		{
			msg_format("The %s glows deep blue...", o_name);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_REST_LIFE:
		{
			msg_format("The %s glows a deep red...", o_name);
			(void)restore_level();
			o_ptr->timeout = 450;
			break;
		}

		case ACT_REST_ALL:
		{
			msg_format("The %s glows a deep green...", o_name);
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			(void)restore_level();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_CURE_700:
		{
			msg_format("The %s glows deep blue...", o_name);
			msg_print("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_cut(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msg_format("The %s glows a bright white...", o_name);
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}

			/* Activate for timed effect */

		case ACT_ESP:
		{
			msg_format("The %s enters your thoughts...", o_name);
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			msg_format("The %s angers you...", o_name);
			(void)set_shero(p_ptr->shero + rand_range(50, 100));
			(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
			o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msg_format("The %s lets out a shrill wail...", o_name);
			k = 3 * p_ptr->lev;
			(void)set_protevil(p_ptr->protevil + randint1(25) + k);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_RESIST_ALL:
		{
			msg_format("The %s glows many colours...", o_name);
			(void)set_oppose_acid(p_ptr->oppose_acid + rand_range(40, 80));
			(void)set_oppose_elec(p_ptr->oppose_elec + rand_range(40, 80));
			(void)set_oppose_fire(p_ptr->oppose_fire + rand_range(40, 80));
			(void)set_oppose_cold(p_ptr->oppose_cold + rand_range(40, 80));
			(void)set_oppose_pois(p_ptr->oppose_pois + rand_range(40, 80));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
			msg_format("The %s glows bright green...", o_name);
			if (!p_ptr->fast)
			{
				(void)set_fast(rand_range(20, 40));
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_XTRA_SPEED:
		{
			msg_format("The %s glows brightly...", o_name);
			if (!p_ptr->fast)
			{
				(void)set_fast(rand_range(75, 150));
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_WRAITH:
		{
			msg_format("The %s fades out...", o_name);
			(void)set_wraith_form(p_ptr->wraith_form +
								  rand_range(plev / 2, plev));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			msg_format("The %s fires a beam of bright white light at you...",
					   o_name);
			(void)set_invuln(p_ptr->invuln + rand_range(8, 16));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_TELEPORT_1:
		{
			msg_format("The %s twists space around you...", o_name);
			teleport_player(100);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

			/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
			msg_format("The %s wells with clear light...", o_name);
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(10, 20);
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msg_format("The %s shines brightly...", o_name);
			map_area();
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

		case ACT_DETECT_ALL:
		{
			msg_format("The %s glows bright white...", o_name);
			msg_print("An image forms in your mind...");
			(void)detect_all();
			o_ptr->timeout = (s16b)rand_range(55, 110);
			break;
		}

		case ACT_DETECT_XTRA:
		{
			msg_format("The %s glows brightly...", o_name);
			(void)detect_all();
			(void)probing();
			(void)identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
			msg_format("The %s glows yellow...", o_name);
			(void)identify_fully();
			o_ptr->timeout = 750;
			break;
		}

		case ACT_ID_PLAIN:
		{
			if (!ident_spell()) return FALSE;
			o_ptr->timeout = 10;
			break;
		}

		case ACT_RUNE_EXPLO:
		{
			msg_format("The %s glows bright red...", o_name);
			(void)explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
			msg_format("The %s glows light blue...", o_name);
			(void)warding_glyph();
			o_ptr->timeout = 400;
			break;
		}

		case ACT_SATIATE:
		{
			msg_format("The %s glows brown...", o_name);
			(void)set_food(PY_FOOD_MAX - 1);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_DEST_DOOR:
		{
			msg_format("The %s glows bright red...", o_name);
			(void)destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
			msg_format("The %s pulsates...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
			msg_format("The %s hums...", o_name);
			(void)recharge(130);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
			msg_format("The %s glows bright yellow...", o_name);
			(void)alchemy();
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DIM_DOOR:
		{
			msg_print("You open a dimensional gate. Choose a destination.");
			if (!dimension_door()) return FALSE;
			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT_2:
		{
			msg_format("The %s twists space around you...", o_name);
			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}

		case ACT_RECALL:
		{
			word_of_recall();

			o_ptr->timeout = 200;
			break;
		}

		default:
		{
			msg_format("Unknown activation effect: %d.", o_ptr->activate);
			return FALSE;
		}
	}

	return TRUE;
}


void random_artifact_resistance(object_type *o_ptr)
{
	bool give_resistance = FALSE, give_power = FALSE;

	/* Terror Mask is for warriors... */
	if (o_ptr->activate == ART_TERROR + 128)
	{
		if (p_ptr->pclass == CLASS_WARRIOR)
		{
			give_power = TRUE;
			give_resistance = TRUE;
		}
		else
		{
			o_ptr->flags3 |=
				(TR3_CURSED | TR3_HEAVY_CURSE | TR3_AGGRAVATE | TR3_TY_CURSE);
			return;
		}
	}

	switch (o_ptr->activate - 128)
	{
		case ART_CELEBORN:
		case ART_ARVEDUI:
		case ART_CASPANION:
		case ART_HITHLOMIR:
		case ART_ROHIRRIM:
		case ART_CELEGORM:
		case ART_ANARION:
		case ART_THRANDUIL:
		case ART_LUTHIEN:
		case ART_THROR:
		case ART_THORIN:
		case ART_NIMTHANC:
		case ART_DETHANC:
		case ART_NARTHANC:
		case ART_STING:
		case ART_TURMIL:
		case ART_THALKETTOTH:
		{
			/* Give a resistance */
			give_resistance = TRUE;
		}
			break;
		case ART_MAEDHROS:
		case ART_GLAMDRING:
		case ART_ORCRIST:
		case ART_ANDURIL:
		case ART_ZARCUTHRA:
		case ART_GURTHANG:
		case ART_HARADEKKET:
		case ART_BRAND:
		case ART_DAWN:
		{
			/* Give a resistance OR a power */
			if (one_in_(2)) give_resistance = TRUE;
			else
				give_power = TRUE;
		}
			break;
		case ART_NENYA:
		case ART_VILYA:
		case ART_BERUTHIEL:
		case ART_FINGOLFIN:
		case ART_THINGOL:
		case ART_ULMO:
		case ART_OLORIN:
		{
			/* Give a power */
			give_power = TRUE;
		}
			break;
		case ART_POWER:
		case ART_GONDOR:
		case ART_AULE:
		{
			/* Give both */
			give_power = TRUE;
			give_resistance = TRUE;
		}
			break;
	}

	if (give_power)
	{
		add_ego_power(EGO_XTRA_ABILITY, o_ptr);
	}

	if (give_resistance)
	{
		(void)random_resistance(o_ptr, rand_range(17, 38), 0);
	}
}


/*
 * Create the artifact of the specified number
 */
void create_named_art(int a_idx, int x, int y)
{
	object_type *q_ptr;
	int i;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return;

	/* Create the artifact */
	q_ptr = object_prep(i);

	/* Set the activation */
	q_ptr->activate = a_idx + 128;

	/* Do not make another one */
	a_ptr->cur_num = 1;

	/* Save the artifact flags */
	q_ptr->flags1 |= a_ptr->flags1;
	q_ptr->flags2 |= a_ptr->flags2;
	q_ptr->flags3 |= a_ptr->flags3;

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Save the inscription */
	q_ptr->xtra_name = quark_add(a_name + a_ptr->name);

	random_artifact_resistance(q_ptr);

	if (!a_ptr->cost)
	{
		/* Hack -- "worthless" artifacts */
		q_ptr->cost = 0L;
	}
	else
	{
		/* Hack - use the artifact price */
		q_ptr->cost = k_info[q_ptr->k_idx].cost + a_ptr->cost;
	}

	/* Drop the artifact from heaven */
	drop_near(q_ptr, -1, x, y);
}
