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

	if (one_in_(4)) o_ptr->flags3 |= TR3_PERMA_CURSE;
	if (one_in_(3)) o_ptr->flags3 |= TR3_TY_CURSE;
	if (one_in_(2)) o_ptr->flags3 |= TR3_AGGRAVATE;
	if (one_in_(3)) o_ptr->flags3 |= TR3_DRAIN_EXP;
	if (one_in_(2)) o_ptr->flags3 |= TR3_TELEPORT;
	else if (one_in_(3)) o_ptr->flags3 |= TR3_NO_TELE;

	if ((p_ptr->pclass != CLASS_WARRIOR) && one_in_(3))
		o_ptr->flags3 |= TR3_NO_MAGIC;

	o_ptr->ident |= IDENT_CURSED;
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
	case 1: case 2:
		o_ptr->flags1 |= TR1_STR;

		if (!artifact_bias && !one_in_(13))
			artifact_bias = BIAS_STR;
		else if (!artifact_bias && one_in_(7))
			artifact_bias = BIAS_WARRIOR;
		break;
	case 3: case 4:
		o_ptr->flags1 |= TR1_INT;

		if (!artifact_bias && !one_in_(13))
			artifact_bias = BIAS_INT;
		else if (!artifact_bias && one_in_(7))
			artifact_bias = BIAS_MAGE;
		break;
	case 5: case 6:
		o_ptr->flags1 |= TR1_WIS;

		if (!artifact_bias && !one_in_(13))
			artifact_bias = BIAS_WIS;
		else if (!artifact_bias && one_in_(7))
			artifact_bias = BIAS_PRIESTLY;
		break;
	case 7: case 8:
		o_ptr->flags1 |= TR1_DEX;

		if (!artifact_bias && !one_in_(13))
			artifact_bias = BIAS_DEX;
		else if (!artifact_bias && one_in_(7))
			artifact_bias = BIAS_ROGUE;
		break;
	case 9: case 10:
		o_ptr->flags1 |= TR1_CON;

		if (!artifact_bias && !one_in_(13))
			artifact_bias = BIAS_CON;
		else if (!artifact_bias && one_in_(9))
			artifact_bias = BIAS_RANGER;
		break;
	case 11: case 12:
		o_ptr->flags1 |= TR1_CHR;

		if (!artifact_bias && !one_in_(13))
			artifact_bias = BIAS_CHR;
		break;
	case 13: case 14:
		o_ptr->flags1 |= TR1_STEALTH;

		if (!artifact_bias && one_in_(3))
			artifact_bias = BIAS_ROGUE;
		break;
	case 15: case 16:
		o_ptr->flags1 |= TR1_SEARCH;

		if (!artifact_bias && one_in_(9))
			artifact_bias = BIAS_RANGER;
		break;
	case 17: case 18:
		o_ptr->flags1 |= TR1_INFRA;

		break;
	case 19:
		o_ptr->flags1 |= TR1_SPEED;

		if (!artifact_bias && one_in_(11))
			artifact_bias = BIAS_ROGUE;
		break;
	case 20: case 21:
		o_ptr->flags1 |= TR1_TUNNEL;

		break;
	case 22: case 23:
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



int random_misc(object_type *o_ptr, int artifact_bias)
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
				o_ptr->flags3 |= TR3_LITE; /* Freebie */
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
			if (!(o_ptr->flags4 & TR4_SLAY_ANIMAL))
			{
				o_ptr->flags4 |= TR4_SLAY_ANIMAL;
            if (one_in_(5))
            {
               o_ptr->flags4 |= TR4_KILL_ANIMAL;
            }
				if (one_in_(2)) return (artifact_bias);
			}
			break;

		case BIAS_ROGUE:
			if ((((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DAGGER)) ||
			     ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_SPEAR)) ||
				 ((o_ptr->tval == TV_POLEARM) &&
				 	 (o_ptr->sval == SV_HATCHET))) &&
					  !(o_ptr->flags2 & TR2_THROW))
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

			if (!(o_ptr->flags5 & TR5_SLAY_EVIL))
			{
				o_ptr->flags5 |= TR5_SLAY_EVIL;
            if (one_in_(5))
            {
               o_ptr->flags5 |= TR5_KILL_EVIL;
            }
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags4 & TR4_SLAY_UNDEAD))
			{
				o_ptr->flags4 |= TR4_SLAY_UNDEAD;
            if (one_in_(5))
            {
               o_ptr->flags4 |= TR4_KILL_UNDEAD;
            }
				if (one_in_(2)) return (artifact_bias);
			}
			if (!(o_ptr->flags4 & TR4_SLAY_DEMON))
			{
				o_ptr->flags4 |= TR4_SLAY_DEMON;
            if (one_in_(5))
            {
               o_ptr->flags4 |= TR4_KILL_DEMON;
            }
				if (one_in_(2)) return (artifact_bias);
			}
			break;
	}

	switch (randint1(34))
	{
		case 1:
         o_ptr->flags1 |= TR1_CHAOTIC;
			if (!artifact_bias && one_in_(4))
				artifact_bias = BIAS_CHAOS;
         break;
  		case 2:
			o_ptr->flags1 |= TR1_VAMPIRIC;
			break;
      case 3:
		case 4:
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
      case 5:
         if (!(artifact_bias == BIAS_LAW) &&
             !(artifact_bias == BIAS_PRIESTLY))
             o_ptr->flags4 |= TR4_SLAY_ANGEL;
            if (one_in_(5))
            {
               o_ptr->flags4 |= TR4_KILL_ANGEL;
            }
         break;
      case 6:
			o_ptr->flags4 |= TR4_SLAY_ANIMAL;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_ANIMAL;
         }
         break;
      case 7:
         o_ptr->flags4 |= TR4_SLAY_DRAGON;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_DRAGON;
         }
         break;
      case 8:
         o_ptr->flags4 |= TR4_SLAY_INSECT;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_INSECT;
         }
         break;
      case 9:
         o_ptr->flags4 |= TR4_SLAY_UNDEAD;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_UNDEAD;
         }
         break;
      case 10:
         o_ptr->flags4 |= TR4_SLAY_KOBOLD;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_KOBOLD;
         }
         break;
      case 11:
         o_ptr->flags4 |= TR4_SLAY_HUMANOID;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_HUMANOID;
         }
         break;
      case 12:
         o_ptr->flags4 |= TR4_SLAY_MULTIHEAD;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_MULTIHEAD;
         }
         break;
      case 13:
         o_ptr->flags4 |= TR4_SLAY_HORROR;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_HORROR;
         }
         break;
      case 14:
         o_ptr->flags4 |= TR4_SLAY_GIANT;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_GIANT;
         }
         break;
      case 15:
         o_ptr->flags4 |= TR4_SLAY_PLANT;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_PLANT;
         }
         break;
      case 16:
         o_ptr->flags4 |= TR4_SLAY_TROLL;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_TROLL;
         }
         break;
      case 17:
         o_ptr->flags4 |= TR4_SLAY_DEMON;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_DEMON;
         }
         break;
      case 18:
         o_ptr->flags4 |= TR4_SLAY_AQUATIC;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_AQUATIC;
         }
         break;
      case 19:
         o_ptr->flags4 |= TR4_SLAY_XENO;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_XENO;
         }
         break;
      case 20:
         o_ptr->flags4 |= TR4_SLAY_HOUND;
         if (one_in_(5))
         {
            o_ptr->flags4 |= TR4_KILL_HOUND;
         }
         break;
      case 21:
         o_ptr->flags5 |= TR5_SLAY_CONSTRUCT;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_CONSTRUCT;
         }
         break;
      case 22:
         o_ptr->flags5 |= TR5_SLAY_FAERY;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_FAERY;
         }
         break;
      case 23:
         o_ptr->flags5 |= TR5_SLAY_GOBLIN;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_GOBLIN;
         }
         break;
      case 24:
         o_ptr->flags5 |= TR5_SLAY_NAGA;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_NAGA;
         }
         break;
      case 25:
         o_ptr->flags5 |= TR5_SLAY_ORC;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_ORC;
         }
         break;
      case 26:
         o_ptr->flags5 |= TR5_SLAY_WORM;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_WORM;
         }
         break;
      case 27:
         o_ptr->flags5 |= TR5_SLAY_YEEK;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_YEEK;
         }
         break;
      case 28:
         o_ptr->flags5 |= TR5_SLAY_MIMIC;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_MIMIC;
         }
         break;
      case 29:
         o_ptr->flags5 |= TR5_SLAY_EVIL;
         if (one_in_(5))
         {
            o_ptr->flags5 |= TR5_KILL_EVIL;
         }
         break;
		case 30:
			o_ptr->flags1 |= TR1_BRAND_FIRE;

			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 31:
			o_ptr->flags1 |= TR1_BRAND_COLD;

			if (!artifact_bias)
				artifact_bias = BIAS_COLD;
			break;
		case 32:
			o_ptr->flags1 |= TR1_BRAND_ELEC;

			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 33:
			o_ptr->flags1 |= TR1_BRAND_ACID;

			if (!artifact_bias)
				artifact_bias = BIAS_ACID;
			break;
		case 34:
			o_ptr->flags1 |= TR1_BRAND_POIS;

			if (!artifact_bias && !one_in_(3))
				artifact_bias = BIAS_POIS;
			else if (!artifact_bias && one_in_(6))
				artifact_bias = BIAS_NECROMANTIC;
			else if (!artifact_bias)
				artifact_bias = BIAS_ROGUE;
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
/*		case BIAS_ELEC:
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
         */
	}

	while (!type || (randint1(100) >= chance))
	{
/*  TEMP FIX  */
		type = randint1(181);
      chance = 75;
		switch (type)
		{
/*			case ACT_SUNLIGHT:
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
      */
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
	    (tval == TV_AMULET) ||
	    (tval == TV_RING))
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


bool create_artifact(object_type *o_ptr, bool a_scroll, bool store_made)
{
	object_type forge;
   object_type *to_ptr;
	char    new_name[1024];
	int     has_pval = 0;
	int     power_level;
	s32b    total_flags;
	bool    a_cursed = FALSE;
	int     warrior_artifact_bias = 0;
	int	  artifact_bias = 0;
   s32b    TotalPowerValue;
   int     checksloop;
   int     TPV;
   int     TPV2;
   int     BasePower,BasePowerRoll;
   bool    CheckCurse = FALSE;
   int     TotalArts;
   int     ArtPct[4];
   int     CanMake[4];
   bool    is_projectile = FALSE;

   /*  Rand Art Spreading Code */
   ArtPct[0] = 0;
   ArtPct[1] = 0;
   ArtPct[2] = 0;
   ArtPct[3] = 0;
   TotalArts = p_ptr->ArtLites + p_ptr->ArtWeapons +
               p_ptr->ArtArmor + p_ptr->ArtJewelry;
   if (TotalArts > 0)
   {
      ArtPct[0] = (int)((float)p_ptr->ArtLites / (float)TotalArts) * 100;
      ArtPct[1] = (int)((float)p_ptr->ArtWeapons / (float)TotalArts) * 100;
      ArtPct[2] = (int)((float)p_ptr->ArtArmor / (float)TotalArts) * 100;
      ArtPct[3] = (int)((float)p_ptr->ArtJewelry / (float)TotalArts) * 100;
   }

   if (ArtPct[0] <= ART_LITE_PCT) CanMake[0] = 1; else CanMake[0] = 0;
   if (ArtPct[1] <= ART_WEAPON_PCT) CanMake[1] = 1; else CanMake[1] = 0;
   if (ArtPct[2] <= ART_ARMOR_PCT) CanMake[2] = 1; else CanMake[2] = 0;
   if (ArtPct[3] <= ART_JEWELRY_PCT) CanMake[3] = 1; else CanMake[3] = 0;

   if (!(a_scroll) && !(store_made)) /*  Normally Generated Artifact Change Only  */
   {
      while(TRUE && TotalArts > 0)
      {
         /* use kind_is_match */
         object_wipe(o_ptr);
         make_object(o_ptr, 15, qreward_theme[p_ptr->pclass]);
         if(!(o_ptr->flags3 & TR3_INSTA_ART)) apply_magic(o_ptr, 10, 30, OC_FORCE_GOOD, FALSE);

         if (CanMake[0] && o_ptr->tval == TV_LITE)
         {
            p_ptr->ArtLites++;
            break;
         }
         if (CanMake[1] && o_ptr->tval >= TV_SHOT && o_ptr->tval <= TV_SWORD)
         {
            p_ptr->ArtWeapons++;
            break;
         }
         if (CanMake[2] && o_ptr->tval >= TV_BOOTS && o_ptr->tval <= TV_DRAG_ARMOR)
         {
            p_ptr->ArtArmor++;
            break;
         }
         if (CanMake[3] && (o_ptr->tval == TV_AMULET || o_ptr->tval == TV_RING))
         {
            p_ptr->ArtJewelry++;
            break;
         }
      }
   	o_ptr->cost = object_value_real(o_ptr);
      /*k_info[o_ptr->k_idx].cost + flag_cost(o_ptr, o_ptr->pval);*/
   }

   BasePower = 0;
   BasePowerRoll = 0;
   BasePowerRoll = randint1(100);
   TotalPowerValue = 0;
   checksloop = 0;
   TPV = 0;TPV2 = 0;

   BasePowerRoll += randint1(p_ptr->depth / 2);

   if (BasePowerRoll <= 33) BasePower = randint1(10);
   if (BasePowerRoll >= 34 && BasePowerRoll <= 66) BasePower = randint1(10) + 10;
   if (BasePowerRoll >= 67 && BasePowerRoll <= 87) BasePower = randint1(15) + 15;
   if (BasePowerRoll >= 88 && BasePowerRoll <= 93) BasePower = randint1(20) + 20;
   if (BasePowerRoll >= 94) BasePower = randint1(30) + 20;

	if (one_in_(WEIRD_LUCK)) BasePower += randint1(BasePower);

   BasePower += 5;

	to_ptr = &forge;

	if (o_ptr->flags3 & TR3_INSTA_ART)
   {
      if (one_in_(3)) a_cursed = TRUE;
   }

   TPV = randint1(200);  /*  1 - 200%  */

   if (TPV < 50) TPV = 50;

   if (a_scroll) TPV2 = randint1(p_ptr->max_depth)+BasePower;
                 else
                 TPV2 = randint1(BasePower)+2;

   if (TPV2 > 107) TPV2 = 107;
   if (TPV2 < 2) TPV2 = 2;

   TotalPowerValue = (int)(((float)TPV / 100.0) * (float)TPV2);

   if (TotalPowerValue < 2) TotalPowerValue = 2;
   if (TotalPowerValue > 214) TotalPowerValue = 214;

   if (one_in_(WEIRD_LUCK))
   {
      if (o_ptr->pval < 0) o_ptr->pval = -(o_ptr->pval);
   }

   if (o_ptr->pval < 0) o_ptr->pval = 0;
   if (o_ptr->to_a < 0) o_ptr->to_a = 0;
   if (o_ptr->to_d < 0) o_ptr->to_d = 0;
   if (o_ptr->to_a < 0) o_ptr->to_h = 0;

   if (o_ptr->tval == TV_LITE)
   {
      o_ptr->flags3 |= TR3_LITE;
      o_ptr->timeout = 0;
/*      o_ptr->pval = 0;*/
   }

	if (o_ptr->flags3 & TR3_CURSED)
   {
	   o_ptr->flags3 &= ~(TR3_CURSED);
      CheckCurse = TRUE;
   }

	if (o_ptr->flags3 & TR3_HEAVY_CURSE)
   {
		o_ptr->flags3 &= ~(TR3_HEAVY_CURSE);
      CheckCurse = TRUE;
   }

	if (o_ptr->flags3 & TR3_PERMA_CURSE)
   {
      CheckCurse = TRUE;
   }

	if (o_ptr->flags3 & TR3_TY_CURSE)
   {
      CheckCurse = TRUE;
   }

   if (CheckCurse && one_in_(25)) CheckCurse = FALSE;

   if (o_ptr->tval != TV_SHOT && o_ptr->tval != TV_BOLT && o_ptr->tval != TV_ARROW)
      TotalPowerValue *= 333;
      else
      TotalPowerValue *= 50;

   if (o_ptr->tval != TV_SHOT && o_ptr->tval != TV_BOLT && o_ptr->tval != TV_ARROW) is_projectile = FALSE;
   if (o_ptr->tval == TV_SHOT || o_ptr->tval == TV_BOLT || o_ptr->tval == TV_ARROW) is_projectile = TRUE;

   if (!(is_projectile) && TotalPowerValue < 5000) TotalPowerValue = 5000;
   if ((is_projectile) && TotalPowerValue < 1250) TotalPowerValue = 1250;

	/* Moria had no artifacts */
	if (ironman_moria) return (FALSE);

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

	if (!a_cursed && one_in_(WEIRD_LUCK))
		TotalPowerValue *= 2;

	if (a_cursed) TotalPowerValue /= 2;

   if (TotalPowerValue > 100000) TotalPowerValue = 100000;

	/* Main loop */
   if (cheat_peek)
   msg_format("TPV : %d  Cost : %d", TotalPowerValue, o_ptr->cost);

   checksloop = 0;
   while (checksloop < (BasePower * 2))
	{
      checksloop++;
      object_copy(to_ptr, o_ptr);

      TPV = randint1(4);

      if (TPV == 2)
      {
   		switch (randint1((to_ptr->tval < TV_BOOTS && to_ptr->tval > TV_BOLT) ? 7 : 5))
	   	{
		   	case 1: case 2:
               if (to_ptr->pval < 1) to_ptr->pval = 1;
			   	artifact_bias = random_plus(to_ptr, artifact_bias);
				   has_pval = 1;
   				break;
	   		case 3: case 4:
		   		artifact_bias = random_resistance(to_ptr, 0, artifact_bias);
			   	break;
   			case 5:
	   			artifact_bias = random_misc(to_ptr, artifact_bias);
		   		break;
			   case 6: case 7:
				   artifact_bias = random_slay(to_ptr, artifact_bias);
   				break;
	   	}
      }

      if (TPV == 3)
      {
   	   has_pval = 1;
         if (to_ptr->pval < 1) to_ptr->pval = 1;
         TPV2 = to_ptr->pval;
         if (to_ptr->flags1 & TR1_BLOWS)
         {
            if (one_in_(TPV2 * 5)) to_ptr->pval++;
         }
         else
         if (one_in_(TPV2 * 2)) to_ptr->pval++;
      }

      if (TPV == 1)
      {
      	/* give it some plusses... */
	      if (to_ptr->tval >= TV_BOOTS && to_ptr->tval <= TV_DRAG_ARMOR)
         {
            TPV2 = to_ptr->to_a;
            if (TPV2 < 1) { TPV2 = 1; to_ptr->to_a = 1; }
            if (TPV2 <= 10 && (one_in_(TPV2) || one_in_(5))) to_ptr->to_a++;
   		   if (TPV2 > 10 && TPV2 < 50 && (one_in_(TPV2 * 2) || one_in_(25))) to_ptr->to_a++;
         	if (one_in_(WEIRD_LUCK) && TPV2 < 20) to_ptr->to_a += 2;

            TPV2 = to_ptr->ac;
            if (TPV2 < 1) { TPV2 = 1; to_ptr->ac = 1; }
            if (TPV2 <= 10 && (one_in_(TPV2 * 2) || one_in_(25))) to_ptr->ac++;
   		   if (TPV2 > 10 && TPV2 < 50 && (one_in_(TPV2 * 4) || one_in_(50))) to_ptr->ac++;
         	if (one_in_(WEIRD_LUCK) && TPV2 < 20) to_ptr->ac += 2;
         }
      	else
	      {
            TPV2 = to_ptr->to_a;
            if (TPV2 < 1) { TPV2 = 1; to_ptr->to_a = 1; }
            if (TPV2 <= 20 && (one_in_(TPV2) || one_in_(5))) to_ptr->to_a++;
   		   if (TPV2 > 20 && TPV2 < 50 && (one_in_(TPV2 * 2) || one_in_(25))) to_ptr->to_a++;
         	if (one_in_(WEIRD_LUCK) && TPV2 < 20) to_ptr->to_a += 2;

            TPV2 = to_ptr->to_h;
            if (TPV2 < 1) { TPV2 = 1; to_ptr->to_h = 1; }
            if (TPV2 <= 20 && (one_in_(TPV2) || one_in_(5))) to_ptr->to_h++;
   		   if (TPV2 > 20 && TPV2 < 50 && (one_in_(TPV2 * 2) || one_in_(25))) to_ptr->to_h++;
         	if (one_in_(WEIRD_LUCK) && TPV2 < 20) to_ptr->to_h += 2;

            TPV2 = to_ptr->to_d;
            if (TPV2 < 1) { TPV2 = 1; to_ptr->to_d = 1; }
            if (TPV2 < 1) TPV2 = 1;
            if (TPV2 <= 20 && (one_in_(TPV2) || one_in_(5))) to_ptr->to_d++;
   		   if (TPV2 > 20 && TPV2 < 50 && (one_in_(TPV2 * 2) || one_in_(25))) to_ptr->to_d++;
         	if (one_in_(WEIRD_LUCK) && TPV2 < 20) to_ptr->to_d += 2;
	      }
      }

      if (TPV == 4 && to_ptr->tval >= TV_SHOT && to_ptr->tval <= TV_SWORD)
      {
      	if (to_ptr->dd && to_ptr->ds)
	      {
   		   if (one_in_(to_ptr->dd * 2) || one_in_(14)) to_ptr->dd++;
      		if (one_in_(to_ptr->ds * 2) || one_in_(14)) to_ptr->ds++;
      	}
      }

      identify_item(to_ptr);
		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);
   	to_ptr->ident |= (IDENT_MENTAL);

   	to_ptr->cost = k_info[to_ptr->k_idx].cost + flag_cost(to_ptr, to_ptr->pval);
/*   	to_ptr->cost = object_value_real(to_ptr);*/

      if (object_value_real(to_ptr) <= TotalPowerValue && object_value_real(to_ptr) <= 150000 && object_value_real(to_ptr)  >= 0)
      {
         object_wipe(o_ptr);
         object_copy(o_ptr, to_ptr);
      }
   };

   if (cheat_peek)
   msg_format("ArtVal : %d %d", o_ptr->cost, (long)object_value_real(o_ptr));

   if (o_ptr->activate == 0 && !(is_projectile))
   {

	   if (!a_cursed && one_in_((o_ptr->tval >= TV_BOOTS)
	      ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE))
   	{
	   	o_ptr->activate = 0;
		   give_activation_power(o_ptr, artifact_bias);
   	}
   }

	/* Just to be sure */
	o_ptr->flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
	                      TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	total_flags = flag_cost(o_ptr, o_ptr->pval);
	if (cheat_peek) msg_format("%ld", total_flags);

	if (a_cursed)
   {
      curse_artifact(o_ptr);
      if (o_ptr->dd > 0) o_ptr->dd = (int)((float)o_ptr->dd * 0.1);
      if (o_ptr->ds > 0) o_ptr->ds = (int)((float)o_ptr->ds * 0.1);
   }

	if (CheckCurse)
   {
      curse_artifact(o_ptr);
      if (o_ptr->dd > 0) o_ptr->dd = (int)((float)o_ptr->dd * 0.25);
      if (o_ptr->ds > 0) o_ptr->ds = (int)((float)o_ptr->ds * 0.25);
   }

	if (o_ptr->tval >= TV_BOOTS)
	{
		if (a_cursed) power_level = 0;
		else if (total_flags < 10000) power_level = 1;
		else if (total_flags < 20000) power_level = 2;
		else power_level = 3;
	}
	else
	{
		if (a_cursed) power_level = 0;
		else if (total_flags < 15000) power_level = 1;
		else if (total_flags < 30000) power_level = 2;
		else power_level = 3;
	}

	/* Set the cost */
   o_ptr->cost = 0;
	o_ptr->cost = k_info[o_ptr->k_idx].cost + flag_cost(o_ptr, o_ptr->pval);

   if (cheat_peek)
   msg_format("F ArtVal : %d %d", o_ptr->cost, (long)object_value_real(o_ptr));

	if (a_scroll)
	{
		char dummy_name[80];
		strcpy(dummy_name, "");
		(void)identify_fully_aux(o_ptr);
		o_ptr->ident |= IDENT_STOREB; /* This will be used later on... */
		if (!(get_string("What do you want to call the artifact? ", dummy_name, 80)))
		{
			if (!(is_projectile)) get_random_name(new_name, o_ptr->tval, power_level);
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

		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);

		/* Save all the known flags */
		o_ptr->kn_flags1 = o_ptr->flags1;
		o_ptr->kn_flags2 = o_ptr->flags2;
		o_ptr->kn_flags3 = o_ptr->flags3;
		o_ptr->kn_flags4 = o_ptr->flags4;
		o_ptr->kn_flags5 = o_ptr->flags5;
		o_ptr->kn_flags6 = o_ptr->flags6;
	}
	else
	{
		if (!(is_projectile)) get_random_name(new_name, o_ptr->tval, power_level);
      o_ptr->ident = 0;
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
	if (!(is_projectile)) o_ptr->xtra_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	/* Make the object an artifact */
	if (!(is_projectile)) o_ptr->flags3 |= TR3_INSTA_ART;

   if (o_ptr->cost > 0 && o_ptr->cost < 300000)
   {
      set_durability(o_ptr, QV_ARTIFICER);
   }
   else
   if (o_ptr->cost < 0)
   {
      set_durability(o_ptr, QV_ANY);
   }
   else
   {
      set_durability(o_ptr, QV_JUNK);
   }

   if (o_ptr->tval == TV_LITE) p_ptr->ArtLites++;
   if (o_ptr->tval >= TV_SHOT && o_ptr->tval <= TV_SWORD) p_ptr->ArtWeapons++;
   if (o_ptr->tval >= TV_BOOTS && o_ptr->tval <= TV_DRAG_ARMOR) p_ptr->ArtArmor++;
   if (o_ptr->tval == TV_AMULET || o_ptr->tval == TV_RING) p_ptr->ArtJewelry++;

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

	/* Get activation */
	activate = o_ptr->activate;

	/* Activate random artifacts and ego items */
	switch (o_ptr->activate)
	{
/*		case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("A line of sunlight appears.");
			(void)lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
			msg_print("It glows extremely brightly...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
			msg_print("It throbs deep green...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_POIS, dir, 25, 3);
			o_ptr->timeout = (s16b)rand_range(4, 8);
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msg_print("It is covered in sparks...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
			o_ptr->timeout = (s16b)rand_range(6, 12);
			break;
		}

		case ACT_BO_ACID_1:
		{
			msg_print("It is covered in acid...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
			o_ptr->timeout = (s16b)rand_range(5, 10);
			break;
		}

		case ACT_BO_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_COLD, dir, damroll(9, 8));
			o_ptr->timeout = (s16b)rand_range(7, 14);
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msg_print("It is covered in fire...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
			o_ptr->timeout = (s16b)rand_range(8, 16);
			break;
		}

		case ACT_BA_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
			msg_print("It glows an intense red...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 150, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 200))
			o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_BA_COLD_2:
		{
			msg_print("It glows an intense blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 200, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
			msg_print("It crackles with electricity...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 200, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)drain_life(dir, 250);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 100);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
			msg_print("It grows magical spikes...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ARROW, dir, 250);
			o_ptr->timeout = (s16b)rand_range(90, 180);
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msg_print("It glows deep red...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 250, 3);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_BA_COLD_3:
		{
			msg_print("It glows bright white...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 400, 3);
			o_ptr->timeout = (s16b)rand_range(325, 650);
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msg_print("It glows deep blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 500, 3);
			o_ptr->timeout = (s16b)rand_range(425, 850);
			break;
		}

		case ACT_WHIRLWIND:
		{
			{
				int y, x;
				cave_type       *c_ptr;
				monster_type    *m_ptr;

				for (dir = 0; dir <= 9; dir++)
				{
					y = py + ddy[dir];
					x = px + ddx[dir];

					if (!in_bounds2(y, x)) continue;

					c_ptr = area(y, x);

					m_ptr = &m_list[c_ptr->m_idx];

					if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
						py_attack(y, x);
				}
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_VAMPIRE_2:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 200);
			}

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
			msg_print("It glows in scintillating colours...");
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
			msg_print("It floods the area with goodness...");
			(void)dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_DISP_GOOD:
		{
			msg_print("It floods the area with evil...");
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


		case ACT_CONFUSE:
		{
			msg_print("It glows in scintillating colours...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)confuse_monster(dir, 50);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
			msg_print("It glows deep blue...");
			(void)sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
			(void)earthquake(py, px, 10);
			o_ptr->timeout = 50;
			break;
		}

		case ACT_TERROR:
		{
			(void)turn_monsters(40 + p_ptr->lev);
			o_ptr->timeout = 3 * (p_ptr->lev + 10);
			break;
		}

		case ACT_TELE_AWAY:
		{
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
			msg_print("It glows deep blue...");
			(void)genocide(TRUE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_MASS_GENO:
		{
			msg_print("It lets out a long, shrill note...");
			(void)mass_genocide(TRUE);
			o_ptr->timeout = 1000;
			break;
		}


		case ACT_CHARM_ANIMAL:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_animal(dir, plev);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_CHARM_UNDEAD:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)control_one_undead(dir, plev);
			o_ptr->timeout = 333;
			break;
		}

		case ACT_CHARM_OTHER:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_CHARM_ANIMALS:
		{
			(void)charm_animals(plev * 2);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_CHARM_OTHERS:
		{
			(void)charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_ANIMAL:
		{
			(void)summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 500);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msg_print("You summon a phantasmal servant.");
			(void)summon_specific(-1, py, px, p_ptr->depth, SUMMON_PHANTOM, TRUE, TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), SUMMON_ELEMENTAL, group, FALSE, pet))
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

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), SUMMON_DEMON, group, FALSE, pet))
			{
				msg_print("The area fills with a stench of sulphur and brimstone.");
				if (pet)
					msg_print("'What is thy bidding... Master?'");
				else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
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
				type = (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD);
				group = TRUE;
			}

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), type,
				                group, FALSE, pet))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				if (pet)
					msg_print("Ancient, long-dead forms arise from the ground to serve you!");
				else
					msg_print("'The dead arise... to punish you for disturbing them!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}


		case ACT_CURE_LW:
		{
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msg_print("It radiates deep purple...");
			(void)hp_player(75);
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = (s16b)rand_range(3, 6);
			break;
		}

		case ACT_CURE_POISON:
		{
			msg_print("It glows deep blue...");
			(void)set_afraid(0);
			(void)set_poisoned(0);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_REST_LIFE:
		{
			msg_print("It glows a deep red...");
			(void)restore_level();
			o_ptr->timeout = 450;
			break;
		}

		case ACT_REST_ALL:
		{
			msg_print("It glows a deep green...");
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
			msg_print("It glows deep blue...");
			msg_print("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)set_cut(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msg_print("It glows a bright white...");
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}


		case ACT_ESP:
		{
			(void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			(void)set_shero(p_ptr->shero + rand_range(50, 100));
			(void)set_blessed(p_ptr->blessed + rand_range(50, 100));
			o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msg_print("It lets out a shrill wail...");
			k = 3 * p_ptr->lev;
			(void)set_protevil(p_ptr->protevil + randint1(25) + k);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_RESIST_ALL:
		{
			msg_print("It glows many colours...");
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
			msg_print("It glows bright green...");
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
			msg_print("It glows brightly...");
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
			(void)set_wraith_form(p_ptr->wraith_form + rand_range(plev / 2, plev));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			(void)set_invuln(p_ptr->invuln + rand_range(8, 16));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_TELEPORT_1:
		{
			msg_print("It twists space around you...");
			teleport_player(100);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}


		case ACT_LIGHT:
		{
			msg_print("It wells with clear light...");
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(10, 20);
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msg_print("It shines brightly...");
			map_area();
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

		case ACT_DETECT_ALL:
		{
			msg_print("It glows bright white...");
			msg_print("An image forms in your mind...");
			(void)detect_all();
			o_ptr->timeout = (s16b)rand_range(55, 110);
			break;
		}

		case ACT_DETECT_XTRA:
		{
			msg_print("It glows brightly...");
			(void)detect_all();
			(void)probing();
			(void)identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
			msg_print("It glows yellow...");
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
			msg_print("It glows bright red...");
			(void)explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
			msg_print("It glows light blue...");
			(void)warding_glyph();
			o_ptr->timeout = 400;
			break;
		}

		case ACT_SATIATE:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_DEST_DOOR:
		{
			msg_print("It glows bright red...");
			(void)destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
			msg_print("It pulsates...");
			if (!get_aim_dir(&dir)) return FALSE;
			(void)wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
			(void)recharge(130);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
			msg_print("It glows bright yellow...");
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
			msg_print("It twists space around you...");
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
   */
	}

	return TRUE;
}

