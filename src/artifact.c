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
#define TABLE_NAME      50

#define A_CURSED        13
#define WEIRD_LUCK      12
#define BIAS_LUCK       20

/*
 * Bias luck needs to be higher than weird luck,
 * since it is usually tested several times...
 */
#define ACTIVATION_CHANCE 3


/*
 * Choose one random sustain
 */
void one_sustain(object_type *o_ptr)
{
	switch (randint0(6))
	{
		case 0: o_ptr->art_flags2 |= (TR2_SUST_STR); break;
		case 1: o_ptr->art_flags2 |= (TR2_SUST_INT); break;
		case 2: o_ptr->art_flags2 |= (TR2_SUST_WIS); break;
		case 3: o_ptr->art_flags2 |= (TR2_SUST_DEX); break;
		case 4: o_ptr->art_flags2 |= (TR2_SUST_CON); break;
		case 5: o_ptr->art_flags2 |= (TR2_SUST_CHR); break;
	}
}


/*
 * Choose one random high resistance
 */
void one_high_resistance(object_type *o_ptr)
{
#ifdef TINYANGBAND
	switch (randint0(7))
	{
		case  0: o_ptr->art_flags2 |= (TR2_RES_POIS);   break;
		case  1: o_ptr->art_flags2 |= (TR2_RES_DARK);   break;
		case  2: o_ptr->art_flags2 |= (TR2_RES_BLIND);  break;
		case  3: o_ptr->art_flags2 |= (TR2_RES_CONF);   break;
		case  4: o_ptr->art_flags2 |= (TR2_RES_SOUND);  break;
		case  5: o_ptr->art_flags2 |= (TR2_RES_NETHER); break;
		case  6: o_ptr->art_flags2 |= (TR2_RES_FEAR);   break;
	}
#else
	switch (randint0(12))
	{
		case  0: o_ptr->art_flags2 |= (TR2_RES_POIS);   break;
		case  1: o_ptr->art_flags2 |= (TR2_RES_LITE);   break;
		case  2: o_ptr->art_flags2 |= (TR2_RES_DARK);   break;
		case  3: o_ptr->art_flags2 |= (TR2_RES_SHARDS); break;
		case  4: o_ptr->art_flags2 |= (TR2_RES_BLIND);  break;
		case  5: o_ptr->art_flags2 |= (TR2_RES_CONF);   break;
		case  6: o_ptr->art_flags2 |= (TR2_RES_SOUND);  break;
		case  7: o_ptr->art_flags2 |= (TR2_RES_NETHER); break;
		case  8: o_ptr->art_flags2 |= (TR2_RES_NEXUS);  break;
		case  9: o_ptr->art_flags2 |= (TR2_RES_CHAOS);  break;
		case 10: o_ptr->art_flags2 |= (TR2_RES_DISEN);  break;
		case 11: o_ptr->art_flags2 |= (TR2_RES_FEAR);   break;
	}
#endif
}

/*
 * Choose one random high resistance ( except poison and disenchantment )
 */
void one_lordly_high_resistance(object_type *o_ptr)
{
#ifdef TINYANGBAND
	switch (randint0(6))
	{
		case 0: o_ptr->art_flags2 |= (TR2_RES_DARK);   break;
		case 1: o_ptr->art_flags2 |= (TR2_RES_BLIND);  break;
		case 2: o_ptr->art_flags2 |= (TR2_RES_CONF);   break;
		case 3: o_ptr->art_flags2 |= (TR2_RES_SOUND);  break;
		case 4: o_ptr->art_flags2 |= (TR2_RES_NETHER); break;
		case 5: o_ptr->art_flags2 |= (TR2_RES_FEAR);   break;
	}
#else
	switch (randint0(10))
	{
		case 0: o_ptr->art_flags2 |= (TR2_RES_LITE);   break;
		case 1: o_ptr->art_flags2 |= (TR2_RES_DARK);   break;
		case 2: o_ptr->art_flags2 |= (TR2_RES_SHARDS); break;
		case 3: o_ptr->art_flags2 |= (TR2_RES_BLIND);  break;
		case 4: o_ptr->art_flags2 |= (TR2_RES_CONF);   break;
		case 5: o_ptr->art_flags2 |= (TR2_RES_SOUND);  break;
		case 6: o_ptr->art_flags2 |= (TR2_RES_NETHER); break;
		case 7: o_ptr->art_flags2 |= (TR2_RES_NEXUS);  break;
		case 8: o_ptr->art_flags2 |= (TR2_RES_CHAOS);  break;
		case 9: o_ptr->art_flags2 |= (TR2_RES_FEAR);   break;
	}
#endif
}


/*
 * Choose one random element resistance
 */
void one_ele_resistance(object_type *o_ptr)
{
	switch (randint0(4))
	{
		case  0: o_ptr->art_flags2 |= (TR2_RES_ACID); break;
		case  1: o_ptr->art_flags2 |= (TR2_RES_ELEC); break;
		case  2: o_ptr->art_flags2 |= (TR2_RES_COLD); break;
		case  3: o_ptr->art_flags2 |= (TR2_RES_FIRE); break;
	}
}


/*
 * Choose one random element or poison resistance
 */
void one_dragon_ele_resistance(object_type *o_ptr)
{
	if (one_in_(7))
	{
		o_ptr->art_flags2 |= (TR2_RES_POIS);
	}
	else
	{
		one_ele_resistance(o_ptr);
	}
}


/*
 * Choose one random resistance
 */
void one_resistance(object_type *o_ptr)
{
	if (one_in_(3))
	{
		one_ele_resistance(o_ptr);
	}
	else
	{
		one_high_resistance(o_ptr);
	}
}


/*
 * Choose one random ability
 */
void one_ability(object_type *o_ptr)
{
	switch (randint0(9))
	{
		case 0: o_ptr->art_flags3 |= (TR3_FEATHER);     break;
		case 1: o_ptr->art_flags3 |= (TR3_LITE);        break;
		case 2: o_ptr->art_flags3 |= (TR3_SEE_INVIS);   break;
		case 3: o_ptr->art_flags3 |= (TR3_TELEPATHY);   break;
		case 4: o_ptr->art_flags3 |= (TR3_SLOW_DIGEST); break;
		case 5: o_ptr->art_flags3 |= (TR3_REGEN);       break;
		case 6: o_ptr->art_flags2 |= (TR2_FREE_ACT);    break;
		case 7: o_ptr->art_flags2 |= (TR2_HOLD_LIFE);   break;
		case 8: o_ptr->art_flags3 |= (TR3_WARNING);   break;
	}
}


static void curse_artifact(object_type * o_ptr)
{
	if (o_ptr->pval) o_ptr->pval = 0 - (o_ptr->pval + randint1(4));
	if (o_ptr->to_a) o_ptr->to_a = 0 - (o_ptr->to_a + randint1(4));
	if (o_ptr->to_h) o_ptr->to_h = 0 - (o_ptr->to_h + randint1(4));
	if (o_ptr->to_d) o_ptr->to_d = 0 - (o_ptr->to_d + randint1(4));

	o_ptr->art_flags3 |= (TR3_HEAVY_CURSE | TR3_CURSED);

	if (randint1(4) == 1) o_ptr->art_flags3 |= TR3_PERMA_CURSE;
	if (randint1(3) == 1) o_ptr->art_flags3 |= TR3_TY_CURSE;
	if (randint1(2) == 1) o_ptr->art_flags3 |= TR3_AGGRAVATE;
	if (randint1(3) == 1) o_ptr->art_flags3 |= TR3_DRAIN_EXP;
	if (randint1(2) == 1) o_ptr->art_flags3 |= TR3_TELEPORT;
	else if (randint1(3) == 1) o_ptr->art_flags3 |= TR3_NO_TELE;

	if ((p_ptr->pclass != CLASS_WARRIOR) && (randint1(3) == 1))
		o_ptr->art_flags3 |= TR3_NO_MAGIC;

	o_ptr->ident |= IDENT_CURSED;
}


void random_plus(object_type * o_ptr)
{
	int this_type = (o_ptr->tval < TV_BOOTS ? 24 : 20);

	switch (artifact_bias)
	{
	case BIAS_WARRIOR:
		if (!(o_ptr->art_flags1 & TR1_STR))
		{
			o_ptr->art_flags1 |= TR1_STR;
			if (randint1(2) == 1) return; /* 50% chance of being a "free" power */
		}
		if (!(o_ptr->art_flags1 & TR1_CON))
		{
			o_ptr->art_flags1 |= TR1_CON;
			if (randint1(2) == 1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_DEX))
		{
			o_ptr->art_flags1 |= TR1_DEX;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_MAGE:
		if (!(o_ptr->art_flags1 & TR1_INT))
		{
			o_ptr->art_flags1 |= TR1_INT;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_PRIESTLY:
		if (!(o_ptr->art_flags1 & TR1_WIS))
		{
			o_ptr->art_flags1 |= TR1_WIS;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_RANGER:
		if (!(o_ptr->art_flags1 & TR1_CON))
		{
			o_ptr->art_flags1 |= TR1_CON;
			if (randint1(2) == 1) return; /* 50% chance of being a "free" power */
		}
		if (!(o_ptr->art_flags1 & TR1_DEX))
		{
			o_ptr->art_flags1 |= TR1_DEX;
			if (randint1(2) == 1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_STR))
		{
			o_ptr->art_flags1 |= TR1_STR;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_ROGUE:
		if (!(o_ptr->art_flags1 & TR1_STEALTH))
		{
			o_ptr->art_flags1 |= TR1_STEALTH;
			if (randint1(2) == 1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_SEARCH))
		{
			o_ptr->art_flags1 |= TR1_SEARCH;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_STR:
		if (!(o_ptr->art_flags1 & TR1_STR))
		{
			o_ptr->art_flags1 |= TR1_STR;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_WIS:
		if (!(o_ptr->art_flags1 & TR1_WIS))
		{
			o_ptr->art_flags1 |= TR1_WIS;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_INT:
		if (!(o_ptr->art_flags1 & TR1_INT))
		{
			o_ptr->art_flags1 |= TR1_INT;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_DEX:
		if (!(o_ptr->art_flags1 & TR1_DEX))
		{
			o_ptr->art_flags1 |= TR1_DEX;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_CON:
		if (!(o_ptr->art_flags1 & TR1_CON))
		{
			o_ptr->art_flags1 |= TR1_CON;
			if (randint1(2) == 1) return;
		}
		break;

	case BIAS_CHR:
		if (!(o_ptr->art_flags1 & TR1_CHR))
		{
			o_ptr->art_flags1 |= TR1_CHR;
			if (randint1(2) == 1) return;
		}
		break;
	}

	if ((artifact_bias == BIAS_MAGE || artifact_bias == BIAS_PRIESTLY) && (o_ptr->tval == TV_SOFT_ARMOR) && (o_ptr->sval == SV_ROBE))
	{
		if (!(o_ptr->art_flags3 & TR3_DEC_MANA) && one_in_(3))
		{
			o_ptr->art_flags3 |= TR3_DEC_MANA;
			if (one_in_(2)) return;
		}
	}

	switch (randint1(this_type))
	{
	case 1: case 2:
		o_ptr->art_flags1 |= TR1_STR;
		if (!artifact_bias && randint1(13) != 1)
			artifact_bias = BIAS_STR;
		else if (!artifact_bias && randint1(7) == 1)
			artifact_bias = BIAS_WARRIOR;
		break;
	case 3: case 4:
		o_ptr->art_flags1 |= TR1_INT;
		if (!artifact_bias && randint1(13) != 1)
			artifact_bias = BIAS_INT;
		else if (!artifact_bias && randint1(7) == 1)
			artifact_bias = BIAS_MAGE;
		break;
	case 5: case 6:
		o_ptr->art_flags1 |= TR1_WIS;
		if (!artifact_bias && randint1(13) != 1)
			artifact_bias = BIAS_WIS;
		else if (!artifact_bias && randint1(7) == 1)
			artifact_bias = BIAS_PRIESTLY;
		break;
	case 7: case 8:
		o_ptr->art_flags1 |= TR1_DEX;
		if (!artifact_bias && randint1(13) != 1)
			artifact_bias = BIAS_DEX;
		else if (!artifact_bias && randint1(7) == 1)
			artifact_bias = BIAS_ROGUE;
		break;
	case 9: case 10:
		o_ptr->art_flags1 |= TR1_CON;
		if (!artifact_bias && randint1(13) != 1)
			artifact_bias = BIAS_CON;
		else if (!artifact_bias && randint1(9) == 1)
			artifact_bias = BIAS_RANGER;
		break;
	case 11: case 12:
		o_ptr->art_flags1 |= TR1_CHR;
		if (!artifact_bias && randint1(13) != 1)
			artifact_bias = BIAS_CHR;
		break;
	case 13: case 14:
		o_ptr->art_flags1 |= TR1_STEALTH;
		if (!artifact_bias && randint1(3) == 1)
			artifact_bias = BIAS_ROGUE;
		break;
	case 15: case 16:
		o_ptr->art_flags1 |= TR1_SEARCH;
		if (!artifact_bias && randint1(9) == 1)
			artifact_bias = BIAS_RANGER;
		break;
	case 17: case 18:
		o_ptr->art_flags1 |= TR1_INFRA;
		break;
	case 19:
		o_ptr->art_flags1 |= TR1_SPEED;
		if (!artifact_bias && randint1(11) == 1)
			artifact_bias = BIAS_ROGUE;
		break;
	case 20:
		o_ptr->art_flags1 |= TR1_MAGIC_MASTERY;
		if (!artifact_bias && randint1(11) == 1)
			artifact_bias = BIAS_MAGE;
		break;
	case 21: case 22:
		o_ptr->art_flags1 |= TR1_TUNNEL;
		break;
	case 23: case 24:
		if (o_ptr->tval == TV_BOW) random_plus(o_ptr);
		else
		{
			o_ptr->art_flags1 |= TR1_BLOWS;
			if (!artifact_bias && randint1(11) == 1)
				artifact_bias = BIAS_WARRIOR;
		}
		break;
	}
}

void one_plus(object_type *o_ptr)
{
	switch (randint0(9))
	{
	case 0: o_ptr->art_flags1 |= TR1_STR; break;
	case 1: o_ptr->art_flags1 |= TR1_INT; break;
	case 2: o_ptr->art_flags1 |= TR1_WIS; break;
	case 3: o_ptr->art_flags1 |= TR1_DEX; break;
	case 4: o_ptr->art_flags1 |= TR1_CON; break;
	case 5: o_ptr->art_flags1 |= TR1_CHR; break;
	case 6: o_ptr->art_flags1 |= TR1_SEARCH; break;
	case 7: o_ptr->art_flags1 |= TR1_INFRA; break;
	case 8: o_ptr->art_flags1 |= TR1_TUNNEL; break;
	}
}

static void random_resistance(object_type * o_ptr, int specific)
{
	if (!specific) /* To avoid a number of possible bugs */
	{
		if (artifact_bias == BIAS_ACID)
		{
			if (!(o_ptr->art_flags2 & TR2_RES_ACID))
			{
				o_ptr->art_flags2 |= TR2_RES_ACID;
				if (randint1(2) == 1) return;
			}
			if ((randint1(BIAS_LUCK) == 1) && !(o_ptr->art_flags2 & TR2_IM_ACID))
			{
				o_ptr->art_flags2 |= TR2_IM_ACID;
				if (randint1(2) == 1) return;
			}
		}
		else if (artifact_bias == BIAS_ELEC)
		{
			if (!(o_ptr->art_flags2 & TR2_RES_ELEC))
			{
				o_ptr->art_flags2 |= TR2_RES_ELEC;
				if (randint1(2) == 1) return;
			}
			if ((o_ptr->tval >= TV_CLOAK) && (o_ptr->tval <= TV_HARD_ARMOR) &&
			   !(o_ptr->art_flags3 & TR3_SH_ELEC))
			{
				o_ptr->art_flags3 |= TR3_SH_ELEC;
				if (randint1(2) == 1) return;
			}
			if (randint1(BIAS_LUCK) == 1 && !(o_ptr->art_flags2 & TR2_IM_ELEC))
			{
				o_ptr->art_flags2 |= TR2_IM_ELEC;
				if (randint1(2) == 1) return;
			}
		}
		else if (artifact_bias == BIAS_FIRE)
		{
			if (!(o_ptr->art_flags2 & TR2_RES_FIRE))
			{
				o_ptr->art_flags2 |= TR2_RES_FIRE;
				if (randint1(2) == 1) return;
			}
			if ((o_ptr->tval >= TV_CLOAK) &&
			    (o_ptr->tval <= TV_HARD_ARMOR) &&
			    !(o_ptr->art_flags3 & TR3_SH_FIRE))
			{
				o_ptr->art_flags3 |= TR3_SH_FIRE;
				if (randint1(2) == 1) return;
			}
			if ((randint1(BIAS_LUCK) == 1) &&
			    !(o_ptr->art_flags2 & TR2_IM_FIRE))
			{
				o_ptr->art_flags2 |= TR2_IM_FIRE;
				if (randint1(2) == 1) return;
			}
		}
		else if (artifact_bias == BIAS_COLD)
		{
			if (!(o_ptr->art_flags2 & TR2_RES_COLD))
			{
				o_ptr->art_flags2 |= TR2_RES_COLD;
				if (randint1(2) == 1) return;
			}
			if ((o_ptr->tval >= TV_CLOAK) &&
			    (o_ptr->tval <= TV_HARD_ARMOR) &&
			    !(o_ptr->art_flags3 & TR3_SH_COLD))
			{
				o_ptr->art_flags3 |= TR3_SH_COLD;
				if (randint1(2) == 1) return;
			}
			if (randint1(BIAS_LUCK) == 1 && !(o_ptr->art_flags2 & TR2_IM_COLD))
			{
				o_ptr->art_flags2 |= TR2_IM_COLD;
				if (randint1(2) == 1) return;
			}
		}
		else if (artifact_bias == BIAS_POIS)
		{
			if (!(o_ptr->art_flags2 & TR2_RES_POIS))
			{
				o_ptr->art_flags2 |= TR2_RES_POIS;
				if (randint1(2) == 1) return;
			}
		}
		else if (artifact_bias == BIAS_WARRIOR)
		{
			if (randint1(3) != 1 && (!(o_ptr->art_flags2 & TR2_RES_FEAR)))
			{
				o_ptr->art_flags2 |= TR2_RES_FEAR;
				if (randint1(2) == 1) return;
			}
			if ((randint1(3) == 1) && (!(o_ptr->art_flags3 & TR3_NO_MAGIC)))
			{
				o_ptr->art_flags3 |= TR3_NO_MAGIC;
				if (randint1(2) == 1) return;
			}
		}
		else if (artifact_bias == BIAS_NECROMANTIC)
		{
			if (!(o_ptr->art_flags2 & TR2_RES_NETHER))
			{
				o_ptr->art_flags2 |= TR2_RES_NETHER;
				if (randint1(2) == 1) return;
			}
			if (!(o_ptr->art_flags2 & TR2_RES_POIS))
			{
				o_ptr->art_flags2 |= TR2_RES_POIS;
				if (randint1(2) == 1) return;
			}
			if (!(o_ptr->art_flags2 & TR2_RES_DARK))
			{
				o_ptr->art_flags2 |= TR2_RES_DARK;
				if (randint1(2) == 1) return;
			}
		}
	}

	switch (specific ? specific : randint1(35))
	{
		case 1:
			if (randint1(WEIRD_LUCK) != 1)
				random_resistance(o_ptr, specific);
			else
			{
				o_ptr->art_flags2 |= TR2_IM_ACID;
				if (!artifact_bias)
					artifact_bias = BIAS_ACID;
			}
			break;
		case 2:
			if (randint1(WEIRD_LUCK) != 1)
				random_resistance(o_ptr, specific);
			else
			{
				o_ptr->art_flags2 |= TR2_IM_ELEC;
				if (!artifact_bias)
					artifact_bias = BIAS_ELEC;
			}
			break;
		case 3:
			if (randint1(WEIRD_LUCK) != 1)
				random_resistance(o_ptr, specific);
			else
			{
				o_ptr->art_flags2 |= TR2_IM_COLD;
				if (!artifact_bias)
					artifact_bias = BIAS_COLD;
			}
			break;
		case 4:
			if (randint1(WEIRD_LUCK) != 1)
				random_resistance(o_ptr, specific);
			else
			{
				o_ptr->art_flags2 |= TR2_IM_FIRE;
				if (!artifact_bias)
					artifact_bias = BIAS_FIRE;
			}
			break;
		case 5:
		case 6:
		case 13:
			o_ptr->art_flags2 |= TR2_RES_ACID;
			if (!artifact_bias)
				artifact_bias = BIAS_ACID;
			break;
		case 7:
		case 8:
		case 14:
			o_ptr->art_flags2 |= TR2_RES_ELEC;
			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 9:
		case 10:
		case 15:
			o_ptr->art_flags2 |= TR2_RES_FIRE;
			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 11:
		case 12:
		case 16:
			o_ptr->art_flags2 |= TR2_RES_COLD;
			if (!artifact_bias)
				artifact_bias = BIAS_COLD;
			break;
		case 17:
		case 18:
			o_ptr->art_flags2 |= TR2_RES_POIS;
			if (!artifact_bias && randint1(4) != 1)
				artifact_bias = BIAS_POIS;
			else if (!artifact_bias && randint1(2) == 1)
				artifact_bias = BIAS_NECROMANTIC;
			else if (!artifact_bias && randint1(2) == 1)
				artifact_bias = BIAS_ROGUE;
			break;
		case 19:
		case 20:
			o_ptr->art_flags2 |= TR2_RES_FEAR;
			if (!artifact_bias && randint1(3) == 1)
				artifact_bias = BIAS_WARRIOR;
			break;
		case 21:
		case 22:
			o_ptr->art_flags2 |= TR2_RES_DARK;
			break;
		case 23:
		case 24:
			o_ptr->art_flags2 |= TR2_RES_BLIND;
			break;
		case 25:
		case 26:
			o_ptr->art_flags2 |= TR2_RES_CONF;
			break;
		case 27:
		case 28:
			o_ptr->art_flags2 |= TR2_RES_SOUND;
			break;
		case 29:
		case 30:
			o_ptr->art_flags2 |= TR2_RES_NETHER;
			if (!artifact_bias && randint1(3) == 1)
				artifact_bias = BIAS_NECROMANTIC;
			break;
		case 31:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->art_flags3 |= TR3_SH_ELEC;
			else
				random_resistance(o_ptr, specific);
			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 32:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->art_flags3 |= TR3_SH_FIRE;
			else
				random_resistance(o_ptr, specific);
			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 33:
			if (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK ||
			    o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR)
				o_ptr->art_flags2 |= TR2_REFLECT;
			else
				random_resistance(o_ptr, specific);
			break;
		case 34:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->art_flags3 |= TR3_SH_COLD;
			else
				random_resistance(o_ptr, specific);
			if (!artifact_bias)
				artifact_bias = BIAS_COLD;
			break;
	}
}



static void random_misc(object_type * o_ptr)
{
	if (artifact_bias == BIAS_RANGER)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_CON))
		{
			o_ptr->art_flags2 |= TR2_SUST_CON;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_STR)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_STR))
		{
			o_ptr->art_flags2 |= TR2_SUST_STR;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_WIS)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_WIS))
		{
			o_ptr->art_flags2 |= TR2_SUST_WIS;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_INT)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_INT))
		{
			o_ptr->art_flags2 |= TR2_SUST_INT;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_DEX)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_DEX))
		{
			o_ptr->art_flags2 |= TR2_SUST_DEX;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_CON)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_CON))
		{
			o_ptr->art_flags2 |= TR2_SUST_CON;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_CHR)
	{
		if (!(o_ptr->art_flags2 & TR2_SUST_CHR))
		{
			o_ptr->art_flags2 |= TR2_SUST_CHR;
			if (randint1(2) == 1) return;
		}
	}
	else if (artifact_bias == BIAS_FIRE)
	{
		if (!(o_ptr->art_flags3 & TR3_LITE))
		{
			o_ptr->art_flags3 |= TR3_LITE; /* Freebie */
		}
	}

	switch (randint1(31))
	{
		case 1:
			o_ptr->art_flags2 |= TR2_SUST_STR;
			if (!artifact_bias)
				artifact_bias = BIAS_STR;
			break;
		case 2:
			o_ptr->art_flags2 |= TR2_SUST_INT;
			if (!artifact_bias)
				artifact_bias = BIAS_INT;
			break;
		case 3:
			o_ptr->art_flags2 |= TR2_SUST_WIS;
			if (!artifact_bias)
				artifact_bias = BIAS_WIS;
			break;
		case 4:
			o_ptr->art_flags2 |= TR2_SUST_DEX;
			if (!artifact_bias)
				artifact_bias = BIAS_DEX;
			break;
		case 5:
			o_ptr->art_flags2 |= TR2_SUST_CON;
			if (!artifact_bias)
				artifact_bias = BIAS_CON;
			break;
		case 6:
			o_ptr->art_flags2 |= TR2_SUST_CHR;
			if (!artifact_bias)
				artifact_bias = BIAS_CHR;
			break;
		case 7:
		case 8:
		case 14:
			o_ptr->art_flags2 |= TR2_FREE_ACT;
			break;
		case 9:
			o_ptr->art_flags2 |= TR2_HOLD_LIFE;
			if (!artifact_bias && (randint1(5) == 1))
				artifact_bias = BIAS_PRIESTLY;
			else if (!artifact_bias && (randint1(6) == 1))
				artifact_bias = BIAS_NECROMANTIC;
			break;
		case 10:
		case 11:
			o_ptr->art_flags3 |= TR3_LITE;
			break;
		case 12:
		case 13:
			o_ptr->art_flags3 |= TR3_FEATHER;
			break;
		case 15:
		case 16:
		case 17:
			o_ptr->art_flags3 |= TR3_SEE_INVIS;
			break;
		case 18:
			o_ptr->art_flags3 |= TR3_TELEPATHY;
			if (!artifact_bias && (randint1(9) == 1))
				artifact_bias = BIAS_MAGE;
			break;
		case 19:
		case 20:
			o_ptr->art_flags3 |= TR3_SLOW_DIGEST;
			break;
		case 21:
		case 22:
			o_ptr->art_flags3 |= TR3_REGEN;
			break;
		case 23:
			o_ptr->art_flags3 |= TR3_TELEPORT;
			break;
		case 24:
		case 25:
		case 26:
			if (o_ptr->tval >= TV_BOOTS && o_ptr->tval <= TV_DRAG_ARMOR)
				random_misc(o_ptr);
			else
				o_ptr->to_a = 4 + randint1(11);
			break;
		case 27:
			o_ptr->art_flags3 |= TR3_SHOW_MODS;
			o_ptr->to_h += 5 + damroll(2, 6);
			break;
		case 28:
			o_ptr->art_flags3 |= TR3_SHOW_MODS;
			o_ptr->to_d += 5 + damroll(2, 6);
			break;
		case 29:
			o_ptr->art_flags3 |= TR3_SHOW_MODS;
			o_ptr->to_h += 2 + damroll(2, 4);
			o_ptr->to_d += 2 + damroll(2, 4);
			break;
		case 30:
			o_ptr->art_flags3 |= TR3_NO_MAGIC;
			break;
		case 31:
			o_ptr->art_flags3 |= TR3_NO_TELE;
			break;
	}
}


static void random_slay(object_type *o_ptr)
{
	if (artifact_bias == BIAS_PRIESTLY &&
	   (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM) &&
	  !(o_ptr->art_flags3 & TR3_BLESSED))
	{
		/* A free power for "priestly" random artifacts */
		o_ptr->art_flags3 |= TR3_BLESSED;
	}

	else if (artifact_bias == BIAS_NECROMANTIC)
	{
		if (!(o_ptr->art_flags1 & TR1_VAMPIRIC) && (o_ptr->tval != TV_BOW))
		{
			o_ptr->art_flags1 |= TR1_VAMPIRIC;
			if (randint1(2) == 1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_BRAND_POIS) && (randint1(2) == 1))
		{
			o_ptr->art_flags1 |= TR1_BRAND_POIS;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_RANGER && (o_ptr->tval != TV_BOW))
	{
		if (!(o_ptr->art_flags1 & TR1_SLAY_ANIMAL))
		{
			o_ptr->art_flags1 |= TR1_SLAY_ANIMAL;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_ROGUE)
	{
		if ((((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DAGGER)) ||
		     ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_SPEAR)))
			 && (o_ptr->tval != TV_BOW) && !(o_ptr->art_flags2 & TR2_THROW))
		{
			/* Free power for rogues... */
			o_ptr->art_flags2 |= TR2_THROW;
		}
		if ((!(o_ptr->art_flags1 & TR1_BRAND_POIS)) && (randint1(2) == 1))
		{
			o_ptr->art_flags1 |= TR1_BRAND_POIS;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_POIS)
	{
		if (!(o_ptr->art_flags1 & TR1_BRAND_POIS))
		{
			o_ptr->art_flags1 |= TR1_BRAND_POIS;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_FIRE)
	{
		if (!(o_ptr->art_flags1 & TR1_BRAND_FIRE))
		{
			o_ptr->art_flags1 |= TR1_BRAND_FIRE;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_COLD)
	{
		if (!(o_ptr->art_flags1 & TR1_BRAND_COLD))
		{
			o_ptr->art_flags1 |= TR1_BRAND_COLD;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_ELEC)
	{
		if (!(o_ptr->art_flags1 & TR1_BRAND_ELEC))
		{
			o_ptr->art_flags1 |= TR1_BRAND_ELEC;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_ACID)
	{
		if (!(o_ptr->art_flags1 & TR1_BRAND_ACID))
		{
			o_ptr->art_flags1 |= TR1_BRAND_ACID;
			if (randint1(2) == 1) return;
		}
	}

	else if (artifact_bias == BIAS_LAW && (o_ptr->tval != TV_BOW))
	{
		if (!(o_ptr->art_flags1 & TR1_SLAY_EVIL))
		{
			o_ptr->art_flags1 |= TR1_SLAY_EVIL;
			if (randint1(2) == 1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_SLAY_UNDEAD))
		{
			o_ptr->art_flags1 |= TR1_SLAY_UNDEAD;
			if (randint1(2) == 1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_SLAY_DEMON))
		{
			o_ptr->art_flags1 |= TR1_SLAY_DEMON;
			if (randint1(2) == 1) return;
		}
	}

	if (o_ptr->tval != TV_BOW)
	{
		switch (randint1(36))
		{
		case 1:
		case 2:
			o_ptr->art_flags1 |= TR1_SLAY_ANIMAL;
			break;
		case 3:
		case 4:
			o_ptr->art_flags1 |= TR1_SLAY_EVIL;
			if (!artifact_bias && (randint1(2) == 1))
				artifact_bias = BIAS_LAW;
			else if (!artifact_bias && (randint1(9) == 1))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 5:
		case 6:
			o_ptr->art_flags1 |= TR1_SLAY_UNDEAD;
			if (!artifact_bias && (randint1(9) == 1))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 7:
		case 8:
			o_ptr->art_flags1 |= TR1_SLAY_DEMON;
			if (!artifact_bias && (randint1(9) == 1))
				artifact_bias = BIAS_PRIESTLY;
			break;
		case 9:
		case 10:
			o_ptr->art_flags1 |= TR1_SLAY_ORC;
			break;
		case 11:
		case 12:
			o_ptr->art_flags1 |= TR1_SLAY_TROLL;
			break;
		case 13:
		case 14:
			o_ptr->art_flags1 |= TR1_SLAY_GIANT;
			break;
		case 15:
		case 16:
			o_ptr->art_flags1 |= TR1_SLAY_DRAGON;
			break;
		case 17:
			o_ptr->art_flags1 |= TR1_KILL_DRAGON;
			break;
		case 18:
		case 19:
			if (o_ptr->tval == TV_SWORD)
			{
				o_ptr->art_flags1 |= TR1_VORPAL;
				if (!artifact_bias && (randint1(9) == 1))
					artifact_bias = BIAS_WARRIOR;
			}
			else
				random_slay(o_ptr);
			break;
		case 20:
			o_ptr->art_flags1 |= TR1_IMPACT;
			break;
		case 21:
		case 22:
			o_ptr->art_flags1 |= TR1_BRAND_FIRE;
			if (!artifact_bias)
				artifact_bias = BIAS_FIRE;
			break;
		case 23:
		case 24:
			o_ptr->art_flags1 |= TR1_BRAND_COLD;
			if (!artifact_bias)
				artifact_bias = BIAS_COLD;
			break;
		case 25:
		case 26:
			o_ptr->art_flags1 |= TR1_BRAND_ELEC;
			if (!artifact_bias)
				artifact_bias = BIAS_ELEC;
			break;
		case 27:
		case 28:
			o_ptr->art_flags1 |= TR1_BRAND_ACID;
			if (!artifact_bias)
				artifact_bias = BIAS_ACID;
			break;
		case 29:
		case 30:
			o_ptr->art_flags1 |= TR1_BRAND_POIS;
			if (!artifact_bias && (randint1(3) != 1))
				artifact_bias = BIAS_POIS;
			else if (!artifact_bias && randint1(6) == 1)
				artifact_bias = BIAS_NECROMANTIC;
			else if (!artifact_bias)
				artifact_bias = BIAS_ROGUE;
			break;
		case 31:
		case 32:
			o_ptr->art_flags1 |= TR1_VAMPIRIC;
			if (!artifact_bias)
				artifact_bias = BIAS_NECROMANTIC;
			break;
		case 33:
		case 34:
			o_ptr->art_flags1 |= TR1_SLAY_HUMAN;
		default:
			o_ptr->art_flags1 |= TR1_CHAOTIC;
			break;
		}
	}
	else
	{
		int rnd = (artifact_bias > 0) ? 6 : 9;
		switch (randint1(rnd))
		{
		case 1:
		case 2:
		case 3:
			o_ptr->art_flags3 |= TR3_XTRA_MIGHT;
			if (!artifact_bias && randint1(9) == 1)
				artifact_bias = BIAS_RANGER;
			break;
		case 4:
		case 5:
		case 6:
			o_ptr->art_flags3 |= TR3_XTRA_SHOTS;
			if (!artifact_bias && randint1(9) == 1)
				artifact_bias = BIAS_RANGER;
			break;
		default:
			switch(randint1(10)){
			case 1:
			case 2:
				o_ptr->art_flags1 |= TR1_BRAND_FIRE;
				if (!artifact_bias)
					artifact_bias = BIAS_FIRE;
				break;
			case 3:
			case 4:
				o_ptr->art_flags1 |= TR1_BRAND_COLD;
				if (!artifact_bias)
					artifact_bias = BIAS_COLD;
				break;
			case 5:
			case 6:
				o_ptr->art_flags1 |= TR1_BRAND_ELEC;
				if (!artifact_bias)
					artifact_bias = BIAS_ELEC;
				break;
			case 7:
			case 8:
				o_ptr->art_flags1 |= TR1_BRAND_ACID;
				if (!artifact_bias)
					artifact_bias = BIAS_ACID;
				break;
			default:
				o_ptr->art_flags1 |= TR1_BRAND_POIS;
				if (!artifact_bias && (randint1(3) != 1))
					artifact_bias = BIAS_POIS;
				else if (!artifact_bias && randint1(6) == 1)
					artifact_bias = BIAS_NECROMANTIC;
				else if (!artifact_bias)
					artifact_bias = BIAS_ROGUE;
				break;
			}
		}
	}
}


void one_brand(object_type *o_ptr)
{
	switch(randint0(6))
	{
	case 0: o_ptr->art_flags1 |= TR1_BRAND_ACID; break;
	case 1: o_ptr->art_flags1 |= TR1_BRAND_ELEC; break;
	case 2: o_ptr->art_flags1 |= TR1_BRAND_COLD; break;
	case 3: o_ptr->art_flags1 |= TR1_BRAND_FIRE; break;
	case 4: o_ptr->art_flags1 |= TR1_BRAND_POIS; break;
	default:
		switch(randint0(4))
		{
		case 0: o_ptr->art_flags1 |= TR1_CHAOTIC; break;
		case 1: o_ptr->art_flags1 |= TR1_IMPACT; break;
		case 2: o_ptr->art_flags1 |= TR1_VORPAL; break;
		default: o_ptr->art_flags1 |= TR1_VAMPIRIC; break;
		}
	}
}


void one_slay(object_type *o_ptr)
{
	switch(randint0(8))
	{
	case 0: o_ptr->art_flags1 |= TR1_SLAY_HUMAN; break;
	case 1: o_ptr->art_flags1 |= TR1_SLAY_ANIMAL; break;
	case 2: o_ptr->art_flags1 |= TR1_SLAY_UNDEAD; break;
	case 3: o_ptr->art_flags1 |= TR1_SLAY_DEMON; break;
	case 4: o_ptr->art_flags1 |= TR1_SLAY_ORC; break;
	case 5: o_ptr->art_flags1 |= TR1_SLAY_TROLL; break;
	case 6: o_ptr->art_flags1 |= TR1_SLAY_GIANT; break;
	case 7: o_ptr->art_flags1 |= ((one_in_(7)) ? TR1_KILL_DRAGON : TR1_SLAY_DRAGON); break;
	default: /* Evil slaying is not branded */
		o_ptr->art_flags1 |= TR1_SLAY_EVIL;
	}
}


void give_activation_power(object_type *o_ptr)
{
	int type = 0, chance = 0;

	if (artifact_bias)
	{
		if (artifact_bias == BIAS_ELEC)
		{
			if (randint1(3) != 1)
			{
				type = ACT_BO_ELEC_1;
			}
			else if (randint1(5) != 1)
			{
				type = ACT_BA_ELEC_2;
			}
			else
			{
				type = ACT_BA_ELEC_3;
			}
			chance = 101;
		}
		else if (artifact_bias == BIAS_POIS)
		{
			type = ACT_BA_POIS_1;
			chance = 101;
		}
		else if (artifact_bias == BIAS_FIRE)
		{
			if (randint1(3) != 1)
			{
				type = ACT_BO_FIRE_1;
			}
			else if (randint1(5) != 1)
			{
				type = ACT_BA_FIRE_1;
			}
			else
			{
				type = ACT_BA_FIRE_2;
			}
			chance = 101;
		}
		else if (artifact_bias == BIAS_COLD)
		{
			chance = 101;
			if (randint1(3) != 1)
				type = ACT_BO_COLD_1;
			else if (randint1(3) != 1)
				type = ACT_BA_COLD_1;
			else if (randint1(3) != 1)
				type = ACT_BA_COLD_2;
			else
				type = ACT_BA_COLD_3;
		}
		else if (artifact_bias == BIAS_PRIESTLY)
		{
			chance = 101;

			if (randint1(13) == 1)
				type = ACT_CHARM_UNDEAD;
			else if (randint1(12) == 1)
				type = ACT_BANISH_EVIL;
			else if (randint1(11) == 1)
				type = ACT_DISP_EVIL;
			else if (randint1(10) == 1)
				type = ACT_PROT_EVIL;
			else if (randint1(9) == 1)
				type = ACT_CURE_1000;
			else if (randint1(8) == 1)
				type = ACT_CURE_700;
			else if (randint1(7) == 1)
				type = ACT_REST_ALL;
			else if (randint1(6) == 1)
				type = ACT_REST_LIFE;
			else if (randint1(5) == 1)
				type = ACT_CURING;
			else
				type = ACT_CURE_MW;
		}
		else if (artifact_bias == BIAS_NECROMANTIC)
		{
			chance = 101;
			if (randint1(66) == 1)
				type = ACT_WRAITH;
			else if (randint1(13) == 1)
				type = ACT_DISP_GOOD;
			else if (randint1(9) == 1)
				type = ACT_MASS_GENO;
			else if (randint1(8) == 1)
				type = ACT_GENOCIDE;
			else if (randint1(13) == 1)
				type = ACT_SUMMON_UNDEAD;
			else if (randint1(9) == 1)
				type = ACT_VAMPIRE_2;
			else if (randint1(6) == 1)
				type = ACT_CHARM_UNDEAD;
			else
				type = ACT_VAMPIRE_1;
		}
		else if (artifact_bias == BIAS_LAW)
		{
			chance = 101;
			if (randint1(8) == 1)
				type = ACT_BANISH_EVIL;
			else if (randint1(4) == 1)
				type = ACT_DISP_EVIL;
			else
				type = ACT_PROT_EVIL;
		}
		else if (artifact_bias == BIAS_ROGUE)
		{
			chance = 101;
			if (randint1(50) == 1)
				type = ACT_SPEED;
			else if (randint1(4) == 1)
				type = ACT_SLEEP;
			else if (randint1(3) == 1)
				type = ACT_DETECT_ALL;
			else if (randint1(8) == 1)
				type = ACT_ID_FULL;
			else
				type = ACT_ID_PLAIN;
		}
		else if (artifact_bias == BIAS_MAGE)
		{
			chance = 66;
			if (randint1(20) == 1)
				type = ACT_SUMMON_ELEMENTAL;
			else if (randint1(10) == 1)
				type = ACT_SUMMON_PHANTOM;
			else if (randint1(5) == 1)
				type = ACT_RUNE_EXPLO;
			else
				type = ACT_ESP;
		}
		else if (artifact_bias == BIAS_WARRIOR)
		{
			chance = 80;
			if (randint1(100) == 1)
				type = ACT_INVULN;
			else
				type = ACT_BERSERK;
		}
		else if (artifact_bias == BIAS_RANGER)
		{
			chance = 101;
			if (randint1(20) == 1)
				type = ACT_CHARM_ANIMALS;
			else if (randint1(7) == 1)
				type = ACT_SUMMON_ANIMAL;
			else if (randint1(6) == 1)
				type = ACT_CHARM_ANIMAL;
			else if (randint1(4) == 1)
				type = ACT_RESIST_ALL;
			else if (randint1(3) == 1)
				type = ACT_SATIATE;
			else
				type = ACT_CURE_POISON;
		}
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
			case ACT_TELEPORT:
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
			case ACT_CURING:
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
	o_ptr->xtra2 = type;
	o_ptr->art_flags3 |= TR3_ACTIVATE;
	o_ptr->timeout = 0;
}


static void get_random_name(char *return_name, byte tval, int power)
{
	if ((tval == TV_AMULET) || (tval == TV_RING) || (tval == TV_LITE))
	{
		get_table_name(return_name);
	}
	else
	{
		get_table_sindarin(return_name);
	}
}


bool create_artifact(object_type *o_ptr, bool a_scroll)
{
	char    new_name[1024];
	int     has_pval = 0;
	int     powers = randint1(5) + 1;
	int     max_type = (o_ptr->tval < TV_BOOTS ? 7 : 5);
	int     power_level;
	s32b    total_flags;
	bool    a_cursed = FALSE;
	int     warrior_artifact_bias = 0;


	artifact_bias = 0;

	/* Nuke enchantments */
	o_ptr->name1 = 0;
	o_ptr->name2 = 0;

	/* Include obvious flags */
	o_ptr->art_flags1 |= k_info[o_ptr->k_idx].flags1;
	o_ptr->art_flags2 |= k_info[o_ptr->k_idx].flags2;
	o_ptr->art_flags3 |= k_info[o_ptr->k_idx].flags3;

	/* base item have pval? */
	if (o_ptr->pval) has_pval = TRUE;

	if (a_scroll && one_in_(4))
	{
		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR:
				artifact_bias = BIAS_WARRIOR;
				break;
			case CLASS_MAGE:
				artifact_bias = BIAS_MAGE;
				break;
			case CLASS_PRIEST:
				artifact_bias = BIAS_PRIESTLY;
				break;
			case CLASS_ARCHER:
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
		switch (randint1(max_type))
		{
			case 1: case 2:
				random_plus(o_ptr);
				has_pval = TRUE;
				break;
			case 3: case 4:
				random_resistance(o_ptr, FALSE);
				break;
			case 5:
				random_misc(o_ptr);
				break;
			case 6: case 7:
				random_slay(o_ptr);
				break;
			default:
#ifdef JP
				if (wizard) msg_print("Switch error in create_artifact!");
#else
				if (wizard) msg_print("Switch error in create_artifact!");
#endif
				powers++;
		}
	};

	/* Some Artifact Weapon gets extra dice */
	if (o_ptr->tval < TV_BOOTS)
	{
		if (!a_cursed) {
			if (one_in_(2 + (o_ptr->dd / 2))) o_ptr->dd++;
		}

		/* Hack -- Super-charge the damage dice */
		while (one_in_(10L * o_ptr->dd)) o_ptr->dd++;

		/* Hack -- Lower the damage dice */
		if (o_ptr->dd > 9) o_ptr->dd = 9;
	}

	if (has_pval)
	{
		if (o_ptr->art_flags1 & (TR1_BLOWS))
		{
			o_ptr->pval = randint1(2);
			if (one_in_(3) && ((o_ptr->dd * (o_ptr->ds + 1)) < 15)) o_ptr->pval++;
		}
		else
		{
			do
			{
				o_ptr->pval++;
			}
			while (o_ptr->pval < randint1(5) || randint1(o_ptr->pval) == 1);
		}

		if ((o_ptr->pval > 4) && (randint1(WEIRD_LUCK) != 1))
			o_ptr->pval = 4;
	}

	/* give it some plusses... ( except Amulets and Rings ) */
	if (o_ptr->tval >= TV_BOOTS && o_ptr->tval <= TV_DRAG_ARMOR)
		o_ptr->to_a += randint1(o_ptr->to_a > 19 ? 1 : 20 - o_ptr->to_a);
	else if (o_ptr->tval <= TV_SWORD)
	{
		o_ptr->to_h += randint1(o_ptr->to_h > 19 ? 1 : 20 - o_ptr->to_h);
		o_ptr->to_d += randint1(o_ptr->to_d > 19 ? 1 : 20 - o_ptr->to_d);
	}

	/* Just to be sure */
	o_ptr->art_flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
			      TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	total_flags = flag_cost(o_ptr, o_ptr->pval);
	if (cheat_peek) msg_format("%ld", total_flags);

	if (a_cursed) curse_artifact(o_ptr);

	if (o_ptr->tval == TV_LITE && o_ptr->sval == SV_LITE_FEANOR)
	{
		artifact_bias = 0;
		o_ptr->xtra2 = 0;
		give_activation_power(o_ptr);
	}
	else if (!a_cursed &&
	    (randint1((o_ptr->tval >= TV_BOOTS)
	    ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE) == 1))
	{
		o_ptr->xtra2 = 0;
		give_activation_power(o_ptr);
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

	if (a_scroll)
	{
		char dummy_name[80];
		strcpy(dummy_name, "");
		(void)identify_fully_aux(o_ptr, TRUE);
		o_ptr->ident |= IDENT_STORE; /* This will be used later on... */
#ifdef JP
		if (!(get_string("このアーティファクトを何と名付けますか？", dummy_name, 80)))
#else
		if (!(get_string("What do you want to call the artifact? ", dummy_name, 80)))
#endif
		{
			get_random_name(new_name, o_ptr->tval, power_level);
		}
		else
		{
#ifdef JP
			strcpy(new_name, "《");
#else
			strcpy(new_name, "'");
#endif
			strcat(new_name, dummy_name);
#ifdef JP
			strcat(new_name, "》という名の");
#else
			strcat(new_name, "'");
#endif
		}
		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}
	else
	{
		get_random_name(new_name, o_ptr->tval, power_level);
	}

	if (cheat_xtra)
	{
		if (artifact_bias)
#ifdef JP
			msg_format("運の偏ったアーティファクト: %d。", artifact_bias);
#else
			msg_format("Biased artifact: %d.", artifact_bias);
#endif
		else
#ifdef JP
			msg_print("アーティファクトに運の偏りなし。");
#else
			msg_print("No bias in artifact.");
#endif
	}

	/* Save the inscription */
	o_ptr->art_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	return TRUE;
}


bool activate_random_artifact(object_type * o_ptr)
{
	int plev = p_ptr->lev;
	int k, dir, dummy;

	if (!o_ptr->xtra2) return FALSE;

	/* Activate for attack */
	switch (o_ptr->xtra2)
	{
		case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
			msg_print("太陽光線が放たれた。");
#else
			msg_print("A line of sunlight appears.");
#endif
			(void)lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
#ifdef JP
			msg_print("それは眩しいくらいに明るく輝いた...");
#else
			msg_print("It glows extremely brightly...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_MISSILE, dir, damroll(2, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
#ifdef JP
			msg_print("それは濃緑色に脈動した...");
#else
			msg_print("It throbs deep green...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, 12, 3);
			o_ptr->timeout = randint0(4) + 4;
			break;
		}

		case ACT_BO_ELEC_1:
		{
#ifdef JP
			msg_print("それは火花に覆われた...");
#else
			msg_print("It is covered in sparks...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ELEC, dir, damroll(4, 8));
			o_ptr->timeout = randint0(6) + 6;
			break;
		}

		case ACT_BO_ACID_1:
		{
#ifdef JP
			msg_print("それは酸に覆われた...");
#else
			msg_print("It is covered in acid...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ACID, dir, damroll(5, 8));
			o_ptr->timeout = randint0(5) + 5;
			break;
		}

		case ACT_BO_COLD_1:
		{
#ifdef JP
			msg_print("それは冷気に覆われた...");
#else
			msg_print("It is covered in frost...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_COLD, dir, damroll(6, 8));
			o_ptr->timeout = randint0(7) + 7;
			break;
		}

		case ACT_BO_FIRE_1:
		{
#ifdef JP
			msg_print("それは炎に覆われた...");
#else
			msg_print("It is covered in fire...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(9, 8));
			o_ptr->timeout = randint0(8) + 8;
			break;
		}

		case ACT_BA_COLD_1:
		{
#ifdef JP
			msg_print("それは冷気に覆われた...");
#else
			msg_print("It is covered in frost...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 48, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
#ifdef JP
			msg_print("それは赤く激しく輝いた...");
#else
			msg_print("It glows an intense red...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 72, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
#ifdef JP
			msg_print("それは黒く輝いた...");
#else
			msg_print("It glows black...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 90))
			o_ptr->timeout = 70;
			break;
		}

		case ACT_BA_COLD_2:
		{
#ifdef JP
			msg_print("それは青く激しく輝いた...");
#else
			msg_print("It glows an intense blue...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
#ifdef JP
			msg_print("電気がパチパチ音を立てた...");
#else
			msg_print("It crackles with electricity...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 100, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
#ifdef JP
			msg_print("それは黒く輝いた...");
#else
			msg_print("It glows black...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			drain_life(dir, 120);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
#ifdef JP
			msg_print("それは黒く輝いた...");
#else
			msg_print("It glows black...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 50))
				hp_player(50);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
#ifdef JP
			msg_print("魔法のトゲが現れた...");
#else
			msg_print("It grows magical spikes...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ARROW, dir, 150);
			o_ptr->timeout = randint0(90) + 90;
			break;
		}

		case ACT_BA_FIRE_2:
		{
#ifdef JP
			msg_print("それは深赤色に輝いた...");
#else
			msg_print("It glows deep red...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 120, 3);
			o_ptr->timeout = randint0(225) + 225;
			break;
		}

		case ACT_BA_COLD_3:
		{
#ifdef JP
			msg_print("それは明白色に輝いた...");
#else
			msg_print("It glows bright white...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 200, 3);
			o_ptr->timeout = randint0(325) + 325;
			break;
		}

		case ACT_BA_ELEC_3:
		{
#ifdef JP
			msg_print("それは深青色に輝いた...");
#else
			msg_print("It glows deep blue...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 250, 3);
			o_ptr->timeout = randint0(425) + 425;
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
					c_ptr = &cave[y][x];

					/* Get the monster */
					m_ptr = &m_list[c_ptr->m_idx];

					/* Hack -- attack monsters */
					if (c_ptr->m_idx && (m_ptr->ml || cave_floor_bold(y, x)))
						py_attack(y, x);
				}
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_VAMPIRE_2:
		{
#ifdef JP
			msg_print("それは黒く輝いた...");
#else
			msg_print("It glows black...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 100))
				hp_player(100);
			}

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
#ifdef JP
			msg_print("それは様々な色の火花に包まれた...");
#else
			msg_print("It glows in scintillating colours...");
#endif
			call_chaos();
			o_ptr->timeout = 350;
			break;
		}

		case ACT_ROCKET:
		{
			if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
			msg_print("ロケットを発射した！");
#else
			msg_print("You launch a rocket!");
#endif
 			sound(SOUND_MISS); /* (Sound substitute) HACK! No rocket sound available, use arrow miss */ 
			fire_ball(GF_ROCKET, dir, 120 + plev, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
#ifdef JP
			msg_print("神聖な雰囲気が充満した...");
#else
			msg_print("It floods the area with goodness...");
#endif
			dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = randint0(200) + 200;
			break;
		}

		case ACT_DISP_GOOD:
		{
#ifdef JP
			msg_print("邪悪な雰囲気が充満した...");
#else
			msg_print("It floods the area with evil...");
#endif
			dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = randint0(200) + 200;
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
#ifdef JP
			msg_print("あなたはエレメントのブレスを吐いた。");
#else
			msg_print("You breathe the elements.");
#endif
			sound(SOUND_BR_ELEMENTS);
			fire_ball(GF_MISSILE, dir, 300, -4);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_PESTICIDE:
		{
#ifdef JP
			msg_print("あなたは害虫を一掃した。");
#else
			msg_print("You exterminate small life.");
#endif
			(void)dispel_monsters(4);
			o_ptr->timeout = randint0(55) + 55;
			break;
		}

		/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
#ifdef JP
			msg_print("それは様々な色の火花に包まれた...");
#else
			msg_print("It glows in scintillating colours...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			confuse_monster(dir, 20);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
#ifdef JP
			msg_print("それは深青色に輝いた...");
#else
			msg_print("It glows deep blue...");
#endif
			sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
#ifdef JP
			msg_print("それは鼓動した...");
#else
			msg_print("It pulsates...");
#endif
			earthquake(py, px, 10);
			o_ptr->timeout = 50;
			break;
		}

		case ACT_FEAR:
		{
#ifdef JP
			msg_print("おどろおどろしい音が鳴り渡った...");
#else
			msg_print("You blow a mighty blast; your enemies tremble!");
#endif
			(void)turn_monsters((3 * p_ptr->lev / 2) + 10);
			o_ptr->timeout = randint0(40) + 40;
			break;
		}

		case ACT_TERROR:
		{
#ifdef JP
			msg_print("おどろおどろしい音が鳴り渡った...");
#else
			msg_print("You blow a mighty blast; your enemies tremble!");
#endif
			turn_monsters(40 + p_ptr->lev);
			o_ptr->timeout = 3 * (p_ptr->lev + 10);
			break;
		}

		case ACT_TELE_AWAY:
		{
#ifdef JP
			msg_print("それは深紫色に輝いた...");
#else
			msg_print("It glows deep purple...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BANISH_EVIL:
		{
			if (banish_evil(100))
			{
#ifdef JP
				msg_print("アーティファクトの力が邪悪を打ち払った！");
#else
				msg_print("The power of the artifact banishes evil!");
#endif
			}
			o_ptr->timeout = 250 + randint1(250);
			break;
		}

		case ACT_GENOCIDE:
		{
#ifdef JP
			msg_print("それは深青色に輝いた...");
#else
			msg_print("It glows deep blue...");
#endif
			(void)genocide(200);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_MASS_GENO:
		{
#ifdef JP
			msg_print("ひどく鋭い音が流れ出た...");
#else
			msg_print("It lets out a long, shrill note...");
#endif
			(void)mass_genocide(200);
			o_ptr->timeout = 1000;
			break;
		}

		/* Activate for summoning / charming */

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
			charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_ANIMAL:
		{
			(void)summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, TRUE, TRUE, TRUE);
			o_ptr->timeout = 200 + randint1(300);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
#ifdef JP
			msg_print("幻霊を召喚した。");
#else
			msg_print("You summon a phantasmal servant.");
#endif
			(void)summon_specific(-1, py, px, dun_level, SUMMON_PHANTOM, TRUE, TRUE, TRUE);
			o_ptr->timeout = 200 + randint1(200);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = (randint1(3) == 1);
			bool group = !(pet && (plev < 50));

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), SUMMON_ELEMENTAL, group, FALSE, pet))
			{
#ifdef JP
				msg_print("エレメンタルが現れた...");
				if (pet) msg_print("あなたに服従しているようだ。");
				else msg_print("それをコントロールできなかった！");
#else
				msg_print("An elemental materializes...");
				if (pet) msg_print("It seems obedient to you.");
				else msg_print("You fail to control it!");
#endif
			}

			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			bool pet = (randint1(3) == 1);
			bool group = !(pet && (plev < 50));

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), SUMMON_DEMON, group, FALSE, pet))
			{
#ifdef JP
				msg_print("硫黄の悪臭が充満した。");
				if (pet) msg_print("「ご用でございますか、ご主人様」");
				else msg_print("「卑しき者よ、我は汝の下僕にあらず！魂を頂くぞ！」");
#else
				msg_print("The area fills with a stench of sulphur and brimstone.");
				if (pet) msg_print("'What is thy bidding... Master?'");
				else msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
#endif
			}

			o_ptr->timeout = 666 + randint1(333);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			bool pet = (randint1(3) == 1);
			bool group;
			int type;

			if (pet)
			{
				type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);
				group = (((plev > 24) && (randint1(3) == 1)) ? TRUE : FALSE);
			}
			else
			{
				type = (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD);
				group = TRUE;
			}

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), type,
						group, FALSE, pet))
			{
#ifdef JP
				msg_print("冷たい風があなたの周りに吹き始めた。それは腐敗臭を運んでいる...");
				if (pet) msg_print("古えの死せる者共があなたに仕えるため土から甦った！");
				else msg_print("死者が甦った。眠りを妨げるあなたを罰するために！");
#else
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
				if (pet) msg_print("Ancient, long-dead forms arise from the ground to serve you!");
				else msg_print("'The dead arise... to punish you for disturbing them!'");
#endif
			}

			o_ptr->timeout = 666 + randint1(333);
			break;
		}

		/* Activate for healing */

		case ACT_CURE_LW:
		{
#ifdef JP
			msg_print("それは薄紫色の光を放射した...");
#else
			msg_print("It radiates light purple...");
#endif
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
#ifdef JP
			msg_print("それは深紫色の光を放射した...");
#else
			msg_print("It radiates deep purple...");
#endif
			hp_player(damroll(4, 8));
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = randint0(3) + 3;
			break;
		}

		case ACT_CURE_POISON:
		{
#ifdef JP
			msg_print("それは深青色に輝いた...");
#else
			msg_print("It glows deep blue...");
#endif
			(void)set_afraid(0);
			(void)set_poisoned(0);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_REST_LIFE:
		{
#ifdef JP
			msg_print("それは深紅に輝いた...");
#else
			msg_print("It glows a deep red...");
#endif
			restore_level();
			o_ptr->timeout = 450;
			break;
		}

		case ACT_REST_ALL:
		{
#ifdef JP
			msg_print("それは濃緑色に輝いた...");
#else
			msg_print("It glows a deep green...");
#endif
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
#ifdef JP
			msg_print("それは深青色に輝いた...");
			msg_print("体内に暖かい鼓動が感じられる...");
#else
			msg_print("It glows deep blue...");
			msg_print("You feel a warm tingling inside...");
#endif
			(void)hp_player(700);
			(void)set_cut(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
#ifdef JP
			msg_print("それは白く明るく輝いた...");
			msg_print("ひじょうに気分がよい...");
#else
			msg_print("It glows a bright white...");
			msg_print("You feel much better...");
#endif
			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}

		case ACT_CURING:
		{
#ifdef JP
			msg_print("それは眩しいくらいに輝いた...");
#else
			msg_print("It glows extremely brightly...");
#endif
			(void)hp_player(50);
			(void)set_poisoned(0);
			(void)set_confused(0);
			(void)set_blind(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_image(0);

			o_ptr->timeout = 100;
			break;
		}

		/* Activate for timed effect */

		case ACT_ESP:
		{
			(void)set_tim_esp(p_ptr->tim_esp + randint1(30) + 25);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			(void)set_shero(p_ptr->shero + randint1(50) + 50);
			(void)set_blessed(p_ptr->blessed + randint1(50) + 50);
			o_ptr->timeout = 100 + randint1(100);
			break;
		}

		case ACT_PROT_EVIL:
		{
#ifdef JP
			msg_print("鋭い音が流れ出た...");
#else
			msg_print("It lets out a shrill wail...");
#endif
			k = 3 * p_ptr->lev;
			(void)set_protevil(p_ptr->protevil + randint1(25) + k);
			o_ptr->timeout = randint0(225) + 225;
			break;
		}

		case ACT_RESIST_ALL:
		{
#ifdef JP
			msg_print("それは様々な色に輝いた...");
#else
			msg_print("It glows many colours...");
#endif
			(void)set_oppose_acid(p_ptr->oppose_acid + randint1(40) + 40);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint1(40) + 40);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint1(40) + 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint1(40) + 40);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint1(40) + 40);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
#ifdef JP
			msg_print("それは明るく緑色に輝いた...");
#else
			msg_print("It glows bright green...");
#endif
			if (!p_ptr->fast)
			{
				(void)set_fast(randint1(20) + 20);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = randint0(100) + 100;
			break;
		}

		case ACT_XTRA_SPEED:
		{
#ifdef JP
			msg_print("それは明るく輝いた...");
#else
			msg_print("It glows brightly...");
#endif
			if (!p_ptr->fast)
			{
				(void)set_fast(randint1(75) + 75);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = randint0(200) + 200;
			break;
		}

		case ACT_WRAITH:
		{
			set_wraith_form(p_ptr->tim_wraith + randint1(plev / 2) + (plev / 2));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			(void)set_invuln(p_ptr->invuln + randint1(8) + 8);
			o_ptr->timeout = 1000;
			break;
		}

		/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
#ifdef JP
			msg_print("澄んだ光があふれ出た...");
#else
			msg_print("It wells with clear light...");
#endif
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = randint0(10) + 10;
			break;
		}

		case ACT_MAP_LIGHT:
		{
#ifdef JP
			msg_print("それは眩しく輝いた...");
#else
			msg_print("It shines brightly...");
#endif
			map_area(DETECT_RAD_MAP);
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = randint0(50) + 50;
			break;
		}

		case ACT_DETECT_ALL:
		{
#ifdef JP
			msg_print("それは白く明るく輝いた...");
			msg_print("心にイメージが浮かんできた...");
#else
			msg_print("It glows bright white...");
			msg_print("An image forms in your mind...");
#endif
			detect_all(DETECT_RAD_DEFAULT);
			o_ptr->timeout = randint0(55) + 55;
			break;
		}

		case ACT_DETECT_XTRA:
		{
#ifdef JP
			msg_print("それは明るく輝いた...");
#else
			msg_print("It glows brightly...");
#endif
			detect_all(DETECT_RAD_DEFAULT);
			probing();
			(void)identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
#ifdef JP
			msg_print("それは黄色く輝いた...");
#else
			msg_print("It glows yellow...");
#endif
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
#ifdef JP
			msg_print("それは明赤色に輝いた...");
#else
			msg_print("It glows bright red...");
#endif
			explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
#ifdef JP
			msg_print("それは明青色に輝いた...");
#else
			msg_print("It glows light blue...");
#endif
			warding_glyph();
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
#ifdef JP
			msg_print("それは明赤色に輝いた...");
#else
			msg_print("It glows bright red...");
#endif
			destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
#ifdef JP
			msg_print("それは鼓動した...");
#else
			msg_print("It pulsates...");
#endif
			if (!get_aim_dir(&dir)) return FALSE;
			wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
#ifdef JP
			msg_print("それは明黄色に輝いた...");
#else
			msg_print("It glows bright yellow...");
#endif
			recharge(130);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
#ifdef JP
			msg_print("それは明黄色に輝いた...");
#else
			msg_print("It glows bright yellow...");
#endif
			(void)alchemy();
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DIM_DOOR:
		{
#ifdef JP
			msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
			msg_print("You open a dimensional gate. Choose a destination.");
#endif
			if (!dimension_door()) return FALSE;
			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT:
		{
#ifdef JP
			msg_print("周りの空間が歪んだ...");
#else
			msg_print("It twists space around you...");
#endif
			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}

		case ACT_RECALL:
		{
			if (ironman_downward)
			{
#ifdef JP
				msg_print("それは一瞬輝いたがすぐ元に戻った。");
#else
				msg_print("It glows and then fades.");
#endif
			}
			else
			{
#ifdef JP
				msg_print("それは乳白色に輝いた...");
#else
				msg_print("It glows soft white...");
#endif
				(void)recall_player(randint0(21) + 15);
				o_ptr->timeout = 200;
			}
			break;
		}

		default:
		{
#ifdef JP
			msg_format("Unknown activation effect: %d.", o_ptr->xtra2);
#else
			msg_format("Unknown activation effect: %d.", o_ptr->xtra2);
#endif
			return FALSE;
		}
	}

	return TRUE;
}


void random_artifact_resistance(object_type * o_ptr, artifact_type *a_ptr)
{
	bool give_resistance = FALSE, give_power = FALSE;

	if (a_ptr->gen_flags & (TRG_XTRA_POWER)) give_power = TRUE;
	if (a_ptr->gen_flags & (TRG_XTRA_H_RES)) give_resistance = TRUE;
	if (a_ptr->gen_flags & (TRG_XTRA_RES_OR_POWER))
	{
		/* Give a resistance OR a power */
		if (one_in_(2)) give_resistance = TRUE;
		else give_power = TRUE;
	}

	if (give_power) one_ability(o_ptr);
	if (give_resistance) one_high_resistance(o_ptr);
}


/*
 * Create the artifact of the specified number
 */
void create_named_art(int a_idx, int y, int x)
{
	object_type forge;
	object_type *q_ptr;
	int i;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Get local object */
	q_ptr = &forge;

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return;

	/* Create the artifact */
	object_prep(q_ptr, i);

	/* Save the name */
	q_ptr->name1 = a_idx;

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->xtra2 = a_ptr->activate;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Hack -- acquire "cursed" flag */
	if (a_ptr->flags3 & TR3_CURSED) q_ptr->ident |= (IDENT_CURSED);

	random_artifact_resistance(q_ptr, a_ptr);

	/* Drop the artifact from heaven */
	(void)drop_near(q_ptr, -1, y, x);
}

/*
 *  named ego
 */
void create_named_ego(object_type *o_ptr)
{
	int max_type = (o_ptr->tval < TV_BOOTS ? 7 : 5);
	char buf[80];
	bool got_random_plus;
	bool has_blows;
	bool has_act;
	u32b o1, o2, o3;
	u32b f1, f2, f3;

	/* Paranoia */
	if (o_ptr->tval <= TV_CORPSE || o_ptr->tval >= TV_LITE) return;

	/* Prevent Ammo */
	if (o_ptr->tval <= TV_BOLT && o_ptr->tval >= TV_SHOT) return;

	/* Save old flags */
	object_flags(o_ptr, &o1, &o2, &o3);

	/* Does it has extra blows */
	has_blows = (o1 & (TR1_BLOWS)) ? TRUE : FALSE;

	/* Does it has activation */
	has_act = (o3 & (TR3_ACTIVATE)) ? TRUE : FALSE;

	/* Add one extra power */
	do
	{
		/* Do not use bias */
		artifact_bias = 0;

		got_random_plus = FALSE;

		/* Get one random power */
		switch (randint1(max_type))
		{
			case 1: case 2:
				random_plus(o_ptr);
				got_random_plus = TRUE;
				break;
			case 3: case 4:
				random_resistance(o_ptr, FALSE);
				break;	
			case 5:
				random_misc(o_ptr);
				break;
			case 6: case 7:
				random_slay(o_ptr);
				break;
			default:
#ifdef JP
				if (wizard) msg_print("銘付エゴの能力追加で不正が発生しました！");
#else
				if (wizard) msg_print("Switch error in create_named_ego!");
#endif
		}

		object_flags(o_ptr, &f1, &f2, &f3);
	}
	/* Check extra power is added truely */
	while ((o1 == f1) && (o2 == f2) && (o3 == f3));

	/* Tweak pval */
	if (got_random_plus)
	{
		/* If don't have pval, gain some pval */
		if (o_ptr->pval == 0)
		{
			o_ptr->pval = 2;
			while (o_ptr->pval < randint1(5) || one_in_(o_ptr->pval)) o_ptr->pval++;
		}

		/* Limit Extra Attacks */
		if (!has_blows && (o_ptr->art_flags1 & (TR1_BLOWS)))
		{
			o_ptr->pval = randint1(2);
			if (one_in_(3) && ((o_ptr->dd * (o_ptr->ds + 1)) < 15)) o_ptr->pval++;
		}

		if ((o_ptr->pval > 4) && (randint1(WEIRD_LUCK) != 1))
			o_ptr->pval = 4;
	}

	/* Add some bonuses to hit and dam (Weapons only)*/
	if (o_ptr->tval < TV_BOOTS)
	{
		o_ptr->to_h += (u16b)randint0(5);
		o_ptr->to_d += (u16b)randint0(5);
	}

	/* Add some bonuses to AC (Armors only) */
	if (o_ptr->tval >= TV_BOOTS)
	{
		o_ptr->to_a += (u16b)randint0(5);
	}

	/* Do not use bias */
	artifact_bias = 0;

	/* Get random activation */
	if (!has_act && one_in_(7))
	{
		o_ptr->xtra2 = 0;
		give_activation_power(o_ptr);
	}

	/* get random name */
	get_table_name(buf);

	/* Set name */
	o_ptr->ego_name = quark_add(buf);
}


bool create_nazgul_ring(object_type *o_ptr)
{
	char    new_name[1024];
#ifdef TINYANGBAND
	int     powers = randint1(3) + 1;
#else
	int     powers = randint1(5) + 1;
#endif

	/* paranoia */
	if ((o_ptr->tval != TV_RING) || (o_ptr->sval != SV_RING_WRAITH))
	{
		return FALSE;
	}

	switch(randint1(3))
	{
		case 1:
			artifact_bias = BIAS_NECROMANTIC;
			break;
		case 2:
			artifact_bias = BIAS_MAGE;
			break;
		default:
			artifact_bias = 0;
	}

	/* Nuke enchantments */
	o_ptr->name1 = 0;
	o_ptr->name2 = 0;

	/* Include obvious flags */
	o_ptr->art_flags1 |= k_info[o_ptr->k_idx].flags1;
	o_ptr->art_flags2 |= k_info[o_ptr->k_idx].flags2;
	o_ptr->art_flags3 |= k_info[o_ptr->k_idx].flags3;

	strcpy(new_name, "");

	while (one_in_(powers) || one_in_(7) || one_in_(10))
		powers++;

	if (one_in_(WEIRD_LUCK)) powers *= 2;

	/* Main loop */
	while (powers--)
	{
		switch (randint1(3))
		{
			case 1:
				random_plus(o_ptr);
				break;
			case 2:
				random_resistance(o_ptr, FALSE);
				break;
			case 3:
				random_misc(o_ptr);
				break;
			default:
#ifdef JP
				if (wizard) msg_print("Switch error in create_nazgul_ring!");
#else
				if (wizard) msg_print("Switch error in create_nazgul_ring!");
#endif
				powers++;
		}
	};

	/* Paranoia */
	o_ptr->art_flags1 &= ~(TR1_BLOWS);

	/* Just to be sure */
	o_ptr->art_flags1 |= (TR1_STR | TR1_INT | TR1_WIS | TR1_DEX | TR1_CON | TR1_CHR);
	o_ptr->art_flags3 |= (TR3_HIDE_TYPE | TR3_SHOW_MODS | TR3_SEE_INVIS);

	/* Get some pluses */
	if (one_in_(WEIRD_LUCK)) o_ptr->art_flags1 |= (TR1_SPEED);

	/* give it some plusses... */
	o_ptr->pval = (one_in_(5)) ? 2 : 1;
	o_ptr->to_a = 0;
	o_ptr->to_h = randint0(3) + 10;
	o_ptr->to_d = o_ptr->to_h;

	/* Just to be sure */
	o_ptr->art_flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC | TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	/* Give one activation */
	o_ptr->xtra2 = 0;
	give_activation_power(o_ptr);

	/* Nazgul rings are always cursed */
	if (one_in_(3)) o_ptr->art_flags3 |= TR3_PERMA_CURSE;
	else o_ptr->art_flags3 |= TR3_HEAVY_CURSE;
	o_ptr->ident |= IDENT_CURSED;

	/* Give some cursed effects */
	o_ptr->art_flags3 |= TR3_DRAIN_EXP;
	if (one_in_(3)) o_ptr->art_flags3 |= TR3_AGGRAVATE;
	if (one_in_(5)) o_ptr->art_flags3 |= TR3_TY_CURSE;
	if (one_in_(10)) o_ptr->art_flags3 |= TR3_NO_TELE;

	/* Get wraith form */
	if (one_in_(666)) o_ptr->art_flags3 |= TR3_WRAITH;

	/* Save the inscription */
	get_table_bad_sindarin(new_name);
	o_ptr->art_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	return TRUE;
}
