/* File: artifact.c */

/*
 * Purpose: some artifact code
 *
 * Included functions, taken from spells2.c, object2.c and cmd6.c:
 *
 * curse_artifact() random_plus() random_resistance() random_misc()
 * random_slay() give_activation_power() get_random_name() create_artifact()
 * artifact_scroll() item_tester_hook_activate() ring_of_power()
 * brand_bolts() activate_random_artifact() do_cmd_activate()
 * item_activation() random_artifact_resistance()
 *
 * wiz_create_named_art() remains in wizard2.c.
 *
 */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#define TABLE_NAME		45
#define A_CURSED		13
#define WEIRD_LUCK		12
#define BIAS_LUCK		20
#define ACTIVATION_CHANCE	3

int artifact_bias;


void curse_artifact(object_type * o_ptr)
{
	if (o_ptr->pval) o_ptr->pval = 0 - ((o_ptr->pval) + randint(4));
	if (o_ptr->to_a) o_ptr->to_a = 0 - ((o_ptr->to_a) + randint(4));
	if (o_ptr->to_h) o_ptr->to_h = 0 - ((o_ptr->to_h) + randint(4));
	if (o_ptr->to_d) o_ptr->to_d = 0 - ((o_ptr->to_d) + randint(4));

	o_ptr->art_flags3 |= (TR3_HEAVY_CURSE | TR3_CURSED);

	if (randint(4)==1) o_ptr->art_flags3 |= TR3_PERMA_CURSE;
	if (randint(3)==1) o_ptr->art_flags3 |= TR3_TY_CURSE;
	if (randint(2)==1) o_ptr->art_flags3 |= TR3_AGGRAVATE;
	if (randint(3)==1) o_ptr->art_flags3 |= TR3_DRAIN_EXP;

	if (randint(2)==1)
		o_ptr->art_flags3 |= TR3_TELEPORT;
	else if (randint(3)==1)
		o_ptr->art_flags3 |= TR3_NO_TELE;

	if (p_ptr->pclass != (CLASS_WARRIOR || CLASS_WEAPONMASTER) &&
			     (randint(3)==1))
		o_ptr->art_flags3 |= TR3_NO_MAGIC;

	o_ptr->ident |= IDENT_CURSED;
}


void random_plus(object_type * o_ptr, bool is_scroll)
{
	int this_type = (o_ptr->tval<TV_BOOTS?23:19);

	if (artifact_bias == BIAS_WARRIOR)
	{
		if (!(o_ptr->art_flags1 & TR1_STR))
		{
			o_ptr->art_flags1 |= TR1_STR;
			if (randint(2)==1) return; /* 50% chance of being a "free" power */
		}

		if (!(o_ptr->art_flags1 & TR1_CON))
		{
			o_ptr->art_flags1 |= TR1_CON;
			if (randint(2)==1) return;
		}

		if (!(o_ptr->art_flags1 & TR1_DEX))
		{
			o_ptr->art_flags1 |= TR1_DEX;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_MAGE)
	{
		if (!(o_ptr->art_flags1 & TR1_INT))
		{
			o_ptr->art_flags1 |= TR1_INT;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_PRIESTLY)
	{
		if (!(o_ptr->art_flags1 & TR1_WIS))
		{
			o_ptr->art_flags1 |= TR1_WIS;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_RANGER)
	{
		if (!(o_ptr->art_flags1 & TR1_CON))
		{
			o_ptr->art_flags1 |= TR1_CON;
			if (randint(2)==1) return; /* 50% chance of being a "free" power */
		}

		if (!(o_ptr->art_flags1 & TR1_DEX))
		{
			o_ptr->art_flags1 |= TR1_DEX;
			if (randint(2)==1) return;
		}

		if (!(o_ptr->art_flags1 & TR1_STR))
		{
			o_ptr->art_flags1 |= TR1_STR;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_ROGUE)
	{
		if (!(o_ptr->art_flags1 & TR1_STEALTH))
		{
			o_ptr->art_flags1 |= TR1_STEALTH;
			if (randint(2)==1) return;
		}
		if (!(o_ptr->art_flags1 & TR1_SEARCH))
		{
			o_ptr->art_flags1 |= TR1_SEARCH;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_STR)
	{
		if (!(o_ptr->art_flags1 & TR1_STR))
		{
			o_ptr->art_flags1 |= TR1_STR;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_WIS)
	{
		if (!(o_ptr->art_flags1 & TR1_WIS))
		{
			o_ptr->art_flags1 |= TR1_WIS;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_INT)
	{
		if (!(o_ptr->art_flags1 & TR1_INT))
		{
			o_ptr->art_flags1 |= TR1_INT;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_DEX)
	{
		if (!(o_ptr->art_flags1 & TR1_DEX))
		{
			o_ptr->art_flags1 |= TR1_DEX;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_CON)
	{
		if (!(o_ptr->art_flags1 & TR1_CON))
		{
			o_ptr->art_flags1 |= TR1_CON;
			if (randint(2)==1) return;
		}
	}
	else if (artifact_bias == BIAS_CHR)
	{
		if (!(o_ptr->art_flags1 & TR1_CHR))
		{
			o_ptr->art_flags1 |= TR1_CHR;
			if (randint(2)==1) return;
		}
	}

	switch (randint(this_type))
	{
	case 1: case 2:
		o_ptr->art_flags1 |= TR1_STR;
		if (!(artifact_bias) && randint(13)!=1)
			artifact_bias = BIAS_STR;
		else if (!(artifact_bias) && randint(7)==1)
			artifact_bias = BIAS_WARRIOR;
		break;
	case 3: case 4:
		o_ptr->art_flags1 |= TR1_INT;
		if (!(artifact_bias) && randint(13)!=1)
			artifact_bias = BIAS_INT;
		else if (!(artifact_bias) && randint(7)==1)
			artifact_bias = BIAS_MAGE;
		break;
	case 5: case 6:
		o_ptr->art_flags1 |= TR1_WIS;
		if (!(artifact_bias) && randint(13)!=1)
			artifact_bias = BIAS_WIS;
		else if (!(artifact_bias) && randint(7)==1)
			artifact_bias = BIAS_PRIESTLY;
		break;
	case 7: case 8:
		o_ptr->art_flags1 |= TR1_DEX;
		if (!(artifact_bias) && randint(13)!=1)
			artifact_bias = BIAS_DEX;
		else if (!(artifact_bias) && randint(7)==1)
			artifact_bias = BIAS_ROGUE;
		break;
	case 9: case 10:
		o_ptr->art_flags1 |= TR1_CON;
		if (!(artifact_bias) && randint(13)!=1)
			artifact_bias = BIAS_CON;
		else if (!(artifact_bias) && randint(9)==1)
			artifact_bias = BIAS_RANGER;
		break;
	case 11: case 12:
		o_ptr->art_flags1 |= TR1_CHR;
		if (!(artifact_bias) && randint(13)!=1)
			artifact_bias = BIAS_CHR;
		break;
	case 13: case 14:
		o_ptr->art_flags1 |= TR1_STEALTH;
		if (!(artifact_bias) && randint(3)==1)
			artifact_bias = BIAS_ROGUE;
		break;
	case 15: case 16:
		o_ptr->art_flags1 |= TR1_SEARCH;
		if (!(artifact_bias) && randint(9)==1)
			artifact_bias = BIAS_RANGER;
		break;
	case 17: case 18:
		o_ptr->art_flags1 |= TR1_INFRA;
		break;
	case 19:
		o_ptr->art_flags1 |= TR1_SPEED;
		if (!(artifact_bias) && randint(11)==1)
			artifact_bias = BIAS_ROGUE;
		break;
	case 20: case 21:
		o_ptr->art_flags1 |= TR1_TUNNEL;
		break;
	case 22: case 23:
		if (o_ptr->tval == TV_BOW) random_plus(o_ptr, is_scroll);
		else
		{
			o_ptr->art_flags1 |= TR1_BLOWS;
			if (!(artifact_bias) && randint(11)==1)
				artifact_bias = BIAS_WARRIOR;
		}
		break;
	}
}


void random_resistance(object_type * o_ptr, bool is_scroll, int specific)
{
  if (!specific) /* To avoid a number of possible bugs */
  {
    if (artifact_bias == BIAS_ACID)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_ACID))
	{
	    o_ptr->art_flags2 |= TR2_RES_ACID;
	    if (randint(2)==1) return;
	}
    if (randint(BIAS_LUCK)==1 && !(o_ptr->art_flags2 & TR2_IM_ACID))
	{
	    o_ptr->art_flags2 |= TR2_IM_ACID;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_ELEC)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_ELEC))
	{
	    o_ptr->art_flags2 |= TR2_RES_ELEC;
	    if (randint(2)==1) return;
	}
    if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR &&
        ! (o_ptr->art_flags3 & TR3_SH_ELEC))
        {
            o_ptr->art_flags2 |= TR3_SH_ELEC;
            if (randint(2)==1) return;
        }
    if (randint(BIAS_LUCK)==1 && !(o_ptr->art_flags2 & TR2_IM_ELEC))
	{
	    o_ptr->art_flags2 |= TR2_IM_ELEC;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_FIRE)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_FIRE))
	{
	    o_ptr->art_flags2 |= TR2_RES_FIRE;
	    if (randint(2)==1) return;
	}
    if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR &&
        ! (o_ptr->art_flags3 & TR3_SH_FIRE))
        {
            o_ptr->art_flags2 |= TR3_SH_FIRE;
            if (randint(2)==1) return;
        }
    if (randint(BIAS_LUCK)==1 && !(o_ptr->art_flags2 & TR2_IM_FIRE))
	{
	    o_ptr->art_flags2 |= TR2_IM_FIRE;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_COLD)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_COLD))
	{
	    o_ptr->art_flags2 |= TR2_RES_COLD;
	    if (randint(2)==1) return;
	}
    if (randint(BIAS_LUCK)==1 && !(o_ptr->art_flags2 & TR2_IM_COLD))
	{
	    o_ptr->art_flags2 |= TR2_IM_COLD;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_POIS)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_POIS))
	{
	    o_ptr->art_flags2 |= TR2_RES_POIS;
	    if (randint(2)==1) return;
	}
	if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR &&
           !(o_ptr->art_flags3 & TR3_SPINES))
        {
            o_ptr->art_flags2 |= TR3_SPINES;
            if (randint(2)==1) return;
        }
    }
    else if (artifact_bias == BIAS_WARRIOR)
    {
	if (randint(3)!=1 && (!(o_ptr->art_flags2 & TR2_RES_FEAR)))
	{
	    o_ptr->art_flags2 |= TR2_RES_FEAR;
	    if (randint(2)==1) return;
	}
    if ((randint(3)==1) && (!(o_ptr->art_flags3 & TR3_NO_MAGIC)))
    {
        o_ptr->art_flags3 |= TR3_NO_MAGIC;
        if (randint(2)==1) return;
    }
    }
    else if (artifact_bias == BIAS_NECROMANTIC)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_NETHER))
	{
	    o_ptr->art_flags2 |= TR2_RES_NETHER;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags2 & TR2_RES_POIS))
	{
	    o_ptr->art_flags2 |= TR2_RES_POIS;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags2 & TR2_RES_DARK))
	{
	    o_ptr->art_flags2 |= TR2_RES_DARK;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_CHAOS)
    {
	if (!(o_ptr->art_flags2 & TR2_RES_CHAOS))
	{
	    o_ptr->art_flags2 |= TR2_RES_CHAOS;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags2 & TR2_RES_CONF))
	{
	    o_ptr->art_flags2 |= TR2_RES_CONF;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags2 & TR2_RES_DISEN))
	{
	    o_ptr->art_flags2 |= TR2_RES_DISEN;
	    if (randint(2)==1) return;
	}
    }
  }

    switch (specific?specific:randint(42))
    {
    case 1:
    if (randint(WEIRD_LUCK)!=1)
        random_resistance(o_ptr, is_scroll, specific);
	else
	{
	o_ptr->art_flags2 |= TR2_IM_ACID;
	if (!(artifact_bias)) artifact_bias = BIAS_ACID;
	}
	break;
    case 2:
    if (randint(WEIRD_LUCK)!=1)
	    random_resistance(o_ptr, is_scroll, specific);
	else
	{
	o_ptr->art_flags2 |= TR2_IM_ELEC;
	if (!(artifact_bias)) artifact_bias = BIAS_ELEC;
	}
	break;
    case 3:
	if (randint(WEIRD_LUCK)!=1)
		random_resistance(o_ptr, is_scroll, specific);
	else
	{
	o_ptr->art_flags2 |= TR2_IM_COLD;
	if (!(artifact_bias)) artifact_bias = BIAS_COLD;
	}
	break;
    case 4:
    if (randint(WEIRD_LUCK)!=1)
	    random_resistance(o_ptr, is_scroll, specific);
	else
	{
	o_ptr->art_flags2 |= TR2_IM_FIRE;
	if (!(artifact_bias)) artifact_bias = BIAS_FIRE;
	}
	break;
    case 5: case 6: case 13:
	o_ptr->art_flags2 |= TR2_RES_ACID;
	if (!(artifact_bias)) artifact_bias = BIAS_ACID;
	break;
    case 7: case 8: case 14:
	o_ptr->art_flags2 |= TR2_RES_ELEC;
    if (!(artifact_bias)) artifact_bias = BIAS_ELEC;
	break;
    case 9: case 10: case 15:
	o_ptr->art_flags2 |= TR2_RES_FIRE;
	if (!(artifact_bias)) artifact_bias = BIAS_FIRE;
	break;
    case 11: case 12: case 16:
	o_ptr->art_flags2 |= TR2_RES_COLD;
	if (!(artifact_bias)) artifact_bias = BIAS_COLD;
	break;
    case 17: case 18:
	o_ptr->art_flags2 |= TR2_RES_POIS;
	if (!(artifact_bias) && randint(4)!=1) artifact_bias = BIAS_POIS;
	else if (!(artifact_bias) && randint(2)==1) artifact_bias = BIAS_NECROMANTIC;
	else if (!(artifact_bias) && randint(2)==1) artifact_bias = BIAS_ROGUE;
	break;
    case 19: case 20:
	o_ptr->art_flags2 |= TR2_RES_FEAR;
	if (!(artifact_bias) && randint(3)==1) artifact_bias = BIAS_WARRIOR;
	break;
    case 21:
	o_ptr->art_flags2 |= TR2_RES_LITE;
	break;
    case 22:
	o_ptr->art_flags2 |= TR2_RES_DARK;
	break;
    case 23: case 24:
	o_ptr->art_flags2 |= TR2_RES_BLIND;
	break;
    case 25: case 26:
	o_ptr->art_flags2 |= TR2_RES_CONF;
	if (!(artifact_bias) && randint(6)==1) artifact_bias = BIAS_CHAOS;
	break;
    case 27: case 28:
	o_ptr->art_flags2 |= TR2_RES_SOUND;
	break;
    case 29: case 30:
	o_ptr->art_flags2 |= TR2_RES_SHARDS;
	break;
    case 31: case 32:
	o_ptr->art_flags2 |= TR2_RES_NETHER;
	if (!(artifact_bias) && randint(3)==1) artifact_bias = BIAS_NECROMANTIC;
	break;
    case 33: case 34:
	o_ptr->art_flags2 |= TR2_RES_NEXUS;
	break;
    case 35: case 36:
	o_ptr->art_flags2 |= TR2_RES_CHAOS;
	if (!(artifact_bias) && randint(2)==1) artifact_bias = BIAS_CHAOS;
	break;
    case 37: case 38:
	o_ptr->art_flags2 |= TR2_RES_DISEN;
	break;
    case 39:
    if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
        o_ptr->art_flags3 |= TR3_SH_ELEC;
    else
	    random_resistance(o_ptr, is_scroll, specific);
    if (!(artifact_bias)) artifact_bias = BIAS_ELEC;
    break;
    case 40:
    if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
        o_ptr->art_flags3 |= TR3_SH_FIRE;
    else
	    random_resistance(o_ptr, is_scroll, specific);
    if (!(artifact_bias)) artifact_bias = BIAS_FIRE;
    break;
    case 41:
	if ((o_ptr->tval >= TV_CLOAK) && (o_ptr->tval <= TV_HARD_ARMOR))
		o_ptr->art_flags3 |= TR3_SPINES;
	else
		random_resistance(o_ptr, is_scroll, specific);
	if (!(artifact_bias)) artifact_bias = BIAS_POIS;
	break;
    case 42:
    if (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK ||
        o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR)
        o_ptr->art_flags2 |= TR2_REFLECT;
    else
	    random_resistance(o_ptr, is_scroll, specific);
    break;
    }
}


void random_misc(object_type * o_ptr, bool is_scroll)
{

    if (artifact_bias == BIAS_RANGER)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_CON))
	{
	    o_ptr->art_flags2 |= TR2_SUST_CON;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_STR)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_STR))
	{
	    o_ptr->art_flags2 |= TR2_SUST_STR;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_WIS)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_WIS))
	{
	    o_ptr->art_flags2 |= TR2_SUST_WIS;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_INT)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_INT))
	{
	    o_ptr->art_flags2 |= TR2_SUST_INT;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_DEX)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_DEX))
	{
	    o_ptr->art_flags2 |= TR2_SUST_DEX;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_CON)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_CON))
	{
	    o_ptr->art_flags2 |= TR2_SUST_CON;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_CHR)
    {
	if (!(o_ptr->art_flags2 & TR2_SUST_CHR))
	{
	    o_ptr->art_flags2 |= TR2_SUST_CHR;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_CHAOS)
    {
	if (!(o_ptr->art_flags3 & TR3_TELEPORT))
	{
	    o_ptr->art_flags3 |= TR3_TELEPORT;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_FIRE)
    {
	if (!(o_ptr->art_flags3 & TR3_LITE))
	{
	    o_ptr->art_flags3 |= TR3_LITE; /* Freebie */
	}
    }

    switch (randint(31))
    {
    case 1:
	o_ptr->art_flags2 |= TR2_SUST_STR;
	if (!artifact_bias) artifact_bias = BIAS_STR;
	break;
    case 2:
	o_ptr->art_flags2 |= TR2_SUST_INT;
	if (!artifact_bias) artifact_bias = BIAS_INT;
	break;
    case 3:
	o_ptr->art_flags2 |= TR2_SUST_WIS;
	if (!artifact_bias) artifact_bias = BIAS_WIS;
	break;
    case 4:
	o_ptr->art_flags2 |= TR2_SUST_DEX;
	if (!artifact_bias) artifact_bias = BIAS_DEX;
	break;
    case 5:
	o_ptr->art_flags2 |= TR2_SUST_CON;
	if (!artifact_bias) artifact_bias = BIAS_CON;
	break;
    case 6:
	o_ptr->art_flags2 |= TR2_SUST_CHR;
	if (!artifact_bias) artifact_bias = BIAS_CHR;
	break;
    case 7: case 8: case 14:
	o_ptr->art_flags2 |= TR2_FREE_ACT;
	break;
    case 9:
	o_ptr->art_flags2 |= TR2_HOLD_LIFE;
	if (!artifact_bias && (randint(5)==1)) artifact_bias = BIAS_PRIESTLY;
	else if (!artifact_bias && (randint(6)==1)) artifact_bias = BIAS_NECROMANTIC;
	break;
    case 10: case 11:
	o_ptr->art_flags3 |= TR3_LITE;
	break;
    case 12: case 13:
	o_ptr->art_flags3 |= TR3_FEATHER;
	break;
    case 15: case 16: case 17:
	o_ptr->art_flags3 |= TR3_SEE_INVIS;
	break;
    case 18:
	o_ptr->art_flags3 |= TR3_TELEPATHY;
	if (!artifact_bias && (randint(9)==1)) artifact_bias = BIAS_MAGE;
	break;
    case 19: case 20:
	o_ptr->art_flags3 |= TR3_SLOW_DIGEST;
	break;
    case 21: case 22:
	o_ptr->art_flags3 |= TR3_REGEN;
	break;
    case 23:
	o_ptr->art_flags3 |= TR3_TELEPORT;
	break;
    case 24: case 25: case 26:
	if (o_ptr->tval>=TV_BOOTS) random_misc(o_ptr, is_scroll);
	else
	{
		o_ptr->art_flags3 |= TR3_SHOW_MODS;
		o_ptr->to_a = 4 + (randint(11));
	}
	break;
    case 27: case 28: case 29:
	o_ptr->art_flags3 |= TR3_SHOW_MODS;
	o_ptr->to_h += 4 + (randint(11));
	o_ptr->to_d += 4 + (randint(11));
	break;
    case 30:
        o_ptr->art_flags3 |= TR3_NO_MAGIC;
        break;
    case 31:
        o_ptr->art_flags3 |= TR3_NO_TELE;
        break;
    }
}


void random_slay(object_type * o_ptr, bool is_scroll)
{

    if (artifact_bias == BIAS_CHAOS && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_CHAOTIC))
	{
	    o_ptr->art_flags1 |= TR1_CHAOTIC;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_PRIESTLY &&
	    (o_ptr->tval == TV_SWORD || o_ptr->tval == TV_POLEARM ||
	     o_ptr->tval == TV_AXE) && !(o_ptr->art_flags3 & TR3_BLESSED))

    {
	o_ptr->art_flags3 |= TR3_BLESSED; /* A free power for "priestly"
					     random artifacts */
    }
    else if (artifact_bias == BIAS_NECROMANTIC && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_VAMPIRIC))
	{
	    o_ptr->art_flags1 |= TR1_VAMPIRIC;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags1 & TR1_BRAND_POIS) && (randint(2)==1))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_POIS;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_RANGER && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_SLAY_ANIMAL))
	{
	    o_ptr->art_flags1 |= TR1_SLAY_ANIMAL;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags1 & TR1_SLAY_ELEMENTAL) && (randint(2)==1))
	{
	    o_ptr->art_flags1 |= TR1_SLAY_ELEMENTAL;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_ROGUE && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_BRAND_POIS))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_POIS;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_POIS && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_BRAND_POIS))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_POIS;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_FIRE && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_BRAND_FIRE))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_FIRE;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_COLD && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_BRAND_COLD))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_COLD;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_ELEC && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_BRAND_ELEC))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_ELEC;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_ACID && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_BRAND_ACID))
	{
	    o_ptr->art_flags1 |= TR1_BRAND_ACID;
	    if (randint(2)==1) return;
	}
    }
    else if (artifact_bias == BIAS_LAW && !(o_ptr->tval == TV_BOW))
    {
	if (!(o_ptr->art_flags1 & TR1_SLAY_EVIL))
	{
	    o_ptr->art_flags1 |= TR1_SLAY_EVIL;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags1 & TR1_SLAY_UNDEAD))
	{
	    o_ptr->art_flags1 |= TR1_SLAY_UNDEAD;
	    if (randint(2)==1) return;
	}
	if (!(o_ptr->art_flags1 & TR1_SLAY_DEMON))
	{
	    o_ptr->art_flags1 |= TR1_SLAY_DEMON;
	    if (randint(2)==1) return;
	}
    }

  if (!(o_ptr->tval == TV_BOW))
  {
    switch (randint(36))
    {
    case 1: case 2:
	o_ptr->art_flags1 |= TR1_SLAY_ELEMENTAL;
	break;
    case 3: case 4:
	o_ptr->art_flags1 |= TR1_SLAY_ANIMAL;
	break;
    case 5: case 6:
	o_ptr->art_flags1 |= TR1_SLAY_EVIL;
	if (!artifact_bias && (randint(2)==1)) artifact_bias = BIAS_LAW;
	else if (!artifact_bias && (randint(9)==1)) artifact_bias = BIAS_PRIESTLY;
	break;
    case 7: case 8:
	o_ptr->art_flags1 |= TR1_SLAY_UNDEAD;
	if (!artifact_bias && (randint(9)==1)) artifact_bias = BIAS_PRIESTLY;
	break;
    case 9: case 10:
	o_ptr->art_flags1 |= TR1_SLAY_DEMON;
	if (!artifact_bias && (randint(9)==1)) artifact_bias = BIAS_PRIESTLY;
	break;
    case 11: case 12:
	o_ptr->art_flags1 |= TR1_SLAY_ORC;
	break;
    case 13: case 14:
	o_ptr->art_flags1 |= TR1_SLAY_TROLL;
	break;
    case 15: case 16:
	o_ptr->art_flags1 |= TR1_SLAY_GIANT;
	break;
    case 17: case 18:
	o_ptr->art_flags1 |= TR1_SLAY_DRAGON;
	break;
    case 19: 
	o_ptr->art_flags1 |= TR1_KILL_DRAGON;
	break;
    case 20:  case 21:
	if (!(o_ptr->tval == TV_HAFTED))
	    {
		o_ptr->art_flags1 |= TR1_VORPAL;
		if (!artifact_bias && (randint(9)==1)) artifact_bias = BIAS_WARRIOR;
	    }
	else random_slay(o_ptr, is_scroll);
	break;
    case 22:
	o_ptr->art_flags1 |= TR1_IMPACT;
	break;
    case 23: case 24:
	o_ptr->art_flags1 |= TR1_BRAND_FIRE;
	if (!artifact_bias) artifact_bias = BIAS_FIRE;
	break;
    case 25: case 26:
	o_ptr->art_flags1 |= TR1_BRAND_COLD;
	if (!artifact_bias) artifact_bias = BIAS_COLD;
	break;
    case 27: case 28:
	o_ptr->art_flags1 |= TR1_BRAND_ELEC;
	if (!artifact_bias) artifact_bias = BIAS_ELEC;
	break;
    case 29: case 30:
	o_ptr->art_flags1 |= TR1_BRAND_ACID;
	if (!artifact_bias) artifact_bias = BIAS_ACID;
	break;
    case 31: case 32:
	o_ptr->art_flags1 |= TR1_BRAND_POIS;
	if (!artifact_bias && (randint(3)!=1)) artifact_bias = BIAS_POIS;
	else if (!artifact_bias && randint(6)==1) artifact_bias = BIAS_NECROMANTIC;
	else if (!artifact_bias) artifact_bias = BIAS_ROGUE;
	break;
    case 33: case 34:
	o_ptr->art_flags1 |= TR1_VAMPIRIC;
	if (!artifact_bias) artifact_bias = BIAS_NECROMANTIC;
	break;
	default:
		o_ptr->art_flags1 |= TR1_CHAOTIC;
		if (!artifact_bias) artifact_bias = BIAS_CHAOS;
	break;
      }
  }
  else
  {
    switch (randint(6))
    {
	case 1: case 2: case 3:
	o_ptr->art_flags3 |= TR3_XTRA_MIGHT;
	if (!artifact_bias && randint(9)==1) artifact_bias = BIAS_RANGER;
	break;
	default:
	o_ptr->art_flags3 |= TR3_XTRA_SHOTS;
	if (!artifact_bias && randint(9)==1) artifact_bias = BIAS_RANGER;
	break;
    }
  }
}


void give_activation_power(object_type * o_ptr)
{
	int type = 0, chance = 0;

	if (artifact_bias)
	{
		if (artifact_bias == BIAS_ELEC)
		{
			if (randint(3)!=1)
			{
				type = ACT_BO_ELEC_1;
			}
			else if (randint(5)!=1)
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
			if (randint(3)!=1)
			{
				type = ACT_BO_FIRE_1;
			}
			else if (randint(5)!=1)
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
			if (randint(3)!=1)
				type = ACT_BO_COLD_1;
			else if (randint(3)!=1)
				type = ACT_BA_COLD_1;
			else if (randint(3)!=1)
				type = ACT_BA_COLD_2;
			else
				type = ACT_BA_COLD_3;
		}
		else if (artifact_bias == BIAS_CHAOS)
		{
			chance = 50;
			if (randint(6)==1)
				type = ACT_SUMMON_DEMON;
			else
				type = ACT_CALL_CHAOS;
		}
		else if (artifact_bias == BIAS_PRIESTLY)
		{
			chance = 101;

			if (randint(13)==1)
				type = ACT_CHARM_UNDEAD;
			else if (randint(12)==1)
				type = ACT_BANISH_EVIL;
			else if (randint(11)==1)
				type = ACT_DISP_EVIL;
			else if (randint(10)==1)
				type = ACT_PROT_EVIL;
			else if (randint(9)==1)
				type = ACT_CURE_1000;
			else if (randint(8)==1)
				type = ACT_CURE_700;
			else if (randint(7)==1)
				type = ACT_REST_ALL;
			else if (randint(6)==1)
				type = ACT_REST_LIFE;
			else
				type = ACT_CURE_MW;
		}
		else if (artifact_bias == BIAS_NECROMANTIC)
		{
			chance = 101;
			if (randint(66)==1)
				type = ACT_WRAITH;
			else if (randint(13)==1)
				type = ACT_DISP_GOOD;
			else if (randint(9)==1)
				type = ACT_MASS_GENO;
			else if (randint(8)==1)
				type = ACT_GENOCIDE;
			else if (randint(13)==1)
				type = ACT_SUMMON_UNDEAD;
			else if (randint(9)==1)
				type = ACT_VAMPIRE_2;
			else if (randint(6)==1)
				type = ACT_CHARM_UNDEAD;
			else
				type = ACT_VAMPIRE_1;
		}
		else if (artifact_bias == BIAS_LAW)
		{
			chance = 101;
			if (randint(8)==1)
				type = ACT_BANISH_EVIL;
			else if (randint(4)==1)
				type = ACT_DISP_EVIL;
			else
				type = ACT_PROT_EVIL;
		}
		else if (artifact_bias == BIAS_ROGUE)
		{
			chance = 101;
			if (randint(50)==1)
				type = ACT_SPEED;
			else if (randint(4)==1)
				type = ACT_SLEEP;
			else if (randint(3)==1)
				type = ACT_DETECT_ALL;
			else if (randint(8)==1)
				type = ACT_ID_FULL;
			else
				type = ACT_ID_PLAIN;
		}
		else if (artifact_bias == BIAS_MAGE)
		{
			chance = 66;
			if (randint(20)==1)
				type = SUMMON_ELEMENTAL;
			else if (randint(10)==1)
				type = SUMMON_PHANTOM;
			else if (randint(5)==1)
				type = ACT_RUNE_EXPLO;
			else
				type = ACT_ESP;
		}
		else if (artifact_bias == BIAS_WARRIOR)
		{
			chance = 80;
			if (randint(100)==1)
				type = ACT_INVULN;
			else
				type = ACT_BERSERK;
		}
		else if (artifact_bias == BIAS_RANGER)
		{
			chance = 101;
			if (randint(20)==1)
				type = ACT_CHARM_ANIMALS;
			else if (randint(7)==1)
				type = ACT_SUMMON_ANIMAL;
			else if (randint(6)==1)
				type = ACT_CHARM_ANIMAL;
			else if (randint(4)==1)
				type = ACT_RESIST_ALL;
			else if (randint(3)==1)
				type = ACT_SATIATE;
			else
				type = ACT_CURE_POISON;
		}
	}

	while (!(type) || (randint(100)>=chance))
	{
		type = randint(255);
		switch (type)
		{
			case ACT_SUNLIGHT: case ACT_BO_MISS_1:
			case ACT_BA_POIS_1: case ACT_BO_ELEC_1:
			case ACT_BO_ACID_1: case ACT_BO_COLD_1: case ACT_BO_FIRE_1:
			case ACT_CONFUSE: case ACT_SLEEP: case ACT_QUAKE:
			case ACT_CURE_LW: case ACT_CURE_MW: case ACT_CURE_POISON:
			case ACT_BERSERK: case ACT_LIGHT: case ACT_MAP_LIGHT:
			case ACT_DEST_DOOR: case ACT_STONE_MUD: case ACT_TELEPORT:
				chance = 101;
				break;
			case ACT_BA_COLD_1: case ACT_BA_FIRE_1: case ACT_DRAIN_1:
			case ACT_TELE_AWAY: case ACT_ESP: case ACT_RESIST_ALL:
			case ACT_DETECT_ALL: case ACT_RECALL:
			case ACT_SATIATE: case ACT_RECHARGE:
				chance = 85;
				break;
			case ACT_TERROR: case ACT_PROT_EVIL: case ACT_ID_PLAIN:
				chance = 75;
				break;
			case ACT_DRAIN_2: case ACT_VAMPIRE_1: case ACT_BO_MISS_2:
			case ACT_BA_FIRE_2: case ACT_REST_LIFE:
				chance = 66;
				break;
			case ACT_BA_COLD_3: case ACT_BA_ELEC_3: case ACT_WHIRLWIND:
			case ACT_VAMPIRE_2: case ACT_CHARM_ANIMAL:
				chance = 50;
				break;
			case ACT_SUMMON_ANIMAL:
				chance = 40;
				break;
			case ACT_DISP_EVIL: case ACT_BA_MISS_3: case ACT_DISP_GOOD:
			case ACT_BANISH_EVIL: case ACT_GENOCIDE: case ACT_MASS_GENO:
			case ACT_CHARM_UNDEAD: case ACT_CHARM_OTHER: case ACT_SUMMON_PHANTOM:
			case ACT_REST_ALL:
			case ACT_RUNE_EXPLO:
				chance = 33;
				break;
			case ACT_CALL_CHAOS: case ACT_ROCKET:
			case ACT_CHARM_ANIMALS: case ACT_CHARM_OTHERS:
			case ACT_SUMMON_ELEMENTAL: case ACT_CURE_700:
			case ACT_SPEED: case ACT_ID_FULL: case ACT_RUNE_PROT:
				chance = 25;
				break;
			case ACT_CURE_1000: case ACT_XTRA_SPEED:
			case ACT_DETECT_XTRA: case ACT_DIM_DOOR:
				chance = 10;
				break;
			case ACT_SUMMON_UNDEAD: case ACT_SUMMON_DEMON:
			case ACT_WRAITH: case ACT_INVULN: case ACT_ALCHEMY:
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


void get_random_name(char * return_name, bool armour, int power)
{
	if (randint(100)<=TABLE_NAME)
		get_table_name(return_name);
	else
	{
		char NameFile[16];
		switch (armour)
		{
			case 1:
				switch(power)
				{
					case 0:
						strcpy(NameFile, "a_cursed.txt");
						break;
					case 1:
						strcpy(NameFile, "a_low.txt");
						break;
					case 2:
						strcpy(NameFile, "a_med.txt");
						break;
					default:
						strcpy(NameFile, "a_high.txt");
				}
				break;
			default:
				switch(power)
				{
					case 0:
						strcpy(NameFile, "w_cursed.txt");
						break;
					case 1:
						strcpy(NameFile, "w_low.txt");
						break;
					case 2:
						strcpy(NameFile, "w_med.txt");
						break;
					default:
						strcpy(NameFile, "w_high.txt");
				}
		}

		get_rnd_line(NameFile, return_name);
	}
}


bool create_artifact(object_type *o_ptr, bool a_scroll)
{
	char new_name[80];
	int has_pval = 0;
	int powers = randint(5) + 1;
	int max_type = (o_ptr->tval<TV_BOOTS?7:5);
	int power_level;
	s32b total_flags;
	bool a_cursed = FALSE;

	int warrior_artifact_bias = 0;

	artifact_bias = 0;

	if (a_scroll && (randint(4)==1))
	{
		switch (p_ptr->pclass)
		{
			case CLASS_WARRIOR: case CLASS_WEAPONMASTER:
				artifact_bias = BIAS_WARRIOR;
				break;
			case CLASS_MAGE: case CLASS_HIGH_MAGE:
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
				if (randint(5)>2) artifact_bias = BIAS_PRIESTLY;
				break;
		}
	}

	if ((randint(100) <= warrior_artifact_bias) && a_scroll) artifact_bias = BIAS_WARRIOR;

	strcpy(new_name,"");

	if ((!a_scroll) && (randint(A_CURSED)==1)) a_cursed = TRUE;

	while ((randint(powers) == 1) || (randint(7)==1) || randint(10)==1)
	{
		powers++;
	}

	if ((!a_cursed) && (randint(WEIRD_LUCK)==1)) powers *= 2;

	/* Main loop */
	while(powers--)
	{
		switch (randint(max_type))
		{
			case 1: case 2:
				random_plus(o_ptr, a_scroll);
				has_pval = TRUE;
				break;
			case 3: case 4:
				random_resistance(o_ptr, a_scroll, FALSE);
				break;
			case 5:
				random_misc(o_ptr, a_scroll);
				break;
			case 6: case 7:
				random_slay(o_ptr, a_scroll);
				break;
			default:
				if(wizard) msg_print ("Switch error in create_artifact!");
				powers++;
		}
	};

	if (has_pval)
	{
#if 0
		o_ptr->art_flags3 |= TR3_SHOW_MODS;
#endif
		o_ptr->art_flags3 |= TR3_HIDE_TYPE;

		if (o_ptr->art_flags1 & TR1_BLOWS)
		{
			o_ptr->pval = randint(2) + 1;
		}
		else
		{
			do
			{
				o_ptr->pval++;
			}
			while (o_ptr->pval<randint(5) || randint(o_ptr->pval)==1);
		}

		if (o_ptr->pval > 4 && (randint(WEIRD_LUCK)!=1))
			o_ptr->pval = 4;
	}

	/* give it some plusses... */
	if (o_ptr->tval>=TV_BOOTS)
		o_ptr->to_a += randint(o_ptr->to_a>19?1:20-o_ptr->to_a);
	else
	{
		o_ptr->to_h += randint(o_ptr->to_h>19?1:20-o_ptr->to_h);
		o_ptr->to_d += randint(o_ptr->to_d>19?1:20-o_ptr->to_d);
	}

	/* Just to be sure */
	o_ptr->art_flags3 |= ( TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
	                       TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	total_flags = flag_cost(o_ptr, o_ptr->pval);
	if (cheat_peek) msg_format("%ld", total_flags);

	if (a_cursed) curse_artifact(o_ptr);

	if ((!a_cursed) &&
	    (randint((o_ptr->tval>=TV_BOOTS)
	    ?ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE)==1))
	{
		o_ptr->xtra2 = 0;
		give_activation_power(o_ptr);
	}


	if(o_ptr->tval>=TV_BOOTS)
	{
		if (a_cursed) power_level = 0;
		else if (total_flags<10000) power_level = 1;
		else if (total_flags<20000) power_level = 2;
		else power_level = 3;
	}

	else
	{
		if (a_cursed) power_level = 0;
		else if (total_flags<15000) power_level = 1;
		else if (total_flags<30000) power_level = 2;
		else power_level = 3;
	}

	if (a_scroll)
	{
		char dummy_name[80];
		strcpy(dummy_name, "");
		identify_fully_aux(o_ptr);
		o_ptr->ident |= IDENT_STOREB;
		if (!(get_string("What do you want to call it (ESC for random)? ", dummy_name, 80)))
			get_random_name(new_name, (o_ptr->tval >= TV_BOOTS), power_level);
		else
		{
			strcpy(new_name,"called '");
			strcat(new_name,dummy_name);
			strcat(new_name,"'");
		}
		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);
	}
	else
	{
		get_random_name(new_name, (o_ptr->tval >= TV_BOOTS), power_level);
	}

	if (cheat_xtra)
	{
		if (artifact_bias)
			msg_format("Biased artifact: %d.", artifact_bias);
		else
			msg_print("No bias in artifact.");
	}

	/* Save the inscription */
	o_ptr->art_name = quark_add(new_name);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);

	return TRUE;
}


bool artifact_scroll()
{
	int             item;
	bool            okay = FALSE;
	object_type     *o_ptr;
	char            o_name[80];

	item_tester_hook = item_tester_hook_weapon_armour;

	/* Get an item (from equip or inven or floor) */
	if (!get_item(&item, "Enchant which item? ", TRUE, TRUE, TRUE))
	{
		if (item == -2) msg_print("You have nothing to enchant.");
		return (FALSE);
	}

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}


	/* Description */
	object_desc(o_name, o_ptr, FALSE, 0);

	/* Describe */
	msg_format("%s %s radiate%s a blinding light!",
	          ((item >= 0) ? "Your" : "The"), o_name,
	          ((o_ptr->number > 1) ? "" : "s"));

	if (o_ptr->name1 || o_ptr->art_name)
	{
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "artifacts" : "an artifact"));
		okay = FALSE;
	}

	else if (o_ptr->name2)
	{
		msg_format("The %s %s already %s!",
		    o_name, ((o_ptr->number > 1) ? "are" : "is"),
		    ((o_ptr->number > 1) ? "ego items" : "an ego item"));
		okay = FALSE;
	}

	else
	{
		if (o_ptr->number > 1)
		{
			msg_print("Not enough enough energy to enchant more than one object!");
			msg_format("%d of your %s %s destroyed!",(o_ptr->number)-1, o_name, (o_ptr->number>2?"were":"was"));
			o_ptr->number = 1;
		}
		okay = create_artifact(o_ptr, TRUE);
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
		msg_print("The enchantment failed.");
	}

	/* Something happened */
	return (TRUE);
}


/* Hook to determine if an object is activatable */
static bool item_tester_hook_activate(object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Not known */
	if (!object_known_p(o_ptr)) return (FALSE);

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Check activation flag */
	if (f3 & (TR3_ACTIVATE)) return (TRUE);

	/* Assume not */
	return (FALSE);
}


/* Hack -- activate The One Ring */
static void ring_of_power(int dir)
{
	/* Pick a random effect */
	switch (randint(10))
	{
		case 1: case 2:
		{
			msg_print("You are surrounded by a malignant aura.");

			/* Decrease all stats (permanently) */
			(void)dec_stat(A_STR, 50, TRUE);
			(void)dec_stat(A_INT, 50, TRUE);
			(void)dec_stat(A_WIS, 50, TRUE);
			(void)dec_stat(A_DEX, 50, TRUE);
			(void)dec_stat(A_CON, 50, TRUE);
			(void)dec_stat(A_CHR, 50, TRUE);

			/* Lose some experience (permanently) */
			p_ptr->exp -= (p_ptr->exp / 4);
			p_ptr->max_exp -= (p_ptr->exp / 4);
			check_experience();

			break;
		}
		case 3:
		{
			msg_print("You are surrounded by a powerful aura.");

			/* Dispel monsters */
			dispel_monsters(1000);

			break;
		}
		case 4: case 5: case 6:
		{
			/* Mana Ball */
			fire_ball(GF_MANA, dir, 450, 3);
			break;
		}
		case 7: case 8: case 9: case 10:
		{
			fire_bolt(GF_MANA, dir, 375);
			break;
		}
	}
}


/* Enchant some bolts (used only by Cubragol) */
static bool brand_bolts(void)
{
	int i;

	/* Use the first acceptable bolts */
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Skip non-bolts */
		if (o_ptr->tval != TV_BOLT) continue;

		/* Skip artifacts and ego-items */
		if (o_ptr->art_name || artifact_p(o_ptr) || ego_item_p(o_ptr))
			continue;

		/* Skip cursed/broken items */
		if (cursed_p(o_ptr) || broken_p(o_ptr)) continue;

		/* Randomize */
		if (rand_int(100) < 75) continue;

		/* Message */
		msg_print("Your bolts are covered in a fiery aura!");

		/* Ego-item */
		o_ptr->name2 = EGO_FLAME;

		/* Enchant */
		enchant(o_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

		/* Notice */
		return (TRUE);
	}

	/* Flush */
	if (flush_failure) flush();

	/* Fail */
	msg_print("The fiery enchantment failed.");

	/* Notice */
	return (TRUE);
}


static bool activate_random_artifact(object_type * o_ptr)
{
	int plev = p_ptr->lev;
	int ii = 0, ij = 0, k, dir, dummy = 0;

	if (!(o_ptr->art_name)) return FALSE; /* oops? */

	/* Activate for attack */
	switch (o_ptr->xtra2)
	{
		case ACT_SUNLIGHT:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("A line of sunlight appears.");
			lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
			msg_print("It glows extremely brightly...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_MISSILE, dir, damroll(3, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
			msg_print("It throbs deep green...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, 18, 3);
			o_ptr->timeout = rand_int(4) + 4;
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msg_print("It is covered in sparks...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ELEC, dir, damroll(6, 8));
			o_ptr->timeout = rand_int(6) + 6;
			break;
		}

		case ACT_BO_ACID_1:
		{
			msg_print("It is covered in acid...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ACID, dir, damroll(8, 8));
			o_ptr->timeout = rand_int(5) + 5;
			break;
		}

		case ACT_BO_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_COLD, dir, damroll(9, 8));
			o_ptr->timeout = rand_int(7) + 7;
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msg_print("It is covered in fire...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_FIRE, dir, damroll(14, 8));
			o_ptr->timeout = rand_int(8) + 8;
			break;
		}

		case ACT_BA_COLD_1:
		{
			msg_print("It is covered in frost...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 72, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
			msg_print("It glows an intense red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 108, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 150))
			o_ptr->timeout = rand_int(100) + 100;
			break;
		}

		case ACT_BA_COLD_2:
		{
			msg_print("It glows an intense blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 150, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
		msg_print("It crackles with electricity...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 150, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
			msg_print("It glows black...");
			if (!get_aim_dir(&dir)) return FALSE;
			drain_life(dir, 180);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 75))
					hp_player(75);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
			msg_print("It grows magical spikes...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_bolt(GF_ARROW, dir, 225);
			o_ptr->timeout = rand_int(90) + 90;
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msg_print("It glows deep red...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 180, 3);
			o_ptr->timeout = rand_int(225) + 225;
			break;
		}

		case ACT_BA_COLD_3:
		{
			msg_print("It glows bright white...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 300, 3);
			o_ptr->timeout = rand_int(325) + 325;
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msg_print("It glows deep blue...");
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 375, 3);
			o_ptr->timeout = rand_int(425) + 425;
			break;
		}

		case ACT_WHIRLWIND:
		{
			{
				int y = 0, x = 0;
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
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
			{
				if (drain_life(dir, 150))
					hp_player(150);
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
			fire_ball(GF_ROCKET, dir, 180 + (plev), 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
			msg_print("It floods the area with goodness...");
			dispel_evil(400);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_DISP_GOOD:
		{
			msg_print("It floods the area with evil...");
			dispel_good(400);
			o_ptr->timeout = rand_int(300) + 300;
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msg_print("You breathe the elements.");
			fire_ball(GF_MISSILE, dir, 450, 4);
			o_ptr->timeout = 500;
			break;
		}

		/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
			msg_print("It glows in scintillating colours...");
			if (!get_aim_dir(&dir)) return FALSE;
			confuse_monster(dir, 20);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
			msg_print("It glows deep blue...");
			sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
			earthquake(py, px, 10);
			o_ptr->timeout = 50;
			break;
		}

		case ACT_TERROR:
		{
			turn_monsters(40 + p_ptr->lev);
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
			if (banish_evil(100))
			{
				msg_print("The power of the artifact banishes evil!");
			}
			o_ptr->timeout = 250 + randint(250);
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

		/* Activate for summoning / charming */

		case ACT_CHARM_ANIMAL:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			(void) charm_animal(dir, plev);
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
			(void) charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_CHARM_ANIMALS:
		{
			(void) charm_animals(plev * 2);
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
			(void)summon_specific_friendly(py, px, plev, SUMMON_ANIMAL_RANGER, TRUE);
			o_ptr->timeout = 200 + randint(300);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msg_print("You summon a phantasmal servant.");
			(void)summon_specific_friendly(py, px, dun_level, SUMMON_PHANTOM, TRUE);
			o_ptr->timeout = 200 + randint(200);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			if (randint(3) == 1)
			{
				if (summon_specific(py, px, plev * 1.5, SUMMON_ELEMENTAL))
				{
					msg_print("An elemental materializes...");
					msg_print("You fail to control it!");
				}
			}
			else
			{
				if (summon_specific_friendly(py, px, plev * 1.5,
				    SUMMON_ELEMENTAL, (plev == 50 ? TRUE : FALSE)))
				{
					msg_print("An elemental materializes...");
					msg_print("It seems obedient to you.");
				}
			}
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			if (randint(3) == 1)
			{
				if (summon_specific(py, px, plev * 1.5, SUMMON_DEMON))
				{
					msg_print("The area fills with a stench of sulphur and brimstone.");
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
				}
			}
			else
			{
				if (summon_specific_friendly(py, px, plev * 1.5,
				    SUMMON_DEMON, (plev == 50 ? TRUE : FALSE)))
				{
					msg_print("The area fills with a stench of sulphur and brimstone.");
					msg_print("'What is thy bidding... Master?'");
				}
			}
			o_ptr->timeout = 666 + randint(333);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			if (randint(3) == 1)
			{
				if (summon_specific(py, px, plev * 1.5,
				    (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD)))
				{
					msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
					msg_print("'The dead arise... to punish you for disturbing them!'");
				}
			}
			else
			{
				if (summon_specific_friendly(py, px, plev * 1.5,
				    (plev > 47 ? SUMMON_HI_UNDEAD_NO_UNIQUES : SUMMON_UNDEAD),
				    (((plev > 24) && (randint(3) == 1)) ? TRUE : FALSE)))
				{
					msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
					msg_print("Ancient, long-dead forms arise from the ground to serve you!");
				}
			}
			o_ptr->timeout = 666 + randint(333);
			break;
		}

		/* Activate for healing */

		case ACT_CURE_LW:
		{
			(void)set_blind(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msg_print("It radiates deep purple...");
			hp_player(damroll(4, 8));
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_cut((p_ptr->cut / 2) - 50);
			o_ptr->timeout = rand_int(3) + 3;
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
			restore_level();
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
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_stun(0);
			(void)set_poisoned(0);
			(void)set_cut(0);
			(void)set_image(0);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msg_print("It glows a bright white...");
			msg_print("You feel much better...");
			(void)hp_player(1000);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_stun(0);
			(void)set_poisoned(0);
			(void)set_cut(0);
			(void)set_image(0);
			o_ptr->timeout = 888;
			break;
		}

		/* Activate for timed effect */

		case ACT_ESP:
		{
			(void)set_tim_esp(p_ptr->tim_esp + randint(30) + 25);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			(void)set_shero(p_ptr->shero + randint(50) + 50);
			(void)set_blessed(p_ptr->blessed + randint(50) + 50);
			o_ptr->timeout = 100 + randint(100);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msg_print("It lets out a shrill wail...");
			(void)set_protevil(p_ptr->protevil + randint(25) + (2 * p_ptr->lev));
			o_ptr->timeout = rand_int(225) + 225;
			break;
		}

		case ACT_RESIST_ALL:
		{
			msg_print("It glows many colours...");
			(void)set_oppose_acid(p_ptr->oppose_acid + randint(40) + 40);
			(void)set_oppose_elec(p_ptr->oppose_elec + randint(40) + 40);
			(void)set_oppose_fire(p_ptr->oppose_fire + randint(40) + 40);
			(void)set_oppose_cold(p_ptr->oppose_cold + randint(40) + 40);
			(void)set_oppose_pois(p_ptr->oppose_pois + randint(40) + 40);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
			msg_print("It glows bright green...");
			if (!p_ptr->fast)
			{
				(void)set_fast(randint(20) + 20);
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
				(void)set_fast(randint(75) + 75);
			}
			else
			{
				(void)set_fast(p_ptr->fast + 5);
			}
			o_ptr->timeout = rand_int(200) + 200;
			break;
		}

		case ACT_WRAITH:
		{
			set_shadow(p_ptr->wraith_form + randint(plev/2) + (plev/2));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			(void)set_invuln(p_ptr->invuln + randint(8) + 8);
			o_ptr->timeout = 1000;
			break;
		}

		/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
			msg_print("It wells with clear light...");
			lite_area(damroll(3, 15), 3);
			o_ptr->timeout = rand_int(10) + 10;
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msg_print("It shines brightly...");
			map_area();
			lite_area(damroll(2, 15), 3);
			o_ptr->timeout = rand_int(50) + 50;
			break;
		}

		case ACT_DETECT_ALL:
		{
			msg_print("It glows bright white...");
			msg_print("An image forms in your mind...");
			detect_all();
			o_ptr->timeout = rand_int(55) + 55;
			break;
		}

		case ACT_DETECT_XTRA:
		{
			msg_print("It glows brightly...");
			detect_all();
			probing();
			identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
			msg_print("It glows yellow...");
			identify_fully();
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
			explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
			msg_print("It glows light blue...");
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
			msg_print("It glows bright red...");
			destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
			msg_print("It pulsates...");
			if (!get_aim_dir(&dir)) return FALSE;
			wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
			recharge(60);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
			msg_print("It glows bright yellow...");
			(void) alchemy();
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DIM_DOOR:
		{
			msg_print("You open a dimensional gate. Choose a destination.");
			if (!tgt_pt(&ii,&ij)) return FALSE;
			p_ptr->energy -= 60 - plev;
			if (!cave_empty_bold(ij,ii) || (cave[ij][ii].info & CAVE_ICKY) ||
			    (distance(ij,ii,py,px) > plev + 2) ||
			    (!rand_int(plev * plev / 2)))
			{
				msg_print("You fail to exit the astral plane correctly!");
				p_ptr->energy -= 100;
				teleport_player(10);
			}
			else teleport_player_to(ij,ii);
			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT:
		{
			msg_print("It twists space around you...");
			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}

		case ACT_RECALL:
		{
			if (dun_level && (p_ptr->max_dlv > dun_level))
			{
				if (get_check("Reset recall depth? "))
				p_ptr->max_dlv = dun_level;
			}

			msg_print("It glows soft white...");
			if (p_ptr->word_recall == 0)
			{
				p_ptr->word_recall = randint(20) + 15;
				msg_print("The air about you becomes charged...");
			}
			else
			{
				p_ptr->word_recall = 0;
				msg_print("A tension leaves the air around you...");
			}
			o_ptr->timeout = 200;
			break;
		}

		default:
		{
			msg_format("Unknown activation effect: %d.", o_ptr->xtra2);
			return FALSE;
		}
	}
	return TRUE;
}


/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
void do_cmd_activate(void)
{
	int             item, i, dir, lev, chance, dummy = 0;

	object_type     *o_ptr;


	/* Prepare the hook */
	item_tester_hook = item_tester_hook_activate;

	/* Get an item (from equip) */
	if (!get_item(&item, "Activate which item? ", TRUE, FALSE, FALSE))
	{
		if (item == -2) msg_print("You have nothing to activate.");
		return;
	}

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Take a turn */
	energy_use = 100;

	/* Extract the item level */
	lev = k_info[o_ptr->k_idx].level;

	/* Hack -- use artifact level instead */
	if (artifact_p(o_ptr)) lev = a_info[o_ptr->name1].level;

	/* Base chance of success */
	chance = p_ptr->skill_dev;

	/* Confusion hurts skill */
	if (p_ptr->confused) chance = chance / 2;

	/* Hight level objects are harder */
	chance = chance - ((lev > 50) ? 50 : lev);

	/* Give everyone a (slight) chance */
	if ((chance < USE_DEVICE) && (rand_int(USE_DEVICE - chance + 1) == 0))
	{
		chance = USE_DEVICE;
	}

	/* Roll for usage */
	if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE))
	{
		if (flush_failure) flush();
		msg_print("You failed to activate it properly.");
		return;
	}

	/* Check the recharge */
	if (o_ptr->timeout)
	{
		msg_print("It whines, glows and fades...");
		return;
	}

	/* Activate the artifact */
	msg_print("You activate it...");

	/* Sound */
	sound(SOUND_ZAP);

	if (o_ptr->art_name)
	{
		(void) activate_random_artifact(o_ptr);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	/* Artifacts */
	else if (o_ptr->name1)
	{
		/* Choose effect */
		switch (o_ptr->name1)
		{
			case ART_GALADRIEL:
			{
				msg_print("The phial wells with clear light...");
				lite_area(damroll(3, 15), 3);
				o_ptr->timeout = 20;
				break;
			}
			case ART_ELENDIL:
			{
				msg_print("The star shines brightly...");
				map_area();
				lite_area(damroll(3, 15), 3);
				o_ptr->timeout = 30;
				break;
			}
			case ART_THRAIN:
			{
				msg_print("The Jewel flashes bright white!");
				wiz_lite();
				o_ptr->timeout = 150;
				break;
			}
			case ART_RED_AMULET:
			{
				msg_print("The amulet glows bright red...");
				if (randint(100) <= 10)
				{
					msg_print("You are not the rightful wearer!  Madness grips your mind...");
					set_image(p_ptr->image + 100 + randint(100));
				}
				else
				{
					charm_monsters(p_ptr->lev * 8);
					(void)set_shero(p_ptr->shero + randint(50) + 50);
					(void)hp_player(30);
					(void)set_afraid(0);
				}
				o_ptr->timeout = rand_int(150) + 150;
				break;
			}
			case ART_INGWE:
			{
				msg_print("The amulet floods the area with goodness...");
				dispel_evil(400);
				o_ptr->timeout = rand_int(200) + 200;
				break;
			}
			case ART_TULKAS:
			{
				msg_print("The ring glows brightly...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(75) + 75);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 5);
				}
				o_ptr->timeout = rand_int(150) + 150;
				break;
			}
			case ART_NARYA:
			{
				msg_print("The ring glows deep red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 150, 3);
				o_ptr->timeout = rand_int(50) + 50;
				break;
			}
			case ART_NENYA:
			{
				msg_print("The ring glows bright white...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 200, 3);
				o_ptr->timeout = rand_int(75) + 75;
				break;
			}
			case ART_VILYA:
			{
				msg_print("The ring glows deep blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 250, 3);
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}
			case ART_POWER:
			{
				msg_print("The ring glows intensely black...");
				if (!get_aim_dir(&dir)) return;
				ring_of_power(dir);
				o_ptr->timeout = rand_int(200) + 200;
				break;
			}
			case ART_RAZORBACK:
			{
				msg_print("Your armor is surrounded by lightning...");
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 225, 3);
				o_ptr->timeout = 250;
				break;
			}
			case ART_BLADETURNER:
			{
				if (!get_aim_dir(&dir)) return;
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 450, 4);
				msg_print("Your armor glows many colours...");
				(void)set_afraid(0);
				(void)set_shero(p_ptr->shero + randint(50) + 50);
				(void)hp_player(30);
				(void)set_blessed(p_ptr->blessed + randint(50) + 50);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(50) + 50);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(50) + 50);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(50) + 50);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(50) + 50);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(50) + 50);
				o_ptr->timeout = 300;
				break;
			}
			case ART_SOULKEEPER:
			{
				msg_print("Your armor glows a bright white...");
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)set_stun(0);
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_poisoned(0);
				(void)set_image(0);
				(void)set_cut(0);
				o_ptr->timeout = 500;
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
				(void)set_image(0);
				(void)set_hero(p_ptr->hero + randint(25) + 25);
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
				destroy_doors_touch();
				o_ptr->timeout = 10;
				break;
			}
			case ART_PAN_TANG:
			{
				msg_print("Your glowing armour shines even brighter...");
				for (i = 0; i < 6; i++) (void)summon_specific_friendly(py, px, dun_level, SUMMON_DEMON, TRUE);
				o_ptr->timeout = 400;
				break;
			}
			case ART_MELNIBONE: case ART_TERROR_MASK:
			{
				turn_monsters(40 + p_ptr->lev);
				o_ptr->timeout = 75;
				break;
			}
			case ART_HOLHENNETH:
			{
				msg_print("Your helm glows bright white...");
				detect_all();
				o_ptr->timeout = 55;
				break;
			}
			case ART_GONDOR:
			{
				msg_print("Your crown glows deep blue...");
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)set_blind(0);
				(void)set_confused(0);
				(void)set_poisoned(0);
				(void)set_image(0);
				(void)set_stun(0);
				(void)set_cut(0);
				o_ptr->timeout = 250;
				break;
			}
			case ART_COLLUIN:
			{
				msg_print("Your cloak glows many colours...");
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(25) + 25);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(25) + 25);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(25) + 25);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(25) + 25);
				(void)set_oppose_pois(p_ptr->oppose_pois + randint(25) + 25);
				o_ptr->timeout = 100;
				break;
			}
			case ART_HOLCOLLETH:
			{
				msg_print("Your cloak glows deep blue...");
				restore_level();
				o_ptr->timeout = 450;
				break;
			}
			case ART_THINGOL:
			{
				msg_print("Your cloak glows bright yellow...");
				recharge(60);
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
				sleep_monsters_touch();
				o_ptr->timeout = 35;
				break;
			}
			case ART_KWLL:
			{
				msg_print("The Hand draws forth undead from Limbo.");
				turn_monsters(p_ptr->lev * 4);
				for (i = 0; i <= ((dun_level + 10) / 5); i++)
					(void)summon_specific_friendly(py, px, dun_level, SUMMON_UNDEAD, TRUE);
				
				o_ptr->timeout = 500;
				break;
			}
			case ART_CAMMITHRIM:
			{
				msg_print("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_MISSILE, dir, damroll(3, 6));
				o_ptr->timeout = 2;
				break;
			}
			case ART_PAURHACH:
			{
				msg_print("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(14, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}
			case ART_PAURNIMMEN:
			{
				msg_print("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}
			case ART_PAURAEGEN:
			{
				msg_print("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}
			case ART_PAURNEN:
			{
				msg_print("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ACID, dir, damroll(8, 8));
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}
			case ART_FINGOLFIN:
			{
				msg_print("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ARROW, dir, 225);
				o_ptr->timeout = rand_int(90) + 90;
				break;
			}
			case ART_FEANOR:
			{
				msg_print("Your boots glow bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
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
			case ART_STORMBRINGER:
			{
				msg_print("Stormbringer summons its brothers!");
				for (i = 0; i < 10; i++) (void)summon_specific_friendly(py, px, dun_level, SUMMON_HELLBLADES, TRUE);
				o_ptr->timeout = 1000;
				break;
			}
			case ART_NARTHANC:
			{
				msg_print("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_FIRE, dir, damroll(14, 8));
				o_ptr->timeout = rand_int(8) + 8;
				break;
			}
			case ART_NIMTHANC:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_COLD, dir, damroll(9, 8));
				o_ptr->timeout = rand_int(7) + 7;
				break;
			}
			case ART_DETHANC:
			{
				msg_print("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = rand_int(6) + 6;
				break;
			}
			case ART_RILIA:
			{
				msg_print("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_POIS, dir, 18, 3);
				o_ptr->timeout = rand_int(4) + 4;
				break;
			}
			case ART_BELANGIL:
			{
				msg_print("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 72, 2);
				o_ptr->timeout = rand_int(5) + 5;
				break;
			}
			case ART_GLAMDRING: case ART_ORCRIST:
			case ART_STING: case ART_ORCHAST:
			{
				msg_print("Your weapon glows brightly...");
				(void)detect_monsters_xxx(RF3_ORC);
				o_ptr->timeout = 10;
				break;
			}
			case ART_ANGUIREL:
			{
				switch(randint(13))
				{
				case 1: case 2: case 3: case 4: case 5:
					teleport_player(10);
					break;
				case 6: case 7: case 8: case 9: case 10:
					teleport_player(222);
					break;
				case 11: case 12:
					(void)stair_creation();
					break;
				default:
					if(get_check("Leave this level? "))
					{
						if (autosave_l)
						{
							is_autosave = TRUE;
							msg_print("Autosaving the game...");
							do_cmd_save_game();
							is_autosave = FALSE;
						}

					new_level_flag = TRUE;
					}
				}
				o_ptr->timeout = 35;
				break;
			}
			case ART_RINGIL:
			{
				msg_print("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 150, 2);
				o_ptr->timeout = 150;
				break;
			}
			case ART_DAWN:
			{
				msg_print("You summon the Legion of the Dawn.");
				(void)summon_specific_friendly(py, px, dun_level, SUMMON_DAWN, TRUE);
				o_ptr->timeout = 500 + randint(500);
				break;
			}
			case ART_KANAJANA:
			{
				msg_print("The sword emits hard radiation...");
				fire_ball(GF_NUKE, 0, 300, 4);
				o_ptr->timeout = 200;
				break;
			}
			case ART_THEODEN:
			{
				msg_print("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 180);
				o_ptr->timeout = 75;
				break;
			}
			case ART_AEGLOS:
			{
				msg_print("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 150, 3);
				o_ptr->timeout = 250;
				break;
			}
			case ART_OROME:
			{
				msg_print("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return;
				wall_to_mud(dir);
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
				hp_player(damroll(6, 8));
				(void)set_cut((p_ptr->cut / 2) - 50);
				o_ptr->timeout = rand_int(3) + 3;
				break;
			}
			case ART_WRATH:
			{
				msg_print("Your trident glows bright white...");
				destroy_area(py, px, 15, TRUE);
				o_ptr->timeout = 120;
				break;
			}
			case ART_ULMO:
			{
				msg_print("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return;
				teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}
			case ART_AVAVIR:
			{
				if (dun_level && (p_ptr->max_dlv > dun_level))
				{
					if (get_check("Reset recall depth? "))
					p_ptr->max_dlv = dun_level;
				}
                
				msg_print("Your scythe glows soft white...");
				if (p_ptr->word_recall == 0)
				{
					p_ptr->word_recall = randint(20) + 15;
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				o_ptr->timeout = 200;
				break;
			}
			case ART_TOTILA:
			{
				msg_print("Your flail glows in scintillating colours...");
				if (!get_aim_dir(&dir)) return;
				confuse_monster(dir, 20);
				o_ptr->timeout = 15;
				break;
			}
			case ART_FIRESTAR:
			{
				msg_print("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 108, 3);
				o_ptr->timeout = 65;
				break;
			}
			case ART_TARATOL:
			{
				msg_print("Your mace glows bright green...");
				if (!p_ptr->fast)
				{
					(void)set_fast(randint(20) + 20);
				}
				else
				{
					(void)set_fast(p_ptr->fast + 10);
				}
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}
			case ART_ERIRIL:
			{
				msg_print("Your quarterstaff glows yellow...");
				if (!ident_spell()) return;
				o_ptr->timeout = 10;
				break;
			}
			case ART_OLORIN:
			{
				msg_print("Your quarterstaff glows brightly...");
				detect_all();
				probing();
				o_ptr->timeout = 100;
				break;
			}
			case ART_TURMIL:
			{
				msg_print("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return;
				drain_life(dir, 140);
				o_ptr->timeout = 50;
				break;
			}
			case ART_BRYIONAK:
			{
				msg_print("You summon the Black Bull of Crinanass!");
				msg_print("It roars around the battlefield...");
				dispel_monsters(250);
				msg_print("The Bull disappears...");
				o_ptr->timeout = rand_int(250) + 250;
				break;
			}
			case ART_CUBRAGOL:
			{
				msg_print("Your crossbow glows deep red...");
				(void)brand_bolts();
				o_ptr->timeout = 999;
				break;
			}
			case ART_COWARDICE:
			{
				msg_print("Your pike glows bright yellow...");
				(void)teleport_player_level();
				o_ptr->timeout = 2;
				break;
			}
			case ART_BASHER:
			{
				msg_print("You pound your mace against the wall with incredible force...");
				destroy_area(py, px, 15, TRUE);
				o_ptr->timeout = 100 + randint(100);
				break;
			}
			case ART_WHIRLWIND:
			{
				int y = 0, x = 0;
				cave_type       *c_ptr;
				monster_type    *m_ptr;

				msg_print("Your ball-and-chain starts to spin...");

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
				o_ptr->timeout = 250;
				break;
			}
			case ART_ARIANROD:
			{
				msg_print("You step into another plane of existence.");
				if (autosave_l)
				{
					is_autosave = TRUE;
					msg_print("Autosaving the game...");
					do_cmd_save_game();
					is_autosave = FALSE;
				}
				new_level_flag = TRUE;
				o_ptr->timeout = 100;
				break;
			}
			case ART_RETALIATOR:
			{
				msg_print("Retaliator glows brightly...");
				confuse_monsters(p_ptr->lev * 6);
				o_ptr->timeout = 75 + randint(75);
				break;
			}
			case ART_ENTANGLEMENT:
			{
				msg_print("Your whip begins to move on its own...");
				slow_monsters();
				o_ptr->timeout = 25 + randint(25);
				break;
			}
			case ART_NIGHT:
			{
				msg_print("Your axe emits a black aura...");
				if (!get_aim_dir(&dir)) return;
				for (dummy = 0; dummy < 3; dummy++)
				{
					if (drain_life(dir, 100))
						hp_player(100);
				}
				o_ptr->timeout = 250;
				break;
			}
			case ART_ENERGY:
			{
				msg_print("Your scythe is covered in sparks...");
				if (!get_aim_dir(&dir)) return;
				fire_bolt(GF_ELEC, dir, damroll(18, 8));
				o_ptr->timeout = rand_int(10) + 10;
				break;
			}
			case ART_NATUREBANE:
			{
				msg_print("Your axe glows blood red...");
				dispel_animals(300);
				o_ptr->timeout = 200 + randint(200);
				break;
			}
			case ART_CATAPULT:
			{
				msg_print("You ready a ball of pitch...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 50, 2);
				o_ptr->timeout = 25 + randint(25);
				break;
			}
			case ART_HERMES:
			{
				if (dun_level && (p_ptr->max_dlv > dun_level))
				{
					if (get_check("Reset recall depth? "))
					p_ptr->max_dlv = dun_level;
				}

				msg_print("Your sandals glow...");
				if (p_ptr->word_recall == 0)
				{
					p_ptr->word_recall = randint(20) + 15;
					msg_print("The air about you becomes charged...");
				}
				else
				{
					p_ptr->word_recall = 0;
					msg_print("A tension leaves the air around you...");
				}
				o_ptr->timeout = randint(50) + 50;
				break;
			}
			case ART_DEATH:
			{
				msg_print("The Scythe of Death glows black...");
				(void) deathray_monsters();
				o_ptr->timeout = randint(500) + 500;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return;
	}

	/* Ego Items */
	else if (o_ptr->name2)
	{
		/* Choose effect */
		switch (o_ptr->name2)
		{
			case EGO_TRUMP:
			{
				msg_print("Your weapon glows as space warps around you...");
				teleport_player(100);
				o_ptr->timeout = 50 + randint(50);
				break;
			}
			case EGO_WEST:
			{
				msg_print("Your weapon glows brightly...");
				(void)detect_monsters_xxx(RF3_ORC);
				o_ptr->timeout = 10;
				break;
			}
			case EGO_SPINES:
			{
				msg_print("Your amour expels some spines...");
				fire_ball(GF_SHARDS, 0, 80, 2);
				o_ptr->timeout = 100 + randint(100);
				break;
			}
		}

		p_ptr->window |= (PW_INVEN | PW_EQUIP);
		return;
	}

	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return;

		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_DRAGON_BLUE:
			{
				msg_print("You breathe lightning.");
				fire_ball(GF_ELEC, dir, 200, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_WHITE:
			{
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, 250, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_BLACK:
			{
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, 200, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_GREEN:
			{
				msg_print("You breathe poison gas.");
				fire_ball(GF_POIS, dir, 275, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_RED:
			{
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, 250, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_MULTIHUED:
			{
				chance = rand_int(5);
				msg_format("You breathe %s.",
				           ((chance == 1) ? "lightning" :
				            ((chance == 2) ? "frost" :
				             ((chance == 3) ? "acid" :
				              ((chance == 4) ? "poison gas" : "fire")))));
				fire_ball(((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
				             ((chance == 4) ? GF_POIS : GF_FIRE)))),
				          dir, 300, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_BRONZE:
			{
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 300, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_GOLD:
			{
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir, 300, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, 350, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_LAW:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),
				          dir, 350, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				msg_format("You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARDS))),
				          dir, 400, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				msg_format("You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 300, 2);
				o_ptr->timeout = randint(75) + 75;
				break;
			}
			case SV_DRAGON_POWER:
			{
				msg_print("You breathe the elements.");
				fire_ball(GF_MISSILE, dir, 450, 3);
				o_ptr->timeout = rand_int(100) + 100;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
	}

	else if (o_ptr->tval == TV_RING)
	{
		switch (o_ptr->sval)
		{
			case SV_RING_TELEPORTATION:
			{
				msg_print("Your ring flickers rapidly...");
				teleport_player(50 + randint(50));
				o_ptr->timeout = 250;
				break;
			}
			case SV_RING_LIGHTNING:
			{
				msg_print("Your ring glows yellow...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ELEC, dir, 75, 2);
				(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
				o_ptr->timeout = 50;
				break;
			}
			case SV_RING_ACID:
			{
				msg_print("Your ring glows green...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_ACID, dir, 75, 2);
				(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
				o_ptr->timeout = 50;
				break;
			}
			case SV_RING_FLAMES:
			{
				msg_print("Your ring glows red...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_FIRE, dir, 75, 2);
				(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
				o_ptr->timeout = 50;
				break;
			}
			case SV_RING_ICE:
			{
				msg_print("Your ring glows white...");
				if (!get_aim_dir(&dir)) return;
				fire_ball(GF_COLD, dir, 75, 2);
				(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
				o_ptr->timeout = 50;
				break;
			}
			case SV_RING_SHADOWS:
			{
				msg_print("Your ring glows black...");
				set_shadow(p_ptr->wraith_form + randint(25) + (p_ptr->lev / 2));
				o_ptr->timeout = rand_int(500) + 500;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return;
    }

	/* Mistake */
	msg_print("Oops.  That object cannot be activated.");
}


/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr item_activation(object_type *o_ptr)
{
	u32b f1, f2, f3;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Require activation ability */
	if (!(f3 & (TR3_ACTIVATE))) return (NULL);

	/*
	 * We need to deduce somehow that it is a random artifact -- one
	 * problem: It could be a random artifact which has NOT YET received
	 * a name. Thus we eliminate other possibilities instead of checking
	 * for art_name
	 */

	if (!(o_ptr->name1) &&
	    !(o_ptr->name2) &&
	    !(o_ptr->xtra1) &&
	     (o_ptr->xtra2))
	{
		switch (o_ptr->xtra2)
		{
			case ACT_SUNLIGHT:
			{
				return "beam of sunlight every 10 turns";
			}
			case ACT_BO_MISS_1:
			{
				return "magic missile (3d6) every 2 turns";
			}
			case ACT_BA_POIS_1:
			{
				return "stinking cloud (18), rad. 3, every 4+d4 turns";
			}
			case ACT_BO_ELEC_1:
			{
				return "lightning bolt (6d8) every 6+d6 turns";
			}
			case ACT_BO_ACID_1:
			{
				return "acid bolt (8d8) every 5+d5 turns";
			}
			case ACT_BO_COLD_1:
			{
				return "frost bolt (9d8) every 7+d7 turns";
			}
			case ACT_BO_FIRE_1:
			{
				return "fire bolt (14d8) every 8+d8 turns";
			}
			case ACT_BA_COLD_1:
			{
				return "ball of cold (72) every 400 turns";
			}
			case ACT_BA_FIRE_1:
			{
				return "ball of fire (108) every 400 turns";
			}
			case ACT_DRAIN_1:
			{
				return "drain life (150) every 100+d100 turns";
			}
			case ACT_BA_COLD_2:
			{
				return "ball of cold (150) every 300 turns";
			}
			case ACT_BA_ELEC_2:
			{
				return "ball of lightning (150) every 500 turns";
			}
			case ACT_DRAIN_2:
			{
				return "drain life (180) every 400 turns";
			}
			case ACT_VAMPIRE_1:
			{
				return "vampiric drain (3*75) every 400 turns";
			}
			case ACT_BO_MISS_2:
			{
				return "arrows (225) every 90+d90 turns";
			}
			case ACT_BA_FIRE_2:
			{
				return "fire ball (180) every 225+d225 turns";
			}
			case ACT_BA_COLD_3:
			{
				return "ball of cold (300) every 325+d325 turns";
			}
			case ACT_BA_ELEC_3:
			{
				return "ball of lightning (375) every 425+d425 turns";
			}
			case ACT_WHIRLWIND:
			{
				return "whirlwind attack every 250 turns";
			}
			case ACT_VAMPIRE_2:
			{
				return "vampiric drain (3*150) every 400 turns";
			}
			case ACT_CALL_CHAOS:
			{
				return "call chaos every 350 turns";
			}
			case ACT_ROCKET:
			{
				return "launch rocket (180+level) every 400 turns";
			}
			case ACT_DISP_EVIL:
			{
				return "dispel evil (400) every 300+d300 turns";
			}
			case ACT_DISP_GOOD:
			{
				return "dispel good (400) every 300+d300 turns";
			}
			case ACT_BA_MISS_3:
			{
				return "elemental breath (450) every 500 turns";
			}
			case ACT_CONFUSE:
			{
				return "confuse monster every 15 turns";
			}
			case ACT_SLEEP:
			{
				return "sleep nearby monsters every 55 turns";
			}
			case ACT_QUAKE:
			{
				return "earthquake (rad 10) every 50 turns";
			}
			case ACT_TERROR:
			{
				return "terror every 3 * (level+10) turns";
			}
			case ACT_TELE_AWAY:
			{
				return "teleport away every 200 turns";
			}
			case ACT_BANISH_EVIL:
			{
				return "banish evil every 250+d250 turns";
			}
			case ACT_GENOCIDE:
			{
				return "genocide every 500 turns";
			}
			case ACT_MASS_GENO:
			{
				return "mass genocide every 1000 turns";
			}
			case ACT_CHARM_ANIMAL:
			{
				return "charm animal every 300 turns";
			}
			case ACT_CHARM_UNDEAD:
			{
				return "enslave undead every 333 turns";
			}
			case ACT_CHARM_OTHER:
			{
				return "charm monster every 400 turns";
			}
			case ACT_CHARM_ANIMALS:
			{
				return "animal friendship every 500 turns";
			}
			case ACT_CHARM_OTHERS:
			{
				return "mass charm every 750 turns";
			}
			case ACT_SUMMON_ANIMAL:
			{
				return "summon animal every 200+d300 turns";
			}
			case ACT_SUMMON_PHANTOM:
			{
				return "summon phantasmal servant every 200+d200 turns";
			}
			case ACT_SUMMON_ELEMENTAL:
			{
				return "summon elemental every 750 turns";
			}
			case ACT_SUMMON_DEMON:
			{
				return "summon demon every 666+d333 turns";
			}
			case ACT_SUMMON_UNDEAD:
			{
				return "summon undead every 666+d333 turns";
			}
			case ACT_CURE_LW:
			{
				return "remove fear & heal 30 hp every 10 turns";
			}
			case ACT_CURE_MW:
			{
				return "heal 4d8 & wounds every 3+d3 turns";
			}
			case ACT_CURE_POISON:
			{
				return "remove fear and cure poison every 5 turns";
			}
			case ACT_REST_LIFE:
			{
				return "restore life levels every 450 turns";
			}
			case ACT_REST_ALL:
			{
				return "restore stats and life levels every 750 turns";
			}
			case ACT_CURE_700:
			{
				return "heal 700 hit points every 250 turns";
			}
			case ACT_CURE_1000:
			{
				return "heal 1000 hit points every 888 turns";
			}
			case ACT_ESP:
			{
				return "temporary ESP (dur 25+d30) every 200 turns";
			}
			case ACT_BERSERK:
			{
				return "heroism and berserk (dur 50+d50) every 100+d100 turns";
			}
			case ACT_PROT_EVIL:
			{
				return "protect evil (dur level*3 + d25) every 225+d225 turns";
			}
			case ACT_RESIST_ALL:
			{
				return "resist elements (dur 40+d40) every 200 turns";
			}
			case ACT_SPEED:
			{
				return "speed (dur 20+d20) every 250 turns";
			}
			case ACT_XTRA_SPEED:
			{
				return "speed (dur 75+d75) every 200+d200 turns";
			}
			case ACT_WRAITH:
			{
				return "wraith form (level/2 + d(level/2)) every 1000 turns";
			}
			case ACT_INVULN:
			{
				return "invulnerability (dur 8+d8) every 1000 turns";
			}
			case ACT_LIGHT:
			{
				return "light area (dam 3d15) every 10+d10 turns";
			}
			case ACT_MAP_LIGHT:
			{
				return "light (dam 3d15) & map area every 50+d50 turns";
			}
			case ACT_DETECT_ALL:
			{
				return "detection every 55+d55 turns";
			}
			case ACT_DETECT_XTRA:
			{
				return "detection, probing and identify true every 1000 turns";
			}
			case ACT_ID_FULL:
			{
				return "identify true every 750 turns";
			}
			case ACT_ID_PLAIN:
			{
				return "identify spell every 10 turns";
			}
			case ACT_RUNE_EXPLO:
			{
				return "explosive rune every 200 turns";
			}
			case ACT_RUNE_PROT:
			{
				return "rune of protection every 400 turns";
			}
			case ACT_SATIATE:
			{
				return "satisfy hunger every 200 turns";
			}
			case ACT_DEST_DOOR:
			{
				return "destroy doors every 10 turns";
			}
			case ACT_STONE_MUD:
			{
				return "stone to mud every 5 turns";
			}
			case ACT_RECHARGE:
			{
				return "recharging every 70 turns";
			}
			case ACT_ALCHEMY:
			{
				return "alchemy every 500 turns";
			}
			case ACT_DIM_DOOR:
			{
				return "dimension door every 100 turns";
			}
			case ACT_TELEPORT:
			{
				return "teleport (range 100) every 45 turns";
			}
			case ACT_RECALL:
			{
				return "word of recall every 200 turns";
			}
			default:
			{
				return "something undefined";
			}
		}
	}

	/* Some artifacts can be activated */
	switch (o_ptr->name1)
	{
		case ART_STORMBRINGER:
		{
			return "summon its brothers every 1000 turns";
		}
		case ART_NARTHANC:
		{
			return "fire bolt (14d8) every 8+d8 turns";
		}
		case ART_NIMTHANC:
		{
			return "frost bolt (9d8) every 7+d7 turns";
		}
		case ART_DETHANC:
		{
			return "lightning bolt (6d8) every 6+d6 turns";
		}
		case ART_RILIA:
		{
			return "stinking cloud (18) every 4+d4 turns";
		}
		case ART_BELANGIL:
		{
			return "frost ball (72) every 5+d5 turns";
		}
		case ART_GLAMDRING: case ART_ORCRIST:
		case ART_STING: case ART_ORCHAST:
		{
			return "detect orcs every 10 turns";
		}
		case ART_DAL:
		{
			return "remove fear and cure poison every 5 turns";
		}
		case ART_RINGIL:
		{
			return "frost ball (150) every 150 turns";
		}
		case ART_DAWN:
		{
			return "summon the Legion of the Dawn every 500+d500 turns";
		}
		case ART_KANAJANA:
		{
			return "radiation ball (300, radius 4) every 200 turns";
		}
		case ART_FIRESTAR:
		{
			return "large fire ball (108) every 65 turns";
		}
		case ART_FEANOR:
		{
			return "haste self (20+d20 turns) every 200 turns";
		}
		case ART_THEODEN:
		{
			return "drain life (180) every 75 turns";
		}
		case ART_TURMIL:
		{
			return "drain life (140) every 50 turns";
		}
		case ART_BRYIONAK:
		{
			return "summon the Bull of Crinanass every 200+d200 turns";
		}
		case ART_CASPANION:
		{
			return "door and trap destruction every 10 turns";
		}
		case ART_PAN_TANG:
		{
			return "demon summoning every 400 turns";
		}
		case ART_AVAVIR:
		{
			return "word of recall every 200 turns";
		}
		case ART_TARATOL:
		{
			return "haste self (20+d20 turns) every 100+d100 turns";
		}
		case ART_ERIRIL:
		{
			return "identify every 10 turns";
		}
		case ART_OLORIN:
		{
			return "detection and probing every 100 turns";
		}
		case ART_EONWE:
		{
			return "mass genocide every 1000 turns";
		}
		case ART_LOTHARANG:
		{
			return "cure wounds (4d7) every 3+d3 turns";
		}
		case ART_WRATH:
		{
			return "word of destruction every 120 turns";
		}
		case ART_CUBRAGOL:
		{
			return "fire branding of bolts every 999 turns";
		}
		case ART_ANGUIREL:
		{
			return "a getaway every 35 turns";
		}
		case ART_AEGLOS:
		{
			return "lightning ball (150) every 250 turns";
		}
		case ART_OROME:
		{
			return "stone to mud every 5 turns";
		}
		case ART_SOULKEEPER:
		{
			return "heal (1000) every 500 turns";
		}
		case ART_BELEGENNON:
		{
			return ("heal (777), curing and heroism every 300 turns");
		}
		case ART_CELEBORN:
		{
			return "genocide every 500 turns";
		}
		case ART_LUTHIEN:
		{
			return "Sleep II every 35 turns";
		}
		case ART_ULMO:
		{
			return "teleport away every 150 turns";
		}
		case ART_COLLUIN:
		{
			return "resistance (25+d25 turns) every 100 turns";
		}
		case ART_HOLCOLLETH:
		{
			return "restore life levels every 450 turns";
		}
		case ART_THINGOL:
		{
			return "recharge item I every 70 turns";
		}
		case ART_COLANNON:
		{
			return "teleport every 45 turns";
		}
		case ART_TOTILA:
		{
			return "confuse monster every 15 turns";
		}
		case ART_CAMMITHRIM:
		{
			return "magic missile (3d6) every 2 turns";
		}
		case ART_PAURHACH:
		{
			return "fire bolt (14d8) every 8+d8 turns";
		}
		case ART_PAURNIMMEN:
		{
			return "frost bolt (9d8) every 7+d7 turns";
		}
		case ART_PAURAEGEN:
		{
			return "lightning bolt (6d8) every 6+d6 turns";
		}
		case ART_PAURNEN:
		{
			return "acid bolt (8d8) every 5+d5 turns";
		}
		case ART_FINGOLFIN:
		{
			return "a magical arrow (225) every 90+d90 turns";
		}
		case ART_HOLHENNETH:
		{
			return "detection every 55 turns";
		}
		case ART_GONDOR:
		{
			return "heal (700) every 250 turns";
		}
		case ART_RAZORBACK:
		{
			return "star ball (225) every 250 turns";
		}
		case ART_BLADETURNER:
		{
			return "breathe elements (450), berserk rage, bless, and resistance";
		}
		case ART_GALADRIEL:
		{
			return "illumination every 20 turns";
		}
		case ART_ELENDIL:
		{
			return "magic mapping and light every 30 turns";
		}
		case ART_THRAIN:
		{
			return "clairvoyance every 150 turns.";
		}
		case ART_INGWE:
		{
			return "dispel evil (400) every 200+d200 turns";
		}
		case ART_RED_AMULET:
		{
			return "charm monsters (x8) every 150+d150 turns";
		}
		case ART_TULKAS:
		{
			return "haste self (75+d75 turns) every 150+d150 turns";
		}
		case ART_NARYA:
		{
			return "large fire ball (150) every 50+d50 turns";
		}
		case ART_NENYA:
		{
			return "large frost ball (200) every 75+d75 turns";
		}
		case ART_VILYA:
		{
			return "large lightning ball (250) every 100+d100 turns";
		}
		case ART_POWER:
		{
			return "bizarre things every 200+d200 turns";
		}
		case ART_MELNIBONE: case ART_TERROR_MASK:
		{
			return "scare monsters every 75 turns";
		}
		case ART_KWLL:
		{
			return "draw forth undead from Limbo every 500 turns";
		}
		case ART_COWARDICE:
		{
			return "teleport level every 2 turns";
		}
		case ART_BASHER:
		{
			return "word of destruction every 100+d100 turns";
		}
		case ART_WHIRLWIND:
		{
			return "whirlwind attack every 250 turns";
		}
		case ART_ARIANROD:
		{
			return "alter reality every 100 turns";
		}
		case ART_RETALIATOR:
		{
			return "confuse monsters (lvl*6) every 75+d75 turns";
		}
		case ART_ENTANGLEMENT:
		{
			return "slow monsters every 25+d25 turns";
		}
		case ART_NIGHT:
		{
			return "vampiric drain (3*100) every 250 turns";
		}
		case ART_ENERGY:
		{
			return "lightning bolt (18d8) every 10+d10 turns";
		}
		case ART_NATUREBANE:
		{
			return "dispel animals (300) every 200+d200 turns";
		}
		case ART_CATAPULT:
		{
			return "a ball of pitch (50) every 25+d25 turns";
		}
		case ART_HERMES:
		{
			return "word of recall every 50+d50 turns";
		}
		case ART_DEATH:
		{
			return "death to life every 500+d500 turns";
		}
	}

	switch (o_ptr->name2)
	{
		case EGO_TRUMP:
		{
			return "teleport every 50+d50 turns";
		}
		case EGO_WEST:
		{
			return "detect orcs every 10 turns";
		}
		case EGO_SPINES:
		{
			return "shoot spines (dam 80) every 100+d100 turns";
		}
	}

	if (o_ptr->tval == TV_RING)
	{
		switch(o_ptr->sval)
		{
			case SV_RING_TELEPORTATION:
				return "teleportation every 250 turns";
			case SV_RING_LIGHTNING:
				return "lightning ball and resist electricity every 50 turns";
			case SV_RING_ACID:
				return "acid ball and resist acid every 50 turns";
			case SV_RING_FLAMES:
				return "fire ball and resist fire every 50 turns";
			case SV_RING_ICE:
				return "frost ball and resist cold every 50 turns";
			case SV_RING_SHADOWS:
				return "wraithform every 500+d500 turns";
			default:
				return NULL;
		}
	}

	/* Require dragon scale mail */
	if (o_ptr->tval != TV_DRAG_ARMOR) return (NULL);

	/* Branch on the sub-type */
	switch (o_ptr->sval)
	{
		case SV_DRAGON_BLUE:
		{
			return "breathe lightning (200) every 75+d75 turns";
		}
		case SV_DRAGON_WHITE:
		{
			return "breathe frost (250) every 75+d75 turns";
		}
		case SV_DRAGON_BLACK:
		{
			return "breathe acid (200) every 75+d75 turns";
		}
		case SV_DRAGON_GREEN:
		{
			return "breathe poison gas (275) every 75+d75 turns";
		}
		case SV_DRAGON_RED:
		{
			return "breathe fire (250) every 75+d75 turns";
		}
		case SV_DRAGON_MULTIHUED:
		{
			return "breathe multi-hued (300) every 75+d75 turns";
		}
		case SV_DRAGON_BRONZE:
		{
			return "breathe confusion (300) every 75+d75 turns";
		}
		case SV_DRAGON_GOLD:
		{
			return "breathe sound (300) every 75+d75 turns";
		}
		case SV_DRAGON_CHAOS:
		{
			return "breathe chaos/disenchant (350) every 75+d75 turns";
		}
		case SV_DRAGON_LAW:
		{
			return "breathe sound/shards (350) every 75+d75 turns";
		}
		case SV_DRAGON_BALANCE:
		{
			return "You breathe balance (400) every 75+d75 turns";
		}
		case SV_DRAGON_SHINING:
		{
			return "breathe light/darkness (300) every 75+d75 turns";
		}
		case SV_DRAGON_POWER:
		{
			return "breathe the elements (450) every 100+d100 turns";
		}
	}

	/* Oops */
	return NULL;
}


void random_artifact_resistance(object_type * o_ptr)
{
	bool give_resistance = FALSE, give_power = FALSE;

	/* Terror Mask is for Warriors... */
	if (o_ptr->name1 == ART_TERROR_MASK)
	{
		if (p_ptr->pclass == CLASS_WARRIOR)
		{
			give_power = TRUE;
			give_resistance = TRUE;
		}
		else
		{
			o_ptr->art_flags3 |= 
			    (TR3_TY_CURSE | TR3_CURSED | TR3_HEAVY_CURSE | TR3_AUTO_CURSE | TR3_DRAIN_EXP);
			o_ptr->ident |= IDENT_CURSED;
			return;
		}
	}

	switch(o_ptr->name1)
	{
		case ART_CELEBORN: case ART_ARVEDUI: case ART_CASPANION:
		case ART_PAN_TANG: case ART_HITHLOMIR: case ART_ROHIRRIM:
		case ART_CELEGORM: case ART_ANARION: case ART_THRANDUIL:
		case ART_THENGEL: case ART_LUTHIEN: case ART_THROR:
		case ART_THORIN: case ART_NIMTHANC: case ART_DETHANC:
		case ART_NARTHANC: case ART_TURMIL: case ART_THALKETTOTH:
		case ART_HACKMEAT: case ART_DRAGONBANE: case ART_SOULSUCKER:
		case ART_FUMA_LA_URSO: case ART_STING: case ART_ENDURANCE:
			{
				/* Give a resistance */
				give_resistance = TRUE;
			}
			break;
		case ART_MAEDHROS: case ART_GLAMDRING: case ART_ORCRIST:
		case ART_KANAJANA: case ART_ZARCUTHRA: case ART_GURTHANG:
		case ART_HARADEKKET: case ART_CUBRAGOL: case ART_DAWN:
		case ART_MORDAGA: case ART_COWARDICE: case ART_WHIRLWIND:
		case ART_HAMMERHAND: case ART_PAURHACH: case ART_PAURNIMMEN:
		case ART_PAURAEGEN: case ART_PAURNEN:
			{
				/* Give a resistance OR a power */
				if (randint(2)==1) give_resistance = TRUE;
				else give_power = TRUE;
			}
			break;
		case ART_NENYA: case ART_VILYA: case ART_BERUTHIEL:
		case ART_FINGOLFIN: case ART_THINGOL: case ART_ULMO:
		case ART_OLORIN: case ART_LIMBSLICER: case ART_ENERGY:
		case ART_CATAPULT: case ART_THAUMATURGIST:
			{
				/* Give a power */
				give_power = TRUE;
			}
			break;
		case ART_POWER: case ART_GONDOR: case ART_AULE:
		case ART_NATUREBANE:
			{
				/* Give both */
				give_power = TRUE;
				give_resistance = TRUE;
			}
			break;
	}

	if (give_power)
	{
		o_ptr->xtra1 = EGO_XTRA_ABILITY;

		/* Randomize the "xtra" power */
		if (o_ptr->xtra1) o_ptr->xtra2 = randint(256);
	}

	artifact_bias = 0;

	if (give_resistance)
	{
		random_resistance(o_ptr, FALSE, ((randint(22))+16));
	}
}


