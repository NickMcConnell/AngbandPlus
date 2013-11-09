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
#define TABLE_NAME      20
#define A_CURSED        13
#define WEIRD_LUCK      48
#define BIAS_LUCK       40
#define IM_LUCK         14

/*
 * Bias luck needs to be higher than weird luck,
 * since it is usually tested several times...
 */
#define ACTIVATION_CHANCE 3


/*
 * Use for biased artifact creation
 */
static int artifact_bias;


/*
 * Choose one random sustain
 */
void one_sustain(object_type *o_ptr)
{
	switch (randint0(A_MAX))
	{
		case A_STR: add_flag(o_ptr->art_flags, TR_SUST_STR); break;
		case A_INT: add_flag(o_ptr->art_flags, TR_SUST_INT); break;
		case A_WIS: add_flag(o_ptr->art_flags, TR_SUST_WIS); break;
		case A_DEX: add_flag(o_ptr->art_flags, TR_SUST_DEX); break;
		case A_CON: add_flag(o_ptr->art_flags, TR_SUST_CON); break;
		case A_CHR: add_flag(o_ptr->art_flags, TR_SUST_CHR); break;
	}
}


/*
 * Choose one random high resistance
 */
void one_high_resistance(object_type *o_ptr)
{
	switch (randint0(12))
	{
		case  0: add_flag(o_ptr->art_flags, TR_RES_POIS);   break;
		case  1: add_flag(o_ptr->art_flags, TR_RES_LITE);   break;
		case  2: add_flag(o_ptr->art_flags, TR_RES_DARK);   break;
		case  3: add_flag(o_ptr->art_flags, TR_RES_SHARDS); break;
		case  4: add_flag(o_ptr->art_flags, TR_RES_BLIND);  break;
		case  5: add_flag(o_ptr->art_flags, TR_RES_CONF);   break;
		case  6: add_flag(o_ptr->art_flags, TR_RES_SOUND);  break;
		case  7: add_flag(o_ptr->art_flags, TR_RES_NETHER); break;
		case  8: add_flag(o_ptr->art_flags, TR_RES_STONE);  break;
		case  9: add_flag(o_ptr->art_flags, TR_RES_CHAOS);  break;
		case 10: add_flag(o_ptr->art_flags, TR_RES_DISEN);  break;
		case 11: add_flag(o_ptr->art_flags, TR_RES_FEAR);   break;
	}
}


/*
 * Choose one random high resistance ( except poison and disenchantment )
 */
void one_lordly_high_resistance(object_type *o_ptr)
{
	switch (randint0(10))
	{
		case 0: add_flag(o_ptr->art_flags, TR_RES_LITE);   break;
		case 1: add_flag(o_ptr->art_flags, TR_RES_DARK);   break;
		case 2: add_flag(o_ptr->art_flags, TR_RES_SHARDS); break;
		case 3: add_flag(o_ptr->art_flags, TR_RES_BLIND);  break;
		case 4: add_flag(o_ptr->art_flags, TR_RES_CONF);   break;
		case 5: add_flag(o_ptr->art_flags, TR_RES_SOUND);  break;
		case 6: add_flag(o_ptr->art_flags, TR_RES_NETHER); break;
		case 7: add_flag(o_ptr->art_flags, TR_RES_STONE);  break;
		case 8: add_flag(o_ptr->art_flags, TR_RES_CHAOS);  break;
		case 9: add_flag(o_ptr->art_flags, TR_RES_FEAR);   break;
	}
}


/*
 * Choose one random element resistance
 */
void one_ele_resistance(object_type *o_ptr)
{
	switch (randint0(4))
	{
		case  0: add_flag(o_ptr->art_flags, TR_RES_ACID); break;
		case  1: add_flag(o_ptr->art_flags, TR_RES_ELEC); break;
		case  2: add_flag(o_ptr->art_flags, TR_RES_COLD); break;
		case  3: add_flag(o_ptr->art_flags, TR_RES_FIRE); break;
	}
}


/*
 * Choose one random element or poison resistance
 */
void one_dragon_ele_resistance(object_type *o_ptr)
{
	if (one_in_(7))
	{
		add_flag(o_ptr->art_flags, TR_RES_POIS);
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
	switch (randint0(8))
	{
	case 0: add_flag(o_ptr->art_flags, TR_LEVITATION);     break;
	case 1: add_flag(o_ptr->art_flags, TR_LITE);        break;
	case 2: add_flag(o_ptr->art_flags, TR_SEE_INVIS);   break;
	case 3: add_flag(o_ptr->art_flags, TR_WARNING);     break;
	case 4: add_flag(o_ptr->art_flags, TR_SLOW_DIGEST); break;
	case 5: add_flag(o_ptr->art_flags, TR_REGEN);       break;
	case 6: add_flag(o_ptr->art_flags, TR_FREE_ACT);    break;
	case 7: add_flag(o_ptr->art_flags, TR_HOLD_LIFE);   break;
	}
}


static void curse_artifact(object_type * o_ptr)
{
	int i;

	for (i = 0; i < A_MAX; i++)
		if (o_ptr->to_stat[i] > 0) o_ptr->to_stat[i] = 0 - (o_ptr->to_stat[i] + randint1(4));
	for (i = 0; i < OB_MAX; i++)
		if (o_ptr->to_misc[i] > 0) o_ptr->to_misc[i] = 0 - (o_ptr->to_misc[i] + randint1(4));
	if (o_ptr->to_align[ALI_GNE] > 0) o_ptr->to_align[ALI_GNE] = 0 - o_ptr->to_align[ALI_GNE];
	if (o_ptr->to_a > 0) o_ptr->to_a = 0 - (o_ptr->to_a + randint1(4));
	if (o_ptr->to_h > 0) o_ptr->to_h = 0 - (o_ptr->to_h + randint1(4));
	if (o_ptr->to_d > 0) o_ptr->to_d = 0 - (o_ptr->to_d + randint1(4));

	o_ptr->curse_flags |= (TRC_HEAVY_CURSE | TRC_CURSED);

	if (have_flag(o_ptr->art_flags, TR_BLESSED))
	{
		remove_flag(o_ptr->art_flags, TR_BLESSED);
		add_flag(o_ptr->art_flags, TR_UNHOLY);
	}

	if (one_in_(4)) o_ptr->curse_flags |= TRC_PERMA_CURSE;
	if (one_in_(3)) add_flag(o_ptr->art_flags, TR_TY_CURSE);
	if (one_in_(2)) add_flag(o_ptr->art_flags, TR_AGGRAVATE);
	if (one_in_(3)) add_flag(o_ptr->art_flags, TR_DRAIN_EXP);
	if (one_in_(2)) add_flag(o_ptr->art_flags, TR_TELEPORT);
	else if (one_in_(3)) add_flag(o_ptr->art_flags, TR_NO_TELE);

	if (!class_info[p_ptr->pclass].realm_choices && one_in_(3))
		add_flag(o_ptr->art_flags, TR_NO_MAGIC);
}


static bool randart_stat_bonus = FALSE;
static int  randart_misc_bonus_cur = 0;
static int  randart_misc_bonus_max = 0;
static bool randart_imm = FALSE;


static void random_plus(object_type * o_ptr)
{
	int this_type = (object_is_weapon(o_ptr) ? 23 : 19);

	switch (randint1(this_type))
	{
	case 1: case 2:
		if (!randart_stat_bonus)
		{
			o_ptr->to_stat[A_STR] = 1;
			randart_stat_bonus = TRUE;
		}
		break;
	case 3: case 4:
		if (!randart_stat_bonus)
		{
			o_ptr->to_stat[A_INT] = 1;
			randart_stat_bonus = TRUE;
		}
		break;
	case 5: case 6:
		if (!randart_stat_bonus)
		{
			o_ptr->to_stat[A_WIS] = 1;
			randart_stat_bonus = TRUE;
		}
		break;
	case 7: case 8:
		if (!randart_stat_bonus)
		{
			o_ptr->to_stat[A_DEX] = 1;
			randart_stat_bonus = TRUE;
		}
		break;
	case 9: case 10:
		if (!randart_stat_bonus)
		{
			o_ptr->to_stat[A_CON] = 1;
			randart_stat_bonus = TRUE;
		}
		break;
	case 11: case 12:
		if (!randart_stat_bonus)
		{
			o_ptr->to_stat[A_CHR] = 1;
			randart_stat_bonus = TRUE;
		}
		break;
	case 13: case 14:
		if (randart_misc_bonus_cur < randart_misc_bonus_max)
		{
			o_ptr->to_misc[OB_STEALTH] = 1;
			randart_misc_bonus_cur++;
		}
		break;
	case 15: case 16:
		if (randart_misc_bonus_cur < randart_misc_bonus_max)
		{
			o_ptr->to_misc[OB_SEARCH] = 1;
			randart_misc_bonus_cur++;
		}
		break;
	case 17: case 18:
		if (randart_misc_bonus_cur < randart_misc_bonus_max)
		{
			o_ptr->to_misc[OB_INFRA] = 1;
			randart_misc_bonus_cur++;
		}
		break;
	case 19:
		if (randart_misc_bonus_cur < randart_misc_bonus_max)
		{
			o_ptr->to_misc[OB_SPEED] = 1;
			randart_misc_bonus_cur++;
		}
		break;
	case 20: case 21:
		if (randart_misc_bonus_cur < randart_misc_bonus_max)
		{
			if (o_ptr->tval == TV_BOW) random_plus(o_ptr);
			else
			{
				o_ptr->to_misc[OB_TUNNEL] = 1;
				randart_misc_bonus_cur++;
			}
		}
		break;
	case 22: case 23:
		if (o_ptr->tval == TV_BOW) random_plus(o_ptr);
		else o_ptr->to_misc[OB_BLOWS] = 1;
		break;
	}
}


static void random_resistance(object_type * o_ptr)
{
	switch (artifact_bias)
	{
	case BIAS_FIRE:
		if (!(have_flag(o_ptr->art_flags, TR_RES_FIRE)))
		{
			add_flag(o_ptr->art_flags, TR_RES_FIRE);
			if (one_in_(2)) return;
		}
		if (object_is_body_armour(o_ptr) &&
		    !(have_flag(o_ptr->art_flags, TR_SH_FIRE)))
		{
			add_flag(o_ptr->art_flags, TR_SH_FIRE);
			if (one_in_(2)) return;
		}
		if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_FIRE)) && !randart_imm)
		{
			randart_imm = TRUE;
			add_flag(o_ptr->art_flags, TR_IM_FIRE);
			if (!one_in_(IM_LUCK))
			{
				remove_flag(o_ptr->art_flags, TR_IM_ELEC);
				remove_flag(o_ptr->art_flags, TR_IM_COLD);
				remove_flag(o_ptr->art_flags, TR_IM_ACID);
			}
			if (one_in_(2)) return;
		}
		break;

	case BIAS_AQUA:
		if (!(have_flag(o_ptr->art_flags, TR_RES_COLD)))
		{
			add_flag(o_ptr->art_flags, TR_RES_COLD);
			if (one_in_(2)) return;
		}
		if (object_is_body_armour(o_ptr) &&
		    !(have_flag(o_ptr->art_flags, TR_SH_COLD)))
		{
			add_flag(o_ptr->art_flags, TR_SH_COLD);
			if (one_in_(2)) return;
		}
		if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_COLD)) && !randart_imm)
		{
			randart_imm = TRUE;
			add_flag(o_ptr->art_flags, TR_IM_COLD);
			if (!one_in_(IM_LUCK))
			{
				remove_flag(o_ptr->art_flags, TR_IM_ELEC);
				remove_flag(o_ptr->art_flags, TR_IM_ACID);
				remove_flag(o_ptr->art_flags, TR_IM_FIRE);
			}
			if (one_in_(2)) return;
		}
		break;

	case BIAS_EARTH:
		if (!(have_flag(o_ptr->art_flags, TR_RES_ACID)))
		{
			add_flag(o_ptr->art_flags, TR_RES_ACID);
			if (one_in_(2)) return;
		}
		if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_ACID)) && !randart_imm)
		{
			randart_imm = TRUE;
			add_flag(o_ptr->art_flags, TR_IM_ACID);
			if (!one_in_(IM_LUCK))
			{
				remove_flag(o_ptr->art_flags, TR_IM_ELEC);
				remove_flag(o_ptr->art_flags, TR_IM_COLD);
				remove_flag(o_ptr->art_flags, TR_IM_FIRE);
			}
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_RES_SHARDS)))
		{
			add_flag(o_ptr->art_flags, TR_RES_SHARDS);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_WIND:
		if (!(have_flag(o_ptr->art_flags, TR_RES_ELEC)))
		{
			add_flag(o_ptr->art_flags, TR_RES_ELEC);
			if (one_in_(2)) return;
		}
		if (object_is_body_armour(o_ptr) &&
		    !(have_flag(o_ptr->art_flags, TR_SH_ELEC)))
		{
			add_flag(o_ptr->art_flags, TR_SH_ELEC);
			if (one_in_(2)) return;
		}
		if (one_in_(BIAS_LUCK) && !(have_flag(o_ptr->art_flags, TR_IM_ELEC)) && !randart_imm)
		{
			randart_imm = TRUE;
			add_flag(o_ptr->art_flags, TR_IM_ELEC);
			if (!one_in_(IM_LUCK))
			{
				remove_flag(o_ptr->art_flags, TR_IM_ACID);
				remove_flag(o_ptr->art_flags, TR_IM_COLD);
				remove_flag(o_ptr->art_flags, TR_IM_FIRE);
			}
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_RES_SOUND)))
		{
			add_flag(o_ptr->art_flags, TR_RES_SOUND);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_LIGHT:
		if (!(have_flag(o_ptr->art_flags, TR_RES_LITE)))
		{
			add_flag(o_ptr->art_flags, TR_RES_LITE);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_DARK:
		if (!(have_flag(o_ptr->art_flags, TR_RES_DARK)))
		{
			add_flag(o_ptr->art_flags, TR_RES_DARK);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_RES_POIS)))
		{
			add_flag(o_ptr->art_flags, TR_RES_POIS);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_RES_NETHER)))
		{
			add_flag(o_ptr->art_flags, TR_RES_NETHER);
			if (one_in_(2)) return;
		}
		break;
	}

	switch (randint1(42))
	{
		case 1:
			if (!one_in_(WEIRD_LUCK) || randart_imm)
				random_resistance(o_ptr);
			else
			{
				randart_imm = TRUE;
				add_flag(o_ptr->art_flags, TR_IM_ACID);
			}
			break;
		case 2:
			if (!one_in_(WEIRD_LUCK) || randart_imm)
				random_resistance(o_ptr);
			else
			{
				randart_imm = TRUE;
				add_flag(o_ptr->art_flags, TR_IM_ELEC);
			}
			break;
		case 3:
			if (!one_in_(WEIRD_LUCK) || randart_imm)
				random_resistance(o_ptr);
			else
			{
				randart_imm = TRUE;
				add_flag(o_ptr->art_flags, TR_IM_COLD);
			}
			break;
		case 4:
			if (!one_in_(WEIRD_LUCK) || randart_imm)
				random_resistance(o_ptr);
			else
			{
				randart_imm = TRUE;
				add_flag(o_ptr->art_flags, TR_IM_FIRE);
			}
			break;
		case 5:
		case 6:
		case 13:
			add_flag(o_ptr->art_flags, TR_RES_ACID);
			break;
		case 7:
		case 8:
		case 14:
			add_flag(o_ptr->art_flags, TR_RES_ELEC);
			break;
		case 9:
		case 10:
		case 15:
			add_flag(o_ptr->art_flags, TR_RES_FIRE);
			break;
		case 11:
		case 12:
		case 16:
			add_flag(o_ptr->art_flags, TR_RES_COLD);
			break;
		case 17:
		case 18:
			add_flag(o_ptr->art_flags, TR_RES_POIS);
			break;
		case 19:
		case 20:
			add_flag(o_ptr->art_flags, TR_RES_FEAR);
			break;
		case 21:
			add_flag(o_ptr->art_flags, TR_RES_LITE);
			break;
		case 22:
			add_flag(o_ptr->art_flags, TR_RES_DARK);
			break;
		case 23:
		case 24:
			add_flag(o_ptr->art_flags, TR_RES_BLIND);
			break;
		case 25:
		case 26:
			add_flag(o_ptr->art_flags, TR_RES_CONF);
			break;
		case 27:
		case 28:
			add_flag(o_ptr->art_flags, TR_RES_SOUND);
			break;
		case 29:
		case 30:
			add_flag(o_ptr->art_flags, TR_RES_SHARDS);
			break;
		case 31:
		case 32:
			add_flag(o_ptr->art_flags, TR_RES_NETHER);
			break;
		case 33:
		case 34:
			add_flag(o_ptr->art_flags, TR_RES_STONE);
			break;
		case 35:
		case 36:
			add_flag(o_ptr->art_flags, TR_RES_CHAOS);
			break;
		case 37:
		case 38:
			add_flag(o_ptr->art_flags, TR_RES_DISEN);
			break;
		case 39:
			if (object_is_body_armour(o_ptr))
				add_flag(o_ptr->art_flags, TR_SH_ELEC);
			else
				random_resistance(o_ptr);
			break;
		case 40:
			if (object_is_body_armour(o_ptr))
				add_flag(o_ptr->art_flags, TR_SH_FIRE);
			else
				random_resistance(o_ptr);
			break;
		case 41:
			if (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK ||
			    o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR)
				add_flag(o_ptr->art_flags, TR_REFLECT);
			else
				random_resistance(o_ptr);
			break;
		case 42:
			if (object_is_body_armour(o_ptr))
				add_flag(o_ptr->art_flags, TR_SH_COLD);
			else
				random_resistance(o_ptr);
			break;
	}
}



static void random_misc(object_type * o_ptr)
{
	switch (artifact_bias)
	{
	case BIAS_FIRE:
		if (!(have_flag(o_ptr->art_flags, TR_SUST_STR)))
		{
			add_flag(o_ptr->art_flags, TR_SUST_STR);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_LITE)))
		{
			add_flag(o_ptr->art_flags, TR_LITE); /* Freebie */
			if (one_in_(2)) return;
		}
		break;

	case BIAS_AQUA:
		if (!(have_flag(o_ptr->art_flags, TR_SUST_STR)))
		{
			add_flag(o_ptr->art_flags, TR_SUST_STR);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_EARTH:
		if (!(have_flag(o_ptr->art_flags, TR_SUST_CHR)))
		{
			add_flag(o_ptr->art_flags, TR_SUST_CHR);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_WIND:
		if (!(have_flag(o_ptr->art_flags, TR_SUST_DEX)))
		{
			add_flag(o_ptr->art_flags, TR_SUST_DEX);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_DARK:
		if (!have_flag(o_ptr->art_flags, TR_FEAR_FIELD) && one_in_(10))
		{
			add_flag(o_ptr->art_flags, TR_FEAR_FIELD);
			if (one_in_(2)) return;
		}
		/* Fall through */
	case BIAS_LIGHT:
		if (o_ptr->tval == TV_SOFT_ARMOR)
		{
			if (!have_flag(o_ptr->art_flags, TR_REGEN_MANA) && one_in_(100))
			{
				add_flag(o_ptr->art_flags, TR_REGEN_MANA);
				if (one_in_(2)) return;
			}
		}
		break;
	}

	switch (randint1(33))
	{
		case 1:
			add_flag(o_ptr->art_flags, TR_SUST_STR);
			break;
		case 2:
			add_flag(o_ptr->art_flags, TR_SUST_INT);
			break;
		case 3:
			add_flag(o_ptr->art_flags, TR_SUST_WIS);
			break;
		case 4:
			add_flag(o_ptr->art_flags, TR_SUST_DEX);
			break;
		case 5:
			add_flag(o_ptr->art_flags, TR_SUST_CON);
			break;
		case 6:
			add_flag(o_ptr->art_flags, TR_SUST_CHR);
			break;
		case 7:
		case 8:
		case 14:
			add_flag(o_ptr->art_flags, TR_FREE_ACT);
			break;
		case 9:
			add_flag(o_ptr->art_flags, TR_HOLD_LIFE);
			break;
		case 10:
		case 11:
			add_flag(o_ptr->art_flags, TR_LITE);
			break;
		case 12:
		case 13:
			add_flag(o_ptr->art_flags, TR_LEVITATION);
			break;
		case 15:
		case 16:
		case 17:
			add_flag(o_ptr->art_flags, TR_SEE_INVIS);
			break;
		case 18:
			if (one_in_(3)) break;
			add_flag(o_ptr->art_flags, TR_TELEPATHY);
			break;
		case 19:
		case 20:
			add_flag(o_ptr->art_flags, TR_SLOW_DIGEST);
			break;
		case 21:
		case 22:
			add_flag(o_ptr->art_flags, TR_REGEN);
			break;
		case 23:
			add_flag(o_ptr->art_flags, TR_TELEPORT);
			break;
		case 24:
		case 25:
		case 26:
			if (object_is_armour(o_ptr))
				random_misc(o_ptr);
			else
			{
				o_ptr->to_a = 4 + randint1(11);
			}
			break;
		case 27:
		case 28:
		case 29:
		{
			int bonus_h, bonus_d;
			add_flag(o_ptr->art_flags, TR_SHOW_MODS);
			bonus_h = 4 + (randint1(11));
			bonus_d = 4 + (randint1(11));
			if (!object_is_weapon(o_ptr) && (o_ptr->tval != TV_GLOVES) && (o_ptr->tval != TV_RING))
			{
				bonus_h /= 2;
				bonus_d /= 2;
			}
			o_ptr->to_h += bonus_h;
			o_ptr->to_d += bonus_d;
			break;
		}
		case 30:
			add_flag(o_ptr->art_flags, TR_NO_MAGIC);
			break;
		case 31:
			add_flag(o_ptr->art_flags, TR_NO_TELE);
			break;
		case 32:
			add_flag(o_ptr->art_flags, TR_WARNING);
			break;
		case 33:
			if (randint1(100) > 9) break;
			add_flag(o_ptr->art_flags, TR_RES_MAGIC);
			break;
	}
}


static void random_slay(object_type *o_ptr)
{
	if (o_ptr->tval == TV_BOW)
	{
		switch (randint1(6))
		{
			case 1:
			case 2:
			case 3:
				if (o_ptr->sval != SV_ROCKET_LAUNCHER)
				{
					add_flag(o_ptr->art_flags, TR_XTRA_MIGHT);
					if (!one_in_(7)) remove_flag(o_ptr->art_flags, TR_XTRA_SHOTS);
				}
				break;
			default:
				add_flag(o_ptr->art_flags, TR_XTRA_SHOTS);
				if (!one_in_(7)) remove_flag(o_ptr->art_flags, TR_XTRA_MIGHT);
			break;
		}

		return;
	}

	switch (artifact_bias)
	{
	case BIAS_FIRE:
		if (!(have_flag(o_ptr->art_flags, TR_BRAND_FIRE)))
		{
			add_flag(o_ptr->art_flags, TR_BRAND_FIRE);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_AQUA:
		if (!(have_flag(o_ptr->art_flags, TR_BRAND_COLD)))
		{
			add_flag(o_ptr->art_flags, TR_BRAND_COLD);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_EARTH:
		if (!(have_flag(o_ptr->art_flags, TR_BRAND_ACID)))
		{
			add_flag(o_ptr->art_flags, TR_BRAND_ACID);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_WIND:
		if (!(have_flag(o_ptr->art_flags, TR_BRAND_ELEC)))
		{
			add_flag(o_ptr->art_flags, TR_BRAND_ELEC);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_LIGHT:
		if (!(have_flag(o_ptr->art_flags, TR_SLAY_EVIL)))
		{
			add_flag(o_ptr->art_flags, TR_SLAY_EVIL);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_SLAY_UNDEAD)))
		{
			add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_SLAY_DEMON)))
		{
			add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
			if (one_in_(2)) return;
		}
		break;

	case BIAS_DARK:
		if (!(have_flag(o_ptr->art_flags, TR_SLAY_GOOD)))
		{
			add_flag(o_ptr->art_flags, TR_SLAY_GOOD);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_SLAY_LIVING)))
		{
			add_flag(o_ptr->art_flags, TR_SLAY_LIVING);
			if (one_in_(2)) return;
		}
		if (!(have_flag(o_ptr->art_flags, TR_UNHOLY)))
		{
			add_flag(o_ptr->art_flags, TR_UNHOLY);
			if (one_in_(2)) return;
		}
		break;
	}

	switch (randint1(41))
	{
		case 1:
		case 2:
			add_flag(o_ptr->art_flags, TR_SLAY_ANIMAL);
			break;
		case 3:
		case 4:
			add_flag(o_ptr->art_flags, TR_SLAY_EVIL);
			break;
		case 5:
		case 6:
			add_flag(o_ptr->art_flags, TR_SLAY_UNDEAD);
			break;
		case 7:
		case 8:
			add_flag(o_ptr->art_flags, TR_SLAY_DEMON);
			break;
		case 9:
		case 10:
			add_flag(o_ptr->art_flags, TR_SLAY_ORC);
			break;
		case 11:
		case 12:
			add_flag(o_ptr->art_flags, TR_SLAY_TROLL);
			break;
		case 13:
		case 14:
			add_flag(o_ptr->art_flags, TR_SLAY_GIANT);
			break;
		case 15:
		case 16:
			add_flag(o_ptr->art_flags, TR_SLAY_DRAGON);
			break;
		case 17:
			add_flag(o_ptr->art_flags, TR_KILL_DRAGON);
			break;
		case 18:
		case 19:
			if (o_ptr->tval == TV_SWORD)
				add_flag(o_ptr->art_flags, TR_VORPAL);
			else
				random_slay(o_ptr);
			break;
		case 20:
			add_flag(o_ptr->art_flags, TR_IMPACT);
			break;
		case 21:
		case 22:
			add_flag(o_ptr->art_flags, TR_BRAND_FIRE);
			break;
		case 23:
		case 24:
			add_flag(o_ptr->art_flags, TR_BRAND_COLD);
			break;
		case 25:
		case 26:
			add_flag(o_ptr->art_flags, TR_BRAND_ELEC);
			break;
		case 27:
		case 28:
			add_flag(o_ptr->art_flags, TR_BRAND_ACID);
			break;
		case 29:
		case 30:
			add_flag(o_ptr->art_flags, TR_BRAND_POIS);
			break;
		case 31:
			add_flag(o_ptr->art_flags, TR_VAMPIRIC);
			break;
		case 32:
			add_flag(o_ptr->art_flags, TR_FORCE_WEAPON);
			break;
		case 33:
		case 34:
			add_flag(o_ptr->art_flags, TR_SLAY_HUMAN);
			break;
		case 35:
		case 36:
			add_flag(o_ptr->art_flags, TR_SLAY_GOOD);
			break;
		case 37:
		case 38:
			add_flag(o_ptr->art_flags, TR_SLAY_LIVING);
			break;
		case 39:
			if (o_ptr->tval == TV_SWORD)
			{
				if (!one_in_(100)) break;
				add_flag(o_ptr->art_flags, TR_EXTRA_VORPAL);
			}
			else
				random_slay(o_ptr);
			break;
		default:
			add_flag(o_ptr->art_flags, TR_CHAOTIC);
			break;
	}
}


static void give_activation_power(object_type *o_ptr)
{
	int type = 0, chance = 0;

	switch (artifact_bias)
	{
	case BIAS_FIRE:
		switch (randint1(15))
		{
		case 1:
			type = ACT_BO_MISS_1;
			chance = 100;
			break;

		case 2:
			type = ACT_TERROR;
			chance = 100;
			break;

		case 3:
			type = ACT_CONFUSE;
			chance = 100;
			break;

		case 4:
			type = ACT_SLEEP;
			chance = 90;
			break;

		case 5:
			type = ACT_BO_FIRE_1;
			chance = 100;
			break;

		case 6:
			type = ACT_BA_FIRE_1;
			chance = 80;
			break;

		case 7:
			type = ACT_BA_FIRE_2;
			chance = 60;
			break;

		case 8:
			type = ACT_BERSERK;
			chance = 40;
			break;

		case 9:
			type = ACT_INVULN;
			chance = 4;
			break;

		case 10:
			type = ACT_BA_MISS_3;
			chance = 15;
			break;

		case 11:
			type = ACT_SUMMON_ELEMENTAL;
			chance = 40;
			break;

		case 12:
			type = ACT_SUMMON1;
			chance = 7;
			break;

		case 13:
			type = ACT_DEC_RAIN;
			chance = 20;
			break;

		case 14:
			type = ACT_DEC_TEMP;
			chance = 20;
			break;

		case 15:
			type = ACT_SH_FIRE;
			chance = 50;
			break;
		}
		break;

	case BIAS_AQUA:
		switch (randint1(16))
		{
		case 1:
			type = ACT_BO_COLD_1;
			chance = 100;
			break;

		case 2:
			type = ACT_BA_COLD_1;
			chance = 100;
			break;

		case 3:
			type = ACT_BA_COLD_2;
			chance = 80;
			break;

		case 4:
			type = ACT_BA_COLD_3;
			chance = 60;
			break;

		case 5:
			type = ACT_CURE_LW;
			chance = 90;
			break;

		case 6:
			type = ACT_CURE_MW;
			chance = 100;
			break;

		case 7:
			type = ACT_CURE_POISON;
			chance = 100;
			break;

		case 8:
			type = ACT_REST_LIFE;
			chance = 60;
			break;

		case 9:
			type = ACT_REST_ALL;
			chance = 40;
			break;

		case 10:
			type = ACT_CURE_700;
			chance = 30;
			break;

		case 11:
			type = ACT_CURE_1000;
			chance = 20;
			break;

		case 12:
			type = ACT_WRAITH;
			chance = 4;
			break;

		case 13:
			type = ACT_SUMMON2;
			chance = 7;
			break;

		case 14:
			type = ACT_INC_RAIN;
			chance = 20;
			break;

		case 15:
			type = ACT_INC_TEMP;
			chance = 20;
			break;

		case 16:
			type = ACT_SH_COLD;
			chance = 50;
			break;
		}
		break;

	case BIAS_EARTH:
		switch (randint1(17))
		{
		case 1:
			type = ACT_BA_POIS_1;
			chance = 100;
			break;

		case 2:
			type = ACT_BO_ACID_1;
			chance = 100;
			break;

		case 3:
			type = ACT_STONE_MUD;
			chance = 100;
			break;

		case 4:
			type = ACT_ALCHEMY;
			chance = 40;
			break;

		case 5:
			type = ACT_SATIATE;
			chance = 80;
			break;

		case 6:
			type = ACT_ROCKET;
			chance = 30;
			break;

		case 7:
			type = ACT_RUNE_EXPLO;
			chance = 20;
			break;

		case 8:
			type = ACT_DEST_DOOR;
			chance = 100;
			break;

		case 9:
			type = ACT_RESIST_ALL;
			chance = 25;
			break;

		case 10:
			type = ACT_RECHARGE;
			chance = 35;
			break;

		case 11:
			type = ACT_QUAKE;
			chance = 50;
			break;

		case 12:
			type = ACT_CHARM_ANIMAL;
			chance = 70;
			break;

		case 13:
			type = ACT_CHARM_ANIMALS;
			chance = 55;
			break;

		case 14:
			type = ACT_SUMMON_ANIMAL;
			chance = 40;
			break;

		case 15:
			type = ACT_SUMMON3;
			chance = 7;
			break;

		case 16:
			type = ACT_DEC_WIND;
			chance = 20;
			break;

		case 17:
			type = ACT_SH_SHARDS;
			chance = 50;
			break;
		}
		break;

	case BIAS_WIND:
		switch (randint1(14))
		{
		case 1:
			type = ACT_BO_MISS_2;
			chance = 40;
			break;

		case 2:
			type = ACT_WHIRLWIND;
			chance = 60;
			break;

		case 3:
			type = ACT_BO_ELEC_1;
			chance = 100;
			break;

		case 4:
			type = ACT_BA_ELEC_2;
			chance = 80;
			break;

		case 5:
			type = ACT_BA_ELEC_3;
			chance = 60;
			break;

		case 6:
			type = ACT_TELE_AWAY;
			chance = 70;
			break;

		case 7:
			type = ACT_DIM_DOOR;
			chance = 10;
			break;

		case 8:
			type = ACT_TELEPORT;
			chance = 80;
			break;

		case 9:
			type = ACT_RECALL;
			chance = 60;
			break;

		case 10:
			type = ACT_SPEED;
			chance = 35;
			break;

		case 11:
			type = ACT_XTRA_SPEED;
			chance = 25;
			break;

		case 12:
			type = ACT_SUMMON4;
			chance = 7;
			break;

		case 13:
			type = ACT_INC_WIND;
			chance = 20;
			break;

		case 14:
			type = ACT_SH_ELEC;
			chance = 50;
			break;
		}
		break;

	case BIAS_LIGHT:
		switch (randint1(15))
		{
		case 1:
			type = ACT_SUNLIGHT;
			chance = 95;
			break;

		case 2:
			type = ACT_DISP_EVIL;
			chance = 50;
			break;

		case 3:
			type = ACT_BANISH_EVIL;
			chance = 50;
			break;

		case 4:
			type = ACT_PROT_EVIL;
			chance = 80;
			break;

		case 5:
			type = ACT_LIGHT;
			chance = 100;
			break;

		case 6:
			type = ACT_MAP_LIGHT;
			chance = 35;
			break;

		case 7:
			type = ACT_DETECT_ALL;
			chance = 25;
			break;

		case 8:
			type = ACT_DETECT_XTRA;
			chance = 10;
			break;

		case 9:
			type = ACT_ID_FULL;
			chance = 30;
			break;

		case 10:
			type = ACT_ID_PLAIN;
			chance = 60;
			break;

		case 11:
			type = ACT_ESP;
			chance = 30;
			break;

		case 12:
			type = ACT_RUNE_PROT;
			chance = 15;
			break;

		case 13:
			type = ACT_CHARM_OTHERS;
			chance = 50;
			break;

		case 14:
			type = ACT_CHARM_OTHER;
			chance = 90;
			break;

		case 15:
			type = ACT_SUMMON5;
			chance = 7;
			break;
		}
		break;

	case BIAS_DARK:
		switch (randint1(13))
		{
		case 1:
			type = ACT_DRAIN_1;
			chance = 90;
			break;

		case 2:
			type = ACT_DRAIN_2;
			chance = 60;
			break;

		case 3:
			type = ACT_VAMPIRE_1;
			chance = 80;
			break;

		case 4:
			type = ACT_VAMPIRE_2;
			chance = 50;
			break;

		case 5:
			type = ACT_CALL_CHAOS;
			chance = 30;
			break;

		case 6:
			type = ACT_DISP_GOOD;
			chance = 30;
			break;

		case 7:
			type = ACT_GENOCIDE;
			chance = 12;
			break;

		case 8:
			type = ACT_MASS_GENO;
			chance = 8;
			break;

		case 9:
			type = ACT_SUMMON_PHANTOM;
			chance = 50;
			break;

		case 10:
			type = ACT_CHARM_UNDEAD;
			chance = 75;
			break;

		case 11:
			type = ACT_SUMMON_DEMON;
			chance = 40;
			break;

		case 12:
			type = ACT_SUMMON_UNDEAD;
			chance = 35;
			break;

		case 13:
			type = ACT_SUMMON6;
			chance = 7;
			break;
		}
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
			case ACT_SH_FIRE:
			case ACT_SH_ELEC:
			case ACT_SH_COLD:
			case ACT_SH_SHARDS:
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
			case ACT_SUMMON1:
			case ACT_SUMMON2:
			case ACT_SUMMON3:
			case ACT_SUMMON4:
			case ACT_SUMMON5:
			case ACT_SUMMON6:
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
			case ACT_DEC_RAIN:
			case ACT_INC_RAIN:
			case ACT_DEC_WIND:
			case ACT_INC_WIND:
			case ACT_DEC_TEMP:
			case ACT_INC_TEMP:
				chance = 20;
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
	add_flag(o_ptr->art_flags, TR_ACTIVATE);
	o_ptr->timeout = 0;
}


static void get_random_name(char *return_name, bool armour, int power)
{
	if (randint1(100) <= TABLE_NAME)
	{
		get_table_name(return_name);
	}
	else
	{
		cptr filename;

		switch (armour)
		{
			case 1:
				switch (power)
				{
					case 0:
#ifdef JP
						filename = "a_cursed_j.txt";
#else
						filename = "a_cursed.txt";
#endif

						break;
					case 1:
#ifdef JP
						filename = "a_low_j.txt";
#else
						filename = "a_low.txt";
#endif

						break;
					case 2:
#ifdef JP
						filename = "a_med_j.txt";
#else
						filename = "a_med.txt";
#endif

						break;
					default:
#ifdef JP
						filename = "a_high_j.txt";
#else
						filename = "a_high.txt";
#endif

				}
				break;
			default:
				switch (power)
				{
					case 0:
#ifdef JP
						filename = "w_cursed_j.txt";
#else
						filename = "w_cursed.txt";
#endif

						break;
					case 1:
#ifdef JP
						filename = "w_low_j.txt";
#else
						filename = "w_low.txt";
#endif

						break;
					case 2:
#ifdef JP
						filename = "w_med_j.txt";
#else
						filename = "w_med.txt";
#endif

						break;
					default:
#ifdef JP
						filename = "w_high_j.txt";
#else
						filename = "w_high.txt";
#endif

				}
		}

		(void)get_rnd_line(filename, artifact_bias, return_name);
#ifdef JP
		if(return_name[0]==0)get_table_name(return_name);
#endif
	}
}


typedef struct bonus_limit_type
{
	int chance;
	int dice;
	int max;
}
bonus_limit_type;

static bonus_limit_type misc_bonus_limit[OB_MAX] =
{
	{1, 2, 9},
	{1, 2, 9},
	{2, 3, 10},
	{2, 3, 10},
	{2, 3, 10},
	{1, 3, 10},
	{1, 24, 3},
	{1, 24, 3},
};

bool create_artifact(object_type *o_ptr, bool a_scroll)
{
	char    new_name[1024];
	int     powers = randint1(5) + 1;
	int     max_type = (object_is_weapon(o_ptr) ? 7 : 5);
	int     power_level;
	s32b    total_flags;
	bool    a_cursed = FALSE;
	int     i;

	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* Reset artifact bias */
	artifact_bias = 0;

	/* Nuke enchantments */
	o_ptr->name1 = 0;
	o_ptr->name2 = 0;

	for (i = 0; i < TR_FLAG_SIZE; i++)
		o_ptr->art_flags[i] |= k_ptr->flags[i];

	if (a_scroll && !one_in_(5))
	{
		switch (p_ptr->pelem)
		{
			case ELEM_FIRE:
				artifact_bias = BIAS_FIRE;
				break;
			case ELEM_AQUA:
				artifact_bias = BIAS_AQUA;
				break;
			case ELEM_EARTH:
				artifact_bias = BIAS_EARTH;
				break;
			case ELEM_WIND:
				artifact_bias = BIAS_WIND;
				break;
			default: /* Paranoia */
				artifact_bias = BIAS_DARK;
				break;
		}
	}
	else artifact_bias = randint1(6);
	if (o_ptr->tval == TV_SWORD && (o_ptr->sval == SV_YOUTOU || o_ptr->sval == SV_DARK_SWORD) && artifact_bias == BIAS_LIGHT) artifact_bias = BIAS_DARK;
	if (o_ptr->tval == TV_HAFTED && (o_ptr->sval == SV_MIGHTY_HAMMER) && artifact_bias == BIAS_LIGHT) artifact_bias = BIAS_DARK;

	strcpy(new_name, "");

	if (!a_scroll && one_in_(A_CURSED))
		a_cursed = TRUE;
	if (((o_ptr->tval == TV_AMULET) || (o_ptr->tval == TV_RING)) && object_is_cursed(o_ptr))
		a_cursed = TRUE;

	while (one_in_(powers) || one_in_(7) || one_in_(10))
		powers++;

	if (!a_cursed && one_in_(WEIRD_LUCK))
		powers *= 2;

	/* Randomly sexual restriction */
	if ((o_ptr->tval >= TV_BOOTS) && !have_flag(o_ptr->art_flags, TR_FEMALE_ONLY) &&
		!have_flag(o_ptr->art_flags, TR_MALE_ONLY) && !a_scroll)
	{
		switch (randint1(100))
		{
		case 1: case 2: case 3: case 4:
			add_flag(o_ptr->art_flags, TR_FEMALE_ONLY);
			break;
		case 5: case 6: case 7: case 8:
			add_flag(o_ptr->art_flags, TR_MALE_ONLY);
			break;
		}
	}
	if (have_flag(o_ptr->art_flags, TR_FEMALE_ONLY) || have_flag(o_ptr->art_flags, TR_MALE_ONLY) || a_scroll)
	{
		powers *= 2;
	}

	if (a_cursed) powers /= 2;

	/* add extra ignore_flag*/
	if (powers > 5)
	{
	add_flag(o_ptr->art_flags, TR_IGNORE_ACID);
	add_flag(o_ptr->art_flags, TR_IGNORE_FIRE);
	add_flag(o_ptr->art_flags, TR_IGNORE_ELEC);
	add_flag(o_ptr->art_flags, TR_IGNORE_COLD);
	}
	
	if (o_ptr->tval == TV_LITE) add_flag(o_ptr->art_flags, TR_LITE);
	
	randart_stat_bonus = FALSE;
	randart_misc_bonus_cur = 0;
	switch (o_ptr->tval)
	{
	case TV_BOOTS:
	case TV_GLOVES:
	case TV_HELM:
	case TV_CROWN:
	case TV_SHIELD:
	case TV_CLOAK:
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_LITE:
	case TV_AMULET:
	case TV_RING:
	case TV_CARD:
		randart_misc_bonus_max = 2;
		break;
	default:
		randart_misc_bonus_max = 1;
		break;
	}
	randart_imm = FALSE;

	for (i = 0; (i < A_MAX) && !randart_stat_bonus; i++)
	{
		if (o_ptr->to_stat[i] && !have_flag(k_ptr->flags, a_to_tr[i]))
			randart_stat_bonus = TRUE;
	}
	for (i = 0; (i < OB_MAX) && (randart_misc_bonus_cur < randart_misc_bonus_max); i++)
	{
		if ((i != OB_BLOWS) && o_ptr->to_misc[i] && !have_flag(k_ptr->flags, ob_to_tr[i]))
			randart_misc_bonus_cur++;
	}

	/* Main loop */
	while (powers--)
	{
		switch (randint1(max_type))
		{
			case 1: case 2:
				if (object_is_ammo(o_ptr)) random_slay(o_ptr);
				else random_plus(o_ptr);
				break;
			case 3: case 4:
				if (one_in_(2) && object_is_weapon(o_ptr) && (o_ptr->tval != TV_BOW))
				{
					if (a_cursed && !one_in_(13)) break;
					if (one_in_(13))
					{
						if (one_in_(o_ptr->ds+4)) o_ptr->ds++;
					}
					else
					{
						if (one_in_(o_ptr->dd+1)) o_ptr->dd++;
					}
				}
				else
					random_resistance(o_ptr);
				break;
			case 5:
				if (object_is_ammo(o_ptr)) random_slay(o_ptr);
				else random_misc(o_ptr);
				break;
			case 6: case 7:
				random_slay(o_ptr);
				break;
			default:
				if (p_ptr->wizard) msg_print("Switch error in create_artifact!");
				powers++;
		}
	};

	randart_stat_bonus = FALSE;
	randart_misc_bonus_cur = 0;
	randart_misc_bonus_max = 0;
	randart_imm = FALSE;

	for (i = 0; i < A_MAX; i++)
	{
		if (o_ptr->to_stat[i])
		{
			while (one_in_(2)) o_ptr->to_stat[i]++;
			if ((o_ptr->to_stat[i] > 9) && !one_in_(WEIRD_LUCK)) o_ptr->to_stat[i] = 9;
		}
	}
	for (i = 0; i < OB_MAX; i++)
	{
		if (o_ptr->to_misc[i])
		{
			while (randint1(misc_bonus_limit[i].dice) <= misc_bonus_limit[i].chance) o_ptr->to_misc[i]++;
			if ((o_ptr->to_misc[i] > misc_bonus_limit[i].max) && !one_in_(WEIRD_LUCK))
				o_ptr->to_misc[i] = misc_bonus_limit[i].max;
		}
	}
	for (i = 0; i < ALI_MAX; i++)
	{
		if ((o_ptr->to_align[i]) || one_in_(5))
		{
			if (one_in_(2)) o_ptr->to_align[i] += randint1(10);
			else o_ptr->to_align[i] -= randint1(10);

			if (!have_flag(k_ptr->flags, ali_to_tr[i])) add_flag(o_ptr->art_flags, ali_to_tr[i]);
			if (o_ptr->to_align[i] == 0) remove_flag(k_ptr->flags, ali_to_tr[i]);
		}
	}

	if (o_ptr->to_misc[OB_BLOWS] > misc_bonus_limit[OB_BLOWS].max)
		o_ptr->to_misc[OB_BLOWS] = misc_bonus_limit[OB_BLOWS].max;

	/* give it some plusses... */
	if (object_is_armour(o_ptr))
		o_ptr->to_a += randint1(o_ptr->to_a > 19 ? 1 : 20 - o_ptr->to_a);
	else if (object_is_weapon(o_ptr))
	{
		o_ptr->to_h += randint1(o_ptr->to_h > 19 ? 1 : 20 - o_ptr->to_h);
		o_ptr->to_d += randint1(o_ptr->to_d > 19 ? 1 : 20 - o_ptr->to_d);
		if ((o_ptr->to_stat[A_WIS] > 0) && (!have_flag(k_ptr->flags, TR_UNHOLY)) && (!(o_ptr->tval == TV_SWORD && (o_ptr->sval == SV_YOUTOU || o_ptr->sval == SV_DARK_SWORD)))) add_flag(o_ptr->art_flags, TR_BLESSED);
	}

	/* Just to be sure */
	switch (artifact_bias)
	{
	case BIAS_FIRE:
		add_flag(o_ptr->art_flags, TR_IGNORE_FIRE);
		if (object_is_weapon(o_ptr)) o_ptr->to_d += randint1(10);
		break;

	case BIAS_AQUA:
		add_flag(o_ptr->art_flags, TR_IGNORE_COLD);
		if (object_is_armour(o_ptr)) o_ptr->to_a += 7 + randint1(5);
		break;

	case BIAS_EARTH:
		add_flag(o_ptr->art_flags, TR_IGNORE_ACID);
		if (object_is_armour(o_ptr)) o_ptr->to_a += 15 + randint1(10);
		o_ptr->weight = o_ptr->weight * 5 / 4;
		break;

	case BIAS_WIND:
		add_flag(o_ptr->art_flags, TR_IGNORE_ELEC);
		if (object_is_armour(o_ptr)) o_ptr->to_h += randint1(10);
		o_ptr->weight = o_ptr->weight * 4 / 5;
		break;
	case BIAS_LIGHT:
		if (o_ptr->to_align[ALI_GNE] < 0) o_ptr->to_align[ALI_GNE] = 0 - o_ptr->to_align[ALI_GNE];
		break;
	case BIAS_DARK:
		if (o_ptr->to_align[ALI_GNE] > 0) o_ptr->to_align[ALI_GNE] = 0 - o_ptr->to_align[ALI_GNE];
		break;
	}

	total_flags = flag_cost(o_ptr);
	if (cheat_peek) msg_format("%ld", total_flags);

	if (a_cursed) curse_artifact(o_ptr);

	if (!a_cursed &&
	    (randint1((object_is_armour(o_ptr))
	    ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE) == 1))
	{
		o_ptr->xtra2 = 0;
		give_activation_power(o_ptr);
	}

	if (object_is_armour(o_ptr))
	{
		while ((o_ptr->to_d+o_ptr->to_h) > 20)
		{
			if (one_in_(o_ptr->to_d) && one_in_(o_ptr->to_h)) break;
			o_ptr->to_d -= (s16b)randint0(3);
			o_ptr->to_h -= (s16b)randint0(3);
		}
		while ((o_ptr->to_d+o_ptr->to_h) > 10)
		{
			if (one_in_(o_ptr->to_d) || one_in_(o_ptr->to_h)) break;
			o_ptr->to_d -= (s16b)randint0(3);
			o_ptr->to_h -= (s16b)randint0(3);
		}
	}

	if (object_is_weapon(o_ptr))
	{
		if (a_cursed) power_level = 0;
		else if (total_flags < 15000) power_level = 1;
		else if (total_flags < 25000) power_level = 2;
		else power_level = 3;
	}

	else
	{
		if (a_cursed) power_level = 0;
		else if (total_flags < 20000) power_level = 1;
		else if (total_flags < 35000) power_level = 2;
		else power_level = 3;
	}

	if (a_scroll)
	{
		char dummy_name[80];

		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);

		/* Mark the item as fully known */
		o_ptr->ident |= (IDENT_MENTAL);

		strcpy(dummy_name, "");
		(void)screen_object(o_ptr, NULL, TRUE);

#ifdef JP
		if (!(get_string("このアーティファクトを何と名付けますか？", dummy_name, 80)))
#else
		if (!(get_string("What do you want to call the artifact? ", dummy_name, 80)))
#endif

		{
			get_random_name(new_name, object_is_armour(o_ptr), power_level);
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
	}
	else
	{
		get_random_name(new_name, object_is_armour(o_ptr), power_level);
	}

	if (cheat_xtra)
	{
#ifdef JP
		msg_format("エレメント %d を持ったアーティファクト。", artifact_bias);
#else
		msg_format("Artifact has element %d.", artifact_bias);
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
	int vit = p_ptr->stat_use[A_CON];
	int k, dir, dummy = 0;

	if (!o_ptr->art_name) return FALSE; /* oops? */

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
			msg_print("それは眩しいくらいに明るく輝いている...");
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
			msg_print("それは濃緑色に脈動している...");
#else
			msg_print("It throbs deep green...");
#endif

			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, 12, 3, FALSE);
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
			o_ptr->timeout = randint0(5) + 5;
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
			o_ptr->timeout = randint0(6) + 6;
			break;
		}

		case ACT_BO_COLD_1:
		{
#ifdef JP
			msg_print("それは霜に覆われた...");
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
			msg_print("それは霜に覆われた...");
#else
			msg_print("It is covered in frost...");
#endif

			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 48, 2, FALSE);
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
			fire_ball(GF_FIRE, dir, 72, 2, FALSE);
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
			drain_life(dir, 100);
			o_ptr->timeout = randint0(100) + 100;
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
			fire_ball(GF_COLD, dir, 100, 2, FALSE);
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
			fire_ball(GF_ELEC, dir, 100, 3, FALSE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
#ifdef JP
			msg_print("黒く輝いている...");
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
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
				fire_bolt(GF_NEW_DRAIN, dir, 50);
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
			fire_bolt(GF_EDGED, dir, 150);
			o_ptr->timeout = randint0(90) + 90;
			break;
		}

		case ACT_BA_FIRE_2:
		{
#ifdef JP
			msg_print("深赤色に輝いている...");
#else
			msg_print("It glows deep red...");
#endif

			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, 120, 3, FALSE);
			o_ptr->timeout = randint0(225) + 225;
			break;
		}

		case ACT_BA_COLD_3:
		{
#ifdef JP
			msg_print("明るく白色に輝いている...");
#else
			msg_print("It glows bright white...");
#endif

			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_COLD, dir, 200, 3, FALSE);
			o_ptr->timeout = randint0(325) + 325;
			break;
		}

		case ACT_BA_ELEC_3:
		{
#ifdef JP
			msg_print("深青色に輝いている...");
#else
			msg_print("It glows deep blue...");
#endif

			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ELEC, dir, 250, 3, FALSE);
			o_ptr->timeout = randint0(425) + 425;
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
						py_attack(y, x, 0);
				}
			}
			o_ptr->timeout = 250;
			break;
		}

		case ACT_VAMPIRE_2:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			for (dummy = 0; dummy < 3; dummy++)
				fire_bolt(GF_NEW_DRAIN, dir, 100);

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
#ifdef JP
			msg_print("様々な色の火花を発している...");
#else
			msg_print("It glows in scintillating colours...");
#endif

			call_chaos(p_ptr->lev);
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

			fire_ball(GF_ROCKET, dir, 250 + plev*3, 2, FALSE);
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
			o_ptr->timeout = randint0(300) + 300;
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
			o_ptr->timeout = randint0(300) + 300;
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

			fire_ball(GF_MISSILE, dir, 300, -4, FALSE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_SUMMON1:
		{
			msg_print("炎の女神ゾショネルを召喚した！ 猛烈な熱風と炎が視界を覆い尽くす。");
			(void)summon_god(GF_PURE_FIRE, randint1(vit * 2) + vit * 5);
			o_ptr->timeout = 666;
			break;
		}

		case ACT_SUMMON2:
		{
			msg_print("水の女神グルーザを召喚した！ 猛烈な氷柱と冷気の嵐が渦巻く。");
			(void)summon_god(GF_PURE_AQUA, randint1(vit * 2) + vit * 5);
			o_ptr->timeout = 666;
			break;
		}

		case ACT_SUMMON3:
		{
			msg_print("大地の女神バーサを召喚した！ 猛烈な無数の岩石が乱舞する。");
			(void)summon_god(GF_PURE_EARTH, randint1(vit * 2) + vit * 5);
			o_ptr->timeout = 666;
			break;
		}

		case ACT_SUMMON4:
		{
			msg_print("風の女神ハーネラを召喚した！ 猛烈な風圧と真空の刃が踊り狂う。");
			(void)summon_god(GF_PURE_WIND, randint1(vit * 2) + vit * 5);
			o_ptr->timeout = 666;
			break;
		}

		case ACT_SUMMON5:
		{
			msg_print("光の神イシュタルを召喚した！ 周囲に強烈な閃光と神聖なオーラがあふれる。");
			(void)summon_god(GF_HOLY_FIRE, randint1(vit * 2) + vit * 7);
			o_ptr->timeout = 666;
			break;
		}

		case ACT_SUMMON6:
		{
			msg_print("闇の神アスモデを召喚した！ 周囲に濃密な暗黒と邪悪な障気がたちこめる。");
			(void)summon_god(GF_HELL_FIRE, randint1(vit * 2) + vit * 7);
			o_ptr->timeout = 666;
			break;
		}

		case ACT_DEC_RAIN:
		{
			msg_print("空気が乾燥していく...");
			set_weather(0 - (4 + randint1(4)), 0, 0);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_INC_RAIN:
		{
			msg_print("空気が湿っていく...");
			set_weather(4 + randint1(4), 0, 0);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DEC_WIND:
		{
			msg_print("風が止まっていく...");
			set_weather(0, 0 - (4 + randint1(4)), 0);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_INC_WIND:
		{
			msg_print("風が強まっていく...");
			set_weather(0, 4 + randint1(4), 0);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DEC_TEMP:
		{
			msg_print("空気が暖まる...");
			set_weather(0, 0, 0 - (4 + randint1(4)));
			o_ptr->timeout = 500;
			break;
		}

		case ACT_INC_TEMP:
		{
			msg_print("空気が凍える...");
			set_weather(0, 0, 4 + randint1(4));
			o_ptr->timeout = 500;
			break;
		}

		case ACT_SH_FIRE:
		{
			set_tim_sh_fire(20 + randint1(20), FALSE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_SH_ELEC:
		{
			set_tim_sh_elec(20 + randint1(20), FALSE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_SH_COLD:
		{
			set_tim_sh_cold(20 + randint1(20), FALSE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_SH_SHARDS:
		{
			set_dustrobe(20 + randint1(20), FALSE);
			o_ptr->timeout = 500;
			break;
		}

		/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
#ifdef JP
			msg_print("様々な色の火花を発している...");
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
			msg_print("深青色に輝いている...");
#else
			msg_print("It glows deep blue...");
#endif

			sleep_monsters_touch(p_ptr->lev);
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
			msg_print("深青色に輝いている...");
#else
			msg_print("It glows deep blue...");
#endif

			(void)symbol_genocide(200, TRUE);
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

			(void)mass_genocide(200, TRUE);
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
			(void)summon_specific(-1, py, px, plev, SUMMON_ANIMAL_RANGER, (PM_ALLOW_GROUP | PM_FORCE_PET));
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

			(void)summon_specific(-1, py, px, dun_level, SUMMON_PHANTOM, (PM_ALLOW_GROUP | PM_FORCE_PET));
			o_ptr->timeout = 200 + randint1(200);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = one_in_(3);
			u32b mode = 0L;

			if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;
			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), SUMMON_ELEMENTAL, mode))
			{
#ifdef JP
				msg_print("エレメンタルが現れた...");
#else
				msg_print("An elemental materializes...");
#endif


				if (pet)
#ifdef JP
					msg_print("あなたに服従しているようだ。");
#else
					msg_print("It seems obedient to you.");
#endif

				else
#ifdef JP
					msg_print("それをコントロールできなかった！");
#else
					msg_print("You fail to control it!");
#endif

			}

			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			bool pet = one_in_(3);
			u32b mode = 0L;

			if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;
			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), SUMMON_DEMON, mode))
			{
#ifdef JP
				msg_print("硫黄の悪臭が充満した。");
#else
				msg_print("The area fills with a stench of sulphur and brimstone.");
#endif

				if (pet)
#ifdef JP
					msg_print("「ご用でございますか、ご主人様」");
#else
					msg_print("'What is thy bidding... Master?'");
#endif

				else
#ifdef JP
					msg_print("「NON SERVIAM! Wretch! お前の魂を頂くぞ！」");
#else
					msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
#endif

			}

			o_ptr->timeout = 666 + randint1(333);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			bool pet = one_in_(3);
			int type;
			u32b mode = 0L;

			type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);

			if (!pet || ((plev > 24) && one_in_(3))) mode |= PM_ALLOW_GROUP;
			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);

			if (summon_specific((pet ? -1 : 0), py, px, ((plev * 3) / 2), type, mode))
			{
#ifdef JP
				msg_print("冷たい風があなたの周りに吹き始めた。それは腐敗臭を運んでいる...");
#else
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
#endif

				if (pet)
#ifdef JP
					msg_print("古えの死せる者共があなたに仕えるため土から甦った！");
#else
					msg_print("Ancient, long-dead forms arise from the ground to serve you!");
#endif

				else
#ifdef JP
					msg_print("死者が甦った。眠りを妨げるあなたを罰するために！");
#else
					msg_print("'The dead arise... to punish you for disturbing them!'");
#endif

			}

			o_ptr->timeout = 666 + randint1(333);
			break;
		}

		/* Activate for healing */

		case ACT_CURE_LW:
		{
			(void)set_afraid(0);
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
#ifdef JP
			msg_print("深紫色の光を発している...");
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
			msg_print("深青色に輝いている...");
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
			msg_print("深紅に輝いている...");
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
			msg_print("濃緑色に輝いている...");
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
			msg_print("深青色に輝いている...");
#else
			msg_print("It glows deep blue...");
#endif

#ifdef JP
			msg_print("体内に暖かい鼓動が感じられる...");
#else
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
			msg_print("白く明るく輝いている...");
#else
			msg_print("It glows a bright white...");
#endif

#ifdef JP
			msg_print("ひじょうに気分がよい...");
#else
			msg_print("You feel much better...");
#endif

			(void)hp_player(1000);
			(void)set_cut(0);
			o_ptr->timeout = 888;
			break;
		}

		/* Activate for timed effect */

		case ACT_ESP:
		{
			(void)set_tim_esp(randint1(30) + 25, FALSE);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			(void)set_afraid(0);
			(void)set_hero(randint1(50) + 50, FALSE);
			(void)set_blessed(randint1(50) + 50, FALSE);
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
			(void)set_protevil(randint1(25) + k, FALSE);
			o_ptr->timeout = randint0(225) + 225;
			break;
		}

		case ACT_RESIST_ALL:
		{
#ifdef JP
			msg_print("様々な色に輝いている...");
#else
			msg_print("It glows many colours...");
#endif

			(void)set_oppose_acid(randint1(40) + 40, FALSE);
			(void)set_oppose_elec(randint1(40) + 40, FALSE);
			(void)set_oppose_fire(randint1(40) + 40, FALSE);
			(void)set_oppose_cold(randint1(40) + 40, FALSE);
			(void)set_oppose_pois(randint1(40) + 40, FALSE);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
#ifdef JP
			msg_print("明るく緑色に輝いている...");
#else
			msg_print("It glows bright green...");
#endif

			(void)set_fast(randint1(20) + 20, FALSE);
			o_ptr->timeout = 250;
			break;
		}

		case ACT_XTRA_SPEED:
		{
#ifdef JP
			msg_print("明るく輝いている...");
#else
			msg_print("It glows brightly...");
#endif

			(void)set_fast(randint1(75) + 75, FALSE);
			o_ptr->timeout = randint0(200) + 200;
			break;
		}

		case ACT_WRAITH:
		{
			set_wraith_form(randint1(plev / 2) + (plev / 2), FALSE);
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			(void)set_invuln(randint1(8) + 8, FALSE);
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
			msg_print("眩しく輝いた...");
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
			msg_print("白く明るく輝いている...");
#else
			msg_print("It glows bright white...");
#endif

#ifdef JP
			msg_print("心にイメージが浮かんできた...");
#else
			msg_print("An image forms in your mind...");
#endif

			detect_all(DETECT_RAD_DEFAULT);
			o_ptr->timeout = randint0(55) + 55;
			break;
		}

		case ACT_DETECT_XTRA:
		{
#ifdef JP
			msg_print("明るく輝いている...");
#else
			msg_print("It glows brightly...");
#endif

			detect_all(DETECT_RAD_DEFAULT);
			probing();
			identify_fully(FALSE);
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
#ifdef JP
			msg_print("黄色く輝いている...");
#else
			msg_print("It glows yellow...");
#endif

			identify_fully(FALSE);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_ID_PLAIN:
		{
			if (!ident_spell(FALSE)) return FALSE;
			o_ptr->timeout = 10;
			break;
		}

		case ACT_RUNE_EXPLO:
		{
#ifdef JP
			msg_print("明るい赤色に輝いている...");
#else
			msg_print("It glows bright red...");
#endif

			explosive_rune(p_ptr->lev);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
#ifdef JP
			msg_print("ブルーに明るく輝いている...");
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
			msg_print("明るい赤色に輝いている...");
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
			msg_print("鼓動している...");
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
			recharge(130);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
#ifdef JP
			msg_print("明るい黄色に輝いている...");
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

			if (!dimension_door(p_ptr->lev)) return FALSE;
			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT:
		{
#ifdef JP
			msg_print("周りの空間が歪んでいる...");
#else
			msg_print("It twists space around you...");
#endif

			teleport_player(100);
			o_ptr->timeout = 45;
			break;
		}

		case ACT_RECALL:
		{
#ifdef JP
			msg_print("やわらかな白色に輝いている...");
#else
			msg_print("It glows soft white...");
#endif
			if (!word_of_recall()) return FALSE;
			o_ptr->timeout = 200;
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

	if (give_power)
	{
		one_ability(o_ptr);
	}

	if (give_resistance)
	{
		one_high_resistance(o_ptr);
	}
}


/*
 * Create the artifact of the specified number
 */
bool create_named_art(int a_idx, int y, int x)
{
	object_type forge;
	object_type *q_ptr;
	int i;

	artifact_type *a_ptr = &a_info[a_idx];

	/* Get local object */
	q_ptr = &forge;

	/* Ignore "empty" artifacts */
	if (!a_ptr->name) return FALSE;

	/* Acquire the "kind" index */
	i = lookup_kind(a_ptr->tval, a_ptr->sval);

	/* Oops */
	if (!i) return FALSE;

	/* Create the artifact */
	object_prep(q_ptr, i);

	/* Save the name */
	q_ptr->name1 = a_idx;

	/* Extract the fields */
	q_ptr->pval = a_ptr->pval;
	q_ptr->ac = a_ptr->ac;
	q_ptr->dd = a_ptr->dd;
	q_ptr->ds = a_ptr->ds;
	for (i = 0; i < A_MAX; i++) q_ptr->to_stat[i] = a_ptr->to_stat[i];
	for (i = 0; i < OB_MAX; i++) q_ptr->to_misc[i] = a_ptr->to_misc[i];
	for (i = 0; i < ALI_MAX; i++) q_ptr->to_align[i] = a_ptr->to_align[i];
	q_ptr->to_a = a_ptr->to_a;
	q_ptr->to_h = a_ptr->to_h;
	q_ptr->to_d = a_ptr->to_d;
	q_ptr->weight = a_ptr->weight;

	/* Hack -- extract the "cursed" flag */
	if (a_ptr->gen_flags & TRG_CURSED) q_ptr->curse_flags |= (TRC_CURSED);
	if (a_ptr->gen_flags & TRG_HEAVY_CURSE) q_ptr->curse_flags |= (TRC_HEAVY_CURSE);
	if (a_ptr->gen_flags & TRG_PERMA_CURSE) q_ptr->curse_flags |= (TRC_PERMA_CURSE);
	if (a_ptr->gen_flags & (TRG_RANDOM_CURSE0)) q_ptr->curse_flags |= get_curse(0, q_ptr);
	if (a_ptr->gen_flags & (TRG_RANDOM_CURSE1)) q_ptr->curse_flags |= get_curse(1, q_ptr);
	if (a_ptr->gen_flags & (TRG_RANDOM_CURSE2)) q_ptr->curse_flags |= get_curse(2, q_ptr);

	random_artifact_resistance(q_ptr, a_ptr);

	/* Drop the artifact from heaven */
	if (!drop_near(q_ptr, -1, y, x)) return FALSE;

	return TRUE;
}
