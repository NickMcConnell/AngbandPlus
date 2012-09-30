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

static void random_plus(object_type *o_ptr)
{
	switch (randint1(o_ptr->tval < TV_BOOTS ? 23 : 19))
	{
		case 1:  case 2:
			o_ptr->flags1 |= TR1_STR;
			break;
		case 3:  case 4:
			o_ptr->flags1 |= TR1_INT;
			break;
		case 5:  case 6:
			o_ptr->flags1 |= TR1_WIS;
			break;
		case 7:  case 8:
			o_ptr->flags1 |= TR1_DEX;
			break;
		case 9:  case 10:
			o_ptr->flags1 |= TR1_CON;
			break;
		case 11:  case 12:
			o_ptr->flags1 |= TR1_CHR;
			break;
		case 13:  case 14:
			o_ptr->flags1 |= TR1_STEALTH;
			break;
		case 15:  case 16:
			o_ptr->flags1 |= TR1_SEARCH;
			break;
		case 17:  case 18:
			o_ptr->flags1 |= TR1_INFRA;
			break;
		case 19:
			o_ptr->flags1 |= TR1_SPEED;
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
			}
			break;
	}
}


void random_resistance(object_type *o_ptr, int specific)
{
	switch (specific ? specific : randint1(42))
	{
		case 1:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_ACID;
			else
				o_ptr->flags2 |= TR2_IM_ACID;
			break;
		case 2:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_ELEC;
			else
				o_ptr->flags2 |= TR2_IM_ELEC;
			break;
		case 3:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_COLD;
			else
				o_ptr->flags2 |= TR2_IM_COLD;
			break;
		case 4:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_FIRE;
			else
				o_ptr->flags2 |= TR2_IM_FIRE;
			break;
		case 5:
		case 6:
		case 13:
			o_ptr->flags2 |= TR2_RES_ACID;
			break;
		case 7:
		case 8:
		case 14:
			o_ptr->flags2 |= TR2_RES_ELEC;
			break;
		case 9:
		case 10:
		case 15:
			o_ptr->flags2 |= TR2_RES_FIRE;
			break;
		case 11:
		case 12:
		case 16:
			o_ptr->flags2 |= TR2_RES_COLD;
			break;
		case 17:
		case 18:
			o_ptr->flags2 |= TR2_RES_POIS;
			break;
		case 19:
		case 20:
			o_ptr->flags2 |= TR2_RES_FEAR;
			break;
		case 21:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_LITE;
			else
				o_ptr->flags4 |= TR4_IM_LITE;
			break;
		case 22:
			if (!one_in_(WEIRD_LUCK))
				o_ptr->flags2 |= TR2_RES_DARK;
			else
				o_ptr->flags4 |= TR4_IM_DARK;
			break;
		case 23:
		case 24:
			o_ptr->flags2 |= TR2_RES_BLIND;
			break;
		case 25:
		case 26:
			o_ptr->flags2 |= TR2_RES_CONF;
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
			break;
		case 33:
		case 34:
			o_ptr->flags2 |= TR2_RES_NEXUS;
			break;
		case 35:
		case 36:
			o_ptr->flags2 |= TR2_RES_CHAOS;
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
			break;
		case 40:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->flags3 |= TR3_SH_FIRE;
			else
				o_ptr->flags2 |= TR2_RES_FIRE;
			break;
		case 41:
			if (o_ptr->tval >= TV_CLOAK && o_ptr->tval <= TV_HARD_ARMOR)
				o_ptr->flags4 |= TR4_SH_COLD;
			else
				o_ptr->flags2 |= TR2_RES_COLD;
			break;
		/* Note: SH_ACID is deliberately omitted here */
		case 42:
			if (o_ptr->tval == TV_SHIELD || o_ptr->tval == TV_CLOAK ||
				o_ptr->tval == TV_HELM || o_ptr->tval == TV_HARD_ARMOR)
				o_ptr->flags2 |= TR2_REFLECT;
			else
				o_ptr->flags2 |= TR2_RES_FEAR;
			break;
	}
}



static void random_misc(object_type *o_ptr)
{
	switch (randint1(37))
	{
		case 1:
			o_ptr->flags2 |= TR2_SUST_STR;
			break;
		case 2:
			o_ptr->flags2 |= TR2_SUST_INT;
			break;
		case 3:
			o_ptr->flags2 |= TR2_SUST_WIS;
			break;
		case 4:
			o_ptr->flags2 |= TR2_SUST_DEX;
			break;
		case 5:
			o_ptr->flags2 |= TR2_SUST_CON;
			break;
		case 6:
			o_ptr->flags2 |= TR2_SUST_CHR;
			break;
		case 7:
		case 8:
		case 14:
			o_ptr->flags2 |= TR2_FREE_ACT;
			break;
		case 9:
			o_ptr->flags2 |= TR2_HOLD_LIFE;
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
			if (o_ptr->tval == TV_GLOVES)
				o_ptr->flags4 |= TR4_GHOUL_TOUCH;
			else
				o_ptr->flags3 |= TR3_SEE_INVIS;

			break;
		case 16:
		case 17:
			o_ptr->flags3 |= TR3_SEE_INVIS;
			break;
		case 18:
			o_ptr->flags3 |= TR3_TELEPATHY;
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
			if (o_ptr->tval >= TV_BOOTS && o_ptr->tval < TV_LITE)
			{
				o_ptr->flags3 |= TR3_SLOW_DIGEST;
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
		case 32:
		case 33:
		case 34:
			/* A slay on a non-weapon gives protection */
			switch (randint1(8))
			{
				case 1: o_ptr->flags1 |= TR1_SLAY_ANIMAL; break;
				case 2: o_ptr->flags1 |= TR1_SLAY_EVIL; break;
				case 3: o_ptr->flags1 |= TR1_SLAY_UNDEAD; break;
				case 4: o_ptr->flags1 |= TR1_SLAY_DEMON; break;
				case 5: o_ptr->flags1 |= TR1_SLAY_ORC; break;
				case 6: o_ptr->flags1 |= TR1_SLAY_TROLL; break;
				case 7: o_ptr->flags1 |= TR1_SLAY_GIANT; break;
				case 8: o_ptr->flags1 |= TR1_SLAY_DRAGON; break;
			}
			break;
		case 35:
			o_ptr->flags4 |= TR4_MUTATE;
			break;
		case 36:
			o_ptr->flags4 |= TR4_PATRON;
			break;
		case 37:
			o_ptr->flags4 |= TR4_STRANGE_LUCK;
	}
}

static void random_curse(object_type *o_ptr, bool evil)
{
	switch (randint1(evil ? 26 : 16))
	{
		case 1:
		case 17:
			o_ptr->flags4 |= TR4_HURT_ACID;
			break;
		case 2:
		case 18:
			o_ptr->flags4 |= TR4_HURT_ELEC;
			break;
		case 3:
		case 19:
			o_ptr->flags4 |= TR4_HURT_FIRE;
			break;
		case 4:
		case 20:
			o_ptr->flags4 |= TR4_HURT_COLD;
			break;
		case 5:
			o_ptr->flags4 |= TR4_HURT_LITE;
			break;
		case 6:
			o_ptr->flags4 |= TR4_HURT_DARK;
			break;
		case 7:
		case 8:
			o_ptr->flags3 |= TR3_AGGRAVATE;
			break;
		case 9:
			o_ptr->flags4 |= TR4_SLOW_HEAL;
			break;
		case 10:
		case 21:
			o_ptr->flags4 |= TR4_DRAIN_STATS;
			break;
		case 11:
		case 12:
			o_ptr->flags4 |= TR4_AUTO_CURSE;
			break;
		case 13:
		case 14:
			o_ptr->flags4 |= TR4_CANT_EAT;
			break;
		case 15:
		case 16:
			o_ptr->flags3 |= TR3_CURSED;
			break;
		case 22:
		case 23:
			o_ptr->flags3 |= TR3_TELEPORT;
			break;
		case 24:
			o_ptr->flags3 |= TR3_DRAIN_EXP;
			break;
		case 25:
		case 26:
			o_ptr->flags3 |= TR3_TY_CURSE;
			break;
	}
}

static void random_slay(object_type *o_ptr)
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
				break;
			default:
				o_ptr->flags3 |= TR3_XTRA_SHOTS;
				break;
		}
	}

	switch (randint1(36))
	{
		case 1:
		case 2:
			o_ptr->flags1 |= TR1_SLAY_ANIMAL;
			break;
		case 3:
		case 4:
			o_ptr->flags1 |= TR1_SLAY_EVIL;
			break;
		case 5:
		case 6:
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;
			break;
		case 7:
		case 8:
			o_ptr->flags1 |= TR1_SLAY_DEMON;
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
			break;
		case 23:
		case 24:
			o_ptr->flags1 |= TR1_BRAND_COLD;
			break;
		case 25:
		case 26:
			o_ptr->flags1 |= TR1_BRAND_ELEC;
			break;
		case 27:
		case 28:
			o_ptr->flags1 |= TR1_BRAND_ACID;
			break;
		case 29:
		case 30:
			o_ptr->flags1 |= TR1_BRAND_POIS;
			break;
		case 31:
		case 32:
			o_ptr->flags1 |= TR1_VAMPIRIC;
			break;
		case 33:
		case 34:
			o_ptr->flags4 |= TR4_PSI_CRIT;
			break;
		default:
			o_ptr->flags1 |= TR1_CHAOTIC;
			break;
	}
}


static void give_activation_power(object_type *o_ptr)
{
	int type = 0, chance = 0;

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


static void random_minor_theme_weapon(object_type *o_ptr)
{
	switch (randint1(39))
	{
		case 1:
		case 2:
		case 3:
			o_ptr->flags1 |= TR1_WIS;
			o_ptr->flags3 |= TR3_BLESSED;

			break;

		case 4:
		case 5:
			o_ptr->flags1 |= TR1_BRAND_ACID;
			o_ptr->flags2 |= TR2_RES_ACID;
			if (o_ptr->tval == TV_SWORD && one_in_(3))
				o_ptr->flags1 |= TR1_TUNNEL;

			if (!o_ptr->activate)
			{
				if (one_in_(2))
					o_ptr->activate = ACT_BO_ACID_1;
			}
			
			break;

		case 6:
		case 7:
			o_ptr->flags1 |= TR1_BRAND_ELEC;
			o_ptr->flags2 |= TR2_RES_ELEC;

			if (!o_ptr->activate)
			{
				if (one_in_(4))
					o_ptr->activate = ACT_BO_ELEC_1;
				else if (one_in_(5))
					o_ptr->activate = ACT_BA_ELEC_2;
				else if (one_in_(5))
					o_ptr->activate = ACT_BA_ELEC_3;
			}

			break;

		case 8:
		case 9:
		case 10:
			o_ptr->flags1 |= TR1_BRAND_FIRE;
			o_ptr->flags2 |= TR2_RES_FIRE;
			o_ptr->flags3 |= TR3_LITE;

			if (!o_ptr->activate)
			{
				if (one_in_(5))
					o_ptr->activate = ACT_BO_FIRE_1;
				else if (one_in_(5))
					o_ptr->activate = ACT_BA_FIRE_1;
				else if (one_in_(5))
					o_ptr->activate = ACT_BA_FIRE_2;
			}

			break;

		case 11:
		case 12:
		case 13:
			o_ptr->flags1 |= TR1_BRAND_COLD;
			o_ptr->flags2 |= TR2_RES_COLD;

			if (!o_ptr->activate)
			{
				if (one_in_(6))
					o_ptr->activate = ACT_BO_COLD_1;
				else if (one_in_(6))
					o_ptr->activate = ACT_BA_COLD_1;
				else if (one_in_(6))
					o_ptr->activate = ACT_BA_COLD_2;
				else if (one_in_(7))
					o_ptr->flags3 |= TR3_ACTIVATE;
			}

			break;

		case 14:
		case 15:	
			o_ptr->flags1 |= TR1_BRAND_POIS;
			o_ptr->flags2 |= TR2_RES_POIS;

			if (!o_ptr->activate)
			{
				if (one_in_(3))
					o_ptr->activate = ACT_BA_POIS_1;
			}

			break;

		case 16:
		case 17:
			o_ptr->flags1 |= TR1_CHAOTIC;
			o_ptr->flags2 |= TR2_RES_CHAOS;
			if (one_in_(3))
				o_ptr->flags4 |= TR4_PATRON;

			if (!o_ptr->activate)
			{
				if (one_in_(11))
					o_ptr->activate = ACT_CALL_CHAOS;
			}
			
			break;

		case 18:
			if (o_ptr->tval == TV_SWORD)
			{
				o_ptr->flags1 |= TR1_VORPAL;
				o_ptr->flags1 |= TR1_TUNNEL;
			}
			else
			{
				o_ptr->flags1 |= TR1_BLOWS;
			}

			break;

		case 19:
		case 20:
			o_ptr->flags1 |= TR1_SLAY_ANIMAL;
			if (one_in_(2))
				o_ptr->flags1 |= TR1_INT;
			if (one_in_(2))
				o_ptr->flags3 |= TR3_REGEN;

			break;

		case 21:
		case 22:
		case 23:
			o_ptr->flags1 |= TR1_SLAY_EVIL;
			o_ptr->flags3 |= TR3_BLESSED;
			if (one_in_(2))
				o_ptr->flags1 |= TR1_WIS;
			if (one_in_(2))
				o_ptr->flags2 |= TR2_RES_FEAR;

			break;

		case 24:
		case 25:
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;
			if (one_in_(2))
				o_ptr->flags1 |= TR1_INT;
			if (one_in_(2))
				o_ptr->flags2 |= TR2_HOLD_LIFE;
			if (one_in_(2))
				o_ptr->flags3 |= TR3_SEE_INVIS;

			break;

		case 26:
		case 27:
			o_ptr->flags1 |= TR1_SLAY_DEMON;
			o_ptr->flags1 |= TR1_INT;
			
			break;

		case 28:
		case 29:
			o_ptr->flags1 |= TR1_SLAY_ORC;
			o_ptr->flags1 |= TR1_DEX;

			break;

		case 30:
		case 31:
			o_ptr->flags1 |= TR1_SLAY_GIANT;
			o_ptr->flags1 |= TR1_STR;

			break;

		case 32:
		case 33:
			o_ptr->flags1 |= TR1_SLAY_DRAGON;
			if (one_in_(3))
				o_ptr->flags1 |= TR1_KILL_DRAGON;
			o_ptr->flags1 |= TR1_CON;

			break;

		case 34:
		case 35:
			o_ptr->flags1 |= TR1_VAMPIRIC;
			o_ptr->flags2 |= TR2_HOLD_LIFE;

			if (!o_ptr->activate)
			{
				if (one_in_(6))
					o_ptr->activate = ACT_VAMPIRE_1;
				else if (one_in_(9))
					o_ptr->activate = ACT_VAMPIRE_2;
			}

			break;

		case 36:
			o_ptr->flags2 |= TR2_HOLD_LIFE;

			if (!o_ptr->activate)
			{
				if (!one_in_(3))
					o_ptr->activate = ACT_DRAIN_1;
				else
					o_ptr->activate = ACT_DRAIN_2;
			}

			break;

		case 37:
			o_ptr->to_h += rand_range(5, 15);
			o_ptr->to_d += rand_range(5, 15);

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_WHIRLWIND;
			}

			break;

		case 38:
			o_ptr->flags1 |= TR1_SLAY_ANIMAL;

			if (!o_ptr->activate)
			{
				if (!one_in_(3))
					o_ptr->activate = ACT_CHARM_ANIMAL;
				else
					o_ptr->activate = ACT_CHARM_ANIMALS;
			}

			break;

		case 39:
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_CHARM_UNDEAD;
			}

			break;

		case 40:
			if (o_ptr->tval == TV_SWORD)
				o_ptr->flags1 |= TR1_TUNNEL;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_STONE_MUD;
			}

			break;
	}

	if (o_ptr->activate)
		o_ptr->flags3 |= TR3_ACTIVATE;
}

static void random_major_theme_weapon(object_type *o_ptr)
{
	switch (randint1(7))
	{
	case 1:
		/* Holy Avenger */
		o_ptr->flags1 |= TR1_SLAY_EVIL;
		o_ptr->flags1 |= TR1_SLAY_UNDEAD;
		o_ptr->flags1 |= TR1_SLAY_DEMON;
		o_ptr->flags3 |= TR3_SEE_INVIS;
		o_ptr->flags3 |= TR3_BLESSED;

		if (!o_ptr->activate)
		{
			if (one_in_(4))
				o_ptr->activate = ACT_DISP_EVIL;
			else if (one_in_(7))
				o_ptr->activate = ACT_CURE_700;
		}
		
		break;

	case 2:
		/* Defender */
		o_ptr->flags2 |= TR2_RES_ACID;
		o_ptr->flags2 |= TR2_RES_ELEC;
		o_ptr->flags2 |= TR2_RES_FIRE;
		o_ptr->flags2 |= TR2_RES_COLD;
		if (one_in_(2))
			o_ptr->flags2 |= TR2_FREE_ACT;
		if (one_in_(2))
			o_ptr->flags3 |= TR3_SEE_INVIS;
		if (one_in_(2))
			o_ptr->flags3 |= TR3_FEATHER;
		if (one_in_(2))
			o_ptr->flags3 |= TR3_REGEN;
		if (one_in_(2))
			o_ptr->to_a += randint1(5);

		if (!o_ptr->activate)
		{
			if (one_in_(8))
				o_ptr->activate = ACT_RESIST_ALL;
		}

		break;

	case 3:
		/* Westernesse */
		o_ptr->flags1 |= TR1_STR;
		o_ptr->flags1 |= TR1_DEX;
		o_ptr->flags1 |= TR1_CON;
		o_ptr->flags1 |= TR1_SLAY_ORC;
		o_ptr->flags1 |= TR1_SLAY_TROLL;
		o_ptr->flags1 |= TR1_SLAY_GIANT;

		break;

	case 4:
		/* Trump Weapon */
		o_ptr->flags1 |= TR1_SLAY_EVIL;
		o_ptr->flags3 |= TR3_TELEPORT;
		o_ptr->flags2 |= TR2_FREE_ACT;
		if (one_in_(2))
			o_ptr->flags1 |= TR1_SEARCH;
		if (one_in_(2))
			o_ptr->flags3 |= TR3_REGEN;
		if (one_in_(2))
			o_ptr->flags3 |= TR3_SLOW_DIGEST;

		if (!o_ptr->activate)
		{
			if (one_in_(3))
				o_ptr->activate = ACT_TELEPORT_2;
			else if (one_in_(3))
				o_ptr->activate = ACT_TELEPORT_1;
		}

		break;

	case 5:
		/* Pattern Weapon */
		o_ptr->flags1 |= TR1_STR;
		o_ptr->flags1 |= TR1_CON;
		o_ptr->flags2 |= TR2_FREE_ACT;
		o_ptr->flags3 |= TR3_SEE_INVIS;
		if (one_in_(2))
			o_ptr->flags1 |= TR1_SLAY_EVIL;
		if (one_in_(2))
			o_ptr->flags1 |= TR1_SLAY_DEMON;
		if (one_in_(2))
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;

		break;

	case 6:
		/* Mixed slays */
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_ANIMAL;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_EVIL;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_DEMON;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_ORC;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_TROLL;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_GIANT;
		if (one_in_(3))
			o_ptr->flags1 |= TR1_SLAY_DRAGON;

		break;

	case 7:
		/* Assassin blade */
		o_ptr->flags1 |= TR1_STEALTH;
		o_ptr->flags1 |= TR1_BLOWS;
		o_ptr->flags2 |= TR2_FREE_ACT;
		
		if (one_in_(2))
			o_ptr->flags1 |= TR1_BRAND_POIS;
		else
			o_ptr->flags1 |= TR1_VAMPIRIC;
		
		if (o_ptr->tval == TV_SWORD)
			o_ptr->flags2 |= TR2_THROW;

		break;
	}

	if (o_ptr->activate)
		o_ptr->flags3 |= TR3_ACTIVATE;
}

static void random_minor_theme_armor(object_type *o_ptr)
{
	switch (randint1(33))
	{
		case 1:
		case 2:
		case 3:
			o_ptr->flags3 |= TR3_SEE_INVIS;
			o_ptr->flags1 |= TR1_SEARCH;

			break;

		case 4:
		case 5:
			o_ptr->flags1 |= TR1_STR;
			o_ptr->flags2 |= TR2_SUST_STR;
			if (one_in_(3))
				o_ptr->flags2 |= TR2_RES_FEAR;

			if (!o_ptr->activate)
			{
				if (one_in_(8))
					o_ptr->activate = ACT_BERSERK;
			}
			
			break;

		case 6:
		case 7:
			o_ptr->flags1 |= TR1_INT;
			o_ptr->flags2 |= TR2_SUST_INT;
			if (one_in_(3))
				o_ptr->flags3 |= TR3_FEATHER;
			
			if (!o_ptr->activate)
			{
				if (one_in_(8))
					o_ptr->activate = ACT_ID_PLAIN;
			}
			
			break;

		case 8:
		case 9:
			o_ptr->flags1 |= TR1_WIS;
			o_ptr->flags2 |= TR2_SUST_WIS;
			if (one_in_(3))
				o_ptr->flags3 |= TR3_SEE_INVIS;

			if (!o_ptr->activate)
			{
				if (one_in_(8))
					o_ptr->activate = ACT_DETECT_ALL;
			}
			
			break;

		case 10:
		case 11:
			o_ptr->flags1 |= TR1_DEX;
			o_ptr->flags2 |= TR2_SUST_DEX;
			if (one_in_(3))
				o_ptr->flags2 |= TR2_FREE_ACT;

			if (!o_ptr->activate)
			{
				if (one_in_(8))
					o_ptr->activate = ACT_SPEED;
			}
			
			break;

		case 12:
		case 13:
			o_ptr->flags1 |= TR1_CON;
			o_ptr->flags2 |= TR2_SUST_CON;
			if (one_in_(3))
				o_ptr->flags3 |= TR3_REGEN;

			if (!o_ptr->activate)
			{
				if (one_in_(8))
					o_ptr->activate = ACT_SATIATE;
			}
			
			break;

		case 14:
		case 15:
			o_ptr->flags1 |= TR1_CHR;
			o_ptr->flags2 |= TR2_SUST_CHR;
			if (one_in_(3))
				o_ptr->flags3 |= TR3_LITE;

			if (!o_ptr->activate)
			{
				if (one_in_(8))
					o_ptr->activate = ACT_RECALL;
			}
			
			break;

		case 16:
			o_ptr->flags3 |= TR3_LITE;
			o_ptr->flags2 |= TR2_RES_LITE;

			if (!o_ptr->activate)
			{
				if (one_in_(3))
					o_ptr->activate = ACT_LIGHT;
			}

			break;

		case 17:
		case 18:
			o_ptr->flags2 |= TR2_RES_FIRE;
			o_ptr->flags3 |= TR3_SH_FIRE;

			break;

		case 19:
			o_ptr->flags2 |= TR2_RES_ELEC;
			o_ptr->flags3 |= TR3_SH_ELEC;

			break;

		case 20:
			o_ptr->flags2 |= TR2_RES_COLD;
			o_ptr->flags4 |= TR4_SH_COLD;

			break;

		case 21:
			o_ptr->flags1 |= TR1_INT;
			o_ptr->flags1 |= TR1_WIS;

			break;

		case 22:
			o_ptr->flags2 |= TR2_RES_LITE;
			o_ptr->flags2 |= TR2_RES_DARK;
			if (one_in_(2))
				o_ptr->flags3 |= TR3_LITE;

			break;

		case 23:
			o_ptr->flags1 |= TR1_SLAY_EVIL;

			if (!o_ptr->activate)
			{
				if (one_in_(3))
					o_ptr->activate = ACT_DISP_EVIL;
				else if (one_in_(2))
					o_ptr->activate = ACT_BANISH_EVIL;
				else
					o_ptr->activate = ACT_PROT_EVIL;
			}

			break;

		case 24:
			o_ptr->flags1 |= TR1_STEALTH;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_SLEEP;
			}

			break;

		case 25:
			o_ptr->flags1 |= TR1_WIS;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_ESP;
			}

			break;

		case 26:
			o_ptr->flags2 |= TR2_RES_DARK;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_SUNLIGHT;
			}

			break;

		case 27:
			o_ptr->flags2 |= TR2_RES_CHAOS;
			o_ptr->flags2 |= TR2_RES_CONF;

			break;

		case 28:
			o_ptr->flags2 |= TR2_RES_NETHER;
			o_ptr->flags2 |= TR2_HOLD_LIFE;

			if (!o_ptr->activate)
			{
				if (one_in_(6))
					o_ptr->activate = ACT_REST_LIFE;
			}

			break;

		case 29:
			o_ptr->flags2 |= TR2_RES_SOUND;
			o_ptr->flags2 |= TR2_RES_SHARDS;

			break;

		case 30:
			o_ptr->flags2 |= TR2_RES_FEAR;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_TERROR;
			}

			break;

		case 31:
			o_ptr->flags1 |= TR1_SLAY_ANIMAL;

			if (!o_ptr->activate)
			{
				if (one_in_(3))
					o_ptr->activate = ACT_CHARM_ANIMAL;
				else if (one_in_(2))
					o_ptr->activate = ACT_CHARM_ANIMALS;
				else
					o_ptr->activate = ACT_SUMMON_ANIMAL;
			}

			break;

		case 32:
			o_ptr->flags1 |= TR1_SLAY_UNDEAD;

			if (!o_ptr->activate)
			{
				if (one_in_(2))
					o_ptr->activate = ACT_CHARM_UNDEAD;
				else
					o_ptr->activate = ACT_SUMMON_UNDEAD;
			}

			break;

		case 33:
			o_ptr->flags1 |= TR1_SLAY_DEMON;

			if (!o_ptr->activate)
			{
				o_ptr->activate = ACT_SUMMON_DEMON;
			}

			break;
	}

	if (o_ptr->activate)
		o_ptr->flags3 |= TR3_ACTIVATE;
}

static void random_major_theme_armor(object_type *o_ptr)
{
	int i;

	switch (randint1(10))
	{
		case 1:
			o_ptr->flags2 |= TR2_RES_ACID;
			o_ptr->flags2 |= TR2_RES_ELEC;
			o_ptr->flags2 |= TR2_RES_FIRE;
			o_ptr->flags2 |= TR2_RES_COLD;

			if (one_in_(3))
				o_ptr->flags2 |= TR2_RES_POIS;

			break;

		case 2:
			o_ptr->flags2 |= TR2_SUST_STR;
			o_ptr->flags2 |= TR2_SUST_INT;
			o_ptr->flags2 |= TR2_SUST_WIS;
			o_ptr->flags2 |= TR2_SUST_DEX;
			o_ptr->flags2 |= TR2_SUST_CON;
			o_ptr->flags2 |= TR2_SUST_CHR;

			break;

		case 3:
			/* Might */
			o_ptr->flags1 |= TR1_STR;
			o_ptr->flags2 |= TR2_SUST_STR;
			o_ptr->flags1 |= TR1_DEX;
			o_ptr->flags2 |= TR2_SUST_DEX;
			o_ptr->flags1 |= TR1_CON;
			o_ptr->flags2 |= TR2_SUST_CON;

			break;

		case 4:
			/* Mental */
			o_ptr->flags1 |= TR1_INT;
			o_ptr->flags2 |= TR2_SUST_INT;
			o_ptr->flags1 |= TR1_WIS;
			o_ptr->flags2 |= TR2_SUST_WIS;
			
			break;

		case 5:
			/* Lohengrin */
			o_ptr->flags1 |= TR1_STEALTH;
			o_ptr->flags1 |= TR1_INT;
			o_ptr->flags1 |= TR1_WIS;
			o_ptr->flags3 |= TR3_SEE_INVIS;

			break;

		case 6:
		case 7:
		case 8:
			/* Several high resists */
			for (i = randint1(3) + 1; i > 0; --i)
				random_resistance(o_ptr, rand_range(17, 38));

			break;

		case 9:
			/* Mixed stat boosts */
			for (i = 3; i > 0; --i)
			{
				switch (randint1(6))
				{
					case 1: o_ptr->flags1 |= TR1_STR; break;
					case 2: o_ptr->flags1 |= TR1_INT; break;
					case 3: o_ptr->flags1 |= TR1_WIS; break;
					case 4: o_ptr->flags1 |= TR1_DEX; break;
					case 5: o_ptr->flags1 |= TR1_CON; break;
					case 6: o_ptr->flags1 |= TR1_CHR; break;
				}
			}

			break;

		case 10:
			/* Thranduil */
			o_ptr->flags1 |= TR1_INT;
			o_ptr->flags1 |= TR1_WIS;
			o_ptr->flags2 |= TR2_RES_BLIND;
			if (o_ptr->tval == TV_HELM || o_ptr->tval == TV_CROWN)
				o_ptr->flags3 |= TR3_TELEPATHY;

			break;
	}

	if (o_ptr->activate)
		o_ptr->flags3 |= TR3_ACTIVATE;
}

static void curse_artifact(object_type *o_ptr)
{
	int i;
	
	if (o_ptr->pval) o_ptr->pval = 0 - (o_ptr->pval + randint1(4));
	if (o_ptr->to_a) o_ptr->to_a = 0 - (o_ptr->to_a + randint1(4));
	if (o_ptr->to_h) o_ptr->to_h = 0 - (o_ptr->to_h + randint1(4));
	if (o_ptr->to_d) o_ptr->to_d = 0 - (o_ptr->to_d + randint1(4));

	o_ptr->flags3 |= (TR3_HEAVY_CURSE | TR3_CURSED);

	for (i = rand_range(2, 5); i > 0; --i)
		random_curse(o_ptr, TRUE);

	if ((p_ptr->rp.pclass != CLASS_WARRIOR) && one_in_(3))
		o_ptr->flags3 |= TR3_NO_MAGIC;
}


bool create_artifact(object_type *o_ptr, int level, bool a_scroll)
{
	char new_name[1024];
	int powers = rand_range(2, 6);
	int power_level;
	s32b total_flags, target_flags;
	bool a_cursed = FALSE;
	int i;
	int given = 0;

	/* No activation yet */
	o_ptr->activate = 0;

	new_name[0] = 0;

	if (!a_scroll && one_in_(A_CURSED))
		a_cursed = TRUE;

	while (one_in_(powers + 1))
		powers++;

#if 0
	if (!a_cursed && one_in_(WEIRD_LUCK))
		powers *= 2;
#endif

	if (a_cursed) powers /= 2;

	target_flags = 0;
	for (i = 0; i < powers; i++)
		target_flags += rand_range(10, 50) * (level + 5);

	/* Sometimes select a major theme - or two */
	while (o_ptr->tval < TV_LITE && randint1(powers) > 3)
	{
		if (o_ptr->tval < TV_BOOTS)
		{
			random_major_theme_weapon(o_ptr);
			o_ptr->to_h += rand_range(5, 15);
			o_ptr->to_d += rand_range(5, 15);
		}
		else
		{
			random_major_theme_armor(o_ptr);
			o_ptr->to_a += rand_range(5, 15);
		}
		powers -= 3;
	}

	if (one_in_(3))
	{
		switch (o_ptr->tval)
		{
			case TV_BOOTS:
				if (one_in_(WEIRD_LUCK))
					o_ptr->flags1 |= TR1_SPEED;
				else if (one_in_(2))
					o_ptr->flags2 |= TR2_FREE_ACT;
				else
					o_ptr->flags3 |= TR3_FEATHER;
				powers--;
				given++;
				break;

			case TV_GLOVES:
				if (one_in_(2))
					o_ptr->flags2 |= TR2_FREE_ACT;
				else
					o_ptr->flags1 |= TR1_DEX;
				powers--;
				given++;
				break;

			case TV_HELM:
			case TV_CROWN:
				if (one_in_(3))
					o_ptr->flags3 |= TR3_TELEPATHY;
				else if (one_in_(2))
					o_ptr->flags3 |= TR3_SEE_INVIS;
				else
					o_ptr->flags1 |= TR1_INFRA;
				powers--;
				given++;
				break;
		}
	}

	/* Lights already have permanent light */
	if (o_ptr->tval == TV_LITE)
		given++;

	total_flags = flag_cost(o_ptr, 1);

	/* Main loop */
	while (total_flags < target_flags || given < 2)
	{
		switch (randint1(o_ptr->tval < TV_BOOTS ? 11 : 7))
		{
			case 1:  case 2:
				random_plus(o_ptr);
				break;
			case 3:  case 4:
				random_resistance(o_ptr, 0);
				break;
			case 5:
				random_misc(o_ptr);
				break;
			case 6:  case 7:
				random_minor_theme_armor(o_ptr);
				break;
			case 8:  case 9:
				random_slay(o_ptr);
				break;
			case 10:  case 11:
				random_minor_theme_weapon(o_ptr);
				break;
		}
		given++;
		
		total_flags = flag_cost(o_ptr, 1);
	}

	if (o_ptr->flags1 & (TR1_PVAL_MASK))
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
			i = randint1(100);

			if (i <= 35)
				o_ptr->pval = 1;
			else if (i <= 65)
				o_ptr->pval = 2;
			else if (i <= 85)
				o_ptr->pval = 3;
			else if (i <= 99)
				o_ptr->pval = 4;
			else
				o_ptr->pval = 5;
		}
	}
	else
		o_ptr->pval = 0;

	/* give it some plusses... */
	if (o_ptr->tval >= TV_BOOTS && o_ptr->tval < TV_LITE)
		o_ptr->to_a += randint1(o_ptr->to_a > 19 ? 1 : 20 - o_ptr->to_a);
	else if (o_ptr->tval < TV_BOOTS)
	{
		o_ptr->to_h += randint1(o_ptr->to_h > 19 ? 1 : 20 - o_ptr->to_h);
		o_ptr->to_d += randint1(o_ptr->to_d > 19 ? 1 : 20 - o_ptr->to_d);
	}

	/* Just to be sure */
	o_ptr->flags3 |= (TR3_IGNORE_ACID | TR3_IGNORE_ELEC |
					  TR3_IGNORE_FIRE | TR3_IGNORE_COLD);

	/* Possibly add some curses ... */
	total_flags = flag_cost(o_ptr, o_ptr->pval);
	if (one_in_(13))
	{
		random_curse(o_ptr, FALSE);
		total_flags = flag_cost(o_ptr, o_ptr->pval);
	}

	/* Penalize too-good artifacts */
	if (!a_scroll)
	{
		if (total_flags >= target_flags * 2 && total_flags >= 5000 &&
				one_in_(2))
		{
			random_curse(o_ptr, FALSE);
			total_flags = flag_cost(o_ptr, o_ptr->pval);
		}
		if (total_flags >= target_flags * 3 && total_flags >= 10000 &&
				!one_in_(WEIRD_LUCK))
		{
			random_curse(o_ptr, (one_in_(3) ? TRUE : FALSE));
			total_flags = flag_cost(o_ptr, o_ptr->pval);
		}
	}

	if (cheat_peek) msgf("%ld", total_flags);

	if (a_cursed) curse_artifact(o_ptr);

	if (!a_cursed && !o_ptr->activate && one_in_((o_ptr->tval >= TV_BOOTS)
							 ? ACTIVATION_CHANCE * 2 : ACTIVATION_CHANCE))
	{
		o_ptr->activate = 0;
		give_activation_power(o_ptr);
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
		dummy_name[0] = 0;
		(void)identify_fully_aux(o_ptr);
		o_ptr->info |= OB_STOREB;

		if (!(get_string(dummy_name, 80,
        				 "What do you want to call the artifact? ")))
		{
			get_random_name(new_name, o_ptr->tval, power_level);
		}
		else
		{
			strnfmt(new_name, 1024, "'%s'", dummy_name);
		}
		/* Identify it fully */
		object_aware(o_ptr);
		object_known(o_ptr);
		object_mental(o_ptr);

		/* Save all the known flags */
		o_ptr->kn_flags1 = o_ptr->flags1;
		o_ptr->kn_flags2 = o_ptr->flags2;
		o_ptr->kn_flags3 = o_ptr->flags3;
		o_ptr->kn_flags4 = o_ptr->flags4;
	}
	else
	{
		get_random_name(new_name, o_ptr->tval, power_level);
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
				msgf("The phial wells with clear light...");
				(void)lite_area(damroll(2, 15), 3);
				o_ptr->timeout = (s16b)rand_range(10, 20);
				break;
			}

			case ART_ELENDIL:
			{
				msgf("The star shines brightly...");
				map_area();
				(void)lite_area(damroll(2, 15), 3);
				o_ptr->timeout = (s16b)rand_range(50, 100);
				break;
			}

			case ART_THRAIN:
			{
				msgf("The Jewel flashes bright red!");
				wiz_lite();
				msgf("The Jewel drains your vitality...");
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
				msgf("The amulet lets out a shrill wail...");
				k = 3 * p_ptr->lev;
				(void)inc_protevil(randint1(25) + k);
				o_ptr->timeout = (s16b)rand_range(225, 450);
				break;
			}

			case ART_INGWE:
			{
				msgf("The amulet floods the area with goodness...");
				(void)dispel_evil(p_ptr->lev * 5);
				o_ptr->timeout = (s16b)rand_range(300, 600);
				break;
			}

			case ART_BARAHIR:
			{
				msgf("You order Frakir to strangle your opponent.");
				if (!get_aim_dir(&dir)) return FALSE;
				if (drain_life(dir, 200))
					o_ptr->timeout = (s16b)rand_range(100, 200);
				break;
			}

			case ART_TULKAS:
			{
				msgf("The ring glows brightly...");
				(void)inc_fast(rand_range(75, 150));
				o_ptr->timeout = (s16b)rand_range(150, 300);
				break;
			}

			case ART_NARYA:
			{
				msgf("The ring glows deep red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 250, 3);
				o_ptr->timeout = (s16b)rand_range(225, 450);
				break;
			}

			case ART_NENYA:
			{
				msgf("The ring glows bright white...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 400, 3);
				o_ptr->timeout = (s16b)rand_range(325, 650);
				break;
			}

			case ART_VILYA:
			{
				msgf("The ring glows deep blue...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_ELEC, dir, 500, 3);
				o_ptr->timeout = (s16b)rand_range(425, 850);
				break;
			}

			case ART_POWER:
			{
				msgf("The ring glows intensely black...");
				if (!get_aim_dir(&dir)) return FALSE;
				ring_of_power(dir);
				o_ptr->timeout = (s16b)rand_range(450, 900);
				break;
			}

			case ART_ELEMENTS:
			{
				msgf("The ring glows in multiple colours...");
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

				msgf("Your armor is surrounded by lightning...");

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
				msgf("You breathe the elements.");
				(void)fire_ball(GF_MISSILE, dir, 1000, 4);
				msgf("Your armor glows many colours...");
				(void)clear_afraid();
				(void)inc_shero(rand_range(50, 100));
				(void)hp_player(30);
				(void)inc_blessed(rand_range(50, 100));
				(void)inc_oppose_acid(rand_range(50, 100));
				(void)inc_oppose_elec(rand_range(50, 100));
				(void)inc_oppose_fire(rand_range(50, 100));
				(void)inc_oppose_cold(rand_range(50, 100));
				(void)inc_oppose_pois(rand_range(50, 100));
				o_ptr->timeout = 100;
				break;
			}

			case ART_SOULKEEPER:
			{
				msgf("Your armor glows a bright white...");
				msgf("You feel much better...");
				(void)hp_player(1000);
				(void)clear_cut();
				o_ptr->timeout = 888;
				break;
			}

			case ART_BELEGENNON:
			{
				msgf("A heavenly choir sings...");
				(void)clear_poisoned();
				(void)clear_cut();
				(void)clear_stun();
				(void)clear_confused();
				(void)clear_blind();
				(void)inc_hero(rand_range(25, 50));
				(void)hp_player(777);
				o_ptr->timeout = 300;
				break;
			}

			case ART_CELEBORN:
			{
				msgf("Your armor glows deep blue...");
				(void)genocide(TRUE);
				o_ptr->timeout = 500;
				break;
			}

			case ART_CASPANION:
			{
				msgf("Your armor glows bright red...");
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
				msgf("Your helm glows bright white...");
				msgf("An image forms in your mind...");
				(void)detect_all();
				o_ptr->timeout = (s16b)rand_range(55, 110);
				break;
			}

			case ART_GONDOR:
			{
				msgf("Your crown glows deep blue...");
				msgf("You feel a warm tingling inside...");
				(void)hp_player(700);
				(void)clear_cut();
				o_ptr->timeout = 250;
				break;
			}

			case ART_KERI:
			{
				object_type *q_ptr;

				msgf("Your rag feels warm for a moment...");

				/* Hack - Create the food ration */
				q_ptr = object_prep(lookup_kind(TV_FOOD, SV_FOOD_RATION));

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, p_ptr->px, p_ptr->py);

				o_ptr->timeout = 100;

				break;
			}

			case ART_COLLUIN:
			{
				msgf("Your cloak glows many colours...");
				(void)inc_oppose_acid(rand_range(20, 40));
				(void)inc_oppose_elec(rand_range(20, 40));
				(void)inc_oppose_fire(rand_range(20, 40));
				(void)inc_oppose_cold(rand_range(20, 40));
				(void)inc_oppose_pois(rand_range(20, 40));
				o_ptr->timeout = 111;
				break;
			}

			case ART_HOLCOLLETH:
			{
				msgf("Your cloak glows deep blue...");
				(void)sleep_monsters_touch();
				o_ptr->timeout = 55;
				break;
			}

			case ART_THINGOL:
			{
				msgf("Your cloak glows bright yellow...");
				(void)recharge(130);
				o_ptr->timeout = 70;
				break;
			}

			case ART_COLANNON:
			{
				msgf("Your cloak twists space around you...");
				teleport_player(100);
				o_ptr->timeout = 45;
				break;
			}

			case ART_LUTHIEN:
			{
				msgf("Your cloak glows a deep red...");
				(void)restore_level();
				o_ptr->timeout = 450;
				break;
			}

			case ART_CAMMITHRIM:
			{
				msgf("Your gloves glow extremely brightly...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
				o_ptr->timeout = 2;
				break;
			}

			case ART_PAURHACH:
			{
				msgf("Your gauntlets are covered in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
				o_ptr->timeout = (s16b)rand_range(8, 16);
				break;
			}

			case ART_CORWIN:
			{
				msgf("Your gauntlets are covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_COLD, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(7, 14);
				break;
			}

			case ART_PAURAEGEN:
			{
				msgf("Your gauntlets are covered in sparks...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = (s16b)rand_range(6, 12);
				break;
			}

			case ART_PAURNEN:
			{
				msgf("Your gauntlets are covered in acid...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(5, 10);
				break;
			}

			case ART_FINGOLFIN:
			{
				msgf("Your cesti grows magical spikes...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ARROW, dir, 250);
				o_ptr->timeout = (s16b)rand_range(90, 180);
				break;
			}

			case ART_FEANOR:
			{
				msgf("Your boots glow bright green...");
				(void)inc_fast(rand_range(20, 40));
				o_ptr->timeout = 200;
				break;
			}

			case ART_DAL:
			{
				msgf("Your boots glow deep blue...");
				(void)clear_afraid();
				(void)clear_poisoned();
				o_ptr->timeout = 5;
				break;
			}

			case ART_NARTHANC:
			{
				msgf("Your dagger is covered in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
				o_ptr->timeout = (s16b)rand_range(8, 16);
				break;
			}

			case ART_NIMTHANC:
			{
				msgf("Your dagger is covered in frost...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_COLD, dir, damroll(8, 8));
				o_ptr->timeout = (s16b)rand_range(7, 14);
				break;
			}

			case ART_DETHANC:
			{
				msgf("Your dagger is covered in sparks...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
				o_ptr->timeout = (s16b)rand_range(6, 12);
				break;
			}

			case ART_RILIA:
			{
				msgf("Your dagger throbs deep green...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_POIS, dir, 25, 3);
				o_ptr->timeout = (s16b)rand_range(4, 8);
				break;
			}

			case ART_BELANGIL:
			{
				msgf("Your dagger is covered in frost...");
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
							p_ptr->state.leaving = TRUE;
						}
				}
				o_ptr->timeout = 35;
				break;
			}

			case ART_RINGIL:
			{
				msgf("Your sword glows an intense blue...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_COLD, dir, 200, 2);
				o_ptr->timeout = 300;
				break;
			}

			case ART_DAWN:
			{
				msgf("You summon the Legion of the Dawn.");
				(void)summon_specific(-1, px, py, p_ptr->depth, SUMMON_DAWN,
									  TRUE, TRUE, TRUE);
				o_ptr->timeout = (s16b)rand_range(500, 1000);
				break;
			}

			case ART_ANDURIL:
			{
				msgf("Your sword glows an intense red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 150, 2);
				o_ptr->timeout = 400;
				break;
			}

			case ART_THEODEN:
			{
				msgf("Your axe blade glows black...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)drain_life(dir, 200);
				o_ptr->timeout = 400;
				break;
			}

			case ART_AEGLOS:
			{
				msgf("Your spear crackles with electricity...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_ELEC, dir, 200, 3);
				o_ptr->timeout = 500;
				break;
			}

			case ART_OROME:
			{
				msgf("Your spear pulsates...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)wall_to_mud(dir);
				o_ptr->timeout = 5;
				break;
			}

			case ART_EONWE:
			{
				msgf("Your axe lets out a long, shrill note...");
				(void)mass_genocide(TRUE);
				o_ptr->timeout = 1000;
				break;
			}

			case ART_LOTHARANG:
			{
				msgf("Your battle axe radiates deep purple...");
				(void)hp_player(100);
				(void)inc_cut(-50);
				o_ptr->timeout = (s16b)rand_range(3, 6);
				break;
			}

			case ART_ULMO:
			{
				msgf("Your trident glows deep red...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)teleport_monster(dir);
				o_ptr->timeout = 150;
				break;
			}

			case ART_AVAVIR:
			{
				msgf("Your scythe glows soft white...");

				word_of_recall();

				o_ptr->timeout = 200;
				break;
			}

			case ART_TOTILA:
			{
				msgf("Your flail glows in scintillating colours...");
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

				msgf("Your ball and chain swings through the air...");

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
				msgf("Your morning star rages in fire...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)fire_ball(GF_FIRE, dir, 200, 3);
				o_ptr->timeout = 100;
				break;
			}

			case ART_ENERGY:
			{
				msgf("Your scythe glows bright green...");
				(void)inc_fast(rand_range(20, 40));
				o_ptr->timeout = (s16b)rand_range(100, 200);
				break;
			}

			case ART_ERIRIL:
			{
				msgf("Your quarterstaff glows yellow...");
				if (!ident_spell()) return FALSE;
				o_ptr->timeout = 10;
				break;
			}

			case ART_OLORIN:
			{
				msgf("Your quarterstaff glows brightly...");
				(void)detect_all();
				(void)probing();
				(void)identify_fully();
				o_ptr->timeout = 1000;
				break;
			}

			case ART_TURMIL:
			{
				msgf("Your hammer glows white...");
				if (!get_aim_dir(&dir)) return FALSE;
				(void)drain_life(dir, 200);
				o_ptr->timeout = 70;
				break;
			}

			case ART_CATAPULT:
			{
				msgf("Your sling hums...");
				(void)clear_afraid();
				(void)hp_player(45);
				o_ptr->timeout = 10;
				break;
			}

			case ART_BRAND:
			{
				msgf("Your crossbow glows deep red...");
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
			msgf("A line of sunlight appears.");
			(void)lite_line(dir);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_BO_MISS_1:
		{
			msgf("The %s glows extremely brightly...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_MISSILE, dir, damroll(3, 6));
			o_ptr->timeout = 2;
			break;
		}

		case ACT_BA_POIS_1:
		{
			msgf("The %s throbs deep green...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_POIS, dir, 25, 3);
			o_ptr->timeout = (s16b)rand_range(4, 8);
			break;
		}

		case ACT_BO_ELEC_1:
		{
			msgf("The %s is covered in sparks...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ELEC, dir, damroll(6, 8));
			o_ptr->timeout = (s16b)rand_range(6, 12);
			break;
		}

		case ACT_BO_ACID_1:
		{
			msgf("The %s is covered in acid...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ACID, dir, damroll(8, 8));
			o_ptr->timeout = (s16b)rand_range(5, 10);
			break;
		}

		case ACT_BO_COLD_1:
		{
			msgf("The %s is covered in frost...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_COLD, dir, damroll(9, 8));
			o_ptr->timeout = (s16b)rand_range(7, 14);
			break;
		}

		case ACT_BO_FIRE_1:
		{
			msgf("The %s is covered in fire...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_FIRE, dir, damroll(11, 8));
			o_ptr->timeout = (s16b)rand_range(8, 16);
			break;
		}

		case ACT_BA_COLD_1:
		{
			msgf("The %s is covered in frost...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 100, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BA_FIRE_1:
		{
			msgf("The %s glows an intense red...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 150, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DRAIN_1:
		{
			msgf("The %s glows black...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			if (drain_life(dir, 200))
				o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_BA_COLD_2:
		{
			msgf("The %s glows an intense blue...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 200, 2);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_BA_ELEC_2:
		{
			msgf("The crackles with electricity...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_ELEC, dir, 200, 3);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DRAIN_2:
		{
			msgf("The %s glows black...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)drain_life(dir, 250);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_VAMPIRE_1:
		{
			if (!get_aim_dir(&dir)) return FALSE;

			msgf("The %s throbs red...", o_name);
			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 100);
			}
			o_ptr->timeout = 400;
			break;
		}

		case ACT_BO_MISS_2:
		{
			msgf("The %s grows magical spikes...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_bolt(GF_ARROW, dir, 250);
			o_ptr->timeout = (s16b)rand_range(90, 180);
			break;
		}

		case ACT_BA_FIRE_2:
		{
			msgf("The %s glows deep red...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_FIRE, dir, 250, 3);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_BA_COLD_3:
		{
			msgf("The %s glows bright white...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_COLD, dir, 400, 3);
			o_ptr->timeout = (s16b)rand_range(325, 650);
			break;
		}

		case ACT_BA_ELEC_3:
		{
			msgf("The %s glows deep blue...", o_name);
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

			msgf("The %s emits a blast of air...", o_name);

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

			msgf("The %s throbs red...", o_name);

			for (dummy = 0; dummy < 3; dummy++)
			{
				(void)drain_gain_life(dir, 200);
			}

			o_ptr->timeout = 400;
			break;
		}


		case ACT_CALL_CHAOS:
		{
			msgf("The %s glows in scintillating colours...", o_name);
			call_chaos();
			o_ptr->timeout = 350;
			break;
		}

		case ACT_ROCKET:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("You launch a rocket!");
			(void)fire_ball(GF_ROCKET, dir, 300 + plev, 2);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_DISP_EVIL:
		{
			msgf("The %s floods the area with goodness...", o_name);
			(void)dispel_evil(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_DISP_GOOD:
		{
			msgf("The %s floods the area with evil...", o_name);
			(void)dispel_good(p_ptr->lev * 5);
			o_ptr->timeout = (s16b)rand_range(300, 600);
			break;
		}

		case ACT_BA_MISS_3:
		{
			if (!get_aim_dir(&dir)) return FALSE;
			msgf("You breathe the elements.");
			(void)fire_ball(GF_MISSILE, dir, 600, 4);
			o_ptr->timeout = 500;
			break;
		}

			/* Activate for other offensive action */

		case ACT_CONFUSE:
		{
			msgf("The %s glows in scintillating colours...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)confuse_monster(dir, 50);
			o_ptr->timeout = 15;
			break;
		}

		case ACT_SLEEP:
		{
			msgf("The %s glows deep blue...", o_name);
			(void)sleep_monsters_touch();
			o_ptr->timeout = 55;
			break;
		}

		case ACT_QUAKE:
		{
			msgf("The %s vibrates...", o_name);

			(void)earthquake(px, py, 10);
			o_ptr->timeout = 50;
			break;
		}

		case ACT_TERROR:
		{
			msgf("The %s emits a loud blast...", o_name);

			(void)turn_monsters(40 + p_ptr->lev);
			o_ptr->timeout = 3 * (p_ptr->lev + 10);
			break;
		}

		case ACT_TELE_AWAY:
		{
			msgf("The %s glows violet...", o_name);

			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_beam(GF_AWAY_ALL, dir, plev);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BANISH_EVIL:
		{
			if (banish_evil(200))
			{
				msgf("The power of the artifact banishes evil!");
			}
			o_ptr->timeout = (s16b)rand_range(250, 500);
			break;
		}

		case ACT_GENOCIDE:
		{
			msgf("The %s glows deep blue...", o_name);
			(void)genocide(TRUE);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_MASS_GENO:
		{
			msgf("The %s lets out a long, shrill note...", o_name);
			(void)mass_genocide(TRUE);
			o_ptr->timeout = 1000;
			break;
		}

			/* Activate for summoning / charming */

		case ACT_CHARM_ANIMAL:
		{
			msgf("The %s twists in your hands...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_animal(dir, plev);
			o_ptr->timeout = 300;
			break;
		}

		case ACT_CHARM_UNDEAD:
		{
			msgf("The %s shudders...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)control_one_undead(dir, plev);
			o_ptr->timeout = 333;
			break;
		}

		case ACT_CHARM_OTHER:
		{
			msgf("The %s fades in and out...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)charm_monster(dir, plev);
			o_ptr->timeout = 400;
			break;
		}

		case ACT_CHARM_ANIMALS:
		{
			msgf("The %s hums softly...", o_name);
			(void)charm_animals(plev * 2);
			o_ptr->timeout = 500;
			break;
		}

		case ACT_CHARM_OTHERS:
		{
			msgf("The %s blinks in and out...", o_name);
			(void)charm_monsters(plev * 2);
			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_ANIMAL:
		{
			msgf("You summon a beast.");
			(void)summon_specific(-1, px, py, plev, SUMMON_ANIMAL_RANGER, TRUE,
								  TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 500);
			break;
		}

		case ACT_SUMMON_PHANTOM:
		{
			msgf("You summon a phantasmal servant.");
			(void)summon_specific(-1, px, py, p_ptr->depth, SUMMON_PHANTOM,
								  TRUE, TRUE, TRUE);
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_SUMMON_ELEMENTAL:
		{
			bool pet = !one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, ((plev * 3) / 2), SUMMON_ELEMENTAL,
				 group, FALSE, pet))
			{
				msgf("An elemental materializes...");

				if (pet)
					msgf("It seems obedient to you.");
				else
					msgf("You fail to control it!");
			}

			o_ptr->timeout = 750;
			break;
		}

		case ACT_SUMMON_DEMON:
		{
			bool pet = !one_in_(3);
			bool group = !(pet && (plev < 50));

			if (summon_specific
				((pet ? -1 : 0), px, py, ((plev * 3) / 2), SUMMON_DEMON, group,
				 FALSE, pet))
			{
				msgf
					("The area fills with a stench of sulphur and brimstone.");
				if (pet)
					msgf("'What is thy bidding... Master?'");
				else
					msgf
						("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}

		case ACT_SUMMON_UNDEAD:
		{
			bool pet = !one_in_(3);
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
				msgf
					("Cold winds begin to blow around you, carrying with them the stench of decay...");
				if (pet)
					msgf
						("Ancient, long-dead forms arise from the ground to serve you!");
				else
					msgf
						("'The dead arise... to punish you for disturbing them!'");
			}

			o_ptr->timeout = (s16b)rand_range(666, 1000);
			break;
		}

			/* Activate for healing */

		case ACT_CURE_LW:
		{
			msgf("The %s radiates light blue...", o_name);
			(void)clear_afraid();
			(void)hp_player(30);
			o_ptr->timeout = 10;
			break;
		}

		case ACT_CURE_MW:
		{
			msgf("The %s radiates deep purple...", o_name);
			(void)hp_player(75);
			(void)inc_cut(-50);
			o_ptr->timeout = (s16b)rand_range(3, 6);
			break;
		}

		case ACT_CURE_POISON:
		{
			msgf("The %s glows deep blue...", o_name);
			(void)clear_afraid();
			(void)clear_poisoned();
			o_ptr->timeout = 5;
			break;
		}

		case ACT_REST_LIFE:
		{
			msgf("The %s glows a deep red...", o_name);
			(void)restore_level();
			o_ptr->timeout = 450;
			break;
		}

		case ACT_REST_ALL:
		{
			msgf("The %s glows a deep green...", o_name);
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
			msgf("The %s glows deep blue...", o_name);
			msgf("You feel a warm tingling inside...");
			(void)hp_player(700);
			(void)clear_cut();
			o_ptr->timeout = 250;
			break;
		}

		case ACT_CURE_1000:
		{
			msgf("The %s glows a bright white...", o_name);
			msgf("You feel much better...");
			(void)hp_player(1000);
			(void)clear_cut();
			o_ptr->timeout = 888;
			break;
		}

			/* Activate for timed effect */

		case ACT_ESP:
		{
			msgf("The %s enters your thoughts...", o_name);
			(void)inc_tim_esp(rand_range(25, 55));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_BERSERK:
		{
			msgf("The %s angers you...", o_name);
			(void)inc_shero(rand_range(50, 100));
			(void)inc_blessed(rand_range(50, 100));
			o_ptr->timeout = (s16b)rand_range(100, 200);
			break;
		}

		case ACT_PROT_EVIL:
		{
			msgf("The %s lets out a shrill wail...", o_name);
			k = 3 * p_ptr->lev;
			(void)inc_protevil(randint1(25) + k);
			o_ptr->timeout = (s16b)rand_range(225, 450);
			break;
		}

		case ACT_RESIST_ALL:
		{
			msgf("The %s glows many colours...", o_name);
			(void)inc_oppose_acid(rand_range(40, 80));
			(void)inc_oppose_elec(rand_range(40, 80));
			(void)inc_oppose_fire(rand_range(40, 80));
			(void)inc_oppose_cold(rand_range(40, 80));
			(void)inc_oppose_pois(rand_range(40, 80));
			o_ptr->timeout = 200;
			break;
		}

		case ACT_SPEED:
		{
			msgf("The %s glows bright green...", o_name);
			(void)inc_fast(rand_range(20, 40));
			o_ptr->timeout = 250;
			break;
		}

		case ACT_XTRA_SPEED:
		{
			msgf("The %s glows brightly...", o_name);
			(void)inc_fast(rand_range(75, 150));
			o_ptr->timeout = (s16b)rand_range(200, 400);
			break;
		}

		case ACT_WRAITH:
		{
			msgf("The %s fades out...", o_name);
			(void)inc_wraith_form(rand_range(plev / 2, plev));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_INVULN:
		{
			msgf("The %s fires a beam of bright white light at you...",
					   o_name);
			(void)inc_invuln(rand_range(8, 16));
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_TELEPORT_1:
		{
			msgf("The %s twists space around you...", o_name);
			teleport_player(100);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

			/* Activate for general purpose effect (detection etc.) */

		case ACT_LIGHT:
		{
			msgf("The %s wells with clear light...", o_name);
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(10, 20);
			break;
		}

		case ACT_MAP_LIGHT:
		{
			msgf("The %s shines brightly...", o_name);
			map_area();
			(void)lite_area(damroll(2, 15), 3);
			o_ptr->timeout = (s16b)rand_range(50, 100);
			break;
		}

		case ACT_DETECT_ALL:
		{
			msgf("The %s glows bright white...", o_name);
			msgf("An image forms in your mind...");
			(void)detect_all();
			o_ptr->timeout = (s16b)rand_range(55, 110);
			break;
		}

		case ACT_DETECT_XTRA:
		{
			msgf("The %s glows brightly...", o_name);
			(void)detect_all();
			(void)probing();
			(void)identify_fully();
			o_ptr->timeout = 1000;
			break;
		}

		case ACT_ID_FULL:
		{
			msgf("The %s glows yellow...", o_name);
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
			msgf("The %s glows bright red...", o_name);
			(void)explosive_rune();
			o_ptr->timeout = 200;
			break;
		}

		case ACT_RUNE_PROT:
		{
			msgf("The %s glows light blue...", o_name);
			(void)warding_glyph();
			o_ptr->timeout = 400;
			break;
		}

		case ACT_SATIATE:
		{
			msgf("The %s glows brown...", o_name);
			(void)set_food(PY_FOOD_MAX - 1);
			o_ptr->timeout = 200;
			break;
		}

		case ACT_DEST_DOOR:
		{
			msgf("The %s glows bright red...", o_name);
			(void)destroy_doors_touch();
			o_ptr->timeout = 10;
			break;
		}

		case ACT_STONE_MUD:
		{
			msgf("The %s pulsates...", o_name);
			if (!get_aim_dir(&dir)) return FALSE;
			(void)wall_to_mud(dir);
			o_ptr->timeout = 5;
			break;
		}

		case ACT_RECHARGE:
		{
			msgf("The %s hums...", o_name);
			(void)recharge(130);
			o_ptr->timeout = 70;
			break;
		}

		case ACT_ALCHEMY:
		{
			msgf("The %s glows bright yellow...", o_name);
			(void)alchemy();
			o_ptr->timeout = 500;
			break;
		}

		case ACT_DIM_DOOR:
		{
			msgf("You open a dimensional gate. Choose a destination.");
			if (!dimension_door()) return FALSE;
			o_ptr->timeout = 100;
			break;
		}


		case ACT_TELEPORT_2:
		{
			msgf("The %s twists space around you...", o_name);
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
			msgf("Unknown activation effect: %d.", o_ptr->activate);
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
		if (p_ptr->rp.pclass == CLASS_WARRIOR)
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
		random_resistance(o_ptr, rand_range(17, 38));
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
	q_ptr->flags4 |= a_ptr->flags4;

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
