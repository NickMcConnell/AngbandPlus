/* File: actmagic.c */

/*
 * RandomBand activation magic system.
 *
 * Purpose : Add the magic casting code activation system so
 * that it is easier to build and use new spells when added to the
 * spells list.
 *
 * Copyright (c) 2003
 * Kenneth A. Strom
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#define BASE_TELEPORT_RANGE         5
#define BASE_RESTORE_MANA           5
#define BASE_CURE_LIGHT             3
#define BASE_BALL_POWER            10
#define BASE_LIGHT_POWER            3
#define BASE_BOLT_POWER             4
#define BASE_TEMP_BOOST             3
#define BASE_EXP_BOOST             10
#define BASE_SUMMON_POWER           2
#define BASE_FLOOD_POWER            3
#define BASE_REPAIR_POWER           3
#define BASE_DISPEL_POWER           4
#define BASE_INVUNERABLE_POWER      1
#define BASE_ENCHANT_VALUE          2


#define MAX_BIZARRE		6

static const int bizarre_num[MAX_BIZARRE] =
{
	SUMMON_BIZARRE1,
	SUMMON_BIZARRE2,
	SUMMON_BIZARRE3,
	SUMMON_BIZARRE4,
	SUMMON_BIZARRE5,
	SUMMON_BIZARRE6,
};


/*  Casting penalty  */
int act_penalty( int dice, int penalty)
{
   int   i;

   i = (dice * (100 - penalty)) / 100;

   return (i);
}

/*
 * Hope to rid this later.
 */

static void wild_magic(int spell)
{
	int px = p_ptr->px;
	int py = p_ptr->py;

	switch (randint0(spell) + randint0(9))
	{
		case 1:
		case 2:
		case 3:
		{
			teleport_player(10);
			break;
		}
		case 4:
		case 5:
		case 6:
		{
			teleport_player(100);
			break;
		}
		case 7:
		case 8:
		{
			teleport_player(200);
			break;
		}
		case 9:
		case 10:
		case 11:
		{
			(void)unlite_area(10, 3);
			break;
		}
		case 12:
		case 13:
		case 14:
		{
			(void)lite_area(damroll(2, 3), 2);
			break;
		}
		case 15:
		{
			(void)destroy_doors_touch();
			break;
		}
		case 16: case 17:
		{
			wall_breaker();
			break;
		}
		case 18:
		{
			(void)sleep_monsters_touch();
			break;
		}
		case 19:
		case 20:
		{
			(void)trap_creation();
			break;
		}
		case 21:
		case 22:
		{
			(void)door_creation();
			break;
		}
		case 23:
		case 24:
		case 25:
		{
			aggravate_monsters(0);
			break;
		}
		case 26:
		{
			(void)earthquake(py, px, 5);
			break;
		}
		case 27:
		case 28:
		{
			(void)gain_mutation(0);
			break;
		}
		case 29:
		case 30:
		{
			(void)apply_disenchant();
			break;
		}
		case 31:
		{
			(void)lose_all_info();
			break;
		}
		case 32:
		{
			(void)fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
			break;
		}
		case 33:
		{
			(void)wall_stone();
			break;
		}
		case 34:
		case 35:
		{
			int i;
			int type = bizarre_num[randint0(6)];

			for (i = 0; i < 8; i++)
			{
				(void)summon_specific(0, py, px, (p_ptr->depth * 3) / 2, type, TRUE, FALSE, FALSE);
			}
			break;
		}
		case 36:
		case 37:
		{
			(void)activate_hi_summon();
			break;
		}
		case 38:
		{
			(void)summon_cyber(-1, py, px);
			break;
		}
		default:
		{
			int count = 0;

			(void)activate_ty_curse(FALSE, &count);

			break;
		}
	}

	return;
}


/*
 * Actual magic and power call here
 *
 * bool may be removed
 */
bool act_magic(int px, int py, int act_num, int s_lev, int s_fail,
               int s_base, int s_bonus, int act_mult, int act_div,
               int cast_penalty, int beam_pct)
{
   bool success = FALSE;
   bool success2 = FALSE;
   int numdice = 0;
   int numsides = 0;
   int power = 0;
   int radius = 0;
   bool bad = FALSE;
   bool group = FALSE;
   int dir = 0;
   int chance = 0;
   int i = 0;
	int plev = p_ptr->lev;

   /*  Calculate base effect power  */
   if (s_lev)  /*  Has fixed value (rod/staff/wand/etc.)  */
      numdice = s_base + (s_lev / s_bonus);
      else
      numdice = s_base + s_bonus;

   /*  If penalized for multiple realms adjust for penalty  */
   if (cast_penalty)
      numdice = act_penalty( numdice, cast_penalty);

   if (numdice < 1) numdice = 1;

   /*  Check for which spell effect to use  */
   switch(act_num)
   {
      case ACT_TELEPORT:
      {
         numsides = (BASE_TELEPORT_RANGE * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         teleport_player(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case ACT_RESTORE_STR:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_STR, power);
         break;
      }

      case ACT_RESTORE_INT:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_INT, power);
         break;
      }

      case ACT_RESTORE_WIS:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_WIS, power);
         break;
      }

      case ACT_RESTORE_DEX:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_DEX, power);
         break;
      }

      case ACT_RESTORE_CON:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_CON, power);
         break;
      }

      case ACT_RESTORE_CHR:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_CHR, power);
         break;
      }

      case ACT_RESTORE_STATS:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         power = damroll( numdice, numsides);
         success = do_res_stat( A_STR, power);
         if (success) success2 = TRUE;
         success = do_res_stat( A_INT, power);
         if (success) success2 = TRUE;
         success = do_res_stat( A_WIS, power);
         if (success) success2 = TRUE;
         success = do_res_stat( A_DEX, power);
         if (success) success2 = TRUE;
         success = do_res_stat( A_CON, power);
         if (success) success2 = TRUE;
         success = do_res_stat( A_CHR, power);
         if (success) success2 = TRUE;
         /*  Proper return statement  */
         if (success2) success = TRUE;
         break;
      }

      case ACT_RESTORE_MANA:
      {
         numsides = (BASE_RESTORE_MANA * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         success = sp_player(damroll(numdice, numsides));
         break;
      }

      case ACT_CURE_LW:
      {
         numsides = (BASE_CURE_LIGHT * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
         if (set_blind(0)) success =  TRUE;

         numdice /= 7;
         numsides /= 7;

         if (numdice < 3) numdice = 3;
         if (numsides < 3) numsides = 3;

         if (set_cut(p_ptr->cut - damroll(numdice, numsides))) success = TRUE;
         break;
      }

      case ACT_BA_FIRE:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_FIRE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_COLD:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_COLD, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_ACID:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_ACID, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_ELEC:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_ELEC, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_ID_PLAIN:
      {
         success = ident_spell();
         break;
      }

      case ACT_ID_FULL:
      {
         success = identify_fully();
         break;
      }

      case ACT_CALL_CHAOS:
      {
         call_chaos();
         success = TRUE;
         break;
      }

      case ACT_REMOVE_CURSE:
      {
         success = remove_curse();
         break;
      }

      case ACT_LITE:
      {
         numsides = (BASE_LIGHT_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         success = lite_area( damroll( numdice, numsides), radius);

         break;
      }

      case ACT_SUMMON_ANIMAL:
      {
         if (one_in_(25)) bad = FALSE;
            else
            bad = TRUE;

         if (one_in_(25)) group = TRUE;

         success = summon_specific(0, p_ptr->py, p_ptr->px, p_ptr->lev + randint1(5),
                                   SUMMON_ANIMAL, group, bad, bad);
         break;
      }
      case ACT_SUMMON_PHANTOM:
      {
         if (one_in_(25)) bad = FALSE;
            else
            bad = TRUE;

         if (one_in_(25)) group = TRUE;

         success = summon_specific(-1, p_ptr->py, p_ptr->px, p_ptr->lev + randint1(5),
                                   SUMMON_PHANTOM, group, bad, bad);
         break;
      }
      case ACT_SUMMON_ELEMENTAL:
      {
         if (one_in_(25)) bad = FALSE;
            else
            bad = TRUE;

         if (one_in_(25)) group = TRUE;

         success = summon_specific(-1, p_ptr->py, p_ptr->px, p_ptr->lev + randint1(5),
                                   SUMMON_ELEMENTAL, group, bad, bad);
         break;
      }
      case ACT_SUMMON_DEMON:
      {
         if (one_in_(25)) bad = FALSE;
            else
            bad = TRUE;

         if (one_in_(25)) group = TRUE;

         success = summon_specific(-1, p_ptr->py, p_ptr->px, p_ptr->lev + randint1(5),
                                   SUMMON_DEMON, group, bad, bad);
         break;
      }
      case ACT_SUMMON_UNDEAD:
      {
         if (one_in_(25)) bad = FALSE;
            else
            bad = TRUE;

         if (one_in_(25)) group = TRUE;

         success = summon_specific(-1, p_ptr->py, p_ptr->px, p_ptr->lev + randint1(5),
                                   SUMMON_UNDEAD, group, bad, bad);
         break;
      }
      case ACT_PHASE_DOOR:
      {
         numsides = ((BASE_TELEPORT_RANGE / 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         teleport_player(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case ACT_TELEPORT_LEVEL:
      {
         (void)teleport_player_level();
         success = TRUE;
         break;
      }

      case ACT_BO_CONFUSE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_CONFUSION, dir, damroll(numdice, numsides));
         break;
      }
      case ACT_MAGIC_MAP:
      {
   		map_area();
         success = TRUE;
         break;
      }
      case ACT_STAR_REMOVE_CURSE:
      {
   		(void)remove_all_curse();
         success = TRUE;
         break;
      }
      case ACT_DETECT_TREASURE:
      {
   		(void)detect_treasure();
         success = TRUE;
         break;
      }
      case ACT_OBJECT_DETECT:
      {
   		(void)detect_objects_normal();
         success = TRUE;
         break;
      }
      case ACT_TRAP_DETECT:
      {
   		(void)detect_traps();
         success = TRUE;
         break;
      }
      case ACT_DOOR_STAIR_DETECT:
      {
   		(void)detect_doors();
	   	(void)detect_stairs();
         success = TRUE;
         break;
      }
      case ACT_ACQUIREMENT:
      {
			acquirement(py, px, 1, TRUE, FALSE);
         success = TRUE;
			break;
		}
		case ACT_STAR_ACQUIREMENT:
		{
			acquirement(py, px, rand_range(2, 3), TRUE, FALSE);
         success = TRUE;
			break;
      }
      case ACT_MASS_GENOCIDE:
      {
			(void)mass_genocide(TRUE);
			success = TRUE;
			break;
      }
      case ACT_TRAP_CREATION:
      {
			if (trap_creation()) success = TRUE;
			break;
      }
      case ACT_DEST_DOOR_TRAP:
      {
			if (destroy_doors_touch()) success = TRUE;
			break;
      }
      case ACT_CREATE_ARTIFACT:
      {
			if (artifact_scroll()) success = TRUE;
			break;
      }
      case ACT_RECHARGE:
      {
			if (recharge(130)) success = TRUE;
			break;
      }
      case ACT_GENOCIDE:
      {
			(void)genocide(TRUE);
			success = TRUE;
         break;
      }

      case ACT_BA_DARKNESS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_DARK, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_PROT_EVIL:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
			if (set_protevil(p_ptr->protevil + randint1(25) + damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_SATIATE:
      {
			if (set_food(PY_FOOD_MAX - 1)) success = TRUE;
			break;
      }

      case ACT_DISP_UNDEAD:
      {
			if (dispel_undead(60)) success = TRUE;
			break;
      }

      case ACT_STAR_ENCHANT_WEAPON:
      {
			if (enchant_spell(randint1(5), randint1(5), 0)) success = TRUE;
			break;
      }

      case ACT_CURSE_WEAPON:
      {
			if (curse_weapon()) success= TRUE;
			break;
      }

      case ACT_STAR_ENCHANT_ARMOR:
      {
			if (enchant_spell(0, 0, rand_range(2, 7))) success = TRUE;
			break;
      }

      case ACT_CURSE_ARMOR:
      {
			if (curse_armor()) success = TRUE;
			break;
      }

      case ACT_BLESS:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
			if (set_blessed(p_ptr->blessed + damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_HOLY_CHANT:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
			if (set_blessed(p_ptr->blessed + damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_HOLY_PRAYER:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
			if (set_blessed(p_ptr->blessed + damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_RECALL:
      {
			word_of_recall();
			success = TRUE;
			break;
      }

      case ACT_STAR_DESTRUCTION:
      {
         numsides = (BASE_LIGHT_POWER * act_mult) / act_div;
			if (destroy_area(py, px, damroll(numdice, numsides)))
				success = TRUE;
			break;
      }

      case ACT_CURING:
      {
         numsides = ((BASE_CURE_LIGHT * 5) * act_mult) / act_div;
			if (hp_player(damroll(numdice, numsides))) success = TRUE;
			if (set_blind(0)) success = TRUE;
			if (set_poisoned(0)) success = TRUE;
			if (set_confused(0)) success = TRUE;
			if (set_stun(0)) success = TRUE;
			if (set_cut(0)) success = TRUE;
			if (set_image(0)) success = TRUE;
			break;
      }

      case ACT_INVUNERABILITY:
      {
         numsides = (BASE_CURE_LIGHT * act_mult) / act_div;

         if (numdice / 4 < 1) numdice = 1; else numdice = numdice / 4;
         if (numsides / 4 < 1) numsides = 1; else numsides = numsides / 4;

         if (numdice < 3) numdice = 3;
         if (numsides < 3) numsides = 3;

			(void)set_invuln(p_ptr->invuln + damroll(numdice, numsides));
			success = TRUE;
			break;
      }

      case ACT_NEW_LIFE:
      {
			do_cmd_rerate();
			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
			{
				chg_virtue(V_CHANCE, -5);

				msg_print("You are cured of all mutations.");
				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
				p_ptr->update |= PU_BONUS;
				handle_stuff();
			}
			success = TRUE;
			break;
      }

      case ACT_CURE_SW:
      {
         numsides = ((BASE_CURE_LIGHT * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
         if (set_blind(0)) success =  TRUE;
			if (set_confused(0)) success = TRUE;

         numdice /= 7;
         numsides /= 7;

         if (numdice < 3) numdice = 3;
         if (numsides < 3) numsides = 3;

         if (set_cut(p_ptr->cut - (damroll(numdice, numsides) + 25))) success = TRUE;
         break;
      }

      case ACT_CURE_CW:
      {
         numsides = ((BASE_CURE_LIGHT * 4) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
         if (set_blind(0)) success = TRUE;
			if (set_confused(0)) success = TRUE;
         if (set_poisoned(0)) success = TRUE;
         if (set_stun(0)) success = TRUE;
         if (set_cut(0)) success = TRUE;
         break;
      }

      case ACT_HEALING:
      {
         numsides = ((BASE_CURE_LIGHT * 8) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
			if (set_blind(0)) success = TRUE;
			if (set_confused(0)) success = TRUE;
			if (set_poisoned(0)) success = TRUE;
			if (set_stun(0)) success = TRUE;
			if (set_cut(0)) success = TRUE;
			break;
      }

      case ACT_EXPERIENCE:
      {
         numsides = (BASE_EXP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (damroll(numdice, numsides) + 10) * p_ptr->lev;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				success= TRUE;
			}
			break;
      }

      case ACT_SLEEP:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (!p_ptr->free_act)
			{
				msg_print("You fall asleep.");
				if (set_paralyzed(p_ptr->paralyzed + rand_range(4, 8)))
				{
					success = TRUE;
				}
			}
			break;
      }

      case ACT_POISON:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
			{
				if (set_poisoned(p_ptr->poisoned + damroll(numdice, numsides)))
				{
					success = TRUE;
				}
			}
			break;
      }

      case ACT_LOSE_MEMORY:
      {
         numsides = (BASE_EXP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (p_ptr->exp > 0)
			{
				s32b ee = (damroll(numdice, numsides) + 10) * p_ptr->lev;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel your memories fade.");
				lose_exp(ee);
				success= TRUE;
			}
			break;
      }

      case ACT_ENLIGHTENMENT:
      {
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			success= TRUE;
			break;
      }

      case ACT_HEROISM:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_afraid(0)) success = TRUE;
			if (set_hero(p_ptr->hero + damroll(numdice + 2, numsides + 4))) success = TRUE;
			if (hp_player(damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_BERSERK_STRENGTH:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_afraid(0)) success = TRUE;
			if (set_shero(p_ptr->shero + damroll(numdice + 2, numsides + 4))) success = TRUE;
			if (hp_player(damroll(numdice, numsides)+10)) success = TRUE;
			break;
      }

      case ACT_BOLDNESS:
      {
			if (set_afraid(0)) success = TRUE;
			break;
      }

      case ACT_RESTORE_LIFE:
      {
			if (restore_level()) success = TRUE;
			break;
      }

      case ACT_RES_FIRE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_oppose_fire(p_ptr->oppose_fire + damroll(numdice, numsides)))
			{
				success = TRUE;
			}
			break;
      }

      case ACT_RES_COLD:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_oppose_cold(p_ptr->oppose_cold + damroll(numdice, numsides)))
			{
				success = TRUE;
			}
			break;
      }

      case ACT_SLOW_POISON:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_poisoned(p_ptr->poisoned - damroll( numsides, numdice))) success = TRUE;
			break;
      }

      case ACT_NEUTRALIZE_POISON:
      {
				if (set_poisoned(0)) success = TRUE;
				break;
      }

      case ACT_RESISTANCE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			(void)set_oppose_acid(p_ptr->oppose_acid + damroll( numdice, numsides));
			(void)set_oppose_elec(p_ptr->oppose_elec + damroll( numdice, numsides));
			(void)set_oppose_fire(p_ptr->oppose_fire + damroll( numdice, numsides));
			(void)set_oppose_cold(p_ptr->oppose_cold + damroll( numdice, numsides));
			(void)set_oppose_pois(p_ptr->oppose_pois + damroll( numdice, numsides));
			success = TRUE;
			break;
      }

      case ACT_STAR_RESISTANCE:
      {
         numsides = ((BASE_TEMP_BOOST * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			(void)set_oppose_acid(p_ptr->oppose_acid + damroll( numdice, numsides));
			(void)set_oppose_elec(p_ptr->oppose_elec + damroll( numdice, numsides));
			(void)set_oppose_fire(p_ptr->oppose_fire + damroll( numdice, numsides));
			(void)set_oppose_cold(p_ptr->oppose_cold + damroll( numdice, numsides));
			(void)set_oppose_pois(p_ptr->oppose_pois + damroll( numdice, numsides));
			success = TRUE;
			break;
      }

      case ACT_BO_LIGHT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_LITE, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_TAME_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_CONTROL_ANIMAL, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_COLD:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_COLD, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_FIRE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_FIRE, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_STONE_TO_MUD:
      {
			if (!get_aim_dir(&dir)) return FALSE;

      	if (wall_to_mud(dir)) success = TRUE;
			break;
      }

      case ACT_BO_POLYMORPH:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_POLY, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_HEAL_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_HEAL, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_HASTE_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_SPEED, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_SLOW_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_SLOW, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BA_HEAL_MONSTER:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_OLD_HEAL, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_HASTE_MONSTER:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_OLD_SPEED, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_SLOW_MONSTER:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_OLD_SLOW, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BO_CONFUSE_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_CONFUSION, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_SLEEP_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_SLEEP, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_DRAIN_LIFE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_DRAIN, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_MAGIC_MISSILE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_MANA, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_CLONE_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_OLD_CLONE, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_SCARE_MONSTER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
/*  NEEDS GF_FEAR  */
/*         success = fire_bolt_or_beam(beam_pct, GF_OLD_CLONE, dir, damroll(numdice, numsides));*/
         break;
      }

      case ACT_BO_TELEPORT_OTHER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_AWAY_ALL, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_DISARM:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_KILL_TRAP, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BA_POIS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_POIS, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BO_WONDER:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, randint0(MAX_GF), dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BO_ACID:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_ACID, dir, damroll(numdice, numsides));
         break;
      }

      case ACT_BA_DRAGON_FIRE:
      {
         numsides = ((BASE_BALL_POWER * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_FIRE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_DRAGON_ELEMENTAL:
      {
         numsides = ((BASE_BALL_POWER * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

			switch (randint1(5))
			{
				case 1:
				{
               success = fire_ball(GF_ACID, dir, damroll(numdice, numsides), radius);
            }
				case 2:
				{
               success = fire_ball(GF_ELEC, dir, damroll(numdice, numsides), radius);
            }
				case 3:
				{
               success = fire_ball(GF_FIRE, dir, damroll(numdice, numsides), radius);
            }
				case 4:
				{
               success = fire_ball(GF_COLD, dir, damroll(numdice, numsides), radius);
            }
				case 5:
				{
               success = fire_ball(GF_POIS, dir, damroll(numdice, numsides), radius);
            }
         }
         break;
      }

      case ACT_ANNIHILATION:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_DISINTEGRATE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_BA_ROCKETS:
      {
         numsides = ((BASE_BALL_POWER * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_ROCKET, dir, damroll(numdice, numsides), radius);

         break;
      }

      case ACT_SUMMON_HOSTILE:
      {
         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         power = damroll( numdice, numsides);

         power /= (51 - p_ptr->lev);

         if (power < 2) power = 2;

			for (radius = 0; radius < power; radius++)
			{
				if (summon_specific(0, py, px, p_ptr->depth, SUMMON_NO_UNIQUES, TRUE, FALSE, FALSE))
				{
					success = TRUE;
				}
			}
			break;
      }

      case ACT_STARLIGHT:
      {
			int num = damroll(5, 3);
			int y, x;
			int attempts;
         int k = 0;
			cave_type *c_ptr;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (!p_ptr->blind)
			{
				msg_print("The end of the staff glows brightly...");
			}

			for (k = 0; k < num; k++)
			{
				attempts = 1000;

				while (attempts--)
				{
					scatter(&y, &x, py, px, 4);

					if (!in_bounds2(y, x)) continue;

					c_ptr = area(y, x);

					if (!cave_floor_grid(c_ptr)) continue;

					if ((y != py) || (x != px)) break;
				}

				(void)project(0, 0, y, x, damroll(numdice, numsides), GF_LITE_WEAK,
						  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL));
			}
			success = TRUE;
			break;
      }

      case ACT_DISPELL_EVIL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (dispel_evil(damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_PROBING:
      {
			success = probing();
			break;
      }

      case ACT_STAR_PROBING:
      {
/*  NEED ADVANCE PROBING CODE  */
			success = probing();
			break;
      }

      case ACT_POWER:
      {
         numsides = ((BASE_DISPEL_POWER * 3) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (dispel_monsters(damroll(numdice, numsides))) success = TRUE;
			break;
      }

      case ACT_IDENT:
      {
         success = ident_spell();
         break;
      }

      case ACT_HOLINESS:
      {
         numsides = ((BASE_DISPEL_POWER * 3) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         if (dispel_evil(damroll(numdice, numsides))) success = TRUE;

         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (set_protevil(p_ptr->protevil + randint1(25) + damroll(numdice, numsides))) success = TRUE;
			if (set_poisoned(0)) success = TRUE;
			if (set_afraid(0)) success = TRUE;
			if (hp_player((damroll(numdice, numsides)/2))) success = TRUE;
			if (set_stun(0)) success = TRUE;
			if (set_cut(0)) success = TRUE;
			break;
      }

      case ACT_HAVOC:
      {
			call_chaos();
			success = TRUE;
			break;
      }

      case ACT_FULL_DETECTION:
      {
			success = detect_all();
			break;
      }

      case ACT_DEATH:
      {
			msg_print("A feeling of Death flows through your body.");
			take_hit(5000, "Doh!", FALSE);
			success = TRUE;
			break;
      }

      case ACT_RUINATION:
      {
			msg_print("Your nerves and muscles feel weak and lifeless!");
			take_hit(damroll(10, 10), "Doh!", FALSE);
			(void)dec_stat(A_DEX, 25, TRUE);
			(void)dec_stat(A_WIS, 25, TRUE);
			(void)dec_stat(A_CON, 25, TRUE);
			(void)dec_stat(A_STR, 25, TRUE);
			(void)dec_stat(A_CHR, 25, TRUE);
			(void)dec_stat(A_INT, 25, TRUE);
			success = TRUE;
			break;
      }

      case ACT_DETONATIONS:
      {
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(50, 20), "Doh!", FALSE);
			(void)set_stun(p_ptr->stun + 75);
			(void)set_cut(p_ptr->cut + 5000);
			success = TRUE;
			break;
      }

      case ACT_AUGMENTATION:
      {
			if (do_inc_stat(A_STR, damroll(numdice, numdice))) success = TRUE;
			if (do_inc_stat(A_INT, damroll(numdice, numdice))) success = TRUE;
			if (do_inc_stat(A_WIS, damroll(numdice, numdice))) success = TRUE;
			if (do_inc_stat(A_DEX, damroll(numdice, numdice))) success = TRUE;
			if (do_inc_stat(A_CON, damroll(numdice, numdice))) success = TRUE;
			if (do_inc_stat(A_CHR, damroll(numdice, numdice))) success = TRUE;
			break;
      }

      case ACT_LIFE:
      {
			msg_print("You feel life flow through your body!");
			(void)restore_level();
			(void)set_poisoned(0);
			(void)set_blind(0);
			(void)set_confused(0);
			(void)set_image(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)do_res_stat(A_STR, 200);
			(void)do_res_stat(A_CON, 200);
			(void)do_res_stat(A_DEX, 200);
			(void)do_res_stat(A_WIS, 200);
			(void)do_res_stat(A_INT, 200);
			(void)do_res_stat(A_CHR, 200);

			update_stuff();

			(void)hp_player(5000);
			success = TRUE;
			break;
      }

      case ACT_SELF_KNOWLEDGE:
      {
			msg_print("You begin to know yourself a little better...");
			msg_print(NULL);
			self_knowledge();
			success = TRUE;
			break;
      }

      case ACT_STAR_ENLIGHTENMENT:
      {
			msg_print("You begin to feel more enlightened...");
			msg_print(NULL);
			wiz_lite();
			(void)do_inc_stat(A_INT, 1);
			(void)do_inc_stat(A_WIS, 1);
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			(void)detect_treasure();
			(void)detect_objects_gold();
			(void)detect_objects_normal();
			identify_pack();
			self_knowledge();
			success = TRUE;
			break;
      }

      case ACT_STAR_SELF_KNOWLEDGE:
      {
			msg_print("You begin to know yourself a little better...");
			msg_print(NULL);
			identify_pack();
			self_knowledge();
			success = TRUE;
			break;
      }

      case ACT_KNOWLEDGE:
      {
         msg_print("The voices of the ancients boom out!!!");
         msg_print(NULL);
         identify_pack2();
			aggravate_monsters(0);
         success = TRUE;
         break;
      }

      case ACT_BA_SHINING:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

   		chance = randint0(2);
			msg_format("You breathe %s.",
			           ((chance == 0 ? "light" : "darkness")));
			success = fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_BA_LAW:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

   		chance = randint0(2);
			msg_format("You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
			success = fire_ball((chance == 0 ? GF_SOUND : GF_SHARDS), dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_BA_CONF:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

			msg_format("You breathe confusion");

         success = fire_ball(GF_CONFUSION, dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_BA_SOUND:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

			msg_format("You breathe confusion");

         success = fire_ball(GF_SOUND, dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_BA_CHAOS:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

   		chance = randint0(2);
			msg_format("You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
			success = fire_ball((chance == 0 ? GF_CHAOS : GF_DISENCHANT), dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_BA_BALANCE:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

			chance = randint0(4);
			msg_format("You breathe %s.",
			           ((chance == 1) ? "chaos" :
			            ((chance == 2) ? "disenchantment" :
			             ((chance == 3) ? "sound" : "shards"))));
			success = fire_ball(((chance == 1) ? GF_CHAOS :
			           ((chance == 2) ? GF_DISENCHANT :
			            ((chance == 3) ? GF_SOUND : GF_SHARDS))),
			          dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_BA_POWER:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

			msg_format("You breathe the elements!");

         success = fire_ball(GF_MISSILE, dir, damroll(numdice, numsides), radius);
			break;
      }

      case ACT_INC_STR:
      {
			if (do_inc_stat(A_STR, damroll(numdice, numdice))) success = TRUE;
			break;
      }

      case ACT_INC_INT:
      {
			if (do_inc_stat(A_INT, damroll(numdice, numdice))) success = TRUE;
			break;
      }

      case ACT_INC_WIS:
      {
			if (do_inc_stat(A_WIS, damroll(numdice, numdice))) success = TRUE;
			break;
      }

      case ACT_INC_DEX:
      {
			if (do_inc_stat(A_DEX, damroll(numdice, numdice))) success = TRUE;
			break;
      }

      case ACT_INC_CON:
      {
			if (do_inc_stat(A_CON, damroll(numdice, numdice))) success = TRUE;
			break;
      }

      case ACT_INC_CHR:
      {
			if (do_inc_stat(A_CHR, damroll(numdice, numdice))) success = TRUE;
			break;
      }

/*  TEMP VALUES NOT IMPLEMENTED YET!  */
      case ACT_STR_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_INT_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_WIS_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_DEX_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_CON_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_CHR_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_STR_SUST_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_INT_SUST_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_WIS_SUST_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_DEX_SUST_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_CON_SUST_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_CHR_SUST_TEMP:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_TEMP_STEALTH:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_TEMP_SEARCH:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_TEMP_INFRA:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_TEMP_SPEED:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_DEC_STR:
      {
			if (dec_stat(A_STR, s_lev, TRUE)) success = TRUE;
			break;
      }

      case ACT_DEC_INT:
      {
			if (dec_stat(A_INT, s_lev, TRUE)) success = TRUE;
			break;
      }

      case ACT_DEC_WIS:
      {
			if (dec_stat(A_WIS, s_lev, TRUE)) success = TRUE;
			break;
      }

      case ACT_DEC_DEX:
      {
			if (dec_stat(A_DEX, s_lev, TRUE)) success = TRUE;
			break;
      }

      case ACT_DEC_CON:
      {
			if (dec_stat(A_CON, s_lev, TRUE)) success = TRUE;
			break;
      }

      case ACT_DEC_CHR:
      {
			if (dec_stat(A_CHR, s_lev, TRUE)) success = TRUE;
			break;
      }

      case  ACT_TEMP_SLOW:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_STAR_RESIST:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_SUSTAIN:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_STAR_SUSTAIN:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_POISON:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_NETHER:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_LIGHT:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_DARK:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_FEARLESS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_CONFUSION:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_CHAOS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_DISENCHANT:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_BLINDNESS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_NEXUS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_SOUND:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_RES_SHARDS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_BLINDNESS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_FEAR:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_CONFUSION:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_HALLUCINATION:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_DISEASE:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_PARALYSIS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_AGGRAVATION:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_SEE_INVIS:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case  ACT_TEMP_TELEPATHY:
      {
         msg_print("This Magic still not implemented!");
         break;
      }

      case ACT_REPAIR:
      {
         numsides = (BASE_REPAIR_POWER * act_mult) / act_div;
         success = repair_spell(damroll(numdice, numsides));
         break;
      }

      case ACT_STAR_REPAIR:
      {
         numsides = (2 * BASE_REPAIR_POWER * act_mult) / act_div;
         success = repair_spell(damroll(numdice, numsides));
         break;
      }

      case   ACT_DETECT_EVIL:
      {
   		(void)detect_monsters_evil();
         success = TRUE;
         break;
      }

      case   ACT_REMOVE_FEAR:
      {
   		(void)set_afraid(0);
         success = TRUE;
         break;
      }

      case   ACT_LIGHT:
      {
         numsides = (BASE_LIGHT_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         success = lite_area( damroll( numdice, numsides), radius);
         break;
      }

      case   ACT_DETECT_TRAPS_DOORS:
      {
   		(void)detect_traps();
	   	(void)detect_doors();
		   (void)detect_stairs();
         success = TRUE;
         break;
      }

      case   ACT_CURE_POISON:
      {
   		(void)set_poisoned(0);
         success = TRUE;
         break;
      }

      case   ACT_SEE_INIVISBLE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

   		(void)set_tim_invis(p_ptr->tim_invis + rand_range(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_HOLY_ORB:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

   		if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_HOLY_FIRE, dir, damroll(numdice, numsides), radius);
         break;
      }

      case   ACT_PROTECTION_FROM_EVIL:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_protevil(p_ptr->protevil + damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_GLYPH_WARDING:
      {
   		(void)warding_glyph();
         success = TRUE;
         break;
      }

      case   ACT_EXORCISM:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)dispel_undead(damroll(numdice, numsides));
	   	(void)dispel_demons(damroll(numdice, numsides));
		   (void)turn_evil(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_DISPELL_UNDEAD_AND_DEMONS:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)dispel_undead(damroll(numdice, numsides));
	   	(void)dispel_demons(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_DAY_OF_THE_DOVE:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)charm_monsters(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_DISPEL_EVIL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)dispel_evil(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_BANISH_EVIL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		if (banish_evil(damroll(numdice, numsides)))
	   	{
		   	msg_print("The power of your god banishes evil!");
            success = TRUE;
   		}
         break;
      }

      case   ACT_HOLY_WORD:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         (void)dispel_evil(damroll(numdice, numsides));

         numsides = ((BASE_CURE_LIGHT * 8) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
		   (void)set_afraid(0);
   		(void)set_poisoned(0);
	   	(void)set_stun(0);
		   (void)set_cut(0);
         success = TRUE;
         break;
      }

      case   ACT_WARDING_TRUE:
      {
   		(void)warding_glyph();
	   	(void)glyph_creation();
         success = TRUE;
         break;
      }

      case   ACT_PRAYER:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_blessed(p_ptr->blessed + damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_BLESS_WEAPON:
      {
         success = bless_weapon();
         break;
      }

      case   ACT_RESTORATION:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)do_res_stat(A_STR, damroll( numdice, numsides));
	   	(void)do_res_stat(A_INT, damroll( numdice, numsides));
		   (void)do_res_stat(A_WIS, damroll( numdice, numsides));
   		(void)do_res_stat(A_DEX, damroll( numdice, numsides));
	   	(void)do_res_stat(A_CON, damroll( numdice, numsides));
		   (void)do_res_stat(A_CHR, damroll( numdice, numsides));
   		(void)restore_level();
         success = TRUE;
         break;
      }

      case   ACT_HEALING_TRUE:
      {
         numsides = ((BASE_CURE_LIGHT * 16) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
	   	(void)set_stun(0);
		   (void)set_cut(0);

         success = TRUE;
         break;
      }

      case   ACT_HOLY_VISION:
      {
/*  Should change this to something else!!!  */
   		success = identify_fully();
         break;
      }

      case   ACT_DIVINE_INTERVENTION:
      {
         numsides = ((BASE_BALL_POWER * 7) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
   		(void)project(0, 1, py, px, damroll(numdice, numsides), GF_HOLY_FIRE, PROJECT_KILL);

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
	   	(void)dispel_monsters(damroll( numdice, numsides));

         (void)slow_monsters();

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)stun_monsters(damroll( numdice, numsides));

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
	   	(void)confuse_monsters(damroll( numdice, numsides));

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
		   (void)turn_monsters(damroll( numdice, numsides));

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)stasis_monsters(damroll( numdice, numsides));

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
	   	(void)summon_specific(-1, py, px, damroll( numdice, numsides), SUMMON_ANGEL, TRUE, TRUE, TRUE);

         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
		   (void)set_shero(p_ptr->shero + damroll( numdice, numsides));

         numsides = (BASE_CURE_LIGHT * 4 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)hp_player(damroll( numdice, numsides));

   		(void)set_afraid(0);
         success = TRUE;
         break;
      }

      case   ACT_HOLY_INVUNERABILITY:
      {
         numsides = (BASE_INVUNERABLE_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
   		(void)set_invuln(p_ptr->invuln + damroll(numsides, numdice));
         success = TRUE;
         break;
      }

      case   ACT_DETECT_MONSTERS:
      {
   		(void)detect_monsters_normal();
         success = TRUE;
         break;
      }

      case   ACT_CONFUSE_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

       	if (!get_aim_dir(&dir)) return FALSE;
		   success = confuse_monster(dir, damroll( numdice, numsides));
         break;
      }

      case   ACT_SLEEP_MONSTER:
      {
		   if (!get_aim_dir(&dir)) return FALSE;

	   	success = sleep_monster(dir);
         break;
      }

      case   ACT_IDENTIFY:
      {
   		success = ident_spell();
         break;
      }

      case   ACT_SLOW_MONSTER:
      {
   		if (!get_aim_dir(&dir)) return FALSE;
	   	success = slow_monster(dir);
         break;
      }

      case   ACT_MASS_SLEEP:
      {
   		success = sleep_monsters();
         break;
      }

      case   ACT_TELEPORT_AWAY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	success = fire_beam(GF_AWAY_ALL, dir, damroll( numdice, numsides));
         break;
      }

      case   ACT_SPEED:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

			(void)set_fast(damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_DETECT_ALL:
      {
   		(void)detect_all();
         success = TRUE;
         break;
      }

      case   ACT_STAR_IDENTIFICATION:
      {
         success = identify_fully();
         break;
      }

      case   ACT_DETECT_OBJECTS_AND_TREASURE:
      {
   		(void)detect_objects_normal();
	   	(void)detect_treasure();
		   (void)detect_objects_gold();
         success = TRUE;
         break;
      }

      case   ACT_DETECT_ENCHANTMENT:
      {
      	(void)detect_objects_magic();
         success = TRUE;
         break;
      }

      case   ACT_CHARM_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

	   	if (!get_aim_dir(&dir)) return FALSE;

		   (void)charm_monster(dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_DIMENSION_DOOR:
      {
   		msg_print("You open a dimensional gate. Choose a destination.");
	   	return dimension_door();
         break;
      }

      case   ACT_STASIS:
      {
   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)stasis_monster(dir);
         success = TRUE;
         break;
      }

      case   ACT_TELEKINESIS:
      {
         numsides = (BASE_BOLT_POWER * 3 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	fetch(dir, damroll(numdice, numsides), FALSE);
         break;
      }

      case   ACT_EXPLOSIVE_RUNE:
      {
   		(void)explosive_rune();
         success = TRUE;
         break;
      }

      case   ACT_CLAIRVOYANCE:
      {
   		wiz_lite();
	   	if (!(p_ptr->telepathy))
		   {
            numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
            if (numsides < 1) numsides = 1;
		      (void)set_tim_esp(p_ptr->tim_esp + rand_range(25, 55));
   		}
         success = TRUE;
         break;
      }

      case   ACT_SORCERY_ENCHANT_WEAPON:
      {
         numsides = (BASE_ENCHANT_VALUE * act_mult / 2) / act_div;
         if (numsides < 1) numsides = 1;

   		success = enchant_spell( damroll(numdice, numsides), damroll(numdice, numsides), 0);
         break;
      }

      case   ACT_SORCERY_ENCHANT_ARMOR:
      {
         numsides = (BASE_ENCHANT_VALUE * act_mult / 2) / act_div;

         if (numsides < 1) numsides = 1;

   		success = enchant_spell(0, 0, damroll(numdice, numsides));
         break;
      }

      case   ACT_ALCHEMY:
      {
   		success = alchemy();
         break;
      }

      case   ACT_GLOBE_INVUNERABILTY:
      {
         numsides = (BASE_INVUNERABLE_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)set_invuln(p_ptr->invuln + damroll(numsides, numdice));
         success = TRUE;
         break;
      }

      case   ACT_CHARM_ANIMAL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)charm_animal(dir, damroll(numsides, numdice));
         success = TRUE;
         break;
      }

      case   ACT_MINOR_RESISTANCE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_oppose_cold(p_ptr->oppose_cold + damroll( numsides, numdice));
	   	(void)set_oppose_fire(p_ptr->oppose_fire + damroll( numsides, numdice));
		   (void)set_oppose_elec(p_ptr->oppose_elec + damroll( numsides, numdice));
         break;
      }

      case   ACT_LIGHTNING_BOLT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	success = fire_bolt_or_beam(beam_pct, GF_ELEC, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_FROST_BOLT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	success = fire_bolt_or_beam(beam_pct, GF_COLD, dir, damroll(numdice, numsides));
         break;
         break;
      }

      case   ACT_LIGHT_BEAM:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	success = fire_bolt_or_beam(beam_pct, GF_LITE_WEAK, dir, damroll(numdice, numsides));
         break;
         break;
      }

      case   ACT_ENTANGLE:
      {
   		(void)slow_monsters();
         break;
      }

      case   ACT_HERBAL_HEALING:
      {
         numsides = ((BASE_CURE_LIGHT * 8) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         if (hp_player(damroll(numdice, numsides))) success = TRUE;
	   	(void)set_stun(0);
		   (void)set_cut(0);
   		(void)set_poisoned(0);
         break;
      }

      case   ACT_DOOR_BUILDING:
      {
   		(void)door_creation();
         break;
      }

      case   ACT_CREATE_STAIRS:
      {
   		(void)stair_creation();
         break;
      }

      case   ACT_STONE_SKIN:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_shield(p_ptr->shield + damroll( numdice, numsides));
         break;
      }

      case   ACT_RESISTANCE_TRUE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_oppose_acid(p_ptr->oppose_acid + damroll( numdice, numsides));
	   	(void)set_oppose_elec(p_ptr->oppose_elec + damroll( numdice, numsides));
		   (void)set_oppose_fire(p_ptr->oppose_fire + damroll( numdice, numsides));
   		(void)set_oppose_cold(p_ptr->oppose_cold + damroll( numdice, numsides));
	   	(void)set_oppose_pois(p_ptr->oppose_pois + damroll( numdice, numsides));
         break;
      }

      case   ACT_ANIMAL_FRIENDSHIP:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)charm_animals(damroll(numdice, numsides));
         break;
      }

      case   ACT_WALL_OF_STONE:
      {
   		(void)wall_stone();
         success = TRUE;
         break;
      }

      case   ACT_PROTECTION_FROM_CORROSION:
      {
   		success = rustproof();
         break;
      }

      case   ACT_EARTHQUAKE:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 5;

         if (radius > 20) radius = 20;
         if (radius < 6) radius = 6;
   		(void)earthquake(py, px, radius);
         success = TRUE;
         break;
      }

      case   ACT_WHIRLWIND:
      {
			int y = 0, x = 0;
			cave_type *c_ptr;
			monster_type *m_ptr;

			for (dir = 0; dir <= 9; dir++)
			{
				y = py + ddy[dir];
				x = px + ddx[dir];

				/* paranoia */
				if (!in_bounds2(y, x)) continue;
				c_ptr = area(y, x);

				/* Get the monster */
				m_ptr = &m_list[c_ptr->m_idx];

				/* Hack -- attack monsters */
				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
					py_attack(y, x);
			}
         break;
      }

      case   ACT_BLIZZARD:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

	   	if (!get_aim_dir(&dir)) return FALSE;

		   success = fire_ball(GF_COLD, dir, damroll( numdice, numsides), radius);
         break;
      }

      case   ACT_LIGHTNING_STORM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

	   	if (!get_aim_dir(&dir)) return FALSE;

		   success = fire_ball(GF_ELEC, dir, damroll( numdice, numsides), radius);
         break;
      }

      case   ACT_WHIRLPOOL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

	   	if (!get_aim_dir(&dir)) return FALSE;

		   success = fire_ball(GF_WATER, dir, damroll( numdice, numsides), radius);
         break;
      }

      case   ACT_CALL_SUNLIGHT:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

   		(void)fire_ball(GF_LITE, 0, damroll( numdice, numsides), radius);
	   	wiz_lite();
		   if ((p_ptr->prace == RACE_VAMPIRE) && !p_ptr->resist_lite)
   		{
	   		msg_print("The sunlight scorches your flesh!");
		   	take_hit(damroll( numdice, numsides) / 3, "sunlight", FALSE);
   		}
         success = TRUE;
         break;
      }

      case   ACT_ELEMENTAL_BRAND:
      {
   		brand_weapon(0);
         break;
      }

      case   ACT_NATURES_WRATH:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)dispel_monsters(damroll( numdice, numsides));
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 5;

         if (radius > 20) radius = 20;
         if (radius < 6) radius = 6;
   		(void)earthquake(py, px, radius);

         numsides = ((BASE_BALL_POWER * 7) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 10;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         (void)project(0, radius, py, px, damroll(numdice, numsides),
            GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM);
         break;
      }

      case   ACT_MAGIC_MISSILE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_MANA, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_TRAP_DOOR_DESTRUCTION:
      {
   		(void)destroy_doors_touch();
         success = TRUE;
         break;
      }

      case   ACT_TOUCH_CONFUSION:
      {
   		if (!p_ptr->confusing)
	   	{
		   	msg_print("Your hands start glowing.");
			   p_ptr->confusing = TRUE;
   			p_ptr->redraw |= (PR_STATUS);
	   	}
         break;
      }

      case   ACT_MANA_BURST:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_MISSILE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_FIRE_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_FIRE, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_FIST_OF_FORCE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_DISINTEGRATE, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_WONDER:
      {
         /*  May Need to redo this  */

   		/*
	   	 * This spell should become more useful (more
		    * controlled) as the player gains experience levels.
   		 * Thus, add 1/5 of the player's level to the die roll.
	   	 * This eliminates the worst effects later on, while
		    * keeping the results quite random.  It also allows
   		 * some potent effects only at high level.
	   	 */

         	int die = randint1(100) + plev / 5;

   			if (die < 26)
   				chg_virtue(V_CHANCE, 1);

   			if (!get_aim_dir(&dir)) return FALSE;
   			if (die > 100)
   				msg_print("You feel a surge of power!");
   			if (die < 8) (void)clone_monster(dir);
   			else if (die < 14) (void)speed_monster(dir);
   			else if (die < 26) (void)heal_monster(dir);
   			else if (die < 31) (void)poly_monster(dir);
   			else if (die < 36)
   				(void)fire_bolt_or_beam(beam_pct, GF_MISSILE, dir,
   				                  damroll(3 + ((plev - 1) / 5), 4));
   			else if (die < 41) (void)confuse_monster(dir, plev);
   			else if (die < 46) (void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
   			else if (die < 51) (void)lite_line(dir);
   			else if (die < 56)
   				(void)fire_bolt_or_beam(beam_pct, GF_ELEC, dir,
   				                  damroll(3 + ((plev - 5) / 4), 8));
   			else if (die < 61)
   				(void)fire_bolt_or_beam(beam_pct, GF_COLD, dir,
   				                  damroll(5 + ((plev - 5) / 4), 8));
   			else if (die < 66)
   				(void)fire_bolt_or_beam(beam_pct, GF_ACID, dir,
   				                  damroll(6 + ((plev - 5) / 4), 8));
   			else if (die < 71)
   				(void)fire_bolt_or_beam(beam_pct, GF_FIRE, dir,
   				                  damroll(8 + ((plev - 5) / 4), 8));
   			else if (die < 76) (void)drain_life(dir, 75);
   			else if (die < 81) (void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
   			else if (die < 86) (void)fire_ball(GF_ACID, dir, 40 + plev, 2);
   			else if (die < 91) (void)fire_ball(GF_ICE, dir, 70 + plev, 3);
   			else if (die < 96) (void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
   			else if (die < 101) (void)drain_life(dir, 100 + plev);
   			else if (die < 104)
   			{
   				(void)earthquake(py, px, 12);
   			}
   			else if (die < 106)
   			{
   				(void)destroy_area(py, px, 15);
   			}
   			else if (die < 108)
   			{
   				(void)genocide(TRUE);
   			}
   			else if (die < 110) (void)dispel_monsters(120);
   			else /* RARE */
   			{
   				(void)dispel_monsters(150);
   				(void)slow_monsters();
   				(void)sleep_monsters();
   				(void)hp_player(300);
   			}
         break;
      }

      case   ACT_CHAOS_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_CHAOS, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_SONIC_BOOM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 20;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

   		msg_print("BOOM! Shake the room!");
	   	(void)project(0, radius, py, px, damroll( numdice, numsides),
		      GF_SOUND, PROJECT_KILL | PROJECT_ITEM);
         break;
      }

      case   ACT_DOOM_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_MANA, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_FIRE_BALL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_FIRE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_DESTRUCTION:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 5;

         if (radius > 20) radius = 20;
         if (radius < 6) radius = 6;

   		(void)destroy_area(py, px, radius);
         break;
      }

      case   ACT_INVOKE_LOGRUS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_CHAOS, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_POLYMORPH_OTHER:
      {
   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)poly_monster(dir);
         success = TRUE;
         break;
      }

      case   ACT_CHAIN_LIGHTNING:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		for (dir = 0; dir <= 9; dir++)
			(void)fire_beam(GF_ELEC, dir, damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_DISINTEGRATION:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_DISINTEGRATE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_ALTER_REALITY:
      {
   		alter_reality();
         break;
      }

      case   ACT_POLYMORPH_SELF:
      {
   		do_poly_self();
         break;
      }

      case   ACT_CHAOS_BRANDING:
      {
   		brand_weapon(1);
         break;
      }

      case   ACT_BEAM_OF_GRAVITY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         if (!get_aim_dir(&dir)) return FALSE;

			(void)fire_beam(GF_GRAVITY, dir, damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_METEOR_SWARM:
      {
			int x, y;
			cave_type *c_ptr;
			int b = rand_range(10, 20);

         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

			for (i = 0; i < b; i++)
			{
				int count = 0;

				while (count < 1000)
				{
					count++;

					x = px - 5 + randint1(10);
					y = py - 5 + randint1(10);

					/* paranoia */
					if (!in_bounds(y, x)) continue;

					c_ptr = area(y, x);

					/* keep going if not in LOS */
					if (!player_has_los_grid(c_ptr)) continue;

					/* if close enough - exit */
					if (distance(py, px, y, x) < 6) break;
				}

				if (count >= 1000) break;

				(void)project(0, radius, y, x, damroll( numdice, numsides), GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
			}
         break;
      }

      case   ACT_FIRE_STRIKE:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 50;

         if (radius > 9) radius = 9;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_FIRE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_MAGIC_ROCKET:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 50;

         if (radius > 9) radius = 9;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_ROCKET, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_MANA_STORM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 50;

         if (radius > 9) radius = 9;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_MANA, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_BREATH_LOGRUS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_DISINTEGRATE, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_CALL_THE_VOID:
      {
   		call_the_();
         success = TRUE;
         break;
      }

      case   ACT_DETECT_UNLIFE:
      {
   		(void)detect_monsters_nonliving();
         success = TRUE;
         break;
      }

      case   ACT_MALEDICTION:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

		   if (!get_aim_dir(&dir)) return FALSE;
   		/* A radius-0 ball may (1) be aimed at objects etc.,
	   	 * and will affect them; (2) may be aimed at ANY
		    * visible monster, unlike a 'bolt' which must travel
   		 * to the monster. */

	   	success = fire_ball(GF_HELL_FIRE, dir, damroll( numdice, numsides), 0);

   		if (one_in_(5))
	   	{
		   	/* Special effect first */
			   i = randint1(1000);
   			if (i == 666)
	   			(void)fire_bolt(GF_DEATH_RAY, dir, damroll( numdice, numsides) * 10);
		   	else if (i < 500)
			   	(void)fire_bolt(GF_TURN_ALL, dir, damroll( numdice, numsides) / 2);
   			else if (i < 800)
	   			(void)fire_bolt(GF_OLD_CONF, dir, damroll( numdice, numsides) / 2);
		   	else
			   	(void)fire_bolt(GF_STUN, dir, damroll( numdice, numsides) / 2);
   		}
         break;
      }

      case   ACT_STINKING_CLOUD:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_POIS, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_RESIST_POISON:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_oppose_pois(p_ptr->oppose_pois + damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_HORRIFY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)fear_monster(dir, damroll( numdice, numsides));
		   (void)stun_monster(dir, damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_ENSLAVE_UNDEAD:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)control_one_undead(dir, damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_ORB_OF_ENTROPY:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_OLD_DRAIN, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_NETHER_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_NETHER, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_TERROR:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
		   (void)turn_monsters(damroll( numdice, numsides));
         break;
      }

      case   ACT_VAMPIRIC_DRAIN:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	i = damroll( numdice, numsides);   /* Dmg */
		   if (drain_gain_life(dir, i))
   		{
	   		/*
		   	 * Hack - this only happens when monster is seen to
			    * be hit.
   			 */

	   		/* Gain nutritional sustenance: 150/hp drained */
		   	/* A Food ration gives 5000 food points (by contrast) */
			   /* Don't ever get more than "Full" this way */
   			/* But if we ARE Gorged, it won't cure us */
	   		i = p_ptr->food + MIN(5000, 100 * i);
		   	if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
			   	(void)set_food(i >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : i);
   		}
         break;
      }

      case   ACT_POISON_BRAND:
      {
   		brand_weapon(2);
         success = TRUE;
         break;
      }

      case   ACT_DISPEL_GOOD:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)dispel_good(damroll(numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_BERSERK:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_shero(p_ptr->shero + damroll( numdice, numsides));
	   	(void)hp_player(damroll( numdice, numsides) / 3);
		   (void)set_afraid(0);
         break;
      }

      case   ACT_INVOKE_SPIRITS:
      {
			int i = randint1(100) + s_lev / 5;
			if (!get_aim_dir(&dir)) return FALSE;

			msg_print("You call on the power of the dead...");

			if (i < 26)
				chg_virtue(V_CHANCE, 1);

			if (i > 100)
				msg_print("You feel a surge of eldritch force!");

			if (i < 8)
			{
				msg_print("Oh no! Mouldering forms rise from the earth around you!");
				(void)summon_specific(0, py, px, p_ptr->depth, SUMMON_UNDEAD, TRUE, FALSE, FALSE);

				chg_virtue(V_UNLIFE, 1);
			}
			else if (i < 14)
			{
				msg_print("An unnamable evil brushes against your mind...");
				(void)set_afraid(p_ptr->afraid + rand_range(4, 8));
			}
			else if (i < 26)
			{
				msg_print("Your head is invaded by a horde of gibbering spectral voices...");
				(void)set_confused(p_ptr->confused + rand_range(4, 8));
			}
			else if (i < 31)
			{
				(void)poly_monster(dir);
			}
			else if (i < 36)
			{
				(void)fire_bolt_or_beam(beam_pct, GF_MISSILE, dir,
					damroll(3 + ((plev - 1) / 5), 4));
			}
			else if (i < 41)
			{
				(void)confuse_monster (dir, plev);
			}
			else if (i < 46)
			{
				(void)fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
			}
			else if (i < 51)
			{
				(void)lite_line(dir);
			}
			else if (i < 56)
			{
				(void)fire_bolt_or_beam(beam_pct, GF_ELEC, dir,
					damroll(3 + ((plev - 5) / 4), 8));
			}
			else if (i < 61)
			{
				(void)fire_bolt_or_beam(beam_pct, GF_COLD, dir,
					damroll(5 + ((plev - 5) / 4), 8));
			}
			else if (i < 66)
			{
				(void)fire_bolt_or_beam(beam_pct, GF_ACID, dir,
					damroll(6 + ((plev - 5) / 4), 8));
			}
			else if (i < 71)
			{
				(void)fire_bolt_or_beam(beam_pct, GF_FIRE, dir,
					damroll(8 + ((plev - 5) / 4), 8));
			}
			else if (i < 76)
			{
				(void)drain_life(dir, 75);
			}
			else if (i < 81)
			{
				(void)fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
			}
			else if (i < 86)
			{
				(void)fire_ball(GF_ACID, dir, 40 + plev, 2);
			}
			else if (i < 91)
			{
				(void)fire_ball(GF_ICE, dir, 70 + plev, 3);
			}
			else if (i < 96)
			{
				(void)fire_ball(GF_FIRE, dir, 80 + plev, 3);
			}
			else if (i < 101)
			{
				(void)drain_life(dir, 100 + plev);
			}
			else if (i < 104)
			{
				(void)earthquake(py, px, 12);
			}
			else if (i < 106)
			{
				(void)destroy_area(py, px, 15);
			}
			else if (i < 108)
			{
				(void)genocide(TRUE);
			}
			else if (i < 110)
			{
				(void)dispel_monsters(120);
			}
			else
			{ /* RARE */
				(void)dispel_monsters(150);
				(void)slow_monsters();
				(void)sleep_monsters();
				(void)hp_player(300);
			}

			if (i < 31)
				msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
         success = TRUE;
         break;
      }

      case   ACT_DARK_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_DARK, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_BATTLE_FRENZY:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)set_shero(p_ptr->shero + damroll( numdice, numsides));
	   	(void)hp_player(damroll( numdice, numsides) / 3);
		   (void)set_afraid(0);
   		if (!p_ptr->fast)
	   	{
		   	(void)set_fast(damroll( numdice, numsides));
   		}
	   	else
		   {
			   (void)set_fast(p_ptr->fast + (damroll( numdice, numsides) / 4));
   		}
         success = TRUE;
         break;
      }

      case   ACT_VAMPIRISM_TRUE:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

		   for (i = 0; i < 3; i++)
   		{
	   		(void)drain_gain_life(dir, damroll( numdice, numsides));
		   }
         success = TRUE;
         break;
      }

      case   ACT_VAMPIRIC_BRANDING:
      {
   		brand_weapon(3);
         break;
      }

      case   ACT_DARKNESS_STORM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_DARK, dir, damroll(numdice, numsides), radius);

         break;
      }

      case   ACT_DEATH_RAY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)death_ray(dir, damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_RAISE_DEAD:
      {
			if (raise_dead(py, px, (bool)(!one_in_(3))))
			{
				msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
			}
			else
			{
				msg_print("Nothing happens.");
			}
         success = TRUE;
         break;
      }

      case   ACT_WORD_OF_DEATH:
      {
         numsides = ((BASE_DISPEL_POWER) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (dispel_living(damroll(numdice, numsides))) success = TRUE;
         break;
      }

      case   ACT_EVOCATION:
      {
         numsides = (BASE_DISPEL_POWER * 2 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)dispel_monsters( damroll( numdice, numsides));
	   	(void)turn_monsters( damroll( numdice, numsides));
		   (void)banish_monsters( damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_HELLFIRE:
      {
         numsides = (BASE_BALL_POWER * 5 * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         success = fire_ball(GF_HELL_FIRE, dir, damroll(numdice, numsides), radius);
   		take_hit(rand_range(50, 100), "the strain of casting Hellfire", FALSE);
         break;
      }

      case   ACT_OMNICIDE:
      {
     		p_ptr->csp -= 100;

   		/* Display doesn't show mana cost (100)
   		 * as deleted until the spell has finished. This gives a
   		 * false impression of how high your mana is climbing.
   		 * Therefore, 'deduct' the cost temporarily before entering the
   		 * loop, then add it back at the end so that the rest of the
   		 * program can deduct it properly
   		 */
   		for (i = 1; i < m_max; i++)
   		{
   			monster_type *m_ptr = &m_list[i];
   			monster_race *r_ptr = &r_info[m_ptr->r_idx];

   			/* Paranoia -- Skip dead monsters */
   			if (!m_ptr->r_idx) continue;

   			/* Hack -- Skip Unique Monsters */
   			if (r_ptr->flags1 & (RF1_UNIQUE)) continue;

   			/* Hack -- Skip Quest Monsters */
   			if (r_ptr->flags1 & RF1_QUESTOR) continue;

   			/* Notice changes in view */
   			if (r_ptr->flags7 & (RF7_LITE_1 | RF7_LITE_2))
   			{
   				/* Update some things */
   				p_ptr->update |= (PU_MON_LITE);
   			}

   			/* Delete the monster */
   			delete_monster_idx(i);

   			/* Take damage */
   			take_hit(randint1(4), "the strain of casting Omnicide", FALSE);

   			/* Absorb power of dead soul - up to twice max. mana */
   			if (p_ptr->csp < (p_ptr->msp * 2))
   				p_ptr->csp++;

   			/* Visual feedback */
   			move_cursor_relative(py, px);

   			/* Redraw */
   			p_ptr->redraw |= (PR_HP | PR_MANA);

   			/* Window stuff */
   			p_ptr->window |= (PW_PLAYER);
   			p_ptr->window |= (PW_SPELL);

   			/* Handle */
   			handle_stuff();

   			/* Fresh */
   			Term_fresh();

   			/* Delay */
   			Term_xtra(TERM_XTRA_DELAY,
   				delay_factor * delay_factor * delay_factor);
   		}

   		/* Restore, ready to be deducted properly */
   		p_ptr->csp += 100;

         break;
      }

      case   ACT_WRAITHFORM:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_wraith_form(p_ptr->wraith_form + (damroll( numdice, numsides) / 3));
         success = TRUE;
         break;
      }

      case   ACT_MIND_BLAST:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         success = fire_bolt_or_beam(beam_pct, GF_PSI, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_SHUFFLE:
      {
   		/* A limited power 'wonder' spell */
   		int i = randint1(120);

   		if ((p_ptr->pclass == CLASS_ROGUE) ||
   			(p_ptr->pclass == CLASS_HIGH_MAGE))
   			i = (randint1(110)) + plev / 5;
   		/* Card sharks and high mages get a level bonus */

   		msg_print("You shuffle the deck and draw a card...");

   		if (i < 30)
   			chg_virtue(V_CHANCE, 1);

   		if (i < 7)
   		{
   			msg_print("Oh no! It's Death!");
   			for (i = 0; i < randint1(3); i++)
   				(void)activate_hi_summon();
   		}
   		else if (i < 14)
   		{
   			msg_print("Oh no! It's the Devil!");
   			(void)summon_specific(0, py, px, p_ptr->depth, SUMMON_DEMON, TRUE, FALSE, FALSE);
   		}
   		else if (i < 18)
   		{
   			int count = 0;

   			msg_print("Oh no! It's the Hanged Man.");
   			(void)activate_ty_curse(FALSE, &count);
   		}
   		else if (i < 22)
   		{
   			msg_print("It's the swords of discord.");
   			aggravate_monsters(0);
   		}
   		else if (i < 26)
   		{
   			msg_print("It's the Fool.");
   			(void)do_dec_stat(A_INT);
   			(void)do_dec_stat(A_WIS);
   		}
   		else if (i < 30)
   		{
   			msg_print("It's the picture of a strange monster.");
   			if (!(summon_specific(0, py, px, (p_ptr->depth * 3) / 2, rand_range(33, 38), TRUE, FALSE, FALSE)))
   				bad = TRUE;
   		}
   		else if (i < 33)
   		{
   			msg_print("It's the Moon.");
   			(void)unlite_area(10, 3);
   		}
   		else if (i < 38)
   		{
   			msg_print("It's the Wheel of Fortune.");
   			wild_magic(randint0(32));
   		}
   		else if (i < 40)
   		{
   			msg_print("It's a teleport trump card.");
   			teleport_player(10);
   		}
   		else if (i < 42)
   		{
   			msg_print("It's Justice.");
   			(void)set_blessed(p_ptr->blessed + p_ptr->lev);
   		}
   		else if (i < 47)
   		{
   			msg_print("It's a teleport trump card.");
   			teleport_player(100);
   		}
   		else if (i < 52)
   		{
   			msg_print("It's a teleport trump card.");
   			teleport_player(200);
   		}
   		else if (i < 60)
   		{
   			msg_print("It's the Tower.");
   			wall_breaker();
   		}
   		else if (i < 72)
   		{
   			msg_print("It's Temperance.");
   			(void)sleep_monsters_touch();
   		}
   		else if (i < 80)
   		{
   			msg_print("It's the Tower.");
   			(void)earthquake(py, px, 5);
   		}
   		else if (i < 82)
   		{
   			msg_print("It's the picture of a friendly monster.");
   			if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE1, FALSE, TRUE, TRUE)))
   				bad = TRUE;
   		}
   		else if (i < 84)
   		{
   			msg_print("It's the picture of a friendly monster.");
   			if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE2, FALSE, TRUE, TRUE)))
   				bad = TRUE;
   		}
   		else if (i < 86)
   		{
   			msg_print("It's the picture of a friendly monster.");
   			if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE4, FALSE, TRUE, TRUE)))
   				bad = TRUE;
   		}
   		else if (i < 88)
   		{
   			msg_print("It's the picture of a friendly monster.");
   			if (!(summon_specific(-1, py, px, (p_ptr->depth * 3) / 2, SUMMON_BIZARRE5, FALSE, TRUE, TRUE)))
   				bad = TRUE;
   		}
   		else if (i < 96)
   		{
   			msg_print("It's the Lovers.");
   			if (get_aim_dir(&dir))
   				(void)charm_monster(dir, MIN(p_ptr->lev, 20));
   		}
   		else if (i < 101)
   		{
   			msg_print("It's the Hermit.");
   			(void)wall_stone();
   		}
   		else if (i < 111)
   		{
   			msg_print("It's the Judgement.");
   			do_cmd_rerate();
   			if (p_ptr->muta1 || p_ptr->muta2 || p_ptr->muta3)
   			{
   				msg_print("You are cured of all mutations.");
   				p_ptr->muta1 = p_ptr->muta2 = p_ptr->muta3 = 0;
   				p_ptr->update |= PU_BONUS;
   				handle_stuff();
   			}
   		}
   		else if (i < 120)
   		{
   			msg_print("It's the Sun.");
   			wiz_lite();
   		}
   		else
   		{
   			msg_print("It's the World.");
   			if (p_ptr->exp < PY_MAX_EXP)
   			{
   				s32b ee = (p_ptr->exp / 25) + 1;
   				if (ee > 5000) ee = 5000;
   				msg_print("You feel more experienced.");
   				gain_exp(ee);
   			}
   		}
         break;
      }

      case   ACT_RESET_RECALL:
      {
      	char	ppp[80];
      	char	tmp_val[160];

			/* Prompt */
			sprintf(ppp, "Reset to which level (1-%d): ", p_ptr->max_depth);

			/* Default */
			sprintf(tmp_val, "%d", MAX(p_ptr->depth, 1));

			/* Ask for a level */
			if (get_string(ppp, tmp_val, 10))
			{
				/* Extract request */
				i = atoi(tmp_val);

				/* Paranoia */
				if (i < 1) i = 1;

				/* Paranoia */
				if (i > p_ptr->max_depth) i = p_ptr->max_depth;

				p_ptr->max_depth = i;

				/* Accept request */
				msg_format("Recall depth set to level %d (%d').", i, i * 50);

            success = TRUE;
			}
			else
			{
				success =  FALSE;
			}
        break;
      }

      case   ACT_REACH:
      {
         numsides = (BASE_BOLT_POWER * 3 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	fetch(dir, damroll(numdice, numsides), FALSE);
         break;
      }

      case   ACT_SUMMON_MONSTER:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
	   	(void)summon_specific(-1, py, px, damroll( numdice, numsides), SUMMON_NO_UNIQUES, TRUE, TRUE, TRUE);
         success = TRUE;
         break;
      }

      case   ACT_BANISH_MONSTERS:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			(void)banish_monsters( damroll( numdice, numsides));
         success = TRUE;
         break;
      }

      case   ACT_JOKER_CARD:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			switch (randint1(4))
			{
				case 1: i = SUMMON_BIZARRE1; break;
				case 2: i = SUMMON_BIZARRE2; break;
				case 3: i = SUMMON_BIZARRE4; break;
				case 4: i = SUMMON_BIZARRE5; break;
			}

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), i, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_SUMMON_SPIDERS:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_SPIDER, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_SUMMON_REPTILES:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_HYDRA, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_SUMMON_HOUNDS:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_HOUND, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_TRUMP_BRANDING:
      {
			brand_weapon(4);
         success = TRUE;
         break;
      }

      case   ACT_LIVING_TRUMP:
      {
			if (one_in_(8))
				/* Teleport control */
				i = 12;
			else
				/* Random teleportation (uncontrolled) */
				i = 77;

			/* Gain the mutation */
			if (gain_mutation(i))
				msg_print("You have turned into a Living Trump.");
         break;
      }

      case   ACT_SUMMON_CYBERDEMON:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_CYBER, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_SUMMON_DRAGON:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_DRAGON, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_MASS_SUMMONING:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			for (i = 0; i < 3 + (plev / 10); i++)
			{
   			bool pet = one_in_(2);
	   		bool group = (pet ? FALSE : TRUE);


            (void)summon_specific(-1, py, px, damroll( numdice, numsides), SUMMON_NO_UNIQUES, group, FALSE, pet);
         }
         success = TRUE;
         break;
      }

      case   ACT_SUMMON_ANCIENT_DRAGON:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_HI_DRAGON, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_GREATER_UNDEAD:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

			if (summon_specific((pet ? -1 : 0), py, px, damroll( numdice, numsides), SUMMON_HI_UNDEAD, group, FALSE, pet))
			{
				if (!pet)
					msg_print("An angry creature appears!");
			}
         success = TRUE;
         break;
      }

      case   ACT_WIZARD_LOCK:
      {
   		if (!get_aim_dir(&dir)) return FALSE;

	   	(void)wizard_lock(dir);
         success = TRUE;
         break;
      }

      case   ACT_DETECT_INVISIBLE:
      {
   		(void)detect_monsters_invis();
         success = TRUE;
         break;
      }

      case   ACT_PHLOGISTON:
      {
   		phlogiston();
         success = TRUE;
         break;
      }

      case   ACT_DETECT_OBJECTS:
      {
   		(void)detect_objects_normal();
         success = TRUE;
         break;
      }

      case   ACT_RESIST_COLD:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_oppose_cold(p_ptr->oppose_cold + damroll(numdice, numsides)))
			{
				success = TRUE;
			}
         break;
      }

      case   ACT_RESIST_FIRE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_oppose_fire(p_ptr->oppose_fire + damroll(numdice, numsides)))
			{
				success = TRUE;
			}
         break;
      }

      case   ACT_RESIST_LIGHTNING:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_oppose_elec(p_ptr->oppose_elec + damroll(numdice, numsides)))
			{
				success = TRUE;
			}
         break;
      }

      case   ACT_RESIST_ACID:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			if (set_oppose_acid(p_ptr->oppose_acid + damroll(numdice, numsides)))
			{
				success = TRUE;
			}
         break;
      }

      case   ACT_ELEMENTAL_BALL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

   		if (!get_aim_dir(&dir)) return FALSE;

	   	switch (randint1(4))
		   {
   			case 1:  i = GF_FIRE; break;
	   		case 2:  i = GF_ELEC; break;
		   	case 3:  i = GF_COLD; break;
			   default: i = GF_ACID; break;
   		}
	   	success = fire_ball(i, dir, damroll( numdice, numsides), radius);
         break;
      }

      case   ACT_FORTIFICATION:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)set_afraid(0);
	   	(void)set_hero(p_ptr->hero + damroll( numdice, numsides));
		   (void)set_blessed(p_ptr->blessed + damroll( numdice, numsides));
   		(void)hp_player(damroll( numdice, numsides) / 4);
         success = TRUE;
         break;
      }

      case   ACT_MYSTIC_SENSING:
      {
   		(void)detect_monsters_normal();
	   	(void)detect_monsters_evil();
		   (void)detect_monsters_nonliving();
   		(void)detect_monsters_invis();
         success = TRUE;
         break;
      }

      case   ACT_OBJECT_SCRYING:
      {
   		(void)detect_treasure();
	   	(void)detect_objects_gold();
		   (void)detect_objects_magic();
   		(void)detect_objects_normal();
         success = TRUE;
         break;
      }

      case   ACT_GUIDING_LIGHT:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)lite_area(damroll( numdice, numsides), 5);
	   	(void)detect_traps();
		   (void)detect_doors();
   		(void)detect_stairs();
         success = TRUE;
         break;
      }

      case   ACT_GREATER_FORTIFICATION:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)set_poisoned(0);
	   	(void)set_stun(0);
		   (void)set_cut(0);
   		(void)set_afraid(0);
	   	(void)set_hero(p_ptr->hero + damroll( numdice, numsides));
		   (void)set_blessed(p_ptr->blessed + damroll( numdice, numsides));
   		(void)set_shield(p_ptr->shield + damroll( numdice, numsides));
	   	(void)set_oppose_acid(p_ptr->oppose_acid + damroll( numdice, numsides));
		   (void)set_oppose_elec(p_ptr->oppose_elec + damroll( numdice, numsides));
   		(void)set_oppose_fire(p_ptr->oppose_fire + damroll( numdice, numsides));
	   	(void)set_oppose_cold(p_ptr->oppose_cold + damroll( numdice, numsides));
		   (void)set_oppose_pois(p_ptr->oppose_pois + damroll( numdice, numsides));
   		(void)set_protevil(p_ptr->protevil + damroll( numdice, numsides));
	   	(void)hp_player(damroll( numdice, numsides) * 2);
         break;
      }

      case   ACT_MYSTIC_BOLT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		if (!get_aim_dir(&dir)) return FALSE;

   		switch (randint1(16))
   		{
   			case 1:  i = GF_ELEC; break;
   			case 2:  i = GF_POIS; break;
   			case 3:  i = GF_ACID; break;
   			case 4:  i = GF_COLD; break;
   			case 5:  i = GF_FIRE; break;
   			case 6:  i = GF_WATER; break;
   			case 7:  i = GF_LITE; break;
   			case 8:  i = GF_DARK; break;
   			case 9:  i = GF_CHAOS; break;
   			case 10: i = GF_FORCE; break;
   			case 11:  i = GF_SHARDS; break;
   			case 12:  i = GF_CHARM; break;
   			case 13:  i = GF_HOLY_FIRE; break;
   			case 14:  i = GF_HELL_FIRE; break;
   			case 15:  i = GF_DEATH_RAY; break;
   			default:  i = GF_AWAY_ALL; break;
   		}
         success = fire_bolt_or_beam(beam_pct, i, dir, damroll(numdice, numsides));
         break;
      }

      case   ACT_SPINNING_DEATH:
      {
		   int y = 0, x = 0, nt = 0;
			cave_type *c_ptr;
			monster_type *m_ptr;

         for (nt = 0; nt < 3; nt++)
         {
   			for (dir = 0; dir <= 9; dir++)
	   		{
		   		y = py + ddy[dir];
			   	x = px + ddx[dir];

				   /* paranoia */
   				if (!in_bounds2(y, x)) continue;
	   			c_ptr = area(y, x);

		   		/* Get the monster */
			   	m_ptr = &m_list[c_ptr->m_idx];

				   /* Hack -- attack monsters */
   				if (c_ptr->m_idx && (m_ptr->ml || cave_floor_grid(c_ptr)))
	   				py_attack(y, x);
            }
			}
         break;
      }

      case   ACT_MYSTIC_BALL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

   		if (!get_aim_dir(&dir)) return FALSE;

   		switch (randint1(16))
   		{
   			case 1:  i = GF_ELEC; break;
   			case 2:  i = GF_POIS; break;
   			case 3:  i = GF_ACID; break;
   			case 4:  i = GF_COLD; break;
   			case 5:  i = GF_FIRE; break;
   			case 6:  i = GF_WATER; break;
   			case 7:  i = GF_LITE; break;
   			case 8:  i = GF_DARK; break;
   			case 9:  i = GF_CHAOS; break;
   			case 10: i = GF_FORCE; break;
   			case 11:  i = GF_SHARDS; break;
   			case 12:  i = GF_CHARM; break;
   			case 13:  i = GF_HOLY_FIRE; break;
   			case 14:  i = GF_HELL_FIRE; break;
   			case 15:  i = GF_DEATH_RAY; break;
   			default:  i = GF_AWAY_ALL; break;
   		}
         success = fire_ball(i, dir, damroll(numdice, numsides), radius);
         break;
      }

      case   ACT_SUPREME_FORTIFICATION:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)dispel_monsters(damroll( numdice, numsides));
   		(void)set_poisoned(0);
   		(void)set_stun(0);
   		(void)set_cut(0);
   		(void)set_afraid(0);

         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

   		(void)set_hero(p_ptr->hero + damroll( numdice, numsides));
   		(void)set_blessed(p_ptr->blessed + damroll( numdice, numsides));
   		(void)set_shield(p_ptr->shield + damroll( numdice, numsides));
   		(void)set_oppose_acid(p_ptr->oppose_acid + damroll( numdice, numsides));
   		(void)set_oppose_elec(p_ptr->oppose_elec + damroll( numdice, numsides));
   		(void)set_oppose_fire(p_ptr->oppose_fire + damroll( numdice, numsides));
   		(void)set_oppose_cold(p_ptr->oppose_cold + damroll( numdice, numsides));
   		(void)set_oppose_pois(p_ptr->oppose_pois + damroll( numdice, numsides));
   		(void)set_protevil(p_ptr->protevil + damroll( numdice, numsides));
   		(void)hp_player(damroll( numdice, numsides));
   		(void)set_invuln(p_ptr->invuln + (damroll( numdice, numsides) / 4));
         success = TRUE;
         break;
      }

      case   ACT_MYSTIC_BURST:
      {
		   int y = 0, x = 0, nt = 0;
			cave_type *c_ptr;

  			for (dir = 0; dir <= 9; dir++)
   		{
            nt = 0;

      		switch (randint1(16))
		      {
      			case 1:  i = GF_ELEC; break;
		      	case 2:  i = GF_POIS; break;
      			case 3:  i = GF_ACID; break;
		      	case 4:  i = GF_COLD; break;
      			case 5:  i = GF_FIRE; break;
		      	case 6:  i = GF_WATER; break;
      			case 7:  i = GF_LITE; break;
		      	case 8:  i = GF_DARK; break;
      			case 9:  i = GF_CHAOS; break;
		      	case 10: i = GF_FORCE; break;
      			case 11:  i = GF_SHARDS; break;
		      	case 12:  i = GF_CHARM; break;
      			case 13:  i = GF_HOLY_FIRE; break;
		      	case 14:  i = GF_HELL_FIRE; break;
      			case 15:  i = GF_DEATH_RAY; break;
		      	default:  i = GF_AWAY_ALL; break;
      		}

	   		y = py + ddy[dir];
		   	x = px + ddx[dir];

			   /* paranoia */
  				if (!in_bounds2(y, x)) continue;
   			c_ptr = area(y, x);

            nt = rand_range( 1, 3);

            /*  Bolt  */
            if (nt == 1)
            {
               numsides = (BASE_BOLT_POWER * act_mult) / act_div;
               if (numsides < 1) numsides = 1;

               (void)fire_bolt_or_beam(33, i, dir, damroll(numdice, numsides));
            }
            /*  Beam  */
            if (nt == 2)
            {
               numsides = (BASE_BOLT_POWER * act_mult) / act_div;
               if (numsides < 1) numsides = 1;

               (void)fire_bolt_or_beam(100, i, dir, damroll(numdice, numsides));
            }
            /*  Ball  */
            if (nt == 3)
            {
               numsides = (BASE_BALL_POWER * act_mult) / act_div;
               if (numsides < 1) numsides = 1;

               radius = (numdice * numsides) / 75;
               if (radius > 7) radius = 7;
               if (radius < 2) radius = 2;

               (void)fire_ball(i, dir, damroll(numdice, numsides), radius);
            }
			}
         success = TRUE;
         break;
      }

      default:
      {
         if (act_num == 0) msg_print("Eeeep! ! !");
         else
            msg_print("Narf!!!!!!!!!!");
         success = FALSE;
         break;
      }
   }
   return (success);
}


/*
 * Magic Descriptors here
 *
 *
 */
cptr act_desc(int px, int py, int act_num, int s_lev, int s_fail,
              int s_base, int s_bonus, int act_mult, int act_div,
              int cast_penalty, int beam_pct)
{
   long ee;
   static char retbuf[80];
   bool success = FALSE;
   bool success2 = FALSE;
   int numdice = 0;
   int numsides = 0;
   int power = 0;
   int radius = 0;
   bool bad = FALSE;
   bool group = FALSE;
   int dir = 0;
   int chance = 0;
   int i = 0;
	int plev = p_ptr->lev;
   cptr temp;
   char temval[25];

   /*  Calculate base effect power  */
   if (s_lev)  /*  Has fixed value (rod/staff/wand/etc.)  */
      numdice = s_base + (s_lev / s_bonus);
      else
      numdice = s_base + s_bonus;

   /*  If penalized for multiple realms adjust for penalty  */
   if (cast_penalty)
      numdice = act_penalty( numdice, cast_penalty);

   if (numdice < 1) numdice = 1;

   /*  Check for which spell effect to use  */
   switch(act_num)
   {
      case ACT_TELEPORT:
      {
         numsides = (BASE_TELEPORT_RANGE * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Teleport %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_STR:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore STR %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_INT:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore INT %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_WIS:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore WIS %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_DEX:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore DEX %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_CON:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore CON %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_CHR:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore CHR %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_STATS:
      {
         numsides = (1 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restore STATS %dd%d", numdice, numsides);
         break;
      }

      case ACT_RESTORE_MANA:
      {
         numsides = (BASE_RESTORE_MANA * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Restore Mana %dd%d", numdice, numsides);
         break;
      }

      case ACT_CURE_LW:
      {
         numsides = (BASE_CURE_LIGHT * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Cures Blindness, Lessens Cuts, Restores %dd%d HP", numdice, numsides);
         break;
      }

      case ACT_BA_FIRE:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Fire Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BA_COLD:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Cold Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BA_ACID:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Acid Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BA_ELEC:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Electric Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_ID_PLAIN:
      {
         strnfmt(retbuf, sizeof(retbuf), "Identify");
         break;
      }

      case ACT_ID_FULL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Identify Fully");
         break;
      }

      case ACT_CALL_CHAOS:
      {
         call_chaos();
         strnfmt(retbuf, sizeof(retbuf), "Call Chaos");
         break;
      }

      case ACT_REMOVE_CURSE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Remove Curse");
         break;
      }

      case ACT_LITE:
      {
         numsides = (BASE_LIGHT_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Light Area %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_SUMMON_ANIMAL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Summon Animal");
         break;
      }
      case ACT_SUMMON_PHANTOM:
      {
         strnfmt(retbuf, sizeof(retbuf), "Summon Phantom");
         break;
      }
      case ACT_SUMMON_ELEMENTAL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Summon Elemental");
         break;
      }
      case ACT_SUMMON_DEMON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Summon Demon");
         break;
      }
      case ACT_SUMMON_UNDEAD:
      {
         strnfmt(retbuf, sizeof(retbuf), "Summon Undead");
         break;
      }
      case ACT_PHASE_DOOR:
      {
         numsides = ((BASE_TELEPORT_RANGE / 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Phase Door %dd%d", numdice, numsides);
         break;
      }

      case ACT_TELEPORT_LEVEL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Teleport Level");
         break;
      }

      case ACT_BO_CONFUSE:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Confusion Bolt %dd%d dmg", numdice, numsides);
         break;
      }
      case ACT_MAGIC_MAP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Maps Area");
         break;
      }
      case ACT_STAR_REMOVE_CURSE:
      {
         strnfmt(retbuf, sizeof(retbuf), "*Remove Curse*");
         break;
      }
      case ACT_DETECT_TREASURE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Treasure");
         break;
      }
      case ACT_OBJECT_DETECT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Objects");
         break;
      }
      case ACT_TRAP_DETECT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Traps");
         break;
      }
      case ACT_DOOR_STAIR_DETECT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Doors and Stairs");
         break;
      }
      case ACT_ACQUIREMENT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Acquirement");
			break;
		}
		case ACT_STAR_ACQUIREMENT:
		{
         strnfmt(retbuf, sizeof(retbuf), "*Acquirement*");
			break;
      }
      case ACT_MASS_GENOCIDE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Mass Genocide");
			break;
      }
      case ACT_TRAP_CREATION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Create Traps");
			break;
      }
      case ACT_DEST_DOOR_TRAP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Destroy Doors and Traps");
			break;
      }
      case ACT_CREATE_ARTIFACT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Artifact Creation");
			break;
      }
      case ACT_RECHARGE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Recharge");
			break;
      }
      case ACT_GENOCIDE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Genocide");
         break;
      }

      case ACT_BA_DARKNESS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Darkness Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_PROT_EVIL:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Protection from Evil %dd%d", numdice, numsides);
			break;
      }

      case ACT_SATIATE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Satisfy Hunger");
			break;
      }

      case ACT_DISP_UNDEAD:
      {
         strnfmt(retbuf, sizeof(retbuf), "Dispel Undead");
			break;
      }

      case ACT_STAR_ENCHANT_WEAPON:
      {
         strnfmt(retbuf, sizeof(retbuf), "*Enchant Weapon*");
			break;
      }

      case ACT_CURSE_WEAPON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Curse Weapon");
			break;
      }

      case ACT_STAR_ENCHANT_ARMOR:
      {
         strnfmt(retbuf, sizeof(retbuf), "*Enchant Armor*");
			break;
      }

      case ACT_CURSE_ARMOR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Curse Armor");
			break;
      }

      case ACT_BLESS:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "Bless %dd%d", numdice, numsides);
			break;
      }

      case ACT_HOLY_CHANT:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "Bless %dd%d", numdice, numsides);
			break;
      }

      case ACT_HOLY_PRAYER:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "Bless %dd%d", numdice, numsides);
			break;
      }

      case ACT_RECALL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Recall");
			break;
      }

      case ACT_STAR_DESTRUCTION:
      {
         numsides = (BASE_LIGHT_POWER * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "Destruction %dd%d", numdice, numsides);
			break;
      }

      case ACT_CURING:
      {
         numsides = ((BASE_CURE_LIGHT * 5) * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "Cures Blindness, Cuts, Confusion, Stun, Restores %dd%d HP", numdice, numsides);
			break;
      }

      case ACT_INVUNERABILITY:
      {
         numsides = (BASE_CURE_LIGHT * act_mult) / act_div;

         if (numdice / 4 < 1) numdice = 1; else numdice = numdice / 4;
         if (numsides / 4 < 1) numsides = 1; else numsides = numsides / 4;

         if (numdice < 3) numdice = 3;
         if (numsides < 3) numsides = 3;
         strnfmt(retbuf, sizeof(retbuf), "Invuneratbility %dd%d", numdice, numsides);
			break;
      }

      case ACT_NEW_LIFE:
      {
         strnfmt(retbuf, sizeof(retbuf), "New Life");
			break;
      }

      case ACT_CURE_SW:
      {
         numsides = ((BASE_CURE_LIGHT * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Cures Blindness, Confusion, Cuts, Restores %dd%d HP", numdice, numsides);
         break;
      }

      case ACT_CURE_CW:
      {
         numsides = ((BASE_CURE_LIGHT * 4) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Cures Blindness, Confusion. Poison, Stun, Cuts, Restores %dd%d HP", numdice, numsides);
         break;
      }

      case ACT_HEALING:
      {
         numsides = ((BASE_CURE_LIGHT * 8) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Cures Blindness, Confusion. Poison, Stun, Cuts, Restores %dd%d HP", numdice, numsides);
			break;
      }

      case ACT_EXPERIENCE:
      {
         numsides = (BASE_EXP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

			ee = (damroll(numdice, numsides) + 10) * p_ptr->lev;
			if (ee > 100000L) ee = 100000L;
			msg_print("You feel more experienced.");
         strnfmt(retbuf, sizeof(retbuf), "Experience %d", (int)ee);
			break;
      }

      case ACT_SLEEP:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Paralyzation %dd%d", numdice, numsides);
			break;
      }

      case ACT_POISON:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Poison %dd%d", numdice, numsides);
			break;
      }

      case ACT_LOSE_MEMORY:
      {
         numsides = (BASE_EXP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

   		ee = (damroll(numdice, numsides) + 10) * p_ptr->lev;
			if (ee > 100000L) ee = 100000L;
			msg_print("You feel your memories fade.");
         strnfmt(retbuf, sizeof(retbuf), "Resotres Experience %ld", (long)ee);
			break;
      }

      case ACT_ENLIGHTENMENT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Enlightement");
			break;
      }

      case ACT_HEROISM:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Heroism %dd%d", numdice, numsides);
			break;
      }

      case ACT_BERSERK_STRENGTH:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Breserk Strength %dd%d", numdice, numsides);
			break;
      }

      case ACT_BOLDNESS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Boldness");
			break;
      }

      case ACT_RESTORE_LIFE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Restore Life");
			break;
      }

      case ACT_RES_FIRE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Resist Fire %dd%d", numdice, numsides);
			break;
      }

      case ACT_RES_COLD:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Resist Cold %dd%d", numdice, numsides);
			break;
      }

      case ACT_SLOW_POISON:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Reduce Poison %dd%d", numdice, numsides);
			break;
      }

      case ACT_NEUTRALIZE_POISON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Neutralise Poison");
      }

      case ACT_RESISTANCE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Resistance %dd%d", numdice, numsides);
			break;
      }

      case ACT_STAR_RESISTANCE:
      {
         numsides = ((BASE_TEMP_BOOST * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "*Resistance* %dd%d", numdice, numsides);
			break;
      }

      case ACT_BO_LIGHT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Light Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_TAME_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Tame Monster Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_COLD:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Cold Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_FIRE:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Fire Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_STONE_TO_MUD:
      {
         strnfmt(retbuf, sizeof(retbuf), "Stone to Mud");
			break;
      }

      case ACT_BO_POLYMORPH:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Polymorphing Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_HEAL_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Heal Monster Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_HASTE_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Haste Monster Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_SLOW_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Slow Monster Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BA_HEAL_MONSTER:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Heal Monster Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BA_HASTE_MONSTER:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Haste Monster Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BA_SLOW_MONSTER:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Slow Monster Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BO_CONFUSE_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Confusion Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_SLEEP_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Sleep Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_DRAIN_LIFE:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Vampiric Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_MAGIC_MISSILE:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Magic Missile %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_CLONE_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Clone Monster %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_SCARE_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Scare Monster %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_TELEPORT_OTHER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Teleport Other %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_DISARM:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Disarm %dd%d", numdice, numsides);
         break;
      }

      case ACT_BA_POIS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Poison Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BO_WONDER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Wonder Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BO_ACID:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Acid Bolt %dd%d", numdice, numsides);
         break;
      }

      case ACT_BA_DRAGON_FIRE:
      {
         numsides = ((BASE_BALL_POWER * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Dragon Fire %dd%d dmg - radius %d", numdice, numsides, radius);

         break;
      }

      case ACT_BA_DRAGON_ELEMENTAL:
      {
         numsides = ((BASE_BALL_POWER * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Elemetnal Dragon Breath %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_ANNIHILATION:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Annihilation %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_BA_ROCKETS:
      {
         numsides = ((BASE_BALL_POWER * 2) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Rockets %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case ACT_SUMMON_HOSTILE:
      {
         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         power = damroll( numdice, numsides);

         power /= (51 - p_ptr->lev);

         if (power < 2) power = 2;

         strnfmt(retbuf, sizeof(retbuf), "Summon Monsters Max Power %d", (numdice * numsides));
			break;
      }

      case ACT_STARLIGHT:
      {
			int num = damroll(5, 3);
			int y, x;
			int attempts;
         int k = 0;
			cave_type *c_ptr;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Starlight %dd%d", numdice, numsides);
			break;
      }

      case ACT_DISPELL_EVIL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Dispel Evil %dd%d", numdice, numsides);
			break;
      }

      case ACT_PROBING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Probing");
			break;
      }

      case ACT_STAR_PROBING:
      {
/*  NEED ADVANCE PROBING CODE  */
         strnfmt(retbuf, sizeof(retbuf), "*Probing*");
			break;
      }

      case ACT_POWER:
      {
         numsides = ((BASE_DISPEL_POWER * 3) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Power %dd%d", numdice, numsides);
			break;
      }

      case ACT_IDENT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Identify");
         break;
      }

      case ACT_HOLINESS:
      {
         numsides = ((BASE_DISPEL_POWER * 3) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Holiness %dd%d", numdice, numsides);
			break;
      }

      case ACT_HAVOC:
      {
         strnfmt(retbuf, sizeof(retbuf), "Havoc");
			break;
      }

      case ACT_FULL_DETECTION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detection");
			break;
      }

      case ACT_DEATH:
      {
         strnfmt(retbuf, sizeof(retbuf), "Death");
			break;
      }

      case ACT_RUINATION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Ruination");
			break;
      }

      case ACT_DETONATIONS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detonation");
			break;
      }

      case ACT_AUGMENTATION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Augmentation %dd%d", numdice, numsides);
			break;
      }

      case ACT_LIFE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Life");
			break;
      }

      case ACT_SELF_KNOWLEDGE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Self Knowledge");
			break;
      }

      case ACT_STAR_ENLIGHTENMENT:
      {
         strnfmt(retbuf, sizeof(retbuf), "*Enlightenment*");
			break;
      }

      case ACT_STAR_SELF_KNOWLEDGE:
      {
         strnfmt(retbuf, sizeof(retbuf), "*Self Knowledge*");
			break;
      }

      case ACT_KNOWLEDGE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Knowledge");
         break;
      }

      case ACT_BA_SHINING:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Shining Breath %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_BA_LAW:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Law Breath %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_BA_CONF:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Confusion Ball %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_BA_SOUND:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Sound Ball %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_BA_CHAOS:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         if (!get_aim_dir(&dir)) return FALSE;

         strnfmt(retbuf, sizeof(retbuf), "Chaos Ball %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_BA_BALANCE:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Balance Ball %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_BA_POWER:
      {
         numsides = ((BASE_BALL_POWER * 3) * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Power Ball %dd%d dmg - radius %d", numdice, numsides, radius);
			break;
      }

      case ACT_INC_STR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Strength %dd%d", numdice, numdice);
			break;
      }

      case ACT_INC_INT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Intelligence %dd%d", numdice, numdice);
			break;
      }

      case ACT_INC_WIS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Wisdom %dd%d", numdice, numdice);
			break;
      }

      case ACT_INC_DEX:
      {
         strnfmt(retbuf, sizeof(retbuf), "Dexterity %dd%d", numdice, numdice);
			break;
      }

      case ACT_INC_CON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Constitution %dd%d", numdice, numdice);
			break;
      }

      case ACT_INC_CHR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Charisma %dd%d", numdice, numdice);
			break;
      }

/*  TEMP VALUES NOT IMPLEMENTED YET!  */
      case ACT_STR_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Strength %dd%d", numdice, numdice);
         break;
      }

      case ACT_INT_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Intelligence %dd%d", numdice, numdice);
         break;
      }

      case ACT_WIS_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Wisdom %dd%d", numdice, numdice);
         break;
      }

      case ACT_DEX_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Dexterity %dd%d", numdice, numdice);
         break;
      }

      case ACT_CON_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Constitution %dd%d", numdice, numdice);
         break;
      }

      case ACT_CHR_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Charisma %dd%d", numdice, numdice);
         break;
      }

      case ACT_STR_SUST_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Sustain Strength %dd%d", numdice, numdice);
         break;
      }

      case ACT_INT_SUST_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Sustain Intelligence %dd%d", numdice, numdice);
         break;
      }

      case ACT_WIS_SUST_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Sustain Wisdom %dd%d", numdice, numdice);
         break;
      }

      case ACT_DEX_SUST_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Sustain Dexterity %dd%d", numdice, numdice);
         break;
      }

      case ACT_CON_SUST_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Sustain Constitution %dd%d", numdice, numdice);
         break;
      }

      case ACT_CHR_SUST_TEMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Sustain Charisma %dd%d", numdice, numdice);
         break;
      }

      case ACT_TEMP_STEALTH:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Stealth %dd%d", numdice, numdice);
         break;
      }

      case ACT_TEMP_SEARCH:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Searching %dd%d", numdice, numdice);
         break;
      }

      case ACT_TEMP_INFRA:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Infravision %dd%d", numdice, numdice);
         break;
      }

      case ACT_TEMP_SPEED:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temp Speed %dd%d", numdice, numdice);
         break;
      }

      case ACT_DEC_STR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Strength %d", s_lev);
			break;
      }

      case ACT_DEC_INT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Intelligence %d", s_lev);
			break;
      }

      case ACT_DEC_WIS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Wisdom %d", s_lev);
			break;
      }

      case ACT_DEC_DEX:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Dexterity %d", s_lev);
			break;
      }

      case ACT_DEC_CON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Constitution %d", s_lev);
			break;
      }

      case ACT_DEC_CHR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Charisma %d", s_lev);
			break;
      }

      case  ACT_TEMP_SLOW:
      {
         strnfmt(retbuf, sizeof(retbuf), "Decrease Speed %d", s_lev);
         break;
      }

      case  ACT_TEMP_STAR_RESIST:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary *Resistance* %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_SUSTAIN:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Sustain %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_STAR_SUSTAIN:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary *Sustain* %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_POISON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Poison %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_NETHER:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Nether %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_LIGHT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Light %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_DARK:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Dark %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_FEARLESS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Fear %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_CONFUSION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Confusion %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_CHAOS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Chaos %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_DISENCHANT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Disenchantment %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_BLINDNESS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Blindness %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_NEXUS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Nexus %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_SOUND:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Sound %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_RES_SHARDS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Shards %dd%d", numdice, numsides);
         break;
      }

      case  ACT_BLINDNESS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Blindness %dd%d", numdice, numsides);
         break;
      }

      case  ACT_FEAR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Fear %dd%d", numdice, numsides);
         break;
      }

      case  ACT_CONFUSION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Confusion %dd%d", numdice, numsides);
         break;
      }

      case  ACT_HALLUCINATION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Hallucination %dd%d", numdice, numsides);
         break;
      }

      case  ACT_DISEASE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Disease %dd%d", numdice, numsides);
         break;
      }

      case  ACT_PARALYSIS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Paralysis %dd%d", numdice, numsides);
         break;
      }

      case  ACT_AGGRAVATION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Aggravation %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_SEE_INVIS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary See Invisible %dd%d", numdice, numsides);
         break;
      }

      case  ACT_TEMP_TELEPATHY:
      {
         strnfmt(retbuf, sizeof(retbuf), "Temporary Telepathy %dd%d", numdice, numsides);
         break;
      }

      case ACT_REPAIR:
      {
         numsides = (BASE_REPAIR_POWER * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "Repair %dd%d", numdice, numsides);
         break;
      }

      case ACT_STAR_REPAIR:
      {
         numsides = (2 * BASE_REPAIR_POWER * act_mult) / act_div;
         strnfmt(retbuf, sizeof(retbuf), "*Repair* %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DETECT_EVIL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Evil");
         break;
      }

      case   ACT_REMOVE_FEAR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Remove Fear");
         break;
      }

      case   ACT_LIGHT:
      {
         numsides = (BASE_LIGHT_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Light %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_DETECT_TRAPS_DOORS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Doors, Traps and Stairs");
         break;
      }

      case   ACT_CURE_POISON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Cure Poison");
         break;
      }

      case   ACT_SEE_INIVISBLE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "See Invisible %dd%d", numdice, numsides);
         break;
      }

      case   ACT_HOLY_ORB:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

   		if (!get_aim_dir(&dir)) return FALSE;

         strnfmt(retbuf, sizeof(retbuf), "Holy Fire %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_PROTECTION_FROM_EVIL:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Protection From Evil %dd%d", numdice, numsides);
         break;
      }

      case   ACT_GLYPH_WARDING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Glyph of Warding");
         break;
      }

      case   ACT_EXORCISM:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Exorcism %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DISPELL_UNDEAD_AND_DEMONS:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Dispel Undead and Demons %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DAY_OF_THE_DOVE:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Mass Charming %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DISPEL_EVIL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Dispel Evil %dd%d", numdice, numsides);
         break;
      }

      case   ACT_BANISH_EVIL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Banish Evil %dd%d", numdice, numsides);
         break;
      }

      case   ACT_HOLY_WORD:
      {
         strnfmt(retbuf, sizeof(retbuf), "Holy Word");
         break;
      }

      case   ACT_WARDING_TRUE:
      {
         strnfmt(retbuf, sizeof(retbuf), "True Warding");
         break;
      }

      case   ACT_PRAYER:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Prayer %dd%d", numdice, numsides);
         break;
      }

      case   ACT_BLESS_WEAPON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Bless Weapon");
         break;
      }

      case   ACT_RESTORATION:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Restoration %dd%d", numdice, numsides);
         break;
      }

      case   ACT_HEALING_TRUE:
      {
         numsides = ((BASE_CURE_LIGHT * 16) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Healing %dd%d", numdice, numsides);
         break;
      }

      case   ACT_HOLY_VISION:
      {
/*  Should change this to something else!!!  */
         strnfmt(retbuf, sizeof(retbuf), "*Identify*");
         break;
      }

      case   ACT_DIVINE_INTERVENTION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Divine Intervention");
         break;
      }

      case   ACT_HOLY_INVUNERABILITY:
      {
         numsides = (BASE_INVUNERABLE_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Invunerability %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DETECT_MONSTERS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Monsters");
         break;
      }

      case   ACT_CONFUSE_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Confusion Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SLEEP_MONSTER:
      {
         strnfmt(retbuf, sizeof(retbuf), "Sleep Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_IDENTIFY:
      {
         strnfmt(retbuf, sizeof(retbuf), "Identify");
         break;
      }

      case   ACT_SLOW_MONSTER:
      {
         strnfmt(retbuf, sizeof(retbuf), "Slow Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_MASS_SLEEP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Mass Sleep");
         break;
      }

      case   ACT_TELEPORT_AWAY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Teleportation Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SPEED:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         strnfmt(retbuf, sizeof(retbuf), "Speed %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DETECT_ALL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect All");
         break;
      }

      case   ACT_STAR_IDENTIFICATION:
      {
         strnfmt(retbuf, sizeof(retbuf), "*Identify*");
         break;
      }

      case   ACT_DETECT_OBJECTS_AND_TREASURE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Objects and Treasure");
         break;
      }

      case   ACT_DETECT_ENCHANTMENT:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Enchantment");
         break;
      }

      case   ACT_CHARM_MONSTER:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Charm Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DIMENSION_DOOR:
      {
         strnfmt(retbuf, sizeof(retbuf), "Dimensional Door");
         break;
      }

      case   ACT_STASIS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Stasis");
         break;
      }

      case   ACT_TELEKINESIS:
      {
         numsides = (BASE_BOLT_POWER * 3 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Telekinesis Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_EXPLOSIVE_RUNE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Explosive Rune");
         break;
      }

      case   ACT_CLAIRVOYANCE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Clairvoyance");
         break;
      }

      case   ACT_SORCERY_ENCHANT_WEAPON:
      {
         numsides = (BASE_ENCHANT_VALUE * act_mult / 2) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Enchant Weapon %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SORCERY_ENCHANT_ARMOR:
      {
         numsides = (BASE_ENCHANT_VALUE * act_mult / 2) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Enchant Armor %dd%d", numdice, numsides);
         break;
      }

      case   ACT_ALCHEMY:
      {
         strnfmt(retbuf, sizeof(retbuf), "Alchemy");
         break;
      }

      case   ACT_GLOBE_INVUNERABILTY:
      {
         numsides = (BASE_INVUNERABLE_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Invunerability %dd%d", numdice, numsides);
         break;
      }

      case   ACT_CHARM_ANIMAL:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Charm Animal %dd%d", numdice, numsides);
         break;
      }

      case   ACT_MINOR_RESISTANCE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Minor Resistance %dd%d", numdice, numsides);
         break;
      }

      case   ACT_LIGHTNING_BOLT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Lightning Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_FROST_BOLT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Frost Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_LIGHT_BEAM:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Light Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_ENTANGLE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Slow monsters");
         break;
      }

      case   ACT_HERBAL_HEALING:
      {
         numsides = ((BASE_CURE_LIGHT * 8) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Heals %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DOOR_BUILDING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Door Creation");
         break;
      }

      case   ACT_CREATE_STAIRS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Stair Creation");
         break;
      }

      case   ACT_STONE_SKIN:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Stone Skin %dd%d", numdice, numsides);
         break;
      }

      case   ACT_RESISTANCE_TRUE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Resistance %dd%d", numdice, numsides);
         break;
      }

      case   ACT_ANIMAL_FRIENDSHIP:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Charm Animals %dd%d", numdice, numsides);
         break;
      }

      case   ACT_WALL_OF_STONE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Wall Creation");
         break;
      }

      case   ACT_PROTECTION_FROM_CORROSION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Rustproof");
         break;
      }

      case   ACT_EARTHQUAKE:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 5;

         if (radius > 20) radius = 20;
         if (radius < 6) radius = 6;

         strnfmt(retbuf, sizeof(retbuf), "Earthquake %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_WHIRLWIND:
      {
         strnfmt(retbuf, sizeof(retbuf), "Whirlwind Attack");

         break;
      }

      case   ACT_BLIZZARD:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Cold Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_LIGHTNING_STORM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Lightning Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_WHIRLPOOL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Water Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_CALL_SUNLIGHT:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Ball of Light %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_ELEMENTAL_BRAND:
      {
   		brand_weapon(0);
         strnfmt(retbuf, sizeof(retbuf), "Elemental Branding");
         break;
      }

      case   ACT_NATURES_WRATH:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
   		(void)dispel_monsters(damroll( numdice, numsides));
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 5;

         if (radius > 20) radius = 20;
         if (radius < 6) radius = 6;

         numsides = ((BASE_BALL_POWER * 7) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 10;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Earthquake and Disintegraion Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_MAGIC_MISSILE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Mana Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_TRAP_DOOR_DESTRUCTION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Trap & Door Destruction");
         break;
      }

      case   ACT_TOUCH_CONFUSION:
      {
         strnfmt(retbuf, sizeof(retbuf), "Monster Confusion");
         break;
      }

      case   ACT_MANA_BURST:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Mana Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_FIRE_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Fire Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_FIST_OF_FORCE:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Disintegration Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_WONDER:
      {
         strnfmt(retbuf, sizeof(retbuf), "Wonder");
         break;
      }

      case   ACT_CHAOS_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Chaos Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SONIC_BOOM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 20;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Sound Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_DOOM_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Doom Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_FIRE_BALL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Fire Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_DESTRUCTION:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 5;

         if (radius > 20) radius = 20;
         if (radius < 6) radius = 6;

         strnfmt(retbuf, sizeof(retbuf), "Destruction - radius %d", radius);
         break;
      }

      case   ACT_INVOKE_LOGRUS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Chaos Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_POLYMORPH_OTHER:
      {
         strnfmt(retbuf, sizeof(retbuf), "Polymorph");
         break;
      }

      case   ACT_CHAIN_LIGHTNING:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Lightning Beams %dd%d", numdice, numsides);
         break;
      }

      case   ACT_DISINTEGRATION:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Disintegration Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_ALTER_REALITY:
      {
         strnfmt(retbuf, sizeof(retbuf), "Alter Reality");
         break;
      }

      case   ACT_POLYMORPH_SELF:
      {
         strnfmt(retbuf, sizeof(retbuf), "Polymorph Self");
         break;
      }

      case   ACT_CHAOS_BRANDING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Chaos Branding");
         break;
      }

      case   ACT_BEAM_OF_GRAVITY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Gravity Beam %dd%d", numdice, numsides);
         break;
      }

      case   ACT_METEOR_SWARM:
      {
         strnfmt(retbuf, sizeof(retbuf), "Meteor Swarm");
         break;
      }

      case   ACT_FIRE_STRIKE:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 50;

         if (radius > 9) radius = 9;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Fire Strike %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_MAGIC_ROCKET:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 50;

         if (radius > 9) radius = 9;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Rocket %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_MANA_STORM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 50;

         if (radius > 9) radius = 9;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Mana Storm %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_BREATH_LOGRUS:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Breath Logrus Storm %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_CALL_THE_VOID:
      {
         strnfmt(retbuf, sizeof(retbuf), "Call the Void");
         break;
      }

      case   ACT_DETECT_UNLIFE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Non-Living");
         break;
      }

      case   ACT_MALEDICTION:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Malediction %dd%d", numdice, numsides);
         break;
      }

      case   ACT_STINKING_CLOUD:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Poison Cloud %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_RESIST_POISON:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Poison %dd%d", numdice, numsides);
         break;
      }

      case   ACT_HORRIFY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Horrify %dd%d", numdice, numsides);
         break;
      }

      case   ACT_ENSLAVE_UNDEAD:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Enslave Undead %dd%d", numdice, numsides);
         break;
      }

      case   ACT_ORB_OF_ENTROPY:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Orb of Entropy %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_NETHER_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Nether Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_TERROR:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Terror %dd%d", numdice, numsides);
         break;
      }

      case   ACT_VAMPIRIC_DRAIN:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Vampiric Drain %dd%d", numdice, numsides);
         break;
      }

      case   ACT_POISON_BRAND:
      {
         strnfmt(retbuf, sizeof(retbuf), "Poison Branding");
         break;
      }

      case   ACT_DISPEL_GOOD:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Dispell Good %dd%d", numdice, numsides);
         break;
      }

      case   ACT_BERSERK:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Berzerk");
         break;
      }

      case   ACT_INVOKE_SPIRITS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Invoke Spirits");
         break;
      }

      case   ACT_DARK_BOLT:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Dark Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_BATTLE_FRENZY:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Battle Frenzy");
         break;
      }

      case   ACT_VAMPIRISM_TRUE:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Vampirism %dd%d", numdice, numsides);
         break;
      }

      case   ACT_VAMPIRIC_BRANDING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Vampiric Branding");
         break;
      }

      case   ACT_DARKNESS_STORM:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Darkness Storm %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_DEATH_RAY:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Death Ray %dd%d", numdice, numsides);
         break;
      }

      case   ACT_RAISE_DEAD:
      {
         strnfmt(retbuf, sizeof(retbuf), "Raise Dead");
         break;
      }

      case   ACT_WORD_OF_DEATH:
      {
         numsides = ((BASE_DISPEL_POWER) * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Word of Death %dd%d", numdice, numsides);
         break;
      }

      case   ACT_EVOCATION:
      {
         numsides = (BASE_DISPEL_POWER * 2 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Evocation %dd%d", numdice, numsides);
         break;
      }

      case   ACT_HELLFIRE:
      {
         numsides = (BASE_BALL_POWER * 5 * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Hell Fire %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_OMNICIDE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Omnicide");
         break;
      }

      case   ACT_WRAITHFORM:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Wraithform %dd%d", numdice, numsides);
         break;
      }

      case   ACT_MIND_BLAST:
      {
      	if (!get_aim_dir(&dir)) return FALSE;

         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Mind Blast %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SHUFFLE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Shuffle");
         break;
      }

      case   ACT_RESET_RECALL:
      {
         strnfmt(retbuf, sizeof(retbuf), "Reset Recall");
         break;
      }

      case   ACT_REACH:
      {
         numsides = (BASE_BOLT_POWER * 3 * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Reach %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SUMMON_MONSTER:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Summon Monster %dd%d", numdice, numsides);
         break;
      }

      case   ACT_BANISH_MONSTERS:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Banishment %dd%d", numdice, numsides);
         break;
      }

      case   ACT_JOKER_CARD:
      {
         strnfmt(retbuf, sizeof(retbuf), "Joker");
         break;
      }

      case   ACT_SUMMON_SPIDERS:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Spiders %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SUMMON_REPTILES:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Reptiles %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SUMMON_HOUNDS:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Hounds %dd%d", numdice, numsides);
         break;
      }

      case   ACT_TRUMP_BRANDING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Trump Branding");
         break;
      }

      case   ACT_LIVING_TRUMP:
      {
         strnfmt(retbuf, sizeof(retbuf), "Living Trump");
         break;
      }

      case   ACT_SUMMON_CYBERDEMON:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Cyberdemons %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SUMMON_DRAGON:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Dragons %dd%d", numdice, numsides);
         break;
      }

      case   ACT_MASS_SUMMONING:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Mass Summoning %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SUMMON_ANCIENT_DRAGON:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Ancient Dragons %dd%d", numdice, numsides);
         break;
      }

      case   ACT_GREATER_UNDEAD:
      {
			bool pet = one_in_(2);
			bool group = (pet ? FALSE : TRUE);

         numsides = (BASE_SUMMON_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Summon Greater Undead %dd%d", numdice, numsides);
         break;
      }

      case   ACT_WIZARD_LOCK:
      {
         strnfmt(retbuf, sizeof(retbuf), "Wizard Lock");
         break;
      }

      case   ACT_DETECT_INVISIBLE:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Invisible");
         break;
      }

      case   ACT_PHLOGISTON:
      {
         strnfmt(retbuf, sizeof(retbuf), "Phlogiston");
         break;
      }

      case   ACT_DETECT_OBJECTS:
      {
         strnfmt(retbuf, sizeof(retbuf), "Detect Objects");
         break;
      }

      case   ACT_RESIST_COLD:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Cold %dd%d", numdice, numsides);
         break;
      }

      case   ACT_RESIST_FIRE:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Fire %dd%d", numdice, numsides);
         break;
      }

      case   ACT_RESIST_LIGHTNING:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Electric %dd%d", numdice, numsides);
         break;
      }

      case   ACT_RESIST_ACID:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;

         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Temporary Resist Acid %dd%d", numdice, numsides);
         break;
      }

      case   ACT_ELEMENTAL_BALL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;

         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Elemental Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_FORTIFICATION:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;
         strnfmt(retbuf, sizeof(retbuf), "Fortification");
         break;
      }

      case   ACT_MYSTIC_SENSING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Mystic Sensing");
         break;
      }

      case   ACT_OBJECT_SCRYING:
      {
         strnfmt(retbuf, sizeof(retbuf), "Object Scrying");
         break;
      }

      case   ACT_GUIDING_LIGHT:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Guiding Light %dd%d", numdice, numsides);
         break;
      }

      case   ACT_GREATER_FORTIFICATION:
      {
         numsides = (BASE_TEMP_BOOST * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Greater Fortification");
         break;
      }

      case   ACT_MYSTIC_BOLT:
      {
         numsides = (BASE_BOLT_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Mystic Bolt %dd%d", numdice, numsides);
         break;
      }

      case   ACT_SPINNING_DEATH:
      {
         strnfmt(retbuf, sizeof(retbuf), "Spinning Death");
         break;
      }

      case   ACT_MYSTIC_BALL:
      {
         numsides = (BASE_BALL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         radius = (numdice * numsides) / 75;

         if (radius > 7) radius = 7;
         if (radius < 2) radius = 2;

         strnfmt(retbuf, sizeof(retbuf), "Mystic Ball %dd%d dmg - radius %d", numdice, numsides, radius);
         break;
      }

      case   ACT_SUPREME_FORTIFICATION:
      {
         numsides = (BASE_DISPEL_POWER * act_mult) / act_div;
         if (numsides < 1) numsides = 1;

         strnfmt(retbuf, sizeof(retbuf), "Supreme Fortification");
         break;
      }

      case   ACT_MYSTIC_BURST:
      {
         strnfmt(retbuf, sizeof(retbuf), "Mystic Burst");
         break;
      }

      default:
      {
         if (act_num == 0) strnfmt(retbuf, sizeof(retbuf), "No Activation");
         else
         strnfmt(retbuf, sizeof(retbuf), "No Activation");
         success = FALSE;
         break;
      }
   }
   return (retbuf);
}

