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

int act_penalty( int dice, int penalty)
{
   int   i;

   i = (dice * (100 - penalty)) / 100;

   return (i);
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

   /*  Calculate base effect power  */
   if (s_lev)  /*  Has fixed value (rod/staff/wand/etc.)  */
      numdice = s_base + (s_lev / s_bonus);
      else
      numdice = s_base + (p_ptr->lev / s_bonus);

   /*  If penalized for multiple realms adjust for penalty  */
   if (cast_penalty)
      numdice = act_penalty( numdice, cast_penalty);

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

         power /= (50 - p_ptr->lev);

         if (power < 2) power = 2;

			for (radius = 0; radius < power; radius++)
			{
				if (summon_specific(0, py, px, p_ptr->depth, 0, TRUE, FALSE, FALSE))
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
         numsides = (BASE_FLOOD_POWER * act_mult) / act_div;
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
         numsides = ((BASE_FLOOD_POWER * 3) * act_mult) / act_div;
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
         numsides = ((BASE_FLOOD_POWER * 3) * act_mult) / act_div;
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
			if (do_inc_stat(A_STR, 1)) success = TRUE;
			if (do_inc_stat(A_INT, 1)) success = TRUE;
			if (do_inc_stat(A_WIS, 1)) success = TRUE;
			if (do_inc_stat(A_DEX, 1)) success = TRUE;
			if (do_inc_stat(A_CON, 1)) success = TRUE;
			if (do_inc_stat(A_CHR, 1)) success = TRUE;
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
			(void)fire_ball(((chance == 1) ? GF_CHAOS :
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
			if (do_inc_stat(A_STR, s_lev)) success = TRUE;
			break;
      }

      case ACT_INC_INT:
      {
			if (do_inc_stat(A_INT, s_lev)) success = TRUE;
			break;
      }

      case ACT_INC_WIS:
      {
			if (do_inc_stat(A_WIS, s_lev)) success = TRUE;
			break;
      }

      case ACT_INC_DEX:
      {
			if (do_inc_stat(A_DEX, s_lev)) success = TRUE;
			break;
      }

      case ACT_INC_CON:
      {
			if (do_inc_stat(A_CON, s_lev)) success = TRUE;
			break;
      }

      case ACT_INC_CHR:
      {
			if (do_inc_stat(A_CHR, s_lev)) success = TRUE;
			break;
      }

/*  TEMP VALUES NOT IMPLEMENTED YET!  */
      case ACT_STR_TEMP:
      {
         break;
      }

      case ACT_INT_TEMP:
      {
         break;
      }

      case ACT_WIS_TEMP:
      {
         break;
      }

      case ACT_DEX_TEMP:
      {
         break;
      }

      case ACT_CON_TEMP:
      {
         break;
      }

      case ACT_CHR_TEMP:
      {
         break;
      }

      case ACT_STR_SUST_TEMP:
      {
         break;
      }

      case ACT_INT_SUST_TEMP:
      {
         break;
      }

      case ACT_WIS_SUST_TEMP:
      {
         break;
      }

      case ACT_DEX_SUST_TEMP:
      {
         break;
      }

      case ACT_CON_SUST_TEMP:
      {
         break;
      }

      case ACT_CHR_SUST_TEMP:
      {
         break;
      }

      case ACT_TEMP_STEALTH:
      {
         break;
      }

      case ACT_TEMP_SEARCH:
      {
         break;
      }

      case ACT_TEMP_INFRA:
      {
         break;
      }

      case ACT_TEMP_SPEED:
      {
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
         break;
      }

      case  ACT_TEMP_STAR_RESIST:
      {
         break;
      }

      case  ACT_TEMP_SUSTAIN:
      {
         break;
      }

      case  ACT_TEMP_STAR_SUSTAIN:
      {
         break;
      }

      case  ACT_TEMP_RES_POISON:
      {
         break;
      }

      case  ACT_TEMP_RES_NETHER:
      {
         break;
      }

      case  ACT_TEMP_RES_LIGHT:
      {
         break;
      }

      case  ACT_TEMP_RES_DARK:
      {
         break;
      }

      case  ACT_TEMP_FEARLESS:
      {
         break;
      }

      case  ACT_TEMP_RES_CONFUSION:
      {
         break;
      }

      case  ACT_TEMP_RES_CHAOS:
      {
         break;
      }

      case  ACT_TEMP_RES_DISENCHANT:
      {
         break;
      }

      case  ACT_TEMP_RES_BLINDNESS:
      {
         break;
      }

      case  ACT_TEMP_RES_NEXUS:
      {
         break;
      }

      case  ACT_TEMP_RES_SOUND:
      {
         break;
      }

      case  ACT_TEMP_RES_SHARDS:
      {
         break;
      }

      case  ACT_BLINDNESS:
      {
         break;
      }

      case  ACT_FEAR:
      {
         break;
      }

      case  ACT_CONFUSION:
      {
         break;
      }

      case  ACT_HALLUCINATION:
      {
         break;
      }

      case  ACT_DISEASE:
      {
         break;
      }

      case  ACT_PARALYSIS:
      {
         break;
      }

      case  ACT_AGGRAVATION:
      {
         break;
      }

      case  ACT_TEMP_SEE_INVIS:
      {
         break;
      }

      case  ACT_TEMP_TELEPATHY:
      {
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

