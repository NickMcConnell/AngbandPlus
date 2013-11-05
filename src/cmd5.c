/* File: cmd5.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

#define INFO_OK 0
#define INFO_LEVEL1 1
#define INFO_LEVEL2 2
#define INFO_REQ1 3
#define INFO_REQ2 4
#define INFO_REQ_SPELL 5
#define INFO_REQ_SHAPE 6
#define INFO_REQ_WOLF 7
#define INFO_REQ_BEAR 8
#define INFO_REQ_SHIELD 9
#define INFO_REQ_2CLAWS 10
#define INFO_REQ_2WEAPONS 11
#define INFO_REQ_POLEARM 12
#define INFO_REQ_BOW 13
#define INFO_NOCLASS 14
#define INFO_UNKNOWN 15

/* Return value of a skill + bonus from equipment */
int skill_value(int skill)
{
     int i;
     object_type *o_ptr;
     int skill_bonus = 0;

     /* Check all wielded items */
     for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
     {
	  o_ptr = &inventory[i];

	  /* If it is a skill item */
	  if (o_ptr->xtra1 >= OBJECT_XTRA_TYPE_SKILL_FIRST && 
	      o_ptr->xtra1 < OBJECT_XTRA_TYPE_SKILL_MULTI)
	  {
	       /* Get actual skill */
	       if (o_ptr->xtra2 == skill)
	       {
		    /* Get bonus */
		    skill_bonus += o_ptr->pval;
	       }
	  }

	  /* If it is a multiple skill item */
	  if (o_ptr->xtra1 >= OBJECT_XTRA_TYPE_SKILL_MULTI && 
	      o_ptr->xtra1 <= OBJECT_XTRA_TYPE_SKILL_LAST)
	  {
	       switch (o_ptr->xtra1)
	       {
	       case OBJECT_SKILL_BARB_ALL:
		    if (skill >= CLASS_WARRIOR * SK_PER_CLASS &&
			skill < (CLASS_WARRIOR + 1) * SK_PER_CLASS)
			 skill_bonus += o_ptr->pval;
		    break; 
	       case OBJECT_SKILL_SORC_ALL:
		    if (skill >= CLASS_MAGE * SK_PER_CLASS &&
			skill < (CLASS_MAGE + 1) * SK_PER_CLASS)
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_SORC_ALL_ELEC:
		    if (skill == SORC_ZAP || skill == SORC_LIGHTNING ||
			skill == SORC_THUNDERSTORM) skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_SORC_ALL_FIRE:
		    if (skill == SORC_FIREBOLT || skill == SORC_FIREBALL ||
			skill == SORC_METEOR) skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_SORC_ALL_COLD:
		    if (skill == SORC_FROSTBOLT || skill == SORC_GLACIAL_SPIKE ||
			skill == SORC_BLIZZARD) skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_ASSI_ALL:
		    if (skill >= CLASS_ROGUE * SK_PER_CLASS &&
			skill < (CLASS_ROGUE + 1) * SK_PER_CLASS)
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_ARCH_ALL:
		    if (skill >= CLASS_RANGER * SK_PER_CLASS &&
			skill < (CLASS_RANGER + 1) * SK_PER_CLASS)
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_ARCH_ALL_POLEARM:
		    if (skill == ARCH_JAB) 
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_ARCH_ALL_BOW:
		    if (skill == ARCH_MAGIC_AMMO || skill == ARCH_CRITICAL)
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_PALA_ALL:
		    if (skill >= CLASS_PALADIN * SK_PER_CLASS &&
			skill < (CLASS_PALADIN + 1) * SK_PER_CLASS)
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_NECRO_ALL:
		    if (skill >= CLASS_NECRO * SK_PER_CLASS &&
			skill < (CLASS_NECRO + 1) * SK_PER_CLASS)
			 skill_bonus += o_ptr->pval;
		    break;
	       case OBJECT_SKILL_NECRO_ALL_DMG:
		    if (skill == NECRO_TEETH || skill == NECRO_FUMES ||
			skill == NECRO_BONE_SPEAR)
			 skill_bonus += o_ptr->pval;
		    break;
	       }
	  }
     }

     /* Return modified skill */
     return (p_ptr->skill[skill] + skill_bonus);
}

static void power_desc(char *p, int skill, bool study)
{
     /* Skill level to display */
     int slev = (study && (skill_value(skill) < SKILL_MAX) ? 
		 skill_value(skill) + 1 : skill_value(skill));

     /* Default */
     strcpy(p, "");

     /* Don't show info for unknown skills unless studying */
     if (!study && slev < 1) return;

     switch (skill)
     {
     case BARB_SWORD: case BARB_POLEARM: case BARB_HAFTED: case BARB_AXE:
	  sprintf(p, " hit +%d, dam +%d", slev * 2, slev); break;
     case BARB_DOUBLE: sprintf(p, " hit +%d, IAS +%d%%", slev, (slev * 15) / 2); break;
     case BARB_STUN: sprintf(p, " %d%% dmg, 1d%d+5 turns", 
			     40 + (slev * 3), slev * 2); break;
     case BARB_BERSERK: sprintf(p, " hit +%d, dam +%d, AC -%d, 1d%d+%d turns", 
				slev * 2, slev, slev * 2, (slev * 2) + 10, 
				(slev * 2) + 10); break;
     case BARB_SPEED: sprintf(p, " speed +%d", slev + (slev / 4)); break;
     case BARB_FRENZY: break;
     case SORC_WARMTH: break;
     case SORC_ZAP: sprintf(p, " dmg 2d%d", (slev * 3 * sorc_dmg_boost(SORC_ELEC_M)) / 100); break; 
     case SORC_FIREBOLT: sprintf(p, " dmg %dd9", (slev * sorc_dmg_boost(SORC_FIRE_M)) / 100); break;
     case SORC_FROSTBOLT: sprintf(p, " dmg %dd6", (slev * sorc_dmg_boost(SORC_COLD_M)) / 100); break;
     case SORC_TELE: sprintf(p, " range %d", slev * 5); break;
     case SORC_LIGHTNING: sprintf(p, " dmg 1d%d", (slev * 5 * sorc_dmg_boost(SORC_ELEC_M)) / 100); break;
     case SORC_FIREBALL: 
	  sprintf(p, " dmg %d, radius %d", ((40 + (slev * 4)) * sorc_dmg_boost(SORC_FIRE_M)) / 100, 
		  2 + (slev / 10)); break;
     case SORC_GLACIAL_SPIKE: 
	  sprintf(p, " dmg %d, radius %d", ((15 + (slev * 3 / 2)) * sorc_dmg_boost(SORC_COLD_M)) / 100, 
		  2 + (slev / 10)); break;
     case SORC_THUNDERSTORM: sprintf(p, " dmg 5d%d, 1d%d targets", 
				     (slev * 2 * sorc_dmg_boost(SORC_ELEC_M)) / 100,
				     (slev / 5) + 1); break;
     case SORC_BLIZZARD: sprintf(p, " %d%% chance of dmg 5d%d", (slev * 3) + 30, 
				 (slev * 2 * sorc_dmg_boost(SORC_COLD_M)) / 100); break;
     case SORC_METEOR: 
	  sprintf(p, " dmg 10d%d, radius 4", ((10 + slev) * sorc_dmg_boost(SORC_FIRE_M)) / 100); 
	  break;
     case SORC_COLD_M: case SORC_FIRE_M: case SORC_ELEC_M:
	  sprintf(p, " dmg +%d%%", (slev * 9) + 20); break;
     case DRUID_UNARMED: 
     {
	  int num_blow;
	  int level = (slev / 2) + 1;
	  int dex_index = (adj_dex_blow[p_ptr->stat_ind[A_DEX]]);
	  if (dex_index > 11) dex_index = 11;
	  num_blow = blows_table[level][dex_index];
	  if (num_blow > 6) num_blow = 6;
	  if (num_blow < 2) num_blow = 2;
	  if (inventory[INVEN_ARM].k_idx && inventory[INVEN_ARM].tval == TV_SHIELD)
	       num_blow /= 2;
	  sprintf(p, " %d blows of 1d%d", num_blow, unarmed_damage(slev));
	  break;
     }
     case DRUID_SUNRAY: sprintf(p, " dmg 1d%d, radius %d", slev * 5, (slev / 5) + 2); 
	  break;
     case DRUID_LYCANTHRO: sprintf(p, " %d turns, +%d%% hp", 40 + (slev * 23), slev * 5); break;
     case DRUID_WOLF:
	  sprintf(p, " dam +%d, speed +%d, IAS +%d%%", slev, 
		  1 + (slev * 7 / 10), slev * 5); break;
     case DRUID_BEAR: sprintf(p, " dam +%d, AC +%d", slev * 2, 2 + (slev * 7 / 5)); break;
     case DRUID_MAUL: sprintf(p, " dam +%d%%", 40 + (slev * 13)); break;
     case DRUID_RAGE: sprintf(p, " hit +%d, dam +%d, AC -%d, 1d%d+%d turns", 
			      slev * 2, slev, slev * 2, (slev * 4) + 20, 
			      (slev * 4) + 20); break;
     case DRUID_SHOCKWAVE: sprintf(p, " radius %d, %d damage", (slev / 4) + 5, slev * 5); break;
     case ASSI_CLAWS: sprintf(p, " hit +%d, dam +%d", slev * 2, slev); break;
     case ASSI_DODGE: sprintf(p, " AC %d", (slev * 5) / 2); break;
     case ASSI_TIGER: sprintf(p, " damage +%d/+%d/+%d", 
			      slev / 2, slev, (slev * 3) / 2); break;
     case ASSI_FM_KICK: sprintf(p, " up to %d kicks, dam +1d%d", (slev / 5) + 2,
				slev * 5); break;
     case ASSI_FM_CLAWS: sprintf(p, " hit +%d, IAS +%d%%", slev, (slev * 15) / 2); break;
     case ASSI_SPEED: sprintf(p, " speed +%d, IAS +%d%%, 1d%d+%d turns", 
			      (slev / 2) + 5, slev * 5, (slev * 2) + 10, (slev * 2) + 10); break;
     case ASSI_COBRA: sprintf(p, " life steal %d%%/%d%%/%d%%",
			      (slev + 5) * 2, (slev + 5) * 5, (slev + 5) * 8); break;
     case ASSI_ASSASINATE: sprintf(p, " %d%% chance", (slev * 9 / 2) + 9); break;
     case ARCH_JAB: sprintf(p, " up to %d jabs, dam +%d%%", (slev / 5) + 2,
				slev * 10); break;
     case ARCH_MAGIC_AMMO:
     {
	  object_type *o_ptr;
	  o_ptr = &inventory[INVEN_BOW];
	  if (o_ptr->k_idx)
	  {
	       int sides = 2;
	       if (o_ptr->sval == SV_SHORT_BOW || o_ptr->sval == SV_LONG_BOW) sides = 4;
	       if (o_ptr->sval == SV_LIGHT_XBOW || o_ptr->sval == SV_HEAVY_XBOW) sides = 5;
	       sprintf(p, " dmg (1d%d+%d)*%d", sides, o_ptr->to_d + slev, p_ptr->ammo_mult);
	  }		    
	  break;
     }
     case ARCH_CRITICAL: sprintf(p, " %d%% chance", (slev * 3) + 15); break;
     case PALA_HEROISM:
	  sprintf(p, " hit +%d, dam +%d", slev * 2, slev); break;
     case PALA_MIGHT: sprintf(p, " dam +%d%%", 10 + (slev * 9 / 2)); break;
     case PALA_SMITE: sprintf(p, " dmg 4d%d", slev * 5); break; 
     case PALA_PRAYER: sprintf(p, " %dhp per turn", slev); break;
     case PALA_SHIELD_BASH: 
     {
	  object_type *o_ptr = &inventory[INVEN_ARM];
	  if (o_ptr->k_idx && o_ptr->tval == TV_SHIELD)
	  {
	       int bash_quality = (p_ptr->skill_thn / 5) + (o_ptr->weight / 3);
	       int bash_mult = (bash_quality / 20) + ((skill_value(PALA_SHIELD_BASH) + 1) / 2);
	       sprintf(p, " %dd%d * %d", o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d), bash_mult);
	  }
	  break;
     }
     case PALA_HOLY_SHIELD: sprintf(p, " +%d%% shield AC, %d%% protection from evil", slev * 25, 
				    (slev * 2) + 10); break;
     case PALA_HOLY_LIGHT: sprintf(p, " 1d%d dmg, %d light radius", slev * 10, 
				   1 + (slev / 10)); break;
     case NECRO_TEETH: 
	  sprintf(p, " %d teeth, dmg 1d%d", ((slev * 3) / 20) + 2, slev * 3); break; 
     case NECRO_FUMES:
	  sprintf(p, " dmg %d, radius %d", 40 + (slev * 4), 2 + (slev / 5)); break;
     case NECRO_BONE_SPEAR: sprintf(p, " dmg 1d%d", slev * 5); break;
     case NECRO_MANA_LEECH: 
          sprintf(p, " %d%% of monster level", slev * 10); break;
     case NECRO_FEAR: sprintf(p, " %d%% chance", (slev * 2) + 10); break;
     case NECRO_SLOW: sprintf(p, " %d%% chance", slev + 5); break;
     }
}

/* Returns the number of skills printed */
static int print_skills(bool study)
{
     byte line_attr;
     int i, y = 1, x = 7;
     int letter = 0;

     /* Dispaly a list of skills */
     prt("", y, x);
     put_str("Name", y, x + 5);
     put_str("Level", y, x + 22);
     put_str("Mana", y, x + 28);

     /* Dump the skills */
     for (i = 0; i < MAX_SKILLS; i++)
     {
	  char skill_desc[80];
	  char info[40];
	  cptr desc = "";
	  bool show_info = INFO_OK;

	  /* Ignore non-existant skills */
	  if (skill_info[i].level > 50) continue;

	  line_attr = TERM_WHITE;

	  /* Studying skills */
	  if (study)
	  {
	       /* Can only study class skills */
	       if (i < (p_ptr->pclass * SK_PER_CLASS) || i >= ((p_ptr->pclass + 1) * SK_PER_CLASS))
	       {
		    line_attr = TERM_L_DARK;
		    show_info = INFO_NOCLASS;
		    continue;
	       }

	       /* Hack - prevent lycanthropy unless player has a shapeshift skill */
	       if (i == DRUID_LYCANTHRO)
	       {
		    if ((!skill_value(DRUID_WOLF)) && 
			(!skill_value(DRUID_BEAR))) {
			line_attr = TERM_RED; show_info = INFO_REQ_SHAPE; }
	       }

	       /* Hack - prevent warmth unless player has a mage skill that uses mana */
	       if (i == SORC_WARMTH)
	       {
		    /* Check each mage skill */
		    int j; bool spell_known = FALSE;
		    for (j = CLASS_MAGE * SK_PER_CLASS; j < (CLASS_MAGE + 1) * SK_PER_CLASS; j++)
			 if (((skill_info[j].mana_base > 0) ||
			      (skill_info[j].mana_plus > 0)) &&
			     (skill_value(j) > 0)) spell_known = TRUE;
		    if (!spell_known) { 
			 line_attr = TERM_RED; show_info = INFO_REQ_SPELL; }
	       }

	       /* Requirement 2 is set */
	       if (skill_info[i].req2 != 999)
	       {
		    /* Does not possess this skill */
		    if (p_ptr->skill[skill_info[i].req2] == 0) {
			 line_attr = TERM_RED; show_info = INFO_REQ2; }
	       }
	       /* Requirement 1 is set */
	       if (skill_info[i].req1 != 999)
	       {
		    /* Does not possess this skill */
		    if (p_ptr->skill[skill_info[i].req1] == 0) {
			 line_attr = TERM_RED; show_info = INFO_REQ1; }
	       }

	       /* High enough level */
	       if (p_ptr->skill[i] + skill_info[i].level - 1 >= p_ptr->lev) {
		    line_attr = TERM_RED; show_info = INFO_LEVEL2; }
	       if (p_ptr->lev < skill_info[i].level)  {
		    line_attr = TERM_RED; show_info = INFO_LEVEL1; }

	       /* Maxed skill, overrides all else */
	       if (skill_value(i) >= SKILL_MAX)
		    line_attr = TERM_L_RED;
	  }
	  else /* Using skills */
	  {
	       /* Known skill */
	       if (skill_value(i) > 0)
	       { 
		    int mana_cost = skill_info[i].mana_base + 
			 ((skill_info[i].mana_plus * skill_value(i)) / 10);

		    /* Barbarian skills that require 2 weapons */
		    if ((i == BARB_DOUBLE) &&
			(!inventory[INVEN_WIELD].k_idx ||
			 inventory[INVEN_WIELD].tval == TV_CLAW ||
			 !dual_wielding() ||
			 inventory[INVEN_ARM].tval == TV_CLAW)) {
			 line_attr = TERM_RED; show_info = INFO_REQ_2WEAPONS; }

		    /* Druid skills that require a certain shapeshift */
		    if (i == DRUID_RAGE && p_ptr->shapeshift != SHAPE_WOLF) {
			 line_attr = TERM_RED; show_info = INFO_REQ_WOLF; }
		    if ((i == DRUID_MAUL || i == DRUID_SHOCKWAVE) && 
			p_ptr->shapeshift != SHAPE_BEAR) {
			 line_attr = TERM_RED; show_info = INFO_REQ_BEAR; }

		    /* Paladin skills that require a shield */
		    if (((!inventory[INVEN_ARM].k_idx) || (inventory[INVEN_ARM].tval != TV_SHIELD)) &&
			(i == PALA_SHIELD_BASH || i == PALA_HOLY_SHIELD)) {
			 line_attr = TERM_RED; show_info = INFO_REQ_SHIELD; }

		    /* Assasin skills that require 2 claws */
		    if ((i == ASSI_FM_CLAWS) &&
			(!inventory[INVEN_WIELD].k_idx ||
			 inventory[INVEN_WIELD].tval != TV_CLAW ||
			 !dual_wielding() ||
			 inventory[INVEN_ARM].tval != TV_CLAW)) {
			 line_attr = TERM_RED; show_info = INFO_REQ_2CLAWS; }

		    /* Amazon skills that require a polearm */
		    if ((i == ARCH_JAB) &&
			(!inventory[INVEN_WIELD].k_idx ||
			 inventory[INVEN_WIELD].tval != TV_POLEARM)) {
			 line_attr = TERM_RED; show_info = INFO_REQ_POLEARM; }

		    /* Amazon skills that require a bow */
		    if ((!inventory[INVEN_BOW].k_idx) && (i == ARCH_MAGIC_AMMO)) {
			 line_attr = TERM_RED; show_info = INFO_REQ_BOW; }

		    /* Not enough mana */
		    if (mana_cost > p_ptr->csp)
			 line_attr = TERM_RED;

		    /* Passive */
		    if (skill_info[i].type == SKILL_PASSIVE)
			 line_attr = TERM_RED;

		    /* Highlight skill relating to active aura or shapeshift */
		    if ((p_ptr->aura && (i == aura_skill[p_ptr->aura])) ||
			(p_ptr->shapeshift && (i == shapeshift_skill[p_ptr->shapeshift])))
			 line_attr = TERM_L_GREEN;
	       }
	       else /* Unknown */
	       {
		    line_attr = TERM_L_DARK;
		    show_info = INFO_UNKNOWN;
		    continue; 
	       }
	  }

	  /* Get info for skill */
	  switch (show_info)
	  {
	  case INFO_OK:
	       power_desc(info, i, study);
	       desc = info;
	       break;
	  case INFO_LEVEL1: desc = format(" requires XP level %d", 
				 skill_info[i].level); break; 
	  case INFO_LEVEL2: desc = format(" requires XP level %d", 
				 p_ptr->skill[i] + skill_info[i].level); break;
	  case INFO_REQ1: desc = format(" requires %s", 
				 skill_info[skill_info[i].req1].name); break;
	  case INFO_REQ2: desc = format(" requires %s", 
			         skill_info[skill_info[i].req2].name); break;
	  case INFO_REQ_SPELL: desc = " must know a sorceror spell"; break;
	  case INFO_REQ_SHAPE: desc = " must know a shapeshift"; break;
	  case INFO_REQ_WOLF: desc = " must be in Werewolf form"; break;
	  case INFO_REQ_BEAR: desc = " must be in Werebear form"; break;
	  case INFO_REQ_SHIELD: desc = " must wear a shield"; break;
	  case INFO_REQ_2CLAWS: desc = " must wield 2 claws"; break;
	  case INFO_REQ_2WEAPONS: desc = " must wield 2 weapons"; break;
	  case INFO_REQ_POLEARM: desc = " must wield a polearm"; break;
	  case INFO_REQ_BOW: desc = " must wield a missile weapon"; break;
	  case INFO_NOCLASS: desc = " cannot study this skill"; break;
	  case INFO_UNKNOWN: desc = " unknown"; break;
	  }

	  /* Show potential studyable skills */
	  if (study && skill_value(i) < SKILL_MAX)
	  {
	       int mana_cost = skill_info[i].mana_base + 
		    ((skill_info[i].mana_plus * (skill_value(i) + 1)) / 10);

	       sprintf(skill_desc, "  %c) %-20s%2d  %3d %s", I2A(letter), 
		       skill_info[i].name, 
		       p_ptr->skill[i] + 1, mana_cost, desc);
	  }
	  else /* Show actual skill */
	  {
	       int mana_cost = skill_info[i].mana_base + 
		    ((skill_info[i].mana_plus * skill_value(i)) / 10);

	       /* Don't show mana for unuseable skills */
	       if (skill_value(i) < 1) mana_cost = 0;

	       sprintf(skill_desc, "  %c) %-20s%2d  %3d %s", I2A(letter), 
		       skill_info[i].name, 
		       skill_value(i), mana_cost, desc);
	  }
	  c_prt(line_attr, skill_desc, y + letter + 1, x);
	  letter++;
     }

     return letter;
}

static int get_skill(bool study)
{
     bool flag = FALSE;
     int i =  -1, ask;
     char choice;
     char out_val[160];
     int letters = 0;
     int skill = -1;

     /* Save the screen */
     Term_save();

     /* Display the skills */
     letters = print_skills(study);

     /* No skills to get */
     if (!letters)
     {
	  Term_load();
	  if (study)
	       msg_print("You cannot study any skills!");
	  else msg_print("You have no skills yet!");
	  return (-1);
     }

     /* Build a prompt (accept all spells) */
     strnfmt(out_val, 78, "(Skills %c-%c, ESC=exit) %s which skill? ",
	     I2A(0), I2A(letters - 1), (study ? "Study" : "Use"));
	
     /* Wait */
     while (!flag && get_com(out_val, &choice))
     {
	  /* Note verify */
	  ask = (isupper(choice));

	  /* Lowercase */
	  if (ask) choice = tolower(choice);

	  /* Extract request */
	  i = (islower(choice) ? A2I(choice) : -1);

	  /* Totally Illegal */
	  if ((i < 0) || (i >= letters))
	  {
	       bell("Illegal skill choice!");
	       continue;
	  }

	  /* Convert i to an actual skill index */
	  {
	  /* Get list of ok skills */
	  int ok_skills[MAX_SKILLS];
	  int num_ok_skills = 0;
	  int j;
	  for (j = 0; j < MAX_SKILLS; j++)
	  {
	       bool count = TRUE;

	       /* Ignore non-existant skills */
	       if (skill_info[j].level > 50) count = FALSE;

	       /* Ignored skills */
	       if (study)
	       {
		    /* Must be a class skill to study it */
		    if (j < (p_ptr->pclass * SK_PER_CLASS) || j >= ((p_ptr->pclass + 1) * SK_PER_CLASS)) 
			 count = FALSE;
	       }
	       else
	       {
		    /* Must be known to use it */
		    if (skill_value(j) < 1) count = FALSE;
	       }

	       /* Store the skill */
	       if (count)
	       {
		    ok_skills[num_ok_skills] = j;
		    num_ok_skills++;
	       }
	  }
	  
	  /* Convert */
	  skill = ok_skills[i];
	  }

	  /* Cannot choose */
	  if (study)
	  {
	       /* Cannot study non class skills */
	       if (skill < (p_ptr->pclass * SK_PER_CLASS) || skill >= ((p_ptr->pclass + 1) * SK_PER_CLASS)) {
		    bell("You cannot study this skill!"); continue; };
	       /* Cannot study maxed skills */
	       if (skill_value(skill) >= SKILL_MAX) { 
		    bell("You cannot study this skill further!"); continue; }
	       /* Need to be high enough XP level */
	       else if (p_ptr->lev < skill_info[skill].level) {
		    bell("You are not experienced enough to study this skill."); continue; }
	       /* Need to be high enough XP level */
	       else if (p_ptr->skill[skill] + skill_info[skill].level - 1 >= p_ptr->lev) {
		    bell("You are not experienced enough to study this skill further."); continue; }
	       /* Need to know pre-requisites */
	       else if ((skill_info[skill].req1 != 999 && 
		    p_ptr->skill[skill_info[skill].req1] == 0) ||
		   (skill_info[skill].req2 != 999 &&
		    p_ptr->skill[skill_info[skill].req2] == 0)) {
		    bell("This skill requires a skill you have not studied."); continue; }
	       /* Need to know a skill that uses mana to learn warmth */
	       else if (skill == SORC_WARMTH) {
		    int j; bool spell_known = FALSE;
		    /* Check each mage skill */
		    for (j = CLASS_MAGE * SK_PER_CLASS; j < (CLASS_MAGE + 1) * SK_PER_CLASS; j++) 
			 if (((skill_info[j].mana_base > 0) ||
			      (skill_info[j].mana_plus > 0)) &&
			     (skill_value(j) > 0)) spell_known = TRUE;
		    if (!spell_known) { 
			 bell("This skill requires a sorceror spell to be known."); continue; }
	       }
	       /* Prevent lycanthropy unless player has a shapeshift skill */
	       else if (skill == DRUID_LYCANTHRO &&
			(!skill_value(DRUID_WOLF)) && (!skill_value(DRUID_BEAR))) {
		    bell("This skill requires a shapeshifting skill to be known."); continue; }
	  }
	  else
	  {
	       int mana_cost = skill_info[skill].mana_base + 
		    ((skill_info[skill].mana_plus * skill_value(skill)) / 10);

	       if (skill_value(skill) == 0) {
		    bell("You don't know this skill!"); continue; }
	       /* Barbarian skills that require 2 weapons */
	       else if ((skill == BARB_DOUBLE) &&
			(!inventory[INVEN_WIELD].k_idx || inventory[INVEN_WIELD].tval == TV_CLAW ||
			 !dual_wielding() || inventory[INVEN_ARM].tval == TV_CLAW)) {
		    bell("You must be wielding 2 weapons to use this skill!"); continue; }
               /* Rage can only be used in wolf form */
	       else if (skill == DRUID_RAGE && p_ptr->shapeshift != SHAPE_WOLF) {
		    bell("You must be in Werewolf form to use this skill!"); continue; }
               /* Maul/Shockwave can only be used in bear form */
	       else if ((skill == DRUID_MAUL || skill == DRUID_SHOCKWAVE) && 
			p_ptr->shapeshift != SHAPE_BEAR) {
		    bell("You must be in Werebear form to use this skill!"); continue; }
               /* Paladin skills that require a shield */
	       else if ((skill == PALA_SHIELD_BASH || skill == PALA_HOLY_SHIELD) && 
			((!inventory[INVEN_ARM].k_idx) || (inventory[INVEN_ARM].tval != TV_SHIELD))) {
		    bell("You must be wearing a shield to use this skill!"); continue; }
	       /* Assasin skills that require 2 claws */
	       else if ((skill == ASSI_FM_CLAWS) &&
			(!inventory[INVEN_WIELD].k_idx || inventory[INVEN_WIELD].tval != TV_CLAW ||
			 !dual_wielding() || inventory[INVEN_ARM].tval != TV_CLAW)) {
		    bell("You must be wielding 2 claws to use this skill!"); continue; }
	       /* Amazon skills that require a polearm */
	       else if ((skill == ARCH_JAB) &&
			(!inventory[INVEN_WIELD].k_idx || inventory[INVEN_WIELD].tval != TV_POLEARM)) {
		    bell("You must be wielding a polearm to use this skill!"); continue; }
	       /* Amazon skills that require a bow */
	       else if ((skill == ARCH_MAGIC_AMMO) && (!inventory[INVEN_BOW].k_idx)) {
		    bell("You must be wielding a bow to use this skill!"); continue; }
	       else if (p_ptr->shapeshift && i == shapeshift_skill[p_ptr->shapeshift]) {
		    bell("This skill is already in use!"); continue; }
	       else if (skill_info[skill].type == SKILL_PASSIVE) {
		    bell("This skill is constant, and cannot be used here."); continue; }
	       /* Don't require mana for deactivating auras */
	       else if (mana_cost > p_ptr->csp && skill != aura_skill[p_ptr->aura]) {
		    bell("You don't have enough mana!"); continue; }
	  }

	  /* If player has an aura active and selected skill is an aura, verify using */
	  if (p_ptr->aura && skill_info[skill].type == SKILL_AURA && !study)
	  {
	       char tmp_val[160];
		    
	       /* Prompt */
	       strnfmt(tmp_val, 78, "Stop using %s? ", 
		       skill_info[aura_skill[p_ptr->aura]].name);
	       
	       /* Belay that order */
	       if (!get_check(tmp_val)) continue;
	  }
	  /* If player is shapeshifted and selected skill is a shapeshift, verify using */
	  else if (p_ptr->shapeshift && skill_info[skill].type == SKILL_SHAPESHIFT && !study)
	  {
	       char tmp_val[160];
		    
	       /* Prompt */
	       strnfmt(tmp_val, 78, "Stop using %s? ", 
		       skill_info[shapeshift_skill[p_ptr->shapeshift]].name);
	       
	       /* Belay that order */
	       if (!get_check(tmp_val)) continue;
	  }
	  /* Verify it */
	  else if (ask)
	  {
	       char tmp_val[160];
	       
	       /* Prompt */
	       strnfmt(tmp_val, 78, "Use %s? ", skill_info[skill].name);

	       /* Belay that order */
	       if (!get_check(tmp_val)) continue;
	  }
	  
	  /* Stop the loop */
	  flag = TRUE;
     }

     /* Restore the screen */
     Term_load();     

     /* Abort if needed */
     if (!flag) return (-1);

     /* Success */
     return (skill);
}

void do_cmd_study_skill()
{
     /* Needs skill points */
     if (p_ptr->skill_points > 0)
     {
	  /* Study skill */
	  p_ptr->skill[get_skill(TRUE)]++;

	  /* Recalculate bonuses */
	  p_ptr->update |= (PU_BONUS | PU_SKILLS);

	  /* Redraw Study Status */
	  p_ptr->redraw |= (PR_STUDY);

	  /* Handle stuff */
	  handle_stuff();

	  /* Take a turn */
	  p_ptr->energy_use = 100;
     }
     else 
     {
	  /* Display details for next level */
	  Term_save();
	  print_skills(TRUE);
	  prt("You have no skill points to spend!", 0, 0);
	  (void)inkey();
	  Term_load();
     }
}

void use_skill(int skill)
{
     int dir;

     /* Was a skill used ? */
     bool done = 0;

     /* Check mana cost, if any */
     int mana_cost = skill_info[skill].mana_base + 
	  ((skill_info[skill].mana_plus * skill_value(skill)) / 10);

     /* If valid skill chosen, and enough mana to use it, unless deactivating an aura */
     if ((skill != -1) && (mana_cost <= p_ptr->csp || skill == aura_skill[p_ptr->aura]))
     {
	  switch (skill)
	  {
	  case BARB_DOUBLE:
	  {
	       /* Count blows used */
	       int blows = 0;
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       char m_name[80];
	       object_type *o_ptr;
	       bool fear = FALSE;
	       bool do_quake = FALSE;

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    int i, bonus, k = 0;

		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    /* Attempt to hit twice */
		    for (i = 0; i < 2; i++)
		    {
			 /* Get weapon */
			 o_ptr = &inventory[INVEN_WIELD];
			 /* Second blow uses second weapon */
			 if (i) o_ptr = &inventory[INVEN_ARM];
			 
			 /* To-hit bonuses */
			 bonus = p_ptr->to_h;
			 bonus += skill_value(BARB_DOUBLE);
			 if (o_ptr->k_idx) bonus += o_ptr->to_h;

			 /* Check for successful hit */
			 if (test_hit_norm(p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
			 {
			      char o_name[80];
			      char hit_desc[10] = "hit";

			      /* Handle normal weapon */
			      k = damroll(o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));
			      k = tot_dam_aux(o_ptr, k, m_ptr);
			      if (p_ptr->impact && (k > 50)) do_quake = TRUE;
			      k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			      k = assasination(r_ptr, k); /* default is to return unchanged */
			      
			      /* Apply the player damage bonuses */
			      k += p_ptr->to_d + weapon_mastery(!i) + might_bonus(k) + 
				   tiger_strike(k, hit_desc);

			      /* Add to charges */
			      charge_ups(TRUE);

			      /* No negative damage */
			      if (k < 0) k = 0;

			      /* Assasin's healing */
			      life_steal(k, hit_desc);

			      /* Message */
			      object_desc(o_name, o_ptr, FALSE, 0);
			      message_format(MSG_HIT, m_ptr->r_idx, "You %s %s with your %s.",
					     hit_desc, m_name, o_name);

			      /* Damage, check for fear and death */
			      if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) 
			      {
				   blows++; break;
			      }
			 }
			 else /* Missed */
			 {
			      message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			 }

			 /* Count attacks */
			 blows++;
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }

	       /* IAS */
	       done = 10000 / (100 + ias_bonus() + (skill_value(BARB_DOUBLE) * 15 / 2));
	       
	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       /* Mega-Hack -- apply earthquake brand */
	       if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, FALSE, 85);
	       break;
	  }
	  case BARB_STUN:
	  {
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       char m_name[80];
	       bool fear = FALSE;
	       bool do_quake = FALSE;

	       /* Get weapon */   
	       object_type *o_ptr = &inventory[INVEN_WIELD];
	       if (!o_ptr->k_idx && dual_wielding()) o_ptr = &inventory[INVEN_ARM];

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    int bonus, k = 0;

		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    bonus = p_ptr->to_h;
		    if (o_ptr->k_idx) bonus += o_ptr->to_h;

		    /* Check for successful hit */
		    if (test_hit_norm(p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
		    {
			 char hit_desc[10] = "unused";

			 /* Handle normal weapon */
			 if (o_ptr->k_idx)
			 {
			      k = damroll(o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));
			      k = tot_dam_aux(o_ptr, k, m_ptr);
			      if (p_ptr->impact && (k > 50)) do_quake = TRUE;
			      k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			      k = assasination(r_ptr, k); /* default is to return unchanged */
			 }
			 else if (skill_value(DRUID_UNARMED) &&
				  !inventory[INVEN_WIELD].k_idx &&
				  !dual_wielding())
			 {
			     /* Unarmed attacks */
			     k = damroll(1, unarmed_damage(skill_value(DRUID_UNARMED)));
			     k = critical_norm(skill_value(DRUID_UNARMED) * 5, p_ptr->to_h, k);
			 }

			 /* Apply the player damage bonuses */
			 k += p_ptr->to_d + weapon_mastery(!(!o_ptr->k_idx && dual_wielding())) 
			      + might_bonus(k) + tiger_strike(k, hit_desc);

			 /* Add to charges */
			 charge_ups(TRUE);

			 /* Reduce damage */
			 k = (k * (40 + (skill_value(BARB_STUN) * 3))) / 100;

			 /* No negative damage */
			 if (k < 0) k = 0;

			 /* Assasin's healing */
			 life_steal(k, hit_desc);

			 /* Message */
			 message_format(MSG_HIT, m_ptr->r_idx, "You attemp to stun %s.", m_name);

			 /* Damage, check for fear and death */
			 if (!mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))
			 {
			      /* Stunning */
			      if (m_ptr->stunned)
			      {
				   msg_format("%^s is stunned slightly more.", m_name);
				   m_ptr->stunned += 
					randint((skill_value(BARB_STUN) / 2) + 1);
				   if (m_ptr->stunned > 24) m_ptr->stunned = 24;
			      }
			      else
			      {
				   msg_format("%^s is stunned.", m_name);
				   m_ptr->stunned += 
					randint(skill_value(BARB_STUN) * 2) + 5;
				   if (m_ptr->stunned > 24) m_ptr->stunned = 24;
			      }

			 } else break; /* Stop hitting */
		    }
		    else /* Missed */
		    {
			 message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }

	       /* IAS */
	       done = 10000 / (100 + ias_bonus());
	       
	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       /* Mega-Hack -- apply earthquake brand */
	       if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, FALSE, 85);

	       break;
	  }
	  case BARB_BERSERK:
	  {
	       int duration = (skill_value(BARB_BERSERK) * 2) + 10;
	       (void)set_afraid(0);
	       (void)set_berserk_rage(p_ptr->berserk_rage + 
				      randint(duration) + duration);
	       done = 100;
	       break;
	  }
	  case BARB_FRENZY:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_FRENZY)
	       {
		    msg_format("%s", aura_end_string[AURA_FRENZY]);
		    p_ptr->aura = 0;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_FRENZY]);
		    p_ptr->aura = AURA_FRENZY;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case SORC_ZAP:
	  {
	       int dmg = damroll(2, (skill_value(SORC_ZAP) * 3 * 
				     sorc_dmg_boost(SORC_ELEC_M)) / 100);
	       if (get_aim_dir(&dir))
	       {
		    fire_bolt(GF_ELEC, dir, dmg);
		    done = 100;
	       }
	       break;
	  }
	  case SORC_FIREBOLT:
	  {
	       int dmg = damroll((skill_value(SORC_FIREBOLT) * sorc_dmg_boost(SORC_FIRE_M)) / 100, 9);
	       if (get_aim_dir(&dir))
	       {
		    fire_bolt(GF_FIRE, dir, dmg);
		    done = 100;
	       }
	       break;
	  }
	  case SORC_FROSTBOLT:
	  {
	       int dmg = damroll((skill_value(SORC_FROSTBOLT) * sorc_dmg_boost(SORC_COLD_M)) / 100, 6);
	       if (get_aim_dir(&dir))
	       {
		    fire_bolt(GF_COLD, dir, dmg);
		    done = 100;
	       }
	       break;
	  }
	  case SORC_TELE:
	  {
	       teleport_player(skill_value(SORC_TELE) * 5);
	       done = 100;
	       break;
	  }
	  case SORC_LIGHTNING:
	  {
	       int dmg = randint((skill_value(SORC_LIGHTNING) * 5 * sorc_dmg_boost(SORC_ELEC_M)) / 100);
	       if (get_aim_dir(&dir))
	       {
		    fire_beam(GF_ELEC, dir, dmg);
		    done = 100;
	       }
	       break;
	  }
	  case SORC_FIREBALL:
	  {
	       int dmg = ((40 + (skill_value(SORC_FIREBALL) * 4)) * sorc_dmg_boost(SORC_FIRE_M)) / 100;
	       if (get_aim_dir(&dir))
	       {
		    fire_ball(GF_FIRE, dir, dmg, 1 + (skill_value(SORC_FIREBALL) / 10));
		    done = 100;
	       }
	       break;
	  }
	  case SORC_GLACIAL_SPIKE:
	  {
	       int dmg = ((15 + (skill_value(SORC_GLACIAL_SPIKE) * 3 / 2)) * 
			  sorc_dmg_boost(SORC_COLD_M)) / 100;
	       if (get_aim_dir(&dir))
	       {
		    fire_bolt(GF_ICE, dir, dmg);
		    fire_ball(GF_COLD, dir, dmg, 1 + (skill_value(SORC_GLACIAL_SPIKE) / 10));
		    done = 100;
	       }
	       break;
	  }
	  case SORC_THUNDERSTORM:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_THUNDERSTORM)
	       {
		    msg_format("%s", aura_end_string[AURA_THUNDERSTORM]);
		    p_ptr->aura = 0;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_THUNDERSTORM]);
		    p_ptr->aura = AURA_THUNDERSTORM;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case SORC_BLIZZARD:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_BLIZZARD)
	       {
		    msg_format("%s", aura_end_string[AURA_BLIZZARD]);
		    p_ptr->aura = 0;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_BLIZZARD]);
		    p_ptr->aura = AURA_BLIZZARD;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case SORC_METEOR:
	  {
	       int dmg = damroll(10, ((skill_value(SORC_METEOR) + 10) * sorc_dmg_boost(SORC_FIRE_M)) / 100);
	       if (get_aim_dir(&dir))
	       {
		    fire_ball(GF_FIRE, dir, dmg, 3);
		    done = 100;
	       }
	       break;
	  }
	  case DRUID_SUNRAY:
	  {
	       (void)lite_area(0, (skill_value(DRUID_SUNRAY) / 5) + 2);
	       if (get_aim_dir(&dir))
		    lite_line(dir, randint(skill_value(DRUID_SUNRAY) * 5));
	       done = 100;
	       break;
	  }
	  case DRUID_WOLF:
	  {
	       /* Change to this form */
	       if (p_ptr->shapeshift != SHAPE_WOLF)
	       {
		    p_ptr->shapeshift = SHAPE_WOLF;
		    p_ptr->tim_shapeshift = 40 + (skill_value(DRUID_LYCANTHRO) * 23);
		    p_ptr->redraw |= (PR_FORM);
		    p_ptr->update |= (PU_BONUS | PU_HP);
		    msg_format("%s", shapeshift_begin_string[SHAPE_WOLF]);
		    done = 100;
	       }
	       else msg_print("You are already in the form of a wolf!");
	       break;
	  }
	  case DRUID_BEAR:
	  {
	       /* Change to this form */
	       if (p_ptr->shapeshift != SHAPE_BEAR)
	       {
		    /* Feral rage ends when wolf form is left */
		    if (p_ptr->shapeshift == SHAPE_WOLF && p_ptr->feral_rage)
			 (void)set_feral_rage(0);

		    p_ptr->shapeshift = SHAPE_BEAR;
		    p_ptr->tim_shapeshift = 40 + (skill_value(DRUID_LYCANTHRO) * 23);
		    p_ptr->redraw |= (PR_FORM);
		    p_ptr->update |= (PU_BONUS | PU_HP);
		    msg_format("%s", shapeshift_begin_string[SHAPE_BEAR]);
		    done = 100;
	       }
	       else msg_print("You are already in the form of a bear!");
	       break;
	  }
	  case DRUID_MAUL:
	  {
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       char m_name[80];
	       bool fear = FALSE;
	       bool do_quake = FALSE;

	       /* Get weapon */   
	       object_type *o_ptr = &inventory[INVEN_WIELD];
	       if (!o_ptr->k_idx && dual_wielding()) o_ptr = &inventory[INVEN_ARM];

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    int bonus, k = 0;

		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    bonus = p_ptr->to_h;
		    if (o_ptr->k_idx) bonus += o_ptr->to_h;

		    /* Check for successful hit */
		    if (test_hit_norm(p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
		    {
			 char hit_desc[10] = "unused";

			 /* Handle normal weapon */
			 if (o_ptr->k_idx)
			 {
			      k = damroll(o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));
			      k = tot_dam_aux(o_ptr, k, m_ptr);
			      if (p_ptr->impact && (k > 50)) do_quake = TRUE;
			      k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			      k = assasination(r_ptr, k); /* default is to return unchanged */
			 }
			 else if (skill_value(DRUID_UNARMED) &&
				  !inventory[INVEN_WIELD].k_idx &&
				  !dual_wielding()) /* unarmed attack */
			 {
			      /* Unarmed attacks */
			      k = damroll(1, unarmed_damage(skill_value(DRUID_UNARMED)));
			      k = critical_norm(skill_value(DRUID_UNARMED) * 5, p_ptr->to_h, k);
			 }
			 else if (skill_value(DRUID_UNARMED) &&
				  !inventory[INVEN_WIELD].k_idx &&
				  !dual_wielding())
			 {
			     /* Unarmed attacks */
			     k = damroll(1, unarmed_damage(skill_value(DRUID_UNARMED)));
			     k = critical_norm(skill_value(DRUID_UNARMED) * 5, p_ptr->to_h, k);
			 }

			 /* Maul damage bonus, from 53% to 300% */
			 k += k * (40 + (skill_value(DRUID_MAUL) * 13)) / 100;

			 /* Apply the player damage bonuses */
			 k += p_ptr->to_d + weapon_mastery(!(!o_ptr->k_idx && dual_wielding())) 
			      + might_bonus(k) + tiger_strike(k, hit_desc);

			 /* Add to charges */
			 charge_ups(TRUE);

			 /* No negative damage */
			 if (k < 0) k = 0;

			 /* Assasin's healing */
			 life_steal(k, hit_desc);

			 /* Message */
			 if (o_ptr->k_idx)
			 {
			      char o_name[80];
			      object_desc(o_name, o_ptr, FALSE, 0);
			      message_format(MSG_HIT, m_ptr->r_idx, "You maul %s with your %s.", 
					     m_name, o_name);
			 }
			 else
			      message_format(MSG_HIT, m_ptr->r_idx, "You maul %s.", m_name);

			 /* Damage, check for fear and death */
			 if (!mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))
			 {
			      /* Stunning */
			      int maul_quality = (p_ptr->skill_thn / 5);
			      if (o_ptr->k_idx)
				   maul_quality += (o_ptr->weight / 3);
			      else if (!inventory[INVEN_WIELD].k_idx &&
				       !dual_wielding())
				   maul_quality += (skill_value(DRUID_UNARMED) * 5 / 3);
			      if (maul_quality + (skill_value(DRUID_MAUL) * 5 / 2) >
				  randint(400 + r_ptr->level * 32))
			      {
				   /* Stunning */
				   if (m_ptr->stunned)
				   {
					msg_format("%^s is stunned slightly more.", m_name);
					m_ptr->stunned += 
					     randint((skill_value(DRUID_MAUL) / 4) + 1);
					if (m_ptr->stunned > 24) m_ptr->stunned = 24;
				   }
				   else
				   {
					msg_format("%^s is stunned.", m_name);
					m_ptr->stunned += rand_int(skill_value(DRUID_MAUL) / 2) + 4;
					if (m_ptr->stunned > 24) m_ptr->stunned = 24;
				   }
			      } else break; /* Stop hitting */
			 }
		    }
		    else /* Missed */
		    {
			 message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }

	       /* IAS */
	       done = 10000 / (100 + ias_bonus());

	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       /* Mega-Hack -- apply earthquake brand */
	       if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, FALSE, 85);

	       break;
	  }
	  case DRUID_RAGE:
	  {
	       int duration = (skill_value(DRUID_RAGE) * 4) + 20;
	       (void)set_afraid(0);
	       (void)set_feral_rage(p_ptr->feral_rage + randint(duration) + duration);
	       done = 100;
	       break;
	  }
	  case DRUID_SHOCKWAVE:
	  {
	      fire_ball(GF_FORCE, 5, skill_value(DRUID_SHOCKWAVE) * 5, 
			(skill_value(DRUID_SHOCKWAVE) / 4) + 5);	       
	       earthquake(p_ptr->py, p_ptr->px, (skill_value(DRUID_SHOCKWAVE) / 4) + 5, TRUE, 
			  90 - (skill_value(DRUID_SHOCKWAVE) * 2));
	       done = 100;
	       break;
	  }
	  case ASSI_TIGER:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_TIGER)
	       {
		    msg_format("%s", aura_end_string[AURA_TIGER]);
		    p_ptr->aura = 0;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_TIGER]);
		    p_ptr->aura = AURA_TIGER;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case ASSI_FM_KICK:
	  {
	       /* Max number of kicks */
	       int max_kicks = (skill_value(ASSI_FM_KICK) / 5) + 2;
	       /* Count blows used */
	       int blows = 0;
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       char m_name[80];
	       bool fear = FALSE;
	       bool do_quake = FALSE;

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    int i, bonus, k = 0;
		    /* Set dmg bonus here so it doesn't get rerolled per hit */
		    int dmg_bonus = damroll(1, skill_value(ASSI_FM_KICK) * 5);

		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    /* Attempt to hit a number of times */
		    for (i = 0; i < max_kicks; i++)
		    {
			 char hit_desc[10] = "unused";

			 bonus = p_ptr->to_h;

			 /* Check for successful hit */
			 if (test_hit_norm(p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
			 {
			      k = dmg_bonus;

			      /* Unarmed combat bonuses */
			      if (skill_value(DRUID_UNARMED))
			      {
				   /* Unarmed attacks */
				   k = damroll(1, unarmed_damage(skill_value(DRUID_UNARMED)));
				   k = critical_norm(skill_value(DRUID_UNARMED) * 5, p_ptr->to_h, k);
			      }

			      k = assasination(r_ptr, k); /* default is to return unchanged */

			      /* Apply the player damage bonuses */
			      k += p_ptr->to_d + might_bonus(k) + tiger_strike(k, hit_desc);

			      /* Add to charges */
			      charge_ups(TRUE);

			      /* Reduce damage of subsequent kicks
			       *   0  1   2   3   4   5
			       * 2 1 75%
			       * 3 1 83% 66%
			       * 4 1 87% 75% 62%
			       * 5 1 90% 80% 70% 60%
			       * 6 1 91% 83% 75% 66% 58% */
			      k -= (k * i) / (max_kicks * 2);

			      /* No negative damage */
			      if (k < 0) k = 0;

			      /* Assasin's healing */
			      life_steal(k, hit_desc);

			      /* Message */
			      message_format(MSG_HIT, m_ptr->r_idx, "You kick %s.", m_name);

			      /* Damage, check for fear and death */
			      if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))
			      {
				   blows++; break;
			      }
			 }
			 else /* Missed */
			 {
			      message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			      			      
			      /* Stop kicking */
			      blows++; break;
			 }

			 /* Count attacks */
			 blows++;
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }

	       /* IAS */
	       done = 10000 / (100 + ias_bonus());
			      
	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       /* Mega-Hack -- apply earthquake brand */
	       if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, FALSE, 85);

	       break;
	  }
	  case ASSI_FM_CLAWS:
	  {
	       /* Count blows used */
	       int blows = 0;
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       char m_name[80];
	       object_type *o_ptr;
	       bool fear = FALSE;
	       bool do_quake = FALSE;

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    int i, bonus, k = 0;

		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    /* Attempt to hit twice */
		    for (i = 0; i < 2; i++)
		    {
			 /* Get weapon */
			 o_ptr = &inventory[INVEN_WIELD];
			 /* Second blow uses second weapon */
			 if (i) o_ptr = &inventory[INVEN_ARM];

			 /* To-hit bonuses */
			 bonus = p_ptr->to_h;
			 bonus += skill_value(ASSI_FM_CLAWS);
			 if (o_ptr->k_idx) bonus += o_ptr->to_h;

			 /* Check for successful hit */
			 if (test_hit_norm(p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
			 {
			      char o_name[80];
			      char hit_desc[10] = "unused";

			      /* Handle normal weapon */
			      k = damroll(o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));
			      k = tot_dam_aux(o_ptr, k, m_ptr);
			      if (p_ptr->impact && (k > 50)) do_quake = TRUE;
			      k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			      k = assasination(r_ptr, k); /* default is to return unchanged */
			      
			      /* Apply the player damage bonuses */
			      k += p_ptr->to_d + weapon_mastery(!i) + might_bonus(k) + 
				   tiger_strike(k, hit_desc);

			      /* Add to charges */
			      charge_ups(TRUE);

			      /* No negative damage */
			      if (k < 0) k = 0;

			      /* Assasin's healing */
			      life_steal(k, hit_desc);

			      /* Message */
			      object_desc(o_name, o_ptr, FALSE, 0);
			      message_format(MSG_HIT, m_ptr->r_idx, "You strike at %s with your %s.",
					     m_name, o_name);

			      /* Damage, check for fear and death */
			      if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))
			      {
				   blows++; break;
			      }
			 }
			 else /* Missed */
			 {
			      message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			 }
			 
			 /* Count attacks */
			 blows++;
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }

	       /* IAS */
	       done = 10000 / (100 + ias_bonus() + (skill_value(ASSI_FM_CLAWS) * 15 / 2));
	       
	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       /* Mega-Hack -- apply earthquake brand */
	       if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, FALSE, 85);
	       break;
	  }
	  case ASSI_SPEED:
	  {
	       int duration = (skill_value(ASSI_SPEED) * 2) + 10;
	       (void)set_speed_burst(p_ptr->speed_burst + randint(duration) + duration);
	       done = 100;
	       break;
	  }
	  case ASSI_COBRA:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_COBRA)
	       {
		    msg_format("%s", aura_end_string[AURA_COBRA]);
		    p_ptr->aura = 0;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_COBRA]);
		    p_ptr->aura = AURA_COBRA;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case ARCH_JAB:
	  {
	       /* Max number of jabs */
	       int max_jabs = (skill_value(ARCH_JAB) / 5) + 2;
	       /* Count blows used */
	       int blows = 0;
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       char m_name[80];
	       bool fear = FALSE;
	       bool do_quake = FALSE;

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    int i, bonus, k, dmg_base;
		    object_type *o_ptr = &inventory[INVEN_WIELD];
		    if (!o_ptr->k_idx && dual_wielding()) o_ptr = &inventory[INVEN_ARM];

		    /* Set random damage here so it doesn't get rerolled per hit */
		    dmg_base = damroll(o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));

		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    /* Attempt to hit a number of times */
		    for (i = 0; i < max_jabs; i++)
		    {
			 bonus = p_ptr->to_h;
			 bonus += o_ptr->to_h;

			 /* Check for successful hit */
			 if (test_hit_norm(p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
			 {
			      char o_name[80];
			      char hit_desc[10] = "hit";

			      /* Apply the player damage bonuses */
			      k = dmg_base;
			      k = tot_dam_aux(o_ptr, k, m_ptr);
			      if (p_ptr->impact && (k > 50)) do_quake = TRUE;
			      k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
			      k = assasination(r_ptr, k); /* default is to return unchanged */
			      
			      /* Add a percentage bonus to damage */
			      k += (k * skill_value(ARCH_JAB) * 10) / 100;

			      /* Apply the player damage bonuses */
			      k += p_ptr->to_d + weapon_mastery(!(!o_ptr->k_idx && dual_wielding())) 
				   + might_bonus(k) + tiger_strike(k, hit_desc);

			      /* Add to charges */
			      charge_ups(TRUE);

			      /* Reduce damage of subsequent jabs
			       *   0  1   2   3   4   5
			       * 2 1 75%
			       * 3 1 83% 66%
			       * 4 1 87% 75% 62%
			       * 5 1 90% 80% 70% 60%
			       * 6 1 91% 83% 75% 66% 58% */
			      k -= (k * i) / (max_jabs * 2);

			      /* No negative damage */
			      if (k < 0) k = 0;

			      /* Assasin's healing */
			      life_steal(k, hit_desc);

			      /* Message */
			      object_desc(o_name, o_ptr, FALSE, 0);
			      message_format(MSG_HIT, m_ptr->r_idx, "You %s %s with your %s.", 
					     hit_desc, m_name, o_name);

			      /* Damage, check for fear and death */
			      if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL))
			      {
				   blows++; break;
			      }
			 }
			 else /* Missed */
			 {
			      message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			      
			      /* Stop jabbing */
			      blows++; break;
			 }

			 /* Count attacks */
			 blows++;
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }

	       /* IAS */
	       done = 10000 / (100 + ias_bonus());
	       
	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       /* Mega-Hack -- apply earthquake brand */
	       if (do_quake) earthquake(p_ptr->py, p_ptr->px, 10, FALSE, 85);

	       break;
	  }
	  case ARCH_MAGIC_AMMO:
	  {
	       object_type *o_ptr;

	       /* Get weapon */
	       o_ptr = &inventory[INVEN_BOW];

	       /* Must be an object here */
	       if (o_ptr->k_idx)
	       {
		    int sides = 2;
		    if (o_ptr->sval == SV_SHORT_BOW || o_ptr->sval == SV_LONG_BOW) 
			 sides = 4;
		    if (o_ptr->sval == SV_LIGHT_XBOW || o_ptr->sval == SV_HEAVY_XBOW) 
			 sides = 5;

		    if (get_aim_dir(&dir))
		    {
			 fire_bolt(GF_ARROW, dir, (damroll(1, sides) + o_ptr->to_d + 
						   skill_value(ARCH_MAGIC_AMMO)) * p_ptr->ammo_mult);
			 done = 100;
		    }      
	       }

	       break;
	  }
	  case PALA_HEROISM:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_HEROISM)
	       {
		    msg_format("%s", aura_end_string[AURA_HEROISM]);
		    p_ptr->aura = 0;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_HEROISM]);
		    p_ptr->aura = AURA_HEROISM;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case PALA_MIGHT:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_MIGHT)
	       {
		    msg_format("%s", aura_end_string[AURA_MIGHT]);
		    p_ptr->aura = 0;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_MIGHT]);
		    p_ptr->aura = AURA_MIGHT;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case PALA_SMITE:
	  {
	       if (get_aim_dir(&dir))
	       {
		    fire_bolt(GF_DISP_UNDEAD, dir, rand_range(4, skill_value(PALA_SMITE) * 5));
		    done = 100;
	       }
	       break;
	  }
	  case PALA_PRAYER:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_PRAYER)
	       {
		    msg_format("%s", aura_end_string[AURA_PRAYER]);
		    p_ptr->aura = 0;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    p_ptr->aura = AURA_PRAYER;
		    msg_format("%s", aura_begin_string[AURA_PRAYER]);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case PALA_SHIELD_BASH:
	  {
	       int y, x;
	       monster_type *m_ptr;
	       monster_race *r_ptr;
	       monster_lore *l_ptr;
	       char m_name[80];
	       int bash_quality, bash_dam;
	       bool fear = FALSE;

	       /* Get shield */   
	       object_type *o_ptr = &inventory[INVEN_ARM];

	       /* Not if afraid */
	       if (p_ptr->afraid)
	       {
		    msg_format("You are too afraid to attack anything!");
		    break;
	       }

	       /* Get a direction (or abort) */
	       if (!get_rep_dir(&dir)) break;

	       /* Get location */
	       y = p_ptr->py + ddy[dir];
	       x = p_ptr->px + ddx[dir];

	       /* Apply confusion */
	       if (confuse_dir(&dir))
	       {
		    /* Get location */
		    y = p_ptr->py + ddy[dir];
		    x = p_ptr->px + ddx[dir];
	       }

	       /* Monster */
	       if (cave_m_idx[y][x] > 0)
	       {
		    /* Get the monster */
		    m_ptr = &m_list[cave_m_idx[y][x]];
		    r_ptr = &r_info[m_ptr->r_idx];
		    l_ptr = &l_list[m_ptr->r_idx];
		    /* Disturb the monster */
		    m_ptr->csleep = 0;
		    /* Extract monster name (or "it") */
		    monster_desc(m_name, m_ptr, 0);
		    /* Auto-Recall if possible and visible */
		    if (m_ptr->ml) monster_race_track(m_ptr->r_idx);
		    /* Track a new monster */
		    if (m_ptr->ml) health_track(cave_m_idx[y][x]);

		    /* Check for successful hit */
		    if (test_hit_norm(p_ptr->skill_thn + (p_ptr->to_h * BTH_PLUS_ADJ), r_ptr->ac, m_ptr->ml))
		    {
			 char hit_desc[10] = "unused";

			 /* Get damage, affected by fighting skill, shield, and strength */
			 bash_quality = (p_ptr->skill_thn / 5) + (o_ptr->weight / 3);
			 bash_dam = damroll(o_ptr->dd, amplify_ds(o_ptr->dd, o_ptr->ds, o_ptr->to_d));
			 bash_dam *= (bash_quality / 20) + 
			      ((skill_value(PALA_SHIELD_BASH) + 1) / 2);
			 bash_dam += (adj_str_td[p_ptr->stat_ind[A_STR]] - 128);

			 /* Apply the player damage bonuses */
			 bash_dam += p_ptr->to_d + might_bonus(bash_dam) + tiger_strike(bash_dam, hit_desc);

			 /* Add to charges */
			 charge_ups(TRUE);

			 /* Holy Shield grants 2-4 times extra damage vs evil */
			 if ((r_ptr->flags3 & RF3_EVIL) && (skill_value(PALA_HOLY_SHIELD)))
			 { 
			      bash_dam *= (skill_value(PALA_HOLY_SHIELD) / 10) + 2;
			      if (m_ptr->ml) /* learn */
				   l_ptr->r_flags3 |= (RF3_EVIL);
			 }
			 if (bash_dam > 125) bash_dam = 125;

			 /* No negative damage */
			 if (bash_dam < 0) bash_dam = 0;
			 
			 /* Assasin's healing */
			 life_steal(bash_dam, hit_desc);

			 /* Message */
			 message_format(MSG_HIT, m_ptr->r_idx, "You bash %s.", m_name);

			 /* Damage, check for fear and death */
			 if (!mon_take_hit(cave_m_idx[y][x], bash_dam, &fear, NULL))
			 {
			      if (bash_quality + 
				  (skill_value(PALA_SHIELD_BASH) * 5 / 2) >
				  randint(400 + r_ptr->level * 32))
			      {
				   /* Stunning */
				   if (m_ptr->stunned)
				   {
					msg_format("%^s is stunned slightly more.", m_name);
					m_ptr->stunned += 
					     randint((skill_value(PALA_SHIELD_BASH) / 4) + 1);
					if (m_ptr->stunned > 24) m_ptr->stunned = 24;
				   }
				   else
				   {
					msg_format("%^s is stunned.", m_name);
					m_ptr->stunned += rand_int(skill_value(PALA_SHIELD_BASH) / 2) + 4;
					if (m_ptr->stunned > 24) m_ptr->stunned = 24;
				   }
			      }

			 } else break; /* Stop hitting */
		    }
		    else /* Missed */
		    {
			 message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
		    }
	       }
	       else
	       {
		    msg_print("There is nobody there!");
		    break;
	       }
	       
	       /* IAS */
	       done = 10000 / (100 + ias_bonus());
	       
	       /* Fear */
	       if (fear && m_ptr->ml)
		    message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);

	       break;
	  }
	  case PALA_HOLY_LIGHT:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_HOLY_LIGHT)
	       {
		    msg_format("%s", aura_end_string[AURA_HOLY_LIGHT]);
		    p_ptr->aura = 0;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_HOLY_LIGHT]);
		    p_ptr->aura = AURA_HOLY_LIGHT;
		    p_ptr->update |= (PU_BONUS);
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case NECRO_TEETH:
	  {
	       if (get_aim_dir(&dir))
	       {
		    int i;
		    for (i = 0; i < ((skill_value(NECRO_TEETH) * 3) / 20) + 2; i++)
			 fire_bolt(GF_ARROW, dir, skill_value(NECRO_TEETH) * 3);
		    done = 100;
	       }
	       break;
	  }
	  case NECRO_FUMES:
	  {
	       if (get_aim_dir(&dir))
	       {
		    fire_ball(GF_POIS, dir, 40 + (skill_value(NECRO_FUMES) * 4), 
			      1 + (skill_value(NECRO_FUMES) / 5));
		    done = 100;
	       }
	       break;
	  }
	  case NECRO_BONE_SPEAR:
	  {
	       if (get_aim_dir(&dir))
	       {
		    fire_beam(GF_ARROW, dir, randint(skill_value(NECRO_BONE_SPEAR) * 5));
		    done = 100;
	       }
	       break;
	  }
	  case NECRO_FEAR:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_FEAR)
	       {
		    msg_format("%s", aura_end_string[AURA_FEAR]);
		    p_ptr->aura = 0;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_FEAR]);
		    p_ptr->aura = AURA_FEAR;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  case NECRO_SLOW:
	  {
	       /* Deactivate */
	       if (p_ptr->aura == AURA_SLOW)
	       {
		    msg_format("%s", aura_end_string[AURA_SLOW]);
		    p_ptr->aura = 0;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       else /* Activate */
	       {
		    msg_format("%s", aura_begin_string[AURA_SLOW]);
		    p_ptr->aura = AURA_SLOW;
		    p_ptr->redraw |= (PR_FORM);
	       }
	       break;
	  }
	  }
     }

     if (done)
     {
	  /* Use mana */
	  p_ptr->csp -= mana_cost;

	  if (p_ptr->cure_sp)
	  {
	       p_ptr->cure_sp -= mana_cost;
	       if (p_ptr->cure_sp < 1)
	       {
		    p_ptr->cure_sp = 0;
		    p_ptr->time_sp = 0;
	       }
	  }

	  /* Take a turn */
	  p_ptr->energy_use = done;

	  /* Redraw mana */
	  p_ptr->redraw |= (PR_MANA);

	  /* Window stuff */
	  p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
     }

     /* Handle stuff */
     handle_stuff();
}


void do_cmd_use_skill()
{
     /* Get skill to use */
     int skill = get_skill(FALSE);

     use_skill(skill);
}

static cptr stat_names_long[A_MAX] = {
     "Strength",
     "Intelligence",
     "Wisdom",
     "Dexterity",
     "Constitution",
     "Charisma"
};

void practice_stat()
{
     bool flag = FALSE;
     int ask;
     char tmp_val[160];
     char out_val[160];
     char choice;

     /* Needs stat points */
     if (p_ptr->stat_points > 0)
     {
	  /* Ask for a stat to raise */
	  Term_save();
	  while (1)
	  {
	       int i, n = 0, stat[A_MAX];
	       char tmp[32];
	       
	       /* Get raiseable stats */
	       for (i = 0; i < A_MAX; i++)
	       {
		    if (p_ptr->stat_max[i] < 18+100)
		    {
			 stat[n] = i;
			 n++;
		    }
	       }
	       
	       /* If there is a choice to be made */
	       if (n > 1)
	       {
		    /* Show choices */
		    for (i = 0; i < n; i++)
		    {
			 cnv_stat(p_ptr->stat_max[stat[i]], tmp);
			 prt(format(" %c) %-14s (cur %s)", I2A(i), 
				 stat_names_long[stat[i]], tmp), 1 + i, 14);
		    }

		    /* Prompt */
		    strnfmt(out_val, 78, "(Stats %c-%c, ESC=exit) Raise which stat?", 
			    I2A(0), I2A(n - 1));

		    /* Wait */
		    while (!flag && get_com(out_val, &choice))
		    {
			 /* Note verify */
			 ask = (isupper(choice));
			 
			 /* Lowercase */
			 if (ask) choice = tolower(choice);
			 
			 /* Extract request */
			 i = (islower(choice) ? A2I(choice) : -1);
			 
			 /* Totally Illegal */
			 if ((i < 0) || (i >= n))
			 {
			      bell("Illegal stat choice!");
			      continue;
			 }

			 /* Verify it */
			 if (ask)
			 {
			      char tmp_val[160];
			      
			      /* Prompt */
			      strnfmt(tmp_val, 78, "Raise %s? ", stat_names_long[stat[i]]);

			      /* Belay that order */
			      if (!get_check(tmp_val)) continue;
			 }
	  
			 /* Stop the loop */
			 flag = TRUE;
		    }

		    Term_load();

		    /* Abort if needed */
		    if (!flag) return;

		    /* Increase the stat */
		    do_inc_stat(stat[i]);
		    p_ptr->stat_bonus[stat[i]]++;
	       }
	       /* Only one choice */
	       else if (n == 1)
	       {
		    /* Prompt */
		    strnfmt(tmp_val, 78, "Raise %s? ", stat_names_long[stat[0]]);

		    Term_load();

		    /* Belay that order */
		    if (!get_check(tmp_val)) return;

		    /* Increase the stat */
		    do_inc_stat(stat[0]);
	       }
	       /* Nothing to raise */
	       else if (n == 0)
	       {
		    Term_load();

		    msg_print("You cannot raise any stats further!");
		    return;
	       }
	       
	       break;
	  }
	  message_flush(); /* Icky */
		
	  /* Update some stuff */
	  p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);
	  
	  /* Redraw some stuff */
	  p_ptr->redraw |= (PR_HP | PR_MANA);
	  
	  /* Window stuff */
	  p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	  
	  /* Handle stuff */
	  handle_stuff();

	  /* Take a turn */
	  p_ptr->energy_use = 100;
     }
     else msg_print("You have no stat points to spend!");
}

/* Return the skill a barbarian has in a weapon, either first hand or second
 * use in monster1.c to show how many blows it takes to kill a monster
 * and files.c to display to-hit and to-damage
 * and py_attack
 */
int weapon_mastery(bool first_hand)
{
     if (first_hand)
     {
	  /* Check weapon slot */
	  if (inventory[INVEN_WIELD].k_idx)
	  {
	       /* Return the relevant skill */
	       switch (inventory[INVEN_WIELD].tval)
	       {
	       case TV_SWORD:
		    return skill_value(BARB_SWORD);
	       case TV_POLEARM:
		    return skill_value(BARB_POLEARM);
	       case TV_HAFTED:
			 return skill_value(BARB_HAFTED);
	       case TV_AXE:
		    return skill_value(BARB_AXE);
	       case TV_CLAW:
		    return skill_value(ASSI_CLAWS);
	       default: /* Should never happen */
		    return 0;
	       }
	  }
     }
     else /* Look in shield slot */
     {
	  /* Return 0 if not a weapon */
	  if (!dual_wielding()) return 0;
	  
	  /* Get skill */
	  switch (inventory[INVEN_ARM].tval)
	  {
	  case TV_SWORD:
	       return skill_value(BARB_SWORD);
	  case TV_POLEARM:
	       return skill_value(BARB_POLEARM);
	  case TV_HAFTED:
	       return skill_value(BARB_HAFTED);
	  case TV_AXE:
	       return skill_value(BARB_AXE);
	  case TV_CLAW:
	       return skill_value(ASSI_CLAWS);
	  default: /* Should never happen */
		    return 0;
	  }
     }

     /* Should never happen */     
     return 0;
}

/* Returns a percentage to increase damage by, depending on relevant elemental
 * mastery skill 
*/
int sorc_dmg_boost(int skill)
{
     if (skill_value(skill) > 0)
     {
	  /* 129% to 300% damage */
	  return ((skill_value(skill) * 9) + 120);
     }
     else return (100); /* No bonus */
}

/* Returns total increased attack speed bonus */
int ias_bonus()
{
     int bonus = 0;

     /* Frenzying */
     if (p_ptr->frenzy)
	  bonus += p_ptr->frenzy * skill_value(aura_skill[AURA_FRENZY]);

     /* Speed burst */
     if (p_ptr->speed_burst)
	  bonus += skill_value(aura_skill[AURA_FRENZY]) * 5;

     /* Wolf */
     if (p_ptr->shapeshift == SHAPE_WOLF)
	  bonus += skill_value(shapeshift_skill[SHAPE_WOLF]) * 5;

     return bonus;
}

/* Add to charge-ups and cost mana */
void charge_ups(bool finishing_move)
{
     /* Finishing moves do not add to assasin's charges */
     if (!finishing_move)
     {
	  if (p_ptr->aura == AURA_TIGER && p_ptr->charge_up[CU_SKILL_TIGER] < 3) 
	  {
	       int manacost = skill_info[ASSI_TIGER].mana_base +
		    (skill_info[ASSI_TIGER].mana_plus * skill_value(ASSI_TIGER) / 10);
	       if (p_ptr->csp >= manacost)
	       {
		    p_ptr->charge_up[CU_SKILL_TIGER]++; 
		    p_ptr->csp -= manacost;
		    if (p_ptr->cure_sp)
		    {
			 p_ptr->cure_sp -= manacost;
			 if (p_ptr->cure_sp < 1)
			 {
			      p_ptr->cure_sp = 0;
			      p_ptr->time_sp = 0;
			 }
		    }
		    p_ptr->redraw |= (PR_BASIC); 
	       }
	  }
	  if (p_ptr->aura == AURA_COBRA && p_ptr->charge_up[CU_SKILL_COBRA] < 3) 
	  {
	       int manacost = skill_info[ASSI_COBRA].mana_base +
		    (skill_info[ASSI_COBRA].mana_plus * skill_value(ASSI_COBRA) / 10);
	       if (p_ptr->csp >= manacost)
	       {
		    p_ptr->charge_up[CU_SKILL_COBRA]++; 
		    p_ptr->csp -= manacost;
		    if (p_ptr->cure_sp)
		    {
			 p_ptr->cure_sp -= manacost;
			 if (p_ptr->cure_sp < 1)
			 {
			      p_ptr->cure_sp = 0;
			      p_ptr->time_sp = 0;
			 }
		    }
		    p_ptr->redraw |= (PR_BASIC); 
	       }
	  }
     }

     /* Barbarian's get frenzy bonus from hits */
     if (p_ptr->aura == AURA_FRENZY)
     {
	  int manacost = skill_info[aura_skill[AURA_FRENZY]].mana_base +
	       (skill_info[aura_skill[AURA_FRENZY]].mana_plus * 
		skill_value(aura_skill[AURA_FRENZY]) / 10);
	  if (p_ptr->csp >= manacost && p_ptr->frenzy < 5 && 
	      /* Random chance of increasing, always if 0 */
	      (rand_int(100) < 30 + (skill_value(aura_skill[AURA_FRENZY]) * 3) ||
	       p_ptr->frenzy < 1))
	  {
	       p_ptr->frenzy++;
	       p_ptr->csp -= manacost;
	       p_ptr->redraw |= (PR_SPEED | PR_BASIC);
	       p_ptr->update |= (PU_BONUS);
	  }
     }
}

/* Add to damage, may modify hit description */
int tiger_strike(int amount, char *p)
{
     int bonus = 0;

     /* 1/2, 1, or 3/2 times skill bonus to damage */
     if (p_ptr->charge_up[CU_SKILL_TIGER])
     {
	  bonus = (skill_value(ASSI_TIGER) * 
		p_ptr->charge_up[CU_SKILL_TIGER]) / 2;
	  
	  p_ptr->charge_up[CU_SKILL_TIGER]--;
	  p_ptr->redraw |= (PR_BASIC);
	  strcpy(p, "strike");
     }
     
     return bonus;
}

/* Life stealing, may modify hit description */
void life_steal(int amount, char *p)
{
     /* (skill + 5) * 2%, 5% or 8% times life steal */
     /* must come after finding final damage */
     if (p_ptr->charge_up[CU_SKILL_COBRA])
     {
	  int healing = amount;
	  healing = (healing * 
		     ((p_ptr->charge_up[CU_SKILL_COBRA] + 5) * 3 - 1) *
		     skill_value(ASSI_COBRA)) / 100;
	  if (hp_player(TRUE, healing, 0))
	  {
	       p_ptr->charge_up[CU_SKILL_COBRA]--;
	       p_ptr->redraw |= (PR_BASIC);
	       strcpy(p, "strike");
	  }
     }
}

/* Might damage bonus */
int might_bonus(int amount)
{
     if (p_ptr->aura == AURA_MIGHT)
	  return (amount * (10 + (skill_value(aura_skill[AURA_MIGHT]) * 9 / 2)) / 100);

     else return 0;
}

/* Taking off a skill item */
void takeoff_skill_item(object_type *o_ptr)
{
     if (o_ptr->xtra1 >= OBJECT_XTRA_TYPE_SKILL_FIRST &&
	 o_ptr->xtra1 <= OBJECT_XTRA_TYPE_SKILL_LAST)
     {
	  /* Skill without item does not exist */
	  if (skill_value(o_ptr->xtra2) - o_ptr->pval < 1)
	  {
	       /* By type */
	       switch (skill_info[o_ptr->xtra2].type)
	       {
	       case SKILL_AURA:
		    /* End auras */
		    if (o_ptr->xtra2 == aura_skill[p_ptr->aura])
		    {
			 msg_format("%s", aura_end_string[p_ptr->aura]);
			 p_ptr->aura = 0;
			 p_ptr->update |= (PU_BONUS);
			 p_ptr->redraw |= (PR_FORM);
		    }
		    break;
	       case SKILL_SHAPESHIFT:
		    /* End shapeshifts */
		    if (o_ptr->xtra2 == shapeshift_skill[p_ptr->shapeshift])
		    {
			 msg_format("%s", shapeshift_end_string[p_ptr->shapeshift]);
			 p_ptr->shapeshift = 0;
			 p_ptr->tim_shapeshift = 0;
			 p_ptr->update |= (PU_BONUS | PU_HP);
			 p_ptr->redraw |= (PR_FORM);
		    }
		    break;
	       }
	  }
     }
}
