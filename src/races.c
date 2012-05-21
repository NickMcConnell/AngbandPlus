#include "angband.h"

static void _devour_flesh_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Devour Flesh");
		break;
	case SPELL_DESC:
		var_set_string(res, "Devour flesh (yours) in order to fill your belly.");
		break;
	case SPELL_CAST:
		msg_print("You devour your own flesh!");
		set_food(PY_FOOD_MAX - 1);
		set_cut(p_ptr->cut + CUT_SEVERE, FALSE);
		take_hit(DAMAGE_USELIFE, p_ptr->mhp / 3, "devouring your own flesh", -1);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}


/****************************************************************
 * Public Entrypoints
 ****************************************************************/
race_t *get_race_t_aux(int prace, int psubrace)
{
	race_t *result = NULL;

	if (p_ptr->mimic_form)
	{
	}
	else
	{
		switch (prace)
		{
		case RACE_HUMAN:
			result = human_get_race_t();
			break;
		case RACE_DEMIGOD:
			result = demigod_get_race_t(psubrace);
			break;
		}
	}

	return result;
}

race_t *get_race_t(void)
{
	return get_race_t_aux(p_ptr->prace, p_ptr->psubrace);
}

/* HACK: This should be handled by the race_t entry point ...
   This is just here while I am refactoring code!!! */
int get_racial_powers(spell_info* spells, int max)
{
	int ct = 0;

	if (mut_present(MUT_ASTRAL_GUIDE))
	{
		spell_info *spell = &spells[ct++];
		spell->level = 5;
		spell->cost = 2;
		spell->fail = calculate_fail_rate(5, 50, p_ptr->stat_ind[A_DEX]);
		spell->fn = phase_door_spell;

		spell = &spells[ct++];
		spell->level = 15;
		spell->cost = 6;
		spell->fail = calculate_fail_rate(15, 50, p_ptr->stat_ind[A_DEX]);
		spell->fn = teleport_spell;
	}

	if (p_ptr->mimic_form)
	{
		switch (p_ptr->mimic_form)
		{
			case MIMIC_DEMON:
			case MIMIC_DEMON_LORD:
			{
				spell_info *spell = &spells[ct++];
				
				spell->level = 15;
				spell->cost = 10;
				spell->fail = calculate_fail_rate(15, 70, p_ptr->stat_ind[A_CON]);
				spell->fn = demon_breath_spell;
				break;
			}
			case MIMIC_VAMPIRE:
			{
				spell_info *spell = &spells[ct++];
				
				spell->level = 2;
				spell->cost = 1;
				spell->fail = calculate_fail_rate(2, 70, p_ptr->stat_ind[A_CON]);
				spell->fn = vampirism_spell;
				break;
			}
		}
	}
	else
	{
		switch (p_ptr->prace)
		{
			case RACE_DWARF:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 5;
				spell->cost = 5;
				spell->fail = calculate_fail_rate(5, 50, p_ptr->stat_ind[A_WIS]);
				spell->fn = detect_doors_stairs_traps_spell;
				break;
			}
			case RACE_NIBELUNG:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 10;
				spell->cost = 5;
				spell->fail = calculate_fail_rate(10, 50, p_ptr->stat_ind[A_WIS]);
				spell->fn = detect_doors_stairs_traps_spell;
				break;
			}
			case RACE_HOBBIT:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 15;
				spell->cost = 10;
				spell->fail = calculate_fail_rate(15, 50, p_ptr->stat_ind[A_INT]);
				spell->fn = create_food_spell;
				break;
			}
			case RACE_GNOME:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 5;
				spell->cost = 2;
				spell->fail = calculate_fail_rate(5, 50, p_ptr->stat_ind[A_INT]);
				spell->fn = phase_door_spell;
				break;
			}
			case RACE_SNOTLING:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 1;
				spell->cost = 0;
				spell->fail = 0;
				spell->fn = _devour_flesh_spell;
				break;
			}
			case RACE_HALF_TROLL:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 10;
				spell->cost = 12;
				spell->fail = calculate_fail_rate(10, 50, p_ptr->stat_ind[A_STR]);
				spell->fn = berserk_spell;
				break;
			}
			case RACE_BARBARIAN:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 8;
				spell->cost = 10;
				spell->fail = calculate_fail_rate(8, 30, p_ptr->stat_ind[A_STR]);
				spell->fn = berserk_spell;
				break;
			}
			case RACE_ENT:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 10;
				spell->cost = 20;
				spell->fail = calculate_fail_rate(10, 70, p_ptr->stat_ind[A_CHR]);
				spell->fn = summon_tree_spell;
				break;
			}
			case RACE_AMBERITE:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 30;
				spell->cost = 50;
				spell->fail = calculate_fail_rate(30, 70, p_ptr->stat_ind[A_INT]);
				spell->fn = shadow_shifting_spell;

				spell = &spells[ct++];
				spell->level = 40;
				spell->cost = 75;
				spell->fail = calculate_fail_rate(40, 75, p_ptr->stat_ind[A_WIS]);
				spell->fn = pattern_mindwalk_spell;
				break;
			}
			case RACE_HALF_OGRE:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 25;
				spell->cost = 35;
				spell->fail = calculate_fail_rate(25, 70, p_ptr->stat_ind[A_INT]);
				spell->fn = explosive_rune_spell;
				break;
			}
			case RACE_HALF_GIANT:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 20;
				spell->cost = 10;
				spell->fail = calculate_fail_rate(20, 70, p_ptr->stat_ind[A_STR]);
				spell->fn = stone_to_mud_spell;
				break;
			}
			case RACE_HALF_TITAN:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 15;
				spell->cost = 10;
				spell->fail = calculate_fail_rate(15, 60, p_ptr->stat_ind[A_INT]);
				spell->fn = probing_spell;
				break;
			}
			case RACE_CYCLOPS:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 20;
				spell->cost = 15;
				spell->fail = calculate_fail_rate(20, 50, p_ptr->stat_ind[A_STR]);
				spell->fn = throw_boulder_spell;
				break;
			}
			case RACE_YEEK:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 15;
				spell->cost = 15;
				spell->fail = calculate_fail_rate(15, 50, p_ptr->stat_ind[A_WIS]);
				spell->fn = scare_monster_spell;
				break;
			}
			case RACE_SPECTRE:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 4;
				spell->cost = 6;
				spell->fail = calculate_fail_rate(4, 50, p_ptr->stat_ind[A_INT]);
				spell->fn = scare_monster_spell;
				break;
			}
			case RACE_KLACKON:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 9;
				spell->cost = 9;
				spell->fail = calculate_fail_rate(9, 50, p_ptr->stat_ind[A_DEX]);
				spell->fn = spit_acid_spell;
				break;
			}
			case RACE_KOBOLD:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 12;
				spell->cost = 8;
				spell->fail = calculate_fail_rate(12, 50, p_ptr->stat_ind[A_DEX]);
				spell->fn = poison_dart_spell;
				break;
			}
			case RACE_DARK_ELF:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 1;
				spell->cost = 1;
				spell->fail = calculate_fail_rate(1, 30, p_ptr->stat_ind[A_INT]);
				spell->fn = magic_missile_spell;
				break;
			}
			case RACE_DRACONIAN:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 1;
				spell->cost = 1;
				spell->fail = calculate_fail_rate(1, 70, p_ptr->stat_ind[A_CON]);
				spell->fn = draconian_breath_spell;
				break;
			}
			case RACE_MIND_FLAYER:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 5;
				spell->cost = 3;
				spell->fail = calculate_fail_rate(5, 50, p_ptr->stat_ind[A_INT]);
				spell->fn = mind_blast_spell;
				break;
			}
			case RACE_GOLEM:
			{/*
				spell_info *spell = &spells[ct++];
				spell->level = 20;
				spell->cost = 15;
				spell->fail = calculate_fail_rate(20, 50, p_ptr->stat_ind[A_CON]);
				spell->fn = stone_skin_spell;*/
				break;
			}
			case RACE_SKELETON:
			case RACE_ZOMBIE:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 30;
				spell->cost = 30;
				spell->fail = calculate_fail_rate(30, 70, p_ptr->stat_ind[A_WIS]);
				spell->fn = restore_life_spell;
				break;
			}
			case RACE_VAMPIRE:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 2;
				spell->cost = 1;
				spell->fail = calculate_fail_rate(2, 60, p_ptr->stat_ind[A_CON]);
				spell->fn = vampirism_spell;
				break;
			}
			case RACE_DEMON:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 15;
				spell->cost = 10;
				spell->fail = calculate_fail_rate(15, 70, p_ptr->stat_ind[A_CON]);
				spell->fn = demon_breath_spell;
				break;
			}
			case RACE_IMP:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 9;
				spell->cost = 8;
				spell->fail = calculate_fail_rate(9, 50, p_ptr->stat_ind[A_INT]);
				spell->fn = imp_fire_spell;
				break;
			}
			case RACE_SPRITE:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 12;
				spell->cost = 12;
				spell->fail = calculate_fail_rate(12, 50, p_ptr->stat_ind[A_INT]);
				spell->fn = sleeping_dust_spell;
				break;
			}
			case RACE_KUTAR:
			{
				spell_info *spell = &spells[ct++];
				spell->level = 20;
				spell->cost = 15;
				spell->fail = calculate_fail_rate(20, 70, p_ptr->stat_ind[A_CHR]);
				spell->fn = kutar_expand_spell;
				break;
			}
		case RACE_ANDROID:
			if (p_ptr->lev < 10)
			{
				spell_info *spell = &spells[ct++];
				spell->level = 1;
				spell->cost = 7;
				spell->fail = calculate_fail_rate(1, 30, p_ptr->stat_ind[A_STR]);
				spell->fn = android_ray_gun_spell;
			}
			else if (p_ptr->lev < 25)
			{
				spell_info *spell = &spells[ct++];
				spell->level = 10;
				spell->cost = 13;
				spell->fail = calculate_fail_rate(10, 30, p_ptr->stat_ind[A_STR]);
				spell->fn = android_blaster_spell;
			}
			else if (p_ptr->lev < 35)
			{
				spell_info *spell = &spells[ct++];
				spell->level = 25;
				spell->cost = 26;
				spell->fail = calculate_fail_rate(25, 40, p_ptr->stat_ind[A_STR]);
				spell->fn = android_bazooka_spell;
			}
			else if (p_ptr->lev < 45)
			{
				spell_info *spell = &spells[ct++];
				spell->level = 35;
				spell->cost = 40;
				spell->fail = calculate_fail_rate(35, 50, p_ptr->stat_ind[A_STR]);
				spell->fn = android_beam_cannon_spell;
			}
			else
			{
				spell_info *spell = &spells[ct++];
				spell->level = 45;
				spell->cost = 60;
				spell->fail = calculate_fail_rate(45, 70, p_ptr->stat_ind[A_STR]);
				spell->fn = android_rocket_spell;
			}
			break;
		}
	}

	return ct;
}

