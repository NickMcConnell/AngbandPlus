/*
 *  Hooks and Callbacks for various classes
 */

#include "angband.h"

/* Goal: This should be the one and only switch off of p_ptr->pclass in the
   entire system! */
class_t *get_class_t_aux(int pclass, int psubclass)
{
class_t *result = NULL;

	switch (pclass)
	{
	case CLASS_ARCHAEOLOGIST:
		result = archaeologist_get_class_t();
		break;
	case CLASS_BLOOD_KNIGHT:
		result = blood_knight_get_class_t();
		break;
	case CLASS_BLOOD_MAGE:
		result = blood_mage_get_class_t();
		break;
	case CLASS_DUELIST:
		result = duelist_get_class_t();
		break;
	case CLASS_FORCETRAINER:
		result = force_trainer_get_class_t();
		break;
	case CLASS_MINDCRAFTER:
		result = mindcrafter_get_class_t();
		break;
	case CLASS_NECROMANCER:
		result = necromancer_get_class_t();
		break;
	case CLASS_PSION:
		result = psion_get_class_t();
		break;
	case CLASS_RAGE_MAGE:
		result = rage_mage_get_class_t();
		break;
	case CLASS_RUNE_KNIGHT:
		result = rune_knight_get_class_t();
		break;
	case CLASS_SCOUT:
		result = scout_get_class_t();
		break;
	case CLASS_WARLOCK:
		result = warlock_get_class_t(psubclass);
		break;
	case CLASS_WEAPONMASTER:
		result = weaponmaster_get_class_t();
		break;
	case CLASS_WILD_TALENT:
		result = wild_talent_get_class_t();
		break;
	}

	return result;
}

class_t *get_class_t(void)
{
	return get_class_t_aux(p_ptr->pclass, p_ptr->psubclass);
}

caster_info *get_caster_info(void)
{
	caster_info *result = NULL;
	class_t *class_ptr = get_class_t();
	if (class_ptr && class_ptr->caster_info)
		result = (class_ptr->caster_info)();
	return result;
}

/* HACK: This should be handled by the class_t entry point ...
   This is just here while I am refactoring code!!! */
int get_class_powers(spell_info* spells, int max)
{
	int ct = 0;

	switch (p_ptr->pclass)
	{
		case CLASS_WARRIOR:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 40;
			spell->cost = 75;
			spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_DEX]);
			spell->fn = sword_dance_spell;
			break;
		}
		case CLASS_HIGH_MAGE:
			if (p_ptr->realm1 == REALM_HEX)
			{
				spell_info* spell = &spells[ct++];
				spell->level = 1;
				spell->cost = 0;
				spell->fail = 0;
				spell->fn = hex_stop_spelling_spell;
				break;
			}
			/* vvvvvv FALL THRU ON PURPOSE vvvvvvv */
		case CLASS_MAGE:
		case CLASS_SORCERER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 25;
			spell->cost = 1;
			spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_INT]);
			spell->fn = eat_magic_spell;
			break;
		}
		case CLASS_PRIEST:
			if (is_good_realm(p_ptr->realm1))
			{
				spell_info* spell = &spells[ct++];
				spell->level = 35;
				spell->cost = 70;
				spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_WIS]);
				spell->fn = bless_weapon_spell;
			}
			else
			{
				spell_info* spell = &spells[ct++];
				spell->level = 42;
				spell->cost = 40;
				spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_WIS]);
				spell->fn = evocation_spell;
			}
			break;

		case CLASS_ROGUE:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 8;
			spell->cost = 12;
			spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_DEX]);
			spell->fn = panic_hit_spell;

			spell = &spells[ct++];
			spell->level = 20;
			spell->cost = 25;
			spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_DEX]);
			spell->fn = explosive_rune_spell;
			break;
		}

		case CLASS_RANGER:
		case CLASS_SNIPER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 15;
			spell->cost = 20;
			spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_INT]);
			spell->fn = probing_spell;
			break;
		}
		case CLASS_PALADIN:
			if (is_good_realm(p_ptr->realm1))
			{
				spell_info* spell = &spells[ct++];
				spell->level = 30;
				spell->cost = 30;
				spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_WIS]);
				spell->fn = holy_lance_spell;
			}
			else
			{
				spell_info* spell = &spells[ct++];
				spell->level = 30;
				spell->cost = 30;
				spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_WIS]);
				spell->fn = hell_lance_spell;
			}
			break;

		case CLASS_WARRIOR_MAGE:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 25;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_INT]);
			spell->fn = hp_to_sp_spell;

			spell = &spells[ct++];
			spell->level = 25;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_INT]);
			spell->fn = sp_to_hp_spell;
			break;
		}
		case CLASS_CHAOS_WARRIOR:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 40;
			spell->cost = 50;
			spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_INT]);
			spell->fn = confusing_lights_spell;
			break;
		}
		case CLASS_MONK:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 25;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = monk_posture_spell;

			spell = &spells[ct++];
			spell->level = 30;
			spell->cost = 30;
			spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_STR]);
			spell->fn = monk_double_attack_spell;
			break;
		}
		case CLASS_TOURIST:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = take_photo_spell;

			spell = &spells[ct++];
			spell->level = 25;
			spell->cost = 20;
			spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
			spell->fn = identify_fully_spell;
			break;
		}
		case CLASS_IMITATOR:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 30;
			spell->cost = 100;
			spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_DEX]);
			spell->fn = double_revenge_spell;
			break;
		}
		case CLASS_BEASTMASTER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_CHR]);
			spell->fn = dominate_living_I_spell;

			spell = &spells[ct++];
			spell->level = 30;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_CHR]);
			spell->fn = dominate_living_II_spell;
			break;
		}
		case CLASS_ARCHER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = create_ammo_spell;
			break;
		}
		case CLASS_MAGIC_EATER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = absorb_magic_spell;
			break;
		}
		case CLASS_BARD:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = bard_stop_singing_spell;
			break;
		}
		case CLASS_RED_MAGE:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 48;
			spell->cost = 20;
			spell->fail = 0;
			spell->fn = double_magic_spell;
			break;
		}
		case CLASS_SAMURAI:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = samurai_concentration_spell;

			spell = &spells[ct++];
			spell->level = 25;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = samurai_posture_spell;
			break;
		}
		case CLASS_BLUE_MAGE:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = blue_learning_spell;
			break;
		}
		case CLASS_CAVALRY:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 10;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_STR]);
			spell->fn = rodeo_spell;
			break;
		}
		case CLASS_BERSERKER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 10;
			spell->cost = 10;
			spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_DEX]);
			spell->fn = recall_spell;
			break;
		}
		case CLASS_MIRROR_MASTER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 1;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = break_mirrors_spell;

			spell = &spells[ct++];
			spell->level = 30;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_INT]);
			spell->fn = mirror_concentration_spell;
			break;
		}
		case CLASS_SMITH:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 5;
			spell->cost = 15;
			spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_INT]);
			spell->fn = smith_judgment_spell;
			break;
		}
		case CLASS_NINJA:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 20;
			spell->cost = 0;
			spell->fail = 0;
			spell->fn = quick_walk_spell;
			break;
		}
	}

	return ct;
}
