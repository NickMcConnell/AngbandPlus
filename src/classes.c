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
	case CLASS_DUELIST:
		result = duelist_get_class_t();
		break;
	}

	return result;
}

class_t *get_class_t(void)
{
	return get_class_t_aux(p_ptr->pclass, p_ptr->psubclass);
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
		case CLASS_MINDCRAFTER:
		case CLASS_FORCETRAINER:
		{
			spell_info* spell = &spells[ct++];
			spell->level = 15;
			spell->cost = 0;
			spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_WIS]);
			spell->fn = clear_mind_spell;
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
			spell->cost = (p_ptr->lev+3)/4;
			spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_CHR]);
			spell->fn = dominate_living_I_spell;

			spell = &spells[ct++];
			spell->level = 30;
			spell->cost = (p_ptr->lev+20)/2;
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
	}

	return ct;
}
/*
		case CLASS_RED_MAGE:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "連続魔");
	#else
			strcpy(power_desc[num].name, "Double Magic");
	#endif

			power_desc[num].level = 48;
			power_desc[num].cost = 20;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 0;
			power_desc[num++].number = -3;
			break;
		}
		case CLASS_SAMURAI:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "気合いため");
	#else
			strcpy(power_desc[num].name, "Concentration");
	#endif

			power_desc[num].level = 1;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_WIS;
			power_desc[num].fail = 0;
			power_desc[num++].number = -3;
	#ifdef JP
	strcpy(power_desc[num].name, "型");
	#else
			strcpy(power_desc[num].name, "Assume a Posture");
	#endif

			power_desc[num].level = 25;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_DEX;
			power_desc[num].fail = 0;
			power_desc[num++].number = -4;
			break;
		}
		case CLASS_BLUE_MAGE:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "ラーニング");
	#else
			strcpy(power_desc[num].name, "Learning");
	#endif

			power_desc[num].level = 1;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 0;
			power_desc[num++].number = -3;
			break;
		}
		case CLASS_CAVALRY:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "荒馬ならし");
	#else
			strcpy(power_desc[num].name, "Rodeo");
	#endif

			power_desc[num].level = 10;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_STR;
			power_desc[num].fail = 10;
			power_desc[num++].number = -3;
			break;
		}
		case CLASS_BERSERKER:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "帰還");
	#else
			strcpy(power_desc[num].name, "Recall");
	#endif

			power_desc[num].level = 10;
			power_desc[num].cost = 10;
			power_desc[num].stat = A_DEX;
			power_desc[num].fail = 20;
			power_desc[num++].number = -3;
			break;
		}
		case CLASS_MIRROR_MASTER:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "鏡割り");
	#else
			strcpy(power_desc[num].name, "Break Mirrors");
	#endif

			power_desc[num].level = 1;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 0;
			power_desc[num++].number = -3;
	#ifdef JP
	strcpy(power_desc[num].name, "静水");
	#else
			strcpy(power_desc[num].name, "Mirror Concentration");
	#endif

			power_desc[num].level = 30;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 20;
			power_desc[num++].number = -4;
			break;
		}
		case CLASS_SMITH:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "目利き");
	#else
			strcpy(power_desc[num].name, "Judgment");
	#endif

			power_desc[num].level = 5;
			power_desc[num].cost = 15;
			power_desc[num].stat = A_INT;
			power_desc[num].fail = 20;
			power_desc[num++].number = -3;
			break;
		}
		case CLASS_NINJA:
		{
	#ifdef JP
	strcpy(power_desc[num].name, "速駆け");
	#else
			strcpy(power_desc[num].name, "Quick Walk");
	#endif

			power_desc[num].level = 20;
			power_desc[num].cost = 0;
			power_desc[num].stat = A_DEX;
			power_desc[num].fail = 0;
			power_desc[num++].number = -3;
			break;
		}
		case CLASS_WARLOCK:
		{
			switch (p_ptr->psubclass)
			{
			case PACT_UNDEAD:
				strcpy(power_desc[num].name, "Satisfy Hunger");
				power_desc[num].level = 5;
				power_desc[num].cost = 5;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -3;

				strcpy(power_desc[num].name, "Restore Life");
				power_desc[num].level = 20;
				power_desc[num].cost = 20;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 30;
				power_desc[num++].number = -4;

				strcpy(power_desc[num].name, "Wraithform");
				power_desc[num].level = 50;
				power_desc[num].cost = 100;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 40;
				power_desc[num++].number = -5;
				break;

			case PACT_DRAGON:
				strcpy(power_desc[num].name, "Detect Objects");
				power_desc[num].level = 5;
				power_desc[num].cost = 5;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -3;

				strcpy(power_desc[num].name, "Heroism");
				power_desc[num].level = 15;
				power_desc[num].cost = 10;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -4;

				strcpy(power_desc[num].name, "Identify");
				power_desc[num].level = 20;
				power_desc[num].cost = 20;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -5;

				strcpy(power_desc[num].name, "Stone Skin");
				power_desc[num].level = 35;
				power_desc[num].cost = 40;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 8;
				power_desc[num++].number = -6;

				strcpy(power_desc[num].name, "Dragon Breath");
				power_desc[num].level = 50;
				power_desc[num].cost = 30;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 12;
				power_desc[num++].number = -7;
				break;

			case PACT_ANGEL:
				strcpy(power_desc[num].name, "Light Area");
				power_desc[num].level = 5;
				power_desc[num].cost = 5;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 6;
				power_desc[num++].number = -3;

				strcpy(power_desc[num].name, "Remove Curse");
				power_desc[num].level = 20;
				power_desc[num].cost = 20;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 20;
				power_desc[num++].number = -4;

				strcpy(power_desc[num].name, "Earthquake");
				power_desc[num].level = 30;
				power_desc[num].cost = 10;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 12;
				power_desc[num++].number = -5;

				strcpy(power_desc[num].name, "Protection from Evil");
				power_desc[num].level = 35;
				power_desc[num].cost = 40;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 12;
				power_desc[num++].number = -6;

				strcpy(power_desc[num].name, "Destruction");
				power_desc[num].level = 35;
				power_desc[num].cost = 40;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 20;
				power_desc[num++].number = -7;

				strcpy(power_desc[num].name, "Invulnerability");
				power_desc[num].level = 50;
				power_desc[num].cost = 100;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 40;
				power_desc[num++].number = -8;
				break;

			case PACT_DEMON:
				strcpy(power_desc[num].name, "Phase Door");
				power_desc[num].level = 5;
				power_desc[num].cost = 5;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -3;

				strcpy(power_desc[num].name, "Teleport");
				power_desc[num].level = 20;
				power_desc[num].cost = 10;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 12;
				power_desc[num++].number = -4;

				strcpy(power_desc[num].name, "Teleport Level");
				power_desc[num].level = 30;
				power_desc[num].cost = 20;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 15;
				power_desc[num++].number = -5;

				strcpy(power_desc[num].name, "Recharge");
				power_desc[num].level = 35;
				power_desc[num].cost = 40;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 20;
				power_desc[num++].number = -6;
				break;

			case PACT_ABERRATION:
				strcpy(power_desc[num].name, "Detect Monsters");
				power_desc[num].level = 5;
				power_desc[num].cost = 5;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -3;

				strcpy(power_desc[num].name, "Detect Doors and Stairs");
				power_desc[num].level = 20;
				power_desc[num].cost = 10;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -4;

				strcpy(power_desc[num].name, "Polymorph Self");
				power_desc[num].level = 30;
				power_desc[num].cost = 30;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 20;
				power_desc[num++].number = -5;

				strcpy(power_desc[num].name, "Magic Mapping");
				power_desc[num].level = 35;
				power_desc[num].cost = 20;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 10;
				power_desc[num++].number = -6;

				strcpy(power_desc[num].name, "Dimension Door");
				power_desc[num].level = 50;
				power_desc[num].cost = 20;
				power_desc[num].stat = A_CHR;
				power_desc[num].fail = 12;
				power_desc[num++].number = -7;
				break;
			}
			break;
		}
	}
	return ct;
}
*/