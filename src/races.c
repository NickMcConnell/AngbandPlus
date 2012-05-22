#include "angband.h"


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
		case RACE_AMBERITE:
			result = amberite_get_race_t();
			break;
		case RACE_ANDROID:
			result = android_get_race_t();
			break;
		case RACE_ANGEL:
			result = archon_get_race_t();
			break;
		case RACE_BARBARIAN:
			result = barbarian_get_race_t();
			break;
		case RACE_BEASTMAN:
			result = beastman_get_race_t();
			break;
		case RACE_CYCLOPS:
			result = cyclops_get_race_t();
			break;
		case RACE_DARK_ELF:
			result = dark_elf_get_race_t();
			break;
		case RACE_DEMIGOD:
			result = demigod_get_race_t(psubrace);
			break;
		case RACE_DEMON:
			result = balrog_get_race_t();
			break;
		case RACE_DUNADAN:
			result = dunadan_get_race_t();
			break;
		case RACE_DRACONIAN:
			result = draconian_get_race_t();
			break;
		case RACE_DWARF:
			result = dwarf_get_race_t();
			break;
		case RACE_ENT:
			result = ent_get_race_t();
			break;
		case RACE_GNOME:
			result = gnome_get_race_t();
			break;
		case RACE_GOLEM:
			result = golem_get_race_t();
			break;
		case RACE_HALF_GIANT:
			result = half_giant_get_race_t();
			break;
		case RACE_HALF_OGRE:
			result = half_ogre_get_race_t();
			break;
		case RACE_HALF_TITAN:
			result = half_titan_get_race_t();
			break;
		case RACE_HALF_TROLL:
			result = half_troll_get_race_t();
			break;
		case RACE_HIGH_ELF:
			result = high_elf_get_race_t();
			break;
		case RACE_HOBBIT:
			result = hobbit_get_race_t();
			break;
		case RACE_HUMAN:
			result = human_get_race_t();
			break;
		case RACE_IMP:
			result = imp_get_race_t();
			break;
		case RACE_KLACKON:
			result = klackon_get_race_t();
			break;
		case RACE_KOBOLD:
			result = kobold_get_race_t();
			break;
		case RACE_KUTAR:
			result = kutar_get_race_t();
			break;
		case RACE_MIND_FLAYER:
			result = mindflayer_get_race_t();
			break;
		case RACE_NIBELUNG:
			result = nibelung_get_race_t();
			break;
		case RACE_S_FAIRY:
			result = shadow_fairy_get_race_t();
			break;
		case RACE_SKELETON:
			result = skeleton_get_race_t();
			break;
		case RACE_SNOTLING:
			result = snotling_get_race_t();
			break;
		case RACE_SPECTRE:
			result = spectre_get_race_t();
			break;
		case RACE_SPRITE:
			result = sprite_get_race_t();
			break;
		case RACE_TONBERRY:
			result = tonberry_get_race_t();
			break;
		case RACE_VAMPIRE:
			result = vampire_get_race_t();
			break;
		case RACE_YEEK:
			result = yeek_get_race_t();
			break;
		case RACE_ZOMBIE:
			result = zombie_get_race_t();
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
	}

	return ct;
}

