#include "angband.h"

void _kiss_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Kiss", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attempt to charm an adjacent monster.", ""));
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev * 2);
		break;
	case SPELL_CAST:
	{
		int y, x, dir = 0, m_idx;
		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		m_idx = cave[y][x].m_idx;
		if (m_idx)
		{
			monster_type *m_ptr = &m_list[m_idx];
			char desc[MAX_NLEN];
			monster_desc(desc, m_ptr, 0);
			if (mon_save_p(m_ptr->r_idx, A_CHR))
			{
				set_monster_csleep(m_idx, 0);
				if (is_hostile(m_ptr))
				{
					switch (randint1(10))
					{
					case 1:
						msg_format("%^s says 'Impudent Strumpet!'", desc);
						break;
					case 2:
						msg_format("%^s says 'Ewwww! Gross!!'", desc);
						break;
					case 3:
						msg_format("%^s says 'You ain't my type!'", desc);
						break;
					default:
						msg_format("%^s resists your charms.", desc);
					}
				}
				else
					msg_format("%^s ignores you.", desc);
			}
			else
			{
				if (is_pet(m_ptr))
					msg_format("%^s slobbers on you affectionately.", desc);
				else if (is_friendly(m_ptr))
				{
					set_pet(m_ptr);
					msg_format("%^s is charmed!", desc);
				}
				else
				{
					set_friendly(m_ptr);
					msg_format("%^s suddenly becomes friendly.", desc);
				}
			}
			var_set_bool(res, TRUE);
		}
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _demeter_clw_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cure Wounds", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Heals cut and HP a little.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(p_ptr->lev/7 + 1, 10, 0));
		break;
	case SPELL_CAST:
		hp_player(damroll(p_ptr->lev/7 + 1, 10));
		set_cut(p_ptr->cut - 10, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

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

void _shine_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Shine", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Generates a large ball of sunlight.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, p_ptr->lev * 6));
		break;
	case SPELL_CAST:
		fire_ball(GF_LITE, 0, p_ptr->lev * 6 * 2, 5);
		var_set_bool(res, TRUE);
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

/****************************************************************
 * Demigod
 ****************************************************************/

void _demigod_gain_level(int new_level)
{
	if (new_level >= 20)
	{
		if (p_ptr->demigod_power[0] < 0)
		{
			int idx = mut_gain_choice(mut_human_pred);
			mut_lock(idx);
			p_ptr->demigod_power[0] = idx;
		}
	}

	if (new_level >= 40)
	{
		if (p_ptr->demigod_power[1] < 0)
		{
			int idx = mut_gain_choice(mut_human_pred);
			mut_lock(idx);
			p_ptr->demigod_power[1] = idx;
		}
	}
}

void _demigod_calc_bonuses(void)
{
	int i;

	if (p_ptr->mimic_form)
		return;

	for (i = 0; i < 6; i++)
		p_ptr->stat_add[i] += demigod_info[p_ptr->psubrace].adj[i];

	switch (p_ptr->psubrace)
	{
	case DEMIGOD_ZEUS:
		p_ptr->resist_elec = TRUE;
		p_ptr->sh_elec = TRUE;
		p_ptr->levitation = TRUE;
		break;
	case DEMIGOD_POSEIDON:
		p_ptr->resist_acid = TRUE;
		p_ptr->resist_cold = TRUE;
		p_ptr->resist_elec = TRUE;
		/*p_ptr->resist_stun = TRUE; Handled as a hack elsewhere ... */
		break;
	case DEMIGOD_HADES:
		p_ptr->resist_neth = TRUE;
		p_ptr->hold_life = TRUE;
		p_ptr->sustain_con = TRUE;
		p_ptr->skill_sav += 15;
		p_ptr->free_act = TRUE;
		break;
	case DEMIGOD_ATHENA:
		p_ptr->sustain_int = TRUE;
		break;
	case DEMIGOD_ARES:
	{
		int dam = 5 + p_ptr->lev/7;

		p_ptr->sustain_str = TRUE;
		p_ptr->to_a += 10 + p_ptr->lev/5;
		p_ptr->dis_to_a += 10 + p_ptr->lev/5;
		p_ptr->weapon_info[0].to_d += dam;
		p_ptr->weapon_info[1].to_d += dam;
		p_ptr->to_d_m  += dam;
		p_ptr->weapon_info[0].dis_to_d += dam;
		p_ptr->weapon_info[1].dis_to_d += dam;
		p_ptr->skill_sav -= 10;
		p_ptr->skill_stl -= 2;
		break;
	}
	case DEMIGOD_HEPHAESTUS:
		p_ptr->resist_disen = TRUE;
		break;
	case DEMIGOD_HERMES:
		p_ptr->skill_stl += 5;
		p_ptr->pspeed += 2;
		break;
	case DEMIGOD_APOLLO:
		p_ptr->resist_lite = TRUE;
		p_ptr->resist_blind = TRUE;
		/* cf calc_torch in xtra1.c for the 'extra light' */
		break;
	case DEMIGOD_ARTEMIS:
		p_ptr->to_d_b += 5 + p_ptr->lev/7;
		p_ptr->skill_thb += 15;
		p_ptr->sustain_dex = TRUE;
		break;
	case DEMIGOD_HERA:
		p_ptr->spell_cap += 3;
		break;
	case DEMIGOD_DEMETER:
		p_ptr->regenerate = TRUE;
		p_ptr->slow_digest = TRUE;
		p_ptr->resist_time = TRUE;
		break;
	case DEMIGOD_APHRODITE:
		p_ptr->sustain_chr = TRUE;
		break;
	}
}

int _demigod_get_powers(spell_info* spells, int max)
{
	int ct = 0;

	if (p_ptr->mimic_form)
		return 0;

	switch (p_ptr->psubrace)
	{
	case DEMIGOD_APOLLO:
	{
		spell_info *spell = &spells[ct++];
		spell->level = 1;
		spell->cost = 10;
		spell->fail = calculate_fail_rate(spell->level, 70, p_ptr->stat_ind[A_CHR]);
		spell->fn = _shine_spell;

		spell = &spells[ct++];
		spell->level = 5;
		spell->cost = 3;
		spell->fail = calculate_fail_rate(5, 50, p_ptr->stat_ind[A_INT]);
		spell->fn = light_area_spell;

		spell = &spells[ct++];
		spell->level = 12;
		spell->cost = 7;
		spell->fail = calculate_fail_rate(12, 60, p_ptr->stat_ind[A_WIS]);
		spell->fn = ray_of_sunlight_spell;

		break;
	}
	case DEMIGOD_HERA:
	{
		spell_info *spell = &spells[ct++];
		spell->level = 15;
		spell->cost = 0;
		spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_WIS]);
		spell->fn = clear_mind_spell;
		break;
	}
	case DEMIGOD_DEMETER:
	{
		spell_info *spell = &spells[ct++];
		spell->level = 5;
		spell->cost = 0;
		spell->fail = calculate_fail_rate(spell->level, 60, p_ptr->stat_ind[A_WIS]);
		spell->fn = _demeter_clw_spell;
		break;
	}
	case DEMIGOD_ARES:
	{
		spell_info *spell = &spells[ct++];
		spell->level = 10;
		spell->cost = 10;
		spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_STR]);
		spell->fn = berserk_spell;
		break;
	}
	case DEMIGOD_APHRODITE:
	{
		spell_info *spell = &spells[ct++];
		spell->level = 1;
		spell->cost = 10;
		spell->fail = calculate_fail_rate(spell->level, 50, p_ptr->stat_ind[A_CHR]);
		spell->fn = _kiss_spell;
		break;
	}
	}
	return ct;
}
race_t *demigod_get_race_t(void)
{
	static race_t me = {0};
	static bool init = FALSE;

	if (!init)
	{
		/* TODO */
		me.gain_level = _demigod_gain_level;
		me.calc_bonuses = _demigod_calc_bonuses;
		me.get_powers = _demigod_get_powers;
		init = TRUE;
	}

	return &me;
}

/****************************************************************
 * Human
 ****************************************************************/

void _human_gain_level(int new_level)
{
	if (new_level >= 30)
	{
		if (p_ptr->demigod_power[0] < 0)
		{
			int idx = mut_gain_choice(mut_human_pred);
			mut_lock(idx);
			p_ptr->demigod_power[0] = idx;
		}
	}
}

race_t *human_get_race_t(void)
{
	static race_t me = {0};
	static bool init = FALSE;

	if (!init)
	{
		/* TODO */
		me.gain_level = _human_gain_level;
		init = TRUE;
	}

	return &me;
}

/****************************************************************
 * Public Entrypoints
 ****************************************************************/
race_t *get_race_t_aux(int prace)
{
	race_t *result = NULL;

	switch (prace)
	{
	case RACE_HUMAN:
		result = human_get_race_t();
		break;
	case RACE_DEMIGOD:
		result = demigod_get_race_t();
		break;
	}

	return result;
}

race_t *get_race_t(void)
{
	return get_race_t_aux(p_ptr->prace);
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

