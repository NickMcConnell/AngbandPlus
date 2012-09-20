#include "angband.h"

/****************************************************************
 * Spells
 ****************************************************************/
static void _kiss_spell(int cmd, variant *res)
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

static void _demeter_clw_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cure Wounds", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Heals cut and HP a little.", ""));
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Decreases cut status by 10 and heals (L/7 + 1)d10 hp.");
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

static void _shine_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Shine", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Generates a large ball of sunlight.", ""));
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Generates a radius 5 ball of light centered on the player. Damage is 6L.");
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

static void _gain_level(int new_level)
{
	if (new_level >= 20)
	{
		if (p_ptr->demigod_power[0] < 0)
		{
			int idx = mut_gain_choice(mut_human_pred);
			mut_lock(idx);
			p_ptr->demigod_power[0] = idx;
		}
		else if (!mut_present(p_ptr->demigod_power[0]))
		{
			mut_gain(p_ptr->demigod_power[0]);
			mut_lock(p_ptr->demigod_power[0]);
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
		else if (!mut_present(p_ptr->demigod_power[1]))
		{
			mut_gain(p_ptr->demigod_power[1]);
			mut_lock(p_ptr->demigod_power[1]);
		}
	}
}

/****************************************************************
 * Aphrodite
 ****************************************************************/
static power_info _aphrodite_powers[] =
{
	{ A_CHR, {1, 10, 50, _kiss_spell}},
	{ -1, {-1, -1, -1, NULL} }
};
static void _aphrodite_calc_bonuses(void)
{
	p_ptr->sustain_chr = TRUE;
}
static int _aphrodite_get_powers(spell_info* spells, int max)
{
	return get_powers_aux(spells, max, _aphrodite_powers);
}
static void _aphrodite_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_SUST_CHR);
}
static void _aphrodite_spoiler_dump(FILE *fff) 
{ 
	spoil_powers_aux(fff, _aphrodite_powers); 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Sustain Charisma\n");
	fprintf(fff, "  * Pet Upkeep Halved\n");
	fprintf(fff, "  * Gains Sexy Swimsuit Bonus (Even if not sexy, or not female).\n");
	fprintf(fff, "  * Sells to Shopkeepers at Full Price\n");
}


/****************************************************************
 * Apollo
 ****************************************************************/
static power_info _apollo_powers[] =
{
	{ A_CHR, {1, 10, 70, _shine_spell}},
	{ A_INT, {5, 3, 50, light_area_spell}},
	{ A_WIS, {12, 7, 60, ray_of_sunlight_spell}},
	{ -1, {-1, -1, -1, NULL} }
};
static void _apollo_calc_bonuses(void)
{
	p_ptr->resist_lite = TRUE;
	p_ptr->resist_blind = TRUE;
	/* cf calc_torch in xtra1.c for the 'extra light' */
}
static int _apollo_get_powers(spell_info* spells, int max)
{
	return get_powers_aux(spells, max, _apollo_powers);
}
static void _apollo_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_RES_BLIND);
	add_flag(flgs, TR_LITE);
}
static void _apollo_get_immunities(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_LITE);
}
static void _apollo_spoiler_dump(FILE *fff) 
{ 
	spoil_powers_aux(fff, _apollo_powers); 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Immune to Light\n");
	fprintf(fff, "  * Resist Blindness\n");
	fprintf(fff, "  * Extra Light (+1 to light radius)\n");
}

/****************************************************************
 * Ares
 ****************************************************************/
static power_info _ares_powers[] =
{
	{ A_STR, {10, 10, 30, berserk_spell}},
	{ -1, {-1, -1, -1, NULL} }
};
static void _ares_calc_bonuses(void)
{
	int dam = 5 + p_ptr->lev/7;
	int ac = 10 + p_ptr->lev/5;

	p_ptr->sustain_str = TRUE;

	p_ptr->to_a += ac;
	p_ptr->dis_to_a += ac;
	
	p_ptr->weapon_info[0].to_d += dam;
	p_ptr->weapon_info[1].to_d += dam;
	p_ptr->to_d_m  += dam;
	p_ptr->weapon_info[0].dis_to_d += dam;
	p_ptr->weapon_info[1].dis_to_d += dam;	
}
static int _ares_get_powers(spell_info* spells, int max)
{
	return get_powers_aux(spells, max, _ares_powers);
}
static void _ares_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_SUST_STR);
}
static void _ares_spoiler_dump(FILE *fff) 
{ 
	spoil_powers_aux(fff, _ares_powers); 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * +5+L/7 to Damage\n");
	fprintf(fff, "  * +10+L/5 to Armor Class\n");
	fprintf(fff, "  * Sustain Strength\n");
	fprintf(fff, "  * Starts as Beginner in All Weapon Skills\n");
}

/****************************************************************
 * Artemis
 ****************************************************************/
static void _artemis_calc_bonuses(void)
{
	p_ptr->to_d_b += 5 + p_ptr->lev/7;
	p_ptr->dis_to_d_b += 5 + p_ptr->lev/7;
	p_ptr->sustain_dex = TRUE;
}
static void _artemis_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_SUST_DEX);
}
static void _artemis_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Sustain Dexterity\n");
	fprintf(fff, "  * +5+L/7 Damage with a Bow\n");
	fprintf(fff, "  * +1+L/12 Shooting Range\n");
	fprintf(fff, "  * Starts as Beginner in All Shooter Skills\n");
	fprintf(fff, "  * Can Master Any Shooter\n");
}

/****************************************************************
 * Athena
 ****************************************************************/
static void _athena_calc_bonuses(void)
{
	p_ptr->sustain_int = TRUE;
}
static void _athena_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_SUST_INT);
}
static void _athena_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * -5%% Spell Fail Rates\n");
	fprintf(fff, "  * -1%% Spell Minimum Fails\n");
	fprintf(fff, "  * Spells that Fail Cost 0sp\n");
	fprintf(fff, "  * Sustain Intelligence\n");
}

/****************************************************************
 * Demeter
 ****************************************************************/
static power_info _demeter_powers[] =
{
	{ A_WIS, {5, 0, 60, _demeter_clw_spell}},
	{ -1, {-1, -1, -1, NULL} }
};
static void _demeter_calc_bonuses(void)
{
	p_ptr->regenerate = TRUE;
	p_ptr->slow_digest = TRUE;
	p_ptr->resist_time = TRUE;
}
static int _demeter_get_powers(spell_info* spells, int max)
{
	return get_powers_aux(spells, max, _demeter_powers);
}
static void _demeter_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_REGEN);
	add_flag(flgs, TR_SLOW_DIGEST);
	add_flag(flgs, TR_RES_TIME);
}
static void _demeter_spoiler_dump(FILE *fff) 
{ 
	spoil_powers_aux(fff, _demeter_powers); 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Regeneration\n");
	fprintf(fff, "  * Slow Digestion\n");
	fprintf(fff, "  * Resist Time\n");
}

/****************************************************************
 * Hades
 ****************************************************************/
static void _hades_calc_bonuses(void)
{
	p_ptr->resist_neth = TRUE;
	p_ptr->hold_life = TRUE;
	p_ptr->sustain_con = TRUE;
	p_ptr->free_act = TRUE;
}
static void _hades_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_RES_NETHER);
	add_flag(flgs, TR_HOLD_LIFE);
	add_flag(flgs, TR_SUST_CON);
	add_flag(flgs, TR_FREE_ACT);
}
static void _hades_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Resist Nether\n");
	fprintf(fff, "  * Hold Life\n");
	fprintf(fff, "  * Sustain Constitution\n");
	fprintf(fff, "  * Free Action\n");
}

/****************************************************************
 * Hephaestus
 ****************************************************************/
static void _hephaestus_calc_bonuses(void)
{
	p_ptr->resist_disen = TRUE;
}
static void _hephaestus_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_RES_DISEN);
}
static void _hephaestus_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Resist Disenchantment\n");
	fprintf(fff, "  * Acid Proof: Equipment will not be damaged by acid\n");
	fprintf(fff, "  * Fell Branding: Any brand/slay gain +1 dice\n");
}

/****************************************************************
 * Hera
 ****************************************************************/
static power_info _hera_powers[] =
{
	{ A_WIS, {15, 0, 30, clear_mind_spell}},
	{ -1, {-1, -1, -1, NULL} }
};
static void _hera_calc_bonuses(void)
{
	p_ptr->spell_cap += 3;
}
static int _hera_get_powers(spell_info* spells, int max)
{
	return get_powers_aux(spells, max, _hera_powers);
}
static void _hera_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_SPELL_CAP);
}
static void _hera_spoiler_dump(FILE *fff) 
{ 
	spoil_powers_aux(fff, _hera_powers); 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * +%d%% Spell Capacity\n", spell_cap_aux(100, 3) - 100);
	fprintf(fff, "  * Resist Mana Drain: Player gets a saving throw to resist all mana draining attacks. This save succeeds if 1d100 > ML-(2*WIS).\n");
}

/****************************************************************
 * Hermes
 ****************************************************************/
static void _hermes_calc_bonuses(void)
{
	p_ptr->pspeed += 2;
}
static void _hermes_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_SPEED);
}
static void _hermes_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * +2 Speed\n");
	fprintf(fff, "  * Player Cannot be Slowed\n");
}

/****************************************************************
 * Poseidon
 ****************************************************************/
static void _poseidon_calc_bonuses(void)
{
	p_ptr->resist_acid = TRUE;
	p_ptr->resist_cold = TRUE;
	p_ptr->resist_elec = TRUE;
	/*p_ptr->resist_stun = TRUE; Handled as a hack elsewhere ... */
}
static void _poseidon_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_RES_ACID);
	add_flag(flgs, TR_RES_COLD);
	add_flag(flgs, TR_RES_ELEC);
}
static void _poseidon_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Resist Acid\n");
	fprintf(fff, "  * Resist Cold\n");
	fprintf(fff, "  * Resist Electricity\n");
	fprintf(fff, "  * Immune to Stuns\n");
	fprintf(fff, "  * Melt Armor: Every melee or ranged hit permanently decreases monster AC by 4, unless monster saves (ML + 1d100 > 2*L + Dex)\n");
}

/****************************************************************
 * Zeus
 ****************************************************************/
static void _zeus_calc_bonuses(void)
{
	p_ptr->resist_elec = TRUE;
	p_ptr->sh_elec = TRUE;
	p_ptr->levitation = TRUE;
}
static void _zeus_get_flags(u32b flgs[TR_FLAG_SIZE])
{
	add_flag(flgs, TR_RES_ELEC);
	add_flag(flgs, TR_SH_ELEC);
	add_flag(flgs, TR_LEVITATION);
}
static void _zeus_spoiler_dump(FILE *fff) 
{ 
	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "  * Resist Electricity\n");
	fprintf(fff, "  * Aura of Electricity\n");
	fprintf(fff, "  * Levitation\n");
	fprintf(fff, "  * All Stat Boosts are \"Double\".\n");
}

race_t *demigod_get_race_t(int psubrace)
{
	static race_t me = {0};
	static bool init = FALSE;
	static int subrace_init = -1;

	/* static info never changes */
	if (!init)
	{
		me.name = "Demigod";
		me.desc = "The term demigod is commonly used to describe mythological figures whose one "
					"parent was a god and whose other parent was human; as such, demigods are "
					"human-god hybrids and are quite powerful. Demigods receive special abilities "
					"depending on their parentage.";
		
		me.infra = 0;

		me.gain_level = _gain_level;
		init = TRUE;
	}

	if (subrace_init != psubrace)
	{
		int i;
		
		/* Reset to Minor God */
		me.stats[A_STR] =  1;
		me.stats[A_INT] =  1;
		me.stats[A_WIS] =  1;
		me.stats[A_DEX] =  1;
		me.stats[A_CON] =  1;
		me.stats[A_CHR] =  1;
		
		me.skills.dis = 4;
		me.skills.dev = 5;
		me.skills.sav = 3;
		me.skills.stl = -2;
		me.skills.srh = 3;
		me.skills.fos = 13;
		me.skills.thn = 15;
		me.skills.thb = 10;

		me.hd = 10;
		me.exp = 220;

		me.calc_bonuses = NULL;
		me.get_powers = NULL;
		me.get_flags = NULL;
		me.get_immunities = NULL;
		me.spoiler_dump = NULL;

		/* Override with New Type */
		switch (psubrace)
		{
		case DEMIGOD_APHRODITE:
			me.stats[A_CHR] += 2;
			me.exp += 60;
			me.calc_bonuses = _aphrodite_calc_bonuses;
			me.get_powers = _aphrodite_get_powers;
			me.get_flags = _aphrodite_get_flags;
			me.spoiler_dump = _aphrodite_spoiler_dump;
			break;
		case DEMIGOD_APOLLO:
			me.exp += 80;
			me.calc_bonuses = _apollo_calc_bonuses;
			me.get_powers = _apollo_get_powers;
			me.get_flags = _apollo_get_flags;
			me.get_immunities = _apollo_get_immunities;
			me.spoiler_dump = _apollo_spoiler_dump;
			break;
		case DEMIGOD_ARES:
			me.stats[A_STR] += 2;
			me.skills.sav -= 10;
			me.skills.stl -= 2;
			me.exp += 100;
			me.calc_bonuses = _ares_calc_bonuses;
			me.get_powers = _ares_get_powers;
			me.get_flags = _ares_get_flags;
			me.spoiler_dump = _ares_spoiler_dump;
			break;
		case DEMIGOD_ARTEMIS:
			me.stats[A_DEX] += 2;
			me.skills.thb += 15;
			me.exp += 80;
			me.calc_bonuses = _artemis_calc_bonuses;
			me.get_flags = _artemis_get_flags;
			me.spoiler_dump = _artemis_spoiler_dump;
			break;
		case DEMIGOD_ATHENA:
			me.stats[A_INT] += 2;
			me.exp += 100;
			me.calc_bonuses = _athena_calc_bonuses;
			me.get_flags = _athena_get_flags;
			me.spoiler_dump = _athena_spoiler_dump;
			break;
		case DEMIGOD_DEMETER:
			me.exp += 60;
			me.calc_bonuses = _demeter_calc_bonuses;
			me.get_powers = _demeter_get_powers;
			me.get_flags = _demeter_get_flags;
			me.spoiler_dump = _demeter_spoiler_dump;
			break;
		case DEMIGOD_HADES:
			me.stats[A_CON] += 3;
			me.skills.sav += 15;
			me.hd += 3;
			me.exp += 120;
			me.calc_bonuses = _hades_calc_bonuses;
			me.get_flags = _hades_get_flags;
			me.spoiler_dump = _hades_spoiler_dump;
			break;
		case DEMIGOD_HEPHAESTUS:
			me.exp += 80;
			me.calc_bonuses = _hephaestus_calc_bonuses;
			me.get_flags = _hephaestus_get_flags;
			me.spoiler_dump = _hephaestus_spoiler_dump;
			break;
		case DEMIGOD_HERA:
			me.stats[A_WIS] += 2;
			me.exp += 60;
			me.calc_bonuses = _hera_calc_bonuses;
			me.get_powers = _hera_get_powers;
			me.get_flags = _hera_get_flags;
			me.spoiler_dump = _hera_spoiler_dump;
			break;
		case DEMIGOD_HERMES:
			me.skills.stl += 5;
			me.exp += 100;
			me.calc_bonuses = _hermes_calc_bonuses;
			me.get_flags = _hermes_get_flags;
			me.spoiler_dump = _hermes_spoiler_dump;
			break;
		case DEMIGOD_POSEIDON:
			me.stats[A_STR] += 1;
			me.stats[A_DEX] += 1;
			me.exp += 120;
			me.calc_bonuses = _poseidon_calc_bonuses;
			me.get_flags = _poseidon_get_flags;
			me.spoiler_dump = _poseidon_spoiler_dump;
			break;
		case DEMIGOD_ZEUS:
			for (i = 0; i < 6; i++)
				me.stats[i]++;
			me.exp += 120;
			me.calc_bonuses = _zeus_calc_bonuses;
			me.get_flags = _zeus_get_flags;
			me.spoiler_dump = _zeus_spoiler_dump;
			break;
		}
		
		subrace_init = psubrace;
	}

	return &me;
}


