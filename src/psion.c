#include "angband.h"

/* Chosen power level for the current casting.  Set in psion_spell().
*/
static int _power = 1;

int psion_power(void) { return _power; }

/* Magic Number Indices 
	p_ptr->magic_num1 functions as a timer for the effect.
	p_ptr->magic_num2 remembers the power of the effect.
*/
#define _WEAPON_GRAFT 0
#define _CLARITY      1
#define _BLENDING     2
#define _SHIELDING    3
#define _COMBAT		  4
#define _SPEED		  5
#define _BACKLASH	  6
#define _FORTRESS	  7
#define _MINDSPRING	  8
#define _FORESIGHT	  9

bool psion_weapon_graft(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_WEAPON_GRAFT] > 0) return TRUE;
	return FALSE;
}

bool psion_clarity(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_CLARITY] > 0) return TRUE;
	return FALSE;
}

bool psion_blending(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_BLENDING] > 0) return TRUE;
	return FALSE;
}

bool psion_shielding(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_SHIELDING] > 0) return TRUE;
	return FALSE;
}

bool psion_combat(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_COMBAT] > 0) return TRUE;
	return FALSE;
}

bool psion_speed(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_SPEED] > 0) return TRUE;
	return FALSE;
}

bool psion_backlash(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_BACKLASH] > 0) return TRUE;
	return FALSE;
}

int psion_backlash_dam(int dam)
{
	if (psion_backlash())
		dam = dam * (10 + 30*p_ptr->magic_num2[_BACKLASH]) / 100;
	return dam;
}

bool psion_mental_fortress(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_FORTRESS] > 0) return TRUE;
	return FALSE;
}

bool psion_mindspring(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_MINDSPRING] > 0) return TRUE;
	return FALSE;
}

void psion_do_mindspring(int energy)
{
	if (!psion_mindspring()) return;
	p_ptr->csp += 25*p_ptr->magic_num2[_MINDSPRING] * energy / 100;
	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}
	p_ptr->redraw |= PR_MANA;
}


bool psion_foresight(void)
{
	if (p_ptr->pclass != CLASS_PSION) return FALSE;
	if (p_ptr->magic_num1[_FORESIGHT] > 0) return TRUE;
	return FALSE;
}

bool psion_check_foresight(void)
{
	if (!psion_foresight()) return FALSE;
	if (randint1(100) <= 13*p_ptr->magic_num2[_FORESIGHT]) 
	{
		msg_print("You saw that one coming!");
		return TRUE;
	}
	return FALSE;
}

bool psion_mon_save_p(int r_idx, int power)
{
	int pl = p_ptr->lev;
	int ml = r_info[r_idx].level;
	int s = p_ptr->stat_ind[A_INT] + 3;

	if (ml + randint1(100) > pl + s + power*14) return TRUE;
	return FALSE;
}

bool psion_process_monster(int m_idx)
{
	bool result = FALSE;
	bool fear = FALSE;
	monster_type *m_ptr = &m_list[m_idx];
	if (m_ptr->ego_whip_ct)
	{
		char m_name[255];

		monster_desc(m_name, m_ptr, 0);

		if (psion_mon_save_p(m_ptr->r_idx, m_ptr->ego_whip_pow))
		{
			msg_format("%^s shakes off your ego whip!", m_name);
			m_ptr->ego_whip_ct = 0;
			m_ptr->ego_whip_pow = 0;
		}
		else
		{
			msg_format("Your ego whip lashes %s!", m_name);
			result = mon_take_hit(m_idx, 25*m_ptr->ego_whip_pow, &fear, NULL);
			m_ptr->ego_whip_ct--;
			if (!m_ptr->ego_whip_ct)
				msg_format("Your ego whip on %s disappears.", m_name);
		}
	}
	return result;
}

/****************************************************************
 * Spells
 ****************************************************************/

static void _brain_smash_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Brain Smash");
		break;
	case SPELL_DESC:
		var_set_string(res, "Pummel the minds of your foes.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_radius(1));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		fire_ball(
			GF_PSI_BRAIN_SMASH, 
			dir, 
			_power,
			1
		);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _combat_transformation_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Combat Transformation");
		break;
	case SPELL_DESC:
		var_set_string(res, "For a short while, you focus your mental powers on effective combat.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_COMBAT])
		{
			msg_print("You are already transformed into a fighting machine.");
			return;
		}
		msg_print("You transform into a fighting machine!");
		p_ptr->magic_num1[_COMBAT] = spell_power(_power * 20);
		p_ptr->magic_num2[_COMBAT] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _ego_whip_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Ego Whip");
		break;
	case SPELL_DESC:
		var_set_string(res, "Lash out against a single monster with psychic energy.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(_power*25)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		fire_ball(
			GF_PSI_EGO_WHIP, 
			dir, 
			spell_power(_power*25),
			0
		);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _energy_blast_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Energy Blast");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an elemental ball.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(spell_power(_power*4), 6, 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		int type = GF_FIRE;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		switch (_power)
		{
		case 1: type = GF_FIRE; break;
		case 2: type = GF_COLD; break;
		case 3: type = GF_POIS; break;
		case 4: type = GF_ACID; break;
		case 5: type = GF_ELEC; break;
		}

		fire_ball(
			type, 
			dir, 
			spell_power(damroll(_power*4, 6)),
			spell_power((1 + _power)/2)
		);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _graft_weapon_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Graft Weapon");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fuses your melee weapon to your arms and gain combat bonuses. For the duration of this power, you cannot unequip/swap your weapon.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_WEAPON_GRAFT])
		{
			msg_print("Your weapon is already grafted!");
			return;
		}
		msg_print("Your weapon fuses to your arm!");
		p_ptr->magic_num1[_WEAPON_GRAFT] = spell_power(12*_power);
		p_ptr->magic_num2[_WEAPON_GRAFT] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _mana_thrust_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Mana Thrust");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires a bolt of pure mana.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(spell_power(_power*4), 6, 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		if (_power == 5)
			fire_beam(GF_MANA, dir, spell_power(damroll(_power*4, 6)));
		else
			fire_bolt(GF_MANA, dir, spell_power(damroll(_power*4, 6)));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _mental_fortress_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Mental Fortress");
		break;
	case SPELL_DESC:
		var_set_string(res, "For a short time, you become resistant to anti-magic, dispel magic and any attack that drains mana.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_FORTRESS])
		{
			msg_print("You already have a mental fortress.");
			return;
		}
		msg_print("You erect a mental fortress.");
		p_ptr->magic_num1[_FORTRESS] = spell_power(_power);
		p_ptr->magic_num2[_FORTRESS] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _mindspring_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Mindspring");
		break;
	case SPELL_DESC:
		var_set_string(res, "For a short time, you regain mana with every action.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_MINDSPRING])
		{
			msg_print("Your mindspring is already flowing.");
			return;
		}
		msg_print("Your mindspring flows.");
		p_ptr->magic_num1[_MINDSPRING] = spell_power(_power * 3);
		p_ptr->magic_num2[_MINDSPRING] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_backlash_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Backlash");
		break;
	case SPELL_DESC:
		var_set_string(res, "For a short while, monsters are damaged whenever they hurt you.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_BACKLASH])
		{
			msg_print("Your psionic revenge is already active.");
			return;
		}
		msg_print("You contemplate revenge!");
		p_ptr->magic_num1[_BACKLASH] = spell_power(_power * 5);
		p_ptr->magic_num2[_BACKLASH] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_blending_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Blending");
		break;
	case SPELL_DESC:
		var_set_string(res, "You will temporarily blend into your surroundings, gaining increased stealth.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_BLENDING])
		{
			msg_print("You are already blending into your surroundings.");
			return;
		}
		msg_print("You blending into your surroundings.");
		p_ptr->magic_num1[_BLENDING] = spell_power(_power * 12);
		p_ptr->magic_num2[_BLENDING] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_clarity_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Clarity");
		break;
	case SPELL_DESC:
		var_set_string(res, "For the duration of this power, you gain increased mental focus.  Your psionic powers become cheaper to cast.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_CLARITY])
		{
			msg_print("Your mind is already focused.");
			return;
		}
		msg_print("You focus your mind.");
		p_ptr->magic_num1[_CLARITY] = spell_power(_power + 3);
		p_ptr->magic_num2[_CLARITY] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void _psionic_crafting_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Psionic Crafting", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to enchant a weapon, ammo or armor.");
		break;
	case SPELL_CAST:
	{
		int         item;
		bool        okay = FALSE;
		object_type *o_ptr;
		char        o_name[MAX_NLEN];

		var_set_bool(res, FALSE);

		item_tester_hook = object_is_weapon_armour_ammo;
		item_tester_no_ryoute = TRUE;

		if (!get_item(&item, "Enchant which item? ", "You have nothing to enchant.", (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

		if (item >= 0)
			o_ptr = &inventory[item];
		else
			o_ptr = &o_list[0 - item];

		object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

		if (_power == 5 && object_is_nameless(o_ptr) && object_is_melee_weapon(o_ptr))
		{
			/* TODO: Ego armour ... */
			brand_weapon_aux(-1, item);
		}
		else
		{
			if (object_is_weapon_ammo(o_ptr))
			{
				if (enchant(o_ptr, randint0(4) + 1, ENCH_TOHIT | ENCH_PSI_HACK)) okay = TRUE;
				if (enchant(o_ptr, randint0(4) + 1, ENCH_TODAM | ENCH_PSI_HACK)) okay = TRUE;
			}
			else
			{
				if (enchant(o_ptr, randint0(3) + 2, ENCH_TOAC | ENCH_PSI_HACK)) okay = TRUE;			
			}

			msg_format("%s %s glow%s brightly!",
				   ((item >= 0) ? "Your" : "The"), o_name,
				   ((o_ptr->number > 1) ? "" : "s"));

			if (!okay)
			{
				if (flush_failure) flush();
				msg_print("The enchantment failed.");
				if (one_in_(3)) chg_virtue(V_ENCHANT, -1);
			}
			else
				chg_virtue(V_ENCHANT, 1);

			calc_android_exp();
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_foresight_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Foresight");
		break;
	case SPELL_DESC:
		var_set_string(res, "For a short while, you can see the future and may be able to avoid damage.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_FORESIGHT])
		{
			msg_print("Your foresight is already active.");
			return;
		}
		msg_print("You see the future!");
		p_ptr->magic_num1[_FORESIGHT] = spell_power(_power * 2);
		p_ptr->magic_num2[_FORESIGHT] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_healing_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Healing");
		break;
	case SPELL_DESC:
		var_set_string(res, "Use your mental powers to repair your body.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_heal(0, 0, spell_power(120*_power - 50)));
		break;
	case SPELL_CAST:
		hp_player(spell_power(120*_power - 50));
		
		set_blind(0, TRUE);
		set_confused(0, TRUE); /* Probably, @ can't cast this while confused! */
		set_stun(0, TRUE);
		set_cut(0, TRUE);
		set_shero(0,TRUE);

		if (_power >= 3)
			set_image(0, TRUE);

		if (_power == 5)
		{
			do_res_stat(A_STR);
			do_res_stat(A_INT);
			do_res_stat(A_WIS);
			do_res_stat(A_DEX);
			do_res_stat(A_CON);
			do_res_stat(A_CHR);
			restore_level();
		}
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_protection_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Protection");
		break;
	case SPELL_DESC:
		switch (_power)
		{
		case 1:
			var_set_string(res, "Gain temporary resistance to fire.");
			break;
		case 2:
			var_set_string(res, "Gain temporary resistance to fire and cold.");
			break;
		case 3:
			var_set_string(res, "Gain temporary resistance to fire, cold and acid.");
			break;
		case 4:
			var_set_string(res, "Gain temporary resistance to fire, cold, acid and poison.");
			break;
		case 5:
			var_set_string(res, "Gain temporary resistance to fire, cold, acid, poison and electricity.");
			break;
		}
		break;
	case SPELL_CAST:
	{
		int dur = spell_power(10 * _power);
		set_oppose_fire(dur, FALSE);
		if (_power >= 2) set_oppose_cold(dur, FALSE);
		if (_power >= 3) set_oppose_acid(dur, FALSE);
		if (_power >= 4) set_oppose_pois(dur, FALSE);
		if (_power >= 5) set_oppose_elec(dur, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}


static void _psionic_seeing_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Seeing");
		break;
	case SPELL_DESC:
		switch (_power)
		{
		case 1:
			var_set_string(res, "Detects monsters.");
			break;
		case 2:
			var_set_string(res, "Detects monsters, doors, stairs and traps.");
			break;
		case 3:
			var_set_string(res, "Detects monsters, doors, stairs, traps and objects.");
			break;
		case 4:
			var_set_string(res, "Detects monsters, doors, stairs, traps and objects.  Maps nearby area.");
			break;
		case 5:
			var_set_string(res, "Detects monsters, doors, stairs, traps and objects.  Maps nearby area and grants temporary telepathy.");
			break;
		}
		break;
	case SPELL_CAST:
		detect_monsters_normal(DETECT_RAD_DEFAULT);

		if (_power >= 2)
		{
				detect_traps(DETECT_RAD_DEFAULT, TRUE);
				detect_doors(DETECT_RAD_DEFAULT);
				detect_stairs(DETECT_RAD_DEFAULT);
		}

		if (_power >= 3)
			detect_objects_normal(DETECT_RAD_DEFAULT);

		if (_power >= 4)
			map_area(DETECT_RAD_MAP);

		if (_power >= 5)
			set_tim_esp(spell_power(20), FALSE);

		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_shielding_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Shielding");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain physical protection from your mental fortitude.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_SHIELDING])
		{
			msg_print("You already have a psionic shield.");
			return;
		}
		msg_print("You create a psionic shield.");
		p_ptr->magic_num1[_SHIELDING] = spell_power(_power * 8);
		p_ptr->magic_num2[_SHIELDING] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_speed_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Speed");
		break;
	case SPELL_DESC:
		var_set_string(res, "You focus your mental energy on quickness of motion.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->magic_num1[_SPEED])
		{
			msg_print("You are already fast.");
			return;
		}
		msg_print("You gain psionic speed.");
		p_ptr->magic_num1[_SPEED] = spell_power(_power * 10);
		p_ptr->magic_num2[_SPEED] = _power;
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_storm_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Storm");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires a huge ball of mana.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(_power*150 - 50)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		fire_ball(
			GF_PSI_STORM, 
			dir, 
			spell_power(_power*150 - 50),
			4 + _power
		);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _psionic_travel_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Psionic Travel");
		break;
	case SPELL_DESC:
		if (_power == 1)
			var_set_string(res, "Short range teleport.");
		else if (_power == 2)
			var_set_string(res, "Long range teleport.");
		else
			var_set_string(res, "Teleport to specified location.");
		break;
	case SPELL_INFO:
		if (_power == 1)
			var_set_string(res, info_range(10));
		else if (_power == 2)
			var_set_string(res, info_range(p_ptr->lev * 4));
		else
			var_set_string(res, info_range(10*(_power - 2)));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);

		if (_power == 1)
			teleport_player(10, 0L);
		else if (_power == 2)
			teleport_player(p_ptr->lev * 4, 0L);
		else
			dimension_door(10*(_power-2));

		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define MAX_PSION_SPELLS	20

/* Note: Indices into this array are persisted in p_ptr->spell_order.
   Please do not reorder or delete!
*/
static spell_info _spells[MAX_PSION_SPELLS] = 
{
    /*lvl cst fail spell */
	{  1,   2, 30, _mana_thrust_spell },
	{  1,   2, 30, _energy_blast_spell },
	{  1,   5, 40, _psionic_seeing_spell },
	{  1,   6, 40, _graft_weapon_spell },
	{  1,   8, 50, _psionic_clarity_spell },

	{ 10,   7, 40, _psionic_blending_spell },
	{ 10,   9, 50, _psionic_shielding_spell },
	{ 10,   8, 30, _psionic_travel_spell },

	{ 20,   4, 30, _psionic_protection_spell },
	{ 20,  16, 50, _combat_transformation_spell },
	{ 20,  10, 40, _ego_whip_spell },

	{ 30,   8, 40, _psionic_speed_spell },
	{ 30,  14, 50, _psionic_healing_spell },
	{ 30,  13, 60, _brain_smash_spell },

	{ 40,  30, 70, _psionic_crafting_spell },
	{ 40,  25, 60, _psionic_storm_spell },
	{ 40,  20, 50, _psionic_backlash_spell },

	{ 50,  50, 30, _mental_fortress_spell },
	{ 50,  40, 50, _mindspring_spell },
	{ 50,  50, 40, _psionic_foresight_spell },
};


static int _num_spells_learned(void)
{
	int i;
	for (i = 0; i < 64; i++) 
	{
		if (p_ptr->spell_order[i] == 99) break;
	}
	return i;
}

static bool _spell_is_known(int idx)
{
	int i;
	for (i = 0; i < 64; i++) 
	{
		if (p_ptr->spell_order[i] == idx) return TRUE;
		if (p_ptr->spell_order[i] == 99) break;
	}
	return FALSE;
}

static int _num_spells_allowed(void)
{
	int ct = 1;
	if (p_ptr->lev >= 10) ct++;
	if (p_ptr->lev >= 15) ct++;
	if (p_ptr->lev >= 20) ct++;
	if (p_ptr->lev >= 30) ct++;
	if (p_ptr->lev >= 35) ct++;
	if (p_ptr->lev >= 40) ct++;
	if (p_ptr->lev >= 50) ct++;
	return ct;
}

static cptr _power_name(menu_choices choices, int which) {
	switch (which)
	{
	case 0: return "Beginner"; break;
	case 1: return "Intermediate"; break;
	case 2: return "Advanced"; break;
	case 3: return "Mighty"; break;
	case 4: return "Godly"; break;
	}
	return "";
}

static bool _get_power(void)
{
	int i;
	menu_list_t list = { "How Powerful?", NULL, NULL,
						_power_name, NULL, NULL, 
						NULL, 5};

	_power = 1;
	i = menu_choose(&list);
	if (i >= 0 && i <= 4)
	{
		_power = i + 1;
		return TRUE;
	}

	return FALSE;
}

void psion_spell(void)
{
	if (_get_power())
		do_cmd_spell();
}

static cptr _spell_name(menu_choices choices, int which) {
	int idx = ((int*)choices)[which];
	return get_spell_name(_spells[idx].fn);
}

static cptr _spell_desc(menu_choices choices, int which) {
	int idx = ((int*)choices)[which];
	return get_spell_desc(_spells[idx].fn);
}

bool _can_study(void)
{
	int num = _num_spells_allowed() - _num_spells_learned();
    if (num <= 0) return FALSE;
    return TRUE;
}

static void _study(void)
{
	int choices[MAX_PSION_SPELLS];
	int i;
	int ct = 0;
	menu_list_t list = { "Gain which power?", "Browse which power?", NULL,
						_spell_name, _spell_desc, NULL, 
						choices, 0};

	for (i = 0; i < MAX_PSION_SPELLS; i++)
	{
		spell_info *s_ptr = &_spells[i];
		if (s_ptr->level <= p_ptr->lev && !_spell_is_known(i))
		{
			choices[ct] = i;
			ct++;
		}
	}

	list.count = ct;
	for (;;)
	{
		i = menu_choose(&list);
		if (i >= 0)
		{
			char buf[1024];
			int idx = choices[i];
			sprintf(buf, "You will learn %s.  Are you sure?", get_spell_name(_spells[idx].fn));
			if (get_check(buf))
			{
				p_ptr->spell_order[_num_spells_learned()] = idx;
				p_ptr->redraw |= PR_STUDY;
				msg_format("You have gained %s.", get_spell_name(_spells[idx].fn));
				break;
			}
		}
		msg_print("Please make a choice!");
	}
}

static void _gain_level(int new_level)
{
	while (_can_study())
		_study();
}

static void _decrement_counter(int which, cptr off)
{
	if (p_ptr->magic_num1[which])
	{
		p_ptr->magic_num1[which]--;
		if (!p_ptr->magic_num1[which])
		{
			p_ptr->magic_num2[which] = 0;
			msg_print(off);
			p_ptr->update |= PU_BONUS;
			p_ptr->redraw |= PR_STATUS;
		}
	}
}

void psion_decrement_counters(void)
{
	if (p_ptr->pclass != CLASS_PSION) return;

	_decrement_counter(_WEAPON_GRAFT, "Your melee weapon is no longer fused to your arm.");	
	_decrement_counter(_CLARITY, "You lose your mental focus.");	
	_decrement_counter(_BLENDING, "You no longer blend into your surroundings.");	
	_decrement_counter(_SHIELDING, "Your psionic shield disappears.");	
	_decrement_counter(_COMBAT, "Your combat transformation expires.");	
	_decrement_counter(_SPEED, "Your psionic speed fades.");	
	_decrement_counter(_BACKLASH, "Your mental revenge abates.");	
	_decrement_counter(_FORTRESS, "Your mental fortress collapses.");	
	_decrement_counter(_MINDSPRING, "Your mindspring dries up.");	
	_decrement_counter(_FORESIGHT, "Your foresight fades.");	
}

static void _clear_counter(int which, cptr off)
{
	if (p_ptr->magic_num1[which])
	{
		p_ptr->magic_num1[which] = 0;
		p_ptr->magic_num2[which] = 0;
		msg_print(off);
		p_ptr->update |= PU_BONUS;
		p_ptr->redraw |= PR_STATUS;
	}
}

void psion_dispel_player(void)
{
	if (p_ptr->pclass != CLASS_PSION) return;

	_clear_counter(_WEAPON_GRAFT, "Your melee weapon is no longer fused to your arm.");	
	_clear_counter(_CLARITY, "You lose your mental focus.");	
	_clear_counter(_BLENDING, "You no longer blend into your surroundings.");	
	_clear_counter(_SHIELDING, "Your psionic shield disappears.");	
	_clear_counter(_COMBAT, "Your combat transformation expires.");	
	_clear_counter(_SPEED, "Your psionic speed fades.");	
	_clear_counter(_BACKLASH, "Your mental revenge abates.");	
	_clear_counter(_FORTRESS, "Your mental fortress collapses.");	
	_clear_counter(_MINDSPRING, "Your mindspring dries up.");	
	_clear_counter(_FORESIGHT, "Your foresight fades.");	
}

static int _get_powers(spell_info* spells, int max)
{
	int ct = 0;

	spell_info* spell = &spells[ct++];
	spell->level = 15;
	spell->cost = 0;
	spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_INT]);
	spell->fn = clear_mind_spell;

	return ct;
}

static int _get_spells(spell_info* spells, int max)
{
	int i, ct = 0;
	int num = _num_spells_allowed();

	for (i = 0; i < num; i++)
	{
		spell_info *base;

		if (ct >= max) break;
		if (p_ptr->spell_order[i] == 99) break;

		base = &_spells[p_ptr->spell_order[i]];
		if (base->level <= p_ptr->lev)
		{
			spell_info* current = &spells[ct];
			int fail = base->fail;
			int cost = base->cost;

			current->fn = base->fn;
			current->level = base->level;

			if (_power > 1)
			{
				cost = cost * _power * (_power + 1) / 2;
				fail += 15 * (_power - 1);
			}

			if (p_ptr->magic_num1[_CLARITY])
			{
				cost = cost * (95 - 7 * p_ptr->magic_num2[_CLARITY]) / 100;
			}

			if (p_ptr->magic_num1[_COMBAT])
			{
				cost *= 2;
			}

			current->cost = cost;
			current->fail = calculate_fail_rate(base->level, fail, p_ptr->stat_ind[A_INT]);
			ct++;
		}
	}
	return ct;
}

static void _calc_bonuses(void)
{
	if (p_ptr->magic_num1[_BLENDING])
	{
		p_ptr->skill_stl += 2 * p_ptr->magic_num2[_BLENDING];
		if ((p_ptr->cursed & TRC_AGGRAVATE) && p_ptr->magic_num2[_BLENDING] == 5)
		{
			p_ptr->cursed &= ~(TRC_AGGRAVATE);
			p_ptr->skill_stl = MIN(p_ptr->skill_stl - 3, (p_ptr->skill_stl + 2) / 2);
		}
	}

	if (p_ptr->magic_num1[_SHIELDING])
	{
		p_ptr->free_act = TRUE;
		if (!p_ptr->shield)
		{
			p_ptr->to_a += 15 * p_ptr->magic_num2[_SHIELDING];
			p_ptr->dis_to_a += 15 * p_ptr->magic_num2[_SHIELDING];
		}
	}

	if (p_ptr->magic_num1[_COMBAT])
	{
		p_ptr->skill_thn += 20*p_ptr->magic_num2[_COMBAT];
	}

	if (p_ptr->magic_num1[_SPEED])
	{
		if (!p_ptr->fast)
			p_ptr->pspeed += 3*p_ptr->magic_num2[_SPEED];
	}
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
	if (p_ptr->magic_num1[_WEAPON_GRAFT])
	{
		info_ptr->to_h += p_ptr->magic_num2[_WEAPON_GRAFT] * 6;
		info_ptr->dis_to_h += p_ptr->magic_num2[_WEAPON_GRAFT] * 6;

		info_ptr->to_d += p_ptr->magic_num2[_WEAPON_GRAFT] * 4;
		info_ptr->dis_to_d += p_ptr->magic_num2[_WEAPON_GRAFT] * 4;
	}

	if (p_ptr->magic_num1[_COMBAT])
	{
		info_ptr->num_blow += (p_ptr->magic_num2[_COMBAT] + 1) / 2;
	}
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "focus";
		me.use_sp = TRUE;
		me.options = CASTER_ALLOW_DEC_MANA;
		init = TRUE;
	}
	return &me;
}

static void _character_dump(FILE* file)
{
	int i, j;

	for (j = 1; j <= 5; j++)
	{
		spell_info spells[MAX_SPELLS];
		int ct;

		_power = j;
		ct = _get_spells(spells, MAX_SPELLS);

		if (ct > 0)
		{
			variant name, info;

			var_init(&name);
			var_init(&info);

			for (i = 0; i < ct; i++)
			{
				spell_info* current = &spells[i];
				current->cost += get_spell_cost_extra(current->fn);
				current->fail = MAX(current->fail, get_spell_fail_min(current->fn));
			}

			fprintf(file, "\n  [Psionic Powers %d]\n", _power);
			fprintf(file, "%-23.23s Lv Cost Fail Info\n", "");
			for (i = 0; i < ct; ++i)
			{
				spell_info *spell = &spells[i];

				(spell->fn)(SPELL_NAME, &name);
				(spell->fn)(SPELL_INFO, &info);

				fprintf(file, "%-23.23s %2d %4d %3d%% %s\n", 
								var_get_string(&name),
								spell->level,
								spell->cost,
								spell->fail,
								var_get_string(&info));
			}

			var_clear(&name);
			var_clear(&info);
		}
	}
}

class_t *psion_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 25,  35,  40,   2,  16,   8,  48,  35};
	skills_t xs = {  7,  11,  12,   0,   0,   0,  13,  11};

		me.name = "Psion";
		me.desc = "The Psion is like a Mindcrafter, and uses innate mental powers. "
					"Unlike the Mindcrafter, however, Psions have the freedom to learn "
					"powers that enforce their own styles. They learn very few powers, "
					"but they can scale their powers to determine the SP cost and the "
					"powers' potency. Psionic powers require great concentration, however, "
					"and psions do not have the mind to spare to care for others.";

		me.stats[A_STR] = -1;
		me.stats[A_INT] =  3;
		me.stats[A_WIS] = -1;
		me.stats[A_DEX] = -1;
		me.stats[A_CON] = -1;
		me.stats[A_CHR] =  1;
		me.mhp = 2;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.calc_bonuses = _calc_bonuses;
		me.calc_weapon_bonuses = _calc_weapon_bonuses;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		me.get_powers = _get_powers;
		me.character_dump = _character_dump;
		me.gain_level = _gain_level;
		init = TRUE;
	}

	return &me;
}
