/****************************************************************
 * The Weaponmaster
 ****************************************************************/

#include "angband.h"

int shoot_hack = SHOOT_NONE;
int shoot_count = 0;
int shoot_item = 0;
int frenzy_items[MAX_FRENZY_ITEMS];
static int _dual_wield = FALSE;

#define _MAX_TARGETS 100

/****************************************************************
 * Private Helpers
 ****************************************************************/

static bool _check_direct_shot(int tx, int ty)
{
	bool result = FALSE;
	u16b path[512];
	int ct = project_path(path, 50, py, px, ty, tx, PROJECT_PATH); /* We don't know the length ... just project from source to target, please! */
	int x, y, i;

	for (i = 0; i < ct; i++)
	{
		x = GRID_X(path[i]);
		y = GRID_Y(path[i]);

		/* Reached target! Yay! */
		if (x == tx && y == ty)
		{
			result = TRUE;
			break;
		}

		/* Stopped by walls/doors */
		if (!cave_have_flag_bold(y, x, FF_PROJECT) && !cave[y][x].m_idx) break;

		/* Monster in the way of target */
		if (cave[y][x].m_idx) break;
	}

	return result;
}

static bool _check_speciality1_equip(void);
static bool _check_speciality1_aux(object_type *o_ptr);
static bool _check_speciality2_equip(void);
static bool _check_speciality2_aux(object_type *o_ptr);
static bool _make_uber_weapon(void);

static int _find_ammo_slot(void)
{
	int i;

	for (i = 0; i < INVEN_PACK; i++)
	{
		if (inventory[i].tval == p_ptr->tval_ammo) return i;
	}
	if (p_ptr->unlimited_quiver) return INVEN_UNLIMITED_QUIVER;
	return -1;
}

static int _count_ammo_slots(void)
{
	int result = 0;
	int i;

	for (i = 0; i < INVEN_PACK; i++)
	{
		if (inventory[i].tval == p_ptr->tval_ammo) result++;
	}

	if (p_ptr->unlimited_quiver) result++;
	return result;
}

static int _get_nearest_target_los(void)
{
	int result = 0;
	int dis = AAF_LIMIT + 1;
	int i;
	monster_type *m_ptr = NULL;
	int rng = bow_range(inventory[INVEN_BOW].sval);
	
	for (i = m_max - 1; i >= 1; i--)
	{
		m_ptr = &m_list[i];
		if (!m_ptr->r_idx
		) continue;
		if (m_ptr->smart & SM_FRIENDLY) continue;
		if (m_ptr->smart & SM_PET) continue;
		if (m_ptr->cdis > rng) continue;
		if (!m_ptr->ml) continue;
		if (!los(py, px, m_ptr->fy, m_ptr->fx)) continue;

		if (m_ptr->cdis < dis)
		{
			result = i;
			dis = m_ptr->cdis;
		}
	}

	return result;
}

static int _get_greater_many_shot_targets(int *targets, int max)
{
	int result = 0;
	int i;
	monster_type *m_ptr = NULL;
	int rng = bow_range(inventory[INVEN_BOW].sval);

	/* shoot *all* line of sight monsters */	
	for (i = m_max - 1; i >= 1; i--)
	{
		m_ptr = &m_list[i];
		if (!m_ptr->r_idx) continue;
		if (m_ptr->smart & SM_FRIENDLY) continue;
		if (m_ptr->smart & SM_PET) continue;
		if (m_ptr->cdis > rng) continue;
		if (!m_ptr->ml) continue;
		if (!los(py, px, m_ptr->fy, m_ptr->fx)) continue;
		if (result >= max) break;

		targets[result] = i;
		result++;
	}

	return result;
}

static int _get_many_shot_targets(int *targets, int max)
{
	int result = 0;
	int i;
	monster_type *m_ptr = NULL;
	int in_sight[_MAX_TARGETS];
	int ct = 0;
	int rng = bow_range(inventory[INVEN_BOW].sval);

	/* pass 1: get line of sight monsters */	
	for (i = m_max - 1; i >= 1; i--)
	{
		m_ptr = &m_list[i];
		if (!m_ptr->r_idx) continue;
		if (m_ptr->smart & SM_FRIENDLY) continue;
		if (m_ptr->smart & SM_PET) continue;
		if (m_ptr->cdis > rng) continue;
		if (!m_ptr->ml) continue;
		if (!los(py, px, m_ptr->fy, m_ptr->fx)) continue;
		if (ct >= _MAX_TARGETS) break;

		in_sight[ct] = i;
		ct++;
	}

	/* pass 2: for each monster in los, build a path from the player to the
	   monster and make sure there are no other intervening monsters */
	for (i = 0; i < ct; i++)
	{
		m_ptr = &m_list[in_sight[i]];
		if (_check_direct_shot(m_ptr->fx, m_ptr->fy))
		{
			if (result > max) break;
			targets[result] = in_sight[i];
			result++;
		}
	}

	return result;
}

static bool _fire(int power)
{
	bool result = FALSE;
	shoot_hack = power;
	shoot_count = 0;
	result = do_cmd_fire();
	shoot_hack = SHOOT_NONE;
	shoot_count = 0;

	return result;
}

/* Weaponmasters have toggle based abilities */
static int _get_toggle(void)
{
	return p_ptr->magic_num1[0];
}

static void _shield_bash_toggle_hack(s32b toggle)
{
	bool do_swap = FALSE;
	if (toggle == TOGGLE_SHIELD_BASH)
	{
		if ( (!inventory[INVEN_RARM].k_idx || object_is_melee_weapon(&inventory[INVEN_RARM]))
		  && object_is_shield(&inventory[INVEN_LARM]) )
		{
			do_swap = TRUE;
		}
	}
	else if (p_ptr->magic_num1[0] == TOGGLE_SHIELD_BASH)
	{
		if ( (!inventory[INVEN_LARM].k_idx || object_is_melee_weapon(&inventory[INVEN_LARM]))
		  && object_is_shield(&inventory[INVEN_RARM]) )
		{
			do_swap = TRUE;
		}
	}

	if (do_swap)
	{
		object_type copy;
		object_copy(&copy, &inventory[INVEN_RARM]);
		object_copy(&inventory[INVEN_RARM], &inventory[INVEN_LARM]);
		object_copy(&inventory[INVEN_LARM], &copy);
		p_ptr->redraw |= PR_EQUIPPY;
	}
}

static int _set_toggle(s32b toggle)
{
	int result = p_ptr->magic_num1[0];

	if (toggle == result) return result;

	_shield_bash_toggle_hack(toggle);
	p_ptr->magic_num1[0] = toggle;

	p_ptr->redraw |= PR_STATUS;
	p_ptr->update |= PU_BONUS;
	handle_stuff();

	return result;
}

/****************************************************************
 * Private Spells
 ****************************************************************/

static int _get_desperation_idx(void)
{
	if (inventory[INVEN_RARM].k_idx && object_is_melee_weapon(&inventory[INVEN_RARM]))
		return INVEN_RARM;

	if (inventory[INVEN_LARM].k_idx && object_is_melee_weapon(&inventory[INVEN_LARM]))
		return INVEN_LARM;

	return -1;
}

static void _desperation_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Desperation");
		break;
	case SPELL_DESC:
		var_set_string(res, "You regain hp by completely disenchanting your current weapon.");
		break;
	case SPELL_CAST:
	{
		int item, ds, hp;
		char o_name[MAX_NLEN];

		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do need a shield.");
			return;
		}
		item = _get_desperation_idx();
		if (item < 0)
		{
			msg_print("Failed!  You do need a weapon to disenchant.");
			return;
		}
		ds = inventory[item].to_h + inventory[item].to_d;
		object_desc(o_name, &inventory[item], OD_NAME_ONLY | OD_OMIT_PREFIX);

		if (ds > 0)
		{
			hp = damroll(7, ds);
			hp_player(hp);

			if (!object_is_artifact(&inventory[item]) || one_in_(2))
			{
				if (inventory[item].to_h > 0) inventory[item].to_h--;
				if (inventory[item].to_d > 0) inventory[item].to_d--;
				msg_format("Your %s is disenchanted.", o_name);
			}
			else
			{
				msg_format("Your %s resists disenchantment.", o_name);
			}
		}
		else
		{
			msg_format("Your %s is too weak to help you any more.", o_name);
		}
		
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _sanctuary_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Sanctuary");
		break;
	case SPELL_DESC:
		var_set_string(res, "You become invulnerable until you damage a monster.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do need a shield.");
			return;
		}
		set_sanctuary(TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _shield_bash_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Shield Bash");
		break;
	case SPELL_DESC:
		var_set_string(res, "You fight with your shield rather than your weapon.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do need a shield.");
			return;
		}
		if (_get_toggle() == TOGGLE_SHIELD_BASH)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_SHIELD_BASH);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_SHIELD_BASH)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _bulwark_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Bulwark");
		break;
	case SPELL_DESC:
		var_set_string(res, "All melee damage that you receive is reduced.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do need a shield.");
			return;
		}
		if (_get_toggle() == TOGGLE_BULWARK)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_BULWARK);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_BULWARK)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _shield_revenge_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Eye for an Eye");
		break;
	case SPELL_DESC:
		var_set_string(res, "Monsters are damaged whenever they hurt you.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do need a shield.");
			return;
		}
		if (_get_toggle() == TOGGLE_SHIELD_REVENGE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_SHIELD_REVENGE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_SHIELD_REVENGE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _tunnel_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Tunnel");
		break;
	case SPELL_DESC:
		var_set_string(res, "Creates a tunnel down to the next level.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality1_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (!cave_valid_bold(py, px))
		{
			msg_print("You need room to dig!");
		}
		else
		{
			msg_print("You tunnel downwards ...");
			stair_creation(TRUE);
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _calamity_of_the_living_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Calamity of the Living");
		break;
	case SPELL_DESC:
		var_set_string(res, "Causes an earthquake or destruction.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (one_in_(3))
		{
			destroy_area(py, px, 12 + randint1(4), FALSE);
		}
		else
		{
			msg_print("The ground rumbles!");
			earthquake(py, px, 10);
		}
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static bool _object_is_corpse_or_skeleton(object_type *o_ptr)
{
	if (o_ptr->tval == TV_CORPSE) return TRUE;
	if (o_ptr->tval == TV_SKELETON) return TRUE;
	return FALSE;
}

static void _bury_dead_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Bury Dead");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain temporary enchantments by burying a corpse or skeleton.");
		break;
	case SPELL_CAST:
	{
		int item;
		char o_name[MAX_NLEN];
		object_type *o_ptr;
		object_type copy;
		int turns;

		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}

		item_tester_hook = _object_is_corpse_or_skeleton;
		if (!get_item(&item, "Bury which corpse? ", "You have nothing to bury.", (USE_INVEN | USE_FLOOR))) return;

		if (item >= 0)
			o_ptr = &inventory[item];
		else
			o_ptr = &o_list[0 - item];

		/* TV_CORPSE, SV_CORPSE = Corpse
		   TV_CORPSE, SV_SKELETON = Skeleton
		   TV_SKELETON, ??? = Skeleton */
		if (o_ptr->tval == TV_CORPSE && o_ptr->sval == SV_CORPSE)
			turns = 15;
		else
			turns = 5;

		object_copy(&copy, o_ptr);
		copy.number = 1;
		object_desc(o_name, &copy, OD_NAME_ONLY);
		msg_format("You dig a hasty grave and toss in %s.", o_name);

		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}

		set_blessed(p_ptr->blessed + turns, FALSE);
		if (p_ptr->lev > 15)
			set_hero(p_ptr->hero + turns, FALSE);
		if (p_ptr->lev > 30)
			set_fast(p_ptr->fast + turns, FALSE);
		if (p_ptr->lev > 40)
			set_resist_magic(p_ptr->resist_magic + turns, FALSE);
	
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _barricade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Barricade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Creates a pile of rubble on the adjacent square that you target.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (!in_bounds(y, x)) return;
		if (!cave_naked_bold(y, x))
		{
			msg_print("You can't create a barricade there!");
			return;
		}

		cave_set_feat(y, x, feat_rubble);
		p_ptr->update |= PU_BONUS;

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _strength_of_the_undertaker_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Strength of Undertaker");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain additional strength based on the quality of your digger.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_STRENGTH_OF_THE_UNDERTAKER)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_STRENGTH_OF_THE_UNDERTAKER);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_STRENGTH_OF_THE_UNDERTAKER)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _stoicism_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Stoicism");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain additional constitution based on the quality of your digger.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_STOICISM)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_STOICISM);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_STOICISM)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _industrious_mortician_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Industrious Mortician");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain additional attacks when using this technique based on the quality of your digger.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_INDUSTRIOUS_MORTICIAN)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_INDUSTRIOUS_MORTICIAN);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_INDUSTRIOUS_MORTICIAN)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static int hit_chance(int to_h, int ac)
{
	int chance = 0;
	int meichuu = p_ptr->skill_thn + (p_ptr->weapon_info[0].to_h + to_h) * BTH_PLUS_ADJ;

	if (meichuu <= 0) return 5;

	chance = 100 - ((ac * 75) / meichuu);

	if (chance > 95) chance = 95;
	if (chance < 5) chance = 5;
	if (p_ptr->pseikaku == SEIKAKU_NAMAKE)
		chance = (chance*19+9)/20;
	return chance;
}

bool _design_monkey_clone(void)
{
	int i;
	monster_race *r_ptr = &r_info[MON_MONKEY_CLONE];
	int dd = 10;
	int ds = 10;
	int dam = 0;
	int tdam = 0;
	int blows = 0;
	int acc = 0;
	int tmp_acc = 0;

	if (r_ptr->cur_num == 1)
	{
		msg_print("You may only have one Monkey Clone at a time!");
		return FALSE;
	}

	r_ptr->hdice = 10;
	r_ptr->hside = p_ptr->mhp / 15;
	r_ptr->ac = p_ptr->ac + p_ptr->to_a;
	r_ptr->speed = p_ptr->pspeed;

	/* Combat */
	if (buki_motteruka(INVEN_RARM))
	{
		object_type *o_ptr = &inventory[INVEN_RARM];
		tdam += p_ptr->weapon_info[0].num_blow * 
			(o_ptr->dd * (o_ptr->ds + 1)/2 + p_ptr->weapon_info[0].to_d + o_ptr->to_d);

		blows += p_ptr->weapon_info[0].num_blow;
		acc = hit_chance(o_ptr->to_h, 150);
	}

	if (buki_motteruka(INVEN_LARM))
	{
		object_type *o_ptr = &inventory[INVEN_LARM];
		tdam += p_ptr->weapon_info[1].num_blow * 
			(o_ptr->dd * (o_ptr->ds + 1)/2 + p_ptr->weapon_info[0].to_d + o_ptr->to_d);

		blows += p_ptr->weapon_info[1].num_blow;
	}

	dam = tdam / MIN(4, blows);
	dd = 10;
	ds = dam/5;
	if (ds < 1) 
	{
		dd = 1;
		ds = 1;
	}

	r_ptr->level = (60*acc + 5200)/(300 - 3*acc); /* Don't ask ... */

	for (i = 0; i < 4 && i < blows; i++)
	{
		r_ptr->blow[i].method = 0;
	}

	for (i = 0; i < 4; i++)
	{
		r_ptr->blow[i].method = RBM_HIT;
		r_ptr->blow[i].effect = RBE_HURT;
		r_ptr->blow[i].d_dice = dd;
		r_ptr->blow[i].d_side = ds;
	}

	/* Resistances */
	r_ptr->flagsr = 0;
	r_ptr->flags3 = 0;
	r_ptr->flags2 = 0;
	r_ptr->flags7 = 0;

	if (p_ptr->resist_acid) r_ptr->flagsr |= RFR_IM_ACID;
	if (p_ptr->resist_elec) r_ptr->flagsr |= RFR_IM_ELEC;
	if (p_ptr->resist_fire) r_ptr->flagsr |= RFR_IM_FIRE;
	if (p_ptr->resist_cold) r_ptr->flagsr |= RFR_IM_COLD;
	if (p_ptr->resist_pois) r_ptr->flagsr |= RFR_IM_POIS;
	if (p_ptr->resist_lite) r_ptr->flagsr |= RFR_RES_LITE;
	if (p_ptr->resist_dark) r_ptr->flagsr |= RFR_RES_DARK;
	if (p_ptr->resist_neth) r_ptr->flagsr |= RFR_RES_NETH;
	if (p_ptr->resist_shard) r_ptr->flagsr |= RFR_RES_SHAR;
	if (p_ptr->resist_sound) r_ptr->flagsr |= RFR_RES_SOUN;
	if (p_ptr->resist_chaos) r_ptr->flagsr |= RFR_RES_CHAO;
	if (p_ptr->resist_nexus) r_ptr->flagsr |= RFR_RES_NEXU;
	if (p_ptr->resist_disen) r_ptr->flagsr |= RFR_RES_DISE;

	if (p_ptr->resist_conf) r_ptr->flags3 |= RF3_NO_CONF;
	if (p_ptr->resist_fear) r_ptr->flags3 |= RF3_NO_FEAR;
	if (p_ptr->free_act) r_ptr->flags3 |= RF3_NO_SLEEP;
	if (p_ptr->sh_cold) r_ptr->flags3 |= RF3_AURA_COLD;

	if (p_ptr->reflect) r_ptr->flags2 |= RF2_REFLECTING;
	if (p_ptr->regenerate) r_ptr->flags2 |= RF2_REGENERATE;
	if (p_ptr->sh_fire) r_ptr->flags2 |= RF2_AURA_FIRE;
	if (p_ptr->sh_elec) r_ptr->flags2 |= RF2_AURA_ELEC;
	if (p_ptr->pass_wall) r_ptr->flags2 |= RF2_PASS_WALL;
	r_ptr->flags2 |= RF2_OPEN_DOOR;
	r_ptr->flags2 |= RF2_CAN_SPEAK;

	r_ptr->flags7 |= RF7_CAN_SWIM;
	if (p_ptr->levitation) r_ptr->flags7 |= RF7_CAN_FLY;

	return TRUE;
}

static void _monkey_king_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Monkey King's Technique");
		break;
	case SPELL_DESC:
		var_set_string(res, "Create a clone of yourself, but at great cost.");
		break;
	case SPELL_CAST:
	{
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_design_monkey_clone() && summon_named_creature(0, py, px, MON_MONKEY_CLONE, PM_FORCE_PET))
			var_set_bool(res, TRUE);
		break;
	}
	case SPELL_COST_EXTRA:
		var_set_int(res, (p_ptr->mhp + 2)/3);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void _circle_kick(void)
{
	int i;
	int dd = 1;
	int ds = p_ptr->lev;
	int bonus = p_ptr->to_h_m;
	int chance = p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ);

	if (inventory[INVEN_FEET].k_idx)
		dd = k_info[inventory[INVEN_FEET].k_idx].ac;
	
	for (i = 0; i < 8; i++)
	{	
		int           dir = cdd[i];
		int           y = py + ddy[dir];
		int           x = px + ddx[dir];
		cave_type    *c_ptr = &cave[y][x];
		monster_type *m_ptr = &m_list[c_ptr->m_idx];

		if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
		{
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		char          m_name[MAX_NLEN];

			monster_desc(m_name, m_ptr, 0);
			
			if (test_hit_norm(chance, r_ptr->ac, m_ptr->ml))
			{
				int dam = damroll(dd, ds) + p_ptr->to_d_m;

				sound(SOUND_HIT);
				msg_format("You kick %s.", m_name);

				if (!(r_ptr->flags3 & RF3_NO_STUN))
				{
					if (MON_STUNNED(m_ptr))
						msg_format("%s is more dazed.", m_name);
					else
						msg_format("%s is dazed.", m_name);

					set_monster_stunned(c_ptr->m_idx, MON_STUNNED(m_ptr) + 1);
				}
				else
					msg_format("%s is not affected.", m_name);


				dam = mon_damage_mod(m_ptr, dam, FALSE);
				/* Hack: Monster AC now reduces damage */
				dam -= (dam * ((r_ptr->ac < 200) ? r_ptr->ac : 200) / 1200);

				if (dam > 0)
				{
					bool fear;
					mon_take_hit(c_ptr->m_idx, dam, &fear, NULL);

					anger_monster(m_ptr);
				}
				retaliation_count = 0; /* AURA_REVENGE */
				touch_zap_player(c_ptr->m_idx);
			}
			else
			{
				sound(SOUND_MISS);
				msg_format("You miss %s.", m_name);
			}
		}
	}
}

static void _circle_kick_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Circle Kick");
		break;
	case SPELL_DESC:
		var_set_string(res, "Kicks all adjacent opponents, stunning them.  Damage depends on your boots!");
		break;
	case SPELL_CAST:
	{
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		_circle_kick();
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

 static bool _vault_attack(void)
{
	int tx, ty;
	int tm_idx = 0;
	u16b path_g[32];
	int path_n, i;
	bool moved = FALSE;
	int flg = PROJECT_THRU | PROJECT_KILL;
	int dis = 0;
	int dir;

	project_length = 3;

	if (!get_aim_dir(&dir)) return FALSE;

	tx = px + project_length * ddx[dir];
	ty = py + project_length * ddy[dir];

	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	if (in_bounds(ty, tx)) tm_idx = cave[ty][tx].m_idx;

	dis = distance(ty, tx, py, px);

	path_n = project_path(path_g, project_length, py, px, ty, tx, flg);
	project_length = 0;

	if (!path_n) return FALSE;

	ty = py;
	tx = px;

	for (i = 0; i < path_n; i++)
	{
		monster_type *m_ptr;
		cave_type *c_ptr;
		bool can_enter = FALSE;
		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);
		
		c_ptr = &cave[ny][nx];
		can_enter = !c_ptr->m_idx && player_can_enter(c_ptr->feat, 0);

		if (can_enter)
		{
			ty = ny;
			tx = nx;
			continue;
		}

		if (!c_ptr->m_idx)
		{
			msg_print("Failed!");
			break;
		}

		/* Move player before updating the monster */
		if (!player_bold(ty, tx)) move_player_effect(ty, tx, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
		moved = TRUE;
		
		update_mon(c_ptr->m_idx, TRUE);

		m_ptr = &m_list[c_ptr->m_idx];
		if (tm_idx != c_ptr->m_idx)
		{
			/* Just like "Acrobatic Charge." Attempts to displace monsters on route. */
			set_monster_csleep(c_ptr->m_idx, 0);
			move_player_effect(ny, nx, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
			ty = ny;
			tx = nx;
			continue;
		}
		py_attack(ny, nx, 0);
		break;
	}

	if (!moved && !player_bold(ty, tx)) move_player_effect(ty, tx, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
	return TRUE;
}

static void _vault_attack_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Vault Attack");
		break;
	case SPELL_DESC:
		var_set_string(res, "Charge and attack a nearby opponent in a single move.");
		break;
	case SPELL_CAST:
	{
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_vault_attack())
			var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}


static void _flurry_of_blows_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Flurry of Blows");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain additional attacks when using this technique, but you lose accuracy.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_FLURRY_OF_BLOWS)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_FLURRY_OF_BLOWS);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_FLURRY_OF_BLOWS)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _greater_flurry_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Greater Flurry");
		break;
	case SPELL_DESC:
		var_set_string(res, "You gain additional attacks when using this technique, but you lose accuracy.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_GREATER_FLURRY)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_GREATER_FLURRY);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_GREATER_FLURRY)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _many_strike_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Many Strike");
		break;
	case SPELL_DESC:
		var_set_string(res, "Your attacks scatter widely among surrounding foes.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_MANY_STRIKE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_MANY_STRIKE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_MANY_STRIKE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _piercing_strike_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Piercing Strike");
		break;
	case SPELL_DESC:
		var_set_string(res, "When you attack a foe, successful hits will pierce the opponent attacking additional monsters.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_PIERCING_STRIKE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_PIERCING_STRIKE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_PIERCING_STRIKE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _trip_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Trip");
		break;
	case SPELL_DESC:
		var_set_string(res, "When you attack a foe, successful hits will attempt to trip up your opponent.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_TRIP)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_TRIP);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_TRIP)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _enclose_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Enclose");
		break;
	case SPELL_DESC:
		var_set_string(res, "A successful attack renders your opponent unable to run away.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_ENCLOSE);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _knock_back_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Knock Back");
		break;
	case SPELL_DESC:
		var_set_string(res, "A successful attack will push your opponent back a square.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_KNOCK_BACK);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _reaping_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Reaping");
		break;
	case SPELL_DESC:
		var_set_string(res, "A successful strike will damage all adjacent opponents granting you wraithform.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_REAPING);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _burning_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Burning Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Damage from your blade becomes fire damage and never misses.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_BURNING_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_BURNING_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_BURNING_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _ice_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Ice Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Damage from your blade becomes frost damage and slows your opponent.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_ICE_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_ICE_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_ICE_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _thunder_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Thunder Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Damage from your blade becomes lightning damage and stuns your opponent.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_THUNDER_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_THUNDER_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_THUNDER_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _blood_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Blood Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Your blade thirsts for the living.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_BLOOD_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_BLOOD_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_BLOOD_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _holy_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Holy Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Your blade fights powerful against the forces of evil.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_HOLY_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_HOLY_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_HOLY_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _order_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Order Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "Your blade becomes a weapon of order, always dealing maximal damage.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_ORDER_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_ORDER_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_ORDER_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _wild_blade_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Wild Blade");
		break;
	case SPELL_DESC:
		var_set_string(res, "This is too crazy to describe.  You wouldn't believe me anyway!");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_WILD_BLADE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_WILD_BLADE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_WILD_BLADE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}



static void _bouncing_pebble_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Bouncing Pebble");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires a pebble or shot at an opponent.  If you hit, the pebble or shot will ricochet in a random direction.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your shooter.");
			return;
		}
		if (_fire(SHOOT_BOUNCE))
			var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		var_set_int(res, 0);	/* already charged in _fire() */
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _combat_expertise_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Combat Expertise");
		break;
	case SPELL_DESC:
		var_set_string(res, "You lose accuracy but gain armor class.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_COMBAT_EXPERTISE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_COMBAT_EXPERTISE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_COMBAT_EXPERTISE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _stone_bones_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Stone Bones");
		break;
	case SPELL_DESC:
		var_set_string(res, "You become nearly impervious to melee damage, but seem hardly able to hurt anything yourself!");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_STONE_BONES)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_STONE_BONES);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_STONE_BONES)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _crusaders_strike_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Crusaders Strike");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attack an adjacent opponent with a single blow.  You regain hp.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_CRUSADERS_STRIKE);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _cunning_strike_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Cunning Strike");
		break;
	case SPELL_DESC:
		var_set_string(res, "Number of attacks are cut in half, but blows are more likely to confuse, stun, knock out your opponent.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_CUNNING_STRIKE);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _smite_evil_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Smite Evil");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attack powerfully at an evil monster.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_SMITE_EVIL);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

/* DAGGER TOSS: Code spiked from do_cmd_throw with heavy mods */
static int _dagger_toss_multiplier(int item)
{
	int result = 4;

	switch (item)
	{
	case INVEN_RARM:
		result = p_ptr->weapon_info[0].num_blow;
		break;
	case INVEN_LARM:
		result = p_ptr->weapon_info[1].num_blow;
		break;
	default:
	{   /* Total Hack Job: Swap the weapon to toss into the right arm, recalc
	       all bonuses, read the result, and then swap back the original item */
		object_type rarm, copy;
		object_copy(&rarm, &inventory[INVEN_RARM]);
		object_copy(&copy, &inventory[item]);
		copy.number = 1;
		object_copy(&inventory[INVEN_RARM], &copy);
		p_ptr->update |= PU_BONUS;
		handle_stuff();
		result = p_ptr->weapon_info[0].num_blow;
		object_copy(&inventory[INVEN_RARM], &rarm);
		p_ptr->update |= PU_BONUS;
		handle_stuff();
	}
	}
	return result;
}

static bool _dagger_toss_imp1(int item);

typedef struct {
	int item;
	object_type *o_ptr;
	int mult;
	int tdis;
	int tx;
	int ty;
	bool come_back;
	bool flying_dagger;
} _dagger_toss_info;

static void _dagger_toss_imp2(_dagger_toss_info * info_ptr);

static bool _dagger_toss(void)
{
	int item;

	/* Prompt for dagger to toss.  Choose equipped dagger and we will toss both, if possible.
	   Choose from pack and it is just a single toss */
	item_tester_hook = _check_speciality2_aux;
	if (!get_item(&item, "Throw which item? ", "You have nothing to throw.", (USE_INVEN | USE_EQUIP)))
	{
		flush();
		return FALSE;
	}

	if (item == INVEN_RARM || item == INVEN_LARM)
	{
		int k_idx = inventory[INVEN_LARM].k_idx;
		if (!_dagger_toss_imp1(INVEN_RARM)) return FALSE;
		/* Hack: First toss did not return, and weapons shuffled */
		if (inventory[INVEN_LARM].k_idx != k_idx && inventory[INVEN_RARM].k_idx == k_idx) 
			_dagger_toss_imp1(INVEN_RARM);
		else
			_dagger_toss_imp1(INVEN_LARM);
		return TRUE;
	}
	else
		return _dagger_toss_imp1(item);
}

static bool _dagger_toss_imp1(int item)
{
	int dir;
	_dagger_toss_info info;
	
	/* Setup info for the toss */
	info.item = item;
	info.o_ptr = &inventory[item];
	info.come_back = FALSE;
	info.flying_dagger = FALSE;
	if (_get_toggle() == TOGGLE_FLYING_DAGGER_STANCE)
		info.flying_dagger = TRUE;

	if (!_check_speciality2_aux(info.o_ptr)) return FALSE;
	if (object_is_cursed(info.o_ptr) && (info.item >= INVEN_RARM))
	{
		msg_print(T("Hmmm, it seems to be cursed.", "ふーむ、どうやら呪われているようだ。"));
		return FALSE;
	}

	if (have_flag(info.o_ptr->art_flags, TR_SIGNATURE)) 
		info.come_back = TRUE;
	else if (info.flying_dagger)
	{
		if (randint1(100) <= 58 + p_ptr->stat_ind[A_DEX])
			info.come_back = TRUE;
	}

	/* Pick a target */
	info.mult = _dagger_toss_multiplier(item);
	{
		int mul, div;
		mul = 10 + 2 * (info.mult - 1);
		div = (info.o_ptr->weight > 10) ? info.o_ptr->weight : 10;
		div /= 2;
		info.tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;
		if (info.tdis > mul) info.tdis = mul;

		project_length = info.tdis + 1;
		if (!get_aim_dir(&dir)) return FALSE;

		info.tx = px + 99 * ddx[dir];
		info.ty = py + 99 * ddy[dir];

		if ((dir == 5) && target_okay())
		{
			info.tx = target_col;
			info.ty = target_row;
		}

		project_length = 0;

		if (info.tx == px && info.ty == py) return FALSE;
	}

	/* Toss */
	_dagger_toss_imp2(&info);

	/* Handle Inventory */
	if (!info.come_back)
	{
		object_type copy;

		object_copy(&copy, info.o_ptr);
		copy.number = 1;

		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
		
		/* Dagger Toss never breaks! */
		drop_near(&copy, 0, info.ty, info.tx);
		
		if (item >= INVEN_RARM)
		{
			p_ptr->redraw |= PR_EQUIPPY;
			p_ptr->update |= PU_BONUS;

			kamaenaoshi(item);
			calc_android_exp();
		}
		handle_stuff();
	}

	return TRUE;
}

static void _dagger_toss_imp2(_dagger_toss_info * info)
{
	char o_name[MAX_NLEN];
	u16b path[512];
	int msec = delay_factor * delay_factor * delay_factor;
	int y, x, ny, nx, dd, tdam;
	int cur_dis, ct;
	int chance;

	chance = p_ptr->skill_tht + ((p_ptr->to_h_b + info->o_ptr->to_h) * BTH_PLUS_ADJ);
	chance *= 2; /* mimicking code for ninja shuriken */

	object_desc(o_name, info->o_ptr, OD_OMIT_PREFIX);
	ct = project_path(path, info->tdis, py, px, info->ty, info->tx, PROJECT_PATH);

	y = py;
	x = px;

	for (cur_dis = 0; cur_dis < ct; )
	{
		/* Peek ahead at the next square in the path */
		ny = GRID_Y(path[cur_dis]);
		nx = GRID_X(path[cur_dis]);

		/* Stopped by walls/doors/forest ... but allow hitting your target, please! */
		if (!cave_have_flag_bold(ny, nx, FF_PROJECT)
		 && !cave[ny][nx].m_idx) 
		{
			break;
		}

		/* The player can see the (on screen) missile */
		if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
		{
			char c = object_char(info->o_ptr);
			byte a = object_attr(info->o_ptr);

			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(c, a, ny, nx);
			move_cursor_relative(ny, nx);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(ny, nx);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Save the new location */
		x = nx;
		y = ny;

		/* Advance the distance */
		cur_dis++;

		/* Monster here, Try to hit it */
		if (cave[y][x].m_idx)
		{
			cave_type *c_ptr = &cave[y][x];
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			bool visible = m_ptr->ml;

			if (test_hit_fire(chance - cur_dis, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				if (!visible)
					msg_format("The %s finds a mark.", o_name);
				else
				{
					char m_name[80];
					monster_desc(m_name, m_ptr, 0);
					msg_format("The %s hits %s.", o_name, m_name);
					if (m_ptr->ml)
					{
						if (!p_ptr->image) monster_race_track(m_ptr->ap_r_idx);
						health_track(c_ptr->m_idx);
					}
				}

				/***** The Damage Calculation!!! *****/
				dd = info->o_ptr->dd;
				if (info->flying_dagger)
					dd += p_ptr->lev/15;
				tdam = damroll(dd, info->o_ptr->ds);				
				tdam = tot_dam_aux(info->o_ptr, tdam, m_ptr, 0, TRUE);
				tdam = critical_shot(info->o_ptr->weight, info->o_ptr->to_h, tdam);
				tdam += info->o_ptr->to_d;
				tdam *= info->mult;
				tdam += p_ptr->to_d_b;
				if (tdam < 0) tdam = 0;
				tdam = mon_damage_mod(m_ptr, tdam, FALSE);
				tdam -= (tdam * ((r_ptr->ac < 200) ? r_ptr->ac : 200) / 1200);

				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, extract_note_dies(real_r_ptr(m_ptr))))
				{
					/* Dead monster */
				}
				else
				{
					message_pain(c_ptr->m_idx, tdam);
					if (tdam > 0)
						anger_monster(m_ptr);

					if (tdam > 0 && m_ptr->cdis > 1 && allow_ticked_off(r_ptr))
					{
						if (!(m_ptr->smart & SM_TICKED_OFF))
						{
							if (mut_present(MUT_PEERLESS_SNIPER) && !one_in_(5))
							{
							}
							else
							{
								char m_name[80];
								monster_desc(m_name, m_ptr, 0);
								msg_format("%^s is ticked off!", m_name);
								m_ptr->smart |= SM_TICKED_OFF;
							}
						}
					}

					if (fear && m_ptr->ml)
					{
						char m_name[80];
						sound(SOUND_FLEE);
						monster_desc(m_name, m_ptr, 0);
						msg_format("%^s flees in terror!", m_name);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	if (info->come_back)
	{
		int i;
		for (i = cur_dis; i >= 0; i--)
		{
			y = GRID_Y(path[i]);
			x = GRID_X(path[i]);
			if (panel_contains(y, x) && player_can_see_bold(y, x))
			{
				char c = object_char(info->o_ptr);
				byte a = object_attr(info->o_ptr);

				/* Draw, Hilite, Fresh, Pause, Erase */
				print_rel(c, a, y, x);
				move_cursor_relative(y, x);
				Term_fresh();
				Term_xtra(TERM_XTRA_DELAY, msec);
				lite_spot(y, x);
				Term_fresh();
			}
			else
			{
				/* Pause anyway, for consistancy */
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}
		msg_format("Your %s comes back to you.", o_name);
	}
	else
	{
		/* Record the actual location of the toss so we can drop the object here if required */
		info->tx = x;
		info->ty = y;
	}
}

static void _dagger_toss_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Dagger Toss");
		break;
	case SPELL_DESC:
		var_set_string(res, "Throws both equipped weapons, or 1 inventory weapon at target monster.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_dagger_toss())
			var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _flying_dagger_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Flying Dagger Stance");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, you gain great prowess with the Dagger Toss.  Thrown weapons often return and damage is greatly increased.  However, this stance leaves you somewhat exposed to your enemies.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_FLYING_DAGGER_STANCE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_FLYING_DAGGER_STANCE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_FLYING_DAGGER_STANCE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _frenzy_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Frenzy");
		break;
	case SPELL_DESC:
		var_set_string(res, "In this posture, you attack foes with great power, using both equipped weapons and weapons in your inventory.  However, inventory items will be destroyed on use.");
		break;
	case SPELL_CAST:
	{
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_FRENZY_STANCE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_FRENZY_STANCE);
		var_set_bool(res, TRUE);
		break;
	}
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_FRENZY_STANCE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _greater_many_shot_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Greater Many Shot");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires pebbles at all visible monsters.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your shooter.");
			return;
		}
		else
		{
			int i, item;
			int tgts[_MAX_TARGETS];
			int ct = _get_greater_many_shot_targets(tgts, _MAX_TARGETS);

			item_tester_tval = p_ptr->tval_ammo;
			if (!get_item(&item, 
						  "Fire which ammo? ", 
						  "You have nothing to fire.", (USE_INVEN | USE_QUIVER)))
			{
				flush();
				return;
			}

			shoot_hack = SHOOT_ALL;

			for (i = 0; i < ct; i++)
			{
				int tgt = tgts[i];
				int tx = m_list[tgt].fx;
				int ty = m_list[tgt].fy;

				do_cmd_fire_aux2(item, &inventory[INVEN_BOW], px, py, tx, ty);
			}

			shoot_hack = SHOOT_NONE;
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _many_shot_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Many Shot");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires pebbles at all visible monsters.  You need to have a direct line of fire to each target, though.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your shooter.");
			return;
		}
		else
		{
			int i, item;
			int tgts[_MAX_TARGETS];
			int ct = _get_many_shot_targets(tgts, _MAX_TARGETS);

			item_tester_tval = p_ptr->tval_ammo;
			if (!get_item(&item, 
						  "Fire which ammo? ", 
						  "You have nothing to fire.", (USE_INVEN | USE_QUIVER)))
			{
				flush();
				return;
			}

			shoot_hack = SHOOT_MANY;

			for (i = 0; i < ct; i++)
			{
				int tgt = tgts[i];
				int tx = m_list[tgt].fx;
				int ty = m_list[tgt].fy;

				do_cmd_fire_aux2(item, &inventory[INVEN_BOW], px, py, tx, ty);
			}

			shoot_hack = SHOOT_NONE;
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _power_attack_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Power Attack");
		break;
	case SPELL_DESC:
		var_set_string(res, "You lose accuracy but gain damage.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_POWER_ATTACK)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_POWER_ATTACK);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_POWER_ATTACK)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _shadow_stance_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Shadow Stance");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, you walk quickly and stealthily.  On attacking a foe, you will swap positions.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_SHADOW_STANCE)
		{
			_set_toggle(TOGGLE_NONE);
			set_action(ACTION_NONE);
		}
		else
		{
			set_action(ACTION_HAYAGAKE); /* Steal Ninja Quickwalk code */
			_set_toggle(TOGGLE_SHADOW_STANCE);
		}
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_SHADOW_STANCE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _rapid_shot_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rapid Shot");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, all of your shots will launch against a single opponent.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your shooter.");
			return;
		}
		if (_get_toggle() == TOGGLE_RAPID_SHOT)
			_set_toggle(TOGGLE_NONE);
		else
		{
			_set_toggle(TOGGLE_RAPID_SHOT);
			/* do_cmd_fire() will handle the gritty details */
		}
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_RAPID_SHOT)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _shot_on_the_run_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Shoot on the Run");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, you automatically fire at the closest opponent every time you move.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your shooter.");
			return;
		}
		if (_get_toggle() == TOGGLE_SHOT_ON_THE_RUN)
			_set_toggle(TOGGLE_NONE);
		else
		{
			/* Prompt for ammo to use, but disallow choosing from the floor since
			   the player will be moving */
			item_tester_tval = p_ptr->tval_ammo;
			if (!get_item(&shoot_item, 
						  "Choose ammo for this technique.", 
						  "You have nothing to fire.", (USE_INVEN | USE_QUIVER)))
			{
				flush();
				return;
			}
			_set_toggle(TOGGLE_SHOT_ON_THE_RUN);
			/* _move_player() will handle the gritty details */
		}
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_SHOT_ON_THE_RUN)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _smash_ground_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Smash Ground");
		break;
	case SPELL_DESC:
		var_set_string(res, "Stuns, scares and aggravates nearby monsters.");
		break;
	case SPELL_CAST:
	{
		int power = spell_power(p_ptr->lev * 4);
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		msg_print("You smash your weapon mightily on the ground.");
		stun_monsters(power);
		turn_monsters(power);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _strike_vulnerability_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Strike Vulnerability");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attack an adjacent opponent with a single blow powerfully.  Your attack will penetrate invulnerability.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
			py_attack(y, x, WEAPONMASTER_STRIKE_VULNERABILITY);
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

/***** THROW WEAPON SPELL **********/
typedef struct {
	int item;
	object_type *o_ptr;
	int mult;
	int tdis;
	int tx;
	int ty;
	bool come_back;
	bool fail_catch;
} _club_toss_info;
static void _club_toss_imp(_club_toss_info * info);

static bool _club_toss(void)
{
	int dir;
	_club_toss_info info;
	int back_chance;
	
	/* Setup info for the toss */
	info.item = INVEN_RARM;
	info.o_ptr = &inventory[INVEN_RARM];

	/* Toss mechanics stolen from Samurai Boomerang ... see do_cmd_throw_aux() */
	back_chance = randint1(30)+20+((int)(adj_dex_th[p_ptr->stat_ind[A_DEX]]) - 128);
	back_chance += 4+randint1(5);

	info.come_back = FALSE;
	info.fail_catch = FALSE;
	if (back_chance > 30 && !one_in_(100))
	{
		info.come_back = TRUE;
		if (back_chance <= 37)
			info.fail_catch = TRUE;
	}
	
	if (!_check_speciality2_aux(info.o_ptr)) return FALSE;
	if (object_is_cursed(info.o_ptr) && (info.item >= INVEN_RARM))
	{
		msg_print(T("Hmmm, it seems to be cursed.", "ふーむ、どうやら呪われているようだ。"));
		return FALSE;
	}

	if (have_flag(info.o_ptr->art_flags, TR_SIGNATURE)) 
	{
		info.come_back = TRUE;
		info.fail_catch = FALSE;
	}

	/* Pick a target */
	info.mult = 1 + p_ptr->weapon_info[0].num_blow;
	{
		int mul, div;
		mul = 10 + 2 * (info.mult - 1);
		div = (info.o_ptr->weight > 10) ? info.o_ptr->weight : 10;
		div /= 2;
		info.tdis = (adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * mul / div;
		if (info.tdis > mul) info.tdis = mul;

		project_length = info.tdis + 1;
		if (!get_aim_dir(&dir)) return FALSE;

		info.tx = px + 99 * ddx[dir];
		info.ty = py + 99 * ddy[dir];

		if ((dir == 5) && target_okay())
		{
			info.tx = target_col;
			info.ty = target_row;
		}

		project_length = 0;

		if (info.tx == px && info.ty == py) return FALSE;
	}

	/* Toss */
	_club_toss_imp(&info);

	/* Handle Inventory */
	if (!info.come_back || info.fail_catch)
	{
		object_type copy;

		object_copy(&copy, info.o_ptr);
		copy.number = 1;

		inven_item_increase(info.item, -1);
		inven_item_describe(info.item);
		inven_item_optimize(info.item);
		

		if (!info.come_back)
			drop_near(&copy, 0, info.ty, info.tx);
		else
		{
			msg_print("But you can't catch!");
			drop_near(&copy, 0, py, px);
		}
		
		if (info.item >= INVEN_RARM)
		{
			p_ptr->redraw |= PR_EQUIPPY;
			p_ptr->update |= PU_BONUS;

			kamaenaoshi(info.item);
			calc_android_exp();
		}
		handle_stuff();
	}

	return TRUE;
}

static void _club_toss_imp(_club_toss_info * info)
{
	char o_name[MAX_NLEN];
	u16b path[512];
	int msec = delay_factor * delay_factor * delay_factor;
	int y, x, ny, nx, dd, tdam;
	int cur_dis, ct;
	int chance;

	chance = p_ptr->skill_tht + ((p_ptr->to_h_b + info->o_ptr->to_h) * BTH_PLUS_ADJ);
	chance *= 2; /* mimicking code for ninja shuriken */

	object_desc(o_name, info->o_ptr, OD_OMIT_PREFIX);
	ct = project_path(path, info->tdis, py, px, info->ty, info->tx, PROJECT_PATH);

	y = py;
	x = px;

	for (cur_dis = 0; cur_dis < ct; )
	{
		/* Peek ahead at the next square in the path */
		ny = GRID_Y(path[cur_dis]);
		nx = GRID_X(path[cur_dis]);

		/* Stopped by walls/doors/forest ... but allow hitting your target, please! */
		if (!cave_have_flag_bold(ny, nx, FF_PROJECT)
		 && !cave[ny][nx].m_idx) 
		{
			break;
		}

		/* The player can see the (on screen) missile */
		if (panel_contains(ny, nx) && player_can_see_bold(ny, nx))
		{
			char c = object_char(info->o_ptr);
			byte a = object_attr(info->o_ptr);

			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(c, a, ny, nx);
			move_cursor_relative(ny, nx);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, msec);
			lite_spot(ny, nx);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, msec);
		}

		/* Save the new location */
		x = nx;
		y = ny;

		/* Advance the distance */
		cur_dis++;

		/* Monster here, Try to hit it */
		if (cave[y][x].m_idx)
		{
			cave_type *c_ptr = &cave[y][x];
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			bool visible = m_ptr->ml;

			if (test_hit_fire(chance - cur_dis, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				if (!visible)
					msg_format("The %s finds a mark.", o_name);
				else
				{
					char m_name[80];
					monster_desc(m_name, m_ptr, 0);
					msg_format("The %s hits %s.", o_name, m_name);
					if (m_ptr->ml)
					{
						if (!p_ptr->image) monster_race_track(m_ptr->ap_r_idx);
						health_track(c_ptr->m_idx);
					}
				}

				/***** The Damage Calculation!!! *****/
				dd = info->o_ptr->dd;
				tdam = damroll(dd, info->o_ptr->ds);				
				tdam = tot_dam_aux(info->o_ptr, tdam, m_ptr, 0, TRUE);
				tdam = critical_shot(info->o_ptr->weight, info->o_ptr->to_h, tdam);
				tdam += info->o_ptr->to_d;
				tdam *= info->mult;
				tdam += p_ptr->to_d_m;
				if (tdam < 0) tdam = 0;
				tdam = mon_damage_mod(m_ptr, tdam, FALSE);
				tdam -= (tdam * ((r_ptr->ac < 200) ? r_ptr->ac : 200) / 1200);

				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, extract_note_dies(real_r_ptr(m_ptr))))
				{
					/* Dead monster */
				}
				else
				{
					message_pain(c_ptr->m_idx, tdam);
					if (tdam > 0)
						anger_monster(m_ptr);

					if (tdam > 0 && m_ptr->cdis > 1 && allow_ticked_off(r_ptr))
					{
						if (!(m_ptr->smart & SM_TICKED_OFF))
						{
							if (mut_present(MUT_PEERLESS_SNIPER) && !one_in_(5))
							{
							}
							else
							{
								char m_name[80];
								monster_desc(m_name, m_ptr, 0);
								msg_format("%^s is ticked off!", m_name);
								m_ptr->smart |= SM_TICKED_OFF;
							}
						}
					}

					if (fear && m_ptr->ml)
					{
						char m_name[80];
						sound(SOUND_FLEE);
						monster_desc(m_name, m_ptr, 0);
						msg_format("%^s flees in terror!", m_name);
					}

					{
						char m_name[80];
						int odds = 5;
						monster_desc(m_name, m_ptr, 0);
				
						if (one_in_(odds))
						{
							if (r_ptr->flags3 & RF3_NO_CONF)
							{
								if (is_original_ap_and_seen(m_ptr)) r_ptr->r_flags3 |= RF3_NO_CONF;
								msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
							}
							else if (mon_save_p(m_ptr->r_idx, A_STR))
							{
								msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
							}
							else
							{
								msg_format(T("%^s appears confused.", "%^sは混乱したようだ。"), m_name);
								set_monster_confused(c_ptr->m_idx, MON_CONFUSED(m_ptr) + 10 + randint0(p_ptr->lev) / 5);
							}
						}

						if (p_ptr->lev >= 20 && one_in_(odds))
						{
							if (r_ptr->flags3 & RF3_NO_SLEEP)
							{
								if (is_original_ap_and_seen(m_ptr)) r_ptr->r_flags3 |= RF3_NO_SLEEP;
								msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
							}
							else if (mon_save_p(m_ptr->r_idx, A_STR))
							{
								msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
							}
							else
							{
								msg_format(T("%^s is knocked out.", ), m_name);
								set_monster_csleep(c_ptr->m_idx, MON_CSLEEP(m_ptr) + 500);
							}
						}

						if (p_ptr->lev >= 45 && one_in_(odds))
						{
							if (r_ptr->flags3 & RF3_NO_STUN)
							{
								if (is_original_ap_and_seen(m_ptr)) r_ptr->r_flags3 |= RF3_NO_STUN;
								msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
							}
							else if (mon_save_p(m_ptr->r_idx, A_STR))
							{
								msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
							}
							else
							{
								msg_format(T("%^s is stunned.", ), m_name);
								set_monster_stunned(c_ptr->m_idx, MAX(MON_STUNNED(m_ptr), 2));
							}
						}
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	if (info->come_back)
	{
		int i;
		for (i = cur_dis; i >= 0; i--)
		{
			y = GRID_Y(path[i]);
			x = GRID_X(path[i]);
			if (panel_contains(y, x) && player_can_see_bold(y, x))
			{
				char c = object_char(info->o_ptr);
				byte a = object_attr(info->o_ptr);

				/* Draw, Hilite, Fresh, Pause, Erase */
				print_rel(c, a, y, x);
				move_cursor_relative(y, x);
				Term_fresh();
				Term_xtra(TERM_XTRA_DELAY, msec);
				lite_spot(y, x);
				Term_fresh();
			}
			else
			{
				/* Pause anyway, for consistancy */
				Term_xtra(TERM_XTRA_DELAY, msec);
			}
		}
		msg_format("Your %s comes back to you.", o_name);
	}
	else
	{
		/* Record the actual location of the toss so we can drop the object here if required */
		info->tx = x;
		info->ty = y;
	}
}

static void _throw_weapon_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Throw Weapon");
		break;
	case SPELL_DESC:
		var_set_string(res, "Throws your leading weapon, which might return to you.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_club_toss())
			var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _trade_blows_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Trade Blows");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, you are somewhat exposed.  However, you will retaliate whenever a monster hits you.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (_get_toggle() == TOGGLE_TRADE_BLOWS)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_TRADE_BLOWS);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_TRADE_BLOWS)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _tumble_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Tumble");
		break;
	case SPELL_DESC:
		var_set_string(res, "Move 1 square in a direction and displace any monster originally there.");
		break;
	case SPELL_CAST:
	{
		int dir, tx, ty;
		cave_type *c_ptr;
		bool can_enter = FALSE;

		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}
		if (!get_rep_dir2(&dir)) return;
		tx = px + ddx[dir];
		ty = py + ddy[dir];

		c_ptr = &cave[ty][tx];

		if (!c_ptr->m_idx)
		{
			if (player_can_enter(c_ptr->feat, 0))
			{
				move_player_effect(ty, tx, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
				var_set_bool(res, TRUE);
			}
		}
		else
		{
			if (player_can_enter(c_ptr->feat, 0))
			{
				set_monster_csleep(c_ptr->m_idx, 0);
				move_player_effect(ty, tx, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
				update_mon(c_ptr->m_idx, TRUE);
				var_set_bool(res, TRUE);
			}
		}
		break;
	}
	case SPELL_ENERGY:
	{
		int energy = 100;
		if (p_ptr->action == ACTION_HAYAGAKE) 
			energy = energy * (45-(p_ptr->lev/2)) / 100;
		var_set_int(res, energy);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _uber_weapon_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Superspecialization");
		break;
	case SPELL_DESC:
		var_set_string(res, "Chooses a single weapon for ultimate power.  You may only choose once, so choose wisely!");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You must equip your speciality weapon.");
			return;
		}
		if (_make_uber_weapon())
			var_set_bool(res, TRUE);		
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _vicious_strike_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Vicious Strike");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attacks an opponent very powerfully, but you become greatly exposed by the effort.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		var_set_bool(res, FALSE);
		if (!_check_speciality2_equip())
		{
			msg_print("Failed!  You do not feel comfortable with your weapon.");
			return;
		}


		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];

		if (cave[y][x].m_idx)
		{
			py_attack(y, x, WEAPONMASTER_VICIOUS_STRIKE);
			set_tim_vicious_strike(p_ptr->tim_vicious_strike + 10, FALSE);
		}
		else
		{
			msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
			return;
		}

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}


/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define _MAX_OBJECTS_PER_SPECIALITY		32
#define _MAX_SPECIALITIES				11
#define _MAX_SPELLS_PER_SPECIALITY		10

int weaponmaster_get_toggle(void)
{
	/* exposed for prtstatus() in xtra1.c 
	   this is easier than rewriting the status code so that classes can maintain it!
	*/
	int result = TOGGLE_NONE;
	if (p_ptr->pclass == CLASS_WEAPONMASTER)
		result = _get_toggle();
	return result;
}

void weaponmaster_set_toggle(int toggle)
{
	if (p_ptr->pclass == CLASS_WEAPONMASTER)
		_set_toggle(toggle);
}

  
typedef struct {
	byte tval;
	byte sval;
} _object_kind;

typedef struct {
	cptr name;
	cptr help;
	int slot1, slot2;
	_object_kind objects[_MAX_OBJECTS_PER_SPECIALITY];	/* There is always a sentinel at the end */
	spell_info spells[_MAX_SPELLS_PER_SPECIALITY];		/* There is always a sentinel at the end */
	_object_kind birth_obj;
} _speciality;

/*	p_ptr->speciality1 indexes into _specialities.
	p_ptr->speciality2 indexes into _speciality.objects.
	Both these indices are persisted in savefiles and are chosen
	by the player at startup, so moving things around is unwise unless
	you put code to fix up old savefiles in load.c.
*/
static _speciality _specialities[_MAX_SPECIALITIES] = {
	{ "Axes",
	  "The mighty axe!  You blows will cleave with damage unsurpassed!",
	  INVEN_RARM, INVEN_LARM,
	  { { TV_POLEARM, SV_BATTLE_AXE },
	    { TV_POLEARM, SV_BEAKED_AXE },
	    { TV_POLEARM, SV_BROAD_AXE },
	    { TV_POLEARM, SV_LOCHABER_AXE },
	    { TV_POLEARM, SV_GREAT_AXE },
		{ 0, 0 },
	  },
	  {
		{ 10,   0,  0, _power_attack_spell },
		{ 15,  10,  0, berserk_spell },
		{ 25,  12,  0, _crusaders_strike_spell },
		{ 35,  25,  0, _strike_vulnerability_spell },
		{ 35,  25,  0, _vicious_strike_spell },
	    { -1,   0,  0, NULL },
	  },
	  { TV_POLEARM, SV_BROAD_AXE },
	},
	{ "Bows",
	  "",
	  INVEN_BOW, -1,
	  { { TV_BOW, SV_SHORT_BOW },
	    { TV_BOW, SV_LONG_BOW },
		{ 0, 0 },
	  },
	  {
	    { -1,   0,  0, NULL },
	  },
	  { TV_BOW, SV_SHORT_BOW },
	},
	{ "Clubs",
	  "You will seek to club your opponents senseless!  This speciality gains passive "
	  "status effects against monsters, such as confusion, knock out and stunning.",
	  INVEN_RARM, INVEN_LARM,
	  { { TV_HAFTED, SV_BALL_AND_CHAIN },
	    { TV_HAFTED, SV_CLUB },
	    { TV_HAFTED, SV_FLAIL },
	    { TV_HAFTED, SV_GREAT_HAMMER },
	    { TV_HAFTED, SV_LEAD_FILLED_MACE },
	    { TV_HAFTED, SV_MACE },
	    { TV_HAFTED, SV_MACE_OF_DISRUPTION },
	    { TV_HAFTED, SV_MORNING_STAR },
	    { TV_HAFTED, SV_TWO_HANDED_FLAIL },
	    { TV_HAFTED, SV_WAR_HAMMER },
		{ 0, 0 },
	  },
	  {
		{  5,   0,  0, _combat_expertise_spell },
		{ 10,  12,  0, _cunning_strike_spell },
		{ 15,   0,  0, _stone_bones_spell },
		{ 30,  50,  0, _smite_evil_spell },
		{ 35,   0,  0, _trade_blows_spell },
	    { -1,   0,  0, NULL },
	  },
	  { TV_HAFTED, SV_CLUB },
	},
	{ "Crossbows",
	  "",
	  INVEN_BOW, -1,
	  { { TV_BOW, SV_LIGHT_XBOW },
	    { TV_BOW, SV_HEAVY_XBOW },
		{ 0, 0 },
	  },
	  {
	    { -1,   0,  0, NULL },
	  },
	  { TV_BOW, SV_LIGHT_XBOW },
	},
	{ "Daggers",
	  "A knife in the back!  This speciality favors dual wielding and rogue-like behavior and "
	  "allows you to unleash extremely strong attacks by sacrificing weapons. "
	  "You will have some ranged combat abilities.",
	  INVEN_RARM, INVEN_LARM,
	  { { TV_SWORD, SV_BASILLARD },
		{ TV_SWORD, SV_BROKEN_DAGGER },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SWORD, SV_FALCON_SWORD },
	    { TV_SWORD, SV_MAIN_GAUCHE },
		{ TV_SWORD, SV_NINJATO },
		{ TV_SWORD, SV_RAPIER },
		{ TV_SWORD, SV_SABRE },
		{ TV_SWORD, SV_TANTO },
		{ 0, 0 },
	  },
	  {
	    {  5,   5,  0, _dagger_toss_spell },
		{ 15,   0,  0, _flying_dagger_spell },
		{ 25,  10,  0, strafing_spell },
		{ 35,   0,  0, _shadow_stance_spell },
		{ 40,   0,  0, _frenzy_spell },
	    { -1,   0,  0, NULL },
	  },
	  { TV_SWORD, SV_DAGGER },
	},
	{ "Polearms",
	  "You don a grim face before setting out to reap your harvest of death.  You will swing "
	  "your weapon wide often affecting multiple surrounding opponents.",
	  INVEN_RARM, INVEN_LARM,
	  { 
	    { TV_POLEARM, SV_AWL_PIKE },
	    { TV_POLEARM, SV_BROAD_SPEAR },
	    { TV_POLEARM, SV_DEATH_SCYTHE },
	    { TV_POLEARM, SV_HALBERD },
	    { TV_POLEARM, SV_FAUCHARD },
	    { TV_POLEARM, SV_GLAIVE },
	    { TV_POLEARM, SV_GUISARME },
	    { TV_POLEARM, SV_LUCERNE_HAMMER },
	    { TV_POLEARM, SV_NAGINATA },
	    { TV_POLEARM, SV_PIKE },
	    { TV_POLEARM, SV_SCYTHE },
	    { TV_POLEARM, SV_SCYTHE_OF_SLICING },
	    { TV_POLEARM, SV_SPEAR },
		{ 0, 0 },
	  },
	  {
	    {  5,   0,  0, _many_strike_spell },
		{ 10,   5,  0, _enclose_spell },
		{ 15,  15,  0, _knock_back_spell },
		{ 25,   0,  0, _piercing_strike_spell },
		{ 35,   0,  0, _trip_spell },
		{ 40,  40,  0, _reaping_spell },
	    { -1,   0,  0, NULL },
	  },
	  { TV_POLEARM, SV_SPEAR },
	},
	{ "Shields",
	  "",
	  INVEN_LARM, INVEN_RARM,
	  { 
		{ TV_SHIELD, SV_DRAGON_SHIELD },
		{ TV_SHIELD, SV_KNIGHT_SHIELD },
		{ TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
		{ TV_SHIELD, SV_LARGE_METAL_SHIELD },
		{ TV_SHIELD, SV_MIRROR_SHIELD },
	    { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
		{ TV_SHIELD, SV_SMALL_METAL_SHIELD },
		{ 0, 0 },
	  },
	  {
	    { 10,  0,  0, _shield_bash_spell },
		{ 15, 10,  0, _desperation_spell },
		{ 30,  0,  0, _bulwark_spell },
		{ 35, 50,  0, _sanctuary_spell },
		{ 40,  0,  0, _shield_revenge_spell },
	    { -1,  0,  0, NULL },
	  },
	  { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
	},
	{ "Slings",
	  "Watch out, Goliath!",
	  INVEN_BOW, -1,
	  { { TV_BOW, SV_SLING },
		{ 0, 0 },
	  },
	  {
	    {  5,   5,  0, _bouncing_pebble_spell },
		{ 15,  15,  0, _many_shot_spell },
		{ 25,   0,  0, _shot_on_the_run_spell },
		{ 30,  15,  0, _greater_many_shot_spell },
		{ 35,   0,  0, _rapid_shot_spell },
	    { -1,   0,  0, NULL },
	  },
	  { TV_BOW, SV_SLING },
	},
	{ "Staves",
	  "Monkey King!  You will battle opponents with a flurry of blows from your mighty "
	  "staff.  You may eventually clone yourself at great cost.",
	  INVEN_RARM, INVEN_LARM,
	  { 
	    { TV_HAFTED, SV_BO_STAFF },
		{ TV_HAFTED, SV_JO_STAFF },
	    { TV_HAFTED, SV_QUARTERSTAFF },
		{ TV_HAFTED, SV_WIZSTAFF },
		{ 0, 0 },
	  },
	  {
		{ 10,  0, 0, _flurry_of_blows_spell },
		{ 15, 15, 0, _vault_attack_spell },
		{ 25, 25, 0, _circle_kick_spell },
		{ 30,  0, 0, _greater_flurry_spell },
		{ 40,  0, 0, _monkey_king_spell },
	    { -1,  0, 0, NULL },
	  },
	  { TV_HAFTED, SV_QUARTERSTAFF },
	},
	{ "Swords",
	  "You will become a true swordmaster!  Mastery of the blade will augment "
	  "your weapon with elemental, vorpal or vampiric powers.",
	  INVEN_RARM, INVEN_LARM,
	  { { TV_SWORD, SV_BASTARD_SWORD } ,
	    { TV_SWORD, SV_BROKEN_SWORD } ,
	    { TV_SWORD, SV_BLADE_OF_CHAOS } ,
	    { TV_SWORD, SV_BROAD_SWORD } ,
	    { TV_SWORD, SV_CLAYMORE } ,
	    { TV_SWORD, SV_CUTLASS } ,
	    { TV_SWORD, SV_DIAMOND_EDGE } ,
	    { TV_SWORD, SV_ESPADON } ,
	    { TV_SWORD, SV_EXECUTIONERS_SWORD } ,
	    { TV_SWORD, SV_FLAMBERGE } ,
	    { TV_SWORD, SV_GREAT_SCIMITAR } , /* Falchion */
	    { TV_SWORD, SV_KATANA } ,
	    { TV_SWORD, SV_LONG_SWORD } ,
	    { TV_SWORD, SV_KHOPESH } ,
	    { TV_SWORD, SV_NO_DACHI },
	    { TV_SWORD, SV_SCIMITAR } ,
	    { TV_SWORD, SV_SHORT_SWORD } ,
	    { TV_SWORD, SV_SMALL_SWORD } ,
	    { TV_SWORD, SV_TULWAR } ,
	    { TV_SWORD, SV_TWO_HANDED_SWORD } ,
	    { TV_SWORD, SV_WAKIZASHI } ,
	    { TV_SWORD, SV_ZWEIHANDER } ,
		{ 0, 0 },
	  },
	  {
		{  5,   0,  0, _burning_blade_spell },
		{ 10,   0,  0, _ice_blade_spell },
		{ 15,   0,  0, _thunder_blade_spell },
		{ 25,   0,  0, _blood_blade_spell },
		{ 30,   0,  0, _holy_blade_spell },
		{ 35,   0,  0, _order_blade_spell },
		{ 40,   0,  0, _wild_blade_spell },
	    { -1,   0,  0, NULL },
	  },
	  { TV_SWORD, SV_LONG_SWORD } ,
	},
	{ "Diggers",
	  "A master of digging.  You prefer rocky enclosures and don't mind "
	  "lugging around a corpse or two.",
	  INVEN_RARM, INVEN_LARM,
	  { 
		{ TV_DIGGING, SV_SHOVEL},
		{ TV_DIGGING, SV_GNOMISH_SHOVEL},
		{ TV_DIGGING, SV_DWARVEN_SHOVEL},
		{ TV_DIGGING, SV_PICK},
		{ TV_DIGGING, SV_ORCISH_PICK},
		{ TV_DIGGING, SV_DWARVEN_PICK},
		{ TV_DIGGING, SV_MATTOCK},
		{ 0, 0 },
	  },
	  {
	    {  5, 10,  0, _bury_dead_spell },
		{ 10,  0,  0, _strength_of_the_undertaker_spell },
		{ 20, 20,  0, _tunnel_spell },
		{ 25,  0,  0, _stoicism_spell },
		{ 30, 40,  0, _barricade_spell },
		{ 35, 40,  0, _calamity_of_the_living_spell },
		{ 40,  0,  0, _industrious_mortician_spell },
	    { -1,  0,  0, NULL },
	  },
	  { TV_DIGGING, SV_PICK },
	},
};

static bool _check_speciality1_aux(object_type *o_ptr)
{
	int i;
	_speciality *ptr = &_specialities[p_ptr->speciality1];

	for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
	{
		if (ptr->objects[i].tval == 0) break;
		if (ptr->objects[i].tval == o_ptr->tval && ptr->objects[i].sval == o_ptr->sval) return TRUE;
	}

	return FALSE;
}

static bool _check_speciality1_equip(void)
{
	_speciality *ptr = &_specialities[p_ptr->speciality1];
	int slot1 = ptr->slot1;
	int slot2 = ptr->slot2;

	_dual_wield = FALSE;

	if (strcmp(ptr->name, "Shields") == 0)
	{
		bool left_check = FALSE;
		bool right_check = FALSE;

		if (inventory[INVEN_RARM].k_idx)
		{
			right_check = _check_speciality1_aux(&inventory[INVEN_RARM]);
			if (!object_is_weapon(&inventory[INVEN_RARM]) && !right_check) return FALSE;
		}

		if (inventory[INVEN_LARM].k_idx)
		{
			left_check = _check_speciality1_aux(&inventory[INVEN_LARM]);
			if (!object_is_weapon(&inventory[INVEN_LARM]) && !left_check) return FALSE;
		}

		return left_check || right_check;
	}

	/* First slot should always match */
	if (!_check_speciality1_aux(&inventory[slot1])) return FALSE;

	/* Second slot might be the shield slot for weaponmasters, or the weapon slot for shieldmasters
	   Try to handle these cases */
	if (slot2 != -1 && inventory[slot2].tval != 0)
	{
		_dual_wield = TRUE;
		if ( !object_is_shield(&inventory[slot2]) 
			&& !_check_speciality1_aux(&inventory[slot2])) return FALSE;
	}

	return TRUE;
}

static bool _check_speciality2_aux(object_type *o_ptr)
{
	_object_kind kind = _specialities[p_ptr->speciality1].objects[p_ptr->speciality2];
	if (o_ptr->tval == kind.tval && o_ptr->sval == kind.sval) return TRUE;
	return FALSE;
}

static bool _check_speciality2_equip(void)
{
	_speciality *ptr = &_specialities[p_ptr->speciality1];
	int slot1 = ptr->slot1;
	int slot2 = ptr->slot2;

	if (strcmp(ptr->name, "Shields") == 0)
	{
		bool left_check = FALSE;
		bool right_check = FALSE;

		if (inventory[INVEN_RARM].k_idx)
		{
			right_check = _check_speciality2_aux(&inventory[INVEN_RARM]);
			if (!object_is_weapon(&inventory[INVEN_RARM]) && !right_check) return FALSE;
		}

		if (inventory[INVEN_LARM].k_idx)
		{
			left_check = _check_speciality2_aux(&inventory[INVEN_LARM]);
			if (!object_is_weapon(&inventory[INVEN_LARM]) && !left_check) return FALSE;
		}

		return left_check || right_check;
	}

	/* First slot should always match */
	if (!_check_speciality2_aux(&inventory[slot1])) return FALSE;

	/* Second slot might be the shield slot for weaponmasters, or the weapon slot for shieldmasters
	   Try to handle these cases 
	   Note: A non-speciality2 weapon in slot2 disqualifies all speciality2 abilities!! */
	if (slot2 != -1 && inventory[slot2].tval != 0)
	{
		if (strcmp(ptr->name, "Shields") == 0)
		{
			if ( !object_is_weapon(&inventory[slot2]) 
			  && !_check_speciality2_aux(&inventory[slot2])) return FALSE;
		}
		else
		{
			if ( !object_is_shield(&inventory[slot2]) 
			  && !_check_speciality2_aux(&inventory[slot2])) return FALSE;
		}
	}

	return TRUE;
}

static bool _make_uber_weapon(void)
{
	char name[MAX_NLEN];
	char prompt[500];
	object_type *o_ptr = &inventory[_specialities[p_ptr->speciality1].slot1];
	bool result = FALSE;

	object_desc(name, o_ptr, 0);
	sprintf(prompt, "You are about to specialize in %s.  This choice is permanent.  Are you sure?", name);
	if (get_check(prompt))
	{
		if (o_ptr->tval == TV_BOW)
		{
			o_ptr->to_h += 17;
			o_ptr->to_d += 3;
		}
		else if (object_is_melee_weapon(o_ptr))
		{
			o_ptr->to_h += 10;
			o_ptr->to_d += 10;
		}
		else if (object_is_shield(o_ptr))
		{
			o_ptr->to_h += 5;
			o_ptr->to_d += 5;
			o_ptr->to_a += 10;
		}
		add_flag(o_ptr->art_flags, TR_SIGNATURE);
		add_flag(o_ptr->art_flags, TR_FREE_ACT);
		add_flag(o_ptr->art_flags, TR_RES_DISEN);
		p_ptr->speciality3 = 1;

		result = TRUE;
	}

	return result;
}


static cptr _speciality_name(menu_choices choices, int which) {
	return _specialities[which].name;
}

static cptr _weapon_name(int speciality1, int speciality2)
{
	_object_kind *ptr = &_specialities[speciality1].objects[speciality2];
	int k_idx = lookup_kind(ptr->tval, ptr->sval);
	static char buf[255];
	strip_name(buf, k_idx);
	return buf; /* Yuk ... but should be safe */
}

static cptr _speciality2_name(menu_choices choices, int which) {
	return _weapon_name(p_ptr->speciality1, which);
}

static cptr _speciality_help(menu_choices choices, int which) {
	return _specialities[which].help;
}

static int _speciality_color(menu_choices choices, int which) {
	if (_specialities[which].spells[0].level < 0)
		return TERM_SLATE;
	return TERM_WHITE;
}

static int _count_weapons(int speciality)
{
	int result = 0;
	int i = 0;

	for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
	{
		if (_specialities[speciality].objects[i].tval == 0) break;
		result++;
	}
	return result;
}

cptr weaponmaster_speciality1_name(void)
{
	cptr result = "";
	if (p_ptr->pclass == CLASS_WEAPONMASTER)
		result = _specialities[p_ptr->speciality1].name;
	return result;
}

int weaponmaster_specialty2_k_idx(void)
{
	_object_kind kind = _specialities[p_ptr->speciality1].objects[p_ptr->speciality2];
	return lookup_kind(kind.tval, kind.sval);
}

cptr weaponmaster_speciality2_name(void)
{
	static char buf[MAX_NLEN];
	object_type forge;
	object_prep(&forge, weaponmaster_specialty2_k_idx());
	forge.number = 2;
	object_desc(buf, &forge, OD_OMIT_PREFIX|OD_NAME_ONLY);
	return buf;
}

static int _get_spells(spell_info* spells, int max)
{
	int i;
	int ct = 0;
	
	for (i = 0; ; i++)
	{
		spell_info *base = &_specialities[p_ptr->speciality1].spells[i];
		if (base->level <= 0) break;
		if (ct >= max) break;
		if (base->level <= p_ptr->lev)
		{
			spell_info* current = &spells[ct];
			current->fn = base->fn;
			current->level = base->level;
			current->cost = base->cost;
			current->fail = 0;			
			ct++;
		}
	}
	if (p_ptr->lev == 50 && p_ptr->speciality3 == 0)
	{
		spell_info* current = &spells[ct];
		current->fn = _uber_weapon_spell;
		current->level = 50;
		current->cost = 0;
		current->fail = 0;			
		ct++;
	}

	if (ct == 0)
		msg_print("You need more experience.  Why not kill something?");

	return ct;
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "skill";
		me.use_hp = TRUE;
		init = TRUE;
	}
	return &me;
}

int _prompt_for_speciality1(void)
{
	menu_list_t list = { "Choose a speciality.", "Browse which speciality?", NULL,
						_speciality_name, _speciality_help, _speciality_color, 
						_specialities, _MAX_SPECIALITIES};

	return menu_choose(&list);
}

int _prompt_for_speciality2(void)
{
	menu_list_t list = { "Choose your primary weapon in this group.", NULL, NULL,
						_speciality2_name, NULL, NULL, 
						_specialities[p_ptr->speciality1].objects, _count_weapons(p_ptr->speciality1)};

	return menu_choose(&list);
}

void _on_birth(void)
{
	object_type forge;
	_object_kind kind;
	int i;

	/* Get the broad specialization */
	for (;;)
	{
		int idx = _prompt_for_speciality1();
		if (idx >= 0)
		{
			char buf[255];
			sprintf(buf, "You will specialize in %s.  Are you sure?", _specialities[idx].name);
			if (get_check(buf))
			{
				p_ptr->speciality1 = idx;
				break;
			}
		}
		else
		{
			msg_print("Before you may begin you must choose your weapon specialities.");
			msg_print(NULL);
		}
	}

	/* Choose a single weapon in the group */
	for (;;)
	{
		int idx;
		
		if (_count_weapons(p_ptr->speciality1) == 1)
		{
			p_ptr->speciality2 = 0;
			break;
		}

		idx = _prompt_for_speciality2();

		if (idx >= 0)
		{
			char buf[255];
			sprintf(buf, "You will specialize in %s.  Are you sure?", _weapon_name(p_ptr->speciality1, idx));
			if (get_check(buf))
			{
				p_ptr->speciality2 = idx;
				break;
			}
		}
		else
		{
			msg_print("Before you may begin you must choose your weapon specialities.");
			msg_print(NULL);
		}
	}

	/* The ultimate speciality weapon will be chosen as a CL50 ability */
	p_ptr->speciality3 = FALSE;

	/* Give the player a starting weapon from this group */
	kind = _specialities[p_ptr->speciality1].birth_obj;
	object_prep(&forge, lookup_kind(kind.tval, kind.sval));
	add_outfit(&forge);

	if (kind.tval == TV_BOW)
	{
		switch (kind.sval)
		{
		case SV_SLING:
			object_prep(&forge, lookup_kind(TV_SHOT, SV_AMMO_NORMAL));
			forge.number = (byte)rand_range(15, 20);
			add_outfit(&forge);
			break;
		case SV_SHORT_BOW:
		case SV_LONG_BOW:
			object_prep(&forge, lookup_kind(TV_ARROW, SV_AMMO_NORMAL));
			forge.number = (byte)rand_range(15, 20);
			add_outfit(&forge);
			break;
		case SV_LIGHT_XBOW:
		case SV_HEAVY_XBOW:
			object_prep(&forge, lookup_kind(TV_BOLT, SV_AMMO_NORMAL));
			forge.number = (byte)rand_range(15, 20);
			add_outfit(&forge);
			break;
		}
	}

	for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
	{
		kind = _specialities[p_ptr->speciality1].objects[i];
		if (kind.tval == 0) break;

		if (kind.tval != TV_SHIELD)
			p_ptr->weapon_exp[kind.tval-TV_WEAPON_BEGIN][kind.sval] = WEAPON_EXP_BEGINNER;
	}

	weaponmaster_adjust_skills();
}

static void _set_max_skill(int tval, int skill)
{
	int j;
	for (j = 0; j < 64; j++)
		s_info[p_ptr->pclass].w_max[tval - TV_WEAPON_BEGIN][j] = skill;
}

void weaponmaster_adjust_skills(void)
{
	int i, j;
	_object_kind kind;

	/* Fix up skills for Speciality.  This needs to be called every time the game is loaded! */
	/* Bang everything in class (melee, bows, shields) down to unskilled max */
	switch (_specialities[p_ptr->speciality1].slot1)
	{
	case INVEN_RARM:
		_set_max_skill(TV_DIGGING, WEAPON_EXP_UNSKILLED);
		_set_max_skill(TV_HAFTED, WEAPON_EXP_UNSKILLED);
		_set_max_skill(TV_POLEARM, WEAPON_EXP_UNSKILLED);
		_set_max_skill(TV_SWORD, WEAPON_EXP_UNSKILLED);
		break;

	case INVEN_BOW:
		_set_max_skill(TV_BOW, WEAPON_EXP_UNSKILLED);
		break;

	case INVEN_LARM:
		/* TODO: We do not keep skills for shields.  Probably never will, either ... */
		break;
	}

	/* Now make favored weapons "masterable" */
	for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
	{
		kind = _specialities[p_ptr->speciality1].objects[i];
		if (kind.tval == 0) break;

		s_info[p_ptr->pclass].w_max[kind.tval-TV_WEAPON_BEGIN][kind.sval] = WEAPON_EXP_MASTER;
	}

	/* Patch up current skills since we keep changing the allowed maximums */
	for (i = 0; i < 5; i++)
	{
		for (j = 0; j < 64; j++)
		{
			if (p_ptr->weapon_exp[i][j] > s_info[p_ptr->pclass].w_max[i][j])
				p_ptr->weapon_exp[i][j] = s_info[p_ptr->pclass].w_max[i][j];
		}
	}

	/* Shooters swap Bow Skills with Melee Skills */
	if (_specialities[p_ptr->speciality1].slot1 == INVEN_BOW)
	{
		s16b tmp;
		tmp = cp_ptr->c_thn;
		cp_ptr->c_thn = cp_ptr->c_thb;
		cp_ptr->c_thb = tmp;

		tmp = cp_ptr->x_thn;
		cp_ptr->x_thn = cp_ptr->x_thb;
		cp_ptr->x_thb = tmp;
	}
}

static void _calc_bonuses(void)
{
	static bool last_spec1 = FALSE;
	static bool last_spec2 = FALSE;
	static bool init = FALSE;
	bool spec1 = _check_speciality1_equip();
	bool spec2 = _check_speciality2_equip();

	p_ptr->speciality1_equip = spec1;
	p_ptr->speciality2_equip = spec2;

	/* Handle cases where user swaps in unfavorable gear */
	if (!spec2 && _get_toggle() != TOGGLE_NONE)
	{
		/* Triggering a recursive call to calc_bonuses would be bad ... */
		/*	_set_toggle(TOGGLE_NONE); */
		/* This assumes all bonus calcs are handled here and in _calc_weapon_bonuses() */
		_shield_bash_toggle_hack(TOGGLE_NONE);
		p_ptr->magic_num1[0] = TOGGLE_NONE;
		p_ptr->redraw |= (PR_STATUS);
		redraw_stuff();
	}

	if (!spec2 && p_ptr->action == ACTION_HAYAGAKE)
		set_action(ACTION_NONE);


	if (strcmp(_specialities[p_ptr->speciality1].name, "Slings") == 0)
	{
		object_type *o_ptr = &inventory[INVEN_BOW];

		if (spec1)
		{
			p_ptr->num_fire += 300 * p_ptr->lev/100;
			p_ptr->return_ammo = TRUE;

			if (p_ptr->lev >= 20)
				p_ptr->big_shot = TRUE;

			if (p_ptr->lev >= 45)
				p_ptr->unlimited_quiver = TRUE;
		}

		if (spec2)
		{
			if (p_ptr->lev >= 10)
				p_ptr->painted_target = TRUE;

			if (p_ptr->lev >= 40)
				p_ptr->num_fire += 100;
		}
	} 
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Daggers") == 0)
	{
		if (spec1) 
		{
			p_ptr->easy_2weapon = TRUE;
			if (p_ptr->lev >= 20 && _dual_wield)
			{
				p_ptr->to_a += 10 + p_ptr->lev/2;
				p_ptr->dis_to_a += 10 + p_ptr->lev/2;
			}
		}
		if (spec2)
		{
			if (p_ptr->lev >= 10)
				p_ptr->skill_stl += 2;

			if (p_ptr->lev >= 30)
				p_ptr->sneak_attack = TRUE;

			switch (_get_toggle())
			{
			case TOGGLE_FLYING_DAGGER_STANCE:
				p_ptr->stat_add[A_CON] -= 4;
				break;

			case TOGGLE_SHADOW_STANCE:
				p_ptr->to_d_b -= 10;
				p_ptr->to_d_m -= 10;
				p_ptr->skill_stl += p_ptr->lev/12;
				break;
			}
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Clubs") == 0)
	{
		if (spec2)
		{
			if (p_ptr->lev >= 40)
				p_ptr->enhanced_crit = TRUE;

			switch (_get_toggle())
			{
			case TOGGLE_TRADE_BLOWS:
				p_ptr->to_a -= 50;
				p_ptr->dis_to_a -= 50;
				break;
			case TOGGLE_COMBAT_EXPERTISE:
				p_ptr->to_a += 2*p_ptr->lev;
				p_ptr->dis_to_a += 2*p_ptr->lev;
				break;
			}
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Axes") == 0)
	{
		if (p_ptr->tim_vicious_strike)
		{
			p_ptr->to_a -= 120;
			p_ptr->dis_to_a -= 120;
			/* TODO: AC should not go below 0 ... */
		}

		if (spec2)
		{
			if (p_ptr->lev >= 5)
				p_ptr->skill_dig += (5 * 20); /* As if you had a +5 digger ... */

			if (p_ptr->lev >= 30)
				p_ptr->cleave = TRUE;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Swords") == 0)
	{
		if (spec1)
		{
			if (p_ptr->lev >= 20)
			{
				/* Hackery to keep the status bar up to date ... sigh */
				if (!IS_HERO())
				{
					p_ptr->constant_hero = TRUE;
					p_ptr->redraw |= (PR_STATUS);
				}
				else
					p_ptr->constant_hero = TRUE;				
			}

			if (p_ptr->lev >= 45)
				p_ptr->vorpal = TRUE;
		}
		else if (last_spec1 && p_ptr->lev >= 20)
		{
			p_ptr->redraw |= (PR_STATUS);
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Polearms") == 0)
	{
		if (spec1)
		{
			if (p_ptr->lev >= 45)
				p_ptr->whirlwind = TRUE;
		}

		if (spec2)
		{
			if (p_ptr->lev >= 30)
			{
				if (p_ptr->entrench_ct >= 3) 
				{
					p_ptr->entrenched = TRUE;
					p_ptr->redraw |= PR_STATUS;

					p_ptr->to_a += 20;
					p_ptr->dis_to_a += 20;
				}
			}
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Shields") == 0)
	{
		if (spec1)
		{
			if (p_ptr->lev >= 20)
				p_ptr->inven_prot = TRUE;

			if (p_ptr->lev >= 45)
			{
				p_ptr->resist_acid = TRUE;
				p_ptr->resist_cold = TRUE;
				p_ptr->resist_fire = TRUE;
				p_ptr->resist_elec = TRUE;
				p_ptr->reflect = TRUE;
			}
		}

		if (spec2)
		{
			/* Block: Shield AC doubled. */
			if (p_ptr->lev >= 5)
			{
				if (inventory[INVEN_RARM].k_idx && object_is_shield(&inventory[INVEN_RARM]))
				{
					p_ptr->to_a += k_info[inventory[INVEN_RARM].k_idx].ac;
					p_ptr->dis_to_a += k_info[inventory[INVEN_RARM].k_idx].ac;
					p_ptr->to_a += inventory[INVEN_RARM].to_a;
					if (object_is_known(&inventory[INVEN_RARM]))
						p_ptr->dis_to_a += inventory[INVEN_RARM].to_a;
				}
				if (inventory[INVEN_LARM].k_idx && object_is_shield(&inventory[INVEN_LARM]))
				{
					p_ptr->to_a += k_info[inventory[INVEN_LARM].k_idx].ac;
					p_ptr->dis_to_a += k_info[inventory[INVEN_LARM].k_idx].ac;
					p_ptr->to_a += inventory[INVEN_LARM].to_a;
					if (object_is_known(&inventory[INVEN_LARM]))
						p_ptr->dis_to_a += inventory[INVEN_LARM].to_a;
				}
			}

			/* Stalwart: +20 saving throws */
			if (p_ptr->lev >= 25)
				p_ptr->skill_sav += 20;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Staves") == 0)
	{
		if (spec1)
		{
			if (p_ptr->elaborate_defense)
			{
				p_ptr->to_a += 10 + p_ptr->lev*2/3;
				p_ptr->dis_to_a += 10 + p_ptr->lev*2/3;
			}

			if (p_ptr->lev >= 20)
				p_ptr->pspeed += 2;
		}

		if (spec2)
		{
			if (p_ptr->cloak_of_shadows && !p_ptr->elaborate_defense)
			{
				p_ptr->to_a += 10 + p_ptr->lev*2/3;
				p_ptr->dis_to_a += 10 + p_ptr->lev*2/3;
			}

			if (p_ptr->lev >= 35)
				p_ptr->lightning_reflexes = TRUE;
		}

		if (object_is_shield(&inventory[INVEN_RARM]) || object_is_shield(&inventory[INVEN_LARM]))
		{
			p_ptr->pspeed -= 5;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Diggers") == 0)
	{
		if (spec1)
		{
			p_ptr->skill_dig += (5 + p_ptr->lev/5) * 20;
			if (p_ptr->lev >= 45)
				p_ptr->kill_wall = TRUE;
		}
		if (spec2)
		{
			if (p_ptr->lev >= 15) /* Earthen Shield */
			{
				int dir, x, y;
				int count = 0;
				for (dir = 0; dir < 8; dir++)
				{
					y = py + ddy_ddd[dir];
					x = px + ddx_ddd[dir];
					if (cave_have_flag_bold(y, x, FF_HURT_ROCK))
						count++;
				}
				p_ptr->to_a += 7*count;
				p_ptr->dis_to_a += 7*count;
			}
			switch (_get_toggle())
			{
			case TOGGLE_STRENGTH_OF_THE_UNDERTAKER:
				if (_check_speciality2_aux(&inventory[INVEN_RARM]))
					p_ptr->stat_add[A_STR] += inventory[INVEN_RARM].pval;
				if (_check_speciality2_aux(&inventory[INVEN_LARM]))
					p_ptr->stat_add[A_STR] += inventory[INVEN_LARM].pval;
				break;

			case TOGGLE_STOICISM:
				if (_check_speciality2_aux(&inventory[INVEN_RARM]))
					p_ptr->stat_add[A_CON] += inventory[INVEN_RARM].pval;
				if (_check_speciality2_aux(&inventory[INVEN_LARM]))
					p_ptr->stat_add[A_CON] += inventory[INVEN_LARM].pval;
				break;
			}
		}
	}

	if (!p_ptr->painted_target)
	{
		p_ptr->painted_target_idx = 0;
		p_ptr->painted_target_ct = 0;
	}

	/* Message about favored gear */
	if (!init || spec1 != last_spec1 || spec2 != last_spec2)
	{
		int slot1 = _specialities[p_ptr->speciality1].slot1;
		if (!spec1)
		{
			switch (slot1)
			{
			case INVEN_BOW:
				msg_print("You do not feel comfortable with your shooter.");
				break;
			case INVEN_LARM:
				msg_print("You do not feel comfortable with your shield.");
				break;
			default:
				msg_print("You do not feel comfortable with your weapon.");
				break;
			}
		}
		else if (!spec2)
		{
			switch (slot1)
			{
			case INVEN_BOW:
				msg_print("Your shooter is OK, but you could do better.");
				break;
			case INVEN_LARM:
				msg_print("Your shield is OK, but you could do better.");
				break;
			default:
				msg_print("Your weapon is OK, but you could do better.");
				break;
			}
		}
		else if (init)
		{
			switch (slot1)
			{
			case INVEN_BOW:
				msg_print("You love your shooter.");
				break;
			case INVEN_LARM:
				msg_print("You love your shield.");
				break;
			default:
				msg_print("You love your weapon.");
				break;
			} 
		}

		init = TRUE;
		last_spec1 = spec1;
		last_spec2 = spec2;
	}
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
	if (strcmp(_specialities[p_ptr->speciality1].name, "Axes") == 0)
	{
	int spec1 = _check_speciality1_aux(o_ptr);
	int spec2 = _check_speciality2_aux(o_ptr);
		if (spec1 && p_ptr->speciality1_equip)
		{
			info_ptr->to_d += 5;
			info_ptr->dis_to_d += 5;
			if (p_ptr->ryoute) info_ptr->num_blow++;

			if (p_ptr->lev >= 20)
			{
				info_ptr->to_d += 10;
				info_ptr->dis_to_d += 10;
			}

			if (p_ptr->lev >= 45)
			{
				info_ptr->to_d += 15;
				info_ptr->dis_to_d += 15;
			}
		}

		if (spec2 && p_ptr->speciality2_equip)
		{
			switch (_get_toggle())
			{
			case TOGGLE_POWER_ATTACK:
				info_ptr->dis_to_h -= p_ptr->lev;
				info_ptr->to_h -= p_ptr->lev;
				if (p_ptr->ryoute)
				{
					info_ptr->dis_to_d += 2*p_ptr->lev/3;
					info_ptr->to_d += 2*p_ptr->lev/3;
				}
				else
				{
					info_ptr->dis_to_d += p_ptr->lev/3;
					info_ptr->to_d += p_ptr->lev/3;
				}
				break;
			}
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Daggers") == 0)
	{
	int spec1 = _check_speciality1_aux(o_ptr);
		if (spec1 && p_ptr->speciality1_equip && p_ptr->lev >= 45)
			info_ptr->num_blow++;

		switch (_get_toggle())
		{
		case TOGGLE_SHADOW_STANCE:
			info_ptr->to_d -= 10;
			info_ptr->dis_to_d -= 10;
			break;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Clubs") == 0)
	{
		if (p_ptr->speciality2_equip)
		{
			if (p_ptr->lev >= 25)
			{
				info_ptr->to_h += 20;
				info_ptr->dis_to_h += 20;
			}
		}

		switch (_get_toggle())
		{
		case TOGGLE_COMBAT_EXPERTISE:
			info_ptr->to_h -= p_ptr->lev;
			info_ptr->dis_to_h -= p_ptr->lev;
			break;
		case TOGGLE_STONE_BONES:
			info_ptr->to_d -= p_ptr->lev * p_ptr->lev / 50;
			info_ptr->dis_to_d -= p_ptr->lev * p_ptr->lev / 50;
			break;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Shields") == 0)
	{
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Swords") == 0)
	{
	int spec1 = _check_speciality1_aux(o_ptr);
	int spec2 = _check_speciality2_aux(o_ptr);
		if (spec1 && p_ptr->speciality1_equip)
		{
			info_ptr->to_h += 10;
			info_ptr->dis_to_h += 10;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Polearms") == 0)
	{
		if (p_ptr->entrenched)
		{
			info_ptr->to_h += 20;
			info_ptr->dis_to_h += 20;
		}

		switch (_get_toggle())
		{
		case TOGGLE_MANY_STRIKE:
			info_ptr->to_h -= 5;
			info_ptr->dis_to_h -= 5;
			break;
		case TOGGLE_PIERCING_STRIKE:
			info_ptr->to_h -= 20;
			info_ptr->dis_to_h -= 20;
			break;
		case TOGGLE_TRIP:
			info_ptr->to_h -= 30;
			info_ptr->dis_to_h -= 30;
			break;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Staves") == 0)
	{
	int spec1 = _check_speciality1_aux(o_ptr);
	int spec2 = _check_speciality2_aux(o_ptr);
		if (spec1 && p_ptr->speciality1_equip && p_ptr->lev >= 45 && p_ptr->chp == p_ptr->mhp)
			info_ptr->num_blow++;

		if (spec2 && p_ptr->speciality2_equip)
		{
			switch (_get_toggle())
			{
			case TOGGLE_FLURRY_OF_BLOWS:
				info_ptr->num_blow++;
				info_ptr->to_h -= 15;
				info_ptr->dis_to_h -= 15;
				break;

			case TOGGLE_GREATER_FLURRY:
				info_ptr->num_blow += 2;
				info_ptr->to_h -= 30;
				info_ptr->dis_to_h -= 30;
				break;
			}
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Diggers") == 0)
	{
	int spec2 = _check_speciality2_aux(o_ptr);
		if (spec2 && p_ptr->speciality2_equip)
		{
			switch (_get_toggle())
			{
			case TOGGLE_INDUSTRIOUS_MORTICIAN:
				info_ptr->num_blow += o_ptr->pval/2;
				break;
			}
		}
	}
}

static void _move_monster(int m_idx)
{
	if (strcmp(_specialities[p_ptr->speciality1].name, "Polearms") == 0)
	{
		if (p_ptr->lev >= 20 && p_ptr->speciality1_equip)
		{
			monster_type *m_ptr = &m_list[m_idx];
			if (m_ptr->cdis == 1)
			{
				char m_name[80];
				monster_desc(m_name, m_ptr, 0);
				msg_format("%^s gets too close!", m_name);
				py_attack(m_ptr->fy, m_ptr->fx, WEAPONMASTER_PROXIMITY_ALERT);
			}
		}
	}
}

static void _process_player(void)
{
	if (strcmp(_specialities[p_ptr->speciality1].name, "Polearms") == 0)
	{
		/* process_player() fires before move_player() */
		if (px == p_ptr->entrench_x && py == p_ptr->entrench_y)
		{
			if (p_ptr->entrench_ct < 1000) /* I suppose we could overflow after a while ... */
				p_ptr->entrench_ct++;
			p_ptr->update |= PU_BONUS;
			p_ptr->redraw |= PR_STATUS;
		}
		else
		{
			p_ptr->entrench_x = px;
			p_ptr->entrench_y = py;
			p_ptr->entrench_ct = 0;
			p_ptr->update |= PU_BONUS;
			p_ptr->redraw |= PR_STATUS;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Staves") == 0)
	{
		if (p_ptr->elaborate_defense)
		{
			p_ptr->elaborate_defense--;
			if (p_ptr->elaborate_defense <= 0)
				p_ptr->update |= PU_BONUS;
		}
		if (p_ptr->cloak_of_shadows)
		{
			p_ptr->cloak_of_shadows--;
			if (p_ptr->cloak_of_shadows <= 0)
				p_ptr->update |= PU_BONUS;
		}
	}
}

static void _move_player(void)
{
	if (_get_toggle() == TOGGLE_SHOT_ON_THE_RUN)
	{
		int idx = -1;
		int num_shots = 1 + p_ptr->num_fire / 400;
		int i;

		/* Paranoia:  Did the player remove their sling? */
		if (!_check_speciality2_equip())
		{
			_set_toggle(TOGGLE_NONE);
			return;
		}

		for (i = 0; i < num_shots; i++)
		{
			/* End the technique when the ammo runs out.   Note that "return ammo"
			   might not consume the current shot.  Note that we will intentionally spill
			   over into the next stack of shots, provided they are legal for this shooter.  */
			if (shoot_item != INVEN_UNLIMITED_QUIVER)
			{
				if (inventory[shoot_item].tval != p_ptr->tval_ammo)
				{
					/* Ugh, with this technique, the ammo slot is constantly moving thanks
					   to pseudo-id/autodestroyer or unstacking and stacking of staves.
					   Just try to find usable ammo should this occur */
					shoot_item = _find_ammo_slot();
					if (shoot_item < 0)
					{
						msg_print("Your ammo has run out.  Time to reload!");
						_set_toggle(TOGGLE_NONE);
						return;
					}
				}
			}

			/* Pick a target to blast */
			idx = _get_nearest_target_los();
			if (idx > 0)
			{
				int tx, ty;

				tx = m_list[idx].fx;
				ty = m_list[idx].fy;
				shoot_hack = SHOOT_RUN;
				do_cmd_fire_aux2(shoot_item, &inventory[INVEN_BOW], px, py, tx, ty);
				shoot_hack = SHOOT_NONE;
			}
		}
	}
	if (strcmp(_specialities[p_ptr->speciality1].name, "Polearms") == 0)
	{
		int y, x;
		if (one_in_(5) && random_opponent(&y, &x))
		{
			py_attack(y, x, WEAPONMASTER_AUTO_BLOW);
			energy_use = 0;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Staves") == 0)
	{
		if (p_ptr->speciality2_equip && p_ptr->lev >= 5)
		{
			p_ptr->cloak_of_shadows = 1;
			p_ptr->update |= PU_BONUS;
		}
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Diggers") == 0)
	{
	/*	handled in move_player_effect() since things like Phase Door don't trigger
	    this fn.
		if (p_ptr->speciality2_equip && p_ptr->lev >= 15)
			p_ptr->update |= PU_BONUS;
	*/
	}
}

static void _character_dump(FILE* file)
{
	cptr o_name = weaponmaster_speciality2_name();
	fprintf(file, "\n\n  [Weaponmaster Abilities]\n\n");
	
	if (strcmp(_specialities[p_ptr->speciality1].name, "Axes") == 0)
	{
		if (p_ptr->lev >= 45)
			fprintf(file, "You gain +30 damage when wielding an axe.\n");	
		else if (p_ptr->lev >= 20)
			fprintf(file, "You gain +15 damage when wielding an axe.\n");	
		else
			fprintf(file, "You gain +5 damage when wielding an axe.\n");	
		
		fprintf(file, "You gain +1 attack when wielding an axe with two hands.\n");

		if (p_ptr->lev >= 5)
			fprintf(file, "You gain +5 tunneling when wielding %s.\n", o_name);

		if (p_ptr->lev >= 30)
			fprintf(file, "You occasionally attack an adjacent opponent after killing a foe when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Clubs") == 0)
	{
		fprintf(file, "Your attacks have a chance to confuse when wielding a club.\n");	
		
		if (p_ptr->lev >= 20)
			fprintf(file, "Your attacks have a chance to knock out when wielding a club.\n");	
		
		if (p_ptr->lev >= 45)
			fprintf(file, "Your attacks have a chance to stun when wielding a club.\n");	

		if (p_ptr->lev >= 25)
			fprintf(file, "You gain +20 to hit when wielding %s.\n", o_name);

		if (p_ptr->lev >= 40)
			fprintf(file, "You gain crushing blows when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Daggers") == 0)
	{
		fprintf(file, "You pay no energy costs when equipping a dagger.\n");	
		fprintf(file, "You dual wield very effectively with daggers.\n");	
		if (p_ptr->lev >= 20)
			fprintf(file, "You gain +%d to AC when wielding a dagger.\n", 10 + p_ptr->lev/2);
		if (p_ptr->lev >= 45)
			fprintf(file, "You gain +1 attack when wielding a dagger.\n");	


		if (p_ptr->lev >= 10)
			fprintf(file, "You gain +2 stealth when wielding %s.\n", o_name);
		if (p_ptr->lev >= 30)
			fprintf(file, "You gain sneak attack and backstab when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Diggers") == 0)
	{
		fprintf(file, "You gain +%d tunneling when wielding a digger.\n", 5 + p_ptr->lev/5);
		if (p_ptr->lev >= 45)
			fprintf(file, "Your steps break walls when wielding a digger.\n");

		if (p_ptr->lev >= 15)
			fprintf(file, "You gain an AC bonus depending on the number of adjacent walls when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Polearms") == 0)
	{
		fprintf(file, "You occasionally get a free round of attacks after moving when wielding a polearm.\n");
		if (p_ptr->lev >= 20)
			fprintf(file, "You automatically attack any enemy that steps next to you when wielding a polearm.\n");
		if (p_ptr->lev >= 45)
			fprintf(file, "You occasionally strike all adjacent foes when wielding a polearm.\n");

		if (p_ptr->lev >= 30)
			fprintf(file, "You gain +20 to hit and +20 to AC when you don't move for 3 rounds when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Slings") == 0)
	{
		fprintf(file, "Your ammo often returns to you when wielding a sling.\n");
		if (p_ptr->lev >= 20)
			fprintf(file, "Your ammo gains extra damage dice when wielding a sling.\n");
		if (p_ptr->lev >= 45)
			fprintf(file, "You have access to an unlimited quiver when wielding a sling.\n");

		if (p_ptr->lev >= 10)
			fprintf(file, "Your shots never miss your target once you score 3 consecutive hits when wielding %s.\n", o_name);
		if (p_ptr->lev >= 40)
			fprintf(file, "You gain extra shots when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Shields") == 0)
	{
		fprintf(file, "You gain two handed wielding bonuses even when wielding a shield.\n");
		if (p_ptr->lev >= 20)
			fprintf(file, "Your inventory items are somewhat protected from destruction when wielding a shield.\n");
		if (p_ptr->lev >= 45)
			fprintf(file, "You gain basic resistance and reflection when wielding a shield.\n");

		if (p_ptr->lev >= 5)
			fprintf(file, "You gain double the AC benefit when wielding %s.\n", o_name);

		if (p_ptr->lev >= 25)
			fprintf(file, "You gain a bonus to saving throws when wielding %s.\n", o_name);
	
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Staves") == 0)
	{
		fprintf(file, "You gain +%d AC until your next turn after any successful hit when wielding a staff.\n", 10 + 2*p_ptr->lev/3);
		fprintf(file, "You gain -5 speed when wielding a shield.\n");
		if (p_ptr->lev >= 20)
			fprintf(file, "You gain +2 speed when wielding a staff.\n");
		if (p_ptr->lev >= 45)
			fprintf(file, "You gain +1 attack when you are at full health and wielding a staff.\n");

		if (p_ptr->lev >= 5)
			fprintf(file, "You gain +%d AC after moving until your next turn when wielding %s.\n", 10 + 2*p_ptr->lev/3, o_name);

		if (p_ptr->lev >= 35)
			fprintf(file, "You are unaffected by elemental auras when wielding %s.\n", o_name);
	}
	else if (strcmp(_specialities[p_ptr->speciality1].name, "Swords") == 0)
	{
		fprintf(file, "You gain +10 to hit when wielding a sword.\n");
		if (p_ptr->lev >= 20)
			fprintf(file, "You gain constant heroism when wielding a sword.\n");
		if (p_ptr->lev >= 45)
			fprintf(file, "You gain vorpal attacks when wielding a sword.\n");
	}
}

class_t *weaponmaster_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 30,  28,  28,   1,  20,  10,  60,  45};
	skills_t xs = { 10,  10,  10,   0,   0,   0,  21,  15};

		me.name = "Weaponmaster";
		me.desc = "The weaponmaster is great with a class of weapons, but truly "
		          "outstanding with the one weapon he specializes in. "
				  "At character creation, the player chooses a sub-specialization "
				  "(e.g. Polearms) and a specialization (e.g. Scythe). "
				  "At level 50, the character gets to super-specialization in "
				  "one particular weapon (e.g. Avavir). The character gets "
				  "bonuses from Sub-specialization if he uses weapon from "
				  "that group, and abilities from Specialization if he uses "
				  "weapon of that type. If dual-wielding, both weapons must "
				  "be of the appropriate type.";

		me.stats[A_STR] =  3;
		me.stats[A_INT] = -1;
		me.stats[A_WIS] = -1;
		me.stats[A_DEX] =  1;
		me.stats[A_CON] =  1;
		me.stats[A_CHR] =  0;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		me.birth = _on_birth;
		me.calc_bonuses = _calc_bonuses;
		me.calc_weapon_bonuses = _calc_weapon_bonuses;
		me.move_player = _move_player;
		me.move_monster = _move_monster;
		me.process_player = _process_player;
		me.character_dump = _character_dump;
		init = TRUE;
	}

	return &me;
}

int weaponmaster_wield_hack(object_type *o_ptr)
{
	int result = 100;

	if (p_ptr->pclass == CLASS_WEAPONMASTER)
	{
		if (strcmp(_specialities[p_ptr->speciality1].name, "Daggers") == 0)
		{
			if (_check_speciality1_aux(o_ptr)) result = 0;
		}
	}
	return result;
}

void weaponmaster_get_frenzy_items(void)
{
	int i;
	int ct = 0;
	for (i = 0; i < MAX_FRENZY_ITEMS; i++)
		frenzy_items[i] = -1;

	for (i = 0; i < INVEN_PACK; i++)
	{
		if (_check_speciality2_aux(&inventory[i]))
		{
			frenzy_items[ct] = i;
			ct++;
		}
	}
}
