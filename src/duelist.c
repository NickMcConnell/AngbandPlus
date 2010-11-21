#include "angband.h"

/* Check for valid equipment.  Note, the pointer we return
   will point to a descriptive error message indefinitely.
   We don't use format(...).  Also, different messages will
   have different addresses, so that _calc_bonuses() can
   keep the user up to date as to why their powers don't work. */
cptr _equip_error(void)
{
	int wgt = 0;

	/* weapons are not weighed ...*/
	if(inventory[INVEN_RARM].tval > TV_SWORD) wgt += inventory[INVEN_RARM].weight;
	if(inventory[INVEN_LARM].tval > TV_SWORD) wgt += inventory[INVEN_LARM].weight;

	wgt += inventory[INVEN_BODY].weight;
	wgt += inventory[INVEN_HEAD].weight;
	wgt += inventory[INVEN_OUTER].weight;
	wgt += inventory[INVEN_HANDS].weight;
	wgt += inventory[INVEN_FEET].weight;

	if (wgt > (100 + (p_ptr->lev * 4)))
		return "The weight of your equipment is disrupting your talents.";

	if ( inventory[INVEN_RARM].tval == TV_SHIELD
	  || inventory[INVEN_RARM].tval == TV_CAPTURE
	  || inventory[INVEN_LARM].tval == TV_SHIELD
	  || inventory[INVEN_LARM].tval == TV_CAPTURE )
	{
		return "Your shield is disrupting your talents.";
	}

	if (inventory[INVEN_RARM].k_idx && inventory[INVEN_LARM].k_idx)
		return "Dual wielding is disrupting your talents.";

	if ( (inventory[INVEN_RARM].tval == TV_SWORD) && (inventory[INVEN_RARM].sval == SV_DOKUBARI)
	  || (inventory[INVEN_LARM].tval == TV_SWORD) && (inventory[INVEN_LARM].sval == SV_DOKUBARI))
	{
		return "The Poison Needle is not an honorable dueling weapon.";
	}

	if (p_ptr->anti_magic)
		return "An anti-magic barrier disrupts your talents.";

	return NULL;
}

cptr duelist_current_challenge(void)
{
	static char current_challenge[200];

	/* paranoia ... this only seems to happen with wizard mode summoned monsters
	   after a save and restore, so probably the wizard 'n' command is broken */
	if (p_ptr->duelist_target_idx && !m_list[p_ptr->duelist_target_idx].r_idx)
		p_ptr->duelist_target_idx = 0;

	if (p_ptr->duelist_target_idx)
	{
		monster_desc(current_challenge, &m_list[p_ptr->duelist_target_idx], MD_ASSUME_VISIBLE);
		return current_challenge;
	}
	if (_equip_error())
		return "Talents Disrupted";

	return "No Current Challenge";
}

int duelist_skill_sav(int m_idx)
{
	int result = p_ptr->skill_sav;
	if ( p_ptr->pclass == CLASS_DUELIST
	  && p_ptr->duelist_target_idx == m_idx )
	{
		/*result += 30;*/
		/* Same effect as "Anti-Magic */
		result = MAX(result, 90 + p_ptr->lev);
	}
	return result;
}

bool duelist_issue_challenge(void)
{
	bool result = FALSE;
	int dir = 5;
	int m_idx = 0;

	if (target_set(TARGET_MARK))
	{
		msg_flag = FALSE; /* Bug ... we get an extra -more- prompt after target_set() ... */
		if (target_who > 0)
			m_idx = target_who;
		else
			m_idx = cave[target_row][target_col].m_idx;
	}

	if (m_idx)
	{
		if (m_idx == p_ptr->duelist_target_idx)
			msg_format("%^s has already been challenged.", duelist_current_challenge());
		else
		{
			/* of course, we must first set the target index before duelist_current_challenge()
			   will return the correct text */
			p_ptr->duelist_target_idx = m_idx;
			msg_format("You challenge %s to a duel!", duelist_current_challenge());
			set_monster_csleep(m_idx, 0);
			result = TRUE;
		}
	}
	else if (p_ptr->duelist_target_idx)
	{
		p_ptr->duelist_target_idx = 0;
		msg_print("You cancel your current challenge!");
	}

	p_ptr->redraw |= PR_STATUS;
	return result;
}


/*
 * I spiked the Ninja/Samurai rush_attack() ... it was not quite what I need.
 */

typedef enum { 
	_rush_cancelled,  /* Don't charge player energy ... they made a dumb request */
	_rush_failed,     /* Rush to foe was blocked by another monster, or foe out of range */
	_rush_succeeded   /* Got him! */
} _rush_result;

typedef enum {
	_rush_normal,     /* Attacks first monster in the way */
	_rush_acrobatic,   /* Displaces intervening monsters (waking them up) */
	_rush_phase,
} _rush_type;

_rush_result _rush_attack(int rng, _rush_type type)
{
	_rush_result result = _rush_cancelled;
	int tx, ty;
	int tm_idx = 0;
	u16b path_g[32];
	int path_n, i;
	bool moved = FALSE;
	int flg = 0;
	int dis = 0;

	if (type == _rush_normal)
		flg = PROJECT_STOP | PROJECT_KILL;
	else if (type == _rush_acrobatic)
		flg = PROJECT_THRU | PROJECT_KILL;
	else
		flg = PROJECT_DISI | PROJECT_THRU;

	if (!p_ptr->duelist_target_idx)
	{
		msg_print("You need to select a foe first (Mark Target).");
		return result;
	}

	tm_idx = p_ptr->duelist_target_idx;
	tx = m_list[tm_idx].fx;
	ty = m_list[tm_idx].fy;

	dis = distance(ty, tx, py, px);

	/* Foe must be visible.  For all charges except the phase charge, the
	   foe must also be in your line of sight */
	if (!m_list[p_ptr->duelist_target_idx].ml ||
	    (type != _rush_phase && !los(ty, tx, py, px)))
	{
		msg_format("%^s is not in your line of sight.", duelist_current_challenge());
		return result;
	}

	if (dis > rng)
	{
		msg_format("Your foe is out of range (%d vs %d).", dis, rng);
		if (!get_check("Charge anyway? ")) return result;
	}

	project_length = rng;
	path_n = project_path(path_g, project_length, py, px, ty, tx, flg);
	project_length = 0;

	if (!path_n) return result;

	result = _rush_failed;

	/* Use ty and tx as to-move point */
	ty = py;
	tx = px;

	/* Project along the path */
	for (i = 0; i < path_n; i++)
	{
		monster_type *m_ptr;
		cave_type *c_ptr;

		int ny = GRID_Y(path_g[i]);
		int nx = GRID_X(path_g[i]);

		if ( (type == _rush_phase && !cave[ny][nx].m_idx && !cave_have_flag_bold(ny, nx, FF_PERMANENT))
		  || (cave_empty_bold(ny, nx) && player_can_enter(cave[ny][nx].feat, 0)))
		{
			ty = ny;
			tx = nx;
			continue;
		}

		if (!cave[ny][nx].m_idx)
		{
			msg_print("Failed!");
			break;
		}

		c_ptr = &cave[ny][nx];

		/* Move player before updating the monster */
		if (!player_bold(ty, tx)) teleport_player_to(ty, tx, TELEPORT_NONMAGICAL);
		moved = TRUE;

		/* Update the monster */
		update_mon(c_ptr->m_idx, TRUE);

		/* Found a monster */
		m_ptr = &m_list[c_ptr->m_idx];

		/* But it is not the monster we seek! */
		if (tm_idx != c_ptr->m_idx)
		{
			/* Acrobatic Charge attempts to displace monsters on route */
			if (type == _rush_acrobatic)
			{
				/* Swap position of player and monster */
				set_monster_csleep(c_ptr->m_idx, 0);
				move_player_effect(ny, nx, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
				ty = ny;
				tx = nx;
				continue;
			}
			/* Normal Charge just attacks first monster on route */
			else
				msg_format("There is %s in the way!", m_ptr->ml ? (tm_idx ? "another monster" : "a monster") : "someone");
		}

		/* Attack the monster */
		if (tm_idx == p_ptr->duelist_target_idx) result = _rush_succeeded;
		py_attack(ny, nx, 0);
		break;
	}

	if (!moved && !player_bold(ty, tx)) teleport_player_to(ty, tx, TELEPORT_NONMAGICAL);
	return result;
}

/****************************************************************
 * Private Spells
 ****************************************************************/
static void _acrobatic_charge_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Acrobatic Charge");
		break;
	case SPELL_DESC:
		var_set_string(res, "Move up to 7 squares and attack your marked foe, displacing any monsters in your way.");
		break;
	case SPELL_CAST:
		var_set_bool(res, _rush_attack(7, _rush_acrobatic) != _rush_cancelled);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _charge_target_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Charge");
		break;
	case SPELL_DESC:
		var_set_string(res, "Move up to 5 squares and attack your marked foe.");
		break;
	case SPELL_CAST:
		var_set_bool(res, _rush_attack(5, _rush_normal) != _rush_cancelled);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _darting_duel_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Darting Duel");
		break;
	case SPELL_DESC:
		var_set_string(res, "Move up to 5 squares and attack your marked foe.  Strafe if you attack your foe.");
		break;
	case SPELL_CAST:
		{
			int tmp = p_ptr->duelist_target_idx;
			_rush_result r = _rush_attack(5, _rush_normal);
			if (r == _rush_cancelled)
				var_set_bool(res, FALSE);
			else 
			{
				var_set_bool(res, TRUE);
				if (r == _rush_succeeded && tmp == p_ptr->duelist_target_idx)
				{
					monster_type *m_ptr = &m_list[p_ptr->duelist_target_idx];
					if (!(m_ptr->smart & SM_TICKED_OFF))
					{
						msg_format("%^s is ticked off!", duelist_current_challenge());
						m_ptr->smart |= SM_TICKED_OFF;
					}
					teleport_player(10, TELEPORT_LINE_OF_SIGHT);
				}
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _disengage_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Disengage");
		break;
	case SPELL_DESC:
		var_set_string(res, "You teleport (range 100), and prevent your marked foe from following, even if it's a monster that can normally follow teleportation.  After the teleport, your foe is no longer marked.");
		break;
	case SPELL_CAST:
		if (!p_ptr->duelist_target_idx)
		{
			msg_print("You need to mark your target first.");
			var_set_bool(res, FALSE);
		}
		else if (!m_list[p_ptr->duelist_target_idx].ml)
		{
			msg_print("You may not disengage unless your foe is visible.");
			var_set_bool(res, FALSE);
		}
		else
		{
			teleport_player(100, TELEPORT_DISENGAGE);
			p_ptr->duelist_target_idx = 0;
			msg_print("You disengage from your current challenge.");
			p_ptr->redraw |= PR_STATUS;
		
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _isolation_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Isolation");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to teleport away all monsters in Line of Sight other than your marked foe.");
		break;
	case SPELL_CAST:
		if (!p_ptr->duelist_target_idx)
		{
			msg_print("You need to mark your target first.");
			var_set_bool(res, FALSE);
		}
		else
		{
			project_hack(GF_ISOLATION, p_ptr->lev * 4);
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _mark_target_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Mark Target");
		break;
	case SPELL_DESC:
		var_set_string(res, "Mark selected monster as designated foe.  You may only mark a single target at a time, and receive great benefits when fighting this target.");
		break;

	case SPELL_INFO:
		var_set_string(res, format("%^s", duelist_current_challenge()));
		break;

	case SPELL_CAST:
		var_set_bool(res, duelist_issue_challenge());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _phase_charge_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Phase Charge");
		break;
	case SPELL_DESC:
		var_set_string(res, "Move up to 10 squares and attack your marked foe.  Functions even if there are walls or closed doors between you and your target.");
		break;
	case SPELL_CAST:
		var_set_bool(res, _rush_attack(10, _rush_phase) != _rush_cancelled);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _strafing_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Strafing");
		break;
	case SPELL_DESC:
		var_set_string(res, "Blink to a new location in the line of sight of your current location.");
		break;
	case SPELL_CAST:
		teleport_player(10, TELEPORT_LINE_OF_SIGHT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

/****************************************************************
 * Spell Table
 ****************************************************************/

#define MAX_DUELIST_SPELLS	8

static spell_info _spells[MAX_DUELIST_SPELLS] = 
{
    /*lvl cst fail spell */
	{  1,   0,  0, _mark_target_spell },
	{  8,  10,  0, _charge_target_spell },
	{ 16,   8,  0, _strafing_spell },
	{ 24,  25,  0, _disengage_spell },
	{ 32,  30,  0, _acrobatic_charge_spell },
	{ 40,  60,  0, _isolation_spell },
	{ 45,  60,  0, _darting_duel_spell },
	{ 48,  60,  0, _phase_charge_spell },
}; 

static int _get_spells(spell_info* spells, int max)
{
	int i;
	int ct = 0;
	int stat_idx = p_ptr->stat_ind[A_DEX];
	cptr msg = _equip_error();

	if (msg)
	{
		msg_print(msg);
		return 0;
	}
	
	/* Initialize a (copied) spell list with current casting costs and fail rates */
	for (i = 0; i < MAX_DUELIST_SPELLS; i++)
	{
		spell_info *base = &_spells[i];
		if (ct >= max) break;
		if (base->level <= p_ptr->lev)
		{
			spell_info* current = &spells[ct];
			current->fn = base->fn;
			current->level = base->level;
			current->cost = base->cost;

			current->fail = calculate_fail_rate(base, stat_idx);			
			ct++;
		}
	}
	return ct;
}

static void _calc_bonuses(void)
{
	static cptr last_msg = NULL;
	cptr msg = _equip_error();

	p_ptr->to_a -= 50;
	p_ptr->dis_to_a -= 50;

	if (!msg)
	{
		int x = (p_ptr->stat_ind[A_INT] + 3);
		int l = p_ptr->lev;
		int to_a = x/2 + x*l/50;
		p_ptr->to_a += to_a;
		p_ptr->dis_to_a += to_a;
	}

	if (msg != last_msg)
	{
		last_msg = msg;
		if (msg)
		{
			msg_print(msg);
			if (p_ptr->duelist_target_idx)
			{
				msg_format("%^s is no longer your target.", duelist_current_challenge());
				p_ptr->duelist_target_idx = 0;
				p_ptr->redraw |= PR_STATUS;
			}
		}
		else
			msg_print("You regain your talents.");
	}

	p_ptr->redraw |= PR_STATUS;
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
	int to_d = (p_ptr->stat_ind[A_DEX] + 3 - 10) + p_ptr->lev/2 - o_ptr->weight/10;

	if (!_equip_error())
	{
		info_ptr->to_d += to_d;
		info_ptr->dis_to_d += to_d;

		/* Blows should always be 1 ... even with Quickthorn and Shiva's Jacket! */
		info_ptr->num_blow = 1;
	}
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "challenge";
		me.use_hp = TRUE;
		init = TRUE;
	}
	return &me;
}

class_t *duelist_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{
		me.calc_bonuses = _calc_bonuses;
		me.calc_weapon_bonuses = _calc_weapon_bonuses;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		init = TRUE;
	}

	/* dynamic info */

	return &me;
}

