#include "angband.h"


void _blood_flow_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Flow", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Cuts yourself.", ""));
		break;
	case SPELL_CAST:
	{
		int cut = p_ptr->cut;
		cut += cut/5;
		if (cut < CUT_LIGHT)
			cut = CUT_LIGHT;

		set_cut(cut, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_sight_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Sight", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Detects living creatures in the vicinity.", ""));
		break;
	case SPELL_CAST:
	{
		if (p_ptr->lev < 30)
			detect_monsters_living(DETECT_RAD_DEFAULT);
		else
			set_tim_blood_sight(randint1(30) + 30, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_spray_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Spray", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Cuts yourself, splattering nearby enemies.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(3, 5, p_ptr->lev + p_ptr->lev/4));
		break;
	case SPELL_CAST:
	{
		int dice = 3;
		int sides = 5;
		int rad = (p_ptr->lev < 30) ? 3 : 4;
		int base = p_ptr->lev + p_ptr->lev/4;

		fire_ball(GF_BLOOD, 5, 2*(damroll(dice, sides) + base), rad);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_bath_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Bath", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Restores constitution and cures poison.", ""));
		break;
	case SPELL_CAST:
	{
		bool chg = FALSE;
		if (do_res_stat(A_CON)) chg = TRUE;
		if (set_poisoned(0, TRUE)) chg = TRUE;
		if (!chg) msg_print("You don't need a bath just yet.");
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_shield_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Shield", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives bonus to AC depending on how wounded you are.  Grants reflection if you are really hurting.", ""));
		break;
	case SPELL_CAST:
	{
		set_tim_blood_shield(randint1(20) + 30, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_seeking_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Seeking", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives slay living to your weapon.", ""));
		break;
	case SPELL_CAST:
	{
		set_tim_blood_seek(randint1(30) + 30, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_rage_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Rage", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Enter a blood frenzy.  Gives speed and big bonuses to hit and damage.", ""));
		break;
	case SPELL_CAST:
	{
		int dur = randint1(p_ptr->lev/2) + p_ptr->lev/2;
		set_fast(dur, FALSE);
		set_shero(dur, FALSE);
		set_afraid(0, TRUE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_feast_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Feast", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("You begin to feast on your opponents blood, doing extra damage but at a cost to your own health.", ""));
		break;
	case SPELL_CAST:
	{
		var_set_bool(res, FALSE);
		if (p_ptr->tim_blood_feast)
		{
			if (!get_check("Cancel the Blood Feast? ")) return;
			set_tim_blood_feast(0, TRUE);
		}
		else
		{
			set_tim_blood_feast(randint1(25) + 25, FALSE);
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_revenge_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Revenge", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives an aura of bloody revenge.  Monsters take damaged based on your cut status.", ""));
		break;
	case SPELL_CAST:
	{
		set_tim_blood_revenge(randint1(5) + 5, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_pool_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Pool", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Creates a macabre Potion of Healing made of your own blood.", ""));
		break;
	case SPELL_CAST:
	{
		object_type forge, *q_ptr = &forge;
		const int blood_cost = 100;

		var_set_bool(res, FALSE);
		if (p_ptr->blood_points < blood_cost)
		{
			msg_print("You need more blood!");
			return;
		}

		msg_print("You feel light headed.");
		object_prep(q_ptr, lookup_kind(TV_POTION, SV_POTION_BLOOD));
		drop_near(q_ptr, -1, py, px);
		p_ptr->blood_points -= blood_cost;
		p_ptr->redraw |= PR_BLOOD_POINTS;
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void _blood_explosion_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blood Explosion", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Damages all living creatures in sight at tremendous cost to your own health.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, 500));
		break;
	case SPELL_CAST:
	{
		const int blood_cost = 0;
		var_set_bool(res, FALSE);
		if (p_ptr->blood_points < blood_cost)
		{
			msg_print("You need more blood!");
			return;
		}
		msg_print("You cut too deep ... Your blood explodes!");
		dispel_living(500);
		if (blood_cost > 0)
		{
			p_ptr->blood_points -= blood_cost;
			p_ptr->redraw |= PR_BLOOD_POINTS;
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}


#define MAX_BLOOD_KNIGHT_SPELLS	10

static spell_info _spells[MAX_BLOOD_KNIGHT_SPELLS] = 
{
    /*lvl cst fail spell */
	{  1,   1, 20, _blood_flow_spell },
    {  5,  5,  30, _blood_sight_spell},
    { 10, 10,  30, _blood_spray_spell},
	{ 15, 20,  30, _blood_bath_spell},
    { 20, 30,  30, _blood_shield_spell},
    { 25, 50,  40, _blood_seeking_spell},
    { 30, 60,  40, _blood_rage_spell},
    { 40, 60,  50, _blood_feast_spell},
	{ 45, 60,   0, _blood_revenge_spell},
	{ 50,500,  60, _blood_explosion_spell},
}; 

static int _get_spells(spell_info* spells, int max)
{
	int i;
	int ct = 0;
	int stat_idx = p_ptr->stat_ind[A_CON];

	for (i = 0; i < MAX_BLOOD_KNIGHT_SPELLS; i++)
	{
		spell_info *base = &_spells[i];
		if (ct >= max) break;
		if (base->level <= p_ptr->lev)
		{
			spell_info* current = &spells[ct];
			current->fn = base->fn;
			current->level = base->level;
			current->cost = base->cost;

			current->fail = calculate_fail_rate(base->level, base->fail, stat_idx);			
			ct++;
		}
	}
	return ct;
}

static void _calc_bonuses(void)
{
	p_ptr->regenerate = TRUE;
	if (p_ptr->lev > 29) p_ptr->resist_fear = TRUE;

	if (p_ptr->cut > 0)
	{
		int to_h = 0;
		int to_d = 0;
		int to_stealth = 0;
		if (p_ptr->cut >= CUT_MORTAL_WOUND)
		{
			to_h = 25;
			to_d = 25;
			to_stealth = -10;
		}
		else if (p_ptr->cut >= CUT_DEEP_GASH)
		{
			to_h = 15;
			to_d = 15;
			to_stealth = -3;
		}
		else if (p_ptr->cut >= CUT_SEVERE)
		{
			to_h = 8;
			to_d = 8;
			to_stealth = -2;
		}
		else if (p_ptr->cut >= CUT_NASTY)
		{
			to_h = 6;
			to_d = 6;
			to_stealth = -2;
		}
		else if (p_ptr->cut >= CUT_BAD)
		{
			to_h = 4;
			to_d = 4;
			to_stealth = -1;
		}
		else if (p_ptr->cut >= CUT_LIGHT)
		{
			to_h = 2;
			to_d = 2;
			to_stealth = -1;
		}
		else
		{
			to_h = 1;
			to_d = 1;
			to_stealth = -1;
		}
		p_ptr->weapon_info[0].to_h += to_h;
		p_ptr->weapon_info[1].to_h += to_h;
		p_ptr->to_h_m  += to_h;
		p_ptr->weapon_info[0].dis_to_h += to_h;
		p_ptr->weapon_info[1].dis_to_h += to_h;

		p_ptr->weapon_info[0].to_d += to_d;
		p_ptr->weapon_info[1].to_d += to_d;
		p_ptr->to_d_m  += to_d;
		p_ptr->weapon_info[0].dis_to_d += to_d;
		p_ptr->weapon_info[1].dis_to_d += to_d;

		p_ptr->skill_stl += to_stealth;
	}
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
	int frac = p_ptr->chp * 100 / p_ptr->mhp;
	if (frac < 20 && p_ptr->lev > 48)
		info_ptr->num_blow += 9;
	else if (frac < 40 && p_ptr->lev > 36)
		info_ptr->num_blow += 6;
	else if (frac < 60 &&  p_ptr->lev > 24)
		info_ptr->num_blow += 4;
	else if (frac < 80 && p_ptr->lev > 12)
		info_ptr->num_blow += 2;
	else if (p_ptr->chp < p_ptr->mhp) /* Hack: frac might be 100 if we are just slightly wounded */
		info_ptr->num_blow += 1;
}

void _on_cast(const spell_info *spell)
{
	set_cut(p_ptr->cut + spell->level, FALSE);
	p_ptr->update |= PU_BONUS;
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "bloodcraft";
		me.use_hp = TRUE;
		me.on_cast = _on_cast;
		init = TRUE;
	}
	return &me;
}

class_t *blood_knight_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 25,  18,  32,   2,  16,   6,  70,  20};
	skills_t xs = { 12,   7,  10,   0,   0,   0,  23,  15};

		me.name = "Blood-Knight";
		me.desc = "A blood knight is a fighter who has delved into the dark arts and can perform "
		          "a limited number of offense effects using his own health.  In addition to the "
				  "HP cost, using an ability also causes bleeding/wounds, with an amount proportional "
				  "to the cost of the ability.";
		me.stats[A_STR] =  2;
		me.stats[A_INT] = -2;
		me.stats[A_WIS] = -2;
		me.stats[A_DEX] =  0;
		me.stats[A_CON] =  3;
		me.stats[A_CHR] = -3;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.calc_bonuses = _calc_bonuses;
		me.calc_weapon_bonuses = _calc_weapon_bonuses;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		init = TRUE;
	}

	return &me;
}

