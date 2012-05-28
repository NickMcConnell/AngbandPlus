#include "angband.h"

/****************************************************************
 * Helpers
 ****************************************************************/

static int _get_toggle(void)
{
	return p_ptr->magic_num1[0];
}

static int _set_toggle(s32b toggle)
{
	int result = p_ptr->magic_num1[0];

	if (toggle == result) return result;

	p_ptr->magic_num1[0] = toggle;

	p_ptr->redraw |= PR_STATUS;
	p_ptr->update |= PU_BONUS;
	handle_stuff();

	return result;
}

int mauler_get_toggle(void)
{
	/* exposed for prtstatus() in xtra1.c 
	   this is easier than rewriting the status code so that classes can maintain it!
	*/
	int result = TOGGLE_NONE;
	if (p_ptr->pclass == CLASS_MAULER)
		result = _get_toggle();
	return result;
}

void process_maul_of_vice(void)
{
	if (!p_ptr->maul_of_vice) return;
	
	p_ptr->au -= randint1(p_ptr->lev);
	p_ptr->redraw |= PR_GOLD;

	if (p_ptr->au < 0)
	{
		int i;

		p_ptr->au = 0;
		p_ptr->update |= PU_BONUS;

		for (i = 0; i < INVEN_TOTAL; i++)
		{
			if (inventory[i].name1 == ART_MAUL_OF_VICE)
			{
				char o_name[MAX_NLEN];
				object_desc(o_name, &inventory[i], OD_OMIT_PREFIX);
				msg_format("A terrible black aura blasts your %s!", o_name);
				blast_object(&inventory[i]);
				disturb(1, 0);
				break;
			}
		}
	}
	else if (p_ptr->au < 1000)
	{
		msg_print("***LOW GOLD WARNING!!!!***");
		disturb(1, 0);
	}
	else if (one_in_(111))
	{
		msg_print("You feel your wealth draining away!");
	}
}

/****************************************************************
 * Spells
 ****************************************************************/
static void _cursed_wounds_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Cursed Wounds");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, any wounds that you inflict on your enemies will never fully heal.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (_get_toggle() == TOGGLE_CURSED_WOUNDS)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_CURSED_WOUNDS);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_CURSED_WOUNDS)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _death_force_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Death Force");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique your weapon gains a force brand. However, since you lack mana, each force strike will decrease the duration of your hasted status. If you are not hasted, the force brand will cease to function.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (_get_toggle() == TOGGLE_DEATH_FORCE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_DEATH_FORCE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_DEATH_FORCE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _killing_spree_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Killing Spree");
		break;
	case SPELL_DESC:
		var_set_string(res, "Engage in wanton destruction! During this time, any foe you slay will haste you, so seek to kill as many enemies as possible!");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->tim_killing_spree)
		{
			msg_print("You are already on a Killing Spree. Show some mercy!");
			return;
		}
		set_tim_killing_spree(44, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _no_earthquake_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Prevent Earthquake");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique your weapon will not produce earthquakes.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (_get_toggle() == TOGGLE_NO_EARTHQUAKE)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_NO_EARTHQUAKE);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_NO_EARTHQUAKE)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _slay_sentient_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Slay Sentient");
		break;
	case SPELL_DESC:
		var_set_string(res, "For a short time, your weapon will slay all monsters except those than cannot be stunned.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (p_ptr->tim_slay_sentient)
		{
			msg_print("Your weapon is already far too powerful!");
			return;
		}
		set_tim_slay_sentient(6 + randint1(6), FALSE);
		var_set_bool(res, TRUE);
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
		var_set_string(res, "Create an earthquake by smashing your weapon powerfully on the ground.");
		break;
	case SPELL_CAST:
	{
		int w = inventory[INVEN_RARM].weight;
		earthquake(py, px, w/40);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _smash_wall_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Smash Wall");
		break;
	case SPELL_DESC:
		var_set_string(res, "Destroys adjacent targeted wall, door, tree, or trap.");
		break;
	case SPELL_CAST:
	{
		int y, x, dir;
		
		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) return;
		if (dir == 5) return;

		y = py + ddy[dir];
		x = px + ddx[dir];
		
		if (!in_bounds(y, x)) return;

		if (cave_have_flag_bold(y, x, FF_HURT_ROCK))
		{
			cave_alter_feat(y, x, FF_HURT_ROCK);
			p_ptr->update |= PU_FLOW;
		}
		else if (cave_have_flag_bold(y, x, FF_TREE))
		{
			cave_set_feat(y, x, one_in_(3) ? feat_brake : feat_grass);
		}
		else
		{
			int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
			project(0, 0, y, x, 0, GF_KILL_DOOR, flg, -1);
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _weapon_as_shield_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Weapon as Shield");
		break;
	case SPELL_DESC:
		var_set_string(res, "When using this technique, you will gain an AC bonus based on the weight of your current weapon.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (_get_toggle() == TOGGLE_WEAPON_AS_SHIELD)
			_set_toggle(TOGGLE_NONE);
		else
			_set_toggle(TOGGLE_WEAPON_AS_SHIELD);
		var_set_bool(res, TRUE);
		break;
	case SPELL_ENERGY:
		if (_get_toggle() != TOGGLE_WEAPON_AS_SHIELD)
			var_set_int(res, 0);	/* no charge for dismissing a technique */
		else
			var_set_int(res, 100);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    {  5,  5, 40, _smash_wall_spell},
	{ 10, 30, 70, _smash_ground_spell},
	{ 15,  0,  0, _weapon_as_shield_spell},
	{ 20,  0, 60, awesome_blow_spell},
	{ 26,  0,  0, _cursed_wounds_spell},
	{ 32, 44, 50, _killing_spree_spell},
	{ 38,  0,  0, _no_earthquake_spell},
	{ 44,100, 90, _slay_sentient_spell},
	{ 50,  0,  0, _death_force_spell},
	{ -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{
	int ct;

	if (!p_ptr->ryoute)
	{
		msg_print("Rargh! You need to wield a single weapon with both hands to properly maul stuff!");
		return 0;
	}

	ct = get_spells_aux(spells, max, _spells, p_ptr->stat_ind[A_STR]);
	
	if (ct == 0)
		msg_print("Rargh! Go maul something for more experience!");

	return ct;
}

static void _calc_bonuses(void)
{
	if (p_ptr->ryoute)
	{
		if (_get_toggle() == TOGGLE_WEAPON_AS_SHIELD)
		{
			int w = inventory[INVEN_RARM].weight;
			int a = w/20 + (w/100)*(w/100);
			p_ptr->to_a += a;
			p_ptr->dis_to_a += a;
		}
	}
}

static void _calc_weapon_bonuses(object_type *o_ptr, weapon_info_t *info_ptr)
{
	if (p_ptr->ryoute)
	{
		/* CL1: Mighty */
		if (p_ptr->lev >= 1)
		{
			int w = o_ptr->weight;
			int d = w/50 + (w/100)*(w/100);

			if (w >= 200)
			{
				int i = p_ptr->stat_ind[A_STR];
				int d2 = (int)(adj_str_td[i]) - 128;
				d += d2;
			}

			info_ptr->to_d += d;
			info_ptr->dis_to_d += d;
		}

		/* CL15: Splattering is handled as a hack in cmd1.c py_attack_aux() */

		/* CL30: Impact */
		if (p_ptr->lev >= 30)
		{
			int w = o_ptr->weight;
			int x = w/250;
			
			if (x > 0)
			{
				info_ptr->to_dd += x;
				info_ptr->to_ds += x;
			}
		}

		/* CL45: Destroyer is handled as a hack in cmd1.c critical_norm() */

	}
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "technique";
		me.use_hp = TRUE;
		init = TRUE;
	}
	return &me;
}

static void _spoiler_dump(FILE* fff)
{
	spoil_spells_aux(fff, _spells);

	fprintf(fff, "\n== Abilities ==\n");
	fprintf(fff, "|| *Lvl* || *Ability* || *Description* ||\n");
	fprintf(fff, "|| 1 || Mighty || +(W/50)+(W/100)^2 todam. Str bonus to damage is doubled for weapons >= 20 pounds. || \n");
	fprintf(fff, "|| 15 || Splattering || Whenever you kill a monster, your last strike damage is applied to all foes within radius 2 ball of recently deceased enemy. ||\n");
	fprintf(fff, "|| 30 || Impact || +(W/250) to dice and sides of wielded weapon. ||\n");
	fprintf(fff, "|| 45 || Destroyer || +(W/20)%% chance of crits (e.g. a 40 lb heavy lance gets +20%% chance to crit). ||\n");
	fprintf(fff, "\n_Where W is your weapon's weight in tenths of a pound._\n");
}

class_t *mauler_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 25,  25,  35,   0,  14,   2,  70,  40 };
	skills_t xs = { 12,  11,  12,   0,   0,   0,  30,  18 };

		me.name = "Mauler";
		me.desc = 
		"The Mauler favors extremely heavy weapons, and possesses powerful abilities whose "
			"effectiveness depends on the weight of the weapon. While they only gain a limited "
			"number of blows which can never be magically increased, they are capable of "
			"hitting opponents very hard to make the most of each strike. The Mauler is "
			"required to use both hands on a single weapon when wielding if they wish their "
			"talents to function properly.",

		me.stats[A_STR] =  6;
		me.stats[A_INT] =  1;
		me.stats[A_WIS] =  0;
		me.stats[A_DEX] = -4;
		me.stats[A_CON] =  3;
		me.stats[A_CHR] =  0;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.hd = 9;
		me.exp = 120;
		me.pets = 40;

		me.calc_bonuses = _calc_bonuses;
		me.calc_weapon_bonuses = _calc_weapon_bonuses;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		me.spoiler_dump = _spoiler_dump;
		init = TRUE;
	}

	return &me;
}
