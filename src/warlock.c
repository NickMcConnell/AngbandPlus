/****************************************************************
 * The Warlock
 ****************************************************************/

#include "angband.h"

bool warlock_is_pact_monster(monster_race *r_ptr)
{
	bool is_pact = FALSE;
	/* First, we list symbols for the alliance */
	char* pc = my_strchr(pact_info[p_ptr->psubclass].alliance, r_ptr->d_char);
	if (pc != NULL)
	{
		is_pact = TRUE;
	}
	else
	{
		/* If that fails, we check flags ... I'd prefer to only check flags
			but I'm not sure how accurate the beastiary is ... */
		switch (p_ptr->psubclass)
		{
		case PACT_UNDEAD:
			if (r_ptr->flags3 & RF3_UNDEAD)
				is_pact = TRUE;
			break;

		case PACT_DRAGON:
			if (r_ptr->flags3 & RF3_DRAGON)
				is_pact = TRUE;
			break;

		case PACT_ANGEL:
			/* Angel pact is now all good monsters!!! */
			if (r_ptr->flags3 & RF3_GOOD)
				is_pact = TRUE;
			break;
				
		case PACT_DEMON:
			if (r_ptr->flags3 & RF3_DEMON)
				is_pact = TRUE;
			break;
				
		case PACT_ABERRATION:
			if (r_ptr->flags2 & RF2_HUMAN)
				is_pact = TRUE;				
			break;
		}
	}

	return is_pact;
}

static int _warlock_range(void)
{
	int rng = 5;

	if (p_ptr->lev > 47)
		rng = 8;
	else if (p_ptr->lev > 31)
		rng = 7;
	else if (p_ptr->lev > 15)
		rng = 6;

	return rng; 
}

static int _warlock_dice(void)
{
	return 1 + (p_ptr->lev/5) + (p_ptr->lev * p_ptr->lev * 3/500);
}

static int _warlock_sides(void)
{
	return warlock_damage_sides[p_ptr->stat_ind[A_CHR]];
}

static void _basic_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Basic");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires your basic Eldritch Blast.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  0);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _extended_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Extended");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an Eldritch Blast with increased range.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range() + 10 * p_ptr->lev/50));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range() + 10 * p_ptr->lev/50;
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  0);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _spear_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Spear");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an Eldritch Beam.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_beam(GF_ELDRITCH, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _burst_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Burst");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an Eldritch Blast with increased radius.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  2);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _stunning_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Stunning");
		break;
	case SPELL_DESC:
		var_set_string(res, "Augments your Eldritch Blast with stunning effects.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH_STUN, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  0);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _empowered_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Empowered");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires a very powerful Eldritch Blast, but you can't use your powers again for a bit.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d*1.5 (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())*3/2), 
				  0);
		set_tim_no_spells(p_ptr->tim_no_spells + 1 + 1, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _draining_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Draining");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an Eldritch Blast which also does Drain Life.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH_DRAIN, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  0);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _prismatic_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Prismatic");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires multiple blasts, one each of fire, frost, acid, lightning and poison.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d*5 (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()/2), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		int dice = _warlock_dice();
		int sides = _warlock_sides();

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_FIRE, dir, spell_power(damroll(dice, sides)/2), 0);
		fire_ball(GF_COLD, dir, spell_power(damroll(dice, sides)/2), 0);
		fire_ball(GF_ACID, dir, spell_power(damroll(dice, sides)/2), 0);
		fire_ball(GF_ELEC, dir, spell_power(damroll(dice, sides)/2), 0);
		fire_ball(GF_POIS, dir, spell_power(damroll(dice, sides)/2), 0);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _dispelling_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Dispelling");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an Eldritch Blast which also does Dispel Magic.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH_DISPEL, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  0);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _vengeful_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Vengeful");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an extremely deadly Eldritch Blast, but you also take damage.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d*2 (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		int dam = damroll(_warlock_dice(), _warlock_sides());
		dam *= 2;
		dam = spell_power(dam);

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;

		fire_ball(GF_ELDRITCH, dir, dam, 0);
		take_hit(DAMAGE_USELIFE, dam/3, "vengeful blast", -1);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _confusing_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Confusing");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires an Eldritch Blast that also confuses your opponent.");
		break;
	case SPELL_INFO:
		var_set_string(res, 
			format("dam %dd%d (rng %d)", 
			       _warlock_dice(), 
			       spell_power(_warlock_sides()), 
					_warlock_range()));
		break;
	case SPELL_CAST:
	{
		int dir = 0;

		var_set_bool(res, FALSE);

		project_length = _warlock_range();
		if (!get_aim_dir(&dir)) return;
		
		fire_ball(GF_ELDRITCH_CONFUSE, 
		          dir, 
				  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
				  0);

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

#define MAX_WARLOCK_SPELLS	7

static spell_info _spells[MAX_WARLOCK_SPELLS] = 
{
    /*lvl cst fail spell */
    {  1,  0,  20, _basic_spell},
    { 10,  0,  40, _extended_spell},
    { 18,  0,  45, _spear_spell},
	{ 26,  0,  60, _burst_spell},
    { 33,  0,  60, _stunning_spell},
	{ 40,  0,  70, NULL},
    { 45,  0,  75, _empowered_spell},
};

static ang_spell _pact_spells[MAX_PACTS] = 
{
	_draining_spell,
	_prismatic_spell,
	_dispelling_spell,
	_vengeful_spell,
	_confusing_spell,
};

static int _get_spells(spell_info* spells, int max)
{
	int i;
	int ct = 0;
	int stat_idx = p_ptr->stat_ind[A_CHR];

	for (i = 0; i < MAX_WARLOCK_SPELLS; i++)
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

			/* Hack for the pact variable slot */
			if (current->fn == NULL)
				current->fn = _pact_spells[p_ptr->psubclass];

			ct++;
		}
	}
	return ct;
}

int _undead_get_powers(spell_info* spells, int max)
{
	int ct = 0;
/*
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
*/
	return ct;
}

int _dragon_get_powers(spell_info* spells, int max)
{
	int ct = 0;
/*
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
*/
	return ct;
}

int _angel_get_powers(spell_info* spells, int max)
{
	int ct = 0;
/*
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
*/

	return ct;
}

int _demon_get_powers(spell_info* spells, int max)
{
	int ct = 0;
/*
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

*/

	return ct;
}

int _aberration_get_powers(spell_info* spells, int max)
{
	int ct = 0;
/*
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
*/

	return ct;
}

static void _undead_calc_bonuses(void)
{
	p_ptr->resist_cold = TRUE;
	p_ptr->skill_stl += 7 * p_ptr->lev/50;
	if (p_ptr->lev > 14) p_ptr->resist_pois = TRUE;
	p_ptr->stat_add[A_CON] += 5 * p_ptr->lev/50;
	if (p_ptr->lev > 29) 
	{
		p_ptr->resist_neth = TRUE;
		p_ptr->hold_life = TRUE;
	}
	if (p_ptr->lev > 34) 
	{
		p_ptr->resist_dark = TRUE;
		p_ptr->resist_blind = TRUE;
	}
	if (p_ptr->lev > 44) p_ptr->resist_shard = TRUE;
}

static void _dragon_calc_bonuses(void)
{
	p_ptr->resist_fear = TRUE;
	p_ptr->skill_thn += 100 * p_ptr->lev / 50;
	if (p_ptr->lev > 14) p_ptr->levitation = TRUE; 
	p_ptr->stat_add[A_STR] += 5 * p_ptr->lev / 50;
	p_ptr->weapon_info[0].to_h += 10 * p_ptr->lev / 50;
	p_ptr->weapon_info[0].dis_to_h +=  10 * p_ptr->lev / 50;
	p_ptr->weapon_info[0].to_d += 10 * p_ptr->lev / 50;
	p_ptr->weapon_info[0].dis_to_d += 10 * p_ptr->lev / 50;
	p_ptr->weapon_info[1].to_h += 10 * p_ptr->lev / 50;
	p_ptr->weapon_info[1].dis_to_h +=  10 * p_ptr->lev / 50;
	p_ptr->weapon_info[1].to_d += 10 * p_ptr->lev / 50;
	p_ptr->weapon_info[1].dis_to_d += 10 * p_ptr->lev / 50;
	if (p_ptr->lev > 29) p_ptr->sustain_con = TRUE;
	if (p_ptr->lev > 29)
	{
		/* only give it if they don't already have it */
		if (!mut_present(MUT_RESIST))
		{
			mut_gain(MUT_RESIST);
			mut_lock(MUT_RESIST);
		}
	}
	/* only remove it if they got it from us ... hey, they could have used !Poly */
	else if (mut_present(MUT_RESIST) && mut_locked(MUT_RESIST))
	{
		mut_unlock(MUT_RESIST);
		mut_lose(MUT_RESIST);
	}
	if (p_ptr->lev > 44)
	{
		/* only give it if they don't already have it */
		if (!mut_present(MUT_BERSERK))
		{
			mut_gain(MUT_BERSERK);
			mut_lock(MUT_BERSERK);
		}
	}
	/* only remove it if they got it from us ... hey, they could have used !Poly */
	else if (mut_present(MUT_BERSERK) && mut_locked(MUT_BERSERK))
	{
		mut_unlock(MUT_BERSERK);
		mut_lose(MUT_BERSERK);
	}
}

static void _angel_calc_bonuses(void)
{
	p_ptr->levitation = TRUE;
	p_ptr->skill_sav += 30 * p_ptr->lev/50;
	if (p_ptr->lev > 14) p_ptr->see_inv = TRUE;
	p_ptr->stat_add[A_WIS] += 5 * p_ptr->lev/50;
	if (p_ptr->lev > 34) p_ptr->reflect = TRUE;
}

static void _demon_calc_bonuses(void)
{
	p_ptr->resist_fire = TRUE;
	p_ptr->skill_dev += 50 * p_ptr->lev/50;
	if (p_ptr->lev > 14) p_ptr->hold_life = TRUE;
	p_ptr->stat_add[A_INT] += 5 * p_ptr->lev/50;
	if (p_ptr->lev > 44)
		p_ptr->kill_wall = TRUE;
	if (p_ptr->lev > 49)
		p_ptr->immune_fire = TRUE;
}

static void _aberration_calc_bonuses(void)
{
	if (!mut_present(MUT_HORNS))
	{
		mut_gain(MUT_HORNS);
		mut_lock(MUT_HORNS);
	}
	p_ptr->skill_thb += 100 * p_ptr->lev/50;
	if (p_ptr->lev > 14)
	{
		/* only give it if they don't already have it */
		if (!mut_present(MUT_BEAK))
		{
			mut_gain(MUT_BEAK);
			mut_lock(MUT_BEAK);
		}
	}
	/* only remove it if they got it from us ... hey, they could have used !Poly */
	else if (mut_present(MUT_BEAK) && mut_locked(MUT_BEAK))
	{
		mut_unlock(MUT_BEAK);
		mut_lose(MUT_BEAK);
	}
	if (p_ptr->lev > 14)
	{
		/* only give it if they don't already have it */
		if (!mut_present(MUT_TENTACLES))
		{
			mut_gain(MUT_TENTACLES);
			mut_lock(MUT_TENTACLES);
		}
	}
	/* only remove it if they got it from us ... hey, they could have used !Poly */
	else if (mut_present(MUT_TENTACLES) && mut_locked(MUT_TENTACLES))
	{
		mut_unlock(MUT_TENTACLES);
		mut_lose(MUT_TENTACLES);
	}
	p_ptr->stat_add[A_DEX] += 5 * p_ptr->lev/50;
	if (p_ptr->lev > 44) p_ptr->telepathy = TRUE;	/* Easier then granting MUT3_ESP :) */
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "blast";
		init = TRUE;
	}
	return &me;
}

class_t *warlock_get_class_t(int psubclass)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 20,  24,  34,   1,  16,  20,  34,  20};
	skills_t xs = {  8,  10,  11,   0,   0,   0,  10,   8};

		me.name = "Warlock";
		me.desc = "A warlock, unlike typical mages, derives his powers from pacts with "
		          "arcane creatures, rather than through careful study.  They can cast "
				  "all magic they know at will without requiring any SP, and depending on "
				  "the type of Warlock, have different abilities.";
		me.stats[A_STR] = -2;
		me.stats[A_INT] =  1;
		me.stats[A_WIS] =  2;
		me.stats[A_DEX] = -1;
		me.stats[A_CON] = -2;
		me.stats[A_CHR] =  4;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		init = TRUE;
	}

	/* dynamic info */
	switch (psubclass)
	{
	case PACT_UNDEAD:
		me.calc_bonuses = _undead_calc_bonuses;
		me.get_powers = _undead_get_powers;
		break;
	case PACT_DRAGON:
		me.calc_bonuses = _dragon_calc_bonuses;
		me.get_powers = _dragon_get_powers;
		break;
	case PACT_ANGEL:
		me.calc_bonuses = _angel_calc_bonuses;
		me.get_powers = _angel_get_powers;
		break;
	case PACT_DEMON:
		me.calc_bonuses = _demon_calc_bonuses;
		me.get_powers = _demon_get_powers;
		break;
	case PACT_ABERRATION:
		me.calc_bonuses = _aberration_calc_bonuses;
		me.get_powers = _aberration_get_powers;
		break;
	}

	return &me;
}

