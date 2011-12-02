#include "angband.h"

/* Table of All Spells.  The index into this table will be stored in 
   object_type.#### and player_type.####, so please do not reorder stuff!
   Always add new spells at the end. */
static spell_t _spells[] = 
{
	{realm_life, cure_wounds_I_spell, 1, 1, 30, spell_allow_potion | spell_allow_shop | spell_allow_read, 1},
};



/***********************************************************************
 * New Spell System ... Spells are objects (implemented as functions)
 * and can now be stored other data types (spell books, scrolls, etc).
 *
 * 'Spell' is misleading.  This will be used by spells, racial powers,
 * mutations, potions, scrolls, etc.
 *
 * I'm attempting a grand unification of all player effects to allow
 * for some more code flexibility (e.g. scrolls that hold *any* spell and
 * Spells copied from scrolls into "books" for spellcasters, etc.)
 ***********************************************************************/
 
void default_spell(int cmd, variant *res) /* Base class */
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Unkown Spell");
		break;

	case SPELL_DESC:
	case SPELL_INFO:
	case SPELL_MUT_DESC:
		var_set_string(res, "");
		break;	

	case SPELL_GAIN_MUT:
	case SPELL_LOSE_MUT:
		var_clear(res);
		break;

	case SPELL_CAST:
	case SPELL_FAIL:
	case SPELL_STOP:
	case SPELL_CONT:
		msg_print("Zap?");
		var_set_bool(res, TRUE);
		break;

	case SPELL_COST_EXTRA:
	case SPELL_FAIL_MIN:
		var_set_int(res, 0);
		break;

	case SPELL_ENERGY:
		var_set_int(res, 100);
		break;

	case SPELL_CALC_BONUS:
		var_set_bool(res, TRUE);
		break;

	case SPELL_COLOR:
		var_set_int(res, TERM_WHITE);
		break;

	default:
		var_clear(res);
		break;
	}
}

bool cast_spell(ang_spell spell)
{
	bool b;
	variant res;
	var_init(&res);
	spell(SPELL_CAST, &res);
	b = var_get_bool(&res);
	var_clear(&res);
	return b;
}

int get_spell_energy(ang_spell spell)
{
	int n;
	variant res;
	var_init(&res);
	spell(SPELL_ENERGY, &res);
	n = var_get_int(&res);
	var_clear(&res);
	return n;
}

int get_spell_cost_extra(ang_spell spell)
{
	int n;
	variant res;
	var_init(&res);
	spell(SPELL_COST_EXTRA, &res);
	n = var_get_int(&res);
	var_clear(&res);
	return n;
}

int get_spell_fail_min(ang_spell spell)
{
	int n;
	variant res;
	var_init(&res);
	spell(SPELL_FAIL_MIN, &res);
	n = var_get_int(&res);
	var_clear(&res);
	return n;
}

/****************************************************************************************
 * UI Utilities
 *   choose_spell - prompt user with a list of spells, they choose one.
 *   browse_spell - show spell list, user picks spells repeatedly.  describe each spell.
 ****************************************************************************************/

 static int _col_height(int ct)
 {
	int  w, h;
	int result = ct;

	Term_get_size(&w, &h);

	h -= 5; /* Room for browsing */
	if (result > h)
	{
		result = (ct + 1)/2;
	}

	return result;
 }

static void _list_spells(spell_info* spells, int ct, int max_cost)
{
	char temp[140];
	int  i;
	int  y = 1;
	int  x = 10;
	int  col_height = _col_height(ct);
	int  col_width;
	variant name, info, color;

	var_init(&name);
	var_init(&info);
	var_init(&color);

	Term_erase(x, y, 255);

	if (col_height == ct)
	{
		Term_erase(x, y, 255);
		put_str("Lv Cost Fail Info", y, x + 29);
	}
	else
	{
		col_width = 42;
		x = 1;
		Term_erase(x, y, 255);
		put_str("Lv Cost Fail", y, x + 29);
		put_str("Lv Cost Fail", y, x + col_width + 29);
	}

	for (i = 0; i < ct; i++)
	{
		char letter = '\0';
		byte attr = TERM_WHITE;
		spell_info* spell = &spells[i];

		var_set_int(&color, TERM_WHITE);

		(spell->fn)(SPELL_NAME, &name);
		(spell->fn)(SPELL_INFO, &info);
		(spell->fn)(SPELL_COLOR, &color);

		attr = var_get_int(&color);

		if (i < 26)
			letter = I2A(i);
		else if (i < 52)
			letter = 'A' + i - 26;
		else
			letter = '0' + i - 52;

		sprintf(temp, "  %c) ", letter);

		strcat(temp, format("%-23.23s %2d %4d %3d%%", 
							var_get_string(&name),
							spell->level,
							spell->cost,
							spell->fail));

		if (col_height == ct)
			strcat(temp, format(" %s", var_get_string(&info)));

		if (spell->fail == 100)
			attr = TERM_L_DARK;

		if (spell->cost > max_cost)
			attr = TERM_L_DARK;

		if (i < col_height)
		{
			c_prt(attr, temp, y + i + 1, x);
		}
		else
		{
			c_prt(attr, temp, y + (i - col_height) + 1, (x + col_width));
		}
	}

	var_clear(&name);
	var_clear(&info);
	var_clear(&color);
}

static void _describe_spell(spell_info *spell, int col_height)
{
	char tmp[62*5];
	int i, line;
	variant info;

	var_init(&info);

	/* 2 lines below list of spells, 5 lines for description */
	for (i = 0; i < 7; i++)
		Term_erase(12, col_height + i + 2, 255);

	/* Get the description, and line break it (max 5 lines) */
	(spell->fn)(SPELL_DESC, &info);
	roff_to_buf(var_get_string(&info), 62, tmp, sizeof(tmp));

	for(i = 0, line = col_height + 3; tmp[i]; i += 1+strlen(&tmp[i]))
	{
		prt(&tmp[i], line, 15);
		line++;
	}

	(spell->fn)(SPELL_INFO, &info);
	prt(format("%^s", var_get_string(&info)), line, 15);

	var_clear(&info);
}

static int _choose_spell(spell_info* spells, int ct, cptr desc, int max_cost)
{
	int choice = -1;
	char prompt1[140];
	char prompt2[140];
	variant name;
	bool describe = FALSE;

	var_init(&name);

	strnfmt(prompt1, 78, "Use which %s? (Type '?' to Browse) ", desc);
	strnfmt(prompt2, 78, "Browse which %s? (Type '?' to Use)", desc);
	_list_spells(spells, ct, max_cost);

	for (;;)
	{
		char ch = '\0';

		/* Prompt User */
		choice = -1;

		if (!get_com(describe ? prompt2 : prompt1, &ch, FALSE)) break;

		if (ch == '?')
		{
			describe = !describe;
			if (!get_com(describe ? prompt2 : prompt1, &ch, FALSE)) break;
		}

		if (isupper(ch))
			choice = ch - 'A' + 26;
		else if (islower(ch))
			choice = ch - 'a';
		else if (ch >= '0' && ch <= '9')
			choice = ch - '0' + 52;

		/* Valid Choice? */
		if (choice < 0 || choice >= ct)
		{
			bell();
			continue;
		}

		if (describe)
		{
			_describe_spell(&spells[choice], _col_height(ct));
			continue;
		}

		/* Good to go! */
		break;
	}
	
	var_clear(&name);
	return choice;
}

int choose_spell(spell_info* spells, int ct, cptr desc, int max_cost)
{
	int choice = -1;

	if (REPEAT_PULL(&choice))
	{
		if (choice >= 0 && choice < ct)
			return choice;
	}

	screen_save();

	choice = _choose_spell(spells, ct, desc, max_cost);
	REPEAT_PUSH(choice);

	screen_load();

	return choice;
}

void browse_spells(spell_info* spells, int ct, cptr desc)
{
	screen_save();

	for(;;)
	{
		spell_info* spell = NULL;
		int choice = -1;
		
		choice = _choose_spell(spells, ct, desc, 10000);
		if (choice < 0 || choice >= ct) break;

		_describe_spell(&spells[choice], _col_height(ct));
	}
	screen_load();
}

int calculate_cost(int cost)
{
	int result = cost;
	caster_info *caster_ptr = get_caster_info();

	if (caster_ptr && (caster_ptr->options & CASTER_NO_SPELL_COST))
		return 0;

	if (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
	{
		if (p_ptr->dec_mana)
			result = result * 3 / 4;
	}

	return result;
}

int calculate_fail_rate(int level, int base_fail, int stat_idx)
{
	int fail = base_fail;
	int min = 0;
	caster_info *caster_ptr = get_caster_info();

	if (p_ptr->lev < level)
		return 100;

	if (caster_ptr && (caster_ptr->options & CASTER_NO_SPELL_FAIL))
		return 0;

	/* Adjust Fail Rate */
	fail -= 3 * (p_ptr->lev - level);
	fail += p_ptr->to_m_chance;
	fail -= 3 * (adj_mag_stat[stat_idx] - 1);
	if (p_ptr->heavy_spell) fail += 20;

	if (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
	{
		if (p_ptr->dec_mana && p_ptr->easy_spell) fail -= 4;
		else if (p_ptr->easy_spell) fail -= 3;
		else if (p_ptr->dec_mana) fail -= 2;
	}

	/* Apply Min Fail Rate */
	min = adj_mag_fail[stat_idx];

	if (caster_ptr && min < caster_ptr->min_fail)
		min = caster_ptr->min_fail;

	if (mut_present(MUT_ARCANE_MASTERY))
		fail -= 5;

	if (prace_is_(RACE_DEMIGOD) && p_ptr->psubrace == DEMIGOD_ATHENA)
	{
		fail -= 5;
		min -= 1;
	}

	if (fail < min) fail = min;

	/* Stunning affects even 0% fail spells */
	if (p_ptr->stun > 50) fail += 25;
	else if (p_ptr->stun) fail += 15;

	/* Max Fail Rate */
	if (fail > 95) fail = 95;

	/* Some effects violate the Min/Max Fail Rates */
	if (p_ptr->heavy_spell) fail += 5; /* Fail could go to 100% */

	if (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
	{
		if (p_ptr->dec_mana) fail--; /* 5% casters could get 4% fail rates */
	}

	if (fail < 0) fail = 0;
	return fail;
}
 
/****************************************************************
 * Entrypoints for the world
 ****************************************************************/

static void _add_extra_costs(spell_info* spells, int max)
{
	int i;
	/* Some spells give extra abilities depending on player level ...
	   Ideally, these spells should scale the costs as well! */
	for (i = 0; i < max; i++)
	{
		spell_info* current = &spells[i];
		current->cost += get_spell_cost_extra(current->fn);
		current->fail = MAX(current->fail, get_spell_fail_min(current->fn));
		current->cost = calculate_cost(current->cost);
	}
}


static int _get_spell_table(spell_info* spells, int max, caster_info **info)
{
	int ct = 0;
	class_t *class_ptr = get_class_t();

	*info = NULL;
	if (class_ptr != NULL)
	{
		if (class_ptr->get_spells != NULL)
			ct = (class_ptr->get_spells)(spells, max);

		if (class_ptr->caster_info != NULL)
			*info = (class_ptr->caster_info)();
	}

	_add_extra_costs(spells, ct);
	return ct;
}

void do_cmd_spell_browse(void)
{
	spell_info spells[MAX_SPELLS];
	caster_info *caster = NULL;
	/* TODO: Prompt for Racial Powers as well! */
	int ct = _get_spell_table(spells, MAX_SPELLS, &caster);
	if (ct == 0)
	{
		/* User probably canceled the prompt for a spellbook */
		return;
	}
	browse_spells(spells, ct, caster->magic_desc);
}

void do_cmd_spell(void)
{
	spell_info spells[MAX_SPELLS];
	caster_info *caster = NULL;
	int ct = 0; 
	int choice = 0;
	int max_cost = 0;
	
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	if (p_ptr->special_defense & KATA_MASK)
	{
		set_action(ACTION_NONE);
	}
	
	ct = _get_spell_table(spells, MAX_SPELLS, &caster);
	if (ct == 0)
	{
		/* User probably canceled the prompt for a spellbook */
		return;
	}

	if (caster->use_hp)
		max_cost = p_ptr->chp;
	else if (caster->use_sp)
		max_cost = p_ptr->csp;
	else
		max_cost = 10000;
	choice = choose_spell(spells, ct, caster->magic_desc, max_cost);

	if (choice >= 0 && choice < ct)
	{
		spell_info *spell = &spells[choice];

		/* Verify Cost ... Note, I'm removing options for over exertion 
		   Also note we now pay casting costs up front for mana casters.  
		   If the user cancels, then we return the cost below.
		*/
		if (caster->use_hp)
		{
			if (spell->cost > p_ptr->chp)
			{
				msg_print("You do not have enough hp to use this power.");
				return;
			}
		}
		else if (caster->use_sp)
		{
			if (spell->cost > p_ptr->csp)
			{
				msg_print("You do not have enough mana to use this power.");
				return;
			}
			p_ptr->csp -= spell->cost;
		}

		/* Check for Failure */
		if (randint0(100) < spell->fail)
		{
			sound(SOUND_FAIL); /* Doh! */
			if (flush_failure) flush();
			msg_print("You failed to concentrate hard enough!");
			if (caster->use_sp && prace_is_(RACE_DEMIGOD) && p_ptr->psubrace == DEMIGOD_ATHENA) 
				p_ptr->csp += spell->cost/2;
			if (caster->on_fail != NULL)
				(caster->on_fail)(spell);
		}
		else
		{
			if (!cast_spell(spell->fn))
			{
				/* Give back the spell cost, since the user canceled the spell */
				if (caster->use_sp) p_ptr->csp += spell->cost;
				return;
			}
			sound(SOUND_ZAP); /* Wahoo! */
		}

		/* Pay Energy Use ... we already paid casting cost */
		energy_use = get_spell_energy(spell->fn);

		if (caster->use_hp && spell->cost > 0)
			take_hit(DAMAGE_USELIFE, spell->cost, "concentrating too hard", -1);

		if (caster->on_cast != NULL)
			(caster->on_cast)(spell);

		p_ptr->redraw |= (PR_MANA);
		p_ptr->redraw |= (PR_HP);
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}
}

void do_cmd_power(void)
{
	spell_info spells[MAX_SPELLS];
	int ct = 0; 
	int choice = 0;
	race_t *race_ptr = get_race_t();
	class_t *class_ptr = get_class_t();
	
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}
	
	if (race_ptr != NULL && race_ptr->get_powers != NULL)
	{
		ct += (race_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);
	}
	/* Temp Hack during refactoring ... */
	ct += get_racial_powers(spells + ct, MAX_SPELLS - ct);

	if (class_ptr != NULL && class_ptr->get_powers != NULL)
	{
		ct += (class_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);
	}
	/* Temp Hack during refactoring ... */
	ct += get_class_powers(spells + ct, MAX_SPELLS - ct);

	ct += mut_get_powers(spells + ct, MAX_SPELLS - ct);

	if (ct == 0)
	{
		msg_print("You have no powers.");
		return;
	}

	_add_extra_costs(spells, ct);

	choice = choose_spell(spells, ct, "power", p_ptr->csp + p_ptr->chp);

	if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
	{
		set_action(ACTION_NONE);
	}

	if (choice >= 0 && choice < ct)
	{
		spell_info *spell = &spells[choice];
		
		if (spell->level > p_ptr->lev)
		{
			msg_print("You can't cast that spell yet!");
			return;
		}

		if (spell->cost > p_ptr->chp + p_ptr->csp)
		{
			msg_print("Using this power will kill you!  Why not rest a bit first?");
			return;
		}

		/* Check for Failure */
		if (randint0(100) < spell->fail)
		{
			sound(SOUND_FAIL); /* Doh! */
			if (flush_failure) flush();
			msg_print("You failed to concentrate hard enough!");
		}
		else
		{
			if (!cast_spell(spell->fn))
				return;
			sound(SOUND_ZAP); /* Wahoo! */
		}

		/* Pay Energy Use ... we already paid casting cost */
		energy_use = get_spell_energy(spell->fn);

		/* Casting costs spill over into hit points */
		if (p_ptr->csp < spell->cost)
		{
			int cost = spell->cost - p_ptr->csp;
			p_ptr->csp = 0;
			take_hit(DAMAGE_USELIFE, cost, T("concentrating too hard", "過度の集中"), -1);
		}
		else 
			p_ptr->csp -= spell->cost;

		p_ptr->redraw |= (PR_MANA);
		p_ptr->redraw |= (PR_HP);
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}
}