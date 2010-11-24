#include "angband.h"

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
		var_set_int(res, 0);
		break;

	case SPELL_ENERGY:
		var_set_int(res, 100);
		break;

	case SPELL_CALC_BONUS:
		var_set_bool(res, TRUE);
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

static int get_spell_energy(ang_spell spell)
{
	int n;
	variant res;
	var_init(&res);
	spell(SPELL_ENERGY, &res);
	n = var_get_int(&res);
	var_clear(&res);
	return n;
}

static int get_spell_cost_extra(ang_spell spell)
{
	int n;
	variant res;
	var_init(&res);
	spell(SPELL_COST_EXTRA, &res);
	n = var_get_int(&res);
	var_clear(&res);
	return n;
}

/****************************************************************************************
 * UI Utilities
 *   choose_spell - prompt user with a list of spells, they choose one.
 *   browse_spell - show spell list, user picks spells repeatedly.  describe each spell.
 ****************************************************************************************/
static void _list_spells(spell_info* spells, int ct)
{
	char temp[140];
	int  i;
	int  y = 1;
	int  x = 10;
	variant name, info;

	var_init(&name);
	var_init(&info);

	/* TODO: Use 2 columns when the list is larger than screen height.
	         But leave at least 5 columns on the bottom for browse info.
	*/

	Term_erase(x, y, 255);
	put_str("Lv Cost Fail Info", y, x + 29);
	for (i = 0; i < ct; i++)
	{
		char letter = '\0';
		spell_info* spell = &spells[i];

		(spell->fn)(SPELL_NAME, &name);
		(spell->fn)(SPELL_INFO, &info);

		if (i < 26)
			letter = I2A(i);
		else
			letter = '0' + i - 26;

		sprintf(temp, "  %c) ", letter);
		strcat(temp, format("%-23.23s %2d %4d %3d%% %s", 
							var_get_string(&name),
							spell->level,
							spell->cost,
							spell->fail,
							var_get_string(&info)));

		prt(temp, y + i + 1, x);
	}

	var_clear(&name);
	var_clear(&info);
}

static int _choose_spell(spell_info* spells, int ct, cptr desc)
{
	int choice = -1;
	char prompt[140];
	variant name;

	var_init(&name);

	strnfmt(prompt, 78, "Use which %s? ", desc);
	_list_spells(spells, ct);

	for (;;)
	{
		char ch = '\0';
		bool confirm_choice = FALSE;

		/* Prompt User */
		choice = -1;
		if (!get_com(prompt, &ch, TRUE)) break;

		/* TODO: Handle more than 26 possibilities! */
		if (isupper(ch))
		{
			confirm_choice = TRUE;
			ch = tolower(ch);
		}

		choice = islower(ch) ? A2I(ch) : -1;

		/* Valid Choice? */
		if (choice < 0 || choice >= ct)
		{
			bell();
			continue;
		}

		/* Confirm Choice? */
		if (confirm_choice)
		{
			char tmp[160];
			spell_info* spell = &spells[choice];
			(spell->fn)(SPELL_NAME, &name);
			strnfmt(tmp, 78, "Use %s? ", var_get_string(&name));
			if (!get_check(tmp)) continue;
		}

		/* Good to go! */
		break;
	}
	
	var_clear(&name);
	return choice;
}

int choose_spell(spell_info* spells, int ct, cptr desc)
{
	int choice = -1;

	if (REPEAT_PULL(&choice))
	{
		if (choice >= 0 && choice < ct)
			return choice;
	}

	screen_save();

	choice = _choose_spell(spells, ct, desc);
	REPEAT_PUSH(choice);

	screen_load();

	return choice;
}

void browse_spells(spell_info* spells, int ct, cptr desc)
{
	char tmp[62*5];
	int i, line;
	variant info;

	var_init(&info);

	screen_save();

	for(;;)
	{
		spell_info* spell = NULL;
		int choice = -1;
		
		choice = _choose_spell(spells, ct, desc);
		if (choice < 0 || choice >= ct) break;

		/* 2 lines below list of spells, 5 lines for description */
		for (i = 0; i < 7; i++)
			Term_erase(12, MIN(ct, 18) + i + 1, 255);

		/* Get the description, and line break it (max 5 lines) */
		spell = &spells[choice];
		(spell->fn)(SPELL_DESC, &info);
		roff_to_buf(var_get_string(&info), 62, tmp, sizeof(tmp));

		for(i = 0, line = MIN(ct, 18) + 3; tmp[i]; i += 1+strlen(&tmp[i]))
		{
			prt(&tmp[i], line, 15);
			line++;
		}
	}
	screen_load();

	var_clear(&info);
}

int calculate_fail_rate(int level, int base_fail, int stat_idx)
{
	int fail = base_fail;

	if (p_ptr->lev < level)
		return 100;

	if (fail)	/* Hack: 0% base failure is always 0% */
	{
		int min = 0;

		fail -= 3 * (p_ptr->lev - level);
		fail += p_ptr->to_m_chance;
		fail -= 3 * (adj_mag_stat[stat_idx] - 1);

		min = adj_mag_fail[stat_idx];
		if (fail < min) fail = min;
	}

	/* Stunning affects even 0% fail spells */
	if (p_ptr->stun > 50) fail += 25;
	else if (p_ptr->stun) fail += 15;

	if (fail > 95) fail = 95;
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
	
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}
	
	ct = _get_spell_table(spells, MAX_SPELLS, &caster);
	if (ct == 0)
	{
		/* User probably canceled the prompt for a spellbook */
		return;
	}

	choice = choose_spell(spells, ct, caster->magic_desc);

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

	choice = choose_spell(spells, ct, "power");

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