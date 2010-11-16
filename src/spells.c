#include "angband.h"

/***********************************************************************
 * New Spell System ... Spells are objects (implemented as functions)
 * and can now be stored other data types (spell books, scrolls, etc).
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
		var_set_string(res, "");
		break;	

	case SPELL_CAST:
	case SPELL_FAIL:
	case SPELL_STOP:
	case SPELL_CONT:
		msg_print("Zap?");
		var_set_bool(res, TRUE);
		break;

	case SPELL_ENERGY:
		var_set_int(res, 100);
		break;
	}
}

static bool cast_spell(ang_spell spell)
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
 
/****************************************************************
 * The Spells
 ****************************************************************/

void detect_traps_spell(int cmd, variant *res)
{
	int rad = DETECT_RAD_DEFAULT;

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Detect Traps");
		break;
	case SPELL_DESC:
		var_set_string(res, "Detects traps in your vicinity.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_radius(rad));
		break;
	case SPELL_CAST:
		detect_traps(rad, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}


bool cast_detect_traps(void)
{
	return cast_spell(detect_traps_spell);
}

void light_area_spell(int cmd, variant *res)
{
	int dice = 2;
	int sides = spell_power(p_ptr->lev / 2);
	int rad = spell_power(p_ptr->lev / 10 + 1);

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Light Area");
		break;
	case SPELL_DESC:
		var_set_string(res, "Lights up nearby area and the inside of a room permanently.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(dice, sides, 0));
		break;
	case SPELL_CAST:
		lite_area(spell_power(damroll(dice, sides)), rad);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

bool cast_light_area(void)
{
	return cast_spell(light_area_spell);
}

void polish_shield_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Polish Shield");
		break;
	case SPELL_DESC:
		var_set_string(res, "Makes your shield reflect missiles and bolt spells.");
		break;
	case SPELL_CAST:
		polish_shield();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

bool cast_polish_shield(void)
{
	return cast_spell(polish_shield_spell);
}

void recharging_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Recharging");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to recharge staffs, wands or rods.  Items may be destroyed on failure.");
		break;
	case SPELL_CAST:
		var_set_bool(res, recharge(4 * p_ptr->lev));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

bool cast_recharging(void)
{
	return cast_spell(recharging_spell);
}

/****************************************************************************************
 * UI Utilities
 *   choose_spell - prompt user with a list of spells, they choose one.
 *   browse_spell - show spell list, user picks spells repeatedly.  describe each spell.
 ****************************************************************************************/
static void _list_spells(spell_info* spells, int ct, caster_info *caster)
{
	char temp[140];
	int  i;
	int  y = 1;
	int  x = 10;
	cptr cost_desc = NULL;
	variant name, info;

	var_init(&name);
	var_init(&info);

	if (caster->use_hp) cost_desc = "HP";
	else if (caster->use_sp) cost_desc = "SP";
	else cost_desc = "";

	Term_erase(x, y, 255);
	put_str(format("Lv   %s   Fail Info", cost_desc), y, x + 35);
	for (i = 0; i < ct; i++)
	{
		spell_info* spell = &spells[i];

		(spell->fn)(SPELL_NAME, &name);
		(spell->fn)(SPELL_INFO, &info);

		sprintf(temp, "  %c) ",I2A(i));
		strcat(temp, format("%-30s%2d %4d  %3d%%  %s", 
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

static int _choose_spell(spell_info* spells, int ct, caster_info *caster)
{
	int choice = -1;
	char prompt[140];
	variant name;

	var_init(&name);

	strnfmt(prompt, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
		caster->magic_desc, I2A(0), I2A(ct - 1), caster->magic_desc);

	_list_spells(spells, ct, caster);

	for (;;)
	{
		char ch = '\0';
		bool confirm_choice = FALSE;

		/* Prompt User */
		choice = -1;
		if (!get_com(prompt, &ch, TRUE)) break;

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

int choose_spell(spell_info* spells, int ct, caster_info *caster)
{
	int choice = -1;

	screen_save();
	choice = _choose_spell(spells, ct, caster);
	screen_load();

	return choice;
}

void browse_spells(spell_info* spells, int ct, caster_info *caster)
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
		
		choice = _choose_spell(spells, ct, caster);
		if (choice < 0 || choice >= ct) break;

		/* 2 lines below list of spells, 5 lines for description */
		for (i = 0; i < 7; i++)
			Term_erase(12, ct + i + 1, 255);

		/* Get the description, and line break it (max 5 lines) */
		spell = &spells[choice];
		(spell->fn)(SPELL_DESC, &info);
		roff_to_buf(var_get_string(&info), 62, tmp, sizeof(tmp));

		for(i = 0, line = ct + 3; tmp[i]; i += 1+strlen(&tmp[i]))
		{
			prt(&tmp[i], line, 15);
			line++;
		}
	}
	screen_load();

	var_clear(&info);
}

int calculate_fail_rate(const spell_info *spell, int stat_idx)
{
	int fail = spell->fail;
	if (fail)	/* Hack: 0% base failure is always 0% */
	{
		int min = 0;

		fail -= 3 * (p_ptr->lev - spell->level);
		fail += p_ptr->to_m_chance;
		fail -= 3 * (adj_mag_stat[stat_idx] - 1);

		min = adj_mag_fail[stat_idx];
		if (fail < min) fail = min;

		if (p_ptr->stun > 50) fail += 25;
		else if (p_ptr->stun) fail += 15;

		if (fail > 95) fail = 95;
	}
	return fail;
}
 
/****************************************************************
 * Entrypoints for the world
 ****************************************************************/

static int _get_spell_table(spell_info* spells, int max, caster_info **info)
{
	int ct = 0;
	*info = NULL;

	switch (p_ptr->pclass)
	{
	case CLASS_ARCHAEOLOGIST:
		ct = archaeologist_get_spells(spells, max);
		*info = &archaeologist_caster_info;
		break;
	}

	return ct;
}

void do_cmd_spell_browse(void)
{
	spell_info spells[MAX_SPELLS];
	caster_info *caster = NULL;
	int ct = _get_spell_table(spells, MAX_SPELLS, &caster);
	browse_spells(spells, ct, caster);
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

	choice = choose_spell(spells, ct, caster);
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
				if (caster->use_hp) p_ptr->chp += spell->cost;
				else if (caster->use_sp) p_ptr->csp += spell->cost;
				return;
			}
			sound(SOUND_ZAP); /* Wahoo! */
		}

		/* Pay Energy Use ... we already paid casting cost */
		energy_use = get_spell_energy(spell->fn);

		if (caster->use_hp)
			take_hit(DAMAGE_USELIFE, spell->cost, "concentrating too hard", -1);

		if (caster->on_cast != NULL)
			(caster->on_cast)(spell);

		p_ptr->redraw |= (PR_MANA);
		p_ptr->redraw |= (PR_HP);
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}
}