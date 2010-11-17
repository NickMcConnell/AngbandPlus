/****************************************************************
 * The Archaeologist
 ****************************************************************/

#include "angband.h"

/****************************************************************
 * Private Spells
 ****************************************************************/
static void _ancient_protection_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Ancient Protection");
		break;
	case SPELL_DESC:
		if (p_ptr->lev < 50)
			var_set_string(res, "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.");
		else
			var_set_string(res, "Sets glyphs on nearby floors. Monsters cannot attack you if you are on a glyph, but can try to break glyph.");
		break;
	case SPELL_CAST:
		warding_glyph();
		if (p_ptr->lev >= 50)
			glyph_creation();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _evacuation_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Escape Rope");
		break;
	case SPELL_DESC:
		var_set_string(res, "Danger!  Abandon this expedition and escape to a new level.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (get_check("Are you sure? (Escape Rope)"))
		{
			teleport_level(0);
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _excavation_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Excavation");
		break;
	case SPELL_DESC:
		var_set_string(res, "You break walls on your quest for treasure!  This takes a bit more time, though.");
		break;
	case SPELL_ENERGY:
		{
			int n = 200;
			n -= 80 * p_ptr->lev / 50;	
			var_set_int(res, n);
		}
		break;
	case SPELL_CAST:
		{
			int dir = 5;
			bool b = FALSE;

			if ( get_rep_dir2(&dir)
			  && dir != 5 )
			{
				int x, y;
				y = py + ddy[dir];
				x = px + ddx[dir];

				if (!in_bounds(y, x))
				{
					msg_print("You may excavate no further.");
				}
				else if ( cave_have_flag_bold(y, x, FF_WALL)
				       || cave_have_flag_bold(y, x, FF_TREE) 
					   || cave_have_flag_bold(y, x, FF_CAN_DIG) )
				{
					msg_print("You dig your way to treasure!");
					cave_alter_feat(y, x, FF_TUNNEL);
					teleport_player_to(y, x, TELEPORT_NONMAGICAL); /*??*/
					b = TRUE;
				}
				else
				{
					msg_print("There is nothing to excavate.");
				}
			}
			var_set_bool(res, b);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _first_aid_spell(int cmd, variant *res)
{
	int dice = 2 + p_ptr->lev/5;
	int sides = spell_power(10);

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "First Aid");
		break;
	case SPELL_DESC:
		if (p_ptr->lev < 8)
			var_set_string(res, "Heals HP and Stun.");
		else if (p_ptr->lev < 12)
			var_set_string(res, "Heals HP and Stun.  Cures cuts.");
		else if (p_ptr->lev < 16)
			var_set_string(res, "Heals HP and Stun.  Cures cuts and slows poison.");
		else if (p_ptr->lev < 20)
			var_set_string(res, "Heals HP and Stun.  Cures cuts and poison.");
		else if (p_ptr->lev < 30)
			var_set_string(res, "Heals HP and Stun.  Cures cuts, poison and blindness.");
		else if (p_ptr->lev < 40)
			var_set_string(res, "Heals HP and Stun.  Cures cuts, poison and blindness.  Restores Con.");
		else if (p_ptr->lev < 45)
			var_set_string(res, "Heals HP and Stun.  Cures cuts, poison and blindness.  Restores Con and Chr.");
		else
			var_set_string(res, "Heals HP and Stun.  Cures cuts, poison and blindness.  Restores Con, Chr and Str.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_heal(dice, sides, 0));
		break;
	case SPELL_CAST:
		hp_player(spell_power(damroll(dice, sides)));
		set_stun(0, TRUE);

		if (p_ptr->lev >= 8)
			set_cut(0, TRUE);
		if (p_ptr->lev >= 12 && p_ptr->lev < 16)
			set_poisoned(p_ptr->poisoned / 2, TRUE);
		if (p_ptr->lev >= 16)
			set_poisoned(0, TRUE);
		if (p_ptr->lev >= 20)
			set_blind(0, TRUE);
		if (p_ptr->lev >= 30)
			do_res_stat(A_CON);
		if (p_ptr->lev >= 40)
			do_res_stat(A_CHR);
		if (p_ptr->lev >= 45)
			do_res_stat(A_STR);

		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _identify_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Identify");
		break;
	case SPELL_DESC:
		if (p_ptr->lev < 25)
			var_set_string(res, "New Treasure!  You examine your new discovery.");
		else
			var_set_string(res, "New Treasure!  You examine your new discovery and learn its deepest truths.");
		break;
	case SPELL_CAST:
		{
			bool b = TRUE;
			if (p_ptr->lev < 25)
				b = ident_spell(FALSE);
			else
				b = identify_fully(FALSE);
			var_set_bool(res, b);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _magic_blueprint_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Magic Blueprint");
		break;
	case SPELL_DESC:
		if (p_ptr->lev < 20)
			var_set_string(res, "A map to treasure!  Maps the surrounding area.");
		else if (p_ptr->lev < 25)
			var_set_string(res, "A map to treasure!  Maps the surrounding area and detects traps and doors.");
		else if (p_ptr->lev < 30)
			var_set_string(res, "A map to treasure!  Maps the surrounding area and detects traps, doors and objects.");
		else if (p_ptr->lev < 35)
			var_set_string(res, "A map to treasure!  Maps the entire level and detects traps, doors and objects.");
		else
			var_set_string(res, "A map to treasure!  Maps and lights the entire level and detects traps, doors and objects.");
		break;
	case SPELL_CAST:
		{
			int rad = DETECT_RAD_DEFAULT;

			if (p_ptr->lev >= 30)
				rad = DETECT_RAD_ALL;

			map_area(rad);
			detect_treasure(rad);
			detect_objects_gold(rad);
			if (p_ptr->lev >= 20)
			{
				detect_traps(rad, TRUE);
				detect_doors(rad);
			}
			if (p_ptr->lev >= 25)
				detect_objects_normal(rad);

			if (p_ptr->lev >= 35)
				wiz_lite(FALSE);	/* somewhat redundant, but I want level wide trap detection! */

			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _pharaohs_curse_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Pharaoh's Curse");
		break;
	case SPELL_DESC:
		var_set_string(res, "Curses all nearby monsters, doing great damage and various effects.");
		break;
	case SPELL_CAST:
		{
			int power = spell_power(p_ptr->lev * 4);
			project_hack(GF_PHARAOHS_CURSE, p_ptr->lev + randint1(p_ptr->lev));
			if (p_ptr->lev >= 46)
				confuse_monsters(power);
			if (p_ptr->lev >= 47)
				slow_monsters();
			if (p_ptr->lev >= 48)
				turn_monsters(power);
			if (p_ptr->lev >= 49)
				stun_monsters(power);
			if (p_ptr->lev >= 50)		/* originally, an extra 50hp damage was suggested */
				stasis_monsters(power);
			if (one_in_(5))
			{
				int mode = 0;
				if (one_in_(2))
					mode = PM_FORCE_PET;
				if (summon_named_creature(0, py, px, MON_GREATER_MUMMY, mode))
				{
					msg_print("You have disturbed the rest of an ancient pharaoh!");
				}
			}
			take_hit(DAMAGE_USELIFE, p_ptr->lev + randint1(p_ptr->lev), "the Pharaoh's Curse", -1);
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _remove_curse_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Remove Curse");
		break;
	case SPELL_DESC:
		if (p_ptr->lev < 40)
			var_set_string(res, "Cursed Treasure!  Removes any weak curses from your equipment.");
		else
			var_set_string(res, "Cursed Treasure!  Removes any curses from your equipment.");
		break;
	case SPELL_CAST:
		if (p_ptr->lev < 40)
		{
			if (remove_curse()) msg_print("You feel the curse has lifted.");
			else msg_print("Hmmm ... nothing happens.");
		}
		else
		{
			if (remove_all_curse()) msg_print("You feel the curse has lifted.");
			else msg_print("Hmmm ... nothing happens.");
		}
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _remove_obstacles_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Remove Obstacles");
		break;
	case SPELL_DESC:
		var_set_string(res, "Clears a path to treasure!  Traps, doors and trees will be removed.");
		break;
	case SPELL_CAST:
		{
			bool b = FALSE;
			int dir = 5;
			if (get_aim_dir(&dir))
			{
				project(0, 1, py, px, 0, GF_REMOVE_OBSTACLE, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE, -1);
				project_hook(GF_REMOVE_OBSTACLE, dir, 0, PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM);
				b = TRUE;
			}
			var_set_bool(res, b);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}



/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define MAX_ARCHAEOLOGIST_SPELLS	13

spell_info archaeologist_spells[MAX_ARCHAEOLOGIST_SPELLS] = 
{
    /*lvl cst fail spell */
	{  1,   3, 20, light_area_spell },
	{  5,   5, 30, _first_aid_spell },
	{  8,   5, 35, detect_traps_spell },
	{ 10,  10, 35, _identify_spell },
	{ 12,  10, 45, _remove_obstacles_spell },
	{ 15,  15, 55, _magic_blueprint_spell },
	{ 18,  10, 40, _excavation_spell },
	{ 25,  20, 50, _remove_curse_spell },
	{ 32,  30, 70, recharging_spell },
	{ 35,  80, 70, _ancient_protection_spell },
	{ 40, 150, 80, polish_shield_spell },
	{ 42,  30, 50, _evacuation_spell },
	{ 45,  50, 85, _pharaohs_curse_spell },
};

/* The Archaeologists casts spells with mana, and never suffers side effects on failures. */
caster_info archaeologist_caster_info = { "spell", TRUE, FALSE, NULL, NULL };

int archaeologist_get_spells(spell_info* spells, int max)
{
	int i;
	int ct = 0;
	int stat_idx = (p_ptr->stat_ind[A_INT] + p_ptr->stat_ind[A_WIS]) / 2;
	
	/* Initialize a (copied) spell list with current casting costs and fail rates */
	for (i = 0; i < MAX_ARCHAEOLOGIST_SPELLS; i++)
	{
		spell_info *base = &archaeologist_spells[i];
		if (ct >= max) break;
		if (base->level <= p_ptr->lev)
		{
			spell_info* current = &spells[ct];
			current->fn = base->fn;
			current->level = base->level;

			current->fail = calculate_fail_rate(base, stat_idx);
			
			/* Mega Hack for First Aid */
			if (current->fn == _first_aid_spell)
			{
				current->cost = base->cost;
				if (p_ptr->lev > 49)
					current->cost += 10;
				if (p_ptr->lev > 44)
					current->cost += 4;
				if (p_ptr->lev > 39)
					current->cost += 4;
				if (p_ptr->lev > 29)
					current->cost += 4;
				if (p_ptr->lev > 19)
					current->cost += 2;
				if (p_ptr->lev > 15)
					current->cost += 2;
				if (p_ptr->lev > 11)
					current->cost += 2;
				if (p_ptr->lev > 7)
					current->cost += 2;
			}
			else
				current->cost = base->cost;
			
			ct++;
		}
	}
	return ct;
}

static bool _sense_great_discovery(int range)
{
	int i, y, x;
	int range2 = range;

	bool detect = FALSE;

	if (d_info[dungeon_type].flags1 & DF1_DARKNESS) range2 /= 3;

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Only alert to great discoveries */
		if (!object_is_artifact(o_ptr)) continue;

		/* Only alert to new discoveries */
	    if (object_is_known(o_ptr)) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (distance(py, px, y, x) > range2) continue;

		/* Hack -- memorize it */
		o_ptr->marked |= OM_FOUND;

		/* Redraw */
		lite_spot(y, x);

		/* Detect */
		detect = TRUE;
	}

	return (detect);
}

void archaeologist_on_process_player(void)
{
	bool sense = _sense_great_discovery(3 + p_ptr->lev/10);
	if (sense && !p_ptr->sense_artifact)
	{
		msg_print("You feel close to a great discovery!");
		p_ptr->sense_artifact = TRUE;
		p_ptr->redraw |= PR_STATUS;
	}
	else if (!sense && p_ptr->sense_artifact)
	{
		msg_print("You feel you are leaving something special behind...");
		p_ptr->sense_artifact = FALSE;
		p_ptr->redraw |= PR_STATUS;
	}
}
