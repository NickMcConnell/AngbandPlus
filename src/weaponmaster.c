/****************************************************************
 * The Weaponmaster
 ****************************************************************/

#include "angband.h"

int shoot_hack = SHOOT_NONE;
int shoot_count = 0;
int shoot_item = 0;

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
static bool _check_speciality2_equip(void);
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

static int _set_toggle(s32b toggle)
{
	int result = p_ptr->magic_num1[0];
	p_ptr->magic_num1[0] = toggle;

	p_ptr->redraw |= (PR_STATUS);
	p_ptr->update |= (PU_BONUS);
	handle_stuff();

	return result;
}

/****************************************************************
 * Private Spells
 ****************************************************************/

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
		break;
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


/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define _MAX_OBJECTS_PER_SPECIALITY		32
#define _MAX_SPECIALITIES				10
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
		{ TV_SWORD, SV_BROKEN_DAGGER } ,
		{ TV_SWORD, SV_DAGGER } ,
		{ TV_SWORD, SV_FALCON_SWORD } ,
	    { TV_SWORD, SV_MAIN_GAUCHE } ,
		{ TV_SWORD, SV_NINJATO } ,
		{ TV_SWORD, SV_RAPIER } ,
		{ TV_SWORD, SV_SABRE } ,
		{ TV_SWORD, SV_TANTO } ,
		{ 0, 0 },
	  },
	  {
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
	    { -1,   0,  0, NULL },
	  },
	  { TV_POLEARM, SV_SPEAR },
	},
	{ "Shields",
	  "",
	  INVEN_LARM, INVEN_RARM,
	  { 
		{ TV_SHIELD, SV_DRAGON_SHIELD },
		{ TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
		{ TV_SHIELD, SV_LARGE_METAL_SHIELD },
		{ TV_SHIELD, SV_MIRROR_SHIELD },
	    { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
		{ TV_SHIELD, SV_SMALL_METAL_SHIELD },
		{ 0, 0 },
	  },
	  {
	    { -1,   0,  0, NULL },
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
	    { -1,   0,  0, NULL },
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
	    { -1,   0,  0, NULL },
	  },
	  { TV_SWORD, SV_LONG_SWORD } ,
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

	/* First slot should always match */
	if (!_check_speciality1_aux(&inventory[ptr->slot1])) return FALSE;

	/* Second slot might be the shield slot for weaponmasters, or the weapon slot for shieldmasters
	   Try to handle these cases */
	if (ptr->slot2 != -1 && inventory[ptr->slot2].tval != 0)
	{
		if (strcmp(ptr->name, "Shields") == 0)
		{
			if ( !object_is_weapon(&inventory[ptr->slot2]) 
			  && !_check_speciality1_aux(&inventory[ptr->slot2])) return FALSE;
		}
		else
		{
			if ( !object_is_shield(&inventory[ptr->slot2]) 
			  && !_check_speciality1_aux(&inventory[ptr->slot2])) return FALSE;
		}
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

	/* First slot should always match */
	if (!_check_speciality2_aux(&inventory[ptr->slot1])) return FALSE;

	/* Second slot might be the shield slot for weaponmasters, or the weapon slot for shieldmasters
	   Try to handle these cases 
	   Note: A non-speciality2 weapon in slot2 disqualifies all speciality2 abilities!! */
	if (ptr->slot2 != -1 && inventory[ptr->slot2].tval != 0)
	{
		if (strcmp(ptr->name, "Shields") == 0)
		{
			if ( !object_is_weapon(&inventory[ptr->slot2]) 
			  && !_check_speciality2_aux(&inventory[ptr->slot2])) return FALSE;
		}
		else
		{
			if ( !object_is_shield(&inventory[ptr->slot2]) 
			  && !_check_speciality2_aux(&inventory[ptr->slot2])) return FALSE;
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
		me.options = CASTER_NO_SPELL_FAIL;
		init = TRUE;
	}
	return &me;
}

int _prompt_for_speciality1(void)
{
	menu_list_t list = { "Choose a speciality.", "Browse which speciality?", NULL,
						_speciality_name, _speciality_help, NULL, 
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

		p_ptr->weapon_exp[kind.tval-TV_WEAPON_BEGIN][kind.sval] = WEAPON_EXP_BEGINNER;
	}
	weaponmaster_adjust_skills();
}

void weaponmaster_adjust_skills(void)
{
	int i;
	_object_kind kind;

	/* Fix up skills for Speciality.  This needs to be called every time the game is loaded! */
	for (i = 0; i < _MAX_OBJECTS_PER_SPECIALITY; i++)
	{
		kind = _specialities[p_ptr->speciality1].objects[i];
		if (kind.tval == 0) break;

		s_info[p_ptr->pclass].w_max[kind.tval-TV_WEAPON_BEGIN][kind.sval] = WEAPON_EXP_MASTER;
	}

	if (strcmp(_specialities[p_ptr->speciality1].name, "Slings") == 0)
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

	if (!p_ptr->painted_target)
	{
		p_ptr->painted_target_idx = 0;
		p_ptr->painted_target_ct = 0;
	}

	/* Handle cases where user swaps in unfavorable gear */
	if (!spec2 && _get_toggle() != TOGGLE_NONE)
		_set_toggle(TOGGLE_NONE);

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
		else
		{
		/*
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
			} */
		}

		init = TRUE;
		last_spec1 = spec1;
		last_spec2 = spec2;
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
		me.move_player = _move_player;
		init = TRUE;
	}

	return &me;
}