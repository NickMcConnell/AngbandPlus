/****************************************************************
 * The Rune Knight
 ****************************************************************/

#include "angband.h"

/****************************************************************
 * Public Helpers
 ****************************************************************/

cptr rune_desc(int which)
{
	switch (which)
	{
	case RUNE_ABSORPTION:
		return "<<Absorption>>";
		break;
	case RUNE_REGENERATION:
		return "<<Regeneration>>";
		break;
	case RUNE_DEFLECTION:
		return "<<Deflection>>";
		break;
	case RUNE_STASIS:
		return "<<Stasis>>";
		break;
	case RUNE_DESTRUCTION:
		return "<<Destruction>>";
		break;
	case RUNE_ELEMENTAL_PROTECTION:
		return "<<Elemental Protection>>";
		break;
	case RUNE_SACRIFICE:
		return "<<Sacrifice>>";
		break;
	case RUNE_REFLECTION:
		return "<<Reflection>>";
		break;
	case RUNE_GOOD_FORTUNE:
		return "<<Good Fortune>>";
		break;
	case RUNE_BODY:
		return "<<Body>>";
		break;
	case RUNE_MIND:
		return "<<Mind>>";
		break;
	}
	return "<<Unknown>>";
}

bool rune_add(object_type *o_ptr, int which, bool prompt)	/* Birthing needs access to this ... */
{
	char o_name[MAX_NLEN];

	if (!which) return FALSE;

	object_desc(o_name, o_ptr, 0);

	/* For now, the restriction is one rune per object.  I'm using flags in case we change our mind ... */
	if (o_ptr->rune_flags)
	{
		msg_format("%^s already has an attached rune.", o_name);
		return FALSE;
	}

	if (prompt)
	{
		if (!get_check(
				format("Really add %^s to %^s?", 
					rune_desc(which), o_name))) return FALSE;
	}

	o_ptr->rune_flags = which;

	/* Runes protect objects from the elements */
	add_flag(o_ptr->art_flags, TR_IGNORE_ACID);
	add_flag(o_ptr->art_flags, TR_IGNORE_ELEC);
	add_flag(o_ptr->art_flags, TR_IGNORE_FIRE);
	add_flag(o_ptr->art_flags, TR_IGNORE_COLD);

	/* Some runes can be completely handled by existing flags */
	if (which == RUNE_DESTRUCTION)
		add_flag(o_ptr->art_flags, TR_FORCE_WEAPON);

	if (which == RUNE_REFLECTION)
		add_flag(o_ptr->art_flags, TR_REFLECT);

	msg_format("%^s gleams.", o_name);
	p_ptr->update |= PU_BONUS;

	return TRUE;
}

/****************************************************************
 * Private Spells and Helpers
 ****************************************************************/

typedef bool (*object_pred)(object_type *o_ptr);

static object_type *_rune_object_prompt(object_pred pred)
{
	object_type * result = NULL;
	object_pred   old = item_tester_hook;
	int           item;

	item_tester_hook = pred;

	if (get_item(&item, 
		         "Enchant which item?", 
			     "You have nothing to enchant.", 
				 (USE_EQUIP | USE_INVEN | USE_FLOOR))) 
	{
		if (item >= 0) /* Pack */
			result = &inventory[item];
		else /* Floor */
			result = &o_list[0 - item];
	}

	item_tester_hook = old;
	return result;
}

static void _rune_default_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->msp);
		break;
	/*case SPELL_COLOR:
		var_set_int(res, TERM_L_BLUE);
		break; */
	default:
		default_spell(cmd, res);
		break;
	}
}

static void _rune_of_absorption_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Absorption");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Absorption on a weapon.  If you are hit with a magical spell while wielding that weapon, you take less damage and you recover some spell points.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_melee_weapon);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_ABSORPTION, TRUE));

		break;
	}
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->msp/3);
		break;
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_regeneration_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Regeneration");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Regeneration on a body armor. If you are wearing that armor then you regain hitpoints more quickly.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_body_armour);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_REGENERATION, TRUE));
		
		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_deflection_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Deflection");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Deflection on a shield. If you are wearing that shield, your AC is improved.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_shield);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_DEFLECTION, TRUE));

		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_stasis_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Stasis");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Stasis on a body armor. If you are wearing that armor, you gain Sustain Str at Lvl 15, Sustain Dex at Lvl 20, Sustain Int at Lvl 25, Sustain Con at Lvl 30, Sustain Wis and Chr at Lvl 35, Hold Life at Lvl 40, Resist Disenchantment at Lvl 45, and Resist Time at Lvl 50.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_body_armour);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_STASIS, TRUE));
		
		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_destruction_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Destruction");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Destruction on a weapon. Your weapon becomes Force-branded.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_melee_weapon);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_DESTRUCTION, TRUE));

		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_elemental_protection_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Protection");
		break;
	case SPELL_DESC:
		var_set_string(res, "Creates a standalone rune. As long as you have this rune in your inventory, your inventory items are 50% less likely to be destroyed by elemental attacks.");
		break;
	case SPELL_CAST:
	{
		object_type forge;

		object_prep(&forge, lookup_kind(TV_RUNE, SV_RUNE));
		rune_add(&forge, RUNE_ELEMENTAL_PROTECTION, FALSE);
		drop_near(&forge, -1, py, px);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_sacrifice_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Sacrifice");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Sacrifice on an artifact. You can now destroy (with 'k' command) the artifact, and if you do so, you restore all HP and SP.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_artifact);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_SACRIFICE, TRUE));

		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_reflection_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Reflection");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Reflection on your shield. Your shield gains the Reflection property.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(object_is_shield);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_REFLECTION, TRUE));

		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static void _rune_of_good_fortune_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Good Fortune");
		break;
	case SPELL_DESC:
		var_set_string(res, "Creates a standalone rune. As long as you have this rune in your inventory, you gain the effects of White Aura mutation.");
		break;
	case SPELL_CAST:
	{
		object_type forge;

		object_prep(&forge, lookup_kind(TV_RUNE, SV_RUNE));
		rune_add(&forge, RUNE_GOOD_FORTUNE, FALSE);
		drop_near(&forge, -1, py, px);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static bool _body_test(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_DRAG_ARMOR:
	case TV_SHIELD:
		return TRUE;
		break;
	}
	return FALSE;
}

static void _rune_of_body_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Body");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Body on your body armor or shield. If you wear that armor or shield, you gain +4 Str, Dex, Con, and Resist Nexus.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(_body_test);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_BODY, TRUE));

		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

static bool _mind_test(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
	case TV_SOFT_ARMOR:
	case TV_HARD_ARMOR:
	case TV_DRAG_ARMOR:
	case TV_SHIELD:
		return TRUE;
		break;
	}
	return FALSE;
}

static void _rune_of_mind_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Rune of Mind");
		break;
	case SPELL_DESC:
		var_set_string(res, "Places a Rune of Mind on a body armor or a shield. If you wear that armor, you gain +4 Int, Telepathy, Resist Confusion, Resist Chaos, and Resist Eldritch Horror.");
		break;
	case SPELL_CAST:
	{
		object_type *o_ptr = _rune_object_prompt(_mind_test);
		var_set_bool(res, FALSE);

		if (o_ptr)
			var_set_bool(res, rune_add(o_ptr, RUNE_MIND, TRUE));

		break;
	}
	default:
		_rune_default_spell(cmd, res);
		break;
	}
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
 #define _MAX_SPELLS_PER_GROUP	15
 #define _MAX_SPELL_GROUPS       4

typedef struct {
	cptr name;
	cptr help;
	int color;
	spell_info spells[_MAX_SPELLS_PER_GROUP];	/* There is always a sentinel at the end */
} _spell_group;

static _spell_group _spell_groups[_MAX_SPELL_GROUPS] = {
	{ "Runes of Creation",
	  "Augment your equipment by attaching runes of various powers.  Also, you may create "
	  "certain stand alone runes that grant powers by virtue of being present in your "
	  "inventory.  Be sure to always keep Absorption handy, for it is your only means "
	  "of regaining spell points!",
	  TERM_L_BLUE,
	  { {  1,   0, 20, _rune_of_absorption_spell },
		{  5,   0, 30, _rune_of_regeneration_spell },
		{ 10,   0, 30, _rune_of_deflection_spell },
		{ 15,   0, 30, _rune_of_stasis_spell },
		{ 20,   0, 40, _rune_of_destruction_spell },
		{ 25,   0, 40, _rune_of_elemental_protection_spell },
		{ 30,   0, 50, _rune_of_sacrifice_spell },
		{ 35,   0, 60, _rune_of_reflection_spell },
		{ 40,   0, 60, _rune_of_good_fortune_spell },
		{ 45,   0, 70, _rune_of_body_spell },
		{ 50,   0, 70, _rune_of_mind_spell },
		{ -1,   0,  0, NULL },
	  }
	},
	{ "Runes of the Novice",
	  "Minor spells and powers, available to the weakest of Rune-Knights.  "
	  "While hardly awe-inspiring, these powers grant minor offense and weak "
	  "utility that are designed to assist the novice in their quest for deeper "
	  "understanding.",
	  TERM_L_GREEN,
	  { {  1,   1, 20, magic_missile_spell },
	    {  1,   2, 25, phase_door_spell },
		{  3,   3, 25, detect_doors_stairs_traps_spell },
		{  5,   5, 35, light_area_spell },
		{  7,  10, 75, resist_poison_spell },
		{  9,   7, 75, magic_mapping_spell },
		{ 11,   9, 35, summon_manes_spell },
		{ 12,  12, 40, orb_of_entropy_spell },
		{ 15,   9, 35, teleport_spell },
		{ -1,   0,  0, NULL },
	  }
	},
	{ "Runes of the Initiate",
	  "Stronger rune powers, available to the experienced Rune-Knight.  "
	  "These powers offer great utility to assist you on your journey "
	  "of knowledge.",
	  TERM_UMBER,
	  { { 16,  16, 45, remove_curse_I_spell },
	    { 18,  12, 60, teleport_other_spell },
		{ 20,  20, 85, resistance_spell },
		{ 21,  20, 80, explosive_rune_spell },
        { 22,  16, 60, stone_to_mud_spell },
        { 25,  25, 85, disintegrate_spell },
        { 28,  20, 70, satisfy_hunger_spell },
        { 29,  23, 60, protection_from_evil_spell },
        { 30,  25, 75, battle_frenzy_spell },
	    { -1,   0,  0, NULL },
	  }
	},
	{ "Runes of the Master",
	  "Mighty powers indeed.  Use them wisely, for the forces of evil have "
	  "grown strong and don't take well to rivals in their quest for domination.",
	  TERM_RED,
	  { { 33,  30, 75, identify_fully_spell },
	    { 35,  33, 45, dimension_door_spell },
        { 36,  70, 75, glyph_of_warding_spell },
		{ 38,  65, 85, mana_branding_spell },
		{ 40,  40, 80, eye_for_an_eye_spell },
		{ 42, 100, 80, clairvoyance_spell },
		{ 43, 100, 45, living_trump_spell },
		{ 45,  58, 85, mana_storm_II_spell },
		{ 47, 100, 90, wraithform_spell },
		{ 49,  80, 85, polymorph_demonlord_spell },
	    { -1,   0,  0, NULL },
	  }
	},
};

static cptr _spell_group_name(menu_choices choices, int which) {
	return _spell_groups[which].name;
}

static cptr _spell_group_help(menu_choices choices, int which) {
	return _spell_groups[which].help;
}

static int _spell_group_color(menu_choices choices, int which) {
	return _spell_groups[which].color;
}

static int _get_spells_imp(spell_info* spells, int max, _spell_group *spell_group)
{
	int i;
	int ct = 0;
	int stat_idx = p_ptr->stat_ind[A_INT];
	
	for (i = 0; ; i++)
	{
		spell_info *base = &spell_group->spells[i];
		if (base->level < 0) break;
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

static int _get_spells(spell_info* spells, int max)
{
	int idx = -1;
	int ct = 0;
	menu_list_t list = { "Use which group of spells?", "Browse which group of spells?", NULL,
						_spell_group_name, _spell_group_help, _spell_group_color, 
						_spell_groups, _MAX_SPELL_GROUPS};

	idx = menu_choose(&list);
	if (idx < 0) return 0;
	ct = _get_spells_imp(spells, max, &_spell_groups[idx]);
	if (ct == 0)
		msg_print("You don't know any of those spells yet!");
	return ct;
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "spell";
		me.use_sp = TRUE;
		init = TRUE;
	}
	return &me;
}

class_t *rune_knight_get_class_t(void)
{
	static class_t me = {0};
	static bool init = FALSE;

	/* static info never changes */
	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
	skills_t bs = { 30,  35,  36,   2,  18,  16,  50,  25};
	skills_t xs = {  7,  10,  10,   0,   0,   0,  15,  11};

		me.name = "Rune-Knight";
		me.desc = "The Rune Knight is a mythical warrior-mage who is dedicated to the power "
				  "of ancient Runes that hold immense power.  By affixing these Runes to his "
				  "equipment, the Rune Knight can become an avatar of destruction, or an "
				  "invulnerable bastion.  Unlike the Warrior-Mage and all other casters, the "
				  "Rune Knight's mana does not regenerate on its own; rather, the Rune Knight "
				  "must siphon mana from magical attacks directed at him.  The Rune Knight has "
				  "a fixed (though very large) selection of spells that he can use his mana on, "
				  "in addition to his unique Rune spells.";

		me.stats[A_STR] = 2;
		me.stats[A_INT] = 2;
		me.stats[A_WIS] = 0;
		me.stats[A_DEX] = 1;
		me.stats[A_CON] = 0;
		me.stats[A_CHR] = 1;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.caster_info = _caster_info;
		me.get_spells = _get_spells;
		init = TRUE;
	}

	return &me;
}