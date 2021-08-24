#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
	int ct = 0;
	spell_info* spell = &spells[ct++];
	spell->level = 1;
	spell->cost = 0;
	spell->fail = 0;
	spell->fn = hex_stop_spelling_spell;
	return ct;
}

static caster_info * _caster_info(void)
{
	static caster_info me = { 0 };
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "spell";
		me.which_stat = A_CHR;
		me.encumbrance.max_wgt = 450;
		me.encumbrance.weapon_pct = 20;
		me.encumbrance.enc_wgt = 1200;
		me.options = CASTER_ALLOW_DEC_MANA;
		init = TRUE;
	}
	return &me;
}

static void _birth(void)
{
	py_birth_obj_aux(TV_SWORD, SV_LONG_SWORD, 1);
	py_birth_obj_aux(TV_HARD_ARMOR, SV_RING_MAIL, 1);
	py_birth_spellbooks();
}

class_t *hexblade_get_class(void)
{
	static class_t me = { 0 };
	static bool init = FALSE;

	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
		skills_t bs = { 30,  35,  36,   2,  18,  16,  50,  50 };
		skills_t xs = { 7,  10,  10,   0,   0,   0,  15,  15 };

		me.name = "Hexblade";
		me.desc = "A Hexblade is an evil fighter who employs hex magic.";

		me.stats[A_STR] = 2;
		me.stats[A_INT] = 1;
		me.stats[A_WIS] = 0;
		me.stats[A_DEX] = 1;
		me.stats[A_CON] = 0;
		me.stats[A_CHR] = 2;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.life = 106;
		me.base_hp = 8;
		me.exp = 140;
		me.pets = 35;

		me.birth = _birth;
		me.caster_info = _caster_info;
		/* TODO: This class uses spell books, so we are SOL
		me.get_spells = _get_spells;*/
		me.get_powers = _get_powers;
		me.character_dump = spellbook_character_dump;
		init = TRUE;
	}

	return &me;
}
