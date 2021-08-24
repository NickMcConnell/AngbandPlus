#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
	int ct = 0;

	spell_info* spell = &spells[ct++];
	spell->level = 25;
	spell->cost = 1;
	spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_INT]);
	spell->fn = eat_magic_spell;

	return ct;
}

static caster_info * _caster_info(void)
{
	static caster_info me = { 0 };
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "spell";
		me.which_stat = A_INT;
		me.encumbrance.max_wgt = 430;
		me.encumbrance.weapon_pct = 100;
		me.encumbrance.enc_wgt = 700;
		me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
		init = TRUE;
	}
	return &me;
}

static void _birth(void)
{
	py_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
	py_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
	py_birth_spellbooks();
}

static void _gain_level(int new_level)
{

	if (new_level > 1)
	{
		chaos_patron_reward(PATRON_LEVEL_UP);
	}
}

class_t *chaos_mage_get_class(void)
{
	static class_t me = { 0 };
	static bool init = FALSE;

	if (!init)
	{           /* dis, dev, sav, stl, srh, fos, thn, thb */
		skills_t bs = { 28,  40,  38,   3,  15,  20,  35,  20 };
		skills_t xs = { 7,  15,  11,   0,   0,   0,   6,   7 };

		me.name = "Chaos-Mage";
		me.desc = "Chaos Mages are the arcane students of the terrible Demon Lords "
			"of Chaos. Every Chaos Mage has a Patron Demon and, when "
			"gaining a level, may receive a reward from his Patron. He might "
			"be healed or polymorphed, his stats could be increased, or he "
			"might be rewarded with an awesome weapon. On the other hand, the "
			"Patrons might surround him with monsters, drain his stats or wreck "
			"his equipment or they might simply ignore him. The Demon Lords of "
			"Chaos are chaotic and unpredictable indeed. The exact type of "
			"reward depends on both the Patron Demon (different Demons give "
			"different rewards) and chance.\n \n"
			"Chaos Mages can select a realm from Chaos and Daemon, and one "
			"other realm excluding Life and Crusade. A  Chaos Mage's prime statistic is "
			"Intelligence as this determines his spell casting ability.\n \n"
			"Chaos Mages are a little hardier than ordinary mages but less perceptive. "
			"They have a class power - 'Eat Magic' - which absorbs mana "
			"from wands, staves or rods."; 

		me.stats[A_STR] = -3;
		me.stats[A_INT] = 3;
		me.stats[A_WIS] = -1;
		me.stats[A_DEX] = 0;
		me.stats[A_CON] = -1;
		me.stats[A_CHR] = -2;
		me.base_skills = bs;
		me.extra_skills = xs;
		me.life = 95;
		me.base_hp = 0;
		me.exp = 135;
		me.pets = 30;

		me.birth = _birth;
		me.caster_info = _caster_info;
		/* level gain in xtra2.c */
		//me.gain_level = _gain_level;
		/* Chaos worshippers can expect a certain number of rewards and mutations levelling up */
		/* These are rarely detrimental, patrons are liable to inflict punishment at other times */
		/* TODO: This class uses spell books, so we are SOL
		me.get_spells = _get_spells;*/
		me.character_dump = spellbook_character_dump;
		me.get_powers = _get_powers;
		init = TRUE;
	}

	return &me;
}
