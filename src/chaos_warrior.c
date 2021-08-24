#include "angband.h"
#include "equip.h"

static void _calc_bonuses(void)
{
    if (p_ptr->lev >= 30) 
        res_add(RES_CHAOS);
    if (p_ptr->lev >= 40) 
        res_add(RES_FEAR);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (p_ptr->lev >= 30)
        add_flag(flgs, OF_RES_CHAOS);
    if (p_ptr->lev >= 40)
        add_flag(flgs, OF_RES_FEAR);
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 40;
    spell->cost = 50;
    spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_INT]);
    spell->fn = confusing_lights_spell;

    return ct;
}

static void _gain_level(int new_level)
{
	/* Chaos worshippers can expect a certain number of rewards and mutations levelling up */
	/* These are rarely detrimental, patrons are liable to inflict punishment at other times */
	if (new_level > 1)
	{
		chaos_patron_reward(PATRON_LEVEL_UP);
	}
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 450;
        me.encumbrance.weapon_pct = 20;
        me.encumbrance.enc_wgt = 1200;
        me.min_fail = 5;
        me.min_level = 2;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_SWORD, SV_BROAD_SWORD, 1);
    py_birth_obj_aux(TV_HARD_ARMOR, SV_METAL_SCALE_MAIL, 1);
    py_birth_spellbooks();
}

class_t *chaos_warrior_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  25,  34,   1,  14,  12,  65,  40};
    skills_t xs = {  7,  11,  10,   0,   0,   0,  20,  17};

        me.name = "Chaos-Warrior";
        me.desc = "Chaos Warriors are the feared servants of the terrible Demon Lords "
                    "of Chaos. Every Chaos Warrior has a Patron Demon and, when "
                    "gaining a level, may receive a reward from his Patron. He might "
                    "be healed or polymorphed, his stats could be increased, or he "
                    "might be rewarded with an awesome weapon. On the other hand, the "
                    "Patrons might surround him with monsters, drain his stats or wreck "
                    "his equipment or they might simply ignore him. The Demon Lords of "
                    "Chaos are chaotic and unpredictable indeed. The exact type of "
                    "reward depends on both the Patron Demon (different Demons give "
                    "different rewards) and chance.\n \n"
                    "Chaos Warriors can select a realm from Chaos and Daemon. They are "
                    "not interested in any other form of magic. They can learn every "
                    "spell. They have a class power - 'Confusing Light' - which stuns, "
                    "confuses, and scares all monsters in sight.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  0;
        me.stats[A_CON] =  2;
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 111;
        me.base_hp = 12;
        me.exp = 125;
        me.pets = 40;
        
        me.birth = _birth;
        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.caster_info = _caster_info;
        me.get_powers = _get_powers;
		/* level gain in xtra2.c */
        //me.gain_level = _gain_level;
		/* Chaos worshippers can expect a certain number of rewards and mutations levelling up */
		/* These are rarely detrimental, patrons are liable to inflict punishment at other times */
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
