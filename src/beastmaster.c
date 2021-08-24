#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 70, plr->stat_ind[A_CHR]);
    spell->fn = dominate_living_I_spell;

    spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 70, plr->stat_ind[A_CHR]);
    spell->fn = dominate_living_II_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_CHR;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.min_fail = 5;
        me.min_level = 3;
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        me.realm1_choices = CH_TRUMP;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_POLEARM, SV_SPEAR, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_spellbooks();
    plr_birth_pet("q.scrawny horse");
}

plr_class_ptr beastmaster_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  25,  32,   2,  18,  16,  52,  63};
    skills_t xs = { 35,  50,  50,   0,   0,   0,  70, 125};

        me = plr_class_alloc(CLASS_BEASTMASTER);
        me->name = "Beastmaster";
        me->desc = "Beastmasters are in tune with the minds of the creatures of the "
                    "world. They are very good at riding, and have enough "
                    "fighting ability. They use monsters which have been summoned or dominated "
                    "as their hands and feet. Beastmasters can cast trump magic, "
                    "and are very good at summoning spells, but they can not summon "
                    "non-living creatures. Charisma determines a Beastmaster's spell "
                    "casting ability.\n \n"
                    "Beastmasters use Trump magic to make good use of their monster "
                    "domination and riding abilities. They are very good at summoning "
                    "living creatures, and they learn summoning spells quicker than "
                    "Mages. However, they cannot summon non-living creatures. They "
                    "have two class powers - 'Dominate a Living Thing' and 'Dominate "
                    "Living Things'.";

        me->stats[A_STR] =  1;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] = -1;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] =  0;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 103;
        me->base_hp = 6;
        me->exp = 120;
        me->pets = 10;
        me->flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_WEAK;
        
        me->hooks.birth = _birth;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
