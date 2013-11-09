#include "angband.h"

/* TODO: Redo spells ... */

void _learning_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Learning");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        if (p_ptr->action == ACTION_LEARN)
            set_action(ACTION_NONE);
        else
            set_action(ACTION_LEARN);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        var_set_int(res, 0);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = _learning_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "power";
        me.which_stat = A_INT;
        me.weight = 430;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

class_t *blue_mage_get_class_t(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  36,   3,  20,  16,  40,  25};
    skills_t xs = {  7,  16,  11,   0,   0,   0,   6,   7};

        me.name = "Blue-Mage";
        me.desc = "A Blue-Mage is a spell caster that must live by his wits, as he "
                    "cannot hope to simply hack his way through the dungeon like a "
                    "warrior. A major difference between the Mage and the Blue-Mage is "
                    "the method of learning spells: Blue-Mages may learn spells from "
                    "monsters. A Blue-Mage's prime statistic is Intelligence as this "
                    "determines his spell casting ability.\n \n"
                    "A Blue-Mage can learn and cast monster ranged attacks, spells, or "
                    "summons as their own spells; this technique is called Blue magic. "
                    "Unlike Imitators, Blue-Mages remember their spells permanently, "
                    "but they must get hit by a monster's spell while their class power "
                    "'Learning' is active to learn spells. Because of this "
                    "requirement, they do not learn spells, like healing, that affect "
                    "the monster itself.";

        me.stats[A_STR] = -4;
        me.stats[A_INT] =  4;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 100;
        me.base_hp = 4;
        me.exp = 130;
        me.pets = 35;
        
        me.caster_info = _caster_info;
        /*TODO: me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}
