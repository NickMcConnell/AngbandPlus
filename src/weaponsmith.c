#include "angband.h"

/* TODO: do_cmd_kaji should be moved here ... */

void _judgment_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Judgment");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        if (p_ptr->lev > 29)
            var_set_bool(res, identify_fully(object_is_weapon_armour_ammo));
        else
            var_set_bool(res, ident_spell(object_is_weapon_armour_ammo));
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
    spell->level = 5;
    spell->cost = 15;
    spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_INT]);
    spell->fn = _judgment_spell;

    return ct;
}

class_t *weaponsmith_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  28,  28,   1,  20,  10,  60,  45};
    skills_t xs = { 10,  10,  10,   0,   0,   0,  21,  15};

        me.name = "Weaponsmith";
        me.desc = "A Weaponsmith can improve weapons and armors for him or herself. "
                    "They are good at fighting, and they have potential ability to "
                    "become even better than Warriors using improved equipments. They "
                    "cannot cast spells, and are poor at skills such as stealth or "
                    "magic defense.\n \n"
                    "A Weaponsmith extracts the essences of special effects from weapons "
                    "or armors which have various special abilities, and can add these "
                    "essences to another weapon or armor. Normally, each equipment can "
                    "be improved only once, but they can remove a previously added "
                    "essence from improved equipment to improve it with another "
                    "essence. To-hit, to-damage bonus, and AC can be improved freely "
                    "up to a maximum value depending on level. Weaponsmiths have a "
                    "class power - 'Judgment' - which allows them to identify (later "
                    "*identify*) weapons and armor.";

        me.stats[A_STR] =  3;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  0;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 111;
        me.base_hp = 12;
        me.exp = 130;
        me.pets = 40;
        
        me.get_powers = _get_powers;
        init = TRUE;
    }

    return &me;
}

