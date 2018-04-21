#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;
    spell_info* spell = &spells[ct++];

    if (is_good_realm(p_ptr->realm1))
    {
        spell->level = 35;
        spell->cost = 70;
        spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_WIS]);
        spell->fn = bless_weapon_spell;
    }
    else
    {
        spell->level = 42;
        spell->cost = 40;
        spell->fail = calculate_fail_rate(spell->level, 80, p_ptr->stat_ind[A_WIS]);
        spell->fn = evocation_spell;
    }

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "prayer";
        me.which_stat = A_WIS;
        me.weight = 430;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

class_t *priest_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  35,  40,   2,  16,   8,  48,  35};
    skills_t xs = {  7,  11,  12,   0,   0,   0,  13,  11};

        me.name = "Priest";
        me.desc = "A Priest is a character devoted to serving a higher power. They "
                    "explore the dungeon in the service of their God. They are fairly "
                    "familiar with magical devices which they believe act as foci for divine "
                    "intervention in the natural order of things. Priests abhor bloodshed, "
                    "and therefore are not comfortable with edged weapons. Wielding one "
                    "will disrupt their ability to concentrate during prayers. "
                    "A Priest's primary stat is Wisdom.\n \n"
                    "Priests can select from Life, Death, Daemon, or Crusade as a first "
                    "realm, and choose almost any second realm. However, if they choose "
                    "a good realm (Life or Crusade) for their first realm, then they are "
                    "precluded from choosing an evil realm (Death or Daemon) for their "
                    "second realm, and vice versa. "
                    "Priests can learn all spells in the selected realms, even if not "
                    "as efficiently as mages. However, when learning spells, priests "
                    "cannot voluntarily decide which spells to study: they are rewarded "
                    "with new prayers by their patron deities, with no money-back "
                    "satisfaction guarantee. Priests that select a good realm "
                    "have a class power - 'Bless Weapon' - which allows "
                    "the priest to wield an edged weapon without penalty. "
                    "Priests that select an Evil Realm have a class power "
                    "- 'Evocation' - which damages, scares and banish all monsters in "
                    "sight.";

        me.stats[A_STR] = -1;
        me.stats[A_INT] = -3;
        me.stats[A_WIS] =  3;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] =  0;
        me.stats[A_CHR] =  2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 100;
        me.base_hp = 4;
        me.exp = 120;
        me.pets = 35;
        
        me.caster_info = _caster_info;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
