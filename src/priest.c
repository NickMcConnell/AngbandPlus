#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;
    spell_info* spell = &spells[ct++];

    if (plr->realm1 == REALM_NATURE)
    {
        /* Ommmmmm ... Focus on the surrounding world to regain spiritual clarity */
        spell->level = 15;
        spell->cost = 0;
        spell->fail = calculate_fail_rate(spell->level, 30, plr->stat_ind[A_WIS]);
        spell->fn = clear_mind_spell;
    }
    else if (is_good_realm(plr->realm1))
    {
        spell->level = 35;
        spell->cost = 70;
        spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_WIS]);
        spell->fn = bless_weapon_spell;
    }
    else
    {
        spell->level = 42;
        spell->cost = 40;
        spell->fail = calculate_fail_rate(spell->level, 80, plr->stat_ind[A_WIS]);
        spell->fn = evocation_spell;
    }

    return ct;
}

static void _calc_bonuses(void)
{
    if (plr->realm1 == REALM_NATURE && plr->lev >= 15)
        plr->clear_mind = TRUE;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "prayer";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 67;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_ALLOW_DEC_MANA;
        me.realm1_choices = CH_LIFE | CH_DEATH | CH_DAEMON | CH_CRUSADE | CH_NATURE;
        me.realm2_choices = CH_LIFE | CH_SORCERY | CH_CHAOS | CH_DEATH | CH_TRUMP |
            CH_ARCANE | CH_ENCHANT | CH_DAEMON | CH_CRUSADE | CH_ARMAGEDDON;
        init = TRUE;
    }
    return &me;
}

static void _calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if ((obj->tval == TV_SWORD || obj->tval == TV_POLEARM) && !is_evil_realm(plr->realm1))
    {
        bool icky = TRUE;

        if (plr->realm1 == REALM_NATURE)
        {
            icky = obj->name2 != EGO_WEAPON_NATURE;
        }
        else
        {
            icky = !obj_has_flag(obj, OF_BLESSED);
        }
        if (icky)
        {
            info->to_h -= 2;
            info->dis_to_h -= 2;

            info->to_d -= 2;
            info->dis_to_d -= 2;

            add_flag(info->paf_flags, PAF_ICKY);
        }
    }
}

bool priest_is_good(void)
{
    if (plr->pclass == CLASS_PRIEST && is_good_realm(plr->realm1))
        return TRUE;
    return FALSE;
}

bool priest_is_evil(void)
{
    if (plr->pclass == CLASS_PRIEST && is_evil_realm(plr->realm1))
        return TRUE;
    return FALSE;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_HAFTED, SV_MACE, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_HEALING, 1);
    plr_birth_spellbooks();
}

plr_class_ptr priest_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  35,  40,   2,  16,   8,  48,  35};
    skills_t xs = { 35,  55,  60,   0,   0,   0,  65,  55};

        me = plr_class_alloc(CLASS_PRIEST);
        me->name = "Priest";
        me->desc = "A Priest is a character devoted to serving a higher power. They "
                    "explore the dungeon in the service of their God. They are fairly "
                    "familiar with magical devices which they believe act as foci for divine "
                    "intervention in the natural order of things.\n \n"
                    "There are three types of priests. If the priest chooses Life "
                    "or Crusade as their first realm, then they will follow the path of good. "
                    "As such, they may not choose an evil realm for their second realm. Also, good "
                    "priests abhor bloodshed, and therefore are not comfortable with edged weapons. "
                    "Wielding one will disrupt their ability to concentrate during prayers. "
                    "Should a priest choose Death or Daemon as their first realm, however, then "
                    "they will serve an evil god. As such, they actually enjoy shedding blood, and "
                    "suffer no such weapon restriction. Of course, evil priests abhor good things, "
                    "and are unable to choose Life or Crusade for their second realm. "
                    "Finally, priests may serve Nature as a druid. Like good priests, they too "
                    "abhor bloodshed. For their second realm, they may choose neither good nor evil prayers.\n \n"
                    "Good priests have a strong affinity for Life prayers, and learn them very well, even better "
                    "than a High Mage. Conversely, evil priests favor prayers of Death, and receive "
                    "strong bonuses when choosing this foul realm. Otherwise, priests learn spells less "
                    "efficiently than a Mage would. Also, they may not choose which spell to learn, but "
                    "are granted new prayers by the whim of their deity, presumably in order to serve "
                    "some greater divine plan of which the priest is not fully cognizant. The priest's "
                    "primary prayer stat is Wisdom.";

        me->stats[A_STR] = -1;
        me->stats[A_INT] = -3;
        me->stats[A_WIS] =  3;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] =  0;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 120;
        me->pets = 35;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_weapon_bonuses = _calc_weapon_bonuses;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
