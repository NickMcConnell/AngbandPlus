#include "angband.h"

/************************************************************************
 * Priest
 ************************************************************************/
static int _priest_get_powers(spell_info* spells, int max)
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

static void _priest_calc_bonuses(void)
{
    if (plr->realm1 == REALM_NATURE && plr->lev >= 15)
        plr->clear_mind = TRUE;
}

static caster_info * _priest_caster_info(void)
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

static void _priest_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
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
    if (plr->pclass != CLASS_PRIEST && plr->pclass != CLASS_HIGH_PRIEST)
        return FALSE;
    return is_good_realm(plr->realm1);
}

bool priest_is_evil(void)
{
    if (plr->pclass != CLASS_PRIEST && plr->pclass != CLASS_HIGH_PRIEST)
        return FALSE;
    return is_evil_realm(plr->realm1);
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
        me->hooks.caster_info = _priest_caster_info;
        me->hooks.get_powers = _priest_get_powers;
        me->hooks.calc_bonuses = _priest_calc_bonuses;
        me->hooks.calc_weapon_bonuses = _priest_calc_weapon_bonuses;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}

/************************************************************************
 * High-Priest
 ************************************************************************/
static int _high_priest_get_powers(spell_info* spells, int max)
{
    spell_info *spell;
    int ct = 0;

    switch (plr->realm1)
    {
    case REALM_LIFE:
    case REALM_CRUSADE:
        spell = &spells[ct++];
        spell->level = 35;
        spell->cost = 70;
        spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_WIS]);
        spell->fn = bless_weapon_spell;
        break;
    case REALM_NATURE:
        /* Ommmmmm ... Focus on the surrounding world to regain spiritual clarity */
        spell = &spells[ct++];
        spell->level = 15;
        spell->cost = 0;
        spell->fail = calculate_fail_rate(spell->level, 30, plr->stat_ind[A_WIS]);
        spell->fn = clear_mind_spell;
        break;
    case REALM_DEATH:
        /* nothing for now */
        break;
    case REALM_DAEMON:
        spell = &spells[ct++];
        spell->level = 25;
        spell->cost = 1;
        spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_WIS]);
        spell->fn = eat_magic_spell;
        break;
    case REALM_HEX:
        spell = &spells[ct++];
        spell->level = 1;
        spell->cost = 0;
        spell->fail = 0;
        spell->fn = hex_stop_spell;
        break;
    case REALM_BLESS:
        spell = &spells[ct++];
        spell->level = 1;
        spell->cost = 0;
        spell->fail = 0;
        spell->fn = bless_stop_spell;
        break;
    }
    return ct;
}
static void _high_priest_calc_bonuses(void)
{
    plr->spell_cap += 3;
    switch (plr->realm1)
    {
    case REALM_LIFE:
        plr->life += 5;
        plr->regen += 2*plr->lev;
        if (plr->lev > 30)
            plr->hold_life++;
        break;
    case REALM_CRUSADE:
        plr->pspeed += 2;
        if (plr->lev >= 30)
            res_add(GF_FEAR);
        break;
    case REALM_NATURE:
        if (plr->lev >= 15)
            res_add(GF_FIRE);
        if (plr->lev >= 20)
            res_add(GF_COLD);
        if (plr->lev >= 15)
            plr->clear_mind = TRUE;
        break;
    case REALM_DEATH:
        /* cf _max_vampiric_drain in plr_attack.c */
        if (plr->lev >= 30)
            res_add(GF_NETHER);
        plr->see_nocto = MAX(plr->see_nocto, 2 + plr->lev/13);
        break;
    case REALM_DAEMON:
        plr->skills.dev += 10;
        plr->align -= 50;
        plr->skills.thn += 20;
        if (plr->lev >= 30)
            res_add(GF_PLASMA);
        break;
    case REALM_HEX:
        hex_calc_bonuses();
        break;
    case REALM_BLESS:
        bless_calc_bonuses();
        break;
    }
}
static void _high_priest_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPELL_CAP);
    switch (plr->realm1)
    {
    case REALM_LIFE:
        add_flag(flgs, OF_LIFE);
        add_flag(flgs, OF_REGEN);
        if (plr->lev > 30)
            add_flag(flgs, OF_HOLD_LIFE);
        break;
    case REALM_CRUSADE:
        add_flag(flgs, OF_SPEED);
        if (plr->lev >= 30)
            add_flag(flgs, OF_RES_(GF_FEAR));
        break;
    case REALM_NATURE:
        if (plr->lev >= 15)
            add_flag(flgs, OF_RES_(GF_FIRE));
        if (plr->lev >= 20)
            add_flag(flgs, OF_RES_(GF_COLD));
        break;
    case REALM_DEATH:
        if (plr->lev >= 30)
            add_flag(flgs, OF_RES_(GF_NETHER));
        break;
    case REALM_DAEMON:
        if (plr->lev >= 30)
            add_flag(flgs, OF_RES_(GF_PLASMA));
        break;
    }
}
static void _high_priest_calc_stats(s16b stats[MAX_STATS])
{
    switch (plr->realm1)
    {
    case REALM_CRUSADE:
    case REALM_DAEMON:
        stats[A_STR] += 2;
        break;
    }
}
static void _high_priest_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    _priest_calc_weapon_bonuses(obj, info);
    if (plr->realm1 == REALM_HEX)
        hex_calc_weapon_bonuses(obj, info);
    if (plr->realm1 == REALM_BLESS)
        bless_calc_weapon_bonuses(obj, info);
}
static caster_info * _high_priest_caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "prayer";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 67;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_ALLOW_DEC_MANA;
        me.realm1_choices = CH_LIFE | CH_DEATH | CH_DAEMON | CH_CRUSADE | CH_NATURE
            | CH_HEX | CH_BLESS;
        init = TRUE;
    }
    return &me;
}
static void _high_priest_timer_on(plr_tim_ptr timer)
{
    if (plr->realm1 == REALM_HEX && hex_count())
    {
        switch (timer->id)
        {
        case T_CONFUSED:
        case T_PARALYZED:
            hex_stop();
            break;
        case T_STUN: /* cf _stun_add */
            if (_1d(STUN_KNOCKED_OUT) <= timer->count)
                hex_stop();
            break;
        }
    }
    else if (plr->realm1 == REALM_BLESS && bless_count())
    {
        switch (timer->id)
        {
        case T_CONFUSED:
        case T_PARALYZED:
            bless_stop();
            break;
        case T_STUN: /* cf _stun_add */
            if (_1d(STUN_KNOCKED_OUT) <= timer->count)
                bless_stop();
            break;
        }
    }
}
plr_class_ptr high_priest_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  40,  40,   2,  16,   8,  40,  30};
    skills_t xs = { 35,  60,  60,   0,   0,   0,  60,  50};

        me = plr_class_alloc(CLASS_HIGH_PRIEST);
        me->name = "High-Priest";
        me->desc = "Similar to the Priest, the High-Priest is a character devoted to serving "
                   "a higher power. However, the High-Priest specializes in a single realm of "
                   "prayer in order to gain awesome bonuses in casting costs and fail rates. "
                   "Service to the good involes choosing Life, Crusade or Benediction magic, and these choices "
                   "restrict the player to using blunt weapons (unless otherwise 'blessed'). "
                   "Service to evil involves choosing one of Death, Daemon or Malediction, and the evil "
                   "priest is under no compunction concerning the spilling of blood. In fact, they "
                   "revel in it! Finally, a High-Priest may serve Nature, a realm of balance. In "
                   "this case, sharp weapons are prohibited as they are kin to the much hated axe, "
                   "feller of trees. Each realm choice will grant additional abilities, skills or "
                   "powers. Of course, wisdom is the primary stat.\n\n"
                   "Benediction and Malediction magic are quite unique and involve spells that are "
                   "chanted continuously to maintain an effect. Like the Bard, the priest vocalizes spells in "
                   "a continuous fashion granting abilities as long as the chant is maintained. "
                   "But unlike the Bard, the priest may evoke multiple chants at once, weaving "
                   "the rhythms in an intricate display of musical prowess.";

        me->stats[A_STR] = -2;
        me->stats[A_INT] = -3;
        me->stats[A_WIS] =  4;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 98;
        me->base_hp = 4;
        me->exp = 135;
        me->pets = 35;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.caster_info = _high_priest_caster_info;
        me->hooks.get_powers = _high_priest_get_powers;
        me->hooks.calc_bonuses = _high_priest_calc_bonuses;
        me->hooks.calc_stats = _high_priest_calc_stats;
        me->hooks.calc_weapon_bonuses = _high_priest_calc_weapon_bonuses;
        me->hooks.get_flags = _high_priest_get_flags;
        me->hooks.timer_on = _high_priest_timer_on;
        me->hooks.character_dump = spellbook_character_dump;
    }

    return me;
}
