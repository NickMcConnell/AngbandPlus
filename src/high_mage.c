#include "angband.h"

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;
    spell_info* spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 1;
    spell->fail = calculate_fail_rate(spell->level, 90, plr->stat_ind[A_INT]);
    spell->fn = eat_magic_spell;
    return ct;
}

static void _calc_bonuses(void)
{
    plr->spell_cap += 3;
    plr->to_d_spell += 5 + plr->lev/5;
/*  plr->spell_power += 2; 
    plr->device_power += 2; */
    if (plr->lev >= 25)
        plr->wizard_sight = TRUE;
}
static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    add_flag(flgs, OF_SPELL_CAP);
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        me.realm1_choices = CH_LIFE | CH_SORCERY | CH_NATURE | CH_CHAOS | CH_DEATH |
            CH_TRUMP | CH_ARCANE | CH_ENCHANT | CH_DAEMON | CH_CRUSADE |
            CH_ARMAGEDDON | CH_ILLUSION;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_DAGGER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_ROBE, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_CLARITY, rand_range(10, 20));
    plr_birth_obj_aux(TV_WAND, EFFECT_BOLT_MISSILE, 1);
    plr_birth_spellbooks();
}

/* XXX future: scan each "plr->realm->spells" for SF_AUTO_ID or SF_AUTO_DETECT */
#define SORCERY_DETECT_TRAPS 2
#define SORCERY_IDENTIFY 9
/* XXX in progress ... I'm adding as I playtest */
static bool _auto_id_aux(obj_ptr obj, int realm, int spell)
{
    int cost = plr_can_auto_cast(realm, spell);
    if (cost)
    {
        /* plr_auto_cast would prompt for the object via do_spell(SPELL_CAST)
         * do it by hand */
        sp_player(-cost);
        identify_item(obj);
        spell_stats_on_cast_old(realm, spell);
        return TRUE;
    }
    return FALSE;
}
extern bool mage_auto_id(obj_ptr obj);
extern bool mage_auto_detect(void);
bool mage_auto_id(obj_ptr obj)
{
    return _auto_id_aux(obj, REALM_SORCERY, SORCERY_IDENTIFY);
}
bool mage_auto_detect(void)
{
    return plr_auto_cast(REALM_SORCERY, SORCERY_DETECT_TRAPS);
}
plr_class_ptr high_mage_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  40,  38,   3,  16,  20,  34,  20};
    skills_t xs = { 35,  75,  55,   0,   0,   0,  30,  35};

        me = plr_class_alloc(CLASS_HIGH_MAGE);
        me->name = "High-Mage";
        me->desc = "High-mages are mages who specialize in one particular field of "
                    "magic and learn it very well - much better than the ordinary mage. "
                    "A high mage's prime statistic is intelligence as this determines "
                    "his spell casting ability.\n \n"
                    "For the price of giving up a second realm of magic, High-mages "
                    "gain substantial benefits in the mana costs, power, minimum levels, and "
                    "failure rates of the spells in their speciality realm. They have "
                    "a class power - 'Eat Magic' - which absorbs mana from wands, "
                    "staves, or rods.";

        me->stats[A_STR] = -4;
        me->stats[A_INT] =  4;
        me->stats[A_WIS] =  0;
        me->stats[A_DEX] =  0;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 94;
        me->base_hp = 0;
        me->exp = 130;
        me->pets = 25;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG | CLASS_MAGE_BONUS;
        
        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.auto_id = mage_auto_id;
        me->hooks.auto_detect = mage_auto_detect;
    }
    return me;
}
