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
        me.options = CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_SWORD, SV_LONG_SWORD, 1);
    py_birth_obj_aux(TV_HARD_ARMOR, SV_CHAIN_MAIL, 1);
    py_birth_spellbooks();
    p_ptr->proficiency[PROF_SWORD] = WEAPON_EXP_BEGINNER;

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_SKILLED;
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
        me.desc = "Chaos-Warriors are the feared servants of the terrible Demon Lords "
                    "of Chaos. Every Chaos-Warrior has a patron demon, who may give him "
                    "a reward on level-up; the Chaos-Warrior may be healed or polymorphed, "
                    "have his stats increased, or be rewarded with an awesome weapon. "
                    "On the other hand, though, he might be severely punished or simply ignored by "
                    "the patron; the Demon Lords of Chaos are unpredictable indeed, although "
                    "rewards are thankfully more common than punishments. The exact reward "
                    "will not depend on anything the player does, and is up entirely to "
                    "random chance and the patron; each patron gives out different rewards "
                    "and punishments.\n \n"
                    "Chaos-Warriors select one spell realm, either Chaos or Daemon; they have "
                    "no interest in other forms of magic. They can learn every spell in "
                    "their chosen realm. At level 40 they gain the powerful ability to emit "
                    "confusing lights, with the potential to stun, scare and confuse every creature in sight.";

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
        me.flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_STRONG |
                   CLASS_SENSE2_STRONG;
        
        me.birth = _birth;
        me.calc_bonuses = _calc_bonuses;
        me.get_flags = _get_flags;
        me.caster_info = _caster_info;
        me.get_powers = _get_powers;
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
