#include "angband.h"
#include "monspell.h"

/* Tera-Hack: The old blue-mage spell system with semi-normal spell codes,
 * spell casting levels, mana costs, etc. is completely inapplicable in modern
 * versions (monster spells are now essentially open-ended, no longer exist as
 * flags and cannot be presented in a single neat table, so this data can't be
 * assigned for each of them individually in a sane manner). To get around
 * this, we piggyback on Possessor code by teaching all spells learned to the
 * monster race Sexy Swimsuit (which can never generate in the wild, since it
 * is itself a hack to accommodate Sexy Rags as a player-monster), and then
 * cast spells like a Sexy Swimsuit Possessor but with a secondary hack to
 * guesstimate reasonable spell powers and levels. */

static void _dump(doc_ptr doc)
{
    monster_race* r_ptr = &r_info[MON_SEXY_SWIMSUIT];
    int i;
    vec_ptr spells;
    if ((!r_ptr) || (!r_ptr->name) || (!r_ptr->spells)) return;
    spells = mon_spells_all(r_ptr->spells);
    blue_mage_update_parms(spells);
    doc_insert(doc, "<topic:Imitations>================================== <color:keypress>B</color>lue Magic =================================\n\n");

    doc_insert(doc, "<color:G><tab:27>Lvl Cost Fail Desc           Cast</color>\n");

    for (i = 0; i < vec_length(spells); i++)
    {
        mon_spell_ptr spell = vec_get(spells, i);
        mon_spell_doc(spell, doc);
        doc_printf(doc, "<tab:27>%3d%5d%4d%%", spell->prob, mon_spell_cost(spell, r_ptr), blue_mage_spell_fail_rate(spell));
        list_spell_info(doc, spell, r_ptr);
        doc_printf(doc, "<tab:56>%4d\n", spell->lore);
    }
    vec_free(spells);

    /*    for (i = 0; i < MST_COUNT; i++)
        {
            mon_spell_group_ptr _group;
            if (!r_ptr->spells->groups[i]) continue;
            _group = r_ptr->spells->groups[i];
            if (!_group->count) continue;
            for (j = 0; j < _group->count; j++)
            {
                mon_spell_ptr spell = &_group->spells[j];
                mon_spell_doc(spell, doc);
                doc_printf(doc, "<tab:27>%3d%5d%4d%%", spell->prob, mon_spell_cost(spell, r_ptr), blue_mage_spell_fail_rate(spell));
                list_spell_info(doc, spell, r_ptr);
                doc_printf(doc, "<tab:56>%4d\n", spell->lore);
            }
        }*/

    doc_printf(doc, "\n");
}

static void _double_revenge_spell(int cmd, variant* res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Revenge");
        break;
    case SPELL_DESC:
        var_set_string(res, "imitate a power at double the damage");
        break;
    case SPELL_CAST:
        double_revenge = TRUE;
        possessor_cast();
        double_revenge = FALSE;
        handle_stuff();
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
    spell->level = 30;
    spell->cost = 100;
    spell->fail = calculate_fail_rate(spell->level, 90, p_ptr->stat_ind[A_DEX]);
    spell->fn = _double_revenge_spell;

    return ct;
}

static caster_info* _caster_info(void)
{
    static caster_info me = { 0 };
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "power";
        me.which_stat = A_INT;
        me.encumbrance.max_wgt = 430;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 600;
        me.options = CASTER_ALLOW_DEC_MANA;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    py_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    py_birth_obj_aux(TV_SOFT_ARMOR, SV_CLOTH_ARMOR, 1);
    py_birth_obj_aux(TV_POTION, SV_POTION_SPEED, 3);

    p_ptr->proficiency[PROF_SWORD] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_SLING] = WEAPON_EXP_BEGINNER;

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_SKILLED;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_SKILLED;
    new_mane = FALSE;
    double_revenge = FALSE;
}

static void _save(savefile_ptr file)
{
    monster_race* r_ptr = &r_info[MON_SEXY_SWIMSUIT];
    if ((!r_ptr) || (!r_ptr->name) || (!r_ptr->spells))
    {
        savefile_write_u16b(file, 0xFFF7);
        return;
    }
    else
    {
        int i;
        savefile_write_u16b(file, MST_COUNT);
        for (i = 0; i < MST_COUNT; i++)
        {
            if (!mon_race_has_spell_type(r_ptr, i))
            {
                savefile_write_byte(file, 0);
            }
            else
            {
                int j;
                mon_spell_group_ptr spells = r_ptr->spells->groups[i];
                savefile_write_byte(file, spells->count);
                for (j = 0; j < spells->count; j++)
                {
                    savefile_write_s16b(file, spells->spells[j].id.effect);
                    savefile_write_s16b(file, spells->spells[j].lore);
                    savefile_write_s16b(file, (s16b)blue_mage_spell_order(i, spells->spells[j].id.effect));
                }
            }
        }
    }
}

static void _load(savefile_ptr file)
{
    int i;
    u16b tmp16u = savefile_read_u16b(file);
    if (tmp16u == 0xFFF7) return;
    for (i = 0; i < (int)tmp16u; i++)
    {
        int j;
        byte count = savefile_read_byte(file);
        for (j = 0; j < count; j++)
        {
            s16b effect = savefile_read_s16b(file);
            s16b lore = savefile_read_s16b(file);
            s16b seniority = savefile_read_s16b(file);
            if ((i < MST_COUNT) && (!p_ptr->is_dead)) blue_mage_learn_spell_aux(i, effect, lore, seniority, FALSE);
        }
    }
    new_mane = FALSE;
}

class_t* imitator_get_class(void)
{
    static class_t me = { 0 };
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
        skills_t bs = { 25,  30,  36,   2,  18,  16,  60,  50 };
        skills_t xs = { 7,  10,  10,   0,   0,   0,  18,  20 };

        me.name = "Imitator";
        me.desc = "Imitators have enough fighting skills to survive, but rely on "
            "their unique technique - 'Imitation' - which imitates monster "
            "spells/techniques including their damage and duration. Dexterity "
            "determines general imitation ability, but a stat related to the "
            "specific action is often also taken into account.\n \n"
            "To use imitation, Imitators must see monster's spell at first. "
            "When a viewable monsters uses a spell, it is added to a temporary "
            "spell list which the imitator can choose from. Spells should be "
            "imitated quickly, because timing and situation are everything. An "
            "imitator can only repeat a spell once each time he observes it. "
            "They only have a small long-term memory for spells, which ranges "
            "from one to three, depending on their level. When they memorize "
            "more spells than this, they will forget the oldest spell in the "
            "list. They have a class power - 'Double Revenge' - which allows "
            "them to imitate spells at double damage or duration.";

        me.stats[A_STR] = 0;
        me.stats[A_INT] = 1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] = 2;
        me.stats[A_CON] = 0;
        me.stats[A_CHR] = -1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 108;
        me.base_hp = 10;
        me.exp = 110;
        me.pets = 20;
        me.flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_STRONG |
            CLASS_SENSE2_STRONG;

        me.birth = _birth;
        me.get_powers = _get_powers;
        me.caster_info = _caster_info;
        init = TRUE;
    }

    return &me;
}