/* Display Monster Lore to the User
   Adapted from the ancient roff_aux() but redesigned for a more
   readable display; a 'Monster Sheet' if you will. */

#include "angband.h"

#include <stdlib.h>
#include <assert.h>

extern void mon_display(mon_race_ptr race);
extern void mon_display_rect(mon_race_ptr race, rect_t display);
extern void mon_display_doc(mon_race_ptr race, doc_ptr doc);
extern void mon_display_possessor(mon_race_ptr race, doc_ptr doc);
static bool _possessor_hack = FALSE;

static void _display_basic(mon_race_ptr race, doc_ptr doc);
static void _display_resists(mon_race_ptr race, doc_ptr doc);
static void _display_spells(mon_race_ptr race, doc_ptr doc);
static void _display_attacks(mon_race_ptr race, doc_ptr doc);
static void _display_other(mon_race_ptr race, doc_ptr doc);
static void _display_kills(mon_race_ptr race, doc_ptr doc);
static void _display_desc(mon_race_ptr race, doc_ptr doc);
/* XXX RFL_EVOLUTION shoud _display_evolution ... */

static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term);
static str_ptr _get_gf_name(int gf);

/**************************************************************************
 * Helpers
 **************************************************************************/
static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term)
{
    int ct = vec_length(v);
    int i;
    for (i = 0; i < ct; i++)
    {
        str_ptr s = vec_get(v, i);
        if (i < ct - 1 && sep)
            doc_printf(doc, "%s%c ", str_buffer(s), sep);
        else if (i == ct - 1 && term)
            doc_printf(doc, "%s%c", str_buffer(s), term);
        else
            doc_insert(doc, str_buffer(s));
    }
}

static str_ptr _get_gf_name(int gf)
{
    gf_info_ptr gfi = gf_lookup(gf);
    return str_alloc_format(
        "<color:%c>%s</color>",
        attr_to_attr_char(gfi->color),
        gfi->name
    );
}

static bool _easy_lore(mon_race_ptr race)
{
    if (plr->wizard) return TRUE;
    if (spoiler_hack) return TRUE;
    if (race->lore.flags & RFL_PROBE) return TRUE; /* Probing */
    return FALSE;
}

static bool _know_armor_hp(mon_race_ptr race)
{
    int level = race->alloc.lvl;
    int kills = race->lore.kills.total;

    if (_easy_lore(race)) return TRUE;

    if (kills > 304 / (4 + level)) return TRUE;
    else if (mon_race_is_unique(race) && kills > 304 / (38 + (5 * level) / 4)) return TRUE;

    return FALSE;
}

/* XXX The following calculations are slightly tweaked from very
 * old original code (was using 80). Spreadsheeting things up showed
 * spell damage not quite right (divisor is a hack) and what I would
 * guess to be the effect of melee damage inflation over the years.
 * I'm not sure what the following is going for, or what advantage it
 * has over say, just requiring 30 sightings to know stuff. */
static bool _know_damage_need(int dam, int lvl, bool unique)
{
    int need = 50*dam/MAX(1, lvl); /* was 80 */
    if (unique) need /= 5;
    need = MIN(100, MAX(5, need));
    return need;
}

static bool _know_damage_aux(int ct, int dam, int lvl, bool unique)
{
    int need = _know_damage_need(dam, lvl, unique);
    return ct >= need;
}

static bool _know_melee_damage(mon_race_ptr race, mon_effect_ptr effect)
{
    if (_easy_lore(race)) return TRUE;
    return _know_damage_aux(effect->lore, effect->dice.dd*effect->dice.ds,
        race->alloc.lvl + 4, mon_race_is_unique(race));
}

static bool _know_spell_damage(mon_race_ptr race, mon_spell_ptr spell)
{
    if (_easy_lore(race)) return TRUE;
    return _know_damage_aux(
        spell->lore, MIN(500, mon_spell_avg_dam(spell, race, FALSE))/10,
        race->alloc.lvl + 4, mon_race_is_unique(race));
}

static bool _know_aura_damage(mon_race_ptr race, mon_aura_ptr aura)
{
    if (_easy_lore(race)) return TRUE;
    return _know_damage_aux(aura->lore, aura->dam.dd * aura->dam.ds * 5,
        race->alloc.lvl + 4, mon_race_is_unique(race));
}

static bool _know_alertness(mon_race_ptr race)
{
    int wake = race->lore.turns.wake;
    int sleep = race->move.sleep;
    int ignore = race->lore.turns.sleep;

    if (_easy_lore(race)) return TRUE;
    if (wake * wake > sleep) return TRUE;
    if (ignore >= 255) return TRUE;
    if (!sleep && race->lore.kills.total >= 10) return TRUE;

    return FALSE;
}

/* help decide whether or not to display a flag.
 * the monster must have it, and the plr must have learned
 * about it (or be a wizard) */
static bool _kind(mon_race_ptr race, u32b flag)
{
    if (!(race->kind & flag)) return FALSE;
    if (_easy_lore(race)) return TRUE;
    if (race->lore.kind & flag) return TRUE;
    return FALSE;
}

static bool _move(mon_race_ptr race, u32b flag)
{
    if (!(race->move.flags & flag)) return FALSE;
    if (_easy_lore(race)) return TRUE;
    if (race->lore.move & flag) return TRUE;
    return FALSE;
}

static bool _ability(mon_race_ptr race, u32b flag)
{
    if (!(race->abilities & flag)) return FALSE;
    if (_easy_lore(race)) return TRUE;
    if (race->lore.abilities & flag) return TRUE;
    return FALSE;
}

static bool _attribute(mon_race_ptr race, u32b flag)
{
    if (!(race->attributes & flag)) return FALSE;
    if (_easy_lore(race)) return TRUE;
    if (race->lore.attributes & flag) return TRUE;
    return FALSE;
}

/**************************************************************************
 * Basic Info
 **************************************************************************/
static char _speed_color(int speed)
{
    if (speed >= 30) return 'r';
    else if (speed >= 20) return 'o';
    else if (speed >= 15) return 'u';
    else if (speed >= 10) return 'R';
    else if (speed >= 1) return 'U';
    else if (speed == 0) return 'w';
    else return 'G';
}
static void _display_level(mon_race_ptr race, doc_ptr doc)
{
    doc_insert(doc, "Level   : ");
    if (race->alloc.lvl == 0)
        doc_insert(doc, "<color:G>Town</color>");
    else if (_easy_lore(race) || race->lore.kills.total > 0)
    {
        if (race->alloc.max_lvl)
            doc_printf(doc, "<color:G>%d to %d</color>", race->alloc.lvl, race->alloc.max_lvl);
        else
            doc_printf(doc, "<color:G>%d</color>", race->alloc.lvl);
    }
    else
        doc_insert(doc, "<color:y>?</color>");
    doc_newline(doc);
    if (spoiler_hack)
        doc_printf(doc, "Rarity  : <color:G>%d</color>\n", race->alloc.rarity);
}
static void _display_ac(mon_race_ptr race, doc_ptr doc)
{
    doc_insert(doc, "AC      : ");
    if (_know_armor_hp(race))
        doc_printf(doc, "<color:G>%d</color>", race->ac);
    else
        doc_insert(doc, "<color:y>?</color>");
    doc_newline(doc);
}
static void _display_hp(mon_race_ptr race, doc_ptr doc)
{
    doc_insert(doc, "HP      : ");
    if (_know_armor_hp(race))
    {
        doc_insert(doc, "<color:G>");
        dice_doc(race->hp, doc);
        doc_insert(doc, "</color>");
    }
    else
        doc_insert(doc, "<color:y>?</color>");
    doc_newline(doc);
}
static void _display_speed(mon_race_ptr race, doc_ptr doc)
{
    int speed = race->move.speed;
    doc_printf(doc, "Speed: <color:%c>%+d</color>", _speed_color(speed), speed);

    if (race->move.random >= 75) doc_insert(doc, " <color:r>Extremely Erratic</color>");
    else if (race->move.random >= 50) doc_insert(doc, " <color:R>Somewhat Erratic</color>");
    else if (race->move.random > 0) doc_insert(doc, " <color:o>A Bit Erratic</color>");

    if (mon_race_never_move(race)) doc_insert(doc, ", <color:u>Stationary</color>");
    if (_move(race, RFM_QUICK)) doc_insert(doc, ", <color:y>Quick</color>");

    doc_newline(doc);
}
static void _display_alertness(mon_race_ptr race, doc_ptr doc)
{
    if (_know_alertness(race))
    {
        doc_insert(doc, "Alert: ");
        if (race->move.sleep > 200)
            doc_insert(doc, "<color:D>Ignores Intruders</color>");
        else if (race->move.sleep > 95)
            doc_insert(doc, "<color:w>Very Inattentive</color>");
        else if (race->move.sleep > 75)
            doc_insert(doc, "<color:W>Inattentive</color>");
        else if (race->move.sleep > 45)
            doc_insert(doc, "<color:U>Overlooks</color>");
        else if (race->move.sleep > 25)
            doc_insert(doc, "<color:y>Unseeing</color>");
        else if (race->move.sleep > 10)
            doc_insert(doc, "<color:y>Fairly Unseeing</color>");
        else if (race->move.sleep > 5)
            doc_insert(doc, "<color:o>Fairly Observant</color>");
        else if (race->move.sleep > 3)
            doc_insert(doc, "<color:R>Observant</color>");
        else if (race->move.sleep > 1)
            doc_insert(doc, "<color:r>Very Observant</color>");
        else if (race->move.sleep > 0)
            doc_insert(doc, "<color:r>Vigilant</color>");
        else
            doc_insert(doc, "<color:v>Ever Vigilant</color>");
        doc_printf(doc, " <color:G>(%d')</color>\n", 10 * race->move.range);
    }
}
static void _display_type(mon_race_ptr race, doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)str_free);
    doc_insert(doc, "Type : <indent><style:indent>");

    if (_kind(race, RFK_HORROR))
        vec_add(v, str_copy_s("<color:v>Sanity Blasting</color>"));
    if (_kind(race, RFK_ANIMAL))
        vec_add(v, str_copy_s("<color:G>Natural</color>"));
    if (_easy_lore(race) || race->lore.flags & RFL_ALIGN)
        vec_add(v, str_copy_s(align_desc(race->align)));
    if (_kind(race, RFK_UNDEAD))
        vec_add(v, str_copy_s("<color:v>Undead</color>"));
    if (_kind(race, RFK_NONLIVING))
        vec_add(v, str_copy_s("<color:U>Nonliving</color>"));
    if (_kind(race, RFK_AMBERITE))
        vec_add(v, str_copy_s("<color:v>Amberite</color>"));
    if (_kind(race, RFK_OLYMPIAN))
        vec_add(v, str_copy_s("<color:v>Olympian</color>"));
    if (_kind(race, RFK_DRAGON))
        vec_add(v, str_copy_s("<color:o>Dragon</color>"));
    if (_kind(race, RFK_DEMON))
        vec_add(v, str_copy_s("<color:v>Demon</color>"));
    if (_kind(race, RFK_GIANT))
        vec_add(v, str_copy_s("<color:U>Giant</color>"));
    if (_kind(race, RFK_TROLL))
        vec_add(v, str_copy_s("<color:B>Troll</color>"));
    if (_kind(race, RFK_ORC))
        vec_add(v, str_copy_s("<color:u>Orc</color>"));
    if (_kind(race, RFK_HUMAN))
        vec_add(v, str_copy_s("<color:W>Human</color>"));
    if (_kind(race, RFK_ELF))
        vec_add(v, str_copy_s("<color:G>Elf</color>"));
    if (_kind(race, RFK_DARK_ELF))
        vec_add(v, str_copy_s("<color:D>Dark-Elf</color>"));
    if (_kind(race, RFK_HOBBIT))
        vec_add(v, str_copy_s("<color:U>Hobbit</color>"));
    if (_kind(race, RFK_DWARF))
        vec_add(v, str_copy_s("<color:u>Dwarf</color>"));
    if (_kind(race, RFK_THIEF))
        vec_add(v, str_copy_s("<color:D>Thief</color>"));
    if (_attribute(race, RF_MALE))
        vec_add(v, str_copy_s("<color:b>Male</color>"));
    if (_attribute(race, RF_FEMALE))
        vec_add(v, str_copy_s("<color:R>Female</color>")); /* Pink? */
    if (plr->pclass == CLASS_WARLOCK && warlock_is_pact_monster(race))
        vec_add(v, str_copy_s("<color:v>Pact</color>"));

    _print_list(v, doc, ',', '\0');
    vec_free(v);
    doc_insert(doc, "</style></indent>\n");
}
static void _prt_equippy(int tval, int sval, doc_ptr doc)
{
    int k_idx = lookup_kind(tval, sval);
    object_kind *k_ptr = &k_info[k_idx];
    if (_possessor_hack)
        doc_insert_char(doc, k_ptr->d_attr, k_ptr->d_char);
    else
        doc_insert_char(doc, k_ptr->x_attr, k_ptr->x_char);
}

static void _display_body(mon_race_ptr race, doc_ptr doc)
{
    equip_template_ptr body = equip_template_lookup(race->body.body_id);
    int j;

    doc_insert(doc, "Body : ");
    for (j = 1; j <= body->max; j++)
    {
        switch (body->slots[j].type)
        {
        case EQUIP_SLOT_GLOVES:
            _prt_equippy(TV_GLOVES, SV_SET_OF_GAUNTLETS, doc);
            break;
        case EQUIP_SLOT_WEAPON_SHIELD:
            if (body->slots[j].hand % 2)
                _prt_equippy(TV_SHIELD, SV_LARGE_METAL_SHIELD, doc);
            else
                _prt_equippy(TV_SWORD, SV_LONG_SWORD, doc);
            break;
        case EQUIP_SLOT_WEAPON:
            _prt_equippy(TV_SWORD, SV_LONG_SWORD, doc);
            break;
        case EQUIP_SLOT_RING:
            _prt_equippy(TV_RING, 0, doc);
            break;
        case EQUIP_SLOT_BOW:
            _prt_equippy(TV_BOW, SV_LONG_BOW, doc);
            break;
        case EQUIP_SLOT_AMULET:
            _prt_equippy(TV_AMULET, 0, doc);
            break;
        case EQUIP_SLOT_LIGHT:
            _prt_equippy(TV_LIGHT, SV_LIGHT_FEANOR, doc);
            break;
        case EQUIP_SLOT_BODY_ARMOR:
            _prt_equippy(TV_HARD_ARMOR, SV_CHAIN_MAIL, doc);
            break;
        case EQUIP_SLOT_CLOAK:
            _prt_equippy(TV_CLOAK, SV_CLOAK, doc);
            break;
        case EQUIP_SLOT_BOOTS:
            _prt_equippy(TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS, doc);
            break;
        case EQUIP_SLOT_HELMET:
            _prt_equippy(TV_HELM, SV_IRON_HELM, doc);
            break;
        case EQUIP_SLOT_ANY:
            doc_insert_char(doc, TERM_WHITE, '*');
            break;
        case EQUIP_SLOT_CAPTURE_BALL:
            _prt_equippy(TV_CAPTURE, 0, doc);
            break;
        }
    }
    doc_newline(doc);
}
static void _display_basic(mon_race_ptr race, doc_ptr doc)
{
    doc_printf(doc, "Name    : <indent><style:indent><color:B>%s</color></style></indent>\n", race->name);
    {
        term_char_t ac = mon_race_visual_ascii(race);
        term_char_t gc = mon_race_visual(race);
        doc_ptr cols[2];

        cols[0] = doc_alloc(20);
        cols[1] = doc_alloc(MAX(20, MIN(50, doc_width(doc) - 20))); /* Monster Recall Terminal */

        /* Column 1 */
        assert(ac.c);
        doc_printf(cols[0], "Display : <color:%c>%c</color>", attr_to_attr_char(ac.a), ac.c);
        if (use_graphics && (gc.c != ac.c || gc.a != ac.a))
        {
            doc_insert_char(cols[0], TERM_WHITE, ' ');
            doc_insert_term_char(cols[0], gc);
        }
        doc_newline(cols[0]);
        _display_level(race, cols[0]);
        _display_ac(race, cols[0]);
        _display_hp(race, cols[0]);

        /* Column 2 */
        #ifdef DEVELOPER
        doc_printf(cols[1], "ID   : <color:B>%s</color>\n", sym_str(race->id));
        #endif
        _display_speed(race, cols[1]);
        if (!_possessor_hack) _display_alertness(race, cols[1]);
        _display_type(race, cols[1]);

        /* Display body info for sighted monsters when playing a possessor.
         * This makes it easier to decide whether or not to switch forms. */
        if ( !(race->body.flags & RF_POS_DISABLED)
          && (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
          && race->lore.sightings
          && !_possessor_hack)
        {
            _display_body(race, cols[1]);
        }

        doc_insert_cols(doc, cols, 2, 0);

        doc_free(cols[0]);
        doc_free(cols[1]);
    }
}

/**************************************************************************
 * Resists
 **************************************************************************/
static void _display_resists(mon_race_ptr race, doc_ptr doc)
{
    int     i;
    int     ct = 0;
    vec_ptr v = vec_alloc((vec_free_f)str_free);

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        u32b mask = (1U << i);
        if (!_easy_lore(race) && !(race->lore.resist & mask)) continue;
        if (!(race->resist & mask)) continue;
        vec_add(v, _get_gf_name(i));
    }
    if (vec_length(v))
    {
        doc_insert(doc, "Resist  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }
    vec_clear(v);
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        u32b mask = (1U << i);
        if (!_easy_lore(race) && !(race->lore.resist & mask)) continue;
        if (!(race->immune & mask)) continue;
        vec_add(v, _get_gf_name(i));
    }
    if (vec_length(v))
    {
        doc_insert(doc, "Immune  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }
    vec_clear(v);
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        u32b mask = (1U << i);
        if (!_easy_lore(race) && !(race->lore.resist & mask)) continue;
        if (!(race->vuln & mask)) continue;
        if (i == GF_DISINTEGRATE)
            vec_add(v, str_copy_s("<color:u>Rock Remover</color>"));
        else
            vec_add(v, _get_gf_name(i));
    }
    if (vec_length(v))
    {
        doc_insert(doc, "Vuln    : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }
    if (ct) doc_newline(doc);
    vec_free(v);
}

/**************************************************************************
 * Spells
 **************************************************************************/
static void _display_frequency(mon_race_ptr race, doc_ptr doc)
{
    int pct = 0;
    assert(race->spells);
    if ( spoiler_hack
      || (!race->lore.turns.spell && (race->lore.flags & RFL_PROBE)) )
    {
        pct = race->spells->freq * 100;
    }
    else if (race->lore.turns.spell)
    {
        int total = race->lore.turns.total;
        pct = race->lore.turns.spell * 10000 / total;
    }
    if (pct)
    {
        vec_ptr v = vec_alloc((vec_free_f)str_free);

        doc_printf(doc, "<color:G>Spells  :</color> <indent><color:G>%d.%02d%%</color> ", pct/100, pct%100);
        if (!spoiler_hack && race->lore.turns.total > 0)
            doc_printf(doc, "(%d of %d moves) ", race->lore.turns.spell, race->lore.turns.total);

        if (race->lore.attributes & RF_SMART)
            vec_add(v, str_copy_s("<color:y>Intelligent</color>"));
        _print_list(v, doc, ',', '\0');
        vec_free(v);
        doc_insert(doc, "</indent>");
    }
    else
    {
        doc_printf(doc, "<color:G>Spells  :</color> <color:y>?%%</color>");
    }
    doc_newline(doc);
}
static bool _is_attack_spell(mon_spell_ptr spell)
{
    switch (spell->id.type)
    {
    case MST_BREATH: case MST_BALL: case MST_BOLT: case MST_BEAM:
    case MST_LOS: return TRUE;
    }
    return FALSE;
}
static void _display_spell_group(mon_race_ptr race, mon_spell_group_ptr group, vec_ptr v)
{
    int i;
    if (!group) return;
    for (i = 0; i < group->count; i++)
    {
        mon_spell_ptr spell = &group->spells[i];
        if (_easy_lore(race) || spell->lore)
        {
            str_ptr s = str_alloc();
            mon_spell_display(spell, s);
            if (_know_spell_damage(race, spell))
            {
                if (spell->parm.tag && spell->id.type != MST_SUMMON)
                {
                    str_append_c(s, ' ');
                    str_append_c(s, '(');
                    if (spoiler_hack || !_is_attack_spell(spell))
                    {
                        mon_spell_parm_print(&spell->parm, s, race);
                        if (spell->id.type == MST_LOS && spell->id.effect == GF_HAND_DOOM)
                            str_append_c(s, '%');
                    }
                    else
                        str_printf(s, "%d", mon_spell_avg_dam(spell, race, !_possessor_hack));
                    if (!spoiler_hack && !_possessor_hack && spell->lore) /* XXX stop repeating yourself! */
                        str_printf(s, ", %dx", spell->lore);
                    str_append_c(s, ')');
                }
                else if (!spoiler_hack && !_possessor_hack && spell->lore) /* XXX stop repeating yourself! */
                    str_printf(s, " (%dx)", spell->lore);
            }
            else if (!spoiler_hack && !_possessor_hack && spell->lore) /* XXX stop repeating yourself! */
                str_printf(s, " (%dx)", spell->lore);
            vec_add(v, s);
        }
    }
}
static void _display_spells(mon_race_ptr race, doc_ptr doc)
{
    int            ct = 0;
    vec_ptr        v = vec_alloc((vec_free_f)str_free);
    mon_spells_ptr spells = race->spells;

    assert(spells);
    if (!_possessor_hack) _display_frequency(race, doc);

    /* Breaths */
    _display_spell_group(race, spells->groups[MST_BREATH], v);
    if (vec_length(v))
    {
        doc_insert(doc, "Breathe : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Offense */
    vec_clear(v);
    _display_spell_group(race, spells->groups[MST_BALL], v);
    _display_spell_group(race, spells->groups[MST_BOLT], v);
    _display_spell_group(race, spells->groups[MST_BEAM], v);
    _display_spell_group(race, spells->groups[MST_LOS], v);
    if (vec_length(v))
    {
        doc_insert(doc, "Offense : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Annoy */
    vec_clear(v);
    _display_spell_group(race, spells->groups[MST_ANNOY], v);
    if (vec_length(v))
    {
        doc_insert(doc, "Annoy   : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Defense */
    vec_clear(v);
    _display_spell_group(race, spells->groups[MST_BUFF], v);
    _display_spell_group(race, spells->groups[MST_BIFF], v);
    _display_spell_group(race, spells->groups[MST_HEAL], v);
    _display_spell_group(race, spells->groups[MST_ESCAPE], v);
    _display_spell_group(race, spells->groups[MST_TACTIC], v);
    _display_spell_group(race, spells->groups[MST_WEIRD], v);
    if (vec_length(v))
    {
        doc_insert(doc, "Defense : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Summoning */
    vec_clear(v);
    _display_spell_group(race, spells->groups[MST_SUMMON], v);
    if (vec_length(v))
    {
        doc_insert(doc, "Summon  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    if (ct) doc_newline(doc);
    vec_free(v);
}

/**************************************************************************
 * Attacks
 **************************************************************************/
static cptr _method_desc(int method)
{
    mon_blow_info_ptr info = mon_blow_info_lookup(method);
    if (info) return info->name;
    return "Weird";
}
static void _dice_dam(dice_t dice, str_ptr s)
{
    if (dice.dd && dice.ds)
    {
        str_printf(s, " (%dd%d", dice.dd, dice.ds);
        if (dice.base)
            str_printf(s, "+%d", dice.base);
        str_append_c(s, ')');
    }
    else if (dice.base)
    {
        str_printf(s, " (%d)", dice.base);
    }
}

static bool _effect_show_dam(int effect)
{
    switch (effect)
    {
    case RBE_CUT:
    case RBE_DRAIN_EXP:
    case RBE_LOSE_STR: case RBE_LOSE_INT: case RBE_LOSE_WIS:
    case RBE_LOSE_DEX: case RBE_LOSE_CON: case RBE_LOSE_CHR:
    case RBE_LOSE_ALL:
    case RBE_EAT_LIGHT:
    case RBE_DRAIN_CHARGES:
    case RBE_EAT_GOLD:
    case RBE_EAT_ITEM:
    case RBE_EAT_FOOD:
        return FALSE;
    }
    if (effect < GF_COUNT)
    {
        gf_info_ptr info = gf_lookup(effect);
        if (effect && !(info->flags & GFF_DAMAGE))
            return FALSE;
    }
    return TRUE;
}
static str_ptr _effect_desc(mon_race_ptr race, mon_effect_ptr effect)
{
    str_ptr s;

    switch (effect->type)
    {
    case RBE_HURT:        s = str_copy_s("Hurt"); break;
    case RBE_DRAIN_CHARGES: s = str_copy_s("Drain Charges"); break;
    case RBE_EAT_GOLD:    s = str_copy_s("Steal Gold"); break;
    case RBE_EAT_ITEM:    s = str_copy_s("Steal Item"); break;
    case RBE_EAT_FOOD:    s = str_copy_s("Steal Food"); break;
    case RBE_EAT_LIGHT:    s = str_copy_s("Absorb Light"); break;
    case RBE_LOSE_STR:    s = str_copy_s("Reduce Strength"); break;
    case RBE_LOSE_INT:    s = str_copy_s("Reduce Intelligence"); break;
    case RBE_LOSE_WIS:    s = str_copy_s("Reduce Wisdom"); break;
    case RBE_LOSE_DEX:    s = str_copy_s("Reduce Dexterity"); break;
    case RBE_LOSE_CON:    s = str_copy_s("Reduce Constitution"); break;
    case RBE_LOSE_CHR:    s = str_copy_s("Reduce Charisma"); break;
    case RBE_LOSE_ALL:    s = str_copy_s("Reduce All Stats"); break;
    case RBE_SHATTER:     s = str_copy_s("Shatter"); break;
    case RBE_DRAIN_EXP:   s = str_copy_s("<color:D>Lower Experience</color>"); break;
    case RBE_DISEASE:     s = str_copy_s("Disease"); break;
    case RBE_VAMP:        s = str_copy_s("<color:D>Vampiric</color>"); break;
    case RBE_CUT:         s = str_copy_s("<color:r>Cut</color>"); break;
    case GF_MISSILE:      s = str_copy_s("Damage"); break;
    case GF_FEAR:     s = str_copy_s("<color:r>Terrify</color>"); break;
    default:              s = str_copy_s(gf_name(effect->type));
    }
    assert(s);
    if (_effect_show_dam(effect->type) && _know_melee_damage(race, effect))
        _dice_dam(effect->dice, s);
    return s;
}
static int _ct_known_attacks(mon_race_ptr race)
{
    int ct = 0;
    int i;
    for (i = 0; i < vec_length(race->blows); i++)
    {
        mon_blow_ptr blow = vec_get(race->blows, i);
        if (blow->lore || _easy_lore(race)) ct++;
    }
    return ct;
}
static void _display_attacks(mon_race_ptr race, doc_ptr doc)
{
    if (mon_race_never_blow(race))
        doc_insert(doc, "Attacks : <color:D>None</color>\n");
    else if (_ct_known_attacks(race))
    {
        int i,j;
        doc_insert(doc, "<color:G>Attacks :</color>\n");
        for (i = 0; i < vec_length(race->blows); i++)
        {
            mon_blow_ptr blow = vec_get(race->blows, i);
            vec_ptr      v;
            char         method[30];

            if (!_easy_lore(race) && !blow->lore) continue;
            v = vec_alloc((vec_free_f)str_free);
            for (j = 0; j < blow->effect_ct; j++)
            {
                mon_effect_ptr effect = &blow->effects[j];
                if (!effect->type) continue;
                if (!_easy_lore(race) && !effect->lore) continue;
                vec_add(v, _effect_desc(race, effect));
            }
            if (blow->method == RBM_MONK)
                vec_add(v, str_copy_s("Various Martial Arts Effects"));
            if (blow->blows < 100)
                sprintf(method, "%s x%d%%", _method_desc(blow->method), blow->blows);
            else if (blow->blows > 100)
            {
                int b = blow->blows/100;
                int f = (blow->blows%100)/10;
                if (f) /* 350 -> x3.5 */
                    sprintf(method, "%s x%d.%d", _method_desc(blow->method), b, f);
                else   /* 300 -> x3 */
                    sprintf(method, "%s x%d", _method_desc(blow->method), b);
            }
            else
                strcpy(method, _method_desc(blow->method));
            doc_printf(doc, "%-9.9s", method);
            if (vec_length(v))
            {
                doc_insert(doc, " <indent><style:indent>");
                _print_list(v, doc, ',', '\0');
                #if 0
                {int mul = mon_crit_avg_mul(race, blow);
                int p = mon_crit_chance(race, blow);
                dice_t d = mon_blow_base_dice(blow);
                int dam = mul*d.dd*(d.ds + 1)/200 + d.base;
                dam = dam*blow->blows/100;
                if (mul > 100)
                    doc_printf(doc, " <color:r>Crit:</color> %d.%02dx %d%% %d", mul/100, mul%100, p/10, dam);}
                #endif
                doc_insert(doc, "</style></indent>");
            }
            doc_newline(doc);
            vec_free(v);
        }
    }
    else
        doc_insert(doc, "Attacks : <color:y>?</color>\n");

    doc_newline(doc);
}

/**************************************************************************
 * Other Info
 **************************************************************************/
static void _display_other(mon_race_ptr race, doc_ptr doc)
{
    int        ct = 0;
    vec_ptr    v = vec_alloc((vec_free_f)str_free);
    mon_aura_ptr aura;

    /* move */
    if (_move(race, RFM_TUNNEL))
        vec_add(v, str_copy_s("<color:U>Tunnel</color>"));

    if (_move(race, RFM_PASSWALL))
        vec_add(v, str_copy_s("<color:B>Passwall</color>"));

    if (_move(race, RFM_PASSWEB))
        vec_add(v, str_copy_s("<color:s>Passweb</color>"));

    if (_move(race, RFM_BASH))
        vec_add(v, str_copy_s("<color:u>Bash</color>"));
    else if (_move(race, RFM_OPEN))
        vec_add(v, str_copy_s("<color:u>Open</color>"));

    if (_move(race, RFM_DESTROY))
        vec_add(v, str_copy_s("<color:r>Destroy</color>"));
    else if (_move(race, RFM_PICKUP))
        vec_add(v, str_copy_s("<color:B>Pickup</color>"));

    if (_move(race, RFM_TRAMPLE))
        vec_add(v, str_copy_s("<color:r>Trample</color>"));
    else if (_move(race, RFM_PUSH))
        vec_add(v, str_copy_s("<color:R>Push</color>"));

    /* lite XXX radius and lantern? */
    if (race->lore.sightings) /* XXX we aren't loring this */
    {
        if (race->light > 0)
            vec_add(v, str_copy_s("<color:y>Shining</color>"));

        if (race->light < 0)
            vec_add(v, str_copy_s("<color:D>Darkness</color>"));
    }

    /* abilities */
    if (_ability(race, RF_REFLECT))
        vec_add(v, str_copy_s("<color:o>Reflection</color>"));

    if (_ability(race, RF_INVIS))
        vec_add(v, str_copy_s("<color:B>Invisible</color>"));

    if (_ability(race, RF_MULTIPLY))
        vec_add(v, str_copy_s("<color:U>Multiply</color>"));

    if (_ability(race, RF_REGEN))
        vec_add(v, str_copy_s("<color:r>Regeneration</color>"));

    /* attributes */
    if (_attribute(race, RF_RIDING))
        vec_add(v, str_copy_s("<color:s>Riding</color>"));

    if (_attribute(race, RF_COLD_BLOOD))
        vec_add(v, str_copy_s("<color:w>Cold Blooded</color>"));

    if (_attribute(race, RF_EMPTY_MIND))
        vec_add(v, str_copy_s("<color:o>Mindless</color>"));

    if (_attribute(race, RF_WEIRD_MIND))
        vec_add(v, str_copy_s("<color:w>Weird Mind</color>"));

    /* other */
    if (race->alloc.flags & RFA_GUARDIAN) /* you've seen a Wanted poster or something */
        vec_add(v, str_copy_s("<color:R>Guardian</color>"));

    if (race->flagsx & RFX_GUARDIAN)
        vec_add(v, str_copy_s("<color:B>Guardian</color>")); /* XXX different color */

    if (vec_length(v))
    {
        doc_insert(doc, "Info    : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    if ( race->evolution.id
      && (_easy_lore(race) || (race->lore.flags & RFL_EVOLUTION)) )
    {
        mon_race_ptr next = mon_race_lookup(race->evolution.id);
        char buf[10];

        big_num_display(race->evolution.exp, buf);

        doc_insert(doc, "Evolves : <indent>");
        doc_insert_term_char(doc, mon_race_visual(next));
        doc_space(doc);
        doc_printf(doc, "<color:B>%s</color> at <color:G>%s xp</color></indent>\n",
            next->name, buf);
    }

    /* Auras */
    vec_clear(v);

    if (_ability(race, RF_REVENGE))
        vec_add(v, str_copy_s("<color:v>Retaliation</color>"));
    if (_ability(race, RF_FEAR))
        vec_add(v, str_copy_s("<color:v>Fear</color>"));

    for (aura = race->auras; aura; aura = aura->next)
    {
        gf_info_ptr gfi;
        if (!_easy_lore(race) && !aura->lore) continue;
        gfi = gf_lookup(aura->gf);
        if (gfi)
        {
            str_ptr s = str_alloc_format("<color:%c>%s</color>", attr_to_attr_char(gfi->color), gfi->name);
            if (_effect_show_dam(aura->gf) && _know_aura_damage(race, aura))
                _dice_dam(aura->dam, s);
            vec_add(v, s);
        }
    }

    if (vec_length(v))
    {
        doc_insert(doc, "Auras   : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    if (ct) doc_newline(doc);
    vec_free(v);
}

/**************************************************************************
 * Kills and Drops
 **************************************************************************/
static int _max_drop_ct(mon_drop_ptr drop)
{
    int ct = 1;
    if (drop->dd && drop->ds)
        ct = drop->dd * drop->ds + drop->base;
    else if (drop->base)
        ct = drop->base;
    return ct;
}

static void _display_drop(mon_drop_ptr drop, doc_ptr doc)
{
    int  ct = _max_drop_ct(drop);
    cptr pluralizer = ct > 1 ? "s" : "";

    if (drop->pct)
        doc_printf(doc, "%d%% chance of ", drop->pct);

    if (drop->drop.flags & OBJ_DROP_STD_ART)
    {
        obj_t forge = {0};
        char  name[MAX_NLEN];
        art_ptr art = arts_lookup(drop->drop.object);

        art_create_std(&forge, art, AM_DEBUG | AM_NO_DROP);
        obj_identify_fully(&forge);
        object_desc(name, &forge, OD_NAME_ONLY | OD_COLOR_CODED);
        doc_printf(doc, "%s", name);
    }
    else
    {
        if (drop->dd && drop->ds)
        {
            doc_printf(doc, "%dd%d", drop->dd, drop->ds);
            if (drop->base)
                doc_printf(doc, "+%d", drop->base);
            doc_insert_char(doc, TERM_WHITE, ' ');
        }
        else if (drop->base)
            doc_printf(doc, "%d ", drop->base);
        else
            doc_insert(doc, "1 ");

        if ((drop->drop.flags & OBJ_DROP_TYPE) && drop->drop.object == TV_GOLD)
            doc_printf(doc, "Treasure%s", pluralizer);
        else
        {
            if (drop->drop.flags & OBJ_DROP_RAND_ART)
                doc_insert(doc, "<color:v>Unique</color> ");
            else if (drop->drop.flags & (AM_GREAT | OBJ_DROP_RAND_EGO | OBJ_DROP_STD_EGO))
                doc_insert(doc, "<color:v>Exceptional</color> ");
            else if (drop->drop.flags & AM_GOOD)
                doc_insert(doc, "<color:r>Good</color> ");

            if (drop->drop.flags & AM_TAILORED)
                doc_insert(doc, "<color:v>Tailored</color> ");

            if (drop->drop.flags & OBJ_DROP_RANDOM)
            {
                doc_printf(doc, "Object%s", pluralizer);
                if (drop->drop.flags & OBJ_DROP_SOME_GOLD)
                    doc_printf(doc, " or Treasure%s", pluralizer);
            }
            else if (drop->drop.flags & OBJ_DROP_TYPE)
            {
                switch (drop->drop.object)
                {
                case TV_RING: doc_printf(doc, "Ring%s", pluralizer); break;
                case TV_HAFTED: doc_printf(doc, "Hafted Weapon%s", pluralizer); break;
                case TV_SWORD: doc_printf(doc, "Sword%s", pluralizer); break;
                case TV_CLOAK: doc_printf(doc, "Cloak%s", pluralizer); break;
                case TV_HARD_ARMOR: doc_printf(doc, "Hard Armor%s", pluralizer); break;
                case TV_SCROLL: doc_printf(doc, "Scroll%s", pluralizer); break;
                case TV_POTION: doc_printf(doc, "Potion%s", pluralizer); break;
                case OBJ_TYPE_HI_BOOK: doc_printf(doc, "High Spellbook%s", pluralizer); break;
                case OBJ_TYPE_BOOK: doc_printf(doc, "Spellbook%s", pluralizer); break;
                default: doc_printf(doc, "Specific Object%s", pluralizer); /* XXX */
                }
            }
            else
            {
                obj_t forge = {0};
                char  name[MAX_NLEN];

                object_prep(&forge, drop->drop.object);
                obj_identify_fully(&forge);
                forge.number = ct;
                object_desc(name, &forge, OD_NAME_ONLY | OD_OMIT_PREFIX | OD_COLOR_CODED);
                doc_printf(doc, "%s", name);
            }
        }
    }
    doc_newline(doc);
}

static void _display_drops(mon_race_ptr race, doc_ptr doc)
{
    int ct_gold = 0;
    int ct_obj = 0;
    mon_drop_ptr drop = NULL;

    /* XXX Lore still needs work in the new drop system. */
    if (!race->drops) return;

    /* XXX Wizards can just print each rule */
    if (_easy_lore(race))
    {
        doc_insert(doc, "Drops   : <indent>");
        for (drop = race->drops; drop; drop = drop->next)
            _display_drop(drop, doc);
        doc_insert(doc, "</indent>");

        return;
    }

    /* XXX Assume the first rule is the principle rule */
    drop = race->drops;

    ct_gold = race->lore.drops.gold;
    ct_obj = race->lore.drops.obj;

    if (ct_gold || ct_obj)
    {
        doc_insert(doc, "Drops   : <indent>");

        /* XXX Separate lines for each works better. Assume objects
         * come from the first rule. Treasures might also come from
         * the first rule, but many monsters have secondary rules for
         * gold drops (e.g. Check out The Serpent of Chaos). */
        if (ct_obj)
        {
            if (ct_obj == 1)
                doc_insert(doc, "1 ");
            else if (ct_obj == 2)
                doc_insert(doc, "1 or 2 ");
            else
                doc_printf(doc, "Up to %d ", ct_obj);

            if (drop->drop.flags & AM_GREAT)
                doc_insert(doc, "<color:v>Exceptional</color> ");
            else if (drop->drop.flags & AM_GOOD)
                doc_insert(doc, "<color:r>Good</color> ");

            doc_printf(doc, "Object%s\n", (ct_obj > 1) ? "s" : "");
        }
        if (ct_gold)
        {
            if (ct_gold == 1)
                doc_insert(doc, "1 ");
            else if (ct_gold == 2)
                doc_insert(doc, "1 or 2 ");
            else
                doc_printf(doc, "Up to %d ", ct_gold);
            doc_printf(doc, "Treasure%s\n", (ct_gold > 1) ? "s" : "");
        }
        doc_insert(doc, "</indent>");
    }
}
static void _display_kills(mon_race_ptr race, doc_ptr doc)
{
    if (mon_race_is_unique(race))
    {
        if (spoiler_hack)
            doc_insert(doc, "Status  : <color:v>Unique</color>");
        else
        {
            doc_insert(doc, "Status  : ");
            if (race->alloc.max_num == 0)
                doc_insert(doc, "<color:D>Dead</color>");
            else if (mon_is_wanted(race->id))
                doc_insert(doc, "<color:v>Wanted</color>");
            else
                doc_insert(doc, "<color:y>Alive</color>");
        }
        doc_newline(doc);
    }
    else if (!spoiler_hack)
    {
        doc_printf(doc, "Kills   : <color:G>%d</color>\n", race->lore.kills.current);
    }

    if (_easy_lore(race) || race->lore.kills.total)
    {
        int plev = spoiler_hack ? 50 : plr->max_plv;
        int xp = race->mexp * race->alloc.lvl / (plev + 2);
        char buf[10];

        big_num_display(xp, buf);
        doc_printf(doc, "Exp     : <color:G>%s</color> at CL%d\n", buf, plev);
    }

    _display_drops(race, doc);
    doc_newline(doc);
}

/**************************************************************************
 * Desc
 **************************************************************************/
static void _display_desc(mon_race_ptr race, doc_ptr doc)
{
    if (race->text)
    {
        doc_insert(doc, race->text);
        doc_newline(doc);
    }
}

/**************************************************************************
 * Public
 **************************************************************************/
void mon_display(mon_race_ptr race)
{
    mon_display_rect(race, ui_menu_rect());
}
void mon_display_rect(mon_race_ptr race, rect_t display)
{
    doc_ptr doc = doc_alloc(MIN(display.cx, 72));

    if (display.cx > 80)
        display.cx = 80;

    mon_display_doc(race, doc);

    screen_save();
    if (doc_cursor(doc).y < display.cy - 3)
    {
        doc_insert(doc, "\n<color:B>[Press Any Key to Continue]</color>\n\n");
        doc_sync_term(doc, doc_range_all(doc), doc_pos_create(display.x, display.y));
        inkey();
    }
    else
    {
        doc_display_aux(doc, "Monster Info", 0, display);
    }
    screen_load();

    doc_free(doc);
}
void mon_display_doc(mon_race_ptr race, doc_ptr doc)
{
    _display_basic(race, doc);
    _display_resists(race, doc);
    if (race->spells && race->spells->freq)
    {
        /* XXX need better way to display possessor info ... */
        if (0 && plr->wizard)
        {
            blue_mage_wizard_probe(race, doc);
            doc_newline(doc);
        }
        else
            _display_spells(race, doc);
    }
    _display_attacks(race, doc);
    _display_other(race, doc);
    if (!_possessor_hack) _display_kills(race, doc);

    #ifdef DEVELOPER
    if (0 || !spoiler_hack)
        mon_spoil_nastiness(race, doc);
    #endif

    _display_desc(race, doc);
}

void mon_display_possessor(mon_race_ptr race, doc_ptr doc)
{
    _possessor_hack = TRUE;
    mon_display_doc(race, doc);
    _possessor_hack = FALSE;
}

