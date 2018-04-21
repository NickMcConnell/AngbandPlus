#include "angband.h"

#include <stdlib.h>
#include <assert.h>

/* Build & Display the "Character Sheet" */

extern void py_display(void);
extern void py_display_birth(void);
extern void py_display_spells(doc_ptr doc, spell_info *table, int ct);
extern void py_display_powers(doc_ptr doc, spell_info *table, int ct);
extern void py_display_character_sheet(doc_ptr doc);
extern void py_display_dungeons(doc_ptr doc);

static void _build_general(doc_ptr doc);
static void _build_equipment(doc_ptr doc);
static void _build_melee(doc_ptr doc);
static void _build_shooting(doc_ptr doc);
static void _build_powers(doc_ptr doc);
static void _build_spells(doc_ptr doc);
static void _build_dungeons(doc_ptr doc);
static void _build_quests(doc_ptr doc);
static void _build_uniques(doc_ptr doc);
static void _build_virtues(doc_ptr doc);
static void _build_race_history(doc_ptr doc);
static void _build_mutations(doc_ptr doc);
static void _build_pets(doc_ptr doc);
static void _build_inventory(doc_ptr doc);
static void _build_home(doc_ptr doc);
static void _build_museum(doc_ptr doc);
static void _build_statistics(doc_ptr doc);

static void _build_messages(doc_ptr doc);
static void _build_options(doc_ptr doc);

/********************************** Page 1 ************************************/
static void _build_general1(doc_ptr doc)
{
    race_t          *race_ptr = get_race();
    class_t         *class_ptr = get_class();
    personality_ptr  pers_ptr = get_personality();

    doc_printf(doc, " Name       : <color:B>%s</color>\n", player_name);
    doc_printf(doc, " Sex        : <color:B>%s</color>\n", sp_ptr->title);
    doc_printf(doc, " Personality: <color:B>%s</color>\n", pers_ptr->name);

    if (race_ptr->mimic)
        doc_printf(doc, " Race       : <color:B>[%s]</color>\n", race_ptr->name);
    else
        doc_printf(doc, " Race       : <color:B>%s</color>\n", race_ptr->name);

    if (race_ptr->subname)
    {
        if (p_ptr->prace == RACE_MON_RING)
            doc_printf(doc, " Controlling: <color:B>%-27.27s</color>\n", race_ptr->subname);
        else if (p_ptr->prace == RACE_MON_MIMIC)
        {
            if (p_ptr->current_r_idx == MON_MIMIC)
                doc_printf(doc, " Mimicking  : <color:B>%-27.27s</color>\n", "Nothing");
            else
                doc_printf(doc, " Mimicking  : <color:B>%-27.27s</color>\n", race_ptr->subname);
        }
        else
            doc_printf(doc, " Subrace    : <color:B>%-27.27s</color>\n", race_ptr->subname);
    }
    else
        doc_printf(doc, " Subrace    : <color:B>%-27.27s</color>\n", "None");

    doc_printf(doc, " Class      : <color:B>%s</color>\n", class_ptr->name);

    /* Assume Subclass and Magic are mutually exclusive ... */
    if (class_ptr->subname)
        doc_printf(doc, " Subclass   : <color:B>%-27.27s</color>\n", class_ptr->subname);
    else if (p_ptr->pclass == CLASS_WARLOCK)
        doc_printf(doc, " Subclass   : <color:B>%-27.27s</color>\n", pact_info[p_ptr->psubclass].title);
    else if (p_ptr->pclass == CLASS_WEAPONMASTER)
        doc_printf(doc, " Subclass   : <color:B>%-27.27s</color>\n", weaponmaster_speciality_name(p_ptr->psubclass));
    else if (p_ptr->prace == RACE_MON_DRAGON)
    {
        dragon_realm_ptr realm = dragon_get_realm(p_ptr->dragon_realm);
        doc_printf(doc, " Realm      : <color:B>%-27.27s</color>\n", realm->name);
    }
    else if (p_ptr->realm1)
    {
        if (p_ptr->realm2)
            doc_printf(doc, " Realm      : <color:B>%s, %s</color>\n", realm_names[p_ptr->realm1], realm_names[p_ptr->realm2]);
        else
            doc_printf(doc, " Realm      : <color:B>%s</color>\n", realm_names[p_ptr->realm1]);
    }
    else
        doc_newline(doc);

    if ((p_ptr->pclass == CLASS_CHAOS_WARRIOR) || mut_present(MUT_CHAOS_GIFT))
        doc_printf(doc, " Patron     : <color:B>%s</color>\n", chaos_patrons[p_ptr->chaos_patron]);
    else
        doc_newline(doc);


    doc_printf(doc, " Level      : <color:G>%8d</color>\n", p_ptr->lev);
    if (p_ptr->prace == RACE_ANDROID)
    {
        doc_printf(doc, " Construct  : <color:%c>%8d</color>\n", p_ptr->exp >= p_ptr->max_exp ? 'G' : 'y', p_ptr->exp);
        doc_newline(doc);
    }
    else
    {
        doc_printf(doc, " Cur Exp    : <color:%c>%8d</color>\n", p_ptr->exp >= p_ptr->max_exp ? 'G' : 'y', p_ptr->exp);
        doc_printf(doc, " Max Exp    : <color:G>%8d</color>\n", p_ptr->max_exp);
    }
    doc_printf(doc, " Adv Exp    : <color:G>%8.8s</color>\n", p_ptr->lev >= PY_MAX_LEVEL ? "*****" : format("%d", exp_requirement(p_ptr->lev)));
    doc_newline(doc);
    doc_newline(doc);

    doc_printf(doc, " Gold       : <color:G>%8d</color>\n", p_ptr->au);
    doc_printf(doc, " Kills      : <color:G>%8d</color>\n", ct_kills());
    doc_printf(doc, " Uniques    : <color:G>%8d</color>\n", ct_uniques());
    doc_printf(doc, " Artifacts  : <color:G>%8.8s</color>\n",
                            no_artifacts ? "N/A" : format("%d+%d" , ct_artifacts(), stats_rand_art_counts.found));
    doc_newline(doc);

    {
        int day, hour, min;
        extract_day_hour_min(&day, &hour, &min);

        doc_printf(doc, " Game Day   : <color:G>%8d</color>\n", day);
        doc_printf(doc, " Game Time  : <color:G>%8.8s</color>\n", format("%d:%02d", hour, min));
    }

    update_playtime();
    doc_printf(doc, " Play Time  : <color:G>%8.8s</color>\n",
                            format("%.2lu:%.2lu", playtime/(60*60), (playtime/60)%60));
}

static void _build_general2(doc_ptr doc)
{
    string_ptr s = string_alloc();
    char       buf[255];
    int        i;

    doc_insert(doc, "   ========== Stats ==========\n");
    for (i = 0; i < MAX_STATS; i++)
    {
        if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
            doc_printf(doc, "<tab:9>%3.3s", stat_names_reduced[i]);
        else
            doc_printf(doc, "<tab:9>%3.3s", stat_names[i]);

        if (p_ptr->stat_max[i] == p_ptr->stat_max_max[i])
            doc_insert(doc, "! : ");
        else
            doc_insert(doc, "  : ");

        cnv_stat(p_ptr->stat_use[i], buf);
        doc_printf(doc, "<color:%c>%9.9s</color>\n", p_ptr->stat_use[i] < p_ptr->stat_top[i] ? 'y' : 'G', buf);
    }

    doc_newline(doc);

    string_clear(s);
    string_printf(s, "%d/%d", p_ptr->chp , p_ptr->mhp);
    doc_printf(doc, "<tab:9>HP   : <color:%c>%9.9s</color>\n",
                    p_ptr->chp >= p_ptr->mhp ? 'G' :
                        p_ptr->chp > (p_ptr->mhp * hitpoint_warn) / 10 ? 'y' : 'r',
                    string_buffer(s));

    string_clear(s);
    string_printf(s, "%d/%d", p_ptr->csp , p_ptr->msp);
    doc_printf(doc, "<tab:9>SP   : <color:%c>%9.9s</color>\n",
                    p_ptr->csp >= p_ptr->msp ? 'G' :
                        p_ptr->csp > (p_ptr->msp * mana_warn) / 10 ? 'y' : 'r',
                    string_buffer(s));

    doc_printf(doc, "<tab:9>AC   : <color:G>%9d</color>\n", p_ptr->dis_ac + p_ptr->dis_to_a);

    /* Dump speed ... What a monster! */
    {
        int  tmp_speed = 0;
        byte attr;
        int  speed = p_ptr->pspeed-110;

        /* Hack -- Visually "undo" the Search Mode Slowdown */
        if (p_ptr->action == ACTION_SEARCH) speed += 10;

        if (speed > 0)
        {
            if (!p_ptr->riding)
                attr = TERM_L_GREEN;
            else
                attr = TERM_GREEN;
        }
        else if (i == 0)
        {
            if (!p_ptr->riding)
                attr = TERM_L_BLUE;
            else
                attr = TERM_GREEN;
        }
        else
        {
            if (!p_ptr->riding)
                attr = TERM_L_UMBER;
            else
                attr = TERM_RED;
        }

        if (!p_ptr->riding)
        {
            if (IS_FAST()) tmp_speed += 10;
            if (p_ptr->slow) tmp_speed -= 10;
            if (IS_LIGHT_SPEED()) tmp_speed = 99;
        }
        else
        {
            if (MON_FAST(&m_list[p_ptr->riding])) tmp_speed += 10;
            if (MON_SLOW(&m_list[p_ptr->riding])) tmp_speed -= 10;
        }

        string_clear(s);
        if (tmp_speed)
        {
            string_printf(s, "%+d%+d", speed-tmp_speed, tmp_speed);
            if (tmp_speed > 0)
                attr = TERM_YELLOW;
            else
                attr = TERM_VIOLET;
        }
        else
        {
            string_printf(s, "%+d", speed);
        }

        doc_printf(doc, "<tab:9>Speed: <color:%c>%9.9s</color>\n", attr_to_attr_char(attr), string_buffer(s));
    }

    doc_newline(doc);
    doc_insert(doc, "   ========== Skills =========\n");

    {
        skills_t        skills = p_ptr->skills;
        int             slot = equip_find_object(TV_BOW, SV_ANY);
        skill_desc_t    desc = {0};

        /* Patch Up Skills a bit */
        skills.thn += p_ptr->to_h_m * BTH_PLUS_ADJ;
        if (slot)
        {
            object_type *bow = equip_obj(slot);
            if (bow)
                skills.thb += (p_ptr->shooter_info.to_h + bow->to_h) * BTH_PLUS_ADJ;
        }
        if (!skills.stl)
            skills.stl = -1; /* Force "Very Bad" */

        /* Display */
        desc = skills_describe(skills.thn, 12);
        doc_printf(doc, "   Melee      : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.thb, 12);
        doc_printf(doc, "   Ranged     : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.sav, 7);
        doc_printf(doc, "   SavingThrow: <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.stl, 1);
        doc_printf(doc, "   Stealth    : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.fos, 6);
        doc_printf(doc, "   Perception : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.srh, 6);
        doc_printf(doc, "   Searching  : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.dis, 8);
        doc_printf(doc, "   Disarming  : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);

        desc = skills_describe(skills.dev, 6);
        doc_printf(doc, "   Device     : <color:%c>%s</color>\n", attr_to_attr_char(desc.color), desc.desc);
    }

    string_free(s);
}

static void _build_general(doc_ptr doc)
{
    doc_ptr cols[2];

    cols[0] = doc_alloc(40);
    cols[1] = doc_alloc(40);

    _build_general1(cols[0]);
    _build_general2(cols[1]);
    doc_insert_cols(doc, cols, 2, 0);

    doc_free(cols[0]);
    doc_free(cols[1]);
}

/********************************** Equipment *********************************/
static void _equippy_chars(doc_ptr doc, int col)
{
    if (equippy_chars)
    {
        int i;
        doc_printf(doc, "<tab:%d>", col);
        for (i = 0; i < equip_count(); i++)
        {
            int          slot = EQUIP_BEGIN + i;
            object_type *o_ptr = equip_obj(slot);

            if (o_ptr)
            {
                byte a;
                char c;

                a = object_attr(o_ptr);
                c = object_char(o_ptr);

                doc_insert_char(doc, a, c);
            }
            else
                doc_insert_char(doc, TERM_WHITE, ' ');
        }
        doc_newline(doc);
    }
}

static void _equippy_heading_aux(doc_ptr doc, cptr heading, int col)
{
    int i;
    doc_printf(doc, " <color:G>%-11.11s</color><tab:%d>", heading, col);
    for (i = 0; i < equip_count(); i++)
        doc_insert_char(doc, TERM_WHITE, 'a' + i);
    doc_insert_char(doc, TERM_WHITE, '@');
}

static void _equippy_heading(doc_ptr doc, cptr heading, int col)
{
    _equippy_heading_aux(doc, heading, col);
    doc_newline(doc);
}

typedef struct {
    u32b py_flgs[TR_FLAG_SIZE];
    u32b tim_py_flgs[TR_FLAG_SIZE];
    u32b obj_flgs[EQUIP_MAX_SLOTS][TR_FLAG_SIZE];
} _flagzilla_t, *_flagzilla_ptr;

static _flagzilla_ptr _flagzilla_alloc(void)
{
    _flagzilla_ptr flagzilla = malloc(sizeof(_flagzilla_t));
    int            i;

    memset(flagzilla, 0, sizeof(_flagzilla_t));

    player_flags(flagzilla->py_flgs);
    tim_player_flags(flagzilla->tim_py_flgs);
    for (i = 0; i < equip_count(); i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);

        if (o_ptr)
            object_flags_known(o_ptr, flagzilla->obj_flgs[i]);
    }

    return flagzilla;
}

static void _flagzilla_free(_flagzilla_ptr flagzilla)
{
    free(flagzilla);
}

static void _build_res_flags(doc_ptr doc, int which, _flagzilla_ptr flagzilla)
{
    int i;
    int flg = res_get_object_flag(which);
    int im_flg = res_get_object_immune_flag(which);
    int vuln_flg = res_get_object_vuln_flag(which);
    int pct = res_pct_known(which);
    char color = 'w';

    doc_printf(doc, " %-11.11s: ", res_name(which));

    for (i = 0; i < equip_count(); i++)
    {
        if (im_flg != TR_INVALID && have_flag(flagzilla->obj_flgs[i], im_flg))
            doc_insert_char(doc, TERM_VIOLET, '*');
        else if (vuln_flg != TR_INVALID && have_flag(flagzilla->obj_flgs[i], vuln_flg) && have_flag(flagzilla->obj_flgs[i], flg))
            doc_insert_char(doc, TERM_L_DARK, '.');
        else if (vuln_flg != TR_INVALID && have_flag(flagzilla->obj_flgs[i], vuln_flg))
            doc_insert_char(doc, TERM_L_RED, '-');
        else if (have_flag(flagzilla->obj_flgs[i], flg))
            doc_insert_char(doc, TERM_WHITE, '+');
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }

    if (im_flg != TR_INVALID && have_flag(flagzilla->py_flgs, im_flg))
        doc_insert_char(doc, TERM_VIOLET, '*');
    else if (im_flg != TR_INVALID && have_flag(flagzilla->tim_py_flgs, im_flg))
        doc_insert_char(doc, TERM_YELLOW, '*');
    else if (have_flag(flagzilla->tim_py_flgs, flg))
    {
        if (vuln_flg != TR_INVALID && have_flag(flagzilla->py_flgs, vuln_flg))
            doc_insert_char(doc, TERM_ORANGE, '#');
        else
            doc_insert_char(doc, TERM_YELLOW, '#');
    }
    else if (vuln_flg != TR_INVALID && have_flag(flagzilla->py_flgs, vuln_flg))
        doc_insert_char(doc, TERM_RED, 'v');
    else if (have_flag(flagzilla->py_flgs, flg))
        doc_insert_char(doc, TERM_WHITE, '+');
    else
        doc_insert_char(doc, TERM_L_DARK, '.');

    if (pct == 100)
        color = 'v';
    else if (pct < 0)
        color = 'D';
    else if (res_is_low(which))
    {
        if (pct >= 72)
            color =  'r';
        else if (pct >= 65)
            color =  'R';
        else if (pct >= 50)
            color =  'y';
    }
    else
    {
        if (pct >= 45)
            color =  'r';
        else if (pct >= 40)
            color =  'R';
        else if (pct >= 30)
            color =  'y';
    }

    if (which == RES_FEAR && res_pct(RES_FEAR) < 100)
        doc_printf(doc, " %3dx", res_ct_known(which));
    else
        doc_printf(doc, " <color:%c>%3d%%</color>", color, pct);
    doc_newline(doc);
}

static void _build_curse_flags(doc_ptr doc, cptr name)
{
    int i;
    doc_printf(doc, " %-11.11s: ", name);
    for (i = 0; i < equip_count(); i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);

        if (o_ptr)
        {
            if (object_is_cursed(o_ptr) && !(o_ptr->ident & IDENT_FULL))
                doc_insert_char(doc, TERM_YELLOW, '?');
            else if (o_ptr->curse_flags & TRC_PERMA_CURSE)
                doc_insert_char(doc, TERM_VIOLET, '*');
            else if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
                doc_insert_char(doc, TERM_L_RED, '+');
            else if (o_ptr->curse_flags & TRC_CURSED)
                doc_insert_char(doc, TERM_WHITE, '+');
            else
                doc_insert_char(doc, TERM_L_DARK, '.');
        }
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }
    doc_insert_char(doc, TERM_L_DARK, '.');
    doc_newline(doc);
}

static void _build_slays_imp(doc_ptr doc, cptr name, int flg, int kill_flg, _flagzilla_ptr flagzilla)
{
    int i;
    doc_printf(doc, " %-11.11s: ", name);
    for (i = 0; i < equip_count(); i++)
    {
        if (kill_flg != TR_INVALID && have_flag(flagzilla->obj_flgs[i], kill_flg))
            doc_insert_char(doc, TERM_RED, '*');
        else if (have_flag(flagzilla->obj_flgs[i], flg))
            doc_insert_char(doc, TERM_WHITE, '+');
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }
    if (kill_flg != TR_INVALID && have_flag(flagzilla->tim_py_flgs, kill_flg))
        doc_insert_char(doc, TERM_YELLOW, '*');
    else if (have_flag(flagzilla->tim_py_flgs, flg))
        doc_insert_char(doc, TERM_YELLOW, '+');
    else if (kill_flg != TR_INVALID && have_flag(flagzilla->py_flgs, kill_flg))
        doc_insert_char(doc, TERM_RED, '*');
    else if (have_flag(flagzilla->py_flgs, flg))
        doc_insert_char(doc, TERM_WHITE, '+');
    else
        doc_insert_char(doc, TERM_L_DARK, '.');

    doc_newline(doc);
}

static int _build_flags_imp(doc_ptr doc, cptr name, int flg, int dec_flg, _flagzilla_ptr flagzilla)
{
    int result = 0;
    int i;
    doc_printf(doc, " %-11.11s: ", name);
    for (i = 0; i < equip_count(); i++)
    {
        if (have_flag(flagzilla->obj_flgs[i], flg))
        {
            doc_insert_char(doc, TERM_WHITE, '+');
            result++;
        }
        else if (dec_flg != TR_INVALID && have_flag(flagzilla->obj_flgs[i], dec_flg))
        {
            doc_insert_char(doc, TERM_L_RED, '-');
            result--;
        }
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }
    if (have_flag(flagzilla->tim_py_flgs, flg))
    {
       doc_insert_char(doc, TERM_YELLOW, '#');
       result++;
    }
    else if (have_flag(flagzilla->py_flgs, flg))
    {
        doc_insert_char(doc, TERM_WHITE, '+');
        result++;
    }
    else
        doc_insert_char(doc, TERM_L_DARK, '.');

    return result;
}

static void _build_flags_aura(doc_ptr doc, cptr name, int flg, _flagzilla_ptr flagzilla)
{
    if (_build_flags_imp(doc, name, flg, TR_INVALID, flagzilla))
    {
        doc_printf(doc, " %dd%d+2", 1 + p_ptr->lev/10, 2 + p_ptr->lev/ 10);
    }
    doc_newline(doc);
}

static void _build_flags(doc_ptr doc, cptr name, int flg, int dec_flg, _flagzilla_ptr flagzilla)
{
    _build_flags_imp(doc, name, flg, dec_flg, flagzilla);
    doc_newline(doc);
}

static void _build_flags1(doc_ptr doc, _flagzilla_ptr flagzilla)
{
    int i;
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Resistances", 14);

    for (i = RES_BEGIN; i < RES_END; i++)
        _build_res_flags(doc, i, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Auras", 14);
    _build_flags_aura(doc, "Aura Elec", TR_SH_ELEC, flagzilla);
    _build_flags_aura(doc, "Aura Fire", TR_SH_FIRE, flagzilla);
    _build_flags_aura(doc, "Aura Cold", TR_SH_COLD, flagzilla);
    _build_flags_aura(doc, "Aura Shards", TR_SH_SHARDS, flagzilla);
    _build_flags(doc, "Revenge", TR_SH_REVENGE, TR_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Slays", 14);
    _build_slays_imp(doc, "Slay Evil", TR_SLAY_EVIL, TR_KILL_EVIL, flagzilla);
    _build_slays_imp(doc, "Slay Undead", TR_SLAY_UNDEAD, TR_KILL_UNDEAD, flagzilla);
    _build_slays_imp(doc, "Slay Demon", TR_SLAY_DEMON, TR_KILL_DEMON, flagzilla);
    _build_slays_imp(doc, "Slay Dragon", TR_SLAY_DRAGON, TR_KILL_DRAGON, flagzilla);
    _build_slays_imp(doc, "Slay Human", TR_SLAY_HUMAN, TR_KILL_HUMAN, flagzilla);
    _build_slays_imp(doc, "Slay Animal", TR_SLAY_ANIMAL, TR_KILL_ANIMAL, flagzilla);
    _build_slays_imp(doc, "Slay Orc", TR_SLAY_ORC, TR_KILL_ORC, flagzilla);
    _build_slays_imp(doc, "Slay Troll", TR_SLAY_TROLL, TR_KILL_TROLL, flagzilla);
    _build_slays_imp(doc, "Slay Giant", TR_SLAY_GIANT, TR_KILL_GIANT, flagzilla);
    _build_slays_imp(doc, "Slay Good", TR_SLAY_GOOD, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Slay Living", TR_SLAY_LIVING, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Acid Brand", TR_BRAND_ACID, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Elec Brand", TR_BRAND_ELEC, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Fire Brand", TR_BRAND_FIRE, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Cold Brand", TR_BRAND_COLD, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Pois Brand", TR_BRAND_POIS, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Mana Brand", TR_FORCE_WEAPON, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Sharpness", TR_VORPAL, TR_VORPAL2, flagzilla);
    _build_slays_imp(doc, "Quake", TR_IMPACT, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Vampiric", TR_VAMPIRIC, TR_INVALID, flagzilla);
    _build_slays_imp(doc, "Chaotic", TR_CHAOTIC, TR_INVALID, flagzilla);
    _build_flags(doc, "Add Blows", TR_BLOWS, TR_DEC_BLOWS, flagzilla);
    _build_flags(doc, "Blessed", TR_BLESSED, TR_INVALID, flagzilla);
    _build_flags(doc, "Riding", TR_RIDING, TR_INVALID, flagzilla);
    _build_flags(doc, "Tunnel", TR_TUNNEL, TR_INVALID, flagzilla);
    _build_flags(doc, "Throwing", TR_THROW, TR_INVALID, flagzilla);
}

static void _build_flags2(doc_ptr doc, _flagzilla_ptr flagzilla)
{
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Abilities", 14);

    _build_flags(doc, "Speed", TR_SPEED, TR_DEC_SPEED, flagzilla);
    _build_flags(doc, "Free Act", TR_FREE_ACT, TR_INVALID, flagzilla);
    _build_flags(doc, "See Invis", TR_SEE_INVIS, TR_INVALID, flagzilla);
    _build_flags(doc, "Warning", TR_WARNING, TR_INVALID, flagzilla);
    _build_flags(doc, "Slow Digest", TR_SLOW_DIGEST, TR_INVALID, flagzilla);
    _build_flags(doc, "Regenerate", TR_REGEN, TR_INVALID, flagzilla);
    _build_flags(doc, "Levitation", TR_LEVITATION, TR_INVALID, flagzilla);
    _build_flags(doc, "Perm Lite", TR_LITE, TR_INVALID, flagzilla);
    _build_flags(doc, "Reflection", TR_REFLECT, TR_INVALID, flagzilla);
    _build_flags(doc, "Hold Life", TR_HOLD_LIFE, TR_INVALID, flagzilla);
    _build_flags(doc, "Dec Mana", TR_DEC_MANA, TR_INVALID, flagzilla);
    _build_flags(doc, "Easy Spell", TR_EASY_SPELL, TR_INVALID, flagzilla);
    _build_flags(doc, "Anti Magic", TR_NO_MAGIC, TR_INVALID, flagzilla);

    _build_flags_imp(doc, "Magic Skill", TR_MAGIC_MASTERY, TR_DEC_MAGIC_MASTERY, flagzilla);
    if (p_ptr->device_power)
    {
        int pow = device_power_aux(100, p_ptr->device_power) - 100;
        doc_printf(doc, " %+3d%%", pow);
    }
    doc_newline(doc);

    if (_build_flags_imp(doc, "Spell Power", TR_SPELL_POWER, TR_DEC_SPELL_POWER, flagzilla))
    {
        int  pow = spell_power_aux(100, p_ptr->spell_power) - 100;
        doc_printf(doc, " %+3d%%", pow);
    }
    doc_newline(doc);

    if (_build_flags_imp(doc, "Spell Cap", TR_SPELL_CAP, TR_DEC_SPELL_CAP, flagzilla))
    {
        int cap = spell_cap_aux(100, p_ptr->spell_cap) - 100;
        doc_printf(doc, " %+3d%%", cap);
    }
    doc_newline(doc);

    if (_build_flags_imp(doc, "Magic Res", TR_MAGIC_RESISTANCE, TR_INVALID, flagzilla))
        doc_printf(doc, " %+3d%%", p_ptr->magic_resistance);
    doc_newline(doc);

    if (_build_flags_imp(doc, "Infravision", TR_INFRA, TR_INVALID, flagzilla))
        doc_printf(doc, " %3d'", p_ptr->see_infra * 10);
    doc_newline(doc);

    _build_flags(doc, "Stealth", TR_STEALTH, TR_DEC_STEALTH, flagzilla);
    _build_flags(doc, "Searching", TR_SEARCH, TR_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Sustains", 14);
    _build_flags(doc, "Sust Str", TR_SUST_STR, TR_INVALID, flagzilla);
    _build_flags(doc, "Sust Int", TR_SUST_INT, TR_INVALID, flagzilla);
    _build_flags(doc, "Sust Wis", TR_SUST_WIS, TR_INVALID, flagzilla);
    _build_flags(doc, "Sust Dex", TR_SUST_DEX, TR_INVALID, flagzilla);
    _build_flags(doc, "Sust Con", TR_SUST_CON, TR_INVALID, flagzilla);
    _build_flags(doc, "Sust Chr", TR_SUST_CHR, TR_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Detection", 14);
    _build_flags(doc, "Telepathy", TR_TELEPATHY, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Evil", TR_ESP_EVIL, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Nonliv", TR_ESP_NONLIVING, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Good", TR_ESP_GOOD, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Undead", TR_ESP_UNDEAD, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Demon", TR_ESP_DEMON, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Dragon", TR_ESP_DRAGON, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Human", TR_ESP_HUMAN, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Animal", TR_ESP_ANIMAL, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Orc", TR_ESP_ORC, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Troll", TR_ESP_TROLL, TR_INVALID, flagzilla);
    _build_flags(doc, "ESP Giant", TR_ESP_GIANT, TR_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Curses", 14);
    _build_curse_flags(doc, "Cursed");
    _build_flags(doc, "Rnd Tele", TR_TELEPORT, TR_INVALID, flagzilla);
    _build_flags(doc, "No Tele", TR_NO_TELE, TR_INVALID, flagzilla);
    _build_flags(doc, "Drain Exp", TR_DRAIN_EXP, TR_INVALID, flagzilla);
    _build_flags(doc, "Aggravate", TR_AGGRAVATE, TR_INVALID, flagzilla);
    _build_flags(doc, "TY Curse", TR_TY_CURSE, TR_INVALID, flagzilla);
}

static void _build_stats(doc_ptr doc, _flagzilla_ptr flagzilla)
{
    int              i, j;
    char             buf[255];
    race_t          *race_ptr = get_race();
    class_t         *class_ptr = get_class();
    personality_ptr  pers_ptr = get_personality();
    s16b             stats[MAX_STATS] = {0};
    s16b             tim_stats[MAX_STATS] = {0};

    mut_calc_stats(stats);
    if (race_ptr->calc_stats)
        race_ptr->calc_stats(stats);
    if (class_ptr->calc_stats)
        class_ptr->calc_stats(stats);

    tim_player_stats(tim_stats);

    _equippy_chars(doc, 14);
    _equippy_heading_aux(doc, "", 14);
    doc_insert(doc, "   Base  R  C  P  E  Total\n");

    for (i = 0; i < MAX_STATS; i++)
    {
        int flg = TR_STR + i;
        int dec_flg = TR_DEC_STR + i;
        int sust_flg = TR_SUST_STR + i;
        int e_adj = 0;

        if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
            doc_printf(doc, "<tab:7>%3.3s", stat_names_reduced[i]);
        else
            doc_printf(doc, "<tab:7>%3.3s", stat_names[i]);

        if (p_ptr->stat_max[i] == p_ptr->stat_max_max[i])
            doc_insert(doc, "! : ");
        else
            doc_insert(doc, "  : ");

        /* abcdefghijkl */
        for (j = 0; j < equip_count(); j++)
        {
            int          slot = EQUIP_BEGIN + j;
            object_type *o_ptr = equip_obj(slot);

            if (o_ptr)
            {
                int adj = 0;

                if (o_ptr->rune)
                {
                    s16b stats[MAX_STATS] = {0};
                    rune_calc_stats(o_ptr, stats);
                    adj += stats[i];
                }
                if (have_flag(flagzilla->obj_flgs[j], dec_flg))
                    adj = -o_ptr->pval;
                else if (have_flag(flagzilla->obj_flgs[j], flg))
                    adj += o_ptr->pval;

                if (adj)
                {
                    byte a = TERM_WHITE;
                    char c = '*';
                    if (abs(adj) < 10)
                        c = '0' + abs(adj);

                    if (adj < 0)
                        a = TERM_RED;
                    else
                    {
                        a = TERM_L_GREEN;
                        if (have_flag(flagzilla->obj_flgs[j], sust_flg))
                            a = TERM_GREEN;
                    }
                    doc_insert_char(doc, a, c);
                }
                else
                {
                    byte a = TERM_L_DARK;
                    char c = '.';

                    if (have_flag(flagzilla->obj_flgs[j], sust_flg))
                    {
                        a = TERM_GREEN;
                        c = 's';
                    }
                    doc_insert_char(doc, a, c);
                }
                e_adj += adj;
            }
            else
                doc_insert_char(doc, TERM_L_DARK, '.');
        }
        /* @ */
        if (stats[i] + tim_stats[i] != 0)
        {
            byte a = TERM_WHITE;
            char c = '*';
            int  adj = stats[i] + tim_stats[i];

            if (abs(adj) < 10)
                c = '0' + abs(adj);

            if (tim_stats[i])
                a = TERM_YELLOW;
            else if (adj > 0)
            {
                a = TERM_L_GREEN;
                if (have_flag(flagzilla->tim_py_flgs, sust_flg) || have_flag(flagzilla->py_flgs, sust_flg))
                    a = TERM_GREEN;
            }
            else if (adj < 0)
                a = TERM_RED;

            doc_insert_char(doc, a, c);
        }
        else
        {
            byte a = TERM_L_DARK;
            char c = '.';

            if (have_flag(flagzilla->tim_py_flgs, sust_flg))
            {
                a = TERM_YELLOW;
                c = 's';
            }
            else if (have_flag(flagzilla->py_flgs, sust_flg))
            {
                a = TERM_GREEN;
                c = 's';
            }
            doc_insert_char(doc, a, c);
        }

        /* Base R C P E */
        cnv_stat(p_ptr->stat_max[i], buf);
        doc_printf(doc, " <color:B>%6.6s%3d%3d%3d%3d</color>",
                    buf, race_ptr->stats[i], class_ptr->stats[i], pers_ptr->stats[i], e_adj);

        /* Total */
        cnv_stat(p_ptr->stat_top[i], buf);
        doc_printf(doc, " <color:G>%6.6s</color>", buf);

        /* Current */
        if (p_ptr->stat_use[i] < p_ptr->stat_top[i])
        {
            cnv_stat(p_ptr->stat_use[i], buf);
            doc_printf(doc, " <color:y>%6.6s</color>", buf);
        }

        doc_newline(doc);
    }
    doc_newline(doc);
}

static void _build_equipment(doc_ptr doc)
{
    bool old_use_graphics = use_graphics;

    /* It always bugged me that equippy characters didn't show in character dumps!*/
    if (equippy_chars && old_use_graphics)
    {
        use_graphics = FALSE;
        reset_visuals();
    }

    if (equip_count_used())
    {
        int slot, i;
        char o_name[MAX_NLEN];
        _flagzilla_ptr flagzilla = 0;

        doc_insert(doc, "<topic:Equipment>============================= Character <color:keypress>E</color>quipment =============================\n\n");
        for (slot = EQUIP_BEGIN, i = 0; slot < EQUIP_BEGIN + equip_count(); slot++, i++)
        {
            object_type *o_ptr = equip_obj(slot);
            if (!o_ptr) continue;

            object_desc(o_name, o_ptr, OD_COLOR_CODED);
            doc_printf(doc, " %c) <indent><style:indent>%s</style></indent>\n", index_to_label(i), o_name);
        }
        doc_newline(doc);


        /* Flags */
        flagzilla = _flagzilla_alloc();
        {
            doc_ptr cols[2];

            cols[0] = doc_alloc(40);
            cols[1] = doc_alloc(40);
            _build_flags1(cols[0], flagzilla);
            _build_flags2(cols[1], flagzilla);
            doc_insert_cols(doc, cols, 2, 0);
            doc_free(cols[0]);
            doc_free(cols[1]);
        }

        /* Stats */
        _build_stats(doc, flagzilla);

        _flagzilla_free(flagzilla);
    }

    if (equippy_chars && old_use_graphics)
    {
        use_graphics = TRUE;
        reset_visuals();
    }
}

/****************************** Combat ************************************/
static void _build_melee(doc_ptr doc)
{
    if (p_ptr->prace != RACE_MON_RING)
    {
        int i;
        doc_insert(doc, "<topic:Melee>==================================== <color:keypress>M</color>elee ====================================\n\n");
        for (i = 0; i < MAX_HANDS; i++)
        {
            if (p_ptr->weapon_info[i].wield_how == WIELD_NONE) continue;
            if (p_ptr->weapon_info[i].bare_hands)
                monk_display_attack_info(doc, i);
            else
                display_weapon_info(doc, i);
        }

        for (i = 0; i < p_ptr->innate_attack_ct; i++)
        {
            display_innate_attack_info(doc, i);
        }
    }
}

static void _build_shooting(doc_ptr doc)
{
    if (equip_find_object(TV_BOW, SV_ANY) && !prace_is_(RACE_MON_JELLY) && p_ptr->shooter_info.tval_ammo != TV_NO_AMMO)
    {
        doc_insert(doc, "<topic:Shooting>=================================== <color:keypress>S</color>hooting ==================================\n\n");
        display_shooter_info(doc);
    }
}

/****************************** Magic ************************************/
void py_display_powers(doc_ptr doc, spell_info *table, int ct)
{
    int i;
    variant vn, vd, vc, vfm;
    if (!ct) return;

    var_init(&vn);
    var_init(&vd);
    var_init(&vc);
    var_init(&vfm);

    doc_printf(doc, "<topic:Powers>=================================== <color:keypress>P</color>owers ====================================\n\n");
    doc_printf(doc, "<color:G>%-20.20s Lvl Cost Fail %-15.15s Cast Fail</color>\n", "", "Desc");
    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];
        spell_stats_ptr stats = spell_stats(spell);

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_COST_EXTRA, &vc);
        spell->fn(SPELL_FAIL_MIN, &vfm);

        doc_printf(doc, "%-20.20s %3d %4d %3d%% %-15.15s %4d %4d %3d%%\n",
            var_get_string(&vn),
            spell->level, calculate_cost(spell->cost + var_get_int(&vc)), MAX(spell->fail, var_get_int(&vfm)),
            var_get_string(&vd),
            stats->ct_cast, stats->ct_fail,
            spell_stats_fail(stats)
        );
    }

    var_clear(&vn);
    var_clear(&vd);
    var_clear(&vc);
    var_clear(&vfm);

    doc_newline(doc);
}

static void _build_powers(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = 0;
    race_t    *race_ptr = get_race();
    class_t   *class_ptr = get_class();

    if (race_ptr->get_powers)
        ct += (race_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);

    if (class_ptr->get_powers)
        ct += (class_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);

    ct += mut_get_powers(spells + ct, MAX_SPELLS - ct);

    py_display_powers(doc, spells, ct);
}

void py_display_spells(doc_ptr doc, spell_info *table, int ct)
{
    int i;
    variant vn, vd, vc, vfm;

    if (!ct) return;

    var_init(&vn);
    var_init(&vd);
    var_init(&vc);
    var_init(&vfm);

    doc_printf(doc, "<topic:Spells>=================================== <color:keypress>S</color>pells ====================================\n\n");
    doc_printf(doc, "<color:G>%-20.20s Lvl Cost Fail %-15.15s Cast Fail</color>\n", "", "Desc");

    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];
        spell_stats_ptr stats = spell_stats(spell);

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_COST_EXTRA, &vc);
        spell->fn(SPELL_FAIL_MIN, &vfm);

        doc_printf(doc, "%-20.20s %3d %4d %3d%% %-15.15s %4d %4d %3d%%\n",
            var_get_string(&vn),
            spell->level, calculate_cost(spell->cost + var_get_int(&vc)), MAX(spell->fail, var_get_int(&vfm)),
            var_get_string(&vd),
            stats->ct_cast, stats->ct_fail,
            spell_stats_fail(stats)
        );
    }

    var_clear(&vn);
    var_clear(&vd);
    var_clear(&vc);
    var_clear(&vfm);

    doc_newline(doc);
}

static void _build_spells(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = 0;
    race_t    *race_ptr = get_race();
    /*class_t   *class_ptr = get_class_t();*/

    if (race_ptr->get_spells)
        ct += (race_ptr->get_spells)(spells + ct, MAX_SPELLS - ct);

    /* TODO: Some classes prompt the user at this point ...
    if (class_ptr->get_spells)
        ct += (class_ptr->get_spells)(spells + ct, MAX_SPELLS - ct); */

    py_display_spells(doc, spells, ct);
}

/****************************** Miscellaneous ************************************/
static void _build_race_history(doc_ptr doc)
{
    if (p_ptr->old_race1 || p_ptr->old_race2)
    {
        int i;

        doc_printf(doc, "\n You were born as %s.\n", get_race_aux(p_ptr->start_race, 0)->name);
        for (i = 0; i < MAX_RACES; i++)
        {
            if (p_ptr->start_race == i) continue;
            if (i < 32)
            {
                if (!(p_ptr->old_race1 & 1L << i)) continue;
            }
            else
            {
                if (!(p_ptr->old_race2 & 1L << (i-32))) continue;
            }
            doc_printf(doc, " You were a %s before.\n", get_race_aux(i, 0)->name);
        }
        doc_newline(doc);
    }
}

static int _compare_quests(quest_type *left, quest_type *right)
{
    if (left->complev < right->complev)
        return -1;
    if (left->complev > right->complev)
        return 1;
    if (left->level < right->level)
        return -1;
    if (left->level > right->level)
        return 1;
    return 0;
}

static void _build_quests(doc_ptr doc)
{
    int     i, ct;
    vec_ptr v = vec_alloc(NULL);
                          /*v--- 'q' and 'Q' are used to quit the document viewer, and take precedence over navigation keys */
    doc_printf(doc, "<topic:uQuests>==================================== Q<color:keypress>u</color>ests ===================================\n\n");

    /* Completed */
    for (i = 1; i < max_quests; i++)
    {
        quest_type *q_ptr = &quest[i];

        if (q_ptr->status == QUEST_STATUS_FINISHED)
        {
            q_ptr->id = i; /* You'll see why in a minute ... */
            vec_add(v, q_ptr);
        }
    }
    if (vec_length(v))
    {
        vec_sort(v, (vec_cmp_f)_compare_quests);

        doc_printf(doc, "  <color:G>Completed Quests</color>\n");
        ct = 0;
        for (i = 0; i < vec_length(v); i++)
        {
            quest_type *q_ptr = vec_get(v, i);
            if (is_fixed_quest_idx(q_ptr->id))
            {
                int old_quest = p_ptr->inside_quest;

                p_ptr->inside_quest = q_ptr->id;
                init_flags = INIT_ASSIGN;
                process_dungeon_file("q_info.txt", 0, 0, 0, 0);
                p_ptr->inside_quest = old_quest;

                if (q_ptr->flags & QUEST_FLAG_SILENT) continue;
            }

            ct++;

            if (!is_fixed_quest_idx(q_ptr->id) && q_ptr->r_idx)
            {
                if (q_ptr->complev == 0)
                {
                    doc_printf(doc, "  %-40s (Dungeon level: %3d) - (Cancelled)\n",
                        r_name + r_info[q_ptr->r_idx].name,
                        q_ptr->level);
                }
                else
                {
                    doc_printf(doc, "  %-40s (Dungeon level: %3d) - level %2d\n",
                        r_name + r_info[q_ptr->r_idx].name,
                        q_ptr->level,
                        q_ptr->complev);
                }
            }
            else
            {
                doc_printf(doc, "  %-40s (Danger  level: %3d) - level %2d\n",
                    q_ptr->name, q_ptr->level, q_ptr->complev);
            }
        }
        if (!ct)
            doc_printf(doc, "  Nothing.\n");

        doc_newline(doc);
    }

    /* Failed */
    vec_clear(v);
    for (i = 1; i < max_quests; i++)
    {
        quest_type *q_ptr = &quest[i];

        if ( q_ptr->status == QUEST_STATUS_FAILED_DONE
          || q_ptr->status == QUEST_STATUS_FAILED )
        {
            q_ptr->id = i; /* You'll see why in a minute ... */
            vec_add(v, q_ptr);
        }
    }

    if (vec_length(v))
    {
        vec_sort(v, (vec_cmp_f)_compare_quests);

        doc_printf(doc, "  <color:R>Failed Quests</color>\n");
        ct = 0;
        for (i = 0; i < vec_length(v); i++)
        {
            quest_type *q_ptr = vec_get(v, i);
            if (is_fixed_quest_idx(q_ptr->id))
            {
                int old_quest = p_ptr->inside_quest;

                p_ptr->inside_quest = q_ptr->id;
                init_flags = INIT_ASSIGN;
                process_dungeon_file("q_info.txt", 0, 0, 0, 0);
                p_ptr->inside_quest = old_quest;

                if (q_ptr->flags & QUEST_FLAG_SILENT) continue;
            }

            ct++;

            if (!is_fixed_quest_idx(q_ptr->id) && q_ptr->r_idx)
            {
                monster_race *r_ptr = &r_info[q_ptr->r_idx];
                if (r_ptr->flags1 & RF1_UNIQUE)
                {
                    doc_printf(doc, "  %-40s (Dungeon level: %3d) - level %2d\n",
                        r_name + r_ptr->name, q_ptr->level, q_ptr->complev);
                }
                else
                {
                    doc_printf(doc, "  %-40s (Kill %d) (Dungeon level: %3d) - level %2d\n",
                        r_name + r_ptr->name, q_ptr->max_num,
                        q_ptr->level, q_ptr->complev);
                }
            }
            else
            {
                doc_printf(doc, "  %-40s (Danger  level: %3d) - level %2d\n",
                    q_ptr->name, q_ptr->level, q_ptr->complev);
            }
        }
        if (!ct)
            doc_printf(doc, "  Nothing.\n");

        doc_newline(doc);
    }

    doc_newline(doc);
    vec_free(v);

    if (p_ptr->arena_number < 0)
    {
        if (p_ptr->arena_number <= ARENA_DEFEATED_OLD_VER)
        {
            doc_printf(doc, "  <color:G>Arena</color>: <color:v>Defeated</color>\n");
        }
        else
        {
            doc_printf(doc, "  <color:G>Arena</color>: <color:v>Defeated</color> by %s in the %d%s fight\n",
                r_name + r_info[arena_info[-1 - p_ptr->arena_number].r_idx].name,
                -p_ptr->arena_number, get_ordinal_number_suffix(-p_ptr->arena_number));
        }
    }
    else if (p_ptr->arena_number > MAX_ARENA_MONS + 2)
    {
        doc_printf(doc, "  <color:G>Arena</color>: <color:B>True Champion</color>\n");
    }
    else if (p_ptr->arena_number > MAX_ARENA_MONS - 1)
    {
        doc_printf(doc, "  <color:G>Arena</color>: <color:R>Champion</color>\n");
    }
    else
    {
        doc_printf(doc, "  <color:G>Arena</color>: %2d Victor%s\n",
            p_ptr->arena_number > MAX_ARENA_MONS ? MAX_ARENA_MONS : p_ptr->arena_number,
            p_ptr->arena_number > 1 ? "ies" : "y");
    }

    doc_newline(doc);
}

static int _compare_monsters_counts(monster_race *left, monster_race *right)
{
    if (left->r_pkills < right->r_pkills)
        return -1;
    if (left->r_pkills > right->r_pkills)
        return 1;
    if (left->level < right->level)
        return -1;
    if (left->level > right->level)
        return 1;
    if (left->mexp < right->mexp)
        return -1;
    if (left->mexp > right->mexp)
        return 1;
    return 0;
}
static int _compare_monsters(monster_race *left, monster_race *right)
{
    if (left->level < right->level)
        return -1;
    if (left->level > right->level)
        return 1;
    if (left->mexp < right->mexp)
        return -1;
    if (left->mexp > right->mexp)
        return 1;
    if (left->r_pkills < right->r_pkills)
        return -1;
    if (left->r_pkills > right->r_pkills)
        return 1;
    return 0;
}
static void _build_uniques(doc_ptr doc)
{
    int ct = ct_kills();

    if (ct)
    {
        int     i;
        vec_ptr v = vec_alloc(NULL);
        int     ctu;

        doc_printf(doc, "<topic:Kills>================================ Monster <color:keypress>K</color>ills ================================\n\n");

        for (i = 0; i < max_r_idx; i++)
        {
            monster_race *r_ptr = &r_info[i];
            if (r_ptr->flags1 & RF1_UNIQUE)
            {
                if (r_ptr->max_num == 0)
                    vec_add(v, r_ptr);
            }
        }

        ctu = vec_length(v);
        if (ctu)
        {
            doc_printf(doc, "You have defeated %d %s including %d unique monster%s in total.\n\n",
                ct, ct == 1 ? "enemy" : "enemies",
                ctu, ctu == 1 ? "" : "s");

            vec_sort(v, (vec_cmp_f)_compare_monsters);

            doc_printf(doc, "  <color:G>%-40.40s <color:R>%3s</color></color>\n", "Uniques", "Lvl");
            for (i = ctu - 1; i >= 0 && i >= ctu - 20; i--)
            {
                monster_race *r_ptr = vec_get(v, i);
                doc_printf(doc, "  %-40.40s %3d\n", (r_name + r_ptr->name), r_ptr->level);
            }
        }
        else
            doc_printf(doc,"You have defeated %d %s.\n", ct, ct == 1 ? "enemy" : "enemies");

        doc_newline(doc);


        vec_clear(v);
        for (i = 0; i < max_r_idx; i++)
        {
            monster_race *r_ptr = &r_info[i];
            if (!(r_ptr->flags1 & RF1_UNIQUE))
            {
                if (r_ptr->r_pkills)
                    vec_add(v, r_ptr);
            }
        }
        ct = vec_length(v);
        if (ct)
        {
            doc_ptr cols[2] = {0};

            cols[0] = doc_alloc(40);
            cols[1] = doc_alloc(40);

            vec_sort(v, (vec_cmp_f)_compare_monsters);

            doc_printf(cols[0], "  <color:G>%-25.25s <color:R>%3s</color> %5s</color>\n", "Non-uniques", "Lvl", "Count");
            for (i = ct - 1; i >= 0 && i >= ct - 20; i--)
            {
                monster_race *r_ptr = vec_get(v, i);
                doc_printf(cols[0], "  %-25.25s %3d %5d\n", (r_name + r_ptr->name), r_ptr->level, r_ptr->r_pkills);
            }
            doc_newline(cols[0]);

            vec_sort(v, (vec_cmp_f)_compare_monsters_counts);
            doc_printf(cols[1], "  <color:G>%-25.25s %3s <color:R>%5s</color></color>\n", "Non-uniques", "Lvl", "Count");
            for (i = ct - 1; i >= 0 && i >= ct - 20; i--)
            {
                monster_race *r_ptr = vec_get(v, i);
                doc_printf(cols[1], "  %-25.25s %3d %5d\n", (r_name + r_ptr->name), r_ptr->level, r_ptr->r_pkills);
            }
            doc_newline(cols[1]);

            doc_insert_cols(doc, cols, 2, 0);
            doc_free(cols[0]);
            doc_free(cols[1]);
        }

        vec_free(v);
    }
}

static void _build_virtues(doc_ptr doc)
{
    if (enable_virtues)
    {
        doc_printf(doc, "<topic:Virtues>=================================== <color:keypress>V</color>irtues ===================================\n\n");
        virtue_display(doc);
        doc_newline(doc);
    }
}

static void _build_mutations(doc_ptr doc)
{
    if (mut_count(NULL))
    {
        doc_printf(doc, "<topic:Mutations>================================== <color:keypress>M</color>utations ==================================\n\n");
        mut_display(doc);
        doc_newline(doc);
    }
}

static void _build_pets(doc_ptr doc)
{
    int i;
    bool pet = FALSE;
    bool pet_settings = FALSE;
    char pet_name[MAX_NLEN];

    for (i = m_max - 1; i >= 1; i--)
    {
        monster_type *m_ptr = &m_list[i];

        if (!m_ptr->r_idx) continue;
        if (!is_pet(m_ptr)) continue;

        pet_settings = TRUE;
        /*if (!m_ptr->nickname && (p_ptr->riding != i)) continue;*/
        if (!pet)
        {
            doc_printf(doc, "<topic:Pets>==================================== <color:keypress>P</color>ets =====================================\n\n");
            doc_printf(doc, "  <color:G>Leading Pets</color>\n");
            pet = TRUE;
        }
        monster_desc(pet_name, m_ptr, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE | MD_NO_PET_ABBREV);
        doc_printf(doc, "  <indent><style:indent>%s</style></indent>\n", pet_name);
    }

    if (pet_settings)
    {
        doc_printf(doc, "\n  <color:G>Options</color>\n");
        doc_printf(doc, "  Pets open doors:                    %s\n", (p_ptr->pet_extra_flags & PF_OPEN_DOORS) ? "ON" : "OFF");
        doc_printf(doc, "  Pets pick up items:                 %s\n", (p_ptr->pet_extra_flags & PF_PICKUP_ITEMS) ? "ON" : "OFF");
        doc_printf(doc, "  Allow teleport:                     %s\n", (p_ptr->pet_extra_flags & PF_TELEPORT) ? "ON" : "OFF");
        doc_printf(doc, "  Allow cast attack spell:            %s\n", (p_ptr->pet_extra_flags & PF_ATTACK_SPELL) ? "ON" : "OFF");
        doc_printf(doc, "  Allow cast summon spell:            %s\n", (p_ptr->pet_extra_flags & PF_SUMMON_SPELL) ? "ON" : "OFF");
        doc_printf(doc, "  Allow involve player in area spell: %s\n", (p_ptr->pet_extra_flags & PF_BALL_SPELL) ? "ON" : "OFF");

        doc_newline(doc);
    }
}

/****************************** Objects ************************************/
static void _build_inventory(doc_ptr doc)
{
    int i;
    char o_name[MAX_NLEN];

    doc_printf(doc, "<topic:Inventory>============================= Character <color:keypress>I</color>nventory =============================\n\n");

    for (i = 0; i < INVEN_PACK; i++)
    {
        if (!inventory[i].k_idx) break;

        object_desc(o_name, &inventory[i], OD_COLOR_CODED);
        doc_printf(doc, "%c) <indent><style:indent>%s</style></indent>\n", index_to_label(i), o_name);
    }

    doc_newline(doc);
}

static void _build_home(doc_ptr doc)
{
    char o_name[MAX_NLEN];
    store_type  *st_ptr = &town[1].store[STORE_HOME];

    if (st_ptr->stock_num)
    {
        int i;
        int page = 1;

        doc_printf(doc, "<topic:Home>================================ <color:keypress>H</color>ome Inventory ===============================\n");

        for (i = 0; i < st_ptr->stock_num; i++)
        {
            if ((i % 12) == 0)
                doc_printf(doc, "\n ( page %d )\n", page++);
            object_desc(o_name, &st_ptr->stock[i], OD_COLOR_CODED);
            doc_printf(doc, "%c) <indent><style:indent>%s</style></indent>\n", I2A(i%12), o_name);
        }

        doc_newline(doc);
    }
}

static void _build_museum(doc_ptr doc)
{
    char o_name[MAX_NLEN];
    store_type  *st_ptr = &town[1].store[STORE_MUSEUM];

    if (st_ptr->stock_num)
    {
        int i;
        int page = 1;

        doc_printf(doc, "<topic:Museum>==================================== <color:keypress>M</color>useum ===================================\n");

        for (i = 0; i < st_ptr->stock_num; i++)
        {
            if ((i % 12) == 0)
                doc_printf(doc, "\n ( page %d )\n", page++);
            object_desc(o_name, &st_ptr->stock[i], OD_COLOR_CODED);
            doc_printf(doc, "%c) <indent><style:indent>%s</style></indent>\n", I2A(i%12), o_name);
        }

        doc_newline(doc);
    }
}

/****************************** Statistics ************************************/
static void _object_counts_imp(doc_ptr doc, int tval, int sval)
{
    int          k_idx = lookup_kind(tval, sval);
    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->counts.found || k_ptr->counts.bought || k_ptr->counts.used || k_ptr->counts.destroyed)
    {
        doc_printf(
            doc,
            "  %-20.20s %5d %6d %5d %5d",
            k_name + k_ptr->name,
            k_ptr->counts.found,
            k_ptr->counts.bought,
            k_ptr->counts.used,
            k_ptr->counts.destroyed
        );

        switch (tval)
        {
        case TV_WAND: case TV_ROD: case TV_STAFF: case TV_SCROLL:
        {
            int         fail;
            object_type forge;
            object_prep(&forge, lookup_kind(tval, sval));
            fail = device_calc_fail_rate(&forge);
            doc_printf(doc, " %3d.%1d%%", fail / 10, fail % 10);
            break;
        }
        }

        doc_newline(doc);
    }
}

static void _device_counts_imp(doc_ptr doc, int tval, int effect)
{
    device_effect_info_ptr entry = device_get_effect_info(tval, effect);

    if (!entry)
        return;

    if (entry->counts.found || entry->counts.bought || entry->counts.used || entry->counts.destroyed)
    {
        effect_t effect;
        int      fail;

        effect.power = entry->level;
        effect.difficulty = entry->level;
        effect.type = entry->type;

        doc_printf(
            doc,
            "  %-20.20s %5d %6d %5d %5d",
            do_effect(&effect, SPELL_NAME, 0),
            entry->counts.found,
            entry->counts.bought,
            entry->counts.used,
            entry->counts.destroyed
        );

        fail = effect_calc_fail_rate(&effect);
        doc_printf(doc, " %3d.%1d%%", fail / 10, fail % 10);
        doc_newline(doc);
    }
}

typedef bool (*_kind_p)(int k_idx);
bool _kind_is_third_book(int k_idx) {
    if (k_info[k_idx].tval == TV_ARCANE_BOOK) return FALSE;
    if ( TV_LIFE_BOOK <= k_info[k_idx].tval
      && k_info[k_idx].tval <= TV_BURGLARY_BOOK
      && k_info[k_idx].sval == 2 )
    {
        return TRUE;
    }
    return FALSE;
}
bool _kind_is_fourth_book(int k_idx) {
    if (k_info[k_idx].tval == TV_ARCANE_BOOK) return FALSE;
    if ( TV_LIFE_BOOK <= k_info[k_idx].tval
      && k_info[k_idx].tval <= TV_BURGLARY_BOOK
      && k_info[k_idx].sval == 3 )
    {
        return TRUE;
    }
    return FALSE;
}

static bool _kind_is_equipment(int i) {
    int tval = k_info[i].tval;
    if ( kind_is_weapon(i)
      || tval == TV_SHIELD
      || tval == TV_BOW
      || tval == TV_RING
      || tval == TV_AMULET
      || tval == TV_LITE
      || kind_is_body_armor(i)
      || tval == TV_HELM
      || tval == TV_CLOAK
      || kind_is_helm(i)
      || tval == TV_GLOVES
      || tval == TV_BOOTS )
    {
        return TRUE;
    }
    return FALSE;
}

static bool _kind_is_device(int i) {
    int tval = k_info[i].tval;
    if ( tval == TV_WAND
      || tval == TV_ROD
      || tval == TV_STAFF
      || tval == TV_POTION
      || tval == TV_SCROLL )
    {
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_corpse(int k_idx) {
    if (k_info[k_idx].tval == TV_CORPSE && k_info[k_idx].sval == SV_CORPSE)
        return TRUE;
    return FALSE;
}
static bool _kind_is_skeleton(int k_idx) {
    if (k_info[k_idx].tval == TV_CORPSE && k_info[k_idx].sval == SV_SKELETON)
        return TRUE;
    return FALSE;
}
static bool _kind_is_spellbook(int k_idx) {
    if ( TV_LIFE_BOOK <= k_info[k_idx].tval
      && k_info[k_idx].tval <= TV_BURGLARY_BOOK )
    {
        return TRUE;
    }
    return FALSE;
}
static bool _kind_is_other(int k_idx) {
    int tval = k_info[k_idx].tval;
    if ( tval == TV_FOOD
      || _kind_is_corpse(k_idx)
      || _kind_is_skeleton(k_idx)
      || _kind_is_spellbook(k_idx)
      || tval == TV_SHOT
      || tval == TV_ARROW
      || tval == TV_BOLT )
    {
        return TRUE;
    }
    return FALSE;
}

static void _group_counts_imp(doc_ptr doc, _kind_p p, cptr text)
{
    int i;
    counts_t totals = {0};
    for (i = 0; i < max_k_idx; i++)
    {
        if (p(i))
        {
            totals.generated += k_info[i].counts.generated;
            totals.found += k_info[i].counts.found;
            totals.bought += k_info[i].counts.bought;
            totals.used += k_info[i].counts.used;
            totals.destroyed += k_info[i].counts.destroyed;
        }
    }

    if (totals.found || totals.bought || totals.used || totals.destroyed)
    {
        doc_printf(
            doc,
            "  %-20.20s %5d %6d %5d %5d\n",
            text,
            totals.found,
            totals.bought,
            totals.used,
            totals.destroyed
        );
    }
}

static void _group_counts_tval_imp(doc_ptr doc, int tval, cptr text)
{
    int i;
    counts_t totals = {0};
    for (i = 0; i < max_k_idx; i++)
    {
        if (k_info[i].tval == tval)
        {
            totals.generated += k_info[i].counts.generated;
            totals.found += k_info[i].counts.found;
            totals.bought += k_info[i].counts.bought;
            totals.used += k_info[i].counts.used;
            totals.destroyed += k_info[i].counts.destroyed;
        }
    }

    if (totals.found || totals.bought || totals.used || totals.destroyed)
    {
        doc_printf(
            doc,
            "  %-20.20s %5d %6d %5d %5d\n",
            text,
            totals.found,
            totals.bought,
            totals.used,
            totals.destroyed
        );
    }
}

static void _ego_counts_imp(doc_ptr doc, int idx, cptr text)
{
    ego_item_type *e_ptr = &e_info[idx];

    if (e_ptr->counts.found || e_ptr->counts.bought || e_ptr->counts.destroyed)
    {
        doc_printf(
            doc,
            "  %-20.20s %5d %6d %5d\n",
            text,
            e_ptr->counts.found,
            e_ptr->counts.bought,
            e_ptr->counts.destroyed
        );
    }
}

typedef bool (*_mon_p)(int r_idx);
static bool _mon_drops_good(int r_idx)
{
    if (r_info[r_idx].flags1 & RF1_DROP_GOOD)
        return TRUE;
    return FALSE;
}
static bool _mon_drops_great(int r_idx)
{
    if (r_info[r_idx].flags1 & RF1_DROP_GREAT)
        return TRUE;
    return FALSE;
}
static bool _mon_is_animal(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_ANIMAL)
        return TRUE;
    return FALSE;
}
static bool _mon_is_breeder(int r_idx)
{
    if (r_info[r_idx].flags2 & RF2_MULTIPLY)
        return TRUE;
    return FALSE;
}
static bool _mon_is_demon(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_DEMON)
        return TRUE;
    return FALSE;
}
static bool _mon_is_dragon(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_DRAGON)
        return TRUE;
    return FALSE;
}
static bool _mon_is_evil(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_EVIL)
        return TRUE;
    return FALSE;
}
static bool _mon_is_giant(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_GIANT)
        return TRUE;
    return FALSE;
}
static bool _mon_is_good(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_GOOD)
        return TRUE;
    return FALSE;
}
static bool _mon_is_hound(int r_idx)
{
    if (r_info[r_idx].d_char == 'Z')
        return TRUE;
    return FALSE;
}
static bool _mon_is_human(int r_idx)
{
    if (r_info[r_idx].flags2 & RF2_HUMAN)
        return TRUE;
    return FALSE;
}
static bool _mon_is_neutral(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_GOOD)
        return FALSE;
    if (r_info[r_idx].flags3 & RF3_EVIL)
        return FALSE;
    return TRUE;
}
static bool _mon_is_orc(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_ORC)
        return TRUE;
    return FALSE;
}
static bool _mon_is_troll(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_TROLL)
        return TRUE;
    return FALSE;
}
static bool _mon_is_undead(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_UNDEAD)
        return TRUE;
    return FALSE;
}
static bool _mon_is_unique(int r_idx)
{
    if (r_info[r_idx].flags1 & RF1_UNIQUE)
        return TRUE;
    return FALSE;
}
static bool _mon_res_acid(int r_idx)
{
    if (r_info[r_idx].flagsr & (RFR_RES_ACID | RFR_IM_ACID))
        return TRUE;
    return FALSE;
}
static bool _mon_res_elec(int r_idx)
{
    if (r_info[r_idx].flagsr & (RFR_RES_ELEC | RFR_IM_ELEC))
        return TRUE;
    return FALSE;
}
static bool _mon_res_fire(int r_idx)
{
    if (r_info[r_idx].flagsr & (RFR_RES_FIRE | RFR_IM_FIRE))
        return TRUE;
    return FALSE;
}
static bool _mon_res_cold(int r_idx)
{
    if (r_info[r_idx].flagsr & (RFR_RES_COLD | RFR_IM_COLD))
        return TRUE;
    return FALSE;
}
static bool _mon_res_pois(int r_idx)
{
    if (r_info[r_idx].flagsr & (RFR_RES_POIS | RFR_IM_POIS))
        return TRUE;
    return FALSE;
}
static bool _mon_res_conf(int r_idx)
{
    if (r_info[r_idx].flags3 & RF3_NO_CONF)
        return TRUE;
    return FALSE;
}
static void _kill_counts_imp(doc_ptr doc, _mon_p p, cptr text, int total)
{
    int i;
    int kills = 0;
    for (i = 0; i < max_r_idx; i++)
    {
        if (p(i))
        {
            if (_mon_is_unique(i))
            {
                if (r_info[i].max_num == 0)
                    kills++;   /* Perhaps The Cloning Pits is messing up r_akills? */
            }
            else
                kills += r_info[i].r_akills;
        }
    }

    if (kills)
    {
        doc_printf(
            doc,
            "  %-20.20s %5d %3d.%1d%%\n",
            text,
            kills,
            kills*100/total,
            (kills*1000/total)%10
        );
    }
}

static void _build_statistics(doc_ptr doc)
{
    int i, total_kills = ct_kills_all();
    counts_t totals = {0};

    doc_printf(doc, "<topic:Statistics>================================== <color:keypress>S</color>tatistics =================================\n\n");

    /* Gold */
    doc_insert(doc, "             <color:y>    Gold</color>\n");
    doc_printf(doc, "  Found    : <color:w>%8d</color>\n", stats_gold_counts.found);
    doc_printf(doc, "  Selling  : <color:w>%8d</color>\n", stats_gold_counts.selling);
    doc_printf(doc, "  Winnings : <color:w>%8d</color> <color:w>%8d</color>\n",
        stats_gold_counts.winnings,
        stats_gold_counts.found + stats_gold_counts.selling + stats_gold_counts.winnings);
    doc_printf(doc, "  Purchases: <color:w>%8d</color>\n", stats_gold_counts.buying);
    doc_printf(doc, "  Services : <color:w>%8d</color>\n", stats_gold_counts.services);
    doc_printf(doc, "  Stolen   : <color:w>%8d</color> <color:w>%8d</color>\n",
        stats_gold_counts.stolen,
        stats_gold_counts.buying + stats_gold_counts.services + stats_gold_counts.stolen);
    doc_printf(doc, "                      <color:y>%8d</color>\n\n", p_ptr->au);

    /* Objects */
    for (i = 0; i < max_k_idx; i++)
    {
        totals.generated += k_info[i].counts.generated;
        totals.found += k_info[i].counts.found;
        totals.bought += k_info[i].counts.bought;
        totals.used += k_info[i].counts.used;
        totals.destroyed += k_info[i].counts.destroyed;
    }

    doc_printf(doc, "  <color:R>Objects Found    :</color> %6d\n", totals.found);
    doc_printf(doc, "  <color:R>Objects Bought   :</color> %6d\n", totals.bought);
    doc_printf(doc, "  <color:R>Objects Destroyed:</color> %6d\n", totals.destroyed);


    doc_printf(doc, "\n  <color:G>Equipment            Found Bought  Used  Dest</color>\n");
    _group_counts_imp(doc, kind_is_weapon, "Weapons");
    _group_counts_tval_imp(doc, TV_SHIELD, "Shields");
    _group_counts_tval_imp(doc, TV_BOW, "Bows");
    _group_counts_tval_imp(doc, TV_RING, "Rings");
    _group_counts_tval_imp(doc, TV_AMULET, "Amulets");
    _group_counts_tval_imp(doc, TV_LITE, "Lights");
    _group_counts_imp(doc, kind_is_body_armor, "Body Armor");
    _group_counts_tval_imp(doc, TV_CLOAK, "Cloaks");
    _group_counts_imp(doc, kind_is_helm, "Helmets");
    _group_counts_tval_imp(doc, TV_GLOVES, "Gloves");
    _group_counts_tval_imp(doc, TV_BOOTS, "Boots");
    _group_counts_imp(doc, _kind_is_equipment, "Totals");

    doc_printf(doc, "\n  <color:G>Devices              Found Bought  Used  Dest</color>\n");
    _group_counts_tval_imp(doc, TV_WAND, "Wands");
    _group_counts_tval_imp(doc, TV_STAFF, "Staves");
    _group_counts_tval_imp(doc, TV_ROD, "Rods");
    _group_counts_tval_imp(doc, TV_POTION, "Potions");
    _group_counts_tval_imp(doc, TV_SCROLL, "Scrolls");
    _group_counts_imp(doc, _kind_is_device, "Totals");

    doc_printf(doc, "\n  <color:G>Other                Found Bought  Used  Dest</color>\n");
    _group_counts_tval_imp(doc, TV_SHOT, "Shots");
    _group_counts_tval_imp(doc, TV_ARROW, "Arrows");
    _group_counts_tval_imp(doc, TV_BOLT, "Bolts");
    _group_counts_imp(doc, _kind_is_spellbook, "Spellbooks");
    _group_counts_tval_imp(doc, TV_FOOD, "Food");
    _group_counts_imp(doc, _kind_is_corpse, "Corpses");
    _group_counts_imp(doc, _kind_is_skeleton, "Skeletons");
    _group_counts_imp(doc, _kind_is_other, "Totals");

    doc_printf(doc, "\n  <color:G>Potions              Found Bought  Used  Dest</color>\n");
    _object_counts_imp(doc, TV_POTION, SV_POTION_CURE_CRITICAL);
    _object_counts_imp(doc, TV_POTION, SV_POTION_CURING);
    _object_counts_imp(doc, TV_POTION, SV_POTION_SPEED);
    _object_counts_imp(doc, TV_POTION, SV_POTION_HEALING);
    _object_counts_imp(doc, TV_POTION, SV_POTION_STAR_HEALING);
    _object_counts_imp(doc, TV_POTION, SV_POTION_LIFE);
    _object_counts_imp(doc, TV_POTION, SV_POTION_RESTORE_MANA);
    _object_counts_imp(doc, TV_POTION, SV_POTION_RESTORE_EXP);
    _object_counts_imp(doc, TV_POTION, SV_POTION_INC_STR);
    _object_counts_imp(doc, TV_POTION, SV_POTION_INC_INT);
    _object_counts_imp(doc, TV_POTION, SV_POTION_INC_WIS);
    _object_counts_imp(doc, TV_POTION, SV_POTION_INC_DEX);
    _object_counts_imp(doc, TV_POTION, SV_POTION_INC_CON);
    _object_counts_imp(doc, TV_POTION, SV_POTION_INC_CHR);
    _object_counts_imp(doc, TV_POTION, SV_POTION_NEW_LIFE);
    _object_counts_imp(doc, TV_POTION, SV_POTION_EXPERIENCE);
    _group_counts_tval_imp(doc, TV_POTION, "Totals");

    doc_printf(doc, "\n  <color:G>Scrolls              Found Bought  Used  Dest  Fail</color>\n");
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_IDENTIFY);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_STAR_IDENTIFY);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_REMOVE_CURSE);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_STAR_REMOVE_CURSE);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_TELEPORT);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_TELEPORT_LEVEL);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_STAR_DESTRUCTION);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_GENOCIDE);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_MASS_GENOCIDE);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_FOREST_CREATION);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_ACQUIREMENT);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_STAR_ACQUIREMENT);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_ARTIFACT);
    _group_counts_tval_imp(doc, TV_SCROLL, "Totals");

    doc_printf(doc, "\n  <color:G>Wands                Found Bought  Used  Dest  Fail</color>\n");
    if (p_ptr->wizard)
    {
        for (i = 0; ; i++)
        {
        device_effect_info_ptr entry = &wand_effect_table[i];

            if (!entry->type) break;
            _device_counts_imp(doc, TV_WAND, entry->type);
        }
    }
    else
    {
        _device_counts_imp(doc, TV_WAND, EFFECT_STONE_TO_MUD);
        _device_counts_imp(doc, TV_WAND, EFFECT_TELEPORT_AWAY);
        _device_counts_imp(doc, TV_WAND, EFFECT_BREATHE_COLD);
        _device_counts_imp(doc, TV_WAND, EFFECT_BREATHE_FIRE);
        _device_counts_imp(doc, TV_WAND, EFFECT_BREATHE_ONE_MULTIHUED);
        _device_counts_imp(doc, TV_WAND, EFFECT_METEOR);
        _device_counts_imp(doc, TV_WAND, EFFECT_BALL_WATER);
        _device_counts_imp(doc, TV_WAND, EFFECT_BALL_DISINTEGRATE);
        _device_counts_imp(doc, TV_WAND, EFFECT_ROCKET);
        _device_counts_imp(doc, TV_WAND, EFFECT_WALL_BUILDING);
    }
    _group_counts_tval_imp(doc, TV_WAND, "Totals");

    doc_printf(doc, "\n  <color:G>Staves               Found Bought  Used  Dest  Fail</color>\n");
    if (p_ptr->wizard)
    {
        for (i = 0; ; i++)
        {
        device_effect_info_ptr entry = &staff_effect_table[i];

            if (!entry->type) break;
            _device_counts_imp(doc, TV_STAFF, entry->type);
        }
    }
    else
    {
        _device_counts_imp(doc, TV_STAFF, EFFECT_IDENTIFY);
        _device_counts_imp(doc, TV_STAFF, EFFECT_ENLIGHTENMENT);
        _device_counts_imp(doc, TV_STAFF, EFFECT_TELEPATHY);
        _device_counts_imp(doc, TV_STAFF, EFFECT_SPEED);
        _device_counts_imp(doc, TV_STAFF, EFFECT_IDENTIFY_FULL);
        _device_counts_imp(doc, TV_STAFF, EFFECT_DESTRUCTION);
        _device_counts_imp(doc, TV_STAFF, EFFECT_HEAL_CURING);
        _device_counts_imp(doc, TV_STAFF, EFFECT_GENOCIDE);
        _device_counts_imp(doc, TV_STAFF, EFFECT_MANA_STORM);
        _device_counts_imp(doc, TV_STAFF, EFFECT_STARBURST);
        _device_counts_imp(doc, TV_STAFF, EFFECT_DARKNESS_STORM);
        _device_counts_imp(doc, TV_STAFF, EFFECT_RESTORE_MANA);
    }
    _group_counts_tval_imp(doc, TV_STAFF, "Totals");

    doc_printf(doc, "\n  <color:G>Rods                 Found Bought  Used  Dest  Fail</color>\n");
    if (p_ptr->wizard)
    {
        for (i = 0; ; i++)
        {
        device_effect_info_ptr entry = &rod_effect_table[i];

            if (!entry->type) break;
            _device_counts_imp(doc, TV_ROD, entry->type);
        }
    }
    else
    {
        _device_counts_imp(doc, TV_ROD, EFFECT_DETECT_TRAPS);
        _device_counts_imp(doc, TV_ROD, EFFECT_DETECT_DOOR_STAIRS);
        _device_counts_imp(doc, TV_ROD, EFFECT_DETECT_MONSTERS);
        _device_counts_imp(doc, TV_ROD, EFFECT_LITE_AREA);
        _device_counts_imp(doc, TV_ROD, EFFECT_RECALL);
        _device_counts_imp(doc, TV_ROD, EFFECT_DETECT_ALL);
        _device_counts_imp(doc, TV_ROD, EFFECT_ENLIGHTENMENT);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_SOUND);
        _device_counts_imp(doc, TV_ROD, EFFECT_SPEED_HERO);
        _device_counts_imp(doc, TV_ROD, EFFECT_HEAL_CURING_HERO);
        _device_counts_imp(doc, TV_ROD, EFFECT_RESTORING);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_MANA);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_SHARDS);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_CHAOS);
        _device_counts_imp(doc, TV_ROD, EFFECT_CLAIRVOYANCE);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_LITE);
    }
    _group_counts_tval_imp(doc, TV_ROD, "Totals");

    doc_printf(doc, "\n  <color:G>Spellbooks           Found Bought  Used  Dest</color>\n");
    _group_counts_imp(doc, _kind_is_third_book, "Third Spellbooks");
    _group_counts_imp(doc, _kind_is_fourth_book, "Fourth Spellbooks");
    _group_counts_imp(doc, kind_is_book, "Totals");

    /* Egos */
    WIPE(&totals, counts_t);
    for (i = 0; i < max_e_idx; i++)
    {
        totals.generated += e_info[i].counts.generated;
        totals.found += e_info[i].counts.found;
        totals.bought += e_info[i].counts.bought;
        totals.destroyed += e_info[i].counts.destroyed;
    }

    doc_printf(doc, "\n  <color:R>Egos Found    :</color> %6d\n", totals.found);
    doc_printf(doc,   "  <color:R>Egos Bought   :</color> %6d\n", totals.bought);
    doc_printf(doc,   "  <color:R>Egos Destroyed:</color> %6d\n", totals.destroyed);

    doc_printf(doc, "\n  <color:G>Egos                 Found Bought  Dest</color>\n");
    _ego_counts_imp(doc, EGO_RING_SPEED, "Ring of Speed");
    _ego_counts_imp(doc, EGO_RING_DEFENDER, "Ring (Defender)");
    _ego_counts_imp(doc, EGO_AMULET_DEFENDER, "Amulet (Defender)");
    _ego_counts_imp(doc, EGO_BOOTS_ELVENKIND, "Boots of Elvenkind");
    _ego_counts_imp(doc, EGO_BOOTS_SPEED, "Boots of Speed");
    _ego_counts_imp(doc, EGO_BOOTS_FEANOR, "Boots of Feanor");

    /* Monsters */
    doc_printf(doc, "\n  <color:G>Monsters             Kills   Pct</color>\n");
    _kill_counts_imp(doc, _mon_is_animal, "Animals", total_kills);
    _kill_counts_imp(doc, _mon_is_breeder, "Breeders", total_kills);
    _kill_counts_imp(doc, _mon_is_demon, "Demons", total_kills);
    _kill_counts_imp(doc, _mon_is_dragon, "Dragons", total_kills);
    _kill_counts_imp(doc, _mon_is_giant, "Giants", total_kills);
    _kill_counts_imp(doc, _mon_is_hound, "Hounds", total_kills);
    _kill_counts_imp(doc, _mon_is_human, "Humans", total_kills);
    _kill_counts_imp(doc, _mon_is_orc, "Orcs", total_kills);
    _kill_counts_imp(doc, _mon_is_troll, "Trolls", total_kills);
    _kill_counts_imp(doc, _mon_is_undead, "Undead", total_kills);
    _kill_counts_imp(doc, _mon_is_unique, "Uniques", total_kills);
    doc_newline(doc);
    _kill_counts_imp(doc, _mon_is_evil, "Evil Monsters", total_kills);
    _kill_counts_imp(doc, _mon_is_good, "Good Monsters", total_kills);
    _kill_counts_imp(doc, _mon_is_neutral, "Neutral Monsters", total_kills);
    if (0)
    {
        doc_newline(doc);
        _kill_counts_imp(doc, _mon_drops_good, "Good Droppers", total_kills);
        _kill_counts_imp(doc, _mon_drops_great, "Great Droppers", total_kills);
        doc_newline(doc);
        _kill_counts_imp(doc, _mon_res_acid, "Resist Acid", total_kills);
        _kill_counts_imp(doc, _mon_res_elec, "Resist Elec", total_kills);
        _kill_counts_imp(doc, _mon_res_fire, "Resist Fire", total_kills);
        _kill_counts_imp(doc, _mon_res_cold, "Resist Cold", total_kills);
        _kill_counts_imp(doc, _mon_res_pois, "Resist Pois", total_kills);
        _kill_counts_imp(doc, _mon_res_conf, "Resist Conf", total_kills);
    }
    doc_printf(doc, "\n  %-20.20s %5d\n", "Totals", total_kills);

    doc_newline(doc);
}

/****************************** Dungeons ************************************/
void py_display_dungeons(doc_ptr doc)
{
    int i;
    for (i = 1; i < max_d_idx; i++)
    {
        bool conquered = FALSE;

        if (!d_info[i].maxdepth) continue;
        if (!max_dlv[i]) continue;
        if (d_info[i].final_guardian)
        {
            if (!r_info[d_info[i].final_guardian].max_num) conquered = TRUE;
        }
        else if (max_dlv[i] == d_info[i].maxdepth) conquered = TRUE;

        if (conquered)
            doc_printf(doc, "!<color:G>%-16s</color>: level %3d\n", d_name+d_info[i].name, max_dlv[i]);
        else
            doc_printf(doc, " %-16s: level %3d\n", d_name+d_info[i].name, max_dlv[i]);
    }
    doc_newline(doc);

    if (p_ptr->is_dead)
    {
        if (p_ptr->total_winner)
        {
            doc_printf(doc, "<color:v>You %s after winning.</color>\n",
                streq(p_ptr->died_from, "Seppuku") ? "did Seppuku" : "retired from the adventure");
        }
        else if (!dun_level)
        {
            doc_printf(doc, "You were killed by %s in %s.\n", p_ptr->died_from, map_name());
        }
        else if (p_ptr->inside_quest && is_fixed_quest_idx(p_ptr->inside_quest))
        {
            /* Get the quest text */
            /* Bewere that INIT_ASSIGN resets the cur_num. */
            init_flags = INIT_ASSIGN;

            process_dungeon_file("q_info.txt", 0, 0, 0, 0);
            doc_printf(doc, "You were killed by %s in the quest '%s'.\n",
                p_ptr->died_from, quest[p_ptr->inside_quest].name);
        }
        else
        {
            doc_printf(doc, "You were killed by %s on level %d of %s.\n",
                p_ptr->died_from, dun_level, map_name());
        }
    }
    else if (character_dungeon)
    {
        if (!dun_level)
        {
            doc_printf(doc, "Now, you are in %s.\n", map_name());
        }
        else if (p_ptr->inside_quest && is_fixed_quest_idx(p_ptr->inside_quest))
        {
            /* Clear the text */
            /* Must be done before doing INIT_SHOW_TEXT */
            for (i = 0; i < 10; i++)
            {
                quest_text[i][0] = '\0';
            }
            quest_text_line = 0;

            /* Get the quest text */
            init_flags = INIT_SHOW_TEXT;

            process_dungeon_file("q_info.txt", 0, 0, 0, 0);
            doc_printf(doc, "Now, you are in the quest '%s'.\n", quest[p_ptr->inside_quest].name);
        }
        else
        {
            doc_printf(doc, "Now, you are exploring level %d of %s.\n", dun_level, map_name());
        }
    }

    if (p_ptr->last_message)
    {
        if (p_ptr->is_dead)
            doc_printf(doc, "\n Last Message: %s\n", p_ptr->last_message);
        else if (p_ptr->total_winner)
            doc_printf(doc, "\n *Winning* Message: %s\n", p_ptr->last_message);
    }
    doc_newline(doc);
}

static void _build_dungeons(doc_ptr doc)
{
    doc_printf(doc, "<topic:Dungeons>=================================== <color:keypress>D</color>ungeons ==================================\n\n");
    py_display_dungeons(doc);
}

/****************************** Messages ************************************/
static void _build_messages(doc_ptr doc)
{
    int i;
    int current_turn = 0;
    int current_row = 0;

    doc_insert(doc, "<topic:LastMessages>================================ <color:keypress>L</color>ast Messages ================================\n");
    doc_insert(doc, "<style:normal>");
    for (i = MIN(msg_count() - 1, 30); i >= 0; i--)
    {
        msg_ptr m = msg_get(i);

        if (m->turn != current_turn)
        {
            if (doc_cursor(doc).y > current_row + 1)
                doc_newline(doc);
            current_turn = m->turn;
            current_row = doc_cursor(doc).y;
        }

        doc_insert_text(doc, m->color, string_buffer(m->msg));
        if (m->count > 1)
        {
            char buf[10];
            sprintf(buf, " <x%d>", m->count);
            doc_insert_text(doc, m->color, buf);
        }
        doc_newline(doc);
    }
    doc_insert(doc, "</style>\n");
}

/******************************** Options ************************************/
static cptr _game_mode_text[GAME_MODE_MAX] = {
    "<color:G>Beginner</color>",
    "Normal",
    "<color:R>Real Life</color>",
    "<color:r>Monster</color>"
};
static void _build_options(doc_ptr doc)
{
    doc_printf(doc, "<topic:Options>=================================== <color:keypress>O</color>ptions ===================================\n\n");

    if (game_mode != GAME_MODE_NORMAL)
        doc_printf(doc, " Game Mode:          %s\n", _game_mode_text[game_mode]);

    doc_printf(doc, " Preserve Mode:      %s\n", preserve_mode ? "On" : "Off");

    doc_printf(doc, " Small Levels:       %s\n", ironman_small_levels ? "*Always*" :
                                                    always_small_levels ? "Always" :
                                                    small_levels ? "Sometimes" : "Never");

    if (easy_id)
        doc_printf(doc, " Easy Identify:      On\n");

    if (no_wilderness)
        doc_printf(doc, " Wilderness:         Off\n");

    if (ironman_shops)
        doc_printf(doc, " No Shops:           On\n");

    if (ironman_downward)
        doc_printf(doc, " Diving Only:        On\n");

    if (ironman_rooms)
        doc_printf(doc, " Unusual Rooms:      On\n");

    if (ironman_nightmare)
        doc_printf(doc, " Nightmare Mode:     On\n");

    doc_printf(doc, " Arena Levels:       %s\n", ironman_empty_levels ? "*Always*" :
                                                    empty_levels ? "Sometimes" : "Never");

    if (ironman_quests)
        doc_printf(doc, " Ironman Quests:     Enabled\n");

    if (no_artifacts)
        doc_printf(doc, " No Artifacts:       Enabled\n");
    else if (random_artifacts)
        doc_printf(doc, " Random Artifacts:   Enabled\n");

    if (no_egos)
        doc_printf(doc, " No Egos:            Enabled\n");

    if (p_ptr->noscore)
        doc_printf(doc, "\n <color:v>You have done something illegal.</color>\n");

    doc_newline(doc);
}

/****************************** Character Sheet ************************************/
void py_display_character_sheet(doc_ptr doc)
{
    doc_insert(doc, "<style:wide>  [PosChengband <$:version> Character Dump]\n");
    if (p_ptr->total_winner)
        doc_insert(doc, "              <color:B>***WINNER***</color>\n");
    else if (p_ptr->is_dead)
        doc_insert(doc, "              <color:v>***LOSER***</color>\n");
    else
        doc_newline(doc);
    doc_newline(doc);

    _build_general(doc);
    _build_equipment(doc);
    _build_melee(doc);
    _build_shooting(doc);
    _build_powers(doc);
    _build_spells(doc);

    {
        class_t *class_ptr = get_class();
        race_t  *race_ptr = get_race();
        if (class_ptr->character_dump)
            (class_ptr->character_dump)(doc);
        if (race_ptr && race_ptr->character_dump)
            race_ptr->character_dump(doc);
    }

    _build_dungeons(doc);
    _build_quests(doc);
    _build_uniques(doc);
    _build_virtues(doc);
    _build_race_history(doc);
    _build_mutations(doc);
    _build_pets(doc);
    _build_inventory(doc);
    _build_home(doc);
    _build_museum(doc);
    _build_statistics(doc);
    _build_messages(doc);
    _build_options(doc);

    doc_insert(doc, "</style>");
}

static int _max_depth(void)
{
    int result = 0;
    int i;

    for(i = 1; i < max_d_idx; i++)
    {
        if (!d_info[i].maxdepth) continue;
        if (d_info[i].flags1 & DF1_RANDOM) continue;
        if (!max_dlv[i]) continue;
        result = MAX(result, max_dlv[i]);
    }

    return result;
}

void py_display(void)
{
    doc_ptr    d = doc_alloc(80);
    string_ptr s = string_alloc_format("%s.txt", player_base);
    string_ptr header = string_alloc();

    doc_change_name(d, string_buffer(s));

    string_append_s(header, "<head>\n");
    string_append_s(header, " <meta name='filetype' value='character dump'>\n");
    string_printf(header,  " <meta name='variant' value='%s'>\n", VERSION_NAME);
    string_printf(header,  " <meta name='variant_version' value='%d.%d.%d'>\n", VER_MAJOR, VER_MINOR, VER_PATCH);
    string_printf(header,  " <meta name='character_name' value='%s'>\n", player_name);
    string_printf(header,  " <meta name='race' value='%s'>\n", get_race()->name);
    string_printf(header,  " <meta name='class' value='%s'>\n", get_class()->name);
    string_printf(header,  " <meta name='level' value='%d'>\n", p_ptr->lev);
    string_printf(header,  " <meta name='experience' value='%d'>\n", p_ptr->exp);
    string_printf(header,  " <meta name='turncount' value='%d'>\n", game_turn);
    string_printf(header,  " <meta name='max_depth' value='%d'>\n", _max_depth());
    string_printf(header,  " <meta name='score' value='%d'>\n", p_ptr->exp); /* ?? Does oook need this? */
    string_printf(header,  " <meta name='fame' value='%d'>\n", p_ptr->fame);
    string_append_s(header, "</head>");
    doc_change_html_header(d, string_buffer(header));

    py_display_character_sheet(d);

    screen_save();
    doc_display(d, "Character Sheet", 0);
    screen_load();

    doc_free(d);
    string_free(s);
    string_free(header);
}

/* This is used by the birth process ... Note that there
   is birth code that assumes the location of fields on the screen,
   and probably that should be cleaned up some day */
void py_display_birth(void)
{
    doc_ptr d = doc_alloc(80);
    _build_general(d);
    doc_sync_term(d, doc_range_all(d), doc_pos_create(0, 1));
    doc_free(d);
}
