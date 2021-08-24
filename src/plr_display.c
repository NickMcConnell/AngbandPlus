#include "angband.h"

#include <stdlib.h>
#include <assert.h>

/* Build & Display the "Character Sheet" */

extern void plr_display(void);
extern void plr_display_birth(void);
extern void plr_display_spells(doc_ptr doc, spell_info *table, int ct);
extern void plr_display_powers(doc_ptr doc, spell_info *table, int ct);
extern void plr_display_character_sheet(doc_ptr doc);
extern void plr_display_dungeons(doc_ptr doc);

static void _build_general(doc_ptr doc);
static void _build_equipment(doc_ptr doc);
static void _build_melee(doc_ptr doc);
static void _build_shooting(doc_ptr doc);
static void _build_powers(doc_ptr doc);
static void _build_spells(doc_ptr doc);
static void _build_dungeons(doc_ptr doc);
static void _build_uniques(doc_ptr doc);
static void _build_virtues(doc_ptr doc);
static void _build_race_history(doc_ptr doc);
static void _build_mutations(doc_ptr doc);
static void _build_pets(doc_ptr doc);
static void _build_inventory(doc_ptr doc);
static void _build_quiver(doc_ptr doc);
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
    doc_printf(doc, " Sex        : <color:B>%s</color>\n", sex_info[plr->psex].title);
    doc_printf(doc, " Personality: <color:B>%s</color>\n", pers_ptr->name);

    if (race_ptr->mimic)
        doc_printf(doc, " Race       : <color:B>[%s]</color>\n", race_ptr->name);
    else
        doc_printf(doc, " Race       : <color:B>%s</color>\n", race_ptr->name);

    if (race_ptr->subname)
    {
        if (plr->prace == RACE_MON_RING)
            doc_printf(doc, " Controlling: <color:B>%-26.26s</color>\n", race_ptr->subname);
        else if (plr->prace == RACE_MON_MIMIC)
        {
            if (sym_equals(plr->current_r_idx, "@.mimic"))
                doc_printf(doc, " Mimicking  : <color:B>%-26.26s</color>\n", "Nothing");
            else
                doc_printf(doc, " Mimicking  : <color:B>%-26.26s</color>\n", race_ptr->subname);
        }
        else
            doc_printf(doc, " Subrace    : <color:B>%-26.26s</color>\n", race_ptr->subname);
    }
    else
        doc_printf(doc, " Subrace    : <color:B>%-26.26s</color>\n", "None");

    doc_printf(doc, " Class      : <color:B>%s</color>\n", class_ptr->name);

    /* Assume Subclass and Magic are mutually exclusive ... */
    if (class_ptr->subname)
        doc_printf(doc, " Subclass   : <color:B>%-26.26s</color>\n", class_ptr->subname);
    else if (plr->prace == RACE_MON_DRAGON)
    {
        dragon_realm_ptr realm = dragon_get_realm(plr->dragon_realm);
        doc_printf(doc, " Realm      : <color:B>%-26.26s</color>\n", realm->name);
    }
    else if (plr->realm1)
    {
        if (plr->realm2)
            doc_printf(doc, " Realm      : <color:B>%s, %s</color>\n", realm_names[plr->realm1], realm_names[plr->realm2]);
        else
            doc_printf(doc, " Realm      : <color:B>%s</color>\n", realm_names[plr->realm1]);
    }
    else
        doc_newline(doc);

    if ((plr->pclass == CLASS_CHAOS_WARRIOR) || mut_present(MUT_CHAOS_GIFT))
        doc_printf(doc, " Patron     : <color:B>%s</color>\n", chaos_patrons[plr->chaos_patron]);
    else
        doc_newline(doc);


    doc_printf(doc, " Level      : <color:G>%8d</color>\n", plr->lev);
    if (plr->prace == RACE_ANDROID)
    {
        doc_printf(doc, " Construct  : <color:%c>%8d</color>\n", plr->exp >= plr->max_exp ? 'G' : 'y', plr->exp);
        doc_newline(doc);
    }
    else
    {
        doc_printf(doc, " Cur Exp    : <color:%c>%8d</color>\n", plr->exp >= plr->max_exp ? 'G' : 'y', plr->exp);
        doc_printf(doc, " Max Exp    : <color:G>%8d</color>\n", plr->max_exp);
    }
    doc_printf(doc, " Adv Exp    : <color:G>%8.8s</color>\n", plr->lev >= PY_MAX_LEVEL ? "*****" : format("%d", exp_requirement(plr->lev)));
    doc_newline(doc);
    doc_newline(doc);

    doc_printf(doc, " Gold       : <color:G>%8d</color>\n", plr->au);
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

    doc_printf(doc, " Play Time  : <color:G>%8.8s</color>\n", playtime_display());
}

static void _display_skill(doc_ptr doc, cptr name, int amt, int div)
{
    skill_desc_t desc = skills_describe(amt, div);
    doc_printf(doc, "   %-11.11s: <color:%c>%s</color>", name, attr_to_attr_char(desc.color), desc.desc);
    if (plr->wizard || 0)
        doc_printf(doc, " (%d)", amt);
    doc_newline(doc);
}

static void _build_general2(doc_ptr doc)
{
    str_ptr s = str_alloc();
    char       buf[255];
    int        i;

    doc_insert(doc, "   ========== Stats ==========\n");
    for (i = 0; i < MAX_STATS; i++)
    {
        if (plr->stat_use[i] < plr->stat_top[i])
            doc_printf(doc, "<tab:9>%3.3s", stat_names_reduced[i]);
        else
            doc_printf(doc, "<tab:9>%3.3s", stat_names[i]);

        if (plr->stat_max[i] == plr->stat_max_max[i])
            doc_insert(doc, "! : ");
        else
            doc_insert(doc, "  : ");

        cnv_stat(plr->stat_use[i], buf);
        doc_printf(doc, "<color:%c>%9.9s</color>\n", plr->stat_use[i] < plr->stat_top[i] ? 'y' : 'G', buf);
    }

    doc_newline(doc);

    str_clear(s);
    str_printf(s, "%d/%d", plr->chp , plr->mmhp);
    doc_printf(doc, "<tab:9>HP   : <color:%c>%9.9s</color>\n",
                    plr->chp >= plr->mhp ? 'G' :
                        plr->chp > (plr->mmhp * hitpoint_warn) / 10 ? 'y' : 'r',
                    str_buffer(s));

    str_clear(s);
    str_printf(s, "%d/%d", plr->csp , plr->msp);
    doc_printf(doc, "<tab:9>SP   : <color:%c>%9.9s</color>\n",
                    plr->csp >= plr->msp ? 'G' :
                        plr->csp > (plr->msp * mana_warn) / 10 ? 'y' : 'r',
                    str_buffer(s));

    doc_printf(doc, "<tab:9>AC   : <color:G>%9d</color>\n", plr->dis_ac + plr->dis_to_a);

    /* Dump speed ... What a monster! */
    {
        int  tmp_speed = 0;
        byte attr;
        int  speed = plr->pspeed;

        /* Hack -- Visually "undo" the Search Mode Slowdown */
        if (plr->action == ACTION_SEARCH) speed += 10;

        if (speed > 0)
        {
            if (!plr->riding)
                attr = TERM_L_GREEN;
            else
                attr = TERM_GREEN;
        }
        else if (i == 0)
        {
            if (!plr->riding)
                attr = TERM_L_BLUE;
            else
                attr = TERM_GREEN;
        }
        else
        {
            if (!plr->riding)
                attr = TERM_L_UMBER;
            else
                attr = TERM_RED;
        }

        if (!plr->riding)
        {
            if (plr_tim_find(T_FAST)) tmp_speed += 10;
            if (plr_tim_find(T_SLOW)) tmp_speed -= 10;
            if (plr_tim_find(T_LIGHT_SPEED)) tmp_speed = 99;
        }
        else
        {
            if (mon_tim_find(dun_mon(cave, plr->riding), T_FAST)) tmp_speed += 10;
            if (mon_tim_find(dun_mon(cave, plr->riding), T_SLOW)) tmp_speed -= 10;
        }

        str_clear(s);
        if (tmp_speed)
        {
            str_printf(s, "%+d%+d", speed-tmp_speed, tmp_speed);
            if (tmp_speed > 0)
                attr = TERM_YELLOW;
            else
                attr = TERM_VIOLET;
        }
        else
        {
            str_printf(s, "%+d", speed);
        }

        doc_printf(doc, "<tab:9>Speed: <color:%c>%9.9s</color>\n", attr_to_attr_char(attr), str_buffer(s));
    }

    doc_newline(doc);
    doc_insert(doc, "   ========== Skills =========\n");

    {
        skills_t skills = plr->skills;
        int      slot = equip_find_obj(TV_BOW, SV_ANY);

        /* Patch Up Skills a bit */
        skills.thn += plr->to_h_m * BTH_PLUS_ADJ;
        if (slot)
        {
            object_type *bow = equip_obj(slot);
            if (bow)
                skills.thb += (plr->shooter_info.to_h + bow->to_h) * BTH_PLUS_ADJ;
        }
        if (!skills.stl)
            skills.stl = -1; /* Force "Very Bad" */

        /* Display */
        _display_skill(doc, "Melee", skills.thn, 12);
        _display_skill(doc, "Archery", skills.thb, 12);
        _display_skill(doc, "SavingThrow", skills.sav, 7);
        _display_skill(doc, "Stealth", skills.stl, 1);
        _display_skill(doc, "Perception", skills.fos, 6);
        _display_skill(doc, "Searching", skills.srh, 6);
        _display_skill(doc, "Disarming", skills.dis, 8);
        _display_skill(doc, "Device", skills.dev, 6);
    }

    str_free(s);
}

static void _build_general(doc_ptr doc)
{
    doc_ptr cols[2];

    cols[0] = doc_alloc(41);
    cols[1] = doc_alloc(39);

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
        for (i = 1; i <= equip_max(); i++)
        {
            object_type *o_ptr = equip_obj(i);

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
    for (i = 1; i <= equip_max(); i++)
        doc_insert_char(doc, TERM_WHITE, 'a' + i - 1);
    doc_insert_char(doc, TERM_WHITE, '@');
}

static void _equippy_heading(doc_ptr doc, cptr heading, int col)
{
    _equippy_heading_aux(doc, heading, col);
    doc_newline(doc);
}

typedef struct {
    u32b plr_flgs[OF_ARRAY_SIZE];
    u32b tim_plr_flgs[OF_ARRAY_SIZE];
    u32b obj_flgs[EQUIP_MAX + 1][OF_ARRAY_SIZE];
} _flagzilla_t, *_flagzilla_ptr;

static _flagzilla_ptr _flagzilla_alloc(void)
{
    _flagzilla_ptr flagzilla = malloc(sizeof(_flagzilla_t));
    int            i;

    memset(flagzilla, 0, sizeof(_flagzilla_t));

    player_flags(flagzilla->plr_flgs);
    /*tim_player_flags(flagzilla->tim_plr_flgs);*/
    plr_tim_flags(flagzilla->tim_plr_flgs);
    for (i = 1; i <= equip_max(); i++)
    {
        object_type *o_ptr = equip_obj(i);

        if (o_ptr)
        {
            obj_flags_known(o_ptr, flagzilla->obj_flgs[i]);
            switch (o_ptr->rune)
            {
            case RUNE_ABSORPTION:
                add_flag(flagzilla->obj_flgs[i], OF_MAGIC_RESISTANCE);
                break;
            case RUNE_SHADOW:
                if (obj_is_body_armor(o_ptr) || o_ptr->tval == TV_CLOAK)
                    add_flag(flagzilla->obj_flgs[i], OF_STEALTH);
                break;
            case RUNE_HASTE:
                if (o_ptr->tval == TV_BOOTS)
                    add_flag(flagzilla->obj_flgs[i], OF_SPEED);
                break;
            }
        }
    }

    return flagzilla;
}

static void _flagzilla_free(_flagzilla_ptr flagzilla)
{
    free(flagzilla);
}

static void _build_res_flags(doc_ptr doc, int gf, _flagzilla_ptr flagzilla)
{
    int i;
    int ct = res_ct_known(gf);
    int pct = res_pct_known(gf);
    char color = 'w';

    doc_printf(doc, " %-11.11s: ", res_name(gf));

    for (i = 1; i <= equip_max(); i++)
    {
        if (have_flag(flagzilla->obj_flgs[i], OF_IM_(gf)))
            doc_insert_char(doc, TERM_VIOLET, '*');
        else if (have_flag(flagzilla->obj_flgs[i], OF_VULN_(gf)) && have_flag(flagzilla->obj_flgs[i], OF_RES_(gf)))
            doc_insert_char(doc, TERM_L_DARK, '.');
        else if (have_flag(flagzilla->obj_flgs[i], OF_VULN_(gf)))
            doc_insert_char(doc, TERM_L_RED, '-');
        else if (have_flag(flagzilla->obj_flgs[i], OF_RES_(gf)))
            doc_insert_char(doc, TERM_WHITE, '+');
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }

    if (have_flag(flagzilla->plr_flgs, OF_IM_(gf)))
        doc_insert_char(doc, TERM_VIOLET, '*');
    else if (have_flag(flagzilla->tim_plr_flgs, OF_IM_(gf)))
        doc_insert_char(doc, TERM_YELLOW, '*');
    else if (have_flag(flagzilla->tim_plr_flgs, OF_RES_(gf)))
    {
        if (have_flag(flagzilla->plr_flgs, OF_VULN_(gf)))
            doc_insert_char(doc, TERM_ORANGE, '#');
        else
            doc_insert_char(doc, TERM_YELLOW, '#');
    }
    else if (have_flag(flagzilla->plr_flgs, OF_VULN_(gf)))
        doc_insert_char(doc, TERM_RED, 'v');
    else if (have_flag(flagzilla->plr_flgs, OF_RES_(gf)))
        doc_insert_char(doc, TERM_WHITE, '+');
    else
        doc_insert_char(doc, TERM_L_DARK, '.');

    if (pct == 100)
        color = 'v';
    else if (pct < 0)
        color = 'D';
    else if (ct == 1)
        color = 'y';
    else if (ct == 2)
        color = 'R';
    else if (ct >= 3)
        color = 'r';

    if (gf == GF_FEAR && res_pct(GF_FEAR) < 100)
        doc_printf(doc, " %3dx", ct);
    else
        doc_printf(doc, " <color:%c>%3d%%</color>", color, pct);
    doc_newline(doc);
}

static void _build_curse_flags(doc_ptr doc, cptr name)
{
    int i;
    doc_printf(doc, " %-11.11s: ", name);
    for (i = 1; i <= equip_max(); i++)
    {
        object_type *o_ptr = equip_obj(i);

        if (o_ptr)
        {
            if (o_ptr->curse_flags & OFC_PERMA_CURSE)
                doc_insert_char(doc, TERM_VIOLET, '*');
            else if (o_ptr->curse_flags & OFC_HEAVY_CURSE)
                doc_insert_char(doc, TERM_L_RED, '+');
            else if (o_ptr->curse_flags & OFC_CURSED)
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
    for (i = 1; i <= equip_max(); i++)
    {
        if (kill_flg != OF_INVALID && have_flag(flagzilla->obj_flgs[i], kill_flg))
            doc_insert_char(doc, TERM_RED, '*');
        else if (have_flag(flagzilla->obj_flgs[i], flg))
            doc_insert_char(doc, TERM_WHITE, '+');
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }
    if (kill_flg != OF_INVALID && have_flag(flagzilla->tim_plr_flgs, kill_flg))
        doc_insert_char(doc, TERM_YELLOW, '*');
    else if (have_flag(flagzilla->tim_plr_flgs, flg))
        doc_insert_char(doc, TERM_YELLOW, '+');
    else if (kill_flg != OF_INVALID && have_flag(flagzilla->plr_flgs, kill_flg))
        doc_insert_char(doc, TERM_RED, '*');
    else if (have_flag(flagzilla->plr_flgs, flg))
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
    for (i = 1; i <= equip_max(); i++)
    {
        if (have_flag(flagzilla->obj_flgs[i], flg))
        {
            doc_insert_char(doc, TERM_WHITE, '+');
            result++;
        }
        else if (dec_flg != OF_INVALID && have_flag(flagzilla->obj_flgs[i], dec_flg))
        {
            doc_insert_char(doc, TERM_L_RED, '-');
            result--;
        }
        else
            doc_insert_char(doc, TERM_L_DARK, '.');
    }
    if (have_flag(flagzilla->tim_plr_flgs, flg))
    {
       doc_insert_char(doc, TERM_YELLOW, '#');
       result++;
    }
    else if (have_flag(flagzilla->plr_flgs, flg))
    {
        doc_insert_char(doc, TERM_WHITE, '+');
        result++;
    }
    else if (dec_flg != OF_INVALID && have_flag(flagzilla->plr_flgs, dec_flg))
    {
        doc_insert_char(doc, TERM_L_RED, '-');
        result++;
    }
    else
        doc_insert_char(doc, TERM_L_DARK, '.');

    return result;
}

static void _build_flags_aura(doc_ptr doc, cptr name, int flg, _flagzilla_ptr flagzilla)
{
    if (_build_flags_imp(doc, name, flg, OF_INVALID, flagzilla))
    {
        doc_printf(doc, " %dd%d+2", 1 + plr->lev/10, 2 + plr->lev/ 10);
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

    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        gf_info_ptr gfi = gf_lookup(i);
        if (gfi->flags & GFF_DISPLAY)
            _build_res_flags(doc, i, flagzilla);
    }

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Auras", 14);
    _build_flags_aura(doc, "Aura Elec", OF_AURA_ELEC, flagzilla);
    _build_flags_aura(doc, "Aura Fire", OF_AURA_FIRE, flagzilla);
    _build_flags_aura(doc, "Aura Cold", OF_AURA_COLD, flagzilla);
    _build_flags_aura(doc, "Aura Shards", OF_AURA_SHARDS, flagzilla);
    _build_flags(doc, "Revenge", OF_AURA_REVENGE, OF_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Slays", 14);
    _build_slays_imp(doc, "Slay Evil", OF_SLAY_EVIL, OF_KILL_EVIL, flagzilla);
    _build_slays_imp(doc, "Slay Undead", OF_SLAY_UNDEAD, OF_KILL_UNDEAD, flagzilla);
    _build_slays_imp(doc, "Slay Demon", OF_SLAY_DEMON, OF_KILL_DEMON, flagzilla);
    _build_slays_imp(doc, "Slay Dragon", OF_SLAY_DRAGON, OF_KILL_DRAGON, flagzilla);
    _build_slays_imp(doc, "Slay Human", OF_SLAY_HUMAN, OF_KILL_HUMAN, flagzilla);
    _build_slays_imp(doc, "Slay Animal", OF_SLAY_ANIMAL, OF_KILL_ANIMAL, flagzilla);
    _build_slays_imp(doc, "Slay Orc", OF_SLAY_ORC, OF_KILL_ORC, flagzilla);
    _build_slays_imp(doc, "Slay Troll", OF_SLAY_TROLL, OF_KILL_TROLL, flagzilla);
    _build_slays_imp(doc, "Slay Giant", OF_SLAY_GIANT, OF_KILL_GIANT, flagzilla);
    _build_slays_imp(doc, "Slay Good", OF_SLAY_GOOD, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Slay Living", OF_SLAY_LIVING, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Acid Brand", OF_BRAND_ACID, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Elec Brand", OF_BRAND_ELEC, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Fire Brand", OF_BRAND_FIRE, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Cold Brand", OF_BRAND_COLD, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Pois Brand", OF_BRAND_POIS, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Mana Brand", OF_BRAND_MANA, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Sharpness", OF_VORPAL, OF_VORPAL2, flagzilla);
    _build_slays_imp(doc, "Quake", OF_IMPACT, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Vampiric", OF_BRAND_VAMP, OF_INVALID, flagzilla);
    _build_slays_imp(doc, "Chaotic", OF_BRAND_CHAOS, OF_INVALID, flagzilla);
    _build_flags(doc, "Add Blows", OF_BLOWS, OF_DEC_BLOWS, flagzilla);
    _build_flags(doc, "Blessed", OF_BLESSED, OF_INVALID, flagzilla);
    _build_flags(doc, "Riding", OF_RIDING, OF_INVALID, flagzilla);
    _build_flags(doc, "Tunnel", OF_TUNNEL, OF_INVALID, flagzilla);
    _build_flags(doc, "Throwing", OF_THROWING, OF_INVALID, flagzilla);
}

static void _display_known_count(doc_ptr doc, int total, int flg)
{
    int ct = total;
    slot_t slot;
    for (slot = 1; slot <= equip_max(); slot++)
    {
        obj_ptr obj = equip_obj(slot);
        u32b    flgs[OF_ARRAY_SIZE];
        u32b    flgs_known[OF_ARRAY_SIZE];

        if (!obj) continue;
        obj_flags(obj, flgs);
        obj_flags_known(obj, flgs_known);
        if (have_flag(flgs, flg) && !have_flag(flgs_known, flg))
            ct--;
    }
    if (ct)
        doc_printf(doc, " %3dx", ct);
    doc_newline(doc);
}
static void _build_flags2(doc_ptr doc, _flagzilla_ptr flagzilla)
{
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Abilities", 14);

    _build_flags(doc, "Speed", OF_SPEED, OF_DEC_SPEED, flagzilla);

    _build_flags_imp(doc, "Free Action", OF_FREE_ACT, OF_INVALID, flagzilla);
    _display_known_count(doc, plr->free_act, OF_FREE_ACT);

    _build_flags_imp(doc, "See Invis", OF_SEE_INVIS, OF_INVALID, flagzilla);
    _display_known_count(doc, plr->see_inv, OF_SEE_INVIS);

    _build_flags(doc, "Warning", OF_WARNING, OF_INVALID, flagzilla);
    _build_flags(doc, "Slow Digest", OF_SLOW_DIGEST, OF_INVALID, flagzilla);

    _build_flags_imp(doc, "Regenerate", OF_REGEN, OF_INVALID, flagzilla);
    doc_printf(doc, " %3d%%\n", plr->regen); /* TODO: Only display known amount ... but then again, you can feel this, no? */

    _build_flags(doc, "Levitation", OF_LEVITATION, OF_INVALID, flagzilla);

    _build_flags_imp(doc, "Perm Light", OF_LIGHT, OF_DARKNESS, flagzilla);
    if (plr->cur_light > 0)
        doc_printf(doc, " <color:w>%3d'</color>\n", 10*plr->cur_light);
    else if (plr->cur_light < 0)
        doc_printf(doc, " <color:D>%3d'</color>\n", -10*plr->cur_light);
    else
        doc_newline(doc);

    _build_flags(doc, "Reflection", OF_REFLECT, OF_INVALID, flagzilla);

    _build_flags_imp(doc, "Hold Life", OF_HOLD_LIFE, OF_INVALID, flagzilla);
    _display_known_count(doc, plr->hold_life, OF_HOLD_LIFE);

    _build_flags_imp(doc, "Dec Mana", OF_DEC_MANA, OF_INVALID, flagzilla);
    if (plr->dec_mana)
        doc_printf(doc, " %3d%%", dec_mana_cost(plr->dec_mana));
    doc_newline(doc);

    _build_flags(doc, "Easy Spell", OF_EASY_SPELL, OF_INVALID, flagzilla);
    _build_flags(doc, "Anti Magic", OF_NO_MAGIC, OF_INVALID, flagzilla);

    _build_flags_imp(doc, "Magic Skill", OF_MAGIC_MASTERY, OF_DEC_MAGIC_MASTERY, flagzilla);
    if (plr->device_power)
    {
        int pow = device_power_aux(100, plr->device_power) - 100;
        doc_printf(doc, " %+3d%%", pow);
    }
    doc_newline(doc);

    if (_build_flags_imp(doc, "Spell Power", OF_SPELL_POWER, OF_DEC_SPELL_POWER, flagzilla))
    {
        int  pow = spell_power_aux(100, plr->spell_power) - 100;
        doc_printf(doc, " %+3d%%", pow);
    }
    doc_newline(doc);

    if (_build_flags_imp(doc, "Spell Cap", OF_SPELL_CAP, OF_DEC_SPELL_CAP, flagzilla))
    {
        int cap = spell_cap_aux(100, plr->spell_cap) - 100;
        doc_printf(doc, " %+3d%%", cap);
    }
    doc_newline(doc);

    if (_build_flags_imp(doc, "Magic Res", OF_MAGIC_RESISTANCE, OF_INVALID, flagzilla))
        doc_printf(doc, " %+3d%%", plr->magic_resistance);
    doc_newline(doc);

    if (_build_flags_imp(doc, "Infravision", OF_INFRA, OF_INVALID, flagzilla))
        doc_printf(doc, " %3d'", plr->see_infra * 10);
    doc_newline(doc);

    _build_flags(doc, "Stealth", OF_STEALTH, OF_DEC_STEALTH, flagzilla);
    _build_flags(doc, "Searching", OF_SEARCH, OF_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Sustains", 14);
    _build_flags(doc, "Sust Str", OF_SUST_STR, OF_INVALID, flagzilla);
    _build_flags(doc, "Sust Int", OF_SUST_INT, OF_INVALID, flagzilla);
    _build_flags(doc, "Sust Wis", OF_SUST_WIS, OF_INVALID, flagzilla);
    _build_flags(doc, "Sust Dex", OF_SUST_DEX, OF_INVALID, flagzilla);
    _build_flags(doc, "Sust Con", OF_SUST_CON, OF_INVALID, flagzilla);
    _build_flags(doc, "Sust Chr", OF_SUST_CHR, OF_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Detection", 14);
    _build_flags(doc, "Telepathy", OF_TELEPATHY, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Evil", OF_ESP_EVIL, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Nonliv", OF_ESP_NONLIVING, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Good", OF_ESP_GOOD, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Undead", OF_ESP_UNDEAD, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Demon", OF_ESP_DEMON, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Dragon", OF_ESP_DRAGON, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Human", OF_ESP_HUMAN, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Animal", OF_ESP_ANIMAL, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Orc", OF_ESP_ORC, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Troll", OF_ESP_TROLL, OF_INVALID, flagzilla);
    _build_flags(doc, "ESP Giant", OF_ESP_GIANT, OF_INVALID, flagzilla);

    doc_newline(doc);
    _equippy_chars(doc, 14);
    _equippy_heading(doc, "Curses", 14);
    _build_curse_flags(doc, "Cursed");
    _build_flags(doc, "Rnd Tele", OF_TELEPORT, OF_INVALID, flagzilla);
    _build_flags(doc, "No Tele", OF_NO_TELE, OF_INVALID, flagzilla);
    _build_flags(doc, "Drain Exp", OF_DRAIN_EXP, OF_INVALID, flagzilla);
    _build_flags(doc, "Aggravate", OF_AGGRAVATE, OF_INVALID, flagzilla);
    _build_flags(doc, "TY Curse", OF_TY_CURSE, OF_INVALID, flagzilla);
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
    plr_hook_calc_stats(stats);
    plr_tim_stats(tim_stats);

    _equippy_chars(doc, 14);
    _equippy_heading_aux(doc, "", 14);
    doc_insert(doc, "   Base  R  C  P  E  Total\n");

    for (i = 0; i < MAX_STATS; i++)
    {
        int flg = OF_STR + i;
        int dec_flg = OF_DEC_STR + i;
        int sust_flg = OF_SUST_STR + i;
        int e_adj = 0;

        if (plr->stat_use[i] < plr->stat_top[i])
            doc_printf(doc, "<tab:7>%3.3s", stat_names_reduced[i]);
        else
            doc_printf(doc, "<tab:7>%3.3s", stat_names[i]);

        if (plr->stat_max[i] == plr->stat_max_max[i])
            doc_insert(doc, "! : ");
        else
            doc_insert(doc, "  : ");

        /* abcdefghijkl */
        for (j = 1; j <= equip_max(); j++)
        {
            object_type *o_ptr = equip_obj(j);

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
                if (have_flag(flagzilla->tim_plr_flgs, sust_flg) || have_flag(flagzilla->plr_flgs, sust_flg))
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

            if (have_flag(flagzilla->tim_plr_flgs, sust_flg))
            {
                a = TERM_YELLOW;
                c = 's';
            }
            else if (have_flag(flagzilla->plr_flgs, sust_flg))
            {
                a = TERM_GREEN;
                c = 's';
            }
            doc_insert_char(doc, a, c);
        }

        /* Base R C P E */
        cnv_stat(plr->stat_max[i], buf);
        doc_printf(doc, " <color:B>%6.6s%3d%3d%3d%3d</color>",
                    buf, race_ptr->stats[i], class_ptr->stats[i], pers_ptr->stats[i], e_adj);

        /* Total */
        cnv_stat(plr->stat_top[i], buf);
        doc_printf(doc, " <color:G>%6.6s</color>", buf);

        /* Current */
        if (plr->stat_use[i] < plr->stat_top[i])
        {
            cnv_stat(plr->stat_use[i], buf);
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
        int slot;
        char o_name[MAX_NLEN];
        _flagzilla_ptr flagzilla = 0;

        doc_insert(doc, "<topic:Equipment>============================= Character <color:keypress>E</color>quipment =============================\n\n");
        for (slot = 1; slot <= equip_max(); slot++)
        {
            object_type *o_ptr = equip_obj(slot);
            if (!o_ptr) continue;

            object_desc(o_name, o_ptr, OD_COLOR_CODED);
            doc_printf(doc, " %c) <indent><style:indent>%s</style></indent>\n", slot - 1 + 'a', o_name);
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
    if (plr->prace == RACE_MON_RING) return;
    if (!vec_length(plr->innate_blows) && !plr->weapon_ct) return;
    {
        plr_attack_t ctx = {0};
        doc_insert(doc, "<topic:Melee>==================================== <color:keypress>M</color>elee ====================================\n\n");
        plr_attack_display_doc(&ctx, doc);
    }
}

static void _build_shooting(doc_ptr doc)
{
    if (equip_find_obj(TV_BOW, SV_ANY) && !prace_is_(RACE_MON_JELLY) && plr->shooter_info.tval_ammo)
    {
        doc_insert(doc, "<topic:Shooting>=================================== <color:keypress>S</color>hooting ==================================\n\n");
        plr_shoot_doc(doc);
    }
}

/****************************** Magic ************************************/
void plr_display_powers(doc_ptr doc, spell_info *table, int ct)
{
    int i;
    var_t vn, vd, vc, vfm;
    if (!ct) return;

    vn = var_create();
    vd = var_create();
    vc = var_create();
    vfm  = var_create();

    doc_printf(doc, "<topic:Powers>=================================== <color:keypress>P</color>owers ====================================\n\n");
    doc_printf(doc, "<color:G><tab:30>Lvl Cost Fail %-19.19s  Cast Fail</color>\n", "Desc");
    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];
        spell_stats_ptr stats = spell_stats(spell);

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_COST_EXTRA, &vc);
        spell->fn(SPELL_FAIL_MIN, &vfm);

        doc_printf(doc, " %c) %-25.25s %3d %4d %3d%% %-19.19s %5d %4d %3d%%\n",
            I2A(i),
            var_get_string(&vn),
            spell->level, spell->cost + var_get_int(&vc), MAX(spell->fail, var_get_int(&vfm)),
            var_get_string(&vd),
            stats->ct_cast, stats->ct_fail,
            spell_stats_fail(stats)
        );
    }

    var_destroy(&vn);
    var_destroy(&vd);
    var_destroy(&vc);
    var_destroy(&vfm);

    doc_newline(doc);
}

static void _build_powers(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = 0;
    race_t    *race_ptr = get_race();
    class_t   *class_ptr = get_class();

    if (race_ptr->hooks.get_powers)
        ct += (race_ptr->hooks.get_powers)(spells + ct, MAX_SPELLS - ct);

    if (class_ptr->hooks.get_powers)
        ct += (class_ptr->hooks.get_powers)(spells + ct, MAX_SPELLS - ct);

    ct += mut_get_powers(spells + ct, MAX_SPELLS - ct);

    plr_display_powers(doc, spells, ct);
}

void plr_display_spells(doc_ptr doc, spell_info *table, int ct)
{
    if (!ct) return;
    doc_printf(doc, "<topic:Spells>=================================== <color:keypress>S</color>pells ====================================\n\n");
    plr_display_spells_aux(doc, table, ct, "");
}

void plr_display_spells_aux(doc_ptr doc, spell_info *table, int ct, cptr heading)
{
    int i;
    var_t vn, vd, vfm;
    caster_info *caster = get_caster_info();

    if (!ct) return;

    vn = var_create();
    vd = var_create();
    vfm = var_create();

    doc_printf(doc, "    %s <color:G><tab:28>", heading);
    if (caster->options & CASTER_GAIN_SKILL)
        doc_insert(doc, "Skill ");
    doc_printf(doc, "Lvl Cost Fail %-15.15s  Cast Fail</color>\n", "Desc");

    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];
        spell_stats_ptr stats = spell_stats(spell);

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_FAIL_MIN, &vfm);

        doc_printf(doc, " %c) %-23.23s ", I2A(i), var_get_string(&vn));
        if (caster->options & CASTER_GAIN_SKILL)
        {
            spell_stats_ptr stats = spell_stats(spell);
            spell_skill_doc(stats, doc);
        }
        doc_printf(doc, "%3d %4d %3d%% %-15.15s %5d %4d %3d%%\n",
            spell->level, spell_cost(spell), MAX(spell->fail, var_get_int(&vfm)),
            var_get_string(&vd),
            stats->ct_cast, stats->ct_fail,
            spell_stats_fail(stats)
        );
    }

    var_destroy(&vn);
    var_destroy(&vd);
    var_destroy(&vfm);

    doc_newline(doc);
}

static void _build_spells(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = 0;
    race_t    *race_ptr = get_race();
    /*class_t   *class_ptr = get_class_t();*/

    if (race_ptr->hooks.get_spells)
        ct += (race_ptr->hooks.get_spells)(spells + ct, MAX_SPELLS - ct);

    /* TODO: Some classes prompt the user at this point ...
    if (class_ptr->hooks.get_spells)
        ct += (class_ptr->hooks.get_spells)(spells + ct, MAX_SPELLS - ct); */

    plr_display_spells(doc, spells, ct);
}

/****************************** Miscellaneous ************************************/
static bool _is_vowel(char c)
{
    return strchr("aeiou", c) != NULL;
}
static void _doc_race(doc_ptr doc, plr_race_ptr race)
{
    if (_is_vowel(race->name[0])) doc_insert(doc, " an ");
    else doc_insert(doc, " a ");
    doc_insert(doc, race->name);
    if (race->subname)
        doc_printf(doc, " (%s)", race->subname);
}
static void _build_race_history(doc_ptr doc)
{
    if (plr->old_race1 || plr->old_race2)
    {
        int i;

        doc_insert(doc, "You were born as");
        _doc_race(doc, plr_race_aux(plr->start_race, 0));
        doc_insert(doc, ".\n");
        for (i = 0; i < MIN(64, MAX_RACES); i++)
        {
            if (plr->start_race == i) continue;
            if (i < 32)
            {
                if (!(plr->old_race1 & 1L << i)) continue;
            }
            else
            {
                if (!(plr->old_race2 & 1L << (i-32))) continue;
            }
            doc_insert(doc, "You were");
            _doc_race(doc, plr_race_aux(i, 0));
            doc_insert(doc, " before.\n");
        }
        doc_newline(doc);
    }
    else if (plr->mimic_form != MIMIC_NONE)
    {
        doc_insert(doc, "You were born as");
        _doc_race(doc, plr_race_aux(plr->start_race, 0));
        doc_insert(doc, ".\n\n");
    }
}

static int _compare_monsters_counts(monster_race *left, monster_race *right)
{
    if (left->lore.kills.current < right->lore.kills.current)
        return -1;
    if (left->lore.kills.current > right->lore.kills.current)
        return 1;
    if (left->alloc.lvl < right->alloc.lvl)
        return -1;
    if (left->alloc.lvl > right->alloc.lvl)
        return 1;
    if (left->mexp < right->mexp)
        return -1;
    if (left->mexp > right->mexp)
        return 1;
    return 0;
}
static int _compare_monsters(monster_race *left, monster_race *right)
{
    if (left->alloc.lvl < right->alloc.lvl)
        return -1;
    if (left->alloc.lvl > right->alloc.lvl)
        return 1;
    if (left->mexp < right->mexp)
        return -1;
    if (left->mexp > right->mexp)
        return 1;
    if (left->lore.kills.current < right->lore.kills.current)
        return -1;
    if (left->lore.kills.current > right->lore.kills.current)
        return 1;
    return 0;
}
static bool _dead_nonunique(mon_race_ptr r)
{
    if (mon_race_is_unique(r)) return FALSE;
    return r->lore.kills.current > 0;
}
static void _build_uniques(doc_ptr doc)
{
    int ct = ct_kills();

    if (ct)
    {
        int     i;
        vec_ptr v;
        int     ct_uniques_dead;

        doc_printf(doc, "<topic:Kills>================================ Monster <color:keypress>K</color>ills ================================\n\n");

        v = mon_race_filter(mon_race_is_dead_unique);
        ct_uniques_dead = vec_length(v);
        if (ct_uniques_dead)
        {
            doc_printf(doc, "You have defeated %d %s including %d unique monster%s in total. ",
                ct, ct == 1 ? "enemy" : "enemies",
                ct_uniques_dead, ct_uniques_dead == 1 ? "" : "s");

            doc_insert(doc, "\n\n");

            vec_sort(v, (vec_cmp_f)_compare_monsters);

            doc_printf(doc, "  <color:G>%-40.40s <color:R>%3s</color></color>\n", "Uniques", "Lvl");
            for (i = ct_uniques_dead - 1; i >= 0 && i >= ct_uniques_dead - 20; i--)
            {
                monster_race *r_ptr = vec_get(v, i);
                doc_printf(doc, "  %-40.40s %3d\n", r_ptr->name, r_ptr->alloc.lvl);
            }
        }
        else
            doc_printf(doc,"You have defeated %d %s.\n", ct, ct == 1 ? "enemy" : "enemies");

        doc_newline(doc);
        vec_free(v);


        v = mon_race_filter(_dead_nonunique);
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
                doc_printf(cols[0], "  %-25.25s %3d %5d\n", r_ptr->name, r_ptr->alloc.lvl, r_ptr->lore.kills.current);
            }
            doc_newline(cols[0]);

            vec_sort(v, (vec_cmp_f)_compare_monsters_counts);
            doc_printf(cols[1], "  <color:G>%-25.25s %3s <color:R>%5s</color></color>\n", "Non-uniques", "Lvl", "Count");
            for (i = ct - 1; i >= 0 && i >= ct - 20; i--)
            {
                monster_race *r_ptr = vec_get(v, i);
                doc_printf(cols[1], "  %-25.25s %3d %5d\n", r_ptr->name, r_ptr->alloc.lvl, r_ptr->lore.kills.current);
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
    doc_printf(doc, "<topic:Virtues>=================================== <color:keypress>V</color>irtues ===================================\n\n");
    virtue_display(doc, FALSE);
    doc_newline(doc);
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
    mon_pack_ptr pets = plr_pets();
    int     i, ct = mon_pack_count(pets);
    char    pet_name[MAX_NLEN_MON];

    if (ct)
    {
        doc_printf(doc, "<topic:Pets>==================================== <color:keypress>P</color>ets =====================================\n\n");
        doc_printf(doc, "  <color:G>Leading Pets</color>\n");
    }

    for (i = 0; i < ct; i++)
    {
        mon_ptr mon = vec_get(pets->members, i);
        monster_desc(pet_name, mon, MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE | MD_NO_PET_ABBREV);
        doc_printf(doc, "  <indent><style:indent><color:%c>%s</color></style></indent>\n",
            mon->dun->id == plr->dun_id ? 'w' : 'D',
            pet_name);
    }

    if (ct)
    {
        doc_printf(doc, "\n  <color:G>Options</color>\n");
        doc_printf(doc, "  Pets open doors:                    %s\n", (plr->pet_extra_flags & PF_OPEN_DOORS) ? "ON" : "OFF");
        doc_printf(doc, "  Pets pick up items:                 %s\n", (plr->pet_extra_flags & PF_PICKUP_ITEMS) ? "ON" : "OFF");
        doc_printf(doc, "  Allow teleport:                     %s\n", (plr->pet_extra_flags & PF_TELEPORT) ? "ON" : "OFF");
        doc_printf(doc, "  Allow cast attack spell:            %s\n", (plr->pet_extra_flags & PF_ATTACK_SPELL) ? "ON" : "OFF");
        doc_printf(doc, "  Allow cast summon spell:            %s\n", (plr->pet_extra_flags & PF_SUMMON_SPELL) ? "ON" : "OFF");
        doc_printf(doc, "  Allow involve player in area spell: %s\n", (plr->pet_extra_flags & PF_BALL_SPELL) ? "ON" : "OFF");
        if (0 || plr->wizard)
            doc_printf(doc, "  Riding Skill:                       %d\n", skills_riding_current());

        doc_newline(doc);
    }
}

/****************************** Objects ************************************/
static void _build_inventory(doc_ptr doc)
{
    slot_t slot;
    char o_name[MAX_NLEN];

    doc_printf(doc, "<topic:Inventory>============================= Character <color:keypress>I</color>nventory =============================\n\n");

    for (slot = 1; slot <= pack_max(); slot++)
    {
        obj_ptr obj = pack_obj(slot);
        if (!obj) continue;
        object_desc(o_name, obj, OD_COLOR_CODED);
        doc_printf(doc, "<indent><style:indent>%s</style></indent>\n", o_name);
    }

    doc_newline(doc);
}

static void _build_quiver(doc_ptr doc)
{
    if (quiver_count(NULL))
    {
        slot_t slot;
        char o_name[MAX_NLEN];

        doc_printf(doc, "<topic:vQuiver>============================== Character Qui<color:keypress>v</color>er ===============================\n\n");

        for (slot = 1; slot <= quiver_max(); slot++)
        {
            obj_ptr obj = quiver_obj(slot);
            if (!obj) continue;
            object_desc(o_name, obj, OD_COLOR_CODED);
            doc_printf(doc, "<indent><style:indent>%s</style></indent>\n", o_name);
        }

        doc_newline(doc);
    }
}

static void _build_home(doc_ptr doc)
{
    if (home_count(NULL))
    {
        doc_printf(doc, "<topic:Home>================================ <color:keypress>H</color>ome Inventory ===============================\n");
        doc_newline(doc);
        home_display(doc, obj_exists, 0);
        doc_newline(doc);
    }
}

static void _build_museum(doc_ptr doc)
{
    if (museum_count(NULL))
    {
        doc_printf(doc, "<topic:Museum>==================================== <color:keypress>M</color>useum ===================================\n");
        doc_newline(doc);
        museum_display(doc, obj_exists, 0);
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
            "  %-20.20s %5d %6d %5d %5d\n",
            k_ptr->name,
            k_ptr->counts.found,
            k_ptr->counts.bought,
            k_ptr->counts.used,
            k_ptr->counts.destroyed
        );
    }
}

static void _device_counts_imp(doc_ptr doc, int tval, int effect)
{
    device_effect_info_ptr entry = device_get_effect_info(tval, effect);

    if (!entry)
        return;

    if (entry->counts.found || entry->counts.bought || entry->counts.used || entry->counts.destroyed)
    {
        effect_t effect = {0};

        effect.power = entry->level;
        effect.difficulty = entry->level;
        effect.type = entry->type;

        doc_printf(
            doc,
            "  %-20.20s %5d %6d %5d %5d\n",
            do_effect(&effect, SPELL_NAME, 0),
            entry->counts.found,
            entry->counts.bought,
            entry->counts.used,
            entry->counts.destroyed
        );
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
static bool _kind_is_other(int k_idx) {
    int tval = k_info[k_idx].tval;
    if ( tval == TV_FOOD
      || _kind_is_corpse(k_idx)
      || _kind_is_skeleton(k_idx)
      || obj_kind_is_spellbook(k_idx)
      || tval == TV_SHOT
      || tval == TV_ARROW
      || tval == TV_BOLT 
      || tval == TV_CHEST )
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
    ego_type *e_ptr = &e_info[idx];

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

static int _kills(mon_race_ptr r)
{
    if (mon_race_is_fixed_unique(r)) /* XXX problem with r_pkills and uniques */
    {
        if (r->alloc.max_num == 0)
            return 1;
        return 0;
    }
    return r->lore.kills.current;
}
static void _kill_counts_imp(doc_ptr doc, mon_race_p p, cptr text, int total)
{
    int i;
    int kills = 0;

    vec_ptr v = mon_race_filter(p);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr r = vec_get(v, i);
        kills += _kills(r);
    }
    vec_free(v);

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

static void _build_mon_kill_stats(doc_ptr doc)
{
    int total_kills = ct_kills_all();
    doc_printf(doc, "  <color:G>Monsters             Kills   Pct</color>\n");
    _kill_counts_imp(doc, mon_race_is_animal, "Animals", total_kills);
    _kill_counts_imp(doc, mon_race_can_multiply, "Breeders", total_kills);
    _kill_counts_imp(doc, mon_race_is_demon, "Demons", total_kills);
    _kill_counts_imp(doc, mon_race_is_dragon, "Dragons", total_kills);
    _kill_counts_imp(doc, mon_race_is_giant, "Giants", total_kills);
    _kill_counts_imp(doc, mon_race_is_hound, "Hounds", total_kills);
    _kill_counts_imp(doc, mon_race_is_human, "Humans", total_kills);
    _kill_counts_imp(doc, mon_race_is_orc, "Orcs", total_kills);
    _kill_counts_imp(doc, mon_race_is_troll, "Trolls", total_kills);
    _kill_counts_imp(doc, mon_race_is_undead, "Undead", total_kills);
    _kill_counts_imp(doc, mon_race_is_fixed_unique, "Uniques", total_kills);
    if (plr->pclass == CLASS_WARLOCK)
        _kill_counts_imp(doc, warlock_is_pact_monster, "Pact", total_kills);
    doc_newline(doc);
    _kill_counts_imp(doc, mon_race_is_evil, "Evil Monsters", total_kills);
    _kill_counts_imp(doc, mon_race_is_good, "Good Monsters", total_kills);
    _kill_counts_imp(doc, mon_race_is_neutral, "Neutral Monsters", total_kills);
    doc_printf(doc, "\n  %-20.20s %5d\n", "Totals", total_kills);
}

static void _spell_count_imp(doc_ptr doc, cptr heading, int ct, int total)
{
    if (!total) return;
    doc_printf(doc, "  %-20.20s %5d %3d.%1d%%\n", heading, ct,
        ct*100/total,
        (ct*1000/total)%10);
}
static void _build_mon_spell_stats(doc_ptr doc, cptr heading, mon_race_p filter)
{
    int ct_total_moves = 0;
    int ct_spell_moves = 0;
    int ct_spells = 0;
    int total_freq = 0;
    double expected_freq;
    int expected_spells;
    int allocation[MST_COUNT] = {0};
    int i, j, k;

    vec_ptr v = mon_race_filter(filter);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr race = vec_get(v, i);
        int          moves;

        if (!race->spells) continue;
        moves = race->lore.turns.total;
        ct_total_moves += moves;
        ct_spell_moves += race->lore.turns.spell;
        total_freq += race->spells->freq * moves;
        for (j = 0; j < MST_COUNT; j++)
        {
            mon_spell_group_ptr group = race->spells->groups[j];
            if (!group) continue;
            for (k = 0; k < group->count; k++)
            {
                mon_spell_ptr spell = &group->spells[k];
                allocation[j] += spell->lore;
                ct_spells += spell->lore; /* ct_spell_moves includes spell failures */
            }
        }
    }
    vec_free(v);

    if (!ct_total_moves) return;

    doc_printf(doc, "  <color:G>%-20.20s Count   Pct</color>\n", heading);
    _spell_count_imp(doc, "Observed", ct_spell_moves, ct_total_moves);
    expected_freq = (double)total_freq/(ct_total_moves * 100.0);
    expected_spells = ct_total_moves * expected_freq; 
    doc_printf(doc, "  %-20.20s %5d %5.1f%%\n", "Expected", expected_spells, expected_freq * 100.0);
    _spell_count_imp(doc, "Failures", ct_spell_moves - ct_spells, ct_spell_moves);
    _spell_count_imp(doc, "Summon", allocation[MST_SUMMON], ct_spell_moves);
    _spell_count_imp(doc, "Heal", allocation[MST_HEAL], ct_spell_moves);
    _spell_count_imp(doc, "Escape", allocation[MST_ESCAPE], ct_spell_moves);
    _spell_count_imp(doc, "Offense",
        allocation[MST_BREATH] + allocation[MST_BALL] + allocation[MST_BOLT]
            + allocation[MST_BEAM] + allocation[MST_LOS],
        ct_spell_moves);
    _spell_count_imp(doc, "Other",
        allocation[MST_BUFF] + allocation[MST_BIFF] + allocation[MST_ANNOY]
            + allocation[MST_TACTIC] + allocation[MST_WEIRD],
        ct_spell_moves);
}
static void _build_monster_stats(doc_ptr doc)
{
    doc_ptr cols[2];
    cols[0] = doc_alloc(40);
    cols[1] = doc_alloc(40);
    _build_mon_kill_stats(cols[0]);
    _build_mon_spell_stats(cols[1], "Spells", NULL);
    doc_newline(cols[1]);
    _build_mon_spell_stats(cols[1], "Unique Spells", mon_race_is_unique);
    doc_insert_cols(doc, cols, 2, 0);
    doc_free(cols[0]);
    doc_free(cols[1]);
}
static bool _has_kills(mon_race_ptr r)
{
    return _kills(r) > 0;
}
static void _build_monster_histogram(doc_ptr doc)
{
    int histogram[20] = {0};
    int i, max_bucket = 0, total = 0, running = 0;
    
    vec_ptr v = mon_race_filter(_has_kills);
    for (i = 0; i < vec_length(v); i++)
    {
        mon_race_ptr race = vec_get(v, i);
        int          bucket = MIN(19, race->alloc.lvl/5);
        int          amt = _kills(race);;
        histogram[bucket] += amt;
        total += amt;
        max_bucket = MAX(bucket, max_bucket);
    }
    vec_free(v);

    if (!total) return;

    doc_insert(doc, "  <color:G>Level   Count</color>\n");
    for (i = 0; i <= max_bucket; i++)
    {
        int min = i*5;
        int max = min + 4;
        int ct = histogram[i];
        running += ct;
        doc_printf(doc, "  %2d - ", min);
        if (i < 19)
            doc_printf(doc, "%2d", max);
        else
            doc_insert(doc, "**");
        doc_printf(doc, " %5d %2d.%02d%% %3d.%02d%%\n", ct,
            ct * 100 / total, (ct * 10000 / total) % 100,
            running * 100 / total, (running * 10000 / total) % 100);
    }
    doc_newline(doc);
}
static void _build_statistics(doc_ptr doc)
{
    int i;
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
    doc_printf(doc, "                      <color:y>%8d</color>\n\n", plr->au);

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
    _group_counts_imp(doc, obj_kind_is_weapon, "Weapons");
    _group_counts_tval_imp(doc, TV_SHIELD, "Shields");
    _group_counts_tval_imp(doc, TV_BOW, "Bows");
    _group_counts_tval_imp(doc, TV_QUIVER, "Quivers");
    _group_counts_tval_imp(doc, TV_RING, "Rings");
    _group_counts_tval_imp(doc, TV_AMULET, "Amulets");
    _group_counts_tval_imp(doc, TV_LIGHT, "Lights");
    _group_counts_imp(doc, obj_kind_is_body_armor, "Body Armor");
    _group_counts_tval_imp(doc, TV_CLOAK, "Cloaks");
    _group_counts_imp(doc, obj_kind_is_helmet, "Helmets");
    _group_counts_tval_imp(doc, TV_GLOVES, "Gloves");
    _group_counts_tval_imp(doc, TV_BOOTS, "Boots");
    _group_counts_imp(doc, obj_kind_is_wearable, "Totals");

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
    if (plr->pclass == CLASS_NINJA)
        _group_counts_tval_imp(doc, TV_SPIKE, "Syuriken");
    _group_counts_imp(doc, obj_kind_is_spellbook, "Spellbooks");
    _group_counts_tval_imp(doc, TV_FOOD, "Food");
    _group_counts_imp(doc, _kind_is_corpse, "Corpses");
    _group_counts_imp(doc, _kind_is_skeleton, "Skeletons");
    _group_counts_tval_imp(doc, TV_CHEST, "Chests");
    _group_counts_imp(doc, _kind_is_other, "Totals");

    doc_printf(doc, "\n  <color:G>Potions              Found Bought  Used  Dest</color>\n");
    _object_counts_imp(doc, TV_POTION, SV_POTION_CURE_CRITICAL);
    _object_counts_imp(doc, TV_POTION, SV_POTION_CURING);
    _object_counts_imp(doc, TV_POTION, SV_POTION_SPEED);
    if (plr->pclass == CLASS_BLOOD_KNIGHT)
        _object_counts_imp(doc, TV_POTION, SV_POTION_BLOOD);
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

    doc_printf(doc, "\n  <color:G>Scrolls              Found Bought  Used  Dest</color>\n");
    /*_object_counts_imp(doc, TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);*/
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
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_BANISHMENT);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_ACQUIREMENT);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_STAR_ACQUIREMENT);
    _object_counts_imp(doc, TV_SCROLL, SV_SCROLL_ARTIFACT);
    _group_counts_tval_imp(doc, TV_SCROLL, "Totals");

    doc_printf(doc, "\n  <color:G>Wands                Found Bought  Used  Dest</color>\n");
    if (plr->wizard)
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
        _device_counts_imp(doc, TV_WAND, EFFECT_GENOCIDE_ONE);
        _device_counts_imp(doc, TV_WAND, EFFECT_BALL_WATER);
        _device_counts_imp(doc, TV_WAND, EFFECT_BALL_DISINTEGRATE);
        _device_counts_imp(doc, TV_WAND, EFFECT_ROCKET);
        _device_counts_imp(doc, TV_WAND, EFFECT_WALL_BUILDING);
    }
    _group_counts_tval_imp(doc, TV_WAND, "Totals");

    doc_printf(doc, "\n  <color:G>Staves               Found Bought  Used  Dest</color>\n");
    if (plr->wizard)
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
        _device_counts_imp(doc, TV_STAFF, EFFECT_CURING);
        _device_counts_imp(doc, TV_STAFF, EFFECT_HEAL);
        _device_counts_imp(doc, TV_STAFF, EFFECT_TELEPATHY);
        _device_counts_imp(doc, TV_STAFF, EFFECT_SPEED);
        _device_counts_imp(doc, TV_STAFF, EFFECT_IDENTIFY_FULL);
        _device_counts_imp(doc, TV_STAFF, EFFECT_DESTRUCTION);
        _device_counts_imp(doc, TV_STAFF, EFFECT_CONFUSING_LIGHT);
        _device_counts_imp(doc, TV_STAFF, EFFECT_HEAL_CURING);
        _device_counts_imp(doc, TV_STAFF, EFFECT_GENOCIDE);
        _device_counts_imp(doc, TV_STAFF, EFFECT_MANA_STORM);
        _device_counts_imp(doc, TV_STAFF, EFFECT_STARBURST);
        _device_counts_imp(doc, TV_STAFF, EFFECT_DARKNESS_STORM);
        _device_counts_imp(doc, TV_STAFF, EFFECT_RESTORE_MANA);
    }
    _group_counts_tval_imp(doc, TV_STAFF, "Totals");

    doc_printf(doc, "\n  <color:G>Rods                 Found Bought  Used  Dest</color>\n");
    if (plr->wizard)
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
        _device_counts_imp(doc, TV_ROD, EFFECT_LIGHT_AREA);
        _device_counts_imp(doc, TV_ROD, EFFECT_RECALL);
        _device_counts_imp(doc, TV_ROD, EFFECT_DETECT_ALL);
        _device_counts_imp(doc, TV_ROD, EFFECT_ENLIGHTENMENT);
        _device_counts_imp(doc, TV_ROD, EFFECT_SPEED_HERO);
        _device_counts_imp(doc, TV_ROD, EFFECT_HEAL_CURING_HERO);
        _device_counts_imp(doc, TV_ROD, EFFECT_RESTORING);
        _device_counts_imp(doc, TV_ROD, EFFECT_GREAT_CLARITY);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_MANA);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_SHARDS);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_CHAOS);
        _device_counts_imp(doc, TV_ROD, EFFECT_CLAIRVOYANCE);
        _device_counts_imp(doc, TV_ROD, EFFECT_BALL_LIGHT);
    }
    _group_counts_tval_imp(doc, TV_ROD, "Totals");

    doc_printf(doc, "\n  <color:G>Spellbooks           Found Bought  Used  Dest</color>\n");
    _group_counts_imp(doc, _kind_is_third_book, "Third Spellbooks");
    _group_counts_imp(doc, _kind_is_fourth_book, "Fourth Spellbooks");
    _group_counts_imp(doc, obj_kind_is_spellbook, "Totals");

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
    _ego_counts_imp(doc, EGO_JEWELRY_DEFENDER, "Ring/Amulet (Defender)");
    _ego_counts_imp(doc, EGO_BOOTS_ELVENKIND, "Boots of Elvenkind");
    _ego_counts_imp(doc, EGO_BOOTS_SPRITE, "Boots of the Sprite");
    _ego_counts_imp(doc, EGO_BOOTS_SPEED, "Boots of Speed");

    doc_newline(doc);
    _build_monster_stats(doc);
    if (0 || plr->wizard)
        _build_monster_histogram(doc);
}

/****************************** Dungeons ************************************/
void plr_display_dungeons(doc_ptr doc)
{
    vec_ptr v = plr_dun_types();
    int     ct = 0;
    int     i;
    for (i = 0; i < vec_length(v); i++)
    {
        dun_type_ptr type = vec_get(v, i);
        char color = 'w';
        if (type->flags.plr & DF_PLR_COMPLETED) color = 'G';
        else if (type->flags.plr & DF_PLR_FAILED) color = 'r';
        doc_printf(doc, " <color:%c>%-21s</color>: level %3d\n", color, type->name, type->plr_max_lvl);
    }
    vec_free(v);
    doc_newline(doc);

    /* In W_AMBER, you get a random dungeon selection and an unknown total number of dungeons.
     * Let the player know how much they are missing. */
    v = world_dun_types();
    ct = 0;
    for (i = 0; i < vec_length(v); i++)
    {
        dun_type_ptr type = vec_get(v, i);
        if (!type->max_dun_lvl) continue;
        if (type->flags.info & DF_RANDOM) continue;
        if (!dun_pos_interior(dun_mgr()->world, type->world_pos)) continue;
        if (dun_grid_at(dun_mgr()->world, type->world_pos)->flags & CELL_MAP) continue;
        ct++;
    }
    vec_free(v);
    doc_printf(doc,
        " There %s <color:%c>%d</color> undiscovered dungeon%s remaining in this world.\n",
        ct == 1 ? "is" : "are",
        ct == 0 ? 'G' : 'R',
        ct,
        ct == 1 ? "" : "s");

    if (plr->is_dead)
    {
        if (plr->total_winner)
        {
            doc_printf(doc, " <color:v>You %s after winning.</color>\n",
                streq(plr->died_from, "Seppuku") ? "did Seppuku" : "retired from the adventure");
        }
        else if (plr_on_surface())
        {
            doc_printf(doc, " You were killed by %s in %s.\n", plr->died_from, map_name());
        }
        else if (plr_in_dungeon())
        {
            doc_printf(doc, " You were killed by %s on level %d of %s.\n",
                plr->died_from, cave->dun_lvl, map_name());
        }
        else if (quests_get_current())
        {
            doc_printf(doc, " You were killed by %s in the quest '%s'.\n",
                plr->died_from, quests_get_current()->name);
        }
        else /* ??? */
        {
            doc_printf(doc, " You were killed by %s in %s.\n", plr->died_from, map_name());
        }
    }
    else if (cave->flags & DF_GENERATED)
    {
        if (plr_on_surface())
            doc_printf(doc, " You are currently in %s.\n", map_name());
        else if (plr_in_dungeon())
            doc_printf(doc, " You are currently exploring level %d of %s.\n", cave->dun_lvl, map_name());
        else if (quests_get_current())
            doc_printf(doc, " You are currently in the quest '%s'.\n", quests_get_current()->name);
        else
            doc_insert(doc, " Hmmm ... Where are you?");
    }

    if (plr->last_message)
    {
        if (plr->is_dead)
            doc_printf(doc, "\n Last Message: %s\n", plr->last_message);
        else if (plr->total_winner)
            doc_printf(doc, "\n *Winning* Message: %s\n", plr->last_message);
    }
    doc_newline(doc);
}

static cptr _world_name(void)
{
    if (plr->initial_world_id == W_SMAUG) return "The World of Middle Earth";
    return dun_worlds_lookup(plr->initial_world_id)->name;
}
static void _build_dungeons(doc_ptr doc)
{
    doc_printf(doc, "<topic:Dungeons>=================================== <color:keypress>D</color>ungeons ==================================\n\n");
    doc_printf(doc, " You are playing <color:B>%s</color>\n\n", _world_name());
    plr_display_dungeons(doc);
}

/****************************** Messages ************************************/
static void _build_messages(doc_ptr doc)
{
    int  i;
    u32b current_turn = 0;
    int  current_row = 0;

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

        doc_insert_text(doc, m->color, str_buffer(m->msg));
        if (m->count > 1)
        {
            char buf[10];
            sprintf(buf, " (x%d)", m->count);
            doc_insert_text(doc, m->color, buf);
        }
        doc_newline(doc);
    }
    doc_insert(doc, "</style>\n");
}

/******************************** Options ************************************/
static void _build_options(doc_ptr doc)
{
    if (no_artifacts || random_artifacts || plr->noscore)
    {
        doc_printf(doc, "<topic:Options>=================================== <color:keypress>O</color>ptions ===================================\n\n");

        if (no_artifacts)
            doc_printf(doc, " No Artifacts:       On\n");
        else if (random_artifacts)
            doc_printf(doc, " Random Artifacts:   %d%%\n", random_artifact_pct);

        if (plr->noscore)
            doc_printf(doc, "\n <color:v>You have done something illegal.</color>\n");

        doc_newline(doc);
    }
}

/****************************** Character Sheet ************************************/
static void _build_quests(doc_ptr doc)
{
    #if 0
    if (plr->initial_world_id == W_AMBER)
    {
        doc_printf(doc, "<topic:uQuests>==================================== Q<color:keypress>u</color>ests ===================================\n\n");
        quests_doc(doc);
    }
    #endif
}

static bool _is_retired(void)
{
    if (plr->total_winner && plr->is_dead && strcmp(plr->died_from, "Ripe Old Age") == 0)
        return TRUE;
    return FALSE;
}

static void _add_html_header(doc_ptr doc)
{
    str_ptr s = str_alloc_format("%s.html", player_base);
    str_ptr header = str_alloc();

    doc_change_name(doc, str_buffer(s));

    str_append_s(header, "<head>\n");
    str_append_s(header, " <meta name='filetype' value='character dump'>\n");
    str_printf(header,  " <meta name='variant' value='%s'>\n", VERSION_NAME);
    str_printf(header,  " <meta name='variant_version' value='%d.%d.%d'>\n", VER_MAJOR, VER_MINOR, VER_PATCH);
    str_printf(header,  " <meta name='character_name' value='%s'>\n", player_name);
    str_printf(header,  " <meta name='race' value='%s'>\n", get_race()->name);
    str_printf(header,  " <meta name='class' value='%s'>\n", get_class()->name);
    str_printf(header,  " <meta name='level' value='%d'>\n", plr->lev);
    str_printf(header,  " <meta name='experience' value='%d'>\n", plr->exp);
    str_printf(header,  " <meta name='turncount' value='%d'>\n", dun_mgr()->turn);
    str_printf(header,  " <meta name='max_depth' value='%d'>\n", plr_max_dun_lvl());
    str_printf(header,  " <meta name='score' value='%d'>\n", plr->exp);
    str_printf(header,  " <meta name='fame' value='%d'>\n", plr->fame);
    str_printf(header,  " <meta name='status' value='%s'>\n",
        plr->total_winner ? "winner" : (plr->is_dead ? "dead" : "alive"));
    str_printf(header,  " <meta name='winner' value='%d'>\n", plr->total_winner ? 1 : 0);
    str_printf(header,  " <meta name='dead' value='%d'>\n", plr->is_dead ? 1 : 0);
    str_printf(header,  " <meta name='retired' value='%d'>\n", _is_retired() ? 1 : 0);

    if (plr->is_dead)
        str_printf(header,  " <meta name='killer' value='%s'>\n", plr->died_from);
    str_append_s(header, "</head>");

    doc_change_html_header(doc, str_buffer(header));

    str_free(s);
    str_free(header);
}

void plr_display_character_sheet(doc_ptr doc)
{
    _add_html_header(doc);

    doc_printf(doc, "<style:wide>  [%s <$:version> Character Dump]\n", VERSION_NAME);
    if (plr->total_winner)
        doc_insert(doc, "              <color:B>***WINNER***</color>\n");
    else if (plr->is_dead)
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
    plr_hook_character_dump(doc);
    _build_dungeons(doc);
    _build_quests(doc);
    _build_uniques(doc);
    _build_virtues(doc);
    _build_race_history(doc);
    _build_mutations(doc);
    _build_pets(doc);
    _build_inventory(doc);
    _build_quiver(doc);
    _build_home(doc);
    _build_museum(doc);
    _build_statistics(doc);
    _build_messages(doc);
    _build_options(doc);
    dun_mgr_doc(doc);

    doc_insert(doc, "</style>");
}

void plr_display(void)
{
    doc_ptr d = doc_alloc(80);

    plr_display_character_sheet(d);

    screen_save();
    doc_display(d, "Character Sheet", 0);
    screen_load();

    doc_free(d);
}

/* This is used by the birth process ... Note that there
   is birth code that assumes the location of fields on the screen,
   and probably that should be cleaned up some day */
void plr_display_birth(void)
{
    doc_ptr d = doc_alloc(80);
    _build_general(d);
    doc_sync_term(d, doc_range_all(d), doc_pos_create(0, 1));
    doc_free(d);
}
