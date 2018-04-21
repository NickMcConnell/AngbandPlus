#include "angband.h"

#include <stdlib.h>
#include <assert.h>

/* Display detailed object information to the user */
extern void obj_display(object_type *o_ptr);
extern void obj_display_rect(object_type *o_ptr, rect_t display);
extern void obj_display_doc(object_type *o_ptr, doc_ptr doc);
extern void device_display_doc(object_type *o_ptr, doc_ptr doc);

static void _display_name(object_type *o_ptr, doc_ptr doc);
static void _display_desc(object_type *o_ptr, doc_ptr doc);
static void _display_stats(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_sustains(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_other_pval(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_brands(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_slays(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_resists(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_abilities(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_auras(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_extra(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_curses(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_activation(object_type *o_ptr, doc_ptr doc);
static void _display_ignore(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc);
static void _display_autopick(object_type *o_ptr, doc_ptr doc);
static void _lite_display_doc(object_type *o_ptr, doc_ptr doc);

static int _calc_net_bonus(int amt, u32b flgs[TR_FLAG_SIZE], int flg, int flg_dec);
static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term);
static string_ptr _get_res_name(int res);

/* Low level helpers.
   Note: We have bonus flags, and penalty flags. For example
   TR_STR and TR_DEC_STR. With Rand-arts and the new Cursed implementation, it
   is possible for an object to have both. Also, pvals can be negative and use
   the bonus flag (rather than being positive and use the penalty flag).
   Note: vec_ptr is always vec<string_ptr> */
static int _calc_net_bonus(int amt, u32b flgs[TR_FLAG_SIZE], int flg, int flg_dec)
{
    int net = 0;

    if (flg != TR_INVALID && have_flag(flgs, flg))
        net += amt;
    if (flg_dec != TR_INVALID && have_flag(flgs, flg_dec))
        net -= amt;

    return net;
}

static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term)
{
    int ct = vec_length(v);
    int i;
    for (i = 0; i < ct; i++)
    {
        string_ptr s = vec_get(v, i);
        if (i < ct - 1 && sep)
            doc_printf(doc, "%s%c ", string_buffer(s), sep);
        else if (i == ct - 1 && term)
            doc_printf(doc, "%s%c", string_buffer(s), term);
        else
            doc_insert(doc, string_buffer(s));
    }
}

string_ptr _get_res_name(int res)
{
    return string_alloc_format(
        "<color:%c>%s</color>",
        attr_to_attr_char(res_color(res)),
        res_name(res)
    );
}

/* Mid level helpers. Output one block of information at a time. */
static void _display_name(object_type *o_ptr, doc_ptr doc)
{
    char o_name[MAX_NLEN];

    object_desc(o_name, o_ptr, OD_COLOR_CODED | OD_NAME_AND_ENCHANT | OD_NO_FLAVOR);
    doc_printf(doc, "%s\n", o_name);
}

static void _display_desc(object_type *o_ptr, doc_ptr doc)
{
    cptr text;
    if (o_ptr->name1 && object_is_known(o_ptr))
        text = a_text + a_info[o_ptr->name1].text;
    else
        text = k_text + k_info[o_ptr->k_idx].text;

    if (strlen(text))
        doc_printf(doc, "%s\n\n", text);
}

/* For convenience, the stats section will include a few other bonuses, like stealh,
   searching, tunneling, and speed. These are things where the pval is readily
   interpreted by the user (unlike, say, device power). */
struct _flag_info_s
{
    int flg;
    int flg_dec;
    cptr name;
};
typedef struct _flag_info_s _flag_info_t, *_flag_info_ptr;
static _flag_info_t _stats_flags[] =
{
    { TR_STR,           TR_DEC_STR,             "STR" },
    { TR_INT,           TR_DEC_INT,             "INT" },
    { TR_WIS,           TR_DEC_WIS,             "WIS" },
    { TR_DEX,           TR_DEC_DEX,             "DEX" },
    { TR_CON,           TR_DEC_CON,             "CON" },
    { TR_CHR,           TR_DEC_CHR,             "CHR" },
    { TR_SPEED,         TR_DEC_SPEED,           "Speed" },
    { TR_MAGIC_MASTERY, TR_DEC_MAGIC_MASTERY,   "Devices" },
    { TR_STEALTH,       TR_DEC_STEALTH,         "Stealth" },
    { TR_SEARCH,        TR_INVALID,             "Searching" },
    { TR_INFRA,         TR_INVALID,             "Infravision" },
    { TR_TUNNEL,        TR_INVALID,             "Digging" },
    { TR_INVALID,       TR_INVALID,             NULL }
};

static void _build_bonus_list(int pval, u32b flgs[TR_FLAG_SIZE], _flag_info_ptr table, vec_ptr v)
{
    while (table->flg != TR_INVALID)
    {
        int net = _calc_net_bonus(pval, flgs, table->flg, table->flg_dec);
        if (net > 0)
            vec_add(v, string_copy_s(table->name));

        table++;
    }
}

static void _build_penalty_list(int pval, u32b flgs[TR_FLAG_SIZE], _flag_info_ptr table, vec_ptr v)
{
    while (table->flg != TR_INVALID)
    {
        int net = _calc_net_bonus(pval, flgs, table->flg, table->flg_dec);
        if (net < 0)
            vec_add(v, string_copy_s(table->name));

        table++;
    }
}

static void _display_stats(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    /* Pass 1: Positive Bonus */
    _build_bonus_list(o_ptr->pval, flgs, _stats_flags, v);
    if (vec_length(v) > 0)
    {
        doc_printf(doc, "<color:G>%+d</color> to ", abs(o_ptr->pval));
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    /* Pass 2: Penalty Phase */
    vec_clear(v);
    _build_penalty_list(o_ptr->pval, flgs, _stats_flags, v);
    if (vec_length(v) > 0)
    {
        doc_printf(doc, "<color:r>%+d</color> to ", -abs(o_ptr->pval));
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static _flag_info_t _sustains_flags[] =
{
    { TR_SUST_STR, TR_INVALID, "STR" },
    { TR_SUST_INT, TR_INVALID, "INT" },
    { TR_SUST_WIS, TR_INVALID, "WIS" },
    { TR_SUST_DEX, TR_INVALID, "DEX" },
    { TR_SUST_CON, TR_INVALID, "CON" },
    { TR_SUST_CHR, TR_INVALID, "CHR" },
    { TR_INVALID,  TR_INVALID, NULL }
};

static void _display_sustains(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    _build_bonus_list(1, flgs, _sustains_flags, v);
    if (vec_length(v) > 0)
    {
        if (vec_length(v) == 6)
            doc_insert(doc, "<color:B>Sustains All Stats</color>");
        else
        {
            doc_insert(doc, "<color:B>Sustain</color> ");
            _print_list(v, doc, ',', '\0');
        }
        doc_newline(doc);
    }
    vec_free(v);
}

/* These pval flags require detailed explanations (e.g. +21% to Spell Power or -1.50 to Attack Speed) */
static void _display_other_pval(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    int net = 0;

    if (!o_ptr->pval) return;

    if (have_flag(flgs, TR_BLOWS) || have_flag(flgs, TR_DEC_BLOWS))
    {
        int num = 0;

        if (have_flag(flgs, TR_BLOWS))
            num +=  o_ptr->pval * 50;
        if (have_flag(flgs, TR_DEC_BLOWS))
            num -=  o_ptr->pval * 100;

        doc_printf(doc, "<color:%c>%+d.%2.2d</color> to Attack Speed\n",
                    (net > 0) ? 'G' : 'r', num / 100, num % 100);
    }
    if (have_flag(flgs, TR_XTRA_SHOTS))
    {
        int num = o_ptr->pval * 25;
        doc_printf(doc, "<color:%c>%+d.%2.2d</color> to Shooting Speed\n",
                    (net > 0) ? 'G' : 'r', num / 100, num % 100);
    }

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_DEVICE_POWER, TR_DEC_MAGIC_MASTERY);
    if (net)
    {
        int pct = device_power_aux(100, net) - 100;
        doc_printf(doc, "<color:%c>%+d%%</color> to Device Power\n",
                    (net > 0) ? 'G' : 'r', pct);
    }

    if (have_flag(flgs, TR_MAGIC_RESISTANCE))
    {
        int pct = o_ptr->pval * 5;
        doc_printf(doc, "<color:%c>%+d%%</color> to Magic Resistance\n",
                    (net > 0) ? 'G' : 'r', pct);
    }

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_SPELL_POWER, TR_DEC_SPELL_POWER);
    if (net)
    {
        int pct = spell_power_aux(100, net) - 100;
        doc_printf(doc, "<color:%c>%+d%%</color> to Spell Power\n",
                    (net > 0) ? 'G' : 'r', pct);
    }

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_SPELL_CAP, TR_DEC_SPELL_CAP);
    if (net)
    {
        int pct = spell_cap_aux(100, net) - 100;
        doc_printf(doc, "<color:%c>%+d%%</color> to Spell Capacity\n",
                    (net > 0) ? 'G' : 'r', pct);
    }

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_LIFE, TR_DEC_LIFE);
    if (net)
    {
        int pct = 3 * net;
        doc_printf(doc, "<color:%c>%+d%%</color> to Life Rating\n",
                    (net > 0) ? 'G' : 'r', pct);
    }
}

static void _display_brands(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    if (have_flag(flgs, TR_BRAND_ACID))
        vec_add(v, string_copy_s("<color:g>Acid Brand</color>"));
    if (have_flag(flgs, TR_BRAND_ELEC))
        vec_add(v, string_copy_s("<color:b>Lightning Brand</color>"));
    if (have_flag(flgs, TR_BRAND_FIRE))
        vec_add(v, string_copy_s("<color:r>Flame Tongue</color>"));
    if (have_flag(flgs, TR_BRAND_COLD))
        vec_add(v, string_copy_s("<color:W>Frost Brand</color>"));
    if (have_flag(flgs, TR_BRAND_POIS))
        vec_add(v, string_copy_s("<color:G>Viper's Fang</color>"));
    if (have_flag(flgs, TR_CHAOTIC))
        vec_add(v, string_copy_s("<color:v>Mark of Chaos</color>"));
    if (have_flag(flgs, TR_VAMPIRIC))
        vec_add(v, string_copy_s("<color:D>Vampiric</color>"));
    if (have_flag(flgs, TR_IMPACT))
        vec_add(v, string_copy_s("<color:U>Earthquakes</color>"));
    if (have_flag(flgs, TR_VORPAL2))
        vec_add(v, string_copy_s("<color:v>*Sharpness*</color>"));
    else if (have_flag(flgs, TR_VORPAL))
        vec_add(v, string_copy_s("<color:R>Sharpness</color>"));
    if (have_flag(flgs, TR_STUN))
        vec_add(v, string_copy_s("<color:o>Stuns</color>"));
    if (have_flag(flgs, TR_ORDER))
        vec_add(v, string_copy_s("<color:W>Weapon of Order</color>"));
    if (have_flag(flgs, TR_WILD))
        vec_add(v, string_copy_s("<color:o>Wild Weapon</color>"));
    if (have_flag(flgs, TR_FORCE_WEAPON))
        vec_add(v, string_copy_s("<color:B>Mana Brand</color>"));

    if (vec_length(v))
    {
        _print_list(v, doc, ';', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static void _display_slays(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    if (have_flag(flgs, TR_KILL_EVIL))
        vec_add(v, string_copy_s("<color:y>*Evil*</color>"));
    else if (have_flag(flgs, TR_SLAY_EVIL))
        vec_add(v, string_copy_s("<color:y>Evil</color>"));

    if (have_flag(flgs, TR_SLAY_GOOD))
        vec_add(v, string_copy_s("<color:W>Good</color>"));

    if (have_flag(flgs, TR_SLAY_LIVING))
        vec_add(v, string_copy_s("<color:o>Living</color>"));

    if (have_flag(flgs, TR_KILL_DRAGON))
        vec_add(v, string_copy_s("<color:r>*Dragons*</color>"));
    else if (have_flag(flgs, TR_SLAY_DRAGON))
        vec_add(v, string_copy_s("<color:r>Dragons</color>"));

    if (have_flag(flgs, TR_KILL_DEMON))
        vec_add(v, string_copy_s("<color:R>*Demons*</color>"));
    else if (have_flag(flgs, TR_SLAY_DEMON))
        vec_add(v, string_copy_s("<color:R>Demons</color>"));

    if (have_flag(flgs, TR_KILL_UNDEAD))
        vec_add(v, string_copy_s("<color:D>*Undead*</color>"));
    else if (have_flag(flgs, TR_SLAY_UNDEAD))
        vec_add(v, string_copy_s("<color:D>Undead</color>"));

    if (have_flag(flgs, TR_KILL_ANIMAL))
        vec_add(v, string_copy_s("<color:g>*Animals*</color>"));
    else if (have_flag(flgs, TR_SLAY_ANIMAL))
        vec_add(v, string_copy_s("<color:g>Animals</color>"));

    if (have_flag(flgs, TR_KILL_HUMAN))
        vec_add(v, string_copy_s("<color:s>*Humans*</color>"));
    else if (have_flag(flgs, TR_SLAY_HUMAN))
        vec_add(v, string_copy_s("<color:s>Humans</color>"));

    if (have_flag(flgs, TR_KILL_ORC))
        vec_add(v, string_copy_s("<color:U>*Orcs*</color>"));
    else if (have_flag(flgs, TR_SLAY_ORC))
        vec_add(v, string_copy_s("<color:U>Orcs</color>"));

    if (have_flag(flgs, TR_KILL_TROLL))
        vec_add(v, string_copy_s("<color:g>*Trolls*</color>"));
    else if (have_flag(flgs, TR_SLAY_TROLL))
        vec_add(v, string_copy_s("<color:g>Trolls</color>"));

    if (have_flag(flgs, TR_KILL_GIANT))
        vec_add(v, string_copy_s("<color:u>*Giants*</color>"));
    else if (have_flag(flgs, TR_SLAY_GIANT))
        vec_add(v, string_copy_s("<color:u>Giants</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Slay ");
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static void _display_resists(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    int     i;
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    /* Immunities */
    for (i = 0; i < RES_TELEPORT; i++)
    {
        int flg = res_get_object_immune_flag(i);
        if (flg != TR_INVALID && have_flag(flgs, flg))
            vec_add(v, _get_res_name(i));
    }
    if (vec_length(v))
    {
        doc_insert(doc, "Immunity to ");
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    /* Resistances */
    vec_clear(v);
    for (i = 0; i < RES_TELEPORT; i++)
    {
        int flg_im = res_get_object_immune_flag(i);
        int net = 0;

        if (flg_im != TR_INVALID && have_flag(flgs, flg_im)) continue;
        net = _calc_net_bonus(1, flgs, res_get_object_flag(i), res_get_object_vuln_flag(i));
        if (net > 0)
            vec_add(v, _get_res_name(i));
    }
    if (vec_length(v))
    {
        doc_insert(doc, "Resist ");
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    /* Vulnerabilities */
    vec_clear(v);
    for (i = 0; i < RES_TELEPORT; i++)
    {
        int flg_im = res_get_object_immune_flag(i);
        int net = 0;

        if (flg_im != TR_INVALID && have_flag(flgs, flg_im)) continue;
        net = _calc_net_bonus(1, flgs, res_get_object_flag(i), res_get_object_vuln_flag(i));
        if (net < 0)
            vec_add(v, _get_res_name(i));
    }
    if (vec_length(v))
    {
        doc_insert(doc, "Vulnerability to ");
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static void _display_abilities(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    if (have_flag(flgs, TR_FREE_ACT))
        vec_add(v, string_copy_s("<color:R>Free Action</color>"));
    if (have_flag(flgs, TR_SEE_INVIS))
        vec_add(v, string_copy_s("<color:B>See Invisible</color>"));
    if (have_flag(flgs, TR_REGEN))
        vec_add(v, string_copy_s("<color:g>Regeneration</color>"));
    if (have_flag(flgs, TR_HOLD_LIFE))
        vec_add(v, string_copy_s("<color:y>Hold Life</color>"));
    if (have_flag(flgs, TR_REFLECT))
        vec_add(v, string_copy_s("<color:o>Reflection</color>"));
    if (have_flag(flgs, TR_LEVITATION))
        vec_add(v, string_copy_s("<color:B>Levitation</color>"));
    if (have_flag(flgs, TR_SLOW_DIGEST))
        vec_add(v, string_copy_s("<color:g>Slow Digestion</color>"));
    if (have_flag(flgs, TR_WARNING))
        vec_add(v, string_copy_s("<color:y>Warning</color>"));
    if (have_flag(flgs, TR_NO_MAGIC))
        vec_add(v, string_copy_s("<color:r>Anti-Magic</color>"));
    if (have_flag(flgs, TR_NO_SUMMON))
        vec_add(v, string_copy_s("<color:v>Prevents Summoning</color>"));
    if (have_flag(flgs, TR_NO_TELE))
        vec_add(v, string_copy_s("<color:r>Prevents Teleportation</color>"));
    if (have_flag(flgs, TR_THROW))
        vec_add(v, string_copy_s("<color:D>Throwing</color>"));
    if (have_flag(flgs, TR_BLESSED))
        vec_add(v, string_copy_s("<color:B>Blessed</color>"));
    if (have_flag(flgs, TR_RIDING))
        vec_add(v, string_copy_s("<color:o>Riding</color>"));
    if (have_flag(flgs, TR_LITE))
    {
        if (o_ptr->name2 == EGO_HELMET_VAMPIRE || o_ptr->name1 == ART_NIGHT)
            vec_add(v, string_copy_s("<color:D>Permanent Darkness (1)</color>"));
        else
            vec_add(v, string_copy_s("<color:y>Permanent Light (1)</color>"));
    }
    if (vec_length(v))
    {
        _print_list(v, doc, ';', '\0');
        doc_newline(doc);
    }

    vec_clear(v);
    if (have_flag(flgs, TR_TELEPATHY))
        vec_add(v, string_copy_s("<color:y>Telepathy</color>"));
    if (have_flag(flgs, TR_ESP_ANIMAL))
        vec_add(v, string_copy_s("<color:B>Sense Animals</color>"));
    if (have_flag(flgs, TR_ESP_UNDEAD))
        vec_add(v, string_copy_s("<color:D>Sense Undead</color>"));
    if (have_flag(flgs, TR_ESP_DEMON))
        vec_add(v, string_copy_s("<color:R>Sense Demons</color>"));
    if (have_flag(flgs, TR_ESP_ORC))
        vec_add(v, string_copy_s("<color:U>Sense Orcs</color>"));
    if (have_flag(flgs, TR_ESP_TROLL))
        vec_add(v, string_copy_s("<color:g>Sense Trolls</color>"));
    if (have_flag(flgs, TR_ESP_GIANT))
        vec_add(v, string_copy_s("<color:u>Sense Giants</color>"));
    if (have_flag(flgs, TR_ESP_DRAGON))
        vec_add(v, string_copy_s("<color:r>Sense Dragons</color>"));
    if (have_flag(flgs, TR_ESP_HUMAN))
        vec_add(v, string_copy_s("<color:s>Sense Humans</color>"));
    if (have_flag(flgs, TR_ESP_EVIL))
        vec_add(v, string_copy_s("<color:y>Sense Evil</color>"));
    if (have_flag(flgs, TR_ESP_GOOD))
        vec_add(v, string_copy_s("<color:w>Sense Good</color>"));
    if (have_flag(flgs, TR_ESP_NONLIVING))
        vec_add(v, string_copy_s("<color:B>Sense Nonliving</color>"));
    if (have_flag(flgs, TR_ESP_UNIQUE))
        vec_add(v, string_copy_s("<color:v>Sense Uniques</color>"));

    if (vec_length(v))
    {
        _print_list(v, doc, ';', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static void _display_auras(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    if (have_flag(flgs, TR_SH_FIRE))
        vec_add(v, _get_res_name(RES_FIRE));
    if (have_flag(flgs, TR_SH_COLD))
        vec_add(v, _get_res_name(RES_COLD));
    if (have_flag(flgs, TR_SH_ELEC))
        vec_add(v, _get_res_name(RES_ELEC));
    if (have_flag(flgs, TR_SH_SHARDS))
        vec_add(v, string_copy_s("<color:U>Shards</color>"));
    if (have_flag(flgs, TR_SH_REVENGE))
        vec_add(v, string_copy_s("<color:v>Retaliation</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Aura of ");
        _print_list(v, doc, ',', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static void _display_extra(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    int net = 0;

    if (have_flag(flgs, TR_EASY_SPELL))
        doc_insert(doc, "It affects your ability to cast spells.\n");

    if (have_flag(flgs, TR_DEC_MANA))
    {
        caster_info *caster_ptr = get_caster_info();
        if (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
            doc_insert(doc, "It decreases your mana consumption.\n");
    }

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_WEAPONMASTERY, TR_INVALID);
    if (net)
    {
        doc_printf(doc, "It %s the damage dice of your melee weapon.\n",
            (net > 0) ? "increases" : "<color:R>decreases</color>");
    }

    switch (o_ptr->name1)
    {
    case ART_STONE_OF_NATURE:
        doc_insert(doc, "It greatly enhances Nature magic.\n");
        break;
    case ART_STONE_OF_LIFE:
        doc_insert(doc, "It greatly enhances Life magic.\n");
        break;
    case ART_STONE_OF_SORCERY:
        doc_insert(doc, "It greatly enhances Sorcery magic.\n");
        break;
    case ART_STONE_OF_CHAOS:
        doc_insert(doc, "It greatly enhances Chaos magic.\n");
        break;
    case ART_STONE_OF_DEATH:
        doc_insert(doc, "It greatly enhances Death magic.\n");
        break;
    case ART_STONE_OF_TRUMP:
        doc_insert(doc, "It greatly enhances Trump magic.\n");
        break;
    case ART_STONE_OF_DAEMON:
        doc_insert(doc, "It greatly enhances Daemon magic.\n");
        break;
    case ART_STONE_OF_CRUSADE:
        doc_insert(doc, "It greatly enhances Crusade magic.\n");
        break;
    case ART_STONE_OF_CRAFT:
        doc_insert(doc, "It greatly enhances Craft magic.\n");
        break;
    case ART_STONE_OF_ARMAGEDDON:
        doc_insert(doc, "It greatly enhances Armageddon magic.\n");
        break;
    case ART_STONEMASK:
        doc_insert(doc, "It makes you turn into a vampire permanently.\n");
        break;
    }

    if (object_is_(o_ptr, TV_SWORD, SV_POISON_NEEDLE))
        doc_insert(doc, "It will attempt to kill a monster instantly.\n");

    if (object_is_(o_ptr, TV_POLEARM, SV_DEATH_SCYTHE))
        doc_insert(doc, "It causes you to strike yourself sometimes.\nIt always penetrates invulnerability barriers.\n");

    if (o_ptr->name2 == EGO_GLOVES_GENJI || o_ptr->name1 == ART_MASTER_TONBERRY || o_ptr->name1 == ART_MEPHISTOPHELES)
        doc_insert(doc, "It affects your ability to hit when you are wielding two weapons.\n");

    if (o_ptr->tval == TV_STATUE)
    {
        if (o_ptr->pval == MON_BULLGATES)
            doc_insert(doc, "It is shameful.\n");
        else if (r_info[o_ptr->pval].flags2 & RF2_ELDRITCH_HORROR)
            doc_insert(doc, "It is fearful.\n");
        else
            doc_insert(doc, "It is cheerful.\n");
    }

    if (o_ptr->tval == TV_FIGURINE)
        doc_insert(doc, "It will transform into a pet when thrown.\n");

    if (o_ptr->name3)
        doc_printf(doc, "It reminds you of the artifact <color:R>%s</color>.\n", a_name + a_info[o_ptr->name3].name);
}

static void _display_curses(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);

    if (object_is_cursed(o_ptr))
    {
        if (o_ptr->curse_flags & TRC_PERMA_CURSE)
            doc_insert(doc, "It is <color:v>Permanently Cursed</color>.\n");
        else if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
            doc_insert(doc, "It is <color:r>Heavily Cursed</color>.\n");
        else
        {
            if (object_is_device(o_ptr) && !(o_ptr->ident & IDENT_FULL))
            {
                /* Hide cursed status of devices until *Identified* */
            }
            else
            {
                doc_insert(doc, "It is <color:D>Cursed</color>.\n");
            }
        }
    }

    if (have_flag(flgs, TR_TY_CURSE) || o_ptr->curse_flags & TRC_TY_CURSE)
        vec_add(v, string_copy_s("<color:v>*Ancient Foul Curse*</color>"));
    if (have_flag(flgs, TR_AGGRAVATE) || o_ptr->curse_flags & TRC_AGGRAVATE)
        vec_add(v, string_copy_s("<color:r>Aggravates</color>"));
    if (have_flag(flgs, TR_DRAIN_EXP) || o_ptr->curse_flags & TRC_DRAIN_EXP)
        vec_add(v, string_copy_s("<color:y>Drains Experience</color>"));
    if (o_ptr->curse_flags & TRC_SLOW_REGEN)
        vec_add(v, string_copy_s("<color:o>Slow Regeneration</color>"));
    if (o_ptr->curse_flags & TRC_ADD_L_CURSE)
        vec_add(v, string_copy_s("<color:w>Adds Weak Curses</color>"));
    if (o_ptr->curse_flags & TRC_ADD_H_CURSE)
        vec_add(v, string_copy_s("<color:b>Adds Heavy Curses</color>"));
    if (o_ptr->curse_flags & TRC_CALL_ANIMAL)
        vec_add(v, string_copy_s("<color:g>Attracts Animals</color>"));
    if (o_ptr->curse_flags & TRC_CALL_DEMON)
        vec_add(v, string_copy_s("<color:R>Attracts Demons</color>"));
    if (o_ptr->curse_flags & TRC_CALL_DRAGON)
        vec_add(v, string_copy_s("<color:r>Attracts Dragons</color>"));
    if (o_ptr->curse_flags & TRC_COWARDICE)
        vec_add(v, string_copy_s("<color:y>Cowardice</color>"));
    if (have_flag(flgs, TR_TELEPORT) || o_ptr->curse_flags & TRC_TELEPORT)
        vec_add(v, string_copy_s("<color:B>Random Teleportation</color>"));
    if (o_ptr->curse_flags & TRC_LOW_MELEE)
        vec_add(v, string_copy_s("<color:G>Miss Blows</color>"));
    if (o_ptr->curse_flags & TRC_LOW_AC)
        vec_add(v, string_copy_s("<color:R>Low AC</color>"));
    if (o_ptr->curse_flags & TRC_LOW_MAGIC)
        vec_add(v, string_copy_s("<color:y>Increased Fail Rates</color>"));
    if (o_ptr->curse_flags & TRC_FAST_DIGEST)
        vec_add(v, string_copy_s("<color:r>Fast Digestion</color>"));
    if (o_ptr->curse_flags & TRC_DRAIN_HP)
        vec_add(v, string_copy_s("<color:o>Drains You</color>"));
    if (o_ptr->curse_flags & TRC_DRAIN_MANA)
        vec_add(v, string_copy_s("<color:B>Drains Mana</color>"));
    if (vec_length(v))
    {
        _print_list(v, doc, ';', '\0');
        doc_newline(doc);
    }

    vec_free(v);
}

static void _display_activation(object_type *o_ptr, doc_ptr doc)
{
    if (obj_has_effect(o_ptr) && object_is_known(o_ptr))
    {
        effect_t e = obj_get_effect(o_ptr);
        cptr     res = do_effect(&e, SPELL_NAME, 0);

        doc_newline(doc);
        doc_printf(doc, "<color:U>Activation:</color><tab:12><color:B>%s</color>\n", res);

        if (o_ptr->ident & IDENT_FULL)
        {
            int fail = effect_calc_fail_rate(&e);

            res = do_effect(&e, SPELL_INFO, 0);
            if (res && strlen(res))
                doc_printf(doc, "<color:U>Info:</color><tab:12>%s\n", res);
            doc_printf(doc, "<color:U>Fail:</color><tab:12>%d.%d%%\n", fail/10, fail%10);
            if (e.cost)
                doc_printf(doc, "<color:U>Timeout:</color><tab:12>%d\n", e.cost);
        }

        doc_newline(doc);
    }
}

static void _display_ignore(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE], doc_ptr doc)
{
    if (have_flag(flgs, TR_IGNORE_ACID) &&
        have_flag(flgs, TR_IGNORE_ELEC) &&
        have_flag(flgs, TR_IGNORE_FIRE) &&
        have_flag(flgs, TR_IGNORE_COLD))
    {
        doc_insert(doc, "<color:B>It cannot be harmed by the elements.</color>\n");
    }
    else
    {
        vec_ptr v = vec_alloc((vec_free_f)string_free);

        if (have_flag(flgs, TR_IGNORE_ACID))
            vec_add(v, _get_res_name(RES_ACID));
        if (have_flag(flgs, TR_IGNORE_ELEC))
            vec_add(v, _get_res_name(RES_ELEC));
        if (have_flag(flgs, TR_IGNORE_FIRE))
            vec_add(v, _get_res_name(RES_FIRE));
        if (have_flag(flgs, TR_IGNORE_COLD))
            vec_add(v, _get_res_name(RES_COLD));

        if (vec_length(v) > 0)
        {
            doc_insert(doc, "It cannot be harmed by ");
            _print_list(v, doc, ',', '.');
            doc_newline(doc);
        }

        vec_free(v);
    }
}

static void _display_autopick(object_type *o_ptr, doc_ptr doc)
{
    if (destroy_debug)
    {
        int idx = is_autopick(o_ptr);
        if (idx >= 0)
        {
            string_ptr s = autopick_line_from_entry(&autopick_list[idx], AUTOPICK_COLOR_CODED);
            doc_printf(doc, "<color:r>Autopick:</color> <indent><style:indent>%s</style></indent>\n", string_buffer(s));
            string_free(s);
        }
    }
}

static void _lite_display_doc(object_type *o_ptr, doc_ptr doc)
{
    if (o_ptr->tval != TV_LITE) return;
    if (o_ptr->name2 == EGO_LITE_DARKNESS || have_flag(o_ptr->art_flags, TR_DARKNESS))
    {
        doc_insert(doc, "It provides no light.\n");

        if (o_ptr->sval == SV_LITE_LANTERN)
            doc_insert(doc, "It decreases radius of light source by 2.\n");
        else if (o_ptr->sval == SV_LITE_TORCH)
            doc_insert(doc, "It decreases radius of light source by 1.\n");
        else
            doc_insert(doc, "It decreases radius of light source by 3.\n");
    }
    else if (o_ptr->name1 || o_ptr->art_name)
    {
        if (o_ptr->name1 == ART_EYE_OF_VECNA)
            doc_insert(doc, "It allows you to see in the dark.\n");
        else
            doc_insert(doc, "It provides light (radius 3) forever.\n");
    }
    else if (o_ptr->name2 == EGO_LITE_EXTRA_LIGHT)
    {
        if (o_ptr->sval == SV_LITE_FEANOR)
            doc_insert(doc, "It provides light (radius 3) forever.\n");
        else if (o_ptr->sval == SV_LITE_LANTERN)
            doc_insert(doc, "It provides light (radius 3) when fueled.\n");
        else
            doc_insert(doc, "It provides light (radius 2) when fueled.\n");
    }
    else
    {
        if (o_ptr->sval == SV_LITE_FEANOR)
            doc_insert(doc, "It provides light (radius 2) forever.\n");
        else if (o_ptr->sval == SV_LITE_LANTERN)
            doc_insert(doc, "It provides light (radius 2) when fueled.\n");
        else
            doc_insert(doc, "It provides light (radius 1) when fueled.\n");
    }
    if (o_ptr->name2 == EGO_LITE_DURATION)
        doc_insert(doc, "It provides light for much longer time.\n");
}

/* Public Interface */
extern void obj_display(object_type *o_ptr)
{
    obj_display_rect(o_ptr, ui_menu_rect());
}

extern void obj_display_rect(object_type *o_ptr, rect_t display)
{
    doc_ptr doc = doc_alloc(MIN(display.cx, 72));

    if (display.cx > 80)
        display.cx = 80;

    obj_display_doc(o_ptr, doc);

    screen_save();
    if (doc_cursor(doc).y < display.cy - 3)
    {
        doc_insert(doc, "\n[Press Any Key to Continue]\n\n");
        doc_sync_term(doc, doc_range_all(doc), doc_pos_create(display.x, display.y));
        inkey();
    }
    else
    {
        doc_display_aux(doc, "Object Info", 0, display);
    }
    screen_load();

    doc_free(doc);
}

extern void obj_display_doc(object_type *o_ptr, doc_ptr doc)
{
    u32b flgs[TR_FLAG_SIZE];

    /* Devices need special handling. For one thing, they cannot be equipped, so
       that most flags are not used, and those that are generally mean something
       different (e.g. TR_HOLD_LIFE means no charge draining). */
    if (object_is_device(o_ptr) || o_ptr->tval == TV_POTION || o_ptr->tval == TV_SCROLL)
    {
        device_display_doc(o_ptr, doc);
        return;
    }

    object_flags_known(o_ptr, flgs);

    _display_name(o_ptr, doc);
    doc_insert(doc, "  <indent>");
    _display_desc(o_ptr, doc);
    doc_insert(doc, "<style:indent>"); /* Indent a bit when word wrapping long lines */

    if (o_ptr->tval == TV_LITE)
        _lite_display_doc(o_ptr, doc);

    _display_stats(o_ptr, flgs, doc);
    _display_sustains(o_ptr, flgs, doc);
    _display_other_pval(o_ptr, flgs, doc);
    _display_slays(o_ptr, flgs, doc);
    _display_brands(o_ptr, flgs, doc);
    _display_resists(o_ptr, flgs, doc);
    _display_abilities(o_ptr, flgs, doc);
    _display_auras(o_ptr, flgs, doc);
    _display_extra(o_ptr, flgs, doc);
    _display_activation(o_ptr, doc);
    _display_curses(o_ptr, flgs, doc);
    _display_ignore(o_ptr, flgs, doc);

    if (object_is_ego(o_ptr) && !ego_is_aware(o_ptr->name2))
        doc_printf(doc, "You are unfamiliar with this ego type. To learn the basic attributes of this ego type, you need to *identify* or sell this object.\n");
    else if (object_is_artifact(o_ptr) && !(o_ptr->ident & IDENT_FULL))
        doc_printf(doc, "This object is an artifact, a unique object whose powers you must learn by *identifying* or selling this object.\n");
    else if (!(o_ptr->ident & IDENT_FULL))
        doc_printf(doc, "This object may have additional powers which you may learn by *identifying* or selling this object.\n");

    _display_autopick(o_ptr, doc);

    doc_insert(doc, "</style></indent>\n");
}

extern void device_display_doc(object_type *o_ptr, doc_ptr doc)
{
    u32b    flgs[TR_FLAG_SIZE];
    int     net = 0;
    int     boost = 0;
    vec_ptr v = NULL;

    _display_name(o_ptr, doc);
    doc_insert(doc, "  <indent>");
    _display_desc(o_ptr, doc);

    if (o_ptr->tval == TV_SCROLL || o_ptr->tval == TV_POTION)
    {
        doc_printf(doc, "%s\n\n", do_device(o_ptr, SPELL_DESC, 0));
        if (o_ptr->ident & IDENT_FULL)
        {
            cptr info = do_device(o_ptr, SPELL_INFO, 0);
            if (info && strlen(info))
                doc_printf(doc, "<color:U>Info: </color>%s\n", info);
            if (o_ptr->tval == TV_SCROLL)
            {
                int fail = device_calc_fail_rate(o_ptr);
                doc_printf(doc, "<color:U>Fail: </color>%d.%d%%\n", fail/10, fail%10);
            }
        }
        _display_autopick(o_ptr, doc);
        doc_insert(doc, "</indent>\n");
        return;
    }

    if (!object_is_device(o_ptr) || !object_is_known(o_ptr))
    {
        _display_autopick(o_ptr, doc);
        doc_insert(doc, "</indent>\n");
        return;
    }

    object_flags_known(o_ptr, flgs);
    if (devicemaster_is_speciality(o_ptr))
        boost = device_power_aux(100, p_ptr->device_power + p_ptr->lev/10) - 100;
    else
        boost = device_power(100) - 100;

    if (o_ptr->ident & IDENT_FULL)
    {
        int sp = device_sp(o_ptr);
        int max_sp = device_max_sp(o_ptr);

        doc_insert(doc, "<color:U>This device has the following magical strength:</color>\n");
        doc_printf(doc, "Power  : <color:G>%d</color>\n", device_level(o_ptr));
        doc_printf(doc, "Mana   : <color:%c>%d</color>/<color:G>%d</color>\n",
                    (sp < max_sp) ? 'y' : 'G', sp, max_sp);
    }
    else
    {
        doc_insert(doc, "<color:U>This device has the following magical strength:</color>\n");
        doc_insert(doc, "Power  : <color:G>?</color>\n");
        doc_insert(doc, "Mana   : <color:G>?</color>/<color:G>?</color>\n");
    }

    v = vec_alloc((vec_free_f)string_free);
    net = _calc_net_bonus(o_ptr->pval, flgs, TR_SPEED, TR_DEC_SPEED);
    if (net)
        vec_add(v, string_alloc_format("<color:%c>%+d Quickness</color>", (net > 0) ? 'G' : 'r', net));

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_DEVICE_POWER, TR_INVALID);
    if (net)
    {
        int        pct = device_power_aux(100, net) - 100;
        string_ptr s = string_alloc_format("<color:%c>%+d%% to Power</color>", (net > 0) ? 'G' : 'r', pct);
        vec_add(v, s);
    }

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_EASY_SPELL, TR_INVALID);
    if (net > 0)
        vec_add(v, string_copy_s("<color:G>Easy Use</color>"));
    else if (net < 0)
        vec_add(v, string_copy_s("<color:r>Hard Use</color>"));

    net = _calc_net_bonus(o_ptr->pval, flgs, TR_REGEN, TR_INVALID);
    if (net)
        vec_add(v, string_alloc_format("<color:%c>%+d to Regeneration</color>", (net > 0) ? 'G' : 'r', net));

    if (have_flag(flgs, TR_HOLD_LIFE))
        vec_add(v, string_copy_s("<color:y>Hold Charges</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Extra  : <indent>");
        _print_list(v, doc, ';', '\0');
        doc_insert(doc, "</indent>\n\n");
    }
    else
        doc_newline(doc);

    vec_free(v);

    if (o_ptr->activation.type != EFFECT_NONE)
    {
        if (o_ptr->ident & IDENT_FULL)
        {
            int  fail = device_calc_fail_rate(o_ptr);
            int  charges = device_sp(o_ptr) / o_ptr->activation.cost;
            int  max_charges = device_max_sp(o_ptr) / o_ptr->activation.cost;
            cptr desc;

            doc_insert(doc, "<color:U>This device is loaded with a spell:</color>\n");
            doc_printf(doc, "Spell  : <color:B>%s</color>\n", do_device(o_ptr, SPELL_NAME, boost));
            desc = do_device(o_ptr, SPELL_INFO, boost);
            if (desc && strlen(desc))
                doc_printf(doc, "Info   : <color:w>%s</color>\n", desc);
            doc_printf(doc, "Level  : <color:G>%d</color>\n", o_ptr->activation.difficulty);
            doc_printf(doc, "Cost   : <color:G>%d</color>\n", o_ptr->activation.cost);
            doc_printf(doc, "Charges: <color:%c>%d</color>/<color:G>%d</color>\n",
                        (charges < max_charges) ? 'y' : 'G', charges, max_charges);
            doc_printf(doc, "Fail   : <color:G>%d.%d%%</color>\n", fail/10, fail%10);
            doc_printf(doc, "Desc   : <indent>%s</indent>\n\n", do_device(o_ptr, SPELL_DESC, boost));
        }
        else
        {
            doc_insert(doc, "<color:U>This device is loaded with a spell:</color>\n");
            doc_printf(doc, "Spell: <color:B>%s</color>\n", do_device(o_ptr, SPELL_NAME, boost));
            doc_printf(doc, "Desc : <indent>%s</indent>\n\n", do_device(o_ptr, SPELL_DESC, boost));
        }
    }

    doc_insert(doc, "<style:indent>"); /* Indent a bit when word wrapping long lines */

    if (object_is_ego(o_ptr) && !(o_ptr->ident & IDENT_FULL))
        doc_printf(doc, "This object may have additional powers which you may learn by *identifying* or selling this object.\n");
    else if (!(o_ptr->ident & IDENT_FULL))
        doc_printf(doc, "You may *identify* or sell this object to learn more about this device.\n");

    _display_ignore(o_ptr, flgs, doc);

    _display_autopick(o_ptr, doc);
    doc_insert(doc, "</style></indent>\n");
}
