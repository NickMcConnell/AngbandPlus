/* Display Monster Lore to the User
   Adapted from the ancient roff_aux() but redesigned for a more
   readable display; a 'Monster Sheet' if you will. */

#include "angband.h"

#include <stdlib.h>
#include <assert.h>

extern void mon_display(monster_race *r_ptr);
extern void mon_display_rect(monster_race *r_ptr, rect_t display);
extern void mon_display_doc(monster_race *r_ptr, doc_ptr doc);

static void _display_basic(monster_race *r_ptr, doc_ptr doc);
static void _display_resists(monster_race *r_ptr, doc_ptr doc);
static void _display_spells(monster_race *r_ptr, doc_ptr doc);
static void _display_attacks(monster_race *r_ptr, doc_ptr doc);
static void _display_other(monster_race *r_ptr, doc_ptr doc);
static void _display_kills(monster_race *r_ptr, doc_ptr doc);
static void _display_desc(monster_race *r_ptr, doc_ptr doc);

static void _print_list(vec_ptr v, doc_ptr doc, char sep, char term);
static string_ptr _get_res_name(int res);

/**************************************************************************
 * Helpers
 **************************************************************************/
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

static string_ptr _get_res_name(int res)
{
    return string_alloc_format(
        "<color:%c>%s</color>",
        attr_to_attr_char(res_color(res)),
        res_name(res)
    );
}

static bool _easy_lore(monster_race *r_ptr)
{
    if (easy_lore) return TRUE;
    if (p_ptr->wizard) return TRUE;
    if (spoiler_hack) return TRUE;
    if (r_ptr->r_xtra1 & MR1_LORE) return TRUE; /* Probing */
    return FALSE;
}

static bool _know_armor_hp(monster_race *r_ptr)
{
    int level = r_ptr->level;
    int kills = r_ptr->r_tkills;

    if (_easy_lore(r_ptr)) return TRUE;

    if (kills > 304 / (4 + level)) return TRUE;
    else if ((r_ptr->flags1 & RF1_UNIQUE) && kills > 304 / (38 + (5 * level) / 4)) return TRUE;

    return FALSE;
}

static bool _know_damage(monster_race *r_ptr, int i)
{
    int l = r_ptr->level + 4;
    int ct = r_ptr->r_blows[i];
    int d1 = r_ptr->blow[i].d_dice;
    int d2 = r_ptr->blow[i].d_side;
    int d = d1 * d2;

    if (_easy_lore(r_ptr)) return TRUE;

    if (d >= (l*MAX_UCHAR)/80)
        d = (l*MAX_UCHAR-1)/80;

    if (l * ct > 80 * d) return TRUE;
    else if ((r_ptr->flags1 & RF1_UNIQUE) && l * 2 * ct > 80 * d) return TRUE;

    return FALSE;
}

static bool _know_alertness(monster_race *r_ptr)
{
    int wake = r_ptr->r_wake;
    int sleep = r_ptr->sleep;
    int ignore = r_ptr->r_ignore;

    if (_easy_lore(r_ptr)) return TRUE;
    if (wake * wake > sleep) return TRUE;
    if (ignore == MAX_UCHAR) return TRUE;
    if (!sleep && r_ptr->r_tkills >= 10) return TRUE;

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
static void _display_level(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "Level   : ");
    if (r_ptr->level == 0)
        doc_insert(doc, "<color:G>Town</color>");
    else if (_easy_lore(r_ptr) || r_ptr->r_tkills > 0)
        doc_printf(doc, "<color:G>%d</color>", (int)r_ptr->level);
    else
        doc_insert(doc, "<color:y>?</color>");
    doc_newline(doc);
}
static void _display_ac(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "AC      : ");
    if (_know_armor_hp(r_ptr))
        doc_printf(doc, "<color:G>%d</color>", (int)r_ptr->ac);
    else
        doc_insert(doc, "<color:y>?</color>");
    doc_newline(doc);
}
static void _display_hp(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, "HP      : ");
    if (_know_armor_hp(r_ptr))
    {
        if ((r_ptr->flags1 & RF1_FORCE_MAXHP) || r_ptr->hside == 1)
        {
            int hp = r_ptr->hdice * r_ptr->hside;
            doc_printf(doc, "<color:G>%d</color>", hp);
        }
        else
        {
            doc_printf(doc, "<color:G>%dd%d</color>", r_ptr->hdice, r_ptr->hside);
        }
    }
    else
        doc_insert(doc, "<color:y>?</color>");
    doc_newline(doc);
}
static void _display_speed(monster_race *r_ptr, doc_ptr doc)
{                        /* v~~~~~~byte */
    int speed = (int)r_ptr->speed - 110;
    int rand = 0;
    doc_printf(doc, "Speed: <color:%c>%+d</color>", _speed_color(speed), speed);

    if (r_ptr->flags1 & RF1_RAND_50) rand += 50;
    if (r_ptr->flags1 & RF1_RAND_25) rand += 25;
    if (rand == 75) doc_insert(doc, " <color:r>Extremely Erratic</color>");
    else if (rand == 50) doc_insert(doc, " <color:R>Somewhat Erratic</color>");
    else if (rand == 25) doc_insert(doc, " <color:o>A Bit Erratic</color>");

    if (r_ptr->flags1 & RF1_NEVER_MOVE) doc_insert(doc, ", <color:u>Stationary</color>");

    doc_newline(doc);
}
static void _display_alertness(monster_race *r_ptr, doc_ptr doc)
{
    if (_know_alertness(r_ptr))
    {
        doc_insert(doc, "Alert: ");
        if (r_ptr->sleep > 200)
            doc_insert(doc, "<color:D>Ignores Intruders</color>");
        else if (r_ptr->sleep > 95)
            doc_insert(doc, "<color:w>Very Inattentive</color>");
        else if (r_ptr->sleep > 75)
            doc_insert(doc, "<color:W>Inattentive</color>");
        else if (r_ptr->sleep > 45)
            doc_insert(doc, "<color:U>Overlooks</color>");
        else if (r_ptr->sleep > 25)
            doc_insert(doc, "<color:y>Unseeing</color>");
        else if (r_ptr->sleep > 10)
            doc_insert(doc, "<color:y>Fairly Unseeing</color>");
        else if (r_ptr->sleep > 5)
            doc_insert(doc, "<color:o>Fairly Observant</color>");
        else if (r_ptr->sleep > 3)
            doc_insert(doc, "<color:R>Observant</color>");
        else if (r_ptr->sleep > 1)
            doc_insert(doc, "<color:r>Very Observant</color>");
        else if (r_ptr->sleep > 0)
            doc_insert(doc, "<color:r>Vigilant</color>");
        else
            doc_insert(doc, "<color:v>Ever Vigilant</color>");
        doc_printf(doc, " <color:G>(%d')</color>\n", 10 * r_ptr->aaf);
    }
}
static void _display_type(monster_race *r_ptr, doc_ptr doc)
{
    vec_ptr v = vec_alloc((vec_free_f)string_free);
    doc_insert(doc, "Type : <indent><style:indent>");

    if (r_ptr->flags2 & RF2_ELDRITCH_HORROR)
        vec_add(v, string_copy_s("<color:v>Sanity Blasting</color>"));
    if (r_ptr->flags3 & RF3_ANIMAL)
        vec_add(v, string_copy_s("<color:G>Natural</color>"));
    if (r_ptr->flags3 & RF3_EVIL)
        vec_add(v, string_copy_s("<color:D>Evil</color>"));
    if (r_ptr->flags3 & RF3_GOOD)
        vec_add(v, string_copy_s("<color:y>Good</color>"));
    if (r_ptr->flags3 & RF3_UNDEAD)
        vec_add(v, string_copy_s("<color:v>Undead</color>"));
    if (r_ptr->flags3 & RF3_AMBERITE)
        vec_add(v, string_copy_s("<color:v>Amberite</color>"));
    if (r_ptr->flags3 & RF3_DRAGON)
        vec_add(v, string_copy_s("<color:o>Dragon</color>"));
    if (r_ptr->flags3 & RF3_DEMON)
        vec_add(v, string_copy_s("<color:v>Demon</color>"));
    if (r_ptr->flags3 & RF3_GIANT)
        vec_add(v, string_copy_s("<color:U>Giant</color>"));
    if (r_ptr->flags3 & RF3_TROLL)
        vec_add(v, string_copy_s("<color:B>Troll</color>"));
    if (r_ptr->flags3 & RF3_ORC)
        vec_add(v, string_copy_s("<color:u>Orc</color>"));
    if (r_ptr->flags2 & RF2_HUMAN)
        vec_add(v, string_copy_s("<color:W>Human</color>"));
    if (r_ptr->flags2 & RF2_THIEF)
        vec_add(v, string_copy_s("<color:D>Thief</color>"));
    /*if (r_ptr->flags2 & RF2_QUANTUM)
        vec_add(v, string_copy_s("<color:v>Quantum</color>"));*/
    if (r_ptr->flags1 & RF1_MALE)
        vec_add(v, string_copy_s("<color:b>Male</color>"));
    if (r_ptr->flags1 & RF1_FEMALE)
        vec_add(v, string_copy_s("<color:R>Female</color>")); /* Pink? */

    _print_list(v, doc, ',', '\0');
    vec_free(v);
    doc_insert(doc, "</style></indent>\n");
}
static void _display_basic(monster_race *r_ptr, doc_ptr doc)
{
    doc_printf(doc, "Name    : <indent><style:indent><color:B>%s</color> ", r_name + r_ptr->name);
    assert(r_ptr->d_char);
    doc_printf(doc, "(<color:%c>%c</color>", attr_to_attr_char(r_ptr->d_attr), r_ptr->d_char);
    if (use_graphics && (r_ptr->x_char != r_ptr->d_char || r_ptr->x_attr != r_ptr->d_attr))
    {
        doc_insert(doc, " / ");
        doc_insert_char(doc, r_ptr->x_attr, r_ptr->x_char);
    }
    doc_insert(doc, ")</style></indent>\n");

    {
        doc_ptr cols[2];

        cols[0] = doc_alloc(20);
        cols[1] = doc_alloc(MAX(20, MIN(50, doc_width(doc) - 20))); /* Monster Recall Terminal */

        /* Column 1 */
        _display_level(r_ptr, cols[0]);
        _display_ac(r_ptr, cols[0]);
        _display_hp(r_ptr, cols[0]);

        /* Column 2 */
        _display_speed(r_ptr, cols[1]);
        _display_alertness(r_ptr, cols[1]);
        _display_type(r_ptr, cols[1]);

        doc_insert_cols(doc, cols, 2, 0);

        doc_free(cols[0]);
        doc_free(cols[1]);
    }
}

/**************************************************************************
 * Resists
 **************************************************************************/
static void _display_resists(monster_race *r_ptr, doc_ptr doc)
{
    int        i;
    int        ct = 0;
    vec_ptr    v = vec_alloc((vec_free_f)string_free);
    const int  flags[RES_MAX] = {
        RFR_RES_ACID, RFR_RES_ELEC, RFR_RES_FIRE, RFR_RES_COLD, RFR_RES_POIS,
        RFR_RES_LITE, RFR_RES_DARK, -1, RFR_RES_NETH, RFR_RES_NEXU, RFR_RES_SOUN,
        RFR_RES_SHAR, RFR_RES_CHAO, RFR_RES_DISE, RFR_RES_TIME, -1, -1, -1};

    for (i = 0; i < RES_MAX; i++)
    {
        int which = flags[i];
        if (which >= 0 && (r_ptr->flagsr & which))
            vec_add(v, _get_res_name(i));
    }
    if ((r_ptr->flagsr & RFR_RES_TELE) && !(r_ptr->flags1 & RF1_UNIQUE))
        vec_add(v, string_copy_s("<color:o>Teleportation</color>"));
    if (r_ptr->flagsr & RFR_RES_WATE)
        vec_add(v, string_copy_s("<color:b>Water</color>"));
    if (r_ptr->flagsr & RFR_RES_PLAS)
        vec_add(v, string_copy_s("<color:R>Plasma</color>"));
    if (r_ptr->flagsr & RFR_RES_WALL)
        vec_add(v, string_copy_s("<color:u>Force</color>"));
    if (r_ptr->flagsr & RFR_RES_GRAV)
        vec_add(v, string_copy_s("<color:s>Gravity</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Resist  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Immunities */
    vec_clear(v);
    if (r_ptr->flagsr & RFR_RES_ALL)
        vec_add(v, string_copy_s("<color:y>Everything</color>"));
    if (r_ptr->flagsr & RFR_IM_ACID)
        vec_add(v, _get_res_name(RES_ACID));
    if (r_ptr->flagsr & RFR_IM_ELEC)
        vec_add(v, _get_res_name(RES_ELEC));
    if (r_ptr->flagsr & RFR_IM_FIRE)
        vec_add(v, _get_res_name(RES_FIRE));
    if (r_ptr->flagsr & RFR_IM_COLD)
        vec_add(v, _get_res_name(RES_COLD));
    if (r_ptr->flagsr & RFR_IM_POIS)
        vec_add(v, _get_res_name(RES_POIS));
    if (r_ptr->flags3 & RF3_NO_FEAR)
        vec_add(v, string_copy_s("<color:s>Fear</color>"));
    if (r_ptr->flags3 & RF3_NO_STUN)
        vec_add(v, string_copy_s("<color:o>Stunning</color>"));
    if (r_ptr->flags3 & RF3_NO_CONF)
        vec_add(v, string_copy_s("<color:U>Confusion</color>"));
    if (r_ptr->flags3 & RF3_NO_SLEEP)
        vec_add(v, string_copy_s("<color:b>Sleep</color>"));
    if ((r_ptr->flagsr & RFR_RES_TELE) && (r_ptr->flags1 & RF1_UNIQUE))
        vec_add(v, string_copy_s("<color:o>Teleportation</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Immune  : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Vulnerabilities */
    vec_clear(v);
    if (r_ptr->flags3 & RF3_HURT_FIRE)
        vec_add(v, _get_res_name(RES_FIRE));
    if (r_ptr->flags3 & RF3_HURT_COLD)
        vec_add(v, _get_res_name(RES_COLD));
    if (r_ptr->flags3 & RF3_HURT_LITE)
        vec_add(v, _get_res_name(RES_LITE));
    if (r_ptr->flags3 & RF3_HURT_ROCK)
        vec_add(v, string_copy_s("<color:u>Rock Remover</color>"));

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
static void _display_frequency(monster_race *r_ptr, doc_ptr doc)
{
    int pct = 0;
    if (r_ptr->r_cast_spell > 100 || _easy_lore(r_ptr))
        pct = r_ptr->freq_spell;
    else if (r_ptr->r_cast_spell)
        pct = ((r_ptr->freq_spell + 9) / 10) * 10; /* ?? */
    if (pct)
    {
        vec_ptr v = vec_alloc((vec_free_f)string_free);
        doc_printf(doc, "Spells  : <color:G>%d%%</color> ", pct);
        if (r_ptr->flags2 & RF2_SMART)
            vec_add(v, string_copy_s("<color:y>Intelligent</color>"));
        if (r_ptr->flags2 & RF2_POWERFUL)
            vec_add(v, string_copy_s("<color:v>Powerful</color>"));
        _print_list(v, doc, ',', '\0');
        vec_free(v);
    }
    else
    {
        doc_printf(doc, "Spells  : <color:y>?%%</color>");
    }
    doc_newline(doc);
}
static void _display_spells(monster_race *r_ptr, doc_ptr doc)
{
    int        i;
    int        ct = 0;
    bool       first = TRUE;
    vec_ptr    v = vec_alloc((vec_free_f)string_free);

    /* Breaths */
    const int  flags[RES_MAX] = {
        RF4_BR_ACID, RF4_BR_ELEC, RF4_BR_FIRE, RF4_BR_COLD, RF4_BR_POIS,
        RF4_BR_LITE, RF4_BR_DARK, RF4_BR_CONF, RF4_BR_NETH, RF4_BR_NEXU, RF4_BR_SOUN,
        RF4_BR_SHAR, RF4_BR_CHAO, RF4_BR_DISE, RF4_BR_TIME, -1, -1, -1};

    for (i = 0; i < RES_MAX; i++)
    {
        int which = flags[i];
        if (which >= 0 && (r_ptr->flags4 & which))
            vec_add(v, _get_res_name(i));
    }
    if (r_ptr->flags4 & RF4_BR_INER)
        vec_add(v, string_copy_s("<color:s>Inertia</color>"));
    if (r_ptr->flags4 & RF4_BR_GRAV)
        vec_add(v, string_copy_s("<color:s>Gravity</color>"));
    if (r_ptr->flags4 & RF4_BR_PLAS)
        vec_add(v, string_copy_s("<color:R>Plasma</color>"));
    if (r_ptr->flags4 & RF4_BR_WALL)
        vec_add(v, string_copy_s("<color:u>Force</color>"));
    if (r_ptr->flags4 & RF4_BR_MANA)
        vec_add(v, string_copy_s("<color:B>Mana</color>"));
    if (r_ptr->flags4 & RF4_BR_NUKE)
        vec_add(v, string_copy_s("<color:G>Toxic Waste</color>"));
    if (r_ptr->flags4 & RF4_BR_DISI)
        vec_add(v, string_copy_s("<color:s>Disintegration</color>"));
    if (r_ptr->flags4 & RF4_BR_STORM)
        vec_add(v, string_copy_s("<color:b>Storm</color>"));

    if (vec_length(v))
    {
        if (first)
        {
            _display_frequency(r_ptr, doc);
            first = FALSE;
        }
        doc_insert(doc, "Breathe : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Offense */
    vec_clear(v);

    if (r_ptr->flags4 & RF4_ROCKET)
        vec_add(v, string_copy_s("<color:u>Rocket</color>"));
    if (r_ptr->flags4 & RF4_SHOOT)
        vec_add(v, string_copy_s("<color:u>Fire Arrow</color>"));

    if (r_ptr->flags5 & RF5_BA_ACID)
        vec_add(v, string_copy_s("<color:g>Acid Ball</color>"));
    if (r_ptr->flags5 & RF5_BA_ELEC)
        vec_add(v, string_copy_s("<color:b>Lightning Ball</color>"));
    if (r_ptr->flags5 & RF5_BA_FIRE)
        vec_add(v, string_copy_s("<color:r>Fire Ball</color>"));
    if (r_ptr->flags5 & RF5_BA_COLD)
        vec_add(v, string_copy_s("<color:W>Frost Ball</color>"));
    if (r_ptr->flags5 & RF5_BA_POIS)
        vec_add(v, string_copy_s("<color:G>Poison Ball</color>"));
    if (r_ptr->flags5 & RF5_BA_NETH)
        vec_add(v, string_copy_s("<color:D>Nether Ball</color>"));
    if (r_ptr->flags5 & RF5_BA_WATE)
        vec_add(v, string_copy_s("<color:b>Water Ball</color>"));
    if (r_ptr->flags4 & RF4_BA_NUKE)
        vec_add(v, string_copy_s("<color:G>Ball of Radiation</color>"));
    if (r_ptr->flags4 & RF4_THROW)
        vec_add(v, string_copy_s("<color:s>Throw Boulder</color>"));
    if (r_ptr->flags5 & RF5_BA_MANA)
        vec_add(v, string_copy_s("<color:B>Mana Storm</color>"));
    if (r_ptr->flags5 & RF5_BA_DARK)
        vec_add(v, string_copy_s("<color:D>Darkness Storm</color>"));
    if (r_ptr->flags5 & RF5_BA_LITE)
        vec_add(v, string_copy_s("<color:y>Starburst</color>"));
    if (r_ptr->flags4 & RF4_BA_CHAO)
        vec_add(v, string_copy_s("<color:v>Invoke Logrus</color>"));
    if (r_ptr->flags6 & RF6_HAND_DOOM)
        vec_add(v, string_copy_s("<color:v>Hand of Doom</color>"));
    if (r_ptr->flags6 & RF6_PSY_SPEAR)
        vec_add(v, string_copy_s("<color:y>Psycho-spear</color>"));
    if (r_ptr->flags5 & RF5_DRAIN_MANA)
        vec_add(v, string_copy_s("<color:s>Drain Mana</color>"));
    if (r_ptr->flags5 & RF5_MIND_BLAST)
        vec_add(v, string_copy_s("<color:R>Mind Blast</color>"));
    if (r_ptr->flags5 & RF5_BRAIN_SMASH)
        vec_add(v, string_copy_s("<color:r>Brain Smash</color>"));
    if (r_ptr->flags5 & RF5_CAUSE_1)
        vec_add(v, string_copy_s("<color:W>Cause Light Wounds</color>"));
    if (r_ptr->flags5 & RF5_CAUSE_2)
        vec_add(v, string_copy_s("<color:W>Cause Serious Wounds</color>"));
    if (r_ptr->flags5 & RF5_CAUSE_3)
        vec_add(v, string_copy_s("<color:W>Cause Critical Wounds</color>"));
    if (r_ptr->flags5 & RF5_CAUSE_4)
        vec_add(v, string_copy_s("<color:W>Cause Mortal Wounds</color>"));
    if (r_ptr->flags5 & RF5_BO_ACID)
        vec_add(v, string_copy_s("<color:g>Acid Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_ELEC)
        vec_add(v, string_copy_s("<color:b>Lightning Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_FIRE)
        vec_add(v, string_copy_s("<color:r>Fire Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_COLD)
        vec_add(v, string_copy_s("<color:W>Frost Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_NETH)
        vec_add(v, string_copy_s("<color:D>Nether Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_WATE)
        vec_add(v, string_copy_s("<color:b>Water Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_MANA)
        vec_add(v, string_copy_s("<color:B>Mana Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_PLAS)
        vec_add(v, string_copy_s("<color:R>Plasma Bolt</color>"));
    if (r_ptr->flags5 & RF5_BO_ICEE)
        vec_add(v, string_copy_s("<color:w>Ice Bolt</color>"));
    if (r_ptr->flags5 & RF5_MISSILE)
        vec_add(v, string_copy_s("<color:s>Magic Missile</color>"));
    if (r_ptr->flags5 & RF5_SCARE)
        vec_add(v, string_copy_s("<color:s>Terrify</color>"));
    if (r_ptr->flags5 & RF5_BLIND)
        vec_add(v, string_copy_s("<color:D>Blind</color>"));
    if (r_ptr->flags5 & RF5_CONF)
        vec_add(v, string_copy_s("<color:U>Confuse</color>"));
    if (r_ptr->flags5 & RF5_SLOW)
        vec_add(v, string_copy_s("<color:u>Slow</color>"));
    if (r_ptr->flags5 & RF5_HOLD)
        vec_add(v, string_copy_s("<color:r>Paralyze</color>"));

    if (vec_length(v))
    {
        if (first)
        {
            _display_frequency(r_ptr, doc);
            first = FALSE;
        }
        doc_insert(doc, "Offense : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Defense */
    vec_clear(v);

    if (r_ptr->flags4 & RF4_SHRIEK)
        vec_add(v, string_copy_s("<color:W>Shriek</color>"));
    if (r_ptr->flags6 & RF6_HASTE)
        vec_add(v, string_copy_s("<color:G>Haste Self</color>"));
    if (r_ptr->flags6 & RF6_HEAL)
        vec_add(v, string_copy_s("<color:w>Heal Self</color>"));
    if (r_ptr->flags6 & RF6_INVULNER)
        vec_add(v, string_copy_s("<color:w>Invulnerability</color>"));
    if (r_ptr->flags4 & RF4_DISPEL)
        vec_add(v, string_copy_s("<color:W>Dispel Magic</color>"));
    if (r_ptr->flags4 & RF4_ANTI_MAGIC)
        vec_add(v, string_copy_s("<color:W>Anti-magic</color>"));
    if (r_ptr->flags4 & RF4_POLY)
        vec_add(v, string_copy_s("<color:R>Polymorph</color>"));
    if (r_ptr->flags6 & RF6_BLINK)
        vec_add(v, string_copy_s("<color:u>Blink Self</color>"));
    if (r_ptr->flags6 & RF6_TPORT)
        vec_add(v, string_copy_s("<color:o>Teleport Self</color>"));
    if (r_ptr->flags6 & RF6_WORLD)
        vec_add(v, string_copy_s("<color:B>Stop Time</color>"));
    if (r_ptr->flags6 & RF6_TELE_TO)
        vec_add(v, string_copy_s("<color:U>Teleport To</color>"));
    if (r_ptr->flags6 & RF6_TELE_AWAY)
        vec_add(v, string_copy_s("<color:u>Teleport Away</color>"));
    if (r_ptr->flags6 & RF6_TELE_LEVEL)
        vec_add(v, string_copy_s("<color:o>Teleport Level</color>"));
    if (r_ptr->flags6 & RF6_DARKNESS)
    {
        if ( p_ptr->pclass != CLASS_NINJA
          || (r_ptr->flags3 & (RF3_UNDEAD | RF3_HURT_LITE))
          || (r_ptr->flags7 & RF7_DARK_MASK) )
        {
            vec_add(v, string_copy_s("<color:D>Create Darkness</color>"));
        }
        else
        {
            vec_add(v, string_copy_s("<color:y>Create Light</color>"));
        }
    }
    if (r_ptr->flags6 & RF6_TRAPS)
        vec_add(v, string_copy_s("<color:b>Create Traps</color>"));
    if (r_ptr->flags6 & RF6_FORGET)
        vec_add(v, string_copy_s("<color:b>Cause Amnesia</color>"));
    if (r_ptr->flags6 & RF6_RAISE_DEAD)
        vec_add(v, string_copy_s("<color:r>Raise Dead</color>"));
    if (r_ptr->flags6 & RF6_SPECIAL)
        vec_add(v, string_copy_s("<color:v>Special</color>"));

    if (vec_length(v))
    {
        if (first)
        {
            _display_frequency(r_ptr, doc);
            first = FALSE;
        }
        doc_insert(doc, "Defense : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Summoning */
    vec_clear(v);
    if (r_ptr->flags6 & RF6_S_MONSTER)
        vec_add(v, string_copy_s("<color:s>Monster</color>"));
    if (r_ptr->flags6 & RF6_S_MONSTERS)
        vec_add(v, string_copy_s("<color:W>Monsters</color>"));
    if (r_ptr->flags6 & RF6_S_KIN)
        vec_add(v, string_copy_s("<color:o>Aid</color>"));
    if (r_ptr->flags6 & RF6_S_ANT)
        vec_add(v, string_copy_s("<color:r>Ants</color>"));
    if (r_ptr->flags6 & RF6_S_SPIDER)
        vec_add(v, string_copy_s("<color:D>Spiders</color>"));
    if (r_ptr->flags6 & RF6_S_HOUND)
        vec_add(v, string_copy_s("<color:U>Hounds</color>"));
    if (r_ptr->flags6 & RF6_S_HYDRA)
        vec_add(v, string_copy_s("<color:G>Hydras</color>"));
    if (r_ptr->flags6 & RF6_S_ANGEL)
        vec_add(v, string_copy_s("<color:y>Angel</color>"));
    if (r_ptr->flags6 & RF6_S_DEMON)
        vec_add(v, string_copy_s("<color:r>Demon</color>"));
    if (r_ptr->flags6 & RF6_S_UNDEAD)
        vec_add(v, string_copy_s("<color:D>Undead</color>"));
    if (r_ptr->flags6 & RF6_S_DRAGON)
        vec_add(v, string_copy_s("<color:o>Dragon</color>"));
    if (r_ptr->flags6 & RF6_S_HI_UNDEAD)
        vec_add(v, string_copy_s("<color:D>Greater Undead</color>"));
    if (r_ptr->flags6 & RF6_S_HI_DRAGON)
        vec_add(v, string_copy_s("<color:o>Ancient Dragons</color>"));
    if (r_ptr->flags6 & RF6_S_CYBER)
        vec_add(v, string_copy_s("<color:u>Cyberdemons</color>"));
    if (r_ptr->flags6 & RF6_S_AMBERITES)
        vec_add(v, string_copy_s("<color:v>Lords of Amber</color>"));
    if (r_ptr->flags6 & RF6_S_UNIQUE)
        vec_add(v, string_copy_s("<color:v>Uniques</color>"));

    if (vec_length(v))
    {
        if (first)
        {
            _display_frequency(r_ptr, doc);
            first = FALSE;
        }
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
    switch (method)
    {
    case RBM_HIT:     return "Hit";
    case RBM_TOUCH:   return "Touch";
    case RBM_PUNCH:   return "Punch";
    case RBM_KICK:    return "Kick";
    case RBM_CLAW:    return "Claw";
    case RBM_BITE:    return "Bite";
    case RBM_STING:   return "Sting";
    case RBM_SLASH:   return "Slash";
    case RBM_BUTT:    return "Butt";
    case RBM_CRUSH:   return "Crush";
    case RBM_ENGULF:  return "Engulf";
    case RBM_CHARGE:  return "Charge";
    case RBM_CRAWL:   return "Crawl";
    case RBM_DROOL:   return "Drool";
    case RBM_SPIT:    return "Spit";
    case RBM_EXPLODE: return "Explode";
    case RBM_GAZE:    return "Gaze";
    case RBM_WAIL:    return "Wail";
    case RBM_SPORE:   return "Spores";
    case RBM_BEG:     return "Beg";
    case RBM_INSULT:  return "Insult";
    case RBM_MOAN:    return "Moan";
    case RBM_SHOW:    return "Sing";
    }
    return "Weird";
}
static cptr _effect_desc(int effect)
{
    switch (effect)
    {
    case RBE_SUPERHURT:   return "Critical Hits";
    case RBE_HURT:        return "";
    case RBE_POISON:      return "Poison";
    case RBE_UN_BONUS:    return "Disenchant";
    case RBE_UN_POWER:    return "Drain Charges";
    case RBE_EAT_GOLD:    return "Steal Gold";
    case RBE_EAT_ITEM:    return "Steal Items";
    case RBE_EAT_FOOD:    return "Eat Your Food";
    case RBE_EAT_LITE:    return "Absorb Light";
    case RBE_ACID:        return "Shoot Acid";
    case RBE_ELEC:        return "Electrocute";
    case RBE_FIRE:        return "Burn";
    case RBE_COLD:        return "Freeze";
    case RBE_BLIND:       return "Blind";
    case RBE_CONFUSE:     return "Confuse";
    case RBE_TERRIFY:     return "Terrify";
    case RBE_PARALYZE:    return "Paralyze";
    case RBE_LOSE_STR:    return "Reduce Strength";
    case RBE_LOSE_INT:    return "Reduce Intelligence";
    case RBE_LOSE_WIS:    return "Reduce Wisdom";
    case RBE_LOSE_DEX:    return "Reduce Dexterity";
    case RBE_LOSE_CON:    return "Reduce Constitution";
    case RBE_LOSE_CHR:    return "Reduce Charisma";
    case RBE_LOSE_ALL:    return "Reduce All Stats";
    case RBE_SHATTER:     return "Shatter";
    case RBE_EXP_10:
    case RBE_EXP_20:
    case RBE_EXP_40:
    case RBE_EXP_80:      return "Lower Experience";
    case RBE_DISEASE:     return "Disease";
    case RBE_TIME:        return "Time";
    case RBE_EXP_VAMP:    return "Drain Life Force";
    case RBE_DR_MANA:     return "Drain Mana";
    }
    return "";
}
static int _ct_known_attacks(monster_race *r_ptr)
{
    int ct = 0;
    int i;
    for (i = 0; i < 4; i++)
    {
        if (!r_ptr->blow[i].method) continue;
        if (r_ptr->blow[i].method == RBM_SHOOT) continue;
        if (r_ptr->r_blows[i] || _easy_lore(r_ptr)) ct++;
    }
    return ct;
}
static void _display_attacks(monster_race *r_ptr, doc_ptr doc)
{
    if (r_ptr->flags1 & RF1_NEVER_BLOW)
        doc_insert(doc, "Attacks : <color:D>None</color>\n");
    else if (_ct_known_attacks(r_ptr))
    {
        int i;
        doc_printf(doc, "Attacks : <color:G>%-7.7s %-5.5s Effect</color>\n", "Type", "Dam");
        for (i = 0; i < 4; i++)
        {
            if (!r_ptr->blow[i].method) continue;
            if (r_ptr->blow[i].method == RBM_SHOOT) continue;
            if (!_easy_lore(r_ptr) && !r_ptr->r_blows[i]) continue;

            if (_know_damage(r_ptr, i))
            {
                char dam[10] = {0};
                int dd = r_ptr->blow[i].d_dice;
                int ds = r_ptr->blow[i].d_side;

                if (dd && ds)
                    sprintf(dam, "%dd%d", dd, ds);

                doc_printf(doc, "          %-7.7s %-5.5s %s\n",
                    _method_desc(r_ptr->blow[i].method), dam, _effect_desc(r_ptr->blow[i].effect));
            }
            else
            {
                doc_printf(doc, "          %-7.7s ", _method_desc(r_ptr->blow[i].method));
                doc_insert(doc, "<color:y>  ?  </color> ");
                doc_insert(doc, _effect_desc(r_ptr->blow[i].effect));
                doc_newline(doc);
            }
        }
    }
    else
        doc_insert(doc, "Attacks : <color:y>?</color>\n");

    doc_newline(doc);
}

/**************************************************************************
 * Other Info
 **************************************************************************/
static void _display_other(monster_race *r_ptr, doc_ptr doc)
{
    int        ct = 0;
    vec_ptr    v = vec_alloc((vec_free_f)string_free);

    if (r_ptr->flags2 & RF2_REFLECTING)
        vec_add(v, string_copy_s("<color:o>Reflection</color>"));

    if (r_ptr->flags7 & (RF7_SELF_LITE_1 | RF7_SELF_LITE_2))
        vec_add(v, string_copy_s("<color:y>Shining</color>"));

    if (r_ptr->flags7 & (RF7_SELF_DARK_1 | RF7_SELF_DARK_2))
        vec_add(v, string_copy_s("<color:D>Shrouded in Darkness</color>"));

    if (r_ptr->flags2 & RF2_INVISIBLE)
        vec_add(v, string_copy_s("<color:B>Invisible</color>"));

    if (r_ptr->flags2 & RF2_COLD_BLOOD)
        vec_add(v, string_copy_s("<color:w>Cold Blooded</color>"));

    if (r_ptr->flags2 & RF2_EMPTY_MIND)
        vec_add(v, string_copy_s("<color:o>Shielded from Telepathy</color>"));

    if (r_ptr->flags2 & RF2_WEIRD_MIND)
        vec_add(v, string_copy_s("<color:w>Weird Mind</color>"));

    if (r_ptr->flags2 & RF2_MULTIPLY)
        vec_add(v, string_copy_s("<color:U>Breeds Explosively</color>"));

    if (r_ptr->flags2 & RF2_REGENERATE)
        vec_add(v, string_copy_s("<color:r>Regeneration</color>"));

    if (r_ptr->flags7 & RF7_RIDING)
        vec_add(v, string_copy_s("<color:s>Suitable for Riding</color>"));

    if ((r_ptr->flags7 & RF7_GUARDIAN) && !no_wilderness)
        vec_add(v, string_copy_s("<color:R>Dungeon Guardian</color>"));

    if (vec_length(v))
    {
        doc_insert(doc, "Info    : <indent><style:indent>");
        _print_list(v, doc, ',', '\0');
        doc_insert(doc, "</style></indent>\n");
        ct += vec_length(v);
    }

    /* Auras */
    vec_clear(v);

    if (r_ptr->flags2 & RF2_AURA_REVENGE)
        vec_add(v, string_copy_s("<color:v>Retaliation</color>"));
    if (r_ptr->flags2 & RF2_AURA_FEAR)
        vec_add(v, string_copy_s("<color:v>Fear</color>"));
    if (r_ptr->flags2 & RF2_AURA_FIRE)
        vec_add(v, _get_res_name(RES_FIRE));
    if (r_ptr->flags3 & RF3_AURA_COLD)
        vec_add(v, _get_res_name(RES_COLD));
    if (r_ptr->flags2 & RF2_AURA_ELEC)
        vec_add(v, _get_res_name(RES_ELEC));

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
static void _display_drops(monster_race *r_ptr, doc_ptr doc)
{
    int ct_gold = 0;
    int ct_obj = 0;

    if (_easy_lore(r_ptr))
    {
        if (r_ptr->flags1 & RF1_DROP_4D2) ct_gold += 8;
        if (r_ptr->flags1 & RF1_DROP_3D2) ct_gold += 6;
        if (r_ptr->flags1 & RF1_DROP_2D2) ct_gold += 4;
        if (r_ptr->flags1 & RF1_DROP_1D2) ct_gold += 2;
        if (r_ptr->flags1 & RF1_DROP_90) ct_gold += 1;
        if (r_ptr->flags1 & RF1_DROP_60) ct_gold += 1;

        ct_obj = ct_gold;

        /* Hack -- but only "valid" drops */
        if (r_ptr->flags1 & RF1_ONLY_GOLD) ct_obj = 0;
        if (r_ptr->flags1 & RF1_ONLY_ITEM) ct_gold = 0;
    }
    else
    {
        ct_gold = r_ptr->r_drop_gold;
        ct_obj = r_ptr->r_drop_item;
    }

    if (ct_gold || ct_obj)
    {
        int ct = MAX(ct_gold, ct_obj);
        cptr obj_text = (ct_obj > 1) ? "Objects" : "Object";
        cptr gold_text = (ct_gold > 1) ? "Treasures" : "Treasure";

        doc_insert(doc, "Drops   : ");

        if (ct == 1)
            doc_insert(doc, "1 ");
        else if (ct == 2)
            doc_insert(doc, "1 or 2 ");
        else
            doc_printf(doc, "Up to %d ", ct);

        if (r_ptr->flags1 & RF1_DROP_GREAT)
            doc_insert(doc, "<color:v>Exceptional</color> ");
        else if (r_ptr->flags1 & RF1_DROP_GOOD)
            doc_insert(doc, "<color:r>Good</color> ");

        if (ct_gold && ct_obj)
            doc_printf(doc, "%s or %s", obj_text, gold_text);
        else if (ct_obj)
            doc_printf(doc, "%s", obj_text);
        else if (ct_gold)
            doc_printf(doc, "%s", gold_text);

        doc_newline(doc);
    }
}
static void _display_kills(monster_race *r_ptr, doc_ptr doc)
{
    if (r_ptr->flags1 & RF1_UNIQUE)
    {
        doc_insert(doc, "Status  : ");
        if (r_ptr->max_num == 0)
            doc_insert(doc, "<color:D>Dead</color>");
        else if (mon_is_wanted(r_ptr->id))
            doc_insert(doc, "<color:v>Wanted</color>");
        else
            doc_insert(doc, "<color:y>Alive</color>");
        doc_newline(doc);
    }
    else
    {
        doc_printf(doc, "Kills   : <color:G>%d</color>\n", r_ptr->r_pkills);
    }

    if (_easy_lore(r_ptr) || r_ptr->r_tkills)
    {
        int xp = r_ptr->mexp * r_ptr->level / (p_ptr->max_plv + 2);
        char buf[10];

        big_num_display(xp, buf);
        doc_printf(doc, "Exp     : <color:G>%s</color> at CL%d\n", buf, p_ptr->max_plv);
    }

    _display_drops(r_ptr, doc);
    doc_newline(doc);
}

/**************************************************************************
 * Desc
 **************************************************************************/
static void _display_desc(monster_race *r_ptr, doc_ptr doc)
{
    doc_insert(doc, r_text + r_ptr->text);
    doc_newline(doc);
}

/**************************************************************************
 * Public
 **************************************************************************/
void mon_display(monster_race *r_ptr)
{
    mon_display_rect(r_ptr, ui_menu_rect());
}
void mon_display_rect(monster_race *r_ptr, rect_t display)
{
    doc_ptr doc = doc_alloc(MIN(display.cx, 72));

    if (display.cx > 80)
        display.cx = 80;

    mon_display_doc(r_ptr, doc);

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
void mon_display_doc(monster_race *r_ptr, doc_ptr doc)
{
    monster_race copy = *r_ptr;
    if (!_easy_lore(r_ptr))
    {
        /* Wipe flags to their 'known' values */
        copy.flags1 &= copy.r_flags1;
        copy.flags2 &= copy.r_flags2;
        copy.flags3 &= copy.r_flags3;
        copy.flags4 &= copy.r_flags4;
        copy.flags5 &= copy.r_flags5;
        copy.flags6 &= copy.r_flags6;
        copy.flagsr &= copy.r_flagsr;

        /* Assume some "obvious" flags */
        if (r_ptr->flags1 & RF1_UNIQUE)  copy.flags1 |= RF1_UNIQUE;
        if (r_ptr->flags1 & RF1_QUESTOR) copy.flags1 |= RF1_QUESTOR;
        if (r_ptr->flags1 & RF1_MALE)    copy.flags1 |= RF1_MALE;
        if (r_ptr->flags1 & RF1_FEMALE)  copy.flags1 |= RF1_FEMALE;

        /* Assume some "creation" flags */
        if (r_ptr->flags1 & RF1_FRIENDS) copy.flags1 |= RF1_FRIENDS;
        if (r_ptr->flags1 & RF1_ESCORT)  copy.flags1 |= RF1_ESCORT;
        if (r_ptr->flags1 & RF1_ESCORTS) copy.flags1 |= RF1_ESCORTS;

        /* Killing a monster reveals some properties */
        if (r_ptr->r_tkills)
        {
            /* Know "race" flags */
            if (r_ptr->flags3 & RF3_ORC)      copy.flags3 |= RF3_ORC;
            if (r_ptr->flags3 & RF3_TROLL)    copy.flags3 |= RF3_TROLL;
            if (r_ptr->flags3 & RF3_GIANT)    copy.flags3 |= RF3_GIANT;
            if (r_ptr->flags3 & RF3_DRAGON)   copy.flags3 |= RF3_DRAGON;
            if (r_ptr->flags3 & RF3_DEMON)    copy.flags3 |= RF3_DEMON;
            if (r_ptr->flags3 & RF3_UNDEAD)   copy.flags3 |= RF3_UNDEAD;
            if (r_ptr->flags3 & RF3_EVIL)     copy.flags3 |= RF3_EVIL;
            if (r_ptr->flags3 & RF3_GOOD)     copy.flags3 |= RF3_GOOD;
            if (r_ptr->flags3 & RF3_ANIMAL)   copy.flags3 |= RF3_ANIMAL;
            if (r_ptr->flags3 & RF3_AMBERITE) copy.flags3 |= RF3_AMBERITE;
            if (r_ptr->flags2 & RF2_HUMAN)    copy.flags2 |= RF2_HUMAN;
            if (r_ptr->flags2 & RF2_QUANTUM)  copy.flags2 |= RF2_QUANTUM;

            /* Know "forced" flags */
            if (r_ptr->flags1 & RF1_FORCE_DEPTH) copy.flags1 |= RF1_FORCE_DEPTH;
            if (r_ptr->flags1 & RF1_FORCE_MAXHP) copy.flags1 |= RF1_FORCE_MAXHP;
        }

    }

    _display_basic(&copy, doc);
    _display_resists(&copy, doc);
    _display_spells(&copy, doc);
    _display_attacks(&copy, doc);
    _display_other(&copy, doc);
    _display_kills(&copy, doc);

    _display_desc(&copy, doc);
}
