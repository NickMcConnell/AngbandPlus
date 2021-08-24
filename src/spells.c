#include "angband.h"

#include "str_map.h"
#include <assert.h>

/***********************************************************************
 * Spell Skills, Stats and Savefile Support
 ***********************************************************************/
/* stats for a single spell */
void spell_stats_load(spell_stats_ptr stats, savefile_ptr file)
{
    stats->flags = savefile_read_u32b(file);
    stats->ct_cast = savefile_read_s32b(file);
    stats->ct_fail = savefile_read_s32b(file);
    stats->skill = savefile_read_s32b(file);
    stats->max_skill = savefile_read_s32b(file);
    if (!stats->max_skill)
        stats->max_skill = SPELL_EXP_MASTER; /* XXX */
    stats->alias = savefile_read_byte(file);
}
void spell_stats_save(spell_stats_ptr stats, savefile_ptr file)
{
    savefile_write_u32b(file, stats->flags);
    savefile_write_s32b(file, stats->ct_cast);
    savefile_write_s32b(file, stats->ct_fail);
    savefile_write_s32b(file, stats->skill);
    savefile_write_s32b(file, stats->max_skill);
    savefile_write_byte(file, stats->alias);
}
void spell_stats_wipe(spell_stats_ptr stats)
{
    memset(stats, 0, sizeof(spell_stats_t));
    stats->max_skill = SPELL_EXP_MASTER; /* XXX */
}

/* global map of spell stats by spell name */
static str_map_ptr _spell_stats_map(void)
{
    static str_map_ptr _map = NULL;
    if (!_map)
        _map = str_map_alloc(free);
    return _map;
}

spell_stats_ptr spell_stats_aux(cptr name)
{
    str_map_ptr     map = _spell_stats_map();
    spell_stats_ptr result = str_map_find(map, name);

    if (!result)
    {
        result = malloc(sizeof(spell_stats_t));
        spell_stats_wipe(result);
        str_map_add(map, name, result);
    }
    return result;
}

spell_stats_ptr spell_stats(spell_info *spell)
{
    cptr name = get_spell_stat_name(spell->fn);
    return spell_stats_aux(name);
}

void spell_stats_on_birth(void)
{
    str_map_ptr map = _spell_stats_map();
    str_map_clear(map);
}

void spell_stats_on_load(savefile_ptr file)
{
    str_map_ptr map = _spell_stats_map();
    int         ct, i;

    str_map_clear(map);
    ct = savefile_read_s32b(file);
    for (i = 0; i < ct; i++)
    {
        char            name[255];
        spell_stats_ptr stats = malloc(sizeof(spell_stats_t));

        spell_stats_wipe(stats);

        savefile_read_cptr(file, name, sizeof(name));
        spell_stats_load(stats, file);

        str_map_add(map, name, stats);
    }
}

void spell_stats_on_save(savefile_ptr file)
{
    str_map_ptr      map = _spell_stats_map();
    str_map_iter_ptr iter;

    savefile_write_s32b(file, str_map_count(map));

    for (iter = str_map_iter_alloc(map);
            str_map_iter_is_valid(iter);
            str_map_iter_next(iter))
    {
        spell_stats_ptr stats = str_map_iter_current(iter);

        savefile_write_cptr(file, str_map_iter_current_key(iter));
        spell_stats_save(stats, file);
    }
    str_map_iter_free(iter);
}

void spell_stats_on_learn(spell_info *spell, int max_skill)
{
    spell_stats_ptr stats = spell_stats(spell);

    stats->flags |= SPELL_FLAG_LEARNED;
    stats->max_skill = max_skill;
}

void spell_stats_on_cast(spell_info *spell)
{
    spell_stats_ptr stats = spell_stats(spell);

    stats->ct_cast++;
}

/* XXX duplicated from do_cmd_cast for book-based spells ... refactor and share XXX */
void spell_stats_gain_skill(spell_info *spell)
{
    spell_stats_gain_skill_aux(
        get_spell_stat_name(spell->fn),
        spell->level
    );
}
void spell_stats_gain_skill_aux(cptr name, int lvl)
{
    spell_stats_ptr stats = spell_stats_aux(name);
    int  current = stats->skill;
    int  dlvl = cave->difficulty;
    s16b gain = 0;

    {  /* You'll need to spreadsheet this to see if this is any good ...
        * Try a cross tab spell level vs dun level. Here is a rough summary
        * of minimum dlvl to reach desired proficiency (but remember that
        * interpolation smooths things out. So you can reach 1530 prof with
        * a L50 spell on DL90, for instance):
        * SLvl  Be  Sk  Ex  Ma
        *   50  24  57  73 100
        *   40  19  47  61  84
        *   30  14  37  49  68
        *   20   9  27  36  51
        *   10   4  17  24  35
        *    5   1  12  18  27
        *    1   1   8  13  20 */
        int ratio = (17 + lvl) * 100 / (10 + dlvl);
        point_t max_tbl[4] = { {60, 1600}, {100, 1200}, {200, 900}, {300, 0} };
        int max = interpolate(ratio, max_tbl, 4);

        if (current < max)
        {
            point_t gain_tbl[9] = { /* 0->900->1200->1400->1600 */
                {0, 128}, {200, 64}, {400, 32}, {600, 16},
                {800, 8}, {1000, 4}, {1200, 2}, {1400, 1}, {1600, 1} };
            gain = interpolate(current, gain_tbl, 9);
        }
        else if (0 || plr->wizard)
        {
            msg_format("<color:B>When casting an <color:R>L%d</color> spell on "
                "<color:R>DL%d</color> your max proficiency is <color:R>%d</color> "
                "(Current: <color:R>%d</color>).</color> <color:D>%d</color>",
                lvl, dlvl, max, current, ratio);
        }
    }

    if (gain)
    {
        int  old_level = spell_exp_level(current);
        int  new_level = old_level;

        stats->skill += gain;
        if (stats->skill > stats->max_skill)
            stats->skill = stats->max_skill;
        new_level = spell_exp_level(stats->skill);
        if (new_level > old_level)
        {
            cptr desc[5] = { "Unskilled", "a Beginner", "Skilled", "an Expert", "a Master" };
            msg_format("You are now <color:B>%s</color> in <color:R>%s</color>.",
                desc[new_level],
                name);
        }
        else if (plr->wizard)
        {
            msg_format("You now have <color:B>%d</color> proficiency in <color:R>%s</color>.",
                stats->skill,
                name);
        }
        else if (stats->skill/100 > current/100)
        {
            msg_format("<color:B>You are getting more proficient with <color:R>%s</color>.</color>",
                name);
        }
    }
}

void spell_stats_on_fail(spell_info *spell)
{
    spell_stats_ptr stats = spell_stats(spell);
    stats->ct_fail++;
}

/* Legacy Spell System */
spell_stats_ptr spell_stats_old(int realm, int spell)
{
    cptr name = do_spell(realm, spell, SPELL_NAME);
    return spell_stats_aux(name);
}


void spell_stats_on_cast_old(int realm, int spell)
{
    spell_stats_ptr stats = spell_stats_old(realm, spell);
    stats->ct_cast++;
}

void spell_stats_on_fail_old(int realm, int spell)
{
    spell_stats_ptr stats = spell_stats_old(realm, spell);
    stats->ct_fail++;
}

/***********************************************************************
 * New Spell System ... Spells are objects (implemented as functions)
 * and can now be stored other data types (spell books, scrolls, etc).
 *
 * 'Spell' is misleading. This will be used by spells, racial powers,
 * mutations, potions, scrolls, etc.
 *
 * I'm attempting a grand unification of all player effects to allow
 * for some more code flexibility (e.g. scrolls that hold *any* spell and
 * Spells copied from scrolls into "books" for spellcasters, etc.)
 ***********************************************************************/

void default_spell(int cmd, var_ptr res) /* Base class */
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Unknown Spell");
        break;

    case SPELL_DESC:
    case SPELL_INFO:
    case SPELL_MUT_DESC:
        var_set_string(res, "");
        break;

    case SPELL_GAIN_MUT:
    case SPELL_LOSE_MUT:
        var_clear(res);
        break;

    case SPELL_FAIL:
    case SPELL_STOP:
    case SPELL_CONT:
        var_set_bool(res, TRUE);
        break;

    case SPELL_CAST:
        msg_print("Zap?");
        var_set_bool(res, TRUE);
        break;

    case SPELL_COST_EXTRA:
    case SPELL_FAIL_MIN:
        var_set_int(res, 0);
        break;

    case SPELL_ENERGY:
        var_set_int(res, 100);
        break;

    case SPELL_CALC_BONUS:
        var_set_bool(res, TRUE);
        break;

    case SPELL_COLOR:
        var_set_int(res, TERM_WHITE);
        break;

    case SPELL_ON_BROWSE:
        var_set_bool(res, FALSE);
        break;

    case SPELL_STAT_NAME: /* must return NULL so clients can requery with SPELL_NAME */
    default:
        var_clear(res);
        break;
    }
}

bool cast_spell(ang_spell spell)
{
    bool b;
    var_t res = var_create();
    spell(SPELL_CAST, &res);
    b = var_get_bool(&res);
    var_destroy(&res);
    return b;
}

void fail_spell(ang_spell spell)
{
    var_t res = var_create();
    spell(SPELL_FAIL, &res);
    var_destroy(&res);
}

int get_spell_energy(ang_spell spell)
{
    int n;
    var_t res = var_create();
    spell(SPELL_ENERGY, &res);
    n = var_get_int(&res);
    var_destroy(&res);
    return n;
}

int get_spell_cost_extra(ang_spell spell)
{
    int n;
    var_t res = var_create();
    spell(SPELL_COST_EXTRA, &res);
    n = var_get_int(&res);
    var_destroy(&res);
    return n;
}

int get_spell_fail_min(ang_spell spell)
{
    int n;
    var_t res = var_create();
    spell(SPELL_FAIL_MIN, &res);
    n = var_get_int(&res);
    var_destroy(&res);
    return n;
}

cptr get_spell_name(ang_spell spell)
{
    static char buf[255];
    var_t v = var_create();
    spell(SPELL_NAME, &v);
    sprintf(buf, "%s", var_get_string(&v));
    var_destroy(&v);
    return buf;
}

cptr get_spell_stat_name(ang_spell spell)
{
    static char buf[255];
    var_t v = var_create();
    spell(SPELL_STAT_NAME, &v);
    if (var_is_null(&v)) /* cf default_spell above */
        spell(SPELL_NAME, &v);
    sprintf(buf, "%s", var_get_string(&v));
    var_destroy(&v);
    return buf;
}

cptr get_spell_desc(ang_spell spell)
{
    static char buf[1024];
    var_t v = var_create();
    spell(SPELL_DESC, &v);
    sprintf(buf, "%s", var_get_string(&v));
    var_destroy(&v);
    return buf;
}

cptr get_spell_spoiler_name(ang_spell spell)
{
    static char buf[255];
    var_t v = var_create();
    spell(SPELL_SPOIL_NAME, &v);

    if (var_is_null(&v))
        spell(SPELL_NAME, &v);

    sprintf(buf, "%s", var_get_string(&v));
    var_destroy(&v);
    return buf;
}

dice_t innate_dice(int dd, int ds, int base) { return dice_create(dd, ds, base); }
dice_t spell_dice(int dd, int ds, int base)
{
    dice_t dice;
    dice.dd = dd;
    dice.ds = ds;
    dice.base = base;
    dice.scale = spell_power(1000);
    return dice;
}
dice_t spell_dam_dice(int dd, int ds, int base)
{
    dice_t dice;
    dice.dd = dd;
    dice.ds = ds;
    dice.base = base + plr->to_d_spell;
    dice.scale = spell_power(1000);
    return dice;
}


/****************************************************************************************
 * UI Utilities
 *   choose_spell - prompt user with a list of spells, they choose one.
 *   browse_spell - show spell list, user picks spells repeatedly. describe each spell.
 ****************************************************************************************/
static doc_ptr _doc = NULL;
#define _BROWSE_MODE 0x01
#define _INNATE 0x02

static int _inkey(void)
{
    return inkey_special(TRUE);
}
static void _sync_term(doc_ptr doc)
{
    Term_load();
    doc_sync_menu(doc);
}
static bool _hidden_spells;
static bool _alias_spells;

void spell_skill_doc(spell_stats_ptr stats, doc_ptr doc)
{
    static cptr _skill_str[5]= {"[Un]", "[Be]", "[Sk]", "[Ex]", "[Ma]"};
    char qualifier = stats->skill == stats->max_skill ? '!' : ' ';
    int  level = spell_exp_level(stats->skill);
    doc_printf(doc, "%c%s ", qualifier, _skill_str[level]);
}
static void _list_spells(spell_info* spells, int ct, int max_cost, int browse_idx, bool hide, u32b mode)
{
    int   i;
    var_t name = var_create();
    var_t info = var_create();
    var_t color = var_create();
    bool skill = FALSE;

    if (!(mode & _INNATE)) /* _list_spells also lists "powers", innate abilities that don't require proficiency */
    {
        caster_info *caster = get_caster_info();
        if (caster->options & CASTER_GAIN_SKILL)
            skill = TRUE;
    }

    doc_printf(_doc, "%-27.27s <color:G>", "");
    if (skill) /*         ![Ma] */
        doc_insert(_doc, "Skill ");
    doc_printf(_doc, "Lvl  SP Fail %-20.20s</color>\n", "Desc");

    _hidden_spells = FALSE;
    _alias_spells = FALSE;

    for (i = 0; i < ct; i++)
    {
        spell_info *spell = &spells[i];
        byte        attr;

        if (spell->stats->flags & SPELL_FLAG_HIDE)
        {
            _hidden_spells = TRUE;
            if (hide) continue;
        }

        if (spell->stats->alias)
            _alias_spells = TRUE;

        var_set_int(&color, TERM_WHITE);

        spell->fn(SPELL_NAME, &name);
        spell->fn(SPELL_INFO, &info);
        spell->fn(SPELL_COLOR, &color);

        attr = TERM_YELLOW;
        if (spell->level > plr->lev || spell->cost > max_cost)
            attr = TERM_L_DARK;
        else if (spell->stats->flags & SPELL_FLAG_CONFIRM)
            attr = TERM_RED;
        doc_printf(_doc, " <color:%c>%c</color>) ", attr_to_attr_char(attr), spell->alias);

        attr = var_get_int(&color);
        if (i == browse_idx)
            attr = TERM_L_BLUE;
        else if (spell->stats->flags & SPELL_FLAG_HIDE)
            attr = TERM_L_DARK;
        doc_printf(_doc, "<color:%c>%-23.23s</color> ",
            attr_to_attr_char(attr), var_get_string(&name));

        if (skill)
        {
            spell_stats_ptr stats = spell_stats(spell);
            spell_skill_doc(stats, _doc);
        }
        doc_printf(_doc, "%3d <color:%c>%3d</color> %3d%% ",
            spell->level,
            spell->cost <= max_cost ? 'w' : 'r',
            spell->cost, spell->fail);

        if (spell->level <= plr->lev)
            doc_printf(_doc, "%-20.20s", var_get_string(&info));
        else
            doc_printf(_doc, "%-20.20s", "");

        doc_newline(_doc);
    }
    var_destroy(&name);
    var_destroy(&info);
    var_destroy(&color);
}

static bool _confirm(spell_info *spell, int max_cost)
{
    char cmd;
    char prompt[255];

    if (spell->level > plr->lev) return FALSE;
    if (spell->cost > max_cost) return FALSE;
    if (!(spell->stats->flags & SPELL_FLAG_CONFIRM)) return TRUE;

    sprintf(prompt, "<color:y>Really cast <color:R>%s</color>? [Y,n]</color>",
            get_spell_name(spell->fn));

    cmd = msg_prompt(prompt, "ny", PROMPT_YES_NO);
    msg_line_clear();
    if (cmd == 'n') return FALSE;

    return TRUE;
}

static int _lookup(spell_info* spells, int ct, char alias, bool hide)
{
    int i;
    for (i = 0; i < ct; i++)
    {
        if (hide && (spells[i].stats->flags & SPELL_FLAG_HIDE)) continue;
        if (spells[i].alias == alias) return i;
    }
    return -1;
}

static int _lookup_ctrl(spell_info* spells, int ct, char alias, bool hide)
{
    int i;
    for (i = 0; i < ct; i++)
    {
        if (hide && (spells[i].stats->flags & SPELL_FLAG_HIDE)) continue;
        if (KTRL(spells[i].alias) == alias) return i;
    }
    return -1;
}

static void _reset_labels(spell_info* spells, int ct, bool hide)
{
    int i;
    for (i = 0; i < ct; i++)
    {
        if (hide && (spells[i].stats->flags & SPELL_FLAG_HIDE)) continue;
        spells[i].alias = I2A(i);
    }
}

static void _override_labels(spell_info* spells, int ct, bool hide)
{
    int i;
    for (i = 0; i < ct; i++)
    {
        char alias;
        if (hide && (spells[i].stats->flags & SPELL_FLAG_HIDE)) continue;
        alias = spells[i].stats->alias;
        /* override default menu key */
        if (alias)
        {
            /* mask any other spell using this alias ... */
            int j = _lookup(spells, ct, alias, hide);
            if (0 <= j && j < ct)
                spells[j].alias = ' ';
            /* ... before setting the override */
            spells[i].alias = alias;
        }
    }
}

static void _customize(spell_info* spells, int ct, int which)
{
    spell_info *spell = &spells[which];
    bool redraw = TRUE;
    int cmd;
    for (;;)
    {
        if (redraw)
        {
            doc_clear(_doc);
            doc_printf(_doc, "<color:y>Configure <color:R>%s</color></color>\n\n",
                get_spell_name(spell->fn));
            if (spell->stats->alias)
            {
                doc_printf(_doc,
                    "   Alias: <color:keypress>%c</color> (Press <color:keypress>DEL</color>"
                    " to remove, <color:keypress>a-z</color> to change)\n",
                    spell->stats->alias);
            }
            else
            {
                doc_insert(_doc,
                    "   No Alias (Press <color:keypress>a-z</color> to use a custom menu alias)\n");
            }
            doc_printf(_doc,
                "   %s this spell (Press <color:keypress>^H</color> to toggle)\n",
                (spell->stats->flags & SPELL_FLAG_HIDE) ? "<color:D>Hide</color>" : "Show");

            doc_printf(_doc,
                "   %s casting spell (Press <color:keypress>^C</color> to toggle)\n",
                (spell->stats->flags & SPELL_FLAG_CONFIRM) ? "<color:r>Confirm</color>" : "Do not confirm");
                
            doc_insert(_doc, "\n<color:y>RET)</color> Return to Main Menu\n");
            _sync_term(_doc);
            redraw = FALSE;
        }
        cmd = _inkey();
        if (cmd == ESCAPE || cmd == '\r') break;
        else if (cmd == 0x7f && spell->stats->alias)
        {
            spell->stats->alias = '\0';
            redraw = TRUE;
        }
        else if (islower(cmd))
        {
            spell->stats->alias = cmd;
            redraw = TRUE;
        }
        else if (cmd == KTRL('C'))
        {
            if (spell->stats->flags & SPELL_FLAG_CONFIRM)
                spell->stats->flags &= ~SPELL_FLAG_CONFIRM;
            else
                spell->stats->flags |= SPELL_FLAG_CONFIRM;
            redraw = TRUE;
        }
        else if (cmd == KTRL('H'))
        {
            if (spell->stats->flags & SPELL_FLAG_HIDE)
                spell->stats->flags &= ~SPELL_FLAG_HIDE;
            else
                spell->stats->flags |= SPELL_FLAG_HIDE;
            redraw = TRUE;
        }
    }
}
static void _prep_spell_table(spell_info* spells, int max)
{
    int i;
    for (i = 0; i < max; i++)
    {
        spell_info* current = &spells[i];
        current->stats = spell_stats(current);
    }
    _reset_labels(spells, max, TRUE);
    _override_labels(spells, max, TRUE);
}

static int _choose_spell(spell_info* spells, int ct, cptr desc, int max_cost, u32b mode)
{
    int result = -1;
    int browse_idx = -1;
    int cmd, i;
    bool redraw = TRUE;
    bool labels = TRUE;  /* apply spell_stats_s.alias */
    bool hide = TRUE;    /* apply SPELL_FLAG_HIDE */
    bool relabel = FALSE;
    static bool help = TRUE;
    var_t info = var_create();

    _prep_spell_table(spells, ct);

    if (REPEAT_PULL(&cmd))
    {
        i = _lookup(spells, ct, cmd, hide);
        if (0 <= i && i < ct)
        {
            spell_info *spell = &spells[i];
            if (spell->level <= plr->lev && spell->cost <= max_cost)
                return i;
        }
    }

    assert(!_doc);
    _doc = doc_alloc(72);

    msg_line_clear();
    screen_save();
    for (;;)
    {
        if (relabel)
        {
            _reset_labels(spells, ct, hide);
            if (labels)
                _override_labels(spells, ct, hide);
        }
        if (redraw)
        {
            doc_clear(_doc);
            doc_printf(_doc, "<color:y>%s which <color:keyword>%s</color>?</color>\n",
                (mode & _BROWSE_MODE) ? "Browse" : "Cast", desc);
            _list_spells(spells, ct, max_cost, browse_idx, hide, mode);
            if (0 <= browse_idx && browse_idx < ct)
            {
                spell_info *spell = &spells[browse_idx];
                spell->fn(SPELL_ON_BROWSE, &info);
                if (!var_get_bool(&info))
                {
                    spell->fn(SPELL_DESC, &info);
                    doc_newline(_doc);
                    doc_insert(_doc, var_get_string(&info));
                    doc_newline(_doc);
                }
            }
            if (help)
            {
                doc_newline(_doc);
                doc_insert(_doc, "Press <color:keypress>SHIFT+choice</color> to display description.\n");
                doc_insert(_doc, "Press <color:keypress>CTL+choice</color> to configure options.\n");
                if (_hidden_spells)
                    doc_printf(_doc, "Press <color:keypress>!</color> to %s hidden spells.\n", hide ? "show" : "not show");
                if (_alias_spells)
                    doc_printf(_doc, "Press <color:keypress>@</color> to %s custom menu keys.\n", labels ? "ignore" : "apply");
            }
            _sync_term(_doc);
            redraw = FALSE;
        }
        cmd = _inkey();
        if (cmd == ESCAPE) break;
        /* XXX note that KTRL('M') == \r, and so forth ... */
        else if (cmd == '@')
        {
            labels = !labels;
            relabel = TRUE;
            redraw = TRUE;
        }
        else if (cmd == '!')
        {
            hide = !hide;
            relabel = TRUE;
            redraw = TRUE;
        }
        else if (cmd == '?')
        {
            help = !help;
            redraw = TRUE;
        }
        else if (isupper(cmd))
        {
            i = _lookup(spells, ct, tolower(cmd), hide);
            if (i < 0 || i >= ct) continue;
            browse_idx = i;
            redraw = TRUE;
        }
        else if (islower(cmd))
        {
            i = _lookup(spells, ct, cmd, hide);
            if (i < 0 || i >= ct) continue;
            if (mode & _BROWSE_MODE)
            {
                browse_idx = i;
                redraw = TRUE;
            }
            else
            {
                spell_info *spell = &spells[i];
                if (!_confirm(spell, max_cost))
                {
                    if (spell->stats->flags & SPELL_FLAG_CONFIRM)
                        redraw = TRUE;
                    continue;
                }
                result = i;
                if (labels && hide && !(spell->stats->flags & SPELL_FLAG_CONFIRM))
                    REPEAT_PUSH(cmd);
                break;
            }
        }
        else if (KTRL(cmd) == cmd)
        {
            i = _lookup_ctrl(spells, ct, cmd, hide);
            if (i < 0 || i >= ct) continue;
            _customize(spells, ct, i);
            relabel = TRUE;
            redraw = TRUE;
        }
    }
    screen_load();

    var_destroy(&info);
    doc_free(_doc);
    _doc = NULL;

    return result;
}

int choose_spell(spell_info* spells, int ct, cptr desc, int max_cost)
{
    return _choose_spell(spells, ct, desc, max_cost, 0);
}

int choose_power(spell_info* spells, int ct, cptr desc, int max_cost)
{
    return _choose_spell(spells, ct, desc, max_cost, _INNATE);
}

void browse_spells(spell_info* spells, int ct, cptr desc)
{
    _choose_spell(spells, ct, desc, 10000, _BROWSE_MODE);
}

void browse_powers(spell_info* spells, int ct, cptr desc)
{
    _choose_spell(spells, ct, desc, 10000, _BROWSE_MODE | _INNATE);
}

/****************************************************************************************
 * Cost and Fail Rates
 ****************************************************************************************/
int spell_cost(spell_info *spell)
{
    int cost = spell->cost;
    cost += get_spell_cost_extra(spell->fn);
    return spell_cost_aux(REALM_NONE, get_spell_stat_name(spell->fn), cost);
}
int spell_cost_aux(int realm, cptr name, int cost)
{
    caster_info *caster = get_caster_info();
    if (caster->options & CASTER_GAIN_SKILL)
    {
        spell_stats_ptr stats = spell_stats_aux(name);
        int mul = 2400 + SPELL_EXP_EXPERT - stats->skill;
        int div = 2400;
        cost = (cost * mul + div - 1) / div;
    }

    if (cost > 0)
    {
        int dec_mana = plr->dec_mana;
        if (realm != REALM_NONE && plr->easy_realm1 == realm)
            dec_mana++;
        if (dec_mana)
            cost = MAX(1, (cost + 1) * dec_mana_cost(dec_mana) / 100);
    }
    return cost;
}

int calculate_fail_rate_aux(int caster_lvl, int realm, int spell_lvl, int base_fail, int stat_idx)
{
    int fail = base_fail;
    int min = 0;
    caster_info *caster_ptr = get_caster_info();
    int dec_mana = plr->dec_mana;

    if (caster_lvl < spell_lvl)
        return 100;

    if (caster_ptr && (caster_ptr->options & CASTER_NO_SPELL_FAIL))
        return 0;

    if (base_fail == 0)
        return 0;

    if (realm != REALM_NONE && plr->easy_realm1 == realm)
        dec_mana++;

    /* Adjust Fail Rate */
    fail -= 3 * (caster_lvl - spell_lvl);
    fail += plr->to_m_chance;
    fail -= 3 * (adj_mag_stat[stat_idx] - 1);
    if (plr->heavy_spell) fail += 20;

    fail += dec_mana_fail1(dec_mana, plr->easy_spell);

    /* Apply Min Fail Rate */
    min = adj_mag_fail[stat_idx];

    if (caster_ptr && min < caster_ptr->min_fail)
        min = caster_ptr->min_fail;

    if (mut_present(MUT_ARCANE_MASTERY))
        fail -= 3;

    if (demigod_is_(DEMIGOD_ATHENA))
    {
        fail -= 2;
        if (min > 0)
            min -= 1;
    }

    if (fail < min) fail = min;
    if (plr_tim_find(T_STUN))
        fail += 50 * plr_tim_amount(T_STUN) / 100;

    if (fail > 95) fail = 95;

    /* Some effects violate the Min/Max Fail Rates */
    if (plr->heavy_spell) fail += 5; /* Fail could go to 100% */

    fail += dec_mana_fail2(dec_mana, plr->easy_spell);

    if (fail < 0) fail = 0;
    if (fail > 100) fail = 100;
    return fail;
}

int calculate_fail_rate(int level, int base_fail, int stat_idx)
{
    return calculate_fail_rate_aux(plr->lev, REALM_NONE, level, base_fail, stat_idx);
}

/****************************************************************
 * Entrypoints for the world
 ****************************************************************/
static void _add_extra_costs(spell_info* spells, int max)
{
    int i;
    for (i = 0; i < max; i++)
    {
        spell_info* current = &spells[i];
        current->cost = spell_cost(current);
        current->fail = MAX(current->fail, get_spell_fail_min(current->fn));
    }
}

static void _add_extra_costs_powers(spell_info* spells, int max)
{
    int i;
    /* Some spells give extra abilities depending on player level ...
       Ideally, these spells should scale the costs as well! */
    for (i = 0; i < max; i++)
    {
        spell_info* current = &spells[i];
        current->cost += get_spell_cost_extra(current->fn);
        current->fail = MAX(current->fail, get_spell_fail_min(current->fn));
    }
}
static int _get_spell_table(spell_info* spells, int max)
{
    int ct = 0;
    class_t *class_ptr = get_class();
    race_t  *race_ptr = get_race();

    if (race_ptr->hooks.get_spells != NULL) /* Monster Races ... */
        ct = (race_ptr->hooks.get_spells)(spells, max);
    else if (class_ptr->hooks.get_spells != NULL)
        ct = (class_ptr->hooks.get_spells)(spells, max);

    _add_extra_costs(spells, ct);
    return ct;
}

void do_cmd_spell_browse(void)
{
    spell_info spells[MAX_SPELLS];
    caster_info *caster = get_caster_info();
    int ct = _get_spell_table(spells, MAX_SPELLS);

    if (ct == 0)
    {
        /* User probably canceled the prompt for a spellbook */
        return;
    }
    browse_spells(spells, ct, caster->magic_desc);
}

void do_cmd_spell(void)
{
    spell_info spells[MAX_SPELLS];
    caster_info *caster = get_caster_info();
    int ct = 0;
    int choice = 0;
    int max_cost = 0;

    if (!caster)
    {
        msg_print("You cannot cast spells.");
        return;
    }

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    if (plr->special_defense & KATA_MASK)
    {
        set_action(ACTION_NONE);
    }

    ct = _get_spell_table(spells, MAX_SPELLS);
    if (ct == 0)
    {
        /* User probably canceled the prompt for a spellbook */
        return;
    }

    if (caster->options & CASTER_USE_CONCENTRATION)
        max_cost = plr->concent;
    else if (caster->options & CASTER_USE_HP)
        max_cost = plr->chp;
    else if (caster->options & CASTER_USE_AU)
        max_cost = plr->au;
    else
        max_cost = plr->csp;
    choice = choose_spell(spells, ct, caster->magic_desc, max_cost);

    if (choice >= 0 && choice < ct)
    {
        spell_info *spell = &spells[choice];

        if (spell->level > plr->lev)
        {
            msg_print("You can't use that spell yet!");
            return;
        }

        /* Verify Cost ... Note, I'm removing options for over exertion
           Also note we now pay casting costs up front for mana casters.
           If the user cancels, then we return the cost below.
        */
        if (caster->options & CASTER_USE_CONCENTRATION)
        {
            if (spell->cost > plr->concent)
            {
                msg_print("You need to concentrate more to use this power.");
                return;
            }
        }
        else if (caster->options & CASTER_USE_HP)
        {
            if (spell->cost > plr->chp)
            {
                msg_print("You do not have enough hp to use this power.");
                return;
            }
        }
        else if (caster->options & CASTER_USE_AU)
        {
            if (spell->cost > plr->au)
            {
                msg_print("You do not have enough gold to use this power.");
                return;
            }
        }
        else
        {
            if (spell->cost > plr->csp)
            {
                msg_print("You do not have enough mana to use this power.");
                return;
            }
            plr->csp -= spell->cost;
        }

        /* Check for Failure */
        if (randint0(100) < spell->fail)
        {
            sound(SOUND_FAIL); /* Doh! */
            spell_stats_on_fail(spell);
            fail_spell(spell->fn);
            if (flush_failure) flush();
            msg_print("You failed to concentrate hard enough!");
            if (!(caster->options & (CASTER_USE_HP | CASTER_USE_AU)) && demigod_is_(DEMIGOD_ATHENA) )
                plr->csp += spell->cost/2;
            if (caster->on_fail != NULL)
                (caster->on_fail)(spell);
        }
        else
        {
            if (!cast_spell(spell->fn))
            {
                /* Give back the spell cost, since the user canceled the spell
                 * There is no CASTER_USE_SP flag so we need to check all the alternatives */
                if (!(caster->options & (CASTER_USE_HP | CASTER_USE_AU | CASTER_USE_CONCENTRATION))) plr->csp += spell->cost;
                return;
            }
            spell_stats_on_cast(spell);
            sound(SOUND_ZAP); /* Wahoo! */
            if (caster->options & CASTER_GAIN_SKILL)
                spell_stats_gain_skill(spell);
        }

        energy_use = get_spell_energy(spell->fn);

        if ((caster->options & CASTER_USE_HP) && spell->cost > 0)
            take_hit(DAMAGE_USELIFE, spell->cost, "concentrating too hard");
        if ((caster->options & CASTER_USE_AU) && spell->cost > 0)
        {
            plr->au -= spell->cost;
            stats_on_gold_services(spell->cost); /* ? */
            plr->update |= (PU_BONUS | PU_HP | PU_MANA);
            plr->redraw |= (PR_GOLD);
        }

        if (caster->on_cast != NULL)
            (caster->on_cast)(spell);

        plr->redraw |= PR_MANA;
        plr->redraw |= PR_HP;
        plr->window |= PW_SPELL;
    }
}

void do_cmd_power(void)
{
    spell_info spells[MAX_SPELLS];
    int ct = 0;
    int choice = 0;
    race_t *race_ptr = get_race();
    class_t *class_ptr = get_class();

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    /* Hack ... Rethink this a bit, but the alternative of hacking into
       the 'm' command is a million times worse!
       Doppelgangers need to be able to cancel their current mimicry.
       Also, add Mimic power back first so it always stays in the 'a' slot. */
    if (race_ptr->mimic && plr->prace == RACE_DOPPELGANGER)
    {
        ct += (get_true_race()->hooks.get_powers)(spells + ct, MAX_SPELLS - ct);
    }

    if (race_ptr->hooks.get_powers != NULL)
    {
        ct += (race_ptr->hooks.get_powers)(spells + ct, MAX_SPELLS - ct);
    }

    if (class_ptr != NULL && class_ptr->hooks.get_powers != NULL)
    {
        ct += (class_ptr->hooks.get_powers)(spells + ct, MAX_SPELLS - ct);
    }

    ct += mut_get_powers(spells + ct, MAX_SPELLS - ct);

    if (ct == 0)
    {
        msg_print("You have no powers.");
        return;
    }

    _add_extra_costs_powers(spells, ct);

    choice = choose_power(spells, ct, "power", plr->csp + plr->chp);

    if (plr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    if (choice >= 0 && choice < ct)
    {
        spell_info *spell = &spells[choice];

        if (spell->level > plr->lev)
        {
            msg_print("You can't use that power yet!");
            return;
        }

        if (spell->cost > plr->chp + plr->csp)
        {
            msg_print("Using this power will kill you!  Why not rest a bit first?");
            return;
        }

        /* Check for Failure */
        if (randint0(100) < spell->fail)
        {
            spell_stats_on_fail(spell);
            sound(SOUND_FAIL); /* Doh! */
            fail_spell(spell->fn);
            if (flush_failure) flush();
            msg_print("You failed to concentrate hard enough!");
        }
        else
        {
            if (!cast_spell(spell->fn))
                return;
            spell_stats_on_cast(spell);
            sound(SOUND_ZAP); /* Wahoo! */
        }

        energy_use = get_spell_energy(spell->fn);

        /* Casting costs spill over into hit points */
        if (plr->csp < spell->cost)
        {
            int cost = spell->cost - plr->csp;
            plr->csp = 0;
            take_hit(DAMAGE_USELIFE, cost, "concentrating too hard");
        }
        else
            plr->csp -= spell->cost;

        plr->redraw |= (PR_MANA);
        plr->redraw |= (PR_HP);
        plr->window |= (PW_SPELL);
    }
}

/***********************************************************************
 * Utilities
 ***********************************************************************/
int get_powers_aux(spell_info* spells, int max, power_info* table)
{
    int i;
    int ct = 0;

    for (i = 0; ; i++)
    {
        power_info *base = &table[i];

        if (ct >= max) break;
        if (!base->spell.fn) break;

        if (base->spell.level <= plr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->spell.fn;
            current->level = base->spell.level;
            current->cost = base->spell.cost;

            current->fail = calculate_fail_rate(
                base->spell.level,
                base->spell.fail,
                plr->stat_ind[base->stat]
            );
            ct++;
        }
    }
    return ct;
}

int get_spells_aux(spell_info* spells, int max, spell_info* table)
{
    int i;
    int ct = 0;
    caster_info *caster_ptr = get_caster_info();
    int idx = plr->stat_ind[caster_ptr->which_stat];

    for (i = 0; ; i++)
    {
        spell_info *base = &table[i];
        if (ct >= max) break;
        if (!base->fn) break;

        if (base->level <= plr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;

            current->fail = calculate_fail_rate(base->level, base->fail, idx);
            ct++;
        }
    }
    return ct;
}

int spell_stats_fail(spell_stats_ptr stats)
{
    int result = 0;
    int attempts = stats->ct_cast + stats->ct_fail;
    if (attempts)
    {
        int pct = stats->ct_fail * 1000 / attempts;
        result = pct / 10;
        if (pct % 10 >= 5)
            result++;
    }
    return result;
}

static void _dump_book(doc_ptr doc, int realm, int book)
{
    int          k_idx = lookup_kind(realm2tval(realm), book);
    int          i, increment = 64;
    caster_info *caster_ptr = get_caster_info();

    if ((plr->pclass == CLASS_SORCERER) || (plr->pclass == CLASS_RED_MAGE)) increment = 0;
    else if (realm == plr->realm1) increment = 0;
    else if (realm == plr->realm2) increment = 32;

    if (realm == REALM_HISSATSU)
    {
        doc_printf(doc, "<color:G>    %-25.25s Lvl  SP %-15.15s  Cast</color>\n", k_info[k_idx].name, "Desc");
    }
    else
    {
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            doc_printf(doc, "<color:G> %-26.26s Profic Lvl  HP Fail %-15.15s  Cast Fail</color>\n", k_info[k_idx].name, "Desc");
        else
            doc_printf(doc, "<color:G> %-26.26s Profic Lvl  SP Fail %-15.15s  Cast Fail</color>\n", k_info[k_idx].name, "Desc");
    }

    for (i = 0; i < 8; i++)
    {
        int         s_idx = book * 8 + i;
        magic_type *s_ptr;
        int         cost;
        bool        max = FALSE;
        char        proficiency[10];
        char        info[80];
        cptr        comment;
        char        line[160];
        char        color = 'w';

        if (is_magic(realm))
            s_ptr = &mp_ptr->info[realm - 1][s_idx];
        else
            s_ptr = &technic_info[realm - MIN_TECHNIC][s_idx];

        if (s_ptr->slevel >= 99) continue;

        if (realm == REALM_HISSATSU)
            cost = s_ptr->smana;
        else
        {
            s16b exp = experience_of_spell(s_idx, realm);
            int  exp_level = spell_exp_level(exp);

            cost = mod_need_mana(s_ptr->smana, s_idx, realm);

            max = FALSE;
            if (!increment && (exp_level == EXP_LEVEL_MASTER)) max = TRUE;
            else if ((increment == 32) && (exp_level >= EXP_LEVEL_EXPERT)) max = TRUE;
            else if ((plr->pclass == CLASS_RED_MAGE) && (exp_level >= EXP_LEVEL_SKILLED)) max = TRUE;

            strncpy(proficiency, exp_level_str[exp_level], 4);
            proficiency[3] = ']';
            proficiency[4] = '\0';
        }

        strcpy(info, do_spell(realm, s_idx, SPELL_INFO));
        comment = info;

        if (plr->pclass == CLASS_SORCERER || plr->pclass == CLASS_RED_MAGE)
        {
            if (s_ptr->slevel > plr->max_plv)
            {
                comment = "unknown";
                color = 'D';
            }
            else if (s_ptr->slevel > plr->lev)
            {
                comment = "forgotten";
                color = 'y';
            }
        }
        else if ((realm == plr->realm1) ?
            ((plr->spell_forgotten1 & (1L << s_idx))) :
            ((plr->spell_forgotten2 & (1L << s_idx))))
        {
            comment = "forgotten";
            color = 'y';
        }
        else if (!((realm == plr->realm1) ?
            (plr->spell_learned1 & (1L << s_idx)) :
            (plr->spell_learned2 & (1L << s_idx))))
        {
            comment = "unknown";
            if (s_ptr->slevel > plr->lev)
                color = 'D';
            else
                color = 'B';
        }
        else if (!((realm == plr->realm1) ?
            (plr->spell_worked1 & (1L << s_idx)) :
            (plr->spell_worked2 & (1L << s_idx))))
        {
            comment = "untried";
        }

        sprintf(line, " %c) ", I2A(i));
        if (realm == REALM_HISSATSU)
        {
            spell_stats_ptr stats = spell_stats_old(realm, s_idx);
            strcat(
                line,
                format(
                    "<color:%c>%-25s %3d %3d %-15.15s %5d</color>",
                    color,
                    do_spell(realm, s_idx, SPELL_NAME),
                    s_ptr->slevel,
                    cost,
                    comment,
                    stats->ct_cast
                )
            );
        }
        else
        {
            spell_stats_ptr stats = spell_stats_old(realm, s_idx);
            strcat(
                line,
                format(
                    "<color:%c>%-25s%c%-4s %3d %3d %3d%% %-15.15s %5d %4d %3d%%</color>",
                    color,
                    do_spell(realm, s_idx, SPELL_NAME),
                    (max ? '!' : ' '),
                    proficiency,
                    s_ptr->slevel,
                    cost,
                    spell_chance(s_idx, realm),
                    comment,
                    stats->ct_cast,
                    stats->ct_fail,
                    spell_stats_fail(stats)
                )
            );
        }

        doc_printf(doc, "%s\n", line);
    }
    doc_newline(doc);
}

static bool _has_spells(int realm, int book)
{
    int i;
    for (i = 0; i < 8; i++)
    {
        int            s_idx = book * 8 + i;
        magic_type *s_ptr;

        if (is_magic(realm))
            s_ptr = &mp_ptr->info[realm - 1][s_idx];
        else
            s_ptr = &technic_info[realm - MIN_TECHNIC][s_idx];

        if (s_ptr->slevel >= 99) continue;

        if (plr->pclass == CLASS_SORCERER || plr->pclass == CLASS_RED_MAGE)
        {
            if (s_ptr->slevel > plr->max_plv) continue;
            else if (s_ptr->slevel > plr->lev) return TRUE;
        }
        else if ((realm == plr->realm1) ?
            ((plr->spell_forgotten1 & (1L << s_idx))) :
            ((plr->spell_forgotten2 & (1L << s_idx))))
        {
            return TRUE;
        }
        else if (realm == plr->realm1 ?
                    (plr->spell_learned1 & (1L << s_idx)) :
                    (plr->spell_learned2 & (1L << s_idx)))
        {
            return TRUE;
        }
    }
    return FALSE;
}

static bool _has_book(int realm, int book)
{
    int tval = realm2tval(realm);
    int sval = book;

    return pack_find_obj(tval, sval);
}

static void _dump_realm(doc_ptr doc, int realm)
{
    int i;
    bool first = TRUE;
    for (i = 0; i < 4; i++)
    {
        bool dump = FALSE;

        /* Red Mages only learn from first 2 books (except for Arcane) */
        if (plr->pclass == CLASS_RED_MAGE && realm != REALM_ARCANE && i > 1) break;

        /* Necromancy probably should be a technique ... Too late now :( */
        if (plr->pclass == CLASS_RED_MAGE && realm == REALM_NECROMANCY) break;

        /* Red Mages and Sorcerers don't learn spells, so make sure the user actually
           has the book in their pack (otherwise we spoil ... ) */
        if (plr->pclass == CLASS_RED_MAGE || plr->pclass == CLASS_SORCERER)
            dump = _has_book(realm, i);
        else
            dump = _has_spells(realm, i) || _has_book(realm, i);

        if (dump)
        {
            if (first)
            {
                doc_printf(doc, "<color:r>Realm:</color> <color:B>%s</color>\n\n", realm_names[realm]);
                first = FALSE;
            }
            _dump_book(doc, realm, i);
        }
    }
    i = virtue_mod_spell_fail(realm, 0);
    if (!first && i)
        doc_printf(doc, " Your alignment is adding <color:R>%+d%%</color> to your fail rates in this realm.\n\n", i);
}

void spellbook_character_dump(doc_ptr doc)
{
    if (plr->pclass == CLASS_RAGE_MAGE)
        return; /* TODO */

    doc_printf(doc, "<topic:Spells>==================================== <color:keypress>S</color>pells ===================================\n\n");

    if (plr->pclass == CLASS_RED_MAGE || plr->pclass == CLASS_SORCERER)
    {
        int realm;
        for (realm = REALM_LIFE; realm <= MAX_MAGIC; realm++)
            _dump_realm(doc, realm);
    }
    else
    {
        if (plr->realm1)
            _dump_realm(doc, plr->realm1);
        if (plr->realm2)
            _dump_realm(doc, plr->realm2);
    }

    if (plr->old_realm)
    {
        int i;
        for (i = 0; i < MAX_MAGIC; i++)
        {
            if (!(plr->old_realm & 1L << i)) continue;
            doc_printf(doc, " You were able to use %s magic before.\n", realm_names[i+1]);
        }
        doc_newline(doc);
    }

    if (plr->spells_per_round > 100)
    {
        doc_printf(doc, " You may cast %d.%02d spells per round.\n\n", plr->spells_per_round/100, plr->spells_per_round%100);
    }
}

void spellbook_destroy(obj_ptr obj)
{
    bool gain_expr = FALSE;
    if (!high_level_book(obj)) return;

    if (plr->prace == RACE_ANDROID)
    {
    }
    else if (plr->pclass == CLASS_WARRIOR)
    {
        gain_expr = TRUE;
    }
    else if (plr->pclass == CLASS_PALADIN)
    {
        if (is_good_realm(plr->realm1))
        {
            if (!is_good_realm(tval2realm(obj->tval))) gain_expr = TRUE;
        }
        else
        {
            if (is_good_realm(tval2realm(obj->tval))) gain_expr = TRUE;
        }
    }

    if (gain_expr && (plr->exp < PY_MAX_EXP))
    {
        s32b tester_exp = plr->max_exp / 20;
        if (tester_exp > 10000) tester_exp = 10000;
        if (obj->sval < 3) tester_exp /= 4;
        if (tester_exp<1) tester_exp = 1;

        msg_print("You feel more experienced.");
        gain_exp(tester_exp * obj->number);
    }

    if (obj->tval == TV_LIFE_BOOK)
    {
        virtue_add(VIRTUE_UNLIFE, 1);
        virtue_add(VIRTUE_VITALITY, -1);
    }
    else if ( high_level_book(obj)
           && (obj->tval == TV_DEATH_BOOK || obj->tval == TV_NECROMANCY_BOOK) )
    {
        virtue_add(VIRTUE_UNLIFE, -1);
        virtue_add(VIRTUE_VITALITY, 1);
    }
}

