#include "angband.h"

#include "str-map.h"
#include <assert.h>

/***********************************************************************
 * Spell Skills, Stats and Savefile Support
 ***********************************************************************/

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
        memset(result, 0, sizeof(spell_stats_t));
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

        memset(stats, 0, sizeof(spell_stats_t));

        savefile_read_cptr(file, name, sizeof(name));
        stats->flags = savefile_read_u32b(file);
        stats->ct_cast = savefile_read_s32b(file);
        stats->ct_fail = savefile_read_s32b(file);
        stats->skill = savefile_read_s32b(file);
        stats->max_skill = savefile_read_s32b(file);

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
        savefile_write_u32b(file, stats->flags);
        savefile_write_s32b(file, stats->ct_cast);
        savefile_write_s32b(file, stats->ct_fail);
        savefile_write_s32b(file, stats->skill);
        savefile_write_s32b(file, stats->max_skill);
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

void default_spell(int cmd, variant *res) /* Base class */
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
    variant res;
    var_init(&res);
    spell(SPELL_CAST, &res);
    b = var_get_bool(&res);
    var_clear(&res);
    return b;
}

void fail_spell(ang_spell spell)
{
    variant res;
    var_init(&res);
    spell(SPELL_FAIL, &res);
    var_clear(&res);
}

int get_spell_energy(ang_spell spell)
{
    int n;
    variant res;
    var_init(&res);
    spell(SPELL_ENERGY, &res);
    n = var_get_int(&res);
    var_clear(&res);
    return n;
}

int get_spell_cost_extra(ang_spell spell)
{
    int n;
    variant res;
    var_init(&res);
    spell(SPELL_COST_EXTRA, &res);
    n = var_get_int(&res);
    var_clear(&res);
    return n;
}

int get_spell_fail_min(ang_spell spell)
{
    int n;
    variant res;
    var_init(&res);
    spell(SPELL_FAIL_MIN, &res);
    n = var_get_int(&res);
    var_clear(&res);
    return n;
}

cptr get_spell_name(ang_spell spell)
{
    static char buf[255];
    variant v;
    var_init(&v);
    spell(SPELL_NAME, &v);
    sprintf(buf, "%s", var_get_string(&v));
    var_clear(&v);
    return buf;
}

cptr get_spell_stat_name(ang_spell spell)
{
    static char buf[255];
    variant v;
    var_init(&v);
    spell(SPELL_STAT_NAME, &v);
    if (var_is_null(&v)) /* cf default_spell above */
        spell(SPELL_NAME, &v);
    sprintf(buf, "%s", var_get_string(&v));
    var_clear(&v);
    return buf;
}

cptr get_spell_desc(ang_spell spell)
{
    static char buf[1024];
    variant v;
    var_init(&v);
    spell(SPELL_DESC, &v);
    sprintf(buf, "%s", var_get_string(&v));
    var_clear(&v);
    return buf;
}

cptr get_spell_spoiler_name(ang_spell spell)
{
    static char buf[255];
    variant v;
    var_init(&v);
    spell(SPELL_SPOIL_NAME, &v);

    if (var_is_null(&v))
        spell(SPELL_NAME, &v);

    sprintf(buf, "%s", var_get_string(&v));
    var_clear(&v);
    return buf;
}

/****************************************************************************************
 * UI Utilities
 *   choose_spell - prompt user with a list of spells, they choose one.
 *   browse_spell - show spell list, user picks spells repeatedly. describe each spell.
 ****************************************************************************************/

static int _col_height(int ct)
{
    int    result = ct;
    rect_t display = ui_menu_rect();

    display.cy -= 5; /* Room for browsing */
    if (result > display.cy)
    {
        result = (ct + 1)/2;
    }

    return result;
}

static void _list_spells(spell_info* spells, int ct, int max_cost)
{
    char temp[140];
    int  i;
    rect_t display = ui_menu_rect();
    int  col_height = _col_height(ct);
    int  col_width;
    variant name, info, color;

    var_init(&name);
    var_init(&info);
    var_init(&color);

    Term_erase(display.x, display.y, display.cx);
    if (col_height == ct)
    {
        put_str("Lvl Cost Fail Desc", display.y, display.x + 29);
    }
    else
    {
        col_width = 42;
        put_str("Lvl Cost Fail", display.y, display.x + 29);
        put_str("Lvl Cost Fail", display.y, display.x + col_width + 29);
    }

    for (i = 0; i < ct; i++)
    {
        char letter = '\0';
        byte attr = TERM_WHITE;
        spell_info* spell = &spells[i];

        var_set_int(&color, TERM_WHITE);

        (spell->fn)(SPELL_NAME, &name);
        (spell->fn)(SPELL_INFO, &info);
        (spell->fn)(SPELL_COLOR, &color);

        attr = var_get_int(&color);

        if (i < 26)
            letter = I2A(i);
        else if (i < 52)
            letter = 'A' + i - 26;
        else
            letter = '0' + i - 52;

        sprintf(temp, "  %c) ", letter);

        strcat(temp, format("%-23.23s %3d %4d %3d%%",
                            var_get_string(&name),
                            spell->level,
                            spell->cost,
                            spell->fail));

        if (col_height == ct)
            strcat(temp, format(" %s", var_get_string(&info)));

        if (spell->fail == 100)
            attr = TERM_L_DARK;

        if (spell->cost > max_cost)
            attr = TERM_L_DARK;

        if (spell->level > p_ptr->lev)
            attr = TERM_L_DARK;

        if (i < col_height)
        {
            Term_erase(display.x, display.y + i + 1, display.cx);
            c_put_str(attr, temp, display.y + i + 1, display.x);
        }
        else
        {
            c_put_str(attr, temp, display.y + (i - col_height) + 1, display.x + col_width);
        }
    }
    Term_erase(display.x, display.y + col_height + 1, display.cx);
    var_clear(&name);
    var_clear(&info);
    var_clear(&color);
}

static bool _describe_spell(spell_info *spell, int col_height)
{
    bool result = TRUE;
    variant info;

    var_init(&info);

    (spell->fn)(SPELL_ON_BROWSE, &info);
    if (!var_get_bool(&info))
    {
        char tmp[62*5];
        int i, line;
        rect_t display = ui_menu_rect();

        /* 2 lines below list of spells, 5 lines for description */
        for (i = 0; i < 7; i++)
            Term_erase(display.x, display.y + col_height + i + 2, display.cx);

        /* Get the description, and line break it (max 5 lines) */
        (spell->fn)(SPELL_DESC, &info);
        roff_to_buf(var_get_string(&info), 62, tmp, sizeof(tmp));

        line = display.y + col_height + 3;
        for(i = 0; tmp[i]; i += 1+strlen(&tmp[i]))
        {
            put_str(&tmp[i], line, display.x + 2);
            line++;
        }

        (spell->fn)(SPELL_INFO, &info);
        put_str(format("%^s", var_get_string(&info)), line, display.x + 2);
        result = FALSE;
    }
    var_clear(&info);
    return result;
}

static int _choose_spell(spell_info* spells, int ct, cptr desc, int max_cost)
{
    int choice = -1;
    char prompt1[140];
    char prompt2[140];
    variant name;
    bool describe = FALSE;

    var_init(&name);

    for (;;)
    {
        char ch = '\0';

        strnfmt(prompt1, 78, "Use which %s? (Type '?' to Browse) ", desc);
        strnfmt(prompt2, 78, "Browse which %s? (Type '?' to Use)", desc);
        _list_spells(spells, ct, max_cost);

        /* Prompt User */
        choice = -1;

        if (!get_com(describe ? prompt2 : prompt1, &ch, FALSE)) break;

        if (ch == '?')
        {
            describe = !describe;
            if (!get_com(describe ? prompt2 : prompt1, &ch, FALSE)) break;
        }

        if (isupper(ch))
        {
            if (ct < 26) /* browse unless a big spell list */
            {
                choice = ch - 'A';
                if (0 <= choice && choice < ct)
                    _describe_spell(&spells[choice], _col_height(ct));
                continue;
            }
            choice = ch - 'A' + 26;
        }
        else if (islower(ch))
            choice = ch - 'a';
        else if (ch >= '0' && ch <= '9')
            choice = ch - '0' + 52;

        /* Valid Choice? */
        if (choice < 0 || choice >= ct)
        {
            bell();
            continue;
        }

        if (describe)
        {
            _describe_spell(&spells[choice], _col_height(ct));
            continue;
        }

        /* Good to go! */
        break;
    }

    var_clear(&name);
    return choice;
}

int choose_spell(spell_info* spells, int ct, cptr desc, int max_cost)
{
    int choice = -1;

    if (REPEAT_PULL(&choice))
    {
        if (choice >= 0 && choice < ct)
            return choice;
    }

    screen_save();

    choice = _choose_spell(spells, ct, desc, max_cost);
    REPEAT_PUSH(choice);

    screen_load();

    return choice;
}

void browse_spells(spell_info* spells, int ct, cptr desc)
{
    screen_save();

    for(;;)
    {
        int choice = -1;

        choice = _choose_spell(spells, ct, desc, 10000);
        if (choice < 0 || choice >= ct) break;

        if (_describe_spell(&spells[choice], _col_height(ct)))
            break;
    }
    screen_load();
}

int calculate_cost(int cost)
{
    int result = cost;

    if (p_ptr->dec_mana && cost > 0)
        result = MAX(1, result * 3 / 4);

    return result;
}

int calculate_fail_rate_aux(int caster_lvl, int spell_lvl, int base_fail, int stat_idx)
{
    int fail = base_fail;
    int min = 0;
    caster_info *caster_ptr = get_caster_info();

    if (caster_lvl < spell_lvl)
        return 100;

    if (caster_ptr && (caster_ptr->options & CASTER_NO_SPELL_FAIL))
        return 0;

    if (base_fail == 0)
        return 0;

    /* Adjust Fail Rate */
    fail -= 3 * (caster_lvl - spell_lvl);
    fail += p_ptr->to_m_chance;
    fail -= 3 * (adj_mag_stat[stat_idx] - 1);
    if (p_ptr->heavy_spell) fail += 20;

    {
        if (p_ptr->dec_mana && p_ptr->easy_spell) fail -= 4;
        else if (p_ptr->easy_spell) fail -= 3;
        else if (p_ptr->dec_mana) fail -= 2;
    }

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
    if (p_ptr->stun)
        fail += 50 * p_ptr->stun / 100;

    if (fail > 95) fail = 95;

    /* Some effects violate the Min/Max Fail Rates */
    if (p_ptr->heavy_spell) fail += 5; /* Fail could go to 100% */

    if (p_ptr->dec_mana) fail--; /* 5% casters could get 4% fail rates */

    if (fail < 0) fail = 0;
    if (fail > 100) fail = 100;
    return fail;
}

int calculate_fail_rate(int level, int base_fail, int stat_idx)
{
    return calculate_fail_rate_aux(p_ptr->lev, level, base_fail, stat_idx);
}

/****************************************************************
 * Entrypoints for the world
 ****************************************************************/

static void _add_extra_costs(spell_info* spells, int max)
{
    int i;
    /* Some spells give extra abilities depending on player level ...
       Ideally, these spells should scale the costs as well! */
    for (i = 0; i < max; i++)
    {
        spell_info* current = &spells[i];
        current->cost += get_spell_cost_extra(current->fn);
        current->fail = MAX(current->fail, get_spell_fail_min(current->fn));
        current->cost = calculate_cost(current->cost);
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
        /*Oops: Powers should not benefit from DEC_MANA or CASTER_NO_SPELL_COST!!!
        current->cost = calculate_cost(current->cost);*/
    }
}

static int _get_spell_table(spell_info* spells, int max)
{
    int ct = 0;
    class_t *class_ptr = get_class();
    race_t  *race_ptr = get_race();

    if (race_ptr->get_spells != NULL) /* Monster Races ... */
        ct = (race_ptr->get_spells)(spells, max);
    else if (class_ptr->get_spells != NULL)
        ct = (class_ptr->get_spells)(spells, max);

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
    bool hp_caster;
    int ct = 0;
    int choice = 0;
    int max_cost = 0;

    if (!caster)
    {
        msg_print("You cannot cast spells.");
        return;
    }

    hp_caster = ((caster->options & CASTER_USE_HP) || (p_ptr->pclass == CLASS_NINJA_LAWYER));

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

    if ((p_ptr->riding) && (player_is_ninja))
    {
        msg_print("You cannot use ninjutsu while riding!");
        return;
    }

    if (p_ptr->special_defense & KATA_MASK)
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
        max_cost = p_ptr->concent;
    else if (hp_caster)
        max_cost = p_ptr->chp;
    else if (caster->options & CASTER_USE_AU)
        max_cost = p_ptr->au;
    else
        max_cost = p_ptr->csp;
    choice = choose_spell(spells, ct, caster->magic_desc, max_cost);

    if (choice >= 0 && choice < ct)
    {
        spell_info *spell = &spells[choice];

        if (spell->level > p_ptr->lev)
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
            if (spell->cost > p_ptr->concent)
            {
                msg_print("You need to concentrate more to use this power.");
                return;
            }
        }
        else if (hp_caster)
        {
            if (spell->cost > p_ptr->chp)
            {
                msg_print("You do not have enough hp to use this power.");
                return;
            }
        }
        else if (caster->options & CASTER_USE_AU)
        {
            if (spell->cost > p_ptr->au)
            {
                msg_print("You do not have enough gold to use this power.");
                return;
            }
        }
        else
        {
            if (spell->cost > p_ptr->csp)
            {
                msg_print("You do not have enough mana to use this power.");
                return;
            }
            p_ptr->csp -= spell->cost;
        }

        /* Check for Failure */
        if (randint0(100) < spell->fail)
        {
            sound(SOUND_FAIL); /* Doh! */
            spell_stats_on_fail(spell);
            fail_spell(spell->fn);
            if (flush_failure) flush();
            msg_print("You failed to concentrate hard enough!");
            if (prompt_on_failure) msg_print(NULL);
            if (!(caster->options & CASTER_USE_AU) && !hp_caster && demigod_is_(DEMIGOD_ATHENA) )
                p_ptr->csp += spell->cost/2;
            if (caster->on_fail != NULL)
                (caster->on_fail)(spell);
        }
        else
        {
            if (!cast_spell(spell->fn))
            {
                /* Give back the spell cost, since the user canceled the spell
                 * There is no CASTER_USE_SP flag so we need to check all the alternatives */
                if ((!hp_caster) && (!(caster->options & (CASTER_USE_AU | CASTER_USE_CONCENTRATION)))) p_ptr->csp += spell->cost;
                return;
            }
            spell_stats_on_cast(spell);
            sound(SOUND_ZAP); /* Wahoo! */
        }

        energy_use = get_spell_energy(spell->fn);

        if ((hp_caster) && spell->cost > 0)
            take_hit(DAMAGE_USELIFE, spell->cost, "concentrating too hard");
        if ((caster->options & CASTER_USE_AU) && spell->cost > 0)
        {
            p_ptr->au -= spell->cost;
            stats_on_gold_services(spell->cost); /* ? */
            p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);
            p_ptr->redraw |= (PR_GOLD);
        }

        if (caster->on_cast != NULL)
            (caster->on_cast)(spell);

        p_ptr->redraw |= PR_MANA;
        p_ptr->redraw |= PR_HP;
        p_ptr->window |= PW_SPELL;
    }
}

void do_cmd_power(void)
{
    spell_info spells[MAX_SPELLS];
    int ct = 0;
    int choice = 0;
    race_t *race_ptr = get_race();
    class_t *class_ptr = get_class();

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

    /* Hack ... Rethink this a bit, but the alternative of hacking into
       the 'm' command is a million times worse!
       Doppelgangers need to be able to cancel their current mimicry.
       Also, add Mimic power back first so it always stays in the 'a' slot. */
    if (race_ptr->mimic && p_ptr->prace == RACE_DOPPELGANGER)
    {
        ct += (get_true_race()->get_powers)(spells + ct, MAX_SPELLS - ct);
    }

    if (race_ptr->get_powers != NULL)
    {
        ct += (race_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);
    }

    if (class_ptr != NULL && class_ptr->get_powers != NULL)
    {
        ct += (class_ptr->get_powers)(spells + ct, MAX_SPELLS - ct);
    }

    ct += mut_get_powers(spells + ct, MAX_SPELLS - ct);

    if (ct == 0)
    {
        msg_print("You have no powers.");
        return;
    }

    _add_extra_costs_powers(spells, ct);

    choice = choose_spell(spells, ct, "power", p_ptr->csp + p_ptr->chp);

    if (p_ptr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
    {
        set_action(ACTION_NONE);
    }

    if (choice >= 0 && choice < ct)
    {
        spell_info *spell = &spells[choice];

        if (spell->level > p_ptr->lev)
        {
            msg_print("You can't use that power yet!");
            return;
        }

        if (spell->cost > p_ptr->chp + p_ptr->csp)
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
            if (prompt_on_failure) msg_print(NULL);
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
        if (p_ptr->csp < spell->cost)
        {
            int cost = spell->cost - p_ptr->csp;
            p_ptr->csp = 0;
            take_hit(DAMAGE_USELIFE, cost, "concentrating too hard");
        }
        else
            p_ptr->csp -= spell->cost;

        p_ptr->redraw |= (PR_MANA);
        p_ptr->redraw |= (PR_HP);
        p_ptr->window |= (PW_SPELL);
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

        if (base->spell.level <= p_ptr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->spell.fn;
            current->level = base->spell.level;
            current->cost = base->spell.cost;

            current->fail = calculate_fail_rate(
                base->spell.level,
                base->spell.fail,
                p_ptr->stat_ind[base->stat]
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
    int idx = p_ptr->stat_ind[caster_ptr->which_stat];

    for (i = 0; ; i++)
    {
        spell_info *base = &table[i];
        if (ct >= max) break;
        if (!base->fn) break;

        if (base->level <= p_ptr->lev)
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

void dump_spells_aux(FILE *fff, spell_info *table, int ct)
{
    int i;
    variant vn, vd, vc, vfm;

    if (!ct) return;

    var_init(&vn);
    var_init(&vd);
    var_init(&vc);
    var_init(&vfm);

    if (character_dump_hack)
    {
        fprintf(fff, "=================================== Spells ====================================\n\n");
        fprintf(fff, "%-20.20s Lvl Cost Fail %-15.15s Cast Fail\n", "", "Desc");
    }
    else
    {
        fprintf(fff, "\n[[[[r|%-20.20s Lvl Cost Fail %-15.15s Cast Fail\n", "Spells", "Desc");
    }
    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];
        spell_stats_ptr stats = spell_stats(spell);

        spell->fn(SPELL_NAME, &vn);
        spell->fn(SPELL_INFO, &vd);
        spell->fn(SPELL_COST_EXTRA, &vc);
        spell->fn(SPELL_FAIL_MIN, &vfm);

        fprintf(fff, "%-20.20s %3d %4d %3d%% %-15.15s %4d %4d %3d%%\n",
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
}

static void _dump_book(doc_ptr doc, int realm, int book)
{
    int          k_idx = lookup_kind(realm2tval(realm), book);
    int          i, increment = 64;
    caster_info *caster_ptr = get_caster_info();

    if ((p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_RED_MAGE)) increment = 0;
    else if (realm == p_ptr->realm1) increment = 0;
    else if (realm == p_ptr->realm2) increment = 32;

    if (realm == REALM_HISSATSU)
    {
        doc_printf(doc, "<color:G>    %-25.25s Lvl  SP %-15.15s  Cast</color>\n", k_name + k_info[k_idx].name, "Desc");
    }
    else
    {
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            doc_printf(doc, "<color:G>    %-23.23s Profic Lvl  HP Fail %-15.15s  Cast Fail</color>\n", k_name + k_info[k_idx].name, "Desc");
        else
            doc_printf(doc, "<color:G>    %-23.23s Profic Lvl  SP Fail %-15.15s  Cast Fail</color>\n", k_name + k_info[k_idx].name, "Desc");
    }

    for (i = 0; i < 8; i++)
    {
        int         s_idx = book * 8 + i;
        magic_type *s_ptr;
        int	    vaikeustaso;
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

        vaikeustaso = lawyer_hack(s_ptr, LAWYER_HACK_LEVEL);
        if (vaikeustaso >= 99) continue;

        if (realm == REALM_HISSATSU)
            cost = s_ptr->smana;
        else
        {
            s16b exp = experience_of_spell(s_idx, realm);
            int  exp_level = spell_exp_level(exp);

            cost = mod_need_mana(lawyer_hack(s_ptr, LAWYER_HACK_MANA), s_idx, realm);

            max = FALSE;
            if (!increment && (exp_level == EXP_LEVEL_MASTER)) max = TRUE;
            else if ((increment == 32) && (exp_level >= EXP_LEVEL_EXPERT)) max = TRUE;
            else if ((p_ptr->pclass == CLASS_RED_MAGE) && (exp_level >= EXP_LEVEL_SKILLED)) max = TRUE;

            strncpy(proficiency, exp_level_str[exp_level], 4);
            proficiency[3] = ']';
            proficiency[4] = '\0';
        }

        strcpy(info, do_spell(realm, s_idx, SPELL_INFO));
        comment = info;

        if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
        {
            if (vaikeustaso > p_ptr->max_plv)
            {
                comment = "unknown";
                color = 'D';
            }
            else if (vaikeustaso > p_ptr->lev)
            {
                comment = "forgotten";
                color = 'y';
            }
        }
        else if ((realm == p_ptr->realm1) ?
            ((p_ptr->spell_forgotten1 & (1L << s_idx))) :
            ((p_ptr->spell_forgotten2 & (1L << s_idx))))
        {
            comment = "forgotten";
            color = 'y';
        }
        else if (!((realm == p_ptr->realm1) ?
            (p_ptr->spell_learned1 & (1L << s_idx)) :
            (p_ptr->spell_learned2 & (1L << s_idx))))
        {
            comment = "unknown";
            if (vaikeustaso > p_ptr->lev)
                color = 'D';
            else
                color = 'B';
        }
        else if (!((realm == p_ptr->realm1) ?
            (p_ptr->spell_worked1 & (1L << s_idx)) :
            (p_ptr->spell_worked2 & (1L << s_idx))))
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
                    vaikeustaso,
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
                    vaikeustaso,
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
        int vaikeustaso;

        if (is_magic(realm))
            s_ptr = &mp_ptr->info[realm - 1][s_idx];
        else
            s_ptr = &technic_info[realm - MIN_TECHNIC][s_idx];

        vaikeustaso = lawyer_hack(s_ptr, LAWYER_HACK_LEVEL);
        if (vaikeustaso >= 99) continue;

        if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
        {
            if (vaikeustaso > p_ptr->max_plv) continue;
            else if (vaikeustaso > p_ptr->lev) return TRUE;
        }
        else if ((realm == p_ptr->realm1) ?
            ((p_ptr->spell_forgotten1 & (1L << s_idx))) :
            ((p_ptr->spell_forgotten2 & (1L << s_idx))))
        {
            return TRUE;
        }
        else if (realm == p_ptr->realm1 ?
                    (p_ptr->spell_learned1 & (1L << s_idx)) :
                    (p_ptr->spell_learned2 & (1L << s_idx)))
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
        if (p_ptr->pclass == CLASS_RED_MAGE && realm != REALM_ARCANE && i > 1) break;

        /* Necromancy probably should be a technique ... Too late now :( */
        if (p_ptr->pclass == CLASS_RED_MAGE && realm == REALM_NECROMANCY) break;

        /* Red Mages and Sorcerers don't learn spells, so make sure the user actually
           has the book in their pack (otherwise we spoil ... ) */
        if (p_ptr->pclass == CLASS_RED_MAGE || p_ptr->pclass == CLASS_SORCERER)
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
    if (p_ptr->pclass == CLASS_RAGE_MAGE)
        return; /* TODO */

    doc_printf(doc, "<topic:Spells>==================================== <color:keypress>S</color>pells ===================================\n\n");

    if (p_ptr->pclass == CLASS_RED_MAGE || p_ptr->pclass == CLASS_SORCERER)
    {
        int realm;
        for (realm = REALM_LIFE; realm <= MAX_MAGIC; realm++)
            _dump_realm(doc, realm);
    }
    else
    {
        if (p_ptr->realm1)
            _dump_realm(doc, p_ptr->realm1);
        if (p_ptr->realm2)
            _dump_realm(doc, p_ptr->realm2);
    }

    if (p_ptr->old_realm)
    {
        int i;
        for (i = 0; i < MAX_MAGIC; i++)
        {
            if (!(p_ptr->old_realm & 1L << i)) continue;
            doc_printf(doc, " You were able to use %s magic before.\n", realm_names[i+1]);
        }
        doc_newline(doc);
    }

    if (p_ptr->spells_per_round > 100)
    {
        doc_printf(doc, " You may cast %d.%02d spells per round.\n\n", p_ptr->spells_per_round/100, p_ptr->spells_per_round%100);
    }
}

void spellbook_destroy(obj_ptr obj)
{
    bool gain_expr = FALSE;
    if (!high_level_book(obj)) return;

    if (p_ptr->prace == RACE_ANDROID)
    {
    }
    else if (p_ptr->pclass == CLASS_WARRIOR || p_ptr->pclass == CLASS_BERSERKER)
    {
        gain_expr = TRUE;
    }
    else if (p_ptr->pclass == CLASS_PALADIN)
    {
        if (is_good_realm(p_ptr->realm1))
        {
            if (!is_good_realm(tval2realm(obj->tval))) gain_expr = TRUE;
        }
        else
        {
            if (is_good_realm(tval2realm(obj->tval))) gain_expr = TRUE;
        }
    }

    if (gain_expr && (p_ptr->exp < PY_MAX_EXP))
    {
        s32b tester_exp = p_ptr->max_exp / 20;
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

