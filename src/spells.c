#include "angband.h"

#include "str-map.h"

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

static spell_stats_ptr _spell_stats(spell_info *spell)
{
    cptr name = get_spell_name(spell->fn);
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

        savefile_read_string(file, name, sizeof(name));
        stats->flags = savefile_read_u32b(file);
        stats->ct_cast = savefile_read_s32b(file);
        stats->ct_fail = savefile_read_s32b(file);
        stats->skill = savefile_read_s32b(file);
        stats->max_skill = savefile_read_s32b(file);
        stats->last_turn = savefile_read_s32b(file);

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

        savefile_write_string(file, str_map_iter_current_key(iter));
        savefile_write_u32b(file, stats->flags);
        savefile_write_s32b(file, stats->ct_cast);
        savefile_write_s32b(file, stats->ct_fail);
        savefile_write_s32b(file, stats->skill);
        savefile_write_s32b(file, stats->max_skill);
        savefile_write_s32b(file, stats->last_turn);
    }
    str_map_iter_free(iter);
}

void spell_stats_on_learn(spell_info *spell, int max_skill)
{
    spell_stats_ptr stats = _spell_stats(spell);

    stats->flags |= SPELL_FLAG_LEARNED;
    stats->max_skill = max_skill;
}

void spell_stats_on_cast(spell_info *spell)
{
    spell_stats_ptr stats = _spell_stats(spell);

    stats->ct_cast++;
}

void spell_stats_gain_skill(spell_info *spell)
{
    static int      last_pexp = 0;
    spell_stats_ptr stats = _spell_stats(spell);

    /* Hack: Try to eliminate spell spamming for experience.
       The experience check is for blasting monsters with consecutive Mana Bursts
       which should be granting spell experience, provided one is damaging monsters.
               
       The turn check is for utility spells (Teleport & Detect tactics) since one
       might be doing a bunch of legitimate casting without fighting monsters.

       Note: Androids will probably always fail to pass the xp check! Hmm ...
       Note: One still might be able to macro up a cast followed by a rest command.
    */
    if ( last_pexp != p_ptr->exp 
      || (!p_ptr->inside_quest && turn > stats->last_turn + 50 + randint1(50)) )
    {
        int skill = 0;
        int dlvl = MAX(base_level, dun_level);

        if (stats->skill < SPELL_EXP_BEGINNER)
            skill += 60;
        else if (stats->skill < SPELL_EXP_SKILLED)
        {
            if (dlvl > 4 && dlvl + 10 > p_ptr->lev)
                skill = 8;
        }
        else if (stats->skill < SPELL_EXP_EXPERT)
        {
            if (dlvl + 5 > p_ptr->lev && dlvl + 5 > spell->level)
                skill = 2;
        }
        else if (stats->skill < SPELL_EXP_MASTER)
        {
            if (dlvl + 5 > p_ptr->lev && dlvl > spell->level)
                skill = 1;
        }
        stats->skill += skill;
        if (stats->skill > stats->max_skill)
            stats->skill = stats->max_skill;
    }
    last_pexp = p_ptr->exp;
    stats->last_turn = turn;
}

void spell_stats_on_fail(spell_info *spell)
{
    spell_stats_ptr stats = _spell_stats(spell);
    stats->ct_fail++;
}

/* Legacy Spell System */
static spell_stats_ptr _spell_stats_old(int realm, int spell)
{
    cptr name = do_spell(realm, spell, SPELL_NAME);
    return spell_stats_aux(name);
}


void spell_stats_on_cast_old(int realm, int spell)
{
    spell_stats_ptr stats = _spell_stats_old(realm, spell);
    stats->ct_cast++;
}

void spell_stats_on_fail_old(int realm, int spell)
{
    spell_stats_ptr stats = _spell_stats_old(realm, spell);
    stats->ct_fail++;
}


/***********************************************************************
 * New Spell System ... Spells are objects (implemented as functions)
 * and can now be stored other data types (spell books, scrolls, etc).
 *
 * 'Spell' is misleading.  This will be used by spells, racial powers,
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
        var_set_string(res, "Unkown Spell");
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
 *   browse_spell - show spell list, user picks spells repeatedly.  describe each spell.
 ****************************************************************************************/

static int _col_height(int ct)
{
    int  w, h;
    int result = ct;

    Term_get_size(&w, &h);

    h -= 5; /* Room for browsing */
    if (result > h)
    {
        result = (ct + 1)/2;
    }

    return result;
}

static void _list_spells(spell_info* spells, int ct, int max_cost)
{
    char temp[140];
    int  i;
    int  y = 1;
    int  x = 13;
    int  col_height = _col_height(ct);
    int  col_width;
    variant name, info, color;

    var_init(&name);
    var_init(&info);
    var_init(&color);

    Term_erase(x, y, 255);

    if (col_height == ct)
    {
        Term_erase(x, y, 255);
        put_str("Lvl Cost Fail Desc", y, x + 29);
    }
    else
    {
        col_width = 42;
        x = 1;
        Term_erase(x, y, 255);
        put_str("Lvl Cost Fail", y, x + 29);
        put_str("Lvl Cost Fail", y, x + col_width + 29);
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
            c_prt(attr, temp, y + i + 1, x);
        }
        else
        {
            c_prt(attr, temp, y + (i - col_height) + 1, (x + col_width));
        }
    }
    Term_erase(x, y + col_height + 1, 255);
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

        /* 2 lines below list of spells, 5 lines for description */
        for (i = 0; i < 7; i++)
            Term_erase(13, col_height + i + 2, 255);

        /* Get the description, and line break it (max 5 lines) */
        (spell->fn)(SPELL_DESC, &info);
        roff_to_buf(var_get_string(&info), 62, tmp, sizeof(tmp));

        for(i = 0, line = col_height + 3; tmp[i]; i += 1+strlen(&tmp[i]))
        {
            prt(&tmp[i], line, 15);
            line++;
        }

        (spell->fn)(SPELL_INFO, &info);
        prt(format("%^s", var_get_string(&info)), line, 15);
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

    strnfmt(prompt1, 78, "Use which %s? (Type '?' to Browse) ", desc);
    strnfmt(prompt2, 78, "Browse which %s? (Type '?' to Use)", desc);
    _list_spells(spells, ct, max_cost);

    for (;;)
    {
        char ch = '\0';

        /* Prompt User */
        choice = -1;

        if (!get_com(describe ? prompt2 : prompt1, &ch, FALSE)) break;

        if (ch == '?')
        {
            describe = !describe;
            if (!get_com(describe ? prompt2 : prompt1, &ch, FALSE)) break;
        }

        if (isupper(ch))
            choice = ch - 'A' + 26;
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

static bool _allow_dec_mana(caster_info *caster_ptr)
{
    if ( (caster_ptr && (caster_ptr->options & CASTER_ALLOW_DEC_MANA))
      || prace_is_(RACE_MON_LICH) ) /* TODO: I was never expecting race to influence casting ... */
    {
        return TRUE;
    }
    return FALSE;
}

int calculate_cost(int cost)
{
    int result = cost;
    caster_info *caster_ptr = get_caster_info();

    if (caster_ptr && (caster_ptr->options & CASTER_NO_SPELL_COST))
        return 0;

    if (_allow_dec_mana(caster_ptr) && cost > 0)
    {
        if (p_ptr->dec_mana)
            result = MAX(1, result * 3 / 4);
    }
    return result;
}

int calculate_fail_rate(int level, int base_fail, int stat_idx)
{
    int fail = base_fail;
    int min = 0;
    caster_info *caster_ptr = get_caster_info();

    if (p_ptr->lev < level)
        return 100;

    if (caster_ptr && (caster_ptr->options & CASTER_NO_SPELL_FAIL))
        return 0;

    if (base_fail == 0)
        return 0;

    /* Adjust Fail Rate */
    fail -= 3 * (p_ptr->lev - level);
    fail += p_ptr->to_m_chance;
    fail -= 3 * (adj_mag_stat[stat_idx] - 1);
    if (p_ptr->heavy_spell) fail += 20;

    if (_allow_dec_mana(caster_ptr))
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

    if (prace_is_(RACE_DEMIGOD) && p_ptr->psubrace == DEMIGOD_ATHENA)
    {
        fail -= 2;
        if (min > 0)
            min -= 1;
    }

    if (fail < min) fail = min;

    /* Stunning affects even 0% fail spells */
    if (p_ptr->stun > 50) fail += 25;
    else if (p_ptr->stun) fail += 15;

    /* Max Fail Rate */
    if (fail > 95) fail = 95;

    /* Some effects violate the Min/Max Fail Rates */
    if (p_ptr->heavy_spell) fail += 5; /* Fail could go to 100% */

    if (_allow_dec_mana(caster_ptr))
    {
        if (p_ptr->dec_mana) fail--; /* 5% casters could get 4% fail rates */
    }

    if (fail < 0) fail = 0;
    if (fail > 100) fail = 100;
    return fail;
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
    class_t *class_ptr = get_class_t();
    race_t  *race_ptr = get_race_t();

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
    int ct = 0; 
    int choice = 0;
    int max_cost = 0;

    if (!caster) 
    {
        msg_print("You cannot cast spells.");
        return;
    }
    
    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
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

    if (caster->options & CASTER_USE_HP)
        max_cost = p_ptr->chp;
    else if (caster->options & CASTER_NO_SPELL_COST)
        max_cost = 10000;
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
        if (caster->options & CASTER_USE_HP)
        {
            if (spell->cost > p_ptr->chp)
            {
                msg_print("You do not have enough hp to use this power.");
                return;
            }
        }
        else if (!(caster->options & CASTER_NO_SPELL_COST))
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
            if (!(caster->options & CASTER_USE_HP) && demigod_is_(DEMIGOD_ATHENA) )
                p_ptr->csp += spell->cost/2;
            if (caster->on_fail != NULL)
                (caster->on_fail)(spell);
        }
        else
        {
            if (!cast_spell(spell->fn))
            {
                /* Give back the spell cost, since the user canceled the spell */
                if (!(caster->options & CASTER_USE_HP)) p_ptr->csp += spell->cost;
                return;
            }
            spell_stats_on_cast(spell);
            sound(SOUND_ZAP); /* Wahoo! */
        }

        energy_use = get_spell_energy(spell->fn);

        if ((caster->options & CASTER_USE_HP) && spell->cost > 0)
            take_hit(DAMAGE_USELIFE, spell->cost, "concentrating too hard", -1);

        if (caster->on_cast != NULL)
            (caster->on_cast)(spell);

        p_ptr->redraw |= (PR_MANA);
        p_ptr->redraw |= (PR_HP);
        p_ptr->window |= (PW_PLAYER);
        p_ptr->window |= (PW_SPELL);
    }
}

void do_cmd_power(void)
{
    spell_info spells[MAX_SPELLS];
    int ct = 0; 
    int choice = 0;
    race_t *race_ptr = get_race_t();
    class_t *class_ptr = get_class_t();
    
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
        ct += (get_true_race_t()->get_powers)(spells + ct, MAX_SPELLS - ct);
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
            take_hit(DAMAGE_USELIFE, cost, "concentrating too hard", -1);
        }
        else 
            p_ptr->csp -= spell->cost;

        p_ptr->redraw |= (PR_MANA);
        p_ptr->redraw |= (PR_HP);
        p_ptr->window |= (PW_PLAYER);
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

    /*    if (base->spell.level <= p_ptr->lev) */
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

    fprintf(fff, "=================================== Spells ====================================\n\n");
    fprintf(fff, "%-20.20s Lvl Cost Fail %-15.15s Cast Fail\n", "", "Desc");
    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];      
        spell_stats_ptr stats = _spell_stats(spell);

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

void dump_powers_aux(FILE *fff, spell_info *table, int ct)
{        
    int i;
    variant vn, vd, vc, vfm;
    if (!ct) return;

    var_init(&vn);
    var_init(&vd);
    var_init(&vc);
    var_init(&vfm);

    fprintf(fff, "=================================== Powers ====================================\n\n");
    fprintf(fff, "%-20.20s Lvl Cost Fail %-15.15s Cast Fail\n", "", "Desc");
    for (i = 0; i < ct; i++)
    {
        spell_info     *spell = &table[i];        
        spell_stats_ptr stats = _spell_stats(spell);

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

static void _dump_book(FILE *fff, int realm, int book)
{
    int          k_idx = lookup_kind(realm2tval(realm), book);
    int          i, increment = 64;
    caster_info *caster_ptr = get_caster_info();

    if ((p_ptr->pclass == CLASS_SORCERER) || (p_ptr->pclass == CLASS_RED_MAGE)) increment = 0;
    else if (realm == p_ptr->realm1) increment = 0;
    else if (realm == p_ptr->realm2) increment = 32;

    if (realm == REALM_HISSATSU)
        fprintf(fff, "    %-25.25s Lvl  SP %-15.15s  Cast\n", k_name + k_info[k_idx].name, "Desc");
    else
    {
        if (caster_ptr && (caster_ptr->options & CASTER_USE_HP))
            fprintf(fff, "    %-23.23s Profic Lvl  HP Fail %-15.15s  Cast Fail\n", k_name + k_info[k_idx].name, "Desc");
        else
            fprintf(fff, "    %-23.23s Profic Lvl  SP Fail %-15.15s  Cast Fail\n", k_name + k_info[k_idx].name, "Desc");
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
            else if ((p_ptr->pclass == CLASS_RED_MAGE) && (exp_level >= EXP_LEVEL_SKILLED)) max = TRUE;

            strncpy(proficiency, exp_level_str[exp_level], 4);
            proficiency[3] = ']';
            proficiency[4] = '\0';
        }

        strcpy(info, do_spell(realm, s_idx, SPELL_INFO));
        comment = info;
        
        if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
        {
            if (s_ptr->slevel > p_ptr->max_plv)
                comment = "unknown";
            else if (s_ptr->slevel > p_ptr->lev)
                comment = "forgotten";
        }
        else if ((realm == p_ptr->realm1) ?
            ((p_ptr->spell_forgotten1 & (1L << s_idx))) :
            ((p_ptr->spell_forgotten2 & (1L << s_idx))))
        {
            comment = "forgotten";
        }
        else if (!((realm == p_ptr->realm1) ?
            (p_ptr->spell_learned1 & (1L << s_idx)) :
            (p_ptr->spell_learned2 & (1L << s_idx))))
        {
            comment = "unknown";
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
            spell_stats_ptr stats = _spell_stats_old(realm, s_idx);
            strcat(
                line, 
                format(
                    "%-25s %3d %3d %-15.15s %5d",
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
            spell_stats_ptr stats = _spell_stats_old(realm, s_idx);
            strcat(
                line, 
                format(
                    "%-25s%c%-4s %3d %3d %3d%% %-15.15s %5d %4d %3d%%",
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

        fprintf(fff, "%s\n", line);
    }
    fprintf(fff, "\n");
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

        if (p_ptr->pclass == CLASS_SORCERER || p_ptr->pclass == CLASS_RED_MAGE)
        {
            if (s_ptr->slevel > p_ptr->max_plv) continue;
            else if (s_ptr->slevel > p_ptr->lev) return TRUE;
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
    int i;

    for (i = 0; i < INVEN_PACK; i++)
    {
        if (inventory[i].tval == tval && inventory[i].sval == sval)
            return TRUE;
    }
    return FALSE;
}

static void _dump_realm(FILE *fff, int realm)
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
                fprintf(fff, "Realm: %s\n\n", realm_names[realm]);
                first = FALSE;    
            }
            _dump_book(fff, realm, i);
        }
    }
}

void spellbook_character_dump(FILE *fff)
{
    fprintf(fff, "\n==================================== Spells ===================================\n\n");

    if (p_ptr->pclass == CLASS_RED_MAGE || p_ptr->pclass == CLASS_SORCERER)
    {
        int realm;
        for (realm = REALM_LIFE; realm <= MAX_MAGIC; realm++)
            _dump_realm(fff, realm);
    }
    else
    {
        if (p_ptr->realm1)
            _dump_realm(fff, p_ptr->realm1);
        if (p_ptr->realm2)
            _dump_realm(fff, p_ptr->realm2);
    }

    if (p_ptr->old_realm)
    {
        int i;
        for (i = 0; i < MAX_MAGIC; i++)
        {
            if (!(p_ptr->old_realm & 1L << i)) continue;
            fprintf(fff, "\n You were able to use %s magic before.", realm_names[i+1]);
        }
        fputc('\n', fff);
    }
}

