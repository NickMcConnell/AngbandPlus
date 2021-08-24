#include "angband.h"

#include "str_map.h"
#include <assert.h>

static str_map_ptr _tbls = NULL;

/*************************************************************************
 * Monk Attack
 *************************************************************************/
typedef struct {
    int          level; /* minimum monk_lvl for this blow */
    int          fail;  /* pick again if 1dL < fail */
    mon_blow_ptr blow;
} _monk_attack_t, *_monk_attack_ptr;

static _monk_attack_ptr _monk_attack_alloc(void)
{
    _monk_attack_ptr a = malloc(sizeof(_monk_attack_t));
    a->level = 0;
    a->fail = 0;
    a->blow = mon_blow_alloc(RBM_PUNCH);
    return a;
}
static void _monk_attack_free(_monk_attack_ptr a)
{
    if (a)
    {
        mon_blow_free(a->blow);
        a->blow = NULL;
        free(a);
    }
}

/*************************************************************************
 * Monk Attack Table
 *************************************************************************/
typedef struct {
    char   *name;
    vec_ptr tbl;
} _monk_attack_tbl_t, *_monk_attack_tbl_ptr;

static _monk_attack_tbl_ptr _monk_attack_tbl_alloc(cptr name)
{
    _monk_attack_tbl_ptr t = malloc(sizeof(_monk_attack_tbl_t));
    t->name = malloc(strlen(name) + 1);
    strcpy(t->name, name);
    t->tbl = vec_alloc((vec_free_f)_monk_attack_free);
    return t;
}
static void _monk_attack_tbl_free(_monk_attack_tbl_ptr t)
{
    if (t)
    {
        vec_free(t->tbl);
        free(t->name);
        t->tbl = NULL;
        t->name = NULL;
        free(t);
    }
}
/* Roll best of N. We assume the table makes sense:
 * [1] attacks exist for every possible lvl
 * [2] no two attacks have the same lvl */
static mon_blow_ptr _monk_attack_tbl_choose(_monk_attack_tbl_ptr tbl, int lvl, int tries)
{
    int i;
    int tbl_size = vec_length(tbl->tbl);
    _monk_attack_ptr best = NULL;

    assert(tries > 0);
    for (i = 0; i < tries; i++)
    {
        _monk_attack_ptr current = NULL;

        for (;;)
        {
            current = vec_get(tbl->tbl, randint0(tbl_size));
            if (lvl < current->level) continue;
            if (current->fail && randint1(lvl) < current->fail) continue;
            break;
        }

        if (!best || best->level < current->level)
            best = current;
    }
    assert(best); /* make sure the table isn't messed up in monk_attacks.txt */
    return best->blow;
}
static _monk_attack_tbl_ptr _get_monk_attack_tbl(cptr tbl_name)
{
    if (!tbl_name) tbl_name = "Monk";
    return str_map_find(_tbls, tbl_name);
}
/*************************************************************************
 * Player
 *************************************************************************/
static int _max_tries_plr(void)
{
    int tries = 0;
    int lvl = plr->monk_lvl;
    /* XXX This assumes a certain table size ... 17 was the original
     * value. Perhaps we should make an effort to support smaller or
     * larger tables? XXX */
    if (plr->special_defense & KAMAE_BYAKKO)
        tries = (lvl < 3 ? 1 : lvl / 3);
    else if (plr->special_defense & KAMAE_SUZAKU)
        tries = 1;
    else if (plr->special_defense & KAMAE_GENBU)
        tries = 1;
    else if (mystic_get_toggle() == MYSTIC_TOGGLE_OFFENSE)
        tries = 1 + lvl/4;
    else if (mystic_get_toggle() == MYSTIC_TOGGLE_DEFENSE)
        tries = 1 + lvl/15;
    else
    {
        tries = (lvl < 7 ? 1 : lvl / 7);
        /* Note: Forcetrainers hit a max monk lvl of 47 for only 6 tries.
         * In Hengband, they got 7 tries, so we boost with the force a bit. */
        if (plr->pclass == CLASS_FORCETRAINER)
            tries += plr->magic_num1[0]/120;
    }
    if (plr_tim_find(T_CONFUSED))
        tries = 1;
    else if (plr_tim_find(T_STUN))
        tries -= tries * MIN(100, plr_tim_amount(T_STUN)) / 150;
    return MAX(1, tries);
}
static int _get_weight_plr(void)
{
    int weight = 6;
    if (plr->special_defense & KAMAE_SUZAKU) weight = 3;
    if (mystic_get_toggle() == MYSTIC_TOGGLE_DEFENSE) weight = 4;
    if (mystic_get_toggle() == MYSTIC_TOGGLE_OFFENSE) weight = 8;
    if ((plr->pclass == CLASS_FORCETRAINER) && (plr->magic_num1[0]))
    {
        weight += (plr->magic_num1[0]/40);
        if (weight > 20) weight = 20;
    }
    return weight * plr->monk_lvl;
}
mon_blow_ptr monk_choose_attack_plr(cptr tbl_name)
{
    _monk_attack_tbl_ptr tbl = _get_monk_attack_tbl(tbl_name);
    int                  tries = _max_tries_plr();
    mon_blow_ptr         blow = _monk_attack_tbl_choose(tbl, plr->monk_lvl, tries);
    blow->weight = _get_weight_plr();
    return blow;
}

/*************************************************************************
 * Monster
 *************************************************************************/
static int _monk_lvl_mon(mon_race_ptr race)
{
    return MAX(1, MIN(50, race->alloc.lvl));
}

static int _max_tries_mon(mon_ptr mon, int lvl)
{
    int tries = lvl < 7 ? 1 : lvl / 7;
    int stun = mon_tim_amount(mon, T_STUN);
    if (mon_tim_find(mon, T_CONFUSED))
        tries = 1;
    else if (stun)
        tries -= tries * MIN(100, stun) / 150;
    return MAX(1, tries);
}
mon_blow_ptr monk_choose_attack_mon(cptr tbl_name, mon_ptr mon)
{
    _monk_attack_tbl_ptr tbl = _get_monk_attack_tbl(tbl_name);
    int                  lvl = _monk_lvl_mon(mon->race);
    int                  tries = _max_tries_mon(mon, lvl);
    mon_blow_ptr         blow = _monk_attack_tbl_choose(tbl, lvl, tries);
    blow->weight = 8*lvl;
    return blow;
}

/*************************************************************************
 * Parser (../lib/edit/monk_attacks.txt)
 *************************************************************************/
static errr _parser(char *line, int options)
{
    static _monk_attack_tbl_ptr t = NULL;

    /* N:Monk */
    if (line[0] == 'N' && line[1] == ':')
    {
        char *zz[10];
        int   num = tokenize(line + 2, 10, zz, 0);

        if (num != 1 || !*zz[0])
        {
            msg_print("Error: Invalid N: line. Syntax: N:name.");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }

        t = _monk_attack_tbl_alloc(zz[0]);
        str_map_add(_tbls, t->name, t);
    }
    /* B:L:F:<mon_blow_t> */
    else if (line[0] == 'B' && line[1] == ':')
    {
        errr  rc = 0;
        char *zz[3];
        int   num;
        _monk_attack_ptr a;

        if (!t)
        {
            msg_print("Error: Missing N: line for this table.");
            return PARSE_ERROR_MISSING_RECORD_HEADER;
        }
        num = tokenize(line + 2, 3, zz, 0);
        if (num != 3)
        {
            msg_print("Error: Invalid B: line. Syntax: B:Level:Fail:HIT:HURT(XdY)...");
            return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        }
        a = _monk_attack_alloc();
        a->level = atoi(zz[0]);
        a->fail = atoi(zz[1]);
        rc = parse_mon_blow(zz[2], a->blow);
        if (rc) { _monk_attack_free(a); return rc; }
        vec_add(t->tbl, a);
    }
    else
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    return 0;
}
bool monk_attack_init(void)
{
    assert(!_tbls);
    _tbls = str_map_alloc((str_map_free_f)_monk_attack_tbl_free);
    return !parse_edit_file("monk_attack.txt", _parser, 0); /* errr -> bool */
}
void monk_attack_shutdown(void)
{
    str_map_free(_tbls);
    _tbls = NULL;
}
cptr monk_verify_table(cptr name)
{
    _monk_attack_tbl_ptr tbl = str_map_find(_tbls, name);
    if (tbl) return tbl->name;
    return NULL;
}

