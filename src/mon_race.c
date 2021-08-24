#include "angband.h"

#include <assert.h>

/************************************************************************
 * Monster Races (r_info)
 ************************************************************************/
static int_map_ptr _races;
static int_map_ptr _aliases;
/* Note: in case you want to change mon_race->id w/o breaking savefiles,
 * I have added a mechanism for aliases. You will need to add an X:<old_key>
 * line to r_info.txt for the old key. When you no longer need to support
 * the old key, remove the alias for efficiency. Note that the savefile
 * currently saves *every* race for lore purposes so any key change will
 * break every savefile ... */

mon_race_ptr mon_race_alloc(sym_t id)
{
    mon_race_ptr r = malloc(sizeof(mon_race_t));
    memset(r, 0, sizeof(mon_race_t));
    r->id = id;
    r->move.range = 20;
    r->move.sleep = 50;
    r->weight = 150;
    r->blows = vec_alloc((vec_free_f)mon_blow_free);
    r->body.body_id = sym_add("Standard");
    r->body.class_id = CLASS_NONE; /* cf _verify_mon_race */
    r->body.spell_stat = A_NONE;
    return r;
}
mon_race_ptr mon_race_alloc_ex(sym_t id, mon_race_ptr base)
{
    mon_race_ptr r = mon_race_alloc(id);

    assert(base);

    r->base_id = base->id;
    r->hp = base->hp;
    r->ac = base->ac;
    r->align = base->align;
    r->light = base->light;
    r->lantern = base->lantern;
    r->weight = base->weight;

    r->move = base->move;
    r->display = base->display;
    visual_set_ascii_aux(r->id, mon_race_visual_ascii(base), 0);

    /* n.b. care must be taken as to what is inherited! */
    r->alloc.flags = base->alloc.flags;   /* e.g. WILD(SHORE | OCEAN) */
    r->alloc.rarity = base->alloc.rarity; /* base rarity for this type */
    r->alloc.dun_type_id = base->alloc.dun_type_id; /* e.g. restrict unique camelot knights to D_CAMELOT */
    r->alloc.world_id = base->alloc.world_id; /* e.g. restrict entire class of monsters to given world */
    /* XXX do not copy max_max_num (cf p.camelot knight and p.Arthur) */
    /* XXX copying lvl and max_lvl similarly makes no sense ... templates never use these fields anyway */
    /* XXX other alloc fields are dynamic and not specified in r_info */

    r->kind = base->kind;
    r->abilities = base->abilities;
    r->attributes = base->attributes;
    r->resist = base->resist;
    r->immune = base->immune;
    r->vuln = base->vuln;

    r->body = base->body;

    /* XXX we do not copy spells, blows, auras, drops, friends or kin */

    r->attributes &= ~RF_TEMPLATE; /* cf _parse_template */
    return r;
}

void mon_race_free(mon_race_ptr race)
{
    mon_aura_ptr aura;
    if (!race) return;
    if (race->name) z_string_free(race->name);
    if (race->text) z_string_free(race->text);
    if (race->spells) mon_spells_free(race->spells);
    if (race->drops) mon_drop_free(race->drops);
    if (race->friends) mon_rule_free(race->friends);
    if (race->kin) mon_rule_free(race->kin);
    if (race->blows) vec_free(race->blows);
    for (aura = race->auras; aura; )
    {
        mon_aura_ptr next = aura->next;
        free(aura);
        aura = next;
    }
    free(race);
}

mon_race_ptr mon_race_lookup(sym_t id)
{
    mon_race_ptr r;
    assert(_races);
    if (!id) return NULL; /* mon_race_parse("@.player"); XXX need to fix these ... cf plr_mon_race() */
    r = int_map_find(_races, id);
    if (!r) r = int_map_find(_aliases, id);
    return r;
}

mon_race_ptr mon_race_parse(cptr token)
{
    sym_t id = sym_find(token);
    if (!id) return NULL;
    return mon_race_lookup(id);
}

/* we have many mon_race "predicates" of the form bool p(mon_race_ptr), 
 * such as mon_race_is_unique, etc. int_map_filter wants to pass along
 * the key since, in general, the value might not know the key (e.g.
 * in a map int->int), but, in our case, race->id gives the key. */
static mon_race_p _filter;
static bool _mon_race_filter(int id, mon_race_ptr race)
{
    if (race->attributes & (RF_TEMPLATE | RF_DEPRECATED)) return FALSE;
    if (!_filter) return TRUE;
    return _filter(race);
}
vec_ptr mon_race_filter(mon_race_p filter)
{
    _filter = filter;
    return int_map_filter(_races, (int_map_filter_f)_mon_race_filter);
}

static mon_race_f _action;
static void _mon_race_iter(int id, mon_race_ptr race)
{
    assert(_action);
    if (race->attributes & (RF_TEMPLATE | RF_DEPRECATED)) return;
    _action(race);
}
void mon_race_iter(mon_race_f f)
{
    _action = f;
    int_map_iter(_races, (int_map_iter_f)_mon_race_iter);
}

bool mon_race_is_(mon_race_ptr race, cptr which)
{
    assert(mon_race_parse(which)); /* paranoia wrt my fat fingers ... */
    return sym_equals(race->id, which);
}
bool mon_race_is_one_(mon_race_ptr race, cptr which[])
{
    int i;
    for (i = 0; ; i++)
    {
        cptr w = which[i];
        if (!w) break;
        if (mon_race_is_(race, w)) return TRUE;
    }
    return FALSE;
}


bool mon_race_is_d_char(char c)
{
    int_map_iter_ptr iii;
    bool found = FALSE;

    for (iii = int_map_iter_alloc(_races);
            !found && int_map_iter_is_valid(iii);
            int_map_iter_next(iii))
    {
        mon_race_ptr r = int_map_iter_current(iii);
        if (mon_race_is_char(r, c))
            found = TRUE;
    }
    int_map_iter_free(iii);
    return found;
}

/************************************************************************
 * r_info parser helpers
 ************************************************************************/
static mon_race_ptr _current;

/* <type>:<line> */
typedef struct {
    char  *type;
    char  *line;
} _parse_line_t, *_parse_line_ptr;
_parse_line_t _parse_line(char *line)
{
    _parse_line_t pl = {0};
    char         *split = strchr(line, ':');
    if (!split) return pl;

    *split = '\0';
    pl.type = line;
    pl.line = split + 1;
    return pl;
}

/* dispatch on <type> to parse a given line */
typedef errr (*_parse_f)(char *buf);
typedef struct {
    cptr type;
    cptr alias;
    _parse_f f;
} _parse_tbl_t, *_parse_tbl_ptr;

static errr _dispatch(_parse_line_t pl, _parse_tbl_ptr tbl)
{
    int i;
    errr rc = 0;
    for (i = 0; ; i++)
    {
        _parse_tbl_ptr t = &tbl[i];
        if (!t->f)
        {
            msg_format("Unkown line type: <color:r>%s</color>.", pl.type);
            msg_format("\nLine: <color:U>%s</color>.", pl.line);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        if ( strcmp(t->type, pl.type) == 0
          || (t->alias && strcmp(t->alias, pl.type) == 0) ) /* alias is optional */
        {
            rc = t->f(pl.line);
            break;
        }
    }
    return rc;
}

#define _MAX_ARGS 20
typedef struct {
    char *name;
    char *args[_MAX_ARGS];
    int   arg_ct;
} _exp_t, *_exp_ptr;

static bool _exp(char *buf, _exp_ptr exp)
{
    exp->arg_ct = parse_args(buf, &exp->name, exp->args, _MAX_ARGS);
    return exp->arg_ct >= 0;
}

typedef errr (*_exp_f)(_exp_ptr exp);
typedef struct {
    cptr name;
    _exp_f f;
} _exp_tbl_t, *_exp_tbl_ptr;

static errr _dispatch_exp(_exp_ptr exp, _exp_tbl_ptr tbl)
{
    int i;
    errr rc = 0;
    for (i = 0; ; i++)
    {
        _exp_tbl_ptr t = &tbl[i];
        if (!t->f)
        {
            msg_format("Unkown expression: <color:r>%s</color>.", exp->name);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        if (strcmp(t->name, exp->name) == 0)
        {
            rc = t->f(exp);
            break;
        }
    }
    return rc;
}

/************************************************************************
 * info parser: info lines contain multiple parse expressions, in any
 * convenient order
 ************************************************************************/
static errr _info_speed(_exp_ptr e)
{
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    /* XXX handle +20 or -15 ... 
     * if (!is_numeric(args[0])) return PARSE_ERROR_GENERIC; */
    _current->move.speed = atoi(e->args[0]);
    if (_current->move.speed > 80) return PARSE_ERROR_OUT_OF_BOUNDS;
    return ERROR_SUCCESS;
}
static errr _info_hp(_exp_ptr e)
{
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    return dice_parse(&_current->hp, e->args[0]);
}
static errr _info_ac(_exp_ptr e)
{
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    if (!is_numeric(e->args[0])) return PARSE_ERROR_GENERIC;
    _current->ac = atoi(e->args[0]);
    return ERROR_SUCCESS;
}
static errr _info_rarity(_exp_ptr e)
{
    int r;
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    if (!is_numeric(e->args[0])) return PARSE_ERROR_GENERIC;
    r = atoi(e->args[0]);
    if (r < 0 || r > 255) return PARSE_ERROR_OUT_OF_BOUNDS;
    _current->alloc.rarity = r;
    return ERROR_SUCCESS;
}
static errr _info_exp(_exp_ptr e)
{
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    if (!is_numeric(e->args[0])) return PARSE_ERROR_GENERIC;
    _current->mexp = atoi(e->args[0]);
    return ERROR_SUCCESS;
}
static errr _info_alert(_exp_ptr e)
{
    int i;
    for (i = 0; i < e->arg_ct; i++)
    {
        char *arg = e->args[i];
        int n;
        char c;

        if (sscanf(arg, "%d%c", &n, &c) == 2 && c == '\'')
            _current->move.range = n/10;
        else if (streq(arg, "Ever Vigilant"))
            _current->move.sleep = 0;
        else if (streq(arg, "Vigilant"))
            _current->move.sleep = 1;
        else if (streq(arg, "Very Observant"))
            _current->move.sleep = 3;
        else if (streq(arg, "Observant"))
            _current->move.sleep = 5;
        else if (streq(arg, "Fairly Observant"))
            _current->move.sleep = 10;
        else if (streq(arg, "Fairly Unseeing"))
            _current->move.sleep = 25;
        else if (streq(arg, "Unseeing"))
            _current->move.sleep = 40;
        else if (streq(arg, "Overlooks"))
            _current->move.sleep = 60;
        else if (streq(arg, "Inattentive"))
            _current->move.sleep = 80;
        else if (streq(arg, "Very Inattentive"))
            _current->move.sleep = 100;
        else if (streq(arg, "Ignores"))
            _current->move.sleep = 200;
        else
        {
            msg_format("Unkown arg to ALERT: <color:r>%s</color>", arg);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}
/* DISPLAY(c,a[,FLAG | FLAG ...]) */
static errr _info_display(_exp_ptr e)
{
    term_char_t tc = term_char_create(_current->display.c, TERM_WHITE);

    if (e->arg_ct < 1 || 3 < e->arg_ct) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    switch (strlen(e->args[0]))
    {
    case 0: break; /* templates ... */
    case 1: tc.c = e->args[0][0]; break;
    default: return PARSE_ERROR_INVALID_FLAG;
    }

    if (e->arg_ct >= 2 && strlen(e->args[1]))
    {
        byte a = color_str_to_attr(e->args[1]);
        if (a > 127) return PARSE_ERROR_GENERIC;
        tc.a = a;
    }

    if (e->arg_ct >= 3)
    {
        char *flags[20];
        int   flag_ct = z_string_split(e->args[2], flags, 20, "|");
        int   j;
        for (j = 0; j < flag_ct; j++)
        {
            char *flag = flags[j];
            if (streq(flag, "CHAR_CLEAR"))
                _current->display.flags |= RFD_CHAR_CLEAR;
            else if (streq(flag, "ATTR_CLEAR"))
                _current->display.flags |= RFD_ATTR_CLEAR;
            else if (streq(flag, "ATTR_MULTI"))
                _current->display.flags |= RFD_ATTR_MULTI;
            else if (streq(flag, "ATTR_ANY"))
                _current->display.flags |= RFD_ATTR_ANY;
            else if (streq(flag, "ATTR_SEMIRAND"))
                _current->display.flags |= RFD_ATTR_SEMIRAND;
            else if (streq(flag, "SHAPECHANGER"))
                _current->display.flags |= RFD_SHAPECHANGER;
            else
            {
                msg_format("Invalid DISPLAY flag: <color:r>%s</color>", flag);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }
    }
    _current->display.c = tc.c;
    visual_set_ascii_aux(_current->id, tc, 0);
    return ERROR_SUCCESS;
}
/* COLOR(Violet) rather than DISPLAY(,Violet) ... common for templates */
static errr _info_color(_exp_ptr e)
{
    term_char_t tc = visual_get_ascii_aux(_current->id);
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    tc.a = color_str_to_attr(e->args[0]);
    if (tc.a > 127) return PARSE_ERROR_GENERIC;
    visual_set_ascii_aux(_current->id, tc, 0);
    return ERROR_SUCCESS;
}
/* EVOLVE(d.blue.mature, 4000) on d.blue.young */
static errr _info_evolve(_exp_ptr e)
{
    if (e->arg_ct != 2) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    _current->evolution.id = sym_add(e->args[0]); /* cf _verify_mon_race */
    _current->evolution.exp = atoi(e->args[1]);
    return ERROR_SUCCESS;
}
/* LVL(9) or LVL(9,40) XXX used to be W:9 to 40:... but I think LVL(9,40) is OK */
static errr _info_lvl(_exp_ptr e)
{
    if (e->arg_ct < 1 || 2 < e->arg_ct) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    _current->alloc.lvl = atoi(e->args[0]);
    if (e->arg_ct >= 2)
        _current->alloc.max_lvl = atoi(e->args[1]);
    return ERROR_SUCCESS;
}
static void _alloc_flag(u32b flag, bool set)
{
    if (set) _current->alloc.flags |= flag;
    else _current->alloc.flags &= ~flag;
}
/* ALLOC(D_ANGBAND | W_AMBER | MAX_5 | NO_QUEST | FORCE_DEPTH)
 * Handles normal dungeon allocation. Using 'D_ANGBAND' vs 'Angband'
 * allows us to efficiently know to delegate to dun_types_parse. */
static errr _info_alloc(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];

        /* special cases */
        if (prefix(flag, "D_"))
        {
            _current->alloc.dun_type_id = dun_types_parse(flag);
            if (!_current->alloc.dun_type_id)
            {
                msg_format("Unknown Dungeon <color:r>%s</color>", flag);
                return PARSE_ERROR_GENERIC;
            }
        }
        else if (prefix(flag, "W_"))
        {
            _current->alloc.world_id = dun_worlds_parse(flag);
            if (!_current->alloc.world_id)
            {
                msg_format("Unknown World <color:r>%s</color>", flag);
                return PARSE_ERROR_GENERIC;
            }
        }
        else if (prefix(flag, "MAX_")) /* e.g. Nazgul gets MAX_5 */
        {
            int n = 0;
            if (sscanf(flag, "MAX_%d", &n) != 1)
                return PARSE_ERROR_GENERIC;
            if (n < 0 || n > 100) return PARSE_ERROR_GENERIC;
            _current->alloc.max_max_num = n;
        }
        /* normal flags ... Note: RFA_WILD_* use the WILD directive */
        else
        {
            bool set = TRUE;
            if (*flag == '+') flag++;
            if (*flag == '-') { flag++; set = FALSE; }
            if (strcmp(flag, "UNIQUE") == 0)
                _alloc_flag(RFA_UNIQUE, set);
            else if (strcmp(flag, "ESCORT") == 0)
                _alloc_flag(RFA_ESCORT, set);
            else if (strcmp(flag, "FORCE_DEPTH") == 0)
                _alloc_flag(RFA_FORCE_DEPTH, set);
            else if (strcmp(flag, "NO_QUEST") == 0)
                _alloc_flag(RFA_NO_QUEST, set);
            else if (strcmp(flag, "NO_SUMMON") == 0)
                _alloc_flag(RFA_NO_SUMMON, set);
            else if (strcmp(flag, "GUARDIAN") == 0)
                _alloc_flag(RFA_GUARDIAN, set);
            else if (strcmp(flag, "UNIQUE2") == 0)
                _alloc_flag(RFA_UNIQUE2, set);
            else if (strcmp(flag, "WEB") == 0)
                _alloc_flag(RFA_WEB, set);
            else
            {
                msg_format("<color:r>%s</color> does not compute!", flag);
                return PARSE_ERROR_UNDEFINED_DIRECTIVE;
            }
        }
    }
    return ERROR_SUCCESS;
}
/* WILD(TOWN | GRASS | ONLY)
 * Handles D_SURFACE allocation */
static errr _info_wild(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool  set = TRUE;

        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }

        if (strcmp(flag, "ONLY") == 0)
            _alloc_flag(RFA_WILD_ONLY, set);
        else if (strcmp(flag, "ALL") == 0)
            _alloc_flag(RFA_WILD_ALL, set);
        else if (strcmp(flag, "TOWN") == 0)
            _alloc_flag(RFA_WILD_TOWN, set);
        else if (strcmp(flag, "SHORE") == 0)
            _alloc_flag(RFA_WILD_SHORE, set);
        else if (strcmp(flag, "OCEAN") == 0)
            _alloc_flag(RFA_WILD_OCEAN, set);
        else if (strcmp(flag, "WASTE") == 0)
            _alloc_flag(RFA_WILD_WASTE, set);
        else if (strcmp(flag, "WOOD") == 0)
            _alloc_flag(RFA_WILD_WOOD, set);
        else if (strcmp(flag, "VOLCANO") == 0)
            _alloc_flag(RFA_WILD_VOLCANO, set);
        else if (strcmp(flag, "MOUNTAIN") == 0)
            _alloc_flag(RFA_WILD_MOUNTAIN, set);
        else if (strcmp(flag, "GRASS") == 0)
            _alloc_flag(RFA_WILD_GRASS, set);
        else if (strcmp(flag, "SWAMP") == 0)
            _alloc_flag(RFA_WILD_SWAMP, set);
        else
        {
            msg_format("<color:r>%s</color> is not a valid wilderness flag.", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}
/* MOVE(PUSH|PICKUP|SWIM|FLY|RAND_30) ... cf SPEED and ALERT */
static void _move_flag(u32b flag, bool set)
{
    if (set) _current->move.flags |= flag;
    else _current->move.flags &= ~flag;
}
static errr _info_move(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool set = TRUE;

        /* special cases */
        if (prefix(flag, "RAND_"))
        {
            int n = 0;
            if (sscanf(flag, "RAND_%d", &n) != 1)
                return PARSE_ERROR_GENERIC;
            if (n < 0 || n > 100) return PARSE_ERROR_GENERIC;
            _current->move.random = n;
            continue;
        }
        /* normal flags */
        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }
        if (strcmp(flag, "NEVER") == 0)
            _move_flag(RFM_NEVER, set);
        else if (strcmp(flag, "OPEN") == 0)
            _move_flag(RFM_OPEN, set);
        else if (strcmp(flag, "BASH") == 0)
            _move_flag(RFM_BASH, set);
        else if (strcmp(flag, "PUSH") == 0)
            _move_flag(RFM_PUSH, set);
        else if (strcmp(flag, "TRAMPLE") == 0)
            _move_flag(RFM_TRAMPLE, set);
        else if (strcmp(flag, "PICKUP") == 0)
            _move_flag(RFM_PICKUP, set);
        else if (strcmp(flag, "DESTROY") == 0)
            _move_flag(RFM_DESTROY, set);
        else if (strcmp(flag, "PASSWALL") == 0)
            _move_flag(RFM_PASSWALL, set);
        else if (strcmp(flag, "TUNNEL") == 0)
            _move_flag(RFM_TUNNEL, set);
        else if (strcmp(flag, "SWIM") == 0)
            _move_flag(RFM_SWIM, set);
        else if (strcmp(flag, "FLY") == 0)
            _move_flag(RFM_FLY, set);
        else if (strcmp(flag, "CLIMB") == 0)
            _move_flag(RFM_CLIMB, set);
        else if (strcmp(flag, "TRUMP") == 0)
            _move_flag(RFM_TRUMP, set);
        else if (strcmp(flag, "QUICK") == 0)
            _move_flag(RFM_QUICK, set);
        else if (strcmp(flag, "PASSWEB") == 0 || strcmp(flag, "WEB") == 0)
            _move_flag(RFM_PASSWEB, set);
        else if (strcmp(flag, "CLEARWEB") == 0)
            _move_flag(RFM_CLEARWEB, set);
        else
        {
            msg_format("<color:r>%s</color> does not compute!", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}
static void _gf_flag(u32b *bits, u32b mask, bool set)
{
    if (set) *bits |= mask;
    else *bits &= ~mask;
}
static errr _info_gf(u32b *bits, _exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool  set = TRUE;
        gf_info_ptr gfi;

        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }

        if (strcmp(flag, "BASE") == 0)
        {   /* ACID|ELEC|FIRE|COLD */
            _gf_flag(bits, (1U << GF_ACID), set);
            _gf_flag(bits, (1U << GF_ELEC), set);
            _gf_flag(bits, (1U << GF_FIRE), set);
            _gf_flag(bits, (1U << GF_COLD), set);
            continue;
        }
        if (strcmp(flag, "ALL") == 0)
        {   /* ACID|ELEC|FIRE|COLD|POIS|LIGHT|DARK|CONF|NETHER|NEXUS|SOUND|SHARDS|CHAOS|DISENCHANT|
               TIME|WATER|PLASMA|FORCE|INERTIA|GRAVITY */
            int k;
            for (k = GF_RES_MIN; k <= GF_RES_MAX; k++)
            {
                if (k == GF_DISINTEGRATE) continue; /* XXX oddball for HURT_ROCK */
                gfi = gf_lookup(k);
                if (gfi->flags & GFF_ELEMENTAL)/* skip SLEEP|STUN|SLOW|BLIND|FEAR|TELE */
                    _gf_flag(bits, (1U << k), set);
            }
            continue;
        }

        gfi = gf_parse_name(flag);
        if (!gfi)
        {
            msg_format("Unkown GF flag: <color:r>%s</color>", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        if (gfi->id < GF_RES_MIN || GF_RES_MAX < gfi->id)
        {
            msg_format("Invalid GF flag: <color:r>%s</color>", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        _gf_flag(bits, (1U << gfi->id), set);
    }
    return ERROR_SUCCESS;
}
static errr _info_resist(_exp_ptr e) { return _info_gf(&_current->resist, e); }
static errr _info_immune(_exp_ptr e) { return _info_gf(&_current->immune, e); }
static errr _info_vuln(_exp_ptr e) { return _info_gf(&_current->vuln, e); }
static void _pos_flag(u32b flag, bool set)
{
    if (set) _current->body.flags |= flag;
    else _current->body.flags &= ~flag;
}
static errr _info_pos(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool  set = TRUE;

        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }

        if (strcmp(flag, "DROP_CORPSE") == 0 || strcmp(flag, "CORPSE") == 0)
            _pos_flag(RF_DROP_CORPSE, set);
        else if (strcmp(flag, "DROP_SKELETON") == 0 || strcmp(flag, "SKELETON") == 0)
            _pos_flag(RF_DROP_SKELETON, set);
        else if (strcmp(flag, "GAIN_AC") == 0 || strcmp(flag, "AC") == 0)
            _pos_flag(RF_POS_GAIN_AC, set);
        else if (strcmp(flag, "SEE_INVIS") == 0)
            _pos_flag(RF_POS_SEE_INVIS, set);
        else if (strcmp(flag, "HOLD_LIFE") == 0)
            _pos_flag(RF_POS_HOLD_LIFE, set);
        else if (strcmp(flag, "TELEPATHY") == 0)
            _pos_flag(RF_POS_TELEPATHY, set);
        else if (strcmp(flag, "DISABLED") == 0 || strcmp(flag, "NO") == 0)
            _pos_flag(RF_POS_DISABLED, set);
        else if (strcmp(flag, "SUST_ALL") == 0)
        {
            _pos_flag(RF_POS_SUST_STR, set);
            _pos_flag(RF_POS_SUST_INT, set);
            _pos_flag(RF_POS_SUST_WIS, set);
            _pos_flag(RF_POS_SUST_DEX, set);
            _pos_flag(RF_POS_SUST_CON, set);
            _pos_flag(RF_POS_SUST_CHR, set);
        }
        else if (strcmp(flag, "SUST_STR") == 0)
            _pos_flag(RF_POS_SUST_STR, set);
        else if (strcmp(flag, "SUST_INT") == 0)
            _pos_flag(RF_POS_SUST_INT, set);
        else if (strcmp(flag, "SUST_WIS") == 0)
            _pos_flag(RF_POS_SUST_WIS, set);
        else if (strcmp(flag, "SUST_DEX") == 0)
            _pos_flag(RF_POS_SUST_DEX, set);
        else if (strcmp(flag, "SUST_CON") == 0)
            _pos_flag(RF_POS_SUST_CON, set);
        else if (strcmp(flag, "SUST_CHR") == 0)
            _pos_flag(RF_POS_SUST_CHR, set);
        else if (strcmp(flag, "BACKSTAB") == 0)
            _pos_flag(RF_POS_BACKSTAB, set);
        else
        {
            msg_format("<color:r>%s</color> does not compute!", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}
static errr _info_align(_exp_ptr e)
{
    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    if (is_numeric(e->args[0]))
    {
        int n = atoi(e->args[0]);
        if (n < -255 || 255 < n) return PARSE_ERROR_OUT_OF_BOUNDS;
        _current->align = n;
    }
    else if (strcmp(e->args[0], "Chaotic") == 0)
        _current->align = ALIGN_CHAOTIC;
    else if (strcmp(e->args[0], "Very Evil") == 0)
        _current->align = ALIGN_VERY_EVIL;
    else if (strcmp(e->args[0], "Evil") == 0)
        _current->align = ALIGN_EVIL;
    else if (strcmp(e->args[0], "Neutral Evil") == 0)
        _current->align = ALIGN_NEUTRAL_EVIL;
    else if (strcmp(e->args[0], "Neutral") == 0)
        _current->align = ALIGN_NEUTRAL;
    else if (strcmp(e->args[0], "Neutral Good") == 0)
        _current->align = ALIGN_NEUTRAL_GOOD;
    else if (strcmp(e->args[0], "Good") == 0)
        _current->align = ALIGN_GOOD;
    else if (strcmp(e->args[0], "Very Good") == 0)
        _current->align = ALIGN_VERY_GOOD;
    else if (strcmp(e->args[0], "Lawful") == 0)
        _current->align = ALIGN_LAWFUL;
    else
    {
        msg_format("Unkown ALIGN: <color:r>%s</color>", e->args[0]);
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    return ERROR_SUCCESS;
}
/* LIGHT(x) or LIGHT(x+y) ... Use LIGHT(0+2) to convert HAS_LITE_2 with no SELF_LITE_ flags */
static errr _info_lite(_exp_ptr e)
{
    int x, y;
    char c;
    if (e->arg_ct != 1)
    {
        msg_print("Syntax Error: Expected LIGHT(x) or LIGHT(x+y)");
        return PARSE_ERROR_GENERIC;
    }
    if (sscanf(e->args[0], "%d%c%d", &x, &c, &y) == 3)
    {
        if (c != '+') return PARSE_ERROR_GENERIC;
        if (x < 0 || y < 0) return PARSE_ERROR_OUT_OF_BOUNDS;
        if (x > 3 || y > 3) return PARSE_ERROR_OUT_OF_BOUNDS;
        _current->light = x;
        _current->lantern = y;
    }
    else if (is_numeric(e->args[0]))
    {
        x = atoi(e->args[0]);
        if (x < 0 || x > 3) return PARSE_ERROR_OUT_OF_BOUNDS;
        _current->light = x;
        _current->lantern = 0;
    }
    else
    {
        msg_format("<color:r>%s</color> does not compute!", e->args[0]);
        return PARSE_ERROR_GENERIC;
    }
    return ERROR_SUCCESS;
}
/* DARK(x) or DARK(x+y) ... Use DARK(0+2) to convert HAS_DARK_2 with no SELF_DARK_ flags */
static errr _info_dark(_exp_ptr e)
{
    int x, y;
    char c;
    if (e->arg_ct != 1)
    {
        msg_print("Syntax Error: Expected DARK(x) or DARK(x+y)");
        return PARSE_ERROR_GENERIC;
    }
    if (sscanf(e->args[0], "%d%c%d", &x, &c, &y) == 3)
    {
        if (c != '+') return PARSE_ERROR_GENERIC;
        if (x < 0 || y < 0) return PARSE_ERROR_OUT_OF_BOUNDS;
        if (x > 3 || y > 3) return PARSE_ERROR_OUT_OF_BOUNDS;
        _current->light = -x;
        _current->lantern = -y;
    }
    else if (is_numeric(e->args[0]))
    {
        x = atoi(e->args[0]);
        if (x < 0 || x > 3) return PARSE_ERROR_OUT_OF_BOUNDS;
        _current->light = -x;
        _current->lantern = 0;
    }
    else
    {
        msg_format("<color:r>%s</color> does not compute!", e->args[0]);
        return PARSE_ERROR_GENERIC;
    }
    return ERROR_SUCCESS;
}
static void _kind_flag(u32b flag, bool set)
{
    if (set) _current->kind |= flag;
    else _current->kind &= ~flag;
}
static errr _info_kind(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool set = TRUE;

        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }

        if (strcmp(flag, "ORC") == 0)
            _kind_flag(RFK_ORC, set);
        else if (strcmp(flag, "TROLL") == 0)
            _kind_flag(RFK_TROLL, set);
        else if (strcmp(flag, "GIANT") == 0)
            _kind_flag(RFK_GIANT, set);
        else if (strcmp(flag, "DRAGON") == 0)
            _kind_flag(RFK_DRAGON, set);
        else if (strcmp(flag, "DEMON") == 0)
            _kind_flag(RFK_DEMON, set);
        else if (strcmp(flag, "UNDEAD") == 0)
            _kind_flag(RFK_UNDEAD, set);
        else if (strcmp(flag, "ANIMAL") == 0)
            _kind_flag(RFK_ANIMAL, set);
        else if (strcmp(flag, "HUMAN") == 0)
            _kind_flag(RFK_HUMAN, set);
        else if (strcmp(flag, "ELF") == 0)
            _kind_flag(RFK_ELF, set);
        else if (strcmp(flag, "DARK_ELF") == 0)
            _kind_flag(RFK_DARK_ELF, set);
        else if (strcmp(flag, "HOBBIT") == 0)
            _kind_flag(RFK_HOBBIT, set);
        else if (strcmp(flag, "DWARF") == 0)
            _kind_flag(RFK_DWARF, set);
        else if (strcmp(flag, "AMBERITE") == 0)
            _kind_flag(RFK_AMBERITE, set);
        else if (strcmp(flag, "THIEF") == 0)
            _kind_flag(RFK_THIEF, set);
        else if (strcmp(flag, "KNIGHT") == 0)
            _kind_flag(RFK_KNIGHT, set);
        else if (strcmp(flag, "OLYMPIAN") == 0)
            _kind_flag(RFK_OLYMPIAN, set);
        else if (strcmp(flag, "NONLIVING") == 0)
            _kind_flag(RFK_NONLIVING, set);
        else if (strcmp(flag, "AQUATIC") == 0)
            _kind_flag(RFK_AQUATIC, set);
        else if (strcmp(flag, "NAZGUL") == 0)
            _kind_flag(RFK_NAZGUL, set);
        else if (strcmp(flag, "HORROR") == 0)
            _kind_flag(RFK_HORROR, set);
        else
        {
            msg_format("<color:r>%s</color> does not compute!", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}
static void _abilities_flag(u32b flag, bool set)
{
    if (set) _current->abilities |= flag;
    else _current->abilities &= ~flag;
}
static errr _info_abilities(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool set = TRUE;

        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }

        if (strcmp(flag, "SPEAK") == 0)
            _abilities_flag(RF_SPEAK, set);
        else if (strcmp(flag, "REFLECT") == 0)
            _abilities_flag(RF_REFLECT, set);
        else if (strcmp(flag, "INVIS") == 0)
            _abilities_flag(RF_INVIS, set);
        else if (strcmp(flag, "MULTIPLY") == 0)
            _abilities_flag(RF_MULTIPLY, set);
        else if (strcmp(flag, "REGEN") == 0)
            _abilities_flag(RF_REGEN, set);
        else if (strcmp(flag, "REVENGE") == 0)
            _abilities_flag(RF_REVENGE, set);
        else if (strcmp(flag, "FEAR") == 0)
            _abilities_flag(RF_FEAR, set);
        else
        {
            msg_format("<color:r>%s</color> does not compute!", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}
static void _attributes_flag(u32b flag, bool set)
{
    if (set) _current->attributes |= flag;
    else _current->attributes &= ~flag;
}
static errr _info_attributes(_exp_ptr e)
{
    char *flags[20];
    int   flag_ct, j;

    if (e->arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    flag_ct = z_string_split(e->args[0], flags, 20, "|");
    for (j = 0; j < flag_ct; j++)
    {
        char *flag = flags[j];
        bool set = TRUE;

        if (*flag == '+') flag++;
        if (*flag == '-') { flag++; set = FALSE; }

        if (strcmp(flag, "MALE") == 0)
        {
            if (set) _attributes_flag(RF_FEMALE, FALSE);
            _attributes_flag(RF_MALE, set);
        }
        else if (strcmp(flag, "FEMALE") == 0)
        {
            if (set) _attributes_flag(RF_MALE, FALSE);
            _attributes_flag(RF_FEMALE, set);
        }
        else if (strcmp(flag, "SMART") == 0)
            _attributes_flag(RF_SMART, set);
        else if (strcmp(flag, "STUPID") == 0)
            _attributes_flag(RF_STUPID, set);
        else if (strcmp(flag, "WEIRD_MIND") == 0)
            _attributes_flag(RF_WEIRD_MIND, set);
        else if (strcmp(flag, "EMPTY_MIND") == 0)
            _attributes_flag(RF_EMPTY_MIND, set);
        else if (strcmp(flag, "COLD_BLOOD") == 0)
            _attributes_flag(RF_COLD_BLOOD, set);
        else if (strcmp(flag, "FRIENDLY") == 0)
            _attributes_flag(RF_FRIENDLY, set);
        else if (strcmp(flag, "RIDING") == 0)
            _attributes_flag(RF_RIDING, set);
        else if (strcmp(flag, "KILL_EXP") == 0)
            _attributes_flag(RF_KILL_EXP, set);
        else if (strcmp(flag, "IM_ILLUSION") == 0)
            _attributes_flag(RF_IM_ILLUSION, set);
        /* XXX Use T: lines to indicate a template ... cf mon_race_alloc_ex
        else if (strcmp(flag, "TEMPLATE") == 0)
            _attributes_flag(RF_TEMPLATE, set); */
        else if (strcmp(flag, "DEPRECATED") == 0)
            _attributes_flag(RF_DEPRECATED, set);
        else
        {
            msg_format("<color:r>%s</color> does not compute!", flag);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    return ERROR_SUCCESS;
}

static _exp_tbl_t _info_tbl[] = {
    { "LVL", _info_lvl },
    { "SPEED", _info_speed },
    { "HP", _info_hp },
    { "AC", _info_ac },
    { "RARITY", _info_rarity },
    { "EXP", _info_exp },
    { "KIND", _info_kind },
    { "CAN", _info_abilities },
    { "ATTR", _info_attributes },
    { "ALERT", _info_alert },
    { "DISPLAY", _info_display },
    { "COLOR", _info_color },
    { "EVOLVE", _info_evolve },
    { "ALLOC", _info_alloc },
    { "WILD", _info_wild },
    { "MOVE", _info_move },
    { "RESIST", _info_resist },
    { "IMMUNE", _info_immune },
    { "VULN", _info_vuln },
    { "POS", _info_pos }, /* XXX */
    { "ALIGN", _info_align },
    { "LIGHT", _info_lite },
    { "DARK", _info_dark },
    { 0 }
};
static errr _parse_info(char *buf)
{
    char *tokens[10];
    int   token_ct = tokenize(buf, 10, tokens, TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);
    int   i, rc;

    assert(_current);

    for (i = 0; i < token_ct; i++)
    {
        char  *token = tokens[i];
        _exp_t exp = {0};

        if (!_exp(token, &exp))
        {
            msg_format("Malformed Info Expression: <color:r>%s</color>", token);
            return PARSE_ERROR_GENERIC;
        }
        rc = _dispatch_exp(&exp, _info_tbl);
        if (rc != ERROR_SUCCESS)
            return rc;
    }
    return ERROR_SUCCESS;
}
 
/************************************************************************
 * possessor body parser: note the multilevel dispatch
 * _parse_mon_race(P:Copy:J.Chaos)
 *   _parse_possessor(Copy:J.Chaos)
 *      _pos_copy(J.Chaos)
 ************************************************************************/
static errr _pos_copy(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);
    int   idx, i;

    if (num < 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    /* P:Copy:<id> ... */
    if (num == 1)
    {
        u32b flags = _current->body.flags; /* XXX for now, do not copy flags */
        mon_race_ptr src = mon_race_parse(zz[0]);
        if (!src) /* one pass parser ... */
        {
            msg_format("Unknown source monster: <color:r>%s</color>.", zz[0]);
            return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;
        }
        _current->body = src->body;
        _current->body.flags = flags;
        _current->weight = src->weight;
    }
    /* P:Copy:Yeek:Tourist */
    else if (num == 2)
    {
        idx = plr_race_parse(zz[0]);
        if (0 <= idx && idx < MAX_RACES) /* XXX don't lookup monsters or mimic forms */
        {
            race_t *race_ptr = get_race_aux(idx, 0);
            for (i = 0; i < MAX_STATS; i++)
                _current->body.stats[i] = race_ptr->stats[i];
            _current->body.skills = race_ptr->skills;
            _current->body.extra_skills = race_ptr->extra_skills;
            _current->body.life = race_ptr->life;
            _current->body.infra = race_ptr->infra;
        }
        else
            return PARSE_ERROR_OUT_OF_BOUNDS;

        idx = plr_class_parse(zz[1]);
        if (idx != CLASS_NONE)
        {
            plr_class_ptr pc = plr_class_aux(idx, 0);
            _current->body.class_id = idx;
            for (i = 0; i < MAX_STATS; i++)
                _current->body.stats[i] += pc->stats[i];
            skills_add(&_current->body.skills, &pc->skills);
            skills_add(&_current->body.extra_skills, &pc->extra_skills);
            _current->body.life = _current->body.life * pc->life / 100;
            if (pc->hooks.caster_info)
                _current->body.spell_stat = (s16b)pc->hooks.caster_info()->which_stat;
            else
                _current->body.spell_stat = A_NONE;
        }
        else
            return PARSE_ERROR_OUT_OF_BOUNDS;
    }
    else
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    return ERROR_SUCCESS;
}
/* P:Stat:Str(3+7):...
 * This version overwrites existing settings; allows specification of
 * level based extra bonuses; allows skipping and reordering stats */
static errr _pos_stat(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);
    int   i;

    for (i = 0; i < num; i++)
    {
        char *token = zz[i];
        char *name;
        char *args[10];
        int   arg_ct = parse_args(token, &name, args, 10);
        int   b = 0, x = 0;

        if (arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        /* Str(3+2) */
        if (sscanf(args[0], "%d+%d", &b, &x) == 2)
        {
        }
        /* Int(-3-5) */
        else if (sscanf(args[0], "%d-%d", &b, &x) == 2)
        {
            x *= -1;
        }
        /* Con(5) */
        else
        {
            b = atoi(args[0]);
            x = 0;
        }

        if (strcmp(name, "Str") == 0)
        {
            _current->body.stats[A_STR] = b;
            _current->body.extra_stats[A_STR] = x;
        }
        else if (strcmp(name, "Int") == 0)
        {
            _current->body.stats[A_INT] = b;
            _current->body.extra_stats[A_INT] = x;
        }
        else if (strcmp(name, "Wis") == 0)
        {
            _current->body.stats[A_WIS] = b;
            _current->body.extra_stats[A_WIS] = x;
        }
        else if (strcmp(name, "Dex") == 0)
        {
            _current->body.stats[A_DEX] = b;
            _current->body.extra_stats[A_DEX] = x;
        }
        else if (strcmp(name, "Con") == 0)
        {
            _current->body.stats[A_CON] = b;
            _current->body.extra_stats[A_CON] = x;
        }
        else if (strcmp(name, "Chr") == 0)
        {
            _current->body.stats[A_CHR] = b;
            _current->body.extra_stats[A_CHR] = x;
        }
        else return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    return ERROR_SUCCESS;
}
/* P:Stats:<Str>:<Int>:<Wis>:<Dex>:<Con>:<Chr>
 * This version adjusts exsiting settings and requires all 6 stats
 * to be specified in assumed order. It is less readable, but occasionally
 * useful. */
static errr _pos_stats(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);

    if (num < 6) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    _current->body.stats[A_STR] += atoi(zz[0]);
    _current->body.stats[A_INT] += atoi(zz[1]);
    _current->body.stats[A_WIS] += atoi(zz[2]);
    _current->body.stats[A_DEX] += atoi(zz[3]);
    _current->body.stats[A_CON] += atoi(zz[4]);
    _current->body.stats[A_CHR] += atoi(zz[5]);

    return ERROR_SUCCESS;
}
/* P:Skill:Dis(30+15):Dev(36+10) ... sets vs adjusts */
static errr _pos_skill(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);
    int   i;

    for (i = 0; i < num; i++)
    {
        char *token = zz[i];
        char *name;
        char *args[10];
        int   arg_ct = parse_args(token, &name, args, 10);
        int   b = 0, x = 0;

        if (arg_ct != 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
        /* Thn(70+20) */
        if (sscanf(args[0], "%d+%d", &b, &x) == 2)
        {
        }
        /* Fos(15) */
        else
        {
            b = atoi(args[0]);
            x = 0;
        }
        

        if (strcmp(name, "Dis") == 0)
        {
            _current->body.skills.dis = b;
            _current->body.extra_skills.dis = x;
        }
        else if (strcmp(name, "Dev") == 0)
        {
            _current->body.skills.dev = b;
            _current->body.extra_skills.dev = x;
        }
        else if (strcmp(name, "Sav") == 0)
        {
            _current->body.skills.sav = b;
            _current->body.extra_skills.sav = x;
        }
        else if (strcmp(name, "Stl") == 0)
        {
            _current->body.skills.stl = b;
            _current->body.extra_skills.stl = x;
        }
        else if (strcmp(name, "Srh") == 0)
        {
            _current->body.skills.srh = b;
            _current->body.extra_skills.srh = x;
        }
        else if (strcmp(name, "Fos") == 0)
        {
            _current->body.skills.fos = b;
            _current->body.extra_skills.fos = x;
        }
        else if (strcmp(name, "Thn") == 0)
        {
            _current->body.skills.thn = b;
            _current->body.extra_skills.thn = x;
        }
        else if (strcmp(name, "Thb") == 0)
        {
            _current->body.skills.thb = b;
            _current->body.extra_skills.thb = x;
        }
        else return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    return ERROR_SUCCESS;
}
/* P:Skills:<Dis>:<Dev>:<Sav>:<Stl>:<Srh>:<Fos>:<Thn>:<Thb> */
static errr _pos_skills(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);

    if (num < 8) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    _current->body.skills.dis += atoi(zz[0]);
    _current->body.skills.dev += atoi(zz[1]);
    _current->body.skills.sav += atoi(zz[2]);
    _current->body.skills.stl += atoi(zz[3]);
    _current->body.skills.srh += atoi(zz[4]);
    _current->body.skills.fos += atoi(zz[5]);
    _current->body.skills.thn += atoi(zz[6]);
    _current->body.skills.thb += atoi(zz[7]);

    return ERROR_SUCCESS;
}
static errr _pos_extra_skills(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);

    if (num < 8) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    _current->body.extra_skills.dis += atoi(zz[0]);
    _current->body.extra_skills.dev += atoi(zz[1]);
    _current->body.extra_skills.sav += atoi(zz[2]);
    _current->body.extra_skills.stl += atoi(zz[3]);
    _current->body.extra_skills.srh += atoi(zz[4]);
    _current->body.extra_skills.fos += atoi(zz[5]);
    _current->body.extra_skills.thn += atoi(zz[6]);
    _current->body.extra_skills.thb += atoi(zz[7]);

    return ERROR_SUCCESS;
}
/* P:Class:Mage ... Sets the "pseudo-class-id" for this form, 
 * and implies the spell stat. This form is needed when not using
 * a P:Copy: directive. (P:Copy: works well for humans, but not
 * so well for hounds, dragons, and other general monsters). */
static errr _pos_class(char *buf)
{
    int id = plr_class_parse(buf);
    if (id != CLASS_NONE)
    {
        plr_class_ptr pc = plr_class_aux(id, 0);
        _current->body.class_id = id;
        if (pc->hooks.caster_info)
            _current->body.spell_stat = pc->hooks.caster_info()->which_stat;
        return ERROR_SUCCESS;
    }
    return PARSE_ERROR_OUT_OF_BOUNDS;
}
static errr _pos_speed(char *buf)
{
    int sp = atoi(buf);
    if (sp > 90) /* Hack: 110 -> +0, 100 -> -10, etc. This is old school.*/
        sp = sp - 110;
    _current->body.speed = sp;
    return ERROR_SUCCESS;
}
static errr _pos_spell_stat(char *buf)
{
    if (streq(buf, "Str")) _current->body.spell_stat = A_STR;
    else if (streq(buf, "Int")) _current->body.spell_stat = A_INT;
    else if (streq(buf, "Wis")) _current->body.spell_stat = A_WIS;
    else if (streq(buf, "Dex")) _current->body.spell_stat = A_DEX;
    else if (streq(buf, "Con")) _current->body.spell_stat = A_CON;
    else if (streq(buf, "Chr")) _current->body.spell_stat = A_CHR;
    else return PARSE_ERROR_OUT_OF_BOUNDS;

    return ERROR_SUCCESS;
}
static errr _pos_life(char *buf)
{
    int life;
    life = atoi(buf);
    if (!_current->body.life || _current->base_id)
        _current->body.life = life;
    else
        _current->body.life = _current->body.life * life / 100;
    return ERROR_SUCCESS;
}
static errr _pos_infra(char *buf)
{
    int n;
    char c;

    if (sscanf(buf, "%d%c", &n, &c) == 2 && c == '\'')
    {
        _current->body.infra = n/10;
        return ERROR_SUCCESS;
    }
    return PARSE_ERROR_GENERIC;
}
static errr _pos_body(char *buf)
{
    if (!equip_template_parse(buf))
        return PARSE_ERROR_OUT_OF_BOUNDS;
    _current->body.body_id = sym_add(buf);
    return ERROR_SUCCESS;
}
static errr _pos_blows(char *buf)
{
    char *zz[20];
    int   num = tokenize(buf, 20, zz, 0);

    if (num < 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    if (num > 3) return PARSE_ERROR_TOO_FEW_ARGUMENTS; /* s/FEW/MANY */

    _current->body.blows_calc.max = atoi(zz[0]);
    if (num >= 2)
        _current->body.blows_calc.wgt = atoi(zz[1]);
    if (num >= 3)
        _current->body.blows_calc.mul = atoi(zz[2]);

    return ERROR_SUCCESS;
}
static errr _pos_weight(char *buf)
{
    _current->weight = atoi(buf);
    return ERROR_SUCCESS;
}

static _parse_tbl_t _pos_tbl[] = {
    { "Weight", NULL, _pos_weight },
    { "Copy", NULL, _pos_copy },
    { "Stat", NULL, _pos_stat },
    { "Stats", NULL, _pos_stats },
    { "Skill", NULL, _pos_skill },
    { "Skills", NULL, _pos_skills },
    { "ExtraSkills", NULL, _pos_extra_skills },
    { "Class", NULL, _pos_class },
    { "Speed", NULL, _pos_speed },
    { "SpellStat", NULL, _pos_spell_stat },
    { "Life", NULL, _pos_life },
    { "Infra", NULL, _pos_infra },
    { "Body", NULL, _pos_body },
    { "Blows", NULL, _pos_blows },
    { 0 }
};
static errr _parse_possessor(char *buf)
{
    _parse_line_t pl = _parse_line(buf);

    if (!pl.type)
    {
        msg_format("Malformed possessor line: <color:r>%s</color>.", buf);
        return PARSE_ERROR_GENERIC;
    }
    return _dispatch(pl, _pos_tbl);
}

/************************************************************************
 * Top Level r_info Parser
 ************************************************************************/
/* N:<id>:<name>
 * N:<id>:<template>:<name> */
static errr _parse_name(char *buf)
{
    char *zz[10];
    int   num = tokenize(buf, 10, zz, TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);
    sym_t id;

    if (num != 2 && num != 3) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

    id = sym_add(zz[0]);
    if (int_map_find(_races, id)) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;

    if (num == 3)
    {
        sym_t base_id = sym_add(zz[1]);
        mon_race_ptr base = int_map_find(_races, base_id);
        if (!base)
        {
            msg_format("Unable to locate base_id of <color:r>%s</color>.", zz[1]);
            return PARSE_ERROR_GENERIC;
        }
        _current = mon_race_alloc_ex(id, base);
        _current->name = z_string_make(zz[2]);
    }
    else
    {
        _current = mon_race_alloc(id);
        _current->name = z_string_make(zz[1]);
    }
    int_map_add(_races, id, _current);
    return ERROR_SUCCESS;
}
static errr _parse_template(char *buf)
{
    int rc = _parse_name(buf);
    if (rc == ERROR_SUCCESS)
        _current->attributes |= RF_TEMPLATE;
    return rc;
}
static errr _parse_blow(char *buf)
{
    errr rc;
    mon_blow_ptr blow = mon_blow_alloc(RBM_NONE);

    rc = parse_mon_blow(buf, blow);
    if (rc == ERROR_SUCCESS)
        vec_add(_current->blows, blow);
    else
        mon_blow_free(blow);
    return rc;
}
static errr _parse_drop(char *buf)
{
    return mon_drop_parse(buf, _current, 0);
}
static errr _parse_spells(char *buf)
{
    if (!_current->spells)
        _current->spells = mon_spells_alloc();
    return parse_mon_spells(buf, _current);
}
static errr _parse_desc(char *buf)
{
    _current->text = z_string_append(_current->text, buf, ' ');
    return ERROR_SUCCESS;
}
/* M:25%:3d2+5:MON("D.power", HASTE) */
static errr _parse_friends(char *buf)
{
    mon_rule_ptr rule = mon_rule_alloc();
    errr rc = mon_rule_parse(rule, buf);
    if (rc)
    {
        mon_rule_free(rule);
        return rc;
    }
    if (!_current->friends)
        _current->friends = rule;
    else
    {
        mon_rule_ptr tail = _current->friends;
        while (tail->next) tail = tail->next;
        tail->next = rule;
    }
    return ERROR_SUCCESS;
}
/* K:50%:MON("P.Hera") */
static errr _parse_kin(char *buf)
{
    mon_rule_ptr rule = mon_rule_alloc();
    errr rc = mon_rule_parse(rule, buf);
    if (rc)
    {
        mon_rule_free(rule);
        return rc;
    }
    if (!_current->kin)
        _current->kin = rule;
    else
    {
        mon_rule_ptr tail = _current->kin;
        while (tail->next) tail = tail->next;
        tail->next = rule;
    }
    return ERROR_SUCCESS;
}
static errr _parse_auras(char *buf)
{
    return parse_mon_auras(buf, _current);
}
static errr _parse_alias(char *buf)
{
    char *tokens[20];
    int   token_ct = tokenize(buf, 20, tokens, TOKENIZE_NO_SLASH | TOKENIZE_NO_ESCAPE);
    int   i;

    for (i = 0; i < token_ct; i++)
    {
        char *token = tokens[i];
        sym_t id = sym_add(token);
        if (int_map_find(_aliases, id)) return PARSE_ERROR_NON_SEQUENTIAL_RECORDS;
        int_map_add(_aliases, id, _current);
    }
    return ERROR_SUCCESS;
}

/* top level line based parser */
static _parse_tbl_t _parse_tbl[] = {
    { "N", "Name", _parse_name },
    { "T", "Template", _parse_template },
    { "I", "Info", _parse_info },
    { "B", "Blow", _parse_blow },
    { "P", "Pos", _parse_possessor },
    { "O", "Drop", _parse_drop },
    { "S", "Spells", _parse_spells },
    { "D", "Desc", _parse_desc },
    { "M", "Friends", _parse_friends },
    { "K", "Kin", _parse_kin },
    { "A", "Auras", _parse_auras },
    { "X", "Alias", _parse_alias },
    { 0 }
};

static errr _parse_mon_race(char *line, int options)
{
    _parse_line_t pl = _parse_line(line);

    if (!pl.type)
    {
        msg_format("Malformed mon race line: <color:r>%s</color>.", line);
        return PARSE_ERROR_GENERIC;
    }
    return _dispatch(pl, _parse_tbl);
}

/************************************************************************
 * Initialize and Verify r_info
 ************************************************************************/
static void _verify_mon_rule(mon_race_ptr race, mon_rule_ptr rule)
{
    while (rule)
    {
        if (!(rule->flags & (MON_RULE_CHAR | MON_RULE_TYPE | MON_RULE_RANDOM)))
        {
            if (!mon_race_lookup(rule->which))
            {
                msg_boundary();
                msg_format("<color:v>Error</color>: <color:U>%s</color> has a friend or kin reference to non-existant <color:R>%s</color>.",
                    sym_str(race->id), sym_str(rule->which));
                msg_print(NULL);
                quit("Unable to parse r_info.txt");
            }
        }
        rule = rule->next;
    }
}
static void _verify_mon_race(int id, mon_race_ptr race)
{
    assert(race->id);
    assert(race->id == id);

    /* display char is required (and sometimes accidentally omitted) */
    if (!race->display.c)
    {
        msg_boundary();
        msg_format("<color:v>Error</color>: <color:U>%s</color> is missing a display char.", sym_str(race->id));
        msg_print(NULL);
        quit("Unable to parse r_info.txt");
    }

    /* fix up gf flags: immune > vuln > resist */
    race->resist &= ~race->immune;
    race->resist &= ~race->vuln;
    race->vuln &= ~race->immune;

    /* some flags imply others ... XXX */
    if (race->kind & (RFK_UNDEAD | RFK_DEMON))
        race->kind |= RFK_NONLIVING;
    if (race->kind & RFK_AQUATIC)
        race->move.flags |= RFM_SWIM;

    if (race->attributes & RF_TEMPLATE) return;

    if (race->body.class_id == CLASS_NONE && !(race->attributes & RF_DEPRECATED))
    {
        msg_boundary();
        msg_format("<color:v>Error</color>: <color:U>%s</color> is missing a class_id.", sym_str(race->id));
        msg_print(NULL);
        quit("Unable to parse r_info.txt");
    }
    if (mon_race_has_noninnate_spell(race) && race->body.spell_stat == A_NONE)
    {
        msg_boundary();
        msg_format("<color:v>Error</color>: <color:U>%s</color> is missing a spell_stat.", sym_str(race->id));
        msg_print(NULL);
        quit("Unable to parse r_info.txt");
    }

    if (race->alloc.flags & RFA_UNIQUE)
    {
        race->alloc.max_max_num = 1;
        if (race->hp.dd)
        {
            msg_boundary();
            msg_format("<color:v>Error</color>: <color:U>%s</color> should have FORCE_MAX_HP.", sym_str(race->id));
            msg_print(NULL);
            quit("Unable to parse r_info.txt");
        }
    }
    race->alloc.max_num = race->alloc.max_max_num; /* cf _mon_race_load */

    /* verify EVOLVE(<id>) */
    if (race->evolution.id)
    {
        assert(race->id != race->evolution.id);
        if (!mon_race_lookup(race->evolution.id))
        {
            msg_boundary();
            msg_format("<color:v>Error</color>: <color:U>%s</color> is set to evolve to non-existant <color:R>%s</color>.",
                sym_str(race->id), sym_str(race->evolution.id));
            msg_print(NULL);
            quit("Unable to parse r_info.txt");
        }
    }
    _verify_mon_rule(race, race->friends);
    _verify_mon_rule(race, race->kin);
}
static void _fuzzy_init(cptr display_chars)
{
    char buf[50];
    cptr pc = display_chars;
    for (; *pc; pc++)
    {
        sprintf(buf, "%c.fuzzy", *pc);
        visual_set_ascii(buf, term_char_create(*pc, TERM_WHITE), 0);
    }
}
bool mon_race_init(void)
{
    assert(!_races);
    _races = int_map_alloc((int_map_free_f)mon_race_free);
    _aliases = int_map_alloc(NULL);
    _current = NULL;
    if (parse_edit_file("r_info.txt", _parse_mon_race, 0)) /* errr -> bool */
        return FALSE;
    _current = NULL;
    int_map_iter(_races, (int_map_iter_f)_verify_mon_race);
    _fuzzy_init("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ");
    return TRUE;
}

/************************************************************************
 * Reset r_info for a new game (player_wipe)
 ************************************************************************/
static void _mon_race_reset(int id, mon_race_ptr race)
{
    race->alloc.cur_num = 0;
    race->alloc.max_num = race->alloc.max_max_num;
    race->flagsx = 0;

    race->lore.kills.current = 0;
    race->stolen_ct = 0;

    race->lore.flags &= ~RFL_PACT;
}
void mon_race_reset(void)
{
    assert(_races);
    int_map_iter(_races, (int_map_iter_f)_mon_race_reset);
}

/************************************************************************
 * Savefile Support for Monster Recall
 ************************************************************************/
static savefile_ptr _file;
static bool _has_lore(mon_race_ptr race)
{
    return race->lore.sightings || race->lore.kills.total; /* XXX */
}
#define _RACE_SAVE_FLAGSX  0x01
#define _RACE_SAVE_LORE    0x02
#define _RACE_SAVE_TURN    0x04
#define _RACE_SAVE_MAX_NUM 0x08
#define _RACE_SAVE_STOLEN  0x10
static void _mon_race_save(mon_race_ptr race)
{
    byte header = 0;

    if (race->alloc.max_num != race->alloc.max_max_num) header |= _RACE_SAVE_MAX_NUM;
    if (race->flagsx) header |= _RACE_SAVE_FLAGSX;
    if (race->stolen_ct) header |= _RACE_SAVE_STOLEN;
    if (_has_lore(race)) header |= _RACE_SAVE_LORE;

    if (!header) return;

    savefile_write_sym(_file, race->id);
    savefile_write_byte(_file, header);
    if (race->alloc.max_num != race->alloc.max_max_num)
        savefile_write_byte(_file, race->alloc.max_num);
    if (race->flagsx)
        savefile_write_byte(_file, race->flagsx);
    if (race->stolen_ct)
        savefile_write_byte(_file, race->stolen_ct);
    if (_has_lore(race))
    {
        int i, j;
        mon_aura_ptr aura;
        savefile_write_u32b(_file, race->lore.flags);
        savefile_write_u32b(_file, race->lore.move);
        savefile_write_u32b(_file, race->lore.kind);
        savefile_write_u32b(_file, race->lore.abilities);
        savefile_write_u32b(_file, race->lore.attributes);
        savefile_write_u32b(_file, race->lore.resist);
        savefile_write_u16b(_file, race->lore.sightings);
        savefile_write_u16b(_file, race->lore.deaths);
        savefile_write_u16b(_file, race->lore.kills.total);
        savefile_write_u16b(_file, race->lore.kills.current);
        savefile_write_u32b(_file, race->lore.turns.total);
        savefile_write_u32b(_file, race->lore.turns.spell);
        savefile_write_u16b(_file, race->lore.turns.wake);
        savefile_write_u16b(_file, race->lore.turns.sleep);
        savefile_write_byte(_file, race->lore.drops.gold);
        savefile_write_byte(_file, race->lore.drops.obj);

        mon_spells_save(race->spells, _file); /* 2 + 5S' bytes where S' is a seen spell */
        savefile_write_byte(_file, vec_length(race->blows));
        for (i = 0; i < vec_length(race->blows); i++)
        {
            mon_blow_ptr blow = vec_get(race->blows, i);
            savefile_write_s16b(_file, blow->lore);
            savefile_write_byte(_file, blow->effect_ct);
            for (j = 0; j < blow->effect_ct; j++)
            {
                mon_effect_ptr effect = &blow->effects[j];
                savefile_write_u16b(_file, effect->lore);
            }
        }
        for (aura = race->auras; aura; aura = aura->next)
        {
            if (aura->lore)
            {
                savefile_write_s16b(_file, aura->gf);
                savefile_write_u16b(_file, aura->lore);
            }
        }
        savefile_write_s16b(_file, 0);
    } 
}
void mon_race_save(savefile_ptr file)
{
    _file = file;
    mon_race_iter(_mon_race_save);
    savefile_write_sym(file, 0); /* sentinel */
}
static void _mon_race_load(mon_race_ptr race, savefile_ptr file)
{
    byte header = savefile_read_byte(file);

    if (header & _RACE_SAVE_MAX_NUM)
    {
        race->alloc.max_num = savefile_read_byte(file);
        /* XXX handle r_info.txt changes, including removing restriction */
        if (race->alloc.max_num > race->alloc.max_max_num)
            race->alloc.max_num = race->alloc.max_max_num;
    }
    if (header & _RACE_SAVE_FLAGSX)
        race->flagsx = savefile_read_byte(file);
    if (header & _RACE_SAVE_STOLEN)
        race->stolen_ct = savefile_read_byte(file);
    if (header & _RACE_SAVE_LORE)
    {
        int  i, j, ct_blows, ct_effects;

        race->lore.flags = savefile_read_u32b(file);
        race->lore.move = savefile_read_u32b(file);
        race->lore.kind = savefile_read_u32b(file);
        race->lore.abilities = savefile_read_u32b(file);
        race->lore.attributes = savefile_read_u32b(file);
        race->lore.resist = savefile_read_u32b(file);
        race->lore.sightings = savefile_read_u16b(file);
        race->lore.deaths = savefile_read_u16b(file);
        race->lore.kills.total = savefile_read_u16b(file);
        race->lore.kills.current = savefile_read_u16b(file);
        race->lore.turns.total = savefile_read_u32b(file);
        race->lore.turns.spell = savefile_read_u32b(file);
        race->lore.turns.wake = savefile_read_u16b(file);
        race->lore.turns.sleep = savefile_read_u16b(file);
        race->lore.drops.gold = savefile_read_byte(file);
        race->lore.drops.obj = savefile_read_byte(file);

        mon_spells_load(race->spells, file);
        ct_blows = savefile_read_byte(file);
        for (i = 0; i < ct_blows; i++)
        {
            mon_blow_ptr blow;
            if (i >= vec_length(race->blows))
            {
                savefile_read_s16b(file);
                ct_effects = savefile_read_byte(file);
                for (j = 0; j < ct_effects; j++)
                    savefile_read_s16b(file);
                continue;
            }
            blow = vec_get(race->blows, i);
            blow->lore = savefile_read_s16b(file);
            ct_effects = savefile_read_byte(file);
            for (j = 0; j < ct_effects; j++)
            {
                if (j >= blow->effect_ct)
                    savefile_read_u16b(file);
                else
                    blow->effects[j].lore = savefile_read_u16b(file);
            }
        }
        for (;;)
        {
            int gf = savefile_read_s16b(file);
            u16b lore;
            mon_aura_ptr aura;
            if (!gf) break;
            lore = savefile_read_u16b(file);
            aura = mon_auras_find(race, gf);
            if (aura)
                aura->lore = lore;
        }

        /* repair flags (in case r_info changed) */
        race->lore.move &= race->move.flags;
        race->lore.kind &= race->kind;
        race->lore.abilities &= race->abilities;
        race->lore.attributes &= race->attributes;
        race->lore.resist &= (race->resist | race->immune | race->vuln);
    }
}
void mon_race_load(savefile_ptr file)
{
    for (;;)
    {
        sym_t id = savefile_read_sym(file);
        mon_race_ptr race;
        if (!id) break;
        race = mon_race_lookup(id);
        if (!race)
            quit(format("Monster Race (%d) out of range!", id));
        _mon_race_load(race, file);
    }
}

/************************************************************************
 * Monster Allocation Table
 *
 * Monsters are chosen from an allocation table (vec<mon_race_ptr>) by
 * applying a small number of user defined filters.
 * The table to use is chosen as follows:
 *   [1] Pass a custom table
 *   [2] Use table for current level if any (cave->mon_alloc_tbl)
 *   [3] Use table for current dungeon type (dun_type()->mon_alloc_tbl)
 *   [4] Use table for current world (dun_mgr()->world->mon_alloc_tbl)
 *   [5] Use global allocation table (mon_alloc_tbl defined here).
 * This allows for themed levels (set cave->mon_alloc_tbl) as well
 * as themed dungeons, or worlds. Use vec_filter to filter the global
 * table as needed. The global table is sorted by depth, and vec_filter
 * will maintain the sort order, but if you do something crazy, be sure
 * to sort your table before using it.
 ************************************************************************/
vec_ptr mon_alloc_tbl;
#define _MAX_FILTERS 10
static mon_race_p _filters[_MAX_FILTERS];
static int _filter_ct = 0;

#define _MAX_WEIGHTS 5
static mon_alloc_weight_f _weights[_MAX_WEIGHTS];
static int _weight_ct = 0;

vec_ptr mon_alloc_current_tbl(void)
{
    if (cave->mon_alloc_tbl) return cave->mon_alloc_tbl;
    if (cave->type->mon_alloc_tbl) return cave->type->mon_alloc_tbl;
    if (dun_mgr()->world->mon_alloc_tbl) return dun_mgr()->world->mon_alloc_tbl;
    return mon_alloc_tbl;
}
static bool _mon_alloc_filter(mon_race_ptr r)
{
    /* XXX cf p.Banor and p.Rupart ... some code is iterating mon_alloc_tbl rather
     * than _races for convenience and we want these forms to show up for monster
     * recall. cf _mon_alloc_prob XXX
    if (r->rarity == 0) return FALSE;
    if (r->rarity > 100) return FALSE;*/
    if (r->attributes & RF_DEPRECATED) return FALSE;
    /* XXX Templates could be deleted once parsing is complete, but they are useful
     * for debugging (e.g `p *mon_race_parse("p.base")` in gdb) */
    if (r->attributes & RF_TEMPLATE) return FALSE;
    return TRUE;
}
static int _mon_alloc_cmp(mon_race_ptr left, mon_race_ptr right)
{
    if (left->alloc.lvl < right->alloc.lvl) return -1;
    if (left->alloc.lvl > right->alloc.lvl) return 1;
    return 0;
}
void mon_alloc_init(void)
{
    if (mon_alloc_tbl) vec_free(mon_alloc_tbl); /* allow re-initialization */
    mon_alloc_tbl = mon_race_filter(_mon_alloc_filter);
    vec_sort(mon_alloc_tbl, (vec_cmp_f)_mon_alloc_cmp);
}
void mon_alloc_clear_filters(void)
{
    int i;
    for (i = 0; i < _MAX_FILTERS; i++)
        _filters[i] = NULL;
    _filter_ct = 0;
}
void mon_alloc_push_filter(mon_race_p filter)
{
    assert(_filter_ct < _MAX_FILTERS);
    assert(filter);
    if (_filter_ct < _MAX_FILTERS)
        _filters[_filter_ct++] = filter;
}
void mon_alloc_pop_filter(void)
{
    assert(_filter_ct > 0);
    if (_filter_ct > 0) _filter_ct--;
}
void mon_alloc_push_weight(mon_alloc_weight_f weight)
{
    assert(_weight_ct < _MAX_WEIGHTS);
    assert(weight);
    if (_weight_ct < _MAX_WEIGHTS)
        _weights[_weight_ct++] = weight;
}
void mon_alloc_pop_weight(void)
{
    assert(_weight_ct > 0);
    if (_weight_ct > 0) _weight_ct--;
}
mon_race_ptr mon_alloc_choose(int level)
{
    return mon_alloc_choose_aux(level, GMN_DEFAULT);
}
mon_race_ptr mon_alloc_choose_aux(int level, u32b options)
{
    return mon_alloc_choose_aux2(mon_alloc_current_tbl(), level, 0, options);
}
static int _mon_alloc_prob(mon_race_ptr race, int level, int min_level, u32b options)
{
    int i, prob;

    if (!race->alloc.rarity || race->alloc.rarity > 100) return 0;
    if (race->alloc.max_lvl && !(options & GMN_IGNORE_MAX_LEVEL) && race->alloc.max_lvl < level) return 0;
    if (cave->type->id != D_SURFACE && !race->alloc.lvl && race->alloc.lvl < level) return 0;
    if (race->alloc.lvl < min_level) return 0;
    if (quests_get_current() && (race->alloc.flags & RFA_NO_QUEST)) return 0;
    if (!who_is_null(summon_specific_who) && (race->alloc.flags & RFA_NO_SUMMON)) return 0;
    if (race->alloc.dun_type_id)
    {
        #if 0
        if ((options & GMN_QUESTOR) && race->dun_type_id == D_AMBER) { /* XXX */ }
        else if (race->dun_type_id != cave->dun_type_id) return 0;
        #else
        if (race->alloc.dun_type_id != cave->type->id) return 0;
        #endif
    }
    if (race->alloc.world_id && race->alloc.world_id != plr->world_id) return 0;
    if (!(options & GMN_ALLOW_DEAD_UNIQUES)) /* Chameleon Lord */
    {
        if (race->alloc.max_max_num && race->alloc.cur_num >= race->alloc.max_num) return 0;
        if ((race->alloc.flags & RFA_UNIQUE2) && race->alloc.cur_num) return 0; 
    }
    if (mon_race_is_unique(race))
    {
        if (options & GMN_NO_UNIQUES) return 0;
        if (mon_race_is_(race, "p.Banor=Rupart"))
        {
            if (mon_race_parse("p.Banor")->alloc.cur_num) return 0;
            if (mon_race_parse("p.Rupart")->alloc.cur_num) return 0;
        }
    }
    if (!chameleon_change_mon && summon_specific_type != SUMMON_GUARDIAN)
    {
        if (race->flagsx & (RFX_QUESTOR | RFX_SUPPRESS | RFX_GUARDIAN)) return 0;
        if (race->alloc.flags & RFA_GUARDIAN) return 0;
        if ((race->alloc.flags & RFA_FORCE_DEPTH) && race->alloc.lvl > cave->dun_lvl) return 0;
    }

    for (i = 0; i < _filter_ct; i++)
        if (!_filters[i](race)) return 0;

    prob = 100/race->alloc.rarity;

    /* Hack: Undersized monsters become more rare ... but only for max_depth restricted monsters.
       The goal is that these monsters gradually become less and less common, rather than suddenly
       disappearing. About 50% of monsters currently have depth restrictions. */
    if ( race->alloc.max_lvl /* <=== Remove this, and the end game becomes too difficult */
      && level > race->alloc.lvl + 9
      && !mon_race_is_unique(race) ) /* Redundant. Uniques never have depth restrictions. */
    {
        int delta = level - race->alloc.lvl;
        prob = prob >> (delta/10);
        if (!prob) prob = 1;
    }

    for (i = 0; i < _weight_ct; i++)
        prob = _weights[i](race, prob);

    if (cave->type->mon_alloc_f)
        prob = cave->type->mon_alloc_f(cave->type, race, prob);

    /* Hack: Try to allocate uniques close to their proper level. */
    if (prob && mon_race_is_fixed_unique(race) && level > race->alloc.lvl + 5)
    {
        int delta = level - race->alloc.lvl;
        prob = prob << MIN(10, delta/5); /* XXX aggressive! */
    }

    return prob;
}
mon_race_ptr mon_alloc_choose_aux2(vec_ptr tbl, int level, int min_level, u32b options)
{
    int i, roll, total = 0;
    int rolls = (options & GMN_POWER_BOOST) ? 3 : 1;
    mon_race_ptr best = NULL;

    /* Boost level if allowed */
    if ((options & GMN_ALLOW_OOD) && level > 0)
    {
        for (i = 0; i < 2; i++)
        {
            if (one_in_(NASTY_MON))
                level += MIN(5, level/10);
        }
    }
    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;

    /* Pass 1: Count up total probability */
    for (i = 0; i < vec_length(tbl); i++)
    {
        mon_race_ptr race = vec_get(tbl, i);
        if (race->alloc.lvl > level) break;  /* assume tbl is sorted */
        race->prob = _mon_alloc_prob(race, level, min_level, options);
        total += race->prob;
    }
    if (options & GMN_DEBUG)
    {
        doc_ptr doc = doc_alloc(80);
        doc_printf(doc, "<color:G>Monster Allocation Table for <color:R>L%d</color></color>\n", level);
        for (i = 0; i < vec_length(tbl); i++)
        {
            mon_race_ptr race = vec_get(tbl, i);
            if (race->alloc.lvl > level) break;  /* assume tbl is sorted */
            if (!race->prob) continue;
            doc_printf(doc, "%5d %3d.%d%% ML%2d %s %d of %d\n", race->prob,
                race->prob*100/total, (race->prob*1000/total)%10,
                race->alloc.lvl, race->name, race->alloc.cur_num, race->alloc.max_num);
        }
        doc_display(doc, "mon_alloc", 0);
        doc_free(doc);
    }
    if (total <= 0) return NULL;
    /* Pass 2: Pick best of N */
    if (who_is_null(summon_specific_who) && !(options & GMN_QUESTOR))
    {
        while (rolls < 5 && randint0(20000) < level*level)
            rolls++;
    }
    assert(rolls > 0);
    for (roll = 0; roll < rolls; roll++)
    {
        mon_race_ptr race = NULL;
        int pick = randint0(total);
        for (i = 0; i < vec_length(tbl); i++)
        {
            race = vec_get(tbl, i);
            assert(race->alloc.lvl <= level);
            if (race->alloc.lvl > level) break;  /* assume tbl is sorted */
            pick -= race->prob;
            if (pick < 0) break;
        }
        assert(race != NULL);
        if (!best || race->alloc.lvl > best->alloc.lvl) best = race;
    }
    assert(best);
    return best;
}

/* D_SURFACE allocation */
bool mon_alloc_town(mon_race_ptr race)
    { return BOOL(race->alloc.flags & (RFA_WILD_TOWN)); }
bool mon_alloc_ocean(mon_race_ptr race)
    { return BOOL(race->alloc.flags & RFA_WILD_OCEAN); }
bool mon_alloc_shore(mon_race_ptr race)
    { return BOOL(race->alloc.flags & RFA_WILD_SHORE); }
bool mon_alloc_waste(mon_race_ptr race)
    { return BOOL(race->alloc.flags & (RFA_WILD_WASTE)); }
bool mon_alloc_woods(mon_race_ptr race)
    { return BOOL(race->alloc.flags & (RFA_WILD_WOOD)); }
bool mon_alloc_mountain(mon_race_ptr race)
    { return BOOL(race->alloc.flags & RFA_WILD_MOUNTAIN); }
bool mon_alloc_grass(mon_race_ptr race)
    { return BOOL(race->alloc.flags & (RFA_WILD_GRASS)); }
bool mon_alloc_volcano(mon_race_ptr race)
    { return BOOL(race->alloc.flags & RFA_WILD_VOLCANO); }
bool mon_alloc_surface(mon_race_ptr race)
    { return BOOL(race->alloc.flags); }

/* dungeon allocation */
bool mon_alloc_dungeon(mon_race_ptr race)
{ 
    if (cave->type->id == D_SURFACE) return TRUE; /* XXX ignore this filter if on surface */

    /* Allow Gwaihir, Thorondor and Meneldor to appear in "The Mountain" */
    if (cave->type->id == D_MOUNTAIN && mon_race_is_char(race, 'B') && (race->alloc.flags & RFA_WILD_MOUNTAIN))
        return TRUE;
    if (cave->type->id == D_RANDOM_MOUNTAIN && mon_race_is_char(race, 'B') && (race->alloc.flags & RFA_WILD_MOUNTAIN))
        return TRUE;
    if (cave->type->id == D_RANDOM_SEA && (race->alloc.flags & RFA_WILD_OCEAN))
        return TRUE;
    if (cave->type->id == D_RANDOM_VOLCANO && (race->alloc.flags & RFA_WILD_VOLCANO))
        return TRUE;
    return !(race->alloc.flags & RFA_WILD_ONLY); 
}
bool mon_alloc_deep_water(mon_race_ptr race)
{ 
    if (!mon_alloc_dungeon(race)) return FALSE;
    return mon_race_is_aquatic(race);
}
bool mon_alloc_shallow_water(mon_race_ptr race)
{
    if (!mon_alloc_dungeon(race)) return FALSE;
    return !mon_auras_find(race, GF_FIRE);
}
bool mon_alloc_floor(mon_race_ptr race)
{ 
    if (!mon_alloc_dungeon(race)) return FALSE;
    return !mon_race_is_aquatic(race) || mon_race_can_fly(race);
}
bool mon_alloc_lava(mon_race_ptr race)
{
    if (!mon_alloc_dungeon(race)) return FALSE;
    if (mon_race_immune(race, GF_FIRE)) return TRUE;
    return mon_race_can_fly(race) && !mon_auras_find(race, GF_COLD);
}

mon_race_p mon_alloc_cell_p(dun_cell_ptr cell)
{
    if (water_is_deep(cell)) return mon_alloc_deep_water;
    if (water_is_shallow(cell)) return mon_alloc_shallow_water;
    if (cell_is_lava(cell)) return mon_alloc_lava;
    return mon_alloc_floor;
}

