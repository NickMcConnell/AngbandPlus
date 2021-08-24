#include "angband.h"

#include <assert.h>

#define CELL_CHANGE_MASK ~(CELL_PERM | CELL_LIT | CELL_LIGHT | CELL_DARK | CELL_LOS | CELL_PROJECT)

/************************************************************************
 * 'Features': There is a dun_cell_type object for every FEAT_FOO code.
 * Function pointers (methods) implement behavior for each class of features.
 ************************************************************************/
/* private: */
struct dun_cell_type_s
{
    int id; /* e.g. FEAT_WALL */

    /* init_angband */
    void (*init)(void); /* setup default ascii visuals before loading pref files */

    /* display */
    cptr (*name)(dun_cell_ptr cell); /* internal name (e.g. WALL_GRANITE) also used for visuals */
    cptr (*desc)(dun_cell_ptr cell); /* display to user (e.g. "granite wall") */
    int  (*priority)(dun_cell_ptr cell); /* for display_map compression */
    void (*display)(dun_cell_ptr cell, int light, map_char_ptr mc);
    int  (*light)(dun_cell_ptr cell);
    /* usually omitted since type->name can be used to lookup the visual (cf cell_display)
     * however, floors layer multiple tiles when trapped, webbed or glyphed. cf _floor_display */

    /* magical detection */
    bool (*detect)(dun_ptr dun, point_t pos, dun_cell_ptr cell);

    /* plr actions: these may 'alter' the feature (e.g. search might WALL->DOOR) */
    int  (*open)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);
    int  (*close)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);
    int  (*bash)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);
    int  (*jam)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);

    int  (*disarm)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);
    int  (*search)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);
    int  (*tunnel)(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options);

    /* mon actions */
    int  (*open_mon)(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon);
    int  (*bash_mon)(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon);
    int  (*tunnel_mon)(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon);

    /* gf_affect_f */
    bool (*affect)(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power);

    /* placement: the allow versions are called to see if the given entity
     * may legally be placed. The accept versions are used for side-effects,
     * such as triggering a trap. Always use cell_allow_mon|plr|mon_race
     * rather than directly using hooks. */
    bool (*allow_obj)(dun_cell_ptr cell);
    bool (*allow_mon)(dun_cell_ptr cell, mon_ptr mon);
    bool (*allow_mon_race)(dun_cell_ptr cell, mon_race_ptr race);
    bool (*allow_plr)(dun_cell_ptr cell);

    void (*accept_obj)(dun_ptr dun, point_t pos, dun_cell_ptr cell, obj_ptr obj);
    void (*accept_mon)(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon);
    void (*accept_plr)(dun_ptr dun, point_t pos, dun_cell_ptr cell);

    /* upkeep (e.g. lava burns monsters and plr) */
    void (*process_plr)(dun_ptr dun, point_t pos, dun_cell_ptr cell);
};
typedef struct dun_cell_type_s dun_cell_type_t, *dun_cell_type_ptr;

static dun_cell_type_ptr _door(void);
static dun_cell_type_ptr _floor(void);
static dun_cell_type_ptr _wall(void);
static dun_cell_type_ptr _stairs(void);
static dun_cell_type_ptr _tree(void);
static dun_cell_type_ptr _water(void);
static dun_cell_type_ptr _lava(void);
static dun_cell_type_ptr _chasm(void);
static dun_cell_type_ptr _portal(void);
static dun_cell_type_ptr _bldg(void);
static dun_cell_type_ptr _pattern(void);

static void _register(int_map_ptr map, dun_cell_type_ptr type)
{
    assert(!int_map_find(map, type->id));
    int_map_add(map, type->id, type);
}
static int_map_ptr _types(void)
{
    static int_map_ptr _map = NULL;
    if (!_map)
    {
        _map = int_map_alloc(free);
        _register(_map, _door());
        _register(_map, _floor());
        _register(_map, _wall());
        _register(_map, _stairs());
        _register(_map, _tree());
        _register(_map, _water());
        _register(_map, _lava());
        _register(_map, _chasm());
        _register(_map, _portal());
        _register(_map, _bldg());
        _register(_map, _pattern());
    }
    return _map;
}
static dun_cell_type_ptr dun_cell_type(int id)
{
    return int_map_find(_types(), id);
}

/************************************************************************
 * Display
 ************************************************************************/
typedef struct {
    cptr name;
    cptr desc;
    term_char_t display;
} _info_t, *_info_ptr;

static void _display_add(cptr name, term_char_t tc)
{
    visual_set_ascii(name, tc, 1);
}
static void _display_add_no_light(cptr name, term_char_t tc)
{
    visual_set_ascii(name, tc, 0);
}
static void _type_init(int id, dun_cell_type_ptr type)
{
    if (type->init)
        type->init();
}
bool dun_feat_init(void)
{
    _display_add_no_light("UNSAFE", term_char_create('x', TERM_L_DARK));
    _display_add_no_light("TOWN", term_char_create('*', TERM_WHITE));
    _display_add_no_light("HILITE", term_char_create(' ', TERM_YELLOW));
    int_map_iter(_types(), (int_map_iter_f)_type_init);
    return TRUE;
}
/************************************************************************
 * Helpers
 ************************************************************************/
static dun_cell_type_ptr dun_cell_type_alloc(int id)
{
    dun_cell_type_ptr type = malloc(sizeof(dun_cell_type_t));
    memset(type, 0, sizeof(dun_cell_type_t));
    type->id = id;
    return type;
}

/************************************************************************
 * General Predicates
 ************************************************************************/
bool cell_stop_disintegrate(dun_cell_ptr cell)
{
    if (cell->flags & CELL_PROJECT) return FALSE;
    if (cell->flags & CELL_PERM) return TRUE;
    /* Doors, Walls and Trees have implicit FF_HURT_DISI */
    if (cell->type == FEAT_DOOR) return FALSE;
    if (cell->type == FEAT_WALL) return FALSE;
    if (cell->type == FEAT_TREE) return FALSE;
    /* Everything else should be marked as PERM ... this is paranoia */
    return TRUE;
}
bool cell_los(dun_cell_ptr cell) { return BOOL(cell->flags & CELL_LOS); }
bool cell_project(dun_cell_ptr cell) { return BOOL(cell->flags & CELL_PROJECT); }
cptr cell_name(dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (!type->name) return "UNKNOWN";
    return type->name(cell);
}
cptr cell_desc(dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (!type->desc) return "something strange";
    return type->desc(cell);
}
bool cell_affect(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power)
{
    bool notice = FALSE;
    bool seen = plr_can_see(pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    switch (gf)
    {
    /* the following are more easily handled here, since strong "flows"
     * "wash out" all non-permanent terrain */
    case GF_LAVA_FLOW:
        if (cell->flags & CELL_PERM) break;
        if (power == 1 && cell_is_floor(cell))
        {
            dun_place_shallow_lava(dun, pos);
            if (seen) notice = TRUE;
        }
        if (power > 1)
        {
            dun_place_deep_lava(dun, pos);
            if (seen) notice = TRUE;
        }
        break;
    case GF_WATER2: /* _water_ball_spell: indexes wash out effectiveness to damage */
        if (cell->flags & CELL_PERM) break;
        if (_1d(50) >= plr->lev) break;
        if (_1d(1000) > power)
        {
            if (cell_is_floor(cell))
            {
                dun_place_shallow_water(dun, pos);
                if (seen) notice = TRUE;
            }
        }
        else
        {
            dun_place_deep_water(dun, pos);
            if (seen) notice = TRUE;
        }
        break;
    case GF_WATER_FLOW:
        if (cell->flags & CELL_PERM) break;
        if (power == 1 && cell_is_floor(cell))
        {
            dun_place_shallow_water(dun, pos);
            if (seen) notice = TRUE;
        }
        if (power > 1)
        {
            dun_place_deep_water(dun, pos);
            if (seen) notice = TRUE;
        }
        break;

    /* light and dark will affect CELL_LIT for all terrain types, provided
     * a dun_blast can reach them. generally, floors, but also hits doors
     * and boundary walls. easier to handle once at top level: */
    case GF_LIGHT_WEAK:
    case GF_LIGHT:
        if (cell->flags & CELL_DARK) break; /* 'dark' tiles resist illumination */
        if (!(cell->flags & CELL_LIT))
        {
            cell->flags |= CELL_LIT;
            plr->update |= PU_LIGHT;
            if (plr_view(pos)) notice = TRUE; /* seen is out of date until PU_LIGHT is processed */
        }
        break;
    case GF_DARK_WEAK:
    case GF_DARK:
        if (cell->flags & CELL_LIGHT) break; /* 'light' tiles maintain illumination (e.g. deep lava) */
        if (cell->flags & CELL_LIT)
        {
            cell->flags &= ~CELL_LIT;
            plr->update |= PU_LIGHT;
            if (seen) notice = TRUE;
        }
        break;

    /* dispatch everything else to cell->type */
    default:
        if (type->affect)
            notice = type->affect(dun, pos, cell, gf, power);
    }
    return notice;
}
void cell_process_plr(void)
{
    dun_ptr dun = plr_dun();
    dun_cell_ptr cell = dun_cell_at(dun, plr->pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (type->process_plr)
        type->process_plr(dun, plr->pos, cell);
}
void cell_detect(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (type->detect)
        type->detect(dun, pos, cell);
}
bool cell_allow_obj(dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (!type->allow_obj) return FALSE;
    return type->allow_obj(cell);
}
bool cell_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race)
{
    dun_cell_type_ptr type;

    if (cell->type != FEAT_WATER && mon_race_is_aquatic(race) && !mon_race_can_fly(race))
        return FALSE;

    type = dun_cell_type(cell->type);
    if (!type->allow_mon_race) return FALSE;
    return type->allow_mon_race(cell, race);
}
bool cell_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    dun_cell_type_ptr type;

    if (!mon) return cell_place(cell);

    if (cell->type != FEAT_WATER && mon_is_aquatic(mon) && !mon_can_fly(mon))
        return FALSE;

    type = dun_cell_type(cell->type);
    if (!type->allow_mon) return FALSE;
    return type->allow_mon(cell, mon);
}
bool cell_allow_plr(dun_cell_ptr cell)
{
    dun_cell_type_ptr type;

    if (plr->riding)
        return cell_allow_mon(cell, plr_riding_mon());

    type = dun_cell_type(cell->type);
    if (!type->allow_plr) return FALSE;
    return type->allow_plr(cell);
}
void cell_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    assert(dun_plr_at(dun, pos));
    if (plr->riding) /* e.g. apply mon_can_tunnel ... */
    {
        mon_ptr mount = plr_riding_mon();
        if (type->accept_mon)
        {
            type->accept_mon(dun, pos, cell, mount);
            type = dun_cell_type(cell->type); /* altered? */
        }
    }
    if (type->accept_plr)
        type->accept_plr(dun, pos, cell);
}
void cell_accept_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    assert(point_equals(mon->pos, pos));
    assert(dun_cell_at(dun, pos) == cell);
    assert(dun_mon_at(dun, pos) == mon);
    if (type->accept_mon)
        type->accept_mon(dun, pos, cell, mon);
}
void cell_display(dun_cell_ptr cell, int light, map_char_ptr mc)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (type->display)
        type->display(cell, light, mc);
    else if (type->name)
    {
        term_char_t tc = visual_get(type->name(cell), light);
        map_char_push(mc, tc);
    }
}
int cell_priority(dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (type->priority)
        return type->priority(cell);
    return 2;
}
int cell_light(dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (type->light)
        return type->light(cell);
    return 0;
}
term_char_t cell_ascii(dun_cell_ptr cell)
{
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    if (type->name)
        return visual_get_ascii(type->name(cell));
    return term_char_create('?', TERM_L_DARK); /* panic */
}
/************************************************************************
 * Door (FEAT_DOOR)
 ************************************************************************/
enum { /* subtype */
    DOOR_DOOR,
    DOOR_CURTAIN,
};
enum { /* parm1 gives the 'door state' */
    DOOR_OPEN,
    DOOR_CLOSED,
    DOOR_LOCKED, /* parm2 gives difficulty of open */
    DOOR_JAMMED, /* parm2 gives difficulty of bash */
    DOOR_BROKEN,
};
bool cell_is_door(dun_cell_ptr cell)
{
    if (cell->type != FEAT_DOOR) return FALSE;
    return TRUE;
}
bool door_is_door(dun_cell_ptr cell)
{
    if (!cell_is_door(cell)) return FALSE;
    return cell->subtype == DOOR_DOOR;
}
bool door_is_curtain(dun_cell_ptr cell)
{
    if (!cell_is_door(cell)) return FALSE;
    return cell->subtype == DOOR_CURTAIN;
}
bool door_is_secret(dun_cell_ptr cell)
{
    return wall_has_secret_door(cell);
}
bool door_is_open(dun_cell_ptr cell)
{
    if (!door_is_door(cell)) return FALSE;
    if (cell->parm1 == DOOR_OPEN) return TRUE;
    /* XXX running code doesn't like this:
     * if (cell->parm1 == DOOR_BROKEN) return TRUE; */
    return FALSE;
}
bool door_is_closed(dun_cell_ptr cell)
{
    if (!door_is_door(cell)) return FALSE;
    if (cell->parm1 == DOOR_CLOSED) return TRUE;
    if (cell->parm1 == DOOR_LOCKED) return TRUE;
    if (cell->parm1 == DOOR_JAMMED) return TRUE;
    return FALSE;
}
bool door_is_locked(dun_cell_ptr cell)
{
    if (!door_is_door(cell)) return FALSE;
    return cell->parm1 == DOOR_LOCKED;
}
bool door_is_jammed(dun_cell_ptr cell)
{
    if (!door_is_door(cell)) return FALSE;
    return cell->parm1 == DOOR_JAMMED;
}
bool door_is_broken(dun_cell_ptr cell)
{
    if (!door_is_door(cell)) return FALSE;
    return cell->parm1 == DOOR_BROKEN;
}
bool curtain_is_open(dun_cell_ptr cell)
{
    if (!door_is_curtain(cell)) return FALSE;
    return cell->parm1 == DOOR_OPEN;
}
bool curtain_is_closed(dun_cell_ptr cell)
{
    if (!door_is_curtain(cell)) return FALSE;
    return cell->parm1 == DOOR_CLOSED;
}
static void _door_init(void)
{
    _display_add_no_light("DOOR_OPEN", term_char_create('\'', TERM_L_UMBER));
    _display_add_no_light("DOOR_CLOSED", term_char_create('+', TERM_L_UMBER));
    _display_add_no_light("DOOR_BROKEN", term_char_create('\'', TERM_UMBER));
    _display_add_no_light("CURTAIN_OPEN", term_char_create('\'', TERM_RED));
    _display_add_no_light("CURTAIN_CLOSED", term_char_create('\'', TERM_L_RED));
}
static cptr _door_name(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_DOOR);
    if (cell->subtype == DOOR_DOOR)
    {
        switch (cell->parm1)
        {
        case DOOR_OPEN: return "DOOR_OPEN";
        case DOOR_BROKEN: return "DOOR_BROKEN";
        default: return "DOOR_CLOSED";
        }
    }
    if (cell->parm1 == DOOR_OPEN) return "CURTAIN_OPEN";
    return "CURTAIN_CLOSED";
}
static cptr _door_desc(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_DOOR);
    if (cell->subtype == DOOR_DOOR)
    {
        switch (cell->parm1)
        {
        case DOOR_OPEN: return "open door";
        case DOOR_BROKEN: return "broken door";
        default: return "closed door";
        }
    }
    if (cell->parm1 == DOOR_OPEN) return "open curtain";
    return "closed curtain";
}
static int _door_priority(dun_cell_ptr cell) {
    if (door_is_curtain(cell)) return 2;
    return 10; }
static void _door_flags(dun_cell_ptr cell)
{
    bool old_los = BOOL(cell->flags & CELL_LOS);

    if ( cell->parm1 == DOOR_OPEN
      || cell->parm1 == DOOR_BROKEN )
    {
        cell->flags |= CELL_LOS | CELL_PROJECT;
    }
    else if (cell->subtype == DOOR_CURTAIN)
    {
        cell->flags |= CELL_PROJECT;
        cell->flags &= ~CELL_LOS;
    }
    else
    {
        cell->flags &= ~(CELL_PROJECT | CELL_LOS);
    }

    if (old_los != BOOL(cell->flags & CELL_LOS))
    {
        plr->update |= PU_VIEW | PU_LIGHT | PU_MON_LIGHT | PU_MONSTERS;
    }
}
static int _door_bash(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    int bash, difficulty, chance;
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);

    if ( cell->parm1 == DOOR_OPEN
      || cell->parm1 == DOOR_BROKEN
      || cell->subtype == DOOR_CURTAIN )
    {
        if (!quiet) msg_print("You see nothing there to bash.");
        return ACTION_ABORT;
    }

    bash = adj_str_blow[plr->stat_ind[A_STR]];
    difficulty = 10*cell->parm2;
    chance = MAX(1, bash - difficulty);
    #if DEVELOPER
    if (0)
        msg_format("<color:D>Bash: %d%%</color>", chance);
    #endif
    if (plr->prace == RACE_MON_VORTEX || force || _1d(100) <= chance)
    {
        if (!quiet) msg_print("The door crashes open!");
        if (one_in_(2))
            cell->parm1 = DOOR_BROKEN;
        else
            cell->parm1 = DOOR_OPEN;
        cell->parm2 = 0;
        _door_flags(cell);
        dun_draw_pos(dun, pos);
        return ACTION_SUCCESS;
    }
    else if (_1d(100) <= adj_dex_safe[plr->stat_ind[A_DEX]] + plr->lev)
    {
        if (!quiet) msg_print("The door holds firm.");
        return ACTION_CONTINUE;
    }

    if (!quiet) msg_print("You are off-balance.");
    plr_tim_add(T_PARALYZED, _1d(4));
    return ACTION_FAIL;
}
static int _door_open(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    int skill, difficulty, chance;
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);
    if (cell->parm1 == DOOR_OPEN || cell->parm1 == DOOR_BROKEN)
    {
        if (!quiet) msg_print("You see nothing there to open.");
        return ACTION_ABORT;
    }
    else if (plr->prace == RACE_MON_VORTEX) /* flavor ... vortices cannot open doors! */
        return _door_bash(dun, pos, cell, options);
    else if (cell->parm1 == DOOR_JAMMED)
    {
        if (force || plr->prace == RACE_MON_VORTEX)
            return _door_bash(dun, pos, cell, options);
        if (!quiet) msg_format("The door appears to be stuck.");
        return ACTION_ABORT;
    }

    if (cell->parm1 == DOOR_CLOSED)
    {
        cell->parm1 = DOOR_OPEN;
        cell->parm2 = 0;
        _door_flags(cell);
        dun_draw_pos(dun, pos);
        return ACTION_SUCCESS;
    }

    /* DOOR_LOCKED */
    skill = plr_skill(plr->skills.dis);
    difficulty = 4 * cell->parm2;
    chance = MAX(2, skill - difficulty);
    #if DEVELOPER
    if (0)
        msg_format("<color:D>Pick Lock: %d%%</color>", chance);
    #endif
    if (force || _1d(100) <= chance)
    {
        if (!quiet)
        {
            if (force) msg_print("Click!");
            else msg_print("You have picked the lock.");
        }
        cell->parm1 = DOOR_OPEN;
        cell->parm2 = 0;
        _door_flags(cell);
        dun_draw_pos(dun, pos);
        if (!force) gain_exp(1);
        return ACTION_SUCCESS;
    }
    msg_print("You fail to pick the lock.");
    return ACTION_CONTINUE;
}
static int _door_bash_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    int skill, difficulty;

    if ( cell->parm1 == DOOR_OPEN
      || cell->parm1 == DOOR_BROKEN
      || cell->subtype == DOOR_CURTAIN )
    {
        return ACTION_CONTINUE;
    }

    if (!mon_can_bash_door(mon))
        return ACTION_ABORT;

    skill = mon->hp/10;
    difficulty = cell->parm2 + 1;
    if (skill <= difficulty)
        return ACTION_ABORT;

    if (_1d(skill) > difficulty)
    {
        if (disturb_minor)
        {
            msg_print("You hear a door burst open!");
            disturb(0, 0);
        }
        if (one_in_(2))
            cell->parm1 = DOOR_BROKEN;
        else
            cell->parm1 = DOOR_OPEN;
        cell->parm2 = 0;
        _door_flags(cell);
        dun_draw_pos(dun, pos);
        mon_lore_bash_door(mon);
        return ACTION_SUCCESS;
    }
    return ACTION_FAIL;
}
static int _door_open_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    int skill, difficulty;

    if (cell->parm1 == DOOR_BROKEN || cell->parm1 == DOOR_OPEN)
        return ACTION_CONTINUE;

    if (cell->parm1 == DOOR_JAMMED)
    {
        if (mon_can_bash_door(mon))
            return _door_bash_mon(dun, pos, cell, mon);
        return ACTION_ABORT;
    }

    if (!mon_can_open_door(mon))
    {
        if (mon_can_bash_door(mon))
            return _door_bash_mon(dun, pos, cell, mon);
        return ACTION_ABORT;
    }

    if (cell->parm1 == DOOR_CLOSED)
    {
        cell->parm1 = DOOR_OPEN;
        cell->parm2 = 0;
        _door_flags(cell);
        dun_draw_pos(dun, pos);
        mon_lore_open_door(mon);
        return ACTION_SUCCESS;
    }

    skill = mon->hp/10;
    difficulty = cell->parm2;
    if (skill <= difficulty)
        return ACTION_ABORT;

    if (_1d(skill) > difficulty || plr->action == ACTION_GLITTER)
    {
        cell->parm1 = DOOR_OPEN;
        cell->parm2 = 0;
        _door_flags(cell);
        dun_draw_pos(dun, pos);
        mon_lore_open_door(mon);
        return ACTION_SUCCESS;
    }

    return ACTION_FAIL;
}
static int _door_close(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    if (cell->parm1 == DOOR_BROKEN)
    {
        if (!quiet) msg_print("The door appears broken.");
        return ACTION_ABORT;
    }
    if (cell->parm1 != DOOR_OPEN)
    {
        if (!quiet) msg_print("You see nothing to close.");
        return ACTION_ABORT;
    }
    if (plr->prace == RACE_MON_VORTEX)
    {
        if (!quiet) msg_print("You spin furiously, but nothing happens.");
        return ACTION_FAIL;
    }
    if (cell->subtype != DOOR_CURTAIN && dun_obj_at(dun, pos))
    {
        if (!quiet) msg_print("An object is in the way.");
        return ACTION_FAIL;
    }
    cell->parm1 = DOOR_CLOSED;
    _door_flags(cell);
    dun_draw_pos(dun, pos);
    return ACTION_SUCCESS;
}
static int _door_jam(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    slot_t slot = pack_find_obj(TV_SPIKE, SV_ANY);
    obj_ptr spike;

    if ( cell->parm1 == DOOR_OPEN
      || cell->parm1 == DOOR_BROKEN
      || cell->subtype == DOOR_CURTAIN )
    {
        if (!quiet) msg_print("You see nothing there to spike.");
        return ACTION_ABORT;
    }
    if (!slot)
    {
        if (!quiet) msg_print("You have no spikes.");
        return ACTION_ABORT;
    }

    if (!quiet) msg_print("You jam the door with a spike.");
    if (cell->parm1 == DOOR_LOCKED)
        cell->parm1 = DOOR_JAMMED;
    cell->parm2++;

    spike = pack_obj(slot);
    spike->number--;
    obj_release(spike, 0);
    return ACTION_SUCCESS;
}
static bool _door_affect(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power)
{
    bool notice = FALSE;
    bool seen = plr_can_see(pos);

    switch (gf)
    {
    case GF_KILL_TRAP:
        if (door_is_locked(cell))
        {
            cell->parm1 = DOOR_CLOSED;
            cell->parm2 = 0;
            if (seen)
            {
                msg_print("Click!");
                notice = TRUE;
            }
        }
        break;

    case GF_KILL_DOOR:
    case GF_REMOVE_OBSTACLE:
        dun->type->place_floor(dun, pos);
        if (seen) notice = TRUE;
        break;

    case GF_JAM_DOOR:
        if ( cell->parm1 == DOOR_OPEN
          || cell->parm1 == DOOR_BROKEN
          || cell->subtype == DOOR_CURTAIN )
        {
            break;
        }
        if (cell->parm1 == DOOR_LOCKED)
            cell->parm1 = DOOR_JAMMED;
        cell->parm2++;
        if (seen)
        {
            msg_print("The door seems stuck.");
            notice = TRUE;
        }
        break;

    case GF_KILL_WALL:
        if (!door_is_closed(cell)) break;
        assert(!door_is_curtain(cell)); /* implied by 'door_is_closed' */
        if (seen && (cell->flags & CELL_MAP))
        {
            msg_format("The %s turns into mud!", _door_desc(cell));
            notice = TRUE;
        }
        dun->type->place_floor(dun, pos);
        break;

    case GF_DISINTEGRATE:
        if (seen && (cell->flags & CELL_MAP))
        {
            msg_format("The %s disintegrates!", _door_desc(cell));
            notice = TRUE;
        }
        dun->type->place_floor(dun, pos);
        break;
    }
    return notice;
}
/* movement: closed doors must be opened or bashed first */
static bool _door_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race)
{
    if (door_is_closed(cell)) /* not a curtain */
        return mon_race_can_passwall(race) || mon_race_can_tunnel(race);
    return TRUE;
}
static bool _door_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    if (door_is_closed(cell))
        return mon_can_passwall(mon) || mon_can_tunnel(mon);
    return TRUE;
}
void _door_accept_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    if (door_is_closed(cell) && mon_can_tunnel(mon))
    {
        dun->type->place_floor(dun, pos);
        mon_lore_tunnel(mon);
    }
}
void _door_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (door_is_closed(cell) && plr->kill_wall)
        dun->type->place_floor(dun, pos);
}
static bool _door_allow_plr(dun_cell_ptr cell)
{
    if (door_is_closed(cell))
        return plr->pass_wall || plr->kill_wall;
    return TRUE;
}
static bool _door_allow_obj(dun_cell_ptr cell)
{
    if (cell->subtype == DOOR_CURTAIN) return TRUE;
    return door_is_open(cell);
}
static dun_cell_type_ptr _door(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_DOOR);

    type->init = _door_init;
    type->name = _door_name;
    type->desc = _door_desc;
    type->priority = _door_priority;

    type->allow_plr = _door_allow_plr;
    type->allow_mon = _door_allow_mon;
    type->allow_mon_race = _door_allow_mon_race;
    type->allow_obj = _door_allow_obj;
    type->accept_mon = _door_accept_mon;
    type->accept_plr = _door_accept_plr;

    type->open = _door_open;
    type->close = _door_close;
    type->bash = _door_bash;
    type->jam = _door_jam;

    type->open_mon = _door_open_mon;
    type->bash_mon = _door_bash_mon;

    type->affect = _door_affect;

    return type;
}

/************************************************************************
 * Wall
 ************************************************************************/
enum {
    WALL_GRANITE,
    WALL_PERMANENT,
    WALL_MAGMA,
    WALL_QUARTZ,
    WALL_MOUNTAIN,  /* for D_SURFACE: levitation allows passage; walls normally require passwall */
    WALL_MOUNTAIN_WALL,
    WALL_RUBBLE,
};
bool cell_is_wall(dun_cell_ptr cell)
{
    if (cell->type != FEAT_WALL) return FALSE;
    return TRUE;
}
bool wall_is_granite(dun_cell_ptr cell)
{
    if (!cell_is_wall(cell)) return FALSE;
    return cell->subtype == WALL_GRANITE;
}
bool wall_is_mountain(dun_cell_ptr cell)
{
    if (!cell_is_wall(cell)) return FALSE;
    return cell->subtype == WALL_MOUNTAIN;
}
bool wall_is_mountain_wall(dun_cell_ptr cell)
{
    if (!cell_is_wall(cell)) return FALSE;
    return cell->subtype == WALL_MOUNTAIN_WALL;
}
bool wall_is_rubble(dun_cell_ptr cell)
{
    if (!cell_is_wall(cell)) return FALSE;
    return cell->subtype == WALL_RUBBLE;
}
enum {
    CELL_GOLD = 0x00010000,
    CELL_DOOR = 0x00020000, /* always CELL_SECRET */
};
bool wall_has_secret_door(dun_cell_ptr cell)
{
    if (!cell_is_wall(cell)) return FALSE;
    if (!(cell->flags & CELL_DOOR)) return FALSE;
    assert(cell->flags & CELL_SECRET);
    return BOOL(cell->flags & CELL_SECRET);
}
bool wall_has_treasure(dun_cell_ptr cell)
{
    if (!cell_is_wall(cell)) return FALSE;
    return BOOL(cell->flags & CELL_GOLD);
}
bool wall_has_hidden_treasure(dun_cell_ptr cell)
{
    if (!wall_has_treasure(cell)) return FALSE;
    return BOOL(cell->flags & CELL_SECRET);
}
static void _wall_make_secret_door(dun_cell_ptr cell)
{
    /* during generation, we use dun_type->place_wall for walls. these
     * might not actually *be* walls, in which case, generation should
     * ignore the request for a door (let alone a secret one!). In other
     * words, don't raise a fuss! */
    if (cell->type == FEAT_WALL && cell->subtype != WALL_RUBBLE)
    {
        cell->flags &= ~CELL_GOLD;
        cell->flags |= CELL_DOOR | CELL_SECRET;
    }
}
static bool _wall_detect(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (wall_has_secret_door(cell))
    {
        cell_make_closed_door(cell);
        dun_draw_pos(dun, pos);
        return TRUE;
    }
    else if (wall_has_hidden_treasure(cell))
    {
        cell->flags &= ~CELL_SECRET;
        dun_draw_pos(dun, pos);
        return TRUE;
    }
    return FALSE;
}
static void _wall_init(void)
{
    _display_add("GRANITE", term_char_create('#', TERM_WHITE));
    _display_add("PERMANENT", term_char_create('#', TERM_L_UMBER));
    _display_add("RUBBLE", term_char_create(':', TERM_WHITE));
    _display_add("MOUNTAIN", term_char_create('#', TERM_WHITE));
    _display_add("MOUNTAIN_WALL", term_char_create('#', TERM_ORANGE));
    _display_add("MAGMA_VEIN", term_char_create('%', TERM_SLATE));
    _display_add("MAGMA_TREASURE", term_char_create('*', TERM_ORANGE));
    _display_add("QUARTZ_VEIN", term_char_create('%', TERM_WHITE));
    _display_add("QUARTZ_TREASURE", term_char_create('*', TERM_ORANGE));
}
static cptr _wall_name(dun_cell_ptr cell)
{
    if (cell->subtype == WALL_GRANITE)
        return "GRANITE";
    if (cell->subtype == WALL_PERMANENT)
        return "PERMANENT";
    if (cell->subtype == WALL_RUBBLE)
        return "RUBBLE";
    if (cell->subtype == WALL_MOUNTAIN)
        return "MOUNTAIN";
    if (cell->subtype == WALL_MOUNTAIN_WALL)
        return "MOUNTAIN_WALL";
    if (cell->subtype == WALL_MAGMA)
    {
        if ((cell->flags & CELL_GOLD) && !(cell->flags & CELL_SECRET))
            return "MAGMA_TREASURE";
        return "MAGMA_VEIN";
    }
    if (cell->subtype == WALL_QUARTZ)
    {
        if ((cell->flags & CELL_GOLD) && !(cell->flags & CELL_SECRET))
            return "QUARTZ_TREASURE";
        return "QUARTZ_VEIN";
    }
    return "Unknown";
}
static cptr _wall_desc(dun_cell_ptr cell)
{
    if (cell->subtype == WALL_GRANITE)
        return "granite wall";
    if (cell->subtype == WALL_PERMANENT)
        return "permanent wall";
    if (cell->subtype == WALL_RUBBLE)
        return "pile of rubble";
    if (cell->subtype == WALL_MOUNTAIN)
        return "mountain chain";
    if (cell->subtype == WALL_MOUNTAIN_WALL)
        return "mountain chain (wall)";
    if (cell->subtype == WALL_MAGMA)
    {
        if ((cell->flags & CELL_GOLD) && !(cell->flags & CELL_SECRET))
            return "magma vein with treasure";
        return "magma vein";
    }
    if (cell->subtype == WALL_QUARTZ)
    {
        if ((cell->flags & CELL_GOLD) && !(cell->flags & CELL_SECRET))
            return "quartz vein with treasure";
        return "quartz vein";
    }
    return "Unknown";
}
static int _wall_priority(dun_cell_ptr cell)
{
    switch (cell->subtype)
    {
    case WALL_PERMANENT:
    case WALL_MOUNTAIN:
    case WALL_MOUNTAIN_WALL: return 5;
    }
    return 2;
}
static void _wall_flags(dun_cell_ptr cell)
{
    if ( cell->subtype == WALL_MOUNTAIN_WALL
      || cell->subtype == WALL_PERMANENT
      || cell->subtype == WALL_MOUNTAIN )
        cell->flags |= CELL_PERM;
}
static int _wall_power(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_WALL);
    assert(!(cell->flags & CELL_PERM));
    if (cell->flags & CELL_DOOR) return 5; /* not really a wall after all ... */
    switch (cell->subtype)
    {
    case WALL_GRANITE: return 40;
    case WALL_MAGMA: return 10;
    case WALL_QUARTZ: return 20;
    }
    return 40;
}
static int _wall_search(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);
    int skill;

    assert(cell->type == FEAT_WALL);
    if (!(cell->flags & CELL_SECRET)) return ACTION_CONTINUE; /* don't leak info */
    if (!(cell->flags & CELL_DOOR)) return ACTION_CONTINUE; /* secret gold cannot be searched for */

    skill = plr_skill(plr->skills.srh);
    if (force || _1d(100) <= skill)
    {
        _wall_detect(dun, pos, cell);
        if (!quiet) msg_print("You have found a secret door.");
        disturb(0, 0);
        return ACTION_SUCCESS;
    }
    return ACTION_CONTINUE;
}
static int _wall_tunnel(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);
    int power;
    if (cell->flags & CELL_PERM)
    {
        if (!quiet) msg_print("This seems to be permanent rock.");
        return ACTION_ABORT;
    }
    if (cell->subtype == WALL_RUBBLE)
    {
        #if DEVELOPER
        if (0) {
            int chance = plr->skill_dig * 100 / 200;
            msg_format("<color:D>Dig: %d%%</color>", chance);
        }
        #endif
        if (force || plr->skill_dig > randint0(200))
        {
            if (!quiet) msg_print("You have removed the pile of rubble.");
            dun->type->place_floor(dun, pos);
            if (dun->type->id != D_SURFACE)
            {
                if (_1d(100) <= 15 - dun->dun_lvl/2)
                {
                    place_object(pos, dun->difficulty, 0);
                    if (!quiet && plr_can_see(pos))
                        msg_print("You have found something!");
                }
            }
            return ACTION_SUCCESS;
        }
        else
        {
            if (!quiet) msg_print("You dig into the pile of rubble.");
            return ACTION_CONTINUE;
        }
    }
    power = _wall_power(cell);
    #if DEVELOPER
    if (0) {
        int skill = plr->skill_dig - power;
        int chance = skill > 0 ? plr->skill_dig * 100 / (40*power) : 0;
        msg_format("<color:D>Dig: %d%%</color>", chance);
    }
    #endif
    if (force || plr->skill_dig > power + _1d(40 * power))
    {
        bool gold = BOOL(cell->flags & CELL_GOLD); /* before place_floor */
        if (!quiet) msg_print("You have finished the tunnel.");
        dun->type->place_floor(dun, pos); /* XXX cell is no longer a wall */
        plr->update |= PU_FLOW | PU_MON_FLOW;
        if (gold)
        {
            place_gold(pos);
            if (!quiet && plr_can_see(pos))
                msg_print("You have found something!");
        }

        if (!force) virtue_add(VIRTUE_DILIGENCE, 1);
        virtue_add(VIRTUE_NATURE, -1);
        return ACTION_SUCCESS;
    }
    if (!quiet) msg_format("You tunnel into the %s.", _wall_desc(cell));
    if (plr->skill_dig <= power && always_repeat) /* hopeless */
        disturb(0, 0);
    if ((cell->flags & CELL_DOOR) && one_in_(4))
        return _wall_search(dun, pos, cell, options); /* stop digging if wall becomes a door */
    return ACTION_CONTINUE;
}
static bool _wall_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race)
{
    if (cell->subtype == WALL_MOUNTAIN) /* XXX oddball wall */
        return mon_race_can_fly(race) || mon_race_can_climb(race);
    if (cell->flags & CELL_PERM) return FALSE;
    return mon_race_can_passwall(race) || mon_race_can_tunnel(race);
}
static bool _wall_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    if (cell->subtype == WALL_MOUNTAIN) /* XXX oddball wall */
        return mon_can_fly(mon) || mon_can_climb(mon);
    if (cell->flags & CELL_PERM) return FALSE;
    return mon_can_passwall(mon) || mon_can_tunnel(mon);
}
static bool _wall_allow_plr(dun_cell_ptr cell)
{
    if (cell->subtype == WALL_MOUNTAIN) /* XXX oddball wall */
        return plr->levitation;
    if (cell->flags & CELL_PERM) return FALSE;
    return plr->pass_wall || plr->kill_wall;
}
static void _wall_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    assert(dun_plr_at(dun, pos));
    if (cell->subtype == WALL_MOUNTAIN) return; /* XXX oddball wall */
  /*if (plr->tunnel_wall) XXX Distinguish 'Orc digger' ... XXX
        _wall_tunnel(dun, pos, cell, ACTION_FORCE);
    else*/ if (plr->kill_wall)
    {
        dun->type->place_floor(dun, pos);
        plr->update |= PU_FLOW | PU_MON_FLOW;
    }
    else if (!elemental_is_(ELEMENTAL_EARTH))
        energy_use = energy_use * 3 / 2;
}
static void _wall_accept_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    assert(point_equals(mon->pos, pos));
    if (cell->subtype == WALL_MOUNTAIN) return; /* XXX oddball wall */
    assert(!(cell->flags & CELL_PERM));
    if (mon_can_tunnel(mon))
    {
        if (one_in_(20))
            msg_print("There is a grinding sound.");
        dun->type->place_floor(dun, pos);
        plr->update |= PU_FLOW | PU_MON_FLOW;
        mon_lore_tunnel(mon);
    }
}
static bool _wall_affect(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power)
{
    bool notice = FALSE;
    bool seen = plr_can_see(pos);
    switch (gf)
    {
    case GF_KILL_WALL: {
        bool gold = BOOL(cell->flags & CELL_GOLD); /* before place_floor */
        if (cell->flags & CELL_PERM) break;
        if (seen && (cell->flags & CELL_MAP))
        {
            msg_format("The %s turns into mud!", _wall_desc(cell));
            notice = TRUE;
        }
        dun->type->place_floor(dun, pos);
        plr->update |= (PU_FLOW | PU_MON_FLOW);
        if (gold)
        {
            place_gold(pos);
            if (seen)
                msg_print("You have found something!");
        }
        break; }
    case GF_DISINTEGRATE:
        if (cell->flags & CELL_PERM) break;
        if (seen && (cell->flags & CELL_MAP))
        {
            msg_format("The %s disintegrates!", _wall_desc(cell));
            notice = TRUE;
        }
        dun->type->place_floor(dun, pos);
        plr->update |= (PU_FLOW | PU_MON_FLOW);
        break;
    }
    return notice;
}
void _wall_process_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    int dam = 0;
    cptr killer = "solid rock";

    assert(dun_plr_at(dun, pos));
    if (wall_is_mountain(cell)) return;
    if (plr_tim_find(T_INVULN)) return;
    if (plr->pass_wall && plr->no_passwall_dam) return;

    dam = 1 + plr->lev/5;
    if (plr->pass_wall)
    {
        msg_print("Your molecules feel disrupted!");
        killer = "corporeal reality";
        if (plr->prace == RACE_SPECTRE && dam > plr->chp) /* spectres never die */
            dam = plr->chp;
    }
    else
        msg_print("You are being crushed!");

    take_hit(DAMAGE_NOESCAPE, dam, killer);
    plr->cave_no_regen = TRUE;
}
static dun_cell_type_ptr _wall(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_WALL);

    type->init = _wall_init;
    type->name = _wall_name;
    type->desc = _wall_desc;
    type->priority = _wall_priority;

    type->detect = _wall_detect;

    type->tunnel = _wall_tunnel;
    type->search = _wall_search;

    type->allow_plr = _wall_allow_plr;
    type->allow_mon = _wall_allow_mon;
    type->allow_mon_race = _wall_allow_mon_race;

    type->affect = _wall_affect;

    type->accept_plr = _wall_accept_plr;
    type->accept_mon = _wall_accept_mon;
    type->process_plr = _wall_process_plr;

    return type;
}
/************************************************************************
 * Tree
 ************************************************************************/
bool cell_is_tree(dun_cell_ptr cell) { return cell->type == FEAT_TREE; }
static void _tree_flags(dun_cell_ptr cell)
{
}
static void _tree_init(void) { _display_add("TREE", term_char_create('#', TERM_L_GREEN)); }
static cptr _tree_name(dun_cell_ptr cell) { return "TREE"; }
static cptr _tree_desc(dun_cell_ptr cell) { return "tree"; }
static bool _tree_allow_plr(dun_cell_ptr cell) { return TRUE; }
static bool _tree_allow_obj(dun_cell_ptr cell) { return TRUE; }
static bool _tree_allow_mon(dun_cell_ptr cell, mon_ptr mon) { return TRUE; }
static bool _tree_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race) { return TRUE; }
static int _tree_tunnel(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);
    int power = 10;
    if (force || plr->skill_dig > power + _1d(40 * power))
    {
        if (!quiet) msg_print("You have cleared away the tree.");
        dun_place_grass(dun, pos);
        if (!force) virtue_add(VIRTUE_DILIGENCE, 1);
        virtue_add(VIRTUE_NATURE, -2);
        return ACTION_SUCCESS;
    }
    if (!quiet) msg_print("You chop away at the tree.");
    return ACTION_CONTINUE;
}
static void _tree_accept_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    assert(point_equals(mon->pos, pos));
    if (mon_can_tunnel(mon))
    {
        cell_make_grass(cell);
        mon_lore_tunnel(mon);
    }
    else if (!mon_can_fly(mon) && !(mon->race->alloc.flags & RFA_WILD_WOOD))
        mon->energy_need += ENERGY_NEED();
}
static void _tree_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    assert(dun_plr_at(dun, pos));
    if (plr->kill_wall)
        cell_make_grass(cell);
    else
    {
        /* movement thru the forest is slow ... riding players require suitable
         * mounts while non-riding players require racial|class bonuses (e.g.
         * rangers, scouts, ents and some elves). flying also speeds things up.  */
        if (plr->riding)
        {
            mon_race_ptr race = plr_riding_race();
            if ( !(race->alloc.flags & RFA_WILD_WOOD)
              && !(race->move.flags & RFM_FLY) )
            {
                energy_use *= 2;
            }
        }
        else if (!plr->pass_tree && !plr->levitation)
        {
            energy_use *= 2;
        }
    }
}
static bool _tree_affect(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power)
{
    bool notice = FALSE;
    bool seen = plr_can_see(pos);

    /* handle tree destruction */
    {
        cptr message = NULL;
        switch (gf)
        {
        case GF_DEATH_TOUCH:
        case GF_DEATH_RAY:
            message = "dies.";
            break;
        case GF_POIS:
        case GF_NUKE:
            message = "is blasted.";
            break;
        case GF_TIME:
            message = "shrinks.";
            break;
        case GF_ACID:
            message = "melts.";
            break;
        case GF_COLD:
        case GF_ICE:
            message = "is frozen and smashed.";
            break;
        case GF_FIRE:
        case GF_ELEC:
        case GF_PLASMA:
            message = "burns up!";
            break;
        case GF_METEOR:
        case GF_CHAOS:
        case GF_MANA:
        case GF_SEEKER:
        case GF_SUPER_RAY:
        case GF_SHARDS:
        case GF_ROCK:
        case GF_ROCKET:
        case GF_SOUND:
        case GF_DISENCHANT:
        case GF_FORCE:
        case GF_GRAVITY:
            message = "is crushed.";
            break;
        case GF_REMOVE_OBSTACLE:
            message = "is removed.";
            break;
        case GF_DISINTEGRATE:
            message = "disintegrates.";
            break;
        }
        if (message)
        {
            if (seen) msg_format("A tree %s", message);
            if (one_in_(3))
                dun_place_brake(dun, pos);
            else
                dun_place_grass(dun, pos);
            if (cell->flags & CELL_MAP) notice = TRUE;
            return notice;
        }
    }
    return notice;
}
static dun_cell_type_ptr _tree(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_TREE);

    type->init = _tree_init;
    type->name = _tree_name;
    type->desc = _tree_desc;

    type->tunnel = _tree_tunnel;

    type->allow_plr = _tree_allow_plr;
    type->allow_mon = _tree_allow_mon;
    type->allow_mon_race = _tree_allow_mon_race;
    type->allow_obj = _tree_allow_obj;

    type->accept_plr = _tree_accept_plr;
    type->accept_mon = _tree_accept_mon;

    type->affect = _tree_affect;

    return type;
}
/************************************************************************
 * Water
 ************************************************************************/
enum {
    WATER_SWAMP,
    WATER_SHALLOW,
    WATER_DEEP
};
bool cell_is_water(dun_cell_ptr cell) { return cell->type == FEAT_WATER; }
bool water_is_swamp(dun_cell_ptr cell)
{
    if (!cell_is_water(cell)) return FALSE;
    return cell->subtype == WATER_SWAMP;
}
bool water_is_deep(dun_cell_ptr cell)
{
    if (!cell_is_water(cell)) return FALSE;
    return cell->subtype == WATER_DEEP;
}
bool water_is_shallow(dun_cell_ptr cell)
{
    if (!cell_is_water(cell)) return FALSE;
    return cell->subtype == WATER_SHALLOW;
}
static void _water_flags(dun_cell_ptr cell)
{
    cell->flags |= CELL_LOS | CELL_PROJECT;
}
static void _water_init(void)
{
    _display_add("SWAMP", term_char_create('.', TERM_L_BLUE));
    _display_add("SHALLOW_WATER", term_char_create('~', TERM_L_BLUE));
    _display_add("DEEP_WATER", term_char_create('~', TERM_BLUE));
}
static cptr _water_name(dun_cell_ptr cell)
{
    if (water_is_swamp(cell)) return "SWAMP";
    if (water_is_shallow(cell)) return "SHALLOW_WATER";
    if (water_is_deep(cell)) return "DEEP_WATER";
    return "UNKNOWN";
}
static cptr _water_desc(dun_cell_ptr cell)
{
    if (water_is_swamp(cell)) return "swamp";
    if (water_is_shallow(cell)) return "shallow water";
    if (water_is_deep(cell)) return "deep water";
    return "something strange";
}
static bool _water_allow_obj(dun_cell_ptr cell)
{
    if (water_is_deep(cell)) return FALSE;
    return TRUE;
}
static bool _water_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race)
{
    if (mon_race_is_aquatic(race)) return TRUE;
    if (mon_race_can_swim(race)) return TRUE;
    if (mon_race_can_fly(race)) return TRUE;

    if (water_is_deep(cell)) return FALSE; /* deep requires aquatic|swim|fly */
    if (mon_auras_find(race, GF_FIRE)) return FALSE; /* shallow|swamp excludes fiery aura */

    return TRUE; /* other monsters may wade into the shallows */
}
static bool _water_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    if (mon_can_fly(mon)) return TRUE; /* mon might gain levitation (RACE_MON_RING) */
    return _water_allow_mon_race(cell, mon->race);
}
static bool _water_allow_plr(dun_cell_ptr cell) { return TRUE; }
static void _water_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (elemental_is_(ELEMENTAL_WATER))
        energy_use /= 2;
    else if (prace_is_(RACE_WATER_ELF))
        energy_use = energy_use * 7 / 10;
}
void _water_process_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    assert(dun_plr_at(dun, pos));
    if (!water_is_deep(cell)) return;
    if (plr->levitation) return;
    if (plr->can_swim) return;
    if (elemental_is_(ELEMENTAL_WATER)) return;
    if (prace_is_(RACE_WATER_ELF)) return;

    if (plr_total_weight() > weight_limit())
    {
        msg_print("You are drowning!");
        take_hit(DAMAGE_NOESCAPE, _1d(plr->lev), "drowning");
        plr->cave_no_regen = TRUE;
    }
}
static dun_cell_type_ptr _water(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_WATER);

    type->init = _water_init;
    type->name = _water_name;
    type->desc = _water_desc;

    type->allow_plr = _water_allow_plr;
    type->allow_mon = _water_allow_mon;
    type->allow_mon_race = _water_allow_mon_race;
    type->allow_obj = _water_allow_obj;

    type->accept_plr = _water_accept_plr;
    type->process_plr = _water_process_plr;
    return type;
}
/************************************************************************
 * Lava
 ************************************************************************/
enum {
    LAVA_SHALLOW,
    LAVA_DEEP
};
bool cell_is_lava(dun_cell_ptr cell) { return cell->type == FEAT_LAVA; }
bool lava_is_deep(dun_cell_ptr cell)
{
    if (!cell_is_lava(cell)) return FALSE;
    return cell->subtype == LAVA_DEEP;
}
bool lava_is_shallow(dun_cell_ptr cell)
{
    if (!cell_is_lava(cell)) return FALSE;
    return cell->subtype == LAVA_SHALLOW;
}
static void _lava_flags(dun_cell_ptr cell)
{
    cell->flags |= CELL_LOS | CELL_PROJECT | CELL_LIT;
    if (lava_is_deep(cell))
        cell->flags |= CELL_LIGHT;
}
static void _lava_init(void)
{
    _display_add("SHALLOW_LAVA", term_char_create('~', TERM_L_UMBER));
    _display_add("DEEP_LAVA", term_char_create('~', TERM_RED));
}
static cptr _lava_name(dun_cell_ptr cell)
{
    if (lava_is_shallow(cell)) return "SHALLOW_LAVA";
    if (lava_is_deep(cell)) return "DEEP_LAVA";
    return "UNKNOWN";
}
static cptr _lava_desc(dun_cell_ptr cell)
{
    if (lava_is_shallow(cell)) return "shallow lava";
    if (lava_is_deep(cell)) return "deep lava";
    return "something strange";
}
static int _lava_light(dun_cell_ptr cell) { return cell->subtype == LAVA_DEEP ? /*XXX*/ 0 : 0; }
static bool _lava_allow_obj(dun_cell_ptr cell)
{
    if (lava_is_deep(cell)) return FALSE;
    return TRUE;
}
static bool _lava_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race)
{
    if (mon_race_can_fly(race)) return TRUE;
    if (!mon_race_immune(race, GF_FIRE)) return FALSE;
    return TRUE;
}
static bool _lava_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    if (mon_can_fly(mon)) return TRUE; /* mon might gain levitation (RACE_MON_RING) */
    return _lava_allow_mon_race(cell, mon->race);
}
static bool _lava_allow_plr(dun_cell_ptr cell) { return TRUE; }
static void _lava_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (elemental_is_(ELEMENTAL_FIRE))
        energy_use /= 2;
}
void _lava_process_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    int dam = 0;

    assert(dun_plr_at(dun, pos));
    if (plr_tim_find(T_INVULN)) return;
    if (elemental_is_(ELEMENTAL_FIRE)) return;

    if (lava_is_deep(cell))
        dam = 6000 + _1d(4000);
    else if (!plr->levitation)
        dam = 3000 + _1d(2000);

    dam = res_calc_dam(GF_FIRE, dam);
    if (plr->levitation) dam = dam / 5;

    if (dam)
    {
        dam = dam / 100 + (_1d(100) <= (dam % 100));

        if (plr->levitation)
        {
            char buf[100];
            msg_print("The heat burns you!");
            sprintf(buf, "flying over %s", _lava_desc(cell));
            take_hit(DAMAGE_NOESCAPE, dam, buf);
        }
        else
        {
            cptr desc = _lava_desc(cell);
            msg_format("The %s burns you!", desc);
            take_hit(DAMAGE_NOESCAPE, dam, desc);
        }
        plr->cave_no_regen = TRUE;
    }
}
static dun_cell_type_ptr _lava(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_LAVA);

    type->init = _lava_init;
    type->name = _lava_name;
    type->desc = _lava_desc;
    type->light = _lava_light;

    type->allow_plr = _lava_allow_plr;
    type->allow_mon = _lava_allow_mon;
    type->allow_mon_race = _lava_allow_mon_race;
    type->allow_obj = _lava_allow_obj;

    type->accept_plr = _lava_accept_plr;
    type->process_plr = _lava_process_plr;

    return type;
}
/************************************************************************
 * Chasm
 ************************************************************************/
bool cell_is_chasm(dun_cell_ptr cell) { return cell->type == FEAT_CHASM; }
static void _chasm_flags(dun_cell_ptr cell) { cell->flags |= CELL_LOS | CELL_PROJECT; }
static void _chasm_init(void) { _display_add("CHASM", term_char_create('.', TERM_L_DARK)); }
static cptr _chasm_name(dun_cell_ptr cell) { return "CHASM"; }
static cptr _chasm_desc(dun_cell_ptr cell) { return "dark pit"; }
static int  _chasm_priority(dun_cell_ptr cell) { return 1; }
static bool _chasm_allow_plr(dun_cell_ptr cell) { return plr->levitation; }
static bool _chasm_allow_mon(dun_cell_ptr cell, mon_ptr mon) { return mon_can_fly(mon); }
static bool _chasm_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race) { return mon_race_can_fly(race); }
static dun_cell_type_ptr _chasm(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_CHASM);

    type->init = _chasm_init;
    type->name = _chasm_name;
    type->desc = _chasm_desc;
    type->priority = _chasm_priority;

    type->allow_plr = _chasm_allow_plr;
    type->allow_mon = _chasm_allow_mon;
    type->allow_mon_race = _chasm_allow_mon_race;

    return type;
}
/************************************************************************
 * Stairs
 ************************************************************************/
enum {
    STAIRS_DOWN,
    STAIRS_UP
};
bool cell_is_stairs(dun_cell_ptr cell) { return cell->type == FEAT_STAIRS; }
bool stairs_go_down(dun_cell_ptr cell)
{
    if (!cell_is_stairs(cell)) return FALSE;
    return cell->subtype == STAIRS_DOWN;
}
bool stairs_go_up(dun_cell_ptr cell)
{
    if (!cell_is_stairs(cell)) return FALSE;
    return cell->subtype == STAIRS_UP;
}
bool stairs_enter_quest(dun_cell_ptr cell)
{
    if (!cell_is_stairs(cell)) return FALSE;
    return BOOL(cell->flags & CELL_QUEST);
}
int stairs_quest_id(dun_cell_ptr cell)
{
    if (!stairs_enter_quest(cell)) return 0;
    return cell->parm2;
}
bool stairs_enter_dungeon(dun_cell_ptr cell)
{
    if (!cell_is_stairs(cell)) return FALSE;
    return BOOL(cell->flags & CELL_DUNGEON);
}
int stairs_dun_type_id(dun_cell_ptr cell)
{
    if (!stairs_enter_dungeon(cell)) return 0;
    return cell->parm2;
}
static void _stairs_flags(dun_cell_ptr cell)
{
    cell->flags |= CELL_PERM | CELL_LOS | CELL_PROJECT;
    if (cell->flags & (CELL_QUEST | CELL_DUNGEON))
        cell->flags |= CELL_LIT | CELL_LIGHT | CELL_MAP;
}
static void _stairs_init(void)
{
    _display_add_no_light("STAIRS_DOWN", term_char_create('>', TERM_WHITE));
    _display_add_no_light("STAIRS_UP", term_char_create('<', TERM_WHITE)); 
    _display_add_no_light("ENTRANCE", term_char_create('>', TERM_VIOLET));
    _display_add_no_light("QUEST_ENTRANCE", term_char_create('>', TERM_YELLOW));
}
static cptr _stairs_name(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_STAIRS);
    if (stairs_enter_quest(cell)) return "QUEST_ENTRANCE";
    else if (stairs_enter_dungeon(cell)) return "ENTRANCE";
    else if (cell->subtype == STAIRS_DOWN) return "STAIRS_DOWN";
    else if (cell->subtype == STAIRS_UP) return "STAIRS_UP";
    return "UNKNOWN";
}
static cptr _stairs_desc(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_STAIRS);
    if (stairs_enter_quest(cell)) return "quest entrance";
    else if (stairs_enter_dungeon(cell)) return "dungeon entrance";
    else if (cell->subtype == STAIRS_DOWN) return "down staircase";
    else if (cell->subtype == STAIRS_UP) return "up staircase";
    return "unknown staircase";
}
static int _stairs_priority(dun_cell_ptr cell) { return 35; }
static bool _stairs_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race) { return TRUE; }
static bool _stairs_allow_mon(dun_cell_ptr cell, mon_ptr mon) { return TRUE; }
static bool _stairs_allow_plr(dun_cell_ptr cell) { return TRUE; }
static dun_cell_type_ptr _stairs(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_STAIRS);

    type->init = _stairs_init;
    type->name = _stairs_name;
    type->desc = _stairs_desc;
    type->priority = _stairs_priority;

    type->allow_plr = _stairs_allow_plr;
    type->allow_mon = _stairs_allow_mon;
    type->allow_mon_race = _stairs_allow_mon_race;

    return type;
}

/************************************************************************
 * Portal
 ************************************************************************/
enum {
    PORTAL_RECALL,
    PORTAL_TRAVEL
};
bool cell_is_portal(dun_cell_ptr cell) { return cell->type == FEAT_PORTAL; }
bool portal_is_recall(dun_cell_ptr cell)
{
    if (!cell_is_portal(cell)) return FALSE;
    return cell->subtype == PORTAL_RECALL;
}
bool portal_is_travel(dun_cell_ptr cell)
{
    if (!cell_is_portal(cell)) return FALSE;
    return cell->subtype == PORTAL_TRAVEL;
}
static void _portal_flags(dun_cell_ptr cell)
{
    cell->flags |= CELL_PERM | CELL_LOS | CELL_PROJECT | CELL_LIGHT;
}
static void _portal_init(void)
{
    _display_add_no_light("RECALL", term_char_create('*', TERM_VIOLET));
    _display_add_no_light("TRAVEL", term_char_create('*', TERM_L_BLUE));
}
static cptr _portal_name(dun_cell_ptr cell)
{
    if (portal_is_recall(cell)) return "RECALL";
    if (portal_is_travel(cell)) return "TRAVEL";
    return "UNKNOWN";
}
static cptr _portal_desc(dun_cell_ptr cell)
{
    if (portal_is_recall(cell)) return "Wizard Tile";
    if (portal_is_travel(cell)) return "Travel Portal";
    return "something strange";
}
static void _portal_display(dun_cell_ptr cell, int light, map_char_ptr mc)
{
    term_char_t tc;
    assert(cell->type == FEAT_PORTAL);

    tc = visual_get("FLOOR", light);
    map_char_push(mc, tc);

    tc = visual_get(_portal_name(cell), light);
    map_char_push(mc, tc);
}
static int _portal_priority(dun_cell_ptr cell) { return 19; }
static int _portal_light(dun_cell_ptr cell) { return 1; }
static bool _portal_allow_plr(dun_cell_ptr cell) { return TRUE; }
static bool _portal_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race) { return TRUE; }
static bool _portal_allow_mon(dun_cell_ptr cell, mon_ptr mon) { return TRUE; }
static void _portal_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (portal_is_travel(cell))
    {
        disturb(0, 0);
        if (msg_prompt("Travel to next world? <color:y>[y/n]</color>", "ny", PROMPT_YES_NO) == 'y')
            dun_mgr_travel_plr();
    }
    else if (portal_is_recall(cell))
    {
        disturb(0, 0);
        if (msg_prompt("Activate Recall? <color:y>[y/n]</color>", "ny", PROMPT_YES_NO) == 'y')
            dun_mgr_recall_plr();
    }
}
static dun_cell_type_ptr _portal(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_PORTAL);

    type->init = _portal_init;
    type->name = _portal_name;
    type->desc = _portal_desc;
    type->display = _portal_display;
    type->priority = _portal_priority;
    type->light = _portal_light;

    type->allow_plr = _portal_allow_plr;
    type->allow_mon = _portal_allow_mon;
    type->allow_mon_race = _portal_allow_mon_race;

    type->accept_plr = _portal_accept_plr;

    return type;
}
/************************************************************************
 * Shops and Buildings (FEAT_BLDG)
 ************************************************************************/
enum {
    BLDG_SHOP, /* parm1 gives SHOP_FOO from shop.h */
    BLDG_BLDG, /* parm1 gives BLDG_FOO from shop.h */
};
bool cell_is_bldg(dun_cell_ptr cell) { return cell->type == FEAT_BLDG; }

bool bldg_is_shop(dun_cell_ptr cell)
{
    if (!cell_is_bldg(cell)) return FALSE;
    return cell->subtype == BLDG_SHOP;
}
bool shop_is_(dun_cell_ptr cell, int which)
{
    if (!bldg_is_shop(cell)) return FALSE;
    return cell->parm1 == which;
}
int shop_id(dun_cell_ptr cell)
{
    if (!bldg_is_shop(cell)) return SHOP_NONE;
    return cell->parm1;
}
bool shop_is_general_store(dun_cell_ptr cell) { return shop_is_(cell, SHOP_GENERAL); }
bool shop_is_armory(dun_cell_ptr cell) { return shop_is_(cell, SHOP_ARMORY); }
bool shop_is_weapon_smith(dun_cell_ptr cell) { return shop_is_(cell, SHOP_WEAPON); }
bool shop_is_temple(dun_cell_ptr cell) { return shop_is_(cell, SHOP_TEMPLE); }
bool shop_is_alchemist(dun_cell_ptr cell) { return shop_is_(cell, SHOP_ALCHEMIST); }
bool shop_is_magic_shop(dun_cell_ptr cell) { return shop_is_(cell, SHOP_MAGIC); }
bool shop_is_black_market(dun_cell_ptr cell) { return shop_is_(cell, SHOP_BLACK_MARKET); }
bool shop_is_home(dun_cell_ptr cell) { return shop_is_(cell, SHOP_HOME); }
bool shop_is_bookstore(dun_cell_ptr cell) { return shop_is_(cell, SHOP_BOOK); }
bool shop_is_museum(dun_cell_ptr cell) { return shop_is_(cell, SHOP_MUSEUM); }
bool shop_is_jeweler(dun_cell_ptr cell) { return shop_is_(cell, SHOP_JEWELER); }

bool bldg_is_bldg(dun_cell_ptr cell)
{
    if (!cell_is_bldg(cell)) return FALSE;
    return cell->subtype == BLDG_BLDG;
}
bool bldg_is_(dun_cell_ptr cell, int which)
{
    if (!bldg_is_bldg(cell)) return FALSE;
    return cell->parm1 == which;
}
int bldg_id(dun_cell_ptr cell)
{
    if (!bldg_is_bldg(cell)) return BLDG_NONE;
    return cell->parm1;
}
bool bldg_is_inn(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_INN); }
bool bldg_is_castle(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_CASTLE); }
bool bldg_is_fighters_guild(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_FIGHTERS_GUILD); }
bool bldg_is_archers_guild(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_ARCHERS_GUILD); }
bool bldg_is_thieves_guild(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_THIEVES_GUILD); }
bool bldg_is_wizards_guild(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_WIZARDS_GUILD); }
bool bldg_is_priests_guild(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_PRIESTS_GUILD); }
bool bldg_is_hunters_office(dun_cell_ptr cell) { return bldg_is_(cell, BLDG_HUNTERS_OFFICE); }

static void _bldg_flags(dun_cell_ptr cell)
{
    cell->flags |= CELL_PERM /* | CELL_LOS | CELL_PROJECT*/;
    if ( shop_is_home(cell)
      && (plr->prace == RACE_VAMPIRE || plr->prace == RACE_MON_VAMPIRE) )
    {
        cell->flags |= CELL_DARK;
    }
    else
        cell->flags |= /*CELL_LIT |*/ CELL_LIGHT;
}
static void _bldg_init(void)
{
    _display_add("GENERAL_STORE", term_char_create('1', TERM_L_UMBER));
    _display_add("ARMORY", term_char_create('2', TERM_SLATE));
    _display_add("WEAPON_SMITHS", term_char_create('3', TERM_WHITE));
    _display_add("TEMPLE", term_char_create('4', TERM_GREEN));
    _display_add("ALCHEMIST", term_char_create('5', TERM_BLUE));
    _display_add("MAGIC_SHOP", term_char_create('6', TERM_RED));
    _display_add("BLACK_MARKET", term_char_create('7', TERM_L_DARK));
    _display_add("HOME", term_char_create('8', TERM_YELLOW));
    _display_add("BOOKSTORE", term_char_create('9', TERM_ORANGE));
    _display_add("MUSEUM", term_char_create('0', TERM_VIOLET));
    _display_add("JEWELER", term_char_create('"', TERM_VIOLET));

    _display_add("BLDG_INN", term_char_create('+', TERM_L_UMBER));
    _display_add("BLDG_CASTLE", term_char_create('+', TERM_VIOLET));
    _display_add("BLDG_FIGHTERS", term_char_create('+', TERM_L_UMBER));
    _display_add("BLDG_ARCHERS", term_char_create('+', TERM_L_UMBER));
    _display_add("BLDG_THIEVES", term_char_create('+', TERM_L_DARK));
    _display_add("BLDG_WIZARDS", term_char_create('+', TERM_L_BLUE));
    _display_add("BLDG_PRIESTS", term_char_create('+', TERM_ORANGE));
    _display_add("BLDG_HUNTERS", term_char_create('+', TERM_L_RED));
}
static cptr _bldg_name(dun_cell_ptr cell)
{
    if (bldg_is_shop(cell))
    {
        switch (shop_id(cell))
        {
        case SHOP_GENERAL: return "GENERAL_STORE";
        case SHOP_ARMORY: return "ARMORY";
        case SHOP_WEAPON: return "WEAPON_SMITHS";
        case SHOP_TEMPLE: return "TEMPLE";
        case SHOP_ALCHEMIST: return "ALCHEMIST";
        case SHOP_MAGIC: return "MAGIC_SHOP";
        case SHOP_BLACK_MARKET: return "BLACK_MARKET";
        case SHOP_HOME: return "HOME";
        case SHOP_BOOK: return "BOOKSTORE";
        case SHOP_MUSEUM: return "MUSEUM";
        case SHOP_JEWELER: return "JEWELER";
        }
    }
    if (bldg_is_bldg(cell))
    {
        switch (bldg_id(cell))
        {
        case BLDG_INN: return "BLDG_INN";
        case BLDG_CASTLE: return "BLDG_CASTLE";
        case BLDG_FIGHTERS_GUILD: return "BLDG_FIGHTERS";
        case BLDG_ARCHERS_GUILD: return "BLDG_ARCHERS";
        case BLDG_THIEVES_GUILD: return "BLDG_THIEVES";
        case BLDG_WIZARDS_GUILD: return "BLDG_WIZARDS";
        case BLDG_PRIESTS_GUILD: return "BLDG_PRIESTS";
        case BLDG_HUNTERS_OFFICE: return "BLDG_HUNTERS";
        }
    }
    return "UNKNOWN";
}
static cptr _bldg_desc(dun_cell_ptr cell)
{
    if (bldg_is_shop(cell))
    {
        switch (shop_id(cell))
        {
        case SHOP_GENERAL: return "General Store";
        case SHOP_ARMORY: return "Armory";
        case SHOP_WEAPON: return "Weapon Smiths";
        case SHOP_TEMPLE: return "Temple";
        case SHOP_ALCHEMIST: return "Alchemist";
        case SHOP_MAGIC: return "Magic Shop";
        case SHOP_BLACK_MARKET: return "Black Market";
        case SHOP_HOME: return "Home";
        case SHOP_BOOK: return "Bookstore";
        case SHOP_MUSEUM: return "Museum";
        case SHOP_JEWELER: return "Jeweler";
        }
    }
    if (bldg_is_bldg(cell))
    {
        switch (bldg_id(cell))
        {
        case BLDG_INN: return "Inn";
        case BLDG_CASTLE: return "Castle";
        case BLDG_FIGHTERS_GUILD: return "Fighters' Hall";
        case BLDG_ARCHERS_GUILD: return "Archers' Guild";
        case BLDG_THIEVES_GUILD: return "Thieves' Guild";
        case BLDG_WIZARDS_GUILD: return "Wizards' Tower";
        case BLDG_PRIESTS_GUILD: return "Inner Temple";
        case BLDG_HUNTERS_OFFICE: return "Hunter's Office";
        }
    }
    return "something strange";
}
static int _bldg_priority(dun_cell_ptr cell) {
    if (bldg_is_shop(cell)) return 6 + (SHOP_COUNT - cell->parm1);
    return 6; }
static int _bldg_light(dun_cell_ptr cell) {
    if ( shop_is_home(cell)
      && (plr->prace == RACE_VAMPIRE || plr->prace == RACE_MON_VAMPIRE) )
    {
        return -5;
    }
    return 1; }
static bool _bldg_allow_plr(dun_cell_ptr cell) { return TRUE; }
static bool _bldg_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race) { return TRUE; }
static bool _bldg_allow_mon(dun_cell_ptr cell, mon_ptr mon) { return TRUE; }
static void _bldg_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (bldg_is_shop(cell))
    {
        int which = shop_id(cell);

        disturb(0, 0);

        if (which == SHOP_HOME) home_ui();
        else if (which == SHOP_MUSEUM) museum_ui();
        else
        {
            town_ptr town = towns_current_town();
            if (!town)
                msg_print("The shop is closed!"); /* XXX bug on D_SURFACE for ROOM_WILDERNESS */
            else
            {
                shop_ptr shop = town_get_shop(town, which);
                shop_ui(shop);
            }
        }
    }
    else if (bldg_is_bldg(cell))
    {
        int which = bldg_id(cell);
        town_ptr town = towns_current_town();
        bldg_ptr bldg = town_get_bldg(town, which);

        disturb(0, 0);
        bldg_ui(bldg);
    }
}
static dun_cell_type_ptr _bldg(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_BLDG);

    type->init = _bldg_init;
    type->name = _bldg_name;
    type->desc = _bldg_desc;
    type->priority = _bldg_priority;
    type->light = _bldg_light;

    type->allow_plr = _bldg_allow_plr;
    type->allow_mon = _bldg_allow_mon;
    type->allow_mon_race = _bldg_allow_mon_race;

    type->accept_plr = _bldg_accept_plr;

    return type;
}

/************************************************************************
 * Floor
 ************************************************************************/
enum {
    FLOOR_FLOOR,
    FLOOR_DIRT,
    FLOOR_GRASS,
    FLOOR_BRAKE,
    FLOOR_FLOWER,
    FLOOR_ROAD,
    FLOOR_COUNT
};
enum {
    /* at most one of the following */
    CELL_TRAP =     0x00010000, /* parm1 = TRAP_FOO, parm2 = difficulty */
    CELL_PLR_TRAP = 0x00020000, /* parm1 = PLR_TRAP_FOO, parm2 = plr->lev? */
    CELL_GLYPH =    0x00040000, /* parm1 = GLYPH_FOO */
    CELL_ILLUSION = 0x00080000, /* parm1 = illusion->type; parm2 = illusion->subtype */
   
    FLOOR_DIRTY =   0x000F0000, /* mask */
};
bool cell_is_floor(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return TRUE;
}
bool floor_is_floor(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return cell->subtype == FLOOR_FLOOR;
}
bool floor_is_dirt(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return cell->subtype == FLOOR_DIRT;
}
bool floor_is_grass(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return cell->subtype == FLOOR_GRASS;
}
bool floor_is_flower(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return cell->subtype == FLOOR_FLOWER;
}
bool floor_is_brake(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return cell->subtype == FLOOR_BRAKE;
}
bool floor_is_road(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return cell->subtype == FLOOR_ROAD;
}
static void _floor_flags(dun_cell_ptr cell);
/************************************************************************
 * Traps: Traps are encoded in cell->parm1 for FEAT_FLOOR whenever
 * CELL_TRAP is set. Thus, we only support 255 traps. parm2 is used
 * for trap difficulty for disarming. Only FEAT_FLOOR supports traps.
 ************************************************************************/
enum {
    TRAP_NONE = 0, /* needed for room_grid->feat_trap to indicate an untrapped feature */
    TRAP_TRAPDOOR,
    TRAP_SLEEP,
    TRAP_TRAPS,
    TRAP_ALARM,
    TRAP_PIT,
    TRAP_TELEPORT,
    TRAP_SLOW,
    TRAP_BLIND,
    TRAP_CONFUSE,
    TRAP_FIRE,
    TRAP_ACID,
    TRAP_POIS,
    TRAP_PIT_SPIKED,
    TRAP_PIT_POIS,
    TRAP_DEC_STR,
    TRAP_DEC_DEX,
    TRAP_DEC_CON,
    TRAP_TY_CURSE,
    TRAP_OPEN,
};
static void _remove_trap(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    cell->flags &= ~CELL_TRAP;
    cell->parm1 = 0;
    cell->parm2 = 0;
    dun_draw_pos(dun, pos); 
}
static int _trap_check_hit(int power)
{
    int k = randint0(100);
    int ac = plr->ac + plr->to_a;

    if (k < 10) return (k < 5);
    ac = plr->ac + plr->to_a;
    return _1d(power) > 3*ac/4;
}
static void _trap_trapdoor(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (plr->levitation)
    {
        msg_print("You fly over a trap door.");
        return;
    }
    msg_print("You have fallen through a trap door!");
    take_hit(DAMAGE_NOESCAPE, _2d(8), "a trap door");
    dun_trap_door_plr(dun);
}
static void _trap_pit(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (plr->levitation)
    {
        msg_print("You fly over a pit trap.");
        return;
    }
    msg_print("You have fallen into a pit!");
    take_hit(DAMAGE_NOESCAPE, _2d(6), "a pit trap");
}
static void _trap_pit_spiked(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (plr->levitation)
    {
        msg_print("You fly over a spiked pit.");
        return;
    }
    msg_print("You fall into a spiked pit!");
    if (one_in_(2))
    {
        msg_print("You are impaled!");
        take_hit(DAMAGE_NOESCAPE, _4d(6), "a spiked pit");
        if (!plr->no_cut) 
            plr_tim_add(T_CUT, _1d(15));
    }
    else
        take_hit(DAMAGE_NOESCAPE, _2d(6), "a pit trap");
}
static void _trap_pit_pois(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (plr->levitation)
    {
        msg_print("You fly over a spiked pit.");
        return;
    }
    msg_print("You fall into a spiked pit!");
    if (one_in_(2))
    {
        msg_print("You are impaled on poisonous spikes!");
        take_hit(DAMAGE_NOESCAPE, _4d(6), "a spiked pit");
        if (!plr->no_cut) 
            plr_tim_add(T_CUT, _1d(15));
        if (res_save_default(GF_POIS))
            msg_print("The poison does not affect you!");
        else
            plr_tim_add(T_POISON, _6d(6));
    }
    else
        take_hit(DAMAGE_NOESCAPE, _2d(6), "a pit trap");
}
static void _trap_traps(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("There is a bright flash of light!");
    _remove_trap(dun, pos, cell);
    dun_burst(dun, who_create_trap(pos), 1, pos, GF_MAKE_TRAP, 0);
}
static void _trap_ty_curse(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    int ct = 2 + _1d(3);
    int i;

    msg_print("There is a bright flash of light!");
    _remove_trap(dun, pos, cell);
    for (i = 0; i < ct; i++)
        summon_specific(who_create_null(), pos, dun->dun_lvl, 0, PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET);

    if (dun->dun_lvl > _1d(100)) /* No nasty effect for low levels */
    {
        bool stop_ty = FALSE;
        int count = 0;
        do {
            stop_ty = activate_ty_curse(stop_ty, &count);
        } while (one_in_(6));
    }
}
static void _trap_teleport(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("You hit a teleport trap!");
    teleport_player(100, TELEPORT_PASSIVE);
}
static void _trap_fire(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("You are enveloped in flames!");
    gf_affect_p(who_create_trap(pos), GF_FIRE, _4d(6), GF_AFFECT_TRAP);
}
static void _trap_acid(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("You are splashed with acid!");
    gf_affect_p(who_create_trap(pos), GF_ACID, _4d(6), GF_AFFECT_TRAP);
}
static void _trap_slow(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (!_trap_check_hit(125))
    {
        msg_print("A small dart barely misses you.");
        return;
    }
    msg_print("A small dart hits you!");
    take_hit(DAMAGE_ATTACK, _1d(4), "a dart trap");
    if (!free_act_save_p(dun->dun_lvl))
        plr_tim_add(T_SLOW, 20 + _1d(20));
}
static void _trap_dec_str(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (!_trap_check_hit(125))
    {
        msg_print("A small dart barely misses you.");
        return;
    }
    msg_print("A small dart hits you!");
    take_hit(DAMAGE_ATTACK, _1d(4), "a dart trap");
    do_dec_stat(A_STR);
}
static void _trap_dec_dex(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (!_trap_check_hit(125))
    {
        msg_print("A small dart barely misses you.");
        return;
    }
    msg_print("A small dart hits you!");
    take_hit(DAMAGE_ATTACK, _1d(4), "a dart trap");
    do_dec_stat(A_DEX);
}
static void _trap_dec_con(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (!_trap_check_hit(125))
    {
        msg_print("A small dart barely misses you.");
        return;
    }
    msg_print("A small dart hits you!");
    take_hit(DAMAGE_ATTACK, _1d(4), "a dart trap");
    do_dec_stat(A_CON);
}
static void _trap_blind(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("A black gas surrounds you!");
    if (!res_save_default(GF_BLIND))
        plr_tim_add(T_BLIND, 25 + _1d(50));
}
static void _trap_confuse(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("A gas of scintillating colors surrounds you!");
    if (!res_save_default(GF_CONFUSION))
        plr_tim_add(T_CONFUSED, 10 + _1d(20));
}
static void _trap_pois(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("A pungent green gas surrounds you!");
    if (!res_save_default(GF_POIS))
        plr_tim_add(T_POISON, 20 + _1d(30));
}
static void _trap_sleep(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("A strange white mist surrounds you!");
    if (!free_act_save_p(0))
        plr_tim_add(T_PARALYZED, _1d(4));
}
static void _trap_alarm(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("An alarm sounds!");
    aggravate_monsters(who_create_trap(pos));
}
static void _trap_open(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    msg_print("Your surrounding walls are suddenly opened!");
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_DISINTEGRATE, 0);
    aggravate_monsters(who_create_trap(pos));
}
typedef struct {
    int  id;
    cptr name;
    byte lvl;
    byte rarity;
    byte difficulty;
    byte color;
    cptr desc;
    void (*trap)(dun_ptr dun, point_t pos, dun_cell_ptr cell);
} _trap_t, *_trap_ptr;
static _trap_t _trap_tbl[] = {
    { TRAP_TRAPDOOR,  "TRAP_TRAPDOOR",    0,  3,   5, TERM_WHITE,   "trap door", _trap_trapdoor },
    { TRAP_SLEEP,     "TRAP_SLEEP",       0,  2,   5, TERM_GREEN,   "gas trap", _trap_sleep },
    { TRAP_TRAPS,     "TRAP_TRAPS",       0,  3,   5, TERM_L_DARK,  "compact rune", _trap_traps },
    { TRAP_ALARM,     "TRAP_ALARM",       0,  2,   5, TERM_L_RED,   "alarm", _trap_alarm },
    { TRAP_PIT,       "TRAP_PIT",         0,  3,   5, TERM_SLATE,   "pit", _trap_pit },
    { TRAP_TELEPORT,  "TRAP_TELEPORT",    0,  2,   5, TERM_ORANGE,  "strange rune", _trap_teleport },
    { TRAP_SLOW,      "TRAP_SLOW",        0,  2,   5, TERM_RED,     "dart trap", _trap_slow },
    { TRAP_BLIND,     "TRAP_BLIND",       0,  2,   5, TERM_GREEN,   "gas trap", _trap_blind },
    { TRAP_CONFUSE,   "TRAP_CONFUSE",     0,  2,   5, TERM_GREEN,   "gas trap", _trap_confuse },
    { TRAP_FIRE,      "TRAP_FIRE",        3,  3,   5, TERM_UMBER,   "discolored spot", _trap_fire },
    { TRAP_ACID,      "TRAP_ACID",        5,  3,   5, TERM_UMBER,   "discolored spot", _trap_acid },
    { TRAP_POIS,      "TRAP_POIS",        5,  3,   5, TERM_GREEN,   "gas trap", _trap_pois },
    { TRAP_PIT_SPIKED,"TRAP_PIT_SPIKED",  5,  5,   5, TERM_SLATE,   "pit", _trap_pit_spiked },
    { TRAP_PIT_POIS,  "TRAP_PIT_POIS",    7,  5,   5, TERM_SLATE,   "pit", _trap_pit_pois },
    { TRAP_DEC_STR,   "TRAP_DEC_STR",    15,  6,   5, TERM_RED,     "dart trap", _trap_dec_str },
    { TRAP_DEC_DEX,   "TRAP_DEC_DEX",    15,  6,   5, TERM_RED,     "dart trap", _trap_dec_dex },
    { TRAP_DEC_CON,   "TRAP_DEC_CON",    15,  6,   5, TERM_RED,     "dart trap", _trap_dec_con },
    { TRAP_TY_CURSE,  "TRAP_TY_CURSE",   30, 20, 100, TERM_L_GREEN, "evil rune", _trap_ty_curse },
    { TRAP_OPEN,      "TRAP_OPEN",       99,  0, 100, TERM_WHITE,   "wall opening trap", _trap_open },
    { 0 }
};
static _trap_ptr _trap_lookup(int id)
{
    int i;
    for (i = 0; ; i++)
    {
        _trap_ptr t = &_trap_tbl[i];
        if (!t->id) break;
        if (t->id == id) return t;
    }
    return NULL;
}
static cptr _trap_name(int id)
{
    _trap_ptr t = _trap_lookup(id);
    if (t) return t->name;
    return "UNKNOWN";
}
static cptr _trap_desc(int id)
{
    _trap_ptr t = _trap_lookup(id);
    if (t) return t->desc;
    return "unknown trap";
}
static int _trap_difficulty(int id)
{
    _trap_ptr t = _trap_lookup(id);
    if (t) return t->difficulty;
    return 0;
}
static _trap_ptr _trap_parse(cptr name)
{
    int i;
    for (i = 0; ; i++)
    {
        _trap_ptr t = &_trap_tbl[i];
        if (!t->id) break;
        if (strcmp(name, t->name) == 0) return t;
    }
    return NULL;
}
static int _random_trap(dun_ptr dun)
{
    int  tot = 0, i, roll;
    bool allow_down = TRUE;

    if (quests_get_current() || dun->dun_lvl >= dun->type->max_dun_lvl)
        allow_down = FALSE;
    if (dun->type->flags.info & DF_RANDOM)
        allow_down = FALSE;

    for (i = 0; ; i++)
    {
        _trap_ptr trap = &_trap_tbl[i];

        if (!trap->id) break;
        if (!trap->rarity) continue;
        if (trap->lvl > dun->dun_lvl) continue;
        if (!allow_down && trap->id == TRAP_TRAPDOOR) continue;

        tot += 100 / trap->rarity;
    }

    if (tot <= 0) return TRAP_SLEEP; /* impossible */
    roll = _1d(tot);

    for (i = 0; ; i++)
    {
        _trap_ptr trap = &_trap_tbl[i];

        if (!trap->id) break;
        if (!trap->rarity) continue;
        if (trap->lvl > dun->dun_lvl) continue;
        if (!allow_down && trap->id == TRAP_TRAPDOOR) continue;

        roll -= 100 / trap->rarity;
        if (roll <= 0)
            return trap->id;
    }
    return TRAP_SLEEP; /* unreachable */
}
static void _hit_trap(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    _trap_ptr trap;
    assert(cell->type == FEAT_FLOOR);
    assert(cell->flags & CELL_TRAP);
    disturb(0, 0);
    trap = _trap_lookup(cell->parm1);
    if (trap && trap->trap)
        trap->trap(dun, pos, cell);
}
static void _secret_trap(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    assert(floor_is_clean(cell));
    cell->flags |= CELL_TRAP;
    cell->flags |= CELL_SECRET;
    cell->parm1 = _random_trap(dun);
    cell->parm2 = _trap_difficulty(cell->parm1) + _1d(dun->difficulty/5);
    assert(cell->parm1 != TRAP_NONE);
    _floor_flags(cell);
}
bool dun_place_trap(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    _secret_trap(dun, pos, cell);
    return TRUE;
}
/************************************************************************
 * CELL_PLR_TRAP: Rogues may set traps that affect monsters.
 ************************************************************************/
enum {
    /* minor traps */
    PLR_TRAP_SLEEP = 1,
    PLR_TRAP_CONFUSE,
    PLR_TRAP_SLOW,
    PLR_TRAP_TELEPORT,
    PLR_TRAP_TRAPDOOR,
    PLR_TRAP_ARROW,

    /* major traps */
    PLR_TRAP_BOLT,
    PLR_TRAP_SOUND,
    PLR_TRAP_SHARDS,
    PLR_TRAP_ELEMENTAL,
    PLR_TRAP_DISINTEGRATE,
    PLR_TRAP_STASIS,
    PLR_TRAP_PIRANHA,

    /* ultimate traps */
    PLR_TRAP_BIRDS,
    PLR_TRAP_DEMONS,
    PLR_TRAP_ANGELS,
    PLR_TRAP_DRAGONS,
    PLR_TRAP_MANA_STORM,
    PLR_TRAP_ROCKET
};
enum {
    PLR_TRAP_MINOR,
    PLR_TRAP_MAJOR,
    PLR_TRAP_ULTIMATE
};
static cptr _mon_name(mon_ptr mon)
{
    static char name[MAX_NLEN];
    monster_desc(name, mon, 0);
    return name;
}
static void _plr_trap_sleep(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a small dart.", _mon_name(mon));
    gf_affect_m(who_create_trap(pos), mon, GF_SLEEP, parm2, GF_AFFECT_TRAP);
}
static void _plr_trap_confuse(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("A gas of scintillating colors surrounds %s!", _mon_name(mon));
    gf_affect_m(who_create_trap(pos), mon, GF_OLD_CONF, parm2, GF_AFFECT_TRAP);
}
static void _plr_trap_slow(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a small dart.", _mon_name(mon));
    gf_affect_m(who_create_trap(pos), mon, GF_SLOW, parm2, GF_AFFECT_TRAP);
}
static void _plr_trap_teleport(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (_1d(100) <= mon_res_pct(mon, GF_TELEPORT))
    {
        mon_lore_resist(mon, GF_TELEPORT);
        if (mon->ml) msg_format("%^s resists teleportation.", _mon_name(mon));
    }
    else
    {
        if (mon->ml) msg_format("%^s disappears.", _mon_name(mon));
        teleport_away(mon, 100, TELEPORT_PASSIVE);
    }
}
static void _plr_trap_trapdoor(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon_can_fly(mon))
    {
        if (mon->ml)
            msg_format("%^s flies over the trap door.", _mon_name(mon));
        /* XXX probably should not remove the trap then ... */
    }
    else
        dun_trap_door_mon(dun, mon);
}
static void _plr_trap_arrow(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml)
        msg_format("%^s is hit by an arrow.", _mon_name(mon));
    gf_affect_m(who_create_trap(pos), mon, GF_MISSILE, _5d(5) + parm2/2, GF_AFFECT_TRAP);
}
static void _plr_trap_bolt(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a seeker bolt.", _mon_name(mon));
    gf_affect_m(who_create_trap(pos), mon, GF_MISSILE, 4*(_6d(5) + parm2/2), GF_AFFECT_TRAP);
}
static void _plr_trap_sound(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a ball of sound.", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_SOUND, _10d(10) + parm2);
}
static void _plr_trap_shards(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a large rock.", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_SHARDS, _10d(10) + 3*parm2);
}
static void _plr_trap_elemental(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a shower of elements.", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_ACID, _7d(7) + parm2/2);
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_ELEC, _7d(7) + parm2/2);
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_FIRE, _7d(7) + parm2/2);
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_COLD, _7d(7) + parm2/2);
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_POIS, _7d(7) + parm2/2);
}
static void _plr_trap_disintegrate(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a disintegration ball.", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 5, pos, GF_DISINTEGRATE, _10d(10) + 3*parm2);
}
static void _plr_trap_stasis(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    gf_affect_m(who_create_trap(pos), mon, GF_STASIS, 4*parm2, GF_AFFECT_TRAP);
}
static void _plr_trap_piranha(dun_ptr dun, point_t pos, int parm2)
{
    int i, num;
    if (plr_can_see(pos))
        msg_print("Your trap explodes and suddenly the room is filled with water and piranhas!");
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_DISINTEGRATE, 100);
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_WATER_FLOW, 1);
    num = 1 + parm2/10;
    for (i = 0; i < num; i++)
        summon_specific(who_create_null(), pos, parm2 * 2, SUMMON_PIRANHA, PM_ALLOW_GROUP | PM_FORCE_PET);
}
static void _plr_trap_birds(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    int i, num;
    if (plr_can_see(pos))
        msg_format("Your trap explodes and suddenly a storm of birds swirls around %s!", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_DISINTEGRATE, 100);

    num = 1 + parm2/10;
    for (i = 0; i < num; i++)
        summon_specific(who_create_null(), pos, parm2 * 2, SUMMON_BIRD, PM_ALLOW_GROUP | PM_FORCE_PET);
}
static void _plr_trap_demons(dun_ptr dun, point_t pos, int parm2)
{
    int i, num;
    if (plr_can_see(pos))
        msg_print("Your trap explodes and suddenly the room is filled with fire and brimstone!");
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_DISINTEGRATE, 100);
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_LAVA_FLOW, 1);

    num = 1 + parm2/10;
    for (i = 0; i < num; i++)
        summon_specific(who_create_null(), pos, parm2 * 2, SUMMON_DEMON, PM_ALLOW_GROUP | PM_FORCE_PET);
}
static void _plr_trap_angels(dun_ptr dun, point_t pos, int parm2)
{
    int i, num;
    if (plr_can_see(pos))
        msg_print("Your trap explodes and suddenly the room is filled with a heavenly choir singing!");
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_DISINTEGRATE, 100);

    num = 1 + parm2/10;
    for (i = 0; i < num; i++)
        summon_specific(who_create_null(), pos, parm2 * 2, SUMMON_ANGEL, PM_ALLOW_GROUP | PM_FORCE_PET);
}
static void _plr_trap_dragons(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    int i, num;
    if (plr_can_see(pos))
        msg_format("Your trap explodes and suddenly ancient dragons surround %s!", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 10, pos, GF_DISINTEGRATE, 100);

    num = 1 + parm2/10;
    for (i = 0; i < num; i++)
        summon_specific(who_create_null(), pos, parm2 * 2, SUMMON_HI_DRAGON, PM_ALLOW_GROUP | PM_FORCE_PET);
}
static void _plr_trap_mana_storm(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a mana storm.", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 5, pos, GF_MANA, 400);
}
static void _plr_trap_rocket(dun_ptr dun, point_t pos, int parm2)
{
    mon_ptr mon = dun_mon_at(dun, pos);
    if (mon->ml) msg_format("%^s is hit by a Rocket.", _mon_name(mon));
    dun_burst(dun, who_create_trap(pos), 2, pos, GF_SHARDS, 400);
}
typedef struct {
    int  id;
    cptr name;
    byte kind; /* minor, major, ultimate */
    byte rarity;
    byte difficulty;
    byte color;
    cptr desc;
    void (*trap)(dun_ptr dun, point_t pos, int parm2);
} _plr_trap_t, *_plr_trap_ptr;
static _plr_trap_t _plr_trap_tbl[] = {
    /* minor traps */
    { PLR_TRAP_SLEEP, "PLR_TRAP_SLEEP", PLR_TRAP_MINOR, 2, 5, TERM_RED, "dart trap", _plr_trap_sleep },
    { PLR_TRAP_CONFUSE, "PLR_TRAP_CONFUSE", PLR_TRAP_MINOR, 2, 5, TERM_GREEN, "gas trap", _plr_trap_confuse },
    { PLR_TRAP_SLOW, "PLR_TRAP_SLOW", PLR_TRAP_MINOR, 2, 5, TERM_RED, "dart trap", _plr_trap_slow },
    { PLR_TRAP_TELEPORT, "PLR_TRAP_TELEPORT", PLR_TRAP_MINOR, 3, 5, TERM_ORANGE, "strange rune", _plr_trap_teleport },
    { PLR_TRAP_TRAPDOOR, "PLR_TRAP_TRAPDOOR", PLR_TRAP_MINOR, 5, 5, TERM_WHITE, "trap door", _plr_trap_trapdoor },
    { PLR_TRAP_ARROW, "PLR_TRAP_ARROW", PLR_TRAP_MINOR, 2, 5, TERM_L_BLUE, "arrow trap", _plr_trap_arrow },

    /* major traps */
    { PLR_TRAP_BOLT, "PLR_TRAP_BOLT",  PLR_TRAP_MAJOR, 2, 10, TERM_RED, "seeker bolt trap", _plr_trap_bolt },
    { PLR_TRAP_SOUND, "PLR_TRAP_SOUND", PLR_TRAP_MAJOR, 4, 10, TERM_ORANGE, "explosive rune", _plr_trap_sound },
    { PLR_TRAP_SHARDS, "PLR_TRAP_SHARDS", PLR_TRAP_MAJOR, 5, 10, TERM_L_UMBER, "explosive rune", _plr_trap_shards },
    { PLR_TRAP_ELEMENTAL, "PLR_TRAP_ELEMENTAL", PLR_TRAP_MAJOR, 4, 10, TERM_L_GREEN, "explosive rune", _plr_trap_elemental },
    { PLR_TRAP_DISINTEGRATE, "PLR_TRAP_DISINTEGRATE", PLR_TRAP_MAJOR, 7, 10, TERM_L_DARK, "explosive rune", _plr_trap_disintegrate },
    { PLR_TRAP_STASIS, "PLR_TRAP_STASIS", PLR_TRAP_MAJOR, 5, 10, TERM_UMBER, "dart trap", _plr_trap_stasis },
    { PLR_TRAP_PIRANHA, "PLR_TRAP_PIRANHA", PLR_TRAP_MAJOR, 3, 10, TERM_BLUE, "concealed portal", _plr_trap_piranha },

    /* ultimate traps */
    { PLR_TRAP_BIRDS, "PLR_TRAP_BIRDS", PLR_TRAP_ULTIMATE, 2, 25, TERM_YELLOW, "concealed portal", _plr_trap_birds },
    { PLR_TRAP_DEMONS, "PLR_TRAP_DEMONS", PLR_TRAP_ULTIMATE, 3, 25, TERM_RED, "concealed portal", _plr_trap_demons },
    { PLR_TRAP_ANGELS, "PLR_TRAP_ANGELS", PLR_TRAP_ULTIMATE, 3, 25, TERM_WHITE, "concealed portal", _plr_trap_angels },
    { PLR_TRAP_DRAGONS, "PLR_TRAP_DRAGONS", PLR_TRAP_ULTIMATE, 2, 25, TERM_GREEN, "concealed portal", _plr_trap_dragons },
    { PLR_TRAP_MANA_STORM, "PLR_TRAP_MANA_STORM", PLR_TRAP_ULTIMATE, 5, 25, TERM_L_BLUE, "explosive rune", _plr_trap_mana_storm },
    { PLR_TRAP_ROCKET, "PLR_TRAP_ROCKET", PLR_TRAP_ULTIMATE, 4, 25, TERM_L_UMBER, "concealed rocket launcher", _plr_trap_rocket },
    { 0 }
};
static _plr_trap_ptr _plr_trap_lookup(int id)
{
    int i;
    for (i = 0; ; i++)
    {
        _plr_trap_ptr t = &_plr_trap_tbl[i];
        if (!t->id) break;
        if (t->id == id) return t;
    }
    return NULL;
}
static cptr _plr_trap_name(int id)
{
    _plr_trap_ptr t = _plr_trap_lookup(id);
    if (t) return t->name;
    return "UNKNOWN";
}
static cptr _plr_trap_desc(int id)
{
    _plr_trap_ptr t = _plr_trap_lookup(id);
    if (t) return t->desc;
    return "unknown trap";
}
static int _plr_trap_difficulty(int id)
{
    _plr_trap_ptr t = _plr_trap_lookup(id);
    if (t) return t->difficulty;
    return 0;
}
static void _remove_plr_trap(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    cell->flags &= ~CELL_PLR_TRAP;
    cell->parm1 = 0;
    cell->parm2 = 0;
    cell->flags &= ~CELL_MAP;
    _floor_flags(cell);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos); 
}
void dun_remove_plr_trap(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    if (!floor_has_plr_trap(cell)) return;
    _remove_plr_trap(dun, pos, cell);
}
static void _hit_plr_trap(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    _plr_trap_ptr trap;
    int parm2 = cell->parm2;

    assert(floor_has_plr_trap(cell));

    trap = _plr_trap_lookup(cell->parm1);
    /* remove first: we can't really say what trap->trap might do. perhaps
     * it will do something and then set another trap? unlikely, but often
     * it will alter cell, either via disintegrate, or lava flow, or water
     * flow ... so, by the time trap->trap returns, we might not no longer
     * be a floor_[that_]has_plr_trap ... */
    _remove_plr_trap(dun, pos, cell);
    if (trap && trap->trap)
        trap->trap(dun, pos, parm2);
}
bool mon_disarm_plr_trap(mon_ptr mon, dun_cell_ptr cell)
{
    int difficulty;
    if (!floor_has_plr_trap(cell)) return FALSE;
    if (mon_is_pet(mon)) return FALSE;

    difficulty = _plr_trap_difficulty(cell->parm1);
    if (_1d(BREAK_MON_TRAP * difficulty / 50) < mon_lvl(mon))
        return TRUE;
    return FALSE;
}
static int _random_plr_trap(dun_ptr dun, int kind)
{
    int  tot = 0, i, roll;
    bool allow_down = TRUE;

    if (quests_get_current() || dun->dun_lvl >= dun->type->max_dun_lvl)
        allow_down = FALSE;

    for (i = 0; ; i++)
    {
        _plr_trap_ptr trap = &_plr_trap_tbl[i];

        if (!trap->id) break;
        if (trap->kind != kind) continue;
        if (!trap->rarity) continue;
        if (!allow_down && trap->id == PLR_TRAP_TRAPDOOR) continue;

        tot += 100 / trap->rarity;
    }

    if (tot <= 0) return PLR_TRAP_SLEEP; /* impossible */
    roll = _1d(tot);

    for (i = 0; ; i++)
    {
        _plr_trap_ptr trap = &_plr_trap_tbl[i];

        if (!trap->id) break;
        if (trap->kind != kind) continue;
        if (!trap->rarity) continue;
        if (!allow_down && trap->id == PLR_TRAP_TRAPDOOR) continue;

        roll -= 100 / trap->rarity;
        if (roll <= 0)
            return trap->id;
    }
    return PLR_TRAP_SLEEP; /* unreachable */
}
static void _plr_trap(dun_ptr dun, point_t pos, dun_cell_ptr cell, int kind)
{
    assert(floor_is_clean(cell));
    cell->flags |= CELL_PLR_TRAP;
    cell->parm1 = _random_plr_trap(dun, kind);
    cell->parm2 = _plr_trap_difficulty(cell->parm1) + plr->lev;
    assert(cell->parm1);
    _floor_flags(cell);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
}
bool dun_place_plr_trap_minor(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    _plr_trap(dun, pos, cell, PLR_TRAP_MINOR);
    return TRUE;
}
bool dun_place_plr_trap_major(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    _plr_trap(dun, pos, cell, PLR_TRAP_MAJOR);
    return TRUE;
}
bool dun_place_plr_trap_ultimate(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    _plr_trap(dun, pos, cell, PLR_TRAP_ULTIMATE);
    return TRUE;
}
/************************************************************************
 * Glyphs: Glyphs are encoded in cell->parm1 for FEAT_FLOOR whenever
 * CELL_GLYPH is set. 
 ************************************************************************/
enum {
    GLYPH_WARDING,
    GLYPH_EXPLODING,
    GLYPH_MIRROR,
    GLYPH_WEB,
    GLYPH_COUNT
};
static _info_t _glyph_tbl[GLYPH_COUNT] = {
    { "GLYPH_WARDING", "glyph of warding", {';', TERM_YELLOW} },
    { "GLYPH_EXPLODING", "explosive rune", {';', TERM_L_RED} },
    { "MIRROR", "mirror", {'*', TERM_VIOLET} },
    { "WEB", "web", {'~', TERM_SLATE} },
};

/************************************************************************
 * Floor (continued)
 ************************************************************************/

bool floor_is_clean(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    return !(cell->flags & FLOOR_DIRTY);
}
bool floor_has_object(dun_cell_ptr cell)
{
    /* this is the old CAVE_OBJECT flag: includes plr traps (rogues),
     * glyphs of warding, mirrors (mirror-master) and explosive runes (half-ogre)
     * CAVE_OBJECT implies a 'floor' grid */
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (cell->flags & CELL_PLR_TRAP) return TRUE;
    if (!(cell->flags & CELL_GLYPH)) return FALSE;
    if (cell->parm1 == GLYPH_WEB) return FALSE;
    return TRUE;
}
bool floor_has_web(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_GLYPH)) return FALSE;
    return cell->parm1 == GLYPH_WEB;
}
bool floor_has_trap(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_TRAP)) return FALSE;
    return TRUE;
}
bool floor_has_plr_trap(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_PLR_TRAP)) return FALSE;
    return TRUE;
}
bool floor_has_trapdoor(dun_cell_ptr cell)
{
    if (!floor_has_trap(cell)) return FALSE;
    return cell->parm1 == TRAP_TRAPDOOR;
}
bool floor_has_known_trap(dun_cell_ptr cell)
{
    if (!floor_has_trap(cell)) return FALSE;
    return !(cell->flags & CELL_SECRET);
}
bool floor_has_secret_trap(dun_cell_ptr cell)
{
    if (!floor_has_trap(cell)) return FALSE;
    return BOOL(cell->flags & CELL_SECRET);
}
bool plr_can_ignore_trap(dun_cell_ptr cell)
{
    if (!floor_has_trap(cell)) return FALSE;
    if (cell->flags & CELL_SECRET) return TRUE;
    switch (cell->parm1)
    {
    case TRAP_TRAPDOOR:
    case TRAP_PIT:
    case TRAP_PIT_SPIKED:
    case TRAP_PIT_POIS:
        if (plr->levitation) return TRUE;
        break;
    case TRAP_TELEPORT:
        if (plr->anti_tele) return TRUE;
        break;
    case TRAP_FIRE:
        if (res_can_ignore(GF_FIRE)) return TRUE;
        break;
    case TRAP_ACID: /* Note: Your armor still gets messed up even if your pack is safe! */
        if (res_pct(GF_ACID) >= 100) return TRUE;
        break;
    case TRAP_BLIND:
        if (res_can_ignore(GF_BLIND)) return TRUE;
        break;
    case TRAP_CONFUSE:
        if (res_can_ignore(GF_CONFUSION)) return TRUE;
        break;
    case TRAP_POIS:
        if (res_can_ignore(GF_POIS)) return TRUE;
        break;
    }
    return FALSE;
}
bool floor_has_mirror(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_GLYPH)) return FALSE;
    return cell->parm1 == GLYPH_MIRROR;
}
bool floor_has_glyph_of_warding(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_GLYPH)) return FALSE;
    return cell->parm1 == GLYPH_WARDING;
}
bool floor_has_explosive_rune(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_GLYPH)) return FALSE;
    return cell->parm1 == GLYPH_EXPLODING;
}
bool floor_has_illusion(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    if (!(cell->flags & CELL_ILLUSION)) return FALSE;
    return TRUE;
}
void floor_remove_mirror(dun_cell_ptr cell)
{
    if (!floor_has_mirror(cell)) return;
    cell->flags &= ~CELL_GLYPH;
    cell->parm1 = 0;
    cell->parm2 = 0;
    _floor_flags(cell);
}
void floor_remove_trap(dun_cell_ptr cell)
{
    if (!floor_has_trap(cell)) return;
    cell->flags &= ~(CELL_TRAP | CELL_SECRET);
    cell->parm1 = 0;
    cell->parm2 = 0;
    _floor_flags(cell);
}
void floor_remove_glyph(dun_cell_ptr cell)
{
    if (!(cell->flags & CELL_GLYPH)) return;
    cell->flags &= ~CELL_GLYPH;
    cell->parm1 = 0;
    cell->parm2 = 0;
    _floor_flags(cell);
}
void floor_remove_web(dun_cell_ptr cell)
{
    if (!floor_has_web(cell)) return;
    cell->flags &= ~CELL_GLYPH;
    cell->parm1 = 0;
    cell->parm2 = 0;
    _floor_flags(cell);
}
void floor_remove_illusion(dun_cell_ptr cell)
{
    if (!floor_has_illusion(cell)) return;
    cell->flags &= ~CELL_ILLUSION;
    cell->parm1 = 0;
    cell->parm2 = 0;
    _floor_flags(cell);
}

static _info_t _floor_tbl[FLOOR_COUNT] = {
    { "FLOOR", "floor", {'.', TERM_WHITE} },
    { "DIRT", "dirt", {'.', TERM_UMBER} },
    { "GRASS", "grass", {'.', TERM_GREEN} },
    { "BRAKE", "brake", {':', TERM_GREEN} },
    { "FLOWER", "flower", {':', TERM_L_GREEN} },
    { "ROAD", "road", {'.', TERM_ORANGE} },
};

static void _floor_init(void)
{
    int i;
    for (i = 0; i < FLOOR_COUNT; i++)
    {
        _info_ptr info = &_floor_tbl[i];
        _display_add(info->name, info->display);
    }
    for (i = 0; ; i++)
    {
        _trap_ptr trap = &_trap_tbl[i];
        term_char_t tc;

        if (!trap->id) break;
        tc.a = trap->color;
        tc.c = '^';
        _display_add_no_light(trap->name, tc);
    }
    for (i = 0; ; i++)
    {
        _plr_trap_ptr trap = &_plr_trap_tbl[i];
        term_char_t tc;

        if (!trap->id) break;
        tc.a = trap->color;
        tc.c = ';';
        _display_add_no_light(trap->name, tc);
    }
    for (i = 0; i < GLYPH_COUNT; i++)
    {
        _info_ptr info = &_glyph_tbl[i];
        _display_add_no_light(info->name, info->display);
    }
}
static cptr _floor_name(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_FLOOR);
    assert(cell->subtype < FLOOR_COUNT);
    return _floor_tbl[cell->subtype].name;
}
static cptr _floor_desc(dun_cell_ptr cell)
{
    assert(cell->type == FEAT_FLOOR);
    assert(cell->subtype < FLOOR_COUNT);
    if (floor_has_trap(cell))
        return _trap_desc(cell->parm1);
    if (floor_has_plr_trap(cell))
        return _plr_trap_desc(cell->parm1);
    if (cell->flags & CELL_GLYPH)
        return _glyph_tbl[cell->parm1].desc;
    return _floor_tbl[cell->subtype].desc;
}
static int _floor_priority(dun_cell_ptr cell)
{
    if (floor_has_glyph_of_warding(cell)) return 16;
    if (floor_has_explosive_rune(cell)) return 16;
    if (floor_has_mirror(cell)) return 16;
    if (floor_is_road(cell)) return 8;
    return 2;
}
static dun_cell_t _illusion(dun_cell_ptr cell);
static void _floor_display(dun_cell_ptr cell, int light, map_char_ptr mc)
{
    term_char_t tc;
    assert(cell->type == FEAT_FLOOR);

    tc = visual_get(_floor_tbl[cell->subtype].name, light);
    map_char_push(mc, tc);

    if (floor_has_illusion(cell))
    {
        dun_cell_t fake = _illusion(cell);
        cell_display(&fake, light, mc);
    }
    else if (cell->flags & CELL_TRAP)
    {
        if (!(cell->flags & CELL_SECRET))
        {
            cptr name = _trap_name(cell->parm1);
            tc = visual_get(name, light);
            map_char_push(mc, tc);
        }
    }
    else if (cell->flags & CELL_GLYPH)
    {
        cptr name = _glyph_tbl[cell->parm1].name;
        tc = visual_get(name, light);
        map_char_push(mc, tc);
        /* Use the web tile as background in case there is a proper foreground
         * tile (monster or object). Give visual indication when monsters are stuck!
         * This actually works better for Glyphs of Warding as well.
        if (cell->parm1 == GLYPH_WEB) */
            mc->background = mc->count - 1;
    }
    else if (cell->flags & CELL_PLR_TRAP)
    {
        cptr name = _plr_trap_name(cell->parm1);
        tc = visual_get(name, light);
        map_char_push(mc, tc);
    }
}
static int _floor_search(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);
    int skill;

    assert(cell->type == FEAT_FLOOR);
    if (!(cell->flags & CELL_SECRET)) return ACTION_CONTINUE; /* don't leak info */
    assert(cell->flags & CELL_TRAP); /* only traps are secret atm */

    skill = plr_skill(plr->skills.srh);
    if (force || _1d(100) <= skill)
    {
        cell->flags &= ~CELL_SECRET;
        dun_draw_pos(dun, pos);
        if (!quiet) msg_print("You have found a trap.");
        disturb(0, 0);
        return ACTION_SUCCESS;
    }
    return ACTION_CONTINUE;
}
static bool _floor_detect(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    if (floor_has_secret_trap(cell))
    {
        cell->flags &= ~CELL_SECRET;
        dun_draw_pos(dun, pos);
        return TRUE;
    }
    return FALSE;
}
bool dun_place_mirror(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    cell->flags |= CELL_GLYPH;
    cell->parm1 = GLYPH_MIRROR;
    cell->parm2 = 0;
    _floor_flags(cell);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
    return TRUE;
}
bool dun_place_glyph_of_warding(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    cell->flags |= CELL_GLYPH;
    cell->parm1 = GLYPH_WARDING;
    cell->parm2 = 0;
    _floor_flags(cell);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
    return TRUE;
}
bool dun_place_explosive_rune(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    cell->flags |= CELL_GLYPH;
    cell->parm1 = GLYPH_EXPLODING;
    cell->parm2 = 0;
    _floor_flags(cell);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
    return TRUE;
}
static void _make_web(dun_cell_ptr cell)
{
    assert(floor_is_clean(cell));
    cell->flags |= CELL_GLYPH;
    cell->parm1 = GLYPH_WEB;
    cell->parm2 = _1d(10); /* stickiness */
    _floor_flags(cell);
}
bool dun_place_web(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (!floor_is_clean(cell)) return FALSE;
    _make_web(cell);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
    return TRUE;
}
void dun_remove_mirror(dun_ptr dun, point_t pos)
{
    dun_grid_ptr g = dun_grid_at(dun, pos);
    floor_remove_mirror(g);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
}
void dun_remove_glyph(dun_ptr dun, point_t pos)
{
    dun_grid_ptr g = dun_grid_at(dun, pos);
    floor_remove_glyph(g);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
}
static int _floor_disarm(dun_ptr dun, point_t pos, dun_cell_ptr cell, u32b options)
{
    bool quiet = BOOL(options & ACTION_QUIET);
    bool force = BOOL(options & ACTION_FORCE);
    int skill, difficulty, chance;
    if (!(cell->flags & CELL_TRAP) || (cell->flags & CELL_SECRET))
    {
        if (!quiet) msg_print("You see nothing there to disarm.");
        return ACTION_ABORT; /* don't even try */
    }
    skill = plr_skill(plr->skills.dis);
    difficulty = cell->parm2;
    chance = MAX(2, skill - difficulty);
    #if DEVELOPER
    if (0)
        msg_format("<color:D>Disarm Trap: %d%%</color>", chance);
    #endif
    if (force || _1d(100) <= chance)
    {
        if (!quiet) msg_format("You have disarmed the %s.", _trap_desc(cell->parm1));
        if (!force) gain_exp(difficulty);
        cell->flags &= ~CELL_TRAP;
        cell->parm1 = 0;
        cell->parm2 = 0;
        dun_draw_pos(dun, pos);
        return ACTION_SUCCESS;
    }
    else if (skill > 5 && _1d(skill) > 5)
    {
        if (flush_failure) flush();
        if (!quiet) msg_format("You failed to disarm the %s.", _trap_desc(cell->parm1));
        return ACTION_CONTINUE;
    }
    if (!quiet) msg_format("You set off the %s.", _trap_desc(cell->parm1));
    /* XXX hit trap now? previously, this did a move_player ... */
    return ACTION_FAIL;
}
static bool _floor_affect(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power)
{
    bool notice = FALSE;
    bool seen = plr_can_see(pos);

    /* Nerf "Wall of Illusion". Plr can repeatedly project thru the illusion,
     * but monsters don't know what is going on. */
    if (floor_has_illusion(cell))
    {
        gf_info_ptr gfi = gf_lookup(gf);
        /* all illusions are timed dungeon effects; remove the timer to dispel the illusion */
        if ((gfi->flags & GFF_ELEMENTAL) && (gfi->flags & GFF_DAMAGE) && one_in_(2))
            dun_tim_remove_at(dun, pos, DT_ILLUSION);
    }

    /* destroy webs */
    if (floor_has_web(cell))
    {
        cptr message = NULL;
        switch (gf)
        {
        case GF_FIRE:
        case GF_ELEC:
        case GF_PLASMA:
            message = "burns up.";
            break;
        case GF_METEOR:
        case GF_CHAOS:
        case GF_MANA:
        case GF_SHARDS:
        case GF_ROCK:
        case GF_ROCKET:
        case GF_FORCE:
            message = "is blasted.";
            break;
        case GF_ACID:
            message = "melts.";
            break;
        case GF_COLD:
        case GF_ICE:
            message = "is frozen.";
            break;
        case GF_PSY_SPEAR:
        case GF_LIGHT:
            message = "shrivels in the light.";
            break;
        case GF_STORM:
        case GF_WATER:
        case GF_WATER2:
            message = "is washed away.";
            break;
        case GF_GRAVITY:
            message = "disappears.";
            break;
        }
        if (message)
        {
            if (seen) msg_format("A web %s", message);
            if (cell->flags & CELL_MAP) notice = TRUE;
            floor_remove_web(cell);
            dun_draw_pos(dun, pos);
        }
    }
    switch (gf)
    {
    case GF_KILL_TRAP:
    case GF_KILL_DOOR:
    case GF_REMOVE_OBSTACLE:
        if (cell->flags & CELL_TRAP)
        {
            if (seen) msg_print("There is a bright flash of light!");
            floor_remove_trap(cell);
            dun_draw_pos(dun, pos);
        }
        if (seen)
        {
            cell->flags &= ~CELL_UNSAFE;
            notice = TRUE;
        }
        break;
    case GF_MAKE_DOOR:
        if (!floor_is_clean(cell)) break;
        if (dun_obj_at(dun, pos)) break;
        if (dun_mon_at(dun, pos)) break;
        if (dun_plr_at(dun, pos)) break;

        cell_make_closed_door(cell);
        dun_note_pos(dun, pos);
        dun_draw_pos(dun, pos);

        if (cell->flags & CELL_MAP) notice = TRUE;
        plr->redraw |= PR_MAP;
        break;
    case GF_MAKE_TRAP:
        if (!floor_is_clean(cell)) break;
        if (dun_obj_at(dun, pos)) break;
        _secret_trap(dun, pos, cell);
        break;
    case GF_WEB:
        if (!floor_is_clean(cell)) break;
     /* if (dun_obj_at(dun, pos)) break; cf _place_web */
        if (dun_mon_at(dun, pos)) break;

        _make_web(cell);
        dun_note_pos(dun, pos);
        dun_draw_pos(dun, pos);

        if (cell->flags & CELL_MAP) notice = TRUE;
        break;
    case GF_MAKE_TREE:
        if (!floor_is_clean(cell)) break;
        if (dun_obj_at(dun, pos)) break;
        if (dun_mon_at(dun, pos)) break;
        if (dun_plr_at(dun, pos)) break;

        dun_place_tree(dun, pos);
        if (cell->flags & CELL_MAP) notice = TRUE;
        break;
    case GF_MAKE_GLYPH:
        if (!floor_is_clean(cell)) break;
        if (dun_obj_at(dun, pos)) break;
        if (dun_mon_at(dun, pos)) break;

        cell->flags |= CELL_GLYPH;
        cell->parm1 = GLYPH_WARDING;
        cell->parm2 = 0;
        _floor_flags(cell);
        dun_note_pos(dun, pos);
        dun_draw_pos(dun, pos);
        break;
    case GF_MAKE_WALL:
        if (!floor_is_clean(cell)) break;
        if (dun_obj_at(dun, pos)) break;
        if (dun_mon_at(dun, pos)) break;
        if (dun_plr_at(dun, pos)) break;

        dun_place_granite(dun, pos);
        if (cell->flags & CELL_MAP) notice = TRUE;
        plr->update |= PU_FLOW;
        plr->redraw |= PR_MAP;
        break;
    case GF_SHARDS:
    case GF_ROCK:
    case GF_ROCKET:
    case GF_SOUND:
        if (!floor_has_mirror(cell)) break;
        if (gf == GF_SOUND && plr->lev >= 40) break; /* XXX don't understand this one ... */
        if (seen)
        {
            msg_print("The mirror shatters!");
            notice = TRUE;
        }
        floor_remove_mirror(cell);
        dun_note_pos(dun, pos);
        dun_draw_pos(dun, pos);
        dun_burst(dun, who_create_mirror(pos), 2, pos, GF_SHARDS, 5 + plr->lev/2);
        break;
    case GF_DISINTEGRATE:
        if (!(cell->flags & (CELL_GLYPH | CELL_PLR_TRAP))) break;
        cell->flags &= ~(CELL_GLYPH | CELL_PLR_TRAP);
        cell->parm1 = 0;
        cell->parm2 = 0;
        _floor_flags(cell);
        dun_note_pos(dun, pos);
        dun_draw_pos(dun, pos);
        if (seen) notice = TRUE;
        break;
    }
    return notice;
}
static bool _floor_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race)
{
    return TRUE;
}
static bool _floor_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    return TRUE;
}
static bool _floor_allow_plr(dun_cell_ptr cell)
{
    return TRUE;
}
static bool _floor_allow_obj(dun_cell_ptr cell)
{
    if (floor_has_web(cell)) return TRUE;
    if (!floor_is_clean(cell)) return FALSE;
    return TRUE;
}
static void _floor_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    assert(dun_plr_at(dun, pos)); /* XXX still working out design ... */
    if (cell->flags & CELL_TRAP)
    {
        if (cell->flags & CELL_SECRET)
        {
            msg_print("You found a trap!");
            cell->flags &= ~CELL_SECRET;
            dun_draw_pos(dun, pos);
        }
        _hit_trap(dun, pos, cell);
        if (!dun_plr_at(dun, pos) || plr->is_dead) return;
    }
}
static void _floor_accept_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon)
{
    if (mon_is_pet(mon)) return;
    if (floor_has_plr_trap(cell))
        _hit_plr_trap(dun, pos, cell);
    else if (floor_has_explosive_rune(cell))
    {
        msg_print("The rune explodes!");
        dun_burst(dun, who_create_trap(pos), 2, pos, GF_MANA, _7d(7) + plr->lev);
        dun_remove_glyph(dun, pos);
    }
    else if (floor_has_web(cell))
    {
        /* Some game mechanics work better if monsters get stuck immediately
         * upon entering webbed terrain. cf _stuck and plr_check_hit */
        mon_aura_ptr aura = mon_auras_find(mon->race, GF_FIRE);
        if (aura)
        {
            floor_remove_web(cell);
            dun_note_pos(dun, mon->pos);
            dun_draw_pos(dun, mon->pos);
            mon_lore_aura(mon, aura);
        }
        else if (mon_can_tunnel(mon))
        {
            floor_remove_web(cell);
            dun_note_pos(dun, pos);
            dun_draw_pos(dun, pos);
        }
        else if (mon_can_passweb(mon))
            mon_lore_passweb(mon);
        else
            mon->mflag2 |= MFLAG2_WEB; 
        /* XXX mon_can_clearweb requires energy and should not be applied on this turn */
    }
}
static void _floor_flags(dun_cell_ptr cell)
{
    bool old_los = BOOL(cell->flags & CELL_LOS);

    if (floor_has_web(cell))
        cell->flags &= ~(CELL_LOS | CELL_PROJECT);
    else
        cell->flags |= CELL_LOS | CELL_PROJECT;

    if (old_los != BOOL(cell->flags & CELL_LOS))
    {
        plr->update |= PU_VIEW | PU_LIGHT | PU_MON_LIGHT | PU_MONSTERS;
    }
}
static dun_cell_type_ptr _floor(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_FLOOR);

    type->init = _floor_init;
    type->name = _floor_name;
    type->desc = _floor_desc;
    type->priority = _floor_priority;
    type->display = _floor_display;

    type->search = _floor_search;
    type->disarm = _floor_disarm;
    type->affect = _floor_affect;

    type->detect = _floor_detect;

    type->allow_plr = _floor_allow_plr;
    type->allow_mon = _floor_allow_mon;
    type->allow_mon_race = _floor_allow_mon_race;
    type->allow_obj = _floor_allow_obj;

    type->accept_plr = _floor_accept_plr;
    type->accept_mon = _floor_accept_mon;
    return type;
}

/* illusions fool monsters during ai processing. code that wants
 * to respect illusions should call the following: */
static dun_cell_t _illusion(dun_cell_ptr cell)
{
    dun_cell_t fake = {0};
    assert(floor_has_illusion(cell));
    fake.type = cell->parm1;
    fake.subtype = cell->parm2;
    fake.flags = cell->flags & CELL_LO_MASK;
    fake.flags &= CELL_CHANGE_MASK;
    /* XXX only WALL illusions are currently supported */
    if (fake.type == FEAT_WALL)
        _wall_flags(&fake);
    return fake;
}
bool illusion_los(dun_cell_ptr cell)
{
    dun_cell_t fake;
    if (!floor_has_illusion(cell)) return cell_los(cell);
    fake = _illusion(cell);
    return cell_los(&fake);
}
bool illusion_project(dun_cell_ptr cell)
{
    dun_cell_t fake;
    if (!floor_has_illusion(cell)) return cell_project(cell);
    fake = _illusion(cell);
    return cell_project(&fake);
}
bool illusion_allow_mon(dun_cell_ptr cell, mon_ptr mon)
{
    dun_cell_t fake;
    if (!floor_has_illusion(cell)) return cell_allow_mon(cell, mon);
    fake = _illusion(cell);
    return cell_allow_mon(&fake, mon);
}
void illusion_display(dun_cell_ptr cell, int light, map_char_ptr mc)
{
    if (floor_has_illusion(cell))
    {
        dun_cell_t fake = _illusion(cell);
        cell_display(&fake, light, mc);
    }
    else
        cell_display(cell, light, mc);
}
/************************************************************************
 * Pattern (FEAT_PATTERN)
 ************************************************************************/
enum { /* subtype */
    PATTERN_START,
    PATTERN_1,
    PATTERN_2,
    PATTERN_3,
    PATTERN_4,
    PATTERN_END,
    PATTERN_EXIT
};
enum { /* flags */
    PATTERN_USED    = 0x010000,
    PATTERN_WRECKED = 0x020000, /* XXX not implemented XXX */
};
bool cell_is_pattern(dun_cell_ptr cell) { return cell->type == FEAT_PATTERN; }

bool pattern_is_start(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_START;
}
bool pattern_is_1(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_1;
}
bool pattern_is_2(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_2;
}
bool pattern_is_3(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_3;
}
bool pattern_is_4(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_4;
}
bool pattern_is_end(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_END;
}
bool pattern_is_exit(dun_cell_ptr cell)
{
    if (!cell_is_pattern(cell)) return FALSE;
    return cell->subtype == PATTERN_EXIT;
}
static void _pattern_flags(dun_cell_ptr cell)
{
    cell->flags |= CELL_LOS | CELL_PROJECT | CELL_PERM;
}
static void _pattern_init(void)
{
    _display_add_no_light("PATTERN_START", term_char_create('*', TERM_WHITE));
    _display_add_no_light("PATTERN_1", term_char_create('*', TERM_L_BLUE));
    _display_add_no_light("PATTERN_2", term_char_create('*', TERM_BLUE));
    _display_add_no_light("PATTERN_3", term_char_create('*', TERM_L_BLUE));
    _display_add_no_light("PATTERN_4", term_char_create('*', TERM_BLUE));
    _display_add_no_light("PATTERN_END", term_char_create('*', TERM_L_WHITE));
    _display_add_no_light("PATTERN_EXIT", term_char_create('*', TERM_WHITE));
}
static cptr _pattern_name(dun_cell_ptr cell)
{
    assert(cell_is_pattern(cell));
    switch (cell->subtype)
    {
    case PATTERN_START: return "PATTERN_START";
    case PATTERN_1: return "PATTERN_1";
    case PATTERN_2: return "PATTERN_2";
    case PATTERN_3: return "PATTERN_3";
    case PATTERN_4: return "PATTERN_4";
    case PATTERN_END: return "PATTERN_END";
    case PATTERN_EXIT: return "PATTERN_EXIT";
    }
    return "UNKNOWN";
}
static cptr _pattern_desc(dun_cell_ptr cell)
{
    assert(cell_is_pattern(cell));
    if (pattern_is_start(cell)) return "Pattern start";
    if (pattern_is_exit(cell)) return "Pattern exit";
    return "section of the Pattern";
}
static int _pattern_priority(dun_cell_ptr cell) { return 16; }
static bool _pattern_allow_plr(dun_cell_ptr cell) { return TRUE; }
static bool _pattern_allow_mon(dun_cell_ptr cell, mon_ptr mon) { return mon_can_fly(mon) || plr->riding == mon->id; }
static bool _pattern_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race) { return mon_race_can_fly(race); }
static void _teleport_level(dun_ptr dun)
{
    int lvl;
    if (msg_input_num("Teleport to level", &lvl, dun->type->min_dun_lvl, dun->type->plr_max_lvl))
    {
        msg_format("You teleport to dungeon level %d.", lvl);
        energy_use = 0;
        dun_mgr_wizard_jump(dun->type->id, lvl);
    }
}
static void _pattern_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    switch (cell->subtype)
    {
    case PATTERN_END: /* once only */
        if (cell->flags & PATTERN_USED) break;
        plr_tim_remove(T_POISON);
        plr_tim_remove(T_HALLUCINATE);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        plr_tim_remove(T_BLIND);
        fear_clear_p();
        do_res_stat(A_STR);
        do_res_stat(A_INT);
        do_res_stat(A_WIS);
        do_res_stat(A_DEX);
        do_res_stat(A_CON);
        do_res_stat(A_CHR);
        restore_level();
        hp_player(1000);
        plr_restore_life(1000);
        msg_print("This section of the Pattern looks less powerful.");
        cell->flags |= PATTERN_USED;
        break;
    case PATTERN_EXIT:
        if (get_check("Recall? "))
            dun_mgr_recall_plr();
        else if (get_check("Teleport level? "))
            _teleport_level(dun);
        else if (get_check("Normal teleport? "))
            teleport_player(200, 0L);
        break;
    }
}
static void _pattern_process_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell)
{
    int dd, ds;
    if (prace_is_(RACE_AMBERITE) && one_in_(2)) return;
    if (plr_tim_find(T_INVULN)) return;

    dd = 1 + dun->difficulty / 40;
    ds = 3 + dun->difficulty / 20;
    take_hit(DAMAGE_NOESCAPE, damroll(dd, ds), "walking the Pattern");
    plr->cave_no_regen = TRUE;
}
static dun_cell_type_ptr _pattern(void)
{
    dun_cell_type_ptr type = dun_cell_type_alloc(FEAT_PATTERN);

    type->init = _pattern_init;
    type->name = _pattern_name;
    type->desc = _pattern_desc;
    type->priority = _pattern_priority;

    type->allow_plr = _pattern_allow_plr;
    type->allow_mon = _pattern_allow_mon;
    type->allow_mon_race = _pattern_allow_mon_race;

    type->accept_plr = _pattern_accept_plr;
    type->process_plr = _pattern_process_plr;

    return type;
}
static void _seq_error(void) { msg_print("You must walk the Pattern in the correct order."); }
bool pattern_legal_move(dun_cell_ptr from, dun_cell_ptr to)
{
    if (!cell_is_pattern(from) && !cell_is_pattern(to)) return TRUE;

    /* begin walking the pattern: requires movement from not pattern tile
     * to pattern start tile. confused|hallucination begins walking without a prompt. */
    if (pattern_is_start(to))
    {
        if (!cell_is_pattern(from))
        {
            if (plr_tim_find(T_CONFUSED) || plr_tim_find(T_HALLUCINATE))
                return TRUE;
            else
                return get_check("If you start walking the Pattern, you must walk the whole way. Ok? ");
        }
        else /* start->1 but not 1->start */
        {
            _seq_error(); 
            return FALSE;
        }
    }
    /* you can only begin walking the pattern from the start */
    if (cell_is_pattern(to) && !cell_is_pattern(from))
    {
        assert(!pattern_is_start(to)); /* handled above */
        msg_print("You must start walking the Pattern from the startpoint.");
        return FALSE;
    }
    if (!cell_is_pattern(to) && cell_is_pattern(from))
    {
        msg_print("You may not step off the Pattern."); /* ever! you need to reach the exit */
        return FALSE;
    }

    /* legal moves */
    assert(cell_is_pattern(to) && cell_is_pattern(from));
    if (pattern_is_exit(to)) /* exit->exit and end->exit */
    {
        if (pattern_is_exit(from) || pattern_is_end(from)) return TRUE;
        else { _seq_error(); return FALSE; }
    }
    if (pattern_is_end(to)) /* _->end */
        return TRUE;
    if (pattern_is_1(to)) /* start->1 and 4->1 and 1->1 (corners) */
    {
        if (pattern_is_start(from) || pattern_is_4(from) || pattern_is_1(from)) return TRUE;
        else { _seq_error(); return FALSE; }
    }
    if (pattern_is_2(to)) /* 1->2 and 2-2 (corners) */
    {
        if (pattern_is_1(from) || pattern_is_2(from)) return TRUE;
        else { _seq_error(); return FALSE; }
    }
    if (pattern_is_3(to)) /* 2->3 and 3->3 (corners) */
    {
        if (pattern_is_2(from) || pattern_is_3(from)) return TRUE;
        else { _seq_error(); return FALSE; }
    }
    if (pattern_is_4(to)) /* 3->4 and 4->4 (corners) */
    {
        if (pattern_is_3(from) || pattern_is_4(from)) return TRUE;
        else { _seq_error(); return FALSE; }
    }
    assert(0); /* missed a case? */
    return TRUE;
}
/* Sample: p=start a=1 b=2 c=3 d=4 P=end B=exit
 * note the first corner: 3->4->4->1 vs (cutting) 3->4->1 (both are legal)
 * note the fifth corner: 3->3->4 vs (cutting) 3->4 (both are legal)
M:%%%%%%%%%%%%%%%%%
M:%...............%
M:%.pabcdabcdabcd.%
M:%.............d.%
M:%.ccdabcdabcc.a.%
M:%.b.........d.b.%
M:%.a.....PPP.a.c.%
M:%.d.bbbcPBP.b.d.%
M:%.c.a...PPP.c.a.%
M:%.b.d.......d.b.%
M:%.a.dcbadcbaa.c.%
M:%.d...........d.%
M:%.dcbadcbadcbaa.%
M:%...............%
M:%%%%%%%%%%%%%%%%% */

/************************************************************************
 * XXX
 ************************************************************************/
bool cell_place(dun_cell_ptr cell)
{
    switch (cell->type)
    {
    case FEAT_FLOOR:
        return TRUE;
    case FEAT_DOOR:
        return cell->subtype == DOOR_CURTAIN 
            || cell->parm1 == DOOR_OPEN
            || cell->parm1 == DOOR_BROKEN;
    case FEAT_WALL:
        return cell->subtype == WALL_MOUNTAIN;
    case FEAT_STAIRS:
    case FEAT_WATER:
    case FEAT_LAVA:
    case FEAT_TREE:
        return TRUE;
    }
    return FALSE;
}
bool cell_teleportable(dun_cell_ptr cell)
{
    if (cell->type == FEAT_CHASM) return TRUE;
    if (cell->type == FEAT_TREE) return FALSE;
    return cell_place(cell);
}
bool cell_is_boring(dun_cell_ptr cell)
{
    if (cell->type != FEAT_FLOOR) return FALSE;
    switch (cell->subtype)
    {
    case FLOOR_FLOOR:
    case FLOOR_DIRT:
    case FLOOR_GRASS:
        if (floor_has_known_trap(cell)) return FALSE; /* XXX secret traps *are* boring ;) */
        else if (floor_has_plr_trap(cell)) return FALSE;
        else if (cell->flags & CELL_GLYPH) return FALSE;
        return TRUE;
    }
    return FALSE;
}
bool cell_notice(dun_cell_ptr cell)
{
    if (cell_is_floor(cell))
    {
        if (floor_has_glyph_of_warding(cell)) return TRUE;
        if (floor_has_explosive_rune(cell)) return TRUE;
        if (floor_has_known_trap(cell)) return TRUE;
        if (floor_has_web(cell)) return TRUE;
        if (floor_has_mirror(cell)) return TRUE;
        return FALSE;
    }
    if (cell_is_door(cell)) return TRUE;
    if (cell_is_stairs(cell)) return TRUE;
    if (cell_is_pattern(cell)) return TRUE;
    if (cell_is_bldg(cell)) return TRUE;
    if (cell_is_portal(cell)) return TRUE;
    /* XXX run-time option to notice (known) gold */

    return FALSE;
}

/************************************************************************
 * Player Actions
 ************************************************************************/
int dun_disarm(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!(cell->flags & CELL_MAP))
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there.");
        return ACTION_ABORT;
    }
    if (!type->disarm)
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing to disarm.");
        return ACTION_ABORT;
    }
    return type->disarm(dun, pos, cell, options);
}
int dun_open(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!(cell->flags & CELL_MAP))
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there.");
        return ACTION_ABORT;
    }
    if (!type->open)
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing to open.");
        return ACTION_ABORT;
    }
    return type->open(dun, pos, cell, options);
}
int dun_close(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!(cell->flags & CELL_MAP))
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there.");
        return ACTION_ABORT;
    }
    if (!type->close)
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing to close.");
        return ACTION_ABORT;
    }
    return type->close(dun, pos, cell, options);
}
int dun_bash(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!(cell->flags & CELL_MAP))
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there.");
        return ACTION_ABORT;
    }
    if (!type->bash)
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing to bash.");
        return ACTION_ABORT;
    }
    return type->bash(dun, pos, cell, options);
}
int dun_jam(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!(cell->flags & CELL_MAP))
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there.");
        return ACTION_ABORT;
    }
    if (!type->jam)
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there to spike.");
        return ACTION_ABORT;
    }
    return type->jam(dun, pos, cell, options);
}
int dun_tunnel(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!(cell->flags & CELL_MAP))
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing there.");
        return ACTION_ABORT;
    }
    if (!type->tunnel)
    {
        if (!(options & ACTION_QUIET)) msg_print("You see nothing to tunnel.");
        return ACTION_ABORT;
    }
    return type->tunnel(dun, pos, cell, options);
}
int dun_search(dun_ptr dun, point_t pos, u32b options)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);
    obj_ptr obj;
    int rc = ACTION_CONTINUE;

    /* traps and secret doors */
    if (type->search)
        rc = type->search(dun, pos, cell, options);

    if (rc != ACTION_CONTINUE)
        return rc;

    /* chests */
    for (obj = dun_obj_at(dun, pos); obj; obj = obj->next)
    {
        if (obj->tval != TV_CHEST) continue;
        if (obj->pval < 0 || !chest_traps[obj->pval]) continue; /* not trapped */
        if (!obj_is_known(obj))
        {
            int skill = plr_skill(plr->skills.srh);
            if ((options & ACTION_FORCE) || _1d(100) <= skill)
            {
                if (!(options & ACTION_QUIET))
                    msg_print("You have discovered a trap on the chest!");
                obj_identify(obj);
                disturb(0, 0);
                rc = ACTION_SUCCESS;
            }
        }
    }

    return rc;
}

/************************************************************************
 * Monster Actions
 *
 * These are helpers for mon_ai:
 * SUCCESS means commit to this move. The monster altered the terrain.
 * CONTINUE means this move is legal, but it is ok to try alternatives.
 * ABORT means try another move, this one is impossible (e.g !mon_can_bash)
 * FAIL means mon tried something and failed. Charge energy, do not move,
 *   stop considering alternatives (e.g. failed to bash open a door)
 ************************************************************************/
int dun_open_mon(dun_ptr dun, point_t pos, mon_ptr mon)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_cell_type_ptr type = dun_cell_type(cell->type);

    if (!type->open_mon)
        return ACTION_CONTINUE;

    return type->open_mon(dun, pos, cell, mon);
}
/************************************************************************
 * Generation: Low Level. Also, this should be the only way to change
 * cell->type or cell->subtype.
 ************************************************************************/
static void _gen_set(dun_cell_ptr cell, int type, int subtype)
{
    cell->type = type;
    cell->subtype = subtype;
    cell->parm1 = 0;
    cell->parm2 = 0;
    cell->flags &= CELL_LO_MASK; /* turn off all hi bits, including dun_gen and D_WORLD flags */
    cell->flags &= CELL_CHANGE_MASK; /* turn off shared lo bits that some features optionally set */
}

/* doors */
static void _door_set(dun_cell_ptr cell, int subtype, int state)
{
    _gen_set(cell, FEAT_DOOR, subtype);
    cell->parm1 = state;
    if (state == DOOR_LOCKED) cell->parm2 = _1d(10);
    if (state == DOOR_JAMMED) cell->parm2 = _1d(10);
    _door_flags(cell);
}
void cell_make_open_curtain(dun_cell_ptr cell) { _door_set(cell, DOOR_CURTAIN, DOOR_OPEN); }
void cell_make_closed_curtain(dun_cell_ptr cell) { _door_set(cell, DOOR_CURTAIN, DOOR_CLOSED); }
void cell_make_open_door(dun_cell_ptr cell) { _door_set(cell, DOOR_DOOR, DOOR_OPEN); }
void cell_make_broken_door(dun_cell_ptr cell) { _door_set(cell, DOOR_DOOR, DOOR_BROKEN); }
void cell_make_closed_door(dun_cell_ptr cell) { _door_set(cell, DOOR_DOOR, DOOR_CLOSED); }
void cell_make_locked_door(dun_cell_ptr cell) { _door_set(cell, DOOR_DOOR, DOOR_LOCKED); }
void cell_make_jammed_door(dun_cell_ptr cell) { _door_set(cell, DOOR_DOOR, DOOR_JAMMED); }

/* floors */
static void _floor_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_FLOOR, subtype);
    _floor_flags(cell);
}
void cell_make_floor(dun_cell_ptr cell) { _floor_set(cell, FLOOR_FLOOR); }
void cell_make_dirt(dun_cell_ptr cell) { _floor_set(cell, FLOOR_DIRT); }
void cell_make_grass(dun_cell_ptr cell) { _floor_set(cell, FLOOR_GRASS); }
void cell_make_flower(dun_cell_ptr cell) { _floor_set(cell, FLOOR_FLOWER); }
void cell_make_brake(dun_cell_ptr cell) { _floor_set(cell, FLOOR_BRAKE); }
void cell_make_road(dun_cell_ptr cell) { _floor_set(cell, FLOOR_ROAD); }
void cell_make_dark_floor(dun_cell_ptr cell) {
    _floor_set(cell, FLOOR_FLOOR);
    cell->flags |= CELL_DARK;
    plr->update |= PU_LIGHT;
}

/* walls */
static void _wall_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_WALL, subtype);
    _wall_flags(cell);
}
void cell_make_granite(dun_cell_ptr cell) { _wall_set(cell, WALL_GRANITE); }
void cell_make_permanent(dun_cell_ptr cell) { _wall_set(cell, WALL_PERMANENT); }
void cell_make_mountain(dun_cell_ptr cell) { _wall_set(cell, WALL_MOUNTAIN); }
void cell_make_mountain_wall(dun_cell_ptr cell) { _wall_set(cell, WALL_MOUNTAIN_WALL); }
void cell_make_rubble(dun_cell_ptr cell) { _wall_set(cell, WALL_RUBBLE); }
void cell_make_magma(dun_cell_ptr cell) { cell_make_magma_aux(cell, 10, 90); }
void cell_make_quartz(dun_cell_ptr cell) { cell_make_quartz_aux(cell, 20, 90); }
static void _wall_treasure(dun_cell_ptr cell, int gold_pct, int secret_pct)
{
    cell->flags &= ~(CELL_GOLD | CELL_SECRET);
    if (_1d(100) <= gold_pct)
    {
        cell->flags |= CELL_GOLD;
        if (_1d(100) <= secret_pct)
            cell->flags |= CELL_SECRET;
    }
}
void cell_make_magma_aux(dun_cell_ptr cell, int gold_pct, int secret_pct)
{
    _wall_set(cell, WALL_MAGMA);
    _wall_treasure(cell, gold_pct, secret_pct);
}
void cell_make_quartz_aux(dun_cell_ptr cell, int gold_pct, int secret_pct)
{
    _wall_set(cell, WALL_QUARTZ);
    _wall_treasure(cell, gold_pct, secret_pct);
}

/* stairs */
static void _stairs_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_STAIRS, subtype);
    _stairs_flags(cell);
}
void cell_make_downstairs(dun_cell_ptr cell) { _stairs_set(cell, STAIRS_DOWN); }
void cell_make_upstairs(dun_cell_ptr cell) { _stairs_set(cell, STAIRS_UP); }
void cell_make_dungeon_entrance(dun_cell_ptr cell, int dun_type_id)
{
    assert(0 < dun_type_id && dun_type_id < 255);
    cell_make_downstairs(cell);
    cell->flags |= CELL_DUNGEON;
    cell->parm2 = dun_type_id;
    _stairs_flags(cell); /* LIT|MAP */
}
void cell_make_quest_entrance(dun_cell_ptr cell, int quest_id)
{
    assert(0 < quest_id && quest_id < 255);
    cell_make_downstairs(cell);
    cell->flags |= CELL_QUEST;
    cell->parm2 = quest_id;
    _stairs_flags(cell); /* LIT|MAP */
}

/* trees */
static void _tree_set(dun_cell_ptr cell)
{
    _gen_set(cell, FEAT_TREE, 0);
    _tree_flags(cell);
}
void cell_make_tree(dun_cell_ptr cell) { _tree_set(cell); }

/* water */
static void _water_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_WATER, subtype);
    _water_flags(cell);
}
void cell_make_deep_water(dun_cell_ptr cell) { _water_set(cell, WATER_DEEP); }
void cell_make_shallow_water(dun_cell_ptr cell) { _water_set(cell, WATER_SHALLOW); }
void cell_make_swamp(dun_cell_ptr cell) { _water_set(cell, WATER_SWAMP); }

/* lava */
static void _lava_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_LAVA, subtype);
    _lava_flags(cell);
}
void cell_make_deep_lava(dun_cell_ptr cell) { _lava_set(cell, LAVA_DEEP); }
void cell_make_shallow_lava(dun_cell_ptr cell) { _lava_set(cell, LAVA_SHALLOW); }

/* chasms */
static void _chasm_set(dun_cell_ptr cell)
{
    _gen_set(cell, FEAT_CHASM, 0);
    _chasm_flags(cell);
}
void cell_make_chasm(dun_cell_ptr cell) { _chasm_set(cell); }

/* portals */
static void _portal_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_PORTAL, subtype);
    _portal_flags(cell);
}
void cell_make_recall(dun_cell_ptr cell) { _portal_set(cell, PORTAL_RECALL); }
void cell_make_travel(dun_cell_ptr cell) { _portal_set(cell, PORTAL_TRAVEL); }

/* buildings */
static void _bldg_set(dun_cell_ptr cell, int subtype, int which)
{
    _gen_set(cell, FEAT_BLDG, subtype);
    cell->parm1 = which;
    _bldg_flags(cell);
}
void cell_make_shop(dun_cell_ptr cell, int which) { _bldg_set(cell, BLDG_SHOP, which); }
void cell_make_general_store(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_GENERAL); }
void cell_make_armory(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_ARMORY); }
void cell_make_weapon_smiths(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_WEAPON); }
void cell_make_temple(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_TEMPLE); }
void cell_make_alchemist(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_ALCHEMIST); }
void cell_make_magic_shop(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_MAGIC); }
void cell_make_black_market(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_BLACK_MARKET); }
void cell_make_home(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_HOME); }
void cell_make_bookstore(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_BOOK); }
void cell_make_museum(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_MUSEUM); }
void cell_make_jeweler(dun_cell_ptr cell) { cell_make_shop(cell, SHOP_JEWELER); }

void cell_make_bldg(dun_cell_ptr cell, int which) { _bldg_set(cell, BLDG_BLDG, which); }
void cell_make_inn(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_INN); }
void cell_make_castle(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_CASTLE); }
void cell_make_fighters_guild(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_FIGHTERS_GUILD); }
void cell_make_archers_guild(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_ARCHERS_GUILD); }
void cell_make_thieves_guild(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_THIEVES_GUILD); }
void cell_make_wizards_guild(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_WIZARDS_GUILD); }
void cell_make_priests_guild(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_PRIESTS_GUILD); }
void cell_make_hunters_office(dun_cell_ptr cell) { cell_make_bldg(cell, BLDG_HUNTERS_OFFICE); }

/* pattern */
static void _pattern_set(dun_cell_ptr cell, int subtype)
{
    _gen_set(cell, FEAT_PATTERN, subtype);
    _pattern_flags(cell);
}
void cell_make_pattern_start(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_START); }
void cell_make_pattern_1(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_1); }
void cell_make_pattern_2(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_2); }
void cell_make_pattern_3(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_3); }
void cell_make_pattern_4(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_4); }
void cell_make_pattern_end(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_END); }
void cell_make_pattern_exit(dun_cell_ptr cell) { _pattern_set(cell, PATTERN_EXIT); }

/************************************************************************
 * Generation: High Level
 *
 * These delete existing monsters during gen. Might also make random decisions
 * such as placing a random door (open|closed|secret|locked|jammed). Handle
 * noting and redrawing during normal play.
 ************************************************************************/

/* during normal gameplay, all terrain changes should pass thru one
 * of the dun_place_ functions, and all should layer on top _dun_place()
 * in order to correctly update display|view|light. the cell_make_ functions
 * are helpers only (also used for D_WORLD generation). */
static void _delete_mon_at(dun_ptr dun, point_t pos)
{
    mon_ptr mon;
    if (dun->flags & DF_GENERATED) return;
    mon = dun_mon_at(dun, pos);
    if (mon) dun_delete_mon(dun, mon->id);
}
static void _redraw_at(dun_ptr dun, point_t pos)
{
    mon_ptr mon;
    if (!(dun->flags & DF_GENERATED)) return;
    mon = dun_mon_at(dun, pos);
    if (mon)
        update_mon(mon, FALSE);
    dun_note_pos(dun, pos);
    dun_draw_pos(dun, pos);
}
static void _dun_place(dun_ptr dun, point_t pos, cell_make_f f)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    bool old_los = BOOL(cell->flags & CELL_LOS);
    _delete_mon_at(dun, pos);
    f(cell);
    if (dun->flags & DF_NO_LIGHT)
        cell->flags |= CELL_DARK;
    _redraw_at(dun, pos);
    if (old_los != BOOL(cell->flags & CELL_LOS))
        plr->update |= PU_VIEW | PU_LIGHT | PU_MON_LIGHT | PU_MONSTERS;
}

/* doors */
void dun_place_open_door(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_open_door); }
void dun_place_broken_door(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_broken_door); }
void dun_place_closed_door(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_closed_door); }
void dun_place_locked_door(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_locked_door); }
void dun_place_jammed_door(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_jammed_door); }
void dun_place_open_curtain(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_open_curtain); }
void dun_place_closed_curtain(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_closed_curtain); }
void dun_place_curtain(dun_ptr dun, point_t pos)
{
    if (_1d(100) < 40) dun_place_open_curtain(dun, pos);
    else dun_place_closed_curtain(dun, pos);
}
void dun_place_random_door(dun_ptr dun, point_t pos)
{
    int roll;

    if (dun->type->flags.gen & DF_GEN_NO_DOORS)
    {
        dun_place_floor(dun, pos);
        return;
    }
    if (dun->type->flags.gen & DF_GEN_CURTAIN)
    {
        int odds = 256;
        if (dun->type->flags.gen & DF_GEN_NO_CAVE) odds = 16;
        if (one_in_(odds))
        {
            dun_place_curtain(dun, pos);
            return;
        }
    }            

    roll = _1d(1000);
    if (roll < 300)
        dun_place_open_door(dun, pos);
    else if (roll < 400)
        dun_place_broken_door(dun, pos);
    else if (roll < 600)
        dun_place_secret_door(dun, pos);
    else
    {
        roll = randint0(400);
        if (roll < 300)
            dun_place_closed_door(dun, pos);
        else if (roll < 399)
            dun_place_locked_door(dun, pos);
        else
            dun_place_jammed_door(dun, pos);
    }
}
void dun_place_secret_door(dun_ptr dun, point_t pos)
{
    dun->type->place_wall(dun, pos);
    _wall_make_secret_door(dun_grid_at(dun, pos));
    _redraw_at(dun, pos);
}
/* floors */
void dun_place_floor(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_floor); }
void dun_place_dark_floor(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_dark_floor); }
void dun_place_dirt(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_dirt); }
void dun_place_grass(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_grass); }
void dun_place_flower(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_flower); }
void dun_place_brake(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_brake); }
void dun_place_road(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_road); }
/* walls */
void dun_place_illusory_wall(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    if (cell->type != FEAT_FLOOR) return;
    if (cell->flags & FLOOR_DIRTY) return;
    cell->flags |= CELL_ILLUSION;
    cell->parm1 = FEAT_WALL;
    cell->parm2 = WALL_GRANITE;
    dun_draw_pos(dun, pos);
    plr->update |= PU_FLOW | PU_MON_FLOW;
}
void dun_place_granite(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_granite); }
void dun_place_permanent(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_permanent); }
void dun_place_mountain(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_mountain); }
void dun_place_mountain_wall(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_mountain_wall); }
void dun_place_rubble(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_rubble); }
void dun_place_magma(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_magma); }
void dun_place_quartz(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_quartz); }
void dun_place_magma_aux(dun_ptr dun, point_t pos, int gold_pct, int secret_pct)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_place_magma(dun, pos);
    _wall_treasure(cell, gold_pct, secret_pct);
    if (dun->flags & DF_GENERATED)
        dun_draw_pos(dun, pos);
}
void dun_place_quartz_aux(dun_ptr dun, point_t pos, int gold_pct, int secret_pct)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_place_quartz(dun, pos);
    _wall_treasure(cell, gold_pct, secret_pct);
    if (dun->flags & DF_GENERATED)
        dun_draw_pos(dun, pos);
}
/* stairs */
void dun_place_downstairs(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_downstairs); }
void dun_place_upstairs(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_upstairs); }
void dun_place_quest_entrance(dun_ptr dun, point_t pos, int quest_id)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_place_downstairs(dun, pos);
    cell->flags |= CELL_QUEST;
    cell->parm2 = quest_id;
    _stairs_flags(cell); /* LIT|MAP */
    _redraw_at(dun, pos);
}
void dun_place_dungeon_entrance(dun_ptr dun, point_t pos, int dun_type_id)
{
    dun_cell_ptr cell = dun_grid_at(dun, pos);
    dun_place_downstairs(dun, pos);
    cell->flags |= CELL_DUNGEON;
    cell->parm2 = dun_type_id;
    _stairs_flags(cell); /* LIT|MAP */
    _redraw_at(dun, pos);
}
/* buildings */
void dun_place_general_store(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_general_store); }
void dun_place_armory(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_armory); }
void dun_place_weapon_smiths(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_weapon_smiths); }
void dun_place_temple(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_temple); }
void dun_place_alchemist(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_alchemist); }
void dun_place_magic_shop(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_magic_shop); }
void dun_place_black_market(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_black_market); }
void dun_place_home(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_home); }
void dun_place_bookstore(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_bookstore); }
void dun_place_museum(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_museum); }
void dun_place_jeweler(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_jeweler); }

void dun_place_inn(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_inn); }
void dun_place_castle(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_castle); }
void dun_place_fighters_guild(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_fighters_guild); }
void dun_place_archers_guild(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_archers_guild); }
void dun_place_thieves_guild(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_thieves_guild); }
void dun_place_wizards_guild(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_wizards_guild); }
void dun_place_priests_guild(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_priests_guild); }
void dun_place_hunters_office(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_hunters_office); }

/* pattern  */
void dun_place_pattern_start(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_start); }
void dun_place_pattern_1(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_1); }
void dun_place_pattern_2(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_2); }
void dun_place_pattern_3(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_3); }
void dun_place_pattern_4(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_4); }
void dun_place_pattern_end(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_end); }
void dun_place_pattern_exit(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_pattern_exit); }

/* misc */
void dun_place_tree(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_tree); }
void dun_place_deep_water(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_deep_water); }
void dun_place_shallow_water(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_shallow_water); }
void dun_place_swamp(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_swamp); }
void dun_place_deep_lava(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_deep_lava); }
void dun_place_shallow_lava(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_shallow_lava); }
void dun_place_chasm(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_chasm); }
void dun_place_recall(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_recall); }
void dun_place_travel(dun_ptr dun, point_t pos) { _dun_place(dun, pos, cell_make_travel); }

/************************************************************************
 * Generation: Templates
 ************************************************************************/
typedef struct {
    cptr name;
    dun_place_f f;
} _feat_t, *_feat_ptr;
static void _dun_place_floor(dun_ptr dun, point_t pos) { dun->type->place_floor(dun, pos); }
static void _dun_place_wall(dun_ptr dun, point_t pos) { dun->type->place_wall(dun, pos); }
static void _dun_place_inner(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    dun->type->place_inner_wall(dun, pos);
    cell->flags &= ~CELL_GEN_MASK;
    cell->flags |= CELL_INNER | CELL_ROOM;
}
static void _dun_place_permanent(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    dun_place_permanent(dun, pos);
    cell->flags &= ~CELL_GEN_MASK;
    cell->flags |= CELL_INNER | CELL_ROOM;
}
static void _dun_place_outer(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    dun->type->place_outer_wall(dun, pos);
    cell->flags &= ~CELL_GEN_MASK;
    cell->flags |= CELL_OUTER | CELL_ROOM;
}
static void _dun_place_dark_floor(dun_ptr dun, point_t pos)
{
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    dun->type->place_floor(dun, pos);
    cell->flags |= CELL_DARK;
}
static void _dun_place_entrance(dun_ptr dun, point_t pos)
{
    /* This hack is for random dungeon entrances for surface wilderness encounters */
    assert(dun->type->id == D_SURFACE);

    /* ENTRANCE(Random Mountain) -> cave_feat = `ENTRANCE
     *                              extra = D_RANDOM_MOUNTAIN
     * apply_room_grid_feat: `ENTRANCE -> _dun_place_entrance
     *                       then correct cell->parm2 to extra (D_RANDOM_MOUNTAIN)
     * This is quite hackish ... cf build_room_template_aux for dun->stairs setup */
    dun_place_dungeon_entrance(dun, pos, D_STRONGHOLD);
                                      /* ^--- dummy to avoid assertion error */
}

static _feat_t _feat_tbl[] = {
    /* access to type->place_* functions */
    { "FLOOR", _dun_place_floor },  /* XXX masks dun_place_floor, perhaps *FLOOR*? */
    { "DARK_FLOOR", _dun_place_dark_floor },
    { "WALL", _dun_place_wall },
    { "INNER", _dun_place_inner },
    { "OUTER", _dun_place_outer },
    { "INNER_PERM", _dun_place_permanent },
    { "ENTRANCE", _dun_place_entrance },

    { "DOOR", dun_place_random_door },
    { "SECRET_DOOR", dun_place_secret_door },
    { "CURTAIN", dun_place_closed_curtain },

    /* specific features XXX Incomplete Listing (cf stairs, doors and FLOOR_FLOOR) XXX */
    { "DIRT", dun_place_dirt },
    { "GRASS", dun_place_grass },
    { "FLOWER", dun_place_flower },
    { "BRAKE", dun_place_brake },
    { "ROAD", dun_place_road },

    { "GRANITE", dun_place_granite },
    { "PERMANENT", dun_place_permanent },
    { "MOUNTAIN", dun_place_mountain },
    { "MOUNTAIN_WALL", dun_place_mountain_wall },
    { "RUBBLE", dun_place_rubble },
    { "MAGMA_VEIN", dun_place_magma },
    { "QUARTZ_VEIN", dun_place_quartz },

    { "TREE", dun_place_tree },
    { "DEEP_WATER", dun_place_deep_water },
    { "SHALLOW_WATER", dun_place_shallow_water },
    { "SWAMP", dun_place_swamp },
    { "DEEP_LAVA", dun_place_deep_lava },
    { "SHALLOW_LAVA", dun_place_shallow_lava },
    { "CHASM", dun_place_chasm },
    { "RECALL", dun_place_recall },
    { "TRAVEL", dun_place_travel },

    { "GENERAL_STORE", dun_place_general_store },
    { "ARMORY", dun_place_armory },
    { "WEAPON_SMITHS", dun_place_weapon_smiths },
    { "TEMPLE", dun_place_temple },
    { "ALCHEMIST", dun_place_alchemist },
    { "MAGIC_SHOP", dun_place_magic_shop },
    { "BLACK_MARKET", dun_place_black_market },
    { "HOME", dun_place_home },
    { "BOOKSTORE", dun_place_bookstore },
    { "MUSEUM", dun_place_museum },
    { "JEWELER", dun_place_jeweler },

    { "BLDG_INN", dun_place_inn },
    { "BLDG_CASTLE", dun_place_castle },
    { "BLDG_FIGHTERS", dun_place_fighters_guild },
    { "BLDG_ARCHERS", dun_place_archers_guild },
    { "BLDG_THIEVES", dun_place_thieves_guild },
    { "BLDG_WIZARDS", dun_place_wizards_guild },
    { "BLDG_PRIESTS", dun_place_priests_guild },
    { "BLDG_HUNTERS", dun_place_hunters_office },

    { "PATTERN_START", dun_place_pattern_start },
    { "PATTERN_1", dun_place_pattern_1 },
    { "PATTERN_2", dun_place_pattern_2 },
    { "PATTERN_3", dun_place_pattern_3 },
    { "PATTERN_4", dun_place_pattern_4 },
    { "PATTERN_END", dun_place_pattern_end },
    { "PATTERN_EXIT", dun_place_pattern_exit },

    { 0 }
};

static _feat_ptr _feat_parse(cptr name)
{
    static int_map_ptr _map = NULL;
    sym_t x;
    if (!_map)
    {
        int i;
        _map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            _feat_ptr f = &_feat_tbl[i];
            if (!f->name) break;
            int_map_add(_map, sym_add(f->name), f); 
        }
    }
    x = sym_find(name);  /* don't bloat global sym table with trash */
    if (!x) return NULL;
    return int_map_find(_map, x);
}

errr parse_room_grid_feature(char* name, char **args, int arg_ct, room_grid_ptr grid)
{
    int i;
    if (!_feat_parse(name))
    {
        msg_format("Unknown Feature: <color:r>%s</color>", name);
        return PARSE_ERROR_UNDEFINED_DIRECTIVE;
    }
    if (arg_ct > 2)
    {
        msg_print("Error: Invalid feature directive. Syntax: <Name>[(<flags> [,<special info>])].");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    grid->cave_feat = sym_add(name);
    /* XXX Syntax needs work: eg ENTRANCE(Random Volcano) doesn't need flags */
    for (i = 0; i < arg_ct; i++)
    {
        char *arg = args[i];
        char *flags[10];
        int   flag_ct = z_string_split(arg, flags, 10, "|");
        int   j;

        for (j = 0; j < flag_ct; j++)
        {
            char* flag = flags[j];

            if (is_numeric(flag))
            {
                grid->flags |= ROOM_GRID_SPECIAL;
                grid->extra = atoi(flag);
            }
            else if (streq(flag, "ROOM"))
                grid->cave_flags |= CELL_ROOM;
            else if (streq(flag, "LIT"))
                grid->cave_flags |= CELL_LIT;
            else if (streq(flag, "MAP"))
                grid->cave_flags |= CELL_MAP | CELL_AWARE;
            else if (streq(flag, "TOWN"))
                grid->cave_flags |= CELL_TOWN;
            else if (streq(flag, "DUNGEON"))
                grid->cave_flags |= CELL_DUNGEON;
            else if (streq(flag, "ROAD"))
                grid->cave_flags |= CELL_ROAD;
            else if (streq(flag, "RIVER"))
                grid->cave_flags |= CELL_RIVER;
            else if (streq(flag, "QUEST"))
                grid->cave_flags |= CELL_QUEST;
            else if (grid->cave_flags & CELL_TOWN)
            {
                int id = towns_parse(flag);
                if (id)
                {
                    grid->flags |= ROOM_GRID_SPECIAL;
                    grid->extra = id;
                }
                else
                {
                    msg_format("Error: Unkown Town <color:r>%s</color>.", flag);
                    return PARSE_ERROR_UNDEFINED_DIRECTIVE;
                }
            }
            else if (grid->cave_flags & CELL_QUEST)
            {
                quest_ptr quest = quests_parse(flag);
                if (quest)
                {
                    grid->flags |= ROOM_GRID_SPECIAL;
                    grid->extra = quest->id;
                }
                else
                {
                    msg_format("Error: Unkown Quest <color:r>%s</color>.", flag);
                    return PARSE_ERROR_UNDEFINED_DIRECTIVE;
                }
            }
            /* CELL_DUNGEON is used on D_WORLD map only
             * ENTRANCE(Random Mountain) is used for wilderness encounters and won't have CELL_DUNGEON set */
            else if ((grid->cave_flags & CELL_DUNGEON) || sym_equals(grid->cave_feat, "ENTRANCE"))
            {
                grid->flags |= ROOM_GRID_SPECIAL;
                grid->extra = dun_types_parse(flag);
                if (!grid->extra)
                {
                    msg_format("Error: Unkown Dungeon <color:r>%s</color>.", flag);
                    return PARSE_ERROR_UNDEFINED_DIRECTIVE;
                }
            }
            else
            {
                msg_format("Error: Unknown Feature Option <color:r>%s</color>.", flag);
                return PARSE_ERROR_INVALID_FLAG;
            }
        }
    }
    if (arg_ct >= 2)
    {
    }
    return 0;
}
errr parse_room_grid_trap(char **args, int arg_ct, room_grid_ptr grid)
{
    if (arg_ct > 2 || arg_ct < 1)
    {
        msg_print("Error: Invalid trap directive. Expected TRAP(<name>[,<pct>]).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    if (streq(args[0], "*"))
    {
        grid->flags |= ROOM_GRID_TRAP_RANDOM;
    }
    else
    {
        _trap_ptr trap = _trap_parse(args[0]);
        if (!trap)
        {
            msg_format("Error: Unknown trap <color:r>%s</color>", args[0]);
            return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
        grid->cave_trap = trap->id;
    } 
    if (arg_ct >= 2)
    {
        int n;
        if (sscanf(args[1], "%d%%", &n) == 1)
            grid->trap_pct = n;
        else
        {
            msg_format("Error: Invalid Trap Pct <color:r>%s</color>", args[1]);
            return PARSE_ERROR_GENERIC;
        }
    }
    return 0;
}
void apply_room_grid_feat(point_t pos, room_grid_ptr grid)
{
    dun_ptr dun = cave; /* XXX pending rooms.c refactor */
    dun_cell_ptr cell = dun_cell_at(dun, pos);
    if (grid->cave_feat)
    {
        _feat_ptr f = _feat_parse(sym_str(grid->cave_feat));
        if (!f) return; /* paranoia */
        f->f(dun, pos);
        cell->flags |= grid->cave_flags;
        if (grid->flags & ROOM_GRID_SPECIAL)
            cell->parm2 = grid->extra;
    }
    if (!floor_is_clean(cell)) return;
    if (grid->cave_trap)
    {
        if (!grid->trap_pct || _1d(100) <= grid->trap_pct)
        {
            cell->flags |= CELL_TRAP | CELL_SECRET;
            cell->parm1 = grid->cave_trap;
            cell->parm2 = _trap_difficulty(cell->parm1) + _1d(dun->difficulty/5);
            _floor_flags(cell);
        }
    }
    else if (grid->flags & ROOM_GRID_TRAP_RANDOM)
    {
        if (!grid->trap_pct || _1d(100) < grid->trap_pct)
            _secret_trap(dun, pos, cell);
    }
}
