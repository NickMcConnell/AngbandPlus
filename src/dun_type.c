#include "angband.h"
#include "dun.h"
#include "dun_gen.h"
#include "int_map.h"
#include <assert.h>

typedef struct {
    int            id;
    cptr           name;
    cptr           parse;
    dun_type_ptr (*create_f)(void);
} _entry_t, *_entry_ptr;

static dun_type_ptr _world(void);
static dun_type_ptr _surface(void);
static dun_type_ptr _quest(void);
static dun_type_ptr _amber(void);
static dun_type_ptr _stronghold(void);
static dun_type_ptr _orc_cave(void);
static dun_type_ptr _forest(void);
static dun_type_ptr _icky_cave(void);
static dun_type_ptr _camelot(void);
static dun_type_ptr _lonely_mountain(void);
static dun_type_ptr _moria(void);
static dun_type_ptr _mountain(void);
static dun_type_ptr _castle(void);
static dun_type_ptr _isengard(void);
static dun_type_ptr _minas_morgul(void);
static dun_type_ptr _dark_tower(void);
static dun_type_ptr _mount_doom(void);
static dun_type_ptr _angband(void);
static dun_type_ptr _olympus(void);
static dun_type_ptr _dragons_lair(void);
static dun_type_ptr _random_forest(void);
static dun_type_ptr _random_mountain(void);
static dun_type_ptr _random_volcano(void);
static dun_type_ptr _random_sea(void);
static dun_type_ptr _wizards_tower(void);
static dun_type_ptr _monastery(void);
static dun_type_ptr _graveyard(void);
static dun_type_ptr _sanctuary(void);
static dun_type_ptr _pandemonium(void);
static dun_type_ptr _numenor(void);
static dun_type_ptr _rlyeh(void);
static dun_type_ptr _dark_castle(void);
static dun_type_ptr _dark_cave(void);

static _entry_t _tbl[] = {
    { D_WORLD, "D_WORLD", "World", _world },
    { D_SURFACE, "D_SURFACE", "Surface", _surface },
    { D_QUEST, "D_QUEST", "Quest", _quest },
    { D_AMBER, "D_AMBER", "Amber", _amber },
    { D_STRONGHOLD, "D_STRONGHOLD", "Stronghold", _stronghold },
    { D_ORC_CAVE, "D_ORC_CAVE", "Orc Cave", _orc_cave },
    { D_LONELY_MOUNTAIN, "D_LONELY_MOUNTAIN", "Lonely Mountain", _lonely_mountain },
    { D_FOREST, "D_FOREST", "Forest", _forest },
    { D_CAMELOT, "D_CAMELOT", "Camelot", _camelot },
    { D_ICKY_CAVE, "D_ICKY_CAVE", "Icky Cave", _icky_cave },
    { D_MORIA, "D_MORIA", "Moria", _moria },
    { D_MOUNTAIN, "D_MOUNTAIN", "Mountain", _mountain },
    { D_CASTLE, "D_CASTLE", "Castle", _castle },
    { D_ISENGARD, "D_ISENGARD", "Isengard", _isengard },
    { D_MINAS_MORGUL, "D_MINAS_MORGUL", "Minas Morgul", _minas_morgul },
    { D_DARK_TOWER, "D_DARK_TOWER", "Dark Tower", _dark_tower },
    { D_MOUNT_DOOM, "D_MOUNT_DOOM", "Mount Doom", _mount_doom },
    { D_ANGBAND, "D_ANGBAND", "Angband", _angband },
    { D_OLYMPUS, "D_OLYMPUS", "Olympus", _olympus },
    { D_DRAGONS_LAIR, "D_DRAGONS_LAIR", "Dragons' Lair", _dragons_lair },
    { D_RANDOM_FOREST, "D_RANDOM_FOREST", "Random Forest", _random_forest },
    { D_RANDOM_MOUNTAIN, "D_RANDOM_MOUNTAIN", "Random Mountain", _random_mountain },
    { D_RANDOM_VOLCANO, "D_RANDOM_VOLCANO", "Random Volcano", _random_volcano },
    { D_RANDOM_SEA, "D_RANDOM_SEA", "Random Sea", _random_sea },
    { D_WIZARDS_TOWER, "D_WIZARDS_TOWER", "Wizards' Tower", _wizards_tower },
    { D_MONASTERY, "D_MONASTERY", "Monastery", _monastery },
    { D_GRAVEYARD, "D_GRAVEYARD", "Graveyard", _graveyard },
    { D_SANCTUARY, "D_SANCTUARY", "Sanctuary", _sanctuary },
    { D_PANDEMONIUM, "D_PANDEMONIUM", "Pandemonium", _pandemonium },
    { D_NUMENOR, "D_NUMENOR", "Numenor", _numenor },
    { D_RLYEH, "D_RLYEH", "R'lyeh", _rlyeh },
    { D_DARK_CASTLE, "D_DARK_CASTLE", "Dark Castle", _dark_castle },
    { D_DARK_CAVE, "D_DARK_CAVE", "Dark Cave", _dark_cave },
    { 0 }
};

static dun_type_ptr dun_type_alloc(int id, cptr name)
{
    dun_type_ptr type = malloc(sizeof(dun_type_t));
    memset(type, 0, sizeof(dun_type_t));
    type->id = id;
    type->name = name;
    type->desc = name;
    type->place_stream1 = dun_place_magma;
    type->place_stream2 = dun_place_quartz;
    type->place_outer_wall = dun_place_granite;
    type->place_inner_wall = dun_place_granite;
    type->place_wall = dun_place_granite;
    type->place_floor = dun_place_floor;
    return type;
}

static void dun_type_free(dun_type_ptr type)
{
    if (!type) return;
    if (type->mon_alloc_tbl) vec_free(type->mon_alloc_tbl);
    if (type->obj_alloc_tbl) vec_free(type->obj_alloc_tbl);
    free(type);
}

int dun_types_parse(cptr name)
{
    int i;
    for (i = 0;; i++)
    {
        _entry_ptr e = &_tbl[i];
        if (!e->create_f) break;
        if (strcmp(e->parse, name) == 0) return e->id;
        if (strcmp(e->name, name) == 0) return e->id;
    }
    return D_NONE;
}

static int_map_ptr _types(void)
{
    static int_map_ptr _map = NULL;
    if (!_map)
        _map = int_map_alloc((int_map_free_f)dun_type_free);
    return _map;
}
dun_type_ptr dun_types_lookup(int id)
{
    dun_type_ptr type = int_map_find(_types(), id);
    assert(vec_length(mon_alloc_tbl)); /* make sure initialization is complete */
    if (!type)
    {
        int i;
        if (!id) return NULL; /* paranoia */
        for (i = 0; !type; i++)
        {
            _entry_ptr e = &_tbl[i];
            if (!e->id) return NULL;
            if (e->id != id) continue;
            assert(e->create_f);
            if (!e->create_f) return NULL; /* paranoia */
            type = e->create_f();
            assert(type->id == id);
            int_map_add(_types(), id, type);
        }
    }
    return type;
}
void dun_types_reset_world(void)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(_types());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_type_ptr type = int_map_iter_current(iter);
        type->world_pos = point_create(0, 0);
    }
    int_map_iter_free(iter);
}

static void dun_type_load(dun_type_ptr type, savefile_ptr file)
{
    sym_t n;
    type->max_dun_lvl = savefile_read_s16b(file);
    /* XXX Support adding a guardian to a dun_type that previously had none.
     * In other words, don't clobber a valid mon_race->id with 0! */
    n = savefile_read_sym(file);
    if (n) type->final_guardian = n;
    type->world_pos.x = savefile_read_s16b(file);
    type->world_pos.y = savefile_read_s16b(file);
    type->plr_max_lvl = savefile_read_s16b(file);
    type->last_recall = savefile_read_byte(file);
    type->last_shop   = savefile_read_byte(file);
    type->flags.plr   = savefile_read_u16b(file);
    if (type->load_f) type->load_f(type, file);
}
static void dun_type_save(dun_type_ptr type, savefile_ptr file)
{
    savefile_write_u16b(file, type->id);
    savefile_write_s16b(file, type->max_dun_lvl);
    savefile_write_sym(file, type->final_guardian);
    savefile_write_s16b(file, type->world_pos.x);
    savefile_write_s16b(file, type->world_pos.y);
    savefile_write_s16b(file, type->plr_max_lvl);
    savefile_write_byte(file, type->last_recall);
    savefile_write_byte(file, type->last_shop);
    savefile_write_u16b(file, type->flags.plr);
    if (type->save_f) type->save_f(type, file);
}

void dun_types_save(savefile_ptr file)
{
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(_types());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_type_ptr type = int_map_iter_current(iter);
        dun_type_save(type, file);
    }
    savefile_write_u16b(file, D_NONE);
    int_map_iter_free(iter);
}

void dun_types_load(savefile_ptr file)
{
    int_map_ptr  types = _types();
    dun_type_ptr type;
    int_map_clear(types);
    for (;;)
    {
        int id = savefile_read_u16b(file);
        if (id == D_NONE) break;
        type = dun_types_lookup(id);
        assert(type);
        assert(type->id == id);
        dun_type_load(type, file);
    }
}

static int _cmp_d_lvl(dun_type_ptr l, dun_type_ptr r)
{
    if (l->max_dun_lvl < r->max_dun_lvl) return -1;
    if (l->max_dun_lvl > r->max_dun_lvl) return 1;
    if (l->id < r->id) return -1;
    if (l->id > r->id) return 1;
    return 0;
}
vec_ptr plr_dun_types(void)
{
    vec_ptr v = vec_alloc(NULL);
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(_types());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_type_ptr type = int_map_iter_current(iter);
        if (!type->max_dun_lvl) continue;
        if (!type->plr_max_lvl) continue;
        if (type->flags.info & DF_RANDOM) continue;
        if (type->flags.plr & DF_PLR_SECRET) continue;
        vec_add(v, type);
    }
    int_map_iter_free(iter);
    vec_sort(v, (vec_cmp_f)_cmp_d_lvl);
    return v;
}
vec_ptr world_dun_types(void)
{
    dun_mgr_ptr      dm = dun_mgr();
    vec_ptr          v = vec_alloc(NULL);
    int_map_iter_ptr iter;

    for (iter = int_map_iter_alloc(_types());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        dun_type_ptr type = int_map_iter_current(iter);
        if (!type->max_dun_lvl) continue;
        if (type->flags.info & DF_RANDOM) continue;
        if (!dun_pos_interior(dm->world, type->world_pos)) continue;
        vec_add(v, type);
    }
    int_map_iter_free(iter);

    vec_sort(v, (vec_cmp_f)_cmp_d_lvl);
    return v;
}
int plr_max_dun_lvl(void)
{
    vec_ptr v = plr_dun_types();
    int max = 0, i;
    for (i = 0; i < vec_length(v); i++)
    {
        dun_type_ptr type = vec_get(v, i);
        if (type->plr_max_lvl > max)
            max = type->plr_max_lvl;
    }
    vec_free(v);
    return max;
}
dun_type_ptr dun_types_random(int level)
{
    dun_mgr_ptr  dm = dun_mgr();
    vec_ptr      v = world_dun_types();
    dun_ptr      skip = plr_dun();
    dun_type_ptr result = NULL;
    int          i, tot = 0;

    assert(skip);
    for (i = 0; i < vec_length(v); i++)
    {
        dun_type_ptr type = vec_get(v, i);
        if (type->id == skip->type->id) continue;
        if (type->min_dun_lvl > level) continue;
        if (type->flags.plr & (DF_PLR_SECRET | DF_PLR_FAILED)) continue;
        if (!dun_pos_interior(dm->world, type->world_pos)) continue;
        tot++;
    }
    if (tot)
    {
        int n = randint0(tot);
        for (i = 0; i < vec_length(v) && !result; i++)
        {
            dun_type_ptr type = vec_get(v, i);
            if (type->id == skip->type->id) continue;
            if (type->min_dun_lvl > level) continue;
            if (type->flags.plr & (DF_PLR_SECRET | DF_PLR_FAILED)) continue;
            if (!dun_pos_interior(dm->world, type->world_pos)) continue;
            n--;
            if (n < 0) result = type;
        }
    }
    vec_free(v);
    return result;
}
dun_type_ptr dun_types_choose(cptr prompt, bool wizard)
{
    vec_ptr      types = wizard ? world_dun_types() : plr_dun_types();
    dun_type_ptr type = NULL;

    if (!vec_length(types))
    {
        msg_print("You haven't entered and dungeons yet.");
    }
    else if (vec_length(types) == 1)
    {
        type = vec_get(types, 0);
    }
    else
    {
        dun_mgr_ptr dm = dun_mgr();
        doc_ptr     doc = doc_alloc(80);
        int         i, default_idx = -1;
        doc_printf(doc, "<color:U>%s</color>\n", prompt);
        for (i = 0; i < vec_length(types); i++)
        {
            dun_type_ptr type = vec_get(types, i);
            int lvl = MAX(type->plr_max_lvl, type->min_dun_lvl);
            char color = 'y';
            if (wizard && type->id == plr_dun_type()->id)
            {
                default_idx = i;
                color = 'r';
            }
            else if (type->flags.plr & DF_PLR_FAILED) color = 'D';
            else if (!dun_pos_interior(dm->world, type->world_pos)) color = 'D';
            doc_printf(doc, " <color:%c>%c</color>) ", color, I2A(i));

            color = 'w';
            if (type->flags.plr & DF_PLR_COMPLETED) color = 'G';
            else if (type->flags.plr & DF_PLR_FAILED) color = 'r';
            else if (type->flags.plr & DF_PLR_SECRET) color = 'D';
            doc_printf(doc, "<color:%c>%-21s</color> DL%3d\n", color, type->name, lvl);
        }

        Term_save();
        for (;;)
        {
            int cmd, idx;
            doc_sync_menu(doc);
            cmd = inkey_special(TRUE);
            if (cmd == ESCAPE) break;
            if (cmd == '\r' && wizard) idx = default_idx;
            else idx = A2I(cmd);
            if (0 <= idx && idx < vec_length(types))
            {
                type = vec_get(types, idx);
                if (type->flags.plr & DF_PLR_FAILED) continue;
                else if (!dun_pos_interior(dm->world, type->world_pos)) continue;
                break;
            }
        }
        Term_load();
        doc_free(doc);
    }

    vec_free(types);
    return type;
}
/************************************************************************
 * Surface
 ************************************************************************/
int _surface_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (is_daytime() && mon_race_vuln(race, GF_LIGHT))
        return 0;
    return prob;
}
static dun_type_ptr _surface(void)
{
    dun_type_ptr type = dun_type_alloc(D_SURFACE, "The Wilderness");
    type->place_floor = dun_place_dirt;
    type->mon_alloc_f = _surface_mon_alloc;
    type->mon_alloc_tbl = vec_filter(mon_alloc_tbl, (vec_item_p)mon_alloc_surface);
    type->flags.info = DF_NO_DESTRUCT | DF_NO_QUAKE;
    return type;
}

/************************************************************************
 * World: D_WORLD is never entered by the player. It exists to define the
 * terrain layout of the surface (towns, roads, entrances, etc).
 ************************************************************************/
static dun_type_ptr _world(void)
{
    return dun_type_alloc(D_WORLD, "The World");
}

/************************************************************************
 * Quest
 ************************************************************************/
static dun_type_ptr _quest(void)
{
    dun_type_ptr type = dun_type_alloc(D_QUEST, "A Quest");
    type->flags.info = DF_NO_DESTRUCT | DF_NO_QUAKE | DF_NO_GENOCIDE;
    return type;
}

/************************************************************************
 * Dungeon Guardians
 ************************************************************************/
static bool _restore_guardian(dun_ptr dun, int r_idx)
{
    bool found = FALSE;
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(dun->mon);
            int_map_iter_is_valid(iter) && !found;
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        if (mon->race->id == r_idx)
        {
            mon->mflag2 |= MFLAG2_QUESTOR; /* paranoia */
            found = TRUE;
        }
    }
    int_map_iter_free(iter);
    return found;
}
static bool _alloc_guardian(dun_type_ptr me, dun_ptr dun)
{
    mon_race_ptr race = mon_race_lookup(me->final_guardian);
    int j;

    assert(cave == dun); /* old place_monster code won't work otherwise ... */
    assert(me->final_guardian);

    /* already allocated ? */
    if (_restore_guardian(dun, race->id)) return TRUE;

    /* already slain? this is a bug! */
    if (mon_race_is_dead_unique(race))
    {
        msg_print("It seems this level was guarded by someone before.");
        me->flags.plr |= DF_PLR_COMPLETED;
        return FALSE;
    }
    for (j = 1000; j > 0; j--)
    {
        point_t pos = dun_random_mon_pos(cave, race);
        mon_ptr mon;
        dun_grid_ptr grid;

        if (!dun_pos_interior(cave, pos)) continue;
        if (!dun_allow_mon_at(dun, pos)) continue;
        grid = dun_grid_at(dun, pos);
        if (!cell_allow_mon_race(grid, race)) continue;

        /* allocated on a different level? move the guardian here! */
        if (mon_race_is_unique(race) && race->alloc.cur_num)
        {
            mon_ptr mon = dun_mgr_relocate_unique(race->id, dun, pos);
            if (!mon) /* bug! cur_num implies mon is somewhere in dun_mgr()->dungeons */
                return FALSE;
            mon->mflag2 |= MFLAG2_QUESTOR;
            break;
        }

        /* Handle normal allocation */
        mon = place_monster_aux(who_create_null(), pos, race, PM_QUESTOR | PM_ALLOW_GROUP);
        if (mon)
        {
            assert(mon->race->id == race->id);
            mon->mflag2 |= MFLAG2_QUESTOR;
            break;
        }
    }
    if (!j) return FALSE; /* Failed to place?! */

    msg_format("<color:v>Beware!</color> This level is guarded by <color:R>%s</color>.",
        race->name);
    return TRUE;
}
static void _conquer(dun_type_ptr me)
{
    msg_format("<color:G>You have conquered <color:U>%s</color>!</color>", me->name);
    virtue_add(VIRTUE_VALOUR, 5);
    gain_chosen_stat();
    plr->fame += randint1(3);
    msg_add_tiny_screenshot(50, 24);
    me->flags.plr |= DF_PLR_COMPLETED;
}
static bool _is_completed(dun_type_ptr me) { return BOOL(me->flags.plr & DF_PLR_COMPLETED); }
static void _reward_one(dun_ptr dun, point_t pos)
{
    obj_t  forge = {0};
    if (make_object(&forge, dun->difficulty, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
        dun_drop_near(dun, &forge, pos);
}
static void _reward(dun_ptr dun, point_t pos)
{
    int i, ct = dun->difficulty/25 + 1;
    for (i = 0; i < ct; i++)
        _reward_one(dun, pos);
}
/************************************************************************
 * Random Guardians
 ************************************************************************/
static bool _r_can_quest(mon_race_ptr race)
{
    if (race->alloc.flags & RFA_WILD_ONLY) return FALSE;
    if (mon_race_is_aquatic(race)) return FALSE;
    if (mon_race_can_multiply(race)) return FALSE;
    if (mon_race_is_friendly(race)) return FALSE;
    if (mon_race_is_dead_unique(race)) return FALSE;
    return TRUE;
}

static bool _get_guardian(dun_type_ptr me)
{
    int  attempt;

    me->final_guardian = 0;

    mon_alloc_clear_filters();
    mon_alloc_push_filter(_r_can_quest);
    mon_alloc_push_filter(mon_race_is_unique);

    for (attempt = 0; attempt < 1000; attempt++)
    {
        mon_race_ptr race;
        int min_lev = me->max_dun_lvl;
        int max_lev = me->max_dun_lvl + 9;
        if (me->max_dun_lvl < 10)
            max_lev -= 2;
        else if (me->max_dun_lvl < 20)
            max_lev -= 1;
        else if (me->max_dun_lvl > 80)
            max_lev += 2;
        else if (me->max_dun_lvl > 70)
            max_lev += 1;

        race = mon_alloc_choose_aux2(mon_alloc_tbl, max_lev, min_lev, GMN_QUESTOR);
        if (!race) break;
        if (race->flagsx & RFX_QUESTOR) continue;
        if (race->flagsx & RFX_GUARDIAN) continue;
        if (race->alloc.flags & RFA_NO_QUEST) continue;
        if (race->alloc.rarity > 100) continue;
        if (race->alloc.lvl > max_lev) continue;
        if (race->alloc.lvl > min_lev || attempt > 5000)
        {
            #ifdef DEVELOPER
            if (0 || plr->wizard)
            {
                msg_format("The next guardian for %s is %s on L%d.",
                    me->name, race->name, me->max_dun_lvl);
            }
            #endif
            me->final_guardian = race->id;
            assert(mon_race_is_unique(race)); /* otherwise, don't set RFX_GUARDIAN! */
            race->flagsx |= RFX_GUARDIAN;
            break;
        }
    }
    mon_alloc_clear_filters();
    return me->final_guardian != 0;
}

/************************************************************************
 * Amber
 ************************************************************************/
static void _amber_next_guardian(dun_type_ptr me)
{
    me->final_guardian = 0;
    while (me->max_dun_lvl < 85)
    {
        me->max_dun_lvl += 7 + _1d(3);
        if (_get_guardian(me)) break;
    }
    if (!me->final_guardian)
    {
        me->max_dun_lvl = 99;
        me->final_guardian = mon_race_parse("p.Oberon")->id;
    }
}
static void _amber_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (me->max_dun_lvl == me->min_dun_lvl)
        _amber_next_guardian(me);
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _amber_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;

        if (mon_race_is_(mon->race, "p.Oberon"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            _reward(dun, mon->pos);
            virtue_add(VIRTUE_VALOUR, 5);
            plr->fame += 5 + randint1(5);

            me->max_dun_lvl++;
            me->final_guardian = mon_race_parse("J.Chaos")->id;
        }
        else if (mon_race_is_(mon->race, "J.Chaos"))
        {
            _conquer(me);
        }
        else /* random guardians (formerly quests) */
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            _reward(dun, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame += _1d(2);
            _amber_next_guardian(me);
        }
    }
}
static dun_type_ptr _amber(void)
{
    dun_type_ptr type = dun_type_alloc(D_AMBER, "The World of Amber");
    type->desc = "the fabulous world of Amber";
    type->min_dun_lvl = 15;
    type->max_dun_lvl = 15; /* XXX roll first random guardian on *first* visit */
    type->change_dun_f = _amber_change_dun;
    type->kill_mon_f = _amber_kill_mon;
    type->flags.info = DF_KNOWN;
    return type;
}
/************************************************************************
 * Stronghold
 ************************************************************************/
static void _stronghold_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _stronghold_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "p.Wutugu"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            _reward_one(dun, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;

            me->max_dun_lvl++;
            me->final_guardian = mon_race_parse("p.Meng Huo")->id;
        }
        else
        {
            int k_idx = lookup_kind(TV_AMULET, SV_ANY);

            if (plr->prace == RACE_MON_BEHOLDER)
                k_idx = lookup_kind(TV_RING, SV_ANY);

            object_prep(&forge, k_idx);
            if (equip_can_wield_kind(forge.tval, forge.sval))
            {
                apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_QUEST);
                dun_drop_near(dun, &forge, mon->pos);
            }
            else
                _reward_one(dun, mon->pos);
            _conquer(me);
        }
    }
}
static int _stronghold_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->alloc.dun_type_id == D_STRONGHOLD) return prob * 50;
    switch (mon_race_char(race))
    {
    case 'p': return prob * 10;
    case 'H': return prob * 3;
    case 'g': return prob * 2;
    }
    return prob;
}
static rect_t _stronghold_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _stronghold(void)
{
    dun_type_ptr type = dun_type_alloc(D_STRONGHOLD, "The Stronghold");
    type->desc = "the gateway to the Stronghold of the Southerlings";
    type->min_dun_lvl = 1;
    type->max_dun_lvl = 14;
    type->final_guardian = mon_race_parse("p.Wutugu")->id;
    type->size_f = _stronghold_size;
    type->change_dun_f = _stronghold_change_dun;
    type->kill_mon_f = _stronghold_kill_mon;
    type->mon_alloc_f = _stronghold_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN;
    type->flags.info = DF_KNOWN;
    return type;
}

/************************************************************************
 * Orc Cave
 ************************************************************************/
static void _orc_cave_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _orc_cave_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "o.Bolg"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            _reward_one(dun, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;

            me->max_dun_lvl++;
            me->final_guardian = mon_race_parse("o.Azog")->id;
        }
        else
        {
            int k_idx = lookup_kind(TV_RING, SV_ANY);

            object_prep(&forge, k_idx);
            if (equip_can_wield_kind(forge.tval, forge.sval))
            {
                apply_magic_ego = EGO_RING_COMBAT;
                apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_FORCE_EGO);
                dun_drop_near(dun, &forge, mon->pos);
            }
            else
                _reward_one(dun, mon->pos);
            _conquer(me);
        }
    }
}
static int _orc_cave_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_orc(race)) return prob * 50;
    if (mon_race_is_animal(race)) return prob * 10;
    switch (mon_race_char(race))
    {
    case 'k': return prob * 3;
    case 'O': return prob * 3;
    }
    return prob;
}
static rect_t _orc_cave_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _orc_cave_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 70)
        dun_place_granite(dun, pos);
    else
        dun_place_mountain_wall(dun, pos);
}
static void _orc_cave_floor(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 70)
        dun_place_dirt(dun, pos);
    else
        dun_place_grass(dun,pos);
}
static dun_type_ptr _orc_cave(void)
{
    dun_type_ptr type = dun_type_alloc(D_ORC_CAVE, "The Orc Cave");
    type->desc = "a dark tunnel leading to the Orc Cave";
    type->place_wall = _orc_cave_wall;
    type->place_floor = _orc_cave_floor;
    type->min_dun_lvl = 15;
    type->max_dun_lvl = 22;
    type->final_guardian = mon_race_parse("o.Bolg")->id;
    type->size_f = _orc_cave_size;
    type->change_dun_f = _orc_cave_change_dun;
    type->kill_mon_f = _orc_cave_kill_mon;
    type->mon_alloc_f = _orc_cave_mon_alloc;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_WATER | DF_GEN_CAVERN | DF_GEN_LAKE_TREE | DF_GEN_DESTROY;
    type->flags.info = DF_KNOWN;
    return type;
}
/************************************************************************
 * Lonely Mountain
 ************************************************************************/
static void _lonely_mountain_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _lonely_mountain_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        obj_drop_theme = R_DROP_SPELLBOOK;
        if (!make_object(&forge, 45, AM_NO_FIXED_ART | AM_TAILORED))
        {
            obj_drop_theme = R_DROP_SPELLBOOK;
            make_object(&forge, 45, AM_NO_FIXED_ART | AM_GOOD);
        }
        dun_drop_near(dun, &forge, mon->pos);
        _conquer(me);
    }
}
static int _lonely_mountain_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_dragon(race)) return prob * 5;
    if (mon_race_is_giant(race)) return prob * 2;
    return prob;
}
static rect_t _lonely_mountain_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _lonely_mountain_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_granite(dun, pos);
    else
        dun_place_chasm(dun, pos);
}
static dun_type_ptr _lonely_mountain(void)
{
    dun_type_ptr type = dun_type_alloc(D_LONELY_MOUNTAIN, "The Lonely Mountain");
    type->desc = "the entrance to the Lonely Mountain";
    type->place_floor = dun_place_dirt;
    type->place_wall = _lonely_mountain_wall;
    type->min_dun_lvl = 30;
    type->max_dun_lvl = 40;
    type->final_guardian = mon_race_parse("D.Smaug")->id;
    type->size_f = _lonely_mountain_size;
    type->change_dun_f = _lonely_mountain_change_dun;
    type->kill_mon_f = _lonely_mountain_kill_mon;
    type->mon_alloc_f = _lonely_mountain_mon_alloc;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_LAVA | DF_GEN_CAVERN | DF_GEN_DESTROY
                    | DF_GEN_LAKE_LAVA | DF_GEN_LAKE_TREE | DF_GEN_LAKE_RUBBLE;
    return type;
}
/************************************************************************
 * The Forest
 ************************************************************************/
static void _forest_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _forest_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        _reward(mon->dun, mon->pos);
        _conquer(me);
    }
}
static int _forest_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_animal(race)) return prob * 20;
    if (race->alloc.flags & RFA_WILD_WOOD) return prob * 10;
    if (mon_race_is_(race, "S.mirkwood")) return prob * 50;
    switch (mon_race_char(race))
    {
    case 'S':
    case 'J':
    case 'Z':
    case '#': return prob * 3;
    }
    return prob;
}
static void _forest_floor(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 85)
        dun_place_grass(dun, pos);
    else if (roll <= 90)
        dun_place_flower(dun, pos);
    else
        dun_place_shallow_water(dun, pos);
}
static void _forest_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_tree(dun, pos);
    else
        dun_place_granite(dun, pos);
}
static void _forest_streamer(dun_ptr dun, point_t pos)
{
    if (!one_in_(3)) return;
    dun_place_granite(dun, pos);
}
static void _forest_aux(dun_type_ptr type)
{
    type->place_floor = _forest_floor;
    type->place_wall = _forest_wall;
    type->place_outer_wall = dun_place_brake;
    type->place_inner_wall = dun_place_tree;
    type->place_stream1 = _forest_streamer;
    type->place_stream2 = dun_place_brake;
    type->mon_alloc_f = _forest_mon_alloc;
    type->flags.gen = DF_GEN_NO_DOORS | DF_GEN_RIVER_WATER;
}
static dun_type_ptr _forest(void)
{
    dun_type_ptr type = dun_type_alloc(D_FOREST, "The Forest");
    type->desc = "a path leading to a Forest";
    _forest_aux(type);
    type->min_dun_lvl = 20;
    type->max_dun_lvl = 32;
    type->final_guardian = mon_race_parse("S.Shelob")->id;
    type->change_dun_f = _forest_change_dun;
    type->kill_mon_f = _forest_kill_mon;
    type->flags.info |= DF_KNOWN;
    return type;
}
static dun_type_ptr _random_forest(void)
{
    dun_type_ptr type = dun_type_alloc(D_RANDOM_FOREST, "Random Forest");
    type->desc = "a path leading to an unknown forest";
    _forest_aux(type);
    type->min_dun_lvl = 15;
    type->max_dun_lvl = 35;
    type->flags.info |= DF_RANDOM;
    return type;
}
/************************************************************************
 * Camelot
 ************************************************************************/
static void _camelot_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _camelot_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};
        int     k_idx = lookup_kind(TV_SHIELD, SV_MIRROR_SHIELD);

        object_prep(&forge, k_idx);
        apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_QUEST);
        dun_drop_near(dun, &forge, mon->pos);
        _conquer(me);
    }
}
static int _camelot_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->alloc.dun_type_id == D_CAMELOT) return prob * 50;
    if (mon_race_is_knight(race)) return prob * 30;
    switch (mon_race_char(race))
    {
    case 'p': return prob * 10;
    case 'H': return prob * 3;
    case 'g': return prob * 2;
    case 'd': return prob * 7;
    }
    return prob;
}
static rect_t _camelot_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _camelot(void)
{
    dun_type_ptr type = dun_type_alloc(D_CAMELOT, "The Land of Camelot");
    type->desc = "the entrance to the fabled land of Camelot";
    type->min_dun_lvl = 25;
    type->max_dun_lvl = 35;
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->final_guardian = mon_race_parse("p.Arthur")->id;
    type->size_f = _camelot_size;
    type->change_dun_f = _camelot_change_dun;
    type->kill_mon_f = _camelot_kill_mon;
    type->mon_alloc_f = _camelot_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN;
    type->flags.info = DF_NO_STATS;
    return type;
}

/************************************************************************
 * Icky Cave
 ************************************************************************/
static void _icky_cave_init(dun_type_ptr me)
{
    mon_race_parse("i.Queen")->flagsx |= RFX_GUARDIAN;
    mon_race_parse("j.Ubbo-Sathla")->flagsx |= RFX_GUARDIAN;
}
static void _icky_cave_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _icky_cave_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "i.Queen"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;

            me->max_dun_lvl = 35;
            me->final_guardian = mon_race_parse("j.Ubbo-Sathla")->id;
        }
        else
        {
            int k_idx = lookup_kind(TV_RING, SV_ANY);

            object_prep(&forge, k_idx);
            if (equip_can_wield_kind(forge.tval, forge.sval))
            {
                apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_GREAT | AM_QUEST);
                dun_drop_near(dun, &forge, mon->pos);
            }
            else
                _reward_one(dun, mon->pos);
            _conquer(me);
        }
    }
}
static int _icky_cave_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    switch (mon_race_char(race))
    {
    case 'i': return prob * 50;
    case 'j': return prob * 30;
    case 'M': return prob * 10;
    }
    return prob;
}
static rect_t _icky_cave_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _icky_cave_floor(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 20)
        dun_place_swamp(dun, pos);
    else if (roll <= 80)
        dun_place_grass(dun, pos);
    else
        dun_place_shallow_water(dun, pos);
}
static dun_type_ptr _icky_cave(void)
{
    dun_type_ptr type = dun_type_alloc(D_ICKY_CAVE, "The Icky Cave");
    type->desc = "a slimy tunnel leading to the Icky Cave";
    type->place_floor = _icky_cave_floor;
    type->min_dun_lvl = 20;
    type->max_dun_lvl = 20;
    type->final_guardian = mon_race_parse("i.Queen")->id;
    type->size_f = _icky_cave_size;
    type->change_dun_f = _icky_cave_change_dun;
    type->kill_mon_f = _icky_cave_kill_mon;
    type->mon_alloc_f = _icky_cave_mon_alloc;
    type->init_f = _icky_cave_init;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_WATER | DF_GEN_CAVERN | DF_GEN_LAKE_TREE | DF_GEN_DESTROY;
    type->flags.info = DF_KNOWN | DF_NO_STATS;
    return type;
}
/************************************************************************
 * Moria
 ************************************************************************/
void _moria_pre_gen(dun_type_ptr me, dun_gen_ptr gen)
{
    if (gen->dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        dun_gen_lava_vault(gen);
}
static void _moria_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _moria_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        art_ptr art = arts_parse("\\.Khazad-dum");
        assert(art);
        if (art && !art->generated)
            create_named_art(art, mon->pos);
        plr->fame += 10;
        _conquer(me);
    }
}
static rect_t _moria_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static int _moria_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->alloc.dun_type_id == D_MORIA) return prob * 50;
    if (mon_race_is_char(race, 'P') && mon_race_is_giant(race)) return prob * 5; /* skip O */
    if (mon_race_is_demon(race)) return prob * 5;
    if (mon_race_is_dark_elf(race)) return prob * 5;
    if (mon_race_is_orc(race) || mon_race_is_troll(race)) return prob * 2;
    return prob;
}
static void _moria_wall(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 60)
        dun_place_granite(dun, pos);
    else if (roll <= 80)
        dun_place_magma_aux(dun, pos, 20, 90);
    else
        dun_place_quartz_aux(dun, pos, 40, 90);
}
static dun_type_ptr _moria(void)
{
    dun_type_ptr type = dun_type_alloc(D_MORIA, "The Mines of Moria");
    type->desc = "the gates of the Mines of Moria";
    type->place_wall = _moria_wall;
    type->min_dun_lvl = 50;
    type->max_dun_lvl = 65;
    type->final_guardian = mon_race_parse("U.Lungorthin")->id;
    type->change_dun_f = _moria_change_dun;
    type->kill_mon_f = _moria_kill_mon;
    type->pre_gen_f = _moria_pre_gen;
    type->size_f = _moria_size;
    type->mon_alloc_f = _moria_mon_alloc;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_CAVERN | DF_GEN_DESTROY
                    | DF_GEN_RIVER_WATER | DF_GEN_RIVER_LAVA | DF_GEN_LAKE_LAVA;
    return type;
}
/************************************************************************
 * Mountain
 ************************************************************************/
static void _mountain_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _mountain_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "P.Utgard"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;

            me->max_dun_lvl++;
            me->final_guardian = mon_race_parse("O.Shuten-douji")->id;
        }
        else
        {
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            _conquer(me);
        }
    }
}
static int _mountain_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_char(race, 'B') && (race->alloc.flags & RFA_WILD_MOUNTAIN)) return prob * 50;
    if (mon_race_is_char(race, 'P') && mon_race_is_giant(race)) return prob * 50;
    if (mon_race_is_troll(race)) return prob * 30;
    if (mon_race_is_orc(race)) return prob * 10;
    if (mon_race_can_fly(race)) return prob * 10;
    switch (mon_race_char(race))
    {
    case 'Y': return prob * 3;
    case 'H': return prob * 3;
    case 'O': return prob * 5;
    }
    return prob;
}
static rect_t _mountain_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _mountain_aux(dun_type_ptr type)
{
    type->place_floor = dun_place_grass;
    type->place_wall = dun_place_mountain_wall;
    /* Using permanent walls for OUTER can trap the player on cavern levels, 
     * since stairs may be inaccessible. This is unlikely, but I am paranoid ...
    type->place_outer_wall = dun_place_mountain_wall; */
    type->place_outer_wall = dun_place_granite;
    type->place_inner_wall = dun_place_granite;
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->size_f = _mountain_size;
    type->mon_alloc_f = _mountain_mon_alloc;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_WATER | DF_GEN_CAVERN | DF_GEN_NO_DOORS;
}
static dun_type_ptr _mountain(void)
{
    dun_type_ptr type = dun_type_alloc(D_MOUNTAIN, "The Mountain");
    type->desc = "a dark tunnel leading to the Mountain";
    _mountain_aux(type);
    type->min_dun_lvl = 35;
    type->max_dun_lvl = 44;
    type->final_guardian = mon_race_parse("P.Utgard")->id;
    type->change_dun_f = _mountain_change_dun;
    type->kill_mon_f = _mountain_kill_mon;
    type->flags.info |= DF_NO_STATS;
    return type;
}
static dun_type_ptr _random_mountain(void)
{
    dun_type_ptr type = dun_type_alloc(D_RANDOM_MOUNTAIN, "Random Mountain");
    type->desc = "a dark tunnel leading to an unknown mountain";
    _mountain_aux(type);
    type->min_dun_lvl = 30;
    type->max_dun_lvl = 50;
    type->flags.info |= DF_RANDOM;
    return type;
}
/************************************************************************
 * Volcano
 ************************************************************************/
void _volcano_pre_gen(dun_type_ptr me, dun_gen_ptr gen)
{
    dun_gen_lava_vault(gen);
}
static rect_t _volcano_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static bool _volcano_p(mon_race_ptr race)
{
    if (mon_race_immune(race, GF_FIRE)) return TRUE;
    if (mon_race_can_fly(race)) return TRUE;
    if (race->alloc.flags & RFA_WILD_VOLCANO) return TRUE;
    return FALSE;
}
static void _volcano_floor(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 40)
        dun_place_dirt(dun, pos);
    else if (roll <= 80)
        dun_place_shallow_lava(dun, pos);
    else
        dun_place_deep_lava(dun, pos);
}
static void _volcano_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_granite(dun, pos);
    else
        dun_place_chasm(dun, pos);
}
static dun_type_ptr _random_volcano(void)
{
    dun_type_ptr type = dun_type_alloc(D_RANDOM_VOLCANO, "Random Volcano");
    type->desc = "a dark tunnel leading to an unknown volcano";
    type->place_wall = _volcano_wall;
    type->place_floor = _volcano_floor;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 60;
    type->pre_gen_f = _volcano_pre_gen;
    type->size_f = _volcano_size;
    type->mon_alloc_tbl = vec_filter(mon_alloc_tbl, (vec_item_p)_volcano_p);
    type->flags.gen = DF_GEN_CAVE | DF_GEN_NO_DOORS;
    type->flags.info = DF_RANDOM;
    return type;
}
/************************************************************************
 * Castle
 ************************************************************************/
static void _castle_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _castle_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static int _castle_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_human(race)) return prob * 50;
    if (mon_race_is_knight(race)) return prob * 10;
    if (mon_race_is_demon(race)) return prob * 6;
    switch (mon_race_char(race))
    {
    case 'H': return prob * 3;
    case 'g': return prob * 5;
    case 'h': return prob * 10;
    }
    return prob;
}
static rect_t _castle_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _castle(void)
{
    dun_type_ptr type = dun_type_alloc(D_CASTLE, "The Old Castle");
    type->desc = "the gates of the Old Castle";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 65;
    type->final_guardian = mon_race_parse("p.Layzark")->id;
    type->size_f = _castle_size;
    type->change_dun_f = _castle_change_dun;
    type->kill_mon_f = _castle_kill_mon;
    type->mon_alloc_f = _castle_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Isengard
 ************************************************************************/
static void _isengard_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _isengard_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static int _isengard_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_orc(race)) return prob * 10;
    if (mon_race_is_troll(race)) return prob * 2;
    switch (mon_race_char(race))
    {
    case 'p':
    case 'h':
        if (!who_is_plr(summon_specific_who) && race->align > 0) return 0;
        if (race->body.class_id == CLASS_MAGE) return prob * 25;
        if (mon_race_is_evil(race)) return prob * 10;
        return prob * 5;
    }
    return prob;
}
static rect_t _isengard_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _isengard(void)
{
    dun_type_ptr type = dun_type_alloc(D_ISENGARD, "The Tower of Isengard");
    type->desc = "the Tower of Isengard";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 60;
    type->final_guardian = mon_race_parse("p.Saruman")->id;
    type->size_f = _isengard_size;
    type->change_dun_f = _isengard_change_dun;
    type->kill_mon_f = _isengard_kill_mon;
    type->mon_alloc_f = _isengard_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA | DF_GEN_TOWER;
    return type;
}
/************************************************************************
 * Minas Morgul
 ************************************************************************/
static void _minas_morgul_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _minas_morgul_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static int _minas_morgul_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->alloc.dun_type_id == D_MINAS_MORGUL) return prob * 50;
    if (mon_race_is_undead(race)) return prob * 20;
    if (!who_is_plr(summon_specific_who) && race->align > 0) return 0;
    switch (mon_race_char(race)) /* Minas Morgul is the Tower of Sorcery */
    {
    case 'p':
    case 'h':
        if (race->body.class_id == CLASS_MAGE) return prob * 20;
        if (mon_race_is_evil(race)) return prob * 5;
        break;
    }
    return prob;
}
static rect_t _minas_morgul_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _minas_morgul(void)
{
    dun_type_ptr type = dun_type_alloc(D_MINAS_MORGUL, "Minas Morgul");
    type->desc = "the gates of Minas Morgul";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 70;
    type->max_dun_lvl = 80;
    type->final_guardian = mon_race_parse("W.King")->id;
    type->size_f = _minas_morgul_size;
    type->change_dun_f = _minas_morgul_change_dun;
    type->kill_mon_f = _minas_morgul_kill_mon;
    type->mon_alloc_f = _minas_morgul_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA | DF_GEN_TOWER;
    return type;
}
/************************************************************************
 * Dark Tower
 ************************************************************************/
static void _dark_tower_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _dark_tower_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "p.Mouth of Sauron"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;

            me->max_dun_lvl = 90;
            me->final_guardian = mon_race_parse("p.Sauron")->id;
        }
        else
        {
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            _conquer(me);
        }
    }
}
static int _dark_tower_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_undead(race)) return prob * 5;
    if (mon_race_is_demon(race)) return prob * 5;
    if (!who_is_plr(summon_specific_who) && race->align > 0) return 0;
    return prob;
}
static rect_t _dark_tower_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _dark_tower(void)
{
    dun_type_ptr type = dun_type_alloc(D_DARK_TOWER, "The Dark Tower");
    type->desc = "the Dark Tower";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 80;
    type->max_dun_lvl = 80;
    type->final_guardian = mon_race_parse("p.Mouth of Sauron")->id;
    type->size_f = _dark_tower_size;
    type->change_dun_f = _dark_tower_change_dun;
    type->kill_mon_f = _dark_tower_kill_mon;
    type->mon_alloc_f = _dark_tower_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA | DF_GEN_TOWER;
    type->flags.info = DF_NO_LIGHT;
    return type;
}
/************************************************************************
 * Mount Doom
 ************************************************************************/
void _mount_doom_pre_gen(dun_type_ptr me, dun_gen_ptr gen)
{
    dun_gen_lava_vault(gen);
}
static rect_t _mount_doom_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static dun_type_ptr _mount_doom(void)
{
    dun_type_ptr type = dun_type_alloc(D_MOUNT_DOOM, "Mount Doom");
    type->desc = "Mount Doom";
    type->min_dun_lvl = 95;
    type->max_dun_lvl = 95;
    type->pre_gen_f = _mount_doom_pre_gen;
    type->size_f = _mount_doom_size;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_CAVERN | DF_GEN_LAKE_LAVA | DF_GEN_RIVER_LAVA | DF_GEN_DESTROY;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Angband
 ************************************************************************/
static void _angband_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _angband_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "C.Carcharoth"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;
            me->max_dun_lvl = 99;
            me->final_guardian = mon_race_parse("U.Gothmog")->id;
        }
        else if (mon_race_is_(mon->race, "U.Gothmog"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 5);
            plr->fame += 5;
            me->max_dun_lvl = 100;
            me->final_guardian = mon_race_parse("P.Morgoth")->id;
        }
        else
        {
            _conquer(me);
        }
    }
}
static dun_type_ptr _angband(void)
{
    dun_type_ptr type = dun_type_alloc(D_ANGBAND, "The Pits of Angband");
    type->desc = "the gates of the Pits of Angband";
    type->min_dun_lvl = 90;
    type->max_dun_lvl = 90;
    type->final_guardian = mon_race_parse("C.Carcharoth")->id;
    type->change_dun_f = _angband_change_dun;
    type->kill_mon_f = _angband_kill_mon;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_CAVERN | DF_GEN_DESTROY
                    | DF_GEN_RIVER_WATER | DF_GEN_LAKE_LAVA | DF_GEN_RIVER_LAVA;
    type->flags.plr = DF_PLR_SECRET; /* cf _sauron_kill_mon */
    return type;
}
/************************************************************************
 * Mount Olympus XXX Olympians need playtesting and re-design XXX
 ************************************************************************/
static void _olympus_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _olympus_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static void _olympus_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 60)
        dun_place_granite(dun, pos);
    else
        dun_place_permanent(dun, pos);
}
static dun_type_ptr _olympus(void)
{
    dun_type_ptr type = dun_type_alloc(D_OLYMPUS, "Mount Olympus");
    type->desc = "a steep path leading to Mount Olympus";
    type->place_wall = _olympus_wall;
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 80;
    type->max_dun_lvl = 90;
    type->final_guardian = mon_race_parse("P.Zeus")->id;
    type->change_dun_f = _olympus_change_dun;
    type->kill_mon_f = _olympus_kill_mon;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_WATER | DF_GEN_CAVERN | DF_GEN_NO_DOORS;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Dragons' Lair
 ************************************************************************/
static int _dragons_lair_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_dragon(race)) return prob * 5;
    if (mon_race_is_giant(race)) return prob * 2;
    return prob;
}
static rect_t _dragons_lair_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _dragons_lair_init(dun_type_ptr me)
{
    mon_race_parse("D.Narse")->flagsx |= RFX_GUARDIAN;
    mon_race_parse("D.Tiamat")->flagsx |= RFX_GUARDIAN;
}
static void _dragons_lair_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _dragons_lair_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "D.Narse"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;

            me->max_dun_lvl = 72;
            me->final_guardian = mon_race_parse("D.Tiamat")->id;
        }
        else
        {
            int k_idx = lookup_kind(TV_DRAG_ARMOR, SV_DRAGON_MULTIHUED);

            object_prep(&forge, k_idx);
            apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_GREAT | AM_QUEST);
            dun_drop_near(dun, &forge, mon->pos);
            _conquer(me);
        }
    }
}
static void _dragons_lair_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_granite(dun, pos);
    else
        dun_place_chasm(dun, pos);
}
static dun_type_ptr _dragons_lair(void)
{
    dun_type_ptr type = dun_type_alloc(D_DRAGONS_LAIR, "The Dragons' Lair");
    type->desc = "the entrance to the Dragons' Lair";
    type->place_floor = dun_place_dirt;
    type->place_wall = _dragons_lair_wall;
    type->min_dun_lvl = 60;
    type->max_dun_lvl = 60;
    type->final_guardian = mon_race_parse("D.Narse")->id;
    type->init_f = _dragons_lair_init;
    type->size_f = _dragons_lair_size;
    type->change_dun_f = _dragons_lair_change_dun;
    type->kill_mon_f = _dragons_lair_kill_mon;
    type->mon_alloc_f = _dragons_lair_mon_alloc;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_LAVA | DF_GEN_CAVERN | DF_GEN_DESTROY
                | DF_GEN_LAKE_LAVA | DF_GEN_LAKE_TREE | DF_GEN_LAKE_RUBBLE;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Random Sea
 ************************************************************************/
static bool _can_swim(mon_race_ptr race)
{
    if (mon_race_is_aquatic(race)) return TRUE;
    if (mon_race_can_swim(race)) return TRUE;
    if (mon_race_can_fly(race)) return TRUE;
    if (race->alloc.flags & RFA_WILD_OCEAN) return TRUE;
    return FALSE;
}
static rect_t _random_sea_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _sea_floor(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 80)
        dun_place_floor(dun, pos);
    else
        dun_place_shallow_water(dun, pos);
}
static void _sea_wall(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 80)
        dun_place_deep_water(dun, pos);
    else if (roll <= 90)
        dun_place_shallow_water(dun, pos);
    else
        dun_place_granite(dun, pos);
}
static void _sea_aux(dun_type_ptr type)
{
    type->place_floor = _sea_floor;
    type->place_wall = _sea_wall;
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->size_f = _random_sea_size;
    type->mon_alloc_tbl = vec_filter(mon_alloc_tbl, (vec_item_p)_can_swim);
    type->flags.gen = DF_GEN_LAKE_WATER | DF_GEN_RIVER_WATER | DF_GEN_DESTROY; 
}
static dun_type_ptr _random_sea(void)
{
    dun_type_ptr type = dun_type_alloc(D_RANDOM_SEA, "Random Sea");
    type->desc = "an entrance to an unknown submerged land";
    _sea_aux(type);
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 60;
    type->flags.info |= DF_RANDOM;
    return type;
}
static void _numenor_init(dun_type_ptr me)
{
    mon_race_parse("J.Jormungand")->flagsx |= RFX_GUARDIAN;
}
static void _numenor_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _numenor_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};
        art_ptr art = arts_parse("/.Wrath");

        assert(art);
        if (art && !art->generated)
            create_named_art(art, mon->pos);
        else if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static dun_type_ptr _numenor(void)
{
    dun_type_ptr type = dun_type_alloc(D_NUMENOR, "Numenor");
    type->desc = "a submerged way to the lost land of Numenor";
    _sea_aux(type);
    type->min_dun_lvl = 55;
    type->max_dun_lvl = 75;
    type->final_guardian = mon_race_parse("J.Jormungand")->id;
    type->init_f = _numenor_init;
    type->change_dun_f = _numenor_change_dun;
    type->kill_mon_f = _numenor_kill_mon;
    type->flags.info |= DF_NO_STATS;
    return type;
}
/************************************************************************
 * Wizard's Tower
 ************************************************************************/
static int _wizards_tower_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    switch (mon_race_char(race))
    {
    case 'p':
    case 'h':
        if (mon_race_is_magical(race)) return prob * race->spells->freq;
        break;
    case 'g':
        return prob * 10; /* golems are animated by magical means ... */
    }
    return prob;
}
static rect_t _wizards_tower_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _wizards_tower(void)
{
    dun_type_ptr type = dun_type_alloc(D_WIZARDS_TOWER, "The Wizards' Tower");
    type->desc = "the Wizards' Tower";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 60;
    type->size_f = _wizards_tower_size;
    type->mon_alloc_f = _wizards_tower_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA | DF_GEN_TOWER;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Monastery
 ************************************************************************/
static bool _is_monk(mon_race_ptr r)
{
    int i;
    assert(r->blows);
    for (i = 0; i < vec_length(r->blows); i++)
    {
        mon_blow_ptr blow = vec_get(r->blows, i);
        if (blow->method == RBM_MONK) return TRUE;
    }
    return FALSE;
}

static int _monastery_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (_is_monk(race)) return prob * 50;
    if (mon_race_is_human(race)) return prob * 5;
    return prob;
}
static rect_t _monastery_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _monastery(void)
{
    dun_type_ptr type = dun_type_alloc(D_MONASTERY, "The Monastery");
    type->desc = "the Monastery";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 35;
    type->max_dun_lvl = 50;
    type->size_f = _monastery_size;
    type->mon_alloc_f = _monastery_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA | DF_GEN_TOWER;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Graveyard
 ************************************************************************/
static int _graveyard_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_undead(race)) return prob * 50;
    if (mon_race_is_nonliving(race)) return prob * 10;
    return prob;
}
static void _graveyard_init(dun_type_ptr me)
{
    mon_race_parse("L.Vecna")->flagsx |= RFX_GUARDIAN;
}
static void _graveyard_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _graveyard_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static void _graveyard_floor(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 85)
        dun_place_floor(dun, pos);
    else
        dun_place_shallow_water(dun, pos);
}
static void _graveyard_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_granite(dun, pos);
    else
        dun_place_chasm(dun, pos);
}
static dun_type_ptr _graveyard(void)
{
    dun_type_ptr type = dun_type_alloc(D_GRAVEYARD, "The Graveyard");
    type->desc = "the Graveyard";
    type->place_floor = _graveyard_floor;
    type->place_wall = _graveyard_wall;
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 50;
    type->max_dun_lvl = 70;
    type->mon_alloc_f = _graveyard_mon_alloc;
    type->final_guardian = mon_race_parse("L.Vecna")->id;
    type->change_dun_f = _graveyard_change_dun;
    type->kill_mon_f = _graveyard_kill_mon;
    type->init_f = _graveyard_init;
    type->flags.gen = DF_GEN_RIVER_WATER | DF_GEN_LAKE_WATER | DF_GEN_ARENA
                    | DF_GEN_DESTROY | DF_GEN_LAKE_RUBBLE;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Sanctuary
 ************************************************************************/
static int _sanctuary_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->align < 0) return 0;
    if (race->light < 0 || race->lantern < 0) return 0;

    if (mon_race_is_char(race, 'A')) prob *= 5;
    if (race->align > ALIGN_NEUTRAL_GOOD) return prob * 3;

    if (mon_race_is_knight(race)) prob *= 5;
    else if (mon_race_is_human(race)) prob *= 2;

    if (mon_race_is_nonliving(race)) prob = (prob + 2)/3;

    return prob;
}
static rect_t _sanctuary_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static void _sanctuary_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 80)
        dun_place_granite(dun, pos);
    else
        dun_place_permanent(dun, pos);
}
static dun_type_ptr _sanctuary(void)
{
    dun_type_ptr type = dun_type_alloc(D_SANCTUARY, "The Sanctuary");
    type->desc = "the grand doorway of The Sanctuary of the Light";
    type->place_wall = _sanctuary_wall;
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 70; /* tower to heaven */
    type->size_f = _sanctuary_size;
    type->mon_alloc_f = _sanctuary_mon_alloc;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA | DF_GEN_TOWER;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Pandemonium
 ************************************************************************/
void _pandemonium_pre_gen(dun_type_ptr me, dun_gen_ptr gen)
{
    if (gen->dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        dun_gen_lava_vault(gen);
}
static void _pandemonium_init(dun_type_ptr me)
{
    mon_race_parse("U.Mephistopheles")->flagsx |= RFX_GUARDIAN;
}
static void _pandemonium_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _pandemonium_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        plr->fame += 10;
        _conquer(me);
    }
}
static int _pandemonium_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (!mon_race_is_evil(race)) return 0;
    if (!mon_race_immune(race, GF_FIRE) && !mon_race_can_fly(race)) return 0;
    if (mon_race_is_demon(race)) prob *= 50;
    return prob;
}
static rect_t _pandemonium_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _pandemonium_floor(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 40)
        dun_place_dirt(dun, pos);
    else if (roll <= 80)
        dun_place_shallow_lava(dun, pos);
    else
        dun_place_deep_lava(dun, pos);
}
static void _pandemonium_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_granite(dun, pos);
    else
        dun_place_chasm(dun, pos);
}
static dun_type_ptr _pandemonium(void)
{
    dun_type_ptr type = dun_type_alloc(D_PANDEMONIUM, "Pandemonium");
    type->desc = "hell: Abandon all hope ye who enter here";
    type->place_wall = _pandemonium_wall;
    type->place_floor = _pandemonium_floor;
    type->min_dun_lvl = 66;
    type->max_dun_lvl = 80;
    type->final_guardian = mon_race_parse("U.Mephistopheles")->id;
    type->init_f = _pandemonium_init;
    type->mon_alloc_f = _pandemonium_mon_alloc;
    type->change_dun_f = _pandemonium_change_dun;
    type->kill_mon_f = _pandemonium_kill_mon;
    type->pre_gen_f = _pandemonium_pre_gen;
    type->size_f = _pandemonium_size;
    type->flags.gen = DF_GEN_CAVERN | DF_GEN_CAVE | DF_GEN_LAKE_LAVA | DF_GEN_RIVER_LAVA | DF_GEN_DESTROY;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * R'lyeh
 ************************************************************************/
static int _rlyeh_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_demon(race)) return prob * 50;
    else if (mon_race_is_horror(race)) return prob * 30;
    return prob;
}
static void _rlyeh_init(dun_type_ptr me)
{
    mon_race_parse("U.Cthulhu")->flagsx |= RFX_GUARDIAN;
}
static void _rlyeh_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _rlyeh_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static void _rlyeh_floor(dun_ptr dun, point_t pos)
{
    int roll = _1d(100);
    if (roll <= 50)
        dun_place_floor(dun, pos);
    else if (roll <= 80)
        dun_place_shallow_water(dun, pos);
    else
        dun_place_deep_water(dun, pos);
}
static dun_type_ptr _rlyeh(void)
{
    dun_type_ptr type = dun_type_alloc(D_RLYEH, "R'lyeh");
    type->desc = "a way to R'lyeh";
    type->place_floor = _rlyeh_floor;
    type->min_dun_lvl = 80;
    type->max_dun_lvl = 96;
    type->mon_alloc_f = _rlyeh_mon_alloc;
    type->final_guardian = mon_race_parse("U.Cthulhu")->id;
    type->change_dun_f = _rlyeh_change_dun;
    type->kill_mon_f = _rlyeh_kill_mon;
    type->init_f = _rlyeh_init;
    type->flags.gen = DF_GEN_RIVER_WATER | DF_GEN_LAKE_WATER | DF_GEN_ARENA;
    type->flags.info = DF_NO_STATS;
    return type;
}
/************************************************************************
 * Dark Castle (of Vlad)
 ************************************************************************/
static void _dark_castle_init(dun_type_ptr me)
{
    mon_race_parse("V.Thuringwethil")->flagsx |= RFX_GUARDIAN;
    mon_race_parse("V.Vlad")->flagsx |= RFX_GUARDIAN;
}
static void _dark_castle_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _dark_castle_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon->dun;
        obj_t   forge = {0};

        if (mon_race_is_(mon->race, "V.Thuringwethil"))
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            plr->fame++;
            me->max_dun_lvl = 60;
            me->final_guardian = mon_race_parse("V.Vlad")->id;
        }
        else
        {
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);

            _conquer(me);
        }
    }
}
static int _dark_castle_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->align > 0) return 0;
    if (race->light + race->lantern > 0) return 0;
    if (mon_auras_find(race, GF_FIRE)) return 0;

    /* Vampires, wolves, bats and rats ... */
    switch (mon_race_char(race))
    {
    case 'V': return prob * 50;
    case 'b': return prob * 25;
    case 'C': return prob * 25;
    case 'r': return prob * 25;
    }
    if (mon_race_is_undead(race)) return prob * 10;
    return prob;
}
static rect_t _dark_castle_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _dark_castle(void)
{
    dun_type_ptr type = dun_type_alloc(D_DARK_CASTLE, "The Dark Castle");
    type->desc = "the gates of the Dark Castle";
    type->place_stream1 = NULL;
    type->place_stream2 = NULL;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 50;
    type->final_guardian = mon_race_parse("V.Thuringwethil")->id;
    type->size_f = _dark_castle_size;
    type->change_dun_f = _dark_castle_change_dun;
    type->kill_mon_f = _dark_castle_kill_mon;
    type->mon_alloc_f = _dark_castle_mon_alloc;
    type->init_f = _dark_castle_init;
    type->flags.gen = DF_GEN_NO_CAVE | DF_GEN_CURTAIN | DF_GEN_ARENA;
    type->flags.info = DF_NO_STATS | DF_NO_LIGHT;
    return type;
}
/************************************************************************
 * Dark Cave
 ************************************************************************/
static void _dark_cave_init(dun_type_ptr me)
{
    mon_race_parse("h.Malekith")->flagsx |= RFX_GUARDIAN;
}
static void _dark_cave_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !_is_completed(me))
        _alloc_guardian(me, dun);
}
static void _dark_cave_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->race->id == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        _reward(mon->dun, mon->pos);
        _conquer(me);
    }
}
static int _dark_cave_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (mon_race_is_dark_elf(race)) return prob * 50;
    switch (mon_race_char(race)) /* there are always spiders and bats in caves */
    {
    case 'S': return prob * 3;
    case 'b': return prob * 3;
    }
    return prob;
}
static rect_t _dark_cave_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static void _dark_cave_wall(dun_ptr dun, point_t pos)
{
    if (_1d(100) <= 90)
        dun_place_granite(dun, pos);
    else
        dun_place_chasm(dun, pos);
}
static dun_type_ptr _dark_cave(void)
{
    dun_type_ptr type = dun_type_alloc(D_DARK_CAVE, "The Dark Cave");
    type->desc = "a dark tunnel leading to caverns dark as pitch";
    type->init_f = _dark_cave_init;
    type->place_wall = _dark_cave_wall;
    type->place_floor = dun_place_dirt;
    type->min_dun_lvl = 30;
    type->max_dun_lvl = 40;
    type->final_guardian = mon_race_parse("h.Malekith")->id;
    type->size_f = _dark_cave_size;
    type->change_dun_f = _dark_cave_change_dun;
    type->kill_mon_f = _dark_cave_kill_mon;
    type->mon_alloc_f = _dark_cave_mon_alloc;
    type->flags.gen = DF_GEN_CAVE | DF_GEN_RIVER_LAVA | DF_GEN_CAVERN | DF_GEN_DESTROY
                | DF_GEN_LAKE_LAVA | DF_GEN_LAKE_TREE | DF_GEN_LAKE_RUBBLE;
    type->flags.info = DF_NO_STATS | DF_NO_LIGHT;
    return type;
}
