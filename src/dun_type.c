#include "angband.h"
#include "dun.h"
#include "dun_gen.h"
#include "int_map.h"
#include <assert.h>

typedef struct {
    int            id;
    cptr           parse;
    dun_type_ptr (*create_f)(void);
} _entry_t, *_entry_ptr;

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

static _entry_t _tbl[] = {
    { D_SURFACE, "Surface", _surface },
    { D_QUEST, "Quest", _quest },
    { D_AMBER, "Amber", _amber },
    { D_STRONGHOLD, "Stronghold", _stronghold },
    { D_ORC_CAVE, "Orc Cave", _orc_cave },
    { D_LONELY_MOUNTAIN, "Lonely Mountain", _lonely_mountain },
    { D_FOREST, "Forest", _forest },
    { D_CAMELOT, "Camelot", _camelot },
    { D_ICKY_CAVE, "Icky Cave", _icky_cave },
    { D_MORIA, "Moria", _moria },
    { D_MOUNTAIN, "Mountain", _mountain },
    { D_CASTLE, "Castle", _castle },
    { D_ISENGARD, "Isengard", _isengard },
    { D_MINAS_MORGUL, "Minas Morgul", _minas_morgul },
    { D_DARK_TOWER, "Dark Tower", _dark_tower },
    { D_MOUNT_DOOM, "Mount Doom", _mount_doom },
    { D_ANGBAND, "Angband", _angband },
    { D_OLYMPUS, "Olympus", _olympus },
    { 0 }
};

static void _set_feat(s16b feat[100], s16b which, int start, int count)
{
    int i;
    for (i = 0; i < count; i++)
    {
        int j = start + i;
        if (j >= 100) break;
        feat[j] = which;
    }
}

static dun_type_ptr dun_type_alloc(int id, cptr name)
{
    dun_type_ptr type = malloc(sizeof(dun_type_t));
    memset(type, 0, sizeof(dun_type_t));
    type->id = id;
    type->name = name;
    type->desc = name;
    type->stream1 = feat_magma_vein;
    type->stream2 = feat_quartz_vein;
    type->feat_wall_outer = feat_granite;
    type->feat_wall_inner = feat_granite;
    type->feat_wall_solid = feat_granite;
    _set_feat(type->floor_type, feat_floor, 0, 100);
    _set_feat(type->fill_type, feat_granite, 0, 100);
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
        for (i = 0; !type; i++)
        {
            _entry_ptr e = &_tbl[i];
            if (e->id != id) continue;
            assert(e->create_f);
            if (!e->create_f) return NULL;
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
        /* dungeon types should be constrained to just a single world, 
         * but, D_AMBER, being located at the crossroads to many worlds,
         * is an exception. */
        type->world_pos = point_create(0, 0);
    }
    int_map_iter_free(iter);
}

static void dun_type_load(dun_type_ptr type, savefile_ptr file)
{
    type->max_dun_lvl = savefile_read_s16b(file);
    type->final_guardian = savefile_read_s16b(file);
    type->world_pos.x = savefile_read_s16b(file);
    type->world_pos.y = savefile_read_s16b(file);
    type->plr_max_lvl = savefile_read_s16b(file);
    type->last_recall = savefile_read_byte(file);
    type->last_shop   = savefile_read_byte(file);
    type->plr_flags   = savefile_read_u16b(file);
    if (type->load_f) type->load_f(type, file);
}
static void dun_type_save(dun_type_ptr type, savefile_ptr file)
{
    savefile_write_u16b(file, type->id);
    savefile_write_s16b(file, type->max_dun_lvl);
    savefile_write_s16b(file, type->final_guardian);
    savefile_write_s16b(file, type->world_pos.x);
    savefile_write_s16b(file, type->world_pos.y);
    savefile_write_s16b(file, type->plr_max_lvl);
    savefile_write_byte(file, type->last_recall);
    savefile_write_byte(file, type->last_shop);
    savefile_write_u16b(file, type->plr_flags);
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
        if (type->flags & DF_RANDOM) continue;
        if (type->plr_flags & DFP_SECRET) continue;
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
        if (type->flags & DF_RANDOM) continue;
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
        if (type->id == skip->dun_type_id) continue;
        if (type->min_dun_lvl > level) continue;
        if (type->plr_flags & (DFP_SECRET | DFP_FAILED)) continue;
        if (!dun_pos_interior(dm->world, type->world_pos)) continue;
        tot++;
    }
    if (tot)
    {
        int n = randint0(tot);
        for (i = 0; i < vec_length(v) && !result; i++)
        {
            dun_type_ptr type = vec_get(v, i);
            if (type->id == skip->dun_type_id) continue;
            if (type->min_dun_lvl > level) continue;
            if (type->plr_flags & (DFP_SECRET | DFP_FAILED)) continue;
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
            else if (type->plr_flags & DFP_FAILED) color = 'D';
            else if (!dun_pos_interior(dm->world, type->world_pos)) color = 'D';
            doc_printf(doc, " <color:%c>%c</color>) ", color, I2A(i));

            color = 'w';
            if (type->plr_flags & DFP_COMPLETED) color = 'G';
            else if (type->plr_flags & DFP_FAILED) color = 'r';
            else if (type->plr_flags & DFP_SECRET) color = 'D';
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
                if (type->plr_flags & DFP_FAILED) continue;
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
    if (is_daytime() && (race->flags3 & RF3_HURT_LITE))
        return 0;
    return prob;
}
static dun_type_ptr _surface(void)
{
    dun_type_ptr type = dun_type_alloc(D_SURFACE, "The Surface");
    _set_feat(type->floor_type, feat_dirt, 0, 100);
    type->mon_alloc_f = _surface_mon_alloc;
    type->mon_alloc_tbl = vec_filter(mon_alloc_tbl, (vec_item_p)mon_alloc_surface);
    type->flags = DF_NO_DESTRUCT | DF_NO_QUAKE;
    return type;
}

/************************************************************************
 * Quest
 ************************************************************************/
static dun_type_ptr _quest(void)
{
    dun_type_ptr type = dun_type_alloc(D_QUEST, "A Quest");
    type->flags = DF_NO_DESTRUCT | DF_NO_QUAKE | DF_NO_GENOCIDE;
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
        if (mon->r_idx == r_idx)
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

    /* already allocated ? */
    if (_restore_guardian(dun, race->id)) return TRUE;

    /* already slain? this is a bug! */
    if ((race->flags1 & RF1_UNIQUE) && !race->max_num)
    {
        msg_print("It seems this level was guarded by someone before.");
        me->plr_flags |= DFP_COMPLETED;
        return FALSE;
    }
    for (j = 1000; j > 0; j--)
    {
        point_t pos = dun_random_mon_pos(cave, race);
        mon_ptr mon;

        if (!dun_pos_interior(cave, pos)) continue;

        /* allocated on a different level? move the guardian here! */
        if ((race->flags1 & RF1_UNIQUE) && race->cur_num)
        {
            mon_ptr mon = dun_mgr_relocate_unique(race->id, dun, pos);
            if (!mon) /* bug! cur_num implies mon is somewhere in dun_mgr()->dungeons */
                return FALSE;
            mon->mflag2 |= MFLAG2_QUESTOR;
            break;
        }

        /* Handle normal allocation */
        mon = place_monster_aux(0, pos, race->id, PM_QUESTOR | PM_ALLOW_GROUP);
        if (mon)
        {
            assert(mon->r_idx == race->id);
            mon->mflag2 |= MFLAG2_QUESTOR;
            break;
        }
    }
    if (!j) return FALSE; /* Failed to place?! */

    msg_format("<color:v>Beware!</color> This level is guarded by <color:R>%s</color>.",
        r_name + race->name);
    return TRUE;
}
static void _conquer(dun_type_ptr me)
{
    msg_format("<color:G>You have conquered <color:U>%s</color>!</color>", me->name);
    virtue_add(VIRTUE_VALOUR, 5);
    gain_chosen_stat();
    p_ptr->fame += randint1(3);
    msg_add_tiny_screenshot(50, 24);
    me->plr_flags |= DFP_COMPLETED;
}
/************************************************************************
 * Amber
 ************************************************************************/
static void _amber_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _amber_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_OBERON)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 5);
            p_ptr->fame += 5 + randint1(5);

            me->max_dun_lvl++;
            me->final_guardian = MON_SERPENT;
        }
        else
        {
            _conquer(me);
        }
    }
}
static dun_type_ptr _amber(void)
{
    dun_type_ptr type = dun_type_alloc(D_AMBER, "The World of Amber");
    type->desc = "the fabulous world of Amber";
    type->min_dun_lvl = 1;
    type->max_dun_lvl = 99;
    type->final_guardian = MON_OBERON;
    type->change_dun_f = _amber_change_dun;
    type->kill_mon_f = _amber_kill_mon;
    return type;
}
/************************************************************************
 * Stronghold
 ************************************************************************/
static void _stronghold_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _stronghold_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_WUTUGU)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            p_ptr->fame++;

            me->max_dun_lvl++;
            me->final_guardian = MON_MENG_HUO;
        }
        else
        {
            int k_idx = lookup_kind(TV_AMULET, SV_ANY);

            if (p_ptr->prace == RACE_MON_BEHOLDER)
                k_idx = lookup_kind(TV_RING, SV_ANY);

            object_prep(&forge, k_idx);
            apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_QUEST);
            dun_drop_near(dun, &forge, mon->pos);
            _conquer(me);
        }
    }
}
static int _stronghold_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->dun_type_id == D_STRONGHOLD) return prob * 50;
    switch (race->d_char)
    {
    case 'p': return prob * 10;
    case 'H': return prob * 3;
    case 'g': return prob * 2;
    }
    return prob;
}
rect_t _stronghold_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _stronghold(void)
{
    dun_type_ptr type = dun_type_alloc(D_STRONGHOLD, "The Stronghold");
    type->desc = "the gateway to the Stronghold of the Southerings";
    type->min_dun_lvl = 1;
    type->max_dun_lvl = 14;
    type->final_guardian = MON_WUTUGU;
    type->size_f = _stronghold_size;
    type->change_dun_f = _stronghold_change_dun;
    type->kill_mon_f = _stronghold_kill_mon;
    type->mon_alloc_f = _stronghold_mon_alloc;
    type->flags = DF_NO_CAVE | DF_CURTAIN;
    return type;
}

/************************************************************************
 * Orc Cave
 ************************************************************************/
static void _orc_cave_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _orc_cave_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_BOLG)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            p_ptr->fame++;

            me->max_dun_lvl++;
            me->final_guardian = MON_AZOG;
        }
        else
        {
            int k_idx = lookup_kind(TV_RING, SV_ANY);

            object_prep(&forge, k_idx);
            apply_magic_ego = EGO_RING_COMBAT;
            apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_FORCE_EGO);
            dun_drop_near(dun, &forge, mon->pos);
            _conquer(me);
        }
    }
}
static int _orc_cave_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->flags3 & RF3_ORC) return prob * 50;
    if (race->flags3 & RF3_ANIMAL) return prob * 10;
    switch (race->d_char)
    {
    case 'k': return prob * 3;
    case 'O': return prob * 3;
    }
    return prob;
}
rect_t _orc_cave_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static dun_type_ptr _orc_cave(void)
{
    dun_type_ptr type = dun_type_alloc(D_ORC_CAVE, "The Orc Cave");
    type->desc = "a dark tunnel leading to the Orc Cave";
    _set_feat(type->floor_type, feat_dirt, 0, 70);
    _set_feat(type->floor_type, feat_grass, 70, 30);
    _set_feat(type->fill_type, feat_mountain_wall, 70, 30);
    type->min_dun_lvl = 15;
    type->max_dun_lvl = 22;
    type->final_guardian = MON_BOLG;
    type->size_f = _orc_cave_size;
    type->change_dun_f = _orc_cave_change_dun;
    type->kill_mon_f = _orc_cave_kill_mon;
    type->mon_alloc_f = _orc_cave_mon_alloc;
    type->flags = DF_CAVE | DF_RIVER_WATER | DF_CAVERN | DF_LAKE_TREE | DF_DESTROY;
    return type;
}
/************************************************************************
 * Lonely Mountain
 ************************************************************************/
static void _lonely_mountain_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _lonely_mountain_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
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
    if (race->flags3 & RF3_DRAGON) return prob * 5;
    if (race->flags3 & RF3_GIANT) return prob * 2;
    return prob;
}
rect_t _lonely_mountain_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static dun_type_ptr _lonely_mountain(void)
{
    dun_type_ptr type = dun_type_alloc(D_LONELY_MOUNTAIN, "The Lonely Mountain");
    type->desc = "the entrance to the Lonely Mountain";
    _set_feat(type->floor_type, feat_dirt, 0, 100);
    _set_feat(type->fill_type, feat_dark_pit, 90, 10);
    type->min_dun_lvl = 30;
    type->max_dun_lvl = 40;
    type->final_guardian = MON_SMAUG;
    type->size_f = _lonely_mountain_size;
    type->change_dun_f = _lonely_mountain_change_dun;
    type->kill_mon_f = _lonely_mountain_kill_mon;
    type->mon_alloc_f = _lonely_mountain_mon_alloc;
    type->flags = DF_CAVE | DF_RIVER_LAVA | DF_CAVERN | DF_DESTROY
                | DF_LAKE_LAVA | DF_LAKE_TREE | DF_LAKE_RUBBLE;
    return type;
}
/************************************************************************
 * The Forest
 ************************************************************************/
static void _forest_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _forest_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};
        int     k_idx = lookup_kind(TV_SWORD, SV_POISON_NEEDLE);

        object_prep(&forge, k_idx);
        dun_drop_near(dun, &forge, mon->pos);
        _conquer(me);
    }
}
static int _forest_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->flags3 & RF3_ANIMAL) return prob * 20;
    if (race->flags8 & RF8_WILD_WOOD) return prob * 10;
    if (race->id == MON_MIRKWOOD_SPIDER) return prob * 50;
    switch (race->d_char)
    {
    case 'S':
    case 'J':
    case 'Z':
    case '#': return prob * 3;
    }
    return prob;
}
static dun_type_ptr _forest(void)
{
    dun_type_ptr type = dun_type_alloc(D_FOREST, "The Forest");
    type->desc = "a path leading to a Forest";
    _set_feat(type->floor_type, feat_grass, 0, 85);
    _set_feat(type->floor_type, feat_flower, 85, 5);
    _set_feat(type->floor_type, feat_shallow_water, 90, 10);
    _set_feat(type->fill_type, feat_tree, 0, 90);
    type->feat_wall_outer = feat_brake;
    type->feat_wall_inner = feat_tree;
    type->feat_wall_solid = feat_brake;
    type->stream1 = feat_granite;
    type->stream2 = feat_brake;
    type->min_dun_lvl = 20;
    type->max_dun_lvl = 32;
    type->final_guardian = MON_SHELOB;
    type->change_dun_f = _forest_change_dun;
    type->kill_mon_f = _forest_kill_mon;
    type->mon_alloc_f = _forest_mon_alloc;
    type->flags = DF_NO_DOORS | DF_RIVER_WATER;
    return type;
}
/************************************************************************
 * Camelot
 ************************************************************************/
static void _camelot_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _camelot_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
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
    if (race->dun_type_id == D_CAMELOT) return prob * 50;
    if (race->flags2 & RF2_KNIGHT) return prob * 30;
    switch (race->d_char)
    {
    case 'p': return prob * 10;
    case 'H': return prob * 3;
    case 'g': return prob * 2;
    case 'd': return prob * 7;
    }
    return prob;
}
rect_t _camelot_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _camelot(void)
{
    dun_type_ptr type = dun_type_alloc(D_CAMELOT, "The Land of Camelot");
    type->desc = "the entrance to the fabled land of Camelot";
    type->min_dun_lvl = 25;
    type->max_dun_lvl = 35;
    type->stream1 = 0;
    type->stream2 = 0;
    type->final_guardian = MON_ARTHUR;
    type->size_f = _camelot_size;
    type->change_dun_f = _camelot_change_dun;
    type->kill_mon_f = _camelot_kill_mon;
    type->mon_alloc_f = _camelot_mon_alloc;
    type->flags = DF_NO_CAVE | DF_CURTAIN;
    return type;
}

/************************************************************************
 * Icky Cave
 ************************************************************************/
static void _icky_cave_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _icky_cave_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_ICKY_QUEEN)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            p_ptr->fame++;

            me->max_dun_lvl = 35;
            me->final_guardian = MON_UBBO_SATHLA;
        }
        else
        {
            int k_idx = lookup_kind(TV_RING, SV_ANY);

            object_prep(&forge, k_idx);
            apply_magic(&forge, me->max_dun_lvl, AM_NO_FIXED_ART | AM_GOOD | AM_GREAT | AM_QUEST);
            dun_drop_near(dun, &forge, mon->pos);
            _conquer(me);
        }
    }
}
static int _icky_cave_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    switch (race->d_char)
    {
    case 'i': return prob * 50;
    case 'j': return prob * 30;
    case 'M': return prob * 10;
    }
    return prob;
}
rect_t _icky_cave_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static dun_type_ptr _icky_cave(void)
{
    dun_type_ptr type = dun_type_alloc(D_ICKY_CAVE, "The Icky Cave");
    type->desc = "a slimy tunnel leading to the Icky Cave";
    _set_feat(type->floor_type, feat_swamp, 0, 20);
    _set_feat(type->floor_type, feat_grass, 20, 60);
    _set_feat(type->floor_type, feat_shallow_water, 80, 20);
    type->min_dun_lvl = 20;
    type->max_dun_lvl = 20;
    type->final_guardian = MON_ICKY_QUEEN;
    type->size_f = _icky_cave_size;
    type->change_dun_f = _icky_cave_change_dun;
    type->kill_mon_f = _icky_cave_kill_mon;
    type->mon_alloc_f = _icky_cave_mon_alloc;
    type->flags = DF_CAVE | DF_RIVER_WATER | DF_CAVERN | DF_LAKE_TREE | DF_DESTROY;
    return type;
}
/************************************************************************
 * Moria
 ************************************************************************/
void _moria_pre_gen(dun_type_ptr me, dun_gen_ptr gen)
{
    if (gen->dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        dun_gen_lava_vault(gen);
}
static void _moria_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _moria_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        if (!a_info[ART_KHAZAD_DUM].generated)
            create_named_art(ART_KHAZAD_DUM, mon->pos);
        p_ptr->fame += 10;
        _conquer(me);
    }
}
rect_t _moria_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static int _moria_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->dun_type_id == D_MORIA) return prob * 50;
    if (race->flags3 & (RF3_ORC | RF3_TROLL)) return prob * 5;
    if (race->d_char == 'P' && (race->flags3 & RF3_GIANT)) return prob * 5;
    if (race->flags3 & RF3_DEMON) return prob * 5;
    if (race->d_char == 'h') return prob * 5;
    return prob;
}
static dun_type_ptr _moria(void)
{
    dun_type_ptr type = dun_type_alloc(D_MORIA, "The Mines of Moria");
    type->desc = "the gates of the Mines of Moria";
    _set_feat(type->fill_type, feat_magma_vein, 60, 20);  /* XXX MAGMA_TREASURE */
    _set_feat(type->fill_type, feat_quartz_vein, 80, 20); /* XXX QUARTZ_TREASURE */
    type->min_dun_lvl = 50;
    type->max_dun_lvl = 65;
    type->final_guardian = MON_LUNGORTHIN;
    type->change_dun_f = _moria_change_dun;
    type->kill_mon_f = _moria_kill_mon;
    type->pre_gen_f = _moria_pre_gen;
    type->size_f = _moria_size;
    type->mon_alloc_f = _moria_mon_alloc;
    type->flags = DF_CAVE | DF_RIVER_WATER | DF_CAVERN | DF_LAKE_LAVA | DF_RIVER_LAVA | DF_DESTROY;
    return type;
}
/************************************************************************
 * Mountain
 ************************************************************************/
static void _mountain_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _mountain_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_UTGARD_LOKE)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            p_ptr->fame++;

            me->max_dun_lvl++;
            me->final_guardian = MON_SHUTEN;
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
    if (race->d_char == 'B' && (race->flags8 & RF8_WILD_MOUNTAIN)) return prob * 50;
    if (race->flags3 & RF3_TROLL) return prob * 30;
    if (race->d_char == 'P' && (race->flags3 & RF3_GIANT)) return prob * 50;
    if (race->flags3 & RF3_ORC) return prob * 10;
    if (race->flags7 & RF7_CAN_FLY) return prob * 10;
    switch (race->d_char)
    {
    case 'Y': return prob * 3;
    case 'H': return prob * 3;
    case 'O': return prob * 5;
    }
    return prob;
}
rect_t _mountain_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 70);
}
static dun_type_ptr _mountain(void)
{
    dun_type_ptr type = dun_type_alloc(D_MOUNTAIN, "The Mountain");
    type->desc = "a dark tunnel leading to the Mountain";
    _set_feat(type->floor_type, feat_grass, 0, 100);
    _set_feat(type->fill_type, feat_mountain_wall, 0, 100);
    type->feat_wall_outer = feat_mountain_wall;
    type->feat_wall_solid = feat_mountain_wall;
    type->feat_wall_inner = feat_granite;
    type->stream1 = 0;
    type->stream2 = 0;
    type->min_dun_lvl = 35;
    type->max_dun_lvl = 44;
    type->final_guardian = MON_UTGARD_LOKE;
    type->size_f = _mountain_size;
    type->change_dun_f = _mountain_change_dun;
    type->kill_mon_f = _mountain_kill_mon;
    type->mon_alloc_f = _mountain_mon_alloc;
    type->flags = DF_CAVE | DF_RIVER_WATER | DF_CAVERN | DF_NO_DOORS;
    return type;
}
/************************************************************************
 * Castle
 ************************************************************************/
static void _castle_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _castle_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static int _castle_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->flags2 & RF2_HUMAN) return prob * 50;
    if (race->flags2 & RF2_KNIGHT) return prob * 10;
    if (race->flags3 & RF3_DEMON) return prob * 6;
    switch (race->d_char)
    {
    case 'H': return prob * 3;
    case 'g': return prob * 5;
    case 'h': return prob * 10;
    }
    return prob;
}
rect_t _castle_size(dun_type_ptr me)
{
    return rect_create(0, 0, 200, 45);
}
static dun_type_ptr _castle(void)
{
    dun_type_ptr type = dun_type_alloc(D_CASTLE, "The Old Castle");
    type->desc = "the gates of the Old Castle";
    type->stream1 = 0;
    type->stream2 = 0;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 65;
    type->final_guardian = MON_LAYZARK;
    type->size_f = _castle_size;
    type->change_dun_f = _castle_change_dun;
    type->kill_mon_f = _castle_kill_mon;
    type->mon_alloc_f = _castle_mon_alloc;
    type->flags = DF_NO_CAVE | DF_CURTAIN | DF_ARENA;
    return type;
}
/************************************************************************
 * Isengard
 ************************************************************************/
static void _isengard_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _isengard_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static int _isengard_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->flags3 & RF3_ORC) return prob * 10;
    if (race->flags3 & RF3_TROLL) return prob * 5;
    switch (race->d_char)
    {
    case 'p':
    case 'h':
        if (summon_specific_who != SUMMON_WHO_PLAYER && (race->flags3 & RF3_GOOD)) return 0;
        if (race->body.class_idx == CLASS_MAGE) return prob * 25;
        if (race->flags2 & RF3_EVIL) return prob * 10;
        return prob * 5;
    }
    return prob;
}
rect_t _isengard_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _isengard(void)
{
    dun_type_ptr type = dun_type_alloc(D_ISENGARD, "The Tower of Isengard");
    type->desc = "the Tower of Isengard";
    type->stream1 = 0;
    type->stream2 = 0;
    type->min_dun_lvl = 40;
    type->max_dun_lvl = 60;
    type->final_guardian = MON_SARUMAN;
    type->size_f = _isengard_size;
    type->change_dun_f = _isengard_change_dun;
    type->kill_mon_f = _isengard_kill_mon;
    type->mon_alloc_f = _isengard_mon_alloc;
    type->flags = DF_NO_CAVE | DF_CURTAIN | DF_ARENA | DF_TOWER;
    return type;
}
/************************************************************************
 * Minas Morgul
 ************************************************************************/
static void _minas_morgul_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _minas_morgul_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static int _minas_morgul_mon_alloc(dun_type_ptr me, mon_race_ptr race, int prob)
{
    if (race->dun_type_id == D_MINAS_MORGUL) return prob * 50;
    if (race->flags3 & RF3_UNDEAD) return prob * 20;
    if (summon_specific_who != SUMMON_WHO_PLAYER && (race->flags3 & RF3_GOOD)) return 0;
    switch (race->d_char) /* Minas Morgul is the Tower of Sorcery */
    {
    case 'p':
    case 'h':
        if (race->body.class_idx == CLASS_MAGE) return prob * 20;
        if (race->flags2 & RF3_EVIL) return prob * 5;
        break;
    }
    return prob;
}
rect_t _minas_morgul_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _minas_morgul(void)
{
    dun_type_ptr type = dun_type_alloc(D_MINAS_MORGUL, "Minas Morgul");
    type->desc = "the gates of Minas Morgul";
    type->stream1 = 0;
    type->stream2 = 0;
    type->min_dun_lvl = 70;
    type->max_dun_lvl = 80;
    type->final_guardian = MON_ANGMAR;
    type->size_f = _minas_morgul_size;
    type->change_dun_f = _minas_morgul_change_dun;
    type->kill_mon_f = _minas_morgul_kill_mon;
    type->mon_alloc_f = _minas_morgul_mon_alloc;
    type->flags = DF_NO_CAVE | DF_CURTAIN | DF_ARENA | DF_TOWER;
    return type;
}
/************************************************************************
 * Dark Tower
 ************************************************************************/
static void _dark_tower_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _dark_tower_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_MOUTH_OF_SAURON)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            p_ptr->fame++;

            me->max_dun_lvl = 90;
            me->final_guardian = MON_SAURON;
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
    if (race->flags3 & RF3_UNDEAD) return prob * 5;
    if (race->flags3 & RF3_DEMON) return prob * 5;
    if (summon_specific_who != SUMMON_WHO_PLAYER && (race->flags3 & RF3_GOOD)) return 0;
    return prob;
}
rect_t _dark_tower_size(dun_type_ptr me)
{
    return rect_create(0, 0, 151, 75);
}
static dun_type_ptr _dark_tower(void)
{
    dun_type_ptr type = dun_type_alloc(D_DARK_TOWER, "The Dark Tower");
    type->desc = "the Dark Tower";
    type->stream1 = 0;
    type->stream2 = 0;
    type->min_dun_lvl = 80;
    type->max_dun_lvl = 80;
    type->final_guardian = MON_MOUTH_OF_SAURON;
    type->size_f = _dark_tower_size;
    type->change_dun_f = _dark_tower_change_dun;
    type->kill_mon_f = _dark_tower_kill_mon;
    type->mon_alloc_f = _dark_tower_mon_alloc;
    type->flags = DF_NO_CAVE | DF_CURTAIN | DF_ARENA | DF_TOWER;
    return type;
}
/************************************************************************
 * Mount Doom
 ************************************************************************/
void _mount_doom_pre_gen(dun_type_ptr me, dun_gen_ptr gen)
{
    dun_gen_lava_vault(gen);
}
rect_t _mount_doom_size(dun_type_ptr me)
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
    type->flags = DF_CAVE | DF_CAVERN | DF_LAKE_LAVA | DF_RIVER_LAVA | DF_DESTROY;
    return type;
}
/************************************************************************
 * Angband
 ************************************************************************/
static void _angband_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _angband_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (mon->r_idx == MON_CARCHAROTH)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 2);
            p_ptr->fame++;
            me->max_dun_lvl = 99;
            me->final_guardian = MON_GOTHMOG;
        }
        else if (mon->r_idx == MON_GOTHMOG)
        {
            dun_quest_stairs(dun, mon->pos, me->max_dun_lvl + 1);
            if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
                dun_drop_near(dun, &forge, mon->pos);
            virtue_add(VIRTUE_VALOUR, 5);
            p_ptr->fame += 5;
            me->max_dun_lvl = 100;
            me->final_guardian = MON_MORGOTH;
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
    type->final_guardian = MON_CARCHAROTH;
    type->change_dun_f = _angband_change_dun;
    type->kill_mon_f = _angband_kill_mon;
    type->flags = DF_CAVE | DF_RIVER_WATER | DF_CAVERN | DF_LAKE_LAVA | DF_RIVER_LAVA | DF_DESTROY;
    type->plr_flags = DFP_SECRET;
    return type;
}
/************************************************************************
 * Mount Olympus XXX Olympians need playtesting and re-design XXX
 ************************************************************************/
static void _olympus_change_dun(dun_type_ptr me, dun_ptr dun)
{
    if (dun->dun_lvl == me->max_dun_lvl && !(me->plr_flags & DFP_COMPLETED))
        _alloc_guardian(me, dun);
}
static void _olympus_kill_mon(dun_type_ptr me, mon_ptr mon)
{
    if (mon->r_idx == me->final_guardian && (mon->mflag2 & MFLAG2_QUESTOR))
    {
        dun_ptr dun = mon_dun(mon);
        obj_t   forge = {0};

        if (make_object(&forge, me->max_dun_lvl, AM_GOOD | AM_GREAT | AM_TAILORED | AM_QUEST))
            dun_drop_near(dun, &forge, mon->pos);

        _conquer(me);
    }
}
static dun_type_ptr _olympus(void)
{
    dun_type_ptr type = dun_type_alloc(D_OLYMPUS, "Mount Olympus");
    type->desc = "a steep path leading to Mount Olympus";
    _set_feat(type->fill_type, feat_permanent, 60, 40);
    type->stream1 = 0;
    type->stream2 = 0;
    type->min_dun_lvl = 80;
    type->max_dun_lvl = 90;
    type->final_guardian = MON_ZEUS;
    type->change_dun_f = _olympus_change_dun;
    type->kill_mon_f = _olympus_kill_mon;
    type->flags = DF_CAVE | DF_RIVER_WATER | DF_CAVERN | DF_NO_DOORS;
    return type;
}
