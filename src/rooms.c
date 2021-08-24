/*
 * File: rooms.c
 * Purpose: make rooms. Used by generate.c when creating dungeons.
 */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

#include "angband.h"
#include "dun.h"
#include "dun_gen.h"
#include "rooms.h"

#include <assert.h>

/************************************************************************
 * Room Templates
 ***********************************************************************/
room_ptr room_alloc(cptr name)
{
    room_ptr room = malloc(sizeof(room_t));
    memset(room, 0, sizeof(room_t));
    room->name = z_string_make(name);
    room->map = vec_alloc(free);
    room->letters = int_map_alloc(free);
    return room;
}

void room_free(room_ptr room)
{
    if (room)
    {
        z_string_free(room->name);
        vec_free(room->map);
        int_map_free(room->letters);
        free(room);
    }
}

/************************************************************************
 * Coordinate Transforms
 ***********************************************************************/
point_t _transform(point_t p, int which)
{
    int i;
    /* transform by
     * [1] rotating counter clockwise: (x,y) -> (-y,x) */
    for (i = 0; i < (which & 03); i++)
    {
        int tmp = p.x;
        p.x = -p.y;
        p.y = tmp;
    }
    /* [2] flipping: (x,y) -> (-x,y) */
    if (which & 04)
        p.x = -p.x;

    return p;
}

transform_ptr transform_alloc(int which, rect_t src)
{
    transform_ptr result = malloc(sizeof(transform_t));

    /* avoid SIGSEGV */
    if (src.cx > MAX_HGT - 2)
        which &= ~01;

    result->which = which;
    result->src = src;
    if (which & 01) /* odd number of rotations: (w,h) -> (h,w) */
        result->dest = rect_create(0, 0, src.cy, src.cx);
    else
        result->dest = rect_create(0, 0, src.cx, src.cy);

    /* rotating around the center is conceptually cleaner, but has
     * issues with rounding (ie the center might not be symmetric).
     * instead, we rotate around the top left and translate to patch
     * things up (with fudge) */
    result->fudge = _transform(point_create(src.cx - 1, src.cy - 1), which);
    if (result->fudge.x > 0) result->fudge.x = 0;
    else result->fudge.x = -result->fudge.x;
    if (result->fudge.y > 0) result->fudge.y = 0;
    else result->fudge.y = -result->fudge.y;

    return result;
}
transform_ptr transform_alloc_random(rect_t src, point_t max_size)
{
    int which = 0;
    /* how many rotations? (0 .. 3) */
    if ( src.cx <= max_size.y - 2 /* make sure odd rotations will fit */
      && src.cy <= max_size.x - 2
      && src.cy*100/src.cx > 70   /* make sure odd rotations look ok */
      && one_in_(2) )
    {
        which |= 01;
    }
    if (one_in_(2))
        which |= 02;
    /* how many flips? (0 .. 1) */
    if (one_in_(2))
        which |= 04;
    return transform_alloc(which, src);
}

transform_ptr transform_alloc_room(room_ptr room, point_t max_size)
{
    if (room->flags & ROOM_NO_ROTATE)
        return transform_alloc(0, rect_create(0, 0, room->width, room->height));

    return transform_alloc_random(rect_create(0, 0, room->width, room->height), max_size);
}

point_t transform_point(transform_ptr xform, point_t p)
{
    assert(rect_contains_point(xform->src, p));
    p = point_subtract(p, rect_top_left(xform->src));
    p = _transform(p, xform->which);
    p = point_add(p, xform->fudge);
    p = point_add(p, rect_top_left(xform->dest));
    assert(rect_contains_point(xform->dest, p));
    return p;
}

void transform_free(transform_ptr x)
{
    if (x) free(x);
}


/*
 * The following functions are used to determine if the given monster
 * is appropriate for inclusion in a monster nest or monster pit or
 * the given type.
 *
 * None of the pits/nests are allowed to include "unique" monsters.
 */


static bool vault_monster_okay(mon_race_ptr race)
{
    if (!mon_alloc_dungeon(race)) return FALSE;
    if (mon_race_is_unique(race)) return FALSE;
    if (mon_race_is_aquatic(race)) return FALSE;
    return TRUE;
}


/* Breath mask for "monster pit (dragon)"
static u32b vault_aux_dragon_mask4; XXX */

/*
 * Helper monster selection function
 */
static bool vault_aux_simple(mon_race_ptr race)
{
    return vault_monster_okay(race);
}

/*
 * Helper function for "monster nest (jelly)"
 */
static bool vault_aux_jelly(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (mon_race_is_(race, "E.caaws") || mon_race_is_(race, "j.shoggoth")) return TRUE;
    if (mon_race_can_trample_mon(race) && !mon_race_never_blow(race)) return FALSE;
    return mon_race_is_char_ex(race, "ijm");
}

/*
 * Helper function for "monster nest (animal)"
 */
static bool vault_aux_animal(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_animal(race)) return FALSE;
    if (mon_race_is_(race, "Z.death")) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster nest (undead)"
 */
static bool vault_aux_undead(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_undead(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster nest (chapel)"
 */
bool vault_aux_chapel_g(mon_race_ptr race)
{
    static cptr chapel_list[] = {
        "p.novice priest", "p.novice paladin",
        "p.priest", "p.jade monk", "p.ivory monk", "p.ultra-elite paladin",
        "p.ebony monk", "p.white knight", "p.knight templar", "p.paladin",
        "p.topaz monk", NULL };

    if (!vault_monster_okay(race)) return FALSE;
    if (race->align < 0) return FALSE;
    if (mon_race_is_(race, "A.gold") || mon_race_is_(race, "A.silver")) return FALSE;

    if (mon_race_is_char(race, 'A')) return TRUE;
    return mon_race_is_one_(race, chapel_list);
}

bool vault_aux_chapel_e(mon_race_ptr race)
{
    static cptr chapel_list[] = {
        "A.fallen", "p.high priest", "p.archpriest", "p.black knight",
        "p.death knight", "p.hell knight", "p.anti-paladin", "p.ipsissimus",
        "p.wyrd sister", NULL };

    if (!vault_monster_okay(race)) return FALSE;
    if (race->align > 0) return FALSE;

    if (mon_race_is_char(race, 'U')) return TRUE;
    return mon_race_is_one_(race, chapel_list);
}

/*
 * Helper function for "monster nest (kennel)"
 */
static bool vault_aux_kennel(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_char_ex(race, "CZ")) return FALSE;
    if (mon_race_is_(race, "Z.death")) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster nest (mimic)"
 */
static bool vault_aux_mimic(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    return mon_race_is_char_ex(race, "!$&(/=?[\\|");
}

/*
 * Helper function for "monster nest (clone)"
 */
static int vault_aux_race;
static bool vault_aux_clone(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    return race->id == vault_aux_race;
}

/*
 * Helper function for "monster nest (symbol clone)"
 */
static char vault_aux_char;
static bool vault_aux_symbol_e(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (mon_race_can_trample_mon(race) && !mon_race_never_blow(race)) return FALSE;
    if (race->align > 0) return FALSE;
    return mon_race_is_char(race, vault_aux_char);
}
static bool vault_aux_symbol_g(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (mon_race_can_trample_mon(race) && !mon_race_never_blow(race)) return FALSE;
    if (race->align < 0) return FALSE;
    return mon_race_is_char(race, vault_aux_char);
}

/*
 * Helper function for "monster pit (orc)"
 */
static bool vault_aux_orc(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_orc(race)) return FALSE;
    if (mon_race_is_undead(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster pit (troll)"
 */
static bool vault_aux_troll(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_troll(race)) return FALSE;
    if (mon_race_is_undead(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster pit (giant)"
 */
static bool vault_aux_giant(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_giant(race)) return FALSE;
    if (race->align > 0) return FALSE;
    if (mon_race_is_undead(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster pit (dragon)"
 */
static bool vault_aux_dragon(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (!mon_race_is_dragon(race)) return FALSE;
    /*if (race->flags4 != vault_aux_dragon_mask4) return FALSE;*/
    if (mon_race_is_undead(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster pit (demon)"
 */
static bool vault_aux_demon(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (mon_race_can_trample_mon(race) && !mon_race_never_blow(race)) return FALSE;
    if (!mon_race_is_demon(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster pit (lovecraftian)"
 */
static bool vault_aux_cthulhu(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (mon_race_can_trample_mon(race) && !mon_race_never_blow(race)) return FALSE;
    if (!mon_race_is_horror(race)) return FALSE;
    return TRUE;
}

/*
 * Helper function for "monster pit (clone)"
 */
static void vault_prep_clone(void)
{
    mon_alloc_push_filter(vault_aux_simple);
    vault_aux_race = mon_alloc_choose(cave->difficulty + 10)->id;
    mon_alloc_pop_filter();
}

/*
 * Helper function for "monster pit (symbol clone)"
 */
static bool _symbol_g(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (race->align < 0) return FALSE;
    return TRUE;
}
static void vault_prep_symbol_g(void)
{
    mon_race_ptr r;
    mon_alloc_push_filter(_symbol_g);
    r = mon_alloc_choose(cave->difficulty + 10);
    if (r) vault_aux_char = mon_race_char(r);
    else vault_aux_char = 'p';
    mon_alloc_pop_filter();
}
static bool _symbol_e(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    if (race->align > 0) return FALSE;
    return TRUE;
}
static void vault_prep_symbol_e(void)
{
    mon_race_ptr r;
    mon_alloc_push_filter(_symbol_e);
    r = mon_alloc_choose(cave->difficulty + 10);
    if (r) vault_aux_char = mon_race_char(r);
    else vault_aux_char = 'u';
    mon_alloc_pop_filter();
}

/*
 * Helper function for "monster pit (dragon)"
 */
static void vault_prep_dragon(void)
{
    #if 0
    /* Pick dragon type */
    switch (randint0(6))
    {
        /* Black */
        case 0:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_ACID;

            /* Done */
            break;
        }

        /* Blue */
        case 1:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_ELEC;

            /* Done */
            break;
        }

        /* Red */
        case 2:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_FIRE;

            /* Done */
            break;
        }

        /* White */
        case 3:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_COLD;

            /* Done */
            break;
        }

        /* Green */
        case 4:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = RF4_BR_POIS;

            /* Done */
            break;
        }

        /* Multi-hued */
        default:
        {
            /* Restrict dragon breath type */
            vault_aux_dragon_mask4 = (RF4_BR_ACID | RF4_BR_ELEC |
                                              RF4_BR_FIRE | RF4_BR_COLD |
                                              RF4_BR_POIS);

            /* Done */
            break;
        }
    }
    #endif
}


/*
 * Helper function for "monster pit (dark elf)"
 */
static bool vault_aux_dark_elf(mon_race_ptr race)
{
    if (!vault_monster_okay(race)) return FALSE;
    return mon_race_is_dark_elf(race);
}

typedef struct vault_aux_type vault_aux_type;
struct vault_aux_type
{
    cptr name;
    bool (*hook_func)(mon_race_ptr race);
    void (*prep_func)(void);
    int level;
    int chance;
};


static int pick_vault_type(vault_aux_type *l_ptr, int allow_flag_mask)
{
    int tmp, total, count;

    vault_aux_type *n_ptr;

    /* Calculate the total possibilities */
    for (n_ptr = l_ptr, total = 0, count = 0; TRUE; n_ptr++, count++)
    {
        /* Note end */
        if (!n_ptr->name) break;

        /* Ignore excessive depth */
        if (n_ptr->level > cave->difficulty) continue;

        /* Not matched with pit/nest flag */
        if (!(allow_flag_mask & (1L << count))) continue;

        /* Count this possibility */
        total += n_ptr->chance * MAX_DEPTH / (MIN(cave->difficulty, MAX_DEPTH - 1) - n_ptr->level + 5);
    }

    /* Pick a random type */
    tmp = randint0(total);

    /* Find this type */
    for (n_ptr = l_ptr, total = 0, count = 0; TRUE; n_ptr++, count++)
    {
        /* Note end */
        if (!n_ptr->name) break;

        /* Ignore excessive depth */
        if (n_ptr->level > cave->difficulty) continue;

        /* Not matched with pit/nest flag */
        if (!(allow_flag_mask & (1L << count))) continue;

        /* Count this possibility */
        total += n_ptr->chance * MAX_DEPTH / (MIN(cave->difficulty, MAX_DEPTH - 1) - n_ptr->level + 5);

        /* Found the type */
        if (tmp < total) break;
    }

    return n_ptr->name ? count : -1;
}

static vault_aux_type nest_types[] =
{
    {"clone",        vault_aux_clone,    vault_prep_clone,   5, 3},
    {"jelly",        vault_aux_jelly,    NULL,               5, 6},
    {"symbol good",  vault_aux_symbol_g, vault_prep_symbol_g, 25, 2},
    {"symbol evil",  vault_aux_symbol_e, vault_prep_symbol_e, 25, 2},
    {"mimic",        vault_aux_mimic,    NULL,              30, 4},
    {"lovecraftian", vault_aux_cthulhu,  NULL,              70, 2},
    {"kennel",       vault_aux_kennel,   NULL,              45, 4},
    {"animal",       vault_aux_animal,   NULL,              35, 5},
    {"chapel",       vault_aux_chapel_g, NULL,              75, 4},
    {"undead",       vault_aux_undead,   NULL,              75, 5},
    {NULL,           NULL,               NULL,               0, 0},
};

static vault_aux_type pit_types[] =
{
    {"orc",          vault_aux_orc,      NULL,               5, 6},
    {"troll",        vault_aux_troll,    NULL,              20, 6},
    {"giant",        vault_aux_giant,    NULL,              20, 6},
    {"lovecraftian", vault_aux_cthulhu,  NULL,              80, 2},
    {"symbol good",  vault_aux_symbol_g, vault_prep_symbol_g, 70, 1},
    {"symbol evil",  vault_aux_symbol_e, vault_prep_symbol_e, 70, 1},
    {"chapel",       vault_aux_chapel_g, NULL,              65, 2},
    {"dragon",       vault_aux_dragon,   vault_prep_dragon, 60, 9},
    {"demon",        vault_aux_demon,    NULL,              80, 6},
    {"dark elf",     vault_aux_dark_elf, NULL,              45, 4},
    {NULL,           NULL,               NULL,               0, 0},
};


/* Nest types code */
#define NEST_TYPE_CLONE        0
#define NEST_TYPE_JELLY        1
#define NEST_TYPE_SYMBOL_GOOD  2
#define NEST_TYPE_SYMBOL_EVIL  3
#define NEST_TYPE_MIMIC        4
#define NEST_TYPE_LOVECRAFTIAN 5
#define NEST_TYPE_KENNEL       6
#define NEST_TYPE_ANIMAL       7
#define NEST_TYPE_CHAPEL       8
#define NEST_TYPE_UNDEAD       9

/* Pit types code */
#define PIT_TYPE_ORC           0
#define PIT_TYPE_TROLL         1
#define PIT_TYPE_GIANT         2
#define PIT_TYPE_LOVECRAFTIAN  3
#define PIT_TYPE_SYMBOL_GOOD   4
#define PIT_TYPE_SYMBOL_EVIL   5
#define PIT_TYPE_CHAPEL        6
#define PIT_TYPE_DRAGON        7
#define PIT_TYPE_DEMON         8
#define PIT_TYPE_DARK_ELF      9


#define NUM_NEST_MON_TYPE 64

static room_grid_ptr _find_room_grid(room_ptr room, char letter)
{
    room_grid_ptr grid1 = int_map_find(room->letters, letter);
    if (grid1 && grid1->scramble)
    {
        room_grid_ptr grid2 = int_map_find(room->letters, grid1->scramble);
        if (grid2) return grid2;
    }
    if ( (room->flags & ROOM_THEME_FORMATION)
      && '0' <= letter && letter <= '9' )
    {
        /* XXX It is very important that FORMATION rooms *not* use
         * global letters. 8 and 9 mean something very different wrt
         * vaults! See 'Spiral Formation' for an example. */
    }
    else if (room->type != ROOM_VAULT && letter == '@')
    {
        /* Ditto with @ ... This is a 'Meaner Monster' letter for
         * vaults, but is player placement for ambushes and quests.
         * Currently, @ is not defined in room_letters, so this check
         * is simply paranoia. */
    }
    else if (!grid1)
        grid1 = int_map_find(room_letters, letter);
    return grid1;
}

static bool _obj_kind_is_good = FALSE;
static int _obj_kind_hack = 0;
static bool _kind_is_hi_book(int k_idx)
{
    obj_kind_ptr kind;
    if (!obj_kind_is_spellbook(k_idx)) return FALSE;
    kind = &k_info[k_idx];
    if (kind->sval < SV_BOOK_MIN_GOOD) return FALSE;
    if (kind->tval == TV_ARCANE_BOOK) return FALSE;
    return TRUE;
}
static bool _obj_kind_hook(int k_idx)
{
    /* Aside: kind_is_good() will reject high level books once a certain number have been
     * found. For monsters with DROP_GOOD, this means they will roll a new object until
     * they get a non-book class of objects. For Quests and Room templates, OBJ(BOOK, DEPTH+5),
     * for example, will yield no object at all which is probably a bad thing. */
    if (_obj_kind_is_good && !kind_is_good(k_idx) && _obj_kind_hack != OBJ_TYPE_HI_BOOK)
        return FALSE;

    switch (_obj_kind_hack)
    {
    case OBJ_TYPE_DEVICE:       return obj_kind_is_device(k_idx);
    case OBJ_TYPE_JEWELRY:      return obj_kind_is_jewelry(k_idx);
    case OBJ_TYPE_BOOK:         return obj_kind_is_spellbook(k_idx);
    case OBJ_TYPE_HI_BOOK:      return _kind_is_hi_book(k_idx);
    case OBJ_TYPE_BODY_ARMOR:   return obj_kind_is_body_armor(k_idx);
    case OBJ_TYPE_OTHER_ARMOR:  return kind_is_other_armor(k_idx);
    case OBJ_TYPE_WEAPON:       return obj_kind_is_weapon(k_idx);
    case OBJ_TYPE_BOW_AMMO:     return kind_is_bow_ammo(k_idx);
    case OBJ_TYPE_MISC:         return obj_kind_is_misc(k_idx);
    default:                    return k_info[k_idx].tval == _obj_kind_hack;
    }
}


static room_grid_ptr _room_grid_hack = 0;
static bool _room_grid_mon_hook(mon_race_ptr race)
{
    return mon_rule_filter(&_room_grid_hack->monster, race);
}

static bool _is_deep_night(void)
{
    int day, hour, min;
    extract_day_hour_min(&day, &hour, &min);
    if (hour > 3 && hour < 22)
        return FALSE;
    return TRUE;
}
static bool _is_broad_daylight(void)
{
    int day, hour, min;
    extract_day_hour_min(&day, &hour, &min);
    if (hour < 8 || hour > 18)
        return FALSE;
    return TRUE;
}
static u32b _room_grid_mode(room_grid_ptr grid, room_ptr room)
{
    u32b mode = mon_rule_mode(&grid->monster);
    if (room->flags & ROOM_THEME_FRIENDLY)
        mode |= PM_FORCE_FRIENDLY;
    return mode;
}
static mon_race_ptr _room_grid_race(point_t pos, room_grid_ptr grid, room_ptr room)
{
    mon_rule_t rule = grid->monster; /* copy since we modify to handle vaults */
    int lvl = cave->difficulty + rule.lvl_boost, amt;
    mon_race_ptr race = NULL;

    /* monster rules are optional */
    if (!mon_rule_is_valid(&rule))
        return NULL;

    amt = mon_rule_amt(&rule);
    if (!amt)
        return NULL;

    /* tweak the rule to match room options */
    if (room->flags & ROOM_THEME_GOOD)
        rule.flags |= MON_RULE_GOOD;
    if (room->flags & ROOM_THEME_EVIL)
        rule.flags |= MON_RULE_EVIL;

    if ((room->flags & ROOM_THEME_NIGHT) && plr_on_surface())
    {
        if (!_is_deep_night())
            return NULL;
    }
    if ((room->flags & ROOM_THEME_DAY) && plr_on_surface())
    {
        if (!_is_broad_daylight())
            return NULL;
    }

    if (room->type == ROOM_VAULT)
    {
        if (room->subtype == VAULT_GREATER)
        {
            rule.flags |= MON_RULE_VAULT;
            if (grid->object.flags & (OBJ_DROP_RAND_EGO | AM_GREAT))
                rule.min_lvl = MIN(55, lvl - 5);
            else
                rule.min_lvl = MIN(40, lvl - 10);
        }
        /* Lesser Vaults only "Power Boost" excellent tiles */
        else if (grid->object.flags & (OBJ_DROP_RAND_EGO | AM_GREAT))
        {
            rule.flags |= MON_RULE_VAULT;
            rule.min_lvl = MIN(37, lvl - 7);
        }
    }
   {dun_cell_ptr cell = dun_cell_at(cave, pos);
    mon_race_p filter = mon_alloc_cell_p(cell);
    mon_alloc_push_filter(filter);}
    race = mon_rule_race(&rule);
    mon_alloc_pop_filter();

    return race;
}
static void _apply_room_grid_mon(point_t p, room_grid_ptr grid, room_ptr room)
{
    mon_race_ptr race = _room_grid_race(p, grid, room);
    mon_ptr mon = NULL;

    if (!race) return;

    /* remove existing monster for excellent vault squares (mon_race->friends) */
    if (grid->object.flags & (OBJ_DROP_STD_EGO | OBJ_DROP_RAND_EGO | OBJ_DROP_RAND_ART | AM_GREAT))
    {
        mon_ptr mon = dun_mon_at(cave, p);
        if (mon) delete_monster(mon);
    }

    mon = place_monster_aux(who_create_null(), p, race, _room_grid_mode(grid, room));
    if (mon)
    {
        if (room->type == ROOM_VAULT && room->subtype == VAULT_GREATER && race->alloc.lvl < 90)
            mon->mflag2 |= MFLAG2_VAULT;
    }
}

obj_ptr room_grid_make_obj(room_grid_ptr grid, int level, int mode)
{
    return obj_drop_make(&grid->object, level, mode);
}

typedef struct { int id; int prob; bool good; } _obj_theme_t, *_obj_theme_ptr;
static _obj_theme_t _obj_theme_tbl[] = {
    { TV_GOLD,              5, FALSE },
    { TV_POTION,           10, FALSE },
    { TV_SCROLL,           10, FALSE },
    { TV_SHOT,              3, TRUE  },
    { TV_ARROW,             3, TRUE  },
    { TV_BOLT,              3, TRUE  },
    { TV_WAND,              3, FALSE },
    { TV_ROD,               3, FALSE },
    { TV_STAFF,             3, FALSE },
    { TV_RING,              1, TRUE  },
    { TV_AMULET,            1, TRUE  },
    { TV_FOOD,              5, FALSE },
    { OBJ_TYPE_BOOK,        5, FALSE },
    { OBJ_TYPE_BODY_ARMOR,  2, TRUE  },
    { OBJ_TYPE_WEAPON,      2, TRUE  },
    { 0 }
};
void _init_obj_theme(void)
{
    int i, total = 0, roll;
    for (i = 0;; i++)
    {
        _obj_theme_ptr p = &_obj_theme_tbl[i];
        if (!p->id) break;
        total += p->prob;
    }
    assert(total > 0);
    roll = randint1(total);
    for (i = 0;; i++)
    {
        _obj_theme_ptr p = &_obj_theme_tbl[i];
        assert(p->id);
        roll -= p->prob;
        if (roll <= 0)
        {
            _obj_kind_hack = p->id;
            _obj_kind_is_good = p->good;
            return;
        }
    }
    assert(FALSE);
}

static obj_ptr _make_obj_theme(room_grid_ptr grid, int level)
{
    obj_t forge = {0};
    int   k_idx;
    u32b  mode = 0;

    assert(_obj_kind_hack); /* call _init_obj_theme */

    if (grid->object.boost)
        level += grid->object.boost;
    if (_obj_kind_is_good)
        mode |= AM_GOOD;

    if (_obj_kind_hack == TV_GOLD)
        make_gold(&forge, 0);
    else
    {
        get_obj_num_hook = _obj_kind_hook;
        get_obj_num_prep();
        k_idx = get_obj_num(level);
        get_obj_num_hook = NULL;
        get_obj_num_prep();

        if (k_idx)
        {
            object_prep(&forge, k_idx);
            apply_magic(&forge, level, mode);
            obj_make_pile(&forge);
        }
        else if (plr->wizard)
        {
            msg_format("Unable to _make_obj_theme(%d)", _obj_kind_hack);
        }
    }
    if (forge.k_idx) return obj_copy(&forge);
    return NULL;
}

static void _apply_room_grid_obj(point_t p, room_grid_ptr grid, room_ptr room)
{
    /* see if tile was trapped in apply_room_grid_feat */
    if (!dun_allow_drop_at(cave, p)) return;

    if (room->type == ROOM_VAULT && (grid->object.flags & (OBJ_DROP_RAND_EGO | AM_GREAT)))
    {
        obj_ptr obj = room_grid_make_obj(grid, cave->difficulty, AM_VAULT);
        if (obj)
        {
            dun_place_obj(cave, obj, p);
            obj_free(obj);
        }
    }
    else
    {
        obj_ptr obj;

        /* themed objects make all OBJ(*) directives use the theme. I suppose there
         * could be other OBJ() directives and we should probably support that */
        if ((room->flags & ROOM_THEME_OBJECT) && (grid->object.flags & OBJ_DROP_RANDOM))
            obj = _make_obj_theme(grid, cave->difficulty);
        else
            obj = room_grid_make_obj(grid, cave->difficulty, 0);
        if (obj)
        {
            dun_place_obj(cave, obj, p);
            obj_free(obj);
        }
    }
}

#define _MAX_FORMATION 10
static int _formation_monsters[_MAX_FORMATION];

static bool _random_formation(point_t p, room_grid_ptr grid, room_ptr room)
{
    room_grid_t copy = *grid;
    int align = 0; /* force consistent formation alignment */
    int level = cave->difficulty + grid->monster.lvl_boost;
    bool fail = FALSE;

    int n = randint0(100), i;
    if (cave->type->id == D_SURFACE) n = 99; /* Hack: Most nests/pits won't allocate on the surface! */

    if (n < 5)
    {
        int which = pick_vault_type(nest_types, 0xffff);

        if (which < 0) 
            return FALSE;

        if (nest_types[which].prep_func)
            nest_types[which].prep_func();

        mon_alloc_push_filter(nest_types[which].hook_func);
    }
    else if (n < 30)
    {
        int which = pick_vault_type(pit_types, 0xffff);

        if (which < 0) 
            return FALSE;

        if (pit_types[which].prep_func)
            pit_types[which].prep_func();

        mon_alloc_push_filter(nest_types[which].hook_func);
    }
    else if (n < 50)
    {
        copy.monster.flags = MON_RULE_TYPE;
        switch (_1d(5))
        {
        case 1: copy.monster.which = SUMMON_KAMIKAZE; break;
        case 2: copy.monster.which = SUMMON_KNIGHT; break;
        case 3: copy.monster.which = SUMMON_HUMAN; break;
        case 4: copy.monster.which = SUMMON_DRAGON; break;
        case 5: copy.monster.which = SUMMON_THIEF; break;
        }
        _room_grid_hack = &copy;
        mon_alloc_push_filter(_room_grid_mon_hook);
    }
    else 
    {
        mon_race_ptr race = _room_grid_race(p, grid, room);

        if (!race)
            return FALSE;

        copy.monster.flags = MON_RULE_CHAR;
        copy.monster.which = mon_race_char(race);

        _room_grid_hack = &copy;
        mon_alloc_push_filter(_room_grid_mon_hook);
    }

    mon_alloc_push_filter(mon_alloc_dungeon);
    for (i = 0; i < _MAX_FORMATION; i++)
    {
        mon_race_ptr race = NULL;
        int attempts = 100;

        while (attempts--)
        {
            race = mon_alloc_choose(level);
            if (!race) break;
            if (align_hostile(align, race->align)) continue;
            if (mon_race_is_unique(race)) continue;
            if (mon_race_is_(race, "W.nazgul")) continue;
            break;
        }

        if (!race || !attempts)
        { 
            fail = TRUE;
            break;
        }

        /* force consistent formation alignment */
        if (race->align > 0) align = MAX(align, race->align); /* more and more good */
        else if (race->align < 0) align = MIN(align, race->align); /* more and more evil */

        _formation_monsters[i] = race->id;
    }
    mon_alloc_pop_filter();
    mon_alloc_pop_filter();
    return !fail;
}
static bool _choose_formation(point_t p, room_grid_ptr grid, room_ptr room)
{
    int i;
    bool fail = FALSE;
    int align = 0; /* force consistent formation alignment */

    if (grid->monster.flags & MON_RULE_RANDOM)
        return _random_formation(p, grid, room);

    for (i = 0; i < _MAX_FORMATION; i++)
    {
        mon_race_ptr race = NULL;
        int attempts = 100;

        while (attempts--)
        {
            race = _room_grid_race(p, grid, room);
            if (!race) break;
            if (align_hostile(align, race->align)) continue;
            if (mon_race_is_unique(race)) continue;
            if (mon_race_is_(race, "W.nazgul")) continue;
            break;
        }

        if (!race || !attempts)
        { 
            fail = TRUE;
            break;
        }

        /* force consistent formation alignment */
        if (race->align > 0) align = MAX(align, race->align); /* more and more good */
        else if (race->align < 0) align = MIN(align, race->align); /* more and more evil */

        _formation_monsters[i] = race->id;
    }
    return !fail;
}
static bool _init_formation(room_ptr room, point_t p)
{
    room_grid_ptr grid = _find_room_grid(room, '0');
    int i, j;

    for (i = 0; i < _MAX_FORMATION; i++)
        _formation_monsters[i] = 0;

    if (!grid || !_choose_formation(p, grid, room))
        return FALSE;

    /* bubble sort */
    for (i = 0; i < _MAX_FORMATION; i++)
    {
        for (j = _MAX_FORMATION - 1; j > i; j--)
        {
            int i1 = j;
            int i2 = j - 1;

            int p1 = mon_race_lookup(_formation_monsters[i1])->alloc.lvl;
            int p2 = mon_race_lookup(_formation_monsters[i2])->alloc.lvl;

            if (p1 > p2)
            {
                int tmp = _formation_monsters[i1];
                _formation_monsters[i1] = _formation_monsters[i2];
                _formation_monsters[i2] = tmp;
            }
        }
    }
    return TRUE;
}

/*
 * Build Rooms from Templates (e.g. Vaults, but also quest levels and towns)
 */
void build_room_template_aux(room_ptr room, transform_ptr xform)
{
    int           x, y;
    dun_cell_ptr  cell;
    room_grid_ptr grid;
    bool          initialized_formation = FALSE;

    assert(room);
    assert(xform);
    assert(xform->src.x == 0);
    assert(xform->src.y == 0);

    /* Pass 1: Place features */
    for (y = 0; y < room->height; y++)
    {
        cptr line = vec_get(room->map, y);
        for (x = 0; x < room->width; x++)
        {
            char    letter = line[x];
            point_t p = transform_point(xform, point_create(x,y));

            if (cave->type->id != D_WORLD && !dun_pos_interior(cave, p)) continue;

            /* ' ' is a transparency for wilderness encounters/ambushes */
            if (letter == ' ' && !_find_room_grid(room, letter)) continue;

            /* Access the grid */
            cell = dun_cell_at(cave, p);

            /* Lay down a floor. For D_SURFACE, we already have terrain layed down
             * and this room is a new "layer" which should not over-write the
             * previous layer (unless explicitly told to do so). */
            if (room->type != ROOM_WILDERNESS && room->type != ROOM_WORLD)
            {
                cave->type->place_floor(cave, p);
                /* default feature specification to the '.' tile so that room
                 * designers need not respecify features. For example, the following
                 * would make GRASS the default floor feature:
                 * L:.:GRASS
                 * L:P:MON(morgoth)
                 * L:$:ART(ringil) */
                grid = _find_room_grid(room, '.');
                if (grid)
                    apply_room_grid_feat(p, grid);
            }

            grid = _find_room_grid(room, letter);
            if (grid)
            {
                apply_room_grid_feat(p, grid);
                /* Force consistent town behavior ... towns are auto-mapped. Quest
                 * entrances and other permanent fixtures are mapped and glowing. */
                if (room->type == ROOM_TOWN)
                {
                    cell->flags |= CELL_AWARE;
                    if (cell->flags & (CELL_LIT | CELL_PERM))
                        cell->flags |= CELL_MAP;
                }
                /* Hack for random dungeon encounters on the surface (cf _gen_monsters in dun_world.c) */
                if (room->type == ROOM_WILDERNESS && stairs_enter_dungeon(cell))
                {
                    dun_stairs_ptr stairs = malloc(sizeof(dun_stairs_t));
                    int            id = stairs_dun_type_id(cell);
                    dun_type_ptr   dt = dun_types_lookup(id);

                    memset(stairs, 0, sizeof(dun_stairs_t));
                    stairs->pos_here = p;
                    stairs->dun_type_id = id;
                    stairs->dun_lvl = rand_range(dt->min_dun_lvl, dt->max_dun_lvl);
                    dun_add_stairs(cave, stairs);
                }
            }
            /* XXX Formations usually define '0', but '1' thru '9'
             * are implicit monster tiles. Apply default '0' or '.' feature
             * to make sure monsters get generated below (cf Ogre Cave) */
            else if ( room->type == ROOM_WILDERNESS
                   && (room->flags & ROOM_THEME_FORMATION)
                   && '1' <= letter && letter <= '9' )
            {
                grid = _find_room_grid(room, '0');
                if (!grid || !grid->cave_feat) grid = _find_room_grid(room, '.');
                if (grid) apply_room_grid_feat(p, grid);
            }

            switch (room->type)
            {
            case ROOM_WILDERNESS:
            case ROOM_TOWN:
            case ROOM_WORLD:
                break;
            case ROOM_VAULT: 
                cell->flags |= CELL_ROOM | CELL_VAULT;
                break;
            default:
                cell->flags |= CELL_ROOM;
            }
        }
    }

    /* Hack for Towns: Road generation should not clobber existing town structure. */
    if (room->type == ROOM_TOWN)
    {
        for (y = 0; y < room->height; y++)
        {
            for (x = 0; x < room->width; x++)
            {
                point_t p = transform_point(xform, point_create(x,y));
                if (!dun_pos_interior(cave, p)) continue;
                if (!(room->flags & ROOM_RUINS)) /* Osgiliath is washed out */
                    dun_cell_at(cave, p)->flags |= CELL_TOWN; /* XXX CELL_INNER, but D_SURFACE */
            }
        }
    }
    /* Skip monsters and objects on the surface. For D_WORLD, I might use monster
     * and object specifications for something else. At any rate, the player never
     * enters D_WORLD so placing monsters would waste space. For the town, note that
     * we regen terrain every time quest status changes, and this is mainly to update
     * features for quest entrances (rewards no longer require updates). */
    if (room->type == ROOM_WORLD || room->type == ROOM_TOWN) return;

    /* Pass2: Place monsters and objects */
    if (room->flags & ROOM_THEME_OBJECT)
        _init_obj_theme();

    for (y = 0; y < room->height; y++)
    {
        cptr line = vec_get(room->map, y);
        for (x = 0; x < room->width; x++)
        {
            char    letter = line[x];
            point_t p = transform_point(xform, point_create(x,y));

            if (!dun_pos_interior(cave, p)) continue;

            /* ' ' is a transparency for wilderness encounters/ambushes */
            if (letter == ' ' && !_find_room_grid(room, letter)) continue;

            /* Monster Formations are a huge, but worthwhile hack 
               '0' to '9' index into the formation array, initialized above.
               User specifies a '0' index in v_info to indicate the type of
               formation (e.g. L:0:MON(ORC, 10))
             */
            if ( (room->flags & ROOM_THEME_FORMATION)
              && '0' <= letter && letter <= '9' )
            {
                if ((room->flags & ROOM_THEME_FORMATION) && !initialized_formation)
                {
                    int k;
                    for (k = 0; k < 100; k++)
                    {
                        /* The Old Monster Pits/Nest fail fairly often. For example,
                           when trying to generate a good chapel of vampires! More 
                           commonly, Orc Pits won't work after a certain depth ... */
                        if (_init_formation(room, p)) 
                            break;
                    }
                    initialized_formation = TRUE;
                }
                {
                    int idx = letter - '0';
                    int r_idx = _formation_monsters[idx]; /* XXX */

                    if (r_idx)
                        place_monster_aux(who_create_null(), p, mon_race_lookup(r_idx), PM_NO_KAGE);
                    /* cf Oval Crypt V: The '0' letter should get a good ego item! */
                    grid = _find_room_grid(room, letter);
                    if (grid)
                        _apply_room_grid_obj(p, grid, room);
                    continue;
                }
            }

            grid = _find_room_grid(room, letter);
            if (grid)
            {
                _apply_room_grid_mon(p, grid, room);
                _apply_room_grid_obj(p, grid, room);
                /* Remove need for tedious P:PX:PY line in quest files ... normally, we
                 * can just use the '<' tile for the player's starting location. That worked
                 * fine for me until the Royal Crypt ... So a '@' will take precedence. */
                /* XXX This assumes the player is about to enter this level. Currently, this
                 * is true: See dun_take_stairs_plr and note that D_QUEST levels are generated
                 * just-in-time (cf dun_mgr_process and dun_gen_connected). */
                if (room->type == ROOM_QUEST)
                {
                    if (letter == '@')
                        plr->new_pos = p;
                    if (letter == '<' && !_find_room_grid(room, '@'))
                        plr->new_pos = p;
                }
                continue;
            }
        }
    }
}

static bool _room_is_allowed(room_ptr room, int type, int subtype)
{
    if (cave->difficulty < room->level) return FALSE;  /* Note: cave->dun_lvl is 0 for wilderness encounters! */
    if (room->max_level && room->max_level < cave->difficulty) return FALSE;
    if (room->dun_type_id && room->dun_type_id != cave->type->id) return FALSE;
    if (room->type != type) return FALSE;
    if (room->subtype != subtype) return FALSE;
    if (!room->rarity) return FALSE;

    if (cave->type->id == D_SURFACE)
    {
        if ((room->flags & ROOM_THEME_DAY) && !_is_broad_daylight()) return FALSE;
        if ((room->flags & ROOM_THEME_NIGHT) && !_is_deep_night()) return FALSE;
    }

    return TRUE;
}

room_ptr choose_room_template(int type, int subtype)
{
    int total = 0;
    int i, n;

    for (i = 0; i < vec_length(room_info); i++)
    {
        room_ptr room = vec_get(room_info, i);
        if (!_room_is_allowed(room, type, subtype)) continue;
        if (room->flags & ROOM_DEBUG) return room;
        total += 1000 / room->rarity;
    }

    if (!total)
        return NULL;

    /* XXX I need a rooms_wizard analagous to quests_wizard ...*/
    if (0 && plr->wizard)
        msg_format("<color:B>Total of (%d,%d) is <color:R>%d</color>.</color>", type, subtype, total);

    n = randint1(total);
    for (i = 0; i < vec_length(room_info); i++)
    {
        room_ptr room = vec_get(room_info, i);
        if (!_room_is_allowed(room, type, subtype)) continue;
        n -= 1000 / room->rarity;
        if (n <= 0)
            return room;
    }

    return NULL;
}

