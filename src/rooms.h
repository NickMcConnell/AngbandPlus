#ifndef INCLUDED_ROOMS_H
#define INCLUDED_ROOMS_H
/*
 * File: rooms.h
 * Purpose: Header file for rooms.c, used only in generate.c
 */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */


/* Constants for room_template_t types and subtypes. I would use
   ROOM_T_* above except the code is unable to handle addition of subtypes.
   Currently, choose_room_template uses these to pick an appropriate
   template as specified in ../lib/edit/v_info.txt. */
enum room_type_e {
    ROOM_UNKNOWN,
    ROOM_VAULT,      /* T:VAULT:... */
    ROOM_ROOM,       /* T:ROOM:... */
    ROOM_WILDERNESS, /* T:WILD:<terrain>...  Subtype is the terrain type */
    ROOM_AMBUSH,     /* T:AMBUSH:<terrain>... Subtype is the terrain type  */
    ROOM_QUEST,      /* T:QUEST:... (cf ../lib/edit/q_old_castle.txt) */
    ROOM_TOWN,       /* T:TOWN:... (cf ../lib/edit/t_telmora.txt) */
    ROOM_WORLD,
};

enum room_subtype_e {
    ROOM_NORMAL = 1, /* T:ROOM:NORMAL... */
    ROOM_SHOP,       /* T:ROOM:SHOP... */
    ROOM_RECALL,     /* T:ROOM:RECALL... */
    ROOM_TRAVEL,
};

enum vault_type_e {
    VAULT_UNKNOWN,
    VAULT_LESSER,    /* T:VAULT:LESSER... */
    VAULT_GREATER,   /* T:VAULT:GREATER... */
};

/* ROOM_WILDERNESS uses TERRAIN_* values as the subtype */

/*
 * Generating rooms from templates
 * This includes support for user defined "letters" in the template file
 * as well as built in predefined "letters" (for historical reasons).
 *
 * See lib/edit/readme.txt for parser syntax.
 * See lib/edit/v_info.txt for sample rooms and vaults.
 * See lib/edit/q_*.txt for sample quests.
 * See lib/edit/t_*.txt for sample towns.
 */

#define ROOM_GRID_MON_TYPE      0x00000001  /* monster is SUMMON_* rather than a specific r_idx */
#define ROOM_GRID_MON_CHAR      0x00000002  /* monster is a "d_char" rather than a specific r_idx */
#define ROOM_GRID_MON_RANDOM    0x00000004
#define ROOM_GRID_MON_NO_GROUP  0x00000008
#define ROOM_GRID_MON_NO_SLEEP  0x00000010
#define ROOM_GRID_MON_NO_UNIQUE 0x00000020
#define ROOM_GRID_MON_FRIENDLY  0x00000040
#define ROOM_GRID_MON_HASTE     0x00000080
#define ROOM_GRID_MON_CLONED    0x00000100  /* hack for The Cloning Pits */

#define ROOM_GRID_TRAP_RANDOM   0x10000000  /* this may override object info */
#define ROOM_GRID_SPECIAL       0x20000000  /* use extra for cave.special field */


#define ROOM_THEME_GOOD        0x00000001
#define ROOM_THEME_EVIL        0x00000002
#define ROOM_THEME_FRIENDLY    0x00000004
#define ROOM_THEME_NIGHT       0x00000008  /* Useful for wilderness graveyards where monsters only spawn at night */
#define ROOM_THEME_DAY         0x00000010
#define ROOM_THEME_FORMATION   0x00000020  /* Hack (see source for details): Allows monster formations. */
#define ROOM_THEME_OBJECT      0x00000040  /* Hack: All objects similar (e.g. Potions, Devices, ...) */
#define ROOM_DEBUG             0x00004000  /* For debugging ... force this template to always be chosen */
#define ROOM_NO_ROTATE         0x00008000

struct room_grid_s
{
    s16b cave_feat;
    s16b cave_trap; /* This could also be a secret door ... */

    u32b cave_info;
    s16b monster;

    obj_drop_t object;
    s16b extra;    /* dungeon and quest ids for special features */

    u32b flags;

    byte letter;
    byte scramble; /* useful in randomizing quests (e.g. Vault and Cloning Pits) */
    byte monster_level;

    byte trap_pct;
    byte mon_pct;
};
typedef struct room_grid_s room_grid_t, *room_grid_ptr;
extern obj_ptr room_grid_make_obj(room_grid_ptr grid, int level, int mode);

struct room_s
{
    int id;
    cptr name;

    byte level;
    byte max_level;
    byte rarity;
    byte type;

    u16b subtype;
    u16b flags;

    byte height;
    byte width;

    vec_ptr map;
    int_map_ptr letters;
};
typedef struct room_s room_t, *room_ptr;

extern room_ptr room_alloc(cptr name);
extern void     room_free(room_ptr room);
extern room_ptr choose_room_template(int type, int subtype); /* from v_info.txt */

/* Coordinate Transformations allow rooms to be rotated */
struct transform_s
{
    int     which;
    rect_t  src;
    rect_t  dest;
    point_t fudge;
};
typedef struct transform_s transform_t, *transform_ptr;

transform_ptr transform_alloc(int which, rect_t src);
transform_ptr transform_alloc_random(rect_t src, point_t max_size);
transform_ptr transform_alloc_room(room_ptr room, point_t max_size);
void          transform_free(transform_ptr x);
point_t       transform_point(transform_ptr x, point_t p);


/* Generate the room from the template, apply the indicated transformation. */
extern void build_room_template_aux(room_ptr room, transform_ptr xform);


extern bool vault_aux_chapel_g(mon_race_ptr race);
extern bool vault_aux_chapel_e(mon_race_ptr race);

#endif
