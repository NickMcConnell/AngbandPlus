#ifndef INCLUDED_DUN_CELL_H
#define INCLUDED_DUN_CELL_H

/************************************************************************
 * A 'cell' is the basic unit of storage for dungeon terrain layout
 * (dun->page). Keep this type small since a typical dungeon level has
 * 14k cells, and a surface level has 72k cells (9 pages with 8k each).
 * dun_mgr_gc triggers at around 20 levels, so every byte in dun_cell_s
 * is about 300k of ram.
 *
 * The meaning of parm1|2 are type specific and should not be relied upon.
 * In addition, each type will use specific flags. Shared flags are listed
 * below.
 ************************************************************************/
struct dun_cell_s
{
    byte type;         /* e.g. FEAT_WALL */
    byte subtype;      /* e.g. WALL_GRANITE */
    byte parm1, parm2; /* e.g. parm1 might give a trap type or a door state (open|closed|locked...) */
    u32b flags;        /* common flags: MAP|AWARE|LOS|LIT|PROJECT...; custom flags also, but private */
}; /* 8 bytes */

enum { /* cell->type */
    FEAT_FLOOR,
    FEAT_WALL,
    FEAT_DOOR,
    FEAT_STAIRS,
    FEAT_PORTAL,
    FEAT_BLDG,
    FEAT_PATTERN,
    FEAT_TREE,
    FEAT_WATER,
    FEAT_LAVA,
    FEAT_CHASM
};

/* for each type foo, we generally have a cell_is_foo predicate for cell->type == FEAT_FOO
 * and then subtype predicates such as foo_is_bar. Calling a foo_is_bar with a cell
 * that is not a foo is legal and convenient: it simply returns false. */
typedef void (*cell_make_f)(dun_cell_ptr cell);
typedef bool (*cell_p)(dun_cell_ptr cell);

/************************************************************************
 * Floor (FEAT_FLOOR)
 *
 * Floors allow general monster and player movement. They can contain traps
 * (possibly secret), webs, and other 'glyphs' (such as mirrors, glyphs of
 * warding and explosive runes). A floor is 'clean' if it contains no such
 * additions. Floors allow objects to be dropped.
 *
 * Subtypes of floors include: dirt, grass, flower, brake and road. All
 * of these are just flavorful ... they behave just like normal floor tiles.
 * (Except roads support 'running' on the surface).
 *
 * For the Illusionist, floors support terrain illusions that fool monsters.
 ************************************************************************/
extern bool cell_is_floor(dun_cell_ptr cell);

extern bool floor_is_floor(dun_cell_ptr cell);
extern bool floor_is_dirt(dun_cell_ptr cell);
extern bool floor_is_grass(dun_cell_ptr cell);
extern bool floor_is_flower(dun_cell_ptr cell);
extern bool floor_is_brake(dun_cell_ptr cell);
extern bool floor_is_road(dun_cell_ptr cell);

extern bool floor_has_web(dun_cell_ptr cell);
extern bool floor_has_trap(dun_cell_ptr cell);
extern bool floor_has_trapdoor(dun_cell_ptr cell);
extern bool floor_has_secret_trap(dun_cell_ptr cell);
extern bool floor_has_known_trap(dun_cell_ptr cell);
extern bool floor_has_mirror(dun_cell_ptr cell);
extern bool floor_has_glyph_of_warding(dun_cell_ptr cell);
extern bool floor_has_explosive_rune(dun_cell_ptr cell);
extern bool floor_has_plr_trap(dun_cell_ptr cell);
extern bool floor_has_illusion(dun_cell_ptr cell);

extern void floor_remove_mirror(dun_cell_ptr cell);
extern void floor_remove_trap(dun_cell_ptr cell);
extern void floor_remove_glyph(dun_cell_ptr cell);
extern void floor_remove_web(dun_cell_ptr cell);
extern void floor_remove_illusion(dun_cell_ptr cell);

/* legacy */
extern bool floor_is_clean(dun_cell_ptr cell);
extern bool floor_has_object(dun_cell_ptr cell); /* old CAVE_OBJECT */
extern bool plr_can_ignore_trap(dun_cell_ptr cell);

/* private */
extern void cell_make_floor(dun_cell_ptr cell);
extern void cell_make_dirt(dun_cell_ptr cell);
extern void cell_make_grass(dun_cell_ptr cell);
extern void cell_make_flower(dun_cell_ptr cell);
extern void cell_make_brake(dun_cell_ptr cell);
extern void cell_make_road(dun_cell_ptr cell);

extern void dun_place_floor(dun_ptr dun, point_t pos);
extern void dun_place_dirt(dun_ptr dun, point_t pos);
extern void dun_place_grass(dun_ptr dun, point_t pos);
extern void dun_place_flower(dun_ptr dun, point_t pos);
extern void dun_place_brake(dun_ptr dun, point_t pos);
extern void dun_place_road(dun_ptr dun, point_t pos);
extern void dun_place_dark_floor(dun_ptr dun, point_t pos);

extern bool dun_place_trap(dun_ptr dun, point_t pos);

extern bool dun_place_glyph_of_warding(dun_ptr dun, point_t pos);
extern bool dun_place_explosive_rune(dun_ptr dun, point_t pos);
extern bool dun_place_mirror(dun_ptr dun, point_t pos);
extern bool dun_place_web(dun_ptr dun, point_t pos);
extern void dun_remove_mirror(dun_ptr dun, point_t pos);
extern void dun_remove_glyph(dun_ptr dun, point_t pos);

extern bool dun_place_plr_trap_minor(dun_ptr dun, point_t pos);
extern bool dun_place_plr_trap_major(dun_ptr dun, point_t pos);
extern bool dun_place_plr_trap_ultimate(dun_ptr dun, point_t pos);
extern void dun_remove_plr_trap(dun_ptr dun, point_t pos);
extern bool mon_disarm_plr_trap(mon_ptr mon, dun_cell_ptr cell);

/************************************************************************
 * Wall (FEAT_WALL)
 *
 * Walls block general monster and player movement, as well as line of
 * sight and spell projection. Walls may contain treasure, perhaps hidden.
 * Walls might also actually be secret doors, discovery of which will
 * turn the cell into a closed door (FEAT_DOOR).
 *
 * The 'Mountain' subtype is for the surface, and is unusual in that it
 * allows passage without requiring 'pass_wall'. Levitation suffices (or
 * mon_can_climb for monsters).
 ************************************************************************/
extern bool cell_is_wall(dun_cell_ptr cell);

extern bool wall_is_granite(dun_cell_ptr cell);
extern bool wall_is_mountain(dun_cell_ptr cell);
extern bool wall_is_mountain_wall(dun_cell_ptr cell);
extern bool wall_is_rubble(dun_cell_ptr cell);

extern bool wall_has_secret_door(dun_cell_ptr cell);
extern bool wall_has_hidden_treasure(dun_cell_ptr cell);
extern bool wall_has_treasure(dun_cell_ptr cell);

/* private */
extern void cell_make_granite(dun_cell_ptr cell);
extern void cell_make_permanent(dun_cell_ptr cell);
extern void cell_make_mountain(dun_cell_ptr cell);
extern void cell_make_mountain_wall(dun_cell_ptr cell);
extern void cell_make_rubble(dun_cell_ptr cell);
extern void cell_make_magma(dun_cell_ptr cell);
extern void cell_make_quartz(dun_cell_ptr cell);
extern void cell_make_magma_aux(dun_cell_ptr cell, int gold_pct, int secret_pct);
extern void cell_make_quartz_aux(dun_cell_ptr cell, int gold_pct, int secret_pct);

extern void dun_place_illusory_wall(dun_ptr dun, point_t pos);
extern void dun_place_granite(dun_ptr dun, point_t pos);
extern void dun_place_permanent(dun_ptr dun, point_t pos);
extern void dun_place_mountain(dun_ptr dun, point_t pos);
extern void dun_place_mountain_wall(dun_ptr dun, point_t pos);
extern void dun_place_rubble(dun_ptr dun, point_t pos);
extern void dun_place_magma(dun_ptr dun, point_t pos);
extern void dun_place_magma_aux(dun_ptr dun, point_t pos, int gold_pct, int secret_pct);
extern void dun_place_quartz(dun_ptr dun, point_t pos);
extern void dun_place_quartz_aux(dun_ptr dun, point_t pos, int gold_pct, int secret_pct);

/************************************************************************
 * Doors (FEAT_DOOR)
 *
 * Doors may be open, closed, locked, jammed or broken. A locked door
 * requires skill and patience to pick the lock; a jammed door requires
 * brute force to open (which might break it). Closed, locked and jammed
 * doors are considered 'closed' for ai and movement purposes: they block
 * line of sight, movement and dis-allow object drops. Monsters and players
 * must first open them somehow.
 *
 * Subtypes of doors included curtains; these may only be open or closed
 * and, when closed, only block los (not projection). They also can be
 * walked thru, unlike normal closed doors.
 *
 * Secret doors are actually walls until discovered.
 ************************************************************************/
extern bool cell_is_door(dun_cell_ptr cell);

extern bool door_is_door(dun_cell_ptr cell);
extern bool door_is_curtain(dun_cell_ptr cell);
extern bool door_is_secret(dun_cell_ptr cell);  /* wall_has_secret_door */

/* normal doors only; no curtains */
extern bool door_is_open(dun_cell_ptr cell);   /* not broken: door could be closed */
extern bool door_is_closed(dun_cell_ptr cell); /* locked/jammed counts as closed */
extern bool door_is_locked(dun_cell_ptr cell);
extern bool door_is_jammed(dun_cell_ptr cell);
extern bool door_is_broken(dun_cell_ptr cell);

extern bool curtain_is_open(dun_cell_ptr cell);
extern bool curtain_is_closed(dun_cell_ptr cell);

/* curtains */
extern bool cell_is_open_curtain(dun_cell_ptr cell);
extern bool cell_is_closed_curtain(dun_cell_ptr cell);

/* private */
extern void cell_make_open_door(dun_cell_ptr cell);
extern void cell_make_broken_door(dun_cell_ptr cell);
extern void cell_make_closed_door(dun_cell_ptr cell);
extern void cell_make_locked_door(dun_cell_ptr cell);
extern void cell_make_jammed_door(dun_cell_ptr cell);
extern void cell_make_open_curtain(dun_cell_ptr cell);
extern void cell_make_closed_curtain(dun_cell_ptr cell);

extern void dun_place_random_door(dun_ptr dun, point_t pos);
extern void dun_place_secret_door(dun_ptr dun, point_t pos);

extern void dun_place_open_door(dun_ptr dun, point_t pos);
extern void dun_place_broken_door(dun_ptr dun, point_t pos);
extern void dun_place_closed_door(dun_ptr dun, point_t pos);
extern void dun_place_locked_door(dun_ptr dun, point_t pos);
extern void dun_place_jammed_door(dun_ptr dun, point_t pos);
extern void dun_place_open_curtain(dun_ptr dun, point_t pos);
extern void dun_place_closed_curtain(dun_ptr dun, point_t pos);

/************************************************************************
 * Stairs (FEAT_STAIRS)
 *
 * Stairs connect dungeon levels, connect the surface to specific dungeon
 * types (stairs_enter_dungeon) and allow the plr to enter specific quests
 * (stairs_enter_quest).
 *
 * Note that stair placement requires dun->stairs to be accurately maintained.
 * cf dun_gen_connected for an example.
 ************************************************************************/
extern bool cell_is_stairs(dun_cell_ptr cell);

extern bool stairs_go_down(dun_cell_ptr cell);
extern bool stairs_go_up(dun_cell_ptr cell);

/* stairs might lead to a quest; always down but we could support up */
extern bool stairs_enter_quest(dun_cell_ptr cell);
extern int  stairs_quest_id(dun_cell_ptr cell);

/* surface: stairs might lead to a dungeon; always down but we could support up */
extern bool stairs_enter_dungeon(dun_cell_ptr cell);
extern int  stairs_dun_type_id(dun_cell_ptr cell);

/* private */
extern void cell_make_downstairs(dun_cell_ptr cell);
extern void cell_make_upstairs(dun_cell_ptr cell);
extern void cell_make_dungeon_entrance(dun_cell_ptr cell, int dun_type_id);
extern void cell_make_quest_entrance(dun_cell_ptr cell, int quest_id);

extern void dun_place_downstairs(dun_ptr dun, point_t pos);
extern void dun_place_upstairs(dun_ptr dun, point_t pos);
extern void dun_place_quest_entrance(dun_ptr dun, point_t pos, int quest_id);
extern void dun_place_dungeon_entrance(dun_ptr dun, point_t pos, int dun_type_id);

/************************************************************************
 * Trees (FEAT_TREE)
 *
 * Trees are like walls in that they block line of sight and spell
 * projection, but unlike in that they allow (restricted) movement.
 * They also support object drops. Trees are often used in place of
 * walls for themed dungeons (e.g. D_FOREST).
 *
 * There are no subtypes of trees.
 ************************************************************************/
extern bool cell_is_tree(dun_cell_ptr cell);

/* private */
extern void cell_make_tree(dun_cell_ptr cell);
extern void dun_place_tree(dun_ptr dun, point_t pos);

/************************************************************************
 * Water (FEAT_WATER)
 *
 * This flavorful terrain provides habitation for aquatic monsters.
 * Subtypes include swamps, shallow water and deep water. Deep water
 * requires mon_can_swim or mon_can_fly. The plr can usually swim, though
 * he may drown if overburdened.
 *
 * On the surface, shallow and deep water are used for lakes and oceans.
 ************************************************************************/
extern bool cell_is_water(dun_cell_ptr cell);

extern bool water_is_swamp(dun_cell_ptr cell);
extern bool water_is_deep(dun_cell_ptr cell);
extern bool water_is_shallow(dun_cell_ptr cell);

/* private */
extern void cell_make_deep_water(dun_cell_ptr cell);
extern void cell_make_shallow_water(dun_cell_ptr cell);
extern void cell_make_swamp(dun_cell_ptr cell);

extern void dun_place_deep_water(dun_ptr dun, point_t pos);
extern void dun_place_shallow_water(dun_ptr dun, point_t pos);
extern void dun_place_swamp(dun_ptr dun, point_t pos);

/************************************************************************
 * Lava (FEAT_LAVA)
 *
 * This flavorful terrain provides habitation for demonic monsters.
 * On the surface, these terrains represent volcanic mountains.
 * Subtypes include shallow lava and deep lava. Both will burn monsters
 * and players, though levitation helps.
 ************************************************************************/
extern bool cell_is_lava(dun_cell_ptr cell);

extern bool lava_is_deep(dun_cell_ptr cell);
extern bool lava_is_shallow(dun_cell_ptr cell);

/* private */
extern void cell_make_deep_lava(dun_cell_ptr cell);
extern void cell_make_shallow_lava(dun_cell_ptr cell);

extern void dun_place_deep_lava(dun_ptr dun, point_t pos);
extern void dun_place_shallow_lava(dun_ptr dun, point_t pos);

/************************************************************************
 * Chasm (FEAT_CHASM)
 *
 * Chasms are deep, dark pits, requiring levitation to traverse. They
 * are common in cavernous dungeons.
 ************************************************************************/
extern bool cell_is_chasm(dun_cell_ptr cell);

/* private */
extern void cell_make_chasm(dun_cell_ptr cell);
extern void dun_place_chasm(dun_ptr dun, point_t pos);

/************************************************************************
 * Portals (FEAT_PORTAL)
 *
 * Believed to be the work of wizards, these magical tiles transport the
 * player, either recalling them to the surface, to the dungeon, or even
 * to another world altogether.
 ************************************************************************/
extern bool cell_is_portal(dun_cell_ptr cell);

extern bool portal_is_recall(dun_cell_ptr cell);
extern bool portal_is_travel(dun_cell_ptr cell);

/* private */
extern void cell_make_recall(dun_cell_ptr cell);
extern void cell_make_travel(dun_cell_ptr cell);
extern void dun_place_recall(dun_ptr dun, point_t pos);
extern void dun_place_travel(dun_ptr dun, point_t pos);

/************************************************************************
 * Shops and Buildings (FEAT_BLDG)
 ************************************************************************/
extern bool cell_is_bldg(dun_cell_ptr cell);

/* shops use SHOP_FOO codes from shop.h */
extern bool bldg_is_shop(dun_cell_ptr cell);
extern bool shop_is_general_store(dun_cell_ptr cell);
extern bool shop_is_armory(dun_cell_ptr cell);
extern bool shop_is_weapon_smiths(dun_cell_ptr cell);
extern bool shop_is_temple(dun_cell_ptr cell);
extern bool shop_is_alchemist(dun_cell_ptr cell);
extern bool shop_is_magic_shop(dun_cell_ptr cell);
extern bool shop_is_black_market(dun_cell_ptr cell);
extern bool shop_is_home(dun_cell_ptr cell);
extern bool shop_is_bookstore(dun_cell_ptr cell);
extern bool shop_is_museum(dun_cell_ptr cell);
extern bool shop_is_jeweler(dun_cell_ptr cell);
extern int  shop_id(dun_cell_ptr cell);
extern bool shop_is_(dun_cell_ptr cell, int which);

/* buildings use BLDG_FOO codes from shop.h */
extern bool bldg_is_bldg(dun_cell_ptr cell);
extern bool bldg_is_inn(dun_cell_ptr cell);
extern bool bldg_is_castle(dun_cell_ptr cell);
extern bool bldg_is_fighters_guild(dun_cell_ptr cell);
extern bool bldg_is_archers_guild(dun_cell_ptr cell);
extern bool bldg_is_thieves_guild(dun_cell_ptr cell);
extern bool bldg_is_wizards_guild(dun_cell_ptr cell);
extern bool bldg_is_priests_guild(dun_cell_ptr cell);
extern bool bldg_is_hunters_office(dun_cell_ptr cell);
extern int  bldg_id(dun_cell_ptr cell);
extern bool bldg_is_(dun_cell_ptr cell, int which);

/* private */
extern void cell_make_shop(dun_cell_ptr cell, int which);
extern void cell_make_bldg(dun_cell_ptr cell, int which);

extern void cell_make_general_store(dun_cell_ptr cell);
extern void cell_make_armory(dun_cell_ptr cell);
extern void cell_make_weapon_smiths(dun_cell_ptr cell);
extern void cell_make_temple(dun_cell_ptr cell);
extern void cell_make_alchemist(dun_cell_ptr cell);
extern void cell_make_magic_shop(dun_cell_ptr cell);
extern void cell_make_black_market(dun_cell_ptr cell);
extern void cell_make_home(dun_cell_ptr cell);
extern void cell_make_bookstore(dun_cell_ptr cell);
extern void cell_make_museum(dun_cell_ptr cell);
extern void cell_make_jeweler(dun_cell_ptr cell);

extern void cell_make_inn(dun_cell_ptr cell);
extern void cell_make_castle(dun_cell_ptr cell);
extern void cell_make_fighters_guild(dun_cell_ptr cell);
extern void cell_make_archers_guild(dun_cell_ptr cell);
extern void cell_make_thieves_guild(dun_cell_ptr cell);
extern void cell_make_wizards_guild(dun_cell_ptr cell);
extern void cell_make_priests_guild(dun_cell_ptr cell);
extern void cell_make_hunters_office(dun_cell_ptr cell);

extern void dun_place_general_store(dun_ptr dun, point_t pos);
extern void dun_place_armory(dun_ptr dun, point_t pos);
extern void dun_place_weapon_smith(dun_ptr dun, point_t pos);
extern void dun_place_temple(dun_ptr dun, point_t pos);
extern void dun_place_alchemist(dun_ptr dun, point_t pos);
extern void dun_place_magic_shop(dun_ptr dun, point_t pos);
extern void dun_place_black_market(dun_ptr dun, point_t pos);
extern void dun_place_home(dun_ptr dun, point_t pos);
extern void dun_place_bookstore(dun_ptr dun, point_t pos);
extern void dun_place_museum(dun_ptr dun, point_t pos);
extern void dun_place_jeweler(dun_ptr dun, point_t pos);

extern void dun_place_inn(dun_ptr dun, point_t pos);
extern void dun_place_castle(dun_ptr dun, point_t pos);
extern void dun_place_fighters_guild(dun_ptr dun, point_t pos);
extern void dun_place_archers_guild(dun_ptr dun, point_t pos);
extern void dun_place_thieves_guild(dun_ptr dun, point_t pos);
extern void dun_place_wizards_guild(dun_ptr dun, point_t pos);
extern void dun_place_priests_guild(dun_ptr dun, point_t pos);
extern void dun_place_hunters_office(dun_ptr dun, point_t pos);

/************************************************************************
 * Pattern (FEAT_PATTERN)
 *
 * Unique to W_AMBER (and D_AMBER), the pattern can be "walked" by the
 * player to access recall, healing and restoration; cf ROOM_PATTERN.
 * In D_AMBER, pattern rooms occassionally fill the roll of ROOM_RECALL.
 * cf _gen_recall in dun_gen.c.
 *
 * pattern_legal_move confirms an intended move by the player ... The
 * pattern must be walked in its entirety, and in the correct sequence.
 * This method should be called to check any player movement, at least
 * in D_AMBER (cf move_player). It might prompt the user.
 *
 * Monsters cannot walk the pattern, but they may fly over it. 
 ************************************************************************/
extern bool pattern_legal_move(dun_cell_ptr from, dun_cell_ptr to);

extern bool cell_is_pattern(dun_cell_ptr cell);

extern bool pattern_is_start(dun_cell_ptr cell);
extern bool pattern_is_1(dun_cell_ptr cell);
extern bool pattern_is_2(dun_cell_ptr cell);
extern bool pattern_is_3(dun_cell_ptr cell);
extern bool pattern_is_4(dun_cell_ptr cell);
extern bool pattern_is_end(dun_cell_ptr cell);  /* gives one time healing/restoration */
extern bool pattern_is_exit(dun_cell_ptr cell); /* prompts for recall/teleport/etc; multi-use */

/* creation of pattern rooms is never hand-coded: cf ../lib/edit/rooms.txt (grep T:ROOM:PATTERN) */
extern void cell_make_pattern_start(dun_cell_ptr cell);
extern void cell_make_pattern_1(dun_cell_ptr cell);
extern void cell_make_pattern_2(dun_cell_ptr cell);
extern void cell_make_pattern_3(dun_cell_ptr cell);
extern void cell_make_pattern_4(dun_cell_ptr cell);
extern void cell_make_pattern_end(dun_cell_ptr cell);
extern void cell_make_pattern_exit(dun_cell_ptr cell);

extern void dun_place_pattern_start(dun_ptr dun, point_t pos);
extern void dun_place_pattern_1(dun_ptr dun, point_t pos);
extern void dun_place_pattern_2(dun_ptr dun, point_t pos);
extern void dun_place_pattern_3(dun_ptr dun, point_t pos);
extern void dun_place_pattern_4(dun_ptr dun, point_t pos);
extern void dun_place_pattern_end(dun_ptr dun, point_t pos);
extern void dun_place_pattern_exit(dun_ptr dun, point_t pos);

/************************************************************************
 * XXX
 ************************************************************************/
/* view and spell projection */
extern bool cell_stop_disintegrate(dun_cell_ptr cell);
extern bool cell_los(dun_cell_ptr cell);
extern bool cell_project(dun_cell_ptr cell);

/* cell->type->methods (dun_cell_type is private) */
extern cptr cell_name(dun_cell_ptr cell);
extern cptr cell_desc(dun_cell_ptr cell);
extern bool cell_allow_obj(dun_cell_ptr cell);
extern bool cell_allow_mon(dun_cell_ptr cell, mon_ptr mon);
extern bool cell_allow_mon_race(dun_cell_ptr cell, mon_race_ptr race);
extern bool cell_allow_plr(dun_cell_ptr cell);
extern void cell_accept_plr(dun_ptr dun, point_t pos, dun_cell_ptr cell);
extern void cell_accept_mon(dun_ptr dun, point_t pos, dun_cell_ptr cell, mon_ptr mon);
extern bool cell_affect(dun_ptr dun, point_t pos, dun_cell_ptr cell, int gf, int power);
extern void cell_process_plr(void);
extern void cell_detect(dun_ptr dun, point_t pos, dun_cell_ptr cell);

/* illusions: ai code only (including flow) */
extern bool illusion_los(dun_cell_ptr cell);
extern bool illusion_project(dun_cell_ptr cell);
extern bool illusion_allow_mon(dun_cell_ptr cell, mon_ptr mon);
extern void illusion_display(dun_cell_ptr cell, int light, map_char_ptr mc);

/* some historical predicates */
extern bool cell_is_boring(dun_cell_ptr cell);
extern bool cell_place(dun_cell_ptr cell);
extern bool cell_teleportable(dun_cell_ptr cell);
extern bool cell_notice(dun_cell_ptr cell);

/* visuals: use of map_char_t allows layering (cf _floor_display) */
extern void cell_display(dun_cell_ptr cell, int light, map_char_ptr mc);
extern term_char_t cell_ascii(dun_cell_ptr cell);
extern int  cell_priority(dun_cell_ptr cell);
extern int  cell_light(dun_cell_ptr cell);

extern bool dun_feat_init(void); /* init_angband and reset_visuals */

/************************************************************************
 * Flags (u32b)
 ************************************************************************/
enum {
    /* plr awareness */
    CELL_MAP =     0x0001,  /* displayed on map window */
    CELL_AWARE =   0x0002,  /* plr can travel to location */
    CELL_UNSAFE =  0x0004,  /* trap status unknown */
    CELL_DETECT =  0x0008,  /* 'interior' of trap detect region (not 'boundary') */

    /* temp flags view and light algorithms */
    CELL_NOTE =    0x0010,  /* cell requires dun_note_pos */
    CELL_REDRAW =  0x0020,  /* cell requires dun_draw_pos */
    CELL_TEMP =    0x0040,  /* e.g. cell part of old view set (used to detect changes in view) */

    /* info and behavior */
    CELL_LIT =     0x0080,  /* cell is permanently illuminated by lite_area (or lit room) */
    CELL_ROOM =    0x0100,  /* cell was generated as part of a room */
    CELL_VAULT =   0x0200,  /* cell was generated as part of a vault */
    CELL_PROJECT = 0x0400,  /* formerly feat->FF_PROJECT */
    CELL_LOS =     0x0800,  /* formerly feat->FF_LOS */
    CELL_PERM =    0x1000,  /* formerly feat->FF_PERMANENT */
    CELL_DARK =    0x2000,  /* cell resists light: never gains CELL_LIT */
    CELL_LIGHT =   0x4000,  /* cell resists darkness: never loses CELL_LIT */

    CELL_LO_MASK = 0x0000FFFF,
    /* XXX         0x00FF0000 probably suffices for custom overloaded hi bits */
    CELL_SECRET =  0x01000000,  /* secret trap, door, treasure ... technically a hi bit (cf _illusion) */


    /* XXX the 5 highest bits are reserved for dun_gen and D_WORLD flags; these might
     * need to be cleared and overloaded at some point, but currently they are
     * left alone. For example, AWARE|UNSAFE|DETECT|NOTE|REDRAW are not needed
     * until after generation, so could be moved to the high range, freeing up more
     * lo bits for common use. D_WORLD only currently needs MAP, but it needs this
     * concurrently with TOWN|DUNGEON|ROAD|QUEST|RIVER. Of course, D_WORLD does not
     * need INNER|OUTER|SOLID|EXTRA ... these are for the tunneler. dun_gen also uses TEMP XXX */

    /* dun_gen needs the following 'wall types' for the tunneler */
    CELL_INNER =   0x08000000,
    CELL_OUTER =   0x10000000,
    CELL_SOLID =   0x20000000,
    CELL_EXTRA =   0x40000000,

    /* D_WORLD needs the following for the surface map ... overlap with dun_gen flags */
    CELL_TOWN =    0x08000000, /* parm2 gives town_id */
    CELL_DUNGEON = 0x10000000, /* parm2 gives dun_type_id */
    CELL_ROAD =    0x20000000,
    CELL_QUEST =   0x40000000,
    /* CELL_RIVER = 0x80000000, is an illegal enumeration */
};
#define CELL_RIVER    0x80000000
#define CELL_GEN_MASK (CELL_ROOM | CELL_VAULT | CELL_INNER | CELL_OUTER | CELL_SOLID | CELL_EXTRA)
#define CELL_WORLD_MASK (CELL_TOWN | CELL_DUNGEON | CELL_ROAD | CELL_QUEST | CELL_RIVER)

/************************************************************************
 * Actions
 ************************************************************************/
/* Actions -- such as tunneling, searching, disarming -- are generally repeatable,
 * require skill to succeed, and may be 'magically forced' to succeed. */
enum { /* return values */
    ACTION_SUCCESS,  /* e.g. disarmed a trap */
    ACTION_ABORT,    /* e.g. no trap to disarm */
    ACTION_CONTINUE, /* e.g. failed to disarm, but repeat is ok */
    ACTION_FAIL      /* e.g. failed to disarm, set off trap */
};
enum { /* 'options' flags */
    ACTION_FORCE = 0x01, /* bypass any skill checks and force ACTION_SUCESS (if possible) */
    ACTION_QUIET = 0x02, /* skip any messages */
};

/* plr actions */
extern int dun_disarm(dun_ptr dun, point_t pos, u32b options);
extern int dun_open(dun_ptr dun, point_t pos, u32b options);
extern int dun_close(dun_ptr dun, point_t pos, u32b options);
extern int dun_bash(dun_ptr dun, point_t pos, u32b options);
extern int dun_jam(dun_ptr dun, point_t pos, u32b options);
extern int dun_tunnel(dun_ptr dun, point_t pos, u32b options);
extern int dun_search(dun_ptr dun, point_t pos, u32b options);

/* mon actions */
extern int dun_open_mon(dun_ptr dun, point_t pos, mon_ptr mon);

/************************************************************************
 * Room Templates XXX dun_room.c pending XXX
 ************************************************************************/
extern errr parse_room_grid_feature(char* name, char **args, int arg_ct, room_grid_ptr grid);
extern errr parse_room_grid_trap(char **args, int arg_ct, room_grid_ptr grid);
extern void apply_room_grid_feat(point_t p, room_grid_ptr grid);

#endif
