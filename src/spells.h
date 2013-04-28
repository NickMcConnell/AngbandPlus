/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* spells.h: spell effects */

#ifndef SPELLS_H_INCLUDED
#define SPELLS_H_INCLUDED

/*
 * Spell types used by project(), and related functions.
 */
enum
{
        GF_ARROW = 1,	/* Pure damage (physical ranged attack) */
        GF_MISSILE,	/* Pure damage (magic missile) */
        GF_MANA,	/* Pure damage (mana) */
        GF_HOLY_ORB,	/* Orb of Draining */
        GF_LITE_WEAK,	/* "Weak" light (damages only susceptible monsters) */
        GF_DARK_WEAK,	/* "Weak" darkness (does no damage) */
        GF_WATER,	/* Water */
        GF_PLASMA,	/* Plasma */
        GF_METEOR,	/* Pure damage (meteors) */
        GF_ICE,		/* Ice (not cold!) */
        GF_GRAVITY,	/* Gravity */
        GF_INERTIA,	/* Inertia */
        GF_FORCE,	/* Force */
        GF_TIME,	/* Time */
        GF_ACID,	/* Elements */
        GF_ELEC,
        GF_FIRE,
        GF_COLD,
        GF_POIS,
        GF_WEBBING,	/* No damage, creates webs */
        GF_LITE,	/* "Strong" light (damages everybody) */
        GF_DARK,	/* "Strong" darkness (damages everybody) */
        GF_CRYO,	/* "Freezing" (irresistable cumulative slowness; possible instakill) */
        GF_CONFUSION,	/* Confusion (damaging) */
        GF_SOUND,	/* Sound */
        GF_SHARD,	/* Shards */
        GF_NEXUS,	/* Nexus */
        GF_NETHER,	/* Nether */
        GF_CHAOS,	/* Chaos */
        GF_DISENCHANT,	/* Disenchantment */
        GF_WIND,	/* Wind */
        GF_KILL_WALL,	/* Stone-to-mud (damages susceptible monsters) */
        GF_KILL_DOOR,	/* Door destruction */
        GF_KILL_TRAP,	/* Trap destruction */
        GF_MAKE_WALL,	/* Wall creation */
        GF_MAKE_DOOR,	/* Door creation */
        GF_MAKE_TRAP,	/* (Player) trap creation */
        GF_AWAKE_TREE,	/* "Awakes tree", turns "tree" features into friendly ents */
        GF_AWAY_UNDEAD,	/* Teleports away undeads */
        GF_AWAY_EVIL,	/* Teleports away evil creatures */
        GF_AWAY_ALL,	/* Teleports away */
        GF_TURN_UNDEAD,	/* Scares undeads */
        GF_TURN_EVIL,	/* Scares evil creatures */
        GF_TURN_ALL,	/* Scares everyone */
        GF_DISP_UNDEAD,	/* Pure damage (dispelling) to undeads */
        GF_DISP_EVIL,	/* Pure damage (dispelling) to evil creatures */
        GF_DISP_ALL,	/* Pure damage (dispelling) */
        GF_MAKE_WARY,	/* "Makes wary" monsters */
        GF_OLD_CLONE,	/* Clones monsters */
        GF_OLD_POLY,	/* Polymorphs monsters */
        GF_OLD_HEAL,	/* Heals monsters */
        GF_OLD_SPEED,	/* Speeds up monsters */
        GF_OLD_SLOW,	/* Slows monsters */
        GF_OLD_CONF,	/* Confuses monsters (does no damage) */
        GF_OLD_SLEEP,	/* Sleeps monsters */
        GF_OLD_DRAIN,	/* Pure damage (life draining) to living monsters */
        GF_SPORE,	/* Spores (poison+disease) */
        GF_CHARM,	/* Charming */
        GF_MAKE_WATER,	/* Terrain feature creation */
        GF_MAKE_ICE,
        GF_MAKE_FIRE,
        GF_MAKE_LAVA,
        GF_MAKE_SAND,
        GF_MAKE_ROCKS,
        GF_MAKE_FOG,
        GF_MAKE_ABYSS,
        GF_MAKE_GRASS,
        GF_MAKE_TREE
};

/*** Function flags ***/

enum
{
        PROJECT_NO,
        PROJECT_NOT_CLEAR,
        PROJECT_CLEAR
};

/*
 * Bit flags for the "project()", the "projectable()", and/or the
 * "project_path()" functions.
 *
 *   BEAM:  Work as a beam weapon (affect every grid passed through)
 *   ARC:   Act as an arc spell (a portion of a caster-centered ball)
 *   STAR:  Act as a starburst - a randomized ball
 *   BOOM:  Explode in some way
 *   WALL:  Affect one layer of any wall, even if not passable
 *   PASS:  Ignore walls entirely
 *   GRID:  Affect each grid in the "blast area" in some way
 *   ITEM:  Affect each object in the "blast area" in some way
 *   KILL:  Affect each monster in the "blast area" in some way
 *   PLAY:  Explicitly affect the player
 *   SAFE:  Hack -- do not affect monsters of caster's type
 *   HIDE:  Hack -- disable graphical effects of projection
 *   STOP:  Stop as soon as we hit a monster
 *   JUMP:  Jump directly to the target location
 *   THRU:  Continue "through" the target (used for projectiles)
 *   CHCK:  Note occupied grids, but do not stop at them
 *   ORTH:  Projection cannot move diagonally (used for wall spells)
 */

/* Projection types */
#define PROJECT_BEAM         0x00000001
#define PROJECT_ARC          0x00000002
#define PROJECT_STAR         0x00000004

/* What projections do */
#define PROJECT_BOOM         0x00000040
#define PROJECT_WALL         0x00000080
#define PROJECT_PASS         0x00000100

/* What projections affect */
#define PROJECT_GRID         0x00000800
#define PROJECT_ITEM         0x00001000
#define PROJECT_KILL         0x00002000
#define PROJECT_PLAY         0x00004000

/* Graphics */
#define PROJECT_HIDE         0x00040000
#define PROJECT_NO_REDRAW    0x00080000

/* How projections travel */
#define PROJECT_STOP         0x00200000
#define PROJECT_JUMP         0x00400000
#define PROJECT_THRU         0x00800000
#define PROJECT_CHCK         0x01000000
#define PROJECT_ORTH	     0x02000000

/*
 * An arc with a width (in degrees) less than this value will lose less
 * power over distance.
 */
#define ARC_STANDARD_WIDTH     90

/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT   0x01
#define ENCH_TODAM   0x02
#define ENCH_TOAC    0x04


/* spells-util.c */
void teleport_away(int m_idx, int dis);
void teleport_player(int dis);
void teleport_player_to(int ny, int nx);
void teleport_towards(int oy, int ox, int ny, int nx);
void teleport_player_level(void);
int set_acid_destroy(const object_type *o_ptr);
int set_elec_destroy(const object_type *o_ptr);
int set_fire_destroy(const object_type *o_ptr);
int set_cold_destroy(const object_type *o_ptr);
void take_hit(int dam, cptr kb_str);
void acid_dam(int dam, cptr kb_str);
void elec_dam(int dam, cptr kb_str);
void fire_dam(int dam, cptr kb_str);
void cold_dam(int dam, cptr kb_str);
bool inc_stat(int stat);
bool dec_stat(int stat, int amount, bool permanent);
bool res_stat(int stat);
void disease(int *damage);
bool apply_disenchant(int mode);
void calc_starburst(int height, int width, byte *arc_first, byte *arc_dist, int *arc_num);
void spread_cave_temp(int y1, int x1, int range, bool room, bool walls);
void clear_temp_array(void);
bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
			 u32b flg, int degrees, byte source_diameter);

/* spells.c */
void spell_wonder(int dir);
bool hp_player(int num);
void warding_glyph(void);
bool do_dec_stat(int stat);
bool do_res_stat(int stat);
bool do_inc_stat(int stat);
void identify_pack(void);
bool remove_curse(void);
bool remove_all_curse(void);
bool restore_level(void);
void self_knowledge(void);
bool lose_all_info(void);
void set_recall(void);
bool detect_traps(void);
bool detect_doors(void);
bool detect_stairs(void);
bool detect_treasure(void);
bool detect_objects_gold(void);
bool detect_objects_normal(void);
bool detect_objects_magic(void);
bool detect_monsters_normal(void);
bool detect_monsters_invis(void);
bool detect_monsters_evil(void);
bool detect_monsters_lawful(void);
bool detect_monsters_chaotic(void);
bool detect_all(void);
void stair_creation(void);
bool enchant(object_type *o_ptr, int n, int eflag);
bool enchant_spell(int num_hit, int num_dam, int num_ac);
bool ident_spell(void);
bool identify_fully(void);
bool recharge(int num);
bool speed_monsters(void);
bool slow_monsters(int power);
bool sleep_monsters(int power);
bool banish_evil(int dist);
bool turn_undead(int power);
bool dispel_undead(int dam);
bool dispel_evil(int dam);
bool dispel_monsters(int dam);
void aggravate_monsters(int who);
void mass_aggravate_monsters(int who);
bool banishment(void);
bool mass_banishment(void);
bool probing(void);
void destroy_area(int y1, int x1, int r, bool full);
void earthquake(int cy, int cx, int r);
void lite_room(int y1, int x1);
void unlite_room(int y1, int x1);
bool lite_area(int dam, int rad);
bool unlite_area(int dam, int rad);
bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ, u32b flg);
bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ, u32b flg);
bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ, u32b flg, int source_diameter);
bool project_los_not_player(int y1, int x1, int dam, int typ);
bool project_arc(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ, u32b flg, int degrees);
bool project_star(int who, int rad, int y0, int x0, int dam, int typ, u32b flg);
bool project_los(int typ, int dam);
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg);
bool fire_ball(int typ, int dir, int dam, int rad);
bool fire_orb(int typ, int dir, int dam, int rad);
bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg,
	int source_diameter);
bool fire_arc(int typ, int dir, int dam, int rad, int degrees);
bool fire_swarm(int num, int typ, int dir, int dam, int rad);
bool fire_bolt(int typ, int dir, int dam);
bool fire_beam(int typ, int dir, int dam);
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
bool fire_jump_ball(int typ, int dam, int rad, int diminish, int limit);
bool project_arc(int who, int rad, int y0, int x0, int y1, int x1,
	int dam, int typ, u32b flg, int degrees);
bool project_star(int who, int rad, int y0, int x0, int dam, int typ,
	u32b flg);
bool project_los_not_player(int y1, int x1, int dam, int typ);
bool project_los(int typ, int dam);
bool explosion(int who, int rad, int y0, int x0, int dam, int typ);
bool lite_line(int dir);
bool strong_lite_line(int dir);
bool drain_life(int dir, int dam);
bool wall_to_mud(int dir, int dam);
bool destroy_door(int dir);
bool disarm_trap(int dir);
bool heal_monster(int dir, int dam);
bool speed_monster(int dir);
bool slow_monster(int dir);
bool sleep_monster(int dir);
bool confuse_monster(int dir, int plev);
bool charm_monster(int dir, int plev);
bool poly_monster(int dir);
bool clone_monster(int dir);
bool fear_monster(int dir, int plev);
bool teleport_monster(int dir);
bool door_creation(void);
bool trap_creation(void);
bool destroy_doors_touch(void);
bool sleep_monsters_touch(void);
bool curse_armor(void);
bool curse_weapon(void);
void brand_object(object_type *o_ptr, byte brand_type);
void brand_weapon(void);
bool brand_ammo(void);
bool brand_bolts(void);
void ring_of_power(int dir);
void do_reforge_artifact(void);

#endif /* SPELLS_H_INCLUDED */
