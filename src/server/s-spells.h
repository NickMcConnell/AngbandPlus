/*
 * File: s-spells.h
 * Purpose: Spell implementations and helpers
 */

#ifndef S_SPELLS_H
#define S_SPELLS_H

#include "../common/spells.h"

/*
 * Bit flags for the "project()" function
 *
 *   NONE: No flags
 *   JUMP: Hack -- jump directly to the target location
 *   BEAM: Work as a beam weapon (affect every grid passed through)
 *   THRU: Continue "through" the target (used for "bolts"/"beams")
 *   STOP: Stop as soon as we hit a monster (used for "bolts")
 *   GRID: Affect each grid in the "blast area" in some way
 *   ITEM: Affect each object in the "blast area" in some way
 *   FREE: Affect each monster in the "blast area" in some way
 *   HIDE: Hack -- disable "visual" feedback from projection
 *   AWARE: Effects are already obvious to the player
 */
#define PROJECT_NONE    0x0000
#define PROJECT_JUMP    0x0001
#define PROJECT_BEAM    0x0002
#define PROJECT_THRU    0x0004
#define PROJECT_STOP    0x0008
#define PROJECT_GRID    0x0010
#define PROJECT_ITEM    0x0020
#define PROJECT_KILL    0x0040
#define PROJECT_HIDE    0x0080
#define PROJECT_AWARE   0x0100

/*
 * Bit flags for the "enchant()" function
 */
#define ENCH_TOHIT   0x01
#define ENCH_TODAM   0x02
#define ENCH_TOAC    0x04

/* spells1.c */
extern int check_for_resist_aux(struct player *p, int type, bitflag flags[OF_SIZE],
    bool real);
extern int check_for_resist(struct player *p, int type, bool real);
extern bool check_side_immune(struct player *p, int type);
extern int inven_damage(struct player *p, int type, int cperc);
extern int adjust_dam(struct player *p, int type, int dam, aspect dam_aspect, int resist);
extern void monster_learn_resists(struct monster *m, struct player *p, int type);
extern void dedup_hates_flags(bitflag *f);
extern s16b poly_r_idx(int depth, int r_idx);
extern bool teleport_away(struct monster *m, int dis);
extern bool teleport_to(int depth, int m_idx, int ny, int nx);
extern bool teleport_player(struct player *p, int dis);
extern bool teleport_player_aux(struct player *p, int dis, bool safe_ghost);
extern bool teleport_player_to(struct player *p, int ny, int nx);
extern bool teleport_player_level(struct player *p);
extern bool deep_descent(struct player *p, bool apply);
extern byte spell_color(int type);
extern bool take_hit(struct player *p, int damage, const char *kb_str, bool non_physical);
extern bool res_stat(struct player *p, int stat);
extern bool apply_disenchant(struct player *p, int mode);
extern bool project(int who, int rad, int depth, int y, int x, int dam, int typ, int flg,
    const char *what);
extern bool check_st_anchor(int depth, int y, int x);
extern s16b get_r_idx(const char *name);
extern void poly_dragon(struct player *p, bool msg);
extern void poly_bat(struct player *p, int chance, char *killer);
extern bool poly_race(struct player *p, int r_idx);
extern bool has_bowbrand(struct player *p, byte brand);
extern bool can_swim(struct player *p);
extern void monster_set_master(struct monster *m, struct player *p, byte status);
extern bool can_charm_monster(struct player *p);
extern int charm_monster(struct monster *m, struct player *p, byte status);
extern void bolt_pict(int Ind, int y, int x, int ny, int nx, int typ, byte *a, char *c);

/* spells2.c */
extern const char *desc_stat_neg[];
extern bool hp_player_safe(struct player *p, int num);
extern bool hp_player(struct player *p, int num);
extern void warding_glyph(struct player *p);
extern bool do_dec_stat(struct player *p, int stat, bool perma);
extern bool do_res_stat(struct player *p, int stat);
extern bool do_inc_stat(struct player *p, int stat);
extern void identify_pack(struct player *p);
extern bool remove_curse(struct player *p);
extern bool remove_all_curse(struct player *p);
extern bool restore_level(struct player *p);
extern void map_area(struct player *p);
extern bool detect_traps(struct player *p);
extern bool detect_doorstairs(struct player *p, bool aware);
extern bool detect_treasure(struct player *p, bool aware, bool full);
extern bool detect_close_buried_treasure(struct player *p);
extern bool detect_monsters_normal(struct player *p, bool pause, bool aware);
extern bool detect_monsters_invis(struct player *p, bool pause, bool aware);
extern bool detect_monsters_evil(struct player *p, bool aware);
extern bool detect_monsters_entire_level(struct player *p);
extern bool detect_all(struct player *p, bool aware);
extern void stair_creation(struct player *p);
extern bool enchant(struct player *p, object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(struct player *p, int num_hit, int num_dam, int num_ac);
extern bool ident_spell(struct player *p);
extern bool recharge(struct player *p, int spell_strength);
extern bool project_los(struct player *p, int typ, int dam, bool obvious);
extern bool speed_monsters(struct player *p);
extern bool slow_monsters(struct player *p, bool aware);
extern bool sleep_monsters(struct player *p, bool aware);
extern bool confuse_monsters(struct player *p, bool aware);
extern bool banish_evil(struct player *p, int dist);
extern bool turn_undead(struct player *p);
extern bool dispel_undead(struct player *p, int dam, bool aware);
extern bool dispel_evil(struct player *p, int dam, bool aware);
extern bool dispel_monsters(struct player *p, int dam);
extern void aggravate_monsters(struct player *p, int who);
extern bool banishment(struct player *p);
extern bool mass_banishment(struct player *p);
extern bool probing(struct player *p);
extern bool destroy_area(struct player *p, int depth, int y1, int x1, int r, bool full);
extern bool earthquake(struct player *p, int depth, int cy, int cx, int r);
extern void cave_unlight_aux(int depth, struct point_set *ps);
extern void cave_room_aux(struct point_set *seen, int depth, int y, int x);
extern void light_room_aux(struct point_set *ps, int depth, int y1, int x1);
extern bool light_area(struct player *p, int dam, int rad);
extern bool unlight_area(struct player *p, int dam, int rad);
extern bool fire_ball(struct player *p, int typ, int dir, int dam, int rad);
extern bool fire_ball_hack(struct player *p, int typ, int dir, int dam, int rad);
extern bool fire_swarm(struct player *p, int num, int typ, int dir, int dam, int rad);
extern bool project_hook(struct player *p, int typ, int dir, int dam, int flg, const char *what);
extern bool fire_bolt(struct player *p, int typ, int dir, int dam);
extern bool fire_bolt_hack(struct player *p, int typ, int dir, int dam);
extern bool fire_beam(struct player *p, int typ, int dir, int dam);
extern bool fire_beam_hack(struct player *p, int typ, int dir, int dam);
extern bool fire_bolt_or_beam(struct player *p, int prob, int typ, int dir, int dam);
extern bool light_line(struct player *p, int dir);
extern bool strong_light_line(struct player *p, int dir);
extern bool drain_life(struct player *p, int dir, int dam);
extern bool wall_to_mud(struct player *p, int dir);
extern bool destroy_door(struct player *p, int dir);
extern bool disarm_trap(struct player *p, int dir);
extern bool heal_monster(struct player *p, int dir);
extern bool speed_monster(struct player *p, int dir);
extern bool slow_monster(struct player *p, int dir, bool aware);
extern bool sleep_monster(struct player *p, int dir, bool aware);
extern bool confuse_monster(struct player *p, int dir, int plev, bool aware);
extern bool poly_monster(struct player *p, int dir, bool aware);
extern bool clone_monster(struct player *p, int dir);
extern bool fear_monster(struct player *p, int dir, int plev, bool aware);
extern bool teleport_monster(struct player *p, int dir);
extern bool door_creation(struct player *p);
extern bool trap_creation(struct player *p, bool silent);
extern bool destroy_doors_touch(struct player *p);
extern bool sleep_monsters_touch(struct player *p, bool aware);
extern bool curse_armor(struct player *p);
extern bool curse_weapon(struct player *p);
extern void brand_weapon(struct player *p, bool with_fire);
extern bool brand_ammo(struct player *p);
extern void do_ident_item(struct player *p, int item, object_type *o_ptr);
extern int get_player_num(struct player *p);
extern void redraw_picture(struct player *p, int old_num);
extern bool wipe_spell(int depth, int cy, int cx, int r);
extern void restore_hp(struct player *p);
extern void restore_sp(struct player *p);
extern bool sp_player(struct player *p, int num);
extern bool reveal_monsters(struct player *p, bool aware);
extern bool fear_monsters(struct player *p);
extern bool stun_monsters(struct player *p);
extern bool fire_breath(struct player *p, bitflag f[RSF_SIZE], int dir, int dam);
extern bool fire_melee(struct player *p, int typ, int dir, int dam);
extern bool blind_monster(struct player *p, int dir, int plev);
extern void current_clear(struct player *p);
extern void sea_runes(struct player *p);
extern void wall_creation(struct player *p, int dir);
extern void mind_vision(struct player *p, quark_t note);
extern bool telekinesis(struct player *p, quark_t note);
extern void elemental_brand(struct player *p, int tries);
extern bool create_poison(struct player *p);

/* x-spell.c */
extern const char *get_spell_name(int tval, int spell);
extern byte get_spell_dir(int tval, int spell);
extern byte get_spell_proj(int tval, int spell);
extern const char *get_spell_desc(int tval, int spell);
extern void get_spell_info(struct player *p, int spell, char *buf, size_t len);
extern void get_ghost_spell_info(struct player *p, int spell, char *buf, size_t len);
extern void get_mimic_spell_info(struct player *p, int spell, char *buf, size_t len);
extern bool cast_spell(struct player *p, int index, quark_t note, int dir);
extern bool cast_ghost_spell(struct player *p, int ability, int dir);
extern bool cast_mimic_spell(struct player *p, int flag, int dir);
extern bool cast_spell_proj(struct player *p, int book, int index);

#endif
