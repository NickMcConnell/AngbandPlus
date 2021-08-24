#ifndef FUNCTION_DECLARATIONS_H
#define FUNCTION_DECLARATIONS_H

#include <src/object_classes.h>
#include <src/player_classes.h>
#include <src/mon_classes.h>
#include <src/dun_classes.h>
#include "src/structures.h"
#include <QString>
#include <QFile>
#include <QMessageBox>
#include <QPixmap>
#include <QFont>



//calcs.cpp
extern int stat_adj_moria(int stat);
extern void calc_spells(void);
extern int calc_blows(object_type *o_ptr, player_state *new_state);
extern void calc_bonuses(object_type inventory[], player_state *state, bool id_only);
extern int weight_limit(void);
extern int normal_speed_weight_limit(void);
extern void calc_stealth(void);
extern byte calc_energy_gain(byte speed);
extern void notice_stuff(void);
extern void update_stuff(void);
extern void redraw_stuff(void);
extern void handle_stuff(void);

//cave.cpp
extern int distance(int y1, int x1, int y2, int x2);
extern int distance_pythagorean(int y1, int x1, int y2, int x2);
extern bool generic_los(int y1, int x1, int y2, int x2, u16b flg);
extern bool no_light(void);
extern bool cave_valid_bold(int y, int x);
extern byte multi_hued_color(monster_race *r_ptr);
extern bool dtrap_edge(int y, int x);
extern void map_info(s16b y, s16b x);
extern void note_spot(int y, int x);
extern void light_spot(int y, int x);
extern coord make_coords(int y, int x);
extern void draw_coords(void);
extern int vinfo_init(void);
extern void forget_view(void);
extern void update_view(void);
extern void update_flows(bool full);
extern void wiz_light(void);
extern void wiz_dark(void);
extern void town_illuminate(bool daytime);
extern void update_los_proj_move(int y, int x);
extern void cave_alter_feat(int y, int x, int action);
extern void cave_set_feat(int y, int x, u16b feat);
extern int  project_path(u16b *path_g, u16b * path_gx, int range, int y1, int x1, int *y2, int *x2, u32b flg);
extern byte projectable(int y1, int x1, int y2, int x2, u32b flg);
extern void scatter(int *yp, int *xp, int y, int x, int d, int m);
extern void health_track(int m_idx);
extern void track_object(int item);
extern void track_object_kind(int k_idx);
extern void monster_race_track(int r_idx);
extern void feature_kind_track(int f_idx);
extern void disturb(bool stop_search, bool wipe_hotkey);



// dungeon.cpp
extern void do_animation(void);
extern void dungeon_change_level(int dlev);
extern void launch_game();


/* dun_effect.cpp */
extern int scan_effects_grid(int *effects, int size, int y, int x);
extern void effect_prep(int x_idx, byte type, u16b f_idx, byte y, byte x, byte countdown,
                            byte repeats, u16b power, s16b source, u16b flags);
extern bool set_effect_lingering_cloud(int f_idx, byte y, byte x, u16b power, s16b source, u16b flag);
extern bool set_effect_glacier(int f_idx, byte y, byte x, s16b source, u16b flag);
extern bool set_effect_shimmering_cloud(int f_idx, byte y, byte x, byte repeats, u16b power, s16b source, u16b flag);
extern bool set_effect_permanent_cloud(int f_idx, byte y, byte x,	u16b power, u16b flag);
extern bool set_effect_trap_passive(int f_idx, byte y, byte x);
extern bool set_effect_trap_smart(int f_idx, byte y, byte x, u16b flags);
extern bool set_effect_trap_player(int f_idx, byte y, byte x);
extern bool set_effect_glyph(byte y, byte x);
extern bool set_effect_rocks(int f_idx, byte y, byte x);
extern bool set_effect_inscription(int f_idx, byte y, byte x, s16b source, u16b flag);
extern void compact_effects(void);
extern void pick_and_set_trap(int y, int x, int mode);
extern void place_trap(int y, int x, byte mode);
extern void wipe_x_list(void);
extern void delete_effect_idx(int x_idx);
extern void delete_effects(int y, int x);
extern void excise_effect_idx(int x_idx);
extern s16b x_pop(void);
extern void process_effects(void);

// dun_feat_info.cpp
extern QString get_feature_description(int f_idx, bool spoilers, bool include_header);
extern void describe_feature(int f_idx, bool spoilers);

/* dun_feature.c */
extern QString feature_desc(u16b feat, bool add_prefix,	bool get_mimic);
extern int feat_adjust_combat_for_player(int chance, bool being_attacked);
extern int feat_adjust_combat_for_monster(const monster_type *m_ptr,
    int chance, bool being_attacked);
extern void find_secret(int y, int x);
extern u16b feat_state(u16b feat, int action);
extern u16b feat_state_power(u16b feat, int action);
extern u16b feat_state_explicit_power(u16b feat, int action);
extern u16b fire_trap_smart(int f_idx, int y, int x, byte mode, QString* desc);
extern QString hit_trap(int f_idx, int y, int x, byte mode);
extern void feat_near(int feat, int y, int x);
extern void count_feat_everseen(void);
extern bool vault_locked_door(int f_idx);
extern int get_feat_num_prep(void);
extern s16b get_feat_num(int level);
extern u16b pick_trap(byte mode);
extern u16b get_secret_door_num(void);
extern void place_secret_door(int y, int x);
extern void place_closed_door(int y, int x);
extern void place_boring_closed_door(int y, int x);
extern void place_open_door(int y, int x);
extern void place_broken_door(int y, int x);
extern void place_locked_door(int y, int x);
extern void place_jammed_door(int y, int x);
extern void place_random_door(int y, int x);
extern void lore_do_probe_feature(int f_idx);
extern dynamic_grid_type *get_dynamic_terrain(byte y, byte x);
extern void cheat_feature_lore(int f_idx, feature_lore *f_l_ptr);
extern void add_dynamic_terrain(byte y, byte x);
extern void remove_dynamic_terrain(byte y, byte x);
extern void process_dynamic_terrain(void);
extern s16b select_powerful_race(void);
extern QString format_monster_inscription(s16b r_idx);
extern void decipher_strange_inscription(int x_idx);
extern void hit_silent_watcher(int y, int x);
extern bool hit_wall(int y, int x, bool do_action);
extern void update_level_flag(void);
extern u32b get_level_flag(u16b feat);
extern u32b get_level_flag_from_race(monster_race *r_ptr);
extern QString describe_one_level_flag(u32b flag);
extern void debug_all_level_flags(u32b flags);


// dun_generate.cpp
extern void place_random_stairs(int y, int x);
extern void get_mon_hook(byte theme);
extern byte get_nest_theme(int nestlevel, bool quest_theme);
extern byte get_pit_theme(int pitlevel, bool quest_theme);
extern byte get_level_theme(s16b orig_theme_num, bool quest_level);
extern byte max_themed_monsters(const monster_race *r_ptr, u32b max_power);
extern void update_arena_level(byte stage);
extern void generate_cave(void);
extern void set_dungeon_type(u16b dungeon_type);


//dun_process.cpp
extern void process_player_energy(byte energy);
extern void change_player_level(void);

// file_output.cpp
extern bool is_white(QColor this_color);
extern bool is_black(QColor this_color);
extern void save_screenshot(byte do_png);
extern void save_character_file(void);

// hotkeys.cpp
extern void clear_all_hotkeys();
extern bool check_hotkey_commands(int key_press, bool shift_key, bool alt_key, bool ctrl_key, bool meta_key);

/* init_game.cpp */
extern void init_player_graf_info();
extern void init_npp_games(void);
extern void cleanup_npp_games(void);
extern void flavor_init(void);

/* load.cpp */
extern bool load_player(void);
extern bool load_gamemode(void);
extern void load_memory_scores(void);
extern void do_hotkey_import(QString file_name);

// mon_attack.cpp
extern bool make_attack_normal(monster_type *m_ptr);

// messages.cpp
extern QString output_messages(byte max_messages);
extern void message(QString msg);
extern void color_message(QString msg, int which_color);
extern void color_message(QString msg, byte red, byte green, byte blue);
extern void color_message(QString msg, QColor msg_color);


//mon_cast.cpp
extern int choose_ranged_attack(int m_idx, int *tar_y, int *tar_x);
extern bool cave_exist_mon(const monster_race *r_ptr, int y, int x, bool occupied_ok, bool damage_ok, bool can_dig);
extern bool make_attack_ranged(monster_type *m_ptr, int attack, int py, int px);


//mon_classes.cpp

// mon_info.cpp
extern QString get_monster_description(int r_idx, bool spoilers, QString extra_message, bool include_header);
extern void describe_monster(int r_idx, bool spoilers, QString extra_message);


// mon_damage.cpp
extern bool mon_inc_timed(int m_idx, int idx, int v, u16b flag);
extern bool mon_dec_timed(int m_idx, int idx, int v, u16b flag);
extern bool mon_clear_timed(int m_idx, int idx, u16b flag);
extern void wake_monster_attack(monster_type *m_ptr, u16b flag);
extern bool sleep_monster_spell(monster_type *m_ptr, int v, u16b flag);
extern s32b get_experience_by_level(int level);
extern void check_experience(void);
extern void gain_exp(s32b amount);
extern void lose_exp(s32b amount);
extern void monster_death(int m_idx, int who);
extern bool mon_take_hit(int m_idx, int dam, bool *fear, QString note, int who, bool do_death_message);

// mon_move.cpp
extern void find_range(monster_type *m_ptr);
extern QString apply_monster_trap(int f_idx, int y, int x, byte mode);
extern bool make_move(monster_type *m_ptr, int *ty, int *tx, bool fear, bool *bash);
extern s16b process_move(monster_type *m_ptr, int ty, int tx, bool bash);
extern bool get_move(monster_type *m_ptr, int *ty, int *tx, bool *fear, bool must_use_target);
extern int cave_passable_mon(monster_type *m_ptr, int y, int x, bool *bash);

// mon_process.cpp
extern void cloud_surround(int r_idx, int *typ, int *dam, int *rad);
extern void process_entities(void);

//mon_ranged_attack.cpp
extern int get_dam(monster_race *r_ptr, int attack);
extern int get_breath_dam(s16b hit_points, int gf_type, bool powerful);
extern int get_ball_beam_dam(int m_idx, monster_race *r_ptr, int attack, int gf_type, bool powerful);
extern void mon_bolt_no_effect(int m_idx, int typ, int dam);
extern void mon_bolt(int m_idx, int typ, int dam, u32b flg);
extern void mon_beam(int m_idx, int typ, int dam, int range);
extern void mon_ball(int m_idx, int typ, int dam, int rad, int py, int px);
extern void mon_cloud(int m_idx, int typ, int dam, int rad);
extern void mon_arc(int m_idx, int typ, bool noharm, int dam, int rad, int degrees_of_arc);

//mon_util.cpp
extern s16b poly_r_idx(const monster_type *m_ptr);
extern void delete_monster_idx(int i);
extern void delete_monster(int y, int x);
extern void compact_monsters(int size);
extern void wipe_mon_list(void);
extern s16b mon_pop(void);
extern int get_mon_num_prep(void);
extern s16b get_mon_num(int level, int y, int x, byte mp_flags);
extern void display_monlist(void);
extern QString monster_desc(monster_type *m_ptr, int mode);
extern QString monster_desc_race(int r_idx);
extern void lore_probe_monster_aux(int r_idx);
extern void lore_do_probe_monster(int m_idx);
extern void lore_treasure(int m_idx, int num_item, int num_gold);
extern void update_mon(int m_idx, bool full);
extern void update_monsters(bool full);
extern s16b monster_carry(int m_idx, object_type *j_ptr);
extern void monster_swap(int y1, int x1, int y2, int x2);
extern bool player_place(int y, int x);
extern void monster_hide(monster_type *m_ptr);
extern void monster_unhide(monster_type *m_ptr);
extern s16b monster_place(int y, int x, monster_type *n_ptr);
extern void calc_monster_speed(int y, int x);
extern void reveal_mimic(int o_idx, bool message);
extern bool place_monster_aux(int y, int x, int r_idx, byte mp_flags);
extern bool place_monster(int y, int x, byte mp_flags);
extern bool alloc_monster(int dis, byte mp_flags);
extern bool summon_specific(int y1, int x1, int lev, int type, byte mp_flags);
extern bool multiply_monster(int m_idx, bool override);
extern void message_pain(int m_idx, int dam);
extern bool add_monster_message(QString mon_name, int m_idx, int msg_code);
extern void flush_monster_messages(void);
extern void update_smart_learn(int m_idx, int what);
extern bool monster_nonliving(monster_race *r_ptr);
extern bool race_breathes_element(monster_race *r_ptr, int gf_type);
extern bool race_similar_breaths(monster_race *r_ptr, monster_race *r2_ptr);
extern bool race_similar_monsters(int m_idx, int m2y, int m2x);

// object_all_menu.cpp
extern void do_cmd_all_objects(int start_screen);

/* object_desc.c */
extern QString strip_name(int k_idx);
extern QString object_desc(object_type *o_ptr, byte mode);
extern QString object_desc_spoil(object_type *o_ptr);
extern QString object_desc(int k_idx, byte mode);
extern QString desc_ego_fake(int ego_num, QString object_string, bool display);


// object_hooks.cpp
extern bool item_tester_hook_wieldable_ided_weapon(object_type *o_ptr);
extern bool item_tester_hook_wieldable_weapon(object_type *o_ptr);
extern bool item_tester_hook_ided_weapon(object_type *o_ptr);
extern bool item_tester_hook_weapon(object_type *o_ptr);
extern bool item_tester_hook_ided_armour(object_type *o_ptr);
extern bool item_tester_hook_armour(object_type *o_ptr);
extern bool item_tester_hook_ided_ammo(object_type *o_ptr);
extern bool item_tester_hook_ammo(object_type *o_ptr);
extern bool item_tester_hook_recharge(object_type *o_ptr);
extern bool item_tester_hook_randart(object_type *o_ptr);
extern bool item_tester_hook_flammable_book(object_type *o_ptr);
extern bool item_tester_hook_activate(object_type *o_ptr);
extern bool item_tester_hook_not_artifact(object_type *o_ptr);
extern bool obj_can_browse(object_type *o_ptr);

// object_info.cpp
extern QString output_desc_list(QString intro, QVector<QString> list, bool use_or, bool end_punctuation);
extern QString screen_out_head(object_type *o_ptr);
extern QString object_info_out(object_type *o_ptr, bool extra_info, bool is_real);
extern QString get_object_description(object_type *o_ptr);
extern void object_info_screen(object_type *o_ptr);
extern QString format_object_history(object_type *o_ptr);
extern bool history_interesting(object_type *o_ptr);
extern QString identify_random_gen(object_type *o_ptr);
extern void desc_art_fake(int a_idx);

/* object-make.cpp */
extern s16b get_obj_num(int level);
extern void object_prep(object_type *o_ptr, int k_idx);
extern s16b charge_wand(object_type *o_ptr, int percent);
extern s16b charge_staff(object_type *o_ptr, int percent);
extern void a_m_aux_1(object_type *o_ptr, int level, int power);
extern void a_m_aux_2(object_type *o_ptr, int level, int power);
extern void object_into_artifact(object_type *o_ptr, artifact_type *a_ptr);
extern void apply_ego_item_magic(object_type *o_ptr, int lev);
extern void apply_magic(object_type *o_ptr, int lev, bool okay, bool good, bool great, bool interesting);
extern void object_quantities(object_type *j_ptr);
extern bool make_object(object_type *j_ptr, bool good, bool great, int objecttype, bool interesting);
extern bool prep_store_object(int storetype);
extern bool prep_object_theme(int themetype);
extern int get_object_mimic_k_idx(const monster_race *r_ptr);
extern bool make_fake_artifact(object_type *o_ptr, byte art_num);
extern void apply_magic_fake(object_type *o_ptr);
extern void make_object_fake(object_type *o_ptr, int k_idx, byte ego_num, bool update_tracking);
extern bool make_gold(object_type *j_ptr);
extern void place_object(int y, int x, bool good, bool great, int droptype);
extern bool place_quest_artifact(int y, int x);
extern void place_gold(int y, int x);
extern int  get_coin_type(const monster_race *r_ptr);
extern void acquirement(int y1, int x1, int num, bool great);
extern void create_food(void);

// object_select.cpp
extern bool get_item(int *cp, QString pmt, QString str, int mode);
extern bool get_item_beside(int *cp, QString pmt, QString str, int sq_y, int sq_x);


// object_use.cpp
extern bool player_can_read(void);
extern bool find_object_in_use(int *item);
extern void command_use(cmd_arg args);

//obj_util.cpp
extern QChar index_to_label(int i);
extern s16b wield_slot_ammo(object_type *o_ptr);
extern s16b wield_slot(object_type *o_ptr);
extern void wield_in_quiver(object_type *o_ptr, int slot);
extern void wield_item(object_type *o_ptr, int item, int slot);
extern bool slot_can_wield_item(int slot, object_type *o_ptr);
extern QString mention_use(int i);
extern QString describe_use(int i);
extern bool item_tester_okay(object_type *o_ptr, int obj_num);
extern int scan_floor(int *items, int size, int y, int x, int mode);
extern void excise_object_idx(int o_idx);
extern void delete_object_idx(int o_idx);
extern void delete_object(int y, int x);
extern void compact_objects(int size);
extern void wipe_o_list(void);
extern s16b o_pop(void);
extern int count_floor_items(int y, int x, bool pickup_only);
extern object_type* get_first_object(int y, int x);
extern object_type* get_next_object(object_type *o_ptr);
extern int get_obj_num_prep(void);
extern bool is_blessed(object_type *o_ptr);
extern s32b object_value(object_type *o_ptr);
extern bool object_similar(object_type *o_ptr, object_type *j_ptr);
extern void object_absorb(object_type *o_ptr, object_type *j_ptr);
extern void object_copy_amt(object_type *dst, object_type *src, int amt);
extern s16b floor_carry(int y, int x, object_type *j_ptr);
extern bool drop_near(object_type *j_ptr, int chance, int y, int x);
extern void inven_item_describe(int item);
extern void inven_item_increase(int item, int num);
extern int get_tag_num(int o_idx, byte *tag_num);
extern int quiver_space_per_unit(const object_type *o_ptr);
extern void save_quiver_size(void);
extern int compare_ammo(int slot1, int slot2);
extern byte quiver_get_group(object_type *o_ptr);
extern int sort_quiver(int slot);
extern void open_quiver_slot(int slot);
extern bool quiver_carry_okay(object_type *o_ptr, int num, int item);
extern void inven_item_optimize(int item);
extern void floor_item_describe(int item);
extern void floor_item_increase(int item, int num);
extern void floor_item_optimize(int item);
extern bool inven_carry_okay(object_type *o_ptr);
extern bool quiver_stack_okay(object_type *o_ptr);
extern bool inven_stack_okay(object_type *o_ptr, int set_limit);
extern s16b quiver_carry(object_type *o_ptr);
extern s16b inven_carry(object_type *o_ptr);
extern s16b inven_takeoff(int item, int amt);
extern void inven_drop(int item, int amt);
extern void combine_pack(void);
extern void combine_quiver(void);
extern void reorder_pack(void);
extern int get_use_device_chance(object_type *o_ptr);
extern void distribute_charges(object_type *o_ptr, object_type *i_ptr, int amt);
extern void reduce_charges(object_type *o_ptr, int amt);
extern s16b lookup_kind(int tval, int sval);
extern s16b lookup_ego(s16b tval, s16b sval, QString ego_title);
extern void display_object_idx_recall(s16b o_idx);
extern bool obj_can_refill(object_type *o_ptr);
extern bool obj_is_chest(object_type *o_ptr);
extern bool chest_requires_disarming(object_type *o_ptr);
extern bool ammo_can_fire(object_type *o_ptr, int item);
extern bool has_correct_ammo(void);
extern bool obj_has_charges(object_type *o_ptr);
extern object_type *object_from_item_idx(int item);
extern bool obj_aim_trap(object_type *o_ptr);
extern bool obj_needs_aim(object_kind *k_ptr);
extern bool obj_needs_aim(int k_idx);
extern bool obj_needs_aim(object_type *o_ptr);
extern bool obj_is_activatable(object_type *o_ptr);
extern bool obj_can_activate(object_type *o_ptr);
extern bool get_item_okay(int item);
extern int scan_items(int *item_list, size_t item_list_max, int mode);
extern bool item_is_available(int item, bool (*tester)(object_type *), int mode);
extern bool object_kind_is_available(int k_idx, int mode);
extern bool is_throwing_weapon(object_type *o_ptr);
extern bool pack_is_full(void);
extern bool pack_is_overfull(void);
extern void pack_overflow(void);
extern void object_history(object_type *o_ptr, byte origin, s16b r_idx);
extern void stack_histories(object_type *o_ptr, const object_type *j_ptr);
extern int quiver_space_per_unit(object_type *o_ptr);
extern bool obj_can_wear(object_type *o_ptr);


/* player_attack.cpp */
extern bool test_hit(int chance, int ac, int vis);
extern int rogue_shot(const object_type *o_ptr, int *plus, player_state shot_state);
extern int brigand_shot(const object_type *o_ptr, u32b mon_race_flag_r1, bool asleep, player_state shot_state);
extern bool check_hit(int power);
extern int critical_hit_chance(object_type *o_ptr, player_state a_state, bool id_only);
extern int critical_hit_check(object_type *o_ptr, int *dd, int *plus);
extern int critical_shot_chance(object_type *o_ptr, player_state a_state, bool is_throw, bool id_only, u32b f3);
extern void py_attack(int y, int x);
extern void do_cmd_fire();
extern void command_fire(cmd_arg args);
extern void command_fire_nearest(cmd_arg args);
extern void do_cmd_fire_at_nearest(void);
extern int weapon_throw_adjust(object_type *o_ptr, u32b f3, int *plus, bool id_only);
extern void command_throw(cmd_arg args);
extern void do_cmd_throw(void);

//player_death.cpp
extern void player_death(void);

//player_ghost.cpp
extern void prepare_ghost_name(void);
extern bool prepare_ghost(int r_idx);
extern void ghost_challenge(bool color);
extern void remove_player_ghost(void);
extern void delete_player_ghost_entry(void);
extern void add_player_ghost_entry(void);
extern void load_player_ghost_file(void);
extern void save_player_ghost_file(void);

// player_info.cpp


// player_process
extern void process_player_terrain_damage(void);
extern void process_player(void);


// player_spell.cpp
extern bool spell_needs_aim(int tval, int spell);
extern bool spell_needs_object(int tval, int spell);
extern QString do_mage_spell(int mode, int spell, int dir);
extern QString do_druid_incantation(int mode, int spell, int dir);
extern QString do_priest_prayer(int mode, int spell, int dir);
extern QString cast_spell(int mode, int tval, int index, int dir);
extern int get_player_spell_realm(void);
extern QString get_spell_name(int tval, int spell);


// player_util.cpp
extern void player_flags(u32b *f1, u32b *f2, u32b *f3, u32b *fn);
extern s16b modify_stat_value(int value, int amount);
extern bool allow_player_confusion(void);

// project.cpp
extern bool teleport_away(int m_idx, int dis);
extern bool teleport_player(int dis, bool native);
extern void teleport_player_to(int ny, int nx);
extern void teleport_towards(int oy, int ox, int ny, int nx);
extern bool teleport_player_level(int who);
extern int drain_charges(object_type *o_ptr, u32b heal);
extern void disease(int *damage);
extern bool apply_disenchant(int mode);
extern byte gf_color(int type);
extern void take_terrain_hit(int dam, int feat, QString kb_str);
extern void take_hit(int dam, QString kb_str);
extern bool object_hates_feature(int feat, object_type *o_ptr);
extern bool object_hates_location(int y, int x, object_type *o_ptr);
extern void acid_dam(int dam, QString kb_str);
extern void elec_dam(int dam, QString kb_str);
extern void fire_dam(int dam, QString kb_str);
extern void cold_dam(int dam, QString kb_str);
extern void disease(int *damage);
extern bool apply_disenchant(int mode);
extern bool project_m(int who, int y, int x, int dam, int typ, u32b flg);
extern bool project_p(int who, int y, int x, int dam, int typ, QString msg);
extern bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
             u32b flg, int degrees, byte source_diameter);

// project_util.cpp
extern bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg);
extern bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg);
extern bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg, int source_diameter);
extern bool explosion(int who, int rad, int y0, int x0, int dam, int typ, u32b flg);
extern bool mon_explode(int who, int rad, int y0, int x0, int dam, int typ);
extern bool project_arc(int who, int rad, int y0, int x0, int y1, int x1,
    int dam, int typ, u32b flg, int degrees);
extern bool project_star(int who, int rad, int y0, int x0, int dam, int typ,
    u32b flg);
extern bool project_los(int y0, int x0, int dam, int typ);
extern void clear_project_grid_array(void);
extern void project_grid_mark(int y, int x, bool room);
extern void spread_project_grid_mark(int y1, int x1, int range, bool room, bool pass_walls);
extern bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg);
extern bool fire_effect_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg, int source_diameter);
extern bool fire_arc_special(int typ, int dir, int dam, int rad, int degrees, u32b flg);
extern bool fire_arc(int typ, int dir, int dam, int rad, int degrees);
extern bool fire_star(int typ, int dam, int rad, u32b flg);
extern bool beam_burst(int y, int x, int typ, int num, int dam);
extern bool fire_swarm(int num, int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam, u32b flg);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool beam_chain(int gf_type, int dam, int max_hits, int decrement);


/* quest.c */
extern QString plural_aux(QString name);
extern int quest_collection_num(quest_type *q_ptr);
extern QString describe_quest(s16b level);
extern void do_cmd_quest_desc(void);
extern void add_reward_gold(void);
extern QString get_title(void);
extern QString get_rep_guild(void);
extern QString get_welcome_guild(void);
extern void grant_reward_hp(void);
extern bool quest_allowed(byte j);
extern bool can_quest_at_level(void);
extern void quest_finished(quest_type *q_ptr);
extern bool guild_purchase(int choice);
extern byte quest_check(int lev);
extern int quest_num(int lev);
extern int quest_item_slot(void);
extern int quest_item_count(void);
extern s32b quest_time_remaining(void);
extern s32b quest_player_turns_remaining(void);
extern void guild_quest_wipe(bool reset_defer);
extern void write_quest_note(bool success);
extern void quest_fail(void);
extern QString format_quest_indicator(byte *attr);
extern void quest_status_update(void);
extern bool quest_fixed(const quest_type *q_ptr);
extern bool quest_slot_fixed(int quest_num);
extern bool quest_multiple_r_idx(const quest_type *q_ptr);
extern bool quest_slot_multiple_r_idx(int quest_num);
extern bool quest_single_r_idx(const quest_type *q_ptr);
extern bool quest_slot_single_r_idx(int quest_num);
extern bool quest_themed(const quest_type *q_ptr);
extern bool quest_type_collection(const quest_type *q_ptr);
extern bool quest_slot_collection(int quest_num);
extern bool quest_slot_themed(int quest_num);
extern bool quest_timed(const quest_type *q_ptr);
extern bool quest_slot_timed(int quest_num);
extern bool quest_no_down_stairs(const quest_type *q_ptr);
extern bool no_down_stairs(s16b check_depth);
extern bool quest_shall_fail_if_leave_level(void);
extern bool quest_might_fail_if_leave_level(void);
extern bool quest_fail_immediately(void);
extern bool quest_might_fail_now(void);

// quest_process.cpp
extern void process_arena_quest(void);
extern void process_labyrinth_quest(void);
extern void process_wilderness_quest(void);
extern void process_greater_vault_quest(void);
extern void process_guild_quests(void);

// qt_mainwindow.cpp




extern QPixmap pseudo_ascii(QChar chr, QColor color, QFont font, QSizeF size);



// qt_ui_functions.cpp
extern QRect visible_dungeon();
extern bool panel_contains(int y, int x);
extern UserInput ui_get_input();
extern void ui_targeting_show(int mode);
extern void ui_targeting_hide(void);
extern void ui_activate_main_window();
extern bool ui_draw_path(u16b path_n, u16b *path_g, int cur_tar_y, int cur_tar_x);
extern void ui_destroy_path();
extern void ui_show_cursor(int y, int x);
extern QSize ui_max_widget_size(QSize this_size);
extern void ui_animate_arc(int y0, int x0, int y1, int x1, int type, int radius, int degrees, u32b flg);
extern void ui_animate_bolt(int y0, int x0, int y1, int x1, int type, u32b flg);
extern void ui_animate_throw(int y0, int x0, int y1, int x1, object_type *o_ptr);
extern void ui_animate_beam(int y0, int x0, int y1, int x1, int type);
extern void ui_animate_ball(int y, int x, int radius, int type, u32b flg);
extern void ui_animate_star(int y, int x, int radius, int type, int gy[], int gx[], int grids);
extern void ui_animate_detection(int y, int x, int rad);
class TileBag;
extern QPixmap ui_get_tile(QString tile_id, bool allow_double_height);
extern void ui_request_size_update(QWidget *widget);
extern void ui_resize_to_contents(QWidget *widget);
class QTableWidget;
extern QSize ui_estimate_table_size(QTableWidget *table, bool horiz = true, bool vert = true,
                                    int padding = 30);
extern QFont ui_main_window_font();
extern QFont ui_message_window_font();
extern QFont ui_sidebar_window_font();
extern void ui_animate_accomplishment(int y, int x, int gf_type);
extern QPixmap ui_make_blank();
extern void ui_animate_victory(int y, int x);
extern int ui_get_dir_from_slope(int y1, int x1, int y2, int x2);
extern QPoint ui_get_center(int y, int x);
extern void ui_update_sidebar_all();
extern void ui_update_sidebar_player();
extern void ui_update_sidebar_mon();
extern QString ui_return_sidebar_text(bool label, int row);
extern void ui_update_statusbar();
extern void ui_update_titlebar();
extern void ui_update_messages();
extern void ui_update_monlist();
extern void ui_update_objlist();
extern void ui_update_mon_recall();
extern void ui_update_obj_recall();
extern void ui_update_feat_recall();
extern void ui_update_message_window();
extern void ui_update_char_basic_window();
extern void ui_update_char_equip_info_window();
extern void ui_update_char_equipment_window();
extern void ui_update_char_inventory_window();
extern void ui_update_char_score();
extern void ui_update_char_turncount();
extern void player_death_close_game(void);
extern bool ui_use_25d_graphics(void);
extern bool ui_using_tiles(void);
extern bool ui_using_monster_tiles(void);
extern void ui_redraw_grid(int y, int x);
extern void ui_handle_grid_wheelevent(bool wheelscroll_increase);
extern void ui_redraw_all();
extern void ui_ensure(int y, int x);
extern bool ui_modify_panel(int y, int x);
extern bool ui_adjust_panel(int y, int x);
extern bool ui_change_panel(int dir);
extern void ui_center(int y, int x);
extern void ui_player_moved();
extern QSize ui_grid_size();
extern void ui_png_screenshot(void);
extern void ui_update_message_label(QString message);
extern void ui_clear_message_label(void);
extern void ui_update_hotkey_toolbar();
extern byte ui_get_sound_volume();


/* randart.c */
extern QString make_random_name(byte min_length, byte max_length);
extern s32b artifact_power(int a_idx);
extern void build_randart_tables(void);
extern void free_randart_tables(void);
extern int do_randart(u32b randart_seed, bool full);
extern bool make_one_randart(object_type *o_ptr, int art_power, bool namechoice);
extern void make_quest_artifact(int lev);
extern void create_quest_artifact(object_type *o_ptr);
extern void artifact_wipe(int a_idx, bool quest_art_wipe);
extern bool can_be_randart(const object_type *o_ptr);

/* save.c */
extern bool save_player(void);
extern void do_hotkey_export(QString file_name);

// spells_detect.cpp
extern bool detect(int dist, u16b detect_checks);

/* spells_misc.c */
extern bool hp_player(int num);
extern bool warding_glyph(void);
extern bool create_elements(int cy, int cx, int range);
extern bool create_glacier(void);
extern bool inc_stat(int stat);
extern bool dec_stat(int stat, int amount, bool permanent);
extern bool res_stat(int stat);
extern bool do_dec_stat(int stat);
extern bool do_res_stat(int stat);
extern bool do_inc_stat(int stat);
extern void do_perm_stat_boost(int stat);
extern void identify_pack(void);
extern void uncurse_object(object_type *o_ptr);
extern bool remove_curse(bool heavy);
extern bool remove_all_curse(void);
extern bool restore_level(void);
extern void self_knowledge(void);
extern bool set_recall(void);
extern bool detect(int dist, u16b detect_checks);
extern void stair_creation(void);
extern bool enchant(object_type *o_ptr, int n, int eflag);
extern bool enchant_spell(int num_hit, int num_dam, int num_ac);
extern bool ident_spell(void);
extern bool identify_fully(void);
extern void recharge_staff_wand(object_type *o_ptr, int percent);
extern bool recharge(int num, bool cannot_fail, int percent);
extern bool speed_monsters(void);
extern bool slow_monsters(int power);
extern bool sleep_monsters(int power);
extern bool banish_evil(int dist);
extern bool turn_undead(int power);
extern bool dispel_undead(int dam);
extern bool dispel_evil(int dam);
extern bool dispel_monsters(int dam);
extern bool mass_polymorph(void);
extern bool fire_player_los(int type, int dam);
extern void aggravate_monsters(int who);
extern void mass_aggravate_monsters(int who);
extern bool banishment(void);
extern bool mass_banishment(void);
extern bool probing(void);
extern void destroy_area(int y1, int x1, int r);
extern void earthquake(int cy, int cx, int r, bool kill_vault);
extern void light_room(int y1, int x1);
extern void unlight_room(int y1, int x1);
extern bool light_area(int dam, int rad);
extern bool unlight_area(int dam, int rad);
extern QString get_spell_type_from_feature(int f_idx, int *gf_type);
extern bool is_player_immune(int gf_type);
extern void identify_object(object_type *o_ptr, bool star_ident);
extern int do_ident_item(int item, object_type *o_ptr);
extern bool curse_armor(void);
extern bool curse_weapon(void);
extern bool brand_object(object_type *o_ptr, byte brand_type, bool do_enchant);
extern bool brand_weapon(bool enchant);
extern bool brand_ammo(bool enchant);
extern bool brand_bolts(bool enchant);
extern bool light_line(int dir, int dam);
extern bool strong_light_line(int dir);
extern bool drain_life(int dir, int dam);
extern bool build_wall(int dir, int dam);
extern bool wall_to_mud(int dir, int dam);
extern bool destroy_door(int dir);
extern bool disarm_trap(int dir);
extern bool heal_monster(int dir, int dam);
extern bool speed_monster(int dir);
extern bool slow_monster(int dir);
extern bool sleep_monster(int dir);
extern bool confuse_monster(int dir, int plev);
extern bool poly_monster(int dir);
extern bool clone_monster(int dir);
extern bool fear_monster(int dir, int plev);
extern bool teleport_monster(int dir);
extern bool door_creation(void);
extern bool trap_creation(int who);
extern bool destroy_doors_touch(void);
extern bool sleep_monsters_touch(void);
extern void ring_of_power(int dir);
extern void identify_and_squelch_pack(void);
extern bool mass_identify(int rad);
extern bool read_minds(void);
extern bool master_elements(int dam, int dir);
extern bool steal_powers(int dir);
extern bool call_huorns(void);

//spoilers.cpp
extern void print_monster_spoiler_file(void);
extern void print_terrain_spoiler_file(void);
extern void print_object_spoiler_file(void);
extern void print_ego_item_spoiler_file(void);
extern void print_artifact_spoiler_file(void);

//squelch.cpp
extern void apply_autoinscription(object_type *o_ptr);
extern void autoinscribe_ground(void);
extern void autoinscribe_pack(void);
extern QString squelch_to_label(int squelch);
extern byte squelch_type_of(object_type *o_ptr);
extern QString quality_squelch_type_label(object_type *o_ptr);
extern QString get_ego_name(ego_item_type *e_ptr);
extern QString get_ego_name(object_type *o_ptr);
extern int squelch_itemp(object_type *o_ptr, byte feeling, bool fullid);
extern int do_squelch_item(int squelch, int item, object_type *o_ptr);
extern void rearrange_stack(int y, int x);
extern bool squelch_item_ok(object_type *o_ptr);
extern void do_squelch_pile(int y, int x);


//squelch_dialogs.cpp
extern void do_object_squelch_menu(void);
extern void do_ego_item_squelch_menu(void);
extern void do_quality_squelch_menu(void);

/*store.c*/
extern QString store_welcome(int store_idx);
extern s16b moria_chr_adj(void);
extern s32b price_item(int this_store, object_type *o_ptr, bool store_buying);
extern void store_item_increase(int st, int item, int num);
extern void store_item_optimize(int st, int item);
extern void store_delete_index(int st, int what);
extern void store_shuffle(int which);
extern void do_cmd_buy(int this_store, cmd_arg args);
extern void do_cmd_reward(int this_store, cmd_arg args);
extern void do_cmd_retrieve(int this_store, cmd_arg args);
extern void do_cmd_sell(int this_store, cmd_arg args);
extern void do_cmd_stash(int this_store, cmd_arg args);
extern void store_maint(int which);
extern void store_init(int which);
extern bool store_will_buy(int store_num, object_type *o_ptr);
extern bool store_overflow(int this_store);



// sound.cpp
extern void sound(int val);
extern void bell(QString this_message);


//tables.cpp
extern const byte moria_class_level_adj[MORIA_MAX_CLASS][MORIA_MAX_LEV_ADJ];
extern const byte moria_blows_table[MORIA_MAX_STR_ADJ][MORIA_MAX_DEX_ADJ];
extern const s16b ddd[9];
extern const s16b ddx[10];
extern const s16b ddy[10];
extern const s16b ddx_ddd[9];
extern const s16b ddy_ddd[9];
extern const int adj_mag_study[STAT_TABLE_SIZE];
extern const int adj_mag_mana[STAT_TABLE_SIZE];
extern const byte adj_mag_fail[STAT_TABLE_SIZE];
extern const int adj_mag_stat[STAT_TABLE_SIZE];
extern const byte adj_chr_gold[STAT_TABLE_SIZE];
extern const s16b adj_chr_charm[STAT_TABLE_SIZE];
extern const byte adj_int_dev[STAT_TABLE_SIZE];
extern const byte adj_wis_sav[STAT_TABLE_SIZE];
extern const byte adj_dex_dis[STAT_TABLE_SIZE];
extern const byte adj_int_dis[STAT_TABLE_SIZE];
extern const byte adj_dex_ta[STAT_TABLE_SIZE];
extern const byte adj_str_td[STAT_TABLE_SIZE];
extern const byte adj_dex_th[STAT_TABLE_SIZE];
extern const byte adj_str_th[STAT_TABLE_SIZE];
extern const byte adj_str_wgt[STAT_TABLE_SIZE];
extern const byte adj_str_hold[STAT_TABLE_SIZE];
extern const byte adj_str_dig[STAT_TABLE_SIZE];
extern const byte adj_str_blow[STAT_TABLE_SIZE];
extern const byte adj_dex_blow[STAT_TABLE_SIZE];
extern const byte adj_dex_safe[STAT_TABLE_SIZE];
extern const byte adj_con_fix[STAT_TABLE_SIZE];
extern const int adj_con_mhp[STAT_TABLE_SIZE];
extern const byte blows_table[12][12];
extern const byte extract_energy_nppmoria[6];
extern const byte extract_energy_nppangband[NPPANGBAND_SPEED_TABLE];
extern const s32b player_exp_nppangband[PY_MAX_LEVEL];
extern const s32b player_exp_nppmoria[PY_MAX_LEVEL_MORIA];
extern const player_sex sex_info[MAX_SEXES];
extern const s16b spell_list_nppmoria_mage[BOOKS_PER_REALM_MORIA][SPELLS_PER_BOOK];
extern const s16b spell_list_nppmoria_priest[BOOKS_PER_REALM_MORIA][SPELLS_PER_BOOK];
extern const s16b spell_list_nppangband_mage[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK];
extern const s16b spell_list_nppangband_priest[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK];
extern const s16b spell_list_nppangband_druid[BOOKS_PER_REALM_ANGBAND][SPELLS_PER_BOOK];
extern QString feeling_themed_level[LEV_THEME_TAIL];
extern const byte chest_traps[64];
extern QString color_names[16];
extern QString stat_names[A_MAX];
extern QString stat_names_reduced[A_MAX];
extern QString stat_names_full[A_MAX];
extern QString window_flag_desc[32];
extern option_entry options[OPT_MAX];
extern QString inscrip_text[MAX_INSCRIP];
extern byte spell_info_RF4[32][5];
extern byte spell_info_RF5[32][5];
extern byte spell_info_RF6[32][5];
extern byte spell_info_RF7[32][5];
extern byte spell_desire_RF4[32][8];
extern byte spell_desire_RF5[32][8];
extern byte spell_desire_RF6[32][8];
extern byte spell_desire_RF7[32][8];
extern const byte get_angle_to_grid[41][41];
extern const byte arena_level_map[ARENA_LEVEL_HGT][ARENA_LEVEL_WID];
extern const byte pit_room_maps[MAX_PIT_PATTERNS][PIT_HEIGHT][PIT_WIDTH];
extern const slays_structure slays_info_nppangband[11];
extern const brands_structure brands_info_nppangband[10];
extern const slays_structure slays_info_nppmoria[4];
extern const slays_structure brands_info_nppmoria[4];
extern const mon_susceptibility_struct mon_suscept[4];
extern const colors_preset preset_colors[MAX_COLORS];

/* target.cpp */
extern bool target_able(int m_idx, bool probing);
extern bool monster_target_exists(void);
extern bool target_okay(void);
extern bool target_set_interactive(int mode, int x, int y);
extern void target_set_monster(int m_idx, bool probing);
extern void target_set_location(int y, int x);
extern int target_dir(UserInput input);
extern bool get_aim_dir(int *dp, bool target_trap);
extern bool target_set_closest(int mode);
extern bool get_rep_dir(int *dp);
extern bool confuse_dir(int *dp);

/* timed.c */
extern bool redundant_timed_event(int idx);
extern bool set_timed(int idx, int v, bool notify);
extern bool inc_timed(int idx, int v, bool notify);
extern bool dec_timed(int idx, int v, bool notify);
extern bool clear_timed(int idx, bool notify);
extern bool player_can_repeat(void);
extern bool set_stun(int v);
extern bool set_cut(int v);
extern bool set_food(int v);


// Utilities.cpp
extern void debug_rarities();
extern int letter_to_number (QChar let);
extern QChar number_to_letter (int num);
extern QString number_to_formatted_string(s32b number);
extern bool is_a_vowel(QChar single_letter);
extern bool begins_with_vowel(QString line);
extern QString capitalize_first(QString line);
extern QString format_stat(s16b value);
extern void pop_up_message_box(QString message, QMessageBox::Icon the_icon = QMessageBox::Information);
extern void pop_up_message_box(QString message, QPixmap into_icon);
extern bool get_check(QString question);
extern QString get_string(QString question, QString description, QString answer);
extern s16b get_quantity_slider(QString prompt, QString unit, int min, int max, int value);
extern s16b get_quantity(QString prompt, int max, int amt, bool allow_zero);
extern QColor add_preset_color(int which_color);
extern QString html_string_to_plain_text(QString text);
extern QString set_html_string_length(QString html_string, int length, bool prepend);
extern QString color_string(QString msg, byte which_color);
extern QString color_string(QString msg, QColor which_color);
extern QString color_char(QChar which_char, QColor which_color);
extern QString color_char(QChar which_char, byte color_num);
extern void cmd_enable_repeat(void);
extern void cmd_disable_repeat(void);
extern QString format_object_weight(object_type *o_ptr);
extern QString formatted_weight_string(s32b weight);
extern QString get_player_title(void);
extern QColor get_object_color(object_type *o_ptr);
extern QColor make_color_readable(QColor clr);
extern void display_info_window(byte mode, int index, QString info);
extern QString to_ascii(QString src);
extern bool strings_match(QString string1, QString string2);
extern QString cnv_stat(int val);
extern void write_note(QString note, s16b depth);
extern QString find_roman_numeral(QString full_name);
extern QString int_to_roman(int n);
extern int roman_to_int(QString roman);
extern void clear_layout(QLayout* layout);


// widard_mode.cpp

extern void do_cmd_wizard_mode(void);




#endif // FUNCTION_DECLARATIONS_H
