/*
 * File: init.h
 * Purpose: Initialization
 */

#ifndef INCLUDED_INIT_H
#define INCLUDED_INIT_H

struct init_module
{
    const char *name;
    void (*init)(void);
    void (*cleanup)(void);
};

extern struct object_kind *unknown_item_kind;
extern struct object_kind *unknown_gold_kind;
extern struct object_kind *pile_kind;

extern bool cfg_report_to_meta;
extern char *cfg_meta_address;
extern char *cfg_bind_name;
extern char *cfg_report_address;
extern char *cfg_console_password;
extern char *cfg_dungeon_master;
extern bool cfg_secret_dungeon_master;
extern bool cfg_no_steal;
extern bool cfg_newbies_cannot_drop;
extern s32b cfg_level_unstatic_chance;
extern s32b cfg_retire_timer;
extern bool cfg_random_artifacts;
extern s16b cfg_limit_stairs;
extern bool cfg_no_recall;
extern bool cfg_no_ghost;
extern bool cfg_more_towns;
extern bool cfg_artifact_drop_shallow;
extern bool cfg_limit_player_connections;
extern s16b cfg_max_townies;
extern s16b cfg_max_trees;
extern s32b cfg_tcp_port;
extern bool cfg_chardump_color;
extern bool cfg_town_wall;
extern s16b cfg_pvp_hostility;
extern bool cfg_base_monsters;
extern bool cfg_extra_monsters;
extern bool cfg_ghost_diving;
extern bool cfg_console_local_only;
extern char *cfg_load_pref_file;
extern s16b cfg_preserve_artifacts;
extern bool cfg_safe_recharge;
extern s16b cfg_party_sharelevel;
extern bool cfg_turn_based;
extern bool cfg_limited_esp;
extern bool cfg_double_purse;
extern bool cfg_ai_learn;

extern void init_file_paths(const char *configpath, const char *libpath, const char *datapath);
extern void create_needed_dirs(void);
extern void init_angband(void);
extern void cleanup_angband(void);
extern void load_server_cfg(void);

#endif /* INCLUDED_INIT_H */
