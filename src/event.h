#include "angband.h"
#include "Python.h"

#ifdef USE_SCRIPT
extern void new_game_callback(void);
extern void load_game_callback(char *data);
extern cptr save_game_callback(void);
extern void play_game_callback(void);

extern void eat_callback(int sval);
extern void kill_monster_callback(int m_idx);
extern bool player_move_callback(int y, int x);
extern bool cmd_open_callback(int y, int x);
extern bool cmd_search_callback(int y, int x);
extern bool cmd_feeling_callback(int feeling);
extern bool cmd_go_up_callback(void);
extern bool building_command_callback(int number, int action);

/* Dungeon grids */
extern bool player_enter_grid_callback(int y, int x);
extern bool player_search_grid_callback(int y, int x);

/* Dungeon levels */
extern bool generate_level_callback(int level);
extern void leave_level_callback(int level);
extern void enter_level_callback(int level);

/* Wilderness */
extern bool wilderness_init_callback(void);
extern bool generate_wilderness_callback(int y, int x);
extern bool enter_wilderness_callback(int y, int x);
extern bool leave_wilderness_callback(int y, int x);

extern void store_examine_callback(const object_type *o_ptr);
extern bool monster_move_callback(int *mm, int m_idx);
extern void create_monster_callback(int m_idx);
extern void delete_monster_callback(int m_idx);
extern void copy_monster_callback(int i1, int i2);
extern char inkey_borg_callback(bool inkey_base, bool inkey_xtra,
                                bool inkey_flag, bool inkey_scan);
extern char inkey_callback(char key);

/* Birth */
extern long player_birth_callback(void);

extern bool get_player_flags_callback(void);
extern bool player_outfit_callback(void);

extern long sense_inventory_callback(void);
extern bool destroy_object_callback(object_type *o_ptr, int number);

/* Object callbacks - global */
extern PyObject* object_create_callback(object_type *o_ptr);
extern PyObject* object_load_callback(char *code);

/* Object callbacks - object specific */
extern bool object_eat_callback(object_type *o_ptr);
extern bool object_browse_callback(const object_type *o_ptr);
extern bool object_cast_callback(object_type *o_ptr);
extern cptr object_save_callback(const object_type *o_ptr);
extern void object_delete_callback(object_type *o_ptr);
extern PyObject* object_copy_callback(object_type *o_ptr, const object_type *j_ptr);
extern long get_object_level_callback(const object_type *o_ptr);
extern long get_object_cost_callback(const object_type *o_ptr);
extern cptr get_object_name_callback(const object_type *o_ptr);
extern char get_object_d_char_callback(const object_type *o_ptr);
extern char get_object_x_char_callback(const object_type *o_ptr);
extern byte get_object_d_attr_callback(const object_type *o_ptr);
extern byte get_object_x_attr_callback(const object_type *o_ptr);
extern bool get_object_aware_callback(const object_type *o_ptr);
extern bool get_object_tried_callback(const object_type *o_ptr);

/* Object_kind callbacks */
extern bool free_object_kind_list_callback(void);
extern bool init_object_kind_list_callback(void);

/* Field callbacks */
extern void field_delete_callback(field_type *f_ptr);
extern PyObject* field_copy_callback(field_type *f_ptr, field_type *g_ptr);
extern PyObject* field_load_callback(char *code);
extern cptr field_save_callback(const field_type *f_ptr);

extern bool use_skill_callback(void);
extern bool process_command_callback(char command);

/* Script callbacks */
extern cptr get_script_window_line(int line);
#endif /* USE_SCRIPT */

#define NEW_GAME_EVENT                1
#define LOAD_GAME_EVENT               2
#define SAVE_GAME_EVENT               3
#define CMD_EAT_EVENT                 4
#define PLAYER_MOVE_EVENT             5
#define CMD_OPEN_EVENT                6
#define CMD_SEARCH_EVENT              7
#define PLAYER_SEARCH_GRID_EVENT      8
#define CMD_FEELING_EVENT             9
#define CMD_GO_UP_EVENT              10
#define KILL_MONSTER_EVENT           11
#define BUILDING_COMMAND_EVENT       12
#define LEAVE_LEVEL_EVENT            13
#define PLAYER_ENTER_GRID_EVENT      14
#define ENTER_LEVEL_EVENT            15
#define GENERATE_LEVEL_EVENT         16
#define GENERATE_WILDERNESS_EVENT    17
#define ENTER_WILDERNESS_EVENT       18
#define LEAVE_WILDERNESS_EVENT       19
#define STORE_EXAMINE_EVENT          20
#define MONSTER_MOVE_EVENT           21
#define CREATE_MONSTER_EVENT         22
#define DELETE_MONSTER_EVENT         23
#define INKEY_BORG_EVENT             24
#define INKEY_EVENT                  25
/* XXX */
#define GET_PLAYER_FLAGS_EVENT       27
#define SENSE_INVENTORY_EVENT        28
#define DESTROY_OBJECT_EVENT         29
/* XXX */
#define OBJECT_CREATE_EVENT          31
#define OBJECT_LOAD_EVENT            32
#define PLAYER_OUTFIT_EVENT          33
#define WILDERNESS_INIT_EVENT        34
#define FREE_OBJECT_KIND_LIST_EVENT  35
#define INIT_OBJECT_KIND_LIST_EVENT  36
/* XXX */
#define COPY_MONSTER_EVENT           39
#define USE_SKILL_EVENT              40
#define GET_SCRIPT_WINDOW_LINE_EVENT 41
#define PLAY_GAME_EVENT              42
#define FIELD_LOAD_EVENT             43
#define PROCESS_COMMAND_EVENT        44
#define PLAYER_BIRTH_EVENT           45

#define MAX_EVENT                    46
