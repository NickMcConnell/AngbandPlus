#include "angband.h"

extern void eat_callback(int sval);
extern void kill_monster_callback(int r_idx);
extern long player_move_callback(int y, int x);
extern long cmd_open_callback(int y, int x);
extern long cmd_search_callback(int y, int x);
extern long search_grid_callback(int y, int x);
extern long cmd_feeling_callback(int feeling);
extern long cmd_go_up_callback(void);
extern long building_command_callback(int number, int action);
extern void callbacks_load_callback(char *data);
extern cptr callbacks_save_callback(void);
extern long player_enter_grid_callback(int index);

/* Dungeon levels */
extern bool generate_level_callback(int level);
extern void leave_level_callback(int level);
extern void enter_level_callback(int level);

/* Wilderness */
extern bool generate_wilderness_callback(int y, int x);
extern bool enter_wilderness_callback(int y, int x);
extern bool leave_wilderness_callback(int y, int x);

extern void store_examine_callback(object_type *o_ptr);
extern bool monster_move_callback(int *mm, int m_idx);
extern void create_monster_callback(int m_idx);
extern void delete_monster_callback(int m_idx);
extern char inkey_borg_callback(bool inkey_base, bool inkey_xtra,
                                bool inkey_flag, bool inkey_scan);
extern char inkey_callback(char key);

extern long get_player_class_callback(void);
extern bool get_player_flags_callback(void);

extern long sense_inventory_callback(void);
bool destroy_object_callback(object_type *o_ptr, int number);


#define CMD_EAT_EVENT              1
#define PLAYER_MOVE_EVENT          2
#define CMD_OPEN_EVENT             3
#define CMD_SEARCH_EVENT           4
#define SEARCH_GRID_EVENT          5
#define CMD_FEELING_EVENT          6
#define CMD_GO_UP_EVENT            7
#define CALLBACKS_LOAD_EVENT       8
#define CALLBACKS_SAVE_EVENT       9
#define KILL_MONSTER_EVENT        10
#define BUILDING_COMMAND_EVENT    11
#define LEAVE_LEVEL_EVENT         12
#define PLAYER_ENTER_GRID_EVENT   13
#define ENTER_LEVEL_EVENT         14
#define GENERATE_LEVEL_EVENT      15
#define GENERATE_WILDERNESS_EVENT 16
#define ENTER_WILDERNESS_EVENT    17
#define LEAVE_WILDERNESS_EVENT    18
#define STORE_EXAMINE_EVENT       19
#define MONSTER_MOVE_EVENT        20
#define CREATE_MONSTER_EVENT      21
#define DELETE_MONSTER_EVENT      22
#define INKEY_BORG_EVENT          23
#define INKEY_EVENT               24
#define GET_PLAYER_CLASS_EVENT    25
#define GET_PLAYER_FLAGS_EVENT    26
#define SENSE_INVENTORY_EVENT     27
#define DESTROY_OBJECT_EVENT      28

#define MAX_EVENT                 29
