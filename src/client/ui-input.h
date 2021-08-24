/*
 * File: ui-input.h
 * Purpose: Some high-level UI functions, inkey()
 */

#ifndef INCLUDED_UI_INPUT_H
#define INCLUDED_UI_INPUT_H

/*
 * Holds a generic command - if cmd is set to other than CMD_NULL
 * it simply pushes that command to the game, otherwise the hook
 * function will be called.
 */
struct cmd_info
{
    const char *desc;
    keycode_t key[2];
    cmd_code cmd;
    void (*hook)(void);
    bool (*prereq)(void);
};

/*
 * A categorised list of all the command lists.
 */
struct command_list
{
    const char *name;
    struct cmd_info *list;
    size_t len;
};

#define SCAN_INSTANT    -1
#define SCAN_OFF        0

#define MAX_COMMAND_LIST 7

extern struct cmd_info cmd_magic[];
extern struct cmd_info cmd_item[];
extern struct cmd_info cmd_action[];
extern struct cmd_info cmd_item_manage[];
extern struct cmd_info cmd_info[];
extern struct cmd_info cmd_util[];
extern struct cmd_info cmd_hidden[];
extern struct command_list cmds_all[MAX_COMMAND_LIST];

typedef bool (*keypress_handler)(char *buf, size_t buflen, size_t *curs, size_t *len,
    struct keypress keypress, bool firsttime);

extern struct keypress *inkey_next;
extern char inkey_scan;
extern bool inkey_flag;
extern byte trap_indicator;
extern bool first_escape;
extern bool topline_icky;
extern int dis_dd;
extern int dis_ds;
extern int dis_to_mhit;
extern int dis_to_mdam;
extern int dis_to_shit;
extern int dis_to_sdam;

extern void flush(game_event_type type, game_event_data *data, void *user);
extern ui_event inkey_ex(void);
extern struct keypress inkey(void);
extern void prt_icky(const char *str, int row, int col);
extern void clear_from(int row);
extern bool askfor_aux_keypress(char *buf, size_t buflen, size_t *curs, size_t *len,
    struct keypress keypress, bool firsttime);
extern bool askfor_aux(char *buf, int len, keypress_handler keypress_h);
extern int askfor_ex(char *buf, int len, keypress_handler keypress_h, bool priv);
extern void textui_input_init(void);
extern bool (*get_file)(const char *suggested_name, char *path, size_t len);
extern void flush_now(void);
extern void flush_hack(void);
extern int target_dir(struct keypress ch);
extern void caveprt(cave_view_type* src, int len, s16b x, s16b y);
extern void cavestr(cave_view_type* dest, const char *src, byte attr, int max_col);
extern const char *extract_file_name(const char *s);
extern ui_event Net_loop(errr (*inkey_handler)(ui_event*, bool, bool),
    void (*callback_begin)(ui_event*), void (*callback_end)(bool), char scan_cutoff, bool inmap);
extern void turn_off_numlock(void);
extern ui_event textui_get_command(void);
extern bool textui_process_key(struct keypress kp, unsigned char *c);
extern void bell_message(game_event_type unused, game_event_data *data, void *user);

#endif /* INCLUDED_UI_INPUT_H */
