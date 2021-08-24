/*
 * File: game-input.h
 * Purpose: Ask for non-command input from the UI.
 */

#ifndef INCLUDED_GAME_INPUT_H
#define INCLUDED_GAME_INPUT_H

/*
 * Bit flags for the "get_item" function
 */
#define USE_EQUIP       0x0001  /* Allow equip items */
#define USE_INVEN       0x0002  /* Allow inven items */
#define USE_FLOOR       0x0004  /* Allow floor items */
#define USE_QUIVER      0x0008  /* Allow quiver items */
#define SHOW_PRICES     0x0010  /* Show item prices in item lists */
#define SHOW_FAIL       0x0020  /* Show device failure in item lists */
#define SHOW_QUIVER     0x0040  /* Show quiver summary when looking at inventory */
#define SHOW_EMPTY      0x0080  /* Show empty slots in equipment display */
#define QUIVER_TAGS     0x0100  /* 0-7 are quiver slots when selecting */
#define BOOK_TAGS       0x0200	/* 0-9 are books when selecting */
#define START_EQUIP     0x0400  /* Force start in equipment mode */
#define START_INVEN     0x0800  /* Force start in inventory mode */
#define START_QUIVER    0x1000  /* Force start in quiver mode */

extern bool (*get_string_hook)(const char *prompt, char *buf, int len);
extern int (*get_string_ex_hook)(const char *prompt, char *buf, int len, bool priv);
extern s32b (*get_quantity_hook)(const char *prompt, s32b max);
extern s32b (*get_quantity_ex_hook)(const char *prompt, s32b max);
extern bool (*get_check_hook)(const char *prompt);
extern int (*get_check_ex_hook)(const char *prompt);
extern bool (*get_com_hook)(const char *prompt, struct keypress *command);
extern bool (*get_com_ex_hook)(const char *prompt, ui_event *command);
extern bool (*get_aim_dir_hook)(int *dir);
extern int (*get_aim_dir_ex_hook)(int *dir);
extern int (*get_spell_hook)(int book, const char *verb, bool (*spell_test)(int, int));
extern bool (*get_item_hook)(struct object **choice, const char *pmt, const char *str, cmd_code cmd,
    item_tester tester, int mode);
extern bool (*get_curse_hook)(int *choice, struct object *obj, char *dice_string);

extern bool get_string(const char *prompt, char *buf, int len);
extern int get_string_ex(const char *prompt, char *buf, int len, bool priv);
extern s32b get_quantity(const char *prompt, s32b max);
extern s32b get_quantity_ex(const char *prompt, s32b max);
extern bool get_check(const char *prompt);
extern int get_check_ex(const char *prompt);
extern bool get_com(const char *prompt, struct keypress *command);
extern bool get_com_ex(const char *prompt, ui_event *command);
extern bool get_aim_dir(int *dir);
extern int get_aim_dir_ex(int *dir);
extern int get_spell(int book, const char *verb, bool (*spell_filter)(int, int));
extern bool get_item(struct object **choice, const char *pmt, const char *str, cmd_code cmd,
    item_tester tester, int mode);
extern bool get_curse(int *choice, struct object *obj, char *dice_string);

#endif /* INCLUDED_GAME_INPUT_H */
