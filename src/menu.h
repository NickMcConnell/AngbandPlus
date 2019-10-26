#ifndef MENU_H
#define MENU_H

enum menu_e {
    MENU_KEY,
    MENU_TEXT,
    MENU_HELP,
    MENU_COLOR,
    MENU_ON_BROWSE,
};

typedef void (*menu_fn)(int cmd, int which, vptr cookie, var_ptr res);
extern void default_menu(int cmd, int which, vptr cookie, var_ptr res);

typedef struct {
    cptr    choose_prompt;
    cptr    browse_prompt;
    cptr    heading;
    menu_fn fn;
    vptr    cookie;
    int     count;
} menu_t, *menu_ptr;

extern int menu_choose(menu_ptr menu);

#endif
