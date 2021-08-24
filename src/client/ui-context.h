/*
 * File: ui-context.h
 * Purpose: Show player and terrain context menus
 */

#ifndef UI_CONTEXT_H
#define UI_CONTEXT_H

extern int context_menu_object(struct object *obj);
extern struct cmd_info *textui_action_menu_choose(void);
extern void free_command_menu(void);

#endif /* UI_CONTEXT_H */
