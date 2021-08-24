/*
 * File: ui-object.h
 * Purpose: Object lists and selection, and other object-related UI functions
 */

#ifndef UI_OBJECT_H
#define UI_OBJECT_H

extern struct object **floor_items;
extern byte floor_num;

extern void show_inven(int mode, item_tester tester);
extern void show_quiver(int mode, item_tester tester);
extern void show_equip(int mode, item_tester tester);
extern void show_floor(int mode, item_tester tester);

extern bool textui_get_item(struct object **choice, const char *pmt, const char *str, cmd_code cmd,
    item_tester tester, int mode);

extern void textui_cmd_ignore_menu(struct object *obj);
extern void textui_cmd_ignore(void);
extern void textui_cmd_toggle_ignore(void);

#endif /* UI_OBJECT_H */
