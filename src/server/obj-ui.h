/*
 * File: obj-ui.h
 * Purpose: Lists of objects and object pictures
 */

#ifndef OBJECT_UI_H
#define OBJECT_UI_H

extern byte object_kind_attr(struct player *p, const struct object_kind *kind);
extern char object_kind_char(struct player *p, const struct object_kind *kind);
extern byte object_attr(struct player *p, const struct object *obj);
extern char object_char(struct player *p, const struct object *obj);
extern void display_inven(struct player *p);
extern void display_equip(struct player *p);
extern void display_floor(struct player *p, struct chunk *c, struct object **floor_list,
    int floor_num);
extern void show_floor(struct player *p, int mode);
extern bool get_item(struct player *p, byte tester_hook);
extern void display_object_recall_interactive(struct player *p, const struct object *obj,
    char *header);

#endif /* OBJECT_UI_H */
