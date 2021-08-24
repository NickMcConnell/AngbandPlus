/*
 * File: map-ui.h
 * Purpose: Writing level map info to the screen
 */

#ifndef INCLUDED_MAP_UI_H
#define INCLUDED_MAP_UI_H

extern byte get_color(byte a, int attr, int n);
extern void grid_data_as_text(struct player *p, struct chunk *cv, bool server,
    struct grid_data *g, u16b *ap, char *cp, u16b *tap, char *tcp);
extern void prt_map(struct player *p);
extern void display_map(struct player *p, bool subwindow);
extern void do_cmd_view_map(struct player *p);
extern void do_cmd_wild_map(struct player *p);

#endif /* INCLUDED_MAP_UI_H */
