/*
 * File: target-ui.h
 * Purpose: UI for targeting code
 */

#ifndef TARGET_UI_H
#define TARGET_UI_H

extern void target_display_help(char *help, size_t len, bool monster, bool free);
extern int draw_path(struct player *p, u16b path_n, struct loc *path_g, struct loc *grid);
extern void load_path(struct player *p, u16b path_n, struct loc *path_g);
extern bool target_set_interactive(struct player *p, int mode, u32b query);

#endif /* TARGET_UI_H */
