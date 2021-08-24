/*
 * File: ui-display.h
 * Purpose: Handles the setting up updating, and cleaning up of the game display.
 */

#ifndef INCLUDED_UI_DISPLAY_H
#define INCLUDED_UI_DISPLAY_H

extern s16b max_line;
extern s16b cur_line;

extern int health_amt;
extern byte health_attr;
extern u32b lag_mark;
extern s16b view_channel;
extern cave_view_type remote_info[ANGBAND_TERM_MAX][MAX_TXT_INFO][NORMAL_WID];
extern s16b last_remote_line[ANGBAND_TERM_MAX];

extern const char *window_flag_desc[PW_MAX_FLAGS];

extern void display_player_screen(bool mode);
extern void redraw_stuff(void);
extern int find_whisper_tab(const char *msg, char *text, size_t len);
extern void message_color_hack(const char *msg, byte *ap);
extern void subwindows_set_flags(u32b *new_flags, size_t n_subwindows);
extern void subwindows_init_flags(void);
extern void subwindows_reinit_flags(void);
extern void init_display(void);
extern void show_splashscreen(void);
extern bool peruse_file(void);

#endif /* INCLUDED_UI_DISPLAY_H */
