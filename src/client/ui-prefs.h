/*
 * File: ui-prefs.h
 * Purpose: Pref file handling code
 */

#ifndef UI_PREFS_H
#define UI_PREFS_H

/*
 * Private data for pref file parsing.
 */
struct prefs_data
{
    bool bypass;
    struct keypress keymap_buffer[KEYMAP_ACTION_MAX];
    bool user;
    bool loaded_window_flag[ANGBAND_TERM_MAX];
    u32b window_flags[ANGBAND_TERM_MAX];
};

extern int use_graphics;

extern void dump_colors(ang_file *fff);
extern void option_dump(ang_file *f);
extern void window_dump(ang_file *f);
extern bool prefs_save(const char *path, void (*dump)(ang_file *), const char *title);
extern errr process_pref_file_command(const char *s);
extern bool process_pref_file(const char *name, bool quiet, bool user);
extern void process_pref_options(void);
extern void reset_visuals(bool load_prefs);
extern void do_cmd_pref(void);

#endif /* UI_PREFS_H */
