/*
 * File: prefs-ui.h
 * Purpose: Pref file handling code
 */

#ifndef PREFS_UI_H
#define PREFS_UI_H

extern byte *monster_x_attr;
extern char *monster_x_char;
extern byte *kind_x_attr;
extern char *kind_x_char;
extern byte (*feat_x_attr)[LIGHTING_MAX];
extern char (*feat_x_char)[LIGHTING_MAX];
extern byte (*trap_x_attr)[LIGHTING_MAX];
extern char (*trap_x_char)[LIGHTING_MAX];
extern byte *flavor_x_attr;
extern char *flavor_x_char;

extern bool process_pref_file(const char *name, bool quiet);
extern bool process_pref_file_xtra(const char *name);
extern void textui_prefs_init(void);
extern void textui_prefs_free(void);

#endif /* PREFS_UI_H */
