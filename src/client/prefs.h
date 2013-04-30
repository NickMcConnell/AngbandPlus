/*
 * File: prefs.h
 * Purpose: Pref file handling code
 */

#ifndef PREFS_H
#define PREFS_H

extern void option_dump(ang_file *fff);
extern void dump_colors(ang_file *fff);
extern bool prefs_save(const char *path, void (*dump)(ang_file *), const char *title);
extern errr process_pref_file_command(const char *s);
extern bool process_pref_file(const char *name, bool quiet, bool user);

#endif /* PREFS_H */
