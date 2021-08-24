/*
 * File: help-ui.h
 * Purpose: In-game help
 */

#ifndef HELP_UI_H
#define HELP_UI_H

extern bool show_file(struct player *p, const char *name, const char *what, int line, int color);
extern void common_file_peruse(struct player *p, u32b query);

#endif /* HELP_UI_H */
