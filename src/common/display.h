/*
 * File: display.h
 * Purpose: Display the character on the screen or in a file
 */

#ifndef INCLUDED_DISPLAY_H
#define INCLUDED_DISPLAY_H

/*** Display hooks ***/
extern errr (*clear_hook)(void);
extern void (*region_erase_hook)(const region *loc);
extern errr (*put_ch_hook)(int x, int y, byte a, char c);
extern errr (*put_str_hook)(int x, int y, int n, byte a, const char *s);
extern bool use_bigtile_hook;

/*** Buffer access functions ***/
extern errr buffer_clear(void);
extern errr buffer_put_ch(int x, int y, byte a, char c);
extern errr buffer_put_str(int x, int y, int n, byte a, const char *s);
extern char *buffer_line(int row);

/*** Utility display functions ***/
extern void display_player(int Ind, bool mode);

/*** Status line display functions ***/
extern size_t display_depth(int Ind, int row, int col);
extern void display_statusline(int Ind, int row, int col);
extern void display_status_subwindow(int Ind, int row, int col);

#endif /* INCLUDED_DISPLAY_H */
