/*
 * File: win-term.h
 * Purpose: Windows terminal structure.
 */

#ifndef INCLUDED_WINTERM_H
#define INCLUDED_WINTERM_H

/*
 * Extra "term" data
 *
 * Note the use of "font_want" for the names of the font file requested by
 * the user, and the use of "font_file" for the currently active font file.
 *
 * The "font_file" is uppercased, and takes the form "8X13.FON", while
 * "font_want" can be in almost any form as long as it could be constructed
 * as attempting to represent the name of a font.
 */
typedef struct
{
    term t;
    const char *s;
    HWND w;
    DWORD dwStyle;
    DWORD dwExStyle;
    uint keys;
    byte rows;
    byte cols;
    uint pos_x;
    uint pos_y;
    uint size_wid;
    uint size_hgt;
    uint size_ow1;
    uint size_oh1;
    uint size_ow2;
    uint size_oh2;
    bool size_hack;
	bool xtra_hack;
    bool visible;
    bool maximized;
    bool bizarre;
    char *font_want;
    char *font_file;
    HFONT font_id;
    uint font_wid;
    uint font_hgt;
    uint tile_wid;
    uint tile_hgt;
    uint map_tile_wid;
    uint map_tile_hgt;
} term_data;

/*
 * Maximum number of windows XXX XXX XXX
 */
#define MAX_TERM_DATA 8

extern bool arg_graphics_nice;
extern int arg_graphics;

/*
 * Default window layout function
 */
extern int default_layout_win(term_data *data, int maxterms);

#endif /* INCLUDED_WINTERM_H */