/* File: intrface.h */

/*
 * Copyright (c) 2007 Leon Marrick
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License as published by the Free
 * Software Foundation, version 2.  Parts may also be available under the
 * terms of the Moria license.  For more details, see "/docs/copying.txt".
 */

#include "angband.h"


/*
 * DEFINES AND ENUMS
 */

/*
 * The new Windows interface, in order to improve appearence, user-friendliness,
 * and design consistency, restricts the user's ability to hand-craft his own
 * displays (in some ways; in others it expands the possibilities).  This trade-
 * off will not make everyone happy.  So we try to preserve flexibility where
 * practicable.  -LM-
 *
 * Possible display modes:  Full-screen or windowed, emphasize map
 * display or emphasize sub-windows.  Mode #0 is the default.
 *
 * The user can re-define the meaning of "emphasize map" and "emphasize
 * sub-windows", but "full-screen" and "windowed" always mean the same things.
 */
#define INTERFACE_SCREEN_EMPHA_MAP    0
#define INTERFACE_SCREEN_EMPHA_SUB    1
#define INTERFACE_WINDOW_EMPHA_MAP    2
#define INTERFACE_WINDOW_EMPHA_SUB    3


/* Number of default window definitions */
#define NUM_WINDOW_DEFAULTS    91


/* The default game font */
#define DEFAULT_GAME_FONT    "8x12x.fon"


/* Multimedia quality levels */
#define QUALITY_LOW     1
#define QUALITY_MEDIUM  2
#define QUALITY_HIGH    3


/*
 * A timer for flickering stuff on screen every so often.
 */
#define TIMER_BASIC   1


/*
 * TYPES
 */

typedef struct srect srect;
typedef struct font_record_type font_record_type;
typedef struct window_type window_type;
typedef struct file_section_type file_section_type;


/*
 * A RECT structure with signed values.  Avoids wrap-around errors.
 */
struct srect
{
	int left;
	int top;
	int right;
	int bottom;
};

/*
 * Listing of available fonts.
 */
struct font_record_type
{
	int hgt;
	int wid;
	char name[64];
};

/*
 * Window data type.
 *
 * Each "window" corresponds to a internal game Term.  These windows could
 * each be (and in fact once were) placed into a separate system window.
 * The current implementation is to section off areas of a unified game
 * window (taking up the full screen by default) using information stored
 * here.  -LM-
 */
struct window_type
{
	s16b resolution_x;   /* Screen resolution for which this window applies */
	s16b resolution_y;

	byte mode;           /* Display mode in which this window definition is used */

	byte Term_idx;      /* Index of game Term that uses this window */

	byte rows;	         /* Displayable rows and columns */
	byte cols;

	int keys;           /* Size of keypress storage (1024 is typical for TERM_MAIN) */

	int window_left;    /* Left-top corner of the window area in client */
	int window_top;     /* coordinates (within application window border) */

	int window_wid;     /* Size of this term window (in pixels) */
	int window_hgt;

	int border_left;    /* Inner border width */
	int border_top;
	int border_right;
	int border_bottom;

	cptr font_want;      /* Requested font file */

	cptr font_file;      /* Currently active font file */

	void *font_id;      /* Pointer to a font structure (system dependent) */

	int tile_wid;       /* Size of a tile in this window */
	int tile_hgt;       /* Usually defined by the font or the graphics used */

	int font_wid;       /* Size of the font */
	int font_hgt;

	bool visible;        /* Window is visible */
	bool unused;         /*  */
};


/*
 * A structure for holding the text and offset of section headings
 */
struct file_section_type
{
	long offset;  /* Byte position within file */
	cptr text;   /* Pointer to header text string */
};





/*
 * GLOBAL VARIABLES
 */


/*
 * screen paletted, i.e. 256 colors
 */
extern bool paletted;


/*
 * Name of application
 */
extern cptr AppName;


/*
 * Path to the application's working folder.  This may be the application path,
 * or it may be the pre-set variable PRIVATE_USER_PATH.
 */
extern char WorkingFolder[1024];


/*
 * Directory names
 */
extern cptr ANGBAND_DIR_XTRA_FONT;
extern cptr ANGBAND_DIR_XTRA_GRAF;
extern cptr ANGBAND_DIR_XTRA_SOUND;
extern cptr ANGBAND_DIR_XTRA_MUSIC;


/*
 * Width and height of the (available) screen.
 */
extern int screen_w;
extern int screen_h;

/*
 * Application window size and position.  Only applies when game is in windowed
 * mode.  Not all ports can actually adjust window position.
 */
extern int app_left;
extern int app_top;
extern int app_wid;
extern int app_hgt;

/*
 * Width and height of the application client window; application-useable
 * space within any borders.
 */
extern int client_w;
extern int client_h;


/*
 * The current and requested display modes
 */
extern int cur_display_mode;
extern int arg_display_mode;


/*
 * An array of internal game terms.
 */
extern term term_data[TERM_MAX];



/*
 * Globals for the sound system
 */
#ifdef USE_SOUND

/*
 * Any of several sounds can be chosen at random for a single event
 */
#define SAMPLE_MAX 6

/*
 * An array of sound file names
 */
extern cptr sound_file[MSG_MAX][SAMPLE_MAX];
extern cptr music_file[MUSIC_MAX][SAMPLE_MAX];

/* Indication of whether the sound and music arrays are ready yet */
extern bool sounds_ready;
extern bool music_ready;


/* Time (in seconds) that the current music started */
extern u32b cur_music_start_time;

/* The theme that the song now playing belongs to */
extern int cur_music_theme;

/* Length of current song */
extern int cur_song_length;


#endif /* USE_SOUND */



/*
 * Multimedia quality level.  Allows the user to balance efficiency against
 * performance.  This knob is not very important yet but it might well become so.
 */
extern int media_quality_level;




/*
 * Gamma correction value
 */
#ifdef SUPPORT_GAMMA

extern int gamma_correction;

#endif /* SUPPORT_GAMMA */


/*
 * Window settings, window defaults.
 */
extern window_type window_settings[TERM_MAX + 1];
extern window_type window_defaults[NUM_WINDOW_DEFAULTS];




/*
 * FUNCTIONS
 */
extern errr Term_xtra_gui_overlap(void);
extern void seek_app_size_defaults(int *wid_tmp, int *hgt_tmp,
	int monitor_wid, int monitor_hgt);
extern void calc_map_display(void);
extern void get_term_position(int *term, int *col, int *row, int x, int y, bool ignore_popups);
extern void overlap_notify(int x1, int y1, int x2, int y2);
extern errr Term_fresh_gui(bool total_erase);
extern errr switch_display_gui(int display);
extern void special_view_gui(int cols, int rows, bool activate);
extern void do_maximize(void);
extern errr Term_user_gui(int n);
extern char *analyze_font(char *path, int *wp, int *hp);
extern void get_font_list(char *filename, char *suffix);
extern void window_change_font_gui(int idx, int cursor_y);
extern bool use_largest_font(window_type *win_ptr, int cell_wid, int cell_hgt);
extern bool load_sound_prefs(void);
extern bool load_music_prefs(void);
extern bool jukebox(int *v, int cur_time, int *fade_in, int *fade_out);
extern int rd_config_int(FILE *fff, cptr section, cptr line, const int v);
extern void rd_config_str(FILE *fff, cptr section, cptr line, char *data, int data_size, cptr v);
extern void free_file_index(void);
extern void replace_section(cptr orig_file, char *section, char *data);
extern void save_prefs(void);
extern void load_prefs(bool first_pass);



/* Hooks */
extern void (*window_change_font_hook)(int idx, int cursor_y);
extern errr (*change_font_hook)(window_type *win_ptr, char *path);
extern void (*remove_font_hook)(window_type *win_ptr);
extern bool (*init_graphics_hook)(bool verify_map);
extern void (*term_data_link_hook)(window_type *win_ptr);
