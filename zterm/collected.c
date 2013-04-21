

#include "angband.h"
#include "langband.h"
#include "lbwindows.h"

#if defined(USE_GTK)
errr init_gtk(int argc, char **argv);
#endif

#if defined(USE_X11)
errr init_x11(int argc, char **argv);
#endif

#if defined(USE_GCU)
errr init_gcu(int argc, char **argv);
#endif

#if defined(USE_SDL)
errr init_sdl(int argc, char **argv);
#endif


void handle_stuff(void);

/** defaults to true as cmucl is default */
int lisp_will_use_callback = 1;

/** set default illegal value. */
LISP_SYSTEMS current_lisp_system = LISPSYS_BAD;

/** the base path for source files */
const char *base_source_path = "./";
/** the base path for config files */
const char *base_config_path = "./config/";
/** the base path for gfx files */
const char *base_gfx_path = "./graphics/";

int which_ui_used = -1;

int been_run_earlier = 0;

// hacks, remove later
int px = 5;
int py = 5;
int cur_wid = 60;
int cur_hgt = 60;

/** quick_messages? */
//bool quick_messages = TRUE;

/** make -more- simple? */
//bool auto_more = FALSE;

/** freshen output after dumping stuff? */
//bool fresh_after = TRUE;

/**
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
    if (str) {
	s16b buffer[1024];
	int i;
	int s_len = strlen(str);
	
	for (i=0; i < s_len; i++) {
	    buffer[i] = (s16b)str[i];
	}
	buffer[i] = 0;
	
	Term_erase(0, 23, 255);
	Term_putstr(20, 23, -1, TERM_WHITE, buffer);
	Term_fresh();
    }
}


/*
 * Handle abrupt death of the visual system
 *
 * This routine is called only in very rare situations, and only
 * by certain visual systems, when they experience fatal errors.
 *
 * XXX XXX Hack -- clear the death flag when creating a HANGUP
 * save file so that player can see tombstone when restart.
 */
void exit_game_panic(void)
{

}



/*
 * Display a string on the screen using an attribute, and clear
 * to the end of the line.
 */
void c_prt(s16b attr, cptr str, int row, int col)
{
    if (str) {
	int i;
	int s_len = strlen(str);
	s16b buffer[1024];
	
	for (i=0; i < s_len; i++) {
	    buffer[i] = (s16b)str[i];
	}
	buffer[i] = 0;
	
	/* Clear line, position cursor */
	Term_erase(col, row, 255);
	
	/* Dump the attr/text */
	Term_addstr(-1, attr, buffer);
    }
}

// we're not handling signals
/*
 * Do nothing
 */
void signals_ignore_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_handle_tstp(void)
{
}

/*
 * Do nothing
 */
void signals_init(void)
{
}


void
disturb(int stop_search, int unused_flag) {

    (void)(stop_search + unused_flag); // to avoid warning
    note("disturb");
}

bool
save_player(void) {

    note("saving on C-side");
    return 0;
}

int current_ui() { return which_ui_used; }

errr
init_c_side(const char *ui, const char *sourcePath, const char *configPath,
	    const char *gfxPath, int extra_flags) {

    int possible_to_go_X = 0;
    int default_mode = 0; // 0 is x, 1 is gcu, 2 is gtk.. hackish
    int wanted_ui = 0;
    int graphical = (extra_flags & LANGBAND_GRAPHICS);
    int init_retval = -666;
    
    // leak
    int argc = 1;
    char **argv = malloc(100);

#ifdef USE_SDL
    use_sound = (extra_flags & LANGBAND_SOUND);
#else
    use_sound = FALSE;
#endif
    
    // z-util
    argv0 = "Langband";
    
    if (!ui) {
	ui = "X11";
    }

    if (sourcePath && (strlen(sourcePath)>0)) {
	char *str = malloc(strlen(sourcePath) +2);
	strcpy(str, sourcePath);
	base_source_path = str;
    }
    
    if (configPath && (strlen(configPath)>0)) {
	char *str = malloc(strlen(configPath) +2);
	strcpy(str, configPath);
	base_config_path = str;
    }
    if (gfxPath && (strlen(gfxPath)>0)) {
	char *str = malloc(strlen(gfxPath) +2);
	strcpy(str, gfxPath);
	base_gfx_path = str;
    }

    /* verify that we have decent value */
    if (!strcmp(ui, "DEFAULT") ||
	!strcmp(ui, "default")) {
	wanted_ui = default_mode;
    }
    else if (!strcmp(ui, "X11") ||
	     !strcmp(ui, "x11") ||
	     !strcmp(ui, "X")) {
	wanted_ui = UITYPE_X11;
    }
    else if (!strcmp(ui, "gcu") ||
	     !strcmp(ui, "curses") ||
	     !strcmp(ui, "GCU")) {
	wanted_ui = UITYPE_GCU;
	graphical = 0; // override
    }
    else if (!strcmp(ui, "gtk") ||
	     !strcmp(ui, "gtk+") ||
	     !strcmp(ui, "GTK")) {
	wanted_ui = UITYPE_GTK;
	graphical = 0; // override
    }
    else if (!strcmp(ui, "win") ||
	     !strcmp(ui, "Win") ||
	     !strcmp(ui, "WIN")) {
	wanted_ui = UITYPE_WIN;
	graphical = 0; // override
    }
    else if (!strcmp(ui, "sdl") ||
	     !strcmp(ui, "SDL")) {
	wanted_ui = UITYPE_SDL;
	graphical = 1; // override
    }

				     
    else {
	ERRORMSG("Unable to find compatible UI with spec '%s'\n", ui);
	return -1;
    }

    if (wanted_ui >= UITYPE_X11 && wanted_ui <= UITYPE_SDL) {

    }
    else {
	ERRORMSG("The UI-value is set to an illegal value: %d\n", wanted_ui);
	return -2;
    }


    {
	int i = 0;
	argv[i++] = "langband";
	if (graphical) {
	    argv[i++] = "-g";
	}
	argv[i++] = NULL;
	
	argc = i-1;
    }

    

    /* let us check if we can go X */
    
#if defined(USE_GTK) || defined(USE_X11)    
    {
	char *val = getenv("DISPLAY");
	if (val && strlen(val)) {
	    possible_to_go_X = 1;
	}
    }
#endif
    

    if (1==0) { }

#if defined (USE_SDL)
    else if (wanted_ui == UITYPE_SDL) {
	which_ui_used = UITYPE_SDL;
	init_retval = init_sdl(argc, argv);
    }
#endif
    
#if defined(USE_X11)
    else if (possible_to_go_X && wanted_ui == UITYPE_X11) {
	which_ui_used = UITYPE_X11;
	init_retval = init_x11(argc,argv); /* proper value set */
    }
#endif
    
#if defined(USE_GTK)
    /* a fallback if X11 doesn't exist */
    else if (possible_to_go_X && (wanted_ui == UITYPE_GTK || wanted_ui == UITYPE_X11)) {
	which_ui_used = UITYPE_GTK;
	init_retval = init_gtk(argc,argv); /* proper value set */
    }
#endif
    
#if defined (USE_GCU)
    else if (wanted_ui == UITYPE_GCU || !possible_to_go_X) {
	which_ui_used = UITYPE_GCU;
	init_retval = init_gcu(argc, argv);
    }
#endif

    else {
	if (!possible_to_go_X && (wanted_ui == UITYPE_X11 || wanted_ui == UITYPE_GTK)) {
	    ERRORMSG("Wanted an X-dependent UI, but unable to find X (no DISPLAY env).\n");
	}
	else {
	    ERRORMSG("Unable to find a suitable UI to use [%s,%d].\n", ui, wanted_ui);
	}
	return -10 - wanted_ui;
    }

    if (init_retval != 0) {
	ERRORMSG("Init of UI screwed up miserably, exiting.\n");
	return init_retval;
    }
//    printf("late init..\n");
    
#if defined(USE_X11) || defined(USE_GCU) || defined(USE_SDL)
    /* Initialize */
//    init_angband(graphical);
//    printf("i a\n");
//    pause_line(23);
//    printf("o a\n");
    play_game(TRUE, graphical);
#endif

    DBGPUT("returning in flames..");
    return 0;
}


void
init_angband(int graphical) {

#if !defined(USE_X11) && !defined(USE_SDL)
    graphical = 0; // local effect
#endif
   

    if (!graphical) {
	
	char buf[1024];
	char fname[1024];
	
        FILE *fp;
	/* Open the News file */
	sprintf(fname, "%snews.txt", base_config_path);
	/*printf("Trying to open |%s|\n", fname);*/
	fp = my_fopen(fname, "r");
	
	/* Dump */
	if (fp)
	{
	    int i = 0;
	    int j = 0;
	    s16b buffer[1024];
	    
	    /* Dump the file to the screen */
	    while (0 == my_fgets(fp, buf, 1024)) {
		for (j =0; j < 1022; j++) {
		    buffer[j] = (s16b)buf[j];
		}
		/* Display and advance */
		Term_putstr(0, i++, -1, (s16b)TERM_WHITE, buffer);
	    }
	    
	    /* Close */
	    my_fclose(fp);
	}
    }

    else {
#ifdef USE_X11
	int val = 0;
	init_graphics();
	
	/* paint and load splash */

	val = load_gfx_image("langtitle.bmp", "other");

	if (val >= 0) {
	    fill_area(val, 0, 0, 0, 40, 23);
	    paint_gfx_image("langtitle.bmp", "other", 5, 0);
	}

	note("[Loading tiles, please wait.]");
	init_tile_files();

#elif USE_SDL
	
	int val = 0;
	
	/* paint and load splash */
	//DBGPUT("Load and paint.\n");
	val = load_gfx_image("langtitle.bmp", "other");

	if (val >= 0) {
	    //DBGPUT("Fill areas %d %p\n", val, Term);
	    fill_area(val, 0, 0, 0, 99, 36);
	    //DBGPUT("paint!");
	    paint_gfx_image("langtitle.bmp", "other", 5, 0);
	}

	//DBGPUT("life goes on\n");
#endif /* use_x11 */

    }

    
    /* Flush it */
    Term_fresh();
    //DBGPUT("after fresh\n");
}



void window_stuff(void) {

//    printf("window stuff\n");

}

void handle_stuff(void) {

}

void
play_game(bool new_game, int graphical) {

    new_game = 0;  // to avoid warning
    
    /* Hack -- Increase "icky" depth */
    character_icky++;

    /* Verify main term */
    if (has_frame(0, ACTIVE) == 0) {
	z_quit("main window does not exist\n");
    }

    /* Make sure main term is active */
    Term_activate(activeFrames[0]->azt);

#ifndef USE_SDL
    /* Verify minimum size */
    if ((Term->hgt < 24) || (Term->wid < 80))
    {
	z_quit("main window is too small\n");
    }
#endif
    
    /* Forbid resizing */
//    Term->fixed_shape = TRUE;

    /* Hack -- Turn off the cursor */
    (void)Term_set_cursor(0);
    /* discard old input */
    Term_flush();
    flush();

    init_angband(graphical); // move to lisp-side later
    
    Term_flush();
    flush();

//    pause_line(23, "[Please hit a key to start loading lisp-data]");
    
    //DBGPUT("lisp-sys %d, callback %d\n", current_lisp_system, lisp_will_use_callback);
    if (lisp_will_use_callback) {
	// this is a callback
	//DBGPUT("going back");
	play_game_lisp();
    }

}

/* Here we try to do a few things on the C-side to avoid allocating
   things on the Lisp-side. */

#define MAX_BUF_SZ 1024
static char *hidden_buffer = NULL;

static void
clean_hidden_buffer() {
    if (!hidden_buffer) {
	C_MAKE(hidden_buffer, MAX_BUF_SZ, char);
    }
    memset(hidden_buffer,MAX_BUF_SZ,0);
}

/*
 * Converts stat num into a six-char (right justified) string
 */
static void
cnv_stat(int val, char *out_val) {

    /* Above 18 */
    if (val > 18) {
	int bonus = (val - 18);

	if (bonus >= 100) {
	    sprintf(out_val, "18/%03d", bonus);
	}
	else {
	    sprintf(out_val, " 18/%02d", bonus);
	}
    }

    /* From 3 to 18 */
    else {
	sprintf(out_val, "    %2d", val);
    }
}


void
print_coloured_stat (int wantedTerm,
		     int attr,
		     int stat,
		     int row,
		     int col) {
    
    term *old = Term;
	
    Term_activate(activeFrames[wantedTerm]->azt);

    //DBGPUT("Printing stat %d at row %d\n", stat, row);
    clean_hidden_buffer();
    cnv_stat(stat,hidden_buffer);
    c_put_str((s16b)attr,hidden_buffer, row, col);
    Term_activate(old);
}

static const char *token_list[] = {
    "", // 0
    "Name",
    "Cur MP",
    "Max MP",
    "Level",
    "LEVEL", // 5
    "Exp",
    "EXP",
    "Cur HP",
    "Max HP",
    "Cur AC", // 10
    "AU",
    "Str",
    "Dex", 
    "Con",
    "Int", // 15
    "Wis",
    "Chr",
    "STR",
    "DEX",
    "CON", //20
    "INT",
    "WIS", 
    "CHR",
    "            "
};

void
print_coloured_token (int wantedTerm,
		      int attr,
		      int token,
		      int row,
		      int col) {

    term *old = Term;
    
    Term_activate(activeFrames[wantedTerm]->azt);
    //DBGPUT("Going for token %d\n", token);
    //DBGPUT("This token is %s\n", token_list[token]);
    c_put_str((s16b)attr,token_list[token], row, col);
    Term_activate(old);
}

void
print_coloured_number (int wantedTerm,
		       int attr,
		       long number,
		       int padding,
		       int row,
		       int col) {

    char *format_str = "%ld";
    term *old = Term;
	
    Term_activate(activeFrames[wantedTerm]->azt);

    clean_hidden_buffer();
    if (padding == 9) {
	format_str = "%9ld";
    }
    else if (padding == 5) {
	format_str = "%5ld";
    }
    else if (padding == 8) {
	format_str = "%8ld";
    }
    else if (padding == 6) {
	format_str = "%6ld";
    }
    else {
	ERRORMSG("no print_col_number for %d\n", padding);
    }
    sprintf(hidden_buffer, format_str, number);
    c_put_str((s16b)attr, hidden_buffer, row, col);
    Term_activate(old);
}

errr
cleanup_c_side(void) {

    int cur_ui = current_ui();
    if (0) { }
#ifdef USE_X11
    else if (cur_ui == 0) {
	return cleanup_X11();
    }
#endif

#ifdef USE_GCU
    else if (cur_ui == 1) {
	return cleanup_GCU();
    }
#endif
    
    return 1;
}

errr
my_Term_putstr(int col, int row, int something, int colour, const char* text) {
    if (text) {
	int i;
	int retval;
	int s_len = strlen(text);
	s16b buffer[1024];
	
	for (i=0; i < s_len; i++) {
	    buffer[i] = (s16b)text[i];
	}
	buffer[i] = 0;
	retval = Term_putstr(col, row, something, (s16b)colour, buffer);
	//DBGPUT("Putting %d string %s gave %d\n", something, text, retval);
	return retval;
    }
    else {
	return -1;
    }
}

void
my_Term_queue_char(int col, int row, int colour, int the_char, int tcol, int tchar) {
//    byte bcol = (byte)colour;
//    char bchar = (char)the_char;
    /*
    if (colour == 128 && the_char == 128) {
	DBGPUT("Calling elvis (%d,%d) with (%d,%d) and (%d,%d)\n", 
	col, row, colour, the_char, bcol, bchar);
    }
    */
    Term_queue_char(col, row, (s16b)colour, (s16b)the_char, (s16b)tcol, (s16b)tchar);
}

errr
my_Term_set_cursor(int v) {
    return Term_set_cursor((bool)v);
}

int
my_term_activate(int num) {
    term *t = NULL;
    //DBGPUT("want to activate %d.\n", num);
    if (!legal_frame_key_p(num, ACTIVE)) {
	ERRORMSG("Illegal key %d given to term_activate().\n", num);
	return -50;
    }

    if (activeFrames[num]) {
	t = activeFrames[num]->azt;
	//DBGPUT("activate %d %d %p %p %p\n", num, t->active_flag, t->xtra_hook, t->text_hook, t->pict_hook);
	Term_activate(t);
	//DBGPUT("now active %d %d %p %p %p\n", num, t->active_flag, t->xtra_hook, t->text_hook, t->pict_hook);
	return num;
    }

    else {
	//ERRORMSG("Tried to activate term for an inactive frame %d.\n", num);
	return -51;
    }

}


int
my_get_current_term() {
    int j;
    for (j = 0; j < max_activeFrames; j++) {
	if (activeFrames[j] && (Term == activeFrames[j]->azt)) {
	    //DBGPUT("current term returns %d\n", j);
	    return j;
	}
    }
    return -1;
}

int
get_sound_status() {
    return use_sound;
}


/* Sound-related stuff */
int max_sound_bites = -1;
sound_bite **sound_bites = NULL;

int
init_sound_system(int size) {

    if (size < 1) {
	ERRORMSG("Illegal size %d given for sound-caching.\n", size);
	return -1;
    }
    else {
#ifdef USE_SDL
	int i = 0;
	sound_bites = malloc(size * sizeof(sound_bite*));
	for (i=0; i < size; i++) {
	    sound_bites[i] = NULL;
	}
	max_sound_bites = size;
#endif
	return 0;
    }
    
}

int
load_sound_effect(const char *fname, int idx) {

    char *ptr = NULL;
    sound_bite *sb = NULL;
    
    if (idx >= max_sound_bites) {
	ERRORMSG("Illegal index %d given for sound-effect %s.\n", idx, fname);
	return -1;
    }
    // we should add it to the list
    if (idx < 0) {
	int i = 0;
	for (i=0; i < max_sound_bites; i++) {
	    if (sound_bites[i] == NULL) {
		idx = i;
		break;
	    }
		
	}
	if (idx < 0) {
	    ERRORMSG("Sound-cache filled already, cannot add more sound-effects.\n");
	    return -3;
	}
    }

    if (!fname || strlen(fname) < 2) {
	ERRORMSG("The filename given for sound-effect %d is not legal.\n", idx);
	return -2;
    }

    ptr = malloc(strlen(fname)+1);
    strcpy(ptr, fname);

    sb = malloc(sizeof(sound_bite));
    
    sb->filename = ptr;
    sb->handle = Mix_LoadWAV(ptr);

    if (sound_bites[idx]) {
	free(sound_bites[idx]);
	sound_bites[idx] = NULL;
    }
    
    sound_bites[idx] = sb;

    return idx;
}

int
textureBackground(int term_num, const char *fname, int alpha) {

    if (FALSE) { return -1;}
#ifdef USE_SDL
    else if (which_ui_used == UITYPE_SDL) {
	return sdl_textureBackground(term_num, fname, alpha);
    }
#endif
    else {
	return -1;
    }
    
}

int
load_gfx_image(const char *fname, const char *type) {

    if (FALSE) { return -1; }
#ifdef USE_SDL
    else if (which_ui_used == UITYPE_SDL) {
	return sdl_load_gfx_image(fname, type);
    }
#endif
    else {
	return -1;
    }
    
}

int
paint_gfx_image(const char *fname, const char *name, int x, int y) {

    if (FALSE) { return -1; }
#ifdef USE_SDL
    else if (which_ui_used == UITYPE_SDL) {
	return sdl_paint_gfx_image(fname, name, x, y);
    }
#endif
    else {
	return -1;
    }
    
}

int
load_scaled_image(const char *filename, int image_index, int width, int height) {

    if (FALSE) { return -1; }
#ifdef USE_SDL
    else if (which_ui_used == UITYPE_SDL) {
	return sdl_load_scaled_image(filename, image_index, width, height);
    }
#endif
    else {
	return -1;
    }
    
}


void
lb_format(FILE *ofile, int priority, const char *fmt, ...) {
    if (!ofile) {

    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	if (priority)
	    fflush(ofile);
    }
}

FILE *dbgout = NULL;

static void
open_dbg_file () {
    dbgout = fopen("dbgout.txt","a");
    fprintf(dbgout, "---------------------------------------\n");
}

#ifdef DEBUG
void
DBGPUT(const char *fmt, ...) {
    FILE *ofile = NULL;

#if defined(USE_GCU) || defined(WIN32)
    if (!dbgout) open_dbg_file();
    ofile = dbgout;
#else
    ofile = stderr;
#endif

    if (!ofile) {
	    
    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	fflush(ofile);
    }
}
#endif /* debug */

void
ERRORMSG(const char *fmt, ...) {

    FILE *ofile = NULL;

#if defined(USE_GCU) || defined(WIN32)
    if (!dbgout) open_dbg_file();
    ofile = dbgout;
#else
    ofile = stderr;
#endif
    
    if (!ofile) {
	    
    }
    else {
	va_list p;
	va_start(p, fmt);
	vfprintf(ofile, fmt, p);
	va_end(p);
	fflush(ofile);
    }

}


#ifdef USE_SDL
extern void SDL_Quit();
extern void Mix_CloseAudio();
#endif

 /*
  * Redefinable "quit" action
  */
void (*quit_aux)(cptr) = NULL;

/*
 * Exit (ala "exit()").  If 'str' is NULL, do "exit(0)".
 * If 'str' begins with "+" or "-", do "exit(atoi(str))".
 * Otherwise, plog() 'str' and exit with an error code of -1.
 * But always use 'quit_aux', if set, before anything else.
 */
void z_quit(cptr str) {
  
  /* Attempt to use the aux function */
  if (quit_aux) (*quit_aux)(str);
  
#ifdef USE_SDL
  DBGPUT("closing %p\n", str);
  if (use_sound) {
    Mix_CloseAudio();
  }
  SDL_Quit();
  DBGPUT("closed\n");
#endif
  
  /* Success */
  if (!str) (void)(exit(0));
  
  /* Extract a "special error code" */
  if ((str[0] == '-') || (str[0] == '+')) (void)(exit(atoi(str)));
  
  /* Send the string as info */
  if (str && strlen(str)) {
    INFOMSG(str);
  }
  
  /* Failure */
  (void)(exit(-1));
}
