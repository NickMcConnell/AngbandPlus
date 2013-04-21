

#include "angband.h"
#include "langband.h"


#if defined(USE_GTK)
errr init_gtk(int argc, char **argv);
#endif

#if defined(USE_X11)
errr init_x11(int argc, char **argv);
#endif

#if defined(USE_GCU)
errr init_gcu(int argc, char **argv);
#endif

#if defined(USE_WIN)
errr voff_win();
#endif

void handle_stuff(void);

/** defaults to true as cmucl is default */
int lisp_will_use_callback = 1;

/** set default illegal value. */
LISP_SYSTEMS current_lisp_system = LISPSYS_BAD;

/** the base path for config files */
const char *base_config_dir = "./";

int which_ui_used = -1;

int been_run_earlier = 0;

/** quick_messages? */
bool quick_messages = TRUE;

/** make -more- simple? */
bool auto_more = FALSE;

/** freshen output after dumping stuff? */
bool fresh_after = TRUE;

/**
 * Hack -- take notes on line 23
 */
static void note(cptr str)
{
    Term_erase(0, 23, 255);
    Term_putstr(20, 23, -1, TERM_WHITE, str);
    Term_fresh();
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
void c_prt(byte attr, cptr str, int row, int col)
{
    /* Clear line, position cursor */
    Term_erase(col, row, 255);

	/* Dump the attr/text */
    Term_addstr(-1, attr, str);
}


/*
 * As above, but in "white"
 */
void prt(cptr str, int row, int col)
{
    /* Spawn */
    c_prt(TERM_WHITE, str, row, col);
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
init_c_side(const char *ui, const char *basePath, int debugging_level) {

    int possible_to_go_X = 0;
    int default_mode = 0; // 0 is x, 1 is gcu, 2 is gtk.. hackish
    int wanted_ui = 0;
        
    // leak
    int argc = 1;
    char **argv = malloc(100);

    
    argv[0] = "langband";
    argv[1] = NULL;

    if (!ui) {
	ui = "X11";
    }

//    {
//        FILE *foo;
//    foo = fopen("c:/tmp/lang-err.txt","w");
//	foo = stderr;
//	fprintf(foo,"C-side.. %s %s %d\n", ui, basePath, debugging_level);
//    fclose(foo);
//    }
    if (basePath && (strlen(basePath)>0)) {
	base_config_dir = basePath;
    }
    

    /* verify that we have decent value */
    if (!strcmp(ui, "DEFAULT") ||
	!strcmp(ui, "default")) {
	wanted_ui = default_mode;
    }
    else if (!strcmp(ui, "X11") ||
	     !strcmp(ui, "x11") ||
	     !strcmp(ui, "X")) {
	wanted_ui = 0;
    }
    else if (!strcmp(ui, "gcu") ||
	     !strcmp(ui, "curses") ||
	     !strcmp(ui, "GCU")) {
	wanted_ui = 1;
    }
    else if (!strcmp(ui, "gtk") ||
	     !strcmp(ui, "gtk+") ||
	     !strcmp(ui, "GTK")) {
	wanted_ui = 2;
    }
    else if (!strcmp(ui, "win") ||
	     !strcmp(ui, "Win") ||
	     !strcmp(ui, "WIN")) {
	wanted_ui = 3;
    }
				     
    else {
	fprintf(stderr, "Unable to find compatible UI with spec '%s'\n", ui);
	return -1;
    }

    if (wanted_ui >= 0 && wanted_ui <= 3) {

    }
    else {
	fprintf(stderr, "The UI-value is set to an illegal value: %d\n", wanted_ui);
	return -2;
    }

    
#ifdef USE_SOUND
    // hackish
    use_sound = 1;
    sound_init();
#endif


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
    
#if defined(USE_X11)
    else if (possible_to_go_X && wanted_ui == 0) {
	which_ui_used = 0;
	init_x11(0,NULL); /* proper value set */
    }
#endif
    
#if defined(USE_GTK)
    else if (possible_to_go_X && wanted_ui == 2) {
	which_ui_used = 2;
	init_gtk(argc,argv); /* proper value set */
    }
#endif
    
#if defined (USE_GCU)
    else if (wanted_ui == 1 || !possible_to_go_X) {
	which_ui_used = 1;
	init_gcu(argc,argv);
    }
#endif

#if defined (USE_WIN)
    else if (wanted_ui == 3) {
	which_ui_used = 3;
	init_win();
    }
#endif
    
    else {
	if (!possible_to_go_X && (wanted_ui == 0 || wanted_ui == 2)) {
	    fprintf(stderr, "Wanted an X-dependent UI, but unable to find X (no DISPLAY env).\n");
	}
	else {
	    fprintf(stderr,"Unable to find a suitable UI to use [%s,%d].\n", ui, wanted_ui);
	}
	return -10 - wanted_ui;
    }

//    printf("late init..\n");
    
#if defined(USE_X11) || defined(USE_GCU)
    /* Initialize */
    init_angband();
//    printf("i a\n");
    pause_line(23);
//    printf("o a\n");
    play_game(TRUE);
#endif

    return 0;
}


void
init_angband(void) {

//       int fd = -1;

//        int mode = 0644;

    FILE *fp;

    char buf[1024];
    char fname[1024];
 
    /* Open the News file */
    sprintf(fname, "%snews.txt", base_config_dir);
    /*printf("Trying to open |%s|\n", fname);*/
    fp = my_fopen(fname, "r");

    /* Dump */
    if (fp)
    {
	int i = 0;

	/* Dump the file to the screen */
	while (0 == my_fgets(fp, buf, 1024))
	{
	    /* Display and advance */
	    Term_putstr(0, i++, -1, TERM_WHITE, buf);
	}

	/* Close */
	my_fclose(fp);
    }


    /* Flush it */
    Term_fresh();

//    puts("mac i");
    macro_init();
    message_init();
//    puts("/mac i");
#ifdef USE_X11
//	process_pref_file("./lib/file/user.prf");
//	puts("prof..");
#endif
	
    note("[Initialization complete]");

}



void window_stuff(void) {

//    printf("window stuff\n");

}

void handle_stuff(void) {

}

void
play_game(bool new_game) {

    new_game = 0;  // to avoid warning
    
    /* Hack -- Increase "icky" depth */
    character_icky++;

    /* Verify main term */
    if (!angband_term[0])
    {
	quit("main window does not exist");
    }

    /* Make sure main term is active */
    Term_activate(angband_term[0]);

    /* Verify minimum size */
    if ((Term->hgt < 24) || (Term->wid < 80))
    {
	quit("main window is too small");
    }

    /* Forbid resizing */
    Term->fixed_shape = TRUE;

    /* Hack -- Turn off the cursor */
    (void)Term_set_cursor(0);

    if (lisp_will_use_callback) {
	// this is a callback
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
print_coloured_stat (byte attr,
		     int stat,
		     int row,
		     int col) {

//    printf("Printing stat %d at row %d\n", stat, row);
    clean_hidden_buffer();
    cnv_stat(stat,hidden_buffer);
    c_put_str(attr,hidden_buffer, row, col);

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
    "Int",
    "Wis",
    "Dex", // 15
    "Con",
    "Chr",
    "STR",
    "INT",
    "WIS", //20
    "DEX",
    "CON",
    "CHR",
    "            "
};

void
print_coloured_token (byte attr,
		      int token,
		      int row,
		      int col) {

//    printf("Going for token %d\n", token);
//    printf("This token is %s\n", token_list[token]);
    c_put_str(attr,token_list[token], row, col);
    
}

void
print_coloured_number (byte attr,
			long number,
			int padding,
			int row,
			int col) {

    char *format_str = "%ld";
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
	printf("no print_col_number for %d\n", padding);
    }
    sprintf(hidden_buffer, format_str, number);
    c_put_str(attr,hidden_buffer, row, col);

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
