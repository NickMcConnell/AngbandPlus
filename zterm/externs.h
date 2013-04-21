/* File: externs.h */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#ifndef INTERFACE
#if defined(WIN_MAKEDLL)
#  define INTERFACE __declspec(dllexport)
#elif defined(WIN_USEDLL)
#  define INTERFACE __declspec(dllimport)
#else
#  define INTERFACE
#endif
#endif /* iface */

/*
 * Note that some files have their own header files
 * (z-virt.h, z-util.h, z-form.h, term.h, random.h)
 */


/*
 * Automatically generated "variable" declarations
 */


/* variable.c */
extern cptr copyright[5];

extern bool arg_fiddle;
extern bool arg_wizard;
extern bool arg_sound;
extern bool arg_graphics;
extern bool arg_force_original;
extern bool arg_force_roguelike;
extern bool character_generated;
extern bool character_loaded;
extern bool character_saved;
extern s16b character_icky;
extern s16b character_xtra;

extern s32b turn;
extern s32b old_turn;
extern bool use_sound;
extern bool use_graphics;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;


extern int player_uid;
extern int player_euid;
extern int player_egid;
extern term *angband_term[8];
extern char angband_term_name[8][16];
extern byte angband_color_table[256][4];
extern sint view_n;
extern u16b *view_g;
//extern player_type *p_ptr;
extern cptr ANGBAND_SYS;
extern cptr ANGBAND_GRAF;
extern cptr ANGBAND_DIR_XTRA;
extern bool use_transparency;
extern bool can_save;


/* collected.c */
extern const char *base_config_dir;
extern int been_run_earlier;
extern void play_game(bool new_game);
INTERFACE void init_angband(void);
//extern bool quick_messages;
//extern bool auto_more;
//extern bool fresh_after;
extern void window_stuff(void);
extern void exit_game_panic(void);
extern void handle_stuff(void);
INTERFACE int current_ui();
INTERFACE void print_coloured_token(byte colour, int token, int row, int col);
INTERFACE void print_coloured_stat(byte colour, int stat, int row, int col);
INTERFACE void print_coloured_number(byte colour, long number, int padding, int row, int col);
INTERFACE errr init_c_side(const char *ui, const char *base_path, int debug);
INTERFACE char *load_sound(int msg, char *fname);
void play_game_lisp();
void readjust_screen_lisp(int width, int height);

/** will we access lisp through callbacks? */
extern int lisp_will_use_callback;
extern LISP_SYSTEMS current_lisp_system;
INTERFACE void set_lisp_system(LISP_SYSTEMS type);

#ifdef WIN32
INTERFACE int setHINST(long val);
#else
INTERFACE void set_lisp_callback(char *name, void *ptr);
#endif

/* util.c */
extern errr path_parse(char *buf, int max, cptr file);
//extern errr path_temp(char *buf, int max);
//extern errr path_build(char *buf, int max, cptr path, cptr file);
extern FILE *my_fopen(cptr file, cptr mode);
extern errr my_fclose(FILE *fff);
extern errr my_fgets(FILE *fff, char *buf, huge n);
extern errr my_fputs(FILE *fff, cptr buf, huge n);
/*
extern errr fd_kill(cptr file);
extern errr fd_move(cptr file, cptr what);
extern errr fd_copy(cptr file, cptr what);
extern int fd_make(cptr file, int mode);
extern int fd_open(cptr file, int flags);
extern errr fd_lock(int fd, int what);
extern errr fd_seek(int fd, huge n);
extern errr fd_chop(int fd, huge n);
extern errr fd_read(int fd, char *buf, huge n);
extern errr fd_write(int fd, cptr buf, huge n);
extern errr fd_close(int fd);
*/
extern sint macro_find_exact(cptr pat);
INTERFACE errr macro_add(cptr pat, cptr act);
extern errr macro_init(void);
extern void flush(void);
INTERFACE char inkey(void);
//INTERFACE void bell(cptr reason);
extern void sound(int val);
//extern s16b message_num(void);
//extern cptr message_str(s16b age);
//extern void message_add(cptr str);
//extern errr message_init(void);
//extern void move_cursor(int row, int col);
//INTERFACE void msg_print(cptr msg);
//extern void msg_format(cptr fmt, ...);
//extern void screen_save(void);
//extern void screen_load(void);
extern void c_put_str(byte attr, cptr str, int row, int col);
//INTERFACE void put_str(cptr str, int row, int col);
extern void c_prt(byte attr, cptr str, int row, int col);
//INTERFACE void c_prt(byte attr, cptr str, int row, int col);
//INTERFACE void prt(cptr str, int row, int col);
INTERFACE void clear_from(int row);
//extern bool askfor_aux(char *buf, int len);
//extern bool get_string(cptr prompt, char *buf, int len);
//extern bool get_check(cptr prompt);
//extern bool get_com(cptr prompt, char *command);
INTERFACE void pause_line(int row);

#ifdef WIN32
INTERFACE int setHINST(long val);
#endif


#ifndef HAS_MEMSET
/* util.c */
extern char *memset(char*, int, huge);
#endif

#ifndef HAS_STRICMP
/* util.c */
extern int stricmp(cptr a, cptr b);
#endif

#ifdef SET_UID
# ifndef HAS_USLEEP
/* util.c */
extern int usleep(huge usecs);
# endif
extern void user_name(char *buf, int id);
#endif

//#ifdef MACINTOSH
/* main-mac.c */
/* extern int main(void); */
//#endif

//#ifdef WINDOWS
/* main-win.c */
/* extern int FAR PASCAL WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, ...); */
//#endif

/* main.c */
/* extern int main(int argc, char *argv[]); */

#ifdef USE_X11
errr cleanup_X11(void);
#endif

#ifdef USE_GCU
errr cleanup_GCU(void);
#endif

#ifdef USE_WIN
errr init_win(void);
#endif

