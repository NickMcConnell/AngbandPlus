/* PosBand -- A variant of Angband roguelike
 *
 * Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 * 
 * NPPAngband Copyright (c) 2003-2004 Jeff Greene
 * PosBand Copyright (c) 2004-2005 Alexander Ulyanov
 */

/* misc.h: misc. definitions */

#ifndef MISC_H_INCLUDED
#define MISC_H_INCLUDED

/**** Typedefs ****/

typedef struct maxima maxima;
typedef struct feature_type feature_type;
typedef struct object_kind object_kind;
typedef struct artifact_type artifact_type;
typedef struct ego_item_type ego_item_type;
typedef struct monster_blow monster_blow;
typedef struct monster_power monster_power;
typedef struct monster_body monster_body;
typedef struct monster_race monster_race;
typedef struct monster_lore monster_lore;
typedef struct vault_req_type vault_req_type;
typedef struct vault_cod_type vault_cod_type;
typedef struct vault_type vault_type;
typedef struct object_type object_type;
typedef struct monster_type monster_type;
typedef struct alloc_entry alloc_entry;
typedef struct quest_type quest_type;
typedef struct owner_type owner_type;
typedef struct store_type store_type;
typedef struct magic_type magic_type;
typedef struct player_magic player_magic;
typedef struct player_sex player_sex;
typedef struct player_race player_race;
typedef struct player_class player_class;
typedef struct hist_type hist_type;
typedef struct player_other player_other;
typedef struct player_type player_type;
typedef struct start_item start_item;
typedef struct high_score high_score;
typedef struct flavor_type flavor_type;
typedef struct pwr_remap pwr_remap;


/*
 * Recommended minimal window size
 */
#define X_MIN		80
#define Y_MIN		24

/*
 * Maximum amount of Angband windows.
 */
#define ANGBAND_TERM_MAX	8

/*
 * Maximum number of high scores in the high score file
 */
#define MAX_HISCORES	100

/*
 * OPTION: Maximum number of macros (see "util.c")
 * Default: assume at most 256 macros are used
 */
#define MACRO_MAX	256

/*
 * OPTION: Maximum number of "quarks" (see "util.c")
 * Default: assume at most 512 different inscriptions are used
 * Increased in PosBand 0.2.0, quarks are used not only for inscriptions -AU-
 */
#define QUARK_MAX	1024

/*
 * OPTION: Maximum number of messages to remember (see "util.c")
 * Default: assume maximal memorization of 2048 total messages
 */
#define MESSAGE_MAX	2048

/*
 * OPTION: Maximum space for the message text buffer (see "util.c")
 * Default: assume that each of the 2048 messages is repeated an
 * average of three times, and has an average length of 48
 */
#define MESSAGE_BUF	32768

/*
 * Maximum value storable in a "byte" (hard-coded)
 */
#define MAX_UCHAR       255

/*
 * Maximum value storable in a "s16b" (hard-coded)
 */
#define MAX_SHORT       32767

/*** Screen Locations ***/

/*
 * Some screen locations for various display routines
 * Currently, row 8 and 15 are the only "blank" rows.
 * That leaves a "border" around the "stat" values.
 */

#define ROW_MAP			1
#define COL_MAP			13	/* ? */

#define ROW_RACE		1
#define COL_RACE		0	/* <race name> */

#define ROW_CLASS		2
#define COL_CLASS		0	/* <class name> */

#define ROW_TITLE		3
#define COL_TITLE		0	/* <title> or <mode> */

#define ROW_LEVEL		4
#define COL_LEVEL		0	/* "LEVEL xxxxxx" */

#define ROW_EXP			5
#define COL_EXP			0	/* "EXP xxxxxxxx" */

#define ROW_GOLD		6
#define COL_GOLD		0	/* "AU xxxxxxxxx" */

#define ROW_EQUIPPY		7
#define COL_EQUIPPY		0	/* equippy chars */

#define ROW_STAT		8
#define COL_STAT		0	/* "xxx   xxxxxx" */

#define ROW_AC			14
#define COL_AC			0	/* "Cur AC xxxxx" */

#define ROW_MAXHP		15
#define COL_MAXHP		0	/* "Max HP xxxxx" */

#define ROW_CURHP		16
#define COL_CURHP		0	/* "Cur HP xxxxx" */

#define ROW_MAXSP		17
#define COL_MAXSP		0	/* "Max SP xxxxx" */

#define ROW_CURSP		18
#define COL_CURSP		0	/* "Cur SP xxxxx" */

#define ROW_INFO		19
#define COL_INFO		0	/* "xxxxxxxxxxxx" monster health bar*/

#define ROW_MON_MANA	20
#define COL_MON_MANA	0	/* "xxxxxxxxxxxx" monster mana bar*/

#define ROW_CUT			21
#define COL_CUT			0	/* <cut> */

#define ROW_STUN		22
#define COL_STUN		0	/* <stun> */

#define ROW_HUNGRY		(Term->hgt - 1)
#define COL_HUNGRY		0	/* "Weak" / "Hungry" / "Full" / "Gorged" */

#define ROW_BLIND		(Term->hgt - 1)
#define COL_BLIND		7	/* "Blind" */

#define ROW_CONFUSED	(Term->hgt - 1)
#define COL_CONFUSED	13	/* "Confused" */

#define ROW_AFRAID		(Term->hgt - 1)
#define COL_AFRAID		22	/* "Afraid" */

#define ROW_POISONED	(Term->hgt - 1)
#define COL_POISONED	29	/* "Poisoned" */

#define ROW_STATE		(Term->hgt - 1)
#define COL_STATE		38	/* <state> */

#define ROW_SPEED		(Term->hgt - 1)
#define COL_SPEED		49	/* "Slow (-NN)" or "Fast (+NN)" */

#define ROW_STUDY		(Term->hgt - 1)
#define COL_STUDY		64	/* "Study" */

#define ROW_DEPTH		(Term->hgt - 1)
#define COL_DEPTH		70	/* "Lev NNN" / "NNNN ft" */

/*
 * Number of keymap modes
 */
#define KEYMAP_MODES	2

/*
 * Mode for original keyset commands
 */
#define KEYMAP_MODE_ORIG	0

/*
 * Mode for roguelike keyset commands
 */
#define KEYMAP_MODE_ROGUE	1

/*
 * Hack -- The main "screen"
 */
#define term_screen	(angband_term[0])

/*** Color constants ***/

/*
 * Angband "attributes" (with symbols, and base (R,G,B) codes)
 *
 * The "(R,G,B)" codes are given in "fourths" of the "maximal" value,
 * and should "gamma corrected" on most (non-Macintosh) machines.
 */
#define TERM_DARK		0	/* 'd' */	/* 0,0,0 */
#define TERM_WHITE		1	/* 'w' */	/* 4,4,4 */
#define TERM_SLATE		2	/* 's' */	/* 2,2,2 */
#define TERM_ORANGE		3	/* 'o' */	/* 4,2,0 */
#define TERM_RED		4	/* 'r' */	/* 3,0,0 */
#define TERM_GREEN		5	/* 'g' */	/* 0,2,1 */
#define TERM_BLUE		6	/* 'b' */	/* 0,0,4 */
#define TERM_UMBER		7	/* 'u' */	/* 2,1,0 */
#define TERM_L_DARK		8	/* 'D' */	/* 1,1,1 */
#define TERM_L_WHITE		9	/* 'W' */	/* 3,3,3 */
#define TERM_VIOLET		10	/* 'v' */	/* 4,0,4 */
#define TERM_YELLOW		11	/* 'y' */	/* 4,4,0 */
#define TERM_L_RED		12	/* 'R' */	/* 4,0,0 */
#define TERM_L_GREEN		13	/* 'G' */	/* 0,4,0 */
#define TERM_L_BLUE		14	/* 'B' */	/* 0,4,4 */
#define TERM_L_UMBER		15	/* 'U' */	/* 3,2,1 */

/* Message types */
enum
{
        MSG_GENERIC,
        MSG_HIT,
        MSG_MISS,
        MSG_FLEE,
        MSG_DROP,
        MSG_KILL,
        MSG_LEVEL,
        MSG_DEATH,
        MSG_STUDY,
        MSG_TELEPORT,
        MSG_SHOOT,
        MSG_QUAFF,
        MSG_ZAP,
        MSG_WALK,
        MSG_TPOTHER,
        MSG_HITWALL,
        MSG_EAT,
        MSG_STORE1,
        MSG_STORE2,
        MSG_STORE3,
        MSG_STORE4,
        MSG_DIG,
        MSG_OPENDOOR,
        MSG_SHUTDOOR,
        MSG_TPLEVEL,
        MSG_BELL,
        MSG_NOTHING_TO_OPEN,
        MSG_LOCKPICK_FAIL,
        MSG_STAIRS,
        MSG_HITPOINT_WARN,
        MSG_MAX
};

/*
 * Mega-Hack -- some primitive sound support (see "main-win.c")
 *
 * Some "sound" constants for "Term_xtra(TERM_XTRA_SOUND, val)"
 */
enum
{
        SOUND_HIT,
        SOUND_MISS,
        SOUND_FLEE,
        SOUND_DROP,
        SOUND_KILL,
        SOUND_LEVEL,
        SOUND_DEATH,
        SOUND_STUDY,
        SOUND_TELEPORT,
        SOUND_SHOOT,
        SOUND_QUAFF,
        SOUND_ZAP,
        SOUND_WALK,
        SOUND_TPOTHER,
        SOUND_HITWALL,
        SOUND_EAT,
        SOUND_STORE1,
        SOUND_STORE2,
        SOUND_STORE3,
        SOUND_STORE4,
        SOUND_DIG,
        SOUND_OPENDOOR,
        SOUND_SHUTDOOR,
        SOUND_TPLEVEL
};

/*
 * Mega-Hack -- maximum known sounds
 *
 * Should be the same as MSG_MAX for compatibility reasons.
 */
#define SOUND_MAX MSG_MAX

/*
 * Maximum number of macro trigger names
 */
#define MAX_MACRO_TRIGGER 200
#define MAX_MACRO_MOD 12

/*
 * Hack -- attempt to reduce various values in "lite" version
 */
#ifdef ANGBAND_LITE
# undef MACRO_MAX
# define MACRO_MAX	128
# undef QUARK_MAX
# define QUARK_MAX	128
# undef MESSAGE_MAX
# define MESSAGE_MAX	128
# undef MESSAGE_BUF
# define MESSAGE_BUF	4096
#endif

/*
 * Available graphic modes
 */
enum
{
        GRAPHICS_NONE,
	GRAPHICS_PSEUDO,
        GRAPHICS_ORIGINAL,
        GRAPHICS_ADAM_BOLT,
        GRAPHICS_DAVID_GERVAIS
};

/*
 * List of commands that will be auto-repeated
 *
 * ToDo: This string should be user-configurable.
 */
#define AUTO_REPEAT_COMMANDS "TBDoc+"

/*
 * Given an array, determine how many elements are in the array.
 */
#define N_ELEMENTS(a) (sizeof(a) / sizeof((a)[0]))


/**** Some types ****/

/*
 * Note that "char" may or may not be signed, and that "signed char"
 * may or may not work on all machines.  So always use "s16b" or "s32b"
 * for signed values.  Also, note that unsigned values cause math problems
 * in many cases, so try to only use "u16b" and "u32b" for "bit flags",
 * unless you really need the extra bit of information, or you really
 * need to restrict yourself to a single byte for storage reasons.
 *
 * Also, if possible, attempt to restrict yourself to sub-fields of
 * known size (use "s16b" or "s32b" instead of "int", and "byte" instead
 * of "bool"), and attempt to align all fields along four-byte words, to
 * optimize storage issues on 32-bit machines.  Also, avoid "bit flags"
 * since these increase the code size and slow down execution.  When
 * you need to store bit flags, use one byte per flag, or, where space
 * is an issue, use a "byte" or "u16b" or "u32b", and add special code
 * to access the various bit flags.
 *
 * Many of these structures were developed to reduce the number of global
 * variables, facilitate structured program design, allow the use of ascii
 * template files, simplify access to indexed data, or facilitate efficient
 * clearing of many variables at once.
 *
 * Note that certain data is saved in multiple places for efficient access,
 * and when modifying the data in one place it must also be modified in the
 * other places, to prevent the creation of inconsistant data.
 */

/*
 * An array of 256 byte's
 */
typedef byte byte_256[256];

/*
 * An array of 256 u16b's
 */
typedef u16b u16b_256[256];

/*
 * Information about maximal indices of certain arrays
 * Actually, these are not the maxima, but the maxima plus one
 */
struct maxima
{
	u32b fake_text_size;
	u32b fake_name_size;

	u16b f_max;		/* Max size for "f_info[]" */
	u16b k_max;		/* Max size for "k_info[]" */
	u16b a_max;		/* Max size for "a_info[]" */
	u16b e_max;		/* Max size for "e_info[]" */
	u16b r_max;		/* Max size for "r_info[]" */
	u16b v_max;		/* Max size for "v_info[]" */
	u16b p_max;		/* Max size for "p_info[]" */
	u16b h_max;		/* Max size for "h_info[]" */
	u16b b_max;		/* Max size per element of "b_info[]" */
	u16b c_max;		/* Max size for "c_info[]" */
	u16b q_max;		/* Max size for "q_info[]" */
	u16b flavor_max; /* Max size for "flavor_info[]" */
	u16b u_max;		/* Max size for "u_info[]" */

	u16b o_max;		/* Max size for "o_list[]" */
	u16b m_max;		/* Max size for "mon_list[]" */
};

/*
 * Semi-Portable High Score List Entry (128 bytes)
 *
 * All fields listed below are null terminated ascii strings.
 *
 * In addition, the "number" fields are right justified, and
 * space padded, to the full available length (minus the "null").
 *
 * Note that "string comparisons" are thus valid on "pts".
 */

struct high_score
{
	char what[8];		/* Version info (string) */

	char pts[10];		/* Total Score (number) */

	char gold[10];		/* Total Gold (number) */

	char turns[10];		/* Turns Taken (number) */

	char day[10];		/* Time stamp (string) */

	char who[16];		/* Player Name (string) */

	char uid[8];		/* Player UID (number) */

	char sex[2];		/* Player Sex (string) */
	char p_r[3];		/* Player Race (number) */
	char p_c[3];		/* Player Class (number) */

	char cur_lev[4];	/* Current Player Level (number) */
	char cur_dun[4];	/* Current Dungeon Level (number) */
	char max_lev[4];	/* Max Player Level (number) */
	char max_dun[4];	/* Max Dungeon Level (number) */

	char how[32];		/* Method of death (string) */
};


/**** Variables. Lots of them ****/

extern int  max_macrotrigger;
extern cptr macro_template;
extern cptr macro_modifier_chr;
extern cptr macro_modifier_name[MAX_MACRO_MOD];
extern cptr macro_trigger_name[MAX_MACRO_TRIGGER];
extern cptr macro_trigger_keycode[2][MAX_MACRO_TRIGGER];

extern const char hexsym[16];
extern cptr color_names[16];
extern cptr window_flag_desc[32];

extern cptr copyright;
extern byte version_major;
extern byte version_minor;
extern byte version_patch;
extern byte version_extra;
extern byte sf_major;
extern byte sf_minor;
extern byte sf_patch;
extern byte sf_extra;
extern u32b sf_xtra;
extern u32b sf_when;
extern u16b sf_lives;
extern u16b sf_saves;
extern bool arg_fiddle;
extern bool arg_wizard;
extern bool arg_sound;
extern bool arg_graphics;
extern bool arg_force_original;
extern bool arg_force_roguelike;
extern bool character_generated;
extern bool character_dungeon;
extern bool character_loaded;
extern bool character_saved;
extern s16b character_icky;
extern s16b character_xtra;
extern u32b seed_randart;
extern u32b seed_flavor;
extern u32b seed_town;
extern s16b num_repro;
extern s16b object_level;
extern s16b monster_level;
extern char summon_kin_type;
extern s32b turn;
extern s32b old_turn;
extern bool use_sound;
extern int use_graphics;
extern s16b image_count;
extern bool use_bigtile;
extern s16b signal_count;
extern bool msg_flag;
extern bool inkey_base;
extern bool inkey_xtra;
extern bool inkey_scan;
extern bool inkey_flag;
extern s16b coin_type;
extern bool shimmer_monsters;
extern bool shimmer_objects;
extern bool repair_mflag_mark;
extern bool repair_mflag_show;
extern byte feeling;
extern s16b rating;
extern bool good_item_flag;
extern bool closing_flag;
extern int player_uid;
extern int player_euid;
extern int player_egid;
extern char savefile[1024];
extern s16b macro__num;
extern cptr *macro__pat;
extern cptr *macro__act;
extern term *angband_term[ANGBAND_TERM_MAX];
extern char angband_term_name[ANGBAND_TERM_MAX][16];
extern byte angband_color_table[256][4];
extern const cptr angband_sound_name[MSG_MAX];
extern maxima *z_info;
extern byte misc_to_attr[256];
extern char misc_to_char[256];
extern byte tval_to_attr[128];
extern char macro_buffer[1024];
extern cptr keymap_act[KEYMAP_MODES][256];
extern cptr ANGBAND_SYS;
extern cptr ANGBAND_GRAF;
extern cptr ANGBAND_DIR;
extern cptr ANGBAND_DIR_APEX;
extern cptr ANGBAND_DIR_BONE;
extern cptr ANGBAND_DIR_DATA;
extern cptr ANGBAND_DIR_EDIT;
extern cptr ANGBAND_DIR_FILE;
extern cptr ANGBAND_DIR_HELP;
extern cptr ANGBAND_DIR_INFO;
extern cptr ANGBAND_DIR_SAVE;
extern cptr ANGBAND_DIR_PREF;
extern cptr ANGBAND_DIR_USER;
extern cptr ANGBAND_DIR_XTRA;
extern cptr ANGBAND_DIR_SCRIPT;
extern bool item_tester_full;
extern byte item_tester_tval;
extern char *note_base;
extern char *note_cur;

extern bool (*item_tester_hook)(const object_type*);
extern bool (*ang_sort_comp)(const void *u, const void *v, int a, int b);
extern void (*ang_sort_swap)(void *u, void *v, int a, int b);
extern bool (*get_mon_num_hook)(int r_idx);
extern bool (*get_obj_num_hook)(int k_idx);
extern void (*object_info_out_flags)(const object_type *o_ptr, u32b *f1, u32b *f2, u32b *f3, u32b *f4);
extern FILE *text_out_file;
extern void (*text_out_hook)(byte a, cptr str);
extern int text_out_wrap;
extern int text_out_indent;
extern bool use_transparency;
extern byte recent_failed_thefts;
extern byte num_trap_on_level;

/* files.c */
void html_screenshot(cptr name);
void safe_setuid_drop(void);
void safe_setuid_grab(void);
s16b tokenize(char *buf, s16b num, char **tokens);
errr process_pref_file_command(char *buf);
errr process_pref_file(cptr name);
void player_flags(u32b *f1, u32b *f2, u32b *f3, u32b *f4);
void display_player(int mode);
errr file_character(cptr name, bool full);
bool show_file(cptr name, cptr what, int line, int mode);
void do_cmd_help(void);
void process_player_name(bool sf);
void get_name(void);
void do_cmd_suicide(void);
void do_cmd_save_game(void);
void show_scores(void);
void display_scores(int from, int to);
void close_game(void);
void exit_game_panic(void);
#ifdef HANDLE_SIGNALS
extern void (*(*signal_aux)(int, void (*)(int)))(int);
#endif /* HANDLE_SIGNALS */
void signals_ignore_tstp(void);
void signals_handle_tstp(void);
void signals_init(void);

/* init.c */
void init_file_paths(char *path);
void init_angband(void);
void cleanup_angband(void);

/* load.c */
bool load_player(void);

/* save.c */
bool save_player(void);

/* util.c */
errr path_parse(char *buf, size_t max, cptr file);
errr path_build(char *buf, size_t max, cptr path, cptr file);
FILE *my_fopen(cptr file, cptr mode);
FILE *my_fopen_temp(char *buf, size_t max);
errr my_fclose(FILE *fff);
errr my_fgets(FILE *fff, char *buf, size_t n);
errr my_fputs(FILE *fff, cptr buf, size_t n);
errr fd_kill(cptr file);
errr fd_move(cptr file, cptr what);
errr fd_copy(cptr file, cptr what);
int fd_make(cptr file, int mode);
int fd_open(cptr file, int flags);
errr fd_lock(int fd, int what);
errr fd_seek(int fd, long n);
errr fd_read(int fd, char *buf, size_t n);
errr fd_write(int fd, cptr buf, size_t n);
errr fd_close(int fd);
errr check_modification_date(int fd, cptr template_file);
void text_to_ascii(char *buf, size_t len, cptr str);
void ascii_to_text(char *buf, size_t len, cptr str);
int macro_find_exact(cptr pat);
errr macro_add(cptr pat, cptr act);
errr macro_init(void);
errr macro_free(void);
errr macro_trigger_free(void);
void flush(void);
void flush_fail(void);
char inkey(void);
void bell(cptr reason);
void sound(int val);
s16b quark_add(cptr str);
cptr quark_str(s16b i);
errr quarks_init(void);
errr quarks_free(void);
s16b message_num(void);
cptr message_str(s16b age);
u16b message_type(s16b age);
byte message_color(s16b age);
errr message_color_define(u16b type, byte color);
void message_add(cptr str, u16b type);
errr messages_init(void);
void messages_free(void);
void move_cursor(int row, int col);
void msg_print(cptr msg);
void msg_format(cptr fmt, ...);
void message(u16b message_type, s16b extra, cptr message);
void message_format(u16b message_type, s16b extra, cptr fmt, ...);
void message_flush(void);
void screen_save(void);
void screen_load(void);
void c_put_str(byte attr, cptr str, int row, int col);
void put_str(cptr str, int row, int col);
void c_prt(byte attr, cptr str, int row, int col);
void prt(cptr str, int row, int col);
void text_out_to_file(byte attr, cptr str);
void text_out_to_screen(byte a, cptr str);
void text_out(cptr str);
void text_out_c(byte a, cptr str);
void clear_from(int row);
bool askfor_aux(char *buf, size_t len);
bool get_string(cptr prompt, char *buf, size_t len);
s16b get_quantity(cptr prompt, int max);
bool get_check(cptr prompt);
bool get_com(cptr prompt, char *command);
void pause_line(int row);
void request_command(bool shopping);
int damroll(int num, int sides);
bool is_a_vowel(int ch);
int color_char_to_attr(char c);
int color_text_to_attr(cptr name);
cptr attr_to_text(byte a);
void note_printf(cptr fmt, ...);
void repeat_push(int what);
bool repeat_pull(int *what);
void repeat_clear(void);
void repeat_check(void);

void build_gamma_table(int gamma);
extern byte gamma_table[256];

extern byte get_angle_to_grid[41][41];
int get_angle_to_target(int y0, int x0, int y1, int x1, int dir);
void get_grid_using_angle(int angle, int y0, int x0,
	int *ty, int *tx);

void make_random_name(char *random_name, size_t max);

/* wizard.c */
void do_cmd_debug(void);

/* spoilers.c */
void do_cmd_spoilers(void);

/*
 * Hack -- conditional (or "bizarre") externs
 */

/* util.c */
#ifdef SET_UID
void user_name(char *buf, size_t len, int id);
#endif /* SET_UID */

#ifdef RISCOS
/* main-ros.c */
char *riscosify_name(cptr path);
#endif /* RISCOS */

#if defined(MAC_MPW) || defined(MACH_O_CARBON)
/* main-mac.c, or its derivatives */
extern u32b _fcreator;
extern u32b _ftype;
# if defined(MAC_MPW) && defined(CARBON)
void convert_pathname(char *path);
# endif
# if defined(MACH_O_CARBON)
void fsetfileinfo(cptr path, u32b fcreator, u32b ftype);
# endif
#endif

/* These should be normally on.  */
#define USE_GRAPHICS
#define USE_SOUND

#endif /* MISC_H_INCLUDED */
