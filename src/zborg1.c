/* File: borg1.c */
/* Purpose: Low level stuff for the Borg -BEN- */
#include "angband.h"


#ifdef ALLOW_BORG

#include "zborg1.h"

/*
 * if a borg has no light and no money to buy fuel, sell some
 *    in order to raise cash.
 * Inscription problem with long art names
 *
 * Mimic Doors.
 *
 * getting to a wilderness grid that has a town.
 *   using waypoints?
 * Using dimdoor to get accross lava and water.
 */

/* Things that would be nice for the borg to know but would
 * require a bit of programming and may not be worth the time.
 *
 * - Aquatic animals stay in aquatic realms.
 * - Wraithform
 * - Summoning
 */

/*
 * This file contains various low level variables and routines.
 */

/* Dynamic borg stuff */
borg_player *bp_ptr;

/*
 * Some variables
 */

bool borg_active;	/* Actually active */

bool borg_cancel;	/* Being cancelled */

bool borg_dont_react = FALSE;
int successful_target = BORG_TARGET;

/*
 * Various silly flags
 */


bool borg_stop_king = TRUE;		/* The borg stops when he wins */
bool borg_cheat_death = FALSE;	/* Is there life after death? */
bool borg_flag_dump = FALSE;	/* Save savefile at each death */
bool borg_flag_save = FALSE;	/* Save savefile at each level */
bool borg_save = FALSE;			/* Do a save next level */

/*
 * Use a simple internal random number generator
 */


u32b borg_rand_local;	/* Save personal setting */


/*
 * Hack -- Time variables
 */

s32b borg_t = 0L;	/* Current "time" */
s32b borg_temp_fill_valid = 0L;	/* When were the monster arrays filled */
s32b need_see_inviso = 0;	/* cast this when required */
s32b borg_see_inv = 0;
bool vault_on_level;	/* Borg will search for a vault */
bool unique_on_level;
int  unique_r_idx;
bool scaryguy_on_level;	/* flee from certain guys */

bool breeder_level = FALSE;	/* Borg will shut door */
s16b old_depth = 128;
s16b borg_no_retreat = 0;

/*
 * Hack -- Other time variables
 */

s32b when_call_lite = 0;	/* When we last did call light */
s32b when_wizard_lite = 0;	/* When we last did wizard light */

s32b when_detect_traps = 0;	/* When we last detected traps */
s32b when_detect_doors = 0;	/* When we last detected doors */
s32b when_detect_walls = 0;	/* When we last detected walls */
s32b when_detect_evil = 0;	/* When we last detected monsters or evil */

bool my_need_alter;	/* incase i hit a wall or door */
bool my_no_alter;	/*  */

/*
 * Some information
 */

s16b goal;	/* Goal type */

bool goal_rising;	/* Currently returning to town */

bool goal_leaving;	/* Currently leaving the level */

bool goal_fleeing;	/* Currently fleeing the level */

bool goal_ignoring;	/* Currently ignoring monsters */

int goal_recalling;	/* Currently waiting for recall, guessing the turns left */

bool goal_less;	/* return to, but dont use, the next up stairs */

s16b borg_times_twitch;	/* how often twitchy on this level */
s16b borg_escapes;	/* how often teleported on this level */

bool stair_less;	/* Use the next "up" staircase */
bool stair_more;	/* Use the next "down" staircase */

s32b borg_began;	/* When this level began */
s32b borg_time_town;	/* how long it has been since I was in town */

s16b avoidance = 0;	/* Current danger thresh-hold */

bool borg_failure;	/* Notice failure */

bool borg_attacking;	/* Simulation flag */
bool borg_offsetting;	/* offset ball attacks */

bool borg_completed;	/* Completed the level */

bool borg_needs_searching;	/* borg will search with each step */
bool borg_full_damage;	/* make danger = full possible damage. */

/* defence flags */
bool borg_prot_from_evil;
bool borg_speed;
bool borg_bless;
bool borg_hero;
bool borg_berserk;
bool my_oppose_fire;
bool my_oppose_cold;
bool my_oppose_acid;
bool my_oppose_pois;
bool my_oppose_elec;
s16b borg_wraith_form;
s16b borg_goi;
s16b borg_inviso;
bool borg_esp;

s16b borg_game_ratio;	/* the ratio of borg time to game time */

bool borg_shield;
bool borg_on_glyph;
bool borg_create_door;
bool borg_open_door_failed;
bool borg_close_door_failed;
bool borg_sleep_spell;
bool borg_sleep_spell_ii;
bool borg_slow_spell;
bool borg_confuse_spell;
bool borg_fear_mon_spell;

/* Which shop or dungeon to visit next */
s16b goal_town = -1;
s16b goal_shop = -1;
s16b goal_dungeon = -1;
s16b goal_explore_x = -1;
s16b goal_explore_y = -1;

/* Current shop/dungeon index */
s16b town_num = -1;
s16b shop_num = -1;
s16b dungeon_num = -1;

/* List of known shops and dungeons */
borg_town *borg_towns;
borg_shop *borg_shops;
borg_dungeon *borg_dungeons;

/* Number of allocated towns */
s16b borg_town_num = 0;
s16b borg_town_size = 20;

/* Number of allocated shops */
s16b borg_shop_num = 0;
s16b borg_shop_size = 16;

/* Number of allocated dungeons */
s16b borg_dungeon_num = 0;
s16b borg_dungeon_size = 16;


/*
 * Location variables
 */

int c_x;	/* Current location (X) */
int c_y;	/* Current location (Y) */

int g_x;	/* Goal location (X) */
int g_y;	/* Goal location (Y) */

s32b g_power;		/* Current power value */
s32b g_power_home;	/* Current power_home value */

int dim_door_y;	/* Safe landing zone for DDoor */
int dim_door_x;

/* BIG HACK! Assume only 50 cursed artifacts */
int bad_obj_x[50];	/* Dropped cursed artifact at location (X) */
int bad_obj_y[50];	/* Dropped cursed artifact at location (Y) */
int bad_obj_n = -1;

/*
 * Some estimated state variables
 */

s16b my_stat_max[6];	/* Current "maximal" stat values */
s16b my_stat_cur[6];	/* Current "natural" stat values */
s16b my_stat_ind[6];	/* Current "additions" to stat values */
bool my_need_stat_check[6];	/* do I need to check my stats? */

s16b my_stat_add[6];	/* additions to stats  This will allow upgrading of */
					  /* equipment to allow a ring of int +4 to be traded */
					  /* for a ring of int +6 even if maximized to allow a */
					  /* later swap to be better. */

s16b home_stat_add[6];

int my_ammo_tval;	/* Ammo -- "tval" */
s16b my_ammo_power;	/* Average power */
s16b my_ammo_range;	/* Shooting range */


/*
 * Various "amounts" (for the player)
 */

s16b amt_food_scroll;
s16b amt_food_lowcal;
s16b amt_torch;
s16b amt_lantern;
s16b amt_flask;

s16b amt_slow_poison;
s16b amt_pot_curing;
s16b amt_star_heal;
s16b amt_life;
s16b amt_rod_heal;

s16b amt_book[8][4];	/* [realm][sval] */

s16b amt_add_stat[6];
s16b amt_fix_stat[7];	/* #7 is to fix all stats */
s16b amt_fix_exp;

s16b amt_enchant_to_a;
s16b amt_enchant_to_d;
s16b amt_enchant_to_h;
s16b amt_brand_weapon;	/* apw brand bolts */
s16b amt_digger;


/*
 * Hack -- extra state variables
 */

int borg_feeling = 0;	/* Current level "feeling" */


/*
 * State variables extracted from the screen
 */

s32b borg_gold;	/* Current gold */

int borg_stat[6];	/* Current stat values */

int borg_book[8][4];	/* Current book slots, [realm][sval] */


/*
 * Constant state variables
 */

int borg_race;	/* Player race */
int borg_class;	/* Player class */


/*
 * Hack -- access the class/race records
 */

player_race *rb_ptr;	/* Player race info */
player_class *cb_ptr;	/* Player class info */

player_magic *pmb_ptr;	/* Player magic info */



/*
 * Number of turns to step for (zero means forever)
 */
u16b borg_step = 0;	/* Step count (if any) */

/*
 * Status message search string
 */
char borg_match[128] = "";

/*
 * Log file
 */
FILE *borg_fff = NULL;	/* Log file */


/*
 * Track "stairs up"
 */
s16b track_less_num = 0;
s16b track_less_size = 16;
int *track_less_x;
int *track_less_y;


/*
 * Track "stairs down"
 */
s16b track_more_num = 0;
s16b track_more_size = 16;
int *track_more_x;
int *track_more_y;

/*
 * Track glyphs
 */
s16b track_glyph_num = 0;
s16b track_glyph_size = 256;
int *track_glyph_x;
int *track_glyph_y;

/*
 * Track Steps
 */
s16b track_step_num = 0;
s16b track_step_size = 256;
int *track_step_x;
int *track_step_y;

/*
 * Track closed doors
 */
s16b track_door_num = 0;
s16b track_door_size = 256;
int *track_door_x;
int *track_door_y;

/*
 * The object list.  This list is used to "track" objects.
 */
s16b borg_takes_cnt = 0;
s16b borg_takes_nxt = 1;
borg_take *borg_takes;


/*
 * The monster list.  This list is used to "track" monsters.
 */
s16b borg_kills_cnt = 0;
s16b borg_kills_nxt = 1;
borg_kill *borg_kills;

/*
 * Maintain a set of grids marked as "BORG_VIEW"
 */

s16b borg_view_n = 0;

s16b *borg_view_x;
s16b *borg_view_y;


/*
 * Maintain a temporary set of grids
 */

/* For any monster within MAX_RANGE */
s16b borg_temp_n = 0;
s16b *borg_temp_x;
s16b *borg_temp_y;

/* For the monsters immediately surrounding the borg */
s16b borg_next_n = 0;
s16b *borg_next_x;
s16b *borg_next_y;

/* For the monsters that can be hit by a bolt */
s16b borg_bolt_n = 0;
s16b *borg_bolt_x;
s16b *borg_bolt_y;

/* For the monsters that can be hit by a beam, basically any monster in LOS */
s16b borg_beam_n = 0;
s16b *borg_beam_x;
s16b *borg_beam_y;

/* For the monsters that can be hit by a ball with radius > 1 */
s16b borg_ball_n = 0;
s16b *borg_ball_x;
s16b *borg_ball_y;


/*
 * Maintain a circular queue of grids
 */

s16b borg_flow_n = 0;

s16b *borg_flow_x;
s16b *borg_flow_y;


/*
 * Hack -- use "flow" array as a queue
 */

int flow_head = 0;
int flow_tail = 0;



/*
 * Strategy flags -- examine the world
 */
bool borg_do_frame = TRUE;	/* Acquire "frame" info */

bool borg_do_spell = TRUE;	/* Acquire "spell" info */


/*
 * Strategy flags -- run certain functions
 */

bool borg_do_destroy = FALSE;

/* am I fighting a unique? */
int borg_fighting_unique;
bool borg_fighting_evil_unique;



/*
 * Query the "attr/char" at a given location on the screen
 * We return "zero" if the given location was legal
 *
 * XXX XXX XXX We assume the given location is legal
 */
errr borg_what_char(int x, int y, byte *a, char *c)
{
	/* Direct access XXX XXX XXX */
	(*a) = (Term->scr->a[y][x]);
	(*c) = (Term->scr->c[y][x]);

	/* Success */
	return (0);
}


/*
 * Query the "attr/chars" at a given location on the screen
 *
 * Note that "a" points to a single "attr", and "s" to an array
 * of "chars", into which the attribute and text at the given
 * location are stored.
 *
 * We will not grab more than "ABS(n)" characters for the string.
 * If "n" is "positive", we will grab exactly "n" chars, or fail.
 * If "n" is "negative", we will grab until the attribute changes.
 *
 * We automatically convert all "blanks" and "invisible text" into
 * spaces, and we ignore the attribute of such characters.
 *
 * We do not strip final spaces, so this function will very often
 * read characters all the way to the end of the line.
 *
 * We succeed only if a string of some form existed, and all of
 * the non-space characters in the string have the same attribute,
 * and the string was long enough.
 *
 * XXX XXX XXX We assume the given location is legal
 */
errr borg_what_text(int x, int y, int n, byte *a, char *s)
{
	int i;

	byte t_a;
	char t_c;

	byte *aa;
	char *cc;

	/* Current attribute */
	byte d_a = 0;

	/* Max length to scan for */
	int m = ABS(n);

	/* Hack -- Do not run off the screen */
	if (x + m > 80) m = 80 - x;

	/* Direct access XXX XXX XXX */
	aa = &(Term->scr->a[y][x]);
	cc = &(Term->scr->c[y][x]);

	/* Grab the string */
	for (i = 0; i < m; i++)
	{
		/* Access */
		t_a = *aa++;
		t_c = *cc++;

		/* Handle spaces */
		if ((t_c == ' ') || !t_a)
		{
			/* Save space */
			s[i] = ' ';
		}

		/* Handle real text */
		else
		{
			/* Attribute ready */
			if (d_a)
			{
				/* Verify the "attribute" (or stop) */
				if (t_a != d_a) break;
			}

			/* Acquire attribute */
			else
			{
				/* Save it */
				d_a = t_a;
			}

			/* Save char */
			s[i] = t_c;
		}
	}

	/* Terminate the string */
	s[i] = '\0';

	/* Save the attribute */
	(*a) = d_a;

	/* Too short */
	if ((n > 0) && (i != n)) return (1);

	/* Success */
	return (0);
}

/* Compare what you get from borg_what_text immediately */
bool borg_term_text_comp(int x, int y, cptr what)
{
	byte t_a;
	int wid, hgt;
	int len = strlen(what);
	char buf[120];

	/* Get size */
	Term_get_size(&wid, &hgt);

	/* That's left or right of the term */
	if (x < 0 || x + len > wid) return (FALSE);

	/* That's higher or lower of the term */
	if (y < 0 || y >= hgt) return (FALSE);

	if (0 == borg_what_text(x, y, strlen(what), &t_a, buf) &&
		streq(buf, what)) return (TRUE);

	/* No match */
	return (FALSE);
}



/*
 * Memorize a message, Log it, Search it, and Display it in pieces
 */
static void borg_note_aux(cptr what)
{
	int j, n, i, k;

	int w, h, x, y;


	term *old = Term;

	/* Memorize it */
	message_add(what, MSG_GENERIC);

	/* Log the message */
	if (borg_fff) froff(borg_fff, "%s\n", what);

	/* Mega-Hack -- Check against the search string */
	if (borg_match[0] && strstr(what, borg_match))
	{
		/* Tell the user why you quit */
		borg_oops("Search string was matched");
	}

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		if (!angband_term[j]) continue;

		/* Check flag */
		if (!(window_flag[j] & PW_BORG_1)) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Access size */
		Term_get_size(&w, &h);

		/* Access cursor */
		Term_locate(&x, &y);

		/* Erase current line */
		clear_row(y);


		/* Total length */
		n = strlen(what);

		/* Too long */
		if (n > w - 2)
		{
			char buf[1024];

			/* Split */
			while (n > w - 2)
			{
				/* Default */
				k = w - 2;

				/* Find a split point */
				for (i = w / 2; i < w - 2; i++)
				{
					/* Pre-emptive split point */
					if (isspace(what[i])) k = i;
				}

				/* Copy over the split message */
				for (i = 0; i < k; i++)
				{
					/* Copy */
					buf[i] = what[i];
				}

				/* Indicate split */
				buf[i++] = '\\';

				/* Terminate */
				buf[i] = '\0';

				/* Show message */
				roff(buf);

				/* Advance (wrap) */
				if (++y >= h) y = 0;

				/* Erase next line */
				clear_row(y);

				/* Advance */
				what += k;

				/* Reduce */
				n -= k;
			}

			/* Show message tail */
			roff(what);

			/* Advance (wrap) */
			if (++y >= h) y = 0;

			/* Erase next line */
			clear_row(y);
		}

		/* Normal */
		else
		{
			/* Show message */
			roff(what);

			/* Advance (wrap) */
			if (++y >= h) y = 0;

			/* Erase next line */
			clear_row(y);
		}


		/* Flush output */
		Term_fresh();

		/* Use correct window */
		Term_activate(old);
	}
}


/* Do a message with formatting */
void borg_note(cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, 1024, fmt, &vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	borg_note_aux(buf);
}


/*
 * Abort the Borg, noting the reason
 */
static void borg_oops_aux(cptr what)
{
	char buf[1024];

	/* Stop processing */
	borg_active = FALSE;

	/* Format the string */
	(void)strnfmt(buf, 1024, "# Aborting (%s).", what);

	/* Give a warning */
	borg_note(buf);

	/* Forget borg keys */
	borg_flush();
}


/* Abort the borg, give a text with formatting */
void borg_oops(cptr fmt, ...)
{
	va_list vp;

	char buf[1024];

	/* Begin the Varargs Stuff */
	va_start(vp, fmt);

	/* Format the args, save the length */
	(void)vstrnfmt(buf, 1024, fmt, &vp);

	/* End the Varargs Stuff */
	va_end(vp);

	/* Display */
	borg_oops_aux(buf);
}


/*
 * A Queue of keypresses to be sent
 */
static char *borg_key_queue;
static s16b borg_key_head;
static s16b borg_key_tail;


/*
 * Add a keypress to the "queue" (fake event)
 */
errr borg_keypress(char k)
{
	char buf[10];

	/* Hack -- Refuse to enqueue "nul" */
	if (!k) return (-1);

	/* Format the string */
	(void)strnfmt(buf, 10, "& Key <%c>", k);

	/* Hack -- note the keypress */
	if (borg_fff) froff(borg_fff, "%s\n", buf);

	/* Store the char, advance the queue */
	borg_key_queue[borg_key_head++] = k;

	/* Circular queue, handle wrap */
	if (borg_key_head == KEY_SIZE) borg_key_head = 0;

	/* Hack -- Catch overflow (forget oldest) */
	if (borg_key_head == borg_key_tail) borg_oops("overflow");

	/* Hack -- Overflow may induce circular queue */
	if (borg_key_tail == KEY_SIZE) borg_key_tail = 0;

	/* Success */
	return (0);
}


/*
 * Add a keypress to the "queue" (fake event)
 */
errr borg_keypresses(cptr str)
{
	cptr s;

	/* Enqueue them */
	for (s = str; *s; s++) borg_keypress(*s);

	/* Success */
	return (0);
}


/*
 * Get the next Borg keypress
 */
char borg_inkey(bool take)
{
	int i;

	/* Nothing ready */
	if (borg_key_head == borg_key_tail)
		return (0);

	/* Extract the keypress */
	i = borg_key_queue[borg_key_tail];

	/* Do not advance */
	if (!take) return (i);

	/* Advance the queue */
	borg_key_tail++;

	/* Circular queue requires wrap-around */
	if (borg_key_tail == KEY_SIZE) borg_key_tail = 0;

	/* Return the key */
	return (i);
}



/*
 * Get the next Borg keypress
 */
void borg_flush(void)
{
	/* Simply forget old keys */
	borg_key_tail = borg_key_head;
}






/*
 * Hack -- take a note later
 */
bool borg_tell(cptr what)
{
	cptr s;

	/* Hack -- self note */
	borg_keypress(':');
	for (s = what; *s; s++) borg_keypress(*s);
	borg_keypress('\n');

	/* Success */
	return (TRUE);
}



/*
 * Attempt to change the borg's name
 */
bool borg_change_name(cptr str)
{
	cptr s;

	/* Cancel everything */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Character description */
	borg_keypress('C');

	/* Change the name */
	borg_keypress('c');

	/* Enter the new name */
	for (s = str; *s; s++) borg_keypress(*s);

	/* End the name */
	borg_keypress('\r');

	/* Cancel everything */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}


/*
 * Attempt to dump a character description file
 */
bool borg_dump_character(cptr str)
{
	cptr s;

	/* Cancel everything */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Character description */
	borg_keypress('C');

	/* Dump character file */
	borg_keypress('f');

	/* Enter the new name */
	for (s = str; *s; s++) borg_keypress(*s);

	/* End the file name */
	borg_keypress('\r');

	/* Cancel everything */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}




/*
 * Attempt to save the game
 */
bool borg_save_game(void)
{
	/* Cancel everything */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Save the game */
	borg_keypress('^');
	borg_keypress('S');

	/* Cancel everything */
	borg_keypress(ESCAPE);
	borg_keypress(ESCAPE);

	/* Success */
	return (TRUE);
}


/*
 * Initialize this file
 */
void borg_init_1(void)
{
	/* Allocate the "keypress queue" */
	C_MAKE(borg_key_queue, KEY_SIZE, char);

	/* Prepare a local random number seed */
	if (!borg_rand_local) borg_rand_local = randint0(0x10000000);

	/*** Special "tracking" arrays ***/

	/* Track "up" stairs */
	C_MAKE(track_less_x, track_less_size, int);
	C_MAKE(track_less_y, track_less_size, int);

	/* Track "down" stairs */
	C_MAKE(track_more_x, track_more_size, int);
	C_MAKE(track_more_y, track_more_size, int);

	/* Track glyphs */
	C_MAKE(track_glyph_x, track_glyph_size, int);
	C_MAKE(track_glyph_y, track_glyph_size, int);

	/* Track Steps */
	C_MAKE(track_step_x, track_step_size, int);
	C_MAKE(track_step_y, track_step_size, int);

	/* Track closed doors */
	C_MAKE(track_door_x, track_door_size, int);
	C_MAKE(track_door_y, track_door_size, int);

	/* Array of objects */
	C_MAKE(borg_takes, BORG_TAKES_MAX, borg_take);

	/* Array of monsters */
	C_MAKE(borg_kills, BORG_KILLS_MAX, borg_kill);

	/* Array of views */
	C_MAKE(borg_view_x, AUTO_VIEW_MAX, s16b);
	C_MAKE(borg_view_y, AUTO_VIEW_MAX, s16b);

	/* Array of temporary coordinates */
	C_MAKE(borg_temp_x, BORG_TEMP_MAX, s16b);
	C_MAKE(borg_temp_y, BORG_TEMP_MAX, s16b);

	/* Array of temporary coordinates */
	C_MAKE(borg_next_x, BORG_NEXT_MAX, s16b);
	C_MAKE(borg_next_y, BORG_NEXT_MAX, s16b);

	/* Array of temporary coordinates */
	C_MAKE(borg_bolt_x, BORG_TEMP_MAX, s16b);
	C_MAKE(borg_bolt_y, BORG_TEMP_MAX, s16b);

	/* Array of temporary coordinates */
	C_MAKE(borg_beam_x, BORG_TEMP_MAX, s16b);
	C_MAKE(borg_beam_y, BORG_TEMP_MAX, s16b);

	/* Array of temporary coordinates */
	C_MAKE(borg_ball_x, BORG_TEMP_MAX, s16b);
	C_MAKE(borg_ball_y, BORG_TEMP_MAX, s16b);

	/* Array of temporary coordinates */
	C_MAKE(borg_flow_x, BORG_FLOW_MAX, s16b);
	C_MAKE(borg_flow_y, BORG_FLOW_MAX, s16b);


	/* Struct for the player information */
	MAKE(bp_ptr, borg_player);
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
