/* File: borg1.c */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

#include "angband.h"

#include "borg.h"


#ifdef ALLOW_BORG


/*
 * This file contains various low level variables and routines.
 */




/*
 * Control variables
 */

s16b borg_active = 0;		/* Activation level */

bool borg_prompt = FALSE;	/* Prompt/Message available */

char *borg_key_queue;	/* Keypress queue */
s16b borg_key_head;		/* Head of queue */
s16b borg_key_tail;		/* Tail of queue */

FILE *borg_fff = NULL;	/* Log file */

char borg_match_string[128] = "";	/* Search string */


/*
 * Optional strategy flags
 */

bool borg_flag_save_level = TRUE;	/* Save savefile when level changes */

bool borg_flag_save_depth = TRUE;	/* Save savefile when depth changes */


/*
 * Internal random number generator
 */

bool borg_rand_quick;		/* Save system setting */

u32b borg_rand_value;		/* Save system setting */

u32b borg_rand_local;		/* Save personal setting */


/*
 * Time variables
 */

s16b borg_time = 0L;			/* Current "time" */

s16b borg_when_began;			/* When this level began */

s16b borg_when_call_lite;		/* When we last did call light */
s16b borg_when_wizard_lite;		/* When we last did wizard light */

s16b borg_when_detect_traps;	/* When we last detected traps */
s16b borg_when_detect_doors;	/* When we last detected doors */
s16b borg_when_detect_walls;	/* When we last detected walls */


/*
 * Some information
 */

s16b borg_task;			/* Current goal type */

s16b borg_avoid;		/* Current danger avoidance */

s16b borg_boost;		/* Current boosted avoidance */

bool borg_rising;		/* Currently returning to town */

bool borg_leaving;		/* Currently leaving the level */

bool borg_fleeing;		/* Currently fleeing the level */

bool borg_ignoring;		/* Currently ignoring monsters */

bool borg_recalling;	/* Currently waiting for recall */

bool borg_completed;	/* Currently bored with dungeon */

bool borg_stair_less;	/* Use the next "up" staircase */
bool borg_stair_more;	/* Use the next "down" staircase */

s16b borg_goal_shop = -1;		/* Next shop to visit */
s16b borg_goal_ware = -1;		/* Next item to buy there */
s16b borg_goal_item = -1;		/* Next item to sell there */

s16b borg_exam_item = -1;		/* Hack -- examine item -- index */

char borg_exam_note[32];		/* Hack -- examine item -- note */

bool borg_need_save;	/* Borg needs to save the game */


/*
 * Various "amounts" (for the player)
 */

sint amt_fuel;
sint amt_food;
sint amt_ident;
sint amt_recall;
sint amt_phase;
sint amt_escape;
sint amt_teleport;

sint amt_cure_critical;
sint amt_cure_serious;

sint amt_detect_trap;
sint amt_detect_door;

sint amt_missile;

sint amt_book[9];

sint amt_add_stat[6];
sint amt_fix_stat[6];
sint amt_fix_exp;

sint amt_enchant_to_a;
sint amt_enchant_to_d;
sint amt_enchant_to_h;


/*
 * Various "amounts" (for the home)
 */

sint num_fuel;
sint num_food;
sint num_ident;
sint num_recall;
sint num_phase;
sint num_escape;
sint num_teleport;

sint num_cure_critical;
sint num_cure_serious;

sint num_missile;

sint num_book[9];

sint num_fix_stat[6];

sint num_fix_exp;

sint num_enchant_to_a;
sint num_enchant_to_d;
sint num_enchant_to_h;


/*
 * State variables extracted from various places
 */

int borg_base_depth;		/* Actual dungeon "level" */

int borg_base_level;		/* Actual player level */

s32b borg_base_exp;			/* Actual experience */

s32b borg_base_au;			/* Actual gold */

int borg_base_pspeed;		/* Actual speed */

int borg_base_ac;			/* Actual ac */

int borg_base_chp;			/* Actual current hitpoints */
int borg_base_mhp;			/* Actual maximum hitpoints */

int borg_base_csp;			/* Actual current spell points */
int borg_base_msp;			/* Actual maximum spell points */

int borg_base_stat[6];		/* Actual stat values XXX XXX */
bool borg_stat_max[6];		/* Stat at maximum */ 

int borg_base_book[9];		/* Actual book slots XXX XXX */

int borg_base_shop = -1;	/* Actual shop index */

int borg_base_wgt;			/* Actual weight */

bool borg_base_is_weak;		/* Actual flag -- weak */
bool borg_base_is_hungry;	/* Actual flag -- hungry/weak */

bool borg_base_is_full;		/* Actual flag -- full/gorged */
bool borg_base_is_gorged;	/* Actual flag -- gorged */

bool borg_base_is_blind;	/* Actual flag -- blind */
bool borg_base_is_afraid;	/* Actual flag -- afraid */
bool borg_base_is_confused;	/* Actual flag -- confused */
bool borg_base_is_poisoned;	/* Actual flag -- poisoned */

bool borg_base_is_cut;		/* Actual flag -- bleeding */
bool borg_base_is_stun;		/* Actual flag -- stunned */

bool borg_base_is_image;	/* Actual flag -- hallucinating */
bool borg_base_is_study;	/* Actual flag -- may learn spells */

bool borg_base_fix_lev;		/* Actual flag -- Drained LEV */
bool borg_base_fix_exp;		/* Actual flag -- Drained EXP */

bool borg_base_fix_stat[6];	/* Actual flags -- Drained Stats XXX XXX */

int borg_feeling;		/* Recent level "feeling" */

int borg_happy_depth;	/* Favorite dungeon depth */
int borg_happy_count;	/* Number of completions */

s32b borg_base_power;	/* Hack -- base power */

bool borg_failure;		/* Notice failure */

bool borg_simulate;		/* Simulation flag */

bool borg_expected;		/* Expected danger */



/*
 * Some global structures
 */

static player_type borg_player_type_body;

player_type *b_ptr = &borg_player_type_body;

static player_xtra borg_player_xtra_body;

player_xtra *xb_ptr = &borg_player_xtra_body;

player_race *rb_ptr;	/* Player race info */
player_class *cb_ptr;	/* Player class info */

player_magic *mb_ptr;	/* Player magic info */

player_anal *ab_ptr;	/* Player anal info */



/*
 * Hack -- message memory
 */

s16b borg_msg_len;
s16b borg_msg_siz;
char *borg_msg_buf;
s16b borg_msg_num;
s16b borg_msg_max;
s16b *borg_msg_pos;
s16b *borg_msg_use;



/*
 * Hack -- the detection arrays
 */

bool borg_detect_wall[6][6];

bool borg_detect_trap[6][6];

bool borg_detect_door[6][6];


/*
 * Track shops
 */

byte *borg_track_shop_x;
byte *borg_track_shop_y;


/*
 * Track stairs up
 */

s16b borg_track_less_num;
byte *borg_track_less_x;
byte *borg_track_less_y;


/*
 * Track stairs down
 */

s16b borg_track_more_num;
byte *borg_track_more_x;
byte *borg_track_more_y;


/*
 * Track interesting grids
 */

s16b borg_wank_num = 0;

auto_wank *borg_wanks;


/*
 * Track interesting objects
 */

s16b borg_takes_cnt;

s16b borg_takes_nxt;

auto_take borg_takes[BORG_MAX_TAKE];


/*
 * Track interesting monsters
 */

s16b borg_kills_cnt;

s16b borg_kills_nxt;

auto_kill borg_kills[BORG_MAX_KILL];


/*
 * Hack -- extra fear per "region"
 */

u16b borg_fear_region[6][18];


/*
 * Hack -- Array[?] to count racial appearances per level
 */

s16b *borg_race_count;


/*
 * Hack -- Array[?] to count racial kills (for uniques)
 */

s16b *borg_race_death;


/*
 * Array[256] for classification of map symbols (feature).
 */

byte *borg_char_feat;

/*
 * Arrays[256] for classification of map symbols (object/monster).
 */

bool *borg_char_is_take;

bool *borg_char_is_kill;


/*
 * Array[BORG_VIEW_MAX] of grids used by "update_view()".
 */

sint borg_view_n = 0;

u16b *borg_view_g;


/*
 * Arrays[BORG_TEMP_MAX] of grids used for various things.
 *
 * Note that "borg_temp_g" and "borg_temp_y"/"borg_temp_x" share memory.
 */

sint borg_temp_n = 0;

u16b *borg_temp_g;

byte *borg_temp_y;
byte *borg_temp_x;


/*
 * Arrays[BORG_HACK_MAX] of grids used for tracking lit floor grids.
 *
 * Note that "borg_hack_g" and "borg_hack_y"/"borg_hack_x" share memory.
 */

sint borg_hack_n = 0;

u16b *borg_hack_g;

byte *borg_hack_y;
byte *borg_hack_x;


/*
 * Arrays[BORG_FLOW_MAX] of grids used for spreading flow information.
 *
 * Note that "borg_flow_g" and "borg_flow_y"/"borg_flow_x" share memory.
 */

sint borg_flow_head = 0;
sint borg_flow_tail = 0;

u16b *borg_flow_g;

byte *borg_flow_y;
byte *borg_flow_x;


/*
 * Arrays[DUNGEON_HGT][256] of dungeon info flags / feature codes.
 */

byte (*borg_cave_info)[256];
byte (*borg_cave_feat)[256];


/*
 * Arrays[DUNGEON_HGT][DUNGEON_WID] of dungeon objects / monsters.
 */

byte (*borg_cave_o_idx)[DUNGEON_WID];
byte (*borg_cave_m_idx)[DUNGEON_WID];


/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of search counts.
 */

byte (*borg_cave_search)[DUNGEON_WID];


/*
 * Array[DUNGEON_HGT][DUNGEON_WID] of cached danger values.
 */

s16b (*borg_cave_danger)[DUNGEON_WID];


/*
 * Arrays[DUNGEON_HGT][DUNGEON_WID] of active/working flow costs.
 */

byte (*borg_flow_cost)[DUNGEON_WID];
byte (*borg_flow_work)[DUNGEON_WID];




/*
 * Strategy flags -- recalculate things
 */

bool borg_do_wipe_danger = FALSE;	/* Recalculate danger */

bool borg_do_update_view = FALSE;	/* Recalculate view */


/*
 * Strategy flags -- examine the world
 */

bool borg_do_inven = TRUE;	/* Acquire "inven" info */

bool borg_do_equip = TRUE;	/* Acquire "equip" info */

bool borg_do_panel = TRUE;	/* Acquire "panel" info */

bool borg_do_frame = TRUE;	/* Acquire "frame" info */

bool borg_do_spell = TRUE;	/* Acquire "spell" info */

byte borg_do_spell_aux = 0;	/* Hack -- book for "borg_do_spell" */

bool borg_do_browse = 0;	/* Acquire "store" info */

byte borg_do_browse_what = 0;	/* Hack -- store for "borg_do_browse" */

byte borg_do_browse_more = 0;	/* Hack -- pages for "borg_do_browse" */


/*
 * Strategy flags -- run certain functions
 */

bool borg_do_crush_junk = FALSE;

bool borg_do_crush_hole = FALSE;

bool borg_do_crush_slow = FALSE;


/*
 * Inventory and shops
 */

auto_item *borg_items;		/* Current "inventory" */

auto_shop *borg_shops;		/* Current "shops" */



/*
 * Safety arrays for simulating possible worlds
 */

auto_item *borg_safe_items;		/* Safety "inventory" */

auto_shop *borg_safe_shops;		/* Safety "shops" */


/*
 * Spell/Prayer info
 */

auto_magic borg_magics[9][9];	/* Magic info, by book/what */



/*
 * Hack -- allow fast access to any term windows.  XXX XXX XXX
 */
const term *borg_term_pointer = NULL;


/*
 * Query the "attr/char" at a given location on the screen
 *
 * Note that "x,y" specifies a screen location, which is assumed to be
 * legal.  XXX XXX XXX
 *
 * We always return 0.
 */
errr borg_what_char(int x, int y, byte *a, char *c)
{
	const byte *aa;
	const char *cc;

	/* Direct access XXX XXX XXX */
	aa = &(borg_term_pointer->scr->a[y][x]);
	cc = &(borg_term_pointer->scr->c[y][x]);

	/* Access */
	(*a) = (*aa);
	(*c) = (*cc);

	/* Success */
	return (0);
}


/*
 * Query the "attr/char codes" at a given location on the screen
 *
 * This function extracts the attr/char codes of some (partial) row of the
 * current screen, called a "region", starting at some given location, and
 * having a maximal length which is specified by the user, and an actual
 * length which is limited to the longest sequence of attr/char codes which
 * share the same "attr" code.
 *
 * Note that "x,y" specifies the starting screen location, which is assumed
 * to be legal.  XXX XXX XXX
 *
 * Note that "n" is used to determine the desired length "m" of the region,
 * equal to "MIN((80-x),ABS(n))", though the actual length of the region is
 * limited to the longest sequence of attr/char codes which share the same
 * "attr" code.
 *
 * Note that if "n" is non-negative, and the actual length of the region is
 * not equal to "n", then we will return 1, otherwise, we will return 0.
 *
 * Note that if "n" is negative, we will always return 0.
 *
 * Note that "a" points to a single "attr", into which the attr code of the
 * region will be stored.
 *
 * Note that "s" points to an array of char's, of "sufficient" size, into
 * which the char codes of the region will be stored.  Note that we always
 * terminate this array of chars, so there must be room for a termination
 * character (to be safe, provide at least 81 characters).  XXX XXX XXX
 *
 * We automatically convert all "blanks" and "invisible text" into spaces,
 * and we ignore the attr of such char's, except that if the entire region
 * consists of such char's, the attr of the region will be zero.
 *
 * We do not strip final spaces, so this function will very often read all
 * the way to the end of the line.
 *
 * This function assumes that "borg_term_pointer" points to a valid "term",
 * which is at least size "80x24".  Otherwise, core dumps are possible.
 *
 * The results of this function may be unexpected if any relevant grids
 * contain "graphic" attr/char pairs.  XXX XXX XXX
 */
errr borg_what_text(int x, int y, int n, byte *a, char *s)
{
	int i;

	byte t_a;
	char t_c;

	const byte *aa;
	const char *cc;

	/* Current attribute */
	byte d_a = 0;

	/* Max length to scan for */
	int m = ABS(n);

	/* Do not run off the screen */
	if (x + m > 80) m = 80 - x;

	/* Direct access XXX XXX XXX */
	aa = &(borg_term_pointer->scr->a[y][x]);
	cc = &(borg_term_pointer->scr->c[y][x]);

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
	if ((n >= 0) && (i != n)) return (1);

	/* Success */
	return (0);
}



/*
 * Steal info from the game -- inventory weight info
 */
void borg_steal_inventory_weight(char *buf)
{
	/* Create a fake description string */
	sprintf(buf, "Inventory (carrying %d.%d pounds)",
	        p_ptr->total_weight / 10, p_ptr->total_weight % 10);
}


/*
 * Steal info from the game -- inventory object description
 */
void borg_steal_inventory_desc(char *buf, int item)
{
	/* Default to "nothing" */
	buf[0] = '\0';

	/* Describe a real item */
	if (inventory[item].k_idx)
	{
		/* Describe it (fully) */
		object_desc(buf, &inventory[item], TRUE, 3);
	}
}



/*
 * Steal info from the game -- spell/prayer description
 *
 * Note the use of "as->cheat" to optimize the spell index extraction
 */
void borg_steal_spell_info(char *buf, int book, int what)
{
	int spell;

	magic_type *s_ptr;

	auto_magic *as;

	cptr comment;

	char info[80];


	/* Default (20 spaces) */
	strcpy(buf, "                    ");


	/* Access the spell */
	as = &borg_magics[book][what];

	/* Skip illegible spells */
	if (as->status == BORG_MAGIC_ICKY) return;


	/* Access the index */
	spell = as->cheat;

	/* Access the spell */
	s_ptr = &mp_ptr->info[spell];

	/* Illegible spells */
	if (s_ptr->slevel >= 99) return;


	/* Get extra info */
	spell_info(info, spell);

	/* Use that info */
	comment = info;

	/* Analyze the spell */
	if ((spell < 32) ?
	    ((p_ptr->spell_forgotten1 & (1L << spell))) :
	    ((p_ptr->spell_forgotten2 & (1L << (spell - 32)))))
	{
		comment = " forgotten";
	}
	else if (!((spell < 32) ?
	           (p_ptr->spell_learned1 & (1L << spell)) :
	           (p_ptr->spell_learned2 & (1L << (spell - 32)))))
	{
		comment = " unknown";
	}
	else if (!((spell < 32) ?
	           (p_ptr->spell_worked1 & (1L << spell)) :
	           (p_ptr->spell_worked2 & (1L << (spell - 32)))))
	{
		comment = " untried";
	}

	/* Extract the spell info */
	sprintf(buf, "%2d %4d %3d%%%s",
	        s_ptr->slevel, s_ptr->smana, spell_chance(spell), comment);
}


/*
 * Steal info from the game -- current dungeon panel info
 */
void borg_steal_panel_info(char *buf)
{
	/* Create a fake screen buffer */
	sprintf(buf, "Map sector [%d,%d]",
	        (p_ptr->wy / PANEL_HGT),
	        (p_ptr->wx / PANEL_WID));
}



/*
 * Log a message to a file
 */
void borg_info(cptr what)
{
	/* Dump a log file message */
	if (borg_fff) fprintf(borg_fff, "%s\n", what);
}



/*
 * Memorize a message, Log it, Search it, and Display it in pieces
 */
void borg_note(cptr what)
{
	int j, n, i, k;

	int w, h, x, y;

	term *old = Term;


	/* Memorize it */
	message_add(what);


	/* Log the message */
	borg_info(what);


	/* Hack -- Check against the search string */
	if (borg_match_string[0] && strstr(what, borg_match_string))
	{
		/* Hack -- Clean cancel */
		if (borg_active != 0) borg_active = 1;
	}


	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		if (!angband_term[j]) continue;

		/* Check flag */
		if (!(op_ptr->window_flag[j] & PW_BORG_1)) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		/* Access size */
		Term_get_size(&w, &h);

		/* Access cursor */
		Term_locate(&x, &y);

		/* Erase current line */
		Term_erase(0, y, 255);


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
				Term_addstr(-1, TERM_WHITE, buf);

				/* Advance (wrap) */
				if (++y >= h) y = 0;

				/* Erase next line */
				Term_erase(0, y, 255);

				/* Advance */
				what += k;

				/* Reduce */
				n -= k;
			}

			/* Show message tail */
			Term_addstr(-1, TERM_WHITE, what);

			/* Advance (wrap) */
			if (++y >= h) y = 0;

			/* Erase next line */
			Term_erase(0, y, 255);
		}

		/* Normal */
		else
		{
			/* Show message */
			Term_addstr(-1, TERM_WHITE, what);

			/* Advance (wrap) */
			if (++y >= h) y = 0;

			/* Erase next line */
			Term_erase(0, y, 255);
		}


		/* Flush output */
		Term_fresh();

		/* Use correct window */
		Term_activate(old);
	}
}



/*
 * Add a keypress to the "queue" (fake event)
 */
errr borg_keypress(char k)
{
	/* Hack -- Refuse to enqueue "nul" */
	if (!k) return (-1);

	/* Hack -- note the keypress */
	if (borg_fff) borg_info(format("& Key <%c>", k));

	/* Store the char, advance the queue */
	borg_key_queue[borg_key_head++] = k;

	/* Circular queue, handle wrap */
	if (borg_key_head == BORG_KEY_SIZE) borg_key_head = 0;

	/* Hack -- Catch overflow (forget oldest) */
	if (borg_key_head == borg_key_tail) borg_oops("overflow");

	/* Hack -- Overflow may induce circular queue */
	if (borg_key_tail == BORG_KEY_SIZE) borg_key_tail = 0;

	/* Success */
	return (0);
}


/*
 * Add a keypress to the "queue" (fake event)
 */
errr borg_keypresses(cptr str)
{
	cptr s;

	/* Send the keypresses */
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
	if (borg_key_head == borg_key_tail) return (0);

	/* Extract the keypress */
	i = borg_key_queue[borg_key_tail];

	/* Do not advance */
	if (!take) return (i);

	/* Advance the queue */
	borg_key_tail++;

	/* Circular queue requires wrap-around */
	if (borg_key_tail == BORG_KEY_SIZE) borg_key_tail = 0;

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
 * Abort the Borg, noting the reason
 */
void borg_oops(cptr what)
{
	/* Hack -- Hard cancel */
	borg_active = 0;

	/* Give a warning */
	borg_note(format("# Aborting (%s).", what));

	/* Forget borg keys */
	borg_flush();
}





#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

