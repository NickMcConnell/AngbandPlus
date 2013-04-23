/* File: borg9.c */

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
 * The "Angband Borg" is an automatic Angband player.
 *
 * Use of the Borg requires re-compilation with ALLOW_BORG defined,
 * and linking the various "borg source files" into the executable.
 *
 * The "borg source files" have been updated for use with Angband 2.8.3,
 * and will not work with any other version without (minor) modifications.
 *
 * Note that you can only use the Borg if your character has been marked
 * as a "Borg User".  You can do this, if necessary, by responding "y"
 * when asked if you really want to use the Borg.  This will (normally)
 * result in your character being inelligible for the high score list.
 *
 * The "do_cmd_borg()" function, called when the user hits "^Z", allows
 * the user to interact with the Borg, by entering a "borg-command" when
 * prompted.  Any borg-command (except escape) will cause the Borg to be
 * initialized (if needed), and then the borg-command will be executed.
 *
 * The first time you enter any borg-command, the Borg is initialized.
 * This consists of three major steps, and requires at least 400K of
 * free memory.  If enough memory is not available, the game may abort.
 * First, the various "borg" modules are initialized.  Second, various
 * important "state" information is extracted, including the level and
 * race/class of the player, and this information is used to perform
 * more initialization.  Third, some "historical" information (killed
 * uniques, maximum dungeon depth, etc) is "stolen" from the game.
 *
 * The simplest borg-command is "z", which "activates" the Borg, causing it
 * to take control of the character, and play the game, until you hit a key.
 * Other borg-commands, including "?", are described in "do_cmd_borg()".
 *
 * When the Borg is "activated", it uses the "inkey_hack" hook to steal
 * control from the user.  Later, if it detects any input from the user,
 * it gracefully relinquishes control by clearing the "inkey_hack" hook
 * after any pending borg-key-sequences are complete.
 *
 * The Borg will abort if it detects any errors (such as bizarre screens),
 * strange situations (such as death), or panic situations (such as being
 * about to die), if the appropriate flags are set.
 *
 * The Borg is only supposed to "know" what a human playing the game could
 * know, that is, observe the information actually displayed on the screen,
 * and is only supposed to "do" the things that a human playing the game
 * could do, that is, send keypresses to the game.
 *
 * The Borg uses the "z-term.c" package to "look" at the screen, using the
 * screen access function "Term_what()", and the cursor location function
 * "Term_locate()".  Actually, it uses inline versions of these functions
 * (for efficiency).  This is very similar to what a human does, except
 * that a human must actually mentally convert visual impulses into colored
 * symbols.
 *
 * The Borg uses the special "inkey_hack" hook in "util.c" to allow it to
 * "steal" control from the "inkey()" function, which is used by the game,
 * and to "send" keypress events as if it was a normal user.  This is not
 * quite what a human does, in particular, by stealing control in such a
 * manner, the Borg is able to know exactly when the game is waiting for
 * a keypress, which allows it to know when the screen has finished being
 * refreshed.  Also, the Borg uses the "inkey_flag" variable to determine
 * when the game is waiting for a top level command.
 *
 *
 * If the code wqs properly designed, that is, if the Borg actually acted
 * just like a real human player, then it could, in theory, be run as an
 * external process, which would actually examine a physical display and
 * send keypresses directly to a physical keyboard, or at least (to avoid
 * the need for a camera and a robotic arm), which would run the game in
 * a "pseudo-terminal", allowing it to examine the current screen image
 * and send keypress events to the underlying process (the Angband Game).
 *
 * Currently, the code is *not* properly designed, that is, there are a
 * few places in which it does things which a human would not be able to
 * do, such as examine (and even change) the values of certain variables
 * private to the game.  However, there are very few of these places, so
 * with a little work, it should be possible for the Borg to actually
 * simulate a real human player.  All known "problems" are listed below,
 * and note that almost all of them would disappear if the Borg was made
 * into a separate executable and taught to generate its own character,
 * and to determine when the game was ready for a keypress.
 *
 * A major problem is that the Borg "knows" when the game is waiting for
 * a keypress.  There is no single solution for this problem, since even
 * humans sometimes get confused when playing over a very slow connection.
 * There are some heuristics which would probably be sufficient, including
 * waiting until the screen does not change for a full second, and assuming
 * that it is therefore waiting for input, or using clever choices of some
 * "free" commands, such as the "note" command, to allow the Borg to cause
 * special things to appear on the screen when the game is ready for the
 * next command.  The hardest thing would actually be making sure that all
 * possible situations were accounted for.
 *
 * A minor problem is that the Borg must assume that a human has already
 * created a character, and set up various options and such so that the
 * Borg can function correctly.  Currently, to avoid crashing if these
 * assumptions are incorrect, the code actually sets some options and
 * such to known values, and redraws the screen using the new options,
 * any time a human user asks the Borg to initialize or resume playing.
 * Since a human may, at any time, take over and perform arbitrary acts,
 * and since the Borg does not actually generate the character, currently,
 * when the Borg is initialized (or restarted) it "steals" some important
 * values directly from the game, including the deepest level which has
 * been explored, and which uniques are known to have been killed.  Other
 * values which could be stolen are actually extracted from the screen,
 * such as the race and class of the character.  All of these problems
 * would go away if the Borg generated the character and knew that nobody
 * else was going to use the character file.  Note that certain options
 * cannot be changed by the Borg, including "preserve" mode and "maximize"
 * mode, which are determined when the character is created.  If "preserve"
 * mode is not true, then the Borg may lose some artifacts.  If "maximize"
 * mode is not false, then the Borg may break in unexpected ways.
 *
 * There are several things that the current Borg code is "able" to learn
 * without "cheating", such as the descriptions of all of the equipment
 * and inventory items, information about each available spell or prayer,
 * and the location of the current panel relative to the overall dungeon,
 * by actually sending various keypresses to the game and observing the
 * results, but this is *extremely* annoying to watch, so by default, this
 * code is not used, and the Borg uses special "cheat" code to obtain this
 * information directly from the game.
 *
 * There are several pieces of information about the game that are used
 * directly by the current Borg code, including some of the tables in the
 * "tables.c" file, and some of the static arrays of information which are
 * initialized in the "init1.c" and "init2.c" files, but note that this is
 * not cheating, since if the Borg was compiled into a separate executable,
 * it would simply use these files directly.
 *
 * Note that the Borg can play any race/class combination, that is, he can
 * make "effective" use of at least a few spells/prayers, and is not "broken"
 * by low strength, blind-ness, hallucination, etc.  This does not, however,
 * mean that he plays all classes equally well, especially since he is so
 * dependant on a high strength for so many things.
 *
 * Note that the Borg understands the concept of "Cheating Death".  If this
 * is available (see "wizard mode" and the "cheat_live" flag), then he will
 * automatically "cheat death" whenever he is "about to die".  Otherwise, he
 * will stop playing as soon as he notices that he has died.
 *
 * Note that "borg_time" bears a close resemblance to the number of "player
 * turns" that have gone by.  Except that occasionally, the Borg will do
 * something that he *thinks* will take time but which actually does not
 * (for example, attempting to open a hallucinatory door), and sometimes,
 * the Borg performs a "repeated" command (rest, open, tunnel, or search),
 * which may actually take longer than a single turn.  This has the effect
 * that "borg_time" is slightly lacking in "precision".  Note that we can
 * store every time-stamp in a 's16b', since we reset "borg_time" to 1000
 * on entering a new level, and we refuse to stay on any level longer than
 * 30000 turns, unless we are totally stuck, in which case we abort.  This
 * almost never happens unless the Borg is permanently paralyzed, or is in
 * a corner surrounded by lice with no means of escape, or runs out of light
 * in a dark hallway and cannot find the stairs.
 */


/*
 * Stat advantages:
 *   High STR (attacks, to-dam, digging, weight limit)
 *   High DEX (attacks, to-hit, armor class)
 *   High CON (hitpoints, recovery)
 *   High WIS (better prayers, saving throws)
 *   High INT (better spells, disarming, device usage)
 *   High CHR (better item costs)
 *
 * Class advantages:
 *   Warrior (good fighting, sensing)
 *   Mage (good spells)
 *   Priest (good prayers, fighting)
 *   Ranger (some spells, fighting)
 *   Rogue (some spells, fighting, sensing)
 *   Paladin (prayers, fighting, sensing)
 *
 * Race advantages:
 *   Gnome (free action)
 *   Dwarf (resist blindness)
 *   High elf (see invisible)
 *   Non-human (infravision)
 */


/*
 * We currently handle:
 *   Level changing (intentionally or accidentally)
 *   Embedded objects (gold) that must be extracted
 *   Ignore embedded objects if too "weak" to extract
 *   Traps (disarm), Doors (open/etc), Rubble (tunnel)
 *   Stores (enter via movement, exit via escape)
 *   Stores (limited commands, and different commands)
 *   Always deal with objects one at a time, not in piles
 *   Discard junk before trying to pick up more stuff
 *   Use "identify" to obtain knowledge and/or money
 *   Rely on "sensing" objects as much as possible
 *   Do not sell junk or worthless items to any shop
 *   Do not attempt to buy something without the cash
 *   Use the non-optimal stairs if stuck on a level
 *   Use "flow" code for all tasks for consistency
 *   Cancel all goals when major world changes occur
 *   Use the "danger" code to avoid potential death
 *   Use the "danger" code to avoid inconvenience
 *   Try to avoid danger (both actively and passively)
 *   Handle "Mace of Disruption", "Scythe of Slicing", etc
 *   Learn spells, and use them when appropriate
 *   Remember that studying prayers is slightly random
 *   Do not try to read scrolls when blind or confused
 *   Do not study/use spells/prayers when blind/confused
 *   Use spells/prayers at least once for the experience
 *   Attempt to heal when "confused", "blind", etc
 *   Attempt to fix "fear", "poison", "cuts", etc
 *   Analyze potential equipment in proper context
 *   Priests should avoid edged weapons (spell failure)
 *   Mages should avoid most gloves (lose mana)
 *   Non-warriors should avoid heavy armor (lose mana)
 *   Keep "best" ring on "tight" right finger in stores
 *   Remove items which do not contribute to total fitness
 *   Wear/Remove/Sell/Buy items in most optimal order
 *   Pursue optimal combination of available equipment
 *   Notice "failure" when using rods/staffs/artifacts
 *   Notice "failure" when attempting spells/prayers
 *   Attempt to correctly track terrain, objects, monsters
 *   Take account of "clear" and "multi-hued" monsters
 *   Take account of "flavored" (randomly colored) objects
 *   Handle similar objects/monsters (mushrooms, coins)
 *   Multi-hued/Clear monsters, and flavored objects
 *   Keep in mind that some monsters can move (quickly)
 *   Do not fire at monsters that may not actually exist
 *   Assume everything is an object until proven otherwise
 *   Parse messages to correct incorrect assumptions
 *   Search for secret doors after exploring the level
 *   React intelligently to changes in the wall structure
 *   Do not recalculate "flow" information unless required
 *   Collect monsters/objects/terrain not currently in view
 *   Keep in mind that word of recall is a delayed action
 *   Keep track of charging items (rods and artifacts)
 *   Be very careful not to access illegal locations!
 *   Do not rest next to dangerous (or immobile) monsters
 *   Recall into dungeon if prepared for resulting depth
 *   Do not attempt to destroy cursed ("terrible") artifacts
 *   Attempted destruction will yield "terrible" inscription
 *   Use "maximum" level and depth to prevent "thrashing"
 *   Use "maximum" hp's and sp's when checking "potentials"
 *   Attempt to recognize large groups of "disguised" monsters
 *   Beware of firing at a monster which is no longer present
 *   Stockpile items in the Home, and use those stockpiles
 *   Discounted spell-books (low level ones are ignored)
 *   Take items out of the home to sell them when no longer needed
 *   Trappers and Mimics (now treated as invisible monsters)
 *   Invisible monsters (induce "fear" of nearby regions)
 *   Fleeing monsters are "followed" down corridors and such
 *
 * We ignore:
 *   Running out of light can (fatally) confuse the Borg
 *   Running out of food can kill you, try not to starve
 *   Long object descriptions may have clipped inscriptions
 *
 * We need to handle:
 *   Better "fleeing" code from nasty monsters
 *   Appearance of "similar" monsters (jackals + grip)
 *   Hallucination (induces fake objects and monsters)
 *   Special screens (including tombstone) with no "walls"
 *   Appearance of the '@' symbol on "special" screens
 *   Technically a room can have no exits, requiring digging
 *   Try to use a shovel/pick to help with tunnelling
 *   If wounded, must run away from monsters, then rest
 *   When blind, monster and object info may be invalid
 *   When totally surrounded by monsters, try to escape rooms
 *   Conserve memory space (each grid byte costs about 15K)
 *   Conserve computation time (especially with the flow code)
 *   Note -- nutrition can come from food, scrolls, or spells
 *   Note -- recall can come from scrolls, rods, or spells
 *   Note -- identify can come from scrolls, rods, staffs, or spells
 *   Becoming "afraid" (attacking takes a turn for no effect)
 *   Beware of firing missiles at a food ration under a mushroom
 *   Attempt to save possibly useful equipment in the home
 *
 * We need to handle "loading" saved games:
 *   The "max_depth" value is lost if loaded in the town
 *   If we track "dead uniques" then this information is lost
 *   The "map" info, "flow" info, "tracking" info, etc is lost
 *   The contents of the shops (and the home) are lost
 *   We may be asked to "resume" a non-Borg character (icky)
 */



/*
 * Mega-Hack -- special "inkey_hack" hook.  XXX XXX XXX
 *
 * A special function hook (see "util.c") which allows the Borg to take
 * control of the "inkey()" function, and substitute in fake keypresses.
 */
extern char (*inkey_hack)(int flush_first);


/*
 * This function lets the Borg "steal" control from the user.
 *
 * The "util.c" file provides a special "inkey_hack" hook which we use
 * to steal control of the keyboard, using the special function below.
 *
 * Since this function bypasses the code in "inkey()" which "refreshes"
 * the screen whenever the game has to wait for a keypress, the screen
 * will only get refreshed when (1) an option such as "fresh_before"
 * induces regular screen refreshing or (2) various explicit calls to
 * "Term_fresh" are made, such as in the "project()" function.  This
 * has the interesting side effect that the screen is never refreshed
 * while the Borg is browsing stores, checking his inventory/equipment,
 * browsing spell books, checking the current panel, or examining an
 * object, which reduces the "screen flicker" considerably.  :-)
 *
 * The only way that the Borg can be stopped once it is started, unless
 * it dies or encounters an error, is to press any key.  This function
 * checks for real user input on a regular basic, and if any is found,
 * it is flushed, and after completing any actions in progress, this
 * function hook is removed, and control is returned to the user.
 *
 * We handle "broken" messages, in which long messages are "broken" into
 * pieces, and all but the first message are "indented" by one space, by
 * collecting all the pieces into a complete message and then parsing the
 * message once it is known to be complete.
 *
 * This function hook automatically removes itself when it realizes that
 * it should no longer be active.  Note that this may take place after
 * the game has asked for the next keypress, but the various "keypress"
 * routines should be able to handle this.
 */
static char borg_inkey_hack(int flush_first)
{
	char ch;

	int y = 0;
	int x = 80;

	byte t_a;

	char buf[128];



	/* Deactivate */
	if (borg_active == 0)
	{
		/* Message */
		borg_note("# Removing keypress hook");

		/* Remove hook */
		inkey_hack = NULL;

		/* Flush keys */
		borg_flush();

		/* Flush */
		flush();

		/* Done */
		return (0);
	}


	/* Mega-Hack -- flush keys */
	if (flush_first)
	{
		/* Only flush if needed */
		if (borg_inkey(FALSE) != 0)
		{
			/* Message */
			borg_note("# Flushing keypress buffer");

			/* Flush keys */
			borg_flush();
		}
	}


	/* Locate the cursor */
	(void)Term_locate(&x, &y);


	/* Assume no prompt/message is available */
	borg_prompt = FALSE;

	/* Mega-Hack -- check for possible prompts/messages */
	/* If the first four characters on the message line all */
	/* have the same attribute (or are all spaces), and they */
	/* are not all spaces (ascii value 0x20)... */
	if ((0 == borg_what_text(0, 0, 4, &t_a, buf)) &&
	    (t_a != TERM_DARK) &&
	    (*((u32b*)(buf)) != 0x20202020))
	{
		/* Assume a prompt/message is available */
		borg_prompt = TRUE;
	}


	/* Mega-Hack -- Catch "Die? [y/n]" messages */
	/* If there is text on the first line... */
	/* And the game does not want a command... */
	/* And the cursor is on the top line... */
	/* And the text acquired above is "Die?" */
	if (borg_prompt && !inkey_flag &&
	    (y == 0) && (x >= 4) &&
	    streq(buf, "Die?"))
	{
		/* Flush messages */
		borg_parse(NULL);

		/* Take note */
		borg_note("# Cheating death...");

		/* Reset death flag */
		b_ptr->is_dead = FALSE;

		/* Cheat death */
		return ('n');
	}


	/* Mega-Hack -- Handle death */
	if (b_ptr->is_dead)
	{
		/* Oops */
		borg_oops("player died");

		/* Useless keypress */
		return (KTRL('C'));
	}


	/* Mega-Hack -- Catch "-more-" messages */
	/* If there is text on the first line... */
	/* And the game does not want a command... */
	/* And the cursor is on the top line... */
	/* And there is text before the cursor... */
	/* And that text is "-more-" */
	if (borg_prompt && !inkey_flag &&
	    (y == 0) && (x >= 7) &&
	    (0 == borg_what_text(x-7, y, 7, &t_a, buf)) &&
	    (streq(buf, " -more-")))
	{
		/* Get the message */
		if (0 == borg_what_text(0, 0, x-7, &t_a, buf))
		{
			/* Parse it */
			borg_parse(buf);
		}

		/* Clear the message */
		return (KTRL('M'));
	}


	/* Mega-Hack -- catch normal messages */
	/* If there is text on the first line... */
	/* And the game wants a command */
	if (borg_prompt && inkey_flag)
	{
		/* Get the message(s) */
		if (0 == borg_what_text(0, 0, -80, &t_a, buf))
		{
			int k = strlen(buf);

			/* Strip trailing spaces */
			while ((k > 0) && (buf[k-1] == ' ')) k--;

			/* Terminate */
			buf[k] = '\0';

			/* Parse it */
			borg_parse(buf);
		}

		/* Clear the message */
		return (KTRL('M'));
	}

	/* Flush messages */
	borg_parse(NULL);


	/* Check for key */
	ch = borg_inkey(TRUE);

	/* Use the key */
	if (ch) return (ch);


	/* Check for user abort */
	(void)Term_inkey(&ch, FALSE, FALSE);

	/* User Abort */
	if (ch != 0)
	{
		/* Oops */
		borg_oops("user abort");

		/* Hack -- Escape */
		return (ESCAPE);
	}


	/* Save the system random info */
	borg_rand_quick = Rand_quick;
	borg_rand_value = Rand_value;

	/* Use the local random info */
	Rand_quick = TRUE;
	Rand_value = borg_rand_local;

	/* Think */
	borg_think();

	/* Save the local random info */
	borg_rand_local = Rand_value;

	/* Restore the system random info */
	Rand_quick = borg_rand_quick;
	Rand_value = borg_rand_value;


	/* Check for key */
	ch = borg_inkey(TRUE);

	/* Use the key */
	if (ch) return (ch);


	/* Oops */
	borg_oops("normal abort");

	/* Hack -- Escape */
	return (ESCAPE);
}



/*
 * Prepare to play
 *
 * These things cannot be done during "initialization" because an
 * obnoxious human might change things while he has control.
 *
 * Should also verify that the "misc_to_attr"/"misc_to_char" pairs
 * are reasonable for flavored objects.
 */
static void borg_prepare(void)
{
	int i;

	int book;

	bool hack = FALSE;


	/*** Options ***/

	/* We prefer not to check for user aborts when resting */
	avoid_abort = TRUE;

	/* We must use the original keypress codes */
	rogue_like_commands = FALSE;

	/* We must pick up items when we step on them */
	always_pickup = TRUE;

	/* We must specify targets by hand (?) */
	use_old_target = FALSE;

	/* We must use the top object in stacks */
	floor_query_flag = FALSE;

	/* We must pick items up without verification */
	carry_query_flag = FALSE;

	/* We must destroy items without verification */
	verify_destroy = FALSE;

	/* We must not allow quantity specification (?) */
	allow_quantity = FALSE;

	/* We must not auto-repeat commands */
	always_repeat = FALSE;

	/* We must not haggle */
	auto_haggle = TRUE;

	/* We must have maximal object description space (needs redraw) */
	show_flavors = FALSE;

	/* We must have maximal object description space (needs redraw) */
	show_labels = FALSE;

	/* We must have maximal object description space (needs redraw) */
	show_weights = FALSE;

	/* We prefer to allow items to stack */
	testing_stack = TRUE;

	/* We prefer to allow monsters to carry */
	testing_carry = TRUE;

	/* We prefer to not ignore discounts */
	stack_force_costs = FALSE;

	/* We prefer to ignore inscriptions */
	stack_force_notes = TRUE;

	/* We must not see hitpoint warnings */
	op_ptr->hitpoint_warn = 0;

	/* We prefer to see depth by dungeon level */
	depth_in_feet = FALSE;

#ifdef ALLOW_EASY_OPEN
	/* We open doors ourself */
	easy_open = FALSE;
#endif /* ALLOW_EASY_OPEN */

#ifdef ALLOW_EASY_DISARM
	/* We disarm traps ourself */
	easy_disarm = FALSE;
#endif /* ALLOW_EASY_DISARM */

	/* No intelligent monsters */
	monster_ai = FALSE;

#ifdef USE_GRAPHICS

	/* The Borg can't work with graphics on, so switch it off */
	if (use_graphics)
	{
		/* Reset to ASCII mode */
		use_graphics = FALSE;
		arg_graphics = FALSE;

		/* Reset visuals */
		reset_visuals(TRUE);
	}

#endif /* USE_GRAPHICS */


	/*** Redraw ***/

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Redraw everything */
	do_cmd_redraw();


	/*** Race/Class/Spells ***/

	/* Extract the race */
	b_ptr->prace = p_ptr->prace;

	/* Extract the class */
	b_ptr->pclass = p_ptr->pclass;

	/* Extract the race pointer */
	rb_ptr = &race_info[b_ptr->prace];

	/* Extract the class pointer */
	cb_ptr = &class_info[b_ptr->pclass];

	/* Extract the magic pointer */
	mb_ptr = &magic_info[b_ptr->pclass];

	/* Initialize the books */
	for (book = 0; book < 9; book++)
	{
		borg_prepare_book(book);
	}


	/*** Reset State ***/

	/* Update some stuff */
	borg_do_update_view = TRUE;

	/* Examine self */
	borg_do_inven = TRUE;
	borg_do_equip = TRUE;
	borg_do_spell = TRUE;
	borg_do_spell_aux = 0;

	/* Examine the world */
	borg_do_panel = TRUE;
	borg_do_frame = TRUE;

	/* Enable some functions */
	borg_do_crush_junk = TRUE;
	borg_do_crush_hole = TRUE;
	borg_do_crush_slow = TRUE;

	/* Allowable Cheat -- Obtain "recall" flag */
	borg_recalling = (p_ptr->word_recall ? TRUE : FALSE);


	/*** Cheat -- dead uniques ***/

	/* Hack -- Extract dead uniques */
	for (i = 1; i < MAX_R_IDX-1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Skip non-uniques */
		if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

		/* Mega-Hack -- Access "dead unique" list */
		if (r_ptr->max_num == 0) borg_race_death[i] = 1;
	}


	/*** Nothing analysis ***/

	/* Check the nothing */
	if (TRUE)
	{
		feature_type *f_ptr = &f_info[0];

		/* Notice use of "graphics" */
		if (f_ptr->x_attr & 0x80) hack = TRUE;

		/* Verify nothing char */
		if (f_ptr->x_char != ' ') hack = TRUE;
	}


	/*** Feature analysis ***/

	/* Scan the features */
	for (i = 1; i < MAX_F_IDX; i++)
	{
		feature_type *f_ptr = &f_info[i];

		/* Skip non-features */
		if (!f_ptr->name) continue;

		/* Switch off "graphics" */
		f_ptr->x_attr = f_ptr->d_attr;
		f_ptr->x_char = f_ptr->d_char;

		/* Notice use of "graphics" */
		if (f_ptr->x_attr & 0x80) hack = TRUE;
		if (f_ptr->x_char & 0x80) hack = TRUE;

		/* Notice first picture use */
		if (borg_char_feat[(byte)(f_ptr->x_char)] == 0)
		{ 
			borg_char_feat[(byte)(f_ptr->x_char)] = i;
		}
	}


	/*** Verify the features ***/

	/* XXX XXX XXX */


	/*** Object analysis ***/

	/* Scan the objects */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Skip non-items */
		if (!k_ptr->name) continue;

		/* Switch off "graphics" */
		k_ptr->x_attr = k_ptr->d_attr;
		k_ptr->x_char = k_ptr->d_char;

		/* Notice use of "graphics" */
		if (k_ptr->x_attr & 0x80) hack = TRUE;

		/* Notice this object */
		borg_char_is_take[(byte)(k_ptr->x_char)] = TRUE;
	}


	/*** Player analysis ***/

	/* Check the player */
	if (TRUE)
	{
		monster_race *r_ptr = &r_info[0];

		/* Switch off "graphics" */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;

		/* Notice use of "graphics" */
		if (r_ptr->x_attr & 0x80) hack = TRUE;

		/* Verify player char */
		if (r_ptr->x_char != '@') hack = TRUE;
	}


	/*** Monster analysis ***/

	/* Scan the monsters */
	for (i = 1; i < MAX_R_IDX-1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Switch off "graphics" */
		r_ptr->x_attr = r_ptr->d_attr;
		r_ptr->x_char = r_ptr->d_char;

		/* Notice use of "graphics" */
		if (r_ptr->x_attr & 0x80) hack = TRUE;

		/* Hack -- Skip "clear" monsters XXX XXX XXX */
		if (r_ptr->flags1 & RF1_CHAR_CLEAR) continue;

		/* Hack -- Skip "multi" monsters XXX XXX XXX */
		if (r_ptr->flags1 & RF1_CHAR_MULTI) continue;

		/* Notice this monster */
		borg_char_is_kill[(byte)(r_ptr->x_char)] = TRUE;
	}


	/*** Disallow Graphics ***/

	/* Oops */
	if (hack) borg_oops("cannot use graphics");
}



/*
 * Initialize some stuff
 */
static void borg_init_1(void)
{
	/*** Allow direct access to visual information ***/

	/* Point to the main window */
	borg_term_pointer = angband_term[0];


	/*** Various things ***/

	/* Allocate the "keypress queue" */
	C_MAKE(borg_key_queue, BORG_KEY_SIZE, char);

	/* Prepare a local random number seed */
	borg_rand_local = rand_int(0x10000000);


	/*** Special counters ***/

	/* Count racial appearances */
	C_MAKE(borg_race_count, MAX_R_IDX, s16b);

	/* Count racial deaths */
	C_MAKE(borg_race_death, MAX_R_IDX, s16b);


	/*** Map analysis arrays ***/

	/* Char code may be a feature */
	C_MAKE(borg_char_feat, 256, byte);


	/*** Classification arrays ***/

	/* Char code may be an object */
	C_MAKE(borg_char_is_take, 256, bool);

	/* Char code may be a monster */
	C_MAKE(borg_char_is_kill, 256, bool);


	/*** Grid arrays ***/

	C_MAKE(borg_view_g, BORG_VIEW_MAX, u16b);

	C_MAKE(borg_temp_g, BORG_TEMP_MAX, u16b);

	borg_temp_y = ((byte*)(borg_temp_g)) + 0;
	borg_temp_x = ((byte*)(borg_temp_g)) + BORG_TEMP_MAX;

	C_MAKE(borg_hack_g, BORG_HACK_MAX, u16b);

	borg_hack_y = ((byte*)(borg_hack_g)) + 0;
	borg_hack_x = ((byte*)(borg_hack_g)) + BORG_HACK_MAX;

	C_MAKE(borg_flow_g, BORG_FLOW_MAX, u16b);

	borg_flow_y = ((byte*)(borg_flow_g)) + 0;
	borg_flow_x = ((byte*)(borg_flow_g)) + BORG_FLOW_MAX;


	/*** Dungeon arrays ***/

	C_MAKE(borg_cave_info, DUNGEON_HGT, byte_256);
	C_MAKE(borg_cave_feat, DUNGEON_HGT, byte_256);

	C_MAKE(borg_cave_o_idx, DUNGEON_HGT, byte_wid);
	C_MAKE(borg_cave_m_idx, DUNGEON_HGT, byte_wid);

	C_MAKE(borg_cave_search, DUNGEON_HGT, byte_wid);

	C_MAKE(borg_cave_danger, DUNGEON_HGT, s16b_wid);

	C_MAKE(borg_flow_cost, DUNGEON_HGT, byte_wid);
	C_MAKE(borg_flow_work, DUNGEON_HGT, byte_wid);


	/*** Tracking arrays ***/

	/* Track the shop locations */
	C_MAKE(borg_track_shop_x, BORG_MAX_SHOP, byte);
	C_MAKE(borg_track_shop_y, BORG_MAX_SHOP, byte);

	/* Track "up" stairs */
	borg_track_less_num = 0;
	C_MAKE(borg_track_less_x, BORG_MAX_LESS, byte);
	C_MAKE(borg_track_less_y, BORG_MAX_LESS, byte);

	/* Track "down" stairs */
	borg_track_more_num = 0;
	C_MAKE(borg_track_more_x, BORG_MAX_MORE, byte);
	C_MAKE(borg_track_more_y, BORG_MAX_MORE, byte);


	/*** Object tracking ***/

	/* No objects yet */
	borg_takes_cnt = 0;
	borg_takes_nxt = 1;


	/*** Monster tracking ***/

	/* No monsters yet */
	borg_kills_cnt = 0;
	borg_kills_nxt = 1;


	/*** Depth tracking ***/

	/* Allowable Cheat -- Access max depth */
	b_ptr->max_depth = p_ptr->max_depth;

	/* Hack -- Assume happy depth */
	borg_happy_depth = (b_ptr->max_depth > 2) ? (b_ptr->max_depth - 2) : b_ptr->max_depth;
}


/*
 * Sorting hook -- comp function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)
{
	cptr *text = (cptr*)(u);
	s16b *what = (s16b*)(v);

	int cmp;

	/* Compare the two strings */
	cmp = (strcmp(text[a], text[b]));

	/* Strictly less */
	if (cmp < 0) return (TRUE);

	/* Strictly more */
	if (cmp > 0) return (FALSE);

	/* Enforce "stable" sort */
	return (what[a] <= what[b]);
}


/*
 * Sorting hook -- swap function -- see below
 *
 * We use "u" to point to an array of strings, and "v" to point to
 * an array of indexes, and we sort them together by the strings.
 */
static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)
{
	cptr *text = (cptr*)(u);
	s16b *what = (s16b*)(v);

	cptr texttmp;
	s16b whattmp;

	/* Swap "text" */
	texttmp = text[a];
	text[a] = text[b];
	text[b] = texttmp;

	/* Swap "what" */
	whattmp = what[a];
	what[a] = what[b];
	what[b] = whattmp;
}



/*
 * Initialize some stuff
 *
 * Note that the Borg will never find Grond/Morgoth, but we
 * prepare the item parsers for them anyway.  Actually, the
 * Borg might get lucky and find some of the special artifacts,
 * so it is always best to prepare for a situation if it does
 * not cost any effort.
 *
 * Note that all six artifact "Rings" will parse as "kind 506"
 * (the first artifact ring) and both artifact "Amulets" will
 * parse as "kind 503" (the first of the two artifact amulets),
 * but as long as we use the "name1" field (and not the "kind"
 * or "sval" fields) we should be okay.
 *
 * We sort the two arrays of items names in reverse order, so that
 * we will catch "mace of disruption" before "mace", "Scythe of
 * Slicing" before "Scythe", and for "Ring of XXX" before "Ring".
 *
 * Note that we do not have to parse "plural artifacts" (!)
 *
 * Hack -- This entire routine is a giant hack, but it works
 */
static void borg_init_3(void)
{
	int i, k, n;

	int num;

	s16b what[512];
	cptr text[512];

	char buf[256];


	/*** Item/Ware arrays ***/

	/* Make the inventory array */
	C_MAKE(borg_items, INVEN_TOTAL, auto_item);

	/* Make the stores in the town */
	C_MAKE(borg_shops, BORG_MAX_SHOP, auto_shop);


	/*** Item/Ware arrays (simulation) ***/

	/* Make the "safe" inventory array */
	C_MAKE(borg_safe_items, INVEN_TOTAL, auto_item);

	/* Make the "safe" stores in the town */
	C_MAKE(borg_safe_shops, BORG_MAX_SHOP, auto_shop);


	/*** Analysis info ***/

	/* Allocate "ab_ptr" */
	MAKE(ab_ptr, player_anal);


	/*** Plural Object Templates ***/

	/* Start with no objects */
	num = 0;

	/* Analyze some "item kinds" */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		object_type hack;

		/* Get the kind */
		object_kind *k_ptr = &k_info[k];

		/* Skip "empty" items */
		if (!k_ptr->name) continue;

		/* Skip "gold" objects */
		if (k_ptr->tval == TV_GOLD) continue;

		/* Skip "artifacts" */
		if (k_ptr->flags3 & TR3_INSTA_ART) continue;

		/* Hack -- make an item */
		object_prep(&hack, k);

		/* Describe a "plural" object */
		hack.number = 2;
		object_desc_store(buf, &hack, FALSE, 0);

		/* Save an entry */
		text[num] = string_make(buf);
		what[num] = k;
		num++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort */
	ang_sort(text, what, num);

	/* Save the num */
	ab_ptr->plural_num = num;

	/* Allocate the "item parsing arrays" (plurals) */
	C_MAKE(ab_ptr->plural_text, ab_ptr->plural_num, cptr);
	C_MAKE(ab_ptr->plural_what, ab_ptr->plural_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->plural_text[i] = text[i];
	for (i = 0; i < num; i++) ab_ptr->plural_what[i] = what[i];


	/*** Singular Object Templates ***/

	/* Start with no objects */
	num = 0;

	/* Analyze some "item kinds" */
	for (k = 1; k < MAX_K_IDX; k++)
	{
		object_type hack;

		/* Get the kind */
		object_kind *k_ptr = &k_info[k];

		/* Skip "empty" items */
		if (!k_ptr->name) continue;

		/* Skip "dungeon terrain" objects */
		if (k_ptr->tval == TV_GOLD) continue;

		/* Skip "artifacts" */
		if (k_ptr->flags3 & TR3_INSTA_ART) continue;

		/* Hack -- make an item */
		object_prep(&hack, k);

		/* Describe a "singular" object */
		hack.number = 1;
		object_desc_store(buf, &hack, FALSE, 0);

		/* Save an entry */
		text[num] = string_make(buf);
		what[num] = k;
		num++;
	}

	/* Analyze the "INSTA_ART" items */
	for (i = 1; i < MAX_A_IDX; i++)
	{
		object_type hack;

		artifact_type *a_ptr = &a_info[i];

		cptr name = (a_name + a_ptr->name);

		/* Skip "empty" items */
		if (!a_ptr->name) continue;

		/* Skip non INSTA_ART things */
		if (!(a_ptr->flags3 & TR3_INSTA_ART)) continue;

		/* Extract the "kind" */
		k = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Hack -- make an item */
		object_prep(&hack, k);

		/* Save the index */
		hack.name1 = i;

		/* Describe a "singular" object */
		hack.number = 1;
		object_desc_store(buf, &hack, FALSE, 0);

		/* Extract the "suffix" length */
		n = strlen(name) + 1;

		/* Remove the "suffix" */
		buf[strlen(buf) - n] = '\0';

		/* Save an entry */
		text[num] = string_make(buf);
		what[num] = k;
		num++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort */
	ang_sort(text, what, num);

	/* Save the num */
	ab_ptr->single_num = num;

	/* Allocate the "item parsing arrays" (plurals) */
	C_MAKE(ab_ptr->single_text, ab_ptr->single_num, cptr);
	C_MAKE(ab_ptr->single_what, ab_ptr->single_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->single_text[i] = text[i];
	for (i = 0; i < num; i++) ab_ptr->single_what[i] = what[i];


	/*** Artifact and Ego-Item Parsers ***/

	/* No entries yet */
	num = 0;

	/* Collect the "artifact names" */
	for (k = 1; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* Skip non-items */
		if (!a_ptr->name) continue;

		/* Extract a string */
		sprintf(buf, " %s", (a_name + a_ptr->name));

		/* Save an entry */
		text[num] = string_make(buf);
		what[num] = k;
		num++;
	}

	/* Collect the "ego-item names" */
	for (k = 1; k < MAX_E_IDX; k++)
	{
		ego_item_type *e_ptr = &e_info[k];

		/* Skip non-items */
		if (!e_ptr->name) continue;

		/* Extract a string */
		sprintf(buf, " %s", (e_name + e_ptr->name));

		/* Save an entry */
		text[num] = string_make(buf);
		what[num] = k + 256;
		num++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort */
	ang_sort(text, what, num);

	/* Save the num */
	ab_ptr->artego_num = num;

	/* Allocate the "item parsing arrays" (plurals) */
	C_MAKE(ab_ptr->artego_text, ab_ptr->artego_num, cptr);
	C_MAKE(ab_ptr->artego_what, ab_ptr->artego_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->artego_text[i] = text[i];
	for (i = 0; i < num; i++) ab_ptr->artego_what[i] = what[i];
}


/*
 * Initialize some stuff
 */
static void borg_init_5(void)
{
	int i;

	int num;

	s16b what[512];
	cptr text[512];


	/*** Old Info ***/

	/* Old depth */
	xb_ptr->old_depth = -1;

	/* Old stuff */
	xb_ptr->old_chp = -1;
	xb_ptr->old_csp = -1;

	/* Old panel */
	xb_ptr->old_wx = -1;
	xb_ptr->old_wy = -1;

	/* Old location */
	xb_ptr->old_px = -1;
	xb_ptr->old_py = -1;


	/*** Message tracking ***/

	/* No chars saved yet */
	borg_msg_len = 0;

	/* Maximum buffer num */
	borg_msg_siz = 4096;

	/* Allocate a buffer */
	C_MAKE(borg_msg_buf, borg_msg_siz, char);

	/* No msg's saved yet */
	borg_msg_num = 0;

	/* Maximum number of messages */
	borg_msg_max = 128;

	/* Allocate array of positions */
	C_MAKE(borg_msg_pos, borg_msg_max, s16b);

	/* Allocate array of use-types */
	C_MAKE(borg_msg_use, borg_msg_max, s16b);


	/*** Object/Monster tracking ***/

	/* Hack -- Array of "wanks" */
	C_MAKE(borg_wanks, BORG_VIEW_MAX, auto_wank);


	/*** Reset the map ***/

	/* Forget the map */
	borg_forget_map();


	/*** Parse "unique" monster names ***/

	/* Start over */
	num = 0;

	/* Collect "unique" monsters */
	for (i = 1; i < MAX_R_IDX-1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Skip non-unique monsters */
		if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

		/* Use it */
		text[num] = r_name + r_ptr->name;
		what[num] = i;
		num++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort */
	ang_sort(text, what, num);

	/* Save the num */
	ab_ptr->unique_num = num;

	/* Allocate the arrays */
	C_MAKE(ab_ptr->unique_text, ab_ptr->unique_num, cptr);
	C_MAKE(ab_ptr->unique_what, ab_ptr->unique_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->unique_text[i] = text[i];
	for (i = 0; i < num; i++) ab_ptr->unique_what[i] = what[i];

	/* Precalculate the lengths */
	C_MAKE(ab_ptr->unique_size, ab_ptr->unique_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->unique_size[i] = strlen(text[i]);


	/*** Parse "normal" monster names ***/

	/* Start over */
	num = 0;

	/* Collect "normal" monsters */
	for (i = 1; i < MAX_R_IDX-1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Skip non-monsters */
		if (!r_ptr->name) continue;

		/* Skip unique monsters */
		if (r_ptr->flags1 & RF1_UNIQUE) continue;

		/* Use it */
		text[num] = r_name + r_ptr->name;
		what[num] = i;
		num++;
	}

	/* Set the sort hooks */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort */
	ang_sort(text, what, num);

	/* Save the num */
	ab_ptr->normal_num = num;

	/* Allocate the arrays */
	C_MAKE(ab_ptr->normal_text, ab_ptr->normal_num, cptr);
	C_MAKE(ab_ptr->normal_what, ab_ptr->normal_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->normal_text[i] = text[i];
	for (i = 0; i < num; i++) ab_ptr->normal_what[i] = what[i];

	/* Precalculate the lengths */
	C_MAKE(ab_ptr->normal_size, ab_ptr->normal_num, s16b);

	/* Save the entries */
	for (i = 0; i < num; i++) ab_ptr->normal_size[i] = strlen(text[i]);
}


/*
 * Initialize the Borg
 */
static errr borg_init(void)
{
	byte *test;

	s16b tester;

	/* Hack -- verify memory */
	C_MAKE(test, 300 * 1024L, byte);
	C_KILL(test, 300 * 1024L, byte);

	/* Hack -- verify integers */
	*(((byte*)(&tester))+0) = 0xFF;
	*(((byte*)(&tester))+1) = 0xFF;
	if (tester != -1) quit("Weird machine!");

	/* Initialize */
	borg_init_1();
	borg_init_3();
	borg_init_5();

	/* Initialize */
	borg_vinfo_init();

	/* Success */
	return (0);
}



/*
 * Hack -- interact with the Borg
 */
void do_cmd_borg(void)
{
	static bool initialize = TRUE;

	char cmd;


	/* Initialize */
	if (initialize)
	{
		/* Message */
		prt("Initializing the Borg...", 0, 0);

		/* Refresh */
		Term_fresh();

		/* Initialize */
		(void)borg_init();

		/* Clear line */
		prt("", 0, 0);

		/* Refresh */
		Term_fresh();

		/* Prepare */
		borg_prepare();

		/* Official message */
		borg_note("# Ready...");

		/* Now it is ready */
		initialize = FALSE;
	}


	/* Get a "Borg command", or abort */
	if (!get_com("Borg command: ", &cmd)) return;


	/* Simple help */
	if (cmd == '?')
	{
		int i = 2;

		/* Save screen */
		screen_save();

		/* Clear the screen */
		Term_clear();

		/* Dump commands */
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'z' runs the Borg.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'x' steps the Borg.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'u' updates the Borg.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'f' modifies some flags.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'l' activates a log file.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 's' activates search mode.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'i' displays grid info.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'a' displays borg_avoids.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 'k' displays monster info.");
		Term_putstr(5, i++, -1, TERM_WHITE, "Command 't' displays object info.");

		/* Dump header */
		Term_putstr(5, 23, -1, TERM_WHITE,
		            "[See the source code for a complete list of commands]");

		/* Prompt for key */
		msg_print("Available commands: z,x,u,f,l,s,i,a,k,t.");
		msg_print(NULL);

		/* Load screen */
		screen_load();

		/* Done */
		return;
	}


	/* Command: Run */
	if (cmd == 'z')
	{
		/* Run forever */
		borg_active = -1;

		/* Prepare */
		borg_prepare();

		/* Message */
		borg_note("# Installing 'inkey_hack' keypress stealer");

		/* Activate the key stealer */
		inkey_hack = borg_inkey_hack;
	}


	/* Command: Step */
	else if (cmd == 'x')
	{
		/* Step once */
		borg_active = 2;

		/* Step N times */
		if (p_ptr->command_arg > 0)
		{
			borg_active = p_ptr->command_arg + 1;
		}

		/* Prepare */
		borg_prepare();

		/* Message */
		borg_note("# Installing 'inkey_hack' keypress stealer");

		/* Activate the key stealer */
		inkey_hack = borg_inkey_hack;
	}


	/* Command: Update */
	else if (cmd == 'u')
	{
		/* Update */
		borg_active = 1;

		/* Prepare */
		borg_prepare();

		/* Message */
		borg_note("# Installing 'inkey_hack' keypress stealer");

		/* Activate the key stealer */
		inkey_hack = borg_inkey_hack;
	}


	/* Command: toggle "strategy" flags */
	else if (cmd == 'f')
	{
		/* Get a "Borg command", or abort */
		if (!get_com("Borg command: Toggle Savefile Flag (s/d): ", &cmd)) return;

		/* Dump savefile when level changes */
		if (cmd == 's')
		{
			borg_flag_save_level = !borg_flag_save_level;
			msg_format("Borg -- borg_flag_save_level is now %d.",
			           borg_flag_save_level);
		}

		/* Dump savefile when depth changes */
		else if (cmd == 'd')
		{
			borg_flag_save_depth = !borg_flag_save_depth;
			msg_format("Borg -- borg_flag_save_depth is now %d.",
			           borg_flag_save_depth);
		}
	}


	/* Start a new log file */
	else if (cmd == 'l')
	{
		char buf[81];

		/* Close the log file */
		if (borg_fff) my_fclose(borg_fff);

		/* Hack -- drop permissions */
		safe_setuid_drop();

		/* Default  */
		strcpy(buf, "borg.log");

		/* Get the name and open the log file */
		if (get_string("Borg Log File: ", buf, 80))
		{
			/* Open a new file */
			borg_fff = my_fopen(buf, "w");

			/* Failure */
			if (!borg_fff) msg_print("Cannot open that file.");
		}

		/* Oops */
		else if (borg_fff)
		{
			/* Mention closing */
			msg_print("Closed borg file.");

			/* Forget old file */
			borg_fff = NULL;
		}

		/* Hack -- grab permissions */
		safe_setuid_grab();
	}


	/* Activate a search string */
	else if (cmd == 's')
	{
		/* Get the new search string (or cancel the matching) */
		if (!get_string("Borg Match String: ", borg_match_string, 80))
		{
			/* Cancel it */
			strcpy(borg_match_string, "");

			/* Message */
			msg_print("Borg Match String de-activated.");
		}
	}


	/* Command: check "info" flags */
	else if (cmd == 'i')
	{
		int x, y;

		u16b mask = 0x00;

		/* Get a "Borg command", or abort */
		if (!get_com("Borg command: Show grids: ", &cmd)) return;

		/* Extract a flag */
		switch (cmd)
		{
			case '0': mask = (1 << 0); break;
			case '1': mask = (1 << 1); break;
			case '2': mask = (1 << 2); break;
			case '3': mask = (1 << 3); break;
			case '4': mask = (1 << 4); break;
			case '5': mask = (1 << 5); break;
			case '6': mask = (1 << 6); break;
			case '7': mask = (1 << 7); break;

			case 'm': mask |= (CAVE_MARK); break;
			case 'g': mask |= (CAVE_GLOW); break;
			case 'd': mask |= (CAVE_DARK); break;
			case 'h': mask |= (CAVE_HACK); break;
			case 's': mask |= (CAVE_SEEN); break;
			case 'v': mask |= (CAVE_VIEW); break;
			case 't': mask |= (CAVE_TEMP); break;
			case 'w': mask |= (CAVE_WALL); break;
		}

		/* Scan map */
		for (y = b_ptr->wy; y < b_ptr->wy + SCREEN_HGT; y++)
		{
			for (x = b_ptr->wx; x < b_ptr->wx + SCREEN_WID; x++)
			{
				byte a = TERM_RED;

				/* Given mask, show only those grids */
				if (mask && !(borg_cave_info[y][x] & mask)) continue;

				/* Given no mask, show unknown grids */
				if (!mask && (borg_cave_info[y][x] & (CAVE_MARK))) continue;

				/* Color */
				if (borg_cave_floor_bold(y, x)) a = TERM_YELLOW;

				/* Display */
				print_rel('*', a, y, x);
			}
		}

		/* Get keypress */
		msg_print("Press any key.");
		msg_print(NULL);

		/* Redraw map */
		prt_map();
	}


	/* Command: check actual danger */
	else if (cmd == 'a')
	{
		int x, y, p;

		int limit = (p_ptr->command_arg ? p_ptr->command_arg : borg_avoid/2);

		/* Scan map */
		for (y = b_ptr->wy; y < b_ptr->wy + SCREEN_HGT; y++)
		{
			for (x = b_ptr->wx; x < b_ptr->wx + SCREEN_WID; x++)
			{
				/* Obtain danger */
				p = borg_danger(y, x, 1);

				/* High danger */
				if (p > limit)
				{
					print_rel('*', TERM_RED, y, x);
				}
			}
		}

		/* Get keypress */
		msg_format("Danger %d and above.", limit);
		msg_print(NULL);

		/* Redraw map */
		prt_map();
	}


	/* Command: check memorized danger */
	else if (cmd == 'd')
	{
		int x, y, p;

		int limit = (p_ptr->command_arg ? p_ptr->command_arg : borg_avoid/2);

		/* Scan map */
		for (y = b_ptr->wy; y < b_ptr->wy + SCREEN_HGT; y++)
		{
			for (x = b_ptr->wx; x < b_ptr->wx + SCREEN_WID; x++)
			{
				/* Obtain danger */
				p = borg_cave_danger[y][x];

				/* Unknown danger */
				if (p < 0)
				{
					print_rel('*', TERM_YELLOW, y, x);
				}

				/* Irrelevant danger */
				else if (borg_do_wipe_danger)
				{
					print_rel('*', TERM_VIOLET, y, x);
				}

				/* High danger */
				else if (p > limit)
				{
					print_rel('*', TERM_RED, y, x);
				}
			}
		}

		/* Get keypress */
		msg_format("Danger %d and above.", limit);
		msg_print(NULL);

		/* Redraw map */
		prt_map();
	}


	/* Command: show "monsters" */
	else if (cmd == 'k')
	{
		int i, n = 0;

		/* Scan the monsters */
		for (i = 1; i < borg_kills_nxt; i++)
		{
			auto_kill *kill = &borg_kills[i];

			/* Still alive */
			if (kill->r_idx)
			{
				int x = kill->x;
				int y = kill->y;

				/* Display */
				print_rel('*', TERM_RED, y, x);

				/* Count */
				n++;
			}
		}

		/* Get keypress */
		msg_format("There are %d known monsters.", n);
		msg_print(NULL);

		/* Redraw map */
		prt_map();
	}


	/* Command: show "objects" */
	else if (cmd == 't')
	{
		int i, n = 0;

		/* Scan the objects */
		for (i = 1; i < borg_takes_nxt; i++)
		{
			auto_take *take = &borg_takes[i];

			/* Still alive */
			if (take->k_idx)
			{
				int x = take->x;
				int y = take->y;

				/* Display */
				print_rel('*', TERM_RED, y, x);

				/* Count */
				n++;
			}
		}

		/* Get keypress */
		msg_format("There are %d known objects.", n);
		msg_print(NULL);

		/* Redraw map */
		prt_map();
	}


	/* Command: show "projectable" */
	else if (cmd == 'p')
	{
		int py = b_ptr->py;
		int px = b_ptr->px;

		int x, y;

		/* Scan map */
		for (y = b_ptr->wy; y < b_ptr->wy + SCREEN_HGT; y++)
		{
			for (x = b_ptr->wx; x < b_ptr->wx + SCREEN_WID; x++)
			{
				byte a = TERM_RED;

				/* Verify flow cost */
				if (!borg_projectable(py, px, y, x)) continue;

				/* Display */
				print_rel('*', a, y, x);
			}
		}

		/* Get keypress */
		msg_format("Projectable grids.");
		msg_print(NULL);

		/* Redraw map */
		prt_map();
	}


	/* Command: debug -- current flow */
	else if (cmd == '%')
	{
		int i, x, y;

		char ch;

		/* Flow */
		for (i = 0; i < 250; i++)
		{
			int n = 0;

			/* Scan map */
			for (y = b_ptr->wy; y < b_ptr->wy + SCREEN_HGT; y++)
			{
				for (x = b_ptr->wx; x < b_ptr->wx + SCREEN_WID; x++)
				{
					byte a = TERM_RED;

					/* Verify flow cost */
					if (borg_flow_cost[y][x] != i) continue;

					/* Display */
					print_rel('*', a, y, x);

					/* Count */
					n++;
				}
			}

			/* Get keypress */
			(void)get_com(format("Flow depth %d (SPC/ESC): ", i), &ch);

			/* Redraw map */
			prt_map();

			/* Stop */
			if (ch == ESCAPE) break;
		}
	}


	/* Command: debug -- danger of grid */
	else if (cmd == '#')
	{
		int n, y, x;

		/* Turns */
		n = (p_ptr->command_arg ? p_ptr->command_arg : 1);

		/* Use target */
		if (target_okay())
		{
			y = p_ptr->target_row;
			x = p_ptr->target_col;
		}

		/* Use player */
		else
		{
			y = p_ptr->py;
			x = p_ptr->px;
		}

		/* Danger of grid */
		msg_format("Danger(%d,%d,%d) is %d",
		           y, x, n, borg_danger(y, x, n));
	}


	/* Command: debug -- fear of depth */
	else if (cmd == '_')
	{
		/* Max depth */
		msg_format("Max depth %d:", b_ptr->max_depth);

		/* Happy depth */
		msg_format("Happy depth %d:", borg_happy_depth);

		/* Happy count */
		msg_format("Happy count %d:", borg_happy_count);
	}


	/* XXX XXX XXX */
	else if (cmd == '&')
	{
		char buf[81];

		sprintf(buf, "%d", borg_when_began);

		if (get_string("Modify borg_when_began: ", buf, 80))
		{
			borg_when_began = atoi(buf);
		}
	}

	/* Keldon hack */
	else if (cmd == '$')
	{
		borg_notice();

		borg_note(format("# Power: %ld", borg_power()));
	}


	/* Oops */
	else
	{
		/* Message */
		msg_print("That is not a legal Borg command.");
	}
}


#else

#ifdef MACINTOSH
static int HACK = 0;
#endif /* MACINTOSH */

#endif /* ALLOW_BORG */

