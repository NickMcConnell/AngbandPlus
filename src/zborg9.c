/* File: zborg9.c */

/* Purpose: Highest level functions for the Borg -BEN- */
#include "angband.h"

#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"
#include "zborg4.h"
#include "zborg5.h"
#include "zborg6.h"
#include "zborg7.h"
#include "zborg8.h"
#include "zborg9.h"

/*
 * This file implements the "APWBorg", an "Automatic Angband Player".
 *
 * This version of the "APWBorg" is designed for use with ZAngband 2.7.x.
 *
 * Use of the "APWBorg" requires re-compilation with ALLOW_BORG defined,
 * and with the various "zborg*.c" files linked into the executable.
 *
 * Note that you can only use the Borg if your character has been marked
 * as a "Borg User".  You can do this, if necessary, by responding "y"
 * when asked if you really want to use the Borg.  This will (normally)
 * result in your character being inelligible for the high score list.
 *
 * The "do_cmd_borg()" function, called when the user hits "^Z", allows
 * the user to interact with the Borg.  You do so by typing "Borg Commands",
 * including 'z' to activate (or re-activate), 'K' to show monsters, 'T' to
 * show objects, 'd' to toggle "demo mode", 'f' to open/shut the "log file",
 * 'i' to display internal flags, etc.  See "do_cmd_borg()" for more info.
 *
 * The first time you enter a Borg command, the Borg is initialized.  This
 * consists of three major steps, and requires at least 400K of free memory,
 * if the memory is not available, the game may abort.
 *
 * (1) The various "borg" modules are initialized.
 *
 * (2) Some important "state" information is extracted, including the level
 *     and race/class of the player, and some more initialization is done.
 *
 * (3) Some "historical" information (killed uniques, maximum dungeon depth)
 *     is "stolen" from the game.
 *
 * When the Borg is "activated", it uses the "Term_inkey_hook" to steal
 * control from the user.  Later, if it detects any input from the real user,
 * it gracefully relinquishes control by clearing the "Term_inkey_hook" after
 * any pending key-sequences are complete.
 *
 * The Borg will abort if it detects any "errors", or if it detects any
 * "situations" such as "death", or if it detects any "panic" situations,
 * such as "imminent death", if the appropriate flags are set.
 *
 * The Borg is only supposed to "know" what is visible on the screen,
 * which it learns by using the "term.c" screen access function "Term_what()",
 * the cursor location function "Term_locate()", and the cursor visibility
 * extraction function "Term_get_cursor()".
 *
 * The Borg is only supposed to "send" keypresses when the "Term_inkey()"
 * function asks for a keypress, which is accomplished by using a special
 * function hook in the "z-term.c" file, which allows the Borg to "steal"
 * control from the "Term_inkey()" and "Term_flush()" functions.  This
 * allows the Borg to pretend to be a normal user.
 *
 * Note that if properly designed, the Borg could be run as an external
 * process, which would actually examine the screen (or pseudo-terminal),
 * and send keypresses directly to the keyboard (or pseudo-terminal).  Thus
 * it should never access any "game variables", unless it is able to extract
 * those variables for itself by code duplication or complex interactions,
 * or, in certain situations, if those variables are not actually "required".
 *
 * Currently, the Ben Borg is a few steps away from being able to be run as
 * an external process, primarily in the "low level" details, such as knowing
 * when the game is ready for a keypress.  Also, the Ben Borg assumes that a
 * character has already been rolled, and maintains no state between saves,
 * which is partially offset by "cheating" to "acquire" the maximum dungeon
 * depth, without which equipment analysis will be faulty.
 *
 * The "theory" behind the Borg is that is should be able to run as a
 * separate process, playing Angband in a window just like a human, that
 * is, examining the screen for information, and sending keypresses to
 * the game.  The current Borg does not actually do this, because it would
 * be very slow and would not run except on Unix machines, but as far as
 * possible, I have attempted to make sure that the Borg *could* run that
 * way.  This involves "cheating" as little as possible, where "cheating"
 * means accessing information not available to a normal Angband player.
 * And whenever possible, this "cheating" should be optional, that is,
 * there should be software options to disable the cheating, and, if
 * necessary, to replace it with "complex" parsing of the screen.
 *
 * Thus, the Borg COULD be written as a separate process which runs Angband
 * in a pseudo-terminal and "examines" the "screen" and sends keypresses
 * directly (as with a terminal emulator), although it would then have
 * to explicitly "wait" to make sure that the game was completely done
 * sending information.
 *
 * The Borg is thus allowed to examine the screen directly (by efficient
 * direct access of the "Term->scr->a" and "Term->scr->c" arrays, which
 * could be replaced by calls to "Term_grab()"), and to access the cursor
 * location (via "Term_locate()") and visibility (via "Term_get_cursor()"),
 * and, as mentioned above, the Borg is allowed to send keypresses directly
 * to the game, and only when needed, using the "Term_inkey_hook" hook, and
 * uses the same hook to know when it should discard all pending keypresses.
 *
 * The Borg should not know when the game is ready for a keypress, and
 * should really do something nasty such as "pause" between turns for
 * some amount of time to ensure that the game is really waiting for
 * a keypress.
 *
 * Various other "cheats" (mostly optional) are described where they are
 * used, primarily in this file.
 *
 * Note that any "user input" will be ignored, and will cancel the Borg,
 * after the Borg has completed any key-sequences currently in progress.
 *
 * Note that the "borg_t" parameter bears a close resemblance to the number of
 * "player turns" that have gone by.  Except that occasionally, the Borg will
 * do something that he *thinks* will take time but which actually does not
 * (for example, attempting to open a hallucinatory door), and that sometimes,
 * the Borg performs a "repeated" command (rest, open, tunnel, or search),
 * which may actually take longer than a single turn.  This has the effect
 * that the "borg_t" variable is slightly lacking in "precision".
 *
 * Note that as of 2.7.9, the Borg can play any class, that is, he can make
 * "effective" use of at least a few spells/prayers, and is not "broken"
 * by low strength, blind-ness, hallucination, etc.  This does not, however,
 * mean that he plays all classes equally well, especially since he is so
 * dependant on a high strength for so many things.  The "demo" mode is useful
 * for many classes (especially Mage) since it allows the Borg to "die" a few
 * times, without being penalized.
 *
 * The Borg assumes that the "maximize" flag is off, and that the
 * "preserve" flag is on, since he cannot actually set those flags.
 * If the "maximize" flag is on, the Borg may not work correctly.
 * If the "preserve" flag is off, the Borg may miss artifacts.
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
 *   Long object descriptions may have clipped inscriptions
 *
 * We need to handle:
 *   Technically a room can have no exits, requiring digging
 *   Conserve memory space (each grid byte costs about 15K)
 *   Conserve computation time (especially with the flow code)
 *
 * We need to handle "loading" saved games:
 *   The "max_depth" value is lost if loaded in the town
 *   If we track "dead uniques" then this information is lost
 *   The "map" info, "flow" info, "tracking" info, etc is lost
 *   The contents of the shops (and the home) are lost
 *   We may be asked to "resume" a non-Borg character (icky)
 */


/*
 * Currently, the Borg "cheats" in a few situations...
 *
 * Cheats that are significant, and possibly unavoidable:
 *   Knowledge of when we are being asked for a keypress.
 *   Note that this could be avoided by LONG timeouts/delays
 *
 * Cheats "required" by implementation, but not signifant:
 *   Direct access to the "screen image" (parsing screen)
 *   Direct access to the "keypress queue" (sending keys)
 *   Direct access to the "cursor visibility" (game state)
 *
 * Cheats that could be avoided by simple (ugly) code:
 *   Direct modification of the "current options"
 *
 * Cheats that could be avoided by duplicating code:
 *   Use of the tables in "tables.c"
 *   Use of the arrays initialized in "init.c"
 *
 * Cheats that the Borg would love:
 *   Immunity to hallucination, blindness, confusion
 *   Unique attr/char codes for every monster and object
 *   Removal of the "mimic" and "trapper" monsters
 *   Removal of the "mushroom" and "gold" monsters
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
 * Some variables
 */

/* Is the borg initialized yet? */
static bool initialized;

/*
 * Mega-Hack -- extract some "hidden" variables
 *
 * XXX XXX XXX This step would not be necessary if more info
 * was available on the screen.
 *
 */
static void borg_hidden(void)
{
	int i;

	/* Clear the stat modifiers */
	for (i = 0; i < A_MAX; i++) my_stat_add[i] = 0;

	/* Scan the usable inventory */
	for (i = 0; i < equip_num; i++)
	{
		list_item *l_ptr = look_up_equip_slot(i);

		/* Skip empty items */
		if (!l_ptr) continue;

		/* Affect stats */
		if (KN_FLAG(l_ptr, TR_STR)) my_stat_add[A_STR] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_INT)) my_stat_add[A_INT] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_WIS)) my_stat_add[A_WIS] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_DEX)) my_stat_add[A_DEX] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_CON)) my_stat_add[A_CON] += l_ptr->pval;
		if (KN_FLAG(l_ptr, TR_CHR)) my_stat_add[A_CHR] += l_ptr->pval;
	}

	/* Mega-Hack -- Guess at "my_stat_cur[]" */
	for (i = 0; i < A_MAX; i++)
	{
		if (!my_need_stat_check[i]) continue;

		/* Hack - get current internal stat value */
		my_stat_cur[i] = p_ptr->stat[i].cur;

		/* Max stat is the max that the cur stat ever is. */
		if (my_stat_cur[i] > my_stat_max[i])
		{
			my_stat_max[i] = my_stat_cur[i];
		}
	}
}


/*
 * Think about the world and perform an action
 *
 * Check inventory/equipment/spells/panel once per "turn"
 *
 * Process "store" and other modes when necessary
 *
 * Technically, we should attempt to parse all the messages that
 * indicate that it is necessary to re-parse the books, and only
 * set the appropriate flags at that point.  This would not only
 * reduce the potential screen flashing, but would also optimize
 * the code a lot.  For paranoia, we could always select items
 * and spells using capital letters.
 */
static bool borg_think(void)
{
	int i;

	static char svSavefile[1024];
	static bool justSaved = FALSE;


	/*** Process inventory/equipment ***/

	/* save now */
	if (borg_save && borg_save_game())
	{
		/* Log */
		borg_note("# Auto Save!");

		borg_save = FALSE;

		return (TRUE);
	}
	if (justSaved)
	{
		memcpy(savefile, svSavefile, sizeof(savefile));
		borg_save_game();
		justSaved = FALSE;
		return (TRUE);
	}

	/*** Find books ***/

	/* Only if needed */
	if (borg_do_spell)
	{
		/* Assume no books */
		for (i = 0; i < 4; i++)
		{
			borg_book[bp_ptr->realm1][i] = -1;
			borg_book[bp_ptr->realm2][i] = -1;
		}

		/* Scan the pack */
		for (i = 0; i < inven_num; i++)
		{
			list_item *l_ptr = &inventory[i];
			int realm;

			/* Stop after the books have run out */
			if (l_ptr->tval < TV_BOOKS_MIN) break;

			/* Which realm is that? */
			realm = l_ptr->tval - TV_BOOKS_MIN + 1;

			/* Does this book belong to a realm that the borg knows */
			if (!borg_has_realm(realm)) continue;

			/* Make a note where in the inv the book is */
			borg_book[realm][k_info[l_ptr->k_idx].sval] = i;
		}
	}

	/*** Process books ***/

	/* Hack -- Warriors never browse */
	if (borg_class == CLASS_WARRIOR) borg_do_spell = FALSE;

	/* XXX XXX XXX Dark */

	/* Cheat */
	if (borg_do_spell)
	{
		/* Only do it once for each realm */
		borg_do_spell = FALSE;

		/* Cheat that realm */
		borg_cheat_spell(bp_ptr->realm1);
		borg_cheat_spell(bp_ptr->realm2);


		/* Done */
		return (FALSE);
	}

	/* If king, maybe retire. */
	if (bp_ptr->winner)
	{
		/* Prepare to retire */
		if (borg_stop_king)
		{
			borg_oops("retire");
		}
	}

	/*** Handle stores ***/

	/* Hack -- Check for being in a store */
	if (borg_term_text_comp(53, 19, "Gold Remaining") ||
		borg_term_text_comp(40, 23, "Gold Remaining"))
	{
		/* Hack -- allow user abort */
		if (borg_cancel) return (TRUE);

		/* Think until done */
		return (borg_think_store());
	}

	/*** Analyze the Frame ***/

	/* Analyze the frame */
	if (borg_do_frame)
	{
		/* Only once */
		borg_do_frame = FALSE;

		/* Analyze the "frame" */
		borg_update_frame();
	}

	/*** Re-activate Tests ***/

	/* Check frame again later */
	borg_do_frame = TRUE;

	/* Check spells again later */
	borg_do_spell = TRUE;

	/*** Think about it ***/

	/* Increment the clock */
	borg_t++;

	/* Examine the screen */
	borg_update();

	/* Extract some "hidden" variables */
	borg_hidden();

	/* Hack -- allow user abort */
	if (borg_cancel) return (TRUE);

	/* Do something */
	return (borg_think_dungeon());
}


/*
 * Hack -- methods of hitting a monster (order not important).
 */
static cptr prefix_hit[] =
{
	"You hit ",
	"You strike ",
	"You hack at ",
	"You bash ",
	"You slash ",
	"You pound ",
	"You score ",
	"You batter ",
	"You gouge ",
	"You bludgeon ",
	"You *smite* ",
	"You bite ",
	"You claw ",
	NULL
};

/*
 * Hack -- methods of hurting a monster (order not important).
 *
 * See "message_pain()" for details.
 */
static cptr suffix_pain[] =
{
	" is unharmed.",
	" barely notices.",
	" flinches.",
	" squelches.",
	" quivers in pain.",
	" writhes about.",
	" writhes in agony.",
	" jerks limply.",

	" roars thunderously.",
	" rumbles.",
	" grunts",
	" hesitates",
	" crumples",

	" hisses.",
	" rears up in anger.",
	" hisses furiously.",
	" roars.",
	" mewls in pain.",
	" mewls pitifully.",

	" chitters.",
	" scuttles about.",
	" twitters.",
	" twitches.",
	" chirps.",
	" squawks.",
	" chatters.",
	" jeers.",
	" flutters about.",
	" squeaks.",

	" snarls with pain.",
	" roars with pain.",
	" gasps.",
	" snarls feebly.",
	" rattles.",
	" stumbles.",
	" staggers.",
	" clatters.",
	" groans.",
	" moans.",
	" hesitates.",
	" grunts.",
	" wails.",
	" howls.",
	" moans softly.",
	" sighs.",
	" shrieks in pain.",
	" shrieks in agony.",

	" spawns!",
	" looks healthier.",
	" starts moving faster.",
	" starts moving slower.",

	" is unaffected!",
	" is immune.",
	" resists a lot.",
	" resists.",
	" resists somewhat.",

	" shrugs off the attack.",
	" snarls with pain.",
	" yelps in pain.",
	" howls in pain.",
	" howls in agony.",
	/* xxx */
	" yelps feebly.",

	" ignores the attack.",
	" grunts with pain.",
	" squeals in pain.",
	" shrieks in pain.",
	" shrieks in agony.",
	/* xxx */
	" cries out feebly.",

	/* xxx */
	/* xxx */
	" cries out in pain.",
	" screams in pain.",
	" screams in agony.",
	/* xxx */
	" cringes from the light!",
	" loses some skin!",

	" is hit hard.",

	NULL
};


/*
 * Hack -- methods of killing a monster (order not important).
 *
 * See "mon_take_hit()" for details.
 */
static cptr prefix_kill[] =
{
	"You have killed ",
	"You have slain ",
	"You have destroyed ",
	NULL
};


/*
 * Hack -- methods of monster death (order not important).
 *
 * See "project_m()", "do_cmd_fire()", "mon_take_hit()" for details.
 */
static cptr suffix_died[] =
{
	" dies.",
	" dies from the Quivering Palm.",
	" is destroyed.",
	" is killed.",
	" dissolves!",
	" shrivels away in the light!",
	" collapses, a mindless husk.",
	NULL
};
static cptr suffix_blink[] =
{
	" disappears!",	/* from teleport other */
	" changes!",	/* from polymorph spell */
	" teleports away.",	/* RF5_TPORT */
	" blinks away.",	/* RF5_BLINK */
	NULL
};

/*
 * Hack -- methods of hitting the player (order not important).
 *
 * The "insult", "moan", and "begs you for money" messages are ignored.
 *
 * See "make_attack_normal()" for details.
 */
static cptr suffix_hit_by[] =
{
	" hits you.",
	" touches you.",
	" punches you.",
	" kicks you.",
	" claws you.",
	" bites you.",
	" stings you.",
	" butts you.",
	" crushes you.",
	" engulfs you.",
	" charges you.",
	" crawls on you.",
	" drools on you.",
	" spits on you.",
	" gazes at you.",
	" wails at you.",
	" releases spores at you.",
	NULL
};


/*
 * Hack -- methods of casting spells at the player (order important).
 *
 * See "make_attack_spell()" for details.
 */
/* AJG These had gotten out of synch with where they are used. */
static cptr suffix_spell[] =
{
	" makes a high pitched shriek.",	/* 0 RF3_SHRIEK */
	" tries to cast a spell, but fails.",	/* 1 RF3_FAILS */
	" does something.",	/* 2 RF3_XXX3X4 */
	" does something.",	/* 3 RF3_XXX4X4 */
	" fires an arrow.",	/* 4 RF3_ARROW */
	" fires an arrow!",	/* 5 RF3_XXX6 */
	" fires a missile.",	/* 6 RF3_XXX7 */
	" fires a missile!",	/* 7 RF3_XXX8 */
	" breathes acid.",	/* 8 RF3_BR_ACID */
	" breathes lightning.",	/* 9 RF3_BR_ELEC */
	" breathes fire.",	/*10 RF3_BR_FIRE */
	" breathes frost.",	/*11 RF3_BR_COLD */
	" breathes gas.",	/*12 RF3_BR_POIS */
	" breathes nether.",	/*13 RF3_BR_NETH */
	" breathes light.",	/*14 RF3_BR_LITE */
	" breathes darkness.",	/*15 RF3_BR_DARK */
	" breathes confusion.",	/*16 RF3_BR_CONF */
	" breathes sound.",	/*17 RF3_BR_SOUN */
	" breathes chaos.",	/*18 RF3_BR_CHAO */
	" breathes disenchantment.",	/*19 RF3_BR_DISE */
	" breathes nexus.",	/*20 RF3_BR_NEXU */
	" breathes time.",	/*21 RF3_BR_TIME */
	" breathes inertia.",	/*22 RF3_BR_INER */
	" breathes gravity.",	/*23 RF3_BR_GRAV */
	" breathes shards.",	/*24 RF3_BR_SHAR */
	" breathes plasma.",	/*25 RF3_BR_PLAS */
	" breathes force.",	/*26 RF3_BR_WALL */
	" does something.",	/*27 RF3_BR_MANA */
	" does something.",	/*28 RF3_XXX5X4 */
	" does something.",	/*29 RF3_XXX6X4 */
	" does something.",	/*30 RF3_XXX7X4 */
	" does something.",	/*31 RF3_XXX8X4 */
	" casts an acid ball.",	/*32 RF4_BA_ACID */
	" casts a lightning ball.",	/*33 RF4_BA_ELEC */
	" casts a fire ball.",	/*34 RF4_BA_FIRE */
	" casts a frost ball.",	/*35 RF4_BA_COLD */
	" casts a stinking cloud.",	/*36 RF4_BA_POIS */
	" casts a nether ball.",	/*37 RF4_BA_NETH */
	" gestures fluidly.",	/*38 RF4_BA_WATE */
	" invokes a mana storm.",	/*39 RF4_BA_MANA */
	" invokes a darkness storm.",	/*40 RF4_BA_DARK */
	" draws psychic energy from you!",	/*41 RF4_DRAIN_MANA */
	" gazes deep into your eyes.",	/*42 RF4_MIND_BLAST */
	" looks deep into your eyes.",	/*43 RF4_BRAIN_SMASH */
	" points at you and curses.",	/*44 RF4_CAUSE_1 */
	" points at you and curses horribly.",	/*45 RF4_CAUSE_2 */
	" points at you, incanting terribly!",	/*46 RF4_CAUSE_3 */
	" points at you, screaming the word DIE!",	/*47 RF4_CAUSE_4 */
	" casts a acid bolt.",	/*48 RF4_BO_ACID */
	" casts a lightning bolt.",	/*49 RF4_BO_ELEC */
	" casts a fire bolt.",	/*50 RF4_BO_FIRE */
	" casts a frost bolt.",	/*51 RF4_BO_COLD */
	" does something.",	/*52 RF4_BO_POIS */
	" casts a nether bolt.",	/*53 RF4_BO_NETH */
	" casts a water bolt.",	/*54 RF4_BO_WATE */
	" casts a mana bolt.",	/*55 RF4_BO_MANA */
	" casts a plasma bolt.",	/*56 RF4_BO_PLAS */
	" casts an ice bolt.",	/*57 RF4_BO_ICEE */
	" casts a magic missile.",	/*58 RF4_MISSILE */
	" casts a fearful illusion.",	/*59 RF4_SCARE */
	" casts a spell, burning your eyes!",	/*60 RF4_BLIND */
	" creates a mesmerising illusion.",	/*61 RF4_CONF */
	" drains power from your muscles!",	/*62 RF4_SLOW */
	" stares deep into your eyes!",	/*63 RF4_HOLD */
	" concentrates on XXX body.",	/*64 RF5_HASTE */
	" does something.",	/*65 RF5_XXX1X6 */
	" concentrates on XXX wounds.",	/*66 RF5_HEAL */
	" does something.",	/*67 RF5_XXX2X6 */
	" does something.",	/*68 RF5_XXX3X6 */
	" does something.",	/*69 RF5_XXX4X6 */
	" commands you to return.",	/*70 RF5_TELE_TO */
	" teleports you away.",	/*71 RF5_TELE_AWAY */
	" gestures at your feet.",	/*72 RF5_TELE_LEVEL */
	" does something.",	/*73 RF5_XXX5 */
	" gestures in shadow.",	/*74 RF5_DARKNESS */
	" casts a spell and cackles evilly.",	/*75 RF5_TRAPS */
	" tries to blank your mind.",	/*76 RF5_FORGET */
	" does something.",	/*77 RF5_XXX6X6 */
	" does something.",	/*78 RF5_XXX7X6 */
	" does something.",	/*79 RF5_XXX8X6 */
	" magically summons help!",	/*80 RF5_S_MONSTER */
	" magically summons monsters!",	/*81 RF5_S_MONSTERS */
	" magically summons ants.",	/*82 RF5_S_ANT */
	" magically summons spiders.",	/*83 RF5_S_SPIDER */
	" magically summons hounds.",	/*84 RF5_S_HOUND */
	" magically summons hydras.",	/*85 RF5_S_HYDRA */
	" magically summons an angel!",	/*86 RF5_S_ANGEL */
	" magically summons a demon from the Courts of Chaos!",	/*87 RF5_S_DEMON */
	" magically summons an undead adversary!",	/*88 RF5_S_UNDEAD */
	" magically summons a dragon!",	/*89 RF5_S_DRAGON */
	" magically summons greater undead!",	/*90 RF5_S_HI_UNDEAD */
	" magically summons ancient dragons!",	/*91 RF5_S_HI_DRAGON */
	" magically summons mighty undead opponents!",	/*92 RF5_S_WRAITH */
	" magically summons special opponents!",	/*93 RF5_S_UNIQUE */

	NULL
};



#if 0
/* XXX XXX XXX */
msgf("%^s looks healthier.", m_name);
msgf("%^s looks REALLY healthy!", m_name);
#endif



/*
 * Hack -- Spontaneous level feelings (order important).
 *
 * See "do_cmd_feeling()" for details.
 */
static cptr prefix_feeling[] =
{
	"Looks like any other level",
	"You feel there is something special",
	"You have a superb feeling",
	"You have an excellent feeling",
	"You have a very good feeling",
	"You have a good feeling",
	"You feel strangely lucky",
	"You feel your luck is turning",
	"You like the look of this place",
	"This level can't be all bad",
	"What a boring place",
	NULL
};



/*
 * Hack -- Parse a message from the world
 *
 * Note that detecting "death" is EXTREMELY important, to prevent
 * all sorts of errors arising from attempting to parse the "tomb"
 * screen, and to allow the user to "observe" the "cause" of death.
 *
 * Note that detecting "failure" is EXTREMELY important, to prevent
 * bizarre situations after failing to use a staff of perceptions,
 * which would otherwise go ahead and send the "item index" which
 * might be a legal command (such as "a" for "aim").  This method
 * is necessary because the Borg cannot parse "prompts", and must
 * assume the success of the prompt-inducing command, unless told
 * otherwise by a failure message.  Also, we need to detect failure
 * because some commands, such as detection spells, need to induce
 * furthur processing if they succeed, but messages are only given
 * if the command fails.
 *
 * Note that certain other messages may contain useful information,
 * and so they are "analyzed" and sent to "borg_react()", which just
 * queues the messages for later analysis in the proper context.
 *
 * Along with the actual message, we send a special formatted buffer,
 * containing a leading "opcode", which may contain extra information,
 * such as the index of a spell, and an "argument" (for example, the
 * capitalized name of a monster), with a "colon" to separate them.
 *
 * XXX XXX XXX Several message strings take a "possessive" of the form
 * "his" or "her" or "its".  These strings are all represented by the
 * encoded form "XXX" in the various match strings.  Unfortunately,
 * the encode form is never decoded, so the Borg currently ignores
 * messages about several spells (heal self and haste self).
 *
 * XXX XXX XXX We notice a few "terrain feature" messages here so
 * we can acquire knowledge about wall types and door types.
 */
static void borg_parse_aux(cptr msg, int len)
{
	int i, tmp;

	char who[256];
	char buf[256];

	/* Log (if needed) */
	if (borg_fff) froff(borg_fff, "& Msg <%s>\n", msg);


	/* Hack -- Notice death */
	if (prefix(msg, "You die."))
	{
		/* Abort (unless cheating) */
		if (!(p_ptr->state.wizard || cheat_live))
		{
			/* Abort */
			borg_oops("death");

			/* Abort right now! */
			borg_active = FALSE;

			/* Noise XXX XXX XXX */
			Term_xtra(TERM_XTRA_NOISE, 1);
		}

		/* Done */
		return;
	}


	/* Hack -- Notice "failure" */
	if (prefix(msg, "You failed "))
	{
		/* Hack -- store the keypress */
		borg_note("# Normal failure.");

		/* Set the failure flag */
		borg_failure = TRUE;

		/* Flush our key-buffer */
		borg_flush();

		/* If we were casting a targetted spell and failed */
		/* it does not mean we can't target that location */
		successful_target = BORG_TARGET;

		return;
	}
	
	/* Hack -- Notice "failure" */
	if (prefix(msg, "Illegal "))
	{
		/* Hack -- store the keypress */
		borg_note("# Failure?");

		/* Set the failure flag */
		borg_failure = TRUE;

		/* Flush our key-buffer */
		borg_flush();
		
		/* Stop borg so we can debug the problem. */
		borg_active = FALSE;

		return;
	}


	/* Ignore teleport trap */
	if (prefix(msg, "You hit a teleport")) return;

	/* Ignore arrow traps */
	if (prefix(msg, "An arrow ")) return;

	/* Ignore dart traps */
	if (prefix(msg, "A small dart ")) return;

	if (prefix(msg, "The cave "))
	{
		borg_react(msg, "QUAKE");
		return;
	}

	/* need to check stat */
	if (prefix(msg, "You feel very") ||
		prefix(msg, "You feel less") || prefix(msg, "Wow!  You feel very"))
	{
		/* need to check str */
		if (prefix(msg, "You feel very weak"))
		{
			my_need_stat_check[0] = TRUE;
		}
		if (prefix(msg, "You feel less weak"))
		{
			my_need_stat_check[0] = TRUE;
		}
		if (prefix(msg, "Wow!  You feel very strong"))
		{
			my_need_stat_check[0] = TRUE;
		}

		/* need to check int */
		if (prefix(msg, "You feel very stupid"))
		{
			my_need_stat_check[1] = TRUE;
		}
		if (prefix(msg, "You feel less stupid"))
		{
			my_need_stat_check[1] = TRUE;
		}
		if (prefix(msg, "Wow!  You feel very smart"))
		{
			my_need_stat_check[1] = TRUE;
		}

		/* need to check wis */
		if (prefix(msg, "You feel very naive"))
		{
			my_need_stat_check[2] = TRUE;
		}
		if (prefix(msg, "You feel less naive"))
		{
			my_need_stat_check[2] = TRUE;
		}
		if (prefix(msg, "Wow!  You feel very wise"))
		{
			my_need_stat_check[2] = TRUE;
		}

		/* need to check dex */
		if (prefix(msg, "You feel very clumsy"))
		{
			my_need_stat_check[3] = TRUE;
		}
		if (prefix(msg, "You feel less clumsy"))
		{
			my_need_stat_check[3] = TRUE;
		}
		if (prefix(msg, "Wow!  You feel very dextrous"))
		{
			my_need_stat_check[3] = TRUE;
		}

		/* need to check con */
		if (prefix(msg, "You feel very sickly"))
		{
			my_need_stat_check[4] = TRUE;
		}
		if (prefix(msg, "You feel less sickly"))
		{
			my_need_stat_check[4] = TRUE;
		}
		if (prefix(msg, "Wow!  You feel very healthy"))
		{
			my_need_stat_check[4] = TRUE;
		}

		/* need to check cha */
		if (prefix(msg, "You feel very ugly"))
		{
			my_need_stat_check[5] = TRUE;
		}
		if (prefix(msg, "You feel less ugly"))
		{
			my_need_stat_check[5] = TRUE;
		}
		if (prefix(msg, "Wow!  You feel very cute"))
		{
			my_need_stat_check[5] = TRUE;
		}
	}

	/* time attacks, just do all stats. */
	if (prefix(msg, "You're not as"))
	{
		my_need_stat_check[0] = TRUE;
		my_need_stat_check[1] = TRUE;
		my_need_stat_check[2] = TRUE;
		my_need_stat_check[3] = TRUE;
		my_need_stat_check[4] = TRUE;
		my_need_stat_check[5] = TRUE;
	}

	/* Nexus attacks, need to check everything! */
	if (prefix(msg, "Your body starts to scramble..."))
	{
		my_need_stat_check[0] = TRUE;
		my_need_stat_check[1] = TRUE;
		my_need_stat_check[2] = TRUE;
		my_need_stat_check[3] = TRUE;
		my_need_stat_check[4] = TRUE;
		my_need_stat_check[5] = TRUE;

		/* max stats may have lowered */
		my_stat_max[0] = 0;
		my_stat_max[1] = 0;
		my_stat_max[2] = 0;
		my_stat_max[3] = 0;
		my_stat_max[4] = 0;
		my_stat_max[5] = 0;

	}

	if (streq(msg, "You have been knocked out."))
	{
		borg_note("Ignoring Messages While KO'd");
		borg_dont_react = TRUE;
	}

	if (streq(msg, "You are paralyzed"))
	{
		borg_note("Ignoring Messages While Paralyzed");
		borg_dont_react = TRUE;
	}

	/* "You have hit it." (etc) */
	for (i = 0; prefix_hit[i]; i++)
	{
		/* "You have hit it." (etc) */
		if (prefix(msg, prefix_hit[i]))
		{
			tmp = strlen(prefix_hit[i]);
			strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
			strnfmt(buf, 256, "HIT:%^s", who);
			borg_react(msg, buf);
			return;
		}
	}

	/* Miss somebody */
	if (prefix(msg, "You miss "))
	{
		tmp = strlen("You miss ");
		strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
		strnfmt(buf, 256, "MISS:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* Miss somebody (because of fear) */
	if (prefix(msg, "You are too afraid to attack "))
	{
		tmp = strlen("You are too afraid to attack ");
		strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
		strnfmt(buf, 256, "MISS:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/*
	 * An inventory item was unaffected by an attack.
	 * It should be filtered out because otherwise the
	 * item is interpreted as a monster and the game unhooks.
	 *
	 * This also filters out messages about pets that are unaffected.
	 */
	if (prefix(msg, "Your ") && suffix(msg, "is unaffected!")) return;

	/* "It screams in pain." (etc) */
	for (i = 0; suffix_pain[i]; i++)
	{
		/* "It screams in pain." (etc) */
		if (suffix(msg, suffix_pain[i]))
		{
			tmp = strlen(suffix_pain[i]);
			strnfmt(who, 1 + len - tmp, "%s", msg);
			strnfmt(buf, 256, "PAIN:%^s", who);
			borg_react(msg, buf);
			return;
		}
	}


	/* "You have killed it." (etc) */
	for (i = 0; prefix_kill[i]; i++)
	{
		/* "You have killed it." (etc) */
		if (prefix(msg, prefix_kill[i]))
		{
			tmp = strlen(prefix_kill[i]);
			strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
			strnfmt(buf, 256, "KILL:%^s", who);
			borg_react(msg, buf);
			return;
		}
	}


	/* "It dies." (etc) */
	for (i = 0; suffix_died[i]; i++)
	{
		/* "It dies." (etc) */
		if (suffix(msg, suffix_died[i]))
		{
			tmp = strlen(suffix_died[i]);
			strnfmt(who, 1 + len - tmp, "%s", msg);
			strnfmt(buf, 256, "DIED:%^s", who);
			borg_react(msg, buf);
			return;
		}
	}

	/* "It blinks or telports." (etc) */
	for (i = 0; suffix_blink[i]; i++)
	{
		/* "It teleports." (etc) */
		if (suffix(msg, suffix_blink[i]))
		{
			tmp = strlen(suffix_blink[i]);
			strnfmt(who, 1 + len - tmp, "%s", msg);
			strnfmt(buf, 256, "BLINK:%^s", who);
			borg_react(msg, buf);
			return;
		}
	}

	/* "It misses you." */
	if (suffix(msg, " misses you."))
	{
		tmp = strlen(" misses you.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "MISS_BY:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* "It is repelled.." */
	/* treat as a miss */
	if (suffix(msg, " is repelled."))
	{
		tmp = strlen(" is repelled.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "MISS_BY:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* Ignore talking monsters */
	if (strstr(msg, " says,")) return;

	/* Ignore monsters picking up objects*/
	if (strstr(msg, " picks up ")) return;

	/* "It hits you." (etc) */
	for (i = 0; suffix_hit_by[i]; i++)
	{
		/* "It hits you." (etc) */
		if (suffix(msg, suffix_hit_by[i]))
		{
			tmp = strlen(suffix_hit_by[i]);
			strnfmt(who, 1 + len - tmp, "%s", msg);
			strnfmt(buf, 256, "HIT_BY:%^s", who);
			borg_react(msg, buf);

			/* If I was hit, then I am not on a glyph */
			if (track_glyph_num)
			{
				/* erase them all and
				 * allow the borg to scan the screen and rebuild the array.
				 * He won't see the one under him though.  So a special check
				 * must be made.
				 */
				/* byte feat = mb_ptr->feat; */

				/* Remove the entire array */
				for (i = 0; i < track_glyph_num; i++)
				{
					/* Stop if we already new about this glyph */
					track_glyph_x[i] = 0;
					track_glyph_y[i] = 0;
				}
				track_glyph_num = 0;
			}

			return;
		}
	}


	/* "It casts a spell." (etc) */
	for (i = 0; suffix_spell[i]; i++)
	{
		/* "It casts a spell." (etc) */
		if (suffix(msg, suffix_spell[i]))
		{
			tmp = strlen(suffix_spell[i]);
			strnfmt(who, 1 + len - tmp, "%s", msg);
			strnfmt(buf, 256, "SPELL_%03d:%^s", i, who);
			borg_react(msg, buf);
			return;
		}
	}

#if 0
	/* State -- Paralyzed */
	if (suffix(msg, " is paralyzed!"))
	{
		tmp = strlen(" is paralyzed!");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE_INVLUN:%^s", who);
		borg_react(msg, buf);
		return;
	}
#endif

	/* State -- Asleep */
	if (suffix(msg, " falls asleep!"))
	{
		tmp = strlen(" falls asleep!");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE_SLEEP:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- confused */
	if (suffix(msg, " looks confused."))
	{
		tmp = strlen(" looks confused.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE_CONFUSED:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- confused */
	if (suffix(msg, " looks more confused."))
	{
		tmp = strlen(" looks more confused.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE_CONFUSED:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- Not Asleep */
	if (suffix(msg, " wakes up."))
	{
		tmp = strlen(" wakes up.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE_AWAKE:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- Afraid */
	if (suffix(msg, " flees in terror!"))
	{
		tmp = strlen(" flees in terror!");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE__FEAR:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- Not Afraid */
	if (suffix(msg, " recovers his courage."))
	{
		tmp = strlen(" recovers his courage.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE__BOLD:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- Not Afraid */
	if (suffix(msg, " recovers her courage."))
	{
		tmp = strlen(" recovers her courage.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE__BOLD:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* State -- Not Afraid */
	if (suffix(msg, " recovers its courage."))
	{
		tmp = strlen(" recovers its courage.");
		strnfmt(who, 1 + len - tmp, "%s", msg);
		strnfmt(buf, 256, "STATE__BOLD:%^s", who);
		borg_react(msg, buf);
		return;
	}

	/* Feature XXX XXX XXX */
	if (streq(msg, "The door appears to be broken."))
	{
		/* Clear goals */
		goal = GOAL_NONE;

		return;
	}

	/* Feature XXX XXX XXX */
	if (streq(msg, "The door appears to be stuck."))
	{
		/* Clear goals */
		goal = GOAL_NONE;

		return;
	}



	/* Feature XXX XXX XXX */
	if (streq(msg, "This seems to be permanent rock."))
	{

		/* Clear goals */
		goal = GOAL_NONE;

		return;
	}

	/* Feature XXX XXX XXX */
	if (streq(msg, "You tunnel into the granite wall."))
	{
		/* Clear goals */
		goal = GOAL_NONE;
	
		return;
	}

	/* Feature Invisible Walls */
	if (streq(msg, "You bump into something."))
	{
		/* Clear goals */
		goal = GOAL_NONE;
	
		return;
	}

	/* Feature XXX XXX XXX */
	if (streq(msg, "You tunnel into the quartz vein."))
	{
		/* Clear goals */
		goal = GOAL_NONE;
	
		return;
	}

	/* Feature XXX XXX XXX */
	if (streq(msg, "You tunnel into the magma vein."))
	{
		/* Clear goals */
		goal = GOAL_NONE;
	
		return;
	}

	/* Word of Recall -- Ignition */
	if (prefix(msg, "The air about you becomes "))
	{
		/* Keep track of this dungeon */
		borg_dungeon_remember(FALSE);

		/* Initiate recall */
		/* Guess how long it will take to lift off */
		goal_recalling = 15000 + 5000;	/* Guess. game turns x 1000 ( 15+rand(20)) */
		return;
	}

	/* Word of Recall -- Lift off */
	if (prefix(msg, "You feel yourself yanked "))
	{
		/* Keep track of this dungeon */
		borg_dungeon_remember(FALSE);

		/* Recall complete */
		goal_recalling = 0;
		return;
	}

	/* Word of Recall -- Cancelled */
	if (prefix(msg, "A tension leaves "))
	{
		/* Hack -- Oops */
		goal_recalling = 0;
		return;
	}

	/* Take the stairs up */
	if (streq(msg, "You enter a maze of up staircases."))
	{
		/* Keep track of this dungeon */
		borg_dungeon_remember(TRUE);

		return;
	}

	/* Take the stairs down */
	if (streq(msg, "You enter a maze of down staircases."))
	{
		/* Keep track of this dungeon */
		borg_dungeon_remember(FALSE);

		return;
	}

	/* Level teleport up */
	if (streq(msg, "You rise up through the ceiling."))
	{
		/* Keep track of this dungeon */
		borg_dungeon_remember(TRUE);

		return;
	}

	/* Level teleport down */
	if (streq(msg, "You sink through the floor."))
	{
		/* Keep track of this dungeon */
		borg_dungeon_remember(FALSE);

		return;
	}

	/* When the borg eats */
	if (streq(msg, "You are full!"))
	{
		/* Keep track of this */
		bp_ptr->status.full = TRUE;
		bp_ptr->status.weak = FALSE;
		bp_ptr->status.hungry = FALSE;
		bp_ptr->status.gorged = FALSE;
		return;
	}

	/* When the borg doesn't eat */
	if (streq(msg, "You are no longer full."))
	{
		/* Keep track of this */
		bp_ptr->status.full = FALSE;
		bp_ptr->status.weak = FALSE;
		bp_ptr->status.hungry = FALSE;
		bp_ptr->status.gorged = FALSE;
		return;
	}

	/* When the borg doesn't eat */
	if (streq(msg, "You are getting hungry."))
	{
		/* Keep track of this */
		bp_ptr->status.full = FALSE;
		bp_ptr->status.weak = FALSE;
		bp_ptr->status.hungry = TRUE;
		bp_ptr->status.gorged = FALSE;
		return;
	}

	/* When the borg doesn't eat */
	if (streq(msg, "You are getting weak from hunger!") ||
		streq(msg, "You are getting faint from hunger!") ||
		streq(msg, "You faint from the lack of food."))
	{
		/* Keep track of this */
		bp_ptr->status.full = FALSE;
		bp_ptr->status.weak = TRUE;
		bp_ptr->status.hungry = FALSE;
		bp_ptr->status.gorged = FALSE;
		return;
	}

	/* When the borg eats too much */
	if (streq(msg, "You have gorged yourself!"))
	{
		/* Keep track of this */
		bp_ptr->status.full = FALSE;
		bp_ptr->status.weak = FALSE;
		bp_ptr->status.hungry = FALSE;
		bp_ptr->status.gorged = TRUE;
		return;
	}

	/* Wearing Cursed Item */
	if ((prefix(msg, "There is a malignant black aura surrounding you...")) ||
		(prefix(msg, "Oops! It feels deathly cold!")) ||
		(suffix(msg, " appears to be cursed.")) ||
		(suffix(msg, " seems to be cursed.")))
	{
		/* Hack -- Oops */
		bp_ptr->status.cursed = TRUE;
		return;
	}

	/* protect from evil */
	if (prefix(msg, "You feel safe from evil!"))
	{
		borg_prot_from_evil = TRUE;
		return;
	}
	if (prefix(msg, "You no longer feel safe from evil."))
	{
		borg_prot_from_evil = FALSE;
		return;
	}
	/* haste self */
	if (prefix(msg, "You feel yourself moving faster!"))
	{
		borg_speed = TRUE;
		return;
	}
	if (prefix(msg, "You feel yourself slow down."))
	{
		borg_speed = FALSE;
		return;
	}
	/* Bless */
	if (prefix(msg, "You feel righteous!"))
	{
		borg_bless = TRUE;
		return;
	}
	if (prefix(msg, "The prayer has expired."))
	{
		borg_bless = FALSE;
		return;
	}

	/* hero */
	if (prefix(msg, "You feel like a hero!"))
	{
		borg_hero = TRUE;
		return;
	}
	if (prefix(msg, "The heroism wears off."))
	{
		borg_hero = FALSE;
		return;
	}
	/* berserk */
	if (prefix(msg, "You feel like a killing machine!"))
	{
		borg_berserk = TRUE;
		return;
	}
	if (prefix(msg, "You feel less Berserk."))
	{
		borg_berserk = FALSE;
		return;
	}
	
	/* stone skin */
	if (prefix(msg, "Your skin turns to stone"))
	{
		borg_shield = TRUE;
		return;
	}
	if (prefix(msg, "Your skin returns to normal."))
	{
		borg_shield = FALSE;
		return;
	}

	/* check for wall blocking but not when confused */
	if ((prefix(msg, "There is a wall ") && !bp_ptr->status.confused))
	{
		my_need_alter = TRUE;
		goal = GOAL_NONE;
		return;
	}

	/* check for jungle blocking but not when confused */
	if ((prefix(msg, "The jungle is impassable.") && !bp_ptr->status.confused))
	{
		my_need_alter = TRUE;

		/* pick a new flow */
		borg_flow_goal_wild();

		return;
	}

	/* check for closed door but not when confused */
	if ((prefix(msg, "There is a closed door blocking your way.") &&
		 !bp_ptr->status.confused))
	{
		my_need_alter = TRUE;
		goal = GOAL_NONE;
		return;
	}

	/* If the borg opens an open door */
	if (prefix(msg, "You see nothing there to open") &&
		!bp_ptr->status.confused)
	{
		borg_open_door_failed = TRUE;
		return;
	}

	/* If the borg closes an closed door */
	if (prefix(msg, "You see nothing there to close") &&
		!bp_ptr->status.confused)
	{
		borg_close_door_failed = TRUE;
		return;
	}

	/* check for mis-alter command.  Sometime induced by never_move guys */
	if (streq(msg, "You spin around.") && !bp_ptr->status.confused)
	{

		/* Examine all the monsters */
		for (i = 1; i < borg_kills_nxt; i++)
		{
			borg_kill *kill = &borg_kills[i];

			/* Skip dead monsters */
			if (!kill->r_idx) continue;

			/* Distance components */
			if ((c_y == kill->y && c_x == kill->x) ||
				(g_y == kill->y && g_x == kill->x))
			{
				/* Hack -- kill em */
				borg_delete_kill(i, "oops, spinning");
			}
		}

		my_no_alter = TRUE;
		goal = GOAL_NONE;
		return;
	}

	/* Feature XXX XXX XXX */
	if (prefix(msg, "You see nothing there "))
	{
		my_no_alter = TRUE;
		/* Clear goals */
		goal = GOAL_NONE;
		return;

	}
	/* Tunneling not understood correctly */
	if (prefix(msg, "You cannot tunnel through air."))
	{
		my_no_alter = TRUE;

		/* Clear goals */
		goal = GOAL_NONE;
		return;

	}

	/* Hack to protect against clock overflows and errors */
	if (prefix(msg, "Illegal "))
	{
		borg_oops("# Borg problem msg: %s", msg);

		/* Hack -- Oops */
		borg_keypress(ESCAPE);
		borg_keypress(ESCAPE);
		return;
	}

	/* resist acid */
	if (prefix(msg, "You feel resistant to acid!"))
	{
		my_oppose_acid = TRUE;
		return;
	}
	if (prefix(msg, "You feel less resistant to acid."))
	{
		my_oppose_acid = FALSE;
		return;
	}

	/* resist electricity */
	if (prefix(msg, "You feel resistant to electricity!"))
	{
		my_oppose_elec = TRUE;
		return;
	}
	if (prefix(msg, "You feel less resistant to electricity."))
	{
		my_oppose_elec = FALSE;
		return;
	}

	/* resist fire */
	if (prefix(msg, "You feel resistant to fire!"))
	{
		my_oppose_fire = TRUE;
		return;
	}
	if (prefix(msg, "You feel less resistant to fire."))
	{
		my_oppose_fire = FALSE;
		return;
	}

	/* resist cold */
	if (prefix(msg, "You feel resistant to cold!"))
	{
		my_oppose_cold = TRUE;
		return;
	}
	if (prefix(msg, "You feel less resistant to cold."))
	{
		my_oppose_cold = FALSE;
		return;
	}

	/* resist poison */
	if (prefix(msg, "You feel resistant to poison!"))
	{
		my_oppose_pois = TRUE;
		return;
	}
	if (prefix(msg, "You feel less resistant to poison."))
	{
		my_oppose_pois = FALSE;
		return;
	}

	/* GOI! */
	if (prefix(msg, "You feel invulnerable!"))
	{
		borg_goi = 12000;		/* keep track of how long it has left (a guess) */
		return;
	}
	if (prefix(msg, "You feel vulnerable once more."))
	{
		borg_goi = 0;
		return;
	}

	/* Wraith_form! */
	if (prefix(msg, "You leave the physical world and turn into a wraith-being!"))
	{
		borg_wraith_form = TRUE;
		return;
	}
	if (prefix(msg, "You feel opaque."))
	{
		borg_wraith_form = FALSE;
		return;
	}
	
	/* Telepathy */
	if (prefix(msg, "You feel your consciousness expand!"))
	{
		borg_esp = TRUE;
		return;
	}
	if (prefix(msg, "Your consciousness contracts again."))
	{
		borg_esp = FALSE;
		return;
	}

	/* Invisible */

	/* Shield */
	if (prefix(msg, "A mystic shield forms around your body!"))
	{
		borg_shield = TRUE;
		return;
	}
	if (prefix(msg, "Your mystic shield crumbles away."))
	{
		borg_shield = FALSE;
		return;
	}

	/* Glyph of Warding (the spell no longer gives a report) */
	/* Sadly  Rune of Protection has no message */
	if (prefix(msg, "You inscribe a mystic symbol on the ground!"))
	{
		/* Check for an existing glyph */
		for (i = 0; i < track_glyph_num; i++)
		{
			/* Stop if we already new about this glyph */
			if ((track_glyph_x[i] == c_x) && (track_glyph_y[i] == c_y)) break;
		}

		/* Track the newly discovered glyph */
		if ((i == track_glyph_num) && (track_glyph_size))
		{
			borg_note("# Noting the creation of a glyph.");
			track_glyph_x[i] = c_x;
			track_glyph_y[i] = c_y;
			track_glyph_num++;
		}
		return;
	}
	if (prefix(msg, "The rune of protection is broken!"))
	{
		/* we won't know which is broken so erase them all and
		 * allow the borg to scan the screen and rebuild the array.
		 * He won't see the one under him though.  So a special check
		 * must be made.
		 */
		/* byte feat = mb_ptr->feat; */

		/* Remove the entire array */
		for (i = 0; i < track_glyph_num; i++)
		{
			/* Stop if we already new about this glyph */
			track_glyph_x[i] = 0;
			track_glyph_y[i] = 0;
			track_glyph_num = 0;
		}

		/* no known glyphs */
		track_glyph_num = 0;
	}
	/* failed glyph spell message */
	if (prefix(msg, "The object resists the spell"))
	{

		/* Forget the newly created-though-failed  glyph */
		track_glyph_x[track_glyph_num] = 0;
		track_glyph_y[track_glyph_num] = 0;
		track_glyph_num--;

		return;
	}

	/* Removed rubble. */
	if (prefix(msg, "You have removed the "))
	{
		return;
	}

	/* need to kill monsters when WoD is used */
	if (prefix(msg, "There is a searing blast of light!"))
	{
		/* Examine all the monsters */
		for (i = 1; i < borg_kills_nxt; i++)
		{
			borg_kill *kill = &borg_kills[i];

			int x9 = kill->x;
			int y9 = kill->y;
			int ax, ay, d;

			/* Skip dead monsters */
			if (!kill->r_idx) continue;

			/* Distance components */
			ax = (x9 > c_x) ? (x9 - c_x) : (c_x - x9);
			ay = (y9 > c_y) ? (y9 - c_y) : (c_y - y9);

			/* Distance */
			d = MAX(ax, ay);

			/* Minimal distance */
			if (d > 12) continue;

			/* Hack -- kill em */
			borg_delete_kill(i, "*destruction*");
		}

		return;
	}

	/* Recognize Drowning */
	if (prefix(msg, "You are drowning"))
	{
		/* Clear goals */
		goal = GOAL_NONE;
		borg_note("# Help! I can't swim");

		return;
	}

	/* Recognize Burning */
	if (prefix(msg, "The lava burns you!") ||
		prefix(msg, "The heat burns you!"))
	{
		/* Clear goals */
		goal = GOAL_NONE;
		borg_note("# Help! I'm burning");

		return;
	}
	
	/* Recognize Acid */
	if (prefix(msg, "The acid burns you!") ||
		prefix(msg, "The fumes burn you!"))
	{
		/* Clear goals */
		goal = GOAL_NONE;
		borg_note("# Help! I'm corroding");

		return;
	}

	/* Recognize Poisoning */
	if (prefix(msg, "The plants poison you!") ||
		prefix(msg, "The fumes poison you!"))
	{
		borg_note("# Help! I'm poisoned");

		/* Clear goals */
		goal = GOAL_NONE;
		return;
	}

	/* Feelings about the level */
	for (i = 0; prefix_feeling[i]; i++)
	{
		/* "You feel..." (etc) */
		if (prefix(msg, prefix_feeling[i]))
		{
			strnfmt(buf, 256, "FEELING:%d", i);
			borg_react(msg, buf);
			return;
		}
	}
}



/*
 * Parse a message, piece of a message, or set of messages.
 *
 * We must handle long messages which are "split" into multiple
 * pieces, and also multiple messages which may be "combined"
 * into a single set of messages.
 */
static void borg_parse(cptr msg)
{
	static char len = 0;
	static char buf[1024];


	/* Flush messages */
	if (len && (!msg || (msg[0] != ' ')))
	{
		int i, j;

		/* Split out punctuation */
		for (j = i = 0; i < len - 1; i++)
		{
			/* Check for punctuation */
			if ((buf[i] == '.') ||
				(buf[i] == '!') || (buf[i] == '?') || (buf[i] == '"'))
			{
				/* Require space */
				if (buf[i + 1] == ' ')
				{
					/* Terminate */
					buf[i + 1] = '\0';

					/* Parse fragment */
					borg_parse_aux(buf + j, (i + 1) - j);

					/* Restore */
					buf[i + 1] = ' ';

					/* Advance past spaces */
					for (j = i + 2; buf[j] == ' '; j++) /* loop */ ;
				}
			}
		}

		/* Parse tail */
		borg_parse_aux(buf + j, len - j);

		/* Forget */
		len = 0;
	}


	/* No message */
	if (!msg)
	{
		/* Start over */
		len = 0;
	}

	/* Continued message */
	else if (msg[0] == ' ')
	{
		/* Collect, verify, and grow */
		len += strnfmt(buf + len, 1024 - len, "%s", msg + 1);
	}

	/* New message */
	else
	{
		/* Collect, verify, and grow */
		len = strnfmt(buf, 1024, "%s", msg);
	}
}


/*
 * Initialize zborg.txt
 */

static void borg_log_death(void)
{
	char buf[1024];
	FILE *borg_log_file;
	time_t death_time;

	path_make(buf, ANGBAND_DIR_USER, "borg-log.txt");

	/* Append to the file */
	borg_log_file = my_fopen(buf, "a");

	/* Failure */
	if (!borg_log_file) return;

	/* Get time of death */
	(void)time(&death_time);

	/* Save the date */
	strftime(buf, 80, "%Y/%m/%d %H:%M\n", localtime(&death_time));

	froff(borg_log_file, buf);

	froff(borg_log_file, "%s the %s %s, Level %d/%d\n", player_name,
			race_info[p_ptr->rp.prace].title,
			class_info[p_ptr->rp.pclass].title, p_ptr->lev, p_ptr->max_lev);

	froff(borg_log_file, "Exp: %lu  Gold: %lu  Turn: %lu\n",
			(long) /* total_points() */ 0, (long)p_ptr->au, (long)turn);
	froff(borg_log_file, "Killed on level: %d (max. %d) by %s\n",
			p_ptr->depth, max_dun_level_reached(), p_ptr->state.died_from);

	froff(borg_log_file, "----------\n\n");

	my_fclose(borg_log_file);
}


static void borg_log_death_data(void)
{
	char buf[1024];
	FILE *borg_log_file;
	time_t death_time;

	path_make(buf, ANGBAND_DIR_USER, "zborg.dat");

	/* Append to the file */
	borg_log_file = my_fopen(buf, "a");

	/* Failure */
	if (!borg_log_file) return;

	/* Get time of death */
	(void)time(&death_time);

	/* dump stuff for easy import to database */
	froff(borg_log_file, "%s, %s, %d, %d, %s\n",
			race_info[p_ptr->rp.prace].title, class_info[p_ptr->rp.pclass].title,
			p_ptr->lev, p_ptr->depth, p_ptr->state.died_from);

	my_fclose(borg_log_file);
}


/*
 * The borg cannot handle all the values of all the options.  This is the list
 * of options that have to be set a certain way.  When the borg is started
 * they get the right value and when the borg stops running the original value
 * is copied back
 */
static bool borg_rogue_like_commands;
static bool borg_carry_query_flag;
static bool borg_use_old_target;
static bool borg_always_pickup;
static bool borg_easy_open;
static bool borg_easy_disarm;
static bool borg_easy_floor;
static bool borg_auto_more;
static bool borg_emergency_stop;
static bool borg_disturb_other;
static bool borg_confirm_wear;
static bool borg_check_transaction;


/* Turn on the right options */
static void borg_set_options(void)
{
	/* We use the original keypress codes */
	borg_rogue_like_commands = rogue_like_commands;
	rogue_like_commands = FALSE;

	/* We must pick items up without verification */
	borg_carry_query_flag = carry_query_flag;
	carry_query_flag = FALSE;

	/* We specify targets before casting the spells */
	borg_use_old_target = use_old_target;
	use_old_target = TRUE;

	/* We pick up items when we step on them */
	borg_always_pickup = always_pickup;
	always_pickup = TRUE;

	/* The borg adds the direction so these should be off */
	borg_easy_open = easy_open;
	borg_easy_disarm = easy_disarm;
	easy_open = FALSE;
	easy_disarm = FALSE;

	/* The borg doesn't understand the floor list */
	borg_easy_floor = easy_floor;
	easy_floor = FALSE;

	/* Prevent the teleport [y/n] question */
	borg_disturb_other = disturb_other;
	disturb_other = FALSE;

	/* The borg can get off-sequence with this */
	borg_auto_more = auto_more;
	borg_emergency_stop = emergency_stop;
	auto_more = FALSE;
	emergency_stop = FALSE;

	/* Keep track of these options values */
	borg_confirm_wear = confirm_wear;
	confirm_wear = FALSE;
	borg_check_transaction = check_transaction;
	check_transaction = FALSE;

	/* set the continous play mode if the game cheat death is on */
	if (cheat_live) borg_cheat_death = TRUE;
}


/* Set the options back to what they were */
static void borg_reset_options(void)
{
	/* We use the original keypress codes */
	rogue_like_commands = borg_rogue_like_commands;

	/* We must pick items up without verification */
	carry_query_flag = borg_carry_query_flag;

	/* We specify targets before casting the spells */
	use_old_target = borg_use_old_target;

	/* We pick up items when we step on them */
	always_pickup = borg_always_pickup;

	/* The borg adds the direction so these should be off */
	easy_open = borg_easy_open;
	easy_disarm = borg_easy_disarm;

	/* The borg doesn't understand the floor list */
	easy_floor = borg_easy_floor;

	/* The borg can get off-sequence with this */
	auto_more = borg_auto_more;
	emergency_stop = borg_emergency_stop;

	/* Prevent some [y/n] questions */
	disturb_other = borg_disturb_other;
	confirm_wear = borg_confirm_wear;
	check_transaction = borg_check_transaction;
}


/*
 * Check the overhead map for info about towns, dungeons and quests.
 * One big hack.  Maybe maid-grf.c should provide some sort of borg_wild_map,
 * like it provides the inventory and the equipment.
 */
static void borg_read_map(void)
{
	int i, j, x, y;
	int min_depth, max_depth;
	place_type *pl_ptr;
	wild_done_type *w_ptr;

	for (i = 0; i < max_wild - 1; i++)
	{
		for (j = 0; j < max_wild - 1; j++)
		{
			w_ptr = &wild[j][i].done;

			/* Do we have a place here? */
			pl_ptr = (w_ptr->place ? &place[w_ptr->place] : NULL);

			/* Skip non-places */
			if (!pl_ptr) continue;

			/* Skip unknown spots */
			if (!(w_ptr->info & WILD_INFO_SEEN)) continue;

			/* Create coords */
			x = i * WILD_BLOCK_SIZE;
			y = j * WILD_BLOCK_SIZE;

			/* If this is a town add it to the list */
			if (pl_ptr->numstores) borg_add_town(x , y, pl_ptr->name);

			/* Is this a dungeon */
			if (pl_ptr->dungeon)
			{
				dun_type *d_ptr = pl_ptr->dungeon;
				bool bottom = FALSE;

				if (d_ptr->recall_depth != 0)
				{
					min_depth = d_ptr->min_level;
					max_depth = d_ptr->recall_depth;
					if (d_ptr->max_level == d_ptr->recall_depth) bottom = TRUE;
				}
				else
				{
					/* A town dungeon always starts at level 1 */
					if (pl_ptr->numstores) min_depth = 1;

					/* In the wilderness the starting depth can vary */
					else
					{
						/* Determine dungeon level */
						min_depth = (d_ptr->min_level + 9) / 10;
						
						/* You never know */
						if (min_depth > 9) min_depth = 9;

						/* Create a possible min level for this dungeon */
						min_depth = min_depth * 10 - 1;
					}

					max_depth = min_depth;
				}

				/* Add dungeon */
				borg_add_dungeon(x, y, min_depth, max_depth, bottom);
			}
		}
	}
}

/*
 * Mega-Hack -- special "inkey_hack" hook.  XXX XXX XXX
 *
 * A special function hook (see "util.c") which allows the Borg to take
 * control of the "inkey()" function, and substitute in fake keypresses.
 */
extern char (*inkey_hack) (int flush_first);

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

	bool borg_prompt;	/* ajg  For now we can just use this locally.
						   in the 283 borg he uses this to optimize knowing if
						   we are waiting at a prompt for info */

	bool borg_rand_quick;	/* Save system setting */
	u32b borg_rand_value;	/* Save system setting */

	/* Refresh the screen */
	Term_fresh();

	/* Deactivate */
	if (!borg_active)
	{
		/* Message */
		borg_note("# Removing keypress hook");

		/* Set the options back to what they were */
		borg_reset_options();

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

	/* Mega-Hack -- Handle death */
	if (p_ptr->state.is_dead)
	{
		/* Flush messages */
		borg_parse(NULL);
		
		/* Log death, if it is wanted */
		if (borg_flag_dump)
		{
			borg_log_death();
			borg_log_death_data();
		}

		/* flush the buffer */
		borg_flush();
		
		if (borg_cheat_death)
		{
			/* Ignore death, and just print a message */
			borg_note("Player died, continuing with borg_cheat_death");
			
			/* If the borg was recalling before he died he isn't now */
			goal_recalling = 0;
			
			/* Cheat death */
			return ('n');
		}
		else
		{
			/* Oops  */
			borg_oops("player died");
		}

		/* Useless keypress */
		return (KTRL('C'));
	}

	/* Set the borg statuses and values correct */
	borg_update_frame();

	/* Assume no prompt/message is available */
	borg_prompt = FALSE;

	/*
	 * Mega-Hack -- check for possible prompts/messages
	 * If the first four characters on the message line all
	 * have the same attribute (or are all spaces), and they
	 * are not all spaces (ascii value 0x20)...
	 */
	if ((0 == borg_what_text(0, 0, 4, &t_a, buf)) &&
		(t_a != TERM_DARK) && (*((u32b *)(buf)) != 0x20202020))
	{
		/* Assume a prompt/message is available */
		borg_prompt = TRUE;
	}

	/* Locate the cursor */
	(void)Term_locate(&x, &y);

	/* 
	 * Mega-Hack -- Catch "-more-" messages
	 * If there is text on the first line...
	 * And the game does not want a command...
	 * And the cursor is on the top line...
	 * And there is text before the cursor...
	 * And that text is "-more-"
	 */
	if (borg_prompt && !p_ptr->cmd.inkey_flag &&
		y == 0 && x >= 7 &&
		borg_term_text_comp(x - 7, y, " -more-"))
	{
		/* Get the message */
		if (0 == borg_what_text(0, 0, x - 7, &t_a, buf))
		{
			/* Parse it */
			borg_parse(buf);
		}
		/* Clear the message */
		return (KTRL('M'));
	}

	/*
	 * Mega-Hack -- catch normal messages
	 * If there is text on the first line...
	 * And the game wants a command
	 */
	if (borg_prompt && p_ptr->cmd.inkey_flag)
	{
		/* Get the message(s) */
		if (0 == borg_what_text(0, 0, -80, &t_a, buf))
		{
			int k = strlen(buf);

			/* Strip trailing spaces */
			while ((k > 0) && (buf[k - 1] == ' ')) k--;

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
	borg_dont_react = FALSE;

	/* Check for key */
	ch = borg_inkey(TRUE);

	/* Use the key */
	if (ch) return (ch);

	/* Hack - check to see if we are doing a repeated action */
	if (p_ptr->state.running || p_ptr->cmd.rep || p_ptr->state.resting)
	{
		return (0);
	}


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
	while (!borg_think()) /* loop */ ;

	/* DVE- Update the status screen */
	borg_status_window();

	/* Save the local random info */
	borg_rand_local = Rand_value;

	/* Restore the system random info */
	Rand_quick = borg_rand_quick;
	Rand_value = borg_rand_value;

	/* Hack -- allow stepping to induce a clean cancel */
	if (borg_step && (!--borg_step)) borg_cancel = TRUE;


	/* Check for key */
	ch = borg_inkey(TRUE);

	/* Use the key */
	if (ch) return (ch);

	/* Oops */
	borg_oops("normal abort - out of keys???");

	/* Hack -- Escape */
	return (ESCAPE);
}

/* This will display the values which the borg believes an item has. */
static void borg_display_item(list_item *l_ptr)
{
	int j = 13;

	/* Clear the screen */
    clear_region(13 - 2, 1, 30);

	/* Describe fully */
	if (l_ptr->o_name) prtf(j, 2, "%s", l_ptr->o_name);

	prtf(j, 4, "k_idx = %-5d    tval = %-5d ",
			   l_ptr->k_idx, l_ptr->tval);

	prtf(j, 5, "number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
			   l_ptr->number, l_ptr->weight, l_ptr->ac, l_ptr->dd, l_ptr->ds);

	prtf(j, 6, "pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
			   l_ptr->pval, l_ptr->to_a, l_ptr->to_h, l_ptr->to_d);

	if (l_ptr->xtra_name)
	{
		prtf(j, 7, "xtra_name = %s", l_ptr->xtra_name);
	}

	prtf(j, 8, "info = %d  timeout = %-d", l_ptr->info, l_ptr->timeout);


	prtf(j, 10, "+------------FLAGS1------------+\n"
				"AFFECT........SLAY........BRAND.\n"
				"              cvae      xsqpaefc\n"
				"siwdcc  ssidsahanvudotgddhuoclio\n"
				"tnieoh  trnipttmiinmrrnrrraiierl\n"
				"rtsxna..lcfgdkcpmldncltggpksdced\n"
                "%v", binary_fmt, l_ptr->kn_flags[0]);

	prtf(j, 17, "+------------FLAGS2------------+\n"
				"SUST...IMMUN..RESIST............\n"
			    "        aefctrpsaefcpfldbc sn   \n"
	    		"siwdcc  clioheatcliooeialoshtncd\n"
			    "tnieoh  ierlrfraierliatrnnnrhehi\n"
			    "rtsxna..dcedwlatdcedsrekdfddrxss\n"
                "%v", binary_fmt, l_ptr->kn_flags[1]);

	prtf(j + 32, 10, "+------------FLAGS3------------+\n"
				     "SH  NO tehsif itdrmsIGNRadtabchp\n"
				     "fe  tm yzdhnelneieihaefccrpgluvm\n"
				     "il  ea cktmativlgggocliotnorercc\n"
				     "re  lg rnyorhtiesehtierlvxrvssrr\n"
	    			 "ec  ec swpdtresptntsdcedtpttsess\n"
                     "%v", binary_fmt, l_ptr->kn_flags[2]);

	prtf(j + 32, 17, "+------------FLAGS4------------+\n"
				     "        IMSH p pt reHURT..  CURS\n"
				     "        ldac alao txaefcld  as h\n"
				     "        iacomtusupupclioia  utee\n"
				     "        trilurcscsrlierltr  taaa\n"
	    		 	 "        ekddtnkwhinodcedek  ottl\n"
                     "%v", binary_fmt, l_ptr->kn_flags[3]);
}

/* Get all sorts of temporary values from the game. */
static void borg_cheat_temp_bools(void)
{
	/* Allowable Cheat -- Obtain "recall" flag */
	goal_recalling = p_ptr->tim.word_recall * 1000;
	
	/* Allowable Cheat -- Obtain "prot_from_evil" flag */
	borg_prot_from_evil = (p_ptr->tim.protevil ? TRUE : FALSE);
	
	/* Allowable Cheat -- Obtain "speed" flag */
	borg_speed = (p_ptr->tim.fast ? TRUE : FALSE);
	
	/* Allowable Cheat -- Obtain "goi" flag */
	borg_goi = (p_ptr->tim.invuln ? 9000 : 0);
	
	/* Allowable Cheat -- Obtain "wraithform" flag */
	borg_wraith_form = (p_ptr->tim.wraith_form ? 9000 : 0);
	
	/* Allowable Cheat -- Obtain "invisibility" flag */
	borg_inviso = (p_ptr->tim.invis ? 9000 : 0);
	
	/* Allowable Cheat -- Obtain "resist" flags */
	my_oppose_acid = (p_ptr->tim.oppose_acid ? TRUE : FALSE);
	my_oppose_elec = (p_ptr->tim.oppose_elec ? TRUE : FALSE);
	my_oppose_fire = (p_ptr->tim.oppose_fire ? TRUE : FALSE);
	my_oppose_cold = (p_ptr->tim.oppose_cold ? TRUE : FALSE);
	my_oppose_pois = (p_ptr->tim.oppose_pois ? TRUE : FALSE);
	
	borg_bless = (p_ptr->tim.blessed ? TRUE : FALSE);
	borg_shield = (p_ptr->tim.shield ? TRUE : FALSE);
	borg_hero = (p_ptr->tim.hero ? TRUE : FALSE);
	borg_berserk = (p_ptr->tim.shero ? TRUE : FALSE);
	borg_esp = (p_ptr->tim.esp ? TRUE : FALSE);
}


/* Initialize the Borg */
void borg_init_9(void)
{
	/*** Hack -- initialize borg.ini options ***/

	/* Message */
	prtf(0, 0, "Initializing the Borg.");

	/* Hack -- flush it */
	Term_fresh();

	/* Initialise player position */
	map_get_player(&c_x, &c_y);

	/* Initialize */
	borg_init_1();
	borg_init_2();
	borg_init_3();
	borg_init_4();
	borg_init_5();
	borg_init_6();
	borg_init_7();
	borg_init_8();

	/* Hack - initialise the hooks into the overhead map code */

	/* Save the borg hooks into the overhead map */
	set_callback((callback_type) borg_map_info, CALL_MAP_INFO, NULL);
	set_callback((callback_type) borg_map_erase, CALL_MAP_ERASE, NULL);

	/* Save old player movement hook */
	set_callback((callback_type) borg_player_move, CALL_PLAYER_MOVE, NULL);

	/* Save the borg hooks for object lists */
	set_callback((callback_type) borg_list_info, CALL_OBJECT_LIST, NULL);

	/*** Redraw ***/

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Redraw everything */
	do_cmd_redraw();


	/*** Hack -- Extract race ***/

	/* Insert the player Race--cheat */
	borg_race = p_ptr->rp.prace;

	/* Extract the race pointer */
	rb_ptr = &race_info[borg_race];


	/*** Hack -- Extract class ***/
	borg_class = p_ptr->rp.pclass;

	/* Extract the class pointer */
	cb_ptr = &class_info[borg_class];

	/* Extract the magic pointer */
	pmb_ptr = &magic_info[borg_class];


	/*** Hack -- react to race and class ***/

	/* Notice the new race and class */
	prepare_race_class_info();


	/*** All done ***/

	/* Clear line */
	clear_msg();

	/* Reset the clock */
	borg_t = 1000;

	/* Official message */
	borg_note("# Ready...");

	/* Now it is ready */
	initialized = TRUE;
}


static void borg_display_map_info(byte data, byte type)
{
	int x, y;

	char c = ' ';
	byte a = TERM_DARK;

	map_block *mb_ptr;

	for (x = p_ptr->panel_x1; x < p_ptr->panel_x2; x++)
	{
		for (y = p_ptr->panel_y1; y < p_ptr->panel_y2; y++)
		{
			a = TERM_DARK;
			c = ' ';

			if (map_in_bounds(x, y))
			{
				mb_ptr = map_loc(x, y);

				switch (type)
				{
					case BORG_SHOW_FEAT:
					{
						if (mb_ptr->feat == data)
						{
							a = TERM_WHITE;
							c = '*';
						}

						break;
					}

					case BORG_SHOW_INFO:
					{
						if (mb_ptr->info & data)
						{
							a = TERM_WHITE;
							c = '*';
						}

						break;
					}

					case BORG_SHOW_FLAG:
					{
						if (mb_ptr->flags & data)
						{
							a = TERM_WHITE;
							c = '*';
						}

						break;
					}

					case BORG_SHOW_FLOW:
					{
						if (mb_ptr->flow == data)
						{
							a = TERM_WHITE;
							c = '*';
						}

						break;
					}

					case BORG_SHOW_AVOID:
					{
						/* Obtain danger */
						int p = borg_danger(x, y, 1, TRUE);

						/* Skip non-avoidances */
						if (p <= avoidance / 3) break;

						if (p <= avoidance)
						{
							a = TERM_YELLOW;
						}
						else
						{
							a = TERM_RED;
						}

						break;
					}

					case BORG_SHOW_FEAR:
					{
						/* Obtain fear */
						int p = mb_ptr->fear;

						/* Skip non-avoidances */
						if (p <= avoidance / 10) break;

						if (p <= avoidance / 4)
						{
							a = TERM_YELLOW;
						}
						else
						{
							a = TERM_RED;
						}

						break;
					}

					case BORG_SHOW_STEP:
					{
						int i;

						/* Check for an existing step */
						for (i = 0; i < track_step_num; i++)
						{
							/* Stop if we already new about this glyph */
							if ((track_step_x[i] == x) &&
								(track_step_y[i] == y))
							{
								a = TERM_WHITE;
								c = '*';

								break;
							}
						}
						break;
					}
				}
			}


			if (c != ' ')
			{
				/* Hack -- Queue it */
				print_rel(c, a, x, y);
			}
		}
	}
}


/* DVE's function for displaying the status of various info */
/* Display what the borg is thinking DvE*/
void borg_status_window(void)
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* Unused */
		if (!angband_term[j]) continue;

		/* Check for borg status term */
		if (window_flag[j] & (PW_BORG_2))
		{
			cptr attr;

			/* Activate */
			Term_activate(angband_term[j]);

			/* Display what resists the borg (thinks he) has */
			prtf(5, 0, "RESISTS");

			/* Basic four */
			attr = CLR_SLATE;
			if (FLAG(bp_ptr, TR_RES_ACID)) attr = CLR_BLUE;
			if (my_oppose_acid) attr = CLR_GREEN;
			if (FLAG(bp_ptr, TR_IM_ACID)) attr = CLR_WHITE;
			prtf(1, 1, "%sAcid", attr);

			attr = CLR_SLATE;
			if (FLAG(bp_ptr, TR_RES_ELEC)) attr = CLR_BLUE;
			if (my_oppose_elec) attr = CLR_GREEN;
			if (FLAG(bp_ptr, TR_IM_ELEC)) attr = CLR_WHITE;
			prtf(1, 2, "%sElec", attr);

			attr = CLR_SLATE;
			if (FLAG(bp_ptr, TR_RES_FIRE)) attr = CLR_BLUE;
			if (my_oppose_fire) attr = CLR_GREEN;
			if (FLAG(bp_ptr, TR_IM_FIRE)) attr = CLR_WHITE;
			prtf(1, 3, "%sFire%s", attr);

			attr = CLR_SLATE;
			if (FLAG(bp_ptr, TR_RES_COLD)) attr = CLR_BLUE;
			if (my_oppose_cold) attr = CLR_GREEN;
			if (FLAG(bp_ptr, TR_IM_COLD)) attr = CLR_WHITE;
			prtf(1, 4, "%sCold", attr);

			/* High resists */
			attr = CLR_SLATE;
			if (FLAG(bp_ptr, TR_RES_POIS)) attr = CLR_BLUE;
			if (my_oppose_pois) attr = CLR_GREEN;
			if (FLAG(bp_ptr, TR_IM_POIS)) attr = CLR_WHITE;
			prtf(1, 5, "%sPois", attr);

			if (FLAG(bp_ptr, TR_RES_FEAR)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(1, 6, "%sFear", attr);

			if (FLAG(bp_ptr, TR_RES_LITE)) attr = CLR_BLUE;
			else if (FLAG(bp_ptr, TR_IM_LITE)) attr = CLR_WHITE;
			else attr = CLR_SLATE;
			prtf(1, 7, "%sLite", attr);

			if (FLAG(bp_ptr, TR_RES_DARK)) attr = CLR_BLUE;
			else if (FLAG(bp_ptr, TR_IM_DARK)) attr = CLR_WHITE;
			else attr = CLR_SLATE;
			prtf(1, 8, "%sDark", attr);

			if (FLAG(bp_ptr, TR_RES_BLIND)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 1, "%sBlind", attr);

			if (FLAG(bp_ptr, TR_RES_CONF)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 2, "%sConfu", attr);

			if (FLAG(bp_ptr, TR_RES_SOUND)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 3, "%sSound", attr);

			if (FLAG(bp_ptr, TR_RES_SHARDS)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 4, "%sShard", attr);

			if (FLAG(bp_ptr, TR_RES_NEXUS)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 5, "%sNexus", attr);

			if (FLAG(bp_ptr, TR_RES_NETHER)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 6, "%sNethr", attr);

			if (FLAG(bp_ptr, TR_RES_CHAOS)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 7, "%sChaos", attr);

			if (FLAG(bp_ptr, TR_RES_DISEN)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(6, 8, "%sDisen", attr);

			/* Other abilities */
			if (FLAG(bp_ptr, TR_SLOW_DIGEST)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 1, "%sS.Dig", attr);

			if (FLAG(bp_ptr, TR_FEATHER)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 2, "%sFeath", attr);

			if (bp_ptr->britelite) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 3, "%sPLite", attr);

			if (FLAG(bp_ptr, TR_REGEN)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 4, "%sRegen", attr);

			if (FLAG(bp_ptr, TR_TELEPATHY)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 5, "%sTelep", attr);

			if (FLAG(bp_ptr, TR_SEE_INVIS)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 6, "%sInvis", attr);

			if (FLAG(bp_ptr, TR_FREE_ACT)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 7, "%sFrAct", attr);

			if (FLAG(bp_ptr, TR_HOLD_LIFE)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(12, 8, "%sHLife", attr);

			/* Display the slays */
			prtf(5, 10, "Weapon Slays:");

			if (FLAG(bp_ptr, TR_SLAY_ANIMAL)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(1, 11, "%sAnimal", attr);

			if (FLAG(bp_ptr, TR_SLAY_EVIL)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(8, 11, "%sEvil", attr);

			if (FLAG(bp_ptr, TR_SLAY_UNDEAD)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(15, 11, "%sUndead", attr);

			if (FLAG(bp_ptr, TR_SLAY_DEMON)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(22, 11, "%sDemon", attr);

			if (FLAG(bp_ptr, TR_SLAY_ORC)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(1, 12, "%sOrc", attr);

			if (FLAG(bp_ptr, TR_SLAY_TROLL)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(8, 12, "%sTroll", attr);

			if (FLAG(bp_ptr, TR_SLAY_GIANT)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(15, 12, "%sGiant", attr);

			if (FLAG(bp_ptr, TR_SLAY_DRAGON)) attr = CLR_BLUE;
			if (FLAG(bp_ptr, TR_KILL_DRAGON)) attr = CLR_GREEN;
			else
				attr = CLR_SLATE;
			prtf(22, 12, "%sDragon", attr);

			if (FLAG(bp_ptr, TR_BRAND_ACID)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(1, 13, "%sAcid", attr);

			if (FLAG(bp_ptr, TR_BRAND_COLD)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(8, 13, "%sCold", attr);

			if (FLAG(bp_ptr, TR_BRAND_ELEC)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(15, 13, "%sElec", attr);

			if (FLAG(bp_ptr, TR_BRAND_FIRE)) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(22, 13, "%sFire", attr);


			/* Display the Concerns */
			prtf(36, 10, "Concerns:");

			if (bp_ptr->status.cursed)
				attr = CLR_BLUE;
			else if (bp_ptr->status.heavy_curse)
				attr = CLR_ORANGE;
			else
				attr = CLR_SLATE;
			prtf(29, 11, "%sCursed", attr);

			if (bp_ptr->status.weak) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(36, 11, "%sWeak", attr);

			if (bp_ptr->status.poisoned) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(43, 11, "%sPoison", attr);

			if (bp_ptr->status.cut) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(29, 12, "%sCut", attr);

			if (bp_ptr->status.stun) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(36, 12, "%sStun", attr);

			if (bp_ptr->status.confused) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(43, 12, "%sConfused", attr);

			if (bp_ptr->status.fixexp) attr = CLR_BLUE;
			else
				attr = CLR_SLATE;
			prtf(43, 13, "%sExp Drain", attr);

			/* Display the Time */
			prtf(60, 10, "Time:");

			prtf(54, 11, "This Level         %d", borg_t - borg_began);

			prtf(54, 12, CLR_SLATE "Since Town         " CLR_WHITE "%d",
					 borg_time_town + (borg_t - borg_began));


			/* Sustains */
			prtf(19, 0, "Sustains");

			if (bp_ptr->sust[A_STR]) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(21, 1, "%sSTR", attr);

			if (bp_ptr->sust[A_INT]) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(21, 2, "%sINT", attr);

			if (bp_ptr->sust[A_WIS]) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(21, 3, "%sWIS", attr);

			if (bp_ptr->sust[A_DEX]) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(21, 4, "%sDEX", attr);

			if (bp_ptr->sust[A_CON]) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(21, 5, "%sCON", attr);

			if (bp_ptr->sust[A_CHR]) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(21, 6, "%sCHR", attr);


			/* Temporary effects */
			prtf(28, 0, "Temp Effects");

			if (borg_prot_from_evil) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 1, "%sProt. Evil", attr);

			if (borg_goi) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 2, "%sInvulnerable", attr);

			if (borg_hero) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 3, "%sHeroism", attr);

			if (borg_berserk) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 4, "%sBerserk", attr);

			if (borg_shield) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 5, "%sShielded", attr);

			if (borg_bless) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 6, "%sBlessed", attr);

			if (borg_speed) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 7, "%sFast", attr);

			if (borg_inviso) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(28, 8, "%sInvisible", attr);

			/* Temporary effects */
			prtf(42, 0, "Level Information");

			if (vault_on_level) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(42, 1, "%sVault on Level", attr);

			if (unique_on_level) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(42, 2, "%sUnique on level", attr);
			if (unique_on_level) prtf(58, 2, "(%s)", mon_race_name(&r_info[unique_r_idx]));
			else
				prtf(58, 2, "");

			prtf(42, 4, CLR_SLATE "Borg is ready for level " CLR_WHITE "%d",
				borg_prepared_depth());

			/* In the dungeon */
			if (bp_ptr->depth || vanilla_town)
			{
				/* level preparedness */
				prtf(42, 6, CLR_SLATE "Reason: " CLR_WHITE
							"%s", borg_prepared(borg_prepared_depth()));
			}
			else
			{
				cptr why;

				switch(goal)
				{
					case GOAL_NONE:
					{
						why = "No goal";
						break;
					}
					case GOAL_KILL:
					{
						why = "Killing a monster";
						break;
					}
					case GOAL_TAKE:
					{
						why = "Getting an object";
						break;
					}
					case GOAL_FLEE:
					{
						why = "Running away";
						break;
					}
					case GOAL_SHOP:
					{
						why = "Shopping";
						break;
					}
					case GOAL_DARK:
					{
						why = "Exploring";
						break;
					}
					case GOAL_TOWN:
					{
						why = "Reaching a town";
						break;
					}
					case GOAL_CAVE:
					{
						why = "Reaching a dungeon";
						break;
					}
					default: why = "Something or other";
				}

				/* level preparedness */
				prtf(42, 6, CLR_SLATE "What to do on the surface: " CLR_WHITE "%s.", why);
			}


			if (goal_fleeing) attr = CLR_WHITE;
			else
				attr = CLR_SLATE;
			prtf(42, 7, "%sFleeing Level", attr);

			prtf(42, 8, CLR_SLATE "Maximal Depth: %d", bp_ptr->max_depth);

			/* Fresh */
			Term_fresh();

			/* Restore */
			Term_activate(old);
		}
	}
}


/*
 * Hack -- interact with the "Ben Borg".
 */
void do_cmd_borg(void)
{
	char cmd;

	/* Get a "Borg command", or abort */
	if (!get_com("Borg command: ", &cmd)) return;

	/* Hack -- force initialization */
	if (!initialized) borg_init_9();

	switch (cmd)
	{

		case 'z':
		case 'Z':
		{
			/* Command: Activate */

			/* Activate */
			borg_active = TRUE;

			/* Reset cancel */
			borg_cancel = FALSE;

			/* Step forever */
			borg_step = 0;

			/*** Redraw ***/

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Redraw everything */
			do_cmd_redraw();

			/* need to check all stats */
			my_need_stat_check[0] = TRUE;
			my_need_stat_check[1] = TRUE;
			my_need_stat_check[2] = TRUE;
			my_need_stat_check[3] = TRUE;
			my_need_stat_check[4] = TRUE;
			my_need_stat_check[5] = TRUE;
			
			/* Fill the borg_bools for temporary states */
			borg_cheat_temp_bools();

			/* Make sure the right options are set */
			borg_set_options();

			/* Read the wilderness map */
			borg_read_map();

			/* Message */
			borg_note("# Installing keypress hook");

			/* Activate the key stealer */
			inkey_hack = borg_inkey_hack;

			break;
		}

		case 'u':
		case 'U':
		{
			/* Command: Update */

			/* Activate */
			borg_active = TRUE;

			/* Immediate cancel */
			borg_cancel = TRUE;

			/* Step forever */
			borg_step = 0;
			
			/* Fill the borg_bools for temporary states*/
			borg_cheat_temp_bools();

			/* Make sure the right options are set */
			borg_set_options();

			/* Read the wilderness map */
			borg_read_map();

			/* Message */
			borg_note("# Installing keypress hook");

			/* Activate the key stealer */
			inkey_hack = borg_inkey_hack;

			break;
		}

		case 'x':
		case 'X':
		{
			/* Command: Step */

			/* Activate */
			borg_active = TRUE;

			/* Reset cancel */
			borg_cancel = FALSE;

			/* Step N times */
			borg_step = 1;

			/*** Redraw ***/

			/* Redraw map */
			p_ptr->redraw |= (PR_MAP);

			/* Window stuff */
			p_ptr->window |= (PW_OVERHEAD);

			/* Redraw everything */
			do_cmd_redraw();

			/* need to check all stats */
			my_need_stat_check[0] = TRUE;
			my_need_stat_check[1] = TRUE;
			my_need_stat_check[2] = TRUE;
			my_need_stat_check[3] = TRUE;
			my_need_stat_check[4] = TRUE;
			my_need_stat_check[5] = TRUE;
			
			/* Fill the borg_bools for temporary states*/
			borg_cheat_temp_bools();

			/* Make sure the right options are set */
			borg_set_options();

			/* Read the wilderness map */
			borg_read_map();

			/* Message */
			borg_note("# Installing keypress hook");

			/* Activate the key stealer */
			inkey_hack = borg_inkey_hack;

			break;
		}

		case 'f':
		case 'F':
		{
			/* Command: toggle "flags" */
						
			/* Save and partly clear the screen */
			Term_save();
			clear_region(0, 1, 6);

			do
			{
				/* List the possibilities */
				prtf(1, 2,
					"a)  The borg stops when he wins:         %s    (borg_stop_king)\n"
					"b)  Allow borg to avoid death:           %s    (borg_cheat_death)\n"
					"c)  Log some characteristics at death:   %s    (borg_flag_dump)\n"
					"d)  Autosave when entering new levels:   %s    (borg_flag_save)\n"
					, (borg_stop_king) ? "true " : "false"
					, (borg_cheat_death) ? "true " : "false"
					, (borg_flag_dump) ? "true " : "false"
					, (borg_flag_save) ? "true " : "false");
			
			
				/* What does the user want? */
				get_com("Borg options  (Command (a-d), ESC)", &cmd);
			
				/* Toggle a bool */
				switch (cmd)
				{
					case 'a':
					case 'A':
					{
						borg_stop_king = !borg_stop_king;
						break;
					}
					case 'b':
					case 'B':
					{
						borg_cheat_death = !borg_cheat_death;
						break;
					}
					case 'c':
					case 'C':
					{
						borg_flag_dump = !borg_flag_dump;
						break;
					}
					case 'd':
					case 'D':
					{
						borg_flag_save = !borg_flag_save;
						break;
					}
					case ESCAPE:
					{
						/* Restore the screen */
						Term_load();
						break;
					}
 				}
 			}
			while (cmd != ESCAPE);
			
			break;
		}

		case 'l':
		case 'L':
		{
			/* Start a new log file */
			char buf[1024];

			/* Close the log file */
			if (borg_fff) my_fclose(borg_fff);

			/* Default  */
			path_make(buf, ANGBAND_DIR_USER, "borg.log");

			/* XXX XXX XXX Get the name and open the log file */
			if (get_string(buf, 70, "Borg Log File: "))
			{
				/* Open a new file */
				borg_fff = my_fopen(buf, "w");

				/* Failure */
				if (!borg_fff) msgf("Cannot open that file.");
			}
			break;
		}

		case 's':
		case 'S':
		{
			/* Activate a search string */

			/* Get the new search string (or cancel the matching) */
			if (!get_string(borg_match, 70, "Borg Match String: "))
			{
				/* Cancel it */
				borg_match[0] = 0;

				/* Message */
				msgf("Borg Match String de-activated.");
			}
			break;
		}

		case 'g':
		case 'G':
		{
			/* Command: check Grid "feature" flags */

			byte feat = 0;

			/* Get a "Borg command", or abort */
			if (!get_com("Borg command: Show grids: ", &cmd)) return;

			/* Extract a flag */
			switch (cmd)
			{
				case '.':
				{
					feat = map_loc(c_x, c_y)->feat;
					break;
				}
				case ',':
				{
					feat = FEAT_OPEN;
					break;
				}
				case '+':
				{
					feat = FEAT_CLOSED;
					break;
				}
				case 'x':
				{
					feat = FEAT_BROKEN;
					break;
				}
				case '<':
				{
					feat = FEAT_LESS;
					break;
				}
				case '>':
				{
					feat = FEAT_MORE;
					break;
				}
				case 's':
				{
					feat = FEAT_SECRET;
					break;
				}
				case ':':
				{
					feat = FEAT_RUBBLE;
					break;
				}
				case 't':
				{
					feat = FEAT_TREES;
					break;
				}
				case 'l':
				{
					feat = FEAT_SHAL_LAVA;
					break;
				}
				case 'L':
				{
					feat = FEAT_DEEP_LAVA;
					break;
				}
				case 'a':
				{
					feat = FEAT_SHAL_WATER;
					break;
				}
				case 'A':
				{
					feat = FEAT_DEEP_WATER;
					break;
				}
				case 'o':
				{
					feat = FEAT_OCEAN_WATER;
					break;
				}
				case 'b':
				{
					feat = FEAT_SHAL_ACID;
				    break;
				}
				case 'B':
				{
					feat = FEAT_DEEP_ACID;
				    break;
				}
				case 'c':
				{
					feat = FEAT_SHAL_SWAMP;
					break;
				}
				case 'C':
				{
					feat = FEAT_DEEP_SWAMP;
					break;
				}
				case 'w':
				{
					feat = FEAT_WALL_EXTRA;
					break;
				}
				case 'p':
				{
					feat = FEAT_PERM_EXTRA;
					break;
				}

				default:
				{
					/* Save the screen and clear it */
					Term_save();
					Term_clear();
					
					prtf(1, 1, "Possible entries for features:\n"
								".  Dungeon floor\n"
								",  Open door\n"
								"+  Closed door\n"
								"x  Broken door\n"
								"s  Secret door\n"
								"<  Up staircase\n"
								">  Down staircase\n"
								":  Rubble\n"
								"t  Trees\n"
								"l  Shallow Lava\n"
								"L  Deep Lava\n"
								"a  Shallow water\n"
								"A  Deep water\n"
								"o  Ocean water\n"
								"b  Shallow acid\n"
								"B  Deep acid\n"
								"c  Shallow swamp\n"
								"C  Deep swamp\n"
								"w  Wall\n"
								"p  Permanent wall\n");

					/* Get keypress */
					msgf("Press any key.");
					message_flush();

					/* Restore the screen */
					Term_load();
					
					return;
				}
			}

			/* Show it */
			borg_display_map_info(feat, BORG_SHOW_FEAT);

			/* Get keypress */
			msgf("Press any key.");
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 'i':
		case 'I':
		{
			/* Command: check "info" flags */
			byte mask;

			/* Get a "Borg command", or abort */
			if (!get_com("Borg command: Show grids: ", &cmd)) return;

			/* Extract a flag */
			switch (cmd)
			{
				case '0':
				{
					mask = 1 << 0;
					break;
				}
				case '1':
				{
					mask = 1 << 1;
					break;
				}
				case '2':
				{
					mask = 1 << 2;
					break;
				}
				case '3':
				{
					mask = 1 << 3;
					break;
				}
				case '4':
				{
					mask = 1 << 4;
					break;
				}
				case '5':
				{
					mask = 1 << 5;
					break;
				}
				case '6':
				{
					mask = 1 << 6;
					break;
				}
				case '7':
				{
					mask = 1 << 7;
					break;
				}

				default:
				{
					mask = 0x00;
					break;
				}
			}

			/* Show it */
			borg_display_map_info(mask, BORG_SHOW_FLAG);

			/* Get keypress */
			msgf("Press any key.");
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 'a':
		case 'A':
		{
			/* Command: check "avoidances" */

			/* Show it */
			borg_display_map_info(0, BORG_SHOW_AVOID);

			/* Get keypress */
			msgf("(%d,%d) Avoidance value %d.", c_x, c_y, avoidance);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 'y':
		{
			/* Command: check previous steps */

			/* Show it */
			borg_display_map_info(0, BORG_SHOW_STEP);

			/* Get keypress */
			msgf("(%d) Steps noted", track_step_num);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 'k':
		{
			/* Command: show "monsters" */
			int i, n = 0;

			/* Scan the monsters */
			for (i = 1; i < borg_kills_nxt; i++)
			{
				borg_kill *kill = &borg_kills[i];
				int x, y;

				/* Still alive */
				if (!kill->r_idx) continue;

					/* Require current knowledge */
				if (kill->when < borg_t) continue;

				x = kill->x;
				y = kill->y;

				/* Display */
				print_rel('*', TERM_RED, x, y);

				/* Count */
				n++;
			}

			/* Get keypress */
			msgf("There are %d known monsters.", n);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 'K':
		{
			/* Command: show "monsters" */
			int i;

			/* Scan the monsters */
			for (i = 0; i < borg_ball_n; i++)
			{
				int x = borg_ball_x[i];
				int y = borg_ball_y[i];

				/* Display */
				print_rel('*', TERM_RED, x, y);
			}

			/* Get keypress */
			msgf("There are %d grids.", borg_ball_n);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 't':
		case 'T':
		{
			/* Command: show "objects" */
			int i, n = 0;

			/* Scan the objects */
			for (i = 1; i < borg_takes_nxt; i++)
			{
				borg_take *take = &borg_takes[i];

				/* Still alive */
				if (take->k_idx)
				{
					int x = take->x;
					int y = take->y;

					/* Display */
					print_rel('*', TERM_RED, x, y);

					/* Count */
					n++;
				}
			}

			/* Get keypress */
			msgf("There are %d known objects.", n);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case '7':
		{
			/* Command: debug -- show towns */
			int i;

			/* Get keypress */
			msgf("There are %d known towns.", borg_town_num);
			message_flush();

			for (i = 0; i < borg_town_num; i++)
			{
				/* Print */
				msgf("i = %d, (x, y) = (%d, %d), visit = %c, name = %s",
					i, borg_towns[i].x, borg_towns[i].y,
					(borg_towns[i].visit) ? 'T' : 'F',
					borg_towns[i].name);

				message_flush();
			}

			break;
		}

		case '8':
		{
			/* Command: debug -- show shops */
			int i, n = 0;

			for (i = 0; i < borg_shop_num; i++)
			{
				char c = (i < 10) ? i + '0' : '*';

				/* Print */
				print_rel(c, TERM_RED, borg_shops[i].x, borg_shops[i].y);

				/* Count the visited shops */
				if (borg_shops[i].visit) n++;
			}

			/* Get keypress */
			msgf("There are %d known shops, %d were visited.", borg_shop_num, n);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case '9':
		{
			/* Command: debug -- show dungeons */
			int i;

			for (i = 0; i < borg_dungeon_num; i++)
			{
				/* Print */
				print_rel('*', TERM_RED, borg_dungeons[i].x, borg_dungeons[i].y);
			}

			/* Get keypress */
			msgf("There are %d known dungeons.", borg_dungeon_num);
			message_flush();

			/* Redraw map */
			prt_map();

			msgf("(c_x, c_y) = (%d, %d), dungeon_num = %d", c_x, c_y, dungeon_num);
			for (i = 0; i < borg_dungeon_num; i++)
			{
				/* Print */
				msgf("i = %d, (x, y) = (%d, %d), min = %d, max = %d, bottom = %c",
					i, borg_dungeons[i].x, borg_dungeons[i].y,
					borg_dungeons[i].min_depth, borg_dungeons[i].max_depth,
					borg_dungeons[i].bottom ? 'T' : 'F');
			}
			break;
		}

		case '%':
		{
			/* Command: debug -- current flow */
			byte i;

			/* Flow */
			for (i = 0; i < 250; i++)
			{
				/* Show it */
				borg_display_map_info(i, BORG_SHOW_FLOW);

				/* Get keypress */
				msgf("Flow depth %d.", i);
				message_flush();

				if (!get_check("Continue?")) break;

				/* Redraw map */
				prt_map();
			}
			break;
		}

		case '^':
		{
			/* Display the intended path to the flow */
			int x, y;
			int o;
			int false_y, false_x;

			false_y = c_y;
			false_x = c_x;

			/* Continue */
			for (o = 0; o < 250; o++)
			{
				int b_n = 0;

				int i, b_i = -1;

				int c, b_c;

				map_block *mb_ptr = map_loc(c_x, c_y);


				/* Flow cost of current grid */
				b_c = mb_ptr->flow * 10;

				/* Prevent loops */
				b_c = b_c - 5;

				/* Look around */
				for (i = 0; i < 8; i++)
				{
					/* Grid in that direction */
					x = false_x + ddx_ddd[i];
					y = false_y + ddy_ddd[i];

					/* Bounds checking */
					if (!map_in_bounds(x, y)) continue;

					/* Access the grid */
					mb_ptr = map_loc(x, y);

					/* Flow cost at that grid */
					c = mb_ptr->flow * 10;

					/* Never backtrack */
					if (c > b_c) continue;

					/* Notice new best value */
					if (c < b_c) b_n = 0;

					/* Apply the randomizer to equivalent values */
					if ((++b_n >= 2) && (randint0(b_n))) continue;

					/* Track it */
					b_i = i;
					b_c = c;
				}

				/* Try it */
				if (b_i >= 0)
				{
					/* Access the location */
					x = false_x + ddx_ddd[b_i];
					y = false_y + ddy_ddd[b_i];

					/* Display */
					print_rel('*', TERM_RED, x, y);

					/* Simulate motion */
					false_y = y;
					false_x = x;
				}

			}
			msgf("Probable Flow Path");
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case '#':
		{
			/* Command: debug -- danger of grid */
			int n = 0;

			/* Turns */
			n = (p_ptr->cmd.arg ? p_ptr->cmd.arg : 1);

			/* Danger of grid */
			msgf("Danger(%d,%d,%d) is %d",
					   p_ptr->target_col, p_ptr->target_row, n,
					   borg_danger(p_ptr->target_col, p_ptr->target_row, n,
								   TRUE));
			break;
		}

		case '_':
		{
			/* Command:  Regional Fear Info */

			/* Show it */
			borg_display_map_info(0, BORG_SHOW_FEAR);

			/* Get keypress */
			msgf("(%d,%d) Regional Fear.", c_x, c_y);
			message_flush();

			/* Redraw map */
			prt_map();
			break;
		}

		case 'p':
		case 'P':
		{
			/* Command: debug -- Power */
			s32b p;

			/* Examine the screen */
			borg_update_frame();

			/* Examine the screen */
			borg_update();

			/* Extract some "hidden" variables */
			borg_hidden();

			/* Evaluate */
			p = borg_power();

			/* Report it */
			msgf("Current Borg Power %ld", p);
			msgf("Current Home Power %ld", borg_power_home());

			break;
		}

		case '!':
		{
			/* Command: Show time */
			s32b time = borg_t - borg_began;

			msgf("time: (%d) ", (int)time);
			time = (borg_time_town + (borg_t - borg_began));

			msgf("; from town (%d)", (int)time);
			msgf("; need inviso (%d)", (int)need_see_inviso);
			break;
		}

		case '2':
		{
			/* Command: APW HACK debug -- preparation for level */

			int i = 0;

			/* Examine the screen */
			borg_update_frame();
			borg_update();

			/* Extract some "hidden" variables */
			borg_hidden();

			/* Examine the inventory */
			borg_notice();
			borg_notice_home();

			/* Dump prep codes */
			for (i = 1; i <= MAX_DEPTH; i++)
			{
				/* Dump fear code */
				if (borg_prepared(i)) break;
			}

			msgf("Max Level: %d  Prep'd For: %d  Reason: %s",
					   bp_ptr->max_depth, i - 1, borg_prepared(i));

			break;
		}

		case 'o':
		case 'O':
		{
			/* Command: Display all known info on item */
			int n;
			
			bool show_inventory = TRUE;
			char tmp_val[80];

			/* Save the screen */
			Term_save();

			do
			{
				if (inven_num == 0) show_inventory = FALSE;

				if (show_inventory)
				{
					strnfmt(tmp_val, 80, "Inven: / for Equip, ESC) Show borg believes on which item? (a-%c) "
						  , I2A(inven_num - 1));
				}
				else
				{
					strnfmt(tmp_val, 80, "Equip: / for Inven, ESC) Show borg believes on which item? (a-%c) "
						  , I2A(equip_num - 1));
				}

				get_com(tmp_val, &cmd);
				if (cmd == '/') show_inventory = !show_inventory;
			}
			while (cmd == '/');
			
			/* Convert to array index*/
			n = A2I(cmd);

			if (show_inventory)
			{
				/* Bound checking */
				if ((n < 0) || (n >= inven_num)) break;

				/* Display the special screen */
				borg_display_item(&inventory[n]);
			}
			else
			{
				/* Is it legal to use this in equipment[]? */
				if ((n < 0) || (n >= equip_num)) break;
				if (!look_up_equip_slot(n)) break;

				/* Display the special screen */
				borg_display_item(&equipment[n]);
			}

			/* Pause for study */
			msgf("Borg believes: ");
			message_flush();

			/* Restore the screen */
			Term_load();

			break;
		}
			
		case 'd':
		case 'D':
		{
			/*
			 * Dump all the spells from the books of your realms
			 * in your inventory
			 */
			byte spell, inv, ii;
			cptr legal = NULL;

			/* Warriors don't have realms */
			if (borg_class == CLASS_WARRIOR) break;

			/* Save the screen */
			Term_save();

			if (borg_class == CLASS_MINDCRAFTER)
			{
				/* Partly clear the screen */
				clear_region(0, 1, 14);

				ii = 2;

				/* Mindcrafters have no books, just spells */
				for (spell = 0; spell < MINDCRAFT_MAX; spell++)
				{
					/* pick up the spell */
					borg_mind *as = &borg_minds[spell];

					/* Can you cast it at all? */
					legal = (as->level <= bp_ptr->lev) ? "Legal" : "Not legal";

					/* Show spell name, legalility and # of casts */
					put_fstr(1, ii, "%s",	as->name);
					put_fstr(32, ii++, "%s, attempted %d times", legal, as->times);
				}

				/* Give a chance to look */
				msgf("Mindcrafter spells:");
				message_flush();

				/* Restore the screen */
				Term_load();

				/* Done */
				break;
			}

			/* Loop through the inventory */
			for (inv = 0; inv < inven_num; inv++)
			{
				borg_magic *as;
				int realm, book;

				/* Partly clear the screen */
				clear_region(0, 1, 12);

				/* On which line do we print? */
				ii = 4;

				/* Get the realm of this book */
				realm = inventory[inv].tval + 1 - TV_BOOKS_MIN;

				/* Is this one the possible realms? */
				if (borg_has_realm(realm))
				{
					/* Display name of the book */
					put_fstr(1, ii - 2, "%s", inventory[inv].o_name);

					/* Which book of this realm is it exactly? */
					book = k_info[inventory[inv].k_idx].sval;

					for (spell = 0; spell < 8; spell++)
					{
						/* pick up spell */
						as = &borg_magics[realm][book][spell];

						/*  Can you cast it at all? */
						if (as->level < 99)
						{
							legal = (borg_spell_legal(realm, book, spell) ?
								 "Legal" : "Not legal");
						}
	
						/* Show spell name, legalility and # of casts */
						put_fstr(1, ii, "%s",	as->name);
						put_fstr(32, ii++, "%s, attempted %d times", legal, as->times);

					}
					/* Give a chance to look */
					msgf("Examining spell books.");
					message_flush();
				}
			}
		
			/* Restore the screen */
			Term_load();

			/* Done */
			break;
		}
		
		case '?':
		{
			/* Save and clear the screen */
			Term_save();
			Term_clear();

			/* First column */
			prtf(2, 3,
				"Command 'z' activates the Borg.\n"
				"Command 'x' steps the Borg.\n"
				"Command 'u' updates the Borg.\n"
		    	"Command '2' level prep info.\n"
		    	"Command '8' Shows the shops.\n"
		    	"Command '9' Shows the dungeons.\n"
		        "Command 's' activates search mode.\n"
		        "Command 'g' displays grid feature.\n"
	    	    "Command 'k' displays monster info.\n"
	        	"Command '%%' displays current flow.\n"
		        "Command '_' Regional Fear info.\n"
		        "Command 'd' Dump spell info.\n"
	    	    "Command '^' Flow Pathway.");
	 
		    /* Second column */
			prtf(42, 3,
				"Command 'f' modifies the options.\n"
				"Command 'l' activates a log file.\n"
				"Command 'i' displays grid info.\n"
			    "Command 'a' displays avoidances.\n"
			    "Command 't' displays object info.\n"
		    	"Command '#' displays danger grid.\n"
			    "Command 'p' Borg Power.\n"
			    "Command '!' Time.\n"
			    "Command 'y' Last 75 steps.\n"
	    		"Command 'o' Examine Inven Item.");

			/* Prompt for key */
			msgf("Commands: ");
			message_flush();

			/* Restore the screen */
			Term_load();

			/* Done */
			break;
		}

		default:
		{
			/* Oops */

			/* Message */
			msgf("That is not a legal Borg command.");
			break;
		}
	}
}

#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
