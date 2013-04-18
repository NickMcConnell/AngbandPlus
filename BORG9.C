/* File: borg9.c */

/* Purpose: Highest level functions for the Borg -BEN- */
#include "angband.h"



#ifdef ALLOW_BORG

#include "borg1.h"
#include "borg2.h"
#include "borg3.h"
#include "borg4.h"
#include "borg5.h"
#include "borg6.h"
#include "borg7.h"
#include "borg8.h"
#include "borg9.h"

bool borg_cheat_death;
static s16b stat_use[6];

/*
 * This file implements the "Ben Borg", an "Automatic Angband Player".
 *
 * This version of the "Ben Borg" is designed for use with Angband 2.7.9v6.
 *
 * Use of the "Ben Borg" requires re-compilation with ALLOW_BORG defined,
 * and with the various "borg*.c" files linked into the executable.
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
 * When the Ben Borg is "activated", it uses the "Term_inkey_hook" to steal
 * control from the user.  Later, if it detects any input from the real user,
 * it gracefully relinquishes control by clearing the "Term_inkey_hook" after
 * any pending key-sequences are complete.
 *
 * The Borg will abort if it detects any "errors", or if it detects any
 * "situations" such as "death", or if it detects any "panic" situations,
 * such as "imminent death", if the appropriate flags are set.
 *
 * The Ben Borg is only supposed to "know" what is visible on the screen,
 * which it learns by using the "term.c" screen access function "Term_what()",
 * the cursor location function "Term_locate()", and the cursor visibility
 * extraction function "Term_get_cursor()".
 *
 * The Ben Borg is only supposed to "send" keypresses when the "Term_inkey()"
 * function asks for a keypress, which is accomplished by using a special
 * function hook in the "z-term.c" file, which allows the Borg to "steal"
 * control from the "Term_inkey()" and "Term_flush()" functions.  This
 * allows the Ben Borg to pretend to be a normal user.
 *
 * Note that if properly designed, the Ben Borg could be run as an external
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
 * Note that the "c_t" parameter bears a close resemblance to the number of
 * "player turns" that have gone by.  Except that occasionally, the Borg will
 * do something that he *thinks* will take time but which actually does not
 * (for example, attempting to open a hallucinatory door), and that sometimes,
 * the Borg performs a "repeated" command (rest, open, tunnel, or search),
 * which may actually take longer than a single turn.  This has the effect
 * that the "c_t" variable is slightly lacking in "precision".  Note that
 * we can store every time-stamp in a 's16b', since we reset the clock to
 * 1000 on each new level, and we refuse to stay on any level longer than
 * 30000 turns, unless we are totally stuck, in which case we abort.
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
 * Cheats that can be avoided by toggling a switch:
 *   Direct extraction of "panel" info (auto_cheat_panel)
 *   Direct extraction of "inven" info (auto_cheat_inven)
 *   Direct extraction of "equip" info (auto_cheat_equip)
 *   Direct extraction of "spell" info (auto_cheat_spell)
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

static bool initialized;    /* Hack -- Initialized */



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
    for (i = 0; i < 6; i++) my_stat_add[i] = 0;

    /* Scan the usable inventory */
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
    {
        auto_item *item = &auto_items[i];

        /* Skip empty items */
        if (!item->iqty) continue;

        /* if we have on unidentified stuff we may have misguessed our */
        /* stats. */
        if (!item->able)
        {
            my_need_stat_check[0] = TRUE;
            my_need_stat_check[1] = TRUE;
            my_need_stat_check[2] = TRUE;
            my_need_stat_check[3] = TRUE;
            my_need_stat_check[4] = TRUE;
            my_need_stat_check[5] = TRUE;
            break;
        }

        /* Affect stats */
        if (item->flags1 & TR1_STR) my_stat_add[A_STR] += item->pval;
        if (item->flags1 & TR1_INT) my_stat_add[A_INT] += item->pval;
        if (item->flags1 & TR1_WIS) my_stat_add[A_WIS] += item->pval;
        if (item->flags1 & TR1_DEX) my_stat_add[A_DEX] += item->pval;
        if (item->flags1 & TR1_CON) my_stat_add[A_CON] += item->pval;
        if (item->flags1 & TR1_CHR) my_stat_add[A_CHR] += item->pval;
    }

    /* Mega-Hack -- Guess at "my_stat_cur[]" */
    for (i = 0; i < 6; i++)
    {
        int value;

        if (!my_need_stat_check[i]) continue;

        /* Reverse known bonuses to get the base stat value */
        value = modify_stat_value(auto_stat[i], -my_stat_add[i]);

        /* If the displayed stat is 18/220 this was just a guess.  */
        /* The player still needs to take off some stuff to get the */
        /* real value. */
        if (auto_stat[i] < 238)
        {
            my_need_stat_check[i] = FALSE;
        }

        /* Hack -- save the maximum/current stats */
        my_stat_cur[i] = value;

        /* Max stat is the max that the cur stat ever is. */
        if (my_stat_cur[i] > my_stat_max[i])
            my_stat_max[i] = my_stat_cur[i];

        /* if this stat is not in need of fixing and */
        /* it is less than the max it should be equal to the max */
        /* and we are done. */
#if 0
        if (my_need_stat_check[i] && !do_fix_stat[i] &&
            my_stat_max[i] > my_stat_cur[i])
        {
            my_stat_cur[i] = my_stat_max[i];
            my_need_stat_check[i] = FALSE;
        }
#endif
    }
}


/*
 * Think about the world and perform an action
 *
 * Check inventory/equipment/spells/panel once per "turn"
 *
 * Process "store" and other modes when necessary
 *
 * Note that the non-cheating "inventory" and "equipment" parsers
 * will get confused by a "weird" situation involving an ant ("a")
 * on line one of the screen, near the left, next to a shield, of
 * the same color, and using --(-- the ")" symbol, directly to the
 * right of the ant.  This is very rare, but perhaps not completely
 * impossible.  I ignore this situation.  :-)
 *
 * The handling of stores is a complete and total hack, but seems
 * to work remarkably well, considering... :-)  Note that while in
 * a store, time does not pass, and most actions are not available,
 * and a few new commands are available ("sell" and "purchase").
 *
 * Note the use of "cheat" functions to extract the current inventory,
 * the current equipment, the current panel, and the current spellbook
 * information.  These can be replaced by (very expensive) "parse"
 * functions, which cause an insane amount of "screen flashing".
 *
 * Technically, we should attempt to parse all the messages that
 * indicate that it is necessary to re-parse the equipment, the
 * inventory, or the books, and only set the appropriate flags
 * at that point.  This would not only reduce the potential
 * screen flashing, but would also optimize the code a lot,
 * since the "cheat_inven()" and "cheat_equip()" functions
 * are expensive.  For paranoia, we could always select items
 * and spells using capital letters, and keep a global verification
 * buffer, and induce failure and recheck the inventory/equipment
 * any time we get a mis-match.  We could even do some of the state
 * processing by hand, for example, charge reduction and such.  This
 * might also allow us to keep track of how long we have held objects,
 * especially if we attempt to do "item tracking" in the inventory
 * extraction code.
 */
static bool borg_think(void)
{
    int i, ware_num;

    byte t_a;

    char buf[128];


    /*** Process inventory/equipment ***/
    /* Cheat */
    if (auto_cheat_equip && auto_do_equip)
    {
        /* Only do it once */
        auto_do_equip = FALSE;

        /* Cheat the "equip" screen */
        borg_cheat_equip();

        /* Done */
        return (FALSE);
    }

    /* Cheat */
    if (auto_cheat_inven && auto_do_inven)
    {
        /* Only do it once */
        auto_do_inven = FALSE;

        /* Cheat the "inven" screen */
        borg_cheat_inven();

        /* Done */
        return (FALSE);
    }

    /* save now */
    if (borg_save && borg_save_game())
    {
        /* Log */
        borg_note("# Auto Save!");

        borg_save = FALSE;
        return (TRUE);
    }

    /* Parse "equip" mode */
    if ((0 == borg_what_text(0, 0, 10, &t_a, buf)) &&
        (streq(buf, "(Equipment) ")))
    {
        /* Parse the "equip" screen */
        borg_parse_equip();

        /* Leave this mode */
        borg_keypress(ESCAPE);

        /* Done */
        return (TRUE);
    }


    /* Parse "inven" mode */
    if ((0 == borg_what_text(0, 0, 10, &t_a, buf)) &&
        (streq(buf, "(Inventory: ")))
    {
        /* Parse the "inven" screen */
        borg_parse_inven();

        /* Leave this mode */
        borg_keypress(ESCAPE);

        /* Done */
        return (TRUE);
    }


    /* Check "equip" */
    if (auto_do_equip)
    {
        /* Only do it once */
        auto_do_equip = FALSE;

        /* Enter "equip" mode */
        borg_keypress('e');

        /* Done */
        return (TRUE);
    }

    /* Check "inven" */
    if (auto_do_inven)
    {
        /* Only do it once */
        auto_do_inven = FALSE;

        /* Enter "inven" mode */
        borg_keypress('i');

        /* Done */
        return (TRUE);
    }


    /*** Find books ***/

    /* Only if needed */
    if (auto_do_spell && (auto_do_spell_aux == 0))
    {
        /* Assume no books */
        for (i = 0; i < 9; i++) auto_book[i] = -1;

        /* Scan the pack */
        for (i = 0; i < INVEN_PACK; i++)
        {
            auto_item *item = &auto_items[i];

            /* Skip non-books */
            if (item->tval != mb_ptr->spell_book) continue;

            /* Note book locations */
            auto_book[item->sval] = i;
        }
    }

    /*** Process books ***/

    /* Hack -- Warriors never browse */
    if (auto_class == CLASS_WARRIOR) auto_do_spell = FALSE;

    /* Hack -- Blind or Confused prevents browsing */
    if (do_blind || do_confused) auto_do_spell = FALSE;

    /* XXX XXX XXX Dark */

    /* Hack -- Stop doing spells when done */
    if (auto_do_spell_aux > 8) auto_do_spell = FALSE;

    /* Cheat */
    if (auto_cheat_spell && auto_do_spell)
    {
        /* Look for the book */
        i = auto_book[auto_do_spell_aux];

        /* Cheat the "spell" screens (all of them) */
        if (i >= 0)
        {
            /* Cheat that page */
            borg_cheat_spell(auto_do_spell_aux);
        }

        /* Advance to the next book */
        auto_do_spell_aux++;

        /* Done */
        return (FALSE);
    }
    /* Check for "browse" mode */
    if ((0 == borg_what_text(COL_SPELL, ROW_SPELL, -12, &t_a, buf)) &&
        (streq(buf, "Lv Mana Fail")))
    {
        /* Parse the "spell" screen */
        borg_parse_spell(auto_do_spell_aux);

        /* Advance to the next book */
        auto_do_spell_aux++;

        /* Leave that mode */
        borg_keypress(ESCAPE);

        /* Done */
        return (TRUE);
    }
    /* Check "spells" */
    if (auto_do_spell)
    {
        /* Look for the book */
        i = auto_book[auto_do_spell_aux];

        /* Enter the "spell" screen */
        if (i >= 0)
        {
            /* Enter "browse" mode */
            borg_keypress('b');

            /* Pick the next book */
            borg_keypress(I2A(i));

            /* Done */
            return (TRUE);
        }

        /* Advance to the next book */
        auto_do_spell_aux++;

        /* Done */
        return (FALSE);
    }
    /* check for anything that needs *ID* */
    if ( auto_do_star_id )
    {
        if (borg_object_star_id())
        {

           return (TRUE);
        }
        auto_do_star_id = FALSE;
    }
    /* if king and in town, retire.  Let the player do this. */
    if (borg_king && auto_stop_king)
    {
        borg_oops("retire");
    }

    /*** Handle stores ***/

    /* Hack -- Check for being in a store */
    if ((0 == borg_what_text(3, 4, 16, &t_a, buf)) &&
        (streq(buf, "Item Description")))
    {
        /* Assume the Home */
        shop_num = 7;

        /* Extract the "store" name */
        if (0 == borg_what_text(50, 2, -20, &t_a, buf))
        {
            int i;

            /* Check the store names */
            for (i = 0; i < (MAX_STORES ); i++)
            {
                cptr name = (f_name + f_info[FEAT_SHOP_HEAD+i].name);
                if (prefix(buf, name)) shop_num = i;
            }
        }

        /* Hack -- reset page/more */
        auto_shops[shop_num].page = 0;
        auto_shops[shop_num].more = 0;


        /* React to new stores */
        if (auto_do_browse_what != shop_num)
        {
            /* Clear all the items */
            for (i = 0; i < 24; i++)
            {
                /* XXX Wipe the ware */
                WIPE(&auto_shops[shop_num].ware[i], auto_item);
            }

            /* Save the store */
            auto_do_browse_what = shop_num;
        }

        /* Extract the "page", if any */
        if ((0 == borg_what_text(20, 4, 8, &t_a, buf)) &&
            (prefix(buf, "(Page "))) /* --)-- */
        {
            /* Take note of the page */
            auto_shops[shop_num].page = (buf[6] - '0') - 1;
            if (auto_shops[shop_num].page == 0 &&
                (0 == borg_what_text(3, 18, 6, &t_a, buf)) &&
                (prefix(buf, "-more-"))) /* --)-- */
            {
                auto_shops[shop_num].more = 1;
                auto_do_browse = 1;
            }
            else
            {
                auto_shops[shop_num].more = 0;
                auto_do_browse = 0;
            }
        }
        else
        {
            /* Clear the second page */
            for (i = 12; i < 24; i++)
            {
                /* XXX Wipe the ware */
                WIPE(&auto_shops[shop_num].ware[i], auto_item);
            }
        }

        /* Extract the current gold (unless in home) */
        if (0 == borg_what_text(68, 20, -9, &t_a, buf))
        {
            /* Save the gold, if valid */
            if (buf[0]) auto_gold = atol(buf);
            /* ah just cheat it */
            auto_gold = p_ptr->au;
        }

        /* Screens which are wide need different store help */
        if (screen_y == 50) ware_num = 24; else ware_num = 12;

        /* Parse the store (or home) inventory */
        for (i = 0; i < ware_num; i++)
        {
            int n;

            char desc[80];
            char cost[10];

            /* Default to "empty" */
            desc[0] = '\0';
            cost[0] = '\0';

            /* Extract actual index */
            n = auto_shops[shop_num].page * 12 + i;

            /* Verify "intro" to the item */
            if ((0 == borg_what_text(0, i + 6, 3, &t_a, buf)) &&
                (buf[0] == I2A(n)) && (buf[1] == p2) && (buf[2] == ' '))
            {
                int k;

                /* Extract the item description */
                if (0 != borg_what_text(3, i + 6, -65, &t_a, desc))
                {
                    desc[0] = '\0';
                }

                /* Strip trailing spaces */
                for (k = strlen(desc); (k > 0) && (desc[k-1] == ' '); k--) /* loop */;
                desc[k] = '\0';

                /* Extract the item cost in stores */
                if (shop_num != 7)
                {
                    if (0 != borg_what_text(68, i + 6, -9, &t_a, cost))
                    {
                        cost[0] = '\0';
                    }
                }
            }

            /* Ignore "unchanged" descriptions */
            if (streq(desc, auto_shops[shop_num].ware[n].desc)) continue;

            /* Analyze the item */
            borg_item_analyze(&auto_shops[shop_num].ware[n], &store[shop_num].stock[n], desc);

            /*need to be able to analize the home inventory to see if it was */
            /* *fully ID*d. */
            /* This is a BIG CHEAT!  It will be less of a cheat if code is put*/
            /* in place to allow 'I' in stores. */
            if (store[shop_num].stock[n].ident & IDENT_MENTAL)
            {
                /* XXX XXX XXX for now, allways cheat to get info on items at */
                /*   home. */
                borg_object_star_id_aux( &auto_shops[shop_num].ware[n],
                                         &store[shop_num].stock[n]);
                auto_shops[shop_num].ware[n].fully_identified = TRUE;
            }

            /* Hack -- Save the declared cost */
            auto_shops[shop_num].ware[n].cost = atol(cost);
        }

        /* Hack -- browse as needed */
        if (auto_shops[shop_num].more && auto_do_browse)
        {
            /* Check next page */
            borg_keypress(' ');

            /* Done browsing */
            auto_do_browse = FALSE;

            /* Done */
            return (TRUE);
        }

        /* Recheck inventory */
        auto_do_inven = TRUE;


        /* Recheck equipment */
        auto_do_equip = TRUE;

        /* Recheck spells */
        auto_do_spell = TRUE;

        /* Restart spells */
        auto_do_spell_aux = 0;

        /* Hack -- browse again later */
        auto_do_browse = TRUE;

        /* Examine the inventory */
        borg_notice(TRUE);

        /* Evaluate the current world */
        my_power = borg_power();

        /* Hack -- allow user abort */
        if (auto_cancel) return (TRUE);

        /* Think until done */
        return (borg_think_store());
    }


    /*** Determine panel ***/

    /* Hack -- cheat */
    if (auto_cheat_panel)
    {
        /* Cheat */
        w_y = p_ptr->wy;
        w_x = p_ptr->wx;

        /* Done */
        auto_do_panel = FALSE;
    }

    /* Hack -- Check for "sector" mode */
    if ((0 == borg_what_text(0, 0, 16, &t_a, buf)) &&
        (prefix(buf, "Map sector ")))
    {
        /* Hack -- get the panel info */
        w_y = (buf[12] - '0') * (SCREEN_HGT / 2);
        w_x = (buf[14] - '0') * (SCREEN_WID / 2);

        /* Leave panel mode */
        borg_keypress(ESCAPE);

        /* Done */
        return (TRUE);
    }
    /* Check equipment */
    if (auto_do_panel)
    {
        /* Only do it once */
        auto_do_panel = FALSE;

        /* Enter "panel" mode */
        borg_keypress('L');

        /* Done */
        return (TRUE);
    }

    /*** Analyze the Frame ***/

    /* Analyze the frame */
    if (auto_do_frame)
    {
        /* Only once */
        auto_do_frame = FALSE;

        /* Analyze the "frame" */
        borg_update_frame();
    }

    /*** Re-activate Tests ***/

    /* Check equip again later */
    auto_do_equip = TRUE;

    /* Check inven again later */
    auto_do_inven = TRUE;

    /* Check panel again later */
    auto_do_panel = TRUE;

    /* Check frame again later */
    auto_do_frame = TRUE;

    /* Check spells again later */
    auto_do_spell = TRUE;

    /* Hack -- Start the books over */
    auto_do_spell_aux = 0;

    /* set the continous play mode if the game cheat death is on */
    if (cheat_live) borg_cheat_death = TRUE;

    /*** Analyze status ***/

    /* Track best level */
    if (auto_level > auto_max_level) auto_max_level = auto_level;
    if (auto_depth > auto_max_depth)
    {
        auto_max_depth = auto_depth;
        finished_level = FALSE;
    }

    /*** Think about it ***/

    /* Increment the clock */
    c_t++;

    /* Increment the panel clock */
    time_this_panel++;

    /* Examine the screen */
    borg_update();
    /* Extract some "hidden" variables */
    borg_hidden();
    /* Examine the equipment/inventory */
    borg_notice(TRUE);
    /* Evaluate the current world */
    my_power = borg_power();

    /* Hack -- allow user abort */
    if (auto_cancel) return (TRUE);

    /* Do something */
    return (borg_think_dungeon());
}



/*
 * Hack -- methods of hurting a monster (order not important).
 *
 * See "message_pain()" for details.
 */
static cptr suffix_pain[] =
{
    " barely notices.",
    " flinches.",
    " squelches.",
    " quivers in pain.",
    " writhes about.",
    " writhes in agony.",
    " jerks limply.",

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
    /* xxx */

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
    " dissolves!",
    " shrivels away in the light!",
    " changes!",         /* from polymorph spell */
    " blinks away.",                /* RF6_BLINK */
    " teleports away.",  /* RF6_TPORT */
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
static cptr suffix_spell[] =
{
    " makes a high pitched shriek.",        /* RF4_SHRIEK */
    " tries to cast a spell, but fails.",   /* RF4_FAILS */
    " does something.",             /* RF4_XXX3X4 */
    " does something.",             /* RF4_XXX4X4 */
    " fires an arrow.",             /* RF4_ARROW_1 */
    " fires an arrow!",             /* RF4_ARROW_2 */
    " fires a missile.",            /* RF4_ARROW_3 */
    " fires a missile!",            /* RF4_ARROW_4 */
    " breathes acid.",              /* RF4_BR_ACID */
    " breathes lightning.",         /* RF4_BR_ELEC */
    " breathes fire.",              /* RF4_BR_FIRE */
    " breathes frost.",             /* RF4_BR_COLD */
    " breathes gas.",               /* RF4_BR_POIS */
    " breathes nether.",            /* RF4_BR_NETH */
    " breathes light.",             /* RF4_BR_LITE */
    " breathes darkness.",          /* RF4_BR_DARK */
    " breathes confusion.",         /* RF4_BR_CONF */
    " breathes sound.",             /* RF4_BR_SOUN */
    " breathes chaos.",             /* RF4_BR_CHAO */
    " breathes disenchantment.",        /* RF4_BR_DISE */
    " breathes nexus.",             /* RF4_BR_NEXU */
    " breathes time.",              /* RF4_BR_TIME */
    " breathes inertia.",           /* RF4_BR_INER */
    " breathes gravity.",           /* RF4_BR_GRAV */
    " breathes shards.",            /* RF4_BR_SHAR */
    " breathes plasma.",            /* RF4_BR_PLAS */
    " breathes force.",             /* RF4_BR_WALL */
    " does something.",             /* RF4_BR_MANA */
    " does something.",             /* RF4_XXX5X4 */
    " does something.",             /* RF4_XXX6X4 */
    " does something.",             /* RF4_XXX7X4 */
    " does something.",             /* RF4_XXX8X4 */

    " casts an acid ball.",         /* RF5_BA_ACID */
    " casts a lightning ball.",         /* RF5_BA_ELEC */
    " casts a fire ball.",          /* RF5_BA_FIRE */
    " casts a frost ball.",         /* RF5_BA_COLD */
    " casts a stinking cloud.",         /* RF5_BA_POIS */
    " casts a nether ball.",            /* RF5_BA_NETH */
    " gestures fluidly.",           /* RF5_BA_WATE */
    " invokes a mana storm.",           /* RF5_BA_MANA */
    " invokes a darkness storm.",       /* RF5_BA_DARK */
    " draws psychic energy from you!",      /* RF5_DRAIN_MANA */
    " gazes deep into your eyes.",      /* RF5_MIND_BLAST */
    " looks deep into your eyes.",      /* RF5_BRAIN_SMASH */
    " points at you and curses.",       /* RF5_CAUSE_1 */
    " points at you and curses horribly.",  /* RF5_CAUSE_2 */
    " points at you, incanting terribly!",  /* RF5_CAUSE_3 */
    " points at you, screaming the word DIE!",  /* RF5_CAUSE_4 */
    " casts a acid bolt.",          /* RF5_BO_ACID */
    " casts a lightning bolt.",         /* RF5_BO_ELEC */
    " casts a fire bolt.",          /* RF5_BO_FIRE */
    " casts a frost bolt.",         /* RF5_BO_COLD */
    " does something.",             /* RF5_BO_POIS */
    " casts a nether bolt.",            /* RF5_BO_NETH */
    " casts a water bolt.",         /* RF5_BO_WATE */
    " casts a mana bolt.",          /* RF5_BO_MANA */
    " casts a plasma bolt.",            /* RF5_BO_PLAS */
    " casts an ice bolt.",          /* RF5_BO_ICEE */
    " casts a magic missile.",          /* RF5_MISSILE */
    " casts a fearful illusion.",       /* RF5_SCARE */
    " casts a spell, burning your eyes!",   /* RF5_BLIND */
    " creates a mesmerising illusion.",     /* RF5_CONF */
    " drains power from your muscles!",     /* RF5_SLOW */
    " stares deep into your eyes!",     /* RF5_HOLD */

    " concentrates on XXX body.",       /* RF6_HASTE */
    " does something.",             /* RF6_XXX1X6 */
    " concentrates on XXX wounds.",     /* RF6_HEAL */
    " does something.",             /* RF6_XXX2X6 */

    " does something.",             /* RF6_XXX3X6 */
    " does something.",             /* RF6_XXX4X6 */
    " commands you to return.",         /* RF6_TELE_TO */
    " teleports you away.",         /* RF6_TELE_AWAY */
    " gestures at your feet.",          /* RF6_TELE_LEVEL */
    " does something.",             /* RF6_XXX5 */
    " gestures in shadow.",         /* RF6_DARKNESS */
    " casts a spell and cackles evilly.",   /* RF6_TRAPS */
    " tries to blank your mind.",       /* RF6_FORGET */
    " does something.",             /* RF6_XXX6X6 */
    " does something.",             /* RF6_XXX7X6 */
    " does something.",             /* RF6_XXX8X6 */
    " magically summons help!",         /* RF6_S_MONSTER */
    " magically summons monsters!",     /* RF6_S_MONSTERS */
    " magically summons ants.",         /* RF6_S_ANT */
    " magically summons spiders.",      /* RF6_S_SPIDER */
    " magically summons hounds.",       /* RF6_S_HOUND */
    " magically summons hydras.",       /* RF6_S_HYDRA */
    " magically summons an angel!",     /* RF6_S_ANGEL */
    " magically summons a hellish adversary!",  /* RF6_S_DEMON */
    " magically summons an undead adversary!",  /* RF6_S_UNDEAD */
    " magically summons a dragon!",     /* RF6_S_DRAGON */
    " magically summons greater undead!",   /* RF6_S_HI_UNDEAD */
    " magically summons ancient dragons!",  /* RF6_S_HI_DRAGON */
    " magically summons mighty undead opponents!",  /* RF6_S_WRAITH */
    " magically summons special opponents!",        /* RF6_S_UNIQUE */

    NULL
};



#if 0
/* XXX XXX XXX */
msg_format("%^s looks healthier.", m_name);
msg_format("%^s looks REALLY healthy!", m_name);
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

    auto_grid *ag = &auto_grids[g_y][g_x];


    /* Log (if needed) */
    if (auto_fff) borg_info(format("& Msg <%s>", msg));


    /* Hack -- Notice death */
    if (prefix(msg, "You die."))
    {
        /* Abort (unless cheating) */
        if (!(p_ptr->wizard || cheat_live))
        {

            /* Abort */
            borg_oops("death");

            /* Abort right now! */
            auto_active = FALSE;

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
        auto_failure = TRUE;

        /* Flush our key-buffer */
        borg_flush();

        /* Check to see if it was a door then convert it */
        if (ag->feat == FEAT_DOOR_HEAD)
        {
            /* What is my chance of opening the door? */
            if (my_skill_dis < 20)
            {
                /* Set door as jammed, then bash it */
                ag->feat = FEAT_DOOR_HEAD + 0x08;
            }
        }

        /* check for glyphs since we no longer have a launch message */
        if (borg_casted_glyph)
        {
            /* Forget the newly created-though-failed  glyph */
            track_glyph_x[track_glyph_num] = 0;
            track_glyph_y[track_glyph_num] = 0;
            track_glyph_num --;
            borg_note("# Removing glyph from array,");
        }
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
        prefix(msg, "You feel less") ||
        prefix(msg, "Wow!  You feel very"))
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

    /* A bug in the 280 game fails to inscribe {empty} on a staff-wand after
     * being hit by amnesia (if the item had a sale inscription).
     * So we will try to use the wand, see that it is empty then inscribe
     * it ourselves.
     */
    if (prefix(msg, "The wand has no charges left") ||
        prefix(msg, "The staff has no charges left"))
    {
        /* make the inscription */
    }
    /* amnesia attacks, re-id wands, staves, equipment. */
    if (prefix(msg, "Your memories fade"))
    {
        int i;

        /* I was hit by amnesia, forget things */
        /* forget equipment */
        /* Look for an item to forget (equipment) */
        for (i = INVEN_WIELD; i <= INVEN_FEET; i++)
        {
            auto_item *item = &auto_items[i];

            /* Skip empty items */
            if (!item->iqty) continue;

            /* Skip known items */
            if (item->fully_identified) continue;

            /* skip certain easy know items */
            if ((item->tval == TV_RING) &&
                ((item->sval == SV_RING_FREE_ACTION) ||
                 (item->sval == SV_RING_SEE_INVIS) ||
                 (item->sval <= SV_RING_SUSTAIN_CHR))) continue;

            /* skip already forgotten or non id'd items */
            if (item->able == FALSE) continue;

            /* forget it */
            item->able = FALSE;

            /* note the forgeting */
            borg_note(format("Borg 'forgeting' qualities of %s",item->desc));

        }

        /* Look for an item to forget (inventory) */
        for (i = 0; i <= INVEN_PACK; i++)
        {
            auto_item *item = &auto_items[i];

            /* Skip empty items */
            if (!item->iqty) continue;

            /* skip certain easy know items */
            if ((item->tval == TV_RING) &&
                (item->flags3 & TR3_EASY_KNOW)) continue;

            if (item->fully_identified) continue;
            /* skip already forgotten or non id'd items */
            if (item->able == FALSE) continue;
            switch (item->tval)
            {
                /* forget wands, staffs, weapons, armour */
                case TV_WAND:
                case TV_STAFF:
                case TV_RING:
                case TV_AMULET:
                case TV_LITE:
                case TV_SHOT:
                case TV_ARROW:
                case TV_BOLT:
                case TV_ROD:
                case TV_BOW:
                case TV_DIGGING:
                case TV_HAFTED:
                case TV_POLEARM:
                case TV_SWORD:
                case TV_BOOTS:
                case TV_GLOVES:
                case TV_HELM:
                case TV_CROWN:
                case TV_SHIELD:
                case TV_CLOAK:
                case TV_SOFT_ARMOR:
                case TV_HARD_ARMOR:
                case TV_DRAG_ARMOR:
                break;

                default:
                    continue;
            }
                /* forget it */
                item->able = FALSE;

                /* note the forgetting */
                borg_note(format("Borg 'forgetting' qualities of %s",item->desc));
         }
    }

    /* Hit somebody */
    if (prefix(msg, "You hit "))
    {
        tmp = strlen("You hit ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }

    /* Removed rubble.  Important when out of lite */
    if (prefix(msg, "You have removed the "))
    {
        int x, y;
        /* remove rubbles from array */
        for (y = c_y -1; y < c_y +1; y++)
        {
            for (x = c_x -1; x < c_x +1; x++)
            {
                /* replace all rubble with broken doors, the borg ignores
                 * broken doors.  This routine is only needed if the borg
                 * is out of lite and searching in the dark.
                 */
                 if (my_cur_lite) continue;

                 if (ag->feat == FEAT_RUBBLE) ag->feat = FEAT_BROKEN;
             }
         }
        return;
    }


    /* Hit somebody */
    if (prefix(msg, "You strike "))
    {
        tmp = strlen("You strike ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You hack at "))
    {
        tmp = strlen("You hack at ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You bash "))
    {
        tmp = strlen("You bash ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You slash "))
    {
        tmp = strlen("You slash ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You pound "))
    {
        tmp = strlen("You pound ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You score "))
    {
        tmp = strlen("You score ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You batter "))
    {
        tmp = strlen("You batter ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You gouge "))
    {
        tmp = strlen("You gouge ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }

    /* Hit somebody */
    if (prefix(msg, "You bludgeon "))
    {
        tmp = strlen("You bludgeon ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }
    /* Hit somebody */
    if (prefix(msg, "You *smite* "))
    {
        tmp = strlen("You *smite* ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }

    /* Hit somebody */
    if (prefix(msg, "You bite "))
    {
        tmp = strlen("You bite ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
    }

    /* Hit somebody */
    if (prefix(msg, "You claw "))
    {
        tmp = strlen("You claw ");
        strnfmt(who, 1 + len - (tmp + 1), "%s", msg + tmp);
        strnfmt(buf, 256, "HIT:%^s", who);
        borg_react(msg, buf);
        return;
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

    /* "It disappears via teleport other." */
    if (suffix(msg, " disappears!"))
    {
        tmp = strlen(" disappears!");
        strnfmt(who, 1 + len - tmp, "%s", msg);
        strnfmt(buf, 256, "KILL:%^s", who);
        borg_react(msg, buf);
        return;
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
            strnfmt(buf, 256, "SPELL_%03d:%^s", 96+i, who);
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
        /* Only process open doors */
        if (ag->feat == FEAT_OPEN)
        {
            /* Mark as broken */
            ag->feat = FEAT_BROKEN;

            /* Clear goals */
            goal = 0;
        }
        return;
    }

    /* Feature XXX XXX XXX */
    if (streq(msg, "The door appears to be stuck."))
    {
        /* Only process non-jammed doors */
        if ((ag->feat >= FEAT_DOOR_HEAD) && (ag->feat <= FEAT_DOOR_HEAD + 0x07))
        {
            /* Mark the door as jammed */
            ag->feat = FEAT_DOOR_HEAD + 0x08;

            /* Clear goals */
            goal = 0;
        }

        return;
    }



    /* Feature XXX XXX XXX */
    if (streq(msg, "This seems to be permanent rock."))
    {
        /* Only process walls */
        if ((ag->feat >= FEAT_WALL_EXTRA) && (ag->feat <= FEAT_PERM_SOLID))
        {
            /* Mark the wall as permanent */
            ag->feat = FEAT_PERM_EXTRA;

            /* Clear goals */
            goal = 0;
        }

        return;
    }

    /* Feature XXX XXX XXX */
    if (streq(msg, "You tunnel into the granite wall."))
    {
        /* reseting my panel clock */
        time_this_panel = 1;

        /* Only process walls */
        if ((ag->feat >= FEAT_WALL_EXTRA) && (ag->feat <= FEAT_PERM_SOLID))
        {
            /* Mark the wall as granite */
            ag->feat = FEAT_WALL_EXTRA;

            /* Clear goals */
            goal = 0;
        }

        return;
    }


    /* Feature XXX XXX XXX */
    if (streq(msg, "You tunnel into the quartz vein."))
    {
        /* Process magma veins with treasure */
        if (ag->feat == FEAT_MAGMA_K)
        {
            /* Mark the vein */
            ag->feat = FEAT_QUARTZ_K;

            /* Clear goals */
            goal = 0;
        }

        /* Process magma veins */
        else if (ag->feat == FEAT_MAGMA)
        {
            /* Mark the vein */
            ag->feat = FEAT_QUARTZ;

            /* Clear goals */
            goal = 0;
        }

        return;
    }

    /* Feature XXX XXX XXX */
    if (streq(msg, "You tunnel into the magma vein."))
    {
        /* Process quartz veins with treasure */
        if (ag->feat == FEAT_QUARTZ_K)
        {
            /* Mark the vein */
            ag->feat = FEAT_MAGMA_K;

            /* Clear goals */
            goal = 0;
        }

        /* Process quartz veins */
        else if (ag->feat == FEAT_QUARTZ)
        {
            /* Mark the vein */
            ag->feat = FEAT_MAGMA;

            /* Clear goals */
            goal = 0;
        }

        return;
    }

    /* Wearing Cursed Item */
    if (prefix(msg, "Oops! It feels deathly cold!"))
    {
        /* Hack -- Oops */
        borg_wearing_cursed =TRUE;
        return;
    }

    /* Nasty poison */
    if (prefix(msg, "You feel deathly cold"))
    {
        /* Hack --  */
        do_nasty_pois =TRUE;
        return;
    }

    /* Nasty poison */
    if (prefix(msg, "You feel revived"))
    {
        /* Hack --  */
        do_nasty_pois =FALSE;
        return;
    }

    /* Hack to protect against clock overflows and errors */
    if (prefix(msg, "Illegal "))
    {
        /* Hack -- Oops */
        borg_respawn = 5;
        borg_keypress(ESCAPE);
        borg_keypress(ESCAPE);
        time_this_panel +=100;
        return;
    }


    /* Word of Recall -- Ignition */
    if (prefix(msg, "The air about you becomes "))
    {
        /* Initiate recall */
        goal_recalling = TRUE;
        return;
    }

    /* Word of Recall -- Lift off */
    if (prefix(msg, "You feel yourself yanked "))
    {
        /* Recall complete */
        goal_recalling = FALSE;
        finished_level = FALSE;
        return;
    }

    /* Word of Recall -- Cancelled */
    if (prefix(msg, "A tension leaves "))
    {
        /* Hack -- Oops */
        goal_recalling = FALSE;
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
    if (prefix(msg, "You fell less Berserk."))
    {
        borg_berserk = FALSE;
        return;
    }

    /* check for wall blocking but not when confused*/
    if ((prefix(msg, "There is a wall ") &&
        (!do_confused)))
    {
        my_need_redraw = TRUE;
        my_need_alter = TRUE;
        goal = 0;
        return;
    }


    /* check for closed door but not when confused*/
    if ((prefix(msg, "There is a closed door blocking your way.") &&
        (!do_confused)))
    {
        my_need_redraw = TRUE;
        my_need_alter = TRUE;
        goal = 0;
        return;
    }

    /* check for mis-alter command.  Sometime induced by never_move guys*/
    if (streq(msg, "You spin around.") &&
    !do_confused)
    {
        /* Examine all the monsters */
        for (i = 1; i < auto_kills_nxt; i++)
        {
            auto_kill *kill = &auto_kills[i];

            /* Skip dead monsters */
            if (!kill->r_idx) continue;

            /* Distance components */
            if (c_y == kill->y && c_x == kill->x)
            {
                /* Hack -- kill em */
                borg_delete_kill(i);
            }
        }
        my_no_alter = TRUE;
        goal = 0;
        return;
    }

    /* check for mis-alter command.  Sometime induced by coins*/
    if (prefix(msg, "You see nothing"))
    {
        my_no_alter = TRUE;
        /* Clear goals */
        goal = 0;
        return;
    }

    /* check for missed full store.*/
    if (prefix(msg, "I have not the room"))
    {
        time_this_panel += 10;
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
        borg_goi = 14; /* keep track of how long it has left (a guess) */
        return;
    }
    if (prefix(msg, "You feel vulnerable once more."))
    {
        borg_goi = 0;
        return;
    }

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

    /* Glyph of Warding (the spell no longer gives a report)*/
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
        /* Reckless, I know, but assume broken glyph is the one under us */

        /* Check all existing glyphs */
        for (i = 0; i < track_glyph_num; i++)
        {
            if ((track_glyph_y[i] == c_y) && (track_glyph_x[i] == c_x))
            {
                /* Forget the newly broken glyph */
                track_glyph_x[i] = 0;
                track_glyph_y[i] = 0;
            }
        }
        return;
    }
    /* failed glyph spell message */
    if (prefix(msg, "The object resists the spell"))
    {

        /* Forget the newly created-though-failed  glyph */
        track_glyph_x[track_glyph_num] = 0;
        track_glyph_y[track_glyph_num] = 0;
        track_glyph_num --;

        /* note it */
        borg_note("# Removing the Glyph under me, placing with broken door.");

        /* mark that we are not on a clear spot.  The borg ignores
         * broken doors and this will keep him from casting it again.
         */
        ag->feat = FEAT_BROKEN;
        return;
    }

    if (prefix(msg, "The enchantment failed"))
    {
        /* reset our panel clock for this */
        time_this_panel = 1;
        return;
    }

    /* need to kill monsters when WoD is used */
    if (prefix(msg, "There is a searing blast of light!"))
    {
        /* Examine all the monsters */
        for (i = 1; i < auto_kills_nxt; i++)
        {
            auto_kill *kill = &auto_kills[i];

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
            borg_delete_kill(i);
        }
        /* clear regional fear as well */
        auto_fear_region[c_y/11][c_x/11] = 0;

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
        for (j = i = 0; i < len-1; i++)
        {
            /* Check for punctuation */
            if ((buf[i] == '.') ||
                (buf[i] == '!') ||
                (buf[i] == '?') ||
                (buf[i] == '"'))
            {
                /* Require space */
                if (buf[i+1] == ' ')
                {
                    /* Terminate */
                    buf[i+1] = '\0';

                    /* Parse fragment */
                    borg_parse_aux(buf + j, (i + 1) - j);

                    /* Restore */
                    buf[i+1] = ' ';

                    /* Advance past spaces */
                    for (j = i + 2; buf[j] == ' '; j++) /* loop */;
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
        len += strnfmt(buf+len, 1024-len, "%s", msg+1);
    }

    /* New message */
    else
    {
        /* Collect, verify, and grow */
        len = strnfmt(buf, 1024, "%s", msg);
    }
}

/* Next set is for the borg in continuous play mode */
static int adjust_stat_borg(int value, int amount, int auto_roll)
{
	/* Negative amounts or maximize mode */
    if ((amount < 0) || p_ptr->maximise)
	{
		return (modify_stat_value(value, amount));
	}

	/* Special hack */
	else
	{
		int i;

		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else if (value < 18+70)
			{
				value += ((auto_roll ? 15 : randint(15)) + 5);
			}
			else if (value < 18+90)
			{
				value += ((auto_roll ? 6 : randint(6)) + 2);
			}
			else if (value < 18+100)
			{
				value++;
			}
		}
	}

	/* Return the result */
	return (value);
}

static void get_stats_borg_aux(void)
{
	int i, j;

	int bonus;

	int dice[18];

	/* Roll and verify some stats */
	while (TRUE)
	{
		/* Roll some dice */
		for (j = i = 0; i < 18; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals */
		if ((j > 42) && (j < 54)) break;
	}

	/* Roll the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Variable stat maxes */
        if (p_ptr->maximise)
		{
			/* Start fully healed */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/class bonuses */
			stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
            stat_use[i] = adjust_stat_borg(p_ptr->stat_max[i], bonus, FALSE);

			/* Save the resulting stat maximum */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];
		}
	}
}

/*
 * Roll for a new characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats_borg(void)
{
	int i, j;

	int bonus;

	int dice[18];
    s16b stat_limit[6];

    s32b auto_round = 0L;

    /* load up the minimal stat values from angband.ini */
    stat_limit[0] = (auto_respawn_str > 0) ? auto_respawn_str  : 0;
    stat_limit[1] = (auto_respawn_int > 0) ? auto_respawn_int  : 0;
    stat_limit[2] = (auto_respawn_wis > 0) ? auto_respawn_wis  : 0;
    stat_limit[3] = (auto_respawn_dex > 0) ? auto_respawn_dex  : 0;
    stat_limit[4] = (auto_respawn_con > 0) ? auto_respawn_con  : 0;
    stat_limit[5] = (auto_respawn_chr > 0) ? auto_respawn_chr  : 0;

    /* minimal stats selected */
    if (stat_limit[0] + stat_limit[1] + stat_limit[2] + stat_limit[3] +
        stat_limit[4] + stat_limit[5] >= 1)
    {
        /* Auto-roll */
        while (1)
        {
            bool accept = TRUE;

            /* Get a new character */
            get_stats_borg_aux();

            /* Advance the round */
            auto_round++;

            /* Hack -- Prevent overflow */
            if (auto_round >= 64000L)
            {
                borg_note("# Minimal Stats too high.");
                break;
            }

            /* Check and count acceptable stats */
            for (i = 0; i < A_MAX; i++)
            {
                /* This stat is okay */
                if (stat_use[i] >= stat_limit[i])
                {
                }
                /* This stat is not okay */
                else
                {
                    accept = FALSE;
                }
             }

             /* Break if "happy" */
             if (accept) break;
        } /* while */
    } /* minimal stats */
    /* Otherwise just get a character */
    else
    {
        borg_note("# Rolling random stats.");
        get_stats_borg_aux();
    }

    borg_note(format("# Rolling Stats. Attempted #%d times.", auto_round));

}

/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra_borg(void)
{
	int i, j, min_value, max_value;


	/* Level one */
	p_ptr->max_lev = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Minimum hitpoints at highest level */
	min_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 3) / 8;
	min_value += PY_MAX_LEVEL;

	/* Maximum hitpoints at highest level */
	max_value = (PY_MAX_LEVEL * (p_ptr->hitdie - 1) * 5) / 8;
	max_value += PY_MAX_LEVEL;

	/* Pre-calculate level 1 hitdice */
	p_ptr->player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */
	while (TRUE)
	{
		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			j = randint(p_ptr->hitdie);
			p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + j;
		}

		/* XXX Could also require acceptable "mid-level" hitpoints */

		/* Require "valid" hitpoints at highest level */
		if (p_ptr->player_hp[PY_MAX_LEVEL-1] < min_value) continue;
		if (p_ptr->player_hp[PY_MAX_LEVEL-1] > max_value) continue;

		/* Acceptable */
		break;
	}
}

/*
 * Forward declare
 */
typedef struct hist_type hist_type;

/*
 * Player background information
 */
struct hist_type
{
	cptr info;			    /* Textual History */

	byte roll;			    /* Frequency of this entry */
	byte chart;			    /* Chart index */
	byte next;			    /* Next chart index */
	byte bonus;			    /* Social Class Bonus + 50 */
};


/*
 * Background information (see below)
 *
 * Chart progression by race:
 *   Human/Dunadan -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Elf      -->  4 -->  1 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Elf/High-Elf  -->  7 -->  8 -->  9 --> 54 --> 55 --> 56
 *   Hobbit        --> 10 --> 11 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Gnome         --> 13 --> 14 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Dwarf         --> 16 --> 17 --> 18 --> 57 --> 58 --> 59 --> 60 --> 61
 *   Half-Orc      --> 19 --> 20 -->  2 -->  3 --> 50 --> 51 --> 52 --> 53
 *   Half-Troll    --> 22 --> 23 --> 62 --> 63 --> 64 --> 65 --> 66
 *
 * XXX XXX XXX This table *must* be correct or drastic errors may occur!
 */
static hist_type bg[] =
{
	{"You are the illegitimate and unacknowledged child ",           10, 1, 2, 25},
	{"You are the illegitimate but acknowledged child ",             20, 1, 2, 35},
	{"You are one of several children ",                             95, 1, 2, 45},
	{"You are the first child ",                                    100, 1, 2, 50},
	{"of a Serf.  ",                                                 40, 2, 3, 65},
	{"of a Yeoman.  ",                                               65, 2, 3, 80},
	{"of a Townsman.  ",                                             80, 2, 3, 90},
	{"of a Guildsman.  ",                                            90, 2, 3,105},
	{"of a Landed Knight.  ",                                        96, 2, 3,120},
	{"of a Titled Noble.  ",                                         99, 2, 3,130},
	{"of a Royal Blood Line.  ",                                    100, 2, 3,140},
	{"You are the black sheep of the family.  ",                     20, 3,50, 20},
	{"You are a credit to the family.  ",                            80, 3,50, 55},
	{"You are a well liked child.  ",                               100, 3,50, 60},
	{"Your mother was a Green-Elf.  ",                               40, 4, 1, 50},
	{"Your father was a Green-Elf.  ",                               75, 4, 1, 55},
	{"Your mother was a Grey-Elf.  ",                                90, 4, 1, 55},
	{"Your father was a Grey-Elf.  ",                                95, 4, 1, 60},
	{"Your mother was a High-Elf.  ",                                98, 4, 1, 65},
	{"Your father was a High-Elf.  ",                               100, 4, 1, 70},
	{"You are one of several children ",                             60, 7, 8, 50},
	{"You are the only child ",                                     100, 7, 8, 55},
	{"of a Green-Elf ",                                              75, 8, 9, 50},
	{"of a Grey-Elf ",                                               95, 8, 9, 55},
	{"of a High-Elf ",                                              100, 8, 9, 60},
	{"Ranger.  ",                                                    40, 9,54, 80},
	{"Archer.  ",                                                    70, 9,54, 90},
	{"Warrior.  ",                                                   87, 9,54,110},
	{"Mage.  ",                                                      95, 9,54,125},
	{"Prince.  ",                                                    99, 9,54,140},
	{"King.  ",                                                     100, 9,54,145},
	{"You are one of several children of a Hobbit ",                 85,10,11, 45},
	{"You are the only child of a Hobbit ",                         100,10,11, 55},
	{"Tramp.  ",                                                       20,11, 3, 55},
	{"Tavern Owner.  ",                                              30,11, 3, 80},
	{"Miller.  ",                                                    40,11, 3, 90},
	{"Home Owner.  ",                                                50,11, 3,100},
	{"Burglar.  ",                                                   80,11, 3,110},
	{"Warrior.  ",                                                   95,11, 3,115},
	{"Mage.  ",                                                      99,11, 3,125},
	{"Clan Elder.  ",                                               100,11, 3,140},
	{"You are one of several children of a Gnome ",                  85,13,14, 45},
	{"You are the only child of a Gnome ",                          100,13,14, 55},
	{"Beggar.  ",                                                    20,14, 3, 55},
	{"Braggart.  ",                                                  50,14, 3, 70},
	{"Prankster.  ",                                                 75,14, 3, 85},
	{"Warrior.  ",                                                   95,14, 3,100},
	{"Mage.  ",                                                     100,14, 3,125},
	{"You are one of two children of a Dwarven ",                    25,16,17, 40},
	{"You are the only child of a Dwarven ",                        100,16,17, 50},
	{"Thief.  ",                                                     10,17,18, 60},
	{"Prison Guard.  ",                                              25,17,18, 75},
	{"Miner.  ",                                                     75,17,18, 90},
	{"Warrior.  ",                                                   90,17,18,110},
	{"Priest.  ",                                                    99,17,18,130},
	{"King.  ",                                                     100,17,18,150},
	{"You are the black sheep of the family.  ",                     15,18,57, 10},
	{"You are a credit to the family.  ",                            85,18,57, 50},
	{"You are a well liked child.  ",                               100,18,57, 55},
	{"Your mother was an Orc, but it is unacknowledged.  ",          25,19,20, 25},
	{"Your father was an Orc, but it is unacknowledged.  ",         100,19,20, 25},
	{"You are the adopted child ",                                  100,20, 2, 50},
	{"Your mother was a Cave-Troll ",                                30,22,23, 20},
	{"Your father was a Cave-Troll ",                                60,22,23, 25},
	{"Your mother was a Hill-Troll ",                                75,22,23, 30},
	{"Your father was a Hill-Troll ",                                90,22,23, 35},
	{"Your mother was a Water-Troll ",                               95,22,23, 40},
	{"Your father was a Water-Troll ",                              100,22,23, 45},
	{"Cook.  ",                                                       5,23,62, 60},
	{"Warrior.  ",                                                   95,23,62, 55},
	{"Shaman.  ",                                                    99,23,62, 65},
	{"Clan Chief.  ",                                               100,23,62, 80},
	{"You are the only child of a Pixie ",                           15,31,32, 55},
	{"You are one of several children of a Pixie ",                 100,31,32, 45},
	{"Vagrant.  ",                                                   20,32,3,  60},
	{"Rogue.  ",                                                     40,32,3,  75},
	{"Ranger.  ",                                                    50,32,3,  90},
	{"Priest.  ",                                                    75,32,3, 100},
	{"Mage.  ",                                                     100,32,3, 120},
	{"You are one of five children of a blue Yeek.  ",               25,33,3,  50},
	{"You are one of five children of a brown Yeek.  ",              75,33,3,  75},
	{"You are one of five children on a master yeek.  ",            100,33,3, 100},
	{"You are a crystal dragon from the north.  ",                   25,34,44, 40},
	{"You are a crystal dragon from the nearby mountains.  ",        75,34,44, 50},
	{"You are a crystal dragon from the fortress of Angband.  ",    100,34,44, 40},
	{"You are a copper dragon from the east.  ",                     25,37,44, 35},
	{"You are a copper dragon from the nearby swamp.  ",             75,37,44, 40},
	{"You are a copper dragon from the far south.  ",               100,37,44, 35},
	{"You are a bronze dragon from Mirkwood.  ",                     25,40,44, 40},
	{"You are a bronze dragon from the nearby forest.  ",            50,40,44, 50},
	{"You are a bronze dragon from the fortress of Angband.  ",     100,40,44, 30},
	{"You are a gold dragon from the Wild.  ",                       25,43,44, 40},
	{"You are a gold dragon from the nearby grasslands.  ",          50,43,44, 50},
	{"You are a gold dragon from the fortress of Angband.  ",       100,43,44, 30},
	{"After being thrown out of home you ",                          75,44,45, 40},
	{"After the untimely death of your parents you ",               100,44,45, 50},
	{"lived like an animal in the wilderness.  ",                    33,45,48, 45},
	{"were captured and trained by a company of gnomes.  ",          67,45,48, 60},
	{"found solace with a group of Dunaedan rangers.  ",            100,45,48, 75},
	{"You are a pseudo dragon from the Misty Mountains.  ",          25,46,44, 45},
	{"You are a pseudo dragon from the fortress of Angband.  ",      75,46,44, 40},
	{"You are a pseudo dragon from the Whithered Heath.  ",         100,46,44, 50},
	{"You have glittering black eyes ",                              50,48,49, 50},
	{"You have glowing red eyes ",                                   70,48,49, 45},
	{"You have softly glowing, iridescent eyes ",                   100,48,49, 60},
	{"and shiny scales.  ",                                          50,49, 0, 50},
	{"and dull, weathered scales.  ",                                75,49, 0, 45},
	{"and glittering scales.  ",                                    100,49, 0, 55},
	{"You have dark brown eyes, ",                                   20,50,51, 50},
	{"You have brown eyes, ",                                        60,50,51, 50},
	{"You have hazel eyes, ",                                        70,50,51, 50},
	{"You have green eyes, ",                                        80,50,51, 50},
	{"You have blue eyes, ",                                         90,50,51, 50},
	{"You have blue-gray eyes, ",                                   100,50,51, 50},
	{"straight ",                                                    70,51,52, 50},
	{"wavy ",                                                        90,51,52, 50},
	{"curly ",                                                      100,51,52, 50},
	{"black hair, ",                                                 30,52,53, 50},
	{"brown hair, ",                                                 70,52,53, 50},
	{"auburn hair, ",                                                80,52,53, 50},
	{"red hair, ",                                                   90,52,53, 50},
	{"blond hair, ",                                                100,52,53, 50},
	{"and a very dark complexion.",                                  10,53, 0, 50},
	{"and a dark complexion.",                                       30,53, 0, 50},
	{"and an average complexion.",                                   80,53, 0, 50},
	{"and a fair complexion.",                                       90,53, 0, 50},
	{"and a very fair complexion.",                                 100,53, 0, 50},
	{"You have light grey eyes, ",                                   85,54,55, 50},
	{"You have light blue eyes, ",                                   95,54,55, 50},
	{"You have light green eyes, ",                                 100,54,55, 50},
	{"straight ",                                                    75,55,56, 50},
	{"wavy ",                                                       100,55,56, 50},
	{"black hair, and a fair complexion.",                           75,56, 0, 50},
	{"brown hair, and a fair complexion.",                           85,56, 0, 50},
	{"blond hair, and a fair complexion.",                           95,56, 0, 50},
	{"silver hair, and a fair complexion.",                         100,56, 0, 50},
	{"You have dark brown eyes, ",                                   99,57,58, 50},
	{"You have glowing red eyes, ",                                 100,57,58, 60},
	{"straight ",                                                    90,58,59, 50},
	{"wavy ",                                                       100,58,59, 50},
	{"black hair, ",                                                 75,59,60, 50},
	{"brown hair, ",                                                100,59,60, 50},
	{"a one foot beard, ",                                           25,60,61, 50},
	{"a two foot beard, ",                                           60,60,61, 51},
	{"a three foot beard, ",                                         90,60,61, 53},
	{"a four foot beard, ",                                         100,60,61, 55},
	{"and a dark complexion.",                                      100,61, 0, 50},
	{"You have slime green eyes, ",                                  60,62,63, 50},
	{"You have puke yellow eyes, ",                                  85,62,63, 50},
	{"You have blue-bloodshot eyes, ",                               99,62,63, 50},
	{"You have glowing red eyes, ",                                 100,62,63, 55},
	{"dirty ",                                                       33,63,64, 50},
	{"mangy ",                                                       66,63,64, 50},
	{"oily ",                                                       100,63,64, 50},
	{"sea-weed green hair, ",                                        33,64,65, 50},
	{"bright red hair, ",                                            66,64,65, 50},
	{"dark purple hair, ",                                          100,64,65, 50},
	{"and green ",                                                   25,65,66, 50},
	{"and blue ",                                                    50,65,66, 50},
	{"and white ",                                                   75,65,66, 50},
	{"and black ",                                                  100,65,66, 50},
	{"ulcerous skin.",                                               33,66, 0, 50},
	{"scabby skin.",                                                 66,66, 0, 50},
	{"leprous skin.",                                               100,66, 0, 50},
	{"You are a multi-hued dragon from the Wild.  ",                 25,67,44, 40},
	{"You are a multi-hued dragon from the Misty Mountains.  ",      50,67,44, 50},
	{"You are a multi-hued dragon from the fortress of Angband.  ", 100,67,44, 30}
};


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history_borg(void)
{
	int i, n, chart, roll, social_class;

	char *s, *t;

	char buf[240];



	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) p_ptr->history[i][0] = '\0';


	/* Clear the history text */
	buf[0] = '\0';

	/* Initial social class */
	social_class = randint(4);

	/* Starting place */
	switch (p_ptr->prace)
	{
		case RACE_HUMAN:
		case RACE_DUNADAN:
		{
			chart = 1;
			break;
		}

		case RACE_HALF_ELF:
		{
			chart = 4;
			break;
		}

		case RACE_ELF:
		case RACE_HIGH_ELF:
		{
			chart = 7;
			break;
		}

		case RACE_HOBBIT:
		{
			chart = 10;
			break;
		}

		case RACE_GNOME:
		{
			chart = 13;
			break;
		}

		case RACE_DWARF:
		{
			chart = 16;
			break;
		}

		case RACE_HALF_ORC:
		{
			chart = 19;
			break;
		}

		case RACE_HALF_TROLL:
		{
			chart = 22;
			break;
		}

		case RACE_PIXIE:
		{
			chart = 31;
			break;
		}

		case RACE_YEEK:
		{
			chart = 33;
			break;
		}

		case RACE_CRYSTALDRAG:
		{
			chart = 34;
			break;
		}

		case RACE_COPPERDRAG:
		{
			chart = 37;
			break;
		}

		case RACE_BRONZEDRAG:
		{
			chart = 40;
			break;
		}

		case RACE_GOLDDRAG:
		{
			chart = 43;
			break;
		}

		case RACE_PSEUDODRAG:
		{
			chart = 46;
			break;
		}

		case RACE_MULTIHUEDDRAG:
		{
			chart = 67;
			break;
		}

		default:
		{
			chart = 0;
			break;
		}
	}

	/* Process the history */
	while (chart)
	{
		/* Start over */
		i = 0;

		/* Roll for nobility */
		roll = randint(100);

		/* Get the proper entry in the table */
        while ((chart != bg[i].chart) || (roll > bg[i].roll)) i++;

		/* Get the textual history */
        strcat(buf, (bg[i].info));

		/* Add in the social class */
        social_class += (int)(bg[i].bonus) - 50;

		/* Enter the next chart */
        chart = bg[i].next;
	}



	/* Verify social class */
	if (social_class > 100) social_class = 100;
	else if (social_class < 1) social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';


	/* Start at first line */
	i = 0;

	/* Collect the history */
	while (TRUE)
	{
		/* Extract remaining length */
		n = strlen(s);

		/* All done */
		if (n < 60)
		{
			/* Save one line of history */
			strcpy(p_ptr->history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 60; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

		/* Save one line of history */
		strcpy(p_ptr->history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */;
	}
}


/*
 * Computes character's age, height, and weight
 */
static void get_ahw_borg(void)
{
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = Rand_normal(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = Rand_normal(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = Rand_normal(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
	}
}




/*
 * Get the player's starting money
 */
static void get_money_borg(void)
{
	int i;

    int gold = 0;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Process the stats */
	for (i = 0; i < A_MAX; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 18+50) gold -= 300;
		else if (stat_use[i] >= 18+20) gold -= 200;
		else if (stat_use[i] > 18) gold -= 150;
		else gold -= (stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}
/*
 * Name segments for random player names
 * Copied Cth by DvE
 * Copied from borgband by APW
 */

/* Dwarves */
static char *dwarf_syllable1[] =
{
	"B", "D", "F", "G", "Gl", "H", "K", "L", "M", "N", "R", "S", "T", "Th", "V",
};

static char *dwarf_syllable2[] =
{
	"a", "e", "i", "o", "oi", "u",
};

static char *dwarf_syllable3[] =
{
	"bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar", "nus", "rin", "ran", "sin", "sil", "sur",
};

/* Elves */
static char *elf_syllable1[] =
{
	"Al", "An", "Bal", "Bel", "Cal", "Cel", "El", "Elr", "Elv", "Eow", "Ear", "F", "Fal", "Fel", "Fin", "G", "Gal", "Gel", "Gl", "Is", "Lan", "Leg", "Lom", "N", "Nal", "Nel",  "S", "Sal", "Sel", "T", "Tal", "Tel", "Thr", "Tin",
};

static char *elf_syllable2[] =
{
	"a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra", "ilma", "il-Ga", "ili", "o", "orfi", "u", "y",
};

static char *elf_syllable3[] =
{
	"l", "las", "lad", "ldor", "ldur", "linde", "lith", "mir", "n", "nd", "ndel", "ndil", "ndir", "nduil", "ng", "mbor", "r", "rith", "ril", "riand", "rion", "s", "thien", "viel", "wen", "wyn",
};

/* Gnomes */
static char *gnome_syllable1[] =
{
	"Aar", "An", "Ar", "As", "C", "H", "Han", "Har", "Hel", "Iir", "J", "Jan", "Jar", "K", "L", "M", "Mar", "N", "Nik", "Os", "Ol", "P", "R", "S", "Sam", "San", "T", "Ter", "Tom", "Ul", "V", "W", "Y",
};

static char *gnome_syllable2[] =
{
	"a", "aa",  "ai", "e", "ei", "i", "o", "uo", "u", "uu",
};

static char *gnome_syllable3[] =
{
	"ron", "re", "la", "ki", "kseli", "ksi", "ku", "ja", "ta", "na", "namari", "neli", "nika", "nikki", "nu", "nukka", "ka", "ko", "li", "kki", "rik", "po", "to", "pekka", "rjaana", "rjatta", "rjukka", "la", "lla", "lli", "mo", "nni",
};

/* Hobbit */
static char *hobbit_syllable1[] =
{
	"B", "Ber", "Br", "D", "Der", "Dr", "F", "Fr", "G", "H", "L", "Ler", "M", "Mer", "N", "P", "Pr", "Per", "R", "S", "T", "W",
};

static char *hobbit_syllable2[] =
{
	"a", "e", "i", "ia", "o", "oi", "u",
};

static char *hobbit_syllable3[] =
{
	"bo", "ck", "decan", "degar", "do", "doc", "go", "grin", "lba", "lbo", "lda", "ldo", "lla", "ll", "lo", "m", "mwise", "nac", "noc", "nwise", "p", "ppin", "pper", "tho", "to",
};

/* Human */
static char *human_syllable1[] =
{
	"Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar", "B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et", "Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha", "Ib", "Jer", "K", "Ka", "Ked", "L", "Loth", "Lar", "Leg", "M", "Mir", "N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T", "Tr", "Th", "V", "Y", "Z", "W", "Wic",
};

static char *human_syllable2[] =
{
	"a", "ae", "au", "ao", "are", "ale", "ali", "ay", "ardo", "e", "ei", "ea", "eri", "era", "ela", "eli", "enda", "erra", "i", "ia", "ie", "ire", "ira", "ila", "ili", "ira", "igo", "o", "oa", "oi", "oe", "ore", "u", "y",
};

static char *human_syllable3[] =
{
	"a", "and", "b", "bwyn", "baen", "bard", "c", "ctred", "cred", "ch", "can", "d", "dan", "don", "der", "dric", "dfrid", "dus", "f", "g", "gord", "gan", "l", "li", "lgrin", "lin", "lith", "lath", "loth", "ld", "ldric", "ldan", "m", "mas", "mos", "mar", "mond", "n", "nydd", "nidd", "nnon", "nwan", "nyth", "nad", "nn", "nnor", "nd", "p", "r", "ron", "rd", "s", "sh", "seth", "sean", "t", "th", "tha", "tlan", "trem", "tram", "v", "vudd", "w", "wan", "win", "wyn", "wyr", "wyr", "wyth",
};

/* Orc */
static char *orc_syllable1[] =
{
	"B", "Er", "G", "Gr", "H", "P", "Pr", "R", "V", "Vr", "T", "Tr", "M", "Dr",
};

static char *orc_syllable2[] =
{
	"a", "i", "o", "oo", "u", "ui",
};

static char *orc_syllable3[] =
{
	"dash", "dish", "dush", "gar", "gor", "gdush", "lo", "gdish", "k", "lg", "nak", "rag", "rbag", "rg", "rk", "ng", "nk", "rt", "ol", "urk", "shnak", "mog", "mak", "rak",
};


/*
 * Random Name Generator
 * based on a Javascript by Michael Hensley
 * "http://geocities.com/timessquare/castle/6274/"
 * Copied from Cth by DvE
 * Copied from borgband by APW
 */
static void create_random_name(int race, char *name)
{
	/* Paranoia */
	if (!name) return;

	/* Select the monster type */
	switch (race)
	{
		/* Create the monster name */
	case RACE_DWARF:
		strcpy(name, dwarf_syllable1[rand_int(sizeof(dwarf_syllable1) / sizeof(char*))]);
		strcat(name, dwarf_syllable2[rand_int(sizeof(dwarf_syllable2) / sizeof(char*))]);
		strcat(name, dwarf_syllable3[rand_int(sizeof(dwarf_syllable3) / sizeof(char*))]);
		break;
	case RACE_ELF:
	case RACE_HALF_ELF:
	case RACE_HIGH_ELF:
		strcpy(name, elf_syllable1[rand_int(sizeof(elf_syllable1) / sizeof(char*))]);
		strcat(name, elf_syllable2[rand_int(sizeof(elf_syllable2) / sizeof(char*))]);
		strcat(name, elf_syllable3[rand_int(sizeof(elf_syllable3) / sizeof(char*))]);
		break;
	case RACE_GNOME:
		strcpy(name, gnome_syllable1[rand_int(sizeof(gnome_syllable1) / sizeof(char*))]);
		strcat(name, gnome_syllable2[rand_int(sizeof(gnome_syllable2) / sizeof(char*))]);
		strcat(name, gnome_syllable3[rand_int(sizeof(gnome_syllable3) / sizeof(char*))]);
		break;
	case RACE_HOBBIT:
		strcpy(name, hobbit_syllable1[rand_int(sizeof(hobbit_syllable1) / sizeof(char*))]);
		strcat(name, hobbit_syllable2[rand_int(sizeof(hobbit_syllable2) / sizeof(char*))]);
		strcat(name, hobbit_syllable3[rand_int(sizeof(hobbit_syllable3) / sizeof(char*))]);
		break;
	case RACE_HUMAN:
	case RACE_DUNADAN:
		strcpy(name, human_syllable1[rand_int(sizeof(human_syllable1) / sizeof(char*))]);
		strcat(name, human_syllable2[rand_int(sizeof(human_syllable2) / sizeof(char*))]);
		strcat(name, human_syllable3[rand_int(sizeof(human_syllable3) / sizeof(char*))]);
		break;
	case RACE_HALF_ORC:
	case RACE_HALF_TROLL:
		strcpy(name, orc_syllable1[rand_int(sizeof(orc_syllable1) / sizeof(char*))]);
		strcat(name, orc_syllable2[rand_int(sizeof(orc_syllable2) / sizeof(char*))]);
		strcat(name, orc_syllable3[rand_int(sizeof(orc_syllable3) / sizeof(char*))]);
		break;
		/* Create an empty name */
	default:
		name[0] = '\0';
		break;
	}
}



static byte player_init_borg[MAX_CLASS][3][2] =
{
	{
		/* Warrior */
		{ TV_POTION, SV_POTION_BESERK_STRENGTH },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_HARD_ARMOR, SV_CHAIN_MAIL }
	},

	{
		/* Mage */
		{ TV_MAGIC_BOOK, 0 },
		{ TV_SWORD, SV_DAGGER },
		{ TV_SCROLL, SV_SCROLL_WORD_OF_RECALL }
	},

	{
		/* Priest */
		{ TV_PRAYER_BOOK, 0 },
		{ TV_HAFTED, SV_MACE },
		{ TV_POTION, SV_POTION_HEALING }
	},

	{
		/* Rogue */
		{ TV_MAGIC_BOOK, 0 },
		{ TV_SWORD, SV_SMALL_SWORD },
		{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
	},

	{
		/* Ranger */
		{ TV_MAGIC_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_BOW, SV_LONG_BOW }
	},

	{
		/* Paladin */
		{ TV_PRAYER_BOOK, 0 },
		{ TV_SWORD, SV_BROAD_SWORD },
		{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL }
	}
};

/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit_borg(void)
{
	int i, tv, sv;

	object_type *i_ptr;
	object_type object_type_body;


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some food */
	object_prep(i_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
	i_ptr->number = (byte)rand_range(3, 7);
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);


	/* Get local object */
	i_ptr = &object_type_body;

	/* Hack -- Give the player some torches */
	object_prep(i_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
	i_ptr->number = (byte)rand_range(3, 7);
	i_ptr->pval = rand_range(3, 7) * 500;
	object_aware(i_ptr);
	object_known(i_ptr);
	(void)inven_carry(i_ptr);

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
        tv = player_init_borg[p_ptr->pclass][i][0];
        sv = player_init_borg[p_ptr->pclass][i][1];

		/* Get local object */
		i_ptr = &object_type_body;

		/* Hack -- Give the player an object */
		object_prep(i_ptr, lookup_kind(tv, sv));
		object_aware(i_ptr);
		object_known(i_ptr);
		(void)inven_carry(i_ptr);
	}
}


/* Allow the borg to play continously.  Reset all values, */
static void resurrect_borg(void)
{
    int i,j;
    int n;

    /* Cheat death */
    p_ptr->is_dead = FALSE;

    /* flush the buffer */
    borg_parse(NULL);
    borg_flush();


    /* remove the spell counters */
    if (mb_ptr->spell_book)
    {
        for (i = 0; i < 9; i++ )
        {
           for (j = 0; j < 8; j++)
            {
                /* get the magics */
                auto_magic *as = &auto_magics[i][j];
                /* reset the counter */
                as->times = 0;
            }
        }
    }


    /*** Wipe the player ***/
    (void)WIPE(p_ptr, player_type);
    turn =0;

    /* Set some player flags to keep it running */
	p_ptr->playing = TRUE;
    p_ptr->leaving = TRUE;
    p_ptr->py = c_y;
    p_ptr->px = c_x;


	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artifacts made yet */
    for (i = 0; i < MAX_A_IDX; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}


	/* Start with no quests */
	for (i = 0; i < MAX_Q_IDX; i++)
	{
		q_list[i].level = 0;
	}

	/* Add a special quest */
	q_list[0].level = 99;

	/* Add a second quest */
	q_list[1].level = 100;


	/* Reset the "objects" */
    for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];


        /* skip some stuff */
        if (k_ptr->tval <= TV_AMULET ||
            k_ptr->tval >= TV_FOOD) continue;

        /* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}


	/* Reset the "monsters" */
    for (i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear player kills */
        r_ptr->r_pkills = 0;
	}


	/* Hack -- no ghosts */
    r_info[MAX_R_IDX-1].max_num = 0;


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* None of the spells have been learned yet */
	for (i = 0; i < 64; i++) p_ptr->spell_order[i] = 99;

    /** Roll up a new character **/

    /* Reset max stuff */
    auto_max_depth = 0;
    auto_max_level = 0;

    /* Sex */
    p_ptr->psex = rand_int(MAX_SEXES);
    sp_ptr = &sex_info[p_ptr->psex];

    /* Race */
    if (auto_respawn_race == -1)
    {
        p_ptr->prace = rand_int(MAX_RACES);
    }
    else
    {
        p_ptr->prace = auto_respawn_race;
    }
    rp_ptr = &race_info[p_ptr->prace];

   if (auto_respawn_class == -1)
   {
    while (1)
    {
       /* Class */
       p_ptr->pclass = rand_int(MAX_CLASS);

       /* Try again if not a legal choice */
       if (!(rp_ptr->choice & (1L << p_ptr->pclass))) continue;

       cp_ptr = &class_info[p_ptr->pclass];
       mp_ptr = &magic_info[p_ptr->pclass];

       break;
    }
   }
   else
   {
        p_ptr->pclass = auto_respawn_class;
        cp_ptr = &class_info[p_ptr->pclass];
        mp_ptr = &magic_info[p_ptr->pclass];
    }

    /* Some Extra things */
    get_stats_borg();
    get_extra_borg();
    get_ahw_borg();
    get_history_borg();
    get_money_borg();

	/* Get a random name */
	create_random_name(p_ptr->prace,op_ptr->full_name);


    /* outfit the player */
    (void)player_outfit_borg();

    /* Reset the Shops */
    for (n = 0; n < MAX_STORES; n++)
    {
		/* Initialize */
		store_init(n);

		/* Maintain the shop (ten times) */
		for (i = 0; i < 10; i++) store_maint(n);
    }

    /* reinit the borg */
    /* Hack -- flush it */
    Term_fresh();

    /*** Hack -- Extract race ***/

    /* Insert the player Race--cheat */
    auto_race = p_ptr->prace;

    /* Extract the race pointer */
    rb_ptr = &race_info[auto_race];


    /*** Hack -- Extract class ***/

    /* Cheat the class */
    auto_class = p_ptr->pclass;

    /* Extract the class pointer */
    cb_ptr = &class_info[auto_class];

    /* Extract the magic pointer */
    mb_ptr = &magic_info[auto_class];


    /*** Hack -- react to race and class ***/

    /* Notice the new race and class */
    prepare_race_class_info();


    /* need to check all stats */
    my_need_stat_check[0] = TRUE;
    my_need_stat_check[1] = TRUE;
    my_need_stat_check[2] = TRUE;
    my_need_stat_check[3] = TRUE;
    my_need_stat_check[4] = TRUE;
    my_need_stat_check[5] = TRUE;

    /* Allowable Cheat -- Obtain "recall" flag */
    goal_recalling = (p_ptr->word_recall ? TRUE : FALSE);

    /* Allowable Cheat -- Obtain "prot_from_evil" flag */
    borg_prot_from_evil = (p_ptr->protevil ? TRUE : FALSE);
    /* Allowable Cheat -- Obtain "speed" flag */
    borg_speed = (p_ptr->fast ? TRUE : FALSE);
    /* Allowable Cheat -- Obtain "goi" flag */
    borg_goi = (p_ptr->invuln ? 9 : 0);
    /* Allowable Cheat -- Obtain "resist" flags */
    my_oppose_acid = (p_ptr->oppose_acid ? TRUE : FALSE);
    my_oppose_elec = (p_ptr->oppose_elec ? TRUE : FALSE);
    my_oppose_fire = (p_ptr->oppose_fire ? TRUE : FALSE);
    my_oppose_cold = (p_ptr->oppose_cold ? TRUE : FALSE);
    my_oppose_pois = (p_ptr->oppose_pois ? TRUE : FALSE);
    borg_bless = (p_ptr->blessed ? TRUE : FALSE);
    borg_shield = (p_ptr->shield ? TRUE : FALSE);
    borg_hero = (p_ptr->hero ? TRUE : FALSE);
    borg_berserk = (p_ptr->shero ? TRUE : FALSE);

    /* Message */
    borg_note("# Respawning");
    borg_respawn = 5;

    /* Done.  Play on */
    cheat_live = TRUE;


}


/* Keep a log of borg deaths in the /user directory.  RR9 */
void borg_log_death(void)
{
   char buf[1024];
    FILE *borg_log_file;
   time_t death_time;


   /* Build the filename */
   path_build(buf, 1024, ANGBAND_DIR_USER, "borg-log.txt");

   /* Hack -- drop permissions */
   safe_setuid_drop();

   /* Append to the file */
   borg_log_file = my_fopen(buf, "a");

    /* Hack -- grab permissions */
   safe_setuid_grab();

   /* Failure */
   if (!borg_log_file) return;

   /* Get time of death */
   (void)time(&death_time);

   /* Save the date */
   strftime(buf, 80, "%Y/%m/%d %H:%M\n", localtime(&death_time));

   fprintf(borg_log_file, buf);

   fprintf(borg_log_file, "%s the %s %s, Level %d/%d\n", op_ptr->full_name,
           race_info[p_ptr->prace].title,
           class_info[p_ptr->pclass].title,
           p_ptr->lev, p_ptr->max_lev);

   fprintf(borg_log_file, "Exp: %lu  Gold: %lu  Turn: %lu\n", (long)total_points(), (long)p_ptr->au, (long)turn);

   fprintf(borg_log_file, "Killed on level: %d (max. %d) by %s\n", p_ptr->depth, p_ptr->max_depth, p_ptr->died_from);

   fprintf(borg_log_file, "----------\n\n");
   fprintf(borg_log_file, "Borg engine date %s\n\n", borg_engine_date);

   my_fclose(borg_log_file);
}



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

bool borg_prompt;  /* ajg  For now we can just use this locally.
                           in the 283 borg he uses this to optimize knowing if
                           we are waiting at a prompt for info */

    /* Refresh the screen */
    Term_fresh();

	/* Deactivate */
	if (!auto_active)
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
	    streq(buf, "Die?") &&
        borg_cheat_death)
	{
		/* Flush messages */
		borg_parse(NULL);

		/* Take note */
		borg_note("# Cheating death...");

        /* Dump the Character Map*/
        if (auto_level >= auto_dump_level) borg_write_map(FALSE);


        /* log the death of the borg */
        borg_log_death();

        /* Reset the player game data then resurrect a new player */
        resurrect_borg();

		/* Cheat death */
		return ('n');
	}


	/* Mega-Hack -- Handle death */
    if (p_ptr->is_dead && !cheat_live)
	{

        /* Print the map */
        if (auto_level > auto_dump_level ) borg_write_map(FALSE);

        /* Log the death */
        borg_log_death();

        /* continual play mode */
        if (borg_cheat_death)
        {
            /* respawn */
            resurrect_borg();

            /* Clear the message */
            return (KTRL('M'));
        }
        else
        {
            /* Oops */
            borg_oops("player died");


            /* Useless keypress */
            return (KTRL('C'));
        }
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
	auto_rand_quick = Rand_quick;
	auto_rand_value = Rand_value;

	/* Use the local random info */
	Rand_quick = TRUE;
	Rand_value = auto_rand_local;


	/* Think */
    while (!borg_think()) /* loop */;

	/* Save the local random info */
	auto_rand_local = Rand_value;

	/* Restore the system random info */
	Rand_quick = auto_rand_quick;
	Rand_value = auto_rand_value;

    /* Hack -- allow stepping to induce a clean cancel */
    if (auto_step && (!--auto_step)) auto_cancel = TRUE;


	/* Check for key */
	ch = borg_inkey(TRUE);

	/* Use the key */
	if (ch) return (ch);


	/* Oops */
	borg_oops("normal abort");

	/* Hack -- Escape */
	return (ESCAPE);
}

#ifdef ALLOW_BORG_GRAPHICS

glyph translate_visuals[255][255];

/*
 * Return the "attr" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_kind_attr(T) \
   (((T)->flavor) ? \
    (misc_to_attr[(T)->flavor]) : \
    ((T)->x_attr))

/*
 * Return the "char" for a given item.
 * Use "flavor" if available.
 * Default to user definitions.
 */
#define object_kind_char(T) \
   (((T)->flavor) ? \
    (misc_to_char[(T)->flavor]) : \
    ((T)->x_char))


void init_translate_visuals(void)
{
   int i,j;


   /* Extract default attr/char code for features */
   for (i = 0; i < MAX_F_IDX; i++)
   {
       feature_type *f_ptr = &f_info[i];

       if (!f_ptr->name) continue;

       /* Store the underlying values */
       translate_visuals[(byte)f_ptr->x_attr][(byte)f_ptr->x_char].d_attr = f_ptr->d_attr;
       translate_visuals[(byte)f_ptr->x_attr][(byte)f_ptr->x_char].d_char = f_ptr->d_char;

       /* Boring grids (floors, etc) */
           if (view_special_lite && (f_ptr->x_attr == TERM_WHITE) && (i <= FEAT_INVIS))
           {
               translate_visuals[TERM_YELLOW][(byte)f_ptr->x_char].d_attr = f_ptr->d_attr;
               translate_visuals[TERM_YELLOW][(byte)f_ptr->x_char].d_char = f_ptr->d_char;

               translate_visuals[TERM_L_DARK][(byte)f_ptr->x_char].d_attr = f_ptr->d_attr;
               translate_visuals[TERM_L_DARK][(byte)f_ptr->x_char].d_char = f_ptr->d_char;

               translate_visuals[TERM_SLATE][(byte)f_ptr->x_char].d_attr = f_ptr->d_attr;
               translate_visuals[TERM_SLATE][(byte)f_ptr->x_char].d_char = f_ptr->d_char;
           }
           else if (view_granite_lite && (f_ptr->x_attr == TERM_WHITE) && (i >= FEAT_SECRET))
           {
               translate_visuals[TERM_L_DARK][(byte)f_ptr->x_char].d_attr = f_ptr->d_attr;
               translate_visuals[TERM_L_DARK][(byte)f_ptr->x_char].d_char = f_ptr->d_char;

               translate_visuals[TERM_SLATE][(byte)f_ptr->x_char].d_attr = f_ptr->d_attr;
               translate_visuals[TERM_SLATE][(byte)f_ptr->x_char].d_char = f_ptr->d_char;
           }

   }

   /* Extract default attr/char code for objects */
   for (i = 0; i < MAX_K_IDX; i++)
   {
       object_kind *k_ptr = &k_info[i];

       if (!k_ptr->name) continue;

       /* Store the underlying values */
       translate_visuals[(byte)object_kind_attr(k_ptr)][(byte)object_kind_char(k_ptr)].d_attr = k_ptr->d_attr;
       translate_visuals[(byte)object_kind_attr(k_ptr)][(byte)object_kind_char(k_ptr)].d_char = k_ptr->d_char;
   }

   /* Extract default attr/char code for monsters */
   for (i = 0; i < MAX_R_IDX; i++)
   {
       monster_race *r_ptr = &r_info[i];

       if (!r_ptr->name) continue;

       /* Store the underlying values */
       translate_visuals[(byte)r_ptr->x_attr][(byte)r_ptr->x_char].d_attr = r_ptr->d_attr;
       translate_visuals[(byte)r_ptr->x_attr][(byte)r_ptr->x_char].d_char = r_ptr->d_char;

       /* Multi-hued monster in ASCII mode */
       if ((r_ptr->flags1 & (RF1_ATTR_MULTI)) &&
           !((r_ptr->x_attr & 0x80) && (r_ptr->x_char & 0x80)))
       {
           for (j = 0; j < 16; j++)
           {
               translate_visuals[j][(byte)r_ptr->x_char].d_attr = j;
               translate_visuals[j][(byte)r_ptr->x_char].d_char = r_ptr->d_char;
           }
       }
   }
}

#endif /* ALLOW_BORG_GRAPHICS */


/*
 * Initialize the Borg
 */
void borg_init_9(void)
{
    int i;

    byte t_a;

    char buf[80];

    byte *test;

    /*** Hack -- verify system ***/

    /* Message */
    prt("Initializing the Borg... (memory)", 0, 0);

    /* Hack -- flush it */
    Term_fresh();

    /* Mega-Hack -- verify memory */
    C_MAKE(test, 400 * 1024L, byte);
    C_KILL(test, 400 * 1024L, byte);


    /*** Hack -- initialize game options ***/

    /* Message */
    prt("Initializing the Borg... (options)", 0, 0);

    /* Hack -- flush it */
    Term_fresh();

    /* We use the original keypress codes */
    rogue_like_commands = FALSE;

    /* We pick up items when we step on them */
    always_pickup = TRUE;

    /* We specify targets by hand */
    use_old_target = FALSE;

	/* We must use the top object in stacks */
	floor_query_flag = FALSE;

	/* We must pick items up without verification */
	carry_query_flag = FALSE;

    /* We repeat by hand */
    always_repeat = FALSE;

    /* We do not haggle */
    auto_haggle = TRUE;

    /*  dont start out auto-scumming. */
    auto_scum = FALSE;

    /* We need space */
    show_labels = FALSE;
    show_weights = FALSE;
    show_flavors = FALSE;

    /* We need the dungeon level */
    depth_in_feet = FALSE;

    /* Allow items to stack */
    stack_force_notes = TRUE;
    stack_force_costs = TRUE;


    /* Ignore discounts */
    stack_force_costs = TRUE;

    /* Ignore inscriptions */
    stack_force_notes = TRUE;

    /* Efficiency */
    avoid_abort = TRUE;

    /* Efficiency */
    alert_hitpoint = 0;

    /* Hack -- notice "command" mode */
    hilite_player = FALSE;


#if 0
    if (!borg_graphics)
    {
        /* Reset the # and % -- Scan the features */
        for (i = 1; i < MAX_F_IDX; i++)
        {
            feature_type *f_ptr = &f_info[i];

            /* Skip non-features */
            if (!f_ptr->name) continue;

            /* Switch off "graphics" */
            f_ptr->x_attr = f_ptr->d_attr;
            f_ptr->x_char = f_ptr->d_char;
        }
    }
#endif

#ifdef ALLOW_BORG_GRAPHICS

   init_translate_visuals();

#endif /* ALLOW_BORG_GRAPHICS */

    /*** Redraw ***/
    /* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD);

	/* Redraw everything */
    do_cmd_redraw();
    /*** Various ***/

    /* Message */
    prt("Initializing the Borg... (various)", 0, 0);

    /* Hack -- flush it */
    Term_fresh();


    /*** Cheat / Panic ***/

    /* Mega-Hack -- Cheat a lot */
    auto_cheat_inven = TRUE;
    auto_cheat_equip = TRUE;

    /* Mega-Hack -- Cheat a lot */
    auto_cheat_spell = TRUE;

    /* Mega-Hack -- Cheat a lot */
    auto_cheat_panel = TRUE;

    /* more cheating */
    borg_cheat_death = FALSE;

    /* set the continous play mode if the game cheat death is on */
    if (cheat_live) borg_cheat_death = TRUE;

    /*** Initialize ***/

    /* Initialize */
    borg_init_1();
    borg_init_2();
    borg_init_3();
    borg_init_4();
    borg_init_5();
    borg_init_6();
    borg_init_7();
    borg_init_8();


    /*** Hack -- Extract race ***/

    /* Check for textual race */
    if (0 == borg_what_text(COL_RACE, ROW_RACE, -12, &t_a, buf))
    {
        /* Scan the races */
        for (i = 0; i < MAX_RACES; i++)
        {
            /* Check the race */
            if (prefix(buf, race_info[i].title))
            {
                /* We got one */
                auto_race = i;
                break;
            }
        }
        /* Dragons have long race names and they are not read correctly */
        if (auto_race == 0)
        {   /* Just cheat the race if dragon (or human) */
            auto_race = p_ptr->prace;
        }

    }

    /* Extract the race pointer */
    rb_ptr = &race_info[auto_race];


    /*** Hack -- Extract class ***/

    /* Check for textual class */
    if (0 == borg_what_text(COL_CLASS, ROW_CLASS, -12, &t_a, buf))
    {
        /* Scan the classes */
        for (i = 0; i < MAX_CLASS; i++)
        {
            /* Check the race */
            if (prefix(buf, class_info[i].title))
            {
                /* We got one */
                auto_class = i;
                break;
            }
        }
    }

    /* Extract the class pointer */
    cb_ptr = &class_info[auto_class];

    /* Extract the magic pointer */
    mb_ptr = &magic_info[auto_class];


    /*** Hack -- react to race and class ***/

    /* Notice the new race and class */
    prepare_race_class_info();


    /*** All done ***/

    /* Done initialization */
    prt("Initializing the Borg... done.", 0, 0);

    /* Clear line */
    prt("", 0, 0);

    /* Official message */
    borg_note("# Ready...");

    /* Now it is ready */
    initialized = TRUE;
}

/*
 * Write a file with the current dungeon info (Borg)
 * and his equipment, inventory and home (Player)
 * and his swap armor, weapon (Borg)
 * From Dennis Van Es
 */
void borg_write_map(bool ask)
{

char buf[1024];
	char filename[40];
	char *c_ptr;

    FILE *borg_map_file;
    char line[DUNGEON_WID + 1];


	auto_item *item;

	int i,j;
    s16b m_idx;
    monster_type *m_ptr;

	store_type *st_ptr = &store[7];

    int k, z, x, y, n;
	bool *okay;
    u16b why =2;
    u16b *who;
    char o_name[80];

    /* Allocate the "okay" array */
    C_MAKE(okay, MAX_A_IDX, bool);

	/* Hack -- drop permissions */
	safe_setuid_drop();

	/* Process the player name */
    sprintf(filename,"%s.map",op_ptr->full_name);

	c_ptr = filename;

	while (*c_ptr)
	{

		/* No control characters */
		if (iscntrl(*c_ptr))
		{
			/* Illegal characters */
			quit_fmt("Illegal control char (0x%02X) in player name", *c_ptr);
		}

		/* Convert all non-alphanumeric symbols */
		if (!isalpha(*c_ptr) && !isdigit(*c_ptr) && (*c_ptr != '.')) *c_ptr = '_';

		c_ptr++;

    }

	/* Build a path */
    path_build(buf, 1024, ANGBAND_DIR_SAVE, filename);

	/* Build the file */
    borg_map_file = my_fopen(buf, "w");

    /* Hack -- grab permissions */
	safe_setuid_grab();

   fprintf(borg_map_file, "%s the %s %s, Level %d/%d\n", op_ptr->full_name,
           race_info[p_ptr->prace].title,
           class_info[p_ptr->pclass].title,
           p_ptr->lev, p_ptr->max_lev);

   fprintf(borg_map_file, "Exp: %lu  Gold: %lu  Turn: %lu\n", (long)total_points(), (long)p_ptr->au, (long)turn);
   fprintf(borg_map_file, "Killed on level: %d (max. %d) by %s\n\n", p_ptr->depth, p_ptr->max_depth, p_ptr->died_from);
   fprintf(borg_map_file, "Borg engine date %s\n\n", borg_engine_date);

	for (i = 0; i < DUNGEON_HGT; i++)
	{
		for (j = 0; j < DUNGEON_WID; j++)
		{
			char ch;

            auto_grid *ag= &auto_grids[i][j];
            m_idx = cave_m_idx[i][j];


            /* reset the ch each time through */
            ch = ' ';

            /* Known grids */
            if (ag->feat)
			{
                ch = f_info[ag->feat].d_char;
			}

            /* Known Items */
            if (ag->take)
			{
                auto_take *take = &auto_takes[ag->take];
				object_kind *k_ptr = &k_info[take->k_idx];
				ch = k_ptr->d_char;
			}

            /* UnKnown Monsters */
            if (m_idx)
			{
                ch = '&';
			}

            /* Known Monsters */
            if (ag->kill)
			{
                auto_kill *kill = &auto_kills[ag->kill];
				monster_race *r_ptr = &r_info[kill->r_idx];
				ch = r_ptr->d_char;
			}


            /* The Player */
            if ((i == c_y) && (j == c_x)) ch = '@';

            line[j] = ch;
		}
		fprintf(borg_map_file, "%s\n", line);
	}


	/* Known/Seen monsters */
    for (i = 1; i < auto_kills_nxt; i++)
	{
        auto_kill *kill = &auto_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Note */
		fprintf(borg_map_file,"monster '%s' (%d) at (%d,%d)\n",
		                 (r_name + r_info[kill->r_idx].name), kill->r_idx,
		                 kill->y, kill->x);
	}

    /*** Dump the last few messages ***/
    i = message_num();
    if (i > 50 && auto_depth !=100) i = 50;
    fprintf(borg_map_file, "\n\n  [Last Messages]\n\n");
    while (i-- >0)
    {
        fprintf(borg_map_file, "%s\n", message_str((s16b)i));
    }

    /*** Player Equipment ***/
	fprintf(borg_map_file, "\n\n  [Character Equipment]\n\n");
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		object_desc(o_name, &inventory[i], TRUE, 3);
		fprintf(borg_map_file, "%c) %s\n",
			    index_to_label(i), o_name);
	}

    fprintf(borg_map_file, "\n\n");


	/* Dump the inventory */
	fprintf(borg_map_file, "  [Character Inventory]\n\n");
	for (i = 0; i < INVEN_PACK; i++)
	{
		object_desc(o_name, &inventory[i], TRUE, 3);
		fprintf(borg_map_file, "%c) %s\n",
		        index_to_label(i), o_name);
	}
	fprintf(borg_map_file, "\n\n");


	/* Dump the Home (page 1) */
	fprintf(borg_map_file, "  [Home Inventory (page 1)]\n\n");
	for (i = 0; i < 12; i++)
	{
		object_desc(o_name, &st_ptr->stock[i], TRUE, 3);
		fprintf(borg_map_file, "%c) %s\n", I2A(i%12), o_name);
	}
	fprintf(borg_map_file, "\n\n");

	/* Dump the Home (page 2) */
	fprintf(borg_map_file, "  [Home Inventory (page 2)]\n\n");
	for (i = 12; i < 24; i++)
	{
		object_desc(o_name, &st_ptr->stock[i], TRUE, 3);
		fprintf(borg_map_file, "%c) %s\n", I2A(i%12), o_name);
	}
	fprintf(borg_map_file, "\n\n");

	/* Write swap info */
	fprintf(borg_map_file, "  [Swap info]\n\n");
    item = &auto_items[weapon_swap];
    fprintf(borg_map_file,"Swap Weapon:  %s\n", item->desc);
    item = &auto_items[armour_swap];
    fprintf(borg_map_file,"Swap Armour:  %s", item->desc);
    fprintf(borg_map_file, "\n\n");

    fprintf(borg_map_file, "   [Player State at Death] \n\n");
    /* Dump the player state */
	if (p_ptr->blind)
	{
        fprintf(borg_map_file,  "You cannot see.\n");
	}
	if (p_ptr->confused)
	{
        fprintf(borg_map_file,  "You are confused.\n");
	}
	if (p_ptr->afraid)
	{
        fprintf(borg_map_file,  "You are terrified.\n");
	}
	if (p_ptr->cut)
	{
        fprintf(borg_map_file,  "You are bleeding.\n");
	}
	if (p_ptr->stun)
	{
        fprintf(borg_map_file,  "You are stunned.\n");
	}
	if (p_ptr->poisoned)
	{
        fprintf(borg_map_file,  "You are poisoned.\n");
	}
	if (p_ptr->image)
	{
        fprintf(borg_map_file,  "You are hallucinating.\n");
	}

	if (p_ptr->aggravate)
	{
        fprintf(borg_map_file,  "You aggravate monsters.\n");
	}
	if (p_ptr->blessed)
	{
        fprintf(borg_map_file,  "You feel rightous.\n");
	}
	if (p_ptr->hero)
	{
        fprintf(borg_map_file,  "You feel heroic.\n");
	}
	if (p_ptr->shero)
	{
        fprintf(borg_map_file,  "You are in a battle rage.\n");
	}
	if (p_ptr->protevil)
	{
        fprintf(borg_map_file,  "You are protected from evil.\n");
	}
	if (p_ptr->shield)
	{
        fprintf(borg_map_file,  "You are protected by a mystic shield.\n");
	}
	if (p_ptr->invuln)
	{
        fprintf(borg_map_file,  "You are temporarily invulnerable.\n");
	}
    if (p_ptr->ethereal_lock)
	{
        fprintf(borg_map_file,  "The heavens are sealed.\n");
	}
	if (p_ptr->confusing)
	{
        fprintf(borg_map_file,  "Your hands are glowing dull red.\n");
	}
	if (p_ptr->word_recall)
	{
        fprintf(borg_map_file,  "You will soon be recalled. (in %d turns)\n", p_ptr->word_recall);
	}
    fprintf(borg_map_file, "\n\n");

    /*** Dump the spells ***/

    if (mb_ptr->spell_book)
	{
		fprintf(borg_map_file,"\n\n   [ Spells ] \n\n");
		fprintf(borg_map_file,"Name                           Legal Times cast\n");
		for (i = 0; i < 9; i++ )
		{
			for (j = 0; j < 8; j++)
			{
                auto_magic *as = &auto_magics[i][j];
				cptr legal;

				if (as->level <99)
				{
					if (mb_ptr->spell_book == TV_PRAYER_BOOK)
					{
						legal = (borg_prayer_legal(i, j) ? "Yes" : "No ");
					}
					else
					{
						legal = (borg_spell_legal(i, j) ? "Yes" : "No ");
					}
					fprintf(borg_map_file,"%-30s   %s   %d\n",as->name, legal, as->times);
				}
			}
			fprintf(borg_map_file,"\n");
		}
	}


    /*** Dump the Uniques and Artifact Lists ***/

	/* Scan the artifacts */
    for (k = 0; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* Default */
		okay[k] = FALSE;

		/* Skip "empty" artifacts */
		if (!a_ptr->name) continue;

		/* Skip "uncreated" artifacts */
		if (!a_ptr->cur_num) continue;

		/* Assume okay */
		okay[k] = TRUE;
	}

	/* Check the dungeon */
	for (y = 0; y < DUNGEON_HGT; y++)
	{
		for (x = 0; x < DUNGEON_WID; x++)
		{
			s16b this_o_idx, next_o_idx = 0;

			/* Scan all objects in the grid */
			for (this_o_idx = cave_o_idx[y][x]; this_o_idx; this_o_idx = next_o_idx)
			{
				object_type *o_ptr;

				/* Get the object */
				o_ptr = &o_list[this_o_idx];

				/* Get the next object */
				next_o_idx = o_ptr->next_o_idx;

				/* Ignore non-artifacts */
				if (!artifact_p(o_ptr)) continue;

				/* Ignore known items */
				if (object_known_p(o_ptr)) continue;

				/* Note the artifact */
				okay[o_ptr->name1] = FALSE;
			}
		}
	}

	/* Check the inventory and equipment */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_type *o_ptr = &inventory[i];

		/* Ignore non-objects */
		if (!o_ptr->k_idx) continue;

		/* Ignore non-artifacts */
		if (!artifact_p(o_ptr)) continue;

		/* Ignore known items */
		if (object_known_p(o_ptr)) continue;

		/* Note the artifact */
		okay[o_ptr->name1] = FALSE;
	}

    fprintf(borg_map_file, "\n\n");


    /* Hack -- Build the artifact name */
    fprintf(borg_map_file, "   [Artifact Info] \n\n");

	/* Scan the artifacts */
    for (k = 0; k < MAX_A_IDX; k++)
	{
		artifact_type *a_ptr = &a_info[k];

		/* List "dead" ones */
		if (!okay[k]) continue;

		/* Paranoia */
		strcpy(o_name, "Unknown Artifact");

		/* Obtain the base object type */
		z = lookup_kind(a_ptr->tval, a_ptr->sval);

		/* Real object */
		if (z)
		{
			object_type *i_ptr;
			object_type object_type_body;

			/* Get local object */
			i_ptr = &object_type_body;

			/* Create fake object */
			object_prep(i_ptr, z);

			/* Make it an artifact */
			i_ptr->name1 = k;

			/* Describe the artifact */
			object_desc_store(o_name, i_ptr, FALSE, 0);
		}

		/* Hack -- Build the artifact name */
        fprintf(borg_map_file, "The %s\n", o_name);
	}
    fprintf(borg_map_file, "\n\n");

	/* Free the "okay" array */
    C_KILL(okay, MAX_A_IDX, bool);

 /* Display known uniques
  *
  * Note that the player ghosts are ignored.  XXX XXX XXX
  */
	/* Allocate the "who" array */
    C_MAKE(who, MAX_R_IDX, u16b);

	/* Collect matching monsters */
    for (i = 1, n = 0; i < MAX_R_IDX-1; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Require unique monsters */
		if (!(r_ptr->flags1 & (RF1_UNIQUE))) continue;

		/* Collect "appropriate" monsters */
		who[n++] = i;
	}

#if 0
    /* Select the sort method */
	ang_sort_comp = ang_sort_comp_hook;
	ang_sort_swap = ang_sort_swap_hook;

	/* Sort the array by dungeon depth of monsters */
	ang_sort(who, &why, n);
#endif

    /* Hack -- Build the artifact name */
    fprintf(borg_map_file, "   [Unique Info] \n\n");

	/* Print the monsters */
	for (i = 0; i < n; i++)
	{
		monster_race *r_ptr = &r_info[who[i]];
		bool dead = (r_ptr->max_num == 0);

		/* Print a message */
        fprintf(borg_map_file, "     %s is %s\n",
			    (r_name + r_ptr->name),
			    (dead ? "dead" : "alive"));
	}

	/* Free the "who" array */
    C_KILL(who, MAX_R_IDX, u16b);


	my_fclose(borg_map_file);

}

/*
 * Hack -- forward declare
 */
void do_cmd_borg(void);


/*
 * Hack -- interact with the "Ben Borg".
 */
void do_cmd_borg(void)
{
    char cmd;


    /* Get a "Borg command", or abort */
    if (!get_com("Borg command: ", &cmd)) return;


    /* Simple help */
    if (cmd == '?')
    {
        int i = 2;

        /* Save the screen */
        Term_save();

        /* Clear the screen */
        Term_clear();

        /* Dump commands */
        i++;
        Term_putstr(2, i, -1, TERM_WHITE, "Command 'z' activates the Borg.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'u' updates the Borg.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command 'x' steps the Borg.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'f' modifies the normal flags.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command 'c' modifies the cheat flags.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'l' activates a log file.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command 's' activates search mode.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'i' displays grid info.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command 'g' displays grid feature.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'a' displays avoidances.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command 'k' displays monster info.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 't' displays object info.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command '%' displays current flow.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command '#' displays danger grid.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command '_' Depth info.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'p' Borg Power.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command '1' change max depth.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command '2' level prep info.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command '3' Feature of grid.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command '!' Time.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command '@' Borg LOS.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'w' My Swap Weapon.");
        Term_putstr(2, i++, -1, TERM_WHITE, "Command 'q' Auto stop on level.");
        Term_putstr(42, i, -1, TERM_WHITE, "Command 'v' Version stamp.");


		/* Prompt for key */
        msg_print("Commands: z,u,x,f,c,w,l,s,i,a,k,t,%,#,_,p,1,2,3,!,@.");
        msg_print(NULL);

        /* Restore the screen */
        Term_load();

        /* Done */
        return;
    }


    /* Hack -- force initialization */
    if (!initialized) borg_init_9();

    switch (cmd)
    {
        /* Command: Nothing */
        case '$':
        {
            borg_write_map(FALSE);
            break;
        }
        /* Command: Activate */
        case 'z':
        case 'Z':
        {
            /* Activate */
            auto_active = TRUE;

            /* Reset cancel */
            auto_cancel = FALSE;

            /* Step forever */
            auto_step = 0;

            /* need to check all stats */
            my_need_stat_check[0] = TRUE;
            my_need_stat_check[1] = TRUE;
            my_need_stat_check[2] = TRUE;
            my_need_stat_check[3] = TRUE;
            my_need_stat_check[4] = TRUE;
            my_need_stat_check[5] = TRUE;

            /* Allowable Cheat -- Obtain "recall" flag */
            goal_recalling = (p_ptr->word_recall ? TRUE : FALSE);

            /* Allowable Cheat -- Obtain "prot_from_evil" flag */
            borg_prot_from_evil = (p_ptr->protevil ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "speed" flag */
            borg_speed = (p_ptr->fast ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "goi" flag */
            borg_goi = (p_ptr->invuln ? 9 : 0);
            /* Allowable Cheat -- Obtain "resist" flags */
            my_oppose_acid = (p_ptr->oppose_acid ? TRUE : FALSE);
            my_oppose_elec = (p_ptr->oppose_elec ? TRUE : FALSE);
            my_oppose_fire = (p_ptr->oppose_fire ? TRUE : FALSE);
            my_oppose_cold = (p_ptr->oppose_cold ? TRUE : FALSE);
            my_oppose_pois = (p_ptr->oppose_pois ? TRUE : FALSE);
            borg_bless = (p_ptr->blessed ? TRUE : FALSE);
            borg_shield = (p_ptr->shield ? TRUE : FALSE);
            borg_hero = (p_ptr->hero ? TRUE : FALSE);
            borg_berserk = (p_ptr->shero ? TRUE : FALSE);
            do_nasty_pois = (p_ptr->nasty_pois ? TRUE: FALSE);


            /* Message */
            borg_note("# Installing keypress hook");

            /* Activate the key stealer */
            inkey_hack = borg_inkey_hack;
            break;
        }

        /* Command: Update */
        case 'u':
        case 'U':
        {
            /* Activate */
            auto_active = TRUE;

            /* Immediate cancel */
            auto_cancel = TRUE;

            /* Step forever */
            auto_step = 0;

            /* Allowable Cheat -- Obtain "recall" flag */
            goal_recalling = (p_ptr->word_recall ? TRUE : FALSE);

            /* Allowable Cheat -- Obtain "prot_from_evil" flag */
            borg_prot_from_evil = (p_ptr->protevil ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "speed" flag */
            borg_speed = (p_ptr->fast ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "goi" flag */
            borg_goi = (p_ptr->invuln ? 9 : 0);
            /* Allowable Cheat -- Obtain "resist" flags */
            my_oppose_acid = (p_ptr->oppose_acid ? TRUE : FALSE);
            my_oppose_elec = (p_ptr->oppose_elec ? TRUE : FALSE);
            my_oppose_fire = (p_ptr->oppose_fire ? TRUE : FALSE);
            my_oppose_cold = (p_ptr->oppose_cold ? TRUE : FALSE);
            my_oppose_pois = (p_ptr->oppose_pois ? TRUE : FALSE);
            borg_bless = (p_ptr->blessed ? TRUE : FALSE);
            borg_shield = (p_ptr->shield ? TRUE : FALSE);
            borg_hero = (p_ptr->hero ? TRUE : FALSE);
            borg_berserk = (p_ptr->shero ? TRUE : FALSE);
            do_nasty_pois = (p_ptr->nasty_pois ? TRUE: FALSE);

            /* Message */
            borg_note("# Installing keypress hook");

            /* Activate the key stealer */
            inkey_hack = borg_inkey_hack;

            break;
        }


        /* Command: Step */
        case 'x':
        case 'X':
        {
            /* Activate */
            auto_active = TRUE;

            /* Reset cancel */
            auto_cancel = FALSE;

		    /* Step N times */
            auto_step = 1;

            /* need to check all stats */
            my_need_stat_check[0] = TRUE;
            my_need_stat_check[1] = TRUE;
            my_need_stat_check[2] = TRUE;
            my_need_stat_check[3] = TRUE;
            my_need_stat_check[4] = TRUE;
            my_need_stat_check[5] = TRUE;

            /* Allowable Cheat -- Obtain "recall" flag */
            goal_recalling = (p_ptr->word_recall ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "prot_from_evil" flag */
            borg_prot_from_evil = (p_ptr->protevil ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "speed" flag */
            borg_speed = (p_ptr->fast ? TRUE : FALSE);
            /* Allowable Cheat -- Obtain "goi" flag */
            borg_goi = (p_ptr->invuln ? 9 : 0);
            /* Allowable Cheat -- Obtain "resist" flags */
            my_oppose_acid = (p_ptr->oppose_acid ? TRUE : FALSE);
            my_oppose_elec = (p_ptr->oppose_elec ? TRUE : FALSE);
            my_oppose_fire = (p_ptr->oppose_fire ? TRUE : FALSE);
            my_oppose_cold = (p_ptr->oppose_cold ? TRUE : FALSE);
            my_oppose_pois = (p_ptr->oppose_pois ? TRUE : FALSE);
            borg_bless = (p_ptr->blessed ? TRUE : FALSE);
            borg_shield = (p_ptr->shield ? TRUE : FALSE);
            borg_hero = (p_ptr->hero ? TRUE : FALSE);
            borg_berserk = (p_ptr->shero ? TRUE : FALSE);
            do_nasty_pois = (p_ptr->nasty_pois ? TRUE: FALSE);

            /* Message */
            borg_note("# Installing keypress hook");

            /* Activate the key stealer */
            inkey_hack = borg_inkey_hack;

            break;
        }

        /* Command: toggle "flags" */
        case 'f':
        case 'F':
        {
            /* Get a "Borg command", or abort */
            if (!get_com("Borg command: Toggle Flag: (m/d/s/f/g) ", &cmd)) return;

            switch (cmd)
            {
                /* Give borg thought messages in window */
                case 'm':
                case 'M':
                {
                    auto_borg_message = !auto_borg_message;
                    msg_format("Borg -- auto_borg_message is now %d.",
                                auto_borg_message);
                    break;
                }

                /* Give borg the ability to use graphics ----broken */
                case 'g':
                case 'G':
                {
                    break;
                }

                /* Dump savefile at each death */
                case 'd':
                case 'D':
                {
                    auto_flag_dump = !auto_flag_dump;
                    msg_format("Borg -- auto_flag_dump is now %d.",
                                auto_flag_dump);
                    break;
                }

                /* Dump savefile at each level */
                case 's':
                case 'S':
                {
                    auto_flag_save = !auto_flag_save;
                    msg_format("Borg -- auto_flag_save is now %d.",
                                auto_flag_save);
                    break;
                }

                /* clear 'fear' levels */
                case 'f':
                case 'F':
                {
                    msg_format("Command No Longer Usefull");
                    break;
                }
            }
            break;
        }



        /* Command: toggle "cheat" flags */
        case 'c':
        case 'C':
        {
            /* Get a "Borg command", or abort */
            if (!get_com("Borg command: Toggle Cheat: (d/i/e/s/p)", &cmd))
                return;

            switch (cmd)
            {
                case 'd':
                case 'D':
                {
                    borg_cheat_death = !borg_cheat_death;
                    msg_format("Borg -- auto_cheat_death is now %d.",
                                borg_cheat_death);
                    break;
                }
                case 'i':
                case 'I':
                {
                    auto_cheat_inven = !auto_cheat_inven;
                    msg_format("Borg -- auto_cheat_inven is now %d.",
                                auto_cheat_inven);
                    break;
                }
                case 'e':
                case 'E':
                {
                    auto_cheat_equip = !auto_cheat_equip;
                    msg_format("Borg -- auto_cheat_equip is now %d.",
                                auto_cheat_equip);
                    break;
                }
                case 's':
                case 'S':
                {
                    auto_cheat_spell = !auto_cheat_spell;
                    msg_format("Borg -- auto_cheat_spell is now %d.",
                                auto_cheat_spell);
                    break;
                }
                case 'p':
                case 'P':
                {
                    auto_cheat_panel = !auto_cheat_panel;
                    msg_format("Borg -- auto_cheat_panel is now %d.",
                                auto_cheat_panel);
                    break;
                }
            }
            break;

        }


        /* Start a new log file */
        case 'l':
        case 'L':
        {
            char buf[80];

            /* Close the log file */
            if (auto_fff) my_fclose(auto_fff);

            /* Hack -- drop permissions */
            safe_setuid_drop();

            /* Default  */
            strcpy(buf, "borg.log");

            /* XXX XXX XXX Get the name and open the log file */
            if (get_string("Borg Log File: ", buf, 70))
            {
                /* Open a new file */
                auto_fff = my_fopen(buf, "w");

                /* Failure */
                if (!auto_fff) msg_print("Cannot open that file.");
            }

            /* Hack -- grab permissions */
            safe_setuid_grab();
            break;
        }


        /* Activate a search string */
        case 's':
        case 'S':
        {
            /* Get the new search string (or cancel the matching) */
            if (!get_string("Borg Match String: ", auto_match, 70))
            {
                /* Cancel it */
                strcpy(auto_match, "");

                /* Message */
                msg_print("Borg Match String de-activated.");
            }
            break;
        }


        /* Command: check Grid "feature" flags */
        case 'g':
        case 'G':
        {
            int x, y;

            u16b low, high = 0;

            /* Get a "Borg command", or abort */
            if (!get_com("Borg command: Show grids: ", &cmd)) return;

            /* Extract a flag */
            switch (cmd)
            {
                case '0': low = high = 1 << 0; break;
                case '1': low = high = 1 << 1; break;
                case '2': low = high = 1 << 2; break;
                case '3': low = high = 1 << 3; break;
                case '4': low = high = 1 << 4; break;
                case '5': low = high = 1 << 5; break;
                case '6': low = high = 1 << 6; break;
                case '7': low = high = 1 << 7; break;

                case '.': low = high = FEAT_FLOOR; break;
                case ' ': low = high = FEAT_NONE; break;
                case 'i': low = high = FEAT_INVIS; break;
                case ';': low = high = FEAT_GLYPH; break;
                case ',': low = high = FEAT_OPEN; break;
                case 'x': low = high = FEAT_BROKEN; break;
                case '>': low = high = FEAT_LESS; break;
                case '<': low = high = FEAT_MORE; break;
                case '@': low = FEAT_SHOP_HEAD;
                          high = FEAT_SHOP_TAIL;
                           break;
                case '^': low = FEAT_TRAP_HEAD;
                          high = FEAT_TRAP_TAIL;
                          break;
                case '+': low  = FEAT_DOOR_HEAD; break;
                          high = FEAT_DOOR_TAIL; break;
                case 's': low = high = FEAT_SECRET; break;
                case ':': low = high = FEAT_RUBBLE; break;
                case 'm': low = high = FEAT_MAGMA; break;
                case 'q': low = high = FEAT_QUARTZ; break;
                case 'r': low = high = FEAT_QUARTZ_H; break;
                case 'k': low = high = FEAT_MAGMA_K; break;
                case '&': low = high = FEAT_QUARTZ_K; break;
                case 'w': low = FEAT_WALL_EXTRA;
                          high = FEAT_WALL_SOLID;
                          break;
                case 'p': low = FEAT_PERM_EXTRA;
                          high = FEAT_PERM_SOLID;
                          break;

                default: low = high = 0x00; break;
            }

            /* Scan map */
            for (y = w_y; y < w_y + SCREEN_HGT; y++)
            {
                for (x = w_x; x < w_x + SCREEN_WID; x++)
                {
                    byte a = TERM_RED;

                    auto_grid *ag = &auto_grids[y][x];

                    /* show only those grids */
                    if (!(ag->feat >= low && ag->feat <= high)) continue;

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
            break;
        }

        /* Command: check "info" flags */
        case 'i':
        case 'I':
        {
            int x, y;

            u16b mask;

            /* Get a "Borg command", or abort */
            if (!get_com("Borg command: Show grids: ", &cmd)) return;

            /* Extract a flag */
            switch (cmd)
            {
                case '0': mask = 1 << 0; break;
                case '1': mask = 1 << 1; break;
                case '2': mask = 1 << 2; break;
                case '3': mask = 1 << 3; break;
                case '4': mask = 1 << 4; break;
                case '5': mask = 1 << 5; break;
                case '6': mask = 1 << 6; break;
                case '7': mask = 1 << 7; break;

                case 'm': mask = BORG_MARK; break;
                case 'g': mask = BORG_GLOW; break;
                case 'd': mask = BORG_DARK; break;
                case 'o': mask = BORG_OKAY; break;
                case 'l': mask = BORG_LITE; break;
                case 'v': mask = BORG_VIEW; break;
                case 't': mask = BORG_TEMP; break;
                case 'x': mask = BORG_XTRA; break;

                default: mask = 0x00; break;
            }

            /* Scan map */
            for (y = w_y; y < w_y + SCREEN_HGT; y++)
            {
                for (x = w_x; x < w_x + SCREEN_WID; x++)
                {
                    byte a = TERM_RED;

                    auto_grid *ag = &auto_grids[y][x];

                    /* Given mask, show only those grids */
                    if (mask && !(ag->info & mask)) continue;

                    /* Given no mask, show unknown grids */
                    if (!mask && (ag->info & BORG_MARK)) continue;

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
            break;
        }

    /* Command: check "avoidances" */
        case 'a':
        case 'A':
        {
            int x, y, p;

            /* Scan map */
            for (y = w_y; y < w_y + SCREEN_HGT; y++)
            {
                for (x = w_x; x < w_x + SCREEN_WID; x++)
                {
                    byte a = TERM_RED;

                    /* Obtain danger */
                    p = borg_danger(y, x, 1);

                    /* Skip non-avoidances */
                    if (p <= avoidance / 2) continue;

                    /* Use yellow for less painful */
                    if (p <= avoidance) a = TERM_YELLOW;

                    /* Display */
                    print_rel('*', a, y, x);
                }
            }

            /* Get keypress */
            msg_format("(%d,%d of %d,%d) Avoidance value %d.", c_y, c_x, p_ptr->wy / PANEL_HGT,p_ptr->wx / PANEL_WID,avoidance);
            msg_print(NULL);

            /* Redraw map */
            prt_map();
            break;
        }


        /* Command: show "monsters" */
        case 'k':
        case 'K':
        {
            int i, n = 0;

            /* Scan the monsters */
            for (i = 1; i < auto_kills_nxt; i++)
            {
                auto_kill *kill = &auto_kills[i];

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
            break;
        }


        /* Command: show "objects" */
        case 't':
        case 'T':
        {
            int i, n = 0;

            /* Scan the objects */
            for (i = 1; i < auto_takes_nxt; i++)
            {
                auto_take *take = &auto_takes[i];

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
            break;
        }


        /* Command: debug -- current flow */
        case '%':
        {
            int i, x, y;

            /* Flow */
            for (i = 0; i < 250; i++)
            {
                int n = 0;

                /* Scan map */
                for (y = w_y; y < w_y + SCREEN_HGT; y++)
                {
                    for (x = w_x; x < w_x + SCREEN_WID; x++)
                    {
                        byte a = TERM_RED;

                        /* Verify flow cost */
                        if (auto_data_flow->data[y][x] != i) continue;

                        /* Display */
                        print_rel('*', a, y, x);

                        /* Count */
                        n++;
                    }
                }

                /* Nothing */
                if (!n) break;

                /* Get keypress */
                msg_format("Flow depth %d.", i);
                msg_print(NULL);

                /* Redraw map */
                prt_map();
            }
            break;
        }

        /* Command: debug -- danger of grid */
        case '#':
        {
            int n;

            /* Turns */
            n = (p_ptr->command_arg ? p_ptr->command_arg : 1);

            /* Examine the screen */
            borg_update_frame();

            /* Examine the screen */
            borg_update();

            /* Cheat the "equip" screen */
            borg_cheat_equip();

            /* Cheat the "inven" screen */
            borg_cheat_inven();

            /* Extract some "hidden" variables */
            borg_hidden();
            /* Examine the inventory */
            borg_object_star_id();
            borg_notice(TRUE);
            borg_notice_home(NULL, FALSE);

            /* Danger of grid */
            msg_format("Danger(%d,%d,%d) is %d",
                        p_ptr->target_col, p_ptr->target_row, n,
                        borg_danger(p_ptr->target_row, p_ptr->target_col, n));
            break;
        }

        /* Command: debug -- fear of depth */
        case '_':
        {
            /* Max depth */
            msg_format("Max depth %d, ", auto_max_depth);

            /* Fear depth */
            msg_format("Fear depth %d, ", fear_depth);

            /* Dump fear codes */
            msg_format("times completed fear depth %d", auto_fear_depth);
            break;
        }

        /* Command: debug -- Power */
        case 'p':
        case 'P':
        {
            s32b p;

            /* Examine the screen */
            borg_update_frame();

            /* Examine the screen */
            borg_update();

            /* Cheat the "equip" screen */
            borg_cheat_equip();

            /* Cheat the "inven" screen */
            borg_cheat_inven();

            /* Extract some "hidden" variables */
            borg_hidden();
            /* Examine the inventory */
            borg_object_star_id();
            borg_notice(TRUE);
            borg_notice_home(NULL, FALSE);


            /* Evaluate */
            p = borg_power();

            /* Report it */
            msg_format("Current Borg Power %ld", p);
            msg_format("Current Home Power %ld", borg_power_home);

            break;
        }

        /* Command: Show time */
        case '!':
        {
            s32b time = c_t - auto_began;
            msg_format("time: (%d) ", time);
            time = (auto_time_town + (c_t - auto_began));
            msg_format("; from town (%d)", time);
            msg_format("; on this panel (%d)", time_this_panel);
            msg_format("; need inviso (%d)", need_see_inviso);
            break;
        }

        /* Command: my projectable places */
        case '@':
        {
            int x, y;

            /* Scan map */
            for (y = w_y; y < w_y + SCREEN_HGT; y++)
            {
                for (x = w_x; x < w_x + SCREEN_WID; x++)
                {
                    byte a = TERM_RED;

                    /* Obtain danger */
                    if (!borg_projectable(y, x, c_y, c_x)) continue;

                    /* Display */
                    print_rel('*', a, y, x);
                }
            }

            /* Get keypress */
            msg_format("Borg has LOS to these places.");
            msg_print(NULL);

            /* Redraw map */
            prt_map();
            break;
        }

       /* APW command: debug -- change max depth */
       case '1':
 		{
           int new_auto_max_depth;
           /* Get the new max depth */
           new_auto_max_depth = get_quantity("Enter new Max Depth: ", MAX_DEPTH - 1);

           /* Allow user abort */
           if (new_auto_max_depth >= 0)
           {
               p_ptr->max_depth = new_auto_max_depth;
               auto_max_depth = new_auto_max_depth;
           }

           break;
       }
       /* APW command: debug -- allow borg to stop */
       case 'q':
       case 'Q':
 		{
           int new_auto_stop_dlevel;
           int new_auto_stop_clevel;
           char cmd;

           /* Get the new max depth */
           new_auto_stop_dlevel = get_quantity("Enter new auto-stop dlevel: ", MAX_DEPTH -1);
           new_auto_stop_clevel = get_quantity("Enter new auto-stop clevel: ", 50);
           get_com("Borg stops after killing Morgoth? (y or n) ", &cmd);

           auto_stop_dlevel = new_auto_stop_dlevel;
           auto_stop_clevel = new_auto_stop_clevel;
           if (cmd == 'n' ||  cmd == 'N')
                auto_stop_king = FALSE;

           break;
       }


        /* Command: APW HACK debug -- preparation for level */
        case '2':
        {
          int i=0;

            /* Examine the screen */
            borg_update_frame();
            borg_update();

            /* Extract some "hidden" variables */
            borg_cheat_equip();
            borg_cheat_inven();
        	borg_hidden();

            /* Examine the inventory */
            borg_object_star_id();
            borg_notice(TRUE);
            borg_notice_home(NULL, FALSE);

            /* Dump prep codes */
            for (i = 1; i <= 127; i++)
            {
               /* Dump fear code*/
               if ((cptr)NULL != borg_prepared(i)) break;
            }
            msg_format("Max Level: %d  Prep'd For: %d  Reason Reason: %s", auto_max_depth, i-1, borg_prepared(i) );
            if (borg_ready_morgoth)
            {
                msg_format("You are ready for the big fight!!");
            }

            break;
        }
        /* Command: debug -- Feature of grid */
        case '3':
        {
            auto_grid *ag;

            /* Examine the screen */
            borg_update_frame();
            borg_update();
        	borg_hidden();

            ag=&auto_grids[p_ptr->target_col][p_ptr->target_row];

            /* Feature of grid */
            msg_format("Feature of (%d,%d) is %d",
                        p_ptr->target_col, p_ptr->target_row, ag->feat);
            break;
        }
        /* Command: List the swap weapon and armour */
        case 'w':
        case 'W':
        {
            auto_item *item;

            /* Examine the screen */
        	borg_update();

            /* Extract some "hidden" variables */
        	borg_hidden();

            /* Cheat the "equip" screen */
    		borg_cheat_equip();
    		/* Cheat the "inven" screen */
    		borg_cheat_inven();
    		/* Examine the inventory */
            borg_notice(TRUE);
            borg_notice_home(NULL, FALSE);
            /* Check the power */
            borg_power();

            /* Examine the screen */
            borg_update_frame();

            /* note the swap items */
            item = &auto_items[weapon_swap];
            msg_format("Swap Weapon:  %s, value= %d", item->desc, weapon_swap_value);
            item = &auto_items[armour_swap];
            msg_format("Swap Armour:  %s, value= %d", item->desc, armour_swap_value);
            break;
        }

        case 'd':
        case 'D':
        {
            int ii= 1;

            /* Save the screen */
            Term_save();

            /* Dump the spells */
            if (mb_ptr->spell_book)
            {

            int i,j;

            for (i = 0; i < 9; i++ )
            {
                /* Clear the screen */
                Term_clear();

                ii = 2;
                Term_putstr(1, ii, -1, TERM_WHITE, "[ Spells ].");
                for (j = 0; j < 8; j++)
                {
                    auto_magic *as = &auto_magics[i][j];
                    cptr legal;

                    if (as->level <99)
                    {
                        if (mb_ptr->spell_book == TV_PRAYER_BOOK)
                        {
                            legal = (borg_prayer_legal(i, j) ? "Legal" : "Not Legal ");
                        }
                        else
                        {
                            legal = (borg_spell_legal(i, j) ? "legal" : "Not Legal ");
                        }
                        Term_putstr(1, ii++, -1, TERM_WHITE, format("%s, %s, attempted %d times",as->name, legal, as->times));
                    }
                }
            get_com("Exam spell books.  Press any key for next book.", &cmd);
            } /* dumps */
            } /* spells */

           /* Restore the screen */
           Term_load();

            /* Done */
            return;
        }

        case 'v':
        case 'V':
        {
            msg_format("APWBorg Version: Oct 3, 2000 ");
            break;
        }

        /* Oops */
        default:
        {
            /* Message */
            msg_print("That is not a legal Borg command.");
            break;
        }
    }
}



#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
