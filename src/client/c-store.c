/*
 * File: c-store.c
 * Purpose: Store stocking and UI (client side)
 *
 * Copyright (c) 1997 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 2007 Andrew Sidwell, who rewrote a fair portion
 * Copyright (c) 2012 MAngband and PWMAngband Developers
 *
 * This work is free software; you can redistribute it and/or modify it
 * under the terms of either:
 *
 * a) the GNU General Public License as published by the Free Software
 *    Foundation, version 2, or
 *
 * b) the "Angband licence":
 *    This software may be copied and distributed for educational, research,
 *    and not for profit purposes provided that this copyright and statement
 *    are included in all such copies.  Other copyrights may also apply.
 */


#include "c-angband.h"
#include "../common/tvalsval.h"
#include "c-cmds.h"
#include "game-event.h"
#include "netclient.h"
#include "ui-menu.h"


/*** Constants and definitions ***/


/* Easy names for the elements of the 'scr_places' arrays. */
enum
{
    LOC_PRICE = 0,
    LOC_OWNER,
    LOC_HEADER,
    LOC_MORE,
    LOC_HELP_CLEAR,
    LOC_HELP_PROMPT,
    LOC_AU,
    LOC_WEIGHT,

    LOC_MAX
};


/* Places for the various things displayed onscreen */
static unsigned int scr_places_x[LOC_MAX];
static unsigned int scr_places_y[LOC_MAX];


/* State flags */
#define STORE_GOLD_CHANGE      0x01
#define STORE_FRAME_CHANGE     0x02
#define STORE_SHOW_HELP        0x04


/* Compound flag for the initial display of a store */
#define STORE_INIT_CHANGE (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE)


/*** Variables to maintain state ***/


/* Flags for the display */
static u16b store_flags;


/* Wait for command to be processed by the server */
static bool store_command_wait = FALSE;


/* Hack - eject player from store */
static bool leave_store;


/*** Flavour text stuff ***/


/*
 * Shopkeeper welcome messages.
 *
 * The shopkeeper's name must come first, then the character's name.
 */
static const char *comment_welcome[] =
{
    "",
    "%s nods to you.",
    "%s says hello.",
    "%s: \"See anything you like, adventurer?\"",
    "%s: \"How may I help you, %s?\"",
    "%s: \"Welcome back, %s.\"",
    "%s: \"A pleasure to see you again, %s.\"",
    "%s: \"How may I be of assistance, good %s?\"",
    "%s: \"You do honour to my humble store, noble %s.\"",
    "%s: \"I am entirely at your service, glorious %s.\""
};


/* Return a random hint from the global hints list */
static char* random_hint(void)
{
    struct hint *v, *r = NULL;
    int n;

    for (v = hints, n = 1; v; v = v->next, n++)
    {
        if (one_in_(n)) r = v;
    }

    return r->hint;
}


/*
 * The greeting a shopkeeper gives the character says a lot about his
 * general attitude.
 *
 * Taken and modified from Sangband 1.0.
 */
static void prt_welcome(void)
{
    char short_name[20];
    int j;

    if (one_in_(2)) return;

    /* Extract the first name of the store owner (stop before the first space) */
    for (j = 0; current_store.owner->name[j] && current_store.owner->name[j] != ' '; j++)
        short_name[j] = current_store.owner->name[j];

    /* Truncate the name */
    short_name[j] = '\0';

    if (one_in_(3))
        prt(format("\"%s\"", random_hint()), 0, 0);
    else if (p_ptr->lev > 5)
    {
        const char *player_name;

        /* We go from level 1 - 50  */
        size_t i = (p_ptr->lev - 1) / 5;

        i = MIN(i, N_ELEMENTS(comment_welcome) - 1);

        /* Get a title for the character */
        if ((i % 2) && randint0(2)) player_name = title;
        else if (randint0(2)) player_name = p_ptr->name;
        else if (p_ptr->psex == SEX_FEMALE) player_name = "lady";
        else if (p_ptr->psex == SEX_MALE) player_name = "sir";
        else player_name = "master";

        /* Balthazar says "Welcome" */
        if (i >= 4)
            prt(format(comment_welcome[i], short_name, player_name), 0, 0);
        else
            prt(format(comment_welcome[i], short_name), 0, 0);
    }
}


/*** Check if a store may buy an object ***/


/*
 * Determine if the current store may purchase the given item
 */
static bool store_may_buy(struct player *p, const object_type *o_ptr)
{
    /* Switch on the store */
    switch (current_store.sidx)
    {
        /* General Store */
        case STORE_GENERAL:
        {
            /* Accept lights (inc. oil), spikes and food */
            if ((o_ptr->tval == TV_LIGHT) || (o_ptr->tval == TV_FOOD) ||
                (o_ptr->tval == TV_FLASK) || (o_ptr->tval == TV_SPIKE))
            {
                break;
            }

            /* PWMAngband: accept tools */
            if ((o_ptr->tval == TV_DIGGING) || (o_ptr->tval == TV_HORN))
                 break;

            return FALSE;
        }

        /* Armoury */
        case STORE_ARMOR: return armor_p(o_ptr);

        /* Weapon Shop */
        case STORE_WEAPON:
            return (wieldable_p(o_ptr) || (o_ptr->tval == TV_MSTAFF));

        /* Temple */
        case STORE_TEMPLE:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_SCROLL:
                case TV_POTION:
                case TV_HAFTED:
                case TV_POLEARM:
                case TV_SWORD:
                case TV_PRAYER_BOOK:
                    break;
                default:
                    return FALSE;
            }
            break;
        }

        /* Alchemist */
        case STORE_ALCHEMY:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_SCROLL:
                case TV_POTION:
                    break;
                default:
                    return FALSE;
            }
            break;
        }

        /* Magic Shop */
        case STORE_MAGIC:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_AMULET:
                case TV_RING:
                case TV_STAFF:
                case TV_WAND:
                case TV_ROD:
                case TV_SCROLL:
                case TV_POTION:
                    break;
                default:
                    return FALSE;
            }
            break;
        }

        /* House of Arcanes */
        case STORE_LIBRARY:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_MAGIC_BOOK:
                case TV_SORCERY_BOOK:
                case TV_SHADOW_BOOK:
                case TV_HUNT_BOOK:
                case TV_PSI_BOOK:
                case TV_DEATH_BOOK:
                case TV_ELEM_BOOK:
                case TV_SUMMON_BOOK:
                    break;
                default:
                    return FALSE;
            }
            break;
        }
    }

    /* Assume okay */
    return TRUE;
}


/*** Display code ***/


/*
 * This function sets up screen locations based on the current term size.
 *
 * Current screen layout:
 *  line 0: reserved for messages
 *  line 1: shopkeeper and their purse / item buying price
 *  line 2: empty
 *  line 3: table headers
 *
 *  line 4: Start of items
 *
 * If help is turned off, then the rest of the display goes as:
 *
 *  line (height - 4): end of items
 *  line (height - 3): "more" prompt
 *  line (height - 2): empty
 *  line (height - 1): Help prompt and remaining gold
 *
 * If help is turned on, then the rest of the display goes as:
 *
 *  line (height - 7): end of items
 *  line (height - 6): "more" prompt
 *  line (height - 5): empty
 *  line (height - 4): gold remaining
 *  line (height - 3): command help
 */
static void store_display_recalc(menu_type *m)
{
    int wid, hgt;
    region loc;

    Term_get_size(&wid, &hgt);

    /* Clip the width at a maximum of 104 (enough room for an 80-char item name) */
    if (wid > 104) wid = 104;

    /* X co-ords first */
    scr_places_x[LOC_PRICE] = wid - 14;
    scr_places_x[LOC_AU] = wid - 26;
    scr_places_x[LOC_OWNER] = wid - 2;
    scr_places_x[LOC_WEIGHT] = wid - 24;

    /* Then Y */
    scr_places_y[LOC_OWNER] = 1;
    scr_places_y[LOC_HEADER] = 3;

    /* If we are displaying help, make the height smaller */
    if (store_flags & (STORE_SHOW_HELP)) hgt -= 3;

    scr_places_y[LOC_MORE] = hgt - 3;
    scr_places_y[LOC_AU] = hgt - 1;

    loc = m->boundary;

    /* If we're displaying the help, then put it with a line of padding */
    if (store_flags & (STORE_SHOW_HELP))
    {
        scr_places_y[LOC_HELP_CLEAR] = hgt - 1;
        scr_places_y[LOC_HELP_PROMPT] = hgt;
        loc.page_rows = -5;
    }
    else
    {
        scr_places_y[LOC_HELP_CLEAR] = hgt - 2;
        scr_places_y[LOC_HELP_PROMPT] = hgt - 1;
        loc.page_rows = -2;
    }

    menu_layout(m, &loc);
}


/*
 * Redisplay a single store entry
 */
static void store_display_entry(menu_type *menu, int oid, bool cursor, int row, int col, int width)
{
    object_type *o_ptr;
    s32b x;
    char o_name[NORMAL_WID];
    char out_val[160];
    byte colour;

    /* Get the object */
    o_ptr = &current_store.stock[oid];

    /* Describe the object */
    my_strcpy(o_name, store_names[oid], sizeof(o_name));

    /* Display the object */
    c_put_str(o_ptr->info_xtra.attr, o_name, row, col);

    /* Show weights */
    colour = curs_attrs[CURS_KNOWN][(int)cursor];
    strnfmt(out_val, sizeof(out_val), "%3d.%d lb", o_ptr->weight / 10, o_ptr->weight % 10);
    c_put_str(colour, out_val, row, scr_places_x[LOC_WEIGHT]);

    /* Extract the "minimum" price */
    x = o_ptr->askprice;

    /* Make sure the player can afford it */
    if (p_ptr->au < x)
        colour = curs_attrs[CURS_UNKNOWN][(int)cursor];

    /* Actually draw the price */
    if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) && (o_ptr->number > 1))
        strnfmt(out_val, sizeof(out_val), "%9ld avg", (long)x);
    else
        strnfmt(out_val, sizeof(out_val), "%9ld    ", (long)x);

    /* The price is not available if 0 (the item is not for sale) */
    if (x == 0)
        c_put_str(TERM_SLATE, "N/A", row, scr_places_x[LOC_PRICE] + 5);
    else
        c_put_str(colour, out_val, row, scr_places_x[LOC_PRICE]);
}


/*
 * Display store (after clearing screen)
 */
static void store_display_frame(void)
{
    char buf[MSG_LEN];

    /* Clear screen (except top line) */
    clear_from(1);

    /* A player owned store */
    if (current_store.sidx == STORE_PLAYER)
    {
        /* Put the owner name */
        strnfmt(buf, sizeof(buf), "%s's %s", current_store.owner->name, store_name);
        put_str(buf, scr_places_y[LOC_OWNER], 1);
    }

    /* Normal stores */
    else
    {
        /* Put the owner name */
        put_str(current_store.owner->name, scr_places_y[LOC_OWNER], 1);

        /* Show the max price in the store (above prices) */
        strnfmt(buf, sizeof(buf), "%s (%ld)", store_name, current_store.owner->max_cost);
        prt(buf, scr_places_y[LOC_OWNER], scr_places_x[LOC_OWNER] - strlen(buf));
    }

    /* Label the object descriptions */
    put_str("Store Inventory", scr_places_y[LOC_HEADER], 1);

    /* Showing weight label */
    put_str("Weight", scr_places_y[LOC_HEADER], scr_places_x[LOC_WEIGHT] + 2);

    /* Label the asking price (in stores) */
    put_str("Price", scr_places_y[LOC_HEADER], scr_places_x[LOC_PRICE] + 4);
}


static void text_out_c(byte attr, const char *text, int y, int* px)
{
    int x;

    /* Check line break */
    x = (*px) + strlen(text);
    if (x > NORMAL_WID - 2) return;

    /* Display text */
    c_put_str(attr, text, y, *px);

    /* Advance */
    (*px) += strlen(text);
}


static void text_out(const char *text, int y, int* px)
{
    text_out_c(TERM_WHITE, text, y, px);
}


/*
 * Display help.
 */
static void store_display_help(void)
{
    int help_loc_y = scr_places_y[LOC_HELP_PROMPT];
    int help_loc_x = 1;

    /* Clear */
    clear_from(scr_places_y[LOC_HELP_CLEAR]);

    /* Display help */
    text_out("Use the ", help_loc_y, &help_loc_x);
    text_out_c(TERM_L_GREEN, "movement keys", help_loc_y, &help_loc_x);
    text_out(" to navigate, or '", help_loc_y, &help_loc_x);
    text_out_c(TERM_L_GREEN, "SPACE", help_loc_y, &help_loc_x);
    text_out("' to advance to the next page.", help_loc_y, &help_loc_x);
    help_loc_y++;
    help_loc_x = 1;
    text_out("'", help_loc_y, &help_loc_x);
    if (OPT(rogue_like_commands))
        text_out_c(TERM_L_GREEN, "x", help_loc_y, &help_loc_x);
    else
        text_out_c(TERM_L_GREEN, "l", help_loc_y, &help_loc_x);
    text_out("' examines and '", help_loc_y, &help_loc_x);
    text_out_c(TERM_L_GREEN, "p", help_loc_y, &help_loc_x);
    text_out("' purchases", help_loc_y, &help_loc_x);
    text_out(" the selected item.", help_loc_y, &help_loc_x);
    if (current_store.sidx == STORE_XBM)
    {
        text_out(" '", help_loc_y, &help_loc_x);
        text_out_c(TERM_L_GREEN, "o", help_loc_y, &help_loc_x);
        text_out("' orders an item.", help_loc_y, &help_loc_x);
    }
    help_loc_y++;
    help_loc_x = 1;
    text_out("'", help_loc_y, &help_loc_x);
    text_out_c(TERM_L_GREEN, "s", help_loc_y, &help_loc_x);
    text_out("' sells an item from your inventory. '", help_loc_y, &help_loc_x);
    text_out_c(TERM_L_GREEN, "ESC", help_loc_y, &help_loc_x);
    text_out("' exits the building.", help_loc_y, &help_loc_x);
}


/*
 * Decides what parts of the store display to redraw.  Called on terminal
 * resizings and the redraw command.
 */
static void store_redraw(void)
{
    if (store_flags & (STORE_FRAME_CHANGE))
    {
        store_display_frame();

        if (store_flags & STORE_SHOW_HELP)
            store_display_help();
        else
            prt("Press '?' for help.", scr_places_y[LOC_HELP_PROMPT], 1);

        store_flags &= ~(STORE_FRAME_CHANGE);
    }

    if (store_flags & (STORE_GOLD_CHANGE))
    {
        prt(format("Gold Remaining: %9ld", (long)p_ptr->au),
            scr_places_y[LOC_AU], scr_places_x[LOC_AU]);
        store_flags &= ~(STORE_GOLD_CHANGE);
    }
}


/*
 * Display player's gold
 */
void store_prt_gold(void)
{
    store_flags |= STORE_GOLD_CHANGE;
    store_redraw();
}


/*
 * Display store frame
 */
void store_prt_frame(void)
{
    store_flags |= STORE_FRAME_CHANGE;
    store_redraw();
}


/*** Higher-level code ***/


/*
 * Buy an object from a store
 */
static bool store_purchase(int item)
{
    int amt, num;
    object_type *o_ptr;
    char o_name[NORMAL_WID];
    s32b price;
    ui_event ea = EVENT_ABORT;

    /* Paranoia */
    if (item < 0) return FALSE;

    /* Get the actual object */
    o_ptr = &current_store.stock[item];

    /* Clear all current messages */
    prt("", 0, 0);

    /* Price of one */
    price = o_ptr->askprice;

    /* Check "shown" items */
    if (price == 0)
    {
        c_msg_print("Sorry, this item is not for sale.");
        return FALSE;
    }

    /* Check if the player can afford any at all */
    if (p_ptr->au < price)
    {
        /* Tell the user */
        c_msg_print("You do not have enough gold for this item.");

        /* Abort now */
        return FALSE;
    }

    /* Work out how many the player can afford */
    amt = o_ptr->info_xtra.max;

    /* Find the number of this item in the inventory */
    num = o_ptr->info_xtra.owned;
    strnfmt(o_name, sizeof(o_name), "Buy how many%s? (max %d) ",
        (num? format(" (you have %d)", num): ""), amt);

    /* Get a quantity */
    amt = get_quantity_ex(o_name, amt);

    /* Allow user abort */
    if (amt <= 0)
    {
        if (amt == -1) Term_event_push(&ea);
        return FALSE;
    }

    /* Wait for command to be processed by the server */
    store_command_wait = TRUE;

    /* Tell the server */
    Send_store_purchase(item, amt);

    return TRUE;
}


/*
 * Sell an item to the store
 */
static bool store_sell(void)
{
    int amt = 1;
    int item;
    int get_mode = USE_EQUIP | USE_INVEN | SHOW_PRICES;
    object_type *o_ptr;
    const char *reject = "You have nothing that I want.";
    const char *prompt = "Sell which item?";
    ui_event ea = EVENT_ABORT;

    /* Prepare the hook */
    item_tester_hook = store_may_buy;

    /* Get an item */
    if (!get_item(&item, prompt, reject, CMD_DROP, get_mode)) return FALSE;
    if (check_store_leave(FALSE)) return FALSE;

    /* Get the item (in the pack) */
    o_ptr = &p_ptr->inventory[item];

    /* Get a quantity (if number of sellable items is greater than 1) */
    /* Note: sale can always be aborted at the "Accept xxx gold?" prompt */
    if (o_ptr->number > 1)
    {
        amt = get_quantity_ex(NULL, o_ptr->number);

        /* Allow user abort */
        if (amt <= 0)
        {
            if (amt == -1) Term_event_push(&ea);
            return FALSE;
        }
    }

    /* Wait for command to be processed by the server */
    store_command_wait = TRUE;

    /* Tell the server */
    Send_store_sell(item, amt);

    return TRUE;
}


/*
 * Examine an item in a store
 */
static void store_examine(int item)
{
    /* Tell the server */
    Send_store_examine(item);
}


/*
 * Order an item
 */
static void store_order(void)
{
    char buf[NORMAL_WID];

    /* Get a name or abort */
    buf[0] = '\0';
    if (!get_string("Enter (partial) object name: ", buf, sizeof(buf))) return;

    /* Tell the server */
    Send_store_order(buf);
}


static void store_menu_set_selections(menu_type *menu)
{
    /* Roguelike */
    if (OPT(rogue_like_commands))
    {
        /* These two can't intersect! */
        menu->cmd_keys = "degiopsx?&";
        menu->selections = "abcfhlmnqrtuvwyzABCDEFGH";
    }

    /* Original */
    else
    {
        /* These two can't intersect! */
        menu->cmd_keys = "degilops?&";
        menu->selections = "abcfhjkmnqrtuvwxyzABCDEF";
    }
}


static void store_menu_recalc(menu_type *m)
{
    struct store *st_ptr = &current_store;

    menu_setpriv(m, st_ptr->stock_num, st_ptr->stock);
}


/*
 * Process a command in a store
 *
 * Note that we must allow the use of a few "special" commands in the stores
 * which are not allowed in the dungeon, and we must disable some commands
 * which are allowed in the dungeon but not in the stores, to prevent chaos.
 */
static bool store_process_command_key(struct keypress kp)
{
    /* Process the keycode */
    switch (kp.code)
    {
        case 'e':
        {
            do_cmd_equip();
            check_store_leave(TRUE);
            break;
        }

        case 'i':
        {
            do_cmd_inven();
            check_store_leave(TRUE);
            break;
        }

        default:
        {
            c_msg_print("That command does not work in stores.");
            return FALSE;
        }
    }

    return TRUE;
}


/*
 * Select an item from the store's stock, and return the stock index
 */
static int store_get_stock(menu_type *m, int oid)
{
    ui_event e;
    int no_act = (m->flags & MN_NO_ACTION);

    /* Set a flag to make sure that we get the selection or escape without running the menu handler */
    m->flags |= MN_NO_ACTION;
    e = menu_select(m, 0, TRUE);
    if (!no_act) m->flags &= ~MN_NO_ACTION;

    if (e.type == EVT_SELECT) return m->cursor;
    if (e.type == EVT_ESCAPE) return -1;

    /* If we do not have a new selection, just return the original item */
    return oid;
}


/* Loop callback */
static void store_callback_begin(ui_event *cp)
{
    /* If we got a ^R key, cancel the wait (in case the server didn't respond...) */
    if ((cp->type == EVT_KBRD) && (cp->key.code == KTRL('R'))) store_command_wait = FALSE;

    /* Wait for command to be processed by the server */
    cp->type = EVT_NONE;
    if (!store_command_wait) cp->type = EVT_DONE;
}


static bool store_menu_handle(menu_type *m, const ui_event *event, int oid)
{
    bool processed = TRUE;

    /* Leave store */
    if (leave_store) return TRUE;

    if (event->type == EVT_SELECT)
    {
        /* Nothing for now. */
        /* In future, maybe we want a display a list of what you can do. */
        return TRUE;
    }
    else if (event->type == EVT_KBRD)
    {
        bool storechange = FALSE;

        switch (event->key.code)
        {
            case 's':
            case 'd':
            {
                /* Paranoia: nothing to sell */
                if (current_store.sidx == STORE_PLAYER)
                    c_msg_print("That command does not work in this store.");
                else
                    storechange = store_sell();
                break;
            }

            case 'p':
            case 'g':
            {
                /* Paranoia: nothing to purchase */
                if (current_store.stock_num <= 0)
                {
                    if (current_store.sidx != STORE_PLAYER)
                        c_msg_print("I am currently out of stock.");
                    else
                        c_msg_print("This player shop is empty.");
                }
                else
                {
                    /* Use the old way of purchasing items */
                    prt("Purchase which item? (ESC to cancel, Enter to select)", 0, 0);
                    oid = store_get_stock(m, oid);
                    prt("", 0, 0);
                    if (oid >= 0) storechange = store_purchase(oid);
                }
                break;
            }

            case 'l':
            case 'x':
            {
                /* Paranoia: nothing to examine */
                if (current_store.stock_num > 0)
                {
                    /* Use the old way of examining items */
                    prt("Examine which item? (ESC to cancel, Enter to select)", 0, 0);
                    oid = store_get_stock(m, oid);
                    prt("", 0, 0);
                    if (oid >= 0) store_examine(oid);
                }
                break;
            }

            case '?':
            {
                /* Toggle help */
                if (store_flags & STORE_SHOW_HELP)
                    store_flags &= ~(STORE_SHOW_HELP);
                else
                    store_flags |= STORE_SHOW_HELP;

                /* Redisplay */
                store_flags |= STORE_INIT_CHANGE;
                break;
            }

            case '&':
            {
                /* Hack -- Redisplay */
                store_flags |= STORE_INIT_CHANGE;
                break;
            }

            case 'o':
            {
                /* Order an item */
                if (current_store.sidx == STORE_XBM)
                    store_order();
                else
                    c_msg_print("You cannot order from this store.");
                break;
            }

            default:
                processed = store_process_command_key(event->key);
        }

        /* Leave store */
        if (leave_store) return TRUE;

        /* Loop, looking for net input and responding to keypresses */
        Net_loop(Term_inkey, store_callback_begin, NULL, SCAN_OFF);

        if (storechange) store_menu_recalc(m);

        if (processed)
        {
            event_signal(EVENT_INVENTORY);
            event_signal(EVENT_EQUIPMENT);
        }

        /* Notice and handle stuff */
        redraw_stuff();

        /* Display the store */
        store_display_recalc(m);
        store_menu_recalc(m);
        store_redraw();

        return processed;
    }

    return FALSE;
}


static region store_menu_region = { 1, 4, -1, -2 };


static const menu_iter store_menu =
{
    NULL,
    NULL,
    store_display_entry,
    store_menu_handle,
    NULL
};


/*
 * Hack -- Memorize the menu to refresh it during selling process
 */
static menu_type *pm = NULL;


/*
 * Enter a store, and interact with it.
 */
void do_cmd_store(void)
{
    menu_type menu;

    /* Hack -- Memorize the menu to refresh it during selling process */
    pm = &menu;

    /* Save the screen */
    screen_save();

    /* We are "shopping" */
    shopping = TRUE;
    leave_store = FALSE;

    /* Clear the top line */
    Term_erase(0, 0, 255);

    /* Wipe the menu and set it up */
    menu_init(&menu, MN_SKIN_SCROLL, &store_menu);
    menu_layout(&menu, &store_menu_region);

    store_menu_set_selections(&menu);
    store_flags = STORE_INIT_CHANGE;
    store_display_recalc(&menu);
    store_menu_recalc(&menu);
    store_redraw();

    /* Say a friendly hello. */
    if (current_store.sidx != STORE_PLAYER) prt_welcome();

    menu_select(&menu, 0, FALSE);

    /* We are no longer "shopping" */
    shopping = FALSE;
    leave_store = FALSE;

    /* Restore the screen */
    screen_load(TRUE);

    /* Redraw */
    p_ptr->redraw |= (PR_EQUIP);

    /* Tell the server that we're outta here */
    Send_store_leave();

    /* Hack -- No more menu */
    pm = NULL;
}


void store_sell_accept(s32b price, s16b reset)
{
    char buf[NORMAL_WID];
    int res;
    ui_event ea = EVENT_ABORT;

    /* If the item was rejected, cancel the wait */
    if (price < 0)
    {
        store_command_wait = FALSE;
        return;
    }

    /* Hack -- Redisplay (unless selling a house) */
    if (pm)
    {
        store_flags |= STORE_INIT_CHANGE;
        store_redraw();
        menu_refresh(pm, FALSE);
    }

    /* Tell the user about the price */
    if (reset)
        my_strcpy(buf, "Do you really want to reset this house? ", sizeof(buf));
    else
        strnfmt(buf, sizeof(buf), "Price is %ld gold. Proceed? ", price);

    /* Accept the price, or cancel the wait */
    res = get_check_ex(buf);
    if (!res) Send_store_confirm();
    else if (res == 1) Term_event_push(&ea);
    else store_command_wait = FALSE;
}


void store_purchase_end(void)
{
    /* Cancel wait */
    store_command_wait = FALSE;
}    


void store_sell_end(void)
{
    /* Cancel wait */
    store_command_wait = FALSE;
}        


void store_leave(void)
{
    ui_event ea = EVENT_ABORT;

    /* Leave store */
    store_command_wait = FALSE;
    Term_event_push(&ea);
    leave_store = TRUE;
}


bool check_store_leave(bool refresh)
{
    ui_event ea = EVENT_ABORT;

    if (leave_store) Term_event_push(&ea);
    else if (shopping)
    {
        /* Hack -- Redisplay */
        store_flags |= STORE_INIT_CHANGE;
        if (refresh) Term_key_push('&');
    }
    return leave_store;
}
