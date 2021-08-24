/*
 * File: ui-store.c
 * Purpose: Store UI
 *
 * Copyright (c) 1997 Robert A. Koeneke, James E. Wilson, Ben Harrison
 * Copyright (c) 1998-2014 Angband developers
 * Copyright (c) 2018 MAngband and PWMAngband Developers
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
    "%s: \"I and my family are entirely at your service, %s.\""
};


/* State flags */
#define STORE_GOLD_CHANGE      0x01
#define STORE_FRAME_CHANGE     0x02
#define STORE_SHOW_HELP        0x04


/* Compound flag for the initial display of a store */
#define STORE_INIT_CHANGE (STORE_FRAME_CHANGE | STORE_GOLD_CHANGE)


/*** Variables to maintain state ***/


/*
 * Store UI data
 */
struct store_context *store_ctx = NULL;


/* Wait for command to be processed by the server */
static bool store_command_wait = false;


/* Hack -- eject player from store */
static bool leave_store;


/* The hints array */
struct hint *hints;


/* The general info about the current store */
struct store current_store;


/* The names of the stuff in the store */
store_name *store_names;


/* Return a random hint from the global hints list */
static char *random_hint(void)
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
 *
 * Note that each comment_hint should have exactly one %s
 */
static void prt_welcome(const struct owner *proprietor)
{
    char short_name[20];
    const char *owner_name = proprietor->name;
    int j;

    if (one_in_(2)) return;

    /* Get the first name of the store owner (stop before the first space) */
    for (j = 0; owner_name[j] && owner_name[j] != ' '; j++)
        short_name[j] = owner_name[j];

    /* Truncate the name */
    short_name[j] = '\0';

    if (one_in_(3))
        prt(format("\"%s\"", random_hint()), 0, 0);
    else if (player->lev > 5)
    {
        const char *player_name;

        /* We go from level 1 - 50  */
        size_t i = (player->lev - 1) / 5;

        i = MIN(i, N_ELEMENTS(comment_welcome) - 1);

        /* Get a title for the character */
        if ((i % 2) && randint0(2)) player_name = title;
        else if (randint0(2)) player_name = player->name;
        else
        {
            switch (player->psex)
            {
                case SEX_MALE: player_name = "sir"; break;
                case SEX_FEMALE: player_name = "lady"; break;
                default: player_name = "ser"; break;
            }
        }

        /* Balthazar says "Welcome" */
        if (i >= 4)
            prt(format(comment_welcome[i], short_name, player_name), 0, 0);
        else
            prt(format(comment_welcome[i], short_name), 0, 0);
    }
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
static void store_display_recalc(struct store_context *ctx)
{
    int wid, hgt;
    region loc;
    struct menu *m = &ctx->menu;
    struct store *store = ctx->store;

    Term_get_size(&wid, &hgt);

    /* Clip the width at a maximum of 104 (enough room for an 80-char item name) */
    if (wid > 104) wid = 104;

    /* X co-ords first */
    ctx->scr_places_x[LOC_PRICE] = wid - 14;
    ctx->scr_places_x[LOC_AU] = wid - 26;
    ctx->scr_places_x[LOC_OWNER] = wid - 2;
    ctx->scr_places_x[LOC_WEIGHT] = wid - 14;

    /* Add space for prices */
    if (store->sidx != STORE_HOME) ctx->scr_places_x[LOC_WEIGHT] -= 10;

    /* Then Y */
    ctx->scr_places_y[LOC_OWNER] = 1;
    ctx->scr_places_y[LOC_HEADER] = 3;

    /* If we are displaying help, make the height smaller */
    if (ctx->flags & (STORE_SHOW_HELP)) hgt -= 3;

    ctx->scr_places_y[LOC_MORE] = hgt - 3;
    ctx->scr_places_y[LOC_AU] = hgt - 1;

    loc = m->boundary;

    /* If we're displaying the help, then put it with a line of padding */
    if (ctx->flags & (STORE_SHOW_HELP))
    {
        ctx->scr_places_y[LOC_HELP_CLEAR] = hgt - 1;
        ctx->scr_places_y[LOC_HELP_PROMPT] = hgt;
        loc.page_rows = -5;
    }
    else
    {
        ctx->scr_places_y[LOC_HELP_CLEAR] = hgt - 2;
        ctx->scr_places_y[LOC_HELP_PROMPT] = hgt - 1;
        loc.page_rows = -2;
    }

    menu_layout(m, &loc);
}


/*
 * Redisplay a single store entry
 */
static void store_display_entry(struct menu *menu, int oid, bool cursor, int row, int col, int width)
{
    struct object *obj;
    s32b x;
    char o_name[NORMAL_WID];
    char out_val[160];
    byte colour;
    struct store_context *ctx = menu_priv(menu);
    struct store *store = ctx->store;

    /* Get the object */
    obj = &ctx->list[oid];

    /* Describe the object */
    my_strcpy(o_name, store_names[oid], sizeof(o_name));

    /* Display the object */
    c_put_str(obj->info_xtra.attr, o_name, row, col);

    /* Show weights */
    colour = curs_attrs[CURS_KNOWN][(int)cursor];
    strnfmt(out_val, sizeof(out_val), "%3d.%d lb", obj->weight / 10, obj->weight % 10);
    c_put_str(colour, out_val, row, ctx->scr_places_x[LOC_WEIGHT]);

    /* Describe an object (fully) in a store */
    if (store->sidx != STORE_HOME)
    {
        /* Extract the "minimum" price */
        x = obj->askprice;

        /* Make sure the player can afford it */
        if (player->au < x)
            colour = curs_attrs[CURS_UNKNOWN][(int)cursor];

        /* Actually draw the price */
        if (tval_can_have_charges(obj) && (obj->number > 1))
            strnfmt(out_val, sizeof(out_val), "%9d avg", x);
        else
            strnfmt(out_val, sizeof(out_val), "%9d    ", x);

        /* The price is not available if 0 (the item is not for sale) */
        if (x == 0)
            c_put_str(COLOUR_SLATE, "N/A", row, ctx->scr_places_x[LOC_PRICE] + 5);
        else
            c_put_str(colour, out_val, row, ctx->scr_places_x[LOC_PRICE]);
    }
}


/*
 * Display store (after clearing screen)
 */
static void store_display_frame(struct store_context *ctx)
{
    char buf[MSG_LEN];
    struct store *store = ctx->store;
    struct owner *proprietor = store->owner;
    unsigned int y;

    /* Clear screen (except top line) */
    for (y = 1; y < ctx->scr_places_y[LOC_HELP_PROMPT]; y++)
        Term_erase(0, y, 255);

    /* The "Home" is special */
    if (store->sidx == STORE_HOME)
    {
        /* Put the owner name */
        put_str("Your Home", ctx->scr_places_y[LOC_OWNER], 1);

        /* Label the object descriptions */
        put_str("Home Inventory", ctx->scr_places_y[LOC_HEADER], 1);

        /* Show weight header */
        put_str("Weight", ctx->scr_places_y[LOC_HEADER], ctx->scr_places_x[LOC_WEIGHT] + 2);
    }
    else
    {
        /* A player owned store */
        if (store->sidx == STORE_PLAYER)
        {
            /* Put the owner name */
            strnfmt(buf, sizeof(buf), "%s's %s", proprietor->name, store->name);
            put_str(buf, ctx->scr_places_y[LOC_OWNER], 1);
        }

        /* Normal stores */
        else
        {
            const char *store_name = store->name;
            const char *owner_name = proprietor->name;

            /* Put the owner name */
            put_str(owner_name, ctx->scr_places_y[LOC_OWNER], 1);

            /* Show the max price in the store (above prices) */
            strnfmt(buf, sizeof(buf), "%s (%d)", store_name, proprietor->max_cost);
            prt(buf, ctx->scr_places_y[LOC_OWNER], ctx->scr_places_x[LOC_OWNER] - strlen(buf));
        }

        /* Label the object descriptions */
        put_str("Store Inventory", ctx->scr_places_y[LOC_HEADER], 1);

        /* Showing weight label */
        put_str("Weight", ctx->scr_places_y[LOC_HEADER], ctx->scr_places_x[LOC_WEIGHT] + 2);

        /* Label the asking price (in stores) */
        put_str("Price", ctx->scr_places_y[LOC_HEADER], ctx->scr_places_x[LOC_PRICE] + 4);
    }
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
    text_out_c(COLOUR_WHITE, text, y, px);
}


static void text_end(int *py, int* px)
{
    Term_erase(*px, *py, 255);
    (*py)++;
    (*px) = 1;
}


/*
 * Display help.
 */
static void store_display_help(struct store_context *ctx)
{
    struct store *store = ctx->store;
    bool is_home = ((store->sidx == STORE_HOME)? true: false);
    int help_loc_y = ctx->scr_places_y[LOC_HELP_PROMPT];
    int help_loc_x = 1;
    unsigned int y;

    /* Clear */
    for (y = ctx->scr_places_y[LOC_HELP_CLEAR]; y < ctx->scr_places_y[LOC_HELP_PROMPT]; y++)
        Term_erase(0, y, 255);

    /* Display help */
    if (OPT(player, rogue_like_commands))
        text_out_c(COLOUR_L_GREEN, "x", help_loc_y, &help_loc_x);
    else
        text_out_c(COLOUR_L_GREEN, "l", help_loc_y, &help_loc_x);
    text_out(" examines and ", help_loc_y, &help_loc_x);
    text_out_c(COLOUR_L_GREEN, "p", help_loc_y, &help_loc_x);
    if (is_home)
        text_out(" picks up the selected item.", help_loc_y, &help_loc_x);
    else
        text_out(" purchases the selected item.", help_loc_y, &help_loc_x);
    if (store->sidx == STORE_XBM)
    {
        text_out(" ", help_loc_y, &help_loc_x);
        text_out_c(COLOUR_L_GREEN, "o", help_loc_y, &help_loc_x);
        text_out(" orders an item.", help_loc_y, &help_loc_x);
    }
    text_end(&help_loc_y, &help_loc_x);
    text_out_c(COLOUR_L_GREEN, "s", help_loc_y, &help_loc_x);
    if (OPT(player, birth_no_selling))
    {
        text_out(" gives an item to the store in return for its identification. Some wands",
            help_loc_y, &help_loc_x);
        text_end(&help_loc_y, &help_loc_x);
        text_out("and staves will also be recharged. ", help_loc_y, &help_loc_x);
        text_out_c(COLOUR_L_GREEN, "ESC", help_loc_y, &help_loc_x);
        text_out(" exits the building.", help_loc_y, &help_loc_x);
    }
    else
    {
        if (is_home)
            text_out(" drops an item from your inventory. ", help_loc_y, &help_loc_x);
        else
            text_out(" sells an item from your inventory. ", help_loc_y, &help_loc_x);
        text_out_c(COLOUR_L_GREEN, "ESC", help_loc_y, &help_loc_x);
        text_out(" exits the building.", help_loc_y, &help_loc_x);
        text_end(&help_loc_y, &help_loc_x);
    }
    text_end(&help_loc_y, &help_loc_x);
}


/*
 * Decides what parts of the store display to redraw.  Called on terminal
 * resizings and the redraw command.
 */
static void store_redraw(struct store_context *ctx)
{
    if (ctx->flags & (STORE_FRAME_CHANGE))
    {
        store_display_frame(ctx);

        if (ctx->flags & STORE_SHOW_HELP)
            store_display_help(ctx);
        else
            prt("Press '?' for help.", ctx->scr_places_y[LOC_HELP_PROMPT], 1);

        ctx->flags &= ~(STORE_FRAME_CHANGE);
    }

    if (ctx->flags & (STORE_GOLD_CHANGE))
    {
        prt(format("Gold Remaining: %9d", player->au),
            ctx->scr_places_y[LOC_AU], ctx->scr_places_x[LOC_AU]);
        ctx->flags &= ~(STORE_GOLD_CHANGE);
    }
}


/*
 * Determine if the current store will purchase the given object
 */
static bool store_will_buy_tester(struct player *p, const struct object *obj)
{
    return obj->info_xtra.sellable;
}


/*
 * Sell an item to the store
 */
static bool store_sell(struct store_context *ctx)
{
    int amt = 1;
    int get_mode = USE_EQUIP | USE_INVEN | USE_QUIVER;
    struct object *obj;
    item_tester tester = NULL;
    const char *reject = "You have nothing that I want.";
    const char *prompt = (OPT(player, birth_no_selling)? "Give which item? ": "Sell which item? ");
    ui_event ea = EVENT_ABORT;
    struct store *store = ctx->store;

    if (store->sidx == STORE_HOME)
        prompt = "Drop which item? ";
    else
    {
        tester = store_will_buy_tester;
        get_mode |= SHOW_PRICES;
    }

    /* Get an item */
    if (!get_item(&obj, prompt, reject, CMD_DROP, tester, get_mode)) return false;
    if (check_store_leave(false)) return false;

    /* Get a quantity (if number of sellable items is greater than 1) */
    /* Note: sale can always be aborted at the "Accept xxx gold?" prompt */
    if (obj->number > 1)
    {
        amt = get_quantity_ex(NULL, obj->number);

        /* Allow user abort */
        if (amt <= 0)
        {
            if (amt == -1) Term_event_push(&ea);
            return false;
        }
    }

    /* Wait for command to be processed by the server */
    store_command_wait = true;

    /* Tell the server */
    Send_store_sell(obj->oidx, amt);

    return true;
}


/*
 * Buy an object from a store
 */
static bool store_purchase(struct store_context *ctx, int item)
{
    struct object *obj = &ctx->list[item];
    char o_name[NORMAL_WID];
    int amt, num;
    ui_event ea = EVENT_ABORT;
    struct store *store = ctx->store;

    /* Clear all current messages */
    prt("", 0, 0);

    if (store->sidx != STORE_HOME)
    {
        /* Price of one */
        s32b price = obj->askprice;

        /* Check "shown" items */
        if (price == 0)
        {
            c_msg_print("Sorry, this item is not for sale.");
            return false;
        }

        /* Check if the player can afford any at all */
        if (player->au < price)
        {
            /* Tell the user */
            c_msg_print("You do not have enough gold for this item.");

            /* Abort now */
            return false;
        }
    }

    /* Work out how many the player can afford */
    amt = obj->info_xtra.max;

    /* Fail if there is no room */
    if (amt == 0)
    {
        c_msg_print("You cannot carry that many items.");
        return false;
    }

    /* Find the number of this item in the inventory */
    num = obj->info_xtra.owned;
    strnfmt(o_name, sizeof(o_name), "%s how many%s? (max %d) ",
        ((store->sidx == STORE_HOME)? "Take": "Buy"),
        (num? format(" (you have %d)", num): ""), amt);

    /* Get a quantity */
    amt = get_quantity_ex(o_name, amt);

    /* Allow user abort */
    if (amt <= 0)
    {
        if (amt == -1) Term_event_push(&ea);
        return false;
    }

    /* Wait for command to be processed by the server */
    store_command_wait = true;

    /* Tell the server */
    Send_store_purchase(item, amt);

    return true;
}


/*
 * Examine an item in a store
 */
static void store_examine(int item, bool describe)
{
    /* Tell the server */
    Send_store_examine(item, describe);
}


static void store_menu_set_selections(struct menu *menu)
{
    /* Roguelike */
    if (OPT(player, rogue_like_commands))
    {
        /* These two can't intersect! */
        menu->cmd_keys = "degiopsxD?|&";
        menu->selections = "abcfhlmnqrtuvwyzABCEFGHI";
    }

    /* Original */
    else
    {
        /* These two can't intersect! */
        menu->cmd_keys = "degilopsD?|&";
        menu->selections = "abcfhjkmnqrtuvwxyzABCEFG";
    }
}


static void store_menu_recalc(struct menu *m)
{
    struct store_context *ctx = menu_priv(m);

    menu_setpriv(m, ctx->store->stock_num, ctx);
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
    /* Hack -- no flush needed */
    prt("", 0, 0);

    /* Process the keycode */
    switch (kp.code)
    {
        case 'e':
        {
            do_cmd_equip();
            if (store_ctx) prt("Press '?' for help.", store_ctx->scr_places_y[LOC_HELP_PROMPT], 1);
            break;
        }

        case 'i':
        {
            do_cmd_inven();
            break;
        }

        case '|':
        {
            do_cmd_quiver();
            break;
        }

        default:
        {
            c_msg_print("That command does not work in stores.");
            return false;
        }
    }

    check_store_leave(true);
    return true;
}


/*
 * Select an item from the store's stock, and return the stock index
 */
static int store_get_stock(struct menu *m, int oid)
{
    ui_event e;
    int no_act = (m->flags & MN_NO_ACTION);

    /* Set a flag to make sure that we get the selection or escape without running the menu handler */
    m->flags |= MN_NO_ACTION;
    e = menu_select(m, 0, true);
    if (!no_act) m->flags &= ~MN_NO_ACTION;

    if (e.type == EVT_SELECT) return m->cursor;
    if (e.type == EVT_ESCAPE) return -1;

    /* If we do not have a new selection, just return the original item */
    return oid;
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


/*
 * Loop callback
 */
static void store_callback_begin(ui_event *cp)
{
    /* If we got a ^R key, cancel the wait (in case the server didn't respond...) */
    if ((cp->type == EVT_KBRD) && (cp->key.code == KTRL('R'))) store_command_wait = false;

    /* Wait for command to be processed by the server */
    cp->type = EVT_NONE;
    if (!store_command_wait) cp->type = EVT_DONE;
}


/*
 * Handle store menu input
 */
static bool store_menu_handle(struct menu *m, const ui_event *event, int oid)
{
    bool processed = true;
    struct store_context *ctx = menu_priv(m);
    struct store *store = ctx->store;

    /* Leave store */
    if (leave_store) return true;

    if (event->type == EVT_SELECT)
    {
        /* Nothing for now. */
        /* In future, maybe we want a display a list of what you can do. */
        return true;
    }
    else if (event->type == EVT_KBRD)
    {
        bool storechange = false;

        switch (event->key.code)
        {
            case 's':
            case 'd':
            {
                /* Paranoia: nothing to sell */
                if (store->sidx == STORE_PLAYER)
                    c_msg_print("That command does not work in this store.");
                else
                    storechange = store_sell(ctx);
                break;
            }

            case 'p':
            case 'g':
            {
                /* Paranoia: nothing to purchase */
                if (store->stock_num <= 0)
                {
                    switch (store->sidx)
                    {
                        case STORE_HOME: c_msg_print("Your home is empty."); break;
                        case STORE_PLAYER: c_msg_print("This player shop is empty."); break;
                        default: c_msg_print("I am currently out of stock."); break;
                    }
                }
                else
                {
                    /* Use the old way of purchasing items */
                    if (store->sidx != STORE_HOME)
                        prt("Purchase which item? (ESC to cancel, Enter to select)", 0, 0);
                    else
                        prt("Get which item? (ESC to cancel, Enter to select)", 0, 0);
                    oid = store_get_stock(m, oid);
                    prt("", 0, 0);
                    if (oid >= 0) storechange = store_purchase(ctx, oid);
                }
                break;
            }

            case 'l':
            case 'x':
            {
                /* Paranoia: nothing to examine */
                if (store->stock_num > 0)
                {
                    /* Use the old way of examining items */
                    prt("Examine which item? (ESC to cancel, Enter to select)", 0, 0);
                    oid = store_get_stock(m, oid);
                    prt("", 0, 0);
                    if (oid >= 0) store_examine(oid, false);
                }
                break;
            }

            case 'D':
            {
                /* Paranoia: nothing to describe */
                if (store->stock_num > 0)
                {
                    /* Use the old way of examining items */
                    prt("Describe which item? (ESC to cancel, Enter to select)", 0, 0);
                    oid = store_get_stock(m, oid);
                    prt("", 0, 0);
                    if (oid >= 0) store_examine(oid, true);
                }
                break;
            }

            case '?':
            {
                /* Toggle help */
                if (ctx->flags & STORE_SHOW_HELP)
                    ctx->flags &= ~(STORE_SHOW_HELP);
                else
                    ctx->flags |= STORE_SHOW_HELP;

                /* Redisplay */
                ctx->flags |= STORE_INIT_CHANGE;
                break;
            }

            case '&':
            {
                /* Hack -- redisplay */
                ctx->flags |= STORE_INIT_CHANGE;
                break;
            }

            case 'o':
            {
                /* Order an item */
                if (store->sidx == STORE_XBM)
                    store_order();
                else
                    c_msg_print("You cannot order from this store.");
                break;
            }

            default:
                processed = store_process_command_key(event->key);
        }

        /* Leave store */
        if (leave_store) return true;

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
        store_display_recalc(ctx);
        store_menu_recalc(m);
        store_redraw(ctx);

        return processed;
    }

    return false;
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


/* Init the store menu */
static void store_menu_init(struct store_context *ctx, struct store *store)
{
    struct menu *menu = &ctx->menu;

    ctx->store = store;
    ctx->flags = STORE_INIT_CHANGE;
    ctx->list = ctx->store->stock;

    /* Init the menu structure */
    menu_init(menu, MN_SKIN_SCROLL, &store_menu);
    menu_setpriv(menu, 0, ctx);

    /* Calculate the positions of things and draw */
    menu_layout(menu, &store_menu_region);
    store_menu_set_selections(menu);
    store_display_recalc(ctx);
    store_menu_recalc(menu);
    store_redraw(ctx);
}


/*
 * Enter a store, and interact with it.
 */
void store_enter(void)
{
    struct store *store = &current_store;
    struct store_context ctx;

    /* Save the screen */
    screen_save();

    /* We are "shopping" */
    store_ctx = &ctx;
    leave_store = false;

    /* Clear the top line */
    Term_erase(0, 0, 255);
    Term_fresh();

    store_menu_init(&ctx, store);

    /* Say a friendly hello. */
    if ((store->sidx != STORE_HOME) && (store->sidx != STORE_PLAYER))
        prt_welcome(store->owner);

    menu_select(&ctx.menu, 0, false);

    /* We are no longer "shopping" */
    store_ctx = false;
    leave_store = false;

    /* Restore the screen */
    screen_load(true);

    /* Redraw */
    player->upkeep->redraw |= (PR_EQUIP);

    /* Tell the server that we're outta here */
    Send_store_leave();
}


/*
 * Display player's gold
 */
void store_prt_gold(void)
{
    store_ctx->flags |= STORE_GOLD_CHANGE;
    store_redraw(store_ctx);
}


/*
 * Display store frame
 */
void store_prt_frame(void)
{
    store_ctx->flags |= STORE_FRAME_CHANGE;
    store_redraw(store_ctx);
}


void store_sell_accept(s32b price, s16b reset)
{
    char buf[NORMAL_WID];
    int res;
    ui_event ea = EVENT_ABORT;

    /* If the item was rejected, cancel the wait */
    if (price < 0)
    {
        store_command_wait = false;
        return;
    }

    /* Hack -- redisplay (unless selling a house) */
    if (store_ctx)
    {
        store_ctx->flags |= STORE_INIT_CHANGE;
        store_redraw(store_ctx);
        menu_refresh(&store_ctx->menu, false);
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
    else store_command_wait = false;
}


void store_purchase_end(void)
{
    /* Cancel wait */
    store_command_wait = false;
}    


void store_sell_end(void)
{
    /* Cancel wait */
    store_command_wait = false;
}        


void store_leave(void)
{
    ui_event ea = EVENT_ABORT;

    /* Leave store */
    store_command_wait = false;
    Term_event_push(&ea);
    leave_store = true;
}


bool check_store_leave(bool refresh)
{
    ui_event ea = EVENT_ABORT;

    if (leave_store) Term_event_push(&ea);
    else if (store_ctx)
    {
        /* Hack -- redisplay */
        store_ctx->flags |= STORE_INIT_CHANGE;
        if (refresh) Term_key_push('&');
    }
    return leave_store;
}
