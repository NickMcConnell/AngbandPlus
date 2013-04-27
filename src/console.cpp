// File: console.cpp
// Purpose: the game console code

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"

const int CONSOLE_SIZE = 64;

struct buffer_type {
    char str[100];
    buffer_type *next;
};

static buffer_type *head;
char *console[CONSOLE_SIZE];

/*
 * Initialize the console
 */
void init_console(void)
{
    for (int i = 0; i < CONSOLE_SIZE; i++) console[i] = NULL;
    head = new buffer_type;
    head->next = NULL;
    strcpy(head->str, "");
}


/*
 * Print to the console
 */
void console_print(char *str)
{
    int i;

    if (console[0]) delete[] console[0];
    for (i = 0; i < CONSOLE_SIZE-1; i++) {
        console[i] = console[i+1];
    }
    console[CONSOLE_SIZE-1] = new char[strlen(str)+1];
    strcpy(console[CONSOLE_SIZE-1], str);
}


/*
 * Print a formatted message to the console
 */
void console_format(char *fmt, ...)
{
    va_list vp;

    char buf[1024];

    /* Begin the Varargs Stuff */
    va_start(vp, fmt);

    /* Format the args, save the length */
    vstrnfmt(buf, 1024, fmt, vp);

    /* End the Varargs Stuff */
    va_end(vp);

    /* Display */
    console_print(buf);
}



/*
 * Draw the console
 */
void draw_console(void)
{
    int i;

    box(0, 0, 639, 159, COLOR_BLACK);
    box(0, 159, 639, 159, COLOR_WHITE);
    for (i = 0; i < 9; i++) {
        if (console[CONSOLE_SIZE-9+i]) {
            put_string(0, i*16, console[CONSOLE_SIZE-9+i], COLOR_WHITE);
        }
    }
    put_string(0, 9*16, format(">%s", cur_console_line), COLOR_WHITE);
}


/*
 * Interpret a console command.
 *
 * The command is passed as the argument.
 *
 * Note that console commands are case-dependent!
 *
 * XXX this needs support for a whole lot more commands.
 * XXX check whether stuff works with stores.
 * XXX do something about pile of streq()'s.
 * XXX infinite console recursion
 * XXX console cheating
 * XXX aliases
 * XXX +commands, -commands
 */
static bool interpret_console(char *cmd)
{
    if (streq(cmd, "")) {
        return TRUE;
    }
    else if (prefix(cmd, "bind ")) {
        char buf1[80], buf2[80], *c;

        // Get the part after that first space
        strcpy(buf1, cmd+5);

        // Search for a space
        c = strchr(buf1, ' ');

        // If no space, error
        if (!c) {
            console_format("Invalid bind command: %s", cmd);
        }

        else {
            // Get the second string
            strcpy(buf2, c+1);

            // Truncate the first string
            *c = 0;

            // Bind
            do_cmd_bind(buf1, buf2);
        }
        return TRUE;
    }
    else if (prefix(cmd, "wiz_")) {
        if (!wizard) console_print("You must be in wizard mode to use wizard commands.");
        else {
            if (streq(cmd, "wiz_cure")) {
                do_cmd_wiz_cure_all();
            }
            else if (streq(cmd, "wiz_create")) {
                wiz_create_item();
            }
            else if (streq(cmd, "wiz_item")) {
                do_cmd_wiz_play();
            }
            else if (streq(cmd, "wiz_detect")) {
                detection();
            }
            else if (streq(cmd, "wiz_edit")) {
                do_cmd_wiz_change();
            }
            else if (streq(cmd, "wiz_exp")) {
                p_ptr->gain_exp(p_ptr->GetExp() + 1);
            }
            else if (prefix(cmd, "wiz_gold ")) {
                long gold;
                sscanf(cmd, "wiz_gold %ld", &gold);
                if (gold < 0) gold = 0;
                p_ptr->SetGold(gold);
            }
            else if (streq(cmd, "wiz_ident")) {
                ident_spell();
            }
            else if (streq(cmd, "wiz_full_ident")) {
                identify_fully();
            }
            else if (streq(cmd, "wiz_lite")) {
                wiz_lite();
            }
            else if (streq(cmd, "wiz_zap")) {
                do_cmd_wiz_zap();
            }
            else if (streq(cmd, "wiz_good")) {
                acquirement(p_ptr->GetY(), p_ptr->GetX(), 1, FALSE);
            }
            else if (streq(cmd, "wiz_vgood")) {
                acquirement(p_ptr->GetY(), p_ptr->GetX(), 1, TRUE);
            }
            else if (streq(cmd, "wiz_tele")) {
                teleport_player(100);
            }
            else if (streq(cmd, "wiz_phase")) {
                teleport_player(10);
            }
            else if (streq(cmd, "wiz_map")) {
                map_area();
            }
            else if (streq(cmd, "wiz_learn")) {
                do_cmd_wiz_learn();
            }
            else if (streq(cmd, "wiz_summon")) {
                do_cmd_wiz_summon();
            }
            else if (streq(cmd, "wiz_jump")) {
                do_cmd_wiz_jump();
            }
            else if (streq(cmd, "wiz_know")) {
                self_knowledge();
            }
            else if (streq(cmd, "wiz_named")) {
                do_cmd_wiz_named();
            }
            else {
                console_format("Unknown wizard command \"%s\"", cmd);
            }
        }
        return TRUE;
    }
    else if (streq(cmd, "wizard")) {
        p_ptr->SetNoScore(TRUE);
        if (wizard) {
            wizard = FALSE;
            console_print("Wizard mode off.");
        }
        else {
            wizard = TRUE;
            console_print("Wizard mode on.");
        }

        // Update monsters
        p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);

        return TRUE;
    }
    else if (streq(cmd, "version")) {
        do_cmd_version();
        return TRUE;
    }
    else if (streq(cmd, "artifacts")) {
        do_cmd_check_artifacts();
        return TRUE;
    }
    else if (streq(cmd, "uniques")) {
        do_cmd_check_uniques();
        return TRUE;
    }
    else if (streq(cmd, "items")) {
        do_cmd_identified_objs();
        return TRUE;
    }
    else if (streq(cmd, "help")) {
        do_cmd_help();
        reset_timer();
        return TRUE;
    }
    else if (streq(cmd, "screen_dump")) {
        dump_screen();
        return TRUE;
    }
    else if (streq(cmd, "suicide")) {
        do_cmd_suicide();
        return TRUE;
    }
    else if (streq(cmd, "save_quit")) {
        alive = FALSE;
        return TRUE;
    }
    else if (streq(cmd, "options")) {
        do_cmd_options();
        return TRUE;
    }
    else if (streq(cmd, "character")) {
        if (show_stuff != SHOW_CHARACTER) show_stuff = SHOW_CHARACTER;
        else show_stuff = SHOW_JUST_MAP;
        return TRUE;
    }
    else if (streq(cmd, "query_symbol")) {
        do_cmd_query_symbol();
        return TRUE;
    }
    else if (streq(cmd, "wield")) {
        return do_cmd_wield();
    }
    else if (streq(cmd, "takeoff")) {
        return do_cmd_takeoff();
    }
    else if (streq(cmd, "drop")) {
        return do_cmd_drop();
    }
    else if (streq(cmd, "destroy")) {
        return do_cmd_destroy();
    }
    else if (streq(cmd, "equipment")) {
        if (show_stuff != SHOW_EQUIP) show_stuff = SHOW_EQUIP;
        else show_stuff = SHOW_JUST_MAP;
        return TRUE;
    }
    else if (streq(cmd, "inventory")) {
        if (show_stuff != SHOW_INVEN) show_stuff = SHOW_INVEN;
        else show_stuff = SHOW_JUST_MAP;
        return TRUE;
    }
    else if (streq(cmd, "search")) {
        return do_cmd_search();
    }
    else if (streq(cmd, "enter_store")) {
        do_cmd_store();
        return TRUE;
    }
    else if (streq(cmd, "observe")) {
        do_cmd_observe();
        return TRUE;
    }
    else if (streq(cmd, "study")) {
        return do_cmd_study();
    }
    else if (streq(cmd, "browse")) {
        do_cmd_browse();
        return TRUE;
    }
    else if (streq(cmd, "magic")) {
        do_cmd_magic();
        return TRUE;
    }
    else if (streq(cmd, "activate")) {
        do_cmd_activate();
        return TRUE;
    }
    else if (streq(cmd, "fire")) {
        do_cmd_fire();
        return TRUE;
    }
    else if (streq(cmd, "zap")) {
        do_cmd_zap();
        return TRUE;
    }
    else if (streq(cmd, "use")) {
        return do_cmd_use();
    }
    else if (streq(cmd, "pause")) {
        do_cmd_pause();
        return TRUE;
    }
    else if (streq(cmd, "show_console")) {
        if (show_stuff != SHOW_CONSOLE) show_stuff = SHOW_CONSOLE;
        else show_stuff = SHOW_JUST_MAP;
        return TRUE;
    }
    else if (streq(cmd, "fps_start")) {
        frames = 0;
        fps_ticker = 0;
        return TRUE;
    }
    else if (streq(cmd, "fps_end")) {
        console_format("Frames: %d  Ticks: %d  FPS: %.1f", frames, fps_ticker,
            ((float)frames)/((float)fps_ticker)*(18.2*8.0));
        return TRUE;
    }
    else if (streq(cmd, "testmons")) {
        do_cmd_test_monsters();
        return TRUE;
    }
    else if (streq(cmd, "testobjs")) {
        do_cmd_test_objects();
        return TRUE;
    }
    else if (prefix(cmd, "walk ")) {
        int dir;
        sscanf(cmd, "walk %d", &dir);
        if ((dir >= 1) && (dir <= 9) && (dir != 5)) {
            return do_cmd_walk(dir);
        }
        else {
            msg_format("Invalid walk command: %s.", cmd);
            return TRUE;
        }
    }
    else if (streq(cmd, "minimap")) {
        show_minimap = !show_minimap;
        return TRUE;
    }
    else if (streq(cmd, "rest")) {
        if (!resting) {
            if (!p_ptr->isBusy()) {
                resting = TRUE;
                p_ptr->action = 0;
                return TRUE;
            }
            else return FALSE;
        }
        else {
            resting = FALSE;
            return TRUE;
        }
    }
    else {
        console_format("Console error: unknown command \"%s\"", cmd);
        return TRUE;
    }
}

void buffer_console(char *cmd)
{
    // Try once right away
    if (interpret_console(cmd)) return;

    // Couldn't execute?
    buffer_type *b = head;
    while (b->next) {
        b = b->next;
    }
    b->next = new buffer_type;
    b = b->next;
    strcpy(b->str, cmd);
    b->next = NULL;
}

void flush_console_buffer(void)
{
    // Try each command in turn
    buffer_type *b = head;
    while (b->next) {
        if (interpret_console(b->next->str)) {
            buffer_type *c = b->next;
            b->next = c->next;
            delete c;
            if (!b->next) break;
        }
        b = b->next;
    }
}