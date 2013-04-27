// File: files.c
// Purpose: code dealing with files (and death)

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"
/*
 * Print number with header at given row, column
 */
static void prt_num(char *header, int num, int row, int col, byte color)
{
    int len = strlen(header);
    char out_val[32];

    put_string(col*8, row*16, header, COLOR_WHITE);
    sprintf(out_val, "%6ld", (long)num);
    put_string((col+len+3)*8, row*16, out_val, color);
}


/*
 * Prints the following information on the screen.
 */
static void display_player_middle(void)
{
    int show_tohit = p_ptr->get_dis_to_h();
    int show_todam = p_ptr->get_dis_to_d();
    CItem *i_ptr = &inventory[INVEN_WIELD];
    char buf[80];

    /* Hack -- add in weapon info if known */
    if (i_ptr->isKnown()) show_tohit += i_ptr->GetToH();
    if (i_ptr->isKnown()) show_todam += i_ptr->GetToD();

    // Dump the bonuses to hit/dam
    prt_num("+ To Hit    ", show_tohit, 9, 1, COLOR_LT_BLUE);
    prt_num("+ To Damage ", show_todam, 10, 1, COLOR_LT_BLUE);

    // Dump the armor class stuff
    prt_num("+ To AC     ", p_ptr->get_dis_to_a(), 11, 1, COLOR_LT_BLUE);
    prt_num("  Base AC   ", p_ptr->get_dis_ac(), 12, 1, COLOR_LT_BLUE);
    prt_num("  Total AC  ", p_ptr->GetTotalDisAC(), 13, 1, COLOR_LT_BLUE);

    // Print some headers
    put_text(28*8, 9*16, "Level", COLOR_WHITE, FONT_BOLD);
    put_text(28*8, 10*16, "Experience", COLOR_WHITE, FONT_BOLD);
    put_text(28*8, 11*16, "Max Exp", COLOR_WHITE, FONT_BOLD);
    put_text(28*8, 12*16, "Exp to Adv.", COLOR_WHITE, FONT_BOLD);
    put_text(28*8, 13*16, "Gold", COLOR_WHITE, FONT_BOLD);

    sprintf(buf, "%d", p_ptr->GetLev());
    put_string(48*8-8*strlen(buf), 9*16, buf, COLOR_LT_GREEN);

    sprintf(buf, "%d", p_ptr->GetExp());
    if (p_ptr->GetExp() >= p_ptr->GetMaxExp()) {
        put_string(48*8-8*strlen(buf), 10*16, buf, COLOR_LT_GREEN);
    }
    else {
        put_string(48*8-8*strlen(buf), 10*16, buf, COLOR_YELLOW);
    }

    sprintf(buf, "%d", p_ptr->GetMaxExp());
    put_string(48*8-8*strlen(buf), 11*16, buf, COLOR_LT_GREEN);

    if (p_ptr->GetLev() >= PY_MAX_LEVEL) {
        strcpy(buf, "*****");
    }
    else {
        sprintf(buf, "%d",
            (s32b)(player_exp[p_ptr->GetLev() - 1] * p_ptr->GetExpFact() / 100L));
    }
    put_string(48*8-8*strlen(buf), 12*16, buf, COLOR_LT_GREEN);

    sprintf(buf, "%d", p_ptr->GetGold());
    put_string(48*8-8*strlen(buf), 13*16, buf, COLOR_LT_GREEN);

    // HP/SP headers    
    put_text(52*8, 9*16, "Max Hit Points", COLOR_WHITE, FONT_BOLD);
    put_text(52*8, 10*16, "Cur Hit Points", COLOR_WHITE, FONT_BOLD);
    put_text(52*8, 11*16, "Max SP (Mana)", COLOR_WHITE, FONT_BOLD);
    put_text(52*8, 12*16, "Cur SP (Mana)", COLOR_WHITE, FONT_BOLD);

    sprintf(buf, "%d", p_ptr->GetMHP());
    put_string(76*8-8*strlen(buf), 9*16, buf, COLOR_LT_GREEN);

    sprintf(buf, "%d", p_ptr->GetCHP());
    put_string(76*8-8*strlen(buf), 10*16, buf,
        get_pct_color(p_ptr->GetCHP(), p_ptr->GetMHP()));

    sprintf(buf, "%d", p_ptr->GetMSP());
    put_string(76*8-8*strlen(buf), 11*16, buf, COLOR_LT_GREEN);

    sprintf(buf, "%d", p_ptr->GetCSP());
    put_string(76*8-8*strlen(buf), 12*16, buf,
        get_pct_color(p_ptr->GetCSP(), p_ptr->GetMSP()));
}


/*
 * Returns a "rating" of x depending on y
 */
static char *likert(int x, int y, byte *color)
{
    /* Paranoia */
    if (y <= 0) y = 1;

    /* Negative value */
    if (x < 0) {
        *color = COLOR_RED;
        return "Very Bad";
    }

    /* Analyze the value */
    switch (x / y) {
        case 0:
        case 1:
            *color = COLOR_RED;
            return "Bad";
        case 2:
            *color = COLOR_ORANGE;
            return "Poor";
        case 3:
        case 4:
            *color = COLOR_YELLOW;
            return "Fair";
        case 5:
            *color = COLOR_YELLOW;
            return "Good";
        case 6:
            *color = COLOR_YELLOW;
            return "Very Good";
        case 7:
        case 8:
            *color = COLOR_LT_GREEN;
            return "Excellent";
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
            *color = COLOR_LT_GREEN;
            return "Superb";
        case 14:
        case 15:
        case 16:
        case 17:
            *color = COLOR_LT_GREEN;
            return "Heroic";
        default:
            *color = COLOR_LT_GREEN;
            return "Legendary";
    }
}


/*
 * Prints ratings on certain abilities
 */
static void display_player_various(void)
{
    int tmp;
    int xthn, xthb, xfos, xsrh;
    int xdis, xdev, xsav, xstl;
    char *desc;
    byte likert_color;

    CItem *i_ptr;


    /* Fighting Skill (with current weapon) */
    i_ptr = &inventory[INVEN_WIELD];
    tmp = p_ptr->get_to_h() + i_ptr->GetToH();
    xthn = p_ptr->GetSkill(SKILL_THN) + tmp*BTH_PLUS_ADJ;

    /* Shooting Skill (with current bow and normal missile) */
    i_ptr = &inventory[INVEN_BOW];
    tmp = p_ptr->get_to_h() + i_ptr->GetToH();
    xthb = p_ptr->GetSkill(SKILL_THB) + tmp*BTH_PLUS_ADJ;

    /* Basic abilities */
    xdis = p_ptr->GetSkill(SKILL_DIS);
    xdev = p_ptr->GetSkill(SKILL_DEV);
    xsav = p_ptr->GetSkill(SKILL_SAV);
    xstl = p_ptr->GetSkill(SKILL_STL);
    xsrh = p_ptr->GetSkill(SKILL_SRH);
    xfos = p_ptr->GetSkill(SKILL_FOS);


    put_text(1*8, 16*16, "Fighting", COLOR_WHITE, FONT_BOLD);
    desc = likert(xthn, 12, &likert_color);
    put_text(15*8, 16*16, desc, likert_color, FONT_BOLD);

    put_text(1*8, 17*16, "Bows/Throw", COLOR_WHITE, FONT_BOLD);
    desc = likert(xthb, 12, &likert_color);
    put_text(15*8, 17*16, desc, likert_color, FONT_BOLD);

    put_text(1*8, 18*16, "Saving Throw", COLOR_WHITE, FONT_BOLD);
    desc = likert(xsav, 6, &likert_color);
    put_text(15*8, 18*16, desc, likert_color, FONT_BOLD);

    put_text(1*8, 19*16, "Stealth", COLOR_WHITE, FONT_BOLD);
    desc = likert(xstl, 1, &likert_color);
    put_text(15*8, 19*16, desc, likert_color, FONT_BOLD);


    put_text(28*8, 16*16, "Perception", COLOR_WHITE, FONT_BOLD);
    desc = likert(xfos, 6, &likert_color);
    put_text(42*8, 16*16, desc, likert_color, FONT_BOLD);

    put_text(28*8, 17*16, "Searching", COLOR_WHITE, FONT_BOLD);
    desc = likert(xsrh, 6, &likert_color);
    put_text(42*8, 17*16, desc, likert_color, FONT_BOLD);

    put_text(28*8, 18*16, "Disarming", COLOR_WHITE, FONT_BOLD);
    desc = likert(xdis, 8, &likert_color);
    put_text(42*8, 18*16, desc, likert_color, FONT_BOLD);

    put_text(28*8, 19*16, "Magic Device", COLOR_WHITE, FONT_BOLD);
    desc = likert(xdev, 6, &likert_color);
    put_text(42*8, 19*16, desc, likert_color, FONT_BOLD);


    put_text(55*8, 16*16, "Blows/Round", COLOR_WHITE, FONT_BOLD);
    put_string(69*8, 16*16, format("%d", p_ptr->get_num_blow()), COLOR_WHITE);

    put_text(55*8, 17*16, "Shots/Round", COLOR_WHITE, FONT_BOLD);
    put_string(69*8, 17*16, format("%d", p_ptr->get_num_fire()), COLOR_WHITE);

    put_text(55*8, 19*16, "Infra-Vision", COLOR_WHITE, FONT_BOLD);
    put_string(69*8, 19*16, format("%d ft", p_ptr->get_see_infra() * 10),
        COLOR_WHITE);
}


/*
 * Display the character on the screen
 *
 * The top two and bottom two lines are left blank.
 */
void display_player(void)
{
    int i, bonus;
    char buf[80];

    /* Clear the screen */
    blank_screen(COLOR_BLACK);

    // Name, Sex, Race, Class
    put_text(8, 2*16, "Name", COLOR_WHITE, FONT_BOLD);
    put_text(8, 3*16, "Sex", COLOR_WHITE, FONT_BOLD);
    put_text(8, 4*16, "Race", COLOR_WHITE, FONT_BOLD);
    put_text(8, 5*16, "Class", COLOR_WHITE, FONT_BOLD);

    put_text(15*8, 2*16, player_name, COLOR_LT_BLUE, FONT_BOLD);
    strcpy(buf,(p_ptr->GetMale()) ? "Male" : "Female");
    put_text(15*8, 3*16, buf, COLOR_LT_BLUE, FONT_BOLD);
    put_text(15*8, 4*16, p_ptr->GetRaceTitle(), COLOR_LT_BLUE, FONT_BOLD);
    put_text(15*8, 5*16, p_ptr->GetClassTitle(), COLOR_LT_BLUE, FONT_BOLD);
    
    put_string(39*8, 1*16, "Base", COLOR_WHITE);
    put_string(45*8, 1*16, "Race", COLOR_WHITE);
    put_string(50*8, 1*16, "Class", COLOR_WHITE);
    put_string(56*8, 1*16, "Equip", COLOR_WHITE);
    put_string(64*8, 1*16, "Total", COLOR_WHITE);

    /* Display the stats */
    for (i = 0; i < 6; i++) {
        // Get the racial bonus to the stat and display it
        bonus = rp_ptr->r_adj[i];
        sprintf(buf, "%s%d", (bonus > 0) ? "+" : "", bonus);
        put_string(48*8-8*strlen(buf), (2+i)*16, buf, COLOR_LT_GREEN);

        // Get the class bonus to the stat and display it
        bonus = cp_ptr->c_adj[i];
        sprintf(buf, "%s%d", (bonus > 0) ? "+" : "", bonus);
        put_string(54*8-8*strlen(buf), (2+i)*16, buf, COLOR_LT_GREEN);
        
        // Get the eq bonus to the stat and display it
        bonus = p_ptr->GetStatAdd(i) - cp_ptr->c_adj[i] - rp_ptr->r_adj[i];
        sprintf(buf, "%s%d", (bonus > 0) ? "+" : "", bonus);
        put_string(60*8-8*strlen(buf), (2+i)*16, buf, COLOR_LT_GREEN);

        /* Special treatment of "injured" stats */
        if (p_ptr->GetStatCur(i) < p_ptr->GetStatMax(i)) {
            // Use lowercase stat name
            put_string(32*8, (2+i)*16, stat_names_reduced[i], COLOR_WHITE);

            // Obtain and display the current stat (unmodified)
            cnv_stat(p_ptr->GetStatCur(i), buf);
            put_string(37*8, (2+i)*16, buf, COLOR_YELLOW);

            // Obtain and display the current stat (modified)
            cnv_stat(p_ptr->GetStatUse(i), buf);
            put_string(63*8, (2+i)*16, buf, COLOR_YELLOW);

            // Obtain and display the current stat (modified)
            cnv_stat(p_ptr->GetStatTop(i), buf);
            put_string(70*8, (2+i)*16, buf, COLOR_LT_GREEN);
        }

        /* Normal treatment of "normal" stats */
        else {
            /* Assume uppercase stat name */
            put_string(32*8, (2+i)*16, stat_names[i], COLOR_WHITE);

            // Obtain and display the current stat (unmodified)
            cnv_stat(p_ptr->GetStatCur(i), buf);
            put_string(37*8, (2+i)*16, buf, COLOR_LT_GREEN);

            // Obtain and display the current stat (modified)
            cnv_stat(p_ptr->GetStatUse(i), buf);
            put_string(63*8, (2+i)*16, buf, COLOR_LT_GREEN);
        }
    }

    /* Extra info */
    display_player_middle();

    put_text(28*8, 15*16, "Miscellaneous Abilities", COLOR_WHITE, FONT_BOLD);
    display_player_various();
}


/*
 * Recursive "help file" perusal.  Return FALSE on "ESCAPE".
 *
 * XXX XXX XXX Consider using a temporary file.
 */
static void do_cmd_help_aux_old(char *name, char *what)
{
    int i, k;

    int line = 0;

    /* Number of "real" lines passed by */
    int next = 0;

    /* Number of "real" lines in the file */
    int size = 0;

    /* Backup value for "line" */
    int back = 0;

    /* Current help file */
    FILE *fff = NULL;

    /* Find this string (if any) */
    char *find = NULL;

    /* Hold a string to find */
    char finder[128];

    /* Describe this thing */
    char caption[128];

    /* Path buffer */
    char path[1024];

    /* General buffer */
    char buf[1024];


    /* Wipe finder */
    strcpy(finder, "");

    /* Wipe caption */
    strcpy(caption, "");


    /* Hack XXX XXX XXX */
    if (what) {
        /* Caption */
        strcpy(caption, what);

        /* Access the "file" */
        strcpy(path, name);

        /* Open */
        fff = my_fopen(path, "r");
    }

    // Look in "help"
    if (!fff) {
        // Access the "help" file
        strcpy(path, "dat/help/");
        strcat(path, name);

        // Open the file
        fff = my_fopen(path, "r");

        // Caption
        sprintf(caption, "Help file '%s'", name);
    }

    /* Oops */
    if (!fff) {
        /* Message */
        quit(format("Cannot open '%s'.", name));

        /* Oops */
        return;
    }


    // Pre-Parse the file
    while (TRUE) {
        // Read a line or stop
        if (my_fgets(fff, buf, 1024)) break;

        /* Count the "real" lines */
        next++;
    }

    /* Save the number of "real" lines */
    size = next;



    /* Display the file */
    while (TRUE) {
        /* Clear the screen */
        blank_screen(COLOR_BLACK);


        /* Restart when necessary */
        if (line >= size) line = 0;


        /* Re-open the file if needed */
        if (next > line) {
            /* Close it */
            fclose(fff);

            /* Hack -- Re-Open the file */
            fff = my_fopen(path, "r");

            /* Oops */
            if (!fff) return;

            /* File has been restarted */
            next = 0;
        }

        /* Skip lines if needed */
        for ( ; next < line; next++) {
            /* Skip a line */
            if (my_fgets(fff, buf, 1024)) break;
        }


        /* Dump the next 20 lines of the file */
        for (i = 0; i < 20; ) {
            /* Hack -- track the "first" line */
            if (!i) line = next;

            /* Get a line of the file or stop */
            if (my_fgets(fff, buf, 1024)) break;

            /* Count the "real" lines */
            next++;

            /* Hack -- keep searching */
            if (find && !i && !strstr(buf, find)) continue;

            /* Hack -- stop searching */
            find = NULL;

            /* Dump the lines */
            put_string(0, (i+2)*16, buf, COLOR_WHITE);

            /* Count the printed lines */
            i++;
        }

        /* Hack -- failed search */
        if (find) {
            bell();
            line = back;
            find = NULL;
            continue;
        }


        /* Show a general "title" */
        put_string(0, 0, format("[%s, Line %d/%d", caption, line, size),
            COLOR_WHITE);


        /* Prompt -- small files */
        if (size <= 20) {
            /* Wait for it */
            put_string(0, 23*16, "[Press ESC to exit.]", COLOR_WHITE);
        }

        /* Prompt -- large files */
        else {
            // Wait for it
            put_string(0, 23*16, "[Press Return, Space, -, /, or ESC to exit.]",
                COLOR_WHITE);
        }

        /* Get a keypress */
        screen_refresh();
        k = scan_inkey();
        k = convert(k, get_shift(), get_capslock());

        /* Hack -- return to last screen */
        if (k == '?') break;

        /* Hack -- try searching */
        if (k == '/') {
            box(0, 23*16, 639, 23*16+15, COLOR_BLACK);
            put_string(0, 23*16, "Find: ", COLOR_WHITE);
            if (askfor_aux(finder, 80, 6, 23)) {
                find = finder;
                back = line;
                line = line + 1;
            }
        }

        /* Hack -- go to a specific line */
        if (k == '#') {
            char tmp[80];
            box(0, 23*16, 639, 23*16+15, COLOR_BLACK);
            put_string(0, 23*16, "Goto Line: ", COLOR_WHITE);
            strcpy(tmp, "0");
            if (askfor_aux(tmp, 80, 11, 23)) {
                line = atoi(tmp);
            }
        }

        /* Hack -- Allow backing up */
        if (k == '-') {
            line = line - 10;
            if (line < 0) line = 0;
        }

        /* Hack -- Advance a single line */
        if ((k == '\n') || (k == '\r')) {
            line = line + 1;
        }

        /* Advance one page */
        if (k == ' ') {
            line = line + 20;
        }

        /* Exit on escape */
        if (k == ESCAPE) break;
    }

    // Close the file and return
    fclose(fff);
    return;
}


/*
 * Recursive help file perusal.
 *
 * XXX XXX XXX Consider using a temporary file.
 */
const int HELP_FGCOLOR = COLOR_BLACK;
const int HELP_X1 = 53;
const int HELP_Y1 = 50;
const int HELP_X2 = 586;
const int HELP_Y2 = 429;
const int HELP_LINES = 21;
static void do_cmd_help_aux(char *name)
{
    // XXX title for help
    FILE *fff;
    char temp[1024], title[80], *buf, *p, *q, c;
    int words, line_num = 0, i, k, cur_x, cur_y, cur_link;
    bool indent, bold, link, word_now = FALSE;
    char links[9][80];

    // Get the path
    sprintf(temp, "dat/help/%s", name);

    // Open
    fff = fopen(temp, "rt");

    // Does not exist?
    if (!fff) {
        quit(format("Cannot open '%s'.", name));
        return;
    }

    // Get the title
    fgets(title, 80, fff);
    title[strlen(title)-1] = 0;

    // Create a giant buffer to hold its words
    buf = new char[100000];
    strcpy(buf, "");
    words = 0;

    // Read words in
    p = buf;
    for (;;) {
        c = fgetc(fff);
        if (feof(fff)) break;
        if (isspace(c)) {
            if (word_now) {
                *p++ = ' ';
                word_now = FALSE;
            }
        }
        else {
            if (word_now) {
                *p++ = c;
            }
            else {
                words++;
                *p++ = c;
                word_now = TRUE;
            }
        }
    }
    *p = 0;

    // Close the file
    fclose(fff);


    // Display the file
    while (TRUE) {
        // Draw a window
        draw_window(HELP_X1-3, HELP_Y1, HELP_X2+3, HELP_Y2, title);

        // Get ready
        indent = bold = link = FALSE;
        cur_x = HELP_X1;
        cur_y = 0;
        cur_link = 0;

        // Display
        p = buf;
        for (i = 0; i < words; i++) {
            // Get the word into temp
            q = temp;
            while (*p != ' ') *q++ = *p++;
            *q = 0;
            p++;

            // Is the word a control word?
            if (temp[0] == '%') {
                // Enable bold
                if (streq(temp, "%b+")) {
                    bold = TRUE;
                }

                // Disable bold
                else if (streq(temp, "%b-")) {
                    bold = FALSE;
                }

                // Enable indentation
                else if (streq(temp, "%indent+")) {
                    indent = TRUE;
                    if (cur_x == HELP_X1) cur_x = HELP_X1+50;
                }

                // Disable indentation
                else if (streq(temp, "%indent-")) {
                    indent = FALSE;
                    if (cur_x == HELP_X1+50) cur_x = HELP_X1;
                }

                // End link
                else if (streq(temp, "%l-")) {
                    link = FALSE;
                }

                // Line break
                else if (streq(temp, "%br")) {
                    cur_x = indent ? HELP_X1+50 : HELP_X1;
                    cur_y++;
                }

                // Start link
                else if (prefix(temp, "%l+")) {
                    link = TRUE;
                    if (cur_link == 9) quit("Too many links.");
                    strcpy(links[cur_link], temp+4);
                    cur_link++;
                }

                // Unknown
                else {
                    quit(format("Unknown control word: %s", temp));
                }
            }

            // It's a regular word
            else {
                int width;

                // Add a space
                strcat(temp, " ");
                
                // Get width
                width = string_width(temp, bold ? FONT_BOLD : FONT_REGULAR);

                // Does it fit?
                if (cur_x+width <= HELP_X2) {
                    if ((cur_y >= line_num) && (cur_y <= line_num+HELP_LINES-1)) {
                        put_text(cur_x, (cur_y-line_num)*16+HELP_Y1+22, temp,
                            HELP_FGCOLOR,
                            bold ? FONT_BOLD : FONT_REGULAR);
                    }
                    cur_x += width;
                }
                else {
                    cur_x = indent ? HELP_X1+50 : HELP_X1;
                    cur_y++;
                    if ((cur_y >= line_num) && (cur_y <= line_num+HELP_LINES-1)) {
                        put_text(cur_x, (cur_y-line_num)*16+HELP_Y1+22, temp,
                            HELP_FGCOLOR,
                            bold ? FONT_BOLD : FONT_REGULAR);
                    }
                    cur_x += width;
                }
            }
        }


        // Prompt
        put_text_format(320, HELP_Y2-17, "[Press ESC to exit]", HELP_FGCOLOR, FONT_BOLD,
            JUST_CENTER);

        // Other info
        if (cur_y <= HELP_LINES-1) {
            put_text(HELP_X1, HELP_Y2-17, "Entire file shown.", HELP_FGCOLOR, FONT_BOLD);
        }
        else {
            put_text(HELP_X1, HELP_Y2-17,
                format("%d%% (lines %d-%d of %d)", (line_num*100)/(cur_y-(HELP_LINES-1)),
                line_num, line_num+HELP_LINES-1, cur_y), HELP_FGCOLOR, FONT_BOLD);
        }

        // Get a keypress
        screen_refresh();
        k = scan_inkey();

        // Scroll up/down
        if (k == KEY_UP) line_num--;
        if (k == KEY_DOWN) line_num++;
        if (k == KEY_PGUP) line_num -= HELP_LINES;
        if (k == KEY_PGDN) line_num += HELP_LINES;
        if (k == KEY_HOME) line_num = 0;
        if (k == KEY_END) line_num = cur_y-(HELP_LINES-1);
        if (line_num > cur_y-(HELP_LINES-1)) line_num = cur_y-(HELP_LINES-1);
        if (line_num < 0) line_num = 0;

        // Access a link
        if ((k == KEY_1) && (cur_link >= 1)) do_cmd_help_aux(links[0]);
        if ((k == KEY_2) && (cur_link >= 2)) do_cmd_help_aux(links[1]);
        if ((k == KEY_3) && (cur_link >= 3)) do_cmd_help_aux(links[2]);
        if ((k == KEY_4) && (cur_link >= 4)) do_cmd_help_aux(links[3]);
        if ((k == KEY_5) && (cur_link >= 5)) do_cmd_help_aux(links[4]);
        if ((k == KEY_6) && (cur_link >= 6)) do_cmd_help_aux(links[5]);
        if ((k == KEY_7) && (cur_link >= 7)) do_cmd_help_aux(links[6]);
        if ((k == KEY_8) && (cur_link >= 8)) do_cmd_help_aux(links[7]);
        if ((k == KEY_9) && (cur_link >= 9)) do_cmd_help_aux(links[8]);

        // Exit on escape
        if (k == KEY_ESCAPE) break;
    }

    // Dealloc buffer
    delete[] buf;
}


/*
 * Peruse the On-Line-Help, starting at the given file.
 */
void do_cmd_help(void)
{
    byte *screen;

    /* Enter "icky" mode */
    character_icky = TRUE;

    /* Save the screen */
    screen = save_screen();

    /* Peruse the main help file */
    do_cmd_help_aux("help.hlp");

    /* Restore the screen */
    restore_screen(screen);
    delete[] screen;

    /* Leave "icky" mode */
    character_icky = FALSE;
}


/*
 * Hack -- display the contents of a file on the screen
 *
 * XXX XXX XXX Use this function for commands such as the
 * "examine object" command.
 */
errr show_file(char *name, char *what)
{
    byte *screen;

    /* Enter "icky" mode */
    character_icky = TRUE;

    /* Save the screen */
    screen = save_screen();

    /* Peruse the requested file */
    do_cmd_help_aux_old(name, what);

    /* Restore the screen */
    restore_screen(screen);
    delete[] screen;

    /* Leave "icky" mode */
    character_icky = FALSE;

    /* Success */
    return (0);
}




/*
 * Hack -- commit suicide
 */
void do_cmd_suicide(void)
{
    /* Verify Retirement */
    if (total_winner) {
        /* Verify */
        if (!get_check("Do you want to retire?")) return;

        // Cause of death
        strcpy(died_from, "old age");
    }

    /* Verify Suicide */
    else {
        /* Verify */
        if (!get_check("Do you really want to commit suicide?")) return;

        // Cause of death
        strcpy(died_from, "suicide");
    }

    /* Stop playing */
    alive = FALSE;

    /* Kill the player */
    death = TRUE;
}



/*
 * Hack -- Calculates the total number of points earned         -JWT-
 */
long total_points(void)
{
    return (p_ptr->GetMaxExp() + (100 * p_ptr->GetMaxDlv()));
}



/*
 * Centers a string within a 31 character string                -JWT-   
 */
static void center_string(char *buf, char *str)
{
    int i, j;

    /* Total length */
    i = strlen(str);

    /* Necessary border */
    j = 15 - i / 2;

    /* Mega-Hack */
    (void)sprintf(buf, "%*s%s%*s", j, "", str, 31 - i - j, "");
}


/*
 * Display a "tomb-stone"
 */
static void print_tomb()
{
    char *p, tmp[160], buf[1024];
    FILE *fp;
    time_t ct = time(NULL);


    /* Clear the screen */
    blank_screen(COLOR_BLACK);

    /* Access the "dead" file */
    strcpy(buf, "dat/dead.txt");

    /* Open the tombstone file */
    fp = my_fopen(buf, "r");

    /* Dump */
    if (fp) {
        int i = 0;

        /* Dump the file to the screen */
        while (0 == my_fgets(fp, buf, 1024)) {
            /* Display and advance */
            put_string(0, (i++)*16, buf, COLOR_WHITE);
        }

        /* Close */
        fclose(fp);
    }


    /* King or Queen */
    if (total_winner || (p_ptr->GetLev() > PY_MAX_LEVEL)) {
        p = "Magnificent";
    }

    /* Normal */
    else {
        p = p_ptr->GetClassTitle();
    }

    center_string(buf, player_name);
    put_string(11*8, 6*16, buf, COLOR_WHITE);

    center_string(buf, "the");
    put_string(11*8, 7*16, buf, COLOR_WHITE);

    center_string(buf, p);
    put_string(11*8, 8*16, buf, COLOR_WHITE);


    center_string(buf, p_ptr->GetClassTitle());
    put_string(11*8, 10*16, buf, COLOR_WHITE);

    sprintf(tmp, "Level: %d", (int)p_ptr->GetLev());
    center_string(buf, tmp);
    put_string(11*8, 11*16, buf, COLOR_WHITE);

    sprintf(tmp, "Exp: %ld", (long)p_ptr->GetExp());
    center_string(buf, tmp);
    put_string(11*8, 12*16, buf, COLOR_WHITE);

    sprintf(tmp, "Gold: %ld", (long)p_ptr->GetGold());
    center_string(buf, tmp);
    put_string(11*8, 13*16, buf, COLOR_WHITE);

    sprintf(tmp, "Killed on Level %d", dun_level);
    center_string(buf, tmp);
    put_string(11*8, 14*16, buf, COLOR_WHITE);

    sprintf(tmp, "by %s.", died_from);
    center_string(buf, tmp);
    put_string(11*8, 15*16, buf, COLOR_WHITE);


    sprintf(tmp, "%-.24s", ctime(&ct));
    center_string(buf, tmp);
    put_string(11*8, 17*16, buf, COLOR_WHITE);
}


/*
 * Display some character info
 */
static void show_info(void)
{
    int i, j, k;
    CItem *i_ptr;
    store_type *st_ptr = &store[7];


    // Hack -- Know everything in the inven/equip
    for (i = 0; i < INVEN_TOTAL; i++) {
        i_ptr = &inventory[i];
        if (i_ptr->exists()) {
            i_ptr->object_aware();
            i_ptr->MakeKnown();
        }
    }

    // Hack -- Know everything in the home
    for (i = 0; i < st_ptr->stock_num; i++) {
        i_ptr = &st_ptr->stock[i];
        if (i_ptr->exists()) {
            i_ptr->object_aware();
            i_ptr->MakeKnown();
        }
    }

    // Hack -- Recalculate bonuses
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

    // Update stuff
    update_stuff();


    // Describe options
    box(0, 21*16, 639, 22*16+15, COLOR_BLACK);
    put_string(0, 21*16,
        "You may now dump a character record to one or more files.", COLOR_WHITE);
    put_string(0, 22*16,
        "Then, hit RETURN to see the character, or ESC to abort.", COLOR_WHITE);

    // Dump character records as requested
    while (TRUE) {
        char out_val[160];

        // Prompt
        put_string(0, 23*16, "Filename: ", COLOR_WHITE);

        // Default
        strcpy(out_val, "");

        // Ask for filename (or abort)
        if (!askfor_aux(out_val, 60, 10, 32)) return;

        // Return means "show on screen"
        if (!out_val[0]) break;

        // Dump a character file
        //file_character(out_val, FALSE);
    }


    /* Show player on screen */
    display_player();

    /* Prompt for inventory */
    box(0, 23*16, 639, 23*16+15, COLOR_BLACK);
    put_string(0, 23*16, "Hit any key to see more information (ESC to abort): ",
        COLOR_WHITE);

    /* Allow abort at this point */
    screen_refresh();
    if (scan_inkey() == KEY_ESCAPE) return;


    /* Show equipment and inventory */

    /* Equipment -- if any */
    if (equip_cnt) {
        blank_screen(COLOR_BLACK);
        item_tester_full = TRUE;
        show_equip();
        put_string(0, 0, "You are using: -more-", COLOR_WHITE);
        screen_refresh();
        if (scan_inkey() == KEY_ESCAPE) return;
    }

    /* Inventory -- if any */
    if (inven_cnt) {
        blank_screen(COLOR_BLACK);
        item_tester_full = TRUE;
        show_inven();
        put_string(0, 0, "You are carrying: -more-", COLOR_WHITE);
        screen_refresh();
        if (scan_inkey() == KEY_ESCAPE) return;
    }



    /* Home -- if anything there */
    if (st_ptr->stock_num) {
        /* Display contents of the home */
        for (k = 0, i = 0; i < st_ptr->stock_num; k++) {
            /* Clear the screen */
            blank_screen(COLOR_BLACK);

            /* Show 12 items */
            for (j = 0; (j < 12) && (i < st_ptr->stock_num); j++, i++)
            {
                char i_name[80];
                char tmp_val[80];

                /* Acquire item */
                i_ptr = &st_ptr->stock[i];

                /* Print header, clear line */
                sprintf(tmp_val, "%c) ", I2A(j));
                box(4*8, (j+2)*16, 639, (j+2)*16+15, COLOR_BLACK);
                put_string(4*8, (j+2)*16, tmp_val, COLOR_WHITE);

                /* Display object description */
                i_ptr->object_desc(i_name, TRUE, 3);
                put_string(7*8, (j+2)*16, i_name, i_ptr->get_attr());
            }

            /* Caption */
            put_string(0, 0, format("Your home contains (page %d): -more-", k+1),
                COLOR_WHITE);

            /* Wait for it */
            screen_refresh();
            if (scan_inkey() == KEY_ESCAPE) return;
        }
    }
}





/*
 * High Score List Entry -- MJC
 * Not portable
 */
struct high_score {
    char what[8];               /* Version info (string) */

    u32b pts;
    u32b gold;
    u32b turns;

    char day[10];               /* Time stamp (string) */
    char who[16];               /* Player Name (string) */

    char sex[2];                /* Player Sex (string) */
    char p_r[3];                /* Player Race (number) */
    char p_c[3];                /* Player Class (number) */

    char cur_lev[4];            /* Current Player Level (number) */
    char cur_dun[4];            /* Current Dungeon Level (number) */
    char max_lev[4];            /* Max Player Level (number) */
    char max_dun[4];            /* Max Dungeon Level (number) */

    char how[32];               /* Method of death (string) */
};



/*
 * The "highscore" file descriptor, if available.
 */
static int highscore_fd = -1;


/*
 * Seek score 'i' in the highscore file
 */
static int highscore_seek(int i)
{
    /* Seek for the requested record */
    return (fd_seek(highscore_fd, (huge)(i) * sizeof(high_score)));
}


/*
 * Read one score from the highscore file
 */
static errr highscore_read(high_score *score)
{
    /* Read the record, note failure */
    return (fd_read(highscore_fd, (char*)(score), sizeof(high_score)));
}


/*
 * Write one score to the highscore file
 */
static int highscore_write(high_score *score)
{
    /* Write the record, note failure */
    return (fd_write(highscore_fd, (char*)(score), sizeof(high_score)));
}




/*
 * Just determine where a new score *would* be placed
 * Return the location (0 is best) or -1 on failure
 */
static int highscore_where(high_score *score)
{
    int i;
    high_score the_score;

    /* Paranoia -- it may not have opened */
    if (highscore_fd < 0) return (-1);

    /* Go to the start of the highscore file */
    if (highscore_seek(0)) return (-1);

    /* Read until we get to a higher score */
    for (i = 0; i < MAX_HISCORES; i++)
    {
        if (highscore_read(&the_score)) return i;
        if (the_score.pts < score->pts) return i;
    }

    /* The "last" entry is always usable */
    return (MAX_HISCORES - 1);
}


/*
 * Actually place an entry into the high score file
 * Return the location (0 is best) or -1 on "failure"
 */
static int highscore_add(high_score *score)
{
    int i, slot;
    bool done = FALSE;
    high_score the_score, tmpscore;


    /* Paranoia -- it may not have opened */
    if (highscore_fd < 0) return (-1);

    /* Determine where the score should go */
    slot = highscore_where(score);

    /* Hack -- Not on the list */
    if (slot < 0) return (-1);

    /* Hack -- prepare to dump the new score */
    the_score = *score;

    /* Slide all the scores down one */
    for (i = slot; !done && (i < MAX_HISCORES); i++) {
        /* Read the old guy, note errors */
        if (highscore_seek(i)) return (-1);
        if (highscore_read(&tmpscore)) done = TRUE;

        /* Back up and dump the score we were holding */
        if (highscore_seek(i)) return (-1);
        if (highscore_write(&the_score)) return (-1);

        /* Hack -- Save the old score, for the next pass */
        the_score = tmpscore;
    }

    /* Return location used */
    return (slot);
}



/*
 * Display the scores in a given range.
 * Assumes the high score list is already open.
 * Only five entries per line, too much info.
 *
 * Mega-Hack -- allow "fake" entry at the given position.
 */
static void display_scores(int from, int to, int note, high_score *score)
{
    int i, j, k, n, attr, place;
    high_score the_score;
    char out_val[256], tmp_val[160];


    /* Paranoia -- it may not have opened */
    if (highscore_fd < 0) return;


    /* Assume we will show the first 10 */
    if (from < 0) from = 0;
    if (to < 0) to = 10;
    if (to > MAX_HISCORES) to = MAX_HISCORES;


    /* Seek to the beginning */
    if (highscore_seek(0)) return;

    /* Hack -- Count the high scores */
    for (i = 0; i < MAX_HISCORES; i++) {
        if (highscore_read(&the_score)) break;
    }

    /* Hack -- allow "fake" entry to be last */
    if ((note == i) && score) i++;

    /* Forget about the last entries */
    if (i > to) i = to;


    /* Show 5 per page, until "done" */
    for (k = from, place = k+1; k < i; k += 5) {
        /* Clear those */
        blank_screen(COLOR_BLACK);

        /* Title */
        put_string(16*8, 0, "Utumno Hall of Fame", COLOR_WHITE);

        /* Indicate non-top scores */
        if (k > 0) {
            sprintf(tmp_val, "(from position %d)", k + 1);
            put_string(40*8, 0, tmp_val, COLOR_WHITE);
        }

        /* Dump 5 entries */
        for (j = k, n = 0; j < i && n < 5; place++, j++, n++) {
            int pr, pc, clev, mlev, cdun, mdun;
            char *when;
            u32b gold, aged;


            /* Hack -- indicate death in yellow */
            attr = (j == note) ? COLOR_YELLOW : COLOR_WHITE;


            /* Mega-Hack -- insert a "fake" record */
            if ((note == j) && score) {
                the_score = (*score);
                attr = COLOR_LT_GREEN;
                score = NULL;
                note = -1;
                j--;
            }

            /* Read a normal record */
            else {
                /* Read the proper record */
                if (highscore_seek(j)) break;
                if (highscore_read(&the_score)) break;
            }

            /* Extract the race/class */
            pr = atoi(the_score.p_r);
            pc = atoi(the_score.p_c);

            /* Extract the level info */
            clev = atoi(the_score.cur_lev);
            mlev = atoi(the_score.max_lev);
            cdun = atoi(the_score.cur_dun);
            mdun = atoi(the_score.max_dun);

            /* Hack -- extract the gold and such */
            for (when = the_score.day; isspace(*when); when++) ;
            gold = the_score.gold;
            aged = the_score.turns;

            /* Dump some info */
            sprintf(out_val, "%3d.%9d  %s the %s %s, Level %d",
                    place, the_score.pts, the_score.who,
                    race_info[pr].title, class_info[pc].title,
                    clev);

            /* Append a "maximum level" */
            if (mlev > clev) strcat(out_val, format(" (Max %d)", mlev));

            /* Dump the first line */
            put_string(0, (n*4+2)*16, out_val, attr);

            /* Another line of info */
            if (cdun) {
                sprintf(out_val, "               Killed by %s on Dungeon Level %d",
                        the_score.how, cdun);
            }

            /* Some people die in the town */
            else {
                sprintf(out_val, "               Killed by %s in the Town",
                        the_score.how);
            }

            /* Append a "maximum level" */
            if (mdun > cdun) strcat(out_val, format(" (Max %d)", mdun));

            /* Dump the info */
            put_string(0, (n*4+3)*16, out_val, attr);

            /* And still another line of info */
            sprintf(out_val,
                    "(Date %s, Gold %d, Turn %d).",
                    when, gold, aged);
            put_string(15*8, (n*4+4)*16, out_val, attr);
        }


        // Display stuff
        box(17*8, 23*16, 639, 23*16+15, COLOR_BLACK);
        put_string(17*8, 23*16, "[Press ESC to quit, any other key to continue.]",
            COLOR_WHITE);
        screen_refresh();
        box(0, 23*16, 639, 23*16+15, COLOR_BLACK);

        // Wait for key, notice escape
        if (scan_inkey() == KEY_ESCAPE) break;
    }
}


/*
 * Enters a players name on a hi-score table, if "legal", and in any
 * case, displays some relevant portion of the high score list.
 *
 * Assumes "signals_ignore_tstp()" has been called.
 */
static errr top_twenty(void)
{
    int j;
    high_score the_score;
    time_t ct = time((time_t*)0);


    /* Wipe screen */
    blank_screen(COLOR_BLACK);

    /* No score file */
    if (highscore_fd < 0) {
        mini_message_box("Note", "Score file unavailable.");
        return 0;
    }

    /* Cheaters are not scored */
    if (p_ptr->GetNoScore()) {
        mini_message_box("Note", "Score not registered for cheaters.");
        display_scores(0, 10, -1, NULL);
        return (0);
    }

    /* Interupted */
    if (!total_winner && streq(died_from, "Interrupting")) {
        mini_message_box("Note", "Score not registered due to interruption.");
        display_scores(0, 10, -1, NULL);
        return (0);
    }

    /* Quitter */
    if (!total_winner && streq(died_from, "quitting")) {
        mini_message_box("Note", "Score not registered due to quitting.");
        display_scores(0, 10, -1, NULL);
        return (0);
    }


    /* Clear the record */
    memset(&the_score, 0, sizeof(high_score));

    /* Save the version */
    strcpy(the_score.what, VERSION_STRING);

    /* Calculate and save the points */
    the_score.pts = total_points();

    /* Save the current gold */
    the_score.gold = p_ptr->GetGold();

    /* Save the current turn */
    the_score.turns = game_turn;

    /* Save the date in standard form (8 chars) */
    strftime(the_score.day, 9, "%m/%d/%Y", localtime(&ct));

    /* Save the player name (15 chars) */
    sprintf(the_score.who, "%-.15s", player_name);

    /* Save the player info */
    sprintf(the_score.sex, "%c", (p_ptr->GetMale() ? 'm' : 'f'));
    sprintf(the_score.p_r, "%2d", p_ptr->GetRace());
    sprintf(the_score.p_c, "%2d", p_ptr->GetClass());

    /* Save the level and such */
    sprintf(the_score.cur_lev, "%3d", p_ptr->GetLev());
    sprintf(the_score.cur_dun, "%3d", dun_level);
    sprintf(the_score.max_lev, "%3d", p_ptr->GetMaxPlv());
    sprintf(the_score.max_dun, "%3d", p_ptr->GetMaxDlv());

    /* Save the cause of death (31 chars) */
    sprintf(the_score.how, "%-.31s", died_from);


    /* Lock (for writing) the highscore file, or fail */
    if (fd_lock(highscore_fd, F_WRLCK)) return (1);

    /* Add a new entry to the score list, see where it went */
    j = highscore_add(&the_score);

    /* Unlock the highscore file, or fail */
    if (fd_lock(highscore_fd, F_UNLCK)) return (1);


    /* Hack -- Display the top fifteen scores */
    if (j < 10) {
        display_scores(0, 15, j, NULL);
    }

    /* Display the scores surrounding the player */
    else {
        display_scores(0, 5, j, NULL);
        display_scores(j - 2, j + 7, j, NULL);
    }


    /* Success */
    return (0);
}


/*
 * Predict the players location, and display it.
 */
static errr predict_score(void)
{
    int j;
    high_score the_score;


    /* No score file */
    if (highscore_fd < 0) {
        mini_message_box("Note", "Score file unavailable.");
        return (0);
    }


    /* Save the version */
    strcpy(the_score.what, VERSION_STRING);

    /* Calculate and save the points */
    the_score.pts = total_points();

    /* Save the current gold */
    the_score.gold = p_ptr->GetGold();

    /* Save the current turn */
    the_score.turns = game_turn;

    /* Hack -- no time needed */
    strcpy(the_score.day, "TODAY");

    /* Save the player name (15 chars) */
    sprintf(the_score.who, "%-.15s", player_name);

    /* Save the player info */
    sprintf(the_score.sex, "%c", (p_ptr->GetMale() ? 'm' : 'f'));
    sprintf(the_score.p_r, "%2d", p_ptr->GetRace());
    sprintf(the_score.p_c, "%2d", p_ptr->GetClass());

    /* Save the level and such */
    sprintf(the_score.cur_lev, "%3d", p_ptr->GetLev());
    sprintf(the_score.cur_dun, "%3d", dun_level);
    sprintf(the_score.max_lev, "%3d", p_ptr->GetMaxPlv());
    sprintf(the_score.max_dun, "%3d", p_ptr->GetMaxDlv());

    /* Hack -- no cause of death */
    strcpy(the_score.how, "nobody (yet!)");


    /* See where the entry would be placed */
    j = highscore_where(&the_score);


    /* Hack -- Display the top fifteen scores */
    if (j < 10) {
        display_scores(0, 15, j, &the_score);
    }

    /* Display some "useful" scores */
    else {
        display_scores(0, 5, -1, NULL);
        display_scores(j - 2, j + 7, j, &the_score);
    }


    /* Success */
    return (0);
}








/*
 * Change the player into a King!                       -RAK-   
 */
static void kingly()
{
    /* Hack -- retire in town */
    dun_level = 0;

    /* Fake death */
    strcpy(died_from, "Ripe Old Age");

    /* Restore the experience */
    p_ptr->SetExp(p_ptr->GetMaxExp());

    /* Restore the level */
    p_ptr->SetLev(p_ptr->GetMaxPlv());

    /* Check the experience */
    /* p_ptr->check_experience(); */

    /* Display a crown */
    blank_screen(COLOR_BLACK);
    put_string(34*8, 1*16, "#", COLOR_WHITE);
    put_string(32*8, 2*16, "#####", COLOR_WHITE);
    put_string(34*8, 3*16, "#", COLOR_WHITE);
    put_string(28*8, 4*16, ",,,  $$$  ,,,", COLOR_WHITE);
    put_string(24*8, 5*16, ",,=$   \"$$$$$\"   $=,,", COLOR_WHITE);
    put_string(22*8, 6*16, ",$$        $$$        $$,", COLOR_WHITE);
    put_string(22*8, 7*16, "*>         <*>         <*", COLOR_WHITE);
    put_string(22*8, 8*16, "$$         $$$         $$", COLOR_WHITE);
    put_string(22*8, 9*16, "\"$$        $$$        $$\"", COLOR_WHITE);
    put_string(23*8, 10*16, "\"$$       $$$       $$\"", COLOR_WHITE);
    put_string(24*8, 11*16, "*#########*#########*", COLOR_WHITE);
    put_string(24*8, 12*16, "*#########*#########*", COLOR_WHITE);

    /* Display a message */
    put_string(26*8, 15*16, "Veni, Vidi, Vici!", COLOR_WHITE);
    put_string(21*8, 16*16, "I came, I saw, I conquered!", COLOR_WHITE);
    if (p_ptr->GetMale()) {
        put_string(22*8, 17*16, "All Hail the Mighty King!", COLOR_WHITE);
    }
    else {
        put_string(22*8, 17*16, "All Hail the Mighty Queen!", COLOR_WHITE);
    }

    /* Wait for response */
    box(0, 23*16, 639, 23*16+15, COLOR_BLACK);
    put_text_format(320, 23*16, "Press any key to continue", COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);
    screen_refresh();
    wait_for_key();
    box(0, 23*16, 639, 23*16+15, COLOR_BLACK);
}


/*
 * Changes whether a certain savefile exists.
 */
void set_savefile_exist(int idx, int to)
{
    FILE *f;
    int i, slot_taken[10];

    // Which slots are open?
    f = fopen("dat/save/save.dat", "rt");
    for (i = 0; i < 10; i++) {
        fscanf(f, "%d", &slot_taken[i]);
    }
    fclose(f);

    // Set the one
    slot_taken[idx] = to;

    // Save
    f = fopen("dat/save/save.dat", "wt");
    for (i = 0; i < 10; i++) {
        fprintf(f, "%d\n", slot_taken[i]);
    }
    fclose(f);
}


/*
 * Close up the current game (player may or may not be dead)
 *
 * This function is called only from "main.c" and "signals.c".
 */
void close_game(void)
{
    char buf[1024];


    /* Update stuff */
    update_stuff();

    /* No suspending now */
    signals_ignore_tstp();


    /* Hack -- Character is now "icky" */
    character_icky = TRUE;


    /* Access the high score file */
    strcpy(buf, "dat/scores.raw");

    /* Open the high score file, for reading/writing */
    highscore_fd = fd_open(buf, O_RDWR);


    // Handle death
    if (death) {
        // Handle retirement
        if (total_winner) kingly();

        // Set existence of proper savefile # to 0
        set_savefile_exist(char_idx, 0);

        // Save memories
        if (!save_player()) mini_message_box("Error", "Death save failed!");

        // You are dead
        print_tomb();

        // Show more info
        show_info();

        // Handle score, show top scores
        top_twenty();
    }

    /* Still alive */
    else {
        /* The player is not dead */
        strcpy(died_from, "(saved)");

        /* Forbid suspend */
        signals_ignore_tstp();

        /* Save the player */
        if (!save_player()) mini_message_box("Error", "Save failed!");

        /* Allow suspend again */
        signals_handle_tstp();

        /* Refresh */
        screen_refresh();

        /* Note that the player is not dead */
        strcpy(died_from, "(alive and well)");

        /* Predict score */
        predict_score(); 
    }


    /* Shut the high score file */
    fd_close(highscore_fd);

    /* Forget the high score fd */
    highscore_fd = -1;


    /* Allow suspending now */
    signals_handle_tstp();
}


/*
 * Handle signals -- suspend
 *
 * Actually suspend the game, and then resume cleanly
 */
static void handle_signal_suspend(int sig)
{
    /* Disable handler */
    signal(sig, SIG_IGN);

#ifdef SIGSTOP

    /* Flush output */
    screen_refresh();

    /* Suspend ourself */
    kill(0, SIGSTOP);

    /* Flush the term */
    screen_refresh();

#endif

    /* Restore handler */
    signal(sig, handle_signal_suspend);
}


/*
 * Handle signal -- abort, kill, etc
 */
static void handle_signal_abort(int sig)
{
    /* Disable handler */
    signal(sig, SIG_IGN);
    printf("%d\n",sig);
    // Nothing to save, just quit with an error
    if (!character_generated || character_saved) quit("software bug");


    // Panic save
    panic_save = 1;
    strcpy(died_from, "(panic save)");

    // Forbid suspend
    signals_ignore_tstp();

    // Attempt to save
    save_player();
    // Quit
    quit("software bug");
}


/*
 * Ignore SIGTSTP signals (keyboard suspend)
 */
void signals_ignore_tstp(void)
{
#ifdef SIGTSTP
    signal(SIGTSTP, SIG_IGN);
#endif
}

/*
 * Handle SIGTSTP signals (keyboard suspend)
 */
void signals_handle_tstp(void)
{
#ifdef SIGTSTP
    signal(SIGTSTP, handle_signal_suspend);
#endif
}


/*
 * Prepare to handle the relevant signals
 */
void signals_init()
{
#ifdef SIGHUP
    signal(SIGHUP, SIG_IGN);
#endif


#ifdef SIGTSTP
    signal(SIGTSTP, handle_signal_suspend);
#endif


#ifdef SIGINT
    signal(SIGINT, handle_signal_abort);
#endif

#ifdef SIGQUIT
    signal(SIGQUIT, SIG_IGN);
#endif


#ifdef SIGFPE
    signal(SIGFPE, handle_signal_abort);
#endif

#ifdef SIGILL
    signal(SIGILL, handle_signal_abort);
#endif

#ifdef SIGTRAP
    signal(SIGTRAP, handle_signal_abort);
#endif

#ifdef SIGIOT
    signal(SIGIOT, handle_signal_abort);
#endif

#ifdef SIGKILL
    signal(SIGKILL, handle_signal_abort);
#endif

#ifdef SIGBUS
    signal(SIGBUS, handle_signal_abort);
#endif

#ifdef SIGSEGV
    signal(SIGSEGV, handle_signal_abort);
#endif

#ifdef SIGTERM
    signal(SIGTERM, handle_signal_abort);
#endif

#ifdef SIGPIPE
    signal(SIGPIPE, handle_signal_abort);
#endif

#ifdef SIGEMT
    signal(SIGEMT, handle_signal_abort);
#endif

#ifdef SIGDANGER
    signal(SIGDANGER, handle_signal_abort);
#endif

#ifdef SIGSYS
    signal(SIGSYS, handle_signal_abort);
#endif

#ifdef SIGXCPU
    signal(SIGXCPU, handle_signal_abort);
#endif

#ifdef SIGPWR
    signal(SIGPWR, handle_signal_abort);
#endif
}
