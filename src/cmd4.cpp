// File: cmd4.cpp
// Purpose: Interface commands

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Pause the game
 */
void do_cmd_pause(void)
{
    char c;

    draw_window(220, 146, 420, 207, "Game Paused");
    put_text_format(320, 178, "Press Escape to unpause", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    screen_refresh();
    for (;;) {
        c = scan_inkey_scan();
        if (c == KEY_ESCAPE) {
            reset_timer();
            return;
        }
    }
}


class COptionsCustom: public CComponent {
private:
    void DrawMe(int mx, int my, bool left)
    {
        put_text(380, 138, format("Game Speed: %d", game_speed), COLOR_BLACK, FONT_BOLD);
        put_text(380, 154, format("(about %d.%.2d sec per turn)", (game_speed*7)/100,
            (game_speed*7)%100), COLOR_BLACK, FONT_BOLD);
        put_text(380, 230, format("Hitpoint Warning: %d%%", hitpoint_warn*10), COLOR_BLACK,
            FONT_BOLD);
    }
public:
    COptionsCustom() : CComponent(0, 0, 0, 0) {}
};


/*
 * Set or unset various options.
 */
void do_cmd_options(void)
{
    char c;
    int i, mx, my, y;

    CWindow *window = new CWindow(100, 100, 440, 250, "Options");
    CButton *exit = new CButton(430, 317, 60, 20, "Exit"); window->Attach(exit);
    CButton *inc_sp = new CButton(380, 180, 60, 20, "Increase"); window->Attach(inc_sp);
    CButton *dec_sp = new CButton(460, 180, 60, 20, "Decrease"); window->Attach(dec_sp);
    CButton *inc_hp = new CButton(380, 256, 60, 20, "Increase"); window->Attach(inc_hp);
    CButton *dec_hp = new CButton(460, 256, 60, 20, "Decrease"); window->Attach(dec_hp);
    COptionsCustom *custom = new COptionsCustom(); window->Attach(custom);

    // The options
    y = 130;
    for (i = 0; options[i].o_desc; i++) {
        if (!options[i].o_show) continue;
        window->Attach(new CText(140, y, options[i].o_desc, FONT_BOLD, JUST_LEFT));
        window->Attach(new CCheckBox(117, y, options[i].o_var));
        y += 16;
    }

    for (;;) {
        // Draw everything
        gui_draw(window);

        // Process keystrokes
        c = scan_inkey_scan();
        if (c == KEY_ESCAPE) break;
        if (c == KEY_ENTER) break;

        // Get mouse releases
        if (get_last_left_button_release(&mx, &my)) {
            window->Click(mx, my);

            if (inc_sp->inClientArea(mx, my)) {
                if (game_speed < 100) game_speed++;
            }
            if (dec_sp->inClientArea(mx, my)) {
                if (game_speed > 1) game_speed--;
            }
            if (inc_hp->inClientArea(mx, my)) {
                if (hitpoint_warn < 9) hitpoint_warn++;
            }
            if (dec_hp->inClientArea(mx, my)) {
                if (hitpoint_warn > 0) hitpoint_warn--;
            }

            if (exit->inClientArea(mx, my)) break;
        }
    }

    reset_timer();
    
    delete window;

    write_options();
}



/*
 * Mention the current version
 */
void do_cmd_version(void)
{
    msg_format("You are playing Utumno %s.", VERSION_STRING);
}



/*
 * Check the status of "artifacts"
 */
void do_cmd_check_artifacts(void)
{
    int i, k, z, x, y;
    FILE *fff;
    char file_name[1024];
    char base_name[80];
    bool okay[MAX_A_IDX];


    /* Temporary file */
    if (path_temp(file_name, 1024)) return;

    /* Open a new file */
    fff = my_fopen(file_name, "w");

    /* Scan the artifacts */
    for (k = 0; k < MAX_A_IDX; k++) {
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
    for (y = 0; y < cur_hgt; y++) {
        for (x = 0; x < cur_wid; x++) {
            CGrid *g_ptr = &cave[y][x];

            /* Process objects */
            if (g_ptr->i_ptr) {
                CItem *i_ptr = g_ptr->i_ptr;

                /* Ignore non-artifacts */
                if (!i_ptr->isArtifact()) continue;

                /* Ignore known items */
                if (i_ptr->isKnown()) continue;

                /* Note the artifact */
                okay[i_ptr->GetName1()] = FALSE;
            }
        }
    }

    /* Check the inventory */
    for (i = 0; i < INVEN_TOTAL; i++) {
        CItem *i_ptr = &inventory[i];

        /* Ignore non-objects */
        if (!i_ptr->exists()) continue;

        /* Ignore non-artifacts */
        if (!i_ptr->isArtifact()) continue;

        /* Ignore known items */
        if (i_ptr->isKnown()) continue;

        /* Note the artifact */
        okay[i_ptr->GetName1()] = FALSE;
    }

    /* Scan the artifacts */
    for (k = 0; k < MAX_A_IDX; k++) {
        artifact_type *a_ptr = &a_info[k];

        /* List "dead" ones */
        if (!okay[k]) continue;

        /* Paranoia */
        strcpy(base_name, "Unknown Artifact");

        /* Obtain the base object type */
        z = lookup_kind(a_ptr->tval, a_ptr->sval);

        /* Real object */
        if (z) {
            CItem *i_ptr;

            // Make an object
            i_ptr = new CItem(z);

            /* Create the artifact */
            i_ptr->SetName1(k);

            /* Describe the artifact */
            i_ptr->object_desc_store(base_name, FALSE, 0);

            // Destroy the object
            delete i_ptr;
        }

        /* Hack -- Build the artifact name */
        fprintf(fff, "     The %s\n", base_name);
    }

    /* Close the file */
    fclose(fff);

    /* Display the file contents */
    show_file(file_name, "Artifacts Seen");

    /* Remove the file */
    fd_kill(file_name);
}


/*
 * Check the status of "uniques"
 */
void do_cmd_check_uniques()
{
    int k;
    FILE *fff;
    char file_name[1024];


    /* Temporary file */
    if (path_temp(file_name, 1024)) return;

    /* Open a new file */
    fff = my_fopen(file_name, "w");

    /* Scan the monster races */
    for (k = 1; k < MAX_R_IDX; k++) {
        CMonsterRace *r_ptr = &r_info[k];

        /* Only print Uniques */
        if (r_ptr->flags1 & RF1_UNIQUE) {
            bool dead = (r_ptr->max_num == 0);

            /* Only display "known" uniques */
            if (dead || r_ptr->r_sights) {
                /* Print a message */
                fprintf(fff, "     %s is %s.\n",
                        (r_name + r_ptr->name),
                        (dead ? "dead" : "alive"));
            }
        }
    }

    /* Close the file */
    fclose(fff);

    /* Display the file contents */
    show_file(file_name, "Known Uniques");

    /* Remove the file */
    fd_kill(file_name);
}


/*
 * Check identified objects
 */
void do_cmd_identified_objs(void)
{
    int i;

    FILE *fff;

    char file_name[1024];


    /* Temporary file */
    if (path_temp(file_name, 1024)) return;

    /* Open a new file */
    fff = my_fopen(file_name, "w");

    // Amulets
    fprintf(fff, "Amulets\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it an amulet?
        if (k_ptr->tval != TV_AMULET) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    amulet of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Rings
    fprintf(fff, "\nRings\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a ring?
        if (k_ptr->tval != TV_RING) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    ring of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Staves
    fprintf(fff, "\nStaves\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a staff?
        if (k_ptr->tval != TV_STAFF) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    staff of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Wands
    fprintf(fff, "\nWands\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a wand?
        if (k_ptr->tval != TV_WAND) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    wand of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Rods
    fprintf(fff, "\nRods\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a rod?
        if (k_ptr->tval != TV_ROD) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    rod of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Scrolls
    fprintf(fff, "\nScrolls\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a scroll?
        if (k_ptr->tval != TV_SCROLL) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    scroll of %s (\"%s\")\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Potions
    fprintf(fff, "\nPotions\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a potion?
        if (k_ptr->tval != TV_POTION) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    potion of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    // Mushrooms
    fprintf(fff, "\nMushrooms\n");
    for (i = 1; i < MAX_K_IDX-1; i++) {
        CObjectKind *k_ptr = &k_info[i];

        // Is it a foodstuff?
        if (k_ptr->tval != TV_FOOD) continue;

        // Is it boring?
        if (k_ptr->sval >= SV_FOOD_MIN_FOOD) continue;

        // No instant artifacts
        if (k_ptr->flags3 & TR3_INSTA_ART) continue;

        // No unawares
        if (!k_ptr->aware) continue;

        // Output
        fprintf(fff, "    mushroom of %s (%s)\n", k_ptr->name,
            flavor_desc(k_ptr));
    }

    /* Close the file */
    fclose(fff);

    /* Display the file contents */
    show_file(file_name, "Known Item Types");

    /* Remove the file */
    fd_kill(file_name);
}



// The names for the scancodes
char *scancode_table[] = {
    "",                          // 0
    "escape",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",                         // 10
    "0",
    "-",
    "=",
    "backspace",
    "tab",
    "q",
    "w",
    "e",
    "r",
    "t",                         // 20
    "y",
    "u",
    "i",
    "o",
    "p",
    "[",
    "]",
    "enter",
    "",
    "a",                         // 30
    "s",
    "d",
    "f",
    "g",
    "h",
    "j",
    "k",
    "l",
    ";",
    "'",                         // 40
    "`",
    "",
    "\\",
    "z",
    "x",
    "c",
    "v",
    "b",
    "n",
    "m",                         // 50
    ",",
    ".",
    "/",
    "",
    "key*",
    "",
    "space",
    "",
    "F1",
    "F2",                        // 60
    "F3",
    "F4",
    "F5",
    "F6",
    "F7",
    "F8",
    "F9",
    "F10",
    "",
    "",                          // 70
    "key7",
    "key8",
    "key9",
    "key-",
    "key4",
    "key5",
    "key6",
    "key+",
    "key1",
    "key2",                      // 80
    "key3",
    "key0",
    "key.",
    "",
    "",
    "",
    "",
    "",
    "",
    "",                          // 90
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",                          // 100
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",                          // 110
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",
    "",                          // 120
    "",
    "",
    "",
    "",
    "",
    "",
    "",                          // 127
};

// Checks whether a scan code is valid
bool scan_valid(int c)
{
    if (c < 0) return FALSE;
    if (c > 127) return FALSE;
    if (!scancode_table[c][0]) return FALSE;
    return TRUE;
}


// The table of current bindings
static char *bind_table[128];

// Bind something
void do_cmd_bind(char *from, char *to)
{
    int i;

    // Search for "from" in the scancode table
    for (i = 0; i < 128; i++) {
        if (streq(scancode_table[i], from)) {
            if (bind_table[i]) delete[] bind_table[i];
            bind_table[i] = new char[strlen(to)+1];
            strcpy(bind_table[i], to);
            return;
        }
    }

    // Error: not found
    msg_format("Key not found: %s", from);
}

// Get the command to be interpreted for a certain scancode
void get_command(int scan, char *buf)
{
    if ((scan < 0) || (scan > 127) || !bind_table[scan]) {
        strcpy(buf, "");
    }
    else {
        strcpy(buf, bind_table[scan]);
    }
}

// Carry out the initial bindings
void init_bind(void)
{
    int i;

    for (i = 0; i < 128; i++) bind_table[i] = NULL;

    buffer_console("bind w wield");
    buffer_console("bind t takeoff");
    buffer_console("bind d drop");
    buffer_console("bind k destroy");
    buffer_console("bind e equipment");
    buffer_console("bind i inventory");
    buffer_console("bind o observe");
    buffer_console("bind s search");
    buffer_console("bind - enter_store");
    buffer_console("bind g study");
    buffer_console("bind b browse");
    buffer_console("bind m magic");
    buffer_console("bind a activate");
    buffer_console("bind f fire");
    buffer_console("bind z zap");
    buffer_console("bind u use");
    buffer_console("bind r rest");
    buffer_console("bind / query_symbol");
    buffer_console("bind c character");
    buffer_console("bind = options");
    buffer_console("bind F1 help");
    buffer_console("bind F2 screen_dump");
    buffer_console("bind F3 fps_start");
    buffer_console("bind F4 fps_end");
    buffer_console("bind F10 save_quit");
    buffer_console("bind [ artifacts");
    buffer_console("bind ] items");
    buffer_console("bind \\ uniques");
    buffer_console("bind ` show_console");
    buffer_console("bind escape pause");
    buffer_console("bind key1 walk 1");
    buffer_console("bind key2 walk 2");
    buffer_console("bind key3 walk 3");
    buffer_console("bind key4 walk 4");
    buffer_console("bind key6 walk 6");
    buffer_console("bind key7 walk 7");
    buffer_console("bind key8 walk 8");
    buffer_console("bind key9 walk 9");
    buffer_console("bind tab minimap");
}
