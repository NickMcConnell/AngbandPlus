// File: init2.c
// Purpose: Initialization (part 2) -BEN-

#include "utumno.h"


/*
 * This file is used to initialize various variables and arrays for the
 * Angband game.  Note the use of "fd_read()" and "fd_write()" to bypass
 * the common limitation of "read()" and "write()" to only 32767 bytes
 * at a time.
 *
 * Several of the arrays for Angband are built from "template" files in
 * the "dat" directory, from which quick-load binary "image" files
 * are constructed whenever they are not present in the "lib/data"
 * directory, or if those files become obsolete, if we are allowed.
 *
 * Warning -- the "ascii" file parsers use a minor hack to collect the
 * name and text information in a single pass.  Thus, the game will not
 * be able to load any template file with more than 20K of names or 60K
 * of text, even though technically, up to 64K should be legal.
 *
 * The "init1.c" file is used only to parse the ascii template files,
 * to create the binary image files.
 */



/*
 * Display the game information.
 */
static void show_splash_aux(void)
{
    FILE *fp, *logo;
    char buf[1024];
    int maxx, maxy, i, x, y, r, g, b;

    // Clear the screen
    blank_screen(COLOR_BLACK);
    // Access the "news" file
    strcpy(buf, "dat/news.txt");
    fp = my_fopen(buf, "r");

    // Open the logo file and display
    logo = fopen("dat/graphics/logo.dat", "rb");
    maxx = fgetc(logo);
    maxx += fgetc(logo) << 8;
    maxy = fgetc(logo);
    maxy += fgetc(logo) << 8;
    for (i = 0; i < 64; i++) {
        r = fgetc(logo);
        g = fgetc(logo);
        b = fgetc(logo);
        set_palette_entry(i+128, r, g, b);
    }
    start_pixel_draw();
    for (y = 0; y < maxy; y++) {
        for (x = 0; x < maxx; x++) {
            draw_pixel(320-maxx/2+x, 32+y, fgetc(logo)+128);
        }
    }
    end_pixel_draw();
    fclose(logo);

    put_text_format(320, 140, format("Xutumno %s", VERSION_STRING), COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);
    put_text_format(320, 180, "Unofficial port of Utumno",
        COLOR_WHITE, FONT_BOLD, JUST_CENTER);

    put_text_format(320,210,"Utumno Programming: Matt Craighead (craighea@citilink.com)", COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    put_text_format(320, 226, "Utumno Artists: Jason Scanlin, Tom Ebling, Joe Lee",
        COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    put_text_format(320, 265, "X port: Marcello Sega & Marco Vecchiocattivi",
        COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    put_text_format(320, 281, "sega@physics.it", COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    
    put_text_format(320, 305, "Visit the Xutumno Home Page", COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);
    put_text_format(320, 321, "http://www.physics.it/xutumno/", COLOR_WHITE,
        FONT_BOLD, JUST_CENTER);
    
    put_text_format(320, 340, "Xutumno needs more artists!", COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);

    // Flush it
    screen_refresh();
}


/*
 * Hack -- verify some files, and display the "news.txt" file
 *
 * This function is often called before "init_some_arrays()",
 * but after the "term.c" package has been initialized, so
 * be aware that many functions will not be "usable" yet.
 *
 * Note that this function attempts to verify the "news" file,
 * and the game aborts (cleanly) on failure.
 *
 * Note that this function attempts to verify (or create) the
 * "high score" file, and the game aborts (cleanly) on failure.
 *
 * Note that one of the most common "extraction" errors involves
 * failing to extract all sub-directories (even empty ones), such
 * as by failing to use the "-d" option of "pkunzip", or failing
 * to use the "save empty directories" option with "Compact Pro".
 * This error will often be caught by the "high score" creation
 * code below.
 */
void show_splash(void)
{
    int fd = -1;
    int mode = 0644;
    char buf[1024];


    /*** Verify the "create.dir" file ***/

    // Attempt to open the "create.dir" file
    strcpy(buf, "dat/create.dir");
    fd = fd_open(buf, O_RDONLY);

    // Failure
    if (fd < 0) {
        quit("Cannot access the 'dat/create.dir' file!\n"
             "The dat/ directory is probably missing or broken.\n"
             "Perhaps the archive was not extracted correctly.\n"
             "See the 'README' file for more information.");
    }

    // Close it
    fd_close(fd);

   
    // Display the news
    
    show_splash_aux();
    
    /*** Verify (or create) the "high score" file ***/

    // Attempt to open the high score file
    strcpy(buf, "dat/scores.raw");
    fd = fd_open(buf, O_RDONLY);

    // Failure
    if (fd < 0) {
        // Create a new high score file
        fd = fd_make(buf, mode);

        // Failure
        if (fd < 0) {
            quit("Cannot create the 'dat/scores.raw' file!");
        }
    }

    /* Close it */
    (void)fd_close(fd);
}



/*
 * Hack -- help give useful error messages
 */
s16b error_idx;
s16b error_line;


/*
 * Hack -- help initialize the fake "name" and "text" arrays when
 * parsing an "ascii" template file.
 */
u16b fake_name_size;
u16b fake_text_size;


/*
 * Standard error message text
 */
static char *err_str[8] = {
    NULL,
    "parse error",
    "obsolete file",
    "missing record header",
    "non-sequential records",
    "invalid flag specification",
    "undefined directive",
    "out of memory"
};



/*** Initialize from binary image files ***/


/*
 * Initialize the "f_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_f_info(void)
{
    errr err;
    FILE *fp;

    /* General buffer */
    char buf[1024];


    /*** Make the header ***/

    /* Allocate the "header" */
    f_head = new header;
    memset(f_head, 0, sizeof(header));


    /*** Make the fake arrays ***/

    /* Fake the size of "f_name" and "f_text" */
    fake_name_size = 20 * 1024L;
    fake_text_size = 60 * 1024L;

    /* Allocate the "f_info" array */
    f_info = new feature_type[MAX_F_IDX];
    memset(f_info, 0, MAX_F_IDX*sizeof(feature_type));

    /* Hack -- make "fake" arrays */
    f_name = new char[fake_name_size];
    memset(f_name, 0, fake_name_size);


    /*** Load the ascii template file ***/

    /* Access the "f_info.txt" file */
    strcpy(buf, "dat/f_info.txt");

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* Parse it */
    if (!fp) quit("Cannot open 'f_info.txt' file.");

    /* Parse the file */
    err = init_f_info_txt(fp, buf);

    /* Close it */
    fclose(fp);

    /* Errors */
    if (err) {
        char oops[500], buf2[500];

        // Error string
        strcpy(oops, (((err > 0) && (err < 8)) ? err_str[err] : "unknown"));

        // Oops
        sprintf(buf2, "Error %d at line %d of 'f_info.txt'.\n"
                      "Record %d contains a '%s' error.\n"
                      "Parsing '%s'.", err, error_line, error_idx, oops, buf);

        // Quit
        quit(buf2);
    }

    // Success
    return 0;
}



/*
 * Initialize the "k_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_k_info(void)
{
    errr err;
    FILE *fp;
    char buf[1024];


    // Make the array
    k_info = new CObjectKind[MAX_K_IDX];
    memset(k_info, 0, MAX_K_IDX*sizeof(CObjectKind));


    /*** Load the ascii template file ***/

    /* Access the "k_info.txt" file */
    strcpy(buf, "dat/k_info.txt");

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* Parse it */
    if (!fp) quit("Cannot open 'k_info.txt' file.");

    /* Parse the file */
    err = init_k_info_txt(fp, buf);

    /* Close it */
    fclose(fp);

    /* Errors */
    if (err) {
        char oops[500], buf2[500];

        /* Error string */
        strcpy(oops ,(((err > 0) && (err < 8)) ? err_str[err] : "unknown"));

        /* Oops */
        sprintf(buf2, "Error %d at line %d of 'k_info.txt'.\n"
                      "Record %d contains a '%s' error.\n"
                      "Parsing '%s'.", err, error_line, error_idx, oops, buf);

        /* Quit */
        quit(buf2);
    }

    /* Success */
    return (0);
}



/*
 * Initialize the "a_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_a_info(void)
{
    errr err;
    FILE *fp;
    char buf[1024];


    // Allocate the "header"
    a_head = new header;
    memset(a_head, 0, sizeof(header));


    /*** Make the fake arrays ***/

    /* Fake the size of "a_name" and "a_text" */
    fake_name_size = 20 * 1024L;
    fake_text_size = 60 * 1024L;

    /* Allocate the "a_info" array */
    a_info = new artifact_type[MAX_A_IDX];
    memset(a_info, 0, MAX_A_IDX*sizeof(artifact_type));

    /* Hack -- make "fake" arrays */
    a_name = new char[fake_name_size];
    memset(a_name, 0, fake_name_size);


    /*** Load the ascii template file ***/

    /* Access the "a_info.txt" file */
    strcpy(buf, "dat/a_info.txt");

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* Parse it */
    if (!fp) quit("Cannot open 'a_info.txt' file.");

    /* Parse the file */
    err = init_a_info_txt(fp, buf);

    /* Close it */
    fclose(fp);

    /* Errors */
    if (err) {
        char oops[500], buf2[500];

        /* Error string */
        strcpy(oops ,(((err > 0) && (err < 8)) ? err_str[err] : "unknown"));

        /* Oops */
        sprintf(buf2, "Error %d at line %d of 'a_info.txt'.\n"
                      "Record %d contains a '%s' error.\n"
                      "Parsing '%s'.", err, error_line, error_idx, oops, buf);

        /* Quit */
        quit(buf2);
    }

    /* Success */
    return (0);
}



/*
 * Initialize the "e_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_e_info(void)
{
    errr err;
    FILE *fp;
    char buf[1024];


    /*** Make the "header" ***/

    /* Allocate the "header" */
    e_head = new header;
    memset(e_head, 0, sizeof(header));


    /*** Make the fake arrays ***/

    /* Fake the size of "e_name" and "e_text" */
    fake_name_size = 20 * 1024L;
    fake_text_size = 60 * 1024L;

    /* Allocate the "e_info" array */
    e_info = new ego_item_type[MAX_E_IDX];
    memset(e_info, 0, MAX_E_IDX*sizeof(ego_item_type));

    /* Hack -- make "fake" arrays */
    e_name = new char[fake_name_size];
    memset(e_name, 0, fake_name_size);
    e_text = new char[fake_text_size];
    memset(e_text, 0, fake_text_size);


    /*** Load the ascii template file ***/

    /* Access the "e_info.txt" file */
    strcpy(buf, "dat/e_info.txt");

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* Parse it */
    if (!fp) quit("Cannot open 'e_info.txt' file.");

    /* Parse the file */
    err = init_e_info_txt(fp, buf);

    /* Close it */
    fclose(fp);

    /* Errors */
    if (err) {
        char oops[500], buf2[500];

        // Error string
        strcpy(oops ,(((err > 0) && (err < 8)) ? err_str[err] : "unknown"));

        // Oops
        sprintf(buf2, "Error %d at line %d of 'e_info.txt'.\n"
                      "Record %d contains a '%s' error.\n"
                      "Parsing '%s'.", err, error_line, error_idx, oops, buf);

        // Quit
        quit(buf2);
    }

    /* Success */
    return (0);
}



/*
 * Initialize the "r_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_r_info(void)
{
    errr err;

    FILE *fp;

    /* General buffer */
    char buf[1024];


    /*** Make the header ***/

    /* Allocate the "header" */
    r_head = new header;
    memset(r_head, 0, sizeof(header));


    /*** Make the fake arrays ***/

    /* Assume the size of "r_name" and "r_text" */
    fake_name_size = 20 * 1024L;
    fake_text_size = 60 * 1024L;

    /* Allocate the "r_info" array */
    r_info = new CMonsterRace[MAX_R_IDX];
    memset(r_info, 0, MAX_R_IDX*sizeof(CMonsterRace));

    /* Hack -- make "fake" arrays */
    r_name = new char[fake_name_size];
    memset(r_name, 0, fake_name_size);
    r_text = new char[fake_text_size];
    memset(r_text, 0, fake_text_size);


    /*** Load the ascii template file ***/

    /* Access the "r_info.txt" file */
    strcpy(buf, "dat/r_info.txt");

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* Parse it */
    if (!fp) quit("Cannot open 'r_info.txt' file.");

    /* Parse the file */
    err = init_r_info_txt(fp, buf);

    /* Close it */
    fclose(fp);

    /* Errors */
    if (err) {
        char oops[500], buf2[500];

        // Error string
        strcpy(oops , (((err > 0) && (err < 8)) ? err_str[err] : "unknown"));

        // Oops
        sprintf(buf2, "Error %d at line %d of 'r_info.txt'.\n"
                      "Record %d contains a '%s' error.\n"
                      "Parsing '%s'.", err, error_line, error_idx, oops, buf);

        // Quit
        quit(buf2);
    }

    /* Success */
    return (0);
}



/*
 * Initialize the "v_info" array
 *
 * Note that we let each entry have a unique "name" and "text" string,
 * even if the string happens to be empty (everyone has a unique '\0').
 */
static errr init_v_info(void)
{
    errr err;

    FILE *fp;

    /* General buffer */
    char buf[1024];


    /*** Make the header ***/

    /* Allocate the "header" */
    v_head = new header;
    memset(v_head, 0, sizeof(header));


    /*** Make the fake arrays ***/

    /* Fake the size of "v_name" and "v_text" */
    fake_name_size = 20 * 1024L;
    fake_text_size = 60 * 1024L;

    /* Allocate the "k_info" array */
    v_info = new vault_type[MAX_V_IDX];
    memset(v_info, 0, MAX_V_IDX*sizeof(vault_type));

    /* Hack -- make "fake" arrays */
    v_name = new char[fake_name_size];
    memset(v_name, 0, fake_name_size);
    v_text = new char[fake_text_size];
    memset(v_text, 0, fake_text_size);


    /*** Load the ascii template file ***/

    /* Access the "v_info.txt" file */
    strcpy(buf, "dat/v_info.txt");

    /* Open the file */
    fp = my_fopen(buf, "r");

    /* Parse it */
    if (!fp) quit("Cannot open 'v_info.txt' file.");

    /* Parse the file */
    err = init_v_info_txt(fp, buf);

    /* Close it */
    fclose(fp);

    /* Errors */
    if (err) {
        char oops[500], buf2[500];

        // Error string
        strcpy(oops , (((err > 0) && (err < 8)) ? err_str[err] : "unknown"));

        // Oops
        sprintf(buf2, "Error %d at line %d of 'v_info.txt'.\n"
                      "Record %d contains a '%s' error.\n"
                      "Parsing '%s'.", err, error_line, error_idx, oops, buf);

        // Quit
        quit(buf2);
    }

    /* Success */
    return (0);
}




/*** Initialize others ***/

/*
 * Hack -- Items sold in the stores -- by tval/sval pair.
 */
static byte store_table[MAX_STORES-2][STORE_CHOICES][2] = {
    {
        /* General Store */
        { TV_FOOD, SV_FOOD_RATION },
        { TV_FOOD, SV_FOOD_RATION },
        { TV_FOOD, SV_FOOD_RATION },
        { TV_FOOD, SV_FOOD_RATION },
        { TV_FOOD, SV_FOOD_RATION },
        { TV_FOOD, SV_FOOD_RATION },
        { TV_FOOD, SV_FOOD_BISCUIT },
        { TV_FOOD, SV_FOOD_JERKY },
        { TV_FOOD, SV_FOOD_PINT_OF_WINE },
        { TV_FOOD, SV_FOOD_PINT_OF_ALE },
        { TV_DIGGING, SV_SHOVEL },
        { TV_DIGGING, SV_PICK },
        { TV_CLOAK, SV_CLOAK },
        { TV_CLOAK, SV_CLOAK },
        { TV_CLOAK, SV_CLOAK },
        { TV_CLOAK, SV_CLOAK },
        { TV_POTION, SV_POTION_WATER },
        { TV_POTION, SV_POTION_WATER },
        { TV_POTION, SV_POTION_WATER },
        { TV_POTION, SV_POTION_APPLE_JUICE },
        { TV_LITE, SV_LITE_TORCH },
        { TV_LITE, SV_LITE_TORCH },
        { TV_LITE, SV_LITE_TORCH },
        { TV_LITE, SV_LITE_TORCH },
        { TV_LITE, SV_LITE_LANTERN },
        { TV_LITE, SV_LITE_LANTERN },
        { TV_FLASK, 0 },
        { TV_FLASK, 0 },
        { TV_FLASK, 0 },
        { TV_FLASK, 0 },
        { TV_FLASK, 0 },
        { TV_FLASK, 0 }
    },

    {
        /* Armory */
        { TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS },
        { TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS },
        { TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
        { TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS },
        { TV_HELM, SV_HARD_LEATHER_CAP },
        { TV_HELM, SV_HARD_LEATHER_CAP },
        { TV_HELM, SV_METAL_CAP },
        { TV_HELM, SV_IRON_HELM },
        { TV_SOFT_ARMOR, SV_ROBE },
        { TV_SOFT_ARMOR, SV_ROBE },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR },
        { TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR },
        { TV_SOFT_ARMOR, SV_HARD_STUDDED_LEATHER },
        { TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
        { TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL },
        { TV_HARD_ARMOR, SV_METAL_SCALE_MAIL },
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_HARD_ARMOR, SV_AUGMENTED_CHAIN_MAIL },
        { TV_HARD_ARMOR, SV_BAR_CHAIN_MAIL },
        { TV_HARD_ARMOR, SV_METAL_BRIGANDINE_ARMOR },
        { TV_HARD_ARMOR, SV_DOUBLE_CHAIN_MAIL },
        { TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
        { TV_GLOVES, SV_SET_OF_LEATHER_GLOVES },
        { TV_GLOVES, SV_SET_OF_GAUNTLETS },
        { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
        { TV_SHIELD, SV_SMALL_LEATHER_SHIELD },
        { TV_SHIELD, SV_LARGE_LEATHER_SHIELD },
        { TV_SHIELD, SV_SMALL_METAL_SHIELD }
    },

    {
        /* Weaponsmith */
        { TV_SWORD, SV_DAGGER },
        { TV_SWORD, SV_MAIN_GAUCHE },
        { TV_SWORD, SV_RAPIER },
        { TV_SWORD, SV_SMALL_SWORD },
        { TV_SWORD, SV_SHORT_SWORD },
        { TV_SWORD, SV_SABRE },
        { TV_SWORD, SV_CUTLASS },
        { TV_SWORD, SV_TULWAR },
        { TV_SWORD, SV_BROAD_SWORD },
        { TV_SWORD, SV_LONG_SWORD },
        { TV_SWORD, SV_SCIMITAR },
        { TV_SWORD, SV_KATANA },
        { TV_SWORD, SV_BASTARD_SWORD },
        { TV_POLEARM, SV_SPEAR },
        { TV_POLEARM, SV_AWL_PIKE },
        { TV_POLEARM, SV_TRIDENT },
        { TV_POLEARM, SV_PIKE },
        { TV_POLEARM, SV_BEAKED_AXE },
        { TV_POLEARM, SV_BROAD_AXE },
        { TV_POLEARM, SV_LANCE },
        { TV_POLEARM, SV_BATTLE_AXE },
        { TV_HAFTED, SV_WHIP },
        { TV_BOW, SV_SLING },
        { TV_BOW, SV_SHORT_BOW },
        { TV_BOW, SV_LONG_BOW },
        { TV_BOW, SV_LIGHT_XBOW },
        { TV_SHOT, SV_AMMO_LIGHT },
        { TV_SHOT, SV_AMMO_NORMAL },
        { TV_ARROW, SV_AMMO_NORMAL },
        { TV_ARROW, SV_AMMO_NORMAL },
        { TV_BOLT, SV_AMMO_NORMAL },
        { TV_BOLT, SV_AMMO_NORMAL },
    },

    {
        /* Temple */
        { TV_HAFTED, SV_WHIP },
        { TV_HAFTED, SV_QUARTERSTAFF },
        { TV_HAFTED, SV_MACE },
        { TV_HAFTED, SV_BALL_AND_CHAIN },
        { TV_HAFTED, SV_WAR_HAMMER },
        { TV_HAFTED, SV_LUCERN_HAMMER },
        { TV_HAFTED, SV_MORNING_STAR },
        { TV_HAFTED, SV_FLAIL },
        { TV_HAFTED, SV_LEAD_FILLED_MACE },
        { TV_SCROLL, SV_SCROLL_REMOVE_CURSE },
        { TV_SCROLL, SV_SCROLL_BLESSING },
        { TV_SCROLL, SV_SCROLL_HOLY_CHANT },
        { TV_POTION, SV_POTION_BOLDNESS },
        { TV_POTION, SV_POTION_HEROISM },
        { TV_POTION, SV_POTION_CURE_LIGHT },
        { TV_POTION, SV_POTION_CURE_SERIOUS },
        { TV_POTION, SV_POTION_CURE_SERIOUS },
        { TV_POTION, SV_POTION_CURE_CRITICAL },
        { TV_POTION, SV_POTION_CURE_CRITICAL },
        { TV_POTION, SV_POTION_RESTORE_EXP },
        { TV_POTION, SV_POTION_RESTORE_EXP },
        { TV_POTION, SV_POTION_RESTORE_EXP },
        { TV_PRAYER_BOOK, 0 },
        { TV_PRAYER_BOOK, 0 },
        { TV_PRAYER_BOOK, 0 },
        { TV_PRAYER_BOOK, 0 },
        { TV_PRAYER_BOOK, 1 },
        { TV_PRAYER_BOOK, 1 },
        { TV_PRAYER_BOOK, 1 },
        { TV_PRAYER_BOOK, 2 },
        { TV_PRAYER_BOOK, 2 },
        { TV_PRAYER_BOOK, 3 }
    },

    {
        /* Alchemy shop */
        { TV_SCROLL, SV_SCROLL_ENCHANT_WEAPON_TO_HIT },
        { TV_SCROLL, SV_SCROLL_ENCHANT_WEAPON_TO_DAM },
        { TV_SCROLL, SV_SCROLL_ENCHANT_ARMOR },
        { TV_SCROLL, SV_SCROLL_IDENTIFY },
        { TV_SCROLL, SV_SCROLL_IDENTIFY },
        { TV_SCROLL, SV_SCROLL_IDENTIFY },
        { TV_SCROLL, SV_SCROLL_IDENTIFY },
        { TV_SCROLL, SV_SCROLL_LIGHT },
        { TV_SCROLL, SV_SCROLL_PHASE_DOOR },
        { TV_SCROLL, SV_SCROLL_PHASE_DOOR },
        { TV_SCROLL, SV_SCROLL_PHASE_DOOR },
        { TV_SCROLL, SV_SCROLL_MONSTER_CONFUSION },
        { TV_SCROLL, SV_SCROLL_MAPPING },
        { TV_SCROLL, SV_SCROLL_DETECT_GOLD },
        { TV_SCROLL, SV_SCROLL_DETECT_ITEM },
        { TV_SCROLL, SV_SCROLL_DETECT_TRAP },
        { TV_SCROLL, SV_SCROLL_DETECT_DOOR },
        { TV_SCROLL, SV_SCROLL_DETECT_INVIS },
        { TV_SCROLL, SV_SCROLL_RECHARGING },
        { TV_SCROLL, SV_SCROLL_SATISFY_HUNGER },
        { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
        { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
        { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
        { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL },
        { TV_POTION, SV_POTION_RESIST_HEAT },
        { TV_POTION, SV_POTION_RESIST_COLD },
        { TV_POTION, SV_POTION_RES_STR },
        { TV_POTION, SV_POTION_RES_INT },
        { TV_POTION, SV_POTION_RES_WIS },
        { TV_POTION, SV_POTION_RES_DEX },
        { TV_POTION, SV_POTION_RES_CON },
        { TV_POTION, SV_POTION_RES_CHR }
    },

    {
        /* Magic-User store */
        { TV_RING, SV_RING_SEARCHING },
        { TV_RING, SV_RING_FEATHER_FALL },
        { TV_RING, SV_RING_PROTECTION },
        { TV_AMULET, SV_AMULET_CHARISMA },
        { TV_AMULET, SV_AMULET_SLOW_DIGEST },
        { TV_AMULET, SV_AMULET_RESIST_ACID },
        { TV_WAND, SV_WAND_SLOW_MONSTER },
        { TV_WAND, SV_WAND_CONFUSE_MONSTER },
        { TV_WAND, SV_WAND_SLEEP_MONSTER },
        { TV_WAND, SV_WAND_MAGIC_MISSILE },
        { TV_WAND, SV_WAND_STINKING_CLOUD },
        { TV_WAND, SV_WAND_WONDER },
        { TV_STAFF, SV_STAFF_LITE },
        { TV_STAFF, SV_STAFF_MAPPING },
        { TV_STAFF, SV_STAFF_DETECT_TRAP },
        { TV_STAFF, SV_STAFF_DETECT_DOOR },
        { TV_STAFF, SV_STAFF_DETECT_GOLD },
        { TV_STAFF, SV_STAFF_DETECT_ITEM },
        { TV_STAFF, SV_STAFF_DETECT_INVIS },
        { TV_STAFF, SV_STAFF_DETECT_EVIL },
        { TV_STAFF, SV_STAFF_TELEPORTATION },
        { TV_STAFF, SV_STAFF_IDENTIFY },
        { TV_MAGIC_BOOK, 0 },
        { TV_MAGIC_BOOK, 0 },
        { TV_MAGIC_BOOK, 0 },
        { TV_MAGIC_BOOK, 0 },
        { TV_MAGIC_BOOK, 1 },
        { TV_MAGIC_BOOK, 1 },
        { TV_MAGIC_BOOK, 1 },
        { TV_MAGIC_BOOK, 2 },
        { TV_MAGIC_BOOK, 2 },
        { TV_MAGIC_BOOK, 3 }
    }
};




/*
 * Initialize some other arrays
 */
static errr init_other(void)
{
    int i, j, k;

    CObjectKind *k_ptr;
    CMonsterRace *r_ptr;

    s16b aux[256];


    /*** Prepare the "dungeon" information ***/

    // Allocate a player object
    p_ptr = new CPlayer;

    // Allocate the player's inventory
    inventory = new CItem[INVEN_TOTAL];


    /* Allocate and wipe each line of the cave */
    for (i = 0; i < MAX_HGT; i++) {
        /* Allocate one row of the cave */
        cave[i] = new CGrid[MAX_WID];
    }



    /*** Prepare the Item Kind Allocator ***/

    /* Clear the aux array */
    memset(aux, 0, MAX_DEPTH*sizeof(s16b));

    /* Make the index */
    alloc_kind_index = new s16b[MAX_DEPTH];
    memset(alloc_kind_index, 0, MAX_DEPTH*sizeof(s16b));

    /* Scan all of the objects */
    for (i = 1; i < MAX_K_IDX; i++) {
        /* Get the i'th object */
        k_ptr = &k_info[i];

        /* Scan all of the locale/chance pairs */
        for (j = 0; j < 4; j++) {
            /* Count valid pairs */
            if (k_ptr->chance[j] && (k_ptr->locale[j] < MAX_DEPTH)) {
                /* Count the total entries */
                alloc_kind_size++;

                /* Count the entries at each level */
                alloc_kind_index[k_ptr->locale[j]]++;
            }
        }
    }

    // Combine the "alloc_kind_index" entries
    for (i = 1; i < MAX_DEPTH; i++) {
        alloc_kind_index[i] += alloc_kind_index[i-1];
    }

    /* Allocate the table */
    alloc_kind_table = new kind_entry[alloc_kind_size];
    memset(alloc_kind_table, 0, alloc_kind_size*sizeof(kind_entry));

    /* Initialize the table */
    for (i = 1; i < MAX_K_IDX; i++) {
        /* Get the i'th object */
        k_ptr = &k_info[i];

        /* Scan all of the locale/chance pairs */
        for (j = 0; j < 4; j++) {
            /* Count valid pairs */
            if (k_ptr->chance[j] && (k_ptr->locale[j] < MAX_DEPTH)) {
                int r, x, y, z;

                /* Extract the chance/locale */
                r = k_ptr->chance[j];
                x = k_ptr->locale[j];

                /* Skip entries preceding our locale */
                y = (x > 0) ? alloc_kind_index[x-1] : 0;

                /* Skip previous entries at this locale */
                z = y + aux[x];

                /* Load the table entry */
                alloc_kind_table[z].k_idx = i;
                alloc_kind_table[z].locale = x;
                alloc_kind_table[z].chance = r;

                /* Another entry complete for this locale */
                aux[x]++;
            }
        }
    }

    /* Paranoia */
    if (!alloc_kind_index[0]) quit("No town objects!");


    /*** Prepare the Monster Race Allocator ***/

    // Clear the aux array
    memset(aux, 0, MAX_DEPTH*sizeof(s16b));

    // Allocate and clear the index
    alloc_race_index = new s16b[MAX_DEPTH];
    memset(alloc_race_index, 0, MAX_DEPTH*sizeof(s16b));

    // Scan the monsters
    for (i = 1; i < MAX_R_IDX; i++) {
        /* Get the i'th race */
        r_ptr = &r_info[i];

        /* Process "real" monsters */
        if (r_ptr->rarity && (r_ptr->level < MAX_DEPTH)) {
            /* Count the total entries */
            alloc_race_size++;

            /* Count the entries at each level */
            alloc_race_index[r_ptr->level]++;
        }
    }

    /* Combine the "alloc_race_index" entries */
    for (i = 1; i < MAX_DEPTH; i++) {
        alloc_race_index[i] += alloc_race_index[i-1];
    }

    // Allocate the alloc_race_table
    alloc_race_table = new race_entry[alloc_race_size];
    memset(alloc_race_table, 0, alloc_race_size*sizeof(race_entry));

    /* Scan the monsters */
    for (i = 1; i < MAX_R_IDX; i++) {
        // Get the i'th race
        r_ptr = &r_info[i];

        // Count valid pairs
        if (r_ptr->rarity && (r_ptr->level < MAX_DEPTH)) {
            int r, x, y, z;

            /* Extract the level/rarity */
            x = r_ptr->level;
            r = r_ptr->rarity;

            /* Skip entries preceding our locale */
            y = (x > 0) ? alloc_race_index[x-1] : 0;

            /* Skip previous entries at this locale */
            z = y + aux[x];

            /* Load the table entry */
            alloc_race_table[z].r_idx = i;
            alloc_race_table[z].locale = x;
            alloc_race_table[z].chance = r;

            /* Another entry complete for this locale */
            aux[x]++;
        }
    }

    /* Paranoia */
    if (!alloc_race_index[0]) quit("No town monsters!");


    /*** Prepare the Stores ***/

    /* Allocate the stores */
    store = new store_type[MAX_STORES];
    memset(store, 0, MAX_STORES*sizeof(store_type));

    /* Fill in each store */
    for (i = 0; i < MAX_STORES; i++) {
        /* Access the store */
        store_type *st_ptr = &store[i];

        /* Assume full stock */
        st_ptr->stock_size = STORE_INVEN_MAX;

        /* Allocate the stock */
        st_ptr->stock = new CItem[st_ptr->stock_size];

        /* No table for the black market or home */
        if ((i == 6) || (i == 7)) continue;

        /* Assume full table */
        st_ptr->table_size = STORE_CHOICES;

        /* Allocate the stock */
        st_ptr->table = new s16b[st_ptr->table_size];

        /* Scan the choices */
        for (k = 0; k < STORE_CHOICES; k++) {
            int k_idx;

            // Extract the tval/sval codes
            int tv = store_table[i][k][0];
            int sv = store_table[i][k][1];

            // Look for it
            for (k_idx = 1; k_idx < MAX_K_IDX; k_idx++) {
                k_ptr = &k_info[k_idx];

                /* Found a match */
                if ((k_ptr->tval == tv) && (k_ptr->sval == sv)) break;
            }

            /* Catch errors */
            if (k_idx == MAX_K_IDX) continue;

            /* Add that item index to the table */
            st_ptr->table[st_ptr->table_num++] = k_idx;
        }
    }


    /*** Pre-allocate space for the "format()" buffer ***/

    /* Hack -- Just call the "format()" function */
    format("%s (%s).", "Matt Craighead", "craighea@citilink.com");


    /* Success */
    return (0);
}



/*
 * Initialize various Angband variables and arrays.
 *
 * Note that the "template" files are initialized first, since they
 * often contain errors.  This means that macros and message recall
 * and things like that are not available until after they are done.
 */
void init_some_arrays(void)
{
    // Load tiles
    FILE *fp;
    char tile[50];
    
    // load tiles in list
    fp = fopen ("dat/tiles.txt","r");
    while(fgets(tile,50,fp))
    {
	*(strpbrk(tile,"\n"))='\0'; // remove '\n' at the end
        load_tile(tile);
    }
    sort_tiles();

    // Initialize the arrays
    if (init_f_info()) quit("Cannot initialize features");
    if (init_k_info()) quit("Cannot initialize objects");
    if (init_a_info()) quit("Cannot initialize artifacts");
    if (init_e_info()) quit("Cannot initialize ego-items");
    if (init_r_info()) quit("Cannot initialize monsters");
    if (init_v_info()) quit("Cannot initialize vaults");
    if (init_other()) quit("Cannot initialize arrays");

    // Load options
    read_options();

    // Initialize the console
    init_console();
}
