// File: birth.cpp
// Purpose: create a player character

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"


// Base stats array
int base_stats[6];

// Costs of various stats -- MJC
int stat_points[18] = {
    0,      // 0
    0,      // 1
    0,      // 2
    0,      // 3
    0,      // 4
    0,      // 5
    0,      // 6
    0,      // 7
    -4,     // 8
    -3,     // 9
    -2,     // 10
    -1,     // 11
    0,      // 12
    2,      // 13
    4,      // 14
    6,      // 15
    9,      // 16
    12,     // 17
};


/*
 * Display the character creation title header.
 */
static void show_create_header()
{
    put_text_format(320, 0, "Character Creation", COLOR_WHITE, FONT_BOLD, JUST_CENTER);
}


/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
    int i, bonus;

    // Acquire the stats
    for (i = 0; i < 6; i++) {
        // Save that value
        p_ptr->SetStatMax(i, base_stats[i]);

        // Obtain a "bonus" for "race" and "class"
        bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

        // Start fully healed
        p_ptr->SetStatCur(i, p_ptr->GetStatMax(i));
    }
}


/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(void)
{
    int i, j, min_value, max_value, hitdie;


    // Level one (never zero!)
    p_ptr->SetLev(1);

    // Hitdice
    hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp;


    // Minimum hitpoints at highest level
    min_value = (PY_MAX_LEVEL * (hitdie - 1) * 3) / 8;
    min_value += PY_MAX_LEVEL;

    /* Maximum hitpoints at highest level */
    max_value = (PY_MAX_LEVEL * (hitdie - 1) * 5) / 8;
    max_value += PY_MAX_LEVEL;

    /* Pre-calculate level 1 hitdice */
    p_ptr->player_hp[0] = hitdie;

    /* Roll out the hitpoints */
    while (TRUE) {
        /* Roll the hitpoint values */
        for (i = 1; i < PY_MAX_LEVEL; i++) {
            j = randint(hitdie);
            p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + j;
        }

        // Require "valid" hitpoints at highest level
        if (p_ptr->player_hp[PY_MAX_LEVEL-1] < min_value) continue;
        if (p_ptr->player_hp[PY_MAX_LEVEL-1] > max_value) continue;

        // Acceptable 
        break;
    }
}


/*
 * Clear all the global "character" data
 */
static void player_wipe()
{
    int i;


    //: Zero the struct by deleting and then remaking it
    delete p_ptr;
    p_ptr = new CPlayer;


    /* No items */
    inven_cnt = 0;
    equip_cnt = 0;

    /* Clear the inventory */
    for (i = 0; i < INVEN_TOTAL; i++) {
        inventory[i].wipe();
    }


    /* Start with no artifacts made yet */
    for (i = 0; i < MAX_A_IDX; i++) {
        artifact_type *a_ptr = &a_info[i];
        a_ptr->cur_num = 0;
    }


    /* Start with no quests */
    for (i = 0; i < MAX_Q_IDX; i++) {
        q_list[i].level = 0;
    }

    // Add the four quests
    q_list[0].level = 50;
    q_list[1].level = 80;
    q_list[2].level = 99;
    q_list[3].level = 100;


    /* Reset the "objects" */
    for (i = 1; i < MAX_K_IDX; i++) {
        CObjectKind *k_ptr = &k_info[i];

        /* Reset "tried" */
        k_ptr->tried = FALSE;

        /* Reset "aware" */
        k_ptr->aware = FALSE;
    }


    /* Reset the "monsters" */
    for (i = 1; i < MAX_R_IDX; i++) {
        CMonsterRace *r_ptr = &r_info[i];

        /* Hack -- Reset the counter */
        r_ptr->cur_num = 0;

        /* Hack -- Reset the max counter */
        r_ptr->max_num = 100;

        /* Hack -- Reset the max counter */
        if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;

        /* Clear player kills */
        r_ptr->r_pkills = 0;
    }


    /* Hack -- Well fed player */
    p_ptr->SetFood(PY_FOOD_FULL - 1);

    // 100 gold
    p_ptr->SetGold(100);


    /* Wipe the spells */
    for (i = 0; i < 64; i++) {
        spell_learned[i] = FALSE;
        spell_worked[i] = FALSE;
        spell_forgotten[i] = FALSE;
        spell_order[i] = 99;
    }


    /* Assume no winning game */
    total_winner = FALSE;

    /* Assume no panic save */
    panic_save = 0;

    // No cheating
    p_ptr->SetNoScore(FALSE);

    // No name
    strcpy(player_name, "");
}




/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static byte player_init[MAX_CLASS][3][2] = {
    {
        // Warrior
        { TV_SHIELD, SV_SMALL_METAL_SHIELD },
        { TV_SWORD, SV_BROAD_SWORD },
        { TV_HARD_ARMOR, SV_CHAIN_MAIL }
    },

    {
        // Mage
        { TV_MAGIC_BOOK, 0 },
        { TV_SWORD, SV_DAGGER },
        { TV_SCROLL, SV_SCROLL_WORD_OF_RECALL }
    },

    {
        // Priest
        { TV_PRAYER_BOOK, 0 },
        { TV_HAFTED, SV_MACE },
        { TV_POTION, SV_POTION_HEALING }
    },

    {
        // Rogue
        { TV_MAGIC_BOOK, 0 },
        { TV_SWORD, SV_SMALL_SWORD },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR }
    },

    {
        // Ranger
        { TV_MAGIC_BOOK, 0 },
        { TV_SWORD, SV_BROAD_SWORD },
        { TV_BOW, SV_LONG_BOW }
    },

    {
        // Paladin
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
static void player_outfit()
{
    int i, tv, sv;
    CItem *i_ptr;

    // Hack -- Give the player some food
    i_ptr = new CItem(TV_FOOD, SV_FOOD_RATION);
    i_ptr->SetNumber(5);
    i_ptr->object_aware();
    i_ptr->MakeKnown();
    inven_carry(i_ptr);
    delete i_ptr;

    // Hack -- Give the player some torches
    i_ptr = new CItem(TV_LITE, SV_LITE_TORCH);
    i_ptr->SetNumber(5);
    i_ptr->SetPval(2500);
    i_ptr->object_aware();
    i_ptr->MakeKnown();
    inven_carry(i_ptr);
    delete i_ptr;

    // Hack -- Give the player three useful objects
    for (i = 0; i < 3; i++) {
        tv = player_init[p_ptr->GetClass()][i][0];
        sv = player_init[p_ptr->GetClass()][i][1];
        i_ptr = new CItem(tv, sv);
        i_ptr->object_aware();
        i_ptr->MakeKnown();
        inven_carry(i_ptr);
        delete i_ptr;
    }
}


/*
 * Create a character.  Then wait for a moment.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
bool player_birth(void)
{
    int i, mx, my;
    char c, out_val[80];
    int points_used;
    int gender, prace, pclass;
    CRadioButton *radios[MAX_CLASS];
    CText *texts[MAX_CLASS];
    byte *screen;

    // Clear old information
    player_wipe();

    // Clear info
    gender = 0;
    prace = 0;
    pclass = 0;

    // Get basic player info
    //: Player names can be too big if all M
    CWindow *window = new CWindow(170, 100, 300, 210, "Character Info");

    window->Attach(new CText(180, 123, "Character name:", FONT_BOLD, JUST_LEFT));
    CTextField *text = new CTextField(180, 141, 120, player_name, 16);
    window->Attach(text);

    window->Attach(new CRadioButton(360, 133, &gender, 0));
    window->Attach(new CText(380, 133, "Male", FONT_BOLD, JUST_LEFT));
    window->Attach(new CRadioButton(360, 149, &gender, 1));
    window->Attach(new CText(380, 149, "Female", FONT_BOLD, JUST_LEFT));

    for (i = 0; i < MAX_RACES; i++) {
        window->Attach(new CText(200+(i/5)*80, 185+(i%5)*16, race_info[i].title,
            FONT_BOLD, JUST_LEFT));
        window->Attach(new CRadioButton(180+(i/5)*80, 185+(i%5)*16, &prace, i));
    }

    for (i = 0; i < MAX_CLASS; i++) {
        texts[i] = new CText(380, 177+i*16, class_info[i].title, FONT_BOLD, JUST_LEFT);
        window->Attach(texts[i]);
        radios[i] = new CRadioButton(360, 177+i*16, &pclass, i);
        window->Attach(radios[i]);
    }

    CButton *ok = new CButton(240, 283, 60, 20, "OK");
    CButton *cancel = new CButton(340, 283, 60, 20, "Cancel");
    window->Attach(ok); window->Attach(cancel);
    screen = save_screen();
    screen_refresh(); 
    for (;;) {

        // Enable/disable each radio button
        rp_ptr = &race_info[prace];
        for (i = 0; i < MAX_CLASS; i++) {
            if (rp_ptr->choice & (1 << i)) {
                radios[i]->Enable();
                texts[i]->Enable();
            }
            else {
                radios[i]->Disable();
                texts[i]->Disable();
            }
        }
        if (!radios[pclass]->isEnabled()) pclass = 0;
	gui_draw(window);
        
	c = scan_inkey_scan(); 
        if (c) {
            if (c == KEY_ENTER) break;
            if (c == KEY_ESCAPE) {
                delete window;
                delete[] screen;
                return FALSE;
            }
            text->ProcessChar(c);
        }
        if (get_last_left_button_release(&mx, &my)) {
            window->Click(mx, my);
            if (ok->inClientArea(mx, my)) break;
            if (cancel->inClientArea(mx, my)) {
                delete window;
                delete[] screen;
                return FALSE;
            }
        }
    }
    delete window;
    restore_screen(screen);
    delete[] screen;
    // Keep some results
    p_ptr->SetMale(gender == 0);
    p_ptr->SetRace(prace);
    rp_ptr = &race_info[p_ptr->GetRace()];
    p_ptr->SetClass(pclass);
    cp_ptr = &class_info[p_ptr->GetClass()];
    mp_ptr = &magic_info[p_ptr->GetClass()];
    // Clear the screen
    blank_screen(COLOR_BLACK);
    // Title everything
    show_create_header();

    // Set up base stats
    for (i = 0; i < 6; i++) base_stats[i] = 12;


    // Roll for base hitpoints
    get_extra();

    // Actually Generate
    while (TRUE) {
        // Get some stats
        get_stats();

        // Calculate the bonuses, HP, and SP
        p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_HP | PU_MANA);

        // Update stuff
        update_stuff();

        // Heal up the player
        p_ptr->SetCSP(p_ptr->GetMSP());
        p_ptr->SetCHP(p_ptr->GetMHP());

        // Display the player
        display_player();

        // Add "character creation" header
        show_create_header();

        // Calculate points used
        points_used = 0;
        for (i = 0; i < 6; i++) points_used += stat_points[base_stats[i]];
        // Prepare a prompt (must squeeze everything in)
        put_string(2*8, 21*16,
            "'S'/'s' to modify STR; 'I'/'i' to modify INT; 'W'/'w' to modify WIS",
            COLOR_WHITE);
        put_string(2*8, 22*16,
            "'D'/'d' to modify DEX; 'C'/'c' to modify CON; 'R'/'r' to modify CHR",
            COLOR_WHITE);
        sprintf(out_val, "Points used: %d of 20", points_used);
        put_string(2*8, 23*16, out_val, (points_used <= 20) ? COLOR_WHITE : COLOR_YELLOW);
        strcpy(out_val, "Hit Enter to accept, Esc to cancel");
        put_string(69*8-8*strlen(out_val), 23*16, out_val, COLOR_WHITE);

        // Prompt and get a command
        screen_refresh();
        c = scan_inkey();

        // Enter accepts the roll
        if (c == KEY_ENTER) {
            if (points_used <= 20) break;
            else {
                bell();
                continue;
            }
        }

        // Escape cancels the character generation
        if (c == KEY_ESCAPE) return FALSE;

        // Other things
        switch (convert(c, get_shift(), get_capslock())) {
            // Modify STR
            case 'S':
                if (base_stats[0] < 17) base_stats[0]++;
                break;
            case 's':
                if (base_stats[0] > 8) base_stats[0]--;
                break;

            // Modify INT
            case 'I':
                if (base_stats[1] < 17) base_stats[1]++;
                break;
            case 'i':
                if (base_stats[1] > 8) base_stats[1]--;
                break;

            // Modify WIS
            case 'W':
                if (base_stats[2] < 17) base_stats[2]++;
                break;
            case 'w':
                if (base_stats[2] > 8) base_stats[2]--;
                break;

            /* Modify DEX */
            case 'D':
                if (base_stats[3] < 17) base_stats[3]++;
                break;
            case 'd':
                if (base_stats[3] > 8) base_stats[3]--;
                break;

            /* Modify CON */
            case 'C':
                if (base_stats[4] < 17) base_stats[4]++;
                break;
            case 'c':
                if (base_stats[4] > 8) base_stats[4]--;
                break;

            /* Modify CHR */
            case 'R':
                if (base_stats[5] < 17) base_stats[5]++;
                break;
            case 'r':
                if (base_stats[5] > 8) base_stats[5]--;
                break;
        }
    }


    // Hack -- outfit the player
    player_outfit();

    // Init the stores
    store_init();

    // Maintain the stores (ten times)
    for (i = 0; i < 10; i++) store_maint();

    // Start at turn 1
    game_turn = 1;

    // Succeeded
    return TRUE;
}

