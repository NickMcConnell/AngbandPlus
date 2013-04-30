/*
 * File: ui-birth.c
 * Purpose: Text-based user interface for character creation
 *
 * Copyright (c) 1987 - 2007 Angband contributors
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
#include "../common/md5.h"
#include "../common/randname.h"
#include "../common/tvalsval.h"
#include "ui-menu.h"


/*
 * Overview
 * ========
 * This file implements the user interface side of the birth process
 * for the classic terminal-based UI of Angband.
 *
 * It models birth as a series of steps which must be carried out in 
 * a specified order, with the option of stepping backwards to revisit
 * past choices.
 */


/*
 * A local-to-this-file global to hold the most important bit of state
 * between calls to the game proper.  Probably not strictly necessary,
   but reduces complexity a bit.
   */
typedef enum birth_stage
{
    BIRTH_BACK = -1,
    BIRTH_RESET = 0,
    BIRTH_SEX_CHOICE,
    BIRTH_RACE_CHOICE,
    BIRTH_CLASS_CHOICE,
    BIRTH_ROLLER_CHOICE,
    BIRTH_ROLLER,
    BIRTH_FINAL_CONFIRM,
    BIRTH_COMPLETE,
    BIRTH_QUIT
} birth_stage;


/*
 * Allows quick creation based on the previous character
 */
static s16b quick_start;


/*
 * Roller type
 */
static byte roller_type = 0;


/*
 * A "keypress" handling function for askfor_aux, that handles the special
 * case of '*' for a new random "name" and passes any other "keypress"
 * through to the default "editing" handler.
 */
static bool get_name_keypress(char *buf, size_t buflen, size_t *curs,
    size_t *len, struct keypress keypress, bool firsttime)
{
    bool result;

    switch (keypress.code)
    {
        case '*':
        {
            *len = randname_make(RANDNAME_TOLKIEN, 4, 8, buf, buflen, name_sections);
            my_strcap(buf);
            *curs = 0;
            result = FALSE;
            break;
        }
        
        default:
        {
            result = askfor_aux_keypress(buf, buflen, curs, len, keypress, firsttime);
            break;
        }
    }

    return result;
}


/*
 * Choose the character's name
 */
static void choose_name(void)
{
    char tmp[23];

    /* Prompt and ask */
    prt("Enter your player's name above (* for a random name, or hit ESCAPE).", 21, 2);

    /* Ask until happy */
    while (1)
    {
        /* Go to the "name" area */
        Term_gotoxy(15, 2);

        /* Save the player name */
        my_strcpy(tmp, nick, sizeof(tmp));

        /* Ask the user for a string */
        if (askfor_aux(tmp, 16, get_name_keypress))
            my_strcpy(nick, tmp, sizeof(nick));

        /* All done */
        break;
    }

    /* Capitalize the name */
    my_strcap(nick);

    /* Pad the name (to clear junk) */
    strnfmt(tmp, sizeof(tmp), "%-15.15s", nick);

    /* Redraw the name (in light blue) */
    c_put_str(TERM_L_BLUE, tmp, 2, 15);

    /* Erase the prompt, etc */
    clear_from(20);
}


/*
 * Choose a password
 */
static void enter_password(void)
{
    int c;
    char tmp[MAX_PASS_LEN];

    /* Prompt and ask */
    prt("Enter your password above (or hit ESCAPE).", 21, 2);

    /* Default */
    my_strcpy(tmp, pass, sizeof(tmp));

    /* Ask until happy */
    while (1)
    {
        /* Go to the "password" area */
        Term_gotoxy(15, 3);

        /* Get an input, ignore "Escape" */
        if (!askfor_ex(tmp, 16, NULL, TRUE))
        {
            if (!strcmp(tmp, "passwd"))
            {
                prt("Please do not use `passwd` as your password.", 22, 2);
                continue;
            }
            else
                my_strcpy(pass, tmp, sizeof(pass));
        }

        /* All done */
        break;
    }

    /* Redraw the password (in light blue) */
    for (c = 0; c < strlen(pass); c++)
        Term_putch(15 + c, 3, TERM_L_BLUE, 'x');

    /* Now hash that sucker! */
    my_strcpy(stored_pass, pass, sizeof(stored_pass));
    MD5Password(stored_pass);

    /* Erase the prompt, etc */
    clear_from(20);
}


/* ------------------------------------------------------------------------
 * The various "menu" bits of the birth process - namely choice of sex,
 * race, class, and roller type.
 * ------------------------------------------------------------------------ */


/* The various menus */
static menu_type sex_menu, race_menu, class_menu, roller_menu;


/* Locations of the menus, etc. on the screen */
#define QUESTION_ROW    7
#define TABLE_ROW       9

#define QUESTION_COL    2
#define SEX_COL         2
#define RACE_COL        14
#define RACE_AUX_COL    29
#define CLASS_COL       29
#define CLASS_AUX_COL   45
#define ROLLER_COL      45

#define MENU_ROWS TABLE_ROW + 15


/* Upper left column and row, width, and lower column */
static region gender_region = {SEX_COL, TABLE_ROW, 14, MENU_ROWS};
static region race_region = {RACE_COL, TABLE_ROW, 15, MENU_ROWS};
static region class_region = {CLASS_COL, TABLE_ROW, 16, MENU_ROWS};
static region roller_region = {ROLLER_COL, TABLE_ROW, 28, MENU_ROWS};


/*
 * We use different menu "browse functions" to display the help text
 * sometimes supplied with the menu items - currently just the list
 * of bonuses, etc, corresponding to each race and class.
 */
typedef void (*browse_f) (int oid, void *db, const region *l);


/*
 * We have one of these structures for each menu we display - it holds
 * the useful information for the menu - text of the menu items, "help"
 * text, current (or default) selection, and whether random selection
 * is allowed.
 */
typedef struct birthmenu_data
{
    const char **items;
    const char *hint;
    bool allow_random;
    u16b choice;
} birthmenu_data;


/*
 * A custom "display" function for our menus that simply displays the
 * text from our stored data in a different colour if it's currently
 * selected.
 */
static void birthmenu_display(menu_type *menu, int oid, bool cursor,
    int row, int col, int width)
{
    struct birthmenu_data *data = menu_priv(menu);
    byte attr = curs_attrs[0 != (data->choice & (1L << oid))][0 != cursor];

    c_put_str(attr, data->items[oid], row, col);
}


/*
 * Our custom menu iterator, only really needed to allow us to override
 * the default handling of "commands" in the standard iterators (hence
 * only defining the display and handler parts).
 */
static const menu_iter birth_iter = {NULL, NULL, birthmenu_display, NULL, NULL};


static void format_help(int col, int row, const char *fmt, ...)
{
    char buf[NORMAL_WID];
    va_list vp;

    va_start(vp, fmt);
    vstrnfmt(buf, sizeof(buf), fmt, vp);
    va_end(vp);

    Term_putstr(col, TABLE_ROW + row, -1, TERM_WHITE, buf);
}


static void erase_help(int col, int *row)
{
     Term_erase(col, TABLE_ROW + *row, NORMAL_WID);
     (*row)++;
}


static void skill_help(int col, int *row, s16b skills[], int mhp, int exp, int infra)
{
    format_help(col, (*row)++, "Hit/Shoot/Throw: %+3d/%+3d/%+3d", skills[SKILL_TO_HIT_MELEE],
        skills[SKILL_TO_HIT_BOW], skills[SKILL_TO_HIT_THROW]);
    format_help(col, (*row)++, "Hit die: %2d     XP mod: %3d%%", mhp, exp);
    format_help(col, (*row)++, "Disarm: %+3d     Devices: %+3d", skills[SKILL_DISARM],
        skills[SKILL_DEVICE]);
    format_help(col, (*row)++, "Save:   %+3d     Stealth: %+3d", skills[SKILL_SAVE],
        skills[SKILL_STEALTH]);
    if (infra >= 0)
        format_help(col, (*row)++, "Infravision:           %2d ft", infra * 10);
    format_help(col, (*row)++, "Digging:                 %+3d", skills[SKILL_DIGGING]);
    format_help(col, (*row)++, "Search:               %+3d/%2d", skills[SKILL_SEARCH],
        skills[SKILL_SEARCH_FREQUENCY]);
    if (infra < 0) erase_help(col, row);
}


static const char *get_flag_desc(bitflag flag)
{
    switch (flag)
    {
        case OF_SUST_STR: return "Sustains strength";
        case OF_SUST_DEX: return "Sustains dexterity";
        case OF_SUST_CON: return "Sustains constitution";
        case OF_RES_POIS: return "Resists poison";
        case OF_RES_LIGHT: return "Resists light damage";
        case OF_RES_DARK: return "Resists darkness damage";
        case OF_RES_BLIND: return "Resists blindness";
        case OF_HOLD_LIFE: return "Sustains experience";
        case OF_FREE_ACT: return "Resists paralysis";
        case OF_REGEN: return "Regenerates quickly";
        case OF_SEE_INVIS: return "Sees invisible creatures";
        case OF_FEATHER: return "Falls like a feather";
        case OF_SLOW_DIGEST: return "Digests food slowly";
        case OF_RES_NEXUS: return "Resists nexus";

        default: return "Undocumented flag";
    }
}


static const char *get_pflag_desc(bitflag flag)
{
    switch (flag)
    {
        case PF_EXTRA_SHOT: return "Gains extra shots with bow";
        case PF_BRAVERY_30: return "Gains immunity to fear";
        case PF_BLESS_WEAPON: return "Prefers blunt/blessed weapons";
        case PF_CUMBER_GLOVE: return NULL;
        case PF_ZERO_FAIL: return "Advanced spellcasting";
        case PF_BEAM: return NULL;
        case PF_CHOOSE_SPELLS: return NULL;
        case PF_PSEUDO_ID_IMPROV: return NULL;
        case PF_KNOW_MUSHROOM: return "Identifies mushrooms";
        case PF_KNOW_ZAPPER: return "Identifies magic devices";
        case PF_SEE_ORE: return "Senses ore/minerals";
        case PF_ORC: return NULL;
        case PF_TROLL: return NULL;
        case PF_ANIMAL: return NULL;
        case PF_GIANT: return NULL;
        case PF_THUNDERLORD: return NULL;
        case PF_DRAGON: return NULL;
        case PF_BACK_STAB: return NULL;
        case PF_STEALING_IMPROV: return NULL;
        case PF_SPEED_BONUS: return NULL;
        case PF_STEALTH_MODE: return NULL;
        case PF_EXTRA_MANA: return NULL;
        case PF_ANTIMAGIC: return NULL;
        case PF_EXTRA_SHOTS: return NULL;
        case PF_MARTIAL_ARTS: return NULL;
        case PF_ELEMENTAL_SPELLS: return NULL;
        case PF_UNDEAD_POWERS: return NULL;
        case PF_MONSTER_SPELLS: return NULL;
        case PF_SUMMON_SPELLS: return NULL;

        default: return "Undocumented pflag";
    }
}


/*
 * Display additional information about each race during the selection.
 */
static void race_help(int i, void *db, const region *l)
{
    int j;
    size_t k;
    struct player_race *r = player_id2race(i);
    int len = (A_MAX + 1) / 2;
    int n_flags = 0;
    int flag_space = 3;

    if (!r) return;

    /* Display relevant details. */
    for (j = 0; j < len; j++)
    {
        const char *name1 = stat_names_reduced[j];
        const char *name2 = stat_names_reduced[j + len];
        int adj1 = r->r_adj[j];
        int adj2 = r->r_adj[j + len];

        format_help(RACE_AUX_COL, j, "%s%+3d  %s%+3d", name1, adj1, name2, adj2);
    }

    skill_help(RACE_AUX_COL, &j, r->r_skills, r->r_mhp, r->r_exp, r->infra);

    for (k = 0; k < OF_MAX; k++)
    {
        if (n_flags >= flag_space) break;
        if (!of_has(r->flags, k)) continue;
        format_help(RACE_AUX_COL, j++, "%-30s", get_flag_desc(k));
        n_flags++;
    }

    for (k = 0; k < PF__MAX; k++)
    {
        const char *s;

        if (n_flags >= flag_space) break;
        if (!pf_has(r->pflags, k)) continue;
        s = get_pflag_desc(k);
        if (!s) continue;
        format_help(RACE_AUX_COL, j++, "%-30s", s);
        n_flags++;
    }

    while (n_flags < flag_space)
    {
        erase_help(RACE_AUX_COL, &j);
        n_flags++;
    }
}


/*
 * Display additional information about each class during the selection.
 */
static void class_help(int i, void *db, const region *l)
{
    int j;
    size_t k;
    struct player_class *c = player_id2class(i);
    int len = (A_MAX + 1) / 2;
    int n_flags = 0;
    int flag_space = 5;

    if (!c) return;

    /* Display relevant details. */
    for (j = 0; j < len; j++)
    {
        const char *name1 = stat_names_reduced[j];
        const char *name2 = stat_names_reduced[j + len];
        int adj1 = c->c_adj[j] + p_ptr->race->r_adj[j];
        int adj2 = c->c_adj[j + len] + p_ptr->race->r_adj[j + len];

        format_help(CLASS_AUX_COL, j, "%s%+3d  %s%+3d", name1, adj1, name2, adj2);
    }

    skill_help(CLASS_AUX_COL, &j, c->c_skills, c->c_mhp, c->c_exp, -1);

    switch (c->spell_book)
    {
        case TV_MAGIC_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns arcane magic");
            break;
        case TV_PRAYER_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns divine magic");
            break;
        case TV_SORCERY_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns sorcery magic");
            break;
        case TV_SHADOW_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns shadow magic");
            break;
        case TV_HUNT_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns hunting magic");
            break;
        case TV_PSI_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns psi powers");
            break;
        case TV_DEATH_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns death magic");
            break;
        case TV_ELEM_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns elemental magic");
            break;
        case TV_SUMMON_BOOK:
            format_help(CLASS_AUX_COL, j++, "%-30s", "Learns summon magic");
            break;
    }

    for (k = 0; k < PF__MAX; k++)
    {
        const char *s;

        if (n_flags >= flag_space) break;
        if (!pf_has(c->pflags, k)) continue;
        s = get_pflag_desc(k);
        if (!s) continue;
        format_help(CLASS_AUX_COL, j++, "%-30s", s);
        n_flags++;
    }

    while (n_flags < flag_space)
    {
        erase_help(CLASS_AUX_COL, &j);
        n_flags++;
    }
}


/*
 * Set up one of our menus ready to display choices for a birth question.
 * This is slightly involved.
 */
static void init_birth_menu(menu_type *menu, int n_choices, int initial_choice,
    const region *reg, bool allow_random, browse_f aux)
{
    struct birthmenu_data *menu_data;

    /* Initialise a basic menu */
    menu_init(menu, MN_SKIN_SCROLL, &birth_iter);

    /*
     * A couple of behavioural flags - we want selections letters in
     * lower case and a double tap to act as a selection.
     */
    menu->selections = lower_case;
    menu->flags = MN_DBL_TAP;

    /* Copy across the game's suggested initial selection, etc. */
    menu->cursor = initial_choice;

    /* Allocate sufficient space for our own bits of menu information. */
    menu_data = mem_alloc(sizeof(*menu_data));

    /* Allocate space for an array of menu item texts and help texts (where applicable) */
    menu_data->items = mem_alloc(n_choices * sizeof(*menu_data->items));
    menu_data->allow_random = allow_random;

    /* Set private data */
    menu_setpriv(menu, n_choices, menu_data);

    /* Set up the "browse" hook to display help text (where applicable). */
    menu->browse_hook = aux;

    /* Lay out the menu appropriately */
    menu_layout(menu, reg);
}


/* Cleans up our stored menu info when we've finished with it. */
static void free_birth_menu(menu_type *menu)
{
    struct birthmenu_data *data = menu_priv(menu);

    mem_free(data->items);
    mem_free(data);
}


/*
 * Clear the previous question
 */
static void clear_question(void)
{
    int i;

    for (i = QUESTION_ROW; i < TABLE_ROW; i++)
    {
        /* Clear line, position cursor */
        Term_erase(0, i, 255);
    }
}


/* Show the birth instructions on an otherwise blank screen */  
static void print_menu_instructions(void)
{
    /* Clear screen */
    Term_clear();

    /* Display some helpful information */
    c_put_str(TERM_L_BLUE, "Please select your character from the menu below:", 1, 1);
    put_str("Use the ", 3, 1);
    c_put_str(TERM_L_GREEN, "movement keys", 3, 9);
    put_str(" to scroll the menu, ", 3, 22);
    c_put_str(TERM_L_GREEN, "Enter", 3, 43);
    put_str(" to select the current menu ", 3, 48);
    put_str("item, '", 4, 1);
    c_put_str(TERM_L_GREEN, "*", 4, 8);
    put_str("' for a random menu item, ", 4, 9);
    c_put_str(TERM_L_GREEN, "ESC", 4, 35);
    put_str(" to step back through the birth ", 4, 38);
    put_str("process, '", 5, 1);
    c_put_str(TERM_L_GREEN, "Q", 5, 11);
    put_str("' to quit.", 5, 12);
}


/*
 * Allow the user to select from the current menu, and return the
 * corresponding command to the game.  Some actions are handled entirely
 * by the UI (displaying help text, for instance).
 */
static enum birth_stage menu_question(enum birth_stage current, menu_type *current_menu)
{
    struct birthmenu_data *menu_data = menu_priv(current_menu);
    ui_event cx;
    enum birth_stage next = BIRTH_RESET;
    
    /* Print the question currently being asked. */
    clear_question();
    Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW, menu_data->hint);

    current_menu->cmd_keys = "*Q";

    while (next == BIRTH_RESET)
    {
        /* Display the menu, wait for a selection of some sort to be made. */
        cx = menu_select(current_menu, EVT_KBRD, FALSE);

        /*
         * As all the menus are displayed in "hierarchical" style, we allow
         * use of "back" (left arrow key or equivalent) to step back in
         * the proces as well as "escape".
         */
        if (cx.type == EVT_ESCAPE)
            next = BIRTH_BACK;
        else if (cx.type == EVT_SELECT)
        {
            switch (current)
            {
                case BIRTH_SEX_CHOICE: p_ptr->psex = current_menu->cursor; break;
                case BIRTH_RACE_CHOICE:
                    p_ptr->race = player_id2race(current_menu->cursor);
                    break;
                case BIRTH_CLASS_CHOICE:
                    p_ptr->clazz = player_id2class(current_menu->cursor);
                    break;
                case BIRTH_ROLLER_CHOICE: roller_type = current_menu->cursor; break;
            }
            next = current + 1;
        }
        else if (cx.type == EVT_KBRD)
        {
            /* '*' chooses an option at random from those the game's provided. */
            if ((cx.key.code == '*') && menu_data->allow_random)
            {
                current_menu->cursor = randint0(current_menu->count);
                switch (current)
                {
                    case BIRTH_SEX_CHOICE: p_ptr->psex = current_menu->cursor; break;
                    case BIRTH_RACE_CHOICE:
                        p_ptr->race = player_id2race(current_menu->cursor);
                        break;
                    case BIRTH_CLASS_CHOICE:
                        p_ptr->clazz = player_id2class(current_menu->cursor);
                        break;
                    case BIRTH_ROLLER_CHOICE: roller_type = current_menu->cursor; break;
                }

                menu_refresh(current_menu, FALSE);
                next = current + 1;
            }
            else if (cx.key.code == 'Q')
                next = BIRTH_QUIT;
        }
    }
    
    return next;
}


static const char *roller_choices[MAX_BIRTH_ROLLERS] =
{
    "Point-based",
    "Standard roller"
};


/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * a method for stat rolling.
 */
static enum birth_stage player_birth_aux_1(enum birth_stage current_stage)
{
    int i;
    struct birthmenu_data *mdata;
    menu_type *menu = &sex_menu;
    enum birth_stage next;

    /* Sex menu fairly straightforward */
    init_birth_menu(menu, MAX_SEXES, p_ptr->psex, &gender_region, TRUE, NULL);
    mdata = menu_priv(menu);
    for (i = 0; i < MAX_SEXES; i++) mdata->items[i] = sex_info[i].title;
    mdata->hint = "Your 'sex' does not have any significant gameplay effects.";
    mdata->choice = 0xFFFF;

    Term_clear();
    print_menu_instructions();

    if (current_stage > BIRTH_SEX_CHOICE)
    {
        int n;
        struct player_race *r;

        menu_refresh(menu, FALSE);
        menu = &race_menu;

        n = player_rmax();

        /* Race menu more complicated. */
        init_birth_menu(menu, n, (p_ptr->race? p_ptr->race->ridx: 0), &race_region, TRUE, race_help);
        mdata = menu_priv(menu);
        for (r = races; r; r = r->next) mdata->items[r->ridx] = r->name;
        mdata->hint = "Your 'race' determines various intrinsic factors and bonuses.";
        mdata->choice = 0xFFFF;
    }

    if (current_stage > BIRTH_RACE_CHOICE)
    {
        int n;
        struct player_class *c;

        menu_refresh(menu, FALSE);
        menu = &class_menu;

        n = player_cmax();

        /* Restrict choices for Dragon race */
        if (pf_has(p_ptr->race->pflags, PF_DRAGON)) n -= 2;

        /* Class menu similar to race. */
        init_birth_menu(menu, n, (p_ptr->clazz? p_ptr->clazz->cidx: 0), &class_region,
            TRUE, class_help);
        mdata = menu_priv(menu);
        for (c = classes; c; c = c->next)
        {
            if (c->cidx < n) mdata->items[c->cidx] = c->name;
        }
        mdata->hint = "Your 'class' determines various intrinsic abilities and bonuses";
        mdata->choice = p_ptr->race->choice;
    }

    if (current_stage > BIRTH_CLASS_CHOICE)
    {
        menu_refresh(menu, FALSE);
        menu = &roller_menu;

        /* Roller menu straightforward again */
        init_birth_menu(menu, MAX_BIRTH_ROLLERS, roller_type, &roller_region, FALSE, NULL);
        mdata = menu_priv(menu);
        for (i = 0; i < MAX_BIRTH_ROLLERS; i++) mdata->items[i] = roller_choices[i];
        mdata->hint = "Your choice of character generation. Point-based is recommended.";
        mdata->choice = 0xFFFF;
    }

    next = menu_question(current_stage, menu);

    if (next == BIRTH_BACK) next = current_stage - 1;

    /* We don't need these any more. */
    free_birth_menu(&sex_menu);
    if (current_stage > BIRTH_SEX_CHOICE) free_birth_menu(&race_menu);
    if (current_stage > BIRTH_RACE_CHOICE) free_birth_menu(&class_menu);
    if (current_stage > BIRTH_CLASS_CHOICE) free_birth_menu(&roller_menu);

    return next;
}


/*
 * Modify a stat value by a "modifier", return new value
 *
 * Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
 * Or even: 18/13, 18/23, 18/33, ..., 18/220
 *
 * Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
 * Or even: 18/13, 18/03, 18, 17, ..., 3
 */
static s16b modify_stat_value(int value, int amount)
{
    int i;

    /* Reward */
    if (amount > 0)
    {
        /* Apply each point */
        for (i = 0; i < amount; i++)
        {
            /* One point at a time */
            if (value < 18) value++;

            /* Ten "points" at a time */
            else value += 10;
        }
    }

    /* Penalty */
    else if (amount < 0)
    {
        /* Apply each point */
        for (i = 0; i < (0 - amount); i++)
        {
            /* Ten points at a time */
            if (value >= 18+10) value -= 10;

            /* Hack -- prevent weirdness */
            else if (value > 18) value = 18;

            /* One point at a time */
            else if (value > 3) value--;
        }
    }

    /* Return new value */
    return (value);
}


/*
 * Initial stat costs (initial stats always range from 8 to 17 inclusive).
 */
static const int birth_stat_costs[10] = {-2, -1, 0, 1, 2, 3, 4, 5, 6, 8};


/* Pool of available points */
#define MAX_BIRTH_POINTS 20


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 8 to 17 (inclusive),
 * each costing a certain amount of points (as above), from a pool of MAX_BIRTH_POINTS
 * available points, to which race/class modifiers are then applied.
 *
 * PWMAngband: each unused point is lost (giving gold would be exploitable)
 */
static enum birth_stage player_birth_aux_2(void)
{
    bool first_time = TRUE;
    int i;
    int cost;
    int stat = 0;
    char buf[NORMAL_WID];
    struct keypress ch;

    /* Clear screen */
    Term_clear();

    /* Initialize stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Initial stats */
        stat_roll[i] = 10;
    }

    /* Title everything */
    put_str("Name        :", 2, 1);
    c_put_str(TERM_L_BLUE, nick, 2, 15);
    put_str("Sex         :", 4, 1);
    c_put_str(TERM_L_BLUE, sex_info[p_ptr->psex].title, 4, 15);
    put_str("Race        :", 5, 1);
    c_put_str(TERM_L_BLUE, p_ptr->race->name, 5, 15);
    put_str("Class       :", 6, 1);
    c_put_str(TERM_L_BLUE, p_ptr->clazz->name, 6, 15);

    put_str("[Press 'ESC' at any time to restart this step, or 'Q' to quit]", 23, 1);

    /* Extra info */
    Term_putstr(5, 8, -1, TERM_WHITE, "The point-based roller allows players to increase or decrease");
    Term_putstr(5, 9, -1, TERM_WHITE, "each stat, each increase costing a certain amount of points,");
    Term_putstr(5, 10, -1, TERM_WHITE, "each decrease giving back some points.");
    strnfmt(buf, sizeof(buf), "The starting pool consists of %d available points.", MAX_BIRTH_POINTS);
    Term_putstr(5, 11, -1, TERM_WHITE, buf);

    /* Interact */
    while (1)
    {
        int dir;

        /* Reset cost */
        cost = 0;

        /* Process stats */
        for (i = 0; i < A_MAX; i++)
        {
            /* Total cost */
            cost += birth_stat_costs[stat_roll[i] - 8];
        }

        /* Restrict cost (upper bound) */
        if (cost > MAX_BIRTH_POINTS)
        {
            /* Warning */
            bell(NULL);

            /* Reduce stat */
            stat_roll[stat]--;

            /* Recompute costs */
            continue;
        }  

        /* Restrict cost (lower bound) */
        if (cost < 0)
        {
            /* Warning */
            bell(NULL);

            /* Increase stat */
            stat_roll[stat]++;

            /* Recompute costs */
            continue;
        }

        /* Display the stats header */
        put_str("  Self    Best", 15, 10);

        /* Display the stats */
        for (i = 0; i < A_MAX; i++)
        {
            int j, m;

            /* Display stat name */
            put_str(stat_names[i], 16 + i, 5);

            /* Display stat value */
            cnv_stat(stat_roll[i], buf, sizeof(buf));
            c_put_str(TERM_L_GREEN, buf, 16 + i, 10);

            /* Race/Class bonus */
            j = p_ptr->race->r_adj[i] + p_ptr->clazz->c_adj[i];

            /* Obtain the "maximal" stat */
            m = modify_stat_value(stat_roll[i], j);

            /* Display "maximal" stat value */
            cnv_stat(m, buf, sizeof(buf));
            c_put_str(TERM_L_GREEN, buf, 16 + i, 18);
        }

        /* Display the costs header */
        put_str("Cost", 15, 26);

        /* Display the costs */
        for (i = 0; i < A_MAX; i++)
        {
            /* Display cost */
            strnfmt(buf, sizeof(buf), "%4d", birth_stat_costs[stat_roll[i] - 8]);
            put_str(buf, 16 + i, 26);
        }

        /* Prompt */
        strnfmt(buf, sizeof(buf),
            "Total Cost %2d/%d.  Use up/down to move, left/right to modify, 'Enter' to accept.",
            cost, MAX_BIRTH_POINTS);
        put_str(buf, 13, 1);

        /* Place cursor just after cost of current stat */
        Term_gotoxy(29, 16 + stat);
        Term_set_cursor(TRUE);

        /* Get key */
        ch = inkey();

        /* Start over */
        if (ch.code == 'Q') return BIRTH_QUIT;

        /* Go back a step, or back to the start of this step */
        if (ch.code == ESCAPE)
        {
            /* Reset cursor stuff */
            Term_set_cursor(FALSE);

            /* Back a step */
            if (first_time) return BIRTH_BACK;

            /* Repeat this step */
            return BIRTH_ROLLER;
        }

        first_time = FALSE;

        /* Done */
        if (ch.code == KC_ENTER) break;

        dir = target_dir(ch);

        /* Prev stat */
        if (dir == 8) stat = (stat + A_MAX - 1) % A_MAX;

        /* Next stat */
        if (dir == 2) stat = (stat + 1) % A_MAX;

        /* Decrease stat */
        if ((dir == 4) && (stat_roll[stat] > 8)) stat_roll[stat]--;

        /* Increase stat */
        if ((dir == 6) && (stat_roll[stat] < 17)) stat_roll[stat]++;
    }

    /* Clear prompt */
    clear_from(23);

    stat_roll[A_MAX] = roller_type;

    /* Reset cursor stuff */
    Term_set_cursor(FALSE);

    /* Done - advance a step */
    return BIRTH_FINAL_CONFIRM;
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "standard" character creation.
 */
static enum birth_stage player_birth_aux_3(void)
{
    int i, j, k, avail[A_MAX];
    struct keypress c;
    char out_val[160], stats[A_MAX][4];

    /* Clear screen */
    Term_clear();

    /* Title everything */
    put_str("Name        :", 2, 1);
    c_put_str(TERM_L_BLUE, nick, 2, 15);
    put_str("Sex         :", 4, 1);
    c_put_str(TERM_L_BLUE, sex_info[p_ptr->psex].title, 4, 15);
    put_str("Race        :", 5, 1);
    c_put_str(TERM_L_BLUE, p_ptr->race->name, 5, 15);
    put_str("Class       :", 6, 1);
    c_put_str(TERM_L_BLUE, p_ptr->clazz->name, 6, 15);
    put_str("Stat roll   :", 8, 1);

    put_str("[Press 'ESC' at any time to restart this step, or 'Q' to quit]", 23, 1);

    /* Extra info */
    Term_putstr(5, 15, -1, TERM_WHITE,
        "The standard roller will automatically ignore characters which do");
    Term_putstr(5, 16, -1, TERM_WHITE,
        "not meet the minimum values of 17 for the first stat, 16 for the");
    Term_putstr(5, 17, -1, TERM_WHITE,
        "second stat and 15 for the third stat specified below.");
    Term_putstr(5, 18, -1, TERM_WHITE,
        "Stats will be rolled randomly according to the specified order.");

    put_str("Choose your stat order: ", 20, 2);

    /* All stats are initially available */
    for (i = 0; i < A_MAX; i++)
    {
        my_strcpy(stats[i], stat_names[i], sizeof(stats[0]));
        avail[i] = 1;
    }

    /* Find the ordering of all stats */
    for (i = 0; i < A_MAX; i++)
    {
        /* Clear line */
        prt("", 21, 0);

        /* Print available stats at bottom */
        for (k = 0; k < A_MAX; k++)
        {
            /* Check for availability */
            if (avail[k])
            {
                strnfmt(out_val, sizeof(out_val), "%c) %s", I2A(k), stats[k]);
                put_str(out_val, 21, k * 9);
            }
        }

        /* Get a stat */
        while (1)
        {
            /* Get a key */
            c = inkey();

            /* Start over */
            if (c.code == 'Q') return BIRTH_QUIT;

            /* Handle ESC */
            if (c.code == ESCAPE)
            {
                /* Back a step */
                if (i == 0) return BIRTH_BACK;

                /* Repeat this step */
                return BIRTH_ROLLER;
            }

            /* Break on valid input */
            j = (islower(c.code) ? A2I(c.code) : -1);
            if ((j < A_MAX) && (j >= 0) && avail[j])
            {
                stat_roll[i] = j;
                c_put_str(TERM_L_BLUE, stats[j], 8 + i, 15);
                avail[j] = 0;
                break;
            }
            else
                bell(NULL);
        }
    }

    /* Clear bottom of screen */
    clear_from(20);

    stat_roll[A_MAX] = roller_type;

    /* Done - move on a stage */
    return BIRTH_FINAL_CONFIRM;
}


/*
 * Helper function for 'player_birth()'.
 *
 * This function handles quick creation based on the previous character.
 */
static bool player_birth_quick(void)
{
    struct keypress ch;

    /* Starting over */
    if (!quick_start) return FALSE;

    /* Clear screen */
    Term_clear();

    /* Prompt for it */
    put_str("Quick-start character based on previous one (y/n)? ", 2, 2);

    while (TRUE)
    {
        /* Get key */
        ch = inkey();

        /* Start over */
        if (ch.code == 'Q') quit(NULL);

        /* Done */
        else if ((ch.code == ESCAPE) || strchr("YyNn\r\n", ch.code)) break;

        /* Error */
        else bell(NULL);
    }

    /* Quick generation */
    if ((ch.code == 'y') || (ch.code == 'Y'))
    {
        /* Prompt for it */
        prt("['Q' to quit, 'ESC' to start over, or any other key to continue]", 23, 5);

        /* Get a key */
        ch = inkey();

        /* Quit */
        if (ch.code == 'Q') quit(NULL);

        /* Start over */
        if (ch.code == ESCAPE) return (FALSE);

        /* Accept */
        stat_roll[A_MAX] = quick_start;
        return (TRUE);
    }

    /* Start over */
    return FALSE;
}


/*
 * Final confirmation of character.
 */
static enum birth_stage get_confirm_command(void)
{
    const char *prompt = "['ESC' to step back, 'S' to start over, or any other key to continue]";
    ui_event ke;

    /* Prompt for it */
    put_str(prompt, 23, 1);

    /* Get a key */
    ke = inkey_ex();

    if (ke.type == EVT_KBRD)
    {
        switch (ke.key.code)
        {
            case 'S':
            case 's': return BIRTH_RESET;

            case 'Q': return BIRTH_QUIT;
        }
    }

    if (is_exit(ke)) return BIRTH_BACK;

    return BIRTH_COMPLETE;
}


/*
 * Create a new character.
 */
void player_birth(void)
{
    static enum birth_stage current_stage = BIRTH_RESET;
    enum birth_stage next = current_stage;

    /* Offer to do a quick creation based on the previous character */
    if (player_birth_quick()) return;

    while (1)
    {
        switch (current_stage)
        {
            case BIRTH_RESET:
            {
                next = BIRTH_SEX_CHOICE;

                break;
            }

            case BIRTH_SEX_CHOICE:
            case BIRTH_RACE_CHOICE:
            case BIRTH_CLASS_CHOICE:
            case BIRTH_ROLLER_CHOICE:
            {
                /* Race, class, etc. choices */
                next = player_birth_aux_1(current_stage);

                break;
            }

            case BIRTH_ROLLER:
            {
                if (roller_type == BR_POINTBASED)
                {
                    /* Fill stats using point-based methods */
                    next = player_birth_aux_2();
                }
                else
                {
                    /* Fills stats using the standard roller */
                    next = player_birth_aux_3();
                }

                if (next == BIRTH_BACK) next = BIRTH_ROLLER_CHOICE;

                break;
            }

            case BIRTH_FINAL_CONFIRM:
            {
                next = get_confirm_command();

                if (next == BIRTH_BACK) next = BIRTH_ROLLER;

                break;
            }

            case BIRTH_COMPLETE: return;

            case BIRTH_QUIT: quit(NULL);
        }

        current_stage = next;
    }
}


static bool enter_server_name(void)
{
    bool result;
    char *s;

    /* Clear screen */
    Term_clear();

    /* Message */
    prt("Enter the server name you want to connect to (ESCAPE to quit): ", 3, 1);

    /* Move cursor */
    Term_gotoxy(1, 5);

    /* Default */
    my_strcpy(server_name, "localhost", sizeof(server_name));

    /* Ask for server name */
    result = askfor_aux(server_name, sizeof(server_name), NULL);

    s = strchr(server_name, ':');
    if (!s) return result;

    sscanf(s, ":%d", &server_port);
    *s = '\0';

    return result;
}


/*
 * Have the player choose a server from the list given by the
 * metaserver.
 */
bool get_server_name(void)
{
    int i, j, y, bytes, socket, offsets[20];
    bool server, info;
    char buf[8192], *ptr, out_val[160];
    int ports[30];
    struct keypress c;

    /* Perhaps we already have a server name from config file ? */
    if (strlen(server_name) > 0) return TRUE;

    /* Message */
    prt("Connecting to metaserver for server list....", 1, 1);

    /* Make sure message is shown */
    Term_fresh();

    /* Connect to metaserver */
    socket = CreateClientSocket(meta_address, 8802);

    /* Check for failure */
    if (socket == -1) return enter_server_name();

    /* Read */
    bytes = SocketRead(socket, buf, 8192);

    /* Close the socket */
    SocketClose(socket);

    /* Check for error while reading */
    if (bytes <= 0) return enter_server_name();

    /* Start at the beginning */
    ptr = buf;
    i = y = 0;

    /* Print each server */
    while (ptr - buf < bytes)
    {
        /* Check for no entry */
        if (strlen(ptr) <= 1)
        {
            /* Increment */
            ptr++;

            /* Next */
            continue;
        }

        info = TRUE;

        /* Save server entries */
        if (*ptr == '%')
        {
            server = info = FALSE;

            /* Save port */
            ports[i] = atoi(ptr+1);
        }
        else if (*ptr != ' ')
        {
            server = TRUE;

            /* Save offset */
            offsets[i] = ptr - buf;

            /* Format entry */
            strnfmt(out_val, sizeof(out_val), "%c) %s", I2A(i), ptr);
        }
        else
        {
            server = FALSE;

            /* Display notices */
            strnfmt(out_val, sizeof(out_val), "%s", ptr);
        }

        if (info)
        {
            /* Strip off offending characters */
            out_val[strlen(out_val) - 1] = '\0';

            /* Print this entry */
            prt(out_val, y + 1, 1);

            /* One more entry */
            if (server) i++;
            y++;
        }

        /* Go to next metaserver entry */
        ptr += strlen(ptr) + 1;

        /* We can't handle more than 20 entries -- BAD */
        if (i > 20) break;
    }

    /* Prompt */
    prt("Choose a server to connect to (Q for manual entry): ", y + 2, 1);

    /* Ask until happy */
    while (1)
    {
        /* Get a key */
        c = inkey();

        /* Check for quit */
        if (c.code == 'Q') return enter_server_name();

        /* Index */
        j = (islower(c.code) ? A2I(c.code) : -1);

        /* Check for legality */
        if ((j >= 0) && (j < i)) break;
    }

    /* Extract server name */
    sscanf(buf + offsets[j], "%s", server_name);

    /* Set port */
    server_port = ports[j+1];

    /* Success */
    return TRUE;
}


/*
 * Choose the account's name
 */
static void choose_account(void)
{
    char tmp[23];

    /* Prompt and ask */
    prt("Enter your account's name above (or hit ESCAPE).", 21, 2);

    /* Default */
    my_strcpy(tmp, nick, sizeof(tmp));

    /* Ask until happy */
    while (1)
    {
        /* Go to the "name" area */
        Term_gotoxy(15, 2);

        /* Ask the user for a string */
        if (askfor_aux(tmp, 16, NULL)) my_strcpy(nick, tmp, sizeof(nick));

        /* All done */
        break;
    }

    /* Capitalize the name */
    my_strcap(nick);

    /* Pad the name (to clear junk) */
    strnfmt(tmp, sizeof(tmp), "%-15.15s", nick);

    /* Redraw the name (in light blue) */
    c_put_str(TERM_L_BLUE, tmp, 2, 15);

    /* Erase the prompt, etc */
    clear_from(20);
}


/*
 * Get the name/pass for this account.
 */
void get_account_name(void)
{
    /* Clear screen */
    Term_clear();

    /* Title everything */
    put_str("Name        :", 2, 1);
    put_str("Password    :", 3, 1);

    /* Choose a name */
    choose_account();

    /* Enter password */
    enter_password();

    /* Message */
    put_str("Connecting to server....", 21, 1);

    /* Make sure the message is shown */
    Term_fresh();

    /* Note player birth in the message recall */
    c_msg_print(" ");
    c_msg_print("  ");
    c_msg_print("====================");
    c_msg_print("  ");
    c_msg_print(" ");
}


/*
 * Get the name/pass for this character.
 */
void get_char_name(void)
{
    size_t i;
    char charname[NORMAL_WID];
    struct keypress c;

    /* Clear screen */
    Term_clear();

    /* Title everything */
    put_str("Name        :", 2, 1);
    put_str("Password    :", 3, 1);

    /* Redraw the name (in light blue) */
    c_put_str(TERM_L_BLUE, nick, 2, 15);

    /* Redraw the password (in light blue) */
    for (i = 0; i < strlen(pass); i++)
        Term_putch(15 + i, 3, TERM_L_BLUE, 'x');

    /* Display some helpful information */
    c_put_str(TERM_L_BLUE, "Please select your character from the list below:", 6, 1);

    /* Display character names */
    for (i = 0; i < char_num; i++)
    {
        /* Character is dead */
        if (char_expiry[i] > 0)
        {
            strnfmt(charname, sizeof(charname), "%c) %s (deceased, expires in %d days)", I2A(i),
                char_name[i], char_expiry[i]);
            c_put_str(TERM_L_DARK, charname, 8 + i, 5);
        }

        /* Character is alive */
        else if (char_expiry[i] == -1)
        {
            strnfmt(charname, sizeof(charname), "%c) %s", I2A(i), char_name[i]);
            put_str(charname, 8 + i, 5);
        }

        /* Paranoia */
        else
        {
            strnfmt(charname, sizeof(charname), "%c) ERROR: expired or unknown character", I2A(i));
            c_put_str(TERM_RED, charname, 8 + i, 5);
        }
    }

    /* Check number of characters */
    if (char_num == MAX_ACCOUNT_CHARS)
    {
        c_put_str(TERM_YELLOW, "Your account is full.", 9 + char_num, 5);
        c_put_str(TERM_YELLOW, "You cannot create any new character with this account.",
            10 + char_num, 5);
    }
    else
    {
        /* Give choice for a new character */
        strnfmt(charname, sizeof(charname), "%c) New character", I2A(char_num));
        c_put_str(TERM_L_BLUE, charname, 9 + char_num, 5);
    }

    /* Ask until happy */
    while (1)
    {
        /* Get a key */
        c = inkey();

        /* Check for quit */
        if (c.code == 'Q') quit(NULL);

        /* Check for legality */
        if (!islower(c.code)) continue;

        /* Index */
        i = A2I(c.code);

        /* Check for legality */
        if ((i > char_num) || (i >= MAX_ACCOUNT_CHARS)) continue;

        /* Paranoia */
        if ((i == char_num) || (char_expiry[i] > 0) || (char_expiry[i] == -1))
            break;
    }

    /* Clear screen */
    Term_clear();

    /* Title everything */
    put_str("Name        :", 2, 1);
    put_str("Password    :", 3, 1);

    quick_start = 0;

    /* Existing character */
    if (i < char_num)
    {
        /* Set the player name to the selected character name */
        my_strcpy(nick, char_name[i], sizeof(nick));

        /* Capitalize the name */
        my_strcap(nick);

        /* Dump the player name */
        c_put_str(TERM_L_BLUE, nick, 2, 15);

        /* Enter password */
        enter_password();

        /* Display actions */
        if (char_expiry[i] > 0)
        {
            bool ok = get_incarnation(1, char_name[i], sizeof(char_name[i]));
            int max = (ok? 3: 2);

            /* Display some helpful information */
            c_put_str(TERM_L_BLUE, "Please select an action from the list below:", 6, 1);

            /* Display actions */
            put_str("a) Get a character dump", 8, 5);
            put_str("b) Delete this character", 9, 5);
            put_str("c) Reroll this character", 10, 5);
            if (ok) put_str("d) Play a new incarnation of this character", 11, 5);

            /* Ask until happy */
            while (1)
            {
                /* Get a key */
                c = inkey();

                /* Check for quit */
                if (c.code == 'Q') quit(NULL);

                /* Check for legality */
                if (!islower(c.code)) continue;

                /* Index */
                i = A2I(c.code);

                /* Check for legality */
                if (i <= max) break;
            }

            /* Get a character dump */
            if (i == 0) my_strcat(nick, "=", sizeof(nick));

            /* Delete character */
            else if (i == 1) my_strcat(nick, "-", sizeof(nick));

            /* Reroll character */
            else if (i == 2) quick_start = BR_QUICK;

            /* Play a new incarnation */
            else
            {
                my_strcat(nick, "+", sizeof(nick));
                quick_start = BR_QDYNA;
            }
        }
    }

    /* New character */
    else
    {
        /* Dump the default name (account name) */
        c_put_str(TERM_L_BLUE, nick, 2, 15);

        /* Choose a name */
        choose_name();

        /* Enter password */
        enter_password();
    }
}
